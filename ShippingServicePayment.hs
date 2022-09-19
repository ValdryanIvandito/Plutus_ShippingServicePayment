------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- LIBRARY
------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Text.Printf          (printf)
import           Data.Char
------------------------------------------------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ONCHAIN
------------------------------------------------------------------------------------------------------------------------------------------------------------------
data DeliveryDatum = DeliveryDatum
    { paymentAddress'   :: PaymentPubKeyHash
    , deliveryDeadline' :: POSIXTime
    , receiptNumber'    :: Integer
    } deriving Show
PlutusTx.unstableMakeIsData ''DeliveryDatum

newtype MyRedeemer = MyRedeemer Integer
PlutusTx.unstableMakeIsData ''MyRedeemer

{-# INLINABLE execValidation #-}
execValidation :: DeliveryDatum -> MyRedeemer -> ScriptContext -> Bool
execValidation datum (MyRedeemer mrd) ctx = traceIfFalse "paymentAddress's signature missing" signedBypaymentAddress' &&
                         traceIfFalse "deliveryDeadline not reached" deliveryDeadline'Reached &&
                         (traceIfFalse "invalid receiptNumber!" $ mrd == receiptNumber' datum)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedBypaymentAddress' :: Bool
    signedBypaymentAddress' = txSignedBy info $ unPaymentPubKeyHash $ paymentAddress' datum

    deliveryDeadline'Reached :: Bool
    deliveryDeadline'Reached = contains (from $ deliveryDeadline' datum) $ txInfoValidRange info

data Delivery
instance Scripts.ValidatorTypes Delivery where
    type instance DatumType Delivery = DeliveryDatum
    type instance RedeemerType Delivery = MyRedeemer

typedValidator :: Scripts.TypedValidator Delivery
typedValidator = Scripts.mkTypedValidator @Delivery
    $$(PlutusTx.compile [|| execValidation ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @DeliveryDatum @MyRedeemer

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
------------------------------------------------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- OFFCHAIN
------------------------------------------------------------------------------------------------------------------------------------------------------------------
data PaymentParams = PaymentParams
    { paymentAddress            :: !PaymentPubKeyHash
    , deliveryDeadline :: !POSIXTime
    , deliveryStart             :: !String
    , deliveryDestination       :: !String
    , weight_Kg                 :: !Integer
    , receiptNumber             :: !Integer 
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type DeliverySchema =
            Endpoint "payment" PaymentParams
        .\/ Endpoint "confirmation" Integer

payment :: AsContractError e => PaymentParams -> Contract w s e ()
payment pp = do
    let price = 1000000
    let datum = DeliveryDatum
                { paymentAddress'   = paymentAddress pp
                , deliveryDeadline' = deliveryDeadline pp
                , receiptNumber'    = receiptNumber pp
                }
    let start = deliveryStart pp
    let destination = deliveryDestination pp
    let route = shippingroute (fmap toUpper start) (fmap toUpper destination)
    let tx  = Constraints.mustPayToTheScript datum $ Ada.lovelaceValueOf $ (price * weight_Kg pp) + route
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a payment of %d lovelace to %s with deliveryDeadline' %s"
        (price)
        (show $ paymentAddress pp)
        (show $ deliveryDeadline pp)

---------------------------------------------
-- ROUTES PATTERN MATCHING
---------------------------------------------
shippingroute :: String -> String -> Integer
shippingroute "BANDUNG" "JAKARTA" = 1000000
shippingroute "JAKARTA" "BANDUNG" = 1000000
shippingroute "BANDUNG" "SURABAYA" = 3000000
shippingroute "SURABAYA" "BANDUNG" = 3000000
shippingroute "BANDUNG" "DENPASAR" = 7000000
shippingroute "DENPASAR" "BANDUNG" = 7000000
shippingroute "BANDUNG" "MEDAN" = 10000000
shippingroute "MEDAN" "BANDUNG" = 10000000
shippingroute "JAKARTA" "SURABAYA" = 4000000
shippingroute "SURABAYA" "JAKARTA" = 4000000
shippingroute "JAKARTA" "DENPASAR" = 8000000
shippingroute "DENPASAR" "JAKARTA" = 8000000
shippingroute "JAKARTA" "MEDAN" = 900000
shippingroute "MEDAN" "JAKARTA" = 900000
shippingroute "SURABAYA" "DENPASAR" = 4000000
shippingroute "DENPASAR" "SURABAYA" = 4000000
shippingroute "SURABAYA" "MEDAN" = 13000000
shippingroute "MEDAN" "SURABAYA" = 13000000
shippingroute "DENPASAR" "MEDAN" = 15500000
shippingroute "MEDAN" "DENPASAR" = 15500000
---------------------------------------------

confirmation :: forall w s e. AsContractError e => Integer -> Contract w s e ()
confirmation settle = do
    now   <- currentTime
    pkh   <- ownPaymentPubKeyHash
    utxos <- Map.filter (isSuitable pkh now) <$> utxosAt scrAddress
    if Map.null utxos
        then logInfo @String $ printf "no payment available => pkh = %s, receiptNumber' = %s" (show $ pkh) (show $ settle)
        else do
            let orefs   = fst <$> Map.toList utxos
                lookups = Constraints.unspentOutputs utxos  <>
                          Constraints.otherScript validator
                tx :: TxConstraints Void Void
                tx      = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData (MyRedeemer settle) | oref <- orefs] <>
                          Constraints.mustValidateIn (from now)
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ printf "payment settled => date %s pkh = %s" (show $ now) (show $ pkh)
  where
    isSuitable :: PaymentPubKeyHash -> POSIXTime -> ChainIndexTxOut -> Bool
    isSuitable pkh now o = case _ciTxOutDatum o of
        Left _          -> False
        Right (Datum e) -> case PlutusTx.fromBuiltinData e of
            Nothing -> False
            Just d  -> paymentAddress' d == pkh && deliveryDeadline' d <= now && receiptNumber' d == settle


endpoints :: Contract () DeliverySchema Text ()
endpoints = awaitPromise (payment' `select` confirmation') >> endpoints
  where
    payment' = endpoint @"payment" payment
    confirmation' = endpoint @"confirmation" confirmation

mkSchemaDefinitions ''DeliverySchema

mkKnownCurrencies []
------------------------------------------------------------------------------------------------------------------------------------------------------------------