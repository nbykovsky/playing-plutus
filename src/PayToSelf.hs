{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module PayToSelf where

import           Control.Monad.Freer.Extras as Extras
import           Control.Exception.Base
import           Control.Monad              hiding (fmap)
import           Data.Map                   as Map
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           Plutus.Contract
import           PlutusTx                   (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Ledger                     hiding (singleton)
import           Ledger.Constraints         as Constraints
-- import qualified Ledger.Scripts             as Scripts
import           Ledger.Ada                 as Ada
import           Playground.Contract        (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH              (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types           (KnownCurrency (..))
import           Prelude                    (IO, Semigroup (..), String)
import           Text.Printf                (printf)
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import           Wallet.Emulator.Wallet
import           Plutus.Contract.Request
import           Plutus.Contract.Test
import qualified Plutus.Trace.Emulator as Trace
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Ledger.Typed.Scripts as Scripts



-- import qualified Plutus.Trace.Playground as Trace
-- import qualified Plutus.Trace.Emulator as Trace


{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# INLINABLE mkValidator #-}
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ _ _ = True

data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = ()
    type instance RedeemerType Typed = ()

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator


type GiftSchema =
            Endpoint "give" Integer
         .\/ Endpoint "grab" ()
         .\/ Endpoint "selfPay" Integer

give :: AsContractError e => Promise () GiftSchema e ()
give = endpoint @"give" @Integer $ \amount -> do
    let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTxConstraints typedValidator tx
    let ledgerTxUnwrapped = case ledgerTx of Right b  -> b
                                             _        -> error ()
    void $ awaitTxConfirmed $ txId ledgerTxUnwrapped
    Contract.logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Promise () GiftSchema e ()
grab = endpoint @"grab" @() $ \ _ ->  do
    utxos <- utxosAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData () | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    let ledgerTxUnwrapped = case ledgerTx of Right b  -> b
                                             _        -> error ()
    void $ awaitTxConfirmed $ txId ledgerTxUnwrapped
    Contract.logInfo @String $ "collected gifts"

selfPay :: AsContractError e => Promise () GiftSchema e ()
selfPay = endpoint @"selfPay" @Integer $  \amount -> do
    ph <- Contract.ownPubKeyHash
    utxos <- utxosAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx = (mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData () | oref <- orefs]) 
            <> (mustPayToOtherScript valHash (Datum $ PlutusTx.toBuiltinData ()) (Ada.lovelaceValueOf amount))
            <>  (mustPayToPubKey ph $ Ada.lovelaceValueOf amount)
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    let ledgerTxUnwrapped = case ledgerTx of Right b  -> b
                                             _        -> error ()
    void $ awaitTxConfirmed $ txId ledgerTxUnwrapped
    Contract.logInfo @String $ "payed to self"


endpoints :: Contract () GiftSchema Text ()
endpoints = Contract.selectList [give, grab, selfPay] >> endpoints

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []

myTrace :: EmulatorTrace ()
myTrace = do
    Extras.logInfo @String $ "Contract Start"
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"give" h1 10000000
    void $ Emulator.waitNSlots 1
    callEndpoint @"give" h1 5000000
    void $ Emulator.waitNSlots 1
    callEndpoint @"selfPay" h1 5000000
    void $ Emulator.waitNSlots 1
    callEndpoint @"grab" h2 ()
    void $ Emulator.waitNSlots 1

test :: IO ()
test = runEmulatorTraceIO myTrace

t1, t2 :: ContractInstanceTag 
t1 = Trace.walletInstanceTag w1
t2 = Trace.walletInstanceTag w2

theContract :: Contract () GiftSchema Text ()
theContract = endpoints

tests :: TestTree
tests = testGroup "gift" 
    [
        checkPredicate "Correct balances change" (walletFundsChange w1 (Ada.adaValueOf (-10)) .&&. 
        walletFundsChange w2 (Ada.adaValueOf 10)) myTrace
    ]

runTests = defaultMain tests