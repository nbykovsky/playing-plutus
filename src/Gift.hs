{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- import qualified Ledger.Scripts             as Scripts
-- import qualified Plutus.Trace.Playground as Trace
-- import qualified Plutus.Trace.Emulator as Trace
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Gift where

import           Control.Exception.Base
import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Map                   as Map
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           Ledger                     hiding (singleton)
import           Ledger.Ada                 as Ada
import           Ledger.Constraints         as Constraints
import qualified Ledger.Typed.Scripts       as Scripts
import           Playground.Contract        (ensureKnownCurrencies, printJson,
                                             printSchemas, stage)
import           Playground.TH              (mkKnownCurrencies,
                                             mkSchemaDefinitions)
import           Playground.Types           (KnownCurrency (..))
import           Plutus.Contract
import           Plutus.Contract            as Contract
import           Plutus.Contract.Request
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator      as Emulator
import qualified Plutus.Trace.Emulator      as Trace
import           PlutusTx                   (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude           hiding (Semigroup (..), unless)
import           Prelude                    (IO, Semigroup (..), String)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf                (printf)
import           Wallet.Emulator.Wallet

{-# INLINEABLE mkValidator #-}
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ _ _ = True

data Typed

instance Scripts.ValidatorTypes Typed where
  type DatumType Typed = ()
  type RedeemerType Typed = ()

typedValidator :: Scripts.TypedValidator Typed
typedValidator =
  Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
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

give :: AsContractError e => Promise () GiftSchema e ()
give = endpoint @"give" @Integer $ \amount -> do
  let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount
  ledgerTx <- submitTxConstraints typedValidator tx
  let ledgerTxUnwrapped = case ledgerTx of
        Right b -> b
        _       -> error ()
  void $ awaitTxConfirmed $ txId ledgerTxUnwrapped
  Contract.logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Promise () GiftSchema e ()
grab = endpoint @"grab" @() $ \_ -> do
  utxos <- utxosAt scrAddress
  let orefs = fst <$> Map.toList utxos
      lookups =
        Constraints.unspentOutputs utxos
          <> Constraints.otherScript validator
      tx :: TxConstraints Void Void
      tx = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData () | oref <- orefs]
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  let ledgerTxUnwrapped = case ledgerTx of
        Right b -> b
        _       -> error ()
  void $ awaitTxConfirmed $ txId ledgerTxUnwrapped
  Contract.logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = Contract.selectList [give, grab] >> endpoints

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []

myTrace :: EmulatorTrace ()
myTrace = do
  Extras.logInfo @String $ "Contract Start"
  h1 <- activateContractWallet w1 endpoints
  h2 <- activateContractWallet w2 endpoints
  callEndpoint @"give" h1 10000000
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
tests =
  testGroup
    "gift"
    [ checkPredicate
        "Correct balances change"
        ( walletFundsChange w1 (Ada.adaValueOf (-10))
            .&&. walletFundsChange w2 (Ada.adaValueOf 10)
        )
        myTrace
    ]

runTests = defaultMain tests
