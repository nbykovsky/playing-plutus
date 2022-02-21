{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE BlockArguments #-}


module ChargeOther where

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
import qualified Ledger.Scripts             as Scripts
import           Ledger.Ada                 as Ada
import           Playground.Contract        (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH              (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types           (KnownCurrency (..))
import           Prelude                    (IO, Semigroup (..), Show (..), String, Foldable (length))
import           Text.Printf                (printf)
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import           Wallet.Emulator.Wallet
import           Plutus.Contract.Request
import           Plutus.Contract.Test
import qualified Plutus.Trace.Emulator as Trace
import           Test.Tasty
import           Test.Tasty.HUnit
import           Plutus.Contract (AsContractError)
import           Data.Aeson           (ToJSON, FromJSON)
import           GHC.Generics         (Generic)
import Plutus.Trace.Effects.EmulatedWalletAPI (EmulatedWalletAPI)
import Data.Maybe



data PaymentParams = PaymentParams {
    fromPk :: PubKeyHash,
    toPk   :: PubKeyHash,
    amount :: Integer
} deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

type TransferSchema = Endpoint "charge" PaymentParams 

charge :: AsContractError e => Promise () TransferSchema e ()
charge = endpoint @"charge" @PaymentParams $ \params -> do
    pk    <- Contract.ownPubKeyHash
    let pkFrom = fromPk params
        pkTo   = toPk params
    utxos <- utxosAt (pubKeyHashAddress pkFrom)
    Contract.logError @String $ show $ Prelude.length utxos
    case Map.keys utxos of 
        []       -> Contract.logError @String "inputs not found"
        oref : _ -> do 
            Contract.logError @String ("oref = " ++ show oref)
            let tx = mustSpendPubKeyOutput oref <> mustPayToPubKey pkTo (Ada.lovelaceValueOf $ amount params)
                lookups = Constraints.unspentOutputs utxos
            ledgerTx <- submitTxConstraintsWith @Void lookups tx 
            let ledgerTxUnwrapped = case ledgerTx of Right b -> b
                                                     _       -> error ()
            void $ awaitTxConfirmed $ txId ledgerTxUnwrapped
            Contract.logInfo @String $ printf "paid %d lovelace" (amount params)


endpoints :: Contract () TransferSchema Text ()
endpoints = Contract.selectList [charge] >> endpoints

myTrace :: EmulatorTrace ()
myTrace = do
    h1 <- activateContractWallet w1 endpoints
    -- h2 <- activateContractWallet w2 endpoints
    let params = PaymentParams { 
        fromPk =  walletPubKeyHash w1, --fromJust $ ownPublicKey <$> (fromMockWallet <$> walletMockWallet w1), 
        toPk   = walletPubKeyHash w2,--fromJust $ ownPublicKey <$> (fromMockWallet <$> walletMockWallet w2),
        amount =  10000000
    }
    callEndpoint @"charge" h1 params
    void $ Emulator.waitNSlots 1

test :: IO ()
test = runEmulatorTraceIO myTrace