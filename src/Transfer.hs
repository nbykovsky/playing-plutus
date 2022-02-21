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


module Transfer where

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
import           Prelude                    (IO, Semigroup (..), Show (..), String)
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




data PaymentParams = PaymentParams {
    beneficiary :: PubKeyHash,
    amount      :: Integer
} deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

type TransferSchema = Endpoint "pay" PaymentParams 

pay :: AsContractError e => Promise () TransferSchema e ()
pay = endpoint @"pay" @PaymentParams $ \params -> do
    let tx = mustPayToPubKey (beneficiary params) (Ada.lovelaceValueOf $ amount params)
    ledgerTx <- submitTx tx 
    let ledgerTxUnwrapped = case ledgerTx of Right b  -> b
                                             _        -> error ()
    void $ awaitTxConfirmed $ txId ledgerTxUnwrapped
    Contract.logInfo @String $ printf "paid %d lovelace" (amount params)


endpoints :: Contract () TransferSchema Text ()
endpoints = Contract.selectList [pay] >> endpoints

myTrace :: EmulatorTrace ()
myTrace = do
    h1 <- activateContractWallet w1 endpoints
    let params = PaymentParams { 
        beneficiary =  walletPubKeyHash w2, 
        amount      =  10000000
    }
    callEndpoint @"pay" h1 params
    void $ Emulator.waitNSlots 1

test :: IO ()
test = runEmulatorTraceIO myTrace