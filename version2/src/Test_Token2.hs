
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

module Test_Token2 where

import           Control.Monad          hiding (fmap)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Data.Maybe             (fromJust)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import Ledger.Value as Value ( flattenValue, singleton )
import           Prelude                (IO, Semigroup (..), Show (..), String, undefined)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

--{-# INLINABLE tokenName #-}
tokenName :: TokenName
tokenName = "DirectEd Token"

{-# INLINABLE mkPolicy #-}
mkPolicy :: TokenName -> PaymentPubKeyHash -> () -> ScriptContext -> Bool
mkPolicy tn pkh () ctx = (txSignedBy txInfo (unPaymentPubKeyHash pkh) &&
                            isNamedCorrectly)
                        || isBurn
    where
        txInfo = scriptContextTxInfo ctx

        isNamedCorrectly = case flattenValue $ txInfoMint txInfo of
                                    [(_,tn',_)] -> tn == tn'
                                    _           -> False
        
        isBurn = case flattenValue $ txInfoMint txInfo of
                          [(_,_,n)] -> n < 0
                          _         -> False  

policy :: PaymentPubKeyHash -> Scripts.MintingPolicy
policy pkh = mkMintingPolicyScript $
  $$(PlutusTx.compile [|| \tn' pkh' -> Scripts.wrapMintingPolicy $ mkPolicy tn' pkh' ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode Test_Token.tokenName
  `PlutusTx.applyCode`
  PlutusTx.liftCode pkh

curSymbol :: PaymentPubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

type TokenSchema = Endpoint "mint" Integer
                   .\/ Endpoint "burn" (Integer, PaymentPubKeyHash)

mint :: Integer -> Contract w TokenSchema Text ()
mint n = do
    pkh <- Contract.ownPaymentPubKeyHash
    let val = singleton (curSymbol pkh) Test_Token.tokenName n
        lookups = Constraints.mintingPolicy $ policy pkh
        tx = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "minted %s" (show val)

burn :: (Integer, PaymentPubKeyHash) -> Contract w TokenSchema Text ()
burn (n, pkh) = do
    let val = singleton (curSymbol pkh) Test_Token.tokenName (- n)
        lookups = Constraints.mintingPolicy $ policy pkh
        tx = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "minted %s" (show val)

endpoints :: Contract () TokenSchema Text ()
endpoints = awaitPromise (mint' `select` burn') >> endpoints
    where
        mint' = endpoint @"mint" mint
        burn' = endpoint @"burn" burn

test :: IO ()
test = runEmulatorTraceIO $ do
    let w1 = knownWallet 1
    h1 <- activateContractWallet w1 endpoints
    callEndpoint @"mint" h1 $ 1337
    void $ Emulator.waitNSlots 1

test2 :: IO ()
test2 = runEmulatorTraceIO $ do
    let w1 = knownWallet 1
    let w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"mint" h1 $ 1337
    void $ Emulator.waitNSlots 1
    let w1State = emptyWalletState w1
    let pkh1 = PaymentPubKeyHash $ fromJust $ fmap ownAddress w1State >>= toPubKeyHash
    let val = singleton (curSymbol pkh1) Test_Token2.tokenName 1
    payToWallet w1 w2 val 
    void $ Emulator.waitNSlots 1
    callEndpoint @"burn" h2 $ (1, pkh1)
    void $ Emulator.waitNSlots 1


--simulateTrace stuff
--activate a contract on a wallet.
--call an endpoint on that wallet.

-- MintingValue
-- SignedBy (our own wallet)????



