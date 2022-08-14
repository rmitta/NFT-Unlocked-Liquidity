-- Contract: 
-- 1. Accepts money from anyone. (Record who the money is from so that it is possible to send some of it back.)
-- 2. Students can apply:
--                     - Similar to BID, they supply their wallet ID and a Token.
-- 3. After a deadline is reached, CLOSE will cause:
--                                              - It checks how many scholarships worth of money is available
--                                              - It !randomely! (actually right now just selects students starting from the earliest applicant) selects that many students from the pool
--                                              - It sends the scholarships to those students, and any excess money is distributed back to the donors 

-- VERSION 1: If you have a token, you get all the money at the script.
-- VERSION 2: You burn your token in order to be able to withdraw up to a certain (10) amount of ADA. Script scans through all
--     of this script's inputs to check total. Also checks total deposited (in case you need to give some back)

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
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE NamedFieldPuns      #-}

{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}




module Test_Contract2 where

import           Control.Monad          hiding (fmap)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import           Ledger.Ada             as Ada
import           Ledger.Contexts        as Contexts
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet
import           Test_Token2             as Test_Token

withdrawLimit :: Value.Value
withdrawLimit = lovelaceValueOf 10_000_000

{-# INLINABLE getScriptInputs #-}
-- Gets all transaction inputs that come from the script address being validated
getScriptInputs :: ScriptContext -> [TxInInfo]
getScriptInputs ctx
    | Just TxInInfo{txInInfoResolved=TxOut{txOutAddress}} <- findOwnInput ctx =
    filter (\txInInfo -> Contexts.txOutAddress (Contexts.txInInfoResolved txInInfo) == txOutAddress) (txInfoInputs $ scriptContextTxInfo ctx)
    | Nothing <- findOwnInput ctx = [] --This case is impossible.

{-# INLINABLE mkValidator #-}
mkValidator :: CurrencySymbol -> TokenName -> PaymentPubKeyHash -> () -> () -> ScriptContext -> Bool
mkValidator cursym tn _ () () ctx = consumesCorrectToken && withdrawUpToLimit
    where
        txInfo = scriptContextTxInfo ctx
        valueMinted = txInfoMint txInfo :: Value
        consumesCorrectToken = valueOf valueMinted cursym tn == (-1) :: Bool

        scriptInputs = getScriptInputs ctx :: [TxInInfo]
        valueWithdrew = foldMap (txOutValue . txInInfoResolved) scriptInputs
        adaWithdrew = Ada.fromValue valueWithdrew

        continuingOutputs = getContinuingOutputs ctx :: [TxOut]
        valueDeposited = foldMap txOutValue continuingOutputs
        adaDeposited = Ada.fromValue valueDeposited

        withdrawUpToLimit = adaWithdrew - adaDeposited <= Ada.fromValue withdrawLimit :: Bool
--An alternative way to write this would be to mandate 1 input 1 output to the script. (At least once we have
-- some datum attached)

data Contract1Type
instance Scripts.ValidatorTypes Contract1Type where
    type instance DatumType Contract1Type = ()
    type instance RedeemerType Contract1Type = ()

typedValidator :: PaymentPubKeyHash -> Scripts.TypedValidator Contract1Type
typedValidator pkh = Scripts.mkTypedValidator @Contract1Type
    ($$(PlutusTx.compile [|| mkValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode (Test_Token.curSymbol pkh)
        `PlutusTx.applyCode` PlutusTx.liftCode Test_Token.tokenName
        `PlutusTx.applyCode` PlutusTx.liftCode pkh)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()

validator :: PaymentPubKeyHash -> Validator
validator = Scripts.validatorScript . typedValidator

valHash :: PaymentPubKeyHash -> Ledger.ValidatorHash
valHash = Scripts.validatorHash . typedValidator

scrAddress :: PaymentPubKeyHash -> Ledger.Address
scrAddress = scriptAddress . validator

type Contract1Schema = Endpoint "grab" ()
                      .\/ Endpoint "give" Integer


give :: PaymentPubKeyHash -> Integer -> Contract w Contract1Schema Text ()
give pkh amount = do
    pkhOwn <- Contract.ownPaymentPubKeyHash
    utxos <- utxosAt $ pubKeyHashAddress pkhOwn Nothing
    let
        lookups = Constraints.unspentOutputs utxos
        tx = Constraints.mustPayToOtherScript (valHash pkh) (Datum $ PlutusTx.toBuiltinData ()) (Ada.lovelaceValueOf amount)
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "gave %s lovelace to the script address" (show amount)


grab :: PaymentPubKeyHash -> () -> Contract w Contract1Schema Text ()
grab pkh () = do
    utxos <- utxosAt $ scrAddress pkh
    pkhOwn <- Contract.ownPaymentPubKeyHash
    ownUtxos <- utxosAt $ pubKeyHashAddress pkhOwn Nothing -- This nothing is questionable, what if we are staking!
    let t               = Value.singleton (Test_Token.curSymbol pkh) Test_Token.tokenName 1
        orefs           = fst <$> Map.toList utxos
--        a               = map (^.ciTxOutValue) $ snd <$> Map.toList utxos
        lookups         = Constraints.unspentOutputs (Map.union utxos ownUtxos) <>
                            Constraints.otherScript (validator pkh) <> Constraints.ownPaymentPubKeyHash pkhOwn -- to fix "missing ownPKH"
--       totalAvailable  = 1
        tx :: TxConstraints Void Void
        tx      = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData ()| oref <- orefs]
                    <> Constraints.mustSpendAtLeast t <> Constraints.mustMintValue (negate t)
    ledgerTx <- adjustAndSubmitWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "grabbed some money and burned the token"
---Currently tries to grab everything. Needs to be changed to grabbing up to 10 ADA. This requires actually picking 
---certain UTXOs to use, OR requires SC design that only permits one UTXO at a time (though that doesn't actually make 
---100% sense, since anyone can always put money at a SC

--TODO: Calculate total of mustSpendScriptOutputs, 
--      slOwnPaymentPubKeyHash to get the contract's pubkeyhash
--      Constraint that we must paytoPubKeyHash total - 10ADA. 

adjustAndSubmitWith :: ( PlutusTx.FromData (Scripts.DatumType a)
                       , PlutusTx.ToData (Scripts.RedeemerType a)
                       , PlutusTx.ToData (Scripts.DatumType a)
                       , AsContractError e
                       )
                    => ScriptLookups a
                    -> TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a)
                    -> Contract w s e CardanoTx
adjustAndSubmitWith lookups constraints = do
    unbalanced <- adjustUnbalancedTx <$> mkTxConstraints lookups constraints
    Contract.logDebug @String $ printf "unbalanced: %s" $ show unbalanced
    unsigned <- balanceTx unbalanced
    Contract.logDebug @String $ printf "balanced: %s" $ show unsigned
    signed <- submitBalancedTx unsigned
    Contract.logDebug @String $ printf "signed: %s" $ show signed
    return signed

endpoints :: PaymentPubKeyHash -> Contract () Contract1Schema Text ()
endpoints pkh = awaitPromise (grab' `select` give') >> Test_Contract2.endpoints pkh
    where
        grab' = endpoint @"grab" (grab pkh)
        give' = endpoint @"give" (give pkh)


test :: IO ()
test = runEmulatorTraceIO $ do
    let w1 = knownWallet 1
    let w2 = knownWallet 2
    let pkh1 = mockWalletPaymentPubKeyHash w1
    h1 <- activateContractWallet w1 Test_Token.endpoints
    h1' <- activateContractWallet w1 $ Test_Contract2.endpoints pkh1
    h2' <- activateContractWallet w2 $ Test_Contract2.endpoints pkh1
    callEndpoint @"mint" h1 1337
    void $ Emulator.waitNSlots 1
    callEndpoint @"give" h2' 10000000
    void $ Emulator.waitNSlots 1
    callEndpoint @"grab" h1' ()
    void $ Emulator.waitNSlots 1
