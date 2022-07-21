#!/bin/bash

amt=10
tn="DirectEd Token"
addrFile=testnet/Authority.addr
skeyFile=testnet/Authority.skey
oref=2f68c1ec3937dfcf3d9f70b8e92b2dd339002b54db06615ca0a7c4aa35b77cab#0 

echo "amt: $amt"
echo "tn: $tn"
echo "address file: $addrFile"
echo "signing key file: $skeyFile"
echo "oref: $oref"

ppFile=testnet/protocol-parameters.json
cardano-cli query protocol-parameters $MAGIC --out-file $ppFile

addr=$(cat $addrFile)

policyFile=testnet/testToken.policy
cabal exec token-policy $policyFile $addr

unsignedFile=testnet/tx.unsigned
signedFile=testnet/tx.signed
pid=$(cardano-cli transaction policyid --script-file $policyFile)

tnHex=$(cat testnet/testToken.nameHex)
#tnHex=$(cabal exec token-name -- $tn)

v="$amt $pid.$tnHex"

echo "currency symbol: $pid"
echo "token name (hex): $tnHex"
echo "minted value: $v"
echo "address: $addr"

cardano-cli transaction build \
    $MAGIC \
    --babbage-era \
    --tx-in $oref \
    --tx-in-collateral $oref \
    --tx-out "$addr + 1500000 lovelace + $v" \
    --mint "$v" \
    --mint-script-file $policyFile \
    --mint-redeemer-file testnet/unit.json \
    --change-address $addr \
    --protocol-params-file $ppFile \
    --required-signer $skeyFile \
    --out-file $unsignedFile \

cardano-cli transaction sign \
    --tx-body-file $unsignedFile \
    --signing-key-file $skeyFile \
    $MAGIC \
    --out-file $signedFile

cardano-cli transaction submit \
    $MAGIC \
    --tx-file $signedFile
