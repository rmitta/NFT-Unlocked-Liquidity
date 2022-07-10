#!/bin/bash

amt=$1
tn=$2
addrFile=$3
skeyFile=$4
oref1=$5
oref2=$6

echo "amt: $amt"
echo "tn: $tn"
echo "address file: $addrFile"
echo "signing key file: $skeyFile"
echo "oref1: $oref1"
echo "oref2: $oref2"

ppFile=testnet/protocol-parameters.json
cardano-cli query protocol-parameters $MAGIC --out-file $ppFile

addr=$(cat $addrFile)

policyFile=PPPToken2.plutus
#policyFile=testnet/testToken.policy
#cabal exec token-policy $policyFile $addr

unsignedFile=testnet/tx.unsigned
signedFile=testnet/tx.signed
pid=$(cardano-cli transaction policyid --script-file $policyFile)

tnHex=$(cat testnet/testToken.nameHex)
#tnHex=$(cabal exec token-name -- $tn)

v="-$amt $pid.$tnHex"

echo "currency symbol: $pid"
echo "token name (hex): $tnHex"
echo "minted value: $v"
echo "address: $addr"

cardano-cli transaction build \
    $MAGIC \
    --babbage-era \
    --tx-in $oref1 \
    --tx-in-collateral $oref2 \
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
