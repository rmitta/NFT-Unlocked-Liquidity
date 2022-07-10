
txClaim=$1
receiveAddrFile=$2
skeyFile=$3
authAddrFile=$4
tn=$5
tnAmt=$6
txToken=$7
txInCollat=$8

tnHex=$(cabal exec token-name -- $tn)

receiveAddr=$(cat $receiveAddrFile)
authAddr=$(cat $authAddrFile)

policyFile=testnet/testToken.policy
cabal exec token-policy $policyFile authAddr

pid=$(cardano-cli transaction policyid --script-file $policyFile)
v="$amt $pid.$tnHex"

cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --change-address $receiveAddr \
    --tx-in $txClaim \
    --tx-in-script-file testnet/checkToken.script \
    --tx-in-datum-file testnet/unit.json \
    --tx-in-redeemer-file testnet/unit.json \
    --tx-in $txToken \
    --tx-in-collateral $txInCollat \
    --tx-out "$receiveAddr 1500000 lovelace + $v" \
    --protocol-params-file testnet/protocol-parameters.json \
    --out-file testnet/tx.body

cardano-cli transaction sign \
    --tx-body-file testnet/tx.body \
    --signing-key-file $skeyFile \
    --testnet-magic 1097911063 \
    --out-file testnet/tx.signed

cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file testnet/tx.signed
