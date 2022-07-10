
txClaim=$1
receiveAddrFile=$2
skeyFile=$3
authAddrFile=$4
txInCollat=$5

receiveAddr=$(cat $receiveAddrFile)
authAddr=$(cat $authAddrFile)

policyFile=testnet/testToken.policy
cabal exec token-policy $policyFile authAddr

pid=$(cardano-cli transaction policyid --script-file $policyFile)

cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --change-address $receiveAddr \
    --tx-in $txClaim \
    --tx-in-script-file testnet/checkToken.script \
    --tx-in-datum-file testnet/unit.json \
    --tx-in-redeemer-file testnet/unit.json \
    --tx-in-collateral $txInCollat \
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
