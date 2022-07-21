
txClaim=e701001959e4b28db6c939ae659ad57ebe21ed1291b236fd0d29777bf91c7f1a#1
receiveAddrFile=testnet/Recipient.addr
skeyFile=testnet/Recipient.skey
authAddrFile=testnet/Authority.addr
tn="DirectEd Token"
tnAmt=1
txToken=c8d196339e7bd4fae13bc3633571125028233c2a11aba47d8ec308deeaaca474#1
txInCollat=25338eeba10ecc5202bbb02857257062dd3f7e0f4738fb32705d27e3b005adfd#0

tnHex=$(cabal exec token-name "$tn")

receiveAddr=$(cat $receiveAddrFile)
authAddr=$(cat $authAddrFile)

policyFile=testnet/testToken.policy
cabal exec token-policy $policyFile $authAddr

pid=$(cardano-cli transaction policyid --script-file $policyFile)
v="$tnAmt $pid.$tnHex"

cardano-cli transaction build \
    --babbage-era \
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
