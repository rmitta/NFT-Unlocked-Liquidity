amt=1
changeAmt=9
tn="DirectEd Token"
sendAddrFile=testnet/Authority.addr
skeyFile=testnet/Authority.skey
receiveAddrFile=testnet/Recipient.addr
authAddrFile=testnet/Authority.addr
#may require two inputs, one with tokens typically cannot be split.
oref1=69f3847abf9835fd912008fc2cd71059c585539f26ead7b489e91e4706ed0723#1
oref2=69f3847abf9835fd912008fc2cd71059c585539f26ead7b489e91e4706ed0723#0

echo "amt: $amt"
echo "changeAmt = $changeAmt"
echo "tn: $tn"
echo "sending address file: $sendAddrFile"
echo "signing key file: $skeyFile"
echo "receiving address file: $receiveAddrFile"
echo "oref1: $oref1"
echo "oref2: $oref2"

ppFile=testnet/protocol-parameters.json
cardano-cli query protocol-parameters $MAGIC --out-file $ppFile

sendAddr=$(cat $sendAddrFile)
receiveAddr=$(cat $receiveAddrFile)
authAddr=$(cat $authAddrFile)

policyFile=testnet/testToken.policy
cabal exec token-policy $policyFile $authAddr

unsignedFile=testnet/tx.unsigned
signedFile=testnet/tx.signed
pid=$(cardano-cli transaction policyid --script-file $policyFile)

tnHex=$(cabal exec token-name "$tn")


v="$amt $pid.$tnHex"
returnV="$changeAmt $pid.$tnHex"

echo "token name (hex): $tnHex"
echo "send address: $sendAddr"
echo "receive address: $receiveAddr"
echo "currency symbol: $pid"

cardano-cli transaction build \
    $MAGIC \
    --babbage-era \
    --change-address $sendAddr \
    --tx-in $oref1 \
    --tx-in $oref2 \
    --tx-out "$receiveAddr 1500000 lovelace + $v" \
    --tx-out "$sendAddr 1500000 lovelace + $returnV" \
    --protocol-params-file $ppFile \
    --out-file $unsignedFile \

cardano-cli transaction sign \
    --tx-body-file $unsignedFile \
    --signing-key-file $skeyFile \
    $MAGIC \
    --out-file $signedFile

cardano-cli transaction submit \
    $MAGIC \
    --tx-file $signedFile

echo "sent value: $v"