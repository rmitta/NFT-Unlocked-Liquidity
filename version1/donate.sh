amt=$1
authAddrFile=$2
skeyFile=$3
changeAddr=$4
txIn=$5

authAddr=$(cat $authAddrFile)

scriptFile=testnet/checkToken.script
cabal exec checkToken-script $policyFile authAddr

./sendProper.sh $amt $(cat $scriptFile) $skeyFile $changeAddr $txIn
