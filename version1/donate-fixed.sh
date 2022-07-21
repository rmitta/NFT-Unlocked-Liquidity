amt=10000000
authAddrFile=testnet/Authority.addr
skeyFile=testnet/Donor.skey
changeAddrFile=testnet/Donor.addr
txIn=542e7d14fbbd824ccf8fecbab8a5466478d38e67128c857ed9baae1da4a9881c#0

authAddr=$(cat $authAddrFile)
changeAddr=$(cat $changeAddrFile)

scriptAddress=$(cat testnet/checkToken.addr)

echo $amt
echo $scriptAddress 
echo $skeyFile 
echo $changeAddr
echo $txIn

./send.sh $amt $scriptAddress $skeyFile $changeAddr $txIn
