toAddr=$1
skeyFile=$2
txIn1=$3
txIn2=$4
txIn3=$5
txIn4=$6
txIn5=$7


cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 1097911063 \
    --change-address $toAddr \
    --tx-in $txIn1 \
    --tx-in $txIn2 \
    --tx-in $txIn3 \
    --tx-in $txIn4 \
    --tx-in $txIn5 \
    --out-file testnet/tx.body

cardano-cli transaction sign \
    --tx-body-file testnet/tx.body \
    --signing-key-file $skeyFile \
    --testnet-magic 1097911063 \
    --out-file testnet/tx.signed

cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file testnet/tx.signed
