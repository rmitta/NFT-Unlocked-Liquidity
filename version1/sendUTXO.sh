toAddr=$1
skeyFile=$2
txIn=$3

cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 1097911063 \
    --change-address $toAddr \
    --tx-in $txIn \
    --out-file testnet/tx.body

cardano-cli transaction sign \
    --tx-body-file testnet/tx.body \
    --signing-key-file $skeyFile \
    --testnet-magic 1097911063 \
    --out-file testnet/tx.signed

cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file testnet/tx.signed
