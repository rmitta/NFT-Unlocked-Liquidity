amt=$1
toAddr=$2
skeyFile=$3
changeAddr=$4
txIn=$5

cardano-cli transaction build \
    --babbage-era \
    --testnet-magic 1097911063 \
    --change-address $changeAddr \
    --tx-in $txIn \
    --tx-out $toAddr" "$amt" lovelace" \
    --tx-out-datum-hash-file testnet/unit.json \
    --out-file testnet/tx.body

cardano-cli transaction sign \
    --tx-body-file testnet/tx.body \
    --signing-key-file $skeyFile \
    --testnet-magic 1097911063 \
    --out-file testnet/tx.signed

cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file testnet/tx.signed
