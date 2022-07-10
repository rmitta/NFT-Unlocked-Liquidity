addrFile=$1

cardano-cli query utxo --address $(cat $addrFile) --testnet-magic 1097911063
