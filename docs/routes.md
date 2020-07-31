# Findora Ledger Server Routes

``air_address/``{*key*}
: the address of *key*.

``asset_issuance_num/``{*code*}
: the issuance number of *code*.

``asset_token/``{*code*}
: the asset token for *code*.

``block_log``
: the block log.

``blocks_since/``{*block_sid*}
: the block log as a list of JSON expressions beginning with block *block_sid*.

``global_state``
: the global state commitment.

``global_state_version/``{*version*}
: asdf.

``kv_lookup/``{*address*}
: the value of *address*.

``ping``
: a value indicating that the Ledger is up and responding.

``public_key``
: the Ledger public key

``txn_sid/``{*sid*}
: the transaction with ID *sid*. If there is no such transaction, return an error.

``utxo_map/``{*sid*}
: the unspent transaction output map for the given ID *sid*.

``utxo_map_checksum``
: the checksum of the unspent transaction output map.

``utxo_partial_map/``{*sidlist*}
: the unspent transaction output map entries for the given list of IDs *sidlist*.

``utxo_sid/``{*sid*}
: the unspent transaction output with ID *sid*.

``version``
: the Ledger Server version

# Findora Submission Server Routes

``force_end_block`` [POST]
: Normally, transactions are posted to the Ledger in batches. To aid development, this route causes the block in progress to be posted immediately, even if it is not full.

``ping``
: a value indicating that the Submission Server is up and responding

``submit_transaction`` [POST]
: Submit a transaction.

``txn_status/``{*handle*}
: the status for the given transaction *handle*.

``version``
: the Submission Server version.

# Findora Query Server Routes

``get_address/``{*txo_sid*}
: the address of the given transaction output *txo_sid*.

``get_created_assets/``{*address*}
: the created assets at the given *address*.

``get_custom_data/``{*key*}
: the custom data at *key*.

``get_issued_records``
: the issued records.

``get_owned_utxos/``{*address*}
: the unspent transaction outputs owned by *address*.

``get_owner_memo/``{*address*}
: the owner memo for the given transaction *address*.

``get_related_txns/``{*address*}
: the transactions associated with the given Ledger *address*.

``store_custom_data`` [POST]
: Stores custom data at a given key.

``version``
: the Query Server version.
