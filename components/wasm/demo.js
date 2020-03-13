// Call 'wasm-pack build --target nodejs' to build from this directory for this command to work
const wasm = require('./pkg/wasm.js');
const axios = require('axios');
const HOST = "localhost";
const SUBMISSION_PORT = "8669";
const QUERY_PORT= "8668";

// Create some keypairs
let kp_alice = wasm.new_keypair();
let kp_bob = wasm.new_keypair();
let kp_charlie = wasm.new_keypair();
let kp_auditor = wasm.new_keypair();

console.log(wasm.public_key_to_base64(kp_bob.get_pk()));

// And a separate tracking key for the auditor
let tracking_kp_auditor = wasm.generate_elgamal_keys();

// Alice will define an asset
let token_code = wasm.random_asset_type();
let memo = "test memo";
let definition_transaction = wasm.WasmTransactionBuilder.new().add_operation_create_asset(kp_alice, memo, token_code).transaction();

let route = 'http://' + HOST + ':' + SUBMISSION_PORT;
let ledger = 'http://' + HOST + ':' + QUERY_PORT;

axios.post(route + '/submit_transaction', JSON.parse(definition_transaction))
    .then(function(response) {
        console.log("Successfully defined asset.");
    })
    .catch(function(e) {
        console.log("Error defining asset. Perhaps the asset has already been created?");
        console.log(e);
    });

// At this point, transaction would be submitted to the ledger
// Once the definition transaction succeeds, Alice can issue and transfer 1000 units to herself
// Sometimes, it is necessary to have a handle on the issuance output for complicated operations like issuances + transfers. 
// Here is an example:

// Manually construct issuance output
let conf_amount = false;
let conf_type = false;
let blind_asset_record = wasm.create_blind_asset_record(1000n, token_code, kp_alice.get_pk(), conf_amount, conf_type);

// Construct transfer operation first
let transfer_op = wasm.WasmTransferOperationBuilder.new()
    .add_input(wasm.create_relative_txo_ref(0n),
        wasm.open_blind_asset_record(blind_asset_record, kp_alice),
        1000n)
    .add_output(1000n, kp_bob.get_pk(), token_code)
    .create(wasm.standard_transfer_type())
    .sign(kp_alice)
    .transaction();

// Txn with issuance and transfer operation
let issue_and_transfer_txn = wasm.WasmTransactionBuilder.new()
    .add_operation_issue_asset(kp_alice, token_code, 1n, blind_asset_record)
    .add_operation(transfer_op)
    .transaction();

axios.post(route + '/submit_transaction', JSON.parse(issue_and_transfer_txn))
    .then(function(response) {
        console.log("Issued and transferred asset.");

    })
    .catch(function(_) {
        console.log("Error issuing and transferring asset");
    });


