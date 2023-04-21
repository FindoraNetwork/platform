
// Call 'wasm-pack build --target nodejs' to build from this directory for this command to work
const wasm = require('./pkg/wasm.js');
const axios = require('axios');
const HOST = "localhost";
const SUBMISSION_PORT = "8669";

// Create some keypairs
let kp_alice = wasm.new_keypair();

let define = function() {
let memo = "test asset";
let asset_type = wasm.random_asset_type();
let definition_transaction = wasm.WasmTransactionBuilder.new().add_operation_create_asset(kp_alice, memo, asset_type).transaction();

// define an asset
let route = 'http://' + HOST + ':' + SUBMISSION_PORT;

axios.post(route + '/submit_transaction', JSON.parse(definition_transaction))
    .then(function(response) {
        console.log("Successfully defined asset.");
    })
    .catch(function(e) {
        console.log("Error defining asset. Perhaps the asset has already been created?");
        console.log(e);
    });

let issuance_transaction = wasm.WasmTransactionBuilder.new()
  .add_basic_issue_asset(kp_alice, "", asset_type, 0n, 1000n)
  .transaction();

axios.post(route + '/submit_transaction', JSON.parse(issuance_transaction))
    .then(function(response) {
        console.log("Successfully issued asset.");
    })
    .catch(function(e) {
        console.log("Error issuing asset.");
        console.log(e);
    });
}

setInterval(define, 1000);
