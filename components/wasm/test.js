
// Call 'wasm-pack build --target nodejs' to build from this directory for this command to work
const wasm = require('./pkg/wasm.js');
const axios = require('axios');
const HOST = "localhost";
const SUBMISSION_PORT = "8669";

// Create some keypairs
let kp_alice = wasm.new_keypair();

let define = function() {
let memo = "test asset";
let definition_transaction = wasm.WasmTransactionBuilder.new().add_operation_create_asset(kp_alice, memo, "").transaction();

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
}

setInterval(define, 1000);
