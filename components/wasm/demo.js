// Call 'wasm-pack build --target nodejs' to build from this directory for this command to work
let wasm = require('./pkg/wasm.js');

// Create some keypairs
let kp_alice = wasm.new_keypair();
let kp_bob = wasm.new_keypair();
let kp_charlie = wasm.new_keypair();
let kp_auditor = wasm.new_keypair();
console.log(kp_alice);

// And a separate tracking key for the auditor
let tracking_kp_auditor = wasm.generate_elgamal_keys();

// Alice will define an asset
let token_code = "abcd";
let memo = "test memo";
let definition_transaction = wasm.WasmTransactionBuilder.new().add_operation_create_asset(kp_alice, memo, token_code, false, false).transaction();
console.log(definition_transaction);


const axios = require('axios')

axios.post('http://localhost:8669/submit_transaction_standalone/'+  definition_transaction,{}, {
    headers: {
      'Content-Type': 'application/x-www-form-urlencoded',
      'Accept': '*/*'
    }
  })
.then((res) => {
    console.log(`statusCode: ${res.statusCode}`)
    console.log(res)
})
.catch((error) => {
    console.error(error)
})

//let submit = wasm.submit_transaction(definition_transaction);

// At this point, transaction would be submitted to the ledger
// Once the definition transaction succeeds, Alice can issue and transfer 1000 units to herself
let issue_op = wasm.WasmTransactionBuilder.new().add_operation_issue_asset(kp_alice, "", token_code, 0n, 1000n).transaction();

// TODO (noah) add more examples









