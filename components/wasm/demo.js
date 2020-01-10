// Call 'wasm-pack build --target nodejs' to build from this directory for this command to work
let wasm = require('./pkg/wasm.js');

// Create some keypairs
let kp_alice = wasm.new_keypair();
let kp_bob = wasm.new_keypair();
let kp_charlie = wasm.new_keypair();
let kp_auditor = wasm.new_keypair();

// And a separate tracking key for the auditor
let tracking_kp_auditor = wasm.generate_elgamal_keys();

// Alice will define an asset
let token_code = "test";
let memo = "test memo";
let definition_transaction = wasm.WasmTransactionBuilder.new().add_operation_create_asset(kp_alice, memo, token_code, false, false).transaction();

// At this point, transaction would be submitted to the ledger
// Once the definition transaction succeeds, Alice can issue and transfer 1000 units to herself
let issue_op = wasm.WasmTransactionBuilder.new().add_operation_issue_asset(kp_alice, "", token_code, 0n, 1000n).transaction();

// TODO (noah) add more examples









