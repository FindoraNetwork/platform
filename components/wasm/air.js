let src = require("./pkg/wasm.js");
let attribute_def = [{name: "credit_score", size: 3}];
let attributes = [{name: "credit_score", val: "760"}];
let check_attributes = [{name: "credit_score", val: "760"}];
let reveal_fields = ["credit_score"];
let issuer = src.wasm_credential_issuer_key_gen(attribute_def);
let user_sign_key = src.new_keypair();
let user = src.wasm_credential_user_key_gen(issuer.get_pk());
let sig = src.wasm_credential_sign(issuer.get_sk(), user.get_pk(), attributes);
let credential = src.create_credential(issuer.get_pk(), sig, attributes);
let commitment = src.wasm_credential_commit(user.get_sk(), user_sign_key.get_pk(), credential);
let reveal_sig = src.wasm_credential_reveal(user.get_sk(), credential, reveal_fields);
src.wasm_credential_verify(issuer.get_pk(), check_attributes, reveal_sig);


