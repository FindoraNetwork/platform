// build.rs

extern crate codegen;
extern crate serde;
extern crate serde_json;

use codegen::{Scope, Type};
use serde_json::{Map, Value};
use std::env;
use std::error::Error;
use std::fs::File;
use std::io::BufReader;
use std::io::Write;
use std::path::Path;

fn read_schema_from_file<P: AsRef<Path>>(path: P) -> Result<Value, Box<dyn Error>> {
  // Open the file in read-only mode with buffer.
  let file = File::open(path)?;
  let reader = BufReader::new(file);

  // Read the JSON contents of the file as an instance of `User`.
  let v = serde_json::from_reader(reader)?;

  Ok(v)
}

fn extract_string(value: &Value) -> &String {
  match value {
    Value::String(s) => return s,
    _ => panic!("Expected a string"),
  }
}

fn to_pair(value: &Value) -> (&String, &String) {
  match value {
    Value::Array(v) => match v.as_slice() {
      [l, r] => (extract_string(l), extract_string(r)),
      _ => panic!("expected a pair"),
    },
    _ => panic!("Expected a string"),
  }
}

fn extract_args(value: &Value) -> Vec<(&String, &String)> {
  match value {
    Value::Array(v) => v.into_iter().map(|p| to_pair(p)).collect(),
    _ => panic!("Expected a string"),
  }
}

fn add_wasm_func(scope: &mut Scope, fn_desc: &Map<String, Value>) {
  scope.raw("#[wasm_bindgen]");
  let fn_name = extract_string(&fn_desc["name"]);
  let new_fn = scope.new_fn(&fn_name);
  let args = extract_args(&fn_desc["args"]);
  for (arg_name, arg_type) in args {
    new_fn.arg(arg_name, arg_type);
  }
  new_fn.vis("pub");
  new_fn.ret(Type::new(&extract_string(&fn_desc["result"])));
  new_fn.line("match split_key_pair(&key_pair) {");
  new_fn.line("    Ok((public_key, secret_key)) => {");
  if &fn_name[..] == "create_asset" || &fn_name[..] == "issue_asset" {
    new_fn.line("        let asset_token = AssetTypeCode::new_from_base64(&token_code).unwrap();");
    new_fn.line("        let mut txn_builder = TransactionBuilder::default();");
  } else if &fn_name[..] == "transfer_asset" {
    new_fn.line("        let mut txn_builder = TransactionBuilder::default();");
    new_fn.line("        let blind_asset_record = serde_json::from_str::<BlindAssetRecord>(&blind_asset_record_str).unwrap();");
  } else {
    panic!("unhandled function name");
  }

  if &fn_name[..] == "create_asset" {
    new_fn.line("        match txn_builder.add_operation_create_asset(&IssuerPublicKey { key: public_key },");
    new_fn.line("                                                     &secret_key,");
    new_fn.line("                                                     Some(asset_token),");
    new_fn.line("                                                     updatable,");
    new_fn.line("                                                     traceable,");
    new_fn.line("                                                     &String::from(\"{}\"),");
    new_fn.line("                                                     true) {");
    new_fn.line("          Ok(_) => Ok(txn_builder.serialize_str().unwrap()),");
    new_fn.line("          Err(_) => Err(JsValue::from_str(\"Could not add operation create_asset to transaction\"))");
    new_fn.line("        }");
  } else if &fn_name[..] == "issue_asset" {
    new_fn.line("        match txn_builder.add_basic_issue_asset(&IssuerPublicKey { key: public_key },");
    new_fn.line("                                                &secret_key,");
    new_fn.line("                                                &asset_token,");
    new_fn.line("                                                seq_num,");
    new_fn.line("                                                amount) {");
    new_fn.line("          Ok(_) => Ok(txn_builder.serialize_str().unwrap()),");
    new_fn.line("          Err(_) => Err(JsValue::from_str(\"Could not add operation issuee_asset to transaction\"))");
    new_fn.line("        }");
  } else if &fn_name[..] == "transfer_asset" {
    new_fn.line("        match txn_builder.add_basic_transfer_asset(&[(&TxoRef::Absolute(TxoSID(txo_sid)),");
    new_fn.line("                                                      &blind_asset_record,");
    new_fn.line("                                                      amount,");
    new_fn.line("                                                      &secret_key)],");
    new_fn.line("                                                   &[(amount,  &AccountAddress { key: public_key })]) {");
    new_fn.line("          Ok(_) => Ok(txn_builder.serialize_str().unwrap()),");
    new_fn.line("          Err(_) => Err(JsValue::from_str(\"Could not add operation issuee_asset to transaction\"))");
    new_fn.line("        }");
  } else {
    panic!("unhandled function name");
  }

  new_fn.line("    },");
  new_fn.line("    _ => Err(JsValue::from_str(\"Could not deserialize key pair\"))");
  new_fn.line("}");
}

fn add_wasm_funcs(mut scope: &mut Scope, vv: &Vec<Value>) {
  for v in vv {
    if let Value::Object(fn_desc) = v {
      add_wasm_func(&mut scope, &fn_desc);
    }
  }
}

fn main() -> Result<(), std::io::Error> {
  let json: Value = read_schema_from_file("input.json").unwrap();
  let out_dir = env::var("OUT_DIR").unwrap();
  let dest_path = Path::new(&out_dir).join("wasm.rs");
  let mut out_file = File::create(&dest_path).unwrap();

  let mut scope = Scope::new();

  match &json["wasm-funcs"] {
    Value::Array(vv) => add_wasm_funcs(&mut scope, vv),
    _ => panic!("Invalid JSON"),
  }

  out_file.write_all(scope.to_string().as_bytes()).unwrap();

  Ok(())
}
