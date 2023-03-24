use crate::*;
use fp_mocks::*;
use pallet_evm_test_vector_support::test_precompile_test_vectors;

#[test]
fn process_consensus_tests() -> std::result::Result<(), String> {
    test_precompile_test_vectors::<Modexp>("../testdata/modexp_eip2565.json")?;
    Ok(())
}

#[test]
fn test_empty_input() -> std::result::Result<(), ExitError> {
    let input: [u8; 0] = [];

    let cost: u64 = 1;

    let context: Context = Context {
        address: Default::default(),
        caller: Default::default(),
        apparent_value: From::from(0),
    };

    match Modexp::execute(
        &input,
        Some(cost),
        &context,
        &BASE_APP.lock().unwrap().deliver_state,
    ) {
        Ok(_) => {
            panic!("Test not expected to pass");
        }
        Err(e) => {
            assert_eq!(
                e,
                PrecompileFailure::Error {
                    exit_status: ExitError::Other(
                        "input must contain at least 96 bytes".into()
                    )
                }
            );
            Ok(())
        }
    }
}

#[test]
fn test_insufficient_input() -> std::result::Result<(), ExitError> {
    let input = hex::decode(
        "0000000000000000000000000000000000000000000000000000000000000001\
        0000000000000000000000000000000000000000000000000000000000000001\
        0000000000000000000000000000000000000000000000000000000000000001",
    )
    .expect("Decode failed");

    let cost: u64 = 1;

    let context: Context = Context {
        address: Default::default(),
        caller: Default::default(),
        apparent_value: From::from(0),
    };

    match Modexp::execute(
        &input,
        Some(cost),
        &context,
        &BASE_APP.lock().unwrap().deliver_state,
    ) {
        Ok(_) => {
            panic!("Test not expected to pass");
        }
        Err(e) => {
            assert_eq!(
                e,
                PrecompileFailure::Error {
                    exit_status: ExitError::Other("insufficient input size".into())
                }
            );
            Ok(())
        }
    }
}

#[test]
fn test_excessive_input() -> std::result::Result<(), ExitError> {
    let input = hex::decode(
        "1000000000000000000000000000000000000000000000000000000000000001\
        0000000000000000000000000000000000000000000000000000000000000001\
        0000000000000000000000000000000000000000000000000000000000000001",
    )
    .expect("Decode failed");

    let cost: u64 = 1;

    let context: Context = Context {
        address: Default::default(),
        caller: Default::default(),
        apparent_value: From::from(0),
    };

    match Modexp::execute(
        &input,
        Some(cost),
        &context,
        &BASE_APP.lock().unwrap().deliver_state,
    ) {
        Ok(_) => {
            panic!("Test not expected to pass");
        }
        Err(e) => {
            assert_eq!(
                e,
                PrecompileFailure::Error {
                    exit_status: ExitError::Other(
                        "unreasonably large base length".into()
                    )
                }
            );
            Ok(())
        }
    }
}

#[test]
fn test_simple_inputs() {
    let input = hex::decode(
        "0000000000000000000000000000000000000000000000000000000000000001\
        0000000000000000000000000000000000000000000000000000000000000001\
        0000000000000000000000000000000000000000000000000000000000000001\
        03\
        05\
        07",
    )
    .expect("Decode failed");

    // 3 ^ 5 % 7 == 5

    let cost: u64 = 100000;

    let context: Context = Context {
        address: Default::default(),
        caller: Default::default(),
        apparent_value: From::from(0),
    };

    match Modexp::execute(
        &input,
        Some(cost),
        &context,
        &BASE_APP.lock().unwrap().deliver_state,
    ) {
        Ok(precompile_result) => {
            assert_eq!(precompile_result.output.len(), 1); // should be same length as mod
            let result = BigUint::from_bytes_be(&precompile_result.output[..]);
            let expected = BigUint::parse_bytes(b"5", 10).unwrap();
            assert_eq!(result, expected);
        }
        Err(_) => {
            panic!("Modexp::execute() returned error"); // TODO: how to pass error on?
        }
    }
}

#[test]
fn test_large_inputs() {
    let input = hex::decode(
        "0000000000000000000000000000000000000000000000000000000000000020\
        0000000000000000000000000000000000000000000000000000000000000020\
        0000000000000000000000000000000000000000000000000000000000000020\
        000000000000000000000000000000000000000000000000000000000000EA5F\
        0000000000000000000000000000000000000000000000000000000000000015\
        0000000000000000000000000000000000000000000000000000000000003874",
    )
    .expect("Decode failed");

    // 59999 ^ 21 % 14452 = 10055

    let cost: u64 = 100000;

    let context: Context = Context {
        address: Default::default(),
        caller: Default::default(),
        apparent_value: From::from(0),
    };

    match Modexp::execute(
        &input,
        Some(cost),
        &context,
        &BASE_APP.lock().unwrap().deliver_state,
    ) {
        Ok(precompile_result) => {
            assert_eq!(precompile_result.output.len(), 32); // should be same length as mod
            let result = BigUint::from_bytes_be(&precompile_result.output[..]);
            let expected = BigUint::parse_bytes(b"10055", 10).unwrap();
            assert_eq!(result, expected);
        }
        Err(_) => {
            panic!("Modexp::execute() returned error"); // TODO: how to pass error on?
        }
    }
}

#[test]
fn test_large_computation() {
    let input = hex::decode(
        "0000000000000000000000000000000000000000000000000000000000000001\
        0000000000000000000000000000000000000000000000000000000000000020\
        0000000000000000000000000000000000000000000000000000000000000020\
        03\
        fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2e\
        fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f",
    )
    .expect("Decode failed");

    let cost: u64 = 100000;

    let context: Context = Context {
        address: Default::default(),
        caller: Default::default(),
        apparent_value: From::from(0),
    };

    match Modexp::execute(
        &input,
        Some(cost),
        &context,
        &BASE_APP.lock().unwrap().deliver_state,
    ) {
        Ok(precompile_result) => {
            assert_eq!(precompile_result.output.len(), 32); // should be same length as mod
            let result = BigUint::from_bytes_be(&precompile_result.output[..]);
            let expected = BigUint::parse_bytes(b"1", 10).unwrap();
            assert_eq!(result, expected);
        }
        Err(_) => {
            panic!("Modexp::execute() returned error"); // TODO: how to pass error on?
        }
    }
}
