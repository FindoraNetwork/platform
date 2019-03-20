#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/transaction_hooks.rs"));

// use libc;



#[no_mangle]
#[link_name = "\u{1}_DeliverTransaction"]
pub extern "C" fn DeliverTransaction(txn: *const BlindTransaction, result: *mut BlindTransactionResult) -> bool {

    // ...  
    true
}
#[no_mangle]
#[link_name = "\u{1}_CommitTransaction"]
pub extern "C" fn CommitTransaction(txn: *const BlindTransaction, result: *const BlindTransactionResult) -> bool {
    // ...  
    true
}
#[no_mangle]
#[link_name = "\u{1}_CheckTransaction"]
pub extern "C" fn CheckTransaction(txn: *const BlindTransaction) -> bool {
    // ...  
    true
}

struct Txn {
    txn: *const BlindTransaction,
}

impl Txn {
    fn new(txn: *const BlindTransaction) -> Txn {
        Txn { txn: txn }
    }
    fn operations(&self) -> OperationIter {
        unsafe {
        OperationIter { on: begin_operations(self.txn), end: end_operations(self.txn) }
        }
    }
    fn signatures(&self) -> SignatureIter {
        unsafe {
        SignatureIter { on: begin_signatures(self.txn), end: end_signatures(self.txn) }
        }
    }
}

struct Operation {
    op: *const BlindOperation,
}

impl Operation {
    fn new(op: *const BlindOperation) -> Operation {
        Operation { op: op }
    }
}

struct Signature {
    sig: *const BlindSignature,
}

impl Signature {
    fn new(sig: *const BlindSignature) -> Signature {
        Signature { sig: sig }
    }
}

struct OperationIter {
    on: *const BlindOperation,
    end: *const BlindOperation,
}
impl OperationIter {
    fn new(begin: *const BlindOperation, end: *const BlindOperation) -> OperationIter {
        OperationIter { on: begin, end: end }
    }
}
impl Iterator for OperationIter {
    type Item = Operation;
    fn next(&mut self) -> Option<Operation> {
        unsafe {
        if self.on == self.end {
            return None;
        }
        let ret = Some(Operation::new(self.on));
        self.on = next_operation(self.on);
        ret
        }
    }
}

struct SignatureIter {
    on: *const BlindSignature,
    end: *const BlindSignature,
}
impl SignatureIter {
    fn new(begin: *const BlindSignature, end: *const BlindSignature) -> SignatureIter {
        SignatureIter { on: begin, end: end }
    }
}
impl Iterator for SignatureIter {
    type Item = Signature;
    fn next(&mut self) -> Option<Signature> {
        unsafe {
        if self.on == self.end {
            return None;
        }
        let ret = Some(Signature::new(self.on));
        self.on = next_signature(self.on);
        ret
        }
    }
}

struct Result {
    res: *const BlindTransactionResult,
}

impl Result {
    fn new(res: *const BlindTransactionResult) -> Result {
        Result { res: res }
    }
    fn op_results(&self) -> OpResultIter {
        unsafe {
        OpResultIter { on: begin_op_results_const(self.res), end: end_op_results_const(self.res) }
        }
    }
}

struct ResOut {
    res: *mut BlindTransactionResult,
}

impl ResOut {
    fn new(res: *mut BlindTransactionResult) -> ResOut {
        ResOut { res: res }
    }
    fn op_results(&self) -> OpResOutIter {
        unsafe {
        OpResOutIter { on: begin_op_results(self.res), end: end_op_results(self.res) }
        }
    }
}

struct OpResult {
    res: *const BlindOperationResult,
}

impl OpResult {
    fn new(res: *const BlindOperationResult) -> OpResult {
        OpResult { res: res }
    }
}

struct OpResOut {
    res: *mut BlindOperationResult,
}

impl OpResOut {
    fn new(res: *mut BlindOperationResult) -> OpResOut {
        OpResOut { res: res }
    }
}

struct OpResultIter {
    on: *const BlindOperationResult,
    end: *const BlindOperationResult,
}
impl OpResultIter {
    fn new(begin: *const BlindOperationResult, end: *const BlindOperationResult) -> OpResultIter {
        OpResultIter { on: begin, end: end }
    }
}
impl Iterator for OpResultIter {
    type Item = OpResult;
    fn next(&mut self) -> Option<OpResult> {
        unsafe {
        if self.on == self.end {
            return None;
        }
        let ret = Some(OpResult::new(self.on));
        self.on = next_op_result_const(self.on);
        ret
        }
    }
}

struct OpResOutIter {
    on: *mut BlindOperationResult,
    end: *mut BlindOperationResult,
}
impl OpResOutIter {
    fn new(begin: *mut BlindOperationResult, end: *mut BlindOperationResult) -> OpResOutIter {
        OpResOutIter { on: begin, end: end }
    }
}
impl Iterator for OpResOutIter {
    type Item = OpResOut;
    fn next(&mut self) -> Option<OpResOut> {
        unsafe {
        if self.on == self.end {
            return None;
        }
        let ret = Some(OpResOut::new(self.on));
        self.on = next_op_result(self.on);
        ret
        }
    }
}

