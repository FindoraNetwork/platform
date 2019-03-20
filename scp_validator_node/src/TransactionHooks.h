#ifndef TRANSACTION_HOOKS_H_
#define TRANSACTION_HOOKS_H_

#ifdef __cplusplus
#include <cstddef>
#include <cstdint>
extern "C" {
#else
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#endif

struct BlindTransaction;
struct BlindTransactionResult;
struct BlindOperation;
struct BlindOperationResult;
struct BlindSignature;

// application plug-in implements these
extern bool DeliverTransaction(struct BlindTransaction const *txn,
                               struct BlindTransactionResult *result);
extern bool CommitTransaction(struct BlindTransaction const *txn,
                              struct BlindTransactionResult const *result);
extern bool CheckTransaction(struct BlindTransaction const *txn);

struct BlindOperation const *
begin_operations(struct BlindTransaction const *txn);
struct BlindOperation const *next_operation(struct BlindOperation const *op);
struct BlindOperation const *end_operations(struct BlindTransaction const *txn);

struct BlindSignature const *
begin_signatures(struct BlindTransaction const *txn);
struct BlindSignature const *next_signature(struct BlindSignature const *sig);
struct BlindSignature const *end_signatures(struct BlindTransaction const *txn);

// TODO(NFY): expose other properties of the transaction?

struct BlindOperationResult *
begin_op_results(struct BlindTransactionResult *result);
struct BlindOperationResult *
next_op_result(struct BlindOperationResult *result);
struct BlindOperationResult *
end_op_results(struct BlindTransactionResult *result);

struct BlindOperationResult const *
begin_op_results_const(struct BlindTransactionResult const *result);
struct BlindOperationResult const *
next_op_result_const(struct BlindOperationResult const *result);
struct BlindOperationResult const *
end_op_results_const(struct BlindTransactionResult const *result);

uint8_t *
allocate_operation_result_buffer(struct BlindOperationResult *into_result,
                                 size_t data_size);
enum TransactionResultStatus {
  txSUCCESS = 0, // all operations succeeded

  txFAILED = -1, // one of the operations failed (none were applied)

  txTOO_EARLY = -2,         // ledger closeTime before minTime
  txTOO_LATE = -3,          // ledger closeTime after maxTime
  txMISSING_OPERATION = -4, // no operation was specified
  txBAD_SEQ = -5,           // sequence number does not match source account

  txBAD_AUTH = -6,             // too few valid signatures / wrong network
  txINSUFFICIENT_BALANCE = -7, // fee would bring account below reserve
  txNO_ACCOUNT = -8,           // source account not found
  txINSUFFICIENT_FEE = -9,     // fee is too small
  txBAD_AUTH_EXTRA = -10,      // unused signatures attached to transaction
  txINTERNAL_ERROR = -11       // an unknown error occured
};
void set_transaction_result_status(struct BlindTransactionResult *for_txn,
                                   enum TransactionResultStatus status);

#ifdef __cplusplus

} // extern "C"
namespace stellar {
struct TransactionEnvelope;
struct TransactionResult;
} // namespace stellar
void ExternCallDeliverTransaction(stellar::TransactionEnvelope const &,
                                  stellar::TransactionResult &);
void ExternCallCommitTransaction(stellar::TransactionEnvelope const &,
                                 stellar::TransactionResult const &);
bool ExternCallCheckTransaction(stellar::TransactionEnvelope const &);
#endif

#endif // TRANSACTION_HOOKS_H_
