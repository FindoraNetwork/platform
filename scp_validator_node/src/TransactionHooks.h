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
struct AppHandle;

// application plug-in implements these
extern struct AppHandle *InitApp();
extern bool DestroyApp(struct AppHandle *);
extern bool DeliverTransaction(struct AppHandle *,
                               struct BlindTransaction const *txn,
                               struct BlindTransactionResult *result);
extern bool CommitTransaction(struct AppHandle *,
                              struct BlindTransaction const *txn,
                              struct BlindTransactionResult const *result);
extern bool CheckTransaction(struct AppHandle *,
                             struct BlindTransaction const *txn);

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

struct ConstBytes {
  uint8_t const *bytes;
  size_t length;
};

struct ConstBytes get_transaction_data(struct BlindTransaction const *txn);
struct ConstBytes
get_transaction_result_data(struct BlindTransactionResult const *res);

void set_transaction_result_status(struct BlindTransactionResult *for_txn,
                                   enum TransactionResultStatus status);
uint8_t *
allocate_transaction_result_buffer(struct BlindTransactionResult *for_txn,
                                   size_t length);

#ifdef __cplusplus

} // extern "C"
namespace stellar {
struct TransactionEnvelope;
struct TransactionResult;
} // namespace stellar
void ExternCallDeliverTransaction(AppHandle *,
                                  stellar::TransactionEnvelope const &,
                                  stellar::TransactionResult &);
void ExternCallCommitTransaction(AppHandle *,
                                 stellar::TransactionEnvelope const &,
                                 stellar::TransactionResult const &);
bool ExternCallCheckTransaction(AppHandle *,
                                stellar::TransactionEnvelope const &);
#endif

#endif // TRANSACTION_HOOKS_H_
