#ifndef TRANSACTION_HOOKS_H_
#define TRANSACTION_HOOKS_H_

#ifdef __cplusplus
#include <cstddef>
#include <cstdint>
#include <memory>
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

typedef struct AppHandle *(*InitApp)();
typedef bool (*DestroyApp)(struct AppHandle *);
typedef bool (*DeliverTransaction)(struct AppHandle *,
                                   struct BlindTransaction const *txn,
                                   struct BlindTransactionResult *result);
typedef bool (*CommitTransaction)(struct AppHandle *,
                                  struct BlindTransaction const *txn,
                                  struct BlindTransactionResult const *result);
typedef bool (*CheckTransaction)(struct AppHandle *,
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

typedef void (*RegisterCallbacks)(InitApp, DestroyApp, DeliverTransaction,
                                  CommitTransaction, CheckTransaction);

struct ConstBytes {
  uint8_t const *bytes;
  size_t length;
};

typedef struct ConstBytes (*GetTransactionData)(
    struct BlindTransaction const *txn);

typedef struct ConstBytes (*GetTransactionResultData)(
    struct BlindTransactionResult const *txn);

typedef void (*SetTransactionResultStatus)(struct BlindTransactionResult *txn,
                                           enum TransactionResultStatus status);

typedef uint8_t *(*AllocateTransactionResultBuffer)(
    struct BlindTransactionResult *for_txn, size_t length);

extern void load_plugin(
    RegisterCallbacks ext_register_callbacks,
    GetTransactionData ext_get_transaction_data,
    GetTransactionResultData ext_get_transaction_result_data,
    SetTransactionResultStatus ext_set_transaction_result_status,
    AllocateTransactionResultBuffer ext_allocate_transaction_result_buffer);

#ifdef __cplusplus

} // extern "C"
namespace stellar {
struct TransactionEnvelope;
struct TransactionResult;
} // namespace stellar

class RustAppHandle {
  AppHandle *inner_ptr_;

public:
  void DeliverTransaction(stellar::TransactionEnvelope const &,
                          stellar::TransactionResult &);
  void CommitTransaction(stellar::TransactionEnvelope const &,
                         stellar::TransactionResult const &);
  bool CheckTransaction(stellar::TransactionEnvelope const &);
  static std::unique_ptr<RustAppHandle> GetRustAppHandle();
};
#endif

#endif // TRANSACTION_HOOKS_H_
