# Address Identity Registry (AIR)

The authenticated map used in the AIR is implemented by a **Sparse Merkle Tree**, or SMT.

There are four roles implemented in the system:
1. **User**, which are the holders of credentials and identity
2. **Issuer** - issuers of credential, eg banks, passport agencies.
3. **Ledger** - The database which holds the map from user address to credential list
4. **Verifier** - A party that interacts with the user, and verifies the users credentials

The Ledger holds the AIR, amongst other things. It must be able to add credentials to an
address and subsequently provide that address as well as a (Merkle) proof of inclusion or non-inclusion.

## Setup
1. Credential issuers are initialized with the number of attributes they can handle.
   The supported attributes are a feature of the issuer; e.g., United States Department of State issues
   U.S. passports, and they decide what's in them. Tricky issue: credential issuers may issue more
   than one kind of credential, with varying numbers of fields.
2. User contacts an Issuer (which one?) and acquires issuer public key (Txn: User -> Issuer -> User)
3. The user generates its own user key pair using the issuer public key.

## Credential Generation
1. The User asks the Issuer to **ac_sign** a set of credentials the user provides and receives a signature back.
   (Txn : User -> Issuer -> User)
2. The User generates a commitment to the signed attribute values, and address using **ac_commit** and stores the randomness
   used to generate the commitment in a User wallet. The User generates an address, and asks the Ledger to store the
   commitment in the AIR. (Txn: User -> Ledger)

## Credential Validation
1. The Verifier asks the User to selectively reveal some attributes. (Txn: Verifier -> User)
2. The User replies with an address, or the Verifier uses the User's public info? (Txn: User -> Verifier)
3. The Verifier retrieves the signature and a Merkle proof of (non)inclusion from the Ledger by looking it
   up in the AIR. The Verifier checks the Merkle Proof, and if it is successful, sends the signature on to the User.
   (NB: The hash function used by the SMT)
4. The User uses his stored randomness and secret key to run **ac_reveal_with_rand**, yielding a new signature, which is sent back
   to the Verifier
5. The Verifier runs **ac_verify** with the signature
