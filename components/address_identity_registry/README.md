# Address Identity Registry 

There are four roles implemented in the system:
1. **User**, which are the holders of credentials and identity
2. **Issuer** - issuers of credential, eg banks, passport agencies, ...
3. **Ledger** - The database which holds the map from user address to credential list
4. **Validator** - A party that interacts with the user to determine the validity of the users credentials

The **Ledger** holds the AIR.

## Setup
1. Credential issuers are initialized with the number of attributes they can handle.
   Is this number a User choice?
2. User contacts an Issuer (which one?) and acquires  public key (Txn: User -> Issuer -> User)
3. The user generates a key pair using the issuer's public key.

## Credential Generation
1. The User asks the Issuer to sign a set of credentials the user provides and receives a signature back.
   (Txn : User -> Issuer -> User)
2. The User generates a commitment to the signed attribute values, and address using **ac_commit** and stores the randomness
   used to generate the commitment in a User wallet. The User generates an address, and asks the Ledger to store the
   commitment in the AIR. (Txn: User -> Ledger)

## Credential Validation
1. The Validator asks the User to selectively reveal some attributes. (Txn: Validator -> User)
2. The User replies with an address, or the Validator uses the User's public info? (Txn: User -> Validator)
3. The Validator retrieves the signature from the Ledger by looking it up in the AIR. The Validator checks the
   Merkle Proof, and if it is successful, sends the signature on to the User.
4. The User uses his stored randomness and secret key to run **ac_reveal_with_rand**, yielding a new signature, which is sent back
   to the Validator
5. The Validator runs **ac_verify** with the signature

## Trust Issues (WIP)

The issuer (specifically, the credential issuer) must be trusted
regarding their standing and their discretion regarding keeping
the attribute values confidential. How does the Issuer know the
User isn't lying about the attribute values? Maybe the Issuer would
only participate under certain conditions.

The user must trust the issuer not to prematurely reveal the
attributes or alter the attributes. Maybe the credential issuer has
limited knowledge of the account requesting attestation or the
pending transaction? If there were a small number of bidders
perhaps the second party could infer something. Maybe the issuer
wouldn't know with whom to conspire?

How does the prover know these attributes are germane? Could the user
game the system by getting the issuer to sign one set of attributes
and later claim they were another set of attributes, i.e. changing
the column headings?

The only thing the prover knows is that the values were committed.
The prover must also trust the issuer. Maybe reputation give the
issuer incentive to be trustworthy. This is all that prevents the
user from lying about the attribute values.
