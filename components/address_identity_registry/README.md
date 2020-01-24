#Anonymous Credentials with Selective Attribute Revelation

I'm not certain the use case below is the one intended.

1. The (credential) issuer generates a key pair with knowledge of
   the number of attributes. The number of attributes is either set by
   the nature of the deal to be made (e.g. a standard form) or the
   user's choice.
2. The user generates keys using the issuer's public key.
3. The user supplies attribute values and their newly generated public
   key to the issuer. Or maybe the issuer already knows the attribute
   values?
4. The issuer signs the attributes with the issuer's secret key
   and the user's public key. This is the issuer's attestation that
   the user has committed to the attribute values.
5. The user presents to the prover (second party) the issuer
   attestation signature as proof the user has committed to certain
   attributes.
6. Possibly immediately, possibly later, the user generates and presents a
   "reveal signature" that makes it possible to prove the user committed
   to the to-be revealed values without revealing the values.
8. The user reveals the attribute values to the prover.
?. Something happens to commit the prover to something?
9. Finally, the prover has sufficient information to prove the user
   did in fact commit to the attributes as revealed.

##Trust Issues

The issuer (specifically, the credential issuer) must be trusted
regarding their standing and their discretion regarding keeping
the attribute values confidential. How does the issuer know the
user isn't lying about the attribute values? Maybe the issuer would
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
