// Copyright 2019 © Findora. All rights reserved.
/// Command line executable to exercise functions related to credentials

// Anonymous Credentials with Selective Attribute Revelation
//
// I'm not certain the use case below is the one intended.
//
// 1. The (credential) issuer generates a key pair with knowledge of
//    the number of attributes. The number of attributes is either set by
//    the nature of the deal to be made (e.g. a standard form) or the
//    user's choice.
// 2. The user generates keys using the issuer's public key.
// 3. The user supplies attribute values and their newly generated public
//    key to the issuer. Or maybe the issuer already knows the attribute
//    values?
// 4. The issuer signs the attributes with the issuer's secret key
//    and the user's public key. This is the issuer's attestation that
//    the user has committed to the attribute values.
// 5. The user presents to the prover (second party) the issuer
//    attestation signature as proof the user has committed to certain
//    attributes.
// 6. Possibly immediately, possibly later, the user generates and presents a
//    "reveal signature" that makes it possible to prove the user committed
//    to the to-be revealed values without revealing the values.
// 8. The user reveals the attribute values to the prover.
// ?. Something happens to commit the prover to something?
// 9. Finally, the prover has sufficient information to prove the user
//    did in fact commit to the attributes as revealed.
//
// Trust Issues
//
// The issuer (specifically, the credential issuer) must be trusted
// regarding their standing and their discression regarding keeping
// the attribute values confidential. How does the issuer know the
// user isn't lying about the attribute values? Maybe the issuer would
// only participate under certain conditions.
//
// The user must trust the issuer not to prematurely reveal the
// attributes or alter the attributes. Maybe the credential issuer has
// limited knowledge of the account requesting attestation or the
// pending transaction? If there were a small number of bidders
// perhaps the second party could infer something. Maybe the issuer
// wouldn't know with whom to conspire?
//
// How does the prover know these attributes are germane? Could the user
// game the system by getting the issuer to sign one set of attributes
// and later claim they were another set of attributes, i.e. changing
// the column headings?
//
// The only thing the prover knows is that the values were committed.
// The prover must also trust the issuer. Maybe reputation give the
// issuer incentive to be trustworthy. This is all that prevents the
// user from lying about the attribute values.
//
// Note: there were minor changes to what was exposed in the Zei
// interface needed to make the code below work outsize of Zei.
//    algebra/mod.rs: Made bn and groups public. Previously pub(crate).
//    src/lib.rs: Made algebra public. Was private.

extern crate rand;
extern crate rand_chacha;
extern crate zei;
use rand::SeedableRng;
use rand_chacha::ChaChaRng;
use zei::algebra::groups::Scalar;
use zei::algebra::bls12_381::{BLSScalar, BLSGt};
use zei::crypto::anon_creds::{ac_keygen_issuer, ac_keygen_user, ac_sign,
                              ac_reveal, ac_verify};

fn main() {
    let mut prng: ChaChaRng;
    // For a real application, the seed should be random.
    prng = ChaChaRng::from_seed([0u8; 32]);

    // Attributes to be revealed. For example, they might be:
    //    account balance, zip code, credit score, and timestamp
    // In this case, account balance will not be revealed.
    let bitmap = [false, true, true, true];
    let attrs = [BLSScalar::from_u64(92574500),
                 BLSScalar::from_u64(95050),
                 BLSScalar::from_u64(720),
                 BLSScalar::from_u64(20190820)];
    let att_count = bitmap.len();
    let (issuer_pk, issuer_sk) =
        ac_keygen_issuer::<_, BLSScalar, BLSGt>(&mut prng, att_count);
    println!("\nIssuer keys\nPrivate key: {:?}\nPublic key: {:?}",
             issuer_sk, issuer_pk);

    let (user_pk, user_sk) =
        ac_keygen_user::<_, BLSScalar, BLSGt>(&mut prng, &issuer_pk);
    println!("\nUser keys\nPrivate key: {:?}\nPublic key: {:?}",
             user_pk, user_sk);

    // Issuer vouches for the user's attributes given above.
    let sig =
        ac_sign::<_, BLSScalar, BLSGt>(&mut prng, &issuer_sk, &user_pk,
                                       &attrs);
    println!("\nCredential signature: {:?}", sig);

    // The user presents this to the second party in a transaction as proof
    // attributes have been committed without revealing the values.
    let reveal_sig = ac_reveal::<_, BLSScalar, BLSGt>(&mut prng,
                                                      &user_sk,
                                                      &issuer_pk,
                                                      &sig,
                                                      &attrs,
                                                      &bitmap).unwrap();


    // Decision point. Does the second party agree to do business?
    // Sometimes this is presumed such as a syndicated investment
    // round, where you'll take money from everyone qualified. Other
    // times, there might be an off-chain negotiation to decide
    // whether to provisionally accept the deal.

    let mut revealed_attrs = vec![];
    for (attr, b) in attrs.iter().zip(&bitmap) {
      if *b {
        revealed_attrs.push(attr.clone());
      }
    }

    // Proves the attributes are what the user committed to. Anyone
    // with the revealed attributes and the reveal signature can do
    // this. But presumably, the reveal signature alone is insufficient to
    // derive the attributes. Presumably if the range of legal values were small,
    // exhaustive search would not be too exhausting. (?)
    if ac_verify::<BLSScalar, BLSGt>(&issuer_pk,
                                     revealed_attrs.as_slice(),
                                     &bitmap,
                                     &reveal_sig).is_ok()
    {
        println!("Verified!");
    } else {
        println!("Verification failed.");
    };
}
