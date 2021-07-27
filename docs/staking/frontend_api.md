> ## Wasm API

> ### delegate

```rust
pub fn add_operation_delegate(
    mut self,
    keypair: &XfrKeyPair,
    validator: TendermintAddr,
) -> Result<TransactionBuilder, JsValue> {
    ...
}
```

```rust
// The system address used to reveive delegation principals.
pub fn get_delegation_target_address() -> String {
  ...
}
```

> ### undelegate

```rust
pub fn add_operation_undelegate(
    mut self,
    keypair: &XfrKeyPair,
) -> Result<TransactionBuilder, JsValue> {
    ...
}
```

> ### claim

```rust
pub fn add_operation_claim(
    mut self,
    keypair: &XfrKeyPair,
    am: u64,
) -> Result<TransactionBuilder, JsValue> {
    ...
}
```

> ## Web API

> ### `validator_list`

params:
- none

return:
- validators: validator addresses, powers, commission rates
- threshold: power limit for every validator

return sample:

```json
{
  "validators": [
    {
      "addr": "0F64C8259BFCD1A9F6E21958D0A60D9E370D9C13",
      "power": 320000000000,
      "commission_rate": [
        1,
        100
      ]
    },
    {
      "addr": "30E07994969FFE8007481914335521CE665BEEFE",
      "power": 320000000000,
      "commission_rate": [
        1,
        100
      ]
    },
    {
      "addr": "325EC027285ABAA2A755286E1982E8F66633C05B",
      "power": 320000000000,
      "commission_rate": [
        1,
        100
      ]
    },
    {
      "addr": "4C2582DC314575DE73AD1EAA06726E555786900E",
      "power": 320000000000,
      "commission_rate": [
        1,
        100
      ]
    },
    {
      "addr": "55B8CF069F6F6C75935F8EB5FAC6B8C8138BC954",
      "power": 320000000000,
      "commission_rate": [
        1,
        100
      ]
    },
    {
      "addr": "EC046D54F2FA16AE7126343425C1E91A96ED18BD",
      "power": 320000000000,
      "commission_rate": [
        1,
        100
      ]
    }
  ],
  "threshold": [
    1,
    5
  ]
}
```

> ### `delegation_info`

params:
- a wallet address in bech32 format

return:
- rewards: total un-claimed delegation rewards
- bond_amount: total delegation principals in `Bond` state
- unbond_amount: total delegation principals in `UnBond` state
- global_staking: total power of all validators in Findora Network
- global_delegation: total delegation pricipals of Findora Network
- return_rate: real-time return on delegation investment

return sample:

```json
{
  "rewards": 100,
  "bond_amount": 1000000,
  "unbond_amount": 0,
  "global_staking": 100000000000000,
  "global_delegation": 100000000000000,
  "return_rate": [
      1,
      10
  ]
}
```

```json
{
  "rewards": 100,
  "bond_amount": 0,
  "unbond_amount": 1000000,
  "global_staking": 100000000000000,
  "global_delegation": 100000000000000,
  "return_rate": [
      1,
      10
  ]
}
```

> ## Test Accounts

> ### Root Account

```json
{
  "Mnemonic": "bright poem guard trade airport artist soon mountain shoe satisfy fox adapt garden decline uncover when pilot person flat bench connect coach planet hidden",
  "PublicKey": "kUfnrBQXOc_UoKT6UdtcIywjUunNaLaO36GUrvz5Ug4="
}
```

> ### Valiadtor Accounts

**AWS Env:**

```json
[
  [
    "shrug game where fish prosper cereal banana swap fetch affair violin zebra exit dolphin sunset happy endless ahead cloud lecture enhance sorry hunt train",
    "cF7onRo9F82AGD5c8d11EfuyYN77g6-_vsAuppfqxt8="
  ],
  [
    "accuse chase machine nest artwork sausage canvas write release cart bleak arch sick antique walk come rather start armed relax raise boil visual half",
    "7uSHa4M42_-qZaccutkidGRCP84Y-luNriQxBU3qRXI="
  ],
  [
    "exclude surge uncle yard first radio foot cat cushion baby delay join stomach critic segment frown almost dream ketchup canvas zone security addict army",
    "xLv_G-276O8cVnSVRAmo8KHjJmeBcV9_LdS7BBpWvCc="
  ],
  [
    "drastic area various chicken common item share fee lottery damp private thumb today since manage clutch victory change charge humor gaze suggest review chest",
    "RcUUC2x-yxIZ7O0antSgN-Yl6ET1MmN-ZX9xjt9sy8I="
  ],
  [
    "fine payment repair monkey train enroll easy exhibit evidence slender mandate predict kangaroo ahead silent submit sort couch head flip unfold plunge tobacco venture",
    "2mevsiKm4-wWImyUOivNTVyecRjXCO2x5NqkFu4cxlA="
  ],
  [
    "wisdom desert often buyer bonus reduce find slice ladder response sudden flat tube cake portion undo fiction prison congress about place affair spice member",
    "0wobjTwVCzH68WCEv4vlzu4dWTK2O0k3yxdt0iOX5Bc="
  ],
  [
    "ozone width album rigid lazy panda mountain shoot column rabbit voice around family recipe fitness romance sure left hard void month noodle mimic measure",
    "W4b1crCUKbDCyGMK2M9AXqGUmC4lAxRMeswb1gAPoIo="
  ],
  [
    "monkey until occur someone buddy sure true involve safe thumb table advice oblige announce reward pattern panic romance second powder squirrel wall old menu",
    "EtQDh6fS9Adj10Pro9VocnPvcCuPua81UHaLELwilfY="
  ],
  [
    "royal scout auction faint absorb unhappy swear mobile man rocket egg theme hurt include disorder atom melt seven trap gravity vicious sock attend minimum",
    "MOor1DGWz87B-l9ib0ntoxxINcyRR_dr-BDHGWK3dnA="
  ],
  [
    "ritual toast penalty average frog worth wide odor number lobster pride filter oil idle rate expect humor frozen spot nerve jacket anxiety quiz cram",
    "7PZGYAG3Z1OrlOjCymmo8yr0z9QeoYxL0CU634kiMI8="
  ],
  [
    "plunge slot recall risk shoulder impulse box knock gift assault panda pig announce flag tomato picnic bus crucial chronic choice merge rocket tobacco syrup",
    "8zSgShT-I4XpsXUNCdCa0Z3RBp6lKuaEqZE_XGKFo1A="
  ],
  [
    "holiday soldier gospel soldier fossil blue tornado patrol short exhibit beyond seed various cheese hazard decade need pumpkin aerobic leg inside caught room whale",
    "AGLlaDJHS5zr1D-M_WAe785IrJvMNG3xTCpa1QQOJdE="
  ],
  [
    "small table guide common mango gadget behave swim decline gift much arena anchor genuine fat inquiry cliff subway trust night creek minor exclude edge",
    "PevSEiTLzWfvkTEES6BBWSuKF43FbHBnOA684M6_Nz4="
  ],
  [
    "galaxy forget kingdom wonder view wrap view debate fame baby between never test use cargo civil raven leave that clip goat rabbit pole diesel",
    "ezH7VOnEYf3QB2VG8b-GsqsqQhGfwC0TO5C-TD6zqf8="
  ],
  [
    "drum seek term ride menu describe copper shrug actor host hundred nose veteran faint human attack volume mystery marble joke fiber field warfare stand",
    "k3MCjemGk_WfQHqE2X9pBkxXK8rV8B7bf72XMi0mAXg="
  ],
  [
    "auction fit aware special resemble aerobic recycle siren tennis vacuum divorce draft street exist suit gallery camp false blind mango aim square rapid creek",
    "cEbZNU2PTPnKgEu4Auq-N31I6kl5N64guQ7h4iZbHKo="
  ],
  [
    "tuna source pave hundred gossip vapor first someone yellow fun pause salmon world weapon symbol cat benefit mom supply brain bargain sport thunder fatal",
    "W-hRhXdwBOnUJLZ3U3Kp-a8eEY8KFa4qwFrQxqyO6uU="
  ],
  [
    "also stamp repeat parrot ignore coin picture armed soft ozone evidence leisure cement sea police submit piano breeze concert draft client assist broom add",
    "p8p0SBhweFmVtxelXjwDuhksZngA5bMWZOroS56rd9E="
  ],
  [
    "chimney want quiz chuckle check wrestle donate modify thumb west ramp dumb road sell marriage grant refuse dose force slam drama laugh truck alone",
    "NHgS1jnli7zKcYQuMFq0xA0c-pzpYVTgE1_00cbaEgo="
  ],
  [
    "hand guess globe injury also define siege dumb autumn south diamond inhale develop crowd turkey uncover wild wire olive miracle inside visa worry lend",
    "A1vNVGGWiP6hdpUE1JJXrpTVpUuUDOV71YvanzWA7LA="
  ]
]
```

**Local Env:**

```json
[
  [
    "alien pride power ostrich will cart crumble judge ordinary picnic bring dinner nut success phone banana fold agent shallow silent dose feel short insane",
    "9hCRf_Ap1OwVK0-EfIN95nhY2LbmqtMlXjmptx8OaRs="
  ],
  [
    "erode wasp helmet advice olive ridge update kid drip toast agree hand oppose hurt creek hazard purity raise main organ bargain patrol ramp toward",
    "cO59Dn_2VfCCm8VQVhL77PH9ng6U_peZ2l5wEMFE9iE="
  ],
  [
    "eternal happy outer ankle write smile admit scrub disease know code mom juice rapid blast ensure switch settle news antique into conduct sphere empower",
    "YwgZyiFMXrQfaKSpG40xz_PsRF3hgbvmbYz8AOWyWao="
  ],
  [
    "script month grain cook demise student foam odor boring endorse layer spell force culture height style observe husband embody tiger that athlete genius clap",
    "1RqZsjXhsN_4cBl7lArQIGJnkqakoZRj0-hEKu9Q8B4="
  ],
  [
    "sustain walk alley since scheme age glue choice fat current frog swallow cable company arrive receive parade anger illness clean maple draft art exile",
    "WScxeHTuhruAdC-VCNsAygeIDhzCCd_hu-czlVMKMSs="
  ],
  [
    "state sick tip glare erupt sign salad melt library churn accident organ book trust sketch embrace addict ice always trouble original vendor merge monkey",
    "sM5_4pR2UNtgfLkSgGxekn7N8D4ob4ihdsbgHOERqcA="
  ],
  [
    "vague random rule forum moon page opinion alcohol mixed circle ask cost life history vast garden reunion use flame west nothing middle kangaroo language",
    "OuVjfmm3NjOY8WpooWhYMSjQtj4-PG8zqIa0JooS6d8="
  ],
  [
    "peace patrol canvas regular together cycle clown region carpet learn price plate state gate long rose topple mango auto canoe media cushion soccer argue",
    "mbFNJrk6ht-l2rSlISTNwAmGtLPcREfOtDZWVpEip_Q="
  ],
  [
    "clump guard become smoke satisfy recall nation oil slide shell case notable escape suspect dawn poverty report smile apology learn column jelly fiber outer",
    "zmopZLxnmUPDshoiytMBRVeVZZRzhfzXsszRS_MfGlg="
  ],
  [
    "element update essence melody evolve razor canvas alcohol destroy tank neutral ride coast dish april cup medal brave palm strike essay flower learn what",
    "12YRbFI_HiXFlfK4aV-Stlx4oJQOpGWEePveTQTVG60="
  ],
  [
    "firm when photo pupil cream design pulse script mule among pupil cloth mechanic obvious amazing panic broom indoor silly member purpose rather upgrade hover",
    "1fKY4r_7bfJfLT0gBFd8al-5JITLmRHcgQ2vgmBVz8Q="
  ],
  [
    "canvas put chalk network thunder caught pigeon voyage dune despair ability hour light between lawsuit breeze disorder naive surround marine ostrich grace report galaxy",
    "G268zarggXuwvxZrA4-UPzOw9vkTyoPouKQ2LmdUF5A="
  ],
  [
    "account peasant found dignity thumb about taste yard elbow truth journey night model cushion dirt suit skirt bus flat dwarf across noble need between",
    "IM6emYj5U7Xx8DjFX-GE4i4l-5U49iGgeIs77xdQyoY="
  ],
  [
    "federal day velvet stairs liberty burst pluck margin capable subway rail eye where spread video journey garden trap salmon sword industry shine elephant arena",
    "P0PDDFZgjKkpcziVJFzt3LNFo8onNfGykGEHMG_OVwg="
  ],
  [
    "empty shy abandon elegant case outside drift voice tuition grace slush vibrant wage future script split educate insect involve unusual method arena option add",
    "oWkiSpti5RhJLRO3kyerOxkjOB1qabUfhOnNKawWMFQ="
  ],
  [
    "theme light sun cram fluid lab entire edge iron visa salt father stomach buffalo keep helmet sword sure shy shop flight teach diary brand",
    "eqV-MYPBmjPpyz9Yo6OtaQqJ4VvgVqLlPeTy0-bFvi0="
  ],
  [
    "comfort elephant manual blur climb blue disagree skate ridge auction loyal remember obscure nurse bar insane please refuse rather once giant fiber midnight foil",
    "xpUAuw1uqaBlJn_IgiKj7-V5-j9V2qEXV7xE6NeZ_bo="
  ],
  [
    "choice speed eternal movie glide culture deal elite sick aspect cluster cruel net moment myself jeans fade radio reflect desk grit toast this proof",
    "XTV61T7IaObMArdq9e5HSOpvfmtpyl06uLtBb3H8czs="
  ],
  [
    "strong fever clock wear forum palm celery smart sting mesh barrel again drive note clump cross unfold buddy tube lesson future lounge flat dune",
    "FWNzYRm-XjfA2t4m8BEkixCbr7H5rDBVrV5fsteLBkw="
  ],
  [
    "margin mention suit twice submit horse drive myth afraid upper neither reward refuse cart caught nurse era beef exclude goose large borrow mansion universe",
    "NhgadkzrXuL_HV5ie8h5t76GahpfqZJlJVWhZlS2hAQ="
  ]
]
```
