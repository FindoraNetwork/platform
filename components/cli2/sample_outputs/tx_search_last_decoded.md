```json
{
  "body": {
    "no_replay_token": [
      [
        57,
        41,
        119,
        176,
        192,
        239,
        225,
        59
      ],
      0
    ],
    "operations": [
      {
        "DefineAsset": {
          "body": {
            "asset": {
              "code": {
                "val": [
                  75,
                  124,
                  141,
                  37,
                  232,
                  127,
                  249,
                  29,
                  221,
                  160,
                  185,
                  17,
                  45,
                  221,
                  113,
                  8,
                  105,
                  9,
                  7,
                  33,
                  21,
                  184,
                  159,
                  157,
                  75,
                  23,
                  137,
                  238,
                  171,
                  129,
                  39,
                  70
                ]
              },
              "issuer": {
                "key": "nwA1kSDevBVXNJgM38IHH7OQ0HwfdzWTZGvzWiBX0WM="
              },
              "memo": "mm",
              "asset_rules": {
                "transferable": true,
                "updatable": true,
                "transfer_multisig_rules": null,
                "max_units": 999999,
                "decimals": 0
              }
            }
          },
          "pubkey": {
            "key": "nwA1kSDevBVXNJgM38IHH7OQ0HwfdzWTZGvzWiBX0WM="
          },
          "signature": "qaQ9RC8NOZwiAdgaac7Ss2xSp52E6yt3A7Dnb6MQXj637reXWzV9MyrDMWXFDN7H1VjIhoGCxPBYeH83mt3LDQ=="
        }
      },
      {
        "IssueAsset": {
          "body": {
            "code": {
              "val": [
                75,
                124,
                141,
                37,
                232,
                127,
                249,
                29,
                221,
                160,
                185,
                17,
                45,
                221,
                113,
                8,
                105,
                9,
                7,
                33,
                21,
                184,
                159,
                157,
                75,
                23,
                137,
                238,
                171,
                129,
                39,
                70
              ]
            },
            "seq_num": 0,
            "num_outputs": 1,
            "records": [
              [
                {
                  "record": {
                    "amount": {
                      "NonConfidential": 100
                    },
                    "asset_type": {
                      "NonConfidential": [
                        75,
                        124,
                        141,
                        37,
                        232,
                        127,
                        249,
                        29,
                        221,
                        160,
                        185,
                        17,
                        45,
                        221,
                        113,
                        8,
                        105,
                        9,
                        7,
                        33,
                        21,
                        184,
                        159,
                        157,
                        75,
                        23,
                        137,
                        238,
                        171,
                        129,
                        39,
                        70
                      ]
                    },
                    "public_key": "nwA1kSDevBVXNJgM38IHH7OQ0HwfdzWTZGvzWiBX0WM="
                  }
                },
                null
              ]
            ]
          },
          "pubkey": {
            "key": "nwA1kSDevBVXNJgM38IHH7OQ0HwfdzWTZGvzWiBX0WM="
          },
          "signature": "Lm_X4d5HmJHHD2QpH8U84qzPDPa6k0LeEC-kXMoNPz6ZJkhlThOs8cc33jJE6MDeowDQE8zSuW98v5NOBLWLAQ=="
        }
      },
      {
        "TransferAsset": {
          "body": {
            "inputs": [
              {
                "Absolute": 0
              }
            ],
            "policies": {
              "valid": true,
              "inputs_tracking_policies": [
                []
              ],
              "inputs_sig_commitments": [
                null
              ],
              "outputs_tracking_policies": [
                [],
                []
              ],
              "outputs_sig_commitments": [
                null,
                null
              ]
            },
            "outputs": [
              {
                "record": {
                  "amount": {
                    "NonConfidential": 1
                  },
                  "asset_type": {
                    "NonConfidential": [
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0
                    ]
                  },
                  "public_key": "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
                }
              },
              {
                "record": {
                  "amount": {
                    "NonConfidential": 9998
                  },
                  "asset_type": {
                    "NonConfidential": [
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0
                    ]
                  },
                  "public_key": "nwA1kSDevBVXNJgM38IHH7OQ0HwfdzWTZGvzWiBX0WM="
                }
              }
            ],
            "transfer": {
              "inputs": [
                {
                  "amount": {
                    "NonConfidential": 9999
                  },
                  "asset_type": {
                    "NonConfidential": [
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0
                    ]
                  },
                  "public_key": "nwA1kSDevBVXNJgM38IHH7OQ0HwfdzWTZGvzWiBX0WM="
                }
              ],
              "outputs": [
                {
                  "amount": {
                    "NonConfidential": 1
                  },
                  "asset_type": {
                    "NonConfidential": [
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0
                    ]
                  },
                  "public_key": "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
                },
                {
                  "amount": {
                    "NonConfidential": 9998
                  },
                  "asset_type": {
                    "NonConfidential": [
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0,
                      0
                    ]
                  },
                  "public_key": "nwA1kSDevBVXNJgM38IHH7OQ0HwfdzWTZGvzWiBX0WM="
                }
              ],
              "proofs": {
                "asset_type_and_amount_proof": "NoProof",
                "asset_tracking_proof": {
                  "asset_type_and_amount_proofs": [],
                  "inputs_identity_proofs": [
                    []
                  ],
                  "outputs_identity_proofs": [
                    [],
                    []
                  ]
                }
              },
              "asset_tracing_memos": [
                [],
                [],
                []
              ],
              "owners_memos": [
                null,
                null
              ]
            },
            "transfer_type": "Standard"
          },
          "body_signatures": [
            {
              "address": {
                "key": "nwA1kSDevBVXNJgM38IHH7OQ0HwfdzWTZGvzWiBX0WM="
              },
              "signature": "QqkKEEBTgwxssOsDjss63uBQpzCeiY3bv5bVJ4a890OiiELu4zZ3LIQf9y2fCda0avsMN2LzsyWe-HDp7JuYAA=="
            }
          ]
        }
      }
    ]
  },
  "signatures": [
    "9PlHSxVOaeE_bEB8JoK8cjazm3oJb0gG5qruQTWw3xtFcW3lCxjw0usXlUFfFOMFbmlofQXrsuTTxGf4M69nAA=="
  ]
}
```
