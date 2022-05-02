
target/release/fn setup -O ~/.findora/mnenomic.key  -S http://0.0.0.0

echo -e "\n ***** 4 times Simple transfer to fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5 *****"
target/release/fn transfer --amount 210000000 --asset FRA -T fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5
sleep 10

target/release/fn transfer --amount 210000000 --asset FRA -T fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5
sleep 10

target/release/fn transfer --amount 210000000 --asset FRA -T fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5
sleep 10

target/release/fn transfer --amount 210000000 --asset FRA -T fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5 --confidential-amount
sleep 10

