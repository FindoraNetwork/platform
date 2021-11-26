
Root_Mnemonic="zoo nerve assault talk depend approve mercy surge bicycle ridge dismiss satoshi boring opera next fat cinnamon valley office actor above spray alcohol giant"
Root_Pk="HZnxwPI5PD_xpQX1NqKTHXqPdHXVXtGe7yQ0JI3MVTs="
Root_Sk="o9gXFI5ft1VOkzYhvFpgUTWVoskM1CEih0zJcm3-EAQ="

Bank_Address="fra18xkez3fum44jq0zhvwq380rfme7u624cccn3z56fjeex6uuhpq6qv9e4g5"
Bank_Mnemonic="field ranch pencil chest effort coyote april move injury illegal forest amount bid sound mixture use second pet embrace twice total essay valve loan"
Bank_Pk="Oa2RRTzdayA8V2OBE7xp3n3NKrjGJxFTSZZybXOXCDQ="
Bank_Sk="Ew9fMaryTL44ZXnEhcF7hQ-AB-fxgaC8vyCH-hCGtzg="


is_unsigned_integer()
{
  re="^[0-9]+$"
  if [[ $1 =~ $re ]]; then
    return 0
  fi
  return 1
}

balance_of()
{
  balance=$(fn account -a "$1" 2>&1 |grep balance|awk -F, '{print$1}'|awk '{print$2}')
  is_unsigned_integer "$balance" || exit 198
  echo "$balance"
}

deposit_to()
{
  fn contract-deposit --addr "$1" --amount "$2" > /dev/null 2>&1 || exit 197
}

source_balance()
{
  balance=$(fn show -b 2>&1 |grep FRA|awk '{print $1}')
  is_unsigned_integer "$balance" || exit 198
  echo "$balance"
}

verified_deposit()
{
  address=$1
  am=$2
  duration=$3
  fee=$4

  source=$(source_balance)
  current=$(balance_of "$address")
  if [ "$source" -lt $((am+fee)) ]; then
    return 1
  fi

  deposit_to "$address" "$am" || return 2

  sleep "$duration"
  expect=$((current+am))
  new=$(balance_of "$address")
  if [ "$new" != "$expect" ]; then
    echo $expect
    return 100
  fi

  return 0
}

setup_env()
{
  env=$1
  owner=$2
  [ -n "$env" ] || env="localhost"
  [ -n "$owner" ] || owner="root"

  if [ "$owner" = "root" ]; then
    echo "$Root_Mnemonic" > mnemonic
    fn setup -O mnemonic > /dev/null 2>&1 || exit 101
  elif [ "$owner" = "bank" ]; then
    echo "$Bank_Mnemonic" > mnemonic
    fn setup -O mnemonic > /dev/null 2>&1 || exit 101
  elif [ "$owner" = "custom" ]; then
    true
  else
    true
  fi


  if [ "$env" = "localhost" ]; then
    fn setup -S http://127.0.0.1 > /dev/null 2>&1 || exit 101
  elif [ "$env" = "qa01" ]; then
    fn setup -S https://dev-qa01.dev.findora.org > /dev/null 2>&1 || exit 101
  elif [ "$env" = "qa02" ]; then
    fn setup -S https://dev-qa02.dev.findora.org > /dev/null 2>&1 || exit 101
  elif [ "$env" = "custom" ]; then
    true
  else
    echo "Unsupported environment"
    exit 102
  fi
}

cleanup_env()
{
  rm -rf mnemonic
  rm -rf "$HOME"/.____fn_config____
}