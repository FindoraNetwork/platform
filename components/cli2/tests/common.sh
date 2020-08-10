FINDORA_STORE_FILE=${FINDORA_HOME:-${HOME}/.findora}/cli2_data.sqlite

setup() {
  # Start from a fresh state
  echo "Deleting $FINDORA_STORE_FILE..."
  rm  -f $FINDORA_STORE_FILE || true
  bash -c '{ echo; echo; } | $CLI2 setup'
}

debug_array() {

  echo "Debugging array..."
  arr=("$@")
  COUNTER=0
  for i in "${arr[@]}";
    do
        echo "[$COUNTER]$i"
        COUNTER=$((COUNTER+1))
    done
}

debug_lines(){
  debug_array "${lines[@]}"
}

check_line() {
  line_number="$1"
  command_str="$2"
  command_str_length=`expr length "$command_str"`
  [ "${lines[$line_number]:0:$command_str_length}" = "$command_str" ]
}

PASSWORD="password"
PASSWORD_PROMPT="echo -e '$PASSWORD\n$PASSWORD'"
SIMPLE_CONFIRM_WITH_PROMPT="echo -e '$PASSWORD\ny'"
SIMPLE_PASSWORD_PROMPT="echo -e '$PASSWORD'"
DOUBLE_CONFIRM_WITH_PROMPT="echo -e '$PASSWORD\n$PASSWORD\n\nY'"
MEMO_ALICE_WITH_PROMPT="echo -e '$PASSWORD\nmemo_alice\n'"
MEMO_ALICE_WITH_SEVERAL_PROMPTS="echo -e '$PASSWORD\nmemo_alice\n$PASSWORD\n$PASSWORD\n$PASSWORD\nY\n'"
PROMPT_TRANSFER_ASSET="echo -e 'utxo0 \n 5000 \n n \n n \n bob \n n\n$PASSWORD\n'"
