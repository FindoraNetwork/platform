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

PASSWORD_PROMPT="echo -e 'password\npassword\n'"
SIMPLE_CONFIRM_WITH_PROMPT="echo -e 'password\ny\n'"
SIMPLE_PASSWORD_PROMPT="echo -e 'password\n'"
DOUBLE_CONFIRM_WITH_PROMPT="echo -e 'password\npassword\n\nY'"
MEMO_ALICE_WITH_PROMPT="echo -e 'password\nmemo_alice\n'"
