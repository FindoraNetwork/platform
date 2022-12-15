
# for 'perl'
export LC_ALL=en_US.UTF-8

export OSMAKE=make
if [[ "FreeBSD"  == `uname -s` ]]; then
    export OSMAKE=gmake
fi

function dbg() {
    msg=$*
    echo -e "\033[01m[** Debug **]\033[00m $msg"
}

function log() {
    msg="$*"
    echo -e "\033[01m[##  Log  ##]\033[00m $msg"
}

function die() {
    log "$*"
    echo -e "\033[31;01m[!! Panic !!]\033[00m $msg"
    exit 1
}

