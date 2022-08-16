#!/usr/bin/env bash

VERSION="v0.33.9"
URL="https://github.com/FindoraNetwork/tendermint-bin/releases/download/${VERSION}"
URLFILENAME=""
OS=$(uname -s)
ARCH=$(uname -m)
SAVEDIR="./tools/tmbin"
TMNAME="tendermint"
BINNAME="$SAVEDIR/${TMNAME}_${VERSION}"
GOBIN="$(go env GOPATH)/bin"
TMSAVE="$GOBIN/$TMNAME"

if [[ "CST" == $(date +%Z) ]]; then 
    URL="https://findora-tendermint.s3.us-west-2.amazonaws.com/${VERSION}"
fi

case $OS in
    "Linux")
        OS="linux"
    ;;
    "Darwin")
        OS="darwin"
    ;;
    "FreeBSD")
        OS="freebsd"
    ;;
    *)
        OS="unknown"
    ;;
esac

case $ARCH in
    "aarch64" | "arm64")
        ARCH="arm64"
    ;; 
    "x86_64" | "amd64")
        ARCH="amd64"
    ;;
    *)
        ARCH="unknown"
    ;;
esac

case "${OS}_${ARCH}" in
    # "linux_amd64")
    #     URLFILENAME="${TMNAME}_${VERSION}_${OS}_${ARCH}"
    # ;;
    "darwin_amd64")
        URLFILENAME="${TMNAME}_${VERSION}_${OS}_${ARCH}"
    ;;
    "darwin_arm64")
        URLFILENAME="${TMNAME}_${VERSION}_${OS}_amd64"
    ;;
    *)
        make tendermint_goleveldb
        exit $?
    ;;
esac

if [ ! -d "$GOBIN" ]; then
    mkdir -p "$GOBIN"
fi

if [ ! -d "$SAVEDIR" ]; then
    mkdir -p "$SAVEDIR"
fi

if [ -f "$BINNAME" ] && [ -s "$BINNAME" ]; then
    if [ ! -x $BINNAME ]; then
        chmod +x $BINNAME
    fi
    if ./$BINNAME help; then
        cp $BINNAME $TMSAVE -f
        echo "binary tendermint already exists, copy it"
        exit 0
    fi
fi
rm -f $BINNAME

if ! wget -t 10 -O "${BINNAME}" "${URL}/$URLFILENAME"; then
    echo "download $BINNAME failed, exit shell script"
    rm -f "$BINNAME"
    exit -1
fi

if [ ! -x $BINNAME ]; then
    chmod +x $BINNAME
fi

cp $BINNAME $TMSAVE -f
echo "download binary tendermint successfully"
exit 0
