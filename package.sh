#!/bin/bash

CWD=$(pwd)
DIR=$(mktemp -d)
LTS=lts-4.1

cp .stack-work/install/x86_64-linux/${LTS}/7.10.3/bin/appianAdmin $DIR/
mkdir ${DIR}/config
cp -r static ${DIR}

cd $DIR
tar -cf ${CWD}/appianAdmin.tar *
cd $CWD
rm -rf ${DIR}
