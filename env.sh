#! /usr/bin/env bash

# $ source env.sh

echo

if [ ! -v LD_LIBRARY_PATH ]; then
    export LD_LIBRARY_PATH=../lib:./lib:.:${LD_LIBRARY_PATH}
fi
echo "export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}"

echo
