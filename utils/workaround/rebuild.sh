#!/bin/bash

cd $(dirname $0)
rm -rf src
git clone --single-branch --branch main https://github.com/toadmaninteractive/hyperion src
server/bin/server stop
. /usr/local/erlang/23.3/activate
cd src
export JSX_FORCE_MAPS=true
make update all install DESTDIR=../server
cd ..
server/bin/server start
