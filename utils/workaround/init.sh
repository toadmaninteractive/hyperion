#!/bin/bash

cd $(dirname $0)
mkdir server
rm -rf src
git clone --single-branch --branch main https://github.com/toadmaninteractive/hyperion src
. /usr/local/erlang/23.3/activate
cd src
export JSX_FORCE_MAPS=true
make update all install DESTDIR=../server
cd ..
cp server/etc/app.config.sample server/etc/app.config
cp server/etc/server.conf.sample server/etc/server.conf
cp server/etc/vm.args.sample server/etc/vm.args
server/bin/server start
