#!/bin/dash

set -e
cd /home/user/chroot-tool
./gen-bindings.scm up /zapusk.chroots/lact-ru-system5-chroota/ | ./filter-bindings.scm 
