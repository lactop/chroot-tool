# chroot-tool
Virtual machines based on chroot and debootstrap.

Pros:
* Super-easy to setup and run.
* Super-lightweight virtual machines based on debian running inside chroots on host machine.
* Shared space of ip ports - no need for bridge networks etc.
* Shared space of pids - easy to manage from host.

Cons:
* Shared system directories like /proc - the code inside chroots should not be malicious.

# Setup

* `apt-get install debootstrap guile`

# Usage

## create
**chroot-tool.sh create -d DIR**

Creates debian-based image inside DIR using debootstrap.

## turn on

**chroot-tool.sh on -d DIR**

1. Mounts user-defined directories from host to chroot.
2. Mounts maintenance directories: /etc, /proc, so on.

## execute command inside chroot

**chroot-tool.sh run "COMMAND" -d DIR**

1. Performs "turn on" step.
2. Executes COMMAND inside chroot.

* If COMMAND starts with a /, executes it as is.
* If file `DIR/chroot.d/COMMAND` exist, executes it.
* else executes `/chroot.d/chroot-cmd.sh COMMAND` (todo that).

Additionally, if `chroot.d/host-cmd.sh` exist, executes it before going into chroot.

## turn off

**chroot-tool.sh off -d DIR**

1. Executes "stop" command inside chroot.
2. Unmounts all mounted directories.

# Authors

* Mikhail Bakhterev - idea and implementation.
* Pavel Vasev - demands and niceties.

# License

MIT

# Copyright

(c) 2020 Pavel Vasev, Mikhail Bakhterev
