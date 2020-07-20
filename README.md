# chroot-tool
Virtual machines based on chroot and debootstrap

# Usage

## create
*chroot-tool.sh create -d DIR*

Creates debian-based image inside DIR using debootstrap.

## turn on

*chroot-tool.sh on -d DIR*

1. Mounts user-defined directories from host to chroot.
2. Mounts maintenance directories (/tmp, etc).

## execute command inside chroot

*chroot-tool.sh run "COMMAND" -d DIR*

1. Performs "turn on" step.
2. Executes COMMAND inside chroot.

* If COMMAND starts with a /, executes it as is.
* If file "DIR/chroot.d/COMMAND" exist, executes it.
* else executes "/chroot.d/chroot-cmd.sh COMMAND".

Additionally, if `chroot.d/host-cmd.sh` exist, executes it before going into chroot.

## turn off

*chroot-tool.sh off -d DIR*

1. Executes "stop" command inside chroot.
2. Unmounts all mounted directories.

# Authors

* Mikhail Bakhterev - idea and implementation.
* Pavel Vasev - demands and niceties.

# License

MIT

# Copyright

(c) 2020 Pavel Vasev, Mikhail Bakhterev
