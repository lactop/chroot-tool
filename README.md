# chroot-tool
Virtual machines based on chroot and debootstrap.

Pros:
* Super-lightweight virtual machines based on debian running inside chroot on host machine.
* Super-easy to setup and run.
* Shared space of ip ports - no need for bridge networks etc.
* Shared space of pids - easy to manage from host.

Cons:
* Shared system directories like /proc - the code inside chroots should not be malicious.

# Workflow

Chroot-tool creates copy of debian in specified directory, and allows to run commands inside it using chroot. 

To allow programs to run comfortably, it mounts all technically required directories
inside that chroot, like `/proc`, `/dev`, so on. It also creates tmpfs directory for /tmp.
Finally it is able to bind user-specified directories from host machine inside chroot.

That is, the workflow is following:
1. Create a directory (DIR) and create chroot inside it using chroot-tool.
2. Create an executable file `DIR/chroot.d/chroot-cmd`.
3. Call `chroot-tool run "some-command"` and chroot-tool will execute `/chroot.d/chroot-cmd some-command`
inside chroot, by the way mounting all required directores before that.

# Setup

Chroot-tool requires `debootstrap` and `guile` to be installed on host machine.

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
* else if file `DIR/chroot.d/COMMAND` exist, executes it.
* else executes `/chroot.d/chroot-cmd COMMAND` (todo that).

Additionally, if `chroot.d/host-cmd` exist, executes it before going into chroot.

## turn off

**chroot-tool.sh off -d DIR**

1. Executes "stop" command inside chroot.
2. Unmounts all mounted directories.

# Custom directories

To bind specific host directories inside chroot, create a file named `DIR/chroot.d/tool.conf`
of the following content:
```
[bind]
src:... tgt: ...
src:... tgt: ...
src:... tgt: ...
```
* src - a path to directory on host machine
* tgt - a path inside chroot

# Authors

* Mikhail Bakhterev - idea and implementation.
* Pavel Vasev - demands and niceties.

# License

MIT

# Copyright

(c) 2020 Pavel Vasev, Mikhail Bakhterev
