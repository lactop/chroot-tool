# chroot-tool
Virtual machines based on chroot and debootstrap.

Pros:
* Super-lightweight virtual machines based on debian running inside chroot on host machine.
* Super-easy to setup and run.
* Shared space of ip ports - no need for bridge networks etc.
* Shared space of pids - easy to manage from host.

Cons:
* Shared system directories like /proc - the code inside chroots should be polite.

# Workflow

Chroot-tool creates copy of debian in specified directory, and allows to run commands inside it using chroot. 

To allow programs to run comfortably, it binds all technically required directories
inside that chroot, like `/proc`, `/dev`, so on. It also creates tmpfs directory for `/tmp`.
Finally it is able to bind user-specified directories from host machine into chroot.

That is, the workflow is following:
1. Use `chroot-tool.sh create` to create a new debian image inside specified DIR.
2. To specify how chroot should react on commands, create an executable file `DIR/chroot.d/chroot-cmd`.
3. To call command inside chroot, call `chroot-tool run "some-command"`.
Chroot-tool will execute your file `/chroot.d/chroot-cmd some-command` inside chroot environment, 
by the way binding all required directores before that.

# Setup

Chroot-tool requires `debootstrap` and `guile` to be installed on host machine.

* `apt-get install debootstrap guile`

# Usage

## create
**chroot-tool.sh create -d DIR**

Creates debian-based image inside DIR using debootstrap.

Example:
* `chroot-tool.sh create -d /var/chroots/my-chroot-1` - will create image in `/var/chroots/my-chroot-1` directory.
* `chroot-tool.sh create` - without -d specified, chroot-tool operates in current directory.

## turn on

**chroot-tool.sh on -d DIR**

1. Mounts user-defined directories from host to chroot.
2. Mounts maintenance directories: /etc, /proc, so on.

The "on" command is runned automatically by chroot-tool before executing any steps.

Example:
* `chroot-tool.sh on -d /var/chroots/my-chroot-1`

## execute command inside chroot

**chroot-tool.sh run "COMMAND" -d DIR**

1. Performs "turn on" step.
2. Executes COMMAND inside chroot.

* If COMMAND starts with a /, executes it as is.
* else if file `DIR/chroot.d/COMMAND` exist, executes it.
* else executes `/chroot.d/chroot-cmd COMMAND` (todo that).

Additionally, if `chroot.d/host-cmd` exist, executes it before going into chroot.

Example:
* `chroot-tool.sh run start`
* `chroot-tool.sh run "rebuild-index alfa"`

## turn off

**chroot-tool.sh off -d DIR**

1. Executes "stop" command inside chroot.
2. Unmounts all mounted directories.

Example:
* `chroot-tool.sh off -d /var/chroots/my-chroot-1`

# Custom directories

To bind specific host directories inside chroot, create a file named `DIR/chroot.d/tool.conf`
with content like following:
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
