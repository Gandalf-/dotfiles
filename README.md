# DotFiles

A huge variety of scripts, configuration files, shell libaries for ease of use,
development, configuration and information gathering on Linux, BSDs and WSL.

- `bin` a number of scripts, the most complex are described below
   - [apocrypha](#Apocrypha) a lightweight, flexible JSON server and client
   - [g](#g) a git wrapper for chaining commands together
   - [qcrypt](#qcrypt) simple file or directory encryption and decryption for the CLI
   - [wizard](#Wizard) an extensible, menu driven snippet organizer

- `etc` my configuration files for [Vim](https://github.com/vim/vim),
  [Tmux](https://github.com/tmux/tmux),
  [Bash](https://www.gnu.org/software/bash/), [Irssi](https://irssi.org/),
  [Fish](https://github.com/fish-shell/fish-shell) and
  [Pylint](https://www.pylint.org/)

- `lib` shell, fish and python libraries that make all this work
    - [autocli](#autocli) a bash code generation library that inspects functions to create menus

# Apocrypha

A flexible JSON database that supports a wide variety of operations. Start the
database server with `python3 -m apocrypha_server`. `bin/d` is the default
client. You can connect to remote servers using `d` with the `-h` or `--host`
flag. It will remember the last argument as the default server until you
provide it again.

Apocrypha is now hosted separately
[here](https://github.com/Gandalf-/apocrypha) and available on pip with `pip install apocrypha`


# g
A wrapper around git that allows any number of supported commands to be chained together. If any command fails, the rest will not be run. This allows common workflows to be written out all at once with terse abbreviations to save time and typing.

### Examples and equivalent git commands:
```
$ g s
$ # git status
$ g dh 2
$ # git diff HEAD~2
$ g s d a cm 'Update readme' f pl - ph
$ # git status; git diff; git add -A; git commit -m 'Update readme'; git fetch; git pull; git push
```

### Usage
```
  g - super git wrapper
    !  : toggle confirmation
    a  : add everything
    bv : branch -vv
    cc : clean -nfd, confirm, clean -fd
    c  : commit
    ca : commit --amend
    cn : commit --amend --no-edit
    cb : checkout -b (branch)
    cl : checkout (remote branch) -b (local branch)
    cf : attempt to checkout branch by bug name using fzf
    cs : attempt to checkout branch by bug name
    cm : commit -m (message)
    co : checkout (file)
    d  : diff changes [output_file]
    dc : diff cached changes [output_file]
    dh : diff commits [number of commits] [output_file]
    ds : diff commits [number of commits] - auto names diff [work only]
    f  : fetch
    l  : log
    ll : log graph
    m  : merge (branch)
    s  : status
    rv : remote -vv
    ri : interactive rebase
    p  : pause
    pl : pull --no-edit [branch]
    pm : pull [branch]
    ph : push [branch]
    pf : push --force [branch]
```


# Wizard

Wizard leverages `lib/autocli.sh` to organize any number of commands into a tree menu structure. The user defines bash functions that are interpreted into options in menus based on their names.
```
wizard_show_disk() {
  df -h
}
```
The function above creates the following command: `wizard show disk`, which can be shortened to `w s d` or any other combination of unambiguous substrings. Wizard comes with dozens of predefined functions, and is easily extensible so users can add their own.

### Example usage
```
$ w sh
wizard show

  disk
  history
  largest-packages
  next-break
  progress
  weather

$ w show disk
Filesystem               Size  Used Avail Use% Mounted on
/dev/sda1                 35G   12G   22G  36% /
devtmpfs                 7.8G     0  7.8G   0% /dev
shmfs                    7.8G   80M  7.8G   1% /dev/shm
tmp                      7.8G  3.1M  7.8G   1% /tmp
tmpfs                    1.6G   16K  1.6G   1% /run
tmpfs                    5.0M     0  5.0M   0% /run/lock
run                      7.8G  704K  7.8G   1% /var/host/dbus
/dev/mapper/encstateful   11G  135M   11G   2% /var/host/timezone
/dev/root                1.7G  1.6G  169M  91% /lib/modules/3.14.0
media                    7.8G     0  7.8G   0% /var/host/media
none                     7.8G     0  7.8G   0% /sys/fs/cgroup
none                     7.8G  4.0K  7.8G   1% /sys/fs/selinux
```

# qcrypt
Full featured command line encryption with OpenSSL

`usage: qcrypt (-a|-e|-d) target`
- The `-a` switch signals *auto* mode, in which qcrypt detects whether to
  encrypt or decrypt the file based on the targets file extension. qcrypt
  defaults to encryption.
- The `-e` or `-d` switches to signal encryption or decryption.

Requires
- `openssl`
- `tar`
- `md5sum`

**qcrypt** compresses the target directory or file and uses AES256 salted
password encryption provided through `openssl`.

**qcrypt** uses the `.qaes256` file extension to denote its own archives. The
existence or lack there of these extensions is what determines whether the
argument is a target or archive, and which compression program to use.

**qcrypt** computes a checksum of the the target before compression and
encryption to ensure that the archive has not been tampered with at rest. This
checksum is then verified after successful decryption. It also maintains the
date the archive was created to alert the user when last the archive was opened
upon successful decryption. This information is kept inside the archive itself
and is removed upon decryption and presentation to the user.

**qcrypt** has extensive error checking. It alerts the user if any part of the
encryption or decryption process fails or is interrupted and attempts to clean
up its working files. The original target or archive is preserved during
encryption and decryption failure respectively so that another attempt can be
made once the problem is remedied.

**qcrypt** supplies a warning if it detects re-encryption of an archive,
however this action is not disallowed. Likewise, attempted decryption of an
archive without a qcrypt file extension will produce a warning but is allowed.
This allows the user to rename archives and remove the qcrypt file extension if
so desired.
