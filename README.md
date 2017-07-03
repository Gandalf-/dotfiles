# DotFiles
My personal configuration files!

## g
Super *git* wrapper.

Any number of commands can be chained together, making your git workflow much
simpler. For example, `g s d a cm 'Updating readme.md  ph`, would run the
following commands in order, pausing for input and confirmation at each step

- git status
- git diff 
- git add -A
- git commit -m 'Updating readme.md'
- git push

```
  g
    !  : toggle confirmation
    a  : add everything
    bv : branch -vv
    bn : checkout (remote branch) -b (local branch)
    c  : commit
    ca : commit amend
    cb : checkout -b (branch)
    cs : attempt to checkout branch by bug name [work only]
    cm : commit -m (message)
    co : checkout (file)
    d  : diff changes [output_file]
    dh : diff commits [number of commits] [output_file]
    ds : diff commits [number of commits] - auto names diff [work only]
    f  : fetch
    l  : log
    ll : log graph
    s  : status
    rv : remote -vv
    ri : interactive rebase
    p  : pause
    pl : pull [branch]
    ph : push [branch]
    pf : push --force [branch]
```


## qcrypt
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


## cleanup
Smart remove duplicate and temporary file names

Suppose you have the following directory structure,
```
$ ls -l directory
file (1).txt
file.txt
data (1).csv
program.pyc
```
**cleanup** will remove `file (1).txt` and `program.pyc` and rename 
`data (1).csv` to `data.csv`. `file (1).txt ` is removed because `file.txt`
exists.
