# DotFiles

- configuration files for *vim*, *tmux*, *bash*, *irssi*, *fish*, *pylint*, *git*. 
- a number of scripts, described below

## g

Examples
```
$ g s
$ g dh 2
$ g s d a cm 'Update readme' f pl - ph
```

Usage
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


## wizard

Examples
```
$ wizard show disk --help
$ w s d
$ wizard update pip --help
$ w u p
```

Usage
```
wizard (-q | -s | -e)                       

  add ...                                   
  bookmark                                  
  clean ...                                 
  do ...                                    
  install ...                               
  insync ...                                
  make ...                                  
  mirror ...                                
  open                                      
  pkg ...                                   
  quick ...                                 
  show ...                                  
  start ...                                 
  update ... 
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
