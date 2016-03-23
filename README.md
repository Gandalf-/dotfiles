# qcrypt
Full featured command line encryption with OpenSSL
- `usage: qcrypt [-a|-e|-d] target_name`
- The `-e` or `-d` switches to signal encryption or decryption.
- The `-a` switch signals *auto* mode, in which qcrypt detects whether to encrypt or decrypt the file based on the targets file extension. Without any flags, it runs in interactive mode where it prompts the user to specify encryption or decryption.
- The `-z` and `-t` flags allow you to require qcrypt to use `zip` or `tar` respectively for compression. By default qcrypt uses which ever is available on the users system, preferring `zip` if both are present.

Requires
- `openssl`
- `zip` *or* `tar`
- `md5sum`


qcrypt compresses the target directory or file and uses AES256 salted password encryption provided through `openssl`.

qcrypt uses the `.zaes256` and `.taes256` file extensions to denote its own archives.

qcrypt computes a checksum of the the target before compression and encryption to ensure that the archive has not been tampered with at rest. This checksum is then verified after successful decryption. It also maintains the date the archive was created to alert the user when last the archive was opened upon successful decryption. This information is kept inside the qcrypt archive itself and is removed upon decryption and presentation to the user. 

qcrypt has extensive error checking. It alerts the user if any part of the encryption or decryption process fails or is interrupted and attempts to clean up its working files. The original target or archive is preserved during encryption and decryption failure respectively so that another attempt can be made once the problem is remedied.

A warning is supplied if qcrypt detects re-encryption of a qcrypt archive, however this action is not disallowed. Likewise, attempted decryption of an archive without a qcrypt file extension will produce a warning but is allowed. This allows the user to rename archives and remove the qcrypt file extension. Without the `-z` or `-t` flags to denote the compression program used, qcrypt will default to using `zip` on the archive when it is unable to detect the compression through the file extension.