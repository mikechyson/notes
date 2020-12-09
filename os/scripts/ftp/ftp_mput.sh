#!/bin/bash



host=
username=
password=
remotedir=

# $1: filename 
# or * standing for all files in the current directory
# -i: Turns off interactive prompting during multiple file transfers.
# -n: Restrains ftp from attempting "auto-login" upon initial connection.
ftp  -ni <<- EOF
open $host
user ${username} ${password}
binary
cd $remotedir
mput $1
close
bye
!
EOF


