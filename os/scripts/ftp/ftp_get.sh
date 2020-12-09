#!/bin/bash


host=
username=
password=
remotedir=

ftp  -ni <<- EOF
open $host
user ${username} ${password}
binary
cd $remotedir
mget $1
close
bye
!
EOF


