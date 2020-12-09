#!/bin/bash


filename=$1
ftp_ip=$2
ftp_port=$3

username=etla
password=etla@#123

time_file=/mfs/gn_enc_ftp/time.log

echo "$1 start `date +'%H:%M:%S'`" >>  $time_file

ftp  -ni <<- EOF
open $ftp_ip $ftp_port
user ${username} ${password}
binary
put $filename
close
bye
!
EOF

echo "$1 end  `date +'%H:%M:%S'`" >>  $time_file

rm $1
