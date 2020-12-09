#!/bin/bash


if [ $# -ne 2 ];then
	echo "usage $0 data_dir time"
	exit 1
fi


data_dir=$1
time=$2

ftp_ip=172.30.191.6
ftp_port=2122
threshold=900

base_dir=`dirname $0`
base_dir=`cd ${base_dir}; pwd`


large_sh=${base_dir}/upload_large.sh
little_sh=${base_dir}/upload_little.sh

for kind in 10{0..9}; do
    $large_sh  $ftp_ip $ftp_port $kind $time $threshold $data_dir
    $little_sh $ftp_ip $ftp_port $kind $time $threshold $data_dir &
done






