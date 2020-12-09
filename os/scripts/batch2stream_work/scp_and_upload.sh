#!/bin/bash

yz_host=$1
#current_minute=$2
current_minute=`date +"%Y%m%d%H%M"`
ten_minutes_ago=`date -d "10 minute ago" +"%Y%m%d%H%M"`
data_dir=/mfs/gn_enc_${yz_host}


base_dir=`dirname $0`
base_dir=`cd ${base_dir}; pwd`


$base_dir/scp.expect $yz_host $current_minute
$base_dir/upload.sh $data_dir $current_minute
$base_dir/rm_tmp_dir.expect $yz_host $ten_minutes_ago
