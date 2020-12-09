#!/bin/bash


if [ $# -ne 6 ]; then
    exit 1
fi


ftp_ip=$1       
ftp_port=$2
kind=$3
time=$4	
let threshold=${5}*1024*1024
data_dir=$6

base_dir=`dirname $0`
base_dir=`cd ${base_dir}; pwd`
ftp_upload_sh=${base_dir}/ftp_upload.sh
filename_prefix=S1U_${kind}_${time}
data_dir_basename=`basename $data_dir`
time_dir=/mfs/gn_enc_ftp/${data_dir_basename}/time
time_file=/mfs/gn_enc_ftp/${data_dir_basename}/time/time_${kind} # for time record in test
parent_dir=$data_dir_basename
filename_suffix=_${parent_dir}.dat
little_file_dir=/mfs/gn_enc_ftp/${data_dir_basename}/littlefiles_${time}

pattern=S1U_${kind}_*.dat

if [ ! -d $little_file_dir ]; then
    mkdir -p $little_file_dir &> /dev/null
fi

if [ ! -d $time_dir ]; then
    mkdir -p $time_dir &> /dev/null
fi

ls ${data_dir}/$pattern &> /dev/null	# judge the files existence
if [ $? -eq 0 ]; then
    mv ${data_dir}/$pattern $little_file_dir
    cd $little_file_dir
    size_array=(`ls -l $pattern | awk '{print $5}'`)
    file_array=(`ls -l $pattern | awk '{print $9}'`)
    array_length_sub_1=$[${#size_array[*]}-1]
    sum=0
    start_index=0
    end_index=0
    count=0
    total_index=$RANDOM

    for i in `seq 0 $array_length_sub_1`; do
	end_index=$i
	let sum+=${size_array[$i]}
	if [ $sum -ge $threshold -o $i -eq $array_length_sub_1 ]; then
	    cat_filename=${filename_prefix}_${count}${filename_suffix}
	    echo "$cat_filename cat start time: `date`" >> $time_file
	    for i in `seq $start_index $end_index`; do
		cat ${file_array[i]} >> ${cat_filename}.tmp
		rm ${file_array[i]} 
	    done
	    echo "$cat_filename cat end   time: `date`" >> $time_file
	    mv ${cat_filename}.tmp ${cat_filename}
	    $ftp_upload_sh ${cat_filename} ${ftp_ip}$[${total_index}%100/10] ${ftp_port}$[${total_index}%10] &

	    sum=0
	    let count+=1
	    let start_index=$i+1
	    let total_index+=1
	fi
    done
fi


