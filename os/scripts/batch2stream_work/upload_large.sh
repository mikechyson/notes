#!/bin/bash



if [ $# -ne 6 ]; then
	echo "usage: $0 ftp_ip(1-9) ftp_port(0-9) kind time threshold data_dir"
    exit 1
fi


ftp_ip=$1       
ftp_port=$2
kind=$3
time=$4	
threshold=$5
data_dir=$6


base_dir=`dirname $0`
base_dir=`cd ${base_dir}; pwd`
ftp_upload_sh=${base_dir}/ftp_upload.sh
data_dir_basename=`basename $data_dir`
large_file_dir=/mfs/gn_enc_ftp/${data_dir_basename}/largefiles_${time}
patten="S1U_${kind}_*.dat"


if [ ! -d $large_file_dir ]; then
    mkdir -p $large_file_dir
fi


# move large files into large file directory
find $data_dir -size +${threshold}M -name "$patten" -exec mv {} $large_file_dir \;

cd $large_file_dir


ls $patten &> /dev/null
if [ $? -eq 0 ]; then
	file_array=(`ls $patten`)
	total_index=$RANDOM
	
	for i in ${file_array[@]}; do
	   $ftp_upload_sh $i  ${ftp_ip}$[${total_index}%100/10] ${ftp_port}$[${total_index}%10] &
	    let total_index+=1
	done
fi




