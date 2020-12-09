#!/bin/bash




if [ $# -ne 5 ]; then
    echo "useage: $0 <ftp_ip> <ftp_port> <kind> <time> <size(M)>"
    exit 1
fi

ftp_ip=$1       
ftp_port=$2	        			# ftp port
kind=$3         				# concatenate kind
time=$4		                		# concatenate time
let size=${5}*1024*1024      			# concatenate size limit

total_index=$RANDOM

BASE=`dirname $0`
BASE=`cd ${BASE}; pwd`
SUBDIR_PREFIX=S1U_SUB_
parent_dir=${BASE##*/}
large_file_dir=$BASE/large_file_dir_${time}


filename_prefix=S1U_${kind}_${time}
filename_suffix=_${parent_dir}.dat
patten=S1U_${kind}_*_${time}*.dat
time_subdir=${SUBDIR_PREFIX}${time}

time_file=${BASE}/time_${kind} # for time record in test

# the put method put file to the ftpserver.
function put(){
    filename=$1
    username=etla
    password=etla@#123
    ftp_ip=$2
    ftp_port=$3

    ftp  -ni <<- EOF
open $ftp_ip $ftp_port
user ${username} ${password}
binary
put $filename
close
bye
!
EOF
}



# concatenate files into a file parallel
function catput(){
    start_index=$1
    end_index=$2
    count=$3
    cat_filename=${filename_prefix}_${count}${filename_suffix}

    echo "${filename_prefix}, start time: `date`" >> $time_file # for test
    for i in `seq $start_index $end_index`; do
	cat ${file_array[i]} >> ${cat_filename}.tmp
	#rm ${file_array[i]} 
    done
    mv ${cat_filename}.tmp ${cat_filename}
    put ${cat_filename} ${ftp_ip}$[${total_index}%100/10] ${ftp_port}$[${total_index}%10]
    rm ${cat_filename}
    echo "${filename_prefix}, end   time: `date`" >> $time_file
}



function parallel(){
    ls $patten &> /dev/null	# judge the files existence
    if [ $? -eq 0 ]; then

	mv $patten ${time_subdir}
	cd ${time_subdir}

	size_array=(`ls -l $patten | awk '{print $5}'`)
	file_array=(`ls -l $patten | awk '{print $9}'`)
	array_length_sub_1=$[${#size_array[*]}-1]
	sum=0
	start_index=0
	end_index=0
	count=0


	for i in `seq 0 $array_length_sub_1`; do
	    end_index=$i
	    let sum+=${size_array[$i]}
	    if [ $sum -ge $size -o $i -eq $array_length_sub_1 ]; then
		catput $start_index $end_index $count  &>/dev/null &
		sum=0
		let count+=1
		let start_index=$i+1
		let total_index+=1
	    fi
	done
    fi
}

# function put_time(){
#     file=$1
#     current_start_index=$2
#     echo "large file start `date`" >> $time_file
#     put $file ${ftp_port}$[${current_start_index}%10]
#     echo "large file end   `date`" >> $time_file
# }

# function put_large(){
#     if [ ! -d $large_file_dir ]; then
# 	mkdir -p $large_file_dir
#     fi

#     find $BASE \( -size +${size}G -o -size ${size}G \) -exec mv {} $large_file_dir \;
#     cd $large_file_dir
#     file_array=(`ls`)
#     count=0
#     current_minute=`date +"%M"`
#     current_start_index=${current_minute:1:1}

#     for i in ${file_array[@]}; do
# 	# put $i &
# 	put_time $i $current_start_index &
# 	let current_start_index+=1
#     done
# }




########################################## script start ###########################################

# make a subdir for this time files
if [ ! -d ${time_subdir} ];then 
    mkdir ${time_subdir}
fi



# test
function minute_function(){
    kind=$1

    for j in {0..9};do
    	$BASE/upload3.sh 172.30.191.6 2122 ${kind} 20180627140${j} 1024  &
	sleep 60
    done

    for j in {10..59};do
    	$BASE/upload3.sh 172.30.191.6 2122 ${kind} 2018062714${j} 1024  &
	sleep 60
    done
}

for i in {0..9}; do
    minute_function 10$i &
done




# formal
#put_large
#parallel


