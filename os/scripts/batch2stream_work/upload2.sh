#!/bin/bash


# +---------------+-------------------------------------------------------------+
# |Date           |2018-06-29                                                   |
# +---------------+-------------------------------------------------------------+
# |Author         |Mingming Li                                                  |
# +---------------+-------------------------------------------------------------+
# |Mail           |limm3@asiainfo.com                                           |
# +---------------+-------------------------------------------------------------+
# |Version        |5.0                                                          |
# +---------------+-------------------------------------------------------------+
# |Update         |move the concatenated files to a new direcotry               |
# +---------------+-------------------------------------------------------------+
# |Description    |Concatenate files every 1 minutes. Put the concatenated files|
# |               |to the ftpserver and ftpserver write the data onto hdfs and  |
# |               |kafka. After that remove the concatenated files. If the      |
# |               |concatenated files greater than 1G, generate a new file.     |
# +---------------+-------------------------------------------------------------+



if [ $# -ne 4 ]; then
    echo "useage: $0 kind time size(M) directory"
    echo "for example:"
    echo "$0 108 201806271400 1024 /gpfs/gn_great"
    exit 1
fi


kind=$1         				# concatenate kind
time=$2		                		# concatenate time
#size=$3                         		# concatenate size limit
let size=${3}*1024*1024      			# concatenate size limit
to_dir=$4



BASE=`dirname $0`
BASE=`cd ${BASE}; pwd`
SUBDIR_PREFIX=S1U_SUB_


host_ip=`hostname -I`
#172.23.77.33 192.168.122.1  
host_ip=${host_ip#* }		# get the latter ip
host_ip=${host_ip%% }		# substract the whitespace at the end

filename_prefix=S1U_${kind}_${time}
filename_suffix=_${host_ip}.dat
patten=S1U_${kind}_*_${time}*.dat
#time_subdir=${SUBDIR_PREFIX}${time}
time_file=${BASE}/time_${kind}.txt # for time record

# echo "patten=S1U_${kind}*${time}*.dat"
# echo "time_subdir=${SUBDIR_PREFIX}${time}"
# echo "time_file=${BASE}/${filename_prefix}_time.txt"

# the put method put file to the ftpserver.
# the file will be written to hdfs and kafka.
function put(){
    filename=$1
    host=10.4.56.224
    port=21221
    username=gnftp
    password=gnftp%123

    ftp  -ni <<- EOF
open $host $port
user ${username} ${password}
binary
put $filename
close
bye
!
EOF

}



# independent for background task for the parallel reason
# concatenate files into a file
function catput(){
    start_index=$1
    end_index=$2
    count=$3

    cat_filename=${filename_prefix}_${count}${filename_suffix}
    echo "${filename_suffix}, start time: `date`" >> $time_file
    for i in `seq $start_index $end_index`; do
	echo "cat ${file_array[i]} >> ${cat_filename}" >> ${BASE}/debug.log
	cat ${file_array[i]} >> ${cat_filename}.tmp
	# when the file has been concatenated into the total file, move the former file to a new 
	# mv ${file_array[i]} $to_dir 	# for test
#	cp ${file_array[i]} $to_dir
    done
    mv ${cat_filename}.tmp ${cat_filename}
    ls -lh ${cat_filename} >> $time_file
    put ${cat_filename} 
    rm ${cat_filename}
    echo "${filename_suffix}, end   time: `date`" >> $time_file
}



function parallel(){

    ls $patten &> /dev/null	# judge the files existence
    if [ $? -eq 0 ]; then
	# make a subdie for this time files
	if [ ! -d ${time_subdir} ];then
	    mkdir ${time_subdir} &> /dev/null
	fi

#	mv $patten ${time_subdir}
#	cp $patten ${time_subdir} # for test
#	cd ${time_subdir}

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
		catput $start_index $end_index $count &>/dev/null &
		sum=0
		let count+=1
		let start_index=$i+1
	    fi
	done
	
    fi
}





########################################## script start ###########################################



parallel
