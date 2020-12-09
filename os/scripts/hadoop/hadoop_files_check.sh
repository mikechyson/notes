#!/bin/bash


# +---------------+-------------------------------------------------------+
# |Date           |2018-06-02                                             |
# +---------------+-------------------------------------------------------+
# |Author         |Mingming Li                                            |
# +---------------+-------------------------------------------------------+
# |Mail           |limm3@asiainfo.com                                     |
# +---------------+-------------------------------------------------------+
# |Version        |1.0                                                    |
# +---------------+-------------------------------------------------------+
# |Update         |                                                       |
# +---------------+-------------------------------------------------------+
# |Description    |check the past 30 minutes hadoop files, if there is no |
# |               |file on hadoop in previous 30 minutes, create a empty  |
# |               |file otherwise do nothing. configure this shell script |
# |               |into the crantab to run it every five minutes at the   |
# |               |right time.                                            |
# +---------------+-------------------------------------------------------+
#


echo `date +"%Y%m%d%H%M00"`
current_time=`date +"%Y%m%d%H%M00"`
# this is to avoid the time effect consumed in the hadoop fs command
time_list=()
for i in {5,10,15,20,25,30}; do 
    time=`date -d -${i}minute +"%Y%m%d%H%M00"`
    time_list+=(${time})
done

echo ${time_list[*]}

# all empty init
not_all_null=flase
for i in ${time_list[*]}; do 
    hadoop_path=/hdfs/mcmmetest/5minutes/s_XXXXX_BAS_XXXX_${i}_000_00_*.dat
    echo $hadoop_path
    files_count=`hadoop fs -ls $hadoop_path 2>/dev/null | wc -l`
    echo $files_count
    if [ $files_count -gt 0 ]; then
	not_all_null=true
	break
    fi
done


if [ $not_all_null = false ]; then
    current_file=s_XXXXX_BAS_XXXX_${current_time}_000_00_*.dat 
    touch $current_file
    hadoop fs -copyFromLocal $current_file /hdfs/mcmmetest/5minutes/
    rm $current_file
fi	
