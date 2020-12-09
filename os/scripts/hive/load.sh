#!/bin/bash

if [ $# -ne 3 ]; then 
	echo "usage: $0 <hadoop_filename> <hive_tablename> <time>"
	exit 2
fi


_filename=$1
_tablename=$2
_time=$3

_month=${_time:0:6}
_day=${_time:0:8}
_hour=${_time:0:10}
_partition="(month_id=${_month},day_id=${_day},hour_id=${_hour})"

#echo $_filename
#echo $_tablename
#echo $_time
#echo $_partition

#hadoop fs -ls $_filename
#if [ $? -eq 0 ]; then
	beeline -u jdbc:hive2://192.168.58.9:10000/hivehpdwshare -p hpdwshare@#123 -n hpdwshare -e "use hivehpdwshare;load data inpath '${_filename}' into table ${_tablename} PARTITION ${_partition};"
	#echo beeline

#else
#	exit 1
#fi
