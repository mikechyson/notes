#!/bin/bash


# Description:
# scp the file in the current file to the same directory on all the hosts configured in the hosts file to

FILE=$1

BASE=`dirname $0`
BASE=`cd ${BASE}; pwd`
CONF_HOSTS=${BASE}/hosts

HOSTS=(`grep -Ev "^$|^[#/;]" $CONF_HOSTS`)
HOST_LENGTH=$[${#HOSTS[@]}-1]

for i in `seq 1 $HOST_LENGTH`
do 
	echo ${HOSTS[i]}
	scp -r $FILE ${HOSTS[i]}:`dirname $BASE`/ 
done
