#!/bin/bash

# Date:
# Author:
# Mail:
# Version:
# Update:
# Description:
# exec command on all the hosts configured in hosts file


BASE=`dirname $0`
BASE=`cd ${BASE}; pwd`
CONF_HOSTS=${BASE}/hosts

hosts=(`grep -Ev "^$|^[#/;]" $CONF_HOSTS`)

for i in ${hosts[@]}
do 
	echo $i
	ssh  $i "$1"
done

