#!/bin/bash


# Date: 2018/06/02
# Author:  Michael Chyson
# Mail:
# Version: 1.0
# Update:
# Description:
# Start and stop all the java applications.
# hosts file specifies the hosts on which java application will start or stop
              


ACTION=$1
if [ "$ACTION" != "start" -a "$ACTION" != "stop" ]; then
    echo "usage: $0 start|stop"
    exit 1
fi



BASE=`dirname $0`
BASE=`cd ${BASE}; pwd`
CONF_HOSTS=${BASE}/hosts
KAFKA_SH=$BASE/app.sh


# a list consists of all the hosts
HOSTS=(`grep -Ev "^$|^[#/;]" $CONF_HOSTS`)



if [ "$ACTION" = "start" ]; then
    for i in ${HOSTS[@]}
    do
        echo $i
        ssh -o ConnectTimeout=2 $i "$APP_SH start"
        echo ------------
    done
fi



if [ "$ACTION" = "stop" ]; then
    for i in ${HOSTS[@]}
    do
        echo $i
        ssh -o ConnectTimeout=2 $i "$APP_SH stop"
        echo ------------
    done
fi


