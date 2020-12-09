#!/bin/bash

# Date: 2018/06/02
# Author: Michael Chyson
# Mail: chyson@aliyun.com
# Version: 1.0
# Update:
# Description:
# start and stop the java application on the local host.
# app-config.json the json confiuration file need for the java application.


ACTION=$1
if [ "$ACTION" != "start" -a "$ACTION" != "stop" ]; then
    echo "usage: $0 start|stop"
    exit 1
fi



# specify the dirs
BASE=`dirname $0`
BASE=`cd ${BASE}; pwd`
CONF_PATH=$BASE/app-config.json
JAR_PATH=$BASE/app-1.0-SNAPSHOT.jar
START_LOG=$BASE/app-start-log/app-start.log
START_LOG_DIR=$BASE/app-start-log

# mkdir start-log directory, if it does not exist
if [ ! -e $START_LOG_DIR ]; then
    mkdir -p $START_LOG_DIR
fi



function start() {
    PROTMP=`ps -ef | grep "$JAR_PATH" | grep -v grep | awk '{print $2}'`

    # if the pid does not exist, start a process
    if [ -z "$PROTMP" ]; then
        nohup java -jar "$JAR_PATH"  $CONF_PATH >> $START_LOG_DIR 2>&1 & 

        # if the start operation succeeded
        if [ $? -eq 0 ]; then
            PROTMP=`ps -ef | grep "$JAR_PATH" | grep -v grep | awk '{print $2}'`
	    if [ -z "$PROTMP" ]; then
	        echo "start executed, but failed"
	    else
	        echo "process: $PROTMP started successfully."
	    fi
        fi
    else
        echo "process $PROTMP already started."
    fi    
}

function stop(){
    PROCESSES=`ps -ef | grep "$JAR_PATH" | grep -v grep | awk '{print $2}'`
    # if the processes is not none, kill all the processes
    if [ "$PROCESSES" != "" ]; then
        for PROCESS in $PROCESSES
        do
            kill -9 $PROCESS
            echo "process: $PROCESS killed"
        done
    fi
}

if [ "$ACTION" = "start" ]; then
    start 
fi

if [ "$ACTION" = "stop" ]; then
    stop 
fi

