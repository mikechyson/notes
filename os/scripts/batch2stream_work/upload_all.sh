#!/bin/bash



BASE=`dirname $0`
BASE=`cd ${BASE}; pwd`



function parallel(){
    kind=$1
    port=$2

    for j in {0..9};do
    	$BASE/upload3.sh 172.30.191.60 2122 ${kind} 20180627140${j} 1024  &
	sleep 60
    done

    for j in {10..59};do
    	$BASE/upload3.sh 172.30.191.60 2122 ${kind} 2018062714${j} 1024  &
	sleep 60
    done
}


for i in {0..9};do 
    echo 10${i}
    parallel 10$i &
done




