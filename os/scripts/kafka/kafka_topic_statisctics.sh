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
# |Description    |                                                       |
# |               |                                                       |
# |               |                                                       |
# +---------------+-------------------------------------------------------+
#
#
#
#



if [ $# -ne 3 ];then
    echo "usage: $0 topic"
    exit 1
fi

topic=$1


function sum(){
    topic=$1
    local sum=0
    
    for i in `/usr/hdp/2.5.0.0-1245/kafka/bin/kafka-run-class.sh kafka.tools.GetOffsetShell \
                              --broker-list YSPETL12:6667 \
                              --topic $topic --time -1 | awk -F ':' '{print $3'}`; do
	sum=$((${sum} + $i))
    done
    echo $sum
}	


offset_start=$(sum $topic)
echo `date` >> ${topic}.txt	
echo $offset_start >> ${topic}.txt


