#!/bin/bash



# +---------------+-------------------------------------------------------+
# |Date           |2018-06-02                                             |
# +---------------+-------------------------------------------------------+
# |Author         |Mingming Li                                            |
# +---------------+-------------------------------------------------------+
# |Mail           |limm3@asiainfo.com                                     |
# +---------------+-------------------------------------------------------+
# |Version        |3.0                                                    |
# +---------------+-------------------------------------------------------+
# |Update         |                                                       |
# +---------------+-------------------------------------------------------+
# |Description    |calculate the current offset of the kafka topic with   |
# |               |the specified group id.                                |
# +---------------+-------------------------------------------------------+
#                                                                          




if [ $# -ne 1 ];then
    echo "usage: $0 group_id"
    exit 1
fi

group_id=$1


function sum(){
	group_id=$1
	local sum=0
	
	for i in `/usr/hdp/2.5.0.0-1245/kafka/bin/kafka-consumer-groups.sh \
			--bootstrap-server localhost:9092  \
			--new-consumer \
			--describe \
			--group $group_id | sed '1d' | awk -F ' ' '{print $5}'`; do
		sum=$((${sum} + $i))
	done
	echo $sum
}	


offset=`sum $group_id`
echo $offset
