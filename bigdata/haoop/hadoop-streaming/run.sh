#!/bin/bash


hadoop jar /usr/hdp/2.5.0.0-1245/hadoop-mapreduce/hadoop-streaming.jar \
-D stream.non.zero.exit.is.failure=false \
-file "/home/ocdp/hadoop-python/mapper.py" \
-mapper "/home/ocdp/hadoop-python/mapper.py" \
-input "/hadoop-python/test.log" \
-output "/hadoop-python/test.out30"




#-file "/home/ocdp/hadoop-python/reduce.py" \
#-reducer "/home/ocdp/hadoop-python/reduce.py" \


#-D stream.non.zero.exit.is.failure=false \

#set mapred.job.queue.name=hpdwshare \
