#!/bin/bash



# +---------------+-------------------------------------------------------+
# |Date           |2018-07-30                                             |
# +---------------+-------------------------------------------------------+
# |Author         |Mingming Li                                            |
# +---------------+-------------------------------------------------------+
# |Mail           |limm3@asiainfo.com                                     |
# +---------------+-------------------------------------------------------+
# |Version        |1.0                                                    |
# +---------------+-------------------------------------------------------+
# |Update         |                                                       |
# +---------------+-------------------------------------------------------+
# |Description    |check the hdfs files, if the specified files is less   |
# |               |than 10, generate an alarm message.                    |
# |               |                                                       |
# +---------------+-------------------------------------------------------+
#
#
#
#
#




# check the hadoop files three minutes ealier
DATE=`date -d -3minute +"%Y%m%d/%H/%M"`
MCNUM=`hadoop fs -ls /zqdj/mc/${DATE} | grep csv | wc -l`
echo $MCNUM

MMENUM=`hadoop fs -ls /zqdj/s1mme/${DATE} | grep csv | wc -l`
echo $MMENUM




passwd='Safe1!2@3#'
username=
host=
remote_dir=

function send_msg
{
    tmpshell=$1
    /usr/bin/expect <<-EOF
	set time 30
	spawn /usr/bin/scp $tmpshell $username@$host:$remote_dir
	expect {
	"yes/no" { send "yes\r";exp_continue }
	"*password:" { send "$passwd\r" }
	}
	expect eof
	spawn ssh -p 22 $username@$host
	expect {
	"yes/no" { send "yes\r";exp_continue }
	"*password:" { send "$passwd\r" }
	}
	expect "*$"
	send "cd $remote_dir; sh $tmpshell\r"
	expect "*$"
	send "exit\r"
	expect eof
	EOF
    rm $tmpshell
}

tmp_shell="kafka2hdfs_alert.sh"
echo $tmp_shell


host=
port=9432
database=dqmetl
user=etl

if [ $MCNUM -lt 10 ];then
    echo "#!bin/bash --login
       . /home/postgres/.bash_profile" > ${tmp_shell}
    msg="政企对接hadoop mc文件个数不足10个告警"
    alarm_id="powerstore_alert_cmod"
    echo "psql -h $host -p $port -d $database -U $user -c \"insert into td_send_bomc (o_id,sub_module,alarm_id,instance_id,alarm_content,alarm_flag) values ('9001.221','SHELL','${alarm_id}',to_char(current_timestamp,'yyyymmddhh24missms'),'${msg}',1)\" " >>${tmp_shell}                 
    echo $msg
    send_msg "${tmp_shell}"
fi

if [ $MMENUM -lt 10 ];then
    echo "#!bin/bash --login
       . /home/postgres/.bash_profile" > ${tmp_shell}
    msg="政企对接hadoop mme文件个数不足十个告警"
    alarm_id="zqdj_hadoop_mme"
    echo "psql -h $host -p $port -d $database -U $user -c \"insert into td_send_bomc (o_id,sub_module,alarm_id,instance_id,alarm_content,alarm_flag) values ('9001.221','SHELL','${alarm_id}',to_char(current_timestamp,'yyyymmddhh24missms'),'${msg}',1)\" " >>${tmp_shell}                 
    echo $msg
    send_msg "${tmp_shell}"
fi
