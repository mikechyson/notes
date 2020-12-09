#!/usr/bin/python

import sys
import os
import re
from collections import OrderedDict

qs = 'av=;a_cat=;a_nm=;a_pub=;aav=;bh=;cd=;co_f=;ct=;dl=;dm=;ets=;ev=;fv=;gc=;hp=;le=;os=;slv=;sr=;sys=;tz=;ua=;ul=;vtid=;vtvs=;ac=;ad=;branch=;cg_n=;cg_s=;mc_id=;mobile=;nv=;pi=;pn=;pn_id=;pn_sku=;rv=;seg_X=;si_cs=;si_n=;si_p=;si_x=;srch=;ti=;tx_cartid=;tx_e=;tx_i=;tx_id=;tx_it=;tx_s=;tx_t=;tx_u=;sku_id=;goods_id=;category=;location=;goods_type=;vt_f=;uc=;vt_sl=;event=;vt_sid=;co=;cid=;dc=;g_co=;oss=;oss_r='

separator='|'

WTti = 'WT.ti='
WTUL = 'WT.UL='
WTac = 'WT.a_cat='
WTut = 'WT.Noneaut='
file_col = [WTti,WTUL,WTac,WTut]
i = 0

def createDict(dict_string, sp=';'):
	d = OrderedDict()
	ps = dict_string.split(sp)
	for p in ps:
		kv = p.split('=')
		if len(kv) >1:
			d[kv[0]] = kv[1]
	return d



for line in sys.stdin:
	if len(line.strip()) == 0:
		continue	

	line_list = line.split()
	left_string = line_list[0:7]
	right_string = line_list[8:15]
	query_string = line_list[7].replace('WT.','')
	ds = createDict(qs)
	ps = createDict(query_string,'&')
	for k in ps.keys():
		if ds.has_key(k):
			ds[k] = ps[k]
	
	result = separator.join(left_string) + separator
	result = result + separator.join(ds.values()) + separator
	result = result + separator.join(right_string) + '\n'
	print ("%s") % (result)
