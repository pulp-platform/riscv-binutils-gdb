# Branch offset must be an even integer in range:[0, 4094]
# 4096 is the upper boundary as 4095 truncates to 4094
target:
	lp.starti x0, -1
	lp.endi x1, -1
	lp.setup x0, t1, -1
	lp.starti x0, -6
	lp.endi x1, -94
	lp.setup x0, t2, -842
	lp.starti x0, 4096
	lp.endi x1, 4096
	lp.setup x0, t3, 4096
	lp.starti x0, 8944
	lp.endi x1, 7382
	lp.setup x0, t4, 20394
