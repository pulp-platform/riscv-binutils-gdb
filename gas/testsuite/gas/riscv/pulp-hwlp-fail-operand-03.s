# Branch offset must be an even integer in range:[0, 30]
# 32 is the upper boundary as 31 truncates to 30
target:
	lp.setupi x0, 1056, -1
	lp.setupi x0, 14, -14
	lp.setupi x0, 67, 32
	lp.setupi x0, 356, 7366
