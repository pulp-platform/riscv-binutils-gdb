# Five bit immediate must be an absolute value in range [0, 31]
target:
	p.clip t0,t3,-1
	p.clipu t0,t3,-1
	p.clip t0,t3,-400
	p.clipu t0,t3,-985
	p.clip t0,t3,32
	p.clipu t0,t3,32
	p.clip t0,t3,859
	p.clipu t0,t3,7283
