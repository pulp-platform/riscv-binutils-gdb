# Five bit immediate must be an absolute value
target:
	p.addn t4,t2,t0,t3
	p.addun t4,t2,t0,t3
	p.addrn t6,t0,t3,t2
	p.addurn t6,t0,t3,t2
	p.subn t6,t0,t3,t2
	p.subun t6,t0,t3,t2
	p.subrn t6,t0,t3,t2
	p.suburn t6,t0,t3,t2
