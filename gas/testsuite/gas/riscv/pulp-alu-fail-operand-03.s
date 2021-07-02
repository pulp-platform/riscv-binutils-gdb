# Source 2 must be of type register
target:
	p.slet t4,t2,3
	p.sletu t4,t2,4
	p.min t4,t2,13
	p.minu t4,t2,7
	p.max t4,t2,17
	p.maxu t4,t2,30
	p.clipr t4,t2,18
	p.clipur t4,t2,29
	p.addn t4,t2,24,4
	p.addun t4,t2,6,4
	p.addrn t6,t0,7,9
	p.addurn t6,t0,18,14
	p.addnr t6,t0,15
	p.addunr t6,t0,24
	p.addrnr t6,t0,3
	p.addurnr t6,t0,2
	p.subn t6,t0,1,6
	p.subun t6,t0,8,24
	p.subrn t6,t0,18,21
	p.suburn t6,t0,25,3
	p.subnr t6,t0,14
	p.subunr t6,t0,7
	p.subrnr t6,t0,18
	p.suburnr t6,t0,26
