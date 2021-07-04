# xcorev march option works for all CORE-V general ALU ops extensions
target:
	p.abs t4,t2
	p.slet t4,t2,t6
	p.sletu t4,t2,t6
	p.min t4,t2,t6
	p.minu t4,t2,t6
	p.max t4,t2,t6
	p.maxu t4,t2,t6
	p.exths t4,t2
	p.exthz t4,t2
	p.extbs t4,t2
	p.extbz t4,t2
	p.clip t4,t2,5
	p.clipu t4,t2,5
	p.clipr t4,t2,t6
	p.clipur t4,t2,t6
	p.addn t4, t2, t0, 4
	p.addun t4, t2, t0, 4
	p.addrn t6, t0, t3, 9
	p.addurn t6, t0, t3, 14
	p.addnr t6, t0, t3
	p.addunr t6, t0, t3
	p.addrnr t6, t0, t3
	p.addurnr t6, t0, t3
	p.subn t6, t0, t3, 6
	p.subun t6, t0, t3, 24
	p.subrn t6, t0, t3, 21
	p.suburn t6, t0, t3, 3
	p.subnr t6, t0, t3
	p.subunr t6, t0, t3
	p.subrnr t6, t0, t3
	p.suburnr t6, t0, t3
