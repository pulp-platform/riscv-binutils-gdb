# Source two must be of type register
target:
	p.mac t4, t2, 344
	p.msu t4, t2, 23
	p.muls t4, t2, 2
	p.mulhhs t4, t2, 8
	p.mulsn t4, t2, 45, 4
	p.mulhhsn t4, t2, 655, 16
	p.mulsrn t4, t2, 465, 10
	p.mulhhsrn t4, t2, 3534, 17
	p.mulu t4, t2, 46
	p.mulhhu t4, t2, 35
	p.mulun t4, t2, 67, 7
	p.mulhhun t4, t2, 6, 16
	p.mulurn t4, t2, 787, 11
	p.mulhhurn t4, t2, 3545, 9
	p.macsn t4, t2, 6, 24
	p.machhsn t4, t2, 765, 11
	p.macsrn t4, t2, 45, 9
	p.machhsrn t4, t2, 7, 24
	p.macun t4, t2, 98, 27
	p.machhun t4, t2, 654, 18
	p.macurn t4, t2, 900, 25
	p.machhurn t4, t2, 354, 5
