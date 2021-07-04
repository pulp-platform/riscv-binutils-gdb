# Destination must be of type register
target:
	p.mac 8, t2, t0
	p.msu 23, t2, t0
	p.muls 43, t2, t0
	p.mulhhs 7, t2, t0
	p.mulsn 345, t2, t0, 4
	p.mulhhsn 356, t2, t0, 16
	p.mulsrn 867, t2, t0, 10
	p.mulhhsrn 3454, t2, t0, 17
	p.mulu 9, t2, t0
	p.mulhhu 54, t2, t0
	p.mulun 965, t2, t0, 7
	p.mulhhun 35, t2, t0, 16
	p.mulurn 87, t2, t0, 11
	p.mulhhurn 38, t2, t0, 9
	p.macsn 985, t2, t0, 24
	p.machhsn 83, t2, t0, 11
	p.macsrn 960, t2, t0, 9
	p.machhsrn 385, t2, t0, 24
	p.macun 58, t2, t0, 27
	p.machhun 6, t2, t0, 18
	p.macurn 35, t2, t0, 25
	p.machhurn 67, t2, t0, 5
