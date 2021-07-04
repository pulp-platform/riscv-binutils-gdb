# Source one must be of type register
target:
	p.mac t4, 43, t0
	p.msu t4, 3, t0
	p.muls t4, 345, t0
	p.mulhhs t4, 54, t0
	p.mulsn t4, 4, t0, 4
	p.mulhhsn t4, 35, t0, 16
	p.mulsrn t4, 53, t0, 10
	p.mulhhsrn t4, 4456, t0, 17
	p.mulu t4, 868, t0
	p.mulhhu t4, 95, t0
	p.mulun t4, 584, t0, 7
	p.mulhhun t4, 37545, t0, 16
	p.mulurn t4, 943, t0, 11
	p.mulhhurn t4, 34, t0, 9
	p.macsn t4, 93, t0, 24
	p.machhsn t4, 584, t0, 11
	p.macsrn t4, 28, t0, 9
	p.machhsrn t4, 9, t0, 24
	p.macun t4, 834, t0, 27
	p.machhun t4, 92, t0, 18
	p.macurn t4, 49, t0, 25
	p.machhurn t4, 6, t0, 5
