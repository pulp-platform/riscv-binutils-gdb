# Absence of xcorev or xpulpmacrnhi march option disables all CORE-V mac extensions
target:
	p.mac t4, t2, t0
	p.msu t4, t2, t0
	p.muls t4, t2, t0
	p.mulhhs t4, t2, t0
	p.mulsn t4, t2, t0, 4
	p.mulhhsn t4, t2, t0, 16
	p.mulsrn t4, t2, t0, 10
	p.mulhhsrn t4, t2, t0, 17
	p.mulu t4, t2, t0
	p.mulhhu t4, t2, t0
	p.mulun t4, t2, t0, 7
	p.mulhhun t4, t2, t0, 16
	p.mulurn t4, t2, t0, 11
	p.mulhhurn t4, t2, t0, 9
	p.macsn t4, t2, t0, 24
	p.machhsn t4, t2, t0, 11
	p.macsrn t4, t2, t0, 9
	p.machhsrn t4, t2, t0, 24
	p.macun t4, t2, t0, 27
	p.machhun t4, t2, t0, 18
	p.macurn t4, t2, t0, 25
	p.machhurn t4, t2, t0, 5
