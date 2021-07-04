# Immediate value must be an absolute expression
target:
	p.mulsn t4, t2, t0, t3
	p.mulhhsn t4, t2, t0, t1
	p.mulsrn t4, t2, t0, t6
	p.mulhhsrn t4, t2, t0, t3
	p.mulun t4, t2, t0, t1
	p.mulhhun t4, t2, t0, t3
	p.mulurn t4, t2, t0, t5
	p.mulhhurn t4, t2, t0, t1
	p.macsn t4, t2, t0, t3
	p.machhsn t4, t2, t0, t5
	p.macsrn t4, t2, t0, t1
	p.machhsrn t4, t2, t0, t6
	p.macun t4, t2, t0, t1
	p.machhun t4, t2, t0, t3
	p.macurn t4, t2, t0, t6
	p.machhurn t4, t2, t0, t5
