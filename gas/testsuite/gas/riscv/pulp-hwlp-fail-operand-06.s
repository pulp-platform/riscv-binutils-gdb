# Loop count must be positive integer in range:[0, 4095]
target:
	lp.counti x0, -1
	lp.setupi x0, -1, 4
	lp.counti x0, -832
	lp.setupi x0, -291, 8
	lp.counti x0, 4096
	lp.setupi x0, 4096, 8
	lp.counti x0, 74285
	lp.setupi x0, 8334, 8
