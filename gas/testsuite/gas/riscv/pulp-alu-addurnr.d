#as: -march=rv32i_xpulpaddsubrn
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+c1fe62db[ 	]+p.addurnr[ 	]+t0,t3,t6
[ 	]+4:[ 	]+c1c2efdb[ 	]+p.addurnr[ 	]+t6,t0,t3
[ 	]+8:[ 	]+c05fee5b[ 	]+p.addurnr[ 	]+t3,t6,t0
