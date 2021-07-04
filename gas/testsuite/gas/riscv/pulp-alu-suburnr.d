#as: -march=rv32i_xpulpaddsubrn
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+c1fe72db[ 	]+p.suburnr[ 	]+t0,t3,t6
[ 	]+4:[ 	]+c1c2ffdb[ 	]+p.suburnr[ 	]+t6,t0,t3
[ 	]+8:[ 	]+c05ffe5b[ 	]+p.suburnr[ 	]+t3,t6,t0
