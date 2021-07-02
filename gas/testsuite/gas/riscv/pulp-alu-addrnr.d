#as: -march=rv32i_xpulpaddsubrn
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+41fe62db[ 	]+p.addrnr[ 	]+t0,t3,t6
[ 	]+4:[ 	]+41c2efdb[ 	]+p.addrnr[ 	]+t6,t0,t3
[ 	]+8:[ 	]+405fee5b[ 	]+p.addrnr[ 	]+t3,t6,t0
