#as: -march=rv32i_xpulpaddsubrn
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+41fe72db[ 	]+p.subrnr[ 	]+t0,t3,t6
[ 	]+4:[ 	]+41c2ffdb[ 	]+p.subrnr[ 	]+t6,t0,t3
[ 	]+8:[ 	]+405ffe5b[ 	]+p.subrnr[ 	]+t3,t6,t0
