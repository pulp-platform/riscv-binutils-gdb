#as: -march=rv32i_xpulpaddsubrn
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+41fe32db[ 	]+p.subnr[ 	]+t0,t3,t6
[ 	]+4:[ 	]+41c2bfdb[ 	]+p.subnr[ 	]+t6,t0,t3
[ 	]+8:[ 	]+405fbe5b[ 	]+p.subnr[ 	]+t3,t6,t0
