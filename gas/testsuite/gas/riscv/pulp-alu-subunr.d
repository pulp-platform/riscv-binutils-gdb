#as: -march=rv32i_xpulpaddsubrn
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+c1fe32db[ 	]+p.subunr[ 	]+t0,t3,t6
[ 	]+4:[ 	]+c1c2bfdb[ 	]+p.subunr[ 	]+t6,t0,t3
[ 	]+8:[ 	]+c05fbe5b[ 	]+p.subunr[ 	]+t3,t6,t0
