#as: -march=rv32i_xpulpaddsubrn
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+c1fe22db[ 	]+p.addunr[ 	]+t0,t3,t6
[ 	]+4:[ 	]+c1c2afdb[ 	]+p.addunr[ 	]+t6,t0,t3
[ 	]+8:[ 	]+c05fae5b[ 	]+p.addunr[ 	]+t3,t6,t0
