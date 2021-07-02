#as: -march=rv32i_xpulpaddsubrn
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+41fe22db[ 	]+p.addnr[ 	]+t0,t3,t6
[ 	]+4:[ 	]+41c2afdb[ 	]+p.addnr[ 	]+t6,t0,t3
[ 	]+8:[ 	]+405fae5b[ 	]+p.addnr[ 	]+t3,t6,t0
