#as: -march=rv32i_xpulpaddsubrn
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+81fe62db[ 	]+p.addurn[ 	]+t0,t3,t6,0
[ 	]+4:[ 	]+9dc2efdb[ 	]+p.addurn[ 	]+t6,t0,t3,14
[ 	]+8:[ 	]+be5fee5b[ 	]+p.addurn[ 	]+t3,t6,t0,31
