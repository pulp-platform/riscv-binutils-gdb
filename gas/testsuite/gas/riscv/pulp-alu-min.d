#as: -march=rv32i_xpulpminmax
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+047342b3[ 	]+p.min[ 	]+t0,t1,t2
[ 	]+4:[ 	]+05f3ceb3[ 	]+p.min[ 	]+t4,t2,t6
[ 	]+8:[ 	]+046f4e33[ 	]+p.min[ 	]+t3,t5,t1
