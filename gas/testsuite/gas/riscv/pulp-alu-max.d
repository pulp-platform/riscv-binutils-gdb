#as: -march=rv32i_xpulpminmax
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+047362b3[ 	]+p.max[ 	]+t0,t1,t2
[ 	]+4:[ 	]+05f3eeb3[ 	]+p.max[ 	]+t4,t2,t6
[ 	]+8:[ 	]+046f6e33[ 	]+p.max[ 	]+t3,t5,t1
