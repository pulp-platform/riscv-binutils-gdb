#as: -march=rv32i_xpulpminmax
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+047372b3[ 	]+p.maxu[ 	]+t0,t1,t2
[ 	]+4:[ 	]+05f3feb3[ 	]+p.maxu[ 	]+t4,t2,t6
[ 	]+8:[ 	]+046f7e33[ 	]+p.maxu[ 	]+t3,t5,t1
