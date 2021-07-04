#as: -march=rv32i_xpulpmacsi
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+427302b3[ 	]+p.mac[ 	]+t0,t1,t2
[ 	]+4:[ 	]+42538eb3[ 	]+p.mac[ 	]+t4,t2,t0
[ 	]+8:[ 	]+426f0e33[ 	]+p.mac[ 	]+t3,t5,t1
