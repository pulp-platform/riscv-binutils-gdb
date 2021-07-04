#as: -march=rv32i_xpulpclip
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+147352b3[ 	]+p.clipr[ 	]+t0,t1,t2
[ 	]+4:[ 	]+15f3deb3[ 	]+p.clipr[ 	]+t4,t2,t6
[ 	]+8:[ 	]+146f5e33[ 	]+p.clipr[ 	]+t3,t5,t1
