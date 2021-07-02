#as: -march=rv32i_xpulpclip
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+140312b3[ 	]+p.clip[ 	]+t0,t1,0
[ 	]+4:[ 	]+14539eb3[ 	]+p.clip[ 	]+t4,t2,5
[ 	]+8:[ 	]+15ff1e33[ 	]+p.clip[ 	]+t3,t5,31
