#as: -march=rv32i_xpulpmulrnhi
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+407342db[ 	]+p.mulhhurn[ 	]+t0,t1,t2,0
[ 	]+4:[ 	]+5253cedb[ 	]+p.mulhhurn[ 	]+t4,t2,t0,9
[ 	]+8:[ 	]+7e6f4e5b[ 	]+p.mulhhurn[ 	]+t3,t5,t1,31
