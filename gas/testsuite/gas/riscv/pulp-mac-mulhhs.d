#as: -march=rv32i_xpulpmulrnhi
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+c07302db[ 	]+p.mulhhs[ 	]+t0,t1,t2
[ 	]+4:[ 	]+c0538edb[ 	]+p.mulhhs[ 	]+t4,t2,t0
[ 	]+8:[ 	]+c06f0e5b[ 	]+p.mulhhs[ 	]+t3,t5,t1
