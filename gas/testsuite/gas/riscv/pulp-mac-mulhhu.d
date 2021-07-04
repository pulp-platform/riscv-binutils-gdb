#as: -march=rv32i_xpulpmulrnhi
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+407302db[ 	]+p.mulhhu[ 	]+t0,t1,t2
[ 	]+4:[ 	]+40538edb[ 	]+p.mulhhu[ 	]+t4,t2,t0
[ 	]+8:[ 	]+406f0e5b[ 	]+p.mulhhu[ 	]+t3,t5,t1
