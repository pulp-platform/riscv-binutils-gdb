#as: -march=rv32i_xpulpmulrnhi
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+007302db[ 	]+p.mulu[ 	]+t0,t1,t2
[ 	]+4:[ 	]+00538edb[ 	]+p.mulu[ 	]+t4,t2,t0
[ 	]+8:[ 	]+006f0e5b[ 	]+p.mulu[ 	]+t3,t5,t1
