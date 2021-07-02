#as: -march=rv32i_xpulpmulrnhi
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+007302db[ 	]+p.mulu[ 	]+t0,t1,t2
[ 	]+4:[ 	]+0e538edb[ 	]+p.mulun[ 	]+t4,t2,t0,7
[ 	]+8:[ 	]+3e6f0e5b[ 	]+p.mulun[ 	]+t3,t5,t1,31
