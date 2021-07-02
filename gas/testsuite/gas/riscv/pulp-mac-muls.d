#as: -march=rv32i_xpulpmulrnhi
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+807302db[ 	]+p.muls[ 	]+t0,t1,t2
[ 	]+4:[ 	]+80538edb[ 	]+p.muls[ 	]+t4,t2,t0
[ 	]+8:[ 	]+806f0e5b[ 	]+p.muls[ 	]+t3,t5,t1
