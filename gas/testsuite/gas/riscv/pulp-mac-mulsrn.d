#as: -march=rv32i_xpulpmulrnhi
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+807342db[ 	]+p.mulsrn[ 	]+t0,t1,t2,0
[ 	]+4:[ 	]+9453cedb[ 	]+p.mulsrn[ 	]+t4,t2,t0,10
[ 	]+8:[ 	]+be6f4e5b[ 	]+p.mulsrn[ 	]+t3,t5,t1,31
