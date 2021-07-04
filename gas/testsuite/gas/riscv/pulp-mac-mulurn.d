#as: -march=rv32i_xpulpmulrnhi
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+007342db[ 	]+p.mulurn[ 	]+t0,t1,t2,0
[ 	]+4:[ 	]+1653cedb[ 	]+p.mulurn[ 	]+t4,t2,t0,11
[ 	]+8:[ 	]+3e6f4e5b[ 	]+p.mulurn[ 	]+t3,t5,t1,31
