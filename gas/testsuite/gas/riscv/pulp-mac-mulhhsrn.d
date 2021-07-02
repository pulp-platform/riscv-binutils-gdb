#as: -march=rv32i_xpulpmulrnhi
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+c07342db[ 	]+p.mulhhsrn[ 	]+t0,t1,t2,0
[ 	]+4:[ 	]+e253cedb[ 	]+p.mulhhsrn[ 	]+t4,t2,t0,17
[ 	]+8:[ 	]+fe6f4e5b[ 	]+p.mulhhsrn[ 	]+t3,t5,t1,31
