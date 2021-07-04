#as: -march=rv32i_xpulpmulrnhi
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+c07302db[ 	]+p.mulhhs[ 	]+t0,t1,t2
[ 	]+4:[ 	]+e0538edb[ 	]+p.mulhhsn[ 	]+t4,t2,t0,16
[ 	]+8:[ 	]+fe6f0e5b[ 	]+p.mulhhsn[ 	]+t3,t5,t1,31
