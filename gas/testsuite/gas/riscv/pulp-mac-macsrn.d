#as: -march=rv32i_xpulpmacrnhi
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+807352db[ 	]+p.macsrn[ 	]+t0,t1,t2,0
[ 	]+4:[ 	]+9253dedb[ 	]+p.macsrn[ 	]+t4,t2,t0,9
[ 	]+8:[ 	]+be6f5e5b[ 	]+p.macsrn[ 	]+t3,t5,t1,31
