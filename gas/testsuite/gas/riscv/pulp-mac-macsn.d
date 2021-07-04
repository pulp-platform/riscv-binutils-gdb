#as: -march=rv32i_xpulpmacrnhi
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+807312db[ 	]+p.macsn[ 	]+t0,t1,t2,0
[ 	]+4:[ 	]+b0539edb[ 	]+p.macsn[ 	]+t4,t2,t0,24
[ 	]+8:[ 	]+be6f1e5b[ 	]+p.macsn[ 	]+t3,t5,t1,31
