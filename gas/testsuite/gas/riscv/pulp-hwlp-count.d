#as: -march=rv32i_xpulphwloop
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+0002a07b[ 	]+lp.count[ 	]+x0,t0
[ 	]+4:[ 	]+0005a0fb[ 	]+lp.count[ 	]+x1,a1
[ 	]+8:[ 	]+000ea07b[ 	]+lp.count[ 	]+x0,t4
