#as: -march=rv32i_xpulphwloop
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+0000307b[ 	]+lp.counti[ 	]+x0,0
[ 	]+4:[ 	]+791030fb[ 	]+lp.counti[ 	]+x1,1937
[ 	]+8:[ 	]+fff0307b[ 	]+lp.counti[ 	]+x0,4095
