#as: -march=rv32i_xpulphwloop
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+0000007b[ 	]+lp.starti[ 	]+x0,0 <target>
[ 	]+4:[ 	]+5e6000fb[ 	]+lp.starti[ 	]+x1,bd0 +<target\+0xbd0>
[ 	]+8:[ 	]+7ff0007b[ 	]+lp.starti[ 	]+x0,1006 +<target\+0x1006>
