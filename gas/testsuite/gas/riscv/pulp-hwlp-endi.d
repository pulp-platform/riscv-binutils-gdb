#as: -march=rv32i_xpulphwloop
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+0000107b[ 	]+lp.endi[ 	]+x0,0 <target>
[ 	]+4:[ 	]+210010fb[ 	]+lp.endi[ 	]+x1,424 +<target\+0x424>
[ 	]+8:[ 	]+7ff0107b[ 	]+lp.endi[ 	]+x0,1006 +<target\+0x1006>
