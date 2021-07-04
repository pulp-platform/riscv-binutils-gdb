#as: -march=rv32i_xpulphwloop
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+0a0000fb[ 	]+lp.starti[ 	]+x1,140 +<target\+0x140>
[ 	]+4:[ 	]+210010fb[ 	]+lp.endi[ 	]+x1,424 +<target\+0x424>
[ 	]+8:[ 	]+1e8350fb[ 	]+lp.setupi[ 	]+x1,488,14 <target\+0x14>
[ 	]+c:[ 	]+0f4f40fb[ 	]+lp.setup[ 	]+x1,t5,1f4 <target\+0x1f4>
[ 	]+10:[ 	]+0005a0fb[ 	]+lp.count[ 	]+x1,a1
[ 	]+14:[ 	]+791030fb[ 	]+lp.counti[ 	]+x1,1937
