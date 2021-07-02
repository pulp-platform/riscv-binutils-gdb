#as: -march=rv32i_xpulphwloop
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+0000507b[ 	]+lp.setupi[ 	]+x0,0,0 <target>
[ 	]+4:[ 	]+1e8350fb[ 	]+lp.setupi[ 	]+x1,488,10 <target\+0x10>
[ 	]+8:[ 	]+fff7d07b[ 	]+lp.setupi[ 	]+x0,4095,26 <target\+0x26>
