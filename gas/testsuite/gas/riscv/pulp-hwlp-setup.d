#as: -march=rv32i_xpulphwloop
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+0002c07b[ 	]+lp.setup[ 	]+x0,t0,0 <target>
[ 	]+4:[ 	]+0f4f40fb[ 	]+lp.setup[ 	]+x1,t5,1ec <target\+0x1ec>
[ 	]+8:[ 	]+7ff5407b[ 	]+lp.setup[ 	]+x0,a0,1006 <target\+0x1006>
