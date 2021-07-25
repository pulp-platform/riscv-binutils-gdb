#as: -march=rv32i_zdinx
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+02c5f553[ 	]+fadd.d[ 	]+a0,a1,a2
[ 	]+4:[ 	]+0ac5f553[ 	]+fsub.d[ 	]+a0,a1,a2
[ 	]+8:[ 	]+12c5f553[ 	]+fmul.d[ 	]+a0,a1,a2
[ 	]+c:[ 	]+1ac5f553[ 	]+fdiv.d[ 	]+a0,a1,a2
