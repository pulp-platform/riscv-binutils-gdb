#as: -march=rv32i_zfinx
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+00c5f553[ 	]+fadd.s[ 	]+a0,a1,a2
[ 	]+4:[ 	]+08c5f553[ 	]+fsub.s[ 	]+a0,a1,a2
[ 	]+8:[ 	]+10c5f553[ 	]+fmul.s[ 	]+a0,a1,a2
[ 	]+c:[ 	]+18c5f553[ 	]+fdiv.s[ 	]+a0,a1,a2
