#source: pulp-fvecalthalf.s
#as: -march=rv32ixfvecalthalf
#objdump: -dr

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <.text>:
[ 	]+0:[ 	]+823110b3[ 	]+vfadd.ah[ 	]+ft1,ft2,ft3
[ 	]+4:[ 	]+a6419433[ 	]+vfge.ah[ 	]+s0,ft3,ft4
