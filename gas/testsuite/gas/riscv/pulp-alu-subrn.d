#as: -march=rv32i_xpulpaddsubrn
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+01fe72db[ 	]+p.subrn[ 	]+t0,t3,t6,0
[ 	]+4:[ 	]+2bc2ffdb[ 	]+p.subrn[ 	]+t6,t0,t3,21
[ 	]+8:[ 	]+3e5ffe5b[ 	]+p.subrn[ 	]+t3,t6,t0,31
