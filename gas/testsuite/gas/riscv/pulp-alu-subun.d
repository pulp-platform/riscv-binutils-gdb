#as: -march=rv32i_xpulpaddsubrn
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+81fe32db[ 	]+p.subun[ 	]+t0,t3,t6,0
[ 	]+4:[ 	]+b1c2bfdb[ 	]+p.subun[ 	]+t6,t0,t3,24
[ 	]+8:[ 	]+be5fbe5b[ 	]+p.subun[ 	]+t3,t6,t0,31
