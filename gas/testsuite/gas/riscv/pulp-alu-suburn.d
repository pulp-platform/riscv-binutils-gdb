#as: -march=rv32i_xpulpaddsubrn
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+81fe72db[ 	]+p.suburn[ 	]+t0,t3,t6,0
[ 	]+4:[ 	]+87c2ffdb[ 	]+p.suburn[ 	]+t6,t0,t3,3
[ 	]+8:[ 	]+be5ffe5b[ 	]+p.suburn[ 	]+t3,t6,t0,31
