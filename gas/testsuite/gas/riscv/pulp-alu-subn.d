#as: -march=rv32i_xpulpaddsubrn
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+01fe32db[ 	]+p.subn[ 	]+t0,t3,t6,0
[ 	]+4:[ 	]+0dc2bfdb[ 	]+p.subn[ 	]+t6,t0,t3,6
[ 	]+8:[ 	]+3e5fbe5b[ 	]+p.subn[ 	]+t3,t6,t0,31
