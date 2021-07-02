#as: -march=rv32i_xpulpaddsubrn
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+01fe62db[ 	]+p.addrn[ 	]+t0,t3,t6,0
[ 	]+4:[ 	]+13c2efdb[ 	]+p.addrn[ 	]+t6,t0,t3,9
[ 	]+8:[ 	]+3e5fee5b[ 	]+p.addrn[ 	]+t3,t6,t0,31
