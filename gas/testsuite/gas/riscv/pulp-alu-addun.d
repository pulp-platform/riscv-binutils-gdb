#as: -march=rv32i_xpulpaddsubrn
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+807322db[ 	]+p.addun[ 	]+t0,t1,t2,0
[ 	]+4:[ 	]+8853aedb[ 	]+p.addun[ 	]+t4,t2,t0,4
[ 	]+8:[ 	]+be6f2e5b[ 	]+p.addun[ 	]+t3,t5,t1,31
