#as: -march=rv32i_xpulpmacrnhi
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+407352db[ 	]+p.machhurn[ 	]+t0,t1,t2,0
[ 	]+4:[ 	]+4a53dedb[ 	]+p.machhurn[ 	]+t4,t2,t0,5
[ 	]+8:[ 	]+7e6f5e5b[ 	]+p.machhurn[ 	]+t3,t5,t1,31
