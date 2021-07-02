#as: -march=rv32i_xcorev
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+42538eb3[ 	]+p.mac[ 	]+t4,t2,t0
[ 	]+4:[ 	]+42539eb3[ 	]+p.msu[ 	]+t4,t2,t0
[ 	]+8:[ 	]+80538edb[ 	]+p.muls[ 	]+t4,t2,t0
[ 	]+c:[ 	]+c0538edb[ 	]+p.mulhhs[ 	]+t4,t2,t0
[ 	]+10:[ 	]+88538edb[ 	]+p.mulsn[ 	]+t4,t2,t0,4
[ 	]+14:[ 	]+e0538edb[ 	]+p.mulhhsn[ 	]+t4,t2,t0,16
[ 	]+18:[ 	]+9453cedb[ 	]+p.mulsrn[ 	]+t4,t2,t0,10
[ 	]+1c:[ 	]+e253cedb[ 	]+p.mulhhsrn[ 	]+t4,t2,t0,17
[ 	]+20:[ 	]+00538edb[ 	]+p.mulu[ 	]+t4,t2,t0
[ 	]+24:[ 	]+40538edb[ 	]+p.mulhhu[ 	]+t4,t2,t0
[ 	]+28:[ 	]+0e538edb[ 	]+p.mulun[ 	]+t4,t2,t0,7
[ 	]+2c:[ 	]+60538edb[ 	]+p.mulhhun[ 	]+t4,t2,t0,16
[ 	]+30:[ 	]+1653cedb[ 	]+p.mulurn[ 	]+t4,t2,t0,11
[ 	]+34:[ 	]+5253cedb[ 	]+p.mulhhurn[ 	]+t4,t2,t0,9
[ 	]+38:[ 	]+b0539edb[ 	]+p.macsn[ 	]+t4,t2,t0,24
[ 	]+3c:[ 	]+d6539edb[ 	]+p.machhsn[ 	]+t4,t2,t0,11
[ 	]+40:[ 	]+9253dedb[ 	]+p.macsrn[ 	]+t4,t2,t0,9
[ 	]+44:[ 	]+f053dedb[ 	]+p.machhsrn[ 	]+t4,t2,t0,24
[ 	]+48:[ 	]+36539edb[ 	]+p.macun[ 	]+t4,t2,t0,27
[ 	]+4c:[ 	]+64539edb[ 	]+p.machhun[ 	]+t4,t2,t0,18
[ 	]+50:[ 	]+3253dedb[ 	]+p.macurn[ 	]+t4,t2,t0,25
[ 	]+54:[ 	]+4a53dedb[ 	]+p.machhurn[ 	]+t4,t2,t0,5
