#as: -march=rv32i_xcorev
#objdump: -d

.*:[ 	]+file format .*


Disassembly of section .text:

0+000 <target>:
[ 	]+0:[ 	]+04038eb3[ 	]+p.abs[ 	]+t4,t2
[ 	]+4:[ 	]+05f3aeb3[ 	]+p.slet[ 	]+t4,t2,t6
[ 	]+8:[ 	]+05f3beb3[ 	]+p.sletu[ 	]+t4,t2,t6
[ 	]+c:[ 	]+05f3ceb3[ 	]+p.min[ 	]+t4,t2,t6
[ 	]+10:[ 	]+05f3deb3[ 	]+p.minu[ 	]+t4,t2,t6
[ 	]+14:[ 	]+05f3eeb3[ 	]+p.max[ 	]+t4,t2,t6
[ 	]+18:[ 	]+05f3feb3[ 	]+p.maxu[ 	]+t4,t2,t6
[ 	]+1c:[ 	]+1003ceb3[ 	]+p.exths[ 	]+t4,t2
[ 	]+20:[ 	]+1003deb3[ 	]+p.exthz[ 	]+t4,t2
[ 	]+24:[ 	]+1003eeb3[ 	]+p.extbs[ 	]+t4,t2
[ 	]+28:[ 	]+1003feb3[ 	]+p.extbz[ 	]+t4,t2
[ 	]+2c:[ 	]+14539eb3[ 	]+p.clip[ 	]+t4,t2,5
[ 	]+30:[ 	]+1453aeb3[ 	]+p.clipu[ 	]+t4,t2,5
[ 	]+34:[ 	]+15f3deb3[ 	]+p.clipr[ 	]+t4,t2,t6
[ 	]+38:[ 	]+15f3eeb3[ 	]+p.clipur[ 	]+t4,t2,t6
[ 	]+3c:[ 	]+0853aedb[ 	]+p.addn[ 	]+t4,t2,t0,4
[ 	]+40:[ 	]+8853aedb[ 	]+p.addun[ 	]+t4,t2,t0,4
[ 	]+44:[ 	]+13c2efdb[ 	]+p.addrn[ 	]+t6,t0,t3,9
[ 	]+48:[ 	]+9dc2efdb[ 	]+p.addurn[ 	]+t6,t0,t3,14
[ 	]+4c:[ 	]+41c2afdb[ 	]+p.addnr[ 	]+t6,t0,t3
[ 	]+50:[ 	]+c1c2afdb[ 	]+p.addunr[ 	]+t6,t0,t3
[ 	]+54:[ 	]+41c2efdb[ 	]+p.addrnr[ 	]+t6,t0,t3
[ 	]+58:[ 	]+c1c2efdb[ 	]+p.addurnr[ 	]+t6,t0,t3
[ 	]+5c:[ 	]+0dc2bfdb[ 	]+p.subn[ 	]+t6,t0,t3,6
[ 	]+60:[ 	]+b1c2bfdb[ 	]+p.subun[ 	]+t6,t0,t3,24
[ 	]+64:[ 	]+2bc2ffdb[ 	]+p.subrn[ 	]+t6,t0,t3,21
[ 	]+68:[ 	]+87c2ffdb[ 	]+p.suburn[ 	]+t6,t0,t3,3
[ 	]+6c:[ 	]+41c2bfdb[ 	]+p.subnr[ 	]+t6,t0,t3
[ 	]+70:[ 	]+c1c2bfdb[ 	]+p.subunr[ 	]+t6,t0,t3
[ 	]+74:[ 	]+41c2ffdb[ 	]+p.subrnr[ 	]+t6,t0,t3
[ 	]+78:[ 	]+c1c2ffdb[ 	]+p.suburnr[ 	]+t6,t0,t3
