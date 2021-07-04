#source: pulp-fmv.s
#as: -march=rv32ixfhalf_xfalthalf_xfquarter
#objdump: -dr

.*:[ 	]+file format .*


Disassembly of section .text:

0+00 <.text>:
[ 	]+0:[ 	]+e40b8653[ 	]+fmv.x.h[ 	]+a2,fs7
[ 	]+4:[ 	]+f40800d3[ 	]+fmv.h.x[ 	]+ft1,a6
[ 	]+8:[ 	]+e40bc653[ 	]+fmv.x.ah[ 	]+a2,fs7
[ 	]+c:[ 	]+f40840d3[ 	]+fmv.ah.x[ 	]+ft1,a6
[ 	]+10:[ 	]+e60b8653[ 	]+fmv.x.b[ 	]+a2,fs7
[ 	]+14:[ 	]+f60800d3[ 	]+fmv.b.x[ 	]+ft1,a6
