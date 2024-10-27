
Schematic
=========

The QG1 is a 32MHz integrated oscillator. The value is
missing in the schematic.

When using only one PIC it could be good to
add a 100k pull-up/down resistor on pin 23 (SDI).



Programming the PICs
=====================

The config bits are not included in the HEX files.
You must set them manually in your programmer !

Here is the full config settings for IC-Prog 1.05C :

config1: 0010 0101 0000 0000 = 2500
config2: 0000 1110 0000 0111 = 0E07
config3: 0000 0001 0000 0000 = 0100
config4: 0000 0000 1000 0001 = 0081
config5: 1100 0000 0000 1111 = C00F
config6: 1110 0000 0000 1111 = E00F
config7: 0100 0000 0000 1111 = 400F

Optionnaly, you can modify the ASM files
and recompile them.
Add these lines, to set the config bits :
 __CONFIG _CONFIG1H, _OSCS_ON_1H & _ECIO_OSC_1H
 __CONFIG _CONFIG2L, _BOR_ON_2L & _PWRT_OFF_2L & _BORV_42_2L
 __CONFIG _CONFIG2H, _WDT_OFF_2H
 __CONFIG _CONFIG3H, _CCP2MX_ON_3H
 __CONFIG _CONFIG4L, _STVR_ON_4L & _LVP_OFF_4L & _DEBUG_OFF_4L
 __CONFIG _CONFIG5L, _CP0_OFF_5L & _CP1_OFF_5L & _CP2_OFF_5L & _CP3_OFF_5L
 __CONFIG _CONFIG5H, _CPB_OFF_5H & _CPD_OFF_5H
 __CONFIG _CONFIG6L, _WRT0_OFF_6L & _WRT1_OFF_6L & _WRT2_OFF_6L & _WRT3_OFF_6L
 __CONFIG _CONFIG6H, _WRTC_OFF_6H & _WRTB_OFF_6H & _WRTD_OFF_6H
 __CONFIG _CONFIG7L, _EBTR0_OFF_7L & _EBTR1_OFF_7L & _EBTR2_OFF_7L & _EBTR3_OFF_7L
 __CONFIG _CONFIG7H, _EBTRB_OFF_7H


PCB:
====

Due to an error in the routing, the Protel PCB is not anymore included in 
this archive.

The Eagle PCB is not done yet. If somebody has time to do it we will
be happy to include it here. We will not make it by ourself because
we don't plan to build more than two O'ksi'D DMX 3/1 interfaces.



