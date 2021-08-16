; Compile:
;	vasm6502_oldstyle.exe 02-pattern-test.asm -o 02-pattern-test.bin -c02 -dotdir -Fbin

	.org $8000

; Base address of the 6522 VIA.
VIA_BASE_ADDR			= $6000

; 6522 VIA ports and registers.
PORT_B					= VIA_BASE_ADDR + 0
PORT_A					= VIA_BASE_ADDR + 1
DDRB					= VIA_BASE_ADDR + 2
DDRA					= VIA_BASE_ADDR + 3

PATTERN_A				= %00000000		; All the pins low.
PATTERN_B				= %11111111		; All the pins high.
PATTERN_C				= %00001111		; Left half low, right half high.
PATTERN_D				= %11110000		; Left half high, right half low.
PATTERN_E				= %10101010		; Alternating.
PATTERN_F				= %01010101		; Alternating.
PATTERN_G				= %11001100		; Alternating in pairs.
PATTERN_H				= %00110011		; Alternating in pairs.


main:
	lda #%11111111				; Set all pins on port B to output.
	sta DDRB

	lda #%10101010				; Place a pattern in the accumulator.
	sta PORT_B

loop:
	lda #PATTERN_A
	sta PORT_B
	nop

	lda #PATTERN_B
	sta PORT_B
	nop

	lda #PATTERN_C
	sta PORT_B
	nop

	lda #PATTERN_D
	sta PORT_B
	nop

	lda #PATTERN_E
	sta PORT_B
	nop

	lda #PATTERN_F
	sta PORT_B
	nop

	lda #PATTERN_G
	sta PORT_B
	nop

	lda #PATTERN_H
	sta PORT_B
	nop

	lda #$01					; Prepare to walk the LEDS from lowest bit to highest bit.

counter:
	sta PORT_B
	nop
	asl
	bcc counter

	jmp loop

	.org #$fffc					; 6502 bootup reads this address and jumps to it.
	.word main
	.word $0000
