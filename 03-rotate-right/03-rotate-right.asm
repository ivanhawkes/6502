; Compile:
;	vasm6502_oldstyle.exe 03-rotate-right.asm -o 03-rotate-right.bin -c02 -dotdir -Fbin

	.org $8000

; Base address of the 6522 VIA.
VIA_BASE_ADDR			= $6000

; 6522 VIA ports and registers.
PORT_B					= VIA_BASE_ADDR + 0
PORT_A					= VIA_BASE_ADDR + 1
DDRB					= VIA_BASE_ADDR + 2
DDRA					= VIA_BASE_ADDR + 3


main:
	lda #%11111111				; Set all pins on port B to output.
	sta DDRB

	lda #%10101010				; Place a pattern in the accumulator.
	sta PORT_B					; Write the pattern to port B.
	clc							; Make sure the carry is in a known state.

loop:
	ror							; Rotate the pattern of bits.
	sta PORT_B					; Write the pattern to port B.
	jmp loop

	.org #$fffc					; 6502 bootup reads this address and jumps to it.
	.word main
	.word $0000
