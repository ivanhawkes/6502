	; To compile:
	;    vasm6502_oldstyle.exe 03-rotate-right.asm -o 03-rotate-right.bin -c02 -dotdir -Fbin

	.org $8000

PORTA = $6001
PORTB = $6000
DDRA = $6003
DDRB = $6002

reset:
	lda #%11111111			; Set all pins on port B to output.
	sta DDRB

	lda #%10101010			; Place a pattern in the accumulator.
	sta PORTB				; Write the pattern to port B.

loop:
	ror						; Rotate the pattern of bits.
	sta PORTB				; Write the pattern to port B.
	jmp loop

	.org #$fffc				; 6502 bootup reads this address and jumps to it.
	.word reset
	.word $0000
