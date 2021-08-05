	; To compile:
	;    vasm6502_oldstyle.exe 02-pattern-test.asm -o 02-pattern-test.bin -c02 -dotdir -Fbin

	.org $8000

PORTA		= $6001
PORTB		= $6000
DDRA		= $6003
DDRB		= $6002

PATTERNA	= %00000000		; All the pins low.
PATTERNB	= %11111111		; All the pins high.
PATTERNC	= %00001111		; Left half low, right half high.
PATTERND	= %11110000		; Left half high, right half low.
PATTERNE	= %10101010		; Alternating.
PATTERNF	= %01010101		; Alternating.
PATTERNG	= %11001100		; Alternating in pairs.
PATTERNH	= %00110011		; Alternating in pairs.

reset:
	lda #%11111111			; Set all pins on port B to output.
	sta DDRB

	lda #%10101010			; Place a pattern in the accumulator.
	sta PORTB				; Write the pattern to port B.

loop:
	lda #PATTERNA
	sta PORTB				; Write the pattern to port B.
	nop

	lda #PATTERNB
	sta PORTB				; Write the pattern to port B.
	nop

	lda #PATTERNC
	sta PORTB				; Write the pattern to port B.
	nop

	lda #PATTERND
	sta PORTB				; Write the pattern to port B.
	nop

	lda #PATTERNE
	sta PORTB				; Write the pattern to port B.
	nop

	lda #PATTERNF
	sta PORTB				; Write the pattern to port B.
	nop

	lda #PATTERNG
	sta PORTB				; Write the pattern to port B.
	nop

	lda #PATTERNH
	sta PORTB				; Write the pattern to port B.
	nop

	lda #$01				; Prepare to walk the LEDS from lowest bit to highest bit.

counter:
	sta PORTB				; Write the pattern to port B.
	nop
	asl
	bcc counter

	jmp loop

	.org #$fffc				; 6502 bootup reads this address and jumps to it.
	.word reset
	.word $0000
