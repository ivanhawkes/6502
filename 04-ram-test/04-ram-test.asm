	; Ram Test:
	;	Place some patterns on the data pins and write them to the lower memory locations where the RAM
	;	should be addressable. Exercise the stack.
	;
	; To compile:
	;    vasm6502_oldstyle.exe 04-ram-test.asm -o 04-ram-test.bin -c02 -dotdir -Fbin


	.org $8000

PORTA		= $6001
PORTB		= $6000
DDRA		= $6003
DDRB		= $6002
RAMTEST		= $4000
PATTERNA	= %00000000		; All the pins low.
PATTERNB	= %11111111		; All the pins high.
PATTERNC	= %00001111		; Left half low, right half high.


reset:
	lda #%11111111			; Set all pins on port B to output.
	sta DDRB

	lda #%10101010			; Place a pattern in the accumulator.
	sta PORTB				; Write the pattern to port B.

loop:
	lda #PATTERNA			; Write a couple of patterns to the pins to show we're running.
	sta PORTB				

	lda #PATTERNB
	sta PORTB				

	lda #$01				; Start with the lowest bit.
	ldx #$00				; Index for offset on storage.

counter:
	sta PORTB				; Write the pattern to port B.
	sta RAMTEST, x			; Try and write that value.

	pha						; Save current value on the stack.

	eor #$ff				; XOR so the value we push is different to the one we load back from RAM.
	sta PORTB				
	
	lda RAMTEST, x			; Try and load from RAM.
	sta PORTB				

	pla						; Get our accumulator back, we need it for the loop.

	inx						; Step through a range of memory locations.
	asl						; Step through powers of two.
	bcc counter				; Keep doing it till we cause the carry to trigger.

	jmp loop

	.org #$fffc				; 6502 bootup reads this address and jumps to it.
	.word reset
	.word $0000
