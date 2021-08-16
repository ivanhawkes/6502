; Compile:
;	vasm6502_oldstyle.exe 04-ram-test.asm -o 04-ram-test.bin -c02 -dotdir -Fbin
;
; Ram Test:
;	Place some patterns on the data pins and write them to the lower memory locations where the RAM
;	should be addressable. Exercise the stack.

	.org $8000

; Base address of the 6522 VIA.
VIA_BASE_ADDR			= $6000

; 6522 VIA ports and registers.
PORT_B					= VIA_BASE_ADDR + 0
PORT_A					= VIA_BASE_ADDR + 1
DDRB					= VIA_BASE_ADDR + 2
DDRA					= VIA_BASE_ADDR + 3

RAM_TEST				= $1000
PATTERN_A				= %00000000		; All the pins low.
PATTERN_B				= %11111111		; All the pins high.
PATTERN_C				= %00001111		; Left half low, right half high.
PATTERN_D				= %11110000		; Left half high, right half low.


main:
	lda #%11111111				; Set all pins on port B to output.
	sta DDRB

	lda #%10101010				; Place a pattern in the accumulator.
	sta PORT_B					; Write the pattern to port B.

loop:
	lda #PATTERN_A				; Write a couple of patterns to the pins to show we're running.
	sta PORT_B				

	lda #PATTERN_B
	sta PORT_B				

	lda #$01					; Start with the lowest bit.
	ldx #$00					; Index for offset on storage.

counter:
	sta PORT_B					; Write the pattern to port B.
	sta RAM_TEST, x				; Try and write that value.

	pha							; Save current value on the stack.

	lda #PATTERN_C				; Flash some LEDS for debug.
	sta PORT_B
	
	lda RAM_TEST, x				; Try and load from RAM.
	sta PORT_B				

	lda #PATTERN_D				; Flash some LEDS for debug.
	sta PORT_B

	pla							; Get our accumulator back, we need it for the loop.
	sta PORT_B							

	inx							; Step through a range of memory locations.
	asl							; Step through powers of two.
	bcc counter					; Keep doing it till we cause the carry to trigger.

	jmp loop

	.org #$fffc					; 6502 bootup reads this address and jumps to it.
	.word main
	.word $0000
