; Compile:
;	vasm6502_oldstyle.exe 05-hello-world.asm -o 05-hello-world.bin -c02 -dotdir -Fbin
;
; Hello world:
;	Rotate a pattern on the data lines to the right and push it to the VIA chip.

; Base address of the 6522 VIA.
VIA_BASE_ADDR			= $6000

; 6522 VIA ports and registers.
PORT_B					= VIA_BASE_ADDR + 0
PORT_A					= VIA_BASE_ADDR + 1
DATA_DIR_B				= VIA_BASE_ADDR + 2
DATA_DIR_A				= VIA_BASE_ADDR + 3
T1CL					= VIA_BASE_ADDR + 4
T1CH					= VIA_BASE_ADDR + 5
T1LL					= VIA_BASE_ADDR + 6
T1LH					= VIA_BASE_ADDR + 7
T2CL					= VIA_BASE_ADDR + 8
T2CH					= VIA_BASE_ADDR + 9
SR					 	= VIA_BASE_ADDR + $A
ACR						= VIA_BASE_ADDR + $B
PCR						= VIA_BASE_ADDR + $C
IFR						= VIA_BASE_ADDR + $D
IER						= VIA_BASE_ADDR + $E
PANOHS					= VIA_BASE_ADDR + $F

; LCD 16x02 display.
LCD_START_INSTRUCTION	= %10000000
LCD_READ_WRITE			= %01000000
LCD_READ_SELECT			= %00100000


	.org $8000

main:
	; Reset the stack to the top.
	ldx #$ff
	txs

	; Set all pins on port B to output.
	lda #%11111111
	sta DATA_DIR_B

	; TODO: Need to switch which pins are being used for this since they conflict with the pins
	; I want to use for the I2C console.

	; Set top 3 pins on port A to output.
	lda #%11100000
	sta DATA_DIR_A

	; Set 8-bit mode, 2-line display, 5x8 font.
	lda #%00111000
	jsr lcdInstruction

	; Display on, cursor on, blink off.
	lda #%00001110
	jsr lcdInstruction

	; Increment and shift cursor, don't shift display.
	lda #%00000110
	jsr lcdInstruction

	; Clear display.
	lda #$00000001
	jsr lcdInstruction

	ldx #0

printStrz:
	lda message, x
	beq printStrzExit
	jsr printChar
	inx
	jmp printStrz

printStrzExit:
	jmp printStrzExit			; Infinite loop.

message:
	.asciiz "Monitor v0.1 Ready"

i2cScratchByte:
	.byte 1

lcdWait:
	pha
	lda #%00000000 				; Port B is input.
	sta DATA_DIR_B

lcdBusy:
	lda #LCD_READ_WRITE
	sta PORT_A
	lda #(LCD_READ_WRITE | LCD_START_INSTRUCTION)
	sta PORT_A
	lda PORT_B
	and #%10000000
	bne lcdBusy

	lda #LCD_READ_WRITE
	sta PORT_A
	lda #%11111111 				; Port B is output.
	sta DATA_DIR_B
	pla
	rts

lcdInstruction:
	jsr lcdWait
	sta PORT_B
	lda #0						; Clear RS / RW / E bits.
	sta PORT_A
	lda #LCD_START_INSTRUCTION	; Set E bit to send instruction.
	sta PORT_A
	lda #0						; Clear RS / RW / E bits.
	sta PORT_A
	rts

printChar:
	jsr lcdWait
	sta PORT_B
	lda #LCD_READ_SELECT		; Set RS, Clear RW / E bits.
	sta PORT_A
	lda #(LCD_READ_SELECT | LCD_START_INSTRUCTION)	; Set E bit to send instruction.
	sta PORT_A
	lda #LCD_READ_SELECT		; Clear E bits.
	sta PORT_A
	rts

	.org $fffc
	.word main
	.word $0000
