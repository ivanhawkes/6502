; Compile:
;	vasm6502_oldstyle.exe 06-hello-i2c.asm -o 06-hello-i2c.bin -c02 -dotdir -Fbin
;
; Hello World and I2C:
; 	Communicate via I2C to the Arduino. Maintain ability to control the LCD display.

; Base address of the 6522 VIA.
VIA_BASE_ADDR			= $6000

; 6522 VIA ports and registers.
PORT_B					= VIA_BASE_ADDR + 0
PORT_A					= VIA_BASE_ADDR + 1
DDRB					= VIA_BASE_ADDR + 2
DDRA					= VIA_BASE_ADDR + 3
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
	ldx #$ff
	txs

	; Set all pins on port B to output.
	lda #%11111111
	sta DDRB

	; TODO: Need to switch which pins are being used for this since they conflict with the pins
	; I want to use for the I2C console.

	; Set top 3 pins on port A to output.
	lda #%11100000
	sta DDRA

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
	sta DDRB

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
	sta DDRB
	pla
	rts

lcdInstruction:
	jsr lcdWait
	sta PORT_B
	lda #0						; Clear RS/RW/E bits.
	sta PORT_A
	lda #LCD_START_INSTRUCTION	; Set E bit to send instruction.
	sta PORT_A
	lda #0						; Clear RS/RW/E bits.
	sta PORT_A
	rts

printChar:
	jsr lcdWait
	sta PORT_B
	lda #LCD_READ_SELECT		; Set RS, Clear RW/E bits.
	sta PORT_A
	lda #(LCD_READ_SELECT | LCD_START_INSTRUCTION)	; Set E bit to send instruction.
	sta PORT_A
	lda #LCD_READ_SELECT		; Clear E bits.
	sta PORT_A
	rts


; ***
; *** I2C Routines.
; ***


I2C_DATA_UP:		.macro
	lda #10000000B 				; Two instructions here. Clear bit 7 of the DDR
	trb DDRA 					; to make PA7 an input and let it float up.
	.endm

I2C_DATA_DN:		.macro
	lda #10000000B 				; Two instructions here. Set bit 7 of the DDR
	tsb DDRA 					; to make PA7 an output and pull it down since
	.endm 						; bit 7 of the output register is a 0.

I2C_CLK_UP:			.macro 		; (as above)
	lda #1
	trb DDRA
	.endm

I2C_CLK_DN:			.macro 		; (as above)
	lda #1
	tsb DDRA
	.endm

i2cStart:
	I2C_DATA_UP
	I2C_CLK_UP
	I2C_DATA_DN

i2cStartExit:
	inc DDRA 					; Clk down. We now know the bit val, so just inc.
	trb DDRA 					; Data up, using accum val left from I2C_DATA_DN above.
	rts

i2cStop:
	I2C_DATA_DN
	I2C_CLK_UP
	I2C_DATA_UP
	bra i2cStartExit

i2cAck:
	I2C_DATA_DN 				; Acknowledge. The ACK bit in I2C is the 9th bit of a "byte".

i2cAckExit:
	I2C_CLK_UP 					; Acknowledging consists of pulling it down.
	inc DDRA 					; Clk down. We know the bit val, so just inc.
	I2C_DATA_UP
	rts

i2cNAck:
	I2C_DATA_UP 				; Not acknowledge.
	bra i2cAckExit

i2cAckQuestion:
	I2C_DATA_UP 				; At end, N=0 means ACK. N=1 means NAK.
	I2C_CLK_UP
	bit PORT_A 					; Bit 7 (the data line) gets put in the N flag.
	tsb DDRA 					; Clk down. Accum still has 1 from I2C_CLK_UP. Take advantage.
	rts

i2cPowerOn:
	lda #10000000B 				; Clear bit 7 of port B. It must first be made an output by doing i2cInit.
	trb PORT_B
	rts

i2cPowerOff: 					; (Basically the same as i2cInit below.)
i2cInit: 						; Set up the port bit directions and values. Leaves power off, clk & data low.
	lda #10000000B
	tsb PORT_B 					; Make PB7 put out a high level (I2C power off) when made an output,
	tsb DDRB 					; then make PB7 an output.

	inc A 						; Put 10000001B in A for data and clock lines on port A.
	tsb DDRA 					; Make PA0 and PA7 outputs to hold clock and data low while power is off,
	trb PORT_A 	 				; and make the output value to be 0 for the same.
	rts 	 					; You might want to leave a delay to let the power die out so devices are really
								; cleared before turning it back on. Then you shouldn't need i2cClear below.

i2cClear: 						; This clears any unwanted transaction that might be in progress, by giving
	jsr i2cStop 				; enough clock pulses to finish a byte and not acknowledging it.
	jsr i2cStart
	I2C_DATA_UP 				; Keep data line released so we don't ACK any byte sent by a device.
	ldx #9 						; Loop 9x to send 9 clock pulses to finish any byte a device might send.

i2cClearLoop:
	dec DDRA 					; Like I2C_CLK_UP since we know i2cStart left clock down (DDRA bit 0 high).
	inc DDRA 					; Like I2C_CLK_DN since we know the state from the above instruction.
	dex
	bne i2cClearLoop
	jsr i2cStart
	jmp i2cStop 				; (jsr, rts)

i2cSendByte: 					; Start with byte in A, and clock low. Ends with i2cAckQuestion.
	sta i2cScratchByte 			; Store the byte in a variable so we can use A with tsb & trb for data line.
	lda #10000000B 				; Init A for mask for trb & tsb below. A does not get disturbed below.
	ldx #8 						; We will do 8 bits.

i2cSB1:
	trb DDRA 					; Release data line. This is like I2C_DATA_UP but saves 1 instruction.
	asl i2cScratchByte 			; Get next bit to send and put it in the C flag.
	bcs i2cSB2
	tsb DDRA 					; If the bit was 0, pull data line down by making it an output.

i2cSB2:
	dec DDRA 					; Do a high pulse on the clock line. Remember there's a 0 in the output
	inc DDRA 					; register bit, and dec'ing DDRA makes that bit an input, so it can float up.
	dex 		 				; IOW, it's backwards from what it seems.
	bne i2cSB1
	jmp i2cAckQuestion			; (jsr, rts)

i2cReceiveByte:					; Start with clock low. Ends with byte in i2cScratchByte. Do ACK bit separately.
	I2C_DATA_UP 				; Make sure we're not holding the data line down. Be ready to input data.
	ldx #8 						; We will do 8 bits. There's no need to init i2cScratchByte.

i2cRB1:
	dec DDRA 					; Set clock line high.
	asl i2cScratchByte 			; Get the forming byte's next bit position ready to accept the bit.
	bit PORT_A 					; Read the data line value into N flag.
	bpl i2cRB2 					; If the data line was high,
	inc i2cScratchByte 			; increment the 1's place to a 1 in the forming byte. (asl made bit 0 = 0.)

i2cRB2:
	inc DDRA 					; Put clock line back low.
	dex
	bne i2cRB1 					; Go back for next bit if there is one.
	rts

	.org $fffc
	.word main
	.word $0000
