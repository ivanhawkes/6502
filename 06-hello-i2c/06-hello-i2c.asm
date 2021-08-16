	; Communicate via I2C to the Arduino.

	; To compile:
	; vasm6502_oldstyle.exe 06-hello-i2c.asm -o 06-hello-i2c.bin -c02 -dotdir -Fbin

LCD_START_INSTRUCTION	= %10000000
LCD_READ_WRITE			= %01000000
LCD_READ_SELECT			= %00100000

VIA_BASE_ADDR			= $6000					; Base address of the 6522 VIA.
PORTB					= VIA_BASE_ADDR + 0 	; Addresses of the 16 registers in 6522.
PORTA					= VIA_BASE_ADDR + 1		; (We really only need the first four for this.)
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
	jmp printStrzExit					; Infinite loop.

message:
	.asciiz "Monitor v0.1 Ready"

i2cScratchByte:
	.byte 1

lcdWait:
	pha
	lda #%00000000 						; Port B is input.
	sta DDRB

lcdBusy:
	lda #LCD_READ_WRITE
	sta PORTA
	lda #(LCD_READ_WRITE | LCD_START_INSTRUCTION)
	sta PORTA
	lda PORTB
	and #%10000000
	bne lcdBusy

	lda #LCD_READ_WRITE
	sta PORTA
	lda #%11111111 						; Port B is output.
	sta DDRB
	pla
	rts

lcdInstruction:
	jsr lcdWait
	sta PORTB
	lda #0								; Clear RS/RW/E bits.
	sta PORTA
	lda #LCD_START_INSTRUCTION			; Set E bit to send instruction.
	sta PORTA
	lda #0								; Clear RS/RW/E bits.
	sta PORTA
	rts

printChar:
	jsr lcdWait
	sta PORTB
	lda #LCD_READ_SELECT				; Set RS, Clear RW/E bits.
	sta PORTA
	lda #(LCD_READ_SELECT | LCD_START_INSTRUCTION)	; Set E bit to send instruction.
	sta PORTA
	lda #LCD_READ_SELECT				; Clear E bits.
	sta PORTA
	rts


; ***
; *** I2C Routines.
; ***


I2C_DATA_UP: .macro
	LDA #10000000B 				; Two instructions here. Clear bit 7 of the DDR
	TRB DDRA 					; to make PA7 an input and let it float up.
	.endm

I2C_DATA_DN: .macro
	LDA #10000000B 				; Two instructions here. Set bit 7 of the DDR
	TSB DDRA 					; to make PA7 an output and pull it down since
	.endm 						; bit 7 of the output register is a 0.

I2C_CLK_UP: .macro 						; (as above)
	LDA #1
	TRB DDRA
	.endm

I2C_CLK_DN: .macro 						; (as above)
	LDA #1
	TSB DDRA
	.endm

I2C_START:
	I2C_DATA_UP
	I2C_CLK_UP
	I2C_DATA_DN
ist1:
	INC DDRA 					; Clk down. We now know the bit val, so just INC.
	TRB DDRA 					; Data up, using accum val left from I2C_DATA_DN above.
	RTS

I2C_STOP:
	I2C_DATA_DN
	I2C_CLK_UP
	I2C_DATA_UP
	BRA ist1

I2C_ACK:
	I2C_DATA_DN 				; Acknowledge. The ACK bit in I2C is the 9th bit of a "byte".
ia1:
	I2C_CLK_UP 					; Acknowledging consists of pulling it down.
	INC DDRA 					; Clk down. We know the bit val, so just INC.
	I2C_DATA_UP
	RTS

I2C_NAK:
	I2C_DATA_UP 				; Not acknowledge.
	BRA ia1

I2C_ACK_QUESTION:
	I2C_DATA_UP 				; At end, N=0 means ACK. N=1 means NAK.
	I2C_CLK_UP
	BIT PORTA 					; Bit 7 (the data line) gets put in the N flag.
	TSB DDRA 					; Clk down. Accum still has 1 from I2C_CLK_UP. Take advantage.
	RTS

I2C_PWR_ON:
	LDA #10000000B 				; Clear bit 7 of port B. It must first be made an output by doing INIT_I2C.
	TRB PORTB
	RTS

I2C_PWR_OFF: 					; (Basically the same as INIT_I2C below.)
INIT_I2C: 						; Set up the port bit directions and values. Leaves power off, clk & data low.
	LDA #10000000B
	TSB PORTB 					; Make PB7 put out a high level (I2C power off) when made an output,
	TSB DDRB 					; then make PB7 an output.

	INC A 						; Put 10000001B in A for data and clock lines on port A.
	TSB DDRA 					; Make PA0 and PA7 outputs to hold clock and data low while power is off,
	TRB PORTA 	 				; and make the output value to be 0 for the same.
	RTS 	 					; You might want to leave a delay to let the power die out so devices are really
								; cleared before turning it back on. Then you shouldn't need CLR_I2C below.

CLR_I2C: 						; This clears any unwanted transaction that might be in progress, by giving
	JSR I2C_STOP 				; enough clock pulses to finish a byte and not acknowledging it.
	JSR I2C_START
	I2C_DATA_UP 				; Keep data line released so we don't ACK any byte sent by a device.
	LDX #9 						; Loop 9x to send 9 clock pulses to finish any byte a device might send.
ci2c:
	DEC DDRA 					; Like I2C_CLK_UP since we know I2C_START left clock down (DDRA bit 0 high).
	INC DDRA 					; Like I2C_CLK_DN since we know the state from the above instruction.
	DEX
	BNE ci2c
	JSR I2C_START
	JMP I2C_STOP 				; (JSR, RTS)

SEND_I2C_BYTE: 					; Start with byte in A, and clock low. Ends with I2C_ACK_QUESTION.
	STA i2cScratchByte 			; Store the byte in a variable so we can use A with TSB & TRB for data line.
	LDA #10000000B 				; Init A for mask for TRB & TSB below. A does not get disturbed below.
	LDX #8 						; We will do 8 bits.
sIb2:
	TRB DDRA 					; Release data line. This is like I2C_DATA_UP but saves 1 instruction.
	ASL i2cScratchByte 			; Get next bit to send and put it in the C flag.
	BCS sIb1
	TSB DDRA 					; If the bit was 0, pull data line down by making it an output.
sIb1:
	DEC DDRA 					; Do a high pulse on the clock line. Remember there's a 0 in the output
	INC DDRA 					; register bit, and DEC'ing DDRA makes that bit an input, so it can float up.
	DEX 		 				; IOW, it's backwards from what it seems.
	BNE sIb2
	JMP I2C_ACK_QUESTION		; (JSR, RTS)


RCV_I2C_BYTE:					; Start with clock low. Ends with byte in i2cScratchByte. Do ACK bit separately.
	I2C_DATA_UP 				; Make sure we're not holding the data line down. Be ready to input data.
	LDX #8 						; We will do 8 bits. There's no need to init i2cScratchByte.
rIb1:
	DEC DDRA 					; Set clock line high.
	ASL i2cScratchByte 			; Get the forming byte's next bit position ready to accept the bit.
	BIT PORTA 					; Read the data line value into N flag.
	BPL rIb2 					; If the data line was high,
	INC i2cScratchByte 			; increment the 1's place to a 1 in the forming byte. (ASL made bit 0 = 0.)
rIb2:
	INC DDRA 					; Put clock line back low.
	DEX
	BNE rIb1 					; Go back for next bit if there is one.
	RTS

			.org $fffc
			.word main
			.word $0000
