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

; TODO: We don't appear to have a unique value for the I2C interface. It needs to be between 1 and 127.

; LCD 16x02 display.
LCD_START_INSTRUCTION	= %01000000
LCD_READ_WRITE			= %00100000
LCD_READ_SELECT			= %00010000
LCD_CTRL_PINS_OUTPUT	= (LCD_START_INSTRUCTION | LCD_READ_WRITE | LCD_READ_SELECT)

; Give our device an unique ID for I2C.
I2C_DEVICE_ID			= %01000011
MONITOR_DEVICE_ID		= %01000010

BIT_I2C_DATA_LINE		= %10000000
BIT_I2C_CLOCK_LINE		= %00000001
BIT_I2C_BOTH_LINES		= (BIT_I2C_DATA_LINE | BIT_I2C_CLOCK_LINE)

	.org $8000
	jmp main

;
; I2C Routines.
;
; NOTES: The code relies on the fact the output pins are set to zero, which will
; pull them down when writing, and allow them to float up when reading. In the
; I2C protocol you are only allowed to drive the lines low.

; Allow the data line to float to a high state.

I2C_DATA_HIGH:				.macro
	lda #BIT_I2C_DATA_LINE
	trb DATA_DIR_A
	.endm

; Drive the data line to a low state.
;   Pre-condition: The PORT_A pins are already set to a low state.

I2C_DATA_LOW:				.macro
	lda #BIT_I2C_DATA_LINE
	tsb DATA_DIR_A
	.endm

; Allow the clock line to float to a high state.

I2C_CLOCK_HIGH:				.macro
	lda #BIT_I2C_CLOCK_LINE
	trb DATA_DIR_A
	.endm

; Drive the clock line to a low state.
;   Pre-condition: The PORT_A pins are already set to a low state.

I2C_CLOCK_LOW:				.macro
	lda #BIT_I2C_CLOCK_LINE
	tsb DATA_DIR_A
	.endm

; Take advantage of known state to pulse the clock high then low in as few
; cycles as possible.
;
; Remember there's a 0 in the output port, and decrementing DDRA makes that
; bit an input, so it can float up. It's opposite world.

I2C_CLOCK_PULSE				.macro
	dec DATA_DIR_A
	inc DATA_DIR_A
	.endm


i2cStart:
	I2C_DATA_HIGH
	I2C_CLOCK_HIGH
	I2C_DATA_LOW
i2cStartExit:
	inc DATA_DIR_A 						; Clk down. We now know the bit val, so just inc.
	trb DATA_DIR_A 						; Data up, using accum val left from I2C_DATA_LOW above.
	rts


i2cStop:
	I2C_DATA_LOW
	I2C_CLOCK_HIGH
	I2C_DATA_HIGH
	bra i2cStartExit


i2cAck:
	I2C_DATA_LOW 					; Acknowledge. The ACK bit in I2C is the 9th bit of a "byte".
i2cAckExit:
	I2C_CLOCK_HIGH 					; Acknowledging consists of pulling it down.
	inc DATA_DIR_A 						; Clk down. We know the bit val, so just inc.
	I2C_DATA_HIGH
	rts


i2cNAck:
	I2C_DATA_HIGH 					; Not acknowledge.
	bra i2cAckExit


i2cAckQuestion:
	I2C_DATA_HIGH 					; At end, N=0 means ACK. N=1 means NAK.
	I2C_CLOCK_HIGH
	bit PORT_A 							; Bit 7 (the data line) gets put in the N flag.
	tsb DATA_DIR_A 						; Clk down. Accum still has 1 from I2C_CLOCK_HIGH. Take advantage.
	rts


i2cPowerOn:
	rts


i2cPowerOff: 							; (Basically the same as i2cInit below.)
i2cInit: 								; Set up the port bit directions and values. Leaves power off, clk & data low.
	lda #BIT_I2C_BOTH_LINES
	tsb DATA_DIR_A 						; Make PA0 and PA7 outputs to hold clock and data low while power is off,
	trb PORT_A 	 						; and make the output value to be 0 for the same.
	rts 	 							; You might want to leave a delay to let the power die out so devices are really
										; cleared before turning it back on. Then you shouldn't need i2cClear below.

i2cClear: 								; This clears any unwanted transaction that might be in progress, by giving
	jsr i2cStop 						; enough clock pulses to finish a byte and not acknowledging it.
	jsr i2cStart
	I2C_DATA_HIGH 					; Keep data line released so we don't ACK any byte sent by a device.
	ldx #9 								; Loop 9x to send 9 clock pulses to finish any byte a device might send.


i2cClearLoop:
	dec DATA_DIR_A 						; Like I2C_CLOCK_HIGH since we know i2cStart left clock down (DDRA bit 0 high).
	inc DATA_DIR_A 						; Like I2C_CLOCK_LOW since we know the state from the above instruction.
	dex
	bne i2cClearLoop
	jsr i2cStart
	bra i2cStop 						; (jsr, rts)


i2cSendByte: 							; Start with byte in A, and clock low. Ends with i2cAckQuestion.
	sta i2cScratchByte 					; Store the byte in a variable so we can use A with tsb & trb for data line.
	lda #BIT_I2C_DATA_LINE				; Init A for mask for trb & tsb below. A does not get disturbed below.
	ldx #8 								; We will do 8 bits.

i2cSB1:
	trb DATA_DIR_A 						; Release data line. This is like I2C_DATA_HIGH but saves 1 instruction.
	nop
	nop
	asl i2cScratchByte 					; Get next bit to send and put it in the C flag.
	bcs i2cSB2
	
	tsb DATA_DIR_A 						; If the bit was 0, pull data line down by making it an output.
	; nop
	; nop

	; Guesses as to why it's not working.
	; Sun spot activity.
	; Not reading / shifting the memory location i2cScratchByte correctly.

i2cSB2:
	I2C_CLOCK_PULSE
	dex
	bne i2cSB1
	bra i2cAckQuestion					; (jsr, rts)


i2cReceiveByte:							; Start with clock low. Ends with byte in i2cScratchByte. Do ACK bit separately.
	I2C_DATA_HIGH 						; Make sure we're not holding the data line down. Be ready to input data.
	ldx #8 								; We will do 8 bits. There's no need to init i2cScratchByte.

i2cRB1:
	dec DATA_DIR_A 						; Set clock line high.
	asl i2cScratchByte 					; Get the forming byte's next bit position ready to accept the bit.
	bit PORT_A 							; Read the data line value into N flag.
	bpl i2cRB2 							; If the data line was high,
	inc i2cScratchByte 					; increment the 1's place to a 1 in the forming byte. (asl made bit 0 = 0.)

i2cRB2:
	inc DATA_DIR_A 						; Put clock line back low.
	dex
	bne i2cRB1 							; Go back for next bit if there is one.
	rts

;
; LCD Routines.
;

message:
	.asciiz "Rdy:"

i2cScratchByte:
	.byte %11100010


printStrz:
	ldx #0								; Init the x register used to offset into the array of characters.


printStrzLoop:
	lda message, x						; Get the next character.
	beq printStrzExit					; It's a zero, we're done.
	jsr printChar						; Non-zero - print it.
	inx
	bra printStrzLoop

printStrzExit:
	rts


lcdWait:
	pha
	stz DATA_DIR_B 						; Port B is input.

lcdBusy:
	lda #LCD_READ_WRITE
	sta PORT_A
	lda #(LCD_READ_WRITE | LCD_START_INSTRUCTION)
	sta PORT_A
	lda PORT_B
	and #%10000000						; Busy flag is bit 7.
	bne lcdBusy

	lda #LCD_READ_WRITE
	sta PORT_A
	lda #%11111111 						; Port B is output.
	sta DATA_DIR_B
	pla
	rts


lcdInstruction:
	jsr lcdWait
	sta PORT_B
	stz PORT_A							; Clear RS / RW / instruction bits.
	lda #LCD_START_INSTRUCTION			; Set E bit to send instruction.
	sta PORT_A
	stz PORT_A							; Clear RS / RW / instruction bits.
	rts


printChar:
	jsr lcdWait
	sta PORT_B
	lda #LCD_READ_SELECT				; Set RS, Clear RW / instruction bits.
	sta PORT_A
	lda #(LCD_READ_SELECT | LCD_START_INSTRUCTION)	; Set E bit to send instruction.
	sta PORT_A
	lda #LCD_READ_SELECT				; Clear instruction bits.
	sta PORT_A
	rts

;
; Misc
;

; Place a pair of counters into the X and Y registers and this will waste about 20 cycles each loop through.

wasteTime:
	; nop
	; nop
	; nop
	; nop
	; nop
	nop
	nop
	nop
	dex
	bne wasteTime
	dey
	bne wasteTime
	rts

;
; Main
;

main:
	; Reset the stack to the top.
	ldx #$ff
	txs

	; Set all pins on port B to output.
	lda #%11111111
	sta DATA_DIR_B

	; TODO: Need to switch which pins are being used for this since they conflict with the pins
	; I want to use for the I2C console.

	; Set control pins on port A to output.
	lda #LCD_CTRL_PINS_OUTPUT
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

	; Print our welcome message.
	jsr printStrz

	; TEST: What do we need to init the I2C?
	;jsr i2cInit

	I2C_DATA_HIGH
	I2C_CLOCK_HIGH
	lda #BIT_I2C_BOTH_LINES
	trb PORT_A

	; Going to reset the pins for the LCD control, just to make debugging the code
	; clearer for a while. This should be able to be removed
	lda LCD_CTRL_PINS_OUTPUT
	trb PORT_A

mainLoop:
	jsr i2cStart

	; nop
	; nop
	
	lda #%11110000
 	jsr i2cSendByte
 	jsr i2cStop

	; nop
	; nop
	
	; Infinite loop.
	bra mainLoop

	; lda #MONITOR_DEVICE_ID
	; asl
	; jsr i2cSendByte

	; jsr i2cStart
	; lda #%10001000
	; jsr i2cSendByte

; 	; Just in case, for testing.
; 	jsr i2cClear

; 	; HACK: knock on all the doors
; 	lda #$01
; loopy:
; 	pha
; 	jsr i2cStart
; 	jsr i2cSendByte

; 	jsr i2cStart
; 	lda #$55
; 	jsr i2cSendByte
; 	jsr i2cStop

; 	nop
; 	nop
; 	nop
; 	nop

; 	pla
; 	inc
; 	bne loopy

monitorTestExit:
	; jsr i2cStop
	
	; ldx #$5
	; ldy #$5
	; jsr wasteTime
	
	; HACK: Anything working?
	; lda #$41
	; jsr printChar

	; Infinite loop.
	bra mainLoop


	.org $fffc
	.word main
	.word $0000
