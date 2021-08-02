; To compile:
;    vasm6502_oldstyle.exe 01-test.asm -c02 -dotdir -Fbin

	.org $8000

reset:
	lda #$ff
	sta $6002

	lda #$50
	sta $6000

loop:
	ror
	sta $6000
	jmp loop

	; 6502 bootup reads this address and jumps to it.
	.org #$fffc
	.word reset
	.word $0000