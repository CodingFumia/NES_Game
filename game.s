.segment "HEADER"
	.byte "NES"		;identification string
	.byte $1A
	.byte $02		;amount of PRG ROM in 16K units
	.byte $01		;amount of CHR ROM in 8K units
	.byte $00		;mapper and mirroing
	.byte $00, $00, $00, $00
	.byte $00, $00, $00, $00, $00
.segment "ZEROPAGE"
; Zero-page memory addresses
BUTTON_STATE     = $00
BUTTON_STATE_OLD = $01
BUTTON_PRESSED   = $02
SPRITEX    = $03
FRAMECOUNT = $04

; PPU/Controller I/O
JOY1             = $4016
.segment "STARTUP"
RESET:
	SEI 		;disables interupts
	CLD			;turn off decimal mode
	
	LDX #%1000000	;disable sound IRQ
	STX $4017
	LDX #$00
	STX $4010		;disable PCM
	
	;initialize the stack register
	LDX #$FF
	TXS 		;transfer x to the stack
	
	; Clear PPU registers
	LDX #$00
	STX $2000
	STX $2001
	
	;WAIT FOR VBLANK
:
	BIT $2002
	BPL :-
	
	;CLEARING 2K MEMORY
	TXA
CLEARMEMORY:		;$0000 - $07FF
	STA $0000, X
	STA $0100, X
	STA $0300, X
	STA $0400, X
	STA $0500, X
	STA $0600, X
	STA $0700, X
		LDA #$FF
		STA $0200, X
		LDA #$00
	INX
	CPX #$00
	BNE CLEARMEMORY

    ; Initialize your sprite X position here
    LDA #$80
    STA SPRITEX

	;WAIT FOR VBLANK
:
	BIT $2002
	BPL :-
	
	;SETTING SPRITE RANGE
	LDA #$02
	STA $4014
	NOP
	
	LDA #$3F	;$3F00
	STA $2006
	LDA #$00
	STA $2006
	
	LDX #$00
LOADPALETTES:
	LDA PALETTEDATA, X
	STA $2007
	INX
	CPX #$20
	BNE LOADPALETTES

;LOADING SPRITES
	LDX #$00
LOADSPRITES:
	LDA SPRITEDATA, X
	STA $0200, X
	INX
	CPX #$20	;16bytes (4 bytes per sprite, 8 sprites total)
	BNE LOADSPRITES
	
;ENABLE INTERUPTS
	CLI
	
	LDA #%10010000
	STA $2000			;WHEN VBLANK OCCURS CALL NMI
	
	LDA #%00011110		;show sprites and backgroundound
	STA $2001

MainLoop:
    ; Move only every few frames
	LDA FRAMECOUNT
	AND #%00001111    ; every 16 frames
	BNE SkipMove

    JSR ReadController

    ;LDA BUTTON_PRESSED
    ;AND #%11000000     ; Check Left and Right (bits 6 and 7)

    ;CMP #%01000000     ; Left only
    ;BEQ LeftPressed

    ;CMP #%10000000     ; Right only
    ;BEQ RightPressed

		; Use for movement (held)
	LDA BUTTON_STATE
	AND #%00000010
	BNE LeftPressed

	; Use for actions (one-shot)
	LDA BUTTON_STATE
	AND #%00000001
	BNE RightPressed

    JMP SkipMove

RightPressed:
    ; --- Move Left (bit 0) ---
    LDA SPRITEX
    CLC
    ADC #1
    STA SPRITEX
    JMP SkipMove

LeftPressed:
    ; --- Move Left (bit 1) ---
    LDA SPRITEX
    SEC
    SBC #1
    STA SPRITEX
	JMP SkipMove

SkipMove:
    ; Transfer sprite OAM
    LDA #$02
    STA $4014
    JMP MainLoop

NMI:
    INC FRAMECOUNT

	; Overwrite sprite X positions with SPRITEX
	LDA SPRITEX
	STA $0203
	CLC
	ADC #8
	STA $0207
	LDA SPRITEX
	STA $020B
	CLC
	ADC #8
	STA $020F

    RTI

PALETTEDATA:
	.byte $33, $0F, $00, $10, 	$00, $0A, $15, $01, 	$00, $29, $28, $27, 	$00, $34, $24, $14 	;background palettes
	.byte $0F, $13, $23, $33, 	$00, $0F, $11, $30, 	$0F, $23, $12, $32, 	$00, $3C, $2C, $1C 	;sprite palettes

;Y, SPRITE NUM, attributes, X
;76543210
;||||||||P
;||||||++- Palette (4 to 7) of sprite
;|||+++--- Unimplemented
;||+------ Priority (0: in front of background; 1: behind background)
;|+------- Flip sprite horizontally
;+-------- Flip sprite vertically
SPRITEDATA:
    ;PLAYER
	.byte $80, $00, $00, $80
	.byte $80, $01, $00, $88
	.byte $88, $10, $00, $80
	.byte $88, $11, $00, $88

.segment "CODE"
;READ INPUT
ReadController:
    ; Save previous state
    LDA BUTTON_STATE
    STA BUTTON_STATE_OLD

    ; Strobe controller
    LDA #$01
    STA JOY1
    LDA #$00
    STA JOY1

    ; Read 8 button bits
    LDY #$08
    LDA #$00         ; Clear BUTTON_STATE
    STA BUTTON_STATE

ReadLoop:
    LDA JOY1
    AND #$01         ; Isolate current button bit
    LSR A            ; Move into carry
    ROL BUTTON_STATE ; Rotate into BUTTON_STATE left to right
    DEY
    BNE ReadLoop

    ; Calculate new presses
    LDA BUTTON_STATE
    EOR BUTTON_STATE_OLD
    AND BUTTON_STATE
    STA BUTTON_PRESSED

    RTS
.segment "VECTORS"
    .word NMI
    .word RESET
    ;specialized hardware interupts
.segment "CHARS"
    .incbin "rom.chr"