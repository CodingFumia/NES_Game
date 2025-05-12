.segment "HEADER"
    .byte "NES"     ;identification string
    .byte $1A
    .byte $02       ;amount of PROGRAM ROM in 16K units
    .byte $01       ;amount of CHARACTER ROM in 8K units
    .byte $00       ;mapper and mirroing
    .byte $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00
.segment "ZEROPAGE"
CONTROLLER: .res 1
SPRITEX: .res 1
FRAMECOUNT: .res 1
.segment "STARTUP"

RESET:
    SEI             ;disables interupts
    CLD             ;turn off decimal mode

    LDX #%1000000    ;disable sound IRQ
    STX $4017
    LDX #$00
    STX $4010   ;disable PCM

    ;initialize the stack register
    LDX #$FF
    TXS ;transfer X to the stack

    ;Clear PPU registers
    LDX #$00
    STX $2000
    STX $2001

    ;Wait for VBLANK
:
    BIT $2002
    BPL :-

    ;CLEARING 2K MEMORY
    TXA
CLEARMEMORY:        ;$0000 - $07FF
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

    ;Wait for VBLANK
    ; RESET SPRITE X
    LDA #$80
    STA SPRITEX
:
    BIT $2002
    BPL :-

    ;SETTIGN SPRITES RANGE
    LDA #$02
    STA $4014

    NOP

    LDA #$3F    ;$3F00
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

    LDX #$00

;LOADING SPRITES

LOADSPRITES:
        LDA SPRITEDATA, X
        STA $0200, X
        INX
        CPX #$20    ;how many sprites should be rendered  $10 = 16 bytes    4 bytes per sprite
        BNE LOADSPRITES

;LOADING BACKGROUNDS

LOADBACKGROUND:
	LDA $2002		;read PPU status to reset high/low latch
	LDA #$21
	STA $2006
	LDA #$00
	STA $2006
	LDX #$00
LOADBACKGROUNDP1:
    LDA BACKGROUNDDATA, X
    STA $2007
    INX
    CPX #$00
    BNE LOADBACKGROUNDP1
LOADBACKGROUNDP2:
	LDA BACKGROUNDDATA+256, X
	STA $2007
	INX
	CPX #$00
	BNE LOADBACKGROUNDP2

;LOAD BACKGROUND PALETTEDATA
	LDA #$23	;$23D0
	STA $2006
	LDA #$D0
	STA $2006
	LDX #$00
LOADBACKGROUNDPALETTEDATA:
	LDA BACKGROUNDPALETTEDATA, X
	STA $2007
	INX
	CPX #$20
	BNE LOADBACKGROUNDPALETTEDATA

	;RESET SCROLL
	LDA #$00
	STA $2005
	STA $2005

;ENABLE INTERUPTS
    CLI

    LDA #%10010000
    STA $2000       ;when vblank occurs call nmi

    LDA #%00011110
    STA $2001       ;show sprites and background

MainLoop:
    ; Only allow movement every 4 frames
    LDA FRAMECOUNT
    AND #%00000011   ; move every 4 frames
    BNE SkipMove

    JSR ReadController

    ; --- Check right ---
    LDA CONTROLLER
    AND #%10000000
    BEQ CheckLeft

    LDA SPRITEX
    CLC
    ADC #1
    STA SPRITEX
    JMP SkipMove   ; ← avoid falling into left check

CheckLeft:
    LDA CONTROLLER
    AND #%01000000
    BEQ SkipMove

    LDA SPRITEX
    SEC
    SBC #1
    STA SPRITEX

SkipMove:
    JMP MainLoop

    JMP INFLOOP
    INFLOOP:
        JMP INFLOOP
NMI:
    INC FRAMECOUNT
    
    ; Transfer sprites to PPU
    LDA #$02
    STA $4014

    ; Update X positions of sprite
    LDA SPRITEX
    STA $0203       ; Top-left
    CLC
    ADC #8
    STA $0207       ; Top-right
    LDA SPRITEX
    STA $020B       ; Bottom-left
    CLC
    ADC #8
    STA $020F       ; Bottom-right

    RTI

PALETTEDATA:
	.byte $00, $0F, $00, $10, 	$00, $0A, $15, $01, 	$00, $29, $28, $27, 	$00, $34, $24, $14 	;background palettes
	.byte $31, $0F, $15, $30, 	$00, $0F, $11, $30, 	$00, $0F, $30, $27, 	$00, $3C, $2C, $1C 	;sprite palettes

SPRITEDATA:
;Y, SPRITE NUM, attributes, X
;76543210
;||||||||P
;||||||++- Palette (4 to 7) of sprite
;|||+++--- Unimplemented
;||+------ Priority (0: in front of background; 1: behind background)
;|+------- Flip sprite horizontally
;+-------- Flip sprite vertically

    ;PLAYER
	.byte $80, $00, %00000010, $80
	.byte $80, $01, %00000010, $88
	.byte $88, $10, %00000010, $80
	.byte $88, $11, %00000010, $88

BACKGROUNDDATA:	;512 BYTES
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

BACKGROUNDPALETTEDATA:	;32 bytes
	.byte $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55
	.byte $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55

.segment "CODE"
;READ INPUT
ReadController:
    ; Strobe the controller
    lda #$01
    sta $4016      ; Write 1 to strobe
    lda #$00
    sta $4016      ; Write 0 to stop strobe

    ; Read 8 bits from controller (buttons)
    ldx #$08       ; Loop 8 times
    lda #$00
    sta CONTROLLER
ReadLoop:
    lda $4016      ; Read a button bit (A first)
    lsr a          ; Shift bit 0 into carry
    rol CONTROLLER ; Rotate carry into CONTROLLER
    dex
    bne ReadLoop

    rts
.segment "VECTORS"
    .word NMI
    .word RESET
    ;specialized hardware interupts
.segment "CHARS"
    .incbin "rom.chr"