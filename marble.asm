;===================================
; Proto-code for project "Marble"
;===================================


;===================================
; Rev history
; v1 = Draw PF from PNG
;===================================

;===================================
; Special Thanks:
;  - graphicsbmp
;===================================

;===================================
; Bank Layouts
; (Only one bank for now)
;===================================

;===================================
; RAM Allocation
;===================================
;---------------------
; $80-$8F = Temp variables
;---------------------
Temp = $80  ; one Word

;---------------------
; $90-$9F = Screen variables
;---------------------
SWCHAStore = $90
ScrollPointerTop = $91
ScrollPointerBottom = $92
OddFrameCheck = $93
Player0HPosition = $94
Player1HPosition = $95
Player0VPosition = $96
Player0VPosition2 = $97

;---------------------
; $A0-$AF = Marble RAM
;---------------------
P0MarbleRAM = $A0 ; 8 bytes
; Next is $A8

;===================================
; Constants
NTSC = 1
LEVEL = 0
  IF LEVEL == 0
MAXLEVELHEIGHT = 200
  ELSE IF LEVEL == 1
MAXLEVELHEIGHT = 255
  ENDIF
;===================================


;===================================
	processor 6502
	include vcs.h
;===================================


;#####################################################
;#####################################################
;#####################################################
;###                                               ###
;###                  Bank 1 below                 ###
;###                                               ###
;#####################################################
;#####################################################
;#####################################################

	;============
	org	$F000
	;============


;=================
Start
;=================
	SEI
	CLD
	LDX	#$FF
	TXS
	LDA	#0
ClearingRAM
	STA	0,X ; clear $FF through $1 (not $0, VSYNC).
	DEX
	BNE	ClearingRAM

;=================
GameInitBank1
;=================

	; Do init stuff
	LDA	#200
	STA	ScrollPointerTop ; highest for practiceLevel
	LDA	#(200-89)
	STA	ScrollPointerBottom ; lowest
	LDA	#1
	STA	OddFrameCheck ; even frame
	LDA	#77
	STA	Player0HPosition
	LDA	#189
	STA	Player0VPosition ; these are divided by 2, so pos is 20.
	LDA	#0
	STA	Player0VPosition2 ; 0 or 1, frame counter to slow ball

;=================
MainLoopBank1
;=================
	JSR	VerticalBlankBank1 ; Execute the vertical blank.
	; Game won?
	; End screen
	;JSR	ResetSelectCheck
	JSR	GameCalcBank1	; Do calculations during Vblank
	JSR	DrawScreenBank1	; Draw the screen
	JSR	OverScanBank1	; Do more calculations during overscan
	;==================
	JMP	MainLoopBank1	; Continue forever.
	;==================

;=================
VerticalBlankBank1
;=================
	LDX	#0
	LDA	#2
	STA	WSYNC
	STA	VSYNC ; Begin vertical sync.
	STA	WSYNC ; First line of VSYNC
	STA	WSYNC ; Second line of VSYNC.
  IF NTSC
	LDA	#44
  ELSE ; (PAL)
	LDA	#54
  ENDIF
	STA	TIM64T
	LDA	#0
	; Now we can end the VSYNC period.
	STA	WSYNC ; Third line of VSYNC.
	STA	VSYNC ; (0)
	;==================
	RTS
	;==================

;=================
GameCalcBank1
;=================

	;=================
	LDA	SWCHA
	STA	SWCHAStore
	;=================

	;=================
	; Joystick up/down movement-- tied to Grinch V Position data
	;=================
	LDA	SWCHAStore
	AND	#%00010000 ; up
	BNE	CheckDown

HandleDown
	; Can we move Player0 down?
	LDA	Player0VPosition	; start value 160
	SEC
	SBC	ScrollPointerBottom
	CMP	#80
	BEQ	ScrollFrameDown
	; otherwise, move the ball down
	INC	Player0VPosition2
	LDA	Player0VPosition2
	AND	#1
	BNE	CheckDown
	INC	Player0VPosition
	JMP	CheckDown
ScrollFrameDown
	LDA	ScrollPointerTop
	CMP	#MAXLEVELHEIGHT
	BEQ	CheckDown

	LDA	OddFrameCheck
	BNE	ScrollDown2
	INC	OddFrameCheck
	JMP	CheckDown ; can turn to BNE later

ScrollDown2
	LDA	#0
	STA	OddFrameCheck
	INC	ScrollPointerTop
	INC	ScrollPointerBottom

CheckDown
	LDA	SWCHAStore
	AND	#%00100000 ; down
	BNE	CheckRight

HandleUp
	; Can we move Player0 up?
	LDA	Player0VPosition	; start value 160
	SEC
	SBC	ScrollPointerBottom
	CMP	#10
	BEQ	ScrollFrameUp
	; otherwise, move the ball up
	INC	Player0VPosition2
	LDA	Player0VPosition2
	AND	#1
	BNE	CheckRight
	DEC	Player0VPosition
	JMP	CheckRight
ScrollFrameUp

	LDA	ScrollPointerTop
	CMP	#89 ; always the bottom
	BEQ	CheckRight
	LDA	OddFrameCheck
	BEQ	ScrollUp2
	DEC	OddFrameCheck
	JMP	CheckRight
ScrollUp2

	LDA	#1
	STA	OddFrameCheck
	DEC	ScrollPointerTop
	DEC	ScrollPointerBottom
CheckRight
	LDA	SWCHAStore
	AND	#%10000000 ; right
	BNE	CheckLeft
	LDA	Player0HPosition
	CMP	#136	; right hand extrema
	BEQ	CheckLeft
	INC	Player0HPosition
CheckLeft
	LDA	SWCHAStore
	AND	#%01000000 ; left
	BNE	InitialJoyCheckDone
	LDA	Player0HPosition
	CMP	#16	; left hand extrema
	BEQ	InitialJoyCheckDone
	DEC	Player0HPosition
InitialJoyCheckDone
	;=================

	;=================
	; set up P0/P1 for timer for frame
	;=================
	STA	WSYNC
	LDY	#7
PlayerCoarseLoop
	DEY
	BPL	PlayerCoarseLoop
	NOP
	STA	RESP0
	STA	RESP1
	LDA	#%00110000
	STA	HMP0
	LDA	#%01000000
	STA	HMP1
	STA	WSYNC
	STA	HMOVE
	;==================

	;==================
	; Load Marble data into RAM
	;==================
	LDY	#7
P0MarbleInRamLoop
	LDA	Marble1,Y
	STA	P0MarbleRAM,Y
	DEY
	BPL	P0MarbleInRamLoop
	;==================

	;==================
	RTS
	;==================

NumberZero
	dc.b	%00011000
	dc.b	%00100100
	dc.b	%01000010
	dc.b	%01000010
	dc.b	%01000010
	dc.b	%00100100
	dc.b	%00011000

NumberOne
	dc.b	%00111110
	dc.b	%00001000
	dc.b	%00001000
	dc.b	%00001000
	dc.b	%00001000
	dc.b	%00011000
	dc.b	%00001000

Marble1
	dc.b	%00000000
	dc.b	%00011000
	dc.b	%00111100
	dc.b	%01111110
	dc.b	%01111110
	dc.b	%01111110
	dc.b	%00111100
	dc.b	%00011000

;=================
DrawScreenBank1
;=================
	LDA	INTIM
	BNE	DrawScreenBank1 ; Whew!
	STA	WSYNC	; [0]
	STA	VBLANK  ; Enable drawing again (set vblank to 0)

	; First 2 lines of kernel are set-up for main stuff.
	LDA	#0
	STA	COLUBK
	LDA	#1
	STA	CTRLPF ; reflected

	;===========
	; DRAW THE TIMER section
    IF NTSC
	LDA	#$82	; blue
    ELSE ; (PAL)
	LDA	#$D2	; blue
    ENDIF
	STA	COLUPF
	LDA	#$0E ; same as NTSC/PAL
	STA	COLUP0
	STA	COLUP1
	LDA	#$C0
	STA	PF2

	LDY	#6
	STA	WSYNC ; 1 blue line for score
ScoreLoop
	LDA	NumberOne,Y
	STA	GRP0
	LDA	NumberZero,Y
	STA	GRP1
	STA	WSYNC
	DEY
	BPL	ScoreLoop
	LDA	#0
	STA	GRP0
	STA	GRP1
    IF NTSC
	LDA	#$82	; blue
    ELSE ; (PAL)
	LDA	#$D2	; blue
    ENDIF
	STA	COLUP0
	; Define player 2 as red
	;===========
	
	;LDY	#(89-1) ; (89*2 = 178 + 8 + 2 + 4 on top = 192)
	LDY	ScrollPointerTop
	DEY

	; we need to set P0's position.
	;====================
	; Calculate P1,P0 HPos
	;====================
	; coarse/fine setting of P1 graphic HPos
	; code by vdub_bobby
	LDX	#0
	LDA	Player0HPosition
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	STX	PF2

	; Calculate Player's left/right position on screen
CalculatePlayersHPosLoop
	sec
	sta	HMCLR
	sta	WSYNC ; [5]
DivideLoopPlayersBank1
	sbc	#15
	bcs	DivideLoopPlayersBank1
	eor	#7
	asl
	asl
	asl
	asl
	sta.wx	HMP0,X
	sta	RESP0,X
	sta	WSYNC ; [6]
	sta	HMOVE
	STA	WSYNC ; without this line, we screw up P1 positioning by
                      ; touching HMP1 with an HMCLR too early on P0 positioning
	;DEX
	;LDA	Player1HPosition
	;BPL	CalculatePlayersHPosLoop

	STA	WSYNC ; to even out

	;==========
	LDA	OddFrameCheck
	BEQ	DrawLoopBank1Pass1
	STA	WSYNC ; [1]
	;==========


DrawLoopBank1Pass1
	;==========
	STA	WSYNC ; [2, 4 = first even frame completed]
	;==========

	LDA	#$08	; white ; [2] (same as NTSC and PAL)

	STA	COLUPF ; [3, 5]
	;==== NEW CODE ====
	LDA	Level_0_WhiteData_PF1_1,Y	; [4, 9]
	STA	PF1	; [3, 12]
	LDA	Level_0_WhiteData_PF2_2,Y	; [4, 16]
	STA	PF2	; [3, 19]
	NOP		; [2, 21]
	NOP		; [2, 23]
	NOP		; [2, 25]
	NOP		; [2, 27]
	NOP		; [2, 29]
	NOP		; [2, 31]
	LDA	$80	; [3, 34]
	LDA	Level_0_WhiteData_PF1_4,Y	; [4, 38]
	STA	PF1	; [3, 41]
	LDA	Level_0_WhiteData_PF2_3,Y	; [4, 45]
	STA	PF2	; [3, 48]
	;== END NEW CODE ==

	;==========
	STA	WSYNC ; [3, = first odd frame completed]
	;==========
	
  IF LEVEL == 0
    IF NTSC
	LDA	#$22	; brown
    ELSE ; (PAL)
	LDA	#$42	; brown
    ENDIF
  ENDIF
  IF LEVEL == 1
    IF NTSC
	LDA	#$82	; blue
    ELSE ; (PAL)
	LDA	#$D2	; blue
    ENDIF
  ENDIF

	STA	COLUPF ; [3, 5]
	;==== NEW CODE ====
	LDA	Level_0_BlueData_PF1_1,Y	; [4, 9]
	STA	PF1	; [3, 12]
	LDA	Level_0_BlueData_PF2_2,Y	; [4, 16]
	STA	PF2	; [3, 19]
	NOP		; [2, 21]
	NOP		; [2, 23]
	NOP		; [2, 25]
	NOP		; [2, 27]
	NOP		; [2, 29]
	NOP		; [2, 31]
	LDA	$80	; [3, 34]
	LDA	Level_0_BlueData_PF1_4,Y	; [4, 38]
	STA	PF1	; [3, 41]
	LDA	Level_0_BlueData_PF2_3,Y	; [4, 45]
	STA	PF2	; [3, 48]
	;== END NEW CODE ==

	DEY
	;CPY	ScrollPointerBottom
	CPY	Player0VPosition
	BNE	DrawLoopBank1Pass1
	;=======================

;=================
	LDX	#7 ; marble frames
DrawLoopBank1Pass2
	;==========
	STA	WSYNC ; [2, 4 = first even frame completed]
	;==========

	LDA	#$08	; white ; [2] (same as NTSC and PAL)

	STA	COLUPF ; [3, 5]
	;==== NEW CODE ====
	LDA	Level_0_WhiteData_PF1_1,Y	; [4, 9]
	STA	PF1	; [3, 12]
	LDA	P0MarbleRAM,X ; [4 , 16]
	STA	GRP0	; [3, 19]
	LDA	Level_0_WhiteData_PF2_2,Y	; [4, 23]
	STA	PF2	; [3, 26]
	NOP		; [2, 28]
	NOP		; [2, 30]
	NOP		; [2, 32]
	NOP		; [2, 34]
	LDA	Level_0_WhiteData_PF1_4,Y	; [4, 38]
	STA	PF1	; [3, 41]
	LDA	Level_0_WhiteData_PF2_3,Y	; [4, 45]
	STA	PF2	; [3, 48]
	;== END NEW CODE ==
	DEX	; marble pointer

	;==========
	STA	WSYNC ; [3, = first odd frame completed]
	;==========
	
  IF LEVEL == 0
    IF NTSC
	LDA	#$22	; brown
    ELSE ; (PAL)
	LDA	#$42	; brown
    ENDIF
  ENDIF
  IF LEVEL == 1
    IF NTSC
	LDA	#$82	; blue
    ELSE ; (PAL)
	LDA	#$D2	; blue
    ENDIF
  ENDIF

	STA	COLUPF ; [3, 5]
	;==== NEW CODE ====
	LDA	Level_0_BlueData_PF1_1,Y	; [4, 9]
	STA	PF1	; [3, 12]
	LDA	P0MarbleRAM,X ; [4 , 16]
	STA	GRP0	; [3, 19]
	LDA	Level_0_BlueData_PF2_2,Y	; [4, 23]
	STA	PF2	; [3, 26]
	NOP		; [2, 28]
	NOP		; [2, 30]
	NOP		; [2, 32]
	NOP		; [2, 34]
	LDA	Level_0_BlueData_PF1_4,Y	; [4, 38]
	STA	PF1	; [3, 41]
	LDA	Level_0_BlueData_PF2_3,Y	; [4, 45]
	STA	PF2	; [3, 48]
	;== END NEW CODE ==
	DEY	; frame pointer
	DEX	; marble pointer
	BPL	DrawLoopBank1Pass2
	;=======================

;=============

DrawLoopBank1Pass3
	;==========
	STA	WSYNC ; [2, 4 = first even frame completed]
	;==========

	LDA	#$08	; white ; [2] (same as NTSC and PAL)

	STA	COLUPF ; [3, 5]
	;==== NEW CODE ====
	LDA	Level_0_WhiteData_PF1_1,Y	; [4, 9]
	STA	PF1	; [3, 12]
	LDA	Level_0_WhiteData_PF2_2,Y	; [4, 16]
	STA	PF2	; [3, 19]
	NOP		; [2, 21]
	NOP		; [2, 23]
	NOP		; [2, 25]
	NOP		; [2, 27]
	NOP		; [2, 29]
	NOP		; [2, 31]
	LDA	$80	; [3, 34]
	LDA	Level_0_WhiteData_PF1_4,Y	; [4, 38]
	STA	PF1	; [3, 41]
	LDA	Level_0_WhiteData_PF2_3,Y	; [4, 45]
	STA	PF2	; [3, 48]
	;== END NEW CODE ==

	;==========
	STA	WSYNC ; [3, = first odd frame completed]
	;==========
	
  IF LEVEL == 0
    IF NTSC
	LDA	#$22	; brown
    ELSE ; (PAL)
	LDA	#$42	; brown
    ENDIF
  ENDIF
  IF LEVEL == 1
    IF NTSC
	LDA	#$82	; blue
    ELSE ; (PAL)
	LDA	#$D2	; blue
    ENDIF
  ENDIF

	STA	COLUPF ; [3, 5]
	;==== NEW CODE ====
	LDA	Level_0_BlueData_PF1_1,Y	; [4, 9]
	STA	PF1	; [3, 12]
	LDA	Level_0_BlueData_PF2_2,Y	; [4, 16]
	STA	PF2	; [3, 19]
	NOP		; [2, 21]
	NOP		; [2, 23]
	NOP		; [2, 25]
	NOP		; [2, 27]
	NOP		; [2, 29]
	NOP		; [2, 31]
	LDA	$80	; [3, 34]
	LDA	Level_0_BlueData_PF1_4,Y	; [4, 38]
	STA	PF1	; [3, 41]
	LDA	Level_0_BlueData_PF2_3,Y	; [4, 45]
	STA	PF2	; [3, 48]
	;== END NEW CODE ==

	DEY
	CPY	ScrollPointerBottom
	BPL	DrawLoopBank1Pass3
	;=======================


	;========================
	; scanline 192
	;========================
	; Clear all registers here to prevent any possible bleeding.
	LDA	#2
	STA	WSYNC  ; Finish this scanline.
	STA	VBLANK ; Make TIA output invisible,
	; Now we need to worry about it bleeding when we turn
	; the TIA output back on.
	LDY	#0
	STY	PF0
	STY	PF1
	STY	PF2
	STY	GRP1
	STY	GRP0
	STY	VDELP1
	STY	ENAM0
	STY	ENAM1
	STY	ENABL	

	;==========
	LDA	OddFrameCheck
	BNE	ReturnFromDrawScreen
	STA	WSYNC
	;==========

ReturnFromDrawScreen
	;==================
	RTS
	;==================


;=================
OverScanBank1
;=================
  IF NTSC
	LDA	#35
  ELSE ; (PAL)
	LDA	#85
  ENDIF
	STA	TIM64T

;=================
GameCalc2Bank1
;=================

	;==================
	; Handle collisions
	;==================

	;==================
	; Clear them for next time
	;==================
DoneHandlingCollisionsGame1
	LDA	#0
	STA	CXCLR

	;============================
	; Loop to get our 30 scanlines for overscan
	;============================
WaitForEndOfOverscanBank1
	LDA	INTIM
	BNE	WaitForEndOfOverscanBank1
	STA	WSYNC	; finish scanline 30
	;==================
	RTS
	;==================


;====================
; GRAPHICS DATA BELOW
;====================

;REF=- (D0 of CTRLPF)
;| 4567 | 76543210 | 01234567 | 4567 | 76543210 | 01234567 |
;| PF0  | PF1      | PF2      | PF0  | PF1      | PF2      |
;
;REF=1 (D0 of CTRLPF)
;| 4567 | 76543210 | 01234567 | 76543210 | 01234567 | 7654 |
;| PF0  | PF1      | PF2      | PF2      | PF1      | PF0  |  



  IF LEVEL==0

	;==========
	ORG	$F700
	;==========
Level_0_WhiteData_PF1_1
	; 20-0
	dc.b	$A2
	dc.b	$82
	dc.b	$A2
	dc.b	$82
	dc.b	$A2
	dc.b	$82
	dc.b	$A2
	dc.b	$82
	dc.b	$A2
	dc.b	$82
	dc.b	$A2
	dc.b	$82
	dc.b	$A2
	dc.b	$82
	dc.b	$A2
	dc.b	$82
	dc.b	$A2
	dc.b	$82
	dc.b	$A2
	dc.b	$82
	; 40-21
	dc.b	$A2
	dc.b	$82
	dc.b	$A2
	dc.b	$82
	dc.b	$A2
	dc.b	$82
	dc.b	$A2
	dc.b	$83
	dc.b	$A0
	dc.b	$C5
	dc.b	$22
	dc.b	$57
	dc.b	$AA
	dc.b	$F4
	dc.b	$BA
	dc.b	$5D
	dc.b	$2E
	dc.b	$17
	dc.b	$0A
	dc.b	$15
	; 60-41
	dc.b	$8A
	dc.b	$DC
	dc.b	$A8
	dc.b	$50
	dc.b	$A8
	dc.b	$5C
	dc.b	$AA
	dc.b	$55
	dc.b	$BA
	dc.b	$55
	dc.b	$AA
	dc.b	$75
	dc.b	$AA
	dc.b	$D5
	dc.b	$A9
	dc.b	$71
	dc.b	$A9
	dc.b	$C9
	dc.b	$A9
	dc.b	$29
	; 80-61
	dc.b	$A9
	dc.b	$A8
	dc.b	$A8
	dc.b	$AA
	dc.b	$A9
	dc.b	$A9
	dc.b	$A9
	dc.b	$AB
	dc.b	$A8
	dc.b	$A8
	dc.b	$A8
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	; 100-81
	dc.b	$AA
	dc.b	$AB
	dc.b	$A8
	dc.b	$AD
	dc.b	$A2
	dc.b	$B5
	dc.b	$88
	dc.b	$D9
	dc.b	$22
	dc.b	$67
	dc.b	$8A
	dc.b	$1D
	dc.b	$2A
	dc.b	$77
	dc.b	$AA
	dc.b	$9D
	dc.b	$AA
	dc.b	$A7
	dc.b	$AA
	dc.b	$A9
	; 120-101
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AB
	dc.b	$A8
	dc.b	$A9
	dc.b	$A8
	dc.b	$AD
	dc.b	$AB
	dc.b	$A3
	dc.b	$A3
	dc.b	$B5
	dc.b	$AE
	dc.b	$8F
	; 140-121
	dc.b	$8F
	dc.b	$D7
	dc.b	$BB
	dc.b	$3D
	dc.b	$3E
	dc.b	$5F
	dc.b	$EF
	dc.b	$F6
	dc.b	$FA
	dc.b	$FD
	dc.b	$7D
	dc.b	$B9
	dc.b	$D8
	dc.b	$A0
	dc.b	$60
	dc.b	$A8
	dc.b	$C8
	dc.b	$B4
	dc.b	$70
	dc.b	$B2
	; 160-141
	dc.b	$AA
	dc.b	$DD
	dc.b	$DC
	dc.b	$AC
	dc.b	$AA
	dc.b	$77
	dc.b	$72
	dc.b	$AD
	dc.b	$AE
	dc.b	$DF
	dc.b	$CE
	dc.b	$B6
	dc.b	$B9
	dc.b	$7D
	dc.b	$BA
	dc.b	$DA
	dc.b	$A7
	dc.b	$77
	dc.b	$AA
	dc.b	$9A
	; 180-161
	dc.b	$BD
	dc.b	$59
	dc.b	$16
	dc.b	$2E
	dc.b	$0F
	dc.b	$16
	dc.b	$25
	dc.b	$2A
	dc.b	$23
	dc.b	$32
	dc.b	$29
	dc.b	$2A
	dc.b	$17
	dc.b	$46
	dc.b	$26
	dc.b	$29
	dc.b	$5D
	dc.b	$9A
	dc.b	$1A
	dc.b	$27
	; 200-181
	dc.b	$77
	dc.b	$AA
	dc.b	$DA
	dc.b	$BD
	dc.b	$79
	dc.b	$B6
	dc.b	$AE
	dc.b	$DF
	dc.b	$DE
	dc.b	$AC
	dc.b	$A8
	dc.b	$70
	dc.b	$70
	dc.b	$A0
	dc.b	$A0
	dc.b	$C0
	dc.b	$C0
	dc.b	$80
	dc.b	$80
	dc.b	$00

	;==========
	ORG	$F800
	;==========
Level_0_WhiteData_PF2_2
	; 20-0
	dc.b	$05
	dc.b	$01
	dc.b	$05
	dc.b	$01
	dc.b	$05
	dc.b	$01
	dc.b	$05
	dc.b	$01
	dc.b	$05
	dc.b	$01
	dc.b	$05
	dc.b	$01
	dc.b	$05
	dc.b	$01
	dc.b	$05
	dc.b	$01
	dc.b	$05
	dc.b	$01
	dc.b	$05
	dc.b	$01
	; 40-21
	dc.b	$05
	dc.b	$01
	dc.b	$05
	dc.b	$01
	dc.b	$05
	dc.b	$03
	dc.b	$04
	dc.b	$0E
	dc.b	$15
	dc.b	$3B
	dc.b	$51
	dc.b	$E0
	dc.b	$40
	dc.b	$80
	dc.b	$00
	dc.b	$00
	dc.b	$01
	dc.b	$03
	dc.b	$05
	dc.b	$0A
	; 60-41
	dc.b	$13
	dc.b	$2B
	dc.b	$4D
	dc.b	$AC
	dc.b	$34
	dc.b	$B0
	dc.b	$D0
	dc.b	$C8
	dc.b	$49
	dc.b	$2B
	dc.b	$29
	dc.b	$AA
	dc.b	$6A
	dc.b	$8A
	dc.b	$5A
	dc.b	$22
	dc.b	$56
	dc.b	$C8
	dc.b	$D5
	dc.b	$B2
	; 80-61
	dc.b	$36
	dc.b	$2D
	dc.b	$13
	dc.b	$2B
	dc.b	$4A
	dc.b	$AD
	dc.b	$34
	dc.b	$B0
	dc.b	$D4
	dc.b	$C5
	dc.b	$55
	dc.b	$15
	dc.b	$55
	dc.b	$D5
	dc.b	$15
	dc.b	$B5
	dc.b	$45
	dc.b	$2D
	dc.b	$51
	dc.b	$CB
	; 100-81
	dc.b	$D4
	dc.b	$B2
	dc.b	$35
	dc.b	$A6
	dc.b	$56
	dc.b	$9A
	dc.b	$59
	dc.b	$6B
	dc.b	$65
	dc.b	$AE
	dc.b	$95
	dc.b	$BB
	dc.b	$55
	dc.b	$EE
	dc.b	$55
	dc.b	$BB
	dc.b	$55
	dc.b	$EE
	dc.b	$55
	dc.b	$BB
	; 120-101
	dc.b	$55
	dc.b	$EE
	dc.b	$55
	dc.b	$B9
	dc.b	$55
	dc.b	$ED
	dc.b	$55
	dc.b	$BB
	dc.b	$55
	dc.b	$EE
	dc.b	$55
	dc.b	$BB
	dc.b	$55
	dc.b	$EE
	dc.b	$55
	dc.b	$BB
	dc.b	$55
	dc.b	$EF
	dc.b	$4F
	dc.b	$B6
	; 140-121
	dc.b	$55
	dc.b	$EB
	dc.b	$47
	dc.b	$8D
	dc.b	$03
	dc.b	$88
	dc.b	$1C
	dc.b	$AA
	dc.b	$77
	dc.b	$AA
	dc.b	$05
	dc.b	$8E
	dc.b	$03
	dc.b	$09
	dc.b	$1C
	dc.b	$2A
	dc.b	$77
	dc.b	$2A
	dc.b	$05
	dc.b	$0F
	; 160-141
	dc.b	$03
	dc.b	$01
	dc.b	$0C
	dc.b	$06
	dc.b	$0E
	dc.b	$04
	dc.b	$0D
	dc.b	$01
	dc.b	$09
	dc.b	$22
	dc.b	$15
	dc.b	$45
	dc.b	$53
	dc.b	$BB
	dc.b	$35
	dc.b	$35
	dc.b	$4E
	dc.b	$EE
	dc.b	$55
	dc.b	$B5
	; 180-161
	dc.b	$7B
	dc.b	$F3
	dc.b	$6D
	dc.b	$5D
	dc.b	$BE
	dc.b	$BD
	dc.b	$5B
	dc.b	$55
	dc.b	$EE
	dc.b	$E5
	dc.b	$5B
	dc.b	$5D
	dc.b	$BE
	dc.b	$9D
	dc.b	$6D
	dc.b	$73
	dc.b	$FB
	dc.b	$75
	dc.b	$B5
	dc.b	$4E
	; 200-181
	dc.b	$EE
	dc.b	$55
	dc.b	$B5
	dc.b	$7B
	dc.b	$F3
	dc.b	$6D
	dc.b	$5D
	dc.b	$BE
	dc.b	$BC
	dc.b	$58
	dc.b	$50
	dc.b	$E0
	dc.b	$E0
	dc.b	$40
	dc.b	$40
	dc.b	$80
	dc.b	$80
	dc.b	$00
	dc.b	$00
	dc.b	$00

	;==========
	ORG	$F900
	;==========
Level_0_WhiteData_PF2_3
	; 20-0
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	; 40-21
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$02
	dc.b	$03
	dc.b	$80
	dc.b	$C5
	dc.b	$A2
	dc.b	$75
	dc.b	$2A
	dc.b	$1D
	dc.b	$0A
	dc.b	$05
	; 60-41
	dc.b	$02
	dc.b	$01
	dc.b	$00
	dc.b	$00
	dc.b	$80
	dc.b	$40
	dc.b	$20
	dc.b	$50
	dc.b	$C8
	dc.b	$D8
	dc.b	$B4
	dc.b	$4C
	dc.b	$AC
	dc.b	$2A
	dc.b	$B6
	dc.b	$D2
	dc.b	$C3
	dc.b	$40
	dc.b	$05
	dc.b	$04
	; 80-61
	dc.b	$06
	dc.b	$01
	dc.b	$0A
	dc.b	$07
	dc.b	$02
	dc.b	$01
	dc.b	$80
	dc.b	$40
	dc.b	$20
	dc.b	$50
	dc.b	$C8
	dc.b	$D8
	dc.b	$B4
	dc.b	$4C
	dc.b	$AC
	dc.b	$2A
	dc.b	$B6
	dc.b	$D2
	dc.b	$C3
	dc.b	$40
	; 100-81
	dc.b	$00
	dc.b	$00
	dc.b	$80
	dc.b	$40
	dc.b	$20
	dc.b	$30
	dc.b	$88
	dc.b	$4C
	dc.b	$A2
	dc.b	$93
	dc.b	$A8
	dc.b	$64
	dc.b	$6A
	dc.b	$5B
	dc.b	$9C
	dc.b	$DD
	dc.b	$AA
	dc.b	$77
	dc.b	$AA
	dc.b	$DD
	; 120-101
	dc.b	$AA
	dc.b	$77
	dc.b	$AA
	dc.b	$DC
	dc.b	$A8
	dc.b	$70
	dc.b	$A8
	dc.b	$DC
	dc.b	$AA
	dc.b	$77
	dc.b	$AA
	dc.b	$DD
	dc.b	$AA
	dc.b	$77
	dc.b	$AA
	dc.b	$DD
	dc.b	$AB
	dc.b	$77
	dc.b	$A7
	dc.b	$DB
	; 140-121
	dc.b	$AA
	dc.b	$75
	dc.b	$A9
	dc.b	$DC
	dc.b	$B0
	dc.b	$64
	dc.b	$8E
	dc.b	$D5
	dc.b	$BB
	dc.b	$55
	dc.b	$A8
	dc.b	$FC
	dc.b	$B0
	dc.b	$24
	dc.b	$0E
	dc.b	$15
	dc.b	$3B
	dc.b	$15
	dc.b	$28
	dc.b	$3C
	; 160-141
	dc.b	$30
	dc.b	$20
	dc.b	$0C
	dc.b	$18
	dc.b	$1C
	dc.b	$08
	dc.b	$2C
	dc.b	$20
	dc.b	$04
	dc.b	$11
	dc.b	$4A ;
	dc.b	$22
	dc.b	$29
	dc.b	$5D
	dc.b	$9A
	dc.b	$9A
	dc.b	$A7
	dc.b	$77
	dc.b	$AA
	dc.b	$DA
	; 180-161
	dc.b	$BD
	dc.b	$79
	dc.b	$B6
	dc.b	$AE
	dc.b	$DF
	dc.b	$DE
	dc.b	$AD
	dc.b	$AA
	dc.b	$77
	dc.b	$72
	dc.b	$AD
	dc.b	$AE
	dc.b	$DF
	dc.b	$CE
	dc.b	$B6
	dc.b	$B9
	dc.b	$7D
	dc.b	$BA
	dc.b	$DA
	dc.b	$A7
	; 200-181
	dc.b	$77
	dc.b	$AA
	dc.b	$DA
	dc.b	$BD
	dc.b	$79
	dc.b	$B6
	dc.b	$AE
	dc.b	$DF
	dc.b	$DE
	dc.b	$AC
	dc.b	$A8
	dc.b	$70
	dc.b	$70
	dc.b	$A0
	dc.b	$A0
	dc.b	$C0
	dc.b	$C0
	dc.b	$80
	dc.b	$80
	dc.b	$00

	;==========
	ORG	$FA00
	;==========
Level_0_WhiteData_PF1_4
	; 20-0
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	; 40-21
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$D5
	dc.b	$15
	dc.b	$B5
	dc.b	$45
	dc.b	$ED
	dc.b	$51
	dc.b	$BB
	dc.b	$54
	dc.b	$EE
	dc.b	$55
	dc.b	$AB
	dc.b	$55
	dc.b	$EA
	dc.b	$55
	dc.b	$AA
	dc.b	$55
	dc.b	$EE
	; 60-41
	dc.b	$55
	dc.b	$BA
	dc.b	$55
	dc.b	$EE
	dc.b	$54
	dc.b	$BA
	dc.b	$52
	dc.b	$EA
	dc.b	$56
	dc.b	$B8
	dc.b	$55
	dc.b	$EE
	dc.b	$54
	dc.b	$BA
	dc.b	$52
	dc.b	$EA
	dc.b	$56
	dc.b	$B8
	dc.b	$55
	dc.b	$EE
	; 80-61
	dc.b	$55
	dc.b	$BB
	dc.b	$55
	dc.b	$EE
	dc.b	$55
	dc.b	$BB
	dc.b	$55
	dc.b	$EE
	dc.b	$54
	dc.b	$B8
	dc.b	$50
	dc.b	$E0
	dc.b	$40
	dc.b	$80
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	; 100-81
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$01
	dc.b	$03
	dc.b	$04
	dc.b	$08
	dc.b	$11
	dc.b	$3B
	dc.b	$15
	dc.b	$0E
	dc.b	$05
	dc.b	$03
	; 120-101
	dc.b	$01
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$01
	dc.b	$03
	dc.b	$01
	dc.b	$02
	dc.b	$07
	dc.b	$07
	dc.b	$07
	dc.b	$0B
	dc.b	$1D
	dc.b	$1E
	; 140-121
	dc.b	$1F
	dc.b	$2F
	dc.b	$77
	dc.b	$7B
	dc.b	$7D
	dc.b	$BE
	dc.b	$DE
	dc.b	$EC
	dc.b	$F5
	dc.b	$FA
	dc.b	$FB
	dc.b	$72
	dc.b	$B4
	dc.b	$44
	dc.b	$CC
	dc.b	$54
	dc.b	$91
	dc.b	$6A
	dc.b	$E0
	dc.b	$64
	; 160-141
	dc.b	$54
	dc.b	$BA
	dc.b	$B8
	dc.b	$58
	dc.b	$54
	dc.b	$EE
	dc.b	$E5
	dc.b	$5B
	dc.b	$5D
	dc.b	$BE
	dc.b	$9D
	dc.b	$6D
	dc.b	$73
	dc.b	$FB
	dc.b	$75
	dc.b	$B5
	dc.b	$4E
	dc.b	$EE
	dc.b	$55
	dc.b	$35
	; 180-161
	dc.b	$7B
	dc.b	$B3
	dc.b	$2D
	dc.b	$5D
	dc.b	$1E
	dc.b	$2D
	dc.b	$0B
	dc.b	$15
	dc.b	$06
	dc.b	$05
	dc.b	$13
	dc.b	$15
	dc.b	$2E
	dc.b	$0D
	dc.b	$4D
	dc.b	$53
	dc.b	$BB
	dc.b	$35
	dc.b	$35
	dc.b	$47
	; 200-181
	dc.b	$EE
	dc.b	$55
	dc.b	$B5
	dc.b	$7B
	dc.b	$F3
	dc.b	$6D
	dc.b	$5D
	dc.b	$BE
	dc.b	$BC
	dc.b	$58
	dc.b	$50
	dc.b	$E0
	dc.b	$E0
	dc.b	$40
	dc.b	$40
	dc.b	$80
	dc.b	$80
	dc.b	$00
	dc.b	$00
	dc.b	$00


	;==========
	ORG	$FB00
	;==========

Level_0_BlueData_PF1_1
	; 20-0
	dc.b	$8A
	dc.b	$82
	dc.b	$8A
	dc.b	$82
	dc.b	$8A
	dc.b	$82
	dc.b	$8A
	dc.b	$82
	dc.b	$8A
	dc.b	$82
	dc.b	$8A
	dc.b	$82
	dc.b	$8A
	dc.b	$82
	dc.b	$8A
	dc.b	$82
	dc.b	$8A
	dc.b	$82
	dc.b	$8A
	dc.b	$82
	; 40-21
	dc.b	$8A
	dc.b	$82
	dc.b	$8A
	dc.b	$82
	dc.b	$8A
	dc.b	$82
	dc.b	$8A
	dc.b	$83
	dc.b	$8A
	dc.b	$D4
	dc.b	$88
	dc.b	$20
	dc.b	$50
	dc.b	$89
	dc.b	$84
	dc.b	$42
	dc.b	$21
	dc.b	$50
	dc.b	$C8
	dc.b	$55
	; 60-41
	dc.b	$22
	dc.b	$00
	dc.b	$00
	dc.b	$25
	dc.b	$63
	dc.b	$41
	dc.b	$00
	dc.b	$22
	dc.b	$66
	dc.b	$44
	dc.b	$00
	dc.b	$02
	dc.b	$06
	dc.b	$05
	dc.b	$01
	dc.b	$01
	dc.b	$09
	dc.b	$09
	dc.b	$29
	dc.b	$29
	; 80-61
	dc.b	$A9
	dc.b	$A9
	dc.b	$A8
	dc.b	$A8
	dc.b	$AA
	dc.b	$A8
	dc.b	$A8
	dc.b	$A8
	dc.b	$A8
	dc.b	$A8
	dc.b	$A9
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	; 100-81
	dc.b	$AA
	dc.b	$AB
	dc.b	$AA
	dc.b	$AC
	dc.b	$A8
	dc.b	$B2
	dc.b	$A2
	dc.b	$C6
	dc.b	$8C
	dc.b	$18
	dc.b	$30
	dc.b	$A0
	dc.b	$C0
	dc.b	$80
	dc.b	$80
	dc.b	$80
	dc.b	$A0
	dc.b	$A0
	dc.b	$A8
	dc.b	$A8
	; 120-101
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AB
	dc.b	$AA
	dc.b	$A8
	dc.b	$A8
	dc.b	$AC
	dc.b	$A8
	dc.b	$A8
	dc.b	$A0
	dc.b	$B0
	dc.b	$A0
	dc.b	$A0
	; 140-121
	dc.b	$A0
	dc.b	$C0
	dc.b	$A0
	dc.b	$A0
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$02
	dc.b	$12
	dc.b	$16
	dc.b	$12
	dc.b	$12
	dc.b	$09
	dc.b	$08
	dc.b	$04
	; 160-141
	dc.b	$04
	dc.b	$02
	dc.b	$02
	dc.b	$01
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$40
	; 180-161
	dc.b	$40
	dc.b	$20
	dc.b	$20
	dc.b	$90
	dc.b	$10
	dc.b	$88
	dc.b	$28
	dc.b	$A4
	dc.b	$24
	dc.b	$B4
	dc.b	$24
	dc.b	$A4
	dc.b	$08
	dc.b	$C8
	dc.b	$10
	dc.b	$90
	dc.b	$20
	dc.b	$20
	dc.b	$40
	dc.b	$80
	; 200-181
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00

	;==========
	ORG	$FC00
	;==========
Level_0_BlueData_PF2_2
	; 20-0
	dc.b	$51
	dc.b	$51
	dc.b	$51
	dc.b	$51
	dc.b	$51
	dc.b	$51
	dc.b	$51
	dc.b	$51
	dc.b	$51
	dc.b	$51
	dc.b	$51
	dc.b	$51
	dc.b	$51
	dc.b	$51
	dc.b	$51
	dc.b	$51
	dc.b	$51
	dc.b	$51
	dc.b	$51
	dc.b	$51
	; 40-21
	dc.b	$51
	dc.b	$51
	dc.b	$51
	dc.b	$51
	dc.b	$51
	dc.b	$5B
	dc.b	$51
	dc.b	$60
	dc.b	$40
	dc.b	$80
	dc.b	$04
	dc.b	$04
	dc.b	$15
	dc.b	$15
	dc.b	$55
	dc.b	$56
	dc.b	$54
	dc.b	$59
	dc.b	$51
	dc.b	$60
	; 60-41
	dc.b	$40
	dc.b	$80
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$02
	dc.b	$03
	dc.b	$0A
	dc.b	$0C
	dc.b	$28
	dc.b	$28
	dc.b	$AA
	dc.b	$6A
	dc.b	$2A
	dc.b	$1A
	dc.b	$0A
	dc.b	$06
	dc.b	$02
	dc.b	$00
	dc.b	$01
	; 80-61
	dc.b	$00
	dc.b	$00
	dc.b	$40
	dc.b	$80
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$02
	dc.b	$04
	dc.b	$0D
	dc.b	$15
	dc.b	$35
	dc.b	$55
	dc.b	$D5
	dc.b	$55
	dc.b	$35
	dc.b	$15
	dc.b	$0D
	dc.b	$05
	dc.b	$03
	; 100-81
	dc.b	$01
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	; 120-101
	dc.b	$01
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	; 140-121
	dc.b	$00
	dc.b	$08
	dc.b	$10
	dc.b	$3C
	dc.b	$74
	dc.b	$62
	dc.b	$40
	dc.b	$00
	dc.b	$00
	dc.b	$08
	dc.b	$54
	dc.b	$7E
	dc.b	$77
	dc.b	$63
	dc.b	$41
	dc.b	$00
	dc.b	$00
	dc.b	$08
	dc.b	$55
	dc.b	$7F
	; 160-141
	dc.b	$77
	dc.b	$6B
	dc.b	$51
	dc.b	$3C
	dc.b	$7F
	dc.b	$3E
	dc.b	$3E
	dc.b	$5C
	dc.b	$5C
	dc.b	$08
	dc.b	$88
	dc.b	$30
	dc.b	$20
	dc.b	$40
	dc.b	$40
	dc.b	$80
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	; 180-161
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	; 200-181
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00

	;==========
	ORG	$FD00
	;==========

Level_0_BlueData_PF2_3
	; 20-0
	dc.b	$AA
	dc.b	$A2
	dc.b	$AA
	dc.b	$A2
	dc.b	$AA
	dc.b	$A2
	dc.b	$AA
	dc.b	$A2
	dc.b	$AA
	dc.b	$A2
	dc.b	$AA
	dc.b	$A2
	dc.b	$AA
	dc.b	$A2
	dc.b	$AA
	dc.b	$A2
	dc.b	$AA
	dc.b	$A2
	dc.b	$AA
	dc.b	$A2
	; 40-21
	dc.b	$AA
	dc.b	$A2
	dc.b	$AA
	dc.b	$A2
	dc.b	$AA
	dc.b	$A2
	dc.b	$AA
	dc.b	$A2
	dc.b	$AA
	dc.b	$A2
	dc.b	$AA
	dc.b	$63
	dc.b	$2A
	dc.b	$14
	dc.b	$08
	dc.b	$00
	dc.b	$81
	dc.b	$81
	dc.b	$A0
	dc.b	$A0
	; 60-41
	dc.b	$A9
	dc.b	$A9
	dc.b	$AA
	dc.b	$6A
	dc.b	$2A
	dc.b	$1A
	dc.b	$0A
	dc.b	$02
	dc.b	$06
	dc.b	$02
	dc.b	$02
	dc.b	$00
	dc.b	$00
	dc.b	$01
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$25
	dc.b	$25
	; 80-61
	dc.b	$A6
	dc.b	$A4
	dc.b	$A8
	dc.b	$A0
	dc.b	$A8
	dc.b	$68
	dc.b	$2A
	dc.b	$1A
	dc.b	$0A
	dc.b	$02
	dc.b	$06
	dc.b	$02
	dc.b	$02
	dc.b	$00
	dc.b	$00
	dc.b	$01
	dc.b	$00
	dc.b	$00
	dc.b	$08
	dc.b	$10
	; 100-81
	dc.b	$34
	dc.b	$D6
	dc.b	$35
	dc.b	$95
	dc.b	$8D
	dc.b	$C5
	dc.b	$63
	dc.b	$31
	dc.b	$18
	dc.b	$0C
	dc.b	$06
	dc.b	$03
	dc.b	$01
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	; 120-101
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$02
	dc.b	$06
	dc.b	$02
	dc.b	$01
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	; 140-121
	dc.b	$00
	dc.b	$04
	dc.b	$08
	dc.b	$1E
	dc.b	$3A
	dc.b	$31
	dc.b	$20
	dc.b	$00
	dc.b	$00
	dc.b	$04
	dc.b	$2A
	dc.b	$3F
	dc.b	$3B
	dc.b	$31
	dc.b	$A0
	dc.b	$00
	dc.b	$80
	dc.b	$04
	dc.b	$AA
	dc.b	$3F
	; 160-141
	dc.b	$BB
	dc.b	$35
	dc.b	$A2
	dc.b	$0F
	dc.b	$BF
	dc.b	$1F
	dc.b	$9F
	dc.b	$2E
	dc.b	$AE
	dc.b	$04
	dc.b	$C4
	dc.b	$18
	dc.b	$90
	dc.b	$20
	dc.b	$20
	dc.b	$40
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	; 180-161
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	; 200-181
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00

	;==========
	ORG	$FE00
	;==========
Level_0_BlueData_PF1_4
	; 20-0
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	; 40-21
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$D5
	dc.b	$55
	dc.b	$35
	dc.b	$15
	dc.b	$0D
	dc.b	$05
	dc.b	$03
	dc.b	$01
	dc.b	$00
	dc.b	$08
	dc.b	$18
	dc.b	$10
	dc.b	$00
	dc.b	$08
	dc.b	$19
	dc.b	$11
	dc.b	$00
	; 60-41
	dc.b	$00
	dc.b	$01
	dc.b	$01
	dc.b	$00
	dc.b	$00
	dc.b	$02
	dc.b	$02
	dc.b	$0A
	dc.b	$06
	dc.b	$02
	dc.b	$01
	dc.b	$00
	dc.b	$00
	dc.b	$02
	dc.b	$02
	dc.b	$0A
	dc.b	$06
	dc.b	$02
	dc.b	$01
	dc.b	$00
	; 80-61
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$02
	dc.b	$02
	dc.b	$0A
	dc.b	$0A
	dc.b	$2A
	dc.b	$2A
	dc.b	$2A
	dc.b	$2A
	dc.b	$2A
	dc.b	$2A
	dc.b	$2A
	; 100-81
	dc.b	$2A
	dc.b	$2A
	dc.b	$2A
	dc.b	$2A
	dc.b	$2A
	dc.b	$2A
	dc.b	$2A
	dc.b	$2A
	dc.b	$2B
	dc.b	$2A
	dc.b	$2C
	dc.b	$28
	dc.b	$31
	dc.b	$25
	dc.b	$46
	dc.b	$04
	dc.b	$40
	dc.b	$40
	dc.b	$50
	dc.b	$50
	; 120-101
	dc.b	$54
	dc.b	$54
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$56
	dc.b	$54
	dc.b	$50
	dc.b	$50
	dc.b	$58
	dc.b	$50
	dc.b	$50
	dc.b	$40
	dc.b	$60
	dc.b	$40
	dc.b	$40
	; 140-121
	dc.b	$00
	dc.b	$80
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$05
	dc.b	$25
	dc.b	$2C
	dc.b	$24
	dc.b	$24
	dc.b	$12
	dc.b	$11
	dc.b	$09
	; 160-141
	dc.b	$09
	dc.b	$05
	dc.b	$05
	dc.b	$02
	dc.b	$01
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$80
	; 180-161
	dc.b	$80
	dc.b	$40
	dc.b	$40
	dc.b	$20
	dc.b	$20
	dc.b	$10
	dc.b	$50
	dc.b	$48
	dc.b	$48
	dc.b	$68
	dc.b	$48
	dc.b	$48
	dc.b	$10
	dc.b	$90
	dc.b	$20
	dc.b	$20
	dc.b	$40
	dc.b	$40
	dc.b	$80
	dc.b	$00
	; 200-181
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00

  ELSE IF LEVEL == 1

	;==========
	ORG	$F500
	;==========
Level_1_WhiteData_PF1_2
	; 20-0
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	; 40-21
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AB
	dc.b	$A8
	; 60-41
	dc.b	$AD
	dc.b	$A2
	dc.b	$B7
	dc.b	$8A
	dc.b	$DD
	dc.b	$2A
	dc.b	$77
	dc.b	$2A
	dc.b	$6D
	dc.b	$4A
	dc.b	$57
	dc.b	$52
	dc.b	$55
	dc.b	$55
	dc.b	$54
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	; 80-61
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$56
	dc.b	$55
	dc.b	$5A
	dc.b	$57
	dc.b	$6A
	dc.b	$5E
	dc.b	$6A
	dc.b	$42
	dc.b	$62
	; 95-81
	dc.b	$42
	dc.b	$62
	dc.b	$42
	dc.b	$62
	dc.b	$42
	dc.b	$62
	dc.b	$42
	dc.b	$62
	dc.b	$42
	dc.b	$62
	dc.b	$42
	dc.b	$62
	dc.b	$42
	dc.b	$62
	dc.b	$42


	;==========
	ORG	$F600
	;==========
Level_1_WhiteData_PF2_3
	; 20-0
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	; 40-21
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$D5
	dc.b	$15
	dc.b	$B5
	dc.b	$45
	dc.b	$ED
	dc.b	$51
	dc.b	$BB
	dc.b	$54
	dc.b	$EE
	dc.b	$45
	; 60-41
	dc.b	$83
	dc.b	$41
	dc.b	$C0
	dc.b	$01
	dc.b	$A3
	dc.b	$45
	dc.b	$EE
	dc.b	$55
	dc.b	$BB
	dc.b	$55
	dc.b	$AE
	dc.b	$55
	dc.b	$3B
	dc.b	$55
	dc.b	$4F
	dc.b	$56
	dc.b	$BC
	dc.b	$5A
	dc.b	$E2
	dc.b	$7A
	; 80-61
	dc.b	$CA
	dc.b	$9A
	dc.b	$0A
	dc.b	$1A
	dc.b	$0A
	dc.b	$1A
	dc.b	$0A
	dc.b	$1A
	dc.b	$0A
	dc.b	$1B
	dc.b	$0A
	dc.b	$1B
	dc.b	$0B
	dc.b	$1B
	dc.b	$0A
	dc.b	$1B
	dc.b	$0A
	dc.b	$1B
	dc.b	$0A
	dc.b	$1B
	; 95-81
	dc.b	$0A
	dc.b	$1B
	dc.b	$0A
	dc.b	$1B
	dc.b	$0A
	dc.b	$1B
	dc.b	$0A
	dc.b	$1B
	dc.b	$0A
	dc.b	$1B
	dc.b	$0A
	dc.b	$1B
	dc.b	$0A
	dc.b	$1B
	dc.b	$0A

	;==========
	ORG	$F700
	;==========
Level_1_WhiteData_PF0_4
	; 20-0
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	; 40-21
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$D0
	dc.b	$10
	dc.b	$B0
	dc.b	$40
	dc.b	$E0
	dc.b	$D0
	dc.b	$B0
	dc.b	$D0
	dc.b	$E0
	dc.b	$50
	dc.b	$B0
	dc.b	$50
	dc.b	$E0
	dc.b	$50
	; 60-41
	dc.b	$B0
	dc.b	$50
	dc.b	$E0
	dc.b	$50
	dc.b	$30
	dc.b	$10
	dc.b	$00
	dc.b	$10
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$10
	; 80-61
	dc.b	$30
	dc.b	$50
	dc.b	$F0
	dc.b	$60
	dc.b	$C0
	dc.b	$A0
	dc.b	$20
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	; 95-81
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$20
	dc.b	$20
	dc.b	$A0
	dc.b	$20
	dc.b	$A0
	dc.b	$E0
	dc.b	$80
	dc.b	$C0
	dc.b	$E0
	dc.b	$50
	dc.b	$60

	;==========
	ORG	$F800
	;==========
Level_1_WhiteData_PF1_5
	; 20-0
	dc.b	$A8
	dc.b	$AA
	dc.b	$A8
	dc.b	$AA
	dc.b	$A8
	dc.b	$AA
	dc.b	$A8
	dc.b	$AA
	dc.b	$A8
	dc.b	$AA
	dc.b	$A8
	dc.b	$AA
	dc.b	$A8
	dc.b	$AA
	dc.b	$A8
	dc.b	$AA
	dc.b	$A8
	dc.b	$AA
	dc.b	$A8
	dc.b	$AA
	; 40-21
	dc.b	$AC
	dc.b	$A2
	dc.b	$B5
	dc.b	$8A
	dc.b	$D7
	dc.b	$2E
	dc.b	$5D
	dc.b	$BA
	dc.b	$F4
	dc.b	$A9
	dc.b	$51
	dc.b	$A5
	dc.b	$C5
	dc.b	$95
	dc.b	$25
	dc.b	$95
	dc.b	$D9
	dc.b	$8D
	dc.b	$56
	dc.b	$AB
	; 60-41
	dc.b	$DD
	dc.b	$8A
	dc.b	$17
	dc.b	$0E
	dc.b	$15
	dc.b	$0B
	dc.b	$17
	dc.b	$2A
	dc.b	$15
	dc.b	$27
	dc.b	$1D
	dc.b	$1F
	dc.b	$1B
	dc.b	$2F
	dc.b	$2B
	dc.b	$1D
	dc.b	$1F
	dc.b	$3E
	dc.b	$2F
	dc.b	$1C
	; 80-61
	dc.b	$3D
	dc.b	$1A
	dc.b	$17
	dc.b	$8A
	dc.b	$DD
	dc.b	$AA
	dc.b	$F7
	dc.b	$EA
	dc.b	$1D
	dc.b	$8A
	dc.b	$07
	dc.b	$82
	dc.b	$05
	dc.b	$84
	dc.b	$04
	dc.b	$8C
	dc.b	$14
	dc.b	$AC
	dc.b	$18
	dc.b	$AC
	; 95-81
	dc.b	$5A
	dc.b	$B9
	dc.b	$70
	dc.b	$B0
	dc.b	$60
	dc.b	$E0
	dc.b	$C4
	dc.b	$C0
	dc.b	$80
	dc.b	$80
	dc.b	$10
	dc.b	$20
	dc.b	$12
	dc.b	$38
	dc.b	$18

	;==========
	ORG	$F900
	;==========
Level_1_WhiteData_PF2_6
	; 20-0
	dc.b	$08
	dc.b	$08
	dc.b	$08
	dc.b	$08
	dc.b	$08
	dc.b	$08
	dc.b	$08
	dc.b	$08
	dc.b	$08
	dc.b	$08
	dc.b	$08
	dc.b	$08
	dc.b	$08
	dc.b	$08
	dc.b	$08
	dc.b	$08
	dc.b	$08
	dc.b	$08
	dc.b	$08
	dc.b	$08
	; 40-21
	dc.b	$08
	dc.b	$08
	dc.b	$08
	dc.b	$09
	dc.b	$0B
	dc.b	$09
	dc.b	$08
	dc.b	$0A
	dc.b	$0A
	dc.b	$0A
	dc.b	$0A
	dc.b	$0A
	dc.b	$0A
	dc.b	$0A
	dc.b	$0A
	dc.b	$0A
	dc.b	$0A
	dc.b	$0A
	dc.b	$0A
	dc.b	$0A
	; 60-41
	dc.b	$09
	dc.b	$0B
	dc.b	$06
	dc.b	$0D
	dc.b	$0B
	dc.b	$05
	dc.b	$0E
	dc.b	$05
	dc.b	$0B
	dc.b	$05
	dc.b	$0F
	dc.b	$05
	dc.b	$0B
	dc.b	$05
	dc.b	$0E
	dc.b	$05
	dc.b	$0B
	dc.b	$05
	dc.b	$0E
	dc.b	$05
	; 80-61
	dc.b	$0B
	dc.b	$05
	dc.b	$0E
	dc.b	$05
	dc.b	$0B
	dc.b	$05
	dc.b	$0E
	dc.b	$05
	dc.b	$0B
	dc.b	$05
	dc.b	$0E
	dc.b	$05
	dc.b	$0B
	dc.b	$05
	dc.b	$0E
	dc.b	$04
	dc.b	$08
	dc.b	$06
	dc.b	$05
	dc.b	$04
	; 95-81
	dc.b	$04
	dc.b	$04
	dc.b	$04
	dc.b	$03
	dc.b	$04
	dc.b	$04
	dc.b	$04
	dc.b	$04
	dc.b	$04
	dc.b	$04
	dc.b	$04
	dc.b	$04
	dc.b	$04
	dc.b	$04
	dc.b	$04

	;==========
	ORG	$FA00
	;==========

Level_1_BlueData_PF1_2
	; 20-0
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	; 40-21
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AB
	dc.b	$AA
	; 60-41
	dc.b	$AC
	dc.b	$A8
	dc.b	$B0
	dc.b	$A0
	dc.b	$C0
	dc.b	$80
	dc.b	$00
	dc.b	$00
	dc.b	$40
	dc.b	$40
	dc.b	$50
	dc.b	$50
	dc.b	$54
	dc.b	$55
	dc.b	$54
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	; 80-61
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$56
	dc.b	$54
	dc.b	$58
	dc.b	$50
	dc.b	$60
	dc.b	$42
	dc.b	$42
	dc.b	$4A
	dc.b	$42
	; 95-81
	dc.b	$4A
	dc.b	$42
	dc.b	$4A
	dc.b	$42
	dc.b	$4A
	dc.b	$42
	dc.b	$4A
	dc.b	$42
	dc.b	$4A
	dc.b	$42
	dc.b	$4A
	dc.b	$42
	dc.b	$4A
	dc.b	$42
	dc.b	$4A

	;==========
	ORG	$FB00
	;==========
Level_1_BlueData_PF2_3
	; 20-0
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	; 40-21
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$D5
	dc.b	$55
	dc.b	$35
	dc.b	$15
	dc.b	$0D
	dc.b	$05
	dc.b	$03
	dc.b	$01
	dc.b	$00
	dc.b	$10
	; 60-41
	dc.b	$00
	dc.b	$54
	dc.b	$C6
	dc.b	$54
	dc.b	$28
	dc.b	$90
	dc.b	$00
	dc.b	$A0
	dc.b	$40
	dc.b	$28
	dc.b	$50
	dc.b	$2A
	dc.b	$84
	dc.b	$2A
	dc.b	$D1
	dc.b	$2A
	dc.b	$44
	dc.b	$2A
	dc.b	$12
	dc.b	$AA
	; 80-61
	dc.b	$4A
	dc.b	$AA
	dc.b	$2A
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AB
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AB
	dc.b	$AB
	dc.b	$AB
	dc.b	$AB
	dc.b	$AB
	dc.b	$AB
	; 95-81
	dc.b	$AB
	dc.b	$AB
	dc.b	$AB
	dc.b	$AB
	dc.b	$AB
	dc.b	$AB
	dc.b	$AB
	dc.b	$AB
	dc.b	$AB
	dc.b	$AB
	dc.b	$AB
	dc.b	$AB
	dc.b	$AB
	dc.b	$AB
	dc.b	$AB
	; dc.b	$2A
	; dc.b	$2B
	; dc.b	$2A
	; dc.b	$2A
	; dc.b	$AA

	;==========
	ORG	$FC00
	;==========
Level_1_BlueData_PF0_4
	; 20-0
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	; 40-21
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$50
	dc.b	$D0
	dc.b	$50
	dc.b	$30
	dc.b	$10
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	; 60-41
	dc.b	$00
	dc.b	$80
	dc.b	$00
	dc.b	$A0
	dc.b	$C0
	dc.b	$20
	dc.b	$90
	dc.b	$40
	dc.b	$B0
	dc.b	$40
	dc.b	$10
	dc.b	$60
	dc.b	$50
	dc.b	$A0
	dc.b	$20
	dc.b	$B0
	dc.b	$D0
	dc.b	$50
	dc.b	$60
	dc.b	$80
	; 80-61
	dc.b	$80
	dc.b	$20
	dc.b	$10
	dc.b	$A0
	dc.b	$40
	dc.b	$A0
	dc.b	$20
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	; 95-81
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$A0
	dc.b	$20
	dc.b	$20
	dc.b	$20
	dc.b	$20
	dc.b	$20
	dc.b	$20
	dc.b	$80
	dc.b	$80
	dc.b	$80
	dc.b	$C0

	;==========
	ORG	$FD00
	;==========
Level_1_BlueData_PF1_5
	; 20-0
	dc.b	$A8
	dc.b	$A8
	dc.b	$A8
	dc.b	$A8
	dc.b	$A8
	dc.b	$A8
	dc.b	$A8
	dc.b	$A8
	dc.b	$A8
	dc.b	$A8
	dc.b	$A8
	dc.b	$A8
	dc.b	$A8
	dc.b	$A8
	dc.b	$A8
	dc.b	$A8
	dc.b	$A8
	dc.b	$A8
	dc.b	$A8
	dc.b	$A8
	; 40-21
	dc.b	$AD
	dc.b	$A8
	dc.b	$B2
	dc.b	$A5
	dc.b	$C9
	dc.b	$90
	dc.b	$21
	dc.b	$42
	dc.b	$85
	dc.b	$8A
	dc.b	$56
	dc.b	$2A
	dc.b	$1A
	dc.b	$2A
	dc.b	$5A
	dc.b	$2A
	dc.b	$06
	dc.b	$12
	dc.b	$09
	dc.b	$54
	; 60-41
	dc.b	$22
	dc.b	$55
	dc.b	$A0
	dc.b	$35
	dc.b	$4C
	dc.b	$A5
	dc.b	$4B
	dc.b	$95
	dc.b	$6A
	dc.b	$1D
	dc.b	$C7
	dc.b	$C4
	dc.b	$C5
	dc.b	$92
	dc.b	$96
	dc.b	$4A
	dc.b	$49
	dc.b	$95
	dc.b	$94
	dc.b	$C3
	; 80-61
	dc.b	$90
	dc.b	$6D
	dc.b	$20
	dc.b	$54
	dc.b	$20
	dc.b	$50
	dc.b	$80
	dc.b	$40
	dc.b	$00
	dc.b	$40
	dc.b	$50
	dc.b	$50
	dc.b	$56
	dc.b	$57
	dc.b	$57
	dc.b	$45
	dc.b	$46
	dc.b	$4D
	dc.b	$4A
	dc.b	$19
	; 95-81
	dc.b	$1C
	dc.b	$1E
	dc.b	$9E
	dc.b	$3D
	dc.b	$3A
	dc.b	$3D
	dc.b	$71
	dc.b	$79
	dc.b	$DD
	dc.b	$E9
	dc.b	$CD
	dc.b	$99
	dc.b	$CD
	dc.b	$93
	dc.b	$4D

	;==========
	ORG	$FE00
	;==========
Level_1_BlueData_PF2_6
	; 20-0
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	; 40-21
	dc.b	$05
	dc.b	$05
	dc.b	$06
	dc.b	$04
	dc.b	$01
	dc.b	$05
	dc.b	$06
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	dc.b	$05
	; 60-41
	dc.b	$06
	dc.b	$04
	dc.b	$09
	dc.b	$02
	dc.b	$04
	dc.b	$0A
	dc.b	$00
	dc.b	$0A
	dc.b	$04
	dc.b	$09
	dc.b	$00
	dc.b	$0B
	dc.b	$00
	dc.b	$0B
	dc.b	$01
	dc.b	$08
	dc.b	$04
	dc.b	$0A
	dc.b	$00
	dc.b	$02
	; 80-61
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$01
	dc.b	$03
	dc.b	$03
	dc.b	$05
	dc.b	$07
	dc.b	$06
	; 95-81
	dc.b	$07
	dc.b	$06
	dc.b	$01
	dc.b	$02
	dc.b	$02
	dc.b	$04
	dc.b	$00
	dc.b	$06
	dc.b	$04
	dc.b	$06
	dc.b	$04
	dc.b	$06
	dc.b	$04
	dc.b	$06
	dc.b	$04

  ENDIF

;==============================

	;==========
	ORG	$FF00
	;==========

;==============================

	;==========
	ORG	$FFFC
	;==========

	dc.w	Start ; FFFC-FFFD
	dc.w	Start ; FFFE-FFFF
