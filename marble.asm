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
; $80-$81 = Temp variables
;---------------------
Temp = $80  ; one Word

;---------------------
; $90-$9F = Screen variables
;---------------------


;===================================
; Constants
NTSC = 1
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

	; Clear things out before our draw loop
	LDA	#0
	STA	PF0
	STA	PF1
	STA	PF2

	;==================
	RTS
	;==================


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
	STA	CTRLPF ; non-reflected

	LDY	#(95-1) ; (95*2 = 190 + 2 on top = 192)

	;==========
	STA	WSYNC ; [1]
	;==========

DrawLoopBank1
	;==========
	STA	WSYNC ; [2, 4 = first even frame completed]
	;==========

  IF NTSC
	LDA	#$08	; white ; [2]
  ELSE ; (PAL)
	LDA	#$08	; white ; [2]
  ENDIF
	STA	COLUPF ; [3, 5]

	LDA	Level_1_WhiteData_PF0_1,Y ; [4, 9]
	STA	PF0	; [3, 12]
	LDA	Level_1_WhiteData_PF1_2,Y ; [4, 16]
	STA	PF1	; [3, 19]
	LDA	Level_1_WhiteData_PF2_3,Y ; [4, 23]
	STA	PF2	; [3, 26]
	LDA	Level_1_WhiteData_PF0_4,Y ; [4, 30]
	STA	PF0	; [3, 33]
	NOP
	NOP
	LDA	Level_1_WhiteData_PF1_5,Y ; [4, 37]
	STA	PF1	; [3, 40]
	NOP
	NOP
	LDA	Level_1_WhiteData_PF2_6,Y ; [4, 44]
	STA	PF2	; [3, 47]

	;==========
	STA	WSYNC ; [3, = first odd frame completed]
	;==========
	
  IF NTSC
	LDA	#$88	; blue
  ELSE ; (PAL)
	LDA	#$D8	; blue
  ENDIF
	STA	COLUPF

	LDA	Level_1_BlueData_PF0_1,Y
	STA	PF0
	LDA	Level_1_BlueData_PF1_2,Y
	STA	PF1
	LDA	Level_1_BlueData_PF2_3,Y
	STA	PF2
	LDA	Level_1_BlueData_PF0_4,Y
	STA	PF0
	NOP
	NOP
	LDA	Level_1_BlueData_PF1_5,Y
	STA	PF1
	NOP
	NOP
	LDA	Level_1_BlueData_PF2_6,Y
	STA	PF2


	DEY
	BPL	DrawLoopBank1

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
	STY	PF1
	STY	GRP1
	STY	GRP0
	STY	VDELP1
	STY	ENAM0
	STY	ENAM1
	STY	ENABL	
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

	;==========
	ORG	$FA00
	;==========
Level_1_WhiteData_PF0_1
	; 20-0
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
	; 40-21
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
	; 60-41
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
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	; 95-81
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
	ORG	$FB00
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
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	; 95-81
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
	; 60-41
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
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	; 95-81
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
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	; 95-81
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
	; 60-41
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
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	; 95-81
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
Level_1_BlueData_PF0_1
	; 20-0
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
	; 40-21
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
	; 60-41
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
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	; 95-81
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
	ORG	$FE00
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
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	; 95-81
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
	; 60-41
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
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	; 95-81
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
	ORG	$FF00
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
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	; 95-81
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
	; 60-41
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
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	; 95-81
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

;==============================

	;==========
	ORG	$FFF8
	;==========

	dc.w	Start ; FFF8-FFF9
	dc.w	Start ; FFFA-FFFB
	dc.w	Start ; FFFC-FFFD
	dc.w	Start ; FFFE-FFFF
