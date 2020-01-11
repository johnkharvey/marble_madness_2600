;===================================
; "Marble Madness"
; -- The Beginner Race
;
; (for your Atari 2600)
;===================================

;===================================
; Special Thanks:
;  - grafixbmp
;===================================

;===================================
; Bank Layouts
; Bank 1 = Graphics/kernels
; Bank 2 = Collision detection handling after Bank3/Bank4 table lookup
; Bank 3 = Collision table processing for left side of screen
; Bank 4 = Collision table processing for right side of screen
;===================================

;===================================
; RAM Allocation
;===================================
;---------------------
; $80-$8F = Normal variables
;---------------------
Temp = $80 ; potentially a WORD

;------------
; Allows us to switch banks and go to a location
; in the new bank
;------------
ReturnAddress = $82 ; a WORD

SecondsRemaining = $84
FrameCounter = $85
LevelNumber = $86
LeftNumber = $87	; and $88
RightNumber = $89 ; and $8A
; 0 = start screen / direction select screen
; 1 = level 1 play
; 2 = level 1 win
GamePhase = $8B
NinetyDegrees = $8C ; 0 = 90, 1 = 45
IncreasingCounter = $8D
CollisionStatusFromTable = $8E

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
Player0VPosition2 = $97 ; just a bit used to slow things down

Player0HPositionA = $98
Player0VPositionA = $99
Player0SpeedDown = $9A
Player0SpeedUp = $9B
Player0SpeedLeft = $9C
Player0SpeedRight = $9D

CollisionByte = $9E

MarbleFallStatus = $9F

;---------------------
; $A0-$AF = Marble RAM
;---------------------
P0MarbleRAM = $A0 ; 8 bytes
; Next is $A8

;===================================
; Constants
NTSC = 1
DEBUG = 0
LEVEL = 0
MAXLEVELHEIGHT = 200
ANGLE_PIPE      = %00000000
ANGLE_SLASH     = %01000000
ANGLE_BACKSLASH = %10000000
ANGLE_MINUS     = %11000000
;===================================


;===================================
	processor 6502
	include hdr/vcs.h
;===================================

        MAC JUMP_TABLE ; put this at the start of every bank
        RORG $F000
Bank1
        cmp SelectBank1	; 3 bytes
        jmp Bank1Code	; 3 bytes
Bank2
        cmp SelectBank2	; 3 bytes
        jmp Bank2Code	; 3 bytes
Bank3
        cmp SelectBank3	; 3 bytes
        jmp Bank3Code	; 3 bytes
Bank4
        cmp SelectBank4	; 3 bytes
        jmp Bank4Code	; 3 bytes
        ENDM

;===================================

	MAC BANKS_AND_VECTORS; put this at the end of every bank
	;RORG $FFF8
	RORG $FFF6
SelectBank1 .byte $00
SelectBank2 .byte $00
SelectBank3 .byte $00
SelectBank4 .byte $00
	.word Bank1; NMI
	.word Bank1; RESET
	.word Bank1; IRQ
	ENDM

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
	org	$C000
	;============
	JUMP_TABLE

	org	$C018
	rorg	$F018

;=================
Bank1Code
;=================

	LDA	ReturnAddress+1
	CMP	#>AfterCollision_InBank1
	BNE	TestReturnAddress2
	LDA	ReturnAddress
	CMP	#<AfterCollision_InBank1
	BNE	TestReturnAddress2
	JMP	AfterCollision_InBank1
TestReturnAddress2

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
; Stuff for First screen
	LDA	#0	; startup phase "Marble Madness" screen
	STA	GamePhase
	STA	NinetyDegrees
	STA	FrameCounter ; zero-frame for seconds timer

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
	LDA	#78
	STA	Player0HPosition
	LDA	#189
	STA	Player0VPosition ; these are divided by 2, so pos is 20.
	LDA	#0
	STA	Player0VPosition2 ; 0 or 1, frame counter to slow ball
	STA	Player0HPositionA ; speed
	STA	Player0VPositionA ; speed
	STA	Player0SpeedDown
	STA	Player0SpeedUp
	STA	Player0SpeedLeft
	STA	Player0SpeedRight
	STA	LevelNumber	; level zero
	STA	MarbleFallStatus
	LDA	#$60 ; BCD, so hex
	STA	SecondsRemaining
	LDA	#>Numbers
	STA	RightNumber+1
	STA	LeftNumber+1

;=================
MainLoopBank1
;=================
	JSR	VerticalBlankBank1 ; Execute the vertical blank.
	; different game phases have different kernels
	INC	IncreasingCounter ; always increases
	LDA	GamePhase
	BNE	NotGamePhaseZero
; Game phase zero
	JSR	ResetSelectCheck
	JSR	GameCalcStartScreenBank1 ; Do calculations during Vblank
	JSR	TitleScreenBank1		 ; Draw the screen
	JMP	AfterDrawScreen
NotGamePhaseZero
	CMP	#1
	BEQ	GamePhaseOne
	CMP	#2
	BEQ	GamePhaseTwo
	JMP	AfterDrawScreen
GamePhaseOne
GamePhaseTwo
	JSR	ResetSelectCheck
	JSR	GameCalcBank1	; Do calculations during Vblank
	JSR	DrawScreenBank1	; Draw the screen

AfterDrawScreen
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

	;========================
	; Reset/select pressed
	;========================
ResetSelectCheck
	LDA	SWCHB
	AND	#%00000001
	BNE	ResetNotPressed
	LDX	#$FF
	TXS
  IF NTSC
	LDY	#0
	LDX	#249
  ELSE ; PAL
	LDY	#1
	LDX	#43
  ENDIF
WsyncLoopOnReset
	STA	WSYNC
	DEX
	BNE	WsyncLoopOnReset
	DEY
	BPL	WsyncLoopOnReset
	JMP	GameInitBank1
ResetNotPressed
	RTS
	;=================

;=================
GameCalcStartScreenBank1
;=================
	; if fire button pressed, then game on
	LDA	INPT4
	BMI	LeftFireButtonNotPressed
	LDA	#1
	STA	GamePhase
LeftFireButtonNotPressed
	; if left/right pressed, increase 90orf5
	LDA	SWCHA
	BPL	RightPressedStartScreen
	ROL
	BPL	LeftPressedStartScreen
	JMP	AfterLeftRightStartScreen
RightPressedStartScreen
	LDA	#1
	STA	NinetyDegrees
	JMP	AfterLeftRightStartScreen
LeftPressedStartScreen
	LDA	#0
	STA	NinetyDegrees
AfterLeftRightStartScreen
	RTS

;=================
GameCalcBank1
;=================
	;=================
	; Deal with timer countdown
	;=================
	LDA	SecondsRemaining
	BNE	GameNotOver
	; Game Over
	JMP	NoPlayerMovement
GameNotOver
	INC	FrameCounter
	LDA	FrameCounter
	CMP	#60
	BNE	Not60Frames
	LDA	#0
	STA	FrameCounter
	SED	; BCD
  IF DEBUG = 1
	LDA	SecondsRemaining
	SEC
	SBC	#0
	STA	SecondsRemaining
  ELSE
	LDA	GamePhase
	CMP	#2
	BEQ	WeWonDontDecTimer
	LDA	SecondsRemaining
	SEC
	SBC	#1
	STA	SecondsRemaining
WeWonDontDecTimer
  ENDIF
	CLD	; back to normal math
Not60Frames
	;=================

	;=================
	LDA	SWCHA
	STA	SWCHAStore
	;=================

	;=================
	LDA	MarbleFallStatus
	BEQ	TransformSWCHA
	JMP	InitialJoyCheckDone
	;=================

	;=================
	; Transform SWCHA - based on 45 or 90
	;=================
TransformSWCHA
	LDA	NinetyDegrees
	BEQ	DealWithUp
	; 45 degree transformation
	; right / left / down / up
	LDA	SWCHAStore
	BPL	Transform45Right
	ROL
	BPL	Transform45Left
	ROL
	BPL	Transform45Down
	ROL
	BPL	Transform45Up
	JMP	DealWithUp ; nothing really to do, no direction pressed
Transform45Right
	LDA	#%01011111 ; right goes down and right
	STA	SWCHAStore
	JMP	DealWithUp ; first transformation done
Transform45Left
	LDA	#%10101111 ; left goes up and left
	STA	SWCHAStore
	JMP	DealWithUp ; first transformation done
Transform45Down
	LDA	#%10011111 ; down goes down and left
	STA	SWCHAStore
	JMP	DealWithUp ; first transformation done
Transform45Up
	LDA	#%01101111 ; up goes up and right
	STA	SWCHAStore
	; first transformation done

	;=================
	; Transform SWCHA - based on inertia
	;=================
DealWithUp
	LDA	SWCHAStore
	AND	#%00010000 ; up
	BNE	UpNotPressed
	; Up pressed.  Are we going down?
	LDA	Player0SpeedDown
	BEQ	NotMovingDownAndUpPressed
	DEC	Player0SpeedDown
	DEC	Player0SpeedDown
	JMP	DontMoveUp
NotMovingDownAndUpPressed
	;
	LDA	Player0SpeedUp
	CMP	#$FE ; going top speed?
	BEQ	UpNotPressed
	INC	Player0SpeedUp
	INC	Player0SpeedUp
UpNotPressed
	LDA	Player0SpeedUp
	CLC
	ADC	Player0VPositionA
	STA	Player0VPositionA
	BCC	DontMoveUp
	LDA	SWCHAStore
	AND	#%11101111
	STA	SWCHAStore
	JMP	DealWithDown
DontMoveUp
	LDA	SWCHAStore
	ORA	#%00010000
	STA	SWCHAStore
	;=================
DealWithDown
	LDA	SWCHAStore
	AND	#%00100000 ; down
	BNE	DownNotPressed
	; Down Pressed.  Are we going up?
	LDA	Player0SpeedUp
	BEQ	NotMovingUpAndDownPressed
	DEC	Player0SpeedUp
	DEC	Player0SpeedUp
	JMP	DontMoveDown
NotMovingUpAndDownPressed
	LDA	Player0SpeedDown
	CMP	#$FE
	BEQ	DownNotPressed
	INC	Player0SpeedDown
	INC	Player0SpeedDown
DownNotPressed
	;LDA	Player0SpeedDown
	;CLC
	;ADC	Player0VPositionA
	;STA	Player0VPositionA
	LDA	Player0VPositionA
	SEC
	SBC	Player0SpeedDown
	STA	Player0VPositionA
	;
	BCS	DontMoveDown
	LDA	SWCHAStore
	AND	#%11011111
	STA	SWCHAStore
	JMP	DealWithLeft
DontMoveDown
	LDA	SWCHAStore
	ORA	#%00100000
	STA	SWCHAStore
	;=================
DealWithLeft
	LDA	SWCHAStore
	AND	#%01000000 ; left
	BNE	LeftNotPressed
	; Left pressed.  Are we going right?
	LDA	Player0SpeedRight
	BEQ	NotMovingRightAndLeftPressed
	DEC	Player0SpeedRight
	DEC	Player0SpeedRight
	JMP	DontMoveLeft
NotMovingRightAndLeftPressed
	LDA	Player0SpeedLeft
	CMP	#$FE ; going top speed?
	BEQ	LeftNotPressed
	INC	Player0SpeedLeft
	INC	Player0SpeedLeft
LeftNotPressed
	;LDA	Player0SpeedLeft
	;CLC
	;ADC	Player0HPositionA
	;STA	Player0HPositionA
	LDA	Player0HPositionA
	SEC
	SBC	Player0SpeedLeft
	STA	Player0HPositionA
	;
	BCS	DontMoveLeft
	LDA	SWCHAStore
	AND	#%10111111
	STA	SWCHAStore
	JMP	DealWithRight
DontMoveLeft
	LDA	SWCHAStore
	ORA	#%01000000
	STA	SWCHAStore
	;=================
DealWithRight
	LDA	SWCHAStore
	AND	#%10000000 ; right
	BNE	RightNotPressed
	; Right pressed.  Are we going left?
	LDA	Player0SpeedLeft
	BEQ	NotMovingLeftAndRightPressed
	DEC	Player0SpeedLeft
	DEC	Player0SpeedLeft
	JMP	DontMoveRight
NotMovingLeftAndRightPressed
	;
	LDA	Player0SpeedRight
	CMP	#$FE ; going top speed?
	BEQ	RightNotPressed
	INC	Player0SpeedRight
	INC	Player0SpeedRight
RightNotPressed
	LDA	Player0SpeedRight
	CLC
	ADC	Player0HPositionA
	STA	Player0HPositionA
	BCC	DontMoveRight
	LDA	SWCHAStore
	AND	#%01111111
	STA	SWCHAStore
	JMP	NoMoreDirections
DontMoveRight
	LDA	SWCHAStore
	ORA	#%10000000
	STA	SWCHAStore
NoMoreDirections
	;=================


	;=================
	; Joystick up/down movement
	;=================
RealJoyChecks
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
NoPlayerMovement
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
	LDA	MarbleFallStatus
	BEQ	KeepMarbleSame
	; play a noise
	LDA	#6
	STA	AUDC0
	LDA	#7
	STA	AUDV0
	LDA	MarbleFallStatus
	STA	AUDF0
	; other stuff
	LDA	IncreasingCounter
	AND	#%00000111
	BNE	KeepMarbleSame
	INC	MarbleFallStatus
	LDA	MarbleFallStatus
	CMP	#8
	BNE	KeepMarbleSame
	LDA	#0
	STA	MarbleFallStatus
KeepMarbleSame
	LDX	#7
	LDA	#7
	CLC
	ADC	MarbleFallStatus

	;SEC
	;SBC	#1 ; just in case
	TAY
P0MarbleInRamLoop
	LDA	Marble1,Y
	STA	P0MarbleRAM,X
	DEY
	DEX
	BPL	P0MarbleInRamLoop
	LDA	#0
	STA	P0MarbleRAM
	RTS

	;==================
	RTS
	;==================


	;==================
	;ORG	$C400
	align 256
	;==================
Numbers	; Should be on a page boundary to be effective
NumberZero
	dc.b	%00011000
	dc.b	%00100100
	dc.b	%00100100
	dc.b	%00100100
	dc.b	%00100100
	dc.b	%00100100
	dc.b	%00011000
	dc.b	0

NumberOne
	dc.b	%00011100
	dc.b	%00001000
	dc.b	%00001000
	dc.b	%00001000
	dc.b	%00001000
	dc.b	%00011000
	dc.b	%00001000
	dc.b	0

NumberTwo
	dc.b	%00111100
	dc.b	%00100000
	dc.b	%00100000
	dc.b	%00011000
	dc.b	%00000100
	dc.b	%00100100
	dc.b	%00011000
	dc.b	0

NumberThree
	dc.b	%00111000
	dc.b	%00000100
	dc.b	%00000100
	dc.b	%00011000
	dc.b	%00000100
	dc.b	%00000100
	dc.b	%00111000
	dc.b	0

NumberFour
	dc.b	%00000100
	dc.b	%00000100
	dc.b	%00000100
	dc.b	%00111100
	dc.b	%00100100
	dc.b	%00100100
	dc.b	%00100100
	dc.b	0

NumberFive
	dc.b	%00011000
	dc.b	%00100100
	dc.b	%00000100
	dc.b	%00011000
	dc.b	%00100000
	dc.b	%00100000
	dc.b	%00111100
	dc.b	0

NumberSix
	dc.b	%00011000
	dc.b	%00100100
	dc.b	%00100100
	dc.b	%00111000
	dc.b	%00100000
	dc.b	%00100000
	dc.b	%00011000
	dc.b	0

NumberSeven
	dc.b	%00010000
	dc.b	%00010000
	dc.b	%00001000
	dc.b	%00001000
	dc.b	%00000100
	dc.b	%00000100
	dc.b	%00111100
	dc.b	0

NumberEight
	dc.b	%00011000
	dc.b	%00100100
	dc.b	%00100100
	dc.b	%00011000
	dc.b	%00100100
	dc.b	%00100100
	dc.b	%00011000
	dc.b	0

NumberNine
	dc.b	%00011000
	dc.b	%00100100
	dc.b	%00000100
	dc.b	%00011100
	dc.b	%00100100
	dc.b	%00100100
	dc.b	%00011000
	dc.b	0

Marble1
  IF DEBUG == 1
	; debug marble
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00011000
	dc.b	%00000000
	dc.b	%00011000
	dc.b	%00000000
	dc.b	%00000000
  ELSE
	dc.b	%00000000
	dc.b	%00011000
	dc.b	%00111100
	dc.b	%01111110
	dc.b	%01111110
	dc.b	%01111110
	dc.b	%00111100
	dc.b	%00011000
  ENDIF

	; Marble space
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000
	dc.b	%00000000

JoyStickGfx90
	dc.b	%00000000
	dc.b	%11101110
	dc.b	%00101010
	dc.b	%00101010
	dc.b	%11101010
	dc.b	%10101010
	dc.b	%10101010
	dc.b	%11101110

JoyStickGfx45
	dc.b	%00000000
	dc.b	%00101110
	dc.b	%00100010
	dc.b	%00100010
	dc.b	%11101110
	dc.b	%10101000
	dc.b	%10101000
	dc.b	%10101110

;=================
TitleScreenBank1
;=================
	LDA	INTIM
	BNE	TitleScreenBank1 ; Whew!
	STA	WSYNC	; [0]
	STA	VBLANK  ; Enable drawing again (set vblank to 0)

	LDA	#0
	STA	COLUBK
	LDA	#1
	STA	CTRLPF ; reflected

	LDA	#$0E
	STA	COLUP0
	STA	COLUP1

	LDX	NinetyDegrees
	LDA	IncreasingCounter
	LSR
	AND	#$0F
	STA	COLUP0,X

	NOP
	NOP
	STA	RESP0
	NOP
	NOP
	NOP
	STA	RESP1

	LDY	#10
TitleScreenLoop1
	STA	WSYNC
	DEY
	BNE	TitleScreenLoop1

	;----------
	LDY	#53	; screen logo 54 high (can be 108)
TitleScreenLoop2
	;LDA	#$88
	;STA	COLUBK
	;STA	WSYNC
	;DEY
	;BPL	TitleScreenLoop2

	;==========
	STA	WSYNC ; [3, = first odd frame completed]
	;==========
    IF NTSC
	LDA	#$82	; blue
    ELSE ; (PAL)
	LDA	#$D2	; blue
    ENDIF
	STA	COLUPF ; [3, 5]
	;==== NEW CODE ====
	LDA	Title_PF1_1,Y	; [4, 9]
	STA	PF1	; [3, 12]
	LDA	Title_PF2_2,Y	; [4, 16]
	STA	PF2	; [3, 19]
	NOP		; [2, 21]
	NOP		; [2, 23]
	NOP		; [2, 25]
	NOP		; [2, 27]
	NOP		; [2, 29]
	NOP		; [2, 31]
	LDA	$80	; [3, 34]
	LDA	Title_PF1_4,Y	; [4, 38]
	STA	PF1	; [3, 41]
	LDA	Title_PF2_3,Y	; [4, 45]
	STA	PF2	; [3, 48]
	;== END NEW CODE ==
	DEY
	BPL	TitleScreenLoop2
	;=======================

	LDA	#0
	STA	COLUBK
	STA	PF1
	STA	PF2
	LDY	#40
TitleScreenLoop3
	STA	WSYNC
	DEY
	BNE	TitleScreenLoop3

	LDY	#7
TitleScreenLoop4
	LDA	JoyStickGfx90,Y
	STA	GRP0
	LDA	JoyStickGfx45,Y
	STA	GRP1
	STA	WSYNC
	DEY
	BPL	TitleScreenLoop4

	LDY	#79
TitleScreenLoop5
	STA	WSYNC
	DEY
	BNE	TitleScreenLoop5

	;=================
	JMP	CleanupScreen
	;=================

;=================
DrawScreenBank1
;=================
	LDA	INTIM
	BNE	DrawScreenBank1 ; Whew!
	STA	WSYNC	; [0]
	STA	VBLANK  ; Enable drawing again (set vblank to 0)

	; First 2 lines of kernel are set-up for main stuff.
	;LDA	#0
	;STA	COLUBK
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

	LDA	SecondsRemaining
	AND	#%00001111 ; mask out right digit
	ASL
	ASL
	ASL
	STA	RightNumber
	LDA	SecondsRemaining
	AND	#%11110000 ; mask out left digit
	CLC
	LSR
	STA	LeftNumber


	LDY	#6
	STA	WSYNC ; 1 blue line for score
ScoreLoop
	LDA	(LeftNumber),Y
	;LDA	NumberFive,Y
	STA	GRP0
	LDA	(RightNumber),Y
	;LDA	NumberFive,Y
	STA	GRP1
	STA	WSYNC
	DEY
	BPL	ScoreLoop
	LDA	#0
	STA	GRP0
	STA	GRP1
    IF NTSC
     IF DEBUG = 1
	LDA	#$0E
     ELSE
	LDA	#$82	; blue
     ENDIF
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
CleanupScreen
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
	JMP	Bank3	;(JSR	HandleCollision)
	
	;==================
	; Clear them for next time
	;==================
AfterCollision_InBank1
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
	;ORG	$C700
	align 256
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

Title_PF1_1
	; 1
	dc.b	$00 ; unused for now

	; 20
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AB
	dc.b	$AB
	dc.b	$AB
	dc.b	$AB
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$FB
	dc.b	$FB
	dc.b	$FB
	dc.b	$FB
	dc.b	$FB
	dc.b	$51

	; 8
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00

	; 20
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AB
	dc.b	$AB
	dc.b	$AB
	dc.b	$AB
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$AA
	dc.b	$FB
	dc.b	$FB
	dc.b	$FB
	dc.b	$FB
	dc.b	$FB
	dc.b	$51

	; 5
	dc.b	$00 ; unused for now
	dc.b	$00 ; unused for now
	dc.b	$00 ; unused for now
	dc.b	$00 ; unused for now
	dc.b	$00 ; unused for now

	; 1
	dc.b	$FF ; unused for now

	;==========
	;ORG	$C800
	align 256
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
Title_PF2_2
	; 1
	dc.b	$00 ; unused for now

	; 20
	dc.b	$4D
	dc.b	$5D
	dc.b	$5D
	dc.b	$5D
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$D5
	dc.b	$D5
	dc.b	$D5
	dc.b	$D5
	dc.b	$D5
	dc.b	$55
	dc.b	$5D
	dc.b	$5D
	dc.b	$5D
	dc.b	$4C

	; 8
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00

	; 20
	dc.b	$D5
	dc.b	$D5
	dc.b	$D5
	dc.b	$5D
	dc.b	$4D
	dc.b	$4D
	dc.b	$4D
	dc.b	$5D
	dc.b	$DD
	dc.b	$DD
	dc.b	$D5
	dc.b	$D5
	dc.b	$55
	dc.b	$55
	dc.b	$55
	dc.b	$5D
	dc.b	$5D
	dc.b	$DD
	dc.b	$DD
	dc.b	$CC

	; 5
	dc.b	$00 ; unused for now
	dc.b	$00 ; unused for now
	dc.b	$00 ; unused for now
	dc.b	$00 ; unused for now
	dc.b	$00 ; unused for now

	; 1
	dc.b	$FF ; unused for now

	;==========
	;ORG	$C900
	align 256
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
Title_PF2_3
	; 1
	dc.b	$00 ; unused for now

	; 20
	dc.b	$5D
	dc.b	$5D
	dc.b	$5D
	dc.b	$5D
	dc.b	$50
	dc.b	$D0
	dc.b	$D0
	dc.b	$D0
	dc.b	$D9
	dc.b	$D9
	dc.b	$59
	dc.b	$59
	dc.b	$51
	dc.b	$51
	dc.b	$51
	dc.b	$51
	dc.b	$5D
	dc.b	$5D
	dc.b	$5D
	dc.b	$5D

	; 8
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00

	; 20
	dc.b	$3B
	dc.b	$3B
	dc.b	$BB
	dc.b	$BB
	dc.b	$A2
	dc.b	$A2
	dc.b	$A2
	dc.b	$A2
	dc.b	$A3
	dc.b	$23
	dc.b	$23
	dc.b	$A3
	dc.b	$A2
	dc.b	$A2
	dc.b	$A2
	dc.b	$A2
	dc.b	$A3
	dc.b	$A3
	dc.b	$23
	dc.b	$23

	; 5
	dc.b	$00 ; unused for now
	dc.b	$00 ; unused for now
	dc.b	$00 ; unused for now
	dc.b	$00 ; unused for now
	dc.b	$00 ; unused for now

	; 1
	dc.b	$FF ; unused for now

	;==========
	;ORG	$CA00
	align 256
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
Title_PF1_4
	; 1
	dc.b	$00 ; unused for now

	; 20
	dc.b	$3B
	dc.b	$3B
	dc.b	$3B
	dc.b	$3B
	dc.b	$22
	dc.b	$22
	dc.b	$22
	dc.b	$22
	dc.b	$3B
	dc.b	$3B
	dc.b	$3B
	dc.b	$3B
	dc.b	$08
	dc.b	$08
	dc.b	$08
	dc.b	$08
	dc.b	$3B
	dc.b	$3B
	dc.b	$3B
	dc.b	$3B

	; 8
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00
	dc.b	$00

	; 20
	dc.b	$01
	dc.b	$01
	dc.b	$01
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
	dc.b	$01
	dc.b	$01
	dc.b	$01
	dc.b	$01

	; 5
	dc.b	$00 ; unused for now
	dc.b	$00 ; unused for now
	dc.b	$00 ; unused for now
	dc.b	$00 ; unused for now
	dc.b	$00 ; unused for now

	; 1
	dc.b	$FF ; unused for now

	;==========
	;ORG	$CB00
	align 256
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
	;ORG	$CC00
	align 256
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
	dc.b	$00
	dc.b	$00
	dc.b	$01
	dc.b	$01
	dc.b	$05
	dc.b	$0D
	dc.b	$05
	dc.b	$03
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
	;ORG	$CD00
	align 256
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
	;ORG	$CE00
	align 256
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

;==============================

;==============================
	ORG $CFF6
	BANKS_AND_VECTORS
;==============================

;#####################################################
;#####################################################
;#####################################################
;###                                               ###
;###                  Bank 2 below                 ###
;###                                               ###
;#####################################################
;#####################################################
;#####################################################

	;============
	org	$D000
	;============
	JUMP_TABLE

	org	$D018
	rorg	$F018

;=================
Bank2Code
;=================
	;==================
	; BANK 2 organized
	;==================
	;JSR	HandleHole
	;JSR	HandleCollision
	;JSR	HandleMusic
	;JSR	EdgeOfScreenCollisions
	; Return to Bank 1
	;LDA	#<AfterCollision_InBank1
	;STA	ReturnAddress
	;LDA	#>AfterCollision_InBank1
	;STA	ReturnAddress+1
	;JMP	Bank1

	;==================
	; Check what our table lookup did
	; and process it accordingly
	;==================
	LDA	CollisionStatusFromTable
	CMP	#WIN
	BNE	NotAWinCondition
	; Win condition
	LDA	#2
	STA	GamePhase
  IF DEBUG = 1
	LDA	#$88
	STA	COLUBK
  ELSE
	LDA	#$88
	STA	COLUBK
  ENDIF
	JMP	ReturntoBank1FromBank2
NotAWinCondition
	CMP	#HOLE
	BNE	NotAHoleCondition
	; Service a "fell in hole" situation
  IF DEBUG = 1
	LDA	#$48
	STA	COLUBK
  ELSE
	JSR	FellInHole
  ENDIF
	JMP	ReturntoBank1FromBank2
NotAHoleCondition
	CMP	#BUMP
	BNE	NotABumpCondition
  IF DEBUG = 1
	LDA	#$28
	STA	COLUBK
  ELSE
	;JSR	SwapMomentum
	JSR	AdjustUsBack	; push us back to before we fell
	LDA	#0
	STA	Player0SpeedDown
	STA	Player0SpeedUp
	STA	Player0SpeedLeft
	STA	Player0SpeedRight
	LDA	#$80 ; middle value
	STA	Player0VPositionA
	STA	Player0HPositionA
	LDA	#7
	STA	AUDC0
	STA	AUDV0
	STA	AUDF0
  ENDIF
	JMP	ReturntoBank1FromBank2
NotABumpCondition
	LDA	#0
	STA	COLUBK
	; Disable sounds, since no collision
	STA	AUDV0
	JMP	ReturntoBank1FromBank2

	;=================
	; Return to Bank 1
	;=================
ReturntoBank1FromBank2
	LDA	#<AfterCollision_InBank1
	STA	ReturnAddress
	LDA	#>AfterCollision_InBank1
	STA	ReturnAddress+1
	JMP	Bank1
	;=================

	;==================
	; Collision detection
	;==================
HandleCollision
	; We are going to do 4 checks here.
	; There is a square that's checked.
	; This checks the "upper left corner"
	LDA	Player0HPosition	; initial value 78
	CLC
	ADC	#3 ; go from bit zero to bit 3 (of 7).
	LSR
	LSR ; divide by 4.
	STA	Temp
	LDA	Player0VPosition ; initial value 189
	SEC
	SBC	#1
	TAX
	LDA	CollisionTable1,X	; returns a number.
	STA	CollisionByte
	AND	#%00111111
	CMP	Temp	; returns a number.
	BNE	ULCheckTable2
	JMP	SwapMomentum
ULCheckTable2
	LDA	CollisionTable2,X	; returns a number.
	STA	CollisionByte
	AND	#%00111111
	CMP	Temp	; returns a number.
	BNE	URCollisionCheck
	JMP	SwapMomentum

	; This checks the "upper right corner"
URCollisionCheck
	LDA	Player0HPosition	; initial value 78
	CLC
	ADC	#4 ; go from bit zero to bit 4 (of 7)
	LSR
	LSR ; divide by 4.
	STA	Temp
	LDA	Player0VPosition ; initial value 189
	SEC
	SBC	#1
	TAX
	LDA	CollisionTable1,X	; returns a number.
	STA	CollisionByte
	AND	#%00111111
	CMP	Temp	; returns a number.
	BEQ	SwapMomentum
	LDA	CollisionTable2,X	; returns a number.
	STA	CollisionByte
	AND	#%00111111
	CMP	Temp	; returns a number.
	BEQ	SwapMomentum

	; This checks the "lower left corner"
	LDA	Player0HPosition	; initial value 78
	CLC
	ADC	#3 ; go from bit zero to bit 3 (of 7).
	LSR
	LSR ; divide by 4.
	STA	Temp
	LDA	Player0VPosition ; initial value 189
	SEC
	SBC	#2
	TAX
	LDA	CollisionTable1,X	; returns a number.
	STA	CollisionByte
	AND	#%00111111
	CMP	Temp	; returns a number.
	BEQ	SwapMomentum
	LDA	CollisionTable2,X	; returns a number.
	STA	CollisionByte
	AND	#%00111111
	CMP	Temp	; returns a number.
	BEQ	SwapMomentum

	; This checks the "lower right corner"
	LDA	Player0HPosition	; initial value 78
	CLC
	ADC	#4 ; go from bit zero to bit 4 (of 7)
	LSR
	LSR ; divide by 4.
	STA	Temp
	LDA	Player0VPosition ; initial value 189
	SEC
	SBC	#2
	TAX
	LDA	CollisionTable1,X	; returns a number.
	STA	CollisionByte
	AND	#%00111111
	CMP	Temp	; returns a number.
	BEQ	SwapMomentum
	LDA	CollisionTable2,X	; returns a number.
	STA	CollisionByte
	AND	#%00111111
	CMP	Temp	; returns a number.
	BEQ	SwapMomentum

	; Disable sounds, since no collision
	LDA	#0
	STA	AUDV0
	JMP	CollisionsDone
	;==================

	;==================
	; Second half of above routine.
	; Calls subroutines
	;==================
SwapMomentum
	; We have a hit!  Play a tone
	LDA	#7
	STA	AUDC0
	STA	AUDV0
	STA	AUDF0
	; Figure out direction to bounce based on piece
	LDA	CollisionByte
	AND	#%11000000
	CMP	#ANGLE_PIPE
	BEQ	HandleAnglePipe
	CMP	#ANGLE_SLASH
	BEQ	HandleAngleSlash
	CMP	#ANGLE_BACKSLASH
	BEQ	HandleAngleBackslash
	; No other situations
	; so, fall through to HandleAngleMinus
HandleAngleMinus
	JSR	AdjustUsBack
	JSR	SwapMomentumVert
	JMP	CollisionsDone
HandleAnglePipe
	JSR	AdjustUsBack
	JSR	SwapMomentumHoriz
	JMP	CollisionsDone
HandleAngleSlash
	JSR	AdjustUsBack
	JSR	SwapMomentumSlash
	JMP	CollisionsDone
HandleAngleBackslash
	JSR	AdjustUsBack
	JSR	SwapMomentumBackSlash
	; TEST CODE
	;LDA	#0
	;STA	Player0SpeedLeft
	;STA	Player0SpeedRight
	;STA	Player0SpeedUp
	;STA	Player0SpeedDown
CollisionsDone
	JSR	EdgeOfScreenCollisions
	;==============
	RTS
	;==============

	;==================
	; if we fell in a hole handler
	;==================
HandleHole
	; We are going to do 4 checks here.
	; There is a square that's checked.
	; This checks the "upper left corner"
	LDA	Player0HPosition	; initial value 78
	CLC
	ADC	#3 ; go from bit zero to bit 3 (of 7).
	LSR
	LSR ; divide by 4.
	STA	Temp
	LDA	Player0VPosition ; initial value 189
	SEC
	SBC	#1
	TAX
	LDA	HoleTable1,X	; returns a number.
	STA	CollisionByte
	AND	#%00111111
	CMP	Temp	; returns a number.
	BNE	HoleULCheckTable2
	JMP	FellInHole
HoleULCheckTable2
	LDA	HoleTable2,X	; returns a number.
	STA	CollisionByte
	AND	#%00111111
	CMP	Temp	; returns a number.
	BNE	HoleURCollisionCheck
	JMP	FellInHole

	; This checks the "upper right corner"
HoleURCollisionCheck
	LDA	Player0HPosition	; initial value 78
	CLC
	ADC	#4 ; go from bit zero to bit 4 (of 7)
	LSR
	LSR ; divide by 4.
	STA	Temp
	LDA	Player0VPosition ; initial value 189
	SEC
	SBC	#1
	TAX
	LDA	HoleTable1,X	; returns a number.
	STA	CollisionByte
	AND	#%00111111
	CMP	Temp	; returns a number.
	BEQ	FellInHole
	LDA	HoleTable2,X	; returns a number.
	STA	CollisionByte
	AND	#%00111111
	CMP	Temp	; returns a number.
	BEQ	FellInHole

	; This checks the "lower left corner"
	LDA	Player0HPosition	; initial value 78
	CLC
	ADC	#3 ; go from bit zero to bit 3 (of 7).
	LSR
	LSR ; divide by 4.
	STA	Temp
	LDA	Player0VPosition ; initial value 189
	SEC
	SBC	#2
	TAX
	LDA	HoleTable1,X	; returns a number.
	STA	CollisionByte
	AND	#%00111111
	CMP	Temp	; returns a number.
	BEQ	FellInHole
	LDA	HoleTable2,X	; returns a number.
	STA	CollisionByte
	AND	#%00111111
	CMP	Temp	; returns a number.
	BEQ	FellInHole

	; This checks the "lower right corner"
	LDA	Player0HPosition	; initial value 78
	CLC
	ADC	#4 ; go from bit zero to bit 4 (of 7)
	LSR
	LSR ; divide by 4.
	STA	Temp
	LDA	Player0VPosition ; initial value 189
	SEC
	SBC	#2
	TAX
	LDA	HoleTable1,X	; returns a number.
	STA	CollisionByte
	AND	#%00111111
	CMP	Temp	; returns a number.
	BEQ	FellInHole
	LDA	HoleTable2,X	; returns a number.
	STA	CollisionByte
	AND	#%00111111
	CMP	Temp	; returns a number.
	BEQ	FellInHole

	; Disable sounds, since didn't fall in hole
	LDA	#0
	STA	AUDV0
	;==================
	RTS
	;==================

	;==================
	; Part of the above, hole handler
	;==================
FellInHole
	LDA	#1
	STA	MarbleFallStatus
	JSR	AdjustUsBack	; push us back to before we fell
	LDA	#0
	STA	Player0SpeedDown
	STA	Player0SpeedUp
	STA	Player0SpeedLeft
	STA	Player0SpeedRight
	LDA	#$80 ; middle value
	STA	Player0VPositionA
	STA	Player0HPositionA
	;==================
	RTS
	;==================

	;==================
	; Music stuff
	;==================
HandleMusic
	RTS


	;==============
	; Swap momentum
	;==============
SwapMomentumVert
	LDA	Player0SpeedUp
	BEQ	GoingDown
GoingUp
	LDA	Player0SpeedUp
	STA	Player0SpeedDown
	LDA	#0
	STA	Player0SpeedUp
	DEC	Player0VPosition
	JMP	GoingLeftOrRight
GoingDown
	LDA	Player0SpeedDown
	BEQ	GoingLeftOrRight
	LDA	Player0SpeedDown
	STA	Player0SpeedUp
	LDA	#0
	STA	Player0SpeedDown
	INC	Player0VPosition
GoingLeftOrRight
	LDA	#$80 ; middle value
	STA	Player0VPositionA
	RTS
	;==================


	;==================
SwapMomentumHoriz
	LDA	Player0SpeedLeft
	BEQ	GoingRight
GoingLeft
	LDA	Player0SpeedLeft
	STA	Player0SpeedRight
	LDA	#0
	STA	Player0SpeedLeft
	INC	Player0HPosition
	JMP	RegularCollisionsDone
GoingRight
	LDA	Player0SpeedRight
	BEQ	RegularCollisionsDone
	LDA	Player0SpeedRight
	STA	Player0SpeedLeft
	LDA	#0
	STA	Player0SpeedRight
	DEC	Player0HPosition
RegularCollisionsDone
	LDA	#$80 ; middle value
	STA	Player0HPositionA
	RTS
	;==================

	;==================
	; adjust us to go back one space
	;==================
AdjustUsBack
	LDA	SWCHAStore
	AND	#%00010000
	BNE	DontAdjustDown
	DEC	Player0VPosition
DontAdjustDown
	LDA	SWCHAStore
	AND	#%00100000
	BNE	DontAdjustUp
	INC	Player0VPosition
DontAdjustUp
	LDA	SWCHAStore
	AND	#%01000000
	BNE	DontAdjustLeft
	INC	Player0HPosition
DontAdjustLeft
	LDA	SWCHAStore
	AND	#%10000000
	BNE	DontAdjustRight
	DEC	Player0HPosition
DontAdjustRight
	RTS
	;==================

	;==================
SwapMomentumSlash
	LDA	Player0SpeedDown
	STA	Temp
	LDA	Player0SpeedLeft
	STA	Player0SpeedDown
	LDA	Temp
	STA	Player0SpeedLeft

	LDA	Player0SpeedUp
	STA	Temp
	LDA	Player0SpeedRight
	STA	Player0SpeedUp
	LDA	Temp
	STA	Player0SpeedRight

	;INC	Player0HPosition
	;DEC	Player0HPosition
	LDA	#$80 ; middle value
	STA	Player0HPositionA
	RTS
	;==================

	;==================
SwapMomentumBackSlash
	LDA	Player0SpeedUp
	STA	Temp
	LDA	Player0SpeedLeft
	STA	Player0SpeedUp
	LDA	Temp
	STA	Player0SpeedLeft

	LDA	Player0SpeedDown
	STA	Temp
	LDA	Player0SpeedRight
	STA	Player0SpeedDown
	LDA	Temp
	STA	Player0SpeedRight

	;INC	Player0HPosition
	;DEC	Player0HPosition
	LDA	#$80 ; middle value
	STA	Player0HPositionA
	RTS
	;==================

	;==================
EdgeOfScreenCollisions
	; Edge of screen collisions
	LDA	Player0HPosition ; initial value 78
	CMP	#16	; left-hand extrema
	BEQ	LeftEdgeCollision
	CMP	#15	; just in case
	BEQ	LeftEdgeCollision
	JMP	NoLeftEdgeCollision
LeftEdgeCollision
	; Swap momentum
	;LDA	Player0SpeedLeft
	;STA	Player0SpeedRight
	LDA	#0
	STA	Player0SpeedLeft
	LDA	#16
	STA	Player0HPosition
	JMP	NoRightEdgeCollision
NoLeftEdgeCollision
	LDA	Player0HPosition ; initial value 78
	CMP	#136	; right-hand extrema
	BEQ	RightEdgeCollision
	CMP	#137	; just in case
	BEQ	RightEdgeCollision
	JMP	NoRightEdgeCollision
RightEdgeCollision
	; Swap momentum
	;LDA	Player0SpeedRight
	;STA	Player0SpeedLeft
	LDA	#0
	STA	Player0SpeedRight
	LDA	#136
	STA	Player0HPosition
NoRightEdgeCollision
	RTS
	;==================


;==============================

	;==========
	;ORG	$DD00
	align 256
	;==========
CollisionTable2
HoleTable1
	; 20-1
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 40-21
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	4
	dc.b	0
	; 60-41
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 80-61
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 100-81
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 160-141
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0

;==============================

	;==========
	;ORG	$DE00
	align 256
	;==========
CollisionTable1
	; 20-1
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 40-21
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 60-41
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 80-61
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 100-81
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	7+ANGLE_PIPE
	dc.b	7+ANGLE_PIPE
	dc.b	7+ANGLE_SLASH
	dc.b	8+ANGLE_SLASH
	dc.b	8+ANGLE_SLASH
	dc.b	9+ANGLE_SLASH
	; 160-141
	dc.b	9+ANGLE_SLASH
	dc.b	10+ANGLE_SLASH
	dc.b	10+ANGLE_SLASH
	dc.b	11+ANGLE_SLASH
	dc.b	12+ANGLE_SLASH
	dc.b	13+ANGLE_SLASH
	dc.b	13+ANGLE_SLASH
	dc.b	14+ANGLE_SLASH
	dc.b	14+ANGLE_SLASH
	dc.b	15+ANGLE_SLASH
	dc.b	15+ANGLE_SLASH
	dc.b	16+ANGLE_SLASH
	dc.b	17+ANGLE_SLASH
	dc.b	18+ANGLE_SLASH
	dc.b	18+ANGLE_SLASH
	dc.b	19+ANGLE_SLASH
	dc.b	20+ANGLE_MINUS
	dc.b	0
	dc.b	4+ANGLE_SLASH
	dc.b	5+ANGLE_SLASH
	; 180-161
	dc.b	5+ANGLE_SLASH
	dc.b	6+ANGLE_SLASH
	dc.b	6+ANGLE_SLASH
	dc.b	7+ANGLE_SLASH
	dc.b	7+ANGLE_SLASH
	dc.b	8+ANGLE_SLASH
	dc.b	8+ANGLE_SLASH
	dc.b	9+ANGLE_SLASH
	dc.b	9+ANGLE_PIPE
	dc.b	9+ANGLE_PIPE
	dc.b	9+ANGLE_PIPE
	dc.b	9+ANGLE_BACKSLASH
	dc.b	8+ANGLE_BACKSLASH
	dc.b	8+ANGLE_BACKSLASH
	dc.b	7+ANGLE_BACKSLASH
	dc.b	7+ANGLE_BACKSLASH
	dc.b	6+ANGLE_BACKSLASH
	dc.b	6+ANGLE_BACKSLASH
	dc.b	5+ANGLE_BACKSLASH
	dc.b	4+ANGLE_BACKSLASH
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0

;==============================

	;==========
	;ORG	$DF00
	align 256
	;==========
HoleTable2
	; 20-1
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 40-21
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	4
	dc.b	0
	; 60-41
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 80-61
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 100-81
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	33+ANGLE_BACKSLASH
	dc.b	33+ANGLE_BACKSLASH
	dc.b	33+ANGLE_BACKSLASH
	dc.b	32+ANGLE_BACKSLASH
	dc.b	32+ANGLE_BACKSLASH
	dc.b	31+ANGLE_BACKSLASH
	; 160-141
	dc.b	31+ANGLE_BACKSLASH
	dc.b	30+ANGLE_BACKSLASH
	dc.b	30+ANGLE_BACKSLASH
	dc.b	29+ANGLE_BACKSLASH
	dc.b	28+ANGLE_BACKSLASH
	dc.b	27+ANGLE_BACKSLASH
	dc.b	27+ANGLE_BACKSLASH
	dc.b	26+ANGLE_BACKSLASH
	dc.b	26+ANGLE_BACKSLASH
	dc.b	25+ANGLE_BACKSLASH
	dc.b	25+ANGLE_BACKSLASH
	dc.b	24+ANGLE_BACKSLASH
	dc.b	23+ANGLE_BACKSLASH
	dc.b	22+ANGLE_BACKSLASH
	dc.b	22+ANGLE_BACKSLASH
	dc.b	21+ANGLE_BACKSLASH
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	35+ANGLE_BACKSLASH
	; 180-161
	dc.b	35+ANGLE_BACKSLASH
	dc.b	34+ANGLE_BACKSLASH
	dc.b	34+ANGLE_BACKSLASH
	dc.b	33+ANGLE_BACKSLASH
	dc.b	33+ANGLE_BACKSLASH
	dc.b	32+ANGLE_BACKSLASH
	dc.b	32+ANGLE_BACKSLASH
	dc.b	31+ANGLE_BACKSLASH
	dc.b	31+ANGLE_PIPE
	dc.b	31+ANGLE_PIPE
	dc.b	31+ANGLE_PIPE
	dc.b	31+ANGLE_SLASH
	dc.b	32+ANGLE_SLASH
	dc.b	32+ANGLE_SLASH
	dc.b	33+ANGLE_SLASH
	dc.b	33+ANGLE_SLASH
	dc.b	34+ANGLE_SLASH
	dc.b	34+ANGLE_SLASH
	dc.b	35+ANGLE_SLASH
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0

;==============================
	ORG $DFF6
	BANKS_AND_VECTORS
;==============================

;#####################################################
;#####################################################
;#####################################################
;###                                               ###
;###                  Bank 3 below                 ###
;###                                               ###
;#####################################################
;#####################################################
;#####################################################

	;============
	org	$E000
	;============
	JUMP_TABLE

	org	$E018
	rorg	$F018

;=================
Bank3Code
;=================


; New constants
FLAT = %00000000
BUMP = %00000001
HOLE = %00000010
WIN  = %00000011

	; Set to a default value
	LDA	#$FF
	STA	CollisionStatusFromTable

	LDA	Player0HPosition	; initial value 78
	SEC
	SBC	#16
	CLC
	ADC	#3 ; go from bit zero to bit 3 (of 7).
	LSR
	LSR ; divide by 4.
	TAX
	; number is 0-31
	; If 0-15, do in this bank
	; If 16-31, do in next bank
	CMP	#16
	BPL	GotoBank4FromBank3

	; Ok, if we fell through to here, we're doing a lookup

	LDA	ProcessTableLSBBank3,X
	STA	ReturnAddress
	LDA	ProcessTableMSBBank3,X
	STA	ReturnAddress+1
	; Get the line
	LDA	Player0VPosition ; initial value 189
	SEC
  IF DEBUG = 1
	SBC	#1
  ELSE
	SBC	#3
  ENDIF
	TAY
	LDA	(ReturnAddress),Y
	;===========
	BNE	FinishedProcessingBank3
	; otherwise, check if we're an edge-case
	LDA	Player0HPosition	; initial value 78
	SEC
	SBC	#16
	CLC
	ADC	#4 ; go from bit zero to bit 4 (of 7).
	LSR
	LSR ; divide by 4.
	TAX
	; number is 0-31
	; If 0-15, do in this bank
	; If 16-31, do in next bank
	CMP	#16
	BPL	GotoBank4FromBank3
	; number is 0-15 for bank4 checks
	LDA	ProcessTableLSBBank3,X
	STA	ReturnAddress
	LDA	ProcessTableMSBBank3,X
	STA	ReturnAddress+1
	; Get the line
	LDA	Player0VPosition ; initial value 189
	SEC
  IF DEBUG = 1
	SBC	#1
  ELSE
	SBC	#3
  ENDIF
	TAY
	LDA	(ReturnAddress),Y
FinishedProcessingBank3
	;===========
	STA	CollisionStatusFromTable
	JMP	Bank2

	;=================
	; Test Bank 4
	;=================
GotoBank4FromBank3
	JMP	Bank4
	;=================

ProcessTableLSBBank3
	; 1-8
	.byte	#<Column1Information
	.byte	#<Column2Information
	.byte	#<Column3Information
	.byte	#<Column4Information
	.byte	#<Column5Information
	.byte	#<Column6Information
	.byte	#<Column7Information
	.byte	#<Column8Information
	; 9-16
	.byte	#<Column9Information
	.byte	#<Column10Information
	.byte	#<Column11Information
	.byte	#<Column12Information
	.byte	#<Column13Information
	.byte	#<Column14Information
	.byte	#<Column15Information
	.byte	#<Column16Information

ProcessTableMSBBank3
	; 1-8
	.byte	#>Column1Information
	.byte	#>Column2Information
	.byte	#>Column3Information
	.byte	#>Column4Information
	.byte	#>Column5Information
	.byte	#>Column6Information
	.byte	#>Column7Information
	.byte	#>Column8Information
	; 9-16
	.byte	#>Column9Information
	.byte	#>Column10Information
	.byte	#>Column11Information
	.byte	#>Column12Information
	.byte	#>Column13Information
	.byte	#>Column14Information
	.byte	#>Column15Information
	.byte	#>Column16Information

	;=================================
	; Column Information organization
	;=================================
	; Conditions handled (LSB):
	;   Nothing       = %0000
	;   Bump          = %0001
	;   Hole          = %0010
	;   Winning space = %0011
	;   8 angles
	; Conditions handled (MSB):
	;   Slope         = 0-15
	;   Slope         = UR/UL/DL/DR bump
	;==================

; New constants
;FLAT = %00000000
;BUMP = %00000001
;HOLE = %00000010
;WIN  = %00000011


Column1Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 60-41
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 80-61
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 120-101
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 140-121
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 160-141
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE

Column2Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	; 60-41
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	; 80-61
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 120-101
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 140-121
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 160-141
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	BUMP
	; 180-161
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column3Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	BUMP
	dc.b	HOLE
	; 60-41
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	; 80-61
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 120-101
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 140-121
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 160-141
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	BUMP
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column4Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	BUMP
	; 60-41
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 80-61
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 120-101
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 140-121
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	; 160-141
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	BUMP
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column5Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	; 60-41
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 80-61
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	; 120-101
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	; 160-141
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	BUMP
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column6Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	; 60-41
	dc.b	WIN
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 80-61
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	; 120-101
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	; 160-141
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column7Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	; 60-41
	dc.b	WIN
	dc.b	WIN
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 80-61
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 160-141
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column8Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	BUMP
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	; 60-41
	dc.b	WIN
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 80-61
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 160-141
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column9Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	dc.b	WIN
	; 60-41
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 80-61
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 160-141
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column10Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	WIN
	dc.b	0
	dc.b	0
	; 60-41
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	; 80-61
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 160-141
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column11Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	; 60-41
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	; 80-61
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0

	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 100-81
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	BUMP
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 160-141
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column12Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	; 60-41
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	; 80-61
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	; 100-81
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 160-141
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column13Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 60-41
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 80-61
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	; 100-81
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 160-141
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column14Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 60-41
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 80-61
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	; 100-81
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 160-141
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column15Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 60-41
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 80-61
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 100-81
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 160-141
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column16Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 60-41
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 80-61
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 100-81
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 160-141
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

;==============================
	ORG $EFF6
	BANKS_AND_VECTORS
;==============================

;#####################################################
;#####################################################
;#####################################################
;###                                               ###
;###                  Bank 4 below                 ###
;###                                               ###
;#####################################################
;#####################################################
;#####################################################

	;============
	org	$F000
	;============
	JUMP_TABLE

	org	$F018
	rorg	$F018

;=================
Bank4Code
;=================
	LDA	Player0HPosition	; initial value 78
	SEC
	SBC	#16
	CLC
	ADC	#3 ; go from bit zero to bit 3 (of 7).
	LSR
	LSR ; divide by 4.
	; number is 0-31
	; If 0-15, handle in previous bank
	; If 16-31, handle in this bank
	CMP	#16
	BMI	EdgeCaseCheckBank4 ; we take this branch if we got here at middle of screen position case
	;BMI	ReturntoBank2FromBank4
	SEC
	SBC	#16
	TAX
	; number is 0-15 for bank4 checks
	LDA	ProcessTableLSBBank4,X
	STA	ReturnAddress
	LDA	ProcessTableMSBBank4,X
	STA	ReturnAddress+1
	; Get the line
	LDA	Player0VPosition ; initial value 189
	SEC
  IF DEBUG = 1
	SBC	#1
  ELSE
	SBC	#3
  ENDIF
	TAY
	LDA	(ReturnAddress),Y
	;===========
	BNE	FinishedProcessingBank4
	; otherwise, check if we're an edge-case
EdgeCaseCheckBank4
	LDA	Player0HPosition	; initial value 78
	SEC
	SBC	#16
	CLC
	ADC	#4 ; go from bit zero to bit 4 (of 7).
	LSR
	LSR ; divide by 4.
	; number is 0-31
	; If 0-15, handle in previous bank
	; If 16-31, handle in this bank
	CMP	#16
	BMI	ReturntoBank2FromBank4 ; should never hit
	SEC
	SBC	#16
	TAX
	; number is 0-15 for bank4 checks
	LDA	ProcessTableLSBBank4,X
	STA	ReturnAddress
	LDA	ProcessTableMSBBank4,X
	STA	ReturnAddress+1
	; Get the line
	LDA	Player0VPosition ; initial value 189
	SEC
  IF DEBUG = 1
	SBC	#1
  ELSE
	SBC	#3
  ENDIF
	TAY
	LDA	(ReturnAddress),Y
FinishedProcessingBank4
	;===========
	STA	CollisionStatusFromTable
ReturntoBank2FromBank4
	;=================
	; Return to Bank 1
	;=================
	JMP	Bank2
	;=================

ProcessTableLSBBank4
	; 17-24
	.byte	#<Column17Information
	.byte	#<Column18Information
	.byte	#<Column19Information
	.byte	#<Column20Information
	.byte	#<Column21Information
	.byte	#<Column22Information
	.byte	#<Column23Information
	.byte	#<Column24Information
	; 25-32
	.byte	#<Column25Information
	.byte	#<Column26Information
	.byte	#<Column27Information
	.byte	#<Column28Information
	.byte	#<Column29Information
	.byte	#<Column30Information
	.byte	#<Column31Information
	.byte	#<Column32Information
	
ProcessTableMSBBank4
	; 17-24
	.byte	#>Column17Information
	.byte	#>Column18Information
	.byte	#>Column19Information
	.byte	#>Column20Information
	.byte	#>Column21Information
	.byte	#>Column22Information
	.byte	#>Column23Information
	.byte	#>Column24Information
	; 25-32
	.byte	#>Column25Information
	.byte	#>Column26Information
	.byte	#>Column27Information
	.byte	#>Column28Information
	.byte	#>Column29Information
	.byte	#>Column30Information
	.byte	#>Column31Information
	.byte	#>Column32Information
	;=================================
	; Column Information organization
	;=================================
	; Conditions handled (LSB):
	;   Nothing       = %0000
	;   Bump          = %0001
	;   Hole          = %0010
	;   Winning space = %0011
	;   8 angles
	; Conditions handled (MSB):
	;   Slope         = 0-15
	;   Slope         = UR/UL/DL/DR bump


Column17Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 60-41
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	; 80-61
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 100-81
	dc.b	0
	dc.b	HOLE
	dc.b	0
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 160-141
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE

Column18Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 60-41
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	; 80-61
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 160-141
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column19Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	; 60-41
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 80-61
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 160-141
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column20Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	; 60-41
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 80-61
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 160-141
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column21Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 60-41
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 80-61
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 160-141
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column22Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 60-41
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 80-61
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 160-141
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column23Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 60-41
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 80-61
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	BUMP
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 160-141
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column24Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 60-41
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 80-61
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 160-141
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column25Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 60-41
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 80-61
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 160-141
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column26Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 60-41
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	; 80-61
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 160-141
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column27Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 60-41
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	; 80-61
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 120-101
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 160-141
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column28Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 60-41
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	; 80-61
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	; 120-101
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	; 160-141
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column29Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 60-41
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 80-61
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	; 120-101
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	; 140-121
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	; 160-141
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	BUMP
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column30Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 60-41
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 80-61
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 120-101
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 140-121
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	; 160-141
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	BUMP
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column31Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 60-41
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 80-61
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 120-101
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 140-121
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 160-141
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 180-161
	dc.b	0
	dc.b	BUMP
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	BUMP
	dc.b	0
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

Column32Information
	; 20-1
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 40-21
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 60-41
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 80-61
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 100-81
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 120-101
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	; 140-121
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	; 160-141
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	BUMP
	; 180-161
	dc.b	BUMP
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE
	dc.b	BUMP
	dc.b	0
	; 200-181
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	0
	dc.b	HOLE
	dc.b	HOLE
	dc.b	HOLE

;==============================
	ORG	$FFF6
	BANKS_AND_VECTORS
;==============================

