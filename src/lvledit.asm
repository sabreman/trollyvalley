; (C) MIKKO KEINÄNEN 2004-2008
; ROOM CHAR-MAP EDITOR FOR
; THE GAME 'TROLLEY VALLEY'

; THIS SOURCE IS RELEASED UNDER
; GPL LICENSE
;--------------------------------------
; Instructions:
;--------------------------------------
; Z      move left
; X      move right
; K      move up
; M      move down
; ENTER  draw char
; SPACE  clear char
; D      pen down
; U      pen up
; F1     next char
; F3     previous char
; F5     next screen
; F7     previous screen
; S      save raw data
; O      save compressed data
; 1	 Col 1
; 2 	 Col 2
; 3 	 Col 3
; CLR/HOME Clear current screen
;--------------------------------------

; holds value of current drawing char
CURCHR   = $02A8
; holds number of current room to be edited
CURROOM  = $02A9
; total amount of rooms
ROOMS    = $02AA
COL      = $02AB
ROW      = $02AC
COUNT    = $02AD
TEMP     = $02AE
ROWCOUNT = $02AF
OLDCHR   = $02B0
NEWCHR   = $02B1
; drawing state
PEN      = $02B2

TMPDISP = $02b3
TMP2 = $02b4
; last uncompressed level data address
; ATTENTION: EDIT THIS AFTER ADDING MORE
; MEMORY TO LEVEL DATA

LVDATEND = $9E00

; compressed level data start address
CMPDATST = $3000

CHRDATA  = $2800


DATPLO   = $FB
DATPHI   = $FC

; holds cursor location on screen
SCRPLO   = $FD
SCRPHI   = $FE

TMPLO    = $0E
TMPHI    = $0F

SPRLOCX = $d000
SPRLOCY = $d001
SPRDISPLAY = $d015
JOYSTICK = $DC00

         ; sys 2061
         *= $080D

         JSR INIT
	 JSR INITSPR
         JMP MAINLOOP
;--------------------------------------
MAINLOOP

         ; TIMING:

         LDA $D012 ;RASTER BEAM LOCATION
         ;CMP #$FF
         BNE MAINLOOP

         LDX #$FF
         LDY #$22

TIMER0
         LDX #$FF

TIMER1
         DEX
         BNE TIMER1

         DEY
         BNE TIMER0

         JSR READKEY
	 jsr readjoy
	 jsr updispx
	 jsr updispy
	 JSR READNK
         JMP MAINLOOP

;--------------------------------------
; INITIALIZATION:
;--------------------------------------
INIT     LDA #$00
         STA CURROOM
         STA CURCHR
         STA COL
         STA ROW
         STA COUNT
         STA PEN

         LDA #$1F
         STA ROOMS

         JSR LOADCHR
         JSR LOADAT
         JSR DRAWSCR
         JSR SHOWCUR
	 JSR DRWCURCHRPOINTER

         ; JSR DRWTXT

         RTS

DEBUG
         INC $D020
         JMP DEBUG

;--------------------------------------
;--------------------------------------
INITSPR
	; set 1st sprite (sprite 0) on
	lda sprdisplay
	ora #$01	
	sta sprdisplay

	rts

;--------------------------------------
; READKEY
;--------------------------------------
READKEY  LDA $CB

         CMP #$40 ; NO KEYPRESS
         BEQ READKEX

         ; F1 - 04 NEXT CHAR

READKPL  CMP #$04
         BNE READKMIN

         JSR NEXTCHR

         RTS

         ; F3 - 05 PREV CHAR

READKMIN CMP #$05
         BNE READKSPC

         JSR PREVCHR

         RTS

         ; <SPACE> : 60 CLEAR CHAR

READKSPC CMP #$3C
         BNE READKN

         JSR CLEARCHR

         RTS

         ; F5 NEXT ROOM

READKN   CMP #$06
         BNE READKP

         JSR NEXTROOM

         RTS

         ; F7 PREV ROOM

READKP   CMP #$03
         BNE READKS

         JSR PREVROOM

         RTS

         ; S       : SAVE

READKS   CMP #$0D
         BNE READKENT

         JSR SAVE

         RTS

         ; <ENTER> : 1  SET CHAR

READKENT CMP #$01
         BNE READKCLR

         JSR INCHR

         RTS

         ; CLR/HOME       : 51 CLEAR

READKCLR CMP #$33
         BNE READKLF


         JSR CLRSCR

         RTS

         ; LEFT : Z (12)

READKLF  CMP #$0C
         BNE READKRG

         JSR MVLF

         RTS

         ; RIGHT : X (23)

READKRG  CMP #$17
         BNE READKUP

         JSR MVRG

         RTS

         ; UP : K (37)
READKUP  CMP #$25
         BNE READKDW

         JSR MVUP

         RTS

         ; DOWN : M (36)

READKDW  CMP #$24
         BNE READKEO

         JSR MVDW


         RTS

         ; O - KEY
         
READKEO
         CMP #$26
         BNE READKD

         JSR COMPRESS

         RTS
         
         ; D-KEY (PEN DOWN)
READKD
         CMP #$12
         BNE READKU
         
         JSR PENDOWN
         RTS
         
         ; U-KEY (PEN UP)
READKU
         CMP #$1E
         BNE READK1
         
         JSR PENUP
         RTS
READKEX
         RTS

;-------------------------------------
;-------------------------------------
readjoy

 	; move up 
         LDA JOYSTICK
         AND #$01
         BNE JOYDOWN
	;if first bit is set, move up

	 dec SPRLOCY

JOYDOWN
         LDA JOYSTICK
         AND #$02
         BNE JOYLEFT
	;LSR A
	;BCS LEFT
	INC SPRLOCY
JOYLEFT
         LDA JOYSTICK
         AND #$04
         BNE JOYRIGHT
	 DEC SPRLOCX
JOYRIGHT
         LDA JOYSTICK
         AND #$08
         BNE READJOYX
	 INC SPRLOCX

READJOYX  RTS

;-------------------------------------
; Read num keys
;-------------------------------------
READNK
	LDA $CB

         CMP #$40 ; NO KEYPRESS
         BEQ READNKX


	; 1 (Bk-Col)
READK1	
	CMP #$38
	BNE READK2

	JSR CHCOL0
	RTS


	; 2 (Col 1)
READK2	
	CMP #$3B
	BNE READK3

	JSR CHCOL1
	RTS


	; 3 (Col 2)
READK3	
	CMP #$08
	BNE READKEX

	JSR CHCOL2
	RTS

READNKX
	RTS

;--------------------------------------
; Change colour 0
;--------------------------------------
CHCOL0
	INC $D021
	RTS

;--------------------------------------
; Change colour 1
;--------------------------------------
CHCOL1
	INC $D022
	RTS

;--------------------------------------
; Change colour 2
;--------------------------------------
CHCOL2
	INC $D023
	RTS


;--------------------------------------
; PEN UP
;--------------------------------------
PENUP
         LDA PEN
         AND #$FE
         STA PEN
         RTS
         
;--------------------------------------
; PEN DOWN
;--------------------------------------
PENDOWN
         LDA PEN
         ORA #$01
         STA PEN
         JSR INCHR
         RTS

;--------------------------------------
; MOVE LEFT
;--------------------------------------
MVLF     JSR CURCL

         LDA COL
         BEQ MVLFX

         ; DECREASE POINTER

         SEC
         LDA SCRPLO
         SBC #$01
         STA SCRPLO
         LDA SCRPHI
         SBC #$00
         STA SCRPHI

         DEC COL

MVLFX
         JSR CURSOR
         JSR CHKPEN

         RTS
         


;--------------------------------------
; MOVE RIGHT
;--------------------------------------
MVRG     JSR CURCL

         LDA COL
         CMP #$1D
         BEQ MVRGX

         CLC
         LDA SCRPLO
         ADC #$01
         STA SCRPLO

         LDA SCRPHI
         ADC #$00
         STA SCRPHI

         INC COL

MVRGX
         JSR CURSOR
         JSR CHKPEN
         RTS
;--------------------------------------
; MOVE UP
;--------------------------------------
MVUP     JSR CURCL

         LDA ROW
         BEQ MVUPX

         SEC
         LDA SCRPLO
         SBC #$28 ; (40 == ROW)
         STA SCRPLO
         LDA SCRPHI
         SBC #$00
         STA SCRPHI

         DEC ROW

MVUPX
         JSR CURSOR
         JSR CHKPEN
         RTS
;--------------------------------------
; MOVE DOWN
;--------------------------------------
MVDW     JSR CURCL

         LDA ROW
         CMP #$18
         BEQ MVDWX

         CLC
         LDA SCRPLO
         ADC #$28
         STA SCRPLO
         LDA SCRPHI
         ADC #$00
         STA SCRPHI

         INC ROW

         JMP MVDWX

MVDWJ
         SEC
         LDA SCRPLO
         SBC #$C0
         STA SCRPLO
         LDA SCRPHI
         SBC #$00
         STA SCRPHI

         SEC
         LDA SCRPHI
         SBC #$03
         STA SCRPHI

         LDA #$00
         STA ROW

MVDWX
         JSR CURSOR
         JSR CHKPEN
         RTS
         
;--------------------------------------
; CHECK PEN, IF DOWN DRAW
;--------------------------------------
CHKPEN   
         LDA PEN
         LSR A
         BCC CHKPENX
         
         JSR INCHR
CHKPENX
         RTS
         
;--------------------------------------
; SHOW CURSOR LOCATION
;--------------------------------------
CURSOR
         ; if current cursor position
         ; is empty 
         LDY #$00
         LDA (SCRPLO),Y
         CMP #$20
         BEQ CURSINV
         CMP #$A0
         BEQ CURSINV
         
         ; get color memory pointer 
         ; TMPLO/HI for current screen
         ; location
         
         JSR GETCOLM
         
         LDA #$09
         STA (TMPLO),Y

         RTS
         
CURSINV ; ... invert it
         ORA #$80 ;10000000
         STA (SCRPLO),Y
         
         RTS
;--------------------------------------
; INVERT BACK CURSOR LOCATION
;--------------------------------------
CURSOFF
         LDY #$00
         LDA (SCRPLO),Y

         ;... INVERT CHAR BACK
         AND #$7F ;01111111

         STA (SCRPLO),Y

         RTS
;--------------------------------------
; CURSOR CLEAR
;--------------------------------------
CURCL    
         
         ; if current cursor position
         ; is empty
         LDY #$00
         LDA (SCRPLO),Y
         CMP #$20
         BEQ CURCLINV
         CMP #$A0
         BEQ CURCLINV
         
         ; get color memory pointer 
         ; TMPLO/HI for current screen
         ; location
         
         JSR GETCOLM
         
         LDA #$08
         STA (TMPLO),Y

         RTS
         
CURCLINV ; invert back
         AND #$7F; 01111111
         STA (SCRPLO),Y
         
         RTS

;-------------------------------------
; INSERT CHAR
;-------------------------------------
; inserts selected char to cursor 
; position
;-------------------------------------
INCHR    LDA CURCHR
         LDY #$00
         STA (SCRPLO),Y
         JSR CURSOR
         RTS
         
;--------------------------------------
; NEXT CHAR
;--------------------------------------
NEXTCHR
         INC CURCHR
         JSR SHOWCUR
         JSR PENUP
         RTS
         
;--------------------------------------
; PREVIOUS CHAR
;--------------------------------------
PREVCHR
         DEC CURCHR
         JSR SHOWCUR
         JSR PENUP
         RTS
         
;--------------------------------------
; SHOW CURRENT ROOM NO
;--------------------------------------
DISPRMNO 
         LDA CURROOM
         STA $0476
         RTS

;--------------------------------------
; LOAD DATA POINTERS
;--------------------------------------
LOADATP

         LDX CURROOM
         LDA LVDATPLO,X
         STA DATPLO
         LDA LVDATPHI,X
         STA DATPHI

         RTS

;--------------------------------------
; COPY SCREEN TO MEM
;--------------------------------------
SCRMEM

         ; START OF DESTINATION
         ; DATA AREA

         JSR LOADATP

         ; START OF MAP SCREEN MEMORY

         LDA #$00
         STA SCRPLO
         LDA #$04
         STA SCRPHI

         ; COUNTERS

         LDX #$1E
         LDY #$00

         LDA #$18
         STA ROW

SCRMEMLO LDA (SCRPLO),Y

         ; COPY CHAR VALUE

         STA (DATPLO),Y

         ; INCREASE POINTERS

         CLC
         LDA SCRPLO
         ADC #$01
         STA SCRPLO
         LDA SCRPHI
         ADC #$00
         STA SCRPHI

         CLC
         LDA DATPLO
         ADC #$01
         STA DATPLO
         LDA DATPHI
         ADC #$00
         STA DATPHI

         DEX
         BNE SCRMEMLO

         LDA ROW
         BEQ SCRMEMX

         DEC ROW

         LDX #$1E

         CLC
         LDA SCRPLO
         ADC #$0A
         STA SCRPLO
         LDA SCRPHI
         ADC #$00
         STA SCRPHI

         JMP SCRMEMLO

SCRMEMX
         ; ADD END SIGNS
         LDA #$FF

         STA (DATPLO),Y
         INY
         STA (DATPLO),Y

         JSR LOADATP

         RTS

;--------------------------------------
updispx
;--------------------------------------
	lda sprlocx
	sta tmpdisp
	lda #$94
	sta tmplo
	lda #$07
	sta tmphi 
	jsr updisp
	rts
;--------------------------------------
updispy
;--------------------------------------
	lda sprlocy
	sta tmpdisp
	lda #$bd
	sta tmplo
	lda #$07
	sta tmphi 
	jsr updisp
	rts
;--------------------------------------
; updates sprite coordinates to display
; coordinate value must be set to tmpdisp
; and location to screen memory must
; be set to tmplo/hi
;--------------------------------------
updisp
	ldy #$00 ; y is screen memory index (0...2)
	; x keeps count how many times can be divided by
	; 100/10 
	ldx #$00
	sec ; set c-flag for substraction	
	lda tmpdisp
updisplo1
	sta tmp2 ; store result before substraction
	sbc #$64 ; 100
	bcc updisp2 ; got below 0
	inx
	jmp updisplo1

updisp2 
	; show hundreds (x-reg)
	clc
	txa
	adc #$30 ; add $30 to get a real number to display
	sta (TMPLO),y ; screen mem. loc #1
	iny
	ldx #$00
	lda tmp2 ; get the result before it went under 0
	sec
updisplo2
	sta tmp2 ; store result before substraction 
	sbc #$0a ; 10	
	bcc updisp3 ; < 0
	inx
	jmp updisplo2

updisp3 ; show tens (x-reg)	
	clc
	txa
	adc #$30
	sta (TMPLO),y
	iny
	; show rest
	clc
	lda tmp2
	adc #$30
	sta (TMPLO),y
	rts

;--------------------------------------
; SHOW CURRENT CHAR
;--------------------------------------
SHOWCUR
	 ; store current chat to TEMP
         LDA CURCHR
	 STA $049A
 	 STA TEMP

         ; SUBSTACT #$05 FROM CURRENT
         ; CHAR VALUE TO SHOW 5 CHARS
         ; BEFORE AND 5 NEXT IN SCREEN

         SEC
         SBC #$05

	 ; store char to be drawn to y-reg
         TAY
         LDX #$0A

SHOWCULO
         ; next char to screen
         TYA
         STA $041D,X

         CPX #$05
         BNE SHOWCULN

         ; CURRENT SELECTED CHAR IS
         ; COLURED WITH DIFFERENT COLOUR

         LDA #$0A
         STA $D81D,X

         INY
         DEX
         JMP SHOWCULO
         
         ; colourize char
SHOWCULN
         LDA #$0B
         STA $D81D,X

         INY
         DEX
         BNE SHOWCULO

         LDA TEMP
         STA CURCHR

         RTS


;--------------------------------------
;--------------------------------------
DRWCURCHRPOINTER
	; DRAW I to point the current char
	LDA #$09
	STA $044A
	RTS

;--------------------------------------
; CLEAR CURRENT CHAR
;--------------------------------------
CLEARCHR
         ; VALUE FOR EMPTY SPACE
         LDA #$20

         ; TO CURRENT SCREEN LOCATION
         STA (SCRPLO),Y

         JSR CURSOR

         RTS
;--------------------------------------
; NEXT ROOM
;--------------------------------------
NEXTROOM
         JSR CURCL

         JSR SCRMEM

         LDA CURROOM
         CMP ROOMS
         BNE NEXTROOX

         LDA #$FF
         STA CURROOM

NEXTROOX INC CURROOM

         INC $D020

         JSR DRAWSCR
         JSR PENUP

         RTS
;--------------------------------------
; PREVIOUS ROOM
;--------------------------------------
PREVROOM
         JSR CURCL
         JSR SCRMEM

         LDA CURROOM
         BNE PREVROOX

         LDA ROOMS
         STA CURROOM
         INC CURROOM

PREVROOX DEC CURROOM

         JSR DRAWSCR
         JSR PENUP

         RTS
;--------------------------------------
; COMPRESS AND OUTPUT LEVEL DATA
;--------------------------------------
COMPRESS
         ; RESET DATA POINTER TO
         ; START OF LEVEL CHAR DATA
         ; AREA

         LDX #$00
         LDA LVDATPLO,X
         STA DATPLO
         LDA LVDATPHI,X
         STA DATPHI

         ; SET COMPRESSED DATA POINTER

         ;LDA CMPDATLO,X
         LDA #<CMPDATST
         STA TMPLO
         LDA #>CMPDATST
         ;LDA CMPDATHI,X
         STA TMPHI

         LDX #$00
         LDY #$00

         ; CURRENT ROOM NUM FOR DATA

         STY CURROOM

         ; VALUE OF PREV & NEW CHAR

         STY OLDCHR
         STY NEWCHR

         ; COUNTER / HOW MANY CHARS
         ; WITH SAME VALUE

         STY COUNT

COMPRLO
         ; GET CURRENT CHAR VALUE

         LDA (DATPLO),Y
         STA OLDCHR

         ; INCREASE COUNTER

         INC COUNT

         ; IF COUNT 240 WRITE TO MEM

         LDA COUNT
         CMP #$FA
         BEQ COMPRLON

         ; PEEK TO NEXT VALUE
         ; IF DIFFERS WRITE TO MEM...

         INY

         LDA (DATPLO),Y
         CMP OLDCHR
         BNE COMPRLON

         ;... ELSE INCREASE POINTER
         ; AND LOOP TO READ NEXT

         CLC
         LDA DATPLO
         ADC #$01
         STA DATPLO
         LDA DATPHI
         ADC #$00
         STA DATPHI

         LDY #$00

         JMP COMPRLO


         ; IF CHAR VALUE IS DIFFERENT
         ; FROM THE ONE IN TEMP
         ; STORE AMOUNT OF CHARS AND
         ; CHAR VALUE

COMPRLON
         ; JSR DEBUG


         ; INCREASE COUNTER

         LDY #$00

         CLC
         LDA DATPLO
         ADC #$01
         STA DATPLO

         LDA DATPHI
         ADC #$00
         STA DATPHI

         ; STORE HOW MANY INSTANCES OF
         ; CURRENT CHAR

         ; NOW COUNT CONTAINS ACTUAL
         ; COUNT OF LAST CHAR

         LDA COUNT
         STA (TMPLO),Y

         INY

         ; STORE COUNTED CHAR VALUE

         LDA OLDCHR
         STA (TMPLO),Y

         ; RESET Y REG

         LDY #$00

         ; INCREASE COMPRESSED DATA
         ; POINTER VALUE BY 2

         CLC

         LDA TMPLO
         ADC #$02
         STA TMPLO

         LDA TMPHI
         ADC #$00
         STA TMPHI

         ; RESET COUNTER

         LDA #$00
         STA COUNT

         ; INCREASE ROOM COUNT IF $FF

         LDA OLDCHR
         CMP #$FF
         BEQ CMPNROOM

         ; LOOP TO NEXT CHAR

         JMP COMPRLO

         ; INCREASE ROOM COUNT
         ; AND EXIT LOOP IF LAST ROOM

CMPNROOM
         LDA CURROOM

         CMP ROOMS
         BEQ COMPRX

         INC CURROOM

         JMP COMPRLO

COMPRX

         ; SAVE DATA TO DISK

         JSR SAVECDAT

         ; INITIALIZE

         JSR INIT
         JSR PENUP

         RTS
;--------------------------------------
; OUTPUT COMPRESSED DATA
;--------------------------------------
SAVECDAT

         ; SETLFS

         LDA #$0F

         LDX #$08

         LDY #$01

         JSR $FFBA

         ; SETNAM

         LDA #$08

         LDX #<FILENOUT

         LDY #>FILENOUT

         JSR $FFBD

         ; SAVE

         ; STORE START ADDRESS OF
         ; COMPRESSED DATA:

         LDX #$00

         ;LDA CMPDATLO,X
         LDA #<CMPDATST
         STA DATPLO

         ;LDA CMPDATHI,X
         LDA #>CMPDATST
         STA DATPHI

         ; TMPLO/HI CONTAINS LAST ADDRES
         ; FROM COMPRESS ROUTINE

         LDX TMPLO

         LDY TMPHI

         LDA #<DATPLO

         JSR $FFD8

         RTS
;--------------------------------------
; SAVE DATA
;--------------------------------------
; save uncompressed level data
;--------------------------------------
SAVE     ; turn cursor off
         JSR CURSOFF

         ; store screen to memory
         JSR SCRMEM

         ; SETLFS FUNCTION CALL:

         ; - logical file 15
         LDA #$0F

         ; - DEVICE 8
         LDX #$08

         ; - COMMAND#
         LDY #$01

         ; - CALL SETLLFS
         JSR $FFBA

         ; SETNAME FUNCTION CALL

         ; - FILENAME LENGTH
         LDA #$08

         ; - FILENAME LOCATIN LO-BYTE
         LDX #<FILENSAV

         ; - FILENAME LOCATION HI-BYTE
         LDY #>FILENSAV

         ; - CALL SETNAM
         JSR $FFBD

         ; SAVE FUNCTION CALL

         ; - SET DATA START ADDRESS

         LDA LVDATPLO
         STA TMPLO
         LDA LVDATPHI
         STA TMPHI

         ; set data end address
         
         LDX #<LVDATEND
         LDY #>LVDATEND
         
         ; - SET DATA START POINTTER
         ;   LOW BYTE TO ACCU

         LDA #<TMPLO

         ; CALL SAVE

         JSR $FFD8

         JSR INIT
         JSR PENUP
         
         RTS
;--------------------------------------
; CLEAR SCREEN
;--------------------------------------
CLRSCR
         LDX #$00
         LDY #$00

         STY ROWCOUNT

         LDA #$00
         STA TMPLO

         LDA #$04
         STA TMPHI


CLRSCLO
         LDA #$20
         STA (TMPLO),Y

         INY ; INCREASE COL
         CPY #$1E ; CHECK IF 30. COL
         BNE CLRSCLO

         LDY #$00

         ; EXIT LOOP IF LAST ROW

         INX
         CPX #$19
         BEQ CLRSCRX

         ; ADD 40 TO SCREEN MEM POINTER

         CLC
         LDA TMPLO
         ADC #$28
         STA TMPLO
         LDA TMPHI
         ADC #$00
         STA TMPHI

         JMP CLRSCLO

         ;STA $0400,X
         ;STA $04FA,X
         ;STA $05F4,X

         ;DEX
         ;BNE CLRSCLO
CLRSCRX
         JSR PENUP
         RTS
;--------------------------------------
; LOAD CHARS:
;--------------------------------------
LOADCHR
         LDA #$0F
         LDX #$08
         LDY #$00
         JSR $FFBA

         LDA #$03
         LDX #<FILENCHR
         LDY #>FILENCHR
         JSR $FFBD

         LDA #$00
         LDX #<CHRDATA
         LDY #>CHRDATA
         JSR $FFD5

         ; CHARSET LOCATION
         
         ; Set charset location:
         ; - load $D018 to ACC.
         ;   upper 4 bits in $D018 control location
         ;   of the screen memory, lower bits 3, 2 
         ;   and 1 control location of the charset
         ; - do AND 11110001
         ; - do ORA 00001010 to set memory bank
         ;   to $2800 - $2FFF
         ; - store the altered value back to $d018

         LDA $D018
         AND #$F1; 11110001 to clear mem bank bits
         ORA #$0A ; 00001010 to set memory bank
         STA $D018
         
         ; Set multicolor mode (4th bit in $d016)
         ; to $d016

         LDA $D016
         ORA #$10
         STA $D016

         LDA #$0B
         STA $D021

         LDA #$04
         STA $D022

         LDA #$07
         STA $D023

         RTS
;--------------------------------------
; LOAD DATA:
;--------------------------------------
LOADAT
         LDA #$0F
         LDX #$08
         LDY #$01
         JSR $FFBA

         LDA #$05
         LDX #<FILENLVD
         LDY #>FILENLVD
         JSR $FFBD

         LDA #$00
         LDX LVDATPLO
         LDY LVDATPHI
         JSR $FFD5
         RTS
;--------------------------------------
; DRAW SCREEN
;--------------------------------------
DRAWSCR  LDX CURROOM

         LDA #$00
         STA ROWCOUNT

         ; GET DATA ADDRESS TO POINTER

         LDA LVDATPLO,X
         STA DATPLO

         LDA LVDATPHI,X
         STA DATPHI

         ; SCREEN MEMORY START ADDRESS

         JSR RSETSCR

         LDY #$00

         ; COUNTER (CHARS/ROW)
         LDX #$1E

         ; DRAW

DRAWSCLO
         ; CHECK IF ALREADY DRAWN 25 ROW

         LDA ROWCOUNT
         CMP #$19
         BEQ DRAWSCX

         LDA (DATPLO),Y ; GET CHAR

         STA (SCRPLO),Y ; PUT TO SCREEN

         ; INCREASE POINTER
         CLC
         LDA SCRPLO
         ADC #$01
         STA SCRPLO
         LDA SCRPHI
         ADC #$00
         STA SCRPHI

         CLC
         LDA DATPLO
         ADC #$01
         STA DATPLO
         LDA DATPHI
         ADC #$00
         STA DATPHI

         DEX
         BNE DRAWSCLO

         ; NEXT ROW
         LDX #$1E
         INC ROWCOUNT

         CLC
         LDA SCRPLO
         ADC #$0A ; 40 CHARS == ROW
         STA SCRPLO
         LDA SCRPHI
         ADC #$00
         STA SCRPHI

         JMP DRAWSCLO

DRAWSCX
         JSR RSETSCR
         JSR DISPRMNO
         RTS
;--------------------------------------
; RESET CURSOR
;--------------------------------------
RSETSCR  LDA #$00
         STA SCRPLO
         STA ROW
         STA COL

         LDA #$04
         STA SCRPHI

         JSR CURSOR

         RTS
         
;--------------------------------------
; GET COLOR MEMORY POINTER
;--------------------------------------
; This routine sets pointer to color
; memory. Zero page adresses 
; SCRLO/SCRHI must contain
; location in screen memory.
; Color memory pointer is set to
; zero page adresses LOTMP/HITMP.
;
; Screen memory location pointer 
; values scrlo/hi are added with 
; $d400 and sum is stored to 
; lotmp and hitmp screen colour 
; memory pointers. These pointers 
; are used to set colour value
; to right location in screen.
;
;--------------------------------------
GETCOLM
         CLC
         LDA SCRPLO
         ADC #$00
         STA TMPLO
         LDA SCRPHI
         ADC #$D4
         STA TMPHI
         
         RTS
         
;--------------------------------------
; FILENAMES:
;--------------------------------------
FILENLVD .TEXT "LVDAT"
FILENSAV .TEXT "@0:LVDAT"
FILENOUT .TEXT "@0:LVOUT"
FILENCHR .TEXT "CHR"
;--------------------------------------
; ROOM CHAR DATA POINTERS
; IN ORDER (ROOM 01, 02, 03, ETC.)
; LOW-BYTE AND HIGH-BYTE:

;--------------------------------------
LVDATPLO .BYTE $00,$F0,$E0,$D0,$C0,$B0
         .BYTE $A0,$90,$80,$70,$60,$50
         .BYTE $40,$30,$20,$10,$00,$F0
         .BYTE $E0,$D0,$C0,$B0,$A0,$90
         .BYTE $80,$70,$60,$50,$40,$30
         .BYTE $20,$10 ;,$00

LVDATPHI .BYTE $40,$42,$45,$48,$4B,$4E
         .BYTE $51,$54,$57,$5A,$5D,$60
         .BYTE $63,$66,$69,$6C,$6F,$71
         .BYTE $74,$77,$7A,$7D,$80,$83
         .BYTE $86,$89,$8C,$8F,$92,$95
         .BYTE $98,$9B ;,$9E
;--------------------------------------
; HELP TEXT STRINGS
;--------------------------------------
HELPTXT  .TEXT "CUR.CHAR:"
         .TEXT "F1-NEXT"
         .TEXT "F2-PREV"
         .TEXT "ROOM:"
         .TEXT "F5-NEXT"
         .TEXT "F7-PREV"
         .TEXT "EDIT.CHR:"
         .TEXT "[SPC]-CLR"
         .TEXT "[RET]-INS"

; AMOUNT OF HELP TEXTS

HLPTXTN  .BYTE $09

; LENGHT OF HELP TEXT STRINGS:

HELPLNG  .BYTE $09,$08,$08,$05,$08,$08
         .BYTE $09,$09,$09

; LOCATIO OF HELP TEXT STRINGS
; (LOW ORDER BYTE):

HLPLOCLO .BYTE $97,$BF,$E7,$0F,$37,$5F
         .BYTE $87,$AF,$D7

; LOCATION OF HELP TEXT STINGS
; (HIGH ORDER BYTE):

HLPLOCHI .BYTE $04,$04,$04,$05,$05,$05
         .BYTE $05,$05,$05

