;--------------------------------------
; TROLLY VALLEY
;
; LICENSED UNDER GPL
; (C) MIKKO KEINÄNEN 2004-2011
;--------------------------------------
;------------------------------------------------
; To add a sprite object
; spid = sprite object id
;----------------------------
; - set sprite state to SPRSTA table
;   spid is index
; - set sprite datapointers to SPRPNST,
;   SPRPNLF, SPRPNRG, SPRPNUP and SPRPNDW.
;   spid is index
; - set amount of animation frames to 
;   SPRFRLF, SPRFRUP.
; - set sprite collision area to SPRCOLX1, -X2,
;   SPRCOLY1, -Y2
; - 
;-------------------------------------
; To add a sprobj to a room
;-------------------------------------
; - set id of the room to RMNUM-table
; - set using the same index a sprite 
;   object id to RMSPRID table
; - set using the same index the animation
;   speed to RMANIMSP table
; - set using the same index the movement
;   speed to RMMOVSTP table
; - set using the same index the sprite
;   object's state bits to RMOBSTAI, see
;   table for more info (this table is
;   used for initial values)
; - reserve room to RMOBSTA table also
;   you can use the same byte value as
;   with RMOBSTAI (this table is used
;   for ingame values)
; - set sprite screen initial location to 
;   RMINIX and RMINIY tables
; - set sprite movement area to RMMINX,
;   RMMAXX,RMMINY and RMMAXY tables
; note also if the sprobj is a friend of moomin's
; and needs a favour from him:
; - the "harmless"-bit must be set in the 
;   sprobj state byte in RMOBSTAI table
; - you should add a message and a pointer
;   values to that message to MATMSGLO and -HI
;   data table.
;   table index is sprobj id
; - ...and a thank you message and pointer
;   values to MAMTHXLO and MAMTHXHI
;   table index is sprobj id
;--------------------------------------
; INSTRUCTIONS
;--------------------------------------
; Controls
; --------
;
; Joystick in port 2
;
; JOY-UP:
;
;   Climb / Jump at place.
;
; Fire-button, wave the sabre
;
; JOY-NW/NE-> DIRECTION JUMP
;
;   Direction jump can be stopped by
;   pulling the joystick to opposite
;   direction. This is important when
;   you have to do exact jumps.
;
; Button & UP (or T-key)
;
;   Pick up an item.
;
; Button & DOWN (or D-key)
;
;   Drop an item.
;
; A - key
;
;   Action key. Use with some item(s)
;   combined with some objects.
;
; RUN/STOP
;
;   Restart game
;
; To climb stairs simply pull stick
; to the direction of stairs and to get 
; out of stairs
; hold stick left/down or right/down.
;
; Story
; ------------------------ 
; Each friend of our hero have lost an
; important personal item. Our hero's
; job is to find and return each item
; to it's owner. After all items have
; been returned the game has been
; solved.
;
; Ingame information panel
; ------------------------  
; HOLDS
;
;   Show item currently being carried.
;
; ENERGY
;
;   Shows how much energy left.
;
; ITEM ICONS
;
;   Shows all the items. Retured items
;   are highlighted.
;
; MESSAGE DISPLAY
;
;   Shows messages from other
;   characters in the game

; start adresses of data areas
;--------------------------------------
SPRDATA  = $2000 ;(...->$3080)
CHRDATA  = $3800 ;(...->$4000)
LVLDATA  = $4800 ;(...->$6EE0 at the moment)
; variables are stored at 
; Zero page: $02, $FB-$FE
; The rest variables:  $02A7-$02FF
; program data: $8000 ($8000-$974C at the moment)

; zero page:
TMPCHR   = $02 ; OK

; screen memory pointer
SCRLO    = $2B    ; zero page needed
SCRHI    = $2C    ; zero page needed
                 
LVCHLO   = $37    ; zero page needed
LVCHI    = $38    ; zero page needed

; can i use $73 to $8a (sbr next byte of basic)
;MSGLOCLO = $73
;MSGLOCHI = $74
; or $b2,$b3 tape buffer start address
; $cc cursor blink
; $ce chr under cursor
; $d1-$d2 current screen line addr
; $d3 cursor column at curr line
; $d6 ...

KEYPRESS = $CB

BLKPNLO = $FB    ; zero page needed
BLKPNHI = $FC    ; zero page needed

LOTMP    = $FD    ; zero page needed
HITMP    = $FE    ; zero page needed


;-----------------
; id (NOT char code!) of an item moomin is carrying.
; $ff if carrying nothing.
MOOHOLDS = $02A7
LIVES    = $02A8
SCORE    = $02A9

; sound state 00000001 = playing 
SNDSTATE = $02AA
; start index for current sound
SND1STP  = $02AB
; sound data table index
SND1CNT  = $02AC
; sound note play time counter
SND1TIME = $02AD

TMPCNT   = $02AE

; screen spefic sprite object counter
; (moomin is not counted)
SPRCNT   = $02AF

; items found and returned to owner
; are set to this byte
; bit 0: harmonica/nuuskamuikkunen
; bit 1: magnification glass/hemuli
; bit 2: nilkkarengas/niiskuneiti
; bit 3: handbag/muumimamma
; bit 4: hat/muumipappa
; bit 5: button/nipsu
; bit 6: RUBIN/WIZARD
; bit 7: not in use, set to 1
ITEMSFND = $02B0

; bit 0: action range bit
; bit 1: on moving platform
ACTBITS  = $02B1

TMP      = $02B2
CURCHR   = $02B3

INIXLOC  = $02B4
INIYLOC  = $02B5

; for string printing sbr
STRCOL   = $02B6  ; string colour
TMP2     = $02B7
CHRFRAME = $02B8  ; char animation frame
CHANITIM = $02B9  ; char animation timer

FRAME    = $02BA  ; moomin anim frame counter
MOOCNT   = $02BB  ; moomin animation delay counter (the delay between altering a frame)

; Available states:
; 00000001 : Moving = 1 / not = 0
; 00000010 : left = 0 / right = 1
; 00000100 : up = 1 / down = 0
; 00001000 : 
; 00010000 : jumping
; 00100000 : falling
; 01000001 : climbing
; 10000010 : fighting, facing left
; 10000000 : fighting, facing right
; fighting is not possible while climbing/falling/jumping
MOOSTATE = $02BC

;moomin collision detect bits:
;bit 0 = left/bg
;bit 1 = right/bg
;bit 2 = up/bg
;bit 3 = down/bg
;bit 4 = climb up ok
;bit 5 = climb down ok
;bit 6 = 
;bit 7 = collision detection off!

;bit on means collision is detected!
MOOCOLL  = $02BD

JMPCNT   = $02BE

; current room number
ROOM     = $02BF

; previous location
PREVROOM = $02D0

FALLDCNT = $02D1

; MOOMIN COLLISION AREA
M1COLX1  = $02D2
M1COLX2  = $02D3
M1COLY1  = $02D4
M1COLY2  = $02D5

; column width for messages
MSGCOLWIDTH = $02D6

OUTSIDECOL1 = $02D7

; memory location aliases

SCREENMEM = $0400
SCREENMEMBK2 = $0500
MSGAREA = $05D6
SCREENMEMBK3 = $0600
SCREENMEMBK4 = $0700
ITEMHOLDER = $0425
SPR0DAT = $07F8 ; moomin sprite data pointer
SPR1DAT = $07F9
MOOX = $D000 ; sprite 0 x location
MOOY = $D001 ; sprite 0 y location
SPR1XLOC = $D002
SPR1YLOC = $D003
RASTER = $D012
SPRDISPLAY = $D015
VICCTRLREG = $D016
VICMEMCTRLREG = $D018
SPRMULTICOL = $D01C
BORDERCOL = $D020
SCREENCOL = $D021
CHRCOL1 = $D022
CHRCOL2 = $D023
SPRMCOL1 = $D025
SPRMCOL2 = $D026
SPR0COL = $D027
SPR1COL = $D028
JOYSTICK = $DC00

;---------------------------------------
         ; sys 32768
         *= $8000 
; initializing:
         JSR SETMEM 
         JSR LOADAT 
         JSR INIT
         JSR INITGFX
         JSR TITLESCR
         
         ; init sprites
         JSR INIENEMY
         JSR INITMOO
         
         ; init room
         JSR LDRMDAT
         JSR DRAWLVL


         ; continue to main loop...
         
;======================================
;
;         M A I N  L O O P
;
;======================================

MAINLOOP
        ; check timing from raster beam
         LDA RASTER
         CMP #$FF
         BNE MAINLOOP
         
         JSR SFX
         JSR ANICHR
         JSR CHGROOM
         JSR CNTBLOCK
         JSR OBJANI
         JSR DRWITEMS
         
         ; check if jump bit set
         LDA MOOSTATE
         LSR A
         LSR A
         LSR A
         LSR A
         LSR A ; jump bit
         BCC ISFALL
         JSR MOOJMP
         JMP PASSJ
ISFALL
         ; check if fall bit set
         LSR A ; fall bit
         BCC ISFIGHT
         JSR MOOFALL
         JMP PASSJ
ISFIGHT
        ;check if fight bit is set
        lsr a
        lsr a
        bcc readj
        jsr animfight

READJ
         JSR READJOY
PASSJ
         JSR COLCHR
         JSR READKEY
         JSR OBJCOLL
         JMP MAINLOOP
       
;======================================
;
;       I N I T I A L I Z I N G
;
;======================================

SETMEM
         ; turn basic ROM off
         LDA $01
         AND #$FE
         STA $01
         RTS
;--------------------------------------
; NEW INI
;--------------------------------------
NEWINI
         ; clear sprites
         JSR CLEARSPR
         JSR INIT
         ; draw title screen
         JSR TITLESCR
         JSR INITROOM
         JSR INIENEMY
         JSR INITMOO
         RTS
         
;--------------------------------------
; INITIALIZE
;--------------------------------------
INIT
         LDA #$00
         ; clear all moomin state bits
         STA MOOSTATE
         ; clear all collision bits
         STA MOOCOLL
         ; clear jump counter
         STA JMPCNT

         STA SPRDISPLAY
         STA SPRMULTICOL
         ; reset score
         STA SCORE
         ; reset sound state
         STA SNDSTATE
         STA SND1CNT
         STA ACTBITS

        ; starting room is $00
        ;lda #$1e
         ;STA ROOM --> actually this is set int the titlescreen sbr

         LDA #$02
         STA FRAME
         STA CHRFRAME
         STA CHANITIM

         LDA #$FF
         STA MOOHOLDS

         LDA #$64 ; 100%
         STA LIVES

         LDA #$05
         STA MOOCNT

         LDA #$00
         STA BLKPNLO
         LDA #$04
         STA BLKPNHI

        ; initial x and y location for
        ; moomin spriteobj
         LDX #$80
         LDY #$D0
         STX INIXLOC
         STY INIYLOC
         
         ; set items found/returned
         LDA #$80 ; 10000000
         STA ITEMSFND
         
         ; set item location values
         ; from initial values table
         LDX #$00
INITITM
         LDA INOBJCHU,X
         BEQ INITOSTA
         
         STA OBJCHRU,X
         LDA INOBJRM,X
         STA OBJROOM,X
         LDA INOBJLO,X
         STA OBJLO,X
         LDA INOBJHI,X
         STA OBJHI,X
         INX
         JMP INITITM
         
         ; restore RMOBSTA sprite
         ;  object state values
         LDX #$00
INITOSTA
         LDA RMOBSTAI,X
         CMP #$FF
         BEQ INITCON
         STA RMOBSTA,X
         INX
         JMP INITOSTA
         
INITCON
         JSR SNDCLR
         JSR COLLOFF
         RTS

;--------------------------------------
; LOAD DATA
;--------------------------------------

LOADAT
         ; SPRITES

         ; setlfs kernal routine call
         LDA #$0F
         LDX #$08

         ; secondary address 
         ; select from 0...14 for buffer
         LDY #$00 
         JSR $FFBA

         ; setnam kernal routine call
         LDA #$03
         LDX #<FILENSPR
         LDY #>FILENSPR
         JSR $FFBD

         ; load kernal routine call
         LDA #$00
         LDX #<SPRDATA
         LDY #>SPRDATA
         JSR $FFD5

         ; LEVEL DATA

         LDA #$0F
         LDX #$08
         LDY #$00 ; must be 0!!!!
         JSR $FFBA

         LDA #$05
         LDX #<FILENLVD
         LDY #>FILENLVD
         JSR $FFBD

         LDA #$00
         LDX #<LVLDATA
         LDY #>LVLDATA
         JSR $FFD5
         
         ; CHARSET

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

         RTS
;--------------------------------------
;INITIALIZE GRAPHICS
;--------------------------------------
; sets character map memory to $3800
; where custom charset is loaded
;--------------------------------------
INITGFX

; Set charset location:
; - load VICMEMCTRLREG to ACC.
;   upper 4 bits in VICMEMCTRLREG control location
;   of the screen memory, lower bits 3, 2 
;   and 1 control location of the charset
; - do AND 11110001
; - do ORA 00001110 to set memory bank
;   to $3800 - $3FFF
; - store the altered value back to VICMEMCTRLREG

         LDA VICMEMCTRLREG
         ;AND #$F0
         ORA #$0E
         STA VICMEMCTRLREG
         
         ; Set multicolor mode 
         ; (4th bit in VICCTRLREG)

         LDA VICCTRLREG
         ORA #$10 ; 00010000
         STA VICCTRLREG
         
         ; Set color 0 (SCREENCOL)
         
         ;LDA #$03 ; light blue
         ;STA SCREENCOL
         
         ; Set color 1 (CHRCOL1)
         
         ;LDA #$0E ; cyan
         ;STA CHRCOL1
         
         ; Set color 2 (CHRCOL2)
         
         ;LDA #$06 ; blue
         ;STA CHRCOL2

         RTS
;======================================
;
; I N G A M E  C H A R A C T E R  G F X
;
;======================================
;--------------------------------------
; COUNT CHAR LOCATION
;--------------------------------------
; count location in screen memory from
; sprite location
;--------------------------------------
CNTBLOCK 
         ; load y location to acc
         LDA MOOY
         SEC
         ; substract non visible pixels
         SBC #$2B

         ; count rows to X
         LDX #$00
         
LOOPBLKY ; char block is 8 pix high
         SBC #$08
         INX
         BCS LOOPBLKY

         LDA #$00
         STA BLKPNLO
         LDA #$04
         STA BLKPNHI

         ; add rows
         
ADDROWS  CLC
         LDA BLKPNLO
         ADC #$28 ; 40 CHARS == row
         STA BLKPNLO
         LDA BLKPNHI
         ADC #$00
         STA BLKPNHI
         DEX
         BNE ADDROWS
         
         ; load x location to acc
         LDA MOOX
         SEC
         ; substract non visible pixels
         SBC #$10

         ; count columns to Y
         LDY #$00
         
LOOPBLKX ; char block is 8 pix wide
         SBC #$08
         INY
         BCS LOOPBLKX

         ; add columns 
ADDCOLS  CLC
         DEY ; fix
         TYA
         ADC BLKPNLO
         STA BLKPNLO
         LDA BLKPNHI
         ADC #$00
         STA BLKPNHI
         RTS

;--------------------------------------
; DEBUG CHAR LOCATION
;--------------------------------------
; changes screen color memory value in
; current char location
;--------------------------------------
DBGCHR
         LDA BLKPNLO
         STA SCRLO
         LDA BLKPNHI
         STA SCRHI
         
         ;get screen colour 
         ;memory pointer values to
         ; LOTMP, HITMP
         
         JSR GETCOLM
         LDY #$00
         LDA (LOTMP),Y
         ADC #$01
         STA (LOTMP),Y

         ;DEBUG
         ; LDY #$00
         ; LDA #$40
         ; STA (BLKPNLO),Y
         
         RTS

;--------------------------------------
; FOR DROPPING ITEMS
;--------------------------------------
DROPOBJ  LDX MOOHOLDS

         CPX #$FF
         BEQ DROPOBX

         LDA BLKPNLO
         STA OBJLO,X

         LDA BLKPNHI
         STA OBJHI,X

         LDA ROOM
         STA OBJROOM,X

         LDY #$00

         LDA (BLKPNLO),Y
         STA OBJCHRU,X

         LDA OBJCHR,X
         STA (BLKPNLO),Y

         LDX #$FF
         STX MOOHOLDS

         ; DROP SOUND
         LDX #$03
         JSR SFXINIT

         JSR DISPITCL
DROPOBX  RTS

;--------------------------------------
; INIT ROOM
;--------------------------------------
INITROOM
         JSR LDRMDAT
         JSR DRAWLVL
         RTS
         
;--------------------------------------
; CHANGE ROOM
; THIS IS CALLED EACH ROUND FROM THE
; Main Loop
;--------------------------------------
CHGROOM 
         ; check x-location to left
         ; if <$10 change to left
         LDA MOOX
         CMP #$10
         BCS CHGROOML
         JSR CHRMLF
         rts
CHGROOML
         ; check x-location to right
         ; if >$fd change to right
         CMP #$FD
         BCC CHGROOMC
         JSR CHRMRG
         rts
CHGROOMC
        ; check y-location to up
        ; if climbable bit is set...
        LDA MOOCOLL
        AND #$10
        BEQ CHGROOMU

        ; ...load moomin y-location
        ; if it's less
        ; than $30 change room 
        LDA MOOY
        CMP #$30
        BCS CHGROOMU
        JSR CHRMUP
        rts        
        ;...else
CHGROOMU
         ; check y-location,
         ; if it's less
         ; than $20 change room 
         LDA MOOY
         CMP #$20
         BCS CHGROOMD
         JSR CHRMUP
         RTS
CHGROOMD
         ; if y-location is more than $E0
         ; change room

         CMP #$E0
         BCC CHGEXIT
         JSR CHRMDW
CHGEXIT
         RTS

;-------------------------------
; Call this sbr after changing 
; a room to initialize the new
; room
;-------------------------------
CHRMINI
         LDX ROOM
         ; store new init location
         LDA MOOX
         STA INIXLOC
         LDA MOOY
         STA INIYLOC
         ; initialize room and enemies 
         JSR INITROOM
         JSR INIENEMY
         RTS

;--------------------------------------
; CHANGE ROOM TO LEFT
;--------------------------------------
CHRMLF
         LDX ROOM
         LDA EXITLFT,X
         CMP #$FF
         BEQ CHRMLFX
         STX PREVROOM
         STA ROOM
         ; sets new x location to $EA
         LDA #$EA
         STA MOOX
         JSR CHRMINI
CHRMLFX
        RTS
;--------------------------------------
; CHANGE ROOM TO RIGHT 
;--------------------------------------
CHRMRG
         INC BORDERCOL
         LDX ROOM
         LDA EXITRGT,X
         CMP #$FF
         BEQ CHRMRGX
         STX PREVROOM
         STA ROOM
         ; sets new X location to $1F
         LDA #$1F
         STA MOOX
         JSR CHRMINI
CHRMRGX
        RTS

;--------------------------------------
; CHANGE ROOM TO UP 
;--------------------------------------
CHRMUP
        ; set current room id
        ; to x-reg
        LDX ROOM

        ;load id for next room up
         LDA EXITUP,X
         ; if room id is $FF
         ; no need to change room
         CMP #$FF
         BEQ CHRMUPX
        ; store current room
        ; as previous room
        STX PREVROOM
        ;set new room as current room
         STA ROOM

        ; set new y-location
         LDA #$D0
         STA MOOY
         JSR CHRMINI
CHRMUPX
        RTS

;--------------------------------------
; CHANGE ROOM TO DOWN 
;--------------------------------------
CHRMDW
        LDX ROOM
        LDA EXITDWN,X
        CMP #$FF
        BEQ CHRMDWX
        STX PREVROOM
        STA ROOM
        ; set new y-location
        LDA #$30
        STA MOOY
        JSR CHRMINI
CHRMDWX
        RTS

;--------------------------------------
; ROOM DATA
;--------------------------------------
LDRMDAT
         LDX ROOM
         LDY #$00
         STY TMP
        ; load start address of room data area
         ;LDA #$00
         LDA #<LVLDATA 
         STA LVCHLO

         ;LDA #$40
         LDA #>LVLDATA
         STA LVCHI
         ; jump over first byte
         ; that indicates the amount of char
             ; indicated by the second byte
         CLC
         LDA LVCHLO
         ADC #$01
         STA LVCHLO
         LDA LVCHI
         ADC #$00
         STA LVCHI

INITROLO
         ; is TMP equal to room we are loading
         ; (TMP is here a room number counter)
         LDA TMP
         CMP ROOM
         BEQ INITROOX

         LDA (LVCHLO),Y
         CMP #$FF ; if room data end sign
         BEQ INITROL2
         ; point to next char
         ; (first byte is amount of char that
         ; is located in the second byte, so we jump
         ; to next char by adding $02 to pointer)
         CLC
         LDA LVCHLO
         ADC #$02
         STA LVCHLO
         LDA LVCHI
         ADC #$00
         STA LVCHI

         JMP INITROLO

INITROL2 ; set tmp for next room
         INC TMP

         CLC
         LDA LVCHLO
         ADC #$02
         STA LVCHLO
         LDA LVCHI
         ADC #$00
         STA LVCHI

         JMP INITROLO

INITROOX ; room data area found!
         ; move pointer back to 1st byte
         ; that indicates the amount of
         ; chars indicated by 2nd byte
         SEC
         LDA LVCHLO
         SBC #$01
         STA LVCHLO
         LDA LVCHI
         SBC #$00
         STA LVCHI

INITROOXX RTS
         
;--------------------------------------
; DRAW ROOM
; LVCHLO/HI must point to the start
; of the room data for selected room
;--------------------------------------

DRAWLVL
         JSR CLEARSCR
         
         ; screen memory pointer
         ; to start address of screen
         ; memory
         LDA #$00
         STA SCRLO
         LDA #$04
         STA SCRHI

         LDY #$00

         LDX #$19 ; ROWS
         STX TMPCNT
         LDX #$1E ; CHARS

DRAWLVLO

         LDA (LVCHLO),Y
         STA TMP ; AMOUNT

         INY

         LDA (LVCHLO),Y
         CMP #$FF
         BEQ DRAWLVX
         STA CURCHR ; CHR VALUE

         LDY #$00

         CLC
         LDA LVCHLO
         ADC #$02
         STA LVCHLO
         LDA LVCHI
         ADC #$00
         STA LVCHI

         ; DRAW CURRENT CHAR(Ó) LOOP

DRWCHRLO
         LDA CURCHR
         STA (SCRLO),Y

         JSR DRWCHCOL

         CLC
         LDA SCRLO
         ADC #$01
         STA SCRLO
         LDA SCRHI
         ADC #$00
         STA SCRHI

         ; DECREASE CHAR/ROW COUNT

         DEX
         BNE DRWCHRLX
DRWCHRLN
         LDX #$1E

         CLC
         LDA SCRLO
         ADC #$0A
         STA SCRLO
         LDA SCRHI
         ADC #$00
         STA SCRHI

         ; DECREASE ROW COUNT

         DEC TMPCNT
         BEQ DRAWLVX

DRWCHRLX
         ; DECREASE CHAR AMOUNT COUNT

         DEC TMP
         BNE DRWCHRLO

         JMP DRAWLVLO

DRAWLVX
         ; Set bg color:
         ; 1. get room type
         LDY ROOM
         lda roomtype,y
         tay
         ; get bg-color for room type in y
         lda rmtypecb,y ;LDA COLBG,Y
         STA SCREENCOL
         
         ; Set color 1 (CHRCOL1)
         lda rmtypec1,y 
         STA CHRCOL1
         
         ; Set color 2 (CHRCOL2)
         lda rmtypec2,y
         STA CHRCOL2
         
         ; update panel
         JSR UPANEL
         RTS

;--------------------------------------
; ANIMATE CHARS
;--------------------------------------
; animates characters by loading new
; data values for character
;--------------------------------------
ANICHR   
         ; animation timer
         
         DEC CHANITIM
         BNE ANICHRX
         
         ; reset animation timer
         LDA #$06
         STA CHANITIM
         
         ; X is index
         LDX #$00
         
ANICHLO  ; main loop

         ; load char no. value to Y
         LDY CHVAL,X
         BEQ ANICHX

         ; set start of charset memory
         ; to temporary pointer
         
         LDA #<CHRDATA
         STA LOTMP
         LDA #>CHRDATA
         STA HITMP
         
         ; set pointer to current
         ; char value by adding
         ; 8 bytes * char value (Y)
ANICHL2
         CLC
         LDA LOTMP
         ADC #$08
         STA LOTMP
         LDA HITMP
         ADC #$00
         STA HITMP
         DEY
         BNE ANICHL2
         
         ; now pointer is at right place
         ; and can be referenced by
         ; (LOTMP),Y
         
         ; get start addr of animation
         
         LDA CHANILO,X
         STA SCRLO
         LDA CHANIHI,X
         STA SCRHI
         
         ; now we can get animation
         ; with (SCRLO),Y
         
         ; set Y to start of right frame
         LDY CHRFRAME
         DEY
         BEQ ANICHL0
         LDA #$00
         CLC
ANICHL3  ; adds frame count * $08 to
         ; Y value
         ADC #$08
         DEY
         BNE ANICHL3
         
         TAY
ANICHL0
         LDA #$00
         STA TMPCNT
         
ANICHL4  ; now Y has right value
         ; for getting frame values
         
         LDA (SCRLO),Y
         STY TMP
         LDY TMPCNT
         STA (LOTMP),Y
         LDY TMP
         
         INY
         
         INC TMPCNT
         LDA TMPCNT
         CMP #$08
         BNE ANICHL4
         
         ; to next char to be animated
         
         INX
         JMP ANICHLO
         
ANICHX
         DEC CHRFRAME
         BEQ ANICHX2
         
ANICHRX  RTS
         
ANICHX2  LDX #$03
         STX CHRFRAME
         RTS



;--------------------------------------
; UPDATE INGAME INFORMATION DISPLAY
;--------------------------------------
; live display @ $04BE
; score display @ $050E

;--------------------------------------
; UPDATE INGAME INFO PANEL
;--------------------------------------
UPANEL
         ; Write txt
         LDX #<TXTHOLDS
         LDY #>TXTHOLDS
         JSR print;
         
         LDX #<TXLIVES
         LDY #>TXLIVES
         JSR print;
         
         ; clear messages console
         JSR CLEARMSG
         JSR UPITEMS
         JSR DISPITEM
         JSR UPLIVES
         jsr upscore
         RTS

;--------------------------------------
; UPDATE LIVES DISPLAY
;--------------------------------------
UPLIVES
         ; Start address of energy
         ; display at screen memory
         LDX #$BE
         STX SCRLO
         LDY #$04
         STY SCRHI
         lda lives
         sta tmp         
         jsr printnum

         ; clear lives
         ;LDY #$09
         ;LDA #$20
;UPLIVLOA
         ;STA (SCRLO),Y
         ;DEY
         ;BNE UPLIVLOA
         
         ; get color memory pointer
         ; to LOTMP/HITMP
         
         ;JSR GETCOLM
 
         ; if more than 9 lives
         ; load 9 to Y

         ;LDA LIVES
         ;CMP #$09
         ;BCC UPLIVES1
         ;LDY #$09 
         ;JMP UPLIVLOB

;UPLIVES1
         ; set amount of lives to Y
         ;LDY LIVES

         ; set live icons to display
;UPLIVLOB ;LDA #$1C ; live symbol
         ;STA (SCRLO),Y
         ;LDA #$02 ; color
         ;STA (LOTMP),Y
         ;DEY
         ;BNE UPLIVLOB
         
         RTS
;--------------------------------------
; UPDATES THE SCORE DISPLAY @ $050E
;--------------------------------------
UPSCORE
        ;;;lo-/hitmp        

        lda #$0E
        sta scrlo
        lda #$05
        sta scrhi 
        lda score
        sta tmp
        jsr printnum 
        RTS

;--------------------------------------
; UPDATE ITEMS ICON DISPLAY
;--------------------------------------
; prints all the items in a row to
; panel. Items not found are in darker
; monochrome color. Items found are
; in clear color (multicolor state).
;
; bit 0: harmonica/nuuskamuikkunen
; bit 1: magnification glass/hemuli
; bit 2: nilkkarengas/niiskuneiti
; bit 3: handbag/muumimamma
; bit 4: hat/muumipappa
; bit 5: button/nipsu
; bit 6: --- no in use / default set 1
; bit 7: --- no in use / default set 1
;--------------------------------------
UPITEMS
         ; screen memory pointer
         LDX #$87
         STX SCRLO
         LDY #$05
         STY SCRHI
         
         ; colour memory pointer
         ; LOTMP/HITMP
         
         JSR GETCOLM
         
         LDX ITEMSFND
         STX TMP
         LDY #$00
         
UPITELO  
         LDX OWNITEMS,Y
         LDA OBJCHR,X
         STA (SCRLO),Y
         
         LSR TMP
         BCS UPITESET
         ; colour value (mono)
         LDA #$06
         JMP UPITECH
         
UPITESET ; colour value (multicolor)
         LDA #$0C

UPITECH  STA (LOTMP),Y
         INY
         CPY #$06
         BNE UPITELO
         
         RTS

;--------------------------------------
; CLEAR CARRYING ITEM DISPLAY
;--------------------------------------
DISPITCL
         LDA #$20
         ; clear item location in screen memory
         STA ITEMHOLDER
         RTS

;--------------------------------------
; DISPLAY ITEM
;--------------------------------------
DISPITEM
         LDX MOOHOLDS
         CPX #$FF
         BEQ DISPNULL
         
         ; get value for char
         LDA OBJCHR,X
         ; location in screen memory
         STA ITEMHOLDER

        
        ; load low byte of the description data location
        LDY OBJDSCHI,X

        ; load high byte of the description data location
        LDA OBJDSCLO,X
        TAX
        JSR WRITEMSG

        ; print description at screen
        
         RTS
DISPNULL
         JSR DISPITCL 
         RTS
 
;--------------------------------------
; <-- UPDATE DISPLAY ROUTINES END
;--------------------------------------

;--------------------------------------
; prints given number in decimal number
; format to the given screen location.
; - location must be set to 'scrlo/-hi'
; - value must be set to 'tmp'
; - uses also 
;        - regs: x,y
;         - tmp2
;--------------------------------------
printnum
        ldy #$00 ; y is screen memory index (0...2)
        ; x keeps count how many times can be divided by
        ; 100/10 
        ldx #$00
        sec ; set c-flag for substraction        
        lda tmp
printnumlo1
        sta tmp2 ; store result before substraction
        sbc #$64 ; 100
        bcc printnum2 ; got below 0
        inx
        jmp printnumlo1

printnum2 
        ; show hundreds (x-reg)
        clc
        txa
        adc #$30 ; add $30 to get a real number to display
        sta (SCRLO),y ; screen mem. loc #1
        iny
        ldx #$00
        lda tmp2 ; get the result before it went under 0
        sec
printnumlo2
        sta tmp2 ; store result before substraction 
        sbc #$0a ; 10        
        bcc printnum3 ; < 0
        inx
        jmp printnumlo2

printnum3 ; show tens (x-reg)        
        clc
        txa
        adc #$30
        sta (SCRLO),y
        iny
        ; show rest
        clc
        lda tmp2
        adc #$30
        sta (SCRLO),y
        rts

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
; lotmp and hitmp as screen colour 
; memory pointers. These pointers 
; are used to set colour value
; to right location in screen.
;
;--------------------------------------
GETCOLM
         LDA SCRLO
         ; no need to add low byte (#$00)
         STA LOTMP

         CLC
         LDA SCRHI
         ADC #$D4
         STA HITMP
         RTS

;--------------------------------------
; COLOUR CHAR
;--------------------------------------
; ACC must contain value of the char 
; to be coloured
; SCRLO/-HI must contain the location
; in screen memory.
;--------------------------------------
DRWCHCOL
         ;Character value in accu is 
         ;substracted with #$3c. 
         ;Substracted value is stored
         ;to Y. Y is used to read 
         ;character colour values 
         ;from 'CHARCOLS' data table.
         
         SEC
         SBC #$3C
         TAY

         ; get color memory pointer
         ; in LOTMP/HITMP

         JSR GETCOLM
         
         ; and get and set the right char colour
         LDA CHARCOLS,Y
         LDY #$00
         STA (LOTMP),Y
         RTS
;--------------------------------------
; DRAW ROOM ITEMS
;--------------------------------------
DRWITEMS LDX #$00

DRWITELO 
         ; Chek if current room is listed
         LDA OBJROOM,X

         CMP #$FF
         BEQ DRWITEMX

         CMP ROOM
         BEQ DRWITEM

         INX
         JMP DRWITELO

DRWITEM  ; get char location in 
         ; screen mem
         
         LDA OBJLO,X
         STA SCRLO
         
         LDA OBJHI,X
         STA SCRHI

         ; get char value
         LDA OBJCHR,X

         ; set char to screen
         LDY #$00
         STA (SCRLO),Y

         ; get color mem pointer LOTMP
         JSR GETCOLM

         ; increase color value
         LDA (LOTMP),Y
         ADC #$01
         STA (LOTMP),Y

         INX
         JMP DRWITELO

DRWITEMX RTS

        
         

        
;=======================================
;
;      C H A R  C O L L I S I O N
;
;=======================================


;--------------------------------------
; CHR COLLISION DETECTION (MAIN)
;--------------------------------------
; Set temporary screen memory pointer
; from current screen memory pointer 
; BLKPNLO/-HI to LO-/HITMP. Substract 
; $28 (40 == 1 row) from BLKPNLO/-HI 
; values before storing to LO-/HITMP. 
; The temporary pointer points to screen
; char location above current location
;--------------------------------------
COLCHR  
         SEC
         LDA BLKPNLO
         SBC #$28
         STA LOTMP
         LDA BLKPNHI
         SBC #$00
         STA HITMP

         ; compare if HITMP is under $04
         ; then the pointer points out of char data 
         ; starting from 0400
         lda hitmp
         cmp #$04
         bcc colchrx ; hitmp < $04

         ; collision for dangerous objs
         JSR COLDNG
         ; clibable chars
         JSR COLCLIMB
         ; left-right collision
         JSR COLMOVE
         ; collision down / fall / elev
         JSR COLDWN
         ; climbing stairs
         JSR STAIRCOL
colchrx
         RTS

;--------------------------------------
; COLLISION UP/RIGHT/LEFT/DOWN
;--------------------------------------
; LOTMP/HITMP must be set as screen
; memory pointer
;--------------------------------------

COLMOVE
         ; CHANGED!!!
         ; SET COLL LF/(OLD:UP)/RG TO 0
         ; 11111100(OLD:11111000)

         LDA MOOCOLL
         AND #$FC ;OLD: $F8
         STA MOOCOLL

         LDX #$00

COLMOVLO
         ; load char to be compared
         ; if same char is find
         ; then the collision bit is set
         ; (cannot move to that direction)
         
         LDA WALLCHRS,X
         BEQ COLMVX
         STA TMPCHR

         ; movement collision up
         ; check the char above
         ; (not in use at the moment)

         ; LDY #$00
         ; LDA (LOTMP),Y
         ; CMP TMPCHR
         ; BNE COLMVLF

         ; SET COL UP BIT:

         ; LDA MOOCOLL
         ; ORA #$04
         ; STA MOOCOLL

COLMVLF
         ; collision left
         ; check the char at left
         
         LDY #$27
         LDA (LOTMP),Y
         CMP TMPCHR
         BNE COLMVRG

         ; SET COL LFT BIT:

         LDA MOOCOLL
         ORA #$01
         STA MOOCOLL

COLMVRG
         ; collision right
         ; check char at right
         
         LDY #$29
         LDA (LOTMP),Y
         CMP TMPCHR
         BNE COLMVNX

         ; SET COL RG BIT:
         LDA MOOCOLL
         ORA #$02
         STA MOOCOLL

COLMVNX
         ; NEXT CHAR:

         INX
         JMP COLMOVLO
COLMVX
         RTS
         
;--------------------------------------
; COLLISION TO CLIMBABLE CHARS
;--------------------------------------
; LOTMP/HITMP must be set as screen
; memory pointer
;--------------------------------------

COLCLIMB ;-----------------------------
         ; clear climb up and down bits
         ;-----------------------------
         LDA MOOCOLL
         AND #$CF ; 11001111
         STA MOOCOLL

         LDX #$00

COLCLILO ;-----------------------------
         ; load char from LADCHR-table
         ; to be compared -
         ; if same char is found at the
         ; position compared
         ; the collision bit is set
         ; (climbing IS allowed!)
         ; leave if $00 (end mark in 
         ; the table)
         ;-----------------------------
         
         LDA LADCHR,X
         BEQ COLCLIX
         STA TMPCHR
         
         ;-----------------------------
         ; clibable up?
         ; check char above and in the
         ; current location and chars
         ; next to (right side) both of 
         ; them.
         ;-----------------------------

         ; LO/HITMP points to
         ; char above current location
         LDY #$00
         LDA (LOTMP),Y
         CMP TMPCHR
         BEQ COLCLUSET
         
         ; char next to previous location
         INY 
         LDA (LOTMP),Y
         CMP TMPCHR
         BEQ COLCLUSET

         ; char at current location
         LDY #$28
         LDA (LOTMP),Y
         CMP TMPCHR
         BEQ COLCLUSET
         
         ; char next to previous location
         INY 
         LDA (LOTMP),Y
         CMP TMPCHR
         BNE COLCLIDW
         
COLCLUSET
         ;-----------------------------
         ; set clibable up bit
         ;-----------------------------
         LDA MOOCOLL
         ORA #$10
         STA MOOCOLL
         
COLCLIDW ;-----------------------------
         ; clibable down?
         ;-----------------------------
         LDY #$50
         LDA (LOTMP),Y
         CMP TMPCHR
         BEQ COLCLDSET
         
         INY
         LDA (LOTMP),Y
         CMP TMPCHR
         BNE COLCLINX
         
COLCLDSET
         ;-----------------------------
         ; set clibable down bit
         ;-----------------------------
         LDA MOOCOLL
         ORA #$20
         STA MOOCOLL
         
COLCLINX ;-----------------------------
         ; loop to next char
         ;-----------------------------
         INX
         JMP COLCLILO
COLCLIX
         RTS
         
;--------------------------------------
; FLOOR COLLISION
;--------------------------------------
; This routine sets FALL BIT in MOOSTATE
; byte if no floor chars are detected.
;
; Also checks elevator chars.
;
; LOTMP/HITMP must be set as screen
; memory pointer
;--------------------------------------
COLDWN   
         ; clear collision down bit
         LDA MOOCOLL
         AND #$F7
         STA MOOCOLL
         
         ;-----------------------------
         ; check if travelling
         ; on moving platform sprite
         ; object from ACTBITS bit 1
         ; otherwise set fall bit
         ;-----------------------------
         
         LDA ACTBITS
         LSR A
         LSR A
         BCS COLDWCON
         
         ; set fall bit
         LDA MOOSTATE
         ORA #$20
         STA MOOSTATE
         
COLDWCON
         ;-----------------------------
         ; check chars in FLORCHRS 
         ; datatable - loop starts
         ;-----------------------------
         LDX #$FF
COLDWLO
         INX
         LDA FLORCHRS,X
         BEQ COLDWX
         STA TMPCHR

         LDY #$50

         LDA (LOTMP),Y
         CMP TMPCHR
         BEQ COLDWSET

         INY
         LDA (LOTMP),Y
         CMP TMPCHR
         BNE COLDWLO
         
COLDWSET
         ;-----------------------------
         ; On a steady ground, clear 
         ; fall bit and reset fall
         ; counter.
         ;-----------------------------
         
         LDA MOOSTATE
         AND #$DF
         STA MOOSTATE
         LDA #$01
         STA FALLDCNT
         
         ;-----------------------------
         ; TMPCHR continues char
         ; that was detected in current
         ; position. Check if it can be
         ; found in climbable chars 
         ; table (LADCHR). If true 
         ; SKIP collision down bit. 
         ;-----------------------------
         LDX #$FF
COLDWL3
         INX
         LDA LADCHR,X
         BEQ COLDWA
         CMP TMPCHR
         ; skip collision down bit
         BEQ COLDWE
         
         JMP COLDWL3 ;<-- loop end
         
COLDWA
         ;-----------------------------
         ; set collision down bit
         ;-----------------------------
         LDA MOOCOLL
         ORA #$08
         STA MOOCOLL
COLDWE
         ;-----------------------------
         ; TMPCHR continues char
         ; that was detected in current
         ; position. check if it can be
         ; found in elevator chars 
         ; table. Move to a direction of
         ; elevator char movement.
         ;-----------------------------
         LDX #$FF
COLDWL2  
         INX
         LDA ELECHRS,X
         BEQ COLDWX
         CMP TMPCHR
         BNE COLDWL2
         ; found elevator char!
         JSR MOVELEV
         
COLDWX
         RTS
         

;--------------------------------------
; PICK UP ITEM 
;--------------------------------------
; - LOTMP/HITMP must be set as screen
;   memory pointer
;--------------------------------------

COLTRS 
        ; if moomin is already carrying
        ; something, leave
        LDA MOOHOLDS
        CMP #$FF
        bne coltrx
          
         ; x is used as index for a datatable
         ; x is set to 0 when loop starts
         LDX #$ff
COLTRLO        
          ;Sets current char block 
         ;location from BLKPNLO/-HI to
         ;temporary location pointer
         ;LO-/HITMP.
         ; this must be reset during each loop round 
         
         LDA BLKPNLO
         STA LOTMP
         LDA BLKPNHI
         STA HITMP

        inx
        ; check if current room is listed in
        ; the objroom dynamic table

        lda objroom,x
        ; exit if end sign $ff found!
        cmp #$ff
        beq coltrx
        cmp room ; current room
        bne coltrlo

        ; room matched!  check
        ; if the location matched in the
        ; objlo/-hi tables, using x as index

        ; if location matches, THEN the item
        ; can be picked up!

        ; compare objlo/-hi to lo-/ hitmp
        ; increasing lo-/hitmp to 0...2

        ; y is a screen memory index
         ldy #$ff
COLTRLO2
        iny
        cpy #$03
        beq coltrlo

        ; add value of y to lo-/hitmp
        clc
        tya
        adc lotmp
        sta lotmp
        lda #$00
        adc hitmp
        sta hitmp
        
        ; compare objlo/-hi to lo-/ hitmp

        lda objhi,x
        cmp hitmp
        bne coltrlo2
        lda objlo,x
        cmp lotmp
        bne coltrlo2

        ; loops coltrlo and coltrlo2 end

        ; location matched!
        ; item can be picked up!
        JSR TREASURE
COLTRX
        RTS

;--------------------------------------
; ITEM FOUND
; This is used from COLTRS routine
; when item can be picked up.
; - X must hold the index of the item found
;   to the OBJCHR datatable
; - LO-/HITMP must hold the location of
;   the item in the screen memory
;--------------------------------------
TREASURE 
         LDA #$FE ; $FE means NULL here
         ; set item locations at screen memory to null
         ; and set room location to null
         STA OBJLO,X
         STA OBJHI,X
         STA OBJROOM,X

        ; replace object char with background char
         LDA OBJCHRU,X
         LDY #$00
         STA (LOTMP),Y

        ; set item index to mooholds
         STX MOOHOLDS
         
         ; play sound
         LDX #$02
         JSR SFXINIT
         
         JSR DISPITEM
         inc score
         JSR UPSCORE

TREASURX RTS         
         
;--------------------------------------
; DANGER ITEMS COLLISION
;--------------------------------------
; LOTMP/HITMP must be set as screen
; memory pointer
;--------------------------------------

COLDNG
         LDX #$00
         LDY #$28

COLDNLO
         LDA DNGRCHRS,X
         BEQ COLDNX ; $00 is end sign
         STA TMPCHR

         LDA (LOTMP),Y
         CMP TMPCHR
         BEQ COLDNSET
         
         LDY #$50
         LDA (LOTMP),Y
         CMP TMPCHR
         BEQ COLDNSET

         INX
         JMP COLDNLO

COLDNSET
         lda #$05 ; set damage to tmp
         sta tmp
         JSR DECLIVES
         JSR INITMOO

COLDNX
         RTS
         
;--------------------------------------
; STAIRS COLLISION
;--------------------------------------
; LOTMP/HITMP contains current location
;
; this routine is used to lift up a 
; stair when moomin approaches one
;--------------------------------------
STAIRCOL
         ; movement bit must be set
         LDA MOOSTATE
         LSR A
         BCC STAIRLOX
         
         ; joystick down bit must NOT 
         ; be set
         LDA JOYSTICK
         LSR A ; 1st is up bit
         LSR A ; 2nd is down bit
         BCC STAIRLOX

         LDX #$00

         LSR A
         BCC STAIRLFT

         LDY #$29
         JMP STAIRLO
STAIRLFT
         LDY #$28
STAIRLO
         LDA STAIRCHR,X
         BEQ STAIRLOX
         STA TMPCHR

         LDA (LOTMP),Y
         CMP TMPCHR
         BEQ STAIRSET

         INX
         JMP STAIRLO

STAIRSET
         SEC
         LDA MOOY
         SBC #$08
         STA MOOY
STAIRLOX
         RTS

;=======================================
;
;    S P R I T E  C O L L I S I O N
;
;=======================================

;--------------------------------------
; SPRITE OBJECT COLLISION

; TODO: BUGI - jos liikkuvalla plarformilla
; ja törmää vaaralliseen spritehahmoon
; putoaa plaformilta!

;--------------------------------------
OBJCOLL         
        ; if not sprobj's at screen leave
         LDA SPRCNT
         BEQ OBJCOLX
 
         ; clear platform bit from 
         ; ACTIONBITS
         
         LDA ACTBITS
         AND #$FD ;1111 1101
         STA ACTBITS

         ; x is index for the loop
         LDX #$00

         ;-----------------------------
         ; Loop to check the sprite
         ; objects in current room.
         ; compare moomin location 
         ; to sprobj's collision area
         ;-----------------------------
OBJCOLO
         LDA OBJCOLX1,X
         CMP MOOX
         BCS OBJCOLON

         LDA OBJCOLX2,X
         CMP MOOX
         BCC OBJCOLON

         LDA OBJCOLY1,X
         CMP MOOY
         BCS OBJCOLON

         LDA OBJCOLY2,X
         CMP MOOY
         BCC OBJCOLON
         
         ;-----------------------------
         ; collision has happened!
         ;-----------------------------

        ; check if enemy sprobj
        ;LDA OBJSTA,X
        ;AND 

        ;-----------------------------
         ; check if this is a
         ; blocking object (door,
         ; boulder, etc.) from
         ; OBJSPST (originally from
         ; SPRSTA) bit 2.
         ;-----------------------------         
         LDA OBJSPST,X
         LSR A
         LSR A
         LSR A
         BCC OBJCOLPF
         
         ;-----------------------------
         ;This is a blocking object
         ;cannot let u pass...
         ;-----------------------------
         JSR BLOCKOBJ
         ;RTS
         ;test:
         JMP OBJCOLON
         
OBJCOLPF ;-----------------------------
         ; no, not a blocking object
         ; try if it's a platform 
         ; object (check the bit 3).
         ;-----------------------------
         LSR A
         BCC OBJCOLE
         JSR PLATFOBJ
         ;RTS
         ;test:
         JMP OBJCOLON
         
OBJCOLE  ;-----------------------------
         ; Check if object
         ; is harmless (bit 6 set)
         ;-----------------------------
         LDA OBJSTA,X
         AND #$40; 01000000
         BNE OBJCOLN
         
         ;-----------------------------
         ; collision happened with 
         ; enemy object
         ;-----------------------------
         ; check if fight mode is on
         ; and the directions so that 
         ; moomin is facing enemy object

         LDA MOOSTATE ; if state is 1xxx xxxx
         AND #$80 ; 1000 0000
         BEQ OBJCOLE2 ; fight state is not set 
        
        
        jsr FIGHTENEMY ;fight state is set
        rts
         
OBJCOLE2
         ;-----------------------------
         ; decrease
         ; lives, initialize moomin and
         ; enemy objects and leave sbr
         ;-----------------------------
         
        ; check if cheat mode is on
        lda MOOCOLL
        and #$80 ; 10000000
        beq OBJCOLXX

         lda #$01 ; set damage to tmp
         sta tmp
         JSR DECLIVES
         ;JSR INITMOO
         ;JSR INIENEMY
         RTS
         
OBJCOLN  ;-----------------------------
         ; harmless sprite object found
         ;-----------------------------

         JSR HEYMATE
         RTS
        ;debug:
        ;JSR DEBUG


OBJCOLON ;-----------------------------
         ; Increase counter and compare
         ; the amount of sprite objects
         ; in room. Loop if not yet eq.
         ;-----------------------------
         INX
         CPX SPRCNT
         BNE OBJCOLO
OBJCOLX
         RTS

OBJCOLXX inc BORDERCOL
         rts

;--------------------------------------
FIGHTENEMY
; x contains the index of this sprite object
; for property tables
;--------------------------------------
        ; if enemy x is less than moomin x
        ; and moomin is facing left
        ; or if enemy x is more than moomin x
        ; and moomin is facing right
        ; reverse the moving direction of enemy object
        
        ; continue only if moving bit (2nd bit) 
        ;  is set for the enemy obj
        LDA OBJSTA,X
        LSR A
        LSR A
        BCC FIGHTENEMYX

        ; check directions from sprite object state OBJSTA
        ; if bit 2 (3rd bit) is set -> facing left
        ; if bit 4 is set -> facing right
        lda objsta,x
        and #$04 ; 0000 0100
        beq FIGHTENEMYR ; left bit is not set
        ;left bit is set, moomin must be facing right
        lda moostate
        and #$02
        beq FIGHTENEMYR ; moomins right bit is not set 
        ; moomin is facing enemy object
        jmp FIGHTENEMYOK
FIGHTENEMYR
        lda objsta,x
        and #$08
        beq FIGHTENEMYX ;right bit is not set
        ; right bit is set, moomin must be facing left
        lda moostate
        and #$02
        bne FIGHTENEMYX ; moomin is not facing left
        
        ; must be moving at l/r-direction
        ; (3rd or 4th bit must be set)
        ;LDA OBJSTA,X
        ;AND #$0C   ;00001100
        ;BEQ FIGHTENEMYX
FIGHTENEMYOK
        LDA OBJSTA,X
        EOR #$0C ; 00001100 ; reverse l/r bits
        STA OBJSTA,X
        ; play hit sound 
        ; store x-reg
        stx tmp ; is tmp free?
        ; set rigt sfx number to x
        ldx #$05
        jsr sfxinit
        ; restore x-reg
        ldx tmp
FIGHTENEMYX
        rts

;--------------------------------------
; PLATFORM OBJECT
;--------------------------------------
; X must contain sprite no. from 
; object collision subroutine 
; (sprobj's room index)
;
; Check if our hero's current location
; is over a platform object. If true, move
; our hero with the speed and direction
; of the platform object. Otherwise leave
; the routine.
;--------------------------------------
PLATFOBJ
         ; get platform objects
         ; y-location and compare
         ; to moomins y-location.
         LDA OBJY,X
         CMP MOOY
         ; if platform y location is greater
         ; than moomin y location
         BCS PLATFOBJ2
         RTS

PLATFOBJ2         
         ; move moomin sprite
         ; if platform is also moving
         ; -> is platform movement speed counter set to 0
         ; (OBJSPCNT,X = 0) otherwise exit

         LDA OBJSPCNT,X
         BEQ PLATFSET
         ; set platform collision bits
         JSR PLATFS2
         RTS

PLATFSET
                  
         ; get movement speed of
         ; of platform obj to TMP
         LDA OBJMOVSTP,X
         STA TMP
         
         ; get direction of platform obj
         LDA OBJSTA,X
         LSR A ; animation bit
         LSR A ; movement bit
         LSR A ; left movement bit
         BCS PLATFLF
         LSR A ; right movement bit
         BCS PLATFRG
         LSR A ; up ...
         BCS PLATFUP
         LSR A ; down ...
         BCS PLATFDW
         
         RTS
PLATFLF
         ; check collision left bit
         
         LDA MOOCOLL
         LSR A

         ; exit if collision left bit set
         BCS PLATFOX
         
         ; check platform location
         ; do not move if min X or less
         
         LDA OBJX,X
         CMP OBJMINX,X
         BEQ PLATFS2
         BCC PLATFS2
        
         ; calculate new x location for moomin
         SEC
         LDA MOOX ; moo x
         SBC TMP
         STA MOOX
         JSR PLATFS2
         RTS
PLATFRG
         ; check collision rgt bit
         
         LDA MOOCOLL
         LSR A
         LSR A
         BCS PLATFOX
         
         ; check platform location
         ; do not move if max X or more
         
         LDA OBJX,X
         CMP OBJMAXX,X
         BEQ PLATFS2
         BCS PLATFS2
         
         CLC
         LDA MOOX
         ADC TMP
         STA MOOX
         JSR PLATFS2
         RTS
PLATFUP
         ; check collision up bit
         
         LDA MOOCOLL
         ;LSR A
         ;LSR A
         ;LSR A
         ;BCS PLATFOX
         AND #$04; 0000 1000
         BNE PLATFOX
         
         ; check platform location
         ; do not move if min Y less
         
         LDA OBJY,X
         CMP OBJMINY,X
         BEQ PLATFS2
         BCC PLATFS2
         
         SEC
         LDA MOOY ; MOO Y
         SBC TMP
         STA MOOY
         JSR PLATFS2
         RTS
PLATFDW
         ; check collision down bit
         
         LDA MOOCOLL
         ;LSR A
         ;LSR A
         ;LSR A
         ;LSR A
         ;BCS PLATFOX
         AND #$08; 0000 1000
         BNE PLATFOX
         
         ; check platform location
         ; do not move if max Y
         
         LDA OBJY,X
         CMP OBJMAXY,X
         BEQ PLATFS2
         BCS PLATFS2
         
         CLC
         LDA MOOY
         ADC TMP
         STA MOOY
PLATFS2
         JSR PLATFCL
PLATFOX
         RTS
;--------------------------------------
; SET ON PLATFORM OBJECT STATES
;--------------------------------------
PLATFCL
         ; clear fall bit 
         LDA MOOSTATE
         AND #$DF
         STA MOOSTATE
         
         ; clear fall counter
         LDA #$00
         STA FALLDCNT
         
         ; set action bit 1
         ; = moving on platform
         LDA ACTBITS
         ORA #$02
         STA ACTBITS
         
         RTS
;--------------------------------------
; RAN INTO A BLOCKING OBJECT
;--------------------------------------
; X must contain sprite no. from 
; object collision subroutine
;--------------------------------------

BLOCKOBJ
         ; clear left/right collision 
         ; bit (bit 0 and 1)
         
         LDA MOOCOLL
         AND #$FC ; was AND $FC - a bug!?
         STA MOOCOLL
         
         ; check on wich side the
         ; blocking object is
         
         LDA OBJX,X
         CMP MOOX
         BCC BLOCKOBL
         
         ; set collision right bit
         LDA MOOCOLL
         ORA #$02
         STA MOOCOLL
         
         RTS
         
         ; set collision left bit
BLOCKOBL
         LDA MOOCOLL
         ORA #$01
         STA MOOCOLL
         
         RTS
         
;--------------------------------------
; FOUND AN FRIENDLY SPRITE OBJECT
;--------------------------------------
; X must contain room sprite index no. from 
; object collision subroutine
;--------------------------------------
HEYMATE
         ; set id of the colliding sprite object to Y
         LDY OBJID,X
         
         ; store x
         STX TMP
         
         ; store the item id (index) to X
         ; (the item moomin is holding)
         LDX MOOHOLDS
         
         ; load the needed item id of the colliding 
         ; sprite object 
         LDA OBJOWN,Y

         ; ...and compare the item value to the item 
         ; that moomin is carrying
         CMP MOOHOLDS
         BEQ HEYTHX ; match!
         
         ; check if item is already
         ; returned
         
         ; set needed item id to x
         TAX
         LDA ITEMSFND
         AND OBJORA,X
         
         BNE HEYTHX2
         
         ; moomin has not the right
         ; item so show message
         
         ; Y CONTAINS SPR OBJ ID
         
         ; load start address of message
         ; to x (lo) and y (hi)
         ; and write message
         
         LDX MATMSGLO,Y
         LDA MATMSGHI,Y
         TAY
         JSR WRITEMSG
         
         ; restore x, return
         LDX TMP
         RTS
         
HEYTHX   ; moomin is carrying a matching
         ; item
         
         ; set this item as returned
         LDX MOOHOLDS
        ; load the byte that holds info about the found items
         LDA ITEMSFND
        ; get the ORA byte for item X, that sets it found
         ORA OBJORA,X
         STA ITEMSFND
         lda #$0a ; set amount of energy boost
         sta tmp
         JSR INCLIVES
        ; check if all the bits are set 
        ; (all the items returned)
         CMP #$FF
         BNE HEYCON
         ; if all bits are set in 
         ; ITEMSFND game is solved!
         JSR CONGRATS
HEYCON
        ; clear moomin's pocket
         LDA #$FF
         STA MOOHOLDS
        
        ; clear and
        ; update item display 
         JSR DISPITCL
         JSR UPITEMS
HEYTHX2  
        ; write thank you -message

         LDX MAMTHXLO,Y
         TYA
         TAY
         LDA MAMTHXHI,Y
         TAY
            
         JSR WRITEMSG
         
         ; restore x, return 
         LDX TMP
         RTS

;--------------------------------------
; 4 DEBUGGING
; loops until 'a'-key is pressed
;--------------------------------------
DEBUG


DEBUGLO
         INC BORDERCOL
         ; READ KEYPRESS
         LDA KEYPRESS
         CMP #$0A ; a-key
         BEQ DEBUGX
         JMP DEBUGLO
DEBUGX
         RTS
         
 
;--------------------------------------
; setting room no 
; for debugging only
;--------------------------------------
INCROOM
        LDA ROOM
        CMP #$20 ; max+1 room id
        BNE INCROO1
        LDA #$00
        JMP INCROO2
INCROO1
        CMP #$FF
        BNE INCROOX
        LDA #$1D
INCROO2
        STA ROOM

INCROOX        
        JSR INITROOM
        JSR INIENEMY
        RTS

        
;--------------------------------------
; DECREASE LIVES
; - set amount of decrease to TMP
;--------------------------------------
DECLIVES
         JSR DIEFX
         sec
         lda lives
         sbc tmp
         bcs xlives 
         JSR GAMEOVER
         RTS
XLIVES   sta lives
         JSR UPLIVES
         RTS

;--------------------------------------
; INCREASE LIVES
; - set amount of increase to TMP
;--------------------------------------
INCLIVES
        clc
        lda lives
        adc tmp
        bcc inclivesx        
        lda #$ff ; full energy
inclivesx
        sta lives
        jsr uplives
        RTS

;--------------------------------------
; LOST A LIFE EFFECTS
;--------------------------------------
DIEFX    ; init sound effect
         ;LDX #$01
         ;JSR SFXINIT
         
         ;LDX #$20
;DIEFXLO
         ; not too fast! (a timer)
         ;LDA RASTER
         ;CMP #$FF
         ;BNE DIEFXLO
         
         ; change sprite colours
         ;INC SPR0COL
         ;DEC SPRMCOL1
         ;INC SPRMCOL2
         
         ; continue sound effect
         ;JSR SFX
         
         ;DEX
         ;BNE DIEFXLO
         
         RTS
 ;--------------------------------------
; SPECIAL ACTIONS (WITH ITEMS)
;--------------------------------------
; Here things happen if carrying 
; certain items, maybe combined
; with other (dropped) items or
; special objects.
;--------------------------------------
ACTION
         ; do actions by items
         ; carrying
         
         ldx mooholds
         lda objchr,x 
         CMP #$83 ; 08 ; candle
         BEQ ACTIONC
         
         CMP #$59 ; 04 ; a key
         BEQ ACTIONK

         CMP #$1e ; 0A ; a magic scroll
         BEQ ACTIONS
        
         cmp #$1c ;0b ; moomin idol (energy booster)
         BEQ ACTIONI
 
         RTS
ACTIONC  ; candle
         JSR BLOWUP
         RTS
         
ACTIONK  ; key
         JSR ACTKEY
         RTS
ACTIONS  ; magic scroll
         JSR ACTMSCROLL
        RTS

ACTIONI ; energy booster
        JSR ACTENERGY
        RTS

;--------------------------------------
; ENERGY BOOSTER ACTION
;--------------------------------------
ACTENERGY
        ; set amount of energy boost to TMP
        lda #$10
        sta tmp
        jsr inclives
        ; remove energy booster item
        lda #$ff
        sta mooholds
        jsr dispitcl
        RTS        

;--------------------------------------
; REMOVE TOMBSTONE
;--------------------------------------
ACTMSCROLL
        LDA #$15 ; tombstone id
        STA TMP
        JSR OPENDOOR
        RTS

;--------------------------------------
; open 'door'-sprite objects
; TMP must contain the sprite object
; id for door-object
;--------------------------------------
OPENDOOR
        ; 'door' sprite object
        ; must be available to use
        ; the 'key'
        LDX #$00
OPENDOORLO
        LDA OBJID,X
        CMP TMP ; 'door'-object id 
        BEQ OPENDOORC
        INX
        cmp sprcnt
         bne opendoorlo
        RTS
OPENDOORC
        ; check if door-object is near enough
        ; X contains the index of sprite
        ; object.

        jsr ACTRANGE
        lda actbits
        lsr a
        bcs opendoorcn
        rts
opendoorcn
        jsr hideobj
        rts

;--------------------------------------
; OPEN DOOR WITH KEY
;--------------------------------------
ACTKEY
        lda #$0d
        sta tmp
        jsr opendoor
        rts
;--------------------------------------
; BLOW UP DYNAMITE
;--------------------------------------
; Action to blow up boulder with
; dynamite and candle. Maybe some
; other things can be blown up too.
;--------------------------------------
BLOWUP   
         ;-----------------------------
         ; to blow up boulder
         ; check that boulder is at
         ; the same room
         ;-----------------------------
         LDX #$00
BLOWULO
         LDA OBJID,X
         CMP #$0C ; boulder id
         BEQ BLOWUPX
         INX
         CPX SPRCNT
         BNE BLOWULO ; loop
         
         ;-----------------------------
         ; leave routine if no boulder
         ;-----------------------------
         RTS
        
        ; x reg contains now index to spritte object
        ; datatables OBJxxx 
BLOWUPX  
         ;-----------------------------
         ; check if boulder is at
         ; "action range" (near enough)
         ;-----------------------------
         JSR ACTRANGE ; x must remain unchanged!
         LDA ACTBITS
         LSR A
         BCS BLOWUPD
         RTS
BLOWUPD
         ; The temporary pointer points 
         ; to screen char location above
         ; and 1 char left from current 
         ; location
         
         SEC
         LDA BLKPNLO
         SBC #$27
         STA LOTMP
         LDA BLKPNHI
         SBC #$00
         STA HITMP
         LDY #$00
         LDA (LOTMP),Y
         ; dynamite char value
         CMP #$82
         BEQ BLOWUPOK
         INY
         LDA (LOTMP),Y
         CMP #$82
         BEQ BLOWUPOK
         ; 1 row down
         LDY #$27
         LDA (LOTMP),Y
         CMP #$82
         BEQ BLOWUPOK
         INY
         LDA (LOTMP),Y
         CMP #$82
         BEQ BLOWUPOK
         ; 2 rows down
         LDY #$4F
         LDA (LOTMP),Y
         CMP #$82
         BEQ BLOWUPOK
         INY
         LDA (LOTMP),Y
         CMP #$82
         BEQ BLOWUPOK
         RTS
BLOWUPOK
         ; store Y index where dynamite
         ; was found
         STX TMP ; store x
         ; do a blowup routine
         ; init a blowup sound!
         LDX #$04 ; no of the sound
         JSR SFXINIT
         LDX TMP ; resotre x
         ; store Y
         ;STY TMP
         
         ; Set lenght of flashing fx
         ; to Y and visit fx sbr
         ;LDY #$20
         ;JSR FLASHFX; NOTE! X must be stored before calling this routine, and restore afterwards
         JSR HIDEOBJ
         
         ; moved!
        
         ; clear dynamite (id $07)
         LDX #$07
         LDA OBJCHRU,X
         ;LDY TMP ; restore y
         STA (LOTMP),Y
         
         ; set item to item's heaven
         LDA #$FE
         STA OBJROOM,X
         
         RTS
;--------------------------------------
; FLASHING BORDER AND BG EFFECT
;--------------------------------------
; Initialize sound first if you want
; sfx also. Y must be set to length of
; effect ($00...$FF).
;--------------------------------------
FLASHFX

FLASHFXLO
        ; get original colour
        LDX BORDERCOL
        STX TMP
        LDX SCREENCOL

         ; timer with raster beam
         LDA RASTER
         CMP #$FF
         BNE FLASHFXLO
         
         INC BORDERCOL
         DEC SCREENCOL
         
         ; continue sound effect
         JSR SFX
         
         DEY
         BNE FLASHFXLO
        
        ; restore backround and
        ; border colours
        STX SCREENCOL
        LDA TMP
        STA BORDERCOL

         RTS

;--------------------------------------
; ACTION RANGE
;--------------------------------------
; Check if moomin is at action range
; to an object action is supposed to
; be taken to.
; X must contain the index of object
; to OBJXXX tables.
;--------------------------------------
ACTRANGE
         ;-----------------------------
         ; clear range bit in ACTBITS
         ;-----------------------------
         LDA ACTBITS
         AND #$FE
         STA ACTBITS
         
         ;-----------------------------
         ; compare objects X-location
         ; to moomin's X-location
         ;-----------------------------
         LDA OBJX,X
         CMP MOOX
         BCC ACRANGX2
         
         ;-----------------------------
         ; x-location is more than
         ; or equal to sprite 1 x
         ;-----------------------------
         SEC
         LDA OBJX,X
         SBC MOOX
         
         ;-----------------------------
         ; if location is under 30 pix
         ; continue
         ;-----------------------------
         CMP #$1E
         BCC ACRANGY1
         
         ;-----------------------------
         ; otherwise leave
         ;-----------------------------
         RTS
         
ACRANGX2 ;-----------------------------
         ; x-location is less than
         ; sprite 1 x-location
         ;-----------------------------
         SEC
         LDA MOOX
         SBC OBJX,X
         
         ;-----------------------------
         ; if location is under 30 pix
         ; continue
         ;-----------------------------
         CMP #$1E
         BCC ACRANGY1
         
         ;-----------------------------
         ; otherwise leave
         ;-----------------------------
         RTS

ACRANGY1
         ;-----------------------------
         ; compare objects y-location
         ; to moomin's y-location
         ;-----------------------------         
         LDA OBJY,X
         CMP MOOY
         BCC ACRANGY2
         
         ;-----------------------------
         ; y-location is more than
         ; or equal to
         ; sprite 1 y-location
         ;-----------------------------
         
         SEC
         LDA OBJY,X
         SBC MOOY
         ;-----------------------------
         ; if location is under 30 pix
         ; continue
         ;-----------------------------
         CMP #$1E
         BCC ACRANOK
         
         ; otherwise leave
         RTS
         
ACRANGY2 ;-----------------------------
         ; y-location is less than
         ; sprite 1 y-location
         ;-----------------------------
         SEC
         LDA MOOY
         SBC OBJY,X
         
         ;-----------------------------
         ; if location is under 30 pix
         ; continue
         ;-----------------------------
         CMP #$1E
         BCC ACRANOK
         
         ; otherwise leave
         RTS
         
ACRANOK  ;-----------------------------
         ; set range bit to ACTBITS
         ; Action range OK!
         ;-----------------------------
         INC SCREENCOL
         LDA ACTBITS
         ORA #$01
         STA ACTBITS
         RTS
        
;======================================
;
;         U S E R  I N P U T
;
;======================================
;--------------------------------------
; READ JOYSTICK
;--------------------------------------
READJOY 
        ; if JOYSTICK HAS xxx11111
        ; nothing has to be done 
         ;-----------------------------
         ; clear moostate bits other
         ; than bit 0 (moving/not)
         ;-----------------------------
         LDA MOOSTATE
         AND #$FE ;
         STA MOOSTATE
         
        ; read button first (5th bit)
        LDA JOYSTICK
        AND #$10
        BNE UP
        ; button is pressed, go to button actions
        JSR JOYBUTTON
        RTS        
UP ; move up or jump at place
        JSR CLEARFIGHT
         LDA JOYSTICK
         AND #$01
         BNE DOWN
        ;if first bit is set, move up
        ;LSR A
        ;BCS DOWN 

         JSR MOVEUP 
DOWN
         LDA JOYSTICK
         AND #$02
         BNE LEFT
        ;LSR A
        ;BCS LEFT
         JSR MOVEDOWN
LEFT
         LDA JOYSTICK
         AND #$04
         BNE RIGHT
        ;LSR A
        ;BCS RIGHT
         JSR MOVELEFT
RIGHT
         LDA JOYSTICK
         AND #$08
         BNE LEAVEJ
        ;LSR A
        ;BCS LEAVEJ
         JSR MOVERGT

LEAVEJ   RTS


;--------------------------------------
; JOYBUTTON
; joystick button is pressed
; read other joystick bits for action
;--------------------------------------
JOYBUTTON

TAKE     ;-----------------------------
         ; TAKE item
         ;-----------------------------
         LDA JOYSTICK
        LSR A
        BCS DROP
         ;AND #$11; 0001 0001
         ;BNE UP
         JSR COLTRS
         RTS
DROP    
        ;LDA JOYSTICK
        ; AND #$12; 0001 0010
        ; BNE TAKE
        LSR A
        BCS FIGHTL
         JSR DROPOBJ
         RTS
         
FIGHTL
         ;LDA JOYSTICK
         ;AND #$10; 0001 0000
        LSR A
        BCS FIGHTR
         ;BNE LEAVEJ
         JSR SETFIGHTL
        rts
FIGHTR
        LSR A
        BCS JOYBUTX
        JSR SETFIGHTR
        rts
        
JOYBUTX
        JSR CLEARFIGHT
        RTS

;--------------------------------------
; READ KEYBOARD
;--------------------------------------
READKEY 
        ; load keypress
        LDA KEYPRESS

         ; no keypress? exit
         CMP #$40 
         BEQ READKEX
         
         ; A (action)
         CMP #$0A
         BNE READKED
         JSR ACTION
         JMP READKEX

         ; D (drop)
READKED  CMP #$12 
         BNE READKERS
         JSR DROPOBJ
         JMP READKEX

         ; RUN/STOP
READKERS CMP #$3F 
         BNE READKET
         JSR GAMEOVER
         JMP READKEX

         ; T (take)
READKET  CMP #$16 
         BNE READKPL
         JSR COLTRS
         JMP READKEX
        
        ; -
READKPL CMP #$2B
        BNE READKMN
        DEC ROOM
        JSR INCROOM
        JMP READKEX

; +
READKMN CMP #$28
        BNE READF1
        INC ROOM
        JSR INCROOM
        JMP READKEX

;F1
READF1        CMP #$04
        BNE READF3
        JSR COLLOFF
        JMP READKEX

;F3
READF3        cmp #$05
        bne READK1
        jsr COLLON
        jmp READKEX

READK1  ;1
        CMP #$38
        BNE READKEX
        lda #$0a ; set amount of energy boost
        sta tmp
        JSR INCLIVES

READKEX  RTS

;--------------------------------------
; read joystick button
;--------------------------------------
READJOYB
        LDA #$FF
        STA JOYSTICK
READJLO
        ; read joy 1
        LDA JOYSTICK
        ; check if button pressed
        AND #$10
        BNE READJLO
        RTS

;--------------------------------------
; WAITS FOR RET KEY
;--------------------------------------
READRET
         LDA #$00
         STA $C6 ; number of chars in keyboard buffer
READRELO
         LDA KEYPRESS
         CMP #$01
         BNE READRELO

         RTS
;======================================
;
;    S P E C I A L  S C R E E N S
;
;======================================

;--------------------------------------
; TITLE SCREEN ($1D)
;--------------------------------------
TITLESCR JSR CLEARSCR
         LDA #$1D ; title screen data as room no. 
         STA ROOM
         JSR INITROOM

         LDX #<TXTITLE2
         LDY #>TXTITLE2
         JSR print
         
         LDX #<TXTJOY
         LDY #>TXTJOY
         JSR print
         
         LDX #<TXCOPY
         LDY #>TXCOPY
         JSR print
        
        ldx #<txtins
        ldy #>txtins
        jsr print

        LDX #$FF
TITLELO
        DEX
        BNE TITLEL1
        LDX #$FF
        JSR ANICHR
titlel1
        ; read joystick
        LDA JOYSTICK
        ; check if button pressed
        AND #$10
        BNE TITLEL2
        JMP TITLEX
TITLEL2
        ; read keypress
        LDA KEYPRESS
        ; if no keypress, loop
        CMP #$40
        BEQ TITLELO
        ; if f1 key show instructions
        CMP #$04
        BNE TITLELO
        JSR INSTR
        
        JMP TITLELO
        
TITLEX  ; set the initial room
        LDA $1e ; #$00
        STA ROOM
         RTS
         
;--------------------------------------
; GAME OVER
;--------------------------------------
GAMEOVER
         ; JSR CLEARSCR
        
         ; clear area
         ; to game over msg
         LDY #$0E
GAMEOVE1
         LDA #$20 ; empty
          STA $04A8,Y
         STA $04D0,Y
         STA $04F8,Y
         STA $0520,Y
         DEY
         BNE GAMEOVE1

         ; set pointer to screen memory
         ; for text ($04FB)

         LDA #$04
         STA SCRHI
         LDA #$FB
         STA SCRLO

         LDX #<TXTGOVER
         LDY #>TXTGOVER
         JSR print
         JSR READJOYB
         JSR NEWINI
         RTS

;--------------------------------------
; GAME SOLVED
;--------------------------------------
CONGRATS
         JSR CLEARSCR
         
         LDX #<TXTCONG
         LDY #>TXTCONG
         JSR print
         
         JSR READRET
         JSR READRET
         JSR READRET
         
         JSR TITLESCR
         JSR NEWINI
         
         RTS
;======================================
;
;    M O O M I N  M O V E M E N T
;
;======================================
;--------------------------------------
; set collision detection off
;--------------------------------------
COLLOFF
        lda MOOCOLL
        ora #$80 ; 10000000 bit 7 on
        sta MOOCOLL
        rts


; set collision on
COLLON
        lda MOOCOLL
        and #$7F ; 01111111 bit 7 off
        sta MOOCOLL
        rts
;--------------------------------------
; HIDEOBJ
;--------------------------------------
; Hide an sprite object
; X must contain index to OBJXXX-tables
;--------------------------------------
HIDEOBJ
         ; set invisible bit to RMOBSTA
         ; get RMxxx table index 1st
         LDY OBJRMIDX,X
         ; get RMOBSTA state value
         LDA RMOBSTA,Y
         ; set bit 7 on
         ORA #$80
         STA RMOBSTA,Y         
         ; so this sprite object will
         ; be loaded to the room
         ; until RMOBSTA values
         ; are restored in the start
         ; of new game f.ex.
         
         ; re-init 'enemy objects'
         JSR INIENEMY
         RTS
 ;--------------------------------------
; INITIALIZE MOOMIN
;--------------------------------------
INITMOO
         ; Set moomin sprite on (Sprite 0) by
         ; setting the 1st bit in SPRDISPLAY.
         
         LDA SPRDISPLAY
         ORA #$01
         STA SPRDISPLAY
         
         ; Set moomin sprite - sprite 0 - 
         ; to multicolor mode
         ; by setting the first bit in SPRMULTICOL.

         LDA SPRMULTICOL
         ORA #$01
         STA SPRMULTICOL
         
         ; Set sprite 0 colour. 
         ; #$01 for white in SPR0COL.
         ; Set sprite 0 multi-colour mode colours. 
         ; #$01 for white in SPRMCOL1.
         ; #$0e for light blue in SPRMCOL2.

         LDX #$01
         STX SPR0COL
         STX SPRMCOL1
         LDY #$0E
         STY SPRMCOL2
         
         ; Set moomin sprite data pointer to $2100 (Moomin
         ; sprite object has 5 frames from $2000 to $2100)
         ; by setting value #$84 to sprite 0 data pointer 
         ; address SPR0DAT.

         LDA #$84
         STA SPR0DAT

         ; Set initial location for moomin sprite (sprite 0)
         ; from INIXLOC and INIYLOC to MOOX and MOOY.
  
         LDX INIXLOC
         STX MOOX
         LDY INIYLOC
         STY MOOY
         
         ; Reset 'fall counter' FALLDCNT.
         
         LDA #$00
         STA FALLDCNT

        ; reset jump counter and clear
        ; jump state
        JSR JUMPOFF

         RTS
         
 
;--------------------------------------
; SET LEFT BIT & MOVEMENT BIT
;--------------------------------------
SETLEFT

         ; set direction bit (bit 1) to
         ; left (state 0) and movement bit on (bit 0)
         
         LDA MOOSTATE
         AND #$FD ; 1111 1101
         ORA #$01
         STA MOOSTATE
         
         RTS
         
;--------------------------------------
; SET RIGHT BIT
;--------------------------------------
SETRGT
         LDA MOOSTATE
         ORA #$03
         STA MOOSTATE
         
         RTS
         
;--------------------------------------
; CLEAR MOVEMENT BIT
;--------------------------------------
USETMOV
         LDA MOOSTATE
         AND #$FE ; 11111110
         STA MOOSTATE
         RTS
         
         
;--------------------------------------
; SET JUMP STATE ON
;--------------------------------------
MOVEJUMP
         ; sets jump bit on
         LDA MOOSTATE
         ORA #$10 ; 0001 0000
         STA MOOSTATE
         
         ; jump sound
         
         ; X must be set for SFXINIT
         ; to index no. of sound
         LDX #$00 
         JSR SFXINIT

         RTS
         
;--------------------------------------
; DO JUMP
;--------------------------------------
MOOJMP
         LDA JMPCNT
         CMP #$1E
         BEQ ENDJMP

         INC JMPCNT

         ; going up or down
         CMP #$0F
         BCS JMPINY
         BCC JMPDEY
         
         
JMPINY
         ; going down
         
         ; check collision, if happened
         ; end jump
         
         LDA MOOCOLL
         AND #$08
         BNE ENDJMP
         
         ; check if on platform sprite
         ; object, end jump if true
         
         LDA ACTBITS
         LSR A
         LSR A
         BCS ENDJMP

         INC MOOY
         JMP DIRJMP
JMPDEY
         ; going up
         LDA MOOCOLL
         AND #$04
         BNE ENDJMP

         DEC MOOY

         ; check if direction jump
         ; from MOOSTATE
         
DIRJMP   LDA MOOSTATE
         LSR A ; 1ST BIT (MOVEMENT)
         BCC LVJMP

         LSR A
         BCS JMPRGT
         
         ; jump to left
JMPLFT
         LDA MOOCOLL
         AND #$01
         BNE ENDJMP
         
         ; check joystick if 
         ; turned to right
         
         LDA JOYSTICK
         AND #$08
         BNE JMPLFT2
         
         ; stop direction jump
         
         JSR USETMOV
         
         ; check jump counter
         
         LDA JMPCNT
         CMP #$0F
         BCS LVJMP
         
         ; set going down
         
         LDA #$0E
         STA JMPCNT
         
         JMP LVJMP
JMPLFT2
         DEC MOOX
         JMP LVJMP
         
         ; jump to right
JMPRGT
         LDA MOOCOLL
         AND #$02
         BNE ENDJMP
         
         ; check joystick if 
         ; turned to left

         LDA JOYSTICK
         AND #$04
         BNE JMPRGT2
         
         ; stop direction jump
         
         JSR USETMOV
         
         ; check jump counter
         
         LDA JMPCNT
         CMP #$0F
         BCS LVJMP
         
         ; set going down
         
         LDA #$0E
         STA JMPCNT
         
         JMP LVJMP
JMPRGT2
         INC MOOX
         JMP LVJMP
         
ENDJMP
        JSR JUMPOFF
LVJMP
         RTS
;--------------------------------------
; EXIT JUMP STATE
;--------------------------------------

JUMPOFF
        ; sets jump bit off
        LDA MOOSTATE
        AND #$EE
        STA MOOSTATE
        ; clear jump counter
        LDX #$00
        STX JMPCNT
        RTS
        
;--------------------------------------
; MOOMIN FALLS
;--------------------------------------
MOOFALL
        ; increase moomin y-location
         INC MOOY
        ; increase fall counter
         INC FALLDCNT
         LDA FALLDCNT
         CMP #$40
         BNE FALLEXIT
        ; after fall counter has reached $40
        ; fall is fatal!
        lda #$05 ; set damage to tmp
        sta tmp
         JSR DECLIVES
         JSR INITMOO
FALLEXIT
         RTS

;--------------------------------------
; Sets the fight mode off 
;--------------------------------------
CLEARFIGHT
        LDA MOOSTATE
        ; set fight bit (8th bit) off
        AND #$7f ;0111 1111
        STA moostate
        rts
        
;--------------------------------------
SETFIGHTL
        JSR SETFIGHT
        RTS
;--------------------------------------
SETFIGHTR
        JSR SETFIGHT
        RTS
;--------------------------------------
; SETS FIGHT MODE ON
;--------------------------------------
SETFIGHT
        ; check 1st moomin state bits 
        ; to see if fighting is possible
        LDA MOOSTATE
        ; 0111 0000 -> jumping/falling/climbing
        AND #$70 ; BEQ-> fighting is possible 
        BNE SETFIGHTX
        
        ; set moomin state bit (8th bit) for fighting on:
        LDA MOOSTATE
        ORA #$80 ; 1000 0000
        STA MOOSTATE 
        INC BORDERCOL ; Debug

SETFIGHTX
        RTS ; FIGHT ENDS

;--------------------------------------
ANIMFIGHT
        ; if animation counter is 0 -> animate
        ldx moocnt
        beq ANIMFIGHT1
        ; else decrease counter and leave
        dec moocnt
        rts
ANIMFIGHT1
        ; set animation counter to initial value
        ldx #$05
        stx moocnt
        ; check the direction moomin
        ; is facing (l/r)
        LDA MOOSTATE
        lsr a
        lsr a
        bcs animfightr
        ; facing left
        LDY #$ba ; 1st fight frame facing left
        cpy spr0dat ; compare to current frame
        bne ANIMFIGHTX
        INY ; INCREASE TO NEXT FRAME 
        jmp animfightx
        
ANIMFIGHTR
        ; facing right
        LDY #$b8 ; 1st fight frame facing right 
        cpy spr0dat ; compare to current frame
        bne ANIMFIGHTX
        INY ; INCREASE TO NEXT FRAME
ANIMFIGHTX
        ; set selected fight frame to moomin
        ; sprite data pointer from y-reg
        STY SPR0DAT
        rts
;--------------------------------------
; MOVE UP
;--------------------------------------
; Move moomin up when stick up.
;--------------------------------------
MOVEUP
        ; check climbable up bit
        ; if the background surface is climbable
        ; if it is not, do a jump
        LDA MOOCOLL
        AND #$10
        BNE MOVEUP1
        JSR MOVEJUMP 
        RTS

MOVEUP1  ; set MOOSTATE to up
         LDA MOOSTATE
         ORA #$04 ; 00000100
         STA MOOSTATE
        ; decrease y-location
         DEC MOOY
        ; if animation counter is 0
         LDX MOOCNT
         BEQ FRAME1UP
        ; else decrease counter and leave
         DEC MOOCNT
         JMP LEAVEMUP
        
FRAME1UP ; set animation counter to initial value
         LDX #$05
         STX MOOCNT
        
        ; check sprite frame at SPR0DAT (moomin sprite data pointer)
        ; first frame up is #$88
         LDY #$88
         CPY SPR0DAT
         BNE SETFRUP ; frame is #$89, set to #$88
         ; frame is #$88, set to #$89
         INY

SETFRUP  ; store new frame data value to moomin
        ; sprite data pointer
        STY SPR0DAT

LEAVEMUP RTS

;--------------------------------------
; MOVE DOWN
;--------------------------------------
MOVEDOWN
         LDA MOOSTATE
         AND #$FB
         STA MOOSTATE

         ; check bg collision down bit
         ; (must NOT be set)
         ; and climbable down bit
         ; (must be set)
         
         LDA MOOCOLL
         LSR A
         LSR A
         LSR A
         LSR A
         BCS LEAVEMDW ; bg coll down
         LSR A
         LSR A
         BCC LEAVEMDW ; climb down bit
        
        ; increase y-location
         INC MOOY

         LDX MOOCNT
         BEQ FR1DWN

         DEC MOOCNT

         JMP LEAVEMDW

FR1DWN   LDX #$05
         STX MOOCNT

         LDY #$88 ; $88 -> first frame down
         CPY SPR0DAT
         BNE SETFRDWN

         INY

SETFRDWN STY SPR0DAT

LEAVEMDW RTS

;--------------------------------------
; MOVE LEFT
;--------------------------------------
MOVELEFT
         JSR SETLEFT

         LDA MOOCOLL
         AND #$01
         BNE LVMVLFT
        ; decrease x location
         DEC MOOX

         LDX MOOCNT
         BEQ CHFRLFT

         DEC MOOCNT
         JMP LVMVLFT

CHFRLFT  LDX #$05
         STX MOOCNT

         LDX FRAME
         BNE NXTFRM

         LDX #$03
         STX FRAME
NXTFRM
         DEC FRAME
LVMVLFT
         CLC
         LDA #$83 ; $83 -> first frame left
         ADC FRAME
         STA SPR0DAT
         RTS

         
;--------------------------------------
; MOVE RIGHT
;--------------------------------------
MOVERGT
        ; set MOOSTATE right bit on
         JSR SETRGT

        ; check if collision right bit set
         LDA MOOCOLL
         AND #$02
         BNE LVMVRGT

        ; increase x location
         INC MOOX

        ; if moomin animation counter
        ; has decreased to zero change
        ; to next frame-right
         LDX MOOCNT
         BEQ CHFRRGT
        ; else decrease animation counter
        ; and leave
         DEC MOOCNT
         ; JMP LVMVRGT
        rts
        ; animate next frame
CHFRRGT  
        ; set animation counter to initial value 
        LDX #$05
         STX MOOCNT

        ; load current frame count value to x-reg
         LDX FRAME
         BNE NXFRRG
        ; if frame count value has reached zero
        ; load initial value to frame counter
         LDX #$03
         STX FRAME
NXFRRG        
        ; decrease frame count
         DEC FRAME
LVMVRGT
        ; set moomin sprite data pointer SPR0DAT to 
        ; $80 + value at the frame counter FRAME
         CLC
         LDA #$80 ; $80 -> first frame right
         ADC FRAME
         STA SPR0DAT
         RTS

;--------------------------------------
; ELEVATOR MOVEMENT
;--------------------------------------
; Moves moomin to the direction of 
; elevator char's direction bits
; X MUST contain the index of character 
; in ELECHRS datatable!
;--------------------------------------
MOVELEV
         ; get direction bits
         LDA ELECHDIR,X
         LSR A ; left
         BCC COLDWRG
         ; move left
         DEC MOOX
COLDWRG
         LSR A; right
         BCC COLDWUP
         ; move right
         INC MOOX
COLDWUP
         LSR A; up
         BCC COLDWDN
         ; move up
         DEC MOOY
COLDWDN
         LSR A; down
         BCC MOVELEX
         ; move down
         INC MOOY
MOVELEX
         RTS

;======================================
;     S P R I T E  O B J E C T S
;       movement and animation
;======================================

;--------------------------------------
; INITIALIZES 'ENEMY' SPRITE OBJECTS IN
; CURRENT ROOM
; - check all the sprobj's defined for
;   current room from RMNUM datatable
;   (the whole table is checked)
; - load all the sprobj data (static data) 
;   for each
;   sprobj in current room using LDOBJDAT sbr
;   to OBJXXX tables (dynamic data)
; - initialize sprite locations 
;   with SETSPLOC sbr
; - set sprites to screen
;--------------------------------------
INIENEMY ; clear sprite counter, Y and
         ; RM...- data table index X
         LDY #$00
         LDX #$00
         STX SPRCNT

         ; check all the sprites defined in RMNUM datatable
         ; for current room until end sign $ff is reached
INIENELO
         LDA RMNUM,X
          ; exit if end sign $FF
          CMP #$FF
         BEQ INIENEMX
        
         ; check if sprobj is defined
         ; for this room
         CMP ROOM
         BNE INIENELN
         
         ; check if this sprite object
         ; should be viewed
         ; (bit 7 invisible bit)
         
         LDA RMOBSTA,X
         AND #$80
         BNE INIENELN

         ; load data for sprite object
         ; and increase sprite counter Y
         JSR LDOBJDAT
         INY
         ; no more than 7 spr objects can be
         ; inserted to screen in addition to 
         ; moomin spr object
         CPY #$07
         BEQ INIENEMX

INIENELN INX
         JMP INIENELO
         ; loop ends

INIENEMX
         ; if no sprites found leave
         CPY #$00
         BEQ INIENEX

         ; else store sprite count
         STY SPRCNT

         ; and initialize sprite objs
         JSR SETSPLOC
INIENEX
         ; IMPORTANT!:
         ; set amount of sprite objects in 
         ; current room back to Y-reg
         LDY SPRCNT

         ; clear all sprites other than
         ; moomin ... 
         LDA SPRDISPLAY
         AND #$01
         ; ...and set needed amount of sprites visible
         ORA SPRORA,Y
         STA SPRDISPLAY
         RTS

;--------------------------------------
; LOAD SPRITE OBJECT DATA
;--------------------------------------
; Y contains current sprite number and
; X contains current index to RMXXX datatables
;--------------------------------------
LDOBJDAT 
         ; store sprite objects
         ; RMxxx-table index
         TXA
         STA OBJRMIDX,Y

        ; store sbrobj state 
         LDA RMOBSTA,X
         STA OBJSTA,Y
        
        ; store animation speed
         LDA RMANISP,X
         STA OBJANISP,Y

        ; store movement step
         LDA RMMOVSTP,X
         STA OBJMOVSTP,Y

        ; store movement speed
         LDA RMMOVSP,X
         STA OBJMOVSP,Y

        ; store min x location
         LDA RMMINX,X
         STA OBJMINX,Y

        ; store max x location
         LDA RMMAXX,X
         STA OBJMAXX,Y

        ; store min y location
         LDA RMMINY,X
         STA OBJMINY,Y

        ; store max y location
         LDA RMMAXY,X
         STA OBJMAXY,Y

        ; store initial location
         LDA RMINIX,X
         STA OBJX,Y
         LDA RMINIY,X
         STA OBJY,Y

        ; store colour
         LDA RMSPRCOL,X
         STA OBJSPCOL,Y
         ; set colour
         STA SPR1COL,Y

         ; store SPRITE ID
         LDA RMSPRID,X
         STA OBJID,Y

        ; ROOM SPECIFIC sprobj info
        ; has been loaded
        ; next load SPRITE SPECIFIC
        ; info

         ; store X
         STX TMPCNT
         ; set object's id to X
         TAX

         ; OBJ.SPR INFO
         ; load with sprite id as index

        ; load sprite state (multi/mono...)
         LDA SPRSTA,X
         STA OBJSPST,Y

         ; check if multicolor or mono
         LSR A
         BCC LDMONO

         ; set multicolor sprite
         LDA SPRMULTICOL
         ORA MCORA,Y
         JMP LDPOINT
         
         ; set monocolor sprite
LDMONO
         LDA SPRMULTICOL
         AND MOAND,Y

LDPOINT
         STA SPRMULTICOL

        ; store sprite data pointers
        ; 1st store pointer to still frame
         LDA SPRPNST,X
         STA OBJPNST,Y
          ; and set it to current frame of 
        ; the sprobj
         STA OBJFRCUR,Y
         STA SPR1DAT,Y

        ;store pointer to facing left 1st frame
         LDA SPRPNLF,X
         STA OBJPNLF,Y

        ;store amount of frames in horizontal movement        
         LDA SPRFRLF,X
         STA OBJFRLR,Y

        ; store pointer to facing right 1st frame
         LDA SPRPNRG,X
         STA OBJPNRG,Y

        ; store pointer to facing uo
         LDA SPRPNUP,X
         STA OBJPNUP,Y

        ; store amount of frames in vertical movement
         LDA SPRFRUP,X
         STA OBJFRUD,Y

        ; store pointer to facing down
         LDA SPRPNDW,X
         STA OBJPNDW,Y

        ; store collision area data
         CLC
         LDA SPRCOLX1,X
         ADC OBJX,Y
         SBC #$10
         STA OBJCOLX1,Y

         CLC
         LDA SPRCOLX2,X
         ADC OBJX,Y
         SBC #$10
         STA OBJCOLX2,Y

         CLC
         LDA SPRCOLY1,X
         ADC OBJY,Y
         SBC #$10
         STA OBJCOLY1,Y

         CLC
         LDA SPRCOLY2,X
         ADC OBJY,Y
         SBC #$10
         STA OBJCOLY2,Y

        ; reset frame counter
         LDA #$00
         STA OBJFRCNT,Y

        ; reset animation speed counter
        STA OBJANICN,Y

        ; reset movement speed counter
        STA OBJSPCNT,Y

        ; restore x
         LDX TMPCNT

         RTS
         
;--------------------------------------
; SET SPRITE LOCATIONS
;--------------------------------------
SETSPLOC LDX #$00
         LDY #$00

SETSPLOO LDA OBJX,X
         STA SPR1XLOC,Y
         LDA OBJY,X
         STA SPR1YLOC,Y

         INX
         INY
         INY
         CPX SPRCNT
         BNE SETSPLOO
         RTS
         
;--------------------------------------
; SET SPRITE FRAMES
;--------------------------------------
SETSPFRA LDX #$00

SETSPFLO LDA OBJFRCUR,X
         STA SPR1DAT,X
         INX
         CPX SPRCNT
         BNE SETSPFLO
         RTS

;--------------------------------------
; SPRITE OBJECT MOVEMENT
; (other than moomin)
;--------------------------------------
OBJANI 
        ; check first amount of sprite obects
        ; in the current room, leave if 0

        LDA SPRCNT
        BEQ OBJANIX
        
        ; clear loop counter (X-reg)
        LDX #$00

        ; loop all the sprites
        ; in current screen (X-reg)
OBJANILO

        ; check first movement speed counter
        ; for sprite X if it's time to get movin

        LDA OBJSPCNT,X
        ; if counter 0 continue
        BEQ OBJANILO2
        
        ; decrease movement speed counter
        DEC OBJSPCNT,X
        ; and loop to next sprite obj
        jmp OBJANILN

OBJANILO2
        ; set movement speed counter to initial value
        LDA OBJMOVSP,X
        STA OBJSPCNT,X

        ; a screen can hold currently
        ; four sprite objects
        ; OBJSTA table holds the state
        ; for each sprobj

        ; load sprobj properties
        ; (sprobj state)
        ; from OBJSTA-table
        ; check the movement bit
        ; (LSR puts bit to C-flag,
        ; bcc branches if C is set)

         LDA OBJSTA,X
         LSR A
         BCC OBJANILN
         LSR A

        ; roll to 'facing left'-bit
        ; if set jump to move left sbr
OBJANILF LSR A
         BCC OBJANIRG
         JSR OBJLFT

        ; roll to 'facing right'-bit
        ; if set, jump to move right sbr
OBJANIRG LSR A
         BCC OBJANIUP
         JSR OBJRGT

        ; roll to 'facing up' bit,
        ; call 'move up' sbr, if set
OBJANIUP LSR A
         BCC OBJANIDW
         JSR OBJUP
        
        ; roll to 'facing down' bit,
        ; call 'move down' sbr if set
OBJANIDW LSR A
         BCC OBJANILN
         JSR OBJDWN

        ; increase counter
        ; and loop
OBJANILN
        ; increase counter and loop if not 
        ; yet amount of sprites in current room
        INX
        CPX SPRCNT
        BNE OBJANILO
        
        ; set sprite locations
         JSR SETSPLOC

         ; set sprite animation frame
         JSR SETSPFRA

OBJANIX  RTS
;--------------------------------------
; OBJECT LEFT
; -----------
; X-reg must hold the sprite counter
; value
;--------------------------------------
OBJLFT 
        ; store acc
        STA TMP

         ; load object x location
         ; and check if minimum x has
         ; already reached (need turn?)
         LDA OBJX,X
         CMP OBJMINX,X
         BEQ OBJLFCH
         BCC OBJLFCH

         ; substract amount of movement speed
         ; from object's x-location (in the acc)
         SEC
         SBC OBJMOVSTP,X
         ; store new x-location
         STA OBJX,X

        ; move also collision areas
         SEC
         LDA OBJCOLX1,X
         SBC OBJMOVSTP,X
         STA OBJCOLX1,X
         SEC
         LDA OBJCOLX2,X
         SBC OBJMOVSTP,X
         STA OBJCOLX2,X

         ; CHECK IF ANIMATION BIT SET:
         LDA OBJSTA,X
         LSR A
         LSR A
         BCC OBJLF2

        ; ... if set, animate
         JSR OBJANIH 
OBJLF2
         JMP OBJLFX

OBJLFCH  ; CHANGE MOV DIRECTION

         LDA OBJSTA,X
         AND #$FB
         ORA #$08
         STA OBJSTA,X

         LDA #$00
         STA OBJFRCNT

OBJLFX   LDA TMP
         RTS
;--------------------------------------
; MOVE RIGHT
;
; X-reg must hold the sprite counter
; value
;--------------------------------------
OBJRGT   STA TMP

         LDA OBJX,X
         CMP OBJMAXX,X
         BEQ OBJRGCH
         BCS OBJRGCH

         CLC
         ADC OBJMOVSTP,X
         STA OBJX,X

         CLC
         LDA OBJCOLX1,X
         ADC OBJMOVSTP,X
         STA OBJCOLX1,X

         CLC
         LDA OBJCOLX2,X
         ADC OBJMOVSTP,X
         STA OBJCOLX2,X

         LDA OBJSTA,X
         LSR A
         LSR A
         BCC OBJRG2

         JSR OBJANIH

OBJRG2   JMP OBJRGX

OBJRGCH  LDA OBJSTA,X
         AND #$F7
         ORA #$04
         STA OBJSTA,X

         LDA #$00
         STA OBJFRCNT

OBJRGX   LDA TMP

         RTS
;--------------------------------------
;OBJECT UP
;
; X-reg must hold the sprite counter
; value
;-------------------------------------0-
OBJUP    STA TMP

         LDA OBJY,X
         CMP OBJMINY,X
         BEQ OBJUPCH
         BCC OBJUPCH

         SEC
         SBC OBJMOVSTP,X
         STA OBJY,X

         SEC
         LDA OBJCOLY1,X
         SBC OBJMOVSTP,X
         STA OBJCOLY1,X

         SEC
         LDA OBJCOLY2,X
         SBC OBJMOVSTP,X
         STA OBJCOLY2,X
         
         ; CHECK IF ANIMATION BIT SET:
         LDA OBJSTA,X
         LSR A
         LSR A
         BCC OBJUP2

         JSR OBJANIH
OBJUP2
         JMP OBJUPX

OBJUPCH  LDA OBJSTA,X
         AND #$EF
         ORA #$20
         STA OBJSTA,X

         LDA #$00
         STA OBJFRCNT

OBJUPX   LDA TMP
         RTS
;--------------------------------------
; OBJECT DOWN
;
; X-reg must hold the sprite counter
; value
;--------------------------------------
OBJDWN   STA TMP

         LDA OBJY,X
         CMP OBJMAXY,X
         BEQ OBJDWCH
         BCS OBJDWCH

         CLC
         ADC OBJMOVSTP,X
         STA OBJY,X

         CLC
         LDA OBJCOLY1,X
         ADC OBJMOVSTP,X
         STA OBJCOLY1,X

         CLC
         LDA OBJCOLY2,X
         ADC OBJMOVSTP,X
         STA OBJCOLY2,X
         
         ; CHECK IF ANIMATION BIT SET:
         LDA OBJSTA,X
         LSR A
         LSR A
         BCC OBJDW2

         JSR OBJANIH
         
OBJDW2
         JMP OBJDWX

OBJDWCH  LDA OBJSTA,X
         AND #$DF
         ORA #$10
         STA OBJSTA,X

         LDA #$00
         STA OBJFRCNT

OBJDWX   LDA TMP
         RTS
;--------------------------------------
; ANIMATE SPRITE OBJECT
;--------------------------------------
; X-register must be set to point
; to current object in datatable.
;--------------------------------------
OBJANIH 
        ; animation is done only if 
        ; animation counter of a sprobj
        ; reaches the animation speed 
        ; value set to it

        ; check animation speed-counter
         ; compare to the value set
         ; to the object in OBJANISP
         ; ~check if it's time to change
         ; animation frame
         LDA OBJANICN,X
         CMP OBJANISP,X
         BNE OBANIHX

         ; set animation speed-counter
         ; to 0
         LDA #$00
         STA OBJANICN,X
        

        ; check the direction the sprobj
        ; is heading and load the pointer
        ; to 1st frame of the animation

         ; animate to l/r/u/d?
         LDA OBJSTA,X
         LSR A ; roll movement bit
         LSR A ; roll animation bit
         LSR A ; roll facing left -bit
        ; ... if set animate to left
         BCS OBJANIHL
         LSR A ; right
         BCS OBJANIHR
         LSR A ; up
         BCS OBJANIHU
         ; otherwise continue to animate down
        
        ; load pointer to 1st animation down -frame to acc
         LDA OBJPNDW,X
         JMP OBJANIHC
         
OBJANIHL
         ; load sprite-left 1st frame
         ; pointer
         LDA OBJPNLF,X
         JMP OBJANIHC
         
OBJANIHR 
         ; loads sprite-right 1st frame
         ; pointer
         LDA OBJPNRG,X
         JMP OBJANIHC
         
OBJANIHU
         ; load pointer to 1st animation up -frame
         LDA OBJPNUP,X
         JMP OBJANIHC
         
OBJANIHC
         ; add current value from 
         ; object frame counter to acc
         ; (the pointer to 1st frame
         ; is already in the acc)
         CLC
         ADC OBJFRCNT,X
         ; store new frame pointer value
         STA OBJFRCUR,X
         
         ; horizontal or vertical
         ; movement?
         LDA OBJSTA,X
         LSR A ; movement bit
         LSR A ; animation bit
         LSR A ; left
         BCS OBJANIHH
         LSR A ; right
         BCS OBJANIHH

        ; vertical animation frame check
        
         ; compare current frame 
         ; counter value
         ; to the amount of vertical animation
         ; frames for the sprobj
         ; if reach, reset the counter
         LDA OBJFRCNT,X
         CMP OBJFRUD,X
         BEQ OBANIHZ

         ; added the following two lines,
         ; not sure though if the horizontal
         ; animation frames should also be 
         ; checked

         ; increase frame counter
         INC OBJFRCNT,X
         JMP OBANIHX 

OBJANIHH
        ; horizontal animation frame check

        ; compare the frame counter to the
        ; amount of horizontal animation frames
        ; for the sprobj
        ; if reached, reset counter
         LDA OBJFRCNT,X
         CMP OBJFRLR,X
         BEQ OBANIHZ

         ; increase frame counter
         INC OBJFRCNT,X
         JMP OBANIHX

OBANIHZ  
         ; set frame counter to 0
         LDA #$00
         STA OBJFRCNT,X

OBANIHX  
         ; increase animation
         ; speed timer counter for the sprobj
         INC OBJANICN,X

         RTS

;--------------------------------------
; clear sprites
;--------------------------------------
CLEARSPR
        ; clear sprite counter
        lda #$00
        sta sprcnt

        ; clear all sprites
        lda SPRDISPLAY
        and #$00
        sta SPRDISPLAY

        rts

;======================================
;
;            S T R I N G S
;
;======================================

;--------------------------------------
; initializes string messages
; and prints string messages to screen
; using printstr subroutine
; 
; X, Y must contain the start adress
; of string data - 
; X low order, Y high order byte.
; the string data _must_ be in the following format:
; 1st byte : screen mem. location low order byte
; 2nd byte : screen mem. location high order byte
; 3rd byte : colour
; 4th byte : width of the message column
; Xth byte : string data
; $00      : end sign
; NOTE: A String cannot be more than 255 bytes long!
 
print
        ; point LVCHLO/HI to string data
        stx LVCHLO
        sty LVCHI

        ; read the screen memory location
        ; for the string to msgloclo/hi
        ldy #$00
        lda (lvchlo),y
        sta scrlo
        iny
        lda (lvchlo),y
        sta scrhi
        ; read message colour to strcol
        iny
        lda (lvchlo),y
        sta strcol
        ; read width of the message column
        ; to MSGCOLWIDTH 
        iny
        lda (lvchlo),y
        sta MSGCOLWIDTH

        ; set lvchlo/hi to the start of actual
        ; string data -> add 4 to lvchlo/hi
        clc
        lda lvchlo
        adc #$04
        sta lvchlo
        lda lvchi
        adc #$00
        sta lvchi
        jsr printstr
        rts
;-------------------------------------
; lvchi/lo must contain the string character data.
; $00 is the end sign for a string.
; msgcolwidth must contain the width of the message column 
; strcol must contain the string colour
; scrlo/hi must contain the start location in screen memory
; NOTE: A String cannot be more than 255 bytes long!
 
printstr

        ; set index counter y-reg to 0        
        ldy #$00
        ; tmpcnt is a width counter
        lda #$00
        sta tmpcnt
        
         ;read screen colour 
         ;memory pointer values to
         ;LOTMP and HITMP
         JSR GETCOLM

        ; now finally read the actual string
        ; until $00 occurs
        ; or y goes to zero again (too long string)
        
printstrloop
        lda (lvchlo),y
        beq printstrx ; end sign $00 occured
        sty tmp
        ldy tmpcnt
        ; set char to screen
        sta (scrlo),y
        ; set right colour to colour memory of screen location
        lda strcol
        sta (lotmp),y
        ldy tmp
        iny
        beq printstrx ; y went over to 0 (too long string)
        inc tmpcnt
        lda tmpcnt 
        cmp msgcolwidth
        ; has column widht already reached
        bne printstrloop
        ; column width has been reached

        lda #$00
        sta tmpcnt
        ; add a row 
        ; (40 chars) to screen 
        ; and colour memory pointer
        clc
        lda scrlo
        adc #$28 ; add a row
        sta scrlo
        lda scrhi
        adc #$00
        sta scrhi
        clc
        lda lotmp
        adc #$28
        sta lotmp
        lda hitmp
        adc #$00
        sta hitmp
        jmp printstrloop

printstrx
        rts

;--------------------------------------
; CLEAR MESSAGES AREA
;--------------------------------------
CLEARMSG
         ; Start address of message
         ; field in screen memory
         LDX #<MSGAREA
         STX LOTMP
         LDY #>MSGAREA
         STY HITMP
         
         ; 6 ROWS + 1
         LDA #$06
         STA TMP
         LDX #$0A
         LDY #$00
         
CLRMSGLO
         LDA #$20
         STA (LOTMP),Y
         INY
         DEX
         BNE CLRMSGLO
         
         CLC 
         LDA LOTMP
         ADC #$1E
         STA LOTMP
         LDA HITMP
         ADC #$00
         STA HITMP
         
         LDX #$0A
         DEC TMP
         BNE CLRMSGLO
         
CLRMSGX
         RTS

;--------------------------------------
; WRITE MESSAGES
;--------------------------------------
; writes messages to ingame message
; console.
;
; X, Y must contain the
; MESSAGE data start address
; X low order, Y high order byte
;--------------------------------------
WRITEMSG 
         ; store start address of the 
         ; message
         STX LVCHLO
         STY LVCHI 
         ; clear message area
         jsr clearmsg 

         ; Start address of message
         ; field in screen memory
         LDX #<MSGAREA
         STX SCRLO
         LDY #>MSGAREA
         STY SCRHI
         ; message colour
         LDA #$0A
         sta strcol
        lda #$0a
        sta msgcolwidth
        ; print the message string
        jsr printstr
        rts
;--------------------------------------
; CLEARS SCREEN
;--------------------------------------
CLEARSCR 
         ; value for empty space
         LDA #$20
         
         LDX #$00
CLEARLO
         ; clear starting from screen mem SCREENMEM
         STA SCREENMEM,X
         ; clear starting from SCREENMEMBK2
         STA SCREENMEMBK2,X
         ; from SCREENMEMBK3
         STA SCREENMEMBK3,X

         DEX
         BNE CLEARLO
         
         ; the rest of screen mem
         ; last screen memory location is
         ; $07e7

         LDX #$E7
CLEARLO2 
         STA SCREENMEMBK4,X
         DEX 
         BNE CLEARLO2
         sta SCREENMEMBK4
         RTS
        
;--------------------------------------
; INSTRUCTIONS
;--------------------------------------
INSTR        JSR CLEARSCR
        LDA #$01
        STA CHRCOL1
        LDA #$02
        STA CHRCOL2
        LDA #$00
        STA SCREENCOL
        STA BORDERCOL 
        LDX #<INSTRTX1
        LDY #>INSTRTX1
        jsr print
        LDX #<INSTRTX2
        LDY #>INSTRTX2
        jsr print
        LDX #<INSTRTX3
        LDY #>INSTRTX3
        jsr print
        JSR READJOYB
        RTS
;======================================
;
;         S F X - routines
;
;======================================
;--------------------------------------
; CLEAR SOUND REGISTERS
;--------------------------------------
; clears all sound registers from
; $D400 to $D418
;--------------------------------------
SNDCLR   LDY #$18
         LDA #$00
SNDCLRLO 
         STA $D400,Y
         DEY
         BNE SNDCLRLO
         STA $D400,Y
         ; set volume to max
         LDA #$0E
         STA $D418
         RTS

;--------------------------------------
; SFX init
;--------------------------------------
; X register must be set to the
; number of the sound wanted to play
; See SOUNDSP and SOUNDLN datatables.
;--------------------------------------
SFXINIT
         ; sound end index to SND1STP
         LDA SOUNDSP,X
         STA SND1STP
         
         ; set sound start index to SND1CNT
         ; sound data pointer
         LDA SOUNDLN,X
         CLC
         ADC SND1STP
         STA SND1CNT
         
         ; clear play time of note in SND1TIME
         LDA #$00
         STA SND1TIME
       
         ; set sound state: PLAYING 00000001
         LDA SNDSTATE
         ORA #$01
         STA SNDSTATE
         RTS

;--------------------------------------
; SFX
;--------------------------------------
; routine for playing game sounds.
; sound must be initialized first
; with SFXINIT routine.
;--------------------------------------
SFX 
        ; CHECK 1ST BIT OF SOUNDSTATE
        ; IS PLAYING?
        ; LEAVE IF BIT IS 0
        LDA SNDSTATE
        LSR A
        BCS SFX2
        RTS
SFX2
         ; STORE X and Y
         STX TMP
         STY TMP2
SFXC         
         ; check if sound has been
         ; played long enough
         LDA SND1TIME
         BEQ SFXLO ; NEXT NOTE
        
         ; play old note,
         ; set sound data pointer to x
         LDX SND1CNT 
         ; get waveform...
         LDA SOUNDWA,X
         ; and play
         STA $D404
         ; decrease playtime counter
         DEC SND1TIME
         ; and leave
          JMP SFXX

SFXLO  
        ; stop old note by setting the 1st bit
        ; in waveform to 0: AND 11111110 ($FE)

        LDA $D404
        AND #$FE
        STA $D404
        
         ; check if sound end index
         ; reached by comparing sound data
         ; pointer to sound end adress pointer
         LDA SND1CNT
         CMP SND1STP
         ; continue if not end index
         BNE SFXLO1

         ; or end sound and exit
         ; set SNDSTATE playing bit to 0
        LDA SNDSTATE
        AND #$FE
        STA SNDSTATE
        JMP SFXX

        ; play new note
SFXLO1
        ; decrease sound data table pointer
         DEC SND1CNT
         ; load sound data pointer to x
         LDX SND1CNT
         
         ; attack/decay
         LDA SOUNDAD,X
         STA $D405
         
         ; sustain/release
         LDA SOUNDSR,X
         STA $D406
                 
         ; note high frequency
         LDA SOUNDHI,X
         STA $D401
         
         ; note low frequency
         LDA SOUNDLO,X
         STA $D400
         
         ; note play time
         LDA SOUNDTM,X
         STA SND1TIME

         ;waveform
         LDA SOUNDWA,X
         STA $D404
        
SFXX
         ; RESTORE X and Y
         LDX TMP
         LDY TMP2
         RTS
;======================================
;
;               D A T A
;
;======================================-

;--------------------------------------
; SOUNDS
;--------------------------------------
; sound id's
; $00 : jump sound
; $01 : hit into enemy sound
; $02 : get item sound
; $03 : drop item sound
; $04 : explosion sound
; $05 : hit enemy with sword

; end index pointers for sounds
; read using sound id listed above.
; sound data is read from right to left
; so the last index is first in order
; to play
SOUNDSP  .BYTE $00,$03,$07,$09,$0B,$0E

; byte lengths of sounds  
; read using sound id listed above
SOUNDLN  .BYTE $03,$04,$02,$02,$03,$02

;--------------------------------------
; no more than $FF bytes long tables
; attack/decay
SOUNDAD  .BYTE $09,$09,$09
         .BYTE $09,$09,$09,$09
         .BYTE $09,$09
         .BYTE $09,$09
         .BYTE $09,$09,$09 ; expl
        .byte $09,$09 ;hit sword
; sustain/release
SOUNDSR  .BYTE $85,$85,$85
         .BYTE $85,$85,$85,$85
         .BYTE $85,$85
         .BYTE $85,$85
         .BYTE $85,$85,$85 ; expl
         .byte $85,$85  ;hit sword
; waveform:      
; 00010001 $11 triangle   01000001 $41 pulse
; 00100001 $21 saw        10000001 $81 buzz

SOUNDWA  .BYTE $21,$21,$21
         .BYTE $11,$11,$11,$11
         .BYTE $21,$21
         .BYTE $21,$21
         .BYTE $81,$81,$81 ; expl
        .BYTE $21,$21 ;hit sword
        
; hi-frequency
SOUNDHI  .BYTE $2B,$26,$22
         .BYTE $22,$26,$2B,$33
         .BYTE $22,$33
         .BYTE $33,$22
         .BYTE $33,$22,$2B ; expl
        .BYTE $22,$33 ;hit sword
; lo-frequency
SOUNDLO  .BYTE $34,$7E,$4B
         .BYTE $4B,$7E,$34,$61
         .BYTE $4B,$61
         .BYTE $61,$4B
         .BYTE $61,$4B,$34 ; expl
        .BYTE $61,$4B ;hit sword
; playtime
SOUNDTM  .BYTE $03,$03,$03
         .BYTE $03,$03,$03,$03
         .BYTE $03,$06
         .BYTE $06,$03
         .BYTE $50,$30,$10 ; expl
        .BYTE $03,$06 ;hit sword
;--------------------------------------
;FILENAMES
;--------------------------------------

FILENSPR .TEXT "SPR"
FILENLVD .TEXT "LVOUT"
FILENCHR .TEXT "CHR"

;--------------------------------------
;STRINGS
;--------------------------------------
; 1st byte : location low order byte
; 2nd byte : location high order byte
; 3rd byte : colour
; 4th byte : width of the message column
; $00      : end sign
;--------------------------------------
; NOTE: A String cannot be more than 255 bytes long!

TXTHOLDS .BYTE $1F,$04,$08,$05
        .enc screen
         .BYTE "HOLDS"
        .enc none
         .BYTE $00
         
TXLIVES  .BYTE $6F,$04,$09,$05
        .enc screen
         .BYTE "ENERGY"
        .enc none 
        .BYTE $00
         
TXTITLE2 .BYTE $36,$06,$0A,$14
        .enc screen
        .BYTE "PRESS FIRE TO START"
        .enc none
         .BYTE $00
         
TXTGOVER .BYTE $FB,$04,$0A,$09
        .enc screen
        .byte "GAME OVER"        
        .enc none
         .BYTE $00

TXTCONG  .BYTE $30,$04,$0A,$10
        .enc screen
        .BYTE "CONGRATULATIONS!"
        .enc none
         .BYTE $00
         
TXCOPY  .BYTE $E4,$05,$0B,$16
        .enc screen
        .BYTE "C MIKKO KEIN"
        .enc none
        .BYTE $1D
        .enc screen
        .BYTE "NEN 2008"
        .enc none
        .BYTE $00
         
TXTJOY  .BYTE $AE,$06,$09,$12
        .enc screen
        .byte "JOYSTICK IN PORT 2"
        .enc none
        .BYTE $00

TXTINS  .BYTE $28,$07,$09,$0D
        .enc screen
        .byte "PRESS F1 FOR INSTRUCTIONS"
        .enc none
        .BYTE $00

INSTRTX1
        .byte $00, $04, $08, $28
        .enc screen
        .TEXT " MOOMIN WANTS TO GO SAILING WITH MISS   "
        .TEXT " NIISKU. BEFORE HE CAN GO HE MUST HELP  "
        .TEXT " HIS FRIENDS TO GATHER SOME MISSING     "
        .TEXT " OBJECTS FROM TROLLY VALLEY. CONTACT    "
        .TEXT " MOOMINS FRIENDS FOR MORE INFO. FIND THE"
        .TEXT " OBJECTS AND RETURN THEM TO THEIR OWNER."
        .enc none
        .byte $00



INSTRTX2
        .byte $18, $05, $08, $28
        .enc screen
        .TEXT " GUIDE MOOMIN USING JOYSTICK IN PORT 2. " 
        .TEXT " PUSH STICK UP, NW AND NE TO JUMP.      "
        .TEXT " PRESS FIRE AND UP TO PICK UP AN OBJECT."
        .TEXT " PRESS FIRE AND DOWN TO DROP AB OBJECT. "
        .TEXT " PRESS A KEY TO USE AN ITEM. PRESS RUN- "
        .TEXT " STOP-KEY TO RESTART THE GAME."         
        .enc none
        .byte $00

INSTRTX3.byte $08, $06, $08, $28
        .enc screen
        .TEXT " PUSH JOYSTICK WITH FIRE BUTTON PRESSED "
        .TEXT " DOWN TO THE DIRECTION OF AN HOSTILE    "
        .TEXT " CREATURE TO DEFEND. YOU CAN FIND OUT   "
        .TEXT " WHO ARE YOUR FRIENDS USING SWORD...    "
        .TEXT " THEY DONT RESPOND TO THE SWORD.        "
        .TEXT " PRESS FIRE."
        .enc none
        .byte $00

INSTRTX4        
        .byte $f8, $06, $08, $28
        .enc screen
        .TEXT " GO TO MISS NIISKU AFTER YOU HAVE HELPED"
        .TEXT " ALL ALL OF YOUR FRIENS.                "
        .TEXT " BEWARE OF THE LITTLE MY                "
        .TEXT " ALSO, SHE IS PISSED OF AT MOOMIN - HE  "
        .TEXT " ATE ALL HER COOKIES! GOOD LUCK!        "
        .TEXT " PRESS FIRE TO START!"
        .enc none
        .byte $00

;--------------------------------------
;MESSAGES
;--------
;all messages must end to $00
;--------------------------------------
.enc screen
MSGMUIKU .BYTE "CAN YOU FIND MY HARMONICA",$00
MSGMUIKB .BYTE "CHEERS MATE",$00
MSGNIISA .BYTE "I CANT FIND MY RING ANYWHERE",$00
MSGNIISB .BYTE "MY HERO",$00
MSGHEMUA .BYTE "OH ERR MY MAGNIFYING CLASS ITS STOLEN",$00
MSGHEMUB .BYTE "YOURE A DECENT BOY",$00
MSGPAPPA .BYTE "HAVE YOU SEEN MY HAT SON",$00
MSGPAPPB .BYTE "GOOD JOB SON!",$00 
MSGMAMMA .BYTE "DEAR SON CAN YOU LOOK FOR MY HANDBAG",$00
MSGMAMMB .BYTE "THANKS DEAR",$00
MSGNIPSA .BYTE "IVE LOST THE RAREST BUTTON OF MY COLLECTION",$00
MSGNIPSB .BYTE "MY PRECIOUS",$00
MSGWIZA  .BYTE "PLEASE FIND MY RUBY",$00
MSGWIZB  .BYTE "ITS MARVELLOUS",$00
.enc none

;--------------------------------------
; ROOMS
; $00
; $01
; $02
; $03
; ...
; $1D        title screen
; $1E mountain cave
; $1F not used
;--------------------------------------
; ROOM EXITS
; room numers are from $00 to $1F 
; $FF is none
; These tables are read using current
; room id.
;--------------------------------------
EXITLFT  .BYTE $01,$03,$04,$08,$FF,$02
         .BYTE $1A,$06,$09,$FF,$16,$FF
         ;$0C...
         .BYTE $0D,$FF,$FF,$FF,$0A,$07
         .BYTE $FF,$11,$10,$13,$18,$15
         ;$18...
         .BYTE $05,$1E,$00,$0E,$12,$FF 
         .BYTE $ff,$0b
         
EXITRGT  .BYTE $1A,$00,$05,$01,$02,$18
         .BYTE $07,$11,$03,$08,$10,$1F
         .BYTE $FF,$0C,$1B,$0E,$14,$13
         .BYTE $1C,$15,$FF,$17,$0A,$FF
         ;$18...
         .BYTE $16,$FF,$06,$FF,$FF,$FF  
         .BYTE $19,$FF
         
EXITUP   .BYTE $FF,$02,$0D,$04,$FF,$0C
         .BYTE $16,$0A,$FF,$FF,$FF,$FF ; -> $0B
         .BYTE $1F,$0B,$FF,$03,$12,$10 ; -> $11
         .BYTE $FF,$14,$1C,$FF,$FF,$FF ; -> $17
         .BYTE $FF,$08,$18,$FF,$FF,$FF ; -> $1D
         .BYTE $09,$FF
         
EXITDWN  .BYTE $1B,$0E,$01,$0F,$03,$00
         .BYTE $FF,$FF,$19,$1E,$07,$0D ;->$0B
         .BYTE $05,$02,$FF,$FF,$11,$FF
         .BYTE $10,$FF,$13,$FF,$06,$FF
         .BYTE $1A,$FF,$FF,$FF,$14,$FF ; -> $1d
         .BYTE $FF,$0c

; ROOM EXIT SPECIFICATIONS END

;--------------------------------------
; SPRITE OBJECT TABLES -
; READ WITH SPRITE ID VALUE RMSPRID
; START HERE TO ADD NEW SPRITE OBJECT!
;--------------------------------------
; sprite id's:
; hattivatti: $00 
; morko     : $01 
; bug       : $02 
; nuuskamuik: $03
; hemuli    : $04 
; haisuli   ; $05 
; niiskun   : $06 
; bat       : $07
; muumipappa: $08 
; muumimamma: $09 
; pikkumyy  : $0A 
; nipsu     : $0B
; boulder   : $0C 
; door      : $0D 
; cloud     : $0E 
; wasp      : $0F
; rapu      : $10 
; seagul    : $11 
; wizard    : $12 
; spider    : $13
; skeleton  : $14
; tombstone : $15
; ghost     : $16

; START ADRESSES OF friendly sprobj 
; MESSAGES

MATMSGLO .BYTE $00,$00,$00,<MSGMUIKU
         .BYTE <MSGHEMUA,$00,<MSGNIISA
         .BYTE $00,<MSGPAPPA,<MSGMAMMA
         .BYTE $00,<MSGNIPSA
         .BYTE $00,$00,$00,$00,$00,$00
         .BYTE <MSGWIZA,$00,$00,$00,$00
         
MATMSGHI .BYTE $00,$00,$00,>MSGMUIKU
         .BYTE >MSGHEMUA,$00,>MSGNIISA
         .BYTE $00,>MSGPAPPA,>MSGMAMMA
         .BYTE $00,>MSGNIPSA
         .BYTE $00,$00,$00,$00,$00,$00
         .BYTE >MSGWIZA,$00,$00,$00,$00
         
MAMTHXLO .BYTE $00,$00,$00,<MSGMUIKB
         .BYTE <MSGHEMUB,$00,<MSGNIISB
         .BYTE $00,<MSGPAPPB,<MSGMAMMB
         .BYTE $00,<MSGNIPSB
         .BYTE $00,$00,$00,$00,$00,$00
         .BYTE <MSGWIZB,$00,$00,$00,$00
         
MAMTHXHI .BYTE $00,$00,$00,>MSGMUIKB
         .BYTE >MSGHEMUB,$00,>MSGNIISB
         .BYTE $00,>MSGPAPPB,>MSGMAMMB
         .BYTE $00,>MSGNIPSB
         .BYTE $00,$00,$00,$00,$00,$00
         .BYTE >MSGWIZB,$00,$00,$00,$00

; sprite state: mono=0, multicolor = 1
; maybe expanded/normal, ...
; bit 0: 0=mono 1=multicolor
; bit 1: 0=normal 1=expanded
; bit 2: 0=can walk through 1=can't...
; bit 3: 0=normal, 1=moving platform


SPRSTA   .BYTE $00,$01,$01,$01,$01,$01 ; spr $00-$05
         .BYTE $01,$01,$01,$01,$01,$01 ; spr $06-$0b
         .BYTE $05,$05,$09,$01,$01,$01 ; spr $0c-$11
         .BYTE $01,$01,$01,$05,$01

; sprite data pointers,
; first sprite is $..(?)
; sprite data pointer / no movement
SPRPNST  .BYTE $8A,$97,$9A,$9C,$9D,$A1
         .BYTE $A3,$A4,$A6,$A7,$A8,$AC
         .BYTE $AE,$AF,$B0,$B2,$8F,$91
         .BYTE $8D,$86,$BC,$93,$C2
; sprite data pointer / moving left
SPRPNLF  .BYTE $8A,$94,$9A,$9C,$9D,$A1
         .BYTE $A3,$A4,$A6,$A7,$A8,$AC
         .BYTE $AE,$AF,$B0,$B5,$8F,$91
         .BYTE $8D,$86,$BC,$93,$C2
; sprite data pointer / moving right
SPRPNRG  .BYTE $8A,$96,$9A,$9C,$9F,$A1
         .BYTE $A3,$A4,$A6,$A7,$AA,$AC
         .BYTE $AE,$AF,$B0,$B2,$8F,$91
         .BYTE $8D,$86,$BF,$93,$C4
; sprite data pointer / moving up
SPRPNUP  .BYTE $8A,$98,$9A,$9C,$9D,$A1
         .BYTE $A3,$A4,$A6,$A7,$A8,$AC
         .BYTE $AE,$AF,$B0,$B2,$8F,$91
         .BYTE $8D,$86,$BC,$93,$C2
; sprite data pointer / moving down
SPRPNDW  .BYTE $8A,$98,$9A,$9C,$9F,$A1
         .BYTE $A3,$A4,$A6,$A7,$A8,$AC
         .BYTE $AE,$AF,$B0,$B5,$8F,$91
         .BYTE $8D,$86,$BF,$93,$C4
; amount of animation frames - 1 (!) for 
; horizontal movement
SPRFRLF  .BYTE $02,$01,$01,$00,$01,$01
         .BYTE $00,$01,$00,$00,$01,$01
         .BYTE $00,$00,$01,$02,$01,$01
         .BYTE $01,$01,$02,$00,$01
; amount of animation frames
; for verovement
SPRFRUP  .BYTE $02,$01,$01,$00,$01,$01
         .BYTE $00,$01,$00,$00,$01,$01
         .BYTE $00,$00,$01,$00,$01,$01
         .BYTE $01,$01,$02,$00,$01

; sprite x/y max/min collision 
; detection area location values.
; Real on-screen values can be obtained by adding
; this value to objx/y location value
; when initialising sprite objects.
; The sums are stored in objcolx1,
; objcolx2, objcoly1 and objcoly2
; datatables.

; sprite collision area / min x
SPRCOLX1 .BYTE $05,$00,$00,$00,$00,$04
         .BYTE $00,$02,$00,$00,$05,$00
         .BYTE $00,$00,$00,$00,$00,$00
         .BYTE $00,$00,$00,$00,$00
; sprite collision area / max x
SPRCOLX2 .BYTE $17,$1D,$1D,$1D,$1D,$19
         .BYTE $1D,$1B,$1D,$1D,$1D,$1D
         .BYTE $1D,$1D,$1D,$1D,$1D,$1D
         .BYTE $1D,$1D,$1D,$1D,$1D
; sprite collision area / min y
; top/left-corner
SPRCOLY1 .BYTE $08,$00,$00,$00,$00,$08
         .BYTE $00,$00,$00,$00,$0A,$00
         .BYTE $00,$00,$00,$00,$00,$00
         .BYTE $00,$00,$00,$00,$00
; sprite collision area / max y
SPRCOLY2 .BYTE $1f,$1F,$1F,$1F,$1F,$1F
         .BYTE $1F,$15,$1F,$1F,$1F,$1F
         .BYTE $1F,$1F,$05,$1F,$1F,$1F
         .BYTE $1F,$1F,$1F,$1F,$1F

; GLOBAL SPRITE OBJECT DEFINITIONS END HERE!
;--------------------------------------------


;--------------------------------------
; sprite ORA values to put sprites on 
; by amount of enemy sprites (SPRDISPLAY).
;--------------------------------------
; 00000001: 0 enemy spr (only moomin on)
; 00000011: 1 enemy sprites 
; 00000111: 2 enemy sprites
; ...
;--------------------------------------
SPRORA   .BYTE $01,$03,$07,$0F,$1F,$3F
         .BYTE $7F,$FF
         
;--------------------------------------
; to switch off certain sprite with
; room specific sprite index number
; use with AND operator
;--------------------------------------
; 1111 1101
; 1111 1011
; 1111 0111
; 1110 1111
;--------------------------------------
SPROFF   .BYTE $FD,$FB,$F7,$EF

;--------------------------------------
; multicolor ORA-value to put sprite 
; multicolor state.
; Read by enemy sprite number 0-3
;--------------------------------------
MCORA    .BYTE $02,$04,$08,$10

;--------------------------------------
; mono AND-values to put sprite into 
; mono state.
; Read by enemy sprite number 0-3
;--------------------------------------
MOAND    .BYTE $FD,$FB,$F7,$EF



;--------------------------------------
; ROOM SPECIFIC SPRITE OBJECT PREFERENCES
;
; START HERE TO ADD NEW SPRITE OBJECT(S) TO
; ROOM(S)
;
; $FF is used as an end sign
; maximum of 7 sprite objects can be 
; inserted to each room
;--------------------------------------
; room number
RMNUM    .BYTE $00,$00,$01,$01,$04,$0D
         .BYTE $04,$04,$04,$07,$06,$06
         .BYTE $17,$0F,$0F,$0F,$0F,$11
         .BYTE $05,$07,$0E,$0E,$0E,$14

         .BYTE $07,$17,$1A,$10,$12,$12
         .BYTE $1A,$1A,$03,$03,$03,$12
         .BYTE $15,$16,$17,$19,$19,$19
         .BYTE $09,$09,$18,$18,$16,$16

         .BYTE $17,$18,$1A,$09,$19,$0D
         .BYTE $00,$00,$01,$02,$02,$02
         .BYTE $05,$05,$0C,$0C,$0D,$13
         .BYTE $0A,$0A,$1B,$1B,$1B,$1B 
         .BYTE $0E,$1e,$1e,$1e,$FF

; sprite-object id
RMSPRID  .BYTE $00,$01,$00,$01,$02,$13
         .BYTE $02,$02,$02,$03,$04,$05
         .BYTE $06,$07,$07,$07,$07,$08
         .BYTE $09,$0A,$0B,$07,$07,$07

         .BYTE $0C,$0D,$0E,$0E,$0E,$0E
         .BYTE $0F,$0F,$0F,$0F,$0F,$12
         .BYTE $10,$11,$10,$0E,$0E,$0E
         .BYTE $0E,$0E,$11,$11,$11,$11

         .BYTE $11,$11,$0E,$05,$05,$13
         .BYTE $00,$00,$05,$13,$13,$13
         .BYTE $13,$13,$02,$02,$02,$0a
         .BYTE $0f,$0f,$14,$14,$14,$14

         .BYTE $15,$16,$0e,$0e,$FF
; animation speed
; smaller value = faster speed
RMANISP  .BYTE $08,$03,$05,$06,$05,$04
         .BYTE $0A,$0A,$0A,$00,$05,$05
         .BYTE $00,$08,$08,$08,$08,$00
         .BYTE $00,$07,$09,$08,$08,$08
         
         .BYTE $00,$00,$04,$04,$04,$04
         .BYTE $0A,$0B,$0B,$0B,$0B,$04
         .BYTE $05,$05,$05,$03,$04,$05
         .BYTE $03,$04,$07,$07,$05,$04

         .BYTE $05,$07,$05,$05,$05,$04
         .BYTE $08,$09,$05,$04,$05,$06
         .BYTE $05,$05,$04,$04,$04,$05
         .BYTE $05,$05,$05,$04,$03,$06

         .BYTE $00,$05,$05,$04,$FF

; movement speed
; greater value = slower speed
RMMOVSP  .BYTE $00,$04,$00,$00,$00,$00
         .BYTE $00,$00,$00,$00,$00,$00
         .BYTE $00,$00,$00,$00,$00,$00
         .BYTE $00,$00,$00,$00,$00,$00

         .BYTE $00,$00,$00,$00,$00,$00
         .BYTE $00,$00,$00,$00,$00,$00
         .BYTE $00,$00,$00,$00,$00,$00
         .BYTE $00,$00,$00,$00,$00,$00

         .BYTE $00,$00,$00,$00,$00,$00
         .BYTE $00,$00,$00,$00,$00,$00
         .BYTE $00,$00,$00,$00,$00,$00
         .BYTE $00,$00,$01,$00,$02,$03

         .BYTE $00,$00,$00,$01,$FF

; movement step in pixels
; greater value = step wider
RMMOVSTP .BYTE $02,$02,$01,$02,$02,$02
         .BYTE $01,$01,$01,$00,$01,$01
         .BYTE $00,$01,$01,$01,$01,$00
         .BYTE $00,$01,$01,$01,$01,$01

         .BYTE $00,$00,$02,$01,$01,$02
         .BYTE $01,$01,$01,$02,$01,$00
         .BYTE $02,$02,$02,$02,$01,$02
         .BYTE $01,$02,$01,$01,$01,$02

         .BYTE $01,$01,$02,$01,$02,$01
         .BYTE $01,$01,$01,$01,$02,$03
         .BYTE $01,$01,$01,$01,$01,$01
         .BYTE $01,$01,$01,$01,$02,$01

         .BYTE $00,$01,$01,$01,$FF

; sprite-object state (store to OBJSTA)
;  sprite-object's state:
;  bit 0: movement (1=yes)
;  bit 1: animation  (1=yes)
;  bit 2: facing left  ( 1 = left )
;  bit 3: facing right 
;  bit 4: facing up  ( 1 = up )
;  bit 5: facing down 
;  bit 6: lethal/harmless (1 = harmless)
;  bit 7: invisible = 1 (this table
;  must be restored before new game!!!)
;  
; 00000000 -> still / no animation
; 00000001 -> still / animated
; 00010011 $13 -> moving, animated, up
; 00100011 $23 -> mov, anim, dw
; 00010111 $17 mov,anim,lft,up
; 00100111 $27 mov,anim,lft,dw
; 00011011 $1B mov,anim,rgt,up
; 00101011 $2B mov,anim,rgt,dw
; 00000111 $07 mov,anim,lft
; 00001011 $0B mov,anim,rgt
; 01000111 $47 mov,anim,lft,harmless
; 01000100 $44 still, lft, harmless
; 01000000 $40 still, harmless
; 01000010 $42 still, anim, harmless
; 01010011 $53 mov,ani,uo,harmless
; 01100011 $63 mov,ani,dw,harmless
; 01000111

; initial values, restore from this
; table:
RMOBSTAI .BYTE $0B,$07,$07,$0b,$23,$0B
         .BYTE $13,$23,$13,$44,$47,$07
         .BYTE $44,$17,$27,$1B,$2B,$44
         .BYTE $44,$07,$47,$17,$27,$1B

         .BYTE $40,$40,$57,$53,$53,$47
         .BYTE $17,$2B,$17,$1B,$2B,$42
         .BYTE $07,$07,$07,$53,$47,$63
         .BYTE $53,$47,$17,$27,$17,$2B

         .BYTE $27,$1B,$53,$07,$0B,$13
         .BYTE $07,$07,$07,$13,$23,$13
         .BYTE $1B,$2B,$13,$23,$23,$07
         .BYTE $17,$27,$07,$07,$0b,$0b

         .BYTE $40,$17,$53,$53,$FF
; dynamic values, restored from RMOBSTAI
; when new game starting 
RMOBSTA  .BYTE $0B,$07,$07,$0b,$23,$0B
         .BYTE $13,$23,$13,$44,$47,$07
         .BYTE $44,$17,$27,$1B,$2B,$44
         .BYTE $44,$07,$47,$17,$27,$1B

         .BYTE $40,$40,$57,$53,$53,$47
         .BYTE $17,$2B,$17,$1B,$2B,$42
         .BYTE $07,$07,$07,$53,$47,$63
         .BYTE $53,$47,$17,$27,$17,$2B

         .BYTE $27,$47,$53,$07,$0B,$13
         .BYTE $07,$07,$07,$13,$23,$13
         .BYTE $1B,$2B,$13,$23,$23,$07
         .BYTE $17,$27,$07,$07,$0b,$0b

         .BYTE $40,$17,$53,$53,$FF
         
; LOCATIONS:
; x: $20...$E8
; min x-location (=>$20)        
RMMINX   .BYTE $20,$20,$25,$20,$df,$30
         .BYTE $5D,$A0,$C0,$BC,$6f,$18
         .BYTE $C2,$50,$40,$A0,$B0,$A0
         .BYTE $B0,$20,$30,$50,$50,$50

         .BYTE $C2,$96,$5D,$2A,$2A,$3a
         .BYTE $90,$90,$20,$18,$18,$6a
         .BYTE $B0,$20,$20,$20,$30,$D8
         .BYTE $50,$60,$20,$18,$20,$20

         .BYTE $20,$20,$68,$A0,$50,$8C
         .BYTE $20,$20,$20,$30,$70,$B0
         .BYTE $18,$18,$30,$70,$68,$b0
         .BYTE $20,$20,$30,$40,$60,$a0

         .BYTE $F0,$20,$50,$28,$FF
; max x-location (<=$E8)
RMMAXX   .BYTE $D0,$A0,$5D,$B0,$df,$F7
         .BYTE $5D,$A0,$C0,$BC,$90,$60
         .BYTE $C2,$F0,$D0,$F0,$F0,$A0
         .BYTE $B0,$aa,$30,$F0,$F0,$F0

         .BYTE $C2,$96,$C2,$2A,$2A,$78
         .BYTE $F7,$F7,$F7,$F7,$F7,$6a
         .BYTE $F7,$F7,$B0,$18,$D0,$D8
         .BYTE $50,$B0,$F7,$F7,$F7,$F7

         .BYTE $8C,$F7,$68,$F7,$F7,$8C
         .BYTE $D0,$F0,$F7,$30,$70,$B0
         .BYTE $DF,$DF,$30,$70,$68,$f7
         .BYTE $b0,$b0,$b0,$90,$a0,$c0
         .BYTE $F0,$f0,$50,$28,$FF
; initial x-location
RMINIX   .BYTE $25,$22,$22,$38,$df,$32
         .BYTE $5D,$A0,$C0,$BC,$80,$50
         .BYTE $C2,$A0,$B0,$C0,$D0,$A0
         .BYTE $B0,$a0,$30,$A0,$B0,$C0

         .BYTE $C2,$96,$C2,$2A,$2A,$60
         .BYTE $9C,$9C,$5D,$8C,$C2,$6a
         .BYTE $B0,$8C,$8C,$18,$8C,$D8
         .BYTE $50,$80,$90,$F0,$40,$B0

         .BYTE $40,$30,$68,$A0,$90,$8C
         .BYTE $C0,$70,$63,$30,$70,$B0
         .BYTE $80,$80,$30,$70,$68,$b2
         .BYTE $50,$a0,$a0,$80,$90,$b0
         .BYTE $F0,$a0,$50,$28,$FF

; y: $3A...$DD
; min y-location (=>$3a)
RMMINY   .BYTE $5D,$B5,$64,$9d,$26,$60
         .BYTE $26,$50,$b7,$6D,$10,$F0
         .BYTE $D0,$80,$80,$80,$80,$D6
         .BYTE $DD,$DD,$CA,$54,$54,$54

         .BYTE $DA,$cD,$D0,$35,$54,$82
         .BYTE $26,$26,$26,$26,$26,$70
         .BYTE $ed,$D0,$cd,$40,$A0,$80
         .BYTE $6C,$C8,$26,$26,$26,$26

         .BYTE $30,$26,$26,$36,$3E,$26
         .BYTE $5D,$DD,$DD,$60,$40,$30
         .BYTE $2E,$95,$63,$b0,$63,$d6
         .BYTE $26,$26,$30,$50,$a0,$c0
         .BYTE $DD,$40,$b0,$85,$FF
; max y-location (<=$dd)
RMMAXY   .BYTE $5D,$B5,$64,$9d,$B0,$60
         .BYTE $C0,$Af,$d8,$6D,$B0,$DD
         .BYTE $D0,$C0,$B0,$A0,$90,$D6
         .BYTE $DD,$DD,$CA,$C0,$C0,$C0

         .BYTE $DA,$cD,$D0,$C5,$DD,$82
         .BYTE $DD,$DD,$DD,$DD,$DD,$70
         .BYTE $ed,$D0,$cd,$B0,$B0,$B0
         .BYTE $DD,$C8,$DD,$DD,$DD,$DD

         .BYTE $DD,$DD,$60,$36,$3E,$DD
         .BYTE $5D,$DD,$DD,$A8,$B0,$DD
         .BYTE $88,$DD,$dd,$dd,$dd,$d6
         .BYTE $dd,$dd,$30,$50,$a0,$c0
         .BYTE $DD,$dd,$d0,$dd,$FF
; initial y-location
RMINIY   .BYTE $5D,$B5,$64,$9d,$90,$60
         .BYTE $35,$A0,$d0,$6D,$BA,$DD
         .BYTE $D0,$A0,$B0,$BA,$90,$D6
         .BYTE $DD,$DD,$CA,$A0,$B0,$BA

         .BYTE $DA,$cD,$D0,$40,$E4,$82
         .BYTE $82,$82,$54,$82,$B0,$70
         .BYTE $ed,$D0,$cd,$8E,$A0,$70
         .BYTE $80,$C8,$B2,$8E,$B2,$50

         .BYTE $80,$30,$40,$36,$3E,$B0
         .BYTE $5D,$DD,$DD,$8E,$8E,$8E
         .BYTE $6C,$B2,$b7,$d0,$a0,$d6
         .BYTE $60,$a0,$30,$50,$a0,$c0
         .BYTE $DD,$bb,$c0,$b0,$FF
; sprite colour
RMSPRCOL .BYTE $01,$02,$01,$0B,$05,$04
         .BYTE $03,$08,$0A,$05,$07,$09
         .BYTE $07,$00,$00,$00,$00,$06
         .BYTE $0A,$08,$09,$00,$00,$00

         .BYTE $06,$06,$06,$06,$06,$0F
         .BYTE $07,$07,$07,$07,$07,$02
         .BYTE $02,$02,$02,$0B,$0C,$0F
         .BYTE $01,$0A,$0B,$0B,$01,$01

         .BYTE $01,$0B,$08,$09,$09,$04
         .BYTE $01,$01,$09,$01,$02,$03
         .BYTE $02,$03,$04,$05,$08,$03
         .BYTE $07,$07,$01,$02,$03,$04
         .BYTE $01,$03,$07,$0d,$FF
         
;--------------------------------------
; DYNAMIC ACTIVE OBJECT DATA TABLES
;--------------------------------------
; Data from sprite object tables and
; room object tables is stored here
; for each active sprite object in
; current room.
;
; These values maintain only for the
; current room, then the values are 
; reset.
;--------------------------------------
; sprobj id for each sprobj in screen
OBJID    .BYTE $00,$00,$00,$00,$00,$00,$00
; index to objects RMxxx table
OBJRMIDX .BYTE $00,$00,$00,$00,$00,$00,$00
; sprite-objects current x-location
OBJX     .BYTE $00,$00,$00,$00,$00,$00,$00
; sprite-objects current y-location
OBJY     .BYTE $00,$00,$00,$00,$00,$00,$00
; sprite-objects min x-location
OBJMINX  .BYTE $00,$00,$00,$00,$00,$00,$00
; sprite-objects max x-location
OBJMAXX  .BYTE $00,$00,$00,$00,$00,$00,$00
; sprite-objects min y-location
OBJMINY  .BYTE $00,$00,$00,$00,$00,$00,$00
; sprite-objects max y-location
OBJMAXY  .BYTE $00,$00,$00,$00,$00,$00,$00
; sprite-object's collision detect area
; min x-value                          
OBJCOLX1 .BYTE $00,$00,$00,$00,$00,$00,$00         
; sprite-object's collision detect area
; max x-value                          
OBJCOLX2 .BYTE $00,$00,$00,$00,$00,$00,$00         
; sprite-object's collision detect area
; min y-value                          
OBJCOLY1 .BYTE $00,$00,$00,$00,$00,$00,$00         
; sprite-object's collision detect area
; max y-value
OBJCOLY2 .BYTE $00,$00,$00,$00,$00,$00,$00
; sprite-object's animation speed
OBJANISP .BYTE $00,$00,$00,$00,$00,$00,$00
; sprite-objects's movement step
; (pixels moved at one step)
OBJMOVSTP .BYTE $00,$00,$00,$00,$00,$00,$00
; sprite-objects's movement speed
OBJMOVSP .BYTE $00,$00,$00,$00,$00,$00,$00
; sprobj state bits
OBJSTA   .BYTE $00,$00,$00,$00,$00,$00,$00
; frame data pointer - still
OBJPNST  .BYTE $00,$00,$00,$00,$00,$00,$00
; frame data pointer left
OBJPNLF  .BYTE $00,$00,$00,$00,$00,$00,$00
; amount of horizontal animation frames
OBJFRLR  .BYTE $00,$00,$00,$00,$00,$00,$00
; frame data pointer right
OBJPNRG  .BYTE $00,$00,$00,$00,$00,$00,$00
; frame data pointer up
OBJPNUP  .BYTE $00,$00,$00,$00,$00,$00,$00
; amount of vertical animation frames
OBJFRUD  .BYTE $00,$00,$00,$00,$00,$00,$00
; frame data pointer down
OBJPNDW  .BYTE $00,$00,$00,$00,$00,$00,$00
; sprite colour
OBJSPCOL .BYTE $00,$00,$00,$00,$00,$00,$00
; sprite state info (multi/mono...) 
OBJSPST  .BYTE $00,$00,$00,$00,$00,$00,$00
; animation frame counter for each sprobj in screen
OBJFRCNT .BYTE $00,$00,$00,$00,$00,$00,$00
; current animation frame pointer for each sprobj in screen
OBJFRCUR .BYTE $00,$00,$00,$00,$00,$00,$00
; animation speed counter for each sprobj in screen
OBJANICN .BYTE $00,$00,$00,$00,$00,$00,$00
; movement speed counter for each sprobj at screen
OBJSPCNT .BYTE $00,$00,$00,$00,$00,$00,$00

;---------------------------------
; C H A R S
;---------------------------------
; CHARACTER CODES
;---------------------------------
; character code / function / (roomtype id) / description
; functions are: 
; a = animation
; b = background scenery illustration (no effect on moomin)
; c = climbable
; e = elevator
; f = floor
; i = item
; l = lethal char
; o = string chars (letters, numbers)
; s = staircase character
; w = wall char
; ...
;-----------------------
; $00 ---
; $01-$1a o letters from a to z
; $1b o ö
; $1c i moomin lives left char
; $1d o ä
; $1e i ancient scroll (to open crypt)
; $1f - fac (????)
; $21 o !
; $22-24 not in use
; $25 ruby
; $26-29 not in use
; $30-$39 o numbers from 0 to 9
; $3c b 1 ROOM GRAPHICS START FROM HERE --- flower
; $3d b        1 flower
; $3e b 0-2 spiders web (p1, hires) 
; $3f o ?
; $40 cf 0 ladder
; $41 cf 0 ladder (added!)
; $42 cf 0 ladder
; $43 f 1, leaf
; $44 f 1, leaf
; $45-47 not at use?
; $48 fs  2 rocky
; $49 fsw 2 rocky
; $4a fw  2 rocky
; $4b w   2 rocky
; $4c w   2 rocky
; $4d fw  2 soil/rock
; $4e fw  2 soil/sand
; $4f fs  2 rocky
; $50 b   2 rocky ceiling  
; $51 b   2 rocky ceiling
; $52 b   2 rocky ceiling
; $53 fs  2 rocky
; $54 cf  2 rocky
; $55 i ring (nilkkarengas)
; $56 i pappa's hat
; $57 i harmonica
; $58 i magnification glass
; $59 i key (to bath house)
; $5a i bag
; $5b i button
; $5c b bottle (hires, not at use?)
; $5d b candle (hires, not at use?)
; $5e-4b b 1 tree trunk
; $5f cfs is this in use?
; $60 b 1 tree
; $61 b 1 tree
; $62 cf 0 box
; $63 f  0 basic floor
; $64 w  0 basic wall
; $65 w  0 basic wall
; $66 fw 0 basic floor/wall
; $67 f  0 basic floor
; $68 f  0 basic floor
; $69 fs 0 stairs
; $6a b  0 stairs
; $6b b  0 stairs
; $6c s  0 stairs
; $6d c  0 box
; $6e c  0 box
; $6f c  0 box
; $70 fs 1 log
; $71 al 1 wave
; $72-75 b large log (hires, not at use)
; $76-75 b plants? (hires)
; $78 fs 0 roof
; $79 f  0 roof
; $7a f  0 roof
; $7b fs 0 roof
; $7c aef  0 roll
; $7d aef  0 roll
; $7e acef 0 elev. up
; $7f acef 0 elev. down
; $80 al 2 lava bubbles
; $81 al 2 lava
; $82 i dynamite
; $83 i candle
; $84-$87 b moomins ancestors (hires)
; $88 b fire (hires)
; $89 a fire (hires)
; $8a b 1 table/chair foot
; $8b b 1 table/chair foot
; $8c f 1 table
; $8d f 1 table
; $8e f 1 table
; $8f-$94 1 b kehys
; $95-$96 1 b table/chair feet
; $97-$9a 1 b bed?
; $9b b   0 pot
; $9c ---
; $9d ?
; $9e-$9f b 0 pillow
; $a0 o inverse space
; $a1-$a5 b 0 candlecrown
; $a6 b 0 bottle
; $a7 b 1 cloud
; $a8 ---
; $a9-ab f 1 bird's nest 
; $ac i diamond
; $ad-af f 2 rocky platform
; $b0-b3 b 1 mountain
; $b4 s 2 skull 
; $b5-ba 0 ceiling
;--------------------------------------
;OBJECTIVE CHARACTERS
;--------------------------------------
; characters for collision testing
;--------------------------------------
; end signs changed from $FF to $00 !!!
;--------------------------------------
STAIRCHR .BYTE $48,$49,$4F,$53,$5F,$69
         .BYTE $6C,$70,$78,$7B,$b4,$00

WALLCHRS .BYTE $64,$65,$66,$49,$4A,$4B
         .BYTE $4C,$4D,$4E,$00

DNGRCHRS .BYTE $71,$80,$81,$00

;--------------------------------------
; Floor chars
;--------------------------------------
; all the STAIRCHRs belong to this
; group also

FLORCHRS .BYTE $40,$41,$42,$43,$44,$48,$49
         .BYTE $4A,$4D,$4E,$4F,$53,$54
         .BYTE $5F,$62,$63,$66,$67,$68
         .BYTE $69,$6A,$6B,$6C,$6D,$6E
         .BYTE $6F,$70,$78,$79,$7A,$7B;!!! eka oli muuttunut $7F:msi muutin takaisin $6f:ksi, seuraa käyttäytymistä
         .BYTE $7C,$7D,$7E,$7F,$8C,$8D
         .BYTE $8E,$A9,$AA,$AB,$AD,$AE
         .BYTE $AF,$00
         
; HISSIMERKIT

ELECHRS   .BYTE $7C,$7D,$7E,$7F,$00

; movement direction bits for MVCHRS
; bit 0: left
; bit 1: right
; bit 2: up
; bit 3: down
ELECHDIR .BYTE $02,$01,$08,$04,$00

; climbable chars
LADCHR   .BYTE $40,$41,$42,$54,$5F,$62,$6D
         .BYTE $6E,$6F,$7E,$7F,$00
;--------------------------------------


;--------------------------------------
; COLOUR CODES FOR ROOM GRAPHICS
; CHARACTERS
;--------------------------------------
;   Character colour is colour#3 in fonted.
;   The 2 multicolour-mode colours can be set
;   using the room specific ROOMTYPE id's.
;   
;   In this table are the colour codes for 
;   characters starting from $3C (60) which is '<'-char.
;   Characters for room graphics start from there.
;   To use colour code you have to 
;   substract $3c from actual character
;   code value. For example.
;   
;   sec
;   lda #$41 ; char code value
;   sbc #$3c ; substract $3c from $41
;   tax      ; put result to x-reg
;   lda charcols,x
;   sta [screen colour memory location]
;
;   color must be $08...$0f for char
;   to be in multicolor mode.
;
; colors in multicolor mode (colour #3):
; $08   black
; $09   white
; $0A   red
; $0B   cyan
; $0C   purple
; $0D   green
; $0E   blue
; $0F   yellow
;--------------------------------------
CHARCOLS ; from char 60 ($3C)
         .BYTE $0A,$0D,$0F,$09,$0D,$0D
         .BYTE $0D,$0D,$0D,$08,$0A,$0A
         ; 72 ($48) ->
         .BYTE $0D,$0F,$0F,$0F,$0F,$0F
         .BYTE $08,$0F,$0F,$0F,$0F,$0F
         ; 84 ($54) ->
         .BYTE $0F,$0E,$0F,$09,$09,$0D
         .BYTE $0F,$10,$0D,$0A,$0A,$09
         ; $60...
         .BYTE $0f,$0f,$0F,$0F,$0F,$0F
         .BYTE $0F,$0F,$0F,$0F,$0F,$0F
         ; $6C...
         .BYTE $0F,$0F,$0F,$0F,$0A,$06
         ; $72...
         .BYTE $09,$09,$09,$09,$0F,$0F
         ; $78...
         .BYTE $0A,$0A,$0A,$0A,$0E,$0E
         .BYTE $0E,$0E,$02,$02,$0F,$0F
         ; $84 ->
         .BYTE $01,$01,$01,$01,$02,$07
         .BYTE $0E,$0E,$0E,$0E,$0E,$0A
         
         ; $90...
         .BYTE $0A,$0A,$0A,$0A,$0A,$0E
         .BYTE $0E,$0E,$0E,$0E,$0E,$0A
         ; $9C...
         .BYTE $01,$0E,$09,$09,$01,$0F
         .BYTE $0F,$0F,$09,$0F,$0A,$09
         ; $A8...
         .BYTE $0D,$0f,$0f,$0f,$0F,$0F
        ; $ae...
         .BYTE $0F,$0f,$01,$01,$01,$0F
         
;--------------------------------------
; ROOM COLOURS
;--------------------------------------
; $00        musta
; $01        valkoinen
; $02        punainen
; $03        cyan
; $04        purppura
; $05        vihreä
; $06        sininen
; $07        keltainen
; $08        oranssi
; $09        ruskea
; $0A        vaalean punainen
; $0B        harmaa 1
; $0C        harmaa 2
; $0D        vaalean vihreä
; $0E        vaalean sininen
; $0F        harmaa 3
;--------------------------------------
; ROOMTYPE id's for each room.
; The room type id indicates the colour
; scheme of the room.
;
; Possible roomtype id's:
;  00 : inside
;  01 : outside
;  02 : cave
;  03 : seaside 
;
; Check data table CHARCOL for individual
; character colours (colour #3).

ROOMTYPE .BYTE $00,$00,$00,$01,$01,$00 
         .BYTE $01,$01,$03,$02,$01,$00 ; -> $0b
         .BYTE $00,$00,$02,$02,$02,$02
         .BYTE $02,$02,$02,$03,$03,$03 ; -> $17

         .BYTE $01,$02,$01,$02,$00,$00 ; -> $1d 
         .BYTE $02,$00 ; -> $1f (last room)

; todo: mieti väriskeemat kullekin huonetyypille
; ja fiksaa merkistö sen mukaisesti
; ulkona 1: oranssi, 2: ruskea, b: sin
; cave 1: -"- , 2: -"- , b: black
; inside 1: cyan, 2: lt. blue, b: black
; background colours for each room type
RMTYPECB .BYTE $00,$06,$00,$0e
; colour 1 for each room type
RMTYPEC1 .BYTE $03,$08,$08,$08
; colour 2 for each room type
RMTYPEC2 .BYTE $0e,$09,$09,$09

;--------------------------------------
; ITEM CHARACTERS
;--------------------------------------
; To add active item character
;--------------------------------------
; - set charcode to OBJCHR datatable
;   id ($00...) of the char is index in table
; - set initial room location for the item to INOBJRM
;   datatable, the room location is a room id
;   (starting from $00)
; - set location in screen memory
;   (see Appendig G in C64 User's Manual)
;   set HI-byte to INOBJHI table and
;   LO-byte to INOBJLO table
; - create string description for item 
; - set data area pointer for item description 
;   to OBJDSCLO and -HI datatables
; - DONT forget to reserve space also to respective
;   ingame item info tables: OBJCHRU, OBJROOM, OBJLO/-HI
;   you can copy the initial values from the static
;   tables edited earlier 
; and IF the item is one of the items to be
; returned to its owner...:
; - set item's bit to zero in ITEMSFND
; - set item id to OWNITEMS datatable 
;   the index in table is the bit index
;   in ITEMSFND-register
; - set owner of the item to OBJOWN table.
;   the index in the table is sprite id 
;   of the owner and the value is the item id
; - add messages regarding to item...
; - (name of the item message)
; - returned item message
;--------------------------------------
; INITIAL ITEM INFO (DO NOT CHANGE)
;-------------------------------------- 
; read character value with item id no.
; $00 $55: nilkkarengas
; $01 $56: hat (muumipappa)
; $02 $57: harmonica
; $03 $58: magnifaction glass
; $04 $59: key (to open swimhouse door)
; $05 $5A: bag
; $06 $5B: button
; $07 $82: dynamite
; $08 $83: candle
; $09 $AC: ruby
; $0A $1E: scroll (to get to crypt)
; $0B $1C: moomin (Energy booster)
; $0C $1C: -"-
;--------------------------------------
; characters representing the items
; listed above
;--------------------------------------
OBJCHR   .BYTE $55,$56,$57,$58,$59,$5A
         .BYTE $5B,$82,$83,$AC,$1E,$1C,$1c,$00
;--------------------------------------  
; needed for restoring items to their
; original places for a new init
;-------------------------------------- 
; item room location in respective
; to the order of the item list above
; end sign: $ff
;--------------------------------------
INOBJRM  .BYTE $09,$00,$18,$04,$19,$0E
         .BYTE $15,$0B,$00,$1B,$19,$00,$01,$FF
         
; char value replacing item 
; when item picked up
; these are initial values, real values are stored
; after item located in a screen
INOBJCHU .BYTE $20,$20,$20,$20,$20,$20
         .BYTE $20,$20,$20,$20,$20,$20,$20,$00

;-------------------------------------
; items location in screen memory
; in respective order to the item
; list above
;--------------------------------------
INOBJLO  .BYTE $23,$A3,$BB,$30,$AA,$B6
         .BYTE $7b,$E0,$D4,$A3,$92,$9a,$9f,$FF
INOBJHI  .BYTE $06,$04,$06,$05,$05,$04
         .BYTE $07,$06,$06,$07,$04,$07,$07,$FF

; item description location in memory
OBJDSCLO .BYTE <OBJDSC0, <OBJDSC1, <OBJDSC2, <OBJDSC3, <OBJDSC4, <OBJDSC5, <OBJDSC6, <OBJDSC7, <OBJDSC8, <OBJDSC9, <OBJDSC10, <OBJDSC11, <OBJDSC11

OBJDSCHI .BYTE >OBJDSC0, >OBJDSC1, >OBJDSC2, >OBJDSC3, >OBJDSC4, >OBJDSC5, >OBJDSC6, >OBJDSC7, >OBJDSC8, >OBJDSC9, >OBJDSC10, >OBJDSC11, >OBJDSC11
        
;-------------------------------------- 
; DYNAMIC TABLES
; FOR STORING INGAME ITEM INFO
;--------------------------------------
; when a new game starts update these
; values from initial values
;--------------------------------------
; char value replacing item 
; when item picked up
OBJCHRU  .BYTE $20,$20,$20,$20,$20,$20
         .BYTE $20,$20,$20,$20,$20,$20,$20,$00
; item room location
OBJROOM  .BYTE $00,$01,$01,$02,$0B,$00
         .BYTE $00,$00,$00,$00,$00,$00,$00,$FF

; items location in screen memory
OBJLO    .BYTE $A0,$AA,$18,$5F,$B8,$A3
         .BYTE $DE,$07,$07,$A5,$92,$00,$00,$FF
OBJHI    .BYTE $07,$07,$06,$04,$06,$07
         .BYTE $06,$07,$07,$07,$04,$00,$00,$FF
;--------------------------------------
; DYNAMIC ITEM TABLES END
;--------------------------------------

; item descriptions
OBJDSC0 .enc screen
        .BYTE "RING"
        .enc none
        .BYTE $00
OBJDSC1 .enc screen
        .BYTE "HAT"
        .enc none
        .BYTE $00
OBJDSC2 .enc screen
        .BYTE "HARMONICA"
        .enc none
        .BYTE $00
OBJDSC3 .enc screen
        .BYTE "MAGNIFYING GLASS"
        .enc none
        .BYTE $00
OBJDSC4 .enc screen
        .BYTE "KEY"
        .enc none
        .BYTE $00
OBJDSC5 .enc screen
        .BYTE "HANDBAG"
        .enc none
        .BYTE $00
OBJDSC6 .enc screen
        .BYTE "BUTTON"
        .enc none
        .BYTE $00
OBJDSC7 .enc screen
        .BYTE "DYNAMITE"
        .enc none
        .BYTE $00
OBJDSC8 .enc screen
        .BYTE "CANDLE"
        .enc none
        .BYTE $00
OBJDSC9 .enc screen
        .BYTE "RUBY"
        .enc none
        .BYTE $00
OBJDSC10
        .enc screen
        .BYTE "ANCIENT SCROLL CONTAINING MAGIC WORDS"
        .enc none
        .BYTE $00
OBJDSC11
        .enc screen
        .BYTE "ENERGY BOOST"
        .enc none
        .BYTE $00

; ORA values for items returned to 
; their owner.
; This table is used when setting item as returned item
; to ITEMSFND-byte and when checking if 
; some object is already returned to 
; it's owner.

; $00 if other item.
; 00000001 HARMONICA
; 00000010 MAG. CLASS
; 00000100 NILKKARENGAS
; 00001000 HANDBAG
; 00010000 HAT
; 00100000 BUTTON
; 01000000 RUBY
; 10000000 -
OBJORA   .BYTE $04,$10,$01,$02,$00,$08
         .BYTE $20,$FF
;--------------------------------------
; OWNED ITEMS
;--------------------------------------
; item id's
; in ITEMSFND bit order from bit 0 to 7
;--------------------------------------
OWNITEMS .BYTE $02,$03,$00,$05,$01,$06
         .BYTE $09,$00
;--------------------------------------
; ITEMS OWNERS
;--------------------------------------
; Read using sprite object id as index.
; Values are item id's.
; This table is used when checking
; is some sprite object is an owner
; of some item object.
; If sprite object doesn't own anything
; the value is $FF.
;--------------------------------------
OBJOWN   .BYTE $FF,$FF,$FF,$02,$03,$FF
         .BYTE $00,$FF,$01,$05,$FF,$06
         ; $0C.....:
         .BYTE $FF,$FF,$FF,$FF,$FF,$FF
         ; $12....:
         .BYTE $09
;--------------------------------------

;--------------------------------------
; CHAR ANIMATIONS
;--------------------------------------
; char value
; $71 wave
; $7c roll
; $7d roll
; $7e elev. up
; $7f elev. down
; $80 lava bubbles
; $81 lava
; $89 fire
;--------------------------------------
CHVAL    .BYTE $71,$7C,$7D,$7E,$7F,$80
         .BYTE $81,$89,$00

CHANILO  .BYTE <CHANIWV,<CHANIRO
         .BYTE <CHANIRL,<CHANIUP
         .BYTE <CHANIDW,<CHANIBUB
         .BYTE <CHANILVA,<CHANIFRE

CHANIHI  .BYTE >CHANIWV,>CHANIRO
         .BYTE >CHANIRL,>CHANIUP
         .BYTE >CHANIDW,>CHANIBUB
         .BYTE >CHANILVA,>CHANIFRE
         
; bubbling lava animation

;0000 0000 $00
;0000 0000 $00
;0000 0000 $00
;0000 0000 $00
;0000 0000 $00
;0000 0000 $00
;0011 0000 $30
;0100 1000 $48

;0000 0000 $00
;0000 0000 $00
;0000 0000 $00
;0011 0000 $30
;0100 1000 $48
;0100 1000 $48
;0011 0010 $30
;0000 0101 $05

;0000 0000 $00
;0100 1000 $48
;0000 0000 $00
;0000 0000 $00
;0100 1000 $48
;0000 0000 $00
;0010 0000 $20
;0101 0010 $52

CHANIBUB .BYTE $00,$00,$00,$00,$00,$00
         .BYTE $30,$48,$00,$00,$00,$30
         .BYTE $48,$48,$30,$05,$00,$48
         .BYTE $00,$00,$48,$00,$20,$52

; lava animation

;1110 1111 $EF
;1111 1111 $FF
;1111 1101 $FD
;1111 1111 $FF
;1111 1111 $FF
;1011 1111 $BF
;1111 1111 $FF
;1111 1011 $FB

;1111 1111 $FF
;1111 1011 $FB
;1111 1111 $FF
;1011 1111 $BF
;1111 1101 $FD
;1111 1111 $FF
;1110 1111 $EF
;1111 1111 $FF

;1111 1011 $FB
;1111 1111 $FF
;1110 1111 $EF
;1111 1111 $FF
;1111 1011 $FB
;1111 1111 $FF
;1111 1111 $FF
;1110 1111 $EF

CHANILVA .BYTE $EF,$FF,$FD,$FF,$FF,$BF
         .BYTE $FF,$FB,$FF,$FB,$FF,$BF
         .BYTE $FD,$FF,$EF,$FF,$FB,$FF
         .BYTE $EF,$FF,$FB,$FF,$FF,$EF
         
; fire animation

;0000 1000 $08
;0000 1000 $08
;0010 1100 $2C
;0010 0110 $26
;0100 1100 $4C
;0010 0110 $26
;0011 1100 $3C
;0001 1000 $18

;0000 0000 $00
;0001 0100 $14
;0011 1000 $38
;0011 1100 $3C
;0111 0100 $74
;0011 1110 $3E
;0011 1100 $3C
;0001 1000 $18

;0000 0000 $00
;0001 0000 $10
;0011 0000 $30
;0010 0100 $24
;0011 0100 $34
;0110 1110 $6E
;0011 1100 $3C
;0001 1000 $18

CHANIFRE .BYTE $08,$08,$2C,$26,$4C,$26
         .BYTE $3C,$18,$00,$14,$38,$3C
         .BYTE $74,$3E,$3C,$18,$00,$10
         .BYTE $30,$24,$34,$6E,$3C,$18

; 3 animation frames for char $71

; frame 2
;0000 0000
;0000 0000
;0000 0100
;0001 0101
;0101 0101
;0101 0101
;0101 0101
;0101 0101

CHANIWV  .BYTE $01,$05,$15,$55,$55,$55
         .BYTE $55,$55,$00,$00,$04,$15
         .BYTE $55,$55,$55,$55,$00,$00
         .BYTE $01,$05,$15,$55,$55,$55
         
; frame 1
;0101 1001 $59
;0101 1001 $59
;1011 1101 $BD
;1011 1101 $BD
;0111 1110 $7E
;0111 1110 $7E
;0110 0101 $65
;0110 0101 $65
; frame 2
;0110 0101 $65
;0110 0101 $65
;0111 1110 $7E
;0111 1110 $7E
;1011 1101 $BD
;1011 1101 $BD
;0101 1001 $59
;0101 1001 $59
; frame 3
;1001 0110 $96
;1001 0110 $96
;0111 1101 $7D
;0111 1101 $7D
;0111 1101 $7D
;0111 1101 $7D
;1001 0110 $96
;1001 0110 $96

CHANIRO  .BYTE $59,$59,$BD,$BD,$7E,$7E
         .BYTE $65,$65,$65,$65,$7E,$7E
         .BYTE $BD,$BD,$59,$59,$96,$96
         .BYTE $7D,$7D,$7D,$7D,$96,$96
         
CHANIRL  .BYTE $59,$59,$BD,$BD,$7E,$7E
         .BYTE $65,$65,$96,$96,$7D,$7D
         .BYTE $7D,$7D,$96,$96,$65,$65
         .BYTE $7E,$7E,$BD,$BD,$59,$59
         
;1111 1111 $FF
;1111 1111 $FF
;1010 1010 $AA
;1010 1010 $AA
;0101 0101 $55
;0101 0101 $55
;0000 0000 $00
;0000 0000 $00
         
CHANIUP  .BYTE $FF,$FF,$AA,$AA,$55,$55
         .BYTE $00,$00,$AA,$AA,$55,$55
         .BYTE $00,$00,$FF,$FF,$55,$55
         .BYTE $00,$00,$FF,$FF,$AA,$AA
         
CHANIDW  .BYTE $00,$00,$55,$55,$AA,$AA
         .BYTE $FF,$FF,$FF,$FF,$00,$00
         .BYTE $55,$55,$AA,$AA,$AA,$AA
         .BYTE $FF,$FF,$00,$00,$55,$55
 
