; Trolly Valley editor
; (C) Mikko Keinänen 2011
; 
; Instructions
; 
; Main menu keys
; 1       : load data
; 2       : save data
; 3       : character editor
; 4       : tile editor
; 5       : screen map editor
; 6       : sprite editor
; 7       : sprite object creator
; 8       : sprite object locater
; 9       : collectible locater
; 0       : 

; Common keys for all program states
; CLR/HOME: Return to main menu

; Common keys in character editor and tile editor
; -----------------------------------------------
;               : Character group selection keys 
; Shift+1       : Background chars
; Shift+2       : Floor chars 
; Shift+3       : Wall chars 
; Shift+4       : Stair chars 
; Shift+5       : Lethal chars 
; Shift+6       : Elevator chars 
; Shift+7       : Climbable chars 
; Shift+8       : Collectible chars 
; Shift+9       : - 
; Shift+0       : All chars / half a set

; 1       : toggle multicolor / normal
; 2       : increase background color 0
; 3       : increase background color 1
; 4       : incerase background color 2

; O       : select previous character used for editing
; P       : select next character used for editing
; 
; Z       : move editor cursor left
; X       : move editor cursor right
; K       : move editor cursor up
; M       : move editor cursor down

; Character editor keys
; ---------------------
; 5       : increase character color (this color will be stored for the character)
; 6       : select multi color pixel type (00)
; 7       : select multi color pixel type (01)
; 8       : select multi color pixel type (10)
; 9       : select multi color pixel type (11)

; N       : set pixel on
; B       : set pixel off

; Tile editor keys
; ----------------
; U       : select previous tile used for editing
; I       : select next tile used for editing
; N       : set selected character to a current location in a tile being edited

atmp                = $02
; currently pressed keycode is stored to $00cb
currkey             = $cb

tmpalo              = $fb
tmpahi              = $fc 

; tmpblo/-hi will be used as a pointer to selected character
; in the edit character memory (store & restore if using 
; for other purposes)
tmpblo              = $fd
tmpbhi              = $fe

tmpclo              = $2b     ; pointer to ...
tmpchi              = $2c     ; ... start of BASIC txt

; tmpdlo/-hi will be used as a pointer to editor screen memory
; (store & restore if using for other purposes)
tmpdlo              = $37     ; pointer to highest...
tmpdhi              = $38     ; ... address used by BASIC

shflag              = $028d

; $02a7 to $02ff is unused memory
; -------------------------------
btmp                = $02a7
ctmp                = $02a8

; number of character groups
nochrgrps           = $02a9

; current pixel in chr editor area
currpx              = $02aa

cursora             = $02ab
cursorb             = $02ac

crsrx               = $02ad
crsry               = $02ae
crsrmax             = $02af
crsrmay             = $02b0
crsrstep            = $02b1
; selected multicolor bit pair mode
mcbitpair           = $02b2

; program state
; 00000000 main menu
; 00000001 character editor
; 00000010 tile editor
; 00000101 screen map editor
; 00001000 character property selector (collectible, floor, ladder, etc.)
; 00010000 sprite editor
; 00100000 sprite object animator
; 01000000 setting sprite objects to rooms
; 10000000 setting collectible objects to rooms
prgstate = $02b3

; selected text colour in print subroutines 
; other uses allowed when not rendering text
strcol   = $02b4

; message column width
; other uses allowed when not rendering text
columnwidth = $02b5

; temporary counter in print routine
; other uses allowed when not rendering text
tmpcnt = $02b6 

; an all purpose tmp variable
tmp    = $02b7

; a program step counter 0..255
pscount = $02b8

; TODO: Now the edit state should be one of the following
;       only characters of certain character group should
;       be presented in character editor and tile editor
;       at once.
; the character group specs (11 bytes)
; byte no:          description:
;chgrp_bg1         = $02b9   ; 1st background char value
                             ; (also text numbers etc.
                             ; characters that are not interacted in game world
                             ; but can be used as background characters)
                             ; characters values from $00...$39 58 chars
;chgrp_bg2         = $02ba   ; no of background chars
;chgrp_fl1         = $02bb   ; 1st floor char
;chgrp_fl2         = $02bc   ; no of floor chars
;chgrp_wl1         = $02bd   ; 1st wall char
;chgrp_wl2         = $02be   ; no of wall chars
;chgrp_sc1         = $02bf   ; 1st stair char
;chgrp_sc2         = $02c0   ; no of stair chars
;chgrp_lh1         = $02c1   ; 1st lethal char
;chgrp_lh2         = $02c2   ; no of lethal chars
;chgrp_ev1         = $02c3   ; elevator 
;chgrp_ev2         = $02c4
;chgrp_cl1         = $02c5   ; climbable (ladder etc)
;chgrp_cl2         = $02c6
;chgrp_co1         = $02c7   ; collectible
;chgrp_co2         = $02c8

; collision can be then simply tested e.g. 
; if character values is >= chgrp_fl1 and character value is < chgrp_wl1 
; it is a floor character and it cannot be fallen through. 

; Default values for these are read from data tables.
; However these values will be saved and loaded togerher with character data.

; start of each character group (8 bytes, $02ba...$02c1)
chrgrpsta           = $02ba
; legth of each chracter group ($02c2...$02c8)
chrgrplen           = $02c2

; selected character group(s)
; bit|show chargroup
; -------------------
; 1  |background 
; 2  |floor
; 3  |wall
; 4  |stair
; 5  |lethal
; 6  |elevator
; 7  |climbable
; 8  |collectible 
chrgrps = $02c9

; numbers of characters in selected character group
chgrpcnt = $02ca

; free memory:
; $02cb-$02ff
; $0313
; $0337-$033b

; $03fc-$03ff
; $c010-$cfff

; index of selected character 
; (note that tmpblo/-hi is used to point the character memory for the character)
curchind            = $0334
; maximimum index to character set that can be selected ...$ff
maxchind            = $0335
; minimum index to character set that can be selected ...$00
minchind            = $0336

scrmemp1            = $0400
scrmemp2            = $0500
scrmemp3            = $0600
scrmemp4            = $0700

colmemp1            = $d800
colmemp2            = $d900
colmemp3            = $da00
colmemp4            = $db00

scrmem_tiles        = $0518

scrmemitms          = $041e

; a screen memory location to present 
; the selected character (1:1)
scrcurch            = $0678

; start address of character editor area
; (magnified character)
chedstart           = $06c8

;-----------------------------------------------------------
; data areas (stored to basic memory) 
; these will be stored/restored to/from disk 
;
; currently from $2b00 to $37ff 
;
; game data to be stored ... $37ff
;

; Character groups for collision testing etc.
; Character can belong only to a one group 
; at once.  

; store character colour values to one page in memory in
chrcolors           = $3300 ; (11008) ... ends in $33ff

; Tile data is stored from $3400 to $37ff (4 pages of memory)
; Each tile consists of 4 characters (2 x 2).
; The first character of a tile x is in tiledata1,x, 
; 2nd character in tiledata2,x, etc.
; This means that we have 255 tiles.

tiledata1           = $3400   ; ... $34ff
tiledata2           = $3500   ; ... $35ff
tiledata3           = $3600   ; ... $36ff
tiledata4           = $3700   ; ... $37ff

; the character set is located to memory $3800...$3fff
chrdata1            = $3800 ;... $3bff
chrdata2            = $3fff
;-----------------------------------------------------------

prgdata             = $8000 ; ...

; character generator ROM image
chrrom              = $d000 ;... $dfff

; raster register
; returns the lower 8 bits of the current raster position
; raster is not visible between 51...251
raster              = $d012
; vic-II control register
vicctrlreg          = $d016
; vic memory control register
vicmemctrlreg       = $d018
; background color #0 
bgcolor0            = $d021
; background color #1 
bgcolor1            = $d022
; background color #2 
bgcolor2            = $d023
;------------------------------------
; list of functions with short
; short descriptions 
;------------------------------------
; mainloop
; init
; initstate
;   initializes the current program state
; mainmenu
;   renders the main menu
; readk
;   read keyboard input
; readka
; readkb
; readchredk
;   read keyboard input (numeric keys)
; incbgc0
;   increase background color 0
; incbgc1
;   increase background color 1
; incbgc2
;   increase background color 2
; mcbitpair0
;   set multicolor bitpair 00 mode in character editor (and possibly sprite editor?)
; mcbitpair1
;   set multicolor bitpair 01 mode in character editor (and possibly sprite editor?)
; mcbitpair2
;   set multicolor bitpair 10 mode in character editor (and possibly sprite editor?)
; mcbitpair3
;   set multicolor bitpair 11 mode in character editor (and possibly sprite editor?)
; deccurch
;   select previous character from editable characters
; inccurch
;   select next character from editable characters
; setselch
;   Renders character magnified to 8:1
; rstcrsr
;   restore the previous state of cursor
; shwcrsr
;   show cursor
; rendbyte
;   renders a byte (bit by bit) from right to left
;   to given position in screen memory
; rendbytm
;   render a multicolor character byte from right to left
;   to given position in screen memory
; tglchrst
;   toggle character mode (standard / multicolor)
; ldchrset
;   load the character set from disk
; svchrset
;   Save character set to disk
; initchrset
;   Initialize character set
; clearscr 
;   fill screen memory with empty space characters
; incrow
;   increase row in screen memory
; prntiles
;   Print 4x4 character graphics tiles to screen
; paintile
;   renders a single tile
; tgchedmem
;   toggle the character memory half being
; mvchredmem
;   copies 1k of character memory being edited to/from edit memory bank
; dmpstdch
;   dumps standard character set from character generator ROM to RAM
; chm2colm
;   sets pointer to color memory
; mvlft
;   move editor cursor to left
; mvrgt
;   move editor cursor right
; mvup
;   move editor cursor up
; mvdown
;   move editor cursor down
; px1off
;   sets selected character pixel off
; px1on
;   sets selected character pixel on
; pxmc1off
;   sets selected multicolor character pixelpair off
; pxmc1on
;   sets selected multicolor character pixelpair on
; print
;   prints messages to screen
; printstr
;   prints string to screen
; tstinc
;   increment two byte value
; tstdec
;   decrement two byte value 
;------------------------------------

          ; sys 32768
          *= prgdata 

; initializing:

          jsr init
          
          ; continue to mainloop

;------------------------------------
mainloop
          ; check the current raster
          ; beam location
          lda raster
          cmp #$ff
          bne mainloop ; busy wait
          
          inc pscount
          jsr readk
          jsr chkprgstate
          jmp mainloop 
;------------------------------------
init
          ; load the initial character group data
          ; (this will be replace from file when
          ;  data is loaded)
          lda #$08
          sta nochrgrps 
          ldx #$00
init_loop

          lda dchrgrpstart,x
          sta chrgrpsta,x
          lda dchrgrplen,x
          sta chrgrplen,x
          inx
          cpx nochrgrps
          bne init_loop

          ; load the standard character set
          ; to location of characters used in screen
          ; this is done only once, after starting
          ; the program and before loading character
          ; set from disk

          lda #<chrdata1
          sta tmpclo
          lda #>chrdata1
          sta tmpchi
          ; dump the standard character set from rom to ram
          jsr dmpstdch 

          jsr initchrset
          lda #$00
          sta prgstate        ; initial program state is main menu 
          jsr mainmenu 
          rts
;------------------------------------
infiniloop
          inc $d020
          jmp infiniloop
          rts

;------------------------------------
; divide
;
; input:
; accu contains the divident
; atmp contains the divider
;
; output:
; x will contain the result
; y will contain the remainder
; accu contains the original divident 
;------------------------------------
divide

          ; store accu
          sta btmp

          ldx #$00

          ; set the c-flag
          sec

divide_loop

          inx
          tay                 ; store possible remainder to y
          sbc atmp            ; subract divider
          beq divide_done     ; no remainder
          bcs divide_loop     ; c flag not cleared, continue
          ; since there was a remainder, decrease x
          ; y contains remainder
          dex

divide_done

          ; restore accu
          lda atmp
          rts
;------------------------------------
; Adds with counter to tmpalo/-hi
; Set the value for addition to accu.
; Set to y how many times this value
; should be added (multiplication).
; accu and y-register is restored 
; to original value before returning.
;------------------------------------
add2tmpb
          sty btmp 
          sta atmp

add2tmpb_lo

          clc
          lda tmpblo
          adc atmp 
          sta tmpblo
          lda tmpbhi
          adc #$00  ; add the possible counter
          sta tmpbhi
          dey
          bne add2tmpb_lo
          
          lda atmp
          ldy btmp
          rts

;------------------------------------
; blink the character selector cursor
;------------------------------------
cursor
          ; if program state is char ed or tile ed
          ; blink the character selector cursor

          ; Curchind contains the index of the selected character.
          ; To get the correct place in screen memory, subtract
          ; minimum cursor index from curchin and add to start of
          ; the screen memory.

          sec
          lda curchind
          sbc minchind
          tax

          ; note: the cursor blink speed depends on which bit we compare
          ; - the first (rightmost) bit changes every other round, so the blinking speed is huge
          ; - the next bit changes every fourth round so the blinking speed halfs, etc
          ; - here we use 5th bit, which seems nice blinking rate
          lda pscount
          and #$10 ; 00010000
          beq cursor_blink

          ; load the current character index
          lda curchind 
          jmp cursor_setcrchr

cursor_blink

          lda #$00 ; @ sign

cursor_setcrchr
          
          sta scrmemp1,x
          rts

;------------------------------------
; Move the character selection cursor
; if o or p keys are pressed and minimum
; or maximum index is not reached.
;
; This is called from the main input
; routine (readk) where is already
; tested if key is pressed at 
; first place.
;------------------------------------
movchrcr
          ; read direction keys first
          ; to see if movement is needed

          lda currkey
          sta tmp

          cmp #$26          ; o key
          bne movchrcr_p1 
          jmp movchrcr_m1          ; ok, do movement

movchrcr_p1

          cmp #$29          ; p key
          bne movchrcr_x  ; just exit, no movement needed

movchrcr_m1

          ; movement was requested 
          ; restore character under the cursor

          ; curchind contains now the index of selected character
          ; to get the correct place in screen memory, subtract
          ; minimum cursor index from curchin and add to start of
          ; the screen memory.

          sec
          lda curchind
          sbc minchind
          tay                     ; Note (TODO): it is an error if c flag is cleared

          ; y contains now the index to screen memory location
          ; from start of the screen memory

          ; set the start of the screen memory to tmpalo/-hi

          lda #<scrmemp1
          sta tmpalo
          lda #>scrmemp1
          sta tmpahi

          ; set the character under cursor before movement to .A
          lda curchind
          sta (tmpalo),y

          ; now the character under cursor is restored and we can move the cursor

          lda tmp           ; load the pressed key
          cmp #$26          ; o key
          bne movchrcr_p2 
          jsr deccurch
          jmp movchrcr_m2

movchrcr_p2

          jsr inccurch

movchrcr_m2

          ; check program state if 
          ; need to update the character editor view
          lda prgstate
          lsr
          bcc movchrcr_x
          jsr setselch

movchrcr_x

          rts
;------------------------------------
; character selection specific 
; main loop actions
;------------------------------------
updatechrsel

          ; all the screens with character selection
          ; (character and tile editor)
          jsr cursor 
          ; todo more
          rts

;------------------------------------
chkprgstate 
;------------------------------------

          lda prgstate
          bne chkprgstate_chedit

          ; main menu specific stuff
          rts

chkprgstate_chedit

          ; character editor specific stuff

          lsr
          bcc chkprgstate_tile

          ; do all the character editor specific main loop stuff
          jsr updatechrsel
          rts

chkprgstate_tile

          lsr
          bcc chkprgstate_screenmap

          ; do all the tile editor specific main loop stuff          
          jsr updatechrsel
          rts

chkprgstate_screenmap

          lsr
          ; bcc ...
          ; TODO
          ; rest of the states
          rts

;------------------------------------
; render main menu
;------------------------------------
mainmenu
          lda #$00
          sta prgstate
          jsr clearscr
          lda #$00
          jsr setcolmem 
          ldx #<txtmenu
          ldy #>txtmenu
          jsr print
          rts
;------------------------------------
inichared

          ; set the program state to character editor
          lda #$01
          sta prgstate
          lda #$00
          sta currpx
          sta crsrx
          sta crsry
          sta mcbitpair
          lda #$07
          sta crsrmax
          sta crsrmay

          lda #$01
          sta crsrstep

          ; set up the character group
          ; background character group is selected by default 
          sta chrgrps 
          jsr setchrgrp
          jsr clearscr
          jsr prnchrset
          
          ; set the pointer to current pixel
          ; in the editor area
          lda #<chedstart
          sta tmpdlo
          lda #>chedstart
          sta tmpdhi

          ; set the colour memory
          lda #$09
          jsr setcolmem

          jsr setselch
          rts

;------------------------------------
; initialize the tile editor screen
;------------------------------------
initileed
          ; set the program state to tile editor
          lda #$02
          sta prgstate

          ; todo : not needed here anymore?
          ;lda #$00
          ;sta minchind
          ;sta curchind
          ;lda #$ff
          ;sta maxchind

          ; set the pointer to current character
          ; in the character memory top half
          ; where editable character set is loaded to
          lda #<chrdata1
          sta tmpblo 
          lda #>chrdata1
          sta tmpbhi

          jsr setcolmem       

          jsr clearscr
          jsr prntiles
          jsr prnchrset

          rts

;------------------------------------
iniroomed
          rts
;------------------------------------
savedata
          jsr svchrset
          rts
;------------------------------------
loaddata
          jsr ldchrset 
          rts
;------------------------------------
readk
          lda currkey 

          ; if no key is pressed, it holds the value #$40
          cmp #$40
          beq readkx

          cmp #$33  ; clr/home key
          bne readk_state
          jsr mainmenu
          rts

readk_state

          ; check program state
          lda prgstate
          bne readk_chredit

          ; read main menu keys
          jsr readkmain
          rts

readk_chredit

          lsr
          bcc readk_tiledit
          jsr movchrcr
          jsr readchsetk
          jsr readchredk
          jsr readka
          jsr readkb
          rts

readk_tiledit
          
          lsr
          bcc readk_roomedit
          jsr movchrcr
          jsr readchsetk
          rts

readk_roomedit

          ; ...

readkx 
          rts
;------------------------------------

readka
          lda currkey

          ; f1 key : load
          cmp #$04
          bne readkf3
          ;load is now done only from main screen ... jsr ldchrset 
          ;TODO: replace this with something else
          jmp readkax

          ; f3 key : save
readkf3   cmp #$05
          bne readkf5
          ; save is now done only from main screen ... jsr svchrset
          ; TODO: replace this with something else
          jmp readkax

          ; f5
readkf5   cmp #$06
          bne readkf7
          nop ; TODO
          jmp readkax


          ; f7
readkf7   cmp #$03
          bne readkq
          nop ; TODO
          jmp readkax

          ;Q
readkq    cmp #$3e
          bne readkz
          ; no need for this anymore : jsr tgchedmem  
          ; TODO: something else? 
          jmp readkax

readkz    ; Z (editor cursor left)
          cmp #$0c
          bne readkxx
          jsr mvlft
          jmp readkax

readkxx   ; X (edit cursor right)
          cmp #$17
          bne readkk
          jsr mvrgt
          jmp readkax

readkk    ; K (editor cursor up)
          cmp #$25
          bne readkm
          jsr mvup
          jmp readkax

readkm    ; M (editor cursor down)
          cmp #$24
          bne readknn
          jsr mvdown
          jmp readkax

readknn   ; N (set single color pixel on)
          cmp #$27  ; 39
          bne readkbb
          jsr px1on 
          jmp readkax

readkbb   ; B (set single color pixel off)
          cmp #$1c ; 28
          bne readkax 
          jsr px1off
          jmp readkax

          ; ...

readkax    rts

;------------------------------------
readkmain
          lda currkey

          ; key 1
          cmp #$38
          bne readkmain_2
          jsr loaddata 
          jmp readkmain_x

readkmain_2         ; key 2
          cmp #$3b
          bne readkmain_3
          jsr savedata 
          jmp readkmain_x

readkmain_3         ; key 3 
          cmp #$08
          bne readkmain_4
          jsr inichared 
          jmp readkmain_x

readkmain_4         ; key 4 
          cmp #$0b
          bne readkmain_5
          jsr initileed
          jmp readkmain_x

readkmain_5         ; key 5 

          cmp #$10
          bne readkmain_x
          jsr iniroomed 

readkmain_x
          rts
;------------------------------------
readkb
          lda currkey
          rts

;------------------------------------
; Read keys for views with character
; selection from character groups. 
; It should have been
; already tested if there's no
; keypress.
;------------------------------------
readchsetk

          ; check first is shift key down
          lda shflag
          lsr
          bcc readchsetk_1
          jsr readchsetksh
          rts

readchsetk_1

          lda currkey
          ; key 1
          cmp #$38
          bne readchsetk_2
          jsr tglchrst
          jmp readchsetk_x

readchsetk_2

          ; key 2
          cmp #$3b
          bne readchsetk_3
          jsr incbgc0
          jmp readchsetk_x

readchsetk_3

          ; key 3 
          cmp #$08
          bne readchsetk_4
          jsr incbgc1
          jmp readchsetk_x

readchsetk_4

          ; key 4 
          cmp #$0b
          bne readchsetk_x
          jsr incbgc2

readchsetk_x

          rts

;------------------------------------
; Read keys with shift key down 
; for views with character
; selection from character groups. 
; It should have been
; already tested if there's no
; keypress and that shift key is pressed
; down.
;------------------------------------
readchsetksh

          lda currkey

          ; key 1 - Background chars
          cmp #$38
          bne readchsetksh_2
          lda #$01
          inc $d020
          jmp readchsetksh_s

readchsetksh_2

          ; key 2 - Floor chars
          cmp #$3b
          bne readchsetksh_3
          lda #$02
          jmp readchsetksh_s

readchsetksh_3

          ; key 3 - Wall chars
          cmp #$08
          bne readchsetksh_4
          lda #$04
          jmp readchsetksh_s

readchsetksh_4

          ; key 4 - Stair chars 
          cmp #$0b
          bne readchsetksh_5
          lda #$08
          jmp readchsetksh_s

readchsetksh_5

          ; Lethal chars

          cmp #$10
          bne readchsetksh_6
          lda #$10
          jmp readchsetksh_s

readchsetksh_6

          ; Elevator chars

          cmp #$13
          bne readchsetksh_7
          lda #$20
          jmp readchsetksh_s

readchsetksh_7

          ; Climbable chars

          cmp #$18
          bne readchsetksh_8
          lda #$40
          jmp readchsetksh_s

readchsetksh_8

          ; Collectible chars

          cmp #$1b
          bne readchsetksh_9
          lda #$80
          jmp readchsetksh_s

readchsetksh_9

          ; none yet

          cmp #$20
          bne readchsetksh_0
          jmp readchsetksh_s

readchsetksh_0

          ; none yet

          cmp #$23
          bne readchsetksh_x

readchsetksh_s

          ; now set the selected character group(s)
          ; only one group at once for now
          sta chrgrps
          jsr setchrgrp

readchsetksh_x

          rts

;------------------------------------
; Read character editor specific 
; keypresses. It should have been
; already tested if there's no
; keypress.
;------------------------------------
readchredk
          lda currkey
          
          ; key 5
          cmp #$10
          bne readkchredk_6
          jsr incchcol
          jmp readkchredk_x

          ; key 6 
          ; set multicolor bitpair 00
readkchredk_6    
          
          cmp #$13
          bne readkchredk_7
          jsr mcbitpair0
          jmp readkchredk_x

          ; key 7 
          ; set multicolor bitpair 01
readkchredk_7    

          cmp #$18
          bne readkchredk_8
          jsr mcbitpair1
          jmp readkchredk_x

          ; key 8 
          ; set multicolor bitpair 10
readkchredk_8    

          cmp #$1b
          bne readkchredk_9
          jsr mcbitpair2
          jmp readkchredk_x

          ; key 9 
          ; set multicolor bitpair 11
readkchredk_9    

          cmp #$20
          bne readkchredk_x
          jsr mcbitpair3
          jmp readkchredk_x

readkchredk_x
          rts
;------------------------------------
incbgc0
          inc bgcolor0
          rts
;------------------------------------
incbgc1
          inc bgcolor1
          rts
;------------------------------------
incbgc2
          inc bgcolor2
          rts
;------------------------------------
mcbitpair0
          ; set multicolor bitpair 00
          lda #$00
          sta mcbitpair
          rts
;------------------------------------
mcbitpair1
          ; set multicolor bitpair 01
          lda #$40  ; 01000000
          sta mcbitpair
          rts
;------------------------------------
mcbitpair2
          ; set multicolor bitpair 10
          lda #$80  ; 10000000
          sta mcbitpair
          rts
;------------------------------------
mcbitpair3
          ; set multicolor bitpair 11
          lda #$c0  ; 11000000
          sta mcbitpair
          rts
;------------------------------------
; select previous character from editable characters
;------------------------------------
deccurch 
          ; do not decrease if minimum index reached
          lda curchind
          cmp minchind
          beq deccurchx

          ; decrease the current character index
          dec curchind

          ; decrease the current character memory pointer
          ; each character is represented by 8 bytes of data
          sec                 ; set carry
          lda tmpblo
          sbc #$08            ; reduce the pointer for 8 bytes
          sta tmpblo
          lda tmpbhi
          sbc #$00            ; subtract with carry to tmpbhi
          sta tmpbhi

deccurchx
          rts
;------------------------------------
inccurch  ; select next character from editable
          ; characters

          ; do not increase if maximum index reached
          lda curchind
          cmp maxchind
          beq inccurchx

          ; increase the current character index
          inc curchind

          ; increse the current character memory pointer
          ; each character is represented by 8 bytes of data
          clc                 ; clear carry
          lda tmpblo
          adc #$08            ; increase the pointer for 8 bytes
          sta tmpblo
          lda tmpbhi
          adc #$00            ; add with carry to tmpbhi
          sta tmpbhi

inccurchx
          rts

;------------------------------------
clrselch
          ; clears the selected character marker
          ; and the character editor
          rts
;------------------------------------
setselch
          ; Renders character magnified 
          ; to 8:1 (a character presents each bit in character data)
          ; print selected character to screen
          lda curchind
          sta scrcurch 

          ; * character data is stored to 'chrdata'
          ; * each character consists of 8 bytes
          ; * character memory offset for selected
          ;   character is curchind * 8 bytes
          ; * tmpblo/-hi points to selected character
          ;   data in the character data in memory

          ; store the start of memory to print the magnified character
          lda #<chedstart
          sta tmpalo
          lda #>chedstart
          sta tmpahi

          ; render each bit into a character
          ; 8 bits / row

          ldy #$00            ; y is index to character data memory
                              ; need y for indirect access
setselch1
          ; tmpblo/-hi points to selected character's data
          ; in the character memory
          lda (tmpblo),y      ; load a byte from character data 
          sta atmp

          ; render byte according to character mode:
          ; normal or multi color 
          lda vicctrlreg
          and #$10            ; 00010000
          bne setselch2

          jsr rendbyte        ; normal
          jmp setselch3

setselch2 jsr rendbytm        ; multi color
setselch3

          ; set the start of next row in the character editor
          ; screen memory 
          clc                 ; clear carry
          lda tmpalo
          adc #$28            ; add a row
          sta tmpalo
          lda tmpahi
          adc #$00            ; add carry (if set)
          sta tmpahi

          iny                 ; next byte in memory, next row in editor
          cpy #$08
          bne setselch1

          ; before callings shwcrsr, set tmpdlo/-hi
          lda #<chedstart
          sta tmpdlo
          lda #>chedstart
          sta tmpdhi
          jsr shwcrsr

          rts
;------------------------------------
rstcrsr
          ; the current location of editor cursor
          ; is set to tmpdlo / -hi

          ; restore the previous state
          ldy #$00
          lda cursorb
          sta (tmpdlo),y

          rts
;------------------------------------
; the current location of editor cursor
; is set to tmpdlo / -hi
;------------------------------------
shwcrsr   

          ldy #$00

          ; store the previous state
          lda (tmpdlo),y
          sta cursorb

          lda cursora
          sta (tmpdlo),y

          ; reset tmpdlo/-hi

          lda #<chedstart
          sta tmpdlo
          lda #>chedstart
          sta tmpdhi
          
          ; add columns
          clc
          lda tmpdlo 
          adc crsrx 
          sta tmpdlo
          lda tmpdhi
          adc #$00
          sta tmpdhi

          ldx crsry
          cpx #$00
          beq shwcrsr2

          ; add rows
shwcrsr1
          clc
          lda tmpdlo
          adc #$28
          sta tmpdlo
          lda tmpdhi
          adc #$00
          sta tmpdhi
          dex
          bne shwcrsr1

shwcrsr2

          ; single color character consists of 8 bytes => 8 x 8 bytes and also 64 pixels 
          ; a multi color pixel consists of a bit pair
          ; if in multicolor mode the tmpdlo/-hi points to left bit of bit pair 

          ; the original character will be stored to
          ; cursorb

          ; the cursor character value is stored in
          ; cursora

          ldy #$00
          lda (tmpdlo),y
          sta cursorb

          lda cursora
          sta (tmpdlo),y

          rts
;------------------------------------
rendbyte    
          ; render a byte (bit by bit) from right to left 
          ; to given position in screen memory

          ; atmp contains byte to be rendered
          ; tmpalo/-hi contains the start byte in screen memory

          ; set tmpclo/-hi to point to color memory accordingly
          jsr chm2colm 

          ; store y to stack
          tya                 ; y -> acc
          pha                 ; acc -> stack

          ldy #$00            ; index to screen memory
                              ; need y for indirect access

rendbyte1
          asl atmp            ; roll a bit from left to carry
          bcs rendbyte2       ; draw char if carry was set

          lda #$20            ; empty space
          jmp rendbyte3

rendbyte2 
          lda curchind

rendbyte3 
          sta (tmpalo),y      ; store empty or mark to char editor screen mem
          lda #$03            ; TODO: set colour dynamically
          sta (tmpclo),y
          iny
          cpy #$08            ; was this last bit to be rendered?
          bne rendbyte1       ; continue if not...

          ; restore y from stack
          pla                 ; stack -> acc
          tay                 ; acc -> y

          rts
;------------------------------------
rendbytm
          ; render a multicolor character byte from right to left 
          ; to given position in screen memory

          ; atmp contains byte to be rendered
          ; tmpalo/-hi contains the start byte in screen memory

          ; set tmpclo/-hi to point to color memory accordingly
          jsr chm2colm 

          ; multicolor character is created by bit pairs:
          ; 00 background color #0 (screen color) $d021
          ; 01 background color #1                $d022
          ; 10 background color #2                $d023
          ; 11 character color                    color RAM

          ; store y to stack
          tya                 ; y -> acc
          pha                 ; acc -> stack

          ; The bit pairs will be tested by rolling the 
          ; character data byte two bits per iteration
          ; to the right. A bitmask 11000000 will be used
          ; to evaluate only the first two bits. 

          ; the marker character will be drawn using 
          ; the '11' bytes and the color ram will be used
          ; for coloring

          ldy #$00            ; index to screen memory
                              ; need y for indirect access

          ; color to be used will be stored to btmp
          ; char to be used will be stored to ctmp
rendbytm1
          lda atmp
          and #$c0            ; 11000000 check only 2 bytes per iteration 

          cmp #$00            ; 00
          bne rendbytm2
          ; draw pair of empty space chars
          lda #$20            ; empty space
          sta ctmp
          jmp rendbytm5

rendbytm2 cmp #$40            ; 01
          bne rendbytm3
          ; draw pair of bg color #1 marker chars 
          lda bgcolor1 
          sta btmp
          lda #$66
          sta ctmp
          jmp rendbytm5

rendbytm3 cmp #$80            ; 10
          bne rendbytm4
          ; draw pair of bg color #2 marker chars
          lda bgcolor2 
          sta btmp
          lda #$66
          sta ctmp
          jmp rendbytm5

rendbytm4 cmp #$c0            ; 11 
          bne rendbytm5
          ; cmp #$03 no need to compare any more
          ; draw pair of character color marker chars
          lda #$66
          sta ctmp

          lda #$04 ; TODO: set dynamically
          sta btmp

rendbytm5 
          ; shift atmp left by a bit pair
          rol atmp
          rol atmp
          lda ctmp
          sta (tmpalo),y      ; store empty or mark to char editor screen mem
          lda btmp 
          sta (tmpclo),y
          iny
          lda ctmp
          sta (tmpalo),y
          lda btmp 
          sta (tmpclo),y
          iny
          cpy #$08            ; was this last bit to be rendered?
          bne rendbytm1       ; continue if not...

          ; restore y from stack
          pla                 ; stack -> acc
          tay                 ; acc -> y

          rts
;------------------------------------
; load character set from disk
;------------------------------------
ldchrset
          ; call SETLFS (Set up a logical file) 
          ; kernal routine (similar to OPEN command in BASIC)
          lda #$0f            ; logical file number
                              ; can be selected from 1...127
                              ; identifies logically the file
          ldx #$08            ; device number: 8
          ldy #$ff            ; secondary address (no command) 
                              ; select from 0...14 for buffer
                              ;
                              ; Wikipedia:
                              ; "...refers to a specific communication"
                              ; "channel established with the device's controller."
                              ; "In disk drives, channel numbers from 0 to 14"
                              ; "inclusive are used to communicate with buffers"
                              ; "within the controller, hence establishing "
                              ; "communications with a particular file in a disk unit."

                              ; "Channel 15 is reserved for communicating with the "
                              ; "controller itself, and thus is known as "
                              ; "the command channel"
                              ; e.g. OPEN 15,8,15,"S0:file name":CLOSE 15
                              ; deletes file from disk

                              ; when storing to device: 
          jsr $ffba           ; SETLFS routine

          ; call SETNAM (Set file name parameters)
          ; kernal routine
          lda #$03            ; file name length
          ldx #<fnchrset      ; pointer to file name (low-byte)
          ldy #>fnchrset      ; pointer to file name (hi-byte)
          jsr $ffbd           ; SETNAM routine
                    
          ; call LOAD (Load or verify file) ; kernal routine
          lda #$00            ; 0 = load, 1-255 verify
          ; set the memory location where to store the data:
          ;ldx #<chrdataed1      ; load address (low-byte)
          ldx #<chrcolors
          ;ldy #>chrdataed1      ; load address (hi-byte)
          ldy #>chrcolors
          jsr $ffd5           ; LOAD routine

          rts
;------------------------------------
svchrset  ; Save character set to disk.

          ; "Memory is saved from an indirect address on page 0
          ; specified by the accumulator to the address stored
          ; in the .X and .Y registers."

          ; "Sent a logical file to an input/ouput device."
          ; call SETLFS (Set up a logical file) 
          ; kernal routine (similar to OPEN command in BASIC)

          lda #$0f ; set logical file no to 15
          ldx #$08 ; device 8

          ldy #$ff ; no command

          ; call SETLFS kernal routine
          jsr $ffba

          ; setname function call

          lda #$06             ; filename length
          ldx #<fnchrssv       ; filename locatin low-order byte
          ldy #>fnchrssv       ; filename location high order byte
          jsr $ffbd            ; call SETNAM kernal routine

          ; save function call

          ; set data start address

          ;lda #<chrdataed1 
          lda #<chrcolors
          sta tmpalo
          ;lda #>chrdataed1 
          lda #>chrcolors
          sta tmpahi

          ; set data end address

          ldx #<chrdata2
          ldy #>chrdata2

          ; set data start pointer
          ; low byte to accu

          lda #<tmpalo

          ; call SAVE (save memory to device)
          jsr $ffd8

          rts

;------------------------------------
;------------------------------------
;---- CHARACTER SET AND COLOUR ------
;--- MEMORY SPECIFIC SUBROUTINES ----
;------------------------------------
;------------------------------------

;------------------------------------
initchrset

; Location of character memory is controlled by
; bits 3,2 and 1 of the VIC-II control register located
; at $d018 (vicmemctrlreg).

; * VIC-II chip can access 16k of memory at time
; * 4 possible banks of 16k memory (in 64k)
; * the bits 1 and 2 at $dd00 is used to control the banks
;
; 00 $c000-$ffff
; 01 $8000-$bfff
; 10 $4000-$7fff
; 11 $0000-$3fff (default)
;
; * In this case I'll stick to the default bank $0000-$3ffff

; * normal character set contains 256 characters
; * each character takes 8 bytes
;   => 256 x 8 = 2k
;   => 8 possible locations for character set in a 16k memory bank

; Possible character memory blocks are:
; 000 $0000-$07ff
; 001 $0800-$0fff
; 010 $1000-$17ff
; 011 $1800-$1fff
; 100 $2000-$27ff
; 101 $2800-$2fff
; 110 $3000-$37ff
; 111 $3800-$3fff

         lda vicmemctrlreg
         ora #$0e            ; 00001110 sets the location
                             ; of character memory to $3800-$3fff
                             ; the character set is loaded from
                             ; file in ldchrset to this area
         sta vicmemctrlreg
         rts

;------------------------------------
; toggles character mode 
; standard / multicolor  
;------------------------------------
tglchrst  

          ; Set multicolor mode 
          ; (bit 4 in vic control register)

          lda vicctrlreg
          eor #$10             ; 00010000
          sta vicctrlreg

          and #$10            ; 00010000
          bne tglchrst1 

          ; single color
          ; cursor step is one
          lda #$01
          sta crsrstep
          jmp tglchrst2 

tglchrst1 ; multi color
          ; cursor step is two
          lda #$02
          sta crsrstep 
          ; restore character under cursor
          jsr rstcrsr

          ; reset cursor
          lda #$00
          sta crsrx
          sta crsry

          lda #<chedstart
          sta tmpdlo
          lda #>chedstart
          sta tmpdhi

          jsr shwcrsr
          
tglchrst2 jsr setselch
          rts

;------------------------------------
; fills the screen memory with space character
;------------------------------------
clearscr  
          ; fill screen memory with
          ; empty space characters

          lda #$20 ; empty space character
          ldx #$00
clearscr1
          ; screen memory starts from $0400
          sta scrmemp1,x         
          sta scrmemp2,x
          sta scrmemp3,x
          dex
          bne clearscr1

          ; clear the rest of the screen until $07e7
          ldx #$e7
clearscr2
          sta scrmemp4,x
          dex
          bne clearscr2
          sta scrmemp4 
          rts

;------------------------------------
; increases the character color value
; for selected character, this
; value will be stored for the 
; character. Values 8...15 are multi
; color values.
;------------------------------------
incchcol
          ; curchind contains the 
          ; index for selected character

          ; to get the actual screen
          ; index subtract minchind
          ; from curchind

          sec
          lda curchind
          sbc minchind

          ; c should never be set zero in this case
          ; curchind is always more than or equal 
          ; to minchind

          tax

          ; get the current colour value
          lda colmemp1,x
          adc #$01

          ; store the increased colour value
          sta colmemp1,x

          rts

;------------------------------------
; sets the colour memory
; input: accu contains the colour value
;------------------------------------
setcolmem
          ; fill color memory with
          ; given colour code
          ; NOTE: if the value in colour memory is
          ; from 0 to 7 the corresponding space on screen
          ; will be rendered in hi-res in the chosen colour.
          ; if the colour is from 8 to 15 that space will
          ; be displayed in multi-color mode.
          ; see pg. 115 in C64 Programmer's Reference Guide

          ldx #$00
setcolmem_1
          ; color memory starts from $d800
          sta colmemp1,x         
          sta colmemp2,x
          sta colmemp3,x
          dex
          bne setcolmem_1 

          ; set the rest of the colour memory until $dbe7
          ldx #$e7
setcolmem_2
          sta colmemp4,x
          dex
          bne setcolmem_2 
          sta colmemp4 
          rts

;------------------------------------
; print the character sub set from
; minchind to maxchind
;------------------------------------
prnchrset


          ; Set the start character regarding 
          ; to value in minchind

          lda minchind
          ldx #$ff

prnchrset_loop

          inx
          cpx maxchind
          bcs prnchrset_x

          sta scrmemp1,x
          adc #$01
          jmp prnchrset_loop

prnchrset_x

          rts

;------------------------------------
incrow
          ; tmpalo/hi points to screen
          ; memory
          ; increase tmpalo/hi by 
          ; one row (40 chars)
          clc
          lda tmpalo
          adc #$28
          sta tmpalo
          lda tmpahi
          adc #$00
          sta tmpahi
          rts

;------------------------------------
; Set the character group settings.
; Point tmpblo/-hi to the character
; data where the selected
; character group starts from.
; set the number of characters in 
; selected character group to chgrpcnt 
;------------------------------------
setchrgrp
          ; - check which bit is set
          ;   in the setchrgrp
          ; - calculate the pointer
          ;   to the start of character
          ;   group data in the character
          ;   set memory 
          ; - set the minchind, maxchind,
          ;   chgrpcnt and curchind accordingly
          ;   maxchind is always minchind + chrgrpcnt

          ; set the tmpblo/-hi to the start of character memory
          lda #<chrdata1
          sta tmpblo
          lda #>chrdata1
          sta tmpbhi

          ldx #$00

          lda chrgrps
          ; check that ctmp is not used in called subroutines for anything
          sta ctmp

          ldy #$08 ; add 8 times in add2tmpb, each character needs 8 bytes

          ; go through the character group definitions until the selected character group 
          ; is found, the loop is needed since we need to add to tmpblo/-hi memory pointer
          ; the memory length needed for each previous character group

setchrgrp_loop
          lda chrgrplen,x
          sta chgrpcnt 
          lda chrgrpsta,x 
          sta minchind 

          lsr ctmp
          bcs setchrgrp_cont
          lda chrgrplen,x
          jsr add2tmpb
          inx
          cpx nochrgrps 
          bne setchrgrp_loop

setchrgrp_cont

          clc
          lda minchind
          sta curchind
          adc chgrpcnt
          ;sbc #$01
          sta maxchind

          jsr clearscr
          jsr prnchrset
          jsr setselch

          rts

;------------------------------------
; copy memory.
; source memory is pointed with 
; tmpalo/-hi
; target memory is pointed with 
; tmpclo/-hi
; x contains number of bytes that
; will be copied
;------------------------------------
memcpybytes
          rts
;------------------------------------
; copy memory.
; source memory is pointed with 
; tmpalo/-hi
; target memory is pointed with 
; tmpclo/-hi
; x contains number of pages that
; will be copied
;------------------------------------
memcpypages
          rts
;------------------------------------
;mvchredmem
          ; Copies x pages of character memory 
          ; to/from edit memory bank, where x is 
          ; value stored in X-register.

          ; The source memory is indexed indirectly
          ; using zero page addresses tmpalo/-hi.

          ; The target memory using tmpclo/-hi
          ; set tmpalo/-hi and tmpclo/-hi using
          ; cpchedmem when copying a character set half
          ; for editing and
          ; stchedmem when storing a character set half
          ; being edited.

          ; TODO: an error control if x is more than 8?
          ; (8 pages is full character set)
 
;         ldy #$00

;mvchredmem1
;         lda (tmpalo),y
;         sta (tmpclo),y
;         iny
;         bne mvchredmem1
;         ; y is now zero

;         ; offset memory pointers tmpalo/-hi
;         ; and tmpclo/-hi by one memory page
;         ; (increase high-order byte by one)
;         inc tmpahi
;         inc tmpchi
;         dex
;         bne mvchredmem1 

;         rts

;------------------------------------
; Copies the full or half of the editable 
; character set from chrdadaed1/2 to chrdata1/2.
;
; Note: we copy the whole set
; in the tile and room editor screens
;------------------------------------
;cpchedmem

          ; check first the program state
          ; if tile editor or room editor
          ; - copy the full set
          ; - set the number of pages being copied in x to 8
          ; if character editor 
          ; - copy only half set
          ; - check which half
          ; - set the number of pages being copied in x to 4

;         lda prgstate
;         lsr          ; check the bit 0
;         bcc cpchedmem_fullset

;         ldx #$04 ; number of pages being copied (half character set)

;         ; copy to chrdata2 (upper half of destination character set)
;         lda #<chrdata2
;         sta tmpclo
;         lda #>chrdata2
;         sta tmpchi

;         ; Check the editstate, which half are
;         ; we editing and set the source character
;         ; memory pointer accordingly.

;         lda editstate
;         lsr       ; check the bit 0 
;         bcc cpchedmem_cont1

;cpchedmem1 
;         ; set the source character memory pointer 
;         ; to the half of character set (characters 128-255)

;         lda #<chrdataed2
;         sta tmpalo
;         lda #>chrdataed2
;         sta tmpahi
;         jmp cpchedmem_cont2

;cpchedmem_fullset

;         ldx #$08          ; 8 pages is full set

;         ; copy to chrdata1 (start of destination character set)
;         lda #<chrdata1
;         sta tmpclo
;         lda #>chrdata1
;         sta tmpchi

;cpchedmem_cont1

;         ; set the source character memory pointer 
;         ; to the start of character set

;         lda #<chrdataed1
;         sta tmpalo
;         lda #>chrdataed1
;         sta tmpahi

;cpchedmem_cont2

;         jsr mvchredmem

;         ; now, set the character colours
;         jsr setchrcolrs

;         rts

;------------------------------------
; sets the character colours for
; full or half a character set 
; being presented
;------------------------------------
;setchrcolrs

;         ; copying will be done from end to beginning
;         ; starting from character 255 or 127

;         ldx #$00  ; "copy from"-index, default is start from end 
;                   ; (decrement before use)
;         ldy #$00  ; copy counter, default is full set (0), 
;                   ; other choice would be half set (128)
;                   ; (decrement before use)

;         ; check the program state
;         ; -> full or half set 
;         lda prgstate
;         lsr          ; check the bit 0
;         bcc setchrcolrs_copy ; go with default values

;         ldy #$80  ; copy colour values half set only 

;         lda editstate
;         lsr
;         bcs setchrcolrs_copy

;         ldx #$80   ; start from character colour 127 to 0

;setchrcolrs_copy

;         dey
;         dex

;setchrcolrs_loop

;         lda chrcolors,x 
;         sta colmemp1,y
;         dex
;         dey 
;         bne setchrcolrs_loop

;         rts


;------------------------------------
; stores the half of character set being
; edited from chrdata2 to chrdadaed1/2
; stores also the character color values
; to chrcolors
;------------------------------------
;stchedmem

;         ; check the program state, which half are
;         ; we editing
;         lda editstate
;         lsr       ; check the bit 0 
;         bcs stchedmem1 
;         ; set pointer to memory for characters 0-127
;         lda #<chrdataed1
;         sta tmpclo
;         lda #>chrdataed1
;         sta tmpchi
;         jmp stchedmem2
;stchedmem1 
;         ; ... characters 128-255
;         lda #<chrdataed2
;         sta tmpclo
;         lda #>chrdataed2
;         sta tmpchi
;stchedmem2

;         ; copy to chrdata2
;         lda #<chrdata2
;         sta tmpalo
;         lda #>chrdata2
;         sta tmpahi

;         ldx #$04 ; number of pages being copied (half character set)
;         jsr mvchredmem

;         ; now store the character color values to memory starting from 
;         ; chrcolors,x where x is either 0 or 128 regarding to the half 
;         ; of character set being edited.

;         ldy #$00 ; by default copy colours for first char set half 
;         ldx #$00  

;         lda editstate
;         lsr       ; check the bit 0 
;         bcc stchedmem_cpycolrs

;         ldy #$80  ; copy colours for second char set half

;stchedmem_cpycolrs

;         lda colmemp1,x
;         sta chrcolors,y
;         iny
;         inx

;         cpx #$80 ; copy colour values from first 128 colour memory locations
;         bne stchedmem_cpycolrs

;         rts

;------------------------------------
; dumps standard character set from 
; character generator ROM to 
; RAM (chrdata1) for editing
; input:
; point the target memory
; using tmpclo/-hi
;------------------------------------
dmpstdch 
          ; turn off interrupts
          sei

          ; switch out I/O registers and switch
          ; in character memory to $d000
          lda $01
          and #$fb            ; 11111011
          sta $01

          ; location of character rom
          lda #<chrrom
          sta tmpalo
          lda #>chrrom
          sta tmpahi

          ; character memory is now in $d000-$dfff
          ; dump the character memory
          ldx #$08  ; go through 8 pages of memory
          ldy #$00

dmpstdch1
          lda (tmpalo),y
          sta (tmpclo),y
          iny
          bne dmpstdch1
          ;ldy #$00 ; this is alredy 00

          ; offset memory pointers tmpalo/-hi, tmpblo/-hi
          ; and tmpclo/-hi by one memory page
          ; (increase high-order byte by one)
          inc tmpahi
          inc tmpchi
          dex
          bne dmpstdch1

          ; switch I/O registers back to $d000 
          ; and character rom out
          lda $01
          ora #$04  ; 00000100
          sta $01

          ; switch back interrupts and return
          cli 
          rts

;------------------------------------
chm2colm
          ; This routine sets pointer to color
          ; memory. Zero page adresses 
          ; tmpalo/-hi must contain
          ; location in screen memory.

          ; note this routine is same as in main game source GETCOLM
          ; TODO: separate common subroutines to one source file
          ;       and unify the variable names

          ; Color memory pointer is set to
          ; zero page adresses tmpclo/-hi.
          
          ; Screen memory location pointer 
          ; values tmpalo/-hi are added with 
          ; $d400 and sum is stored to 
          ; tmpclo/-hi as screen colour 
          ; memory pointers. These pointers 
          ; are used to set colour value
          ; to right location in screen.

          ; load screen memory pointer low byte 
          ; and store to color memory pointer low byte
          lda tmpalo ; same as SCRLO in game         
          sta tmpclo ; same as LOTMP in game 

          ; load screen memory pointer high byte
          ; add #$d4 to it and store to color memory
          ; pointer high byte

          ; NOTE: since the low byte is of the memory
          ; offset value is #$00 there's no need to use
          ; the calculation of lowbyte. Otherwise an addition
          ; using carry would be needed:

          ; clc ; clear carry
          ; lda tmpalo        ; scrlo in game
          ; adc somevalue ; add with carry
          ; sta tmpclo        ; lotmp in game
          ; lda tmpahi        ; scrhi in game
          ; adc anothervalue ; add with carry
          ; sta tmpchi        ; hitmp in game

          clc
          lda tmpahi ; same as SCRHI in game
          adc #$d4
          sta tmpchi ; same as HITMP in game 
          rts

;------------------------------------
;------------------------------------
;---- CHARACTER SET AND COLOUR ------
;--- MEMORY SPECIFIC SUBROUTINES ----
;--------------- END ----------------
;------------------------------------
;------------------------------------

;------------------------------------
;------------------------------------
;-------- TILE SET SPECIFIC ---------
;----------- SUBROUTINES ------------
;------------------------------------
;------------------------------------

;------------------------------------
; Print 4x4 character graphics tiles 
; to screen
;------------------------------------
prntiles
          ; tile data starts from 'tiledata1'

          ; tmpalo/hi and tmpclo/hi will be used 
          ; as screen memory pointers.
          ; the two top row characters will be printed
          ; using tmpalo/hi and the two bottom row 
          ; characters will be printed using
          ; tmpclo/hi

            ; start printing tiles from 
            ; the 8th row

          lda #<scrmem_tiles
          sta tmpalo
          sta tmpclo
          
          lda #>scrmem_tiles
          sta tmpahi
          sta tmpchi

          ; add row to tmpclo/-hi
          clc
          lda tmpclo
          adc #$28
          sta tmpclo
          lda tmpchi
          adc #$00
          sta tmpchi

          ; atmp will be used as a row counter
          lda #$06
          sta atmp

          ldx #$00
          ; y is used as tiles per row counter
          ldy #$0d ; 13
prntiles1
          jsr paintile

          ; next tile
          ; add 03 (three chars) to both
          ; screen memory pointers.
          clc 
          lda tmpalo
          adc #$03
          sta tmpalo
          lda tmpahi
          adc #$00
          sta tmpahi
          clc 
          lda tmpclo
          adc #$03
          sta tmpclo
          lda tmpchi
          adc #$00
          sta tmpchi

          inx
          dey
          bne prntiles1 
          ldy #$0d           ; 14 tiles per row

          ; decrease the row counter
          dec atmp
          ; branch if enough rows have been printed
          beq prntiles2

          ; next row of tiles
          ; add 81 chars (2 rows + 1 char) to both
          ; screen memory pointers.
          clc 
          lda tmpalo
          adc #$51
          sta tmpalo
          lda tmpahi
          adc #$00
          sta tmpahi
          clc 
          lda tmpclo
          adc #$51
          sta tmpclo
          lda tmpchi
          adc #$00
          sta tmpchi
          jmp prntiles1
prntiles2
          rts
;------------------------------------
paintile
          ; paints a tile from tiledata1/2/3/4
          ; indexed by x-register

          ; tmpalo/hi must contain the tile top row 
          ; location in the screen memory

          ; tmpclo/hi must containt the tile bottom 
          ; row location in the screen memory
          
          ; store .Y to stack
          tya
          pha

          ; print top row 1st char
          ldy #$00
          lda tiledata1,x
          sta (tmpalo),y

          ; print top row 2nd char
          iny
          lda tiledata2,x
          sta (tmpalo),y

          ; print bottom row 1st char
          ldy #$00
          lda tiledata3,x
          sta (tmpclo),y

          ; print bottom row 2nd char
          iny
          lda tiledata4,x
          sta (tmpclo),y

          ; restore .Y from stack
          pla
          tay

          rts

;------------------------------------
;------------------------------------
;-------- TILE SET SPECIFIC ---------
;----------- SUBROUTINES ------------
;--------------- END ----------------
;------------------------------------
;------------------------------------





;------------------------------------
mvlft     
          ; move editor cursor to left

          ; check if minimum not reached
          lda crsrx
          cmp #$00
          beq mvlftx

          ; decrease cursor x value by cursor step
          sec 
          lda crsrx
          sbc crsrstep
          sta crsrx
          
          ; restore character at current 
          ; location
          jsr rstcrsr

          ; decrease the location
          ; tmpdlo/-hi points
          ; to screen memory
          sec
          lda tmpdlo
          sbc crsrstep 
          sta tmpdlo
          lda tmpdhi
          sbc #$00
          sta tmpdhi

          ; show cursor at new location
          jsr shwcrsr
mvlftx
          rts
;------------------------------------
mvrgt
          ; move editor cursor right
          lda crsrx
          cmp crsrmax
          beq mvrgtx

          ; extra check for multicolor
          ; mode
          lda vicctrlreg
          and #$10            ; 00010000
          beq mvrgt1 

          lda crsrx
          cmp #$06
          beq mvrgtx

mvrgt1

          clc
          lda crsrx      
          adc crsrstep
          sta crsrx

          jsr rstcrsr

          clc 
          lda tmpdlo
          adc crsrstep 
          sta tmpdlo
          lda tmpdhi
          adc #$00
          sta tmpdhi

          jsr shwcrsr
mvrgtx
          rts
;------------------------------------
mvup
          ; move editor cursor up
          lda crsry
          cmp #$00 
          beq mvupx
          dec crsry      

          jsr rstcrsr

          sec
          lda tmpdlo
          sbc #$28
          sta tmpdlo
          lda tmpdhi
          sbc #$00
          sta tmpdhi

          jsr shwcrsr
mvupx
          rts
;------------------------------------
mvdown
          ; move editor cursor down
          lda crsry
          cmp crsrmay
          beq mvdownx
          inc crsry      

          jsr rstcrsr

          clc 
          lda tmpdlo
          adc #$28
          sta tmpdlo
          lda tmpdhi
          adc #$00
          sta tmpdhi

          jsr shwcrsr
mvdownx
          rts
;------------------------------------
px1off
          ; sets selected character pixel off

          ; multi color mode?

          lda vicctrlreg
          and #$10            ; 00010000
          beq px1off2 

          ; set a multi color bit pair to 00 and return
          jsr pxmc1off
          rts

px1off2    ; single color mode

          ; tmpblo/-hi points to the character
          ; being edited in the character edit
          ; memory
          
          ; the byte that can be indexed using value
          ; from crsry contains the selected bit
          ; 
          ; the selected bit no is contained in crsrx
          ; but in reversed order

          ldy crsry 
          ; (tmpblo),y points to the byte
          ; containing selected bit

          ldx crsrx
          inx
          lda #$ff 
          ; create a mask to set the bit off
          clc       ; set c flag to enable the highest 
                    ; bit on after ror
px1off1    ; roll bit to right until the right bit reached
          ror
          dex
          bne px1off1

          ; A contains the filter, set the bit off
          and (tmpblo),y
          sta (tmpblo),y
          
          jsr setselch

          rts

;------------------------------------
px1on
          ; sets selected character pixel on

          ; multi color mode?

          lda vicctrlreg
          and #$10            ; 00010000
          beq px1on2 

          ; store a multi color bit pair and return
          jsr pxmc1on
          rts

px1on2    ; single color mode

          ; tmpblo/-hi points to the character
          ; being edited in the character edit
          ; memory
          
          ; the byte that can be indexed using value
          ; from crsry contains the selected bit
          ; 
          ; the selected bit no is contained in crsrx
          ; but in reversed order

          ldy crsry 
          ; (tmpblo),y points to the byte
          ; containing selected bit

          ldx crsrx
          inx
          lda #$00 
          ; create a mask to set the bit on
          sec       ; set c flag to enable the highest 
                    ; bit on after ror
px1on1    ; roll bit to right until the right bit reached
          ror
          dex
          bne px1on1

          ; A contains the filter, set the bit on
          ora (tmpblo),y
          sta (tmpblo),y
          
          jsr setselch

          rts
;------------------------------------
pxmc1off
          ; sets selected multicolor character 
          ; pixelpair off

          ; tmpblo/-hi points to the character
          ; being edited in the character edit
          ; memory
          
          ; the byte that can be indexed using value
          ; from crsry contains the selected bit
          ; 
          ; the selected bit number is contained in crsrx
          ; but in reversed order

          ldy crsry 
          ; (tmpblo),y points to the byte
          ; containing selected bit

          ; create a bit mask to .A
          ; 
          ; roll the mask to the correct bit pair slot
          ; (presenting the pixel being removed)

          lda #$3f  ; 0011111

          ldx crsrx
          beq pxmc1off2      ; no need to roll bits, 
                              ; the filter is ready

          ; create a mask to set the bit off
          ; set the c-flag to set the highest bit to 1
          ; after rol
          sec
pxmc1off1 ; roll bit to right until the right bit reached
          ; a bit pair per iteration
          ror
          ror
          ; also decrease .X by two
          dex       ; in multicolor mode the x values are
          dex       ; 0 2 4 or 6 so the test is not going to skip
          bne pxmc1off1

pxmc1off2 ; A contains the filter, set the bit pair to 00
          and (tmpblo),y
          sta (tmpblo),y
          
          jsr setselch

          rts
;------------------------------------
pxmc1on
          ; sets selected multicolor character 
          ; pixelpair on

          jsr pxmc1off

          ; the pixelpair is set at mcbitpair 

          ; tmpblo/-hi points to the character
          ; being edited in the character edit
          ; memory
          
          ; the byte that can be indexed using value
          ; from crsry contains the selected bit
          ; 
          ; the selected bit no is contained in crsrx
          ; but in reversed order

          ldy crsry 
          ; (tmpblo),y points to the byte
          ; containing selected bit

          ; store the selected bit pair combination
          ; as a bit mask to .A
          ; 
          ; roll the filter to the correct bit pair slot
          ; (presenting the pixel being set)

          ; set the selected bit pair mode
          lda mcbitpair ; #$c0  ; 1100000

          ldx crsrx
          beq pxmc1on2        ; no need to roll bits, 
                              ; the filter is ready

          ; create a mask to set the bit on
          ; clear the c-flag to set the highest bit to 0
          ; after rol
          clc
pxmc1on1  ; roll bit to right until the right bit reached
          ; a bit pair per iteration
          ror
          ror
          ; also decrease .X by two
          dex       ; in multicolor mode the x values are
          dex       ; 0 2 4 or 6 so the test is not going to skip
          bne pxmc1on1

pxmc1on2  ; A contains the filter, set the bit pair
          ora (tmpblo),y
          sta (tmpblo),y
          
          jsr setselch

          rts

;------------------------------------
tstinc    ; increment two byte value
          ; (adapted from 'Compute's Programming the Commodore 64 - The Definitive Guide')

          inc tmpalo
          bne tstincx
          ; needed only when low byte
          ; overflowed (from #$ff to #$00)
          inc tmpahi
tstincx
          rts

;--------------------------------------
; initializes string messages
; and prints string messages to screen
; using printstr subroutine
;
; note this routine is mostly same as in main game source 
; TODO: separate common subroutines to one source file
;       and unify the variable names
;
; X, Y must contain the start adress
; of string data - 
; X low order, Y high order byte.
;
; the string data _must_ be in the following format:
; 1st byte : screen mem. location low order byte
; 2nd byte : screen mem. location high order byte
; 3rd byte : colour
; 4th byte : width of the message column
; Xth byte : string data
; $00      : end sign
; NOTE: A String cannot be more than 255 bytes long!
;------------------------------------
print
        ; point tmpclo/-hi to string data
        stx tmpdlo ; LVCHLO in game
        sty tmpdhi ; LVCHI in game

        ; read the screen memory location
        ; for the string to tmpalo/hi
        ldy #$00
        lda (tmpdlo),y ; lvchlo in game
        sta tmpalo ; scrlo in game
        iny
        lda (tmpdlo),y ; lvchlo in game
        sta tmpahi ; scrhi in game
        ; read message colour to strcol
        iny
        lda (tmpdlo),y ; lvchlo in game
        sta strcol
        ; read width of the message column
        ; to columnwidth
        iny
        lda (tmpdlo),y ; lvchlo in game
        sta columnwidth ; MSGCOLWIDTH in game

        ; set tmpclo/hi to the start of actual
        ; string data -> add 4 to tmpclo/hi
        clc
        lda tmpdlo  ; lvchlo in game
        adc #$04
        sta tmpdlo  ; lvchlo in game
        lda tmpdhi  ; lvchi in game
        adc #$00
        sta tmpdhi  ; lvchi in game
        jsr printstr
        rts

;-------------------------------------
; tmpchi/lo must contain the string character data.
; $00 is the end sign for a string.

; note this routine is mostly same as in main game source 
; TODO: separate common subroutines to one source file
;       and unify the variable names

; columnwidth must contain the width of the message column 
; strcol must contain the string colour
; tmpdlo/hi must contain the start location in screen memory
; NOTE: A String cannot be more than 255 bytes long!
;-------------------------------------
printstr

        ; set index counter y-reg to 0        
        ldy #$00
        ; tmpcnt is a width counter
        lda #$00
        sta tmpcnt
        
         ;read screen colour memory pointer values to tmpclo/-hi
         jsr chm2colm ; JSR GETCOLM in game

        ; now finally read the actual string
        ; until $00 occurs
        ; or y goes to zero again (too long string)
        
printstrloop
        lda (tmpdlo),y ; lvchlo in game
        beq printstrx ; end sign $00 occured
        ;cmp #$ff      ; end of line char
        ;beq printstr_nl 
        sty tmp
        ldy tmpcnt
        ; set char to screen
        sta (tmpalo),y ; scrlo in game
        ; set right colour to colour memory of screen location
        lda strcol
        sta (tmpclo),y ; lotmp in game
        ldy tmp
        iny
        beq printstrx ; y went over to 0 (too long string)
        inc tmpcnt
        lda tmpcnt 
        cmp columnwidth 
        ; has column widht already reached
        bne printstrloop      ; continue to next char
        ; column width has been reached
printstr_nl                   ; new line
        lda #$00
        sta tmpcnt
        ; add a row 
        ; (40 chars) to screen 
        ; and colour memory pointer
        clc
        lda tmpalo  ; scrlo in game
        adc #$28 ; add a row
        sta tmpalo  ; scrlo in game
        lda tmpahi  ; scrhi in game
        adc #$00
        sta tmpahi  ; scrhi in game
        clc
        lda tmpclo  ; lotmp in game
        adc #$28
        sta tmpclo  ; lotmp in game
        lda tmpchi  ; hitmp in game
        adc #$00 
        sta tmpchi  ; hitmp in game
        jmp printstrloop
printstrx
        rts
;------------------------------------
tstdec    ; decrement two byte value
          ; (adapted from 'Compute's Programming the Commodore 64 - The Definitive Guide')

          lda tmpalo
          bne tstdec1
          ; need to decrease high byte
          ; only when low byte is #$00
          dec tmpahi

tstdec1   dec tmpalo
          rts
;------------------------------------
fnchrset  .text "CHR"
fnchrssv  .text "@0:CHR"

;------------------------------------
; text format
;------------------------------------
; byte 01: screen location low order byte
; byte 02: screen location high order byte
; byte 03: text colour
; byte 04: text column width 
; byte 05...ff text content
; $ff    : end of line (if smaller than column width)
; $00    : end of text block
;------------------------------------
txtmenu   .byte $01,$04,$04,$15
          .enc screen
          .text " TROLLY VALLEY EDITOR"
          .text " 1 LOAD DATA         "
          .text " 2 SAVE DATA         "
          .text " 3 EDIT CHARACTER SET"
          .text " 4 EDIT TILE SET     "
          .text " 5 EDIT ROOM MAPS    "
          .enc none
          .byte $00
;------------------------------------
          
; Initial character group data.
; this will be copied to actual character group
; data location on startup and will be saved
; to game data and restored with game data.

; byte|chargroup
; -------------------
; 1   |background 
; 2   |floor
; 3   |wall
; 4   |stair
; 5   |lethal
; 6   |elevator
; 7   |climbable
; 8   |collectible 

dchrgrpstart
          .byte $00,$40,$70,$a0,$b8,$c8,$d8,$f0
dchrgrplen
          ;      64 +48 +48 +24 +16 +16 +24 +16 = 256 
          .byte $40,$30,$30,$18,$10,$10,$18,$10

