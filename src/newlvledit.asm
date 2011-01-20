; Trolly Valley editor
; (C) Mikko Keinänen 2011
; 
; Instructions
; Character editor keys:
; 1       : toggle multicolor / normal
; 2       : increase background color 0
; 3       : increase background color 1
; 4       : incerase background color 2
; 5       : select multi color pixel type (00)
; 6       : select multi color pixel type (01)
; 7       : select multi color pixel type (10)
; 8       : select multi color pixel type (11)
; O       : select previous character for editing
; P       : select next character for editing
; Q       : toggle charset half being edited
; Z       : move editor cursor left
; X       : move editor cursor right
; K       : move editor cursor up
; M       : move editor cursor down
; N       : set pixel on
; F1      : load charset from disk
; F2      : save charset to disk

atmp                = $02
; currently pressed keycode is stored to $00cb
currkey             = $cb

tmpalo              = $fb
tmpahi              = $fc 
; tmpblo/-hi will be used as a pointer to selected character
; in the edit character memory
tmpblo              = $fd
tmpbhi              = $fe

tmpclo              = $2b     ; pointer to ...
tmpchi              = $2c     ; ... star of BASIC txt

; tmpdlo/-hi will be used as a pointer to editor screen memory
tmpdlo              = $37     ; pointer to highest...
tmpdhi              = $38     ; ... address used by BASIC

btmp                = $02a7
ctmp                = $02a8

; bit 0:  0 first 1k of charset being edited
;         1 second 1k of charset being edited
prgstate            = $02a9

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

; free memory:
; $02b2-$02ff
; $0313
; $0337-$033b

; $03fc-$03ff
; $c000-$cfff

; index of selected character
curchind            = $0334
maxchind            = $0335
minchind            = $0336

scrmemp1            = $0400
scrmemp2            = $0500
scrmemp3            = $0600
scrmemp4            = $0700

; a screen memory location to present 
; the selected character (1:1)
scrcurch            = $0518

; start address of character editor area
; (magnified character)
chedstart           = $0608

; store character set to basic memory

; character set being edited (half a set at time)
; character data for characters 0-127
chrdataed1          = $3000 ;... $33ff
; character data for characters 128-255 
chrdataed2          = $3400 ;... $37ff
chrdataed3          = $37ff ; character data ends

; the standard character set is copied to $3800
; the editor ui will use the lower half of the character 
chrdata1            = $3800 ;... $3bff
; the half being edited from $3000-$37ff will be copied
; to $3c00-$3fff 
chrdata2            = $3c00 ;... $3fff

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

          ; sys 32768
          *= $8000 
; initializing:

          ;jsr ldchrset 
          jsr dmpstdch
          jsr initchrset
          jsr clearscr
          jsr init
          jsr cpchedmem
          jsr printchs

;------------------------------------
mainloop
          lda raster
          cmp #$ff
          bne mainloop ; busy wait
          
          jsr readk
          jmp mainloop 
;------------------------------------
init
          lda #$00
          sta prgstate
          sta currpx
          sta crsrx
          sta crsry
          sta mcbitpair
          lda #$07
          sta crsrmax
          sta crsrmay
          lda #$01
          sta crsrstep

          ; Set the index of current character
          ; and minimum / maximum character index
          lda #$80
          sta curchind
          sta minchind
          lda #$ff
          sta maxchind

          ; set the pointer to current character
          ; in the character memory top half
          ; where character being edited are loeaded to
          lda #<chrdata2
          sta tmpblo 
          lda #>chrdata2
          sta tmpbhi

          ; set the pointer to current pixel
          ; in the editor area
          lda #<chedstart
          sta tmpdlo
          lda #>chedstart
          sta tmpdhi

          ; set the cursor position

          rts
;------------------------------------
readk
          lda currkey 

          ; if no key is pressed, it holds the value #$40
          cmp #$40
          beq readkx

          jsr readnumk

          ; f1 key : load
          cmp #$04
          bne readkf3
          jsr ldchrset 
          jmp readkx

          ; f3 key : save
readkf3   cmp #$05
          bne readkf5
          jsr svchrset
          jmp readkx

          ; f5
readkf5   cmp #$06
          bne readkf7
          ;inc bgcolor1 
          jmp readkx

          ; f7
readkf7   cmp #$03
          bne readko
          ;inc bgcolor2 
          jmp readkx

          ;O
readko    cmp #$26
          bne readkp
          jsr deccurch
          jmp readkx

          ;P
readkp    cmp #$29
          bne readkq
          jsr inccurch
          jmp readkx

          ;Q
readkq    cmp #$3e
          bne readkz
          jsr tgchedmem  
          jmp readkx

readkz    ; Z (editor cursor left)
          cmp #$0c
          bne readkxx
          jsr mvlft
          jmp readkx

readkxx   ; X (edit cursor right)
          cmp #$17
          bne readkk
          jsr mvrgt
          jmp readkx

readkk    ; K (editor cursor up)
          cmp #$25
          bne readkm
          jsr mvup
          jmp readkx

readkm    ; M (editor cursor down)
          cmp #$24
          bne readkn
          jsr mvdown
          jmp readkx

readkn    ; N (set single color pixel on)
          cmp #$27  ; 39
          bne readkx
          jsr px1on 
          jmp readkx

          ; ...

readkx    rts

;------------------------------------
readnumk
          ; key 1
          cmp #$38
          bne readk2
          jsr tglchrst
          jmp readknumx

          ; key 2
readk2    cmp #$3b
          bne readk3
          inc bgcolor0 
          jmp readknumx

          ; key 3 
readk3    cmp #$08
          bne readk4
          inc bgcolor1 
          jmp readknumx

          ; key 4 
readk4    cmp #$0b
          bne readk5
          inc bgcolor2 
          jmp readknumx

          ; key 5 
          ; set multicolor bitpair 00
readk5    cmp #$10
          bne readk6
          lda #$00
          sta mcbitpair
          jmp readknumx

          ; key 6 
          ; set multicolor bitpair 01
readk6    cmp #$13
          bne readk7
          lda #$40  ; 01000000
          sta mcbitpair
          jmp readknumx

          ; key 7 
          ; set multicolor bitpair 10
readk7    cmp #$18
          bne readk8
          lda #$80  ; 10000000
          sta mcbitpair
          jmp readknumx

          ; key 8 
          ; set multicolor bitpair 11
readk8    cmp #$1b
          bne readknumx
          lda #$c0  ; 11000000
          sta mcbitpair
          jmp readknumx

readknumx

          rts
;------------------------------------
deccurch
          ; do not decrease if minimum index reached
          lda curchind
          cmp minchind
          beq deccurchx

          ; decrease the current character index
          dec curchind

          ; decrease the current character memory pointer
          sec                 ; set carry
          lda tmpblo
          sbc #$08            ; reduce the pointer for 8 bytes
          sta tmpblo
          lda tmpbhi
          sbc #$00            ; subtract with carry to tmpbhi
          sta tmpbhi

          jsr setselch
deccurchx
          rts
;------------------------------------
inccurch 
          ; do not increase if maximum index reached
          lda curchind
          cmp maxchind
          beq inccurchx

          ; increase the current character index
          inc curchind

          ; increse the current character memory pointer
          clc                 ; clear carry
          lda tmpblo
          adc #$08            ; increase the pointer for 8 bytes
          sta tmpblo
          lda tmpbhi
          adc #$00            ; add with carry to tmpbhi
          sta tmpbhi

          jsr setselch
inccurchx
          rts
;------------------------------------
setselch
          ; prints selected character to screen 1:1
          ; and magnified to 8:1 (a character presents
          ; each bit in character data)

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
shwcrsr   
          ; the current location of editor cursor
          ; is set to tmpdlo / -hi

          ldy #$00

          ; store the previous state
          lda (tmpdlo),y
          sta cursorb

          lda cursora
          sta (tmpdlo),y
          rts

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
          lda curchind; #$04

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
tglchrst  ; toggles character mode 
          ; standard / multicolor

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

          ; TODO replace reset cursor with this after implementing screen memory setting by cursor index:
          ;lda #$02
          ;sta crsrstep

          ; set the cursor position divisible by 2
          ;lda crsrx
          ;and #$fe
          ;sta crsrx

          ;lda crsry
          ;and #$fe
          ;sta crsry

          ; TODO: need an update function
          ; to set screen memory pointer according
          ; to crsrx/-y

          ; show cursor
          jsr shwcrsr
          
tglchrst2 jsr setselch
          rts
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
          ldx #<chrdataed1      ; load address (low-byte)
          ldy #>chrdataed1      ; load address (hi-byte)
          jsr $ffd5           ; LOAD routine
          
          jsr cpchedmem
          jsr setselch

          rts

;------------------------------------
svchrset  ; Save character set to disk.

          ; set caracter buffer being edited
          ; to memory
          jsr stchedmem
          ;jsr tgchedmem
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

          lda #<chrdataed1 
          sta tmpalo
          lda #>chrdataed1 
          sta tmpahi

          ; set data end address

          ldx #<chrdataed3
          ldy #>chrdataed3

          ; set data start pointer
          ; low byte to accu

          lda #<tmpalo

          ; call SAVE (save memory to device)
          jsr $ffd8

          rts
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

         ; Set multicolor mode 
         ; (4th bit in vic control register)

         ;lda vicctrlreg
         ;ora #$10             ; 00010000
         ;sta vicctrlreg

         rts

;------------------------------------
clearscr  
          ; fill screen memory with
          ; empty space characters

          lda #$20            ; empty space character
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
printchs
          ; prints the characters 128-255 
          ; to the start of screen memory

          ldx #$00
          ldy #$80
printchs1
          tya
          sta scrmemp1,x
          inx
          iny
          bne printchs1
          rts

;------------------------------------
tgchedmem ; toggle the character memory half being
          ; edited to edit memory buffer
          jsr stchedmem
          lda prgstate
          eor #$01
          sta prgstate
          jsr cpchedmem
          jsr setselch
          rts
;------------------------------------
mvchredmem
          ; copies 1k of character memory being edited
          ; to/from edit memory bank
          ; the source memory is indexed indirectly
          ; using zero page addresses tmpalo/-hi
          ; the target memory using tmpclo/-hi
          ; set tmpalo/-hi and tmpclo/-hi using
          ; cpchedmem when copying a character set half
          ; for editing and
          ; stchedmem when storing a character set half
          ; being edited

          ; TODO: to use this more generally set x in calling 
          ; routine.
          ldx #$04  ; go through 4 pages of memory
          ldy #$00

mvchredmem1
          lda (tmpalo),y
          sta (tmpclo),y
          iny
          bne mvchredmem1
          ; y is now zero

          ; offset memory pointers tmpalo/-hi
          ; and tmpclo/-hi by one memory page
          ; (increase high-order byte by one)
          inc tmpahi
          inc tmpchi
          dex
          bne mvchredmem1 

          rts

;------------------------------------
cpchedmem
          ; copies the half of character set being
          ; edited from chrdadaed1/2 to chrdata2

          ; check the program state, which half are
          ; we editing
          lda prgstate
          lsr       ; check the bit 0 
          bcs cpchedmem1 
          ; set pointer to memory for characters 0-127
          lda #<chrdataed1
          sta tmpalo
          lda #>chrdataed1
          sta tmpahi
          jmp cpchedmem2

cpchedmem1 
          ; ... characters 128-255
          lda #<chrdataed2
          sta tmpalo
          lda #>chrdataed2
          sta tmpahi

cpchedmem2

          ; copy to chrdata2
          lda #<chrdata2
          sta tmpclo
          lda #>chrdata2
          sta tmpchi
          jsr mvchredmem
          rts

          ;ldx #$04  ; go through 4 pages of memory
          ;ldy #$00

;cpchedmem2
          ;lda (tmpalo),y
          ;sta (tmpclo),y
          ;iny
          ;bne cpchedmem2

          ; offset memory pointers tmpalo/-hi
          ; and tmpclo/-hi by one memory page
          ; (increase high-order byte by one)
          ;inc tmpahi
          ;inc tmpchi
          ;dex
          ;bne dmpstdch1

          ;rts
;------------------------------------
stchedmem
          ; stores the half of character set being
          ; edited from chrdata2 to chrdadaed1/2

          ; check the program state, which half are
          ; we editing
          lda prgstate
          lsr       ; check the bit 0 
          bcs stchedmem1 
          ; set pointer to memory for characters 0-127
          lda #<chrdataed1
          sta tmpclo
          lda #>chrdataed1
          sta tmpchi
          jmp stchedmem2
stchedmem1 
          ; ... characters 128-255
          lda #<chrdataed2
          sta tmpclo
          lda #>chrdataed2
          sta tmpchi
stchedmem2

          ; copy to chrdata2
          lda #<chrdata2
          sta tmpalo
          lda #>chrdata2
          sta tmpahi

          jsr mvchredmem
          rts
;------------------------------------
dmpstdch 
          ; dumps standard character set from 
          ; character generator ROM to 
          ; RAM (chrdata1) for editing

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

          ; location of characters used in screen
          lda #<chrdata1
          sta tmpblo
          lda #>chrdata1
          sta tmpbhi

          ; location of character set being edited
          lda #<chrdataed1
          sta tmpclo
          lda #>chrdataed1
          sta tmpchi

          ; character memory is now in $d000-$dfff
          ; dump the character memory
          ldx #$08  ; go through 8 pages of memory
          ldy #$00
dmpstdch1
          lda (tmpalo),y
          sta (tmpblo),y
          sta (tmpclo),y
          iny
          bne dmpstdch1
          ;ldy #$00 ; this is alredy 00

          ; offset memory pointers tmpalo/-hi, tmpblo/-hi
          ; and tmpclo/-hi by one memory page
          ; (increase high-order byte by one)
          inc tmpahi
          inc tmpbhi
          inc tmpchi
          dex
          bne dmpstdch1

          ; switch I/O registers back to $d000 
          ; and character rom out
          lda $01
          ora #$04  ; 00000100
          sta $01

          ; switch back interrupts
          cli 

          rts
;------------------------------------
chm2colm
          ; This routine sets pointer to color
          ; memory. Zero page adresses 
          ; tmpalo/-hi must contain
          ; location in screen memory.

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
          lda tmpalo          
          sta tmpclo 

          ; load screen memory pointer high byte
          ; add #$d4 to it and store to color memory
          ; pointer high byte

          ; NOTE: since the low byte is of the memory
          ; offset value is #$00 there's no need to use
          ; the calculation of lowbyte. Otherwise an addition
          ; using carry would be needed:

          ; clc ; clear carry
          ; lda tmpalo
          ; adc somevalue ; add with carry
          ; sta tmpalo
          ; lda tmpahi
          ; adc anothervalue ; add with carry
          ; sta tmpahi

          clc
          lda tmpahi 
          adc #$d4
          sta tmpchi 
          rts
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
pxmc1on
          ; sets selected multicolor character 
          ; pixelpair on

          ; TODO: call clear bit pair subroutine first!

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
fnchrset  .text "CHR"
fnchrssv  .text "@0:CHR"

