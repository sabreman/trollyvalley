; Trolly Valley editor
; (C) Mikko Keinänen 2011
; 
; Instructions
; Character editor:
; 1      : toggle multicolor / normal
; 2      : increase background color 0
; 3      : increase background color 1
; 4      : incerase background color 2
; O       : select previous character for editing
; P       : select next character for editing
; Q       : toggle charset half being edited

atmp                = $02
; currently pressed keycode is stored to $00cb
currkey             = $cb

tmpalo              = $fb
tmpahi              = $fc 
; tmpblo/-hi will be used as a pointer to selected character
; in the character memory
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
; bit 1:  this is only for keeping track every second main loop run
;         for blinking cursor and maybe some other stuff
prgstate            = $02a9

; current pixel in chr editor area
currpx              = $02aa

; free memory:
; $02ab-$02ff
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
          jsr showcur
          jmp mainloop 
;------------------------------------
init
          lda #$00
          sta prgstate
          sta currpx

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

          rts
;------------------------------------
readk
          lda currkey 

          ; if no key is pressed, it holds the value #$40
          cmp #$40
          beq readkx

          jsr readnumk

          ; f1 key
          cmp #$04
          bne readkf3
          ;jsr tglchrst
          jmp readkx

          ; f3 key
readkf3   cmp #$05
          bne readkf5
          ;inc bgcolor0 
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
          bne readkx
          jsr mvdown
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
          bne readknumx
          inc bgcolor2 
          jmp readknumx
readknumx

          rts
;------------------------------------
showcur 
          ; blink the current selection
          ; in the character editor

          ; the current location of editor cursor
          ; is in tmpdlo / -hi

          ; single color character consists of 8 bytes => 8 x 8 bytes and also 64 pixels 
          ; a multi color pixel consists of a bit pair
          ; if in multicolor mode the tmpdlo/-hi points to left bit of bit pair 

          ; fetch the color memory from cursor location
          ; set tmpclo/-hi to point to screen memory in cursor location
          lda tmpdlo
          sta tmpclo
          clc
          lda tmpdhi
          adc #$d4            ; offset is #$d400
          sta tmpchi
          ldy #$00

          ; load current colour to acc and store to x-register
          lda (tmpclo),y
          tax

          ; check the program state bit 1 (blink or not to blink)
          lda prgstate
          and #$02            ; 00000010
          bne  showcur1

          dex       ; back to original colour
          jmp showcur2
showcur1
          inx       ; increase the color by one 
showcur2
          txa 
          sta (tmpclo),y

          ; check if in multicolor state
          lda vicctrlreg
          and #$10            ; 00010000
          beq showcurx

          ; blink also the other "zoomed" 
          iny
          txa 
          sta (tmpclo),y

showcurx
          ; flip the "every second round" bit
          lda prgstate
          eor #$02
          sta prgstate

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
tglchrst 
          ; toggles character mode 
          ; standard / multicolor

          ; Set multicolor mode 
          ; (bit 4 in vic control register)

          lda vicctrlreg
          eor #$10             ; 00010000
          sta vicctrlreg

          jsr setselch

          rts
;------------------------------------
ldchrset
          ; call SETLFS (Set up a logical file) 
          ; kernal routine (similar to OPEN command in BASIC)
          lda #$0f            ; logical file number
                              ; can be selected from 1...127
                              ; identifies logically the file
          ldx #$08            ; device number: 8
          ldy #$00            ; secondary address 
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
          ldx #<chrdata1      ; load address (low-byte)
          ldy #>chrdata1      ; load address (hi-byte)
          jsr $ffd5           ; LOAD routine

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
tgchedmem
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
          rts
;------------------------------------
mvrgt
          rts
;------------------------------------
mvup
          rts
;------------------------------------
mvdown
          rts
;------------------------------------

fnchrset  .text "CHR"

