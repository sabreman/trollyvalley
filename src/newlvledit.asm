atmp                = $02
; currently pressed keycode is stored to $00cb
currkey             = $cb

tmpalo              = $fb
tmpahi              = $fc 
; tmpblo/-hi will be used as a pointer to selected character
; in the character memory
tmpblo              = $fd
tmpbhi              = $fe

btmp                = $02a7

; free memory:
; $02a7-$02ff
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

; start address of character editor area
; (magnified character)
chedstart           = $0608

; store character set to basic memory
chrdata             = $3800 ;... $3fff
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
          ;jsr initchrset
          jsr clearscr
          jsr printchs
          jsr init

;------------------------------------
mainloop
          lda raster
          cmp #$ff
          bne mainloop ; busy wait
          
          jsr readk
          jmp mainloop 
;------------------------------------
init
          ; Set the index of current character
          ; and minimum / maximum character index
          lda #$00
          sta curchind
          sta minchind
          lda #$ff
          sta maxchind

          ; set the pointer to current character
          ; in the character memory.
          lda #<chrdata
          sta tmpblo 
          lda #>chrdata
          sta tmpbhi

          rts
;------------------------------------
readk
          lda currkey 
          ; if no key is pressed, it holds the value #$40
          cmp #$40
          beq readkx

          ; f1 key
          cmp #$04
          bne readkf3
          jsr tglchrst
          jmp readkx

          ; f3 key
readkf3   cmp #$05
          bne readkf5
          inc bgcolor0 
          jmp readkx

          ; f5
readkf5   cmp #$06
          bne readkf7
          inc bgcolor1 
          jmp readkx

          ; f7
readkf7   cmp #$03
          bne readko
          inc bgcolor2 
          jmp readkx

          ;O
readko    cmp #$26
          bne readkp
          jsr deccurch
          jmp readkx

          ;P
readkp    cmp #$29
          bne readkx
          jsr inccurch
          jmp readkx

readkx    rts

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
          ; print selected character to screen
          lda curchind
          sta $0518

          ; * character data is stored to 'chrdata'
          ; * each character consists of 8 bytes
          ; * character memory offset for selected
          ;   character is curchind * 8 bytes
          ; * tmpblo/-hi points to selected character
          ;   data in the character memory

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
          lda (tmpblo),y      ; load (indirectly) a byte of character data 
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
          ; screen memory ( 40 chars - 8 chars -> add #$20)
          clc
          lda tmpalo
          adc #$28
          sta tmpalo
          lda tmpahi
          adc #$00
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

rendbytm1
          lda atmp
          and #$c0            ; 11000000 check only 2 bytes per iteration 

          cmp #$00
          bne rendbytm2
          ; draw pair of empty space chars
          lda #$20            ; empty space
          jmp rendbytm5

rendbytm2 cmp #$01
          bne rendbytm3
          ; draw pair of bg color #1 marker chars 
          lda curchind ; todo
          jmp rendbytm5

rendbytm3 cmp #$02
          bne rendbytm4
          ; draw pair of bg color #2 marker chars
          lda curchind ; todo
          jmp rendbytm5

rendbytm4 ; cmp #$03 no need to compare any more
          ; draw pair of character color marker chars
          lda curchind ; todo

rendbytm5 
          ; shift atmp left by a bit pair
          rol atmp
          rol atmp
          sta (tmpalo),y      ; store empty or mark to char editor screen mem
          iny
          sta (tmpalo),y
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
          ldx #<chrdata       ; load address (low-byte)
          ldy #>chrdata       ; load address (hi-byte)
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

         lda vicctrlreg
         ora #$10             ; 00010000
         sta vicctrlreg

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
          ; prints the loaded charset
          ; to the start of screen memory

          ldx #$00
          txa
printchs1
          sta scrmemp1,x
          txa
          inx
          bne printchs1
          rts
;------------------------------------
dmpstdch 
          ; dumps standard character set from 
          ; character generator ROM to 
          ; RAM (chrdata) for editing

          ; turn off interrupts
          sei

          ; switch out I/O registers and switch
          ; in character memory to $d000
          lda $01
          and #$fb            ; 11111011
          sta $01

          lda #<chrrom
          sta tmpalo
          lda #>chrrom
          sta tmpahi

          lda #<chrdata
          sta tmpblo
          lda #>chrdata
          sta tmpbhi

          ; character memory is now in $d000-$dfff
          ; dump the character memory
          ldx #$08  ; go through 8 pages of memory
          ldy #$00
dmpstdch1
          lda (tmpalo),y
          sta (tmpblo),y
          iny
          bne dmpstdch1
          ;ldy #$00 ; this is alredy 00

          ; offset a page to tmpalo/-hi
          ; (increase high byte by one)
          inc tmpahi
          inc tmpbhi
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

fnchrset  .text "CHR"

