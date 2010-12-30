
scrmemp1            = $0400
scrmemp2            = $0500
scrmemp3            = $0600
scrmemp4            = $0700
; store character set to basic memory
chrdata             = $3800 ;... $3fff
vicmemctrlreg       = $d018
vicctrlreg          = $d016

          ; sys 32768
          *= $8000 
; initializing:

          jsr ldchrset 
          jsr initchrset
          jsr clearscr
          jsr printchs

;------------------------------------
mainloop
          rts
          ; jmp mainloop 
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
fnchrset  .text "CHR"

