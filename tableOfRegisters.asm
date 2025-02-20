.model tiny

; -----------------------------------------------
;                       DATA
; -----------------------------------------------
.code

varsStart:

.data

; -------------------------         VARIABLES       ----------------------------------------

isTableOfRegistersOpen                      db 0
ProgrammStartString                         db "Programm has started...", 10, "$"  ; 10 = new line char code

; frame submodule variables

; contains all possible formats for frame
; FIXME: to hex
FrameStyles                 db 201, 205, 187, 186, 32, 186, 200, 205, 188 ; double edge
                            db 218, 196, 191, 179, 32, 179, 192, 196, 217 ; single edge
                            db '123456789'                                ; for debug purposes
                            db '#-#| |#-#'
CurrentFrameStyle           db '#-#| |#-#'
TextMessage                 db "Table of regs" ; message that is shown in the middle of table
backgroundColor             db 1 dup(?)                                             ; Is it ok to store just one byte in memory?
; ASK: cringe?
AxRegTableName              db "AX:"
BxRegTableName              db "BX:"
CxRegTableName              db "CX:"
DxRegTableName              db "DX:"



; -------------------------         CONSTS          ----------------------------------------

VIDEO_MEMORY_ADDR                           equ 0b800h
SCREEN_WIDTH                                equ 80
CHAR_STYLE                                  equ 3Fh
KEYBOARD_PORT                               equ 60h
ESC_ASCII_CODE                              equ 1
INTERRUPTION_CONTROLLER_PORT                equ 20h
END_OF_INTERRUPT_CODE                       equ 20h ; = EOI
ACTIVATION_CODE                             equ 59
REGISTER_HEX_LEN                            equ 4
TERMINATE_AND_STAY_RESIDENT_FUNC_CODE       equ 3100h
JMP_COMMAND_CODE                            equ 0eah
TIMER_INT_CODE                              equ 08h
KEYBOARD_INT_CODE                           equ 09h
SPACING_BETWEEN_REG_NAME_AND_VAL            equ 7
REG_NAME_STRING_LEN                         equ 3
TEXT_MESSAGE_LEN                            equ 13

; frame submodule consts
COMMAND_LINE_MEMORY_ADDR                    equ 80h
TEXT_MESSAGE_COLOR_ATTR                     equ 3Fh
SCREEN_WIDTH                                equ 80
STYLE_STRING_LEN                            equ 9
STYLE_STRING_ONE_ROW_LEN                    equ 3
FRAME_WIDTH                                 equ 20
FRAME_HEIGHT                                equ 10
FRAME_TOP_LEFT_CORN_OFFSET                  equ 0

oldScreenBuffer                             db 2 * FRAME_WIDTH * FRAME_HEIGHT dup(?)
outputImageBuffer                           db 2 * FRAME_WIDTH * FRAME_HEIGHT dup(?)
frameImageBuffer                            db 2 * FRAME_WIDTH * FRAME_HEIGHT dup(?)



.code
org 100h

varsEnd:


































; ---------------------------------         MAIN PROGRAM        ---------------------------------------

Start:
    mov ah, 09h
    mov dx, offset ProgrammStartString
    int 21h



    cli     ; processor stops  considering interruptions
            ; (we don' want any interruptions to happen while we chantge table of interruptions)
    call loadMyKeyboardInterruptorAndSavePrev
    call loadMyTimerInterruptorAndSavePrev
    sti     ; processor starts considering interruptions

    ; finish program, but it stay's as a resident in memory and continues to work
    mov ax, TERMINATE_AND_STAY_RESIDENT_FUNC_CODE
    ; counting size that out program takes
    mov bx, offset varsEnd
    sub bx, offset varsStart
    add bx, 2
    add bx, 2000

    mov dx, offset EndOfProgram     ; size of our programm in bytes
    add dx, bx

    shr dx, 4                       ; system func requires size in paragraphs (each paragraph is 16 bytes)
    inc dx                          ; in case if dx has a remainder when we divide it by 16
    ;add dx, 100
    int 21h














; --------------------------------     DRAWING FRAME       ----------------------------------------------


; Loads chosen frame style to CurrentFrameStyle
; Entry : BX - style index
; Exit  : None
decideFrameStyle        proc
    push cx ; save cx

    mov si, offset FrameStyles
    mov cx, bx
    dec cx
    bruh:   ; ASK:
        add si, STYLE_STRING_LEN
        loop bruh

    push di ; save di
    mov cx, STYLE_STRING_LEN
    push ds
    pop  es
    mov di, offset CurrentFrameStyle
    rep movsb
    pop di cx

    ret
    endp



; Draws frame of table
; Entry: None
; Exit : None
; Destr: si, ax, bx
loadFrame2FrameImageBuffer   proc
    mov si, offset CurrentFrameStyle



    mov ah, backgroundColor ; set color attribute

    ; save first line of frame
    mov cx, FRAME_WIDTH
    call loadFrameRow2FrameImageBuffer
    add si, STYLE_STRING_ONE_ROW_LEN

    mov cx, FRAME_HEIGHT ; hardcoded, frame height
    dec cx ; cx -= 2, height - 2 (because first and last rows are already considered)
    dec cx
    cycleThroughRows:
        push cx ; save cx

        mov cx, FRAME_WIDTH
        call loadFrameRow2FrameImageBuffer

        pop cx ; restore cx
        loop cycleThroughRows

    add si, STYLE_STRING_ONE_ROW_LEN
    ; save last line of frame
    mov cx, FRAME_WIDTH
    call loadFrameRow2FrameImageBuffer

    ret
    endp

; Draws line of code
; Entry: AH = color attribute
;        DS:SI = style string
;        ES:DI = where to save string
; Require: DF (direction flag) = 0
; Exit :
; Destr:
loadFrameRow2FrameImageBuffer    proc
    push si ; save si

                                ; add di, 2 * 10 ; x coord offset
                                ; draws first symbol of row
    lodsb                       ; puts beginning style character to AL
    mov es:[di], ax                ; saves char with color to frame image buffer
    add di, 2                   ; move col position

    lodsb                       ; puts middle style character to AL
    sub cx, 2                   ; number of chars in the middle = width - 2 (first and last characters)
    cycleThroughCols:
        mov es:[di], ax            ; save char with color attr to video memory
        add di, 2               ; move col position
        loop cycleThroughCols

                                ; draws last symbol of row
    lodsb                       ; puts ending style character to AL
    mov es:[di], ax             ; saves char with color to video memory
    add di, 2                   ; move col position

    pop si                      ; restore si (can be changed to -3 = number of lodsb)
    ret
    endp

; Draws text message
; Entry: AH = color attribute
;        DS:SI = address of message string
;        ES:DI = address in frame image buffer where to start drawing message
;        CX = line length
; Require: DF (direction flag) = 0
; Exit :
; Destr:
addTextMessageToFrameImageBuffer     proc
    ; push di                 ; save di
    push si                 ; save si

    charLoop:
        lodsb               ; load char from message to al

        mov es:[di], ax     ; save char with color attr to frame image buffer
        add di, 2           ; move col position
        loop charLoop

    pop si                  ; restore si
    ; pop di                  ; restore di
    ret
    endp

















; ---------------------------------         NEW INTERRUPTION FUNCS        ------------------------------------

loadMyKeyboardInterruptorAndSavePrev       proc
    xor ax, ax ; ax = 0
    mov es, ax
    mov bx, KEYBOARD_INT_CODE * 4     ; keyboard interruption code

    mov ax, es:[bx]
    mov OldKeyboardInterrupFuncOffset, ax       ; save offset of old interruption receiver
    mov ax, es:[bx + 2]
    mov OldKeyboardInterrupFuncCodeSegment, ax  ; save code segment of old interruption receiver

    ; save to the table of interrutions, offset of our function in current code segment
    mov es:[bx], offset drawScanCodeOfPressedKey    ; set offset to low bits
    mov ax, cs
    mov es:[bx + 2], ax                             ; set code segment to high bits

    ret
    endp

loadMyTimerInterruptorAndSavePrev       proc
    xor ax, ax ; ax = 0
    mov es, ax
    mov bx, TIMER_INT_CODE * 4     ; save timer interruption code

    mov ax, es:[bx]
    mov OldTimerInterrupFuncOffset, ax       ; save offset of old interruption receiver
    mov ax, es:[bx + 2]
    mov OldTimerInterrupFuncCodeSegment, ax  ; save code segment of old interruption receiver

    ; save to the table of interrutions, offset of our function in current code segment
    mov es:[bx], offset myTimerInterruptionFunc     ; set offset to low bits
    mov ax, cs
    mov es:[bx + 2], ax                             ; set code segment to high byte

    ret
    endp



; Takes integer value from bx, transforms it to hex and draws on screen
; Entry  : BX - number to output
;          AH - color of symbols
;          ES:DI - where  to output in memory
; Exit   :
; Destr  : CX, BX, DI
numberToHexStr      proc
    push cx ; save cx
    add di, REGISTER_HEX_LEN ; we need to reverse output
    add di, REGISTER_HEX_LEN
    sub di, 2

    mov cx, REGISTER_HEX_LEN
    hexCycle:
        push cx

        mov cx, bx
        and cx, 0Fh ; get only last 4 bits of ax
        shr bx, 4   ; divide by 16 (remove last 4 bits of ax)

        cmp cx, 9
        jle decimalDigit
            add cx, 'A' - 10    ; digit >= 10, we transform it to hex char
            jmp digitIfEnd
        decimalDigit:
            add cx, '0'         ; digit <  10
        digitIfEnd:

        and al, 0  ; clear AL
        or  ax, cx ; save char
        mov es:[di], ax
        sub di, 2

        pop cx
        loop hexCycle

    pop cx ; restore cx
    ret
    endp

myTimerInterruptionFunc     proc
    push ax bx cx dx si di sp bp es ds
    push cs
    pop  ds

    cmp cs:isTableOfRegistersOpen, 1h
    jne doNotOpenTable
        call drawTableOfRegisters
    doNotOpenTable:

    pop  ds es bp sp di si dx cx bx ax
    jmp OldTimerInterruptorFuncAddr

    iret
    endp




saveOldScreen       proc
    push ds ; save ds
    push VIDEO_MEMORY_ADDR ds   ; source      segment register (ds) = VIDEO_MEMORY_ADDR 
    pop  es ds                  ; destination segment register (es) = ds (data segment)

    mov si, FRAME_TOP_LEFT_CORN_OFFSET
    mov di, offset cs:oldScreenBuffer
    mov cx, FRAME_WIDTH * FRAME_HEIGHT
    mov bx, cx
    sub bx, FRAME_WIDTH     ; when is next time when we go to the next row
    oldScreenPixelsLoop:
        cmp bx, cx
        jne stillOnSameRow
            sub bx, FRAME_WIDTH
            add si, 2 * SCREEN_WIDTH ; move to the next screen line
            sub si, 2 * FRAME_WIDTH
        stillOnSameRow:

        movsw   ; moves one word from ds:si to es:di
        loop oldScreenPixelsLoop

    pop ds
    endp
    ret

; loads words (High = color, Low = char) from buffer (FRAME_WIDTH x FRAME_HEIGHT)
; to screen (video memory) from address FRAME_TOP_LEFT_CORN_OFFSET
; Entry : SI = offset (memory address) of buffer
; Exit  :
; Destr : 
loadImageToScreenFromBuffer     proc
    push VIDEO_MEMORY_ADDR  ; source      segment register (es) = ds (data segment)
    pop  es                 ; destination segment register (es) = VIDEO_MEMORY_ADDR 

    mov di, FRAME_TOP_LEFT_CORN_OFFSET
    mov cx, FRAME_WIDTH * FRAME_HEIGHT
    mov bx, cx
    sub bx, FRAME_WIDTH     ; when is next time when we go to the next row
    newScreenPixelsLoop:
        cmp bx, cx
        jne stillOnSameRowNewScreen
            sub bx, FRAME_WIDTH
            add di, 2 * SCREEN_WIDTH ; move to the next screen line
            sub di, 2 * FRAME_WIDTH
        stillOnSameRowNewScreen:

        movsw
        loop newScreenPixelsLoop

    endp
    ret

restoreOldScreen       proc
    mov si, offset oldScreenBuffer
    call loadImageToScreenFromBuffer

    endp
    ret

prepareFrameImageBuffer     proc
    mov bx, 1
    call decideFrameStyle
    mov  backgroundColor, 3Fh
    push ds
    pop  es
    mov  di, offset frameImageBuffer
    call loadFrame2FrameImageBuffer

    mov  si, offset TextMessage
    mov  ah, CHAR_STYLE
    push cs
    pop  es
    mov  di, offset frameImageBuffer
    add  di, 2 * (2 * FRAME_WIDTH + 3)
    mov  cx, TEXT_MESSAGE_LEN
    call cs:addTextMessageToFrameImageBuffer

    ret
    endp

drawFrameImageBuffer        proc
    mov si, offset frameImageBuffer
    call loadImageToScreenFromBuffer

    ret
    endp

; updates oldScreenBuffer, loads new screen bytes to it, where our image was overwritten
; i.e. screen word != frameImageBuffer word
; es:[di] - frameImageBuffer, ds:[si] - video memory
; Entry : DI = offset (memory address) of frameImageBuffere
; Exit  :
; Destr : 
updateOldScreenBuffer       proc
    push ds                     ; save ds
    push ds VIDEO_MEMORY_ADDR   ; source      segment register (ds) = VIDEO_MEMORY_ADDR
    pop  ds es                  ; destination segment register (es) = data segment (ds)

    mov si, FRAME_TOP_LEFT_CORN_OFFSET
    mov di, offset cs:frameImageBuffer
    mov cx, FRAME_WIDTH * FRAME_HEIGHT
    mov bx, cx
    sub bx, FRAME_WIDTH     ; when is next time when we go to the next row
    oldScreenUpdPixelsLoop:
        cmp bx, cx
        jne stillOnSameRowOldScreenUpd
            sub bx, FRAME_WIDTH
            add si, 2 * SCREEN_WIDTH ; move to the next screen line
            sub si, 2 * FRAME_WIDTH
        stillOnSameRowOldScreenUpd:

        cmpsw
        je noUpdateNeeded
            push di ; save frameImageBuffer
            sub si, 2

            push bx
            mov bx, di
            sub bx, offset cs:frameImageBuffer
            sub bx, 2
            mov di, offset cs:oldScreenBuffer
            add di, bx

            movsw
            pop bx
            pop di ; restore frameImageBuffer
        noUpdateNeeded:

        loop oldScreenUpdPixelsLoop

    pop ds ; restore ds
    ret
    endp

; Entry : BX - stack offset of word in stack
;         DS:SI - message before reg value (all have same len = 3)
;         ES:DI - where to drawing output in video memory
; Exit  :
; Destr :
drawOneRegInfo      proc
    push di ; save di

    push bx
    mov  ah, CHAR_STYLE
    push cs
    pop  es
    mov  cx, REG_NAME_STRING_LEN
    call addTextMessageToFrameImageBuffer
    add  di, SPACING_BETWEEN_REG_NAME_AND_VAL * 2
    pop  bx

    mov bp, sp
    add bp, bx
    mov bx, [bp]    ; ax
    call numberToHexStr
    add di, 2 * FRAME_WIDTH
    add di, 2

    pop di ; restore di
    add di, 2 * FRAME_WIDTH ; go the next line

    ret
    endp

addRegsInfoToFrameImageBuffer       proc
    push cs
    pop  es
    mov ah, CHAR_STYLE
    mov di, offset cs:frameImageBuffer
    add di, (4 * FRAME_WIDTH + 3) * 2

    mov si, offset AxRegTableName
    mov bx, 26
    call drawOneRegInfo

    mov si, offset BxRegTableName
    mov bx, 24
    call drawOneRegInfo

    mov si, offset CxRegTableName
    mov bx, 22
    call drawOneRegInfo

    mov si, offset DxRegTableName
    mov bx, 20
    call drawOneRegInfo

    ret
    endp

drawTableOfRegisters        proc
    ; ASK: pusha, how to? with .286 doesn't work properly

    ; call saveOldScreen

    ; FIXME: frameImageBuffer is almost constant
    ; there is no need to call loadFrame func every time
    ; because frame position is constant, only text changes
    ; so there's need to only change those position in buffer, where text lies
    cld
    call updateOldScreenBuffer
    call prepareFrameImageBuffer
    call addRegsInfoToFrameImageBuffer
    call drawFrameImageBuffer


    ret
    endp

drawScanCodeOfPressedKey        proc
    push ax di es ds si bx cx
    push cs
    pop  ds

    ; mov ax, VIDEO_MEMORY_ADDR
    ; mov es, ax
    ; mov ah, CHAR_STYLE
    ; mov di, 5 * SCREEN_WIDTH * 2 + 10 * 2
    cld

    in al, KEYBOARD_PORT
    ; stosw


    cli
    cmp al, ACTIVATION_CODE
    jne notActivationCode
        cmp cs:isTableOfRegistersOpen, 1h
        jne openTableOfRegisters
            call restoreOldScreen
            jmp endOpenDecisionIf
        openTableOfRegisters:
            call saveOldScreen
        endOpenDecisionIf:

        xor cs:isTableOfRegistersOpen, 1h   ; change state of visibility table
        in  al,  61h
        mov ah,  al  ; save previous val
        or  al,  80h ; set highest bit
        out 61h, al
        mov al,  ah  ; restore previous val
        out 61h, al

        mov al, END_OF_INTERRUPT_CODE
        out INTERRUPTION_CONTROLLER_PORT, al

        pop cx bx si ds es di ax
        iret

    notActivationCode:


    in  al,  61h
    mov ah,  al  ; save previous val
    or  al,  80h ; set highest bit
    out 61h, al
    mov al,  ah  ; restore previous val
    out 61h, al

    mov al, END_OF_INTERRUPT_CODE
    out INTERRUPTION_CONTROLLER_PORT, al

    pop cx bx si ds es di ax
    jmp OldKeyboardInterrupFuncAddr

    iret         ; special return for interruptions, stores not only registers,
                 ; but also flags and command segments
    endp



; ; far jump
OldKeyboardInterrupFuncAddr:
    db JMP_COMMAND_CODE
; code of our program changes, while program is running,
; that's not security safe, so modern systems forbid to do so
OldKeyboardInterrupFuncOffset      dw 0
OldKeyboardInterrupFuncCodeSegment dw 0

OldTimerInterruptorFuncAddr:
    db JMP_COMMAND_CODE
OldTimerInterrupFuncOffset                  dw 0
OldTimerInterrupFuncCodeSegment             dw 0


EndOfProgram:

end Start

