.model tiny

; -----------------------------------------------
;                       DATA
; -----------------------------------------------
.code

varsStart:

.data

; -------------------------         VARIABLES       ----------------------------------------

isTableOfRegistersOpen                      db 1
ProgrammStartString                         db "Programm has started...", 10, "$"  ; 10 = new line char code

; frame submodule variables

; contains all possible formats for frame
; FIXME: to hex
FrameStyles                 db 201, 205, 187, 186, 32, 186, 200, 205, 188 ; double edge
                            db 218, 196, 191, 179, 32, 179, 192, 196, 217 ; single edge
                            db '123456789'                                ; for debug purposes
                            db '#-#| |#-#'
CurrentFrameStyle           db '#-#| |#-#'
TextMessage                 db "I am so stupid and dumb. That's why I love a dump." ; message that is shown in the middle of table
backgroundColor             db 1 dup(?)                                             ; Is it ok to store just one byte in memory?
; ASK: cringe?
AxRegTableName              db "AX:"
BxRegTableName              db "BX:"
CxRegTableName              db "CX:"
DxRegTableName              db "DX:"



; -------------------------         CONSTS          ----------------------------------------

VIDEO_MEMORY_ADDR                           equ 0b800h
SCREEN_WIDTH                                equ 80
CHAR_STYLE                                  equ 4Eh
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

; frame submodule consts
COMMAND_LINE_MEMORY_ADDR                    equ 80h
TEXT_MESSAGE_COLOR_ATTR                     equ 3Fh
SCREEN_WIDTH                                equ 80
STYLE_STRING_LEN                            equ 9
STYLE_STRING_ONE_ROW_LEN                    equ 3
FRAME_WIDTH                                 equ 20
FRAME_HEIGHT                                equ 10


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
drawFrame   proc
    mov bx, VIDEO_MEMORY_ADDR
    mov es, bx ; set memory segment to video memory
    mov si, offset CurrentFrameStyle



    mov ah, backgroundColor ; set color attribute


    ; draw first line of frame
    ; mov di, 1 * 2 * SCREEN_WIDTH ; move video memory pointer to the 2th line
    mov di, 0
    mov cx, FRAME_WIDTH
    call drawLine
    add di, 2 * SCREEN_WIDTH ; move video memory pointer to the next line
    add si, STYLE_STRING_ONE_ROW_LEN

    mov cx, FRAME_HEIGHT ; hardcoded, frame height
    dec cx ; cx -= 2, height - 2 (because first and last rows are already considered)
    dec cx
    cycleThroughRows:
        push cx ; save cx

        ; lea si, TableFormat + 3
        mov cx, FRAME_WIDTH ; TODO: hardcoded, frame width
        call drawLine
        add di, 2 * SCREEN_WIDTH ; move video memory pointer to the next line

        pop cx ; restore cx
        loop cycleThroughRows

    add si, STYLE_STRING_ONE_ROW_LEN
    ; draw last line of frame
    mov cx, FRAME_WIDTH
    call drawLine

    ret
    endp

; Draws line of code
; Entry: AH = color attribute
;        DS:SI = address of style string sequence
;        ES:DI = address in video memory where to begin drawing line
;        CX = frame width
; Require: DF (direction flag) = 0
; Exit :
; Destr:
drawLine    proc
    push di ; save di
    push si ; save si

                                ; add di, 2 * 10 ; x coord offset
                                ; draws first symbol of row
    lodsb                       ; puts beginning style character to AL
    mov es:[di], ax             ; saves char with color to video memory
    add di, 2                   ; move col position

    lodsb                       ; puts middle style character to AL
    sub cx, 2                   ; number of chars in the middle = width - 2 (first and last characters)
    cycleThroughCols:
        mov es:[di], ax         ; save char with color attr to video memory
        add di, 2               ; move col position
        loop cycleThroughCols

                                ; draws last symbol of row
    lodsb                       ; puts ending style character to AL
    mov es:[di], ax             ; saves char with color to video memory
    add di, 2                   ; move col position

    pop si                      ; restore si (can be changed to -3 = number of lodsb)
    pop di                      ; restore di
    ret
    endp

; Draws text message
; Entry: AH = color attribute
;        DS:SI = address of message string
;        ES:DI = address in video memory where to begin drawing text
;        CX = line length
; Require: DF (direction flag) = 0
; Exit :
; Destr:
drawTextMessage     proc
    push di                 ; save di
    push si                 ; save si

    lea si, TextMessage
    charLoop:
        lodsb               ; load char from message to al

        mov es:[di], ax     ; save char with color attr to video memory
        add di, 2           ; move col position
        loop charLoop

    pop si                  ; restore si
    pop di                  ; restore di
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
    push sp bp si bx di ax cx es ds
    push cs
    pop  ds

    cmp cs:isTableOfRegistersOpen, 1h
    jne doNotOpenTable
        call drawTableOfRegisters
    doNotOpenTable:

    pop ds es cx ax di bx si bp sp
    jmp OldTimerInterruptorFuncAddr

    iret
    endp

drawTableOfRegisters        proc
    ; ASK: pusha, how to? with .286 doesn't work properly



    cld
    mov bx, 1
    call decideFrameStyle
    mov  backgroundColor, 3Fh
    call drawFrame


    mov cx, 10
    push VIDEO_MEMORY_ADDR
    pop  es
    mov  di, SCREEN_WIDTH * 2 * 5 + 5 * 2

    mov si, offset TextMessage
    mov ah, CHAR_STYLE
    call cs:drawTextMessage






    mov ax, VIDEO_MEMORY_ADDR
    mov es, ax
    mov ah, CHAR_STYLE
    mov di, 5 * SCREEN_WIDTH * 2 + 15 * 2

    mov bp, sp
    mov bx, [bp]    ; es
    call numberToHexStr
    add di, 2 * SCREEN_WIDTH
    add di, 2

    mov bp, sp
    mov bx, [bp + 2]    ; cx
    call numberToHexStr
    add di, 2 * SCREEN_WIDTH
    add di, 2

    mov bp, sp
    mov bx, [bp + 4]    ; ax
    call numberToHexStr
    add di, 2 * SCREEN_WIDTH
    add di, 2

    mov bp, sp
    mov bx, [bp + 6]    ; di
    call numberToHexStr
    add di, 2 * SCREEN_WIDTH
    add di, 2

    mov bp, sp
    mov bx, [bp + 8]    ; bx
    call numberToHexStr
    add di, 2 * SCREEN_WIDTH
    add di, 2

    ret
    endp

drawScanCodeOfPressedKey        proc
    push ax di es

    mov ax, VIDEO_MEMORY_ADDR
    mov es, ax
    mov ah, CHAR_STYLE
    mov di, 5 * SCREEN_WIDTH * 2 + 10 * 2
    cld

    in al, KEYBOARD_PORT
    stosw


    cmp al, ACTIVATION_CODE
    jne notActivationCode
        xor cs:isTableOfRegistersOpen, 1h   ; change state of visibility table
        in  al,  61h
        mov ah,  al  ; save previous val
        or  al,  80h ; set highest bit
        out 61h, al
        mov al,  ah  ; restore previous val
        out 61h, al

        mov al, END_OF_INTERRUPT_CODE
        out INTERRUPTION_CONTROLLER_PORT, al
        pop es di ax
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

    pop es di ax
    jmp OldKeyboardInterrupFuncAddr

    iret         ; special return for interruptions, stores not only registers,
                 ; but also flags and command segments
    endp


; openWindow:
;     ACTIVATION_CODE
;
;     iret
;     endp



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

