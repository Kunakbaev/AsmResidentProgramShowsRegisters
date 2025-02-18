.model tiny

; -----------------------------------------------
;                       DATA
; -----------------------------------------------


.data

; -------------------------         VARIABLES       ----------------------------------------

isTableOfRegistersOpen                      db 1
ProgrammStartString                         db "Programm has started...", 10, "$"  ; 10 = new line char code
OutInterruptionFuncMesage                   db "I am custom interruption function", 10, "$"


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
TERMINANTE_AND_STAY_RESIDENT_FUNC_CODE      equ 3100h
JMP_COMMAND_CODE                            equ 0eah
TIMER_INT_CODE                              equ 08h
KEYBOARD_INT_CODE                           equ 09h

.code
org 100h

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
    mov ax, TERMINANTE_AND_STAY_RESIDENT_FUNC_CODE
    ; counting size that out program takes
    mov dx, offset EndOfProgram     ; size of our programm in bytes
    shr dx, 4                       ; system func requires size in paragraphs (each paragraph is 16 bytes)
    inc dx                          ; in case if dx has a remainder when we divide it by 16
    int 21h



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
    ; di += REGISTER_HEX_LEN * 2
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
    cmp cs:isTableOfRegistersOpen, 1h
    jne doNotOpenTable
        call drawTableOfRegisters
    doNotOpenTable:

    jmp OldTimerInterruptorFuncAddr

    iret
    endp

drawTableOfRegisters        proc
    push bp si bx di ax cx es

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

    pop es cx ax di bx si bp
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

