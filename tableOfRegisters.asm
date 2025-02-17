.model tiny

; -----------------------------------------------
;                       DATA
; -----------------------------------------------


.data

; -------------------------         VARIABLES       ----------------------------------------

isKeyboardInterruptionActive                db 1
ProgrammStartString                         db "Programm has started...", 10, "$"  ; 10 = new line char code
OutInterruptionFuncMesage                   db "I am custom interruption function", 10, "$"
; OldTimerInterruptorFuncAddr                 label dword
; OldTimerInterrupFuncOffset                  dw 0
; OldTimerInterrupFuncCodeSegment             dw 0


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

.code
org 100h

Start:
    mov ah, 09h
    mov dx, offset ProgrammStartString
    int 21h

    xor ax, ax ; ax = 0
    mov es, ax ; ???
    mov bx, 09h * 4


    mov ax, es:[bx]
    mov OldKeyboardInterrupFuncOffset, ax       ; save offset of old interruption receiver
    mov ax, es:[bx + 2]
    mov OldKeyboardInterrupFuncCodeSegment, ax  ; save code segment of old interruption receiver
;     sti
;

    cli     ; processor stops  considering interruptions
            ; (we don' want any interruptions to happen while we chantge table of interruptions)
    ; save to the table of interrutions, offset of our function in current code segment
    mov es:[bx], offset drawScanCodeOfPressedKey    ; set offset to low bits
    mov ax, cs
    mov es:[bx + 2], ax                             ; set code segment to high bits
    sti     ; processor starts considering interruptions

    ; int 09h ; check that our function works

    ; finish program, but it stay's as a resident in memory and continues to work
    mov ax, TERMINANTE_AND_STAY_RESIDENT_FUNC_CODE   ; what's 3100h?
    ; counting size that out program takes
    mov dx, offset EndOfProgram     ; size in bytes
    shr dx, 4        ; system func requires size in paragraphs (each paragraph is 16 bytes)
    inc dx           ; in case if dx has a remainder when we divide it by 16
    int 21h

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
        and cx, 0Fh ; get last 4 bits of ax
        shr bx, 4  ; divide by 16 (remove last 4 bits of ax)

        cmp cx, 9
        jle decimalDigit
            add cx, 'A' - 10
            jmp digitIfEnd
        decimalDigit:
            add cx, '0'
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

drawScanCodeOfPressedKey        proc
    push ax di es

    mov ax, VIDEO_MEMORY_ADDR
    mov es, ax
    mov ah, CHAR_STYLE
    mov di, 5 * SCREEN_WIDTH * 2 + 10 * 2
    cld

    in al, KEYBOARD_PORT


    cmp al, ACTIVATION_CODE
    jne notActivationCode
        xor cs:isKeyboardInterruptionActive, 1h
        jmp bibaIboba
    notActivationCode:

    cmp cs:isKeyboardInterruptionActive, 1h
    je doBruhMoment
        mov al, END_OF_INTERRUPT_CODE
        out INTERRUPTION_CONTROLLER_PORT, al

        pop es di ax
        jmp OldKeyboardInterrupFuncAddr
    doBruhMoment:
    bibaIboba:



    stosw

    mov di, 5 * SCREEN_WIDTH * 2 + 15 * 2
    push bx
    mov bx, 1234h
    mov ah, CHAR_STYLE
    call numberToHexStr
    pop bx

    in  al,  61h
    mov ah,  al  ; save previous val
    or  al,  80h ; set highest bit
    out 61h, al
    mov al,  ah  ; restore previous val
    out 61h, al

    mov al, END_OF_INTERRUPT_CODE
    out INTERRUPTION_CONTROLLER_PORT, al

    pop es di ax

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
    db 0eah
; code of our program changes, while program is running,
; that's not security safe, so modern systems forbid to do so
OldKeyboardInterrupFuncOffset      dw 0
OldKeyboardInterrupFuncCodeSegment dw 0


EndOfProgram:

end Start

