.model tiny

; -----------------------------------------------
;                       DATA
; -----------------------------------------------


.data

ProgrammStartString                 db  "Programm has started...", 10, "$"  ; 10 = new line char code
OutInterruptionFuncMesage           db  "I am custom interruption function", 10, "$"
VIDEO_MEMORY_ADDR                   equ 0b800h
SCREEN_WIDTH                        equ 80
CHAR_STYLE                          equ 4Eh
KEYBOARD_PORT                       equ 60h
ESC_ASCII_CODE                      equ 1
INTERRUPTION_CONTROLLER_PORT        equ 20h
END_OF_INTERRUPT_CODE               equ 20h ; = EOI

.code
org 100h

Start:
    mov ah, 09h
    mov dx, offset ProgrammStartString
    int 21h

    xor ax, ax ; ax = 0
    mov es, ax ; ???
    mov bx, 09h * 4
    cli     ; processor stops  considering interruptions
            ; (we don' want any interruptions to happen while we chantge table of interruptions)
    ; save to the table of interrutions, offset of our function in current code segment
    mov es:[bx], offset drawScanCodeOfPressedKey    ; set offset to low bits
    mov ax, cs
    mov es:[bx + 2], ax                             ; set code segment to high bits
    sti     ; processor starts considering interruptions

    ; int 09h ; check that our function works

    ; finish program, but it stay's as a resident in memory and continues to work
    mov ax, 3100h   ; what's 3100h?
    ; counting size that out program takes
    mov dx, offset EndOfProgram     ; size in bytes
    shr dx, 4        ; system func requires size in paragraphs (each paragraph is 16 bytes)
    inc dx           ; in case if dx has a remainder when we divide it by 16
    int 21h

    ; mov ax, 4c00h
    ; int 21h

drawScanCodeOfPressedKey        proc
    push ax di es dx

    ; mov ah, 09h
    ; mov dx, offset OutInterruptionFuncMesage
    ; int 21h

    mov ax, VIDEO_MEMORY_ADDR
    mov es, ax
    mov ah, CHAR_STYLE
    mov di, 5 * SCREEN_WIDTH * 2 + 10 * 2
    cld

    in al, KEYBOARD_PORT
    stosw
    in  al,  61h
    mov ah,  al  ; save previous val
    or  al,  80h ; set highest bit
    out 61h, al
    mov al,  ah  ; restore previous val
    out 61h, al

    mov al, END_OF_INTERRUPT_CODE
    out INTERRUPTION_CONTROLLER_PORT, al

    pop dx es di ax

    iret         ; special return for interruptions, stores not only registers,
                 ; but also flags and command segments
    endp

EndOfProgram:

end Start

