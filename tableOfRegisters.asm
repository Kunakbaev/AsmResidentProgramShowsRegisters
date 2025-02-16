.model tiny

; -----------------------------------------------
;                       DATA
; -----------------------------------------------


.data

ProgrammStartString         db  "Programm has started...", 10, "$"  ; 10 = new line char code
VIDEO_MEMORY_ADDR           equ 0b800h
SCREEN_WIDTH                equ 80
CHAR_STYLE                  equ 4Eh
KEYBOARD_PORT               equ 60h
ESC_ASCII_CODE              equ 1

.code
org 100h

Start:
    ; print ProgramStartString
    mov ah, 09h
    mov dx, offset ProgrammStartString
    int 21h

    mov bx, VIDEO_MEMORY_ADDR
    mov es, bx
    mov di, 5 * SCREEN_WIDTH + 20 * 2
    mov ah, CHAR_STYLE

    readKeyboardTillEsc:
        in  al, KEYBOARD_PORT       ; read char from keyboard
        mov es:[di], ax             ; draw it on screen
        cmp al, ESC_ASCII_CODE      ; if char != ESC, then we continue
        jne readKeyboardTillEsc

    mov ax, 4c00h
    int 21h


end Start


