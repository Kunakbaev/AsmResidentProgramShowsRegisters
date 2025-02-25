.model tiny
.code

KEYBOARD_PORT                               equ 60h
ESC_ASCII_CODE                              equ 1
EXIT_CODE                                   equ 4c00h

org 100h
Start:
    mov bx, 5555h
    mov es, bx
    ; mov bx,
    ; mov sp, bx
    ; mov bx, acedh
    ; mov ds, bx

    mov ax, 1111h
    mov bx, 2222h
    mov dx, 4444h
    mov si, 6666h
    mov di, 7777h

    mov cx, 3333h
    workTillEsc:
        ; dec cx
        in al, KEYBOARD_PORT
        cmp al, ESC_ASCII_CODE
        jne workTillEsc

    mov ax, EXIT_CODE
    int 21h

end Start
