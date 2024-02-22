.286
.model tiny
.code
org 100h

;----------------------------------------------------------
; Polling example

VIDEO_MEMORY_ADDRESS		equ 0B800h
START_SYMBOL			equ 80d * 5d + 40d	; middle of 6th string
VIDEO_MEMORY_CELL_SIZE		equ 2			; color and symbol
SYMBOLS_COLOR			equ 4Eh			; yellow symbol, red background

KEYBOARD_READ_PORT		equ 60h			; a port to read scan code from
STOP_READING_SCAN_CODE		equ 01h			; esc button stops reading scan codes
TERMINATE_PROGRAM		equ 4C00h		; DOS Fn 4C00h - terminate
;----------------------------------------------------------
Start:		mov ax, VIDEO_MEMORY_ADDRESS
		mov es, ax

		mov bx, START_SYMBOL * VIDEO_MEMORY_CELL_SIZE
		mov ah, SYMBOLS_COLOR
		
NextSymbol:	in al, KEYBOARD_READ_PORT	; al = scan_code
		mov es:[bx], ax			; print symbol with ascii code equal to our scan code

		cmp al, STOP_READING_SCAN_CODE	; if (al == esc) break;
		jne NextSymbol

		mov ax, TERMINATE_PROGRAM
		int 21h

end		Start














