.286
.model tiny
.code
org 100h

;----------------------------------------------------------
INTERRUPT_ADDRESS_SIZE		equ DWORD		;segment and offset
SYMBOLS_COLOR				equ 4eh			;
VIDEO_MEMORY_ADDRESS		equ 0b800h		;

HIGH_BIT_MASK				equ 80h			; 10000000b
;----------------------------------------------------------

Start:
		;----------------------
		; We do not want our interrupt to start
		; while we are fixing interrupt address.
		; That is why we set interrupt flag to 0,
		; to avoid interrupts.
		;----------------------
		cli										; if = 0

		xor bx, bx								;
		mov es, bx								; es = 0

		mov bx, 09h * INTERRUPT_ADDRESS_SIZE	; we change 9-th interrupt
		mov es:[bx], offset New09Interrupt		; set offset

		push cs
		pop ax									; ax -> code
		mov es:[bx + WORD], ax					; set segment

		;----------------------
		; Now we can use our interrupt
		; IF = 1
		;----------------------
		sti

		mov dx, offset EndOfFile
		shr dx, 4
		inc dx

		mov ax, 3100h							; DOS Fn 3100h, terminate and stay resident
		int 21h

;----------------------------------------------------------
;
;
;
;
;----------------------------------------------------------
New09Interrupt	proc

		push ax bx es

		mov ax, VIDEO_MEMORY_ADDRESS
		mov es, ax

		mov bx, cs:PrintOffset
		mov ah, SYMBOLS_COLOR

		in al, 60h
		mov es:[bx], ax
		add bx, WORD
		mov cs:PrintOffset, bx

		in al, 61h
		or al, HIGH_BIT_MASK				; enable keyboard
		out 61h, al
		and al, not HIGH_BIT_MASK			; disable keyboard
		out 61h, al

		mov al, 20h							; send end of input to interrupt controller
		out 20h, al

		pop es bx ax

		iret
		endp

PrintOffset dw (80d * 5 + 40d) * 2

EndOfFile:

end		Start
