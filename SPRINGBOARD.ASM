.286
.model tiny
.code
org 100h

;----------------------------------------------------------
INTERRUPT_ADDRESS_SIZE		equ DWORD		; segment and offset
SYMBOLS_COLOR			equ 4eh			; color of new symbols
VIDEO_MEMORY_ADDRESS		equ 0b800h		; segment where video memory starts
;----------------------------------------------------------

Start:
		;----------------------
		; We do not want our interrupt to start
		; while we are fixing interrupt address.
		; That is why we set interrupt flag to 0,
		; to avoid interrupts.
		;----------------------
		cli					; if = 0

		xor bx, bx				; bx = 0
		mov es, bx				; es = 0

		;----------------------
		; We want to save and use old interrupt
		;----------------------
		mov ax, 3509h				; DOS Fn 35h - Get Interrupt Vector
		int 21h					; es = segment, bx = offset

		mov Old09Offset, bx			; get old offset
		mov bx, es				; bx = es
		mov Old09Segment, bx			; get old segment

		;----------------------
		; Now we change interrupt address
		; to our interrupt code segment
		;----------------------
		xor bx, bx				; bx = 0
		mov es, bx				; es = 0

		mov bx, 09h * INTERRUPT_ADDRESS_SIZE	; we change 9-th interrupt
		mov es:[bx], offset New09Interrupt	; set offset

		push cs
		pop ax					; ax -> code
		mov es:[bx + WORD], ax			; set segment

		;----------------------
		; Now we can use our interrupt
		; IF = 1
		;----------------------
		sti

		;----------------------
		; Save code in memory after end of program
		;----------------------
		mov dx, offset EndOfFile
		shr dx, 4
		inc dx

		mov ax, 3100h				; DOS Fn 3100h, terminate and stay resident
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

		in al, 60h				; al = pressed_button
		mov es:[bx], ax				; print button scan-code
		add bx, WORD				; bx += 2
		mov cs:PrintOffset, bx			; PrintOffset = bx

GoOld09Interrupt:
		pop es bx ax

		;----------------------
		; self-defining code with long jump
		; jmp *offset* *segment*
		;----------------------
		db 0EAh
Old09Offset	dw 0
Old09Segment	dw 0

		endp

PrintOffset dw (80d * 5 + 40d) * 2

EndOfFile:

end		Start
