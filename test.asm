.286
.model tiny
.code
org 100h

Start:          call PrintCSFromStack

TestLoop:       mov ax, 1111h
                mov bx, 2222h
                mov cx, 3333h
                mov dx, 4444h
                mov si, 5555h
                mov di, 6666h
                mov bp, 7777h
                mov es, ax

                in al, 60h
                cmp al, 14h ; T
                jne TestLoop

                mov ax, 4c00h
                int 21h


;----------------------------------------------------------
;
;
;
;
;----------------------------------------------------------
PrintCSFromStack proc

                push cs

                mov ah, 02h
                mov bp, sp
                add bp, BYTE

                mov cx, WORD

                lea bx, hex_digitals	; bx -> hex_digitals array

PrintCSSymbol:	mov al, ss:[bp]		; al = ss:[bp], half of register
		shr al, 4		; al /= 16, quarter of register
		xlat			; al = hex symbol equal to al value
		mov dl, al		; print al value
                int 21h

		mov al, ss:[bp]		; same as above
		and al, 0Fh		; al &= 00001111b, quarter of register
		xlat			; same as above
                mov dl, al
		int 21h			; same as above

		sub bp, BYTE		; bp++ for second half of register
		loop PrintCSSymbol

		add bp, DWORD

		pop cx			; get external loop iterator
		ret
		endp



;----------------------------------------------------------
; String with all hex digitals in place of their integer equivalent
;----------------------------------------------------------
hex_digitals		db "0123456789ABCDEF"

end             Start
