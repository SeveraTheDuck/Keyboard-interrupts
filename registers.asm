.286
.model tiny
.code
org 100h

;----------------------------------------------------------
X_BEGIN			        equ 20		; frame center on horizontal
Y_BEGIN			        equ 5		; frame center on vertical
FRAME_WIDTH                     equ 20		; frame width
FRAME_HEIGHT                    equ 15		; frame height

PATTERN_STRING_STEP		equ 3		; 3 symbols from pattern are used to draw a line
NUM_OF_LINE_BORDER_SYMBOLS	equ 2		; left and right symbols in line

FRAME_COLOR			equ 4eh		; red background with yellow symbols

VIDEO_MEM_CELL_SIZE		equ WORD	; two bytes: color and symbol
LINE_LENGTH_IN_SYMBOLS		equ 80		; window width in symbols
LINES_NUMBER			equ 25		; window height in symbols

NUM_OF_REGISTERS		equ 13		; number of registers
EACH_REGISTER_STRING_SIZE	equ 5		; size of string like "AX = "
OPEN_FRAME_KEY                  equ 2dh         ; press X to open keyboard
INTERRUPT_ADDRESS_SIZE		equ DWORD	; segment and offset
;----------------------------------------------------------



;----------------------------------------------------------
; Straight forward to main: label
; Used to separate code to save and not to save when stay
; resident and terminate.
;----------------------------------------------------------
Start:		jmp main
;----------------------------------------------------------
;----------------------------------------------------------



;----------------------------------------------------------
; Function replaces standart 8 (timer) interrupt and prints
; register values in the frame from 9 interrupt.
; Enter: Old08Offset, Old08Segment
; Exit:  none
; Destr: none (push / pop EVERYTHING)
;----------------------------------------------------------
New08Interrupt  proc

		cmp cs:frame_is_printed, 1
		jne GoOld08Interrupt

                push ss es ds sp bp di si dx cx bx ax	; push all registers (except IP)

		push cs					; ds = cs for data
		pop ds

		call PrintFrameWithRegs

                pop ax bx cx dx si di bp sp ds es ss		; pop all registers

GoOld08Interrupt:

		;----------------------
		; self-defining code with long jump
		; jmp *offset* *segment*
		;----------------------
		db 0EAh
Old08Offset	dw 0
Old08Segment	dw 0

                endp
;----------------------------------------------------------
;----------------------------------------------------------



;----------------------------------------------------------
;
;
;
;
;----------------------------------------------------------
PrintFrameWithRegs	proc

		mov ax, 0b800h
		mov es, ax			; es = video memory segment

		;----------------------
                ; set args for PrintFrame function
                ;----------------------
                mov ah, FRAME_COLOR
                mov bl, FRAME_WIDTH
                mov bh, FRAME_HEIGHT
                mov si, offset frame_pattern
                mov di, (X_BEGIN + (Y_BEGIN - 1) * LINE_LENGTH_IN_SYMBOLS) * VIDEO_MEM_CELL_SIZE
                cld
		call PrintFrame

		call PrintRegisterNames		; Prints registers' names in the frame

        	mov di, (X_BEGIN + 1 + EACH_REGISTER_STRING_SIZE + Y_BEGIN * LINE_LENGTH_IN_SYMBOLS) * VIDEO_MEM_CELL_SIZE
		mov cx, NUM_OF_REGISTERS			; CX = NUM_OF_REGISTERS

		mov bp, sp					; BP -> stack head
		add bp, WORD + BYTE

PrintRegisterValue:
		push di						; save begin of output

		call PrintHexWordFromStack			; print value

		pop di						; go to next line begin
		add di, LINE_LENGTH_IN_SYMBOLS * VIDEO_MEM_CELL_SIZE
		loop PrintRegisterValue				; loop for num of registers

		ret
		endp

;----------------------------------------------------------
; Function prints value from stack and moves bp
; Enter: AH = print_color, BP = stack_value_offset, DI = output_offset,
;	 ES = video_memory_segment
; Exit:  BP += 1
; Destr: AL, BX
;----------------------------------------------------------
PrintHexWordFromStack	proc

		push cx			; save external loop iterator

		mov cx, WORD		; CX = 2
		mov bx, 0b800h
		mov es, bx		; ES = video memory segment

		lea bx, hex_digitals	; bx -> hex_digitals array

PrintHexSymbol:	mov al, ss:[bp]		; al = ss:[bp], half of register
		shr al, 4		; al /= 16, quarter of register
		xlat			; al = hex symbol equal to al value
		stosw			; print al value

		mov al, ss:[bp]		; same as above
		and al, 0Fh		; al &= 00001111b, quarter of register
		xlat			; same as above
		stosw			; same as above

		sub bp, BYTE		; bp++ for second half of register
		loop PrintHexSymbol

		add bp, DWORD

		pop cx			; get external loop iterator
		ret
		endp
;----------------------------------------------------------
;----------------------------------------------------------



;----------------------------------------------------------
; Function replaces standart 9 (keyboard) interrupt, prints
; a frame and sets standart 8 (timer) interrupt replacement function.
; Enter: Old09Offset, Old09Segment
; Exit:  none
; Destr: none
;----------------------------------------------------------
New09Interrupt  proc

                push ax

                in al, 60h                      ; al = new scan code
                cmp al, OPEN_FRAME_KEY          ; if (al != OPEN_FRAME_KEY)
                jne GoOld09Interrupt            ;       GoOld09Interrupt;

		mov cs:frame_is_printed, 1	; frame print flag to true

GoOld09Interrupt:
                pop ax				; pop used registers

		;----------------------
		; self-defining code with long jump
		; jmp *offset* *segment*
		;----------------------
		db 0EAh
Old09Offset	dw 0
Old09Segment	dw 0

                endp
;----------------------------------------------------------
;----------------------------------------------------------



;----------------------------------------------------------
; PrintFrame prints a frame at given place with given color and symbols
; Enter: AH = FRAME_COLOR,          BL = frame_width,   BH = frame_heigth,
; 	 ES = VIDEO_MEMORY_ADDRESS, SI -> pattern_line, DI = frame_corner, DF = 0
; Exit:  none
; Destr: AX, BX, CX, SI, DI
;----------------------------------------------------------
PrintFrame	proc

		;----------------------
		; PRINT TOP ROW
		; cl = num_of_interior_symbols = frame_width - 2
		;----------------------
		xor cx, cx
		mov cl, bl
		sub cl, NUM_OF_LINE_BORDER_SYMBOLS
		call PrintRow

		;----------------------
		; PRINT MIDDLE ROWS
		; cl = num_of_middle_rows = frame_height - 2
		;----------------------
		xor cx, cx
		mov cl, bh
		sub cl, NUM_OF_LINE_BORDER_SYMBOLS

MiddleRows:	;----------------------
		; save cx for external loop
		; cl = num_of_internal_symbols = frame_width - 2
		;----------------------
		push cx
		xor cx, cx
		mov cl, bl
		sub cl, NUM_OF_LINE_BORDER_SYMBOLS
		call PrintRow

		;----------------------
		; get cx value for external loop
		;----------------------
		pop cx

		;----------------------
		; si -= 3 during the loop
		; for repeating pattern
		;----------------------
		sub si, PATTERN_STRING_STEP

		loop MiddleRows

		;----------------------
		; PRINT BOTTOM ROW
		; cl = num_of_internal_symbols = frame_width - 2
		; si += 3 to move the pattern
		;----------------------
		xor cx, cx
		mov cl, bl
		sub cl, NUM_OF_LINE_BORDER_SYMBOLS
		add si, PATTERN_STRING_STEP
		call PrintRow

		ret
		endp
;----------------------------------------------------------
;----------------------------------------------------------



;----------------------------------------------------------
; PrintRow prints a row of symbols with a given from ds pattern
; Enter: AH = frame_color, CX = num_of_internal_symbols, SI -> pattern_line
; Exit:  none
; Destr: AL, CX, SI, DI
;----------------------------------------------------------
PrintRow	proc

		push di         ; remember start position for quick return

		lodsb           ; read pattern symbol
		stosw           ; print first symbol

		lodsb           ; read pattern symbol
		rep stosw       ; print internal symbols

		lodsb           ; read pattern symbol
		stosw           ; print last symbol

		;----------------------
		; return start position
		; and move to next line
		;----------------------
		pop di
		add di, LINE_LENGTH_IN_SYMBOLS * VIDEO_MEM_CELL_SIZE

		ret
		endp
;----------------------------------------------------------
;----------------------------------------------------------



;----------------------------------------------------------
; Prints registers' names in the frame printed before
; Enter: none
; Exit:  none
; Destr: AX, CX, SI, DI
;----------------------------------------------------------
PrintRegisterNames	proc

		mov ah, FRAME_COLOR
		mov cx, NUM_OF_REGISTERS

		mov si, offset registers_string
		mov di, (X_BEGIN + 1 + Y_BEGIN * LINE_LENGTH_IN_SYMBOLS) * VIDEO_MEM_CELL_SIZE

PrintRegister:	push cx
		push di

		mov cx, EACH_REGISTER_STRING_SIZE
PrintRegisterSymbol:
		lodsb
		stosw
		loop PrintRegisterSymbol

		pop di
		pop cx
		add di, LINE_LENGTH_IN_SYMBOLS * VIDEO_MEM_CELL_SIZE
		loop PrintRegister

		ret
		endp
;----------------------------------------------------------
;----------------------------------------------------------

frame_is_printed	db 0

;----------------------------------------------------------
; String with all hex digitals in place of their integer equivalent
;----------------------------------------------------------
hex_digitals		db "0123456789ABCDEF"

;----------------------------------------------------------
; String with registers' phrases to print in the frame
;----------------------------------------------------------
registers_string 	db "AX = BX = CX = DX = SI = DI = ", \
			   "BP = SP = DS = ES = SS = IP = CS = "

;----------------------------------------------------------
; string with frame pattern
;----------------------------------------------------------
frame_pattern 	db 0c9h, 0cdh, 0bbh, 0bah, ' ', 0bah, 0c8h, 0cdh, 0bch

EndOfFile:



;----------------------------------------------------------
; Function start and end, calls for SetNew09Interrupt and
; terminates program and stays resident with amount of segments in dx.
; dx = (EndOfFile - Start) / 16 + 1
;----------------------------------------------------------
main:		mov al, 08h
		mov cx, offset New08Interrupt
		mov si, offset Old08Offset
		mov di, offset Old08Segment
		call SetNewNInterrupt

		mov al, 09h
		mov cx, offset New09Interrupt
		mov si, offset Old09Offset
		mov di, offset Old09Segment
		call SetNewNInterrupt

                ;----------------------
		; Save code in memory after end of program
		;----------------------
		mov dx, offset EndOfFile
		shr dx, 4
		inc dx

		mov ax, 3100h			; terminate and stay resident
		int 21h
;----------------------------------------------------------
;----------------------------------------------------------



;----------------------------------------------------------
; Function changes N-th interrupt address and saves old one
; in OldNSegment and OldNOffset.
; Enter: AL = interrupt_number, SI -> OldNOffzet, DI -> OldNSegment,
;	 CX -> NewNInterrupt
; Exit:  OldNOffset, OldNSegment
; Destr: AX, BX, ES, IF
;----------------------------------------------------------
SetNewNInterrupt       proc

                ;----------------------
		; We do not want our interrupt to start
		; while we are fixing interrupt address.
		; That is why we set interrupt flag to 0,
		; to avoid interrupts.
		;----------------------
		cli					; IF = 0

		xor bx, bx				; BX = 0
		mov es, bx				; ES = 0

		;----------------------
		; We want to save and use old interrupt
		;----------------------
		mov ah, 35h				; DOS Fn 35h - Get Interrupt Vector
		int 21h					; ES = segment, bx = offset

		mov cs:[si], bx				; get old offset
		mov bx, es				; BX = ES
		mov cs:[di], bx				; get old segment

		;----------------------
		; Now we change interrupt address
		; to our interrupt code segment
		;----------------------
		xor ah, ah
		xor bx, bx				; BX = 0
		mov es, bx				; ES = 0
		mov bx, INTERRUPT_ADDRESS_SIZE		; BX = INTERRUPT_ADDRESS_SIZE, for multiplying

		mul bx
		mov bx, ax
		mov es:[bx], cx				; set offset

		push cs
		pop ax					; AX -> code
		mov es:[bx + WORD], ax			; set segment

		;----------------------
		; Now we can use our interrupt
		; IF = 1
		;----------------------
		sti

                ret
                endp
;----------------------------------------------------------
;----------------------------------------------------------



end		Start
