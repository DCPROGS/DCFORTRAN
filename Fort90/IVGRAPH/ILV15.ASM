;UNIVERSITY COLEGE LONDON
;PHARMACOLOGY DEPARTMENT
;IOANA LUISA VAIS
;MODULE ILV15.ASM
;MODIFIED:09.11.1993
;MODIFIED:16.12.1993

;*************************************************************************
;									 *
;THIS MODULES CONTAINS SUBROUTINES USING BIOS INTERRUPTS                 *
;                                                                        *
;	1. MODE (MODE)                                                   *
;	-set the alphanumeric/graphic mode                               *
;									 *
;	2. SET_CURSOR (X,Y)                                              *
;	-set the cursor at (x,y) position                                *
;	  X = 0-79 ; Y = 0-29                                            *
;									 *
;	3. GET_CURSOR (X,Y)                                              *
;	-get the cursor at (x,y) position                                *
;	  X = 0-79 ; Y = 0-29                                            *
;									 *
;	4. SET_CHARACTER (CHAR,COLOR)                                    *
;       -output character at cursor position                             *
;									 *
;	5. GET_CHARACTER (CHAR,COLOR)                                    *
;       -get character at cursor position                                *
;									 *
;	6. SET_STRING (STRING,COLOR)                                     *
;	-set string at cursor_position                                   *
;								         *
;	7. DET_STRING (STRING,COLOR,OUTCHAR)                             *
;	-set string at cursor position; returns first character          *
;									 *
;	8. PUT_STRING(STRING,COLOR,COLOR_UP,CHAR)                        *
;	-set string at cursor position; returns first character;change   *
;	 color upper cases                                               *
;									 *
;	9. WRITE_STRING(STRING,COLOR,COLOR_UP)                           *
;	-set string at cursor position; change color first 2 characters  *
;	 ( only 11 characters )                                          *
;									 *
;	10.OUT_TEXT (X,Y,STRING,COLOR)                                   *
;	-set string at (x,y) position                                    *
;	  X = 0-79 ; Y = 0-29                                            *
;									 *
;	11.WRCHARS (STRING,COLOR)                                        *
;	-set string without erasing background                           *
;									 *
;	12.WRSTRING (X,Y,STRING,COLOR,BACKCOLOR)                         *
;	-set string without erasing background at (x,y) position         *
;	  X = 0-79 ; Y = 0-29                                            *
;                                                                        *
;	13.SETCUR18(X,Y)                                                 *
;	-move cursor at position (x,y) in mode 18                        *
;                                                                        *
;	14.SETCHAR18(CH,COLOR,BKCOLOR)                                   *
;	-write character at cursor position in color in mode 18          *
;                                                                        *
;	13.WRSTRING18 (X,Y,STRING,COLOR,BACKCOLOR)                       *
;	-set string without erasing background at (x,y) position         *
;	  X = 0-639 ; Y = 0-479                                          *
;                                                                        *
;	14.ISETSTRING(X,Y,STRING,COLOR,COLOR_UP,BACKCOLOR,CHAROUT)       *
;	-set string without erasing background at (x,y) position         *
;	  X = 0-639 ; Y = 0-479                                          *
;                                                                        *
;       15.RESET_MOUSE(STATUS,BUTTONS_NUMBER)                            *
;	-initialize the mouse driver and return the driver status and    *
;	 the number of mouse buttons                                     *
;                                                                        *
;	16.SHOW_MOUSE()                                                  *
;	-displays the mouse                                              *
;                                                                        *
;	17.HIDE_MOUSE()                                                  *
;	-removes the mouse                                               *
;                                                                        *
;	18.GET_MOUSE(STATUS,X,Y)                                         *
;	-returns the current button status and pointer position          *
;	  X = 0-639 ; Y = 0-479                                          *
;                                                                        *
;	19.MOUSE_UPDATE(STATUS,X,Y)                                      *
;	-returns the current button status and pointer position          *
;	  X = 0-639 ; Y = 0-479                                          *
;                                                                        *
;	20.GET_BUTTON(STATUS,IDENTIFIER,COUNTER,X,Y)                     *
;	-returns the current button status and pointer position          *
;	  X = 0-639 ; Y = 0-479                                          *
;                                                                        *
;	21.SET_MOUSE(X,Y)                                                *
;	-set mouse at (x,y)                                              *
;	  X = 0-639 ; Y = 0-479                                          *
;                                                                        *
;	22.SET_BLINK(CHOICE)                                             *
;       -togle blink/intensity                                           *
;                                                                        *
;       23.SET_PALREG(PALLETE_REGISTER,COLOR)                            *
;	-sets the correspondence of a palette register to a displayable  *
;	 color                                                           *
;                                                                        *
;       24.GET_PALREG(PALLETE_REGISTER,COLOR)                            *
;	returns the color associated with the specified palette register *
;                                                                        *
;       25.SET_RGB(COLOR_REGISTER,RED,GREEN,BLUE)                        *
;	-programs an individual color register with a red-green-blue     *
;	 combination(RGB)                                                *
;                                                                        *
;       26.GET_RGB(COLOR_REGISTER,RED,GREEN,BLUE)                        *
;       -returns the contents of a color register as its red,green,and   *
;	 blue components;                                                *
;                                                                        *
;*************************************************************************

include macros.pl

;======================================================================

	start_data

integer_1	color_letters
integer_1	color_up        ;color upper cases
integer_2	xcoord
integer_2	ycoord

	end_data

;======================================================================
	start_code              	;start the code
;======================================================================

;***************************************************************************
;***************************************************************************

	;subroutine to be called from the main program
	;call MODE(mode)

;======================================================================
	subroutine	MODE,STDFRAME,1
;=======================================================================
	save_regs
	arg_ptr	1, ebx			; point to arg1 :mode
	mov ah,00h
	mov al,byte ptr [ebx]
	int 10h
	restore_regs
	return
	end_subroutine

;***************************************************************************
;***************************************************************************
	;subroutine to be called from the main program
	;call SET_CURSOR(X,Y)
	;move cursor at position (x,y) in alphanumeric mode
;======================================================================
	subroutine	SET_CURSOR,STDFRAME,2
;=======================================================================
	save_regs
	mov 	ah,02h
	mov	bh,00h
	arg_ptr	1, ecx			; point to arg1 x
	mov 	dl,byte ptr [ecx]
	arg_ptr	2, edi			; point to arg2 y
	mov 	dh,byte ptr [edi]
	int 10h
	restore_regs
	return
	end_subroutine

;***************************************************************************
;***************************************************************************
	;subroutine to be called from the main program
	;call GET_CURSOR(X,Y)
	;get the cursor position in alphanumeric mode
;======================================================================
	subroutine	GET_CURSOR,STDFRAME,2
;=======================================================================
	save_regs
	mov 	ah,03h
	mov	bh,00h
	arg_ptr	1, ecx			; point to arg1 x
	arg_ptr	2, edi			; point to arg1 y
	mov 	byte ptr [ecx],dh
	mov 	byte ptr [edi],dl
	int 10h
	restore_regs
	return
	end_subroutine

;***************************************************************************
;***************************************************************************
	;subroutine to be called from the main program
	;call SET_CHARACTER(CH,COLOR)
	;write character at cursor position in color in alphanumeric mode
;======================================================================
	subroutine	SET_CHARACTER,STDFRAME,2
;=======================================================================
	save_regs
	mov 	ah,09h
	mov	bh,00h
	arg_ptr	1,ecx			; character
     	mov 	al,byte ptr [ecx]
	arg_ptr	2, edi			; color
	mov 	bl,byte ptr [edi]
	mov	cx,1
	int 10h
	restore_regs
	return
	end_subroutine

;***************************************************************************
;***************************************************************************
	;subroutine to be called from the main program
	;call GET_CHARACTER(CH,COLOR)
	;get the character at cursor position in alphanumeric mode
;======================================================================
	subroutine	GET_CHARACTER,STDFRAME,2
;=======================================================================
	save_regs
	mov 	ah,08h
	mov	bh,00h
	arg_ptr	1, ecx			; character
	arg_ptr	2, edi			; color
	mov 	byte ptr [ecx],al
	mov 	byte ptr [edi],ah
	int 10h
	restore_regs
	return
	end_subroutine

;***************************************************************************
;***************************************************************************

	;subroutine to be called from the main program
	;call SET_STRING(STRING,COLOR)
	;write string at cursor position in color in alphanumeric mode

;======================================================================
	subroutine	SET_STRING,STDFRAME,2
;=======================================================================
	save_regs
	mov	bh,00
	arg_ptr	2, edi			;color
	mov 	bl,byte ptr [edi]
	char_arg_ptr	1,1,esi,ecx       ;string in esi
					;length in ecx
chloop:
	lodsb
	mov	ah,0eh
	int	10h
	loop	chloop

	restore_regs
	return
	end_subroutine

;***************************************************************************
;***************************************************************************
	;subroutine to be called from the main program
	;call DET_STRING(STRING,COLOR,CHAR)
	;write string at cursor position in color in alphanumeric mode
	;detect upper case letters and change their color to color=color-4
	;return the letter detected, but in lower case
;======================================================================
	subroutine	DET_STRING,STDFRAME,3
;=======================================================================
	save_regs
	push	esi
	arg_ptr	2, edi			;color
	mov 	bl,byte ptr [edi]
	mov	color_letters,bl
	char_arg_ptr	1,1,esi,ecx       ;string in esi
					;length in ecx
next:
	lodsb
	cmp	al,'A'                  ;look for upper case characters
	jb	short cycle
	cmp	al,'Z'
	ja	short cycle
	sub	bl,4                    ;if upper case ,change color
	mov	dl,al                   ;store it in dl

cycle:
	mov	ah,0eh
	int	10h
	mov	bl,color_letters
	loop	next

	mov	al,dl
	or	al,20h                  ;make it lower case,because with
					;getch() you can have some trouble
					;with upper case
     	char_arg_ptr 3,2,esi,ecx
     	mov	[esi],al                ;return it

	pop	esi
	restore_regs
	return
	end_subroutine

;***************************************************************************
;***************************************************************************
	;subroutine to be called from the main program
	;call PUT_STRING(STRING,COLOR1,COLOR2,CHAR)
	;write string at cursor position in color1 in alphanumeric mode
	;detect upper case letters and change their color
	;return the letter detected, but in lower case
;======================================================================
	subroutine	PUT_STRING,STDFRAME,4
;=======================================================================

	save_regs
	push	esi
	arg_ptr	2, edi			;color
	mov 	bl,byte ptr [edi]
	mov	color_letters,bl
	arg_ptr	3, edi			;color
	mov 	bl,byte ptr [edi]
	mov	color_up,bl
     	char_arg_ptr 1,1,esi,ecx
					;length in ecx
next1:
	lodsb
	cmp	al,'A'                  ;look for upper case characters
	jb	short cyc1
	cmp	al,'Z'
	ja	short cyc1
	mov	bl,color_up   ;if upper case ,change color
	mov	dl,al                   ;store it in dl

cyc1:
	mov	ah,0eh
	int	10h
	mov	bl,color_letters
	loop	next1

	mov	al,dl
	or	al,20h                  ;make it lower case,because with
					;getch() you can have some trouble
					;with upper case
     	char_arg_ptr 4,2,esi,ecx
     	mov	[esi],al                ;return it

	pop	esi
	restore_regs
	return
	end_subroutine

;***************************************************************************
;***************************************************************************
	;subroutine to be called from the main program
	;call write_STRING(STRING,COLOR1,COLOR2)
	;only for length string = 11
	;write string at cursor position in color1 in alphanumeric mode
	;change the color of first two to color2
;======================================================================
	subroutine	write_STRING,STDFRAME,3
;=======================================================================

	save_regs
	push	esi
	arg_ptr	2, edi			;color
	mov 	bl,byte ptr [edi]
	mov	color_letters,bl
	arg_ptr	3, edi			;color
	mov 	bl,byte ptr [edi]
	mov	color_up,bl

     	char_arg_ptr 1,1,esi,ecx
					;length in ecx
nexte1:
	lodsb
	cmp	cl,9                  ;look for upper case characters
	jg	short cyce1
	mov	bl,color_letters      ;if upper case ,change color

cyce1:
	mov	ah,0eh
	int	10h
	mov	bl,color_up
	loop	nexte1

	pop	esi
	lodsb
	mov	ah,0eh
	int	10h
	loop	nexte1

	pop	esi
	restore_regs
	return
	end_subroutine

;***************************************************************************
;***************************************************************************
	;subroutine to be called from the main program
	;call OUT_TEXT(X,Y,STRING,COLOR)
	;write string at USER position in color in alphanumeric mode
;======================================================================
	subroutine	OUT_TEXT,STDFRAME,4
;=======================================================================
	save_regs
	mov 	ah,02h
	mov	bh,00h
	arg_ptr	1, ecx			; point to arg1 x
	mov 	dl,byte ptr [ecx]
	arg_ptr	2, edi			; point to arg2 y
	mov 	dh,byte ptr [edi]
	int 10h

	arg_ptr	4, edi			;color
	mov 	bl,byte ptr [edi]

	char_arg_ptr	3,1,esi,ecx       ;string in esi,length in ecx
nexte2:
	lodsb
	mov	ah,0eh
	int	10h
	loop	nexte2
	restore_regs
	return
	end_subroutine

;***************************************************************************
;***************************************************************************

	;subroutine to be called from the main program
	;call wrchars(STRING,COLOR)
	;write string at cursor position in color in alphanumeric mode
	;80 X 30
	;WITHOUT ERASING THE BACKGROUND
	;BACKGROUND = 0 -> RESULTING COLOR = COLOR
	;BACKGROUND <>0 -> RESULTING COLOR = COLOR xor BACKGROUND
;======================================================================
	subroutine	wrchars,STDFRAME,2
;=======================================================================

	save_regs
	mov	al,8
	arg_ptr	2, edi			;color
	mov 	bl,byte ptr [edi]
	and	bl,0fh
	mov	cx,04
	shl	al,cl
	and	al,0f0h
	or	bl,al

	char_arg_ptr	1,1,esi,ecx       ;string in esi
					;length in ecx
chloop1:
	lodsb
	mov	ah,0eh
	int	10h
	loop	chloop1

	restore_regs

	return
	end_subroutine

;***************************************************************************
;***************************************************************************
	;subroutine to be called from the main program
	;call wrstring(x,y,STRING,COLOR,backCOLOR)
	;write string at cursor position in color in alphanumeric mode
	;WITHOUT ERASING THE BACKGROUND
	;COMBINE SET_CURSOR+WRCHARS
	;X,Y : CURSOR COORDINATES ( 80 X 30 )
	;BACKCOLOR <=> 0 -> RESULTING COLOR = COLOR
	;IF OVERLAPP ON OTHER BACKGROUNDS
	;		-> RESULTING COLOR = COLOR xor BACKCOLOR xor BACKGROUND
;======================================================================
	subroutine	wrstring,STDFRAME,5
;=======================================================================

	save_regs
	mov 	ah,02h
	mov	bh,00h
	arg_ptr	1, ecx			; point to arg1 x
	mov 	dl,byte ptr [ecx]
	arg_ptr	2, edi			; point to arg2 y
	mov 	dh,byte ptr [edi]
	int 10h
	arg_ptr	5, edi			;bkcolor
	mov 	al,byte ptr [edi]
	arg_ptr	4, edi			;color
	mov 	bl,byte ptr [edi]
	and	bl,0fh
	xor	bl,al
	mov	al,8
	mov	cx,04
	shl	al,cl
	and	al,0f0h
	or	bl,al

	char_arg_ptr	3,1,esi,ecx       ;string in esi
					;length in ecx
chlp1:
	lodsb
	mov	ah,0eh
	int	10h
	loop	chlp1

	restore_regs

	return
	end_subroutine


;***************************************************************************
;***************************************************************************

	;subroutine to be called from the main program
	;call SETCUR18(X,Y)
	;move cursor at position (x,y) in mode 18
;======================================================================
	subroutine	SETCUR18,STDFRAME,2
;=======================================================================
	save_regs
	arg_ptr	1, edi			; point to arg1 x
	mov 	cx,word ptr [edi]
	mov	xcoord,cx
	arg_ptr	2, edi			; point to arg2 y
	mov 	ax,word ptr [edi]
	mov	ycoord,ax

	;do the mapping for x : (x-xmodulo8)/8
	xor	ebx,ebx
	xor	edx,edx
	mov	ax,cx
	mov	bx,08h
	div	bx
	mov	ax,xcoord
	sub	ax,dx
	shr	ax,1
	shr	ax,1
	shr	ax,1
	mov	cl,al

	;do the mapping for y : 29-(y-ymodulo8)/16
	mov	ax,ycoord
	mov	bx,8
	div	bx
	mov	ax,ycoord
	sub	ax,dx
	shr	ax,1
	shr	ax,1
	shr	ax,1
	shr	ax,1
	sub	ax,29
	neg	ax

	mov 	ah,02h
	mov	bh,00h
	mov 	dl,cl
	mov 	dh,al
	int 10h
	restore_regs
	return
	end_subroutine

;***************************************************************************
;***************************************************************************
	;subroutine to be called from the main program
	;call SETCHAR18(CH,COLOR,BKCOLOR)
	;write character at cursor position in color in mode 18
;======================================================================
	subroutine	SETCHAR18,STDFRAME,3
;=======================================================================
	save_regs
	mov	bh,00h

	arg_ptr	3, edi			;bkcolor
	mov 	al,byte ptr [edi]
	arg_ptr	2, edi			;color
	mov 	bl,byte ptr [edi]
	and	bl,0fh
	xor	bl,al
	mov	al,8
	mov	cx,04
	shl	al,cl
	and	al,0f0h
	or	bl,al

	char_arg_ptr	1,1,esi,ecx			; character
	lodsb
	mov 	ah,0eh
	int 10h
	restore_regs
	return
	end_subroutine

;***************************************************************************
;***************************************************************************
	;subroutine to be called from the main program
	;call wrstring18(x,y,STRING,COLOR,backcolor)
	;write string at cursor position in color in mode 18
	;WITHOUT ERASING THE BACKGROUND
	;COMBINE SET_CURSOR+WRCHARS
	;mode = 18   -> X,Y : CURSOR COORDINATES ( 640 X 480 )
	;DOES THR MAPPING TO 80 x 30
	;BACKCOLOR <=> 0 -> RESULTING COLOR = COLOR
	;IF OVERLAPP ON OTHER BACKGROUNDS
	;		-> RESULTING COLOR = COLOR xor BACKCOLOR xor BACKGROUND
;======================================================================
	subroutine	wrstring18,STDFRAME,5
;=======================================================================

	save_regs
	arg_ptr	1, edi			; point to arg1 x
	mov 	cx,word ptr [edi]
	mov	xcoord,cx
	arg_ptr	2, edi			; point to arg2 y
	mov 	ax,word ptr [edi]
	mov	ycoord,ax

	;do the mapping for x : (x-xmodulo8)/8
	xor	ebx,ebx
	xor	edx,edx
	mov	ax,cx
	mov	bx,08h
	div	bx
	mov	ax,xcoord
	sub	ax,dx
	shr	ax,1
	shr	ax,1
	shr	ax,1
	mov	cl,al

	;do the mapping for y : 29-(y-ymodulo8)/16
	mov	ax,ycoord
	mov	bx,8
	div	bx
	mov	ax,ycoord
	sub	ax,dx
	shr	ax,1
	shr	ax,1
	shr	ax,1
	shr	ax,1
	sub	ax,29
	neg	ax

	mov 	ah,02h
	mov	bh,00h
	mov 	dl,cl
	mov 	dh,al
	int 10h
	arg_ptr	5, edi			;bkcolor
	mov 	al,byte ptr [edi]
	arg_ptr	4, edi			;color
	mov 	bl,byte ptr [edi]
	and	bl,0fh
	xor	bl,al
	mov	al,8
	mov	cx,04
	shl	al,cl
	and	al,0f0h
	or	bl,al

	char_arg_ptr	3,1,esi,ecx       ;string in esi
					;length in ecx
chlp12:
	lodsb
	mov	ah,0eh
	int	10h
	loop	chlp12

	restore_regs

	return
	end_subroutine

;***************************************************************************
;***************************************************************************
	;subroutine to be called from the main program
	;call isetstring(x,y,string,color,color_uppercase,backcolor,charout)
	;write string at cursor position in color in mode 18
	;returns first character
	;change color of upper cases
	;WITHOUT ERASING THE BACKGROUND
	;COMBINE SET_CURSOR+WRCHARS
	;mode = 18   -> X,Y : CURSOR COORDINATES ( 640 X 480 )
	;DOES THR MAPPING TO 80 x 30
	;BACKCOLOR <=> 0 -> RESULTING COLOR = COLOR
	;IF OVERLAPP ON OTHER BACKGROUNDS
	;		-> RESULTING COLOR = COLOR xor BACKCOLOR xor BACKGROUND
;======================================================================
	subroutine	isetstring,STDFRAME,7
;=======================================================================

	save_regs
	arg_ptr	1, edi			; point to arg1 x
	mov 	cx,word ptr [edi]
	mov	xcoord,cx
	arg_ptr	2, edi			; point to arg2 y
	mov 	ax,word ptr [edi]
	mov	ycoord,ax

	;do the mapping for x : (x-xmodulo8)/8
	xor	ebx,ebx
	xor	edx,edx
	mov	ax,cx
	mov	bx,08h
	div	bx
	mov	ax,xcoord
	sub	ax,dx
	shr	ax,1
	shr	ax,1
	shr	ax,1
	mov	cl,al

	;do the mapping for y : 29-(y-ymodulo8)/16
	mov	ax,ycoord
	mov	bx,8
	div	bx
	mov	ax,ycoord
	sub	ax,dx
	shr	ax,1
	shr	ax,1
	shr	ax,1
	shr	ax,1
	sub	ax,29
	neg	ax

	mov 	ah,02h
	mov	bh,00h
	mov 	dl,cl
	mov 	dh,al
	int 10h

	arg_ptr	6, edi			;bkcolor
	mov 	al,byte ptr [edi]
	arg_ptr	5, edi			;color
	mov 	bl,byte ptr [edi]
	and	bl,0fh
	xor	bl,al
	mov	al,8
	mov	cx,04
	shl	al,cl
	and	al,0f0h
	or	bl,al
	mov	color_up,bl

	xor	eax,eax
	xor	ebx,ebx

	arg_ptr	6, edi			;bkcolor
	mov 	al,byte ptr [edi]
	arg_ptr	4, edi			;color
	mov 	bl,byte ptr [edi]
	and	bl,0fh
	xor	bl,al
	mov	al,8
	mov	cx,04
	shl	al,cl
	and	al,0f0h
	or	bl,al
	mov	color_letters,bl


	char_arg_ptr	3,1,esi,ecx       ;string in esi
					;length in ecx
nextletter:
	lodsb
	cmp	al,'A'                  ;look for upper case characters
	jb	short lowercase
	cmp	al,'Z'
	ja	short lowercase
	mov	bl,color_up   ;if upper case ,change color
	mov	dl,al                   ;store it in dl

lowercase:
	mov	ah,0eh
	int	10h
	mov	bl,color_letters
	loop	nextletter

	mov	al,dl
	or	al,20h                  ;make it lower case,because with
					;getch() you can have some trouble
					;with upper case
     	char_arg_ptr 7,2,esi,ecx
     	mov	[esi],al                ;return it


	restore_regs

	return
	end_subroutine




;***************************************************************************
;***************************************************************************
;======================================================================
	;subroutine to be called from the main program
	;call reset_mouse(status,buttons_number)
	;initialize the mouse driver and return the driver status and
	;the number of mouse buttons
	;status=ffffh -> mouse support available

	subroutine	RESET_MOUSE,STDFRAME,2

;=======================================================================
	save_regs
	mov ax,00h
	int 033h

	arg_ptr	1, ecx			; point to arg1 :status
	arg_ptr	2, edi			; point to arg2 :number
	mov 	word ptr [ecx],ax
	mov 	word ptr [edi],bx
	restore_regs
	return
	end_subroutine

;***************************************************************************
;***************************************************************************
	;subroutine to be called from the main program
	;displays the mouse pointer
	;call show_mouse()

	subroutine	SHOW_MOUSE,STDFRAME,0
;=======================================================================
	save_regs
	mov ax,01h
	int 033h

	restore_regs
	return
	end_subroutine

;***************************************************************************
;***************************************************************************
	;subroutine to be called from the main program
	;removes the mouse from the display
	;call hide_mouse()
	subroutine	HIDE_MOUSE,STDFRAME,0
;=======================================================================
	save_regs
	mov ax,02h
	int 033h

	restore_regs
	return
	end_subroutine

;***************************************************************************
;***************************************************************************
;======================================================================
	;subroutine to be called from the main program
	;call get_mouse(status,ix,iy)
	;returns the current button status and pointer position
	;for 2 buttons mouse:
	;status=1 -> left buton down
	;status=2 -> right button down
	;for 3 buttons mouse:
	;status=1 -> left buton down
	;status=2 -> right buton down
	;status=4 -> center button down
	;it is modified for mode 18

	subroutine	GET_MOUSE,STDFRAME,3
;=======================================================================
	save_regs
	mov ax,03h
	int 033h

	arg_ptr	1, edi			; point to arg1 :status
	mov 	word ptr [edi],bx
	arg_ptr	2, edi			; point to arg2 :x
	mov 	word ptr [edi],cx
	arg_ptr	3, edi			; point to arg3 :y
	sub	dx,479
	neg	dx
	mov 	word ptr [edi],dx
	restore_regs
	return
	end_subroutine

;***************************************************************************
;***************************************************************************
	;subroutine to be called from the main program
	;call mouse_update(status,ix,iy)
	;modified version of get_mouse:it seems faster!!!!
	;returns the current button status and pointer position
	;for 2 buttons mouse:
	;status=1 -> left buton down
	;status=2 -> right button down
	;for 3 buttons mouse:
	;status=1 -> left buton down
	;status=2 -> right buton down
	;status=4 -> center button down
	;it is modified for mode 18
;=======================================================================

	subroutine mouse_update,stdframe,3

;=======================================================================

	save_regs
	mov ax,03h
	int 033h
	arg_ptr	1, edi			; point to arg1 :status
	mov 	word ptr [edi],bx
	mov ax,11h
	int 033h

	arg_ptr	2, edi			; point to arg2 :x
	mov 	word ptr [edi],cx
	arg_ptr	3, edi			; point to arg3 :y
	sub	dx,479
	neg	dx
	mov 	word ptr [edi],dx
	restore_regs
	return
	end_subroutine



;***************************************************************************
	;subroutine to be called from the main program
	;call get_button(status,button_identifier,button_counter,ix,iy)
	;return the current status of mouse buttons and the number of presses
	;and the position of the last press for a specified button
	;button_identifier = 1 -> left buton down
	;button_identifier = 2 -> right buton down
	;button_identifier = 4 -> center button down
	subroutine	GET_BUTTON,STDFRAME,5
;=======================================================================
	save_regs
	mov 	ax,05h
	arg_ptr	2, edi			; point to arg2 :identifier
	mov 	bx,word ptr [edi]
	int 033h

	arg_ptr	3, esi			; point to arg3 :counter
	mov 	word ptr [esi],bx
	arg_ptr	4, edi			; point to arg4 :ix
	mov 	word ptr [edi],cx
	arg_ptr	5, edi			; point to arg5 :iy
	sub	dx,479
	neg	dx
	mov 	word ptr [edi],dx
	arg_ptr	1, esi			; point to arg1 :status
	mov 	word ptr [esi],ax
	restore_regs
	return
	end_subroutine

;***************************************************************************
;***************************************************************************
;======================================================================
	;subroutine to be called from the main program
	; call set_mouse(ix,iy)
	;sets the position for the mouse pointer
	;modified for mode 18
	subroutine	SET_MOUSE,STDFRAME,2
;=======================================================================
	save_regs
	mov ax,04h
	int 033h

	arg_ptr	1, edi			; point to arg1 x
	mov 	cx,word ptr [edi]
	arg_ptr	2, edi			; point to arg2 y
	mov 	dx,word ptr [edi]
	sub	dx,479
	neg	dx
	int 033h
	restore_regs
	return
	end_subroutine

;***************************************************************************
;***************************************************************************
;	subroutine to be called from the main program:
;	set_blink(choice)
; 	toggle blink/intensity
;	0 = enable intensity
;	1 = enable blinking
;=======================================================================
	subroutine	SET_BLINK,STDFRAME,1
;=======================================================================
	save_regs
	mov ah,10h
	mov al,03h
	arg_ptr	1, edi			; point to arg1 choice
	mov 	bl,byte ptr [edi]

	int 10h

	restore_regs
	return
	end_subroutine
;***************************************************************************
;***************************************************************************
;	subroutine to be called from the main program:
;	set_palreg(palette_register,color)
;	sets the correspondence of a palette register to a displayable color
;=======================================================================
	subroutine	SET_PALREG,STDFRAME,2
;=======================================================================
	save_regs
	mov ah,10h
	mov al,00h
	arg_ptr	1, esi			; point to arg1 palette register
	mov 	bl,byte ptr [esi]
	arg_ptr	2, edi			; point to arg2 color
	mov 	bh,byte ptr [edi]

	int 10h

	restore_regs
	return
	end_subroutine


;***************************************************************************
;***************************************************************************
;	subroutine to be called from the main program:
;	get_palreg(palette_register,color)
;	returns the color associated with the specified palette register
;=======================================================================
	subroutine	GET_PALREG,STDFRAME,2
;=======================================================================
	save_regs
	mov ah,10h
	mov al,07h
	arg_ptr	1, edi			; point to arg1 palette register
	mov 	bl,byte ptr [edi]

	int 10h
	arg_ptr	2, esi			; point to arg2 color
	mov 	byte ptr [esi],bh

	restore_regs
	return
	end_subroutine


;***************************************************************************
;***************************************************************************
;	subroutine to be called from the main program:
;	set_rgb(color_register,red,green,blue)
;	programs an individual color register with a red-green-blue
;	combination(RGB)
;=======================================================================
	subroutine	SET_RGB,STDFRAME,4
;=======================================================================
	save_regs
	mov ah,10h
	mov al,10h
	arg_ptr	1, esi			; point to arg1 color register
	mov 	bx,word ptr [esi]
	arg_ptr	2, esi			; point to arg2 red
	mov 	dh,byte ptr [esi]
	arg_ptr	3, esi			; point to arg3 green
	mov 	ch,byte ptr [esi]
	arg_ptr	4, esi			; point to arg4 blue
	mov 	cl,byte ptr [esi]

	int 10h

	restore_regs
	return
	end_subroutine
;***************************************************************************
;***************************************************************************
;	subroutine to be called from the main program:
;	get_rgb(color_register,red,green,blue);
;       returns the contents of a color register as its red,green,and blue
;	components;
	subroutine	GET_RGB,STDFRAME,4
;=======================================================================
	save_regs
	mov ah,10h
	mov al,15h
	arg_ptr	1, esi			; point to arg1 color register
	mov 	bx,word ptr [esi]
	int 10h
	arg_ptr	2, esi			; point to arg2 red
	mov 	byte ptr [esi],dh
	arg_ptr	3, esi			; point to arg3 green
	mov 	byte ptr [esi],ch
	arg_ptr	4, esi			; point to arg4 blue
	mov 	byte ptr [esi],cl


	restore_regs
	return
	end_subroutine

;***************************************************************************
;***************************************************************************
;	subroutine to be called from the main program:
;	get_rgb(color_register,red,green,blue);
;       returns the contents of a color register as its red,green,and blue
;	components;
	subroutine	GET_PRINTER,STDFRAME,1
;=======================================================================
	save_regs
	arg_ptr	1, esi			; point to arg1 color register
	mov 	cx,word ptr [esi]
	mov	ah,cl
	mov     bl,20h
	int 10h


	restore_regs
	return
	end_subroutine

;***************************************************************************
;***************************************************************************
	end_code
	end
