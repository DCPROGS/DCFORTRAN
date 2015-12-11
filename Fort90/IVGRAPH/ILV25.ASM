

;#############################################################################
;Ioana Luisa Vais
;UCL
;Pharmacology
;
;MODULE ILV25.ASM
;
;MODIFIED:05.10.1993
;modified:16.12.1993
;modified:18.05.1994
;
;THIS IS A GRAPHIC LIBRARY WRITTEN IN PHAR LAP ASSEMBLY

;SUBROUTINES USING DIRECT READING/WRITING:
;	 1. WPIXEL(X,Y,COLOR)
;	 -draw a pixel at coord. x,y in color

;	 2. RPIXEL(X,Y,COLOR)
;	 -return color of a pixel with coord. x,y

;	 3. ILINE(X1,Y1,X2,Y2,COLOR)
;        -draw a line in color

;	 4. HLINE(X1,X2,Y,COLOR)
;        -draw a horizontal line

;	 5. DHLINE(X1,X2,Y,COLOR,PATTERN)
;        -draw a horizontal dotted line

;	 6. TDHLINE(X1,X2,Y,COLOR,PATTERN,THICKNESS)
;        -draw a horizontal dotted line with thickness control

;	 6. VLINE(X,Y1,Y2,COLOR)
;        -draw a vertical line

;	 7. DVLINE(X,Y1,Y2,COLOR,PATTERN,THICKNESS)
;        -draw a vertical dotted line with thickness control

;        8. IPOLY(X(N),Y(N),N,COLOR)
;	-draw a polygon in color

;	 9. ICIRCLE(X,Y,R,COLOR)
;	 -draw a circle

;	10. CLEARBYTE (COLOR)
;	-fill with color a byte coloumn of pixels

;	11. CLEARS (Y1,Y2,COLOR)
;	-fill with color an array (0:640,y1:y2)

;       12. FILLWIN(X1,Y1,X2,Y2,COLOR)
;	-fill with color a rectangular array (x1:x2,y1:y2)

;	13. RDGWIN1(X1,Y1,X2,Y2,BUFFER)
;       -read a rectangular disply area

;	14. WRGWIN1(X1,Y1,X2,Y2,BUFFER)
;       -write a rectangular disply area

;       15. FILLPOLY(X(N),Y(N),N,COLOR,COLOR_FILL)
;	-fill a polygon in color

;       16. FILLCIRCLE(X,Y,R,COLOR,COLOR_FILL)
;       -fill a circle in color

;====================================================================

include macros.pl
include ioana.pl4

;======================================================================
	start_data
	end_data
;======================================================================



;======================================================================
	start_code              	;start the code
;======================================================================

;***************************************************************************
;***************************************************************************

	;THIS IS A SUBROUTINE TO DRAW A PIXEL
	;subroutine to be called from the main program
	;call WPIXEL(x,y,color):utilize a VLINE code modified


;==========================================================================
	subroutine    	WPIXEL,STDFRAME,3
;==========================================================================

	save_regs			;save required registers
	push fs

	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0fh

	arg_ptr	3,ebx
	mov	cl,byte ptr [ebx]

 	OUTPUT	GRAPHICS, SET_RESET, cl


	arg_ptr	1,ebx          		;point to first argument x
	mov	ax,word ptr [ebx]       ;load in ax
	mov	x,ax
	;calculate xbyte
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     xbyte,ax              	;store in xbyte

	mov	cx,x
	and	cx,7
	mov 	bl,080h
	shr	bl,cl
	mov	mask0,bl

	mov	ax,00h
	mov	bx,00h
	;calculate address
	arg_ptr	2,ebx                   ;point to arg 2 y
	mov	ax,word ptr [ebx]       ;load in ax
	sub	ax,ROWS
	neg	ax
      	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,xbyte               	;add xbyte to result
	mov	si,ax              	;store address in addr

	mov	ax,01fh
	mov	fs,ax

  	OUTPUT 	GRAPHICS, BIT_MASK, MASK0

       	mov	al,fs:[si]
       	mov	fs:[si],al             	;output color
	pop fs
	restore_regs			;restore protected integer registers


	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 00h
	return
        end_subroutine


;***************************************************************************
;***************************************************************************
	subroutine    	WXEL,STDFRAME,3
;==========================================================================

	save_regs			;save required registers
	push fs

	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0fh

	arg_ptr	3,ebx
	mov	cl,byte ptr [ebx]

 	OUTPUT	GRAPHICS, SET_RESET, cl


	arg_ptr	1,ebx          		;point to first argument x
	mov	ax,word ptr [ebx]       ;load in ax
	mov	x,ax
	arg_ptr	2,ebx          		;point to first argument x
	mov	ax,word ptr [ebx]       ;load in ax
	mov	y,ax

	WRITEPIX  X, Y            ;xc-y; yc-x

	pop fs
	restore_regs			;restore protected integer registers


	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 00h
	return
        end_subroutine
;***************************************************************************
;***************************************************************************

	;THIS IS A SUBROUTINE TO READ A PIXEL VALUE
	;subroutine to be called from the main program
	;call RPIXEL(x,y,color) : R. WILTON p.128+96


;==========================================================================
	subroutine     RPIXEL,STDFRAME,3
;==========================================================================

	save_regs			;save required registers
	push 	fs

	arg_ptr	1,ebx          		;point to first argument x
	mov	bx,word ptr [ebx]       ;load in ax

	mov	cl,bl			;in cl low order byte of x

	;calculate xbyte
	shr	bx,1
	shr	bx,1
	shr	bx,1

	;calculate address
	arg_ptr	2,edx                   ;point to arg 2 y
	mov	ax,word ptr [edx]       ;load in ax
	xor	edx,edx
	sub	ax,ROWS
	neg	ax
      	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	bx,ax               	;add xbyte to result :address
	add	bx,0              	;store address in address

	mov	ax,01fh
	mov	fs,ax

	and 	cl,7             	;cl = x&7
	xor	cl,7                    ;cl = nr of bits to shift left
	mov	ah,1                    ;unshifted bit mask
					;ah=011fh
	mov	ch,ah                   ;ch=1
	shl	ch,cl        		;bit mask in proper position
					;ch=?

	mov	si,bx                   ;address in si
	xor 	bl,bl                   ;bl used to accumulate the pixel value
	mov	dx,3ceh                 ;dx= grphics i/o port_addr
	mov	ax,304h                 ;ah = initial bit plane number
					;al = READ_MAP_SELECT
l01:	out	dx,ax                   ;select bit plane
	mov	bh,fs:[si]              ;bh = byte from the current bit plane
					;!!! always 0 ???????
	and	bh,ch                   ;mask one bit
	neg	bh                      ;bit 7 of bh = 1 (if masked bit = 1)
			                ;bit 7 of bh = 1 (if masked bit = 1)

	rol	bx,1                    ; bit 0 of bl = next bit from pixel value
	dec	ah                      ;next bit plane number
	jge	l01

	mov	al,bl                   ;al = pixel value
	xor	ah,ah                   ;ax = pixel value
	arg_ptr	3,ebx
	mov	word ptr [ebx],ax       ;returns 0:i have always bh=0
	pop 	fs                      ;
	restore_regs			;restore protected integer registers
	return
        end_subroutine


;***************************************************************************
;***************************************************************************
	;THIS IS A SUBROUTINE TO DRAW A LINE
	;subroutine to be called from the main program
	;call ILINE(x1,y1,x2,y2,color)

;==========================================================================
	subroutine    	ILINE,STDFRAME,5
;===========================================================================


	save_regs			;save required registers
	push fs

	mov	ax,01fh
	mov	fs,ax


	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0Fh
	arg_ptr	5,ebx
	mov	cl,byte ptr [ebx]
	mov	color,cl

    	OUTPUT	GRAPHICS, SET_RESET, Cl

	xor	edx,edx
	arg_ptr	2,edi          		;point to second argument y1
	mov	dx,word ptr [edi]       ;load in ax
	mov	ylo,dx

	xor	ebx,ebx
	arg_ptr	4,edi                    ;y2
	mov	bx,word ptr [edi]
	mov	yhi,bx
	sub	bx,dx
	mov	deltay,bx                  ;adeltay=y2-y1
	cmp	bx,0
	jg	lin2
	neg	bx                        ;bx=dy

lin2:
	mov	adeltay,bx                   ;adeltay=|y2-y1|
	xor	eax,eax
	arg_ptr	1,edi          		;point to first argument x1
	mov	ax,word ptr [edi]       ;load in ax
	mov	xlo,ax
	xor	edi,edi
	arg_ptr	3,edi                    ;x2
	mov	di,word ptr [edi]
	mov	xhi,di
	sub	di,ax                     ;di=dx
	mov	deltax,di                   ;deltax=x2-x1
	cmp	di,0        		  ;x2>x1?
	jg	lin1
	xor	ecx,ecx
	mov	cx,xhi                    ;x2<x1
	mov	x,cx                     ;x=x2
	xor	ecx,ecx
	mov	cx,yhi
	mov	y,cx                     ;y=y2
	neg	di                        ;di=|x2-x1|=dx
	mov	incr0,80             ;y2<y1; inc=+80
	mov	cx,deltay
	cmp	cx,0                    ;y2>y1?
	jg	lin3
	neg	incr0              ;inc=-80
lin3:
	jmp	linor

lin1:                                     ;x2>x1
	mov	x,ax                     ;x=x1
	mov	y,dx                     ;y=y1
	mov	incr0,80             ;y2<y1; inc=+80
	cmp	deltay,0                    ;y2>y1?
	jle	linor
	neg	incr0               ;y2>y1 inc=-80

linor:
	mov	adeltax,di                    ;adeltax=|x2-x1|

	;calculate x>>3 for address
	mov	ax,x
	shr	ax,1
	shr	ax,1
	shr	ax,1
	mov	xbyte,ax


	;calculate mask
	mov	cx,x
	and	cx,7
      	mov 	al,080h
      	shr	al,cl
	mov	mask0,al

	sub	di,bx                	;dx-dy>0? di=dx-dy
	cmp	di,0
	jle	lin5

;xline:                                 ;dx>dy

	mov	ax,bx                   ;ax=dy
	shl	ax,1                    ;2*dy
	mov	incr1,ax               ;incr1=2*dy
	sub	ax,adeltax                  ;2*dy-dx
	mov	delta,ax                   ;d=2*dy-dx
	neg	di                      ;di=dy-dx
	shl	di,1                    ;2*(dy-dx)
	mov	incr2,di               ;incr2=2*(dy-dx)
	xor	edi,edi
	;calculate address
	C_ADDRESS  y,xbyte

	inc	adeltax
  	OUTPUT 	GRAPHICS, BIT_MASK, 8h
	mov	al,mask0
	mov	cx,adeltax
	mov	bx,delta
	xor 	edi,edi
	mov	di,incr0
lloop1:
	out	dx,al
	mov	ah,fs:[si]
	mov	fs:[si],ah
	shr	al,1
	jnc	same
	mov	al,80h
	inc	si
same:
	cmp	bx,0
	jl	noinc1
	add	bx,incr2
	add	si,di
	jmp	check1
noinc1:
	add	bx,incr1
check1:
	loop	lloop1
	jmp	lend

	;start code for dx<dy
lin5:
	mov	ax,adeltax
	shl	ax,1
	mov	incr1,ax    	;incr1=2*dx
	sub	ax,adeltay          ;ax=2*dx-dy
	mov	delta,ax           ;d=2*dx-dy
	shl	di,1
	mov	incr2,di       ;incr2=2*(dx-dy)
	mov	di,80
	neg	di
	mov	incr0,di
	inc	adeltay
	cmp	deltay,0          ;y2-y1>0?
	jg	lin51

	; here for y1>y2
	mov	ax,xhi          ;y2<y1 x=x2,y=y2
	shr	ax,1
	shr	ax,1
	shr	ax,1
	mov	xbyte,ax

	;calculate address
	C_ADDRESS  yhi,xbyte
	;calculate mask
	mov	cx,xhi
	and	cx,7
      	mov 	al,080h
      	shr	al,cl
	mov	mask0,al

	cmp	deltax,0           ;x2-x1>0?
	jle	ypline           ;x2<=x1
	jmp	ynline           ;x2>x1

lin51:
	mov	ax,xlo
	shr	ax,1
	shr	ax,1
	shr	ax,1
	mov	xbyte,ax

	;calculate address
	C_ADDRESS  ylo,xbyte
	;calculate mask
	mov	cx,xlo
	and	cx,7
      	mov 	al,080h
      	shr	al,cl
	mov	mask0,al
	cmp	deltax,0
	jle	ynline

ypline:
  	OUTPUT 	GRAPHICS, BIT_MASK, 8h
	mov	cx,adeltay
	mov	bx,delta
	mov	al,mask0
lloop3:
	out	dx,al
	mov	ah,fs:[si]
	mov	fs:[si],ah
	add	si,incr0
	cmp	bx,0
	jl	noinc3
	add	bx,incr2
	shr	al,1
	jnc	check3
	mov	al,080h
	inc	si
	jmp	check3
noinc3:
	add	bx,incr1
check3:
	loop	lloop3

	jmp	lend

ynline:
  	OUTPUT 	GRAPHICS, BIT_MASK, 8h
	mov	cx,adeltay
	mov	bx,delta
	mov	al,mask0
lloop2:
	out	dx,al
	mov	ah,fs:[si]
	mov	fs:[si],ah
	add	si,incr0
	cmp	bx,0
	jl	noinc2
	add	bx,incr2
	shl	al,1
	jnc	check2
	mov	al,01h
	dec	si
	jmp	check2
noinc2:
	add	bx,incr1
check2:
	loop	lloop2
	jmp	lend
lend:
	pop fs

	restore_regs			;restore protected integer registers


 	OUTPUT 	GRAPHICS, MAP_MASK, 0Fh
	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 00h
	return
        end_subroutine


;***************************************************************************
;***************************************************************************

	;THIS IS A SUBROUTINE TO DRAW A HORIZONTAL LINE
	;subroutine to be called from the main program
	;call HLINE(x1,x2,y,color) : R.FERRARO p.511-515


;==========================================================================
	subroutine     HLINE,STDFRAME,4
;==========================================================================



	save_regs			;save required registers
	push fs

	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0Fh


	arg_ptr	4,ebx                 	;get color
	mov	cl,byte ptr [ebx]
	mov	color,cl
	mov	cl,0h

 	OUTPUT	GRAPHICS, SET_RESET, COLOR

	arg_ptr	1,ebx          		;point to first argument x1
	mov	ax,word ptr [ebx]       ;load in ax

	arg_ptr 2,ebx                   ;point to second argument x2
	mov	cx,word ptr [ebx]       ;load in cx

	cmp	ax,cx
	jl	cont                    ;ordonate
	mov	dx,ax
	mov	ax,cx
	mov	cx,dx                   ;end change

cont:			;restore dx
	mov	xlo,ax
	mov	xhi,cx
	cmp	xlo,639
	jg      ende
	cmp	xlo,0
	jge     niv1
	mov	xlo,0
niv1:
	cmp	xhi,0
	jl      ende
	cmp	xhi,639
	jle     niv2
	mov	xhi,639
niv2:
	mov	ax,xlo
	mov	cx,xhi
	mov	dx,00h

	;calculate xbytelo
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     xbytelo,ax              	;store in x1byte
	;calculate xbytehi
	shr	cx,1
	shr	cx,1
	shr	cx,1
      	mov	xbytehi,cx               ;store in x2byte
	;calculate deltax
	sub	cx,ax                   ;x2byte-x1byte and store in cx
	mov	deltax,cx		;store in deltax

	;calculate nbyte
	sub	cx,1			;calculate nbyte
	mov	bytes_per_line,cx		;store nbyte

	xor 	dx,dx
	mov 	ax,00h

	;calculate address
	arg_ptr	3,ebx                   ;point to arg 3 y
	mov	ax,word ptr [ebx]       ;load in ax

	;now we want to move the origin in the lowest left corner
	;initially it was in top left corner

	sub	ax,ROWS                  ;this is specific to mode 12h
	neg	ax                      ;where we have 480 rows
      	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,xbytelo               ;add x1byte to result
	mov	address,ax              ;store address in address

	mov 	ax,00h
	xor 	dx,dx
	;calculate x1 modulo 8
       mov 	ax,xlo			;mov x1 in ax
       mov	bx,8                    ;divide by 8
       div	bx
       mov	temp1,dl		;store result in temp1
	xor 	dx,dx

	 mov	ax,00h
	;calculate x2 modulo 8
	mov 	ax,xhi		;mov x2 in ax
	mov	bx,8                    ;divide by 8
	div	bx
	mov	temp2,dl		;store result in temp2

	mov	bx,00h
	mov	dx,00h

	;begin the x1modulo8 case
	;compare with values 0:7 and choose the correct mask
	mov	ah,temp1		;mov in ah x1modulo8
	cmp	ah,0
	je	a0
	cmp	ah,1
	je	a1
	cmp	ah,2
	je	a2
	cmp	ah,3
	je	a3
	cmp	ah,4
	je	a4
	cmp	ah,5
	je	a5
	cmp	ah,6
	je	a6
	cmp	ah,7
	je	a7

a0:	mov	bx,mask10
	jmp	done1
a1:	mov	bx,mask11
	jmp	done1
a2:	mov	bx,mask12
	jmp	done1

a3:	mov	bx,mask13
	jmp	done1

a4:	mov	bx,mask14
	jmp	done1

a5:	mov	bx,mask15
	jmp	done1

a6:	mov	bx,mask16
	jmp	done1

a7:	mov	bx,mask17
	jmp	done1

done1:	;begin case x2modulo8
	mov	ah,temp2
	cmp	ah,0
	je	a20
	cmp	ah,1
	je	a21
	cmp	ah,2
	je	a22
	cmp	ah,3
	je	a23
	cmp	ah,4
	je	a24
	cmp	ah,5
	je	a25
	cmp	ah,6
	je	a26
	cmp	ah,7
	je	a27

a20:	mov	dx,mask20
	jmp	done2
a21:	mov	dx,mask21
	jmp	done2
a22:	mov	dx,mask22
	jmp	done2

a23:	mov	dx,mask23
	jmp	done2

a24:	mov	dx,mask24
	jmp	done2

a25:	mov	dx,mask25
	jmp	done2

a26:	mov	dx,mask26
	jmp	done2

a27:	mov	dx,mask27
	jmp	done2

done2:

	mov	cx,00h
	mov	mask1,bl
	mov	mask2,dl

	mov	cx,deltax   		;compare deltax with 1
	cmp	cx,1
	je	two0                    	;if 1 jump to two
	jg	three0                 	;>1 jump to three

	;begin drawing line for 1 byte
	and	bx,dx
	mov 	cx,00h

	mov	dx,00h

       	mov 	ax,01fh			;point to display address

       	mov 	fs,ax
       	mov 	si,address              ;get address

       	mov	ax,3ceh   		;point to mask address
       	mov	dx,ax
       	mov	al,8
       	out	dx,al
       	inc	dx                      ;point to mask register

	mov 	al,bl                   ;get mask
       	out     dx,al                   ;load mask
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	jmp	ende
two0:
	mov	ax,01fh
	mov	fs,ax
	mov 	si,address              ;get address

       	mov	ax,3ceh   		;point to mask address
       	mov	dx,ax
       	mov	al,8
       	out	dx,al
       	inc	dx                      ;point to mask register
	;output first byte
	mov	al,mask1		;get mask1
       	out     dx,al                   ;load mask
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	;output second byte
	mov	al,mask2

	out	dx,al
	inc 	si
       	out     dx,al                   ;load mask
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	jmp	ende
three0:
	mov	ax,01fh
	mov	fs,ax

 	mov 	si,address              ;get address


       	mov	ax,3ceh   		;point to mask address
       	mov	dx,ax
       	mov	al,8
       	out	dx,al
       	inc	dx                      ;point to mask register

	;output first byte
	mov	al,mask1		;get mask1

       	out     dx,al                   ;load mask
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color


	;load the middle bytes
	mov	al,0ffh
	out	dx,al
	mov	cx,1
loop1:
	inc	si
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	inc	cx

	cmp	cx,bytes_per_line
	jle	loop1

	;output end byte
	mov	al,mask2                ;get mask2


	out	dx,al
	inc 	si
       	out     dx,al                   ;load mask
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color


ende:
	pop fs
	restore_regs			;restore protected integer registers


	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 00h
	OUTPUT	SEQUENCER, MAP_MASK, 0Fh
	return
        end_subroutine

;***************************************************************************
;***************************************************************************

	;THIS IS A SUBROUTINE TO DRAW A HORIZONTAL LINE
	;subroutine to be called from the main program
	;call dHLINE(x1,x2,y,color,dot) : R.FERRARO p.511-515
	;dot : 0 = no space,continue
	;      1,2   = one space
	;      3,4,5 = three spaces
	;      6,7   = seven spaces
	;      22    = 2+2
	;      44    = 4+4
	;      88    = 8+8
;==========================================================================
	subroutine     dHLINE,STDFRAME,5
;==========================================================================



	save_regs			;save required registers
	push fs

	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0Fh

	arg_ptr	1,ebx          		;point to first argument x1
	mov	ax,word ptr [ebx]       ;load in ax
	mov	x_left,ax
	arg_ptr 2,ebx                   ;point to second argument x2
	mov	cx,word ptr [ebx]       ;load in cx
	mov	x_right,cx

	arg_ptr	3,ebx                   ;point to arg 3 y
	mov	ax,word ptr [ebx]       ;load in ax
	mov     ylo,ax

	arg_ptr	5,ebx                 	;get color
	mov	cl,byte ptr [ebx]
	mov	dot,cl

	mov	newmask,0ffh
	cmp 	cl,0
	je      fhcontinue

	cmp 	cl,88
	je      fhcontinue

	mov	newmask,055h
	cmp	cl,1
	je      fhcontinue
	cmp	cl,2
	je      fhcontinue

	mov	newmask,011h
	cmp	cl,3
	je      fhcontinue
	cmp	cl,4
	je      fhcontinue
	cmp	cl,5
	je      fhcontinue

	mov	newmask,001h
	cmp	cl,6
	je      fhcontinue
	cmp	cl,7
	je      fhcontinue

	mov	newmask,033h
	cmp	cl,22
	je      fhcontinue

	mov	newmask,0fh

fhcontinue:

	arg_ptr	4,ebx                 	;get color
	mov	cl,byte ptr [ebx]
	mov	color,cl
	mov	cl,0h

 	OUTPUT	GRAPHICS, SET_RESET, COLOR

	mov	ax,x_left
	mov	cx,x_right


	cmp	ax,cx
	jl	fcontd                    ;ordonate
	mov	dx,ax
	mov	ax,cx
	mov	cx,dx                   ;end change

fcontd:			;restore dx
	mov	xlo,ax
	mov	xhi,cx
	mov	dx,00h

	;calculate x1byte
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     xbytelo,ax              	;store in x1byte
	;calculate x2byte
	shr	cx,1
	shr	cx,1
	shr	cx,1
      	mov	xbytehi,cx               ;store in x2byte
	;calculate deltax
	sub	cx,ax                   ;x2byte-x1byte and store in cx
	mov	deltax,cx		;store in deltax

	;calculate nbyte
	sub	cx,1			;calculate nbyte
	mov	bytes_per_line,cx		;store nbyte

       mov 	ax,bytes_per_line			;mov x1 in ax
       mov	bx,2                    ;divide by 8
       div	bx
       mov	temp0,dl		;store result in temp1
	xor 	dx,dx
	mov 	ax,00h

	;calculate address
	mov	ax,ylo              ;load in ax

	;now we want to move the origin in the lowest left corner
	;initially it was in top left corner

	sub	ax,ROWS                  ;this is specific to mode 12h
	neg	ax                      ;where we have 480 rows
      	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,xbytelo               ;add x1byte to result
	mov	address,ax              ;store address in address

	mov 	ax,00h
	xor 	dx,dx
	;calculate x1 modulo 8
       mov 	ax,xlo		;mov x1 in ax
       mov	bx,8                    ;divide by 8
       div	bx
       mov	temp1,dl		;store result in temp1
	xor 	dx,dx

	 mov	ax,00h
	;calculate x2 modulo 8
	mov 	ax,xhi			;mov x2 in ax
	mov	bx,8                    ;divide by 8
	div	bx
	mov	temp2,dl		;store result in temp2

	mov	bx,00h
	mov	dx,00h

	;begin the x1modulo8 case
	;compare with values 0:7 and choose the correct mask
	mov	ah,temp1		;mov in ah x1modulo8
	cmp	ah,0
	je	fda0
	cmp	ah,1
	je	fda1
	cmp	ah,2
	je	fda2
	cmp	ah,3
	je	fda3
	cmp	ah,4
	je	fda4
	cmp	ah,5
	je	fda5
	cmp	ah,6
	je	fda6
	cmp	ah,7
	je	fda7

fda0:	mov	bx,mask10
	jmp	fddone1
fda1:	mov	bx,mask11
	jmp	fddone1
fda2:	mov	bx,mask12
	jmp	fddone1

fda3:	mov	bx,mask13
	jmp	fddone1

fda4:	mov	bx,mask14
	jmp	fddone1

fda5:	mov	bx,mask15
	jmp	fddone1

fda6:	mov	bx,mask16
	jmp	fddone1

fda7:	mov	bx,mask17
	jmp	fddone1

fddone1:	;begin case x2modulo8
	mov	ah,temp2
	cmp	ah,0
	je	fda20
	cmp	ah,1
	je	fda21
	cmp	ah,2
	je	fda22
	cmp	ah,3
	je	fda23
	cmp	ah,4
	je	fda24
	cmp	ah,5
	je	fda25
	cmp	ah,6
	je	fda26
	cmp	ah,7
	je	fda27

fda20:	mov	dx,mask20
	jmp	fddone2
fda21:	mov	dx,mask21
	jmp	fddone2
fda22:	mov	dx,mask22
	jmp	fddone2

fda23:	mov	dx,mask23
	jmp	fddone2

fda24:	mov	dx,mask24
	jmp	fddone2

fda25:	mov	dx,mask25
	jmp	fddone2

fda26:	mov	dx,mask26
	jmp	fddone2

fda27:	mov	dx,mask27
	jmp	fddone2

fddone2:

	mov	cx,00h
	mov	mask1,bl
	mov	mask2,dl

	mov	cx,deltax   		;compare deltax with 1
	cmp	cx,1
	je	fdtwo0                    	;if 1 jump to two
	jg	fdthree0                 	;>1 jump to three

	;begin drawing line for 1 byte
	and	bx,dx
	mov 	cx,00h

	mov	dx,00h

       	mov 	ax,01fh			;point to display address

       	mov 	fs,ax
       	mov 	si,address              ;get address

       	mov	ax,3ceh   		;point to mask address
       	mov	dx,ax
       	mov	al,8
       	out	dx,al
       	inc	dx                      ;point to mask register

	mov 	al,bl                   ;get mask
       	out     dx,al                   ;load mask
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	jmp	dendef
fdtwo0:
	mov	ax,01fh
	mov	fs,ax
	mov 	si,address              ;get address

       	mov	ax,3ceh   		;point to mask address
       	mov	dx,ax
       	mov	al,8
       	out	dx,al
       	inc	dx                      ;point to mask register
	;output first byte
	mov	al,mask1		;get mask1
	and	al,newmask
       	out     dx,al                   ;load mask
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	;output second byte
	mov	al,mask2
	and	al,newmask

	out	dx,al
	inc 	si
       	out     dx,al                   ;load mask
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	jmp	dendef
fdthree0:
	mov	ax,01fh
	mov	fs,ax

 	mov 	si,address              ;get address


       	mov	ax,3ceh   		;point to mask address
       	mov	dx,ax
       	mov	al,8
       	out	dx,al
       	inc	dx                      ;point to mask register

	;output first byte
	mov	al,mask1		;get mask1
	and	al,newmask

       	out     dx,al                   ;load mask
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color


	;load the middle bytes
	mov	al,0ffh
	and	al,newmask
	out	dx,al
	mov	cx,1
fdloop1:
	inc	si
	cmp	dot,88
	jne	fdotty
	inc	cx
	inc	si
fdotty:
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	inc	cx

	cmp	dot,88
	jne	fnodec
	cmp	cx,bytes_per_line
	jl	fdloop1
	jmp     flalala
fnodec:
	cmp	cx,bytes_per_line
 	;loop loop1
	 jle	fdloop1

	;mov 	ax,00h
	;output end byte
flalala:
	mov	al,mask2                ;get mask2
	and	al,newmask


	out	dx,al
	cmp	dot,88
	jne	fnoinc
	cmp	temp0,0
	je	fnoinc
	inc	si
fnoinc:
	inc 	si
       	out     dx,al                   ;load mask
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color


dendef:
	pop fs
	restore_regs			;restore protected integer registers


	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 00h
	OUTPUT	SEQUENCER, MAP_MASK, 0Fh
	return
        end_subroutine

;***************************************************************************
;***************************************************************************


	;THIS IS A SUBROUTINE TO DRAW A HORIZONTAL LINE
	;subroutine to be called from the main program
	;call tdHLINE(x1,x2,y,color,dot,thickness) : R.FERRARO p.511-515
	;dot : 0 = no space,continue
	;      1,2   = one space
	;      3,4,5 = three spaces
	;      6,7   = seven spaces
	;      22    = 2+2
	;      44    = 4+4
	;      88    = 8+8
;==========================================================================
	subroutine     tdHLINE,STDFRAME,6
;==========================================================================



	save_regs			;save required registers
	push fs

	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0Fh

	arg_ptr	1,ebx          		;point to first argument x1
	mov	ax,word ptr [ebx]       ;load in ax
	mov	x_left,ax
	arg_ptr 2,ebx                   ;point to second argument x2
	mov	cx,word ptr [ebx]       ;load in cx
	mov	x_right,cx

	arg_ptr	3,ebx                   ;point to arg 3 y
	mov	ax,word ptr [ebx]       ;load in ax
	mov     ylo,ax

	arg_ptr	6,ebx                 	;get thickness
	mov	cx,word ptr [ebx]
	mov	thickness,cx
	xor	ecx,ecx
	arg_ptr	5,ebx                 	;get color
	mov	cl,byte ptr [ebx]
	mov	dot,cl

	mov	newmask,0ffh
	cmp 	cl,0
	je      hcontinue

	cmp 	cl,88
	je      hcontinue

	mov	newmask,055h
	cmp	cl,1
	je      hcontinue
	cmp	cl,2
	je      hcontinue

	mov	newmask,011h
	cmp	cl,3
	je      hcontinue
	cmp	cl,4
	je      hcontinue
	cmp	cl,5
	je      hcontinue

	mov	newmask,001h
	cmp	cl,6
	je      hcontinue
	cmp	cl,7
	je      hcontinue

	mov	newmask,033h
	cmp	cl,22
	je      hcontinue

	mov	newmask,0fh

hcontinue:

	arg_ptr	4,ebx                 	;get color
	mov	cl,byte ptr [ebx]
	mov	color,cl
	mov	cl,0h

 	OUTPUT	GRAPHICS, SET_RESET, COLOR

	mov	incr0,1
loopthick:
	mov	ax,x_left
	mov	cx,x_right


	cmp	ax,cx
	jl	contd                    ;ordonate
	mov	dx,ax
	mov	ax,cx
	mov	cx,dx                   ;end change

contd:			;restore dx
	mov	xlo,ax
	mov	xhi,cx
	mov	dx,00h

	;calculate x1byte
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     xbytelo,ax              	;store in x1byte
	;calculate x2byte
	shr	cx,1
	shr	cx,1
	shr	cx,1
      	mov	xbytehi,cx               ;store in x2byte
	;calculate deltax
	sub	cx,ax                   ;x2byte-x1byte and store in cx
	mov	deltax,cx		;store in deltax

	;calculate nbyte
	sub	cx,1			;calculate nbyte
	mov	bytes_per_line,cx		;store nbyte

       mov 	ax,bytes_per_line			;mov x1 in ax
       mov	bx,2                    ;divide by 8
       div	bx
       mov	temp0,dl		;store result in temp1
	xor 	dx,dx
	mov 	ax,00h

	;calculate address
	mov	ax,ylo              ;load in ax

	;now we want to move the origin in the lowest left corner
	;initially it was in top left corner

	sub	ax,ROWS                  ;this is specific to mode 12h
	neg	ax                      ;where we have 480 rows
      	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,xbytelo               ;add x1byte to result
	mov	address,ax              ;store address in address

	mov 	ax,00h
	xor 	dx,dx
	;calculate x1 modulo 8
       mov 	ax,xlo		;mov x1 in ax
       mov	bx,8                    ;divide by 8
       div	bx
       mov	temp1,dl		;store result in temp1
	xor 	dx,dx

	 mov	ax,00h
	;calculate x2 modulo 8
	mov 	ax,xhi			;mov x2 in ax
	mov	bx,8                    ;divide by 8
	div	bx
	mov	temp2,dl		;store result in temp2

	mov	bx,00h
	mov	dx,00h

	;begin the x1modulo8 case
	;compare with values 0:7 and choose the correct mask
	mov	ah,temp1		;mov in ah x1modulo8
	cmp	ah,0
	je	da0
	cmp	ah,1
	je	da1
	cmp	ah,2
	je	da2
	cmp	ah,3
	je	da3
	cmp	ah,4
	je	da4
	cmp	ah,5
	je	da5
	cmp	ah,6
	je	da6
	cmp	ah,7
	je	da7

da0:	mov	bx,mask10
	jmp	ddone1
da1:	mov	bx,mask11
	jmp	ddone1
da2:	mov	bx,mask12
	jmp	ddone1

da3:	mov	bx,mask13
	jmp	ddone1

da4:	mov	bx,mask14
	jmp	ddone1

da5:	mov	bx,mask15
	jmp	ddone1

da6:	mov	bx,mask16
	jmp	ddone1

da7:	mov	bx,mask17
	jmp	ddone1

ddone1:	;begin case x2modulo8
	mov	ah,temp2
	cmp	ah,0
	je	da20
	cmp	ah,1
	je	da21
	cmp	ah,2
	je	da22
	cmp	ah,3
	je	da23
	cmp	ah,4
	je	da24
	cmp	ah,5
	je	da25
	cmp	ah,6
	je	da26
	cmp	ah,7
	je	da27

da20:	mov	dx,mask20
	jmp	ddone2
da21:	mov	dx,mask21
	jmp	ddone2
da22:	mov	dx,mask22
	jmp	ddone2

da23:	mov	dx,mask23
	jmp	ddone2

da24:	mov	dx,mask24
	jmp	ddone2

da25:	mov	dx,mask25
	jmp	ddone2

da26:	mov	dx,mask26
	jmp	ddone2

da27:	mov	dx,mask27
	jmp	ddone2

ddone2:

	mov	cx,00h
	mov	mask1,bl
	mov	mask2,dl

	mov	cx,deltax   		;compare deltax with 1
	cmp	cx,1
	je	dtwo0                    	;if 1 jump to two
	jg	dthree0                 	;>1 jump to three

	;begin drawing line for 1 byte
	and	bx,dx
	mov 	cx,00h

	mov	dx,00h

       	mov 	ax,01fh			;point to display address

       	mov 	fs,ax
       	mov 	si,address              ;get address

       	mov	ax,3ceh   		;point to mask address
       	mov	dx,ax
       	mov	al,8
       	out	dx,al
       	inc	dx                      ;point to mask register

	mov 	al,bl                   ;get mask
       	out     dx,al                   ;load mask
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	jmp	dende
dtwo0:
	mov	ax,01fh
	mov	fs,ax
	mov 	si,address              ;get address

       	mov	ax,3ceh   		;point to mask address
       	mov	dx,ax
       	mov	al,8
       	out	dx,al
       	inc	dx                      ;point to mask register
	;output first byte
	mov	al,mask1		;get mask1
	and	al,newmask
       	out     dx,al                   ;load mask
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	;output second byte
	mov	al,mask2
	and	al,newmask

	out	dx,al
	inc 	si
       	out     dx,al                   ;load mask
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	jmp	dende
dthree0:
	mov	ax,01fh
	mov	fs,ax

 	mov 	si,address              ;get address


       	mov	ax,3ceh   		;point to mask address
       	mov	dx,ax
       	mov	al,8
       	out	dx,al
       	inc	dx                      ;point to mask register

	;output first byte
	mov	al,mask1		;get mask1
	and	al,newmask

       	out     dx,al                   ;load mask
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color


	;load the middle bytes
	mov	al,0ffh
	and	al,newmask
	out	dx,al
	mov	cx,1
dloop1:
	inc	si
	cmp	dot,88
	jne	dotty
	inc	cx
	inc	si
dotty:
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	inc	cx

	cmp	dot,88
	jne	nodec
	cmp	cx,bytes_per_line
	jl	dloop1
	jmp     lalala
nodec:
	cmp	cx,bytes_per_line
 	;loop loop1
	 jle	dloop1

	;mov 	ax,00h
	;output end byte
lalala:
	mov	al,mask2                ;get mask2
	and	al,newmask


	out	dx,al
	cmp	dot,88
	jne	noinc
	cmp	temp0,0
	je	noinc
	inc	si
noinc:
	inc 	si
       	out     dx,al                   ;load mask
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color


dende:
	inc     incr0
	mov	dx,thickness
	cmp	incr0,dx
	jg	termi
	inc	ylo
	jmp	loopthick

termi:
	pop fs
	restore_regs			;restore protected integer registers


	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 00h
	OUTPUT	SEQUENCER, MAP_MASK, 0Fh
	return
        end_subroutine

;***************************************************************************
;***************************************************************************
	;THIS IS A SUBROUTINE TO DRAW A VERTICAL LINE
	;subroutine to be called from the main program
	;call VLINE(x,y1,y2,color):R.FERRARO p.516-518

;==========================================================================
	subroutine    	VLINE,STDFRAME,4
;===========================================================================

	;define local storage
	;====================


	save_regs			;save required registers
	push fs
	arg_ptr	1,ebx          		;point to first argument x
	mov	ax,word ptr [ebx]       ;load in ax
	mov	x,ax
	cmp     ax,0
	jl      vend
	cmp     ax,639
	jg	vend
	;calculate xbyte
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     xbyte,ax              	;store in xbyte

	mov	cx,x
	and	cx,7
	mov 	bl,mask20
	shr	bl,cl
	mov	mask0,bl

	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0Fh

  	OUTPUT 	GRAPHICS, BIT_MASK, mask0

	arg_ptr	4,ebx
	mov	cl,byte ptr [ebx]
	mov	color,cl
	mov	cl,0h

 	OUTPUT	GRAPHICS, SET_RESET, COLOR

	arg_ptr	2,ebx          		;point to first argument x1
	mov	ax,word ptr [ebx]       ;load in ax

	arg_ptr 3,ebx                   ;point to second argument x2
	mov	cx,word ptr [ebx]       ;load in cx

	cmp	ax,cx
	jl	vcontv                    ;ordonate
	mov	dx,ax
	mov	ax,cx
	mov	cx,dx                   ;end change
	xor	edx,edx
vcontv:			;restore dx
	mov	ylo,ax
	mov	yhi,cx
	cmp	ylo,479
	jg      vend
	cmp	ylo,0
	jge     vniv1
	mov	ylo,0
vniv1:
	cmp	yhi,0
	jl      vend
	cmp	yhi,479
	jle     vniv2
	mov	yhi,479
vniv2:
	mov     cx,yhi
	mov	ax,ylo
	sub	ax,ROWS
	neg	ax
	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,xbyte               	;add x1byte to result
	mov	si,ax              	;store address in addr

	mov 	ax,ylo
	add	cx,1
	sub	cx,ax
	mov	deltay,cx

	mov	ax,01fh
	mov	fs,ax


	mov	cx,deltay
	mov	bx,OFSET
;====================================
;====================================
vrunloop:
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	sub	si,bx
	loop vrunloop
vend:
	pop fs
	restore_regs			;restore protected integer registers


	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 00h
	return
        end_subroutine


;***************************************************************************
;***************************************************************************

	;THIS IS A SUBROUTINE TO DRAW A VERTICAL LINE
	;subroutine to be called from the main program
	;call dVLINE(x,y1,y2,color,dot):R.FERRARO p.516-518



;==========================================================================
	subroutine    	dVLINE,STDFRAME,5
;===========================================================================

	;define local storage
	;====================

	save_regs			;save required registers
	push fs
;============================
	arg_ptr	5,edi          		;point to first argument x
	mov	cl,byte ptr [edi]       ;load in ax
	mov	dot,cl
;=============================


	xor	ecx,ecx
	xor	edi,edi
	arg_ptr	1,ebx          		;point to first argument x
	mov	ax,word ptr [ebx]       ;load in ax
	mov	xlo,ax
	;calculate xbyte
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     xbyte,ax              	;store in xbyte

	mov	cx,xlo
	and	cx,7
	mov 	bl,mask20
	shr	bl,cl
	mov	mask0,bl

	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0Fh

  	OUTPUT 	GRAPHICS, BIT_MASK, MASK0

	arg_ptr	4,ebx
	mov	cl,byte ptr [ebx]
	mov	color,cl
	mov	cl,0h

 	OUTPUT	GRAPHICS, SET_RESET, COLOR

	arg_ptr	2,ebx          		;point to first argument y1
	mov	ax,word ptr [ebx]       ;load in ax

	arg_ptr 3,ebx                   ;point to second argument y2
	mov	cx,word ptr [ebx]       ;load in cx




	cmp	ax,cx
	jl	dvcontv                    ;ordonate
	mov	dx,ax
	mov	ax,cx
	mov	cx,dx                   ;end change
	xor	edx,edx
dvcontv:			;restore dx
	mov	ylo,ax
	mov	yhi,cx

	mov	ax,ylo
	sub	ax,ROWS
	neg	ax
	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,xbyte               	;add x1byte to result
	mov	address,ax              	;store address in addr

	mov 	ax,ylo
	add	cx,1
	sub	cx,ax
	mov	deltay,cx

	mov	ax,01fh
	mov	fs,ax

 	mov 	si,address              	;get address

	;mov	cx,deltay
	mov	bx,OFSET

	cmp	dot,22
	je	dend2
	cmp	dot,44
	je	dend4
	cmp	dot,88
	je	dend8
	mov	cx,1

;====================================
;====================================
dvrunloop:
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	sub	si,bx
	add	cx,1
	cmp	dot,0
	je	dota

	sub	si,bx
	add	cx,1
	cmp	dot,1
	je	dota

	sub	si,bx
	add	cx,1
	cmp	dot,2
	je	dota

	sub	si,bx
  	add	cx,1
 	cmp	dot,3
 	je	dota

 	sub	si,bx
  	add	cx,1
 	cmp	dot,4
 	je	dota

 	sub	si,bx
  	add	cx,1
 	cmp	dot,5
 	je	dota

 	sub	si,bx
  	add	cx,1
 	cmp	dot,6
 	je	dota

 	sub	si,bx
  	add	cx,1
 	cmp	dot,7
 	je	dota

 	sub	si,bx
  	add	cx,1

dota:
	cmp	cx,deltay
	jle 	dvrunloop
	jmp	dend

dend2:
	mov	cx,1
d22:
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	sub	si,bx
	add	cx,1
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color

	sub	si,bx
	sub	si,bx
	sub	si,bx
	add	cx,3
	cmp	cx,deltay
	jle	d22
	jmp	dend


dend4:
	mov	dx,1

d44:
	mov	cx,4
l44:

       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	add	dx,1
	cmp	dx,deltay
	jge	dend
	sub	si,bx
	loop	l44
	add	dx,4
	sub	si,bx
	sub	si,bx
	sub	si,bx
	sub	si,bx
	cmp	dx,deltay
	jle	d44
	jmp	dend

dend8:
	mov	dx,1

d88:
	mov	cx,8
l88:
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	sub	si,bx
	add	dx,1
	cmp	dx,deltay
	jge	dend
	loop	l88
	add	dx,8
	sub	si,bx
	sub	si,bx
	sub	si,bx
	sub	si,bx
	sub	si,bx
	sub	si,bx
	sub	si,bx
	sub	si,bx
	cmp	dx,deltay
	jle	d88
	jmp	dend

dend:

	pop fs
	restore_regs			;restore protected integer registers


	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 00h
	return
        end_subroutine

;****************************************************************************
;***************************************************************************
	;subroutine to be called from the main program
	;call ICIRCLE(x,y,r,color)

;==========================================================================
	subroutine    	ICIRCLE,STDFRAME,4
;===========================================================================


	save_regs			;save required registers
	push fs


	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0Fh


	arg_ptr	4,ebx
	mov	cl,byte ptr [ebx]
	mov	color,cl

 	OUTPUT	GRAPHICS, SET_RESET, CL

	xor	eax,eax
	arg_ptr	3,edx
	mov	ax,word ptr [edx]
	mov	y,ax
	shl	ax,1
	sub	ax,3
	neg	ax
	mov	incr0,ax
	xor	edi,edi
	xor	ebx,ebx
	arg_ptr	1,ecx
	mov	di,word ptr [ecx]
	xor	ebx,ebx
	arg_ptr	2,ecx
	mov	bx,word ptr [ecx]
	mov	x,0
cirloop:
	xor	edx,edx
	xor	ecx,ecx
	xor	eax,eax
	mov	ax,x
	add	ax,di           	;x+xc
	mov	xlo,ax
	mov	cx,y                    ;y+yc
	add	cx,bx
	mov	ylo,cx
        WRITEPIX  Xlo, Ylo
	xor	ecx,ecx
	mov	cx,y
	sub	cx,bx
	neg	cx
	mov	ylo,cx
        WRITEPIX  Xlo, Ylo            	;x+xc, yc-y
	xor	eax,eax
	mov   	ax,x
	sub	ax,di
	neg	ax
	mov	xlo,ax
        WRITEPIX  Xlo, Ylo              ;xc-x, yc-y
	xor	ecx,ecx
	mov   	cx,y
	add	cx,bx
	mov	ylo,cx
        WRITEPIX  Xlo, Ylo             ;xc-x, y+yc
	xor	ecx,ecx
	xor	eax,eax
	mov	ax,y
	add	ax,di
	mov	xlo,ax
	mov	cx,x
	add	cx,bx
	mov	ylo,cx
 	 WRITEPIX  Xlo, Ylo             ;y+xc; x+yc
	xor	ecx,ecx
	mov	cx,x
	sub	cx,bx
	neg	cx
	mov	ylo,cx
 	 WRITEPIX  Xlo, Ylo            ;y+yc; yc-x
	xor	eax,eax
	mov   	ax,y
	sub	ax,di
	neg	ax
  	mov	xlo,ax
 	 WRITEPIX  Xlo, Ylo            ;xc-y; yc-x
	xor	ecx,ecx
	mov   	cx,x
	add	cx,bx
	mov	ylo,cx
 	 WRITEPIX  Xlo, Ylo           ;xc-y; yc+x
	xor	eax,eax
	cmp	incr0,0
	jge	decy
	mov	ax,x
	shl	ax,1
	shl	ax,1
	add	ax,6
	add	ax,incr0
	mov	incr0,ax
	jmp	circontinue
decy:
	mov	ax,x
	sub	ax,y
	sal	ax,1
	sal	ax,1
	add	ax,10
	add	ax,incr0
	mov	incr0,ax
	dec	y
circontinue:
	xor	edx,edx
	inc	x
	mov	dx,x
	cmp	dx,y
	jle	cirloop

	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 00h
	return
        end_subroutine

;****************************************************************************
;***************************************************************************
	;THIS IS A SUBROUTINE TO FILL WITH COLOR AN ONE BYTE COLOUMN OF PIXELS
	;subroutine to be called from the main program
	;call CLEARBYTE(color)

;==========================================================================
	subroutine    	clearbyte,STDFRAME,1
;===========================================================================

	;define local storage
	;====================


	save_regs			;save required registers
	push fs
	mov	cx,0
	and	cx,7
	mov 	bl,mask20
	shr	bl,cl
	mov	mask0,bl

	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0Fh

  	OUTPUT 	GRAPHICS, BIT_MASK, MASK0

	arg_ptr	1,ebx
	mov	cl,byte ptr [ebx]
	mov	color,cl
	mov	cl,0h

 	OUTPUT	GRAPHICS, SET_RESET, COLOR

	mov	ax,0
	sub	ax,ROWS
	neg	ax
	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	mov	si,ax                   ;store address in si
	mov	ax,01fh
	mov	fs,ax


	mov	cx,ROWS
	add	cx,1
	mov	bx,OFSET
roop:
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	sub	si,bx
	loop roop

	pop fs
	restore_regs			;restore protected integer registers


	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 00h
	return
        end_subroutine


;***************************************************************************
;***************************************************************************


	;THIS IS A SUBROUTINE TO FILL WITH COLOR AN ARRAY (0:640, Y1:Y2)
	;subroutine to be called from the main program
	;call CLEARS(y1,y2,color):R.FERRARO p.516-518

;==========================================================================
	subroutine    	clears,STDFRAME,3
;===========================================================================

	;define local storage
	;====================

	begin_autos
	end_autos

	save_regs			;save required registers
	push fs
	mov	di,0000h
clsc1:

	mov	ax,di       ;load in ax
	mov	x,ax
	;calculate x1byte
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     xbyte,ax              	;store in x1byte

	mov	cx,x
	and	cx,7
	mov 	bl,mask10
	shr	bl,cl
	mov	mask0,bl

	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0Fh

  	OUTPUT 	GRAPHICS, BIT_MASK, MASK0

	arg_ptr	3,ebx
	mov	cl,byte ptr [ebx]
	mov	color,cl
	mov	cl,0h

 	OUTPUT	GRAPHICS, SET_RESET, COLOR

	;calculate address
	arg_ptr	1,ebx                   ;
	mov	ax,word ptr [ebx]       ;load in ax
	mov	ylo,ax
	sub	ax,ROWS
	neg	ax
	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,xbyte               	;add x1byte to result
	mov	address,ax              	;store address in addr
	;calculate dy
	arg_ptr	2,ebx
	mov	cx,word ptr [ebx]
	mov 	ax,ylo
	add	cx,1
	sub	cx,ax
	mov	deltay,cx

	mov	ax,01fh
	mov	fs,ax

 	mov 	si,address              	;get address

	mov	cx,deltay
	mov	bx,OFSET
rloop:
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	sub	si,bx
	loop rloop
	add	di,08h
	cmp	di,COLOUMNS
	jl	clsc1

	pop fs

	restore_regs			;restore protected integer registers


	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 00h
	return
        end_subroutine


;***************************************************************************
;***************************************************************************
;===========================================================================
;        call IPOLY(X(N),Y(N),N,COLOR)
;	-draw a polygon in color

	subroutine    	IPOLY,STDFRAME,4
;===========================================================================

	save_regs			;save required registers
	push fs

	mov	ax,01fh
	mov	fs,ax
	xor	eax,eax
	arg_ptr	3,ebx
	mov	ax,word ptr [ebx]
	sub	ax,1
	shl	ax,1
	mov	incr,eax

	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0Fh
	arg_ptr	4,ebx
	mov	cl,byte ptr [ebx]

    	OUTPUT	GRAPHICS, SET_RESET, Cl

	mov	dec_flag,0
	mov	index,0

drawpoly:
	xor	edx,edx
	arg_ptr	2,edi
	add	edi,index         		;point to second argument y1
	mov	dx,word ptr [edi]       ;load in ax
	mov	ylo,dx
	add	edi,2
	cmp	dec_flag,0
	je	modnox
	add	edi,incr
	sub	edi,2
modnox:
	mov	bx,word ptr [edi]
	mov	yhi,bx
	sub	bx,dx
	mov	deltay,bx                  ;deltay=y2-y1
	cmp	bx,0
	jg	dlin2
	neg	bx                        ;bx=dy

dlin2:
	mov	adeltay,bx                   ;adeltay=|y2-y1|
	xor	eax,eax
	arg_ptr	1,edi          		;point to first argument x1
	add	edi,index         		;point to second argument y1
	mov	ax,word ptr [edi]       ;load in ax
	mov	xlo,ax
	add	edi,2
	cmp	dec_flag,0
	je	modnoy
	add	edi,incr
	sub	edi,2
modnoy:
	mov	di,word ptr [edi]
	mov	xhi,di
	sub	di,ax                     ;di=dx
	mov	deltax,di                   ;deltax=x2-x1
	cmp	di,0        		  ;x2>x1?
	jg	dlin1
	xor	ecx,ecx
	mov	cx,xhi                    ;x2<x1
	mov	x,cx                     ;x=x2
	xor	ecx,ecx
	mov	cx,yhi
	mov	y,cx                     ;y=y2
	neg	di                        ;di=|x2-x1|=dx
	mov	incr0,80             ;y2<y1; inc=+80
	mov	cx,deltay
	cmp	cx,0                    ;y2>y1?
	jg	dlin3
	neg	incr0              ;inc=-80
dlin3:
	jmp	dlinor

dlin1:                                     ;x2>x1
	mov	x,ax                     ;x=x1
	mov	y,dx                     ;y=y1
	mov	incr0,80             ;y2<y1; inc=+80
	cmp	deltay,0                    ;y2>y1?
	jle	dlinor
	neg	incr0                ;y2>y1 inc=-80

dlinor:
	mov	adeltax,di                    ;adeltax=|x2-x1|

	;calculate x>>3 for address
	mov	ax,x
	shr	ax,1
	shr	ax,1
	shr	ax,1
	mov	xbyte,ax


	;calculate mask
	mov	cx,x
	and	cx,7
      	mov 	al,080h
      	shr	al,cl
	mov	mask0,al

	sub	di,bx                	;dx-dy>0? di=dx-dy
	cmp	di,0
	jle	dlin5

;xline:                                 ;dx>dy

	mov	ax,bx                   ;ax=dy
	shl	ax,1                    ;2*dy
	mov	incr1,ax               ;incr1=2*dy
	sub	ax,adeltax                  ;2*dy-dx
	mov	delta,ax                   ;d=2*dy-dx
	neg	di                      ;di=dy-dx
	shl	di,1                    ;2*(dy-dx)
	mov	incr2,di               ;incr2=2*(dy-dx)
	xor	edi,edi
	;calculate address
	C_ADDRESS  y,xbyte

	inc	adeltax
  	OUTPUT 	GRAPHICS, BIT_MASK, 8h
	mov	al,mask0
	mov	cx,adeltax
	mov	bx,delta
	xor 	edi,edi
	mov	di,incr0
dlloop1:
	out	dx,al
	mov	ah,fs:[si]
	mov	fs:[si],ah
	shr	al,1
	jnc	dsame
	mov	al,80h
	inc	si
dsame:
	cmp	bx,0
	jl	dnoinc1
	add	bx,incr2
	add	si,di
	jmp	dcheck1
dnoinc1:
	add	bx,incr1
dcheck1:
	loop	dlloop1
	jmp	dlend

	;start code for dx<dy
dlin5:
	mov	ax,adeltax
	shl	ax,1
	mov	incr1,ax    	;incr1=2*dx
	sub	ax,adeltay          ;ax=2*dx-dy
	mov	delta,ax           ;d=2*dx-dy
	shl	di,1
	mov	incr2,di       ;incr2=2*(dx-dy)
	mov	di,80
	neg	di
	mov	incr0,di
	inc	adeltay
	cmp	deltay,0          ;y2-y1>0?
	jg	dlin51

	; here for y1>y2
	mov	ax,xhi          ;y2<y1 x=x2,y=y2
	shr	ax,1
	shr	ax,1
	shr	ax,1
	mov	xbyte,ax

	;calculate address
	C_ADDRESS  yhi,xbyte
	;calculate mask
	mov	cx,xhi
	and	cx,7
      	mov 	al,080h
      	shr	al,cl
	mov	mask0,al

	cmp	deltax,0           ;x2-x1>0?
	jle	dypline           ;x2<=x1
	jmp	dynline           ;x2>x1

dlin51:
	mov	ax,xlo
	shr	ax,1
	shr	ax,1
	shr	ax,1
	mov	xbyte,ax

	;calculate address
	C_ADDRESS  ylo,xbyte
	;calculate mask
	mov	cx,xlo
	and	cx,7
      	mov 	al,080h
      	shr	al,cl
	mov	mask0,al
	cmp	deltax,0
	jle	dynline

dypline:
  	OUTPUT 	GRAPHICS, BIT_MASK, 8h
	mov	cx,adeltay
	mov	bx,delta
	mov	al,mask0
dlloop3:
	out	dx,al
	mov	ah,fs:[si]
	mov	fs:[si],ah
	add	si,incr0
	cmp	bx,0
	jl	dnoinc3
	add	bx,incr2
	shr	al,1
	jnc	dcheck3
	mov	al,080h
	inc	si
	jmp	dcheck3
dnoinc3:
	add	bx,incr1
dcheck3:
	loop	dlloop3

	jmp	dlend

dynline:
  	OUTPUT 	GRAPHICS, BIT_MASK, 8h
	mov	cx,adeltay
	mov	bx,delta
	mov	al,mask0
dlloop2:
	out	dx,al
	mov	ah,fs:[si]
	mov	fs:[si],ah
	add	si,incr0
	cmp	bx,0
	jl	dnoinc2
	add	bx,incr2
	shl	al,1
	jnc	dcheck2
	mov	al,01h
	dec	si
	jmp	dcheck2
dnoinc2:
	add	bx,incr1
dcheck2:
	loop	dlloop2
	jmp	dlend
dlend:
	cmp     dec_flag,1
	je	realend
	add	index,2
	xor	ebx,ebx
	mov	ebx,index
	cmp	ebx,incr
	jl	drawpoly
	mov	index,0
	mov	dec_flag,1
	jmp	drawpoly
realend:
	pop fs

	restore_regs			;restore protected integer registers


 	OUTPUT 	GRAPHICS, MAP_MASK, 0Fh
	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 00h
	return
        end_subroutine



;***************************************************************************
;***************************************************************************


	;THIS IS A SUBROUTINE TO FILL WITH COLOR A RECTANGULAR ARRAY
	;subroutine to be called from the main program
	;call FILLWIN(x1,y1,x2,y2,color)

;==========================================================================
	subroutine    	FILLWIN,STDFRAME,5
;===========================================================================

	;define local storage
	;====================
	save_regs			;save required registers
	push fs
;========================
	mov	dl,1
	mov     mapmask,dl
;=============================
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0Fh
	arg_ptr	5,ebx
	mov	cl,byte ptr [ebx]


    	OUTPUT	GRAPHICS, SET_RESET, CL


	arg_ptr	1,ebx          		;point to first argument x1
	mov	ax,word ptr [ebx]       ;load in ax

	arg_ptr 3,ebx                   ;point to THIRD argument x2
	mov	cx,word ptr [ebx]       ;load in cx

	cmp	ax,cx
	jl	ffcont                    ;ordonate
	mov	dx,ax
	mov	ax,cx
	mov	cx,dx                   ;end change

ffcont:			;restore dx
	mov	xlo,ax
	mov	xhi,cx
	mov	dx,00h

	add	cx,1
	sub	cx,ax
	shr	cx,1
	shr	cx,1
	shr	cx,1
	add	cx,1
	mov	bytes_per_line,cx              ;bytes/row:(x2-x1+1)>>3

;	LAST IMPROVMENT:
;===========================================
	xor	ebx,ebx
	xor	edx,edx
	;calculate x1 modulo 8
	mov 	ax,xlo			;mov x1 in ax
	mov	bx,8                    ;divide by 8
	div	bx
	mov	modxlo,dl		;store result in temp2
	xor 	dx,dx
	xor 	bx,bx
	mov	ax,00h
	;calculate x2 modulo 8
	mov 	ax,xhi			;mov x2 in ax
	mov	bx,8                    ;divide by 8
	div	bx
	mov	modxhi,dl		;store result in temp2


	cmp	modxlo,0
	jg	ex0
	cmp	modxhi,7
	jl	nochange
	sub	bytes_per_line,1
	jmp	nochange
ex0:
	cmp	modxlo,1
	je	nochange
ex1:
	cmp	modxlo,2
	jg	ex2
	cmp	modxhi,0
	jg	nochange
  	add	bytes_per_line,1
	jmp	nochange
ex2:
	cmp	modxlo,3
	jg	ex3
	cmp	modxhi,1
	jg	nochange
   	add	bytes_per_line,1
	jmp	nochange
ex3:
	cmp	modxlo,4
	jg	ex4
	cmp	modxhi,2
	jg	nochange
    	add	bytes_per_line,1
	jmp	nochange
ex4:
	cmp	modxlo,5
	jg	ex5
	cmp	modxhi,3
	jg	nochange
    	add	bytes_per_line,1
	jmp	nochange
ex5:
	cmp	modxlo,6
	jg	ex6
	cmp	modxhi,4
	jg	nochange
      	add	bytes_per_line,1
	jmp	nochange
ex6:
	cmp	modxhi,5
	jg	nochange
      	add	bytes_per_line,1

;===========================================
nochange:
	xor 	dx,dx
	xor	eax,eax
	;calculate xbyte
	mov	ax,xlo
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     xbyte,ax              	;store in xbyte

	xor	eax,eax
	arg_ptr	2,ebx          		;point to first argument x1
	mov	ax,word ptr [ebx]       ;load in ax

	arg_ptr 4,ebx                   ;point to second argument x2
	mov	cx,word ptr [ebx]       ;load in cx
	cmp	ax,cx
	jl	ffcont1                    ;ordonate
	mov	dx,ax
	mov	ax,cx
	mov	cx,dx                   ;end change

ffcont1:
	mov	ylo,ax
	mov	dx,00h
	sub	ax,ROWS
	neg	ax
	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,xbyte              ;add xbyte to result
	mov	address,ax



;==================================
;	mov	si,address
;=================================
	xor	eax,eax
	xor	edi,edi

	mov 	ax,ylo
	add	cx,1
	sub	cx,ax
	mov	num_rows,cx

	xor	cx,cx                   ;calculate mask first byte
      	mov	cx,xlo
      	and	cx,7
      	mov 	bl,0ffh
      	shr	bl,cl
      	mov	mask1,bl
	xor	cx,cx
      	mov	cx,xhi                   ;calculate mask last byte
      	and	cx,7
      	mov 	bx,0ff80h
      	shr	bx,cl
      	mov	mask2,bl

;===============================
biw:
	mov	si,address
	OUTPUT SEQUENCER, MAP_MASK, mapmask

;===============================

	mov	ax,01fh
	mov	fs,ax
	xor	dx,dx
	xor	cx,cx
	xor	bx,bx
	mov	dx,num_rows
	mov	bx,OFSET
	sub	bx,bytes_per_line
	mov	cx,01h

fclall:                              	;big loop after number bytes/row
	cmp	cx,01                   ;after filling a byte coloumn
	je	fnxt1
   	cmp	cx,bytes_per_line
   	jne	fnxt3
   	OUTPUT 	GRAPHICS, BIT_MASK, mask2
   	jmp	fnxt2
fnxt1:
  	OUTPUT 	GRAPHICS, BIT_MASK, mask1
	jmp	fnxt2

fnxt3:
  	OUTPUT 	GRAPHICS, BIT_MASK, 0ffh
fnxt2:
	mov	bx,num_rows

fclwin:                                  ;small loop after number rows
        mov	al,fs:[si]              ;write a byte and increase row
        mov	fs:[si],al              ;output color
	sub	si,OFSET
	dec 	bx
	jg	fclwin


	mov	si,address
	add 	si,cx
	inc	cx
	cmp	cx,bytes_per_line
	jle	fclall

;===============================
	mov	dl,mapmask
	shl	dl,1
	mov	mapmask,dl
	cmp	dl,8
	jle	biw
;===============================
	pop fs

	restore_regs			;restore protected integer registers

 	OUTPUT 	GRAPHICS, MAP_MASK, 0Fh
	OUTPUT SEQUENCER, MAP_MASK, 0fh
	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 00h
	return
        end_subroutine


;***************************************************************************
;***************************************************************************

	;READS A RECTANGULAR ARRAY
	;subroutine to be called from the main program
	;call RDGWIN1(x1,y1,x2,y2,buf)
;==========================================================================
	subroutine     RDGWIN1,STDFRAME,5
;==========================================================================
	;define local storage
	save_regs	;save required registers
	push 	es
;======================================================================
	;load and store arguments
;======================================================================



	arg_ptr	1,ebx          		;point to first argument x1
	mov	ax,word ptr [ebx]       ;load in ax
	arg_ptr	3,edx                    ;x2
	mov	cx,word ptr [edx]
	cmp	ax,cx
	jl	rfcont                    ;ordinate
	mov	dx,ax
	mov	ax,cx
	mov	cx,dx                   ;end change

rfcont:
	mov	xlo,ax
	mov	xhi,cx
	mov	dx,00h


	add	cx,1
	sub	cx,ax
	shr	cx,1
	shr	cx,1
	shr	cx,1
	add	cx,1
	mov	bytes_per_line,cx              ;bytes/row:(x2-x1+1)>>3
;===========================================
	xor	ebx,ebx
	xor	edx,edx
	;calculate x1 modulo 8
	mov 	ax,xlo			;mov x1 in ax
	mov	bx,8                    	;divide by 8
	div	bx
	mov	modxlo,dl			;store result
	xor 	dx,dx
	xor 	bx,bx
	mov	ax,00h
	;calculate x2 modulo 8
	mov 	ax,xhi			;mov x2 in ax
	mov	bx,8                    	;divide by 8
	div	bx
	mov	modxhi,dl			;store result


	cmp	modxlo,0
	jg	ex0r
	cmp	modxhi,7
	jl	nochanger
	sub	bytes_per_line,1
	jmp	nochanger
ex0r:
	cmp	modxlo,1
	je	nochanger
ex1r:
	cmp	modxlo,2
	jg	ex2r
	cmp	modxhi,0
	jg	nochanger
  	add	bytes_per_line,1
	jmp	nochanger
ex2r:
	cmp	modxlo,3
	jg	ex3r
	cmp	modxhi,1
	jg	nochanger
   	add	bytes_per_line,1
	jmp	nochanger
ex3r:
	cmp	modxlo,4
	jg	ex4r
	cmp	modxhi,2
	jg	nochanger
    	add	bytes_per_line,1
	jmp	nochanger
ex4r:
	cmp	modxlo,5
	jg	ex5r
	cmp	modxhi,3
	jg	nochanger
    	add	bytes_per_line,1
	jmp	nochanger
ex5r:
	cmp	modxlo,6
	jg	ex6r
	cmp	modxhi,4
	jg	nochanger
      	add	bytes_per_line,1
	jmp	nochanger
ex6r:
	cmp	modxhi,5
	jg	nochanger
      	add	bytes_per_line,1

;===========================================
nochanger:

	xor	eax,eax
	xor	edx,edx
	;calculate xbyte
	mov	ax,xlo
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     xbyte,ax              ;store in xbyte

	xor	eax,eax
	arg_ptr	2,ecx          		;point to first argument x1
	mov	cx,word ptr [ecx]       ;load in ax

	arg_ptr 4,eax                   ;point to second argument x2
	mov	ax,word ptr [eax]       ;load in cx
	cmp	cx,ax
	jl	fct1                    ;ordonate
	mov	dx,cx
	mov	cx,ax
	mov	ax,dx                   ;end change

fct1:
	mov	yhi,ax
	mov	ylo,cx
	sub	ax,ROWS
	neg	ax
	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,xbyte             ;add xbyte to result
	mov	address,ax             ;address

	mov 	ax,yhi
	add	ax,1
	sub	ax,cx
	mov	num_rows,ax                ;nr rows
	xor	eax,eax
	xor	edx,edx
	mov	ax,bytes_per_line

	mov	dx,num_rows
	mul	edx
	mov     incr,eax
	mov	al,0
	mov	readmap,al
	arg_ptr	5,ebx

bigjump:
	OUTPUT GRAPHICS,READ_MAP_SELECT,readmap

	mov	ax,01fh
	mov	fs,ax
	mov	si,address
      	mov	dx,num_rows
       	mov	ax,80
       	mov	cx,bytes_per_line
	sub	ax,cx
	mov	incr0,ax
	mov	temp0,cl
	xor	ch,ch

lup11:
	mov 	cl,temp0
;============================================================
l33:
	mov	ah,fs:[si]                      ;MOVE DISPLAY TO BUFFER
	mov	byte ptr [ebx],ah
	add	si,1
	add	ebx,1
	dec	cl
	jnz	l33
;============================================================
	add	si,incr0
	dec	dx
	jg	lup11
	inc	readmap
	cmp	readmap,3
	jle	bigjump
endrg:
	pop fs
	restore_regs			;restore protected integer registers


	return
        end_subroutine

;*************************************************************************
;*************************************************************************
	;WRITES A GRAPHIC WINDOW
	;subroutine to be called from the main program
	;call WRGWIN1(x1,y1,x2,y2,buf)
;==========================================================================
	subroutine     wrGWIN1,STDFRAME,5
;==========================================================================
	save_regs	;save required registers
	push 	es
;======================================================================
	;load and store arguments
;======================================================================

	arg_ptr	1,ebx          		;point to first argument x1
	mov	ax,word ptr [ebx]       ;load in ax
	arg_ptr	3,edx                    ;x2
	mov	cx,word ptr [edx]
	cmp	ax,cx
	jl	rfcont1                    ;ordonate
	mov	dx,ax
	mov	ax,cx
	mov	cx,dx                   ;end change

rfcont1:
	mov	xlo,ax
	mov	xhi,cx
	mov	dx,00h

	add	cx,1
	sub	cx,ax
	shr	cx,1
	shr	cx,1
	shr	cx,1
	add	cx,1
	mov	bytes_per_line ,cx              ;bytes/row:(x2-x1+1)>>3
;===========================================
	xor	ebx,ebx
	xor	edx,edx
	;calculate x1 modulo 8
	mov 	ax,xlo		;mov x1 in ax
	mov	bx,8                    ;divide by 8
	div	bx
	mov	modxlo,dl		;store result
	xor 	dx,dx
	xor 	bx,bx
	mov	ax,00h
	;calculate x2 modulo 8
	mov 	ax,xhi		;mov x2 in ax
	mov	bx,8                    ;divide by 8
	div	bx
	mov	modxhi,dl		;store result in


	cmp	modxlo,0
	jg	ex0w
	cmp	modxhi,7
	jl	nochangew
	sub	bytes_per_line ,1
	jmp	nochangew
ex0w:
	cmp	modxlo,1
	je	nochangew
ex1w:
	cmp	modxlo,2
	jg	ex2w
	cmp	modxhi,0
	jg	nochangew
  	add	bytes_per_line ,1
	jmp	nochangew
ex2w:
	cmp	modxlo,3
	jg	ex3w
	cmp	modxhi,1
	jg	nochangew
   	add	bytes_per_line ,1
	jmp	nochangew
ex3w:
	cmp	modxlo,4
	jg	ex4w
	cmp	modxhi,2
	jg	nochangew
    	add	bytes_per_line ,1
	jmp	nochangew
ex4w:
	cmp	modxlo,5
	jg	ex5w
	cmp	modxhi,3
	jg	nochangew
    	add	bytes_per_line ,1
	jmp	nochangew
ex5w:
	cmp	modxlo,6
	jg	ex6w
	cmp	modxhi,4
	jg	nochangew
      	add	bytes_per_line ,1
	jmp	nochangew
ex6w:
	cmp	modxhi,5
	jg	nochangew
      	add	bytes_per_line ,1

;===========================================
nochangew:


	xor	eax,eax
	xor	edx,edx
	;calculate xbyte
	mov	ax,xlo
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     xbyte,ax            ;store

	xor	eax,eax
	arg_ptr	2,ebx          		;point to first argument x1
	mov	cx,word ptr [ebx]       ;load in ax

	arg_ptr 4,ebx                   ;point to second argument x2
	mov	ax,word ptr [ebx]       ;load in cx
	cmp	cx,ax
	jl	fact1                    ;ordonate
	mov	dx,cx
	mov	cx,ax
	mov	ax,dx                   ;end change

fact1:
	mov	yhi,ax
	mov	ylo,cx

	sub	ax,ROWS
	neg	ax
	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,xbyte           ;add xbyte to result
	mov	address,ax            ;address

	mov 	ax,yhi
	add	ax,1
	sub	ax,cx
	mov	num_rows,ax               ;nr rows

	xor	eax,eax
	xor	edx,edx
	mov	ax,bytes_per_line
	mov	dx,num_rows
	mul	edx
	mov	incr,eax
	mov	al,1
	mov	mapmask,al
    	;mov	di,24[ebp]
	arg_ptr	5,ebx

bigjumpw:
	OUTPUT SEQUENCER, MAP_MASK, mapmask

	mov	ax,01fh
	mov	fs,ax
	mov	si,address
      	mov	dx,num_rows
       	mov	ax,80
       	mov	cx,bytes_per_line
	sub	ax,cx
	mov	incr0,ax
	mov	temp0,cl
	xor	ch,ch

lup11w:
	mov	cl,temp0
;============================================================
l33w:
	mov	ah,byte ptr [ebx]
	mov	fs:[si],ah         	;MOVE BUFFER TO DISPLAY
	add	si,1
	add	ebx,1
	dec	cl
	jnz	l33w
;============================================================
	add	si,incr0
	dec	dx
	jg	lup11w
	mov	dl,mapmask
	shl	dl,1
	mov	mapmask,dl
	cmp	dl,8
	jle	bigjumpw

endwg:
	pop fs
	restore_regs			;restore protected integer registers

	OUTPUT SEQUENCER, MAP_MASK, 0fh

	return
        end_subroutine
;*************************************************************************
;*************************************************************************
	;WRITES A GRAPHIC WINDOW
	;subroutine to be called from the main program
	;call WRGWIN2(x1,y1,x2,y2,buf)
;==========================================================================
	subroutine     wrGWIN2,STDFRAME,5
;==========================================================================
	save_regs	;save required registers
	push 	es
;======================================================================
	;load and store arguments
;======================================================================

	arg_ptr	1,ebx          		;point to first argument x1
	mov	ax,word ptr [ebx]       ;load in ax
	arg_ptr	3,edx                    ;x2
	mov	cx,word ptr [edx]
	cmp	ax,cx
	jl	rfcont12                    ;ordonate
	mov	dx,ax
	mov	ax,cx
	mov	cx,dx                   ;end change

rfcont12:
	mov	xlo,ax
	mov	xhi,cx
	mov	dx,00h

	add	cx,1
	sub	cx,ax
	shr	cx,1
	shr	cx,1
	shr	cx,1
	add	cx,1
	mov	bytes_per_line ,cx              ;bytes/row:(x2-x1+1)>>3
;===========================================
	xor	ebx,ebx
	xor	edx,edx
	;calculate x1 modulo 8
	mov 	ax,xlo		;mov x1 in ax
	mov	bx,8                    ;divide by 8
	div	bx
	mov	modxlo,dl		;store result
	xor 	dx,dx
	xor 	bx,bx
	mov	ax,00h
	;calculate x2 modulo 8
	mov 	ax,xhi		;mov x2 in ax
	mov	bx,8                    ;divide by 8
	div	bx
	mov	modxhi,dl		;store result in


	cmp	modxlo,0
	jg	ex0w2
	cmp	modxhi,7
	jl	nochangew2
	sub	bytes_per_line ,1
	jmp	nochangew2
ex0w2:
	cmp	modxlo,1
	je	nochangew2
ex1w2:
	cmp	modxlo,2
	jg	ex2w2
	cmp	modxhi,0
	jg	nochangew2
  	add	bytes_per_line ,1
	jmp	nochangew2
ex2w2:
	cmp	modxlo,3
	jg	ex3w2
	cmp	modxhi,1
	jg	nochangew2
   	add	bytes_per_line ,1
	jmp	nochangew2
ex3w2:
	cmp	modxlo,4
	jg	ex4w2
	cmp	modxhi,2
	jg	nochangew2
    	add	bytes_per_line ,1
	jmp	nochangew2
ex4w2:
	cmp	modxlo,5
	jg	ex5w2
	cmp	modxhi,3
	jg	nochangew2
    	add	bytes_per_line ,1
	jmp	nochangew2
ex5w2:
	cmp	modxlo,6
	jg	ex6w2
	cmp	modxhi,4
	jg	nochangew2
      	add	bytes_per_line ,1
	jmp	nochangew2
ex6w2:
	cmp	modxhi,5
	jg	nochangew2
      	add	bytes_per_line ,1

;===========================================
nochangew2:


	xor	eax,eax
	xor	edx,edx
	;calculate xbyte
	mov	ax,xlo
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     xbyte,ax            ;store

	xor	eax,eax
	arg_ptr	2,ebx          		;point to first argument x1
	mov	cx,word ptr [ebx]       ;load in ax

	arg_ptr 4,ebx                   ;point to second argument x2
	mov	ax,word ptr [ebx]       ;load in cx
	cmp	cx,ax
	jl	fact12                    ;ordonate
	mov	dx,cx
	mov	cx,ax
	mov	ax,dx                   ;end change

fact12:
	mov	yhi,ax
	mov	ylo,cx

	sub	ax,ROWS
	neg	ax
	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,xbyte           ;add xbyte to result
	mov	address,ax            ;address

	mov 	ax,yhi
	add	ax,1
	sub	ax,cx
	mov	num_rows,ax               ;nr rows

	xor	eax,eax
	xor	edx,edx
	mov	ax,bytes_per_line
	mov	dx,num_rows
	mul	edx
	mov	incr,eax
	mov	al,1
	mov	mapmask,al
    	;mov	di,24[ebp]
	arg_ptr	5,ebx

bigjumpw2:
	OUTPUT SEQUENCER, MAP_MASK, mapmask

	mov	ax,01fh
	mov	fs,ax
	mov	si,address
      	mov	dx,num_rows
       	mov	ax,80
       	mov	cx,bytes_per_line
	sub	ax,cx
	mov	incr0,ax
	mov	temp0,cl
	xor	ch,ch

lup11w2:
	mov	cl,temp0
;============================================================
l33w2:
	mov	ah,byte ptr [ebx]
	cmp	ah,255
	jne	cont2
      ;	cmp 	ah,0
      ;	je     cont2
	mov	fs:[si],ah         	;MOVE BUFFER TO DISPLAY
cont2:
	add	si,1
	add	ebx,1
	dec	cl
	jnz	l33w2
;============================================================
	add	si,incr0
	dec	dx
	jg	lup11w2
	mov	dl,mapmask
	shl	dl,1
	mov	mapmask,dl
	cmp	dl,8
	jle	bigjumpw2

endwg2:
	pop fs
	restore_regs			;restore protected integer registers

	OUTPUT SEQUENCER, MAP_MASK, 0fh

	return
        end_subroutine

;*************************************************************************
;*************************************************************************

	;subroutine to be called from the main program
	;call FILLPOLY(x,y,n,color,color_fill)
	;draw a filled polygon
;==========================================================================
	subroutine    	FILLPOLY,STDFRAME,5
;==========================================================================

	save_regs			;save required registers
	push fs

	arg_ptr	5,edx
	mov	cx,word ptr [edx]
	mov	color,cl
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0Fh
	arg_ptr	4,edx
	xor	ecx,ecx
	mov	cx,word ptr [edx]
	mov	bkcolor,cl

    	OUTPUT	GRAPHICS, SET_RESET, Cl

	;find_top

	xor	ecx,ecx
	arg_ptr	3,edx
	mov	cx,word ptr [edx]
	mov	n_dim,cx
	dec	cx
	arg_ptr 2,edi

	mov	ax,word ptr [edi]
	mov	y_top,ax
ntop:
	add	edi,2
	mov	bx,word ptr [edi]
	cmp	bx,y_top
	jle	ntcont
	mov	y_top,bx
ntcont:
	loop	ntop

	;find_bottom
	arg_ptr 2,edi

	mov	ax,word ptr [edi]
	mov	y_bottom,ax
	mov	cx,n_dim
	dec	cx
nbottom:
	add	edi,2
	mov	bx,word ptr [edi]
	cmp	bx,y_bottom
	jge	nbcont
	mov	y_bottom,bx
nbcont:
	loop	nbottom

	;find_right
	arg_ptr 1,esi

	mov	ax,word ptr [esi]
	mov	x_right ,ax
	mov	cx,n_dim
	dec	cx
npright:
	add	esi,2
	mov	ax,word ptr [esi]
	cmp	ax,x_right
	jle	nrcont
	mov	x_right ,ax
nrcont:
	loop	npright

	;find left
	;x_left,y_left starting pont for scaning up and down
	arg_ptr 1,esi

	mov	bx,word ptr [esi]
	mov	x_left ,bx
	mov	cx,n_dim
	dec	cx
npleft:
	add	esi,2
	mov	bx,word ptr [esi]
	cmp	bx,x_left
	jge	nlcont
	mov	x_left  ,bx
nlcont:
	loop	npleft

	xor	esi,esi
	xor	edi,edi

	;now: bx = ix_left , ax = niy_left   , dx = ix_right


	mov	ax,01fh
	mov	fs,ax
	mov	ax,n_dim
	sub	ax,1
	shl	ax,1
	mov	incr,eax


	mov	dec_flag,0
	mov	index,0

fdrawpoly:
	xor	edx,edx
	arg_ptr	2,edi
	add	edi,index         		;point to second argument y1
	mov	dx,word ptr [edi]       ;load in ax
	mov	ylo,dx
	add	edi,2
	cmp	dec_flag,0
	je	fmodnox
	add	edi,incr
	sub	edi,2
fmodnox:
	mov	bx,word ptr [edi]
	mov	yhi,bx
	sub	bx,dx
	mov	deltay,bx                  ;deltay1=y2-y1
	cmp	bx,0
	jg	fdlin2
	neg	bx                        ;bx=dy

fdlin2:
	mov	adeltay,bx                   ;adeltay=|y2-y1|
	xor	eax,eax
	arg_ptr	1,edi          		;point to first argument x1
	add	edi,index
	mov	ax,word ptr [edi]       ;load in ax
	mov	xlo,ax
	add	edi,2
	cmp	dec_flag,0
	je	fmodnoy
	add	edi,incr
	sub	edi,2
fmodnoy:
	mov	di,word ptr [edi]
	mov	xhi,di
	sub	di,ax                     ;di=dx
	mov	deltax,di                   ;deltax=x2-x1
	cmp	di,0        		  ;x2>x1?
	jg	fdlin1
	xor	ecx,ecx
	mov	cx,xhi                    ;x2<x1
	mov	x,cx                     ;x=x2
	xor	ecx,ecx
	mov	cx,yhi
	mov	y,cx                     ;y=y2
	neg	di                        ;di=|x2-x1|=dx
	mov	incr0,80             ;y2<y1; inc=+80
	mov	cx,deltay
	cmp	cx,0                    ;y2>y1?
	jg	fdlin3
	neg	incr0              ;inc=-80
fdlin3:
	jmp	fdlinor

fdlin1:                                     ;x2>x1
	mov	x,ax                     ;x=x1
	mov	y,dx                     ;y=y1
	mov	incr0,80             ;y2<y1; inc=+80
	cmp	deltay,0                    ;y2>y1?
	jle	fdlinor
	neg	incr0                ;y2>y1 inc=-80

fdlinor:
	mov	adeltax,di                    ;adeltax=|x2-x1|

	;calculate x>>3 for address
	mov	ax,x
	shr	ax,1
	shr	ax,1
	shr	ax,1
	mov	xbyte,ax


	;calculate mask
	mov	cx,x
	and	cx,7
      	mov 	al,080h
      	shr	al,cl
	mov	mask0,al

	sub	di,bx                	;dx-dy>0? di=dx-dy
	cmp	di,0
	jle	fdlin5

;xline:                                 ;dx>dy

	mov	ax,bx                   ;ax=dy
	shl	ax,1                    ;2*dy
	mov	incr1,ax               ;incr1=2*dy
	sub	ax,adeltax                  ;2*dy-dx
	mov	delta,ax                   ;d=2*dy-dx
	neg	di                      ;di=dy-dx
	shl	di,1                    ;2*(dy-dx)
	mov	incr2,di               ;incr2=2*(dy-dx)
	xor	edi,edi
	;calculate address
	C_ADDRESS  y,xbyte

	inc	adeltax
  	OUTPUT 	GRAPHICS, BIT_MASK, 8h
	mov	al,mask0
	mov	cx,adeltax
	mov	bx,delta
	xor 	edi,edi
	mov	di,incr0
fdlloop1:
	out	dx,al
	mov	ah,fs:[si]
	mov	fs:[si],ah
	shr	al,1
	jnc	fdsame
	mov	al,80h
	inc	si
fdsame:
	cmp	bx,0
	jl	fdnoinc1
	add	bx,incr2
	add	si,di
	jmp	fdcheck1
fdnoinc1:
	add	bx,incr1
fdcheck1:
	loop	fdlloop1
	jmp	fdlend

	;start code for dx<dy
fdlin5:
	mov	ax,adeltax
	shl	ax,1
	mov	incr1,ax    	;incr1=2*dx
	sub	ax,adeltay          ;ax=2*dx-dy
	mov	delta,ax           ;d=2*dx-dy
	shl	di,1
	mov	incr2,di       ;incr2=2*(dx-dy)
	mov	di,80
	neg	di
	mov	incr0,di
	inc	adeltay
	cmp	deltay,0          ;y2-y1>0?
	jg	fdlin51

	; here for y1>y2
	mov	ax,xhi          ;y2<y1 x=x2,y=y2
	shr	ax,1
	shr	ax,1
	shr	ax,1
	mov	xbyte,ax

	;calculate address
	C_ADDRESS  yhi,xbyte
	;calculate mask
	mov	cx,xhi
	and	cx,7
      	mov 	al,080h
      	shr	al,cl
	mov	mask0,al

	cmp	deltax,0           ;x2-x1>0?
	jle	fdypline           ;x2<=x1
	jmp	fdynline           ;x2>x1

fdlin51:
	mov	ax,xlo
	shr	ax,1
	shr	ax,1
	shr	ax,1
	mov	xbyte,ax

	;calculate address
	C_ADDRESS  ylo,xbyte
	;calculate mask
	mov	cx,xlo
	and	cx,7
      	mov 	al,080h
      	shr	al,cl
	mov	mask0,al
	cmp	deltax,0
	jle	fdynline

fdypline:
  	OUTPUT 	GRAPHICS, BIT_MASK, 8h
	mov	cx,adeltay
	mov	bx,delta
	mov	al,mask0
fdlloop3:
	out	dx,al
	mov	ah,fs:[si]
	mov	fs:[si],ah
	add	si,incr0
	cmp	bx,0
	jl	fdnoinc3
	add	bx,incr2
	shr	al,1
	jnc	fdcheck3
	mov	al,080h
	inc	si
	jmp	fdcheck3
fdnoinc3:
	add	bx,incr1
fdcheck3:
	loop	fdlloop3

	jmp	fdlend

fdynline:
  	OUTPUT 	GRAPHICS, BIT_MASK, 8h
	mov	cx,adeltay
	mov	bx,delta
	mov	al,mask0
fdlloop2:
	out	dx,al
	mov	ah,fs:[si]
	mov	fs:[si],ah
	add	si,incr0
	cmp	bx,0
	jl	fdnoinc2
	add	bx,incr2
	shl	al,1
	jnc	fdcheck2
	mov	al,01h
	dec	si
	jmp	fdcheck2
fdnoinc2:
	add	bx,incr1
fdcheck2:
	loop	fdlloop2
	jmp	fdlend
fdlend:
	cmp     dec_flag,1
	je	here
	add	index,2
	xor	ebx,ebx
	mov	ebx,index
	cmp	ebx,incr
	jl	fdrawpoly
	mov	index,0
	mov	dec_flag,1
	jmp	fdrawpoly


here:
 	OUTPUT 	GRAPHICS, MAP_MASK, 0Fh
	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 00h

	mov	bx,x_left
	add	bx,1
	mov	x_left,bx
	mov	x,bx
;begin the big loop

;begin the up scanning to find intersection(s) with border
nscdraw:
	mov	temp0,0
	mov	di,y_bottom
nreadpixel:
	mov	ax,di
	mov	bx,x
	mov	cl,bl			;in cl low order byte of x

	;calculate xbyte
	shr	bx,1
	shr	bx,1
	shr	bx,1

	xor	edx,edx
	sub	ax,ROWS
	neg	ax
      	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	bx,ax               	;add xbyte to result :address
	add	bx,0              	;store address in address
	mov	si,bx                   ;address in si

	mov	ax,01fh
	mov	fs,ax

	and 	cl,7             	;cl = x&7
	xor	cl,7                    ;cl = nr of bits to shift left
	mov	ah,1                    ;unshifted bit mask
					;ah=011fh
	mov	ch,ah                   ;ch=1
	shl	ch,cl        		;bit mask in proper position
					;ch=?

	xor 	bl,bl                   ;bl used to accumulate the pixel value
	mov	dx,3ceh                 ;dx= grphics i/o port_addr
	mov	ax,304h                 ;ah = initial bit plane number
					;al = READ_MAP_SELECT
npl01:	out	dx,ax                   ;select bit plane
	mov	bh,fs:[si]              ;bh = byte from the current bit plane
					;!!! always 0 ???????
	and	bh,ch                   ;mask one bit
	neg	bh                      ;bit 7 of bh = 1 (if masked bit = 1)
			                ;bit 7 of bh = 1 (if masked bit = 1)

	rol	bx,1                    ; bit 0 of bl = next bit from pixel value
	dec	ah                      ;next bit plane number
	jge	npl01

	mov	al,bl                   ;al = pixel value
	xor	ah,ah                   ;ax = pixel value
	cmp	al,bkcolor
	jne     npjh
	push	di
	add     temp0,1
npjh:

	add	di,1
	cmp 	di,y_top
	jle	nreadpixel
nnowcomp:

	cmp     temp0,0
	je      npend
	cmp	temp0,1
	jg      ncmp2
	pop	di
	jmp     npend
ncmp2:
	cmp	temp0,2
	jg      ncmp3
	pop 	di
	mov	ax,di       ;m(2)
	pop 	di
	mov	bx,di       ;m(1)
	sub	ax,bx
	cmp     ax,1
	jle     npend
	add	ax,bx
	sub	ax,1
	mov	ylo,ax
	add	bx,1
	mov	yhi,bx
	jmp     ndrawline
ncmp3:
	cmp	temp0,3
	jg      ncmp4
	pop 	di
	mov	ax,di       ;m(3)
	pop 	di
	mov	bx,di       ;m(2)
	sub	ax,bx
	cmp     ax,1
	jle     nch31
	add	ax,bx
	sub	ax,1
	mov	ylo,ax
	add	bx,1
	mov	yhi,bx
	pop	di            ;m(1)
	jmp     ndrawline
nch31:
	mov	ax,bx       ;m(2)
	pop 	di
	mov	bx,di       ;m(1)
	sub	ax,bx
	cmp     ax,1
	jle     npend
	add	ax,bx
	sub	ax,1
	mov	ylo,ax
	add	bx,1
	mov	yhi,bx
	jmp     ndrawline

ncmp4:
	cmp	temp0,4
	jg      ncmp5
	pop 	di
	mov	ax,di       ;m(4)
	pop 	di
	mov	bx,di       ;m(3)
	sub	ax,bx
	cmp     ax,1
	jle     nch41
	add	ax,bx
	sub	ax,1
	mov	ylo,ax
	add	bx,1
	mov	yhi,bx
	pop 	di          ;m(2)
	pop 	di          ;m(1)
	jmp     ndrawline
nch41:
	mov	ax,bx       ;m(3)
	pop 	di
	mov	bx,di       ;m(2)
	sub	ax,bx
	cmp     ax,1
	jle     nch42
	add	ax,bx
	sub	ax,1
	mov	ylo,ax
	add	bx,1
	mov	yhi,bx
	pop 	di           ;m(1)
	jmp     ndrawline

nch42:
	mov	ax,bx       ;m(2)
	pop 	di
	mov	bx,di       ;m(1)
	sub	ax,bx
	cmp     ax,1
	jle     nbine
	add	ax,bx
	sub	ax,1
	mov	ylo,ax
	add	bx,1
	mov	yhi,bx
	jmp     ndrawline

ncmp5:
	pop 	di
	mov	ax,di       ;m(5)
	pop 	di
	mov	bx,di       ;m(4)
	sub	ax,bx
	cmp     ax,1
	jle     nch51
	add	ax,bx
	sub	ax,1
	mov	ylo,ax
	add	bx,1
	mov	yhi,bx
	pop 	di          ;m(3)
	pop 	di          ;m(2)
	pop 	di          ;m(1)
	cmp	temp0,5
	jg	nbine
	jmp     ndrawline
nch51:
	mov	ax,bx       ;m(4)
	pop 	di
	mov	bx,di       ;m(3)
	sub	ax,bx
	cmp     ax,1
	jle     nch52
	add	ax,bx
	sub	ax,1
	mov	ylo,ax
	add	bx,1
	mov	yhi,bx
	pop 	di           ;m(2)
	pop 	di           ;m(1)
	cmp	temp0,5
	jg	nbine
	jmp     ndrawline
nch52:
	mov	ax,bx       ;m(3)
	pop 	di
	mov	bx,di       ;m(2)
	sub	ax,bx
	cmp     ax,1
	jle     nch53
	add	ax,bx
	sub	ax,1
	mov	ylo,ax
	add	bx,1
	mov	yhi,bx
	pop 	di           ;m(1)
	cmp	temp0,5
	jg	nbine
	jmp     ndrawline

nch53:
	mov	ax,bx       ;m(2)
	pop 	di
	mov	bx,di       ;m(1)
	sub	ax,bx
	cmp     ax,1
	jle     nbine
	add	ax,bx
	sub	ax,1
	mov	ylo,ax
	add	bx,1
	mov	yhi,bx
	cmp	temp0,5
	jg	nbine
	jmp     ndrawline


nbine:
	xor	ch,ch
	mov	cl,temp0
	sub	cl,5
npopy:
	pop	di
	loop	npopy

ndrawline:
	mov	ax,x
	;calculate xbyte
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     xbyte,ax              	;store in xbyte

	mov	cx,x
	and	cx,7
	mov 	bl,mask20
	shr	bl,cl

	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0Fh

  	OUTPUT 	GRAPHICS, BIT_MASK, bl


 	OUTPUT	GRAPHICS, SET_RESET,color

	mov	ax,ylo       ;load in ax
	mov	cx,yhi    ;load in cx

	cmp	ax,cx
	jl	zizi                    ;ordonate
	mov	dx,ax
	mov	ax,cx
	mov	cx,dx                   ;end change
	xor	edx,edx
zizi:					;restore dx
	mov	ylo,ax
	mov	yhi,cx

	mov	ax,ylo
	sub	ax,ROWS
	neg	ax
	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,xbyte              	;add x1byte to result
	mov	si,ax              	;store address in addr

	mov 	ax,ylo
	add	cx,1
	sub	cx,ax

	mov	ax,01fh
	mov	fs,ax
	mov	bx,OFSET
;====================================
dara:
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	sub	si,bx
	loop	dara
npend:
	inc	x
	mov	si,x_right
	cmp	x,si
	jl	nscdraw
	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 00h


	pop fs
	restore_regs			;restore protected integer registers
	return
        end_subroutine


;***************************************************************************
;***************************************************************************
	;subroutine to be called from the main program
	;call FILLCIRCLE(x,y,r,color,color_fill)

;==========================================================================
	subroutine    	FILLCIRCLE,STDFRAME,5
;==========================================================================

	save_regs			;save required registers
	push fs
	arg_ptr	5,ebx
	mov	cl,byte ptr [ebx]
	mov	color,cl
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0Fh

	arg_ptr	4,edx
	xor	ecx,ecx
	mov	cx,word ptr [edx]
	mov	bkcolor,cl

 	OUTPUT	GRAPHICS, SET_RESET, CL


	xor	eax,eax
	xor	ebx,ebx
	xor	edi,edi
	xor	esi,esi

	arg_ptr	3,edx
	mov	ax,word ptr [edx]      ;move ax, radius
	mov	y,ax

	arg_ptr	1,edx
	mov	si,word ptr [edx]      ;move si, x
	mov	di,si
	sub	si,ax
	mov	x_left,si
	add	si,ax
	add	si,ax
	mov	x_right,si

	arg_ptr	2,edx
	mov	si,word ptr [edx]
	mov	bx,si
	sub	si,ax
	mov	y_bottom,si
	add	si,ax
	add	si,ax
	mov	y_top,si

	mov	ax,y
	shl	ax,1
	sub	ax,3
	neg	ax
	mov	incr0,ax

	mov	x,0
irloop:
	xor	edx,edx
	xor	ecx,ecx
	xor	eax,eax
	mov	ax,x
	add	ax,di           	;x+xc
	mov	xlo,ax
	mov	cx,y                    ;y+yc
	add	cx,bx
	mov	ylo,cx
        WRITEPIX  Xlo, Ylo
	xor	ecx,ecx
	mov	cx,y
	sub	cx,bx
	neg	cx
	mov	ylo,cx
        WRITEPIX  Xlo, Ylo            	;x+xc, yc-y
	xor	eax,eax
	mov   	ax,x
	sub	ax,di
	neg	ax
	mov	xlo,ax
        WRITEPIX  Xlo, Ylo              ;xc-x, yc-y
	xor	ecx,ecx
	mov   	cx,y
	add	cx,bx
	mov	ylo,cx
        WRITEPIX  Xlo, Ylo             ;xc-x, y+yc
	xor	ecx,ecx
	xor	eax,eax
	mov	ax,y
	add	ax,di
	mov	xlo,ax
	mov	cx,x
	add	cx,bx
	mov	ylo,cx
 	 WRITEPIX  Xlo, Ylo             ;y+xc; x+yc
	xor	ecx,ecx
	mov	cx,x
	sub	cx,bx
	neg	cx
	mov	ylo,cx
 	 WRITEPIX  Xlo, Ylo            ;y+yc; yc-x
	xor	eax,eax
	mov   	ax,y
	sub	ax,di
	neg	ax
  	mov	xlo,ax
 	 WRITEPIX  Xlo, Ylo            ;xc-y; yc-x
	xor	ecx,ecx
	mov   	cx,x
	add	cx,bx
	mov	ylo,cx
 	 WRITEPIX  Xlo, Ylo           ;xc-y; yc+x
	xor	eax,eax
	cmp	incr0,0
	jge	ecy
	mov	ax,x
	shl	ax,1
	shl	ax,1
	add	ax,6
	add	ax,incr0
	mov	incr0,ax
	jmp	ircontinue
ecy:
	mov	ax,x
	sub	ax,y
	sal	ax,1
	sal	ax,1
	add	ax,10
	add	ax,incr0
	mov	incr0,ax
	dec	y
ircontinue:
	xor	edx,edx
	inc	x
	mov	dx,x
	cmp	dx,y
	jle	irloop

 	OUTPUT 	GRAPHICS, MAP_MASK, 0Fh
	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 00h

	mov	bx,x_left
	add	bx,1
	mov	x_left,bx
	mov	x,bx
;begin the big loop

;begin the up scanning to find intersection(s) with border
cnscdraw:
	mov	temp0,0
	mov	di,y_bottom
cnreadpixel:
	mov	ax,di
	mov	bx,x
	mov	cl,bl			;in cl low order byte of x

	;calculate xbyte
	shr	bx,1
	shr	bx,1
	shr	bx,1

	xor	edx,edx
	sub	ax,ROWS
	neg	ax
      	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	bx,ax               	;add xbyte to result :address
	add	bx,0              	;store address in address
	mov	si,bx                   ;address in si

	mov	ax,01fh
	mov	fs,ax

	and 	cl,7             	;cl = x&7
	xor	cl,7                    ;cl = nr of bits to shift left
	mov	ah,1                    ;unshifted bit mask
					;ah=011fh
	mov	ch,ah                   ;ch=1
	shl	ch,cl        		;bit mask in proper position
					;ch=?

	xor 	bl,bl                   ;bl used to accumulate the pixel value
	mov	dx,3ceh                 ;dx= grphics i/o port_addr
	mov	ax,304h                 ;ah = initial bit plane number
					;al = READ_MAP_SELECT
cnpl01:	out	dx,ax                   ;select bit plane
	mov	bh,fs:[si]              ;bh = byte from the current bit plane
					;!!! always 0 ???????
	and	bh,ch                   ;mask one bit
	neg	bh                      ;bit 7 of bh = 1 (if masked bit = 1)
			                ;bit 7 of bh = 1 (if masked bit = 1)

	rol	bx,1                    ; bit 0 of bl = next bit from pixel value
	dec	ah                      ;next bit plane number
	jge	cnpl01

	mov	al,bl                   ;al = pixel value
	xor	ah,ah                   ;ax = pixel value
	cmp	al,bkcolor
	jne     cnpjh
	push	di
	add     temp0,1
cnpjh:

	add	di,1
	cmp 	di,y_top
	jle	nreadpixel
cnnowcomp:

	cmp     temp0,0
	je      cnpend
	cmp	temp0,1
	jg      cncmp2
	pop	di
	jmp     cnpend
cncmp2:
	cmp	temp0,2
	jg      cncmp3
	pop 	di
	mov	ax,di       ;m(2)
	pop 	di
	mov	bx,di       ;m(1)
	sub	ax,bx
	cmp     ax,1
	jle     cnpend
	add	ax,bx
	sub	ax,1
	mov	ylo,ax
	add	bx,1
	mov	yhi,bx
	jmp     cndrawline
cncmp3:
	cmp	temp0,3
	jg      cncmp4
	pop 	di
	mov	ax,di       ;m(3)
	pop 	di
	mov	bx,di       ;m(2)
	sub	ax,bx
	cmp     ax,1
	jle     cnch31
	add	ax,bx
	sub	ax,1
	mov	ylo,ax
	add	bx,1
	mov	yhi,bx
	pop	di            ;m(1)
	jmp     cndrawline
cnch31:
	mov	ax,bx       ;m(2)
	pop 	di
	mov	bx,di       ;m(1)
	sub	ax,bx
	cmp     ax,1
	jle     cnpend
	add	ax,bx
	sub	ax,1
	mov	ylo,ax
	add	bx,1
	mov	yhi,bx
	jmp     cndrawline

cncmp4:
	cmp	temp0,4
	jg      ncmp5
	pop 	di
	mov	ax,di       ;m(4)
	pop 	di
	mov	bx,di       ;m(3)
	sub	ax,bx
	cmp     ax,1
	jle     cnch41
	add	ax,bx
	sub	ax,1
	mov	ylo,ax
	add	bx,1
	mov	yhi,bx
	pop 	di          ;m(2)
	pop 	di          ;m(1)
	jmp     cndrawline
cnch41:
	mov	ax,bx       ;m(3)
	pop 	di
	mov	bx,di       ;m(2)
	sub	ax,bx
	cmp     ax,1
	jle     cnch42
	add	ax,bx
	sub	ax,1
	mov	ylo,ax
	add	bx,1
	mov	yhi,bx
	pop 	di           ;m(1)
	jmp     cndrawline

cnch42:
	mov	ax,bx       ;m(2)
	pop 	di
	mov	bx,di       ;m(1)
	sub	ax,bx
	cmp     ax,1
	jle     cnbine
	add	ax,bx
	sub	ax,1
	mov	ylo,ax
	add	bx,1
	mov	yhi,bx
	jmp     cndrawline

cncmp5:
	pop 	di
	mov	ax,di       ;m(5)
	pop 	di
	mov	bx,di       ;m(4)
	sub	ax,bx
	cmp     ax,1
	jle     cnch51
	add	ax,bx
	sub	ax,1
	mov	ylo,ax
	add	bx,1
	mov	yhi,bx
	pop 	di          ;m(3)
	pop 	di          ;m(2)
	pop 	di          ;m(1)
	cmp	temp0,5
	jg	cnbine
	jmp     cndrawline
cnch51:
	mov	ax,bx       ;m(4)
	pop 	di
	mov	bx,di       ;m(3)
	sub	ax,bx
	cmp     ax,1
	jle     cnch52
	add	ax,bx
	sub	ax,1
	mov	ylo,ax
	add	bx,1
	mov	yhi,bx
	pop 	di           ;m(2)
	pop 	di           ;m(1)
	cmp	temp0,5
	jg	cnbine
	jmp     cndrawline
cnch52:
	mov	ax,bx       ;m(3)
	pop 	di
	mov	bx,di       ;m(2)
	sub	ax,bx
	cmp     ax,1
	jle     nch53
	add	ax,bx
	sub	ax,1
	mov	ylo,ax
	add	bx,1
	mov	yhi,bx
	pop 	di           ;m(1)
	cmp	temp0,5
	jg	cnbine
	jmp     cndrawline

cnch53:
	mov	ax,bx       ;m(2)
	pop 	di
	mov	bx,di       ;m(1)
	sub	ax,bx
	cmp     ax,1
	jle     cnbine
	add	ax,bx
	sub	ax,1
	mov	ylo,ax
	add	bx,1
	mov	yhi,bx
	cmp	temp0,5
	jg	cnbine
	jmp     cndrawline


cnbine:
	xor	ch,ch
	mov	cl,temp0
	sub	cl,5
cnpopy:
	pop	di
	loop	cnpopy

cndrawline:
	mov	ax,x
	;calculate xbyte
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     xbyte,ax              	;store in xbyte

	mov	cx,x
	and	cx,7
	mov 	bl,mask20
	shr	bl,cl

	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0Fh

  	OUTPUT 	GRAPHICS, BIT_MASK, bl


 	OUTPUT	GRAPHICS, SET_RESET,color

	mov	ax,ylo       ;load in ax
	mov	cx,yhi    ;load in cx

	cmp	ax,cx
	jl	czizi                    ;ordonate
	mov	dx,ax
	mov	ax,cx
	mov	cx,dx                   ;end change
	xor	edx,edx
czizi:					;restore dx
	mov	ylo,ax
	mov	yhi,cx

	mov	ax,ylo
	sub	ax,ROWS
	neg	ax
	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,xbyte              	;add x1byte to result
	mov	si,ax              	;store address in addr

	mov 	ax,ylo
	add	cx,1
	sub	cx,ax

	mov	ax,01fh
	mov	fs,ax
	mov	bx,OFSET
;====================================
cdara:
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	sub	si,bx
	loop	cdara
cnpend:
	inc	x
	mov	si,x_right
	cmp	x,si
	jl	cnscdraw
	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 00h


	pop fs
	restore_regs			;restore protected integer registers
	return
        end_subroutine


;***************************************************************************
;***************************************************************************
	end_code
	end

;############################################################################
