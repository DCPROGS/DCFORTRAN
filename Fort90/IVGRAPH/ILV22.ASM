;#############################################################################
;MODULE ILV22.ASM
;21.09.1993
;MODIFIED:05.10.1993

;THIS IS A GRAPHIC LIBRARY WRITTEN IN PHAR LAP ASSEMBLY
;TRYING TO SOLVE DIRECT DISPLAY READ/WRITE.
;AT THE BEGINING YOU'LL FIND SOME SUBROUTINE USING BIOS INTERUPS LIKE:

;SUBROUTINES USING DIRECT READING/WRITING:
;	1. WPIXEL(X,Y,COLOR)    -draw a pixel at coord. x,y in color

;	2. RPIXEL(X,Y,COLOR)
;	-return color of a pixel with coord. x,y

;	3. ILINE(X1,Y1,X2,Y2,COLOR) draw a line

;	4.  HLINE(X1,X2,Y,COLOR)	-draw a horizontal line
;	4.1 DHLINE(X1,X2,Y,COLOR,idot)	-draw a dashed horizontal line
;	     idot=0 continuous
;	     idot=1,3,7  draw single pixels with hdot pixel gaps
;	     idot=22,44,88  2 on/2 off; 4 on/4 off; 8 on/8 off

;	5.  VLINE(X,Y1,Y2,COLOR)	-draw a vertical line
;	5.1 DVLINE(X,Y1,Y2,COLOR,idot)	-draw a dashed vertical line
;	     idot as for DHLINE but idot-2,4,6, also OK

;	6. WARRAY(X,Y,N,COLOR)
;	-x,y:integer array,color,n:integer
;	-draw an array of pixels in one color

;	7. RARRAY(X,Y,N,MCOLOR)
;	-get the color of each pixel from an array of pixels

;	8. EARRAY(X,Y,N,MCOLOR)
;	-x,y,mcolor:integer array;n:integer
;	-erase  an array of pixels without erasing the background
;	-or draws an array of pixel in multicolor


;       9. EARRAY1(X,Y,N,COLOR,FIRST,LAST)
;	-erase  an array of pixels without erasing the background
;	 with i=first to i=last

;	10.DARRAY(X,Y,N,COLOR,MCOLOR)
;	-erase  and draw an array of pixels without erasing the background

;	11. DARRAY1(X,Y,N,COLOR,MCOLOR,FIRST,LAST)
;	-erase  and draw an array of pixels without erasing the background
;	 with i=first to i=last

;	12. CLEARBYTE (COLOR)
;	-fill with color a byte coloumn of pixels

;	13. CLEARS (Y1,Y2,COLOR)
;	-fill with color an array (0:640,y1:y2)

;       14. FILLWIN(X1,Y1,X2,Y2,COLOR)
;	-fill with color a rectangular array (x1:x2,y1:y2)

;	15. RDGWIN1(X1,Y1,X2,Y2,BUFFER)
;       -read a rectangular display area (x,y pixel units)

;	16. WRGWIN1(X1,Y1,X2,Y2,BUFFER)
;       -write a rectangular disply area

;	17. FILLPOLY(X,Y,N,COLOR,COLOR_FILL)
;	-DRAWS A FILLED POLYGON
include macros.pl

;define some constants
;=====================

	mask10	equ    0ffh
	mask11	equ    07fh
	mask12	equ    03fh
	mask13	equ    01fh
	mask14	equ    00fh
	mask15	equ    007h
	mask16	equ    003h
	mask17	equ    001h
	mask20	equ    080h
	mask21	equ    0c0h
	mask22	equ    0e0h
	mask23	equ    0f0h
	mask24	equ    0f8h
	mask25	equ    0fch
	mask26	equ    0feh
	mask27	equ    0ffh
	mask2_2 equ    033h
	mask1_1 equ    055h
	mask3_3 equ    011h
;define some macros we'll utilize in subroutines
;================================================================
OUTPUT		MACRO	port_addr,index,value
		mov	dx,port_addr
		mov	al,index
		out	dx,al
		inc	dx
		mov	al,value
		out	dx,al
		ENDM


SELECT		MACRO	port_addr,index
		mov	dx,port_addr
		mov	al,index
		out	dx,al
		ENDM


ERASE		MACRO 	color
		mov	ax,01fh
		mov 	gs:[di],ax
		mov	di,0
		mov	dx,1e0h
		mov	bx,50h
		mov	cx,50h
		mov	al,color
		ENDM

C_ADDRESS       MACRO	yad,xsh
		mov	ax,yad
		sub	ax,479
		neg	ax
		mov	dx,80
		mul	dx                      ;multiply by 80
		add	ax,xsh               ;add xshift to result
		mov	si,ax
		ENDM
	start_data
PUBLIC	SEQUENCER, GRAPHICS, SET_RESET, ENABLE_SET_RES, MAP_MASK, DATA_ROTATE
PUBLIC	READ_MAP_SELECT, MODE_REG, BIT_MASK, OFSET, ROWS, COLOUMNS
PUBLIC	hline
;define VGA Registers I/O Port Addresses (Hex): R.FERRARO p. 340
;================================================================
		;REGISTER     PORT_ADDR
		;========     =========

integer_2	GRAPHICS, 1, 3CEh
integer_2	SEQUENCER, 1, 3C4h
;Graphics Controller Registers Indexes (Hex): R>FERRARO p. 387
;================================================================

integer_1	SET_RESET, 1, 00h
integer_1	ENABLE_SET_RES,	1, 01h
integer_1	MAP_MASK , 1, 02h		;color compare
integer_1	DATA_ROTATE, 1, 03h
integer_1	READ_MAP_SELECT, 1, 04h
integer_1	MODE_REG, 1, 05h
integer_1	BIT_MASK, 1, 08h

integer_2	ROWS, 1, 479
integer_2	OFSET, 1, 80
integer_2	COLOUMNS, 1, 640
	;mode	12h (18): 16 colors
	;	-ofset=80
	;	-rows=479
	;	-pixel resolution 640 X 480
	;	-page 1
	; 	-8 pixel/byte
	;
	;mode 0Dh (13): 16 colors
	;	-ofset=40
	;	-rows=199
	;	-pixel resolution 320 X 200
	;	-page 4
	; 	-8 pixel/byte
	;mode 29h (): 16 colors
	;	-ofset=100
	;	-rows=599
	;	-pixel resolution 800 X 600
	;	-page ?
	; 	-8 pixel/byte
	;mode 13h (19):	256 colors
	;	-ofset=320
	;	-rows=199
	;	-pixel resolution 320 X 200
	;	-page 1
	; 	-1 pixel/byte
	end_data





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

	;define local storage
	begin_autos
	integer_1	colour
	integer_1       masca
	integer_2       x
	integer_2       xbyte
	integer_2       addr

	end_autos

	save_regs			;save required registers
	push fs



	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0fh

	arg_ptr	3,ebx
	mov	cl,byte ptr [ebx]
	mov	colour,cl
	mov	cl,0h

 	OUTPUT	GRAPHICS, SET_RESET, COLOUR


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
	mov	masca,bl

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
	mov	addr,ax              	;store address in addr

	mov	ax,01fh
	mov	fs,ax

 	mov 	si,addr              	;get address
  	OUTPUT 	GRAPHICS, BIT_MASK, MASCA

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
	;THIS IS A SUBROUTINE TO READ A PIXEL VALUE
	;subroutine to be called from the main program
	;call RPIXEL(x,y,color) : R. WILTON p.128+96


;==========================================================================
	subroutine     RPIXEL,STDFRAME,3
;==========================================================================

	;define local storage
	begin_autos
	end_autos
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

	;define local storage
	;====================

	begin_autos
	integer_1 	lmask
	integer_1	lcolor
	integer_2 	incremento
	integer_2 	lx1
	integer_2 	lx2
	integer_2 	ldx
	integer_2 	ldx1
	integer_2 	ly1
	integer_2 	ly2
	integer_2 	ldy
	integer_2 	ldy1
	integer_2 	ld
	integer_2 	lx
	integer_2 	ly
	integer_2 	lxshift
	integer_2 	lincr1
	integer_2 	lincr2
	end_autos

	save_regs			;save required registers
	push fs

	mov	ax,01fh
	mov	fs,ax


	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0Fh
	arg_ptr	5,ebx
	mov	cl,byte ptr [ebx]
	mov	lcolor,cl

    	OUTPUT	GRAPHICS, SET_RESET, Cl

	xor	edx,edx
	arg_ptr	2,edi          		;point to second argument y1
	mov	dx,word ptr [edi]       ;load in ax
	mov	ly1,dx

	xor	ebx,ebx
	arg_ptr	4,edi                    ;y2
	mov	bx,word ptr [edi]
	mov	ly2,bx
	sub	bx,dx
	mov	ldy1,bx                  ;ldy1=y2-y1
	cmp	bx,0
	jg	lin2
	neg	bx                        ;bx=dy

lin2:
	mov	ldy,bx                   ;ldy=|y2-y1|
	xor	eax,eax
	arg_ptr	1,edi          		;point to first argument x1
	mov	ax,word ptr [edi]       ;load in ax
	mov	lx1,ax
	xor	edi,edi
	arg_ptr	3,edi                    ;x2
	mov	di,word ptr [edi]
	mov	lx2,di
	sub	di,ax                     ;di=dx
	mov	ldx1,di                   ;ldx1=x2-x1
	cmp	di,0        		  ;x2>x1?
	jg	lin1
	xor	ecx,ecx
	mov	cx,lx2                    ;x2<x1
	mov	lx,cx                     ;x=x2
	xor	ecx,ecx
	mov	cx,ly2
	mov	ly,cx                     ;y=y2
	neg	di                        ;di=|x2-x1|=dx
	mov	incremento,80             ;y2<y1; inc=+80
	mov	cx,ldy1
	cmp	cx,0                    ;y2>y1?
	jg	lin3
	neg	incremento              ;inc=-80
lin3:
	jmp	linor

lin1:                                     ;x2>x1
	mov	lx,ax                     ;x=x1
	mov	ly,dx                     ;y=y1
	mov	incremento,80             ;y2<y1; inc=+80
	cmp	ldy1,0                    ;y2>y1?
	jle	linor
	neg	incremento                ;y2>y1 inc=-80

linor:
	mov	ldx,di                    ;ldx=|x2-x1|

	;calculate x>>3 for address
	mov	ax,lx
	shr	ax,1
	shr	ax,1
	shr	ax,1
	mov	lxshift,ax


	;calculate mask
	mov	cx,lx
	and	cx,7
      	mov 	al,080h
      	shr	al,cl
	mov	lmask,al

	sub	di,bx                	;dx-dy>0? di=dx-dy
	cmp	di,0
	jle	lin5

;xline:                                 ;dx>dy

	mov	ax,bx                   ;ax=dy
	shl	ax,1                    ;2*dy
	mov	lincr1,ax               ;incr1=2*dy
	sub	ax,ldx                  ;2*dy-dx
	mov	ld,ax                   ;d=2*dy-dx
	neg	di                      ;di=dy-dx
	shl	di,1                    ;2*(dy-dx)
	mov	lincr2,di               ;incr2=2*(dy-dx)
	xor	edi,edi
	;calculate address
	C_ADDRESS  ly,lxshift

	inc	ldx
  	OUTPUT 	GRAPHICS, BIT_MASK, 8h
	mov	al,lmask
	mov	cx,ldx
	mov	bx,ld
	xor 	edi,edi
	mov	di,incremento
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
	add	bx,lincr2
	add	si,di
	jmp	check1
noinc1:
	add	bx,lincr1
check1:
	loop	lloop1
	jmp	lend

	;start code for dx<dy
lin5:
	mov	ax,ldx
	shl	ax,1
	mov	lincr1,ax    	;incr1=2*dx
	sub	ax,ldy          ;ax=2*dx-dy
	mov	ld,ax           ;d=2*dx-dy
	shl	di,1
	mov	lincr2,di       ;incr2=2*(dx-dy)
	mov	di,80
	neg	di
	mov	incremento,di
	inc	ldy
	cmp	ldy1,0          ;y2-y1>0?
	jg	lin51

	; here for y1>y2
	mov	ax,lx2          ;y2<y1 x=x2,y=y2
	shr	ax,1
	shr	ax,1
	shr	ax,1
	mov	lxshift,ax

	;calculate address
	C_ADDRESS  ly2,lxshift
	;calculate mask
	mov	cx,lx2
	and	cx,7
      	mov 	al,080h
      	shr	al,cl
	mov	lmask,al

	cmp	ldx1,0           ;x2-x1>0?
	jle	ypline           ;x2<=x1
	jmp	ynline           ;x2>x1

lin51:
	mov	ax,lx1
	shr	ax,1
	shr	ax,1
	shr	ax,1
	mov	lxshift,ax

	;calculate address
	C_ADDRESS  ly1,lxshift
	;calculate mask
	mov	cx,lx1
	and	cx,7
      	mov 	al,080h
      	shr	al,cl
	mov	lmask,al
	cmp	ldx1,0
	jle	ynline

ypline:
  	OUTPUT 	GRAPHICS, BIT_MASK, 8h
	mov	cx,ldy
	mov	bx,ld
	mov	al,lmask
lloop3:
	out	dx,al
	mov	ah,fs:[si]
	mov	fs:[si],ah
	add	si,incremento
	cmp	bx,0
	jl	noinc3
	add	bx,lincr2
	shr	al,1
	jnc	check3
	mov	al,080h
	inc	si
	jmp	check3
noinc3:
	add	bx,lincr1
check3:
	loop	lloop3

	jmp	lend

ynline:
  	OUTPUT 	GRAPHICS, BIT_MASK, 8h
	mov	cx,ldy
	mov	bx,ld
	mov	al,lmask
lloop2:
	out	dx,al
	mov	ah,fs:[si]
	mov	fs:[si],ah
	add	si,incremento
	cmp	bx,0
	jl	noinc2
	add	bx,lincr2
	shl	al,1
	jnc	check2
	mov	al,01h
	dec	si
	jmp	check2
noinc2:
	add	bx,lincr1
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

	;define local storage
	;====================
	begin_autos
	integer_2     	x1byte
	integer_2	x2byte
	integer_2     	x1
	integer_2	x2
	integer_2	deltax
	integer_2	nbyte
	integer_2	address
	integer_1	temp1
	integer_1	temp2
	integer_1	color
	integer_1	mask1
	integer_1	mask2
	end_autos



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
	mov	x1,ax
	mov	x2,cx
	mov	dx,00h

	;calculate x1byte
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     x1byte,ax              	;store in x1byte
      ;	mov	temp1,ah
	;calculate x2byte
	shr	cx,1
	shr	cx,1
	shr	cx,1
      	mov	x2byte,cx               ;store in x2byte
       ;mov	temp2,ch
	;calculate deltax
	sub	cx,ax                   ;x2byte-x1byte and store in cx
	mov	deltax,cx		;store in deltax

	;calculate nbyte
	sub	cx,1			;calculate nbyte
	mov	nbyte,cx		;store nbyte

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
	add	ax,x1byte               ;add x1byte to result
	mov	address,ax              ;store address in address

	mov 	ax,00h
	xor 	dx,dx
	;calculate x1 modulo 8
       mov 	ax,x1			;mov x1 in ax
       mov	bx,8                    ;divide by 8
       div	bx
       mov	temp1,dl		;store result in temp1
	xor 	dx,dx

	 mov	ax,00h
	;calculate x2 modulo 8
	mov 	ax,x2			;mov x2 in ax
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
;	mov	cx,nbyte
loop1:
	inc	si
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	inc	cx

	cmp	cx,nbyte
 	;loop loop1
	 jle	loop1

	;mov 	ax,00h
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


;==========================================================================
	subroutine     dHLINE,STDFRAME,5
;==========================================================================

	;define local storage
	;====================
	begin_autos
	integer_2     	dx1byte
	integer_2	dx2byte
	integer_2     	dx1
	integer_2	dx2
	integer_2	ddeltax
	integer_2	dnbyte
	integer_2	daddress
	integer_1	dtemp1
	integer_1	dtemp2
	integer_1	dcolor
	integer_1	dmask1
	integer_1	dmask2
	integer_1	newmask
	integer_1	hdot
	integer_1	nb1
	end_autos



	save_regs			;save required registers
	push fs

	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0Fh

	arg_ptr	5,ebx                 	;get color
	mov	cl,byte ptr [ebx]
	mov	hdot,cl

	mov	newmask,0ffh
	cmp 	cl,0
	je      hcontinue

	cmp 	cl,88
	je      hcontinue

	mov	newmask,055h
	cmp	cl,1
	je      hcontinue

	mov	newmask,011h
	cmp	cl,3
	je      hcontinue

	mov	newmask,001h
	cmp	cl,7
	je      hcontinue

	mov	newmask,033h
	cmp	cl,22
	je      hcontinue

	mov	newmask,0fh

hcontinue:

	arg_ptr	4,ebx                 	;get color
	mov	cl,byte ptr [ebx]
	mov	dcolor,cl
	mov	cl,0h

 	OUTPUT	GRAPHICS, SET_RESET, dCOLOR

	arg_ptr	1,ebx          		;point to first argument x1
	mov	ax,word ptr [ebx]       ;load in ax

	arg_ptr 2,ebx                   ;point to second argument x2
	mov	cx,word ptr [ebx]       ;load in cx

	cmp	ax,cx
	jl	contd                    ;ordonate
	mov	dx,ax
	mov	ax,cx
	mov	cx,dx                   ;end change

contd:			;restore dx
	mov	dx1,ax
	mov	dx2,cx
	mov	dx,00h

	;calculate x1byte
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     dx1byte,ax              	;store in x1byte
      ;	mov	dtemp1,ah
	;calculate x2byte
	shr	cx,1
	shr	cx,1
	shr	cx,1
      	mov	dx2byte,cx               ;store in x2byte
       ;mov	dtemp2,ch
	;calculate deltax
	sub	cx,ax                   ;x2byte-x1byte and store in cx
	mov	ddeltax,cx		;store in deltax

	;calculate nbyte
	sub	cx,1			;calculate nbyte
	mov	dnbyte,cx		;store nbyte

       mov 	ax,dnbyte			;mov x1 in ax
       mov	bx,2                    ;divide by 8
       div	bx
       mov	nb1,dl		;store result in temp1
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
	add	ax,dx1byte               ;add x1byte to result
	mov	daddress,ax              ;store address in address

	mov 	ax,00h
	xor 	dx,dx
	;calculate x1 modulo 8
       mov 	ax,dx1			;mov x1 in ax
       mov	bx,8                    ;divide by 8
       div	bx
       mov	dtemp1,dl		;store result in temp1
	xor 	dx,dx

	 mov	ax,00h
	;calculate x2 modulo 8
	mov 	ax,dx2			;mov x2 in ax
	mov	bx,8                    ;divide by 8
	div	bx
	mov	dtemp2,dl		;store result in temp2

	mov	bx,00h
	mov	dx,00h

	;begin the x1modulo8 case
	;compare with values 0:7 and choose the correct mask
	mov	ah,dtemp1		;mov in ah x1modulo8
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
	mov	ah,dtemp2
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
	mov	dmask1,bl
	mov	dmask2,dl

	mov	cx,ddeltax   		;compare deltax with 1
	cmp	cx,1
	je	dtwo0                    	;if 1 jump to two
	jg	dthree0                 	;>1 jump to three

	;begin drawing line for 1 byte
	and	bx,dx
	mov 	cx,00h

	mov	dx,00h

       	mov 	ax,01fh			;point to display address

       	mov 	fs,ax
       	mov 	si,daddress              ;get address

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
	mov 	si,daddress              ;get address

       	mov	ax,3ceh   		;point to mask address
       	mov	dx,ax
       	mov	al,8
       	out	dx,al
       	inc	dx                      ;point to mask register
	;output first byte
	mov	al,dmask1		;get mask1
	and	al,newmask
       	out     dx,al                   ;load mask
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	;output second byte
	mov	al,dmask2
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

 	mov 	si,daddress              ;get address


       	mov	ax,3ceh   		;point to mask address
       	mov	dx,ax
       	mov	al,8
       	out	dx,al
       	inc	dx                      ;point to mask register

	;output first byte
	mov	al,dmask1		;get mask1
	and	al,newmask

       	out     dx,al                   ;load mask
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color


	;load the middle bytes
	mov	al,0ffh
	and	al,newmask
	out	dx,al
	mov	cx,1
;	mov	cx,dnbyte
dloop1:
	inc	si
	cmp	hdot,88
	jne	dotty
	inc	cx
	inc	si
dotty:
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	inc	cx

	cmp	hdot,88
	jne	nodec
	cmp	cx,dnbyte
	jl	dloop1
	jmp     lalala
nodec:
	cmp	cx,dnbyte
 	;loop loop1
	 jle	dloop1

	;mov 	ax,00h
	;output end byte
lalala:
	mov	al,dmask2                ;get mask2
	and	al,newmask


	out	dx,al
	cmp	hdot,88
	jne	noinc
	cmp	nb1,0
	je	noinc
	inc	si
noinc:
	inc 	si
       	out     dx,al                   ;load mask
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color


dende:
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

	begin_autos
	integer_1	vmasca
	integer_1	vcolour
	integer_1	vdot
	integer_2	vnxbyte
	integer_2	vdeltay
	integer_2	vy1
	integer_2	vy2
	integer_2	vnx
	integer_2	vnaddr


	end_autos

	save_regs			;save required registers
	push fs

	xor 	eax,eax

	arg_ptr	1,ebx          		;point to first argument x
	mov	ax,word ptr [ebx]       ;load in ax
	mov	vnx,ax
	;calculate xbyte
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     vnxbyte,ax              	;store in xbyte

	mov	cx,vnx
	and	cx,7
	mov 	bl,mask20
	shr	bl,cl
	mov	vmasca,bl

	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0Fh

  	OUTPUT 	GRAPHICS, BIT_MASK, vMASCA

	arg_ptr	4,ebx
	mov	cl,byte ptr [ebx]
	mov	vcolour,cl
	mov	cl,0h

 	OUTPUT	GRAPHICS, SET_RESET, vCOLOUR

	xor 	eax,eax
	xor 	ecx,ecx
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
	mov	vy1,ax
	mov	vy2,cx

	mov	ax,vy1
	sub	ax,ROWS
	neg	ax
	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,vnxbyte               	;add x1byte to result
	mov	vnaddr,ax              	;store address in addr

	mov 	ax,vy1
	add	cx,1
	sub	cx,ax
	mov	vdeltay,cx

	mov	ax,01fh
	mov	fs,ax

 	mov 	si,vnaddr              	;get address

	mov	cx,vdeltay
	mov	bx,OFSET
;====================================
;====================================
vrunloop:
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	sub	si,bx
	loop vrunloop

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

	begin_autos
	integer_1	dvmasca
	integer_1	dvcolour
	integer_1	dvdot
	integer_2	dvnxbyte
	integer_2	dvdeltay
	integer_2	dvy1
	integer_2	dvy2
	integer_2	dvnx
	integer_2	dvnaddr


	end_autos

	save_regs			;save required registers
	push fs
;============================
	arg_ptr	5,edi          		;point to first argument x
	mov	cl,byte ptr [edi]       ;load in ax
	mov	dvdot,cl
;=============================


	xor	ecx,ecx
	xor	edi,edi
	arg_ptr	1,ebx          		;point to first argument x
	mov	ax,word ptr [ebx]       ;load in ax
	mov	dvnx,ax
	;calculate xbyte
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     dvnxbyte,ax              	;store in xbyte

	mov	cx,dvnx
	and	cx,7
	mov 	bl,mask20
	shr	bl,cl
	mov	dvmasca,bl

	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0Fh

  	OUTPUT 	GRAPHICS, BIT_MASK, dvMASCA

	arg_ptr	4,ebx
	mov	cl,byte ptr [ebx]
	mov	dvcolour,cl
	mov	cl,0h

 	OUTPUT	GRAPHICS, SET_RESET, dvCOLOUR

	arg_ptr	2,ebx          		;point to first argument x1
	mov	ax,word ptr [ebx]       ;load in ax

	arg_ptr 3,ebx                   ;point to second argument x2
	mov	cx,word ptr [ebx]       ;load in cx

	cmp	ax,cx
	jl	dvcontv                    ;ordonate
	mov	dx,ax
	mov	ax,cx
	mov	cx,dx                   ;end change
	xor	edx,edx
dvcontv:			;restore dx
	mov	dvy1,ax
	mov	dvy2,cx

	mov	ax,dvy1
	sub	ax,ROWS
	neg	ax
	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,dvnxbyte               	;add x1byte to result
	mov	dvnaddr,ax              	;store address in addr

	mov 	ax,dvy1
	add	cx,1
	sub	cx,ax
	mov	dvdeltay,cx

	mov	ax,01fh
	mov	fs,ax

 	mov 	si,dvnaddr              	;get address

	;mov	cx,dvdeltay
	mov	bx,OFSET

	cmp	dvdot,22
	je	dend2
	cmp	dvdot,44
	je	dend4
	cmp	dvdot,88
	je	dend8
	mov	cx,1

;====================================
;====================================
dvrunloop:
       	mov	al,fs:[si]
       	mov	fs:[si],al              ;output color
	sub	si,bx
	add	cx,1
	cmp	dvdot,0
	je	dot

	sub	si,bx
	add	cx,1
	cmp	dvdot,1
	je	dot

	sub	si,bx
	add	cx,1
	cmp	dvdot,2
	je	dot

	sub	si,bx
  	add	cx,1
 	cmp	dvdot,3
 	je	dot

 	sub	si,bx
  	add	cx,1
 	cmp	dvdot,4
 	je	dot

 	sub	si,bx
  	add	cx,1
 	cmp	dvdot,5
 	je	dot

 	sub	si,bx
  	add	cx,1
 	cmp	dvdot,6
 	je	dot

 	sub	si,bx
  	add	cx,1
 	cmp	dvdot,7
 	je	dot

 	sub	si,bx
  	add	cx,1

dot:
	cmp	cx,dvdeltay
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
	cmp	cx,dvdeltay
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
	cmp	dx,dvdeltay
	jge	dend
	sub	si,bx
	loop	l44
	add	dx,4
	sub	si,bx
	sub	si,bx
	sub	si,bx
	sub	si,bx
	cmp	dx,dvdeltay
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
	cmp	dx,dvdeltay
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
	cmp	dx,dvdeltay
	jle	d88
	jmp	dend

dend:

	pop fs
	restore_regs			;restore protected integer registers


	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 00h
	return
        end_subroutine



;***************************************************************************
;***************************************************************************



	;THIS IS A SUBROUTINE TO DRAW AN ARRAY OF PIXELS
	;subroutine to be called from the main program
	;call WARRAY(x,y,n,color):utilize a VLINE code modified


;==========================================================================
	subroutine    	WARRAY,STDFRAME,4
;==========================================================================

	;define local storage
	begin_autos
	integer_2	number

	end_autos



	save_regs			;save required registers
	push fs



	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0fh

	arg_ptr	4,ecx
	mov	cl,byte ptr [ecx]
	mov	colour,cl
	mov	cx,00h


	arg_ptr	3,ecx
	mov	cx,word ptr [ecx]
	mov	number,cx
	mov	ecx,0000h

	arg_ptr	1,ebx          		;point to first argument x
	arg_ptr	2,edi                   ;point to arg 2 y
repeat:
 	OUTPUT	GRAPHICS, SET_RESET, COLOUR

	mov	ax,word ptr [ebx]       ;load in ax
	mov	x,ax
	;calculate xbyte
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     xbyte,ax              	;store in xbyte
	mov	ax,00h

	mov	cx,x
	and	cx,7
	mov 	al,080h
	shr	al,cl
	mov	masca,al


	;calculate address
	mov	ax,word ptr [edi]       ;load in ax
	sub	ax,ROWS
	neg	ax
      	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,xbyte               	;add x1byte to result
	mov	addr,ax              	;store address in addr

	mov	ax,01fh
	mov	fs,ax

 	mov 	si,addr              	;get address
  	OUTPUT 	GRAPHICS, BIT_MASK, MASCA

       	mov	al,fs:[si]
       	mov	fs:[si],al             	;output color
	add	ebx,2

	add	edi,2
	dec     number
	jnz	repeat

	pop fs
	restore_regs			;restore protected integer registers


	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 00h
	return
        end_subroutine



;***************************************************************************
;***************************************************************************


	;THIS IS A SUBROUTINE TO READ THE VALUES OF AN ARRAY OF PIXELS
	;subroutine to be called from the main program
	;call RARRAY(x,y,n,color) : modified R. WILTON p.128+96

;==========================================================================
	subroutine     RARRAY,STDFRAME,4
;==========================================================================
	save_regs			;save required registers
	push 	fs

	;define local storage
	begin_autos
	integer_4	counte
	end_autos

	arg_ptr	1,ecx          		;point to first argument: x
	arg_ptr 2,edi                   ;point to second argument: y

	mov	counte,0001h
cycle:    				;begin the big loop

	mov	ax,word ptr [ecx]       ;load first element of array x in bx
	mov	x,ax
	;calculate xbyte
	shr	ax,1
	shr	ax,1
	shr	ax,1
	mov	xbyte,ax
        mov	ax,word ptr [edi]       ;load first element of array y in ax


	;calculate address
	sub	ax,ROWS
	neg	ax
      	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,xbyte               	;add xbyte to result :address
	mov	addr,ax

	mov	ax,01fh
	mov	fs,ax
	xor	dx,dx

	mov	dx,x                    ;calculate mask
	and	dx,7
	xor	dl,7
	mov	ah,1
	cmp	dl,0
	je	lala1
lola1:
	shl	ah,1
	dec	dl
	cmp	dl,0
	jg	lola1

lala1:	mov	masca,ah                ;save ah
	mov	si,addr
	xor 	bl,bl                   ;bl used to accumulate the pixel value
	mov	dx,3ceh                 ;dx= grphics i/o port_addr
	mov	ax,304h                 ;ah = initial bit plane number
					;al = READ_MAP_SELECT
l0111:	out	dx,ax                   ;select bit plane
	mov	bh,fs:[si]              ;bh = byte from the current bit plane
					;!!! always 0 ???????
	and	bh,masca                ;mask one bit
	neg	bh                      ;bit 7 of bh = 1 (if masked bit = 1)
			                ;bit 7 of bh = 1 (if masked bit = 1)

	rol	bx,1                    ; bit 0 of bl = next bit from pixel value
	dec	ah                      ;next bit plane number
	jge	l0111

	mov	al,bl                   ;al = pixel value
	xor	ah,ah			;ax = pixel value
	push	ax
	add	edi,2                   ;increase edi
	add	ecx,2                   ;increase ecx
	xor	eax,eax
	xor	edx,edx
	arg_ptr	3,edx                   ;point to third argument:n
	mov	dx,word ptr [edx]       ;load it in cx
	mov	eax,counte
	inc	eax
	mov	counte,eax
	cmp	ax,dx
	jle	cycle

	xor	ebx,ebx
	xor	edx,edx
	xor	ecx,ecx
	arg_ptr	3,ebx
	mov	cx,word ptr [ebx]
	mov	dx,cx
	shl	dx,1
	sub	dx,2
	arg_ptr	4,ebx
	add	ebx,edx

mall11:
	pop	ax
	mov	word ptr [ebx],ax
	sub	ebx,2
	loop 	mall11
	pop 	fs
	restore_regs			;restore protected integer registers
	return
        end_subroutine

;***************************************************************************
;***************************************************************************




	;THIS IS A SUBROUTINE TO ERASE AN ARRAY OF PIXELS
	;subroutine to be called from the main program
	;call EARRAY(x,y,n,color):utilize a VLINE code modified


;==========================================================================
	subroutine    	EARRAY,STDFRAME,4
;==========================================================================

	;define local storage
	begin_autos
	integer_2	result
	end_autos
	save_regs			;save required registers
	push fs



	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0fh

	mov	dx,00h
	arg_ptr	3,ecx
	mov	cx,word ptr [ecx]
	mov	number,cx

	mov	cx,00h

	arg_ptr	1,ebx          		;point to first argument x
	arg_ptr	2,edi                   ;point to arg 2 y
	arg_ptr 4,ecx

repcol:
	mov	dl,byte ptr [ecx]
	mov	color,dl
	cmp	dl,0
	jne	q1
	mov	color,0
q1:
	mov	dx,00h
       	OUTPUT	GRAPHICS, SET_RESET, COLOR

	mov	ax,word ptr [ebx]       ;load in ax
	mov	x,ax
	;calculate xbyte
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     xbyte,ax              	;store in xbyte
	mov	ax,00h

	mov	dx,0h
	mov	dx,x
	and	dx,7
	mov	al,80h
	cmp	dl,0
	je	lala
lola:
	shr	al,1
	dec	dl
	cmp	dl,0
	jg	lola

lala:	mov	masca,al
	mov	dx,0h

	;calculate address
	mov	ax,word ptr [edi]       ;load in ax
	sub	ax,ROWS
	neg	ax
      	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,xbyte                ;add x1byte to result
	mov	addr,ax                 ;store address in addr

	mov	ax,01fh
	mov	fs,ax

 	mov 	si,addr              	;get address
  	OUTPUT 	GRAPHICS, BIT_MASK, MASCA

       	mov	al,fs:[si]
       	mov	fs:[si],al             	;output color
	add	ebx,2
	add	edi,2
	add	ecx,2

	dec     number

	jnz	repcol

	pop fs
	restore_regs			;restore protected integer registers


	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 00h
	return
        end_subroutine

;****************************************************************************
;***************************************************************************

	;THIS IS A SUBROUTINE TO ERASE AN ARRAY OF PIXELS
	;between x(i),y(i) and x(f),y(f)
	;subroutine to be called from the main program
	;call EARRAY1(x,y,n,color,first,last)


;==========================================================================
	subroutine    	EARRAY1,STDFRAME,6
;==========================================================================

	;define local storage
	begin_autos
	integer_2	prim
	integer_2	ultim
	integer_2	dif

	integer_4	dprim

	end_autos

	save_regs			;save required registers
	push fs
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0fh

	mov	dx,00h

	arg_ptr 3,ecx
	mov	ax,word ptr [ecx]
	mov	number,ax


	arg_ptr	5,ecx
	mov	cx,word ptr [ecx]  ;load begin of cycle
	mov	prim,cx
       	shl	cx,1
       	mov	dprim,ecx
	xor	ecx,ecx

	arg_ptr	6,ecx
	mov	ax,word ptr [ecx]
	mov	ultim,ax
	mov	dx,prim
	sub	ax,dx
	add	ax,1
	mov	dif,ax

	arg_ptr	1,ebx          		;point to first argument x
	mov	ax,word ptr [ebx]
	add	ebx,dprim               ;go to the element you begin with
	mov	ax,word ptr [ebx]

	arg_ptr	2,edi                   ;point to arg 2 y
	add	edi,dprim               ;go to element you begin with

	arg_ptr 4,ecx
	add	ecx,dprim

repcol20:
	mov	dl,byte ptr [ecx]
	mov	color,dl
	cmp	dl,0
	jne	q20
	mov	color,0
q20:
	mov	dx,00h
       	OUTPUT	GRAPHICS, SET_RESET, COLOR

	mov	ax,word ptr [ebx]       ;load in ax,x
	mov	x,ax
	;calculate xbyte
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     xbyte,ax              	;store in x1byte
	mov	ax,00h
	mov	dx,0h
	mov	dx,x
	and	dx,7
	mov	al,80h
	cmp	dl,0
	je	lala20
lola20:
	shr	al,1
	dec	dl
	cmp	dl,0
	jg	lola20

lala20:	mov	masca,al
	mov	dx,0h

	;calculate address
	mov	ax,word ptr [edi]       ;load in ax,y
	sub	ax,ROWS
	neg	ax
      	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,xbyte               	;add xbyte to result
	mov	addr,ax              	;store address in addr

	mov	ax,01fh
	mov	fs,ax

 	mov 	si,addr              	;get address
  	OUTPUT 	GRAPHICS, BIT_MASK, MASCA

       	mov	al,fs:[si]
       	mov	fs:[si],al             	;output color
	add	ebx,2
	add	edi,2
	add	ecx,2

	dec     dif
	jnz	repcol20

	pop fs
	restore_regs			;restore protected integer registers


	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 00h
	return
        end_subroutine

;***************************************************************************
;***************************************************************************

	;THIS IS A SUBROUTINE TO DRAW AND ERASE AN ARRAY OF PIXELS
	;subroutine to be called from the main program
	;call DARRAY(x,y,n,color,MCOLOR):utilize a VLINE code modified


;==========================================================================
	subroutine    	DARRAY,STDFRAME,5
;==========================================================================

	;define local storage
	begin_autos
	end_autos

	save_regs				;save required registers
	push fs

	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0fh

	arg_ptr	4,ecx
	mov	cl,byte ptr [ecx]
	mov	colour,cl
	mov	cx,00h

       ;OUTPUT	GRAPHICS, SET_RESET, COLOUR

	arg_ptr	3,ecx
	mov	cx,word ptr [ecx]
	mov	number,cx

	arg_ptr	1,ebx          			;point to first argument x
	arg_ptr	2,edi                   	;point to arg 2 y

repeat1:
 	OUTPUT	GRAPHICS, SET_RESET, COLOUR

	mov	ax,word ptr [ebx]       	;load in ax
	mov	x,ax
	;calculate x1byte
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     xbyte,ax              		;store in x1byte
	mov	ax,00h

	mov	cx,x
	and	cx,7
	mov 	al,080h
	shr	al,cl
	mov	masca,al


	;calculate address
	mov	ax,word ptr [edi]       	;load in ax
	sub	ax,ROWS
	neg	ax
      	mov	dx,OFSET
	mul	dx                      	;multiply by 80
	add	ax,xbyte               		;add x1byte to result
	mov	addr,ax              		;store address in address

	mov	ax,01fh
	mov	fs,ax

 	mov 	si,addr              		;get address
  	OUTPUT 	GRAPHICS, BIT_MASK, MASCA

       	mov	al,fs:[si]
       	mov	fs:[si],al             		;output color
	add	ebx,2

	add	edi,2
	dec     number
	jnz	repeat1

       ;pop fs
       ;restore_regs			;restore protected integer registers


      	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh

	mov	dx,00h
	arg_ptr	3,ecx
	mov	cx,word ptr [ecx]
	mov	number,cx

	mov	cx,00h

	arg_ptr	1,ebx          		;point to first argument x
	arg_ptr	2,edi                   ;point to arg 2 y
	arg_ptr 5,ecx

repcol1:
	mov	dl,byte ptr [ecx]
	mov	color,dl
	cmp	dl,0
	jne	q11
	mov	color,0
q11:
	mov	dx,00h
       	OUTPUT	GRAPHICS, SET_RESET, COLOR

	mov	ax,word ptr [ebx]       ;load in ax
	mov	x,ax
	;calculate x1byte
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     xbyte,ax              	;store in x1byte
	mov	ax,00h
	mov	dx,0h
	mov	dx,x
	and	dx,7
	mov	al,80h
	cmp	dl,0
	je	lala2
lola2:
	shr	al,1
	dec	dl
	cmp	dl,0
	jg	lola2

lala2:	mov	masca,al
	mov	dx,0h

	;calculate address
	mov	ax,word ptr [edi]       ;load in ax
	sub	ax,ROWS
	neg	ax
      	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,xbyte               	;add x1byte to result
	mov	addr,ax              	;store address in addr

	mov	ax,01fh
	mov	fs,ax

 	mov 	si,addr              	;get address
  	OUTPUT 	GRAPHICS, BIT_MASK, MASCA

       	mov	al,fs:[si]
       	mov	fs:[si],al             	;output color
	add	ebx,2
	add	edi,2
	add	ecx,2

	dec     number

	jnz	repcol1

	pop fs
	restore_regs			;restore protected integer registers


	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 00h
	return
        end_subroutine

;***************************************************************************
;***************************************************************************
	;THIS IS A SUBROUTINE TO DRAW AND ERASE AN ARRAY OF PIXELS
	;between x(i),y(i) and x(f),y(f)
	;subroutine to be called from the main program
	;call DARRAY1(x,y,n,color,MCOLOR,first,last):utilize a VLINE code modified


;==========================================================================
	subroutine    	DARRAY1,STDFRAME,7
;==========================================================================

	;define local storage
	begin_autos

	end_autos
	save_regs				;save required registers
	push fs
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0fh

	arg_ptr	4,ecx
	mov	cl,byte ptr [ecx]
	mov	color,cl
	mov	cx,00h

	arg_ptr	3,ecx
	mov	cx,word ptr [ecx]
	mov	number,cx

	arg_ptr	6,ecx
	mov	cx,word ptr [ecx]  		;load begin of cycle
	mov	prim,cx
       	shl	cx,1
	sub	cx,2
       	mov	dprim,ecx

	xor	ecx,ecx

	arg_ptr	7,ecx
	mov	ax,word ptr [ecx]
	mov	ultim,ax
	mov	dx,prim
	sub	ax,dx
	add	ax,1
	mov	dif,ax

	arg_ptr	1,ebx          			;point to first argument x
	add	ebx,dprim                       ;go to the element with which
						;you want to begin
	arg_ptr	2,edi                   	;point to arg 2 y
	add	edi,dprim                       ;go to the element with which
						;you want to begin

rept1:

 	OUTPUT	GRAPHICS, SET_RESET, COLOR

	mov	ax,word ptr [ebx]       	;load in ax
	mov	x,ax
	;calculate x1byte
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     xbyte,ax              		;store in x1byte
	mov	ax,00h

	mov	cx,x
	and	cx,7
	mov 	al,080h
	shr	al,cl
	mov	masca,al


	;calculate address
	mov	ax,word ptr [edi]       	;load in ax
	sub	ax,ROWS
	neg	ax
      	mov	dx,OFSET
	mul	dx                      	;multiply by 80
	add	ax,xbyte               		;add xbyte to result
	mov	addr,ax              		;store address in addr

	mov	ax,01fh
	mov	fs,ax

 	mov 	si,addr              		;get address
  	OUTPUT 	GRAPHICS, BIT_MASK, MASCA

       	mov	al,fs:[si]
       	mov	fs:[si],al             		;output color
	add	ebx,2

	add	edi,2
	dec     dif
	jnz	rept1

      	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh

	;erase
	mov	dx,00h
	arg_ptr	6,ecx
	mov	cx,word ptr [ecx]  		;load begin of cycle
	mov	prim,cx
       	shl	cx,1
	sub	cx,2
       	mov	dprim,ecx

	xor	ecx,ecx

	arg_ptr	7,ecx
	mov	ax,word ptr [ecx]
	mov	ultim,ax
	mov	dx,prim
	sub	ax,dx
	add	ax,1
	mov	dif,ax


	arg_ptr	1,ebx          			;point to first argument x
	add	ebx,dprim

	arg_ptr	2,edi                   	;point to arg 2 y
	add	edi,dprim
	xor	ecx,ecx

	arg_ptr 5,ecx
	add	ecx,dprim

rpcol1:
	mov	dl,byte ptr [ecx]
	mov	color,dl
	cmp	dl,0
	jne	q110
	mov	color,0
q110:
	mov	dx,00h
       	OUTPUT	GRAPHICS, SET_RESET, COLOR

	mov	ax,word ptr [ebx]       	;load in ax
	mov	x,ax
	;calculate x1byte
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     xbyte,ax              		;store in xbyte
	mov	ax,00h

	mov	dx,0h
	mov	dx,x
	and	dx,7
	mov	al,80h
	cmp	dl,0
	je	lal2
lol2:
	shr	al,1
	dec	dl
	cmp	dl,0
	jg	lol2

lal2:	mov	masca,al
	mov	dx,0h

	;calculate address
	mov	ax,word ptr [edi]       	;load in ax
	sub	ax,ROWS
	neg	ax
      	mov	dx,OFSET
	mul	dx                      	;multiply by 80
	add	ax,xbyte               		;add xbyte to result
	mov	addr,ax              		;store address in addr

	mov	ax,01fh
	mov	fs,ax

 	mov 	si,addr              		;get address
  	OUTPUT 	GRAPHICS, BIT_MASK, MASCA

       	mov	al,fs:[si]
       	mov	fs:[si],al             		;output color
	add	ebx,2
	add	edi,2
	add	ecx,2

	dec     dif

	jnz	rpcol1

	pop fs
	restore_regs			;restore protected integer registers


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

	begin_autos


	end_autos

	save_regs			;save required registers
	push fs
	mov	cx,0
	and	cx,7
	mov 	bl,mask20
	shr	bl,cl
	mov	masca,bl

	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0Fh

  	OUTPUT 	GRAPHICS, BIT_MASK, MASCA

	arg_ptr	1,ebx
	mov	cl,byte ptr [ebx]
	mov	colour,cl
	mov	cl,0h

 	OUTPUT	GRAPHICS, SET_RESET, COLOUR

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
	integer_2	cy1
	integer_2	cdeltay
	integer_1	ccolour
	integer_1       cmasca
	integer_2       clx
	integer_2       cxbyte
	integer_2       caddr
	end_autos

	save_regs			;save required registers
	push fs
	mov	di,0000h
clsc1:

	mov	ax,di       ;load in ax
	mov	cx,ax
	;calculate x1byte
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     cxbyte,ax              	;store in x1byte

	mov	cx,clx
	and	cx,7
	mov 	bl,cmasca
	shr	bl,cl
	mov	cmasca,bl

	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0Fh

  	OUTPUT 	GRAPHICS, BIT_MASK, CMASCA

	arg_ptr	3,ebx
	mov	cl,byte ptr [ebx]
	mov	ccolour,cl
	mov	cl,0h

 	OUTPUT	GRAPHICS, SET_RESET, COLOUR

	;calculate address
	arg_ptr	1,ebx                   ;point ro arg 3 y
	mov	ax,word ptr [ebx]       ;load in ax
	mov	cy1,ax
	sub	ax,ROWS
	neg	ax
	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,cxbyte               	;add x1byte to result
	mov	caddr,ax              	;store address in addr
	;calculate dy
	arg_ptr	2,ebx
	mov	cx,word ptr [ebx]
	mov 	ax,cy1
	add	cx,1
	sub	cx,ax
	mov	cdeltay,cx

	mov	ax,01fh
	mov	fs,ax

 	mov 	si,caddr              	;get address

	mov	cx,cdeltay
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
	;THIS IS A SUBROUTINE TO FILL WITH COLOR A RECTANGULAR ARRAY
	;subroutine to be called from the main program
	;call FILLWIN(x1,y1,x2,y2,color)

;==========================================================================
	subroutine    	FILLWIN,STDFRAME,5
;===========================================================================

	;define local storage
	;====================

	begin_autos
	integer_1 	fnrplane
	integer_1 	fbmask
	integer_1 	fmasc1
	integer_1 	fmasc2
	integer_1 	fmodxf
	integer_1 	fmodxi

	integer_2 	fxi
	integer_2 	fxf
	integer_2 	fyi

	integer_2 	fxshift
	integer_2 	fnrow
	integer_2 	fnumbyte
	integer_2 	fadresa
	end_autos

	save_regs			;save required registers
	push fs
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
	mov	fxi,ax
	mov	fxf,cx
	mov	dx,00h

	add	cx,1
	sub	cx,ax
	shr	cx,1
	shr	cx,1
	shr	cx,1
	add	cx,1
	mov	fnumbyte,cx              ;bytes/row:(x2-x1+1)>>3

;	LAST IMPROVMENT:
;===========================================
	xor	ebx,ebx
	xor	edx,edx
	;calculate x1 modulo 8
	mov 	ax,fxi			;mov x1 in ax
	mov	bx,8                    ;divide by 8
	div	bx
	mov	fmodxi,dl		;store result in temp2
	xor 	dx,dx
	xor 	bx,bx
	mov	ax,00h
	;calculate x2 modulo 8
	mov 	ax,fxf			;mov x2 in ax
	mov	bx,8                    ;divide by 8
	div	bx
	mov	fmodxf,dl		;store result in temp2


	cmp	fmodxi,0
	jg	ex0
	cmp	fmodxf,7
	jl	nochange
	sub	fnumbyte,1
	jmp	nochange
ex0:
	cmp	fmodxi,1
	je	nochange
ex1:
	cmp	fmodxi,2
	jg	ex2
	cmp	fmodxf,0
	jg	nochange
  	add	fnumbyte,1
	jmp	nochange
ex2:
	cmp	fmodxi,3
	jg	ex3
	cmp	fmodxf,1
	jg	nochange
   	add	fnumbyte,1
	jmp	nochange
ex3:
	cmp	fmodxi,4
	jg	ex4
	cmp	fmodxf,2
	jg	nochange
    	add	fnumbyte,1
	jmp	nochange
ex4:
	cmp	fmodxi,5
	jg	ex5
	cmp	fmodxf,3
	jg	nochange
    	add	fnumbyte,1
	jmp	nochange
ex5:
	cmp	fmodxi,6
	jg	ex6
	cmp	fmodxf,4
	jg	nochange
      	add	fnumbyte,1
	jmp	nochange
ex6:
	cmp	fmodxf,5
	jg	nochange
      	add	fnumbyte,1

;===========================================
nochange:
	xor 	dx,dx
	xor	eax,eax
	;calculate xbyte
	mov	ax,fxi
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     fxshift,ax              	;store in xshift

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
	mov	fyi,ax
;	mov	fyf,cx
	mov	dx,00h
	sub	ax,ROWS
	neg	ax
	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,fxshift               ;add xshift to result
	mov	fadresa,ax
	mov	si,fadresa
	xor	eax,eax
	xor	edi,edi

	mov 	ax,fyi
	add	cx,1
	sub	cx,ax
	mov	fnrow,cx

	xor	cx,cx                   ;calculate mask first byte
      	mov	cx,fxi
      	and	cx,7
      	mov 	bl,0ffh
      	shr	bl,cl
      	mov	fmasc1,bl
	xor	cx,cx
      	mov	cx,fxf                   ;calculate mask last byte
      	and	cx,7
      	mov 	bx,0ff80h
      	shr	bx,cl
      	mov	fmasc2,bl

	mov	ax,01fh
	mov	fs,ax
	xor	dx,dx
	xor	cx,cx
	xor	bx,bx
	mov	dx,fnrow
	mov	bx,OFSET
	sub	bx,fnumbyte
	mov	cx,01h

fclall:                              	;big loop after number bytes/row
	cmp	cx,01                   ;after filling a byte coloumn
	je	fnxt1
   	cmp	cx,fnumbyte
   	jne	fnxt3
   	OUTPUT 	GRAPHICS, BIT_MASK, fmasc2
   	jmp	fnxt2
fnxt1:
  	OUTPUT 	GRAPHICS, BIT_MASK, fmasc1
	jmp	fnxt2

fnxt3:
  	OUTPUT 	GRAPHICS, BIT_MASK, 0ffh
fnxt2:
	mov	bx,fnrow

fclwin:                                  ;small loop after number rows
        mov	al,fs:[si]              ;write a byte and increase row
        mov	fs:[si],al              ;output color
	sub	si,OFSET
	dec 	bx
	jg	fclwin


	mov	si,fadresa
	add 	si,cx
	inc	cx
	cmp	cx,fnumbyte
	jle	fclall
	pop fs

	restore_regs			;restore protected integer registers

 	OUTPUT 	GRAPHICS, MAP_MASK, 0Fh
	OUTPUT 	GRAPHICS, BIT_MASK, 0FFh
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 00h
	return
        end_subroutine


;***************************************************************************
;***************************************************************************


;***************************************************************************
;***************************************************************************
	;READS A RECTANGULAR ARRAY
	;subroutine to be called from the main program
	;call RDGWIN1(x1,y1,x2,y2,buf)
;==========================================================================
	subroutine     RDGWIN1,STDFRAME,5
;==========================================================================
	;define local storage
	begin_autos
	integer_1 	mapcode
	integer_1 	sto
	integer_1 	rmodxi
	integer_1 	rmodxf

	integer_2 	c_80
	integer_2 	rx_low
	integer_2 	rx_up
	integer_2 	ry_low
	integer_2 	ry_up

	integer_2 	rwxshift
	integer_2 	rwrows
	integer_2 	rwbytes
	integer_2 	rwoffset
	integer_4	byteperwin

	end_autos

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
	jl	ct1                    ;ordonate
	mov	dx,ax
	mov	ax,cx
	mov	cx,dx                   ;end change

ct1:


	mov	rx_up,cx
	mov	rx_low,ax
	;calculate ncol
	add	cx,1
	sub	cx,ax
	shr	cx,1
	shr	cx,1
	shr	cx,1
	add	cx,1
	mov	rwbytes,cx              ;bytes/row:(x2-x1+1)>>3
;===========================================
	xor	ebx,ebx
	xor	edx,edx
	;calculate x1 modulo 8
	mov 	ax,rx_low			;mov x1 in ax
	mov	bx,8                    	;divide by 8
	div	bx
	mov	rmodxi,dl			;store result
	xor 	dx,dx
	xor 	bx,bx
	mov	ax,00h
	;calculate x2 modulo 8
	mov 	ax,rx_up			;mov x2 in ax
	mov	bx,8                    	;divide by 8
	div	bx
	mov	rmodxf,dl			;store result


	cmp	rmodxi,0
	jg	ex0r
	cmp	rmodxf,7
	jl	nochanger
	sub	rwbytes,1
	jmp	nochanger
ex0r:
	cmp	rmodxi,1
	je	nochanger
ex1r:
	cmp	rmodxi,2
	jg	ex2r
	cmp	rmodxf,0
	jg	nochanger
  	add	rwbytes,1
	jmp	nochanger
ex2r:
	cmp	rmodxi,3
	jg	ex3r
	cmp	rmodxf,1
	jg	nochanger
   	add	rwbytes,1
	jmp	nochanger
ex3r:
	cmp	rmodxi,4
	jg	ex4r
	cmp	rmodxf,2
	jg	nochanger
    	add	rwbytes,1
	jmp	nochanger
ex4r:
	cmp	rmodxi,5
	jg	ex5r
	cmp	rmodxf,3
	jg	nochanger
    	add	rwbytes,1
	jmp	nochanger
ex5r:
	cmp	rmodxi,6
	jg	ex6r
	cmp	rmodxf,4
	jg	nochanger
      	add	rwbytes,1
	jmp	nochanger
ex6r:
	cmp	rmodxf,5
	jg	nochanger
      	add	rwbytes,1

;===========================================
nochanger:

	xor	eax,eax
	xor	edx,edx
	;calculate xbyte
	mov	ax,rx_low
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     rwxshift,ax              ;store in xshift


	arg_ptr	2,ecx                   ;y1
	mov	cx,word ptr [ecx]
	arg_ptr	4,eax                   ;point to arg 4 y2
	mov	ax,word ptr [eax]       ;load in ax
	cmp	cx,ax
	jl	rafct1                    ;ordonate
	mov	dx,cx
	mov	cx,ax
	mov	ax,dx                   ;end change

rafct1:

	mov	ry_up,ax
	mov	ry_low,cx
	;calculate start address
	sub	ax,ROWS
	neg	ax
	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,rwxshift             ;add xshift to result
	mov	rwoffset,ax             ;address

	;calculate nrow
	mov 	ax,ry_up
	add	ax,1
	sub	ax,cx
	mov	rwrows,ax                ;nr rows

	xor	eax,eax
	xor	edx,edx
	mov	ax,rwbytes
	mov	dx,rwrows
	mul	edx
	mov	byteperwin,eax
	mov	al,0
	mov	mapcode,al
    	;mov	di,24[ebp]
	arg_ptr	5,ebx

bigjump:
	OUTPUT GRAPHICS,READ_MAP_SELECT,mapcode

	mov	ax,01fh
	mov	fs,ax
	mov	si,rwoffset
      	mov	dx,rwrows
       	mov	ax,80
       	mov	cx,rwbytes
	sub	ax,cx
	mov	c_80,ax
	mov	sto,cl
	xor	ch,ch

lup11:
	mov 	cl,sto
;============================================================
l33:
	mov	ah,fs:[si]                      ;MOVE DISPLAY TO BUFFER
	mov	byte ptr [ebx],ah
	add	si,1
	add	ebx,1
	dec	cl
	jnz	l33
;============================================================
	add	si,c_80
	dec	dx
	jg	lup11
	inc	mapcode
	cmp	mapcode,3
	jle	bigjump

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
	;define local storage
	begin_autos
	integer_1 	mapmask
	integer_1 	stow
	integer_1 	wmodxi
	integer_1 	wmodxf

	integer_2 	c_80w
	integer_2 	rx_loww
	integer_2 	rx_upw
	integer_2 	ry_loww
	integer_2 	ry_upw

	integer_2 	rwxshiftw
	integer_2 	rwrowsw
	integer_2 	rwbytesw
	integer_2 	rwoffsetw
	integer_4	byteperwinw

	end_autos

	save_regs	;save required registers
	push 	es
;======================================================================
	;load and store arguments
;======================================================================



	arg_ptr	1,ebx          		;point to first argument x1
	mov	ax,word ptr [ebx]       ;load in ax
	xor	ecx,ecx

	;calculate ncol
	arg_ptr	3,edx                    ;x2
	mov	cx,word ptr [edx]
	cmp	ax,cx
	jl	ctq1                    ;ordonate
	mov	dx,ax
	mov	ax,cx
	mov	cx,dx                   ;end change

ctq1:

	mov	rx_loww,ax
	mov	rx_upw,cx
	add	cx,1
	sub	cx,ax
	shr	cx,1
	shr	cx,1
	shr	cx,1
	add	cx,1
	mov	rwbytesw,cx              ;bytes/row:(x2-x1+1)>>3
;===========================================
	xor	ebx,ebx
	xor	edx,edx
	;calculate x1 modulo 8
	mov 	ax,rx_loww		;mov x1 in ax
	mov	bx,8                    ;divide by 8
	div	bx
	mov	wmodxi,dl		;store result
	xor 	dx,dx
	xor 	bx,bx
	mov	ax,00h
	;calculate x2 modulo 8
	mov 	ax,rx_upw		;mov x2 in ax
	mov	bx,8                    ;divide by 8
	div	bx
	mov	wmodxf,dl		;store result in


	cmp	wmodxi,0
	jg	ex0w
	cmp	wmodxf,7
	jl	nochangew
	sub	rwbytesw,1
	jmp	nochangew
ex0w:
	cmp	wmodxi,1
	je	nochangew
ex1w:
	cmp	wmodxi,2
	jg	ex2w
	cmp	wmodxf,0
	jg	nochangew
  	add	rwbytesw,1
	jmp	nochangew
ex2w:
	cmp	wmodxi,3
	jg	ex3w
	cmp	wmodxf,1
	jg	nochangew
   	add	rwbytesw,1
	jmp	nochangew
ex3w:
	cmp	wmodxi,4
	jg	ex4w
	cmp	wmodxf,2
	jg	nochangew
    	add	rwbytesw,1
	jmp	nochangew
ex4w:
	cmp	wmodxi,5
	jg	ex5w
	cmp	wmodxf,3
	jg	nochangew
    	add	rwbytesw,1
	jmp	nochangew
ex5w:
	cmp	wmodxi,6
	jg	ex6w
	cmp	wmodxf,4
	jg	nochangew
      	add	rwbytesw,1
	jmp	nochangew
ex6w:
	cmp	wmodxf,5
	jg	nochangew
      	add	rwbytesw,1

;===========================================
nochangew:


	xor	eax,eax
	xor	edx,edx
	;calculate xbyte
	mov	ax,rx_loww
	shr	ax,1
	shr	ax,1
	shr	ax,1
      	mov     rwxshiftw,ax            ;store



	arg_ptr	2,eax                   ;point to arg 4 y2
	mov	cx,word ptr [eax]       ;load in ax
	arg_ptr	4,eax                   ;point to arg 4 y2
	mov	ax,word ptr [eax]       ;load in ax
	cmp	cx,ax
	jl	afct1                    ;ordonate
	mov	dx,cx
	mov	cx,ax
	mov	ax,dx                   ;end change

afct1:
	mov	ry_upw,ax
	mov	ry_loww,cx
	;calculate start address
	sub	ax,ROWS
	neg	ax
	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,rwxshiftw            ;add xshift to result
	mov	rwoffsetw,ax            ;address
	mov 	ax,ry_upw
	add	ax,1
	sub	ax,cx
	mov	rwrowsw,ax               ;nr rows

	xor	eax,eax
	xor	edx,edx
	mov	ax,rwbytesw
	mov	dx,rwrowsw
	mul	edx
	mov	byteperwinw,eax
	mov	al,1
	mov	mapmask,al
    	;mov	di,24[ebp]
	arg_ptr	5,ebx

bigjumpw:
	OUTPUT SEQUENCER, MAP_MASK, mapmask

	mov	ax,01fh
	mov	fs,ax
	mov	si,rwoffsetw
      	mov	dx,rwrowsw
       	mov	ax,80
       	mov	cx,rwbytesw
	sub	ax,cx
	mov	c_80w,ax
	mov	stow,cl
	xor	ch,ch

lup11w:
	mov 	cl,stow
;============================================================
l33w:
	mov	ah,byte ptr [ebx]
	mov	fs:[si],ah         	;MOVE BUFFER TO DISPLAY
	add	si,1
	add	ebx,1
	dec	cl
	jnz	l33w
;============================================================
	add	si,c_80
	dec	dx
	jg	lup11w
	mov	dl,mapmask
	shl	dl,1
	mov	mapmask,dl
	cmp	dl,8
	jle	bigjumpw

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

	begin_autos
	integer_1	vcolor
	integer_1	vbkcolor
	integer_1	temp0
	integer_1	mask0
	integer_2	pxbyte
	integer_2	n_dim
	integer_2	y
	integer_2	y_top
	integer_2	y_bottom
	integer_2	ylo
	integer_2	yhi
	integer_2	x_left
	integer_2	x_right
	integer_2	xlo
	integer_2	xhi
	integer_2	incr0
	integer_2	incr1
	integer_2	incr2
	integer_2	delta
	integer_2	eltax
	integer_2	eltay
	integer_2	adeltax
	integer_2	adeltay
	integer_2	dec_flag
	integer_4	incr
	integer_4	index


	end_autos


	save_regs			;save required registers
	push fs

	arg_ptr	5,edx
	mov	cx,word ptr [edx]
	mov	vcolor,cl
	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0Fh
	arg_ptr	4,edx
	xor	ecx,ecx
	mov	cx,word ptr [edx]
	mov	vbkcolor,cl

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
	mov	eltay,bx                  ;deltay1=y2-y1
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
	mov	eltax,di                   ;deltax=x2-x1
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
	mov	cx,eltay
	cmp	cx,0                    ;y2>y1?
	jg	fdlin3
	neg	incr0              ;inc=-80
fdlin3:
	jmp	fdlinor

fdlin1:                                     ;x2>x1
	mov	x,ax                     ;x=x1
	mov	y,dx                     ;y=y1
	mov	incr0,80             ;y2<y1; inc=+80
	cmp	eltay,0                    ;y2>y1?
	jle	fdlinor
	neg	incr0                ;y2>y1 inc=-80

fdlinor:
	mov	adeltax,di                    ;adeltax=|x2-x1|

	;calculate x>>3 for address
	mov	ax,x
	shr	ax,1
	shr	ax,1
	shr	ax,1
	mov	pxbyte,ax


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
	C_ADDRESS  y,pxbyte

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
	cmp	eltay,0          ;y2-y1>0?
	jg	fdlin51

	; here for y1>y2
	mov	ax,xhi          ;y2<y1 x=x2,y=y2
	shr	ax,1
	shr	ax,1
	shr	ax,1
	mov	pxbyte,ax

	;calculate address
	C_ADDRESS  yhi,pxbyte
	;calculate mask
	mov	cx,xhi
	and	cx,7
      	mov 	al,080h
      	shr	al,cl
	mov	mask0,al

	cmp	eltax,0           ;x2-x1>0?
	jle	fdypline           ;x2<=x1
	jmp	fdynline           ;x2>x1

fdlin51:
	mov	ax,xlo
	shr	ax,1
	shr	ax,1
	shr	ax,1
	mov	pxbyte,ax

	;calculate address
	C_ADDRESS  ylo,pxbyte
	;calculate mask
	mov	cx,xlo
	and	cx,7
      	mov 	al,080h
      	shr	al,cl
	mov	mask0,al
	cmp	eltax,0
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
	cmp	al,vbkcolor
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
      	mov     pxbyte,ax              	;store in xbyte

	mov	cx,x
	and	cx,7
	mov 	bl,mask20
	shr	bl,cl

	OUTPUT	GRAPHICS, ENABLE_SET_RES, 0Fh

  	OUTPUT 	GRAPHICS, BIT_MASK, bl


 	OUTPUT	GRAPHICS, SET_RESET,vcolor

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
	add	ax,pxbyte              	;add x1byte to result
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



	end_code
	end

;############################################################################
