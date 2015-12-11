; AIVPIX5.ASM
; Version of AIVPIX3 that divides each iydisp(i) by 2**nscale, and then
; adds iyoff, before using it (nscale and iyoff=integer*4). idata() not changed.
; For correct erasure specify iyoff for previous display separately=iyoffprev
; Set nscale=0 to skip scaling and offset.
; NB in this version assume NDISP and NSCALE are same for draw and erase
;
;call AIVPIX4(ixdisp,iydisp(ifst),ndisp,iydisp(ifprev),ipen,ifirst,ibk,
;     nscale,iyoff,iyoffprev)
; Draws ndisp points starting at iydisp(ifst) in colour=ipen, but overwrites
; only pixels that are already in background colour=ibk  (and sets
; ifirst=0 before leaving).
; ifirst>0: draws ndisp points starting at iydisp(ifst) with offset=iyoff
; ifirst=0; draws as above, but first erases ndisp points (with offset
;		=iyoffprev) starting at iydisp(ifprev)
; ifirst<0; erases only
;
; This is version of AIVPIX1 that has the modifs made in
; BOTH AIVPIX0 and AIVPIX2, viz:
; (1) lacks the jfst argument, so MUST be called with idata(ifst), rather
;   than just 'idata'.  This is more convenient for arrays that start with
;   negative indices (see comments in AIVPIX0)
; (2) Erases (if ifirst=0) the ndisp points starting at idata(ifprev)
;    (i.e. ifprev is specified rather than INC to specify which points
;    are to be erased). NB must call with idata(ifprev) so address to
;	start erasing is known (alternative would be to call with
;	idata(ifst),ifst,ifprev, in call, so diff between ifst and
;	islast can be used to calc addres of idata(ifprev))
;


;====================================================================

include macros.pl
include ioana.pl4

;====================================================================
	start_data
	end_data
;====================================================================

	start_code

;==========================================================================
	subroutine     Aivpix5,STDFRAME,10
;==========================================================================

	;define local storage
	begin_autos
	integer_4     	varnum
	integer_4	ylast
	end_autos
	save_regs	;save required registers
	push 	fs
;======================================================================
	;load and store arguments
;call AIVPIX4(ixdisp,iydisp(ifst),ndisp,iydisp(ifprev),ipen,ifirst,ibk,
;     nscale,iyoff,iyoffprev)
;======================================================================
	xor	ebx,ebx
       	arg_ptr	3,edx                   ;point to third arg:ndisp
       	mov	ebx,dword ptr [edx]
      	mov	n_dim,bx             ;store it

	xor	eax,eax
      	arg_ptr	4,edx          		 ;point to 4th arg: address of y(ifprev)
	mov	ylast,edx
;      	arg_ptr	4,edx          		 ;point to 4th arg:ifprev
;     	mov	eax,dword ptr [edx]      ;
;     	shl	eax,1                    ;multiply by 2
;     	sub	eax,2                    ;now calculate:j=ifprev-1
;     	mov	ifprev,eax               ;store it in ifprev

     	xor	ebx,ebx
       	arg_ptr	5,edx          		 ;point to 5th arg:color
       	mov	ebx,dword ptr [edx]        ;store it in color
       	mov	color,bl

     	xor	ebx,ebx
       	arg_ptr	6,edx          		 ;point to 6th arg: ifirst
       	mov	ebx,dword ptr [edx]        ;store it in index
       	mov	index,ebx              ;keep ifirst as int*4 to keep sign!

       	xor	ebx,ebx
       	arg_ptr	7,edx          		 ;point to 7th arg:background color
       	mov	ebx,dword ptr [edx]        ;store it in bkcolor
       	mov	bkcolor,bl

	xor	ebx,ebx
       	arg_ptr	8,edx                   ;point to eigth arg:nscale
       	mov	ebx,dword ptr [edx]
      	mov	temp0,bl               ;store it

	xor	ebx,ebx
       	arg_ptr	9,edx                    ;point to ninth arg:iyoff
       	mov	ebx,dword ptr [edx]
      	mov	yhi,bx                 ;store it

	xor	ebx,ebx
       	arg_ptr	10,edx                    ;point to tenth arg:iyoffprev
       	mov	ebx,dword ptr [edx]
      	mov	ylo,bx                 ;store it

       	arg_ptr	1,ecx          		 ;point to first argument: x

      	arg_ptr 2,edi			 ;point to second arg: y

jump4:
;=====================================================================
	;begin to calculate xbyte and the mask which is the same
	;for j and jlast
;=======================================================================
 	mov	ax,word ptr [ecx]       ;load in ax first element of x array
	cmp	ax,280h
	jg	jump3
	cmp	ax,0h
	jl	jump3
	mov	x,ax                 ;store it

  	;calculate xbyte                ;xbyte=x>>3
  	shr	ax,1
  	shr	ax,1
  	shr	ax,1
  	mov	xbyte,ax              ;store it

  	mov	ax,01fh
  	mov	fs,ax                   ;point fs to address 01fh

  	mov	dx,x           	;calculate mask
  	and	dx,7
        xor	dl,7
  	mov	ah,1
  	cmp	dl,0
  	je	go1
go2:
  	shl     ah,1
  	dec	dl
  	cmp	dl,0
  	jg	go2
go1:
  	mov	mask0,ah             ;store mask

;====================================================================
	;now we have to decide what to do
;====================================================================
;	xor	eax,eax
;	arg_ptr	6,edx          		;point to 6th arg: ifirst
;      	mov	ax,word ptr [edx]       ;
	mov	eax,index
       	cmp	eax,0h                  ;compare with 0:if not you
      	jg	jump1               	;draw only (no erase) if ifirst>1
;      	jne	jump1               	;go and draw first time

;====================================================================
; ERASE: calculate address  for x(i) and y(jlast)
;====================================================================
	xor	eax,eax
	xor	edx,edx
;	sub	edi,varinc
; y index for draw is ifst, ifst+1,....
;  For erase want index=ifprev, ifprev+1, . . .
;  	mov	ax,word ptr [edi]       ;load in ax
	mov	eax,ylast	;address for ydisp(ifprev),ydisp(ifprev+1),...
  	mov	ax,word ptr [eax]       ;load in ax
;==================================================
	mov	dl,temp0
	cmp	dl,0h		;skip scale if nscale=0
	jle	skip
lyly1:
	sar	ax,1		;signed shift right (shr=unsigned)
	dec	dl
	jnz	lyly1
	add	ax,ylo
skip:
	cmp	ax,0h
	jle	jump3
  	sub	ax,ROWS
  	neg	ax
      	mov	dx,OFSET
  	mul	dx                      ;multiply by 80
  	add	ax,xbyte             	;add xbyte to result :address
	mov	si,ax                   ;address in si

;====================================================================
	;read pixel ( x(i),y(ifprev),ic )
;====================================================================
	xor 	bl,bl                   ;bl used to accumulate the pixel value
	mov	dx,3ceh                 ;dx= grphics i/o port_addr
	mov	ax,304h                 ;ah = initial bit plane number
					;al = READ_MAP_SELECT
in1:	out	dx,ax                   ;select bit plane
	mov	bh,fs:[si]              ;bh = byte from the current bit plane
	and	bh,mask0             ;mask one bit
	neg	bh                      ;bit 7 of bh = 1 (if masked bit = 1)
	rol	bx,1                    ; bit 0 of bl = next bit from pixel value
	dec	ah                      ;next bit plane number
	jge	in1

	mov	al,bl                   ;al = pixel value
	xor	ah,ah                   ;ax = pixel value

;====================================================================
       	;now you have in ax the color read from the display
	;at the current position and start to
	;erase:=write pixel ( x(i),y(ifprev),0 )
;====================================================================

	cmp	al,color                   ;compare color with ipen
	jne	jump2                   ;if it is not equal (can be zero
       	OUTPUT	3ceh, 01h, 0fh

 	OUTPUT	3ceh, 0h, bkcolor
	;mov	ax,01fh
	;mov	fs,ax

	OUTPUT 3ceh, 08h, mask0

       	mov	al,fs:[si]
       	mov	fs:[si],al             ;output color
jump2:
;;	add	edi,varinc             ;go back to j=jlast+inc -OLD VERSION
jump1:
	xor	eax,eax
;
; If ifirst<0 then skip DRAW
	mov	eax,index
	cmp	eax,0h
	jl	jump3		;skip draw
	xor	eax,eax
;====================================================================
	;DRAW: calculate address for x(i) and y(ifst), y(ifst+1), . . .
;====================================================================
	mov	ax,word ptr [edi]       ;load in ax
	mov	dl,temp0
	cmp	dl,0h		;skip scale if nscale=0
	jle	skip1

lyly2:
	sar	ax,1		;signed shift right (shr=unsigned)
	dec	dl
	jnz	lyly2
	add	ax,yhi
skip1:
	cmp	ax,0h
	jle	jump3
	sub	ax,ROWS
	neg	ax
      	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,xbyte              ;add xbyte to result :address
	mov	si,ax                   ;address in si
	xor 	bl,bl                   ;bl used to accumulate the pixel value
	mov	dx,3ceh                 ;dx= grphics i/o port_addr
	mov	ax,304h                 ;ah = initial bit plane number
					;al = READ_MAP_SELECT
in2:	out	dx,ax                   ;select bit plane
	mov	bh,fs:[si]              ;bh = byte from the current bit plane
	and	bh,mask0              ;mask one bit
	neg	bh                      ;bit 7 of bh = 1 (if masked bit = 1)
	rol	bx,1                    ; bit 0 of bl = next bit from pixel value
	dec	ah                      ;next bit plane number
	jge	in2

	mov	al,bl                   ;al = pixel value
	xor	ah,ah                   ;ax = pixel value
	cmp	al,bkcolor                   ;compare pixel color with 0
	jne	jump3                   ;if not,do not do anything
       	OUTPUT	3ceh, 01h, 0fh

 	OUTPUT	3ceh, 0h, color  ;if 0 ,draw in ipen
	;mov	ax,01fh
	;mov	fs,ax

	OUTPUT 3ceh, 08h, mask0

       	mov	al,fs:[si]
       	mov	fs:[si],al             ;output color
jump3:
;====================================================================
	;go to next element
;====================================================================
	add	edi,2      		;next y value to draw
	add	ecx,2                   ;next x
	add	ylast,2                 ;next y value to erase

	dec	n_dim                ;decrease ndisp
	jnz	jump4                   ;if not zero, continue

	xor	eax,eax 		;return ifirst=0
	xor	edx,edx
	mov	eax,0h
	arg_ptr	6,edx
	mov	dword ptr [edx],eax       ;retrieve first=0

	pop fs
	restore_regs			;restore protected integer registers


	OUTPUT 3ceh, 08h, 0FFh
	OUTPUT	3ceh, 01h, 00h
	return
        end_subroutine

	end_code

	end

