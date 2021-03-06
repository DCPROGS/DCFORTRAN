; AIVPIX6.ASM
; Version of AIVPIX5 with extra argument, ndelt, such that y(i) displayed
; for i=ifst,ifst+ndelt,ifst+2*ndelt,.... -against x(j), j=1,2,3
;
; Version of AIVPIX4 that specifies number of points to erase (ndprev)
; separately from number to be drawn (ndisp).
;call AIVPIX6(ixdisp,iydisp(ifst),ndisp,iydisp(ifprev),ipen,ifirst,ibk,
;     nscale,iyoff,iyoffprev,ndprev,ndelt)
;
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
;
; ifirst>0: draws (only) ndisp points starting at iydisp(ifst) with offset=iyoff
; ifirst=0; draws as above, but first erases ndisp points (with offset
;		=iyoffprev) starting at iydisp(ifprev)
;               (for AIVPIX5, same except ndprev points erased)
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

include macros.pl4

OUTPORT		MACRO	port_addr,index,value
		mov	dx,port_addr
		mov	al,index
		out	dx,al
		inc	dx
		mov	al,value
		out	dx,al
		ENDM

	start_data
EXTRN	SEQUENCER:word, GRAPHICS:word, SET_RESET:byte, ENABLE_SET_RES:byte
EXTRN	MAP_MASK:byte, DATA_ROTATE:byte, READ_MAP_SELECT:byte, MODE_REG:byte
EXTRN	BIT_MASK:byte
EXTRN	ROWS:word, OFSET:word
;	VGA/EGA REGISTERS :
;	=================


;       GENERAL/EXTERNAL REGISTERS :
;	==========================
	STATUSC		EQU	3DAh     	;I/O PORT ADDRESS COLOR
	STATUSM		EQU	3BAh            ;MONOCHROME



;	SEQUENCER  REGISTERS:
;	======================
;	SEQUENCER	EQU	3C4h            ;I/O PORT ADDRESS
;	REGISTER NAME           INDEX
;	-------------           -----
;	RESET		EQU	00h
;	CLOCKING_MODE	EQU	01h
;	MAP_MASK	EQU	02h
;	CHAR_MAP_SELECT EQU	03h
;	MEMORY_MODE	EQU	04h



;	CRT CONTROLLER REGISTERS
;	========================
	CRTCC		EQU	3D4h        	;I/O PORT ADDRESS COLOR
	CRTCM		EQU	3B4h            ;MONOCHROME
;	REGISTER NAME           INDEX
;	-------------           -----
	CRTC_START_H	EQU	0Ch
	CRTC_START_L	EQU	0Dh
	OFFSET_INDEX	EQU	13h



;	GRAPHICS CONTROLLER REGISTERS:
;	=============================
;	GRAPHICS	EQU	3CEh            ;I/O PORT ADDRESS
;	REGISTER NAME           INDEX
;	-------------           -----
;	SET_RESET	EQU	00h
;	ENABLE_SET_RES	EQU	01h
;	COLOR_COMPARE   EQU	02h
;	DATA_ROTATE	EQU	03h
;	READ_MAP_SELECT	EQU	04h
;	MODE_REG	EQU	05h
;	BIT_MASK	EQU	08h



;	ATTRIBUTE CONTROLLER REGISTERS
;	==============================
	ATTRIBUTE	EQU	3C0h           	;I/O PORT ADDRESS
;	REGISTER NAME           INDEX
;	-------------           -----
	H_PIX_PANNING   EQU	13h
	PAN_INDEX	EQU	33h             ;13h OR 20h

	BYTES_LINE	EQU	40
	MAXSTEP		EQU	256

	end_data

	start_code

;==========================================================================
	subroutine     Aivpix6,STDFRAME,12
;==========================================================================

	;define local storage
	begin_autos
	integer_1	varipen
	integer_1	icback
	integer_1	varmask
	integer_1	nscale
	integer_2     	varx
	integer_2     	varbyte
	integer_4     	ndisp
	integer_4     	ifirst
	integer_2     	iyoff
	integer_2     	iyprev
	integer_4     	ndprev
	integer_4     	varnum
	integer_4	ylast
	integer_4	ndelt
;;	integer_4	varinc
	end_autos
	save_regs	;save required registers
	push 	fs
;======================================================================
	;load and store arguments
;call AIVPIX5(ixdisp,iydisp(ifst),ndisp,iydisp(ifprev),ipen,ifirst,ibk,
;     nscale,iyoff,iyoffprev,ndprev)
;======================================================================
	xor	ebx,ebx
       	arg_ptr	3,edx                   ;point to third arg:ndisp
       	mov	ebx,dword ptr [edx]
      	mov	ndisp,ebx             ;store it

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
       	mov	ebx,dword ptr [edx]        ;store it in varipen
       	mov	varipen,bl

     	xor	ebx,ebx
       	arg_ptr	6,edx          		 ;point to 6th arg: ifirst
       	mov	ebx,dword ptr [edx]        ;store it in ifirst
       	mov	ifirst,ebx              ;keep ifirst as int*4 to keep sign!

       	xor	ebx,ebx
       	arg_ptr	7,edx          		 ;point to 7th arg:background color
       	mov	ebx,dword ptr [edx]        ;store it in varipen
       	mov	icback,bl

	xor	ebx,ebx
       	arg_ptr	8,edx                   ;point to eigth arg:nscale
       	mov	ebx,dword ptr [edx]
      	mov	nscale,bl               ;store it

	xor	ebx,ebx
       	arg_ptr	9,edx                    ;point to ninth arg:iyoff
       	mov	ebx,dword ptr [edx]
      	mov	iyoff,bx                 ;store it

	xor	ebx,ebx
       	arg_ptr	10,edx                    ;point to tenth arg:iyoffprev
       	mov	ebx,dword ptr [edx]
      	mov	iyprev,bx                 ;store it

	xor	ebx,ebx
       	arg_ptr	11,edx                    ;point to 11th arg:ndprev
       	mov	ebx,dword ptr [edx]
      	mov	ndprev,ebx                ;store it

	xor	ebx,ebx
       	arg_ptr	12,edx                    ;point to 12th arg:ndelt
       	mov	ebx,dword ptr [edx]
	shl	ebx,1			;*2 for bytes
      	mov	ndelt,ebx                ;store it

       	arg_ptr	1,ecx          		 ;point to first argument: x

      	arg_ptr 2,edi			 ;point to second arg: y

jump4:
;=====================================================================
	;begin to calculate xbyte and the mask which is the same
	;for j and jlast
;=======================================================================
	xor	eax,eax
 	mov	ax,word ptr [ecx]       ;load in ax first element of x array
	cmp	ax,280h
	jg	jump3
	cmp	ax,0h
	jl	jump3
	mov	varx,ax                 ;store it

  	;calculate xbyte                ;xbyte=x>>3
  	shr	ax,1
  	shr	ax,1
  	shr	ax,1
  	mov	varbyte,ax              ;store it

  	mov	ax,01fh
  	mov	fs,ax                   ;point fs to address 01fh

  	mov	dx,varx           	;calculate mask
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
  	mov	varmask,ah             ;store mask

;====================================================================
	;now we have to decide what to do
;====================================================================
;	xor	eax,eax
;	arg_ptr	6,edx          		;point to 6th arg: ifirst
;      	mov	ax,word ptr [edx]       ;
	mov	eax,ifirst
       	cmp	eax,0h                  ;compare with 0:if not you
      	jg	jump1               	;draw only (no erase) if ifirst>1
;      	jne	jump1               	;go and draw first time
; also skip erase if ndprev.le.0 (but ndisp may still be >0)
	mov	eax,ndprev
       	cmp	eax,0h
	jle	jump1

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
	mov	dl,nscale
	cmp	dl,0h		;skip scale if nscale=0
	jle	skip
lyly1:
	sar	ax,1		;signed shift right (shr=unsigned)
	dec	dl
	jnz	lyly1
	add	ax,iyprev
skip:
	cmp	ax,0h
	jle	jump3
  	sub	ax,ROWS
  	neg	ax
      	mov	dx,OFSET
  	mul	dx                      ;multiply by 80
  	add	ax,varbyte             	;add xbyte to result :address
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
	and	bh,varmask             ;mask one bit
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

	cmp	al,varipen                   ;compare color with ipen
	jne	jump2                   ;if it is not equal (can be zero
       	OUTPORT	3ceh, 01h, 0fh

 	OUTPORT	3ceh, 0h, icback
	;mov	ax,01fh
	;mov	fs,ax

	OUTPORT 3ceh, 08h, varmask

       	mov	al,fs:[si]
       	mov	fs:[si],al             ;output color
jump2:
;;	add	edi,varinc             ;go back to j=jlast+inc -OLD VERSION
jump1:
	xor	eax,eax
;
; If ifirst<0 then skip DRAW
	mov	eax,ifirst
	cmp	eax,0h
	jl	jump3		;skip draw
; also skip draw if ndisp.le.0 (but ndprev may still be >0)
	mov	eax,ndisp
       	cmp	eax,0h                  ;compare with 0:if not you
	jle	jump3           ;skip draw
	xor	eax,eax
;====================================================================
	;DRAW: calculate address for x(i) and y(ifst), y(ifst+1), . . .
;====================================================================
	mov	ax,word ptr [edi]       ;load in ax
	mov	dl,nscale
	cmp	dl,0h		;skip scale if nscale=0
	jle	skip1
lyly2:
	sar	ax,1		;signed shift right (shr=unsigned)
	dec	dl
	jnz	lyly2
	add	ax,iyoff
skip1:
	cmp	ax,0h
	jle	jump3
	sub	ax,ROWS
	neg	ax
      	mov	dx,OFSET
	mul	dx                      ;multiply by 80
	add	ax,varbyte              ;add varbyte to result :address
	mov	si,ax                   ;address in si
	xor 	bl,bl                   ;bl used to accumulate the pixel value
	mov	dx,3ceh                 ;dx= grphics i/o port_addr
	mov	ax,304h                 ;ah = initial bit plane number
					;al = READ_MAP_SELECT
in2:	out	dx,ax                   ;select bit plane
	mov	bh,fs:[si]              ;bh = byte from the current bit plane
	and	bh,varmask              ;mask one bit
	neg	bh                      ;bit 7 of bh = 1 (if masked bit = 1)
	rol	bx,1                    ; bit 0 of bl = next bit from pixel value
	dec	ah                      ;next bit plane number
	jge	in2

	mov	al,bl                   ;al = pixel value
	xor	ah,ah                   ;ax = pixel value
	cmp	al,icback              ;compare pixel color with background
	jne	jump3                   ;if not background, do not do anything
       	OUTPORT	3ceh, 01h, 0fh

 	OUTPORT	3ceh, 0h, varipen  ;if 0 ,draw in ipen
	;mov	ax,01fh
	;mov	fs,ax

	OUTPORT 3ceh, 08h, varmask

       	mov	al,fs:[si]
       	mov	fs:[si],al             ;output color
jump3:
;====================================================================
	;go to next element
;====================================================================
;	add	edi,2      		;next y value to draw
	add	edi,ndelt      		;next y value to draw
	add	ecx,2                   ;next x
;	add	ylast,2                 ;next y value to erase
	mov	eax,ylast
	add	eax,ndelt
	mov 	ylast,eax
; repeat loop if EITHER ndisp or ndprev is still above zero (other one
; may have gone negative so do not use jnz) -make sure that BOTH
; are decremented while still positive)
	dec	ndprev               	;decrement ndprev
	dec	ndisp                	;decrement ndisp
;	jnz	jump4                   ;if ndisp positive, continue
	jg	jump4                   ;if not zero, continue
; check if ndprev still positive -if so continue
;	jnz	jump4                   ;if not zero, continue
	mov	eax,ndprev
       	cmp	eax,0h                  ;compare with 0:if not you
	jg	jump4                   ;if not zero, continue

; now finished
	xor	eax,eax 		;return ifirst=0
	xor	edx,edx
	mov	eax,0h
	arg_ptr	6,edx
	mov	dword ptr [edx],eax       ;retrieve first=0

	pop fs
	restore_regs			;restore protected integer registers


	OUTPORT 3ceh, 08h, 0FFh
	OUTPORT	3ceh, 01h, 00h
	return
        end_subroutine

	end_code

	end

