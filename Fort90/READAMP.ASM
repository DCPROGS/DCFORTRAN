;	call READAMP(ichan,N,iadc)
; To read N channels of Amplicon ADC board (NB ichan=0,..,15)
; The n channel numbers are in ichan(1),ichan(2),...,ichan(n)
; The result for channel #i goes into iadc(i)
; In main prog have
; integer*2 ichan(16),iadc(16),n
;
; NB on 80386 the following can be used as general registers: (E)AX,DX,CX,BX,
; BP,SI,DI (first four can be accessed as bytes AX=AH,AL etc

include	macros.pl

INT2SIZE	equ	2
BASE_ADD	equ	310h		; base address of ADC board
AD_lo		equ	BASE_ADD
AD_hi		equ	BASE_ADD+1
START		equ	BASE_ADD+2
MUX  		equ	BASE_ADD+3

;	start_data			; Start a data segment
;;  public allows 'called' to be accessible to other modules, if
;;  necessary
;	public	called
;	integer_2	called,1,0	; Initialized to zero
;	character	message,1,'Array addressing error$'
;	end_data			; End a data segment

	start_code			; Start a code segment

	subroutine	readamp,STDFRAME,3

;
;  Define some local storage
;
	begin_autos
	character	lo_byte
	integer_2	nchan
	integer_2	ichan
	integer_2	count
	end_autos

	save_regs			; Save regs ebx,edi,esi

	arg_ptr	2, ebx			; point to arg2 (int*2)
;	mov	nchan, word ptr [ebx]	; and get # of channels to be read
	mov	ax, word ptr [ebx]	; and get # of channels to be read
	mov	nchan,ax
;
	arg_ptr	1, ebx			; point to arg1 (int*2) = ichan(1)
	arg_ptr	3, ecx			; point to arg3 (int*2) = iadc(1)
; To get ichan(i) into AX need 'mov ax,word ptr [ebx+(i-1)*INT2SIZE] (NB
; ichan(1) in this context is actually the value in the call eg if called
; as 'call TARRAY(ichan(5),,...)' then word ptr [ebx] refers to ichan(5) etc)
; read channel # specified in AL
next:	xor	ax,ax
;	xor	edx,edx     ;clear DX (needed when called from SOME progs)
	mov	al, byte ptr [ebx]	; put next channel # in AL
	mov	ichan,ax		; keep copy in ichan
	mov	dx,MUX			; must use DX for port# > 256d
	out	dx,al			; output channel # to multiplexor

; insert delay after changing channel #, before START
	mov	count,130h
del1:	dec	count
	jnz	del1
;
	mov	al,0			; write anything to 'start' to go
	mov	dx,START		; must use DX for port# > 256d
	out	dx,al
;
; Insert delay between START and reading ADC (CX already in use, so use COUNT)
; NB THIS DELAY IS CRITICAL: count of 30h or less does not work at all
; Count of 40h is marginal -gives some big jitter (e.g. iadc jumps
; between 86 and 255). Count of 50h OK (on Dell 450)
;	10 microsec delay needed here
;	mov	cx,7FFFh
;	mov	cx,40h
;delay:	dec	cx
;	jnz	delay
;	mov	count,7FFFh
	mov	count,60h
;	mov	count,40h
delay:	dec	count
	jnz	delay
;
	mov	dx,AD_lo		; must use DX for port# > 256d
	in	al,dx
	mov	lo_byte,al		;keep the lo byte
	mov	dx,AD_hi		; must use DX for port# > 256d
	in 	al,dx
	mov	ah,al			;move AL to high byte
	mov	al,lo_byte		;put low byte back into AL
; ADC value now in EAX.  If the 12th bit (bit 11 when number 0-15) is set
; this corresponds to a negative value (see Amplicon book), and in this
; case, to turn the result into a proper 2s complement value, that will
; return a negative value to Fortran, must set bits 12-15 also
;
	cmp	ax,800h
	jl	done		;if less than 800h, bit 11 is not set
;
;	or	ax,topbyte		; set bits 12-15
;	or	eax,0000f000h		; set bits 12-15
	or	ax,0f000h		; set bits 12-15 (need leading 0 to
;					; indicate it is a number)
; when done put output in iadc(ichan)

;NB following two do not work -scal factor (*int2size) nust be 2,4 or 8
; and offset added to register must be a constant(immediate), so must calculate
; address first.  SHL can be used to multiply by 2 the required number of times
; and the number of times MUST be in the CL register. Base address for iadc() is
; in edx so want address as edx+ichan*int2size=edx+2*ichan so in this case
; just need a single shift left
done:	xor	edi,edi		; clear edi (upper byte may not be clear)
	mov	di,ichan	; copy current channel # to edi
	shl	di,1		; *2 for words
;NB must use ECX not CX for address of output array (latter works only
; if value us less than 16 bits!!)
;;	add	di,cx		; add to address of 1st element of iadc()
;;	mov	word ptr [di],ax	;to get address of iadc(ichan)
	add	edi,ecx		; add to address of 1st element of iadc()
	mov	word ptr [edi],ax	;to get address of iadc(ichan)
					;and put result there
;	mov	word ptr [ecx+dx*int2size],ax		;still illegal!!!
;	mov	word ptr [ecx+ichan*int2size],ax        ;illegal
	add	ebx,int2size		; get address for next channel #
	dec	nchan			; done all channels yet?
	jnz	next			; do next channel
;
	restore_regs			; Restore protected integer regs
        return
        end_subroutine

	end_code

	end

