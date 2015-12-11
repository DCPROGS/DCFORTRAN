;	call TCOM(i)
;
; NB on 80386 the following can be used as general registers: (E)AX,DX,CX,BX,
; BP,SI,DI (first four can be accessed as bytes AX=AH,AL etc

include	macros.pl4

;INT2SIZE	equ	2
;BASE_ADD	equ	310h		; base address of ADC board

	start_data			; Start a data segment
;;  public allows 'called' to be accessible to other modules, if
;;  necessary
;	public	called
;	integer_2	called,1,0	; Initialized to zero
;	character	message,1,'Array addressing error$'
	end_data			; End a data segment

;	start_common	foo
;	start_common		;unnamed common
;	integer_2	ix,10
;	end_common


	start_code			; Start a code segment

	subroutine	tcom,STDFRAME,1

;
;  Define some local storage
;
;	begin_autos
;	character	lo_byte
;	integer_2	nchan
;	integer_2	ichan
;	end_autos

	save_regs			; Save required integer regs

	arg_ptr	1, ebx			; point to arg1 (int*2)
	mov	ax, word ptr [ebx]	; and get its value in AX
;	mov	dx,ix			;get value from common
	mov	dx,ax			;get value from common
;done:	mov	di,ichan	; copy current channel # to edi
;	shl	di,1		; *2 for words
;	add	di,cx		; add to address of 1st element of iadc()
;	mov	word ptr [di],ax	;to get address of iadc(ichan)
;					;and put result there
;;	mov	word ptr [ecx+dx*int2size],ax		;still illegal!!!
;;	mov	word ptr [ecx+ichan*int2size],ax        ;illegal
;
	restore_regs			; Restore protected integer regs
        return
        end_subroutine

	end_code

	end

