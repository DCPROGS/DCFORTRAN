;       call SCALINT(idata,ndat,nscale,iyoff)
; To divide idata(1) to idata(ndat) by 2**nscale, and then add iyoff
; NB on 80386 the following can be used as general registers: (E)AX,DX,CX,BX,
; BP,SI,DI (first four can be accessed as bytes AX=AH,AL etc

include	macros.pl

	 start_code			; Start a code segment

	 subroutine	scalint,STDFRAME,4
;  Define some local storage
;
	begin_autos
	integer_1	nscale
	end_autos

	save_regs			; Save required integer regs

	xor	ebx,ebx
	arg_ptr	2, ebx			; point to arg2: ndat=int*4
	mov	ebx, dword ptr [ebx]

	xor	eax,eax
	arg_ptr	3, eax			; point to arg3 nscale=int*4
	mov	ecx, dword ptr [eax]
	mov	nscale,cl
;
	xor	esi,esi
	arg_ptr	4, eax			; point to arg3: iyoff=int*4
	mov	esi, dword ptr [eax]
;
	arg_ptr	1,edi			; point to arg1: int*2 idata())
jumpy:
	movsx	eax,word ptr [edi]	;keep sign of iy()
	mov	cl,nscale
	shr	eax,cl
	add	eax,esi
	mov	word ptr [edi],ax
	add	edi,2
	dec	ebx

	jnz	jumpy

	restore_regs			; Restore protected integer regs
        return
	end_subroutine

	end_code

	end

