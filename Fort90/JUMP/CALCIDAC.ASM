;       call IDAC(IDAC,J1,J2,NPT,ION,IOFF,NOSTEP)
; NB on 80386 the following can be used as general registers: (E)AX,DX,CX,BX,
; BP,SI,DI (first four can be accessed as bytes AX=AH,AL etc

include	macros.pl4

	 start_code			; Start a code segment

	 subroutine	CALCIDAC,STDFRAME,7
;  Define some local storage
;
	begin_autos
	integer_2	m
	integer_2	n
	integer_2	j2
	integer_2	npt
	integer_2	ion
	integer_2	ioff
	integer_2	result
	integer_2	s1
	end_autos

	save_regs			; Save required integer regs

	xor	edi,edi
	mov	di,0
	mov	m,di
	mov	n,di
	xor	eax,eax
	xor	ebx,ebx
	xor	ecx,ecx
	xor	edx,edx
	arg_ptr	1,esi

	arg_ptr 4,ebx
	mov	bx,word ptr [ebx]
	mov	npt,cx

	arg_ptr 5,eax
	mov	ax,word ptr [eax]
	mov	ion,ax

	arg_ptr 6,edi
	mov	di,word ptr [edi]
	mov	ioff,di

	sub	ax,di
	div	bx
	mov	result,ax


	arg_ptr 2,ebx
	mov	bx,word ptr [ebx] ;ebx:j

	arg_ptr 3,eax
	mov	ax,word ptr [eax]
	mov	j2,ax

	xor	edx,edx
	arg_ptr	7, edx			; point to arg2: ndat=int*4
	mov	dx, word ptr [edx]
	cmp	dx,1
	jne	not_nostep

	mov	dx,npt
	add	dx,bx     ;edx:j1+npt
	mov	s1,dx
	mov	cx,ax
	shl     cx,1
	sub     cx,bx
	sub	cx,1	    ;ecx:2*j2-j1-1
	cmp	dx,ax     ;if (j1+npt.gt.j2) goto loop2
	jg	loop2
	mov	cx,dx
	add	cx,ax
	sub	cx,1       ;ecx:j2+npt-1
loop1:
	cmp     bx,s1
	jg      step
	inc     m
	mov	dx,m
	mov     ax,result
	imul	dx
	add	ax,ioff
	jmp	endl1
step:
	cmp     bx,j2
	jge     ramp
	mov	ax,ion
	jmp	endl1
ramp:
	inc	n
	mov	dx,n
	mov     ax,result
	mul	dx
	sub	ax,ion
	neg	ax
endl1:
	mov	word ptr [esi],ax
	add	esi,2
	add     bx,1
	loop    loop1
	jmp	veryend

loop2:
	cmp     bx,j2
	jge     ramp2
	inc     m
	xor	edx,edx
	mov	dx,m
	mov     ax,result
	mul	dx
	add	ax,ioff
	mov	di,ax
	jmp	endl2
ramp2:
	inc	n
	xor	edx,edx
	mov	dx,n
	mov     ax,result
	mul	dx
	sub	ax,di
	neg	ax
endl2:
	mov	word ptr [esi],ax
	add	esi,2
	add     bx,1
	loop    loop2
	jmp	veryend

not_nostep:
	mov	cx,j2
loop3:
	mov	ax,ion
	mov	word ptr [esi],ax
	add	esi,2
	loop	loop3
veryend:

	restore_regs			; Restore protected integer regs
        return
	end_subroutine

	end_code

	end

