;	call TADDR(i1,i2)
; MYSTERY: WHEN TPIX IS RUN NORMALLY IT RETURNS 87 IN i2, BUT WHEN RUN
; UNDER 386DEBUG THE VALUE RETURNED FROM AX IS SEEN TO BE 87 (hex) AND
; THE VALUE PRINTED BY TPIX IS 135d!! 
; NB on 80386 the following can be used as general registers: (E)AX,DX,CX,BX,
; BP,SI,DI (first four can be accessed as bytes AX=AH,AL etc

include	macros.pl4

INT2SIZE	equ	2
	extrn	gseg_:WORD

	start_data			; Start a data segment
	end_data			; End a data segment

	start_code			; Start a code segment

	subroutine	TADDR,STDFRAME,2

;
;  Define some local storage
;
;	begin_autos
;	character	lo_byte
;	integer_2	gseg_
;	end_autos

	save_regs			; Save required integer regs
	push	fs
;
;	arg_ptr	1, ebx			; point to arg1 (int*2)
;	mov	ax, word ptr [ebx]	; and get its value in AX
;
;	mov	ecx,0h
;	mov	ax,01Fh
;	mov	ds,ax	;THIS LINE CAUSES INTEGER OVERFLOW (IN _LF_PAUSE)
			; CANNOT USE DS?
;	mov	al,byte ptr ds:[ecx]
;	mov	byte ptr ds:[ecx],al
;	arg_ptr	1, ebx			; point to arg1 (int*2)
;	mov	word ptr [ebx],ax
; USE OF ES CAUSES ILLEGAL MEMORY ACCESS ERROR
; USE OF SS CAUSES INVALID DATA ACCESS ERROR WHEN ax RETURNED IN ARG 1
; Use of FS for segment selector gives no errors (even when FS is not
; saved/restored with push/pop)
	mov	ecx,0h
	mov	ax,01Fh
	mov	fs,ax
	mov	al,byte ptr fs:[ecx]
	mov	byte ptr fs:[ecx],al
	arg_ptr	1, ebx			; point to arg1 (int*2)
	mov	word ptr [ebx],ax	;returns 0
;
; Try gseg_ next
	arg_ptr	2, ebx			; point to arg2 (int*2)
	mov	ax,[gseg_]
	mov	word ptr [ebx],ax
;;	mov	bx,[_lf_graph_sel_a]
;
;
	pop	fs			;restore
	restore_regs			; Restore protected integer regs
        return
        end_subroutine

	end_code

	end

