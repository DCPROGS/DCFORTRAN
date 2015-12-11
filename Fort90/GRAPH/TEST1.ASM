;	call TEST1(i)
; MYSTERY: WHEN FTEST IS RUN NORMALLY IT RETURNS 87 IN i2, BUT WHEN RUN
; UNDER 386DEBUG THE VALUE RETURNED FROM AX IS SEEN TO BE 87 (hex) AND
; THE VALUE PRINTED BY TPIX IS 135d!!

include	macros.pl4

	extrn	gseg_:WORD

	start_code			; Start a code segment

	subroutine	TEST1,STDFRAME,1

	save_regs			; Save required integer regs
;
	arg_ptr	1, ebx			; point to arg2 (int*2)
	mov	ax,[gseg_]
	mov	word ptr [ebx],ax
;
	restore_regs			; Restore protected integer regs
        return
        end_subroutine

	end_code

	end

