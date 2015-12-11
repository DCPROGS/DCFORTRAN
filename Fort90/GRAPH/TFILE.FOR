FILE(1) ftest.for compiled with Lahey V5.0

	program FTEST
c
	integer*2 i
c
	call PLOTS(0,1,0)		!start Lahey graphics
	call TEST1(i)
	call PLOT(0.,0.,999)
	print 1,i
1	format(' i = ',i8)
	end


FILE(2): test1.asm assembled with 386ASM

;	call TEST1(i)
; MYSTERY: WHEN FTEST IS RUN NORMALLY IT RETURNS 87 IN i, BUT WHEN RUN
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

FILE (3): ftest.lnk for linking with 386LINK
ftest,test1
-lib graph3

