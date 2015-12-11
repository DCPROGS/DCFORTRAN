	subroutine DCFORMAT(x,ifield,iright,cnum)
c DC subroutine for use with new Hgraph, to replace HFORMAT in the
c old Hgraph.  HFORMAT was used only for Fiw.id fixed format.
c Converts the real number X to a character array CNUM with IFIELD
c digits, of which IRIGHT are to the right of the decimal point.
c VERSION USING FORTRAN 'INTERNAL WRITE' (see 9.2.1 in Language ref)
	character cnum*(*),cfield*11,cright*11,fmt*8
c
	call INTCONV(ifield,cfield)
	call INTCONV(iright,cright)
	nf=NBLANK(cfield)
	nr=NBLANK(cright)
	fmt='(F'//cfield(1:nf)//'.'//cright(1:nr)//')'
	WRITE(cnum,fmt) x
	RETURN
	end

