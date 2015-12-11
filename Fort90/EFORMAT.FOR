	subroutine EFORMAT(x,cnum)
c EFORMAT is variant of DCFORMAT for real to ASCII conversion that
c outputs E13.6 format.
c DC subroutine for use with new Hgraph, to replace HFORMAT in the
c old Hgraph.  HFORMAT was used only for Fiw.id fixed format.
c Converts the real number X to a character array CNUM with IFIELD
c digits, of which IRIGHT are to the right of the decimal point.
c VERSION USING FORTRAN 'INTERNAL WRITE' (see 9.2.1 in Language ref)
	character cnum*(*),fmt*8
c
	fmt='(E13.6)'
	WRITE(cnum,fmt) x
	RETURN
	end

