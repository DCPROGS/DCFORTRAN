	subroutine DCFORMAT(x,ifield,iright,cnum)
c DC subroutine for use with new Hgraph, to replace HFORMAT in the
c old Hgraph.  HFORMAT was used only for Fiw.id fixed format.
c Converts the real number X to a character array CNUM with IFIELD
c digits, of which IRIGHT are to the right of the decimal point.
c VERSION USING FORTRAN 'INTERNAL WRITE' (see 9.2.1 in Language ref)
c Verion of DCFORMAT that should take field width up to 20 (07/29/02 10:22am)
	character cnum*(*),cfield*20,cright*20,fmt*8
c
	call INTCONV1(ifield,cfield)
	call INTCONV1(iright,cright)
	nf=NBLANK(cfield)
	nr=NBLANK(cright)
	fmt='(F'//cfield(1:nf)//'.'//cright(1:nr)//')'
	WRITE(cnum,fmt) x
	RETURN
	end

	subroutine INTCONV1(int,istring)
c To convert an arbitrarily long integer to its ASCII equivalent, character
c by character. Max number of digits in integer=10 + one place for sign
	character*20 istring
c
	n=20		!changed 07/29/02 10:17am from n=11
c Initialise istring: NB for NBLANK to work correctly must initialise
c with spaces (char(32)), not NUL (char(0))
	do i=1,n
	   istring(i:i)=char(32)
	enddo
c check if negative
	isign=1
	if(int.lt.0) then
	 isign=-1
	 int=iabs(int)
	else if(int.eq.0) then
	 istring(1:1)=char(48)
	 goto 99
	endif
c
c Calc number of digits in the integer
	x= 1.0d-20+dlog10(dble(int))
c	ndig=1 + ifix(1.0d-20+dlog10(dble(int)))
	ndig=1 + ifix(x)
c
	int1=int		!so INT not altered
	do 1 i=ndig,1,-1
	 ival=10**(i-1)
	 n=int1/ival
	 j=ndig-i+1		!=1,2,...
	 istring(j:j)=char(n+48)
	 int1=int1-ival*n
1	continue
c
99	continue
	if(isign.eq.-1) then
	 int=-int				!restore sign
	 istring='-'//istring		!add minus sign to string
	endif
c
	istring=CHARNB(istring)		!remove trailing blanks
	return
	end



