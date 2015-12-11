	subroutine INTCONV(int,istring)
c To convert an arbitrarily long integer to its ASCII equivalent, character
c by character. Max number of digits in integer=10 + one place for sign
	character*11 istring
c
c Initialise istring: NB for NBLANK to work correctly must initialise
c with spaces (char(32)), not NUL (char(0))
	int1=int	!so int not changed

	do i=1,11
	   istring(i:i)=char(32)
	enddo

c check if negative
	isign=1
	if(int1.lt.0) then
	 isign=-1
	 int1=iabs(int1)
	else if(int1.eq.0) then
	 istring(1:1)=char(48)
	 goto 99
	endif
c
c Calc number of digits in the integer
	x= 1.0d-20+dlog10(dble(int1))
	ndig=1 + ifix(x)
c
	do i=ndig,1,-1
	 ival=10**(i-1)
	 n=int1/ival
	 j=ndig-i+1		!=1,2,...
	 istring(j:j)=char(n+48)
	 int1=int1-ival*n
	enddo	
c
99	continue
	if(isign.eq.-1) then
	 istring='-'//istring		!add minus sign to string
	endif
c
c	istring=CHARNB(istring)		!remove trailing blanks
	return
	end


