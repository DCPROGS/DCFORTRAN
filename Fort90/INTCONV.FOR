	subroutine INTCONV(int,istring)
c To convert an arbitrarily long integer to its ASCII equivalent, character
c by character. Max number of digits in integer=10 + one place for sign
	character*11 istring
c
c Initialise istring: NB for NBLANK to work correctly must initialise
c with spaces (char(32)), not NUL (char(0))
	do 3 i=1,11
3	istring(i:i)=char(32)
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


