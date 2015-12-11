	subroutine REALCONV(rnum,rstring)
c THIS VERSION HAS ERROR WHEN THERE ARE ZEROS AFTER THE DECIMAL POINT
C eg 10.1 is OK, but 10.01, 10.001 all give '10.1'!!!
c To convert an arbitrarily long real to its ASCII equivalent, character
c by character. This version keeps about 4 sig figs after the decimal
c point (102.123 could be represented in computer as 102.122999999 so
c this is rather arbitrary
	integer*2 n1,n2
	character rstring*30,istring1*11,istring2*11
c
c Initialise istring: NB for NBLANK to work correctly must initialise
c with spaces (char(32)), not NUL (char(0))
	do 3 i=1,30
3	rstring(i:i)=char(32)
c check if negative
	isign=1
	if(rnum.lt.0.) then
	 isign=-1
	 rnum=abs(rnum)
	else if(rnum.eq.0.) then
	 rstring=char(48)//'.'
	 goto 99
	endif
c
c Get part before the decimal point as an integer, and convert it in istring1
	int=(ifix(rnum))
	call INTCONV(int,istring1)
c Now the part after the decimal point
	rem=dble(rnum-float(int))
c How many sig figs has this got. Try multiplying by increasing powers
c of 10 until we get an integer (within specified precision)
	do 1 i=1,6
	i1=i
	rem1=rem*(10**i)
	diff=abs(rem1-float(ifixr(rem1)))
	if(diff.lt.1.e-4) goto 2	!jump out
1	continue
2	int=ifixr(rem1)
	call INTCONV(int,istring2)
c
	n1=nblank(istring1)
	n2=nblank(istring2)
	rstring=istring1(1:n1)//'.'//istring2(1:n2)
c debug:
c	print 20,n1,n2,istring1,istring2,istring1(1:n1),istring2(1:n2),
c     & rstring
c20	format(' n1,n2= ',2i8,/,1x,a11,' istring1',
c     & /,1x,a11,' istring2',/,1x,a11,' istring1',/,1x,a11,' istring2',
c     & /,1x,a30,' rstring')

c
99	continue
	if(isign.eq.-1) then
	 rnum=-rnum				!restore sign
	 rstring='-'//rstring		!add minus sign to string
	endif
c
	rstring=CHARNB(rstring)		!remove trailing blanks
	return
	end


