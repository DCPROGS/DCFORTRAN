	subroutine REALASC(x,rstr)
c THIS PROG NOW REPLACED BY NEW DCFORMAT, EFORMAT etc BASED ON INTERNAL WRITE!
c Converts a real number,x, to a character string of length 12 (at
c present). Like REALTOCH except that the string is always in the exponent
c form (as required by ASCIO) so last 4 characters always have the
c form 'E-01' , and the mantissa always has a decimal point as 2nd character
c eg '9.999' or '1.001'.  Thus have 12-5=7 characters for the significant
c figs.
c
	integer*2 n1,n2,n3
	character rstr*12
	character string*30,istr1*11,istr2*11,istr3*11
	logical equal,less
c	logical expform
c
c define functions
	equal(x,y)=abs(x-y).le.1.e-5
	less(x,y)=x-y.lt.-1.e-5
c
	real=x		!so input x not changed
c
	ncn=12
c Keep count of number of digits to be put in string
	ndig=0
c Initialise string: NB for NBLANK to work correctly must initialise
c with spaces (char(32)), not NUL (char(0))
	do 3 i=1,ncn
3	string(i:i)=char(32)
c check if negative or zero
	isign=1
	if(real.lt.0.) then
	 isign=-1
	 ndig=1	!for initial - sign
	 real=abs(real)
	else if(real.eq.0.) then
	 string='0.0E+00'
	 goto 99
	endif
c Get no of digits before decimal point (nb) and exponent
c (if expform, for which nb=1)
	iexp=ifix(alog10(real))		!e.g.  123.4 gives iexp=2
	nb=iexp+1
	if(real.lt.1.0) then
	   iexp=iexp-1	!0.0123 gives iexp=-2
	   nb=1
	endif
c Now must check size to decide if E notation to be used
c	expform=(real.lt.0.001.or.real.ge.100000.0)	!E format
c	expform=.true.
c	if(expform) then
	   nb=1
	   real=real*(10.**(-iexp))	!e.g.1.234e2
	   ie=iabs(iexp)
	   if(ie.ge.1.and.ie.le.9) ne=1	!1 digit in iexp
	   if(ie.ge.10.and.ie.le.99) ne=2	!2 digits in iexp
	   ndig=ndig+ne
	   if(iexp.lt.0) ndig=ndig+1		!for - sign after E
	   ndig=ndig+nb+2				!for 'm.nnnE'
c	endif
c Get part before the decimal point as an integer, and convert it in istr1
c Now the part after the decimal point
c Fix problem referred to at top of prog by doing this way:
	rem=AMOD(real,1.0)
	int=IFIXR(real-rem)
	call INTCONV(int,istr1)
	n1=nblank(istr1)
c
	na=ncn-ndig
	if(na.lt.0) na=0
c Try multiplying REM by increasing powers of 10 until we get an integer
c (within specified precision)
c Problem if REM contains leading zeros (eg 0.002)-count them in nz
	nz=0
	do i=1,6
	  rem1=rem*(10**i)
	  int=ifixr(rem1)
c	  if(rem1.lt.1.0) nz=nz + 1
c======next line wrong eg for 0.006, as rem1=0.6 rounds to 1 so nz too small
c	  if(int.lt.1) nz=nz + 1
	  if(less(rem1,1.0)) nz=nz + 1
	  diff=abs(rem1-float(int))
c=	  if(diff.lt.1.e-4.or.i.gt.na) goto 2	!jump out
c	  if(diff.lt.1.e-5.or.i.gt.na) goto 2	!jump out
	  if(equal(diff,0.).or.i.gt.na) goto 2	!jump out
	enddo
c2	int=ifixr(rem1)
2	continue
	call INTCONV(int,istr2)
c Add NZ zeros before the integer in istr2
	if(nz.gt.0) then
	   do i=1,nz
		istr2='0'//istr2
	   enddo
	endif
c Check istr2 for trailing zeroes
	n2=nblank(istr2)
	do 4 i=n2,1,-1
	  if(istr2(i:i).eq.'0') then
	     n2=n2-1
	  else
	     goto 6		!jump out when non-zero value found
	  endif
4	continue
6	if(n2.eq.0) n2=1
c

c	if(expform) then
	   if(n2.gt.na) n2=na
	   string=istr1(1:n1)//'.'//istr2(1:n2)
	   call INTCONV(ie,istr3)
c if only one digit in exponent, add a 0 in front of it
	   call GBLANK(istr3,11,n11,n22)
	   if(n11.eq.n22) istr3='0'//istr3(n11:n11)	!e.g. '02'
	   n3=nblank(string)
	   if(iexp.gt.0) then
		string=string(1:n3)//'E'//istr3
	   else
		string=string(1:n3)//'E-'//istr3
	   endif
c	endif
c
99	continue
	if(isign.eq.-1) then
	 real=-real				!restore sign
	 string='-'//string		!add minus sign to string
	endif
c
	rstr=string(1:ncn)
c
	RETURN
	end



