	subroutine INPUTni2(n,intarray,ndim)
c Integer*2 version of INPUTni
c To input an array of n integers, where numbers are specified as
c e.g. 21,25,31-39,41
c Returns n, and the numbers in intarray(ndim).
c Decimal points or 1.e3 etc are OK.  Spaces ignored.
c Negative values specified as -100,8,-12--9  (where -12--9 generates
c -12,-11,-10,-9.
c NB ranges must have number before '-' smaller than number after '-'
	character ch*1,ch1*12,chin*79
	logical digchar,isnum,numon
	integer*2 intarray(ndim)
c
	numon=isnum()	!num lock is on already on
	if(.not.numon) call NUMSET	!Spindrift library
c
	ch1='           '
	do i=1,79
	  chin(i:i)=char(32)
	enddo
c
	print 10
10	format(' (enter as ''25,28,31-39,41,...'' etc.): ')
	read 1,chin
1	format(a79)
c
c Return with input values of n,intarray() if ENTER hit, so chin is blank
	do i=1,79
	   if(ichar(chin(i:i)).ne.32) goto 11
	enddo
	RETURN
11	continue
c
c Search for comma or '-'.  Note that digchar=true for '-' sign as it
c may occur as in '-3.4e-10', or as in '-5'.
c
c First remove blanks and non-digit characters (except comma)
	n=0			!count valid characters
	do i=1,79
	   ival=ichar(chin(i:i))
	   if((digchar(ival).or.ival.eq.44).and.(ival.ne.32)) then
		n=n+1
		chin(n:n)=chin(i:i)
	   endif
	enddo		!end of i loop
	do i=n+1,79
	   chin(i:i)=char(32)
	enddo
c
c Look for comma (44) and '-' (45)
	n=0
	j=0
	iflag=0
	i2=NBLANK(chin)
	do i=1,i2
	   ch=chin(i:i)
	   ival=ichar(ch)
	   if(ival.eq.44) then 		!comma; number finished
		call CHTOREAL(ch1,real) !convert
		ch1='           '
		if(iflag.eq.0) then	!simple number
		   n=n+1
		   intarray(n)=int2(IFIXr(real))
		else
		   nlast=IFIXr(real)
		   do k=nfirst+1,nlast
			n=n+1
			intarray(n)=int2(k)
		   enddo
		   iflag=0
		endif
		j=0
	   else if(ival.eq.45.and.(ivprev.ge.48.and.ivprev.le.57)) then
c           Now bit done when '-' found that is preceded by a digit
c           from 0-9 so number just finished is interpreted as start
c           of a range. When '-' is preceded by anything else (eg ',',
c		'-','e','E' etc) it is interpreted as minus sign.
		call CHTOREAL(ch1,real) !convert unless blank
		ch1='           '
		n=n+1
		intarray(n)=int2(IFIXr(real))
		j=0
		iflag=1	!to signal that next comma=end of range
		nfirst=int4(intarray(n))
	   else
		j=j+1
		ch1(j:j)=ch
	   endif
	   ivprev=ival
	enddo
c
c Convert the number that follows the last separator
	call CHTOREAL(ch1,real) !convert
	if(iflag.eq.0) then	!simple number
	   n=n+1
	   intarray(n)=int2(IFIXr(real))
	else
	   nlast=IFIXr(real)
	   do k=nfirst+1,nlast
		n=n+1
		intarray(n)=int2(k)
	   enddo
	endif
c
	if(.not.numon) call NUMCLR	!Spindrift library
	RETURN
	end

