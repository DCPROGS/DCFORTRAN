	subroutine FIXDEC1(z,m0,n,nmax)
	logical caplock
c Subroutine (based on FIXFORM and FIXDEC) to give the number of significant
c (non-zero) figs both before and after the decimal point to
c allow construction of fixed point format that just takes the number
c as Fm.n where m=m0+n+3
c NMAX is the max number of sig figs AFTER the decimal point to be allowed
c (eg might take nmax=2 or 3 for numbers on axis labels, for which this
c prog is primarily intended). The input number, z, is FIRST rounded
c correctly to nmax sig figs after the decimal point, and then format
c is determined. Thus, for example, if z=1.00003 and nmax=2 the rounded
c value is 1.00 (n=nmax=2), but output value is n=0 because there are,
c after rounding, no sig figs after point. (If format was determined first
c would get n=5, and format would output n=nmax=2 so label determined
c with HFORMAT would be '1.00' rather than '1')
c  Input value of z is unchanged (not rounded)
c Tested in TFORM.FOR
c Check whether z=0.
	if(z.gt.-1.e-37.and.z.lt.1.e-37) then	!z is 'zero'
	   m0=0		!if z=0.
	   n=0
	   goto 5	!define frmt
	endif
c
c Round to nmax sig figs after the decimal point
c	   z1=10.**(-nmax)*ifixr(z*10.**nmax)
	   z1=ROUND(z,nmax)
c
c Number of sig figs after decimal point
c  Multiply the numbers by increasing factors of 10 until it is integral
c   Up to 5 sig fig at present)
	do 1 i=1,6
c	do 1 i=1,5
	i1=i-1
	z2=z1*(10.**i1)
c NB it is size of this remainder (non-integer part) relative to
c size of z that really matters for judging how close number
c is to an integer. Also AMOD not ideal because 7.99999 will give
c remainder of 0.99999, whereas in fact it is virtually an integer
	iz2=ifixr(z2)
	rem=SNGL(Dble(iz2)-DBLE(z2))
c	if(caplock()) print 21,z,z2,rem,abs(rem/z)
c21    FORMAT(' z,z2,rem,abs(rem/z)= ',4(f16.8,2x))
c	if(abs(rem/z).lt.1.e-6) goto 3  !integer (or nearly)- jump out
	if(abs(rem/z).lt.1.e-5) goto 3  !integer (or nearly)- jump out
1	continue
c	if(caplock()) then
c	   call BELL(1)
c	   print 2,z,z2,rem,abs(rem/z)
c2	   format(
c     &   ' z= ',f16.8,' has too many sig figs in FIXFORM:',/,
c     &   ' z2,rem,abs(rem/z)= ',3(f16.8,2x))
c	endif
	n=5		!set it anyway
c	n=4		!set it anyway
	goto 10
c
3	n=i1
c
c Now number of figs before decimal point
c10	m0=1+ifixr(alog10(abs(z)))
10	m0=1+ifix(alog10(abs(z)))
	if(m0.lt.0) m0=0
c
5	continue
	RETURN
	end

