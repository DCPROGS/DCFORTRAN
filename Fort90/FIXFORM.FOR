	subroutine FIXFORM(x,m0,n,frmt)
	character*12 frmt
	logical caplock
c Subroutine (based on FIXDEC) to give the number of significant
c (non-zero) figs both before and after the decimal point to
c allow construction of fixed point format that just takes the number
c as Fm.n where m=m0+n+3
c Tested in TFORM.FOR
c
c Number of sig figs after decimal point
c  Multiply the numbers by increasing factors of 10 until it is integral
c   Up to 5 sig fig at present)
	if(x.lt.-1.e-37.or.x.gt.1.e-37) goto 4	!x non-zero
	m0=0		!if x=0.
	n=0
	goto 5	!define frmt
c
4	continue
C	do 1 i=1,6
	do 1 i=1,5
	i1=i-1
	x1=x*(10.**i1)
c	rem=abs(amod(x1,1.))
c	rem=amod(x1,1.)
c NB it is size of this remainder (non-integer part) relative to
c size of x that really matters for judging how close number
c is to an integer. Also AMOD not ideal because 7.99999 will give
c remainder of 0.99999, wheres in fact it is virtually an integer
	ix1=ifixr(x1)
	rem=SNGL(Dble(float(ix1))-DBLE(X1))
	if(caplock()) print 21,x,x1,rem,abs(rem/x)
21    FORMAT(' X,x1,rem,abs(rem/x)= ',4(f16.8,2x))
	if(abs(rem/x).lt.1.e-6) goto 3  !integer (or nearly)- jump out
1	continue
	call BELL(1)
2	format(
     &' X= ',f16.8,' has too many sig figs in FIXFORM:',/,
     & ' x1,rem,abs(rem/x)= ',3(f16.8,2x))
C	n=5		!set it anyway
	n=4		!set it anyway
	goto 10
c
3	n=i1
c
c Now number of figs before decimal point
c10	m0=1+ifixr(alog10(abs(x)))
10	m0=1+ifix(alog10(abs(x)))
	if(m0.lt.0) m0=0
c
c Now construct the format
5	m=m0+n+3
c NB can represent integers 0-9 in ascii as char(n+48) but
c if m>9 following line does not work
	if(m.gt.9) goto 11
	frmt='(F'//char(m+48)//'.'//char(n+48)//')'
	goto 12
c if m=10-99 can split into 2 digits
11	m2=mod(m,10)	!eg if m=12, m2=2
	m1=(m-m2)/10	!eg if m=12, m1=1
	frmt='(F'//char(m1+48)//char(m2+48)//'.'//char(n+48)//')'
12	continue
	RETURN
	end

