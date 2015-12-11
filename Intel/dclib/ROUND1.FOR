	real function ROUND1(x,m)
c To round x correctly so that there are m sig figs altogether
c (cf ROUND which gives m sig figs after the decimal point)
c
c (see TRC.FOR for tests)
	real*8 x2
c
	if(x.eq.0.0) RETURN
	sign1=1.
	if(x.lt.0.) sign1=-1.
	z=abs(x)
	i=ifixr(alog10(z))	!eg = 1 for x=22.1
c Problem here if number of sig figs is large, so d is large and ifixr(z*d) is
c too big for integer*4
	d=10.0**(m-i-1)
	x1=z*d
	if(x1.gt.-2.1474836e9.and.x1.lt.2.1474836e9) then
	   ROUND1=sign1*float(ifixr(x1))/d
	else
	   x2=dble(x1+SIGN(0.5,x1))     !NB x is positive
	   x2=x2 - DMOD(x2,1.0d0)     !so this rounds down
	   ROUND1=sign1*sngl(x2/dble(d))
	endif
c
	return
	end



