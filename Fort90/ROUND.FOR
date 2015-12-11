	real function ROUND(x,m)
c To round x correctly so that there are m sig figs after the
c decimal point e.g.
c round(761.7778,2)=761.7800
c round(761.7778,1)=761.8000
c round(761.7778,0)=762.0000        !(same as IFIXR but floating)
c round(761.7778,-1)=760.8000
c round(761.7778,-2)=800.8000
c
c Modified 07/29/02 09:57am for case where IFIXR gives error because number
c to be rounded is too big for integer*4 -in this case decrease m
c until ifixr works OK (more rounding)
c
	if(abs(x).lt.1.e-34) RETURN
	n=m	!so input not changed
1	d=10.0**n
	r=x*d
	if(r.gt.-2.1474836e9.and.r.lt.2.1474836e9) then
	   ROUND=float(ifixr(r))/d
	else
	   n=n-1
	   goto 1
	endif
	return
	end



