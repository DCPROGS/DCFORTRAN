	real*8 function DROUND(x,m)
c Double precision version of ROUND(x,m)
c To round x correctly so that there are m sig figs after the
c decimal point e.g.
c
	real*8 x,d
c
	d=10.0d0**m
	DROUND=dble(IFIXD(x*d))/d
	return
	end


