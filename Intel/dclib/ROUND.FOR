	real function ROUND(x,m)
c To round x correctly so that there are m sig figs after the
c decimal point e.g.
c round(761.7778,2)=761.7800
c round(761.7778,1)=761.8000
c round(761.7778,0)=762.0000        !(same as IFIXR but floating)
c round(761.7778,-1)=760.8000
c round(761.7778,-2)=800.8000
c
	d=10.0**m
	ROUND=float(ifixr(x*d))/d
	return
	end



