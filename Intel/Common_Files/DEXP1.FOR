	real*8 function dexp1(x)
c Double precision version of EXP1, to prevent floating underflow
	real*8 x
c
      dexp1=0.d0
	if(x.lt.-708.) then
		dexp1=0.0d0
	else
		dexp1=dexp(x)
	endif
	RETURN
	end


