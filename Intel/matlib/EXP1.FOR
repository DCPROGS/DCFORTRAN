	real function exp1(x)
c Note even when call UNDER0 is in force can still get NDP underflow
c with numbers less than 3.4E-4932 (approx 3.4exp(-10115)) so this
c function can replace exp() to check
c	if(x.lt.-87.) then
	if(x.lt.-80.) then
		exp1=0.0
	else
		exp1=exp(x)
	endif
	RETURN
	end


