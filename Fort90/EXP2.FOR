	real function exp2(x)
c Like exp1(x) but also guards against overflow if x>+88
c Note even when call UNDER0 is in force can still get NDP underflow
c with numbers less than 3.4E-4932 (approx 3.4exp(-10115)) so this
c function can replace exp() to check
c	if(x.lt.-87.) then
	if(x.lt.-85.) then
	   exp2=0.0
c	else if(x.gt.88.) then
c	   exp2=exp(88.)
	else if(x.gt.85.) then
	   exp2=exp(85.)
	else
	   exp2=exp(x)
	endif
	RETURN
	end

