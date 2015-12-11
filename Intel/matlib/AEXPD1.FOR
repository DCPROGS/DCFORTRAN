	real*8 function AEXPd1(a,x)
c To calculate a*exp(x) with single precision, and avoiding floating underflow
c (exp1 does this for the exponential part, but if a is very small may underflow
c when multiplied by a)
c Can still get problem eg if x=-704 (limit=-708) and a is small then
c e=a*dexp1(x) can underflow when neither term separately does

	real*8 a,x,e
c
c==	e=a*dexp1(x)	!can still underflow!
	e=dexp1(x)
	if(e.eq.0.0d0) then
	   AEXPd1=0.0d0
	   RETURN
	endif
c If x is very large and negative then exponential below may overflow
c but dexp1() routine should set dexxp1(x)=0 in this case, and return
	if(a.gt.dexp1(-708.d0-x)) then	!eg if x=-704 need a>dexp(-4)
	   e=a*e
	else
	   e=0.0d0
	endif
	AEXPd1=e
	RETURN
	end


