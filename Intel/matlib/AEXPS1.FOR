	real*4 function AEXPs1(a,x)
c To calculate a*exp(x) with single precision, and avoiding floating underflow
c (exp1 does this for the exponential part, but if a is very small may underflow
c when multiplied by a)
c Can still get problem eg if x=-704 (limit=-708) and a2 is small then
c e=a2*dexp1(x2) can underflow when neither term separately does

	real*8 a2,x2,e
c
	a2=dble(a)
	x2=dble(x)
c==	e=a2*dexp1(x2)	!can still underflow!
	e=dexp1(x2)
	if(e.eq.0.0d0) then
	   AEXPs1=0.0
	   RETURN
	endif
	if(a2.gt.dexp(-708.d0-x2)) then	!eg if x2=-704 need a2>dexp(-4)
	   e=a2*e
	else
	   e=0.0d0
	endif
	if(dabs(e).lt.1.d-36) then
	   AEXPs1=0.0
	else
	   AEXPs1=sngl(e)
	endif
	RETURN
	end


