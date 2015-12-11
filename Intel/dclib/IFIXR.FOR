	integer*4 function IFIXR(R)
c To round real variable correctly (even if neg)
	x=r+SIGN(0.5,r)
	if(r.gt.-2.1474836e9.and.r.lt.2.1474836e9) then
	   IFIXR=IFIX(x)
	else
c	   call BELL(3)
	   pause ' Real number too big for integer in IFIXR'
	endif
	RETURN
	end

