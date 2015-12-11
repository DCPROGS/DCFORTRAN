	integer*4 function IFIXR(R)
c To round real variable correctly to an integer (even if neg)
	x=r+SIGN(0.5,r)
	if(r.gt.-2.1474836e9.and.r.lt.2.1474836e9) then
	   IFIXR=IFIX(x)
	else
	   call BELL(3)
	   print 1,r
1	   format(' Real number ',g14.6,' too big for integer in IFIXR')
	endif
	RETURN
	end

