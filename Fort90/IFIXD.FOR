	integer function IFIXD(d)
c To round a double-precsion value (d) correctly to integer*4
	real*8 d
	IFIXD=IDINT(d+dsign(0.5d0,d))
	RETURN
	end

