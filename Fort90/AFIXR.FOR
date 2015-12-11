	function AFIXR(x)
c To round a real value correctly (like ifixr but result real)
	x1=x+sign(0.5,x)
	afixr=x1-amod(x1,1.)
	return
	end

