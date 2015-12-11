	character*(*) function getreal(rinput,n)

	call realtoch(rinput,getreal,13)
	n=nblank1(getreal)

	end
