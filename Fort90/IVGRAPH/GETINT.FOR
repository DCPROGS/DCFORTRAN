	character*(*) function getint(inputv,n)

	call intconv(inputv,getint)
	n=nblank1(getint)

	end
