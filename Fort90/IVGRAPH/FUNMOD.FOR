	character*(*) function getint(input,n)

	call intconv(input,getint)
	getint=getint(1:n)
	end

	character*(*) function getreal(rinput,n)

	call realtoch(rinput,getreal,n)
	end
