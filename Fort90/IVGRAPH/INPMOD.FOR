	character*(*) function mgetint(minput,n)


	call intconv(minput,mgetint)
	n=nblank1(mgetint)
	end

	character*(*) function mgetreal(rinput,n)

	call realtoch(rinput,mgetreal,13)
	n=nblank1(mgetreal)
	end
