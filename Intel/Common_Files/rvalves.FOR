	subroutine rvalves(ndim,numax,ivalves)
c     numax<=ndim
c     ndim =max number

	integer ivalves(ndim)
	logical repeat
	common/rand/ix1,iy1,iz1

	iwrite=0
	call RANDSKV(ix1,iy1,iz1,1,repeat)		!write IX,IY,IZ
	call RANPERM(ivalves,ndim,numax)
	call RANDSKV(ix1,iy1,iz1,1,repeat)		!write IX,IY,IZ

	end
