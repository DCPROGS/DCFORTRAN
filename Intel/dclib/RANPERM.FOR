	subroutine RANPERM(iperm,n,ndim)
c Lahey version of RPERM1. Array dimension is given as arg.
c To return a random permutation of the digits 1 to n in array IPERM
c Prob no need to initialise IPERM every time it is called. Just set
c IPERM(i)=i before first call
c
	integer IPERM(n)
	common/rand/ix,iy,iz
	do i=n,2,-1
c   select random integer in range 1 to i (NB no need to do i=1 as
c   only one choice left)
	   j=1 + ifix(0.5+float(i-1)*random())
c   swap iperm(j) with the last element not yet permuted, iperm(i), so
c   the random perm builds up in elements n,n-1,n-2,... of IPERM
	   itemp=iperm(i)
	   iperm(i)=iperm(j)
	   iperm(j)=itemp
	enddo
c
	RETURN
	end
