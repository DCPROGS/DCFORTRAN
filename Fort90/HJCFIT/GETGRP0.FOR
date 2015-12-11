	subroutine GETGROUP(rootX,kX,kdim)
c To identify groups of 'identical' roots for asymptotic HJC distribution
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 rootX(kdim)
c for common/grp/
	logical checkgrp,grouped
	integer ngrp(100)		!number of roots in the ith group
	real*8 rgmin(100),gmean(100)
	common/grp/checkgrp,grouped,ngrp,ngroup,rcrit,rgmin,gmean
c
	do i=1,kX
	   ngrp(i)=1
	   rgmin(i)=1.d20
	enddo
	ngroup=1		!number of groups
	do i=1,kX-1
	   r=dabs(rootX(i)/rootX(i+1))
	   if(r.lt.rcrit) then
		ngrp(ngroup)=ngrp(ngroup)+1
		if(r.lt.rgmin(ngroup)) rgmin(ngroup)=r
	   else
		ngroup=ngroup+1
	   endif
	enddo
	grouped=ngroup.lt.kX
c
c Calculate (geometric) mean of all roots in each group and keep in gmean(n)
	n1=1
	do n=1,ngroup
	   n2=0
	   do j=1,n
	      n2=n2+ngrp(j)
	   enddo
c	   print *,n,n1,n2
	   prod=0.0d0
	   do i=n1,n2
		prod=prod+DLOG(-rootX(i))		!roots are all neg
	   enddo
	   gmean(n)=-DEXP1(prod/dfloat(n2-n1+1))
c
	   n1=n2+1		!first in next group
	enddo
c
	RETURN
	end

