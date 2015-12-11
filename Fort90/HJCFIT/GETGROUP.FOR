	subroutine GETGROUP(rootX,kX,kdim)
c To identify groups of roots for asymptotic HJC distribution
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 rootX(kdim)
c for common/grp/
	logical slopsch,checkgrp,grouped
	integer ngrp(100)		!number of roots in the ith group
	integer*4 n1(100),n2(100)
	common/grp1/slopsch,checkgrp,grouped,rcrit,ngpcheck
	common/grp2/ngroup,n1,n2
c
	do i=1,kX
	   ngrp(i)=1
	enddo
	ngroup=1		!number of groups
	do i=1,kX-1
	   r=dabs(rootX(i)/rootX(i+1))
	   if(r.lt.rcrit) then
		ngrp(ngroup)=ngrp(ngroup)+1
	   else
		ngroup=ngroup+1
	   endif
	enddo
	grouped=ngroup.lt.kX
c
c Calculate n(n),n2(n)=index of 1st and last root in group(n)
	n1(1)=1
	do n=1,ngroup
	   n2(n)=0
	   do j=1,n
	      n2(n)=n2(n)+ngrp(j)
	   enddo
c Calculate (geometric) mean of all roots in each group and keep in gmean(n)
c	   prod=0.0d0
c	   do i=n1,n2
c		prod=prod+DLOG(-rootX(i))		!roots are all neg
c	   enddo
c	   gmean(n)=-DEXP1(prod/dfloat(n2-n1+1))
c
	   if(n.lt.ngroup) then
		n1(n+1)=n2(n)+1		!first in next group
	   endif
 	enddo
c
	RETURN
	end

