	program tgrp
c To test code for searching for closely spaced groups of roots in ROOTSCH
	real root(10)
	integer ngrp(100)
c Test sign (separate prob!)
	real*8 d
	d=1.1d200
	print*,dsign(1.d0,d)
	d=0.0d0
	print*,dsign(1.d0,d)
	d=-1.1d200
	print*,dsign(1.d0,d)
c
	nroot=10
	rcrit=2.0
c
	root(1)=-100.
	root(2)=-20.1
	root(3)=-20.0
	root(4)=-6.3
	root(5)=-6.2
	root(6)=-6.1
	root(7)=-6.0
	root(8)=-1.1
	root(9)=-1.0
	root(10)=-0.1
c
	do i=1,nroot
	   ngrp(i)=1
	enddo
c
	ng=1		!number of groups
	do i=1,nroot-1
	   r=abs(root(i)/root(i+1))
	   if(r.lt.rcrit) then
		ngrp(ng)=ngrp(ng)+1
	   else
		ng=ng+1
	   endif
	enddo
c First and last in each group
	n1=1
	do i=1,ng
	   n2=0
	   do j=1,i
		n2=n2+ngrp(j)
	   enddo
	   print *,i,n1,n2
c Would be best to find highest and lowest root in each group  first, so that
c a detailed search could be made between these limits for the rest of the roots
c Look for point halfway betwen last root in 1 group and first in the next
c (halfway on log scale?)
c
	   n1=n2+1		!first in next group
	enddo
c

	continue
	end


