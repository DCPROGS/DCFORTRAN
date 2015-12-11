	subroutine EQOCC1(Q,peq,k,km)
c To calc equilibrium occupancies from Q by reduced matrix method (should
c be the fastest). See TEQ.FOR for tests.
c	real*8 Q(10,10),Q1(10,10),r(10),peq(10),s,one
	real*8 Q(km,km),peq(km)			!param
	allocatable::Q1,r
	real*8 Q1(:,:),r(:),s,one		!local
c
	one=1.0d0
	allocate(Q1(k,k),r(k))
c
	do i=1,k-1
	   do j=1,k-1
		Q1(i,j)=Q(i,j)-q(k,j)
	   enddo
	enddo
      call MATINV(Q1,k-1,k,Q1,k)		!inv(Q1) in Q1
c
	do j=1,k-1
	   r(j)=q(k,j)
	enddo
      call MATMUL(r,Q1,peq,1,k-1,k-1,-one,1,k,k,k,1,km)
c Find last occupancy by difference
	s=0.0d0
	do j=1,k-1
	   s=s+peq(j)
	enddo
c
	peq(k)=one-s
c
	deallocate(Q1,r)
	RETURN
	end

