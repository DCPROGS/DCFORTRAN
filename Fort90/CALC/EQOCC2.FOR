	subroutine EQOCC2(Q,peq,k,km)
c To solve peq*Q=0 by Hawkes method
c See TEQ.FOR for tests.
	real*8 Q(km,km),peq(km)
	real*8 X,XT,Z,uT,peq1
	allocatable:: X(:,:),XT(:,:),Z(:,:),uT(:,:),peq1(:,:)
	real*8 one,s
c
	allocate(X(k,k+1),XT(k+1,k),Z(k,k),uT(1,k),peq1(1,k))
	k1=k+1
	one=1.0d0
c
	do j=1,k
	   uT(1,j)=one		!define
	enddo
c
	do i=1,k
	   do j=1,k
		X(i,j)=Q(i,j)
	   enddo
	enddo
c
	do i=1,k
         X(i,k1)=one 	!add column of ones
	enddo
c
	do i=1,k
	   do j=1,k1
		XT(j,i)=X(i,j)
	   enddo
	enddo
c
      call MATMUL(X,XT,Z,k,k1,k,one,
     & k,k+1,k+1,k,k,k)
      call MATINV(Z,k,k,Z,k)		!inv(Z) in Z
      call MATMUL(uT,Z,peq1,1,k,k,-one,1,k,k,k,1,k)
c Check sum to 1
	s=0.0d0
	do i=1,k
	   peq(i)=peq1(1,i)
	   s=s+peq(i)
	enddo

	do i=1,k
	   peq(i)=peq(i)/s
	enddo
	deallocate(X,XT,Z,uT,peq1)
	RETURN
	end

