	subroutine EQOCC2(Q,peq,k,km)
c To solve peq*Q=0 by Hawkes method
c See TEQ.FOR for tests.
	real*8 Q(10,10),X(10,11),XT(11,10),Z(10,10),peq(10)
	real*8 uT(1,10),one,s
c
	k1=k+1
	one=1.0d0
c
	do 31 j=1,k
31	uT(1,j)=one		!define
c
	do 1 i=1,k
	do 1 j=1,k
1	X(i,j)=Q(i,j)
c
	do 2 i=1,k
2     X(i,k1)=one 	!add column of ones
c
	do 3 i=1,k
	do 3 j=1,k1
3	XT(j,i)=X(i,j)
c
      call MATMUL(X,XT,Z,k,k1,k,one,
     & km,km+1,km+1,km,km,km)
      call MATINV(Z,k,km,Z,km)		!inv(Z) in Z
      call MATMUL(uT,Z,peq,1,k,k,one,1,km,km,km,1,km)
c Check sum to 1
	s=0.0d0
	do 4 i=1,k
4	s=s+peq(i)
	do 5 i=1,k
5	peq(i)=peq(i)/s
	RETURN
	end

