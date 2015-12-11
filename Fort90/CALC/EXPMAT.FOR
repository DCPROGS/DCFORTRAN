	subroutine EXPMAT(EXPQ,amat,eigen,t,k,km)
c To calculate EXPQ=exp(Q*t)
c EXPQ(i,j)=SUM[amat(i,j,m)*exp(eigen(m)*t)]
c NB sums amat(i,j,m) from m=1,k (not m=k-1)
c Input:
c real*8 t,amat,eigen (last 2 are expansion of Q from prev call to QMAT(5))
	real*8 t,EXPQ(km,km),amat(km,km,km),eigen(km)
	real*8 d,exprt(10)	!internal -for speed
c
	do m=1,k
	   exprt(m)=dexp(eigen(m)*t)
	enddo
c
	do i=1,k
	   do j=1,k
		d=0.0d0
		do m=1,k
		   d=d + amat(i,j,m)*exprt(m)
		enddo
		EXPQ(i,j)=d
	   enddo
	enddo
c
	RETURN
	end

