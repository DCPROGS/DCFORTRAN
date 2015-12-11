	subroutine CALCPI(Q,PI,k,kd1,kd2)
c To calc pi matrix from Q (set to -1 if denom is too small)
	real*8 Q(kd1,kd2),pi(kd1,kd2)
c
	do i=1,k
	   do j=1,k
		pi(i,j)=-1.		!in case denom=0
		if(dabs(Q(i,i)).ge.1.d-25) pi(i,j)=Q(i,j)/(-Q(i,i))
	   enddo
	enddo
	RETURN
	end
