	subroutine PTCAL1(pt,t,p0,pinf,eigen,amat,k,km)
c Version of PTCALC which does not return bmj(m,j)
c Use dexp1() to prevent floating underflow
c
c To calc occupancies p(t) at time t, in pt(j), given initial occs in p0,
c final occs in pinf, and the spectral expansion of Q for the post-jump
c conditions in eigen,amat. Returns also the coefficients b(m,j) of
c the mth exponential component for p(j), so once this is calc
c can calc new p(t), for range of t, quickly as
c pj(t)=pj(inf) + sum[b(m,j)*dexp(eigen(m)*t)]; sum from m=2,...,k
c NB QMAT5 gives eigen(k) as the zero eigenvalue, so sum from m=1,...,k-1
c rather than m=2,...,k
	real*8 pt(km),p0(km),pinf(km),eigen(km),amat(km,km,km)
	real*8 tt,bm
	real*4 t
c
	tt=dble(t)
	do j=1,k
	   pt(j)=pinf(j)
	   do m=1,k-1
		bm=0.0d0
		do i=1,k
		   bm=bm + p0(i)*Amat(i,j,m)
		enddo
c		bmj(m,j)=bm
		pt(j)=pt(j) + bm*dexp1(eigen(m)*tt)
	   enddo
	enddo
	RETURN
	end

