	subroutine PTCALC(pt,t,bmj,p0,pinf,eigen,amat,k,km)
c To calc occupancies p(t) at time t, in pt(j), given initial occs in p0,
c final occs in pinf, and the spectral expansion of Q for the post-jump
c conditions in eigen,amat. Returns also the coefficients b(m,j) of
c the mth exponential component for p(j), so once this is calc
c can calc new p(t), for range of t, quickly as
c pj(t)=pj(inf) + sum[b(m,j)*dexp(eigen(m)*t)]; sum from m=2,...,k
c NB QMAT5 gives eigen(k) as the zero eigenvalue, so sum from m=1,...,k-1
c rather than m=2,...,k
c Use dexp1() to prevent floating underflow
	real*8 pt(km),p0(km),pinf(km),eigen(km),amat(km,km,km),bmj(km,km)
	real*8 tt,bm
	real*4 t
c
	tt=dble(t)
	do 24 j=1,k
	  pt(j)=pinf(j)
	  do 25 m=1,k-1
	   bm=0.0d0
	   do 26 i=1,k
26	   bm=bm + p0(i)*Amat(i,j,m)
	   bmj(m,j)=bm
	   pt(j)=pt(j) + bm*dexp1(eigen(m)*tt)
25	  continue
24	continue
	RETURN
	end

