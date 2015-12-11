	subroutine PTCALCS(pt,t,tau,amp,pinfs,p0s,p0,pinf,eigen,amat,k,km)
c Version of PTCALC that returns
c OUTPUT: single precision values of pt(), tau(), and amp(), and p0s, pinfs()
c		but still double precision output of result=pt()
c INPUT: (double prec) of p0(),pinf(),eigen(),amat().
c
c NB t in seconds
c
c To calc occupancies p(t) at time t, in pt(j), given initial occs in p0,
c final occs in pinf, and the spectral expansion of Q for the post-jump
c conditions in eigen,amat. Returns also the coefficients b(m,j) of
c the mth exponential component for p(j), so once this is calc
c can calc new p(t), for range of t, quickly as
c pj(t)=pj(inf) + sum[amp(m,j)*exp(t/tau(m))]; sum from m=1,...,k-1
c (NB QMAT5 gives eigen(k) as the zero eigenvalue, so sum from m=1,...,k-1
c rather than m=2,...,k)
	real*8 p0(km),pinf(km),eigen(km),amat(km,km,km),pt(km)
	real*8 bm,tt
	real*4 t,tau(km),amp(km,km),pinfs(km),p0s(km)
c
	do m=1,k-1
	   if(dabs(eigen(m)).gt.1.d-37) then
	       tau(m)=-1.e3/sngl(eigen(m))
	   else
		tau(m)=1.e20
	   endif
	enddo
c
	tt=dble(t)		!in seconds
	do j=1,k
	   pinfs(j)=sngl(pinf(j))	!for output
	   p0s(j)=sngl(p0(j))		!for output
	   pt(j)=pinfs(j)
	   do m=1,k-1
		bm=0.0d0
	 	do i=1,k
	 	   bm=bm + p0(i)*Amat(i,j,m)
		enddo
		amp(m,j)=sngl(bm)
		pt(j)=pt(j) + bm*dexp(tt*eigen(m))
	   enddo     !m=1,...,k-1
	enddo       !j=1,...,k
c
	RETURN
	end

