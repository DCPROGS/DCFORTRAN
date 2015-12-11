	subroutine HILLVARe(Ym,aK1,hill1,sdK,sdY,sdnH,corYK,corYn,corKn)
c SD for fit of Hill eq using EXPECTED info matrix
c Methods as in hillfit.mcd and hyperr.mcd
c
	real*8 finfo(3,3),cov(3,3),den
	real*8 zero,one,z,z1,z2,s,a
	real*8 Ymax,aK,hill,x2,w2
c	real*8 z2
	real*4 cor(3,3)
	common/data1/ n,x(100),y(100),w(100)	!for hyperr/func1/2
c
	zero=0.0d0
	one=1.0d0
	Ymax=dble(Ym)
	aK=dble(aK1)
	hill=dble(hill1)
c
c (dY/dYmax)^2
	s=zero
	do i=1,n
	   x2=dble(x(i))
	   w2=dble(w(i))
	   z=(aK/x2)**hill
	   s=s + w2*(one/(one+z))**2
	enddo
	finfo(1,1)=s
c
c (dY/dYmax)(dY/dK)
	s=zero
	do i=1,n
	   x2=dble(x(i))
	   w2=dble(w(i))
	   z1=(x2/aK)**hill
	   s=s + w2*(z1*z1/(one+z1)**3)
	enddo
	finfo(1,2)=-s*hill*Ymax/aK
	finfo(2,1)=finfo(1,2)
c
c (dY/dYmax)(dY/dnH)
	s=zero
	do i=1,n
	   x2=dble(x(i))
	   w2=dble(w(i))
	   a=dlog(x2/aK)
	   z1=(x2/aK)**hill
	   s=s + w2*a*(z1*z1/(one+z1)**3)
	enddo
c
	finfo(1,3)=s*Ymax
	finfo(3,1)=finfo(1,3)
c
c (dY/dK)(dY/dnH)
	s=zero
	do i=1,n
	   x2=dble(x(i))
	   w2=dble(w(i))
	   a=dlog(x2/aK)
	   z1=(x2/aK)**hill
	   s=s + w2*a*(z1*z1/(one+z1)**4)
	enddo
c
	finfo(2,3)=-s*hill*Ymax*Ymax/aK
	finfo(3,2)=finfo(2,3)
c
c (dY/dK)^2
	s=zero
	do i=1,n
	   x2=dble(x(i))
	   w2=dble(w(i))
	   z1=(x2/aK)**hill
	   s=s + w2*(z1*z1/(one+z1)**4)
	enddo
c
	finfo(2,2)=s*((hill*Ymax/aK)**2)
c
c (dY/dnH)^2
	s=zero
	do i=1,n
	   x2=dble(x(i))
	   w2=dble(w(i))
	   a=dlog(x2/aK)
	   z1=(x2/aK)**hill
	   z2=a*z1/(one+z1)**2
	   s=s + w2*z2*z2
	enddo
	finfo(3,3)=s*Ymax*Ymax
c finfo now defined
c
c Covar matrix
	call MATINV(finfo,3,3,cov,3)
c Errors (SD)
	if(cov(1,1).gt.0.) then
	   sdY=sqrt(sngl(cov(1,1)))
	else
	   sdY=-1.
	endif
	if(cov(2,2).gt.0.) then
	   sdK=sqrt(sngl(cov(2,2)))
	else
	   sdK=-1.
	endif
	if(cov(3,3).gt.0.) then
	   sdnH=sqrt(sngl(cov(3,3)))
	else
	   sdnH=-1.
	endif
c
c Correlation matrix
	do i=1,2		!i=1,j=2,3;  i=2, j=3
	   do j=i+1,3
		den=cov(i,i)*cov(j,j)
		if(den.gt.1.d-20) then
		   cor(i,j)=sngl(cov(i,j)/dsqrt(den))
		else
		   cor(i,j)=-2.
		endif
	   enddo
	enddo
c define corYK,corYn,corKnc
	corYK=cor(1,2)
	corYn=cor(1,3)
	corKn=cor(2,3)
c
	RETURN
	end






