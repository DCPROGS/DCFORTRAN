	subroutine HILLVAR(Ymax1,aK1,hill1,sdK,sdY,sdnH,corYK,corYn,corKn)
c To calculate approx SD from analytical Hessian matrix in case of
c fit of Hill equation (K,Ymax,nHill), i.e OBSERVED info matrix.
c Methods are as worked out in HILLFIT.MCD
c
	real*8 finfo(3,3),cov(3,3),den,two
	real*8 zero,one,z,s,a,a2,b,c
	real*8 d2Y,d2K,d2n,d2YK,d2Yn,d2Kn
	real*8 Ymax,aK,hill,x2,y2,w2
c	real*8 z2
	real*4 cor(3,3)
	common/data1/ n,x(100),y(100),w(100)	!for hyperr/func1/2
c
	zero=0.0d0
	one=1.0d0
	two=2.0d0
	Ymax=dble(Ymax1)
	aK=dble(aK1)
	hill=dble(hill1)
c First Hessian
c d2(SSD)/dYm^2
	s=zero
	do i=1,n
	   x2=dble(x(i))
	   w2=dble(w(i))
	   z=(aK/x2)**hill
	   s=s + w2*(one/(one+z))**2
	enddo
	d2Y=two*s
c
c d2(SSD)/dK^2
	s=zero
	do i=1,n
	   x2=dble(x(i))
	   y2=dble(y(i))
	   w2=dble(w(i))
	   z=(aK/x2)**hill
c    	   a=hill/(aK*aK)
c	   b=(one-hill)+z*(one+hill)/(one+z)**3
c	   c=(one-hill)+z*(one+two*hill)/(one+z)**4
c	   s=s + w2*a*z*(y2*b - Ymax*c)
	   b=(one-hill)+z*(one+hill)
	   c=((one-hill)+z*(one+two*hill))/(one+z)
	   s=s + w2*z*(y2*b - Ymax*c)/(one+z)**3
	enddo
	d2K=-two*s*Ymax*hill/(aK*aK)
c
c d2(SSD)/dnH^2
	s=0.
	do i=1,n
	   x2=dble(x(i))
	   y2=dble(y(i))
	   w2=dble(w(i))
	   z=(aK/x2)**hill
c	   z2=z*z
c	   b=(z-z2)/(one+z)**3
c	   c=(z-two*z2)/(one+z)**4
	   b=(one-z)
	   c=(one-two*z)/(one+z)
	   a=dlog(aK/x2)
	   a2=a*a
	   s=s + w2*a2*z*(y2*b - Ymax*c)/(one+z)**3
	enddo
	d2n=two*Ymax*s
c
c Cross derivative d2(SSD)/dYm dK
	s=0.
	do i=1,n
	   x2=dble(x(i))
	   y2=dble(y(i))
	   w2=dble(w(i))
	   z=(aK/x2)**hill
	   a=hill/aK
	   b=z
	   c=z/(one+z)
	   s=s + w2*a*(y2*b - two*Ymax*c)/(one+z)**2
	enddo
	d2YK=two*s
c
c Cross derivative d2(SSD)/dYm dnH
	s=0.
	do i=1,n
	   x2=dble(x(i))
	   y2=dble(y(i))
	   w2=dble(w(i))
	   z=(aK/x2)**hill
	   a=dlog(aK/x2)
	   b=one
	   c=one/(one+z)
	   s=s + w2*a*z*(y2*b - two*Ymax*c)/(one+z)**2
	enddo
	d2Yn=two*s
c
c Cross derivative d2(SSD)/dK dnH
	s=0.
	do i=1,n
	   x2=dble(x(i))
	   y2=dble(y(i))
	   w2=dble(w(i))
	   z=(aK/x2)**hill
	   a=dlog(aK/x2)
	   b=((one+hill*a)+z*(one-hill*a))
	   c=((one+hill*a)+z*(one-two*hill*a))/(one+z)
	   s=s + w2*z*(y2*b - Ymax*c)/(one+z)**3
	enddo
	d2Kn=two*Ymax*s/aK
c
c Info matrix
	finfo(1,1)=d2Y/two
	finfo(2,2)=d2K/two
	finfo(3,3)=d2n/two
	finfo(1,2)=d2YK/two
	finfo(2,1)=finfo(1,2)
	finfo(1,3)=d2Yn/two
	finfo(3,1)=finfo(1,3)
	finfo(2,3)=d2Kn/two
	finfo(3,2)=finfo(2,3)
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


