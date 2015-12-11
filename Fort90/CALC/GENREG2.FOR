	subroutine GENREG2(bd,X,y,np,N,ndp,nd)
c GENREG2 is version of GENREG1 that outputs coefficients, bd() as real*8
c Modif 10/06/93 10:13am to simplify array dimension spec in XTRANX
c Uses subroutine XTRANX to calculate the product X'X without using a
c special array to hold it
c n=number of obs
c np=number of param ('p' below)
c
c Explicit calculation of least squares linear regression
c      Ycalc = X*b
c      n x 1 = (n x p)*(p x 1)
c Up to 20 parameters, 2048 observations at present
c
c  b=inv(X'X) X'y
c
c where
c	b=p x 1 vector of parameters (p is denoted np in arguments)
c	X=matrix of X values, n x p
c	X'=transpose of X, p x n;  thus X'X is p x p.
c	y=n x 1 vector of obs
c
c	real*4 X(n,np),y(n)
	real*4 X(nd,ndp),y(nd)
	real*8 bd(ndp)
	real*8 Xd,yd,Z,xdty
	allocatable Xd(:,:),yd(:),Z(:,:),xdty(:)
c
	allocate(Xd(n,np),yd(n),Z(np,np),xdty(np))
c
c Convert to double precision
	do 2 i=1,N
	 yd(i)=dble(y(i))
	 do 1 j=1,np
	  Xd(i,j)=dble(X(i,j))
1	 continue
2	continue
c
c Calc X'X in Z
	call XTRANX(Xd,z,n,np)
c Calc inv(X'X) in Z
	call MATINV(Z,np,np,Z,np)
c
c Calculate X'y  (np x n) x (n x 1) = (np x 1)  in xdty(j)
	do 4 j=1,np
	xdty(j)=0.0d0
	do 5 k=1,n
5	xdty(j)=xdty(j) + Xd(k,j)*yd(k)
4	continue
c Calc b=Z*xdty = (np x np) x (np x 1) = (np x 1)
	do 6 j=1,np
	bd(j)=0.0d0
	do 7 k=1,np
7	bd(j)=bd(j) + Z(j,k)*xdty(k)
6	continue
cc Convert back to single precision
c	do 3 i=1,np
c	 b(i)=sngl(bd(i))
c3	continue
c
	deallocate(Xd,yd,Z,xdty)
c
	RETURN
	end


