	subroutine GENREG3(b,X,y,np,N)
c
c GENREG3 is version of GENREG1 that assumes b,X,y have been allocated to
c exactly the required size before calling.
c GENREG1 is version of GENREG that uses subroutine XTRANX to calculate
c the product X'X without using a special array to hold it
c(Written because of memory problem in GENREG with 2 Real*8
c arrays of 2048*20=655360 bytes!)
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
	real*4 b(np),X(N,np),y(N)	!parameters
c	real*4 b(ndp),X(ndx1,ndx2),y(ndy)
c	real*8 bd(20),Xd(2048,20),Z(20,20),yd(2048),xdty(20)
	ALLOCATABLE bd,Xd,Z,yd,xdty
	real*8 bd(:),Xd(:,:),Z(:,:),yd(:),xdty(:)
c	real*8 one
c
	ALLOCATE(bd(np),Xd(N,np),Z(np,np),yd(N),xdty(np))
c	kn=2048
c	kp=20
c	one=1.0d0
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
c	call XTRANX(Xd,z,n,np,nd1,nd2,ndz)
	call XTRANX(Xd,z,n,np)
c Calc inv(X'X) in Z
c	call MATINV(Z,np,kp,Z,kp)
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
c Convert back to single precision
	do 3 i=1,np
	 b(i)=sngl(bd(i))
3	continue
c
	DEALLOCATE(bd,Xd,Z,yd,xdty)
c
	RETURN
	end


