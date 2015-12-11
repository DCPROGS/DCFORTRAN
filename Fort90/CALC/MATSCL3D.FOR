	subroutine MATSCL3D(phi,XAF,m,end,kA,kF,SCAL,
     &	kp,kq1,kq2,kq3,ke)
C
c MATSCL3D is version of MATSCL3 in which 2nd parameter is not nec square
c (e.g. can use XAF(i,j,m) which is kAxkF, not only AMAT(i,j,m) which
c is assumed to be kAxkA)
c MATSCL3 is version of MATSCL in which matrix is 3D, Amat(i,j,m)
c and m is specified as a parameter. Top left ka*ka elements used
c
C CONVERTS MATRIX TO A SCALAR BY PREMULTIPLYING
C BY VECTOR PHI ( 1 X k) AND POSTMULTIPLYING BY VECTOR
C END (k X 1). RESULT IN SCAL.
C	KP= DECLARED DIM OF PHI(1,KP)
C	KQ= DECLARED DIM OF Q(KQ,KQ)
C	KE= DECLARED DIM OF END(KE,1)
C
	REAL*8 PHI(1,kp),XAF(kq1,kq2,kq3),END(ke,1),SCAL,ZERO
	REAL*8 T
C
	zero=0.0d0
	scal=zero
	do i=1,kA
	   t=zero
	   do j=1,kF
		t=t+XAF(i,j,m)*end(j,1)	!accum ith element of XAF(m)*end
	   enddo
	   scal=scal+phi(1,i)*t
	enddo
	RETURN
	END


