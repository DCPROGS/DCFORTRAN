	subroutine MATSCL2d(phi,XAF,col,kA,kF,scal,kp,kq1,kq2,kc)
c MATSCL2d is like matscl2 except that XAF need not be declared as a square
c matrix: dimensions specified as kq1,kq2
c
C Matscl2 is variant of MATSCL for non-square matrices
C CONVERTS MATRIX XAF (kA X kF) TO A SCALAR BY PREMULTIPLYING
C BY VECTOR PHI ( 1 X kA) AND POSTMULTIPLYING BY VECTOR col (kF X 1).
c RESULT IN SCAL.
C	KP= DECLARED DIM OF PHI(1,KP)
C	kq1,kq2= DECLARED DIM OF XAF(KQ1,KQ2)
C	KE= DECLARED DIM OF col(KE,1)
C
	REAL*8 phi(1,kp),XAF(kq1,kq2),col(kc,1),scal,zero
	REAL*8 T
C
	zero=0.0d0
	scal=zero
	do i=1,kA
	   T=zero
	   do j=1,kF
		T=T+XAF(i,j)*col(j,1)
	   enddo
	   scal=scal+phi(1,i)*T
	enddo
	RETURN
	END


