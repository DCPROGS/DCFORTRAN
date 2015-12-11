	subroutine RAC2(ROW,COL,Amat,K,W,W1,NR,NC,NA)
C RAC2 is same as RAC, but uses AMAT rather than EM,EN
C SUBROUTINE TO CALC COEFFICIENTS W1(M) (REAL*8) AS
C ROW*A(M)*COL WHERE ROW IS 1 X K, A(M) IS K X K AND COL
C IS K X 1. THE A(M) ARE SUPPLIED AS EM,EN.
C ALSO OUTPUTS REAL*4 VALUES IN W(M)
C   DECLARED DIMENSIONS IN CALLING PROG:
C	NR FOR ROW(1,NR)
C	NC FOR COL(NC,1)
C	NA FOR EM(NA,NA),EN(NA,NA)
C
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	DIMENSION ROW(1,NR),COL(NC,1),amat(na,na,na),W1(100)
	REAL*4 W(100)
c
	do m=1,k
	   w1(m)=0.0d0
	   do L=1,k
		do n=1,k
		   w1(m)=w1(m)+row(1,L)*amat(L,n,m)*col(n,1)
		enddo
	   enddo
	   w(m)=SNGL(w1(m))
	enddo
c
	RETURN
	END

