	subroutine MATPOW(AMAT,EIGEN,ipow,Qout,k,ka,ke,kq)
c Lahey version (args changed) 09/28/89 08:08pm
C SUBROUTINE TO CALCULATE IPOW'TH POWER OF A MATRIX, THE SPECTRAL
C EXPANSION OF WHICH IS SUPPLIED IN AMAT,EIGEN. RESULT RETURNED
C IN Qout.
C	k=size of matrix to be raised to power
C	ka,ke,kq = declared dimension of AMAT,EIGEN and Qout in calling prog
C
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 AMAT(ka,ka,ka),EIGEN(ke),Qout(kq,kq)
C
C SET Qout TO UNIT MATRIX IF IR=0
	IF(IPOW.GT.0) GOTO 337
	DO 335 I=1,K
	DO 335 J=1,K
	Qout(I,J)=0.0D0
335	IF(I.EQ.J) Qout(I,J)=1.0D0
	RETURN
C
337	CONTINUE
	pow=float(ipow)
	DO 338 i=1,k
	DO 338 j=1,k
	Qout(i,j)=0.0D0
	DO 338 m=1,k
	IF(EIGEN(m).GE.0.0D0) GOTO 1
	IF(EIGEN(m).LT.-1.0D-10) print 2,M,EIGEN(m)
C IF LESS THAN -1.E-10 MAY NOT BE JUST ROUNDING ERROR!
2	FORMAT( ' ***EIGEN(',I2,')= ',G13.6,' RESET TO ZERO IN MATPOW',/)
	EIGEN(m)=0.0D0
1	Qout(i,j)=Qout(i,j)+AMAT(i,j,m)*(EIGEN(m)**pow)
338	CONTINUE
C
	RETURN
	END



