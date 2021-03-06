	subroutine GMAT(AB,QM,GAB,kdim,KR1,KR2)
c	subroutine GMAT(AB,QM,GAB,kdim,KAM,KAMAX,KR1,KR2)
C SUBROUTINE TO CALCULATE PROB TRANSITION MATRICES ,GAB ETC, FROM Q
C	GAB=-INV(QAA)*QAB (UP TO 8 X 8 IN SIZE)
C	AB=INTEGER CODE FOR REQ MATRIX
C	QM=DOUBLE PREC Q
C	kdim=DIM OF QM IN MAIN PROG (SQUARE)
C	KR1,KR2=DIM OF GAB  IN MAIN PROG (SQUARE)
c Modif 07/21/99 07:04am to allow any array size
C
	allocatable::temp
	real*8 temp(:,:)
	real*8 QM(kdim,kdim),GAB(KR1,KR2),one
	INTEGER AB
	COMMON/KBLK/KA,KB,KC,KD
C
	allocate(temp(kdim,kdim))
	one=1.0D0
C FIRST SEPARATE THE DIGITS OF 'AB' AS IN SUBMAT
	I=AB/10	!LEFT HAND DIGIT
	J=MOD(AB,10)	!RIGHT HAND DIGIT
C RESULT WILL BE OF SIZE (K1 X K2) WHERE
	IF(I.EQ.1) K1=KA
	IF(I.EQ.2) K1=KB
	IF(I.EQ.3) K1=KC
	IF(I.EQ.4) K1=KD
	IF(I.EQ.5) K1=KB+KC	!=F
	IF(I.EQ.6) K1=KA+KB	!=E
	IF(I.EQ.7) K1=KB+KC+KD	!=T
	if(i.eq.8) k1=kC+kD
C
	IF(J.EQ.1) K2=KA
	IF(J.EQ.2) K2=KB
	IF(J.EQ.3) K2=KC
	IF(J.EQ.4) K2=KD
	IF(J.EQ.5) K2=KB+KC
	IF(J.EQ.6) K2=KA+KB
	IF(J.EQ.7) K2=KB+KC+KD
	if(j.eq.8) k2=kC+kD
C
C GET SUBSCRIPT FOR QAA ETC
	II=I*10+I
C GET QII IN GAB
	call SUBMAT(QM,II,GAB,kdim,kdim,KR1,KR2)
C INVERT IT GIVING INV(QII) IN GAB.  Q11 IS SIZE (K1 X K1)
	call MATINV(GAB,K1,KR1,GAB,KR1)
C GET QAB IN TEMP. ITS SIZE IS (K1 X K2)
	call SUBMAT(QM,AB,TEMP,kdim,kdim,KDIM,KDIM)
C GET -QII*TEMP=GAB IN GAB
	call MATMUL(GAB,TEMP,GAB,K1,K1,K2,-ONE,KR1,KR2,KDIM,KDIM,KR1,KR2)
C
	DEALLOCATE(temp)
	RETURN
	END

