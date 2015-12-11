	subroutine GMAT2(AB,QM,GAB,km,kr,RECIP)
c GMAT2 is like GMAT but returns the inverse matrix in RECIP
c to avoid recalc later (superfluous array dimensions omitted from
c call: RECIP must be km x km = 10x10 at present, like QM)
c
c Modif 07/21/99 07:04am to allow any array size
c
C SUBROUTINE TO CALCULATE PROB TRANSITION MATRICES ,GAB ETC, FROM Q
C	GAB=-INV(QAA)*QAB (UP TO 8 X 8 IN SIZE)
C	AB=INTEGER CODE FOR REQ MATRIX
C	QM=DOUBLE PREC Q
C	km=DIM OF QM IN MAIN PROG (SQUARE)
C	KR=DIM OF GAB  IN MAIN PROG (SQUARE)
C
	allocatable::temp
	real*8 temp(:,:)
	real*8 QM(km,km),GAB(kr,kr),RECIP(km,km)
	real*8 one
	INTEGER AB
	COMMON/KBLK/KA,KB,KC,KD
C
	kdim=km
	one=1.0d0
	allocate(temp(kdim,kdim))
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
	call SUBMAT(QM,II,RECIP,km,km,kr,kr)
C INVERT IT GIVING INV(QII) IN RECIP.  Q11 IS SIZE (K1 X K1)
	call MATINV(RECIP,K1,km,RECIP,km)
C GET QAB IN TEMP. ITS SIZE IS (K1 X K2)
	call SUBMAT(QM,AB,TEMP,km,km,KDIM,KDIM)
C GET -QII*TEMP=GAB IN GAB
	call MATMUL(RECIP,TEMP,GAB,K1,K1,K2,-ONE,km,km,KDIM,KDIM,kr,kr)
c
	DEALLOCATE(temp)
	RETURN
	END

