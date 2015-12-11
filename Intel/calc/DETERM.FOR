c      SUBROUTINE DETV(A,N,D,L,M,ndexp)
      subroutine DETERM(A,N,D,L,M,ndexp,nerr)
c	dimension A(100)
c	DIMENSION L(10),M(10)
	dimension A(n*n)
	DIMENSION L(n),M(n)
C
      DOUBLE PRECISION A,D,BIGA,HOLD,DABS,d1
C
c Modified version of DETV (see eqoccd). Now returns nerr=10 if
c overflow of determinant is imminent
c
C Modification of MINV to calculate only the determinant
c ***Modified March 89 to prevent floating overflow in calc of
c determinant (as in MINV2). Extra param added NDEXP such that value
c of D which is output must be mult by 10**ndexp to get correct
c determinant
C
C           THE STANDARD GAUSS-JORDAN METHOD IS USED.
C
cd	do 501 i=1,25
cd501	type *,i,a(i)
c initialise
	ndexp=0
      D=1.0d0
      NK=-N
c
      DO 80 K=1,N
C   SEARCH FOR LARGEST ELEMENT
      NK=NK+N
      L(K)=K
      M(K)=K
      KK=NK+K
      BIGA=A(KK)
cd	print 500,kk,a(kk)
cd500	format( ' A(',i2,')= ',g13.6)
      DO 20 J=K,N
      IZ=N*(J-1)
      DO 20 I=K,N
      IJ=IZ+I
cd	print 500,ij,a(ij)
   10 IF(DABS(BIGA)-DABS(A(IJ))) 15,20,20
   15 BIGA=A(IJ)
      L(K)=I
      M(K)=J
   20 CONTINUE
C
C        INTERCHANGE ROWS
C
      J=L(K)
      IF(J-K) 35,35,25
   25 KI=K-N
      DO 30 I=1,N
      KI=KI+N
cd	print 500,ki,a(ki)
      HOLD=-A(KI)
      JI=KI-K+J
cd	print 500,ji,a(ji)
      A(KI)=A(JI)
   30 A(JI) =HOLD
C
C        INTERCHANGE COLUMNS
C
   35 I=M(K)
      IF(I-K) 45,45,38
   38 JP=N*(I-1)
      DO 40 J=1,N
      JK=NK+J
      JI=JP+J
      HOLD=-A(JK)
      A(JK)=A(JI)
   40 A(JI) =HOLD
C
C        DIVIDE COLUMN BY MINUS PIVOT (VALUE OF PIVOT ELEMENT IS
C        CONTAINED IN BIGA)
C
   45 IF(BIGA) 48,46,48
   46 D=0.0d0
      RETURN
   48 DO 55 I=1,N
      IF(I-K) 50,55,50
   50 IK=NK+I
cd	print 500,ik,a(ik)
      A(IK)=A(IK)/(-BIGA)
   55 CONTINUE
C
C        REDUCE MATRIX
C
      DO 65 I=1,N
      IK=NK+I
      HOLD=A(IK)
      IJ=I-N
      DO 65 J=1,N
      IJ=IJ+N
      IF(I-K) 60,65,60
   60 IF(J-K) 62,65,62
   62 KJ=IJ-I+K
cd	print 500,kj,a(kj)
      A(IJ)=HOLD*A(KJ)+A(IJ)
   65 CONTINUE
C
C        DIVIDE ROW BY PIVOT
C
      KJ=K-N
      DO 75 J=1,N
      KJ=KJ+N
      IF(J-K) 70,75,70
   70 A(KJ)=A(KJ)/BIGA
   75 CONTINUE
C
C        PRODUCT OF PIVOTS
C
c test for overflow
	nerr=0
	d1=dlog10(dabs(D))+dlog10(dabs(biga))
	if(d1.gt.238.d0) then
	   nd=int4(d1)
	   if(nd.gt.200) then
		nerr=10
		RETURN
	   endif
	   D=D*(10.d0**(-nd))	!scale down
	   ndexp=ndexp+nd		!keep count
	endif
c test for underflow
	if(d1.lt.-238.d0) then
	   D=0.d0
	else
	   D=D*BIGA
	endif
C
C        REPLACE PIVOT BY RECIPROCAL
C
      A(KK)=1.0/BIGA
   80 CONTINUE
C
	RETURN
      END

