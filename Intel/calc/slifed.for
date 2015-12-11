	SUBROUTINE SLIFED(Q,PEQ,KQ,KP)
c Version of SLIFED with dimension Q
C TYPE MEAN LIFE IN EACH SUBSET (COULD ALSO HAVE S AND PVAL AS
C ARRAYS IN PARAM LIST). DOUBLE PRECISION VERSION
	real*8 Q(kq,kq)
	real*8 PEQ(kp),S,PVAL
	logical pon
	INTEGER KVAL(4)
	logical discprt
	common/dp/discprt
	COMMON/KBLK/KA,KB,KC,KD
C
	pon=.false.
c
	KVAL(1)=KA
	KVAL(2)=KA+KB
	KVAL(3)=KA+KB+KC
	KVAL(4)=KA+KB+KC+KD
	K=KVAL(4)
C
	K2=0
c
	DO 100 M=1,4	!EACH SUBSET
	S=0.0D0
	K1=K2+1
	K2=KVAL(M)
	IF(K2.LT.K1) GOTO 100	!SKIP EMPTY SUBSETS
	PVAL=0.0D0
	DO 11 I=K1,K2
11	PVAL=PVAL+PEQ(I)	!TOTAL OCC FOR CURRENT SUBSET
	if(pval.lt.1.d-15) goto 100
	DO 10 I=K1,K2		!ITH ROW IN CURRENT SUBSET
	DO 10 J=1,K		!BUT SKIP ELEMENTS WITHIN CURRENT SUBSET
	IF(J.LT.K1.OR.J.GT.K2) S=S+(PEQ(I)/PVAL)*Q(I,J)	!TOTAL RATE OUT
c	print 200,M,I,J,PVAL,PEQ(I),Q(I,J),S
c200	FORMAT(3I4,4G13.6)
10	CONTINUE
	if(s.lt.1.0d-29) goto 100
	IF(M.EQ.1) print 12,1.E3/S
	IF(pon.and.M.EQ.1) write(7,12)1.E3/S
      if(discprt.and.m.eq.1) write(7,12)1.E3/S
12	FORMAT( ' Mean life in A (ms) ',6X,G13.6)
	IF(M.EQ.2) print 13,1.E3/S
	IF(pon.and.M.EQ.2) write(7,13)1.E3/S
      if(discprt.and.m.eq.2) write(7,13)1.E3/S
13	FORMAT( ' Mean life in B (ms) ',6X,G13.6)
	IF(M.EQ.3) print 14,1.E3/S
	IF(pon.and.M.EQ.3) write(7,14)1.E3/S
      if(discprt.and.m.eq.3) write(7,14)1.E3/S
14	FORMAT( ' Mean life in C (ms) ',6X,G13.6)
	IF(M.EQ.4) print 15,1.E3/S
	IF(pon.and.M.EQ.4) write(7,15)1.E3/S
      if(discprt.and.m.eq.4) write(7,15)1.E3/S
15	FORMAT( ' Mean life in D (ms) ',6X,G13.6)
100	CONTINUE
C
c Add mean life in F=B u C=all shut states (Feb 1989)
	k1=ka+1
	k2=k
	S=0.0D0
	PVAL=0.0D0
	DO 21 I=K1,K2
21	PVAL=PVAL+PEQ(I)	!TOTAL OCC FOR CURRENT SUBSET
	if(pval.lt.1.d-15) goto 200
	DO 22 I=K1,K2		!ITH ROW IN CURRENT SUBSET
	DO 22 J=1,K		!BUT SKIP ELEMENTS WITHIN CURRENT SUBSET
	IF(J.LT.K1.OR.J.GT.K2) S=S+(PEQ(I)/PVAL)*Q(I,J)	!TOTAL RATE OUT
c	print 200,M,I,J,PVAL,PEQ(I),Q(I,J),S
22	CONTINUE
	if(s.lt.1.0d-29) goto 200
	print 23,1.E3/S
	IF(pon) write(7,23)1.E3/S
      if(discprt) write(7,23)1.E3/S
23	FORMAT( ' Mean life in F (=B u C) (ms) ',6X,G13.6)
200	continue
	RETURN
	END


