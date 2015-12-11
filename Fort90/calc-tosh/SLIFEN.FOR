	subroutine SLIFEn(Q,Peq,inew,kq,kp)
c SLIFEn.FOR is version of SLIFED1, for use in MPERM, SCLUST that types
c original (as from QGEN2) state numbering, as well as current numbering
c
c Version of SLIFED with modified print-out (based on MPERM), and
c also printing mean latency to next opening/shutting, given
c start in each state
C TYPE MEAN LIFE IN EACH SUBSET (COULD ALSO HAVE S AND PVAL AS
C ARRAYS IN PARAM LIST). DOUBLE PRECISION VERSION
	real*8 Q(kq,kq),Q1(10,10)
	real*8 PEQ(kp),S,PVAL
c	real*8 Q(10,10),Q1(10,10)
c	real*8 PEQ(10),S,PVAL
	real*4 tim(10)
	integer kval(4),INEW(10)
	logical discprt
	common/dp/discprt
	COMMON/KBLK/KA,KB,KC,KD
C
	k=kA+kB+kC+kD
	KVAL(1)=KA		!NEW KA ETC
	KVAL(2)=KA+KB
	KVAL(3)=KA+KB+KC
	KVAL(4)=KA+KB+KC+KD
	K2=0
c Calc of latency to next opening, given we start in specified shut state
c is given by f(t)=p(0)*exp(-QTT*t)*(-QTT)*uT (T=all shut states) where
c p(0) =[0 0 0..1..0]  ie=1 for state in question. Mean=p(0)*inv(-QTT)*uT
c ie the sum of the ith row of inv(-QTT). Do calcs now
	km=10
	call SUBMAT(Q,11,Q1,km,km,km,km)		!QAA in Q1
	call MATINV(Q1,kA,km,Q1,km)			!inv(QAA) in Q1
	is=0
	do 50 i=1,kA
	is=is+1
	tim(is)=0.0
	do 50 j=1,k
50	tim(is)=tim(is)-Q1(i,j)
c and now shut states
	kT=kB+kC+kD
	call SUBMAT(Q,77,Q1,km,km,km,km)		!QTT in Q1
	call MATINV(Q1,kT,km,Q1,km)			!inv(QTT) in Q1
	do 51 i=1,kT
	is=is+1
	tim(is)=0.0
	do 51 j=1,k
51	tim(is)=tim(is)-Q1(i,j)
c
C DO LOOP FOR EACH SUBSET
	DO 1000 M=1,4	!EACH SUBSET
	if(m.eq.1) then
	  print 21		!TITLES for A states
	  if(discprt) write(8,21)
21	  FORMAT(/,
     &'   (Original numbering of states shown in parentheses)',/,
     &' Subset  Open     Equilibrium    Mean life   Mean latency (ms)'/
     &'         state     occupancy        (ms)     to next shutting'/
     &'         (#i)                                given start in i')
	else if(m.eq.2) then
        print 19		!TITLES for all shut states
	  if(discprt) write(8,19)
19	  FORMAT(/,
     &' Subset  Shut     Equilibrium    Mean life   Mean latency (ms)'/
     &'         state     occupancy        (ms)     to next opening'/
     &'         (#i)                                given start in i')
	endif
c
	S=0.0D0
	K1=K2+1
	K2=KVAL(M)
	IF(K2.LT.K1) GOTO 1000	!SKIP EMPTY SUBSETS
C CALC MEAN LIFETIME IN CURRENT SUBSET
	PVAL=0.0D0
	DO 211 I=K1,K2
211	PVAL=PVAL+PEQ(I)	!TOTAL OCC FOR CURRENT SUBSET
	DO 20 I=K1,K2		!ITH ROW IN CURRENT SUBSET
	DO 20 J=1,K		!BUT SKIP ELEMENTS WITHIN CURRENT SUBSET
	IF(J.LT.K1.OR.J.GT.K2) S=S+(PEQ(I)/PVAL)*Q(i,j)	!TOTAL RATE OUT
c	print 200,M,I,J,PVAL,PEQ(I),Q(i,j),S
c      if(discprt) write(8,200) M,I,J,PVAL,PEQ(I),Q(i,j),S
c200	FORMAT(3I4,4G13.6)
20	CONTINUE	!END OF LIFE CALC
C TYPE SUBSET OCC AND LIFETIME
	IF(M.EQ.1) then
		print 212,PVAL,1.E3/S
      	if(discprt) write(8,212) PVAL,1.E3/S
c212		FORMAT(/,'    A ',12X,2G13.6)
212		FORMAT('    A ',12X,G13.6,1x,g13.6)	!no CR after title
	else if(M.EQ.2) then
		print 213,PVAL,1.E3/S
      	if(discprt) write(8,213) PVAL,1.E3/S
c213		FORMAT(/,'    B ',12X,2G13.6)
213		FORMAT('    B ',12X,G13.6,1x,g13.6)	!no CR after title
	else if(M.EQ.3) then
		print 214,PVAL,1.E3/S
	      if(discprt) write(8,214) PVAL,1.E3/S
214		FORMAT(/,'    C ',12X,G13.6,1x,g13.6)
	else if(M.EQ.4) then
		print 215,PVAL,1.E3/S
      	if(discprt) write(8,215) PVAL,1.E3/S
215		FORMAT(/,'    D ',12X,G13.6,1x,g13.6)
	endif
C TYPE INDIV STATE NO,OCC AND LIFETIME within each subset
	DO 16 I=K1,K2
	print 17,I,inew(i),PEQ(I),-1.E3/Q(i,i),1.e3*tim(i)
16    if(discprt) write(8,17) I,inew(i),PEQ(I),-1.E3/Q(i,i),1.e3*tim(i)
17	FORMAT(8x,i2,' (',i2,')',2x,3(G13.6,2x))
1000	CONTINUE
C
cc Add mean life in F=B u C=all shut states (Feb 1989)
c	k1=ka+1
c	k2=k
c	S=0.0D0
c	PVAL=0.0D0
c	DO 21 I=K1,K2
c21	PVAL=PVAL+PEQ(I)	!TOTAL OCC FOR CURRENT SUBSET
c	if(pval.lt.1.d-15) goto 200
c	DO 22 I=K1,K2		!ITH ROW IN CURRENT SUBSET
c	DO 22 J=1,K		!BUT SKIP ELEMENTS WITHIN CURRENT SUBSET
c	IF(J.LT.K1.OR.J.GT.K2) S=S+(PEQ(I)/PVAL)*Q(I,J)	!TOTAL RATE OUT
cc	print 200,M,I,J,PVAL,PEQ(I),Q(I,J),S
c22	CONTINUE
c	if(s.lt.1.0d-29) goto 200
c	print 23,1.E3/S
c      if(discprt) write(8,23)1.E3/S
c23	FORMAT( ' Mean life in F (=B u C) (ms) ',6X,G13.6)
c200	continue
c
	RETURN
	END


