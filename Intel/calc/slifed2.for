	subroutine SLIFED2(Q,Peq,pon,discprt,kq,kp)
c Version of SLIFED1 with pon and discprt as params (so can show on screen only)
c Version of SLIFED with modified print-out (based on MPERM), and
c also printing mean latency to next opening/shutting, given
c start in each state
C TYPE MEAN LIFE IN EACH SUBSET (COULD ALSO HAVE S AND PVAL AS
C ARRAYS IN PARAM LIST). DOUBLE PRECISION VERSION
	allocatable::Q1,tim
	real*8 Q(kq,kq),Q1(:,:)
	real*8 PEQ(kp),S,PVAL,det
	real*4 tim(:)
	logical pon,discprt,latOK
	INTEGER KVAL(4)
	COMMON/KBLK/KA,KB,KC,KD
	COMMON/determ/det			!for MATINV
C
	km=kq			!for submat, matinv
c
	allocate(Q1(kq,kq),tim(kq))
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
	call SUBMAT(Q,11,Q1,km,km,km,km)		!QAA in Q1
	call MATINV(Q1,kA,km,Q1,km)			!inv(QAA) in Q1
	is=0
	do i=1,kA
	   is=is+1
	   tim(is)=0.0
	   do j=1,k
		tim(is)=tim(is)-sngl(Q1(i,j))
	   enddo
	enddo
c and now shut states
	kT=kB+kC+kD
	call SUBMAT(Q,77,Q1,km,km,km,km)		!QTT in Q1
	call MATINV(Q1,kT,km,Q1,km)			!inv(QTT) in Q1
	latOK=dabs(det).ge.1.e-10	!QTT not singular so shut latencies OK
	if(latOK) then
	   do i=1,kT
		is=is+1
		tim(is)=0.0
		do j=1,k
		   tim(is)=tim(is)-sngl(Q1(i,j))
		enddo
	   enddo
	endif
c
C DO LOOP FOR EACH SUBSET
	DO 1000 M=1,4	!EACH SUBSET
	if(m.eq.1) then
c	  print 21		!TITLES for A states
	  if(pon) write(7,21)
	  if(discprt) write(7,21)
21	  FORMAT(/,
     &' Subset  Open     Equilibrium    Mean life   Mean latency (ms)'/
     &'         state     occupancy        (ms)     to next shutting'/
     &'         (#i)                                given start in i')
	else if(m.eq.2) then
c        print 19		!TITLES for all shut states
	  if(pon) write(7,19)
	  if(discprt) write(7,19)
19	  FORMAT(/,
     &' Subset  Shut     Equilibrium    Mean life   Mean latency (ms)'/
     &'         state     occupancy        (ms)     to next opening'/
     &'         (#i)                                given start in i')
	endif
c
	S=0.0d0
	k1=k2+1
	k2=kval(m)
	if(k2.lt.k1) goto 1000	!SKIP EMPTY SUBSETS
C CALC MEAN LIFETIME IN CURRENT SUBSET
	Pval=0.0d0
	do 211 i=k1,k2
211	Pval=Pval+Peq(i)	!total occ for current subset
	if(Pval.ge.1.d-20) then
	   do 20 i=k1,k2		!ITH ROW IN CURRENT SUBSET
	   do 20 j=1,k		!BUT SKIP ELEMENTS WITHIN CURRENT SUBSET
	   if(j.lt.k1.or.j.gt.k2) S=S+(Peq(i)/Pval)*Q(i,j)	!TOTAL RATE OUT
20	   continue	!END OF LIFE CALC
	endif
c
c TYPE SUBSET OCC AND LIFETIME
c
	if(m.eq.1) then
	   if(s.ge.1.d-20.and.Pval.gt.1.d-20) then
c		print 212,pval,1.d3/S
      	if(pon) write(7,212) pval,1.d3/S
      	if(discprt) write(7,212) pval,1.d3/S
212		format('    A ',12x,g13.6,1x,g13.6)	!no CR after title
	   else if(s.lt.1.d-20.and.Pval.gt.1.d-20) then
c		print 2121,pval
      	if(pon) write(7,2121) pval
      	if(discprt) write(7,2121) pval
2121		format('    A ',12x,g13.6,1x,' >3.e12 years')
	   else if(Pval.lt.1.d-20) then
c		print 2122,pval
      	if(pon) write(7,2122) pval
      	if(discprt) write(7,2122) pval
2122		format('    A ',12x,g13.6)
	   endif
	else if(m.eq.2) then
	   if(s.ge.1.d-20.and.Pval.gt.1.d-20) then
c		print 213,pval,1.d3/S
      	if(pon) write(7,213) pval,1.d3/S
      	if(discprt) write(7,213) pval,1.d3/S
213		format('    B ',12x,g13.6,1x,g13.6)	!no CR after title
	   else if(s.lt.1.d-20.and.Pval.gt.1.d-20) then
c		print 2131,pval
      	if(pon) write(7,2131) pval
      	if(discprt) write(7,2131) pval
2131		format('    B ',12x,g13.6,1x,' >3.e12 years')
	   else if(Pval.lt.1.d-20) then
c		print 2132,pval
      	if(pon) write(7,2132) pval
      	if(discprt) write(7,2132) pval
2132		format('    B ',12x,g13.6)
	   endif
	else if(m.eq.3) then
	   if(s.ge.1.d-20.and.Pval.gt.1.d-20) then
c		print 214,pval,1.d3/S
      	if(pon) write(7,214) pval,1.d3/S
      	if(discprt) write(7,214) pval,1.d3/S
214		format('    C ',12x,g13.6,1x,g13.6)	!no CR after title
	   else if(s.lt.1.d-20.and.Pval.gt.1.d-20) then
c		print 2141,pval
      	if(pon) write(7,2141) pval
      	if(discprt) write(7,2141) pval
2141		format('    C ',12x,g13.6,1x,' >3.e12 years')
	   else if(Pval.lt.1.d-20) then
c		print 2142,pval
      	if(pon) write(7,2142) pval
      	if(discprt) write(7,2142) pval
2142		format('    C ',12x,g13.6)
	   endif
	else if(m.eq.4) then
	   if(s.ge.1.d-20.and.Pval.gt.1.d-20) then
c		print 215,pval,1.d3/S
      	if(pon) write(7,215) pval,1.d3/S
      	if(discprt) write(7,215) pval,1.d3/S
215		format('    D ',12x,g13.6,1x,g13.6)	!no CR after title
	   else if(s.lt.1.d-20.and.Pval.gt.1.d-20) then
c		print 2151,pval
      	if(pon) write(7,2151) pval
      	if(discprt) write(7,2151) pval
2151		format('    D ',12x,g13.6,1x,' >3.e12 years')
	   else if(Pval.lt.1.d-20) then
c		print 2152,pval
      	if(pon) write(7,2152) pval
      	if(discprt) write(7,2152) pval
2152		format('    D ',12x,g13.6)
	   endif
	endif
c
c TYPE INDIV STATE NO,OCC AND LIFETIME within each subset
	do 16 i=k1,k2
	if(-q(i,i).ge.1.d-20.and.latOK) then
c	   print 17,i,Peq(i),-1.e3/Q(i,i),1.e3*tim(i)
         if(pon) write(7,17) i,Peq(i),-1.e3/Q(i,i),1.e3*tim(i)
         if(discprt) write(7,17) i,Peq(i),-1.e3/Q(i,i),1.e3*tim(i)
17	   format(7x,i4,7x,3(g13.6,2x))
	else if(-q(i,i).ge.1.d-20.and.(.not.latOK)) then
c	   print 170,i,Peq(i),-1.e3/Q(i,i)
         if(pon) write(7,170) i,Peq(i),-1.e3/Q(i,i)
         if(discprt) write(7,170) i,Peq(i),-1.e3/Q(i,i)
170	   format(7x,i4,7x,2(g13.6,2x))
	else if(-q(i,i).lt.1.d-20) then
c	   print 171,i,Peq(i)
         if(pon) write(7,171) i,Peq(i)
         if(discprt) write(7,171) i,Peq(i)
171	   format(7x,i4,7x,g13.6,2x,' >3.e12 years')
	endif
16	continue
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
c	IF(pon) write(7,23)1.E3/S
c      if(discprt) write(7,23)1.E3/S
c23	FORMAT( ' Mean life in F (=B u C) (ms) ',6X,G13.6)
c200	continue
c
	deallocate(Q1,tim)
	RETURN
	END

