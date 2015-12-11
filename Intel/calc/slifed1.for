	subroutine SLIFED1(Q,Peq,kq,kp)
C TYPE MEAN LIFE IN EACH SUBSET (COULD ALSO HAVE S AND PVAL AS
C ARRAYS IN PARAM LIST). DOUBLE PRECISION VERSION
c Version of SLIFED with modified print-out (based on MPERM), and
c also printing mean latency to next opening/shutting, given
c start in each state
c Modif 03/21/95 05:28pm for case where subset is occupied, and a route
c out of the subset exists, but the route out is from a state that has
c zero occupancy -this case gave error (at zero conc) -life in subset
c should be set undefined in this case
	allocatable::Q1,tim
	real*8 Q(kq,kq),Q1(:,:)
	real*8 PEQ(kp),S,PVAL
	real*4 tim(:)
	logical undef,inf
	logical pon
	INTEGER KVAL(4)
	character setchar*8
	logical discprt
	common/dp/discprt
	COMMON/KBLK/KA,KB,KC,KD
C
	pon=.false.
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
	km=100
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
	S=0.0D0
	K1=K2+1
	K2=KVAL(M)
	IF(K2.LT.K1) GOTO 1000	!SKIP EMPTY SUBSETS
C CALC MEAN LIFETIME IN CURRENT SUBSET
	PVAL=0.0D0
	inf=.true.		!lifetime infinite
	DO 211 I=K1,K2
211	PVAL=PVAL+PEQ(I)	!TOTAL OCC FOR CURRENT SUBSET
	undef=pval.lt.1.d-25
	if(.not.undef) then
	  do 20 i=k1,k2		!ITH ROW IN CURRENT SUBSET
	  do 20 j=1,k		!BUT SKIP ELEMENTS WITHIN CURRENT SUBSET
	   if(j.lt.k1.or.j.gt.k2) then
		S=S+(peq(i)/pval)*Q(i,j)	!TOTAL RATE OUT
		if(q(i,j).gt.1.d-25) then
		   inf=.false.					!lifetime not inf
		   if(peq(i).lt.1.d-25) undef=.true.	!but undef if zero occ!
		endif
	   endif
20	  continue	!END OF LIFE CALC
	endif
C TYPE SUBSET OCC AND LIFETIME
	if(m.eq.1) then
	   setchar='    A '
	else if(m.eq.2) then
	   setchar='    B '
	else if(m.eq.3) then
	   setchar='    C '
	else if(m.eq.4) then
	   setchar='    D '
	endif
	if(undef) then
c		print 212,setchar,pval
      	if(pon) write(7,212) setchar,pval
      	if(discprt) write(7,212) setchar,pval
212		format(a6,12x,g13.6,1x,'undefined')
	else if(inf) then
c		print 213,setchar,pval
      	if(pon) write(7,213) setchar,pval
      	if(discprt) write(7,213) setchar,pval
213		format(a6,12x,g13.6,1x,'infinite')
	else
c		print 214,setchar,pval,1000./s
      	if(pon) write(7,214) setchar,pval,1000./s
      	if(discprt) write(7,214) setchar,pval,1000./s
214		format(a6,12x,g13.6,1x,g13.6)
	endif
c
C TYPE INDIV STATE NO,OCC AND LIFETIME within each subset
	do 16 i=k1,k2
	 if(-q(i,i).gt.1.d-25) then
c	   print 17,I,PEQ(I),-1.E3/Q(i,i),1.e3*tim(i)
         if(pon) write(7,17) I,PEQ(I),-1.E3/Q(i,i),1.e3*tim(i)
         if(discprt) write(7,17) I,PEQ(I),-1.E3/Q(i,i),1.e3*tim(i)
17	   format(7x,i4,7x,3(g13.6,2x))
	 else
c	   print 18,i,peq(i)
         if(pon) write(7,18) i,peq(i)
         if(discprt) write(7,18) i,peq(i)
18	   format(7x,i4,7x,g13.6,'  infinite')
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

