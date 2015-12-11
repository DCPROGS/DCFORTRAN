	subroutine SCJOFF(QD,p0,HAA,eighaa,RAA,amat,hmat,end2,ucol,
     & short,nmax,km)
c Subroutine for SCJUMP to calculate off-jumps for jump from finite conc
c to zero conc of agonist.  In this case QCC and QFF usually (always?)
c singular, and must calculate distributions of the 'rest of the burst'
c given that we are in B at t=0.  For these calculations C must contain
c all the absorbing shut states, and B all the non-absorbing shut states
c (regardless of their length).
c 	The end of burst vector,eb,is already calc in end2=(GAB*GBC+GAC)*uC
c HAA=GAB*GBA and eighaa also already calc
c RAA=inv(I-HAA) ditto
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	dimension QD(100,100),Q1(100,100),Q2(100,100)
	dimension RAA(100,100),HAA(100,100)
	dimension AMAT(100,100,100),Hmat(100,100,100)
	dimension GAB(100,100),GBA(100,100)
	dimension EIGHAA(100),EIGEN(100)
	dimension P0(100)		!holds initial occs
	dimension END2(100,1)
	dimension ROW1(1,100),ROW2(1,100),COL1(100,1),COL2(100,1)
	dimension UCOL(100,1)
	dimension phi0(100),phi1(100)		!init vectors at t=0
	dimension W1(100)
	dimension Prn(100)		!to keep Prob[r=>n]
	real*4 W(100),am,sd,area(100),amean(100)
	real*4 amob,amgb,amo,amg,amto1,amto,amtg1,amb1,amb,popen
	INTEGER A,B,C,F,AA,AB,AC,BA,BB,BC,CA,CB,CC,EE,AF,FA,FF
c
	logical slock,caplock,pon,debug,skip,short
	logical discprt
      character*1 ans,UC
c
	common/KBLK/kA,kB,kC,kD
	COMMON/determ/det			!from MATINV,GMAT1
	common/dp/discprt
c
c
	pon()=slock()
	debug()=caplock()
c
C MISC DEFINITIONS
3	format(i8)
	one=1.0d0
	k=kA+kB+kC
	kE=kA+kB
c
C DEFINE AA,AB ETC AS ARGUMENTS FOR SUBMAT WHICH GETS A SUBMATRIX
C FROM QD. THEY ARE DECLARED AS INTEGERS AND VALUE IJ=ROW,COL- THEY
C ARE ARBITRARY INTEGERS INTERP BY SUBMAT TO GET REQ ROWS AND COLS
	A=1
	B=2
	C=3
	F=5
	AA=11
	AB=12
	AC=13
	BA=21
	BB=22
	BC=23
	CA=31
	CB=32
	CC=33
	AF=15	!5=CODE FOR F(=B+C) SECTION
	FA=51
	FF=55
	EE=66		!6=CODE FOR E(=BURST=A+B)
c  Recalculate GAB, GBA
      call GMAT1(AB,QD,GAB,km,km)			!GAB in GAB
      call GMAT1(BA,QD,GBA,km,km)			!GBA in GBA
c
      print 8
      if(pon()) write(7,8)
      if(discprt) write(8,8)
8	format(//,
     & ' DISTRIBUTIONS CALCULATED ONLY FOR CASE OF JUMP TO ZERO CONC',/,
     & '  (ALL CONDITIONAL ON THERE BEING AT LEAST ONE OPENING)',/,
     & '********************************************************',/,
     & '********************************************************',/,/,
     & ' DISTRIBUTIONS CONDITIONAL ON BEING IN B AT t=0',/,
     & '***********************************************')
c
	call INVEC(B,phi0,row1,P0,skip,km)		!subset 2=B
      print 73,(ROW1(1,j),j=1,kB)
      if(pon()) write(7,73) (ROW1(1,j),j=1,kB)
      if(discprt) write(8,73) (ROW1(1,j),j=1,kB)
73	format(' Initial vector, phiB(0) = pB(0)/pB(0).uB =',7(1x,f10.6))
c
c Fraction that start in A,B,C
	pA=0.0d0
	do i=1,kA
	 pA=pA+p0(i)		!=pA(0)uA   =total A occ at t=0
	enddo
c
	pC=0.0d0
	do 1 i=kE+1,k
1	 pC=pC+p0(i)		!=pC(0)uC   =total C occ at t=0
c
	pF=0.0d0
	do 2 i=kA+1,k
2	 pF=pF+p0(i)		!=pF(0)uF   =total shut occ at t=0
c
	pB=0.0d0
	do 4 i=kA+1,kE
4	 pB=pB+p0(i)		!=pB(0)uB   =total B occ at t=0
c
	if(pF.gt.1.d-20) then
	   pC1=pC/pF
	   pB1=pB/pF
         print 6,pC1,pB1
         if(pon()) write(7,6) pC1,pB1
         if(discprt) write(8,6) pC1,pB1
6	   format(
     & ' Fraction of channels that are SHUT at t=0 which',/,
     & ' (1) are in C, and so produce no opening',g13.6,/,
     & ' (2) are in B, and so may produce an opening',g13.6)
	else
         print 5
         if(pon()) write(7,5)
         if(discprt) write(8,5)
5	   format(
     &' Total fraction shut at t=0 is zero so cannot calculate fraction'
     &,' that start in B,C')
	endif
c
c Calculate Prob(0)=phiB(0).GBC.uC, given shut at t=0
      call GMAT1(BC,QD,Q1,km,km)			!GBC in Q1
	call MATMUL(Q1,ucol,col1,kB,kC,1,one,km,km,km,1,km,1)    !GBC.uC in col1
      call VECMUL(phi0,col1,Pr0,kC,km,km)	  !Prob(0) in Pr0 given shut
c Amd as fraction of ALL channels (SCJUMP1.CHI notes, eq 2.6,2.7)
	pnull=0.0d0
	do i=1,kB
	   pnull=pnull + p0(i+kA)*col1(i,1)		!pB(0)*GBC*uC
	enddo
	pnull=pC + pnull 		!add to give pC(0)*uC+pB(0)*GBC*uC
c
c Calculate Prob(r>=1)=phiB(0).GBA.uA
c      call GMAT1(BA,QD,Q1,km,km)			!GBA in Q1
	call MATMUL(GBA,ucol,col2,kB,kA,1,one,km,km,km,1,km,1)    !GBA.uA in col2
      call VECMUL(phi0,col2,Prr,kB,km,km)				!Prob(r=>1) in Prr
	pnullf=(pC + pB*Pr0)/pF
c Amd as fraction of ALL channels (SCJUMP1.CHI notes, eq 2.6,2.7), calc
c p(r>=1) separately, as check, as pA(0)*uA + pB(0)*GBA*uA
	pop=pA            !=pA(0)*uA
	do i=1,kB
	   pop=pop + p0(i+kA)*col2(i,1)	!pA(0)*uA + pB(0)*GBA*uA
	enddo
	tot=pnull+pop	!check total
c Keep pA(0) + pB(0)*GBA for later use as init vector for overall pdf
	do i=1,kA
	   phi1(i)=p0(i)
	   do j=1,kB
		phi1(i)=phi1(i) + p0(j+kA)*GBA(j,i)
	   enddo
	enddo
	s=0.0d0
	do i=1,kA
	   s=s + phi1(i)		!check the sum phi1*uA=pop
	enddo
c Last 2 (pnull, pop) added 12/28/94 02:50pm
      print 7,Pr0,1.0-Pr0,pnullf,1.0-pnullf,pnull,pop,tot
      if(pon()) write(7,7) Pr0,1.0-Pr0,pnullf,1.0-pnullf,pnull,pop,tot
      if(discprt) write(8,7) Pr0,1.0-Pr0,pnullf,1.0-pnullf,pnull,pop,tot
7	format(/,
     & ' Fraction of those in B at t=0 that produce NO openings = ',
     & f10.6,/,
     & ' Fraction of those in B at t=0 that produce openings = ',
     & f10.6,/,
     & ' Fraction of those SHUT at t=0 that produce NO openings = ',
     & f10.6,/,
     & ' Fraction of those SHUT at t=0 that produce openings = ',
     & f10.6,/,/,
     & ' Fraction of ALL channels that produce NO openings = ',
     & f10.6,/,
     & ' Fraction of ALL channels that produce at least one opening = ',
     & f10.6,/,
     & '   Sum of last two = ',
     & f10.6,/)
	if(.not.short) then
	   call ATYPD(GBA,' GBA    ',kB,kA,km,km)
	   call ATYPD(end2,' END2=eb',kA,1,km,km)
	endif
c
c Probability of getting r openings after t=0
c P(r)=phiB(0).GBA.[(GAB*GBA)**(r-1)]*eb
c	call QMAT5(HAA,Hmat,kA,eighaa,ibad,km,km,km)
c	IF(IBAD.NE.0) print 14,IBAD
c14	format(' ***ERROR IN SPECTRAL EXPANSION: ibad= ',i4)
      print 321
      if(pon()) write(7,321)
      if(discprt) write(8,321)
321	FORMAT(//,
     & ' Number (r) of openings following start in B at t=0 (r=>1)',/,
     & ' =========================================================',/,
     & '  m     lambda(m)     norm mean')
	do 322 m=1,kA
	   amean(m)=sngl(one/(one-eighaa(m)))
	   print 323,m,EIGHAA(m),amean(m)
	   if(pon()) write(7,323)m,EIGHAA(m),amean(m)
         if(discprt) write(8,323)m,EIGHAA(m),amean(m)
322	continue	!END OF m LOOP
323	format(i3,2x,2g13.6)
c Calculate complete initial vec phiB(0).GBA in row1
      call MATMUL(phi0,GBA,row1,1,kB,kA,one,1,km,km,km,1,km)
c Calc amplitudes (also some P(r) values?)
      print 72,(ROW1(1,j),j=1,kA)
      if(pon()) write(7,72) (ROW1(1,j),j=1,kA)
      if(discprt) write(8,72) (ROW1(1,j),j=1,kA)
72	format(' Initial vector, phiB(0)*GBA= ',7(1x,f10.6))
	call RAC2(row1,end2,Hmat,kA,w,w1,km,km,km)
	do m=1,kA
	   area(m)=sngl(w1(m)/(one-eighaa(m)))		!area
	   area(m)=area(m)/Prr                    !conditional on at least one
	enddo
	call PDrOUTs('  ',-1,-1,area,amean,kA,am,sd,
     & km,pon(),.true.,discprt)
c Direct calc of means, as check. phiB(0)*GBA*RAA*uA
      call MATMUL(row1,RAA,row2,1,kA,kA,one,1,km,km,km,1,km)
      call VECMUL(row2,ucol,am1,kA,km,km)	      !*ucol
	am2=am1/Prr       !am1/P(r=>1)
      print 9,am1,am2
      if(pon()) write(7,9) am1,am2
      if(discprt) write(8,9) am1,am2
9	format(
     & ' Mean number of openings (inc zeros) after start in B at t=0 = '
     & ,f10.6,/,
     & ' Mean number of openings, given at least one opening         = '
     & ,f10.6)
c Calc some P(r) values?
      print 10
10	format(
     & ' Print probability of r openings, given at least one [Y] ? ')
	ans='Y'
	call INPUTa(ans)
      if(UC(ans).ne.'Y') goto 17
	n1=1 		!defaults
	n2=nmax
      print 11,n2
11	format(' Print for r = n1 to n2:  n1, n2 [1,',i2,'] = ')
	call INPUT2i(n1,n2)
	if(n1.lt.1) n1=1		!n1<1 not allowed
      print 141
      if(pon()) write(7,141)
      if(discprt) write(8,141)
141	format(/,'   r        P(r)         P(r)/P(r=>1)')
c Initial vec = phiB(0).GBA in row1; end vec =eb in end2
	s1=0.0d0
	s2=0.0d0
	do 13 i=n1,n2
	 kth=i-1
       call MATPOW(HMAT,eighaa,kth,Q1,kA,km,km,km)	!HAA**kth in Q1
       call MATMUL(row1,Q1,row2,1,kA,kA,one,1,km,km,km,1,km)
       call VECMUL(row2,end2,pr1,kA,km,km)
	 s1=s1+pr1
	 pr2=pr1/Prr	!normalised for r=>1
	 s2=s2+pr2
       print 15,i,pr1,pr2
       if(pon()) write(7,15) i,pr1,pr2
       if(discprt) write(8,15) i,pr1,pr2
15	 format(i4,3x,g13.6,2x,g13.6)
13	continue
      print 16,s1,s2
      if(pon()) write(7,16) s1,s2
      if(discprt) write(8,16) s1,s2
16	format(
     & ' --------------------------------------',/,
     & ' Sum:   ',g13.6,2x,g13.6)
17	continue
c
c LENGTH OF THE BURST FOLLOWING t=0
c f(t)=phiB(0)*[exp(QEE*t)]BA*(-QAA)*eb/den
c Add length of burst measured from start of 1st opening to end of last
c given start in B at t=0
      print 30
      if(pon()) write(7,30)
      if(discprt) write(8,30)
30	format(//,
     & ' Length of burst following start in B at t=0',/,
     & '     (from start of first opening to end of the last opening)',/,
     & ' ===========================================')
	fac=-one/Prr	!1/P(r=>1), neg since -QAA needed
	call MATMUL(QD,end2,col1,kA,kA,1,fac,km,km,km,1,km,1) !-QAA*end2 in col1
	call SUBMAT(QD,EE,Q1,km,km,km,km)	!QEE in Q1
	call QMAT5(Q1,Amat,kE,eigen,IBAD,km,km,km)
	IF(IBAD.NE.0) print 14,IBAD
14	format(' ***ERROR IN SPECTRAL EXPANSION: ibad= ',i4)
c Initial vector is phiB(0).GAB in row1, and use AA section of exp(QEE.t)
	call RAC3(row1,col1,amat,1,kA,1,kA,kE,w,w1,km,km,km)
	call PDFOUTd('            ',-1,-1,w1,eigen,kE,am,sd,
     & km,pon(),.true.,discprt)
      print 108		!new line
      if(pon()) write(7,108)
      if(discprt) write(8,108)
108	format(/)
c
c Burst length measured from t=0 to end of last opening
      print 18
      if(pon()) write(7,18)
      if(discprt) write(8,18)
18	format(//,
     & ' Length of burst following start in B at t=0',/,
     & '     (from t=0 to end of the last opening)',/,
     & ' ===========================================')
C Use kB,kA,not kE, in RAC as
c We want only the BA section of exp(QEE*t), so use rows kA+1.kA+kB and
c columns 1 to kA
	call RAC3(phi0,col1,amat,kA+1,kA+kB,1,kA,kE,w,w1,km,km,km)
	call PDFOUTd('            ',-1,-1,w1,eigen,kE,am,sd,
     & km,pon(),.true.,discprt)
      print 108		!new line
      if(pon()) write(7,108)
      if(discprt) write(8,108)
c
      print 21,nmax
21	format(
     & ' Calculate pdf of open and shut times 1 to n: n [',i2,'] = ')
	call INPUTi(nmax)
	if(nmax.lt.1) nmax=1
c
c LENGTH OF kth OPENING FOLLOWING t=0
c f(t)=phiB(0)*GBA*[(GAB*GBA)**k-1]*exp(QAA*t)*(-QAA)*uA/P(r=>k), k=1,2,...
c where P(r=>k)=phiB(0)*GBA*[(GAB*GBA)**k-1]*uA
c For k=1 the initial vector is phi0*GBA -already in row1 (from burst length
c calc), and GAB*GBA is in HAA (from main prog)
      print 182
      if(pon()) write(7,182)
      if(discprt) write(8,182)
182	format(//,
     & ' Length of nth opening following start in B at t=0',/,
     & ' =================================================')
c end vector=-QAA*uA/P(r=>1) in col1
	call SUBMAT(QD,AA,Q2,km,km,km,km)	!QAA in Q1
	call MATMUL(Q1,ucol,col1,kA,kA,1,-one,km,km,km,1,km,1)
c Expand QAA
	call SUBMAT(QD,AA,Q2,km,km,km,km)	!QAA in Q2
	call QMAT5(Q2,Amat,kA,eigen,IBAD,km,km,km)
c Loop for n openings. NB must also calc P(r=>n)=row1*uA in the loop
	do 23 n=1,nmax
      print 721,n,(row1(1,j),j=1,kA)
      if(pon()) write(7,721) n,(row1(1,j),j=1,kA)
      if(discprt) write(8,721) n,(row1(1,j),j=1,kA)
721	format(' n =',i3,': Initial vector=',7(1x,f10.6))
c Calc P(r=>n)=row1*uA
      call VECMUL(row1,ucol,Prn(n),kA,km,km)		!Prob(r=>n) in Prn
c Divide end vector by this normalizing factor
	do 24 j=1,kA
24	row2(1,j)=row1(1,j)/Prn(n)
	call RAC2(row2,col1,amat,kA,w,w1,km,km,km)
      print 26,Prn(n),(row2(1,j),j=1,kA)
      if(pon()) write(7,26) Prn(n),(row2(1,j),j=1,kA)
      if(discprt) write(8,26) Prn(n),(row2(1,j),j=1,kA)
26	format(' P(r=>n) =',f10.6,': Normalized init vector=',7(1x,f10.6))
	call PDFOUTd('            ',-1,-1,w1,eigen,kA,am,sd,
     & km,pon(),.true.,discprt)
      print 108		!new line
      if(pon()) write(7,108)
      if(discprt) write(8,108)
c Multiply init vector by GAB*GBA in HAA
	call MATMUL(row1,HAA,row1,1,kA,kA,one,1,km,km,km,1,km)
23	continue		!end of n loop
c
c LENGTH OF nth GAP FOLLOWING t=0 (n=1 is latency to 1st opening)
c f(t)=phiB(0)*[(GBA*GAB)**k-1]*exp(QBB*t)*QBA*uA/P(r=>k), k=1,2,...
c For k=1 the initial vector is just phi0 -copy it into row1 again
      print 181
      if(pon()) write(7,181)
      if(discprt) write(8,181)
181	format(//,
     & ' Length of nth shut time following start in B at t=0',/,
     & ' ===================================================')
	do 20 j=1,kB
20	row1(1,j)=phi0(j)
c Calc GBA*GAB in Q1
c      call GMAT1(BA,QD,Q1,km,km)			!GBA in Q1
c      call GMAT1(AB,QD,Q2,km,km)			!GAB in Q2
c	call MATMUL(Q1,Q2,Q1,kB,kA,kB,one,km,km,km,km,km,km)	!GBA*GAB in Q1
	call MATMUL(GBA,GAB,Q1,kB,kA,kB,one,km,km,km,km,km,km)	!GBA*GAB in Q1
c end vector=QBA*uA in col1
	call SUBMAT(QD,BA,Q2,km,km,km,km)	!QBA in Q2
	call MATMUL(Q2,ucol,col1,kB,kA,1,one,km,km,km,1,km,1) !QBA.uA in col1
c Expand QBB
	call SUBMAT(QD,BB,Q2,km,km,km,km)	!QBB in Q2
	call QMAT5(Q2,Amat,kB,eigen,IBAD,km,km,km)
c Loop for n openings
	do 22 n=1,nmax
	if(n.eq.1) then
         print 75
         if(pon()) write(7,75)
         if(discprt) write(8,75)
75	   format(/,' Latency to the first opening:')
	endif
      print 721,n,(row1(1,j),j=1,kB)
      if(pon()) write(7,721) n,(row1(1,j),j=1,kB)
      if(discprt) write(8,721) n,(row1(1,j),j=1,kB)
c721	format(' n =',i3,': Initial vector=',7(1x,f10.6))
c Normalize with P(r=>n)=Prn(n) calc above
	do 25 j=1,kB
25	row2(1,j)=row1(1,j)/Prn(n)
	call RAC2(row2,col1,amat,kB,w,w1,km,km,km)
      print 26,Prn(n),(row2(1,j),j=1,kB)
      if(pon()) write(7,26) Prn(n),(row2(1,j),j=1,kB)
      if(discprt) write(8,26) Prn(n),(row2(1,j),j=1,kB)
	call PDFOUTd('            ',-1,-1,w1,eigen,kB,am,sd,
     & km,pon(),.true.,discprt)
      print 108		!new line
      if(pon()) write(7,108)
      if(discprt) write(8,108)
c Multiply init vector by GBA*GAB so phiB(0)*[GBA*GAB**n-1] accum in ROW1
	call MATMUL(row1,Q1,row1,1,kB,kB,one,1,km,km,km,1,km)
22	continue		!end of n loop
c
c NOW DO OVERALL DISTRIBUTIONS -THIS SECTION IS FOR conc=0 SO GET ONLY
c ONE BURST (AT MOST). Can thus calculate overall pdf of open times, shut
c times within the burst (inc and exc 1st latency), regardless of state
c at t=0.  But first get overall burst length.
c Have already stored [pA(0) + pB(0)*GBA] in phi1(), above -copy
c it into row1()
	do j=1,kA
	   row1(1,j)=phi1(j)
	enddo
      print 81
      if(pon()) write(7,81)
      if(discprt) write(8,81)
81	format(//,
     & ' OVERALL DISTRIBUTIONS FOR ANY STARTING STATES AT t=0',/,
     & '******************************************************')
c
      print 82
      if(pon()) write(7,82)
      if(discprt) write(8,82)
82	format(//,
     & ' Overall Length of burst (excluding 1st latency)',/,
     & ' ===============================================')
c f(t)=[pA(0) + pB(0)*GBA]*[exp(QEE*t)]AA*(-QAA)*eb/P(R>=1)
c where P(R>=1) is overall prob of 1 or more openings, calc as POP above
	fac=-one/pop	!1/P(r=>1), neg since -QAA needed
	call MATMUL(QD,end2,col1,kA,kA,1,fac,km,km,km,1,km,1) !-QAA*end2 in col1
	call SUBMAT(QD,EE,Q1,km,km,km,km)	!QEE in Q1
	call QMAT5(Q1,Amat,kE,eigen,IBAD,km,km,km)
	IF(IBAD.NE.0) print 14,IBAD
c Initial vector is [pA(0) + pB(0)*GBA] in row1, and use AA section of exp(QEE.t)
	call RAC3(row1,col1,amat,1,kA,1,kA,kE,w,w1,km,km,km)
	call PDFOUTd('            ',-1,-1,w1,eigen,kE,amb,sd,
     & km,pon(),.true.,discprt)
c      print 108		!new line
c      if(pon()) write(7,108)
c      if(discprt) write(8,108)
c
      print 83
      if(pon()) write(7,83)
      if(discprt) write(8,83)
83	format(//,
     & ' Overall total-open-time per burst',/,
     & ' ===============================================')
C Calc VAA
	do i=1,kA
	 do j=1,kA
	   Q2(I,J)=-HAA(I,J)
	   if(i.eq.j) Q2(i,j)=one-HAA(i,j)		!(I-HAA) in Q2
	 enddo
	enddo
	call SUBMAT(QD,AA,Q1,km,km,km,km)		!QAA IN Q1
	call MATMUL(Q1,Q2,Q1,kA,kA,kA,ONE,km,km,km,km,km,km)		!VAA IN Q1
C   Final vector=-VAA*uA in COL1
	fac=-one/pop	!1/P(r=>1), neg since -QAA needed
	call MATMUL(Q1,ucol,col1,kA,kA,1,fac,km,km,km,1,km,1)
C Spec exp of VAA
	call QMAT5(Q1,Amat,kA,eigen,IBAD,km,km,km)
	if(ibad.ne.0) print 14,IBAD
c Initial vector is [pA(0) + pB(0)*GBA] in row1, and use AA section of exp(QEE.t)
	call RAC3(row1,col1,amat,1,kA,1,kA,kA,w,w1,km,km,km)
	call PDFOUTd('            ',-1,-1,w1,eigen,kA,amto,sd,
     & km,pon(),.true.,discprt)
c
c Openings per burst
c P(r)=[pA(0) + pB(0)*GBA] [(GAB*GBA)**(r-1)]*eb/P(R>=1)
      print 84
      if(pon()) write(7,84)
      if(discprt) write(8,84)
84	format(//,
     & ' Overall number of openings per burst',/,
     & ' ====================================')
	call RAC2(row1,end2,Hmat,kA,w,w1,km,km,km)
	amr=0.0d0		!for double-prec mean
	do m=1,kA
	   w1(m)=w1(m)/pop		!incorporate P(R>=1) here
	   amean(m)=sngl(one/(one-eighaa(m)))
	   area(m)=sngl(w1(m)/(one-eighaa(m)))		!area
	   amr=amr + w1(m)/(one-eighaa(m))**2
	enddo
	call PDrOUTs('  ',-1,-1,area,amean,kA,amob,sd,
     & km,pon(),.true.,discprt)
c
c Calc some P(r) values?
      print 89
89	format(/,
     & ' Print probability of r openings [Y] ? ')
	ans='Y'
	call INPUTa(ans)
      if(UC(ans).ne.'Y') goto 90
	n1=1 		!defaults
c	n2=10
	n2=nmax
      print 11,n2
c11	format(' Print for r = n1 to n2:  n1, n2 [1,',i2,'] = ')
	call INPUT2i(n1,n2)
	if(n1.lt.1) n1=1		!n1<1 not allowed
      print 92
      if(pon()) write(7,92)
      if(discprt) write(8,92)
92	format(/,
     & '   r       P(r)       P(r)/P(r=>1)       rP(r)   rP(r)/P(r=>1)')
c Initial vec = phiB(0).GBA in row1; end vec =eb in end2
	s1=0.0d0
	s2=0.0d0
	s3=0.0d0
	s4=0.0d0
	do i=n1,n2
	   r=dble(float(i))
	   kth=i-1
         call MATPOW(HMAT,eighaa,kth,Q1,kA,km,km,km)	!HAA**kth in Q1
         call MATMUL(row1,Q1,row2,1,kA,kA,one,1,km,km,km,1,km)
         call VECMUL(row2,end2,pr1,kA,km,km)
	   s1=s1+pr1
	   pr2=pr1/pop	!normalised for r=>1
	   s2=s2+pr2
	   rpr1=r*pr1
	   s3=s3+rpr1
	   rpr2=r*pr2
	   s4=s4+rpr2
         print 93,i,pr1,pr2,rpr1,rpr2
         if(pon()) write(7,93) i,pr1,pr2,rpr1,rpr2
         if(discprt) write(8,93) i,pr1,pr2,rpr1,rpr2
93	   format(i4,2x,g13.6,1x,g13.6,3x,g13.6,1x,g13.6)
	enddo
      print 94,s1,s2,s3,s4
      if(pon()) write(7,94) s1,s2,s3,s4
      if(discprt) write(8,94) s1,s2,s3,s4
94	format(
     & ' -------------------------------------------------------',/,
     & ' Sum:   ',g13.6,1x,g13.6,2x,g13.6,1x,g13.6)
90	continue
c
c
c Overall distribution of open times
c==================================
c f(t) = [pA(0) + pB(0)*GBA]*inv(I-GAB*GBA)*exp(QAA*t)*(-QAA)*uA / E(r)
      print 85
      if(pon()) write(7,85)
      if(discprt) write(8,85)
85	format(//,
     & ' Overall distribution of open times',/,
     & ' ==================================')
c Init vector [pA(0) + pB(0)*GBA]*inv(I-GAB*GBA) in row2
      call MATMUL(row1,RAA,row2,1,kA,kA,one,1,km,km,km,1,km)
c Final vector (-QAA)*uA / E(r)*P(R>=1) in col1
	fac=-one/(amr*pop)	!1/E(r)*P(R>=1), neg since -QAA needed
	call SUBMAT(QD,AA,Q1,km,km,km,km)	!QAA in Q1
	call MATMUL(Q1,ucol,col1,kA,kA,1,fac,km,km,km,1,km,1)
c Expand QAA
	call QMAT5(Q1,Amat,kA,eigen,IBAD,km,km,km)
	call RAC2(row2,col1,amat,kA,w,w1,km,km,km)
	call PDFOUTd('            ',-1,-1,w1,eigen,kA,amo,sd,
     & km,pon(),.true.,discprt)
c
c Overall distribution of gaps within the burst
c================================================
c f(t) = [pA(0) + pB(0)*GBA]*GAB*inv(I-GBA*GAB)*exp(QBB*t)*QBA*uA / den
      print 86
      if(pon()) write(7,86)
      if(discprt) write(8,86)
86	format(//,
     & ' Overall pdf of shut times in the burst (exc first latency)',/,
     & ' =========================================================')
c      call GMAT1(BA,QD,Q1,km,km)			!GBA in Q1
c      call GMAT1(AB,QD,Q2,km,km)			!GAB in Q2
	call MATMUL(GBA,GAB,Q1,kB,kA,kB,one,km,km,km,km,km,km)	!GBA*GAB in Q1
c calc RBB=inv(I-GBA*GAB) in Q2
	do i=1,kB
	 do j=1,kB
	   Q2(i,j)=-Q1(i,j)
	   if(i.eq.j) Q2(i,j)=one-Q1(i,j)		!(I-HBB) in Q2
	 enddo
	enddo
	call MATINV(Q2,kB,km,Q2,km)			!inv(I-HBB) in Q2
c  [pA(0) + pB(0)*GBA]*GAB in row2
      call MATMUL(row1,GAB,row2,1,kA,kB,one,1,km,km,km,1,km)
c Init vector [pA(0) + pB(0)*GBA]*GAB*inv(I-GBA*GAB) in row2
      call MATMUL(row2,Q2,row2,1,kB,kB,one,1,km,km,km,1,km)
c Calculate denominator [pA(0) + pB(0)*GBA]*inv(I-GAB*GBA)*GAB*GBA*uA
      call MATMUL(HAA,ucol,col2,kA,kA,1,one,km,km,km,1,km,1)!GAB*GBA*uA in col2
c    premult by RAA=inv(I-GAB*GBA)
      call MATMUL(RAA,col2,col2,kA,kA,1,one,km,km,km,1,km,1)
      call VECMUL(row1,col2,den,kA,km,km)	  !premult by row1 -> den
	den1=(amr-one)*pop	![E(r)-1]*P(r=>1)
      print 87,den,amr,pop,den1
      if(pon()) write(7,87) den,amr,pop,den1
      if(discprt) write(8,87) den,amr,pop,den1
87	format(/,
     & ' Denominator for this pdf = ',g14.7,/,
     & ' i.e. [E(r)-1]*P(r=>1) = (',g13.6,' - 1)*',g13.6,' = ',g14.7,/)
c end vector=QBA*uA/den in col1
	fac=one/den
	call SUBMAT(QD,BA,Q1,km,km,km,km)	!QBA in Q1
	call MATMUL(Q1,ucol,col1,kB,kA,1,fac,km,km,km,1,km,1) !QBA.uA/den in col1
c Expand QBB
	call SUBMAT(QD,BB,Q2,km,km,km,km)	!QBB in Q2
	call QMAT5(Q2,Amat,kB,eigen,IBAD,km,km,km)
c calc pdf
	call RAC2(row2,col1,amat,kB,w,w1,km,km,km)
	call PDFOUTd('            ',-1,-1,w1,eigen,kB,amg,sd,
     & km,pon(),.true.,discprt)
c
c Summary of means
	amgb=amob-1.0	!gaps/bst
	amto1=amob*amo	!open time/bst
	amtg1=amgb*amg	!shut time/bst
	amb1=amto+amtg1	!burst length
	popen=amto/amb	!Popen
      print 88,
     & amob,amgb,amo,amg,amto1,amto,amtg1,amb1,amb,popen
      if(pon()) write(7,88)
     & amob,amgb,amo,amg,amto1,amto,amtg1,amb1,amb,popen
      if(discprt) write(8,88)
     & amob,amgb,amo,amg,amto1,amto,amtg1,amb1,amb,popen
88	format(/,/,
     & ' Summary of means for overall distributions for the burst',/,
     & '  after jump to zero conc (given at least one opening)',/,
     & ' ========================================================',/,
     & ' (1) mean number of openings = ',g13.6,/,
     & ' (2) mean number of gaps (exc first latencies) = ',g13.6,/,
     & ' (3) mean open time = ',g13.6,/,
     & ' (4) mean shut time (exc first latencies) = ',g13.6,/,
     & ' (5) open time per burst = (1)*(3) = ',g13.6,/,
     & ' (6) open time per burst (direct calc) = ',g13.6,/,
     & ' (7) shut time per burst (exc first latencies) = (2)*(4) = ',
     &	g13.6,/,
     & ' (8) mean burst length (exc first latencies) = (6)+(7) = ',
     &	g13.6,/,
     & ' (9) mean burst length  (direct calc) = ',g13.6,/,
     & ' (10) P(open) within burst (exc first latencies) = (6)/(9) = ',
     &	g13.6)
c
c=====================
c== add direct calc of means
	RETURN
	end

