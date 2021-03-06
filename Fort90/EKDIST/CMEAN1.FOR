	subroutine CMEAN1(tint,ampl,iprops,nint,nintt,avamp,acrit,
     & tcrit,TC,nbst,ISG)
c	SUBROUTINE CMEAN1(TINT,NINT,iampl,ACAL,MAVAMP,IACRIT,
c     & TCRIT,TC,NBST,SUBLEV,ISG)
c Version of CMEAN for new EKDIST/SCAN 02/16/94 07:25am
c
C Lahey version revised 06/23/90 08:45pm
c 11-73 version Aug 86. Arrays AVS,SDS,NS can be local now
c
C 14-SEP-84. NEW SUBROUTINE FOR EKDIST (MUCH-MODIF VERSION OF CDIST2) TO
C CALC VARIOUS MEANS (- ALL CASES WHERE NO DISTRIBUTION
C TO BE DISPLAYED OR FITTED)
C	ISTART(I)=INDEX IN TINT() OF 1ST OPENING IN THE ITH BURST
C	NOPS(I)=NUMBER OF OPENINGS IN THE ITH BURST (CONTIGUOUS SUBLEVELS
C		COUNTED AS SEP OPENINGS)
C
	real*4 TINT(nintt),ampl(nintt)
	integer*1 iprops(nintt)
	dimension AVS(81),SDS(81)
	dimension NS(80)
C==	INTEGER ISTART(512),NOPS(512)	!LOCAL
	character*1 ans,UC
c	LOGICAL OHIST,BHIST,GHIST,THIST,AHIST,OBHIST
c	logical sublev,bprint
	logical bprint
	LOGICAL CONAM1,DEBUG,OPEN,SUB,FULL,BAD,EXCSUB,NEXTOP
	LOGICAL SHUT,pon,exass,exass1,slock,caplock
	logical dubious,badgap,btest
	logical discprt
	common/dp/discprt
	COMMON/OVLBLK/TC1,M,CONAM1,exass,exass1,AFLAG,AMPLO,EXCSUB,Ith
C== IN THIS COMMON THE VARIABLES OBMAX,JTH WHICH ARE NOT NEEDED HERE
C ARE RELABELLED AS TC1,M WITHIN CMEAN- THESE ARE NEEDED HERE. SIMILARLY
C AMPHI IS RELABELLED AS AFLAG(NO LONGER USE AFLAG 21-SEP-84)
c
c
c Define functions
	pon()=slock()
	debug()=caplock()
	dubious(i)=BTEST(iprops(i),0)	!ampl(i) was dubious (bit 0 set ='1')
	badgap(i)=BTEST(iprops(i),3)	!tint(i) was unusable (bit 3='8' set)
c
108	FORMAT(/)
101	format(a1)
102	FORMAT(I8)
	DO 216 I=1,80
	AVS(I)=0.
	SDS(I)=0.
	NS(I)=0
216	CONTINUE
c	NOPEN=0		!NO OF INDIV OPENINGS ACCUM OVER EXPT
c	NASET=0		!NO OF SJOURNS IN C&H SET A, ACCUM OVER EXPT
C
320	print 321
321	FORMAT('&Critical gap length (ms)= ')
	call INPUTr(tcrit)
	N1=-1	!DEFAULTS
	N2=-1
	ISG=-1	!DEFAULT FOR NO ACTION
	AFLAG=-1.
	TC=0.
C	IF(.NOT.debug()) GOTO 623
	print 3211
3211	FORMAT('&List some burst compositions? ')
	read 101,ans
	if(UC(ans).EQ.'N') GOTO 623
3215	print 112
112	format('  (1) List all bursts')
	print 113
113	format('  (2) List from burst i1 to i2')
	print 114
114	format(
     & '  (3) List bursts with a gap longer than a specified value')
	print 115
115	format(
     & '  (4) List bursts with duration longer than a specified value')
	print 116
116	format(
     & '  (5) List bursts with K open periods')
	print 117
117	format(
     & '  (6) List bursts that contain a sublevel')
	print 118
118	format(/,' Option number (0 for help)= ')
	read 102,J
	if(j.lt.1.or.j.gt.6) GOTO 3215
	if(pon()) goto(1121,1131,1141,1151,1161,1171) J
	goto 3216
1121	print 112
	goto 3216
1131	print 113
	goto 3216
1141	print 114
	goto 3216
1151	print 115
	goto 3216
1161	print 116
	goto 3216
1171	print 117
3216	IF(j.EQ.1) TCRIT=-TCRIT
	IF(j.EQ.6) ISG=-2
	IF(j.NE.2) GOTO 3217
	print 3218
3218	FORMAT(' i1,i2= ')
	read 3422,N1,N2
3422	FORMAT(2I8)
	if(pon()) write(7,111)n1,n2
      if(discprt) write(8,111)n1,n2
111	format(' i1,i2= ',2i8)
	GOTO 623
3217	IF(J.NE.3.AND.J.NE.4) GOTO 3212
	print 3219
3219	FORMAT( '  Specified value (ms)= ')
	call INPUTr(tc)
	if(pon()) write(7,60)tc
      if(discprt) write(8,60)tc
60	format(  ' Specified value (ms)= ',g13.6)
	IF(J.EQ.4) TC=-TC
	GOTO 623
3212	IF(J.NE.5) GOTO 623
	print 624
624	FORMAT( ' K= ')
	read 102,ISG
C
623	continue
c	NSGAP=0		!NO OF GAPS .LE. TCRIT
c	NOPEN=0		!TOTAL NO OF INDIV OPENINGS (=NO OF A PERIODS-1)
c	NBST=0		!NO OF BURSTS
C
616	print 332
332	FORMAT(
     & '&Max number of open periods/burst for calcs (up to 8). M= ')
	read 102,M
	IF(M.GT.8) GOTO 616	!M=8 NEEDS 79 VALUES-OK FOR AVS(81)
C	print 333
C333	FORMAT( ' EXCLUDE BSTS WITH 1 OP FROM ''ANY BSTS'' CALCS? ')
C	read 101,ans
C	if(UC(ans).EQ.'Y') AFLAG=1.
	print 617
617	FORMAT(
     & '&Print mean open period before & after intermed gaps? ')
	read 101,ans
	TC1=-1.
	if(UC(ans).EQ.'N') GOTO 618
	print 3321
3321	FORMAT('&  minimum length for intermed gap (ms)= ')
	call INPUTr(tc1)
	if(pon()) write(7,3322)tc1
      if(discprt) write(8,3322)tc1
3322	FORMAT('   Minimum length for intermed gap (ms)= ',g13.6)
618	print 336
336	FORMAT('&Exclude sublevel durations? ')
	read 101,ans
	EXCSUB=.FALSE.
	if(UC(ans).EQ.'Y') EXCSUB=.TRUE.
	if(pon().and.excsub) write(7,123)
	if(discprt.and.excsub) write(8,123)
123	format(' Sublevel durations excluded')
C N.B.EXCLUSION OF SUBLEVELS MEANS EXCLUSION OF THEIR DURATION.
C JTH OPENING MEANS OPEN PERIOD PRECEDING JTH GAP WHETHER
C EXCSUB TRUE OR NOT
C
C330	CONTINUE	!JUMP HERE IF IFILE>1
	NMEAN=M*M+2*M-1		!TOTAL NUMBER OF MEANS
	I=1
	NB1=0		!NO OF BURSTS IN CURRENT EXPT
	NSG1=0		!NO OF GAPS-WITHIN-BST IN CURRENT EXPT
	NOP1=0		!NO IF INDIV OPENINGS DITTO
	NAS1=0		!NO OF OPEN (SUBSET A) PERIODS IN CURRENT EXPT
C BURST DEFINED BY GAPS (ZERO AMP) < TCRIT
C NOP AND NOPS(I) COUNT CONTIGUOUS SUBLEVELS AS SEP OPENINGS AS IN CDIST2
C NSG COUNTS GAPS<TCRIT WITHIN BURSTS SO NUMBER OF SOJOURNS IN C&H
C SUBSET A PER BURST=NSG+1=< NOP.
C NOTE UNUSABLE GAP COUNTED AS RESOLVABLE SO IT ENDS BURST, BUT
C UNUSABLE OPENING CAUSES WHOLE BURST TO BE IGNORED
C  FIRST LOOK FOR FIRST OPENING (MUST BE USABLE) IN BURST
360	bprint=.false.		!reset for each bst
c
c==	IF(IACAL(I,iampl).NE.0.AND.TINT(I).GT.0.0) GOTO 322
	if(ampl(i).ne.0.and.(.not.badgap(i))) goto 322
C    First opening of burst, tint(i), found so go to 322
	i=i+1
	if(i.le.nint) goto 360	!KEEP LOOKING FOR FIRST OPENING
	goto 99
C  Now have 1st opening of bst =tint(i). Do not change i until
c burst ends
322	NOP=1		!NUMBER OF OPENINGS IN CURRENT BURST
	NSG=0		!NO OF GAPS<TCRIT IN CURRENT BURST
	BLENG=0.	!TOTAL BURST LENGTH
c	NAS=0		!NO OF OPEN (SUBSET A) PERIODS IN CURRENT BST -not used
	NB1=NB1+1	!UPDATE # OF BURSTS- REVERSED BELOW IF BST IS BAD
	BAD=.FALSE.	!CURRENT BURST IS USABLE
C RECORD INDEX (=I)IN TINT() OF FIRST OPENING
C==	ISTART(NB1)=I
	BLENG=BLENG+TINT(I)	!ADD IN 1ST INTERVAL
	amp=ampl(i)
	OPEN=amp.ne.0
	SHUT=.NOT.OPEN
	FULL=abs(amp-avamp).le.acrit
c==	IF((.NOT.SUBLEV).AND.OPEN) FULL=.TRUE.
C SO IACRIT,MAVAMP NOT NEEDED WHEN SUBLEV=FALSE- ALL OPS COUNTED AS FULL
	SUB=OPEN.AND.(.NOT.FULL)
	IF(SUB.AND.ISG.EQ.-2) BPRINT=.TRUE. !PRINT BSTS WITH SUBLEVELS
	k1=i+1		!LOOK AT NEXT INTERVAL	DOES IT END BURST?
C  K1 LOOP STARTS AT LABEL 323
323	continue
	OPEN=ampl(k1).ne.0
	SHUT=.NOT.OPEN
	if(shut.and.(tint(k1).gt.abs(tcrit).or.
     & (badgap(k1).and.k1.lt.nint))) goto 324
C  GOTO TO 324 WHEN END OF BST FOUND WITH K1=INDEX OF GAP THAT ENDS IT
C REST DOWN TO 'GOTO 323' DONE FOR 2ND,3RD... INTERVAL IN BST
C NOTE THAT THE LAST INTERVAL IS ALWAYS SET UNUSABLE BUT IT IS NOT
C A VALID END-OF-BURST
	IF((OPEN.AND.badgap(k1)).OR.(K1.EQ.NINT)) BAD=.TRUE.
C    BURST NOT COUNTED IF IT CONTAINS AN UNUSABLE OPENING ,OR IF LAST
C  DATA POINT REACHED BEFORE VALID END-OF-BURST FOUND. SO SET BAD,
C  BUT CARRY ON TO END ANYWAY TO MAKE SURE START OF NEXT BST FOUND
C  CORRECTLY
	full=abs(ampl(k1)-avamp).le.acrit
c==	FULL=IABS(IACAL(K1,iampl)-MAVAMP).LE.IACRIT
c==	IF((.NOT.SUBLEV).AND.OPEN) FULL=.TRUE.
C SO IACRIT,MAVAMP NOT NEEDED WHEN SUBLEV=FALSE- ALL OPS COUNTED AS FULL
	SUB=OPEN.AND.(.NOT.FULL)
	IF(SUB.AND.ISG.EQ.-2) BPRINT=.TRUE. !PRINT BSTS WITH SUBLEVELS
	BLENG=BLENG+TINT(K1)	!ADD IN 2ND,3RD,... INTERVALS
	IF(SHUT.AND.(TC.GT.0.001).AND.TINT(K1).GE.TC) BPRINT=.TRUE.
C
	IF(OPEN) NOP=NOP+1
	IF(SHUT) NSG=NSG+1		!MUST BE <TCRIT AND USABLE
c	IF(debug()) print 405,I,K1,NOP,NSG
c405	FORMAT( ' I,K1,NOP,NSG= ',4I8)
	K1=K1+1
	IF(K1.LE.NINT) GOTO 323	!CONTINUE SEARCH FOR END OF BURST
	NB1=NB1-1	!REVERSE NB1 UPDATE IF NO PROPER ENDING FOUND
	GOTO 99		!DO NOT COUNT AS BURST IF NO PROPER ENDING FOUND
C
C END OF BURST FOUND. RECORD IT (IF NOT SET BAD) IF SET BAD YVAL(NB1)
C VALUES WILL BE OVERWRITTEN BY THOSE FOR NEXT BURST
C AT THIS POINT I=INDEX OF 1ST OPENING IN THIS BST, AND K1-1=INDEX
C  OF LAST OPENING.
324	CONTINUE
	IF(BAD) NB1=NB1-1	!REVERSE UPDATE OF NB1
	IF(BAD) GOTO 325
c	IF(debug()) print 4051,I,K1,NB1,NOP
c4051	FORMAT( ' I,K1,NB1,NOP= ',4I8)
	IF((TC.LT.-0.001).AND.(BLENG.GT.ABS(TC))) GOTO 357    !print BURST
	IF((TCRIT.LT.0.).OR.(NB1.GE.N1.AND.NB1.LE.N2).OR.BPRINT) GOTO 357
	IF(NSG+1.EQ.ISG) GOTO 357
	GOTO 354
357	DO 355 L=I,K1-1
	L1=L
	if(pon()) write(7,356)NB1,L,TINT(L),ampl(L1)
      if(discprt) write(8,356)NB1,L,TINT(L),ampl(L1)
355	print 356,NB1,L,TINT(L),ampl(L1)
356	FORMAT(2I6,2X,2G13.6)
	if(pon()) write(7,3571)BLENG
      if(discprt) write(8,3571)BLENG
	print 3571,BLENG
3571	FORMAT(' Length= ',G13.6)
	print 108
354	CONTINUE
C ASSIGN NOPS()
C==	NOPS(NB1)=NSG+1
C AT THIS POINT I=INDEX OF 1ST OPENING IN THIS BST, AND K1-1=INDEX
C  OF LAST OPENING OF CURRENT BURST. GO BACK THROUGH THIS BURST TO
C ASSIGN LENGTHS OF OPEN ANDSHUT PERIODS UNLESS (A)BURST HAS MORE THAN
C M OPEN PERIODS OR (B)BURST IS SET BAD
C ALL THIS WILL BE SKIPPED IF BURST SET BAD ALREADY.
C   N.B.EXCLUSION OF SUBLEVELS MEANS EXCLUSION OF THEIR DURATION.
C  JTH OPENING MEANS OPEN PERIOD PRECEDING JTH GAP WHETHER
C EXCSUB TRUE OR NOT.
C YVAL SET TO GAP LENGTH OR TO LENGTH OF OPEN PERIOD (EXC TIME IN
C SUBLEVELS IF EXCSUB TRUE)
	K=NSG+1		!NO OF OPEN PERIODS IN CURRENT BURST
	IF(K.GT.M) GOTO 338	!SKIP THIS BURST
C
	JSG=0
C JSG COUNTS SHORT GAPS IN THIS BST; JSG+1=OPEN PERIOD #
	MGAP=-1
	YVAL=0.0	!READY FOR 1ST OPEN PERIOD
C
c	IF(debug()) print 700,NBST+NB1,I,K1
c700	FORMAT(/,' ***NBST,I,K1= ',3I8)
C
	ifst=i
	ntemp=k1-i
	DO 339 L=ifst,K1-1		!GO THROUGH ALL INTERVALS IN BST
	L1=L
c==	OPEN=IACAL(L1,iampl).NE.0	!THIS INTERVAL=OPEN
	open=ampl(L1).ne.0
	SHUT=.NOT.OPEN
	NEXTOP=ampl(L1+1).ne.0		!NEXT INTERVAL OPEN
	IF(L.EQ.K1-1) NEXTOP=.FALSE.
	full=abs(ampl(L1)-avamp).le.acrit
c==	IF((.NOT.SUBLEV).AND.OPEN) FULL=.TRUE.
C SO IACRIT,MAVAMP NOT NEEDED WHEN SUBLEV=FALSE- ALL OPS COUNTED AS FULL
	SUB=OPEN.AND.(.NOT.FULL)
C	IF(SUB.AND.EXCSUB.AND.K.EQ.1) print 700,NBST+NB1,I,K1
	IF(SUB.AND.EXCSUB) GOTO 339	!NEXT INTERVAL IN THIS BST
	IF(OPEN) YVAL=YVAL+TINT(L)	!ACCUM OPEN PERIOD
c	IF(debug()) print 702,L,YVAL
c702	FORMAT( ' *L,YVAL= ',I8,G13.6)
	IF(OPEN.AND.NEXTOP)GOTO 339	!OPEN PERIOD NOT FINISHED YET
	IF(SHUT) YVAL=TINT(L)
	J=JSG+1		!INDEX OF CURRENT OP PERIOD OR GAP
c	IF(debug())print 701,L,J,JSG,YVAL
c701	FORMAT( ' L,J,JSG,YVAL= ',3I8,G13.6)
C   ASSIGN THIS OP PERIOD OR GAP TO APPROPRIATE ELEMENT OF AVS,SDS,NS.
C   EACH GET ALLOCATED TWICE- ONCE FOR  'ALL BURST' AND ONCE FOR
C   'BURSTS WITH K OPEN PERIODS'
	L1=2*J-1	!INDEX FOR 'ANY BSTS'
	IF(SHUT) L1=L1+1
	CALL ALLOC(AVS,SDS,NS,L1,YVAL)
C706	CONTINUE
C
c	print 703,L1,AVS(L1),SDS(L1),NS(L1)
703	FORMAT(' L1,AVS,SDS,NS= ',I8,2G13.6,I8)
C
C NOW ALLOCATE VALUES FOR 'BSTS WITH K OP PERIODS'
	L2=L1+2*M-1+(K-1)*(K-1)		!INDEX FOR BST WITH K OP PERIODS
	CALL ALLOC(AVS,SDS,NS,L2,YVAL)
C
c	print 704,L2,AVS(L2),SDS(L2),NS(L2)
704	FORMAT(' L2,AVS,SDS,NS= ',I8,2G13.6,I8)
C
C ALLOCATE VALUES FOR LENGTH OF OPEN PERIODS BEFORE AND AFTER
C AN INTERMEDIATE GAP (IF TC1 IS POS).
	IF(TC1.LT.0.001) GOTO 619
	IF(SHUT.AND.YVAL.GT.TC1) MGAP=J
C ALLOCATE LENGTH OF PRECEDING OPEN PERIOD WHICH IS IN YLAST
c	IF(debug())print 705,J,MGAP,YLAST,YVAL
c705	FORMAT(' J,MGAP,YLAST,YVAL= ',2I8,2G13.6)
	L1=NMEAN+1
	IF(J.EQ.MGAP) CALL ALLOC(AVS,SDS,NS,L1,YLAST)
c	IF(debug().AND.J.EQ.MGAP) print 703,L1,AVS(L1),SDS(L1),NS(L1)
C ALLOCATE LENGTH OF FOLLOWING OPEN PERIOD IN YVAL,ON NEXT CYCLE
	L2=L1+1
	IF(J.EQ.MGAP+1) CALL ALLOC(AVS,SDS,NS,L2,YVAL)
c	IF(debug().AND.J.EQ.MGAP+1) print 704,L2,AVS(L2),SDS(L2),NS(L2)
	IF(J.EQ.MGAP+1) MGAP=-1		!RESET SO NEXT GAP NOT INC
C TIDY UP BEFORE NEXT INTERVAL
619	IF(SHUT) JSG=JSG+1
	YLAST=YVAL
	YVAL=0.0	!ZERO READY FOR NEXT OPEN PERIOD
339	CONTINUE
C
C
C	IF(debug()) print 353,JSG,NSG
C353	FORMAT( ' JSG,NSG= ',2I8)
C  UPDATE NSG1 ETC BY ADDING VALUES FOR LAST BURST
338	NSG1=NSG1+NSG	!NSG1=TOTAL NO OF SHORT GAPS IN CURRENT EXPT
	NOP1=NOP1+NOP
	NAS1=NAS1+(NSG+1)
C
325	I=K1+1	!INDEX OF INTERVAL AFTER THAT WHICH ENDED LAST BST
	IF(I.LE.NINT) GOTO 360
C RECORDING OF VALUES FOR LAST BURST COMPLETED- GO TO 360 TO LOOK
C EXPLICITLY FOR FIRST OPENING OF NEXT BURST
C
C
C ALL BURSTS IDENTIFIED.
c99	NBST=NBST+NB1
c	NSGAP=NSGAP+NSG1
c	NOPEN=NOPEN+NOP1
c	NASET=NASET+NAS1
99	NBST=NB1
	NSGAP=NSG1
	NOPEN=NOP1
	NASET=NAS1
C
	print 3281
3281	FORMAT('&print burst parameters ? ')
	read 101,ans
	if(UC(ans).EQ.'N') GOTO 612
	if(pon()) write(7,328)NOPEN,NASET,NSGAP,NBST,
     & FLOAT(NOPEN)/FLOAT(NBST),FLOAT(NASET)/FLOAT(NBST),
     & FLOAT(NSGAP)/FLOAT(NBST)
      if(discprt) write(8,328)NOPEN,NASET,NSGAP,NBST,
     & FLOAT(NOPEN)/FLOAT(NBST),FLOAT(NASET)/FLOAT(NBST),
     & FLOAT(NSGAP)/FLOAT(NBST)
	print 328,NOPEN,NASET,NSGAP,NBST,FLOAT(NOPEN)/FLOAT(NBST),
     & FLOAT(NASET)/FLOAT(NBST),FLOAT(NSGAP)/FLOAT(NBST)
328	FORMAT( ' No of indiv openings= ',I8,/
     & ,' No of open periods= ',I8,/
     & ,' No of gaps below Tcrit= ',I8,/
     & ,' No of bursts= ',I8,/
     & ,' Mean no of indiv openings/burst= ',G13.6,/
     & ,' Mean no of open periods/burst= ',G13.6,/
     & ,' Mean no of gaps/burst= ',G13.6/)
C
C NOW FINISH CALC, AND THEN PRINT OUT MEANS,SDS,NS FOR ALL VARIABLES
612	CONTINUE
	NMEAN=M*M+2*M-1		!TOTAL NUMBER OF MEANS
C
	S1=0.		!USE TO CALC OVERALL MEAN OP AND GAP
	SS1=0.
	S2=0.
	SS2=0.
	N1=0
	N2=0
	DO 625 J=1,M
	L1=2*J-1
	L2=L1+1
	S1=S1+AVS(L1)
	SS1=SS1+SDS(L1)
	N1=N1+NS(L1)
	IF(J.EQ.M) GOTO 625
	S2=S2+AVS(L2)
	SS2=SS2+SDS(L2)
	N2=N2+NS(L2)
625	CONTINUE
	S1=S1/FLOAT(N1)
	SS1=SQRT(SS1/FLOAT(N1-1))	!SD NOT QUITE RIGHT
	S2=S2/FLOAT(N2)			!-SHOULD CALC WITH OVERALL MEAN!
	SS2=SQRT(SS2/FLOAT(N2-1))
C VALUE FOR 1ST OPENING IN ANY BURST WHEN BURST WITH ONE OPENING
C EXCLUDED (SUM OF LATTER IN AVS(2M))
	YVAL=AVS(1)-AVS(2*M)		!SUM
	L=NS(1)-NS(2*M)			!N
	YVAL=YVAL/FLOAT(L)		!MEAN
C
	DO 600 I=1,NMEAN
C CONVERT TO MEAN,SD
	IF(NS(I).LE.1) GOTO 601		!CHECK WHETHER N>1
	FI=FLOAT(NS(I))
	AVS(I)=AVS(I)/FI
	SDS(I)=SQRT(SDS(I)/(FI-1.0))
	GOTO 600
601	AVS(I)=0.		!IF N<2
	SDS(I)=0.
600	CONTINUE
C
C PRINT VALUES FOR 'ANY BURST'
	print 602
	if(pon()) write(7,602)
	if(discprt) write(8,602)
602	FORMAT(/,' Jth interval in any burst',/)
	print 6021
	if(pon()) write(7,6021)
	if(discprt) write(8,6021)
6021	FORMAT(T11,' Open period',T51,' Gaps',/,
     & '    J',T8,' Mean        SD         N  ',T44,
     & ' Mean        SD         N  ')
C
	DO 603 J=1,M-1	!ALL LINES BUT LAST
	L1=2*J-1
	L2=L1+1
	if(pon()) write(7,604)J,AVS(L1),SDS(L1),NS(L1),AVS(L2),
     & SDS(L2),NS(L2)
      if(discprt) write(8,604)J,AVS(L1),SDS(L1),NS(L1),AVS(L2),
     & SDS(L2),NS(L2)
	print 604,J,AVS(L1),SDS(L1),NS(L1),AVS(L2),SDS(L2),NS(L2)
	IF(J.EQ.1) print 626,J,YVAL,L
	IF(pon().and.J.EQ.1) write(7,626)J,YVAL,L
      if(discprt.and.j.eq.1) write(8,626)J,YVAL,L
626	FORMAT(2X,I3,G11.4,12X,I5,'  (Exc K=1)')
603	CONTINUE
C
	L1=2*M-1	!LAST LINE
	if(pon()) write(7,605)M,AVS(L1),SDS(L1),NS(L1)
      if(discprt) write(8,605)M,AVS(L1),SDS(L1),NS(L1)
	print 605,M,AVS(L1),SDS(L1),NS(L1)
604	FORMAT(2X,I3,2G11.4,1X,I5,8X,2G11.4,1X,I5)
605	FORMAT(2X,I3,2G11.4,1X,I5)
	print 6041,S1,SS1,N1,S2,SS2,N2
	if(pon()) write(7,6041)S1,SS1,N1,S2,SS2,N2
      if(discprt) write(8,6041)S1,SS1,N1,S2,SS2,N2
6041	FORMAT(/,' Mean',2G11.4,1X,I5,8X,2G11.4,1X,I5)
C
C print RESULTS FOR BURSTS WITH K OPEN PERIODS PER BURST
	print 606
	if(pon()) write(7,606)
	if(discprt) write(8,606)
606	FORMAT(/,/,' Jth interval in burst with K open periods',/)
	print 6021
	if(pon()) write(7,6021)
	if(discprt) write(8,6021)
C
	DO 607 K=1,M
	print 608,K
	if(pon()) write(7,608)K
      if(discprt) write(8,608)K
608	FORMAT(/,' K=',I2)
	DO 609 J=1,K
	L1=2*M+(K-1)**2+2*J-2
	L2=L1+1
	IF(J.EQ.K) GOTO 610	!FOR LAST LINE AT EACH K
	if(pon()) write(7,604)J,AVS(L1),SDS(L1),NS(L1),AVS(L2),
     & SDS(L2),NS(L2)
      if(discprt) write(8,604)J,AVS(L1),SDS(L1),NS(L1),AVS(L2),
     & SDS(L2),NS(L2)
	print 604,J,AVS(L1),SDS(L1),NS(L1),AVS(L2),SDS(L2),NS(L2)
	GOTO 609	!NEXT J
610	print 605,J,AVS(L1),SDS(L1),NS(L1)
	if(pon()) write(7,605)J,AVS(L1),SDS(L1),NS(L1)
      if(discprt) write(8,605)J,AVS(L1),SDS(L1),NS(L1)
609	CONTINUE	!NEXT J
607	CONTINUE	!NEXT K
C
C print MEAN LENGTH OF OPEN PERIOD BEFORE AND AFTER MED GAP
	IF(TC1.LT.0.001) GOTO 620	!SKIP
	print 621
	if(pon()) write(7,621)
	if(discprt) write(8,621)
621	FORMAT(/,/,' Mean open period before and after intermed gap',/)
	print 6022
	if(pon()) write(7,6022)
	if(discprt) write(8,6022)
6022	FORMAT(T11,'   Before',T51,' After',/,
     & T8,' Mean        SD         N  ',T44,
     & ' Mean        SD         N  ')
	L1=NMEAN+1
	L2=L1+1		!NB WOULD BE 81 IF M=8
	L=L2
	IF(L.GT.80) L=80
	IF(NS(L1).LT.2) GOTO 620
	FI=NS(L1)
	if(pon()) write(7,622)AVS(L1)/FI,SQRT(SDS(L1)/(FI-1.)),NS(L1),
     & AVS(L2)/FI,SQRT(SDS(L2)/(FI-1.)),NS(L)
      if(discprt) write(8,622)AVS(L1)/FI,SQRT(SDS(L1)/(FI-1.)),NS(L1),
     & AVS(L2)/FI,SQRT(SDS(L2)/(FI-1.)),NS(L)
	print 622,AVS(L1)/FI,SQRT(SDS(L1)/(FI-1.)),NS(L1),
     & AVS(L2)/FI,SQRT(SDS(L2)/(FI-1.)),NS(L)
622	FORMAT(6X,2G11.4,1X,I5,8X,2G11.4,1X,I5)
C
620	print 615
	if(pon()) print 615
615	FORMAT(/,/)
C
999	CONTINUE
	call flush(7)
	pause 'print <RETURN> to continue'
	RETURN
	END


	SUBROUTINE ALLOC(AVS,SDS,NS,L1,YVAL)
	dimension AVS(81),SDS(81)
	dimension NS(80)
C
C ACCUMULATES YVAL INTO AVS(L1),SDS(L1) AND INCREMENTS NS(L1)
C (NB. FOR M=8 NEED 79 FOR MEANS +2 FOR VALUES BEFORE AND AFTER INTERMED
C GAPS=81 ALTOGETHER. BUT HAVE ONLY 80 ELEMENTS IN NS (SO CAN MAKE
C AVS,SDS,NS, EQUIV TO FREQ,XAXIS IN MAIN PROG). IF L1=81 ASSUME THAT
C APPROPRIATE VALUE OF N IS IN NS(80),ALREADY UPDATED
C
	IF(L1.EQ.81) GOTO 1
	NS(L1)=NS(L1)+1		!UPDATE NUMBER
	N=NS(L1)
	GOTO 2
1	N=NS(80)
2	CONTINUE
C
	IF(N.GT.1) GOTO 540
	AVS(L1)=YVAL	!INITIALISE FOR SINGLE PASS METHOD WHEN N=1
	SDS(L1)=0.0
	GOTO 541
540	AVS(L1)=AVS(L1)+YVAL	!FOR N>1 ACCUMULATE SUM AND SSD
	FI=FLOAT(N)
	SDS(L1)=SDS(L1)+(FI*YVAL-AVS(L1))**2/(FI*(FI-1.0))
541	CONTINUE
	RETURN
	END



