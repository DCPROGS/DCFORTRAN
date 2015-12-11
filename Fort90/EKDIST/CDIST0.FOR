	subroutine CDIST0(TINT,NINT,ACAL,IAVAMP,MAVAMP,IACRIT,YVAL,NYVAL,
     & OHIST,BHIST,ID,GHIST,THIST,AHIST,OBHIST,trise,iscan,
     & SUBLEV,treso,tresg,tres,IDEST,idsav,xsav,tcfac,
     & latency,tint1,nlon,nloff)
c 05/28/89 01:16pm Lahey version: debug no longer an arg
c note: ID=IDTYPE
c Modif 12/06/91 05:38pm for 1st latency calcs (signalled by GHIST.and.id=11,12
c  for ON and OFF latencies resp)
C Version of CDIST1 for EKDIS3
c 11-73 version July 1986. All expts in enlarged TINT etc.
C **22-FEB-84. CALDIS SEPARATED INTO 2 SUBROUTINES IN SEP OVERLAYS
C FOR SPACE SAVING. CDIST1 FOR ALL DIST EXCEPT BURSTS AND CDIST2 FOR ALL
C BURST DISTRIBUTIONS
C SUBROUTINE FOR EKDIST TO GET Y VALUES AND COMPUTE HISTOGRAM
C FROM THEM
C
	real TINT(20480)
	real TINT1(10240,2)		!EQUIV with TINT (used for latencies)
	integer*2 iampl(20480)
	dimension YVAL(20480)		!=10*512
	dimension ylo(10),yhi(10)
	dimension sy0(10),syy0(10),ny0(10),sy1(10),syy1(10),ny1(10)
	LOGICAL OHIST,BHIST,GHIST,THIST,AHIST,OBHIST,SUBLEV
	LOGICAL CONAM1,OPEN,BAD,EXCSUB,gap,both
	logical pon,exass,exass1,debug,slock,caplock
	logical sbin,shist,sres,sexp,latency
	character*1 ans,UC
	logical discprt,prt,deb
	common/dp/discprt
	COMMON/AMPBLK/iampl
	common/sblk/sbin,shist,sres,sexp
	COMMON/OVLBLK/OBMAX,Jth,CONAM1,exass,exass1,AMPHI,AMPLO,
     & EXCSUB,Ith
C
	pon()=slock()
	debug()=caplock()
c
102	FORMAT(I8)
c108	FORMAT(/)
	if(shist) id=idsav
C NOW GET PARAMETERS FOR HISTOGRAMS
	DO 1081 I=1,20480
1081	YVAL(I)=0.0	!NEEDS TO BE INITIALISED FOR SOME BURST DISTS
C
101	FORMAT(A1)
C
	OHIST=.FALSE.
	BHIST=.FALSE.
	GHIST=.FALSE.
	AHIST=.FALSE.
	OBHIST=.FALSE.		!DIST=OPENINGS/BURST
	if(.not.sbin) NBIN=-1		!TO SIGNAL THAT NOT YET SET
	if(shist) goto 304	!Use prev CONAM?
	CONAM1=.FALSE.
	exass=.FALSE.
304	continue
	if(latency) then
	   GHIST=.TRUE.
	   tres=tresg
	   j1=idest		!=1 for ON, =2 for OFF
	   id=idest+10	!so GHIST.and.id=11 or id=12 implies ON/OFF latencies
	   n=nlon
	   if(idest.eq.2) n=nloff
	   j=0		!index for Yval
	   do 208 i=1,n
	   i1=i
C  -exclude if unusable
	   y=tint1(i,j1)
	   if(y.lt.0.) goto 208
	   j=j+1
	   yval(j)=y
208	   continue
	   nyval=j
	   goto 207	!Yval() now defined
	endif
c Now fix for all cases other than first latencies
	IF(IDEST.EQ.1) GOTO 202		!AMPLITUDES
	IF(IDEST.EQ.2) GOTO 201		!GAPS
	IF(IDEST.EQ.3) GOTO 200		!OPEN TIMES
C
C NOW DEFINE ARRAY YVAL CONTAINING VARIABLES FOR WHICH
C HISTOGRAM IS TO BE PLOTTED
C
C SECTION FOR DURATIONS OF OPENINGS
200	tres=treso	!in ms
	if(shist) goto 235
	goto 22
233	print 230
230	FORMAT( ' Type of distribution (1,2,3,4; 0=HELP)= ')
	read 102,ID
	IF(ID.GT.0) GOTO 235
22	print 231
231	FORMAT(' (1)Duration of individual apparent openings')
	print 232
232	FORMAT(' (2)Duration of contiguous open times (set A)')
	print 242
242	FORMAT(
     & ' (3)Duration of individual apparent openings that are adjacent',
     & /,'     to a gap with duration in a specified range')
	print 243
243	FORMAT(
     & ' (4)Mean durations of individual apparent openings that are',
     & /,'     adjacent to gaps with durations in a specified ranges,',
     & /,'     and correlation between open times and subsequent gap')
	GOTO 233
235	OHIST=.TRUE.
	idsav=id
c	print 108
c	if(pon()) write(7,108)
c	if(discprt) write(8,108)
c Modif May 87 so amp constraint can be used for option 2 also (see 2272)
c	IF(ID.EQ.2) GOTO 236
C
	if(shist) goto 2251	!^^?omit if conam etc to be asked for
	AMPLO=-10000.0	!DEFAULT IS TO
	AMPHI=10000.	!INCLUDE OPENINGS OF ANY AMPLITUDE
	CONAM1=.FALSE.
	exass=.FALSE.
	print 226
226	FORMAT(' Specify amplitude range for openings [N] ? ')
	read 101,ans
	if(UC(ans).EQ.'Y') CONAM1=.TRUE.
	if(iscan.ne.-1003) print 2261
2261	FORMAT('&Exclude openings with ''assumed'' amps? ')
	if(iscan.eq.-1003) print 2263
2263	FORMAT('&Exclude openings with ''dubious'' amps [Y] ? ')
	read 101,ans
	if(UC(ans).ne.'N') exass=.TRUE.
	IF(.NOT.CONAM1) GOTO 2251
2262	print 2271
2271	format(' Define amplitude range:')
	if(id.eq.2) print 2272
2272	format(
     & ' Must be in this range throughout a contiguous open period')
	print 227
227	FORMAT('  Low amp, high amp (real pA with sign)= ')
	call INPUT2r(amplo,amphi)
	if(amphi.gt.amplo) goto 2251
	a1=amphi	!swap
	amphi=amplo
	amplo=a1
2251	CONTINUE
	call PDUB(exass,shist,iscan,pon())
	if(exass.and.id.eq.2) print 2273
	if(exass.and.pon().and.id.eq.2) write(7,2273)
	if(exass.and.discprt.and.id.eq.2) write(8,2273)
2273	format(' (i.e. ANY dubious amp excludes whole open period)')
	if(conam1.and.pon()) write(7,25) amplo,amphi
	if(conam1.and.discprt) write(8,25) amplo,amphi
	if(conam1.and.shist) print 25,amplo,amphi
25	format(' Openings with amplitudes beween ',f7.3,' and ',
     & f7.3,' pA only')
	if(conam1.and.pon().and.id.eq.2) write(7,2272)
	if(conam1.and.discprt.and.id.eq.2) write(8,2272)
	print 1080
	if(pon()) write(7,1080)
	if(discprt) write(8,1080)
1080	format(' ------------------------------------------------')
	if(id.eq.1) goto 234
	IF(ID.EQ.2) GOTO 236
	if(id.eq.3) goto 251
	if(id.eq.4) goto 258
c
c Section for id=1
234	print 231		!TITLE
	if(pon()) write(7,231)
	if(discprt) write(8,231)
	J=0	!COUNTS YVAL
c In new version all expts already pooled
	DO 204 I=1,NINT
	i1=i
	iamp=IAVAL(i1)
c#	if(debug())call DEBTYP(0,i1,i1,tint,iampl,acal,
c#     & 0,-1,-1,yval,pon())
	IF(iamp.EQ.0.OR.TINT(I).LT.0.0) GOTO 204
C EXCLUDES SHUT OR UNUSABLE DURATIONS
	A1=ACAL*FLOAT(iamp)
C NOTE- THIS IS IN PA, WITH SIGN
C
C NEXT LINE EXCLUDES OPENINGS OF ASSUMED AMP (SET TO 9999.) WHEN
C exass IS TRUE
	IF(exass.AND.IABS(IAMPL(I)).GT.9000) GOTO 204
C IF CURRENT OPENING HAS NOT ALREADY BEEN EXCLUDED BY PREV LINE THEN
C NEXT TEST IF AMP IS BETWEEN SPEC LIMITS- EXCLUDE IF NOT
	IF(conam1.and.(A1.LT.AMPLO.OR.A1.GT.AMPHI)) GOTO 204
	J=J+1
	YVAL(J)=TINT(I)
c#	if(debug()) call DEBTYP(0,-1,-1,tint,iampl,acal,
c#     & 0,j,j,yval,pon())	!print YVAL(j)
204	CONTINUE	!END OF I LOOP
	NYVAL=J
	GOTO 207
c
C   NEXT SUBSECTION OF OHIST TO CALCULATE LENGTHS OF CONTIGUOUS OPEN
C   PERIODS
236	print 232		!title
	if(pon()) write(7,232)
	if(discprt) write(8,232)
	Ifst=1
	J=0   !COUNTS PERIODS IN A (=SUBSET OF OPEN STATES) FOR THIS EXPT
C NEXT LOOK FOR START OF A GROUP OF OPENINGS I.E.ANY OPENING THAT HAS
C DEFINED DURATION (I.E.USABLE). A SINGLE UNUSABLE OPENING IN A GROUP
C MAKES ITS LENGTH UNDEFINE SO IT IS EXCLUDED
247	iamp=IAVAL(Ifst)
	A1=ACAL*float(Iamp)
	open=iamp.ne.0
	if(conam1) open=(A1.ge.amplo).and.(A1.le.amphi)
C NEXT LINE EXCLUDES OPENINGS OF ASSUMED AMP (SET TO 9999.) WHEN
C exass IS TRUE
	IF(exass.and.iabs(iampl(ifst)).gt.9000) goto 2471
	IF(OPEN.AND.TINT(Ifst).GT.0.0) GOTO 248	!START OF GROUP
2471	Ifst=Ifst+1
	IF(Ifst.LE.NINT) GOTO 247	!START NOT YET FOUND-TRY NEXT INTERVAL
	GOTO 992			!finished all data
248	BAD=.FALSE.
	J=J+1	!COUNT GROUPS FOUND SO FAR- BUT MAY BE BAD
	YVAL(J)=TINT(Ifst)		!FIRST IN GRP (ALWAYS USABLE)
	K=Ifst+1    !INDEX IN TINT() OF INTERVAL FOLLOWING FIRST OF GROUP
c K loop starts at 250
250	iamp=IAVAL(k)
	A1=ACAL*float(Iamp)
	open=iamp.ne.0
	if(conam1) open=(A1.ge.amplo).and.(A1.le.amphi)
c Go to 249 when end of group found.
c (1) If no amplitude constraint group ends when ANY shut period found
c    (whether usable or unusable)
c (2) If amplitude constrained group ends when any amplitude outside
c    the specified range is found.
c Whole group is set bad, and so rejected, if (1) contains any unusable
c opening (2) when, if exass=true, it contains any opening with dubious
c amplitude OR if the interval that ends the group has 'dubious'amp.
	IF(exass.and.iabs(iampl(k)).gt.9000) bad=.true.
	IF(.not.open) GOTO 249		!end of group found
	YVAL(J)=YVAL(J)+TINT(K)	!ADD ANY SUBSEQUENT OPENINGS TO SAME YVAL
	IF(TINT(K).LT.0.0) BAD=.TRUE.	!THIS GROUP BAD,BUT FINISH ANYWAY
	K=K+1
	IF(K.LE.NINT) GOTO 250	!KEEP LOOKING FOR END
	J=J-1		!last bst bad
	GOTO 992	!DATA FINISHED BEFORE END FOUND, SO DO NOT COUNT
c Group completed
249	continue
c	if(.not.debug()) goto 50
cc print the group for debug
c	call DEBTYP(0,Ifst,k-1,tint,iampl,acal,
c     & 0,-1,-1,yval,pon())
cc print interval that follows group
c	call DEBTYP(0,k,k,tint,iampl,acal,
c     & 0,-1,-1,yval,pon())
cc if good, print yval
c	if(.not.bad) call DEBTYP(0,-1,-1,tint,iampl,acal,
c     & 0,j,j,yval,pon())	!print YVAL(j)
c50	continue
	Ifst=K+1	!ANOTHER GROUP COMPLETED (J UPDATED ABOVE)
	IF(BAD) J=J-1	!IF BAD DO NOT COUNT. YVAL(J) GETS OVERWRITTEN
	IF(Ifst.LT.NINT) GOTO 247	!LOOK FOR START OF NEXT GROUP
C N.B. DOES NOT RETURN IF Ifst=NINT AS IF TINT(NINT) IS START OF A
C GROUP THERE IS NO WAY TO TELL IF THIS GROUP HAS BEEN COMPLETED-
C THE NEXT INTERVAL MIGHT HAVE BEEN OPEN TOO
C  NOTE THAT NYVAL=NASET CALC HERE MAY NOT BE IDENTICAL WITH THAT
C FOUND IN CALC OF BURST DISTRIBUTIONS BELOW AS LATTER INCLUDES
C ONLY THOSE OPEN PERIODS THAT ARE PART OF VALID BURSTS (NO BAD
C OPENINGS AND PROPER END)
992	NYVAL=J
	GOTO 207
c
C  (id=3) NEXT SUBSECTION OF OHIST to calculate lengths of app openings
c adjacent to shut period in specified range (id=3)
251	continue
	print 2521
2521	format(' Low, high limits for gap length (ms)= ')
	call INPUT2r(gaplo,gaphi)
	if(gaphi.gt.gaplo) goto 2531
	a1=gaphi	!swap
	gaphi=gaplo
	gaplo=a1
2531	continue
	print 29
29	format(' Use both preceding AND following gaps [Y] ')
	read 101,ans
	both=.true.
	if(UC(ans).eq.'N') both=.false.
	print 257,gaplo,gaphi
	if(pon()) write(7,257) gaplo,gaphi
	if(discprt) write(8,257) gaplo,gaphi
257	FORMAT(
     & ' (3)Duration of individual apparent openings that are adjacent',
     & /,'     to a gap with duration between ',g13.6,' and ',g13.6,
     & ' ms.')
	if(.not.both) print 2572
	if(pon().and.(.not.both)) write(7,2572)
	if(discprt.and.(.not.both)) write(8,2572)
2572	format(' ONLY the gap following the opening used')
	J=0	!COUNTS YVAL
c
	DO 254 I=1,NINT
	i1=i
c#	if(debug())call DEBTYP(0,i1,i1,tint,iampl,acal,
c#     & 0,-1,-1,yval,pon())
	iamp=IAVAL(i1)
	IF(iamp.EQ.0.OR.TINT(I).LT.0.0) GOTO 254
C EXCLUDES SHUT OR UNUSABLE DURATIONS
	A1=ACAL*FLOAT(IAVAL(i1))
C NOTE- THIS IS IN PA, WITH SIGN
C
C NEXT LINE EXCLUDES OPENINGS OF ASSUMED AMP (SET TO 9999.) WHEN
C exass IS TRUE
	IF(exass.AND.IABS(IAMPL(I)).GT.9000) GOTO 254
C IF CURRENT OPENING HAS NOT ALREADY BEEN EXCLUDED BY PREV LINE THEN
C NEXT TEST IF AMP IS BETWEEN SPEC LIMITS- EXCLUDE IF NOT
	IF(conam1.and.(A1.LT.AMPLO.OR.A1.GT.AMPHI)) GOTO 254
c If still OK test whether the interval that precedes OR follows this
c opening is (a) shut and (b) within specified duration range
c   (1) preceding interval
	if(i1.eq.1) goto 255	!try following int
	if(.not.both) goto 255	!try following int
	iamp=IAVAL(i1-1)
	tin=tint(i1-1)
	if(iamp.ne.0.or.tin.lt.0.) goto 255	!prev int is open or unusable
	IF(exass.and.iabs(iampl(i1-1)).gt.9000) goto 255 !prev int is dubious
c	if(tin.ge.gaplo.and.tin.lt.gaphi) goto 256	!OK- in range
	if(tin.ge.gaplo.and.tin.lt.gaphi) goto 259	!OK- in range
	goto 255		!not in range- try following int
259	continue
	J=J+1
	YVAL(J)=TINT(I)
c#	if(debug()) call DEBTYP(0,-1,-1,tint,iampl,acal,
c#     & 0,j,j,yval,pon())	!print YVAL(j)
cc   (2) following interval (not tested if prev int was OK)
c   (2) following interval (test whether or not prev int was OK)
255	continue
	if(i1.eq.nint) goto 254		!finished
	iamp=IAVAL(i1+1)
	tin=tint(i1+1)
	if(iamp.ne.0.or.tin.lt.0.) goto 254	!next int is open or unusable
	IF(exass.and.iabs(iampl(i1+1)).gt.9000) goto 254 !next int is dubious
	if(tin.ge.gaplo.and.tin.lt.gaphi) goto 256	!OK -in range
	goto 254		!not in range- look for next good opening
c goes to 256 when valid value found
256	continue
	J=J+1
	YVAL(J)=TINT(I)
c#	if(debug()) call DEBTYP(0,-1,-1,tint,iampl,acal,
c#     & 0,j,j,yval,pon())	!print YVAL(j)
254	CONTINUE	!END OF I LOOP
	NYVAL=J
c
	GOTO 207
c
C  (id=4) NEXT SUBSECTION OF OHIST to calculate mean lengths of app openings
c adjacent to shut period in several specified ranges. Calculation is
c done both (a) counting every opening twice according to the gap
c that precedes it and that which follows it as Magleby- kept in sy0(),ny0()
c and (b) counting only the gap that FOLLOWS the opening. Opening is
c discarded if it is preceeded/followed by another opening.
c Distributions of these open times not done, but YVAL is defined to
c contain alternately the open, shut times and these are used for
c calculation of the open-shut correlation coefficient in CORCL3
258	continue
	yhi(10)=-1.	!to indicate not set yet
	print 420
420	format(
     & ' Number of gap duration ranges to be specified (up to 10)',/,
     & ' [default=6 ranges: -0.3, -3, -30, -100, -300, -3000] = ')
	read 102,nval
	if(nval.ge.1) goto 60
	nval=6
	ylo(1)=tresg
	yhi(1)=0.3
	ylo(2)=0.3
	yhi(2)=3.0
	ylo(3)=3.0
	yhi(3)=30.
	ylo(4)=30.
	yhi(4)=100.
	ylo(5)=100.
	yhi(5)=300.
	ylo(6)=300.
	yhi(6)=3000.
	yhi(10)=3000.	!so default used
60	print 2571
	if(pon()) write(7,2571)
	if(discprt) write(8,2571)
2571	FORMAT(
     & ' Mean durations of individual apparent openings that are',/,
     & '  adjacent to a gaps with durations in following ranges (ms)')
c
	do 423 i=1,nval
	sy0(i)=0.		!initialise
	syy0(i)=0.		!initialise
	ny0(i)=0		!initialise
	sy1(i)=0.		!initialise
	syy1(i)=0.		!initialise
	ny1(i)=0		!initialise
	if(yhi(10).gt.0.) goto 74	!use default
	print 422,i,i
422	format(1x,' (',i2,
     & ') Gap times (ms) for range',i2,': low time, high time= ')
	call INPUT2r(ylo(i),yhi(i))
	if(ylo(i).lt.yhi(i)) goto 74
	a1=ylo(i)
	ylo(i)=yhi(i)
	yhi(i)=a1
74	continue
	deb = debug()
	prt = pon()
c
c Main loop starts here
c
c	if(pon()) write(7,453) i,ylo(i),yhi(i)
c	if(discprt) write(8,453) i,ylo(i),yhi(i)
453	format(i4,3x,g13.6,' to ',g13.6)
423	continue
c
	J=0	!COUNTS YVAL
c	j1=0
	DO 252 I=1,NINT
	i1=i
c#	if(debug())call DEBTYP(0,i1,i1,tint,iampl,acal,
c#     & 0,-1,-1,yval,pon())
	iamp=IAVAL(i1)
	IF(iamp.EQ.0.OR.TINT(I).LT.0.0) GOTO 252
C EXCLUDES SHUT OR UNUSABLE DURATIONS
	A1=ACAL*FLOAT(IAVAL(i1))	!amp of the opening (pA with sign)
C NEXT LINE EXCLUDES OPENINGS OF ASSUMED AMP (SET TO 9999.) WHEN
C exass IS TRUE
	IF(exass.AND.IABS(IAMPL(I)).GT.9000) GOTO 252
C IF CURRENT OPENING HAS NOT ALREADY BEEN EXCLUDED BY PREV LINE THEN
C NEXT TEST IF AMP IS BETWEEN SPEC LIMITS- EXCLUDE IF NOT
	IF(conam1.and.(A1.LT.AMPLO.OR.A1.GT.AMPHI)) GOTO 252
c Opening OK so test whether the interval that precedes OR follows this
c opening is (a) shut and (b) within any of the specified duration ranges
c   (1) preceding interval
	if(i1.eq.1) goto 253	!try following int
	iamp=IAVAL(i1-1)
	tin=tint(i1-1)
	if(iamp.ne.0.or.tin.lt.0.) goto 253	!prev int is open or unusable
	IF(exass.and.iabs(iampl(i1-1)).gt.9000) goto 253 !prev int is dubious
c
c  First loop
c
	do 424 k=1,nval
	if(tin.ge.ylo(k).and.tin.lt.yhi(k)) goto 2561	!OK- in range
	goto 424		!not in range- try next range
2561	sy0(k)=sy0(k)+tint(i)		!add open times
	syy0(k)=syy0(k)+tint(i)**2	! for SD
	ny0(k)=ny0(k)+1			!increment number
c	if(k.eq.1) j1=j1+1
c	if(k.eq.1) tstor(j1)=tint(i)
	if(deb) print 229,tint(i),k,ny0(k)
	if(prt) write(7,229) tint(i),k,ny0(k)
c*	if(discprt) write(8,229) tint(i),k,ny0(k)
229	format(1x,g13.6,' ms counted for range # ',i3,' : N= ',i8)
424	continue
c   (2) following interval (tested whether or not prev int was OK)
253	if(i1.eq.nint) goto 252		!finished
	iamp=IAVAL(i1+1)
	tin=tint(i1+1)
	if(iamp.ne.0.or.tin.lt.0.) goto 252	!next int is open or unusable
	IF(exass.and.iabs(iampl(i1+1)).gt.9000) goto 252 !next int is dubious
c
c Second loop
c
	do 425 k=1,nval
	if(tin.ge.ylo(k).and.tin.lt.yhi(k)) goto 2562	!OK- in range
	goto 425			!not in range- try next range
2562	sy0(k)=sy0(k)+tint(i)		!add open times
	syy0(k)=syy0(k)+tint(i)**2	! for SD
	ny0(k)=ny0(k)+1			!increment number
	sy1(k)=sy1(k)+tint(i)		!ditto for 'following gap' only
	syy1(k)=syy1(k)+tint(i)**2	! for SD
	ny1(k)=ny1(k)+1			!increment number
c	if(k.eq.1) j1=j1+1
c	if(k.eq.1) tstor(j1)=tint(i)
	if(.not.deb) goto 425
	print 2291,tint(i),k,ny0(k),ny1(k)
	if(prt) write(7,2291) tint(i),k,ny0(k),ny1(k)
	if(discprt) write(8,2291) tint(i),k,ny0(k),ny1(k)
2291	format(1x,g13.6,' ms counted for range # ',i3,' : N0,N1= ',2i8)
425	continue
c
c Must now define Yval() with valid open and shut times alternating
c Tint(i) is a valid open time and the shut time that follows it, TINT(i+1)
c will also be valid if this point reached (if it was unusable or open
c would have skipped to end of loop (252) above
	j=j+1
	Yval(j)=Tint(i)
	if(i+1.gt.nint) goto 252
	j=j+1
	Yval(j)=Tint(i+1)
c#	if(debug()) call DEBTYP(0,-1,-1,tint,iampl,acal,
c#     & 0,j-1,j,yval,pon())	!print last two YVAL(j)
c
252	CONTINUE	!END OF i=1,...,Nint LOOP
	NYVAL=J
c	open(UNIT=18,NAME='DL1:SCRTCH.DAT',TYPE='OLD',
c    & ACCESS='DIRECT',FORM='UNFORMATTED',RECORDSIZE=1024,
c    & MAXREC=2,ASSOCIATEVARIABLE=KS)
c	WRITE(18'2) j1,(tstor(j),j=1,1023)
c	CLOSE(unit=18)
c Print means and then go to CORCL3 to get open-shut correlation from Yval
	print 428
	if(prt) write(7,428)
	if(discprt) write(8,428)
428	format(8x,
     & ' Gap length range (ms)',8x,' # of ops   mean open time',
     & '   Std dev')
	do 429 j=1,2		!for 'both' and 'following gap'
	do 427 i=1,nval
	if(j.eq.2) goto 430
	n=ny0(i)
	if(n.le.1) goto 4271
	ym=sy0(i)/float(n)
	sd=sqrt((syy0(i)-(sy0(i)*sy0(i)/float(n)))/float(n-1))
	goto 431
c  for 'following gap only' (j=2):
430	if(i.eq.1) print 432
	if(i.eq.1.and. pon()) write(7,432)
	if(i.eq.1.and.discprt) write(8,432)
432	format(/,' For following gap only:')
	n=ny1(i)
	if(n.le.1) goto 4271
	ym=sy1(i)/float(n)
	sd=sqrt((syy1(i)-(sy1(i)*sy1(i)/float(n)))/float(n-1))
431	print 426,i,ylo(i),yhi(i),n,ym,sd
	if(pon()) write(7,426) i,ylo(i),yhi(i),n,ym,sd
	if(discprt) write(8,426) i,ylo(i),yhi(i),n,ym,sd
426	format(i4,3x,g13.6,' to ',g13.6,5x,i4,2(3x,g13.6))
	goto 427
4271	print 4261,i,ylo(i),yhi(i),n
	if(pon()) write(7,4261) i,ylo(i),yhi(i),n
	if(discprt) write(8,4261) i,ylo(i),yhi(i),n
4261	format(i4,3x,g13.6,' to ',g13.6,5x,i4)
427	continue
429	continue
	GOTO 207
c
c
c NEXT SECTION FOR DURATIONS OF SHUT PERIODS
C
201	continue
	GHIST=.TRUE.
	tres=tresg
	if(shist) goto 309
	goto 310
314	print 315
315	FORMAT( ' Type of distribution (1,2,3; 0=HELP)= ')
	read 102,ID
	IF(ID.GT.0) GOTO 309
310	print 311
311	FORMAT( ' (1)Duration of all shut times')
	print 312
312	FORMAT(
     & ' (2)Duration of all shut times that are bordered on each side',/,
     & '    by openings IN a specified amplitude range')
	print 313
313	FORMAT(
     & ' (3)Duration of all sojourns at amplitudes that are NOT in',/,
     & '    in a specified amplitude range')
	GOTO 314
309	idsav=id
c	print 108
c	if(pon()) write(7,108)
c	if(discprt) write(8,108)
	if(id.eq.1) then
	   print 301
301	   format(
     &' Exclude gaps bordered by ''dubious'' amplitude openings [Y] ? ')
	   read 101,ans
	   exass=UC(ans).ne.'N'
	   if(exass.and.pon()) write(7,302)
	   if(exass.and.discprt) write(8,302)
302	   format(
     &' Gaps that are bordered by openings with ''dubious'' amplitudes',
     &' are excluded')
	   goto 321
	endif
c
	print 2271
c2271	format(' Define amplitude range:')
	print 227
c227	FORMAT('  Low amp, high amp (real pA with sign)= ')
	call INPUT2r(amplo,amphi)
	if(amphi.gt.amplo) goto 452
	a1=amphi	!swap
	amphi=amplo
	amplo=a1
452	if(pon()) write(7,4531) amplo,amphi
	if(discprt) write(8,4531) amplo,amphi
4531	format(' Amplitude range (pA)= ',g13.6,' to ',g13.6)
	tc=0.
	if(id.eq.3) tc=-1.	!see setdub
	call setdub(iscan,exass,shist,pon(),tc,trise,tcfac)
	if(exass.and.id.eq.3) print 2273
	if(exass.and.pon().and.id.eq.3) write(7,2274)
	if(exass.and.discprt.and.id.eq.3) write(8,2274)
2274	format(' (i.e. ANY dubious amp excludes whole ''gap'' period)')
c
	print 1080
	if(pon()) write(7,1080)
	if(discprt) write(8,1080)
	goto (321,322,323) ID
c All gaps:
321	print 311		!title
	IF(pon()) write(7,311)
	if(discprt) write(8,311)
	J=0	!TO COUNT GOOD GAPS
	DO 205 I=1,NINT
	i1=i
C  -exclude if not shut, or if unusable
	IF(IAVAL(i1).NE.0.OR.TINT(I).LT.0.) GOTO 205
	if(exass) then
	   i0=i1-1
	   i2=i1+1
	   if(i0.gt.0.and.iaval(i0).ne.0) then
		if(iabs(iampl(i0)).gt.9000) goto 205
	   endif
	   if(i2.le.nint.and.iaval(i0).ne.0) then
		if(iabs(iampl(i2)).gt.9000) goto 205
	   endif
	endif
	J=J+1
	YVAL(J)=TINT(I)
205	CONTINUE	!END OF I LOOP
	NYVAL=J		!NO OF USABLE GAPS
	goto 207	!Yval() now defined
c
c Option 2 (specified amp both sides of gap)
322	print 312	!title
	IF(pon()) write(7,312)
	if(discprt) write(8,312)
	J=0	!TO COUNT GOOD GAPS
	DO 317 i=2,NINT-1		!so i-1,i+1 always defined
	i1=i
c#	if(debug()) call DEBTYP(0,i1-1,i1+1,tint,iampl,acal,
c#     & 0,-1,-1,yval,pon())
c	subroutine DEBTYP(nval,n1,n2,tint,iampl,acal,jval,j1,j2,yval,prnt)
c To print values of intervals and amps for debugging in EKDIST
c Nval,jval used only to indicate no of values when i1,i2 asked for
c  here (n1=0 or j1=0), otherwise ignored.
c (1)If n1>0 in call then TINT,AMP values from n1 to n2 are printed
c without asking for values here (and without title)
c (2) If n1=0 in call then asks which elements of TINT, AMP to type
c (3) If n1<0 in call then do only Jval part
c (4) Print of Yval() similarly controlled by Jval,j1,j2
c
C  -exclude if not shut, or if gap unusable
	IF(IAVAL(i1).NE.0.OR.TINT(I).LT.0.) GOTO 317
c now TINT(i) is a usable gap
c  -also exclude if either preceding or following opening is
c not in specified amp range (or is neg/unusable)
	i2=i-1		!preceding opening
319	if(i2.lt.1.or.i2.gt.nint) goto 317
	if(tint(i2).lt.0.) goto 317	!exclude neg/unusable ops always
	IF(exass.and.IABS(IAMPL(i2)).GT.9000) GOTO 317 !EXC ASSUMED AMPS
	if((tc.gt.0.).and.tint(i2).lt.tc) goto 317  !exclude short ops
	A1=ACAL*FLOAT(IAVAL(i2))	!amp (pA), with sign
	IF((A1.LT.AMPLO).OR.(A1.GT.AMPHI)) GOTO 317	!exc if not in range
c repeat above for opening AFTER the gap
	if(i2.eq.i1+1) goto 318
	i2=i1+1		!following opening
	goto 319
c
318	J=J+1
	YVAL(J)=tint(i1)	!valid gap found
c#	if(debug()) call DEBTYP(0,-1,-1,tint,iampl,acal,
c#     & 0,j,j,yval,pon())	!print YVAL(j)
317	CONTINUE	!END OF I LOOP
	NYVAL=J		!NO OF USABLE GAPS
	goto 207	!Yval() now defined for id=2
c
c Option 3 (all intervals NOT in range)
323	print 313		!title
	IF(pon()) write(7,313)
	if(discprt) write(8,313)
	Ifst=1
	J=0   !counts periods NOT in specified range
C NEXT LOOK FOR START OF A GROUP OF gaps I.E.ANY usable interval that is
c not in range. A SINGLE UNUSABLE interval IN A GROUP
C MAKES ITS LENGTH UNDEFINE SO IT IS EXCLUDED
347	iamp=IAVAL(Ifst)
	A1=ACAL*float(Iamp)
	gap=(A1.lt.amplo).or.(A1.gt.amphi)
C NEXT LINE EXCLUDES intervals with ASSUMED AMP (SET TO 9999.) WHEN
C exass IS TRUE
	IF(exass.and.iabs(iampl(ifst)).gt.9000) goto 3471	!invalid start
	IF(gap.AND.TINT(Ifst).GT.0.0) GOTO 348	!START OF GROUP
3471	Ifst=Ifst+1
	IF(Ifst.LE.NINT) GOTO 347	!START NOT YET FOUND-TRY NEXT INTERVAL
	GOTO 993		!finished all data
348	BAD=.FALSE.
	J=J+1	!COUNT GROUPS FOUND SO FAR- BUT MAY BE BAD
	YVAL(J)=TINT(Ifst)		!FIRST IN GRP (ALWAYS USABLE)
	K=Ifst+1    !INDEX IN TINT() OF INTERVAL FOLLOWING FIRST OF GROUP
c K loop starts at 350
350	iamp=IAVAL(k)
	A1=ACAL*float(Iamp)
	gap=(A1.lt.amplo).or.(A1.gt.amphi)
c Go to 349 when end of group found -ie when ANY period with amplitude
c IN specified range found
c Whole group is set bad, and so rejected, if (1) contains any unusable
c interval (2) when, if exass=true, it contains any interval with dubious
c amplitude OR if the interval that ends the group has 'dubious' amp.
	IF(exass.and.iabs(iampl(k)).gt.9000) bad=.true.
	IF(.not.gap) GOTO 349		!end of group found
	YVAL(J)=YVAL(J)+TINT(K)	!ADD ANY SUBSEQUENT OPENINGS TO SAME YVAL
	IF(TINT(K).LT.0.0) BAD=.TRUE.	!THIS GROUP BAD,BUT FINISH ANYWAY
	K=K+1
	IF(K.LE.NINT) GOTO 350	!KEEP LOOKING FOR END
	J=J-1		!last bst bad
	GOTO 993	!DATA FINISHED BEFORE END FOUND, SO DO NOT COUNT
c Group completed
349	if(.not.debug()) goto 53
c print the group for debug
	call DEBTYP(0,Ifst,k-1,tint,iampl,acal,
     & 0,-1,-1,yval,pon())
c print interval that follows group
	call DEBTYP(0,k,k,tint,iampl,acal,
     & 0,-1,-1,yval,pon())
c if good, print yval
	if(.not.bad) call DEBTYP(0,-1,-1,tint,iampl,acal,
     & 0,j,j,yval,pon())	!print YVAL(j)
53	Ifst=K+1	!ANOTHER GROUP COMPLETED (J UPDATED ABOVE)
	IF(BAD) J=J-1	!IF BAD DO NOT COUNT. YVAL(J) GETS OVERWRITTEN
	IF(Ifst.LT.NINT) GOTO 347	!LOOK FOR START OF NEXT GROUP
993	NYVAL=J
	GOTO 207
c
C
C NEXT SECTION FOR DISTRIBUTION OF AMPLITUDES
C ALSO OPTION TO LIST ALL GROUPS OF OPENINGS THAT CONTAIN SUBLEVELS
C (NO DISTRIBUTION FITTED)
202	AHIST=.TRUE.
	id=idsav
	if(shist) GOTO 2021
c
2301	continue
	id=2		!default here
	print 2311
2311	FORMAT( ' (1)Amplitude distribution with signs')
	print 2321
2321	FORMAT( ' (2)Amplitude distribution with reversed sign')
	print 2023
2023	FORMAT(' Type of distribution (1,2) [2] = ')
	call INPUTi(id)
	if(id.lt.1.or.id.gt.2) goto 2301
2352	idsav=id
2021	print 1080
	if(pon()) write(7,1080)
	if(discprt) write(8,1080)
	IF(ID.EQ.1) print 2311
	IF(ID.EQ.2) print 2321
	IF(ID.EQ.1.and.pon()) write(7,2311)
	IF(ID.EQ.1.and.discprt) write(8,2311)
	IF(ID.EQ.2.and.pon()) write(7,2321)
	IF(ID.EQ.2.and.discprt) write(8,2321)
	tc=0.		!see setdub
	call setdub(iscan,exass,shist,pon(),tc,trise,tcfac)
C
	J=0
	DO 206 I=1,NINT
	i1=i
	if(tint(i1).lt.0.) goto 206	!exclude neg/unusable always
	IF(exass.and.IABS(IAMPL(I)).GT.9000) GOTO 206 !EXC ASSUMED AMPS
	IF(IAVAL(i1).EQ.0) GOTO 206	!EXCLUDE ZERO AMPS
	if((tc.gt.0.).and.tint(i1).lt.tc) goto 206  !exclude short ones
	J=J+1
	YVAL(J)=ACAL*FLOAT(IAVAL(i1))
C NOTE- INCLUDES SIGN
	IF(ID.EQ.2) YVAL(J)=-YVAL(J)
206	CONTINUE
	NYVAL=J
	GOTO 207
C
C
207	CONTINUE
	RETURN		!Rest done in SETBIN
	END


	subroutine SETDUB(iscan,exass,shist,prnt,tc,trise,tcfac)
	logical exass,shist,prnt
	character*1 ans,UC
	logical discprt
	common/dp/discprt
c If called with tc=-1. then does only first part (does not ask
c whether to exclude short ones)
101	FORMAT(A1)
108	FORMAT(/)
2021	if(iscan.ne.-1003) print 44
44	format(' Exclude ''assumed'' amplitudes? ')
	if(iscan.eq.-1003) print 441
441	format(' Exclude ''dubious'' amplitudes [Y] ? ')
	read 101,ans
	exass=UC(ans).ne.'N'
	call pdub(exass,shist,iscan,prnt)
	if(tc.lt.-0.5) RETURN
c
	print 40
40	format('&Exclude amplitudes of short openings [Y] ? ')
	read 101,ans
	tc=-1.		!do not exclude if neg
	if(UC(ans).eq.'N') goto 41
	call RISTIM(trise,-1.,prnt)
232	print 42,tcfac
42	format(
     & ' -exclude events shorter than f risetimes (0 to calc more)',
     & ' [',f5.1,']  f = ')
	call INPUTr(tcfac)
	if(tcfac.eq.0.) then
	   print 231
231	   format(' length/trise= ')
	   call INPUTr(tc)
	   call RISTIM(trise,tcfac,prnt)
	   goto 232
	else
	   tc=tcfac*trise*1.e-3	!ms
	endif
c
41	continue
	if(prnt.and.tc.gt.0.) write(7,43) tc,tc*1.e3/trise
	if(discprt.and.tc.gt.0.) write(8,43) tc,tc*1.e3/trise
43	format(
     & ' Amplitudes of openings shorter than ',f8.4,' ms(',f6.1,
     & ' rise times) excluded')
	return
	end		!of SETDUB



