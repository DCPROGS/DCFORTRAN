	SUBROUTINE CDIST3(TINT,IAMPL,NINT,ACAL,MAVAMP,IACRIT,
     & YVAL,NYVAL,iscan,TCRIT,TC,NBST,OBBAR,OHIST,BHIST,ID,
     & GHIST,AHIST,OBHIST,iexc,SUBLEV,ISG,treso,tresg,tres,
     & idsav,ibamp,ibtype)
C
c 08/25/92 12:34pm Lahey V5.x version
c 06/06/89 11:35am Lahey version
c
c CDIST3 is version of CDIST2 that allows 3 different definitions of a burst
c**CDIST2 modif April 87 so that 'amplitude' of bursts is defined and can
c (a) look at distribution of such amplitudes, and (b) limit other
c distributions to bursts with amplitudes in specified range.
c	11-73 version  July 1986. All expts in enlarged TINT etc
C**22-FEB-84. CALDIS SEP INTO CDIST1 (ALL DISTS BUT BURST) AND CDIST2
C FOR BURST DISTNS
C SUBROUTINE FOR OVERLAY IN EKDIST TO GET Y VALUES AND COMPUTE HISTOGRAM
C FROM THEM
C
	dimension TINT(20480)
	integer*2 iampl(20480)
	integer*2 ibampl(10240)		!for burst amps
	dimension YVAL(20480)
	dimension ops(501),gaps(500)	!temp store for 1 burst
	dimension isubs(500)
	character*1 ans,UC
	LOGICAL OHIST,BHIST,GHIST,AHIST,OBHIST,SUBLEV,BPRINT
	LOGICAL CONAM1,exass,DEBUG,OPEN,shut,SUB,FULL,BAD,BAD1,EXCSUB
	logical exass1,ppopen
	logical sbin,shist,sres,sexp,assume,pon,prt
	logical discprt,slock,caplock
	common/dp/discprt
	common/sblk/sbin,shist,sres,sexp
	COMMON/OVLBLK/OBMAX,JTH,CONAM1,exass,exass1,AMPHI,AMPLO,
     & EXCSUB,Ith
C
	pon()=slock()
	debug()=caplock()
c
108	FORMAT(/)
101	format(a1)
102	format(i8)
C NOW GET PARAMETERS FOR HISTOGRAMS
	DO 1081 I=1,20480
1081	YVAL(I)=0.0	!NEEDS TO BE INITIALISED FOR SOME BURST DISTS
	if(shist) id=idsav
C
	OHIST=.FALSE.
	BHIST=.FALSE.
	GHIST=.FALSE.
	AHIST=.FALSE.
	OBHIST=.FALSE.		!DIST=OPENINGS/BURST
	ppopen=.false.
	if(shist) goto 303	!same histo print (and tcrit??)
C
C NOW DEFINE ARRAY YVAL CONTAINING VARIABLES FOR WHICH
C HISTOGRAM IS TO BE PLOTTED
	if(tcrit.lt.0.) then
	   print 321
321	   format('&Critical gap length (ms)= ')
	   call INPUTr(tcrit)
	else
	   print 320,tcrit
320	   format('&Critical gap length [',f7.2,'ms] = ')
	   call INPUTr(x)
	   if(x.gt.0.0001) tcrit=x	!else prev tcrit used
	endif
c Can calc this in RESINT which calls TCSUB
	print 417
417	format(/,' Options for definition of bursts:')
	print 4171
4171	format(
     & ' (1)Bursts to include ANY open state separated by shut periods',
     & /,'    that are all shorter than tcrit (normal definition).')
	print 4172
4172	format(
     & ' (2)Bursts consist of any series of openings to a specified',/,
     & '    amplitude (range), these openings being separated by',/,
     & '    periods in ANY other level (open or shut) that is',/,
     & '    (a) not in the specified amplitude range, and',/,
     & '    (b) has total duration < tcrit',/,
     & '    (''50pS/non-50pS'' print definition in which all amps',/,
     & '    outside range are treated as ''shut'').')
	print 4173
4173	format(
     & ' (3)Bursts consist of alternations between open (in specified',/,
     & '    amplitude range) and SHUT (< tcrit))')
4174	print 4175
4175	format(' Option number [1] (-1 for more details)= ' )
	read 102,ibtype
	if(ibtype.eq.0) ibtype=1
	if(ibtype.ge.1.and.ibtype.le.3) goto 303
	print 4181
4181	format(
     & ' (1)Bursts to include ANY open state separated by shut periods',/,
     & '    that are all shorter than tcrit: ended by gap that is',/,
     & '    longer than tcrit (or by unusable shut period).',/,
     & '    (normal definition).')
	print 4182
4182	format(
     & ' (2)Bursts consist of any series of openings to a specified',/,
     & '    amplitude (range), these openings being separated by',/,
     & '    periods in ANY other level (open or shut) that is',/,
     & '    (a) not in the specified amplitude range, and',/,
     & '    (b) has total duration < tcrit',/,
     & '     Burst ended by any sojourn longer than tcrit outside the',/,
     & '    specified range (or by unusable gap). This is the',/,
     & '    ''50pS/non-50pS'' print definition in which all amps NOT in',
     & /,'    range are treated as ''shut''. Looks at transitions',/,
     & '    from specified amp to all other states, and back.')
	print 4183
4183	format(
     & ' (3)Bursts consist of alternations open (in specified range)',/,
     & '    and SHUT (< tcrit). Burst ends when',/,
     & '    (a) shut period > tcrit (or unusable) found, OR',/,
     & '    (b) transition from specifed open level to ANY other open',/,
     & '        level (whatever its duration) is found, OR',/,
     & '    (c) transition to shut period of ANY duration is found',/,
     & '        if the shut period is followed by transition to an',/,
     & '        opening outside the specified range.',/,
     & '    Looks at transitions from specified amplitude to brief',/,
     & '    shut states, and back.')
	goto 4174
C
303	N1=-1	!DEFAULTS
	N2=-1
	TC=0.
	kplast=0
C	IF(.NOT.DEBUG) GOTO 3212
	print 3211
3211	FORMAT( ' List some burst compositions [N] ? ')
	read 101,ans
	if(UC(ans).ne.'Y') GOTO 3212
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
116	format(/,' Option number= ')
	read 102,J
	if(j.lt.1.or.j.gt.4) GOTO 3215
	if(pon()) goto(1121,1131,1141,1151) J
	goto 3216
1121	print 112
	goto 3216
1131	print 113
	goto 3216
1141	print 114
	goto 3216
1151	print 115
3216	IF(J.EQ.1) TCRIT=-TCRIT
	IF(J.NE.2) GOTO 3217
	print 3218
3218	FORMAT('  i1,i2= ')
	read 3422,N1,N2
3422	format(2i8)
	if(pon()) write(7,111)n1,n2
      if(discprt) write(8,111)n1,n2
111	format(  ' i1,i2= ',2i8)
	GOTO 3212
3217	IF(J.NE.3.AND.J.NE.4) GOTO 3212
	print 3219
3219	FORMAT( '  Specified value (ms)= ')
	call INPUTr(tc)
	if(pon()) write(7,60)tc
      if(discprt) write(8,60)tc
60	format(  ' Specified value (ms)= ',g13.6)
	IF(J.EQ.4) TC=-TC
C
3212	NSGAP=0		!NO OF GAPS .LE. TCRIT
	NOPEN=0		!TOTAL NO OF INDIV OPENINGS (=NO OF A PERIODS-1)
	NBST=0		!NO OF BURSTS
C
C	NOW SELECT TYPE OF BURST DISTRIBUTION WANTED
C NOTE THAT IF THERE ARE NO SUBLEVELS 1 AND 2 SHOULD BE THE SAME
C (COULD ALSO CALC NO OF FULL OPENINGS/BST, AND NO OF SUBLEVELS/BST)
C ALSO 3,4 SAME AND 5,6 SAME
	if(shist) goto 304
416	continue
	print 500
500	format(/,' Options for distribution type:')
	print 501
501	FORMAT( ' (1) No of individual openings per burst')
	print 502
502	FORMAT(
     & ' (2) No of open periods per burst (=1+short gaps/bst)')
	print 503
503	FORMAT( ' (3) Total burst length')
	if(ibtype.eq.1) print 504
c504	FORMAT( ' (4) Total burst length (exc time in sublevels)')
504	FORMAT(
     & ' (4) Total burst length (exc time outside spec range)')
	print 505
505	FORMAT( ' (5) Total open time per burst')
	if(ibtype.eq.1) print 506
506	FORMAT(
     & ' (6) Total open time per burst (exc time outside spec range)')
c     &  ' (6) Total open time per burst (exc time in sublevels)',/)
	print 507
507	FORMAT(
     & ' (7) Burst length for bursts with K apparent openings')
	print 508
508	FORMAT(
     & ' (8) Open time/bst for bursts with K apparent openings')
	print 509
509	FORMAT( ' (9) Length of gaps within bursts')
	print 510
510	FORMAT( ' (10) Length of openings within a burst')
	print 511
511	FORMAT( ' (11) Correlation of Ith and Jth opening in burst')
	if(ibtype.eq.1) print 512
512	FORMAT(' (12) Distribution of ''mean amplitudes'' of bursts')
	if(ibtype.eq.1) print 513
513	FORMAT(' (13) Distribution of ''mean amplitudes'' of bursts',/,
     & '       with signs reversed')
	print 514
514	FORMAT(' (14) Distribution of P(open) for bursts')
c NB ID=7,-7,8,9,10 are now ID=7,8,9,10,11
	print 116	!option no
	read 102,ID
	if(id.lt.1.or.id.gt.14) GOTO 416
	if(ibtype.ne.1.and.(id.eq.4.or.id.eq.6.or.id.eq.12.or.
     & id.eq.13)) goto 416	!invalid options
	idsav=id
c304	if(.not.pon()) goto 117
304	continue
	if(ibtype.eq.1) then
		print 4171
		if(pon()) write(7,4171)
		if(discprt) write(8,4171)
	else if(ibtype.eq.2) then
		print 4172
		if(pon()) write(7,4172)
		if(discprt) write(8,4172)
	else if(ibtype.eq.3) then
		print 4173
		if(pon()) write(7,4173)
		if(discprt) write(8,4173)
	endif
	print 1080
	if(pon()) write(7,1080)
	if(discprt) write(8,1080)
1080	format(
     & ' -------------------------------------------------------------')
c
	if(ID.eq.1) then
		print 501
		if(pon()) write(7,501)
		if(discprt) write(8,501)
	else if(id.eq.2) then
		print 502
		if(pon()) write(7,502)
		if(discprt) write(8,502)
	else if(id.eq.3) then
		print 503
		if(pon()) write(7,503)
		if(discprt) write(8,503)
	else if(id.eq.4) then
		print 504
		if(pon()) write(7,504)
		if(discprt) write(8,504)
	else if(id.eq.5) then
		print 505
		if(pon()) write(7,505)
		if(discprt) write(8,505)
	else if(id.eq.6) then
		print 506
		if(pon()) write(7,506)
		if(discprt) write(8,506)
	else if(id.eq.7) then
		print 507
		if(pon()) write(7,507)
		if(discprt) write(8,507)
	else if(id.eq.8) then
		print 508
		if(pon()) write(7,508)
		if(discprt) write(8,508)
	else if(id.eq.9) then
		print 509
		if(pon()) write(7,509)
		if(discprt) write(8,509)
	else if(id.eq.10) then
		print 510
		if(pon()) write(7,510)
		if(discprt) write(8,510)
	else if(id.eq.11) then
		print 511
		if(pon()) write(7,511)
		if(discprt) write(8,511)
	else if(id.eq.12) then
		print 512
		if(pon()) write(7,512)
		if(discprt) write(8,512)
	else if(id.eq.13) then
		print 513
		if(pon()) write(7,513)
		if(discprt) write(8,513)
	else if(id.eq.14) then
		print 514
		if(pon()) write(7,514)
		if(discprt) write(8,514)
	endif
	print 1080				!dashed line
	if(pon()) write(7,1080)
	if(discprt) write(8,1080)
c
117	OBHIST=id.le.2
	BHIST=(id.ge.3.and.id.le.11).or.id.eq.14
	AHIST=id.eq.12.or.id.eq.13
	tres=treso
	if(id.eq.9) tres=tresg
	if(shist) goto 2251    	!^^?omit if conam etc to be asked for
	AMPLO=-10000.0	!DEFAULT IS TO
	AMPHI=10000.	!INCLUDE OPENINGS OF ANY AMPLITUDE
	CONAM1=.FALSE.
	exass=.FALSE.
	ppopen=.false.
	print 405
405	format(' Type P(open) etc for each burst [N] ? ')
	read 101,ans
	if(UC(ans).eq.'Y') ppopen=.true.
c
	if(id.eq.12.or.id.eq.13) goto 520
c	if(ibtype.gt.1) conam1=.true.
	if(ibtype.gt.1) goto 619
	if(id.eq.4.or.id.eq.6) goto 619
	print 226
226	FORMAT('&Specify amplitude range for bursts [N] ? ')
	read 101,ans
	if(UC(ans).EQ.'Y') CONAM1=.TRUE.
	IF(.NOT.CONAM1) GOTO 2262
619	continue
	iexc=0
	if(id.ne.4.and.id.ne.6) goto 701
	print 702
702	format(' (1) Exclude time spent in sublevels')
	print 703
703	format(
     & ' (2) Exclude time spent in openings with amplitudes outside',
     & '     specified range')
	print 704
704	format(' Option no= ')
	read 102,iexc
	if(pon().and.iexc.eq.1) write(7,702)
	if(discprt.and.iexc.eq.1) write(8,702)
	if(pon().and.iexc.eq.2) write(7,703)
	if(discprt.and.iexc.eq.2) write(8,703)
	if(iexc.eq.1) goto 2262
701	continue
	print 227
227	FORMAT(/,' Define amplitude range:',/,
     & ' Low amp, high amp (real pA with sign)= ')
	call INPUT2r(amplo,amphi)
	if(amphi.gt.amplo) goto 2262
	a1=amphi	!swap
	amphi=amplo
	amplo=a1
	if(pon().and.iexc.eq.2) write(7,705)amplo,amphi
      if(discprt) write(8,705)amplo,amphi
705	format(' Amplitude range from ',g13.6,' to ',g13.6)
c
2262	if(iscan.ne.-1003) print 2261
2261	FORMAT( '&Exclude bursts with any ''assumed'' amps [N] ? ')
	if(iscan.eq.-1003) print 2263
2263	FORMAT( '&Exclude bursts with any ''dubious'' amps [N] ? ')
	read 101,ans
	exass=UC(ans).eq.'Y'
	IF(.NOT.CONAM1) GOTO 2251
c
c Need to specify how 'burst amp' to be defined (a) for option 12
c and (b) if restricted amplitude range to be used for other options
520	if(ibtype.gt.1) goto 2251
	print 521
521	FORMAT( ' (1)''Burst amplitude'' defined as:',/,
     & '  mean of the amps fitted to each opening of burst',/)
	print 522
522	FORMAT( ' (2)''Burst amplitude'' defined as:',/,
     & '  mean current (excluding shut periods) during burst')
	print 116	!option no?
	read 102,ibamp
2251	CONTINUE
	call pdub(exass,shist,iscan,pon())
	if(conam1.and.pon()) write(7,25)amplo,amphi
      if(conam1.and.discprt) write(8,25)amplo,amphi
	if(conam1.and.shist) print 25,amplo,amphi
25	format(' ''Amplitude'' range for bursts from ',f7.3,' to ',
     & f7.3,' pA only')
	if(pon().and.ibamp.eq.1) write(7,521)
	if(discprt.and.ibamp.eq.1) write(8,521)
	if(pon().and.ibamp.eq.2) write(7,522)
	if(discprt.and.ibamp.eq.2) write(8,522)
c
	if(id.ne.7.and.id.ne.8) goto 331
3322	continue
	if(shist) goto 305
	print 332
332	FORMAT('&Number of apparent openings, k= ')
	read 102,i
	ISG=i-1		!=no of short gaps
c NB if specify 1 opening/bst (isg=0) then no point in asking for
c '1 or more openings'=ANY burst!
	if(id.eq.11.or.isg.eq.0) goto 305
	print 3321
3321	FORMAT(
     &'&Include bursts with more than k apparent openings also ? ')
	read 101,ans
	if(UC(ans).EQ.'Y') ISG=-ISG
305	if(isg.ge.0.and.shist) print 118,isg+1
	if(isg.lt.0.and.shist) print 119,-isg+1
	if(pon().and.isg.ge.0) write(7,118)isg+1
      if(discprt.and.isg.ge.0) write(8,118)isg+1
118	format('  for bursts with ',i4,' apparent openings exactly.')
	if(pon().and.isg.lt.0) write(7,119) -isg+1
      if(discprt.and.isg.lt.0) write(8,119) -isg+1
119	format('  for bursts with ',i4,' or more apparent openings.')
	GOTO 337
C
331	IF(IABS(ID).LT.9.OR.ID.GT.11) GOTO 337	!ie for id=9,10,11
	if(id.ne.11) goto 606
	if(shist) goto 608
	print 609
609	FORMAT(' Correlation of Ith and Jth opening: I,J= ')
	read 3422,Ith,Jth
	if(Jth.gt.Ith) goto 608
	i=Ith
	Ith=Jth
	Jth=i
608	if(shist) print 1221,ith,jth
	if(pon()) write(7,1221)ith,jth
      if(discprt) write(8,1221)ith,jth
1221	format('  Correlation between ',i4,'th and ',i4,'th openings')
	goto 120
c
606	if(shist) goto 306
	print 333
333	FORMAT('&Duration of Jth event in the burst. J= ')
	read 102,JTH
c	IF(ID.EQ.11) GOTO 306
	print 361
361	FORMAT('&Include values above Jth also [N] ? ')	!FOR ID=9,10
	read 101,ans
	if(UC(ans).eq.'Y') JTH=-JTH
306	if(jth.gt.0.and.shist) print 121,jth
	if(pon().and.jth.gt.0) write(7,121)jth
      if(discprt.and.jth.gt.0) write(8,121)jth
121	format('  For ',i4,'th value only.')
	if(jth.lt.0.and.shist) print 122,-jth
	if(pon().and.jth.lt.0) write(7,122)jth
      if(discprt.and.jth.lt.0) write(8,122)jth
122	format('  For ',i4,'th and greater values.')
120	ISG=-10000	!IF GAPS/BST NOT SPECIFIED
	print 334
334	FORMAT(
     &  '&Use bursts with a specified number of apparent openings ? ')
	read 101,ans
	if(UC(ans).EQ.'N') GOTO 337
	GOTO 3322	!get ISG value
C
c Option to exclude time in sublevels for ibtype=1 and id=7,8,10,11
337	EXCSUB=.FALSE.
	if(shist) goto 307
	if(ibtype.eq.1.and.sublev.and.(id.eq.7.or.id.eq.8.or.
     & id.eq.10.or.id.eq.11)) goto 3301
	goto 330	!skip with excsub=false
c	IF(ibtype.gt.1.or.ID.LE.6.OR.ID.EQ.9.OR.
c     & (.NOT.SUBLEV)) GOTO 330		!skip
3301	print 336
336	FORMAT('&Exclude sublevel durations [N] ? ')
	read 101,ans
	if(UC(ans).EQ.'Y') EXCSUB=.TRUE.
307	if(id.eq.10.or.id.eq.11) goto 624
	if(shist.and.excsub) print 123
	if(pon().and.excsub) write(7,123)
	if(discprt.and.excsub) write(8,123)
123	format(' Sublevel durations excluded')
	goto 330
c diff printout for id=10,11
624	if(shist.and.excsub) print 1231
	if(pon().and.excsub) write(7,1231)
	if(discprt.and.excsub) write(8,1231)
1231	format(
     & ' Open periods with any sublevels disqualify whole burst')
C N.B.EXCLUSION OF SUBLEVELS MEANS EXCLUSION OF THEIR DURATION. FOR
C ID=9,10 JTH OPENING MEANS OPEN PERIOD PRECEDING JTH GAP WHETHER
C EXCSUB TRUE OR NOT
C
330	continue
	if(id.eq.14) then
		bmin=0.0	!none excluded (none<0.0)
		ibmin=0	!none excluded
		if(shist) then
		  print 108
		  if(pon()) write(7,108)
		  if(discprt) write(8,108)
		endif
		print 20
20		format('&(1) Include all bursts',/,
     &	' (2) Include only bursts longer than specified duration',/,
     &	' (3) Include only bursts with n or more open periods',/,
     &	' Option number [1] = ')
		read 102,i
		iopt=1
		if(i.ge.1.and.i.le.3) iopt=i
		if(iopt.eq.2) then
		  print 22
22		  format('& include bursts longer than t (ms) = ')
		  call INPUTr(bmin)
		  if(pon()) write(7,23) bmin
		  if(discprt) write(8,23) bmin
23		  format(' Only bursts longer than',g12.4,' ms included')
		else if(iopt.eq.3) then
		  ibmin=2
		  print 21
21		  format('& include bursts with n or more open periods:',
     &	  ' n [2] = ')
		  read 102,i
		  if(i.ge.2) ibmin=i
		  if(pon()) write(7,24) ibmin
		  if(discprt) write(8,24) ibmin
24		  format(
     &	' Only bursts with ',i2,' or more open periods included')
		endif
      	print 26
	      if(pon()) write(7,26)
	      if(discprt) write(8,26)
26		format(
     &' NB: the mean P(open) calculated here is a ''length-biassed''',/,
     &' estimate (e.g. every isolated short opening contributes 1.0)',/,
     &' Mean open time per burst/mean burst length usually better.')
		print 108
	endif
c Now define bursts:
c	I=1
	Ifst=1
c	ilast=1		!for debugging
	Nbst=0		!NO OF BURSTS
	Nb14=0		!no of bursts for id=14 (< Nbst if bmin or ibmin used)
	Nsgap=0		!NO OF GAPS-WITHIN-BST
	Nopen=0		!NO IF INDIV OPENINGS
	NAset=0		!NO OF OPEN (SUBSET A) PERIODS
	NBset=0		!NO OF GAP (SUBSET B) PERIODS
	J2=0		!COUNTS YVAL FOR ID=7,8,9,10
	J=0		!DITTO FOR ID=9,10 IF JTH NEGATIVE
	prt=pon()		!to avoid INT 11 crashes
C BURST DEFINED BY GAPS (ZERO AMP) < TCRIT
C NOP INCLUDES ANY OPENINGS INC SUBLEVELS
C NSG COUNTS GAPS<TCRIT WITHIN BURSTS ( SO NUMBER OF SOJOURNS IN C&H
C SUBSET A / BURST=NSG+1=< NOP.
C NOTE UNUSABLE GAP COUNTED AS RESOLVABLE SO IT ENDS BURST, BUT
C UNUSABLE OPENING CAUSES WHOLE BURST TO BE IGNORED
C  FIRST LOOK FOR FIRST OPENING (MUST BE USABLE) IN BURST
	bad1=.false.		!for ibtype=2,3
360	BPRINT=.FALSE.		!RESET FOR EACH BST
	goto (611,612,613) ibtype	!find 1st and last opening
c Burst extends from TINT(ifst) to TINT(K-1)
c Locate start and end if burst for ibtype=1.
c Note that burst may start with a sublevel opening even if EXCSUB
c is true as may have later full openings which need to be counted
c in for ID=4,6 for example.
c MODIF 07/17/90 04:12pm so bursts with sublevels rejected only
c if excsub=true for id=9,10.
c For  id=10,11 may also have EXCSUB true. For id=10 best
c omit entirely any burst for which the Jth (or any later if Jth neg)
c open period contains any sublevel. For id=11 omit the pair of values
c if EITHER the Ith OR the Jth open period contains any sublevel.
c For each open period ISUBS()=0 if it contains no sublevels; =1 if
c it contains any sublevels.
611	iamp=IACAL(Ifst,iampl)
	OPEN=Iamp.NE.0
	IF(open.AND.TINT(Ifst).GT.0.0) GOTO 322
C    FIRST OPENING OF BURST,TINT(Ifst), FOUND SO GO TO 322
	Ifst=Ifst+1
	IF(Ifst.LE.NINT) GOTO 360	!KEEP LOOKING FOR FIRST OPENING
	GOTO 99
C  NOW HAVE 1ST OPENING OF BST =TINT(Ifst). DO NOT CHANGE Ifst UNTIL
c BURST ENDS
322	NOP=1		!NUMBER OF OPENINGS IN CURRENT BURST
	NSG=0		!NO OF GAPS<TCRIT IN CURRENT BURST
	NAS=0		!NO OF OPEN (SUBSET A) PERIODS IN CURRENT BST
	NBS=0		!NO OF gaps (SUBSET B) PERIODS IN CURRENT BST
c			!=NAS-1 (same as NSG for ibtype=1,3)
	topen=0.0	!length of open period
	isub=0
	tgap=0.0	!length of gap-within-bst period
	Nbst=Nbst+1	!UPDATE # OF BURSTS- REVERSED BELOW IF BST IS BAD
	BAD=.FALSE.	!CURRENT BURST IS USABLE
	BAD1=.TRUE.
C    FOR ID=4,6 MUST HAVE AT LEAST ONE FULL OPENING OR GET RUBBISH-BAD1
C     CHECKS THIS
	FULL=IABS(Iamp-MAVAMP).LE.IACRIT
	A1=ACAL*float(Iamp)
	if(.not.sublev) full=Iamp.ne.0	!SO IACRIT,MAVAMP NOT NEEDED
	if((id.eq.4.or.id.eq.6).and.iexc.eq.2) FULL=(A1.ge.amplo)
     & .and.(A1.le.amphi)
	IF(FULL) BAD1=.FALSE.	!AT LEAST ONE FULL OPENING IN BURST
	SUB=OPEN.AND.(.NOT.FULL)
	if(.not.sub) goto 44
	isub=1		!this open period contains a sublevel
	if(excsub) goto 48			!omit sub
	if(id.eq.4.or.id.eq.6) goto 48		!omit sub
44	topen=tint(ifst)	!first indiv opening
48	igflag=0	!currently not in a gap period
	ioflag=1	!currently in an open period
	K=Ifst+1		!LOOK AT NEXT INTERVAL	DOES IT END BURST?
C  K LOOP STARTS AT LABEL 323
323	iamp=IACAL(k,iampl)
	A1=ACAL*float(Iamp)
	OPEN=Iamp.NE.0
	shut=.not.open
	FULL=IABS(Iamp-MAVAMP).LE.IACRIT
	if(.not.sublev) full=OPEN
	if((id.eq.4.or.id.eq.6).and.iexc.eq.2) FULL=(A1.ge.amplo)
     & .and.(A1.le.amphi)
	IF(FULL) BAD1=.FALSE.	!AT LEAST ONE FULL OPENING IN BURST
	SUB=OPEN.AND.(.NOT.FULL)
	if(sub) isub=1		!this open period contains a sublevel
	IF(shut.AND.(TC.GT.0.001).AND.TINT(K).GE.TC) BPRINT=.TRUE.
c End of burst? If shut can update TGAP here because if this shutting
c ends the burst then the current tgap not used for anything, and
c if it does not enf the burts then TGAP to be updated anyway
c	if(shut) ttest=tgap+tint(k)	!do not update tgap yet
	if(shut) tgap=tgap+tint(k)
	IF(shut.AND.(tgap.GT.ABS(TCRIT).OR.
     & ((TINT(K).LT.0.0).AND.K.LT.NINT))) GOTO 324
C  GOTO TO 324 WHEN END OF BST FOUND WITH K=INDEX OF GAP THAT ENDS IT
C NOTE THAT THE LAST INTERVAL IS ALWAYS SET UNUSABLE BUT IT IS NOT
C A VALID END-OF-BURST
	if(full) topen=topen+tint(k)	!note-each opening is sub OR full
	if(sub.and.excsub) goto 40			!omit sub
	if(sub.and.(id.eq.4.or.id.eq.6)) goto 40	!omit sub
	if(sub) topen=topen+tint(k)			!add in sub
40	continue
	IF((OPEN.AND.(TINT(K).LT.0.0)).OR.(K.EQ.NINT)) BAD=.TRUE.
C    BURST NOT COUNTED IF IT CONTAINS AN UNUSABLE OPENING ,OR IF LAST
C  DATA POINT REACHED BEFORE VALID END-OF-BURST FOUND. SO SET BAD,
C  BUT CARRY ON TO END ANYWAY TO
c Now assign lengths for open and gap periods.
	if(open) goto 41	!'open'
	goto 42			!'gap'
c end of ibtype=1
c
c Locate start and end if burst for ibtype=2 (50/non50 type)
c Burst starts with any usable opening in specified amp range='full' say
612	iamp=IACAL(Ifst,iampl)
	A1=ACAL*float(Iamp)
	full=(A1.ge.amplo).and.(A1.le.amphi)
	IF(FULL.and.TINT(Ifst).GT.0.0) GOTO 3221
C    FIRST OPENING OF BURST,TINT(Ifst), FOUND SO GO TO 3221
	Ifst=Ifst+1
	IF(Ifst.LE.NINT) GOTO 360	!KEEP LOOKING FOR FIRST OPENING
	GOTO 99
C  NOW HAVE 1ST OPENING OF BST =TINT(Ifst). DO NOT CHANGE Ifst UNTIL
c BURST ENDS
3221	NOP=1		!NUMBER OF OPENINGS IN CURRENT BURST
	NSG=0		!NO OF GAPS<TCRIT IN CURRENT BURST
	NAS=0		!NO OF OPEN (SUBSET A) PERIODS IN CURRENT BST
	NBS=0		!NO OF gap (SUBSET B) PERIODS IN CURRENT BST
c			!(same as NSG for ibtype=1)
	topen=0.0	!length of open period
	tgap=0.0	!length of gap-within-bst period
	Nbst=Nbst+1	!UPDATE # OF BURSTS- REVERSED BELOW IF BST IS BAD
	BAD=.FALSE.	!CURRENT BURST IS USABLE
	topen=tint(ifst)	!first indiv opening
	klast=ifst		!index of latest full opening
	igflag=0	!currently not in a gap period
	ioflag=1	!currently in an open period
	K=Ifst+1		!LOOK AT NEXT INTERVAL	DOES IT END BURST?
C  K LOOP STARTS AT LABEL 3231. Need to accumulate (in TGAP) total time
c spent in contiguous sojourns OUTSIDE specified range to see if total is
c less than tcrit- as soon as it is > tcrit (or unusable SHUT time
c found) the burst ends
3231	iamp=IACAL(k,iampl)
	OPEN=Iamp.NE.0
	shut=.not.open
	A1=ACAL*float(Iamp)
	full=(A1.ge.amplo).and.(A1.le.amphi)
c End of burst  yet? Note that get normal end of burst if a single
c 'gap' interval > tcrit follows immediately after last opening.
c But after last opening may have one OR MORE 'gap' intervals each
c of which is below Tcrit, but which eventually add to more than
c Tcrit causing burst to end: in this case the last open time
c will already have been assigned, and NSG and K will have been
c incremented so must correct these if interval preceding that which
c caused the ending was not an 'opening' (='full')
c end for unusable shutting
	if(shut.and.(tint(k).lt.0.0.and.k.lt.nint)) goto 3242
c end for 'gap'>tcrit
c	if(.not.full) ttest=tgap+tint(k)  !do not update tgap yet
	if(.not.full) tgap=tgap+tint(k)
	if(tgap.gt.abs(tcrit)) goto 3242	!test preceding interval
	goto 620				!not the end
c Before ending, test preceding interval
3242	iamp=IACAL(k-1,iampl)		!amp of prec interval
	A1=ACAL*float(Iamp)
c  -prev interval was open ('full') so end directly
	if((A1.ge.amplo).and.(A1.le.amphi)) goto 324	!end bst
c prev interval was 'gap' so correct NSG and K (and do not asssign
c last opening length again- already done)
	nsg=nsg-(k-klast-1)		!correct the value
	k=klast+1			!correct the value
	goto 3241			!end burst
c
c gap=.not.full
620	if(full) topen=topen+tint(k)
	if(full) klast=k	!klast=index of latest opening
	IF((OPEN.AND.(TINT(K).LT.0.0)).OR.(K.EQ.NINT)) BAD=.TRUE.
C    BURST NOT COUNTED IF IT CONTAINS AN UNUSABLE OPENING ,OR IF LAST
C  DATA POINT REACHED BEFORE VALID END-OF-BURST FOUND. SO SET BAD,
C  BUT CARRY ON TO END ANYWAY TO
C    MAKE SURE START OF NEXT BST FOUND CORRECTLY
	IF(shut.AND.(TC.GT.0.001).AND.TINT(K).GE.TC) BPRINT=.TRUE.
c Now assign lengths for open and gap periods.
	if(full) goto 41	!'open'
	goto 42			!'gap'
c End of ibtype=2
c
c Locate start and end if burst for ibtype=3
613	iamp=IACAL(Ifst,iampl)
	A1=ACAL*float(Iamp)
	full=(A1.ge.amplo).and.(A1.le.amphi)
	IF(FULL.and.TINT(Ifst).GT.0.0) GOTO 3222
C    FIRST OPENING OF BURST,TINT(Ifst), FOUND SO GO TO 3222
	Ifst=Ifst+1
	IF(Ifst.LE.NINT) GOTO 360	!KEEP LOOKING FOR FIRST OPENING
	GOTO 99
C  NOW HAVE 1ST OPENING OF BST =TINT(Ifst). DO NOT CHANGE Ifst UNTIL
c BURST ENDS
3222	NOP=1		!NUMBER OF OPENINGS IN CURRENT BURST
	NSG=0		!NO OF GAPS<TCRIT IN CURRENT BURST
	NAS=0		!NO OF OPEN (SUBSET A) PERIODS IN CURRENT BST
	NBS=0		!NO OF gap (SUBSET B) PERIODS IN CURRENT BST
c			!(same as NSG for ibtype=1)
	topen=0.0	!length of open period
	tgap=0.0	!length of gap-within-bst period
	Nbst=Nbst+1	!UPDATE # OF BURSTS- REVERSED BELOW IF BST IS BAD
	BAD=.FALSE.	!CURRENT BURST IS USABLE
	topen=tint(ifst)	!first indiv opening
	igflag=0	!currently not in a gap period
	ioflag=1	!currently in an open period
	K=Ifst+1		!LOOK AT NEXT INTERVAL	DOES IT END BURST?
C  K LOOP STARTS AT LABEL 3232. Need to accumulate (in TGAP) total time
c spent in contiguous sojourns OUTSIDE specified range to see if total is
c less than tcrit- as soon as it is > tcrit (or unusable SHUT time
c found) the burst ends
3232	iamp=IACAL(k,iampl)
	OPEN=Iamp.NE.0
	shut=.not.open
	A1=ACAL*float(Iamp)
	full=(A1.ge.amplo).and.(A1.le.amphi)
c gap=shut
	if(shut) tgap=tgap+tint(k)
c End of burst  yet?
c end for unusable shutting
	if(shut.and.(tint(k).lt.0.0.and.k.lt.nint)) goto 324
c end for 'gap'>tcrit
	if(tgap.gt.abs(tcrit)) goto 324
c end for ANY transition to opening outside amp range, AND
c end if transition to shut period of ANY duration is found if the
c shut period is followed by transition to an opening outside the
c specified range. Thus the 1st time  any opening to level outside range
c is found it may be preceded by (a) an opening within range- in which
c case burst ends at the end of the preceding opening, OR
c (b) by a (single) shut period, in which case the burst ends at the
c end of the open period that precedes this shut period- This
c shut period will have been put into TGAP; NSG will have been
c incremented, and the previous open period (correctly) assigned (but
c will have been assigned as the last opening so can skip last-op
c assignment at 324).
c However Tgap will not have been put into GAPS(), and NBS not
c incremented since invalid end (opening outside range) has been
c found for this gap. So need only decrement NSG and K if out-of-range
c opening was preceded by gap
	if(shut.or.full) goto 607	!burst NOT ended
c must be (open.and.(.not.full)),ie opening outside range, so end
	iamp=IACAL(k-1,iampl)		!amp of previous interval
c prev interval open, but in range:
	if(iamp.ne.0) goto 324		! end burst
c prev interval was shut:
	nsg=nsg-1		!correct the value
	k=k-1			!correct the value
	goto 3241			!end burst
C  GOTO TO 324 WHEN END OF BST FOUND WITH K=INDEX OF GAP THAT ENDS IT
C NOTE THAT THE LAST INTERVAL IS ALWAYS SET UNUSABLE BUT IT IS NOT
C A VALID END-OF-BURST
607	if(full) topen=topen+tint(k)
	IF((OPEN.AND.(TINT(K).LT.0.0)).OR.(K.EQ.NINT)) BAD=.TRUE.
C    BURST NOT COUNTED IF IT CONTAINS AN UNUSABLE OPENING ,OR IF LAST
C  DATA POINT REACHED BEFORE VALID END-OF-BURST FOUND. SO SET BAD,
C  BUT CARRY ON TO END ANYWAY TO
C    MAKE SURE START OF NEXT BST FOUND CORRECTLY
	IF(shut.AND.(TC.GT.0.001).AND.TINT(K).GE.TC) BPRINT=.TRUE.
c Now assign lengths for open and gap periods.
	if(full) goto 41	!'open'
	goto 42			!'gap'
c
c Allocate 'open' and 'gap' lengths- same for all IBTYPE
c Gap ends as soon as open period found (except for 1st open period, NAS=1)
c so assign length of gap that precedes it (only done when 1st opening of
c an open period found- controlled by IOFLAG)
c Next bit done if tint(k) is 'open'
41	nop=nop+1	!no of indiv openings
	ioflag=1	!currently in an open period
	if(igflag.eq.0) goto 43
	nbs=nbs+1
	gaps(nbs)=tgap
c#	if(debug()) print 614,nbs,gaps(nbs)
614	format(' GAPS(',i3,')= ',G13.6)
	tgap=0.0		!zero for next gap period
	igflag=0		!clear flag
	goto 43
c Next bit done if tint(k) is 'gap'
c Open period ends as soon as gap found so assign length of open period
c that precedes it (only done when 1st gap of an gap period found-
c controlled by IGFLAG). (For ibtype=1,3 'gap period'=single shut
c period anyway; but for ibtype=2 may have several intervals in one 'gap')
42	nsg=nsg+1	!no of indiv gap periods
	igflag=1	!currently in a gap period
	if(ioflag.eq.0) goto 43
	nas=nas+1
	if(nas.gt.500) print 623
623	format(' ***ERROR: MORE THAN 500 OPENINGS/BURST') !enlarge OPS()
	ops(nas)=topen
	isubs(nas)=isub	   !=1 if this open period contains a sublevel
c#	if(debug()) print 615,nas,ops(nas)
615	format(' OPS(',i3,')= ',G13.6)
	topen=0.0		!zero for next open period
	isub=0
	ioflag=0		!clear flag
43	continue
cd	IF(DEBUG()) print 405,Ifst,K,NOP,NSG,nas,nbs,ioflag,igflag
cd405	FORMAT( ' Ifst,K,NOP,NSG,nas,nbs,ioflag,igflag= ',/,8I8)
	K=K+1
c Continue in same burst
	IF(K.LE.NINT) GOTO (323,3231,3232) ibtype
	Nbst=Nbst-1	!REVERSE Nbst UPDATE IF NO PROPER ENDING FOUND
	GOTO 99		!DO NOT COUNT AS BURST IF NO PROPER ENDING FOUND
C
C END OF BURST FOUND. RECORD IT (IF NOT SET BAD). IF SET BAD YVAL(Nbst)
C VALUES WILL BE OVERWRITTEN BY THOSE FOR NEXT BURST
324	continue
	nas=nas+1
	ops(nas)=topen		!assign last opening of the burst
c#	if(debug()) print 615,nas,ops(nas)
c if debug, print all intervals up to end of this bst
3241	continue
c#	if(debug()) then
c#c     type intervals preceding current burst if any
c#	  if(ifst-1.gt.ilast) call DEBTYP(0,ilast,ifst-1,tint,iampl,
c#     &   acal,0,-1,-1,yval,prt)
c#	   print 618,Ifst,k-1
c#618	   format(/,/,' BURST=intervals ',i5,' to ',i5)
c#c      print current bst
c#	   call DEBTYP(0,ifst,k-1,tint,iampl,acal,
c#     & 0,-1,-1,yval,prt)
c#c     print 2 intervals following current bst
c#	   call DEBTYP(0,k,k+1,tint,iampl,acal,
c#     & 0,-1,-1,yval,prt)
c#	endif
c
c	ilast=k		! 1st interval to print for next call
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
	IF(BAD1.AND.(ID.EQ.4.OR.ID.EQ.6)) BAD=.TRUE.
c
c Calculate 'amplitude' for the burst just defined, if needed (needed
c for amp dist (id=12,13), or if conam1 or exass=true). Burst already
c excluded if it contains an unusable opening
	if((.not.bad).and.(id.eq.12.or.id.eq.13.or.conam1.or.
     & exass)) goto 523	!calc amp for burst
	goto 524		!skip amp calc
523	continue
	assume=.false.
	s=0.
	den=0.
	DO 525 M=Ifst,K-1	!GO THROUGH ALL INTERVALS IN BST
	m1=m
	iamp=IACAL(m1,iampl)
	if(iamp.eq.0) goto 525	!skip shut
	if(iabs(iampl(m1)).gt.9000) assume=.true. !at least 1 dubious
	if(ibamp.eq.2) goto 5231
	s=s+float(iamp)		!accum amp
	den=den+1.0
	goto 525
5231	s=s + tint(m)*float(iamp)
	den=den + tint(m)
525	continue
c
	if(den.gt.0.01) goto 526
	call bell(4)
	print 527
527	format(' Burst with no openings found!')
	stop
526	iamp=ifixr(s/den)	!define amp
	IF(ASSUME.AND.Iamp.GT.0) Iamp=Iamp+10000
	IF(ASSUME.AND.Iamp.LT.0) Iamp=Iamp-10000
	ibampl(nbst)=iamp		!amp (set dubious if nec)
c#	if(debug()) print 617,nbst,ACAL*float(IACAL(nbst,ibampl))
617	format(' Mean amp for burst #',i4,' (pA) = ',G13.6)
c
c Exclude bursts as specified
	IF(exass.and.iabs(Ibampl(nbst)).GT.9000) bad=.true.
	if(id.eq.12.or.id.eq.13) goto 524
	A1=ACAL*float(IACAL(nbst,ibampl))
	IF(conam1.and.((A1.LT.AMPLO).or.(A1.GT.AMPHI))) bad=.true.
524	continue
	IF(BAD) Nbst=Nbst-1	!REVERSE UPDATE OF Nbst
	IF(BAD) GOTO 325	!Skip assignment of Yval; look for next bst
cd	IF(DEBUG()) print 4051,Ifst,K,Nbst,NOP,nsg,nas,nbs
cd4051	FORMAT( ' End of burst: Ifst,K,Nbst,NOP,nsg,nas,nbs= ',/,7I8)
c
c
	if(nas.ne.nbs+1) goto 597
	if((ibtype.eq.1.or.ibtype.eq.3).and.nbs.ne.nsg) goto 597
	if((ibtype.eq.1.or.ibtype.eq.3).and.nas.ne.nsg+1) goto 597
	goto 599
597	call bell(2)
	print 598,nas,nbs,nop,nsg
598	format(' ***ERROR: nas,nbs,nop,nsg= ',4i8)
	pause
599	continue
c
	if(id.eq.12.or.id.eq.13) goto 602
	if(id.gt.2) goto 6021
c Assign YVAL() for id=1,2
	IF(ID.EQ.1) YVAL(Nbst)=FLOAT(NOP)
c	IF(ID.EQ.2) YVAL(Nbst)=FLOAT(NSG+1)
	IF(ID.EQ.2) YVAL(Nbst)=FLOAT(NBS+1)
c#	if(debug()) call DEBTYP(0,-1,-1,tint,iampl,acal,
c#     & 0,nbst,nbst,yval,prt)	!print YVAL(j)
	if(id.le.2) goto 338		!this burst finished
c
6021	if(id.gt.6) goto 601
c Assign YVAL() for id=3-6
c NB must now assign values for ID=3-6 after burst finished, as for
c others, in case amps needed (conam1 or exass)
c (NB sublevel times already excluded from OPS() for id=4,6)
	do 600 m=1,nAs
	yval(nbst)=yval(nbst) + ops(m)	!add open times for all
c add gaptime for 'total burst length'
	if(m.eq.nAs) goto 600	!no gaps(nas)
	if(id.eq.3.or.id.eq.4) yval(nbst)=yval(nbst) + gaps(m)
600	continue
c#	if(debug()) call DEBTYP(0,-1,-1,tint,iampl,acal,
c#     & 0,nbst,nbst,yval,prt)	!print YVAL(j)
c
c	DO 600 M=Ifst,K-1	!GO THROUGH ALL INTERVALS IN BST
c	m1=m
c	iamp=IACAL(m1,iampl)
c	OPEN=Iamp.NE.0
c	FULL=IABS(Iamp-MAVAMP).LE.IACRIT
c	if(.not.sublev) full=Iamp.ne.0
c	SUB=OPEN.AND.(.NOT.FULL)
cC   ADD IN INTERVAL LENGTHS AS APPROPRIATE,
cC   ACCORDING AS TINT(m) IS SHUT,SUB OR FULL. EACH INTERVAL MUST BE
cC   ONE (AND ONLY ONE) OF THESE.
cC    WHEN TINT(m)=SHUT I.E.SHORT GAP, ADD IN FOR ID=3,4
c	IF(.NOT.OPEN.AND.(ID.EQ.3.OR.ID.EQ.4))YVAL(Nbst)=
c     & YVAL(Nbst)+TINT(m)
cC    WHEN TINT(m) IS FULL OPENING, ADD IN FOR ID=3,4,5,6
c	IF(FULL.AND.(ID.GE.3.AND.ID.LE.6)) YVAL(Nbst)=YVAL(Nbst)+TINT(m)
cC    WHEN TINT(m) IS A SUBLEVEL OPENING, ADD IN FOR ID=3,5
c	IF(SUB.AND.(ID.EQ.3.OR.ID.EQ.5)) YVAL(Nbst)=YVAL(Nbst)+TINT(m)
c600	continue	!end of m loop
c
cC NOW ASSIGN YVAL FOR THOSE DISTRIBUTIONS (ID=-7,7,8,9,10) THAT MAY
C NOW ASSIGN YVAL FOR THOSE DISTRIBUTIONS (ID=,7,8,9,10,11) THAT MAY
C DEPEND ON THE NUMBER OF OPENINGS IN BURST JUST COMPLETED. AT THIS POINT
C I=INDEX OF 1ST OPENING IN THIS BST, AND K-1=INDEX OF LAST OPENING.
C ALL THIS WILL BE SKIPPED IF BURST SET BAD ALREADY.
C   N.B.EXCLUSION OF SUBLEVELS MEANS EXCLUSION OF THEIR DURATION. FOR
C ID=9,10 JTH OPENING MEANS OPEN PERIOD PRECEDING JTH GAP WHETHER
C EXCSUB TRUE OR NOT. EACH BURST SHOULD GIVE RISE TO, AT MOST, A SINGLE
C VALUE OF YVAL SO INCREMENT J2 EACH TIME A VALID BURST IS FOUND (EXCEPT
C FOR ID=11 FOR WHICH EACH VALID BST GIVES TWO VALUES, LENGTH OF 1ST
C AND JTH OPENINGS)
C  IF ID=9,10 AND JTH NEGATIVE MAY GET SEVERAL VALUES PER BST. IN THIS
C CASE USE J AS INDEX AND INCREMENT IT (FOR ID=10) WHEN FLAG J1 IS SET
C (=1) TO INDICATE THAT END OF CONCAT GROUP OF OPENINGS FOUND (IE GAP
C FOUND)
c Now ID > 6
601	continue
	if(id.eq.7.or.id.eq.8) goto 603
	if(id.eq.9.or.id.eq.10) goto 604
	if(id.eq.11) goto 605
	if(id.eq.14) goto 338		!Popen calc at end
	goto 338	!error- no id specified
c ID=7,8- omit bursts as specified
603	if(isg.eq.-10000) goto 350	!no of ops irrelevant
	if(isg.lt.0.and.nbs.lt.-isg) goto 338	!k or more?-not enough gaps
	if(isg.ge.0.and.nbs.ne.isg) goto 338	!NOT k exactly- skip bst
350	continue
c first screen for any invalid values
	do 50 m=1,nas
	if(ops(m).lt.0.0001) goto 3381
	if(id.eq.7.and.m.lt.nas.and.gaps(m).lt.0.0001) goto 3381
50	continue
c OK so assign Yval
	j2=j2+1		!valid burst
	do 45 m=1,nas
	yval(j2)=yval(j2) + ops(m)	!add open time
	if(id.eq.7.and.m.lt.nas) yval(j2)=yval(j2) + gaps(m) !add shut time
45	continue
c#	if(debug()) call DEBTYP(0,-1,-1,tint,iampl,acal,
c#     & 0,j2,j2,yval,prt)	!print YVAL(j)
	goto 338			!finished this bst
c
c Now ID=9,10- first exclude invalid bursts
604	if(isg.eq.-10000) goto 3501	!no of ops irrelevant
	if(isg.lt.0.and.nbs.lt.-isg) goto 338	!k or more?-not enough gaps
	if(isg.ge.0.and.nbs.ne.isg) goto 338	!NOT k exactly- skip bst
3501	if(id.eq.9.and.iabs(Jth).gt.nBs) goto 338   !no Jth gap so skip
	if(id.eq.10.and.iabs(Jth).gt.nAs) goto 338  !no Jth opening so skip
c
	if(Jth.lt.0) goto 46
c skip burst if open period contains a sublevel for id=10
	if(ibtype.eq.1.and.id.eq.10.and.isubs(Jth).ne.0.and.
     & excsub) goto 3382
c skip burst if any value undefined
	if(id.eq.9.and.gaps(Jth).lt.0.0001) goto 3381
	if(id.eq.10.and.Ops(Jth).lt.0.0001) goto 3381
	j2=j2+1
	if(id.eq.9) Yval(j2)=gaps(Jth)
	if(id.eq.10) Yval(j2)=Ops(Jth)
c#	if(debug()) call DEBTYP(0,-1,-1,tint,iampl,acal,
c#     & 0,j2,j2,yval,prt)	!print YVAL(j2)
	goto 338			!finished this bst
c   -for Jth < 0 may have several Yval() values per burst
46	j2=j2+1
c	jfst=j+1			!for debug
	n=nbs
	if(id.eq.10) n=nas
c first screen for invalid values- if any found skip whole burst
	do 49 m=-Jth,n
	if(id.eq.9.and.gaps(m).lt.0.0001) goto 3381
	if(id.eq.10.and.Ops(m).lt.0.0001) goto 3381
c skip bst if any op has sublevel
	if(ibtype.eq.1.and.id.eq.10.and.isubs(m).ne.0.and.
     & excsub) goto 3382
49	continue
c OK so assign Yval
	do 47 m=-Jth,n		!start at Jth op or gap
	j=j+1
	if(id.eq.9) Yval(j)=gaps(m)	!omit m=nas
47	if(id.eq.10) Yval(j)=ops(m)
c#	if(debug()) call DEBTYP(0,-1,-1,tint,iampl,acal,
c#     & 0,jfst,j,yval,prt)	!print YVAL(j)
	goto 338			!finished this bst
c
c Now ID=11
605	if(Jth.gt.nAs) goto 338	!no Jth opening so skip
	if(isg.eq.-10000) goto 3502	!no of ops irrelevant
	if(nbs.ne.isg) goto 338		!NOT k exactly- skip bst
3502	continue
c first screen for invalid values- if any found skip whole burst
	if(Ops(Ith).lt.0.0001.or.Ops(Jth).lt.0.0001) goto 3381
c skip bst if any op has sublevel
	if(ibtype.eq.1.and.
     & (isubs(Ith).ne.0.or.isubs(Jth).ne.0)) goto 3382
	j2=j2+1
	Yval(j2)=ops(Ith)
	j2=j2+1
	Yval(j2)=ops(Jth)
c#	if(debug()) call DEBTYP(0,-1,-1,tint,iampl,acal,
c#     & 0,j2-1,j2,yval,prt)	!print both YVAL(j)
	goto 338			!finished this bst
c
c Section for ID=12,13 -burst amp distribution
602	Yval(nbst)=acal*float(IACAL(nbst,ibampl))
	if(id.eq.13) Yval(nbst)=-Yval(nbst)
c#	if(debug()) call DEBTYP(0,-1,-1,tint,iampl,acal,
c#     & 0,nbst,nbst,yval,prt)		!print YVAL(nbst)
	goto 338
c
c Messages if values<0.001 or sublevel-containing periods found
3381	call bell(4)
	print 51,nbst
51	format( ' Interval < 0.0001 found: burst no= ',i6)
	goto 338
3382	print 53,nbst
53	format(' Burst no ',i6,
     & ' rejected because open period contains sublevel')
c
338	continue
C End of current burst: UPDATE Nsgap ETC BY ADDING VALUES FOR LAST BURST
c print this burst?
c Bit added 06/22/89 01:29pm to print total open, total shut and
c Popen for each burst when id=3, if ppopen=true.
c	if(.not.ppopen.or.id.ne.3) goto 400
	totop=0.
	totgap=0.
	do 401 i=1,nas
401	totop=totop+ops(i)
	do 402 i=1,nbs
402	totgap=totgap+gaps(i)
	blength=totop+totgap
	popen=totop/blength
c	if(id.eq.14.and.blength.gt.bmin) then
	if(id.eq.14) then
	   if(bmin.gt.0.000001) then
	     if(blength.gt.bmin) then
		nb14=nb14+1
		Yval(nb14)=popen
	     endif
	   else if(ibmin.gt.1) then
	     if(nas.ge.ibmin) then
		nb14=nb14+1
		Yval(nb14)=popen
	     endif
	   else
	     nb14=nb14+1
	     Yval(nb14)=popen
	   endif
c#	   if(debug()) call DEBTYP(0,-1,-1,tint,iampl,acal,
c#     &  0,nb14,nb14,yval,prt)		!print YVAL(nbst)
	endif
	if(.not.ppopen) goto 400
	print 403,nbst,totop,nas,totgap,nbs,blength,popen
	if(prt)write(7,403)nbst,totop,nas,totgap,nbs,blength,popen
	if(discprt)write(8,403)nbst,totop,nas,totgap,nbs,
     & blength,popen
403	format(' Burst #',i4,/,
     & ' total open (n)= ',g13.6,'(',i4,')',
     & ' total shut (n)= ',g13.6,'(',i4,')',/,
     & ' Burst length= ',g13.6,'   P(open)= ',g13.6)
400	continue
c
	IF((TC.LT.-0.001).AND.ID.GE.3.AND.ID.LE.6.AND.(YVAL(Nbst).GT.
     & ABS(TC))) GOTO 357	!print BURST
	IF((TCRIT.LT.0.).OR.(Nbst.GE.N1.AND.Nbst.LE.N2).OR.
     & BPRINT) GOTO 357	!print burst
	GOTO 354		!skip type
357	continue
	if((kplast.gt.0).and.(ifst-1.ge.kplast+1)) then
         print 54
         if(prt) write(7,54)
         if(discprt) write(8,54)
54	   format(' Intervals between bursts:')
	   do 55 m=kplast+1,ifst-1
	   m1=m
         print 56,m,tint(m),acal*iacal(m1,iampl)
         if(pon()) write(7,56) m,tint(m),acal*iacal(m1,iampl)
         if(discprt) write(8,56) m,tint(m),acal*iacal(m1,iampl)
56	   format(6x,i6,2x,2g13.6)
55	   continue
	   print 108
	   if(prt) write(7,108)
	   if(discprt) write(8,108)
	endif
	do 355 m=Ifst,k-1
	m1=m
	if(prt) write(7,356)Nbst,M,TINT(M),ACAL*IACAL(m1,iampl)
      if(discprt) write(8,356)Nbst,M,TINT(M),ACAL*IACAL(m1,iampl)
355	print 356,Nbst,M,TINT(M),ACAL*IACAL(m1,iampl)
356	format(2I6,2X,2G13.6)
	if(id.lt.7) print 3571,YVAL(Nbst)
	if(prt.and.id.lt.7) write(7,3571)YVAL(Nbst)
      if(discprt.and.id.lt.7) write(8,3571)YVAL(Nbst)
3571	format(' Y value = ',G13.6)
	kplast=k-1		!index of last tint() printed
	print 108
	if(prt) write(7,108)
	if(discprt) write(8,108)
354	continue
c
	Nsgap=Nsgap+NSG	!Nsgap=TOTAL NO OF SHORT GAPS IN CURRENT EXPT
	Nopen=Nopen+NOP
	NAset=NAset+nAs
	NBset=NBset+nBs
325	Ifst=K+1	!INDEX OF INTERVAL AFTER THAT WHICH ENDED LAST BST
	IF(Ifst.LE.NINT) GOTO 360
c END of Ifst loop
C RECORDING OF VALUES FOR LAST BURST COMPLETED- GO TO 360 TO LOOK
C EXPLICITLY FOR FIRST OPENING OF NEXT BURST
C
C ALL BURSTS IDENTIFIED.
99	continue
	IF(ID.GE.1.AND.ID.LE.6) NYVAL=Nbst
	if(id.eq.12.or.id.eq.13) Nyval=Nbst
	if(id.eq.14) Nyval=Nb14
	IF(ID.EQ.7.or.id.eq.8.OR.ID.EQ.11) NYVAL=J2
	IF((ID.EQ.9.OR.ID.EQ.10).AND.JTH.GT.0) NYVAL=J2
	IF((ID.EQ.9.OR.ID.EQ.10).AND.JTH.LT.0) NYVAL=J
	IF(ID.EQ.1) OBBAR=FLOAT(NOPEN)/FLOAT(NBST)
	IF(ID.EQ.2) OBBAR=FLOAT(NASET)/FLOAT(NBST)
	print 3281
3281	FORMAT(' Print burst parameters [Y] ? ')
	read 101,ans
	if(UC(ans).EQ.'N') GOTO 4091
	if(prt) write(7,328)tcrit,NOPEN,nsgap,NASET,nBset,NBST,
     & FLOAT(NOPEN)/FLOAT(NBST),FLOAT(NASET)/FLOAT(NBST),
     & FLOAT(nBset)/FLOAT(NBST)
      if(discprt) write(8,328)tcrit,NOPEN,nsgap,NASET,nBset,NBST,
     & FLOAT(NOPEN)/FLOAT(NBST),FLOAT(NASET)/FLOAT(NBST),
     & FLOAT(nBset)/FLOAT(NBST)
	print 328,tcrit,NOPEN,nsgap,NASET,nBset,NBST,
     & FLOAT(NOPEN)/FLOAT(NBST),FLOAT(NASET)/FLOAT(NBST),
     & FLOAT(nBset)/FLOAT(NBST)
328	FORMAT(/,
     & ' Critical gap length (ms)= ',g13.6,/,
     & ' No of indiv openings= ',I8,/
     & ' No of indiv gaps within bursts= ',I8,/
     & ,' No of open periods= ',I8,/
     & ,' No of gap periods within bursts= ',I8,/
     & ,' No of bursts= ',I8,/
     & ,' Mean no of indiv openings/burst= ',G13.6,/
     & ,' Mean no of open periods/burst= ',G13.6,/
     & ,' Mean no of gaps/burst= ',G13.6/)
c NB only if ibtype=2 can no of indiv gaps differ from no of gap periods
c
4091	continue
c	goto 408
c#	if(.not.debug()) GOTO 408
c	if(prt) write(7,4081)NYVAL
c      if(discprt) write(8,4081)NYVAL
c	print 4081,NYVAL
c4081	FORMAT(' Number of values for distribution= ',I8,/)
408	continue
	if(ibtype.ne.1) goto 207
	if(.not.debug().or.ibtype.ne.1) goto 207
	print 622
622	format(' Write Yval to disc for test [N] ? ')
	read 101,ans
	if(UC(ans).ne.'Y') goto 207
C 2*2560 real=2*5120 words=40 blocks on DK:
      OPEN(unit=14,file='D:\EKDIST.DAT',status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=10240)
	write(14,rec=1) (Yval(i),i=1,2559),nyval
	close(unit=14)
C
C YVAL AND NYVAL HAVE NOW BEEN DEFINED.
207	CONTINUE
	RETURN		!rest done in SETBIN
	END

