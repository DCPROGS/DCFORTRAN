	subroutine CDIST2(tint,ampl,iprops,nint,nintt,ndimy,avamp,acrit,
     & Yval,Nyval,iscan,tcrit,tc,nbst,obbar,ohist,bhist,ID,
     & ghist,ahist,pahist,obhist,iexc,ISG,treso,tresg,tres,trise,
     & revamp,idsav,ibamp,ibtype,ipop,ibmin,
     & nfile,iexstrt,iexend,onetcrit,tcvals)
c
c To define Yval() for burst distributions
c Version for new EKDIST (for new SCAN) (prev called cdist3) 02/15/94 06:59pm
c TO DO:
c (1) How should 'burst amplitude' be labelled as dubious now?! (see label 526)
c (2) Debtyp: note new call
c	subroutine DEBTYP(nval,n1,n2,tint,ampl,iprops,jval,j1,j2,yval,
c     & prnt,nintt,ndimy)
c  (old call: DEBTYP(nval,n1,n2,tint,iampl,acal,jval,j1,j2,yval,prnt)
c To print values of intervals and amps for debugging in EKDIST
c Nval,jval used only to indicate no of values when i1,i2 asked for
c  here (n1=0 or j1=0), otherwise ignored.
c (1)If n1>0 in call then TINT,AMP values from n1 to n2 are printed
c without asking for values here (and without title)
c (2) If n1=0 in call then asks which elements of TINT, AMP to type
c (3) If n1<0 in call then do only Jval part
c (4) Print of Yval() similarly controlled by Jval,j1,j2
c
c 08/25/92 12:34pm Lahey V5.x version
c 06/06/89 11:35am Lahey version
c
c CDIST3 is version of CDIST2 that allows 3 different definitions of a burst
c**CDIST2 modif April 87 so that 'amplitude' of bursts is defined and can
c (a) look at distribution of such amplitudes, and (b) limit other
c distributions to bursts with amplitudes in specified range.
c	11-73 version  July 1986. All expts in enlarged TINT etc
C**22-FEB-84. CALDIS SEP INTO CDIST1 (ALL DISTS BUT BURST) AND CDIST2
C
	real*4 TINT(nintt),ampl(nintt),yval(ndimy)
	integer*1 iprops(nintt)
	real*4 bampl(10240)		!for burst amps
	dimension ops(5001),gaps(5000)	!temp store for 1 burst
	dimension isubs(5000)
	character*1 ans,UC
	LOGICAL OHIST,BHIST,GHIST,AHIST,pahist,OBHIST,BPRINT
	LOGICAL CONAM1,exass,DEBUG,OPEN,shut,SUB,FULL,BAD,BAD1,EXCSUB
	logical exass1,ppopen,pbst
	logical sbin,shist,sres,sexp,pon,prt,deb
	logical dubious,badgap,btest,dubamp,revamp
	logical onetcrit,listall,newfile,findgap,badend
	logical discprt,slock,caplock
c
	real*4 tcvals(50)
	integer iexstrt(50),iexend(50)
c Arrays for burst mean (previously in sep subroutine, cmean1)
	ALLOCATABLE::avs,sds,ns
	real*4 avs(:),sds(:)
	integer ns(:)
c
	common/dp/discprt
	common/sblk/sbin,shist,sres,sexp
	COMMON/OVLBLK/OBMAX,JTH,CONAM1,exass,exass1,AMPHI,AMPLO,
     & EXCSUB,Ith
C
c Define functions
	pon()=slock()
	debug()=caplock()
	dubious(i)=BTEST(iprops(i),0)	!ampl(i) was dubious (bit 0 set ='1')
	badgap(i)=BTEST(iprops(i),3)	!tint(i) was unusable (bit 3='8' set)
c
108	format(/)
c101	format(a1)
C NOW GET PARAMETERS FOR HISTOGRAMS
	do i=1,nintt
	   yval(i)=0.0	!needs to be initialised for some burst distns
	enddo
c
	if(shist) then
	   id=idsav
c	   tcrit=abs(tcrit)	!switch off debugs 2nd time
	   listall=.false.
	   tc=abs(tc)
	   ppopen=.false.
	   goto 303		!same histo print (and tcrit??)
	else
	   OHIST=.FALSE.
	   BHIST=.FALSE.
	   GHIST=.FALSE.
	   AHIST=.FALSE.
	   pahist=.false.		!point amp histo
	   revamp=.false.		!point amp histo
	   OBHIST=.FALSE.		!DIST=OPENINGS/BURST
	   ppopen=.false.
	   ipop=0	!flag
	endif
c
C NOW DEFINE ARRAY YVAL CONTAINING VARIABLES FOR WHICH
C HISTOGRAM IS TO BE PLOTTED
	if(nfile.eq.1) then
	   if(tcrit.lt.0.) then		!first time
		print 321
321		format(' Critical gap length (ms) = ')
		call INPUTr(tcrit)
	   else
		print 320,tcrit
320		format(' Critical gap length [',f7.2,'ms] = ')
		call INPUTr(x)
		if(x.gt.0.0001) tcrit=x	!else prev tcrit used
	   endif
	else
	   ans='N'
	   if(onetcrit) ans='Y'
	   call DCASK('Critical gap length same for all files',ans,ans)
	   onetcrit=ans.eq.'Y'
	   if(tcrit.lt.0.) then		!first time
		if(onetcrit) then
		   print 33
33		   format(' Critical gap length (ms) = ')
		   call INPUTr(tcrit)
		   do i=1,nfile
			tcvals(i)=tcrit
		   enddo
		else
		   do i=1,nfile
			print 31,i
31			format(' File #',i3,': critical gap length (ms) = ')
			call INPUTr(tcvals(i))
		   enddo
		endif
	   else
		if(onetcrit) then
		   print 320,tcrit
c320		   format(' Critical gap length [',f7.2,'ms] = ')
		   call INPUTr(x)
		   if(x.gt.0.0001) tcrit=x	!else prev tcrit used
		   do i=1,nfile
			tcvals(i)=tcrit
		   enddo
		else
		   do i=1,nfile
			print 32,i,tcvals(i)
32			format(
     &        ' File #',i3,': critical gap length (ms) [',f7.2,'ms] = ')
			call INPUTr(tcvals(i))
		   enddo
		endif
	   endif
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
4175	format(' Option number (-1 for more details) [1] = ' )
	ibtype=1
	call INPUTi(ibtype)
	if(ibtype.ge.1.and.ibtype.le.3) goto 303
	print 4181
4181	format(
     & ' (1)Bursts to include ANY open state separated by shut periods',/,
     & '    that are all shorter than tcrit: ended by gap that is',/,
     & '    longer than tcrit.',/,
     & '    (normal definition).')
	print 4182
4182	format(
     & ' (2)Bursts consist of any series of openings to a specified',/,
     & '    amplitude (range), these openings being separated by',/,
     & '    periods in ANY other level (open or shut) that is',/,
     & '    (a) not in the specified amplitude range, and',/,
     & '    (b) has total duration < tcrit',/,
     & '     Burst ended by any sojourn longer than tcrit outside the',/,
     & '    specified range. This is the',/,
     & '    ''50pS/non-50pS'' print definition in which all amps NOT in',
     & /,'    range are treated as ''shut''. Looks at transitions',/,
     & '    from specified amp to all other states, and back.')
	print 4183
4183	format(
     & ' (3)Bursts consist of alternations open (in specified range)',/,
     & '    and SHUT (< tcrit). Burst ends when',/,
     & '    (a) shut period > tcrit found, OR',/,
     & '    (b) transition from specifed open level to ANY other open',/,
     & '        level (whatever its duration) is found, OR',/,
     & '    (c) transition to shut period of ANY duration is found',/,
     & '        if the shut period is followed by transition to an',/,
     & '        opening outside the specified range.',/,
     & '    Looks at transitions from specified amplitude to brief',/,
     & '    shut states, and back.')
	goto 4174
c
303	n1=-1		!defaults (no debug printing)
	n2=-1
	TC=0.
	kplast=0
C	IF(.NOT.DEBUG) GOTO 3212
	print 3211
3211	format(' List intervals in some/all bursts [N] ? ')
	ans='N'
	call INPUTa(ans)
c	read 101,ans
	if(UC(ans).ne.'Y') GOTO 3212
3215	print 112
112	format('  (1) List for all bursts')
	print 113
113	format('  (2) List from burst i1 to i2')
	print 114
114	format(
     & '  (3) List bursts with a gap longer than a specified value')
	print 115
115	format(
     & '  (4) List bursts with duration longer than a specified value')
	print 116
116	format(/,' Option number = ')
	call INPUTi(j)
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
3216	listall=j.eq.1
c	if(j.eq.1) tcrit=-tcrit
	if(j.ne.2) goto 3217
	print 3218
3218	format('&  i1,i2 = ')
	call INPUT2i(n1,n2)
c	if(pon()) write(7,111) n1,n2
c      if(discprt) write(8,111) n1,n2
c111	format('&  i1,i2 = ',2i8)
	goto 3212
3217	if(j.ne.3.and.j.ne.4) goto 3212
	print 3219
3219	FORMAT(' Specified value (ms)= ')
	call INPUTr(tc)
	if(pon()) write(7,60)tc
      if(discprt) write(8,60)tc
60	format(' Specified value (ms)= ',g13.6)
	if(j.eq.4) tc=-tc
c
3212	nsgap=0		!no of gaps .le. tcrit
	nopen=0		!total no of indiv openings (=no of A periods-1)
	nbst=0		!no of bursts
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
501	format( ' (1) No of individual openings per burst')
	print 502
502	format(
     & ' (2) No of open periods per burst (=1+short gaps/bst)')
	print 503
503	format( ' (3) Total burst length')
	if(ibtype.eq.1) print 504
c504	format( ' (4) Total burst length (exc time in sublevels)')
504	format(
     & ' (4) Total burst length (exc time outside spec range)')
	print 505
505	format(' (5) Total open time per burst')
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
509	format(' (9) Length of gaps within bursts')
	print 510
510	format(' (10) Length of open periods (1st, 2nd...) in a burst')
	print 511
511	format(' (11) Correlation of Ith and Jth opening in burst')
	if(ibtype.eq.1) print 512
512	format(' (12) Distribution of ''mean amplitudes'' of bursts')
	if(ibtype.eq.1) print 513
513	format(' (13) Distribution of ''mean amplitudes'' of bursts',/,
     & '       with signs reversed')
	print 514
514	format(' (14) Distribution of P(open) for bursts')
	print 515
515	format(' (15) Print means for kth open period/gap')
c NB ID=7,-7,8,9,10 are now ID=7,8,9,10,11
	print 116	!option no
	call INPUTi(ID)
	if(id.lt.1.or.id.gt.15) GOTO 416
	if(ibtype.ne.1.and.(id.eq.4.or.id.eq.6.or.id.eq.12.or.
     & id.eq.13)) goto 416	!invalid options
	idsav=id
c304	if(.not.pon()) goto 117
304	continue
c
	if(id.eq.15) then 	!initialise
	   if(nmax.le.0) nmax=8
	   print 340,nmax
340	   format(
     & '&Max number of open periods/burst for calcs, Nmax [',i4,'] = ')
	   call INPUTi(nmax)
	   Nmean=nmax*nmax+2*nmax-1		!total number of means
	   ALLOCATE(avs(nmean+1),sds(nmean+1),ns(nmean+1))
	   do i=1,nmean+1
		avs(i)=0.0
		sds(i)=0.0
		ns(i)=0
	   enddo
	endif
c
	ans='Y'
	call DCASK(
     &'Find a gap > tcrit before the first burst in each file',ans,ans)
	findgap=ans.eq.'Y'
	ans='N'
	call DCASK(
     &'Treat an unusable shut time as a valid end of burst',ans,ans)
	badend=ans.eq.'Y'
c
	if(ibtype.eq.1) then
		print 4171
		if(pon()) write(7,4181)
		if(discprt) write(8,4181)
	else if(ibtype.eq.2) then
		print 4172
		if(pon()) write(7,4182)
		if(discprt) write(8,4182)
	else if(ibtype.eq.3) then
		print 4173
		if(pon()) write(7,4183)
		if(discprt) write(8,4183)
	endif
	if(findgap) then
	   if(pon()) write(7,35)
	   if(discprt) write(8,35)
35	   format(
     &	' First burst starts only after gap > tcrit in each file')
	else
	   if(pon()) write(7,36)
	   if(discprt) write(8,36)
36	   format(
     &	' First burst starts with first good opening in each file')
	endif
	if(badend) then
	   if(pon()) write(7,37)
	   if(discprt) write(8,37)
37	   format(' Unusable shut time treated as a valid end of burst')
	else
	   if(pon()) write(7,38)
	   if(discprt) write(8,38)
38	   format(' Unusable shut time aborts a burst')
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
		revamp=.true.
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
	amplo=-10000.0
	amphi=10000.	!include openings of any amp
	conam1=.FALSE.
	exass=.FALSE.
	ppopen=.false.
	print 405
405	format(' Type P(open), Yval etc for each burst [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).eq.'Y') ppopen=.true.
c
	if(id.eq.12.or.id.eq.13) goto 520
c	if(ibtype.gt.1) conam1=.true.
	if(ibtype.gt.1) goto 619
	if(id.eq.4.or.id.eq.6) goto 619
	print 226
226	format('&Specify amplitude range for bursts [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).EQ.'Y') CONAM1=.TRUE.
	if(.not.conam1) goto 2262
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
704	format(' Option no = ')
	call INPUTi(iexc)
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
	if(amphi.lt.amplo) then
	   a1=amphi	!swap
	   amphi=amplo
	   amplo=a1
	endif
	if(pon().and.iexc.eq.2) write(7,705)amplo,amphi
      if(discprt) write(8,705)amplo,amphi
705	format(' Amplitude range from ',g13.6,' to ',g13.6)
c
2262	continue
	if(iscan.eq.-1003.or.iscan.ge.100) then
	   print 2263
2263	   format('&Exclude bursts with any ''dubious'' amps [N] ? ')
	else
	   print 2261
2261	   format('&Exclude bursts with any ''assumed'' amps [N] ? ')
	endif
	ans='N'
	call INPUTa(ans)
	exass=UC(ans).eq.'Y'
	if(.not.conam1) goto 2251
c
c Need to specify how 'burst amp' to be defined (a) for option 12
c and (b) if restricted amplitude range to be used for other options
520	continue
	if(ibtype.gt.1) goto 2251
	ibamp=1	!default
	print 521
521	format(' (1) ''Burst amplitude'' defined as:',/,
     & '  mean of the amps fitted to each opening of burst')
	print 522
522	format(' (2) ''Burst amplitude'' defined as:',/,
     & '  mean current (excluding shut periods) during burst')
	print 30
30	format(' Option number [1] = ')
	call INPUTi(ibamp)
2251	CONTINUE
	call PDUB(exass,shist,iscan,pon())
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
	ISG=-10000	!if gaps/bst not specified
c	if(id.ne.7.and.id.ne.8) goto 331
	if(id.eq.7.or.id.eq.8.or.id.eq.12.or.id.eq.13) goto 3322
	goto 331	!for all other id
c Ask for number of ops/bst (id=7,8,12,13 and pos id=9,10,11
3322	continue		!return for isg for id=9,10,11
	if(id.eq.12.or.id.eq.13) then
	   call SETDUB(iscan,exass,shist,prt,tc1,trise,tcfac)
	   print 334
334	   FORMAT(
     &'&Use bursts with a specified number of apparent openings [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	   if(UC(ans).ne.'Y') goto 331
	endif
	if(shist) goto 305
	print 332
332	FORMAT('&Number of apparent openings, k = ')
	call INPUTi(i)
	ISG=i-1		!=no of short gaps
c NB if specify 1 opening/bst (isg=0) then no point in asking for
c '1 or more openings'=ANY burst!
	if(id.eq.11.or.isg.eq.0) goto 305
	print 3321
3321	format(
     &'&Include bursts with MORE than k apparent openings also [n] ? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).eq.'Y') ISG=-ISG
305	if(isg.ge.0.and.shist) print 118,isg+1
	if(isg.lt.0.and.shist) print 119,-isg+1
	if(pon().and.isg.ge.0) write(7,118)isg+1
      if(discprt.and.isg.ge.0) write(8,118)isg+1
118	format(/,' For bursts with ',i4,' apparent openings exactly.',/)
	if(pon().and.isg.lt.0) write(7,119) -isg+1
      if(discprt.and.isg.lt.0) write(8,119) -isg+1
119	format(/,' For bursts with ',i4,' or more apparent openings.',/)
	GOTO 337
C
331	if(iabs(ID).lt.9.or.ID.gt.11) GOTO 337	!ie for id=9,10,11
	if(id.ne.11) goto 606
	if(shist) goto 608
	print 609
609	format(' Correlation of Ith and Jth opening: I,J= ')
	call INPUT2i(Ith,Jth)
c	read 3422,Ith,Jth
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
	call INPUTi(jth)
	print 361
361	FORMAT('&Include values above Jth also [N] ? ')	!FOR ID=9,10
	ans='N'
	call INPUTa(ans)
	if(UC(ans).eq.'Y') JTH=-JTH
306	if(jth.gt.0.and.shist) print 121,jth
	if(pon().and.jth.gt.0) write(7,121)jth
      if(discprt.and.jth.gt.0) write(8,121)jth
121	format('  For ',i4,'th value only.')
	if(jth.lt.0.and.shist) print 122,-jth
	if(pon().and.jth.lt.0) write(7,122)jth
      if(discprt.and.jth.lt.0) write(8,122)jth
122	format('  For ',i4,'th and greater values.')
120	ISG=-10000	!if gaps/bst not specified
	print 335
335	   FORMAT(
     &'&Use bursts with a specified number of apparent openings [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).eq.'N') GOTO 337
	goto 3322	!get ISG value
C
c Option to exclude time in sublevels for ibtype=1 and id=7,8,10,11
337	EXCSUB=.FALSE.
	if(shist) goto 307
c==	if(ibtype.eq.1.and.sublev.and.(id.eq.7.or.id.eq.8.or.
	if(ibtype.eq.1.and.(id.eq.7.or.id.eq.8.or.
     & id.eq.10.or.id.eq.11)) goto 3301
	goto 330	!skip with excsub=false
c	IF(ibtype.gt.1.or.ID.LE.6.OR.ID.EQ.9.OR.
c     & (.NOT.SUBLEV)) GOTO 330		!skip
3301	print 336
336	FORMAT('&Exclude sublevel durations [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).EQ.'Y') EXCSUB=.true.
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
	   ipop=0	!flag
	   bmin=0.0	!none excluded (none<0.0)
	   ibmin=0	!none excluded
	   if(shist) then
		print 108
		if(pon()) write(7,108)
		if(discprt) write(8,108)
	   endif
	   print 20
20	   format('&(1) Include all bursts',/,
     &	' (2) Include only bursts longer than specified duration',/,
     &	' (3) Include only bursts with n or more open periods',/,
     &	' (4) Calculate Popen omitting last opening (so have',/,
     &	'       equal number of open and shut times)        ',/,
     &	' Option number [1] = ')
	   i=1
	   call INPUTi(i)
	   iopt=1
	   if(i.ge.1.and.i.le.4) iopt=i
	   if(iopt.eq.2) then
		print 22
22		format('& include bursts longer than t (ms) = ')
		call INPUTr(bmin)
		if(pon()) write(7,23) bmin
		if(discprt) write(8,23) bmin
23		format(' Only bursts longer than',g12.4,' ms included')
	   else if(iopt.eq.3) then
		ibmin=2
		print 21
21		format('& include bursts with n or more open periods:',
     &	 ' n [2] = ')
	      call INPUTi(ibmin)
		if(i.le.1) ibmin=0
		if(pon()) write(7,24) ibmin
		if(discprt) write(8,24) ibmin
24		format(
     &	' Only bursts with ',i2,' or more open periods included')
	   else if(iopt.eq.4) then
	      ipop=1	!flag
		if(pon()) write(7,241)
		if(discprt) write(8,241)
241		format(/,
     &	' NB Popen CALCULATED OMITTING LAST OPENING!',/)
	   endif
         print 26
	   if(pon()) write(7,26)
	   if(discprt) write(8,26)
26	   format(
     &' NB: the mean P(open) calculated here is a ''length-biassed''',/,
     &' estimate (e.g. every isolated short opening contributes 1.0)',/,
     &' Mean open time per burst/mean burst length usually better.')
	   print 108
	endif
c
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
	nbad1=0		!number of bursts aborted by bad open time
	nbad2=0		!number of bursts aborted by unusable shut time
	J2=0		!COUNTS YVAL FOR ID=7,8,9,10,11,12,13; may be <nbst
	J=0		!DITTO FOR ID=9,10 IF JTH NEGATIVE
	indlast=0	!to control printing below
	prt=pon()		!to avoid INT 11 crashes
c
C BURST DEFINED by gaps (zero amp) < Tcrit
C NOP includes any openings inc sublevels
C NSG counts gaps<Tcrit within bursts (so number of sojourns in C&H subset A
C per burst = NSG+1 =< NOP.
C NOTE unusable gap counted as resolvable so it ends burst, but
C an unusable opening causes whole burst to be ignored
C  FIRST LOOK FOR FIRST OPENING (must be usable) in burst.
c OR, IF FINDGAP=T, LOOK FOR USABLE GAP > TCRIT BEFORE STARTING
c Also, if onetcrit=false, need to see which file we are in currently
c and set tcrit accordingly. Also need to ensure gap >tcrit before
c first burst in each file if findgap=true
c Set newfile=true while looking for first burst in current file
c and set it false after a burst has been succesfully found
	ifile=1
	newfile=.true.
	if(nfile.gt.1) then
	   tcrit=tcvals(1)
	   iflast=0
	endif
	bad1=.false.		!for ibtype=2,3
360	bprint=.false.		!reset for each bst
	deb=debug()			!ditto
c check which file the current ifst is in, and set tcrit accordingly
c (also need to know when new file starts when findgap=true)
	if(nfile.gt.1) then
	   do i=1,nfile
		if(i.eq.nfile) then
		   iend=nint
		else
		   iend=iexstrt(i+1)-1
		endif
		if(ifst.ge.iexstrt(i).and.ifst.le.iend) then	!ifile found
		   ifile=i
		   tcrit=tcvals(ifile)
		   if(ifile.ne.iflast) then
			newfile=.true.
			iflast=ifile
		   endif
		   goto 34
		endif
	   enddo
	endif
	if(newfile) then
	   print 70,ifile,tcrit
	   if(discprt.and.deb) write(8,70) ifile,tcrit
70	   format(' New file: #',i3,'  tcrit = ',g13.6,' ms')
	endif
34	goto (611,612,613) ibtype	!find 1st and last opening
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
611	continue
	amp=ampl(ifst)
	OPEN=amp.ne.0
c==   if(open.and.(.not.badgap(ifst))) goto 322
	shut=.not.open
	if(newfile.and.findgap) then	!find gap>tcrit before starting
	   if(shut.and.(tint(ifst).ge.tcrit).and.
     &		(.not.badgap(ifst))) then
		ifst=ifst+1
		if(ifst.gt.nint) goto 99
		amp=ampl(ifst)
		open=amp.ne.0
		if(open.and.(.not.badgap(ifst))) goto 322
	   endif
	else
	   if(open.and.(.not.badgap(ifst))) goto 322
	endif
c    First opening of burst, tint(Ifst), found so go to 322
	Ifst=Ifst+1
	if(Ifst.le.nint) goto 360	!KEEP LOOKING FOR FIRST OPENING
	goto 99
C  Now have 1st opening of bst =tint(Ifst). Do not change Ifst until
c burst ends
322	NOP=1		!Number of openings in current burst
	NSG=0		!No of gaps<Tcrit in current burst
	NAS=0		!No of open (subset A) periods in current bst
	NBS=0		!No of gaps (subset B) in current bst
c			!=NAS-1 (same as NSG for ibtype=1,3)
	topen=0.0	!length of open period
	isub=0
	tgap=0.0	!length of gap-within-bst period
	Nbst=Nbst+1	!Update # of bursts- reversed below if bst is bad
	BAD=.false.	!current burst is usable
	BAD1=.true.
C    For ID=4,6 must have at least one full opening,or get rubbish -BAD1
C     checks this.
	full=abs(amp-avamp).le.acrit
	if((id.eq.4.or.id.eq.6).and.iexc.eq.2) FULL=(amp.ge.amplo)
     & .and.(amp.le.amphi)
	if(FULL) bad1=.false.	!at least one full opening in burst
	SUB=open.and.(.not.full)
	if(.not.sub) goto 44
	isub=1		!this open period contains a sublevel
	if(excsub) goto 48			!omit sub
	if(id.eq.4.or.id.eq.6) goto 48		!omit sub
44	topen=tint(ifst)	!first indiv opening
48	igflag=0	!currently not in a gap period
	ioflag=1	!currently in an open period
	k=Ifst+1		!LOOK AT NEXT INTERVAL	DOES IT END BURST?
C  k loop starts at label 323
323	continue
	amp=ampl(k)
	open=amp.ne.0
	shut=.not.open
	full=abs(amp-avamp).le.acrit
c	if(.not.sublev) full=open
	if((id.eq.4.or.id.eq.6).and.iexc.eq.2) FULL=(amp.ge.amplo)
     & .and.(amp.le.amphi)
	if(full) bad1=.false.	!at least one full opening in burst
	sub=open.and.(.not.full)
	if(sub) isub=1		!this open period contains a sublevel
	IF(shut.and.(tc.gt.0.001).and.tint(k).ge.tc) bprint=.true.
c End of burst? If shut can update TGAP here because if this shutting
c ends the burst then the current tgap not used for anything, and
c if it does not end the burst then TGAP to be updated anyway
c	if(shut) ttest=tgap+tint(k)	!do not update tgap yet
	if(shut) tgap=tgap+tint(k)
c	if(shut.and.(tgap.gt.tcrit.or.
c     & (badgap(k).and.k.lt.nint))) goto 324
	if(shut) then
	   if(badend) then
		if(tgap.gt.tcrit.or.(badgap(k).and.k.lt.nint)) goto 324
	   else
		if(badgap(k).and.k.lt.nint) then
		   nbad2=nbad2+1
		   bad=.true.	!abort burst
		   print 71,k
		   if(deb.and.discprt) write(8,71) k,tint(k),iprops(k)
71		   format(
     &	 ' Bad gap #',i5,' aborts burst:',g12.5, ' ms, props = ',i2)
		else
		   if(tgap.gt.tcrit) goto 324	!valid end of burst
		endif
	   endif
	endif
C  Go to 324 when end of bst found, with k=index of gap that ends it.
C NOTE that the last interval is always set unusable, but it is not
C a valid end-of-burst
	if(full) topen=topen+tint(k)	!note-each opening is sub OR full
	if(sub.and.excsub) goto 40			!omit sub
	if(sub.and.(id.eq.4.or.id.eq.6)) goto 40	!omit sub
	if(sub) topen=topen+tint(k)			!add in sub
40	continue
	if((open.and.badgap(k)).or.(k.eq.nint)) then
	   nbad1=nbad1+1
	   bad=.true.
	endif
C    Burst not counted if it contains an unusable opening (not pos in current
c	version of SCAN ,or if last data point reached before valid end-of-burst
c 	found, so set bad, but carry on to end anyway to make sure start of
c	next burst found correctly
c Now assign lengths for open and gap periods.
	if(open) goto 41	!'open'
	goto 42			!'gap'
c end of ibtype=1
c
c Locate start and end if burst for ibtype=2 (50/non50 type)
c Burst starts with any usable opening in specified amp range='full' say
612	continue
	amp=ampl(ifst)
	full=(amp.ge.amplo).and.(amp.le.amphi)
c==	if(full.and.(.not.badgap(Ifst))) goto 3221
	shut=amp.ne.0
	if(newfile.and.findgap) then	!find gap>tcrit before starting
	   if(shut.and.(tint(ifst).ge.tcrit).and.
     &		(.not.badgap(ifst))) then
		ifst=ifst+1
		if(ifst.gt.nint) goto 99
		amp=ampl(ifst)
		full=(amp.ge.amplo).and.(amp.le.amphi)
		if(full.and.(.not.badgap(ifst))) goto 3221
	   endif
	else
	   if(full.and.(.not.badgap(ifst))) goto 3221
	endif
C    First opening of burst, tint(Ifst), found
	Ifst=Ifst+1
	if(Ifst.le.nint) goto 360	!keep looking for first opening
	goto 99
C  Now have 1st opening of bst =tint(Ifst). Do not change Ifst until
c burst ends
3221	NOP=1		!Number of openings in current burst
	NSG=0		!No of gaps<Tcrit in current burst
	NAS=0		!No of open (subset A) periods in current bst
	NBS=0		!No of gaps (subset B) in current bst
c			!=NAS-1 (same as NSG for ibtype=1)
	topen=0.0	!length of open period
	tgap=0.0	!length of gap-within-bst period
	Nbst=Nbst+1	!UPDATE # OF BURSTS- REVERSED BELOW IF BST IS BAD
	bad=.false.	!current burst is usable
	topen=tint(ifst)	!first indiv opening
	klast=ifst		!index of latest full opening
	igflag=0	!currently not in a gap period
	ioflag=1	!currently in an open period
	k=Ifst+1		!look at next interval -does it end burst?
C  k loop starts at label 3231. Need to accumulate (in TGAP) total time
c spent in contiguous sojourns OUTSIDE specified range to see if total is
c less than tcrit- as soon as it is > tcrit (or unusable SHUT time
c found) the burst ends
3231	continue
c==	iamp=IACAL(k,iampl)
	amp=ampl(k)
	open=amp.ne.0
	shut=.not.open
	full=(amp.ge.amplo).and.(amp.le.amphi)
c End of burst  yet? Note that get normal end of burst if a single
c 'gap' interval > tcrit follows immediately after last opening.
c But after last opening may have one OR MORE 'gap' intervals each
c of which is below Tcrit, but which eventually add to more than
c Tcrit causing burst to end: in this case the last open time
c will already have been assigned, and NSG and K will have been
c incremented so must correct these if interval preceding that which
c caused the ending was not an 'opening' (='full')
c end for unusable shutting
c==	if(shut.and.badgap(k).and.k.lt.nint) goto 3242
	if(shut) then
	   if(badend) then
		if(badgap(k).and.k.lt.nint) goto 3242
	   else
		if(badgap(k).and.k.lt.nint) then
		   nbad2=nbad2+1
		   bad=.true.	!abort burst
		   if(deb.and.discprt) write(8,71) k,tint(k),iprops(k)
c71		   format(
c     &	 ' Bad gap #',i5,' aborts burst:',g12.5, ' ms, props = ',i2)
		endif
	   endif
	endif
c end for 'gap'>tcrit
c	if(.not.full) ttest=tgap+tint(k)  !do not update tgap yet
	if(.not.full) tgap=tgap+tint(k)
	if(tgap.gt.tcrit) goto 3242	!test preceding interval
	goto 620				!not the end
c Before ending, test preceding interval
3242	continue
	amp=ampl(k-1)		!amp of prec interval
c  -prev interval was open ('full') so end directly
	if((amp.ge.amplo).and.(amp.le.amphi)) goto 324	!end bst
c prev interval was 'gap' so correct NSG and K (and do not asssign
c last opening length again- already done)
	nsg=nsg-(k-klast-1)		!correct the value
	k=klast+1			!correct the value
	goto 3241			!end burst
c
c gap=.not.full
620	if(full) topen=topen+tint(k)
	if(full) klast=k	!klast=index of latest opening
	if((open.and.badgap(k)).or.(k.eq.nint)) then
	   nbad1=nbad1+1
	   bad=.true.
	endif
C    Burst not counted if it contains an unusable opening (not pos in current
c	version of SCAN ,or if last data point reached before valid end-of-burst
c 	found, so set bad, but carry on to end anyway to make sure start of
c	next burst found correctly
	if(shut.and.(tc.gt.0.001).and.tint(k).ge.tc) bprint=.true.
c Now assign lengths for open and gap periods.
	if(full) goto 41	!'open'
	goto 42			!'gap'
c End of ibtype=2
c
c Locate start and end if burst for ibtype=3
613	continue
	amp=ampl(ifst)
	full=(amp.ge.amplo).and.(amp.le.amphi)
c==	if(full.and.(.not.badgap(Ifst))) goto 3222
	shut=amp.ne.0
	if(newfile.and.findgap) then	!find gap>tcrit before starting
	   if(shut.and.(tint(ifst).ge.tcrit).and.
     &		(.not.badgap(ifst))) then
		ifst=ifst+1
		if(ifst.gt.nint) goto 99
		amp=ampl(ifst)
		full=(amp.ge.amplo).and.(amp.le.amphi)
		if(full.and.(.not.badgap(ifst))) goto 3222
	   endif
	else
	   if(full.and.(.not.badgap(ifst))) goto 3222
	endif
C    FIRST OPENING OF BURST,TINT(Ifst), FOUND SO GO TO 3222
	Ifst=Ifst+1
	if(Ifst.le.nint) goto 360	!KEEP LOOKING FOR FIRST OPENING
	goto 99
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
	bad=.false.	!CURRENT BURST IS USABLE
	topen=tint(ifst)	!first indiv opening
	igflag=0	!currently not in a gap period
	ioflag=1	!currently in an open period
	k=Ifst+1		!LOOK AT NEXT INTERVAL	DOES IT END BURST?
C  K LOOP STARTS AT LABEL 3232. Need to accumulate (in TGAP) total time
c spent in contiguous sojourns OUTSIDE specified range to see if total is
c less than tcrit- as soon as it is > tcrit (or unusable SHUT time
c found) the burst ends
3232	continue
c	iamp=IACAL(k,iampl)
	amp=ampl(k)
	open=amp.ne.0
	shut=.not.open
	full=(amp.ge.amplo).and.(amp.le.amphi)
c gap=shut
	if(shut) tgap=tgap+tint(k)
c End of burst  yet?
c end for unusable shutting
c==	if(shut.and.(badgap(k).and.k.lt.nint)) goto 324
	if(shut.and.(badgap(k).and.k.lt.nint)) then
	   if(badend) then
		goto 324
	   else
		nbad2=nbad2+1
		bad=.true.	!abort burst
		if(deb.and.discprt) write(8,71) k,tint(k),iprops(k)
c71		format(
c     &	 ' Bad gap #',i5,' aborts burst:',g12.5, ' ms, props = ',i2)
	   endif
	endif
c end for 'gap'>tcrit
	if(tgap.gt.tcrit) goto 324
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
c==	iamp=IACAL(k-1,iampl)		!amp of previous interval
	amp=ampl(k-1)
c prev interval open, but in range:
	if(amp.ne.0) goto 324		! end burst
c prev interval was shut:
	nsg=nsg-1		!correct the value
	k=k-1			!correct the value
	goto 3241			!end burst
C  GOTO TO 324 WHEN END OF BST FOUND WITH K=INDEX OF GAP THAT ENDS IT
C NOTE THAT THE LAST INTERVAL IS ALWAYS SET UNUSABLE BUT IT IS NOT
C A VALID END-OF-BURST
607	if(full) topen=topen+tint(k)
	if(open.and.(badgap(k).or.k.eq.nint)) then
	   nbad1=nbad1+1
	   bad=.true.
	endif
C    BURST NOT COUNTED IF IT CONTAINS AN UNUSABLE OPENING ,OR IF LAST
C  DATA POINT REACHED BEFORE VALID END-OF-BURST FOUND. SO SET BAD,
C  BUT CARRY ON TO END ANYWAY TO
C    MAKE SURE START OF NEXT BST FOUND CORRECTLY
	if(shut.and.(tc.gt.0.001).and.tint(k).ge.tc) bprint=.true.
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
c#	if(deb) print 614,nbs,gaps(nbs)
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
	if(nas.gt.5000) print 623
623	format(' ***ERROR: MORE THAN 5000 OPENINGS/BURST') !enlarge OPS()
	ops(nas)=topen
	isubs(nas)=isub	   !=1 if this open period contains a sublevel
c#	if(deb) print 615,nas,ops(nas)
615	format(' OPS(',i3,')= ',G13.6)
	topen=0.0		!zero for next open period
	isub=0
	ioflag=0		!clear flag
43	continue
c	IF(deb) print 405,Ifst,K,NOP,NSG,nas,nbs,ioflag,igflag
c405	format(' Ifst,K,NOP,NSG,nas,nbs,ioflag,igflag= ',/,8I8)
	k=k+1
c Continue in same burst
	if(k.le.nint) goto (323,3231,3232) ibtype
	Nbst=Nbst-1	!REVERSE Nbst UPDATE IF NO PROPER ENDING FOUND
	goto 99		!DO NOT COUNT AS BURST IF NO PROPER ENDING FOUND
C
C END OF BURST FOUND. RECORD IT (IF NOT SET BAD). IF SET BAD YVAL(Nbst)
C VALUES WILL BE OVERWRITTEN BY THOSE FOR NEXT BURST
324	continue
	nas=nas+1
	ops(nas)=topen		!assign last opening of the burst
c#	if(deb) print 615,nas,ops(nas)
c if debug, print all intervals up to end of this bst
3241	continue
	newfile=.false.	!set false after a valid burst located
	if(deb) then
c     type intervals preceding current burst if any
	  if(ifst-1.gt.ilast) call DEBTYP(0,ilast,ifst-1,tint,ampl,
     &   iprops,0,-1,-1,yval,prt,nintt,ndimy)
	   print 618,Ifst,k-1
618	   format(/,/,' BURST=intervals ',i5,' to ',i5)
c      print current bst
	   call DEBTYP(0,ifst,k-1,tint,ampl,iprops,
     & 0,-1,-1,yval,prt,nintt,ndimy)
c     print 2 intervals following current bst
	   call DEBTYP(0,k,k+1,tint,ampl,iprops,
     & 0,-1,-1,yval,prt,nintt,ndimy)
	endif
c
c	ilast=k		! 1st interval to print for next call
c
	if(bad1.and.(ID.eq.4.or.ID.eq.6)) bad=.true.
c
c Calculate 'amplitude' for the burst just defined, if needed (needed
c for amp dist (id=12,13), or if conam1 or exass=true). Burst already
c excluded if it contains an unusable opening
	if((.not.bad).and.(id.eq.12.or.id.eq.13.or.conam1.or.
     & exass)) goto 523	!calc amp for burst
	goto 524		!skip amp calc
523	continue
	dubamp=.false.
	s=0.
	den=0.
	do 525 m=Ifst,k-1	!go through all intervals in bst
	   m1=m
	   amp=ampl(m1)
	   if(amp.eq.0) goto 525	!skip shut
	   if(dubious(m1)) dubamp=.true.	!true if ANY dubious amp found
	   if(ibamp.eq.2) goto 5231
	   s=s + amp		!accum amp
	   den=den+1.0
	   goto 525
5231	   s=s + tint(m)*amp
	   den=den + tint(m)
525	continue
c
	if(den.gt.0.01) goto 526
	call bell(4)
	print 527
527	format(' Burst with no openings found!')
	stop
526	amp=s/den		!define amp
c How to label burst amplitude as dubious now?!
	bampl(nbst)=amp		!amp (set dubious if nec)
c#	if(deb) print 617,nbst,ACAL*float(IACAL(nbst,ibampl))
617	format(' Mean amp for burst #',i4,' (pA) = ',G13.6)
c
c Exclude bursts as specified
	if(exass.and.dubamp) bad=.true.
	if(id.eq.12.or.id.eq.13) goto 524
	amp=bampl(nbst)
	if(conam1.and.((amp.lt.amplo).or.(amp.gt.amphi))) bad=.true.
524	continue
c If set bad then overwrite the burst that has just been completed
c If findgap=true then, after bad gap at end of file causes bad=true
c here, the gap that ends the overwriten burst is a valid start for
c the next one, so reduce ifst by one here to prevent it being skipped
	if(bad) then
	   Nbst=Nbst-1	!reverse update of Nbst
	   if(findgap) k=k-1	!so ifst is reduced by 1 at label 325
	   goto 325 	!Skip assignment of Yval; look for next bst
	endif
c 	IF(deb) print 4051,Ifst,K,Nbst,NOP,nsg,nas,nbs
c 4051	format(' End of burst: Ifst,K,Nbst,NOP,nsg,nas,nbs= ',/,7I8)
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
	if(ID.eq.1) YVAL(Nbst)=float(nop)
c	if(ID.eq.2) YVAL(Nbst)=float(nsg+1)
	if(ID.eq.2) YVAL(Nbst)=float(nbs+1)
	if(deb) call DEBTYP(0,-1,-1,tint,ampl,iprops,
     & 0,nbst,nbst,yval,prt,nintt,ndimy)	!print YVAL(j)
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
	if(deb) call DEBTYP(0,-1,-1,tint,ampl,iprops,
     & 0,nbst,nbst,yval,prt,nintt,ndimy)	!print YVAL(j)
c
C NOW ASSIGN YVAL FOR THOSE DISTRIBUTIONS (ID=-7,7,8,9,10,11) THAT MAY
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
	if(deb) call DEBTYP(0,-1,-1,tint,ampl,iprops,
     & 0,j2,j2,yval,prt,nintt,ndimy)	!print YVAL(j)
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
	if(deb) call DEBTYP(0,-1,-1,tint,ampl,iprops,
     & 0,j2,j2,yval,prt,nintt,ndimy)	!print YVAL(j2)
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
	if(deb) call DEBTYP(0,-1,-1,tint,ampl,iprops,
     & 0,jfst,j,yval,prt,nintt,ndimy)	!print YVAL(j)
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
	if(deb) call DEBTYP(0,-1,-1,tint,ampl,iprops,
     & 0,j2-1,j2,yval,prt,nintt,ndimy)	!print both YVAL(j)
	goto 338			!finished this bst
c
c Section for ID=12,13 -burst amp distribution
c now check for # number of ops/bst (and use j2 as index for yval)
c Also check length if req (tc1>0) (if no length check s=0. and tc1=-1.
c (see setdub) so s>tc1 always).
602	continue
c=	Yval(nbst)=bampl(nbst)
c=	if(id.eq.13) Yval(nbst)=-Yval(nbst)
	s=0.0
	if(tc1.gt.0.) then	!calc bst length and test it
	   do m=1,nAs
		s=s + ops(m)	!add open times for all
		if(m.lt.nAs) s=s + gaps(m)
	   enddo
	endif
	if((isg.eq.-10000.or.(isg.ge.0.and.nbs.eq.isg).or.
     &  	(isg.lt.0.and.nbs.ge.-isg)).and.s.gt.tc1) then
	   j2=j2+1				!valid burst
	   Yval(j2)=bampl(nbst)		!so Yval=mean burst amp
	   if(id.eq.13) Yval(j2)=-Yval(j2)
	else
	   goto 338		!skip this burst
	endif
	if(deb) then
	   call DEBTYP(0,-1,-1,tint,ampl,iprops,
     &	 0,nbst,nbst,yval,prt,nintt,ndimy)		!print YVAL(nbst)
	endif
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
c
c Addition 09/23/97 07:57am to print burst means (prev in cmean1)
c Get menas directly from open periods in last burst=ops(m), m=1,..,nas
c and gaps in last burst=gaps(m), m=1,...,nbs  (nbs=nas-1)
c plus number of open periods in the burst=nas ('burst with k open periods)
c NB This does not include excsub -to do this need 'amplitude' of open period
c   Assign each open period or gap twice, once for 'all bursts' and once
c   for 'bursts with k open periods
	if(id.eq.15) then
	   ndim=nmean+1
c	   do m=1,nAs
	   nA=nAS
	   if(nA.gt.nmax) nA=nmax
	   do m=1,nA
		i1=2*m-1	!index for 'any burst' =1,3,5,...
		call ASSIGN(avs,sds,ns,i1,ops(m),ndim)
		if(nAs.le.nmax) then
		   i2=i1+2*nmax-1+(nA-1)*(nA-1)   !index for bst with k=nas op periods
c eg when nAs=1, m=1 (only) so i1=1 for opening, and i2=i1+2*nmax-1 = 2*nmax
c NB for 'any burst' the nmax'th opening may be followed by a valid shut time
c (if nAs>nmax) or may not, but must exclude it with present numbering
c by using shuttings only up to nmax-1 (nA-1 in general, na=min(nAs,nmax)
c otherwise, for example, element avx(2*nmax) would get overwritten
		   call ASSIGN(avs,sds,ns,i2,ops(m),ndim)
		endif
c======	if(m.lt.nas) then
		if(m.lt.na) then
		   i1=i1+1	!=2,4,6,...
		   call ASSIGN(avs,sds,ns,i1,gaps(m),ndim)
		   if(nAs.le.nmax) then
			i2=i2+1
			call ASSIGN(avs,sds,ns,i2,gaps(m),ndim)
		   endif
		endif
	   enddo
	endif		!end of id=15
c
c Bit added 06/22/89 01:29pm to print total open, total shut and
c Popen for each burst when id=3, if ppopen=true.
	totop=0.
	totgap=0.
	do i=1,nas
	   totop=totop+ops(i)
	enddo
	if(nas.gt.1) then
	   totop1=0.0
	   do i=1,nas-1
		totop1=totop1+ops(i)
	   enddo
	endif
	do i=1,nbs
	   totgap=totgap+gaps(i)
	enddo
	blength=totop+totgap
	popen=totop/blength
	bleng1=totop1+totgap
	if(nas.gt.1) popen1=totop1/bleng1
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
	   else if(ipop.eq.1) then
	     if(nas.gt.1) then
	      nb14=nb14+1
	      Yval(nb14)=popen1
	     endif
	   else
	      nb14=nb14+1
	      Yval(nb14)=popen
	   endif
	   if(deb) call DEBTYP(0,-1,-1,tint,ampl,iprops,
     &  0,nb14,nb14,yval,prt,nintt,ndimy)		!print YVAL(nbst)
	endif
c
	pbst=(tc.lt.-0.001).and.ID.ge.3.and.ID.le.6.and.(Yval(Nbst).gt.
     &    abs(tc))		!print BURST
	pbst=pbst.or.listall.or.(Nbst.ge.N1.and.Nbst.le.N2).or.
     & bprint
	if(.not.pbst) then
	   if(ppopen) then	!print bst number since skipped if pbst=false
	      print 57, nbst
      	if(pon()) write(7,57) nbst
	      if(discprt) write(8,57) nbst
	   endif
	   goto 354		!skip print of intervals in burst
	endif
357	continue
	if((kplast.gt.0).and.(ifst-1.ge.kplast+1)) then
         print 54
         if(prt) write(7,54)
         if(discprt) write(8,54)
54	   format(/,' Intervals between bursts:')
	   do m=kplast+1,ifst-1
	      m1=m
            print 56,m,tint(m),ampl(m1)
            if(pon()) write(7,56) m,tint(m),ampl(m1)
            if(discprt) write(8,56) m,tint(m),ampl(m1)
c56	      format(6x,i6,2x,2g13.6)
56	      format(i6,2x,2g13.6)
	   enddo
	   print 108
	   if(prt) write(7,108)
	   if(discprt) write(8,108)
	endif
      print 57, nbst
      if(pon()) write(7,57) nbst
      if(discprt) write(8,57) nbst
57	format(' Burst #',i4)
	do m=ifst,k-1
	   m1=m
	   if(prt) write(7,356) m,tint(m),ampl(m1)
         if(discprt) write(8,356) m,tint(m),ampl(m1)
355	   print 356,m,tint(m),ampl(m1)
356	   format(i6,2x,2g13.6)
	enddo
	kplast=k-1		!index of last tint() printed
c	print 108
c	if(prt) write(7,108)
c	if(discprt) write(8,108)
354	continue
c
	if(ppopen) then
	   print 403,ifst,totop,nas,totgap,nbs,
     &	blength,popen
	   if(prt)write(7,403) ifst,totop,nas,totgap,nbs,
     &	blength,popen
	   if(discprt)write(8,403) ifst,totop,nas,totgap,nbs,
     &	blength,popen
c403	   format(' Burst #',i4,/,
403	   format(' Burst starts at interval # ',i9,/,
     &  ' Total open = ',g13.6,' (n = ',i4,');',
     &  ' Total shut = ',g13.6,'(n = ',i4,')',/,
     &  ' Burst length = ',g13.6,'   P(open) = ',g13.6)
c   Also print yval if a valid value is defined -need to define index for
c yval to do this (as in def of nyval below)
	   if(id.ge.1.and.id.le.6) index=nbst
	   if(ID.eq.7.or.id.eq.8.or.ID.eq.11.or.id.eq.12.or.id.eq.13) then
		index=j2
	   endif
	   if(id.eq.14) index=nb14
	   if((ID.eq.9.or.ID.eq.10).and.Jth.gt.0) index=j2
	   if((ID.eq.9.or.ID.eq.10).and.Jth.lt.0) index=j
	   if(index.ne.indlast) then
      	print 404, index,Yval(index)
	      if(pon()) write(7,404)  index,Yval(index)
      	if(discprt) write(8,404)  index,Yval(index)
404		format(
     &  ' Value for distribution: Yval(',i4,') = ',g13.6)
		indlast=index
	   endif
	endif
c
	Nsgap=Nsgap+NSG	!Nsgap=TOTAL NO OF SHORT GAPS IN CURRENT EXPT
	Nopen=Nopen+NOP
	NAset=NAset+nAs
	NBset=NBset+nBs
325	Ifst=k+1	!index of interval after that which ended last bst
	if(Ifst.le.nint) goto 360
c END of Ifst loop
C RECORDING OF VALUES FOR LAST BURST COMPLETED- GO TO 360 TO LOOK
C EXPLICITLY FOR FIRST OPENING OF NEXT BURST
C
C ALL BURSTS IDENTIFIED.
99	continue
	if(id.ge.1.and.id.le.6) Nyval=Nbst
c=	if(id.eq.12.or.id.eq.13) Nyval=Nbst
	if(id.eq.12.or.id.eq.13) Nyval=j2
	if(id.eq.14) Nyval=Nb14
	if(ID.eq.7.or.id.eq.8.or.ID.eq.11) Nyval=j2
	if((ID.eq.9.or.ID.eq.10).and.Jth.gt.0) Nyval=j2
	if((ID.eq.9.or.ID.eq.10).and.Jth.lt.0) Nyval=j
	if(ID.eq.1) obbar=float(Nopen)/float(Nbst)
	if(ID.eq.2) obbar=float(NAset)/float(Nbst)
c
	print 581, nbad1,nbad2
	if(prt) write(7,581)
	if(discprt) write(8,581)
581	format(/,
     & ' Number of bursts aborted by bad open time = ',i4,/,
     & ' Number of bursts aborted by bad shut time = ',i4,/)
	print 3281
3281	FORMAT(' Print burst parameters [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).EQ.'N') GOTO 4091
c
	if(onetcrit) then
	   if(prt) write(7,58) tcrit
	   if(discprt) write(8,58) tcrit
58	   format(' Critical gap length (ms)= ',g13.6)
	else
	   do i=1,nfile
		if(prt) write(7,59) i,tcvals(i)
		if(discprt) write(8,59) i,tcvals(i)
59		format(' File # ',i3,': critical gap length (ms)= ',g13.6)
	   enddo
	endif
	if(prt) write(7,328) NOPEN,nsgap,NASET,nBset,NBST,
     & FLOAT(NOPEN)/FLOAT(NBST),FLOAT(NASET)/FLOAT(NBST),
     & FLOAT(nBset)/FLOAT(NBST)
      if(discprt) write(8,328) NOPEN,nsgap,NASET,nBset,NBST,
     & FLOAT(NOPEN)/FLOAT(NBST),FLOAT(NASET)/FLOAT(NBST),
     & FLOAT(nBset)/FLOAT(NBST)
	print 328,NOPEN,nsgap,NASET,nBset,NBST,
     & FLOAT(NOPEN)/FLOAT(NBST),FLOAT(NASET)/FLOAT(NBST),
     & FLOAT(nBset)/FLOAT(NBST)
328	FORMAT(/,
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
	if(id.eq.15) then
c    Complete calc of means and sd, and print the means etc
	   S1=0.		!USE TO CALC OVERALL MEAN OP AND GAP
	   SS1=0.
	   S2=0.
	   SS2=0.
	   N1=0
	   N2=0
	   do j=1,nmax
		i1=2*j-1
		i2=i1+1
		S1=S1+avs(i1)
		SS1=SS1+sds(i1)
		N1=N1+ns(i1)
		if(j.lt.nmax) then
		   S2=S2+avs(i2)
		   SS2=SS2+sds(i2)
		   N2=N2+ns(i2)
		endif
	   enddo
	   S1=S1/float(N1)
	   SS1=sqrt(SS1/float(N1-1))	!SD NOT QUITE RIGHT
	   S2=S2/float(N2)			!-SHOULD CALC WITH OVERALL MEAN!
         SS2=sqrt(SS2/float(N2-1))
C Value for 1st opening in any burst when bursts with one opening
C are excluded (sum of latter in avs(2nmax))
	   yv1=avs(1)-avs(2*nmax)	!SUM
	   n1=NS(1)-NS(2*nmax)		!N
	   yv1=yv1/float(n1)		!MEAN
C
	   do i=1,nmean
C Convert to mean, sd
		if(ns(i).gt.1) then
		   en=float(ns(i))
		   avs(i)=avs(i)/en
		   sds(i)=sqrt(sds(i)/(en-1.0))
		else
		   avs(i)=0.		!if n<2
		   sds(i)=0.
		endif
	   enddo
c
c Print values for 'any burst'
	   print 802
	   if(pon()) write(7,802)
	   if(discprt) write(8,802)
802	   FORMAT(/,' Jth interval in any burst',/)
	   print 8021
	   if(pon()) write(7,8021)
	   if(discprt) write(8,8021)
8021	   FORMAT(T11,' Open period',T51,' Gaps',/,
     & '    J',T8,' Mean        SD         N  ',T44,
     & ' Mean        SD         N  ')
c
	   do j=1,nmax-1		!ALL LINES BUT LAST
		i1=2*j-1
		i2=i1+1
		if(pon()) write(7,804)J,AVS(i1),SDS(i1),NS(i1),AVS(i2),
     &    SDS(i2),NS(i2)
	      if(discprt) write(8,804)J,AVS(i1),SDS(i1),NS(i1),AVS(i2),
     &    SDS(i2),NS(i2)
		print 804,J,AVS(i1),SDS(i1),NS(i1),AVS(i2),SDS(i2),NS(i2)
		if(j.eq.1) then
		   print 626,j,yv1,n1
		   if(pon()) write(7,626) j,yv1,n1
      	   if(discprt) write(8,626) j,yv1,n1
626		   format(2x,i3,g11.4,12x,i5,'  (Exc k = 1)')
		endif
	   enddo
C
	   i1=2*nmax-1	!LAST LINE
	   if(pon()) write(7,805)nmax,AVS(i1),SDS(i1),NS(i1)
         if(discprt) write(8,805)nmax,AVS(i1),SDS(i1),NS(i1)
	   print 805,nmax,AVS(i1),SDS(i1),NS(i1)
804	   format(2X,I3,2G11.4,1X,I5,8X,2G11.4,1X,I5)
805	   format(2X,I3,2G11.4,1X,I5)
	   print 8041,S1,SS1,N1,S2,SS2,N2
	   if(pon()) write(7,8041)S1,SS1,N1,S2,SS2,N2
         if(discprt) write(8,8041)S1,SS1,N1,S2,SS2,N2
8041	   format(/,' Mean',2G11.4,1X,I5,8X,2G11.4,1X,I5)
C
C print RESULTS FOR BURSTS WITH K OPEN PERIODS PER BURST
	   print 806
	   if(pon()) write(7,806)
	   if(discprt) write(8,806)
806	   FORMAT(/,/,' Jth interval in burst with K open periods',/)
	   print 8021
	   if(pon()) write(7,8021)
	   if(discprt) write(8,8021)
C
	   do k=1,nmax
		print 808,k
		if(pon()) write(7,808) k
      	if(discprt) write(8,808) k
808  		format(/,' k = ',I2)
		do j=1,k
		   i1=2*nmax+(k-1)**2+2*j-2
		   i2=i1+1
		   if(j.lt.k) then
			if(pon()) write(7,804)J,AVS(i1),SDS(i1),NS(i1),
     & 		 AVS(i2),SDS(i2),NS(i2)
      		if(discprt) write(8,804)J,AVS(i1),SDS(i1),NS(i1),
     & 	   	 AVS(i2),SDS(i2),NS(i2)
			print 804,J,AVS(i1),SDS(i1),NS(i1),
     &		 AVS(i2),SDS(i2),NS(i2)
		   else	!last line
		      print 805,J,AVS(i1),SDS(i1),NS(i1)
		      if(pon()) write(7,805)J,AVS(i1),SDS(i1),NS(i1)
      	      if(discprt) write(8,805)J,AVS(i1),SDS(i1),NS(i1)
		   endif
		enddo		!next j
	   enddo		!next k
	   pause
	   DEALLOCATE(avs,sds,ns)
	endif		!end of printing for idtype=15
c
	if(ibtype.ne.1) goto 207
	if(.not.debug().or.ibtype.ne.1) goto 207
	print 622
622	format(' Write Yval to disc for test [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).ne.'Y') goto 207
C 2*2560 real=2*5120 words=40 blocks on DK:
      OPEN(unit=14,file='EKDIST.DAT',status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=10240)
	write(14,rec=1) (Yval(i),i=1,2559),nyval
	close(unit=14)
C
C YVAL AND NYVAL HAVE NOW BEEN DEFINED.
207	CONTINUE
	RETURN		!rest done in SETBIN
	END

	subroutine ASSIGN(avs,sds,ns,m,yval,ndim)
	dimension AVS(ndim),SDS(ndim)
	dimension NS(ndim)
C
c Accumulates Yval into avs(m) and sds(m), and increments ns(m)
	ns(m)=ns(m)+1
	n=ns(m)
c
	if(n.eq.1) then
	   avs(m)=Yval	!initialise for single pass method
	   sds(m)=0
	else
	   avs(m)=avs(m)+Yval	!accum sum and SSD
	   en=float(n)
	   SDs(m)=SDs(m)+(en*Yval-avs(m))**2/(en*(en-1.0))
	endif
	RETURN
	END




