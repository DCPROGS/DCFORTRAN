	subroutine CDIST2(tint,ampl,iprops,nint,nintt,ndimy,avamp,acrit,
     & Yval,Nyval,iscan,tcrit,tc,nbst,obbar,ohist,bhist,ID,
     & ghist,ahist,pahist,obhist,pophist,iexc,ISG,
     & treso,tresg,tres,trise,revamp,idsav,ibamp,ibtype,ipop,ibmin,
     & bmin,nfile,iexstrt,iexend,onetcrit,tcvals,index,
     & tint0,consamdef,adcfil,nfits,ifits,timsav,ibaselin,dfinter,
     & sfac2,npfilt,qfile,nsam,srate,irecfst,calfac,ioffset,
     & nset,jset,nmax)
c
c To define Yval() for burst distributions
c Version for new EKDIST (for new SCAN) (prev called cdist3) 02/15/94 06:59pm
c
c NB burst display does not work for simulated data at the moment, because
c timsav, nfits etc not read in diskin1 for simulated .scn files
c
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
c Modif 01/14/05 12:10pm so ops(), gaps() now allocatable, not fixed at 5000
c    corrected 02/23/05 03:39pm for premature deallocation of above!
c
c Major modif 12/28/00 06:57pm to use 2D arrays for tint() etc, for
c case where nset>1.  Parameters added jset, nset, nmax
c
c  Modif 06/30/00 12:01pm so that can use 'Popen without last opening' in
c   addition to other constraints
c
c Modif 05/05/99 05:20pm so that when bursts are rejected in VSAMP, the
c number of the burst is recorded and another burst distribution can
c be done (without leaving cdist2) with the same bursts omitted.
c The variable omitbst=.true. (and dispbst=.false.) for runs after the
c first in which bursts are omitted autonatically.
c
c Modified 04/06/99 08:25am for display of bursts in VSAMP (params index, tint0
c  to npfilt added for this)
c Modified 03/21/99 11:32am
c 'Exclude time in sublevels' option now obsolete (with acrit=0., the default
c value, everything is a sublevel). This option now removed (iexc=2 always for
c distributions id=4 and id=6 -these do not use excsub which is now always
c false and is superfluous too -previously it was an option for ibtype=1 and
c id=7,8,10,11)
c
c Modified 09/25/97 10:52pm to include, as option 15, the burst means
c previously found in cmean1
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
	allocatable::bampl,ibomit
	real*4 bampl(:)		!for burst amps
	integer*4 ibomit(:)	!=0 if burst include, =1 if omitted
c	real*4 bampl(10240)		!for burst amps
c	dimension ops(5001),gaps(5000)	!temp store for 1 burst
c	dimension isubs(5000)
	allocatable::ops, gaps,isubs
	real*4 ops(:),gaps(:)
	integer*1 isubs(:)
	character*1 ans,UC
	LOGICAL OHIST,BHIST,GHIST,AHIST,pahist,OBHIST,BPRINT,pophist
	LOGICAL conamp,exass,DEBUG,OPEN,shut,SUB,FULL,BAD,BAD1,EXCSUB
	logical ppopen,pbst,badlast,skipshort,boxdef
	logical sbin,shist,sres,sexp,pon,prt,deb
	logical dubious,badgap,btest,dubamp,revamp,exctwo
	logical onetcrit,listall,newfile,pflag,findgap,badend
	logical discprt,slock,caplock,usedef
	logical omitbst,present,allocated,dflag
	logical dub
c
	real*4 tcvals(50),tcvals1(50)
	integer iexstrt(50),iexend(50)
c Arrays for burst mean (previously in sep subroutine, cmean1)
	ALLOCATABLE::avs,sds,ns
	real*4 avs(:),sds(:)
	integer ns(:)
c For display in VSAMP
	real*4 TINT0(nintt)
	integer*4 index(nintt)
	real*8 timsav(nfits),dfinter
	integer*4 ifits(nfits)
	integer*2 ibaselin(nfits)
	character*33 adcfil
	character line1*70,line2*70,line3*70,cnum*11,cnum1*11
	logical consamdef,dispbst,colseq,reject,stop
	character*40 qfile
	integer*4 videotyp
c For getcons
	character cdate1*11,adctim1*8,title*76		!read from consam
	logical noconsam,newform,cjdat
c For bstomit.dat
	logical exass1,findgap1,badend1,conam1,nopath
	character adcfil1*33,afile*12,afile1*12,path*30,ndev*2
	character pname*8,pname1*8,suffix*3,suffix1*3
c
c
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi
	common/dp/discprt
	common/sblk/sbin,shist,sres,sexp
	COMMON/OVLBLK/OBMAX,JTH,conamp,exass,exass1,AMPHI,AMPLO,
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
	ALLOCATE(bampl(50000))
	if(shist.or.jset.gt.1) then
	   id=idsav
c	   tcrit=abs(tcrit)	!switch off debugs 2nd time
	   listall=.false.
	   tc=abs(tc)
	   ppopen=.false.
c      must ask for tcrit separately for each set when nset>1
	   if(nset.eq.1) goto 303		!same histo print (and tcrit??)
	else
	   OHIST=.FALSE.
	   BHIST=.FALSE.
	   GHIST=.FALSE.
	   AHIST=.FALSE.
	   pahist=.false.		!point amp histo
	   pophist=.false.
	   revamp=.false.		!point amp histo
	   OBHIST=.FALSE.		!DIST=OPENINGS/BURST
	   ppopen=.false.
	   ipop=0	!flag
	   omitbst=.false.
	endif
c
C NOW DEFINE ARRAY YVAL CONTAINING VARIABLES FOR WHICH
C HISTOGRAM IS TO BE PLOTTED
	if(nfile.eq.1) then
	   onetcrit=.true.
c	   if(jset.gt.1) then
		tcrit=tcvals(1)		!default
c	   endif
	   if(tcrit.lt.0.) then		!first time
		print 321
321		format(/,
     &	' Critical gap length, tcrit, to define bursts (ms) = ')
		call INPUTr(tcrit)
		tcvals(1)=tcrit
	   else
		print 320,tcrit
320		format(/,
     &  ' Critical gap length, tcrit, to define bursts [',f7.2,'ms] = ')
		call INPUTr(tcrit)
		tcvals(1)=tcrit
	   endif
	else
	   ans='N'
	   if(onetcrit) ans='Y'
	   call DCASK('Critical gap length same for all files',ans,ans)
	   onetcrit=ans.eq.'Y'
	   if(tcrit.lt.0.) then		!first time
		if(onetcrit) then
		   print 33
33		   format(' Critical gap length for all files (ms) = ')
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
		   call INPUTr(tcrit)
		   do i=1,nfile
			tcvals(i)=tcrit
		   enddo
		else
		   do i=1,nfile
			print 32,i,tcvals(i)
32			format(
     &       ' File #',i3,': critical gap length (ms) [',f7.2,' ms] = ')
			call INPUTr(tcvals(i))
		   enddo
		endif
	   endif
	endif
c Can calc this in RESINT which calls TCSUB
c
	if(jset.gt.1) goto 303		!defaults same for all sets
c
	print 133
133	format(/,
     & ' DEFAULTS FOR DEFINITION OF BURSTS',/,
     & ' (1) Bursts include ANY open state separated by shut periods',/,
     & '    that are all shorter than tcrit (normal definition)',/,
     & ' (2) ''Burst amplitude'' defined as:',/,
     & '       mean current (excluding shut periods) during burst',/,
     & ' (3) Bursts with any ''assumed'' amps are INCLUDED',/,
     & ' (4) Require a gap > tcrit before the 1st burst in each file',/,
     & ' (5) Unusable shut time NOT a valid end of burst',/,
     & ' (6) No listing of the individual bursts',/,/,
     & ' Use ALL of the above defaults [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	usedef=.false.
	if(ans.eq.'Y') then
	   usedef=.true.
	   ibtype=1
	   ibamp=1
	   exass=.false.
	   findgap=.true.
	   badend=.false.
	   n1=-1		!defaults (no debug printing)
	   n2=-1
	   TC=0.
	   kplast=0
	   ppopen=.false.
	   goto 134
	endif

c
	print 417
417	format(/,' Options for definition of bursts:')
	print 4171
4171	format(
     &' (1) Bursts include ANY open state separated by shut periods',/,
     & '    that are all shorter than tcrit (normal definition).')
	print 4172
4172	format(
     & ' (2) Bursts consist of any series of openings to a specified',/,
     & '    amplitude (range), these openings being separated by',/,
     & '    periods in ANY other level (open or shut) that is',/,
     & '    (a) not in the specified amplitude range, and',/,
     & '    (b) has total duration < tcrit',/,
     & '    (''50pS/non-50pS'' print definition in which all amps',/,
     & '    outside range are treated as ''shut'').')
	print 4173
4173	format(
     &' (3)Bursts consist of alternations between open (in specified',/,
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
303	continue
	n1=-1		!defaults (no debug printing)
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
	goto(1121,1131,1141,1151) J
	if(discprt) goto(1121,1131,1141,1151) J
	goto 3216
1121	write(8,112)
	goto 3216
1131	write(8,113)
	goto 3216
1141	write(8,114)
	goto 3216
1151	write(8,115)
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
	nbst=0		!no of bursts (some may have been omitted)
	nbst0=0		!no of bursts (total number of bursts without omissions)
	nblast=0			!for reject option
C
C	NOW SELECT TYPE OF BURST DISTRIBUTION WANTED
C NOTE THAT IF THERE ARE NO SUBLEVELS 1 AND 2 SHOULD BE THE SAME
C (COULD ALSO CALC NO OF FULL OPENINGS/BST, AND NO OF SUBLEVELS/BST)
C ALSO 3,4 SAME AND 5,6 SAME
134	continue	!jump here to print if defaults accepted
	if(shist) goto 304
	if(jset.gt.1) goto 304
416	continue
	print 500
500	format(/,' OPTIONS FOR DISTRIBUTION TYPE:')
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
     & ' (7) Burst length for bursts with K open periods')
	print 508
508	FORMAT(
     & ' (8) Open time/bst for bursts with K open periods')
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
	print 516
516	format(' (16) Length of the last open period in burst')
	print 517
517	format(' (17) Length of the last gap in burst')
c NB ID=7,-7,8,9,10 are now ID=7,8,9,10,11
	print 116	!option no
	call INPUTi(ID)
	if(id.lt.1.or.id.gt.17) GOTO 416
	if(id.eq.1) then
	   print 2
2	   format(
     &   ' There may be many small amplitude transitions during one',/,
     &   ' ''opening'', each of which will count as an individual ',/,
     &   ' opening, so generally better to look at ''open periods''')
	   ans='N'
	   call DCASK(
     &   'Are you sure you want individual openings per burst',ans,ans)
	   if(ans.eq.'N') goto 416
	endif
	if(id.eq.17) then
	   exctwo=.false.
	   ans='N'
	   call DCASK(
     &   'Exclude bursts that have only one shut time',ans,ans)
	   exctwo=ans.eq.'Y'
	endif
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
	if(.not.usedef) then
	   ans='N'
	   call DCASK(
     &   'Find a gap > tcrit before the first burst in each file',
     &	ans,ans)
	   findgap=ans.eq.'Y'
c
	   ans='N'
	   call DCASK(
     &   'Treat an unusable shut time as a valid end of burst',ans,ans)
	   badend=ans.eq.'Y'
	endif
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
	else if(id.eq.16) then
		print 516
		if(pon()) write(7,516)
		if(discprt) write(8,516)
	else if(id.eq.17) then
		print 517
		if(discprt) write(8,517)
	endif
	print 1080				!dashed line
	if(pon()) write(7,1080)
	if(discprt) write(8,1080)
	if(nfile.eq.1) then
	   ans='N'
	   call DCASK(
     & 'Display bursts (only if raw data file present)',ans,ans)
	   dispbst=ans.eq.'Y'
	else
	   dispbst=.false.
	endif
	skipshort=.false.
c
117	OBHIST=id.le.2
	BHIST=(id.ge.3.and.id.le.11).or.id.eq.16.or.id.eq.17
	pophist=id.eq.14		!true for fitting Gau to Popen/bst
	AHIST=id.eq.12.or.id.eq.13
	tres=treso
	if(id.eq.9) tres=tresg
	if(shist) goto 2251    	!^^?omit if conam etc to be asked for
	if(jset.gt.1) goto 2251
	amplo=-10000.0
	amphi=10000.	!include openings of any amp
	yval1=-1001.
	if(.not.usedef) then
	   ppopen=.false.
	   print 405
405	   format(' Type P(open), Yval etc for each burst [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	   if(UC(ans).eq.'Y') ppopen=.true.
	endif
c
	conamp=.FALSE.
	if(id.eq.12.or.id.eq.13) goto 520
	if(ibtype.gt.1.or.id.eq.4.or.id.eq.6) then
	   conamp=.true.
	   goto 619
	endif
c	if(id.eq.4.or.id.eq.6) goto 619
	print 226
226	format(' Specify amplitude range for bursts [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).EQ.'Y') conamp=.TRUE.
619	if(conamp) then
	   print 227
227	   FORMAT(/,' Define amplitude range:',/,
     & ' Low amp, high amp (real pA with sign)= ')
	   call INPUT2r(amplo,amphi)
	   if(amphi.lt.amplo) then
		a1=amphi	!swap
		amphi=amplo
		amplo=a1
	   endif
         if(discprt) write(8,705)amplo,amphi
705	   format(' Amplitude range from ',g13.6,' to ',g13.6)
	endif
	iexc=0
c Sublevel option now obsolete (with acrit=0., the default value, everything is
c a sublevel)
	if(id.eq.4.or.id.eq.6) then
	   iexc=2		!always -so now superfluous
	   print 703
	   if(pon()) write(7,703)
	   if(discprt) write(8,703)
703	   format(
     & ' Exclude time spent in openings with amplitudes outside',
     & ' specified range')
c	   print 702
c702	   format(' (1) Exclude time spent in sublevels')
c	   print 703
c703	   format(
c     & ' (2) Exclude time spent in openings with amplitudes outside',
c     & ' specified range')
c	   print 704
c704	   format(' Option no = ')
c	   call INPUTi(iexc)
c	   if(pon().and.iexc.eq.1) write(7,702)
c	   if(discprt.and.iexc.eq.1) write(8,702)
c	   if(pon().and.iexc.eq.2) write(7,703)
c	   if(discprt.and.iexc.eq.2) write(8,703)
c	   if(iexc.eq.1) goto 2262
	endif
c701	continue
c
c
2262	continue
	exass=.FALSE.
	if(.not.usedef) then
	   if(iscan.eq.-1003.or.iscan.ge.100) then
		print 2263
2263		format('&Exclude bursts with any ''dubious'' amps [N] ? ')
	   else
		print 2261
2261		format('&Exclude bursts with any ''assumed'' amps [N] ? ')
	   endif
	   ans='N'
	   call INPUTa(ans)
	   exass=UC(ans).eq.'Y'
	endif
c	if(.not.conamp) goto 2251	!define ibamp for print whether conam or not
c
c Need to specify how 'burst amp' to be defined (a) for option 12
c and (b) if restricted amplitude range to be used for other options
520	continue
	if((.not.usedef.and.ibtype.eq.1)) then
	   if(id.eq.12.or.conamp) then
		print 521
521		format(' (1) ''Burst amplitude'' defined as:',/,
     & '  mean current (excluding shut periods) during burst')
		print 522
522		format(' (2) ''Burst amplitude'' defined as:',/,
     & '  mean of the amps fitted to each opening of burst')
		print 30
30		format(' Option number [1] = ')
		ibamp=1	!default
		call INPUTi(ibamp)
	   endif
	endif
2251	CONTINUE
	call PDUB(exass,shist,iscan,pon())
      if(conamp.and.discprt) write(8,25)amplo,amphi
	if(conamp.and.shist) print 25,amplo,amphi
25	format(' ''Amplitude'' range for bursts from ',f7.3,' to ',
     & f7.3,' pA only')
	if(discprt.and.ibamp.eq.1) write(8,521)
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
     &'&Use bursts with a specified number of open periods [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	   if(UC(ans).ne.'Y') goto 331
	endif
	if(shist) goto 305
	if(jset.gt.1) goto 305
	print 332
332	FORMAT('&Number of open periods, k = ')
	call INPUTi(i)
	ISG=i-1		!=no of short gaps
c NB if specify 1 opening/bst (isg=0) then no point in asking for
c '1 or more openings'=ANY burst!
	if(id.eq.11.or.isg.eq.0) goto 305
	print 3321,isg+1
3321	format(
     & '&Include bursts with MORE than ',i3,
     & ' open periods also [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).eq.'Y') ISG=-ISG
305	if(isg.ge.0.and.shist) print 118,isg+1
	if(isg.lt.0.and.shist) print 119,-isg+1
	if(pon().and.isg.ge.0) write(7,118)isg+1
      if(discprt.and.isg.ge.0) write(8,118)isg+1
118	format(/,' For bursts with ',i4,' open periods exactly.',/)
	if(pon().and.isg.lt.0) write(7,119) -isg+1
      if(discprt.and.isg.lt.0) write(8,119) -isg+1
119	format(/,' For bursts with ',i4,' or more open periods.',/)
	GOTO 337
C
331	if(iabs(ID).lt.9.or.ID.gt.11) GOTO 337	!ie for id=9,10,11
	if(id.ne.11) goto 606
	if(shist) goto 608
	if(jset.gt.1) goto 608
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
	if(jset.gt.1) goto 306
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
     &'&Use bursts with a specified number of open periods [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).eq.'N') GOTO 337
	goto 3322	!get ISG value
C
c Option to exclude time in sublevels for ibtype=1 and id=7,8,10,11
337	EXCSUB=.FALSE.
c	if(shist) goto 307
c	if(ibtype.eq.1.and.(id.eq.7.or.id.eq.8.or.
c     & id.eq.10.or.id.eq.11)) goto 3301
c	goto 330	!skip with excsub=false
c3301	print 336
c336	FORMAT('&Exclude sublevel durations [N] ? ')
c	ans='N'
c	call INPUTa(ans)
c	if(UC(ans).EQ.'Y') EXCSUB=.true.
c307	if(id.eq.10.or.id.eq.11) goto 624
c	if(shist.and.excsub) print 123
c	if(pon().and.excsub) write(7,123)
c	if(discprt.and.excsub) write(8,123)
c123	format(' Sublevel durations excluded')
c	goto 330
cc diff printout for id=10,11
c624	if(shist.and.excsub) print 1231
c	if(pon().and.excsub) write(7,1231)
c	if(discprt.and.excsub) write(8,1231)
c1231	format(
c     & ' Open periods with any sublevels disqualify whole burst')
cC N.B.EXCLUSION OF SUBLEVELS MEANS EXCLUSION OF THEIR DURATION. FOR
cC ID=9,10 JTH OPENING MEANS OPEN PERIOD PRECEDING JTH GAP WHETHER
cC EXCSUB TRUE OR NOT
C
c330	continue
	if(id.eq.14) then
	   ipop=0	!flag
	   if(bmin.lt.0.00001) bmin=0.0	!default 1st time
	   if(ibmin.eq.0) ibmin=2		!default
	   if(shist) then
		print 108
		if(pon()) write(7,108)
		if(discprt) write(8,108)
	   endif
c
	   print 19
19	   format(
     &	' Calculate Popen omitting last opening (so there are',/,
     &	'    equal numbers of open and shut times)',/,
     &	'  O.K [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(ans.eq.'Y') then
	      ipop=1	!flag
		if(pon()) write(7,241)
		if(discprt) write(8,241)
241		format(/,
     &	' NB Popen CALCULATED OMITTING LAST OPENING!',/)
	   endif
c
	   i=1
	   if(ipop.eq.1) i=2
	   print 20,i
20	   format('&(1) Include all bursts',/,
     &	' (2) Include only bursts longer than specified duration',/,
     &	' (3) Include only bursts with n or more open periods',/,
     &	' Option number [',i2,'] = ')
	   call INPUTi(i)
	   iopt=1
	   if(i.ge.1.and.i.le.3) iopt=i
	   if(iopt.eq.1) then
		bmin=0.0	!none excluded (none<0.0)
	      ibmin=0	!none excluded
	   else if(iopt.eq.2) then
	      ibmin=0
		print 22,bmin
22		format('& include bursts longer than t (ms) [',f8.3,'] = ')
		call INPUTr(bmin)
		if(pon()) write(7,23) bmin
		if(discprt) write(8,23) bmin
23		format(' Only bursts longer than',g12.4,' ms included')
	   else if(iopt.eq.3) then
		bmin=0.0
		print 21,ibmin
21		format('& include bursts with n or more open periods:',
     &	 ' n [',i4,'] = ')
	      call INPUTi(ibmin)
		if(i.le.1) ibmin=0
		if(pon()) write(7,24) ibmin
		if(discprt) write(8,24) ibmin
24		format(
     &	' Only bursts with ',i2,' or more open periods included')
	   endif
	   if(dispbst.and.(iopt.eq.2.or.iopt.eq.3)) then
	      ans='Y'
	      call DCASK(
     & 'Display bursts only if above specified length)',ans,ans)
		skipshort=ans.eq.'Y'
	   endif
c
         print 26
	   if(pon()) write(7,26)
	   if(discprt) write(8,26)
26	   format(/,
     &' NB: the mean P(open) calculated here is a ''length-biassed''',/,
     &' estimate (e.g. every isolated short opening contributes 1.0)',/,
     &' Mean open time per burst/mean burst length usually better.')
	   print 108
	endif
c
c All params for defining bursts now defined -check whether any omissions
c have been written to disc
	INQUIRE(file='bstomit.dat',exist=present,flen=nlen)
630	if(present.and.nlen.gt.0) then
	   OPEN(unit=11,file='bstomit.dat',status='OLD',
     &	access='TRANSPARENT')
	   read(11,rec=1) nbomit,adcfil1,ibtype1,ibamp1,exass1,findgap1,
     &     badend1,conam1,amplo1,amphi1,nfile1,tcvals1,nbst01
	   if(nbomit.eq.0) goto 362
	   if(ibtype.ne.ibtype1) goto 362
	   if(ibamp.ne.ibamp1) goto 362
	   if(exass.neqv.exass1) goto 362
	   if(findgap.neqv.findgap1) goto 362
	   if(badend.neqv.badend1) goto 362
	   if(conamp.neqv.conam1) goto 362
	   if(conamp) then
	      if(amplo.ne.amplo1) goto 362
	      if(amphi.ne.amphi1) goto 362
	   endif
	   if(nfile.ne.nfile1) goto 362
	   do i=1,nfile
		if(tcvals(i).ne.tcvals1(i)) goto 362
	   enddo
	   call PARSNAME(adcfil,path,ndev,pname,suffix,nopath,33)
	   call PARSNAME(adcfil1,path,ndev,pname1,suffix1,nopath,33)
	   afile=charnb(pname)//'.'//charnb(suffix)
	   afile1=charnb(pname1)//'.'//charnb(suffix1)
	   if(afile.ne.afile1) goto 362
c       Everything matches so read omissions
	   ALLOCATE(ibomit(nbomit))
	   read(11,rec=513) (ibomit(i),i=1,nbomit)
	   print 363,nbomit
363	   format(
     &' Previously, ',i4,' bursts were omitted -omit same again [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   omitbst=ans.eq.'Y'
	   if(omitbst) jb=1		!to count omissions
362	   continue
	   CLOSE(unit=11)
	endif
c If omissions from disk not used, then allocate ibomit() here so it can
c be written to disk for future
	if(.not.omitbst) then
	   nbomit=0
	   if(dispbst) then
		if(allocated(ibomit)) DEALLOCATE(ibomit)
		ALLOCATE(ibomit(50000))
	   endif
	endif
c
c
c Now define bursts:
	Ifst=1
	nbst=0		!no of bursts (some may have been omitted)
	nbst0=0		!no of bursts (total number of bursts without omissions)
	Nb14=0		!no of bursts for id=14 (< Nbst if bmin or ibmin used)
	Nsgap=0		!NO OF GAPS-WITHIN-BST
	Nopen=0		!NO IF INDIV OPENINGS
	NAset=0		!NO OF OPEN (SUBSET A) PERIODS
	NBset=0		!NO OF GAP (SUBSET B) PERIODS
	nbad1=0		!number of bursts aborted by bad open time
	nbad2=0		!number of bursts aborted by unusable shut time
	nbshort=0		!no of bsts rejected because too short or too few ops
	J2=0		!COUNTS YVAL FOR ID=7,8,9,10,11,12,13; may be <nbst
	J=0		!DITTO FOR ID=9,10 IF JTH NEGATIVE
	indlast=0	!to control printing below
	prt=pon()
c
      print 80
80	format(/,
     & ' Now get values for the distribution',/,
     & ' To see the values (''Yval'') that go into distribution,',
     & ' put CAP LOCK on now',/,
     & ' (press any key to continue)')
	call ANYKEY()
c Allocate arrays ops(), gaps(),isubs() and if nasmax not big enough,
c deallocate, make bigger and start again
	nasmax=5000		!initially
700	ALLOCATE(ops(nasmax),gaps(nasmax),isubs(nasmax))
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
c Define pflag so 'new file' does not print again when still looking
c for first opening of burst
	pflag=.true.
c
c Loop for each burst starts at label 360
c Go into graphics mode before loop starts unless debug is on
	if(dispbst.and.(.not.consamdef)) then
	   cjdat=.false.
c Get consam/axon file name
	   call GETCONS(adcfil,noconsam,title,cdate1,adctim1,nsam,
     &    srate,cjdat,irecfst,newform,ioffset,calfac,ffilt,idest)
	   if(noconsam) then
		dispbst=.false.
		goto 360
	   else
		dfinter=1.d6/dble(srate)	!microsec
	   endif
	endif
	if(dispbst) then	!whether or not consamdef=true
	   if(.not.debug()) call MODE(18)
	   consamdef=.true.
	   call gsetcols(0)
	   call errswi(-1)
	   call brkswi(1)
	   call chaswi(1)
	   call grfmod (1)
	   call harcha
	   call papenq(xp,yp,ipap)
c Want to set graphboundary to correspond to data window, bottom of which
c is at iyd1=127 pixels, so need graph units which correspond to this
c	   ix=0
c	   call PIXGRA(ix,iyd1,xg,yg)
	   vxlo=0	! for VIEWPORT
	   vxhi=xp
	   vylo=0
c	   vylo=yg
	   vyhi=yp
	   xlo=0		! for graphboundary
	   xhi=xp
	   ylo=0
	   yhi=yp
c	   xlo=0.05*xp			! set display location on screen
c	   xhi=0.95*xp
c	   ylo=0.05*yp
c	   yhi=0.8*yp	!lower to leav room for dialog box
c	   call mode(18)
c	   icf=7		!frame colour for dialog box 1
c	   ict=11	!text colour for dialog box 1
c	   if(.not.debug()) call MODE(18)
c	else if(dispbst) then	!consamdef=true
	   if(.not.debug()) call MODE(18)
	endif
c Loop for each burst starts at label 360
	badlast=.false.
	boxdef=.false.		!dialog box not defined until after 1st VSAMP call
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
			pflag=.true.
			iflast=ifile
		   endif
		   goto 34
		endif
	   enddo
	endif
	if(newfile.and.pflag) then
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
	pflag=.false.		!so 'new file' does not print again
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
	Nbst0=Nbst0+1	!Update total # of bursts- reversed below if bst is bad
	BAD=.false.	!current burst is usable
	BAD1=.true.
C    For ID=4,6 must have at least one full opening,or get rubbish -BAD1
C     checks this.
	if((id.eq.4.or.id.eq.6).and.iexc.eq.2) FULL=(amp.ge.amplo)
     & .and.(amp.le.amphi)
	if(FULL) bad1=.false.	!at least one full opening in burst
	SUB=open.and.(.not.full)
	if(.not.sub) goto 44
	isub=1		!this open period contains a sublevel
c	if(excsub) goto 48			!omit sub -always false now
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
c=	full=abs(amp-avamp).le.acrit
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
c If NOT badend, so normally a bad gap is not treated as valid end of
c burst, then ask separately about the last interval (always set bad)
	if(shut.and.(.not.badend).and.k.eq.nint.and.badgap(k)) then
	   print 72
72	   format(/,
     & ' The last interval is set ''bad'':',/,
     & ' Should it be treated as a valid end for the last burst [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(ans.eq.'Y') goto 324	!accept last burst
	endif
	if(shut) then
	   if(badend) then
c=====	if(tgap.gt.tcrit.or.(badgap(k).and.k.lt.nint)) goto 324
		if(tgap.gt.tcrit.or.(badgap(k).and.k.le.nint)) goto 324
	   else
		if(badgap(k).and.k.lt.nint) then
		   nbad2=nbad2+1
		   bad=.true.	!abort burst
c		   call BELL(1)
		   if(VIDEOTYP().eq.3) then
		      print 71,k
		   else if(boxdef) then	  !wdialog defined only after 1st vsamp call
		      call WDIALOG(1,'Bad gap aborts burst',12)	!as in VSAMP
		   endif
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
c NB the number of intervals is now reduced by one when the last interval
c is set bad, so '.or.(k.eq.nint)' causes last burst/cluster to be omitted
c=	if((open.and.badgap(k)).or.(k.eq.nint)) then
	if(open.and.badgap(k)) then
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
	pflag=.false.		!so 'new file' does not print again
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
	Nbst=Nbst+1	!Update # of bursts- reversed below if bst is bad
	Nbst0=Nbst0+1	!Update total # of bursts- reversed below if bst is bad
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
c If NOT badend, so normally a bad gap is not treated as valid end of
c burst, then ask separately about the last interval (always set bad)
	if(shut.and.(.not.badend).and.k.eq.nint.and.badgap(k)) then
	   print 72
c72	   format(/,
c     & ' The last interval is set ''bad'':',/,
c     & ' Should it be treated as a valid end for the last burst [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(ans.eq.'Y') goto 324	!accept last burst
	endif
	if(shut) then
	   if(badend) then
		if(badgap(k).and.k.le.nint) goto 3242
	   else
		if(badgap(k).and.k.le.nint) then
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
c NB the number of intervals is now reduced by one when the last interval
c is set bad, so '.or.(k.eq.nint)' causes last burst/cluster to be omitted
c=	if((open.and.badgap(k)).or.(k.eq.nint)) then
	if(open.and.badgap(k)) then
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
	open=amp.ne.0
	shut=.not.open
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
	pflag=.false.		!so 'new file' does not print again
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
	Nbst=Nbst+1	!Update # of bursts- reversed below if bst is bad
	Nbst0=Nbst0+1	!Update total # of bursts- reversed below if bst is bad
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
c If NOT badend, so normally a bad gap is not treated as valid end of
c burst, then ask separately about the last interval (always set bad)
	if(shut.and.(.not.badend).and.k.eq.nint.and.badgap(k)) then
	   print 72
c72	   format(/,
c     & ' The last interval is set ''bad'':',/,
c     & ' Should it be treated as a valid end for the last burst [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(ans.eq.'Y') goto 324	!accept last burst
	endif
	if(shut.and.(badgap(k).and.k.le.nint)) then
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
c NB the number of intervals is now reduced by one when the last interval
c is set bad, so '.or.(k.eq.nint)' causes last burst/cluster to be omitted
c=	if((open.and.badgap(k)).or.(k.eq.nint)) then
	if(open.and.badgap(k)) then
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
c End of ibtype=3
c
c Allocate 'open' and 'gap' lengths- same for all IBTYPE
c Gap ends as soon as open period found (except for 1st open period, NAS=1)
c so assign length of gap that precedes it (only done when 1st opening of
c an open period found- controlled by IOFLAG)
c Next bit done if tint(k) is 'open'
c
41	continue
	nop=nop+1	!no of indiv openings
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
c	if(nas.gt.5000) print 623
c623	format(' ***ERROR: MORE THAN 5000 OPENINGS/BURST') !enlarge OPS()
	if(nas.gt.nasmax) then
	   print 623,nasmax
623	   format(
     &   ' ***More than ',i6,' openings/burst: array size increased') !enlarge OPS()
	   nasmax=nasmax*2
	   nbst=nbst-1	!reverse increment
	   Nbst0=Nbst0-1	!ditto for total
	   if(allocated(ops)) DEALLOCATE(ops,gaps,isubs)
	   goto 700
	endif
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
	Nbst=Nbst-1		!reverse update if no proper ending found
	Nbst0=Nbst0-1	!ditto for total
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
	pflag=.false.
	ilast=k-1
	if(deb) then
c       Type intervals preceding current burst if any (type BURST etc later, in
c       in case this burst is bad, and gets overwritten later
c	   print 618,nbst,Ifst,k-1
c	   if(discprt) write(8,618) nbst,ifst,k-1
c618	   format(/,/,' BURST ',i7,' = intervals ',i5,' to ',i5)
c      print current bst
	   call DEBTYP(0,ifst,k-1,tint,ampl,iprops,
     & 0,-1,-1,yval,.false.,nintt,ndimy)
c     print 2 intervals following current bst
	   call DEBTYP(0,k,k+1,tint,ampl,iprops,
     & 0,-1,-1,yval,.false.,nintt,ndimy)
	endif
c
c
	if(bad1.and.(ID.eq.4.or.ID.eq.6)) bad=.true.
c
c Calculate 'amplitude' for the burst just defined, if needed (needed
c for amp dist (id=12,13), or if conamp or exass=true). Burst already
c excluded if it contains an unusable opening
c====calc amplitude whether used or not for now
c====	if((.not.bad).and.(id.eq.12.or.id.eq.13.or.conamp.or.
c====     & exass)) goto 523	!calc amp for burst
c====	goto 524		!skip amp calc
523	continue
	dubamp=.false.
	s=0.
	den=0.
	do 525 m=Ifst,k-1	!go through all intervals in bst
	   m1=m
	   amp=ampl(m1)
	   if(amp.eq.0) goto 525	!skip shut
c mystery crash on call to DUBIOUS with simulated data
c	   if(dubious(m1)) dubamp=.true.	!true if ANY dubious amp found
	   dub=BTEST(iprops(m1),0)
	   if(dub) dubamp=.true.	!true if ANY dubious amp found
c
	   if(ibamp.eq.1) then
		s=s + tint(m)*amp
		den=den + tint(m)
	   else
		s=s + amp		!accum amp
		den=den+1.0
	   endif
525	continue
c
	if(den.ge.treso) goto 526
c	call bell(4)
	print 527
527	format(' Burst with no openings found!')
	stop
526	amp=s/den		!define amp
c How to label burst amplitude as dubious now?!
c NO -do later, in case this burst bad and so overwritten
c	bampl(nbst)=amp		!amp (set dubious if nec)
c	if(deb) then
c	   print 617,nbst,bampl(nbst)
c	   if(discprt) write(8,617) nbst,bampl(nbst)
c617	   format(' Mean amp for burst #',i4,' (pA) = ',G13.6,/)
c	endif
c
c Exclude bursts as specified
	if(exass.and.dubamp) bad=.true.
	if(id.eq.12.or.id.eq.13) goto 524
c=	amp=bampl(nbst)
	if(conamp.and.((amp.lt.amplo).or.(amp.gt.amphi))) bad=.true.
524	continue
c If set bad then overwrite the burst that has just been completed
c If findgap=true then, after bad gap at end of file causes bad=true
c here, the gap that ends the overwriten burst is a valid start for
c the next one, so reduce ifst by one here to prevent it being skipped
c SKIP RIGHT TO END OF LOOP IF BAD
	if(bad) then
	   Nbst=Nbst-1	!reverse update of Nbst
	   Nbst0=Nbst0-1	!ditto for total
	   if(findgap) k=k-1	!so ifst is reduced by 1 at label 325
	   goto 325 	!Skip assignment of Yval; look for next bst
	endif
c    If jb>nbomit then there are no further bursts to be omitted
	if(omitbst.and.jb.le.nbomit) then
	   if(nbst0.eq.ibomit(jb)) then
		jb=jb+1	!so points to index of next burst to be omitted
		Nbst=Nbst-1	!reverse update of Nbst
		goto 325	!skip burst (and display of switched on)
	   endif
	endif
c
c 	IF(deb) print 4051,Ifst,K,Nbst,NOP,nsg,nas,nbs
c 4051	format(' End of burst: Ifst,K,Nbst,NOP,nsg,nas,nbs= ',/,7I8)
c
c This burst is OK so now print and define  bampl()
	bampl(nbst)=amp		!amp (set dubious if nec)
	if(deb) then
	   print 132,nbst,Ifst,ilast
	   if(discprt) write(8,132) nbst,ifst,ilast
132	   format(' BURST ',i7,' = intervals ',i5,' to ',i5)
	   print 617,nbst,amp
	   if(discprt) write(8,617) nbst,amp
617	   format(' Mean amp for burst #',i4,' (pA) = ',G13.6,/,/)
	endif
c Display the burst here if OK -now below so Yval can be shown
c
	if(nas.ne.nbs+1) goto 597
	if((ibtype.eq.1.or.ibtype.eq.3).and.nbs.ne.nsg) goto 597
	if((ibtype.eq.1.or.ibtype.eq.3).and.nas.ne.nsg+1) goto 597
	goto 599
597	continue
c	call bell(2)
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
	yval1=yval(nbst)		!for VSAMP
	if(deb) call DEBTYP(0,-1,-1,tint,ampl,iprops,
     & 0,nbst,nbst,yval,.false.,nintt,ndimy)	!print YVAL(j)
	if(id.le.2) goto 338		!this burst finished
c
6021	if(id.gt.6) goto 601
c Assign YVAL() for id=3-6
c NB must now assign values for ID=3-6 after burst finished, as for
c others, in case amps needed (conamp or exass)
c (NB sublevel times already excluded from OPS() for id=4,6)
	do m=1,nAs
	   yval(nbst)=yval(nbst) + ops(m)	!add open times for all
c add gaptime for 'total burst length'
	   if(m.eq.nAs) goto 600	!no gaps(nas)
	   if(id.eq.3.or.id.eq.4) yval(nbst)=yval(nbst) + gaps(m)
600	   continue
	enddo
	yval1=yval(nbst)		!for VSAMP
	if(deb) call DEBTYP(0,-1,-1,tint,ampl,iprops,
     & 0,nbst,nbst,yval,.false.,nintt,ndimy)	!print YVAL(j)
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
	if(id.eq.9.or.id.eq.10.or.id.eq.16.or.id.eq.17) goto 604
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
	do m=1,nas
	   yval(j2)=yval(j2) + ops(m)	!add open time
	   if(id.eq.7.and.m.lt.nas) yval(j2)=yval(j2) + gaps(m) !add shut time
	enddo
	yval1=yval(j2)		!for VSAMP
	if(deb) call DEBTYP(0,-1,-1,tint,ampl,iprops,
     & 0,j2,j2,yval,.false.,nintt,ndimy)	!print YVAL(j)
	goto 338			!finished this bst
c
c Now ID=9,10,16,17 -first exclude invalid bursts
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
	if(id.eq.16.and.Ops(nas).lt.0.0001) goto 3381
	if(id.eq.17) then
	   if(nbs.lt.1) goto 3381	!nbs=0 if only one opening
	   if(exctwo.and.nbs.lt.2) goto 3381	!only bsts with two or more gaps
	   if(gaps(nbs).lt.0.0001) goto 3381
	endif
	j2=j2+1
	if(id.eq.9) Yval(j2)=gaps(Jth)
	if(id.eq.10) Yval(j2)=Ops(Jth)
	if(id.eq.16) Yval(j2)=Ops(nas)
	if(id.eq.17) Yval(j2)=gaps(nbs)
	yval1=yval(j2)		!for VSAMP
	if(deb) call DEBTYP(0,-1,-1,tint,ampl,iprops,
     & 0,j2,j2,yval,.false.,nintt,ndimy)	!print YVAL(j2)
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
	do m=-Jth,n		!start at Jth op or gap
	   j=j+1
	   if(id.eq.9) Yval(j)=gaps(m)	!omit m=nas
	   if(id.eq.10) Yval(j)=ops(m)
	   if(deb) call DEBTYP(0,-1,-1,tint,ampl,iprops,
     &    0,jfst,j,yval,.false.,nintt,ndimy)	!print YVAL(j)
	enddo
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
     & 0,j2-1,j2,yval,.false.,nintt,ndimy)	!print both YVAL(j)
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
	   yval1=yval(j2)		!for VSAMP
	   if(id.eq.13) Yval(j2)=-Yval(j2)
	   yval1=yval(j2)		!for VSAMP
	else
	   goto 338		!skip this burst
	endif
	if(deb) then
	   call DEBTYP(0,-1,-1,tint,ampl,iprops,
     &	 0,nbst,nbst,yval,.false.,nintt,ndimy)		!print YVAL(nbst)
	endif
	goto 338
c
c Messages if values<0.001 or sublevel-containing periods found
3381	continue
c	call bell(4)
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
c  Modif 06/30/00 12:01pm so that can use 'Popen without last opening' in
c   addition to other constraints
	if(id.eq.14) then
	   dflag=.false.	!yval not defined for current burst so no display in vsamp
	   if(bmin.gt.0.000001) then
		if(blength.gt.bmin) then
		   nb14=nb14+1
		   if(ipop.eq.0) then
			Yval(nb14)=popen
		   else
			Yval(nb14)=popen1
		   endif
		   yval1=yval(nb14)		!for VSAMP
		   dflag=.true.    !Yval defined for current burst so display in vsamp
		else
		   nbshort=nbshort+1
	      endif
	   else if(ibmin.gt.1) then
		if(nas.ge.ibmin) then
		   nb14=nb14+1
		   if(ipop.eq.0) then
			Yval(nb14)=popen
		   else
			Yval(nb14)=popen1
		   endif
		   yval1=yval(nb14)		!for VSAMP
		   dflag=.true.	!value defined for current burst so displayin vsamp
		else
		   nbshort=nbshort+1
	     endif
	   else
	      nb14=nb14+1
		if(ipop.eq.0) then
		   Yval(nb14)=popen
		else
		   Yval(nb14)=popen1
		endif
		yval1=yval(nb14)		!for VSAMP
		dflag=.true.	!value defined for current burst so displayin vsamp
	   endif
	   if(deb) call DEBTYP(0,-1,-1,tint,ampl,iprops,
     &  0,nb14,nb14,yval,.false.,nintt,ndimy)		!print YVAL(nbst)
	endif
c
	pbst=(tc.lt.-0.001).and.ID.ge.3.and.ID.le.6.and.(Yval(Nbst).gt.
     &    abs(tc))		!print BURST
	pbst=pbst.or.listall.or.(Nbst.ge.N1.and.Nbst.le.N2).or.
     & bprint
	if(.not.pbst) then
	   if(ppopen) then	!print bst number since skipped if pbst=false
	      print 57, nbst
	      if(discprt) write(8,57) nbst
	   endif
	   goto 354		!skip print of intervals in burst
	endif
357	continue
	if((kplast.gt.0).and.(ifst-1.ge.kplast+1)) then
         print 54
         if(discprt) write(8,54)
54	   format(/,' Intervals between bursts:')
	   do m=kplast+1,ifst-1
	      m1=m
            print 56,m,tint(m),ampl(m1)
            if(discprt) write(8,56) m,tint(m),ampl(m1)
c56	      format(6x,i6,2x,2g13.6)
56	      format(i6,2x,2g13.6)
	   enddo
	   print 108
	   if(discprt) write(8,108)
	endif
      print 57, nbst
      if(discprt) write(8,57) nbst
57	format(' Burst #',i4)
	do m=ifst,k-1
	   m1=m
         if(discprt) write(8,356) m,tint(m),ampl(m1)
355	   print 356,m,tint(m),ampl(m1)
356	   format(i6,2x,2g13.6)
	enddo
	kplast=k-1		!index of last tint() printed
c	print 108
c	if(discprt) write(8,108)
354	continue
c
	if(ppopen) then
	   print 403,ifst,totop,nas,totgap,nbs,
     &	blength,popen,popen1
	   if(discprt)write(8,403) ifst,totop,nas,totgap,nbs,
     &	blength,popen,popen1
c403	   format(' Burst #',i4,/,
403	   format(' Burst starts at interval # ',i9,/,
     &  ' Total open = ',g13.6,' (n = ',i4,');',
     &  ' Total shut = ',g13.6,'(n = ',i4,')',/,
     &  ' Burst length = ',g13.6,'   P(open) = ',f8.5,
     &  ' (exc last op = ',f8.5,')')
c   Also print yval if a valid value is defined -need to define index for
c yval to do this (as in def of nyval below)
	   if(id.ge.1.and.id.le.6) index1=nbst
	   if(ID.eq.7.or.id.eq.8.or.ID.eq.11.or.id.eq.12.or.id.eq.13) then
		index1=j2
	   endif
	   if(id.eq.14) index1=nb14
	   if((ID.eq.9.or.ID.eq.10).and.Jth.gt.0) index1=j2
	   if((ID.eq.9.or.ID.eq.10).and.Jth.lt.0) index1=j
	   if(index1.ne.indlast) then
		yval1=yval(index1)		!for VSAMP
      	print 404, index1,Yval(index1)
      	if(discprt) write(8,404)  index1,Yval(index1)
404		format(
     &  ' Value for distribution: Yval(',i4,') = ',g13.6)
		indlast=index1
	   endif
	endif
c
	Nsgap=Nsgap+NSG	!Nsgap=TOTAL NO OF SHORT GAPS IN CURRENT EXPT
	Nopen=Nopen+NOP
	NAset=NAset+nAs
	NBset=NBset+nBs
c DISPLAY BURST NEXT
c NB may have problem with upper display if burst aborted by a bad gap -if
c the 'bad gap' actually encompasses a long stretch of record then transition
c ifst-2 may be a LONG way before start of burst, giving too many points on
c upper display (very slow, and in extreme cases, run out of allocatable memory)
c If bad gap encountered (BADLAST=true) then, if first display starts more
c than say 3*tcrit before beginning of burst then leave only 3*tcrit
	if(id.eq.14.and.(.not.dflag).and.skipshort) goto 3252
3251	if(dispbst) then
	   idelt=10.*sngl(1.d3/dfinter)
	   i1=ifst-2	!two transitions before start of burst
	   i2=ilast+3		!two transitions after end of burst
	   ib1=ifst
	   ib2=ilast+1
	   if(i1.lt.1) i1=1
	   if(i2.gt.nint) i2=nint
c         Must now convert i1,i2,ibstart, ibend, to index
c	    in consam=j1, j2, jb1,jb2
	   iflast=1	!first time -the do indices in ascending order
	   i1=index(i1)	!index in raw data, before resolution imposed
	   j1=INDEXG(i1,
     &    iflast,nfits,ifits,timsav,tint0,dfinter,nintt)
c for the transition that starts the burst, use indexgb version to get
c baseline
	   ib1=index(ib1)	!index in raw data, before resolution imposed
	   jb1=INDEXGB(ib1,ibase,
     &    iflast,nfits,ifits,timsav,ibaselin,tint0,dfinter,nintt)
	   ib2=index(ib2)	!index in raw data, before resolution imposed
	   jb2=INDEXG(ib2,
     &    iflast,nfits,ifits,timsav,tint0,dfinter,nintt)
	   i2=index(i2)	!index in raw data, before resolution imposed
	   j2=INDEXG(i2,
     &    iflast,nfits,ifits,timsav,tint0,dfinter,nintt)
c
c On lower panel show 10 ms before and after each burst=idelt points
	   idelt=int4(10.*sngl(1.d3/dfinter))
	   jb1=jb1-idelt
	   if(jb1.lt.j1) j1=jb1
	   jb2=jb2+idelt
	   if(jb2.gt.j2) j2=jb2
c
c If last burst aborted check that display in upper panel not too long (see above)
	   if(badlast) then
		tdelt=float(jb1-j1)*sngl(dfinter)*1.e-3
		tnew=2.*tcrit
		if(tnew.lt.50.) tnew=50.	!50 ms baseline before start of bst
		idelt=int4(tnew*sngl(1.d3/dfinter))
		if(tdelt.gt.tnew) j1=jb1 - idelt
		if(j1.lt.1) j1=1
c (need to check length AFTER burst too in upper panel?)
	   endif
c
c	   imode=1	!estimate baseline internally in VSAMP
	   imode=0
	   ybase=float(ibase)/sfac2	!sfac2 converts intermed units to pA
c	   amp=-1.		!scale internally
	   amp=abs(avamp)
	   colseq=.false.
	   call INTCONV(nbst0,cnum)
	   line1='Burst '//charnb(cnum)//' ('
	   if(id.eq.14) then
		n=nb14-1
		if(n.lt.0) n=0
	      call INTCONV(n,cnum)	!present one not decided yet
	   else
	      call INTCONV(nbst-1,cnum)	!present one not decided yet
	   endif
	   call INTCONV(nbst0,cnum1)
	   line1=charnb(line1)//charnb(cnum)//'/'//charnb(cnum1)//
     &	' accepted)'
c	   call INTCONV(ifst,cnum)
c	   line1=charnb(line1)//'; ints '//charnb(cnum)
c	   call INTCONV(ilast,cnum)
c	   line1=charnb(line1)//' to '//charnb(cnum)
	   if(id.ne.9.and.id.ne.10.and.id.ne.11)then
		if(id.eq.14.and.(.not.dflag)) then
	         line1=charnb(line1)//': REJECTED (TOO SHORT)'
c		   reject=.false.		!so reject button does not show (set below)
		else if(yval1.gt.-1000.) then
		   call DCFORMAT(yval1,8,3,cnum)
	         line1=charnb(line1)//': dist = '//charnb(cnum)
		endif
	   endif
c More details in line2 and 3 (op periods, tot open, tot shut, Popen, Popen1)
	   call INTCONV(nsg+1,cnum)
	   line2=charnb(cnum)//' ops/bst'
	   call DCFORMAT(totop,8,3,cnum)
	   line2=charnb(line2)//'; open (ms)'//charnb(cnum)
	   call DCFORMAT(totgap,8,3,cnum)
	   line2=charnb(line2)//'; shut'//charnb(cnum)
	   call DCFORMAT(blength,8,3,cnum)
	   line2=charnb(line2)//'; length'//charnb(cnum)
	   call DCFORMAT(popen,6,3,cnum)
	   line3='Popen'//charnb(cnum)
	   call DCFORMAT(popen1,6,3,cnum)
	   line3=charnb(line3)//
     &	'; Popen (without last op)'//charnb(cnum)
	   reject=.true. 		!show the reject option
	   if(omitbst) reject=.false.	!already omitting preselcted bursts
	   if(id.eq.14) then
		if(dflag) then
		   reject=.true.		!so reject button does shows
		else
		   reject=.false.		!so reject button does not show
		endif
	   endif
	   stop=.true.		!show the stop display option
	   if(deb) call MODE(18) 	!into graphics for display only
	   call VSAMP(j1,j2,jb1,jb2,consamdef,adcfil,nline,yline,
     &     ybase,amp,imode,colseq,j11,j22,line1,line2,line3,reject,
     &	stop,qfile,nsam,srate,irecfst,calfac,ioffset)
	   boxdef=.true.		!dialog box not defined until after 1st VSAMP call
	   if(deb) call VIDEOMOD(3)
	   if(omitbst) reject=.false.	!already omitting preselcted bursts
	   if(reject) then	!NB do NOT change nbst0=total number of bursts
		nbomit=nbomit+1
		ibomit(nbomit)=nbst0
c		nbst=nbst-1
c		yval(nbst)=0.0
c  care -some distns add more than one value per burst!!!
		do n=nblast+1,nbst
		   yval(n)=0.0
		enddo
		nbst=nblast		!restore to prev value
		if(id.eq.14) nb14=nb14-1
	   endif
	endif
	if(dispbst) then
	   if(stop) then
		dispbst=.false.
		call VIDEOMOD(3)
	   endif
	endif
c	if(dispbst) then
c	   ans='N'
c	   call DCASK('Stop displaying bursts',ans,ans)
c	   dispbst=ans.eq.'N'
c	endif
c End of display
3252	continue
c
c END OF BURST LOOP
c Now the end of loop for each burst
325	continue
	nblast=nbst
	badlast=bad
	Ifst=k+1	!index of interval after that which ended last bst
	if(Ifst.le.nint) goto 360
c END of Ifst loop
c
	if(allocated(ops)) DEALLOCATE(ops,gaps,isubs)
c
C RECORDING OF VALUES FOR LAST BURST COMPLETED- GO TO 360 TO LOOK
C EXPLICITLY FOR FIRST OPENING OF NEXT BURST
C
C ALL BURSTS IDENTIFIED.
c====
99	continue
	if(VIDEOTYP().ne.3) call VIDEOMOD(3)
c
	if(id.ge.1.and.id.le.6) Nyval=Nbst
c=	if(id.eq.12.or.id.eq.13) Nyval=Nbst
	if(id.eq.12.or.id.eq.13) Nyval=j2
	if(id.eq.14) Nyval=Nb14
	if(ID.eq.7.or.id.eq.8.or.ID.eq.11.or.id.eq.16.or.id.eq.17) then
	   Nyval=j2
	endif
	if((ID.eq.9.or.ID.eq.10).and.Jth.gt.0) Nyval=j2
	if((ID.eq.9.or.ID.eq.10).and.Jth.lt.0) Nyval=j
	if(ID.eq.1) obbar=float(Nopen)/float(Nbst)
	if(ID.eq.2) obbar=float(NAset)/float(Nbst)
c
	print 581, nbad1,nbad2
	if(discprt) write(8,581) nbad1,nbad2
581	format(/,
     & ' Number of bursts aborted by bad open time = ',i4,/,
     & ' Number of bursts aborted by bad shut time = ',i4,/)
	if(id.eq.14) then
	   print 582, nbst0,nbshort,nbomit,nyval
	   if(discprt) write(8,582) nbst0,nbshort,nbomit,nyval
582	   format(
     &  ' Total number of bursts = ',i6,/,
     &  ' Number of bursts rejected because too short = ',i6,/,
     &  ' Number of bursts rejected manually = ',i6,/,
     &  ' Number of bursts for accepted for distribution = ',i6,/)
	endif
	print 3281
3281	FORMAT(' Print burst parameters [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).EQ.'N') GOTO 4091
c
	if(onetcrit) then
	   if(discprt) write(8,58) tcrit
58	   format(' Critical gap length (ms)= ',g13.6)
	else
	   do i=1,nfile
		if(discprt) write(8,59) i,tcvals(i)
59		format(' File # ',i3,': critical gap length (ms)= ',g13.6)
	   enddo
	endif
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
4091	continue
c
	if(omitbst) then
	   print 62,nbst0,nbomit
	   if(discprt) write(8,62) nbst0,nbomit
62	   format(/,1x,i4,
     &  ' bursts of which ',i4,' omitted (as specified in bstomit.dat)')
	   if(nbst0.ne.nbst01) then
c		call BELL(2)
		print 621,nbst0,nbst01
621		format(
     &	 ' Total number of bursts,',i4,
     &	 ' not the same as before (',i4,'): O.K. [N] ? ')
		ans='N'
		call INPUTa(ans)
		if(ans.eq.'N') goto 630
	   endif
	else if(nbomit.gt.0) then
	   print 63,nbst0,nbomit
	   if(discprt) write(8,63) nbst0,nbomit
63	   format(/,1x,i4,
     &    ' bursts of which ',i4,' rejected')
c
	   print 64
64	   format(' Save the bursts to be omitted for use again [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(ans.eq.'Y') then
		OPEN(unit=11,file='bstomit.dat',status='UNKNOWN',
     &	   access='TRANSPARENT')
		write(11,rec=1) nbomit,adcfil,ibtype,ibamp,exass,
     &	 findgap,badend,conamp,amplo,amphi,nfile,tcvals,nbst0

		write(11,rec=513) (ibomit(i),i=1,nbomit)
		CLOSE(unit=11)
	   endif
	endif
c
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
	   yv2=avs(1)-avs(2*nmax)	!SUM
	   n2=NS(1)-NS(2*nmax)		!N
	   yv2=yv2/float(n2)		!MEAN
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
		   print 626,j,yv2,n2
		   if(pon()) write(7,626) j,yv2,n2
      	   if(discprt) write(8,626) j,yv2,n2
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
c      Print overall mean of above
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
	if(allocated(ibomit)) then
	DEALLOCATE(ibomit)
	endif
	DEALLOCATE(bampl)
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




