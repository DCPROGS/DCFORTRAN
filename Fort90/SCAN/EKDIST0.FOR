	PROGRAM EKDIST0
c EKDIST0 is version for old directories (now defunct)
c May 1989. Version of EKDIS3 for Lahey Fortran
c Modif 08/11/92 07:15pm for Lahey V5.x
c Modif 07/22/92 10:35am so .INI keeps both the expt files that were
c used on the last run (nfile,kfile) AND the expt files that were used
c to define bad sections in STABLOT (nfileb,kfileb)
c Modif 12/06/91 09:44am so 1st latencies can be read in from CJFIT (in
c	DISKIN) and fitted.  Resolution is already imposed on them (in CJFIT)
c	and values for treso,tresg read from directory in this case
c 02/28/90 06:25pm Altered so nfix,jfix are in a separate common block,
c not in LIKBLK, because jfix is needed in EXPLIK etc.to prevent alteration
c of an AREA parameter (because total area>1) when area is meant to be fixed
c May 88: titlex now 40 char
C
C TO TAKE SINGLE CHANNEL DATA FROM SCAN1 AND FIT DISTRIBUTIONS
C OF OPEN TIME,SHUT TIME AND AMPLITUDE (BASED ON SCDIST.FOR)
c  NINT1=no of intervals in each file read (local to this subroutine)
c  NINTT=total no of intervals in all files before resolution imposed
c  NINT=total no of intervals AFTER resolution imposed (see RESINT)
C
C****MODIF 7-JAN-82 TO CORRECT HISTO OF OPEN TIMES WITH SPEC
C	AMPLITUDE RANGE WHEN AMPS NEGATIVE. NOW USES ABSOLUTE
C	AMPS THROUGHOUT SO CANNOT DO INWARD AND OUTWARD CURRENTS
C	IN SAME RUN
C
	dimension TINT(20480)	!now up to 20*1024 [up to 5 *1024]
	real TINT1(10240,2)		!EQUIV with TINT (used for latencies)
	integer*2 IAMPL(20480)
	dimension YVAL(20480)
	dimension FREQ(510),XAXIS(510),xsav(5,4)
	dimension xcalc(512),fcalc(512)
	dimension obfreq(510),frqcal(510)
	dimension ylo(10),yhi(10),tcseq(10)		!for seqlst
	real*4 ampval(10)	!to store amplitudes marked on stability plot
	REAL AMEAN(10),AREA(10),SD(10)	!TO HOLD PARAMETERS
	REAL theta(20)			!FOR MLFITS,CSIMP
	real thetsav(20,5),ylosav(5),yhisav(5)		!to save past fits
	integer ksav(5),idsav(5),ncompsav(5),nfitsav(5),isgsav(5) !to save past fits
	character*12 fitype,fitsav(5)			!to save past fits
	INTEGER JFIX(20),kfile(20)	!up to 20 files now
	integer*2 kfileb(20),nfileb	!for EKDIST.INI
	integer isbad(20),iebad(20)	!for bad bits (see stabplot)
	LOGICAL OHIST,BHIST,GHIST,THIST,AHIST,OBHIST,FITTED,PREFIT
	LOGICAL ERRORS,DEBUG,CONAM1,exass,exass1,AMPOS,sdcon,lastfit
	LOGICAL SUBLEV,EXCSUB,GAMDST,logt,first,latency
	logical pon,fastf,slock,caplock,present,readini,calcor
c#	EXTERNAL EXPLIK,GAULIK,OBLIK,elfun3
	logical sbin,shist,sres,sexp,crembad,samex,mono
	logical discprt
	character*40 titlex,titley
	character filnam*32
	character*11 cdate,ctime
	character*2 ndev,ndev1		!for DISKIN
      character*1 ans,UC
c
c	integer Lblast(12)
c	character*78 textlast(12)
c
	equivalence (tint,tint1)
c
c	common/DBOX/Lblast,nblast,cslast,ihb,csize,ifonb,ifont   !Lahey 5.x version
	COMMON/AMPBLK/iampl
	common/sblk/sbin,shist,sres,sexp
c	COMMON/LIKBLK/K,kfit,JFIX,ELMAX,EM,DELMIN,NDISP,THETA,NPAR
	COMMON/LIKBLK/K,kfit,elmax,em,delmin,ndisp,theta,npar
	common/FIXBLK/nfix,jfix
	COMMON/LOGBLK/ AHIST,THIST,OBHIST
	common/RBLCK/treso,tresg,iacrit,mavamp
	COMMON/FITBLK/yval,xaxis,freq,Nyval,
     & Ylow,Yhigh,Ncomp,Nfit,ampos,pen,gamdst,isg,fastf,logt,nb1,nb2
	COMMON/OVLBLK/OBMAX,Jth,CONAM1,exass,exass1,AMPHI,AMPLO,
     & EXCSUB,Ith
	character*40 mtitle	!machine name
	common/dp/discprt
	common/dpp/filnam,machine,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
C  COMMON/OVLBLK/ IS TO KEEP VALUES OF VARIABLES FROM SUBROUTINES
C  (CDIST0, CDIST3, CMEAN) SO THEY ARE NOT LOST DURING OVERLAYING
C
	pon()=slock()
	debug()=caplock()
c
101	format(a1)
	ndimy=20480
	fitted=.false.
	lastfit=.false.
	nfits=0		!number of past fits stored
	tcfac=2.0
	call UNDER0('true')	!so underflows set=0 (Lahey manual p12.8)
c
	OPEN(unit=7,file='PRN',iostat=nerr)
	filnam='ekdist0.prt'
	call WINPRINT
c
	ylo(1)=-1000.
	first=.true.
	logt=.false.	!unless reset
	tcrit=-1.		!until defined
	nfit=0
	iydat=0
	idiskq=-1		!disc for plot queue (until defined)
	ihjc=1		!default=set resolution by HJC method
	mono=icol.eq.0
c NB read nbad as nbad1 from .INI so that if runs done without bad bits
c removed (nbad set to 0) the values in the .INI file will not be erased
c (unless bad bits are redefined by another STABPLOT)
	nbad=0		!no bad sections
	nbad1=0		!in case .INI not read
	readini=.false.
	INQUIRE(file='EKDIST0.INI',exist=present,flen=len)
	if(present.and.len.gt.0) then
	   readini=.true.
         OPEN(unit=19,file='EKDIST0.INI',status='UNKNOWN',
     &   access='DIRECT',form='UNFORMATTED',recl=512)
	   read(19,rec=1) ndev,nfile,kfile,iydat,nbad1,isbad,iebad,
     &      nfileb,kfileb,tresob,tresgb,idiskq,treso,tresg
	   if(debug()) then
		print 700,nbad1,idiskq
700		format(' nbad,idiskq from .INI = ',2i8)
	   endif
	   CLOSE(unit=19)
	endif
c^^	MFLAG=1		!PLOT CONTROL-1ST PLOT IN LOW POSITION
c Return to 205 for another expt to find where next. First time this
c only sets SBIN etc
205	call WNEXT1(idest,first)
	if(idest.eq.1) goto 99	!exit
	print 3000
	if(first) then
	   if(pon()) write(7,3000)
	   if(discprt) write(8,3000)
3000	   format(/,' EKDIST0: Single channel distributions ')
	   call DATE1(cdate)		!DC subroutine
	   call TIME(ctime)
	   print 1,cdate,ctime(1:8),mtitle
	   if(pon()) write(7,1) cdate,ctime(1:8),mtitle
	   if(discprt) write(8,1) cdate,ctime(1:8),mtitle
1	   format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     &	'   Machine = ',a40)
	   print 399
399	   format(
     & ' SCROLL LOCK on for printing'/
     & ' CAPS LOCK on for debugging (and to omit some data)')
	endif
c	if(debug()) print 2051,sbin,shist,sres,sexp
c2051	format(' sbin,shist,sres,sexp= ',4(2x,l2))
C   NOW GET DATA
	idest=0
	if(debug().or.iydat.ne.1) then
         print 12
12	   format(
     & ' [1] Read data from normal data disc',/,
     & ' (2) Get special data from YVAL.DAT',/,
     & ' Option number [1] = ')
	   read 11,iydat
11	   format(i8)
	   if(iydat.le.0) iydat=1
	else
	   if(iydat.eq.1) goto 13	!after first time
	endif
	if(iydat.eq.2) then
	   call YVALIN(titlex,yval,nyval,pon(),thist,obhist,
     &     ahist,idtype,ndimy)
	   fitted=.false.
	   prefit=.false.
	   logt=.false.
	   goto 210
	endif
13	continue
	call DISKIN(TINT,IAMPL,Nintt,acal,iavamp,idisk,
     & sublev,jfilt,rms,idest,nfile,kfile,trise,fc,iscan,
     & TINT1,nlon,nloff,tjlen,ttoff,toffset,nintt1,nintt2,treso,tresg,
     & ndev,ndev1,first,readini,calcor,acalmin)
	latency=iscan.eq.-4001
	if(idest.eq.99) goto 99		!out
c NB When nbad>0 is read from .INI file, question below asked first time
c (before 'first' set false), but cannot actually remove the points
c until after resolution is imposed, so set CREMBAD here if points are
c to be removed after call to RESINT
	crembad=.false.
	if(readini.and.nbad1.gt.0) then
c check whether the bad bits were defined for the current expts or not
	samex=.true.
	 if(nfile.ne.int4(nfileb)) samex=.false.
	 do 38 i=1,nfile
38	  if(kfile(i).ne.int4(kfileb(i))) samex=.false.
	 if(treso.ne.tresob) samex=.false.  !resolution alterded
	 if(tresg.ne.tresgb) samex=.false.
	 if(.not.samex) then
          print 39
c          if(pon()) write(7,39)
c          if(discprt) write(8,39)
39	    format(' The bad sections on disc are for',
     &	' different experiments and/or resolution')
	 else if(first.and.samex) then
	  call BELL(1)
        print 37,nbad1,kfileb(1),kfileb(nfileb),1000.*treso,1000.*tresg
37	  format(' NOTE: ',i3,
     &' bad sections have already been defined by stability plot for',/,
     & ' expts ',i5,' to ',i5,' (resolution ',2f8.2,' microsec)',/,
     & ' Remove these bad sections before analysis [N] ? ')
	  read 101,ans
	  if(UC(ans).eq.'Y') then
	    nbad=nbad1
	    crembad=.true.       	!remove them
	  else
	    nbad=0		!don't remove bad bits (but nbad1 preserved)
          print 331
          if(pon()) write(7,331)
          if(discprt) write(8,331)
	  endif
	 else if(sexp.and.sres.and.samex) then
	  if(nbad.gt.0) then
          print 33,nbad,nint
          if(pon()) write(7,33) nbad,nint
          if(discprt) write(8,33) nbad,nint
33	    format(/,
     &  ' The ',i3,' bad sections (from stability plot) are omitted',/,
     &  ' There are now ',i5,' good intervals',/,
     &  '       Bad from #       to #',/)
	    do 34 n=1,nbad
          print 35,n,isbad(n),iebad(n)
          if(pon()) write(7,35) n,isbad(n),iebad(n)
          if(discprt) write(8,35) n,isbad(n),iebad(n)
35	    format(i8,3x,i8,2x,i8)
34	    continue
	    print 108
	    if(pon()) write(7,108)
	    if(discprt) write(8,108)
108	    format(/)
	  else if(nbad.eq.0) then
          print 331
          if(pon()) write(7,331)
          if(discprt) write(8,331)
331	    format(/,
     &  ' The bad sections (from stability plot) are NOT omitted',/)
	  endif
	 else if((.not.sres).or.(.not.sexp).or.(.not.samex)) then
	  nbad=0
        print 36
        if(pon()) write(7,36)
        if(discprt) write(8,36)
36	  format(/,
     & ' Experiment and/or resolution altered so nbad reset to 0',/,
     & ' Do another stability plot to redefine bad sections',/)
	   pause
	 endif
	endif
c
	first=.false.
c
C NEXT RECALC DATA SO IT HAS A CONSISTENT TEMPORAL
C RESOLUTION (TRES, WHICH IS REQUESTED IN RESINT); TRES ETC AND
C IAMPL IS IN COMMON (RBLCK AND ABLCK)
	if(.not.latency) then
	   imode=0		!call RESINT from EKDIST
	   call RESINT(TINT,NINTt,nint,acal,iavamp,imode,iampl,
     &    sublev,jfilt,fc,rms,trise,ihjc)
c Choice of distn NOW moved from end of RESINT3 to main prog, below
c (previously idest=distribution type on exit from RESINT3)
c If bad sections to be removed (and resolution has not been changed)
c then do it (once!) here, now resolution has been imposed
	   if(crembad) then
	      call REMBAD(tint,iampl,nint,nbad,isbad,iebad,pon())
		crembad=.false.	!so not done again
	   endif
407	   continue
	   if(shist) idest=idsav1
	   if(shist) goto 411
	   print 100
100	   format(
     & ' (1) Amplitudes',/,
     & ' (2) Shut times (and times when NOT in specified amp range)',/,
     & ' (3) Open times (and times when IN specified amp range),',/,
     & '      and open-shut conditional dist and means',/,
     & ' (4) Bursts',/,
     & ' (5) Burst means (lists various burst chacteristics)',/,
     & ' (6) Subconductance sequence listing',/,
     & ' (7) Stability plots for open periods,shut times or P(open)',/,
     & ' (8) Stability plots for amplitudes',/,
     & ' Option number = ')
	   read 11,iopt
	   if(iopt.lt.1.or.iopt.gt.8) goto 407
	   idest=iopt
	   idsav1=idest		!save dist type
411	   if(idest.eq.4.and.(.not.shist)) then
c	      print 410
c410	      FORMAT(' Calculate Tcrit to define bursts [N] ? ')
c	      read 101,ans
		call DCASK('Calculate Tcrit to define bursts','n',ans)
	      if(UC(ans).EQ.'Y') call TCSUB
	   endif
	else if(latency) then
102	   continue
	   if(shist) idest=idsav1
	   if(shist) goto 103
	   print 104
104	   format(
     & ' (1) Latency to first opening after ON jump',/,
     & ' (2) Latency to first opening after OFF jump',/,
     & ' (3) Stability plot for latencies',/,
     & ' Option number= ')
	   read 11,iopt
	   if(iopt.lt.1.or.iopt.gt.3) goto 102
	   idest=iopt
103	   if(idest.eq.3) idest=7	!signals stab plot
	   idsav1=idest		!save dist type
	endif
c
c
c NB if bad sections are defined in STABPLOT, so nbad>0 on exit, then
c we wish to use the redefined TINT for calc of various distributions
c - can do this only while same experiments, with same resolution, are
c being used (sexp=true, sres=true) because indices of bad values are
c defined via TINT() on which resolution has already been imposed. In
c this case the values of TINT,IAMPL are not altered in DISKIN or RESINT3
c so can use the modified TINT output here for all distributions.
c===fix stabplot for channels after jump (also ask if ON or OFF req)
	if(idest.eq.7.or.idest.eq.8) then
	   call STABPLOT(tint,iampl,nint,nbad,nbad1,isbad,iebad,
     &    namp,ampval,acal,trise,tcfac,iscan,idiskq,iopt,mono)
	   if(nbad1.lt.0) then		!new bad bits were defined
		nbad1=-nbad1		!restore sign
		nfileb=int2(nfile)	!record expt for which defined in .INI
		do 31 i=1,nfileb
31		 kfileb(i)=int2(kfile(i))
		tresob=treso
		tresgb=tresg
	   endif
	   goto 256	!where next
	endif
c	ntmin=100	!at least 100 transitions for p(open)
C
C TYPE OF DISTRIBUTION IS NOW REQUESTED IN RESINT (WHEN IFILE=1) AND
C OUTPUT AS IDEST.
C NOW GET PARAMETERS FOR HISTOGRAMS. NOW (4-JUN-81) HAVE OPTION TO
C ADD NEW FREQ ON TO PREV VALUES (WITH XAXIS ETC UNCHANGED) TO GET
C HISTOGRAM FOR > 512 OPENINGS. NFILE=NUMBER OF EXPTS SO FILED (WITH
C EXPT NOS IN KFILE(I)). IFILE=INDEX OF CURRENT EXPT (SEE DISKIN)
	lastfit=fitted.and.sexp.and.shist		!redisplay with same fit?
	fitted=.false.
	PREFIT=.FALSE.
	IF(IDEST.EQ.4) GOTO 402			!BURST DISTNs
	IF(IDEST.EQ.5) GOTO 403			!CMEAN
	if(idest.eq.6) goto 404			!Sequences
c Use CDIST0 for idest=1,2,3
401	call CDIST0(TINT,NINT,ACAL,IAVAMP,MAVAMP,IACRIT,YVAL,NYVAL,
     & OHIST,BHIST,IDtype,GHIST,THIST,AHIST,OBHIST,trise,iscan,
     & SUBLEV,treso,tresg,tres,IDEST,idsav,xsav,tcfac,
     & latency,tint1,nlon,nloff)
	if(OHIST.and.idtype.eq.4) goto 303	!open-shut correlation
	GOTO 210
c
402	call CDIST3(TINT,IAMPL,NINT,ACAL,MAVAMP,IACRIT,
     & YVAL,NYVAL,iscan,TCRIT,TC,NBST,OBBAR,OHIST,BHIST,IDtype,
     & GHIST,AHIST,OBHIST,iexc,
     & SUBLEV,ISG,treso,tresg,tres,idsav,ibamp,ibtype)
	IF(IDTYPE.EQ.11) GOTO 303	!correlation
	GOTO 210
403	call CMEAN(TINT,NINT,iampl,ACAL,MAVAMP,IACRIT,
     & TCRIT,TC,NBST,SUBLEV,ISG)
	goto 256	!where next?
404	call SEQLST(tint,iampl,nint,mavamp,iacrit,acal,trise,iscan,
     & ylo,yhi,nval,ksav,treso,tresg,tcseq)
	goto 256	!where next?
c
210	continue
c set bin widths (previously this was last part of CDIST)
	call SETBIN(YVAL,FREQ,XAXIS,nyval,thist,ohist,bhist,
     & ghist,obhist,logt,tres,obmax,idtype,xsav,ybar,nbin,nbw,
     & xwbase,nbdec,fmin,fmax,flo,fhi,ymin,ymax)
	if(nbin.eq.0) goto 256	!skip histo; where next?
C
C NOW DISPLAY HISTOGRAM OF ALL DATA IN YVAL
c	IF(IDTYPE.EQ.11) GOTO 303	!now before setbin
c Define labels etc for graph
204	continue
	call SCVD1(NBIN,XWBASE,FITTED,PREFIT,AHIST,THIST,OBHIST,
     & IDEST,OHIST,GHIST,BHIST,IDTYPE,isg,jth,xaxis,logt,ncalc,
     & xcalc,fcalc,titlex,titley,xw,ilabel,iydat,latency)
c
c Calc values now done inside SCVDU
c Plot graphs
2042	continue
	call SCVDU(XAXIS,XMIN,XMAX,FREQ,NBIN,FMIN,FMAX,FLO,FHI,
     & FITTED,PREFIT,AHIST,OBHIST,THIST,tres,IDEST,xtic,ftic,idiskq,
     & logt,first,titlex,titley,xwbase,xw,ilabel,ncalc,xcalc,fcalc,
     & obfreq,frqcal,theta,ncomp,kmax,ntx,nty,itx,ity,idtype,
     & amean,area,sd,antrue,isg,gamdst,sdcon,lastfit,
     & nfits,thetsav,ksav,idsav,ncompsav,nfitsav,ylosav,yhisav,
     & isgsav,fitsav,mono)
c
c after fitting (or deciding not to fit) idest=0 or -1
	if(idest.le.0) then
	   print 250
250	   FORMAT(' Calculate correlations [N] ? ')
	   read 101,ans
	   if(UC(ans).EQ.'Y') goto 303	!calc correlation
	   goto 205		!where next
	endif
c	IF(IDEST.EQ.21.OR.IDEST.EQ.22) GOTO 303	   !CORRELATION TESTS
	IF(IDEST.EQ.2) GOTO 300		!FITTING
	IF(IDEST.EQ.4) PREFIT=.TRUE.
	GOTO 300		!DISP CALC CURVE WITH PRE-FITTED PARAM
c
303	call CORCAL(YVAL,NYVAL,AHIST,OHIST,GHIST,BHIST,OBHIST,
     & IDTYPE,FREQ,XAXIS,XMIN,XMAX,NBIN,FMIN,FMAX,
     & FLO,FHI,XWBASE,IDEST)
	if(idest.eq.31) goto 204	!display transformed var
	goto 205		!where next
C
300	CONTINUE
C NOW FIT DISTRIBUTION CURVE BETWEEN YLOW,YHIGH
	IF(IDEST.EQ.2.OR.IDEST.EQ.22) GOTO 304
	IF(IDEST.EQ.21) GOTO 2064	!where next?
	GOTO 3010	!PRE-CALC CURVE
304	NFIX=-1		!FOR 1ST CALL TO MLFITS
c	do 200 i=1,10
c200	print 201,i,yval(i)
c201	format(' *',i8,g13.6)
c Prepare for fit
	CALL MLFIT(THETA,YBAR,AHIST,THIST,OBHIST,FITTED,nmax,
     & k,kfit,idtype,jfix,errfac,ndisp,nbin,sdcon,tres,ymin,ymax,mono)
C Do fit
	call CSIMP(k,THETA,JFIX,AHIST,OBHIST,ELMAX,
     & NEV,ERRFAC,NDISP,errors,idtype)	!FIT
	kmax=k
c
c Save results of last 5 fits in case they are needed for superimposition
	if(nfits.eq.5) then	!arrays full, so scroll up
	   do j=1,4
		ksav(j)=ksav(j+1)		!scroll up
		idsav(j)=idsav(j+1)
		ncompsav(j)=ncompsav(j+1)
		nfitsav(j)=nfitsav(j+1)
		fitsav(j)=fitsav(j+1)
		ylosav(j)=ylosav(j+1)
		yhisav(j)=yhisav(j+1)
		do i=1,ksav(j)
		   thetsav(i,j)=thetsav(i,j+1)
		enddo
	   enddo
	endif
	if(nfits.lt.5) nfits=nfits+1		!number of fits saved
	j=nfits
	if(ahist) then
	   fitype='Gaussians'
	else if(obhist) then
	   fitype='geometrics'
	else if(thist) then
	   fitype='exponentials'
	else if(gamdst) then
	   fitype='gammas'
	endif
	ksav(j)=k
	idsav(j)=idtype
	ncompsav(j)=ncomp
	nfitsav(j)=nfit
	fitsav(j)=fitype
	ylosav(j)=ylow
	yhisav(j)=yhigh
	isgsav(j)=isg
	do i=1,k
	  thetsav(i,j)=theta(i)
	enddo
c
C TYPE  RESULTS BEFORE DISPLAY. OR, IF ERRORS WANTED, CALL PTYP1
C ONLY TO CALCULATE AMEAN,AREA,SD,ANTRUE BUT PRINT RESULTS
C LATER IN EKERR
3010	call PTYP1(THETA,AREA,AMEAN,SD,K,NCOMP,YHIGH,YLOW,NFIT,ANTRUE,
     & ERRORS,GAMDST,ISG,idtype,ahist,thist,obhist)
C
C CALL GETPAR TO INPUT PRE-DET PARAM RATHER THAN FIT DATA
	if(prefit) call GETPAR(AMEAN,AREA,SD,IDtype,ANTRUE,AHIST,THIST,
     & OBHIST,Ylow,yhigh,Ncomp,gamdst,isg)		!ID=IDTYPE
	if(prefit) GOTO 204		!TO DISPLAY FITTED CURVE
c
	if(errors) call EKERR(YLOW,YHIGH,ncomp,nfit,
     & ERRORS,antrue,gamdst,isg)
	pause 'Display follows'
	GOTO 204		!TO DISPLAY FITTED CURVE
C
C
256	idest=-1	!options 5-9 only
	call SAVEDP	!save disc printout
	goto 205	!where next?
2064	idest=0 	!all options
	call SAVEDP	!save disc printout
	goto 205	!where next?
99	CONTINUE
c Update .INI file
      OPEN(unit=19,file='EKDIST0.INI',status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=512)
	write(19,rec=1) ndev,nfile,kfile,iydat,nbad1,isbad,iebad,
     &  nfileb,kfileb,tresob,tresgb,idiskq,treso,tresg
	CLOSE(unit=19)
c
	call ENDPRINT
	CLOSE(unit=7)
	CLOSE(unit=8)
	END


	INTEGER FUNCTION IACAL(KT,iampl)
C TO GET AMPLITUDE IN ADC UNITS FROM IAMPL(KT). REMOVES 10000 IF
C THIS HAS BEEN ADDED  (FOR POS AMP) OR SUBRACTED (FOR NEG AMP)
C TO INDICATE 'ASSUMED' VALUE .
	integer*2 IAMPL(20480)
	integer*2 ix
C
	IX=0
	IF(KT.EQ.0) GOTO 1
	IX=IAMPL(KT)
	IF(IX.GT.9999) IX=IX-10000
	IF(IX.LT.-9999) IX=IX+10000
1	IACAL=int4(IX)
	RETURN
	END

	integer*4 function IAVAL(i)
c Version with IAMPL in COMMON can now replace IACAL
C To get amplitude in ADC units from IAMPL(i). Removes 10000 if
C this has been added  (for pos amp) or subtracted (for neg amp)
C to indicate 'dubious' value .
	integer*2 IAMPL(20480)
	integer*2 ix
	COMMON/AMPBLK/iampl
C
	ix=0
	if(i.eq.0) goto 1
	ix=IAMPL(i)
	if(ix.gt.9999) ix=ix-10000
	if(ix.lt.-9999) ix=ix+10000
1	IAVAL=int4(ix)
	RETURN
	END



