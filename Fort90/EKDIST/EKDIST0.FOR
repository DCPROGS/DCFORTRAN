	PROGRAM EKDIST
c
c VERSION FOR DATA FILES FROM NEW SCAN
c To do:
c (1) First latencies etc not yet incorporated
c (2) For files from new SCAN (but not those converted to new format
c	from PDP scan) have got durations of gaps that were set unusable,
c	so could add option to include/exclude them eg for shut time distn
c (3) HMAT2 not fixed to do errors for amp histos with constraint on SDs
c (4) Fix sequential excisions in STABPLOT!
c
c New version:
c (1) Keep original data, with no resolution imposed, in tint0,ampl0,
c	iprops0, so no need to re-read data if resolution changed
c (2) Keep all amplitudes in picoamps, in ampl(i)
c (3) depends on ampl(i).eq.0 to detect shuttings -this seems to be
c	quite reliable (see T0.FOR) at least when ampl(i)=float(iampl(i)) as here.
c (4) Problem -cannot have YVAL (data for histos) allocatable because want
c	to have it in COMMON/FITBLK so accessible to EXPLIK etc used for SIMPLEX
c     -now made 81920 (previously 51200 and 20480) -CHANGE NDIMY ACCORDINGLY
c
c OLD NOTES:
c May 1989. Version of EKDIS3 for Lahey Fortran
c Modif 08/11/92 07:15pm for Lahey V5.x
c Modif 07/22/92 10:35am so .INI keeps both the expt files that were
c used on the last run (nfile,kfile) AND the expt files that were used
c to define bad sections in STABPLOT (nfileb,kfileb)
c Modif 12/06/91 09:44am so 1st latencies can be read in from CJFIT (in
c	DISKIN) and fitted.  Resolution is already imposed on them (in CJFIT)
c	and values for treso,tresg read from directory in this case
c 02/28/90 06:25pm Altered so nfix,jfix are in a separate common block,
c not in LIKBLK, because jfix is needed in EXPLIK etc.to prevent alteration
c of an AREA parameter (because total area>1) when area is meant to be fixed
c
c  NINT1=no of intervals in each file read (local to this subroutine)
c  NINTT=total no of intervals in all files before resolution imposed
c  NINT=total no of intervals AFTER resolution imposed (see RESINT)
C
C****MODIF 7-JAN-82 TO CORRECT HISTO OF OPEN TIMES WITH SPEC
C	AMPLITUDE RANGE WHEN AMPS NEGATIVE. NOW USES ABSOLUTE
C	AMPS THROUGHOUT SO CANNOT DO INWARD AND OUTWARD CURRENTS
C	IN SAME RUN
c
c New declarations
c Now use allocatable arrays
	allocatable::tint,tint0
	allocatable::iampl0,ampl0,ampl
	allocatable::iprops,iprops0
	allocatable::index
	allocatable::ifreq1,ifreq2,ifreq3   !all point, shut point, open point
	allocatable::jfreq1,jfreq2,jfreq3   !temporary versions to read each file
	real*4 tint(:),tint0(:),ampl0(:),ampl(:)
	integer*2 iampl0(:)
	integer*1 iprops(:),iprops0(:)
	integer*4 index(:)
	integer*4 ifreq1(:),ifreq2(:),ifreq3(:)
	integer*4 jfreq1(:),jfreq2(:),jfreq3(:)
	integer*4 ifmin(4),ifmax(4)
	real*4 freqs(-5000:5000)		!in common/pablk/ for GAUFIT
c
	integer kfile(50)
	character*33 pfile(50)	!path names for SCANDAT files
	character name*12
	real*4 calfacs2(50)
	integer nval(50),irecs(50)
C
	real*4 xsav(5,4)
	dimension xcalc(2048),fcalc(2048)
	dimension obfreq(510),frqcal(510)
	dimension ylo(10),yhi(10),ymid(10),tcseq(10)		!for seqlst
	real*4 yloop(10),yhiop(10),ylogap(10),yhigap(10)	!for .ini
	real*4 ampval(10)	!to store amplitudes marked on stability plot
	REAL AMEAN(10),AREA(10),SD(10)	!TO HOLD PARAMETERS
	REAL theta(20)
	real thetsav(20,5),ylosav(5),yhisav(5)		!to save past fits
	integer ksav(5),idsav(5),ncompsav(5),nfitsav(5),isgsav(5) !to save past fits
	character*12 fitype,fitsav(5)			!to save past fits
	INTEGER JFIX(20)
	integer*2 kfileb(50),nfileb	!for EKDIST.INI
	integer isbad(20),iebad(20)	!for bad bits (see stabplot)
	LOGICAL OHIST,BHIST,GHIST,THIST,AHIST,pahist,OBHIST,FITTED,PREFIT
	LOGICAL ERRORS,DEBUG,CONAM1,exass,exass1,AMPOS,lastfit
	LOGICAL EXCSUB,GAMDST,logt,first,revamp,excreg
	logical pon,fastf,slock,caplock,present,readini,nopoint
c#	EXTERNAL EXPLIK,GAULIK,OBLIK,elfun3
	logical sbin,shist,sres,sexp,crembad,samex,mono
	logical discprt
	character*40 titlex,titley
	character*11 cdate,ctime
	character*2 ndev		!for DISKIN
      character*1 ans,UC,ansc
c for SCVDU
	real XVAL(0:511,3),YVAL1(0:511,3)	!call yval1 (so diff from yval above)
	real XCAL(2048,5),YCAL(2048,5)
c for histo data
	integer ndat(3),icurvd(3),ijoin(3)
c for calc curves
	integer ncal(5),icurvc(5),iline(5)
c for cjump
	logical cjump
	allocatable::jumps,kjumps0,kjumps,jumpomit,jomit
	integer*2 jumps(:),kjumps0(:),kjumps(:),jumpomit(:),jomit(:)
c
	character*40 mtitle*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	COMMON/AMPBLK/iampl
	common/sblk/sbin,shist,sres,sexp
	COMMON/LIKBLK/K,kfit,elmax,em,delmin,ndisp,theta,npar
	common/FIXBLK/nfix,jfix
	COMMON/LOGBLK/ AHIST,THIST,OBHIST
	common/rblck/treso,tresg,acrit,avamp	!for RESINT and STABPLOT
	real*4 Yval(81920)
	real*4 wfreq(51200)	!for idealized point amp
	real*4 freq(510),xaxis(510)
	COMMON/FITBLK/yval,xaxis,freq,Nyval,
     & Ylow,Yhigh,Ncomp,Nfit,ampos,pen,gamdst,isg,fastf,logt,nb1,nb2,
     & excreg,yexclow,yexchigh,isdcon,wfreq,anfit,
     & nfitt,yvalt(5120)
	COMMON/OVLBLK/OBMAX,Jth,CONAM1,exass,exass1,AMPHI,AMPLO,
     & EXCSUB,Ith
	common/PABLK/freqs,calfac2,ilow,ihigh		!for GAUFIT,MLFITS
	common/gapsav/ncomps,ameans(10),areas(10)
	common/ampsav/ncompa,ameana(10),areaa(10),sda(10)
	common/dp/discprt
C  COMMON/OVLBLK/ IS TO KEEP VALUES OF VARIABLES FROM SUBROUTINES
C  (CDIST0, CDIST3, CMEAN) SO THEY ARE NOT LOST DURING OVERLAYING
C
	pon()=slock()
	debug()=caplock()
c
101	format(a1)
C=	ndimy=51200		!size of yval,wfreq (but can't use var dimension
	ndimy=81920		!size of yval (but can't use var dimension
c				!in common/fitblk so dimensioned 81920 in all
c				!subroutines where common used
c !NB wfreq() is now of fixed dimension everywhere (51200 at present)
	ncomps=0	!set when shut time pdf fitted
	ncompa=0	!set when amplitude pdf fitted
	fitted=.false.
	lastfit=.false.
	nfits=0		!number of past fits stored
	tcfac=2.0
	call UNDER0('true')	!so underflows set=0 (Lahey manual p12.8)
c
	filnam='ekdist.prt'
	call WINPRINT
	OPEN(unit=7,file=prtport,iostat=nerr)
c
	ndimd=3
	ndimc=5
	ylo(1)=-1000.
	first=.true.
	logt=.false.	!unless reset
	tcrit=-1.		!until defined
	nfit=0
	anfit=-1.0		!signal not to use wfreq() in GAULIK
	iydat=0
	idiskq=-1		!disc for plot queue (until defined)
	ihjc=1		!default=set resolution by HJC method
	mono=icol.eq.0
	shutav=-1.0		!for cdistj
	openav=-1.0		!for cdistj
	ansc='Y'		!default for 'calculate correlations'
c NB read nbad as nbad1 from .INI so that if runs done without bad bits
c removed (nbad set to 0) the values in the .INI file will not be erased
c (unless bad bits are redefined by another STABPLOT)
	nbad=0		!no bad sections
	nbad1=0		!in case .INI not read
	readini=.false.
	INQUIRE(file='EKDIST.INI',exist=present,flen=len)
c Allocate temp array to read jumpomit() (nsweep not yet known, and must
c  start index at zero in case nomit=0)
	ALLOCATE(jomit(0:1000))
	if(present.and.len.gt.0) then
	   readini=.true.
c         OPEN(unit=19,file='EKDIST.INI',status='UNKNOWN',
c     &   access='DIRECT',form='UNFORMATTED',recl=3072)
         OPEN(unit=19,file='EKDIST.INI',status='UNKNOWN',
     &	access='TRANSPARENT')
	   read(19,rec=1) ndev,nfile,kfile,pfile,iydat,
     &	nbad1,isbad,iebad,nfileb,kfileb,tresob,tresgb,
     &	idiskq,treso,tresg,
     & 	ylogap,yhigap,nvalgap,yloop,yhiop,nvalop,
     &	nomit,(jomit(i),i=1,nomit)
	   if(debug()) then
		print 700,nbad1,idiskq
700		format(' nbad,idiskq from .INI = ',2i8)
	   endif
	   CLOSE(unit=19)
	endif
c
c Return to 205 for another expt to find where next. First time this
c only sets SBIN etc
205	call WNEXT1(idest,first)
	if(idest.eq.1) goto 99	!exit
	print 3000
	if(first) then
	   if(pon()) write(7,3000)
	   if(discprt) write(8,3000)
3000	   format(/,' EKDIST: Single channel distributions ')
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
c	if(debug().or.iydat.ne.1) then
	if(first) then
         print 12,iydat
12	   format(
     & ' (1) Read data from normal data disc',/,
     & ' (2) Get special data from C:\YVAL.DAT',/,
     & ' Option number [',i2,']  = ')
	   call INPUTi(iydat)
	endif
c
	if(iydat.le.0) iydat=1
	if(iydat.eq.1) goto 13	!after first time
	if(iydat.eq.2) then
	   call YVALIN(titlex,yval,nyval,pon(),thist,obhist,
     &     ahist,idtype,ndimy)
c          ifreq is in call to SCVDU so must allocate it even though
c		it is not used (otherwise get mystery crash when SCVDU called:
c		-says 'read beyond end of file'!)
	   ALLOCATE(ifreq1(1:1),ifreq2(1:1),ifreq3(1:1))
	   fitted=.false.
	   prefit=.false.
	   logt=.false.
	   goto 210
	endif
13	continue
c
	if(sexp) goto 20
c
	if(.not.first) then	!deallocate arrays before 'new expt'
	   DEALLOCATE(tint0,iampl0,iprops0,ampl0)
	   DEALLOCATE(tint,ampl,iprops)
	   DEALLOCATE(index)
	   DEALLOCATE(ifreq1,ifreq2,ifreq3)
	   DEALLOCATE(jumps,kjumps0,kjumps,jumpomit)
	endif
c First part of data reading now done in DISKIN1, to define which files are
c to be read.  Then return to main prog to ALLOCATE tint(),iampl(),iprops()
c and ifreq1 etc.  If disk has not got data for point amp histo (nopoint=true)
c then imin, imax set to 1 (must still allocate array for subroutine calls)
	call DISKIN1(nfile,kfile,pfile,nval,irecs,calfacs2,nintt,
     & avamp,rms,ffilt,trise,fc,iscan,first,readini,ndev,idest,
     & imin,imax,nopoint,name,idiscn,cjump,nsweep,tzero,tsamp)
	if(idest.eq.99) goto 99		!out
c
c Now have number of intervals, so allocate arrays
	ALLOCATE(tint0(nintt),iampl0(nintt),iprops0(nintt),ampl0(nintt))
	ALLOCATE(tint(nintt),ampl(nintt),iprops(nintt))
	ALLOCATE(index(nintt))
	ALLOCATE(ifreq1(imin:imax),ifreq2(imin:imax),ifreq3(imin:imax))
	ALLOCATE(jfreq1(imin:imax),jfreq2(imin:imax),jfreq3(imin:imax))   !temp
	if(.not.cjump) nsweep=1		!must allocate jumps,kjumps anyway
	ALLOCATE(jumps(nsweep),kjumps0(nsweep),kjumps(nsweep),
     & jumpomit(nsweep))
c
c Read data into these arrays
	call DISKIN2(tint0,iampl0,ampl0,iprops0,nintt,nfile,
     & kfile,pfile,calfacs2,calfac2,nval,irecs,nopoint,
     & ifreq1,ifreq2,ifreq3,imin,imax,jfreq1,jfreq2,jfreq3,
     & cjump,nsweep,jumps,kjumps0,jumpomit,nomit)
	DEALLOCATE(jfreq1,jfreq2,jfreq3)	!no longer needed
c Copy values from .ini
	if(first) then
	   if(cjump) then
		do i=1,nomit
		   jumpomit(i)=jomit(i-1)
	      enddo
	   endif
	   DEALLOCATE(jomit)
	endif
c Check for zero length intervals
	n=0
	do i=1,nintt
	   if(tint0(i).lt.0.0001) n=n+1
	enddo
	if(n.gt.0) then
	   print 25,n
25	   format(/,
     &' Data contain ',i4,' intervals shorter than 0.1 microsec:',
     &' list them [N] ? ')
	   read 101,ans
	   if(UC(ans).eq.'Y') then
	      do i=1,nintt
		   if(tint0(i).lt.0.0001) then
			print 24,i,tint0(i),ampl0(i),iprops0(i)
			if(discprt) write(8,24) i,tint0(i),ampl0(i),iprops0(i)
24			format(
     &' Zero length interval # ',i5,' length, amp, props = ',2g13.6,i3)
		   endif
		enddo
		print 108
		if(discprt) write(8,108)
	   endif
	endif
c
20	continue	!skip DISKIN when sexp=true (same experiments to be used)
c
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
	imode=0		!call RESINT from EKDIST
c	call DCASK('Use RESINT4','Y',ans)	!temp=================
c	if(ans.eq.'N') then
	   call RESINT3(tint0,tint,ampl0,ampl,iprops0,iprops,nint,
     &     nintt,imode,ffilt,fc,rms,trise,index,ihjc,
     &     cjump,nsweep,kjumps0,kjumps)
c	else
c	   call RESINT4(tint0,tint,ampl0,ampl,iprops0,iprops,nint,
c     &     nintt,imode,ffilt,fc,rms,trise,index,
c     &     cjump,nsweep,kjumps0,kjumps)
c	endif
c Choice of distn NOW moved from end of RESINT to main prog, below
c (previously idest=distribution type on exit from RESINT)
c If bad sections to be removed (and resolution has not been changed)
c then do it (once!) here, now resolution has been imposed
	if(crembad) then
	   call REMBAD(tint,ampl,iprops,nint,nintt,nbad,
     &	isbad,iebad,pon)
c==	   call REMBAD(tint,iampl,nint,nbad,isbad,iebad,pon())
	   crembad=.false.	!so not done again
	endif
407	continue
	if(shist) idest=idsav1
	if(shist) goto 411
	iopt=idsav1
	if(iopt.lt.1) iopt=1
	if(.not.cjump) then
	   print 100,iopt
100	   format(
     & ' (1) Amplitudes',/,
     & ' (2) Shut times (and times when NOT in specified amp range)',/,
     & ' (3) Open times and open periods',/,
     & '       Also open-shut, open-open conditional pdfs and means',/,
     & '         (and times when IN specified amp range)',/,
     & ' (4) Bursts',/,
     & ' (5) Burst means (lists various burst chacteristics)',/,
     & ' (6) Subconductance sequence listing',/,
     & ' (7) Stability plots for open periods,shut times or P(open)',/,
     & ' (8) Stability plots for amplitudes',/,
     & ' Option number [',i2,'] = ')
	   call INPUTi(iopt)
	   if(iopt.lt.1.or.iopt.gt.8) goto 407
	else
	   print 102,iopt
102	   format(
     & ' (1) Amplitudes (all jumps)',/,
     & ' (2) Shut times after jump',/,
     & ' (3) Open periods (length or number) after jump',/,
     & '     (and means for open|adjacent shut and open|open)',/,
     & ' (4) Activation length and open time per jump',/,
     & ' (5) Bursts within jumps',/,
     & ' Option number [',i2,'] = ')
	   call INPUTi(iopt)
	   if(iopt.lt.1.or.iopt.gt.5) goto 407
	endif
	idest=iopt
	idsav1=idest		!save dist type

411	if(.not.shist) then
	 if((idest.eq.4.and.(.not.cjump)).or.(idest.eq.5.and.cjump)) then
	   call DCASK('Calculate Tcrit to define bursts','n',ans)
	   if(UC(ans).EQ.'Y') then
		kth=0		!print all
		call TCRITS(ameans,areas,ncomps,kth,.false.)
	   endif
	 endif
	endif
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
	   call STABPLOT(tint,ampl,iprops,nint,nintt,nbad,nbad1,isbad,
     &    iebad,namp,ampval,acal,trise,tcfac,iscan,idiskq,
     & 	index,iopt,mono)
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
C NOW GET PARAMETERS FOR HISTOGRAMS.
	lastfit=fitted.and.sexp.and.shist		!redisplay with same fit?
	fitted=.false.
	PREFIT=.FALSE.
c
c Jump distributions for shut, open and bursts done in CDISTJ
	if(cjump.and.idest.ge.2.and.idest.le.5) then
	   call CDISTJ(tint,ampl,iprops,nint,nintt,ndimy,
     & 	yval,nyval,ohist,bhist,IDtype,ghist,ahist,pahist,obhist,
     &	treso,tresg,tres,idest,idsav,shutav,openav,sdopen,tcrit,
     & 	nsweep,jumps,kjumps,jumpomit,nomit,nth,yvalt,nfitt,nnull,
     &       ylogap,yhigap,nvalgap,yloop,yhiop,nvalop)
	   if(idtype.eq.104.or.idtype.eq.113.or.idtype.eq.115.or.
     &	  idtype.eq.116) then	!means only, no graph
		goto 256	!options 5-9 only
	   else
	      goto 210
	   endif
c	endif
c	if(idest.eq.4) GOTO 402			!BURST DISTNs
c	if(idest.eq.5) GOTO 403			!CMEAN
c	if(idest.eq.6) goto 404			!Sequences
c Use CDIST0 for idest=1,2,3 -new version =cdist1
	else if(idest.le.3) then
401	  call CDIST1(tint,ampl,iprops,nint,nintt,ndimy,
     &   yval,nyval,ohist,bhist,IDtype,ghist,ahist,pahist,obhist,
     &   trise,iscan,treso,tresg,tres,idest,idsav,tcfac,nopoint,
     &   revamp,wfreq,ylogap,yhigap,nvalgap,yloop,yhiop,nvalop)
	  if(OHIST.and.idtype.eq.4) goto 303	!open-shut correlation
	  if(pahist) goto 204	!don't call setbin for point amps
	  goto 210
c
	else if(idest.eq.4) then
402	  call CDIST2(tint,ampl,iprops,nint,nintt,ndimy,avamp,acrit,
     &   Yval,Nyval,iscan,tcrit,tc,nbst,obbar,ohist,bhist,IDtype,
     &   ghist,ahist,pahist,obhist,iexc,ISG,treso,tresg,tres,trise,
     &   revamp,idsav,ibamp,ibtype)
	  if(idtype.eq.11) goto 303	!correlation
	  goto 210
c
	else if(idest.eq.5) then
403	   call CMEAN1(tint,ampl,iprops,nint,nintt,avamp,acrit,
     &    tcrit,TC,nbst,ISG)
	   goto 256	!where next?
c
	else if(idest.eq.6) then
404	   call SEQLST(tint,ampl,iprops,nint,nintt,avamp,acrit,
     &    trise,iscan,ylo,yhi,ymid,ksav,treso,tresg,tcseq,index,
     &    kfile,nfile,name,idiscn,nvalsav)
c     &   trise,iscan,ylo,yhi,ymid,ksav,treso,tresg,tcseq,index)
	   goto 256	!where next?
	endif
c
210	continue
c Set bin widths (previously this was last part of CDIST)
	call SETBIN1(YVAL,FREQ,XAXIS,nyval,thist,ohist,bhist,
     & ghist,obhist,logt,tres,obmax,idtype,xsav,ybar,nbin,nbw,
     & xwbase,nbdec,fmin,fmax,flo,fhi,ymin,ymax,ndimy,ahist,wfreq,
     & cjump,yvalt,nfitt,idest)
	if(nbin.eq.0) then
	   if(idest.eq.-1) goto 249		!do correlations?
	   goto 256	!skip histo; where next?
	endif
C
C NOW DISPLAY HISTOGRAM OF ALL DATA IN YVAL
c	IF(IDTYPE.EQ.11) GOTO 303	!now before setbin
c Define labels etc for graph
204	continue
c	call SCVD1(NBIN,XWBASE,FITTED,PREFIT,AHIST,THIST,OBHIST,
c     & idest,ohist,ghist,bhist,IDtype,isg,jth,xaxis,logt,ncalc,
c     & xcalc,fcalc,titlex,titley,xw,ilabel,iydat,latency)
c
c All now done inside SCVDU
c Plot graphs
c	call SCVDU(XAXIS,XMIN,XMAX,FREQ,nbin,fmin,fmax,flo,fhi
c     & FITTED,PREFIT,AHIST,OBHIST,THIST,tres,idest,xtic,ftic,idiskq,
c     & logt,first,titlex,titley,xwbase,xw,ilabel,ncalc,xcalc,fcalc,
c     & obfreq,frqcal,theta,ncomp,kmax,ntx,nty,itx,ity,idtype,
c     & amean,area,sd,antrue,isg,gamdst,sdcon,lastfit,
c     & nfits,thetsav,ksav,idsav,ncompsav,nfitsav,ylosav,yhisav,
c     & isgsav,fitsav,mono)
c NB call with yval1(:,:) for the VHIST data
	call SCVDU(xaxis,xmin,xmax,freq,nbin,fmin,fmax,flo,fhi,
     & fitted,prefit,ahist,pahist,obhist,thist,tres,xtic,ftic,idiskq,
     & idest,ohist,ghist,bhist,jth,iydat,revamp,
     & logt,first,titlex,titley,xwbase,xw,ilabel,ncalc,xcalc,fcalc,
     & obfreq,frqcal,theta,ncomp,ntx,nty,itx,ity,idtype,
     & amean,area,sd,antrue,isg,gamdst,isdcon,lastfit,
     & nfits,thetsav,ksav,idsav,ncompsav,nfitsav,ylosav,yhisav,
     & isgsav,fitsav,mono,ndimd,ndimc,
     & xval,yval1,xcal,ycal,ndat,icurvd,ijoin,ncal,icurvc,iline,
     & imin,imax,ifreq1,ifreq2,ifreq3,ifmin,ifmax,
     & excreg,yexclow,yexchigh,anfit,nsweep,nnull,nfitt,
     & cjump,nth)
c===are ifmin,ifmax needed as args -prob not!
c
c After fitting (or deciding not to fit) idest=0 or -1
c NB In cjump case all correlations (open/shut or open/open) are done
c by CORCALJ (called in CDISTJ)
249	continue
c	if(idest.le.0.and.cjump) goto 205	!where next
	if(idest.le.0.and.cjump) goto 257	!where next
	if(idest.le.0) then
	   goto 303		!calc correlations?
	endif
c	IF(IDEST.EQ.21.OR.IDEST.EQ.22) GOTO 303	   !CORRELATION TESTS
	IF(IDEST.EQ.2) GOTO 300		!FITTING
	IF(IDEST.EQ.4) PREFIT=.TRUE.
	GOTO 300		!DISP CALC CURVE WITH PRE-FITTED PARAM
c
303	continue
	if(ohist.and.idtype.eq.1) then
	   call DCASK(
     &   'Calculate autocorrelations between open times',ansc,ansc)
	else if(ohist.and.idtype.eq.2.or.idtype.eq.112.or.
     &	idtype.eq.115.or.idtype.eq.116) then
	   call DCASK(
     &   'Calculate autocorrelations between open periods',ansc,ansc)
	else if(ghist.and.idtype.eq.1) then
	   call DCASK(
     &   'Calculate autocorrelations between shut times',ansc,ansc)
	else
	   call DCASK(
     &  'Calculate autocorrelations for current distribution',ansc,ansc)
	endif
c	if(UC(ansc).eq.'N') goto 205		!where next
	if(UC(ansc).eq.'N') goto 257		!where next
	if(ohist.and.idtype.eq.1) then
	   print 21
	   if(pon()) write(7,21)
	   if(discprt) write(8,21)
21	   format(//,' AUTOCORRELATIONS BETWEEN INDIVIDUAL OPEN TIMES')
	else if(ohist.and.idtype.eq.2.or.idtype.eq.112.or.
     &	idtype.eq.115.or.idtype.eq.116) then
	   print 22
	   if(pon()) write(7,22)
	   if(discprt) write(8,22)
22	   format(//,' AUTOCORRELATIONS BETWEEN LENGTHS OF OPEN PERIODS')
	else if(ghist.and.idtype.eq.1) then
	   print 23
	   if(pon()) write(7,23)
	   if(discprt) write(8,23)
23	   format(//,' AUTOCORRELATIONS BETWEEN LENGTHS OF SHUT TIMES')
	endif
	call CORCAL(YVAL,NYVAL,OHIST,GHIST,BHIST,OBHIST,
     & 	IDTYPE,IDEST,ndimy)
c==	if(idest.eq.31) goto 204	!display transformed var -REMOVED!
	if(ohist.and.idtype.eq.4.or.
     &	(cjump.and.(idtype.eq.104.or.idtype.eq.113))) then
	   idest=-1	!options 5-9 only
	endif
c	goto 205		!where next
	goto 257		!where next
C
300	CONTINUE
C NOW FIT DISTRIBUTION CURVE BETWEEN YLOW,YHIGH
	IF(IDEST.EQ.2.OR.IDEST.EQ.22) GOTO 304
	IF(IDEST.EQ.21) GOTO 257	!where next?
	GOTO 3010	!PRE-CALC CURVE
304	continue
	nfix=0
c	do 200 i=1,10
c200	print 201,i,yval(i)
c201	format(' *',i8,g13.6)
c Prepare for fit and do it
	call MLFITS(theta,ybar,ahist,thist,obhist,fitted,nfix,
     & k,kfit,idtype,jfix,errfac,ndisp,nbin,tres,ymin,ymax,mono,
     & elmax,nev,errors,pahist,revamp,
     & cjump,tzero,tsamp)
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
     & ERRORS,GAMDST,ISG,idtype,ahist,thist,obhist,ghist,
     & excreg,yexclow,yexchigh,isdcon,anfit,nsweep,nnull,nfitt)
C
C CALL GETPAR TO INPUT PRE-DET PARAM RATHER THAN FIT DATA
	if(prefit) call GETPAR(AMEAN,AREA,SD,IDtype,ANTRUE,AHIST,THIST,
     & OBHIST,Ylow,yhigh,Ncomp,gamdst,isg)		!ID=IDTYPE
	if(prefit) GOTO 204		!TO DISPLAY FITTED CURVE
c
	if(errors) call EKERR(YLOW,YHIGH,ncomp,nfit,
     & ERRORS,antrue,gamdst,isg,idtype,ghist,
     & excreg,yexclow,yexchigh,isdcon,anfit,nsweep,nnull,nfitt)
	pause 'Display follows'
	GOTO 204		!TO DISPLAY FITTED CURVE
C
C
256	idest=-1	!options 5-9 only
	call SAVEDP		!save disc printout
	goto 205 		!where next?
257	idest=0 		!all options
	call SAVEDP		!save disc printout
	goto 205		!where next?
99	CONTINUE
c Update .INI file
      OPEN(unit=19,file='EKDIST.INI',status='UNKNOWN',
     &	access='TRANSPARENT')
c      OPEN(unit=19,file='EKDIST.INI',status='UNKNOWN',
c     & access='DIRECT',form='UNFORMATTED',recl=3072)
	write(19,rec=1) ndev,nfile,kfile,pfile,iydat,
     &	nbad1,isbad,iebad,nfileb,kfileb,tresob,tresgb,
     &	idiskq,treso,tresg,
     & 	ylogap,yhigap,nvalgap,yloop,yhiop,nvalop,
     &	nomit,(jumpomit(i),i=1,nomit)
	CLOSE(unit=19)
c
	call ENDPRINT
	CLOSE(unit=7)
	CLOSE(unit=8)
	END

