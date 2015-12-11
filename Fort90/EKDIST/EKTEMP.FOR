	PROGRAM EKDIST
c
c VERSION FOR DATA FILES FROM NEW SCAN
c
c Modif 08/29/00 06:53am to enable data sets at different conc to be fitted
c with common tau values.  Changes are as follows
======NB FOR FITTING EXPONENTIALS DO NOT, UNLIKE HJCFIT, NEED TO KNOW
======WHAT AGONIST CONC IS, BUT AT PRESENT CAN FIT SEVERAL FILES AT ONCE, BUT
======THEY HAVE ALL PARAMETERS IN COMMON -STILL NEED PFILE(I1,I2) SO CAN
======SPECIFIY WHICH DATA SETS HAVE COMMON AREAS AND WHICH HAVE DIFF AREAS, EVEN
=====IF ALL HAVE THE SAME TAU
TRY DIFF METHOD FROM THAT USED IN HJCFIT FOR MULTIPLE SETS TO AVOID CONVERTING
ALL ARRAYS TO 2D.  KEEP ALL TINT() ETC IN ONE LONG 1D ARRAY BUT STORE
BOUNDARIES OF SETS WHEN NEC
EG SET 1=1,...,N1
   SET 2=n1+1,...,N2 ETC
c  ibound0(i)=[n10,n20, . . . ], i=1,nset   (in raw data)
c  ibound(i)=[n1,n2, . . . ], i=1,nset   (after res)
c When, as usual, nset=1, have only ibound(1)=ninnt.  Most subroutines should
c not need to be changed, but in those in which sets have to be distinguished
c can have a loop like this
c	i1=0
c	do n=1,nset
c	   i1=i1+1
c	   i2=ibound(n)
c	   do i=i1,i2
c		tin=tint(i)
c		===
c	   enddo
c	enddo
c NB must be able to display fit to each set separately
v
c
c Changes apply to all distributions except amplitudes
c
c         OLD                         NEW
c  character*33 pfile(50)	  character*40 pfile(20,10)
c         n/a                         nset
c         n/a	             real*4 concA(10),concB(10)
=======
c
c Modif 05/09/00 09:41am: the modification below for display causes problems
c when more then one scan.dat file is pooled -the value of nfits will not be
c same for each and must be kept for each, but more seriously, the pooled
c scan.dat files may not even be from the same CONSAM, and if they are they
c will not be exactly contiguous.  Only sensible solution is probably
c to allow display of original consam only when nfile=1
c
c Modif 04/03/99 03:35pm: nfits added to params of diskin1 so timsav etc can
c be read in diskin2, to allow display of original channels in VSAMP (cdist2,
c and SEQLST)
c
c Modif 12/29/98 02:14pm to deal with new form of SCAN data files (iscanver=103
c or more).  Use new variable
c    idatyp=1 for new SCAN.DAT
c    idatyp=2 for original SCANDAT.DAT
c
c 11/13/97 03:56pm Modif throughout for 50 paramters!
c    theta(50),theta(50,5), jfix(50), titlep(5)
c
c To do:
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
c Modified 11/04/97 05:59pm titlep() defined in mlfits for use in likint
c
c Modified 12/16/96 11:50am so kjumps() is now integer*4 throughout (except
c for local integer*2 verion in DISKIN2 which is needed to read values from
c scandat.dat, until it new version with integer*4 for all progs is fixed -
c also need to make istrec() integer*4 in cjump, scan etc)
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
	real*4 tint(:,:),tint0(:,:),ampl0(:,:),ampl(:,:)
	integer*2 iampl0(:,:)
	integer*1 iprops(:,:),iprops0(:,:)
	integer*4 index(:,:)
===ifreq etc also 2D?
	integer*4 ifreq1(:),ifreq2(:),ifreq3(:)
	integer*4 jfreq1(:),jfreq2(:),jfreq3(:)
	integer*4 ifmin(4),ifmax(4)
	real*4 freqs(-5000:5000)	!in common/pablk/ -use also for Patlak
	allocatable:: ameanval,sdval
	real*4 ameanval(:),sdval(:)
c For multiple sets
	real*4 concA(10),concB(10)
c
c Arrays for use in display of consam data
	allocatable timsav,ifits,ibaselin
===next 4 also 2D?
	real*8 timsav(:)
      real*8 dfinter
	integer*2 ibaselin(:)
	integer*4 ifits(:)
c For Patlak frequencies
c	real*4 Yfreq(-2047:2048) !OK for plus/minus 20.4 pA (use freqs())
c
c    to read old .ini files
c==	character*33 pfile(50)	!path names for SCANDAT files
	ALLOCATABLE::pfile0,kfile0,kfileb0,isbad0,iebad0,tcvals0
	character*33 pfile0(:)	!to read old pfile
	integer kfile0(:),isbad0(:),iebad0(:)
	integer*2 kfileb0(:),nfileb0
	real*4 tcvals0(:)
c
	character*40 pfile(20,10)
	character name*12
c==	real*4 calfacs2(50)
c==	integer nval(50),irecs(50),nswps(50)
c==	real*4 tcvals(50)
	real*4 calfacs2(20,10)
	integer nval(20,10),irecs(20,10)
	real*4 concA(10),concB(10)		!to hold concentrations for each expt
	real*4 concA1(10),concB1(10)		!to hold concentrations for each expt
	real*4 tcrit(10),tresol(10),tresolb(10)
	real*4 tcvals(20,10)
===next 2 also 2D?
	integer iexstrt0(50),iexend0(50)
	integer iexstrt(50),iexend(50)
	logical onetcrit,opendown,oneshut
C
	real*4 xsav(5,4)
	dimension xcalc(2048),fcalc(2048)
	dimension obfreq(510),frqcal(510)
	dimension ylo(10),yhi(10),ymid(10),tcseq(10)		!for seqlst
	real*4 yloop(10),yhiop(10),ylogap(10),yhigap(10)	!for .ini
	real*4 ampval(10)	!to store amplitudes marked on stability plot
	REAL AMEAN(10),AREA(10),SD(10)	!TO HOLD PARAMETERS
	REAL theta(50)
	real thetsav(50,5),ylosav(5),yhisav(5)		!to save past fits
	integer ksav(5),idsav(5),ncompsav(5),nfitsav(5),isgsav(5) !to save past fits
	character*12 fitype,fitsav(5)			!to save past fits
	INTEGER JFIX(50)
c
c==	integer kfile(50)
c==	integer*2 kfileb(50),nfileb	!for EKDIST.INI
c==	integer isbad(20),iebad(20)	!for bad bits (see stabplot)
===NB kfileb(), nfileb are now integer*4
	integer kfile(20,10),nfile(10),nfileb(10),nintt(10),nint(10)
	integer kfileb(20,10)
	integer isbad(20,10),iebad(20,10)	!for bad bits (see stabplot)
	integer nbad(10),nbad1(10)

	LOGICAL OHIST,BHIST,GHIST,THIST,AHIST,pahist,OBHIST,FITTED,PREFIT
	logical pophist	!in common -true for fittting Gaussian to Popen/bst
	LOGICAL ERRORS,DEBUG,CONAM1,exass,exass1,AMPOS,lastfit,errflag
	LOGICAL EXCSUB,GAMDST,logt,first,revamp,excreg,nocor
	logical pon,fastf,slock,caplock,present,readini,nopoint
c#	EXTERNAL EXPLIK,GAULIK,OBLIK,elfun3
	logical sbin,shist,sres,sexp,crembad,samex,mono,stabcut
	logical btest,dispmean,samexp
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
===next 2 lines 2D?
	integer*2 jumps(:),jumpomit(:),jomit(:)
	integer*4 kjumps0(:),kjumps(:)
c
	character*20 TITLEP(50)		!names of params for fit
c  For parsname etc
	character path*30,pname*8,suffix*3	!for parsname
	logical nopath
	logical dprt,csfound,digchar
	character prtfil*40
	character defname*6,cnum*11
c
	character*40 mtitle*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	COMMON/AMPBLK/iampl
	common/sblk/sbin,shist,sres,sexp
	COMMON/LIKBLK/K,kfit,elmax,em,delmin,ndisp,theta,npar
	common/FIXBLK/nfix,jfix
	COMMON/LOGBLK/ AHIST,THIST,OBHIST
	common/logblk1/pophist	!in setbin1,gaulik
	common/rblck/treso,tresg,acrit,avamp	!for RESINT and STABPLOT
	real*4 Yval(81920)
	real*4 wfreq(51200)	!for idealized point amp
	real*4 freq(510),xaxis(510)
	COMMON/FITBLK/yval,xaxis,freq,Nyval,
     & Ylow,Yhigh,Ncomp,Nfit,ampos,pen,gamdst,isg,fastf,logt,nb1,nb2,
     & excreg,yexclow,yexchigh,isdcon,wfreq,anfit,
     & nfitt,yvalt(5120),iconst
	COMMON/OVLBLK/OBMAX,Jth,CONAM1,exass,exass1,AMPHI,AMPLO,
     & EXCSUB,Ith
	common/PABLK/freqs,calfac2,ilow,ihigh		!for GAUFIT,MLFITS
	common/gapsav/ncomps,ameans(10),areas(10)
	common/ampsav/ncompa,ameana(10),areaa(10),sda(10)
	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls
	common/popvalx/mxlo,mylo,myhi,ictx,ibkx,icfx	!values for poptext calls
	logical excamp						!for RESINT5
	common/censor/excamp,alo,ahi				!for RESINT5
	character*33 adcfil,adcfil1
	character*40 qfile
	common/queue/qfile	!for vplot and vhist
c Need topen=overall mean open period in TCRITS/BLFUNC (calc in cdist1 at present
c from observed values rather than from fit)
	common/top/topen
	common/dp/discprt
C  COMMON/OVLBLK/ IS TO KEEP VALUES OF VARIABLES FROM SUBROUTINES
C  (CDIST0, CDIST3, CMEAN) SO THEY ARE NOT LOST DURING OVERLAYING
	logical mouse_on,mous_set,horizont
	common/mousval/mouse_on,nbutton
C
	pon()=slock()
	debug()=caplock()
c
c Use F90 routine to prevent underflow crashes??
	errflag=.true.
	call UNDFL(errflag)
c call gino here to avoid repeated calls in vsamp
	call GINO
	call VGA
	call VIDEOMOD(3)
c
101	format(a1)
	call SETMOUSE()		!define values in common\mousval\ (in IVLIB)
C=	ndimy=51200		!size of yval,wfreq (but can't use var dimension
c	ndimy=81920		!size of yval (but can't use var dimension
	ndimy=150000		!size of yval (but can't use var dimension
c				!in common/fitblk so dimensioned 81920 in all
c				!subroutines where common used
c !NB wfreq() is now of fixed dimension everywhere (51200 at present)
	ncomps=0	!set when shut time pdf fitted
	ncompa=0	!set when amplitude pdf fitted
	fitted=.false.
	lastfit=.false.
	stabcut=.false.
	dispmean=.false.
	topen=-1.		!until defined
	nfitdone=0		!number of past fits stored
	tcfac=2.0
	nset=1
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
	onetcrit=.true.
	nfit=0
	anfit=-1.0		!signal not to use wfreq() in GAULIK
	iydat=0
	idiskq=-1		!disc for plot queue (until defined)
c	ihjc=1		!default=set resolution by HJC method
	mono=icol.eq.0
	shutav=-1.0		!for cdistj
	openav=-1.0		!for cdistj
	ansc='N'		!default for 'calculate correlations'
	nocor=.false.
	isolate=2
c Values for pop-ups
c	common/popvalx/mxlo,mylo,myhi,ictx,ibkx,icfx	!values for poptext calls
	mxlo=70		!ixlo for poptext boxes -in common/popvals/
	mylo=-1		!iylo for poptext boxes (-1 -> define top LH corner)
	myhi=400		!iyhi for poptext boxes (-1 -> define bottom LH corner)
c	ictx=14		!yellow text
c	ibkx=1		!dark blue background
c	icfx=14		!yellow border
	ictx=2		!dark green text
	ibkx=15		!white background
	icfx=10		!bright green border
c Values for POPMENU calls
c	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls
	nxlo=100
	nylo=-1
	nyhi=470
	ictm=15		!text white
	ibkm=8		!background dark grey
	icfm=2		!frame/title green
	icupm=12		!upper case red
c NB read nbad as nbad1 from .INI so that if runs done without bad bits
c removed (nbad set to 0) the values in the .INI file will not be erased
c (unless bad bits are redefined by another STABPLOT)
	do j=1,10
	   nbad(j)=0		!no bad sections
	   nbad1(j)=0		!in case .INI not read
	enddo
	readini=.false.
	INQUIRE(file='EKDIST.INI',exist=present,flen=len)
	newini=len.eq.10240
c Allocate temp array to read jumpomit() (nsweep not yet known, and must
c  start index at zero in case nomit=0)
c Modif 08/29/00 06:46am for ini=10240
===add nset etc to .ini (as in hjcfit)

	ALLOCATE(jomit(0:1000))
	if(present.and.len.gt.0) then
	   readini=.true.
c         OPEN(unit=19,file='EKDIST.INI',status='UNKNOWN',
c     &   access='DIRECT',form='UNFORMATTED',recl=3072)
	    if(newini) then
	      read(19,rec=1) ndev,nfile,kfile,pfile,iydat,
     &	nbad1,isbad,iebad,nfileb,kfileb,tresob,tresgb,
     &	idiskq,treso,tresg,
     & 	ylogap,yhigap,nvalgap,yloop,yhiop,nvalop,
     &	nomit,(jomit(i),i=1,nomit),
     &	onetcrit,tcvals,tcrit0,qfile,
     &	nvalsav,(ylo(i),i=1,10),(yhi(i),i=1,10),
     &	(tcseq(i),i=1,10),opendown,oneshut,adcfil1,
     &	nset,nmax,ntot,concA,concB
	    else		!read old, and rewrite as new format
		ALLOCATE(pfile0(50),kfile0(50),kfileb0(50),tcvals0(50))
		ALLOCATE(isbad0(20),iebad0(20)
		OPEN(unit=20,file='EKDIST.INI',status='UNKNOWN',
     &	access='TRANSPARENT')
	      read(20,rec=1) ndev,nfile0,kfile0,pfile0,iydat,
     &	nbad11,isbad0,iebad0,nfileb0,kfileb0,tresob,tresgb,
     &	idiskq,treso,tresg,
     & 	ylogap,yhigap,nvalgap,yloop,yhiop,nvalop,
     &	nomit,(jomit(i),i=1,nomit),
     &	onetcrit,(tcvals0(i),i=1,nfile),tcrit,qfile,
     &	nvalsav,(ylo(i),i=1,10),(yhi(i),i=1,10),
     &	(tcseq(i),i=1,10),opendown,oneshut,adcfil1
c		convert old arrays to new format
		nset=1
		nfile(1)=nfile0
		tcrit(1)=tcrit0
		nfileb(1)=int4(nfileb0)
		do i=1,nfile(1)
		   pfile(i,1)(1:33)=pfile0(i)
		   pfile(i,1)(34:40)='       '
		   kfile(i,1)=kfile0(i)
		   kfileb(i,1)=int4(kfileb0(i))
		   tcvals(i,1)=tcvals0(i)
		enddo
		nbad1(1)=nbad11
		do i=1,20
		   isbad(i,1)=isbad0(i)
		   iebad(i,1)=iebad0(i)
		enddo
		DEALLOCATE(pfile0,kfile0,kfileb0,tcvals0)
		DEALLOCATE(isbad0,iebad0)
		CLOSE(unit=20)
		nset=1
c           now re-write .ini in new format
		OPEN(unit=19,file='EKDIST.INI',status='UNKNOWN',
     &	 access='DIRECT',form='UNFORMATTED',recl=10240)
	      write(19,rec=1) ndev,nfile,kfile,pfile,iydat,
     &	nbad1,isbad,iebad,nfileb,kfileb,tresob,tresgb,
     &	idiskq,treso,tresg,
     & 	ylogap,yhigap,nvalgap,yloop,yhiop,nvalop,
     &	nomit,(jomit(i),i=1,nomit),
     &	onetcrit,tcvals,tcrit,qfile,
     &	nvalsav,(ylo(i),i=1,10),(yhi(i),i=1,10),
     &	(tcseq(i),i=1,10),opendown,oneshut,adcfil1,
     &	nset,nmax,ntot,concA,concB
c	   read(19,rec=1) ndev,nfile,kfile,pfile,iydat,
c     &	nbad1,isbad,iebad,nfileb,kfileb,tresob,tresgb,
c     &	idiskq,treso,tresg,
c     & 	ylogap,yhigap,nvalgap,yloop,yhiop,nvalop,
c     &	nomit,(jomit(i),i=1,nomit),
c     &	onetcrit,(tcvals(i),i=1,nfile),tcrit,qfile,
c     &	nvalsav
c	   if(nvalsav.ge.1.and.nvalsav.le.10) then
c	   endif
	   if(idatyp.lt.1.or.idatyp.gt.2) idatyp=1
	   if(debug()) then
		print 700,nbad1,idiskq
700		format(' nbad,idiskq from .INI = ',2i8)
	   endif
c	   do i=1,10
c		pfile(i)(1:1,1)='D'		!==========temp fix
c	   enddo
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
c	   call GETSPEC('ekdist.exe')
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
	if(first) then
         print 12,iydat
12	   format(/,
     & ' (1) Read data from SCAN.SCN (scan.dat) file',/,
     & ' (2) Read data from old SCANDAT.DAT disc',/,
     & ' (3) Get special data from C:\YVAL.DAT',/,
     & ' Option number [',i2,']  = ')
	   call INPUTi(iydat)
	   if(iydat.eq.1.or.iydat.eq.2) idatyp=iydat
	endif
c
	if(iydat.eq.3) then
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
	call DISKIN1(idatyp,nfile,kfile,pfile,nval,irecs,calfacs2,nintt,
     & avamp,rms,ffilt,trise,fc,iscan,first,readini,ndev,idest,
     & imin,imax,nopoint,name,idiscn,cjump,nswps,tzero,tsamp,toffset,
     & qfile,adcfil,npatch,defname,nfits,dfinter,sfac2,npfilt,
     & treso,tresg,
     & nfileb,pfileb,nmax,ntot,nset,concA,concB,samexp)
c The value of npfilt in scan.dat is actually the number of points
c to jump after a fit, so npfilt is recalculated now (as in INSCAN)
c Recalculate npfit (see above)
	if(readini) then
	   call CHKFILE(adcfil1,nerr,33)
	   if(nerr.eq.0) adcfil=adcfil1	!name from .ini if valid
	endif
c   Define tf50 etc (see GAUFILT.MCD)
	tf1=101.2187/ffilt
	tf99=717.7249/ffilt
	npfilt=ifixr((tf99-tf1)/sngl(dfinter))
c
	if(idest.eq.99) goto 99		!out
c      OPEN(unit=19,file='EKDIST.INI',status='UNKNOWN',
c     &	access='TRANSPARENT')
	 OPEN(unit=19,file='EKDIST.INI',status='UNKNOWN',
     &	 access='DIRECT',form='UNFORMATTED',recl=10240)
	  write(19,rec=1) ndev,nfile,kfile,pfile,iydat 		!KEEP FILE NAMES
	CLOSE(unit=19)
c
c Now have number of intervals, so allocate arrays
c The new treatment of the junction between separate steady state
c expt files is insert an unusable shut time between the last opening of
c one file and the first opening of the next (unless a file ends with
c a shutting in which case the existing shutting is set unusable).
c Also add unusable shutting after last opening (if not already there)
c so because this shutting may not be at end of a burst
c Thus may need nfile extra intervals so allocate tint0 etc large enough
c to allow for this.
	nd=nintt+nfile
	ALLOCATE(tint0(nd),iampl0(nd),iprops0(nd),ampl0(nd))
	ALLOCATE(tint(nd),ampl(nd),iprops(nd))
	ALLOCATE(index(nd))
	ALLOCATE(ifreq1(imin:imax),ifreq2(imin:imax),ifreq3(imin:imax))
	ALLOCATE(jfreq1(imin:imax),jfreq2(imin:imax),jfreq3(imin:imax))   !temp
	if(allocated(timsav)) deallocate(timsav)
	if(allocated(ifits)) deallocate(ifits)
	if(allocated(ibaselin)) deallocate(ibaselin)
	ALLOCATE(timsav(nfits),ifits(nfits),ibaselin(nfits))
	if(cjump) then
	   nsweep=0
	   do i=1,nfile
		nsweep=nsweep+nswps(i)	!nsweep=total # of jumps in all files
	   enddo
	else
	   nsweep=2		!must allocate jumps,kjumps anyway
	endif
	ALLOCATE(jumps(nsweep),kjumps0(nsweep),kjumps(nsweep),
     & jumpomit(nsweep))
c Copy values from .ini
	if(first.and.cjump) then
	   do i=1,nomit
		jumpomit(i)=jomit(i)
	   enddo
	   DEALLOCATE(jomit)
	endif
c
c Read data into these arrays
	call DISKIN2(idatyp,tint0,iampl0,ampl0,iprops0,nintt,nd,
     & nfile,kfile,pfile,calfacs2,calfac2,nval,irecs,nopoint,
     & ifreq1,ifreq2,ifreq3,imin,imax,jfreq1,jfreq2,jfreq3,
     & cjump,nsweep,nswps,jumps,kjumps0,jumpomit,nomit,iscan,
     & iexstrt0,iexend0,nfits,timsav,ifits,ibaselin)
	DEALLOCATE(jfreq1,jfreq2,jfreq3)	!no longer needed
	stabcut=.false.
c Check for zero length intervals
	n=0
	n1=0
	do i=1,nintt
	   if(tint0(i).lt.0.0001) then
		n=n+1
		if(BTEST(iprops0(i),3))	then   !tint(i) was unusable (bit 3='8' set)
		   n1=n1+1
		endif
	   endif
	enddo
	if(n.gt.0) then
	   print 25,n,n1
25	   format(/,
     &' Data contain ',i4,' intervals shorter than 0.1 microsec:',/,
     &'  of which ',i4,' are set as unusable',
     &' list them [N] ? ')
	   read 101,ans
	   if(UC(ans).eq.'Y') then
	      do i=1,nintt
		   if(tint0(i).lt.0.0001) then
			print 24,i,tint0(i),ampl0(i),iprops0(i)
			if(discprt) write(8,24) i,tint0(i),ampl0(i),iprops0(i)
24			format(
     &' Interval # ',i5,' length, amp, props = ',2g13.6,i3)
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
	 if(nfile(===).ne.nfileb(===)) samex=.false.
	 do 38 i=1,nfile
38	  if(kfile(i,===).ne.kfileb(i,===)) samex=.false.
c NB next line sometimes gives false incorrectly! (floating point comparisons!)
c	 if(treso.ne.tresob) samex=.false.  !resolution altered
	 if(abs(treso-tresob).gt.1.e-6) samex=.false.  !resolution altered
	 if(abs(tresg-tresgb).gt.1.e-6) samex=.false.  !resolution altered
	 if(.not.samex) then
          print 39
c          if(pon()) write(7,39)
c          if(discprt) write(8,39)
39	    format(' The bad sections on disc are for',
     &	' different experiments and/or resolution')
	 else if(first.and.samex) then
	  call BELL(1)
        print 37,nbad1(===),kfileb(1,*),kfileb(nfileb,*),
     &	1000.*treso,1000.*tresg
37	  format(' NOTE: ',i3,
     &' bad sections have already been defined by stability plot for',/,
     & ' expts ',i5,' to ',i5,' (resolution ',2f8.2,' microsec)',/,
     & ' Remove these bad sections before analysis [N] ? ')
	  read 101,ans
	  if(UC(ans).eq.'Y') then
	    nbad(===)=nbad1(===)
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
	    pause
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
	    pause
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
c====TEMP
c	print 780
c780	format(/,
c     & ' (1) Old resolution method',/,
c     & ' (2) New resolution method (strict HJC definition)',/,
cc     & ' (3) Both methods',/,
c     & '  Option number [2] = ')
	iopt=2
c	call INPUTi(iopt)
	if(iopt.eq.1.or.iopt.eq.3) then
	   call RESINT50(tint0,tint,ampl0,ampl,iprops0,iprops,nint,
     &     nintt,imode,ffilt,fc,rms,trise,index,
     &     cjump,nsweep,kjumps0,kjumps)
c Resint5 is version in which kjumps0 and kjumps are integer*4
	endif
	if(iopt.eq.2.or.iopt.eq.3) then
	   call RESINT6(tint0,tint,ampl0,ampl,iprops0,iprops,nint,
     &     nintt,imode,ffilt,fc,rms,trise,index,
     &     cjump,nsweep,kjumps0,kjumps)
	endif
c Choice of distn NOW moved from end of RESINT to main prog, below
c (previously idest=distribution type on exit from RESINT)
c
c check on resint
	do i=1,nint
	   if(ampl(i).eq.0) then
		if(tint(i).lt.tresg.and.tint(i).gt.-0.9) then
		   print 701,i,index(i),tint(i),ampl(i),iprops(i)
		   if(discprt) write(8,701)
     &		i,index(i),tint(i),ampl(i),iprops(i)
701		   format(' ERROR:',2i9,2g13.6,i5)
		endif
	   else
		if(tint(i).lt.treso.and.tint(i).gt.-0.9) then
		   print 701,i,index(i),tint(i),ampl(i),iprops(i)
		   if(discprt) write(8,701)
     &		i,index(i),tint(i),ampl(i),iprops(i)
		endif
	   endif
	enddo
c
c Now convert index for start and end of each pooled expt from index
c in raw data (defined in diskin2) to index after resolution imposed
c Want to find i, for index(i)=i0. -do it the crude way!
c integer iexstrt0(50),iexend0(50,iexstrt(50),iexend(50)
c NB index() refers to first interval of a concat group, so may not
c have index(j) EQUAL to iexend(i)
	j1=1
	if(nfile.gt.1) then
	   do i=1,nfile
		iexstrt(i)=0
		iexend(i)=0
		do j=j1,nint
		   if(index(j).ge.iexstrt0(i).and.iexstrt(i).eq.0) then
			iexstrt(i)=j
		   endif
		   if(index(j).le.iexend0(i)) then
			iexend(i)=j
			j1=j	!start here for next file
		   else
			goto 9	!next file
		   endif
		enddo
9		continue
	   enddo
	endif
c
c If bad sections to be removed (and resolution has not been changed)
c then do it (once!) here, now resolution has been imposed
	if(crembad) then
	   call REMBAD(tint,ampl,iprops,nint,nintt,nbad,
     &	isbad,iebad,index,pon())
c==	   call REMBAD(tint,iampl,nint,nbad,isbad,iebad,pon())
	   crembad=.false.	!so not done again
	endif
c
407	continue
	if(shist) idest=idsav2
	if(shist) goto 411
	iopt=idsav1
	if(iopt.lt.1) iopt=1
	call BELL(1)
	if(.not.cjump) then
	   print 100,iopt
c100	   format(
c     & ' (1) Amplitudes',/,
c     & ' (2) Shut times (and times when NOT in specified amp range)',/,
c     & ' (3) Open times and open periods',/,
c     & '       Also open-shut, open-open conditional pdfs and means',/,
c     & '         (and times when IN specified amp range)',/,
c     & ' (4) Bursts',/,
c     & ' (5) Burst means (lists various burst chacteristics)',/,
c     & ' (6) Subconductance sequence listing',/,
c     & ' (7) Stability plots for open periods,shut times or P(open)',/,
c     & ' (8) Stability plots for amplitudes',/,
c     & ' Option number [',i2,'] = ')
100	   format(
     & '     MENU OF DISTRIBUTION TYPES',/,
     & ' (1) Stability plots for amplitudes',/,
     & ' (2) Stability plots for open periods,shut times or P(open)',/,
     & ' (3) Amplitudes',/,
     & ' (4) Shut times (and times when NOT in specified amp range)',/,
     & ' (5) Open times and open periods',/,
     & '       Also open-shut, open-open conditional pdfs and means',/,
     & '         (and times when IN specified amp range)',/,
     & ' (6) Bursts',/,
     & ' (7) Burst means (***now under ''bursts'')',/,
     & ' (8) Subconductance sequence listing',/,
     & ' (9) Bivariate distributions and dependency plots',/,
     & ' Option number [',i2,'] = ')
	   call INPUTi(iopt)
	   if(iopt.lt.1.or.iopt.gt.9) goto 407
	   idsav1=iopt		!save dist type
	   if(iopt.eq.7) iopt=6		!burst means now under (6)
	   if(iopt.eq.1) then
		iopt=8
	   else if(iopt.eq.2) then
		iopt=7
	   else if(iopt.ge.3.and.iopt.le.8) then
		iopt=iopt-2
	   endif
	else
	   print 102,iopt
102	   format(
     & '     MENU OF DISTRIBUTION TYPES FOR JUMPS',/,
     & ' (1) Amplitudes (all jumps)',/,
     & ' (2) Shut times after jump',/,
     & ' (3) Open periods (length or number) after jump',/,
     & '     (and means for open|adjacent shut and open|open)',/,
     & ' (4) Activation length, P(open)/act, and open time/jump',/,
     & ' (5) Bursts within jumps',/,
     & ' Option number [',i2,'] = ')
	   call INPUTi(iopt)
	   if(iopt.lt.1.or.iopt.gt.5) goto 407
	   idsav1=iopt		!save dist type
	endif
	idest=iopt
	idsav2=iopt		!save dist type (new numbering)

411	if(.not.shist) then
	 if((idest.eq.4.and.(.not.cjump)).or.(idest.eq.5.and.cjump)) then
	   call DCASK('Calculate Tcrit to define bursts','N',ans)
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
c Also use stabplot to show mean of nth vs n (because it already has vplot5)
206	if(idest.eq.7.or.idest.eq.8.or.dispmean) then
	   call STABPLOT(tint,ampl,iprops,nint,nintt,nbad,nbad1,isbad,
     &    iebad,namp,ampval,acal,trise,tcfac,iscan,idiskq,
     & 	index,iopt,mono,stabcut,nfile,iexstrt,iexend,
     & 	dispmean,ameanval,sdval,ndimm,nthmax,openav,shutav,idtype)
	   if(dispmean) then
		dispmean=.false.
		DEALLOCATE(ameanval,sdval)
		goto 256	!where next?
	   endif
	   if(nbad1(===).lt.0) then		!new bad bits were defined
		nbad1(===)=-nbad1(===)		!restore sign
		nfileb(===)=nfile			!record expt for which defined in .INI
		do 31 i=1,nfileb(===)
31		 kfileb(i,===)=kfile(i,===)
		tresob=treso
		tresgb=tresg
	   endif
	   goto 256	!where next
	endif
C
C NOW GET PARAMETERS FOR HISTOGRAMS.
	lastfit=fitted.and.sexp.and.shist		!redisplay with same fit?
	fitted=.false.
	PREFIT=.FALSE.
c
c Jump distributions for shut, open and bursts done in CDISTJ
	if(cjump.and.idest.ge.2.and.idest.le.5) then
	   ndimm=200
	   if(allocated(ameanval)) then
		DEALLOCATE(ameanval,sdval)	!not used
	   endif
	   ALLOCATE(ameanval(0:ndimm),sdval(0:ndimm))	!for 'mean of nth' display
	   call CDISTJ(tint,ampl,iprops,nint,nintt,ndimy,
     & 	yval,nyval,ohist,bhist,IDtype,ghist,ahist,pahist,obhist,
     &	treso,tresg,tres,idest,idsav,shutav,openav,sdopen,tcrit,
     & 	nsweep,jumps,kjumps,jumpomit,nomit,nth,yvalt,nfitt,nnull,
     &      ylogap,yhigap,nvalgap,yloop,yhiop,nvalop,toffset,thist,
     & 	ameanval,sdval,ndimm,nthmax,idiskq)
c	   if(idtype.eq.104.or.idtype.eq.113.or.idtype.eq.115.or.
c     &	  idtype.eq.116) then	!means only, no graph -NOW plotted
c  plot of condtional open time means not yet fixed -must be done
c via ascii files (averaged if req) in cvfit for now
	   if(idtype.eq.104.or.idtype.eq.113) then	!means only, no graph -NOW plotted
		ans='Y'
		call DCASK('Plot these means',ans,ans)
		if(ans.eq.'Y') then
		   dispmean=.true.
		   goto 206		!display (in stabplot; already has vplot5)
		else
		   goto 256	!options 5-9 only
		endif
	   else
		DEALLOCATE(ameanval,sdval)	!not used
	      goto 210
	   endif
c	endif
c	if(idest.eq.4) GOTO 402			!BURST DISTNs
c	if(idest.eq.5) GOTO 403			!CMEAN
c	if(idest.eq.6) goto 404			!Sequences
c Use CDIST0 for idest=1,2,3 -new version =cdist1
	else if(idest.le.3) then	!amps, shut, open
401	  call CDIST1(tint,ampl,iprops,nint,nintt,ndimy,
     &   yval,nyval,ohist,bhist,IDtype,ghist,ahist,pahist,obhist,
     &   trise,iscan,treso,tresg,tres,idest,idsav,tcfac,nopoint,
     &   revamp,wfreq,ylogap,yhigap,nvalgap,yloop,yhiop,nvalop,
     &   nocor,isolate,idiskq,rms,freqs,noutlo,nouthi,
     &   consamdef,adcfil,nfits,ifits,timsav,ibaselin,dfinter,
     &   sfac2,npfilt,qfile,nsam,srate,irecfst,calfac,ioffset,nfile)
c I think reference to "ohist.and.idtype.eq.4" is reference to obsolete
c route for calculation of correlations? Now id=4 refers to open periods
c conditional on adjacent gap
c	  if(OHIST.and.idtype.eq.4) goto 303	!open-shut correlation
	  if(ahist.and.idtype.eq.8) goto 204	!don't call setbin for Patlak
	  if(pahist) goto 204	!don't call setbin for point amps
	  goto 210
c
	else if(idest.eq.4) then	!bursts
c pophist true for fitting gaussians to Popen/bst
402	  call CDIST2(tint,ampl,iprops,nint,nintt,ndimy,avamp,acrit,
     &   Yval,Nyval,iscan,tcrit,tc,nbst,obbar,ohist,bhist,IDtype,
     &   ghist,ahist,pahist,obhist,pophist,iexc,ISG,
     &   treso,tresg,tres,trise,revamp,idsav,ibamp,ibtype,ipop,ibmin,
     &   bmin,nfile,iexstrt,iexend,onetcrit,tcvals,index,
     &   tint0,consamdef,adcfil,nfits,ifits,timsav,ibaselin,dfinter,
     &   sfac2,npfilt,qfile,nsam,srate,irecfst,calfac,ioffset)
	  if(idtype.eq.11) goto 303	!correlation
	  if(idtype.eq.15) then		!burst means
	  	idest=-1	!options 5-9 only
		goto 257	!where next
	  endif
	  goto 210
c
c	else if(idest.eq.5) then	!this is now in cdist2
c403	   call CMEAN1(tint,ampl,iprops,nint,nintt,avamp,acrit,
c     &    tcrit,TC,nbst,ISG)
c	   goto 256	!where next?
c
	else if(idest.eq.6) then
404	   call SEQLST(tint,ampl,iprops,nint,nintt,avamp,acrit,
     &    trise,iscan,ylo,yhi,ymid,ksav,treso,tresg,tcseq,index,
     &    kfile,nfile,name,idiscn,nvalsav,idiskq,
     &    iexstrt,iexend,onetcrit,tcvals,tcrit,ibamp,
     &    tint0,consamdef,adcfil,nfits,ifits,timsav,ibaselin,dfinter,
     &    sfac2,npfilt,opendown,oneshut,
     &    nsam,srate,irecfst,calfac,ioffset)
	   goto 256	!where next?
	else if(idest.eq.9) then
	   call CDIST3D(tint,ampl,iprops,nint,nintt,ndimy,
     &	 iscan,treso,tresg,tres,idsav,idiskq)
	   goto 256	!where next?
	endif
210	continue
11	print 10,nyval
10	format(1x,i6,' values for the distribution:',/,
     & ' (1) Use all',/,
     & ' (2) use first half only',/,
     & ' (3) use last half only',/,
     & ' (4) specify which ones to use',/,
     & ' (5) skip distribution',/,
     & ' Option number [1] = ')
	iopt=1
	call INPUTi(iopt)
	if(iopt.lt.1.or.iopt.gt.5) goto 11
	if(iopt.eq.5) goto 257		!where next
	nysav=nyval		!for print
	if(iopt.eq.2) then
	   n1=1
	   nyval=nyval/2
	   n2=nyval
	else if(iopt.eq.3) then
	   nyval=nyval/2
	   n1=nyval+1
	   n2=2*nyval
	   do i=1,nyval
		yval(i)=yval(i+nyval)
	   enddo
	else if(iopt.eq.4) then
	   n1=1
	   n2=nyval
	   print 14,n1,n2
14	   format(' Use intervals n1 to n2: n1, n2 [',i5,',',i5,'] = ')
	   call INPUT2i(n1,n2)
	   if(n2.gt.nyval) n2=nyval
	   if(n1.lt.1) n1=1
	   n=n2-n1+1
	   do i=1,n
		yval(i)=yval(i+n1-1)
	   enddo
	   nyval=n
	endif
	if(iopt.ge.2.and.iopt.le.4) then
	   print 15,nysav,nyval,n1,n2
	   if(pon()) write(7,15) nysav,nyval,n1,n2
	   if(discprt) write(8,15) nysav,nyval,n1,n2
15	   format(/,' NOT ALL VALUES USED FOR DISTRIBUTION',/,
     &     '  Originally ',i5,' values of which ',i5,' used: numbers ',
     &     i5,' to',i5,/)
	endif
c Set bin widths (previously this was last part of CDIST)
c pophist(in common) true for fitting gaussians to Popen/bst
	call SETBIN1(YVAL,FREQ,XAXIS,nyval,thist,ohist,bhist,
     & ghist,obhist,logt,tres,obmax,idtype,xsav,ybar,nbin,nbw,
     & xwbase,nbdec,fmin,fmax,flo,fhi,ymin,ymax,ndimy,ahist,wfreq,
     & cjump,yvalt,nfitt,ihtype,idest)
	if(nbin.eq.0) then
	   if(idest.eq.-1.and.(.not.nocor)) goto 249		!do correlations?
	   goto 256	!skip histo; where next?
	endif
C
C NOW DISPLAY HISTOGRAM OF ALL DATA IN YVAL
c	IF(IDTYPE.EQ.11) GOTO 303	!now before setbin
c Define labels etc for graph
204	continue
c
c All now done inside SCVDU
c Plot graphs
c NB call with yval1(:,:) for the VHIST data
	call SCVDU(xaxis,xmin,xmax,freq,nbin,fmin,fmax,flo,fhi,
     & fitted,prefit,ahist,pahist,obhist,thist,tres,xtic,ftic,idiskq,
     & idest,ohist,ghist,bhist,pophist,jth,iydat,revamp,
     & logt,first,titlex,titley,xwbase,xw,ilabel,ncalc,xcalc,fcalc,
     & obfreq,frqcal,theta,ncomp,ntx,nty,itx,ity,idtype,
     & amean,area,sd,antrue,isg,gamdst,isdcon,lastfit,
     & nfitdone,thetsav,ksav,idsav,ncompsav,nfitsav,ylosav,yhisav,
     & isgsav,fitsav,mono,ndimd,ndimc,
     & xval,yval1,xcal,ycal,ndat,icurvd,ijoin,ncal,icurvc,iline,
     & imin,imax,ifreq1,ifreq2,ifreq3,ifmin,ifmax,ybar,
     & excreg,yexclow,yexchigh,anfit,nsweep,nnull,nfitt,
     & cjump,nth,ipop,ibmin,bmin,noutlo,nouthi)	!freqs() is in common
c===are ifmin,ifmax needed as args -prob not!
	if(allocated(ameanval)) then
	   DEALLOCATE(ameanval,sdval)	!no longer needed
	endif
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
	print 231
	if(discprt) write(8,231)
231	format(
     & ' WARNING: check stability plots! Instability in data can',/,
     & '  cause false positive correlation (independent of lag)')
c
	call CORCAL(YVAL,NYVAL,OHIST,GHIST,BHIST,OBHIST,
     & 	IDTYPE,idiskq,IDEST,ndimy)
c==	if(idest.eq.31) goto 204	!display transformed var -REMOVED!
c
c I think reference to "ohist.and.idtype.eq.4" is reference to obsolete
c route for calculation of correlations? Now id=4 refers to open periods
c conditional on adjacent gap
c	if(ohist.and.idtype.eq.4.or.
	if(cjump.and.(idtype.eq.104.or.idtype.eq.113)) then
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
c Prepare for fit and do it
	call MLFITS(theta,ybar,ahist,thist,obhist,pophist,fitted,nfix,
     & k,kfit,idtype,jfix,errfac,ndisp,nbin,tres,ymin,ymax,
     & mono,elmax,nev,errors,pahist,revamp,ihtype,xmax,titlep,
     & cjump,tzero,tsamp)
	kmax=k
c
c Save results of last 5 fits in case they are needed for superimposition
	if(nfitdone.eq.5) then	!arrays full, so scroll up
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
	if(nfitdone.lt.5) nfitdone=nfitdone+1		!number of fits saved
	j=nfitdone
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
     & ERRORS,antrue,gamdst,isg,idtype,ghist,titlep,
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
     &	 access='DIRECT',form='UNFORMATTED',recl=10240)
c      OPEN(unit=19,file='EKDIST.INI',status='UNKNOWN',
c     &	access='TRANSPARENT')
c      OPEN(unit=19,file='EKDIST.INI',status='UNKNOWN',
c     & access='DIRECT',form='UNFORMATTED',recl=3072)
	write(19,rec=1) ndev,nfile,kfile,pfile,iydat,
     &	nbad1,isbad,iebad,nfileb,kfileb,tresob,tresgb,
     &	idiskq,treso,tresg,
     & 	ylogap,yhigap,nvalgap,yloop,yhiop,nvalop,
     &	nomit,(jumpomit(i),i=1,nomit),
     &	onetcrit,tcvals,tcrit,qfile,
     &	nvalsav,(ylo(i),i=1,10),(yhi(i),i=1,10),
     &	(tcseq(i),i=1,10),opendown,oneshut,adcfil,
     &	nset
	CLOSE(unit=19)
c
	dprt=discprt
	call ENDPRINT
	CLOSE(unit=7)
	CLOSE(unit=8)
c Set default file name for ekdist.prt use same name as adcfil except suffix=
c .PRT or .PRn
	if(dprt) then
	   ans='Y'
	   call DCASK(
     &   'Make copy of disc printout file with default name',ans,ans)
	   if(ans.eq.'Y') then
		call PARSNAME(adcfil,path,ndev,pname,suffix,nopath,33)
		csfound=.true.
		do i=1,6
		   ival=ICHAR(pname(i:i))
		   if(.not.DIGCHAR(ival)) then
			csfound=.false.
			goto 4
		   endif
		enddo
4		continue
		suffix='prt'
		if(csfound) then
		   pname(7:7)='E'	!replace C with E
		   prtfil=charnb(path)//charnb(pname)//'.'//suffix
		else
		   if(npatch.ge.0.and.npatch.le.9) then
			call INTCONV(npatch,cnum)
			prtfil=defname//'E'//cnum(1:1)//'.'//suffix(1:3)
		   else
			prtfil=defname//'EK'//'.'//suffix(1:3)
		   endif
		endif
		call TITENT0(
     &	'Name for copy of EKDIST.PRT:',prtfil,40,.false.)
		call COPY(filnam,prtfil)
	      OPEN(unit=8,file=prtfil,status='UNKNOWN',
     &     ACCESS='APPEND',FORM='FORMATTED',CARRIAGE CONTROL='FORTRAN')
		print 91,prtfil
		write(8,91) prtfil
91		format(' Print file, EKDIST.PRT, renamed as ',a40)
		CLOSE(unit=8)
	   endif
	endif
	END

