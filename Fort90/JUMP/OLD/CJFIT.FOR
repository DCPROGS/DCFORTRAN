 	program CJFIT

c To take conc jumps from the data file n:\CJUMP.DAT, and
c (1) Do further averaging
c (2) subtraction of curves
c (3) Fit the results
c (4) Write those to be kept to the permanent jump files -NOW REMOVED
c
c NB Several subroutines are in a single file called CJFSUBS.FOR
c
c Lahey V5 version of CJFIT1 08/24/94 02:07pm.  This has allocatable
c arrays, and should be able to read data from all CJUMP.DAT files, with
c any size for data arrays.
c  (1) Uses VPLOT5, which is version for use with allocated arrays, so both
c array dimensions for xval, yval can be passed as parameters.
c
c (2) For iver=-1004 (CJUMP4) jmask() is integer*1, not integer*2 as in
c earlier versions of CJUMP.
c
c (3) No longer need ngr, jc, jv etc: just allocate Yval() to have 1st dimension
c of ndv1=nsamp, so can fit any size sample into Yval(i,1), i=1,2,..,nsamp.
c (nsamp is queued for all versions of CJUMP so this can be done for all).
c
c (4) All first latency stuff removed now (will be done in SCAN/EKDIST)
c
c (5) ALLOCATION OF JMASK(): For versions up to CJUMP2 all points were kept so
c so jmask not used. For CJUMP3, nsamp should be the total length, and
c CALCNS2 calculates the number actually kept (so nsamp gets renamed as
c nsamp0).  For CJUMP4 onwards, nmax (=kmax in CJUMP) is in CJUMP.DAT, and
c this is declared (allocated) size of data arrays (= or > nsamp).  Thus
c for CJUMP3 onwards should be OK to allocate jmask(nsamp0)
c
c (6) Remove RELDAT.DAT read and write altogether -it was little used
c   and directory structure (separate .dir and .dat) id old style.
c
c (7) NB: IKEEP1() is now integer*4, as in new CJUMP, because may have
c more than 32K data points now, NOT integer*2 as in earlier versions.
c
c (8) Now reads ivhclamp (=holding pot set on V-clamp box), and ipatch
c	(=patch type) from CJUMP.DAT so can correct I-V curves etc for potential
c	that may be set on clamp (added that from 1401).
c      ipatch=(1) outside-out', (2) inside-out', (3) cell-attached',/,
c	 (4) whole-cell', (5) simulated data'
c     Values of ivhold, ivolt ivhclamp etc from CJUMP are all pipette
c	 potentials but for display fix them so that actual membrane pot is
c	 shown with normal sign convention.
c	Actual holding potential = ivhold+ivhclamp
c	For ipatch=1,4 voltages this has correct sign; for ipatch=2,3 reverse sign
c
c Modif 01/30/97 05:09pm for new CJUMP5 format for the CJUMP.DAT files
c The old integer*2 version of istrec() is renamed as istrec2(), and
c the first record # for each sweep is now kept as integer*4 istrec()
c whether it points to a 512 byte record (as in all versions up to
c cjump4) or too the byte #, as in cjump5 onwards/
c
c Modif 10/25/95 12:41pm for averaging jumps after removing latency
c NB potential confusion between nsweep from CJUMP.DAT file which is
c number of sweeps set for a multi-sweep run in CJUMP, and nsweep
c from SCANDAT.DAT, which is the number of jumps that were analysed
c in SCAN (the first jump # of these, numbered as in CJUMP.DAT, is recorded
c only in jumps(1)).
c
c Modif 07/15/91 04:29pm by making ndimd=15 for all VPLOT data arrays
c  (this is needed only in CJVDU but must make dimensions agree in READAT,
c  READREL,REAVER,CJVDU,CJSIMP also)
c
c 06/28/91 12:00pm Modified for nsamp>2048 (see def of jc,jv below) (CJFIT1)
c 06/07/91 08:02am Modified to fit CJUMP3 data. The number of recorded points
c (nsamp1 in CJUMP3) must replace nsamp here.
c======= NB RELWRT and READAT not yet altered to take CJUMP3 data
c 05/23/91 12:06pm The array ISTREC on disc has 250 elements, but made 500
c here in order to hold files recovered from partially overwritten disc (may
c be more than 250 altogether): also changed in REAVER
c 03/17/91 11:57am Modified:
c  ILENc(),igapc(),ilenv(),igapv() all now kept as integer*4 microseconds
c  in the program, but for disc read/write kept as (scaled) integer*2
c  for comapibility with earlier versions
c Scaling of ilenc etc for int*2 versions on disc
c  (1) If iDd=1000 ( 1kHz  DAC rate) then keep in msec (up to 32.7 sec)
c  (2) If iDd=10000 ( 100 Hz  DAC rate) then keep in 10ms units (up to 327 sec)
c  (3) If iDd=100 ( 10 kHz  DAC rate) then keep in 0.1ms units (up to 3.27 sec)
c i.e. keep ilenc,igapc etc in number of DAC tics
c and length in microsec=iDd*int4(ilenc(i))
c
c NB problem here because may want to plot 2 records, both of which are
c controls (or both 'drug'), but 2nd AVCUR would overwrite first AVCUR (in
c former case; ditto for AVCURCON in latter). Thus better put data straight
c into Xval,Yval when read in main prog. If two sets of currents read (set #nj1
c and #nj2) then put them in Yval(i,1),Yval(i,2)
c (a) If one data set only read (#nj1, nj2=0) then
c	Yval(i,1)=AVCUR (if 'drug') or AVCURCON (if control) for #nj1
c			(or float(iADC) for single sweeps)
c and, if V recorded,
c	Yval(i,2)=AVOLT (if 'drug') or AVOLTCON (if control) for #nj1
c			(or float(iADC1) for single sweeps)
c (b) If two data sets read (#nj1, #nj2) then
c	Yval(i,1)=AVCUR (if 'drug') or AVCURCON (if control) for #nj1
c			(or float(iADC) for single sweeps)
c	Yval(i,2)=AVCUR (if 'drug') or AVCURCON (if control) for #nj2
c			(or float(iADC) for single sweeps)
c and, if V recorded,
c	Yval(i,3)=AVOLT (if 'drug') or AVOLTCON (if control) for #nj1
c			(or float(iADC1) for single sweeps)
c	Yval(i,4)=AVOLT (if 'drug') or AVOLTCON (if control) for #nj2
c			(or float(iADC1) for single sweeps)
c
c IF NSAMP > 2048 then can still plot with VPLOTR if more than 1 array
c used per graph. Need ngr=1+(nsamp-1)/2048 arrays of 2048k each. Adopt
c convention that jc, jv are indices for the FIRST array used (containing
c the 1st 2048 points).
c (a) If one data set only read (#nj1, nj2=0) then jc=1,jv=1+ngr so need
c	2*ngr arrays altogether (OK up to ngr=10, nsamp=20480 if voltage
c	not recorded, or ngr=5, nsamp=10240 if voltage recorded)
c (b) If two data sets read (#nj1, #nj2) then for #nj1 jc=1, and
c	for #nj2 jc=1+ngr: 2*ngr arrays altogether (OK up to ngr=5, nsamp=10240
c	if voltage not recorded. If voltage recorded then for #nj1 jv=2*ngr+1
c	and for #nj2 jv=3*ngr+1; need 4*ngr arrays altogether (OK up to ngr=2,
c	 nsamp=4096)
c
c
c	real*4 avcur(2048),avcurcon(2048)	!average current (drug,control)
c	real*4 avolt(2048),avoltcon(2048)	!average voltage (drug,control)
c	integer*2 iADC(2048),iADC1(2048)
c=	real*4 avcur(20480),avolt(10240)	!needed only for RELWRT
c=====NB avcur, avolt used in main prog only for RELWRT and this now removed
c=	ALLOCATABLE::avcur,avolt
c=	real*4 avcur(:),avolt(:)	!needed only for RELWRT
	ALLOCATABLE::xfit,yfit
	ALLOCATABLE::tintlat,jumps,jumpomit,jomit
	integer*2 jumps(:)
	integer*2 jumpomit(:),jomit(:)
	real*4 xfit(:),yfit(:)		!for fitting
	real*4 tintlat(:)
	ALLOCATABLE::jmask
	integer*1 jmask(:)
	integer*4 ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
	integer*4 ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	integer*2 ilen2c(10),igap2c(10)	!integer*2 versions for disc
	integer*2 ilen2v(10),igap2v(10)
	integer*2 ivolt1(10),ivolt2(10),ivhold  !pots for each V jump (integer mV)
	integer*2 ivhclamp,ipatch,ivhsav,ivhcsav
	integer*2 istrec2(500)		!orig=500 bytes (kept as int*2 to fit in 1 rec)
	integer*4 istrec(1000)		!as in CJUMP5
	integer isuper(20)		!sweep #s to be superimposed
	integer lrnfit(20)		!learn fit sequence
      character cdate*11,ctime*11,ndev*2,datfil*33
      character cdate1*11,ctime1*11,cdate0*11,ctime0*11
	character title1*79,title11*79,title0*79
c	character titlec*76,ctimec*8		!for CONSAM
c	character*33 pfile(20)	 !path names for SCANDAT files
	character*33 scanfil(20) !name/path for scandat.dat (see getscan)
	logical discprt,pon,slock,debug,caplock,present,vjump,sampv
	logical control,control1,vjump1,vramp,first,refit,search,bad
	logical samescal,rescale,super,fixgain,allocated,mono
	logical readini,fixpot,sampatch,average
	logical filter,logx,logy,align,newform,plotqdat
c For plot queue
	integer*2 jstrec(200),lstrec(200)
c for CJUMP3 data
	integer*2 iver1
c=	integer*2 ikeep1(4,2),kstep(5),jmask(20480)
	integer*4 ikeep1(4,2)
	integer*2 kstep(5)
	logical keepall
c declarations for CJVDU
c=	dimension xcal(2048,10),ycal(2048,10)
c=	real*4 YVAL(2048,15),Yval1(20480)		!equivalenced
c=	real*4 XVAL(2048,15)
c	dimension ndat(15),icurvd(15),isym(15),ijoin(15)	!for data
c	real symsiz(15)				!for data
	ALLOCATABLE::YVAL,XVAL
	real*4 YVAL(:,:),XVAL(:,:)
	ALLOCATABLE::YIN,YOUT		!for filtering
	real*4 YIN(:),YOUT(:)
	ALLOCATABLE::ndat,icurvd,isym,ijoin,symsiz
	integer ndat(:),icurvd(:),isym(:),ijoin(:)  !for data
	real*4 symsiz(:)				!for data
	ALLOCATABLE::weight,icurvw
	real weight(:,:)		!needed for read from plot queue
	integer icurvw(:)		!needed for read from plot queue
	integer icrvdsav(15)	!to save orig icurvd if points averaged
	real*4 t1v(10),t2v(10),t1c(10),t2c(10),vstep(10)
	real*4 t1v1(10),t2v1(10),t1c1(10),t2c1(10)	!to avoid duplic in COMMONS
C for CJSIMP
	character*10 TITLEP(20)		!names of params for fit
	real*4 theta(20)
	integer JFIX(20)
	LOGICAL fitted,dcurve
c for data
c	dimension ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd)
c for calc curves
c	dimension ncal(ndimc),icurvc(ndimc),iline(ndimc)
	dimension ncal(10),icurvc(10),iline(10)		!for calc curve
	character*40 titlex,titley
	character*44 title
	logical doframe
c For getscfil (aligned activations from SS record)
	integer kfile(20),kfile1(20)
	character consfil*40			!path for consam.dat file
	character*40 scdatfil(20)	!path names for SCANDAT files (char*40 here)
	character*40 scantfil(20)  !path names for SCANTEMP files
	allocatable::indfst,indend
	integer*4 indfst(:),indend(:)
c=	integer*2 ibline(:)	!value of ibaseline for start of each burst
	allocatable::jndfst,jndend   !temp versions
	integer*4 jndfst(:),jndend(:)
	real*8 dfinter
	logical avact
c For IV mouse routines
	logical mouse_on,mous_set
	common/mousval/mouse_on,nbutton
c For lab private versions!
      logical private
      common/lab/private
c
c==	EQUIVALENCE (yval,yval1)
      character*1 ans,UC,ans1
	common/dp/discprt
	character*40 mtitle*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,jcol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c==	common/dpp/filnam,machine,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	common/vrev/km,theta	!for EFUNC and CJVDU
c COMMON/cjpar/ for values in RELWRT, READAT (which reads back data written
c by RELWRT), and REAVER. NB this common has the integer*4 (microseconds)
c versions of ILENc()...etc
c Avcur and Avolt removed from common (allocatable now) and ivhold moved to
c end to improve alignment -not needed now
c	COMMON/cjpar/nsweep,
c     & naver,navc,control,vjump,sampv,nsamp,itsamp,nAv,dnAv,
c     & nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,calvolt,
c     & irate,iTPREc,nAc,dnAc,ncjump,ilenc,igapc,
c     & t1v1,t2v1,t1c1,t2c1,nDv,iDd,keepall,ivhold
	COMMON/JLOGO/t1c,t2c,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c	!for VPLOT
	common/rblck/treso,tresg,acrit,avamp	!for RESINT
	integer icol(100)
	COMMON/cols/icol,mono		!for VPLOT, CJVDU
	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls

c For interpolation of data and calculation of time to 50% threshold
c in REAVER1
c	common/bisec/xl(11),yl(11),yl2(11),n,klo,khi
c	external FUNC
c
	pon()=slock()
	debug()=caplock()
c
      private=.true.
101   format(a1)
4	format(i8)
      filnam='CJFIT.PRT'
	call WINPRINT()	!print file control
      OPEN(unit=7,file=prtport,iostat=nerr)		!open printer
	mono=jcol.eq.0	!monochrome screen (call AFTER winprint!)
	print 1
	if(pon()) write(7,1)
	if(discprt) write(8,1)
1	format(' CJFIT- Relaxation analysis program',/)
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 2,cdate,ctime(1:8),mtitle
	if(pon()) write(7,2) cdate,ctime(1:8),mtitle
	if(discprt) write(8,2) cdate,ctime(1:8),mtitle
2	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 3
3	format(
     & ' SCROLL LOCK on for printing'/
     & ' CAPS LOCK on for debugging')
c
	call SETMOUSE()         !define values in common\mousval\ (in IVLIB)
c Joke!
c	INQUIRE(file='C:\SB16\VOCUTIL\CJF1.WAV',exist=present,flen=len)
c	if(present) then
c	   call SYSTEM('C:\SB16\VOCUTIL\WPLAY CJF1.WAV /Q')
c	endif
c Initialise
	filter=.false.
	plotqdat=.false.
	idiskq=-1		!until defined
	fitted=.false.
	dcurve=.false.	!for now
	rescale=.false.	!do not rescale display in CJVDU before refitting
c==	store=.false.	!do not store permanently on relaxation floppy
	super=.false.	!do not superimpose plots
	avact=.false.	!true when averaging activations
	fixgain=.false.	!use gain as read from disc
	fixpot=.false.	!use holding pot as read from disc
	search=.false.
	keepall=.true.
	jfit=1
	nj1=0
	njlast=-1
	ilrn=0
	do 361 i=1,20
361	lrnfit(i)=1		!initial fit sequence
c Initialise 'group means' for cjvdu
	average=.false.
	nmean=1
c Defaults for calibration
	ftape=1.0		!some defaults
	errfac=1.0
	gain=10.
	amVpA1=10.		!mV/pA on patch clamp
c Values for POPMENU calls
c	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls
	nxlo=100
	nylo=-1
	nyhi=470
	ictm=15		!text white
c	ibkm=8		!background dark grey
	ibkm=1		!background dark blue
	icfm=2		!frame/title green
	icupm=12		!upper case red
c Default for plot queue # to read
	iplot=1
c  Read .INI
	readini=.false.
	INQUIRE(file='CJFIT.INI',exist=present,flen=len)
c Allocate temp array to read jumpomit() (nsweep not yet known, and must
c  start index at zero in case nomit=0) NB must allocate jumpomit for
c wrting of .ini file (if actually used it will be re-allocated below)
	ALLOCATE(jomit(0:1000))
	ALLOCATE(jumpomit(0:1))
	if(present.and.len.gt.0) then
	   readini=.true.
         OPEN(unit=19,file='CJFIT.INI',status='UNKNOWN',
     &    access='TRANSPARENT')
c         OPEN(unit=19,file='CJFIT.INI',status='UNKNOWN',
c     &   access='DIRECT',form='UNFORMATTED',recl=1024)
	   read(19,rec=1) ndev,datfil,idiskq,nfile1,scanfil,kfile1,
     &	treso,tresg,
     &	consfil,scantfil,scdatfil,nfile,kfile,iread,jfit,
     &	nomit,(jomit(i),i=1,nomit)
	   CLOSE(unit=19)
	endif
c
37	if(private) then
           print 36,iread
36         format(
     &     ' (1) Take data from CJUMP program (CJUMP.DAT)',/,
     &     ' (2) Import data from MathCad (MCDAT.DAT)',/,
     &     ' (3) Take data for fitting from plot queue (PLOTQ.DAT)',/,
     &     ' (4) Average aligned bursts from CONSAM.DAT',/,
     &     ' Option number [',i2,'] = ')
      else
c          if(iread.eq.4) iread=1
           print 360,iread
360        format(
     &     ' (1) Take data from CJUMP program (CJUMP.DAT)',/,
     &     ' (2) Import data from MathCad (MCDAT.DAT)',/,
     &     ' (3) Take data for fitting from plot queue (PLOTQ.DAT)',/,
     &     ' Option number [',i2,'] = ')
      end if
	call INPUTi(iread)
	if(iread.le.0.or.iread.gt.4) iread=1
	if(iread.eq.2) then
	   print 28
28	   format(' Sample rate (integer Hz) = ')
	   call INPUTi(irate)
27	   if(.not.readini) then
	      ndev='A:'		!default = floppy
	      call DISCNUM1(idisc,ndev,-1,1)
	      if(idisc.eq.-1) goto 999	!if ESC hit in discnum
	      datfil=ndev//'\MCDAT.DAT'
	   endif
         call TITENT0('Data file name:',datfil,33,.false.)
	   if(datfil(2:2).eq.':') ndev=datfil(1:1)
	   if(ndev.eq.'A:') pause ' Now mount data floppy:'
         INQUIRE(file=datfil,exist=present,flen=len)
	   if(.not.present.or.len.le.0) then
		call BELL(2)
		goto 27
	   endif
c About 17-18 bytes per number, so nsamp approx LEN/17; say 16 to be on
c safe side. But other cases have only 9 bytes/number (small numbers sep only
c by CR/LF) so better divide by 8 for now (or count cr/lf to get N?)
c	   ndv1=len/16
	   ndv1=len/8
	   ndimd=1
	   ALLOCATE(yval(ndv1,ndimd),xval(ndv1,ndimd)) !where is xval defined?
	   ALLOCATE(ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd))
	   ALLOCATE(symsiz(ndimd))
	   call READMCAD(datfil,len,nsamp,Yval,ndv1,ndimd)
c=	   call READMC(ndev,nsamp,irate,Yval,ierr)
	   nj2=0
	   nvjump=0
	   ncjump=0
	   sampv=.false.
	   vramp=.false.
	   goto 307		!display
	endif
c
c Section to read data from plot queue (iread=3)
	if(iread.eq.3) then
29	   call TITENT0(
     &	'Name and path of plot queue file:',datfil,33,.false.)
	   INQUIRE(file=datfil,exist=present,flen=len)
	   if(.not.present.or.len.eq.0) then
		call BELL(2)
		call DCASK('Queue not found: try again',ans,ans)
		if(ans.eq.'Y') goto 29
		goto37
	   else
		plotqdat=.true.	!e.g. to prevent reset if t1c() etc in cjvdu
            OPEN(unit=11,file=datfil,status='OLD',
     &       access='DIRECT',form='UNFORMATTED',recl=1024)
c     &       access='DIRECT',form='UNFORMATTED',recl=1024,err=302)
c The array jstrec(200) contains in jstrec(nplot) the start record
c for plot #nplot, and in lstrec(nplot) the last record number for it
      	read(11,rec=1) nplot,jstrec,lstrec,iverq
		print 55,nplot,iplot
55		format(
     &	' Queue contains ',i4,' plots: use plot # [',i3,'] = ')
		call INPUTi(iplot)
		print 58,iplot,datfil
	      if(pon()) write(7,58) iplot,datfil
	      if(discprt) write(8,58) iplot,datfil
58		format(
     &	' Data taken from PLOT QUEUE: plot #',i4,' from ',a33)
		irec=int4(jstrec(iplot))	!1st record no for plot # iplot
		nrect=int4(lstrec(iplot))-irec+1		!total number of records
		nrect=nrect-2				!number of records for Xval etc
		read(11,rec=irec) iptype
		if(iptype.lt.11.or.iptype.gt.25) then	!not vplot
		   call BELL(2)
		   print*, 'NOT VPLOT DATA'
		   STOP
		ENDIF
	      if(iptype.eq.14) then
		   read(11,rec=irec) iptype,ndv1,ndimd,ndc1,ndimc,
     &		itit,title
		   kwi=100	!dimensions for weight()
		   kwj=10
		   kmax=20
		   if(ndv1.eq.0) ndv1=1
		   if(ndimd.eq.0) ndimd=1
		   if(ndc1.eq.0) ndc1=1
		   if(ndimc.eq.0) ndimc=1
		else if(iptype.eq.15) then
		   read(11,rec=irec)iptype,ndv1,ndimd,ndc1,ndimc,
     &		kwi,kwj,kmax,itit,title
		else
	         ndv1=2048	!dimensions as for earlier versions
		   ndimd=10
		   ndc1=2048
		   ndimc=10
		   kwi=100	!dimensions for weight()
		   kwj=10
		   kmax=20
		endif
		print 57,title
57		format(' Plot title: ',a44,' O.K. [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		if(ans.eq.'N') goto 37
		if(allocated(xval)) then
		   DEALLOCATE(Xval,Yval)
		endif
		if(allocated(ndat)) then
	         DEALLOCATE(ndat,isym,ijoin,symsiz)
		endif
		if(allocated(icurvd)) then
	         DEALLOCATE(icurvd)
		endif
c		if(allocated(xcal)) then
c		   DEALLOCATE(Xcal,Ycal)
c		endif
c		if(allocated(theta)) then
c		   DEALLOCATE(theta)
c		endif
c		if(allocated(ncal)) then
c	         DEALLOCATE(ncal,iline)
c		endif
		if(ndv1.le.0) ndv1=1		!in case no data curves
		if(ndimd.le.0) ndimd=1		!in case no data curves
		if(ndc1.le.0) ndc1=1		!in case no calc curves
		if(ndimc.le.0) ndimc=1		!in case no calc curves
c No weights needed in cjfit (weight,icurvw set locally in CJVDU)
c  but must have them here for reading from queue!!
		if(allocated(weight)) then
		   DEALLOCATE(weight,icurvw)
		endif
		if(kwi.le.0) kwi=1
		if(kwj.le.0) kwj=1
		ALLOCATE(weight(kwi,kwj),icurvw(ndimd))
		ALLOCATE(Xval(ndv1,ndimd),Yval(ndv1,ndimd))
		if(kmax.lt.1) kmax=1		!must allocate theta whether used or not
		ALLOCATE(ndat(ndimd),isym(ndimd),
     &    		ijoin(ndimd),symsiz(ndimd))
		ALLOCATE(icurvd(ndimd))
c Up to this point, have read PLOTQ as though it was always in 1024 byte
c records, but for RDVPLQ must reopen as transparent for iptype>15
		if(iptype.ge.15.and.iptype.le.25) then
		   CLOSE(unit=11)
               OPEN(unit=11,file=datfil,status='UNKNOWN',
     &		access='TRANSPARENT')
		   irec=1 + int4(jstrec(iplot)-1)*1024  !start rec when transparent
		endif
		call READQ(irec,xval,yval,ndimd,ndimc,ncurvd,
     &	 ndat,isym,ijoin,symsiz,
     &	 xmin,xmax,ymin,ymax,
     & xcross,ycross,xtic,ytic,ntx,nty,itx,ity,ixlo,ixhi,iylo,iyhi,
     & itit,title,csize,ifont,ilog,iscal,doframe,titlex,titley,ilabel,
     & inumx,inumy,sval,theta,ifitype,ncomp,isdev,weight,y0,yinf,iptype,
     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,ivplot,
     & interp,itrace,ntrace,ytsep,ndv1,ndc1,kwi,kwj,icurvw,kmax)
		do i=1,10
		  t1c1(i)=t1c(i)
		  t2c1(i)=t2c(i)
		  t1v1(i)=t1v(i)
		  t2v1(i)=t2v(i)
		enddo
c           Even if queued plot had fitted curve they are not used
c		because data are being read in for fitting -xcal etc are
c		defined only within readq to allow correct reading but
c NB This is meant to read a digitised jump form plot queue.  The queue
c does not contain values of nsamp, irate etc.  Can only take nsamp=ndat(iplot)
c and assume x values are in ms so get irate from this.
c Set 'fitted' etc.  Ask whether to show queued fit (if any)? -not in
c this version.  Also need to define sample rate, nsamp
		xminsav=xmin		!save to restore expanded plot in CJVDU
		xmaxsav=xmax
		yminsav=ymin
		ymaxsav=ymax
		ndimc=1
		ncurvc=0
		fitted=.false.
		nsamp=ndat(iplot)
		jc=1		!must ask if more than one data set in queued data!
		dx=xval(2,jc)-xval(1,jc)
		srate=1.e3/dx
		irate=ifixr(srate)
		print 56,irate
56		format(' Sample rate (Hz) = ',i5,' O.K. [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		if(ans.eq.'N') then
		   CLOSE(unit=11)
		   goto 37
		endif
c filter again?
		ans='N'
		call DCASK('Re-filter the data',ans,ans)
		if(ans.eq.'N') then
	   	   goto 307		!display the data
		else
		   print 531,fcn
531		   format(
     &    ' Specify present filter cut-off, fc (Hz) [',i5,'] = ')
		   call INPUTr(fcn)
		   ffilt=fcn
		   print 53,fcn
c53		   format(
c     &    ' At present filtered at ',f9.1,' Hz -new final fc (Hz) = ')
		   call INPUTr(fcn)
		   if(fcn.lt.ffilt-1.) then
		     fcnew=1./sqrt((1.0/(fcn*fcn)) - (1.0/(ffilt*ffilt)))
		     print 52,ffilt,fcnew,fcn
	           if(pon()) write(7,52) ffilt,fcnew,fcn
	           if(discprt) write(8,52) ffilt,fcnew,fcn
c52		     format(
c     &  ' Data already filtered at ',g13.6,' Hz.',/,
c     &  '  Filtered at fc = ',g13.6,' Hz to give final fc = ',g13.6)
c	      tpref=0.8*float(itPREc)/1000.
		     print 54,tpref
c54		     format(
c     & ' Use mean of first and last x ms to eliminate end effect: [',
c     &   	f8.2,'] x = ')
			call INPUTr(tpref)
			itpref=tpref*float(irate)/1000.	!number of points
			nd1=-53
			nd2=nsamp+54
			ALLOCATE(yin(nd1:nd2),yout(nd1:nd2))
			do i=1,nsamp
			   if(i.le.itpref) y1=y1+yval(i,jc)	!mean of 1st itpref points
			   if(i.gt.nsamp-itpref) y2=y2+yval(i,jc) !mean of last itpref points
			   yin(i)=yval(i,jc) 		!copy data to ytemp
		      enddo
		      y1=y1/float(itpref)
		      y2=y2/float(itpref)
		      do i=nd1,0
			   yin(i)=y1
	 	      enddo
		      do i=nsamp+1,nd2
			   yin(i)=y2
		      enddo
		      srate=float(irate)
  		      call GFILTERR(yin,yout,nd1,nd2,srate,fcnew,nd1,nd2)
		      do i=1,nsamp
			   yval(i,jc)=yout(i)
		      enddo
		      DEALLOCATE(yin,yout)
		   endif
	   	   goto 307		!display the data
		endif		!end of filtering
	   endif
	endif		!end of read from plot queue
c
c Alignment section (iread=4)
	if(iread.eq.4) then
c       Get paths for CONSAM, SCANTEMP, SCANDAT
	   call GETSCFIL(consfil,scantfil,nfile,kfile,scdatfil,
     &	 nlen,srate,readini,idest)
c=	   if(idest.eq.99) goto ===
c       Get details of bursts
	   ndim1=10000	!size  of temp arrays -must be at least nburst
	   ALLOCATE(jndfst(ndim1),jndend(ndim1))
	   call GETBURST(scantfil,scdatfil,nfile,kfile,
     &  tcrit,ndim1,jndfst,jndend,nburst,avsamp,tpre,
     &  nsamp,nsamp1,dfinter)
c Transfer ibstrt,ibend to new arrays of correct size (nburst)
	   ALLOCATE(indfst(nburst),indend(nburst))
	   do i=1,nburst
		indfst(i)=jndfst(i)
		indend(i)=jndend(i)
	   enddo
	   DEALLOCATE(jndfst,jndend)
c Alocate Yval, and other arrays for display
	   ndv1=nsamp
	   ndimd=2		!need 2 to dispaly residuals
	   ALLOCATE(yval(ndv1,ndimd),xval(ndv1,ndimd)) !where is xval defined?
	   ALLOCATE(ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd))
	   ALLOCATE(symsiz(ndimd))
c	  Choose bursts to average
	   call AVER_ACT(nburst,indfst,indend,consfil,nlen,
     & avsamp,tpre,nsamp,nsamp1,dfinter,tcrit,idest,yval,ndimd,ndv1)
c       Display average for fitting
	   avact=.true.	!true when averaging activations
	   goto 307
c===	   goto 9999
c
	endif
c
25	continue
	if(.not.readini) then
	   if(ndisc.gt.1) then
		print 251
251		format(' Specify disk partition for CJUMP data file:')
	   endif
	   call DISCNUM1(idisc,ndev,-1,1)
	   if(idisc.eq.-1) goto 999	!if ESC hit in discnum
	   datfil=ndev//'\CJUMP.DAT'
	endif
	call TITENT0('Give name/path for CJUMP data file:',datfil
     & ,33,.false.)
	if(datfil(2:2).eq.':') ndev=datfil(1:1)
	n=NBLANK(datfil)
c Look for extension on file name. If '.' present assume its given, otherwise
c add extension .DAT
	do i=1,n
	   if(datfil(i:i).eq.'.') goto 255	!extension given
	enddo
	datfil=datfil(1:n)//'.dat'			!add extension
	n=n+4
255	continue
	if(ndev.eq.'A:') pause ' Now mount data floppy:'
	INQUIRE(file=DATFIL,exist=present,flen=len)
	if(.not.present) then
	   call BELL(3)
	   print 24,DATFIL
24	   format(' Cannot find ',a33)
	   goto 25
	else
         OPEN(unit=14,file=DATFIL,status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=512)
	    read(14,rec=1) njd
	    newform=.false.	!old format file (512 byte records)
	    if(njd.lt.1) then
		newform=.true.	!new format 'transparent' file
		CLOSE(unit=14)
c          Re-open in correct format
	      OPEN(unit=14,file=datfil,status='UNKNOWN',
     &	 access='TRANSPARENT')
		read(14,rec=1) jver,njd,nxtrec,ioffset,(ISTREC(i),i=1,njd)
	    else
	      read(14,rec=1) njd,nxtrec,(ISTREC2(i),i=1,250)
c Convert the integer*2 istrec2() into integer *4 for all subsequent use
		do i=1,njd
		   irec=int4(istrec2(i))
		   if(irec.le.-1.and.irec.ge.-32768) then
			irec=irec+65536
		   else if(irec.lt.-32768) then
			print*,' IREC IS TOO BIG!!'
			STOP
		   endif
		   istrec(i)=irec
		enddo
	    endif
	    irec=istrec(njd)
	    read(14,rec=irec) cdate0,ctime0
	    maxrec=len/512
	    if(newform) maxrec=len
	    if(pon()) write(7,411) cdate0
	    if(discprt) write(8,411) cdate0
411       format(' Date of experiment: ',a11)
	endif
c Note that if CJUMP.DAT file has been overwritten in error then data may
c still be there, but irec=1 will me overwritten so njd,istrec will be
c lost: can try to regenerate them here
c
	lastrec=nxtrec-1
	if(maxrec.gt.lastrec) then
	   print 49,datfil,njd,ctime0(1:8),cdate0,istrec(njd),nxtrec-1,
     &	maxrec,njd,lastrec
49	   format(
     & ' The file ',a15,' contains data beyond the indexed sweeps',/,
     &   ' presumably because a longer file was overwritten.',/,
     & ' The last indexed sweep (#',i4,') was at  = ',a8,' on ',a11,/,
     & ' Last indexed sweep is in record numbers = ',i8,' to ',i8,/,
     & ' Total number of records in file = ',i8,/,
     & ' Options:',/,
     & ' (1) Leave the file as it is',/,
     & ' (2) Search for valid sweeps after #',i4,' in overwritten file',
     & /,' (3) Truncate the file at the end of the indexed sweeps',
     & ' (record #',i8,')',/,
     & ' Options number [1] = ')
	   iopt=1
	   call INPUTi(iopt)
	   search=iopt.eq.2
	   if(search) then
		n1=njd		!start with last known record
		n2=500
		goto 50
	   endif
	   if(iopt.eq.3) then
		read(14,rec=lastrec) i	  !position record at end of data
		ENDFILE 14
		maxrec=lastrec
	   endif
	endif
c
67	continue
	print 62,njd,datfil,ctime0(1:8),cdate0
62	format(' ',i4,
     & ' sweeps on disc in ',a33,/,' Last sweep at ',a8,' on ',a11,/,
     & ' [1] No (more) listing',/,
     & ' (2) List brief details only (scroll lock on to print)',/,
     & ' (3) List full details (scroll lock on to print)',/,
     & ' Option number [1] = ')
	iopt=1
	call INPUTi(iopt)
	iopt=iopt-1
	if(iopt.eq.0) goto 42
	iprt=1
	if(iopt.eq.2) iprt=2
	if(.not.search) then
c	   if(pon()) write(7,622) njd,datfil,ctime0(1:8),cdate0
	   if(discprt) write(8,622) njd,datfil,ctime0(1:8),cdate0
622	   format(' ',i4,' sweeps on disc in ',a33,/,
     &	' Last sweep at ',a8,' on ',a11)
	   print 60
60	   format(' List for sweeps n1 to n2:  n1 (,n2) = ')
	   call INPUT2i(n1,n2)
	   if(n1.eq.0) goto 67
	   if(n2.eq.0) n2=n1
	endif
50	continue
c NB need preliminary read here to get version number, so we know
c whether to read ikeep1 as integer*2 (=ikeep12) or integer*4(=ikeep1)
	irec=istrec(n1)
c	irec=int4(istrec(n1))
c	if(irec.le.-1.and.irec.ge.-32768) then
c	   irec=irec+65536
c	else if(irec.lt.-32768) then
c	   print*,' IREC IS TOO BIG!!'
c	   STOP
c	endif
	imode=1	!so READ14 returns iver1 only
	call READ14(irec,cdate,ctime,title1,naver,navc,iav,control,
     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &     ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     &     ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax,
     &     ivhclamp,ipatch,ffilt,sampatch,imode)
	do 64 i=n1,n2
261	   continue
	   irec=istrec(i)
c	   irec=int4(istrec(i))
c	   if(irec.le.-1.and.irec.ge.-32768) then
c		irec=irec+65536
c	   else if(irec.lt.-32768) then
c		print*,' IREC IS TOO BIG!!'
c		STOP
c	   endif
c	   print 700,i,irec,maxrec
c700	   format(' i,irec,maxrec= ',3i8)
	   imode=0		!so READ14 reads header (and corrects it if nec)
	   call READ14(irec,cdate,ctime,title1,naver,navc,iav,control,
     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &     ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     &     ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax,
     &     ivhclamp,ipatch,ffilt,sampatch,imode)
c Now have value for nmax, so arrays can be allocated (iver1=-1004)
c This 'list details' loop needs only jmask (for calcns2)
	   nsamp0=nsamp		!# of points if all points kept
	   if(iver1.le.-1003) then
		if(allocated(jmask)) DEALLOCATE(jmask)
		ALLOCATE(jmask(nsamp0))
		call CALCNS2(ikeep1,nkeep,kstep,nsamp,nsamp1,jmask,
     &		 keepall,nsamp0)
	      nsamp0=nsamp		!orig # of points samples
	      nsamp=nsamp1		!# of points kept on disc
	   endif
	   bad=search.and.cdate(8:10).ne.'199'
	   if(bad) goto 212
	   call ILCONV(ilenc,igapc,ilenv,igapv,
     &	ilen2c,igap2c,ilen2v,igap2v,iDd,1)	!convert ilen2c to ilenc etc
	   call PREC(i,title1,cdate,ctime,naver,navc,iav,vjump,
     &   control,iver1,1)
	   call PPAR(iprt,0,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     &   calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     &   vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     &   iver1,amVpA1,ftape,gain,errfac,nkeep,ikeep1,kstep,keepall,
     &   ivhclamp,ipatch,ffilt)
c===========NB SEARCH NOT YET FIXED FOR NEW FILE FORMAT
212	   if(search) then
		if(i.gt.njd) then
		   if(bad) then
			call BELL(1)
			print 211,i,irec
			if(pon()) write(7,211) i,irec
			if(discprt) write(8,211) i,irec
211			format(
     &	    ' Invalid header for sweep #',i5,' in record # ',i6)
c		   else
c		      print 21
c21			format(' O.K. ? ')
c			input 101,ans
c			bad=UC(ans).ne.'Y'
		   endif
		   if(bad) then
c			if(debug())print 22,i-1,nslast,nrlast,istrec(i-1),
c     &		i,istrec(i),maxrec
c22			format(
c     &' Sweep #',i4,' had ',i6,' points and ',i6,' records (1st rec = ',
c     & i6,')',/,
c     & ' For sweep #',i4,' 1st record = ',i6,' just tried:')
cc     & ' For sweep #',i4,' 1st record = ',i6,' just tried:',/,
cc     & '   now try record # (maximum ',i6,') = ')
cc			input 4,irec
			irec=irec+1		!try next record for valid header
			istrec(i)=int2(irec)
			if(irec.gt.maxrec) then
		         njd=i-1
			   print 641,maxrec,njd
			   if(pon()) write(7,641) maxrec,njd
			   if(discprt) write(8,641) maxrec,njd
		         goto 26
			endif
		      goto 261	!try specified start record for sweep #i if bad
		   endif
		endif
c		Calc length of this sweep and 1st record # for next sweep
		nrec=1 + (nsamp-1)/256	!for iADC(i)  =int*2 so 1 rec=256 values
		if(iav.ne.0) nrec=2*nrec	!for AVCUR(i) =real*4
		if(sampv) nrec=2*nrec         !for voltage data
		nrec=nrec+1				!for 'header'
		irec1=int4(istrec(i))+nrec	!start rec for next sweep
		if(irec1.gt.maxrec.or.i+1.gt.500) then
		   njd=i
		   print 641,maxrec,njd
		   if(pon()) write(7,641) maxrec,njd
		   if(discprt) write(8,641) maxrec,njd
641		   format(
     &	' End of file (',i8,' records): ',i6,' sweeps found')
		   goto 26		!out of loop
		else
		   istrec(i+1)=int2(irec1)	!start rec for next sweep
		endif
c	      nslast=nsamp
c	      nrlast=nrec
	   endif
64	continue		!end of loop for listing specified sweep details
26	continue
	search=.false.		!so not done again
	if(pon()) then
	   write(7,6)
6	   format('1') !form feed (in case scroll lock used to print file summary)
	   call FLUSH(7)
	endif
c
	print 41
41	format(' Use this disc file [Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') then
         CLOSE(unit=14)
	   goto 25		!try another disc
	endif
	goto 67	!list more?
c First record contains number of jumps recorded so far =NJD, next vacant
c record # = NXTREC and an array ISTREC(ijd)=record # for 1st record of
c each jump (that with the header info) (kept as int*2 to save space)
C ### MUST CONVERT ISTREC() TO INTEGER*4 BEFORE USING FOR READ
c Each jump written to 2 or more records. First contains directory-type stuff,
c the others contain data. 256 bytes=128 int*2 values of iADC
c Keep NAVER=number of sweeps averaged. Iav=0 (whatever naver/navc) means that
c iADC follows header; Iav=1 (naver/navc>1) means that AVCUR() follows header;
c then data that follows is twice as long (=AVCUR(i)=real*4)
c From CJDISK:
c If IAV=0 then writes only the current sweep (in iADC, and IADC1 if voltage
c sampled too), regardless of value of NAVER or NAVC
c Otherwise (IAV=1) records the average that has just been
c completed, ie if no v-jump then records AVCUR, if vjump done then
c (1) if CONTROL is false ('drug' -c-jump also done) records AVCUR,AVOLT or
c (2) if CONTROL is true ('con' -no c-jump) records AVCURCON,AVOLTCON
c NB may have CONTROL=true with SAMPV=false; eg if doing only jumps, not
c ramps then voltage is not recorded on ADC1, but will want to do
c jumps with and without agonist (c-jump)
c    When IAV=1 then returns without recording anything if naver (or navc)=1
c since no point in recording average of one trace (already recorded in iADC)
c THUS when reading back, any record with naver/navc=1 is iADC not AVCUR.
c BUT converse not nec true: if naver/navc>1 then data may nevertheless be
c for a single sweep (in iADC/iADC1); thus data record now contains IAV also.
c
c Read all titles?
42	continue
	align=.false.
	super=.false.
	print 40,nj1+1,nj1+1,jfit
40	format(
     & ' (1) Fit single record, or subtracted pair',/,
     & ' (2) Re-average single sweeps from file before fitting',/,
     & ' (3) Fit the next single record (#',i5,')',/,
     & ' (4) Fit the next single record (#',i5,') -same graph scales',/,
     & ' (5) Superimpose up to 20 I(t) vs t sweeps',/,
     & ' (6) Re-average single sweeps after removing first latency',/,
     & '      (after fitting with SCAN)',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(jfit)
	if(jfit.eq.3.or.jfit.eq.4) then
	   nj1=nj1+1
	   nj2=0
	   goto 21
	endif
	if(jfit.eq.5) then		!Superimposed plots
	   if(pon()) write(7,4211)
	   if(discprt) write(8,4211)
4211	   format( ' Superimposed plots:')
	   print 421
421	   format('&Number of sweeps to be superimposed = ')
	   call INPUTi(nsuper)
	   if(fixgain) then
		print 423
423		format('&override the gain read from disc as before [Y] ? ')
		read 101,ans
		if(UC(ans).eq.'N') fixgain=.false.
	   endif
c For iver1=-1004 need a preliminary read of all data records to get the
c largest nmax value so arrays can be allocated accordingly: need to do
c this whatever in order to find whether ANY of the requested plots
c have iver1=-1004.  Can also get sweep numbers in this prelim loop.
	   nj1=0	!initial default
	   id=1
	   nmax=0		!unless iver.le.-1004 so nmax defined
	   nsampmax=0	!max number of points to be plotted
	   do js=1,nsuper
46		nj1=nj1+id
		print 44,js,nj1
44	      format('&',i3,': sweep number [',i4,'] = ')
		call INPUTi(nj1)
		isuper(js)=nj1
		if(js.gt.1) id=isuper(js)-isuper(js-1)	!default increment
		irec=istrec(nj1)			!read data set #nj1
c		irec=int4(istrec(nj1))			!read data set #nj1
c		if(irec.le.-1.and.irec.ge.-32768) then
c		   irec=irec+65536
c		else if(irec.lt.-32768) then
c		   print*,' IREC IS TOO BIG!!'
c		   STOP
c	      endif
		call READ14(irec,cdate,ctime,title1,naver,navc,iav,control,
     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &     ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     &     ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax1,
     &     ivhclamp,ipatch,ffilt,sampatch,0)
	  	nsamp0=nsamp		!# of points if all points kept
		if(iver1.le.-1004) then		!nmax defined
		   if(nmax1.gt.nmax) nmax=nmax1	!the largest nmax
		endif
		if(iver1.le.-1003) then
		   if(allocated(jmask)) DEALLOCATE(jmask)
		   ALLOCATE(jmask(nsamp0))
		   call CALCNS2(ikeep1,nkeep,kstep,nsamp,nsamp1,jmask,
     &		 keepall,nsamp0)
	  	   nsamp0=nsamp		!orig # of points samples
	         nsamp=nsamp1		!# of points kept on disc
		endif
		if(nsamp.gt.nsampmax) nsampmax=nsamp
	      call ILCONV(ilenc,igapc,ilenv,igapv,
     &	ilen2c,igap2c,ilen2v,igap2v,iDd,1)	!convert ilen2c to ilenc etc
		call PREC(nj1,title1,cdate,ctime,naver,navc,iav,vjump,
     & 	control,iver1,2)
		call PPAR(1,1,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     &    calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     &   vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     &   iver1,amVpA1,ftape,gain,errfac,nkeep,ikeep1,kstep,keepall,
     &   ivhclamp,ipatch,ffilt)
		print 45
45		format(' This record O.K. [Y] ? ')
		read 101,ans
		if(UC(ans).eq.'N') goto 46
	   enddo
c Now allocate arrays needed for superimposed plots, viz jmask, yval, xval.
	   if(allocated(jmask)) DEALLOCATE(jmask)
	   ALLOCATE(jmask(nsamp0))
c Can now allocate Xval, Yval to correct size for ANY version of cjump
	   if(allocated(xval)) DEALLOCATE(xval)
	   if(allocated(yval)) DEALLOCATE(yval)
	   if(allocated(ndat)) then
		DEALLOCATE(ndat,icurvd,isym,ijoin,symsiz)
	   endif
	   ndv1=nsampmax
	   ndimd=nsuper
	   ALLOCATE(Xval(ndv1,ndimd),Yval(ndv1,ndimd))
	   ALLOCATE(ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd))
	   ALLOCATE(symsiz(ndimd))
c
c Now loop through superimposed plots to define Xval,Yval
	   jc=1	!data goes into Yval(i,jc), i=1,...,nsamp
	   do 43 js=1,nsuper
		nj1=isuper(js)
		irec=istrec(nj1)			!read data set #nj1
c		irec=int4(istrec(nj1))			!read data set #nj1
c		if(irec.le.-1.and.irec.ge.-32768) then
c		   irec=irec+65536
c		else if(irec.lt.-32768) then
c		   print*,' IREC IS TOO BIG!!'
c		   STOP
c	      endif
		call READ14(irec,cdate,ctime,title1,naver,navc,iav,control,
     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &     ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     &     ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax,
     &     ivhclamp,ipatch,ffilt,sampatch,0)
c NB iver1=-1001 for CJUMP2 data (values as far as swtime recorded) and
c iver1=-1003 for CJUMP3 data, for which all values up to errfac are recorded
c For iver1=-1004 we also read nmax,ivhclamp,ipatch,ffilt (nmax not used!)
		if(iver1.eq.-1003.or.iver1.eq.-1004) then
		   call CALCNS2(ikeep1,nkeep,kstep,nsamp,nsamp1,jmask,
     &		 keepall,nmax)
	  	   nsamp0=nsamp		!orig # of points samples
	         nsamp=nsamp1		!# of points kept on disc
		endif
c
	      call ILCONV(ilenc,igapc,ilenv,igapv,
     &	ilen2c,igap2c,ilen2v,igap2v,iDd,1)	!convert ilen2c to ilenc etc
		call PREC(nj1,title1,cdate,ctime,naver,navc,iav,vjump,
     & 	control,iver1,2)
		call PPAR(1,1,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     &    calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     &   vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     &   iver1,amVpA1,ftape,gain,errfac,nkeep,ikeep1,kstep,keepall,
     &   ivhclamp,ipatch,ffilt)
		if(fixgain) then
		   calfac=calfac1
		   calvolt=calvolt1
		endif
c	Read currents for this one
c 		Call READREL with sampv=false so voltages not read
		call READREL(irec,nsamp,iav,.false.,calfac,calvolt,
     & 	  ipatch,ivhclamp,Yval,jc,jv,ndv1,ndimd,ioffset,newform)
c 	and calc Xval here, while jmask is available for the current sweep
	      dx=1.e3/float(irate)
	      xmin=0.0
	      xmax=float(itsamp)/1000.		!msec
	      xoff1=0.
	      if(keepall) then
	       do 413 i=1,nsamp
	       t=float(i-1)*dx	!1st point at t=0
	       do 414 j=1,nsuper
414	       xval(i,j)=t
413	       continue
	      else
	       i1=0
	       do 151 i=1,nsamp0
	       if(jmask(i).eq.0) goto 151	!skipped point
	       t=float(i-1)*dx			!=msec from 0 to itsamp-1
	       i1=i1+1
	       xval(i1,js)=t
151	       continue
	      endif
c Prepare for next sweep:
		if(js.lt.nsuper) then
		  jc=jc+1	!currents in yval(i,jc), jc=1,2,...,nsuper
		endif
43	   continue		!end of js=1,nsuper loop
c
431	   print 422
422	   format(' Normalise all to same baseline [N] ? ')
	   read 101,ans
	   if(UC(ans).eq.'Y') then
		nsuper=-nsuper
	   endif
	   super=.true.
	   goto 307		!plot them
	endif
c End of superimposed plots
c
c Now jfit=2 -Re-average single sweeps from file before fitting
	if(jfit.eq.2) then
501	   continue
	   call REAVER1(istrec,title1,nj1,nj2,n1,n2,jmean,makav,
     &   iver1,ikeep1,nkeep,kstep,nsamp0,nmax,calfac,idest,
     &   nsweep,naver,navc,control,vjump,sampv,nsamp,itsamp,nAv,dnAv,
     &   nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,calvolt,
     &   irate,iTPREc,nAc,dnAc,ncjump,ilenc,igapc,
     &   nDv,iDd,keepall,ivhold,
     &   ivhclamp,ipatch,ivhcsav,ivhsav,sampatch,fixpot,
     &   filter,itpref,fcnew,fcn,align,njd)
	   if(idest.eq.67) goto 67	!more details
c Can now allocate Xval, Yval to correct size for ANY version of cjump
c Also allocate jmask here (it is param of cjvdu so must be allocated at
c	time when cjvdu called)
	   if(allocated(xval)) DEALLOCATE(xval)
	   if(allocated(yval)) DEALLOCATE(yval)
	   if(allocated(ndat)) then
		DEALLOCATE(ndat,icurvd,isym,ijoin,symsiz)
	   endif
	   ndv1=nsamp0
c	   ndimd=4		!max number of arrays in yval (see reaver2)
	   ndimd=10		!set on the safe side
	   ALLOCATE(Xval(ndv1,ndimd),Yval(ndv1,ndimd))
	   ALLOCATE(ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd))
	   ALLOCATE(symsiz(ndimd))
	   if(allocated(jmask)) DEALLOCATE(jmask)
	   ALLOCATE(jmask(nsamp0))
c Do graphics and averaging:
c	   call REAVER2(istrec,Yval,nj1,nj2,n1,n2,jmean,mono,jmask,
c     &    iver1,ikeep1,nkeep,kstep,control,itsamp,vjump,sampv,keepall,
c     &    nsamp,nsamp0,nmax,ndv1,ndimd,makav,calfac,calvolt,idest,
c     &    ivhclamp,ipatch,ivhcsav,ivhsav,sampatch,fixpot,
c     &    filter,itpref,fcnew)
c       tintlat() and jumps() not used here, but must be allocated
	   if(allocated(tintlat)) then
		DEALLOCATE(tintlat,jumps)
	   endif
	   ndim=1
	   ALLOCATE(tintlat(ndim),jumps(ndim))
	   call REAVER3(istrec,Yval,nj1,nj2,n1,n2,jmean,mono,jmask,
     &    iver1,ikeep1,nkeep,kstep,control,itsamp,vjump,sampv,keepall,
     &    nsamp,nsamp0,nmax,ndv1,ndimd,makav,calfac,calvolt,idest,
     &    ivhclamp,ipatch,ivhcsav,ivhsav,sampatch,fixpot,
     &    filter,itpref,fcnew,ioffset,newform,
     &    align,tintlat,jumps,tzero,tsamp,njd,ndim)
	   if(idest.eq.3)	goto 999	!exit
	   if(idest.eq.4)	goto 42	!no events averaged
	   if(idest.eq.5.or.idest.eq.6)	goto 501	!return to REAVER1
	   ilrn=0
	   goto 307				!plot/fit
	endif
c
c Now jfit=6 -Re-average single sweeps from file after removing first
c latency (as fitted in SCAN and read from SCANDAT,DAT)
	if(jfit.eq.6) then
	   align=.true.
c  Do not need to return whole tint() etc to main prog -just return
c array of 1st latencies (call RESINT within GETSCAN).  Thus can
c keep tint() etc local within GETSCAN, and no need to split it (as in DISKIN,
c on which GETSCAN is based) in order to return to main prog to allocate tint()
c Need to allocate array of 1st latencies (dimension=nsweep, already
c known from READ14) here.
c===(easier to number them as in CJUMP.DAT, or as 1,..,nsweep?)
c Easier to number latencies same as the jumps in cjump.dat -there may
c be more jumps in cjump.dat (=njd) than were fitted in scan, but can't
c have more in the latter, so safe to declare tintlat() as njd in size
c/===
c NB potential confusion between nsweep from CJUMP.DAT file which is
c number of sweeps set for a multi-sweep run in CJUMP, and nsweep
c from SCANDAT.DAT, which is the number of jumps that were analysed
c in SCAN (the first jump # of these, numbered as in CJUMP.DAT, is recorded
c only in jumps(1)).  Thus, rename nsweep from SCANDAT as NJUMPt, and first
c jump, numbered as in CJUMP.DAT, as NJUMP1
	   if(allocated(tintlat)) then
		DEALLOCATE(tintlat,jumps)
	   endif
	   if(allocated(jumpomit)) then
		DEALLOCATE(jumpomit)
	   endif
	   ndim=njd
	   ALLOCATE(tintlat(ndim),jumps(ndim),jumpomit(0:ndim))
	   if(nomit.gt.0) then
	      do i=1,nomit
		   jumpomit(i)=jomit(i)
	      enddo
	   endif
	   DEALLOCATE(jomit)
c
	   call GETSCAN(tintlat,jumps,nsweep,nfile1,scanfil,kfile1,
     &	tzero,tsamp,nomit,jumpomit,readini,njd,ndim)
c/====
c=	   njumpt=nsweep		!see note above
c=	   njump1=jumps(1)	!see note above
c
c/========also do jumpomit here? -or as in ekdist.for
c	   if(allocated(tintlat)) then
c		DEALLOCATE(tintlat,jumps)
c	   endif
c	   ALLOCATE(tintlat(njumpt),jumps(njumpt))
c	   do i=1,nsweep
c		tintlat(i)=tintlat0(i)
c		jumps(i)=jumps0(i)
c	   enddo
c	   DEALLOCATE(tintlat0,jumps0)
c      Read header for CJUMP.DAT
502	   continue
c NB potential confusion between nsweep from CJUMP.DAT file which is
c number of sweeps set for a multi-sweep run in CJUMP, and nsweep
c from SCANDAT.DAT, which is the number of jumps that were analysed
c in SCAN (the first jump # of these, numbered as in CJUMP.DAT, is recorded
c only in jumps(1)).  Thus, rename nsweep from SCANDAT as NJUMPt, and first
c jump, numbered as in CJUMP.DAT, as NJUMP1
	   call REAVER1(istrec,title1,nj1,nj2,n1,n2,jmean,makav,
     &   iver1,ikeep1,nkeep,kstep,nsamp0,nmax,calfac,idest,
     &   nsweep,naver,navc,control,vjump,sampv,nsamp,itsamp,nAv,dnAv,
     &   nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,calvolt,
     &   irate,iTPREc,nAc,dnAc,ncjump,ilenc,igapc,
     &   nDv,iDd,keepall,ivhold,
     &   ivhclamp,ipatch,ivhcsav,ivhsav,sampatch,fixpot,
     &   filter,itpref,fcnew,fcn,align,njd)
c Allocate arrays as for jfit=2
	   if(allocated(xval)) DEALLOCATE(xval)
	   if(allocated(yval)) DEALLOCATE(yval)
	   if(allocated(ndat)) then
		DEALLOCATE(ndat,icurvd,isym,ijoin,symsiz)
	   endif
	   ndv1=nsamp0
c	   ndimd=4		!max number of arrays in yval (see reaver2)
	   ndimd=10		!set on the safe side
	   ALLOCATE(Xval(ndv1,ndimd),Yval(ndv1,ndimd))
	   ALLOCATE(ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd))
	   ALLOCATE(symsiz(ndimd))
	   if(allocated(jmask)) DEALLOCATE(jmask)
	   ALLOCATE(jmask(nsamp0))
c Do graphics and averaging:
c Shift and average
	   call REAVER3(istrec,Yval,nj1,nj2,n1,n2,jmean,mono,jmask,
     &    iver1,ikeep1,nkeep,kstep,control,itsamp,vjump,sampv,keepall,
     &    nsamp,nsamp0,nmax,ndv1,ndimd,makav,calfac,calvolt,idest,
     &    ivhclamp,ipatch,ivhcsav,ivhsav,sampatch,fixpot,
     &    filter,itpref,fcnew,ioffset,newform,
     &    align,tintlat,jumps,tzero,tsamp,njd,ndim)
	   if(idest.eq.3)	goto 999	!exit
	   if(idest.eq.4)	goto 42	!no events averaged
	   if(idest.eq.5.or.idest.eq.6) goto 502	!return to REAVER1
	   ilrn=0
	   goto 307				!plot/fit
	endif		!end of jfit=6
c
c WHICH TRACE(S) TO FIT?
c Arrive here for jfit=1 ('Fit single record, or subtracted pair')
c (and jump to 21, below, for jfit=3,4 ('fit next', so nj1 already known)
c NB For C-jumps alone will probably want to read only one sweep (current trace)
c but for V-jumps/ramps will want to read control (no agonist) sweep also
c
33	continue
	print 34,njd
34	format(' Number of records on disc = ',i5,/,
     &' To fit a single sweep specify its number; to fit difference',/,
     &' between two sweeps (drug and control) specify both numbers',/,
     & ' Read sweeps:  n1 (,n2) [more details] =  ')
	nj1=0
	call INPUT2i(nj1,nj2)
	if(nj1.le.0) goto 67
c
c  Jump to here for jfit=3,4 ('fit next')
21	continue
	if(nj1.ne.njlast+1) then
	   ndelt=0		!initialise learning of xdelt
	   xdelt=2.0	!initially put cursor 2ms before peak in CJVDU
	endif
	njlast=nj1
c Restart the learned fit sequence in lrnfit() each time a new record is read
	ilrn=0
c
	irec=istrec(nj1)
c	irec=int4(istrec(nj1))
c	if(irec.le.-1.and.irec.ge.-32768) then
c	   irec=irec+65536
c	else if(irec.lt.-32768) then
c	   print*,' IREC IS TOO BIG!!'
c	   STOP
c	endif
c Read the specified record  'header'
	call READ14(irec,cdate,ctime,title1,naver,navc,iav,control,
     &   vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &   ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &   ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     &   ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax,
     &   ivhclamp,ipatch,ffilt,sampatch,0)
	if(.not.filter) fcn=ffilt	!define fcn if not already defined
c Allocate jmask()
	nsamp0=nsamp		!# of points if all points kept
	if(iver1.le.-1003) then
	   if(allocated(jmask)) DEALLOCATE(jmask)
	   ALLOCATE(jmask(nsamp0))
	   call CALCNS2(ikeep1,nkeep,kstep,nsamp,nsamp1,jmask,
     &    keepall,nsamp0)
	   nsamp0=nsamp		!orig # of points samples
	   nsamp=nsamp1		!# of points kept on disc
	endif
	call ILCONV(ilenc,igapc,ilenv,igapv,
     &	ilen2c,igap2c,ilen2v,igap2v,iDd,1)	!convert ilen2c to ilenc etc
	call PREC(nj1,title1,cdate,ctime,naver,navc,iav,vjump,
     & control,iver1,1)
	if(nsweep.gt.1.and.isweep.eq.1) then
	   print 30,nsweep
	   if(pon()) write(7,30) nsweep
	   if(discprt) write(8,30) nsweep
30	   format(' First sweep of series of ',i3)
	else if(nsweep.gt.1.and.isweep.eq.nsweep) then
	   print 31,nsweep
	   if(pon()) write(7,31) nsweep
	   if(discprt) write(8,31) nsweep
31	   format(' Last sweep of series of ',i3)
	endif
	call PPAR(1,0,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     & calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     & vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     &   iver1,amVpA1,ftape,gain,errfac,nkeep,ikeep1,kstep,keepall,
     &   ivhclamp,ipatch,ffilt)
	if(nj2.gt.0) then
	   irec=istrec(nj2)
c	   irec=int4(istrec(nj2))
c	   if(irec.le.-1.and.irec.ge.-32768) then
c		irec=irec+65536
c	   else if(irec.lt.-32768) then
c		print*,' IREC IS TOO BIG!!'
c		STOP
c	   endif
c Read the specified record  'header' (part only)
	   read(14,rec=irec) cdate1,ctime1,title11,naver1,navc1,iav1,
     &	control1,vjump1
	   call PREC(nj2,title11,cdate1,ctime1,naver1,navc1,iav1,vjump1,
     &   control1,iver1,1)
	   call PPAR(1,0,cdate1,ctime1,iTSAMP,iTPREc,nsamp,nDv,iDd,
     &   calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     & vjump1,control1,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     &   iver1,amVpA1,ftape,gain,errfac,nkeep,ikeep1,kstep,keepall,
     &   ivhclamp,ipatch,ffilt)
	endif
91	print 12,fcn
12	format(/,
     &' (1) Use these records as they are',/,
     &' (2) Specify new calibration (value from CJUMP is wrong)',/,
     &' (3) Specify new holding potential (value from CJUMP is wrong)',/
     &,' (4) Now filtered at fc = ',f8.1,' Hz -specify new filtering',/,
     &' (5) Specify different records',/,
     & ' Option number [1] = ')
	iopt=1
	call INPUTi(iopt)
	if(iopt.le.0.or.iopt.gt.5) iopt=1
	if(iopt.eq.5) goto 33
c
	if(iopt.eq.4) then
	   filter=.true.
	   print 53,fcn
53	   format(
     &    ' At present filtered at ',f9.1,' Hz -new final fc (Hz) = ')
	   call INPUTr(fcn)
	   if(fcn.lt.ffilt-1.) then
	     fcnew=1./sqrt((1.0/(fcn*fcn)) - (1.0/(ffilt*ffilt)))
	     print 52,ffilt,fcnew,fcn
           if(pon()) write(7,52) ffilt,fcnew,fcn
           if(discprt) write(8,52) ffilt,fcnew,fcn
52	     format(
     &  ' Data already filtered at ',g13.6,' Hz.',/,
     &  '  Filtered at fc = ',g13.6,' Hz to give final fc = ',g13.6)
	      tpref=0.8*float(itPREc)/1000.
	      print 54,tpref
54	      format(
     & ' Use mean of first and last x ms to eliminate end effect: [',
     &   	f8.2,'] x = ')
		call INPUTr(tpref)
		itpref=tpref*float(irate)/1000.	!number of points
	   else
		filter=.false.
	   endif
	   goto 91
	endif
c
	if(fixgain) then
	   call BELL(1)
	   print 681
681	   format('&Continue to override gain read from disc [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') then
		fixgain=.false.
	   else
		calfac=calfac1
		calvolt=calvolt1
	   endif
	endif
	if(fixpot) then
	   call BELL(1)
	   print 682
682	   format(
     &	'&Continue to override holding pot read from disc [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') then
		fixpot=.false.
	   else
		ivhold=ivhsav
		ivhclamp=ivhcsav
	   endif
	endif
	if(iopt.eq.2) then
	   calvolt=10.		!factor by which Vout is greater then true Em
	   call GETCAL(calfac,amVpA,amVpA1,VpA,VpA1,pAV,pAV1,
     &   itape,ftape,gain,errfac)
	   i=ifixr(calvolt)
	   print 68,i
68	   format(
     & ' mV out from clamp per mV membrane pot (integer) [',i3,'] = ')
	   call INPUTi(i)
	   calvolt=float(i)
	   calfac1=calfac		!save values to overwrite those from disc
	   calvolt1=calvolt	!save values to overwrite those from disc
	   fixgain=.true.
	endif
	if(iopt.eq.3) then
	   i=ivhold
	   print 683,i
683	   format('&Holding potential set by 1401 (mV) [',i4,'] = ')
	   call INPUTi(i)
	   ivhsav=int2(i)
	   ivhold=ivhsav
	   i=ivhclamp
	   print 684,i
684	   format(
     & '&Holding potential set on patch clamp box (mV) [',i4,'] = ')
     	   call INPUTi(i)
	   ivhcsav=int2(i)
	   ivhclamp=ivhsav
	   fixpot=.true.
	endif
c	read 101,ans
c	if(UC(ans).eq.'N') goto 33
c
c
	irec=istrec(nj1)			!read data set #nj1
c	irec=int4(istrec(nj1))			!read data set #nj1
c	if(irec.le.-1.and.irec.ge.-32768) then
c	   irec=irec+65536
c	else if(irec.lt.-32768) then
c	   print*,' IREC IS TOO BIG!!'
c	   STOP
c	endif
	jc=1		!current in yval(i,1)
	jv=2		!voltage in yval(i,2)
	if(nj2.gt.0) jv=3		!voltage in yval(i,3)
c Set Vramp=true if sweep contains at least one voltage ramp (used to
c set ndimd below, and in CJVDU.
	vramp=.false.
	if(nvjump.gt.0) then
c=	   nvramp=0
	   do i=1,nvjump
       	if(ivolt1(i).ne.ivolt2(i)) vramp=.true.
	   enddo
	endif
c
c Can now allocate Xval, Yval to correct size for ANY version of cjump
	if(allocated(xval)) DEALLOCATE(xval)
	if(allocated(yval)) DEALLOCATE(yval)
	ndv1=nsamp0
c=	ndimd=2		!max number of arrays in yval
	ndimd=10		!set on the safe side
	ALLOCATE(Xval(ndv1,ndimd),Yval(ndv1,ndimd))
	if(allocated(ndat)) then
	   DEALLOCATE(ndat,icurvd,isym,ijoin,symsiz)
	endif
	ALLOCATE(ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd))
	ALLOCATE(symsiz(ndimd))
c
	call READREL(irec,nsamp,iav,sampv,calfac,calvolt,
     &    ipatch,ivhclamp,Yval,jc,jv,ndv1,ndimd,ioffset,newform)
	if(filter) then
	   nd1=-53
	   nd2=nsamp+54
	   ALLOCATE(yin(nd1:nd2),yout(nd1:nd2))
	   do i=1,nsamp
		if(i.le.itpref) y1=y1+yval(i,jc)	!mean of 1st itpref points
		if(i.gt.nsamp-itpref) y2=y2+yval(i,jc) !mean of last itpref points
		yin(i)=yval(i,jc) 		!copy data to ytemp
	   enddo
	   y1=y1/float(itpref)
	   y2=y2/float(itpref)
	   do i=nd1,0
		yin(i)=y1
	   enddo
	   do i=nsamp+1,nd2
		yin(i)=y2
	   enddo
	   srate=float(irate)
	   call GFILTERR(yin,yout,nd1,nd2,srate,fcnew,nd1,nd2)
	   do i=1,nsamp
		yval(i,jc)=yout(i)
	   enddo
	   DEALLOCATE(yin,yout)
	endif
c
c Print the details (not on screen)
	call PREC(nj1,title1,cdate,ctime,naver,navc,iav,vjump,
     & control,iver1,3)
	idprt=2	!full details to disc
	if(title1.eq.title0) idprt=1	!brief details only if same title
	title0=title1		!keep last title for next time round
	call PPAR(0,idprt,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     & calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     & vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     &   iver1,amVpA1,ftape,gain,errfac,nkeep,ikeep1,kstep,keepall,
     &   ivhclamp,ipatch,ffilt)
c Read 2nd data set if req
	if(nj2.gt.0) then
	   irec=istrec(nj2)
c	   irec=int4(istrec(nj2))
c	   if(irec.le.-1.and.irec.ge.-32768) then
c		irec=irec+65536
c	   else if(irec.lt.-32768) then
c		print*,' IREC IS TOO BIG!!'
c		STOP
c	   endif
	   call READ14(irec,cdate,ctime,title1,naver1,navc1,iav,control1,
     &  vjump,sampv,nsamp2,itsamp1,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &     ivolt1,ivolt2,calfac,calvolt,ivhold,irate1,iTPREc,nAc,dnAc,
     &     ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     &     ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax,
     &     ivhclamp,ipatch,ffilt,sampatch,0)
c value of nsamp read from disk now named nsamp2 (nsamp1 previously) OK?
	   nsamp0=nsamp2		!# of points if all points kept
c  Check if ipatch changed for #nj2 also
	   if(iver1.le.-1003) then
		if(allocated(jmask)) DEALLOCATE(jmask)
		ALLOCATE(jmask(nsamp0))
		call CALCNS2(ikeep1,nkeep,kstep,nsamp2,nsamp1,jmask,
     &    	  keepall,nsamp0)
		nsamp0=nsamp		!orig # of points samples
		nsamp=nsamp1		!# of points kept on disc
	   endif
	   call ILCONV(ilenc,igapc,ilenv,igapv,
     &	ilen2c,igap2c,ilen2v,igap2v,iDd,1)	!convert ilen2c to ilenc etc
	   if(fixpot) then
		call BELL(1)
		print 685
685		format(
     &	'&Override holding pot for second jump too [Y] ? ')
		read 101,ans
		if(UC(ans).eq.'N') then
		   fixpot=.false.
	 	else
		   ivhold=ivhsav
		   ivhclamp=ivhcsav
		endif
	   endif
	   call PREC(nj2,title1,cdate,ctime,naver1,navc1,iav,vjump,
     &   control1,iver1,2)
	   call PPAR(1,idprt,cdate,ctime,iTSAMP1,iTPREc,nsamp2,nDv,iDd,
     &   calfac,calvolt,nAc,dnAc,irate1,ncjump,ilenc,igapc,ivhold,sampv,
     &  vjump,control1,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     &   iver1,amVpA1,ftape,gain,errfac,nkeep,ikeep1,kstep,keepall,
     &   ivhclamp,ipatch,ffilt)
c Test a few things to check samples comparable
	if(itsamp.ne.itsamp1.or.irate.ne.irate1.or.nsamp.ne.nsamp2) then
	   call BELL(3)
	   print 20
20	   format(' Parameters for the 2 data sets do not match')
	   goto 42
	endif
c NB problem here because may want to plot 2 records, both of which are
c controls (or both 'drug'), but 2nd AVCUR would overwrite first AVCUR (in
c former case; ditto for AVCURCON in latter. Thus better put data straight
c into Xval,Yval here)
	   if(control.and.(.not.control1)) then
		ncon=1		!#nj1 is control
	   else if(control1.and.(.not.control)) then
		ncon=2		!#nj2 is control
	   endif
	   if(control.and.control1) then
		call BELL(2)
		print 130
130		format(' Both records are controls O.K. [N] ? ')
		read 101,ans
		if(UC(ans).ne.'Y') goto 33	!read again
		ncon=1		!#nj1 is control
		print 132
132		format('&For net trace subtract #1 from #2 [Y] ? ')
		if(UC(ans).eq.'N') ncon=2
	   else if((.not.control).and.(.not.control1)) then
		call BELL(2)
		print 131
131		format(' Neither record is a control O.K. [N] ?')
		read 101,ans
		if(UC(ans).ne.'Y') goto 33	!read again
		ncon=1		!#nj1 is control
		print 132
		if(UC(ans).eq.'N') ncon=2
	   endif
c (a) If one data set only read (#nj1, nj2=0) then jc=1,jv=2
c (b) If two data sets read (#nj1, #nj2) then for #nj1 jc=1, and
c	for #nj2 jc=2. If voltage recorded then for #nj1 jv=3
c	and for #nj2 jv=4,
	   jc=2		!current in yval(i,2)
	   jv=4		!voltage in yval(i,4)
	   call READREL(irec,nsamp,iav,sampv,calfac,calvolt,
     &     ipatch,ivhclamp,Yval,jc,jv,ndv1,ndimd,ioffset,newform)
	   if(filter) then
		nd1=-53
		nd2=nsamp0+54
		ALLOCATE(yin(nd1:nd2),yout(nd1:nd2))
		do i=1,nsamp0
		   if(i.le.itpref) y1=y1+yval(i,jc)	!mean of 1st itpref points
		   if(i.gt.nsamp0-itpref) y2=y2+yval(i,jc) !mean of last itpref points
		   yin(i)=yval(i,jc) 		!copy data to ytemp
		enddo
		y1=y1/float(itpref)
		y2=y2/float(itpref)
		do i=nd1,0
		   yin(i)=y1
		enddo
		do i=nsamp0+1,nd2
		   yin(i)=y2
		enddo
		srate=float(irate)
		call GFILTERR(yin,yout,nd1,nd2,srate,fcnew,nd1,nd2)
		do i=1,nsamp0
		   yval(i,jc)=yout(i)
		enddo
		DEALLOCATE(yin,yout)
	   endif
	endif
c
c NB At present all values for jumps taken from 2nd record if two are
c read (values overwrite those for 1st record); check that rates etc
c are same for both?
c
c Header and data read for the specified record: now plot it, properly
c scaled.  Option to plot more than one (superimposed). Also (option?)
c to plot the DAC pulse (as a calc curve, or as overlay done after VPLOT1?,
c or as two arrows, to mark start and end of pulse; arrows can be inserted
c via common/tpos/ in VPLOT1, as done in AUTPLT1 for example).
c OR write special VPLOTR routine for relaxations?
c
c Options for display:
c (1) If c-jump only (1 record read in; nj2=0) then show this record (in AVCUR
c	whether averaged or not) and decide on region to be fitted
c (2) If V-jump then 2 data sets read in (nj2 > 0) so display both, and
c	then difference. If nvjump>1 then must first specify which of the
c	jumps/ramps is to be displayed. For ramps display as I/V curve
c	Also option to display Voltage vs time when sampv=true.
	samescal=.false.	!do not use same scales as before
307	continue
	ndv1=nsamp
	if(allocated(xfit)) DEALLOCATE(Xfit,Yfit)
	ALLOCATE(Xfit(ndv1),Yfit(ndv1))
	if(allocated(jmask)) then
	   nmax=nsamp0		!size of jmask
	else
	   nmax=1			!allocate anyway, as it is a param of CJVDU
	   ALLOCATE(jmask(nmax))
	endif
	call CJVDU(Xval,Yval,xval0,xvaln,vramp,sampv,
     & nsamp,itsamp,nAv,dnAv,nvjump,ilenv,igapv,ivolt1,ivolt2,
     & irate,nAc,dnAc,ncjump,ilenc,igapc,nj1,nj2,ncon,
     & t1c1,t2c1,t1v1,t2v1,vstep,samescal,rescale,ypeak,ybase,tau1,
     & ISYM,ILOG,ISCAL,IJOIN,iline,XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,
     & ixlo,ixhi,iylo,iyhi,y0,yinf,xcross,ycross,titlex,titley,title,
     & itit,ilabel,inumx,inumy,ncurvd,ndat,icurvd,ncurvc,ncal,icurvc,
     & symsiz,idiskq,ntx,nty,itx,ity,fitted,doframe,dcurve,refit,
     & ifit,super,nsuper,isuper,xdelt,ndelt,ilrn,lrnfit,jfit,
     & ikeep1,nkeep,kstep,keepall,nsamp0,jmask,idest,Erev,
     & ndv1,ndimd,xfit,yfit,nmax,ivhclamp,ipatch,vmin,vmax,
     & average,nmean,xmean,ncrvdsav,icrvdsav,
     & xmin0,xmax0,ymin0,ymax0,logx,logy,
     & xminsav,xmaxsav,yminsav,ymaxsav,
     & avact,tpre,avsamp,dfinter,plotqdat)
c Now fit if idest=3, otherwise (if no fit, or fit already displayed) ask
c if another required
	if(idest.eq.3) then
	   call CJSIMP(vramp,kfit,jfix,nfix,titlep,fitted,logx,
     &   dcurve,theta,nev,sres,smin,xval,yval,xmin,xmax,ymin,ymax,ndat,
     &   icurvd,ypeak,ybase,tau1,xfit,yfit,ndv1,ndimd,idest)
	   call CJOUT(nev,smin,jfix,fitted,titlep,
     &    vramp,vmin,vmax,Erev)
	   print 5
5	   format(' Type <any key> for display')
	   call ANYKEY
	   goto 307	!for display/plot with fitted=true
	endif
c=RELDAT files abolished now!
c=	else if(iread.ne.2) then	!do not ask if read from RELDAT!
c=	   ans='N'
c=	   if(store) ans='Y'
c=	   print 52,ans
c=52	   format(
c=     &' Store data permanently on jump (relaxation) floppy [',a1,'] ? ')
c=	   read 101,ans1
c=	   ans1=UC(ans1)
c=c Change default only if ans='y' and ans1='n', or vice versa
c=	   if(ans.eq.'Y'.and.ans1.eq.'N'.or.ans.eq.'N'.and.ans1.eq.'Y')
c=     &	store=.not.store
c=	   if(store) then
c=c=======not fixed for CJUMP3 OR for nsamp>2048
c=c Store values on floppy
c=		nsweep=nj1
c=		nsweep1=nj2
c=		first=.true.
c=	      call PREC(nj1,title1,cdate,ctime,naver,navc,iav,vjump,
c=     &      control,iver1,1)
c=		if(nj2.gt.0) then
c=	        call PREC(nj2,title11,cdate1,ctime1,naver1,navc1,iav1,
c=     &        vjump1,control1,iver1,1)
c=54		  continue
c=		  if(jfit.eq.1.or.jfit.eq.3.or.jfit.eq.4) then
c=		   print 53,nj1,nj2
c=53		   format(' (1) Store sweep ',i5,/,' (2) Store sweep ',i5,/,
c=     &	  '  Option number = ')
c=		  else if(jfit.eq.2) then
c=		   print 531,nj1,nj2
c=531		   format(
c=     &	' (1) Store first average -started with event ',i5,/,
c=     &	' (2) Store second average -started with event ',i5,/,
c=     &	  '  Option number = ')
c=		  endif
c=	   	  call INPUTi(iopt)
c=		  if(iopt.ne.1.and.iopt.ne.2) goto 54
c=		  if(iopt.eq.2) then
c=			nsweep=nj2
c=			nsweep1=nj1
c=		  endif
c=		endif		!end of nj2>0 bit
c=c store sweep # nsweep -reread 'header'
c=58		continue
c=		irec=int4(istrec(nsweep))
c=		read(14,rec=irec) cdate,ctime,title1,naver,navc,iav,control,
c=     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
c=     & 	ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
c=     &      ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
c=     &      ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac,nmax
c=c==	      if(iver1.eq.-1003) then
c=	      if(iver1.eq.-1003.or.iver1.eq.-1004) then
c=	       call CALCNS2(ikeep1,nkeep,kstep,nsamp,nsamp1,jmask,
c=     &         keepall,nmax)
c=	       nsamp0=nsamp		!orig # of points samples
c=	       nsamp=nsamp1		!# of points kept on disc
c=	      endif
c=	      call ILCONV(ilenc,igapc,ilenv,igapv,
c=     &	ilen2c,igap2c,ilen2v,igap2v,iDd,1)	!convert ilen2c to ilenc etc
c=c	 and reread data for jfit=1 case (for jfit=2 data is only in Yval)
c=		if(jfit.eq.1) then
c=		 jc=1		!read current into yval(i,1)
c=		 jv=2		!read voltage into yval(i,2)
c=		call READREL(irec,nsamp,iav,sampv,calfac,calvolt,
c=     &    	  Yval,jc,jv,ndv1,ndimd,ioffset,newform)
c=		 do 55 i=1,nsamp
c========THIS IS FIRST TIME AVCUR IS USED IN MAIN PROG! USED ONLY FOR RELWRT!
c=55		 avcur(i)=Yval(i,1)
c=		 if(sampv) then
c=		   do 56 i=1,nsamp
c=56		   avolt(i)=Yval(i,2)
c=		 endif
c=		else if(jfit.eq.2) then
c=		 if(nsweep.eq.nj1) then
c=		  jc=1			!current already in yval(i,1)
c=		  jv=2			!voltage already in yval(i,2)
c=		  if(nj2.gt.0) jv=3		!voltage in yval(i,3)
c=		 else if(nsweep.eq.nj2) then
c=		  jc=2		!current in yval(i,2)
c=		  jv=4		!voltage in yval(i,4)
c=		 endif
c=		 do 70 i=1,nsamp
c=c70		 avcur(i)=Yval(i,jc)
c=70		 avcur(i)=Yval1(i)		! Brian's temp modification
c========THIS IS LAST TIME AVCUR IS USED IN MAIN PROG!
c=		 if(sampv) then
c=		    do 71 i=1,nsamp
c=71		    avolt(i)=Yval(i,jv)
c=		 endif
c=		endif
c=c    Put most args in COMMON for RELWRT -easier to use latter for both CJFIT
c=c	and TRANAV this way
c=		iprog=1		!tells RELWRT that data is from CJUMP
c=		call RELWRT(iprog,cdate,calfac)
c=		if(nj2.gt.0.and.first) then
c=		 if(jfit.eq.1) then
c=		   print 57,nsweep1
c=57		   format(' Store also sweep # ',i4,' [Y] ? ')
c=		 else if(jfit.eq.2) then
c=		   print 572
c=572		   format(' Store other average too [Y] ? ')
c=		 endif
c=		 read 101,ans
c=		 if(UC(ans).ne.'N') then
c=			nsweep=nsweep1
c=			first=.false.
c=			goto 58	!store the other
c=		 endif
c=		endif
c=	   endif		!end of storage in RELDAT
c=c
c
	   call SAVEDP		!saveguard disc print file!
c
511	   continue
	   print 51
c51	   format('&Display/fit another curve [Y] ? ')
c	   read 101,ans
c	   if(UC(ans).ne.'N') then
c		print 511
c511		format('&Refit the same data [N] ? ')
c	      read 101,ans
c	      if(UC(ans).ne.'Y') goto 42    !different data
51	   format(
     &	' (1) Refit same data',/,
     &	' (2) Display/fit new data',/,
     &	' (3) Exit from CJFIT',/,
     &   	' Option number [2] = ')
	   iopt=2
	   call INPUTi(iopt)
	   if(iopt.eq.3) goto 999
	   if(iopt.eq.0) iopt=2
	   if(iopt.lt.1.or.iopt.gt.3) goto 511
	   if(iopt.eq.2) goto 42		!new data
c=	   if((iopt.eq.1).and.(makav.eq.3)) then
c=	     print 1010
c=1010     format(' Fit the ensemble for off-jump activations [Y] ? ')
c=	     read 101,ans
c=	     if(UC(ans).eq.'N') then
c=	       goto 9999			! finish
c=	     else
c=	       Do 1020 i=1,nsamp
c=		   if(i.le.ntemp1) then
c=	           YVAL1(i)=yacav2(i)
c=		   else if(i.gt.ntemp1) then
c=		     YVAL1(i)=0.0
c=		   endif
c=1020	       continue
c=	       goto 307
c=	     endif
c=	   else

	   fitted=.false.			!same data
	   ncurvc=0
	   print 512
512	   format('&Same scales as before for display [Y] ? ')
	   read 101,ans
	   if(UC(ans).ne.'N') then
		xmin=xminsav
		xmax=xmaxsav
		ymin=yminsav
		ymax=ymaxsav
		xcross=xmin
		ycross=ymin
		samescal=.true.
	   endif
	   goto 307                      !same data
c=	endif
c
999	continue
      if(iread.eq.1) CLOSE(unit=14)
9999	continue
      OPEN(unit=19,file='CJFIT.INI',status='UNKNOWN',
     &   access='TRANSPARENT')
	write(19,rec=1) ndev,datfil,idiskq,nfile1,scanfil,kfile1,
     &	treso,tresg,
     &	consfil,scantfil,scdatfil,nfile,kfile,iread,jfit,
     &	nomit,(jumpomit(i),i=1,nomit)
	CLOSE(unit=19)
	call ENDPRINT
	END

