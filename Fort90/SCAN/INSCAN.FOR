	subroutine INSCAN(iampl,tint,iprops,ntmax,nbuf,novlap,srate,
     & finter,tsect,ioff,ndat,nsec,nrlast,savin,nshutfit,
     & kt,nwrit,nwsav,newpar,igback,restart,opendown,invert,
     & usepots,disp,smult,scrit,rms1,ntrig,navtest,dgain,iboff,
     & expfac,bdisp,ibflag,iautosub,xtrig,ndev,cdate,adctime,
     & nsetup,filtfile,filt1,nfilt1,dtf1,timamax,avamp,ampdone,fcdone,
     & tf01,tf1,tf50,tf99,tf999,npfilt,ffilt,filt1z,nfilt1z,dtf1z,
     & yd1,yd2,sfac1,sfac2,sfac3,nscale,calfac,calfac1,calfac2,iyoff,
     & dfinter,disptran,dispderiv,dispguess,ampfac,tmin,
     & tsfac,tlfac,tcfac,sdone,facjump,errfac,confac,stpfac,
     & derivfac,idest,izoom,fcz,fczoom,ampz,avampz,filtzoom,
     & cjump,njump,ktjump,nfjump,tzero,intzero,tsample,istrec,
     & njdim,jumplen,njfirst,ntotjump,filtf,itsimp,minmeth,nbasemin,
     & newfile,ioffset,iscrit,scritvar,smultmin,nchan,idatyp,cdate1,
     & adcfil,expdate,defname,tapeID,ipatch,npatch,Emem,temper)
c
c SOME PARAM AND ALL OF COMMON ARE FOR READ OF RESTART VALUES

c Modif 03/10/99 08:01am by adding total number of jumps as parameter, ntotjump
c Modif 12/28/98 09:32pm to read expdate etc from modified CONSAM (iver=1002)
c   or to input it here for other inputs (using getexpt, as in consam.for)
c
c
c Modif 10/16/98 08:58pm by addition of parameter
c iscrit=1 to use scrit=smult*avamp (as in all recent versions)
c iscrit=2 to use scrit=smult*rms (as in orig version) -latter could be
c better for records with very wide range of amps
c  Also option to use smaller Scrit for fittings that have only small
c  amplitudes when scritvar=true -in this case value of Scrit changed after
c  region to be fitted has been defined, with minimum Smult=smultmin

c Modif 05/16/96 04:01pm to read Axon files.  The logical variable, pdpdata,
c if read back as an integer*4, gives true=1, false=0, therefore replace it
c with idatyp=0 for usual CONSAM, idatyp=1 for pdpdata and idatyp=2
c for Axon data.  Then old scantemp files should read back correctly
c Modif 07/27/95 07:04pm for change of filter when zoomed -add to
c  the parameters
c  (1) izoom=default zoom factor to be used
c  (2) fcz = final fc (equal to or less than input value=filtf) for
c	 filtering when zoomed (fczoom=actual fc applied to achieve this)
c	(both in kHz throughout, except for display in menu)
c  (3) avampz=alternative 'full amplitude' to use for event detection
c     when zoomed/filtered (3) default zoom factor (NB use zoomfac=1
c     or fczoom=filtf to get filtering only or zoom only).
c	avampz in intermed units (same as avamp); ampz=same thing pA
c Also define
c  (4) filtzoom=true if fcz less than filtf, so zoomed records are to
c	be filtered
c
c Modif 01/06/95 05:44pm to add CJUMP options
c Modif 03/24/94 04:56pm for new definition of SMULT (see SCAN)
c (avamp and ampdone added to parameters)
	character*1 ans,UC,ans1,savin
	real*8 dfinter,dround,dtick,tsample
	logical invert,opendown,newpar,sdone,restart,readini,fcdone
	logical disp,present,usepots,ampdone,good,newfile,scritvar
	logical disptran,dispderiv,dispguess,markamp,cjump,filtzoom
	logical discprt
	logical pon,slock,debug,caplock,axon
	integer*4 istrec(1000)		!new format
c	integer*2 istrec(250)		!orig=500 bytes (kept as int*2 to fit in 1 rec)
c declarations from CONSAM
	character ADCFIL*30,cdate*11,cdate1*11,ndev*2,ndevdat*2
	integer*2 idt,inchan,id1,id2,iver
	character cs*3,adctime*8
	character tapeID*24
	character*14 ptype(5)
	character expdate*11,defname*6,title1*70,title*76
c	character*76 title
c	character dosdate*12,dostime*12
c
	real*4 tint(ntmax)
	integer*2 iampl(0:ntmax)
	integer*1 iprops(ntmax)	!properties of ith interval (see SCAN)
	real*4 timamax(49)
c==	integer ksect(1024),ishut(1024)
c For PARAMWIN
	include 'c:\lf90\lib\spinatti.hdr'
	character*60 names(35)
	character*11 values(35)
	character*165 helpstr(35)
	integer jfix(35)
	integer*2 ialter(35)
c For GETFILT
	real*4 filt(100),filt1(1024),filt1z(1024)
	character ftitle*80,filtfile*20
	character*32 filnam
	character*40 mtitle
	common/dp/discprt
	character*4 prtport
	common/dpp/filnam,prtport,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c=	common/rstrtblk/nchan,idatyp,adcfil,cdate1
	common/aafunc/ trise
	common/ampmark/nampmark,iamark(10)
	external AFUNC
c
c define function
	pon()=slock()
	debug()=caplock()
c
101	format(a1)
c Get new parameters?
	if(newpar) goto 310
	axon=.false.
c
c INITIALISATIONS
	idest=0
c	nbuf=1048576 !1024*1024=dimension of array containing current data section
	nbuf=131072		!=1024*128
c	nbuf=65536		!=1024*64
c	nbuf=32768		!=1024*32
	novlap=2048
c	ntmax=1024	!dimension of tint(i) and iampl(i) -set in SCAN now
c SET DEFAULT PARAMETER VALUES
c ISUB=0: Returns from SUBLEV every time i.e. never
c stay in sublev for more than one fitting; if last amp was not shut
c then assumes next is shut
c ISUB=1: Returns from sublevel only if last amp was shut
c ISUB=2: Returns from SUBLEV if last amp was shut OR last amp was
c   within 10% of mean 'full' amp
	opendown=.true.
	INVERT=.FALSE.	!DO NOT INVERT IN PROG
	ntrig=2		!NO OF POINTS ABOVE TRIG LEVEL
	navtest=ntrig-1
	if(navtest.le.0) navtest=1
c navtest=number averaged before average curlev is used, rather than
c input curlev in FINDTRANS (NB must be less than ntrig, or, for example,
c if input baseline is not close to current baseline (ie baseline has
c drifted since last time) then will get a 'trigger' straight away!
	DGAIN=1.0	!GAIN FACTOR FOR DISPLAY
	IBOFF=0		!BASELINE OFFSET FOR DISPLAY (ADC)
	nwrit=100	!write tint() etc to disc every nwrit transitions
	expfac=2.	!INITIAL DISPLAY EXPANSION FACTOR
	bdisp=0.75	!display baseline:y=75% up screen (if openings downwards)
	if(.not.opendown) bdisp=0.25	!default display baseline if openings upwards
	isub=0
	xtrig=0.2	!trigger at 20% of X axis on screen
	usepots=.false.
	nshutfit=50
	facjump=0.6
	stpfac=0.1
	iautosub=1
	disptran=.false.
	dispderiv=.false.
	dispguess=.true.
	markamp=.false.
	nwsav=0		!used for auto disc write
	igback=0
	tmin=15.		!microsec below which refit suggested (see CHECKFIT)
	tsfac=2.0		!tsfac*trise=tshort
	tlfac=3.0		!tlfac*trise=tlong
	tcfac=4.		!tcfac*trise=tclose
	ampfac=0.05		!fraction of full amp for 'real' amp diff (see CHECKFIT)
c				! and CHKGUESS
	errfac=0.005	!convergence criterion for Simplex
	confac=0.5		!contraction factor for Simplex
	derivfac=3.		!see DERIVSCH
	itsimp=1200		!# of simplex iterations before dfpmin used
	minmeth=2
	nbasemin=10
	iscrit=1
c	iscrit=2
	smult=0.14 				!scrit=0.14*avamp
	if(iscrit.eq.2) smult=5.	!scrit=5.0*rms
	scritvar=.false.
	smultmin=2.5		!2.5*rms1 in intermed units
	filtzoom=.false.
	izoom=1
c
c INSCAN is skipped if Restart requested
	restart=.false.
	readini=.false.
c	if(.not.restart) then         !read .INI
	iopt=2
	INQUIRE(file='scan.ini',exist=present,flen=inilen)
	if(present.and.inilen.gt.0) then
	   print 10,iopt
10	   format(
     & ' (1) Use program default values for all parameters',/,
     & ' (2) Read parameter values from SCAN.INI',/,
     & ' Option [',i2,'] = ')
	   call INPUTi(iopt)
	else
	   iopt=1
	endif
c=========need more default values for cjump case in .ini?
c CJUMP etc added 01/06/95 05:44pm
	if(iopt.eq.2) then
	   readini=.true.
	   if(inilen.eq.256) then
      	OPEN(unit=15,file='SCAN.INI',status='UNKNOWN',
     &      access='DIRECT',form='UNFORMATTED',recl=256)
	      read(15,rec=1)savin,nbuf,novlap,opendown,invert,smult,ntrig,
     &   dgain,iboff,nwrit,iexp,bdisp,isub,xtrig,usepots,ndevdat,disp,
     &   nsetup,filtfile,navtest,iautosub,disptran,dispderiv,dispguess,
     &   tsfac,tlfac,tcfac,facjump,ampfac,errfac,derivfac,confac,
     &   adcfil,nampmark,iamark,tmin,cjump,
     &   izoom,fcz,ampz,itsimp,minmeth,nbasemin,iscrit
		CLOSE(unit=15)
c now write the new .ini with 512 byte record
      	OPEN(unit=15,file='SCAN.INI',status='UNKNOWN',
     &      access='DIRECT',form='UNFORMATTED',recl=512)
	     write(15,rec=1)savin,nbuf,novlap,opendown,invert,smult,ntrig,
     &   dgain,iboff,nwrit,iexp,bdisp,isub,xtrig,usepots,ndevdat,disp,
     &   nsetup,filtfile,navtest,iautosub,disptran,dispderiv,dispguess,
     &   tsfac,tlfac,tcfac,facjump,ampfac,errfac,derivfac,confac,
     &   adcfil,nampmark,iamark,tmin,cjump,
     &   izoom,fcz,ampz,itsimp,minmeth,nbasemin,iscrit,
     &   scritvar,smultmin,stpfac
	       CLOSE(unit=15)
	   else if(inilen.eq.512) then
      	OPEN(unit=15,file='SCAN.INI',status='UNKNOWN',
     &      access='DIRECT',form='UNFORMATTED',recl=512)
	     read(15,rec=1)savin,nbuf,novlap,opendown,invert,smult,ntrig,
     &   dgain,iboff,nwrit,iexp,bdisp,isub,xtrig,usepots,ndevdat,disp,
     &   nsetup,filtfile,navtest,iautosub,disptran,dispderiv,dispguess,
     &   tsfac,tlfac,tcfac,facjump,ampfac,errfac,derivfac,confac,
     &   adcfil,nampmark,iamark,tmin,cjump,
     &   izoom,fcz,ampz,itsimp,minmeth,nbasemin,iscrit,
     &   scritvar,smultmin,stpfac
	       CLOSE(unit=15)
	   endif
	   if(izoom.lt.1) izoom=4
	   if(fcz.lt.0.01) fcz=1.		!1kHz
	   if(abs(ampz).lt.0.01) ampz=2.5	!pA
	   if(minmeth.lt.1.or.minmeth.gt.3) minmeth=2	!minimisation method
	   if(itsimp.lt.50.or.itsimp.gt.5000) itsimp=1000
	   if(nbasemin.lt.4.or.nbasemin.gt.500) nbasemin=10
	   if(iscrit.lt.1.or.iscrit.gt.2) iscrit=1
	   if(iscrit.eq.1) then
		if(smult.ge.1.0) smult=0.12      !set default
	   else if(iscrit.eq.2) then
		if(smult.lt.1.0.or.smult.gt.8.) smult=3.0     !set default
	   endif
	   if(stpfac.lt.0.01.or.stpfac.gt.5.) stpfac=0.1
	endif
c Initialisations when NOT restart
	kt=0		!# of transitions found
c
	do i=kt+1,ntmax
	   iampl(i)=-9999	!until defined
	   tint(i)=0.
	   iprops(i)=0
	enddo
c
c Read signal from disc
52	continue
	iopt=1
	if(cjump) iopt=2
	print 50,iopt
50	format(
     & ' (1) Data from continuous sample (CONSAM) file',/,
     & ' (2) Data from CJUMP data file',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(iopt)
	if(iopt.eq.1) then
	   cjump=.false.
	else if(iopt.eq.2) then
	   cjump=.true.
	else
	   goto 52
	endif
c
c For C-jumps, get name of file, and leave it open with ioff=byte offset
c to read the binary data for the current jump (in READSEC)
	if(cjump) then
	  ians=1
711	  print 71,ians
71	  format(
     & ' (1) Data from standard CJUMP.DAT file ',/,
     & ' (2) Data from AXON continuous data file (Axon interface)',/,
     & ' Option number [',i2,'] = ')
	  call INPUTi(ians)
	  if(ians.eq.1) then
	    call READJUMP(njump,nsamp,ioff,ndisc,adcfil,calfac,
     &      srate,ffilt,ktjump,nfjump,njdim,tzero,intzero,
     &      tsample,readini,istrec,jumplen,njfirst,ntotjump,
     &	newfile,ioffset,ipatch,Emem,idest,0)
	  else
	    ndev=ndevdat	!default from .INI
	    if(.not.readini) then
	     if(ndisc.gt.1) then
	     print 55
	     call DISCNUM1(idisc,ndev,0,1)
	     if(idisc.eq.-1) STOP
	    else
	     ndev='C:'
	    endif
	     ADCFIL=ndev//'\CJUMP.DAT'
	    endif
3301	    call TITENT0('Input CJUMP file name:',ADCFIL,30,.false.)
	    INQUIRE(file=ADCFIL,exist=present,flen=nlen)
	    if(present.and.nlen.gt.0) then
	     axon=.true.
	     call READAXON(adcfil,title,cdate,adctime,ilen,inchan,
     &       calfac,pAV,srate,ffilt,iver,ioff,good)
		 faccal=pAV/calfac	!to convert calfac if pAV corrected
		 nsamp=ilen/2
		 njump=200
		 njdim=nsamp/njump
	     else
	      print 301,adcfil
301	      format(' CAN NOT FIND: ',a30)
		goto 3301
	     endif
	  endif
	  if(idest.eq.999) STOP	!abort DISCNUM
	  if(.not.axon) pAV=calfac*6553.6		!pA/volt (display now scaled in volts)
	  ndat=nsamp
	  call GETEXPT(expdate,defname,tapeID,ipatch,
     &		npatch,Emem,temper)
	  goto 200
	endif
c
c Next the stuff for CONSAM files
c Define idatyp=0 for usual CONSAM, idatyp=1 for pdpdata and idatyp=2
c for Axon data, idatyp=3 for generic file.
	if(idatyp.eq.0) then
	   ians=1
	else if(idatyp.eq.1) then
	   ians=3
	else if(idatyp.eq.2) then
	   ians=2
	endif
511	print 51,ians
51	format(
     & ' (1) Data from standard CONSAM.DAT file (CED 1401 interface)',/,
     & ' (2) Data from AXON continuous data file (Axon interface)',/,
     & ' (3) Data from PDP11 file (CED512 interface)',/,
     & ' (4) Data from any 16 bit integer binary file',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(ians)
	if(ians.eq.1) then
	   idatyp=0
	else if(ians.eq.2) then
	   idatyp=2
	else if(ians.eq.3) then
	   idatyp=1
	else if(ians.eq.4) then
	   idatyp=3
	endif
	if(idatyp.eq.1) then	!PDP consam file
	   if(ndisc.gt.1) then
	      print 55
55	      format(' Specify disc partition for CONSAM file:')
	      call DISCNUM1(idisc,ndev,0,0)	!ask for winchester (only)- no default
	   else
		ndev='C:'
	   endif
c	   if(idisc.eq.-1) goto 999
	   if(idisc.eq.-1) STOP
c	   adcfil=ndev//'\CONSAM.DAT'
	   adcfil(1:2)=ndev
	   call TITENT0('Input CONSAM file name:',ADCFIL,30,.false.)
	   INQUIRE(file=ADCFIL,exist=present,flen=ilen)
	   if(.not.present.or.ilen.eq.0) goto 52
	   call GETCAL(calfac,amVpA,amVpA1,VpA,VpA1,pAV,pAV1,
     &    itape,ftape,gain,errfac1)
	   calfac=calfac*6553.6/409.6	!correct for 502 interface (-2048 to 2047)
	   print 531
531	   format('&Sample rate (Hz) = ')
	   call INPUTr(srate)
	   call GETEXPT(expdate,defname,tapeID,ipatch,
     &		npatch,Emem,temper)
	   print 53,ndev,ilen,calfac,srate
	   if(discprt) write(8,53) ndev,ilen,calfac,srate
53	   format(' PDP11 file CONSAM.DAT on ',a2,' has ',i8,' bytes',/,
     &   ' gain (pA per ADC unit) = ',g13.6,
     &	':   sample rate (Hz) = ',g13.6)
	   nchan=1
	   ioff=0
         OPEN(unit=14,file=ADCFIL,status='UNKNOWN',access='TRANSPARENT')
	   goto 54
	else if(idatyp.eq.3) then	!generic binary file
c The first data point, idata(1) starts at byte (record #) ioff+1, idata(2) at
c ioff+3, idata(3) at ioff+5 -so point #in (in whole data) starts at
c byte # ioff+(2*in-1)
	   call TITENT0('Name and path for data file',ADCFIL,30,.true.)
	   i1=1
	   print 60,i1
60	   format(
     &	' Byte # for start of data (=header length+1) [',i4,'] = ')
	   call INPUTi(i1)
	   ioff=i1-1
	   INQUIRE(file=ADCFIL,exist=present,flen=ilen)
	   if(.not.present.or.ilen.eq.0) goto 52
	   print 62
62	   format(
     &   ' Calibration factor (pA per ADC unit) = ')
	   call INPUTr(calfac)
	   print 531
c531	   format('&Sample rate (Hz) = ')
	   call INPUTr(srate)
	   print 3
c3	   format(' Low pass filter (Hz, -3dB) = ')
	   call INPUTr(ffilt)
	   print 63,ilen,adcfil,calfac,srate
	   if(discprt) write(8,63) ilen,adcfil,calfac,srate
63	   format(
     &   ' Generic data file has ',i8,' bytes: name = ',a30,/,
     &   ' gain (pA per ADC unit) = ',g13.6,
     &	':   sample rate (Hz) = ',g13.6)
	   nchan=1
	   ffilt=ffilt/1000.		!convert to kHz
         OPEN(unit=14,file=ADCFIL,status='UNKNOWN',access='TRANSPARENT')
	   goto 54
	endif
c	print 20
c20	format(' Read names of existing CONSAM files [N] ? ')
c	read 101,ans
c      if(UC(ans).eq.'Y') then
c	  n=ndisc		!from winprint.ini
c	  do i=1,n
c	    ndev=char(i+66)//':'
c	    ADCFIL=ndev//'\CONSAM.DAT'
c	    INQUIRE(file=ADCFIL,exist=present,flen=len)
c	    if(present.and.len.gt.0) then
c            OPEN(unit=14,file=ADCFIL,status='UNKNOWN',
c     &	  access='TRANSPARENT')
c	      read(14,rec=1) title,cdate1,adctime	!don't overwrite today's CDATE
c	      CLOSE(unit=14)
c	      print 111,ndev,cdate1,adctime,title
c111	      format(' Disk ',a2,' Date= ',a11,' time= ',a8,/,1x,a76,/)
c	    endif
c	  enddo
c	  pause
c	endif
c Which disk to take CONSAM from?
	ndev=ndevdat	!default from .INI
	if(.not.readini) then
	 if(ndisc.gt.1) then
	   print 55
c55	   format(' Specify disc partition for CONSAM file:')
	   call DISCNUM1(idisc,ndev,0,1)
	   if(idisc.eq.-1) STOP
	 else
	   ndev='C:'
	 endif
c	 ADCFIL=ndev//'\CONSAM.DAT'
	 ADCFIL=ndev//'\YYMMDDcs.DAT'
	endif
	call TITENT0('Input CONSAM file name:',ADCFIL,30,.false.)
c
c Open data file, and read header
31	INQUIRE(file=ADCFIL,exist=present,flen=nlen)
	if(present.and.nlen.gt.0) then
        OPEN(unit=14,file=ADCFIL,status='UNKNOWN',access='TRANSPARENT')
	  if(idatyp.ne.2) then
	     read(14,rec=1) iver
	     if(iver.eq.1002) then	!already have expdate etc
		 read(14,rec=1)iver,title1,cdate,adctime,idt,ioff,ilen,
     &	 inchan,id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,
     &	 expdate,defname,tapeID,ipatch,npatch,Emem,temper
	     else
	       read(14,rec=1) title,cdate,adctime,idt,ioff,ilen,inchan,
     &        id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,iver
		 title1=title(1:70)
		 if(iver.ne.1001) then
			call BELL(2)
			print 311
311			format(' ERROR IN CONSAM VERSION NUMBER')
		 endif
C=========REMOVE GETEXPT WHILE DEBUGGING
c		 ipatch=3		!FOR DEBUG
		 call GETEXPT(expdate,defname,tapeID,ipatch,
     &		npatch,Emem,temper)
c          write back header in new format
		 ans='Y'
		 call DCASK(
     &	 'Save these details in CONSAM (new format)',ans,ans)
		 if(ans.eq.'Y') then
		    iver=1002
		    write(14,rec=1)iver,title1,cdate,adctime,idt,ioff,ilen,
     &	   inchan,id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,
     &	    expdate,defname,tapeID,ipatch,npatch,Emem,temper
		 endif
	     endif
	  else
	     axon=.true.
	     call READAXON(adcfil,title,cdate,adctime,ilen,inchan,
     &            calfac,pAV,srate,ffilt,iver,ioff,good)
	      faccal=pAV/calfac	!to convert calfac if pAV corrected
		if(.not.good) goto 511
c          Axon files may not have correct filter frequency in header
		print 2,ffilt
2		format(' Low pass filter (-3dB) = ',f8.1,' Hz: O.K. [N] ? ')
		ans='N'
		call INPUTa(ans)
		if(UC(ans).ne.'Y') then
		   print 3
3		   format(' Low pass filter (Hz, -3dB) = ')
		   call INPUTr(ffilt)
		endif
		call GETEXPT(expdate,defname,tapeID,ipatch,
     &		npatch,Emem,temper)
	  endif
	  if(.not.axon) pAV=calfac*6553.6			!pA/volt (display now scaled in volts)
        print 35, ndev,cdate,adctime,ffilt,title,srate,ilen,calfac
        if(pon()) write(7,35) ndev,cdate,adctime,ffilt,title,srate,
     &	ilen,calfac
        if(discprt)write(8,35) ndev,cdate,adctime,ffilt,title,srate,
     &	ilen,calfac
35	  format(' Disk ',a2,' Date= ',a11,' time= ',a8,
     &    ' filter (Hz) = ',f9.1,/,1x,a76,/,
     &    ' sample rate (Hz) = ',f10.1,'  length (bytes) = ',i10,/,
     &    ' calibration factor (pA/ADC unit) = ',g13.6)
	  ffilt=ffilt/1000.		!convert to kHz
	  ffilt1=ffilt1/1000.		!convert to kHz
	else
	   print 30,adcfil
30	   format(' CAN NOT FIND: ',a30)
	   if(ndisc.gt.1) then
		print 55
c55	      format(' Specify disc partition for CONSAM file:')
		call DISCNUM1(idisc,ndev,0,1)
		if(idisc.eq.-1) STOP
	   else
		ndev='C:'
	   endif
	   ADCFIL=ndev//'\CONSAM.DAT'
	   call TITENT0('Input CONSAM file name:',ADCFIL,30,.false.)
	   goto 31
	endif
c
c and print details of patch
	ptype(1)='outside-out'
	ptype(2)='inside-out'
	ptype(3)='cell-attached'
	ptype(4)='whole-cell'
	ptype(5)='simulated data'
	print 28,expdate,npatch,tapeID,ptype(ipatch),Emem,temper
      if(discprt) write(8,28)
     & expdate,npatch,tapeID,ptype(ipatch),Emem,temper

28	format(/,
     & ' Date of experiment: ',a11,' (patch number ',i3,')'/,
     & ' Tape details: ',a24,/,
     & ' Patch type: ',a14,/,
     & ' Membrane potential (mV) = ',f9.2,/,
     & ' Temperature = ',f9.1)
c
	if(pon()) write(7,229) adcfil
	if(discprt) write(8,229) adcfil
229   format(/,' CONSAM file is: ',a30,/)
c
200	continue
c For C-jump data, jump to here after READJUMP
c	if(debug()) then
	   ialt=0
	   print 351,pAV
351	   format(' Gain (pA/V) = ',g13.6,' O.K. [Y] ')
	   ans='Y'
	   call INPUTa(ans)
	   if(UC(ans).eq.'N') then
		ialt=1
		print 352
352		format('&Gain (pA/V) = ')
		call INPUTr(pAV)
		if(axon) then
		   calfac=pAV/faccal
		else
		   calfac=pAV/6553.6
		endif
	   endif
	   print 11,ffilt
11	   format(' Low pass filter (kHz) = ',g13.6,' O.K. [Y] ')
	   ans='Y'
	   call INPUTa(ans)
	   if(UC(ans).eq.'N') then
		ialt=1
		print 12,ffilt
12		format(' Low pass filter (kHz) [',f8.3,'] = ')
		call INPUTr(ffilt)
	   endif
c        ===fix correction of disk for c-jump case and for axon case
	   if((ialt.eq.1).and.(.not.cjump).and.(.not.axon)) then
		print 353
353		format('&Write correction(s) to disc [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		if(ans.eq.'Y') then
		   f=1000.*ffilt		!kept as Hz
	         if(iver.eq.1002) then	!already have expdate etc
		    write(14,rec=1)iver,title1,cdate,adctime,idt,ioff,ilen,
     &	    inchan,id1,id2,cs(1:3),calfac,srate,f,ffilt1,calfac1
		   else
		     write(14,rec=1) title,cdate,adctime,idt,ioff,ilen,
     &	      inchan,id1,id2,cs(1:3),calfac,srate,f,ffilt1,
     &	      calfac1,iver
		   endif
		 endif
	   endif
c	endif 	!end of debug
	if(cjump) then
	   nchan=1
	else
	   if(inchan.lt.1) inchan=1
	   nchan=int4(inchan)	!for use below
	endif

	if(nchan.gt.1) then
	   call BELL(3)
	   print 541
541	   format(' SCAN not yet fixed for data from more than 1 channel')
	   STOP
	endif
54	continue		!jump here for PDP data
	if(.not.cjump) then
	   nbyte=ilen		!NB ilen is integer*4
	   nint=nbyte/2	!total number of int*2 values in record
	   ndat=nint/nchan	!number of values per channel
	else
	   ndat=nsamp
	   nint=ndat
	   nbyte=2*ndat
	endif
c
c NB in new version have iver=1002, and CS(1:2) contain clock spec eg 'C ' or
c 'HT' (cs(3:3) is redundant)
c
c	if(cs(2:2).eq.'C') then
c	   clock=1.0d6      !clock rate
c	else if(cs(2:2).eq.'H') then
c	   clock=4.0d6      !clock rate
c	endif
c	div=dble(float(id1)*float(id2))
c	rate=clock/div	!sample rate
cc
c	print 36,idt,ioff,ilen,ilen/(2*inchan),inchan,id1,id2,rate,
c     & cs(2:2),cs(3:3)
c36	format(' Data type (0=cont; 1=intermittent) = ',i8,/,
c     & ' Offset (bytes) to start of data = ',i8,/,
c     & ' Length (bytes)',i8,' = ',i8,' values on each channel',/,
c     & ' Number of channels = ',i8,/,
c     & ' Rate divisors = ',2i8,'  Sample rate (Hz)= ',g14.7,/,
c     & ' Clock rate (C=1 MHz, H=4MHz) = ',a1,/,
c     & ' Trigger control (T= sample triggered from ev4) = ',a1)
c
c Initialise.
c
c	if(disp) then
c	   ans1='y'
c	else
c	   ans1='n'
c	endif
	ans1='N'
	call DCASK('Display only (no step-response function)',ans1,ans)
	disp=ans.eq.'Y'
	filtf=ffilt	!default=value from CONSAM -set even for disp (trise=0 gives crash)
	trise=332.1/filtf		!rise time (microsec)
	if(.not.disp) then
	   filtf=ffilt	!default=value from CONSAM
	   if(nsetup.gt.0) nsetup=-nsetup	!so GETFILT checks value!
c	   nsetup=0		!so asks which one
	   fcdone=.true.	!unless set false in getfilt
	   call GETFILT(nfilt,dtf,filt,nsetup,filtf,filtfile,ftitle,
     &	.false.,fcdone)
	   ffilt=filtf		!in case wrong value from CONSAM has been corrected
c 'normalise' the step response.
	   call NORMFILT(nfilt,dtf,filt,dtf1,nfilt1,filt1,
     &  tf01,tf1,tf50,tf99,tf999,nsetup,filtf)
c Make table of amp/amp(max) vs length (microsec) , for A/Amax=.05,(.05),.95
	   trise=332.1/filtf		!rise time (microsec)
c	   print 228
c	   if(pon()) write(7,228)
c	   if(discprt) write(8,228) trise
c228	   format(' Filter rise time (microsec) = ',g13.6,/,/,
c     & 	8x,' A/Amax       length (microsec)    length/trise')
	   do i=1,49
		aam=float(i)*0.02	!a/amax
		wlo=0.0		!low guess for w
		whi=20.*trise
		epsx=0.1	!0.1 microsec accuracy
		ndisp=-2	!no printing
		call BISEC0(AFUNC,wlo,whi,aam,wout,Yout,EPSx,epsy,
     &	 Nerr,Ndisp,.false.)
		timamax(i)=wout
c		print 23,timamax(i),aam
c		if(pon()) write(7,23) timamax(i),aam
c		if(discprt) write(8,23) i,aam,timamax(i),timamax(i)/trise
23		format(i4,3x,3(g13.6,4x))
	   enddo
	endif
c
c (Next bit from DATIN in old PDP program)
	ibflag=0	!TO SIGNAL IBOFF CHANGE VIA NEWPAR
c
C FINTER=NO OF MICROSEC BETWEEN DATA POINTS
	finter=1.e6/srate
c Need double prec versions of above if time of transition from 1st point
c in CONSAM to be recorded accurately (see FITSUB).  At present, cannot
c have smaller finter than 0.25 microsec (with 4 MHz clock) so want to get
c rid of non sig figs when DBLE(finter) is calc
c Could also get exact double-prec value as id1*id2/4.d0 (microsec)
	dtick=dble(4.0*finter)
	idtick=ifixd(dtick)
	dfinter=dfloat(idtick)/4.d0	!exact double prec value
	dfinter=DROUND(dfinter,4)	!round to 4 figs after dec point
c Define npfilt1= number of data points for filter to go from 1% to 99%
c and npfilt=number of points to jump forward after a transition, to start
c search for next transition
	npfilt1=ifixr((tf99-tf1)/finter)
	npfilt=ifixr(float(npfilt1)*facjump)
c==	nshutfit=8*npfilt		!default value
c change def of nshutfit? or use fixed value?
c
c Move definition of calibration factors up to here, because calfac2 needed
c to convert avampz to pA
	sfac1=(yd2-yd1)/65536.
	sfac1=sfac1*dgain			!true scal fac for ADC to pixel units
	nscale=1 + ifix(alog(4096./(yd2-yd1))/alog(2.))
	sfac2=sfac1*float(2**nscale)	!Converts ADC units to intermed units
	sfac3=1.0/float(2**nscale) 	!converts intermed to pixel units
	calfac1=calfac/sfac1		!converts pixel display units to pA
	calfac2=calfac/sfac2		!converts intermed units to pA
c
	if(iscrit.le.0.or.iscrit.gt.3) iscrit=1
c
310	continue	!jump here for newpar
c	basoff=float(iboff)*calfac/abs(dgain)	!baseline offset in pA
C SHOW DEFAULT VALUES
	names(1)='Invert trace (openings must be downwards) ?'
c==	names(2)='Openings are downwards in the data'
	names(2)='Auto-fit to avoid sublevels if possible'
	if(iscrit.eq.1) then
	   names(3)='Percent of full amp for critical level'
	else if(iscrit.eq.2) then
	   names(3)='Multiple of RMS for critical level   '
	endif
	names(4)='Number of points past critical level'
	names(5)='Number of shut points to be fitted at ends'
	names(6)='Min number of shut points for baseline'		!nbasemin
	names(7)='Initial display expansion factor'
	names(8)='Auto-write to disc every n''th transition'
	names(9)='Display gain factor'
	names(10)='Display baseline (percent of Y axis)'
	names(11)='Trigger position (percent of X axis)'
	names(12)='Fraction of filter length (1-99%) to jump after trans'
	names(13)='Minimum interval (microsec)               '	!tmin
	names(14)='''Short'' interval (multiple of risetime)'		!tshort
	names(15)='''Long'' interval (multiple of risetime)'		!tlong
	names(16)='''Close'' transitions (multiple of risetime)'	!tclose
	names(17)='Fraction of max amp for ''real'' difference'	!ampfac
	names(18)='Error factor for fitting'				!errfac
	names(19)='Sensitivity for multiple trans (vs sublevel)'	!derivfac				!
	names(20)='Default zoom factor                     '
	names(21)='Filter cut-off frequency when zoomed (Hz)'
	names(22)='''Full amplitude'' for zoomed display   '
	names(23)='Minimisation method (see help).            '
	names(24)='No of SIMPLEX iterations before DFPMIN used'
	names(25)='Simplex contraction factor'
	names(26)='Number of data points per section'
	names(27)='Number of data points in overlaps'
	names(28)='Mark specified levels on screen ?'
	names(29)='Mark transition points on display ?'
	names(30)='Show 1st derivative on display when used ?'
	names(31)='Definition of critical level (see HELP)'
	names(32)='Use variable critical level (see HELP) ?'
	names(33)='Minimum critical level (see HELP): multiple of RMS'
	names(34)='Initial step size factor for Simplex fitting'
	if(newpar) then
	   names(1)='Cannot invert trace except at start'
	   names(26)='Cannot set number of points/section except at start'
	   names(27)='Cannot set size of overlap except at start'
	endif
c
	if(.not.readini) then	!defaults if not read form .ini
	   fcz=ffilt
	   izoom=1
	   ampz=5.0
	endif
c Set values for paramwin as ASCII
	do i=1,3
	   values(i)='N'
	enddo
	if(invert) values(1)='Y'
c==	if(opendown) values(2)='Y'
	if(iautosub.eq.1) values(2)='Y'
	if(iscrit.eq.1) then
	   call DCFORMAT(100.*smult,6,1,values(3))
	else if(iscrit.eq.2) then
	   call DCFORMAT(smult,6,1,values(3))
	endif
      call INTCONV(ntrig,values(4))
      call INTCONV(nshutfit,values(5))
      call INTCONV(nbasemin,values(6))
	call DCFORMAT(expfac,6,2,values(7))
      call INTCONV(nwrit,values(8))
	call DCFORMAT(dgain,6,2,values(9))
	call DCFORMAT(100.*bdisp,7,2,values(10))
	call DCFORMAT(100.*xtrig,7,2,values(11))
	call DCFORMAT(facjump,7,2,values(12))
	call DCFORMAT(tmin,7,2,values(13))
	call DCFORMAT(tsfac,6,2,values(14))
	call DCFORMAT(tlfac,6,2,values(15))
	call DCFORMAT(tcfac,6,2,values(16))
	call DCFORMAT(ampfac,6,2,values(17))
	call DCFORMAT(errfac,7,4,values(18))
	call DCFORMAT(derivfac,6,2,values(19))
      call INTCONV(izoom,values(20))
	call DCFORMAT(1000.*fcz,7,1,values(21))            	!Hz
	call DCFORMAT(ampz,6,2,values(22)) 		      !pA
      call INTCONV(minmeth,values(23))
c//	if(usepots) values(23)='Y'
      call INTCONV(itsimp,values(24))
	call DCFORMAT(confac,6,2,values(25))
      call INTCONV(nbuf,values(26))
      call INTCONV(novlap,values(27))
	if(nampmark.gt.0) then
	   values(28)='Y'
	else
	   values(28)='N'
	endif
	values(29)='N'
	if(disptran) values(29)='Y'
	values(30)='N'
	if(dispderiv) values(30)='Y'
      call INTCONV(iscrit,values(31))
	values(32)='N'
	if(scritvar) values(32)='Y'
	call DCFORMAT(smultmin,6,1,values(33))	!display as mult of rms1
	call DCFORMAT(stpfac,6,3,values(34))	!for simplex
c
	usepots=.false.	!not available!!
c
c Define help strings for paramwin
c examples of length of strings to prevent word breaks in help box
c	helpstr(1)='                                         '
c     &	   //'                                         '
c	helpstr(10)='                                        '
c     &	    //'                                        '
	helpstr(1)='Trace inverted for display on screen.'
c	helpstr(2)='Openings appear as downward deflections  '
c     &	   //' on the screen. MUST BE TRUE AT PRESENT! '
	helpstr(2)='In case of doubt fit brief open-shut-open'
     & 	   //' rather than fitting a sublevel.'
	if(iscrit.eq.1) then
	   helpstr(3)='Percentage of full amplitude for critical'
     & 	      //' level (Scrit) beyond which transition is'
     &	      //' deemed to occur.'
	else if(iscrit.eq.2) then
	    helpstr(3)='Multiple of RMS noise to define critical '
     & 	       //' level (Scrit) beyond which transition is'
     &	       //' deemed to occur.'
	endif
	helpstr(4)='Number of consecutive points beyond Scrit'
     & 	   //' for a transition to be deemed to have'
     &	   //' occurred.'
	helpstr(5)='Number of points before first, and after '
     & 	   //' last, transition to be fitted in auto '
     &	   //' mode.'
	helpstr(6)='Minimum no. of points at the shut level  '
     & 	    //'within a fit required for this shut level'
     & 	    //'to be used to estimate the amplitude of  '
     & 	    //'nearby openings.                         '
	helpstr(7)='Factor by which trace is expanded when   '
     & 	    //' ''expand'' is first hit.'
	helpstr(8)='Data will be written to disk at (approx) '
     & 	    //' every nth transition, so analysis can be'
     & 	    //' restarted by using the ''restart'''
     &	    //' option when SCAN reentered.'
	helpstr(9)='Trace will be amplified by this factor   '
     & 	    //' before display (but better to amplify  '
     &	    //' correctly BEFORE sampling).'
	helpstr(10)='Position of baseline on screen is offset '
     & 	    //' to this level after initial ''get piece '
     &	    //' of baseline on screen'' is completed.'
	helpstr(11)='When opening crosses the red trigger line'
     &	    //'display stops with the opening transition'
     & 	    //' at this point on the x-axis of display.'
	helpstr(12)='Fraction of step-response length (1-99%) '
     & 	    //' allowed after a transition before search'
     & 	    //' for next transition is started.'
	helpstr(13)='Length of fitted event (microsec) below  '
     & 	    //' which refit, omitting short events, is  '
     & 	    //' offered.  Events guessed to be shorter  '
     & 	    //' than this are rejected before fitting.'	!see SHORTEV
	helpstr(14)='Length (multiple of risetime) of event   '
     & 	    //' below which its amplitude is fixed (also'
     & 	    //' length guessed from peak amplitude.'
	helpstr(15)='Length (multiple of risetime) of event   '
     & 	    //'above which amplitude is ''well-defined'''
     & 	    //' so usable to fix length of an adjacent'
     & 	    //' brief opening.'
	helpstr(16)='Length (multiple of risetime) of interval'
     & 	    //'between two transitions (in same         '
     & 	    //'direction) below which an attempt is made'
     & 	    //'to fit brief events rather than sublevel.'
	helpstr(17)='Amplitude difference (as fraction of full'
     & 	    //' amp) below which openings are deemed to '
     & 	    //' have ''same'' amplitude: for (a) elim of'
     & 	    //'  short gaps (b) setting guesses.'
	helpstr(18)='Factor by which initial guess must be    '
     & 	    //' reduced before Simplex converges; (e.g. '
     & 	    //'  0.01=low precision; 0.0001=high        '
     & 	    //'   precision)                            '
	helpstr(19)='Multiple of SD of 1st deriv used to find '
     & 	    //' inflections; small value e.g. 2.0 makes '
     & 	    //' it more likely that multiple transitions'
     & 	    //' fitted, rather than sublevel.'
	helpstr(20)='Default zoom factor (must be an integer  '
     & 	   //' power of 2 (i.e. 1,2,4,8,...etc.)'
	helpstr(21)='Filter cut-off (Hz, -3 dB) when zoomed   '
     & 	   //' (default = same as input data, i.e. no  '
     &  	   //' extra filtering when zoomed)   '
	helpstr(22)='''Full amplitude'' to be used to identify'
     & 	   //' transitions when zoomed (approx size of  '
     & 	   //' small channels).'
c	helpstr(23)='Use potentiometers, as well as keys, to  '
c     &	   //' control the graphical display.          '
	helpstr(23)='1=SIMPLEX only (as before).              '
     &	    //'2=SIMPLEX for fixed number of iterations '
     &	    //'  (see next line), then swap to DFPMIN.  '
     &	    //'3=Davidon-Fletcher-Powell (DFPMIN) only. '
	helpstr(24)='No. of iterations done by SIMPLEX before '
     &	   //' swapping to DFPMIN (Davidon-Fletcher-    '
     &	    //' Powell minimisation) when both used.'
	helpstr(25)='Controls how fast the simplex contracts  '
     & 	    //' around a putative minimum.  Usually 0.5;'
     & 	    //' smaller value (down to 0.2) gives faster'
     &	    //' convergence but fit may be worse'
	helpstr(26)='Number of data points read into memory at'
     &	    //' each disk read -bigger the better (max  '
     & 	    //' depends on how much RAM you have).'
	helpstr(27)='Number of extra points read in, at each  '
     & 	    //' end of data section to allow display of '
     & 	    //' transitions on section boundaries; 2048 '
     &	    //' is OK usually.'
	helpstr(28)='Show horizontal lines to mark specified  '
     & 	    //' amplitude levels.                       '
	helpstr(29)='Show position of guessed transition      '
     & 	   //' points on screen as purple line + blue  '
     &  	   //' line to mark end of transition.'
	helpstr(30)='When first derivative used to identify   '
     & 	   //' two closely-spaced transitions, display '
     & 	   //' it below the trace.'
	helpstr(31)='Definition of threshold for detecting    '
     & 	   //' transitions: 1=fraction of mean full amp'
     & 	   //' 2=multiple of rms noise. (Level is set  '
     & 	   //' as parameter 3, page 1).'
	helpstr(32)='Use reduced critical amplitude          '
     & 	   //' for detecting transitions when          '
     & 	   //' all amplitudes in the fit are smaller   '
     &	   //' than the full amplitude    '
	helpstr(33)='Minimum crit level when using lower     '
     & 	   //' threshold for detecting transitions when'
     & 	   //' all amplitudes in the fit are smaller   '
     &	   //' than the full amplitude (as mult of RMS)'
	helpstr(34)='Step factor for Simplex fitting. Smaller'
     & 	   //' values gives smaller initial step size, '
     & 	   //' so less chance of wandering too far from'
     &	   //' initial guess, but may be slower.       '
c Call paramwin
	nvals=34		!number of rows in names(), values()
	do i=1,nvals
	   jfix(i)=0
	enddo
	if(newpar) then
	   jfix(1)=1	!can't change 'invert'
	   jfix(26)=1	!can't change nbuf
	   jfix(27)=1	!can't change novlap
	endif
	call PARAMWIN(nvals,names,values,helpstr,'Parameter values',
     &	ialter,nalter,1,jfix)
c Now redefine values for any altered parameter
	if(nalter.gt.0) then
	   do i=1,nalter
		SELECT CASE(ialter(i))
		case(1)
		   if(.not.newpar) then
			invert=values(1).eq.'Y'
		   endif
c		case(2)
c		   opendown=values(2).eq.'Y'
		case(2)
		   iautosub=2
		   if(UC(values(2)).eq.'Y') iautosub=1
		case(3)
		   call CHTOREAL(values(3),x)
		   if(iscrit.eq.1) then
		      smult=0.01*x		!set whether ampdone or not
		      if(ampdone) scrit=smult*abs(avamp)	!in intermed units
		   else if(iscrit.eq.2) then
		      smult=x		!set whether ampdone or not
		      if(ampdone) scrit=smult*rms1		!in intermed units
		   endif
		case(4)
		   call CHTOREAL(values(4),x)
		   ntrig=IFIXR(x)
		case(5)
		   call CHTOREAL(values(5),x)
		   nshutfit=IFIXR(x)
		case(6)
		   call CHTOREAL(values(6),x)
		   nbasemin=IFIXR(x)
		case(7)
		   call CHTOREAL(values(7),expfac)
		case(8)
		   call CHTOREAL(values(8),x)
		   nwrit=IFIXR(x)
		case(9)
		   call CHTOREAL(values(9),dgain)
		case(10)
		   call CHTOREAL(values(10),bdisp)
		   bdisp=0.01*bdisp
		   if(newpar) ibflag=1	!set so baseline altered later
		case(11)
		   call CHTOREAL(values(11),xtrig)
		   xtrig=0.01*xtrig
		case(12)
		   call CHTOREAL(values(12),facjump)
		case(13)
		   call CHTOREAL(values(13),tmin)
		case(14)
		   call CHTOREAL(values(14),tsfac)
		case(15)
		   call CHTOREAL(values(15),tlfac)
		case(16)
		   call CHTOREAL(values(16),tcfac)
		case(17)
		   call CHTOREAL(values(17),ampfac)
		case(18)
		   call CHTOREAL(values(18),errfac)
		case(19)
		   call CHTOREAL(values(19),derivfac)
		case(20)
		   call CHTOREAL(values(20),x)
		   izoom=IFIXR(x)
		case(21)
		   call CHTOREAL(values(21),fcz)
		   fcz=0.001*fcz		!in kHz
		case(22)
		   call CHTOREAL(values(22),ampz)
		case(23)
		   call CHTOREAL(values(23),x)
		   minmeth=IFIXR(x)
		case(24)
c		   usepots=values(23).eq.'Y'
		   call CHTOREAL(values(24),x)
		   itsimp=IFIXR(x)
		case(25)
		   call CHTOREAL(values(25),confac)
		case(26)
		   if(.not.newpar) then
			call CHTOREAL(values(26),x)
			nbuf=IFIXR(x)
		   endif
		case(27)
		   if(.not.newpar) then
			call CHTOREAL(values(27),x)
			novlap=IFIXR(x)
		   endif
		case(28)
		   markamp=values(28).eq.'Y'
		case(29)
		   disptran=values(29).eq.'Y'
		case(30)
		   dispderiv=values(30).eq.'Y'
		case(31)
		   call CHTOREAL(values(31),x)
		   iscrit=IFIXR(x)
		   if(iscrit.lt.1.or.iscrit.gt.2) iscrit=1
		case(32)
		   scritvar=values(32).eq.'Y'
		case(33)
		   call CHTOREAL(values(33),x)
		   smultmin=x	!*rms1=in intermed units
		case(34)
		   call CHTOREAL(values(34),x)
		   stpfac=x
		END SELECT
	   enddo
	endif
c
	avampz=ampz/calfac2		!convert to intermed units
	filtzoom=ffilt-fcz.gt.0.001
c Calculate actual fczoom if filtering to be done
	if(filtzoom) then
	   fczoom=1./sqrt((1.0/(fcz*fcz)) - (1.0/(ffilt*ffilt)))
	   print 45,1000.*ffilt,1000.*fczoom,1000.*fcz
         if(pon()) write(7,45) 1000.*ffilt,1000.*fczoom,1000.*fcz
         if(discprt) write(8,45) 1000.*ffilt,1000.*fczoom,1000.*fcz
45	   format(
     &  ' Data are filtered at ',g13.6,' Hz.',/,
     &  '  For zoom, filter at fc = ',g13.6,' Hz to give final fc = '
     &	,g13.6)
c Calculate interpolated step response for fc=fczoom in filt1z(1024)
c for use later (always Gaussian filter so nsetup=-1000), and number
c of points in it=nfilt1z (NB do not overwrite tf01 etc, which are
c reallocated later when fczoom is in use)
	   call NORMFILT(nfilt,dtf,filt,dtf1z,nfilt1z,filt1z,
     &    x,x,x,x,x,-1000,fczoom)
	endif
c
c For next but scrit must be defined whether smult altered or not
	if(newpar.and.ampdone.and.sdone) then
	 call CHTOREAL(values(3),x)
	 if(iscrit.eq.1) then
	    smult=0.01*x		!set whether ampdone or not
	    if(ampdone) scrit=smult*abs(avamp)	!in intermed units
	 else if(iscrit.eq.2) then
	    smult=x		!set whether ampdone or not
	    if(sdone) scrit=smult*rms1		!in intermed units
	 endif
	 scr1=scrit/rms1		!both in intermed units
	 if(scr1.lt.3.) then
	   call BELL(1)
	   print 24,scr1,avamp,calfac2*scrit,calfac2*rms1
24	   format(
     & ' WARNING: search threshold is only ',f7.2,' times rms noise',/,
     & ' ( full amp = ',f8.2,' pA; threshold = ',f7.2,' pA; rms = ',
     & f7.2,' pA',/,
     & ' O.K. [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(UC(ans).eq.'N') then
		x=100.*smult
		print 25,x
25		format(
     & ' New percentage of full amplitude for threshold [',f6.1,'] = ')
		call INPUTr(x)
		smult=0.01*x
	      iscrit=smult*abs(avamp)		!in intermed units
	   endif
	 endif
	endif
c
	if(navtest.le.0) navtest=1
      OPEN(unit=15,file='SCAN.INI',status='UNKNOWN',
     &  access='DIRECT',form='UNFORMATTED',recl=512)
c	call DCASK('Save default values in SCAN.INI','y',ans)
c Make default to NOT save param
	savin='N'
	call DCASK('Save default values in SCAN.INI',savin,ans)
c=========need more default values for cjump case in .ini?
	savin=ans
	if(ans.eq.'Y') then
	   write(15,rec=1) savin,nbuf,novlap,opendown,invert,smult,ntrig,
     &   dgain,iboff,nwrit,iexp,bdisp,isub,xtrig,usepots,ndev,disp,
     &   nsetup,filtfile,navtest,iautosub,disptran,dispderiv,dispguess,
     &   tsfac,tlfac,tcfac,facjump,ampfac,errfac,derivfac,confac,
     &   adcfil,nampmark,iamark,tmin,cjump,
     &   izoom,fcz,ampz,itsimp,minmeth,nbasemin,iscrit,
     &   scritvar,smultmin,stpfac
	else
	   write(15,rec=1) savin	!still keep savin!
	endif
	CLOSE(unit=15)
c Print all the parameter values
      if(discprt) then
	   write(8,21)
21	   format(/,' Parameter settings',/,' -----------------------')
	   names(26)='Number of data points per section'	!restore name
	   names(27)='Number of data points in overlaps'
	   do i=3,12
		write(8,20) names(i),values(i)
20		format(1x,a60,2x,a11)
	   enddo
	   write(8,22) npfilt
22	   format(4x,'(',i2,' data points)')
	   do i=13,27
		write(8,20) names(i),values(i)
	   enddo
	   do i=33,34
		write(8,20) names(i),values(i)
	   enddo
	endif

c calc nsec etc here, in case default nbuf altered
	if(ndat.lt.nbuf) nbuf=ndat    !allocate smaller array
	nsec= 1 + (ndat-1)/nbuf  !number of sections
	nrlast=ndat - (nsec-1)*nbuf  !number of idata in last section
C TSECT=time (microsec) from first point of one section to first point of next
	tsect=float(nbuf)*finter
	npfilt1=ifixr((tf99-tf1)/finter)
	npfilt=ifixr(float(npfilt1)*facjump)	!in case facjump altered
c
c=	if(invert) dgain=-abs(dgain)
c	if(newpar) goto 2741
c=	if(invert) opendown=.not.opendown	!SO opendown=DIRECTION ON DISPLAY
c
	if(newpar) goto 999	!reset cal factors and exit
C
      print 3001,nsec
      if(pon()) write(7,3001) nsec
      if(discprt) write(8,3001) nsec
3001	format(' Number of sections = ',i8)
C NSECT=TOTAL NO OF SECTIONS TO BE ANALYSED. INITIALLY SET TO ZERO
c
802	continue
c
c
C PREPARE TO READ FIRST SECTION OF NBUF POINTS
c==	NBREAD=NBS	!NO OF BLOCKS TO BE READ EACH TIME (EX LAST)
c NB set sdone=false BEFORE inscan called, so know whether sdone=true or
c not when INSCAN called for new parameters (newpar=true)
c	sdone=.false.	!no baseline SD yet -NO -set BEFORE INSCAN
	idest=0
	if(.not.restart) idest=2151  !to indicate 1st read in DATGET
c
999	continue
c Set scale factors before returning
	sfac1=(yd2-yd1)/65536.
	sfac1=sfac1*dgain			!true scal fac for ADC to pixel units
	nscale=1 + ifix(alog(4096./(yd2-yd1))/alog(2.))
	sfac2=sfac1*float(2**nscale)	!Converts ADC units to intermed units
	sfac3=1.0/float(2**nscale) 	!converts intermed to pixel units
	calfac1=calfac/sfac1		!converts pixel display units to pA
	calfac2=calfac/sfac2		!converts intermed units to pA
c==	iyoff=ifixr(yd1 + bdisp*(yd2-yd1))	!baseline position (in Y pixels)
	if(.not.newpar) then
	   iyoff=ifixr(yd1 + 0.5*(yd2-yd1))	!zero in centre until baseline done
	endif
c (NB iyoff is in pixel units)
c
c	nampmark=0
	if(markamp) then
	   print 26,nampmark
26	   format(
     & ' Number of amplitude markers to be drawn on screen [',i2,'] = ')
	   call INPUTi(nampmark)
	   if(nampmark.gt.0) then
		do i=1,nampmark
		   x=calfac1*float(iamark(i))
		   print 27,i,x
27		   format('&Amplitude #',i2,' (pA) [',f7.2,'] = ')
		   call INPUTr(x)
		   iamark(i)=ifixr(abs(x)/calfac1)	!x pA
		   if(opendown) iamark(i)=-iamark(i)
		enddo
	   endif
	endif
c
	ffilt=filtf		!in case wrong value from CONSAM has been corrected
c
	RETURN
c278	idest=278
c	RETURN
c259	idest=259
	END




