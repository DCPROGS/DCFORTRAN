	subroutine INSCAN(iampl,tint,iprops,ntmax,nbuf,novlap,srate,
     & finter,tsect,ioff,ndat,nsec,nrlast,savin,nshutfit,
     & kt,nwrit,nwsav,newpar,igback,restart,opendown,invert,
     & usepots,disp,smult,scrit,vary,ntrig,navtest,dgain,iboff,
     & expfac,bdisp,ibflag,iautosub,xtrig,ndev,cdate,adctime,
     & nsetup,filtfile,filt1,nfilt1,dtf1,timamax,avamp,ampdone,
     & tf01,tf1,tf50,tf99,tf999,npfilt,ffilt,
     & yd1,yd2,sfac1,sfac2,sfac3,nscale,calfac,calfac1,calfac2,iyoff,
     & dfinter,disptran,dispderiv,dispguess,ampfac,tmin,
     & tsfac,tlfac,tcfac,sdone,iscanver,facjump,errfac,confac,
     & derivfac,idest,
     & cjump,njump,ktjump,nfjump,tzero,intzero,tsample,istrec,
     & njdim,jumplen)
c
c SOME PARAM AND ALL OF COMMON ARE FOR READ OF RESTART VALUES
c Modif 01/06/95 05:44pm to add CJUMP options
c Modif 03/24/94 04:56pm for new definition of SMULT (see SCAN)
c (avamp and ampdone added to parameters)
	character*1 ans,UC,ans1,savin
	real*8 dfinter,dround,dtick,tsample
	logical invert,opendown,newpar,sdone,restart,readini
	logical disp,pdpdata,present,usepots,ampdone
	logical disptran,dispderiv,dispguess,markamp,cjump
	logical discprt
	logical pon,slock,debug,caplock
	integer*2 istrec(250)		!orig=500 bytes (kept as int*2 to fit in 1 rec)
c===new params: filt1?,nsetup,ndisc,icol1 etc,
c===params drom PDP DATIN:
c===CHANGES: initial expansion factor (iexp) now real*4 expfac
c declarations from CONSAM
	character ADCFIL*30,cdate*11,cdate1*11,ndev*2,ndevdat*2
	integer*2 idt,inchan,id1,id2,iver
	character cs*3,adctime*8
	character*76 title
c	character dosdate*12,dostime*12
c
	real*4 tint(ntmax)
	integer*2 iampl(0:ntmax)
	integer*1 iprops(ntmax)	!properties of ith interval (see SCAN)
	real*4 timamax(49)
c==	integer ksect(1024),ishut(1024)
c For PARAMWIN
	include '\f77l3\lib\spinatt.hdr'
	character*60 names(27)
	character*11 values(27)
	character*165 helpstr(27)
	integer*2 ialter(27)
c For GETFILT
	real*4 filt(100),filt1(1024)
	character ftitle*80,filtfile*20
	character*32 filnam
	character*74 mtitle
	common/dp/discprt
	common/dpp/filnam,machine,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	common/rstrtblk/nchan,pdpdata,adcfil,cdate1
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
c
c INITIALISATIONS
	idest=0
c	nbuf=1048576 !1024*1024=dimension of array containing current data section
c	nbuf=131072		!=1024*128
	nbuf=65536		!=1024*64
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
c	SMULT=4.0
	smult=0.12
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
	nshutfit=40
	facjump=0.6
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
c
c INSCAN is skipped if Restart requested
	restart=.false.
	readini=.false.
c	if(.not.restart) then         !read .INI
	iopt=2
	INQUIRE(file='scan.ini',exist=present,flen=ilen)
	if(present.and.len.gt.0) then
	   print 10,iopt
10	   format(
     & ' (1) Use program default values for all parameters',/,
     & ' (2) Read parameter values from SCAN.INI',/,
     & ' Option [',i2,'] = ')
	   call INPUTi(iopt)
	else
	   iopt=1
	endif
	if(iopt.eq.2) then
	   readini=.true.
         OPEN(unit=15,file='SCAN.INI',status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=256)
c     &	access='TRANSPARENT')
c	   read(15,rec=1) nbuf,novlap,opendown,invert,smult,ntrig,
	   read(15,rec=1) savin,nbuf,novlap,opendown,invert,smult,ntrig,
     &   dgain,iboff,nwrit,iexp,bdisp,isub,xtrig,usepots,ndevdat,disp,
     &   nsetup,filtfile,navtest,iautosub,disptran,dispderiv,dispguess,
     &   tsfac,tlfac,tcfac,facjump,ampfac,errfac,derivfac,confac,
     &   adcfil,nampmark,iamark,tmin,cjump
c=========need more default values for cjump case in .ini?
c CJUMP etc added 01/06/95 05:44pm
	   CLOSE(unit=15)
	   if(smult.ge.1.0) smult=0.12      !set default
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
	   call READJUMP(njump,nsamp,ioff,ndisc,adcfil,calfac,
     &    srate,ffilt,ktjump,nfjump,njdim,tzero,intzero,
     &    tsample,readini,istrec,jumplen,idest,0)
	   if(idest.eq.999) STOP	!abort DISCNUM
	   pAV=calfac*6553.6		!pA/volt (display now scaled in volts)
	   ndat=nsamp
	   goto 200
	endif
c
c Next the stuff for CONSAM files
	print 51
51	format(' CONSAM taken from PDP11 [N] ? ')
	read 101,ans
      pdpdata=UC(ans).eq.'Y'
	if(pdpdata) then
	   if(ndisc.gt.1) then
	      print 55
55	      format(' Specify disc partition for CONSAM file:')
	      call DISCNUM1(idisc,ndev,0,0)	!ask for winchester (only)- no default
	   else
		ndev='C:'
	   endif
c	   if(idisc.eq.-1) goto 999
	   if(idisc.eq.-1) STOP
	   adcfil=ndev//'\CONSAM.DAT'
	   call TITENT0('Input CONSAM file name:',ADCFIL,30,.false.)
	   INQUIRE(file=ADCFIL,exist=present,flen=ilen)
	   if(.not.present.or.ilen.eq.0) goto 52
	   call GETCAL(calfac,amVpA,amVpA1,VpA,VpA1,pAV,pAV1,
     &    itape,ftape,gain,errfac1)
c	   print 530
c530	   format(' Gain (pA per volt in computer) = ')
c	   call INPUTr(pAV)
c	   calfac=pAV/6553.6	!=[pA/V]/[ADC/V] = pA per ADC unit
	   calfac=calfac*6553.6/409.6	!correct for 502 interface (-2048 to 2047)
	   print 531
531	   format('&Sample rate (Hz) = ')
	   call INPUTr(srate)
	   print 53,ndev,ilen,calfac,srate
	   if(discprt) write(8,53) ndev,ilen,calfac,srate
53	   format(' PDP11 file CONSAM.DAT on ',a2,' has ',i8,' bytes',/,
     &   ' gain (pA per ADC unit) = ',g13.6,
     &	':   sample rate (Hz) = ',g13.6)
	   nchan=1
	   ioff=0
         OPEN(unit=14,file=ADCFIL,status='UNKNOWN',access='TRANSPARENT')
	   goto 54
	endif
	print 20
20	format(' Read names of existing CONSAM files [N] ? ')
	read 101,ans
      if(UC(ans).eq.'Y') then
	  n=ndisc		!from winprint.ini
	  do i=1,n
	    ndev=char(i+66)//':'
	    ADCFIL=ndev//'\CONSAM.DAT'
	    INQUIRE(file=ADCFIL,exist=present,flen=len)
	    if(present.and.len.gt.0) then
            OPEN(unit=14,file=ADCFIL,status='UNKNOWN',
     &	  access='TRANSPARENT')
	      read(14,rec=1) title,cdate1,adctime	!don't overwrite today's CDATE
	      CLOSE(unit=14)
	      print 111,ndev,cdate1,adctime,title
111	      format(' Disk ',a2,' Date= ',a11,' time= ',a8,/,1x,a76,/)
	    endif
	  enddo
c	  pause
	endif
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
	 ADCFIL=ndev//'\CONSAM.DAT'
	endif
	call TITENT0('Input CONSAM file name:',ADCFIL,30,.false.)
c
c Open data file, and read header
31	INQUIRE(file=ADCFIL,exist=present,flen=nlen)
	if(present.and.nlen.gt.0) then
        OPEN(unit=14,file=ADCFIL,status='UNKNOWN',access='TRANSPARENT')
	  read(14,rec=1) title,cdate,adctime,idt,ioff,ilen,inchan,
     &   id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,iver
c     & id1,id2,cs(1:3),calfac,srate,filt,calfac1,iver
c	CLOSE(unit=14)			!leave open throughout
	  pAV=calfac*6553.6			!pA/volt (display now scaled in volts)
        print 35, ndev,cdate,adctime,ffilt,title,srate,ilen
        if(pon()) write(7,35) ndev,cdate,adctime,ffilt,title,srate,ilen
        if(discprt)write(8,35) ndev,cdate,adctime,ffilt,title,srate,ilen
35	  format(' Disk ',a2,' Date= ',a11,' time= ',a8,
     & ' filter (Hz) = ',f9.1,/,1x,a76,/,
     & ' sample rate (Hz) = ',f10.1,'  length (bytes) = ',i10,/)
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
	if(pon()) write(7,229) adcfil
	if(discprt) write(8,229) adcfil
229   format(/,' CONSAM file is: ',a30,/)
c
200	continue
c For C-jump data, jump to here after READJUMP
	if(debug()) then
	   print 351,pAV
351	   format(' Gain (pA/V) = ',g13.6,' O.K. [Y] ')
	   read 101,ans
	   if(UC(ans).eq.'N') then
		print 352
352		format('&Gain (pA/V) = ')
		call INPUTr(pAV)
		calfac=pAV/6553.6
c        ===fix correction of disk for c-jump case
		if(.not.cjump) then
		  print 353
353		  format('&Write corrected gain to disc [Y] ? ')
		  read 101,ans
		  if(UC(ans).ne.'N') then
		   write(14,rec=1) title,cdate,adctime,idt,ioff,ilen,inchan,
     &	   id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,iver
		  endif
		endif
	   endif
	endif
	if(cjump) then
	   nchan=1
	else
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
c NB in new version have iver=1001, and CS(1:2) contain clock spec eg 'C ' or
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
	if(disp) then
	   ans1='y'
	else
	   ans1='n'
	endif
	call DCASK('Display only (no step-response function)',ans1,ans)
	disp=ans.eq.'Y'
	if(.not.disp) then
	   filtf=ffilt	!default=value from CONSAM
	   if(nsetup.gt.0) nsetup=-nsetup	!so GETFILT checks value!
c	   nsetup=0		!so asks which one
	   call GETFILT(nfilt,dtf,filt,nsetup,filtf,filtfile,ftitle,
     &	.false.)
c 'normalise' the step response.
	   call NORMFILT(nfilt,dtf,filt,dtf1,nfilt1,filt1,
     &  tf01,tf1,tf50,tf99,tf999)
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
	nshutfit=8*npfilt		!default value
c
c
310	continue	!jump here for newpar
c	basoff=float(iboff)*calfac/abs(dgain)	!baseline offset in pA
C TYPE DEFAULT VALUES
c========old numbering:
c	names(1)='Invert trace for display '
c	names(2)='Openings are downwards in the data'
cc=	names(3)='Use potentiometers to control display'
cc=	names(4)='Mark transition points on display'
cc=	names(5)='Show 1st derivative on display when used'
c	names(6)='Auto-fit to avoid sublevels if possible'
c	names(7)='Percent of full amp for critical level'
c	names(8)='Number of points past critical level'
c	names(9)='Number of shut points to be fitted'
c	names(10)='Initial display expansion factor'
c	names(11)='Auto-write to disc every n''th transition'
c	names(12)='Display gain factor'
c	names(13)='Display baseline (percent of Y axis)'
c	names(14)='Trigger position (percent of X axis)'
cc=	names(15)='Number of data points per section'
cc=	names(16)='Number of data points in overlaps'
c	names(17)='Fraction of filter length to jump after trans'
c	names(18)='Interval (musec) below which refit offered'	!tmin
c	names(19)='''Short'' interval (multiple of risetime)'		!tshort
c	names(20)='''Long'' interval (multiple of risetime)'		!tlong
c	names(21)='''Close'' transitions (multiple of risetime)'	!tclose
c	names(22)='Fraction of max amp for ''real'' difference'	!ampfac
c=======new numbering:
	names(1)='Invert trace for display '
	names(2)='Openings are downwards in the data'
	names(3)='Auto-fit to avoid sublevels if possible'
	names(4)='Percent of full amp for critical level'
	names(5)='Number of points past critical level'
	names(6)='Number of shut points to be fitted'
	names(7)='Initial display expansion factor'
	names(8)='Auto-write to disc every n''th transition'
	names(9)='Display gain factor'
	names(10)='Display baseline (percent of Y axis)'
	names(11)='Trigger position (percent of X axis)'
	names(12)='Fraction of filter length to jump after trans'
	names(13)='Interval (musec) below which refit offered'	!tmin
	names(14)='''Short'' interval (multiple of risetime)'		!tshort
	names(15)='''Long'' interval (multiple of risetime)'		!tlong
	names(16)='''Close'' transitions (multiple of risetime)'	!tclose
	names(17)='Fraction of max amp for ''real'' difference'	!ampfac
	names(18)='Error factor for fitting'				!errfac
	names(19)='Sensitivity for multiple trans (vs sublevel)'	!derivfac				!
	names(20)=' (not used)             '				!
c===any other new params? pref 2 more!
	names(21)='Mark transition points on display'
	names(22)='Show 1st derivative on display when used'
	names(23)='Use potentiometers to control display'
	names(24)='Number of data points per section'
	names(25)='Number of data points in overlaps'
	names(26)='Simplex contraction factor'
	names(27)='Mark specified levels on screen'
c
c Set values for paramwin as ASCII
	do i=1,3
	   values(i)='N'
	enddo
	if(invert) values(1)='Y'
	if(opendown) values(2)='Y'
	if(iautosub.eq.1) values(3)='Y'
	call DCFORMAT(100.*smult,6,1,values(4))
      call INTCONV(ntrig,values(5))
      call INTCONV(nshutfit,values(6))
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
	values(20)='-'	!until used
	values(21)='N'
	values(22)='N'
	values(23)='N'
	if(disptran) values(21)='Y'
	if(dispderiv) values(22)='Y'
	if(usepots) values(23)='Y'
      call INTCONV(nbuf,values(24))
      call INTCONV(novlap,values(25))
	call DCFORMAT(confac,6,2,values(26))
	if(nampmark.gt.0) then
	   values(27)='Y'
	else
	   values(27)='N'
	endif
c
c Define help strings for paramwin
c examples of length of strings to prevent word breaks in help box
c	helpstr(1)='                                         '
c     &	   //'                                         '
c	helpstr(10)='                                        '
c     &	    //'                                        '
	helpstr(1)='Trace inverted for display on screen.'
	helpstr(2)='Openings appear as downward deflections  '
     &	   //' on the screen.                          '
	helpstr(3)='In case of doubt fit brief open-shut-open'
     & 	   //' rather than fitting a sublevel.'
	helpstr(4)='Percentage of full amplitude for critical'
     & 	   //' level (Scrit) beyond which transition is'
     &	   //' deemed to occur.'
	helpstr(5)='Number of consecutive points beyond Scrit'
     & 	   //' for a transition to be deemed to have'
     &	   //' occurred.'
	helpstr(6)='Number of points before first, and after '
     & 	   //' last, transition to be fitted in auto '
     &	   //' mode.'
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
     & 	    //' which automatic refit, omitting short   '
     & 	    //' events, is offered.             '
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
	helpstr(20)='Not yet used                             '
	helpstr(21)='Show position of guessed transition      '
     & 	   //' points on screen as purple line + blue  '
     &  	   //' line to mark end of transition.'
	helpstr(22)='When first derivative used to identify   '
     & 	   //' two closely-spaced transitions, display '
     & 	   //' it below the trace.'
	helpstr(23)='Use potentiometers, as well as keys, to  '
     &	   //' control the graphical display.          '
	helpstr(24)='Number of data points read into memory at'
     &	    //' each disk read -bigger the better (max  '
     & 	    //' depends on how much RAM you have).'
	helpstr(25)='Number of extra points read in, at each  '
     & 	    //' end of data section to allow display of '
     & 	    //' transitions on section boundaries; 2048 '
     &	    //' is OK usually.'
	helpstr(26)='Controls how fast the simplex contracts  '
     & 	    //' around a putative minimum.  Usually 0.5;'
     & 	    //' smaller value (down to 0.2) gives faster'
     &	    //' convergence but fit may be worse'
	helpstr(27)='Show horizontal lines to mark specified  '
     & 	    //' amplitude levels.                       '
c Call paramwin
	nvals=27		!number of rows in names(), values()
	call PARAMWIN(nvals,names,values,helpstr,'Parameter values',
     &	ialter,nalter,1)
c Now redefine values for any altered parameter
	if(nalter.gt.0) then
	   do i=1,nalter
		SELECT CASE(ialter(i))
		case(1)
		   if(.not.newpar) invert=values(1).eq.'Y'
		case(2)
		   opendown=values(2).eq.'Y'
		case(3)
		   iautosub=2
		   if(UC(values(3)).eq.'Y') iautosub=1
		case(4)
		   call CHTOREAL(values(4),x)
		   smult=0.01*x		!set whether ampdone or not
		   if(ampdone) then		!i.e. vary is defined
		      scrit=smult*abs(avamp)	!in intermed units
		   endif
		case(5)
		   call CHTOREAL(values(5),x)
		   ntrig=IFIXR(x)
		case(6)
		   call CHTOREAL(values(6),x)
		   nshutfit=IFIXR(x)
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

		case(21)
		   disptran=values(21).eq.'Y'
		case(22)
		   dispderiv=values(22).eq.'Y'
		case(23)
		   usepots=values(23).eq.'Y'
		case(24)
		   call CHTOREAL(values(24),x)
		   nbuf=IFIXR(x)
		case(25)
		   call CHTOREAL(values(25),x)
		   novlap=IFIXR(x)
		case(26)
		   call CHTOREAL(values(26),confac)
		case(27)
		   markamp=values(27).eq.'Y'
		END SELECT
	   enddo
	endif
	if(newpar.and.ampdone.and.sdone) then
	 scr1=scrit/sqrt(vary)	!both in intermed units
	 if(scr1.lt.3.) then
	   call BELL(1)
	   print 24,scr1,avamp,calfac2*scrit,calfac2*sqrt(vary)
24	   format(
     & ' WARNING: search threshold is only ',f7.2,' times rms noise',/,
     & ' ( full amp = ',f8.2,' pA; threshold = ',f7.2,' pA; rms = ',
     & f7.2,' pA',/,
     & ' O.K. [Y] ? ')
	   read 101,ans
	   if(UC(ans).eq.'N') then
		x=100.*smult
		print 25,x
25		format(
     & ' New percentage of full amplitude for threshold [',f6.1,'] = ')
		call INPUTr(x)
		smult=0.01*x
	      scrit=smult*abs(avamp)		!in intermed units
	   endif
	 endif
	endif
c
	if(navtest.le.0) navtest=1
      OPEN(unit=15,file='SCAN.INI',status='UNKNOWN',
     &  access='DIRECT',form='UNFORMATTED',recl=256)
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
     &   adcfil,nampmark,iamark,tmin,cjump
	else
	   write(15,rec=1) savin	!still keep savin!
	endif
	CLOSE(unit=15)
c calc nsec etc here, in case default nbuf altered
	if(ndat.lt.nbuf) nbuf=ndat    !allocate smaller array
	nsec= 1 + (ndat-1)/nbuf  !number of sections
	nrlast=ndat - (nsec-1)*nbuf  !number of idata in last section
C TSECT=time (microsec) from first point of one section to first point of next
	tsect=float(nbuf)*finter
	npfilt1=ifixr((tf99-tf1)/finter)
	npfilt=ifixr(float(npfilt1)*facjump)	!in case facjump altered
c
	if(invert) dgain=-abs(dgain)
c	if(newpar) goto 2741
	if(invert) opendown=.not.opendown	!SO opendown=DIRECTION ON DISPLAY
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
	RETURN
c278	idest=278
c	RETURN
c259	idest=259
	END




