 	program CJFIT1
c Lahey V2 version
c To take conc jumps from the temporary file n:\CJUMP.DAT, and
c (1) Do further averaging
c (2) subtraction of curves
c (3) Fit the results
c (4) Write those to be kept to the permanent jump files
c
c NB Several subroutines are in a single file called CJFSUBS.FOR
c
c Modif 07/15/91 04:29pm by making ndimd=15 for all VPLOT data arrays
c  (this is needed only in CJVDU but must make dimensions agree in READAT,
c  READREL,REAVER,CJVDU,CJSIMP also)
c   NB link with cjvdu1+readrel1+reaver1+ssdcj1+ycalcj1+cjsimp1+readat1+
c	relwrt1
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
	real*4 avcur(20480),avolt(10240)	!needed only for RELWRT
c	real*4 avcur(40960),avolt(10240)	!needed only for RELWRT
	integer*4 ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
	integer*4 ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	integer*2 ilen2c(10),igap2c(10)	!integer*2 versions for disc
	integer*2 ilen2v(10),igap2v(10)
	integer*2 ivolt1(10),ivolt2(10),ivhold  !pots for each V jump (integer mV)
	integer*2 istrec(500)		!orig=500 bytes (kept as int*2 to fit in 1 rec)
	integer isuper(10)		!sweep #s to be superimposed
	integer lrnfit(20)		!learn fit sequence
      character cdate*11,ctime*11,ndev*2,datfil*15,filnam*30
      character cdate1*11,ctime1*11,cdate0*11,ctime0*11
	character title1*79,title11*79,title0*79
	logical discprt,pon,slock,debug,caplock,present,vjump,sampv
	logical control,control1,vjump1,vramp,first,refit,search,bad
	logical samescal,rescale,store,super,fixgain
c for CJUMP3 data
	integer*2 iver1
	integer*2 ikeep1(4,2),kstep(5),jmask(20480)
	logical keepall
c declarations for CJVDU
c	dimension xcal(2048,10),ycal(2048,10)
	real*4 YVAL(2048,15),Yval1(20480)		!equivalenced
	real*4 YACAV2(20480)				!for 1st latency activations
	real*4 XVAL(2048,15)
	real*4 t1v(10),t2v(10),t1c(10),t2c(10),vstep(10)
	real*4 t1v1(10),t2v1(10),t1c1(10),t2c1(10)	!to avoid duplic in COMMONS
C for CJSIMP
	character*10 TITLEP(20)		!names of params for fit
	real*4 theta(20)
	integer JFIX(20)
	LOGICAL defolt,fitted,dcurve
c for data
c	dimension ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd)
c for calc curves
c	dimension ncal(ndimc),icurvc(ndimc),iline(ndimc)
	dimension ndat(15),icurvd(15),isym(15),ijoin(15)	!for data
	real symsiz(15)				!for data
	dimension ncal(10),icurvc(10),iline(10)		!for calc curve
	character*40 titlex,titley
	character*44 title
	logical doframe
c
	EQUIVALENCE (yval,yval1)
      character*1 ans,UC,ans1
	character*40 mtitle	!machine name
	common/dp/discprt
	common/dpp/filnam,machine,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
c COMMON/cjpar/ for values in RELWRT, READAT (which reads back data written
c by RELWRT), and REAVER. NB this common has the integer*4 (microseconds)
c versions of ILENc()...etc
	COMMON/cjpar/avcur,avolt,nsweep,
     & naver,navc,control,vjump,sampv,nsamp,itsamp,nAv,dnAv,
     & nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,calvolt,
     & ivhold,irate,iTPREc,nAc,dnAc,ncjump,ilenc,igapc,
     & t1v1,t2v1,t1c1,t2c1,nDv,iDd,keepall
	COMMON/JLOGO/t1c,t2c,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c	!for VPLOT2
c For interpolation of data and calculation of time to 50% threshold
c in REAVER1
c	common/bisec/xl(11),yl(11),yl2(11),n,klo,khi
c	external FUNC
c
	pon()=slock()
	debug()=caplock()
c
101   format(a1)
4	format(i8)
      filnam='H:\CJFIT.PRT'
      OPEN(unit=7,file='PRN',iostat=nerr)
	call WINPRINT()	!print file control
	print 102
	if(pon()) write(7,102)
	if(discprt) write(8,102)
102	format(' CJFIT- Relaxation analysis program',/)
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 1,cdate,ctime(1:8),mtitle
	if(pon()) write(7,1) cdate,ctime(1:8),mtitle
	if(discprt) write(8,1) cdate,ctime(1:8),mtitle
1	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 399
399	format(
     & ' SCROLL LOCK on for printing'/
     & ' CAPS LOCK on for debugging')
c
c Initialise
	idiskq=-1		!until defined
	fitted=.false.
	dcurve=.false.	!for now
	rescale=.false.	!do not rescale display in CJVDU before refitting
	store=.false.	!do not store permanently on relaxation floppy
	super=.false.	!do not superimpose plots
	fixgain=.false.	!use gain as read from disc
	search=.false.
	keepall=.true.
	jfit=1
	nj1=0
	njlast=-1
	ilrn=0
	do 361 i=1,20
361	lrnfit(i)=1		!initial fit sequence
c Defaults for calibration
	ftape=1.0		!some defaults
	errfac=1.0
	gain=10.
	amVpA1=10.		!mV/pA on patch clamp
c
	print 36
36	format(
     & ' (1) Take data from CJUMP program (CJUMP.DAT)',/,
     & ' (2) Read back permanently stored relaxation data (RELDAT.DAT)',
     &/,' (3) Import data from MathCad (MCDAT.DAT)',/,
     &  ' Option number [1] = ')
	input 4,iread
	if(iread.le.0.or.iread.gt.3) iread=1
	if(iread.eq.2) then	!read old data
	   call READAT(nj1,nj2,ncon,nav1,nav2,YVAL,idest)
	   if(idest.eq.999) goto 9999
	   call GETSTEP(nvjump,ivolt1,ivolt2,ilenv,iDd,nvramp,vstep)
	   goto 307		!display old data
	else if(iread.eq.3) then
	   ndev='A:'		!default = floppy
27	   call DISCNUM1(idisc,ndev,-1,1)
	   if(idisc.eq.-1) goto 999	!if ESC hit in discnum
	   call READMC(ndev,nsamp,irate,Yval,ierr)
	   if(ierr.ne.0) goto 27
	   nj2=0
	   nvjump=0
	   ncjump=0
	   sampv=.false.
	   vramp=.false.
	   goto 307		!display
	endif
c
25	call DISCNUM1(idisc,ndev,0,0)
	if(idisc.eq.-1) goto 999	!if ESC hit in discnum
	print 251
251	format(' Use default file name [CJUMP.DAT] [N] ? ')
	input 101,ans
	if(UC(ans).eq.'Y') then
	   datfil=ndev//'\CJUMP.DAT'
	else
	   print 252
252	   format('&File name (e.g. cjump1[.dat]) = ')
	   input 253,datfil
253	   format(a15)
	   n=NBLANK(datfil)
c Look for extension on file name. If '.' present assume its given, otherwise
c add extension .DAT
	   do 254 i=1,n
254	   if(datfil(i:i).eq.'.') goto 255	!extension given
	   datfil=datfil(1:n)//'.dat'			!add extension
	   n=n+4
255	   datfil=ndev//'\'//datfil(1:n)
	endif
	INQUIRE(file=DATFIL,exist=present,flen=len)
	if(.not.present) then
	   call BELL(3)
	   print 24,DATFIL
24	   format('&',1x,a12,' does not exist')
	   goto 25
	else
         OPEN(unit=14,file=DATFIL,status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=512)
	    read(14,rec=1) njd,nxtrec,(ISTREC(i),i=1,250)
	    irec=int4(istrec(njd))
	    read(14,rec=30) cdate0,ctime0
	    maxrec=len/512
	   if(pon()) write(7,411) cdate0
	   if(discprt) write(8,411) cdate0
411	   format(' Date of experiment: ',a11)
	endif
c Note that if CJUMP.DAT file has been overwritten in error then data may
c still be there, but irec=1 will me overwritten so njd,istrec will be
c lost: can try to regenerate them here
671	continue
c
	lstrec=nxtrec-1
	if(maxrec.gt.lstrec) then
	   print 49,datfil,njd,ctime0(1:8),cdate0,istrec(njd),nxtrec-1,
     &	maxrec,njd,lstrec
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
	   input 4,iopt
	   if(iopt.le.0) iopt=1
	   search=iopt.eq.2
	   if(search) then
		n1=njd		!start with last known record
		n2=500
		goto 50
	   endif
	   if(iopt.eq.3) then
		read(14,rec=lstrec) i	  !position record at end of data
		ENDFILE 14
		maxrec=lstrec
	   endif
	endif
c
67	continue
	print 62,njd,datfil,ctime0(1:8),cdate0
62	format(' ',i4,
     & ' sweeps on disc in ',a15,'. Last sweep at ',a8,' on ',a11,/,
     & ' [0] No (more) listing',/,
     & ' (1) List brief details only (scroll lock on to print)',/,
     & ' (2) List full details (scroll lock on to print)',/,
     & ' Option number [0] = ')
	input 4,iopt
	if(iopt.eq.0) goto 42
	iprt=1
	if(iopt.eq.2) iprt=2
	if(.not.search) then
c	   if(pon()) write(7,622) njd,datfil
	   if(discprt) write(8,622) njd,datfil
622	   format(' ',i4,' sweeps on disc in ',a12)
	   print 60
60	   format(' List for sweeps n1 to n2:  n1 (,n2) = ')
	   input 63,n1,n2
63	   format(2i8)
	   if(n1.eq.0) goto 67
	   if(n2.eq.0) n2=n1
	endif
50	continue
	do 64 i=n1,n2
261	   irec=int4(istrec(i))
c	   print 700,i,irec,maxrec
c700	   format(' i,irec,maxrec= ',3i8)
	   read(14,rec=irec) cdate,ctime,title1,naver,navc,iav,control,
     &  vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &   ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &   ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     &   ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac
	   if(iver1.eq.-1003) then
	     call CALCNS2(ikeep1,nkeep,kstep,nsamp,nsamp1,jmask,keepall)
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
     &   iver1,amVpA1,ftape,gain,errfac,nkeep,ikeep1,kstep,keepall)
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
64	continue
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
	input 101,ans
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
	super=.false.
	print 40,nj1+1,nj1+1,jfit
40	format(
     & ' (1) Fit single record, or subtracted pair',/,
     & ' (2) Re-average single sweeps from file before fitting',/,
     & ' (3) Fit the next single record (#',i5,')',/,
     & ' (4) Fit the next single record (#',i5,') -same graph scales',/,
     & ' (5) Superimpose up to 10 I(t) vs t sweeps',/,
     & ' Option number [',i2,'] = ')
	input 4,j
	if(j.ge.1.and.j.le.5) jfit=j
	if(jfit.eq.3.or.jfit.eq.4) then
	   nj1=nj1+1
	   nj2=0
	   goto 21
	endif
	if(jfit.eq.5) then
	   if(pon()) write(7,4211)
	   if(discprt) write(8,4211)
4211	   format( ' Superimposed plots:')
	   print 421
421	   format('&Number of sweeps to be superimposed = ')
	   input 4,nsuper
	   if(fixgain) then
		print 423
423		format('&Overide the gain read from disc as before [Y] ? ')
		input 101,ans
		if(UC(ans).eq.'N') fixgain=.false.
	   endif
	   jc=1
	   nj1=0	!initial default
	   id=1
	   do 43 js=1,nsuper
46		nj1=nj1+id
		print 44,js,nj1
44	      format('&',i3,': sweep number [',i4,'] = ')
		input 4,j
		if(j.gt.0) nj1=j
		isuper(js)=nj1
		if(js.gt.1) id=isuper(js)-isuper(js-1)	!default increment
		irec=int4(istrec(nj1))			!read data set #nj1
	      read(14,rec=irec) cdate,ctime,title1,naver,navc,iav,control,
     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &      ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &      ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     &      ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac
c NB iver1=-1001 for CJUMP2 data (values as far as swtime recorded) and
c iver1=-1003 for CJUMP3 data, for which all values read above are recorded
	      if(iver1.eq.-1003) then
	       call CALCNS2(ikeep1,nkeep,kstep,nsamp,nsamp1,jmask,keepall)
	       nsamp0=nsamp		!orig # of points sampled
	       nsamp=nsamp1		!# of points kept on disc
	   	 if(.not.keepall.and.nsamp.gt.2048) then
		    call BELL(3)
		    print 72
72		    format(
     & ' More than 2048 points not allowed with CJUMP3 unless all kept')
		    goto 42
		 endif
	      endif
		ngr=1 + (nsamp-1)/2048
	      call ILCONV(ilenc,igapc,ilenv,igapv,
     &	ilen2c,igap2c,ilen2v,igap2v,iDd,1)	!convert ilen2c to ilenc etc
		call PREC(nj1,title1,cdate,ctime,naver,navc,iav,vjump,
     & 	control,iver1,2)
		call PPAR(1,1,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     &    calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     &   vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     &   iver1,amVpA1,ftape,gain,errfac,nkeep,ikeep1,kstep,keepall)
		if(fixgain) then
		   calfac=calfac1
		   calvolt=calvolt1
		endif
		print 45
45		format(' This record O.K. [Y] ? ')
		input 101,ans
		if(UC(ans).eq.'N') goto 46
c	Read currents for this one
c 		Call READREL with sampv=false so voltages not read
		call READREL(irec,nsamp,iav,.false.,calfac,calvolt,
     &	Yval,jc,jv)
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
c		  jc=jc+1	!currents in yval(i,jc), jc=1,2,...,nsuper
		  jc=jc+ngr	!currents in yval(i,jc)
		  if(jc.gt.11-ngr) then
		   call BELL(3)
		   print 73
73		   format(' No room for more sweeps')
		   goto 431
		  endif
		endif
43	   continue		!end of js=1,nsuper loop
c
431	   print 422
422	   format(' Normalise all to same baseline [N] ? ')
	   input 101,ans
	   if(UC(ans).eq.'Y') then
		nsuper=-nsuper
	   endif
	   super=.true.
	   goto 307		!plot them
	endif
c
	if(jfit.eq.2) then
c	   print 231,niav
c231	   format(
c     & ' File contains ',i3,' events that are either single sweeps, or',
c     & /,' are ''averages'' of one sweep')
	   call REAVER1(istrec,Yval,Yval1,title1,nj1,nj2,machine,idest,
     &	makav,ntemp1,Yacav2)
	   if(idest.eq.67) goto 67	!more details
	   if(idest.eq.3)	goto 999	!exit
	   ilrn=0
	   goto 307				!plot/fit
	endif
c NB For C-jumps alone will probably want to read only one sweep (current trace)
c but for V-jumps/ramps will want to read control (no agonist) sweep also
3	continue
	print 2,njd
2	format(' Number of records on disc = ',i5,/,
     &' To fit a single sweep specify its number; to fit difference',/,
     &' between two sweeps (drug and control) specify both numbers',/,
     & ' Read sweeps:  n1 (,n2) [more details] =  ')
	input 63,nj1,nj2
	if(nj1.le.0) goto 67
c
21	continue
	if(nj1.ne.njlast+1) then
	   ndelt=0		!initialise learning of xdelt
	   xdelt=2.0	!initially put cursor 2ms before peak in CJVDU
	endif
	njlast=nj1
c Restart the learned fit sequence in lrnfit() each time a new record is read
	ilrn=0
c
	irec=int4(istrec(nj1))
c Read the specified record  'header'
	read(14,rec=irec) cdate,ctime,title1,naver,navc,iav,control,
     & vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     & ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     & ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     & ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac
	if(iver1.eq.-1003) then
	   call CALCNS2(ikeep1,nkeep,kstep,nsamp,nsamp1,jmask,keepall)
	   nsamp0=nsamp		!orig # of points samples
	   nsamp=nsamp1		!# of points kept on disc
	   if(.not.keepall.and.nsamp.gt.2048) then
		call BELL(3)
		print 72
		goto 42
	   endif
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
     &   iver1,amVpA1,ftape,gain,errfac,nkeep,ikeep1,kstep,keepall)
	if(nj2.gt.0) then
	   irec=int4(istrec(nj2))
c Read the specified record  'header'
	   read(14,rec=irec) cdate1,ctime1,title11,naver1,navc1,iav1,
     &	control1,vjump1
	   call PREC(nj2,title11,cdate1,ctime1,naver1,navc1,iav1,vjump1,
     &   control1,iver1,1)
	   call PPAR(1,0,cdate1,ctime1,iTSAMP,iTPREc,nsamp,nDv,iDd,
     &   calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     & vjump1,control1,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     &   iver1,amVpA1,ftape,gain,errfac,nkeep,ikeep1,kstep,keepall)
	endif
	print 12
c' Are these records O.K. [Y] ? ')
12	format(
     & ' (1) Use these records as they are',/,
     & ' (2) Specify new calibration (value from CJUMP is wrong)',/,
     & ' (3) Specify different records',/,
     & ' Option number [1] = ')
	input 4,iopt
	if(iopt.le.0.or.iopt.gt.3) iopt=1
	if(iopt.eq.3) goto 3
	if(fixgain) then
	   call BELL(1)
	   print 681
681	   format('&Continue to overide gain read from disc [Y] ? ')
	   input 101,ans
	   if(UC(ans).eq.'N') then
		fixgain=.false.
	   else
		calfac=calfac1
		calvolt=calvolt1
	   endif
	endif
	if(iopt.eq.2) then
	   calvolt=10.		!factor by which Vout is greater then true Em
	   call GETCAL(calfac,amVpA,amVpA1,VpA,VpA1,pAV,pAV1,
     &   itape,ftape,gain,errfac)
	   print 68,ifixr(calvolt)
68	   format(
     & ' mV out from clamp per mV membrane pot (integer) [',i3,'] = ')
	   input 4,i
	   if(i.ne.0) calvolt=float(i)
	   calfac1=calfac		!save values to overwrite those from disc
	   calvolt1=calvolt	!save values to overwrite those from disc
	   fixgain=.true.
	endif
c	input 101,ans
c	if(UC(ans).eq.'N') goto 3
c
c
	irec=int4(istrec(nj1))			!read data set #nj1
	ngr=1 + (nsamp-1)/2048
	jc=1		!current in yval(i,1)
c	jv=2		!voltage in yval(i,2)
c	if(nj2.gt.0) jv=3		!voltage in yval(i,3)
	jv=1+ngr			!voltage in yval(i,2)
	if(nj2.gt.0) jv=1+2*ngr		!voltage in yval(i,3)
	call READREL(irec,nsamp,iav,sampv,calfac,calvolt,Yval,jc,jv)
c Print the details (not on screen)
	call PREC(nj1,title1,cdate,ctime,naver,navc,iav,vjump,
     & control,iver1,3)
	idprt=2	!full details to disc
	if(title1.eq.title0) idprt=1	!brief details only if same title
	title0=title1		!keep last title for next time round
	call PPAR(0,idprt,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     & calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     & vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     &   iver1,amVpA1,ftape,gain,errfac,nkeep,ikeep1,kstep,keepall)
c Read 2nd data set if req
	if(nj2.gt.0) then
	   irec=int4(istrec(nj2))
	   read(14,rec=irec) cdate,ctime,title1,naver1,navc1,iav,control1,
     &  vjump,sampv,nsamp1,itsamp1,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     &   ivolt1,ivolt2,calfac,calvolt,ivhold,irate1,iTPREc,nAc,dnAc,
     &   ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     &   ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac
	   if(iver1.eq.-1003) then
	      call CALCNS2(ikeep1,nkeep,kstep,nsamp,nsamp1,jmask,keepall)
	      nsamp0=nsamp		!orig # of points samples
	      nsamp=nsamp1		!# of points kept on disc
		if(.not.keepall.and.nsamp.gt.2048) then
		 call BELL(3)
		 print 72
		 goto 42
	      endif
	   endif
	   call ILCONV(ilenc,igapc,ilenv,igapv,
     &	ilen2c,igap2c,ilen2v,igap2v,iDd,1)	!convert ilen2c to ilenc etc
	   call PREC(nj2,title1,cdate,ctime,naver1,navc1,iav,vjump,
     &   control1,iver1,2)
	   call PPAR(1,idprt,cdate,ctime,iTSAMP1,iTPREc,nsamp1,nDv,iDd,
     &   calfac,calvolt,nAc,dnAc,irate1,ncjump,ilenc,igapc,ivhold,sampv,
     &  vjump,control1,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     &   iver1,amVpA1,ftape,gain,errfac,nkeep,ikeep1,kstep,keepall)
c Test a few things to check samples comparable
	if(itsamp.ne.itsamp1.or.irate.ne.irate1.or.nsamp.ne.nsamp1) then
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
		input 101,ans
		if(UC(ans).ne.'Y') goto 3	!read again
		ncon=1		!#nj1 is control
		print 132
132		format('&For net trace subtract #1 from #2 [Y] ? ')
		if(UC(ans).eq.'N') ncon=2
	   else if((.not.control).and.(.not.control1)) then
		call BELL(2)
		print 131
131		format(' Neither record is a control O.K. [N] ?')
		input 101,ans
		if(UC(ans).ne.'Y') goto 3	!read again
		ncon=1		!#nj1 is control
		print 132
		if(UC(ans).eq.'N') ncon=2
	   endif
c (a) If one data set only read (#nj1, nj2=0) then jc=1,jv=1+ngr so need
c	2*ngr arrays altogether (OK up to ngr=10, nsamp=20480 if voltage
c	not recorded, or ngr=5, nsamp=10240 if voltage recorded)
c (b) If two data sets read (#nj1, #nj2) then for #nj1 jc=1, and
c	for #nj2 jc=1+ngr: 2*ngr arrays altogether (OK up to ngr=5, nsamp=10240
c	if voltage not recorded. If voltage recorded then for #nj1 jv=2*ngr+1
c	and for #nj2 jv=3*ngr+1; need 4*ngr arrays altogether (OK up to ngr=2,
c	 nsamp=4096)
c	   jc=2		!current in yval(i,2)
c	   jv=4		!voltage in yval(i,4)
	   jc=1+ngr		!current in yval(i,2)
	   jv=1+3*ngr	!voltage in yval(i,4)
	   call READREL(irec,nsamp,iav,sampv,calfac,calvolt,Yval,jc,jv)
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
	call CJVDU(Xval,Yval,xval0,xvaln,vramp,sampv,
     & nsamp,itsamp,nAv,dnAv,nvjump,ilenv,igapv,ivolt1,ivolt2,
     & irate,nAc,dnAc,ncjump,ilenc,igapc,nj1,nj2,ncon,
     & t1c1,t2c1,t1v1,t2v1,vstep,samescal,rescale,ypeak,ybase,tau1,
     & ISYM,ILOG,ISCAL,IJOIN,iline,XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,
     & ixlo,ixhi,iylo,iyhi,y0,yinf,xcross,ycross,titlex,titley,title,
     & itit,ilabel,inumx,inumy,ncurvd,ndat,icurvd,ncurvc,ncal,icurvc,
     & symsiz,idiskq,ntx,nty,itx,ity,theta,fitted,doframe,dcurve,refit,
     & ifit,super,nsuper,isuper,xdelt,ndelt,ilrn,lrnfit,jfit,
     & ikeep1,nkeep,kstep,keepall,nsamp0,jmask,iread,idest)
c Now fit if idest=3, otherwise (if no fit, or fit already displayed) ask
c if another required
	if(idest.eq.3) then
	   call CJSIMP(vramp,kfit,jfix,nfix,defolt,titlep,fitted,
     &   dcurve,theta,nev,sres,smin,xval,yval,xmin,xmax,ymin,ymax,ndat,
     &   icurvd,ypeak,ybase,tau1,idest)
	   call CJOUT(theta,nev,smin,jfix,fitted,titlep)
	   print 5
5	   format(' Type <any key> for display')
	   call ANYKEY
	   goto 307	!for display/plot with fitted=true
	else if(iread.ne.2) then	!do not ask if read from RELDAT!
	   ans='N'
	   if(store) ans='Y'
	   print 52,ans
52	   format(
     &' Store data permanently on jump (relaxation) floppy [',a1,'] ? ')
	   input 101,ans1
	   ans1=UC(ans1)
c Change default only if ans='y' and ans1='n', or vice versa
	   if(ans.eq.'Y'.and.ans1.eq.'N'.or.ans.eq.'N'.and.ans1.eq.'Y')
     &	store=.not.store
	   if(store) then
c=======not fixed for CJUMP3 OR for nsamp>2048
c Store values on floppy
		nsweep=nj1
		nsweep1=nj2
		first=.true.
	      call PREC(nj1,title1,cdate,ctime,naver,navc,iav,vjump,
     &      control,iver1,1)
		if(nj2.gt.0) then
	        call PREC(nj2,title11,cdate1,ctime1,naver1,navc1,iav1,
     &        vjump1,control1,iver1,1)
54		  continue
		  if(jfit.eq.1.or.jfit.eq.3.or.jfit.eq.4) then
		   print 53,nj1,nj2
53		   format(' (1) Store sweep ',i5,/,' (2) Store sweep ',i5,/,
     &	  '  Option number = ')
		  else if(jfit.eq.2) then
		   print 531,nj1,nj2
531		   format(
     &	' (1) Store first average -started with event ',i5,/,
     &	' (2) Store second average -started with event ',i5,/,
     &	  '  Option number = ')
		  endif
		  input 4,iopt
		  if(iopt.ne.1.and.iopt.ne.2) goto 54
		  if(iopt.eq.2) then
			nsweep=nj2
			nsweep1=nj1
		  endif
		endif		!end of nj2>0 bit
c store sweep # nsweep -reread 'header'
58		continue
		irec=int4(istrec(nsweep))
		read(14,rec=irec) cdate,ctime,title1,naver,navc,iav,control,
     &    vjump,sampv,nsamp,itsamp,nAv,dnAv,nvjump,iTPREv,ilen2v,igap2v,
     & 	ivolt1,ivolt2,calfac,calvolt,ivhold,irate,iTPREc,nAc,dnAc,
     &      ncjump,ilen2c,igap2c,nDv,iDd,iver1,nsweep,swtime,isweep,
     &      ikeep1,nkeep,kstep,amVpA1,ftape,gain,errfac
	      if(iver1.eq.-1003) then
	       call CALCNS2(ikeep1,nkeep,kstep,nsamp,nsamp1,jmask,keepall)
	       nsamp0=nsamp		!orig # of points samples
	       nsamp=nsamp1		!# of points kept on disc
		 if(.not.keepall.and.nsamp.gt.2048) then
		   call BELL(3)
		   print 72
		   goto 42
	       endif
	      endif
	      call ILCONV(ilenc,igapc,ilenv,igapv,
     &	ilen2c,igap2c,ilen2v,igap2v,iDd,1)	!convert ilen2c to ilenc etc
c	 and reread data for jfit=1 case (for jfit=2 data is only in Yval)
		if(jfit.eq.1) then
		 jc=1		!read current into yval(i,1)
		 jv=2		!read voltage into yval(i,2)
		call READREL(irec,nsamp,iav,sampv,calfac,calvolt,Yval,jc,jv)
		 do 55 i=1,nsamp
55		 avcur(i)=Yval(i,1)
		 if(sampv) then
		   do 56 i=1,nsamp
56		   avolt(i)=Yval(i,2)
		 endif
		else if(jfit.eq.2) then
		 if(nsweep.eq.nj1) then
		  jc=1			!current already in yval(i,1)
		  jv=2			!voltage already in yval(i,2)
		  if(nj2.gt.0) jv=3		!voltage in yval(i,3)
		 else if(nsweep.eq.nj2) then
		  jc=2		!current in yval(i,2)
		  jv=4		!voltage in yval(i,4)
		 endif
		 do 70 i=1,nsamp
c70		 avcur(i)=Yval(i,jc)
70		 avcur(i)=Yval1(i)		! Brian's temp modification
		 if(sampv) then
		    do 71 i=1,nsamp
71		    avolt(i)=Yval(i,jv)
		 endif
		endif
c    Put most args in COMMON for RELWRT -easier to use latter for both CJFIT
c	and TRANAV this way
		iprog=1		!tells RELWRT that data is from CJUMP
		call RELWRT(iprog,cdate,calfac)
		if(nj2.gt.0.and.first) then
		 if(jfit.eq.1) then
		   print 57,nsweep1
57		   format(' Store also sweep # ',i4,' [Y] ? ')
		 else if(jfit.eq.2) then
		   print 572
572		   format(' Store other average too [Y] ? ')
		 endif
		 input 101,ans
		 if(UC(ans).ne.'N') then
			nsweep=nsweep1
			first=.false.
			goto 58	!store the other
		 endif
		endif
	   endif		!end of storage in RELDAT
c
c
	   call SAVEDP		!saveguard disc print file!
c
511	   continue
	   print 51
c51	   format('&Display/fit another curve [Y] ? ')
c	   input 101,ans
c	   if(UC(ans).ne.'N') then
c		print 511
c511		format('&Refit the same data [N] ? ')
c	      input 101,ans
c	      if(UC(ans).ne.'Y') goto 42    !different data
51	   format(
     &	' (1) Refit data',/,
     &	' (2) Display/fit new data',/,
     &	' (3) Exit from CJFIT',/,
     &   	' Option number [2] = ')
	   input 4,iopt
	   if(iopt.eq.3) goto 999
	   if(iopt.eq.0) iopt=2
	   if(iopt.lt.1.or.iopt.gt.3) goto 511
	   if(iopt.eq.2) goto 42		!new data
	   if((iopt.eq.1).and.(makav.eq.3)) then
	     print 1010
1010	     format(' Fit the ensemble for off-jump activations [Y] ? ')
	     input 101,ans
	     if(UC(ans).eq.'N') then
	       goto 9999			! finish
	     else
	       Do 1020 i=1,nsamp
		   if(i.le.ntemp1) then
	           YVAL1(i)=yacav2(i)
		   else if(i.gt.ntemp1) then
		     YVAL1(i)=0.0
		   endif
1020	       continue
	       goto 307
	     endif
	   else
		fitted=.false.			!same data
		ncurvc=0
		print 512
512		format('&Same scales as before for display [Y] ? ')
	      input 101,ans
	      if(UC(ans).ne.'N') samescal=.true.
		goto 307                      !same data
	   endif
	endif
c
999	continue
      if(iread.eq.1) CLOSE(unit=14)
9999	continue
	call ENDPRINT
	END

