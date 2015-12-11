program CJUMP31
c	24.06.93
c Lahey V5.n version with Ioana's 1401 commands
c PRIMES.DAT and CJUMP3.SPR now in same directory as prog
c
c To control and record conc jumps. Controls Piezo via DAC2 and membrane pot
c via DAC3, with option to do voltage jumps and/or ramps
c
c NB most subroutines are in a single file called CJSUBS.FOR
c
c CJUMP3 (07/11/91 01:24pm) Is modified version of cjump3 (now cjump30) that can
c	keep up to 20480 points on disc (=part or whole of uploaded ADC sample)
c	Sample size can be up to 20480 for single current vs time sweeps
c	or up to 10240 when 2 traces are to be subtracted (whether or
c	not voltage recorded: because VPLOTR in CJFIT cannot show more
c	than two x 10240 point traces at once so to show drug and control
c	together limits sample to 10240 points)
c	Note that AVCUR etc now twice size of AVOLT etc to reflect max sample size
c
c 06/27/91 12:06pm ist,isz etc converted to integer*4 (so works with
c 1401-plus too)
c
c 04/26/91 10:43am CJUMP3 is like CJUMP2 but with option to omit specified
c sections of record (eg baseline) to allow high sample rate without
c exceeding 2048 points. The 1401 samples as many points as needed but
c keep only points IKEEP(i,1,m) to IKEEP(i,2,m) for i=1,..4
c sections of total sample for sweep #m in a multiple sweep series (for eg gap
c series need different IKEEP for every sweep in series, though eg for
c voltage series IKEEP would be same for each). Say NKEEP (=1,...,4) sections
c are kept (with m=1,..,30 sweeps) the default being NKEEP=1 and IKEEP(1,1,m)=1,
c IKEEP(1,2,m)=nsamp ie keep the whole sample. For keeping data on disk (CJDISK)
c each sweep kept separately so need only keep IKEEP1(i,j) for that sweep.
c Also define constant jkeep=0 if the same time points are kept in each of a
c series of multiple sweeps; or jkeep=1 if they are different (as, eg, in case
c of gap series); or jkeep=-1 if they are different, but at fixed times
c relative to each C-jump (as used in practice for gap series), and jkeep=-2
c is same but times relative to V-jumps
c	In this version whole sample is uploaded which allows option to keep
c points in between those specified but with a lower sample rate. There may be
c between nkeep-1 and nkeep+1 in-between bits (depending on whether the kept
c bits reach the ends of the samples so define KSTEP(i), i=1,..,5 as n where
c every nth point only is kept for the section before 1st kept-bit,...,after
c the last.
c Example: keep points 399-502, nkeep=1, then kstep(1)=5 means every 5th point
c kept ie points 1,6,11,16,...,396 then 399,340,341,...,502, then, if
c kstep(2)=10, 512,522,...,992 (for nsamp=1000).  DEFINE kstep(i)=0 if
c no points to be retained in in-between bits.
c
c
c 04/17/91 09:22pm Add 3rd level menu
c 03/27/91 01:50pm Big revision including
c (1) Reload everything whenever parameters changed, to be sure
c (2) Ability to run a series of sweeps automatically. At present, if series
c	contains consecutive equal params ( eg c-jump length=10,10,20,20,...)
c	then average is calculated and displayed, but not wriiten to disk).
c
c 03/17/91 11:57am Modified:
c  ILENc(),igapc(),ilenv(),igapv() all now kept as integer*4 microseconds
c  in the program, and now stored in .SPR as such (no need to convert to
c  integer*2 as in earlier version now .SPR is bigger)
c
c 10/30/90 09:52am  Modified:
c (1) so voltage can be less than 5V for smaller excursion of piezo, and
c (2) so can set ncjump=0 if V-jump only required (in this case none of the
c output values for DAC2 are defined). In this case must still hit #1 when
c agonist present (put in bath 'by hand' rather than via piezo controlled
c from DAC2), and #2 for control, so data is accumulated in appropriate arrays.
c (NB could also use DAC2 to control taps for bath applic)
c [CJUMPT is test version of CJUMP for Tosh. Lines labelled c# prevent call
c to CED routines]
c NB following are int*4, and are kept in integer microsec
c	iTSAMP=total sample length;
c	iTPREc, iTPREV=time to first C- ,V-jump
c	iDd=time between DAC outputs (1 channel)
c	dA=time between ADC samples  (1 channel)
c
c	ILEN,IGAP etc are int*2 at present, so can get
c	only up to 32767 =32 sec if in msec as now (but only up to 32 ms if
c	ilen kept in musec too!)
c
c Jobs to be done:
c==== fix INIPLOT and PLOTS so do as little redrawing of graphics as poss
c	between jumps
c===option for scope trigger on DIG?
c	NB: MEMDAC updates all the specified DACS at each clock tick so,
c unlike ADCMEM, there is no need to change clock rate if number of channels
c is altered!
c	NB Both ADCMEM and MEMDAC when triggered should sample ADC/set DAC at the
c moment of the trigger- NO- experimentally seems that ADCMEM does this, but
c MEMDAC puts out first value one tick AFTER the trigger (eg if IDAC(1) is high
c and rest zero, and rate is 1kHz the DAC sets from 1-2 ms (not from 0-1ms) but
c ADC starts sample at 0 (=trigger time).  When DAC rate=2kHz DAC output
c is set from 0.5-1.0ms (1 tick=0.5ms)
c Thus iADCc(nAc) is at time = (nAc-1)*dA from moment of trigger (t=0)
c  and DAC goes high at time nDc*dD where iDACc(nDc) is 1st element set high
c where dA=time between ADC samples=1/ADC freq=integer # of 0.25 mus ticks
c and dD=time between DAC outputs=1/DAC freq=integer # of 0.25 mus ticks=1ms now
c To get an ADC sample, viz that in ADC(nAc), coincident with the moment
c the (first) DAC (c-jump) pulse starts then must have
c		ITPREc = (nAc-1)*dA = nDc*dD = time from trigger to start of pulse
c ie
c		nAc = 1 + nDc*dD/dA  must be exactly an integer 		(1)
c
c When V-jumps done, we also require (preferably) that the moment of each
c V-jump is also exactly coincident with one of the ADC sample points, and
c sample rates, dD,dA are same, so similarly, if iTPREv=time to start of 1st
c V-jump
c
c		iTPREv = (nAv-1)*dA = nDv*dD
c
c where iADC(nAv) is coincindent with the moment of the (1st) V-jump when
c iDACv(nDv) is the first element to depart from resting pot.
c 	However for V-RAMPS it does not really matter whether an ADC sample
c is coincident with start of ramp, as long as both I and V are sampled
c sufficiently often during the ramp to cover the range
c	In fact coincidence is not so important for c-jump or v-jump either
c but if points not coincident then must keep not only nAc (nAv) but also
c the time difference from iADC(nAc) to moment that jump starts, so that
c relaxation can be plotted with the correct t=0.
c When ADC sample NOT coincident can calc nAc (nAv) as in eq(1) above
c except that calc done in floating point. If nAc=11.75 say this means
c that moment of jump corresponds to 0.75*dA msec after iADC(11), ie the
c jump starts at (11.75-1)*dA msec from the moment of the trigger =nD*dD.
c
c NB Size (in bytes), isz and iszout, MUST be multiple of 4 (for 2-byte
c data ie 12 bit accuracy)

	real*4 avcur(20480),avcurcon(20480)	!average current (drug,control)
	real*4 tcur(20480),tcurcon(20480)	!total current (drug,control)
	real*4 avolt(10240),avoltcon(10240)	!average voltage (drug,control)
	real*4 tvolt(10240),tvoltcon(10240)	!total voltage (drug,control)
	real*4 swval(30) 		!values that change between sweeps
	integer*4 iperm(30)
	integer*4 ispre1,ispre2,iscount,iflag	!for timing sweeps
	integer nprime(1900)	!holds primes up to 16381 (see PRIMGEN.FOR)
 	integer*2 ierr,iADC(20480)
	integer*2 iADC1(10240)		!for voltage on ADC1, if req
	integer*2 jmask(20480)
	integer*2 iDAC(10000)		!for DAC output
	integer*2 iDACc(5000),iDACv(5000)   !for DAC2 (c-jump) and DAC3 (v-jump)
	integer*4 ist,isz,irept,ipre,icount		!args for ADCMEM
	integer*4 istout,iszout,ipre1,icount1	!args for MEMDAC
	integer*4 ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
	integer*4 ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	integer*2 ivolt1(10),ivolt2(10),ivhold  !pots for each V jump (integer mV)
	integer*2 ivhdac			!ivhold in DAC units
	integer*2 istrec(250)		! First record # for CJDISK
	integer*2 istatus,ktype
	INTEGER*2 e0,e1				! For error test
	integer*2 videotyp
	integer*2 ikeep(4,2,30),ikeep1(4,2),kstep(5)
	integer ival1(10),ival2(10),ival3(12)
	integer*4 lval(0:3)
      character filnam*30,cdate*11,ctime*11,cnaver*11,datfil*12
	character title*60,title1*79,ndev*2,titles*30,note*79
	character*6 fstring
	character chs*11,chs1*14
	character*1 ch,getch,a,ANI
	character*1 ans,UC
	character*60		str1
	character*60		fmt
	logical discprt,pon,slock,debug,caplock,present,abort,newpar
	logical record,vjump,control,noquery,sampv,auto,kbhit,reverse
	logical rec1,first,samepar,repeat,randomiz,mkeep,keepall,recscn
	logical readpar,mono
	logical plus		!=true if 1401-plus connected
  			!for boxes
  			!For DCMENU
	integer Lb(18)
	character*78 text(18)
	character*14 text1(10),text2(10),text3(12),tspec(5)
	integer ispec(5)
	common/DMENU/csizb,ifonb,csize,ifont2,nboxlast,nblast
	common/hgv/ivxlo,ivxhi,ivylo,ivyhi,ixlo,ixhi,iylo,iyhi
	common/fixswp/ismode,nsweep,swval,itPREc,itPREv,ncjump,nvjump,
     & ilenc,igapc,ilenv,igapv,tkpre,tkpost,jkeep		!for MODKEEP
	common/lscal/sx,sy,xoff,yoff		!for Lahey graphics
	common/lgrf/xpos,ypos,ipen		!ditto
	common/rand/ix,iy,iz
	common/inter/plus		!for LOAD.FOR (plus=true if 1401-plus connected)
	common/dp/discprt
	character*74 mtitle
	common/dpp/filnam,machine,ndisc,jcol,mtitle !for WINPRINT,ENDPRINT,DISCNUM

	pon()=slock()
	debug()=caplock()

	filnam='CJUMP3.PRT'
	OPEN(unit=7,file='PRN',iostat=nerr)
	call WINPRINT	!print file control
	print 1
	if(pon()) write(7,1)
	if(discprt) write(8,2001)
2001	format(' CJUMP3- Concentration- and V-jump experiment program',/)
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 2225,cdate,ctime(1:8),mtitle
	if(pon()) write(7,2002) cdate,ctime(1:8),mtitle
	if(discprt) write(8,2002) cdate,ctime(1:8),mtitle
2002	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 2003
2003	format(
     & ' SCROLL LOCK on for printing',/,
     & ' CAPS LOCK on for debugging',/,/,
     & ' Sample current on ADC0',/,
     & ' Sample voltage on ADC1 (for V-ramps only)',/)
	mono=jcol.eq.0	!monochrome screen
			!defs for DCMENU
	icol1=14	!yellow text/border for boxes in DCMENU
	icol2=8	!grey background for boxes
	icol3=12	!red text/border
	icol4=15	!white fill
	if(mono) then
	   	icol3=15		!white text/border
	   	icol4=0		!black fill
	endif
	ifonb=0
	csizb=1.7
	ifont=4
	csize=2.
	ivxlo=0		! for VIEWPORT
	ivxhi=10000
	ivylo=0
	ivyhi=7000
	ixlo=0		! for graphboundary
	ixhi=10000
	iylo=0
	iyhi=7000
	call ForOpen1401(ierr) 	!OPEN 1401
	if(ierr.ne.0) then
	   print*,'1401 open failed',ierr
	   call EXIT(2)
	else
	  print*,' 1401-plus opened succesfully'
	endif

			!Load commands that are needed
			!NB the array IWORK used in Ld() is
			!temporary work space only- needs to be
			!about 1.5*size of the largest command loaded.
			!No need to use separate
			!array (iwork here)- could use say iADC() array
			!which can be set to 0 after
			!Ld() call ready for its real use to
			!hold sampled data.  This saves space.
	idw=10240		!bytes for work array
	call ForLd('c:\1401\','KILL,MEMDAC,ADCMEM,TIMER2',1,20,ies,ierr)
	if(ierr.ne.0) then
	   	print*,'1401 command load failed',ierr
	   	call ForClose1401()
	   	call EXIT(2)
	else
	   	print*,'1401 command LOAD OK!',ierr
	endif
			!Set size of ADC data array
	kmax=10240		!max size of iADC1,avolt,tvolt etc
	kmax2=20480		!max size for iADC,avcur,tcur etc
	idim=5000	!max size of iDACc and iDACv arrays- up to 5 sec at 1 kHz
	idim2=10000	!max size of iDAC array

			!Prepare to record results on disc
	nfull=0
	ijd=0		!# of jumps recorded so far
	njd=0		!no jumps on disc (unless specified below)
	nxtrec=2	! record #1 holds njd,nxtrec,istrec
	do i=1,250
   		istrec(i)=0		!initialise
	end do

25	continue
	call DISCNUM1(idisc,ndev,0,0)	!ask for winchester or floppy
	if(idisc.eq.-1) goto 9999	!if ESC hit in discnum
	datfil=ndev//'\CJUMP.DAT'
	INQUIRE(file=DATFIL,exist=present)
	if(present) then
         	OPEN(unit=14,file=DATFIL,status='UNKNOWN',
     &   	access='DIRECT',form='UNFORMATTED',recl=512)
	   	read(14,rec=1) njd,nxtrec,ISTREC
	   	call BELL(2)
	   	print 2004,DATFIL,njd,250-njd
2004	   	format(1x,a12,' already exists; contains',i5,' jumps:',/,
     & 	' (1) Append results to existing file (room for ',i5,' more)',/,
     & 	' (2) Overwrite the existing file',/,
     & 	' (3) Use a different disc',/,
     & 	' Option number [1] = ')
	   	read 4,iopt
	   	if(iopt.le.0) iopt=1
	   	if(iopt.eq.1) then
			ijd=njd	!use nxtrec,istrec as read from disc
	   	else if(iopt.eq.2) then
			ijd=0		!# of jumps recorded so far
			njd=0		!no jumps on disc (unless specified below)
			nxtrec=2	! record #1 holds njd,nxtrec,istrec
			do i=1,250
				istrec(i)=0		!initialise
			end do
	   	else
			goto 25	!get another disc
	   	endif
	else
		ijd=0		!# of jumps recorded so far
		njd=0		!no jumps on disc (unless specified below)
		nxtrec=2	! record #1 holds njd,nxtrec,istrec
		do i=1,250
   			istrec(i)=0		!initialise
		end do
	endif
	CLOSE(unit=14)
			!Carry on after choosing new disk when one filled up
	if(nfull.eq.1) goto (99,205,312) iret
      OPEN(unit=13,file='PRIMES.DAT',status='UNKNOWN',
     & 	access='DIRECT',form='UNFORMATTED',recl=7600)
			!NB recl is in bytes (7600 bytes=1900 integer*4)
	read(13,rec=1) nprime
      CLOSE(unit=13)
			!zero input array to hold ADC data
	do i=1,kmax2
		iADC(i)=-1
	end do
			! Scaling: 1401 units are -32768 t0 +32752
			!in 16 unit increments; dividing
			! Define defaults (later could read
			!choice of these from a file)
			! Version number for .SPR file (iver=3 has ikeep,nkeep,kstep)
	iver=3
	nkeep=1     !initialize nkeep
	do m=1,30
  		ikeep(1,1,m)=1	!initialise ikeep(1,2,m)=nsamp once defined
	end do
	do i=1,5
		kstep(i)=0
	end do
	mkeep=.false.	!modify ikeep
	jkeep=0		!keep same times for each sweep
	keepall=.true.
			! If sweep protocol read from disk
			! get chance to modify ikeep after 457,
			! but if not read from disk then
			! do not define IKEEP until nsamp defined
			! (done in DEFNSAMP now)
			! Defaults for calibration
	ftape=1.0		!some defaults
	itape=0		!not taped
	errfac=1.0
	gain=10.0
	amVpA1=10.		!mV/pA on patch clamp
	calvolt=10.		!factor by which Vout is greater then true Em
			!Misc defaults
	iDd=1000
	comfac=10.		!List=0.1 for current command signal
	ivhold=-100
	iTPREc=50000	!mus=5ms
	iTSAMP=200000	!mus=200 ms
	ncjump=1
	nvjump=1
	ilenc(1)=50000		!microsec
	itrig=1
	irate=10000		!Hz
	auto=.false.
	vjump=.false.
	control=.false.
	sampv=.false.
	noquery=.false.
	abort=.false.
	newpar=.true.
	first=.true.	!true for 1st time prog entered
	nsweep=1
	idisp=1
	ntch=60		!char in title
	nsch=30		!char in titles
	do i=1,79
  	note(i:i)=' '
	end do
	do i=1,nsch
  		titles(i:i)=' '
	end do
	do i=1,ntch
  		title(i:i)=' '
	end do
	do i=1,30
   		iperm(i)=i
	end do
	randomiz=.false.
	call RANDSK(ix,iy,iz,-1,repeat)
	record=.true.
	readpar=.false.
	print 2005
2005	format(' Switch RECORD on from the start [Y] ? ')
	read 2006,ans
2006	format(a1)
	if(UC(ans).eq.'N') record=.false.	!not on disc initially
			! Set isweep=0 until initial boxes drawn;
			!set to 1 when jump(s) requested
			! by 'JUMP NOW' or 'CONTROL NOW'
	isweep=0	!until after boxes drawn before start of sweep(s)
	swtime=5.	!5 sec between sweeps
  			!See notes in ABORTJ re best way to abort jumps
	fstring=char(27)//',F,'//char(13)//';'	!esc,F,CR to flush 1401 i/o
								!not working properly
	reverse=.false.
	ivdac2=2500
	do i=1,3
		ispec(i)=0	!defaults assigned in SETPOT, unless values read from disc
	end do
			! Define text for all special option keys now;
			! For ispec=2 define also ispec21=-1
			!to display text1; -2 to display text2;
			! and ispec21=1,2 to display them with
			!pos sign not neg (NB sign must always
			! be in text(4:4))
	text1(1)='1. -10 mV     '
	text1(2)='2. -20 mV     '
	text1(3)='3. -30 mV     '
	text1(4)='4. -40 mV     '
	text1(5)='5. -50 mV     '
	text1(6)='6. -60 mV     '
	text1(7)='7. -70 mV     '
	text1(8)='8. -80 mV     '
	text1(9)='9. -90 mV     '
	text1(10)='0.  0 mV      '
	text2(1)='1. -110 mV    '
	text2(2)='2. -120 mV    '
	text2(3)='3. -130 mV    '
	text2(4)='4. -140 mV    '
	text2(5)='5. -150 mV    '
	text2(6)='6. -160 mV    '
	text2(7)='7. -170 mV    '
	text2(8)='8. -180 mV    '
	text2(9)='9. -190 mV    '
	text2(10)='0. -100 mV    '
	ispec21=-1		!initially
	 		!define corresponding values in ival1(10), ival2(10)

	do i=1,9
		ival1(i)=-10*i
		ival2(i)=-10*i-100
	end do
	ival1(10)=0
	ival2(10)=-100
			!Text for ispec=3 keys
	text3(1)='1. 0.2 ms     '
	text3(2)='2. 0.5 ms     '
	text3(3)='3. 1 ms       '
	text3(4)='4. 2 ms       '
	text3(5)='5. 5 ms       '
	text3(6)='6. 10 ms      '
	text3(7)='7. 15 ms      '
	text3(8)='8. 20 ms      '
	text3(9)='9. 30 ms      '
	text3(10)='0. 50 ms      '
	text3(11)='+. 100 ms     '
	text3(12)='*. 200 ms     '
		!and define correponding values (in microsec) in ival3()
	ival3(1)=200		!microsec=0.2 msec
	ival3(2)=500
	ival3(3)=1000
	ival3(4)=2000
	ival3(5)=5000
	ival3(6)=10000
	ival3(7)=15000
	ival3(8)=20000
	ival3(9)=30000
	ival3(10)=50000
	ival3(11)=100000
	ival3(12)=200000
456	continue
	print 2007
2007	format(' Read jump parameters from disc [Y] ? ')


c CJUMP.SPR can be initialised with CJINIT
c Make new param file, CJUMP3.SPR (recl=1024 twice as big as CJUMP2.SPR to allow
c for future developments!) to hold also IVER=version number,amVpA1=mV/pA,
c ftape,gain (for calibration),nsweep,ismode,swtime,swval() for multiple sweeps,
c and PPAR altered to print param for multiple sweeps for approval when read
c from disc.
	read 2006,ans
	if(UC(ans).eq.'N') then
	   	readpar=.false.
	   	mkeep=.false.
	   	goto 457
	endif
	readpar=.true.
4561	continue
      OPEN(unit=17,file='CJUMP3.SPR',status='UNKNOWN',
     & 	access='DIRECT',form='UNFORMATTED',recl=1024)
	read(17,rec=1) npset
4531	continue
	iset=1
	print 2008,npset,iset
2008	format('&Read parameter set number (1 to',i3,') [',i2,'] = ')
	read 4,i
	if(i.ge.1.and.i.le.npset) iset=i
	read(17,rec=iset+1) iver,comfac,ivhold,iTPREc,itsamp,irate,nsamp,
     & nAc,dnAc,ncjump,ilenc,igapc,itrig,vjump,nAv,dnAv,nvjump,iTPREv,
     & ilenv,igapv,ivolt1,ivolt2,amVpA1,ftape,gain,calfac,ndiv1,ndiv2,
     & sampv,calvolt,iDd,ispec,nsweep,swtime,ismode,swval,titles,
     & nkeep,ikeep,kstep,jkeep,tkpre,tkpost
      close(unit=17)

 			!Put title for the protocol read in into
			!the sweep title, as a default title
	n=NBLANK(titles)
	title(1:n)=titles(1:n)
	do i=n+1,ntch
		title(i:i)=' '	!blank the rest
	end do
	if(nsweep.gt.1) then
	   	ispre1=100
	   	ispre2=10
	   	iscount=ifixr(swtime*1000.)
	endif
			!Calc nsamp1 (needed in PPAR2) and JMASK
			!(for multiple double pulse
			!sweeps, but not others,
			!need to recalc jmask for each sweep)
	call CALCNS1(ikeep,nkeep,kstep,nsamp,nsamp1,jmask,keepall,
     & 		ikeep1,1)
			!NB ikeep1 for 1st sweep is defined in CALCNS1
			!iprt,idprt=0,1,2 for no,brief,full
			!print screen, and to disc resp
	print 2009,iset
	if(pon()) write(7,4541) iset
2009	format(/,' SET number ',i4)
	call PPAR2(2,0,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     & calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     & vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     & amVpA1,ftape,gain,nsweep,swtime,ismode,swval,nkeep,ikeep,kstep,
     & jkeep,nsamp1,tkpre,tkpost)
4551	print 2010
2010	format(
     & ' (1) Use present values as they stand',/,
     & ' (2) Modify definition of multiple sweeps',/,
     & ' (3) Modify which points to be kept stored on disc',/,
     & ' (4) Modify any or all of the values',/,
     & ' (5) Read a different set',/,
     & ' Option number [1] = ')
	read 4,iopt
	if(iopt.le.0) iopt=1
	if(iopt.eq.5) goto 456
	if(iopt.eq.1) then
	   	noquery=.true.
	   	goto 59
	else if(iopt.eq.2) then
	   	noquery=.true.	!go on to sweep questions at 457
	else if(iopt.eq.3) then
	   	noquery=.true.
	   	nsmax=2*kmax	!but may be only kmax if controls done!
	   	if(sampv) nsmax=kmax
	   	if(nsamp.gt.kmax.and.nsmax.eq.2*kmax) then	!nsamp=<kmax always OK
			print 2011,nsamp,nsmax,kmax,kmax
2011			format('&Sample has ',i7,' points at present:',/,
     & 			' Maximum number of points = ',i7,
     &			' unless it is wished to show',/,
     & 	'''drug'' and ''control'' traces together in CJFIT in which'
     & 			,/,' case maximum is ',i7,
     & 			': make the maximum ',i7,' [N] ? ')
			read 101,ans
			if(UC(ans).eq.'Y') nsmax=kmax
	   	endif
	   	if(nsweep.gt.1) then
			mkeep=.true.
			iopt1=1
			if(ismode.eq.4) iopt1=2
			print 2012,iopt1
2012			format(
     &		'&(1) Keep the same points for each sweep ',/,
     &	' (2) Keep the same points relative to time of C-jumps',/,
     &	' (3) Keep the same points relative to time of V-jumps',/,
     &    ' (4) Specify the points to be kept separately for each sweep'
     &	,/,' Option number [',i2,'] = ')
			read 4,i
			if(i.ge.1.and.i.le.4) iopt1=i
			if(iopt1.eq.1) then
		   		n=1
		   		jkeep=0
			else if(iopt1.eq.2) then
		   		n=1
		   		jkeep=-1
			else if(iopt1.eq.3) then
		   		n=1
		   		jkeep=-2
			else if(iopt1.eq.4) then
		   		n=nsweep
		   		jkeep=1
			endif
			do m=1,n
		  		call MODKEEP(ikeep,nkeep,kstep,
     &				irate,nsamp,nsamp1,nsmax,m)
	     		end do
	   	else if(nsweep.eq.1) then		!also recalc jmask
			call MODKEEP(ikeep,nkeep,kstep,
     &				irate,nsamp,nsamp1,nsmax,1)
	   	endif
	   	call CALCNS1(ikeep,nkeep,kstep,nsamp,nsamp1,jmask,keepall,
     & 		ikeep1,1)
	   	mkeep=.false.	!reset
	   	if(readpar) goto 4551	!any more modifs?
	endif

457	continue
c MULTIPLE SWEEPS -ask here if they are needed, after param read from disk,
c but before manual reading of params (if they were NOT read from disc). This
c way the correct iDAC etc for 1st sweep can be setup first time through
c (the manual read also downloads iDAC etc).  If sweep parameters read from
c disc then check them here?
c NB for ismode=4 (and =3??) will need to redefine jmask between each sweep

1121	print 2013,nsweep
2013	format(' Number of sweeps to be done [',i3,'] = ')
	read 4,i
	if(i.ge.1) nsweep=i      !otherwise default
	if(nsweep.gt.1) then
	   	print 2014,swtime
2014	   	format('&Time between sweeps (seconds) [',f8.1,'] = ')
	   	call INPUTr(x)
	   	if(x.gt.0.1) swtime=x
	   	ispre1=100
	   	ispre2=10
	   	iscount=ifixr(swtime*1000.)
114	   	print 2015,ismode
2015	   	format(
     &   	' (1) identical sweeps',/,
     &   ' (2) change V-jump potential between sweeps (not with ramps!)'
     &   	,/,' (3) change C-jump length between sweeps',/,
     &   	' (4) change gap between two C-jumps between sweeps',/,
     &   	' Option number [',i2,'] = ')
	   	read 4,i
	   	if(i.ge.1.and.i.le.4) ismode=i
	   	if(ismode.ne.1.and.nsweep.gt.30) then
			call BELL(2)
			print 2016
2016			format(
     &		' Maximum is 30 sweeps, unless sweeps are identical')
			goto 1121
	   	endif
	   	if(ismode.gt.1.and.ismode.le.4) then
	      	do m=1,nsweep
1182				if(ismode.eq.2) print 2017,m,swval(m)
2017				format('&Sweep ',i3,
     &			': V-jump potential (mV) [',f8.1,'] = ')
				if(ismode.eq.3) print 2018,m,swval(m)
2018				format('&Sweep ',i3,
     &			': C-jump length (ms) [',f8.1,'] = ')
				if(ismode.eq.4) print 2019,m,swval(m)
2019				format(
     &			'&Sweep ',i3,': gap between C-jumps (ms) ['
     &			,f8.1,'] = ')
				call INPUTr(x)
				if(ismode.eq.2.and.x.eq.0.) then
		   		 	print 2020
2020		   			format(
     &				'&Confirm that 0 mV required [Y] ? ')
		   			read 2006,ans
		   			if(UC(ans).eq.'N') goto 1182
		   			swval(m)=0.0
				else
	         			if(x.ne.0.0) swval(m)=x
				endif
			end do
	   	endif
				!Now define nsamp1, keepall,
				!jmask (but for ismode=3 will have to redefine
				!jmask between sweeps)
	   	call CALCNS1(ikeep,nkeep,kstep,nsamp,nsamp1,jmask,keepall,
     & 		ikeep1,1)
	   	if(readpar) goto 4551	!any more modifs?
   				!now do setup
	endif			!end of nsweep defs



c NOW SET UP ALL PARAMETERS (when prog first entered, or new parameters
c read by return to 456, nsweep=1 so nothing omitted here: after setting
c up for single sweep them ask if several to be done . If so, unless they
c are all the same, then redo the setup with values for the 1st sweep but
c no queries (later store entire sweep setups eg in CJUMP.SSW file)
c
c Get calibration
c Scaling: 1401 units are -32768 t0 +32752 in 16 unit increments; dividing
c by 16 gives range as -2048 to +2047 (as for CED502) =-5.0V to +4.9976V
c So 32768/5=6553.6 ADC units/volt

59	continue
	if(first.or.newpar) then
	   	print 2021,ifixr(amVpA1),ifixr(gain),errfac
2021	   	format(
     & 	' Calibration: ',i3,' mV/pA; gain= ',i3,
     &	'; error fac = ',f5.1,
     & 	': O.K. [Y] ? ')
	   	read 101,ans
	   	if(UC(ans).eq.'N') then
			ftape=-ftape	!so asks for separate gain etc
			call GETCAL(calfac,amVpA,amVpA1,VpA,VpA1,pAV,pAV1,
     & 		itape,ftape,gain,errfac)
	   	else
			calfac=(1000.*ftape)/(amVpA1*gain*errfac*6553.6)
	   	endif
	endif

	if(nsweep.gt.1.and.ismode.ne.1) then
	   	print 2022
2022	   	format(
     & 	' Randomise multiple sweep sequence [N] ? ')
	   	read 101,ans
	   	if(UC(ans).ne.'Y') then
			do 2023 i=1,30
2023			iperm(i)=i
			randomiz=.false.
	   	else
			randomiz=.true.	!randomization done after 4571
			call RANPERM(iperm,nsweep,30)
			print 2024,(iperm(i),i=1,nsweep)
			if(discprt) write(8,595) (iperm(i),i=1,nsweep)
2024			format(' Random sequence: ',/,30i3)
	   	endif
	endif
c
c
C	THE BIG LOOP
4571	continue
c
c Start timer,mode=0, if nsweep.gt.1 (start it straight after last time period
c ran out -just before the goto 4571 statement)
c No need to start timer before the last jump of series, i.e. if isweep=nsweep
c (as will always be true if single sweep being done). For ismode=1 the clock
c is started after label 42, which is where we loop back to for series
c of identical sweeps. But do not start before the initial boxes put up, ie
c if isweep=0 still

c***********************************************************************
c***********************************************************************
c***********************************************************************

	if(isweep.ge.1.and.isweep.lt.nsweep.and.ismode.ge.2) then
	   	print 125
125	   	format(' TIMER STARTED')
		fmt='(a10,a1,i6,a1,i6,a1,i6,a1)'
		write(str1,fmt) 'TIMER2,C,0',',',ispre1,',',ispre2,',',
     &			iscount,';'
		print*,str1
		call ForSendString(str1,40,IERR)
	endif
c
c If several sweeps being done define values for next sweep and download
c  If a sweep is same as last one (eg set c-jump length to 10,10,20,20,..)
c then set samepar=true for 2nd one, so it gets averaged)
c  For c-jump length also alter DAC rate here if necessary (no provision
c for the 100Hz DAC rate at present)
	samepar=.false.
	if(nsweep.gt.1.and.isweep.ge.1.and.ismode.ge.2) then
         	jswp=iperm(isweep)
	   	if(jswp.gt.1) then
			if(swval(jswp).eq.swval(jswp-1)) samepar=.true.
	   	endif
	   	if(ismode.eq.2) then
			do i=1,nvjump		!assumes no ramps!
		 		ivolt1(i)=int2(ifixr(swval(jswp)))
		 		ivolt2(i)=ivolt1(i)		!to signal jump
			end do
	   	else if(ismode.eq.3) then
			do i=1,ncjump
				ilenc(i)=ifixr(1000.*swval(jswp))		!microsec
			end do
			if(ilenc(1).lt.1000) then
		   		iDd=100		!10kHz DAC rate
			else if(ilenc(1).ge.1000) then
		   		iDd=1000		!1 kHz DAC rate
			endif
	 	else if(ismode.eq.4) then
			do i=1,ncjump-1
		 		igapc(i)=ifixr(1000.*swval(jswp))		!microsec
			end do
	      	call CALCNS1(ikeep,nkeep,kstep,nsamp,nsamp1,jmask,
     &     			keepall,ikeep1,jswp)	!redefine jmask for current sweep
	   	endif
	endif
c
	if(debug()) then
	   dx=1.e3/float(irate)
	   j=0
	   do 707 i=1,nsamp
	   if(jmask(i).eq.0) goto 707
	   j=j+1
	   t=float(i-1)*dx
	   write(8,708) i,j,t
708      format(2i8,g13.6)
707	   continue
	   write(8,709) nsamp,j,nsamp1
709	   format(' nsamp,j,nsamp1 = ',3i8)
	endif
c Define rate for output of values to DACs (1000 Hz normally)
	istout=0		!start address in 1401 memory for DAC pulse
c (define iszout below, when min necessary length of IDAC to be downloaded
c has been found)





	call DEFDRATE(iDd,ipre1,icount1,noquery,nerr)	!set DAC rate







	if(noquery) goto 76
c
c Define total sample length (itsamp now mus)


	call DEFLEN(itsamp)


c
c Define resting membrane pot (and V-jumps if required)
c As long as iDd not changed then no need to call DEFVOLT if anything other
c than V-jumps changed

8	continue

c Set up pulse pattern on DAC2 to control piezo
c As long as iDd not changed then no need to call DEFCONC if anything other
c than C-jumps changed


76	continue

	call ForTo1401(idac,396,0,2,ierr)
c	call ForTo1401(iDAC,iszout,istout,2,ierr)
c	-----------------------------------------
	if(ierr.ne.0) then
	   print 1565,ierr,iszout,istout
1565	   format(' For transfer of DAC pulse shape to 1401:',/,
     &	' ierr= ',i8,' isz = ',i8,' ist = ',i8)
	   call ForClose1401()
	   call EXIT(2)
	else
		print*,'ForTo1401 4:05 OK!'
	endif
c	ani=getch(bani)




	call SETPIEZO(ivdac2,idac2,reverse,noquery)


	call ForTo1401(idac,396,0,2,ierr)
c	call ForTo1401(iDAC,iszout,istout,2,ierr)
c	-----------------------------------------
	if(ierr.ne.0) then
	   print 1555,ierr,iszout,istout
1555	   format(' For transfer of DAC pulse shape to 1401:',/,
     &	' ierr= ',i8,' isz = ',i8,' ist = ',i8)
		print*,'ForTo1401 after setpiezo 4:10 KO'
	   call ForClose1401()
	   call EXIT(2)
	else
		print*,'ForTo1401 after setpiezo 4:10 OK!'
	endif
c	ani=getch(bani)




c
c
	call DEFCONC(ncjump,iTPREc,ilenc,igapc,iDACc,idD,nDc,
     & jclast,noquery,reverse,idac2,nerr,idim)
c	if(nerr.eq.2) goto 8		!need to redefine iTPRE
c
	call SETHOLD(ivhold,comfac,ivhdac,noquery)
      call DEFVOLT(ivhold,comfac,vjump,nvjump,nvramp,ilenv,igapv,ivolt1,
     & ivolt2,iDACv,jvlast,iTPREv,iDd,nDv,noquery,nerr,idim,sampv)
	if(nerr.eq.1) goto 76	!iDd changed so redo c-jump
c	if(nerr.eq.0) goto 8		!need to redefine iTPRE
c Calibration for voltage on ADC1 requested only if V-ramps done
	if(sampv.and.(.not.noquery)) then
	   print 68,ifixr(calvolt)
68	   format(
     & ' mV out from clamp per mV membrane pot (integer) [',i3,'] = ')
	   read 4,i
	   if(i.ne.0) calvolt=float(i)
	endif
c
c Define final array iDAC() for output to DAC2,3, from the iDACv() and
c iDACc(0) defined above
	call DEFDAC(iDAC,iDACc,iDACv,jclast,jvlast,jmax,vjump,
     & control,iszout,idim,idim2)
c Now define start position for ADC data in 1401 as straight after end
c of iDAC() array ie
	ist=istout+iszout+2
c
	if(noquery) then
	   ndiv=ndiv1*ndiv2	!# of 0.25 mus ticks between ADC samples
	   dA=0.25*float(ndiv)	!# of microsec between ADC samples
	   if(sampv) dA=2.0*dA	!for each channel separately
c 	Define ipre,icount
	   ipre=ndiv1
	   icount=ndiv2	!as in DEFADC
	   isz=2*nsamp	!sample size in bytes=mult of 4; as in DEFNSAMP
	   if(sampv) isz=4*nsamp	!for 2 channel sampling
	   goto 62
	endif
c Define ADC sample rate
	call DEFADC(nprime,irate,idD,nDc,nDv,srate,ndiv1,ndiv2,ndiv,
     & dA,nAc,dnAc,nAv,dnAv,ipre,icount,iexact,ncjump,nvjump,sampv,
     & ipre1,icount1,nerr,noquery)
c
c Define number of points in sample ( and isz, for ADCMEM), and also
c which points to be kept on disc
	call DEFNSAMP(kmax,iTSAMP,dA,nsamp,isz,ibad,sampv,
     &	ikeep,nkeep,kstep,jkeep,irate,nsweep,nsamp1)
	call CALCNS1(ikeep,nkeep,kstep,nsamp,nsamp1,jmask,keepall,
     & 	ikeep1,1)
	if(ibad.ne.0) then
	   noquery=.false.
	   goto 8
	endif
c
62	continue



c Download iDAC() to 1401 in memory pos=0-999
c	ierr=To1401(iDAC,iszout4,istout4)
c***********************************************************************

c	ierr=To1401(iDAC,iszout,istout)

c	do i=1,100
c		idac(i)=200+i
c	end do
c	call ForTo1401(idac,396,0,2,ierr)
	call ForTo1401(iDAC,iszout,istout,2,ierr)
c	-----------------------------------------
	if(ierr.ne.0) then
	   print 1665,ierr,iszout,istout
1665	   format(' For transfer of DAC pulse shape to 1401:',/,
     &	' ierr= ',i8,' isz = ',i8,' ist = ',i8)
	   call ForClose1401()
	   call EXIT(2)
	else
		print*,'ForTo1401 OK!'
	endif
	if(debug()) then
		do i=1,368
			if(idac(i).ne.0) print*,i,idac(i)
		end do
		print*,end
	endif
c	ani=getch(bani)
c
c Trigger from Keyboard?
	if(nsweep.gt.1) itrig=2
	if(noquery) goto 74
	print 22,itrig
22	format(
     & ' (1) Trigger externally from event 3,4 inputs',/,
     & ' (2) Trigger internally from keyboard',/,
     & ' Option number [',i2,'] = ')
	read 4,i
4	format(i8)
	if(i.ne.0) itrig=i
74	continue
	if(itrig.eq.1) then
c	   write(10,1005)
c1005	   format('CLEAR;')	!make sure external events enabled
	call ForSendString('CLEAR;',40,IERR)
	print*,'CLEAR ierr=',ierr
c	-----------------------------------
c	ani=getch(bani)
	else
c	   write(10,1002)
c1002	   format('EVENT,D,24;')	!disable external events 3,4
c	   write(10,1003)
c1003	   format('EVENT,M,128;')	!so responds to internal pulse 'EVENT,I'
	call ForSendString('EVENT,D,24;',40,IERR)
c	print*,'EVENT D ierr=',ierr
c	-----------------------------------------
c	ani=getch(bani)
	call ForSendString('EVENT,M,128;',40,IERR)
c	print*,'EVENT M ierr=',ierr
c	------------------------------------------
	endif
c	ani=getch(bani)
	call FLUSH(10)
c
	if(isweep.gt.1) goto 126	!avoid questions!
c
c Set special options
c	if(first.or.newpar) then
	if(first) then
	   call SETOPT(ncjump,nvjump,nvramp,ispec,tspec)
	endif
c
c Title for this sample?
	if(first.or.newpar) then
		call TITENT1(' Enter new title:',TITLE,60,.true.)
	   	call GBLANK(title,60,n1,n2)
	endif
c
126	continue
c Initialise averages, and define title, before 1st sweep
c If there is a series of sweeps, not all identical, then do NOT start
c new average if current sweep is same as last (need check only the
c parameters altered between sweeps, according to value of ismode -
c samepar is set true above if param same)
	if(.not.samepar) call NEWAV(kmax,naver,navc,tcur,tvolt,tcurcon,
     & tvoltcon,control,cnaver,title1,title,n1,n2)		!initialise averages
c
c Print parameters to disc (not screen) before 1st sweep,or after parameters
c changed
	if(first.or.(newpar.and.record)) then
	   newpar=.false.			!NB reset here only if recording, or first!
c         print 1081
         if(pon()) write(7,1081)
         if(discprt) write(8,1081)
1081	   format(/,' NEW PARAMETERS:')
c	   print *,title1
	   if(pon()) write(7,*) title1
	   if(discprt) write(8,*) title1
	   call PPAR2(0,2,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     &   calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     &   vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     &   amVpA1,ftape,gain,nsweep,swtime,ismode,swval,nkeep,ikeep,kstep,
     &   jkeep,nsamp1,tkpre,tkpost)
	endif
c
c SETUP done, now ask if multiple sweeps required, unless set of sweeps
c already started
	first=.false.
	if(isweep.eq.0) goto 214	!get boxes up before the first jump
c


C 	ONE BIG LOOP!!!!!!!!!!!!!!!!!!!!!!!
c RETURN HERE FOR ANOTHER JUMP WITH SAME PARAMETERS
42	continue			!return here for another jump

c
c
c Start timer,mode=0, if nsweep.gt.1
c No need to start timer before the last jump of series, i.e. if isweep=nsweep
c (as will always be true if single sweep being done). Done here for series
c of identical sweeps (but after label 4571 for non-identical sweeps)
c But do not start before the initial boxes put up, ie if isweep=0 still
	if(isweep.ge.1.and.isweep.lt.nsweep.and.ismode.eq.1) then
	   print 125
C	   write(10,1013) ispre1,ispre2,iscount
C1013	   FORMAT('TIMER2,C,0,',i6,',',i6,',',i6,';')
c***********************************************************************
	fmt='(a10,a1,i6,a1,i6,a1,i6,a1)'
	write(str1,fmt) 'TIMER2,C,0',',',ispre1,',',ispre2,',',iscount,';'
c	print*,str1
	call ForSendString(str1,40,IERR)
c	print*,'TIMER2 ierr=',ierr
c---------------------------------------------------------------

C	a=getch(b)
	endif
c
c Invoke MEMDAC command to wait for trigger (arguments are exactly same as
c for ADCMEM)
	irept=1
	if(debug()) print 70,istout,iszout,irept,ipre1,icount1
70	format(' istout,iszout,irept,ipre1,icount1=',/,5i8)
	if(.not.vjump) then					    !DAC2 only for c-jump
c	   	write(10,1009) istout,iszout,irept,ipre1,icount1
c1009	   	FORMAT('MEMDAC,I,2,',i6,',',i6,',2,',i6,',',
c     &    	'HT',',',i6,',',i6,';err')
c     &   	'HT',',',i6,',',i6,';')
		ichanout=2
		fmt='(a10,a1,i6,a1,i6,a1,i6,a1,i6,a1,a2,a1,i6,a1,i6,a1)'
		write(str1,fmt) 'MEMDAC,I,2',',',istout,',',iszout,',',
     &	ichanout,
     &	',',irept,',','HT',',',ipre1,',',icount1,';'
c		print*,str1
		call ForSendString(str1,40,IERR)
c		print*,'MEMDAC ierr=',ierr
c		print*,'memdac err=',ierr
c		a=getch(b)
c	   	read(11,*) e0,e1			! so we wait til done
	else if((vjump.and.control).or.ncjump.eq.0) then	!V-jump only = DAC3 only
		ichanout=3
c	   	write(10,1007) istout,iszout,irept,ipre1,icount1
c1007	   	FORMAT('MEMDAC,I,2,',i6,',',i6,',3,',i6,',',
c     &    	'HT',',',i6,',',i6,';err')

		fmt='(a10,a1,i6,a1,i6,a1,i6,a1,i6,a1,a2,a1,i6,a1,i6,a1)'
		write(str1,fmt) 'MEMDAC,I,2',',',istout,',',iszout,',',
     & 	ichanout,
     &	',',irept,',','HT',',',ipre1,',',icount1,';'
c		print*,str1
		call ForSendString(str1,40,IERR)
c		print*,'MEMDAC ierr=',ierr
c---------------------------------------------------------------

c		print*,'memdac err=',ierr
c		a=getch(b)
c	   	read(11,*) e0,e1			! so we wait til done
	else if(vjump.and.(.not.control).and.ncjump.gt.0) then  !out to DAC2 & 3
c	   	write(10,1000) istout,iszout,irept,ipre1,icount1
c1000	   	FORMAT('MEMDAC,I,2,',i6,',',i6,',2 3,',i6,',',
c     &    	'HT',',',i6,',',i6,';err')

c     &   	'HT',',',i6,',',i6,';')
		fmt='(a10,a1,i6,a1,i6,a1,a3,a1,i6,a1,a2,a1,i6,a1,i6,a1)'
		write(str1,fmt) 'MEMDAC,I,2',',',istout,',',iszout,',',
     &	'2 3',
     &	',',irept,',','HT',',',ipre1,',',icount1,';'
c		print*,str1
		call ForSendString(str1,40,IERR)
c		print*,'MEMDAC ierr=',ierr
c	   	read(11,*) e0,e1			! so we wait til done
	endif
c NB seems that if 'err' command is included then MUST read e0,e1 or get
c problems later
	if(debug().or.e0.ne.0.or.e1.ne.0) then
	   if(KBHIT()) ch=GETCH(ktype)	!remove any waiting character
	   call CLRKB()
	   call BELL(1)
	   print 72,e0,e1
72	   format( ' After MEMDAC: e0,e1 = ',2i8)
	   if(e0.ne.0.or.e1.ne.0) then
	      if(auto.or.(isweep.lt.nsweep)) abort=.true.

c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	      call ABORTJ(reverse,ivdac2,ivhold,comfac,auto,
     &	isweep,pon())
	   endif
	   goto 4571	!reload everything after abort
	endif
c
c Invoke ADCMEM command
	irept=1
c	if(debug()) print 71,ist,isz,irept,ipre,icount
c71	format(' ist,isz,irept,ipre,icount=',/,5i8)
	if(sampv) then		!sample ADC0,1
c***********************************************************************
c	   	write(10,1008) ist,isz,irept,ipre,icount
c1008	   	FORMAT('ADCMEM,I,2,',i6,',',i6,',0 1,',i6,',',
c     &   	'HT',',',i6,',',i6,';err')
c     &   	'HT',',',i6,',',i6,';')
		fmt='(a10,a1,i6,a1,i6,a1,a3,a1,i6,a1,a2,a1,i6,a1,i6,a1)'
		write(str1,fmt) 'ADCMEM,I,2',',',ist,',',isz,',','0 1',
     &	irept,',',
     &	'HT',ipre,',',icount,';'
c		print*,str1
		call ForSendString(str1,40,IERR)
c		print*,'ADCMEM ierr=',ierr
c	   	read(11,*) e0,e1			! so we wait til done
	else				!sample ADC0 only
c	   	write(10,1001) ist,isz,irept,ipre,icount
c1001	   	FORMAT('ADCMEM,I,2,',i6,',',i6,',0,',i6,',',
c     &   	'HT',',',i6,',',i6,';err')
c     &   	'HT',',',i6,',',i6,';')
		fmt='(a11,i6,a1,i6,a3,i6,a4,i6,a1,i6,a1)'
		write(str1,fmt) 'ADCMEM,I,2,',ist,',',isz,',0,',
     &	irept,',HT,',ipre,',',icount,';'
c		print*,str1
		call ForSendString(str1,40,IERR)
c		print*,'ADCMEM ierr=',ierr
c	   	read(11,*) e0,e1			! so we wait til done
	endif
c	a=getch(b)

	call FLUSH(10)
	if(debug().or.e0.ne.0.or.e1.ne.0) then
	   if(KBHIT()) ch=GETCH(ktype)	!remove any waiting character
	   call CLRKB()
	   call BELL(1)
	   print 73,e0,e1
73	   format( ' After ADCMEM: e0,e1 = ',2i8)
	   if(e0.ne.0.or.e1.ne.0) then
	      if(auto.or.(isweep.lt.nsweep)) abort=.true.
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	      call ABORTJ(reverse,ivdac2,ivhold,comfac,auto,
     &	isweep,pon())
	   endif
c	   pause
	   goto 4571	!reload everything after abort
c	   goto 214
	endif
c	if(debug()) pause 'after invoking ADCMEM'
c
	if(itrig.eq.1) then
	   if(auto) then
	   	if(.not.record) call DCMENU(nbox,5,Lb,text,icol1,icol2)	!rec screen
	      call NEWPEN(12)
	      call GTEXT(27,10,
     &	'AUTO-TRIGGER MODE: waiting for external trigger . . .')
		call NEWPEN(4)
	      call GTEXT(28,10,
     &	'(Hit ESC key to leave auto mode)')
	   else
	      print 23
23	      format(' Waiting for external trigger (ESC to abort). . .')
	   endif
	endif
c
	if(itrig.eq.2) THEN
		call ForSendString('EVENT,I,24;',40,IERR)
	print*,'EVENT I ierr=',ierr
	ENDIF
C	write(10,1004)
C1004  format('EVENT,I,24;')	!pulse events 3 and 4 internally
c
c
	j=0
21	continue
c	pause '1'
c***********************************************************************
c	write(10,1006)
c1006	FORMAT ('ADCMEM,?;')
	call ForSendString('ADCMEM,?;',40,IERR)
c	print*,'ADCMEM ?ierr=',ierr
	call ForLongsFrom1401(lval,2,flag4)
	istatus=lval(0)
c		call set_cursor(1,13)
c		print*,'ADCMEM status',status,'pointer',point,
c     &				'(',ibuff(0),ibuff(1),')'
c		print 10, status,point,ibuff(0),ibuff(1)
c10		format('ADCMEM status',i4,'pointer',i8,'(',i8,i8,')')
c		if(status.ne.0.and.point.ne.0) goto 1


c	OK until here!!!!!!!!!!!!!

c	read(11,*) istatus
	do 2 k=1,10000		!insert slight pause to make sure istatus read
2	   s=123456./float(k)
c	write(10,1014) fstring
c1014	format(a6)	!flush 1401 i/o buffers: another go to prevent mystery crash
c	call ForSendString(fstring,40,ierr)
c	print*,'red?'
c	a=getch(b)

c	print*,fstring
c	print*,'fstring ierr=',ierr
c	if(debug()) print 2,j,istatus
c2	format(' status = ',i6,4x,i4)
	j=j+1
	if(KBHIT()) then
	   ch=GETCH(ktype)	!remove the character or NKEY sees it
	   call CLRKB()		!remove any extra char if >1 key hit
	   if(ktype.eq.3) then		!record screen while in auto mode
	      ival=ichar(ch)
		if(ival.eq.48) then     !key 0 to record screen while in auto mode
		   call TIME(ctime)
		   rec1=(control.and.navc.ge.1).or.
     &		(.not.control.and.naver.ge.1)
		   if(.not.record.and.rec1) then !recorded below if RECORD is on
c RECORD DATA ON DISC: record only single sweeps here (ie iADC())- signalled
c by setting iav=0 in call (record average whenever new mean started)
	         call CJDISK3(0,ndev,naver,navc,vjump,control,cdate,ctime,
     &   iADC,iADC1,avcur,avcurcon,avolt,avoltcon,calfac,calvolt,title1,
     &   nAc,dnAc,irate,nsamp,itsamp,ncjump,iTPREc,ilenc,igapc,ivhold,
     &   nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,sampv,nDv,iDd,
     &   ijd,nxtrec,istrec,kmax,kmax2,nfull,nsweep,swtime,isweep,
     &   nkeep,nsamp1,ikeep1,kstep,amVpA1,ftape,gain,errfac)
		   if(nfull.eq.1) then
			ktype=16	!so MEMDAC etc killed below
			iret=1
			goto 254	!kill before getting new disc
		   endif
		   if(auto) then		!put up message now
			call INTCONV(ijd,chs)
			call NEWPEN(12)
			call GTEXT(1,63,'Swp #'//charnb(chs)//' recorded')
		   else
			recscn=.true.	!so message put up in LPLOTCJ
		   endif
c and print brief details
		   if(pon()) write(7,*) title1
		   if(discprt) write(8,*) title1
	         call PPAR2(0,1,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     &   calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     &   vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     &   amVpA1,ftape,gain,nsweep,swtime,ismode,swval,nkeep,ikeep,kstep,
     &   jkeep,nsamp1,tkpre,tkpost)
		   endif		!end of print/record
		endif
	   endif
254	   continue
c	If ESC hit then abort
	   if(ktype.eq.16) then	!ESC
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c		write(10,1010) astring		!all in ABORTJ now
c1010		format(a6)	!reset 1401
		abort=.true.
		if(debug()) print 802,istatus
802		   format(' BEFORE ABORT istatus = ',i5)
	      call ABORTJ(reverse,ivdac2,ivhold,comfac,auto,
     &	  isweep,pon())
		if(nfull.eq.1) goto 25
c		goto 214
		goto 4571	!reload everything after abort
	   endif
	endif
	if(istatus.ne.0.and.istatus.ne.-1) goto 21

c	bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb

	CALL ForToHost(IADC,ISZ,IST,2,IERR)
	if(ierr.ne.0.or.debug()) then
	   print 3993,isz,ist,ierr
3993	   format(
     & ' For transfer to host of ',i8,' bytes starting at ',i8,
     & ' ierr= ',i8)
	   if(auto.or.(isweep.lt.nsweep)) abort=.true.
	   call ABORTJ(reverse,ivdac2,ivhold,comfac,auto,
     &	isweep,pon())
	   call CLRKB()
	   pause
	else
		print*,'ForToHost OK!'
	endif
c
	if(istatus.eq.-1) then
	   call BELL(3)
	   print 92
92	   format(' SAMPLES MISSED -SAMPLING TOO FAST')
	   pause
	endif
c
	if(control) then
	   navc=navc+1	!'control'		!skip this if jump aborted
	   call INTCONV(navc,cnaver)
	else
	   naver=naver+1	!'drug'
	   call INTCONV(naver,cnaver)
	endif
	title1=title(n1:n2)//':  #'//cnaver
c
c	if(debug()) then
c	   print 91,j
c91	   format(' status checked ',i5,' times')
c	   pause 'Sample taken in 1401'
c	endif
c
c Transfer data to host (NB tested for completion within Bergel's TOHOST code)
c	INTEGER*2 FUNCTION ToHost(object,size,addr1401)
c At present upload all points (to new bigger array), rather than each IKEEP
c section separately (upload all gives chance to take every nth point
c in 'non-keep' part, rather than rejecting entirely)
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c	print*,'red?'
c	a=getch(b)
C	ierr=ToHost(iADC,isz,ist)
c		call ForToHost(ibuff,10,0,2,flag5)

c	bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
	CALL ForToHost(IADC,ISZ,IST,2,IERR)

c	-----------------------------------
	if(ierr.ne.0.or.debug()) then
	   print 3,isz,ist,ierr
3	   format(
     & ' For transfer to host of ',i8,' bytes starting at ',i8,
     & ' ierr= ',i8)
	   if(auto.or.(isweep.lt.nsweep)) abort=.true.
	   call ABORTJ(reverse,ivdac2,ivhold,comfac,auto,
     &	isweep,pon())
	   call CLRKB()
	   pause
	else
		print*,'ForToHost OK!'
	endif

	if(debug()) then
333	   print 331,isz/2
331	   format(' Number of integers transferred = ',i8,/,
     & '  print iADC,iDACv(i1) to (i2) [0 to end]; i1,i2 = ')
	   read 31,i1,i2
	   if(i1.eq.-1) then
			do i=1,368
				if(iadc(i).ne.0) print*,idac(i)
			end do
			print*,end
			goto 991
	   endif
	   if(i1.le.0) goto 991
	   do 332 i=i1,i2
	   print 3321,i,iADC(i),iDACv(i)
3321	   format(3i8)
	   if(mod(i-i1+1,24).eq.0) then
		print 411
		call ANYKEY()
	   endif
332	   continue
	   goto 333
991	   continue
	endif
c	pause
c
c NB if SAMPV is true iADC() will contain ADC0,ADC1 interleaved; if so
c separate them now (if keepall there is no need to use jmask, but must
c still allocate iUPLOAD to iADC
	if(sampv) then
	   j=0
	   do 69 i=2,2*nsamp,2    !i=2,4,6,...,2*nsamp
	   if(jmask(i/2).eq.0) goto 69	!skip this point
	   j=j+1                 !j=1,2,3,...,nsamp
	   iADC1(j)=iADC(i)      !i=2,4,6,...,2*nsamp; voltage
	   iADC(j)=iADC(i-1)	 !i-1=1,3,5,...,2*nsamp-1; current=ADC0
c=========OK for iADC?
69	   continue
	else
	   j=0
	   do 692 i=1,nsamp
	   if(jmask(i).eq.0) goto 692	!skip this point
	   j=j+1                 !j=1,2,3,...,nsamp
	   iADC(j)=iADC(i)      !i=2,4,6,...,2*nsamp; voltage
692	   continue
	endif
c
c	if(debug()) then
c32	   print 33,nsamp
c33	   format(' nsamp = ',i8,/,
c     & '  print iADC,iADC1 and iDACv(i1) to (i2) [0 to end]; i1,i2 = ')
c	   read 31,i1,i2
31	   format(2i8)
c	   if(i1.le.0) goto 99
cc	   n=i2-i1+1
c	   do 40 i=i1,i2
c	   if(.not.sampv) then
c	      print 5,i,iADC(i)
c5	      format(1x,2i8)
c	   else
c	      print 51,i,iADC(i),iADC1(i),iDACv(i)
c	      if(discprt) write(8,51) i,iADC(i),iADC1(i),iDACv(i)
c51	      format(1x,4i8)
c	   endif
c	   if(mod(i-i1+1,24).eq.0) then
c		print 411
411		format(' Hit any key to continue')
c		call ANYKEY()
c	   endif
c40	   continue
c	   goto 32
c	endif
c
c Print everything after sample taken (but not on screen)
99	continue
c print final values used
	call TIME(ctime)
	rec1=(control.and.navc.ge.1).or.(.not.control.and.naver.ge.1)
	if(record.and.rec1) then !print brief details for jump and keep on disc
c        print 108
c         if(pon()) write(7,108)
c         if(discprt) write(8,108)
108	   format(/)
c
c RECORD DATA ON DISC: record only single sweeps here (ie iADC())- signalled
c by setting iav=0 in call (record average whenever new mean started)
	  call CJDISK3(0,ndev,naver,navc,vjump,control,cdate,ctime,
     &   iADC,iADC1,avcur,avcurcon,avolt,avoltcon,calfac,calvolt,title1,
     &   nAc,dnAc,irate,nsamp,itsamp,ncjump,iTPREc,ilenc,igapc,ivhold,
     &   nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,sampv,nDv,iDd,
     &   ijd,nxtrec,istrec,kmax,kmax2,nfull,nsweep,swtime,isweep,
     &   nkeep,nsamp1,ikeep1,kstep,amVpA1,ftape,gain,errfac)
	   if(nfull.eq.1) then
		iret=1
		goto 25
	   endif
c and print brief details
c	   print *,title1
	   if(pon()) write(7,*) title1
	   if(discprt) write(8,*) title1
	   call PPAR2(0,1,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     &   calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     &   vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     &   amVpA1,ftape,gain,nsweep,swtime,ismode,swval,nkeep,ikeep,kstep,
     &   jkeep,nsamp1,tkpre,tkpost)
	endif		!end of print/record
	call SAVEDP		!save disc print out
c Accumulate total and mean separately for current and voltage (if measured)
c and separately for 'drug' and control (=control=v-jump without c-jump)
	en=float(naver)
	enavc=float(navc)		!navc=number of controls (control) averaged
	do 150 i=1,nsamp1
	if(control) then
	   tcurcon(i)=tcurcon(i)+float(iADC(i))
	   avcurcon(i)=tcurcon(i)/enavc
	else
	   tcur(i)=tcur(i)+float(iADC(i))	!no calfac,so scale as for iADC
	   avcur(i)=tcur(i)/en
	endif
	if(.not.sampv) goto 150
	if(control) then
	   tvoltcon(i)=tvoltcon(i) + float(iADC1(i))
	   avoltcon(i)=tvoltcon(i)/enavc
	else
	   tvolt(i)=tvolt(i) + float(iADC1(i))
	   avolt(i)=tvolt(i)/en
	endif
150	continue
c
c	if(debug()) pause		!temp fix to see print out before graph
	call VIDEOMOD(3)		!redraw completely for each jump
214	continue			!return here to start graphics from scratch
	idev=0
	if(videotyp().ne.18) then
	   call INIPLT(idev,.false.,1.0)	!start Hgraph
	   imode=18
	   i2=0
	   call PLOTS(0,i2,imode)		!start Lahey graphics
	endif
c Plot data with Lahey screen routines
	call LPLOTCJ3(naver,navc,iADC,avcur,avcurcon,nsamp,nAc,dnAc,
     & title1,ijd,itsamp,nAv,dnAv,dA,ncjump,ilenc,igapc,vjump,control,
     & nvjump,ilenv,igapv,record,kmax,kmax2,abort,mkeep,ikeep1,kstep,
     & jmask,nsamp1,recscn)
	if(nsweep.gt.1.and.isweep.ge.1.and.ismode.ge.2) then
	   call INTCONV(ifixr(swval(jswp)),chs)
	   n=NBLANK(chs)
	   if(ismode.eq.2) chs1=chs(1:n)//' mV'
	   if(ismode.eq.3.or.ismode.eq.4) chs1=chs(1:n)//' ms'
	   call NEWPEN(13)
	   call GTEXT(3,70,chs1)
	endif
c
c Now redraw boxes (unless in auto mode, or doing series of jumps that has
c not yet finished)
c If autotrigger mode is in effect then go straight to next jump
	if(auto) goto 42		!do next jump without drawing boxes
c
c If nsweep>0 then do next sweep without drawing boxes
c Read timer2 in loop until flag is set (timer not set before last sweep)
	if(nsweep.gt.1.and.isweep.ge.1.and.isweep.lt.nsweep) then
	   i=0
121	   continue
	   i=i+1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C	   write(10,1012)
C1012	   FORMAT('TIMER2,R,0;')
	     CALL ForSendString('TIMER2,R,0;',40,IERR)
c	print*,'TIMER2 R ierr=',ierr
c	   read(11,*) iflag
	   if(i.eq.1.and.iflag.eq.1) then
		call BELL(2)
		print 120
120		format(
     &  ' CYCLE TOO SHORT - time already elapsed at end of cycle')
	   endif
C	   if(iflag.eq.0) goto 121		!wait until time up
	   if(ierr.eq.0) goto 121
c
c Want to restart clock as soon as possible after prev time period has
c elapsed, ie at label 42 or 4571
   	   isweep=isweep+1
	   if(isweep.le.nsweep) then
		if(ismode.eq.1) then	!identical sweeps so restart clock here
		   goto 42		!another identical sweep
		else
	         noquery=.true.		!so no stops for questions
		   goto 4571		!load up for next cycle
		endif
	   endif
	endif
c
213	continue			!return here to start boxes from scratch
c
c Put up boxes
	isweep=0	!reset isweep=0 (whether single sweep or series)
	newpar=.false.	!NB reset here for next sweep
	ifont=2		!simplex (on 0-11 scale)
	csize=2.0
	nbox=10
	call SETLBOX(nbox,Lb,1)
212	continue
	call NUMSET			!set num lock on ready for response
c	call DCMENU(0,4,Lb,text,0,0)		!delete all
	if(.not.vjump) then
	   Lb(2)=0
	   Lb(8)=0
	endif
	if(ispec(1).eq.0) Lb(9)=0		!see SETOPT
	if(ispec(2).eq.0) Lb(10)=0
	if(record) then
	   if(ispec(1).eq.6) Lb(9)=0
	   if(ispec(2).eq.6) Lb(10)=0
	endif
	if(itrig.eq.1) then
	  if(vjump) then
	   text(1)='1.AGONIST JUMP'
	  else
	   text(1)='1. DO JUMPS   '
	  endif
	else
	  if(vjump) then
	   text(1)='1.AGONIST JUMP'
	  else
	   text(1)='1.AGONIST NOW '
c	   text(1)='1.JUMP  NOW   '
	  endif
	endif
	if(vjump.and.itrig.eq.1) then
c	   text(2)='2.V-JUMP ONLY '
	   text(2)='2.CONTROL JUMP'
	else if(vjump.and.itrig.eq.2) then
	   text(2)='2.CONTROL NOW '
	endif
	text(3)='3.AUTO-TRIGGER'
	text(4)='4.NEW TITLE'
	text(5)='5.NEW MEAN '
	if(.not.record) then
	   text(6)='6. RECORD  '
	else
	   text(6)='6.NO RECORD'
	endif
	text(7)='7.PARAMS /EXIT'
	text(8)='8.SHOW V-JUMP '
	text(9)(1:14)=tspec(1)
	text(10)(1:14)=tspec(2)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
211	ikey=nkey()
	goto(201,202,203,204,205,206,207,208,209,209) ikey
	goto 211		!no valid key
c
c Another jump. If not vjump then do it straight away. If vjump then
c do BOTH c-jump and V-jump now.  If 'control' has been set true earlier
c then must redefine iDAC() here so that both done.
201	continue
	isweep=1		!jump, or series of jumps, now started
	if(randomiz.and.nsweep.gt.1) then
	   call RANPERM(iperm,nsweep,30)
	   print 595,(iperm(i),i=1,nsweep)
	   if(discprt) write(8,595) (iperm(i),i=1,nsweep)
c595	   format(' Random sequence: ',/,30i3)
	endif
	if(.not.vjump) goto 421		!another identical jump
	if(vjump.and.(.not.control)) goto 421		!another identical jump
	control=.false.	!reset 'control'; output to DACs 2 AND 3
	call NEWPEN(0)
	call GTEXT(1,0,'         ')
	call NEWPEN(10)
	call GTEXT(1,0,'C-JUMP')
	goto 2021		!redefine iDAC() for 2 DAC channels and download it
c
c Do V-jumps only, no C-jumps
202	continue
	isweep=1		!jump, or series of jumps, now started
	if(.not.vjump) goto 211
	if(control) goto 421		!already set for v-jump only
	control=.true.		!reset 'control', then redefine iDAC()
	call NEWPEN(0)
	call GTEXT(1,0,'         ')
	call NEWPEN(13)
C	call GTEXT(1,0,'NO C-JUMP')
	call GTEXT(1,0,'CONTROL')
2021	continue
	call DEFDAC(iDAC,iDACc,iDACv,jclast,jvlast,jmax,vjump,
     & control,iszout,idim,idim2)
c Now define start position for ADC data in 1401 as straight after end
c of iDAC() array ie
	ist=istout+iszout+2
c     Download altered iDAC() to 1401 in memory pos=0-999
	ierr=To1401(iDAC,iszout,istout)
	if(ierr.ne.0) then
	   print 651,ierr
651	   format(' For transfer of DAC pulse shape to 1401: ierr= ',i8)
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@''
		CALL ForClose1401()
c	-------------------
c	   call LabEnd
	   call EXIT(2)
	endif
c	goto 42	!next jump
421	continue
	isweep=1
	if(nsweep.eq.1) then
	   goto 42		!another identical sweep
	else if(nsweep.gt.1) then
	   if(ismode.eq.1) then	!identical sweeps so restart clock here
		goto 42		!another identical sweep
	   else
	      noquery=.true.		!so no stops for questions
		goto 4571		!load up for next cycle
	   endif
	endif
c
c Auto-trigger mode
203	continue
	if(nsweep.gt.1) then
	   call VIDEOMOD(3)
	   call BELL(4)
	   print 2031
2031	   format(
     & ' At present set up for multiple sweeps: before entering AUTO',/,
     & ' mode, please set up a single-sweep protocol')
	   noquery=.false.
	   newpar=.true.
	   nsweep=1
	   goto 456		!read new parameters
	endif
	call BELL(1)
	print 35
35	format(
     &' ENTERING AUTO-TRIGGER MODE.',/,
     &' In this mode a jump will be done each time external trigger',/,
     &' is given, but system is hung while waiting for trigger. To',/,
     &' escape from auto-trigger mode, so other options are',/,
     &' accessible, hit ESC key while waiting for trigger.')
	itrig=1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C	write(10,1005)
c1005	format('CLEAR;')	!make sure external events enabled
	CALL ForSendString('CLEAR;',30,IERR)
	print*,'CLEAR ierr=',ierr
c	-------------------------------------
	if(.not.record) then	!chance to record event on screen anyway
	   call SETLBOX(0,Lb,1)
	   Lb(10)=1
	   call NUMSET			!set num lock on ready for response
	   call DCMENU(0,4,Lb,text,0,0)		!delete all
	   text(10)='0.RECORD SWEEP'
	   call DCMENU(nbox,5,Lb,text,icol1,icol2)
	endif
c Insert SETPIEZO here because problems encountered with Piezo moving
c when entering auto mode!
	call SETPIEZO(ivdac2,idac2,reverse,.true.)
	auto=.true.
	goto 42
c
c New title:
204	continue
	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
	call TITENT1(' Enter new title:',TITLE,60,.false.)
	call GBLANK(title,60,n1,n2)
	title1=title(n1:n2)//':  #'//cnaver
c	Lb(2)=-1
	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
	goto 214	!redraw completely
c	goto 212
c
c Start new mean. Record current average first; set iav=1 in call to
c indicate that average to be recorded (unless it is average of 1 only)
205	continue
	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
	rec1=(control.and.navc.ge.1).or.(.not.control.and.naver.ge.1)
	if(record.and.rec1) then
	  call CJDISK3(0,ndev,naver,navc,vjump,control,cdate,ctime,
     &   iADC,iADC1,avcur,avcurcon,avolt,avoltcon,calfac,calvolt,title1,
     &   nAc,dnAc,irate,nsamp,itsamp,ncjump,iTPREc,ilenc,igapc,ivhold,
     &   nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,sampv,nDv,iDd,
     &   ijd,nxtrec,istrec,kmax,kmax2,nfull,nsweep,swtime,isweep,
     &   nkeep,nsamp1,ikeep1,kstep,amVpA1,ftape,gain,errfac)
	   if(nfull.eq.1) then
		iret=2
		goto 25
	   endif
	endif
	call NEWAV(kmax,naver,navc,tcur,tvolt,tcurcon,tvoltcon,
     &  control,cnaver,title1,title,n1,n2)		!initialise averages
c	goto 42
c	Lb(3)=-1
c	goto 212
	goto 2141	!redraw completely
c
206	continue
	record=.not.record
	call NEWPEN(0)
	call GTEXT(0,70,'         ')        !delete existing message
	if(record) then
	   call NEWPEN(12)
	   call GTEXT(0,70,'RECORDING')
	else
	   call NEWPEN(14)
	   call GTEXT(0,70,'NO RECORD')
	endif
c	Lb(4)=-1
	goto 212
c
c Display V-jumps
208	continue
	if(.not.vjump) goto 211		!invalid choice
	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
111	print 110,naver,navc,idisp
110	format(
     & ' (1) Use the current ''drug'' and ''control'' (means of ',i3,','
     &       ,i3,' sweeps)',/,
     & ' (2) Specify which sweeps to be subtracted (read from disc)',/,
     & ' Option number [',i2,'] = ')
	read 4,i
	if(i.eq.1.or.i.eq.2) idisp=i
	if(idisp.eq.1) then
	   call VJDISP3(avcur,avcurcon,avolt,avoltcon,nsamp,nvjump,
     &   naver,navc,ilenv,igapv,ivolt1,ivolt2,ivhold,calfac,calvolt,
     &   sampv,nAv,dnAv,dA,title1,itsamp,kmax,jmask,nsamp1,keepall,
     &   vjump,ncjump,nAc,dnAc,ilenc,igapc)
	else if(idisp.eq.2) then
	   call VJDSPRD3(ndev)
	else
	  goto 111
	endif
	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
c	call CLS			!clear screen (still in graphics?)
	goto 214			!redraw all graphics
c
c Change sampling params
207	continue
c First print current values (all details, screen only)
c iprt,idprt=0,1,2 for no,brief,full print screen, and to disc resp
	call LOCATE(0,0)
	call PPAR2(2,0,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     & calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     & vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     & amVpA1,ftape,gain,nsweep,swtime,ismode,swval,nkeep,ikeep,kstep,
     & jkeep,nsamp1,tkpre,tkpost)
	print 85
85	format(' Hit ESC to return without changing anything')
	nbox=10
	call SETLBOX(nbox,Lb,1)
	if(ispec(3).eq.0) Lb(10)=0	!see SETOPT
400	continue
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
c	text(1)='1.ADC SAMPLE  '
c	text(2)='2.SPECIAL OPT '
	text(1)='1.MORE OPTIONS'
	text(2)='2.ADC SAMPLE  '
	text(3)='3.C-JUMP PULSE '
	text(4)='4.V-JUMP/RAMP  '
	if(itrig.eq.2) then
	   text(5)='5.EXT TRIGGER '
	else
	   text(5)='5.KEYBRD TRIG '
	endif
	text(6)='6.CALIBRATION '
	text(7)='7.READ PARAMS '
	text(8)='8.SAVE PARAMS '
	text(9)='9.EXIT PROGRAM '
c	text(10)='10.PIEZO VOLTS '
	text(10)(1:14)=tspec(3)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
311	ikey=nkey()
	if(ikey.eq.-16) goto 310		!ESC key
c Record current average and initialise new one for keys=2,3,4,6,7
c For key 10 ispec=: 1=holding pot; 2=v-jump pot; 3=c-jump length;
c 4=piezo (DAC2) volts; 5=calibration; 6=record screen (when RECORD off)
c so should record current average an start new one if ispec=1,2,3,4,5
c options on 10 need new average at present).  Set iav=1 in call to
c indicate that average to be recorded (unless it is average of 1 only)
312	continue
	if(ikey.eq.2.or.ikey.eq.3.or.ikey.eq.4.or.ikey.eq.6.or.
     & ikey.eq.7.or.(ikey.eq.10.and.ispec(3).ne.6)) then
c===	  call VIDEOMOD(3)		!utility lib- this makes graph go for good!
	  rec1=(control.and.navc.ge.1).or.(.not.control.and.naver.ge.1)
	  if(record.and.rec1) then
	  call CJDISK3(0,ndev,naver,navc,vjump,control,cdate,ctime,
     &   iADC,iADC1,avcur,avcurcon,avolt,avoltcon,calfac,calvolt,title1,
     &   nAc,dnAc,irate,nsamp,itsamp,ncjump,iTPREc,ilenc,igapc,ivhold,
     &   nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,sampv,nDv,iDd,
     &   ijd,nxtrec,istrec,kmax,kmax2,nfull,nsweep,swtime,isweep,
     &   nkeep,nsamp1,ikeep1,kstep,amVpA1,ftape,gain,errfac)
	   if(nfull.eq.1) then
		iret=3
		goto 25
	   endif
	  endif
	  call NEWAV(kmax,naver,navc,tcur,tvolt,tcurcon,tvoltcon,
     &  control,cnaver,title1,title,n1,n2)		!initialise averages
	endif
	if(ikey.eq.10) then
	   goto (2091,210,87,309,306,801) ispec(3)
c ispec=: 1=holding pot; 2=v-jump pot; 3=c-jump length; 4=piezo (DAC2) volts;
c 5=calibration; 6=record screen (when RECORD off)
	endif
	goto(301,302,303,304,305,306,307,308,999,309) ikey
	goto 311	!no valid key
c
c Record last jump as single sweep (iav=0) when RECORD is off (this is done
c separately, after KBHIT, when in auto mode)
801	continue
	rec1=(control.and.navc.ge.1).or.(.not.control.and.naver.ge.1)
	if(.not.record.and.rec1) then
	   call CJDISK3(0,ndev,naver,navc,vjump,control,cdate,ctime,
     &   iADC,iADC1,avcur,avcurcon,avolt,avoltcon,calfac,calvolt,title1,
     &   nAc,dnAc,irate,nsamp,itsamp,ncjump,iTPREc,ilenc,igapc,ivhold,
     &   nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,sampv,nDv,iDd,
     &   ijd,nxtrec,istrec,kmax,kmax2,nfull,nsweep,swtime,isweep,
     &   nkeep,nsamp1,ikeep1,kstep,amVpA1,ftape,gain,errfac)
	   recscn=.true.		!so message put up in LPLOTCJ
	   if(nfull.eq.1) then
		iret=1
		goto 25
	   endif
c and print brief details
	   if(pon()) write(7,*) title1
	   if(discprt) write(8,*) title1
	   call PPAR2(0,1,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     &   calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     &   vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     &   amVpA1,ftape,gain,nsweep,swtime,ismode,swval,nkeep,ikeep,kstep,
     &   jkeep,nsamp1,tkpre,tkpost)
	endif		!end of print/record
	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
	goto 214	!redraw completely
c
310	continue			!ESC hit
c	goto 212
	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
	goto 214	!redraw completely
c
301	continue
C 3RD LEVEL MENU
	call SETLBOX(2,Lb,1)
	continue
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	text(1)='1.PRINT NOTE  '
	text(2)='2.SPECIAL KEYS'
C	text(3)='3.            '
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
	ikey=nkey()
	if(ikey.lt.1.or.ikey.gt.2) goto 301		!invalid key
	goto(401,402) ikey
c
c Print an message on printer/disc print file
401	continue
	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
	if(pon()) write(7,4011) note
	if(discprt) write(8,4011) note
4011	format(' =============================================',/,
     & 1x,a79,/,
     &  ' =============================================')
	goto 214	!redraw completely
c
c Change special option ?
402	continue
	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
	call SETOPT(ncjump,nvjump,nvramp,ispec,tspec)
	call SETLBOX(10,Lb,1)
cc	goto 212
	goto 214	!redraw completely
c
302	continue
c Redefine sample length and rate
	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
81	continue
	noquery=.false.
	call DEFLEN(itsamp)
	call DEFADC(nprime,irate,idD,nDc,nDv,srate,ndiv1,ndiv2,ndiv,
     & dA,nAc,dnAc,nAv,dnAv,ipre,icount,iexact,ncjump,nvjump,sampv,
     & ipre1,icount1,nerr,noquery)
	if(nerr.eq.1) then
	   noquery=.true.
	   newpar=.true.
	   goto 4571	!reload everything
	endif
	call DEFNSAMP(kmax,iTSAMP,dA,nsamp,isz,ibad,sampv,
     &	ikeep,nkeep,kstep,jkeep,irate,nsweep,nsamp1)
	call CALCNS1(ikeep,nkeep,kstep,nsamp,nsamp1,jmask,keepall,
     & 	ikeep1,1)
	if(ibad.ne.0) goto 81
	goto 2141	!redraw completely
c
303	continue
c Redefine c-jump DAC pulse
83	continue
	noquery=.false.
	call DEFCONC(ncjump,iTPREc,ilenc,igapc,iDACc,idD,nDc,
     & jclast,noquery,reverse,idac2,nerr,idim)
c	if(nerr.eq.2) goto 83		!need to redefine iTPRE
	noquery=.true.
c may need to redefine nAc etc
	call DEFADC(nprime,irate,idD,nDc,nDv,srate,ndiv1,ndiv2,ndiv,
     & dA,nAc,dnAc,nAv,dnAv,ipre,icount,iexact,ncjump,nvjump,sampv,
     & ipre1,icount1,nerr,noquery)
	newpar=.true.
	goto 4571	!reload everything (whether DAC rate changed or not)
c
304	continue
c Redefine V-jump DAC pulse
	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
84	continue
	noquery=.false.
      call DEFVOLT(ivhold,comfac,vjump,nvjump,nvramp,ilenv,igapv,ivolt1,
     & ivolt2,iDACv,jvlast,iTPREv,iDd,nDv,noquery,nerr,idim,sampv)
c	if(nerr.eq.2) goto 84	!need to redefine iTPRE
	noquery=.true.
	newpar=.true.
	goto 4571	!reload everything (whether DAC rate changed or not)
c
305	continue
	if(itrig.eq.1) then
	   itrig=2
C	   write(10,1002)
c1002	   format('EVENT,D,24;')	!disable external events 3,4
C	   write(10,1003)
c1003	   format('EVENT,M,128;')	!so responds to internal pulse 'EVENT,I'
	call ForSendString('EVENT,D,24;',40,IERR)
	print*,'EVENT ierr=',ierr
c	------------------------------------------
	call ForSendString('EVENT,M,128;',40,IERR)
	print*,'EVENT 1867 ierr=',ierr
c	------------------------------------------
	else
	   itrig=1
C	   write(10,1005)
c1005	   format('CLEAR;')	!make sure external events enabled
	call ForSendString('CLEAR;',40,IERR)
	print*,'CLEAR 1874 ierr=',ierr
c	------------------------------------------
	endif
	call FLUSH(10)
	call SETLBOX(10,Lb,1)
c	Lb(5)=-1
	goto 212
c
c New calibration factor
306	continue
	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
	ftape=-ftape	!so asks for separate gain etc
	call GETCAL(calfac,amVpA,amVpA1,VpA,VpA1,pAV,pAV1,
     & itape,ftape,gain,errfac)
c Calibration for voltage on ADC1 requested only if V-ramps done
	if(sampv) then
	   print 68,ifixr(calvolt)
c68	   format(
c     & ' mV out from clamp per mV membrane pot (integer) [',i3,'] = ')
	   read 4,i
	   if(i.ne.0) calvolt=float(i)
	endif
	goto 2141	!redraw completely
c
c Read parameters from .SPR file (start new average)
307	continue
	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
	noquery=.false.
	newpar=.true.
	nsweep=1
c	goto 4561		!read .SPR file
	goto 456		!read new parameters
c
c Save parameters
308	continue
	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
	if(.not.vjump) nvjump=0
c      OPEN(unit=17,file='D:\CJUMP3.SPR',status='UNKNOWN',
      OPEN(unit=17,file='CJUMP3.SPR',status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=1024)
	read(17,rec=1) npset
	iset=npset+1	!default
	print 451,iset
451	format(' Save as set number [next set =',i3,'] = ')
	read 4,i
	if(i.ne.0) iset=i
	if(iset.gt.npset) npset=npset+1
	write(17,rec=1) npset
	write(17,rec=iset+1) iver,comfac,ivhold,iTPREc,itsamp,irate,nsamp,
     & nAc,dnAc,ncjump,ilenc,igapc,itrig,vjump,nAv,dnAv,nvjump,iTPREv,
     & ilenv,igapv,ivolt1,ivolt2,amVpA1,ftape,gain,calfac,ndiv1,ndiv2,
     & sampv,calvolt,iDd,ispec,nsweep,swtime,ismode,swval,titles,
     & nkeep,ikeep,kstep,jkeep,tkpre,tkpost
	close(unit=17)
      print 452,iset
      if(pon()) write(7,452) iset
      if(discprt) write(8,452) iset
452	format(' Parameters saved as set number ',i4)
	call SETLBOX(10,Lb,1)
	goto 214	!redraw completely
c
309	continue	!set piezo voltage
	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
	call SETPIEZO(ivdac2,idac2,reverse,.false.)
c   Redefine iDACc with new piezo voltage
	noquery=.true.
	newpar=.true.
	goto 4571	!reload everything (whether DAC rate changed or not)
c
c NOW SPECIAL OPTIONS keys 9,10 -both go to to 209 initially
c ispec=: 1=holding pot; 2=v-jump pot; 3=c-jump length; 4=piezo (DAC2) volts;
c 5=calibration; 6=record screen (when RECORD off). Start new average
c for ispec=1-5 (not for ispec=6) -could also record current average (as
c at label 312) but this not generally useful so skip here)
209	continue
	if(ikey.eq.9) then
	   if(ispec(1).ne.6) call NEWAV(kmax,naver,navc,tcur,tvolt,
     &   tcurcon,tvoltcon,control,cnaver,title1,title,n1,n2) !initialise means
	   goto (2091,210,87,309,306,801) ispec(1)
	endif
	if(ikey.eq.10) then
	   if(ispec(2).ne.6) call NEWAV(kmax,naver,navc,tcur,tvolt,
     &   tcurcon,tvoltcon,control,cnaver,title1,title,n1,n2) !initialise means
	   goto (2091,210,87,309,306,801) ispec(2)
	endif
c
c Set holding potential
2091	continue
	noquery=.false.
	call SETHOLD(ivhold,comfac,ivhdac,noquery)
	noquery=.true.
	newpar=.true.
	goto 4571	!reload everything (whether DAC rate changed or not)
c
210	continue
c ispec=2. Option to change voltage (of step, not holding pot) in case
c where there is a single V-step
c Define text for keys; ispec21=-1 to display text1; -2 to display text2;
c and ispec21=1,2 to display them with pos sign not neg
c	if(nvjump.ne.1) goto 212	!not valid
c  Define ibig,isign for current ispec21 value (in case it is not reset below)
	isign=1
	if(ispec21.lt.0) isign=-1
	ibig=0
	if(iabs(ispec21).eq.2) ibig=1
500	continue
c	if(debug()) print 871,ikey,ibig,isign,ivolt1(1),ispec21,
c     & (ival1(i),i=1,10),(ival2(i),i=1,10)
	nbox=12
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	do 520 i=1,10
	   if(iabs(ispec21).eq.1) then
		text(i)(1:14)=text1(i)
		if(int4(ivolt1(1)).eq.ival1(i)) Lb(i)=-1	!current value in italic
	   else if(iabs(ispec21).eq.2) then
		text(i)(1:14)=text2(i)
		if(int4(ivolt1(1)).eq.ival2(i)) Lb(i)=-1	!current value in italic
	   endif
	   if(ispec21.gt.0) text(i)(4:4)='+'	!otherwise leave as neg sign
520	continue
	if(iabs(ispec21).eq.1) then
	    text(11)='*. >100 mV    '
	else if(iabs(ispec21).eq.2) then
	    text(11)='*. <100 mV    '
	endif
	if(ispec21.lt.0) then
	   text(12)='+.Positive    '
	else
	   text(12)='+.Negative    '
	endif
	call DCMENU(nbox,6,Lb,text,icol1,icol2)
	ch=GETCH(ktype)	!<ENTER> to accept existing title
	ival=ichar(ch)
c Deal with '+' and '*' keys; they just redraw keys
	if((ktype.eq.3.and.ival.eq.43).or.(ktype.eq.2.and.ival.eq.42))then
	   if(ktype.eq.3.and.ival.eq.43) then	!'+' key
		isign=1
		if(ispec21.gt.0) isign=-1		!'+' key labelled neg
	   else						!'*' key
		ibig=0					!0-100
		if(iabs(ispec21).eq.1) ibig=1		!100-200 '*' key labelled >100
	   endif
	   ispec21=1		!reset it
	   if(ibig.eq.1) ispec21=2
	   ispec21=ispec21*isign
c Reset values in ival1,ival2 to current sign
	   do 507 i=1,10
		ival1(i)=isign*iabs(ival1(i))
		ival2(i)=isign*iabs(ival2(i))
507	   continue
	   goto 500		!redraw boxes
	endif
c Now deal with voltage keys (1-10)
	ikey=ival-48	!=0,1,..,9
	if(ktype.ne.3.or.ikey.lt.0.or.ikey.gt.9) goto 500	!invalid key
	ivolt1(1)=int2(ikey*10)		!positive
	if(ibig.eq.1) ivolt1(1)=ivolt1(1)+100
	ivolt1(1)=int2(isign)*ivolt1(1)
	ivolt2(1)=ivolt1(1)       !to signify V-jump, not ramp
c Redraw selected box in italic
	call DCMENU(0,4,Lb,text,0,0)		!delete all
c	call DELBOX(Lblast)	!delete all drawn at last DRAWBOX call
	nbox=12
	call SETLBOX(nbox,Lb,1)
	if(ikey.eq.0) ikey=10
	Lb(ikey)=-1
	call DCMENU(nbox,6,Lb,text,icol1,icol2)
c	if(debug()) print 871,ikey,ibig,isign,ivolt1(1),ispec21,
c     & (ival1(i),i=1,10),(ival2(i),i=1,10)
c871	format(' ikey,ibig,isign,ivolt1,ispec21 = ',5i5,/,
c     & ' ival1= ',10i6,/,' ival2 = ',10i6)
c now reset iDACv,iDAC and download iDAC (noquery=true)
	noquery=.true.
	newpar=.true.
	goto 4571	!reload everything (whether DAC rate changed or not)
c
c Alter c-jump length with special key
c Modified 03/18/91 03:36pm so ALL cjumps (if>1) changed
87	continue
c	if(ncjump.ne.1) goto 212	!not valid
	call SETLBOX(12,Lb,1)
c If current c-jump length is one of the values on display put the current
c value in italics
	do 501 i=1,12
	if(ilenc(1).eq.ival3(i)) Lb(i)=-1
501	continue
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	do 502 i=1,12
	   text(i)(1:14)=text3(i)
502	continue
	call DCMENU(nbox,6,Lb,text,icol1,icol2)
504	ch=GETCH(ktype)	!<ENTER> to accept existing title
	ival=ichar(ch)
	if(ktype.eq.3.and.ival.eq.43) then
	   ikey=11					!'+'=11 key (microsec)
	else if(ktype.eq.2.and.ival.eq.42) then	!'*'=12 key
	   ikey=12
	else if(ktype.eq.3.and.(ival.ge.48.and.ival.le.57)) then	!0-9 keys
	   ikey=ival-48
	   if(ikey.eq.0) ikey=10
	else
	   goto 212		!no valid key
	endif
	il=ival3(ikey)
c check length
	if(il+iTPREc.gt.itsamp) then
	   call BELL(1)
	   print 503,itprec/1000,il,itsamp/1000
503	   format(
     & ' Delay (',i5,' ms) plus c-jump (',i5,
     & ' ms) is longer than ADC sample (',i5,' ms)')
	   goto 504
	endif
c Reset length, and DAC rate if necessary (all sorts of pos problems e.g.
c iDd might be set too long if there is a very short v-jump, or vice versa)
c 06/26/91 12:03pm Next bit altered -may not want to change DAC rate to 1000
c when cjump length.ge.1000, eg if short delay (<1ms) required before the
c the c-jump, so now check delay = itPREc (microsec) too
	do 505 i=1,ncjump
505	ilenc(i)=il
c Redraw selected box in italic
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	nbox=12
	call SETLBOX(nbox,Lb,1)
	Lb(ikey)=-1
	call DCMENU(nbox,6,Lb,text,icol1,icol2)
c	nerr1=0		!iDd not altered
c	if(il.lt.1000.and.iDd.ge.1000) then
	if((il.lt.1000.or.itPREc.lt.1000).and.iDd.ge.1000) then
c	   nerr1=1		!iDd altered
	   call BELL(1)
	   iDd=100
	   print 5031
5031	   format(' DAC rate changed to 10 kHz')
c	else if(il.ge.1000.and.iDd.eq.100) then
	else if(il.ge.1000.and.itPREc.ge.1000.and.iDd.eq.100) then
c	   nerr1=1		!iDd altered
	   call BELL(1)
	   iDd=1000
	   print 5032
5032	   format(' DAC rate changed to 1 kHz')
	endif
c and redefine and download everything
	noquery=.true.
	newpar=.true.
	goto 4571	!reload everything (whether DAC rate changed or not)
c
c Print all details of new parameters to disc, after one or more
c of them has been changed, before re-displaying
2141	continue
	call SETLBOX(10,Lb,1)
	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
	if(record) then
         print 1081
         if(pon()) write(7,1081)
         if(discprt) write(8,1081)
c1081	   format(/,' NEW PARAMETERS:')
c	   print *,title1
	   if(pon()) write(7,*) title1
	   if(discprt) write(8,*) title1
	   call PPAR2(0,2,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     &   calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     &   vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     &   amVpA1,ftape,gain,nsweep,swtime,ismode,swval,nkeep,ikeep,kstep,
     &   jkeep,nsamp1,tkpre,tkpost)
	endif
	goto 214	!redraw completely
c
c====Next ispec option
c
999	continue
	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
c	CALL LabEnd			!end of CJUMP
	call ForClose1401()
c	------------------------------------------
	call RANDSK(ix,iy,iz,1,repeat)	!keep last ix,iy,iz
	call ENDPRINT		!close printer and discprint files
9999	continue
	print 9991
9991	format(' REMEMBER TO TURN OFF PIEZO POWER BEFORE THE CED1401',/,
     & ' Have you done it [N] ? ')
	call BELL(10)
	read 101,ans
	if(UC(ans).ne.'Y') goto 9999
	end



