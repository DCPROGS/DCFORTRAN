	program CJUMP4
c===========================================================================
c 25.01.95:
c If you start the program with Caps Lock on you have the oportunity
c to change the name for *.dat file and *.prt file !
c 20.01.95:
c 1. nostep taken out
c 2. qdialog FIXED!!!!!
c 3. 15 boxes !!!!!
c
c 19.X.94
c CJUMP4 is the latest version of CJUMP3M with POPMENUS & DIALOG BOXES
c CJUMP4 is new version of CJUMP3M with larger (allocatable) data arrays
c (NB use of allocatable data arrays allows both dimensions of data array
c  to be passed to VPLOT in CJFIT -see TDIM ans TMEM programs).
c  (1) Now make all arrays the same size, regardless of whether to be subtracted
c      or not, but make them allocatable
c  (2) Now JMASK made integer*1 to save space (also allocatable)
c  (3) In graphics mode throughout: use dialog box etc i/o
c
c To set colour for Hgraph using the Lahey graphics/IV graphics colour numbers
c Lahey/Ioana colours:
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c For HP550C plotting colours should be
c 15=black  (NB black/white interchanged compared with screen numbering)
c 14=yellow; 13=magenta; 12=red; 11=cyan; 10=green; 9=blue; 8=white; 7=white
c and 0-6 are same as 8-14.
c
c CJUMP3M: Lahey V5.n version with Ioana's 1401 commands 20.08.93
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
c In this version whole sample is uploaded which allows option to keep
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

c     DECLARATIONS:
c============================================================================
	ALLOCATABLE::avcur,avcurcon,tcur,tcurcon
	ALLOCATABLE::avolt,avoltcon,tvolt,tvoltcon
	ALLOCATABLE::iADC,iADC1,jmask
	ALLOCATABLE::iDaC,iDaCc,idacv
	real*4    avcur(:),avcurcon(:),tcur(:),tcurcon(:)
	real*4    avolt(:),avoltcon(:),tvolt(:),tvoltcon(:)
	real*4    swval(30) 		!values that change between sweeps
	integer*1 jmask(:)
 	integer*2 iADC(:)
	integer*2 iADC1(:)		!for voltage on ADC1, if req
 	integer*2 ierr
	integer*2 iDAC(:)		!for DAC output
	integer*2 iDACc(:),iDACv(:)   !for DAC2 (c-jump) and DAC3 (v-jump)
	integer*2 ivolt1(10),ivolt2(10),ivhold  !pots for each V jump (integer mV)
	integer*2 ivhdac			!ivhold in DAC units
	integer*2 ivhclamp,ipatch
	integer*2 istrec(250)		! First record # for CJDISK
	integer*2 istatus
	integer*2 videotyp
      integer*2 kstep(5),i2keep(4,2,30)
	integer*4 ival1(10),ival2(10),ival3(12)
	integer*4 nprime(1900)	!holds primes up to 16381 (see PRIMGEN.FOR)
	integer*4 Lb(18),ispec(5)
	integer*4 ikeep(4,2,30),ikeep1(4,2)
	integer*4 iperm(30)
	integer*4 ispre1,ispre2,iscount,iflag	!for timing sweeps
	integer*4 lval(0:3)
	integer*4 ist,isz,irept,ipre,icount		!args for ADCMEM
	integer*4 istout,iszout,ipre1,icount1	!args for MEMDAC
	integer*4 ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
	integer*4 ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	integer*4 iramp
	character*1  e0,e1,ch,getch,ans,UC,buff(0:20),charout
	character    cans*30,def*30
      character 	 cdate*11,ctime*11,cnaver*11,datfil*24,datfilj*24
	character 	 title*60,title1*70,ndev*2,titles*30,note*79
	character 	 chs*11,chs1*14
	character*24  datfili
	character*11 clk,helps(10)
      character*10 getint
	character*13 getreal,titlew
	character*14 text1(12),text2(12),text3(12)
	character*40 mtitle*40,filnam*32,prtport*4	!for WINPRINT
	character*60 fmt
	character*72 str1,que(10),wtext
	character*78 text(18)
	logical 	discprt,pon,slock,debug,caplock,abort,newpar,newfile
	logical 	record,vjump,control,noquery,sampv,auto,kbhit,reverse
	logical 	rec1,first,samepar,repeat,randomiz,mkeep,keepall
	logical 	readpar,mono,recscn,readini,present
	logical 	plus,menu_flag		!=true if 1401-plus connected
	logical 	mouse_on,mous_set,sprec
	logical 	openb1,openb2,openb3,openb4,openb5
	logical 	openb6,openb7,openb8,openb9,openb10,out_menu


c For DCMENU:
c==============================================================================
	common/DMENU/csizb,ifonb,csize,ifont2,nboxlast,nblast
	common/hgv/ivxlo,ivxhi,ivylo,ivyhi,ixlo,ixhi,iylo,iyhi
	common/fixswp/ismode,nsweep,swval,itPREc,itPREv,ncjump,nvjump,
     & ilenc,igapc,ilenv,igapv,tkpre,tkpost,jkeep		!for MODKEEP
	common/lscal/sx,sy,xoff,yoff		!for Lahey graphics
	common/lgrf/xpos,ypos,ipen		!ditto
	common/rand/ix,iy,iz
	common/inter/plus		!for LOAD.FOR (plus=true if 1401-plus connected)
	common/dp/discprt
	common/dpp/filnam,prtport,ndisc,jcol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	common/mousval/mouse_on,nbutton
	common/popvalx/mxlo,mylo,myhi,ictx,ibkx,icfx	!values for poptext calls
	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls
	common/popvalt/lxlo,lylo,lyhi,ictt,ibkt,icft,icht	!values for poptable calls
	common/dialval1/ictd,icfd,icbd  !colours for dialog box #1 (text,frame,back)
	common/datpos/yd1,yd2,iyd1,iyd2	!position of data area
c
	pon()=slock()
	debug()=caplock()
	call SETMOUSE()		!define values in common\mousval\ (in IVLIB)
	mous_set=.false.	!initialise
	menu_flag=.false.
	mouse_on=.true.
	iszmax0=64000

c     defs for popmenu/poptext

	ixlow=200
	iyhiw=400
	iylow=-1
	ic=15
	icf=8
	icup=15
	ibkw=4
	nhelp=1
	titlew='OPTIONS'

c     defs for DIALOG:

	openb1=.false.
	openb2=.false.
	openb3=.false.
	openb4=.false.
	openb5=.false.
	openb6=.false.
	openb7=.false.
	openb8=.false.
	openb9=.false.
	openb10=.false.
	ibk=0
	iclo=2
	ncol=72
	irhi=3
	irhimin=5
	nrow=24
	nrowmin=10
	nrow5=3
	icqd=14
	icwd=11
	icb1=1
	icb2=2
	icb3=3
	icb4=4
	icb5=5
	icb6=6
	icb7=7
	icb8=13
	icb9=9
	icb10=10
      call DEFDIALOG(1,irhi,iclo,nrow,ncol,ibk)
	do l=2,10
         call DEFDIALOG(l,irhimin,iclo,nrowmin,ncol,ibk)
	enddo
      call DEFDIALOG(5,irhimin,iclo,nrow5,ncol,ibk)

c 	defs for DCMENU:
	nboxlast=10		!in case attempt to delete boxes before any drawn
	nblast=4		!ditto
	icol1=14	!yellow text/border for boxes in DCMENU
	icol2=8	!grey background for boxes
	icol3=12	!red text/border
	icol4=15	!white fill
	ifonb=0
	csizb=1.7
	csize=2.
	ivxlo=0		! for VIEWPORT
	ivxhi=10000
	ivylo=0
	ivyhi=7000
	ixlo=0		! for graphboundary
	ixhi=10000
	iylo=0
	iyhi=7000

c Define text for all special option keys now;
c For ispec=2 define also ispec21=-1 to display text1; -2 to display text2;
c and ispec21=1,2 to display them with pos sign not neg (NB sign must always
c be in text(4:4))

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
	ispec21=-1		!initiall

c and define corresponding values in ival1(10), ival2(10)

	do i=1,9
		ival1(i)=-10*i
		ival2(i)=-10*i-100
	end do
	ival1(10)=0
	ival2(10)=-100

c Text for ispec=3 keys

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

c and define correponding values (in microsec) in ival3()

	ival3(1)=200	   !microsec=0.2 msec
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
	ivdac2=4000
c**************************************************************************

c	PROGRAM STARTs:

	 datfili='CJUMP.DAT'
	 filnam ='CJUMP.PRT'


	call WINPRINT	!print file control
	call CAPCLR

	OPEN(unit=7,file=prtport,iostat=nerr)

	readini=.false.
	INQUIRE(file='CJUMP.INI',exist=present,flen=len)
	if(present.and.len.eq.1024) then
	   readini=.true.
         OPEN(unit=19,file='CJUMP.INI',status='UNKNOWN',
     &   access='DIRECT',form='UNFORMATTED',recl=512)
	   read(19,rec=1) ndev,ipatch,ffilt,ivdac2,datfili,filnam
	   CLOSE(unit=19)
	   if (datfili.eq.' ') datfili='CJUMP.DAT'
	   if (filnam.eq.' ') filnam='C:\CJUMP.PRT'
	endif

	call mode(18)
	call clears(0,480,0)
	ibox=2
	call opendialog(ibox,icb5,.true.)
	openb2=.true.
	CALL WDIALOG(2,'OPTIONS FOR FILES NAMES (WITHOUT EXTENTION)',
     &ICWD)
	call wdialog(2,'PRINT FILE WILL HAVE SAME NAME AND SEND ON H:'
     &,icwd)
	call wdialog(2,'IF YOU PRESS ENTER, THE DEFAULT WILL BE CHOOSED',
     &ICWD)
765	continue
	nc=nblank1(datfili)
	call qdialog(2,'Name for files ',
     &' ['//datfili(1:nc-4)//'] = ',icqd,cans)
	if(cans.eq.' ') goto 654
	nc1=nblank1(cans)
	if(nc1.le.20) then
		datfili=cans(1:nc1)//'.DAT'
	      nc=nblank1(datfili)
		goto 654
	endif
	call wdialog(2,'YOU ENTERED AN INVALID FILE NAME',icwd)
	goto 765
654	continue
	filnam=filnam(1:3)//datfili(1:nc-4)//'.PRT'
	np=nblank1(filnam)
	call wdialog(2,'Data file:'//datfili(1:nc),icwd)
	call wdialog(2,'Print file:'//filnam(1:np),icwd)
	ans=getch(b)

	openb2=.false.
	call clrdialog(2,0)
	call enddialog(2,0)

	call INIPLT(idev,.false.,1.0)	!start Hgraph
	imode=18
	i2=1
	call PLOTS(0,i2,imode)		!start Lahey graphics
	ibox=1
      call opendialog(ibox,icb1,.true.)
	openb1=.true.
	call wdialog(1,'CJUMP -Concentration and V-jump program'
     & ,icwd)
	if(pon()) write(7,1)
	if(discprt) write(8,1)
	call DATE1(cdate)		!DC subroutine
	ctime=clk()
	if(pon()) write(7,2225) cdate,ctime(1:8),mtitle
	if(discprt) write(8,2225) cdate,ctime(1:8),mtitle
	call wdialog(1,'Date of analysis: '//cdate//' Time of analysis: '
     &   //ctime(1:8),icwd)
	call wdialog(1,'Machine = '//mtitle,icwd)
	call wdialog(1,'SCROLL LOCK on for printing',icwd)
	call wdialog(1,'CAPS LOCK on for debugging',icwd)
	call wdialog(1,'Sample current on ADC0',icwd)
	call wdialog(1,'Sample voltage on ADC1 (for V-ramps only)',icwd)
	mono=jcol.eq.0	!monochrome screen
	if(mono) then
	   icol3=15		!white text/border
	   icol4=0		!black fill
	endif

c 	Open 1401 :

88	call ForOpen1401(ierr)
	if(ierr.ne.0) then
	   call wdialog(ibox,'1401 OPEN FAILED : ERR = '//
     &   getint(ierr,nm)(1:nm),12)
	   select case (ierr)
	      case (-500)
		   call wdialog(ibox,'1401 SWITCHED OFF ; '//
     &	   'PLEASE SWITCH ON and PRESS ANYKEY',12)
		   ans=getch(b)
		   ind5=ind5+1
		   if(ind5.ge.3) goto 9888
		   goto 88
		case (-501)
		   call wdialog(ibox,'1401 NOT CONNECTED: CHECK AND RESTART'
     &         ,12)
		case (-502)
		   call wdialog(ibox,'1401 ILL ( CALL THE DOCTOR ? ): '//
     &	   'SWITCH OFF/ON 1401 AND REBOOT ',12)
		case (-503)
		   call wdialog(ibox,'I/F CARD MISSING: CHECK AND RESTART',
     &	   12)
		case (-505)
		   call wdialog(ibox,'I/F CARD BAD SWITCHES: '//
     &	   'CHECK AND RESTART ',12)
		case (-506)
		   call wdialog(ibox,'1401+ FAILED TO COME READY : '//
     & 	   'SWITCH OFF/ON 1401 AND REBOOT',12)
		case (-508)
		   call wdialog(ibox,'1401 IS ALREADY IN USE : PLEASE WAIT',
     &         12)
		   call ForClose1401()
		   indwait=indwait+1
		   if(indwait.gt.15) then
	   		call wdialog(ibox,'SORRY , EXIT PROGRAM !',12)
			call EXIT(2)
		   endif
		   goto 88
		case (-509)
		   call wdialog(ibox,'COULD NOT GET DMA CHANNELS : '//
     &	   'SWITCH OFF/ON 1401 AND REBOOT',12)
		case(-580)
		call wdialog(ibox,'NOT 386 ENHANCED MODE: CHECK AND RESTART'
     &	,12)
		case(-581)
		   call wdialog(ibox,'NO DEVICE DRIVER : CHECK AND RESTART',
     &         12)
		case(-582)
		   call wdialog(ibox,'DEVICE DRIVER TOO OLD : NO COMMENTS !'
     &         ,12)
	   end select
9888	   call wdialog(ibox,'SORRY , EXIT PROGRAM !',12)
	   call EXIT(2)
	else
	   call wdialog(ibox,'1401-PLUS OPENED SUCCESFULLY',10)
	endif

c Load commands that are needed
c NB the array IWORK used in Ld() is temporary work space only- needs to be
c about 1.5*size of the largest command loaded.  No need to use separate
c array (iwork here)- could use say iADC() array which can be set to 0 after
c Ld() call ready for its real use to hold sampled data.  This saves space.

888	call ForLd('c:\1401\','KILL,MEMDAC,ADCMEM,TIMER2',ies,ierr)
	if(ierr.ne.0) then
	   call wdialog(ibox,'1401 COMMAND LOAD FAILED'//
     &   getint(ierr,nm)(1:nm),12)
		if(ierr.eq.(-540)) call wdialog(ibox,
     &	   'COMMAND FILE NOT FOUND',12)
		if(ierr.eq.(-590)) then
		   call wdialog(ibox,'TIMEOUT OCCURED ! WAIT : TRYING AGAIN'
     &         ,12)
		   ind=ind+1
		   if(ind.ge.3) goto 8888
		   goto 888
		endif
		call wdialog(ibox,'ADVISE: SWICH OFF/ON 1401 AND REBOOT',12)
8888	   	continue
	      call wdialog(ibox,'SORRY , EXIT PROGRAM !',12)
		call ForClose1401()
	   	call EXIT(2)
	else
	   	call wdialog(ibox,'1401 COMMAND LOAD OK!',10)
	endif
	ind=0

c Set size of ADC data array
	kmax=51200
	call defolti(kmax,def)
	call qdialog(1,'Size of data arrays ',def,icqd,cans)
	call getinpi(cans,kmax)

	idimc=5000
c	call defolti(idimc,def)
c	call qdialog(1,'Size of DAC2 : c-jump ',def,icqd,cans)
c	call getinpi(cans,idimc)
	call wdialog(1,'Size of DAC2 : c-jump '//
     &     getint(idimc,nm)(1:nm),icqd)

	idimv=5000
c	call defolti(idimv,def)
c	call qdialog(1,'Size of DAC3 : v-jump ',def,icqd,cans)
c	call getinpi(cans,idimv)
	call wdialog(1,'Size of DAC3 : v-jump '//
     &     getint(idimv,nm)(1:nm),icqd)

	if (idimc.ge.idimv) then
		idim2=2*idimc
	else
		idim2=2*idimv
	endif

c  Allocate the allocatable arrays (keep size in .SPR?)

	ALLOCATE(iDaCc(idimc),iDaCv(idimv),idac(idim2))

c	idim=5000	!max size of iDACc and iDACv arrays- up to 5 sec at 1 kHz
c	idim2=10000	!max size of iDAC array

c Prepare to record results on disc

	nfull=0
	ijd=0		!# of jumps recorded so far
	njd=0		!no jumps on disc (unless specified below)
	nxtrec=2	! record #1 holds njd,nxtrec,istrec
	do i=1,250
		istrec(i)=0		!initialise
	end do
25	continue
	call DISCDIALOG(1,' Disk partition for '//datfili(1:nc)
     & ,idisc,ndev,0,1,icqd)
	if(idisc.eq.-1) goto 9999	!if ESC hit in discnum
	datfilj=ndev//'\'//datfili
	nc=nblank1(datfilj)
	call wdialog(1,'Files in use : '//datfilj(1:nc)//' and '//
     & filnam(1:np),icwd)
	INQUIRE(file=DATFILJ,exist=present)

	if(present) then
         OPEN(unit=14,file=DATFILJ,status='UNKNOWN',
     &   access='DIRECT',form='UNFORMATTED',recl=512)
	   read(14,rec=1) njd,nxtrec,ISTREC
	   call BELL(2)
	   wtext=datfilj(1:nc)//' already exists; contains '//
     &   getint(njd,nm)(1:nm)
     &  //' jumps: (room for '//getint(250-njd,nm)(1:nm)//' more)'
	   call wdialog(1,wtext,icwd)
         que(1)=' (1) Append results to existing file '
         que(2)=' (2) Overwrite the existing file'
         que(3)=' (3) Use a different disc'
	   n=3
	   iopt=1
	   nhelp=1
	   call POPMENU(ixlow,iylow,iyhiw,que,n,ic,icf,icup,ibkw,
     &   titlew,helps,nhelp,iopt,charout,ival)
	   if(iopt.le.0) iopt=1
	   if(iopt.eq.1) then
		ijd=njd	!use nxtrec,istrec as read from disc
		if(debug()) print*,'ijd=',ijd
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
	endif   	!end of if present

	CLOSE(unit=14)

c Carry on after choosing new disk when one filled up
	if(nfull.eq.1) goto (99,205,3012) iret

c NB recl is in bytes (7600 bytes=1900 integer*4)

      OPEN(unit=13,file='PRIMES.DAT',status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=7600)
	read(13,rec=1) nprime
      CLOSE(unit=13)

c zero input array to hold ADC data

	if(debug()) print*,'before iadc'

	if(debug()) print*,'after iadc'
c Scaling: 1401 units are -32768 t0 +32752 in 16 unit increments; dividing
c Define defaults (later could read choice of these from a file)
c Initialise nkeep
	nkeep=1
	do 27 m=1,30
27	ikeep(1,1,m)=1	!initialise ikeep(1,2,m)=nsamp once defined
	do 28 i=1,5
28	kstep(i)=0
	mkeep=.false.	!modify ikeep
	jkeep=0		!keep same times for each sweep
	keepall=.true.

c If sweep protocol read from disk get chance to modify ikeep after 457,
c but if not read from disk then do not define IKEEP until nsamp defined
c (done in DEFNSAMP now)
c Defaults for calibration

	ftape=1.0		!some defaults
	itape=0		!not taped
	errfac=1.0
	gain=10.0
	amVpA1=10.		!mV/pA on patch clamp
	calvolt=10.		!factor by which Vout is greater then true Em

c Misc defaults

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
	if(debug()) print*,'before record'
	call RANDSK(ix,iy,iz,-1,repeat)
	record=.true.
	readpar=.false.
	ans='Y'
	call defolta(ans,def)
	call qdialog(1,'Switch RECORD on from the start ',def,
     & icqd,cans)
	call getinpa(cans,ans)
	if(UC(ans).eq.'N') record=.false.	!not on disc initially

	call wdialog(1,'Enter patch type ',icwd)
	j1=ipatch
	if(ipatch.eq.0) j1=1
	que(1)='(1) outside-out'
	que(2)='(2) inside-out'
	que(3)='(3) cell-attached'
	que(4)='(4) whole-cell'
	que(5)='(5) simulated data'
	nop=5
	titlew='Options'
	i=j1
c	ival=j1
	nhelp=1
	call POPMENU(ixlow,-1,iyhiw,que,nop,ic,icf,icup,ibkw,
     &titlew,helps,nhelp,i,charout,ival)
	if(i.ge.1.and.i.le.5) j1=i
	ipatch=int2(j1)
	titlew='Options'
	call wdialog(1,'Patch type :'//que(j1),icwd)
	call defoltr(ffilt,def)
	call qdialog(1,'Filter setting [Hz] ',def,icqd,cans)
	call getinpr(cans,ffilt)

c Set isweep=0 until initial boxes drawn; set to 1 when jump(s) requested
c by 'JUMP NOW' or 'CONTROL NOW'

	isweep=0	!until after boxes drawn before start of sweep(s)
	swtime=5.	!5 sec between sweeps

c See notes in ABORTJ re best way to abort jumps

	reverse=.false.
	do i=1,3
		ispec(i)=0	!defaults assigned in SETPOT, unless values read from disc
	end do

c************************************************************************

456	continue

	if(.not.openb1) then
		ibox=1
      	call opendialog(1,icb1,.true.)
		openb1=.true.
	endif
	iver=4
	call defolti(iver,def)
	call qdialog(1,'Version number for *.SPR file ',def,icqd,cans)
	call getinpi(cans,iver)
	datfil='CJUMP4.SPR'
	lrec=2048
	if(iver.eq.3) then
		datfil='CJUMP3.SPR'
		lrec=1024
	endif
	call wdialog(1,'READ/WRITE '//datfil,icwd)
	INQUIRE(file=DATFIL,exist=sprec)
	if(.not.sprec) then
	   readpar=.false.
	   mkeep=.false.
	   ALLOCATE(avcur(kmax),avcurcon(kmax),tcur(kmax),tcurcon(kmax))
	   ALLOCATE(avolt(kmax),avoltcon(kmax),tvolt(kmax),tvoltcon(kmax))
	   ALLOCATE(iADC(kmax),iADC1(kmax),jmask(kmax))
	   do i=1,kmax
		iADC(i)=-1
	   end do
	   goto 457
	endif
	ans='Y'
	call defolta(ans,def)
	call qdialog(1,'Read jump parameters from disc ',def,
     &icqd,cans)
	call getinpa(cans,ans)
c CJUMP.SPR can be initialised with CJINIT
c Make new param file, CJUMP3.SPR (recl=1024 twice as big as CJUMP2.SPR to allow
c for future developments!) to hold also IVER=version number,amVpA1=mV/pA,
c ftape,gain (for calibration),nsweep,ismode,swtime,swval() for multiple sweeps,
c and PPAR altered to print param for multiple sweeps for approval when read
c from disc.
c Version number for .SPR file (iver=3 has ikeep,nkeep,kstep)

	if(UC(ans).eq.'N') then
	   readpar=.false.
	   mkeep=.false.
	   ALLOCATE(avcur(kmax),avcurcon(kmax),tcur(kmax),tcurcon(kmax))
	   ALLOCATE(avolt(kmax),avoltcon(kmax),tvolt(kmax),tvoltcon(kmax))
	   ALLOCATE(iADC(kmax),iADC1(kmax),jmask(kmax))
	do i=1,kmax
		iADC(i)=-1
	end do
	   goto 457
	endif
	readpar=.true.

4561	continue

      OPEN(unit=17,file=datfil,status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=lrec)
	read(17,rec=1) npset
4531	continue
	iset=1
	i=1
	call defolti(iset,def)
	wtext='Read parameter set number (1 to '//getint(npset,nm)(1:nm)//
     &' )'
	call qdialog(1,wtext,def,icqd,cans)
	call getinpi(cans,i)
	if(i.ge.1.and.i.le.npset) iset=i

1111  continue
      if(iver.eq.3) then
	   read(17,rec=iset+1) iver,comfac,ivhold,iTPREc,itsamp,irate,
     &   nsamp,
     &   nAc,dnAc,ncjump,ilenc,igapc,itrig,vjump,nAv,dnAv,nvjump,iTPREv,
     &   ilenv,igapv,ivolt1,ivolt2,amVpA1,ftape,gain,calfac,ndiv1,ndiv2,
     &   sampv,calvolt,iDd,ispec,nsweep,swtime,ismode,swval,titles,
     &   nkeep,i2keep,kstep,jkeep,tkpre,tkpost,iramp,ivhclamp
         do i=1,4
	      do j=1,2
	         do k=1,30
		      ikeep(i,j,k)=int4(i2keep(i,j,k))
	         enddo
	      enddo
	   enddo
	   goto 2001
	else
	   read(17,rec=iset+1) iver,comfac,ivhold,iTPREc,itsamp,irate,
     &   nsamp,
     &   nAc,dnAc,ncjump,ilenc,igapc,itrig,vjump,nAv,dnAv,nvjump,iTPREv,
     &   ilenv,igapv,ivolt1,ivolt2,amVpA1,ftape,gain,calfac,ndiv1,ndiv2,
     &   sampv,calvolt,iDd,ispec,nsweep,swtime,ismode,swval,titles,
     &   nkeep,ikeep,kstep,jkeep,tkpre,tkpost,iramp,ivhclamp
	endif
2001	close(unit=17)
	if(nsamp.gt.kmax) kmax=nsamp+1200
c  Allocate the allocatable arrays (keep size in .SPR?)
	ALLOCATE(avcur(kmax),avcurcon(kmax),tcur(kmax),tcurcon(kmax))
	ALLOCATE(avolt(kmax),avoltcon(kmax),tvolt(kmax),tvoltcon(kmax))
	ALLOCATE(iADC(kmax),iADC1(kmax),jmask(kmax))
	do i=1,kmax
		iADC(i)=-1
	end do
c Put title for the protocol read in into the sweep title, as a default
c title
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

c	Calc nsamp1 (needed in PPAR2) and JMASK (for multiple double pulse
c     sweeps, but not others, need to recalc jmask for each sweep)
	call CALCNS1(ikeep,nkeep,kstep,nsamp,nsamp1,jmask,keepall,
     & ikeep1,kmax,1)

4441	continue

c NB ikeep1 for 1st sweep is defined in CALCNS1
	call NUMSET			!set num lock on ready for response

	que(1)='1. Use present values as they stand'
	que(2)='2. Modify definition of multiple sweeps'
	que(3)='3. Modify which points to be kept stored on disc'
	que(4)='4. Modify any or all of the values'
	que(5)='5. Read a different set'
	nopt=5
	iopt=1
	call PPAR4(cdate,ctime,iTSAMP,iTPREc,nsamp,iDd,
     & calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     & vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     & amVpA1,ftape,gain,nsweep,swtime,ismode,swval,nkeep,ikeep,kstep,
     & jkeep,nsamp1,tkpre,tkpost,que,nopt,iopt,iset,titles,-1,
     & ipatch,ivhclamp,ffilt,randomiz,iperm)
	if(iopt.le.0) iopt=1
	if(iopt.eq.4) then
		readpar=.false.
		noquery=.false.
	endif
	if(iopt.eq.5) then
	   deALLOCATE(avcur,avcurcon,tcur,tcurcon)
	   deALLOCATE(avolt,avoltcon,tvolt,tvoltcon)
         deALLOCATE(iADC,iADC1,jmask)
	   goto 456
	endif
	if(iopt.eq.1) then
	   noquery=.true.
	   goto 59
	else if(iopt.eq.2) then
	   noquery=.true.	!go on to sweep questions at 457
	else if(iopt.eq.3) then
	   noquery=.true.
	   nsmax=kmax	!but may be only kmax if controls done!
	   if(nsamp.gt.kmax) then	!nsamp=<kmax always OK
	      call wdialog(1,'Sample has '//getint(nsamp,nm)(1:nm)//
     &      ' points at present:',icwd)
	      call wdialog(1,'Maximum number of points = '//
     &      getint(nsmax,nm)(1:nm)//'unless it is wished to show',icwd)
	      call wdialog(1,'  ''drug'' and ''control'' traces together
     &in CJFIT, ',icwd)
	      call wdialog(1,'  in which case maximum is '//
     &	      getint(kmax,nm)(1:nm)//' : ',icwd)
		ans='N'
		call defolta(ans,def)
	      call qdialog(1,'Make the maximum '//getint(kmax,nm)(1:nm),
     &      def,icwdq,cans)
		call getinpa(cans,ans)

		if(UC(ans).eq.'Y') nsmax=kmax
	   endif
	   ibox=1
	   if(nsweep.gt.1) then
		mkeep=.true.
		iopt1=1
		if(ismode.eq.4) iopt1=2
		que(1)='(1) Keep the same points for each sweep '
		que(2)='(2) Keep the same points relative to time of Cjumps'
		que(3)='(3) Keep the same points relative to time of Vjumps'
		que(4)=
     &     '(4) Specify the points to be kept separately for each sweep'
		nop=4
		i=1
c		ival=1
	   	nhelp=1
	   	call POPMENU(ixlow,-1,iyhiw,que,nop,ic,icf,icup,ibkw,
     &   	titlew,helps,nhelp,i,charout,ival)
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

		do 458 m=1,n
458		call MODKEEP(ikeep,nkeep,kstep,irate,nsamp,nsamp1,nsmax,m,
     &	ibox,icwd,icqd)
	   else if(nsweep.eq.1) then		!also recalc jmask
		call MODKEEP(ikeep,nkeep,kstep,irate,nsamp,nsamp1,nsmax,1,
     &	ibox,icwd,icqd)
	   endif
	   call CALCNS1(ikeep,nkeep,kstep,nsamp,nsamp1,jmask,keepall,
     &   ikeep1,kmax,1)
	   mkeep=.false.	!reset
	   if(readpar) goto 4441	!any more modifs?
	endif

c=======================================================================

457	continue

c MULTIPLE SWEEPS -ask here if they are needed, after param read from disk,
c but before manual reading of params (if they were NOT read from disc). This
c way the correct iDAC etc for 1st sweep can be setup first time through
c (the manual read also downloads iDAC etc).  If sweep parameters read from
c disc then check them here?
c NB for ismode=4 (and =3??) will need to redefine jmask between each sweep

1121  continue
	i=nsweep
	call defolti(nsweep,def)
	call qdialog(1,'Number of sweeps to be done',def,icqd,cans)
	call getinpi(cans,i)
	if(i.ge.1) nsweep=i      !otherwise default

	if(nsweep.gt.1) then
	   call defoltr(swtime,def)
	   call qdialog(1,'Time between sweeps (seconds) ',def,icqd,cans)
	   call getinpr(cans,x)
	   if(x.gt.0.1) swtime=x
	   ispre1=100
	   ispre2=10
	   iscount=ifixr(swtime*1000.)
114	   continue
         que(1)='(1) identical sweeps'
         que(2)='(2) different V-jump potential for each sweep (not with
     & ramps!)'
         que(3)='(3) differnt C-jump length for each sweep '
         que(4)='(4) different gap between two C-jumps for each sweep'
	   n=4
	   i=1
	   nhelp=1
	   call POPMENU(ixlow,iylow,iyhiw,que,n,ic,icf,icup,ibkw,
     &   titlew,helps,nhelp,i,charout,ival)
	   if(i.ge.1.and.i.le.4) ismode=i
	   if(ismode.ne.1.and.nsweep.gt.30) then
		call BELL(2)
		call wdialog(1,'Maximum is 30 sweeps, unless sweeps are iden
     &tical',icwd)
		goto 1121
	   endif
	   if(ismode.gt.1.and.ismode.le.4) then
	      do 115 m=1,nsweep
		   x=swval(m)
	         call defoltr(x,def)
1182		   if(ismode.eq.2) then
	            call qdialog(1,'Sweep '//getint(m,nm)(1:nm)//
     &            ': V-jump potential (mV)',def,icqd,cans)
	            call getinpr(cans,x)
		      vjump=.true.
		   endif
		   if(ismode.eq.3) then
	            call qdialog(1,'Sweep '//getint(m,nm)(1:nm)//
     &	      ': C-jump length (ms)',def,icqd,cans)
	         call getinpr(cans,x)
		   endif
		   if(ismode.eq.4) then
	            call qdialog(1,'Sweep '//getint(m,nm)(1:nm)//
     &            ': Gap between C-jumps (ms)',def,icqd,cans)
	            call getinpr(cans,x)
		   endif
		   if(ismode.eq.2.and.x.eq.0.) then
			ans='Y'
			call defolta(ans,def)
			call qdialog(1,'Confirm that 0 mV required ',def,
     &            icqd,cans)
			call getinpa(cans,ans)
		      if(UC(ans).eq.'N') goto 1182
		      swval(m)=0.0
		   else
	            if(x.ne.0.0) swval(m)=x
		   endif
115		continue		!end of m1=,nsweep loop
	   endif
			!Now define nsamp1, keepall,
			!jmask (but for ismode=3 will have to redefine
			!jmask between sweeps)
	   call CALCNS1(ikeep,nkeep,kstep,nsamp,nsamp1,jmask,keepall,
     & 	ikeep1,kmax,1)
	   if(readpar) goto 4441	!any more modifs?
	endif		!end of nsweep defs

c NOW SET UP ALL PARAMETERS (when prog first entered, or new parameters
c read by return to 456, nsweep=1 so nothing omitted here: after setting
c up for single sweep them ask if several to be done . If so, unless they
c are all the same, then redo the setup with values for the 1st sweep but
c no queries (later store entire sweep setups eg in CJUMP.SSW file)

c Get calibration
c Scaling: 1401 units are -32768 t0 +32752 in 16 unit increments; dividing
c by 16 gives range as -2048 to +2047 (as for CED502) =-5.0V to +4.9976V
c So 32768/5=6553.6 ADC units/volt

59	continue
	if(nsweep.gt.1.and.ismode.ne.1) then
		ans='N'
		call defolta(ans,def)
      	call qdialog(1,'Randomise multiple sweep sequence ',
     &	def,icqd,cans)
		call getinpa(cans,ans)
	   	if(UC(ans).ne.'Y') then
			do 593 i=1,30
593			iperm(i)=i
			randomiz=.false.
	   	else
			randomiz=.true.	!randomization done after 4571
			call RANPERM(iperm,nsweep,30)
                  call wdialog(1,'Random sequence: ',icwd)
			k=1
			nk=1
100			que(k)=getint(iperm(nk),nm)(1:nm)//' ; '
			nk=2
			do i=nk,nsweep
			   md=nblank1(que(k))
			   que(k)=que(k)(1:md)//getint(iperm(i),nm)(1:nm)//' ; '
			   if(md.gt.70.and.irf.eq.0) then
				irf=1
				k=k+1
				que(k)=' & '
				nk=i
				goto 100
			   endif
			enddo
			do mi=1,k
			   call wdialog(1,que(mi),icwd)
			enddo
			if(discprt) write(8,595) (iperm(i),i=1,nsweep)
	   	endif
	endif

	if(first.or.newpar) then
	   ans='Y'
	   call defolta(ans,def)
	   call qdialog(1,'Calibration: '//getint(ifixr(amVpA1),nm)(1:nm)
     &   //' mV/pA; gain= '//getint(ifixr(gain),nm)(1:nm)//
     &   '; error fac = '//getreal(errfac,nm)(1:nm),def,icqd,cans)
	   call getinpa(cans,ans)
	   if(UC(ans).eq.'N') then
		ftape=-ftape	!so asks for separate gain etc
		call GETcal2(calfac,amVpA,amVpA1,VpA,VpA1,pAV,pAV1,
     & 	itape,ftape,gain,errfac,ibox,icwd,icqd)
	   else
		calfac=(1000.*ftape)/(amVpA1*gain*errfac*6553.6)
	   endif
	endif

c*********************************************************************

4571	continue

c Start timer,mode=0, if nsweep.gt.1 (start it straight after last time period
c ran out -just before the goto 4571 statement)
c No need to start timer before the last jump of series, i.e. if isweep=nsweep
c (as will always be true if single sweep being done). For ismode=1 the clock
c is started after label 42, which is where we loop back to for series
c of identical sweeps. But do not start before the initial boxes put up, ie
c if isweep=0 still

	if(isweep.ge.1.and.isweep.lt.nsweep.and.ismode.ge.2) then
C	VERIFY WITH ISMODE 2 AND  1< ISWEEP < NSWEEP ?

		call NEWPEN(12)
		call GTEXT(3,0,'TIMER STARTED')
		fmt='(a10,a1,i6,a1,i6,a1,i6,a1)'

		write(str1,fmt) 'TIMER2,C,0',',',ispre1,',',ispre2,',',
     &			iscount,';'
		call ForSendString(str1,IERR)
	endif

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
		   do 122 i=1,nvjump		!assumes no ramps!
		 	ivolt1(i)=int2(ifixr(swval(jswp)))
		 	ivolt2(i)=ivolt1(i)		!to signal jump
122		   continue
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
     &     		keepall,ikeep1,kmax,jswp)	!redefine jmask for current sweep
	   	endif
	endif

	if(debug()) then
	   	dx=1.e3/float(irate)
	   	j=0
	   	do 707 i=1,nsamp
	   		if(jmask(i).eq.0) goto 707
	   		j=j+1
	   		t=float(i-1)*dx
	   		write(8,708) i,j,t
707	   	continue
		write(8,709) nsamp,j,nsamp1
	endif

c Define rate for output of values to DACs (1000 Hz normally)
	istout=0		!start address in 1401 memory for DAC pulse
c (define iszout below, when min necessary length of IDAC to be downloaded
c has been found)

	call DEFDRATE(iDd,ipre1,icount1,noquery,nerr)	!set DAC rate

	if(noquery) goto 76

c Define total sample length (itsamp now mus)

	call DEFLEN(itsamp,ibox,icqd)

c Define resting membrane pot (and V-jumps if required)
c As long as iDd not changed then no need to call DEFVOLT if anything other
c than V-jumps changed

8	continue

c Set up pulse pattern on DAC2 to control piezo
c As long as iDd not changed then no need to call DEFCONC if anything other
c than C-jumps changed
c
76	continue
	call SETPIEZO(ivdac2,idac2,reverse,noquery,ibox,icqd)
	iflagr=0
	call DEFCONC(ncjump,iTPREc,ilenc,igapc,iDACc,iDd,nDc,
     & jclast,noquery,reverse,idac2,nerr,idimc,ibox,icwd,icqd,iflagr)
	call SETHOLD(ivhold,ivhclamp,comfac,ivhdac,noquery,ibox,icqd)
      call DEFVOLT(ivhold,comfac,vjump,nvjump,nvramp,ilenv,igapv,ivolt1,
     & ivolt2,iDACv,jvlast,iTPREv,iDd,nDv,noquery,nerr,idimv,sampv,ibox,
     & icwd,icqd,ismode)
	if(nerr.eq.1) goto 76	!iDd changed so redo c-jump

c Calibration for voltage on ADC1 requested only if V-ramps done
	if(sampv.and.(.not.noquery)) then
	   	i=ifixr(calvolt)
		call defolti(i,def)
            call qdialog(ibox,' mV out from clamp per mV membrane pot
     &(integer) ',def,icqd,cans)
		call getinpi(cans,i)
	   	if(i.ne.0) calvolt=float(i)
	endif

c Define final array iDAC() for output to DAC2,3, from the iDACv() and
c iDACc(0) defined above
c	goto 62
	call DEFDAC(iDAC,iDACc,iDACv,jclast,jvlast,jmax,vjump,
     & 	control,iszout,idimc,idimv,idim2)

c Now define start position for ADC data in 1401 as straight after end
c of iDAC() array ie

	ist=istout+iszout+2
	if(noquery) then
	   	ndiv=ndiv1*ndiv2	!# of 0.25 mus ticks between ADC samples
	   	dA=0.25*float(ndiv)	!# of microsec between ADC samples
	   	if(sampv) dA=2.0*dA	!for each channel separately
	   	ipre=ndiv1      	!define ipre,icount
	   	icount=ndiv2	!as in DEFADC
	   	isz=2*nsamp	!sample size in bytes=mult of 4; as in DEFNSAMP
	   	if(sampv) isz=4*nsamp	!for 2 channel sampling
	   	goto 62
	endif

c Define ADC sample rate

	call DEFADC(nprime,irate,iDd,nDc,nDv,srate,ndiv1,ndiv2,ndiv,
     & 	dA,nAc,dnAc,nAv,dnAv,ipre,icount,iexact,ncjump,nvjump,sampv,
     & 	ipre1,icount1,nerr,noquery,ibox,icwd,icqd)

c Define number of points in sample ( and isz, for ADCMEM), and also
c which points to be kept on disc

	call DEFNSAMP(kmax,iTSAMP,dA,nsamp,isz,ibad,sampv,
     & ikeep,nkeep,kstep,jkeep,irate,nsweep,nsamp1,ibox,icwd,icqd)
	call CALCNS1(ikeep,nkeep,kstep,nsamp,nsamp1,jmask,keepall,
     & 	ikeep1,kmax,1)
	if(ibad.ne.0) then
	   	noquery=.false.
	   	goto 8
	endif

62	continue

c Download iDAC() to 1401 in memory pos=0-999????

	call ForTo1401(iDAC,iszout,istout,2,ierr)
	call ForSendString('ERR;',ierr1)
	call ForGetString(buff,15,ierr1)
	e0=buff(0)
	e1=buff(2)

c NB seems that if 'err' command is included then MUST read e0,e1 or get
c problems later

	if(e0.ne.'0'.or.e1.ne.'0') then
	   call wdialog(ibox,'After To1401 : '//buff(0)//buff(1)//buff(2)
     &   //buff(3)//buff(4)//buff(5)//buff(6)//buff(7),12)
	endif
	if(ierr.ne.0) then
		if(ierr.eq.-560) call wdialog(ibox,'TO1401 FAILED',12)
		if(ierr.eq.-590) call wdialog(ibox,'TIMEOUT OCCURED',12)
		call wdialog(ibox,'For transfer of DAC pulse shape to 1401:'
     &      //' ierr = '//getint(ierr,nm)(1:nm)//' iszout = '//
     &	getint(iszout,nm)(1:nm)//' istout = '//
     &      getint(istout,nm)(1:nm),12)
	   	call ForClose1401()
		call wdialog(ibox,'ADVISE: SWICH OFF/ON 1401 AND REBOOT',
     &      12)
	deALLOCATE(avcur,avcurcon,tcur,tcurcon)
	deALLOCATE(avolt,avoltcon,tvolt,tvoltcon)
	deALLOCATE(iADC,iADC1,jmask)
	deALLOCATE(iDaC,iDaCc,idacv)
	   	call EXIT(2)
	else
	   call newpen(10)
	   call gtext(1,35,'ForTo1401 OK!')
	endif
	if(debug()) then
		do i=1,368
			if(idac(i).ne.0) print*,i,idac(i)
		end do
		print*,'end'
	endif
	if(nsweep.gt.1) itrig=2  !Trigger from Keyboard?
	if(noquery) goto 74
	n=2
	i=1
	nhelp=1
      que(1)=' (1) Trigger externally from event 3,4 inputs'
      que(2)=' (2) Trigger internally from keyboard'
	call POPMENU(ixlow,iylow,iyhiw,que,n,ic,icf,icup,ibkw,
     &   titlew,helps,nhelp,i,charout,ival)
	if(i.ne.0) itrig=i
74	continue
	if(itrig.eq.1) then       !External trigger?
		call ForSendString('CLEAR;',IERR)
	else
		call ForSendString('EVENT,D,24;',IERR)!disable external events 3,4
		call ForSendString('EVENT,M,128;',IERR)!so responds to internal pulse 'EVENT,I'
	endif
	call FLUSH(10)
	if(isweep.gt.1) goto 126	!avoid questions!
	if(out_menu) goto 1026
	if(first.or.newpar) then     !Title for this sample?
666		call tdialog(1,'Enter new title:',TITLE,60,.false.,icqd)
	   	call GBLANK(title,60,n1,n2)
		if(n1.eq.0.or.n2.eq.0) goto 666
c		title1=title(n1:n2)//': #'//cnaver
		titles=title(n1:n2)
	endif
	if(debug()) print*,'after title'
126	continue
	if(openb1) then
	   openb1=.false.
	   call clrdialog(1,0)
	   call enddialog(1,0)
	endif
1026	continue
	out_menu=.false.
c Initialise averages, and define title, before 1st sweep
c If there is a series of sweeps, not all identical, then do NOT start
c new average if current sweep is same as last (need check only the
c parameters altered between sweeps, according to value of ismode -
c samepar is set true above if param same)
	if(.not.samepar) call NEWAV(kmax,naver,navc,tcur,tvolt,tcurcon,
     & tvoltcon,control,cnaver,title1,title,n1,n2)		!initialise averages

c 	Print parameters to disc (not screen) before 1st sweep,or after parameters
c 	changed:

	if(first.or.(newpar.and.record)) then
	   	newpar=.false.	!NB reset here only if recording, or first!
         	if(pon()) write(7,1081)
         	if(discprt) write(8,1081)
	   	if(pon()) write(7,*) title1
	   	if(discprt) write(8,*) title1
	   	call PPAR2(0,2,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     &   	calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,
     &	ivhold,sampv,
     &   	vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,
     &	ivolt1,ivolt2,
     &   	amVpA1,ftape,gain,nsweep,swtime,ismode,swval,
     &	nkeep,ikeep,kstep,
     &   	jkeep,nsamp1,tkpre,tkpost,iramp)
	endif

c  	SETUP done, now ask if multiple sweeps required, unless set of sweeps
c 	already started

	first=.false.
	if(isweep.eq.0) goto 214	!get boxes up before the first jump

c======================================================================

42	continue			!return here for another jump

c Start timer,mode=0, if nsweep.gt.1
c No need to start timer before the last jump of series, i.e. if isweep=nsweep
c (as will always be true if single sweep being done). Done here for series
c of identical sweeps (but after label 4571 for non-identical sweeps)
c But do not start before the initial boxes put up, ie if isweep=0 still

	if(isweep.ge.1.and.isweep.lt.nsweep.and.ismode.eq.1) then
		call NEWPEN(12)
		call GTEXT(3,0,'TIMER STARTED')
		fmt='(a11,i6,a1,i6,a1,i6,a1)'
		write(str1,fmt) 'TIMER2,C,0,',ispre1,',',ispre2,',',iscount
     &				,';'
		call ForSendString(str1,IERR)
	endif

c Invoke MEMDAC command to wait for trigger (arguments are exactly same as
c for ADCMEM):

	irept=1
	if(debug()) print 70,istout,iszout,irept,ipre1,icount1
	if(.not.vjump) then					    !DAC2 only for c-jump
		ichanout=2
		fmt='(a10,a1,i6,a1,i6,a1,i6,a1,i6,a1,a2,a1,i6,a1,i6,a1)'
		write(str1,fmt) 'MEMDAC,I,2',',',istout,',',iszout,',',
     &	ichanout,
     &	',',irept,',','HT',',',ipre1,',',icount1,';'
		call ForSendString(str1,IERR)
		call ForSendString('ERR;',ierr)
		call ForGetString(buff,15,ierr)
		e0=buff(0)
		e1=buff(2)
	else if((vjump.and.control).or.ncjump.eq.0) then	!V-jump only = DAC3 only
		ichanout=3
		fmt='(a10,a1,i6,a1,i6,a1,i6,a1,i6,a1,a2,a1,i6,a1,i6,a1)'
		write(str1,fmt) 'MEMDAC,I,2',',',istout,',',iszout,',',
     & 	ichanout,
     &	',',irept,',','HT',',',ipre1,',',icount1,';'
		call ForSendString(str1,ierr)
		call ForSendString('ERR;',ierr)
		call ForGetString(buff,15,ierr)
		e0=buff(0)
		e1=buff(2)
	else if(vjump.and.(.not.control).and.ncjump.gt.0) then  !out to DAC2 & 3
		fmt='(a10,a1,i6,a1,i6,a1,a3,a1,i6,a1,a2,a1,i6,a1,i6,a1)'
		write(str1,fmt) 'MEMDAC,I,2',',',istout,',',iszout,',',
     &	'2 3',
     &	',',irept,',','HT',',',ipre1,',',icount1,';'
		call ForSendString(str1,IERR)
		call ForSendString('ERR;',ierr)
		call ForGetString(buff,15,ierr)
		e0=buff(0)
		e1=buff(2)
	endif

c NB seems that if 'err' command is included then MUST read e0,e1 or get
c problems later

	if(debug().or.e0.ne.'0'.or.e1.ne.'0') then
	   if(.not.openb5) then
	      call opendialog(5,icb,.true.)
		openb5=.true.
	   endif
	   if(KBHIT()) ch=GETCH(ktype)	!remove any waiting character
	   call CLRKB()
	   call BELL(1)
	   call wdialog(ibox,'After MEMDAC : '//buff(0)//buff(1)//buff(2)
     &   //buff(3)//buff(4)//buff(5)//buff(6)//buff(7),12)
	   if(e0.ne.'0'.or.e1.ne.'0') then
	   if(auto.or.(isweep.lt.nsweep)) abort=.true.
	      call ABORTJ(reverse,ivdac2,ivhold,comfac,auto,
     &	isweep,pon())
	   endif
	   openb5=.false.
	   call clrdialog(5,0)
	   call enddialog(5,0)
	   goto 4571	!reload everything after abort
	endif

c Invoke ADCMEM command :
	irept=1
	if(sampv) then		!sample ADC0,1
		fmt='(a11,i6,a1,i6,a5,i6,a4,i6,a1,i6,a1)'
		write(str1,fmt) 'ADCMEM,I,2,',ist,',',isz,',0 1,',
     &	irept,
     &	',HT,',ipre,',',icount,';'
		call ForSendString(str1,IERR)
		call ForSendString('ERR;',ierr)
		call ForGetString(buff,15,ierr)
		e0=buff(0)
		e1=buff(2)
	else				!sample ADC0 only
		fmt='(a11,i6,a1,i6,a3,i6,a4,i6,a1,i6,a1)'
		write(str1,fmt) 'ADCMEM,I,2,',ist,',',isz,',0,',
     &	irept,',HT,',ipre,',',icount,';'
		call ForSendString(str1,IERR)
		call ForSendString('ERR;',ierr)
		call ForGetString(buff,15,ierr)
		e0=buff(0)
		e1=buff(2)
	endif
	call FLUSH(10)
	if(debug().or.e0.ne.'0'.or.e1.ne.'0') then
	   ibox=5
	   if(.not.openb5) then
            call opendialog(ibox,icb,.true.)
		openb5=.true.
	   endif
	   if(KBHIT()) ch=GETCH(ktype)	!remove any waiting character
	   call CLRKB()
	   call BELL(1)
	   call wdialog(ibox,'After ADCMEM : '//buff(0)//buff(1)//buff(2)
     &   //buff(3)//buff(4)//buff(5)//buff(6)//buff(7),12)
	   if(e0.ne.'0'.or.e1.ne.'0') then
	      if(auto.or.(isweep.lt.nsweep)) abort=.true.
	      call ABORTJ(reverse,ivdac2,ivhold,comfac,auto,
     &	isweep,pon())
	   endif
	   openb5=.false.
	   call clrdialog(5,0)
	   call enddialog(5,0)
	   goto 4571	!reload everything after abort
	endif
	if(itrig.eq.1) then
	   if(auto) then
	   	if(.not.record) then
		   if(menu_flag) call DCMENU(0,4,Lb,text,0,0)	!delete all
		   call DCMENU(15,5,Lb,text,icol1,icol2)	!rec screen
		endif
	      call NEWPEN(12)
	      call GTEXT(3,15,
     &	'AUTO-TRIGGER MODE: waiting for external trigger . .')
		call NEWPEN(4)
	      call GTEXT(4,15,
     &	'(Hit ESC key to leave auto mode)')

	   else
	      call NEWPEN(12)
	      call GTEXT(3,15,
     &	'Waiting for external trigger  (ESC to abort)')
	   endif
	endif
	if(itrig.eq.2) then
		call ForSendString('EVENT,I,24;',IERR)
	endif				!pulse events 3 and 4 internally

	j=0
21	continue
	call ForSendString('ADCMEM,?;',IERR)
	call ForLongsFrom1401(lval,2,ierr)
	istatus=lval(0)
	do k=1,10000		!insert slight pause to make sure istatus read
	   	s=123456./float(k)
	end do
	j=j+1

c recorded below if RECORD is on

c RECORD DATA ON DISC: record only single sweeps here (ie iADC())- signalled
c by setting iav=0 in call (record average whenever new mean started)
c and print brief details

	if(KBHIT()) then
	   	ch=GETCH(ktype)	!remove the character or NKEY sees it
	   	call CLRKB()		!remove any extra char if >1 key hit
	   	if(ktype.eq.3) then		!record screen while in auto mode
	        ival=ichar(ch)
		  if(ival.eq.48) then     !key 0 to record screen while in auto mode
			ctime=clk()
		   	rec1=(control.and.navc.ge.1).or.
     &		(.not.control.and.naver.ge.1)
		   	if(.not.record.and.rec1) then
         			call CJDISK4(0,ndev,naver,navc,vjump,control,
     &				cdate,ctime,
     &   				iADC,iADC1,avcur,avcurcon,avolt,
     &				avoltcon,calfac,calvolt,title1,
     &   				nAc,dnAc,irate,nsamp,itsamp,ncjump,
     &				iTPREc,ilenc,igapc,ivhold,
     &   				nAv,dnAv,nvjump,iTPREv,ilenv,igapv,
     &				ivolt1,ivolt2,sampv,nDv,iDd,
     &   				ijd,nxtrec,istrec,kmax,nfull,
     &				nsweep,swtime,isweep,
     &   				nkeep,nsamp1,ikeep1,kstep,amVpA1,
     &				ftape,gain,errfac,
     & 				ipatch, ivhclamp,ffilt,datfilj)
		   		if(nfull.eq.1) then
					ktype=16	!so MEMDAC etc killed below
					iret=1
					goto 254	!kill before getting new disc
		   		endif
		   		if(auto) then		!put up message now
					call INTCONV(ijd,chs)
					call NEWPEN(12)
					call GTEXT(1,63,
     &					'Swp #'//charnb(chs)//' recorded')
		   		else
				recscn=.true.	!so message put up in LPLOTCJ
		   		endif
		   		if(pon()) write(7,*) title1
		   		if(discprt) write(8,*) title1
	         		call PPAR2(0,1,cdate,ctime,iTSAMP,
     &				iTPREc,nsamp,nDv,iDd,
     &   				calfac,calvolt,nAc,dnAc,irate,
     &				ncjump,ilenc,igapc,ivhold,sampv,
     &   				vjump,control,nAv,dnAv,nvjump,
     &				iTPREv,ilenv,igapv,ivolt1,ivolt2,
     &   				amVpA1,ftape,gain,nsweep,swtime,
     &				ismode,swval,nkeep,ikeep,kstep,
     &   				jkeep,nsamp1,tkpre,tkpost,iramp)
		  	endif		!end of print/record
	        endif
	   	endif
254	   continue
		if(ktype.eq.16) then	!ESC
			abort=.true.
			noquery=.true.
			if(debug()) print 802,istatus
	      	call ABORTJ(reverse,ivdac2,ivhold,comfac,auto,
     &	  	isweep,pon())
			if(nfull.eq.1) goto 25
			goto 4571	!reload everything after abort
	 	endif
	endif
	if(istatus.ne.0.and.istatus.ne.-1) goto 21
	if(istatus.eq.-1) then
	   	call BELL(3)
	      call NEWPEN(12)
	      call GTEXT(3,15,' SAMPLES MISSED -SAMPLING TOO FAST')
		ans=getch(ktype)
	endif
	if(control) then
	   	navc=navc+1	!'control'		!skip this if jump aborted
	   	call INTCONV(navc,cnaver)
	else
	   	naver=naver+1	!'drug'
	   	call INTCONV(naver,cnaver)
	endif
	title1=title(n1:n2)//':  #'//cnaver

c Transfer data to host (NB tested for completion within Bergel's TOHOST code)
c	INTEGER*2 FUNCTION ToHost(object,size,addr1401)
c At present upload all points (to new bigger array), rather than each IKEEP
c section separately (upload all gives chance to take every nth point
c in 'non-keep' part, rather than rejecting entirely)
	if(isz.gt.iszmax0) then
	   iszmax=iszmax0
	   istnew=ist
	   k=1
	   j=1
1001	   continue
	   CALL ForToHost(IADC(j),ISZMAX,ISTNEW,2,IERR)
	   idiff=isz-k*iszmax0
	   if(idiff.lt.iszmax0) iszmax=idiff
	   istnew=istnew+iszmax0+2
	   j=(istnew-ist)/2
	   k=k+1
	   if(istnew.ge.isz+ist) goto 1002
	   goto 1001
      else
		CALL ForToHost(IADC,ISZ,IST,2,IERR)
	endif

1002	continue

	call ForSendString('ERR;',ierr1)
	call ForGetString(buff,15,ierr1)
	e0=buff(0)
	e1=buff(2)

c NB seems that if 'err' command is included then MUST read e0,e1 or get
c problems later
	if(e0.ne.'0'.or.e1.ne.'0') then
	if(.not.openb5) then
	   ibox=5
	   call opendialog(5,icb,.true.)
	   openb5=.true.
	endif
	   call wdialog(ibox,'After ToHost : '//buff(0)//buff(1)//buff(2)
     &   //buff(3)//buff(4)//buff(5)//buff(6)//buff(7),12)
	   openb5=.false.
	   call clrdialog(5,0)
	   call enddialog(5,0)
	endif
	if(ierr.ne.0.or.debug()) then
	if(.not.openb5) then
	   ibox=5
	   call opendialog(5,icb,.true.)
	   openb5=.true.
	endif
	   if(ierr.eq.-560) call wdialog(ibox,'TOHost FAILED',12)
	   if(ierr.eq.-590) call wdialog(ibox,'TIMEOUT OCCURED',12)
	   call wdialog(ibox,'For transfer to host of:'//
     &   getint(isz,nm)(1:nm)//' bytes sarting at '
     &   //getint(ist,nm)(1:nm)
     &   //' ; ierr = '//getint(ierr,nm)(1:nm),12)
	   openb5=.false.
	   call clrdialog(5,0)
	   call enddialog(5,0)
	   if(auto.or.(isweep.lt.nsweep)) abort=.true.
	   call ABORTJ(reverse,ivdac2,ivhold,comfac,auto,
     &   isweep,pon())
	   call CLRKB()
	   pause
	else
	   call newpen(10)
	   call gtext(1,35,'ForToHost OK!')
	endif

	if(debug()) then
333	   	print 331,isz/2
	   	read 31,i1,i2
	   	if(i1.eq.-1) then
			do i=1,368
				if(iadc(i).ne.0) print*,idac(i)
			end do
			print*,'end'
			goto 991
	   	endif
	   	if(i1.le.0) goto 991
	   	do i=i1,i2
	   		print 3321,i,iADC(i),iDACv(i)
		end do
	   	if(mod(i-i1+1,24).eq.0) then
			print 411
			call ANYKEY()
	   	endif
332	   	continue
	   	goto 333
991	   	continue
	endif

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
69		continue
	else
	   	j=0
	   	do 692 i=1,nsamp
	   		if(jmask(i).eq.0) goto 692	!skip this point
	   		j=j+1                 !j=1,2,3,...,nsamp
	   		iADC(j)=iADC(i)      !i=2,4,6,...,2*nsamp; voltage
692	      continue
	endif
99	continue

c print final values used :
	ctime=clk()

c RECORD DATA ON DISC: record only single sweeps here (ie iADC())- signalled
c by setting iav=0 in call (record average whenever new mean started)

	rec1=(control.and.navc.ge.1).or.(.not.control.and.naver.ge.1)
	if(record.and.rec1) then !print brief details for jump and keep on disc
	  	call CJDISK4(0,ndev,naver,navc,vjump,control,cdate,ctime,
     &   	iADC,iADC1,avcur,avcurcon,avolt,avoltcon,calfac,
     &	calvolt,title1,
     &   	nAc,dnAc,irate,nsamp,itsamp,ncjump,iTPREc,ilenc,
     &	igapc,ivhold,
     &   	nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     &	sampv,nDv,iDd,
     &   	ijd,nxtrec,istrec,kmax,nfull,nsweep,swtime,isweep,
     &   	nkeep,nsamp1,ikeep1,kstep,amVpA1,ftape,gain,errfac,
     & 				ipatch, ivhclamp,ffilt,datfilj)
	   	if(nfull.eq.1) then
			iret=1
			goto 25
	   	endif
	   	if(pon()) write(7,*) title1  !and print brief details
	   	if(discprt) write(8,*) title1
	   	call PPAR2(0,1,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     &   	calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,
     &	ivhold,sampv,
     &   	vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,
     &	igapv,ivolt1,ivolt2,
     &   	amVpA1,ftape,gain,nsweep,swtime,ismode,swval,
     &	nkeep,ikeep,kstep,
     &   	jkeep,nsamp1,tkpre,tkpost,iramp)
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

c========================================================================

214	continue
C	Return here to start graphics from scratch:
	call clears(0,480,0)

	idev=0
	if(videotyp().ne.18) then
	   	call INIPLT(idev,.false.,1.0)	!start Hgraph
	   	imode=18
	   	i2=1
	   	call PLOTS(0,i2,imode)		!start Lahey graphics
	endif

c======================================================================

c Plot data with Lahey screen routines:
	call LPLOTCJ4(naver,navc,iADC,avcur,avcurcon,nsamp,nAc,dnAc,
     & title1,ijd,itsamp,nAv,dnAv,dA,ncjump,ilenc,igapc,vjump,control,
     & nvjump,ilenv,igapv,record,kmax,abort,mkeep,ikeep1,kstep,
     & jmask,nsamp1,recscn)
	if(nsweep.gt.1.and.isweep.ge.1.and.ismode.ge.2) then
	   	call INTCONV(ifixr(swval(jswp)),chs)
	   	n=NBLANK(chs)
	   	if(ismode.eq.2) chs1=chs(1:n)//' mV'
	   	if(ismode.eq.3.or.ismode.eq.4) chs1=chs(1:n)//' ms'
	   	call NEWPEN(13)
	   	call GTEXT(2,70,chs1)
	endif

c Now redraw boxes (unless in auto mode, or doing series of jumps that has
c not yet finished)
c If autotrigger mode is in effect then go straight to next jump

	if(auto) goto 42		!do next jump without drawing boxes

c If nsweep>0 then do next sweep without drawing boxes
c Read timer2 in loop until flag is set (timer not set before last sweep)

	if(nsweep.gt.1.and.isweep.ge.1.and.isweep.lt.nsweep) then
	   	i=0
121	  	continue
	   	i=i+1
	     	CALL ForSendString('TIMER2,R,0;',ierr)
		call ForLongsFrom1401(lval,2,ierr)
		iflag=lval(0)
	   	if(i.eq.1.and.iflag.eq.1) then
			call BELL(2)
			call newpen(12)
		      call gtext(3,15,
     &  	'CYCLE TOO SHORT - time already elapsed at end of cycle')
	   	endif
	   	if(iflag.eq.0) goto 121		!wait until time up
   	   	isweep=isweep+1 !Want to restart clock as soon as possible
 				    !after prev time period has elapsed, ie at label 42 or 4571
	   	if(isweep.le.nsweep) then
			if(ismode.eq.1) then	!identical sweeps so restart clock here
		   		goto 42		!another identical sweep
			else
	         		noquery=.true.		!so no stops for questions
		   		goto 4571		!load up for next cycle
			endif
	   	endif
	endif

c======================================================================

213	continue			!return here to start boxes from scratch

c 	Put up boxes

	isweep=0	!reset isweep=0 (whether single sweep or series)
	newpar=.false.	!NB reset here for next sweep
c	ifont=2		!simplex (on 0-11 scale)
	csize=2.0
	nbox=15
	call SETLBOX(nbox,Lb,1)
2012	continue
	call NUMSET			!set num lock on ready for response
	Lb(11)=1
	Lb(12)=1
	Lb(13)=1
	Lb(14)=1
	Lb(15)=1
	if(.not.vjump) then
	   	Lb(2)=0
	   	Lb(8)=0
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
	 	endif
	endif
	if(vjump.and.itrig.eq.1) then
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
	text(7)='7.SHOW PARAMS'
	text(8)='8.SHOW V-JUMP'
	text(9)='9.CALIBRATION'
	text(10)='0.CJUMP LENGTH'
	text(11)='-.PIEZO VOLTS'
	text(12)='+.MORE '
	text(13)='*.READ PARAMS'    !+ exit,mult sweeps
	text(14)='/.SAVE PARAMS '
	text(15)='..EXIT '
	if(menu_flag) call DCMENU(0,4,Lb,text,0,0)	!delete all
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
2011	continue
	call CKEY(ch,ikey)
	if(ikey.lt.-1.or.ikey.gt.15) goto 2011

c		 1   2   3   4   5   6   7   8   9  10  11  12  13  14
	goto(201,202,203,204,205,206,207,208,209,210,212,211,307,308,
c		15
     &     999) ikey
	goto 2011		!no valid key


c=======================================================================

201	continue

c 	Another jump. If not vjump then do it straight away. If vjump then
c 	do BOTH c-jump and V-jump now.  If 'control' has been set true earlier
c 	then must redefine iDAC() here so that both done.
C	jump, or series of jumps, now started

	isweep=1
c	if(randomiz.and.nsweep.gt.1) then
c	   	call RANPERM(iperm,nsweep,30)
c	   	if(discprt) write(8,595) (iperm(i),i=1,nsweep)
c	endif
	if(.not.vjump) goto 421		!another identical jump
	if(vjump.and.(.not.control)) goto 421		!another identical jump
	control=.false.	!reset 'control'; output to DACs 2 AND 3
	call NEWPEN(0)
	call GTEXT(1,0,'         ')
	call NEWPEN(10)
	call GTEXT(1,0,'C-JUMP')
	goto 2021		!redefine iDAC() for 2 DAC channels and download it

c Do V-jumps only, no C-jumps

c===============================================================

202	continue

	isweep=1		!jump, or series of jumps, now started
	if(.not.vjump) goto 2011
	if(control) goto 421		!already set for v-jump only
	control=.true.		!reset 'control', then redefine iDAC()
	call NEWPEN(0)
	call GTEXT(1,0,'         ')
	call NEWPEN(13)
	call GTEXT(1,0,'CONTROL')
2021	continue
C	can use gtext!
	call DEFDAC(iDAC,iDACc,iDACv,jclast,jvlast,jmax,vjump,
     & control,iszout,idimc,idimv,idim2)

c Now define start position for ADC data in 1401 as straight after end
c of iDAC() array ie
	ist=istout+iszout+2

c     Download altered iDAC() to 1401 in memory pos=0-999
	call ForTo1401(idac,iszout,istout,2,ierr)
	if(ierr.ne.0) then
	   if(ierr.eq.-560) call wdialog(ibox,'TO1401 FAILED',12)
	   if(ierr.eq.-590) call wdialog(ibox,'TIMEOUT OCCURED',12)
         call wdialog(ibox,' For transfer of DAC pulse shape to 1401 :
     &   ierr = '//getint(ierr,nm)(1:nm),12)
	   CALL ForClose1401()
	   call wdialog(ibox,'ADVISE : SWICH OFF/ON 1401 AND REBOOT',
     &   12)
	   deALLOCATE(avcur,avcurcon,tcur,tcurcon)
	   deALLOCATE(avolt,avoltcon,tvolt,tvoltcon)
         deALLOCATE(iADC,iADC1,jmask)
	   deALLOCATE(iDaCc,iDaCv,idac)
	   call EXIT(2)
	endif

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


c=======================================================================

203	continue

c 	Auto-trigger mode:

	if(nsweep.gt.1) then
	   call BELL(4)

         que(1)='At present set up for multiple sweeps: '
	   que(2)='Before entering AUTO mode, please set up a single-sweep
     & protocol'
	   call POPTEXT(ixlow,iylow,iyhiw,que,2,ic,icf,ibkw)

	   noquery=.false.
	   newpar=.true.
	   nsweep=1
	   deALLOCATE(avcur,avcurcon,tcur,tcurcon)
	   deALLOCATE(avolt,avoltcon,tvolt,tvoltcon)
         deALLOCATE(iADC,iADC1,jmask)
	   goto 456		!read new parameters
	endif
	call BELL(1)
      que(1)='ENTERING AUTO-TRIGGER MODE.'
      que(2)='In this mode a jump will be done each time external trigge
     &r is given,'
      que(3)=' but system is hung while waiting for trigger.'
      que(4)='To escape from auto-trigger mode, so other options are
     &accessible '
      que(5)='Hit ESC key while waiting for trigger.'
	call POPTEXT(ixlow,iylow,iyhiw,que,5,ic,icf,ibkw)
	itrig=1
	CALL ForSendString('CLEAR;',IERR)
	if(.not.record) then	!chance to record event on screen anyway
	   	call SETLBOX(0,Lb,1)
	   	Lb(10)=1
	   	call NUMSET			!set num lock on ready for response
	   	call DCMENU(0,4,Lb,text,0,0)		!delete all
	   	text(10)='0.RECORD SWEEP'
	   	call DCMENU(15,5,Lb,text,icol1,icol2)
	endif

c 	Insert SETPIEZO here because problems encountered with Piezo moving
c 	when entering auto mode!
	call SETPIEZO(ivdac2,idac2,reverse,.true.,ibox,icqd)
	auto=.true.
	goto 42                !!!!


c===============================================================

204	continue

c 	New title:

	if(.not.openb5) then
	   ibox=5
	   call opendialog(ibox,icb5,.true.)
	   openb5=.true.
	endif
	call Tdialog(ibox,'Enter new title:',TITLE,60,.false.,icqd)
	call GBLANK(title,60,n1,n2)
	title1=title(n1:n2)//':  #'//cnaver
	titles=title(n1:n2)
	openb5=.false.
	call clrdialog(5,0)
	call enddialog(5,0)
      goto 2141	!redraw completely

c===============================================================

205	continue

c 	Start new mean. Record current average first; set iav=1 in call to
c 	indicate that average to be recorded (unless it is average of 1 only)

	rec1=(control.and.navc.ge.1).or.(.not.control.and.naver.ge.1)
	if(record.and.rec1) then
	  	call CJDISK4(0,ndev,naver,navc,vjump,control,cdate,ctime,
     &   	iADC,iADC1,avcur,avcurcon,avolt,avoltcon,calfac,
     &	calvolt,title1,
     &   	nAc,dnAc,irate,nsamp,itsamp,ncjump,iTPREc,
     &	ilenc,igapc,ivhold,
     &   	nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,
     &	ivolt2,sampv,nDv,iDd,
     &   	ijd,nxtrec,istrec,kmax,nfull,
     &	nsweep,swtime,isweep,
     &   	nkeep,nsamp1,ikeep1,kstep,
     &	amVpA1,ftape,gain,errfac,
     &      ipatch, ivhclamp,ffilt,datfilj)
	   	if(nfull.eq.1) then
			iret=2
			goto 25
	   	endif
	endif
	call NEWAV(kmax,naver,navc,tcur,tvolt,tcurcon,tvoltcon,
     &  	control,cnaver,title1,title,n1,n2)		!initialise averages
	goto 2141	!redraw completely

c===============================================================

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
	goto 2012

c=======================================================================

207	continue

c	SHOW PARAMETERS:
	call NUMSET			!set num lock on ready for response

c First print current values (all details, screen only)
c iprt,idprt=0,1,2 for no,brief,full print screen, and to disc resp
c go over this!
	call LOCATE(0,0)
	que(1)='1. Use present values as they stand'
	que(2)='2. Modify any or all of the values'
	que(3)='3. Exit program'
	nopt=3
	i=1
	call PPAR4(cdate,ctime,iTSAMP,iTPREc,nsamp,iDd,
     & calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     & vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     & amVpA1,ftape,gain,nsweep,swtime,ismode,swval,nkeep,ikeep,kstep,
     & jkeep,nsamp1,tkpre,tkpost,que,nopt,i,iset,titles,-1,
     & ipatch,ivhclamp,ffilt,randomiz,iperm)
	if(i.gt.0.and.i.lt.4) iopt=i
	if(iopt.eq.1) goto 214
	if(iopt.eq.2) then
		noquery=.false.
		readpar=.false.
		if(.not.openb1) then
	   	   ibox=1
	   	   call opendialog(ibox,icb1,.true.)
	   	   openb1=.true.
		endif
		goto 457
	endif
	if(iopt.eq.3) goto 999

c==========================================================================

208	continue

c	Display V-Jumps:

	if(.not.vjump) goto 2011		!invalid choice
      que(1)='(1) Use the current ''drug'' and ''control'' (means of '//
     &getint(naver,nm)(1:nm)//','//getint(navc,nm)(1:nm)//' sweeps)'
      que(2)='(2) Specify which sweeps to be subtracted (read from disc)
     &'
111   continue
	   n=2
c	   i=1
	   i=idisp
	   nhelp=1
	   call POPMENU(ixlow,iylow,iyhiw,que,n,ic,icf,icup,ibkw,
     &   titlew,helps,nhelp,i,charout,ival)
	if(i.eq.1.or.i.eq.2) then
		idisp=i
		if(.not.openb8) then
	   	   ibox=8
	   	   call opendialog(ibox,icb8,.true.)
	   	   openb8=.true.
	      endif
	endif
	if(idisp.eq.1) then
	   	call VJDISP4(avcur,avcurcon,avolt,avoltcon,nsamp,nvjump,
     &   	naver,navc,ilenv,igapv,ivolt1,ivolt2,ivhold,calfac,calvolt,
     &   	sampv,nAv,dnAv,dA,title1,itsamp,kmax,
     &	jmask,nsamp1,keepall,
     &   	vjump,ncjump,nAc,dnAc,ilenc,igapc,ibox,icwd,icqd)
	else if(idisp.eq.2) then
	   	call VJDSPRD4(ndev,kmax,datfilj,ibox,icwd,icqd)
	else
	  	goto 111
	endif
	openb8=.false.
	call clrdialog(8,0)
	call enddialog(8,0)
	goto 214			!redraw all graphics

c=====================================================================

209   continue

c	Calibration:

	menu_flag=.true.
	call NEWAV(kmax,naver,navc,tcur,tvolt,tcurcon,tvoltcon,control,
     &cnaver,title1,title,n1,n2) !initialise means
	if (.not.openb6) then
		ibox=6
		call opendialog(ibox,icb6,.true.)
		openb6=.true.
	endif
2098	ftape=-ftape	!so asks for separate gain etc
	call GETcal2(calfac,amVpA,amVpA1,VpA,VpA1,pAV,pAV1,
     & itape,ftape,gain,errfac,ibox,icwd,icqd)

c Calibration for voltage on ADC1 requested only if V-ramps done
	if(sampv) then
		i=ifixr(calvolt)
		call defolti(i,def)
		call qdialog(ibox,' calvolt ? ',def,icqd,cans)
		call getinpi(cans,i)
	   	if(i.ne.0) calvolt=float(i)
	endif
	ans='N'
	call defolta(ans,def)
	call qdialog(ibox,'DO YOU WANT TO ALTER SOMETHING ',def
     &      ,icqd,cans)
	call getinpa(cans,ans)
	if (UC(ans).eq.'Y') goto 2098
	openb6=.false.
	call clrdialog(6,0)
	call enddialog(6,0)
	call DCMENU(0,4,Lb,text,0,0)
	goto 2141	!redraw completely

c=======================================================================

210	continue

c 	Alter c-jump length with special key
c 	Modified 03/18/91 03:36pm so ALL cjumps (if>1) changed

	menu_flag=.true.
	call NEWAV(kmax,naver,navc,tcur,tvolt,tcurcon,tvoltcon,control,
     &cnaver,title1,title,n1,n2) !initialise means
	call SETLBOX(15,Lb,1)

c 	If current c-jump length is one of the values on display put the current
c 	value in italics:
	do i=1,12
		if(ilenc(1).eq.ival3(i)) Lb(i)=-1
		itemp=i
	end do
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	do i=1,12
	   	text(i)(1:14)=text3(i)
	end do
	Lb(13)=0
	Lb(14)=0
	Lb(15)=0
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
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
		Lb(itemp)=1
		call SETLBOX(15,Lb,1)
	   	goto 2012		!no valid key
	endif
	il=ival3(ikey)                !!!!!!!!!!!!!!!

c 	check length:
	if(il+iTPREc.gt.itsamp) then
	   call BELL(1)
	   call newpen(12)
	   itp1=itprec/1000
	   itp2=itsamp/1000
	   call gtext(3,15,'Delay ( '//getint(itp1,nm)(1:nm)//
     &   ' ms) plus c-jump ( '//getint(il,nm)(1:nm)//
     &   ' ms) is longer than ADC sample ( '
     &   //getint(itp2,nm)(1:nm)//' ms)')
	   goto 504
	endif

c 	Reset length, and DAC rate if necessary (all sorts of pos problems e.g.
c 	iDd might be set too long if there is a very short v-jump, or vice versa)
c 	06/26/91 12:03pm Next bit altered -may not want to change DAC rate to 1000
c 	when cjump length.ge.1000, eg if short delay (<1ms) required before the
c 	the c-jump, so now check delay = itPREc (microsec) too
	do i=1,ncjump
		ilenc(i)=il
	end do

c 	Redraw selected box in italic:
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	nbox=15
	call SETLBOX(nbox,Lb,1)
	Lb(13)=0
	Lb(14)=0
	Lb(15)=0
	Lb(ikey)=-1
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
	Lb(ikey)=1
	if (.not.openb5) then
		ibox=5
		call opendialog(ibox,icb5,.true.)
		openb5=.true.
	endif
	if((il.lt.1000.or.itPREc.lt.1000).and.iDd.ge.1000) then
	   	  call BELL(1)
	   	  iDd=100
		  call wdialog(5,' DAC rate changed to 10 kHz',icwd)
	else if(il.ge.1000.and.itPREc.ge.1000.and.iDd.eq.100) then
	   	  call BELL(1)
	   	  iDd=1000
		  call wdialog(5,' DAC rate changed to 1 kHz',icwd)
	endif
c and redefine and download everything
	call tdialog(ibox,'  Enter new title:',TITLE,60,.false.,icqd)
	call GBLANK(title,60,n1,n2)
	title1=title(n1:n2)//':  #'//cnaver
	titles=title
	openb5=.false.
	call clrdialog(5,0)
	call enddialog(5,0)
	out_menu=.true.
	noquery=.true.
	newpar=.true.
	goto 4571	!reload everything (whether DAC rate changed or not)
c=====================================================================

211	continue

c 	Set piezo voltage

	menu_flag=.true.
	call NEWAV(kmax,naver,navc,tcur,tvolt,tcurcon,tvoltcon,control,
     &cnaver,title1,title,n1,n2) !initialise means
	if (.not.openb9) then
		ibox=9
		call opendialog(ibox,icb9,.true.)
		openb9=.true.
	endif
9225	call SETPIEZO(ivdac2,idac2,reverse,.false.,ibox,icqd)

c   Redefine iDACc with new piezo voltage

	noquery=.true.
	newpar=.true.
	ans='N'
	call defolta(ans,def)
	call qdialog(ibox,'DO YOU WANT TO ALTER SOMETHING',' [N] = '
     &      ,icqd,cans)
	 call getinpa(cans,ans)
	if(UC(ans).eq.'Y') goto 9225
	call tdialog(ibox,'  Enter new title:',TITLE,60,.false.,icqd)
	call GBLANK(title,60,n1,n2)
	title1=title(n1:n2)//':  #'//cnaver
	titles=title
	openb9=.false.
	call clrdialog(9,0)
	call enddialog(9,0)
	out_menu=.true.
	call DCMENU(0,4,Lb,text,0,0)
	goto 4571	!reload everything (whether DAC rate changed or not)


c======================================================================

212   CONTINUE

c     SECOND MENU:

	nbox=15
	call SETLBOX(nbox,Lb,1)

400	continue
	call NUMSET			!set num lock on ready for response
	if(.not.vjump) then
	   	Lb(6)=0
	endif
	Lb(11)=0
	Lb(12)=0
	Lb(13)=0
	Lb(14)=0
	if(record) Lb(8)=0
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	text(1)='1.HOLDING POT'    !+ exit,mult sweeps
	text(2)='2.ADC SAMPLE  '
	text(3)='3.C-JUMP PULSE '
	text(4)='4.V-JUMP/RAMP  '
	if(itrig.eq.2) then
	   	text(5)='5.EXT TRIGGER '
	else
	   	text(5)='5.KEYBRD TRIG '
	endif
	text(6)='6.JUMP POT'
	text(7)='7.PRINT NOTE  '
	text(8)='8.RECORD SWEEP'
	text(9)='9.SWEEPS'      !
	text(10)='0.MAIN MENU'
	text(11)='+.MORE OPTIONS'
	text(15)='..EXIT PROGRAM '
	call DCMENU(nbox,5,Lb,text,icol1,icol2)

3011	continue
	call CKEY(ch,ikey)
	if(ikey.lt.-1.or.ikey.gt.15) goto 3011
	if(ikey.eq.13) goto 3011						  !invalid key
	if(ikey.eq.-16) then
		call DCMENU(0,4,Lb,text,0,0)		!delete all
		goto 214
      endif
c Record current average and initialise new one for keys=2,3,4,6,7
c For key 10 ispec=: 1=holding pot; 2=v-jump pot; 3=c-jump length;
c 4=piezo (DAC2) volts; 5=calibration; 6=record screen (when RECORD off)
c so should record current average an start new one if ispec=1,2,3,4,5
c options on 10 need new average at present).  Set iav=1 in call to
c indicate that average to be recorded (unless it is average of 1 only)

3012	continue
	if(ikey.eq.2.or.ikey.eq.3.or.ikey.eq.4.or.ikey.eq.6.or.
     & ikey.eq.7.or.ikey.eq.1.or.ikey.eq.9) then
	  	rec1=(control.and.navc.ge.1).or.(.not.control.and.naver.ge.1)
	  	if(record.and.rec1) then
	  		call CJDISK4(0,ndev,naver,navc,vjump,
     &		control,cdate,ctime,
     &   		iADC,iADC1,avcur,avcurcon,avolt,avoltcon,
     &		calfac,calvolt,title1,
     &   		nAc,dnAc,irate,nsamp,itsamp,ncjump,
     &		iTPREc,ilenc,igapc,ivhold,
     &   		nAv,dnAv,nvjump,iTPREv,ilenv,igapv,
     &		ivolt1,ivolt2,sampv,nDv,iDd,
     &   		ijd,nxtrec,istrec,kmax,nfull,
     &		nsweep,swtime,isweep,
     &   		nkeep,nsamp1,ikeep1,kstep,amVpA1,ftape,gain,errfac,
     &      	ipatch,ivhclamp,ffilt,datfilj)
	   		if(nfull.eq.1) then
				iret=3
				goto 25
	   		endif
	  	endif
	  	call NEWAV(kmax,naver,navc,tcur,tvolt,tcurcon,tvoltcon,
     &  	control,cnaver,title1,title,n1,n2)		!initialise averages
	endif

c            1   2   3   4   5   6   7   8   9  10   11   12   13   14
	goto(301,302,303,304,305,306,401,402,309,310,3011,3011,3011,3011,
c            15
     &	999) ikey
	goto 3011	!no valid key

c=========================================================================

301   continue

c Set holding potential

	menu_flag=.true.
	call NEWAV(kmax,naver,navc,tcur,tvolt,tcurcon,tvoltcon,control,
     &cnaver,title1,title,n1,n2) !initialise means
	if (.not.openb7) then
		ibox=7
		call opendialog(ibox,icb7,.true.)
		openb7=.true.
	endif
2226	noquery=.false.
	call SETHOLD(ivhold,ivhclamp,comfac,ivhdac,noquery,ibox,icqd)
	noquery=.true.
	newpar=.true.
	ans='N'
	call defolta(ans,def)
	call qdialog(ibox,'DO YOU WANT TO ALTER SOMETHING',def
     &      ,icqd,cans)
	call getinpa(cans,ans)
	if(UC(ans).eq.'Y') goto 2226
	openb7=.false.
	call enddialog(7,0)
	call clrdialog(7,0)
	out_menu=.true.
	goto 4571	!reload everything (whether DAC rate changed or not)

c=========================================================================

302	continue

c Redefine sample length and rate

81	continue
	ibox=2
	if (.not.openb2) then
		call opendialog(ibox,icb2,.true.)
		openb2=.true.
	endif
810	noquery=.false.
	call DEFLEN(itsamp,ibox,icqd)
	call DEFADC(nprime,irate,iDd,nDc,nDv,srate,ndiv1,ndiv2,ndiv,
     & dA,nAc,dnAc,nAv,dnAv,ipre,icount,iexact,ncjump,nvjump,sampv,
     & ipre1,icount1,nerr,noquery,ibox,icwd,icqd)
	if(nerr.eq.1) then
	   	noquery=.true.
	   	newpar=.true.
	      call tdialog(ibox,'  Enter new title:',TITLE,60,.false.,icqd)
	      call GBLANK(title,60,n1,n2)
		title1=title(n1:n2)//':  #'//cnaver
		openb2=.false.
		call clrdialog(ibox,0)
		call enddialog(ibox,0)
		out_menu=.true.
		call DCMENU(0,4,Lb,text,0,0)
	   	goto 4571	!reload everything
	endif

	call DEFNSAMP(kmax,iTSAMP,dA,nsamp,isz,ibad,sampv,
     &ikeep,nkeep,kstep,jkeep,irate,nsweep,nsamp1,ibox,icwd,icqd)
	call CALCNS1(ikeep,nkeep,kstep,nsamp,nsamp1,jmask,keepall,
     & 	ikeep1,kmax,1)
	if(ibad.ne.0) then
	   	goto 810
	endif
	ans='N'
	call defolta(ans,def)
	call qdialog(ibox,'DO YOU WANT TO ALTER SOMETHING',def
     &      ,icqd,cans)
	call getinpa(cans,ans)
	if(UC(ans).eq.'Y') goto 810
	call clrdialog(ibox,0)
	call enddialog(ibox,0)
	openb2=.false.
	call DCMENU(0,4,Lb,text,0,0)
	goto 2141	!redraw completely

c=========================================================================

303	continue

c Redefine c-jump DAC pulse

83	continue
	if (.not.openb3) then
		ibox=3
		call opendialog(ibox,icb3,.true.)
		openb3=.true.
	endif
					!open
830	noquery=.false.
	iflagr=1
	call DEFCONC(ncjump,iTPREc,ilenc,igapc,iDACc,iDd,nDc,
     & jclast,noquery,reverse,idac2,nerr,idimc,ibox,icwd,icqd,iflagr)
	noquery=.true.
	call DEFADC(nprime,irate,iDd,nDc,nDv,srate,ndiv1,ndiv2,ndiv,
     & dA,nAc,dnAc,nAv,dnAv,ipre,icount,iexact,ncjump,nvjump,sampv,
     & ipre1,icount1,nerr,noquery,ibox,icwd,icqd)
	newpar=.true.
	ans='N'
	call defolta(ans,def)
	call qdialog(ibox,'DO YOU WANT TO ALTER SOMETHING',def
     &      ,icqd,cans)
	call getinpa(cans,ans)
	if(UC(ans).eq.'Y') goto 830
	call tdialog(ibox,'  Enter new title:',TITLE,60,.false.,icqd)
	call GBLANK(title,60,n1,n2)
	title1=title(n1:n2)//':  #'//cnaver
	call clrdialog(ibox,0)
	call enddialog(ibox,0)
	openb3=.false.
	call DCMENU(0,4,Lb,text,0,0)
	out_menu=.true.
	goto 4571	!reload everything (whether DAC rate changed or not)

c=========================================================================

304	continue

c 	Redefine V-jump DAC pulse

84	continue
	if (.not.openb4) then
		ibox=4
		call opendialog(ibox,icb4,.true.)
		openb4=.true.
	endif
840	noquery=.false.
      call DEFVOLT(ivhold,comfac,vjump,nvjump,nvramp,ilenv,igapv,ivolt1,
     & ivolt2,iDACv,jvlast,iTPREv,iDd,nDv,noquery,nerr,idimv,sampv,ibox,
     & icwd,icqd,ismode)
	noquery=.true.
	newpar=.true.
	ans='N'
	call defolta(ans,def)
	call qdialog(ibox,'DO YOU WANT TO ALTER SOMETHING ',def
     &      ,icqd,cans)
	call getinpa(cans,ans)
	if(UC(ans).eq.'Y') goto 840
	call tdialog(ibox,'  Enter new title:',TITLE,60,.false.,icqd)
	call GBLANK(title,60,n1,n2)
	title1=title(n1:n2)//':  #'//cnaver
	call clrdialog(4,0)
	call enddialog(4,0)
	openb4=.false.
	call DCMENU(0,4,Lb,text,0,0)
	out_menu=.true.
	goto 4571	!reload everything (whether DAC rate changed or not)

c=========================================================================

305	continue

c	Trigger:

	if(itrig.eq.1) then
	   	itrig=2
		call ForSendString('EVENT,D,24;',IERR)
		call ForSendString('EVENT,M,128;',IERR)
	else
	   	itrig=1
		call ForSendString('CLEAR;',IERR)
	endif
	call FLUSH(10)
	call DCMENU(0,4,Lb,text,0,0)
	call SETLBOX(15,Lb,1)
	goto 2012

c====================================================================

306	continue
	if(.not.vjump) goto 3011		!invalid choice

c Option to change voltage (of step, not holding pot) in case
c where there is a single V-step
c Define text for keys; ispec21=-1 to display text1; -2 to display text2;
c and ispec21=1,2 to display them with pos sign not neg
c  Define ibig,isign for current ispec21 value (in case it is not reset below)
	menu_flag=.true.
	call NEWAV(kmax,naver,navc,tcur,tvolt,tcurcon,tvoltcon,control,
     &cnaver,title1,title,n1,n2) !initialise means

	isign=1
	if(ispec21.lt.0) isign=-1
	ibig=0
	if(iabs(ispec21).eq.2) ibig=1
500	continue
	nbox=15
	call SETLBOX(nbox,Lb,1)
	Lb(13)=0
	Lb(14)=0
	Lb(15)=0
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	do i=1,10
	   	if(iabs(ispec21).eq.1) then
			text(i)(1:14)=text1(i)
			if(int4(ivolt1(1)).eq.ival1(i)) Lb(i)=-1	!current value in italic
	   	else if(iabs(ispec21).eq.2) then
			text(i)(1:14)=text2(i)
			if(int4(ivolt1(1)).eq.ival2(i)) Lb(i)=-1	!current value in italic
	   	endif
		itemp1=i
	   	if(ispec21.gt.0) text(i)(4:4)='+'	!otherwise leave as neg sign
	end do
	if(iabs(ispec21).eq.1) then
	    	if(ispec21.gt.0)  text(11)='*. >100 mV    '
		if(ispec21.lt.0)  text(11)='*. < -100 mV    '
	else if(iabs(ispec21).eq.2) then
	    	if(ispec21.gt.0)	text(11)='*. <100 mV    '
		if(ispec21.lt.0)  text(11)='*. > -100 mV    '
	endif
	if(ispec21.lt.0) then
	   	text(12)='+.Positive    '
	else
	   	text(12)='+.Negative    '
	endif
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
	ch=GETCH(ktype)	!<ENTER> to accept existing title
	ival=ichar(ch)
	if(ival.eq.13) then
	   Lb(itemp1)=1
	   call SETLBOX(15,Lb,1)
	   goto 2012
	endif
c 	Deal with '+' and '*' keys; they just redraw keys

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
	   do i=1,10 !Reset values in ival1,ival2 to current sign
		ival1(i)=isign*iabs(ival1(i))
		ival2(i)=isign*iabs(ival2(i))
	   end do
	   goto 500		!redraw boxes
	endif

c 	Now deal with voltage keys (1-10)

	ikey=ival-48	!=0,1,..,9
	if(ktype.ne.3.or.ikey.lt.0.or.ikey.gt.9) goto 500	!invalid key
	ivolt1(1)=int2(ikey*10)		!positive
	if(ibig.eq.1) ivolt1(1)=ivolt1(1)+100
	ivolt1(1)=int2(isign)*ivolt1(1)
	ivolt2(1)=ivolt1(1)       !to signify V-jump, not ramp

c 	Redraw selected box in italic:
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	nbox=15
	call SETLBOX(nbox,Lb,1)
	Lb(13)=0
	Lb(14)=0
	Lb(15)=0
	if(ikey.eq.0) ikey=10
	Lb(ikey)=-1
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
	Lb(ikey)=1
	if (.not.openb5) then
		ibox=5
		call opendialog(ibox,icb5,.true.)
		openb5=.true.
	endif
	call tdialog(ibox,'  Enter new title:',TITLE,60,.false.,icqd)
	call GBLANK(title,60,n1,n2)
	title1=title(n1:n2)//':  #'//cnaver
	titles=title
	openb5=.false.
	call clrdialog(5,0)
	call enddialog(5,0)
	out_menu=.true.
c 	now reset iDACv,iDAC and download iDAC (noquery=true)
	noquery=.true.
	newpar=.true.
	goto 4571	!reload everything (whether DAC rate changed or not)

c=========================================================================
307	continue

c	read new parameters :
	randomiz=.false.
	noquery=.false.
	newpar=.true.
	nsweep=1
	call DCMENU(0,4,Lb,text,0,0)
	deALLOCATE(avcur,avcurcon,tcur,tcurcon)
	deALLOCATE(avolt,avoltcon,tvolt,tvoltcon)
      deALLOCATE(iADC,iADC1,jmask)
	goto 456

c=========================================================================

308	continue

c	Save parameters:

	if (.not.openb8) then
		ibox=8
		call opendialog(ibox,icb8,.true.)
		openb8=.true.
	endif
	if(.not.vjump) nvjump=0
      OPEN(unit=17,file=datfil,status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=lrec)
	if(sprec) read(17,rec=1) npset
	iset=npset+1	!default
2221	i=iset
	call defolti(i,def)
	call qdialog(ibox,' Save as set number [next set] ',def,icqd,cans)
	call getinpi(cans,i)
	if(i.ne.0) iset=i
	if(iset.gt.npset) npset=npset+1
	titles=title
	write(17,rec=1) npset
      if(iver.eq.3) then
         do i=1,4
	      do j=1,2
	         do k=1,30
		      i2keep(i,j,k)=int2(ikeep(i,j,k))
	         enddo
	      enddo
	   enddo
	   write(17,rec=iset+1) iver,comfac,ivhold,iTPREc,itsamp,irate,
     &   nsamp,
     &   nAc,dnAc,ncjump,ilenc,igapc,itrig,vjump,nAv,dnAv,nvjump,iTPREv,
     &   ilenv,igapv,ivolt1,ivolt2,amVpA1,ftape,gain,calfac,ndiv1,ndiv2,
     &   sampv,calvolt,iDd,ispec,nsweep,swtime,ismode,swval,titles,
     &   nkeep,i2keep,kstep,jkeep,tkpre,tkpost,iramp,ivhclamp
	   goto 2002
	endif
	write(17,rec=iset+1) iver,comfac,ivhold,iTPREc,itsamp,irate,nsamp,
     & nAc,dnAc,ncjump,ilenc,igapc,itrig,vjump,nAv,dnAv,nvjump,iTPREv,
     & ilenv,igapv,ivolt1,ivolt2,amVpA1,ftape,gain,calfac,ndiv1,ndiv2,
     & sampv,calvolt,iDd,ispec,nsweep,swtime,ismode,swval,titles,
     & nkeep,ikeep,kstep,jkeep,tkpre,tkpost,iramp,ivhclamp
2002	close(unit=17)
      call wdialog(ibox,' Parameters saved as set number = '
     &//getint(iset,nm)(1:nm),icwd)
      if(pon()) write(7,452) iset
      if(discprt) write(8,452) iset
452	format(' Parameters saved as set number ',i4)
	ans='N'
	call defolta(ans,def)
	call qdialog(ibox,'DO YOU WANT TO ALTER SOMETHING',def
     &      ,icqd,cans)
	call getinpa(cans,ans)
	if(UC(ans).eq.'Y') goto 2221
	openb8=.false.
	call clrdialog(ibox,0)
	call enddialog(ibox,0)
	call DCMENU(0,4,Lb,text,0,0)
	call SETLBOX(15,Lb,1)
c	call clears(0,480,0)
	goto 214	!redraw completely

c======================================================================

309   continue

c	Change definition of multiple sweeps
	ibox=10
	if (.not.openb10) then
		ibox=10
		call opendialog(ibox,icb10,.true.)
		openb10=.true.
	endif

3121	i=nsweep
	call defolti(nsweep,def)
	call qdialog(ibox,'Number of sweeps to be done',def,icqd,cans)
	call getinpi(cans,i)
	if(i.ge.1) nsweep=i      !otherwise default
	if(nsweep.gt.1) then
	   call defoltr(swtime,def)
	   call qdialog(10,'Time between sweeps (seconds) ',def,icqd,cans)
	   call getinpr(cans,x)
	   if(x.gt.0.1) swtime=x
	   ispre1=100
	   ispre2=10
	   iscount=ifixr(swtime*1000.)
3114	   continue
         que(1)='(1) identical sweeps'
         que(2)='(2) change V-jump potential between sweeps (not with ra
     &mps!)'
         que(3)='(3) change C-jump length between sweeps'
         que(4)='(4) change gap between two C-jumps between sweeps'
	   n=4
	   i=1
	   nhelp=1
	   call POPMENU(ixlow,iylow,iyhiw,que,n,ic,icf,icup,ibkw,
     &   titlew,helps,nhelp,i,charout,ival)
	   if(i.ge.1.and.i.le.4) ismode=i
	   if(ismode.ne.1.and.nsweep.gt.30) then
		call BELL(2)
		call wdialog(10,'Maximum is 30 sweeps, unless sweeps are ide
     &ntical',icwdq)
		goto 3121
	   endif
	   if(ismode.gt.1.and.ismode.le.4) then
	      do 3115 m=1,nsweep
		   x=swval(m)
	         call defoltr(swval(m),def)
3182		   if(ismode.eq.2) then
	            call qdialog(10,'Sweep '//getint(m,nm)(1:nm)//
     &            ': V-jump potential (mV)',def,icqd,cans)
	         call getinpr(cans,x)
		   endif
		   if(ismode.eq.3) then
	            call qdialog(10,'Sweep '//getint(m,nm)(1:nm)//
     &	      ': C-jump length (ms)',def,icqd,cans)
	         call getinpr(cans,x)
		   endif
		   if(ismode.eq.4) then
	            call qdialog(10,'Sweep '//getint(m,nm)(1:nm)//
     &            ': gap between C-jumps (ms)',def,cans,icqd)
	         call getinpr(cans,x)
		   endif
		   if(ismode.eq.2.and.x.eq.0.) then
			ans='N'
			call defolta(ans,def)
			call qdialog(10,'Confirm that 0 mV required',def
     &            ,icqd,cans)
			call getinpa(cans,ans)
		      if(UC(ans).eq.'N') goto 3182
		      swval(m)=0.0
		   else
	            if(x.ne.0.0) swval(m)=x
		   endif
3115		continue		!end of m1=,nsweep loop
	   endif
	   call CALCNS1(ikeep,nkeep,kstep,nsamp,nsamp1,jmask,keepall,
     & 	ikeep1,kmax,1)
	endif		!end of nsweep defs
	call tdialog(ibox,'  Enter new title:',TITLE,60,.false.,icqd)
	call GBLANK(title,60,n1,n2)
	title1=title(n1:n2)//':  #'//cnaver
	openb10=.false.
	call clrdialog(10,0)
	call enddialog(10,0)
	call DCMENU(0,4,Lb,text,0,0)
	out_menu=.true.
	noquery=.true.
	newpar=.true.
	goto 4571     	! reload everything

c=======================================================================

310   continue
	call DCMENU(0,4,Lb,text,0,0)		!delete all
	call SETLBOX(15,Lb,1)
	goto 214      ! MAIN MENU

c========================================================================

401	continue

	if (.not.openb5) then
		ibox=5
		call opendialog(ibox,icb5,.true.)
		openb5=.true.
	endif
	call tdialog(ibox,'TYPE IN THE NOTE : ',note,72,.false.,icqd)
	if(pon()) write(7,4011) note
	if(discprt) write(8,4011) note
4011	format(' =============================================',/,
     & 1x,a79,/,
     &  ' =============================================')
	openb5=.false.
	call clrdialog(5,0)
	call enddialog(5,0)
	call DCMENU(0,4,Lb,text,0,0)
	call SETLBOX(15,Lb,1)
	goto 212	! : second menu

c==============================================================

402	continue
	if(record) goto 3011
c 	Record last jump as single sweep (iav=0) when RECORD is off (this is done
c 	separately, after KBHIT, when in auto mode)

	rec1=(control.and.navc.ge.1).or.(.not.control.and.naver.ge.1)
	if(.not.record.and.rec1) then
	   call CJDISK4(0,ndev,naver,navc,vjump,control,cdate,ctime,
     &   iADC,iADC1,avcur,avcurcon,avolt,avoltcon,calfac,calvolt,title1,
     &   nAc,dnAc,irate,nsamp,itsamp,ncjump,iTPREc,ilenc,igapc,ivhold,
     &   nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,sampv,nDv,iDd,
     &   ijd,nxtrec,istrec,kmax,nfull,nsweep,swtime,isweep,
     &   nkeep,nsamp1,ikeep1,kstep,amVpA1,ftape,gain,errfac,
     &      	ipatch,ivhclamp,ffilt,datfilj)
	   recscn=.true.		!so message put up in LPLOTCJ
	   if(nfull.eq.1) then
		iret=1
		goto 25
	   endif
	   if(pon()) write(7,*) title1 !and print brief details
	   if(discprt) write(8,*) title1
	   call PPAR2(0,1,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     &   calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     &   vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     &   amVpA1,ftape,gain,nsweep,swtime,ismode,swval,nkeep,ikeep,kstep,
     &   jkeep,nsamp1,tkpre,tkpost,iramp)
	endif		!end of print/record

	call DCMENU(0,4,Lb,text,0,0)
	goto 214	!redraw completely


c=======================================================================

2141	continue

c 	Print all details of new parameters to disc, after one or more
c 	of them has been changed, before re-displaying

	if(record) then
         if(pon()) write(7,1081)
         if(discprt) write(8,1081)
	   if(pon()) write(7,*) title1
	   if(discprt) write(8,*) title1
	   call PPAR2(0,2,cdate,ctime,iTSAMP,iTPREc,nsamp,nDv,iDd,
     &   calfac,calvolt,nAc,dnAc,irate,ncjump,ilenc,igapc,ivhold,sampv,
     &   vjump,control,nAv,dnAv,nvjump,iTPREv,ilenv,igapv,ivolt1,ivolt2,
     &   amVpA1,ftape,gain,nsweep,swtime,ismode,swval,nkeep,ikeep,kstep,
     &   jkeep,nsamp1,tkpre,tkpost,iramp)
	endif
	call DCMENU(0,4,Lb,text,0,0)
	call SETLBOX(15,Lb,1)
	goto 214	!redraw completely

c=======================================================================

999	continue
	call VIDEOMOD(3) !utility lib- this makes graph go for good!
	call ForClose1401()
	call RANDSK(ix,iy,iz,1,repeat)	!keep last ix,iy,iz
c Write .ini
      OPEN(unit=19,file='CJUMP.INI',status='UNKNOWN',
     &   access='DIRECT',form='UNFORMATTED',recl=1024)
	write(19,rec=1) ndev,ipatch,ffilt,ivdac2,datfili,filnam
	CLOSE(unit=19)
	print*,filnam
	call ENDPRINT		!close printer and discprint files
9999	continue
	print 9991
9991	format(' REMEMBER TO TURN OFF PIEZO POWER BEFORE THE CED1401',/,
     & ' Have you done it [N] ? ')
	call BELL(3)
	read 101,ans
c!	ans=getch(b)
c!	print*,UC(ans)
	if(UC(ans).ne.'Y') goto 9999
	deALLOCATE(avcur,avcurcon,tcur,tcurcon)
	deALLOCATE(avolt,avoltcon,tvolt,tvoltcon)
	deALLOCATE(iADC,iADC1,jmask)
	deALLOCATE(iDaCc,iDaCv,idac)

c======================================================================

c	FORMAT:

1	format(' CJUMP4- Concentration- and V-jump experiment program',/)
101	format(a1)
108	format(/)
1081	format(/,' NEW PARAMETERS:')
125	format(' TIMER STARTED')
2225	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
26	format(1x,a12,' already exists; contains',i5,' jumps:',/,
     & ' (1) Append results to existing file (room for ',i5,' more)',/,
     & ' (2) Overwrite the existing file',/,
     & ' (3) Use a different disc',/,
     & ' Option number [1] = ')
31	format(2i8)
331	format(' Number of integers transferred = ',i8,/,
     & '  print iADC,iDACv(i1) to (i2) [0 to end]; i1,i2 = ')
3321	format(3i8)
3335	format(
     & ' SCROLL LOCK on for printing',/,
     & ' CAPS LOCK on for debugging',/,/,
     & ' Sample current on ADC0',/,
     & ' Sample voltage on ADC1 (for V-ramps only)',/)
411	format(' Hit any key to continue')
45	format(' Read jump parameters from disc [Y] ? ')
453	format('&Read parameter set number (1 to',i3,') [',i2,'] = ')
454	format(
     & ' Voltage command/membrane potential ratio = ',f8.3,/,
     & ': Vhold = ',i5,'mV')
4541	format(/,' SET number ',i4)
455	format(
     & ' (1) Use present values as they stand',/,
     & ' (2) Modify definition of multiple sweeps',/,
     & ' (3) Modify which points to be kept stored on disc',/,
     & ' (4) Modify any or all of the values',/,
     & ' (5) Read a different set',/,
     & ' Option number [1] = ')
50	format(' Size of data arrays [',i7,'] = ')
595	format(' Random sequence: ',/,30i3)
70	format(' istout,iszout,irept,ipre1,icount1=',/,5i8)
708   format(2i8,g13.6)
709	format(' nsamp,j,nsamp1 = ',3i8)
802	format(' BEFORE ABORT istatus = ',i5)

c=======================================================================

	END

c***********************************************************************
