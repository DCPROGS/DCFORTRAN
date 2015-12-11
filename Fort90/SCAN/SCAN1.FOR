	program SCAN
c
c Single channel time course fitting (and threshold crossing too?)
c 03/01/93 03:37pm Lahey V5.x version
c
c=====TO DO
c (1) Can the displays for fstdisp, lastdisp also be done regular
c	aivpix in the loop (problem is when we want to truncate on left!)?
c (2) fstdisp,lstdisp should never be used if novlap big enough -check!
c	-also should only be used backward/not backward
c (3) PROBLEM with NEWLEN -this alters amplitudes as well as calc new
c	guess for length of short events -do not want ylevel() changed in
c	cases where amplitudes fixed (manual-one-amp or manual-sublevel -former
c	now fixed -what about latter?)
c  NB gettrans,findtran are not used now (former renamed as GETALL, latter
c  is routine to locate transitions with preset amps that include sublevels)
c  DEFAMP not used now either
c
c Modif 07/30/95 05:19am so filtering can be changed as well as zoom
c  Zoom/filter can now be invoked only in expmode (not after fit), and
c  when filtering required, the whole of the current section is
c  filtered within EXPMODE (and set filtdone=true to indicate that this done);
c  the required section of the filtered data is then copied into idata0() in
c  expmode for fitting (filtered=true transmitted for fitsub/fitsub6 if
c  filtered data so sent for fitting.  Before idata() is filtered, the
c  original unfiltered version is kept in idatsav(), within EXPMODE, and
c  subsequent zoom/unzoom, within this section, can then be done quickly by
c  swapping elements of idata() and idatsav().
c  Earlier version had 'zoomed' and 'filtered' to indicate when these
c  operations were in effect, but they always go together, so now
c  'zoomed'=true indicates that data is currently both zoomed and
c  filtered, so idata() contains filtered version, filt1() contains filtered
c  step response function and tf50, scrit etc are set for filtered data.
c  When filtering not actually needed (i.e. fc set to input value) then
c  filtzoom=false, so can prevent waste of time taken in filtering.
c    Now have three filt1() arrays.  The normal one is stored in filt1n(),
c  the heavily-filtered one is stored in filt1z() (both in common/filtblkz/),
c  and the required one of these is copied into filt1() (in SETFILT) which
c  is communicated to scanfunc etc via common/filtblk/ as before.
c Modif 01/13/95 07:29am to read and fit CJUMP data (IVER=101)
c	ktjump = # of transitions for current jump (kt=for all jumps)
c	nfjump = # of fits for current jump (nfits=for all jumps)
c	jumps(ijump)=nj1, ijump=1,2,...,nsweep =jump # in CJUMP.DAT
c     ijump= sweep # (ijump=1,2,...,nsweep)
c	kjumps(ijump)=kt, ijump=1,2,...,nsweep =index in tint() of 1st latency
c	  for sweep #ijump (could get this from iprops, but simpler to use kjumps)
c	njump= sweep # (from cjump.dat) for the sweep being fitted
c	njfit=# of jump (as in CJUMP) that contains the latest fit (infit is
c	  used in MARK to show this, but for jumps infit reset for each
c	  jump, so if goback used may be in jump that does not contain
c	  latest fit. Update njfit each time a fit is accepted (and
c	  reset it if 'redo n fits' is used.
c     NB njump denotes the jump that is currently being displayed (as read
c		via ioff=512*int4(istrec(njump)).  If GOBACKS used to view another
c		section, njump (but not njfit) is changed. For 'redo' options
c		njfit is changed also. But ijump is used only to count the number
c		of sweeps that have been fitted, so it is changed (incremented)
c		only after '9. Quit/next jump'.
c		Can get problem if we move to next sweep, fit nothing in it,
c		and then goback to earlier section -after this 'go to most
c		recent fit' will return to an earlier section than the current
c		one, and if 'next jump' (key 9) done again an error results
c		-now have message if attempt made to fit in any jump earlier
c		than the current #ijump.
c
c	For conc jumps adopt conventions that
c	(a) ttrans etc measured from t=0
c	(b) when next jump read in, reset ktjump, nfjump
c	    and infit to zero (but not kt, nfits), and tlast=0.0d0
c	(c) mark the first interval in each sweep by setting bit 5 ='32' in iprops
c	    and mark the last interval in each sweep by setting bit 6 ='64',
c	    i.e. iprops(kt)=IBSET(iprops(kt),6)
c
c	INDEXING of data for CJUMP data
c	(1) kt indexed continuously, throughout all sweeps
c	(2) timsav,ibaselin,ifits indexed continuously (by nfits) as usual
c	(3) indfst etc restart at beginning for every sweep, isdfst etc
c		as before, for sections within a sweep -usually whole sweep
c		fits into 1 section (nbuf points), but not nec
c	(4) ?need new index that indexes points in all the fitted sweeps
c		(nsamp points in each) as though they were one continuous
c		set of data with njump*nsamp points. NB this means that the
c		order in which jumps are read in is important for indexing
c		-keep the jump numbers that are read (nj1 values) in array
c		jumps(ijump)=njump, ijump=1,2,...,nsweep.
c
c MODIFICATION HISTORY
c Modif 09/15/94 12:20pm so never sets kfix()=2 in MISSEV or CHECKFIT,
c to fix short openings to average amp -this causes problems particularly
c when fitting an opening larger than AVAMP (but NOT doing it may cause
c problems too? -need better criterion about when to fix -eg don't fix
c if FIXAMP constraint likely to work, or if init guess already >avamp?)
c
c Modif 09/14/94 05:19pm so only 5 values averaged for AVAMP, and value
c not changed thereafter (unless altered manually). Also asks only
c once per fitting now.  Also navamp not reset to 0 when avamp is reset
c manually to avoid possibility of avamp being reset to -2.5 (default).
c
c Modif 09/02/94 10:45am to draw dashed amplitude levels when markamp=true
c  (values read in INSCAN and transmitted via common/ampmark/
c Modif 03/24/94 05:12pm: definition of SCRIT changed to be fraction
c of AVAMP (rather than multiple of rms). Experience so far shows
c this definition will be less sensitive to signal/noise ratio of
c data than the old one. Problem! -can't detect an opening to get initial
c full amp value until threshold (scrit) set, but can't set scrit until
c amplitude known. So use scrit=4*rms for detection of 1st amplitude, then
c reset scrit to smult*avamp subsequently.
c
c POINT AMP -want to keep ifreq() in SCANTEMP -but can't allocate ifreq()
c 	until sfac2 known. Trys allocating temp arrays, jfreq1 etc, bigger than
c	is likely to be need (eg -10k:10k), read temp data into them in
c	READSCAN, return to main prog, allocate ifreq(), then copy data from
c	jfreq and deallocate latter!
c
c TIMING (450DE): (microsec) HLINE=121; RPIXEL(*640)=6443; RDGWIN1=330 (for 1 line)
c
c DATA STORAGE
c (1) real*4 Tint(i), i=1,..,kt holds durations in ms as before
c (2) integer*2 iampl(i) holds amplitudes in intermed units
c (3) integer*1 iprops(i) holds 'properties' of i'th duration and amp
c (integer*1 has range -128 to +127 (bit 7 set gives -128; can use bits 0-6)
c	 0=all OK;
c	+1=amplitude dubious = bit 0;
c	+2=amplitude fixed = bit 1;
c	+4 amplitude of opening constrained (see fixamp) = bit 2;
c	+8=duration unusable = bit 3;
c	etc
c and keep sum of values of more than one property is true
c ie set bits using VAXIBM3 library functions (see TBIT.FOR)
c	IBSET(iprops,i)	-sets bit #i=0,...,7   (integer*4 function)
c	IBCLR(iprops,i)	-clears bit #i=0,...,7  (integer*4 function)
c	BTEST(iprops,i)   -=true if bit #i is set (logical function)
c
c DATA STORAGE FOR RESTART/GO BACK n FITTINGS
c Declarations for temporary data files
c	real*8 dfinter,ttrans,tlast
c	real*8 timsav(1024)
c	integer*2 ifits(1024),ibaselin(1024)
c
c
c GRAPHICS ROUTINES
c  AIVPIX0 -no longer need to offset jfst
c; AIVPIX0.ASM This is version of AIVPIX1 without the jfst argument, which
c;MUST be called with idata(jfst), rather than just 'idata'.  This is more
c;convenient for arrays that start with negative indices, because, for example,
c;if idata is declared as idata(-100,1100) then 'idata' in the call implicitly
c;refers to idata(-100), so must call AIVPIX1 with jfst=101 to start display
c;at idata(1).  It is simpler to call AIVPIX0(idata(1),...) than to
c;call AIVPIX1(idata,jfst=101,...)
c;call aivpix0(ixdisp,iydisp(jfst),ndisp,inc,ipen,first,bk)
c
c Notation:
c READSEC now outputs the following each time a section is read
c  Absolute indices:
c   in1=1 + (isec-1)*nbuf = abs index of 1st point in section (exc overlap)
c   in2=isec*nbuf = abs index of last point in section (exc overlap)
c   jn1=in1 - novlap = abs index of 1st point actually read in (inc overlap)
c   jn2=in2 + novlap = abs index of last point actually read in (inc overlap)
c   indfst= abs index of first point on display
c   indlast= abs index of last point on display
c  Indices in idata (within section)
c   is1=1 = index of 1st point in section (exc overlap)
c   is2=nbuf (or nrlast for isec=nsec)= index of last point (exc overlap)
c   js1=is1 - novlap = index of 1st point in idata (inc overlap)
c   js2=is2 + novlap = index of last point in idata (inc overlap)
c   isdfst= index in idata() of first point on display
c   isdlast= index in idata() of last point on display
c
c SCALING:
C 1401 units are -32768 t0 +32752 in 16 unit increments; dividing
c by 16 gives range as -2048 to +2047 (as for CED502) =-5.0V to +4.9976V
c So 32768/5=6553.6 units/volt, or, for CED502 data 2048/5=409.6
c Now (05/21/93 09:22am) scale in 2 stages to avoid loss of original
c precision that would occur if scaled in READSEC to display range integers
c (0-480, or actually yd1 to yd2= 127 to 457).  Now calculate scalfac to scale
c orig values to integers that cover the range up to at least 4096, so as to
c keep original precision, so numbers are larger than required for display
c by a FACTOR, nscale, that is in integer power of 2.  Also do not add
c offset in READSEC.  Then display routines have only to divide by
c NSCALE and add offset, which should be very fast operations.
c RAW UNITS=ADC units read from disc
c INTERMEDIATE UNITS=units output by readsec (ADC*sfac2) (no offset)
C		e.g. ybase,base,itriglev
c PIXEL UNITS=(values in intermed units)/2**nscale+iyoff (with offset)
c		e.g. ybasevga,basevga,itrigvga
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
c Hgraph colours
c -1=black; 0=white; 1=yellow (bright); 2=purple (bright); 3=red (bright)
c 4= pale blue (bright); 5=green (bright); 6=dark blue (bright);
c 7=dark gray; 8=pale gray; 9=brown (bright); 10=purple; 11=brown;
c 12=pale blue; 13=green; 14=dark blue
c For HP550C plotting colours should be
c 0=black  (NB black/white interchanged compared with screen numbering)
c 1=yellow; 2=magenta; 3=red; 4=cyan; 5=green; 6=blue; 7=white; -1=white
c and 8-14 are same as 0-6.
c
c DEFINITION OF VARIOUS ARRAYS
c  (1) SCANTEMP ARRAYS
c	IFITS(i) = index of tint(), iampl() for the duration and amplitude
c of the interval following first transition of fitting #i
c	TIMSAV(): time from 1st data point in CONSAM is
c		 ttrans=t0 + finter*float(infirst-1)
c NB 30 minute record=1800 sec = 1.8x10**9 microsec -too big for single prec if
c aim was to calc all intervals from differences in such values -but if used
c only to record abs time of last transition, so when next transition fitted
c the interval preceding it can be calculated -diff should never be less than
c 100 microsec, but even this accuracy implies 1 in 1.8x10**7 -still too big
c so better use real*8 for ttrans, tlast
c		nfits=nfits+1
c		timsav(nfits)=ttrans
c		ifits(nfits)=kt+1		!points to element of tint(), iampl()
c		ibaselin(nfits)=ifixr(base)
c
c  (2) FITTING ARRAYS
c infirst=isprev + (isec-1)*nbuf = absolute index of 1st point in idata1(),
c	needed to calc transition times.
c dfinter=real*8 version of finter
c dt(i), i=1,ntrans-1:   dt(1)=time from 1st to 2nd transition
c				 dt(2)=time from 1st to 3rd transition
c				 dt(3)=time from 1st to 4th transition etc
c delt(i), i=0,ntrans-1: delt(0)=time from prev transition to 1st transition fitted
c	     (microsec)	 delt(1)=time from 1st to 2nd transition
c				 delt(2)=time from 2nd to 3rd transition
c				 delt(3)=time from 3rd to 4th transition etc
c ylevel(i), i=1,ntrans+1:  ylevel(1)=current level (intermed units) before
c					the first transition (baseline not subtracted)
c 				    ylevel(i)=level before ith transition
c				    ylevel(ntrans+1)=level after last transition
c
c  IDEAS FOR SCAN
c	(2) Use of inset displays? eg while fitting enlarged trace, show
c unenlarged version in box? How to mark bit to be enlarged for fitting?
c -like marking 'good bit'?
c	(3) Colours e.g. -green=fitted; red=unusable; ?=not yet fitted
c	(4) Might seem faster if fairly small sections used as each section
c	would not take long to read from disc?
c	(5) If array with data in it was deallocated and reallocated at each
c	read, index in it could (?) be kept same as index in all data
c	e.g. for section 1 allocate idata(1:100), for section 2 idata(101:200) etc
c	(6) will need some other fix for nchan=2 because cannot use EQUIV
c	for allocatable arrays: best sort out into 2 sep arrays before
c	starting if nchan=2; do this in consam?
c
c Scaling from SCAN5: iboff=offset initially but this is adjusted (a) in ROLDAT
c when initial baseline set (b) in AFIT1 when baseline adjusted.
c from DATGET:
c	DO 204 I=1,NBUF
c204	IBUF(I)=IFIX(DGAIN*FLOAT(IBUF(I)))+IBOFF
c from ROLDAT:
c	IOFF1=IBASE-IFIXR(YBASE)  !OFFSET;SETS DISPLAY BASELINE AT ibdisp
cc IOFF1=change (vs that used after read) in offset from last time
c	DO 2044 I=1,NBUF	!OFFSET CURRENT SECTION BEFORE SCROLL ETC
c2044	IBUF(I)=IBUF(I)+IOFF1
c	iboff=iboff+ioff1		!new offset for next read
cc need to adjust IOFF etc each time baseline set in AFIT?
c
c
c  Indexing used in SAMPED
c	Can now keep track of individual points by their index=IN in the total
c data set on disc (in=1,2,...,ndat), though this has to be read in in sections.
c The point with index=IS, idata(is), in section number ISEC has index
c	in= is + (isec-1)*nbuf
c Conversely point with index=IN in the whole data set occurs in section
c	isec= 1 + (in-1)/nbuf
c and has index, within this section,
c	is= in - (isec-1)*nbuf
c Display starts at point with index is=isfst in current section so point
c with index=ID on display (1st displayed point=1) has index in section of
c	is= id + isfst - 1
c Similarly incurs,iscurs,idcurs= index in whole array, section, display
c respectively of the data point nearest to the cursor.
c
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c
	integer*2 ktype,videotyp
	integer*2 iser,iserror
c	integer*2 lrow,int2
c	integer iact(0:15)
	logical discprt,allocated,btest,filtzoom,filtdone,zoomed
	logical pon,slock,debug,caplock,debug1,goback,cjump,readnew
	character*11 cdate,ctime,cnum,cnum1,cnum2
	character*12 tstring
	character*3 microsec
	character*55 helpst(18),helps(5)	!help strings
	character*20 title 			!for popmenu
	character filtfile*20		!for GETFILT
	character ndev*2,scanbak*30,text1*67
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	real*4 dtsav(49),dt(49),delt(0:50)	!now params of FITSUB/4
	logical seqdisp	!true while showing sequence from ekdist.tmp
c for common/fitblk/
	real*4 stepamp(50)
c For CJUMP.DAT
	logical control,vjump
	character title1*79
	integer*2 istrec(250)		!orig=500 bytes (kept as int*2 to fit in 1 rec)
	allocatable::jumps,kjumps
	integer*2 jumps(:),kjumps(:)
	allocatable::jump1,kjump1	!temp version for READSCAN
	integer*2 jump1(:),kjump1(:)
c For INSCAN:
	real*4 timamax(49)
	real*4 filt1(1024),filt1n(1024),filt1z(1024)
	character*8 adctime
	logical invert,opendown,newpar,sdone,baseOK,restart,restart1
	logical disp,end,backward,stopped,triggered,expmod
	logical fitmode,usepots,fstdisp,lastdisp,mono,ampdone
	logical open,shut,shutprev,shutsav,trigset,gostrt
	logical disptran,dispderiv,dispguess,refit,newrange,newguess
	logical newrms,accepted,present,numamp
c For Amplicon ADCs
	integer*2 ichan(0:15),iadc(0:15),iadc0(0:15),nadc
c
c Declarations for temporary data files
	real*8 dfinter,tlast,tzerod,tsample
c=	real*8 timsav(1024)
c=	integer*2 ifits(1024),ibaselin(1024)
	allocatable::timsav
	allocatable::ifits
	allocatable::ibaselin
	real*8 timsav(:)
	integer*2 ifits(:),ibaselin(:)
c
c Data files
c=	real*4 tint(1024)
c=	integer*2 iampl(1024)
c=	integer*1 iprops(1024)	!properties of ith interval (see above)
	allocatable::tint
	allocatable::iampl
	allocatable::iprops
	real*4 tint(:)
	integer*2 iampl(:)
	integer*1 iprops(:)
c============================================================
	allocatable	:: idata
	integer*2 idata(:)
	integer*2 ix(2048),ixsav(2048)
	integer*2 idata1(2048)	!for data to be fitted (and as temp file)
c					!(scaled version for IDCPIX)
	integer*2 idata0(2048)	!unscaled version of idata1 for AIVPIX
	real*4 data(2048)		!for FITSUBn
	integer iact(0:15)	!for IDCPIX
c===arrays for point amp histo
	allocatable::ifreq1,ifreq2,ifreq3   !all point, shut point, open point
	allocatable::jfreq1,jfreq2,jfreq3   !temp versions for READSCAN
	integer*4 ifreq1(:),ifreq2(:),ifreq3(:)
	integer*4 jfreq1(:),jfreq2(:),jfreq3(:)
	integer*4 ifmin(4),ifmax(4)
c
	integer*1 array1(:)	!for 'popup' notices
	allocatable :: array1
	character*60 string(1)
	character*35 strings(8)
c==================
c	integer*4 iabar3(0:641)
	integer*1 isavtrig(320),isavbase(320)!to hold graphics overwritten by line
	character*1	getch,ans,UC,savin
	logical kbhit
c end of IV declarations
C For DCMENU
	integer Lb(18)
	character*78 text(18)
c
	logical mouse_on,mous_set
c
	character mtitle*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,jcol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	common/dp/discprt
	common/mousval/mouse_on,nbutton
	common/popvalx/mxlo,mylo,myhi,ictx,ibkx,icfx	!values for poptext calls
	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls
	common/popvalt/lxlo,lylo,lyhi,ictt,ibkt,icft,icht	!values for poptable calls
	common/dialval1/ictd,icfd,icbd  !colours for dialog box #1 (text,frame,back)
	common/datpos/yd1,yd2,iyd1,iyd2	!position of data area
	common/filtblk/filt1,nfilt1,dtf1,npfilt,finter,filtf
	common/filtblkz/filt1n,nfilt1n,dtf1n,filt1z,nfilt1z,dtf1z
	common/fitblk/data,if1,if2,ntrans,neval,nevfix,itype,stepamp,y0
	common/aafunc/ trise
	common/ampmark/nampmark,iamark(10)
	common/prog/ixbox,iprogx1,iprogx2,ibdpp,iyframe,icprog,ibk
c	common/prog/iprogx0,iprogx1,iprogx2,ipmax,ibdpp,iyframe,icprog,ibk
	common/DMENU/csizb,ifonb,csize,ifont2,nboxlast,nblast
	common/hgv/ivxlo,ivxhi,ivylo,ivyhi,ixlo,ixhi,iylo,iyhi
C
c define function
	pon()=slock()
	debug()=caplock()
c
	debug1=.false.		!to check trigger search etc
c
	filnam='SCAN.PRT'
	call WINPRINT	!print file control
c	OPEN(unit=7,file='PRN',iostat=nerr)
	OPEN(unit=7,file=prtport,iostat=nerr)
	print 1
	if(pon()) write(7,1)
	if(discprt) write(8,1)
1	FORMAT(' SCAN: Time course fitting of single channels',/)
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
     & ' CAPS LOCK on for debugging',//)
c
101	format(a1)
c
	call DEFNOTE
c!	call BEETH5()
c
c Define version number for SCAN
c	iscanver=100
	iscanver=101		!V101 takes cjump data
c
c Define NTMAX = size of tint, iampl, iprops, and NFMAX=size of timsav() etc
c and allocate arrays
c	ntmax=10240	!dimension of tint(i) and iampl(i) -set in SCAN now
c	nfmax=5120	!dimension of timsav() etc
	ntmax=20480	!dimension of tint(i) and iampl(i) -set in SCAN now
	nfmax=10240	!dimension of timsav() etc
	ALLOCATE(tint(ntmax),iampl(0:ntmax),iprops(ntmax))
	ALLOCATE(timsav(nfmax),ifits(nfmax),ibaselin(nfmax))
c
	iampl(0)=0		!saves errors before first amplitude defined!
c
	call SETATTR(112)		!'inverse' attrib seems nec while in graphics
	call SETMOUSE()		!define values in common\mousval\ (in IVLIB)
	mous_set=.false.	!initialise
c	nsetup=0	!so asks for step response function in INSCAN
	nsetup=-1000	!so default step response function is Gaussian
	navamp=0	!no full amps averaged yet (avamp set to 2.5pA after inscan)
	avtot=0.0
	fitmode=.false.
	expmod=.false.
	disptran=.true.	!display transition points found by FINDTRANS
	dispderiv=.true.	!display first derivative and peaks located in it
	dispguess=.true.	!display init guess for fit
	ampdone=.false.	!initial amplitude estimate not yet done
	sdone=.false.	!not yet calculated baseline and RMS
	baseOK=.false.	!not yet accepted bit of baseline for RMS
	shut=.true.		!before first opening located (not nec if cjump?)
	SHUTSAV=.true.	!before first opening located
	open=.not.shut
c	nshutfit=40		!default # of shut points to be fitted after last trans
c (nshutfit now defined in inscan)
	mono=jcol.eq.0	!monochrome screen
	tlast=0.0d0		!must initialise for CJUMP case
	infit=0		!abs index of trigger point for last transition fitted
	ixfprev=-1 		!x value where previous trigger point was drawn
	ix0prev=-1 		!x value where previous t=0 point was drawn (cjump)
	idiskq=-1		!until defined
	itrigvga=-1		!until trig level defined
	ifirst=1		!no erase in AIVPIX1 first time (which resets it to 0)
	refit=.false.
	newpar=.false.
	newrange=.false.
	newguess=.false.
	newrms=.false.
	numamp=.false.	!for numerical spec of full amp
	microsec=' '//char(230)//'s'	!char(230)=greek mu
c Scaling: 1401 units are -32768 t0 +32752 in 16 unit increments; dividing
c by 16 gives range as -2048 to +2047 (as for CED502) =-5.0V to +4.9976V
c So 32768/5=6553.6 ADC units/volt
c Use Y coords (0-479) between 127, 457 for now, to span 65536 ADC units
	yd1=127.	!lowest point on screen (Y pixels) for display of channels
	yd2=457.	!highest point on screen (Y pixels) for display of channels
	iyd1=ifixr(yd1)
	iyd2=ifixr(yd2)
c Values for poptext calls
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
	if(mono) then
	   ictx=14
	   ibkx=0
	   icfx=14
	endif
c Values for POPMENU calls
c	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls
	nxlo=100
	nylo=-1
	nyhi=470
	ictm=15		!text white
	ibkm=8		!background dark grey
	icfm=2		!frame/title green
	icupm=12		!upper case red
c Values for poptable calls
c	common/popvalm/lxlo,lylo,lyhi,ictt,ibkt,icft,icht	!values for poptable calls
	lxlo=100
	lylo=-1
	lyhi=470
	ictt=1		!text dark blue
	ibkt=15		!white background
	icft=3		!frame light blue
	icht=12		!header/footer red
c
	ix1=70		!ixlo for popup boxes
	iy2=200		!iyhi for popup boxes
c
	kt=0			!number of transitions located so far
	ktjump=0		!# of transitions for current jump (kt=for all jumps)
	nfits=0		!number of fits done so far
	nnull=0		!counts number of sweeps with no transitions
c
c colours:
	ipen=14           !COLOR FOR DATA
	ibk1=1           	!BACKGROUND COLOR for data area
	ibk=3           	!BACKGROUND COLOR for outside data area
	icw=11		!colour to write in dialog box (light blue)
	icw1=12		!red to write in dialog box for warnings
c	common/dialval1/ictd,icfd,icbd  !colours for dialog box #1 (text,frame,back)
	ictd=icw		!icw is older notation -better use value in common/dialval/
	icbd=0			!background col for dialog box (0 until fixed)
	icbl=10		!baseline line colour
	ictrig=12         !color of trigger level
	if(mono) then
	   ipen=15
	   ibk=0	!for monochrome
	   ibk1=1	!for monochrome
	   icw=15
	   icw1=15
	   ictrig=15
	endif
	ibk2=8            !background color in expand mode (and FIT mode?)
	ipen=14		!data colour
	if(mono) then
	   ibk2=0
	   ipen=15
	endif
c
c defs for DCMENU
c	nbox=10	!number of boxes for DCMENU
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
c
c NB set sdone=false BEFORE inscan called, so know whether sdone=true or
c not when INSCAN called for new parameters (newpar=true)
	sdone=.false.	!no baseline SD yet
c
	call DCASK('Auto-restart of previous analysis','y',ans)
	restart=ans.eq.'Y'
	if(.not.restart) then
	   print 23
23	   format(
     & ' NOTE: starting a new analysis will delete your existing',/,
     & '  restart file, SCANTEMP.DAT.  If you want to keep it, and',/,
     & '  it is not already backed-up, leave now and back it up. ',/,
     & '  Do you want to leave now [N] ? ')
	   read 101,ans
	   if(UC(ans).eq.'Y') STOP
	else		!restart
c       Allocate temp oversize arrays to read point amps into
	   im1=-20000
	   im2=20000
	   ALLOCATE(jfreq1(im1:im2),jfreq2(im1:im2),jfreq3(im1:im2))
	   do i=imin,imax
		jfreq1(i)=0
		jfreq2(i)=0
		jfreq3(i)=0
	   enddo
c Ditto for jumps (not known yet whether run was cjump)
	   ijd=2000
	   ALLOCATE(jump1(ijd),kjump1(ijd))	!OK for up to 2000 jumps
	   call READSCAN(kt,tint,iampl,iprops,ntmax,nfits,timsav,ifits,
     &	 ibaselin,nfmax,nbuf,novlap,srate,finter,ndisc,errfac,
     &	 tsect,ioff,ndat,nsec,nrlast,avtot,navamp,avamp,rms,
     &	 nwrit,nwsav,newpar,opendown,invert,confac,
     &	 usepots,disp,smult,scrit,vary,ntrig,navtest,dgain,iboff,
     &	 expfac,bdisp,ibflag,iautosub,xtrig,ndev,cdate,adctime,
     &	 nsetup,filtfile,ffilt,filtf,
     &	sfac1,sfac2,sfac3,nscale,calfac,calfac1,calfac2,iyoff,ioff1,
     &	 disptran,dispderiv,dispguess,ampfac,tmin,
     & 	 tsfac,tlfac,sdone,dfinter,tlast,goback,shutsav,
     & 	 shut,shutprev,backward,prevlevel,t0sav,y0sav,vard,nshutfit,
     &	 infit,infirst,ixfprev,idiskq,ifirst,base,basevga,ibasevga,
     &	 itrig,itrigvga,itriglev,inc,incabs,
     &	 indfst,isdfst,isec,ndisp,ndisp1,
     &	 filt1,nfilt1,dtf1,timamax,
     & 	 tf01,tf1,tf50,tf99,tf999,npfilt,tstring,
     &	 iscanver,tcfac,facjump,derivfac,mtitle,
     &	 jfreq1,jfreq2,jfreq3,imin,imax,im1,im2,ifmin,ifmax,
     & 	 cjump,njdim,jump1,kjump1,ijd,tzerod,intzero,tsample,
     &	 ktjump,njump,njfit,ijump,istrec,jumplen,nnull,ktlast,
     & 	 izoom,fcz,fczoom,ampz,avampsav,gostrt)
	   ntmax=20480	!dimension of tint(i) and iampl(i) -set in SCAN now
	   nfmax=10240	!dimension of timsav() etc
	   if(iscanver.eq.100.or.(.not.cjump)) then
		cjump=.false.
		njdim=1   !must allocate arrays anyway as they are args for subroutines
	      ALLOCATE(jumps(njdim),kjumps(njdim))
	   endif
c Now allocate ifreq() with correct dimensions, copy data from jfreq() and
c deallocate latter
	   imax=2*ifix(32768.*abs(sfac2))+1	!round up  (see below!)
	   imin=-imax
c==	   imax=ifix(32752.*sfac2)+1 		!round up
c==	   imin=-ifix(32768.*sfac2)-1		!round down
	   ALLOCATE(ifreq1(imin:imax),ifreq2(imin:imax),ifreq3(imin:imax))
	   do i=imin,imax
		ifreq1(i)=0
		ifreq2(i)=0
		ifreq3(i)=0
	   enddo
	   do i=ifmin(1),ifmax(1)
		ifreq1(i)=jfreq1(i)
	   enddo
	   do i=ifmin(2),ifmax(2)
		ifreq2(i)=jfreq2(i)
	   enddo
	   do i=ifmin(3),ifmax(3)
		ifreq3(i)=jfreq3(i)
	   enddo
	   DEALLOCATE(jfreq1,jfreq2,jfreq3)
	   call CATCHBUG(ifreq1,ifreq2,ifreq3,imin,imax,ifmin,ifmax)
c     define filter for zoom/filter (as in INSCAN)
	   avampz=ampz/calfac2		!convert to intermed units
	   filtzoom=ffilt-fcz.gt.0.001
	   if(filtzoom) then
		call NORMFILT(nfilt,dtf,filt,dtf1z,nfilt1z,filt1z,
     &  	 x,x,x,x,x,-1000,fczoom)
	   endif
c Set normal filter
	   nfilt1n=nfilt1
	   dtf1n=dtf1
	   do i=1,nfilt1
		filt1n(i)=filt1(i)
	   enddo
c
	   if(cjump) then
	 	if(.not.allocated(jumps)) then
		   ALLOCATE(jumps(njdim),kjumps(njdim))
		endif
		do i=1,ijump
		   jumps(i)=jump1(i)	!copy from temp array
		   kjumps(i)=kjump1(i)
		enddo
	      tjlen=float(jumplen)*640./finter
	   endif
	   DEALLOCATE(jump1,kjump1)
c
	   if(gostrt) goto 998		!gostrt now set in READSCAN
c
c After restart check that last interval was not already set as first
c interval of a null sweep, which was confirmed as being null before exit
c (see case(9)=quit/next jump). If so, warn to go straight to next jump
c===must put 'go straight to next jump' below, after defdialog etc
c	   if(cjump) then
c		restart1=.false.
c		if(kt.eq.ktlast.and.BTEST(iprops(kt),5).and.
c     &	   tint(kt).eq.sngl((tsample - tzerod)*1.d-3)) then
c		   call BELL(3)
c		   print 10,njump
c10		   format(
c     & ' The current jump, #',i4,' is already specified as null',/,
c     & ' Go straight to NEXT JUMP now. O.K. [Y] ?')
cc		   call ANYKEY
c		endif
c		if(BTEST(iprops(kt),6)) then
c		   call BELL(3)
c		   print 12,njump
c12		   format(
c     & ' The current jump, #',i4,' is already specified as completed',/,
c     & ' Go straight to NEXT JUMP now. O.K. [Y] ?')
cc		   call ANYKEY
c		endif
c		ans='Y'
c		call INPUTa(ans)
c		if(ans.eq.'Y') then
c		   restart1=.true.	!so some bits skipped after 13
c		   ikey=9		!next jump etc
c		   iline=1		!next jump
c		   nval=3
c		   goto 13
c		endif
c	   endif
c
c Now allocate data array and set other values that were not kept in AUTWRIT
	   ALLOCATE(idata(1-novlap:nbuf+novlap))
	   baseOK=.true.		!can't have autwrit before these set true
	   ampdone=.true.
	   scrit=smult*abs(avamp)		!in intermed units
	   iampcrit=iabs(ifixr(ampfac*avamp))
	   ifirst=1	  !no erase needed in first AIVPIX (which resets it to 0)
	   indsav=indfst				!update indfst
	   isdsav=isdfst
	   indlast=indfst+ndisp-1
	   if(.not.cjump) then
	      indfst0=indfst      !save its value
	      isec0=1 + (indfst0-1)/nbuf     	!section # for point #indfst
	   else
		indfst0=intzero-ifix(xtrig*float(ndisp))	!show t=0 in standard posn
		if(indfst0.lt.1) indfst0=1
		isec0=1 + (indfst0-1)/nbuf     	!section # for point #indfst
	   endif
	   idev=0			!screen
	   rms1=sqrt(vary)	!in intermed units
	   call INIPLT(idev,.false.,1.0)		!Hgraph mode 18
c Invisible call to ARROWiv to define values used in VARROW (in MARK)
	   call ARROWiv(0,445,0,380,0.3,20,0)
c	   goto 102		!skip inscan
	   goto 249		!skip inscan
	endif
c
100	continue	!jump here for new params
c
	call INSCAN(iampl,tint,iprops,ntmax,nbuf,novlap,srate,
     & finter,tsect,ioff,ndat,nsec,nrlast,savin,nshutfit,
     & kt,nwrit,nwsav,newpar,igback,restart,opendown,invert,
     & usepots,disp,smult,scrit,vary,ntrig,navtest,dgain,iboff,
     & expfac,bdisp,ibflag,iautosub,xtrig,ndev,cdate,adctime,
     & nsetup,filtfile,filt1n,nfilt1n,dtf1n,timamax,avamp,ampdone,
     & tf01,tf1,tf50,tf99,tf999,npfilt,ffilt,filt1z,nfilt1z,dtf1z,
     & yd1,yd2,sfac1,sfac2,sfac3,nscale,calfac,calfac1,calfac2,iyoff,
     & dfinter,disptran,dispderiv,dispguess,ampfac,tmin,
     & tsfac,tlfac,tcfac,sdone,iscanver,facjump,errfac,confac,
     & derivfac,idest,izoom,fcz,fczoom,ampz,avampz,filtzoom,
     & cjump,njump,ktjump,nfjump,tzero,intzero,tsample,istrec,
     & njdim,jumplen,njfirst,filtf)
c Set normal filter initially
	   nfilt1=nfilt1n
	   dtf1=dtf1n
	   do i=1,nfilt1
		filt1(i)=filt1n(i)
	   enddo
c
	if(.not.cjump) then
	   njdim=1   !must allocate arrays anyway as they are args for subroutines
	   if(.not.allocated(jumps)) then
		ALLOCATE(jumps(njdim),kjumps(njdim))
	   endif
	endif
	if(cjump.and.(.not.newpar)) then
	   tzerod=dfloat(intzero)*dfinter		!tzero in microsec
	   njfit=0			!no fit yet
	   ijump=1
	   ijumpsav=ijump
	   if(.not.allocated(jumps)) then
		ALLOCATE(jumps(njdim),kjumps(njdim))
	   endif
	   jumps(ijump)=njump	!ijump=1 here
	   kjumps(ijump)=1	!=kt for latency for 1st sweep
c	  Calc factor such that, when divide by ndisp, gives jump length
c		in pixels for display in MARK
	   tjlen=float(jumplen)*640./finter
	endif
	if(idest.eq.99) goto 999
c Set arbitrary value of avamp (2.5pA) to prevent crashes in NEWLEN etc
c before avamp is defined
	if(navamp.eq.0) then
	   avamp=2.5/calfac2
	   if(opendown) avamp=-avamp
	   avampsav=avamp
	endif
c Allocate arrays for point amplitude histograms, now sfac2 defined
C 1401 units are -32768 t0 +32752 in 16 unit increments
c NB if data were recorded with baseline right at one edge of the 1401
c input range, then an amplitude could equal the entire range, so need
c to make ifreq() arrays big enough to hold whole range in either
c positive OR negaitve direction, i.e. twice the max range
	if(.not.newpar) then
	   imax=2*ifix(32768.*abs(sfac2))+1	!round up (use abs in case dgain & sfac neg)
	   imin=-imax
c==	   imax=ifix(32752.*sfac2)+1 		!round up
c==	   imin=-ifix(32768.*sfac2)-1		!round down
c	   cmax=float(imax)*calfac2    !max pA
c	   cmin=float(imin)*calfac2    !max pA
c	   irange=imax-imin+1
	   ALLOCATE(ifreq1(imin:imax),ifreq2(imin:imax),ifreq3(imin:imax))
	   do i=imin,imax
		ifreq1(i)=0
		ifreq2(i)=0
		ifreq3(i)=0
	   enddo
	endif
c
	if(newpar) then
c	   newpar=.false.	!set false later (needs to be true to avoid READSEC)
	   iampcrit=iabs(ifixr(ampfac*avamp))	!in case ampfac changed
	   idev=0			!screen
	   call INIPLT(idev,.false.,1.0)		!Hgraph mode 18
c	   call MODE(18)	!back to graphics
c	   call OPENDIALOG(1,1,.true.)	!redraw dialog box -after FILLWIN!
	   if(ibflag.eq.1) then		!baseline position reset
		ibflag=0
	      basevga=yd1 + bdisp*(yd2-yd1)	!BASELINE AFTER OFFSET BELOW (pixel units)
	      ybasevga=base*sfac3 + float(iyoff)	!convert to display (pixel) units
	      ioff1=ifixr(basevga-ybasevga)		!offset to adjust baseline to bdisp
	      iyoff=iyoff+ioff1		!new offset for next display
	   	if(itrigvga.eq.-1) then
		   if(opendown) then
	         	itrigvga=ifix(basevga-0.12*(yd2-yd1))		!init value for trigger level
		   else
	   	   	itrigvga=ifix(basevga+0.12*(yd2-yd1))		!init value for trigger level
		   endif
	      endif
c          Recalculate changed base and itriglev here (only)
		itriglev=(itrigvga-iyoff)/sfac3	!in intermed units (as idata)
	      base=(basevga-float(iyoff))/sfac3	!convert to intermed units (like curlev etc)
		itriglev=(itrigvga-iyoff)/sfac3	!in intermed units (as idata)
c  Recalculate duration of opening corresponding to trigger level
	      if(kt.gt.0) then
      	 aam=abs((float(itriglev)-base)/avamp)
		 ttold=ttrig
		 call AMAX(aam,trise,ttrig,nerr)
		 if(nerr.eq.0) then
         	   call INTCONV(ifixr(ttrig),cnum)
		   n=NBLANK(cnum)
		   tstring='trig='//cnum(1:n)//microsec
		 else
		   tstring='trig= ?     '
		 endif
	       call WRITBOX(2,tstring,1,10)	!display trigger level
c		 if(abs(ttrig-ttold).gt.5.) then !print only if changes by > 5 mus
c      	    if(pon()) write(7,24) ttrig
c      	    if(discprt) write(8,24) ttrig
c24		    format(
c     &	  ' Threshold corresponds to ',g13.6,' microsecond opening')
c		  endif
	      endif
	   endif
	   goto 250       !redraw all starting at current indfst
	endif
c
102	continue
c Now allocate data array
	ALLOCATE(idata(1-novlap:nbuf+novlap))

c==================================================================
c	OTHER DEFINITIONS  AND  CONSTANTS
C==================================================================

c Enter graphics mode
c	call MODE(18)		!clears screen
	idev=0			!screen
	call INIPLT(idev,.false.,1.0)		!Hgraph mode 18
c Invisible call to ARROWiv to define values used in VARROW (in MARK)
	call ARROWiv(0,445,0,380,0.3,20,0)
c	call VIEWPORT(ivxlo,ivxhi,ivylo,ivyhi)
c	call GRAPHBOUNDARY(ixlo,ixhi,iylo,iyhi)
C================================================================
C	ENTER THE DEFAULT SETTINGS
C================================================================
	ndisp=640    	!NDISPLAY
	ndisp1=ndisp	!normally, but ndisp1 smaller for truncated display
c	incabs=160     	!absolute value of INCREMENT for rolling display
	incabs=16		!slow roll until RMS and baseline done
	inc=incabs     !'working' value   for increment (may be pos or neg)
	indfst=1        	!****ABSOLUTE INDEX for 1st point to display
	if(cjump) then
	   indfst=intzero-ifix(xtrig*float(ndisp))	!show t=0 in standard posn
	   if(indfst.lt.1) indfst=1
	endif
	indlast=indfst+ndisp-1
	indfst0=indfst      !save its value
	isec0=1 + (indfst0-1)/nbuf     	!section # for point #indfst
c SET BASELINE ,AMPLITUDE,THRESHOLD
c Default trigger level now set after label 31
c	if(itrigvga.eq.-1) then		!ie not yet defined
c	   if(opendown) then
c		itrigvga=ifixr(yd1 + 0.25*(yd2-yd1))	!init value for trigger level
c	   else
c		itrigvga=ifixr(yd1 + 0.75*(yd2-yd1))	!init value for trigger level
c	   endif
cc itrigvga is in pixel units for display
c	   itriglev=(itrigvga-iyoff)/sfac3	!in intermed units (same as idata)
c	endif
c
249	continue		!jump here after restart (or amp histo?)
c Initialise for ADCs
	do i=0,15
	    ichan(i)=0
	    iadc(i)=0
	enddo
	ichan(0)=0
	nadc=1	!read channel 0 only
c initialise fstdisp,lastdisp etc (used only for part-screen displays at
c section boundaries, when novlap is not big enough to cope)
	fstdisp=.false.
	ixoff=1
	lastdisp=.false.
c Initialize for zoom
	nscale1=nscale	!for zoom
	iyoffz=iyoff	!for zoom
	izoomfac=1		!for zoom	!set to =izoom if zoomed in expmode
	cfac1=calfac1	!for zoom
c Check errfac (problem with old SCANTEMP etc which do not contain a value)
	if(errfac.gt.0.02.or.errfac.lt.0.0001) errfac=0.005
	if(derivfac.lt.0.5.or.derivfac.gt.5.0) derivfac=3.0
	if(confac.lt.0.2.or.confac.gt.0.6) confac=0.5
c
c jump here after parameters reset (newpar)
250	continue
c
c Calcs for progress line
C	FOR DRAWING THE THREE CURSORS ON THE SCREEN AND THE FRAME INSIDE
C	WHICH WE MOVE THE PROGRESS LINE:
C		1.MARKER (BLUE CURSOR) : FIX, SHOW THE ABSOLUTE POSITION IN
C			THE FILE AT WHICH YOU START THE ANALYSE
C		2.PROGRESS LINE(WHITE CURSOR):MOVES AUTOMATICALLY
C			WITH THE FAST SCROLLING
C		3.RED CURSOR:MOVES MANUAL TO THE DESIRED POSITION IN THE FILE
C	THIS IS DONE HERE BECAUSE IF YOU WANT,EVENTUALLY TO START
C	AT A DESIRED POSITION TO NOW THE TOTAL NUMBER OF VALUES IN THE
C	RECORD
C==================================================================
	ibdx=ifix(640./float(nsec))     ! pixels per section for prog line
	ibdpp=nbuf/ibdx		! data points per pixel on progress line
	ibproc=ifixr(float(ibdx*nrlast)/float(nbuf))  !pixels for last section
	ixframe=ibdx*(nsec-1)+ibproc	!total pixels inc last section
	ixbox=0				!x origin for prog line frame
c	ibabs=ndat/ixframe      	!data points/pixel on prog line (=ibdpp)
	iyframe=472    	      	!top edge of progress line
	icframe=0          		!prog line frame colour
	if(mono) icframe=8		!monochrome screen
	icprog=15         		!PROGRESS LINE COLOR
	ixcr=0  			! X FOR RED CURSOR
	iycr=iyframe-6          ! Y FOR RED CURSOR
	iccr=13                 ! RED CURSOR COLOR
	ifabs0=1 + float(indfst0)/ibdpp
	ixmark=ifabs0-1         ! X MARKER
	iymark=iyframe+8        ! Y MARKER
	icmark=11               ! COLOR MARKER
c==	iprogx0=1			!starting X-coord (pixel #) for prog line
c end of progress line calcs
c
C========================================================
C	Having indfst, calc section #, and index (isdfst) within the section
C=====================================================

	call CLEARS(0,480,ibk)	!fill screen with light blue (for outside data area)
251	continue
	call FILLWIN(0,iyd1,639,iyd2,ibk1)	!fill data area with colour ibk
	call HLINE(0,639,iyd1-1,14)		!line to mark bottom of data area
	call HLINE(0,639,iyd2+1,14)	!line to mark top of data area
c==	call CALLINE(ibasevga,calfac1,opendown,ndisp,finter,-1)	!calibration
	if((.not.sdone).or.restart) then
c open a 3 row dialog box top=row 23; cols 2-58
c	subroutine DEFDIALOG(ibox,irhi,iclo,nrow,ncol,icb)
c Draw the box defined by DEFDIALOG
c	subroutine OPENDIALOG(ibox,icf,shadow)
c	subroutine DEFBOX(ibox,irhi,iclo,nrow,ncol,icb)
	   call DEFDIALOG(1,23,2,3,68,icbd)
	   call DEFBOX(1,3,72,1,5,icbd)        !use simpler box for open/shut
	   call DEFBOX(2,20,2,1,12,icbd)       !use simpler box for ttrig
	   call DEFBOX(3,3,67,1,10,icbd)       !and for 'trans # 3' in SETLEVEL
	   if(cjump) call DEFBOX(4,20,71,1,8,icbd)        !for jump #
	   call DEFBOX(5,23,73,2,6,icbd)       !for tic calibration in CALLINE
	   call OPENDIALOG(1,7,.true.)	!shadow border (blue=1) (7=white/grey)
	   if(restart.and.cjump) then		!straight to next section?
c After restart check that last interval was not already set as first
c interval of a null sweep, which was confirmed as being null before exit
c (see case(9)=quit/next jump). If so, warn to go straight to next jump
		restart1=.false.
		restart=.false.		!reset now
		if(kt.eq.ktlast.and.BTEST(iprops(kt),5).and.
     &	   tint(kt).eq.sngl((tsample - tzerod)*1.d-3)) then
		   call BELL(3)
	         call INTCONV(njump,cnum) !transition #
	         call WDIALOG(1,
     & 'Current jump, #'//charnb(cnum)//' is already specified as null',
     &	    icw)
		   ans='Y'
 	         call DEFOLTa(ans,defolt)
       	   call QDIALOG(1,
     &     		'Go straight to NEXT JUMP now. O.K.',
     &	 	  defolt,icw,cans)
		endif
		if(BTEST(iprops(kt),6)) then
		   call BELL(3)
	         call INTCONV(njump,cnum) !transition #
	         call WDIALOG(1,
     & 'Current jump, #'//charnb(cnum)//
     & ' is already specified as completed',icw)
		   ans='Y'
 	         call DEFOLTa(ans,defolt)
       	   call QDIALOG(1,
     &     		'Go straight to NEXT JUMP now. O.K.',
     &	 	  defolt,icw,cans)
		endif
 		call GETINPa(cans,ans)
		if(ans.eq.'Y') then
		   restart1=.true.	!so some bits skipped after 13
		   ikey=9		!next jump etc
		   iline=1		!next jump
		   nval=3
		   goto 13
		endif
		if(BTEST(iprops(kt),5)) then
	         call WDIALOG(1,'START FITTING FROM t=0',icw1)
		endif
	   endif
	   if(.not.sdone) then
	      call WDIALOG(1,
     &	 'GET BIT OF EMPTY BASELINE ON SCREEN FOR SD',icw1)
	      call BELL(2)
	   endif
	endif
c

c RETURN HERE AFTER 'GO TO RED CURSOR' TO (RE)START DISPLAY WITH ARBITRARY indfst
444	CONTINUE	!return here after 'go to red cursor'
c The point with index=IS, idata(is), in section number ISEC has index
c	in = is + (isec-1)*nbuf
c Conversely point with index=IN in the whole data set occurs in section
c	isec = 1 + (in-1)/nbuf
c and has index, within this section,
c	is = in - (isec-1)*nbuf

	isec=1 + (indfst-1)/nbuf     	!section # for point #indfst
	call CALCIND(isec,nbuf,nrlast,novlap,nsec,
     & in1,in2,jn1,jn2,is1,is2,js1,js2,nread,jn1next)  !get indices for section
	isdfst=indfst - (isec-1)*nbuf	!index WITHIN section of point #indfst
	isdlast=isdfst+ndisp-1
c Check that the specified starting point for the display, consam(indfst),
c is not so near to the end of section #isec (just calc) that there are
c too few points to show it (but if no overlap then must show whatever there is!)
C***********************************************************************
c If following condition is true here, display crosses section boundary,
c so goto next section and use its initial overlap part to do display:
c	if(indfst.gt.(in2-ndisp).and.indfst.lt.in2) then
c		isdfst=isdfst-nbuf
c		isdlast=isdfst+ndisp-1
c		isec=isec+1
c	endif         !CONDITION FOR THE LIMIT BETWEEN SECTIONS
c Alter this so overlap in current section is used if long enough. If not
c then use next section (a) if there is one and (b) if it has overlap
c long enough to show the specified first point (indfst).  Otherwise show
c truncated display, starting with the right point in current section but
c with fewer than ndisp points.
	ndisp1=ndisp		!unless truncated
	if(indlast.gt.jn2) then	!current section has not got all ndisp points
	   if(isec.lt.nsec.and.indfst.ge.jn1next) then	!use next section
		isec=isec+1
		call CALCIND(isec,nbuf,nrlast,novlap,nsec,
     & 	in1,in2,jn1,jn2,is1,is2,js1,js2,nread,jn1next)	!recalc indices
	   else 			!show fewer than ndisp points from current section
		ndisp1=jn2-indfst+1
	   endif
c===later:	put up red box to say display truncated?!
	endif
c jfst no longer needed with AIVPIX0 (see above); just call with idata(isdfst)
c where isdfst=index, within current section, of first point to be displayed
c
c jump here for NEXT SECTION
333   continue
c READ IDATA AND SCALE INSIDE READSEC
	if(newpar) then		!section already read in
	   call OPENDIALOG(1,7,.true.)	!shadow border (blue=1) (7=white/grey)
	   newpar=.false.
	else
	   call WDIALOG(1,
     &	'Reading data from disk . . .',-icw1)
	   call READSEC(isec,idata,nrlast,nsec,ioff,nbuf,nread,novlap,
     &     sfac2,in1,in2,jn1,jn2,is1,is2,js1,js2,cjump,invert)
	   filtdone=.false.		!this section not yet filtered
	   zoomed=.false.		!reset tf1 etc for unfiltered data
	   call SETFILT(zoomed,filtzoom,ffilt,fczoom,tf1,tf50,tf99,
     &    npfilt,nshutfit,scrit,iampcrit,finter,facjump,
     &    smult,ampfac,avamp,avampsav,avampz,filt1,nfilt1,
     &    dtf1,filt1z,nfilt1z,dtf1z,filt1n,nfilt1n,dtf1n)
c         Erase message (by writing again in backround col, icbd, to avoid
c		problems with string length in wdialog)
	   call WDIALOG(1,
     &	'Reading data from disk . . .',-icbd)
	endif
c Parameters for progress line (move_prog_line in IOLIB) (NB only upper line
c moves backwards -call with flag_pline=0 to move both lines, and flag_pline=1
c to move upper line only):
c (iprogx1,iprogx2 were prev isecf,isec1)
c ipxs not used now
c	ipxs=(isec-1)*ibdx+1	!ibdx=pixels/section on prog line
c					!so ipxs=pixel # for 1st point in section #isec
	iprogx1=1 + indfst/ibdpp	!current X coord (# of pixels) for upper prog line
	iprogx2=iprogx1			!ditto for lower line (does not move back)
c
	backward=.false.
	end=.false.
c
c JUMP HERE WHEN NDISP changed
5	continue
c Jump to 31 to calc baseline and rms noise when baseline OK
c note- ybase, vary are in intermed units, without offset
31	if(baseOK.and.(.not.sdone)) then
	   triggered=.false.
	   accepted=.false.
c Calc mean and var of first derivative also, for use in FITSUB (use dx=1
c for now, and rescale mean and var as required later).
c	   call IVAR2(idata1,ndfit,ybase,vary)
	   call IVAR3(idata0,ndfit,1,ybase,vary,ybard,vard)
	   rms1=sqrt(vary)		!in intermed units
	   rms=rms1*calfac2  		!in pA (calfac2 for intermed to pA)
c set temporary scrit here as 4*rms to use for detection of 1st full amp
c -then reset scrit=smult*avamp
	   scrit=4.*sqrt(vary)	!in intermed units
c baseline kept in display units (pixels=0-480), but keep as floating point
	   basevga=yd1 + bdisp*(yd2-yd1)	!BASELINE AFTER OFFSET BELOW (pixel units)
c ybase= abs position of baselin in 'intermed units' (no offset, so 0=0pA)
c Insert def of diplay values here, before 'O.K. ?'
c   iyoff, ioff1 must be in pixel units
	   ybasevga=ybase*sfac3 + float(iyoff)	!convert to display (pixel) units
	   ioff1=ifixr(basevga-ybasevga)		!offset to adjust baseline to bdisp
	   iyoff=iyoff+ioff1		!new offset for next display
	   if(itrigvga.eq.-1) then
		if(opendown) then
	         itrigvga=ifix(basevga-0.12*(yd2-yd1))		!init value for trigger level
		else
	   	   itrigvga=ifix(basevga+0.12*(yd2-yd1))		!init value for trigger level
		endif
	      itriglev=(itrigvga-iyoff)/sfac3	!in intermed units (same as idata)
	   else
		itrigvga=itrigvga-ioff1			!reset trigger level too
		itriglev=(itrigvga-iyoff)/sfac3	!in intermed units (as idata)
	   endif
	   base=(basevga-float(iyoff))/sfac3	!convert to intermed units (like curlev etc)
c  Recalculate duration of opening corresponding to trigger level
	   if(kt.gt.0) then
      	aam=abs((float(itriglev)-base)/avamp)
		ttold=ttrig
		call AMAX(aam,trise,ttrig,nerr)
		if(nerr.eq.0) then
         	   call INTCONV(ifixr(ttrig),cnum)
		   n=NBLANK(cnum)
		   tstring='trig='//cnum(1:n)//microsec
c		   if(abs(ttrig-ttold).gt.5.) then !print only if changes by > 5 mus
c      	     if(pon()) write(7,24) ttrig
c      	     if(discprt) write(8,24) ttrig
c		   endif
		else
		   tstring='trig= ?     '
		endif
	      call WRITBOX(2,tstring,1,10)	!display trigger level
	   endif
	   incabs=160			!fast roll now
	   if(newrms) then
c		newrms=.false.			!reset -later, to avoid CLRDIALOG!
		indfst=indfst1			!start again from orig position
		indlast=indfst+ndisp-1
		isdfst=isdfst1
		isdlast=isdfst+ndisp-1
c Do NOT go back to start, except for cjump case for which we should return
c to t=0 after doing 'full amp'
	   else if(cjump) then
		indfst=indfst0			!start again from beginning
		indlast=indfst+ndisp-1
		isdfst=indfst0 - (isec0-1)*nbuf	!index WITHIN section of point #indfst
		isdlast=isdfst+ndisp-1
	   endif
c	   iprogx1=1 + indfst/ibdpp	!current X coord (# of pixels) for upper prog line
c	   iprogx2=iprogx1			!ditto for lower line (does not move back)
c	   call NEWPROGLINE()	!restore prog line
c	   call DRAWPROG(ixbox,ixframe,iyframe,icframe,iprogx1,iprogx2,
c	&   icprog,ixcr,iycr,iccr,ixmark,iymark,icmark,ibdx,ibk,iyd2)
	   call MOVEPROG(indfst,backward)	!other params in common/prog/
c and display shifted data before asking if OK
c	   call AIVPIX4(ix,idata(isdfst),ndisp1,idata(isprev),
c     &	ipen,ifirst,ibk1,nscale,iyoff,iyoffprev)
	   call AIVPIX5(ix,idata(isdfst),ndisp1,idata(isprev),
     &	ipen,ifirst,ibk1,nscale,iyoff,iyoffprev,ndprev)
	   isprev=isdfst	!record value in case needed in DELPIX
	   ndprev=ndisp1	!ditto
	   iyoffprev=iyoff
	   ibasevga=ifixr(basevga)
	   call RDGWIN1(0,ibasevga,639,ibasevga,isavbase)  !keep data overwritten
	   call HLINE(0,639,ibasevga,icbl)	   !show baseline
	   if(nampmark.gt.0) then
		do i=1,nampmark
		   iy=ibasevga+iamark(i)	!sign of iamark set in inscan
		   call DHLINE(0,639,iy,10,22)
		enddo
	   endif
c end of display
	   x=ybase*calfac2	!in pA (calfac2 for intermed units to pA)
	   call DCFORMAT(rms,8,3,cnum)
	   call WDIALOG(1,'RMS baseline noise (pA)'//cnum,icw)
	   call WDIALOG(1,'Baseline set correctly [Y] ?',icw)
c         print 33,rms,x				!in pA
         if(pon()) write(7,33) rms,x		!in pA
         if(discprt) write(8,33) rms,x		!in pA
33	   format(' RMS baseline noise (pA)= ',g11.4,/,
     &	' Baseline set to (pA)= ',G13.6)
	   call BELL(1)
c	   read 101,ans
c NB -the 'do while' form will jump straight out, without waiting for
c character, if ans='Y','N' or ' ' before 'do while' on arrival here
c	   do while (ival.ne.78.and.ival.ne.89.and.ival.ne.13)      !N,Y, enter
32	   ans=UC(GETCH(ktype))
	   ival=ICHAR(ans)
c	   enddo
	   if(ival.ne.78.and.ival.ne.89.and.ival.ne.13) goto 32     !N,Y, enter
c	   call CLRDIALOG(1,ibk)	!clear dialog box #1
	   if(newrms) then
		newrms=.false.			!reset
c	   else
c	      call CLRDIALOG(1,0)		!clear dialog box #1 to black
	   endif
	   call FILLWIN(0,iyd1,639,iyd2,ibk1)	!clear data area
	   call CALLINE(ibasevga,calfac1,opendown,ndisp,finter,-1)	!calibration
	   sdone=.true.
	   if(UC(ans).eq.'N') then
		sdone=.false.	!carry on to do again
	      call WDIALOG(1,
     &	 'GET BIT OF EMPTY BASELINE ON SCREEN FOR SD',icw1)
	      call BELL(2)
		goto 78
	   endif
c ybasevga= up to NEWPROGLINE was here -now above 'OK?'
	   if(.not.ampdone) then
	      call WDIALOG(1,
     &  'NOW FIT A GOOD FULL OPENING FOR AMPLITUDE (F2=help)',icw1)
	      call BELL(2)
	   else
	      scrit=smult*abs(avamp)		!in intermed units
	   endif
c Don't go back to start
c=	   if(isec.ne.isec0) then
c=		isec=isec0
c=		goto 444	!read in 1st section again
c	   else
c still in current section -new iyoff should be used for next display
c	      do i=js1,js2
c		  idata(i)=idata(i) + ioff1
c	      enddo
c=	   endif
	endif		!end of 'baseOK.and.(.not.sdone)'
c
c Draw progress line (+marker, red cursor)
78	continue
c=========check this is not in loop!! -put higher?
c Draw the progress line
	call DRAWPROG(ixbox,ixframe,iyframe,icframe,iprogx1,iprogx2,
	& icprog,ixcr,iycr,iccr,ixmark,iymark,icmark,ibdx,ibk,iyd2)
c	call put_cursors(0,ixframe,iyframe,icframe,iprogx1,icprog,
c	&		     ixcr,iycr,iccr,IXMARK,IYMARK,icmark,ibdx,ibk)
c	CALCULATE IX=X array for display (outside loop) so ix()= 0 to 639
781	continue
	dx=639.0/float(ndisp-1)		!use ndisp (not ndisp1) here
	dxsav=dx
	do i=1,ndisp
	   ix(i)=ifixr(float(i-1)*dx)	!=0 for 1st point
	   ixsav(i)=ix(i)
	end do
c===?	ifirst=1		!no erase in AIVPIX1 first time (which resets it to 0)
c
c Do initial display so there is something on screen when prog stops (in
c scroll_bar) at initial menu. Don't increment indfst so same display repeated
c in loop when roll forward started (so first INC points are searched for trigger)
cc	call AIVPIX1(ix,idata,ndisp,jfst,inc,ipen,ifirst,ibk)
c	call AIVPIX0(ix,idata(isdfst),ndisp1,inc,ipen,ifirst,ibk)
c	call AIVPIX3(ix,idata(isdfst),ndisp1,idata(isprev),
c     &	ipen,ifirst,ibk1)
c	call AIVPIX4(ix,idata(isdfst),ndisp1,idata(isprev),
c     &	ipen,ifirst,ibk1,nscale,iyoff,iyoffprev)
	call AIVPIX5(ix,idata(isdfst),ndisp1,idata(isprev),
     &	ipen,ifirst,ibk1,nscale,iyoff,iyoffprev,ndprev)
c Draw 'up to here marker' if it is on screen
c For marking sequences with several arrows, call MARK with ixfprev=-1
c so 2nd call does not delete previous arrow
	if(.not.seqdisp) then
	   call MARK(infit,indfst,ndisp1,ixfprev,ix,14,ibk1,
     &     cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
	else
	   call MARK(inseq1,indfst,ndisp1,-1,ix,12,ibk1,
     &       cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
	   call MARK(inseq2,indfst,ndisp1,-1,ix,12,ibk1,
     &       cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
	endif
	isprev=isdfst	!record value in case needed in DELPIX
	ndprev=ndisp1	!ditto
	iyoffprev=iyoff
	if(cjump) then
	   call OPENBOX(4,7,.true.)	!draw box 4, for jump #
	   call INTCONV(njump,cnum)
	   call WRITBOX(4,'Jump '//charnb(cnum),1,14)
	endif
	if(kt.gt.0) then
	  call OPENBOX(1,7,.true.)	!draw box 1, for 'open'/'shut'
c	  if(shut) then			!display final level
c	     call WDIALOG(2,'SHUT',11)
c	  else
c	     call WDIALOG(2,'OPEN',12)
c	  endif
	  if(shut) then			!display final level
	     call WRITBOX(1,'SHUT',1,11)
	  else
	     call WRITBOX(1,'OPEN',1,12)
	  endif
	  call OPENBOX(2,7,.true.)
	  call WRITBOX(2,tstring,1,10)	!duration equiv to trigger level
	endif

	if(sdone) then
c==	   do i=0,639
c==		call rpixel(i,itrigvga,iabar3(i))
c==      end do
c	   call timer(n1)
c	   do i=1,10000
c	      call RDGWIN1(0,itrigvga,639,itrigvga,isavtrig)  !keep data overwritten
c	   enddo
c	   call timer(n2)
c	   print*,'time (ms) = ',1.*float(n2-n1)
	   call RDGWIN1(0,itrigvga,639,itrigvga,isavtrig)  !keep data overwritten
	   call HLINE(0,639,itrigvga,ictrig)   !Show threshold line
	   ibasevga=ifixr(basevga)
	   call RDGWIN1(0,ibasevga,639,ibasevga,isavbase)  !keep data overwritten
	   call HLINE(0,639,ibasevga,icbl)	   !show baseline
	   if(nampmark.gt.0) then
		do i=1,nampmark
		   iy=ibasevga+iamark(i)	!sign of iamark set in inscan
		   call DHLINE(0,639,iy,10,22)
		enddo
	   endif
	endif

22	CONTINUE		!return here to REROLL
c==	kx=152
c write section number (?)
c	call set_cursor(ixstr,iystr)
c	call det_string(pch(isec),icstr,ch)
c
c Read initial values of ADC for rolling display

	if(usepots) call READAMP(ichan,nadc,iadc0)
c
25	continue
			!LOOP WHEN YOU STOP FROM BACKWARDS LOOP
			!STOP (MANUAL/AUTOMATICALLY) FROM FORWARDS LOOP
			!get to END of data
c STOP option returns here and waits for new option
c	stopped=.false. !necessary? Don't reset here as it is needed in GETOPT?
c	call SET_MENU(iXIN1,iYIN1,iXUP1,iYUP1,TXTWIN1,iFWIN1,iCTXT1,
c     &jX1,jY1,kX)
c	nout1=1
c===========
150	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	if(cjump.and.ktjump.eq.0) then
	   Lb(13)=1
	else
	   Lb(13)=0
	endif
	Lb(13)=0	!remove 'skip jump' until fixed properly!
	if(.not.sdone) then
	   Lb(4)=0		!draw separately
	   Lb(6)=0		!no trigger
	   Lb(7)=0        !no fitting yet
	endif
c	call DCMENU(0,4,Lb,text,0,0)		!delete all
151	continue
	text(1)='1: ROLL ON     '
	text(2)='2: ROLL BACK  '
	text(3)='3:EXPAND MODE'
	if(sdone) then
	   text(4)='4: FIT  (AUTO)'
	else
	   text(4)='4.ACCEPT BASELINE'
	endif
	text(5)='5: GO TO . . . '
	text(6)='6:TRIGGER LEVEL'
	text(7)='7: FIT (MANUAL)'
	text(8)='8: STOP ROLL'
	text(9)='9: QUIT'
	if(cjump) text(9)='9:QUIT/ NEXT JUMP'
	text(10)='0:ADJUST BASELINE'
c	text(10)='10: QUEUE PLOT '
	text(11)='+:FASTER ROLL'
	text(12)='-:SLOWER ROLL'
	text(13)='*:SKIP THIS JUMP'
	text(14)='/:SCROLL DIALOG'
	text(15)='.:MORE OPTIONS'
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
c	call DCMENU(-4,4,Lb,text,0,0)	!delete box 4 only
c	Lb(4)=-1		!italic
	if(.not.sdone) call DCMENU(-4,4,Lb,text,icol3,icol4)	!draw box 4 only
c
30	continue
c152	ikey=nkey()
152	call CKEY(ch,ikey)
	if(ikey.lt.-2.or.ikey.gt.15) goto 152
c==	if(ikey.eq.-1) ikey=15		!F1=help
	if(ikey.eq.13) goto 152				!invalid key -OK now?
	if(.not.sdone.and.(ikey.eq.6.or.ikey.eq.7)) goto 152    !invalid key
	if(seqdisp.and.ikey.ne.3) then	!delete red arrows -draw in ibk1
	   call MARK(inseq1,indfst,ndisp1,-1,ix,ibk1,ibk1,
     &       cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
	   call MARK(inseq2,indfst,ndisp1,-1,ix,ibk1,ibk1,
     &       cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
	endif
	if(ikey.eq.-1) then	!F1=help!
		helpst(1)=
     &	  '1.ROLL ON: Roll data forwards (alter rate with +/-).'
		helpst(2)=
     &	  '2.ROLL BACK: Roll data back   (alter rate with +/-).'
		helpst(3)=
     &	  '3.EXPAND MODE: expand data, ready for fitting.      '
	   if(sdone) then
		helpst(4)=
     &	  '4.FIT (AUTO): Fit all transitions shown on screen   '
		helpst(5)=
     &	  '  automatically (usually better to EXPAND first).   '
	   else
		helpst(4)=
     &	  '4.ACCEPT BASELINE: hit when screen is showing only a'
		helpst(5)=
     &	  '  piece of ''typical'' baseline with no transitions.'
	   endif
		helpst(6)=
     &	  '5.GO TO ...: Options to jump to elsewhere in data'
		helpst(7)=
     &	  '6.TRIGGER LEVEL: Adjust level of threshold line,  '
		helpst(8)=
     &	  '  crossing of which causes data to stop rolling.   '
		helpst(9)=
     &	  '7.FIT (MANUAL): Fit all transitions shown on screen'
		helpst(10)=
     &	  '  manually (usually better to EXPAND first).      '
	   if(.not.cjump) then
		helpst(11)=
     &	  '9.QUIT: leave program and store data.      '
		helpst(12)=
     &	  '+ or -: Adjust the roll rate.                      '
		helpst(13)=
     &	  '/:SCROLL DIALOG: Scroll the text in the dialog box up'
		helpst(14)=
     &	  '  or down, to see earlier text that is off-screen now'
	   else
		helpst(11)=
     &	  '9.QUIT/NEXT JUMP: read in the next jump, OR         '
		helpst(12)=
     &	  '  leave program and store the data.                  '
		helpst(13)=
     &	  '+ or -: Adjust the roll rate.                        '
		helpst(14)=
     &	  '*:SKIP THIS JUMP: omit jump altogether (possible only'
		helpst(15)=
     &	  '  if fitting has not yet been started) NOT WORKING)! '
		helpst(16)=
     &	  '/:SCROLL DIALOG: Scroll the text in the dialog box up'
		helpst(17)=
     &	  '  or down, to see earlier text that is off-screen now'
	   endif
c
	   ntext=14
	   if(cjump) ntext=17
	   call POPTEXT(mxlo,mylo,myhi,helpst,ntext,ictx,ibkx,icfx)
	   call WDIALOG(1,
     &       'Next choose another menu option',-icw)
	   goto 152		!get new menu option
	endif			!end of help
c
	if(ikey.eq.-2.and.(.not.ampdone)) then 	!F2 for help with 'full amp'
	   title='Options'
	   strings(1)='Specify amplitude numerically'
	   strings(2)='Try to fit one  (F1 for help)'
	   nval=2
	   iline=1		!default
	   if(cjump) then
		helps(1)='If the current jump does not have a good'
		helps(2)='opening, try another jump by using the  '
		helps(3)='GO TO..., option ''Start of c-jump #'' to'
		helps(4)='find one that does, fit it and use GO TO '
		helps(5)='again to return to the current jump.     '
		nhelp=5
	   else
		helps(1)='Scroll through data until a good full'
		helps(2)='opening is found and fit it.         '
		nhelp=2
	   endif
	   call BELL(2)
	   call POPMENU(nxlo,nylo,nyhi,strings,nval,ictm,icfm,icupm,
     &      ibkm,title,helps,nhelp,iline,ans,ival)
	   if(iline.eq.1) then
		amp=-5.0		!default
c	      call DCFORMAT(amp,7,2,cnum)
 	      call DEFOLTr(amp,defolt)
c		n=NBLANK(cnum)
	      call QDIALOG(1,'Specify full amplitude (pA with sign)',
     &	 defolt,12,cans)
		call GETINPr(cans,amp)
		avamp=amp/calfac2		!in intermed units
		numamp=.true.
		fitmode=.true.
		goto 200	!complete as though amplitude was fitted
	   endif
	endif
c
c Jump to 13 for 'straight to next jump' after restart (with restart1=true,
c ikey=9, iline=1)
13	continue
c
	select case(ikey)

	   case(1)		!roll on
		if(indlast.ge.ndat) then
      	   call BELL(1)
		   call POPTIME(250,130,'END OF DATA',1,15,12,14,1.5)
		endif
		if(end) goto 30
		inc=incabs		!set increment positive
		backward=.false.
		stopped=.false.

	   case(2)		!roll back
		if(indfst.le.1) then
	         call BELL(1)
		   call POPTIME(250,130,'START OF DATA',1,15,12,14,1.5)
		endif
	      if(indfst.lt.incabs) goto 25
		end=.false.
		inc=-incabs
		backward=.true.

	   case(3)
		goto 800		!expand mode

	   case(4)			!fit/accept baseline
		if(sdone) then	!auto-fit
		   ndfit=ndisp			!copy data for fitting into idata1
		   do i=isprev,isprev+ndisp-1
			j=i-isprev+1
			idata0(j)=idata(i)	!copy data to be fitted into idata0()
		   enddo
		   fitmode=.true.
		   ifitmode=0
		   infirst=isprev + (isec-1)*nbuf !record abs index of 1st point in idata1()
		   goto 200
		else
		   ndfit=ndisp			!copy data for rms/baseline
		   do i=isprev,isprev+ndisp-1
		     j=i-isprev+1
		     idata0(j)=idata(i)	!copy data for rms/baseline to idata0()
		   enddo
		   call NUMCLR
		   baseOK=.true.	!baseline accepted
		   goto 31
		endif

	   case(5)		!go to red cursor and other go to options
		end=.false.
		triggered=.false.		!in case come to here after trigger set
c		in=indfst
		call GOBACKS(indfst,indlast,ndisp,ibdpp,SHUT,goback,
     &	 shutsav,ixcr,iycr,iccr,ixframe,ibk,icase,prevlevel,
     &	 nfits,timsav,ifits,ibaselin,dfinter,xtrig,ktlast,
     &       kt,base,infit,tlast,iampl,tint,iprops,npfilt,ntmax,nfmax,
     &	 cjump,ijump,kjumps,njdim,ndat,indfst0,ktjump,readnew,
     & 	 njump,njfit,intzero,tzerod,tsample,jumps,ijumpsav,
     &       iyoff,ioff1,ibasevga,basevga,sfac3,seqdisp,inseq1,inseq2)
		iprogx1=1 + indfst/ibdpp	!current X coord (# of pixels) for upper prog line
		iprogx2=iprogx1
		call DRAWPROG(ixbox,ixframe,iyframe,icframe,iprogx1,iprogx2,
	&      icprog,ixcr,iycr,iccr,ixmark,iymark,icmark,ibdx,ibk,iyd2)
		ndisp1=ndisp		!unless truncated
		isec1=1 + (indfst-1)/nbuf     	!section # for point #indfst
		ndisp1=ndisp		!unless truncated
		call CALCIND(isec1,nbuf,nrlast,novlap,nsec,
     & 	 in1,in2,jn1,jn2,is1,is2,js1,js2,nread,jn1next)  !get indices for section
		isdfst=indfst - (isec1-1)*nbuf	!index WITHIN section of point #indfst
		isdlast=isdfst+ndisp-1
		if(indlast.gt.jn2) then	!current section has not got all ndisp points
		   if(isec1.lt.nsec.and.indfst.ge.jn1next) then	!use next section
			isec1=isec1+1
			call CALCIND(isec1,nbuf,nrlast,novlap,nsec,
     &	 	in1,in2,jn1,jn2,is1,is2,js1,js2,nread,jn1next)	!recalc indices
		   else 			!show fewer than ndisp points from current section
			ndisp1=jn2-indfst+1
		   endif
c===later:	put up red box to say display truncated?!
		endif
c Now need section #isec1 (if section changed, AIVPIX won't erase data so
c call FILLWIN here to erase)
c In case of cjump, may need to read in a different jump now (but for
c 'go to most recent fit', may now be re-reading the current section here)
		if(cjump) then		!readnew is set in GOBACKS
		   readnew=readnew.or.(isec1.ne.isec)
	         ir=int4(istrec(njump))
	         if(ir.le.-1.and.ir.ge.-32768) then
	            ir=ir+65536
	         else if(ir.lt.-32768) then
	            print*,' IR IS TOO BIG!!'
	            STOP
	         endif
	         ioff=512*ir
c		   ioff=512*int4(istrec(njump))  !offset to read data in READSEC
		else
		   readnew=isec1.ne.isec	!normal case
		endif
		if(readnew) then
		   call FILLWIN(0,iyd1,639,iyd2,ibk1)	!fill data area
		   call CALLINE(ibasevga,calfac1,opendown,ndisp,finter,-1)	!calibration
		   ixfprev=-1	!x value where previous trigger point was drawn
		   ix0prev=-1	!x value where previous t=0 point was drawn (cjump)
		   ifirst=1	!no erase needed in AIVPIX (which resets it to 0)
		   isec=isec1		!new section number
		   call WDIALOG(1,
     &		'Reading data from disk . . .',-icw1)
		   call READSEC(isec,idata,nrlast,nsec,ioff,nbuf,nread,novlap,
     &	     sfac2,in1,in2,jn1,jn2,is1,is2,js1,js2,cjump,invert)
		   filtdone=.false.		!this section not yet filtered
		   zoomed=.false.		!reset tf1 etc for unfiltered data
		   call SETFILT(zoomed,filtzoom,ffilt,fczoom,tf1,tf50,tf99,
     &	    npfilt,nshutfit,scrit,iampcrit,finter,facjump,
     &	    smult,ampfac,avamp,avampsav,avampz,filt1,nfilt1,
     & 	   dtf1,filt1z,nfilt1z,dtf1z,filt1n,nfilt1n,dtf1n)
		   call WDIALOG(1,
     &		'Reading data from disk . . .',-icbd)	!erase
		endif
		isdfst=indfst - (isec-1)*nbuf	!index WITHIN section of point #indfst
		isdlast=isdfst+ndisp1-1
	      indsav=indfst				!update indfst
	      isdsav=isdfst
	      ndsav=ndisp1
		if(icase.eq.4.or.icase.eq.5) then	!'redo' options
		   call BELL(1)
		   call WDIALOG(1,
     &		'Now continue fitting from here',-icw)
		endif
c		goto 444
		goto 781		!or to 5?

	   case(6)		!Set trigger level
c		call set_cursor(ixmes,iymes)
c		call det_string('<Enter>=continue; \/=down; /\=up',
c     &	 14,ch)
		string(1)=
     &       'Move trigger level with up/down arrows; <Enter> when done'
		call WINDIM(ix1,iy1,ix2,iy2,idimen,string,1,ixc0,iyc0)
		ALLOCATE(array1(idimen))
		call RDGWIN1(ix1,iy1,ix2,iy2,array1)  !Store orig in array1
		call FRAMEFILL(ix1,iy1,ix2,iy2,1,14)
		call WRSTRING18(ixc0,iyc0,string,1,14)
		call MOVELINE(itrigvga,ictrig,isavtrig,ikey1)
c		call MOVE_LINE(itrigvga,ictrig,iabar3,ii)
		itriglev=(itrigvga-iyoff)/sfac3	!in intermed units (as idata)
c Remove POPUP and redraw orig screen
		call WRGWIN1(ix1,iy1,ix2,iy2,array1)
		deallocate(array1)
c recalculate duration of opening at trigger level
		aam=abs((float(itriglev)-base)/avamp)
		ttold=ttrig
		call AMAX(aam,trise,ttrig,nerr)
		call OPENBOX(2,7,.true.)
		if(nerr.eq.0) then
         	   call INTCONV(ifixr(ttrig),cnum)
		   n=NBLANK(cnum)
		   tstring='trig='//cnum(1:n)//microsec
		else
		   tstring='trig= ?     '
		endif
	      call WRITBOX(2,tstring,1,10)	!display trigger level
c		if(abs(ttrig-ttold).gt.5.) then !print only if changes by > 5 mus
c      	   if(pon()) write(7,24) ttrig
c      	   if(discprt) write(8,24) ttrig
c		endif
		goto 30

	   case(7)		!FIT (manual)
		ndfit=ndisp			!copy data for fitting into idata0
		do i=isprev,isprev+ndisp-1
		   j=i-isprev+1
		   idata0(j)=idata(i)	!copy data to be fitted into idata0()
		enddo
		fitmode=.true.
		ifitmode=2
		infirst=isprev + (isec-1)*nbuf !record abs index of 1st point in idata1()
		goto 200

	   case(8)		!stop
		stopped=.true.
		goto 25

	   case(9)		!quit/read next jump
		call NUMCLR
c		call BELL(2)
		if(cjump) then
c		   Finished a sweep now: (A) if it contained transitions then
c		   set the last interval (t=0 to end), otherwise (B) set entire
c		   sweep as a first latency
c		   First check that we have not gone back to earlier section -
c		   -if so, should use 'go to most recent fit' to return to
c			current section (or, if nothing yet fitted in current
c			sweep, so bit 5 ('32') set for most recent transition,
c			must use 'goto to start of jump # ijump')
		   if(restart1) then	!straight to next jump after restart
			goto 14
		   endif
		   nj=jumps(ijump)
		   if(njump.lt.nj) then
			call INTCONV(nj,cnum)
			n=NBLANK(cnum)
			call BELL(2)
			if(kt.eq.ktlast) then
			  helpst(1)='YOU ARE NOT IN THE CURRENT JUMP'
			  helpst(2)='Use the 5.GO TO... option to go'
			  helpst(3)='to ''Start of c-jump #'' '//cnum(1:n)
			  nh=3
			else
			  helpst(1)='YOU ARE NOT IN THE CURRENT JUMP'
			  helpst(2)='Use the 5.GO TO... option to '
			  helpst(3)='''Go to most recent fit'' or to'
			  helpst(4)='''Start of c-jump #'' '//cnum(1:n)
			  nh=4
			endif
			call POPTEXT(mxlo,mylo,myhi,helpst,nh,ictx,ibkx,icfx)
			goto 150		!main menu
		   endif
c              (A) Add the interval from last transition to end of sample
c                At this point kt already points to last interval and
c		    iampl(kt) already defined, so need to define only
c		    tint(kt) and iprops(kt).  Do NOT increment kt here -after 1st
c		    there is one extra value (compared with steady state analysis)
c		    and this is the latency which is inserted after acceptance
c		    of first fit in a jump (but for cjump set nint=kt, rather than
c		    kt-1, so this last value is written to disk).
c             (B) Check whether last sweep had no openings -if so kt will
c		    not have been incremented and no tint() values set, so
c		    set 1st latency  (=sample length-tzero) for it now
c              NB do not set tint etc yet, in case this is not end of
c		   sweep, but EXIT hit below with view to restarting later -set
c		   then only if NEXT SWEEP used.  But if this is FINAL exit
c		   then DO want to set them -so best set them here, but reset
c		   iprops(kt)=0 when restart is done.
c              If 'next jump' was hit last, then kt=ktlast and kjumps() updated
c		   but there is no way to know whether the jump just read in was
c		   actually null, or whether it contains transitions that have
c		   not yet been fitted.  If kt>ktlast then 'last interval' can
c		   safely be set here, but if kt=ktlast then better postpone
c		   action until we decide whether this is 'next jump' or 'exit'.
c=		   if(kt.eq.ktlast) then	!last sweep was blank
c=			nnull=nnull+1
c=			kt=kt+1
c=			tint(kt)=sngl((tsample - tzerod)*1.d-3)   !tzerod is in microsec
c=			iampl(kt)=0
c=			iprops(kt)=IBSET(iprops(kt),5)	!1st lat.-set bit 5='32'
c=		      ktlast=kt
c=		   else		!last sweep not blank
c=		   if(kt.gt.ktlast) then		!last sweep not blank
c=			tint(kt)=sngl((tsample - tlast)*1.d-3)   !all mus
c=			iprops(kt)=IBSET(iprops(kt),6)  !last interval -set bit 6='64'
c=	      	call INTCONV(kt,cnum) !transition #
c=			call TFORMAT(tint(kt),cnum1)
c=			if(iampl(kt).eq.0) then
c=			   cnum2='   0       '              !amplitude
c=			else
c=			   a=calfac2*float(iampl(kt))
c=			   call DCFORMAT(a,6,2,cnum2)		!amplitude
c=			endif
c=			call WDIALOG(1,
c=     & 'Interval after last transition = #'//charnb(cnum)//', '
c=     &  //char(242)//charnb(cnum1)//' ms, '//charnb(cnum2)//' pA',icw)
c=		   endif
c
28		   title='What next?'
         	   call INTCONV(njump+1,cnum)
		   strings(1)='Read the next jump (#'//charnb(cnum)//')'
		   strings(2)='Specify another jump'
		   strings(3)='Exit (and store data)'
		   nval=3
		   iline=1		!default
		   helps(1)='Read another jump  '
		   helps(2)=' -give jump # next '
		   helps(3)='Exit -when all jump'
		   helps(4)=' jumps are analysed'
		   nhelp=4
		   call BELL(2)
		   call POPMENU(nxlo,nylo,nyhi,strings,nval,ictm,icfm,icupm,
     &	   ibkm,title,helps,nhelp,iline,ans,ival)
		   if(iline.eq.0) goto 152			!iline=0 for ESC=cancel
14		   continue	    !jump here for 'straight to next jump' after restart
		   if(iline.ge.1.and.iline.le.nval) then
c EXIT
		      if(iline.eq.3) then		!EXIT
			   if(kt.eq.ktlast) then	!last sweep was blank
c                If leaving without fitting anything to current sweep, but it
c                does contain openings that it is intended to fit after restart,
c                is it necessary to correct nnull, iprops etc here, e.g. by
c                asking if this sweep is in fact null, and if not suggesting
c                that at least on transition is fitted.  Similarly
c			if kt>ktlast, need to ask if all openings have been fitted
c			because if so need to set the last interval here (so
c			now don't need to reset iprops(kt)=0 in READSCAN, which
c			may be incorrect)
				call BELL(1)
		      	call WDIALOG(1,
     &	'No fittings in current jump: options are:',12)
				ans='N'
 		      	call DEFOLTa(ans,defolt)
	      		call QDIALOG(1,
     &	'Is this jump really null (''N'' if not yet checked)',
     &	 	        defolt,12,cans)
				call GETINPa(cans,ans)
				if(ans.eq.'Y') then
				   nnull=nnull+1
				   kt=kt+1
				   tint(kt)=sngl((tsample - tzerod)*1.d-3)   !tzerod is in microsec
				   iampl(kt)=0
				   iprops(kt)=IBSET(iprops(kt),5)	!1st lat.-set bit 5='32'
			         ktlast=kt
				endif
c=				call WDIALOG(1,
c=     &'Fit at least one transition before exit, if there are any!',icw)
c=				ans='Y'
c= 	      		call DEFOLTa(ans,defolt)
c=	      		call QDIALOG(1,'Return to fit transition(s)',
c=     &	 	       defolt,11,cans)
c=				call GETINPa(cans,ans)
c=				if(ans.eq.'Y') then
c=				   nnull=nnull-1        !restore value
c=				   iprops(kt)=0		!ditto
c=				   kt=kt-1			!ditto
c=c==surely BEFORE kt=kt-1
c=c===				   iprops(kt)=0		!ditto
c=			         ktlast=kt
c=				   goto 152
c=				endif
c=			   endif
			   else if(kt.gt.ktlast) then		!last sweep not blank
11				call BELL(1)
		      	call WDIALOG(1,
     &	'Have ALL the openings in current the jump been fitted?',12)
				ians=3
 		      	call DEFOLTi(ians,defolt)
	      		call QDIALOG(1,
     &	'Options: (1) Yes: (2) No: (3) Go back to check',
     &	 	        defolt,12,cans)
				call GETINPi(cans,ians)
				if(ians.lt.1.or.ians.gt.3) goto 11
				if(ians.eq.3) then
				   goto 152
				else if(ians.eq.1) then
				   tint(kt)=sngl((tsample - tlast)*1.d-3)   !all mus
				   iprops(kt)=IBSET(iprops(kt),6)  ! -set bit 6='64'
	      		   call INTCONV(kt,cnum) !transition #
				   call TFORMAT(tint(kt),cnum1)
				   if(iampl(kt).eq.0) then
					cnum2='   0       '              !amplitude
				   else
					a=calfac2*float(iampl(kt))
					call DCFORMAT(a,6,2,cnum2)		!amplitude
				   endif
				   call WDIALOG(1,
     &	 'Interval after last transition = #'//charnb(cnum)//', '
     &  //char(242)//charnb(cnum1)//' ms, '//charnb(cnum2)//' pA',icw)
				endif
			   endif
c
			   goto 27		!exit
			endif
c NEXT JUMP
c Next bit for iline=1,2 (next jump) need to set iprops etc as
c deleted above (c=)
c If 'go straight to next jump' after restart then jumps to here with
c iline=1, restart1=true
			if(restart1) then
			   restart1=.false.
			   goto 15
			endif
c
			if(kt.eq.ktlast) then	!last sweep was blank
			   nnull=nnull+1
			   kt=kt+1
			   tint(kt)=sngl((tsample - tzerod)*1.d-3)   !tzerod is in microsec
			   iampl(kt)=0
			   iprops(kt)=IBSET(iprops(kt),5)	!1st lat.-set bit 5='32'
		         ktlast=kt
			else		!last sweep not blank
			   tint(kt)=sngl((tsample - tlast)*1.d-3)   !all mus
			   iprops(kt)=IBSET(iprops(kt),6)  !last interval -set bit 6='64'
	      	   call INTCONV(kt,cnum) !transition #
			   call TFORMAT(tint(kt),cnum1)
			   if(iampl(kt).eq.0) then
				cnum2='   0       '              !amplitude
			   else
				a=calfac2*float(iampl(kt))
				call DCFORMAT(a,6,2,cnum2)		!amplitude
			   endif
			   call WDIALOG(1,
     & 'Interval after last transition = #'//charnb(cnum)//', '
     &  //char(242)//charnb(cnum1)//' ms, '//charnb(cnum2)//' pA',icw)
			endif
c
15			continue
			if(iline.eq.1) then
			   njump=njump+1			!next sweep #
			else if(iline.eq.2) then
			   njump=njump+1			!next sweep #
			   call DEFOLTi(njump,defolt)
			   call QDIALOG(1,
     &	         'Next sweep number',
     &		   defolt,ictd,cans)
			   call GETINPi(cans,njump)
			endif
c***			irec=int4(istrec(njump))
c File 14 is now opened in transparent mode so byte record # for header is
	            ir=int4(istrec(njump) - 1)
	            if(ir.le.-1.and.ir.ge.-32768) then
	               ir=ir+65536
	            else if(ir.lt.-32768) then
	               print*,' IR IS TOO BIG!!'
	               STOP
	            endif
	            irec=1 + 512*ir
c			irec=1 + 512*(int4(istrec(njump)) - 1) 	!see READJUMP
			read(14,rec=irec) cdate,ctime,title1,naver,navc,
     &		  iav,control,vjump
			call PREC(njump,title1,cdate,ctime,naver,navc,
     &		  iav,vjump,control,iver1,3) !print only -not screen
			ans='Y'
 	      	call DEFOLTa(ans,defolt)
	      	call QDIALOG(1,title1(1:60),
     &	 	defolt,11,cans)
			call GETINPa(cans,ans)
			if(ans.eq.'N') goto 28
c              Now have next jump # = njump -reset values as necessary
c		  NB kjumps() defined as trans # for first latency of next jump
c			here, but iprops() not set to 32 until latency fitted
c			and fit is accepted.  Also set kt=ktlast here.
			ijump=ijump+1
			ijumpsav=ijump
			jumps(ijump)=int2(njump)	 !record jump #
			kjumps(ijump)=int2(kt+1) !index of 1st latency for next jump
	            ir=int4(istrec(njump))
	            if(ir.le.-1.and.ir.ge.-32768) then
	               ir=ir+65536
	            else if(ir.lt.-32768) then
	               print*,' IR IS TOO BIG!!'
	               STOP
	            endif
	            ioff=512*ir
c			ioff=512*int4(istrec(njump))  !offset to read data in READSEC
c               Reset values for next sweep
			shut=.true.		!in case prev sweep ended in open state
			SHUTSAV=.true.	!ditto
			indfst=indfst0
			indlast=indfst+ndisp-1
			isec=isec0
			isdfst=indfst - (isec-1)*nbuf	!index WITHIN section of point #indfst
			isdlast=isdfst+ndisp-1
c=			infit=0	!don't change until another fit done!
			ixfprev=-1	!x value where previous trigger point was drawn
			ix0prev=-1	!x value where previous t=0 point was drawn (cjump)
			ktjump=0
			nfjump=0
			tlast=0.0d0
			ktlast=kt	!needed to detect jumps with no openings
		   	call OPENDIALOG(1,7,.true.)	!shadow border (blue=1) (7=white/grey)
	            call WDIALOG(1,'START FITTING FROM t=0',icw1)
	            call WDIALOG(1,
     &		'(or hit * to skip before fitting started)',icw1)
			goto 251	!start next section
		   endif
		endif
c
27		continue		!Exit (straight to here if not cjump)
	      ans='N'
 	      call DEFOLTa(ans,defolt)
	      call QDIALOG(1,'Do you really want to quit'
     &	 ,defolt,12,cans)
		call GETINPa(cans,ans)
		if(ans.eq.'Y') goto 99

	   case(10)		!adjust baseline
c		NB basevga=standard screen position of baseline, which never changes
c            and so screen position of trigger line, itrigvga, should not
c		 change either.  But if baseline drifts and is reset here, then
c		 base=data value (intermed units) changes, and iyoff=offset
c		 needed to keep display at standard position also changes. And
c		 itriglev=data value for triggering should change by same amount
c		 as base (so difference between them, and hence tstring, same)
	      call WDIALOG(1,
     & 'Adjust baseline level with arrow keys; then <enter>',-icw)
	      ibasevga=ifixr(basevga)
	      call NUMSET
	      call MOVELINE(ibasevga,icbl,isavbase,i)
	      call WDIALOG(1,
     & 'Adjust baseline level with arrow keys; then <enter>',-icbd)	!erase
	      ybasevga=float(ibasevga)	!new ibasevga
	      basevga=yd1 + bdisp*(yd2-yd1)	!BASELINE AFTER OFFSET -same as before
	      ioff1=ifixr(basevga-ybasevga)	!offset to adjust baseline to bdisp
	      iyoff=iyoff+ioff1		!new offset for next display
	   	if(itrigvga.eq.-1) then
		   if(opendown) then
	         	itrigvga=ifix(basevga-0.12*(yd2-yd1))		!init value for trigger level
		   else
	   	   	itrigvga=ifix(basevga+0.12*(yd2-yd1))		!init value for trigger level
		   endif
	      endif
c          Recalculate changed base and itriglev here (only), with new offset
		itriglev=(itrigvga-iyoff)/sfac3	!in intermed units (as idata)
	      base=(basevga-float(iyoff))/sfac3	!convert to intermed units (like curlev etc)
c          Recalculate data, with new offset -NO -done after label 210!
c		call SCALINT1(idata0,ndfit,nscale,iyoff,idata1)
c  Recalculate duration of opening corresponding to trigger level
	      if(kt.gt.0) then
      	 aam=abs((float(itriglev)-base)/avamp)
		 ttold=ttrig
		 call AMAX(aam,trise,ttrig,nerr)
		 if(nerr.eq.0) then
         	   call INTCONV(ifixr(ttrig),cnum)
		   n=NBLANK(cnum)
		   tstring='trig='//cnum(1:n)//microsec
c		   if(abs(ttrig-ttold).gt.5.) then !print only if changes by > 5 mus
c      	      if(pon()) write(7,24) ttrig
c      	      if(discprt) write(8,24) ttrig
c		   endif
		 else
		   tstring='trig= ?     '
		 endif
	      endif
	      call WRITBOX(2,tstring,1,10)	!display trigger level
		call BELL(1)
	      call WDIALOG(1,
     &       'Next choose another menu option',-icw)
	      indsav=indfst		!so indfst set after 210
		isdfst=indfst - (isec-1)*nbuf	!index WITHIN section of point #indfst
		isdsav=isdfst		!so set after 210
		goto 210		!redraw with new baseline
c		goto 152	!get another option

	   case(11)		!Increase roll rate
		   if(incabs.lt.(ndisp-16)) then
			incabs=incabs+16
			inc=incabs
			if(backward) inc=-incabs
		   endif
		   goto 30

	   case(12)		!decrease roll rate
		if(incabs.ge.16) then
		   incabs=incabs-16
		   if(incabs.le.0) incabs=4
		   inc=incabs
		   if(backward) inc=-incabs
		endif
		goto 30

	   case(13) 	!skip jump
		if(ktjump.gt.1) then	!fitting already started
		   call BELL(2)
		   call WDIALOG(1,
     & 	 'Fitting already started so cannot skip now',icw1)
c===could set kjumps(ijump) negative as sign to ignore this sweep later
		else
		   ijump=ijump+1
		   ijumpsav=ijump
		   jumps(ijump)=0		!reset
		   kjumps(ijump)=0	!reset
		   ijump=ijump-1		!reset
		   ijumpsav=ijump
		endif
		goto 152	!get another option

	   case(14)
		call SCROLDIAL(1)
		goto 152		!back to main menu

	   case(15)		!default options
		iret1=1
		goto 500		!more options
	end select
c
c Now carry straight on for 'roll-on' or 'roll-back'
c
c After trigger, goes back to AIVPIX to do display according to XTRIG value
c then to 25 to wait for option.  If 'roll on' (forward or back) chosen, then
c arrives here. Move on INC points, so same trigger not found again (NB means
c that any alter trigger crossings in the same INC points as that in which
c original trigger found will be missed (could move on fewer points?)
c After fit, trigger has been reset to false, so use 'accepted' to make
c sure that display moves on.
	if(triggered.or.accepted) then
	   triggered=.false.
	   indfst=indsav + inc			!move on INC points after trigger
	   indlast=indfst+ndisp1-1
	   isdfst=indfst - (isec-1)*nbuf	!index WITHIN section of point #indfst
	   isdlast=isdfst+ndisp1-1
c================to ensure keep moving on wen triggered at every sweep
	   indsav=indfst				!update indfst
	   isdsav=isdfst
	   ndsav=ndisp1
c	   if(indfst.ge.jn1.and.indlast.le.jn2) goto 77	!whole display in isec
	   if(indfst.lt.jn1.or.indlast.gt.jn2) goto 50	!need new section!
	endif

77	continue
	call NUMSET		!num lock on for kbhit() options
c	if (debug()) then
c	   call set_cursor(ixdat,iydat-2)
c	   print 700,indfst,isdfst,ndisp1,inc,isec,
c     &	triggered,backward,stopped
c	endif
c	call AIVPIX4(ix,idata(isdfst),ndisp1,idata(isprev),
c     &	ipen,ifirst,ibk1,nscale,iyoff,iyoffprev)
	call AIVPIX5(ix,idata(isdfst),ndisp1,idata(isprev),
     &	ipen,ifirst,ibk1,nscale,iyoff,iyoffprev,ndprev)
	call MARK(infit,indfst,ndisp1,ixfprev,ix,14,ibk1,
     &  cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
	isprev=isdfst	!record start value ready for erasing next time
	ndprev=ndisp1	!ditto
	iyoffprev=iyoff
c Debug:
	if(discprt.and.debug1) then
	   write(8,701) indfst,indfst+ndisp1-1,isdfst,isdfst+ndisp1-1,isec
701	   format(
     & ' Display points ',i8,' to ',i8,' i.e. ',i8,' to ',i8,
     & ' in section ',i4)
	endif
771	continue	!jump here when lastdisp=true
c CALCULATE PARAMETERS TO MOVE PROGRESS LINE:
c	iprogx1=ipxs+ifixr(float(isdfst)/float(ibdpp))
c I think here i have to put a delay for backwards because first
c draw the progress line and then the points at the section limits!
c	call move_progress_line(iprogx0,iprogx1,iprogx2,iyframe,
c     &   icprog,ibk,flag_pline)
c	call move_prog_line(iprogx0,iprogx1,iprogx2,iyframe,
c     &   icprog,ibk,backward)
c	call MOVE_PROG_LINE(indfst,iprogx0,iprogx1,ibdpp,iyframe,
c     & icprog,ibk,backward)
c Draw prog line taking position from indfst
c	call MOVE_PROG_LINE(indfst,backward)	!other params in common/prog/
	call MOVEPROG(indfst,backward)	!other params in common/prog/
c	iprogx0=iprogx1	!now in subroutine
c	iprogx2=iprogx1
c
c Deal with trigger after display: go to 25 to wait for option to be
c given (with triggered=true still).  Next display will depend on whether
c 'roll on' (forward or backward) is chosen, or 'expand' is chosen
c
	if(triggered) then
	   call NUMCLR
	   goto 25	!wait for options in main menu (eg roll on/expand/...)
	endif
c
c Update section# display -romoved for now
cc	if (jfst.lt.novlap.and.backward) then
c	if (backward.and.isdfst.lt.1) then	!prev line is 'isdfst.lt.0'; wrong!?
c		call set_cursor(ixstr,iystr)
c		call det_string(pch(isec-1),icstr,ch)
c	else
c		call set_cursor(ixstr,iystr)
c		call det_string(pch(isec),icstr,ch)
c	endif
c====================================================================
c Search for threshold crossing: if found set 'triggered'=true
c Can search up to idata(js2)=last defined value in idata()
	if(.not.sdone) goto 40	!skip trigger search
	istart=isprev
	iend=isdfst+incabs-1	!look at first INC points in section normally
	if(iend.gt.js2) iend=js2
c If lastdisp or fstdisp then search whole display (actually not quite right
c because some of these may get searched again in next/prev section -next
c bit should fix this for lastdisp)
c===not fixed yet for fstdisp; this is done when moving backwards so
c need search only first incabs points here, but some points will not get
c searched when we move to prev section for next display)
	if(lastdisp) then
	   iend=isprev+ndprev-1		!search whole display
	   if(isec.lt.nsec) then
		iend=jn1next - (isec-1)*nbuf	!next display will start at jn1next
	   endif
	endif
c debug:
	if(discprt.and.debug1) then
	   kn1=istart + (isec-1)*nbuf
	   kn2=iend + (isec-1)*nbuf
	   write(8,702) kn1,kn2,isdfst,iend,isec
702	   format(
     & ' Search points ',i8,' to ',i8,' i.e. ',i8,' to ',i8,
     & ' in section ',i4)
	endif
	do k=istart,iend
	   if(idata(k).le.itriglev) then
		triggered=.true.
		ktrig=k	!record index where trigger occurred
		ksect=isec	!and section #
		if(discprt.and.debug1) write(8,703) ktrig,ksect
703		format(' Trigger found at point ',i7,' section ',i4)
c		indsav=indfst	!keep orig value
c		isdsav=isdfst	!keep orig value
c		ndsav=ndisp1	!keep orig value
c	   redefine display according to xtrig
		if(trigset) then
		   isdfst=k-ifix(xtrig*float(ndisp1))
		   isdlast=isdfst+ndisp1-1
		   indfst=isdfst + (isec-1)*nbuf
		   indlast=indfst+ndisp1-1
		   indsav=indfst	!keep orig value
		   isdsav=isdfst	!keep orig value
		   ndsav=ndisp1	!keep orig value
		endif
		trigset=.false.
		goto 50	!skip increment, then display according to xtrig
c		goto 25
	   endif
	end do
40	continue
	trigset=.true.		!no trigger found
c If end reached then, after display and search for trigger, return to menu (25)
c and wait for new option
	if(end) then
	   end=.false.
	   goto 25
	endif

c Now decide on increment for next section (via pots or keys), or respond
c to other key inputs, before incrementing index for next section
c Control via POTS
	if(usepots) then
	   call READAMP(ichan,nadc,iadc)
	   idt0=int4(iadc(0)-iadc0(0))
c	   inc=inc + idt0/5
	   inc=160+idt0/8			!gives problem with 'rewind' key??
	   backward=inc.lt.0
	   incabs=iabs(inc)		!reset abs value
c	   iadc0(0)=iadc(0)		!reset init value
	   call LOCATE(3,0)
	   print *,inc
	endif
c Menu is same as above, though not all options vailable here: must hit a
c number pad key!
	if(kbhit()) then
c	   achar=getch(ktype)
	   call GETIKEY(ikey)
	   if(ikey.le.0) goto 98	!carry on
	   select case(ikey)
		case(1)		!roll on
		   if(indlast.ge.ndat) then
      	      call BELL(1)
		      call POPTIME(250,130,'END OF DATA',1,15,12,14,1.5)
		   endif
		   inc=incabs		!set increment positive
		   backward=.false.
		case(2)		!roll back
		   if(indfst.le.1) then
	            call BELL(1)
		      call POPTIME(250,130,'START OF DATA',1,15,12,14,1.5)
		   endif
		   inc=-incabs
		   backward=.true.
		case(3)
		   call NUMCLR
		   goto 800		!expand mode
		case(4)			!set trigger level -not here
		case(5)		!go to red cursor -not here
		case(6)		!new parameters -not here
		case(7)
		case(8)		!stop roll
		   call NUMCLR
		   stopped=.true.
		   goto 25
		case(9)		!quit
	         ans='N'
 	        call DEFOLTa(ans,defolt)
	        call QDIALOG(1,'Do you really want to quit'
     &	   ,defolt,12,cans)
		   call GETINPa(cans,ans)
		   if(ans.eq.'Y') then
			call NUMCLR
			goto 99
		   endif
		case(10)		!plot	-not here
		case(11)		!Increase roll rate
		   if(incabs.lt.(ndisp-16))then		!ndisp1?
			incabs=incabs+16
			inc=incabs
		      if(backward) inc=-incabs
		   endif
		case(12)		!decrease roll rate
		   if(incabs.ge.16) then
			incabs=incabs-16
			if(incabs.le.0) incabs=4
			inc=incabs
			if(backward) inc=-incabs
		   endif
		case(13)
		case(14)
		case(15)
		   iret1=1
		   goto 500		!more options
	   end select
98	   continue
	   ndisp1=ndisp	!unless display truncated =========needed here?
	   if(debug()) then
	      print 700,indfst,isdfst,ndisp1,inc,isec,
     &	  triggered,backward,stopped
700		format(' indfst=',i9,' isdfst,ndisp,inc,isec = ',4i7,/,
     &	  ' trig= ',l4,' back = ',l4,' stop= ',l4)
	   endif
	endif		!end of kbhit section
c
c Now increment for next display
c (NB lastdisp, fstdisp should never be used if novlap big enough -can be
c removed from loop somehow?)
	if(lastdisp.and.isec.lt.nsec) then
	   lastdisp=.false.	!reset
c now delete AFTER new section read in
	   indfst=jn1next       !display from 1st point in next section
	   indlast=indfst+ndisp-1
	else if(fstdisp.and.isec.gt.1) then
	   fstdisp=.false.	!reset
c now delete AFTER new section read in
	   indlast=jn2prev
	   indfst=indlast-ndisp+1
	else
 	   isdfst=isdfst+inc
	   isdlast=isdfst+ndisp-1
	   indfst=indfst+inc
	   indlast=indfst+ndisp-1
	endif
	if(isec.eq.1.and.indfst.lt.1) then		!reached start
	   isdfst=1
	   isdlast=ndisp
	   indfst=1
	   indlast=ndisp
	   call NUMCLR
	   goto 25	!start of data: stop and choose new option
	endif
	if(isec.eq.nsec.and.indlast.gt.ndat) then	!reached end
	   isdlast=js2
	   isdfst=isdlast-ndisp+1
	   if(isdfst.lt.js1) then
		isdfst=js1
		ndisp1=isdlast-isdfst+1
	   endif
	   indfst=isdfst + (isec-1)*nbuf
	   indlast=indfst+ndisp1-1
	   end=.true.
c	   goto 25	!end of data: stop and choose new option -NOT YET DISPLAYED!
	endif
c	call set_cursor(ixdat,iydat-2)
c
c These values define next display -are all necessary points present
c in IDATA()? If not then read another section into IDATA
c Check that the specified starting point for the display, consam(indfst),
c is not so near to the end of section #isec (just calc) that there are
c too few points to show it (but if no overlap then must show whatever there is!)
c   Use overlap in current section if long enough. If not
c then use next/prev section (a) if there is one and (b) if it has overlap
c long enough to show the specified first point (indfst).  Otherwise show
c truncated display, starting with the correct point in current section but
c with fewer than ndisp points.  Only one such display is needed to show
c all the available data in current section (no point in rolling it across
c the screen) so use special call to AIVPIX here.  But if not rolled must
c search ALL the points for a trigger
50	continue
	if(indfst.ge.jn1.and.indlast.le.jn2) goto 77	!whole display in isec
									!so keep rolling
	ndisp1=ndisp		!unless truncated
	if(indlast.gt.jn2) then	!display ends in the next section
	   if(isec.lt.nsec.and.indfst.ge.jn1next) then	!use next section
		isec=isec+1
		call CALCIND(isec,nbuf,nrlast,novlap,nsec,
     & 	 in1,in2,jn1,jn2,is1,is2,js1,js2,nread,jn1next)	!recalc indices
	   else		!show remainder (<ndisp points) from current section
c next AIVPIX is called with ndisp1<ndisp so it would erase only ndisp1 points,
c therefore delete all points drawn last time (ndprev) now: use aivpix4
c with ifirst=-1 to erase only (could also use DELPIX4)
c		ifirst=-1		!erase only in AIVPIX4
c		call AIVPIX4(ix,idata,ndprev,idata(isprev),
c     &	 ipen,ifirst,ibk1,nscale,iyoff,iyoffprev)
c		ndisp1=jn2-indfst+1
c		ifirst=1	!so draw only (no erase) in next AIVPIX
c		call AIVPIX4(ix,idata(isdfst),ndisp1,idum,
c     &	 ipen,ifirst,ibk1,nscale,iyoff,idum)
c With AIVPIX5 should be able to replace next 2 calls above with one
		ndisp1=jn2-indfst+1
		ifirst=0		!erase & draw
		call AIVPIX5(ix,idata(isdfst),ndisp1,idata(isprev),
     &	 ipen,ifirst,ibk1,nscale,iyoff,iyoffprev,ndprev)
		call MARK(infit,indfst,ndisp1,ixfprev,ix,14,ibk1,
     & 	 cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
		isprev=isdfst	!record value in case needed in DELPIX
		ndprev=ndisp1	!ditto
		iyoffprev=iyoff
		if(discprt.and.debug1) then	!debug
		   kn2=indfst+ndisp1-1
		   ks2=isdfst+ndisp1-1
		   write(8,701) indfst,kn2,isdfst,ks2,isec
		endif
		lastdisp=.true.	!so right points are searched etc
c   now back to loop with lastdisp=true, but skip aivpix, which is already done
		goto 771	!no new section to be read
	   endif
	else if(indfst.lt.jn1) then	!display starts in the prev section
						!i.e. rolling back
	   jn2prev=(isec-1)*nbuf+novlap		!last point in isec-1
	   if(isec.gt.1.and.indlast.le.jn2prev) then	!use prev section
		isec=isec-1
		call CALCIND(isec,nbuf,nrlast,novlap,nsec,
     & 	 in1,in2,jn1,jn2,is1,is2,js1,js2,nread,jn1next)	!recalc indices
	   else 			!show fewer than ndisp points from current section
c problem when rolling back AND novlap not big enough to allow full length
c (ndisp points) display from either current or previous section -partial display
c as above should use the specified indlast at right edge of screen and work
c leftwards to point jn1=last available point in current section. Can achieve
c this by calling AIVPIX with ix(ixoff) where ixoff normally 1, but reset here.
c Delete all points drawn last time (ndprev) now: use aivpix4
c with ifirst=-1 to erase only (could also use DELPIX4)
c		ifirst=-1		!erase only in AIVPIX4
c		call AIVPIX4(ix,idum,ndprev,idata(isprev),
c     &	 ipen,ifirst,ibk1,nscale,idum,iyoffprev)
c		ndisp1=indlast-jn1+1
c		ixoff=ndisp-ndisp1+1
c		ifirst=1	!so no delete in next AIVPIX
c		call AIVPIX4(ix(ixoff),idata(is1),ndisp1,idata(isprev),
c     &	 ipen,ifirst,ibk1,nscale,iyoff,iyoffprev)
c With AIVPIX5 should be able to replace next 2 calls above with one
		ndisp1=indlast-jn1+1
		ixoff=ndisp-ndisp1+1
		ifirst=0		!erase & draw
		call AIVPIX5(ix,idata(is1),ndisp1,idata(isprev),
     &	 ipen,ifirst,ibk1,nscale,iyoff,iyoffprev,ndprev)
		call MARK(infit,indfst,ndisp1,ixfprev,ix(ixoff),14,ibk1,
     & 	 cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev)
		isprev=is1		!record value in case needed in DELPIX
		ndprev=ndisp1	!ditto
		iyoffprev=iyoff
		if(discprt.and.debug1) then	!debug
		   ks2=is1+ndisp1-1
		   kn1=is1 + (isec-1)*nbuf
		   kn2=kn1+ndisp1-1
		   write(8,701) kn1,kn2,is1,ks2,isec
		endif
		fstdisp=.true.		!so right points searched etc
c   now back to loop with lastdisp=true, but skip aivpix, which is already done
		goto 771	!no new section to be read
	   endif
	endif
c===later:	put up red box to say display truncated?!
c Now in new section so erase last display separately here (and set ifirst=1
c so next AIVPIX draws new display only) do this before idata() changed!
c -but this leaves the screen blank while new data is being read in -can
c avoid this by copying current data to idata1, and using this to delete
c AFTER call to READSEC
c Next line is fix to prevent occasional crashes when fitting on section
c border -should be able to fix it properly!!
	if(ndprev+isprev-1.gt.nbuf) isprev=isprev-nbuf
	do i=1,ndprev
	   idata0(i)=idata(i+isprev-1)
	enddo
	ifirst=1
c Read the new section
	call WDIALOG(1,
     &  'Reading data from disk . . .',-icw1)
	call READSEC(isec,idata,nrlast,nsec,ioff,nbuf,nread,novlap,
     & sfac2,in1,in2,jn1,jn2,is1,is2,js1,js2,cjump,invert)
	filtdone=.false.		!this section not yet filtered
	zoomed=.false.		!reset tf1 etc for unfiltered data
	call SETFILT(zoomed,filtzoom,ffilt,fczoom,tf1,tf50,tf99,
     & npfilt,nshutfit,scrit,iampcrit,finter,facjump,
     & smult,ampfac,avamp,avampsav,avampz,filt1,nfilt1,
     & dtf1,filt1z,nfilt1z,dtf1z,filt1n,nfilt1n,dtf1n)
      call WDIALOG(1,
     &  'Reading data from disk . . .',-icbd)	!erase
c	call set_cursor(ixstr,iystr)
c	call det_string(pch(isec),icstr,ch)

c Now delete previous display AFTER new data read in (note: nbuf, novlap in
c call to DELPIX replaced with 2048, 0 i.e. dimensions of idata1())
c (ixoff normally =1, but needed for deletion after FSTDISP when
c rollong backwards): use aivpix4
c with ifirst=-1 to erase only (could also use DELPIX4)
	ifirst=-1		!erase only in AIVPIX
c	call AIVPIX4(ix(ixoff),idum,ndprev,idata0,
c     &	 ipen,ifirst,ibk1,nscale,idum,iyoffprev)
	call AIVPIX5(ix(ixoff),idum,ndprev,idata0,
     &	 ipen,ifirst,ibk1,nscale,idum,iyoffprev,ndprev)
c Recalc indices for display appropriate for the section just read.
	ixoff=1		!reset
	isdfst=indfst - (isec-1)*nbuf	!index WITHIN section of point #indfst
	isdlast=isdfst+ndisp-1
	goto 77

c**********************************************************************
c EXPAND MODE HERE
800	continue
c if arrive at expmode with trigger set, then reset here and use display
c dictated by XTRIG as starting point for expand/contract as specified
c in indfst, ndisp1 (pre trigger display already stored in indsav, ndsav.
c For expand/contract store the display to be restored (on return from
c expmode) via isprev, ndprev here)
c NO -leave triggered=true, so when EXPMODE is left to reroll, the display
c jumps past trigger, and 'triggered' is reset, after GETOPT
c	if(triggered) then	!expand mode after trigger
c	   triggered=.false.
c	else
	if(.not.triggered) then	!(if triggered, indsav etc already set)
	   isdsav=isprev		!to restore display after expmode
	   indsav=isprev + (isec-1)*nbuf
	   ndsav=ndprev
	endif
	ip1sav=iprogx1	!save progress line position
	ip2sav=iprogx2	!save progress line position
	call EXPMODE(IDATA,isdfst,indfst,ndisp1,
     & nsec,iyd1,iyd2,ibk,ibk1,ibk2,expfac,js1,js2,isec,nbuf,novlap,
     & sdone,usepots,mono,baseOK,fitmode,ifitmode,basevga,
     & nscale,iyoff,sfac3,icol1,icol2,icol3,icol4,icbl,isavbase,
     & ipen,idata0,ndfit,ix,dx,infirst,infit,ixfprev,shut,kt,
     & idiskq,ndev,srate,calfac,calfac2,ioff,cdate,adctime,
     & calfac1,opendown,cfac1,nscale1,iyoffz,izoomfac,base,
     & izoom,fczoom,filtzoom,filtdone,zoomed,nread,
     & cjump,intzero,ix0prev,njump,njfit,tjlen,ndat,
     & avamp,avampz,avampsav,iampcrit,ampfac,scrit,smult,facjump,
     & tf1,tf50,tf99,ffilt,seqdisp,inseq1,inseq2)
c ndfit,ix,dx are returned with fitmode=true when section on screen is
c to be fitted; idata0(1) to idata0(ndfit) contain data to be fitted in FITSUB
c
c=	if(fitmode) goto 200	!fit (using ndfit,dx,ix,idata1)
	if(fitmode) then
	   expmod=.true.
	   goto 200
	endif
c
	if(ndfit.ne.ndisp1) then	!restore ix(), dx after expansion
	   dx=dxsav
	   do i=1,ndisp1
		ix(i)=ixsav(i)
	   end do
	endif
c
c REROLL after expmode; if returning after accepting a fit, then display
c with the 'up to here' mark at fraction xtrig along x axis
c===POSSIBLE PROBLEM - may not all be in present section!
210	continue
	call FILLWIN(0,iyd1,639,iyd2,ibk1)		!clear data area
	ixfprev=-1 		!x value where previous trigger point was drawn
	ix0prev=-1 		!x value where previous t=0 point was drawn (cjump)
	ndisp1=ndsav
	if(ndsav.eq.0) ndisp1=ndisp	!fix!! -is ndsav always defined??
	call CALLINE(ibasevga,calfac1,opendown,ndisp1,finter,-1)	!calibration
	if(accepted) then
	   indfst=infit - ifix(xtrig*float(ndisp1))
	   isdfst=indfst - (isec-1)*nbuf	!index WITHIN section of point #indfst
	   indsav=indfst				!reset value
	   isdsav=isdfst				!reset value
	   accepted=.false.		!reset
	else
	   indfst=indsav		!restore value before expmode
	   isdfst=isdsav		!restore value before expmode
	endif
	indlast=indfst+ndisp1-1
	isdlast=isdfst+ndisp1-1
c	call NEWPROGLINE()	!restore prog line
c	call DRAWPROG(ixbox,ixframe,iyframe,icframe,iprogx1,iprogx2,
c	& icprog,ixcr,iycr,iccr,ixmark,iymark,icmark,ibdx,ibk,iyd2)
	call MOVEPROG(indfst,backward)	!other params in common/prog/
	ifirst=1	!prev display erased by fillwin in EXPMODE
	if(baseOK.and.(.not.sdone)) then
	   call NUMCLR
	   goto 31	!restore prog line after 31
	else
c restore progress line to what it was before EXPMODE
	   iprogx1=ip1sav	!restore progress line position
	   iprogx2=ip2sav	!restore progress line position
	   goto 5	!REROLL (goto 5 to do 1st display there, not above)
	endif
c
c Now transition fitting
200	continue
	if(fitmode) then
	   if(numamp) then	!initial amp specified numerically
		numamp=.false.
		goto 38
	   endif
	   ibasevga=ifixr(basevga)
c get floating point data for fitting
	   do i=1,ndfit
	     data(i)=float(idata0(i))		!in intermed units (in COMMON)
	   enddo
c and equivalent integer data in pixel units for display (keep any
c zoom that was set in expmode)
	   call SCALINT1(idata0,ndfit,nscale1,iyoffz,idata1)
	   if(expmod) then
		expmod=.false.
		goto 201		!data already drawn in expmode
	   endif
36	   continue		!jump here to redraw data for refit
	   call SETACT(iact,-1)		!no draw
	   iact(ibk2)=15			!unless background col, when draw white
c=36	   continue		!jump here to redraw data for refit
	   call FILLWIN(0,iyd1,639,iyd2,ibk2)	!clear data area
	   call CALLINE(ibasevga,cfac1,opendown,ndfit,finter,-1)	!calibration
	   call IDCPIX(ix,idata1,ndfit,iact)
	   call RDGWIN1(0,ibasevga,639,ibasevga,isavbase)  !keep data overwritten
	   call HLINE(0,639,ibasevga,icbl)	   !show baseline
	   call MARK(infit,indfst,ndfit,ixfprev,ix,14,ibk1,
     & 	 cjump,intzero,ix0prev,njump,njfit,tjlen,ijprev) !'up-to-here'
	   if(nampmark.gt.0) then
		do i=1,nampmark
		   iy=ibasevga+iamark(i)	!sign of iamark set in inscan
		   call DHLINE(0,639,iy,10,22)
		enddo
	   endif
201	   continue
c Do selected fit type
	   if(ifitmode.eq.0.or.ifitmode.eq.1) then
	     call FITSUB(idata1,ix,dx,ndfit,infirst,base,basevga,
     & 	ampdone,ntrig,navtest,scrit,tf50,trise,opendown,goback,
     & 	nscale,iyoff,ioff1,sfac3,dfinter,timamax,mono,open,shut,
     & 	nshutfit,tlast,prevlevel,shutprev,shutsav,infit,ixfprev,
     & 	iautosub,vard,disptran,dispderiv,dispguess,refit,newrange,
     & 	newguess,t0sav,y0sav,dtsav,dt,delt,iampcrit,tmin,ndat,
     &      tsfac,tlfac,tcfac,accepted,ntmax,nfmax,
     & 	calfac1,calfac2,tint,iampl,iprops,ifitmode,ifitold,isavbase,
     &	iyd1,iyd2,yd1,yd2,icol1,icol2,icw,icw1,icbl,ibk,ibk2,
     &	avtot,navamp,avamp,kt,timsav,nfits,ifits,ibaselin,rms1,
     &      tstring,microsec,bdisp,itriglev,itrigvga,
     &      imin,imax,ifreq1,ifreq2,ifreq3,idata0,
     & 	idiskq,ndev,indfst,srate,calfac,ioff,cdate,adctime,
     &      cfac1,nscale1,iyoffz,izoomfac,errfac,confac,derivfac,
     & 	cjump,tzerod,intzero,ix0prev,ktjump,nfjump,tsample,
     &	ijump,njump,njfit,jumps,njdim,avampsav)
c//     & 	filtered,ffilt,fczoom,tf1,tf99,
c//     & 	facjump,smult,ampfac,avampsav,avampz)
		call CATCHBUG(ifreq1,ifreq2,ifreq3,imin,imax,ifmin,ifmax)
		if(refit) goto 36	!redraw, then refit according to ifitmode value
c Refitting options
	   else
	     call FITSUB6(idata1,ix,dx,ndfit,infirst,base,basevga,
     &      ampdone,ntrig,navtest,scrit,tf50,trise,opendown,goback,
     &      nscale,iyoff,ioff1,sfac3,dfinter,timamax,mono,open,shut,
     &      nshutfit,tlast,prevlevel,shutprev,shutsav,infit,ixfprev,
     &      iautosub,vard,disptran,dispderiv,dispguess,refit,
     & 	newguess,t0sav,y0sav,dtsav,dt,delt,iampcrit,rms1,
     &      calfac1,calfac2,tint,iampl,iprops,ifitmode,ifitold,isavbase,
     & 	iyd1,iyd2,yd1,yd2,icol1,icol2,icw,icw1,icbl,ibk,ibk2,
     &	avtot,navamp,avamp,kt,timsav,nfits,ifits,ibaselin,ndat,
     &      ntmax,nfmax,tsfac,tlfac,tcfac,accepted,
     &      tstring,microsec,bdisp,itriglev,itrigvga,
     & 	imin,imax,ifreq1,ifreq2,ifreq3,idata0,
     & 	idiskq,ndev,indfst,srate,calfac,ioff,cdate,adctime,
     & 	cfac1,nscale1,iyoffz,izoomfac,errfac,confac,derivfac,
     & 	cjump,tzerod,intzero,ix0prev,ktjump,nfjump,tsample,
     &	ijump,njump,njfit,jumps,njdim,avampsav)
		call CATCHBUG(ifreq1,ifreq2,ifreq3,imin,imax,ifmin,ifmax)
		if(refit) goto 36	!redraw, then refit according to ifitmode value
	   endif
c
c Arrive here if no refit (accept or reject fit)
	   fitmode=.false.
	   triggered=.false.
c Auto-write
	   if((kt-nwsav).ge.nwrit) then	!do auto-write
     		call INTCONV(kt,cnum)
		n=NBLANK(cnum)
	      call WDIALOG(1,cnum(1:n)//
     &	   ' transitions: automatic data back up . . .',-icw)
		call AUTWRIT(kt,tint,iampl,iprops,ntmax,nfits,timsav,ifits,
     &	 ibaselin,nfmax,nbuf,novlap,srate,finter,errfac,derivfac,
     &	 tsect,ioff,ndat,nsec,nrlast,avtot,navamp,avamp,rms,
     &	 nwrit,nwsav,newpar,opendown,invert,confac,
     &	 usepots,disp,smult,scrit,vary,ntrig,navtest,dgain,iboff,
     &	 expfac,bdisp,ibflag,iautosub,xtrig,ndev,cdate,adctime,
     &	 nsetup,filtfile,npfilt,ffilt,
     &	sfac1,sfac2,sfac3,nscale,calfac,calfac1,calfac2,iyoff,ioff1,
     &	 disptran,dispderiv,dispguess,ampfac,tmin,
     &	 tsfac,tlfac,sdone,dfinter,tlast,goback,shutsav,
     &	 shut,shutprev,backward,prevlevel,t0sav,y0sav,vard,nshutfit,
     &	 infit,infirst,ixfprev,idiskq,ifirst,base,basevga,ibasevga,
     &	 itrig,itrigvga,itriglev,inc,incabs,
     &	 indfst,isdfst,isec,ndisp,ndisp1,
     &  	 iscanver,tcfac,facjump,
     & 	 ifreq1,ifreq2,ifreq3,imin,imax,ifmin,ifmax,
     & 	 cjump,ijump,njdim,jumps,kjumps,tzerod,intzero,tsample,
     &	 ktjump,njump,njfit,nnull,ktlast,
     & 	 izoom,fcz,fczoom,ampz,avampsav)
		nwsav=kt    !update for next
c         Erase message (by writing again in backround col, icbd, to avoid
c		problems with string length in wdialog)
c	      call WDIALOG(1,cnum(1:n)//
c     &	   ' transitions: automatic data back up . . .',-icbd)
	   endif
c Warn
	   if(kt.gt.ntmax-50) then
		nleft=ntmax-kt
		call BELL(4)
     		call INTCONV(nleft,cnum)
		n=NBLANK(cnum)
	      call WDIALOG(1,'WARNING: room for '//cnum(1:n)//
     &	   ' more transitions in this experiment file',icw1)
	   endif
	   if(nfits.gt.nfmax-50) then
		nleft=nfmax-nfits
		call BELL(4)
     		call INTCONV(nleft,cnum)
		n=NBLANK(cnum)
	      call WDIALOG(1,'WARNING: room for '//cnum(1:n)//
     &	   ' more fits in temporary data file',icw1)
	   endif
c debug
	   if(discprt.and.debug1) then	!debug
		write(8,7041)
7041		format('  i  kt   timsav        ifits   ibaselin')
		do i=1,nfits
		   write(8,704) i,kt,timsav(i),ifits(i),ibaselin(i)
704		   format(i5,1x,i5,3x,g16.8,2x,i8,2x,i8)
		enddo
	   endif
c  Recalculate duration of opening corresponding to trigger level
c (recalc itriglev in case iyoff changed by 'adjust baseline' in FITSUB/6)
	   if(kt.gt.0) then
		itriglev=(itrigvga-iyoff)/sfac3	!in intermed units (as idata)
      	aam=abs((float(itriglev)-base)/avamp)
		ttold=ttrig
		call AMAX(aam,trise,ttrig,nerr)
		if(nerr.eq.0) then
         	   call INTCONV(ifixr(ttrig),cnum)
		   n=NBLANK(cnum)
		   tstring='trig='//cnum(1:n)//microsec
c		   if(abs(ttrig-ttold).gt.5.) then !print only if changes by > 5 mus
c      	      if(pon()) write(7,24) ttrig
c      	      if(discprt) write(8,24) ttrig
c		   endif
		else
		   tstring='trig= ?     '
		endif
	      call WRITBOX(2,tstring,1,10)	!display trigger level
	   endif
c
c Jump to 38 with ampdone=false if initial full amp was specified numerically
38	   continue
	   if(ampdone) then
	      scrit=smult*abs(avamp)		!in intermed units
		goto 210		!reroll from present position
	   else
		do i=1,ntrans
		   iampl(i)=0 	!reset after prelim fit of 'full amp'
		   iprops(i)=0
		enddo
		amp=avamp*calfac2
		if(rms.gt.1.e-20) then
		   signois=abs(amp/rms)
		endif
	      call DCFORMAT(amp,7,2,cnum)
	      call DCFORMAT(signois,7,1,cnum1)
	      call WDIALOG(1,'Amplitude '//CHARNB(cnum)//
     & 	' pA: signal/noise = '//CHARNB(cnum1),icw)
	      scrit=smult*abs(avamp)		!in intermed units
		scr1=scrit/sqrt(vary)	!both in intermed units
	      call DCFORMAT(scrit*calfac2,7,2,cnum)
	      call DCFORMAT(scr1,7,2,cnum1)
	      call WDIALOG(1,'Threshold (Scrit) '//CHARNB(cnum)//
     & 	' pA: Scrit/rms noise = '//CHARNB(cnum1),icw)
		call BELL(1)
		call WDIALOG(1,' O.K. [Y] ?',icw1)
34		ans=UC(GETCH(ktype))
		ival=ICHAR(ans)
		if(ival.ne.78.and.ival.ne.89.and.ival.ne.13) goto 34     !N,Y, enter
c		read 101,ans
c		call CLRDIALOG(1,0)		!clear dialog box #1 to black
		call FILLWIN(0,iyd1,639,iyd2,ibk1)	!clear data area
	      call CALLINE(ibasevga,calfac1,opendown,ndisp,finter,-1)	!calibration
		if(UC(ans).eq.'N') then
		   ampdone=.false.	!carry on to do again
	         call WDIALOG(1,
     &    'TRY ANOTHER GOOD FULL OPENING FOR AMPLITUDE (F2=help)',icw1)
	         call BELL(2)
		   goto 78
		else
	         if(pon()) write(7,35) amp,signois,scrit*calfac2,scr1
	         if(discprt) write(8,35) amp,signois,scrit*calfac2,scr1
35   		   format(' Initial amplitude estimate = ',f7.2,' pA',/,
     &		' Signal/noise ratio = ',f9.2,/,
     &		' Threshold for transition search = ',f7.2,' pA',/,
     &		' Threshold/rms noise = ',f7.2)
		   ampdone=.true.
	   	   scrit=smult*abs(avamp)		!in intermed units
		   scr1=scrit/sqrt(vary)	!in intermed units
		   iampcrit=iabs(ifixr(ampfac*avamp))
		   shut=.true.		!in case not so after prelim amp fit!
		   SHUTSAV=.true.	!before first opening located
		   open=.not.shut
c		   call CLRDIALOG(1,0)		!clear box 1
		   if(cjump) then
			if(njump.ne.njfirst) then
			   call BELL(2)
         		   call INTCONV(njfirst,cnum)
			   n=NBLANK(cnum)
	      	   call WDIALOG(1,
     & 'Use GO TO... option ''Start of c-jump #'' to return to jump #'//
     &  cnum(1:n),icw1)
	               call WDIALOG(1,' THEN START FITTING FROM t=0',icw1)
			else
	               call WDIALOG(1,'NOW START FITTING FROM t=0',icw1)
			endif
		   else
	            call WDIALOG(1,'NOW START FITTING',icw1)
		   endif
c  Calculate duration of opening at trigger level
		   aam=abs((float(itriglev)-base)/avamp)
		   ttold=ttrig
		   call AMAX(aam,trise,ttrig,nerr)
		   if(nerr.eq.0) then
         		call INTCONV(ifixr(ttrig),cnum)
			n=NBLANK(cnum)
			tstring='trig='//cnum(1:n)//microsec
c		      if(abs(ttrig-ttold).gt.5.) then !print only if changes by > 5 mus
c      	         if(pon()) write(7,24) ttrig
c      	         if(discprt) write(8,24) ttrig
c			endif
		   else
			tstring='trig= ?     '
		   endif
		   call OPENBOX(2,7,.true.)
		   call WRITBOX(2,tstring,1,10)	!display trigger level
	      endif
c Don't start again from beginning!
c		indfst=indfst0			!start again from beginning
c		indlast=indfst+ndisp-1
c		isdfst=indfst0 - (isec0-1)*nbuf	!index WITHIN section of point #indfst
c		isdlast=isdfst+ndisp-1
		call MOVEPROG(indfst,backward)	!other params in common/prog/
c		if(isec.ne.isec0) then
c		   isec=isec0
c		   goto 444	!read section isec0
c		else
		   goto 5	!re-roll from start
c		endif
	   endif	!end of initial amplitude setting
	endif		!end of fitting
	goto 99
c
c Now Misc help/default menus
c Now DCMENU for some defaults (Opt 15)
500	continue
	call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
501	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	if(kt.eq.0) then
	   Lb(3)=0
	   Lb(4)=0
	   Lb(5)=0
	endif
	Lb(14)=0	!not yet used
	call NUMSET			!set num lock on ready for response
502	continue
	text(1)='0:ADJUST BASELINE'
	text(1)='1:SET NEW PARAMS '
	text(2)='2:NEW RMS NOISE  '
	text(3)='3:NEW SENSITIVITY'
	text(4)='4:SET AMP DUBIOUS'
	text(5)='5:SET GAP UNUSABLE'
	text(6)='6:LIST RESULTS   '
 	text(7)='7:LIST LATEST    '
	text(8)='8:PRINT NOTE     '
	text(9)='9:EMERGENCY EXIT '
	text(10)='10: QUEUE PLOT   '
	text(11)='+:AMPLITUDE HISTO'
	text(12)='-:RESET FULL AMP '
	text(13)='*:WRITE SCANTEMP '
	text(14)='/:               '
	text(15)='.: MAIN MENU     '
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
c	call DCMENU(-4,4,Lb,text,0,0)	!delete box 4 only
c	Lb(4)=-1		!italic
503	call CKEY(ch,ikey)
	if(ikey.lt.-1.or.ikey.gt.15) goto 503
	if(kt.eq.0.and.(ikey.ge.3.and.ikey.le.7)) then
	   call BELL(1)
	   call WDIALOG(1,
     &  'NO TRANSITIONS FITTED YET! Choose another option',-12)
	   goto 503
	endif
	if(ikey.eq.-1) then	!F1=help
		helpst(1)=
     &	  '1:NEW PARAMETERS: Allows most values to be reset.    '
		helpst(2)=
     &	  '2.NEW RMS: Re-estimate baseline position and noise.  '
		helpst(3)=
     &	  '3.NEW SENSITIVITY: changes sensitivity of search for '
c		helpst(4)=
c     &	  '  transitions -sets value of the multiple of rms noise'
		helpst(4)=
     &	  '  transitions -sets the percentage of full amplitude  '
		helpst(5)=
     &	  '  used for detection (Scrit) (also set with key 1)   '
		helpst(6)=
     &	  '4.SET AMP DUBIOUS: mark ill-defined (rather than just'
		helpst(7)=
     &	  '  brief) amplitude as being ''dubious''.             '
		helpst(8)=
     &	  '5.SET GAP UNUSABLE: Specify that a shut period cannot'
		helpst(9)=
     &	  '  be used because it contains rubbish (or doubles).  '
		helpst(10)=
     &	  '6.LIST RESULTS: List all durations and amplitudes,   '
		helpst(11)=
     &	  '  with option to print to disk/printer afterwards.   '
		helpst(12)=
     &	  '7.LIST LATEST: list last 22 durations and amplitudes.'
		helpst(13)=
     &	  '8.PRINT NOTE: insert text note into printout.        '
		helpst(14)=
     &	  '9.EMERGENCY EXIT: Leave program without writing the  '
		helpst(15)=
     &	  'temporary restart file (so old file not overwritten).'
		helpst(16)=
     &	  '*.WRITE SCANTEMP: Update SCANTEMP.DAT (as for        '
		helpst(17)=
     &	  ' automatic backup) so restart will be from here.     '
		helpst(18)=
     &	  '-.RESET FULL AMP: Replace a corrupted ''full amplitude'''
	   ntext=18
	   call POPTEXT(mxlo,mylo,myhi,helpst,ntext,ictx,ibkx,icfx)
	   call WDIALOG(1,
     &       'Next choose another menu option',-icw)
	   goto 503
	endif
c
26	continue
	SELECT CASE(ikey)
	   CASE(1)		!new params
		newpar=.true.
		call MODE(3)	!alpha mode
		goto 100

	   CASE(2)        !new rms noise
		sdone=.false.	!not yet calculated baseline and RMS
		baseOK=.false.	!not yet accepted bit of baseline for RMS
		newrms=.true.
		incabs=16		!slow roll until RMS and baseline done
		indfst1=indfst	!save current position in data
		isdfst1=isdfst	!save current position in data
	   	call WDIALOG(1,
     &	 'GET BIT OF EMPTY BASELINE ON SCREEN FOR SD',-icw1)
	      call BELL(2)
	      call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
		goto 150

	   CASE(3)
		title='SET S(crit)'
		strings(1)='very high 6 %'
		strings(2)='high      8 %'
		strings(3)='higher    10%'
		strings(4)='medium    12%'
		strings(5)='lower     14%'
		strings(6)='lower     16%'
		strings(7)='low       18%'
		strings(8)='very low  20%'
		nval=8
c initially highlight value closest to current smult, i.e. line #iline=
		iline=ifixr(100.*smult/2.)-2		!for above values!
c
		helps(1)='Choose the percentage of  '
		helps(2)=' full amp, beyond which   '
		helps(3)=' a transition is deemed to'
		helps(4)=' to have occurred (smaller'
		helps(5)=' value=more sensitive).   '
		nhelp=5
		call POPMENU(nxlo,nylo,nyhi,strings,nval,ictm,icfm,icupm,
     &	 ibkm,title,helps,nhelp,iline,ans,ival)
		if(iline.ge.1.and.iline.le.nval) then	!iline=0 for ESC=cancel
		   smult=2.0*float(iline+2)/100.	!for above numbers
c==		   scrit=smult*sqrt(vary)	!in intermed units
		   if(ampdone.and.sdone) then
	   	      scrit=smult*abs(avamp)		!in intermed units
			scr1=scrit/sqrt(vary)	!both in intermed units
			if(scr1.lt.3.) then
			   call BELL(2)
			   call DCFORMAT(scr1,6,1,cnum)
			   n=nblank(cnum)
		         ans='Y'
	 	         call DEFOLTa(ans,defolt)
		   	   call QDIALOG(1,
     &' WARNING: threshold is only '//cnum(1:n)//'x rms noise: O.K.',
     &			defolt,12,cans)
			   call GETINPa(cans,ans)
			   if(ans.eq.'N') goto 26
			endif
		   endif
		endif

      	call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
		goto 150
c		goto 503	!get another option

	   CASE(4)		!set dubious
52		k1=kt-8	!display tint(kt-8) to tint(kt)
		call SELDATA(k1,kt,tint,iampl,calfac2,kval,ntmax,1)
		if(kval.ne.-1) then		!not 'cancel'
		   if(iampl(kval).eq.0) then	!skip if 'cancel'
		     call BELL(2)
		     call WDIALOG(1,
     &	    'Only openings can be set dubious -try again',-icw1)
		     goto 52
		   endif
		   iprops(kval)=IBSET(iprops(kval),0)   !set bit 0='1'
		endif
		call BELL(1)
		call WDIALOG(1,
     &	 'Continue -choose another menu option',-icw1)
	      call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
		goto 150		!back to main menu after 'list'
c		goto 503	!get another option

	   CASE(5)		!set unusable
51		k1=kt-8	!display tint(kt-8) to tint(kt)
		call SELDATA(k1,kt,tint,iampl,calfac2,kval,ntmax,0)
		if(kval.ge.1) then		!not 'cancel'
		   if(iampl(kval).ne.0) then	!skip if 'cancel'
		     call BELL(2)
		     call WDIALOG(1,
     &	    'Only shuttings can be set unusable -try again',-icw1)
		     goto 51
		   endif
		   iprops(kval)=IBSET(iprops(kval),3)   !set bit 3='8'
		endif
		call BELL(1)
		call WDIALOG(1,
     &	 'Continue -choose another menu option',-icw1)
	      call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
		goto 150		!back to main menu after 'list'
c		goto 503	!get another option

	   CASE(6)        !list all results
c convert all results so far to ascii and list/print
		call LISTDATA(kt,tint,iampl,iprops,calfac2,ntmax,
     & 	 timsav,ifits,nfits,nfmax,dfinter,cjump,
     &	 jumps,kjumps,njdim,ijump,0)
	      call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
		goto 150		!back to main menu after 'list'

	   CASE(7)        !list latest results only
c convert all results so far to ascii and list/print
		call LISTDATA(kt,tint,iampl,iprops,calfac2,ntmax,
     & 	  timsav,ifits,nfits,nfmax,dfinter,cjump,
     &	  jumps,kjumps,njdim,ijump,1)
	      call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
		goto 150		!back to main menu after 'list'

	   CASE(8)		!done
            call NUMCLR
		call TDIALOG(1,'Enter the note',text1,67,.true.,icw)
	      if(pon()) write(7,37) text1
	      if(discprt) write(8,37) text1
37		format(1x,a78)
	      call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
	      goto 150

	   CASE(9)		!emergency exit
		call BELL(2)
	      ans='N'
 	      call DEFOLTa(ans,defolt)
	      call QDIALOG(1,'Do you really want emergency exit'
     &	  ,defolt,12,cans)
		call GETINPa(cans,ans)
		if(ans.eq.'Y') then
		   deallocate(idata)
		   if(VIDEOTYP().ne.3) call VIDEOMOD(3)
		   call NUMCLR
		   goto 996
		else
         	   call NUMCLR
	         call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
	         goto 150		!main menu
		endif

	   CASE(10) 	!plot
c           calfac=pA/ADC (ymin,ymax not needed in AUTPLOT?)
c call with ntrans=0 here -no fitted curve
		call SCQPLT(idiskq,ndev,indfst,isdfst,ndisp1,idata,nbuf,
     &      novlap,srate,calfac,calfac2,ioff,cdate,adctime,mono,iesc,
     &      0,y0,t0,nfilt,dtf,dt,stepamp,filt,tif1,tif2,base)
		call BELL(1)
		if(iesc.ne.0) then	!plot cancelled -get another option
		   call WDIALOG(1,
     &		'Plot cancelled -choose another option',icw1)
		else
		   call WDIALOG(1,
     &		'Plot queued -choose another option',-ictd)
		endif
	      call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
		goto 150		!back to main menu after 'plot'

	   CASE(11)		!point amp histos
		call SCAMHIST(imin,imax,ifreq1,ifreq2,ifreq3,calfac2,
     &	  icw,icw1,opendown,ifmin,ifmax)
		call CATCHBUG(ifreq1,ifreq2,ifreq3,imin,imax,ifmin,ifmax)
	      call DEFDIALOG(1,23,2,3,68,icbd)	!redefine after VHIST
c	      call OPENDIALOG(1,7,.true.)	!shadow border (blue=1) (7=white/grey)
 		call CLEARS(0,480,ibk)	!fill screen with light blue (for outside data area)
		call FILLWIN(0,iyd1,639,iyd2,ibk1)	!fill data area with colour ibk
		call HLINE(0,639,iyd1-1,14)		!line to mark bottom of data area
		call HLINE(0,639,iyd2+1,14)	!line to mark top of data area
	      call CALLINE(ibasevga,calfac1,opendown,ndisp,finter,-1)	!calibration
c	      call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
c		goto 150		!back to main menu after 'plot'
		newpar=.true.	!so redraws dialog box at 333
		goto 333


	   CASE(12)		!reset full amp
  342		amp=avamp*calfac2
	      call DCFORMAT(amp,7,2,cnum)
c	      call DCFORMAT(signois,7,1,cnum1)
 	      call DEFOLTr(amp,defolt)
		n=NBLANK(cnum)
	      call QDIALOG(1,'Mean full amplitude now '//cnum(1:n)//
     &	': new value',defolt,12,cans)
		call GETINPr(cans,amp)
		avamp=amp/calfac2		!in intermed units
		avampsav=avamp
		if(rms.gt.1.e-20) then
		   signois=abs(amp/rms)
		endif
	      call DCFORMAT(amp,7,2,cnum)
	      call DCFORMAT(signois,7,1,cnum1)
	      call WDIALOG(1,'Amplitude '//CHARNB(cnum)//
     & 	' pA: signal/noise = '//CHARNB(cnum1),icw)
	      scrit=smult*abs(avamp)		!in intermed units
		scr1=scrit/sqrt(vary)	!both in intermed units
	      call DCFORMAT(scrit*calfac2,7,2,cnum)
	      call DCFORMAT(scr1,7,2,cnum1)
	      call WDIALOG(1,'Threshold (Scrit) '//CHARNB(cnum)//
     & 	' pA: Scrit/rms noise = '//CHARNB(cnum1),icw)
		call BELL(1)
		call WDIALOG(1,' O.K. [Y] ?',icw1)
341		ans=UC(GETCH(ktype))
		ival=ICHAR(ans)
		if(ival.ne.78.and.ival.ne.89.and.ival.ne.13) goto 341     !N,Y, enter
c		call FILLWIN(0,iyd1,639,iyd2,ibk1)	!clear data area
c	      call CALLINE(ibasevga,calfac1,opendown,ndisp,finter,-1)	!calibration
		if(UC(ans).eq.'N') then
		   goto 342
		else
	         if(pon()) write(7,35) amp,signois,scrit*calfac2,scr1
	         if(discprt) write(8,35) amp,signois,scrit*calfac2,scr1
c35   	   format(' Initial amplitude estimate = ',f7.2,' pA',/,
c     &	   ' Signal/noise ratio = ',f9.2,/,
c     &	   ' Threshold for transition search = ',f7.2,' pA',/,
c     &	   ' Threshold/rms noise = ',f7.2)
c		   ampdone=.true.
c		   navamp=0		!start new mean
c		   avtot=0.0
		   navamp=11		!so no more averaged in
		   avtot=11.*avamp	!in case more are averaged with later modif
	   	   scrit=smult*abs(avamp)		!in intermed units
		   scr1=scrit/sqrt(vary)	!in intermed units
		   iampcrit=iabs(ifixr(ampfac*avamp))
	         call WDIALOG(1,'Now carry on fitting',icw1)
c  Calculate duration of opening at trigger level
		   aam=abs((float(itriglev)-base)/avamp)
		   ttold=ttrig
		   call AMAX(aam,trise,ttrig,nerr)
		   if(nerr.eq.0) then
         		call INTCONV(ifixr(ttrig),cnum)
			n=NBLANK(cnum)
			tstring='trig='//cnum(1:n)//microsec
c		      if(abs(ttrig-ttold).gt.5.) then !print only if changes by > 5 mus
c      	         if(pon()) write(7,24) ttrig
c      	         if(discprt) write(8,24) ttrig
c			endif
		   else
			tstring='trig= ?     '
		   endif
		   call OPENBOX(2,7,.true.)
		   call WRITBOX(2,tstring,1,10)	!display trigger level
		endif
		goto 150		!back to main menu after 'plot'

	   CASE(13)			!auto write
     		call INTCONV(kt,cnum)
		n=NBLANK(cnum)
	      call WDIALOG(1,cnum(1:n)//
     &	   ' transitions: automatic data back up . . .',-icw)
		call AUTWRIT(kt,tint,iampl,iprops,ntmax,nfits,timsav,ifits,
     &	 ibaselin,nfmax,nbuf,novlap,srate,finter,errfac,derivfac,
     &	 tsect,ioff,ndat,nsec,nrlast,avtot,navamp,avamp,rms,
     &	 nwrit,nwsav,newpar,opendown,invert,confac,
     &	 usepots,disp,smult,scrit,vary,ntrig,navtest,dgain,iboff,
     &	 expfac,bdisp,ibflag,iautosub,xtrig,ndev,cdate,adctime,
     &	 nsetup,filtfile,npfilt,ffilt,
     &	sfac1,sfac2,sfac3,nscale,calfac,calfac1,calfac2,iyoff,ioff1,
     &	 disptran,dispderiv,dispguess,ampfac,tmin,
     &	 tsfac,tlfac,sdone,dfinter,tlast,goback,shutsav,
     &	 shut,shutprev,backward,prevlevel,t0sav,y0sav,vard,nshutfit,
     &	 infit,infirst,ixfprev,idiskq,ifirst,base,basevga,ibasevga,
     &	 itrig,itrigvga,itriglev,inc,incabs,
     &	 indfst,isdfst,isec,ndisp,ndisp1,
     &  	 iscanver,tcfac,facjump,
     & 	 ifreq1,ifreq2,ifreq3,imin,imax,ifmin,ifmax,
     & 	 cjump,ijump,njdim,jumps,kjumps,tzerod,intzero,tsample,
     &	 ktjump,njump,njfit,nnull,ktlast,
     & 	 izoom,fcz,fczoom,ampz,avampsav)
		nwsav=kt    !update for next
c         Erase message (by writing again in backround col, icbd, to avoid
c		problems with string length in wdialog)
c	      call WDIALOG(1,cnum(1:n)//
c     &	   ' transitions: automatic data back up . . .',-icbd)
	   goto 150

	   CASE(15)		!done
            call NUMCLR
	      call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
	      if(iret1.eq.1) goto 150
	      if(iret1.eq.2) goto 150

	END SELECT
c
	call DCMENU(0,4,Lb,text,ibk,ibk)		!delete all
	Lb(ikey)=-ikey		!italic
	goto 502		!another 'options' choice
c
c
c QUIT NOW
99	continue
      call NUMCLR
c	Ask if you want to continue or quit:
c	call set_cursor(ixmes,iymes)
c	call det_string('do you really want to Quit ?',icstr,ch)
c	ans=getch(ktype)
c	if(UC(ans).eq.'n') then
c		call FILLWIN(300,90,639,110,ibk)
c		ifile=1
c		goto 222		!restart from beginning
c	endif
c
c
c999	call mode(3)
c	end
999	continue
	deallocate(idata)
	if(VIDEOTYP().ne.3) call VIDEOMOD(3)

c	call ENDPLT		!Hgraph version
	call NUMCLR
c
998	continue	!for 'go straight to permanent data file'
	nint=kt-1	!number of defined intervals
	if(cjump) then
	   nint=kt		!=======correct?
	   nsweep=ijump
         print 46,kt,nsweep,nnull
         if(pon()) write(7,46) kt,nsweep,nnull
         if(discprt) write(8,46) kt,nsweep,nnull
46	   format(1x,i8,
     &' transitions fitted to ',i6,' jumps, of which ',i4,' were null')
	endif
	call DSKWRIT(tint,iampl,iprops,nint,ntmax,ffilt,rms,avamp,
     & calfac2,expdate,iscanver,
     & ifreq1,ifreq2,ifreq3,imin,imax,ifmin,ifmax,
     & cjump,nsweep,njdim,jumps,kjumps,tzerod,tsample)
c
	if(gostrt) goto 997	!no need for autwrit
c
c After 'quit' store all things need in case restart requested later!
c (put this after DSKWRIT to give another chance to leave with control-C
c and thus avoid overwriting SCANTEMP.DAT)
	call AUTWRIT(kt,tint,iampl,iprops,ntmax,nfits,timsav,ifits,
     &  ibaselin,nfmax,nbuf,novlap,srate,finter,errfac,derivfac,
     &  tsect,ioff,ndat,nsec,nrlast,avtot,navamp,avamp,rms,
     &  nwrit,nwsav,newpar,opendown,invert,confac,
     &  usepots,disp,smult,scrit,vary,ntrig,navtest,dgain,iboff,
     &  expfac,bdisp,ibflag,iautosub,xtrig,ndev,cdate,adctime,
     &  nsetup,filtfile,npfilt,ffilt,
     &  sfac1,sfac2,sfac3,nscale,calfac,calfac1,calfac2,iyoff,ioff1,
     &  disptran,dispderiv,dispguess,ampfac,tmin,
     &  tsfac,tlfac,sdone,dfinter,tlast,goback,shutsav,
     &  shut,shutprev,backward,prevlevel,t0sav,y0sav,vard,nshutfit,
     &  infit,infirst,ixfprev,idiskq,ifirst,base,basevga,ibasevga,
     &  itrig,itrigvga,itriglev,inc,incabs,
     &  indfst,isdfst,isec,ndisp,ndisp1,
     &  iscanver,tcfac,facjump,
     &  ifreq1,ifreq2,ifreq3,imin,imax,ifmin,ifmax,
     &  cjump,ijump,njdim,jumps,kjumps,tzerod,intzero,tsample,
     &  ktjump,njump,njfit,nnull,ktlast,
     &  izoom,fcz,fczoom,ampz,avampsav)
	   call CATCHBUG(ifreq1,ifreq2,ifreq3,imin,imax,ifmin,ifmax)
c
997	continue
	call BELL(2)
	call DCASK(
     &'Do you want to make a backup copy of your restart file'//
     &' (SCANTEMP.DAT)','Y',ans)
	scanbak='scantemp.dat'
42	if(ans.eq.'Y') then
	   call TITENT0(
     &	'Name (and path) for backup copy:',scanbak,30,.false.)
	   INQUIRE(file=scanbak,exist=present,flen=len)
	   if(.not.present.or.len.eq.0) then
c		call SYSTEM('copy scantemp.dat '//scanbak)
c          Modif to warn if specified path does not exist
		call COPY('scantemp.dat',scanbak)   !UTIL3 library routine
		iser=iserror()
		if(iser.eq.0) then
		   if(pon()) write(7,43) scanbak
		   if(discprt) write(8,43) scanbak
43		   format(/,' Restart file (SCANTEMP.DAT) backed up as ',a30)
		else
		   call BELL(1)
		   if(iser.eq.3) then
			print*,'Path not found'
		   else if(iser.eq.5) then
			print*,'Access denied'
		   else if(iser.eq.15) then
			print*,'Invalid drive letter'
		   else if(iser.eq.19) then
			print*,'Disk full'
		   endif
	   	   print 44
44		   format(
     &    		' (1) Try again',/,
     &		' (2) Abandon backup',/,
     &		' Option number [1] = ')
		   iopt=1
		   call INPUTi(iopt)
		   if(iopt.eq.1) goto 42
		endif
	   else
		call BELL(2)
		print 41
41		format(
     &  	' Backup file name already exists:',/,
     &	' (1) Overwrite it',/,
     &	' (2) Give a different name or path',/,
     &	' Option number [2] = ')
		iopt=2
		call INPUTi(iopt)
		if(iopt.eq.1) then
		   call COPY('scantemp.dat',scanbak)   !UTIL3 library routine
		   if(pon()) write(7,43) scanbak
		   if(discprt) write(8,43) scanbak
		else
		   goto 42
		endif
	   endif
	endif
996	continue
	call ENDPRINT
	end

