	subroutine GETBURST(scantfil,scdatfil,nfile,kfile,
     &  tcrit,ndim1,indfst,indend,nburst,avsamp,tpre,
     &  nsamp,nsamp1,dfinter,icfil,idatyp,iscan)
c
c
c Called after GETSCFIL in CJFIT.  This subroutine reads CONSAM,
c SCANTEMP etc to see how many bursts/activations (as defined by tcrit) are
c in the data, and how long data array must be to hold the average
c of the aligned bursts so array can be declared for the
c averaging in AVER_ACT.
c
c Averaging bursts is done only on steady state records so DO NOT NEED
c JUMP DATA HERE.
c
c NB It is necessary to impose resolution, because this affects the
c definition of bursts (e.g. omission of a short opening could easily
c make a gap>tcrit).  But don't need tint() etc except for definition
c of start and end of bursts, so can be local to this subroutine.
c
c Modif 05/08/00 03:49pm to read SCAN.DAT files
c idatyp=1 for scan.dat, idatyp=2 for scandat.dat (defined in getscfil
c iscan here -iscanver in call
c
c Modif 12/16/96 12:47pm so kjumps0, kjumps are integer*4 as in ekdist/resint5
c (they are not read or used here, but must be declared for resint)
c
c Allocate tint etc locally
	allocatable::ygap,yval
	allocatable::tint,tint0
	allocatable::iampl0,ampl0,ampl
	allocatable::iprops,iprops0
	allocatable::index
	real*4 tint(:),tint0(:),ampl0(:),ampl(:)
	real*4 ygap(:),yval(:)
	integer*2 iampl0(:)
	integer*1 iprops(:),iprops0(:)
	integer*4 index(:)
	allocatable::kjumps0,kjumps
	integer*4 kjumps0(:),kjumps(:)
c
c For diskin2-based code
	real*4 calfacs2(50)
c For cdist2 method of defining bursts
	allocatable::ops,gaps
	real*4 ops(:),gaps(:)
c for scan.dat
	character*1 qfile1*35	!as in scan.dat
	character defname*6,adcfil*30
	integer*4 ifmin(3),ifmax(3)	!ifmin(4) in scan, but only 3 written
	integer*4 nfitsav(10)		!keep nfits for each scan.dat
c For read from scantemp
	real*8 dfinter
	allocatable::timsav,ifits,ibaselin
	allocatable::ifits2
	real*8 timsav(:)
	integer*4 ifits(:)
	integer*2 ifits2(:),ibaselin(:)
	integer*4 indfst(ndim1),indend(ndim1)
	allocatable::ibstrt,ibend	!for transition # that start, end bursts
	integer*4 ibstrt(:),ibend(:)
	character ndev*2,cdate*11,adctime*8,filtfile*20
c
c For getscfil (aligned activations from SS record)
	integer kfile(10)
c	character consfil*40		!path for consam.dat file -not needed here
	character*40 scdatfil(10)	!path names for SCANDAT files (char*40 here)
	character*40 scantfil(10)  !path names for SCANTEMP files
	integer nval(10),irecs(10)
c
	integer*4 jstrec(200)
	character name*12,ans*1,UC*1,expdate*11,title*70
	character tapeID*24,expnum*9
	logical cjump
c
	logical opendown,invert,usepots,disp
	logical discprt,readini
	logical btest,dubious,badint,bad,endbad,badshut
	logical open,shut,open1
	logical setbad,setbad1,setbad2
	logical endbst
	logical ppopen,prt
	logical pon,slock,debug,caplock,debon
c
	common/dp/discprt
	common/rblck/treso,tresg,acrit,avamp	!for resint3
c
c
	pon()=slock()
	debug()=caplock()
	dubious(i)=BTEST(iprops(i),0)	!ampl(i) was dubious (bit 0 set ='1')
	badint(i)=BTEST(iprops(i),3)	!tint(i) was unusable (bit 3='8' set)
c
c
101	format(a1)
	nintt=0
	atemp=0.0		!for mean temp
	aavamp=0.0		!for mean avamp
	arms=0.0		!for mean rms
	aEmem=0.0		!for man Emem
c
	OPEN(unit=15,file=scdatfil(1),status='UNKNOWN',
     &		access='TRANSPARENT')
	ind=1		!i=1,...,nintt
	nfm=0
	do i=1,nfile
	   if(idatyp.eq.1) then		!read SCAN.DAT (SCAN.SCN) files
		read(unit=15,rec=1) iscan
		if(iscan.ne.-103) then
		    read(unit=15,rec=1) iscan,ioffset,nint,title,expdate,
     &	    defname,tapeID,ipatch,npatch,Emem,temper,adcfil,
     &	    qfile1,cjump,nfits,ntmax,nfmax,nbuf,novlap,srate,finter,
     &	    tsect,ioff,ndat,nsec,nrlast,avtot,navamp,avamp,rms,
     &	     nwrit,nwsav,newpar,opendown,invert,
     &	     usepots,disp,smult,scrit,vary,ntrig,navtest,dgain,
     &	     iboff,
     &	     expfac,bdisp,ibflag,iautosub,xtrig,ndev,cdate,adctime,
     &	     nsetup,filtfile,ffilt,npfilt,
     &	     sfac1,sfac2,sfac3,nscale,calfac,calfac1,calfac2,iyoff,
     & 	     ioff1,disptran,dispderiv,dispguess,ampfac,tmin,
     &	     tsfac,tlfac,sdone,dfinter
c  read as far as nfmax so can get nfm=max(nfmax) here, rather than below
c Modif 11/27/00 04:09pm -need to read as far as rms!
c=no -need to go up to calfac2 also!  In fact safer to go right up to dfinter
c
		    nfm=nfm+nfmax
		    nfitsav(i)=nfits
		    if(cjump) then
			call BELL(3)
			print 70
70			format(' ERROR: file contains CJUMP data')
			STOP
		    endif
c
	      else if(iscan.eq.-103) then
c    read simulated data in new scan.dat format -dep on whether cjump or not
		 read(18,rec=1) iscan,ioffset,nint,title,expdate,
     &	  tapeID,ipatch,Emem,cjump,avamp,rms,ffilt,calfac2,
     &	  treso,tresg
		 if(cjump) then
		    call BELL(3)
		    print 70
c70		    format(' ERROR: file contains CJUMP data')
		    STOP
		 endif
c		 if(cjump) then
c		    read(18,rec=1) iscan,ioffset,nint,title,expdate,
c     &	    tapeID,ipatch,Emem,cjump,avamp,rms,ffilt,calfac2,
c     &	    treso,tresg,nsweep,tzero,tsamp
c		 endif
		endif
		irecs(i)=ioffset
		kt=nint
		temp=temper
c=		qfile(1:35)=qfile1
		if(iscan.lt.103) then
		   call BELL(2)
		   print 63,iscan
63		   format(' ERROR IN SCAN VERSION NUMBER = ',i5)
		   pause
		endif
	   else if(idatyp.eq.2) then

c Now old scandat files
		irec1=irecs(i)

		if(i.gt.1.and.scdatfil(i).ne.scdatfil(1)) then
		   CLOSE(unit=15)
		   OPEN(unit=15,file=scdatfil(i),status='UNKNOWN',
     &		access='TRANSPARENT')
		endif
		read(15,rec=1) name,idiscn,nfirst,nfiles,jstrec,nextrec
		ifile=kfile(i)
		irec=jstrec(ifile-nfirst+1)	!where to start reading header
		read(15,rec=irec)expnum,ifile1,kt,iscan,expdate,title,
     &       tapeID,ipatch,Emem,temp,ffilt,avamp,rms,calfac2,ioffset,
     &       imin,imax,cjump,nsweep,tzero,tsamp
	   endif
c
	   if(cjump) then
		 call BELL(3)
		 print 70
c70		 format(' ERROR: file contains CJUMP data')
		 STOP
	   endif
	   nintt=nintt+kt			!accumulate total number
	   nval(i)=kt
c	   irec1=irec+ioffset      !where to start reading data -record here
	   irecs(i)=irec + ioffset
	   calfacs2(i)=calfac2
	   avamp=avamp*calfac2		!convert to pA
c	   if(ifile.ne.ifile1) then
c		print 37,ifile,ifile1
c37		format(' ERROR; ifile,ifile = ',2i8)
c		stop
c	   endif
c Check that filter setting and patch type same for all files
	   if(i.eq.1) then
		ffilt1=ffilt
		ipatch1=ipatch
		Emem1=Emem
	   else
		if(ffilt.ne.ffilt1) then
		   call BELL(1)
	         print 30,i,ffilt,ffilt1
		   if(pon()) write(7,30) i,ffilt,ffilt1
	         if(discprt) write(8,30) i,ffilt,ffilt1
30		   format(' WARNING: filter setting for file #',i3,
     &		' not same as that for first file (',2g13.6,')')
		endif
		if(ipatch.ne.ipatch1) then
		   call BELL(1)
	         print 31,ipatch,i,ipatch1
		   if(pon()) write(7,31) ipatch,i,ipatch1
	         if(discprt) write(8,31) ipatch,i,ipatch1
31		   format(' WARNING: patch type for file #',i3,
     &		' not same as that for first file')
		endif
		if(Emem.ne.Emem1) then
		   call BELL(1)
	         print 32,Emem,i,Emem1
		   if(pon()) write(7,32) Emem,i,Emem1
	         if(discprt) write(8,32) Emem,i,Emem1
32		   format(' WARNING: potential for file #',i3,
     &		' not same as that for first file')
		endif
	   endif
c	 calc means
	   atemp=atemp+temp
	   aavamp=aavamp+avamp
	   arms=arms+rms
	   aEmem=aEmem+Emem
c=== print details for each expt file as in getscan? Omitted for now!
	enddo		!end of first pass
c
	CLOSE(unit=18)
c
c    calc mean temp, avamp, rms: and check ffilt, ipatch same for all
	if(iscan.ge.99) then		!new data disk from PC SCAN
	   temp=atemp/float(nfile)
	   avamp=aavamp/float(nfile)
	   rms=arms/float(nfile)
	   Emem=aEmem/float(nfile)
c
         print 39, nintt,temp,avamp,rms,Emem
         if(pon()) write(7,39) nintt,temp,avamp,rms,Emem
         if(discprt) write(8,39) nintt,temp,avamp,rms,Emem
39	   format(/,' Total number of transitions = ',i8,/,
     &     ' Temperature (mean) = ',f9.1,/,
     &     ' Approx full amplitude (mean) = ',f9.2,/,
     &     ' RMS baseline noise (mean) = ',f10.4,/,
     &     ' Potential (mV) (mean) = ',f9.2,/)
	endif
c
c Can now allocate arrays
	ALLOCATE(tint0(nintt),iampl0(nintt),iprops0(nintt),ampl0(nintt))
	ALLOCATE(tint(nintt),ampl(nintt),iprops(nintt))
	ALLOCATE(index(nintt))
	ALLOCATE(ygap(nintt),yval(nintt))
	ndimy=nintt		!dimension for Yval
c jumps not used here, but must be allocated for RESINT
	ALLOCATE(kjumps0(nintt),kjumps(nintt))
c
c Rest of code is based on DISKIN2 (all point amp stuff removed -not needed here
c
c Now read and pool the expts
	OPEN(unit=15,file=scdatfil(1),status='UNKNOWN',
     &	access='TRANSPARENT')
	ind=1		!i=1,...,nintt
	do i=1,nfile
	   if(i.gt.1.and.scdatfil(i).ne.scdatfil(1)) then
		CLOSE(unit=15)
		OPEN(unit=15,file=scdatfil(i),status='UNKNOWN',
     &		access='TRANSPARENT')
	   endif
c Read of data (tint, iampl, iprops) should be same for new scan.dat files
c as for old scandat.dat files, as long as irecs() set properly (above)
	     irec1=irecs(i)
	     j1=ind
	     j2=ind+nval(i)-1
c NOTE!
c Do not read point amp data, even when present
c Averaging bursts is done only on steady state records so do not need
c jump data either here.
c
	   read(15,rec=irec1)
     &      (tint0(j),j=j1,j2),(iampl0(j),j=j1,j2),(iprops0(j),j=j1,j2)
c     &	 (ifmin(j),j=1,3),(ifmax(j),j=1,3)
c Can avoid declaring jfreq etc by calculating the start record, in bytes,
c for the start of the jumps() array.  The tint0 array starts at rec=irec1
c tint0 = (j2-j1+1)*4 bytes
c iampl0 = (j2-j1+1)*2 bytes
c iprops0 = (j2-j1+1)*1 bytes
c ifmin=3*4=12 bytes
c ifmax=3*4=12 bytes
c jfreq1=(ifmax(1)-ifmin(1)+1)*4 bytes
c jfreq2=(ifmax(2)-ifmin(2)+1)*4 bytes
c jfreq1=(ifmax(3)-ifmin(3)+1)*4 bytes
c	   irec2=irec1 + (j2-j1+1)*7 + 24 + (ifmax(1)-ifmin(1)+1)*4 +
c     &	(ifmax(2)-ifmin(2)+1)*4 + (ifmax(3)-ifmin(3)+1)*4
c	   read(15,rec=irec2)
c     &  	 (jumps(j),j=1,nsweep),(kjumps0(j),j=1,nsweep)
c
c
c Now convert amplitudes to picoamps with calfac for this particular file
c
	   do j=j1,j2
		ampl0(j)=calfacs2(i)*float(iampl0(j))
	   enddo
c
	enddo		!end of 'do i=1,nfile'
c
	CLOSE(unit=15)
c Now find all gaps > tcrit.  No need to bother about resolution here since
c gaps > tcrit will always be resolvable?  Actually this is not so, because
c one missed opening could easily extend a gap beyond tcrit, when it
c would have been below if an opening had been detected.
c
c Set resolution
	   imode=0		!call RESINT from EKDIST
	   fc=ffilt			!kHz (-3dB)
	   trise=332.1/fc		!rise time (microsec)
	   call RESINT5(tint0,tint,ampl0,ampl,iprops0,iprops,nint,
     &     nintt,imode,ffilt,fc,rms,trise,index,
     &     cjump,nsweep,kjumps0,kjumps)
	   DEALLOCATE(kjumps0,kjumps)		!needed only for call to RESINT
c
c Locate bursts: as in CDIST2 (line 585), (or as in CDISTJ? line 2370)
c All we need is point (abs index) in CONSAM of the start and end of the
c burst (end needed so that rest of sample can be filled with baseline).
c BUT need also to check whether the burst so located contains any
c bad intervals, if bursts with bad intervals are to be excluded.
c
326	if(tcrit.le.0.) then
	   print 321
321	   format(/,' Critical gap length to define clusters (ms) = ')
	   call INPUTr(tcrit)
	else
	   print 320,tcrit
320	   format(' Critical gap length [',f7.2,'ms] = ')
	   call INPUTr(tcrit)
	endif
	if(tcrit.le.0.00001) then
	   call BELL(1)
	   goto 326
	endif
c
	endbad=.false.
	print 85
85	format(
     & ' Should a gap of undefined length be deemed to end burst ',
     & '(>tcrit) [N] ? ')
	read 101,ans
	if(UC(ans).eq.'Y') endbad=.true.
c
c Allocate arrays ibstrt,ibend to record transition # for the transitions
c that start and end each burst (these are converted later to absolute
c index of corresponding points in CONSAM.  Dimension=nint (much too big,
c but safe)
	ALLOCATE(ibstrt(nint),ibend(nint))
c -moved to main prog so they are available later
c LOCATE BURSTS AS IN CDISTJ (LINE 2370)
c This now gives same answer as CDIST2 method, so remove this one?
c (left in as check for now)
	nbmax=2000
90	continue 	!return here to re-allocate larger arrays
	ALLOCATE(ops(nbmax),gaps(nbmax))
c
	j=0 		!to count open periods
	j1=0		!to count number of bursts
	n1=0		!to count number of open periods per burst
	nbad1=0	!to count number of open bursts abandoned because
c			!of bad interval
	bad=.false.	!current burst contains no intervals of undefined length
c
c
	ifirst=1
	ilast=nint
c
c Find start of a group (open period) -valid start must have a good open
c time followed by a good shutting -if a bad opening is found as first (or
c any later) opening then the open period is abandoned altogether, and the
c next good shut time sought as start for next open period
	ifst=ifirst
	n=0			!counts number of open periods
	sum1=0.0		!add burst length
	sum2=0.0		!add TO/bst
80	continue
	if(mod(j1-1,10).eq.0) then	!check if debug on every 10th burst
	   debon=debug()
	endif
	do while (ifst.le.ilast-1)
	   a1=ampl(ifst)
	   a2=ampl(ifst+1)
	   open1=a1.ne.0
c	   open2=a2.ne.0
c	   shut1=.not.open1
c	   shut2=.not.open2
	   setbad1=badint(ifst)		!tint(i) was unusable
	   setbad2=badint(ifst+1)		!tint(i) was unusable
	   if(debon) then	!print first in group
		print 501,ifst,tint(ifst),ampl(ifst),iprops(ifst)
      	if(discprt) write(8,501)ifst,tint(ifst),ampl(ifst),
     &		iprops(ifst)
501	   	format(i6,g12.5,g12.5,i5)
	   endif
	   if(setbad1.or.setbad2) then
		ifst=ifst+1
		n=n+1      		!count this as nth open period, although not usable
		bad=.true.		!not reset true until end of current burst
		goto 80  		!look for next good start
	   else if(open1) then
		if(debon) then	!print first in group
		   print 507
      	   if(discprt) write(8,507)
507		   format(' start of open period found')
		endif
		n=n+1 		!count this as nth open period
		ifst=ifst-1		!so ifst-> 1st opening when incremented below
		goto 81		!valid 'start of open period' now found
	   endif
	   ifst=ifst+1		!keep looking for good start
	enddo
c if this point reached, all intervals inspected
	j=j-1
	goto 83	!all intervals in this jump inspected
c valid 'start of open period' found
81	continue
	j=j+1		!count number of open periods found (reversed if bad)
	ifst=ifst+1		!index of first opening in open period just started
	ibstrt(j1+1)=ifst		!j1 not yet incremented
c now sum1=bst length, sum2=TO/bst
	sum1=sum1 + tint(ifst)	!add length of this opening to prev op period
	sum2=sum2 + tint(ifst)	!add length of this opening to prev op period
	if(debon) then	!print first in group
	   print 501,ifst,tint(ifst),ampl(ifst),iprops(ifst)
         if(discprt) write(8,501)ifst,tint(ifst),ampl(ifst),iprops(ifst)
	endif
c if last interval is open there are no more intervals
c to find, and do not want the next 'do while' to decrement j, so skip to end.
	if(ifst.eq.ilast) then
	   k=ifst	!so do not return to 80 to look for new group
	   goto 82
	endif
	k=ifst+1	!index of interval that follows the first opening
c Now find the rest of the openings in this open period
	do while (k.le.ilast)
	   if(debon) then		!print rest in group (inc shutting that ends it)
		print 501,k,tint(k),ampl(k),iprops(k)
      	if(discprt) write(8,501) k,tint(k),ampl(k),iprops(k)
	   endif
	   a1=ampl(k)
	   open1=a1.ne.0
c Go to 82 when end of group found.
c Whole group is set bad, and so rejected, if it contains any unusable
c opening
	   if(.not.open1) goto 82		!end of group found
c now sum1=bst length, sum2=TO/bst
	   sum1=sum1 + tint(k)	!add any subsequent openings to same YVAL
	   sum2=sum2 + tint(k)	!add any subsequent openings to same YVAL
	   setbad=badint(k)		!tint(k) was unusable
	   if(setbad) bad=.true.	!this group bad, but finish anyway
	   k=k+1
	enddo
c get here if last interval is reached without finding end
c of the current open period
	j=j-1		! j not used for anything here
	goto 83	!all intervals in this jump inspected
c
c Open period completed
82	continue
	n1=n1+1			!count number of open periods/burst
c
	ifst=k	!so do not skip past shut time needed to start new group
	ygap(j)=TINT(k)   !record length of shut time following open period #j
	badshut=badint(k)	!set true if gap is bad
	if(ampl(k).ne.0) then
c	abort burst and go to next sweep if open at end
	   nbad1=nbad1+1	!count # of aborted bursts
	   sum1=0.0		!initialise for next burst
	   sum2=0.0		!initialise for next burst
	   n1=0.0		!initialise for next burst
	   bad=.false.	!initialise for next burst
	   goto 79
	endif
c NOW check if end of burst reached
	if(badshut) then
	   endbst=endbad		!bad gap ends burst in endbad=true
	else
	   endbst=ygap(j).gt.tcrit
	endif
	if(.not.endbst) then
	   sum1=sum1 + ygap(j)		!add shut time for total bst length
	   if(badshut) bad=.true.	!bad gap makes whole burst bad
	   if(ifst.lt.ilast) goto 80	!look for start of next open period
c Arrive here if last interval is reached but burst has not ended
c In this case, burst is not complete so this burst
c must be discarded, so abort burst and go to next sweep
	   sum1=0.0		!initialise for next burst
	   sum2=0.0		!initialise for next burst
	   n1=0.0		!initialise for next burst
	   bad=.false.	!initialise for next burst
	   goto 79
	else		!burst ended
	   if(.not.bad) then
		j1=j1 + 1	!count number of bursts
		ibend(j1)=k
      	n2=n2 + 1 	!count number bursts per sweep
		Yval(j1)=sum1	!burst length
c=		Yval(j1)=sum2	!TO/burst
c=		Yval(j1)=float(n1) 	!no of open periods/burst
		if(debon) then
		   print 502,j1,yval(j1)
	         if(discprt) write(8,502) j1,yval(j1)
502		   format(36x,' Yval(',i4,') = ',g12.5)
		endif
	   else
		nbad1=nbad1+1	!count # of bad bursts
		if(debon) then
		   print 506
	         if(discprt) write(8,506)
506		   format(36x,' Burst contained a bad interval -discarded')
		endif
	   endif
	   sum1=0.0		!initialise for next burst
	   sum2=0.0		!initialise for next burst
	   n1=0.0		!initialise for next burst
	   bad=.false.	!initialise for next burst
	   if(ifst.lt.ilast) goto 80	!look for start of next open period
c						!i.e. start of next burst
	endif
C N.B. does not return if ifst=nint, as if tint(nint) is start of an
C open period there is no way to tell if this group has been completed-
C (the next interval might have been open too)
c
83	continue	!all intervals in this jump inspected
c
79	continue
c
	NYVAL=j1		!no of bursts
c
c ALL DONE: Yval() now defined
c207	continue
	if(allocated(ygap)) DEALLOCATE(ygap)
      print 86, nyval
      if(pon()) write(7,86) nyval
      if(discprt) write(8,86) nyval
86	format(/,' ',i5,' values for distribution')
c
      print 87,nbad1
      if(pon()) write(7,87) nbad1
      if(discprt) write(8,87) nbad1
87	format(
     & ' ',i4,' bursts aborted by undefined intervals')
c
999	continue
	call VARV1(Yval,nyval,ybar,vary,ymin,ymax)
	print 58,nyval,ybar,sqrt(vary),ymin,ymax
	if(pon()) write(7,58) nyval,ybar,sqrt(vary),ymin,ymax
	if(discprt) write(8,58) nyval,ybar,sqrt(vary),ymin,ymax
58	format(/,' Burst lengths (cdistj method):',/,
     & ' Mean and SD of ',i5,' values= ',g13.6,' +/-',g13.6,
     & /,' Range from ',g13.6,' to ',g13.6,/)
c=	pause 'now CDIST2 method'
c
c============
cALTERNATIVE -LOCATE BURSTS AS IN CDIST2 (line 585)
c=============
	Ifst=1
	Nbst=0		!NO OF BURSTS
	Nsgap=0		!NO OF GAPS-WITHIN-BST
	Nopen=0		!NO IF INDIV OPENINGS
	NAset=0		!NO OF OPEN (SUBSET A) PERIODS
	NBset=0		!NO OF GAP (SUBSET B) PERIODS
	indlast=0	!to control printing below
	prt=pon()
c
c BURST defined by gaps (zero amp) < Tcrit
c NOP includes any openings inc sublevels
c NSG counts gaps<Tcrit within bursts (so number of sojourns in C&H subset A
c per burst = NSG+1 =< NOP.
c  FIRST LOOK FOR FIRST OPENING (must be usable) in burst.
360	continue
c=	bprint=.false.		!reset for each bst
	if(mod(nbst-1,10).eq.0) then	!check if debug on every 10th burst
	   debon=debug()
	endif
c Burst extends from TINT(ifst) to TINT(k-1)
c Locate start and end of burst
	amp=ampl(ifst)
	OPEN=amp.ne.0
	IF(open.and.(.not.badint(ifst))) goto 322
c    First opening of burst, tint(ifst), found so go to 322
	ifst=ifst+1
	if(ifst.le.nint) goto 360	!KEEP LOOKING FOR FIRST OPENING
	goto 99
C  Now have 1st opening of bst =tint(ifst). Do not change ifst until
c burst ends
322	continue
	NOP=1		!Number of openings in current burst
	NSG=0		!No of gaps<Tcrit in current burst
	NAS=0		!No of open (subset A) periods in current bst
	NBS=0		!No of gaps (subset B) in current bst
c			!=NAS-1 (same as NSG for ibtype=1,3)
	topen=0.0	!length of open period
	tgap=0.0	!length of gap-within-bst period
	topen=tint(ifst)	!first indiv opening
	nbst=nbst+1	!Update # of bursts- reversed below if bst is bad
	ibstrt(nbst)=ifst
	bad=.false.	!current burst is usable
	igflag=0	!currently not in a gap period
	ioflag=1	!currently in an open period
	k=ifst+1		!look at next interval -does it end burst?
C  k loop starts at label 323
323	continue
	amp=ampl(k)
	open=amp.ne.0
	shut=.not.open
c=	if(shut.and.(tc.gt.0.001).and.tint(k).ge.tc) bprint=.true.
c End of burst? If shut can update TGAP here because if this shutting
c ends the burst then the current tgap not used for anything, and
c if it does not enf the burts then TGAP to be updated anyway
	if(shut) tgap=tgap+tint(k)
	if(shut.and.(tgap.gt.tcrit)) goto 324
	if(endbad.and.badint(k).and.k.lt.nint) goto 324
C  Go to 324 when end of bst found, with k=index of gap that ends it.
C NOTE that the last interval is always set unusable, but it is not
C a valid end-of-burst
	if(open) topen=topen+tint(k)	!note-each opening is sub OR full
40	continue
	if((open.and.badint(k)).or.(k.eq.nint)) bad=.true.
c    Burst not counted if it contains an unusable opening (not pos in current
c	version of SCAN ,or if last data point reached before valid end-of-burst
c 	found, so set bad, but carry on to end anyway to make sure start of
c	next burst found correctly
c Now assign lengths for open and gap periods.
c===	if(open) goto 41	!'open'
c===	goto 42			!'gap'
c===line 701 in cdist2
c===line 865 in cdist2
c Allocate 'open' and 'gap' lengths
c Gap ends as soon as open period found (except for 1st open period, NAS=1)
c so assign length of gap that precedes it (only done when 1st opening of
c an open period found- controlled by IOFLAG)
c Next bit done if tint(k) is 'open'
	if(open) then
41	   nop=nop+1	!no of indiv openings
	   ioflag=1	!currently in an open period
	   if(igflag.eq.0) goto 43
	   nbs=nbs+1
	   gaps(nbs)=tgap
	   tgap=0.0		!zero for next gap period
	   igflag=0		!clear flag
c==	   goto 43
c Next bit done if tint(k) is 'gap'
c Open period ends as soon as gap found so assign length of open period
c that precedes it (only done when 1st gap of an gap period found-
c controlled by IGFLAG)
	else
42	   nsg=nsg+1	!no of indiv gap periods
	   igflag=1	!currently in a gap period
	   if(ioflag.eq.0) goto 43
	   nas=nas+1
	   if(nas.gt.nbmax) then
		print 623,nbmax,3*nbmax
623	      format(' *** MORE THAN ',i5,' OPENINGS/BURST',/,
     &    ' Re-allocating arrays to hold ',i6,' and starting again') !enlarge OPS()
		DEALLOCATE(ops,gaps)
		nbmax=3*nbmax
		goto 90	!start again
	   endif
	   ops(nas)=topen
	   topen=0.0		!zero for next open period
	   ioflag=0		!clear flag
	endif

43	continue
	k=k+1
c Continue in same burst
	if(k.le.nint) goto 323
	Nbst=Nbst-1	!REVERSE Nbst UPDATE IF NO PROPER ENDING FOUND
	goto 99		!DO NOT COUNT AS BURST IF NO PROPER ENDING FOUND
C
C END OF BURST FOUND. RECORD IT (IF NOT SET BAD). IF SET BAD YVAL(Nbst)
C VALUES WILL BE OVERWRITTEN BY THOSE FOR NEXT BURST
324	continue
	nas=nas+1
	ops(nas)=topen		!assign last opening of the burst
3241	continue
	if(debon) then
c     type intervals preceding current burst if any
	  if(ifst-1.gt.ilast) call DEBTYP(0,ilast,ifst-1,tint,ampl,
     &   iprops,0,-1,-1,yval,prt,nintt,ndimy)
	   print 618,Ifst,k-1
618	   format(/,/,' BURST=intervals ',i5,' to ',i5)
c      print current bst
	   call DEBTYP(0,ifst,k-1,tint,ampl,iprops,
     & 0,-1,-1,yval,prt,nintt,ndimy)
c     print 2 intervals following current bst
	   call DEBTYP(0,k,k+1,tint,ampl,iprops,
     & 0,-1,-1,yval,prt,nintt,ndimy)
	endif
c
	ilast=k		! 1st interval to print for next call
c
524	continue
	if(bad) then
	   Nbst=Nbst-1	!reverse update of Nbst
	   goto 325 	!Skip assignment of Yval; look for next bst
	endif
c
 	if(debon) print 4051,ifst,k,Nbst,Nop,nsg,nas,nbs
4051	format(' End of burst: ifst,k,Nbst,Nop,nsg,nas,nbs= ',/,7I8)
c
	if(nas.ne.nbs+1) goto 597
	if(nbs.ne.nsg) goto 597
	if(nas.ne.nsg+1) goto 597
	goto 599
597	call bell(2)
	print 598,nas,nbs,nop,nsg
598	format(' ***ERROR: nas,nbs,nop,nsg= ',4i8)
	pause
599	continue
c
c Assign YVAL() for id=1,2
c=	YVAL(Nbst)=float(nop)
c=	YVAL(Nbst)=float(nbs+1)
c=	if(deb) call DEBTYP(0,-1,-1,tint,ampl,iprops,
c=     & 0,nbst,nbst,yval,prt,nintt,ndimy)	!print YVAL(j)
c=	if(id.le.2) goto 338		!this burst finished
c
	ibend(nbst)=k	!transition that ends burst
	yval(nbst)=0.0
	do 600 m=1,nAs
	   yval(nbst)=yval(nbst) + ops(m)	!add open times for all (-> TO/bst)
c add gaptime for 'total burst length'
	   if(m.lt.nAs) then
		yval(nbst)=yval(nbst) + gaps(m)
	   endif
600	continue
	if(debon) call DEBTYP(0,-1,-1,tint,ampl,iprops,
     & 0,nbst,nbst,yval,prt,nintt,ndimy)	!print YVAL(j)
c
C End of current burst: UPDATE Nsgap ETC BY ADDING VALUES FOR LAST BURST
c print this burst?
c Bit added 06/22/89 01:29pm to print total open, total shut and
c Popen for each burst when id=3, if ppopen=true.
	totop=0.
	totgap=0.
	do 401 i=1,nas
401	totop=totop+ops(i)
	do 402 i=1,nbs
402	totgap=totgap+gaps(i)
	blength=totop+totgap
	popen=totop/blength
c
357	continue
	if(debon.and.(kplast.gt.0).and.(ifst-1.ge.kplast+1)) then
         print 54
         if(prt) write(7,54)
         if(discprt) write(8,54)
54	   format(/,' Intervals between bursts:')
	   do m=kplast+1,ifst-1
	      m1=m
            print 56,m,tint(m),ampl(m1)
            if(pon()) write(7,56) m,tint(m),ampl(m1)
            if(discprt) write(8,56) m,tint(m),ampl(m1)
c56	      format(6x,i6,2x,2g13.6)
56	      format(i6,2x,2g13.6)
	   enddo
	   print 108
	   if(prt) write(7,108)
	   if(discprt) write(8,108)
108	   format(/)
	endif
	if(debon) then
         print 57, nbst
         if(pon()) write(7,57) nbst
         if(discprt) write(8,57) nbst
57	   format(' Burst #',i4)
	   do m=ifst,k-1
		m1=m
		if(prt) write(7,356) m,tint(m),ampl(m1)
	      if(discprt) write(8,356) m,tint(m),ampl(m1)
355		print 356,m,tint(m),ampl(m1)
356		format(i6,2x,2g13.6)
	   enddo
	   print 108
	   if(prt) write(7,108)
	   if(discprt) write(8,108)
	endif
	kplast=k-1		!index of last tint() printed
354	continue
c
	ppopen=.false.		!for now
	if(ppopen) then
	   print 403,totop,nas,totgap,nbs,
     &	blength,popen
	   if(prt)write(7,403) totop,nas,totgap,nbs,
     &	blength,popen
	   if(discprt)write(8,403) totop,nas,totgap,nbs,
     &	blength,popen
c403	   format(' Burst #',i4,/,
403	   format(
     &  ' Total open = ',g13.6,' (n = ',i4,');',
     &  ' Total shut = ',g13.6,'(n = ',i4,')',/,
     &  ' Burst length = ',g13.6,'   P(open) = ',g13.6)
c   Also print yval if a valid value is defined -need to define index for
c yval to do this (as in def of nyval below)
	   index1=nbst
	   if(index1.ne.indlast) then
      	print 404, index1,Yval(index1)
	      if(pon()) write(7,404)  index1,Yval(index1)
      	if(discprt) write(8,404)  index1,Yval(index1)
404		format(
     &  ' Value for distribution: Yval(',i4,') = ',g13.6)
		indlast=index1
	   endif
	endif
c
	Nsgap=Nsgap+NSG	!Nsgap=TOTAL NO OF SHORT GAPS IN CURRENT EXPT
	Nopen=Nopen+NOP
	NAset=NAset+nAs
	NBset=NBset+nBs
325	ifst=k+1	!index of interval after that which ended last bst
	if(ifst.le.nint) goto 360
c END of ifst loop
C RECORDING OF VALUES FOR LAST BURST COMPLETED- GO TO 360 TO LOOK
C EXPLICITLY FOR FIRST OPENING OF NEXT BURST
C
C ALL BURSTS IDENTIFIED.
99	continue
	Nyval=Nbst
	print 3281
3281	FORMAT(' Print burst parameters [Y] ? ')
	read 101,ans
	if(UC(ans).EQ.'N') GOTO 4091
	if(prt) write(7,328)tcrit,NOPEN,nsgap,NASET,nBset,NBST,
     & FLOAT(NOPEN)/FLOAT(NBST),FLOAT(NASET)/FLOAT(NBST),
     & FLOAT(nBset)/FLOAT(NBST)
      if(discprt) write(8,328)tcrit,NOPEN,nsgap,NASET,nBset,NBST,
     & FLOAT(NOPEN)/FLOAT(NBST),FLOAT(NASET)/FLOAT(NBST),
     & FLOAT(nBset)/FLOAT(NBST)
	print 328,tcrit,NOPEN,nsgap,NASET,nBset,NBST,
     & FLOAT(NOPEN)/FLOAT(NBST),FLOAT(NASET)/FLOAT(NBST),
     & FLOAT(nBset)/FLOAT(NBST)
328	FORMAT(/,
     & ' Critical gap length (ms)= ',g13.6,/,
     & ' No of indiv openings= ',I8,/
     & ' No of indiv gaps within bursts= ',I8,/
     & ,' No of open periods= ',I8,/
     & ,' No of gap periods within bursts= ',I8,/
     & ,' No of bursts= ',I8,/
     & ,' Mean no of indiv openings/burst= ',G13.6,/
     & ,' Mean no of open periods/burst= ',G13.6,/
     & ,' Mean no of gaps/burst= ',G13.6/)
c NB only if ibtype=2 can no of indiv gaps differ from no of gap periods
c
4091	continue
	DEALLOCATE(ops,gaps)
	call VARV1(Yval,nyval,ybar,vary,ymin,ymax)
	print 58,nyval,ybar,sqrt(vary),ymin,ymax
	if(pon()) write(7,58) nyval,ybar,sqrt(vary),ymin,ymax
	if(discprt) write(8,58) nyval,ybar,sqrt(vary),ymin,ymax
c
c YVAL AND NYVAL HAVE NOW BEEN DEFINED.
	DEALLOCATE(Yval)
c
	nburst=nyval
c
c Decide length of sample for averaging -take default as 2*longest burst
c rounded up to nearest 10, 100, 1000 according as ymax is of order of 10, 100,
c 1000 . . .
	if(icfil.gt.1) goto 10	!avsamp and tpre must be same for all CONSAM
c
	ord=10**(ifix(alog10(ymax)))	!=10, 100, 1000
	avsamp=ord*float(1+ifix(ymax/ord))
	print 6,ymax,avsamp
6	format(/,
     & ' Longest burst is ',g13.6, 'ms: ',/,
     & ' Length of sample to be averaged (ms) [',f11.1,'] = ')
	call INPUTr(avsamp)
	tpre=0.05*avsamp	!default
	if(tpre.gt.tcrit) tpre=tcrit
71	print 7,tpre
7	format(' Length before t=0  (ms) [',f7.1,'] = ')
	call INPUTr(tpre)
	if(tpre.gt.tcrit) then
	   call BELL(1)
	   print 8,tcrit
8	   format(' -should not be more than tcrit = ',f7.3)
	   goto 71
	endif
c
10	continue	!jump here if icfil>1 (after first consam)
      if(discprt) write(8,60) tcrit,avsamp,tpre
60	format(
     & ' Critical gap length to define clusters (ms) = ',g11.4,/,
     & ' Length of to be averaged (after t=0)   (ms) = ',g11.4,/,
     & ' (plus ',g11.4,' ms displayed before t=0)')
c
c
c Now get stuff for fits
c Allocate and read timsav,ifits,nfits
c
	if(idatyp.eq.2) goto 61		!read old format scandat.dat
c
c READ FOR SCAN.DAT (SCAN.SCN) files (all in scan.dat -no need for scantemp
c which is identical)
c
c		read(unit=18,rec=1) iscan
c		if(iscan.ne.-103) then
c		    read(unit=18,rec=1) iscan,ioffset,nint,title,expdate,
c						  4      4     4    70    11           =93
c     &	     defname,tapeID,ipatch,npatch,Emem,temper,adcfil,
c			  6      24     4       4     4     4     30             =76
c     &	     qfile1,cjump,
c			 35     4                                                =39
c     &	     nfits,ntmax,nfmax,nbuf,novlap,srate,finter,
c			 4     4     4     4     4     4      4                  =28
c     &	     tsect,ioff,ndat,nsec,nrlast,avtot,navamp,avamp,rms,
c			 4    4    4    4     4     4       4     4    4         =36
c     &	     nwrit,nwsav,newpar,opendown,invert,
c			 4     4     4      4        4                           =20
c     &	     usepots,disp,smult,scrit,vary,ntrig,navtest,dgain,iboff,
c			  4     4     4    4     4     4     4       4     4,    =36
c     &	     expfac,bdisp,ibflag,iautosub,xtrig,ndev,cdate,adctime,
c			 4      4      4       4       4    4   11      8        =43
c     &	     nsetup,filtfile,ffilt,npfilt,
c			 4       20      4     4                                 =32
c     &	     sfac1,sfac2,sfac3,nscale,calfac,calfac1,calfac2,iyoff,
c			 4     4      4      4     4      4        4    4        =32
c     &	     ioff1,disptran,dispderiv,dispguess,ampfac,tmin,
c			  4     4        4          4       4     4          =24 =459
c     &	     tsfac,tlfac,sdone,dfinter,tlast,
c			  4    4     4     8           459+12=471 so dfinter=byte 472
c     &	     shut,shutprev,backward,prevlevel,t0sav,y0sav,vard,
c     &	     nshutfit,
c     &	     infit,infirst,ixfprev,idiskq,ifirst,base,basevga,
c     &	     ibasevga,
c     &	     itrig,itrigvga,itriglev,inc,incabs,
c     &	     indfst,isdfst,isec,ndisp,ndisp1,
c     &	     idatyp,cdate1,nchan,
c     &	     tcfac,facjump,shutsav,goback,
c     &	     imin,imax,errfac,derivfac,confac,
c     &	     nsweep,njdim,tzerod,intzero,tsample,ktjump,njfit,
c     &	     njump,nnull,ktlast,
c     &	     izoom,fcz,fczoom,ampz,avampsav,
c     &	     itsimp,minmeth,nbasemin,iscrit,scritvar,smultmin
c		else if(iscan.eq.-103) then
cc    read simulated data in new scan.dat format -dep on whether cjump or not
c		 read(18,rec=1) iscan,ioffset,nint,title,expdate,
c    &	  tapeID,ipatch,Emem,cjump,avamp,rms,ffilt,calfac2,
c    &	  treso,tresg
c		 if(cjump) then
c		    read(18,rec=1) iscan,ioffset,nint,title,expdate,
c    &	    tapeID,ipatch,Emem,cjump,avamp,rms,ffilt,calfac2,
c    &	    treso,tresg,nsweep,tzero,tsamp
c		 endif
c		endif
c		irecs(i)=ioffset
c		kt=nint
c		temp=temper
c and data format for scan.dat is
c	if(.not.cjump) then
c	   write(unit=15,rec=ioffset)
c     &  (tint(i),i=1,nint),(iampl(i),i=1,nint),(iprops(i),i=1,nint),
c     &  (ifmin(i),i=1,3),(ifmax(i),i=1,3),
c     &  (ifreq1(i),i=ifmin(1),ifmax(1)),
c     &  (ifreq2(i),i=ifmin(2),ifmax(2)),
c     &  (ifreq3(i),i=ifmin(3),ifmax(3)),
c     &  (timsav(i),i=1,nfits),(ifits(i),i=1,nfits),
c     &  (ibaselin(i),i=1,nfits)
c	else if(cjump) then
c	   write(unit=15,rec=ioffset)
c     &  (tint(i),i=1,nint),(iampl(i),i=1,nint),(iprops(i),i=1,nint),
c     &  (ifmin(i),i=1,3),(ifmax(i),i=1,3),
c     &  (ifreq1(i),i=ifmin(1),ifmax(1)),
c     &  (ifreq2(i),i=ifmin(2),ifmax(2)),
c     &  (ifreq3(i),i=ifmin(3),ifmax(3)),
c     &  (jumps(i),i=1,nsweep),(kjumps(i),i=1,nsweep),
c     &  (timsav(i),i=1,nfits),(ifits(i),i=1,nfits),
c     &  (ibaselin(i),i=1,nfits)
c	endif
c
c
c Find total number of fits (>nfmax if more than one file)
c  nfm=SUM(nfmax) already defined above for scan.scn file
	ALLOCATE(timsav(nfm),ifits(nfm),ifits2(nfm),ibaselin(nfm))
c Re-open scan.scn (rtather than scantemp , and
c  read up to ioffset only, then dfinter separately
	OPEN(unit=15,file=scdatfil(1),status='UNKNOWN',
     &		access='TRANSPARENT')
c No need to read tint, iampl, iprops again, and don't want point amp stuff
c so read timsav etc directly
c Can avoid declaring jfreq etc by calculating the start record, in bytes,
c for the start of the timav() array.  The tint0 array starts at rec=ioffset
c tint0 = (j2-j1+1)*4 bytes=nval(i)*4
c iampl0 = (j2-j1+1)*2 bytes
c iprops0 = (j2-j1+1)*1 bytes
c ifmin=3*4=12 bytes
c ifmax=3*4=12 bytes
c jfreq1=(ifmax(1)-ifmin(1)+1)*4 bytes
c jfreq2=(ifmax(2)-ifmin(2)+1)*4 bytes
c jfreq1=(ifmax(3)-ifmin(3)+1)*4 bytes
c	   irec2=irec1 + (j2-j1+1)*7 + 24 + (ifmax(1)-ifmin(1)+1)*4 +
c     &	(ifmax(2)-ifmin(2)+1)*4 + (ifmax(3)-ifmin(3)+1)*4
c tint=4*kt bytes, iampl=2*kt bytes, iprops=kt bytes, total=7*kt bytes
c
	read(unit=15,rec=1) iscanver,ioffset
c=	read(unit=15,rec=472) dfinter
	if(iscan.ne.-103) then
	   read(unit=15,rec=ioffset+7*nval(1)) ifmin,ifmax
c    Calc record for timsav etc (different for cjump, but no cjump data here)
	   irec2=ioffset + 7*nval(1) + 24 + (ifmax(1)-ifmin(1)+1)*4 +
     &	(ifmax(2)-ifmin(2)+1)*4 + (ifmax(3)-ifmin(3)+1)*4
	   nfits=nfitsav(1)	!values read and saved above
	   read(unit=15,rec=irec2)
     &     (timsav(i),i=1,nfits),(ifits(i),i=1,nfits),
     &     (ibaselin(i),i=1,nfits)
	   CLOSE(unit=15)
c	else if(iscan.q.-103) then	!simulated
c      timsav etc don't exist for simulated data
	endif
c Must add on the rest of the values, if nfile>1
	nfit=nfits
	if(nfile.gt.1) then
	   do i=2,nfile
		OPEN(unit=15,file=scdatfil(i),status='UNKNOWN',
     &		access='TRANSPARENT')
		read(unit=15,rec=1) iscanver,ioffset,kt
		nfit=nfit+nfitsav(i)		!add current value
		j1=nfit
		j2=nfit+nfitsav(i)
		nfit=j2
		if(iscan.ne.-103) then
		   read(unit=15,rec=ioffset+7*nval(i)) ifmin,ifmax
		   irec2=ioffset + 7*nval(i) + 24 + (ifmax(1)-ifmin(1)+1)*4+
     &		(ifmax(2)-ifmin(2)+1)*4 + (ifmax(3)-ifmin(3)+1)*4
		   nfits=nfitsav(i)	!values read and saved above
		   read(unit=15,rec=irec2)
     &  	    (timsav(j),j=j1,j2),(ifits(j),j=j1,j2),
     &	    (ibaselin(j),j=j1,j2)
		endif
		CLOSE(unit=15)
	   enddo
	   nfits=nfit
	   nfmax=nfm
	endif
	goto 62		!skip read of scandat files
c
61	continue
c
c READ for SCANDAT.DAT
c 1st and last transition # for each burst now in ibstrt(), ibend().
c Now convert them to absolute index in CONSAM of these transitions.
c For conversion need to read timsav, ifits etc from SCANTEMP
c UNFORTUNATELY MUST READ AS FAR AS DFINTER WHICH IS ESSENTIAL! -just
c read the relevant bytes?  All values are *4 up to dfinter, tlast (which
c are real*8) so ith value starts at 1+4*(i-1), EXCEPT for ndev*2,
c cdate*11, adctime*8, filtfile*20.  DFinter is 63rd value=byte 249+
c 2+11+8+20-4*4=byte 274
c
c	   read(unit=13,rec=1) iscanver,kt,nfits,ntmax,nfmax,ioffset,     6
c byte #=                        1    5   9     13
c     &   nbuf,novlap,srate,finter,                                     10
c     &   tsect,ioff,ndat,nsec,nrlast,avtot,navamp,avamp,rms,           19
c     &   nwrit,nwsav,newpar,opendown,invert,                           24
c     &   usepots,disp,smult,scrit,vary,ntrig,navtest,dgain,iboff,      33
c     &   expfac,bdisp,ibflag,iautosub,xtrig,ndev,cdate,adctime,        41
c     &   nsetup,filtfile,ffilt,npfilt,                                 45
c     &   sfac1,sfac2,sfac3,nscale,calfac,calfac1,calfac2,iyoff,        53
c     &   ioff1,disptran,dispderiv,dispguess,ampfac,tmin,               59
c     &   tsfac,tlfac,sdone,dfinter,tlast,                              tsfac=60
c     &   shut,shutprev,backward,prevlevel,t0sav,y0sav,vard,nshutfit,
c     &   infit,infirst,ixfprev,idiskq,ifirst,base,basevga,ibasevga,
c     &   itrig,itrigvga,itriglev,inc,incabs,
c     &   indfst,isdfst,isec,ndisp,ndisp1,
c     &   idatyp,adcfil,cdate1,nchan,
c     &   iscanver,tcfac,facjump,shutsav,goback,
c     &   imin,imax,errfac,derivfac,confac,
c     &   cjump,ijump,njdim,tzerod,intzero,tsample,ktjump,njfit,
c     &   njump,nnull,ipatch,Emem,ktlast,
c     &   izoom,fcz,fczoom,ampz,avampsav
c	INQUIRE(file=scantfil,exist=present,flen=len)
c NB may have more that one scantemp! -one for each expt on scandat!
c Find total number of fits (>nfmax if more than one file)
	nfm=0
	do i=1,nfile
	   OPEN(unit=13,file=SCANTFIL(i),status='UNKNOWN',
     &     access='TRANSPARENT')
	   read(unit=13,rec=1) iscanver,kt,nfits,ntmax,nfmax
	   nfm=nfm+nfmax
	   CLOSE(unit=13)
	enddo
	if(iscanver.ge.102) then
	   ALLOCATE(timsav(nfm),ifits(nfm),ibaselin(nfm))
	else
	   ALLOCATE(timsav(nfm),ifits(nfm),ifits2(nfm),ibaselin(nfm))
	endif
c
	OPEN(unit=13,file=SCANTFIL(1),status='UNKNOWN',
     &     access='TRANSPARENT')
c read up to ioffset only, then dfinter separately
c Allocate and read timsav,ifits,nfits
c No need to read tint, iampl, iprops again!
c tint=4*kt bytes, iampl=2*kt bytes, iprops=kt bytes, total=7*kt bytes
	read(unit=13,rec=1) iscanver,kt,nfits,ntmax,nfmax,ioffset
	read(unit=13,rec=274) dfinter
c Allocate and read timsav,ifits,nfits
c No need to read tint, iampl, iprops again!
c tint=4*kt bytes, iampl=2*kt bytes, iprops=kt bytes, total=7*kt bytes
c	read(unit=13,rec=ioffset)
c     &   (tint(i),i=1,kt),(iampl(i),i=1,kt),(iprops(i),i=1,kt),
c     &   (timsav(i),i=1,nfits),(ifits(i),i=1,nfits),
c     &   (ibaselin(i),i=1,nfits),
	if(iscanver.ge.102) then
	   read(unit=13,rec=ioffset+7*kt)
     &   (timsav(i),i=1,nfits),(ifits(i),i=1,nfits),
     &   (ibaselin(i),i=1,nfits)
	else
	   read(unit=13,rec=ioffset+7*kt)
     &   (timsav(i),i=1,nfits),(ifits2(i),i=1,nfits),
     &   (ibaselin(i),i=1,nfits)
c  convert old integer*2 ifits() to integer*4
	   do i=1,nfits
		j=int4(ifits2(i))
	      if(j.le.-1.and.j.ge.-32768) then
	         j=j+65536
	   	else if(j.lt.-32768) then
	         print*,' IFITS2() IS TOO BIG!!'
	         STOP
	      endif
	   	ifits(i)=j
	   enddo
	endif
	if(iscanver.lt.102) DEALLOCATE(ifits2)
	CLOSE(unit=13)
c Must add on the rest of the values, if nfile>1
	nfit=nfits
	if(nfile.gt.1) then
	   do i=2,nfile
		OPEN(unit=13,file=SCANTFIL(i),status='UNKNOWN',
     &     access='TRANSPARENT')
		read(unit=13,rec=1) iscanver,kt,nfits,ntmax,nfmax,ioffset
		nfit=nfit+nfits		!add current value
		j1=nfit
		j2=nfit+nfits
		nfit=j2
		if(iscanver.ge.102) then
		   read(unit=13,rec=ioffset+7*kt)
     &  	    (timsav(i1),i1=j1,j2),(ifits(i1),i1=j1,j2),
     &	    (ibaselin(i1),i1=j1,j2)
		else
		   read(unit=13,rec=ioffset+7*kt)
     &	   (timsav(i1),i1=j1,j2),(ifits2(i1),i1=j1,j2),
     &   	   (ibaselin(i1),i1=j1,j2)
c  convert old integer*2 ifits() to integer*4
		   do i1=j1,j2
			j=int4(ifits2(i1))
	      	if(j.le.-1.and.j.ge.-32768) then
		         j=j+65536
		   	else if(j.lt.-32768) then
		         print*,' IFITS2() IS TOO BIG!!'
		         STOP
		      endif
		   	ifits(i1)=j
		   enddo
		endif
		CLOSE(unit=13)
	   enddo
	   nfits=nfit
	   nfmax=nfm
	endif
c
c
62	continue 		!skip to here after reading scan.scn data
c Now we have dfinter, calculate sample sizes
	fint=sngl(dfinter/1.d3)		!ms between points
	nsamp1=ifixr(avsamp/fint)	!number of points for the burst
	nsamp=ifixr((avsamp+tpre)/fint)	!total number to be displayed
	nsamp0=nsamp-nsamp1		!number before t=0
c
c Get absolute index in these arrays NB this requires tint0(), NOT tint()
c Need to add points equivalent to two rise-times at end to ensure the
c trace is right back to baseline -number of points to be added is
	   trise=332.1/fc		!rise time (microsec)
	nend=2*ifixr(trise/sngl(dfinter))
c
	iflast=1		!start at first fit
	do i=1,nburst
c For start of burst # i
	   itran=index(ibstrt(i))	!index of trans before resolution imposed
	   call GETINDEX(itran,i0,if1,iflast,ifits,nfits,ival,nfmax)
c returns if1=fit # preceding tint(k), i0=index in tint() of first interval
c in this fit; and ival=1 if fit #if1 starts
c with tint(k), ival=2 if tint(k) is 2nd interval of fit #if1 etc
c=	   ibline(i)=ibaselin(if1)	!integer*2 baseline at start of burst
	   indfst(i)=sngl(timsav(if1)/dfinter)	!index for 1st trans of fit
	   if(ival.gt.1) then      !add intervals that follow first in fit
		j1=0 		!correction for index of tint()
		dt=0.0
		do j=i0,i0+ival-2	!adds one interval if ival=2 etc
		   dt=dt+tint0(j+j1)
		enddo
		indfst(i)=indfst(i)+ifixr(dt/fint)
	   endif
	   iflast=if1		!start searching at fit #iflast next cycle
c Repeat for end of burst # i
	   itran=index(ibend(i))	!index of trans before resolution imposed
	   call GETINDEX(itran,i0,if1,iflast,ifits,nfits,ival,nfmax)
c returns if1=fit # preceding tint(k), i0=index in tint() of first interval
c in this fit; and ival=1 if fit #if1 starts
c with tint(k), ival=2 if tint(k) is 2nd interval of fit #if1 etc
	   indend(i)=sngl(timsav(if1)/dfinter)	!index for 1st trans of fit
	   if(ival.gt.1) then      !add intervals that follow first in fit
		j1=0 		!correction for index of tint()
		dt=0.0
		do j=i0,i0+ival-2	!adds one interval if ival=2 etc
		   dt=dt+tint0(j+j1)
		enddo
		indend(i)=indend(i)+ifixr(dt/fint)
	   endif
	   indend(i)=indend(i) + nend		!add extra points
	   iflast=if1		!start searching at fit #iflast next cycle
	enddo
	DEALLOCATE(ibstrt,ibend)	!now in main prog
	DEALLOCATE(tint0,ampl0,iprops0)
	DEALLOCATE(tint,ampl,iprops,index)
	DEALLOCATE(timsav,ifits,ibaselin)
c Need only indfst,indend for the rest of program
	RETURN
	END

