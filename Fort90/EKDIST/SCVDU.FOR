	subroutine SCVDU(xaxis,xmin,xmax,freq,nbin,fmin,fmax,flo,fhi,
     & fitted,prefit,ahist,pahist,obhist,thist,tres,xtic,ftic,idiskq,
     & idest,ohist,ghist,bhist,pophist,jth,iydat,revamp,
     & logt,first,titlex,titley,xwbase,xw,ilabel,ncalc,xcalc,fcalc,
     & obfreq,frqcal,theta,ncomp,ntx,nty,itx,ity,idtype,
     & amean,area,sd,antrue,isg,gamdst,isdcon,lastfit,
     & nfitdone,thetsav,ksav,idsav,ncompsav,nfitsav,ylosav,yhisav,
     & isgsav,fitsav,mono,ndimd,ndimc,
     & xval,yval,xcal,ycal,ndat,icurvd,ijoin,ncal,icurvc,iline,
     & imin,imax,ifreq1,ifreq2,ifreq3,ifmin,ifmax,ybar,
     & excreg,yexclow,yexchigh,anfit,nsweep,nnull,nfitt,
     & cjump,nth,ipop,ibmin,bmin,noutlo,nouthi,
     & jset,nset,neq,itcon,
     & yval0,nyval)
c
c Display for EKDIST
c
c Modified 01/24/03 07:13am to increase size of ameant() areat() from 10 to 100
c
c Modif 10/22/02 06:19pm by adding yval0 (=yval() in main prog) to arguments
c so raw data for distribution can be transmitted to RANCOMP to do
c randomistaion tests (when nset>1)

c Now (03/03/94 05:42pm)
c (1) includes code formerly in SCVD1
c (2) returns xval,yval to main prog for display after fitting without recalc
c (3) If point amp histo is to be fitted, define freqs() here, to contain
c	the relevant ifreq() values for fitting (lower and upper limits for
c	fitting are defined in MLFITS as for other distributions). Values
c	for fitting are in common/PABLK/ for communication to GAUFIT
c	(so cannot use allocatable array for freqs()).
c (4) calfac2 now in common/pablk/ for GAUFIT
c
c 05/28/89 10:54pm Lahey version
c Modif 11/14/97 02:09pm for VHIST5 (can allocate xval etc to any
c  size now, but still fixed at old values for now)
c Modif 02/07/95 03:51pm to take jump data from SCAN
c Modif 12/06/91 05:43pm for 1st latency dists
c Modif 03/03/94 03:21pm for point amplitude display
c    IJOIN(j) gives line type for main histo bars: for Hgraph types 0-8
c
c		0=solid line
c		1=dotted line
c		2-6=dashed line with increasing dash length
c		7=dash-dot
c		8=dash-dot-dot
c
c	ILINE(j) defines line type for the jth calculated curve, via ZSEQ
c         (prev numbered 1-5, but now make 0-4 so 0=continuous as in Hgraph)
c		0=continuous line
c		1=long dash
c		2=short dash
c		3=long,short dash
c		4=long,short,short dash
c
C Version of SCVDU for EKDIS3
C REDUCED VERSION to be used with SCVD1 and SCVD2 to reduce size
c of graphics overlay Apr 1987
c Apr87- modif for log(t) display
c DC version for 11-73. Aug 1986.
C SUBROUTINE FOR EKDIST TO DO ALL DISPLAY (AS SCVDU BUT
C SUBLEV IS PARAM: NEEDED FOR WRITE TO DISK)
C LAST ROW OF PARAM IS ONLY FOR COMMUNICATION TO PLOTING DISC FILE
C	IDEST IS INTEGER VAR TO SPECIFY DESTINATION AFTER DISPLAY
C	ON ENTRY IDEST=31 FOR DISPLAY OF GAUSSIAN-TRANSFORMED TIMES
C
c  Raw data for distribution =YVAL in main prog. for use in randomisation test
	real*4 YVAL0(81920,10)
	integer nyval(10)
c
	real XVAL(0:511,ndimd),YVAL(0:511,ndimd)
	real XCAL(2048,ndimc),YCAL(2048,ndimc)
c for histo data
	integer ndat(ndimd),icurvd(ndimd),ijoin(ndimd)
	logical histo
c For Vplot
	ALLOCATABLE:: xval1,yval1
	real*4 xval1(:,:),yval1(:,:)
	integer isym(ndimd)	!for vplot
	real*4 syms(ndimd)	!for vplot
c for calc curves
	integer ncal(ndimc),icurvc(ndimc),iline(ndimc)
c	real XVAL(0:511,3),YVAL(0:511,3)
c	real XCAL(2048,5),YCAL(2048,5)	!2048 for VHIST3
c for histo data
c	integer ndat(3),icurvd(3),ijoin(3)
c for calc curves
c	integer ncal(5),icurvc(5),iline(5)
	ALLOCATABLE:: colhead,data
	real*4 data(:,:)
	character*20 colhead(:)
c
c declarations formerly in SCVD1
	character word*11,num*4,text*25,text1*40
	character*66 text2
c
	real theta(50),theta1(50)
	real thetsav(50,5),ylosav(5),yhisav(5)	!to save past fits
	integer ksav(5),idsav(5),ncompsav(5),nfitsav(5),isgsav(5) !to save past fits
	character*12 fitsav(5)			!to save past fits
	dimension XAXIS(510),FREQ(510)
	dimension xcalc(2048),fcalc(2048)
	dimension obfreq(510),frqcal(510)
	character*40 titlex,titley
	character*64 title1
	character*79 note
	character*1 ans
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	logical ahist,pahist,obhist,thist,ohist,ghist,bhist,excreg
	logical pophist	!in common -true for fittting Gaussian to Popen/bst
	logical fitted,prefit
	logical logt,first,debug,caplock,gamdst,mono,revamp
	logical ahist1,obhist1,thist1,gamdst1
	logical landscap,autplt,draft,doframe,lastfit,allocated
	logical ivplot,interp		!for vplot
	logical sbin,shist,sres,sexp
	logical discprt
c For vplot
	ALLOCATABLE:: weight,icurvw
	real*4 weight(:,:)
	integer*4 icurvw(:)
c For point amplitude histos
	integer*4 ifreq1(imin:imax),ifreq2(imin:imax),ifreq3(imin:imax)
	integer*4 ifmin(4),ifmax(4)
	real*4 freqs(-5000:5000)		!in common/pablk/ for GAUFIT
c For Patlak
c=	real*4 Yfreq(-2047:2048)		!(use freqs() for Patlak)
c For cjumps
	logical cjump
	logical student,cluster,iwindows
	character cnum*11
c For popmenu
	character*35 strings(10)
	character*55 helps(15)	!help strings
	character*20 title 			!for popmenu
	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls
c
c For nset>1
	integer ncomp(10)
	real*4 yexclow(10),yexchigh(10)
	real amean(10,10),area(10,10),sd(10,10)	!for CALDIS
	real amean1(10,10),area1(10,10),sd1(10,10)	!for CALDIS
	real*4 antrue(10)
	real*4 antrue1(10)
	integer itcon(10,10)
c To communicate values to partext.for
	real*4 sdt(10)
	common/ptext/ameant(100),areat(100)	   !in partext, scvdu, vhsq5, rdvhsq
c
	common/adj/iadj		!for scvdu/cdist1
c
	common/dp/discprt
	common/sblk/sbin,shist,sres,sexp
	common/PABLK/freqs,calfac2,ilow,ihigh		!for GAUFIT,MLFITS
	common/user/student,cluster,iwindows
c
	debug()=caplock()
c
101	format(a1)
108	FORMAT(/)
c
c Modif 11/14/97 02:09pm for VHIST5 (can allocate xval etc to any
c  size now, but still fixed at old values for now)
	ndv1=511	!dimension for xval,yval
	ndc1=2048	!dimension for xcal, ycal
	kmax=50	!dimension for theta, jfix
c
c Values for POPMENU calls
c	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls
	nxlo=100
	nylo=-1
	nyhi=470
	ictm=15		!text white
	ibkm=8		!background dark grey
	icfm=2		!frame/title green
	icupm=12		!upper case red
c
c Code previously in SCVD1
	ID=IDTYPE
	ilabel=1
	titley='Number '
	if(ahist.and.id.eq.7) titley='Number x ms'
	if(obhist.and.logt) titley='r x frequency(r)'
	xw=xwbase  !so Y axis labelled 'per 10 ms' (reset below as nec)
	if(logt) xw=0.	!not so labelled for log-bins
	if(id.eq.14) xw=0.
	if(id.eq.52) xw=0.      !Popen from YVAL.DAT
	if(id.eq.124.or.id.eq.125) xw=0.      !Popen per jump
	if(iydat.eq.3) goto 602		!titley already set for YVAL.DAT data
c
	if(isg.ge.0) then
	   if(isg.eq.0) then
		text=' bsts with '//char(isg+1+48)//' opening'
	   else
		text=' bsts with '//char(isg+1+48)//' openings'
	   endif
	else if(isg.lt.0) then
	   text='bursts with more than '//char(-isg+1+48)//' openings'
	endif
c NB Char(242)=equal to or greater than sign
c
	if(idest.eq.31) then
	   titlex='Transformed value '
	   xw=0.		!so Y axis NOT labelled 'per 10 ms'
	   goto 602
	endif
	if(ghist) then
	   titlex='Shut time (ms) '
	   if(cjump) then
		if(id.eq.101) then
		   titlex='Latency to the first opening'
		else if(id.eq.102) then
		   if(nth.eq.1) then
			cnum='1st       '
		   else if(nth.eq.2) then
			cnum='2nd       '
		   else if(nth.eq.3) then
			cnum='3rd       '
		   else
		      call INTCONV(nth,cnum)
		      n=NBLANK(cnum)
			cnum=cnum(1:n)//'th'
		   endif
		   n=NBLANK(cnum)
		   titlex='Length of '//cnum(1:n)//' shut time after t=0'
		else if(id.eq.103) then
		   titlex='Length of all shut times after jump'
		else if(id.eq.105) then
		   titlex='Length of last shut times after jump'
		endif
	   endif
	else if(ahist) then
	   if(id.eq.1) then
		titlex='Amplitude (pA) '
	   else if(id.eq.2) then
		titlex='Amplitude (pA): all point'
	   else if(id.eq.3) then
		titlex='Amplitude (pA): shut point'
	   else if(id.eq.4) then
		titlex='Amplitude (pA): open point'
	   else if(id.eq.5) then
		titlex='Amplitude (pA): open/shut point'
	   else if(id.eq.6) then
		titlex='Largest amp in open period (pA)'
	   else if(id.eq.7) then
c		titlex='Amplitude (pA): idealized open point'
		titlex='Amplitude (pA): (length-weighted)'
	   else if(id.eq.8) then
		titlex='Amplitude (pA): (low-variance)'
	   else if(id.eq.12.or.id.eq.13) then
		if(isg.eq.-10000) then
		   titlex='Mean Burst Amplitude (pA)'
		else
		   call GBLANK(text,25,nl1,nl2)
		   titlex='Mean Burst Amp: '//text(nl1:nl2)
		endif
	   endif
	   if(revamp) titlex='-'//titlex(1:39)	!neg sign
	   xw=-xwbase	!so Y axis labelled 'per 10 pA')
	else if(ohist) then
	   if(id.eq.1) titlex='  Open time (ms) '
	   if(id.eq.2) titlex='Open period (ms) '
	   if(id.eq.3) titlex='Open time (adj to spec gap) '
	   if(id.eq.4) titlex='Open period (adj to spec gap) '
	   if(id.eq.5) then
		if(iadj.eq.1) then
		   titlex='Open periods with spec gap on BOTH sides'
		else if(iadj.eq.2) then
		   titlex='Open periods EXCLUDE spec gap BOTH sides'
		endif
	   endif
	   if(cjump) then
		if(id.eq.112) then
		   titlex='Length of all open periods (ms)'
		else if(id.eq.114) then
		   titlex='Number of open periods per jump'
		else if(id.eq.111) then
		   if(nth.eq.1) then
			cnum='1st       '
		   else if(nth.eq.2) then
			cnum='2nd       '
		   else if(nth.eq.3) then
			cnum='3rd       '
		   else
		      call INTCONV(nth,cnum)
		      n=NBLANK(cnum)
			cnum=cnum(1:n)//'th'
		   endif
		   n=NBLANK(cnum)
		   titlex='Length of '//cnum(1:n)//' open period after t=0'
		else if(id.eq.117) then
		   titlex='Length of last open period (ms)'
		endif
	   endif
	else if(obhist) then
	   if(id.eq.1) titlex='    Openings/burst '
	   if(id.eq.2) titlex='Open periods/burst '
	   xw=0.		!so Y axis NOT labelled 'per 10 ms'
	   if(cjump) then
		if(id.eq.114) then
		   titlex='Number of open periods per jump'
		else if(id.eq.133) then
		   titlex='Number of open periods per burst'
		else if(id.eq.134) then
		   titlex='Number of bursts per jump'
		endif
	   endif
	else if(.not.bhist) then
	   print *,' ERROR IN SCVDU'	!should not reach here!
	   goto 602
	endif
c Rest is for BURST histograms (down to 602)
	if(.not.bhist) goto 602
	if(id.eq.3) titlex='     Burst length (ms)    '
	if(id.eq.4) titlex='  Burst length (exc sub)  '
	if(id.eq.5) titlex='     Open time/burst      '
	if(id.eq.6) titlex='Open time/burst (exc sub) '
c cjump distributions
	if(id.eq.121) titlex='Activation length (including latency)'
	if(id.eq.122) titlex='Activation length (excluding latency)'
	if(id.eq.123) titlex='Total open time per activation'
	if(id.eq.124) titlex='P(open) in activation (exc latency)'
	if(id.eq.125) titlex='P(open) in activation (inc latency)'
	if(id.eq.131) titlex='Burst length (ms)'
	if(id.eq.132) titlex='Open time per burst (ms)'
	if(debug()) then
	   print 888,isg,jth
888	   format(' isg,jth = ',2i8)
	   pause
	endif
	if(id.eq.7.or.id.eq.8) then
	   call GBLANK(text,25,nl1,nl2)
	   if(id.eq.7) titlex='   Burst length: '//text(nl1:nl2)
	   if(id.eq.8) titlex='Open time/burst: '//text(nl1:nl2)
	endif
	if(id.eq.9.or.id.eq.10) then
	   if(id.eq.9) word='gap'
	   if(id.eq.10) word='open period'
	   call GBLANK(word,11,nw1,nw2)
	   ij=iabs(jth)
	   if(ij.eq.1) then
		num='1st '
	   else if(ij.eq.2) then
		num='2nd '
	   else if(ij.eq.3) then
		num='3rd '
	   else if(ij.ge.4) then
		num=char(ij+48)//'th '
	   endif
	   if(isg.eq.-10000) then	!ops/bst not specified
	      if(jth.ge.1) then
		   text1=num//word(nw1:nw2)//' in any burst'
	      else if(jth.le.-1) then
		   if(num.eq.'1st ') then
		      text1= 'Any '//word(nw1:nw2)//' in any burst'
		   else
		      text1='more than ' //num//word(nw1:nw2)//' in any burst'
		   endif
	      endif
		titlex=text1
c NB Char(242)=equal to or greater than sign
	   else if(isg.ne.-10000) then	!ops/bst specified
	      if(jth.ge.1) then
		   text1=num//word(nw1:nw2)//' in burst'
	      else if(jth.le.-1) then
		   if(num.eq.'1st ') then
		      text1= 'Any '//word(nw1:nw2)//' in burst'
		   else
		      text1='more than '//num//word(nw1:nw2)//' in burst'
		   endif
	      endif
		if(isg.ge.0) then
		   if(isg.eq.0) then
		      text=' with '//char(isg+1+48)//' opening'
		   else
		      text=' with '//char(isg+1+48)//' openings'
		   endif
	      else if(isg.lt.0) then
		   text='with '//'more than '//char(-isg+1+48)//' openings'
	      endif
	      call GBLANK(text1,40,nw1,nw2)
	      call GBLANK(text,25,nl1,nl2)
	      text2=text1(nw1:nw2)//' '//text(nl1:nl2)
	      call LBLANK(text2,66)		!move text to start of array
	      titlex=text2(1:40)
	   endif
	endif
c	if(id.eq.9) titlex='    Jth shut time (ms)    '
c	if(id.eq.10) titlex='   Jth open time (ms)    '
	if(id.eq.16) titlex='Last open period in burst (ms)'
	if(id.eq.17) titlex='Last gap in burst (ms)'
602	continue
	if(pophist) then
	   if(ipop.eq.0) then
		titlex='P(open)/burst'
	   else if(ipop.eq.1) then
		titlex='P(open)/burst (exc last op)'
	   endif
	   if(ibmin.gt.0) then
		call INTCONV(ibmin,cnum)
		n=NBLANK(cnum)
		titlex=charnb(titlex)//':'//cnum(1:n)//' ops or more'
	   else if(bmin.gt.0.0001) then
		call DCFORMAT(bmin,7,2,cnum)
		n=NBLANK(cnum)
		titlex=charnb(titlex)//': >'//cnum(1:n)//' ms'
	   endif
	endif
c
	if(idest.ne.31) idest=0
c
c For log display superimpose a dashed exponential if there is no fitted
c curve to show the shape of the dist of log(t)
c Separate calc need for log geometric which is diff shape for small means

	if(fitted.or.prefit) goto 210		!skip calc exponential
	ncalc=0
	if(.not.logt) goto 210			!ditto
c
c NB for dist of log(t) the values of XMIN,XMAX are in log10 units
c so dxcalc=log10(factor)
	if(thist) then
	   NCALC=2048
	   xm=xaxis(1)
	   xmax=xaxis(nbin+1)	!temp- reset in SCVDU
	   fmax=1.2*fmax		!temp- reset in SCVDU
	   tau=exp1((alog(xm)+alog(xmax))/2.)	!half way along log scale
	   scal=(fmax/1.3)*tau/(exp1(alog(tau)-1.))
	   DXCALC=(alog10(XMAX)-alog10(XM))/FLOAT(NCALC-1)  !increment
	   tau=exp1((alog(xm)+alog(xmax))/2.)	!half way along log scale
	   scal=(fmax/1.3)*tau/(exp1(alog(tau)-1.))
	   do i=1,ncalc
		X=alog10(XM)+FLOAT(I-1)*DXCALC	!this is log10 value
		x=10.**x		!log10 taken in VHIST
		xcalc(i)=x
		f=scal*(1./tau)*exp1(-x/tau)
c For log(t) distn xwbase defined in CDIST as bin width in log10 units
		f=f*x*2.30259		!f(log10(t))=2.3*t*f(t)
		FCALC(I)=XWBASE*F
	   enddo
	else if(obhist) then
	   ncalc=nbin
	   xm=1.0
	   xmax=float(nbin)	!temp- reset in SCVDU
	   fmax=1.2*fmax		!temp- reset in SCVDU
	   dxcalc=1.0		!increment
	   tau=ybar 	!overall mean from setbin1
	   scal=(fmax/1.3)*tau/(exp1(alog(tau)-1.))/2.30259
	   do i=1,nbin
		r=float(i)
		xcalc(i)=r
		f=scal*(1./tau)*(1.0 - 1.0/tau)**(r-1.0)	!single geo dist
c For log(t) distn xwbase defined in CDIST as bin width in log10 units
c		fcalc(i)=f*r*2.30259
		fcalc(i)=f*r
	   enddo
	endif
c
210	continue
c end of SCVD1 code
c
c Now display
	ict=11	!text colour for DIALOG box
	if(mono) ict=15
	autplt=.false.		!for VHIST
	draft=.false.		!for VHIST
	doframe=.true.		!for VHIST
	landscap=.true.		!for VHIST
	cbig=2.5
	ifont=4
	isval=0	!no arrow
	xcross=0.0
	ycross=0.0
	inumx=-1	!X axis in fixed format if log scale
c	if(nset.gt.1.and.(.not.fitted)) then
	if(nset.gt.1) then
	   itit=1
         call INTCONV(jset,cnum)
	   title1='Set #'//charnb(cnum)
	endif
c	ndimd=3	!now in main
c	ndimc=5
	iask=2	!do not ask before leaving display; leave graph on screen
c IFITYPE=0 for no display of param
c IFITYPE=1 for time histos (THIST)
c IFITYPE=-1 for time histos (THIST) when nset>1 in new ekdist
c IFITYPE=2 for op/bst histos (OBHIST)
c IFITYPE=-2 for op/bst histos (OBHIST) when nset>1 in new ekdist
c IFITYPE=3 for amp  histos (AHIST)
c IFITYPE=31 for amp  histos (AHIST) if SD are proportional to mean
c IFITYPE=32 for amp  histos (AHIST) if SD are constrained to be equal
c IFITYPE=33, 34, 35=above 3 for Gaussian fit to Popen/bst pdf
	ifitype=0
      if((.not.fitted).and.lastfit) then
	   ans='N'
	   call DCASK(
     &   'Plot the last fitted curve on this histogram',ans,ans)
         if(ans.eq.'Y') fitted=.true.
      endif
	if(fitted.or.prefit) then
	   if(thist) then
		if(neq.eq.0) then
		   ifitype=1
		else
		   ifitype=-1
		endif
	   else if(obhist) then
		if(neq.eq.0) then
		   ifitype=2
		else
		   ifitype=-2
		endif
	   else if(ahist) then
		ifitype=3
		if(isdcon.eq.2) ifitype=31	!SD proportional to mean
		if(isdcon.eq.3) ifitype=32	!SD same for all
	   else if(pophist) then
		ifitype=33
		if(isdcon.eq.2) ifitype=34
		if(isdcon.eq.3) ifitype=35
	   endif
	endif
c	pause
	call flush(7)
	first=.false.
c
	if(idest.ne.31) idest=0
	lt2=2       !short dash for lo,hi bins
	ilog=0
c	if(logt) ilog=1		!plot vs log(t)
	if(logt) ilog=6		!plot sqrt(y) vs log(t)
	iscal=0			!use input xmin,xmax etc
	xlo=-1.
c
	do i=1,5
	   icurvc(i)=i	!in case needed
	   iline(i)=0	!continuous
	enddo
c
c Set up values for point amplitude histos separately
c NB call with fmin,fmax,ftic (not ymin,ymax,ytic)
	if(pahist) then
	   if(fitted.or.prefit) goto 110		!XMIN etc already fixed
	   call POINTAMP(imin,imax,ifreq1,ifreq2,ifreq3,calfac2,
     &	revamp,nbin,xval,yval,xmin,xmax,fmin,fmax,xtic,ftic,
     &	ifmin,ifmax,xwbase,idtype,ndimd)
	   logt=.false.
	   ncurvd=1
	   icurvd(1)=1
	   ndat(1)=nbin
	   ijoin(1)=0
	   goto 30
	endif
c Patlak plot also needs special subroutine to process freqs (defined in cdist1)
208	if(ahist.and.id.eq.8) then
	   if(fitted.or.prefit) goto 110		!XMIN etc already fixed
c NB call with fmin,fmax,ftic (not ymin,ymax,ytic)
	   call PATPLOT(freqs,noutlo,nouthi,
     &     revamp,nbin,xval,yval,xmin,xmax,fmin,fmax,xtic,ftic,
     &     i1min,i1max,xwbase,idtype,ndimd)
	   logt=.false.
	   ncurvd=1
	   icurvd(1)=1
	   ndat(1)=nbin
	   ijoin(1)=0
	   goto 30
	endif
c And log display of geometric need special section here because it uses
c VPLOT rather then VHIST.  The 'sample' curve, displayed before fitting,
c is

	if(obhist.and.logt) then
	   if(fitted.or.prefit) goto 110		!XMIN etc already fixed
	   ALLOCATE(xval1(ndv1,ndimd),yval1(ndv1,ndimd))
	   ncurvd=1
	   icurvd(1)=1
	   syms(1)=2.5
	   isym(1)=-3	!square
c	   ijoin(1)=0
	   ijoin(1)=-1	!not joined
	   ndat(1)=nbin
	   do i=1,nbin
		xval1(i,1)=float(i)
	   enddo
	   do i=1,nbin
		yval1(i,1)=float(i)*freq(i)	!N*r*P(r)=r*freq(r)
	   enddo
	   if(ncalc.gt.0) then	!show shape of curve
		ncurvc=1		!copy first calc curve here
		ncal(1)=ncalc
		icurvc(1)=1
		iline(1)=2		!so calc curve is dashed
		do i=1,ncal(1)
		   xcal(i,1)=xcalc(i)
		   ycal(i,1)=fcalc(i)
		enddo
	   endif
	   ilog=6 		!sqrt(y) vs log(x)
	   ilabel=1		!default pos for axis labels
	   titley='r.P(r)'	!titlex already defined
	   iask=-2
	   ncjump=0
	   nvjump=0
	   autplt=.false.
	   fitted=.false.
	   draft=.false.
	   interp=.false.
	   ivplot=.false.
	   doframe=.true.
	   landscap=.true.
	   xlo1=-1		!whole screen
	   ntx=5
	   nty=5
	   itx=1		!ticks orientated normally
	   ity=1
	   itrace=0
	   isetcol=0
	   kwi=1
	   kwj=1
	   ALLOCATE(icurvw(ndimd),weight(kwi,kwj))
	   do i=1,ndimd
		icurvw(i)=-1	!no SD
	   enddo
	   iscal=1	!scale internally 1st time
	   iask=2	!do not ask before leaving display; leave graph on screen
	   ncomp1=ncomp(jset)
	   histo=.false.
	   call VPLOT5(XVAL1,YVAL1,NDAT,icurvd,ncurvd,ijoin,syms,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO1,XHI1,YLO1,YHI1,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,draft,itit,title1,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp1,interp,
     & isetcol,itrace,ndv1,ndc1,weight,kwi,kwj,icurvw,kmax,iver)
	   if(allocated(icurvw)) then
		DEALLOCATE(icurvw,weight)
	   endif
	   DEALLOCATE(xval1,yval1)
	   goto 201
	endif		!end of log display of geometric
c
	if(nset.eq.1) then
	   if(fitted.or.prefit) goto 110		!XMIN etc already fixed
	endif
c When nset>1 do next bit even when fitted to restore xval, yval
c etc from xaxis, freq
c
c Now fix lo,hi bins etc and define xmin,fmin etc (all here rather than
c in SCVD1 now)
c
c histos:
	ncurvd=1
	icurvd(1)=1
	ndat(1)=nbin
	ijoin(1)=0
	do i=1,nbin+1
	  xval(i,1)=xaxis(i)
	enddo
	do i=1,nbin
	  yval(i,1)=freq(i)
	enddo
c
c If nset>1 then get to here when fitted, to restore correct xval, yval
c but if fitted now jump to 110 for fitted curve -don't change xmin, xmax etd
c but first set low and high bins as below
	if(nset.gt.1.and.fitted) then
	   yval(0,1)=flo		!set whether 0 or not- it is checked in VHIST
	   xval(0,1)=xval(1,1)	!if no flo- it is checked in VHIST (in MINMAX)
	   xval(nbin+2,1)=xval(nbin+1,1)		!ditto if no fhi bin
	   if(flo.gt.0.0001) then
		xval(0,1)=xval(1,1)-xwbase
		if(logt) xval(0,1)=xval(1,1)*(xval(1,1)/xval(2,1))
		xmin=xval(0,1)
	   endif
	   yval(nbin+1,1)=fhi
	   if(fhi.gt.0.0001) then
		xval(nbin+2,1)=xval(nbin+1,1)+xwbase
		if(logt) xval(nbin+2,1)=xval(nbin+1,1)*
     &	(xval(nbin+1,1)/xval(nbin,1))
c	  keep ratio for hi bin as for other bins, so same width on log scale
		xval(nbin+2,1)=xval(nbin+1,1)*(xval(nbin+1,1)/xval(nbin,1))
c==	   	xmax=xval(nbin+2,1)	!don't change xmamx?
	   endif
	   goto 110
	endif
c
	fmin=0.
	xmin=0.
	if(logt) xmin=xaxis(1)		!if no lo bin
	if(ahist.or.idest.eq.31) xmin=xaxis(1)
	xmax=xaxis(nbin+1)	!=xval(nbin+1,1), if no hi bin
c Round up xmax, but not if it is already an 'integer')
	if(abs(amod(xmax,1.0)).gt.1.e-4) xmax=float(1+ifix(xmax))
	if(.not.ahist) xmax=float(1+ifix(1.2*xmax))	!ROUND UP
	fmax=fmax*1.2     !value from SETBIN
	fmax=float(1+ifix(fmax))	!ROUND UP
	ftic=0.5*10.0**ifix(alog10((fmax-fmin)*0.5))
2102	if((fmax-fmin)/ftic.gt.10.) goto 2101
	ftic=ftic/2.
	goto 2102	!ensure at least 5 tics so get numerical label
2101	xtic=2.*xwbase	!number every 10th bin
c# For Lahey/Hgraph xtic refers to major, not minor tics so make
c bigger ( say 2-fold rather than 5-fold for now?- 5-fold faster!)
	xtic=xtic*5.
	if(.not.logt) ftic=ftic*5.	!smaller tics for sqrt scale for now
c
	yval(0,1)=flo		!set whether 0 or not- it is checked in VHIST
	xval(0,1)=xval(1,1)	!if no flo- it is checked in VHIST (in MINMAX)
	xval(nbin+2,1)=xval(nbin+1,1)		!ditto if no fhi bin
	if(flo.gt.0.0001) then
	   xval(0,1)=xval(1,1)-xwbase
	   if(logt) xval(0,1)=xval(1,1)*(xval(1,1)/xval(2,1))
	   xmin=xval(0,1)
	endif
	yval(nbin+1,1)=fhi
	if(fhi.gt.0.0001) then
	   xval(nbin+2,1)=xval(nbin+1,1)+xwbase
	   if(logt) xval(nbin+2,1)=xval(nbin+1,1)*
     &	(xval(nbin+1,1)/xval(nbin,1))
c	  keep ratio for hi bin as for other bins, so same width on log scale
	   xval(nbin+2,1)=xval(nbin+1,1)*(xval(nbin+1,1)/xval(nbin,1))
	   xmax=xval(nbin+2,1)
	 endif
c Curves:  (NB may have calc curve- to show exponential shape in logt
c plots) even if not fitted
	if(ncalc.gt.0) then
	  ncurvc=1		!copy first calc curve here
	  ncal(1)=ncalc
	  icurvc(1)=1
	  iline(1)=0		!so calc curve is continuous
	  do 2 i=1,ncal(1)
		xcal(i,1)=xcalc(i)
		ycal(i,1)=fcalc(i)
2	  continue
	endif
c
30	continue	!jump to here for point amp histos
	if(logt) iscal=1	!scale internally 1st time
	ntx=5		!if not logt set initial input values
	nty=5
	itx=1
	ity=1
c curves:
	ncurvc=0		!no calc curve yet
	if(logt) then
	  ncurvc=1	!already copied in
	  iline(1)=2		!so calc curve is short dashed
	endif
	if(debug()) then
	  print 77,xmin,xmax,fmin,fmax,xtic,ftic,flo,fhi,xw
77	  format(' xmin,xmax,fmin,fmax,xtic,ftic,flo,fhi,xw=',/,9g13.6)
	  print 771,titlex,titley
771	  format(' X axis: ',a40,/,' Y axis: ',a40)
	  pause
	endif
	isetcol=0
	ncomp1=ncomp(jset)
	histo=.true.
	call VHIST5(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ILOG,ISCAL,
     & XMIN,XMAX,fMIN,fMAX,XTIC,fTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO,XHI,YLO,YHI,xw,lt2,inumx,inumy,
     & titlex,titley,ilabel,doframe,idiskq,
     & autplt,draft,itit,title1,cbig,ifont,landscap,fitted.or.prefit,
     & theta,ifitype,ncomp1,sval,isval,iask,
     & isetcol,ndv1,ndc1,kmax,iver)
c     & theta,ifitype,ncomp,sval,isval,iask,iver)
c
	goto 201
C
110	CONTINUE
C NOW SECTION DONE ONLY WHEN RETURNING FROM FITTING,TO CALC
C AND DISPLAY FITTED CURVE
c Try calculating the curves here, after xmin etc defined, so curve
c covers whole range? But will cover whole X-range only- will not help
c if curve gets outside ymin=ymax before xmin/xmax reached- will still miss
c a bit of the curve
c NB for dist of log(t) the values of XMIN,XMAX are in log10 units
c from 1st call to VHIST so dxcalc=log10(factor)
	call CALDIS(XAXIS,XMIN,XMAX,FREQ,NBIN,XWBASE,
     & AMEAN,AREA,SD,NCOMP,ANTRUE,AHIST,OBHIST,ISG,
     & GAMDST,logt,ncalc,xcalc,fcalc,obfreq,frqcal,idtype,
     & jset,nset)
c Set current values of amean() and tau() for current set for use in partext
c (to put values on graphs) -commumicated via common/ptext/ameant(10),areat(10)
c  Make tau() negative here to signal that it is constrained
	do i=1,ncomp(jset)
	   ameant(i)=amean(i,jset)
	   if(itcon(i,jset).eq.0) ameant(i)=-ameant(i)
	   areat(i)=area(i,jset)
	   sdt(i)=sd(i,jset)
	enddo
c
	if(ncalc.gt.0) then
	  ncurvc=1		!copy first calc curve here
	  ncal(1)=ncalc
	  icurvc(1)=1
	  iline(1)=0		!so calc curve is continuous
	  do 3 i=1,ncal(1)
		xcal(i,1)=xcalc(i)
		ycal(i,1)=fcalc(i)
3	  continue
	endif
257	continue		!come here to repeat display
	IF(AHIST) GOTO 111
	if(idtype.eq.52) goto 111
	IF(OBHIST) GOTO 127
	if(pophist) goto 111	!gaussian for Popen distn
c Plot data and calc curve (+calc freq density if req)
c histos:
	if(frqcal(1).gt.-0.5) then
	  ncurvd=2
	  icurvd(2)=2
	  ndat(2)=nbin
	  ijoin(2)=3      !med dashes
	  do 4 i=1,nbin+1
4	    xval(i,2)=xaxis(i)
	  do 41 i=1,nbin
41	    yval(i,2)=frqcal(i)
	  yval(0,2)=-1		!?
	  yval(nbin+2,2)=-1
c	  xval(0,2)=xval(0,1)
c	  xval(nbin+2,2)=xval(nbin+2,1)
	endif
c
c curves:  already copied
	if(debug()) then
	  print 772,xmin,xmax,fmin,fmax,xtic,ftic,flo,fhi,xw
772	  format('Pos#2: xmin,xmax,fmin,fmax,xtic,ftic,flo,fhi,xw='
     &	,/,9g13.6)
	  pause
	endif
	isetcol=0
	ncomp1=ncomp(jset)
	histo=.true.
	call VHIST5(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ILOG,ISCAL,
     & XMIN,XMAX,fMIN,fMAX,XTIC,fTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO,XHI,YLO,YHI,xw,lt2,inumx,inumy,
     & titlex,titley,ilabel,doframe,idiskq,
     & autplt,draft,itit,title1,cbig,ifont,landscap,fitted.or.prefit,
     & theta,ifitype,ncomp1,sval,isval,iask,
     & isetcol,ndv1,ndc1,kmax,iver)
c
	goto 206
C
C NEXT CALC CURVE FOR AHIST-CALC CURVE ONLY
111	CONTINUE
c Histo and curve already copied into Xval etc
	isetcol=0
	ncomp1=ncomp(jset)
	histo=.true.
	call VHIST5(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ILOG,ISCAL,
     & XMIN,XMAX,fMIN,fMAX,XTIC,fTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO,XHI,YLO,YHI,xw,lt2,inumx,inumy,
     & titlex,titley,ilabel,doframe,idiskq,
     & autplt,draft,itit,title1,cbig,ifont,landscap,fitted.or.prefit,
     & theta,ifitype,ncomp1,sval,isval,iask,
     & isetcol,ndv1,ndc1,kmax,iver)
c
	goto 206
C
C NOW SECTION TO CALC SINGLE OR DOUBLE GEOMETRIC AS FITTED IN MLFIT
c Section for case where 'calc curve' is a histo (ops/burst etc)
127	CONTINUE
	ncurvc=0
	ncurvd=2
	icurvd(2)=2
	ndat(2)=nbin	!same for calc and obs histos?
1271	continue
	if(.not.logt) then
	   ijoin(2)=3      !dashed
	   do i=1,nbin+1
		xval(i,2)=xaxis(i)
	   enddo
	   do i=1,nbin
		yval(i,2)=obfreq(i)
	   enddo
	   yval(0,2)=-1		!?
	   yval(nbin+2,2)=-1
	   isetcol=0
	   ncomp1=ncomp(jset)
	   histo=.true.
	   call VHIST5(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ILOG,ISCAL,
     & XMIN,XMAX,fMIN,fMAX,XTIC,fTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO,XHI,YLO,YHI,xw,lt2,inumx,inumy,
     & titlex,titley,ilabel,doframe,idiskq,
     & autplt,draft,itit,title1,cbig,ifont,landscap,fitted.or.prefit,
     & theta,ifitype,ncomp1,sval,isval,iask,
     & isetcol,ndv1,ndc1,kmax,iver)
	   goto 206
	else if(logt) then	!xval1 is local so must recalc completely
	   ALLOCATE(xval1(ndv1,ndimd),yval1(ndv1,ndimd))
	   do i=1,nbin
		xval1(i,1)=float(i)
	   enddo
	   do i=1,nbin
		yval1(i,1)=float(i)*freq(i)	!N*r*P(r)=r*freq(r)
	   enddo
	   do i=1,nbin
		xval1(i,2)=float(i)
	   enddo
	   do i=1,nbin
		yval1(i,2)=obfreq(i)	!calc freq from caldis
	   enddo
	   syms(1)=3.0
	   isym(1)=-3	!filled square for data
	   syms(2)=2.5
	   isym(2)=-7	!filled circ for fit
c	   ijoin(1)=0
	   ijoin(1)=-1	!not joined
	   ijoin(2)=1	!dotted
	   isetcol=0
	   kwi=1
	   kwj=1
	   ALLOCATE(icurvw(ndimd),weight(kwi,kwj))
	   do i=1,ndimd
		icurvw(i)=-1	!no SD
	   enddo
c ***Temp addition for T671A paper
c	   goto 777		!skip
cc Better plot wild type as data points not calc curve
c	   iscal=1	!scale internally for test data
cc	   ncurvc=1
cc	   icurvc(1)=1
c	   ncalc=nbin
cc	   ncal(1)=nbin
cc	   iline(1)=1
c	   ncurvd=3
c	   icurvd(3)=3
c	   xm=1.0
c	   nbin1=99
c	   ndat(3)=nbin1
c	   xmax=100.
c	   ijoin(3)=-1
c	   isym(3)=-2		!triangle
c	   fmax=1.2*fmax		!temp- reset in SCVDU
c	   dxcalc=1.0		!increment
c	   am1=1.177	!mean for WT NMDA/NR2A
c	   a1=0.3538	!area
c	   am2=4.0325
c	   a2=0.416
c	   am3=30.06
c	   a3=0.2304
c	   do i=1,nbin1
c		r=float(i)
cc		xcal(i,1)=r
c		xval1(i,3)=r
c		f=a1*(1./am1)*(1.0 - 1.0/am1)**(r-1.0)
c		f=f+a2*(1./am2)*(1.0 - 1.0/am2)**(r-1.0)
c		f=f+a3*(1./am3)*(1.0 - 1.0/am3)**(r-1.0)
cc For log(t) distn xwbase defined in CDIST as bin width in log10 units
cc		ycal(i,1)=antrue(jset)*f*r*2.30259
c		ycal(i,5)=antrue(jset)*f	!use ycal(i,5) now as temp storage for print
c		yval1(i,3)=antrue(jset)*f*r
c	   enddo
c	   print 420
c420	   FORMAT(' Print calculated values for WT NR1/NR2A [N] ? ')
c	   ans='N'
c	   call INPUTa(ans)
c	   if(ans.eq.'Y') then
c		print 421
c		if(discprt) write(8,421)
c421 		format(' Calculated values for WT NR1/NR2A',/,
c     &	 '    r values      Frequency(r)',/,
c     &	 ' log(r) values     r*freq(r))')
c		do i=1,nbin
c		   print 32,xval1(i,3),ycal(i,5)
c		   if(discprt) write(8,32) xval1(i,3),ycal(i,5)
cc32		   format(g13,6,2x,g13.6)
c		   print 33,alog10(xval1(i,3)),yval1(i,3)
c		   if(discprt) write(8,33) alog10(yval1(i,3)),yval1(i,3)
cc33		   format(3x,g13.5,1x,g13.5)
c		   if(mod(i,18).eq.0) print 421
c		enddo
c	   endif
c
777	   continue
c ***end of temp addition
c=	   iask=-2
	   iask=2
	   ncomp1=ncomp(jset)
	   histo=.false.
	   call VPLOT5(XVAL1,YVAL1,NDAT,icurvd,ncurvd,ijoin,syms,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO1,XHI1,YLO1,YHI1,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,draft,itit,title1,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp1,interp,
     & isetcol,itrace,ndv1,ndc1,weight,kwi,kwj,icurvw,kmax,iver)
	   ans='N'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,'Redisplay plot with data bins pooled',
     &		defolt,12,cans)
	   call GETINPa(cans,ans)
c	   ans='N'
c	   call DCASK('Redisplay with data bins pooled',ans,ans)
	   if(ans.eq.'Y') then
		call VIDEOMOD(3)
		call LOCATE(0,0)		!start typing at top of page
		n=20
		nb=5
c check to get integer number of pooled bins with same number of bins in each
c nearest number of pooled bins is
		np=(nbin-n)/nb
		n0=nbin-np*nb	!number of unpooled bins
23		print 21,n0,nb
21		format(' For r > N pool sets of NB bins: N, NB [',
     &	  i3,',',i3,'] = ')
		call INPUT2i(n0,nb)
		np=(nbin-n0)/nb
		n0=nbin-np*nb	!number of unpooled bins
		print 22,n0,nb
22		format(' Nearest: ',
     & ' single bins up to r = ',i3,' then ',i3,' bins pooled, OK [Y] ')
		ans='Y'
		call INPUTa(ans)
		if(ans.eq.'N') goto 23
		do i=1,n0
		   xval1(i,1)=float(i)
		   yval1(i,1)=float(i)*freq(i)	!N*r*P(r)=r*freq(r)
		   ycal(i,5)=freq(i)	 !use ycal(i,5) as temp storage for print
		enddo
		ndat(1)=n0 + np
		if(discprt) write(8,24) ndat(1),n0,nb
24		format(/,' Pooled data bins: total number of bins = ',i4,/,
     & ' single bins up to r = ',i3,' then ',i3,' bins pooled',/,
     & ' (frequencies averaged, and multiplied by mean r value)')
c  now have np pooled bins, each containing nb original bins
C The ordinate can be pooled in various ways -in present version
c average the frequencies (rather than the r*freq(r)) values
		i=n0
		do j=1,np
		   sx=0.0
		   sy=0.0
		   do m=1,nb
			i=i+1
			sx=sx + float(i)
c			sy=sy + float(i)*freq(i)	!average r*freq(r)
			sy=sy + freq(i)	!average freq(r)
		   enddo
		   xval1(j+n0,1)=sx/float(nb)
c		   yval1(j+n0,1)=sy/float(nb)	!mean (r*freq(r))
		   ycal(i,5)=sy/float(nb)	 !use ycal(i,5) as temp storage for print
		   yval1(j+n0,1)=xval1(j+n0,1)*sy/float(nb)	!mean r * mean freq
		enddo
		print 20
20		FORMAT(' Print new frequencies [N] ? ')
		ans='N'
		call INPUTa(ans)
		if(ans.eq.'Y') then
		   print 31
		   if(discprt) write(8,31)
31 		   format(
     &	 '    r values      Frequency(r)',/,
     &	 ' log(r) values     r*freq(r))')
		   do i=1,ndat(1)
			print 32,xval1(i,1),ycal(i,5)
			if(discprt) write(8,32) xval1(i,1),ycal(i,5)
32			format(g13.6,2x,g13.6)
			print 33,alog10(xval1(i,1)),yval1(i,1)
			if(discprt) write(8,33) alog10(xval1(i,1)),yval1(i,1)
33			format(3x,g13.5,1x,g13.5)
			if(mod(i,18).eq.0) print 31
		   enddo
		endif
		goto 777	!replot with new bins
	   endif
	   if(allocated(icurvw)) then
		DEALLOCATE(icurvw,weight)
	   endif
	   DEALLOCATE(xval1,yval1)
	   goto 206
	endif		!end of log display of geometric
C
201	CONTINUE
C   AFTER DISPLAY OF TRANSFORMED DIST, RETURN.
	IF(IDEST.EQ.31) GOTO 256
C GO TO MLFIT EVEN FOR OBHIST WITH ONE COMP SO THAT CAN FIT OVER LIMITED
C RANGE AND SO THAT ERRORS CAN BE CALC. N.B. AS ARRANGED NOW CANNOT PLOT
C A NON-FITTED DISPLAY IF ASK FOR CORRELATION TESTS
	if(ahist.and.id.eq.8) then
	   ans='N'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,'Replot with different bins',defolt,ict,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'Y') then
		call VIDEOMOD(3)
		call LOCATE(0,0)		!start typing at top of page
	      goto 208
	   endif
	endif
c
	IDEST=0
	if(nset.eq.1) then
	 ans='Y'
	 call DEFOLTa(ans,defolt)
	 call QDIALOG(1,'Fit this display',defolt,ict,cans)
	 call GETINPa(cans,ans)
	 if(ans.eq.'Y') then
	   if(pahist) then	!copy data to freq() for point amp fit
		do i=-5000,5000
		   freqs(i)=0.0	!initialise
		enddo
		if(id.eq.2) then
		   do i=ifmin(1),ifmax(1)
			j=i
			if(revamp) j=-i
			freqs(j)=float(ifreq1(i))
		   enddo
		else if(id.eq.3) then
		   do i=ifmin(2),ifmax(2)
			j=i
			if(revamp) j=-i
			freqs(j)=float(ifreq2(i))
		   enddo
		else if(id.eq.4) then
		   do i=ifmin(3),ifmax(3)
			j=i
			if(revamp) j=-i
			freqs(j)=float(ifreq3(i))
		   enddo
		else if(id.eq.5) then
		   do i=ifmin(4),ifmax(4)
			j=i
			if(revamp) j=-i
			freqs(j)=float(ifreq2(i)+ifreq3(i))
		   enddo
		endif
	   endif
c Define idest=2 so fits next
	   IF(IDEST.NE.21) IDEST=2		!FIT ONLY
	   IF(IDEST.EQ.21) IDEST=22	!CORRELATIONS AND FIT
c note that if display of transformed variables requested in CORCAL
c cannot do fit even if requested here
	   RETURN
	 endif	!end if nset=1, ans=Y
	else if(nset.gt.1) then
28	   title='What next?'
	   if(jset.lt.nset) then
         	call INTCONV(jset+1,cnum)
		strings(1)='Display the next set (#'//charnb(cnum)//')'
		strings(2)='Fit all sets simultaneously now'
		strings(3)='Compare sets by randomisation test'
		nstr=3
	   else if(jset.eq.nset) then
		strings(1)='Skip both fit and test'
		strings(2)='Fit all sets simultaneously now'
		strings(3)='Compare sets by randomisation test'
		nstr=3
	   endif
c	   helps(1)='Read another jump  '
c	   helps(2)=' -give jump # next '
c	   nhelp=2
	   nhelp=0

	   if(ilin.lt.1.or.ilin.gt.3) then
		ilin=1		!default
		if(jset.eq.nset) ilin=2
	   endif
	   call BELL(2)
	   call POPMENU(nxlo,nylo,nyhi,strings,nstr,ictm,icfm,icupm,
     &	ibkm,title,helps,nhelp,ilin,ans,ival)
		if(ilin.eq.0) goto 28			!iline=0 for ESC=cancel
11	   if(ilin.eq.1) then
	 	   call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
		   if(jset.eq.nset) RETURN
		   jset=jset+1
		   idest=705	!so goto 705  in ekdist for next display
		   RETURN
	   else if(ilin.eq.2) then
c Define idest=2 so fits next -or 702 if not all displayed before fit
c (no option for correlations when nset>1)
		   idest=702		!pre-fit display skipped for some sets
		   RETURN
	   else if(ilin.eq.3) then		!randomisation test
c		   j2=jset		!(last displayed)
c		   if(j2.gt.1) j1=1
		   j1=1
		   j2=2
 		   call DEFOLT2i(j1,j2,defolt)
		   call QDIALOG(1,
     &	   'Compare sets',defolt,ict,cans)
		   call GETINP2i(cans,j1,j2)
c Must either return to main prog to do test, or put yval() in common
c NB Yval() here contains frequencies, but in call to SCVDU this array
c is called yval1() and yval() in main program still contains actual
c numbers (durations etc) used for the histo. OR just add yval to args
		   call VIDEOMOD(3)
		   call LOCATE(0,0)		!start typing at top of page
		   call RANCOMP(yval0,nyval,j1,j2,thist,bhist,obhist,idtype)
		   iopt=1
		   print 10 ,iopt
10		   format(
     &		' (1) Fit all sets simultaneously now',/,
     &		' (2) Compare sets with randomisation test',/,
     &		' (3) Carry on',/,
     &		' Option number [',i2,'] = ')
		   call INPUTi(iopt)
		   if(iopt.eq.1) then
			ilin=2
			goto 11
		   else if(iopt.eq.2) then
			ilin=3
			goto 11
		   else if(iopt.eq.3) then
			fitted=.false.
		   endif
	   endif
c	   else if(jset.eq.nset) then
c		ans='Y'
c		call DEFOLTa(ans,defolt)
c		call QDIALOG(1,'Fit all sets simultaneously now',
c     &		defolt,ict,cans)
c		call GETINPa(cans,ans)
c		if(ans.eq.'Y') then
c		   IF(IDEST.NE.21) IDEST=2		!FIT ONLY
c		   IF(IDEST.EQ.21) IDEST=22	!CORRELATIONS AND FIT
c		   RETURN
c		endif
c	   endif
	endif
c
	IF(IDEST.EQ.21) then
 	   call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	   RETURN
	else
	   goto 206
	endif
c
c Now type note and leave (or superimpose previous fit)
c but if nset>1 then check if need to increment jset to show next fit
c
206	continue
c
	if(fitted.and.nset.gt.1) then
	   if(jset.lt.nset) then
281		title='What next?'
         	call INTCONV(jset+1,cnum)
		strings(1)='Display the next fit (#'//charnb(cnum)//')'
		strings(2)='Skip display of rest of fits'
		nstr=2
c		helps(1)='Read another jump  '
c		helps(2)=' -give jump # next '
c		helps(3)='Exit -when all jump'
c		helps(4)=' jumps are analysed'
c		nhelp=4
		nhelp=0
		ilin=1		!default
		call BELL(2)
		call POPMENU(nxlo,nylo,nyhi,strings,nstr,ictm,icfm,icupm,
     &	ibkm,title,helps,nhelp,ilin,ans,ival)
		if(ilin.eq.0) goto 281			!iline=0 for ESC=cancel
		if(ilin.eq.1) then
		   jset=jset+1
		   idest=705	!so goto 705  in ekdist for next display
	 	   call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
		   RETURN
		else if(ilin.eq.2) then
		   idest=706      !finished (or ask whether to redisplay a specified set)
		endif
	   else if(jset.eq.nset) then
		ans='N'
		call DEFOLTa(ans,defolt)
		call QDIALOG(1,'Redisplay all the fits',
     &		defolt,ict,cans)
		call GETINPa(cans,ans)
		if(ans.eq.'Y') then
		   jset=1
		   idest=707
		   RETURN
		else
		   idest=0
		endif
	   endif
	endif
c
	call VIDEOMOD(3)
	call LOCATE(0,0)		!start typing at top of page
	if(debug().and.histo) then
c	LO bin is from Xval(0,j) to Xval(1,j); freq in Yval(0,j)
c	First bin is from Xval(1,j) to Xval(2,j); freq in Yval(1,j)
c	ith bin is from Xval(i,j) to Xval(i+1,j); freq in Yval(i,j)
c	last bin is from Xval(nbin,j) to Xval(nbin+1,j); freq in Yval(nbin,j)
c	HI bin is from Xval(nbin+1,j) to Xval(nbin+2,j); freq in Yval(nbin+1,j)
	   ans='Y'
	   call DCASK('Output bin freqs as ASCII file',ans,ans)
	   lentext=0	!no text string
	   icolwid=20
	   ncol=2
	   nrow=ndat(1)
	   ALLOCATE(colhead(ncol),data(nrow,ncol))
	   colhead(1)=' Xmid'
	   colhead(2)=' Yval'
	   do i=1,nrow
		data(i,1)=(xval(i,1)+xval(i+1,1))/2.
		data(i,2)=yval(i,1)
	   enddo
c	   call ASCWRITE(data,nrow,ncol,text,ndim1,ndim2,lentext,
c     & 	icolwid,colhead,ic)
	   call ASCWRITE(data,nrow,ncol,text,nrow,ncol,lentext,
     & 	icolwid,colhead,0)
	   DEALLOCATE(colhead,data)
	   if(ncurvc.gt.0) then
		call DCASK('Output fitted curve as ASCII',ans,ans)
		ncol=2
		nrow=ncal(1)
		ALLOCATE(colhead(ncol),data(nrow,ncol))
		colhead(1)=' Xcalc'
		colhead(2)=' Ycalc'
		do i=1,nrow
		   data(i,1)=xcal(i,1)
		   data(i,2)=ycal(i,1)
		enddo
		call ASCWRITE(data,nrow,ncol,text,nrow,ncol,lentext,
     & 	icolwid,colhead,0)
		DEALLOCATE(colhead,data)
	   endif
	endif
c20	continue	!JUMP TO HERE IF ALREADY FITTED
207	continue
	iopt=1
c
	if(nfitdone.gt.1) then
	   print 40
40	   FORMAT(
     &    ' (1) Carry on',/,
     &    ' (2) Type in a note to go on the printout',/,
     &    ' (3) Repeat the same display',/,
     &    ' (4) Display precalculated curve and print parameters',/,
     &    ' (5) Fit the same histogram again',/,
     &    ' (6) Superimpose previous fitted curve(s) on graph',/,
     &    ' Option number [1] = ')
	   call INPUTi(iopt)
	   if(iopt.lt.1.or.iopt.gt.6) goto 207
	else
	   print 2063
2063	   FORMAT(
     &    ' (1) Carry on',/,
     &    ' (2) Type in a note to go on the printout',/,
     &    ' (3) Repeat the same display',/,
     &    ' (4) Display precalculated curve and print parameters',/,
     &    ' (5) Fit the same histogram again',/,
     &    ' Option number [1] = ')
	   call INPUTi(iopt)
	   if(iopt.lt.1.or.iopt.gt.5) goto 207
	endif
	if(iopt.eq.2) then
	   call TITENT1(' Type in the note:',note,79,.true.)	!blank=true
	   if(discprt) write(8,4011) note
4011	   format(' =============================================',/,
     &    1x,a79,/,
     &    ' =============================================')
	   goto 207		!redisplay?
	else if(iopt.eq.3) then		!redisplay
	   autplt=.true.
	   itit=1
	   goto 257
	else if(iopt.eq.4) then
	   IDEST=4		!IF PRE-CALC CURVE WANTED
	   RETURN
	else if(iopt.eq.5) then
	   IDEST=2		!FIT ONLY
	   goto 999
	else if(iopt.eq.6) then
	   autplt=.true.
	   itit=1
	   print 46
46	   format(' Previous fits:')
	   do j=1,nfitdone
	      print 42,j,fitsav(j),ncompsav(j)
42		format(' (',i2,') Fit of ',a12,':',i2,' components')
	   enddo
	   nsuper=1
	   print 43
43	   format(' Number of these to be superimposed [1] = ')
	   call INPUTi(nsuper)
	   jsup=nfitdone-1	!previous fit=default
	   do i1=1,nsuper
		print 44,i1,jsup
44		format(' (',i2,') Superimpose fit number [',i2,'] = ')
		call INPUTi(jsup)
c	     call ptyp1 (with errors=false) to calc area, amean etc
		do i=1,20
		   theta1(i)=thetsav(i,jsup)
		enddo
		if(fitsav(jsup).eq.'Gaussians') then
		   ahist1=.true.
		else if(fitsav(jsup).eq.'geometrics') then
		   obhist1=.true.
		else if(fitsav(jsup).eq.'exponentials') then
		   thist1=.true.
		else if(fitsav(jsup).eq.'gammas') then
		   gamdst1=.true.
		endif
		if(obhist1) then
		   ncurvd=ncurvd+1
		   j=ncurvd
		else
		   ncurvc=ncurvc+1
		   j=ncurvc
		endif
	      print 45,jsup,j
	      if(discprt) write(8,45) jsup,j
45		format(' Superimpose fit # ',i2,' as curve # ',i3,':')
c Calculate the required curve. First calc amean etc (don't change current vals)
c (call with ghist=false to prevent calc of tcrits)
		call PTYP1(theta1,area1,amean1,sd1,ksav(jsup),
     &	ncompsav(jsup),yhisav(jsup),ylosav(jsup),nfitsav(jsup),
     &	antrue1,.false.,gamdst1,isgsav(jsup),idsav(jsup),
     &	ahist1,thist1,obhist1,.false.,
     &      excreg,yexclow,yexchigh,isdcon,anfit,nsweep,nnull,nfitt)
		call CALDIS(xaxis,xmin,xmax,freq,nbin,xwbase,
     &	 amean1,area1,sd1,ncompsav(jsup),antrue1,ahist1,obhist1,
     &	 isgsav(jsup),
     &	 gamdst1,logt,ncalc,xcalc,fcalc,obfreq,frqcal,idsav(jsup),
     &	 jset,nset)
c	now put results in xcal,ycal (or xval,yval for geometrics)
c 	 (j defined above as nurvc or ncurvd as appropriate)
		if(obhist1) then
		   ndat(j)=nbin
		   icurvd(j)=j
		   ijoin(j)=4
		   do i=1,nbin+1
			xval(i,j)=xaxis(i)
		   enddo
		   do i=1,nbin
			yval(i,j)=obfreq(i)
		   enddo
		   yval(0,j)=-1
		   yval(nbin+2,j)=-1
		else
		   ncal(j)=ncalc
		   icurvc(j)=j
		   iline(j)=3		!so superimposed curve is dashed
		   do i=1,ncalc
			xcal(i,j)=xcalc(i)
			ycal(i,j)=fcalc(i)
		   enddo
		endif
	   enddo		!end of jsup loop
	   if(obhist) goto 1271
	   goto 257
	else
	   goto 2064	!carry on
	endif
c
c This analysis ended so choose where next
256	continue
	idest=-1		!options 5-8 only in WNEXT
	goto 999
2064	continue
	IDEST=0
c	RETURN
c
999	continue
cc If fitted, copy the calc curve into xcal(i,3),ycal(i,3) so it can be
cc superimposed on next fit
c	if(fitted) then
c	   do i=1,ncal(1)
c		xcal(i,3)=xcal(i,1)
c		ycal(i,3)=ycal(i,1)
c	   enddo
c	   ncal(3)=ncal(1)
c	   fitdef=.true.
c	endif
	RETURN
c
	END



