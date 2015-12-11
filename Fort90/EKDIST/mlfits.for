	subroutine MLFITS(theta,ybar,ahist,thist,obhist,pophist,
     & fitted,nfix,k,kfit,id,jfix,errfac,ndisp,nbin,tres,ymin,ymax,
     & mono,elmax,nev,errors,pahist,revamp,ihtype,xmax,titlep,
     & cjump,tzero,tsamp,
     & nset,readini)
c NB id=idtype
c MLFITS includes code from previous MLFIT and CSIMP in one routine
c
c Modif 12/05/00 02:17pm for simultaneous fits of nset sets of data
c with shared parameters.  At present this is done only for THIST and OBHIST
c and only tau (mean) can be shared, not areas.
c ncomp(j)=number of components for set j
c nfit(j)=number points to be fitted in set j
c ylow(j),yhigh(j) = fitted range
c tau(i,j), area(i,j) = mean,area for component i of set j (read guesses
c   into these and make initial theta() from them)
c neq=number of constraints tau(ieq(n),jeq(n))=tau(ieq0(n),jeq0(n)), n=1,neq
c Define 3 auxiliary arrays to speed up identification of elements of theta
c (1)itcon(i,j)
c   Each tau(i,j) is either
c   (a) free and so in theta(); for these itcon(i,j)=index in theta of tau(i,j)
c   (b) constrained, so not in theta; for these itcon(i,j)=0
c (2) ialoc(i,j)=index in theta() of area(i,j)
c (3) itloc(m)=0 if theta(m) is a time constant, =1 if it is an area, but =-1
c      for the last area in each set (# i=ncomp(j)-1)
c
c Modif 04/14/98 06:58am =default fit range now up to xmax (=largest
c value on display) rather than ymax=largest observed value
c 11/13/97 03:56pm Modif throughout for 50 paramters!
c    theta(50),theta(50,5), jfix(50), titlep(5)
c
c Modif 11/04/97 05:58pm so parameter names are defined in titlep()
c
c Modif 07/27/94 02:17pm for (a) new amplitude constraints, and
c (b) to exclude a region from fit (not point amp histo only at present)
c
c Modif 07/06/93 11:06am to put text into the dialog box that was defined
c  in VHIST2 (stll in graphics mode up to call VIDEOMOD(3))
c 06/03/89 02:22pm Lahey version of mlfit3
c
C Version for MLFITS for EKDIS3
c DC modif for 11-73 AUG 1986. Add Xaxis,Freq for fast fit (May 87)
c
C VERSION OF MLFIT1 WITH SIMPLEX RATHER THAN PATTERNSEARCH. TOO BIG SO
C CALLS TO SIMPLEX REMOVED TO SEP OVERLAY (CSIMP.FOR). IF CALLED WITH
C NFIX=-1 DOES EVERYTHING UP TO THESE CALLS, THEN RETURNS TO CALL CSIMP,
C THEN CALL MLFITS AGAIN-WHEN NFIX =>0 GOES STRAIGHT TO END TO FINISH OFF
C LAST 4 PARAM (JFIX-NDISP) ADDED FOR TRANSMISSION TO CSIMP.
C
C VERSION OF MLFIT WITH YVAL AS PARAM RATHER THAN IN COMMON. ALSO PARAM
C FOR PTSCH5,EXPLIK ETC
C SUBLEV,NFILE,IDISK,NVALUE,NEEDED ONLY FOR WRITING TODISC FOR CHAIN TO
C ERROR,PROG (FOR USE ON RETURN TO SCDIST)
	REAL THETA(50)		!PARAM
	INTEGER JFIX(50)
	integer*2 videotyp
C AREA=FRACTIONAL AREA,AMEAN=TAU (FOR THIST) OR MEANS
C (FOR AHIST),SD=STANDARD DEV (FOR AHIST)
C NOW DECLARE LOCAL ARRAYS FOR FITTING
	character*1 ans,UC
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character cnum0*11,cnum1*11,cnum2*11,cnum3*11		!for dialog
	LOGICAL AHIST,pahist,THIST,DEBUG,FITTED,OBHIST,AMPOS,
     & GAMDST,fastf,logt,mono,graphic,excreg,usewfreq
	logical pophist	!in common -true for fittting Gaussian to Popen/bst
	logical discprt,slock,caplock,pon1,errors,revamp,cjump
	logical negpar,help,readini
c For point amplitude histos
	real*4 freqs(-5000:5000)		!in common/pablk/ for GAUFIT
c
	character*62 helps(7)	!help strings
	character*32 strings(7)	!for popmenu
c
	character*20 TITLEP(50)		!names of params for fit
c
c New arrays for nset>1
	real*4 tau(10,10),area(10,10)
	integer itcon(10,10),ialoc(10,10),itloc(50)	!see above
	integer ncomp(10)
	integer nfit(10)
	real*4 ylow(10),yhigh(10)
	real*4 yexclow(10),yexchigh(10)
	integer*4 neq,ieq(50),jeq(50),ieq0(50),jeq0(50)
	common/const/nset1,neq,ieq0,jeq0,ieq,jeq,itcon,ialoc,itloc
	real*4 setlik(10)
	common/logliks/setlik	!likelihood for each set separately
c
	common/dp/discprt
	common/PABLK/freqs,calfac2,ilow,ihigh		!for GAUFIT,MLFITS
	COMMON/FITBLK/yval(500000,10),xaxis(510),freq(510),Nyval(10),
     & Ylow,Yhigh,Ncomp,Nfit,ampos,pen,gamdst,isg,fastf,logt,nb1,nb2,
     & excreg,yexclow,yexchigh,isdcon,wfreq(51200),anfit,
     & nfitt,yvalt(5120),iconst
	common/popvalx/mxlo,mylo,myhi,ictx,ibkx,icfx	!values for poptext calls
	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls
	common/hlp/help    !for QDIALOG
c
	EXTERNAL EXPLIK,GAULIK,OBLIK,gaufit
c
	debug()=caplock()
c	pon()=slock()
	pon1=slock()
C
101	format(a1)
108	FORMAT(/)
c
	idtype=id
	nset1=nset	!for common/const/
c
	GAMDST=.FALSE.
	fastf=.false.
	ict=11	!text colour for DIALOG box
	graphic=VIDEOTYP().eq.18	!may not be in graphics for refitting
	if(mono) ict=15
c==	if(.not.logt) goto 50 ========? why?
c
c Remove fast fit option -not necessary now
	fastf=.false.
	if(ahist.and.idtype.eq.8) fastf=.true.	!for Patlak plot
c	if(graphic) then
c	   ans='N'
c	    call DEFOLTa(ans,defolt)
c	    call QDIALOG(1,'Use fast (log-binned) ML fitting',
c     &     defolt,ict,cans)
c	   call GETINPa(cans,ans)
c	   fastf=ans.eq.'Y'
c	else
c	   print 501
c501	   format('&Use fast (log-binned) ML fitting [N] ? ')
c	   read 101,ans
c	   fastf=UC(ans).eq.'Y'
c	endif
	if(fastf.and.discprt) then
	   write(8,502)
502	   format(' Fast (log-binned) fitting used')
	   if(ahist.and.idtype.eq.8) write(8,5021)
5021	   format(' 0.01 pA bins used for fitting regardless of display')
	endif
c
50	continue
c  START  OF JSET LOOP FOR BOTH THIST AND OBHIST
	do j=1,nset
	   if(thist) then
		ylow(j)=tres
cc		yhigh=10.*ymax
cc		yhigh=ymax		!default changed 04/13/98 02:57pm
cc		yhigh=xmax		!default changed again 04/14/98 07:00am
		if(logt) then
	 		yhigh(j)=10.0**xmax
	 	else
	 		yhigh(j)=xmax				!largest value on display
		endif
		if(ymax.gt.yhigh(j)) yhigh(j)=ymax		!largest obs value
		if(ihtype.eq.2.or.id.eq.101) then
			yhigh(j)=xmax	!for non-log disp or 1st latency default=max value displayed
		endif
		if(cjump) then
			if(yhigh(j).gt.tsamp-tzero) yhigh(j)=tsamp-tzero
		endif
		if(graphic) then
		   call DCFORMAT(ylow(j),8,3,cnum1)
		   call DCFORMAT(yhigh(j),8,3,cnum2)
c	         y1=ROUND(ylow,2)	!2 sig figs after dec point
c	         y2=ROUND(yhigh,2)	!2 sig figs after dec point
c	         call REALTOCH(y1,cnum1,11)
c		   call REALTOCH(y2,cnum2,11)
		   ans='Y'
		   if(nset.gt.1) then
			   call INTCONV(j,cnum0)
			   call WDIALOG(1,'Set # '//CHARNB(cnum0),12)
		   endif
		   call DEFOLTa(ans,defolt)
		   call QDIALOG(1,'Fit between Tres, Yhi = '//
     &         	  CHARNB(cnum1)//' and '//CHARNB(cnum2)//' (inc)',
     &		  defolt,ict,cans)
		   call GETINPa(cans,ans)
		else
		   if(nset.gt.1) then
			print 2073,j
2073			format(' SET # ',i3)
		   endif
		   print 207,ylow(j),yhigh(j)
207		   format(
     &'&Fit between Tres, Ymax = ',2g13.6,' (inclusive) O.K. [Y] ? ')
		   ans='Y'
		   call INPUTa(ans)
		endif
		if(UC(ans).eq.'N') then
		   if(graphic) then
			call QDIALOG(1,
     &		'Fit between Ylo, Yhi (inclusive) (ms)',
     &             ' ',ict,cans)
			call GETINP2r(cans,ylow(j),yhigh(j))
		   else
			print 208
208			format(
     &	'&Fit intervals between Ylow, Yhigh (inclusive) (ms) = ')
			call INPUT2r(ylow(j),yhigh(j))
		   endif
		endif
	   else if(ahist) then
		if(graphic) then
		   call QDIALOG(1,
     &	 'Fit amps between Ylo, Yhi (inc) (pA)',
     &	 ' ',ict,cans)
		   call GETINP2r(cans,ylow(j),yhigh(j))
		else
		   print 2081
2081		   format(
     &	'&Fit amplitudes between Ylow, Yhigh (inclusive) (pA) = ')
	         call INPUT2r(ylow(j),yhigh(j))
		endif
c For point amp histos, define relevant bins in common/pablk/
c (may include zero bins below ifmin() and/or above ifmax())
c NB values in freqs(), defined in SCVDU, now have sign allocated correctly
c when revamp=true.
c Also define NFIT here for point amp histos
		if(pahist) then
		   ilow=ifixr(ylow(j)/calfac2)
		   ihigh=ifixr(yhigh(j)/calfac2)
		   anf=0.0
		   do i=ilow,ihigh
			anf=anf+freqs(i)
		   enddo
		   nfit=ifixr(anf)
		endif
	   else if(pophist) then
		if(graphic) then
		   call QDIALOG(1,
     &	 'Fit Popen between Ylow, Yhigh (inclusive)',
     &	 ' ',ict,cans)
		   call GETINP2r(cans,ylow(j),yhigh(j))
		else
		   print 2083
2083		   format(
     &	'&Fit Popen between Ylow, Yhigh (inclusive) = ')
	         call INPUT2r(ylow(j),yhigh(j))
		endif
	   else if(obhist) then
		ylow(j)=1.0
c		yhigh=2.*ymax
c		yhigh=ymax
		if(logt) then
	 	   yhigh(j)=10.0**xmax
		else
	         yhigh(j)=xmax				!largest value on display
		endif
		if(graphic) then
	         call REALTOCH(ylow(j),cnum1,11)
	         call REALTOCH(yhigh(j),cnum2,11)
		   if(nset.gt.1) then
			call INTCONV(j,cnum0)
			call WDIALOG(1,'Set # '//CHARNB(cnum0),12)
		   endif
	   	   ans='Y'
	         call DEFOLTa(ans,defolt)
	         call QDIALOG(1,'Fit ops/burst between '//
     &       CHARNB(cnum1)//' and '//CHARNB(cnum2)//' (incl)',
     &	 defolt,ict,cans)
	         call GETINPa(cans,ans)
		else
		   if(nset.gt.1) then
			print 209,j
209			format(' SET # ',i3)
		   endif
	         print 2072,ylow(j),yhigh(j)
2072	   	   format(
     &   ' Fit ops/burst between = ',2g13.6,' (inclusive) O.K. [Y] ? ')
		   ans='Y'
		   call INPUTa(ans)
		endif
	      if(UC(ans).eq.'N') then
		   if(graphic) then
		      call QDIALOG(1,
     &	   'Fit ops/burst between Ylow, Yhigh (incl)',
     &	     ' ',ict,cans)
		      call GETINP2r(cans,ylow(j),yhigh(j))
		   else
		      print 2082
2082		      format(
     &	'&Fit ops/burst between Ylow, Yhigh (inclusive) (ms) = ')
	            call INPUT2r(ylow(j),yhigh(j))
		   endif
		endif
	   endif	!end of obhist
	   if(yhigh(j).lt.ylow(j)) then
		t=ylow(j)		!swap
		ylow(j)=yhigh(j)
		yhigh(j)=t
	   endif
c
c Ask if region is to be excluded from fit
c NB not for point amplitude histos at present (need to fix iexclow,
c iexchigh if this option to be used with point amp histos -see GAUFIT1.FOR
c and above)
	   excreg=.false.
	   if(ahist.and.idtype.eq.8) goto 72	!no exclusion for Patlak yet
	   if(.not.pahist) then
		excreg=.false.
		if(graphic) then
		   ans='N'
	 	   call DEFOLTa(ans,defolt)
	 	   call QDIALOG(1,
     &  	'Exclude from fitting a region within this range',
     &	defolt,ict,cans)
		   call GETINPa(cans,ans)
		   excreg=ans.eq.'Y'
		 else
		   print 54
54		   format(
     &	'&Exclude from fitting a region within this range [N] ? ')
		   ans='N'
		   call INPUTa(ans)
		   excreg=UC(ans).eq.'Y'
		 endif
59		if(excreg) then
		   if(graphic) then
		   call QDIALOG(1,
     &	 'Exclude values between Ylow, Yhigh (inclusive)',
     &	 ' ',ict,cans)
		   call GETINP2r(cans,yexclow(j),yexchigh(j))
		   else
			print 55
55			format(
     &	'&Exclude values between Ylow, Yhigh (inclusive) = ')
			call INPUT2r(yexclow(j),yexchigh(j))
		   endif
		   if(yexchigh(j).lt.yexclow(j)) then
			t=yexclow(j)		!swap
			yexclow(j)=yexchigh(j)
			yexchigh(j)=t
		   endif
		   if(yexclow(j).lt.ylow(j).or.yexchigh(j).gt.yhigh(j)) then
			call BELL(3)
			if(graphic) then
			   call WDIALOG(1,
     &		'Excluded range must be within overall range',12)
			else
			   print 58
58			   format(
     &		' Excluded range must be within overall range')
			endif
			goto 59
		   endif
		endif
	   endif	!end of .not.pahist
c
72	   continue
c NB for FASTF these must be bin boundaries!
c At present fastf option is used onl for Patlak plot, and for this
c yval and xaxis are not defined. Also value of nbin here is that
c for display bins, whereas fit uses the 0.01 pA frequencies defined
c in freqs() in CDIST1.
c freqs(-2)=frequency of values from -0.03 to -0.02 pA
c freqs(-1)=frequency of values from -0.02 to -0.01 pA
c freqs(0)=frequency of values from -0.01 to 0.0 pA
c freqs(1)=frequency of values from 0 to 0.01 pA
c freqs(2)=frequency of values from 0.01 to 0.02 pA
c so 100*amp corresponds with upper bound of bin Yfreq(i)
c This can be used to calculate start and end bins for the fit from
c ylow, yhigh, e.g. if ylow=0.01 pA then first bin=bin #2 = 1+ifixr(ylow*100.)
c and if yhigh=0.02, last bin =#2 = ifixr(yhigh*100.)
	   if(fastf.and.ahist.and.idtype.eq.8) then
		nb1=1+ifixr(ylow*100.)
		nb2=ifixr(yhigh*100.)
c This will round ylow, yhigh to nearest 0.01 pA so recalculate values
		ylow=float(nb1-1)/100.
		yhigh=float(nb2)/100.
		anf=0.0
		do i=nb1,nb2
		   anf=anf+freqs(i)
		enddo
		anfit=anf
		nfit=ifixr(anf)
c
		if(graphic) then
	         call INTCONV(nb1,cnum1)
		   call INTCONV(nb2,cnum2)
		   call WDIALOG(1,'Fit bins '//CHARNB(cnum1)//' to '
     &    		//CHARNB(cnum2),ict)
		else
		   print 71,nb1,nb2
71		   format(' Fitted bins= ',2i6)
		endif
		if(discprt) write(8,73) ylow,yhigh,nb1,nb2
73		format(
     &  ' Fit from ',f8.3,' pA to ',f8.3,' pA (bins ',i3,' to ',i3,')')
c		if(nb1.lt.1.or.nb2.gt.nbin) then
c		call BELL(1)
c		if(graphic) then
c		   call OPENDIALOG(2,12,.true.)		!draw dialog box #2
c		   call WDIALOG(2,'Not in range',12)
c		else
c		   print 70
c70		   format(' Values not within range of bins')
c		endif
c		goto 50
c		endif
	   endif		!end of fastf section
c
	   IF(OBHIST.AND.YLOW(j).LT.1.0) YLOW(j)=1.0
	   if(nset.gt.1) then
		if(pon1) write(7,2073) j
		if(discprt) write(8,2073) j
c2073		format(' SET # ',i3)
	   endif
	   if(pon1) write(7,2071) ylow(j),yhigh(j)
	   if(discprt) write(8,2071) ylow(j),yhigh(j)
2071	   format(' Fitted between ',g13.6,' and ',g13.6)
	   if(excreg) then
		if(pon1) write(7,56) yexclow(j),yexchigh(j)
		if(discprt) write(8,56) yexclow(j),yexchigh(j)
56		format('  -excluding values between ',g13.6,' and ',g13.6)
	   endif
C 1.0 IS MIN VALUE ALLOWABLE FOR OBHIST- CALC OF PROB(Y<YLOW) IN OBLIK1
C WILL BE NONSENSE IF SMALLER YLOW GIVEN

c	Now bursts with specified number of open periods k=isg+1 (isg=number
c    of short gaps)
	   IF(.NOT.(THIST.AND.ISG.GT.0.and.(id.eq.7.or.id.eq.8)))
     &     GOTO 53
c
	   if(graphic) then
	    ans='Y'
	    call DEFOLTa(ans,defolt)
	    call QDIALOG(1,
     &    'Fit Gamma distribution',
     &     defolt,ict,cans)
	    ans='Y'
	    call GETINPa(cans,ans)
	   else
	    print 52
52	    FORMAT('&Fit Gamma distribution [Y] ? ')
	    ans='Y'
	    call INPUTa(ans)
         endif
	   if(UC(ans).EQ.'Y') GAMDST=.TRUE.
	   if(gamdst.and.pon1) write(7,521) isg+1
	   if(gamdst.and.discprt) write(8,521) isg+1
	   if(graphic.and.gamdst) then
		call WDIALOG(1,
     &    'Specify ''one component'' if open time pdf is 1 exp',ict)
		call WDIALOG(1,
     &    'Specify ''two components'' if open time pdf is 2 exp',ict)
		call WDIALOG(1,
     &    '   (for sum of two openings only)',ict)
	   else if(gamdst) then
		print 521,isg+1
521	   format(' Gamma distribution for sum of ',i3,'''openings''.',/,
     &'    Parameters for this are the param for the ''open time''',/,
     &' distribution. Specify ''one component'' if the latter is',/,
     &' single-exponential (can be used for sum of ANY number of'/
     &' openings); specify ''two components'' if it is double',/,
     &' exponential (can be used only for the sum of two openings)'/)
	   endif
C N.B. 2-COMPONENTS MEANS THAT OPEN TIME PDF IS SUM OF 2 EXPONENTIALS
C WITH TIME CONST AND AREAS THAT ARE PARAM FOR THIS FITTING. FOR
C NCOMP=1 CONVOLUTION OF ANY NUMBER (ISG+1) OF THESE IS A GAMMA
C DISTRIBUTION. IF OPEN TIME IS SUM OF TWO EXP THEN CONVOLUTION
C OF SEVERAL OF THEM IS MORE COMPLEX AND PROG ALLOWS FIT ONLY FOR
C CONV OF TWO (ISG=1)
C
53	   continue
	   if(ncomp(1).lt.1) ncomp(1)=1
	   if(j.gt.1) ncomp(j)=ncomp(1)
	   if(graphic) then
 		call DEFOLTi(ncomp(j),defolt)
		call QDIALOG(1,'Number of components to be fitted',
     & 	 defolt,ict,cans)
		call GETINPi(cans,ncomp(j))
	   else
		print 1,ncomp(j)
1		FORMAT('&Number of components to be fitted [',i2,'] = ')
		call INPUTi(ncomp(j))
	   endif
	   IF(GAMDST.AND.(NCOMP(j).GT.2.OR.(NCOMP(j).EQ.2.AND.
     & 	ISG.NE.1))) goto 53
C REDEFINE YVAL SO IT CONTAINS ONLY THE NFIT VALUES BETWEEN
C YLOW AND YHIGH. REDEFINE YBAR AS MEAN OF THESE.
404	   continue
C FOR  OBHIST MAKE SURE THAT VALUES 'EXACTLY' EQUAL TO YLOW,YHIGH
C ARE NOT EXCLUDED
	   s=0.
	   if(obhist) s=1.0e-5
	   YLO=YLOW(j) - s
	   YHI=YHIGH(j) + s
c
c If wfreq() used (see CDIST1 and SETBIN1) then effective N is sum of
c wfreq() (actually wfreq(i)*0.001 used in SETBIN1)
	   if(ahist.and.idtype.eq.8) goto 3	!skip SD calc for Patlak plot (for now)
c
	   if(.not.pahist) then
		usewfreq=ahist.and.id.eq.7
		nfit(j)=0
		if(excreg) then
		   do i=1,nyval(j)
		     if((yval(i,j).ge.ylo).and.(yval(i,j).le.yexclow(j)).or.
     &	     (yval(i,j).ge.yexchigh(j)).and.(yval(i,j).le.yhi)) then
			   nfit(j)=nfit(j)+1
			   yval(nfit,j)=yval(i,j)
		     endif
		   enddo
	      else
		   do i=1,nyval(j)
			if((yval(i,j).ge.ylo).and.(yval(i,j).le.yhi)) then
			   nfit(j)=nfit(j)+1
			   yval(nfit,j)=yval(i,j)
			endif
		   enddo
		endif
c
	      if(usewfreq) then
		   call VARF1(Yval(1,j),wfreq,nfit(j),ybar,vary,
     &		sumw,ymin,ymax)
		   anfit=sumw*0.001		!0.001 used in SETBIN1 (for ms)
c anfit=effective nfit to be used for scaling fitted curve (NB must
c keep Nfit too for use in GAULIK etc).
		else
		   call VARV1(Yval(1,j),Nfit(j),YBAR,VARY,ymin,ymax)
		   anfit=-1.0		!signal not to use wfreq() in GAULIK
		endif
c
	   else		!for point amp histo
		sumw=0.
		sumy=0.
		sumyy=0.
		do i=ilow,ihigh
		   y=float(i)*calfac2
		   sumw=sumw+freqs(i)
		   sumy=sumy+freqs(i)*Y
		   sumyy=sumyy+freqs(i)*Y*Y
		enddo
		sumyy=sumyy - (sumy*sumy)/sumw
		if(sumw.gt.0.0) then
		   YBAR=sumy/sumw
		   VARY=sumyy/(sumw-1.0)
		else
		   vary=-1.
		endif
	   endif
	   if(vary.ge.0.) then
		sd=sqrt(vary)
	   else
		sd=-1.0
	   endif
c
	   if(pon1) write(7,301) NFIT(j),YBAR,sd
	   if(discprt) write(8,301) NFIT(j),YBAR,sd
	   if(graphic) then
		call INTCONV(nfit(j),cnum0)
		call REALTOCH(round1(ybar,2),cnum1,11)
		call REALTOCH(round1(s,3),cnum2,11)
		call WDIALOG(1,'Mean and SD of '//CHARNB(cnum0)
     &    //' values in range = '
     &    //CHARNB(cnum1)//', '//CHARNB(cnum2),ict)
	   else
		print 301,NFIT(j),YBAR,sd
301		FORMAT(
     &    ' Mean and SD of',I5,' values in fitted range (inc) = ',
     &    2g13.6)
	   endif
C
	   IF(AHIST) GOTO 3
	   if(id.eq.52) goto 3
	   if(pophist) goto 3	!fit gau to Popen dist
c===	   IF(OBHIST) GOTO 400	!now do obhist in same loop as thist
C
C SECTION FOR INIT GUESSES FOR TIME HISTOGRAMS
c NOW DO OBHIST IN SAME LOOP
93	   continue		!start again here if area>1
	   k=2*ncomp(j)-1	!total NO OF PARAMETERS for set j
	   call INTCONV(j,cnum0)
	   if(nset.eq.1) then
		if(graphic) then
		   call WDIALOG(1,'Initial guesses:',12)
		else
		   print 6
6		   FORMAT( ' Initial guesses:',/)
		endif
	   else if(nset.gt.1) then
		if(graphic) then
		   call WDIALOG(1,
     &		'Initial guesses for set # '//CHARNB(cnum0),12)
		else
		   print 61,j
61		   FORMAT( ' Initial guesses for set # ',i3,/)
		endif
	   endif
c First get the tau(i,j), area(i,j)
c (for obhist keep means in tau(i,j) locally)
c
	   S=0.
	   negpar=.false.
         do i=1,ncomp(j)
		if(graphic) then
		   call INTCONV(i,cnum0)
		   if(j.gt.1) then
			tau(i,j)=tau(i,1)		!default
			call DEFOLTr(tau(i,1),defolt)
		   else
			if(thist) then
			   tau(i,j)=0.01*(10**i)
			else if(obhist) then
			   tau(i,j)=1.01 + 3.*float(i-1)
			endif
			call DEFOLTr(tau(i,j),defolt)
		   endif
		   if(thist) then
		    call QDIALOG(1,
     &	     ' Tau('//CHARNB(cnum0)//') (ms)',defolt,ict,cans)
		   else if(obhist) then
		    call QDIALOG(1,
     &	     ' Mean openings/bst('//CHARNB(cnum0)//')',
     &		defolt,ict,cans)
		   endif
		   call GETINPr(cans,tau(i,j))
		   if(i.lt.ncomp(j)) then
			if(j.gt.1) then
			   area(i,j)=area(i,1)		!default
			else
			   area(i,j)=1.0/float(ncomp(j))
			endif
			call DEFOLTr(area(i,j),defolt)
		      call QDIALOG(1,
     &	      ' Fractional area of component '//CHARNB(cnum0),
     &  	      defolt,ict,cans)
		      call GETINPr(cans,area(i,j))
		   endif
		else
		   if(j.gt.1) then
			tau(i,j)=tau(i,1)		!default
		   else
			if(thist) then
			   tau(i,j)=0.01*(10**i)
			else if(obhist) then
			   tau(i,j)=1.01 + 3.*float(i-1)
			endif
		   endif
		   if(thist) then
		      print 7,i,tau(i,j)
7		      FORMAT('& Tau(',i2,') (ms) [',f9.4,'] = ')
		   else if(obhist) then
		      print 8,i,tau(i,j)
8		      FORMAT('& Mean openings/bst(',i2,') [',
     &			f9.4,'] = ')
		   endif
	         call INPUTr(tau(i,j))
		   if(obhist) then
			if(tau(i,j).le.1.0) tau(i,j)=1.01
		   endif
		   if(i.lt.ncomp(j)) then
			if(j.gt.1) then
			   area(i,j)=area(i,1)		!default
			else
			   area(i,j)=1.0/float(ncomp(j))
			endif
			print 9,i,area(i,j)
9			FORMAT(
     & 	     '& Fractional area of component ',i2, ' [',f7.4,'] = ')
	      	call INPUTr(area(i,j))
		   endif
		endif
		if(i.lt.ncomp(j)) then
		   S=S+area(i,j)
		   if(S.ge.1.0) then
			call BELL(2)
			if(graphic) then
			   call OPENDIALOG(2,12,.true.)		!draw dialog box #2
			   call WDIALOG(2,'AREA > 1',12)
			else
			   print 92
92			   FORMAT(
     &			' *** TOTAL AREA GREATER THAN ONE. TRY AGAIN'/)
			endif
			goto 93		!start areas again
		   endif
		endif
		if(area(i,j).lt.0.) negpar=.true.
	   enddo
	enddo		!END OF j=1,NSET
c
c Now allocate parameters to theta (and set parameter titles)
c For nset=1 they will be in same order as before
c tau=theta(1,3,5,7,9)
c area=theta(2,4,6,8)
c but for nset>1 some tau will be omitted
c When nset>1 now set the number of constraints
	if(nset.gt.1) then
	   if(neq.lt.0) neq=0
	   if(graphic) then
 		call DEFOLTi(neq,defolt)
		if(thist) then
		   call QDIALOG(1,'Number of tau values to be constrained',
     & 		 defolt,ict,cans)
		else if(obhist) then
		  call QDIALOG(1,'No. of ops/bst values to be constrained',
     & 		 defolt,ict,cans)
		endif
		call GETINPi(cans,neq)
	   else
		if(thist) then
		   print 11,neq
11	         FORMAT('&Number of tau values to be constrained ['
     &		,i2,'] = ')
		else if(obhist) then
		   print 16,neq
16	         FORMAT(
     &	   '&Number of mean ops/bst values to be constrained ['
     &		,i2,'] = ')
		endif
		call INPUTi(neq)
	   endif
c now set the constraints
	   do n=1,neq
		i=1
		j=2
		i0=1
		j0=1
114		if(readini.and.ieq(n).gt.0) then
		   i=ieq(n)
		   j=jeq(n)
		endif
		if(graphic) then
		   call INTCONV(n,cnum0)
		   call WDIALOG(1,'Constraint '//charnb(cnum0),12)
 		   call DEFOLT2i(i,j,defolt)
		   if(thist) then
		      call QDIALOG(1,'Constrain tau(i) in set j: i,j ',
     & 		 defolt,ict,cans)
		   else if(obhist) then
		      call QDIALOG(1,'Constrain ops/bst(i) in set j: i,j ',
     & 		 defolt,ict,cans)
		   endif
		   call GETINP2i(cans,i,j)
		   ieq(n)=i
		   jeq(n)=j
		   if(readini.and.ieq(n).gt.0) then
			i0=ieq0(n)
			j0=jeq0(n)
		   endif
		   call INTCONV(i,cnum0)
		   call INTCONV(j,cnum1)
		   call INTCONV(i0,cnum2)
		   call INTCONV(j0,cnum3)
 		   call DEFOLT2i(i0,j0,defolt)
		   if(thist) then
		     call QDIALOG(1,'Constrain tau('//charnb(cnum0)//
     &		') in set '//charnb(cnum1)//
     &	   	' = tau('//charnb(cnum2)//
     &		') in set '//charnb(cnum3)//': i,j',
     & 		 defolt,ict,cans)
		   else if(obhist) then
		     call QDIALOG(1,'Ops/bst('//charnb(cnum0)//
     &		') in set '//charnb(cnum1)//
     &	   	' = ops/bst('//charnb(cnum2)//
     &		') in set '//charnb(cnum3)//': i,j',
     & 		 defolt,ict,cans)
		   endif
		   call GETINP2i(cans,i0,j0)
		   ieq0(n)=i0
		   jeq0(n)=j0
		   call INTCONV(i0,cnum2)
		   call INTCONV(j0,cnum3)
		   ans='Y'
		   call DEFOLTa(ans,defolt)
		   if(thist) then
		      call QDIALOG(1,'tau('//charnb(cnum0)//
     &		') in set '//charnb(cnum1)//
     &	   	' = tau('
     & 		//charnb(cnum2)//') in set '//charnb(cnum3),
     & 		 defolt,ict,cans)
		   else if(obhist) then
		      call QDIALOG(1,'Ops/bst('//charnb(cnum0)//
     &		') in set '//charnb(cnum1)//
     &	   	' = ops/bst('
     & 		//charnb(cnum2)//') in set '//charnb(cnum3),
     & 		 defolt,ict,cans)
		   endif
		   call GETINPa(cans,ans)
		   if(ans.eq.'N') goto 114
		else
		   if(thist) then
		      print 111,i,j
111		      format(' Constrain tau(i) in set j; i,j  ['
     &			,i2,',',i2,'] = ')
		   else if(obhist) then
		      print 17,i,j
17		      format(' Constrain ops/bst(i) in set j; i,j  ['
     &			,i2,',',i2,'] = ')
		   endif
		   call INPUT2i(i,j)
		   ieq(n)=i
		   jeq(n)=j
		   if(readini.and.ieq(n).gt.0) then
			i0=ieq0(n)
			j0=jeq0(n)
		   endif
		   if(thist) then
		      print 112,i,j,i0,j0,i0,j0
112		      format(' Constrain tau(',i2,') in set ',i2,
     &	   	' to be equal to tau(',i2,') in set '
     &			,i2,'; i,j [',i2,',',i2,'] = ')
		   else if(obhist) then
		      print 18,i,j,i0,j0,i0,j0
18		      format(' Constrain ops/bst(',i2,') in set ',i2,
     &	   	' to be equal to ops/bst(',i2,') in set '
     &			,i2,'; i,j [',i2,',',i2,'] = ')
		   endif
		   call INPUT2i(i0,j0)
		   ieq0(n)=i0
		   jeq0(n)=j0
		   if(thist) then
		      print 113,i,j,i0,j0
113		      format(' tau(',i2,') in set ',i2,
     &		' is constrained to be equal to tau(',
     &		i2,') in set ',i2,': OK [Y] ? ')
		   else if(obhist) then
		      print 191,i,j,i0,j0
191		      format(' ops/bst(',i2,') in set ',i2,
     &		' is constrained to be equal to ops/bst(',
     &		i2,') in set ',i2,': OK [Y] ? ')
		   endif
		   ans='Y'
		   call INPUTa(ans)
		   if(ans.eq.'N') goto 114
		endif
	   enddo
	endif	!end of if(nset.gt.1)
c Print constraints
	if(neq.gt.0) then
	   if(discprt) write(8,146)
146		format(/,' CONSTRAINTS')
		do n=1,neq
		if(thist) then
		   if(discprt) write(8,144) ieq(n),jeq(n),ieq0(n),jeq0(n)
144		   format(' tau(',i2,') in set ',i2,
     &		' is constrained to be equal to tau(',
     &		i2,') in set ',i2)
		else if(obhist) then
		      if(discprt) write(8,144) ieq(n),jeq(n),ieq0(n),jeq0(n)
145		      format(' ops/bst(',i2,') in set ',i2,
     &		' is constrained to be equal to ops/bst(',
     &		i2,') in set ',i2)
		endif
	   enddo
	else if(nset.gt.1) then
	   if(discprt) write(8,147)
147	   format(/,' NO CONSTRAINTS')
	endif
c
c Now construct theta for initial guesses, and keep array that indicates
c where missing (constrained) theta values are
c Also define itcon(i,j) for all tau(i,j), as above
	m=0
	do j=1,nset
	   do i=1,ncomp(j)
		if(neq.gt.0) then	!check if param i,j is constrained
		   do n=1,neq
			if(i.eq.ieq(n).and.j.eq.jeq(n)) then
			   itcon(i,j)=0
			   goto 21
			endif
		   enddo
		endif
		m=m+1
		theta(m)=tau(i,j)
		itcon(i,j)=m
		call INTCONV(i,cnum1)
		if(nset.eq.1) then
		  if(thist) then
		   titlep(m)='  tau('//charnb(cnum1)//')'
		  else if(obhist) then
		   titlep(m)=' ops/bst('//charnb(cnum1)//')'
		  endif
		else
		   call INTCONV(j,cnum2)
		  if(thist) then
		   titlep(m)='tau('//charnb(cnum1)//','//charnb(cnum2)//')'
		  else if(obhist) then
		   titlep(m)='ops/bst('//charnb(cnum1)//','//
     &		charnb(cnum2)//')'
		  endif
		endif
		itloc(m)=0 		!theta(m) is a tau
21		continue		!next param=area if a tau was constrained
		if(i.lt.ncomp(j)) then
		   m=m+1
		   theta(m)=area(i,j)
		   call INTCONV(i,cnum1)
		   if(nset.eq.1) then
			titlep(m)=' area('//charnb(cnum1)//')'
		   else
			call INTCONV(j,cnum2)
			titlep(m)='area('//charnb(cnum1)//','//
     &			charnb(cnum2)//')'
		   endif
		   ialoc(i,j)=m
		   itloc(m)=1		!theta(m) is an area
		   if(i.eq.ncomp(j)-1) itloc(m)=-1	!theta(m) is last free area
		endif
	   enddo
	enddo
	if(nset.gt.1) k=m		!total number of free parameters
c
	GOTO 4		!INIT GUESSES SET
c
3	CONTINUE
C NOW SECTION FOR AMPLITUDE HISTOGRAM GUESSES
c (also for fitting Gaussian to Popen/bst if pophist=true)
C ORDER OF PARAMETERS IS MEAN(1),SD(1),AREA(1),MEAN(2),SD(2)....
C ..SD(NCOMP),AREA(NCOMP)
c Only done for nset=1 so set j=1
	K=3*NCOMP(1)-1
	isdcon=1		!default
	if(ncomp(1).gt.1) then
	   if(graphic) then
 		call DEFOLTi(isdcon,defolt)
		call QDIALOG(1,
     &  '(1) Fit all SD; (2) SD=const*mean; (3) All SD equal;',
     & 	  defolt,ict,cans)
		call GETINPi(cans,isdcon)
	   else
	   print 57,1
57	      FORMAT(
     &	' (1) Fit all the SD freely',/,
     &	' (2) Force SD to be constant * mean',/,
     &	' (3) Make all SD the same',/,
     &	' Option number [',i2,'] = ')
	      call INPUTi(isdcon)
	   endif
	   if(isdcon.ne.2.and.isdcon.ne.3) isdcon=1
	   if(isdcon.ne.1) then
		k=k-(ncomp(1)-1)
	   endif
	endif
C
94	S=0.
	J=0
	theta(3)=1.0	!AREA OF COMPONENT 1 UNLESS REDEFINED
	DO 35 I=1,NCOMP(1)
	J=J+1
	if(graphic) then
	   call INTCONV(j,cnum1)
	   call INTCONV(i,cnum2)
	   if(pophist) then
	      call QDIALOG(1,'(Param '//CHARNB(cnum1)//
     &    '): Mean Popen('//CHARNB(cnum2)//')',' ',ict,cans)
	   else
	      call QDIALOG(1,'(Param '//CHARNB(cnum1)//
     &    '): Mean amplitude('//CHARNB(cnum2)//') (pA)',' ',ict,cans)
	   endif
	   call GETINPr(cans,theta(j))
	   titlep(j)='mean amplitude('//charnb(cnum2)//')'
	else
	   if(pophist) then
		print 391,J,I
391		format('&Param(',I2,')  Mean Popen(',I2,') = ')
	   else
		print 39,J,I
39		format('&Param(',I2,')  Mean amp(',I2,') (pA)= ')
	   endif
         call INPUTr(theta(j))
	endif
	if(i.gt.1.and.k.lt.3*ncomp(1)-1) goto 371		!SD equal
	J=J+1
	if(graphic) then
	   call INTCONV(j,cnum1)
	   if(pophist) then
		call QDIALOG(1,'(Param '//CHARNB(cnum1)//
     &    '): Standard dev('//CHARNB(cnum2)//')',' ',ict,cans)
	   else
		call QDIALOG(1,'(Param '//CHARNB(cnum1)//
     &    '): Standard dev('//CHARNB(cnum2)//') (pA)',' ',ict,cans)
	   endif
	   call GETINPr(cans,theta(j))
	   titlep(j)='standard dev('//charnb(cnum2)//')'
	else
	   print 37,J,I
37	   FORMAT('&Param(',I2,') Standard dev(',I2,')= ')
        call INPUTr(theta(j))
	endif
	if(isdcon.eq.2) then    !theta(2)=sdfac in this case SD=sdfac*mean
	   theta(j)=theta(j)/theta(j-1)	!=sd/mean for 1st component
	endif
c
371	IF(NCOMP(1).EQ.1.OR.I.EQ.NCOMP(1)) GOTO 35
	J=J+1
	if(graphic) then
	   call INTCONV(j,cnum1)
	   call QDIALOG(1,'(Param '//CHARNB(cnum1)//
     &    '): Fraction with amplitude '//CHARNB(cnum2),' ',ict,cans)
	   call GETINPr(cans,theta(j))
	   titlep(j)='Fraction with amp'//charnb(cnum2)//')'
	else
	   print 38,J,I
38	   FORMAT('&Param(',I2,'): Fraction with amplitude(',I2,')= ')
         call INPUTr(theta(j))
	endif
	S=S+theta(J)
c	IF(S.LT.1.0) GOTO 35	!OK
	if(s.ge.1.0) then
	   call BELL(2)
	   if(graphic) then
	      call OPENDIALOG(2,12,.true.)		!draw dialog box #2
	      call WDIALOG(2,'AREA > 1',12)
	   else
	     print 92		! *** TOTAL AREA GREATER THAN ONE. TRY AGAIN
	   endif
	   goto 94
	endif
35	CONTINUE
C
	IF(NCOMP(1).EQ.1) GOTO 4
c	theta(3*NCOMP(1))=1.0-S		!=AREA(NCOMP)
	theta(j+1)=1.0-S		!=AREA(NCOMP)
	call INTCONV(ncomp,cnum2)
	titlep(j+1)='area('//charnb(cnum2)//')'
c	GOTO 4
C
c
4	CONTINUE
	ERRFAC=1.0E-4		!DEFAULT
	FITTED=.TRUE.
	NFIX=0
	ndisp=20
	do j=1,50
	   jfix(j)=0	!INITIALISE
	enddo
	if(graphic) then
	   ans='N'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,'Fix any parameters',
     &     defolt,ict,cans)
	   call GETINPa(cans,ans)
	else
	   print 41
41	   format('&Fix any parameters [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	endif
	if(UC(ans).eq.'Y') then
	   if(graphic) then
		call QDIALOG(1,' Number to be fixed [0]',
     &	' ',ict,cans)
		call GETINPi(cans,nfix)
	   else
	      print 411
411		format('& -how many: n= ')
		call INPUTi(nfix)
	   endif
	   do i=1,nfix
		if(graphic) then
		   call INTCONV(i,cnum1)
		   call QDIALOG(1,' '//CHARNB(cnum1)//
     & 	    ': Fix parameter #',' ',ict,cans)
		   call GETINPi(cans,j)
		else
		   print 43,i
43		   FORMAT('&',1x,i3,'. Parameter number= ')
		   call INPUTi(j)
		endif
		if(pon1) write(7,431) j,theta(j)
		if(discprt) write(8,431) j,theta(j)
431		format(' Param no ',i3, ' fixed at ',g13.6)
		JFIX(J)=1
	   enddo
	endif
C
42	KFIT=K-NFIX	!NUMBER OF PARAM TO BE FITTED
c
c suppress graph after guesses in
c	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
c	call LOCATE(0,0)
cc NOW IN ALPHA MODE -now below!
c
	AMPOS=.TRUE.
	do j=1,nset
	   if(ncomp(j).gt.1)  goto 143
	enddo
	goto 142 	!if all ncomp=1
c	IF(NCOMP.EQ.1) GOTO 142
143	if(negpar) then
	   ans='N'
	else
	   ans='Y'
	endif
	call DEFOLTa(ans,defolt)
	call QDIALOG(1,'Constrain areas to be positive',
     &  defolt,ict,cans)
	call GETINPa(cans,ans)
	if(ans.eq.'N') AMPOS=.FALSE.
	if(pon1.and.ampos) write(7,3021)
	if(discprt.and.ampos) write(8,3021)
3021	format(/,' Areas constrained to be positive',/)
	PEN=0.
	IF(AMPOS.or.(.not.THIST)) GOTO 142
c Now section to impose constraints that are necessary when there
c are negative areas.
c  Two options now:
c  (1) Constrain f(0) to be non-negative (this can give crashes if f(t)
c	  becomes negative at later t, e.g. if area between tlow, thigh
c	  becomes negative
c  (2) Constrain f(0)=0 AND initial slope to be non-negative (new option:
c	 probably more useful/safe in cases where f(0)=0 looks OK)
c
1431  continue
	strings(1)='Constrain f(0)=0 and f''(0)=>0'
	strings(2)='cOnstrain f(0)=>0'
	strings(3)='conStrain f(0)=>0 and f''(0)=>0'
	strings(4)='No constraints'
	nval=4
	iline=1		!default
	helps(1)=
     &   ' If some areas are negative the p.d.f. could become'
	helps(2)=
     &   ' negative which is not allowed! The p.d.f. at t=0, f(0),'
	helps(3)=
     &   ' can be fixed to 0 (which reduces the number of free'
	helps(4)=
     &   ' parameters by 1), or it can be constrained to be'
	helps(5)=
     &   ' non-negative by use of a penalty factor.  The initial'
	helps(6)=
     &   ' slope, f''(0), can also be constrained to be'
	helps(7)=
     &   '   non-negative by use of a penalty factor'
	nhelp=7
152	call BELL(2)
	call POPMENU(nxlo,nylo,nyhi,strings,nval,ictm,icfm,icupm,
     &   ibkm,'What constraint?',helps,nhelp,iline,ans,ival)
	if(iline.eq.0) goto 152			!iline=0 for ESC=cancel
	if(iline.ge.1.and.iline.le.nval) then
	   iconst=iline
	   if(iconst.eq.4) iconst=0	!no constraint
	endif
	if(iconst.eq.0) goto 142
	pen=10.
	call DEFOLTr(pen,defolt)
	call QDIALOG(1,
     &  'Penalty constant for constraint (F1=help)',
     & 	  defolt,ict,cans)
	if(help) then
	   helps(1)=
     &   ' If some amplitudes are negative the p.d.f. could become'
	   helps(2)=
     &   ' negative which is not allowed! The p.d.f. or slope at t=0'
	   helps(3)=
     &   ' can be forced to be non-negative by artificially decreasing'
	   helps(4)=
     &   ' the likelihood steeply if it goes negative. The steepness'
	   helps(5)=
     &   ' depends on the penalty factor (e.g.try 1.and 10. and 50.)'
	   call POPTEXT(mxlo,mylo,myhi,helps,5,ictx,ibkx,icfx)
	   goto 1431
	else
	   call GETINPr(cans,pen)
	endif
c	if(pen.gt.0.) goto 1422
c	print 1421
c1421	format(
c     & ' If some amplitudes are negative the p.d.f. could become',/,
c     & ' negative which is not allowed! The p.d.f. or slope at t=0',/,
c     & ' can be forced to be non-negative by artificially decreasing',/,
c     & ' the likelihood steeply if it goes negative. The steepness',/,
c     & ' depends on the penalty factor: try 1.and 10. and 50.')
c	goto 1431
c1422	continue
	if(iconst.eq.1) then
	   if(pon1) write(7,1423) pen
	   if(discprt) write(8,1423) pen
1423	   format(
     &  ' Constrained so f(0)=0 and f''(0)=>0: penalty factor = ',g13.6)
	else if(iconst.eq.2) then
	   if(pon1) write(7,1424) pen
	   if(discprt) write(8,1424) pen
1424	   format(
     &  ' Constrained so f(0)=>0: penalty factor = ',g13.6)
	else if(iconst.eq.3) then
	   if(pon1) write(7,1425) pen
	   if(discprt) write(8,1425) pen
1425	   format(
     & ' Constrained so f(0)=>0 and f''(0)=>0: penalty factor = ',g13.6)
	else if(iconst.eq.0) then
	   if(pon1) write(7,1426)
	   if(discprt) write(8,1426)
1426	   format(
     & ' Not constrained ')
	endif
142	continue
c
c suppress graph
	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
	call LOCATE(0,0)
c NOW IN ALPHA MODE
	print 311,errfac
311	FORMAT(' ERROR=errfac*initial guess: [',f9.6,'] errfac = ')
      call INPUTr(errfac)
	irestrt=3
	print 312,irestrt
312	FORMAT(' Limit number of restarts to [',i2,']: n = ')
	call INPUTi(irestrt)
	print 15
15	FORMAT(' Type every Nth iteration [20]: N = ')
	call INPUTi(ndisp)
      print 32
      if(discprt) write(8,32)
32    FORMAT(' Initial guesses: ')
      do 12 i=1,k
      print 13,i,theta(i)
12    if(discprt) write(8,13) i,theta(i)
13    FORMAT(I8,G13.6)
	print 108
c	if(pon1) write(7,108)
	if(discprt) write(8,108)
C
	IF(.NOT.debug()) GOTO 31
342   print 340
340   FORMAT(' List some values of variable between YLO,YHI [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).EQ.'N') GOTO 31
	if(nset.gt.1) then
	    print 3422
3422	   format(' Print for set # = ')
	   call INPUTi(j)
	   if(j.lt.1.or.j.gt.nset) j=1
	else
	   j=1
	endif
	n1=1
	n2=10
	print 3421
3421	FORMAT(' Between values N1,N2= ')
	call INPUT2i(n1,n2)
	DO 33 I=N1,N2
c	if(pon()) write(7,13) I,YVAL(I,j)
	if(pon1) write(7,13) I,YVAL(I,j)
	if(discprt) write(8,13) I,YVAL(I,j)
33	print 13,I,YVAL(I,j)
	print 108
c	if(pon()) write(7,108)
	if(pon1) write(7,108)
	if(discprt) write(8,108)
	GOTO 342
31	CONTINUE
c
c  (start of code from old CSIMP)
	stpfac=0.1	!factor for step size (set in SIMP5V)
c ERRFAC was set in MLFITS)
c
c	DO 1 I=1,K
c	STEP(I)=0.1*theta(I)
c	CRTSTP(I)=ERRFAC*theta(I)
c1	CONTINUE
C
C LMIN=-LMAX
	nevmax=-30000	!neg so iterations not printed to disc
	confac=0.5		!parameter for simplex3
	if(ahist.or.idtype.eq.14.or.idtype.eq.52) then	!fit gau to ahist or Popen dist
	   nevmax=-30000	!neg so iterations not printed to disc
	   if(pahist) then
		call SIMPLEX3(k,theta,stpfac,errfac,nev,nevmax,
     & 	  elmin,GAUFIT,ndisp,jfix,-1.,confac,irestrt,iconv)
	   else
		call SIMPLEX3(k,theta,stpfac,errfac,nev,nevmax,
     & 	  elmin,GAULIK,ndisp,jfix,-1.,confac,irestrt,iconv)
	   endif
	else if(obhist) then	!fit geometrics
	   call SIMPLEX3(k,theta,stpfac,errfac,nev,nevmax,
     &     elmin,OBLIK,ndisp,jfix,-1.,confac,irestrt,iconv)
	else
	   call SIMPLEX3(k,theta,stpfac,errfac,nev,nevmax,
     &     elmin,EXPLIK,ndisp,jfix,-1.,confac,irestrt,iconv)
	endif
C
	print 19
19	FORMAT( ' End of fitting')
	print 20,(theta(i),i=1,k)
20	format(4(5g13.6,/))
c	IF(K.LE.5) print 20,(THETA(I),I=1,K)
c20	FORMAT(1X,5G13.6)
c	IF(K.GT.5) print 21,(THETA(I),I=1,K)
c21	FORMAT(1X,5G13.6,/,5G13.6)
	ELMAX=-ELMIN
	print 942,NEV,ELMAX
	if(pon1) write(7,942) NEV,ELMAX
	if(discprt) write(8,942) NEV,ELMAX
942	FORMAT(i5,' function evaluations.',/,
     & '  Maximum log(likelihood) = ',G13.6)
	if(nset.gt.1) then
	   do j=1,nset
		print 90,j,setlik(j)
		if(discprt) write(8,90) j,setlik(j)
90		FORMAT(' Likelihood for set ',i3,' separately = ',g13.6)
	   enddo
	   errors=.false.
	else
	   errors=.true.
	   ans='N'
	   call DCASK('Calculate errors',ans,ans)
	   if(UC(ans).ne.'Y') errors=.false.
	endif
	RETURN
	END




