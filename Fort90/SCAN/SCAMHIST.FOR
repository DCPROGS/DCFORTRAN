	subroutine SCAMHIST(imin,imax,ifreq1,ifreq2,ifreq3,
     &  icw,icw1,opendown,ifmin,ifmax,allpoint)
c
c  To display point amplitude histograms in SCAN
c
c Modif 06/07/98 01:05pm to allow fitting in case of allpoint histo only
c (in which case don't write to scandat, so can't fit in ekdist).
c
c	ndat(j)=nbin.
c	LO bin is from Xval(0,j) to Xval(1,j); freq in Yval(0,j)
c	First bin is from Xval(1,j) to Xval(2,j); freq in Yval(1,j)
c	ith bin is from Xval(i,j) to Xval(i+1,j); freq in Yval(i,j)
c	last bin is from Xval(nbin,j) to Xval(nbin+1,j); freq in Yval(nbin,j)
c	HI bin is from Xval(nbin+1,j) to Xval(nbin+2,j); freq in Yval(nbin+1,j)
c
c Modif 11/14/97 02:09pm for VHIST5 (can allocate xval etc to any
c  size now, but still fixed at old values for now)
c
	integer*4 ifreq1(imin:imax),ifreq2(imin:imax),ifreq3(imin:imax)
	integer*4 ifmin(4),ifmax(4)
	logical landscap,fitted,autplt,draft,doframe,logt,binset,posamp
	logical opendown,allpoint
	logical discprt
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character*40 titlex,titley
	character*64 title1
	character UC*1,ans*1
	character cnum0*11,cnum*11,cnum1*11,cnum2*11,cnum3*11,text*60
c  For allpoint mode
	real*4 freqs(-5000:5000)		!in common/pablk/ for GAUFIT
	logical graphic,revamp,excreg,ampos,debug,caplock
	REAL THETA(50)		!PARAM
	INTEGER JFIX(50)
	character*20 TITLEP(50)		!names of params for fit
	real*4 AREA(10),AMEAN(10),SD(10)
	common/FIXBLK/nfix,jfix
	COMMON/FITBLK/Ylow,Yhigh,Ncomp,Nfit,ampos,excreg,
     &  yexclow,yexchigh,isdcon,anfit
	common/PABLK/freqs,calfac2,ilow,ihigh		!for GAUFIT,MLFITS
	external gaufits
c
c for VHIST
	allocatable:: xval,yval,xcal,ycal
	real*4 xval(:,:),yval(:,:),xcal(:,:),ycal(:,:)
c	real XVAL(0:511,1),YVAL(0:511,1)
c	real*4 XCAL(2048,1),YCAL(2048,1)
c for histo data
	integer ndat(1),icurvd(1),ijoin(1)
c for calc curves
	integer ncal(1),icurvc(1),iline(1)
c
	common/dp/discprt
c
	debug()=caplock()
c
101	format(a1)
	binset=.false.
c Modif 11/14/97 02:09pm for VHIST5 (can allocate xval etc to any
c  size now, but still fixed at old values for now)
	ndv1=511	!dimension for xval,yval
	ndc1=2048	!dimension for xcal, ycal
	kmax=50	!dimension for theta, jfix
	posamp=.true.		!plot absolute amplitude
	fitted=.false.
c	real XVAL(0:511,1),YVAL(0:511,1)
c	real*4 XCAL(2048,1),YCAL(2048,1)
	ALLOCATE(xval(0:511,1),yval(0:511,1))
	ALLOCATE(xcal(2048,1),ycal(2048,1))
c
c Find largest and smallest non-zero entries in ifreq()
	call GETIFMAX(imin,imax,ifreq1,ifreq2,ifreq3,ifmin,ifmax)
c
	if(allpoint) then
	   iplot=1
	   goto 12
	endif
c
	if(ifmin(3).lt.ifmin(2)) then
	   ifmin(4)=ifmin(3)
	else
	   ifmin(4)=ifmin(2)
	endif
	if(ifmax(3).gt.ifmax(2)) then
	   ifmax(4)=ifmax(3)
	else
	   ifmax(4)=ifmax(2)
	endif
c	cmax=float(imax)*calfac2    !max pA
c	cmin=float(imin)*calfac2    !min pA
c	irange=imax-imin+1
	iplot=4
c	print 11,iplot
c11	format(
c     & ' (1) All point amplitude histogram',/,
c     & ' (2) Shut point amplitude histogram',/,
c     & ' (3) Open point amplitude histogram',/,
c     & ' (4) Open and shut point amplitude histograms',/,
c     & ' Option number [',i2,'] = ')
c	call INPUTi(iplot)
	call WDIALOG(1,
     &'(1) All point amplitude histogram; (2) Shut point amp histogram',
     &  icw)
	call WDIALOG(1,
     &'(3) Open point amplitude histogram; (4) Shut and open point',
     &  icw)
	call DEFOLTi(iplot,defolt)
	call QDIALOG(1,'Option number',defolt,icw,cans)
	call GETINPi(cans,iplot)
12	continue		!return for next histo
c
c defaults
c	nbw=1
c	ndat(j)=nbin.
c	LO bin is from Xval(0,j) to Xval(1,j); freq in Yval(0,j)
c	First bin is from Xval(1,j) to Xval(2,j); freq in Yval(1,j)
c	ith bin is from Xval(i,j) to Xval(i+1,j); freq in Yval(i,j)
c	last bin is from Xval(nbin,j) to Xval(nbin+1,j); freq in Yval(nbin,j)
c	HI bin is from Xval(nbin+1,j) to Xval(nbin+2,j); freq in Yval(nbin+1,j)
c  Use exact number of ifreq elements per bin
c Set bin width (default=0.1 pA, as near as pos). In ifreq(), one element
c =1 intermed unit = calfac2 pA, so 1/calfac2 elements per pA ,so 0.1pA is
c approx idelt elements
	idelt=ifixr(0.1/calfac2)	!approx # of elements for 0.1 pA
	dx=float(idelt)*calfac2		!exact pA per bin (i.e. per idelt elements)
	xwbase=dx		!if only one bin width
c
13	continue	!return here with binset=true if binwidth etc reset
c  (re)set xmin every time so get correct xval(1,1) (values of xmin,xmax for
c display are set below, or defined from xmin1,xmax1 when binset=true)
	xmin=float(ifmin(iplot))*calfac2    !min pA
	xmax=float(ifmax(iplot))*calfac2    !max pA
	ans='Y'
	call DEFOLTa(ans,defolt)
	call QDIALOG(1,'Reverse the sign of amplitudes',defolt,icw,cans)
	call GETINPa(cans,ans)
	revamp=ans.eq.'Y'
c	if(posamp.and.opendown) then		!reverse sign
	if(revamp) then				!reverse sign
	   if=ifmax(iplot)		!index for ifreq
	   inc=-1		!work back from ifreq(ifmax) down to ifreq(ifmin)
	   x=xmax		!save
	   xmax=-xmin
	   xmin=-x
	else
	   if=ifmin(iplot)		!index for ifreq
	   inc=1		!work up from ifreq(ifmin) up to ifreq(ifmax)
	endif
c
	nbin=1 + ifix((xmax-xmin)/dx)       !round up
c Last ifreq() element referred to will be if0+inc*(nbin*idelt-1)
c where inc=1 or -1. If this goes beyond the declared dimension of ifreq
c must have narrower bin at end, or put extra values in overflow bin
c Do latter for now
17	ilast=if+inc*(nbin*idelt-1)
	if(inc.eq.1) then
	   if(ilast.gt.imax) then
		nbin=nbin-1
		goto 17
	   endif
	else if(inc.eq.-1) then
	   if(ilast.lt.imin) then
		nbin=nbin-1
		goto 17
	   endif
	endif
c  If nbin has been reduced above, may have some values beyond last bin
c	HI bin is from Xval(nbin+1,j) to Xval(nbin+2,j); freq in Yval(nbin+1,j)
c set NO lo/hi bin
	xval(0,1)=xval(1,1)-dx		!lo bin
	yval(0,1)=0.0			!lo bin
	xval(nbin+2,1)=xval(nbin+1,1)+dx	!hi bin
	yval(nbin+1,1)=0.0		!hi bin
	if(inc.eq.1) then
	   if(iplot.eq.1) then
		do i=ilast,imax
		   yval(nbin+1,1)=yval(nbin+1,1)+float(ifreq1(i))	!hi bin
		enddo
	   else if(iplot.eq.2) then
		do i=ilast,imax
		   yval(nbin+1,1)=yval(nbin+1,1)+float(ifreq2(i))	!hi bin
		enddo
	   else if(iplot.eq.4) then
		do i=ilast,imax
		   yval(nbin+1,1)=yval(nbin+1,1)+float(ifreq3(i))	!hi bin
		enddo
	   else if(iplot.eq.4) then
		do i=ilast,imax
		   yval(nbin+1,1)=yval(nbin+1,1)+float(ifreq2(i)+ifreq3(i))
		enddo
	   endif
	else if(inc.eq.-1) then
	   if(iplot.eq.1) then
		do i=ilast,imin,-1
		   yval(0,1)=yval(0,1)+float(ifreq1(i))	!lo bin
		enddo
	   else if(iplot.eq.2) then
		do i=ilast,imin,-1
		   yval(0,1)=yval(0,1)+float(ifreq2(i))	!lo bin
		enddo
	   else if(iplot.eq.3) then
		do i=ilast,imin,-1
		   yval(0,1)=yval(0,1)+float(ifreq3(i))	!lo bin
		enddo
	   else if(iplot.eq.4) then
		do i=ilast,imin,-1
		   yval(0,1)=yval(0,1)+float(ifreq2(i)+ifreq3(i))
		enddo
	   endif
	endif
c
	xval(1,1)=xmin
	ymax=0.0
	do i=1,nbin
	   xval(i+1,1)=xval(1,1) + float(i)*dx
	   itot=0
	   if(iplot.eq.1) then
		do i1=1,idelt
	         itot=itot + ifreq1(if)
		   if=if+inc
		enddo
	   else if(iplot.eq.2) then
		do i1=1,idelt
	         itot=itot + ifreq2(if)
		   if=if+inc
		enddo
	   else if(iplot.eq.3) then
		do i1=1,idelt
	         itot=itot + ifreq3(if)
		   if=if+inc
		enddo
	   else if(iplot.eq.4) then
		do i1=1,idelt
	         itot=itot + ifreq2(if) + ifreq3(if)
		   if=if+inc
		enddo
	   endif
	   yval(i,1)=float(itot)
	   if(yval(i,1).gt.ymax) ymax=yval(i,1)
	enddo
c
	ilog=0
	logt=ilog.eq.6		!plot sqrt(y) vs log(t)
	xlo=-1
c
	ymin=0.0
	ymax=ymax*1.2	!max for display
	ymax=float(1+ifix(ymax))	!ROUND UP
	ytic=0.5*10.0**ifix(alog10((ymax-ymin)*0.5))
2102	if((ymax-ymin)/ytic.gt.10.) goto 2101
	ytic=ytic/2.
	goto 2102	!ensure at least 5 tics so get numerical label
2101	continue
	if(.not.logt) ytic=ytic*5.	!smaller tics for sqrt scale for now
	if(binset) then	!skip questions
	   xmin=xmin1	!restore value set below for display
	   xmax=xmax1	!restore value set below
	   binset=.false. !reset
	   goto 14
	endif
c round xmin down to nearest 1 pA for start point of histogram
	xmax=xmin+float(nbin-1)*dx		!xmax adj to nbin just defined
	xmax=float(ifix(xmax)+1)		!nearest pA above
c	xmin=0.1*float(ifix(10.*xmin)-1)	!nearest 0.1pA
	xmin=float(ifix(xmin)-1)		!nearest pA below
c	xtic=2.*xwbase	!number every 10th bin
c# For Lahey/Hgraph xtic refers to major, not minor tics so make
c bigger ( say 2-fold rather than 5-fold for now?- 5-fold faster!)
c	xtic=xtic*5.
	xtic=1.0		!major tics=1pA
c	print 12,nbin,dx,xmin,xmax
c12	format(1x,i3,
c     & ' bins, width ',f9.3,' pA, from ',f9.3,' to ',f9.3,
c     & ' pA: O.K. [Y] ')
c	read 101,ans
16	continue
     	call INTCONV(nbin,cnum)
	call DCFORMAT(dx,7,2,cnum1)
	call DCFORMAT(xmin,7,2,cnum2)
	call DCFORMAT(xmax,7,2,cnum3)
	text=charnb(cnum)//' bins, width '//charnb(cnum1)//'pA, from'
	n=nblank(text)
	text=text(1:n)//charnb(cnum2)//' to '//charnb(cnum3)
	call WDIALOG(1,text,icw)
      ans='Y'
      call DEFOLTa(ans,defolt)
      call QDIALOG(1,'O.K.',defolt,-icw,cans)
	call GETINPa(cans,ans)
	if(UC(ans).eq.'N') then
         ans='Y'
         call DEFOLTa(ans,defolt)
         call QDIALOG(1,'Plot absolute amplitudes',defolt,-icw,cans)
	   call GETINPa(cans,ans)
	   posamp=ans.eq.'Y'
	   xmin=float(ifmin(iplot))*calfac2    !min pA -restore orig
	   xmax=float(ifmax(iplot))*calfac2    !max pA
	   xmin=float(ifix(xmin)-1)		!nearest pA below
	   xmax=xmin+float(nbin-1)*dx		!xmax adj to nbin just defined
	   xmax=float(ifix(xmax)+1)		!nearest pA above
	   if(posamp.and.opendown) then
		xmin1=-xmax		!defolt (xmin,xmax will be reversed later)
		xmax1=-xmin
	   else
		xmin1=xmin
		xmax1=xmax
	   endif
c    NB xmin,xmax here are for display (reset above to recalc histo) so
c    save values input here as xmin1,xmax1 and restore after histo calcs
	   call DEFOLTr(xmin1,defolt)
	   call QDIALOG(1,'Histogram to start at (pA)',defolt,icw,cans)
	   call GETINPr(cans,xmin1)
	   call DEFOLT2r(dx,xmax1,defolt)
	   call QDIALOG(1,'Bin width, Last x value (pA)',defolt,icw,cans)
	   call GETINP2r(cans,dx,xmax1)
	   idelt=ifixr(dx/calfac2)	!approx # of elements for 0.1 pA
	   dx=float(idelt)*calfac2	!exact pA per bin (i.e. per idelt elements)
	   xwbase=dx		!if only one bin width
	   call DCFORMAT(dx,9,5,cnum1)
c need Qdialog here, otherwise dialog box cleared before next line can be read
         ans='Y'
         call DEFOLTa(ans,defolt)
         call QDIALOG(1,'Bin width reset to '//charnb(cnum1)//' pA',
     &	defolt,-icw,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'N') goto 16
	   binset=.true.
	   goto 13		!recalculate for new bins
	endif
c
14	continue
	if(discprt) then
	   if(discprt) write(8,15) nbin,dx,xmin,xmax
15	   format(
     & ' Point amplitude histogram: ',i4,' bins, width ',f9.5,' pA',/,
     & ' plotted between ',f7.3,' pA and ',f7.3,' pA',/)
	endif
c histos:
	ncurvd=1
	icurvd(1)=1
	ndat(1)=nbin
	ijoin(1)=0
c
	ncurvc=0
c
	ndimd=1
	ndimc=1
c
	titley='frequency density'
	if(iplot.eq.1) then
	   titlex='amplitude (pA): all point'
	else if(iplot.eq.2) then
	   titlex='amplitude (pA): shut point'
	else if(iplot.eq.3) then
	   titlex='amplitude (pA): open point'
	else if(iplot.eq.4) then
	   titlex='amplitude (pA): open/shut point'
	endif
	if(posamp.and.opendown) titlex='-'//titlex(1:39)	!neg sign
	ilabel=1			!so above labels used
	autplt=.false.		!for VHIST
	draft=.false.		!for VHIST
	doframe=.true.		!for VHIST
	landscap=.true.		!for VHIST
	lt2=2       !short dash for lo,hi bins
	iscal=0			!use input xmin,xmax etc
	cbig=2.5
	ifont=4
	isval=0	!no arrow
	xcross=0.0
	ycross=0.0
	inumx=-1	!X axis in fixed format if log scale
	iask=2	!do not ask before leaving display; leave graph on screen
	ntx=5		!if not logt set initial input values
	nty=5
	itx=1
	ity=1
c IFITYPE=0 for no display of param
c IFITYPE=1 for time histos (THIST)
c IFITYPE=2 for op/bst histos (OBHIST)
c IFITYPE=3 for amp  histos (AHIST)
c IFITYPE=-3 for amp  histos (AHIST) if SD are constrained to be equal
	ifitype=0
100	continue	!return to show fit
	isetcol=0
	call VHIST5(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ILOG,ISCAL,
     & xmin,xmax,ymin,ymax,xtic,ytic,xcross,ycross,ntx,nty,itx,ity,
     & XLO,XHI,YLO,YHI,xwbase,lt2,inumx,inumy,
     & titlex,titley,ilabel,doframe,idiskq,
     & autplt,draft,itit,title1,cbig,ifont,landscap,fitted,
     & theta,ifitype,ncomp,sval,isval,iask,
     & isetcol,ndv1,ndc1,kmax,iver)
c
c NB following messages appear in the VHIST dialog boxes, so make shorter
	if(fitted) goto 99
	if(allpoint) goto 90	!fit here?
      ans='N'
      call DEFOLTa(ans,defolt)
      call QDIALOG(1,'Another histogram',defolt,-icw,cans)
	call GETINPa(cans,ans)
	if(ans.eq.'Y') then
	   iplot=iplot+1
	   if(iplot.gt.4) iplot=1
	   call WDIALOG(1,
     &   '(1) All point histogram; (2) Shut point histogram',
     &    icw)
	   call WDIALOG(1,
     &    '(3) Open point histogram; (4) Shut and open point',
     &    icw)
	   call DEFOLTi(iplot,defolt)
	   call QDIALOG(1,'Option number',defolt,icw,cans)
	   call GETINPi(cans,iplot)
	   goto 12
	endif
	goto 99
c
c section to do fit here, rather than ekdist, when doing ONLY point
c amp histo
90	continue
	if(.not.allpoint) goto 99
	graphic=.true.
	ans='Y'
	call DEFOLTa(ans,defolt)
	call QDIALOG(1,'Fit this display',defolt,icw,cans)
	call GETINPa(cans,ans)
	if(ans.eq.'N') goto 99
c
	do i=-5000,5000
	   freqs(i)=0.0	!initialise
	enddo
	do i=ifmin(1),ifmax(1)
	   j=i
	   if(revamp) j=-i
	   freqs(j)=float(ifreq1(i))
	enddo
	ylow=xmin		!default
	yhigh=xmax		!default
	call DEFOLT2r(ylow,yhigh,defolt)
	call QDIALOG(1,
     &   'Fit amps between Ylow, Yhigh (inc) (pA)',
     &   defolt,icw,cans)
	call GETINP2r(cans,ylow,yhigh)
	ilow=ifixr(ylow/calfac2)
	ihigh=ifixr(yhigh/calfac2)
	anf=0.0
	do i=ilow,ihigh
	  anf=anf+freqs(i)
	enddo
	nfit=ifixr(anf)
	if(ncomp.lt.1) ncomp=1
 	call DEFOLTi(ncomp,defolt)
	call QDIALOG(1,'Number of components to be fitted',
     &    defolt,icw,cans)
	call GETINPi(cans,ncomp)
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
	if(vary.ge.0.) then
	   s=sqrt(vary)
	else
	   s=-1.0
	endif
c
	if(discprt) write(8,301) NFIT,YBAR,s
301	FORMAT(
     &    ' Mean and SD of',I5,' values in fitted range (inc) = ',
     &    2g13.6)
	call INTCONV(nfit,cnum0)
	call REALTOCH(round1(ybar,2),cnum1,11)
	call REALTOCH(round1(s,3),cnum2,11)
	call WDIALOG(1,'Mean and SD of '//CHARNB(cnum0)
     &    //' values in range = '
     &    //CHARNB(cnum1)//', '//CHARNB(cnum2),icw)
	K=3*NCOMP-1
	isdcon=1		!default
	ifitype=3
	if(ncomp.gt.1) then
	   if(graphic) then
 		call DEFOLTi(isdcon,defolt)
		call QDIALOG(1,
     &  '(1) Fit all SD; (2) SD=const*mean; (3) All SD equal;',
     & 	  defolt,icw,cans)
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
	   if(isdcon.eq.3) ifitype=-3
	   if(isdcon.ne.1) then
		k=k-(ncomp-1)
	   endif
	endif
C
94	S=0.
	J=0
	theta(3)=1.0	!AREA OF COMPONENT 1 UNLESS REDEFINED
	DO 35 I=1,NCOMP
	J=J+1
	if(graphic) then
	   call INTCONV(j,cnum1)
	   call INTCONV(i,cnum2)
	   call QDIALOG(1,'(Param '//CHARNB(cnum1)//
     &    '): Mean amplitude('//CHARNB(cnum2)//') (pA)',' ',icw,cans)
	   call GETINPr(cans,theta(j))
	   titlep(j)='mean amplitude('//charnb(cnum2)//')'
	else
	   print 39,J,I
39	   format('&Param(',I2,')  Mean amp(',I2,') (pA)= ')
         call INPUTr(theta(j))
	endif
	if(i.gt.1.and.k.lt.3*ncomp-1) goto 371		!SD equal
	J=J+1
	if(graphic) then
	   call INTCONV(j,cnum1)
	   call QDIALOG(1,'(Param '//CHARNB(cnum1)//
     &    '): Standard dev('//CHARNB(cnum2)//') (pA)',' ',icw,cans)
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
371	IF(NCOMP.EQ.1.OR.I.EQ.NCOMP) GOTO 35
	J=J+1
	if(graphic) then
	   call INTCONV(j,cnum1)
	   call QDIALOG(1,'(Param '//CHARNB(cnum1)//
     &    '): Fraction with amplitude '//CHARNB(cnum2),' ',icw,cans)
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
92		FORMAT(' *** TOTAL AREA GREATER THAN ONE. TRY AGAIN'/)
	   endif
	   goto 94
	endif
35	CONTINUE
C
	if(ncomp.gt.1) then
c	theta(3*NCOMP)=1.0-S		!=AREA(NCOMP)
	   theta(j+1)=1.0-S		!=AREA(NCOMP)
	   call INTCONV(ncomp,cnum2)
	   titlep(j+1)='area('//charnb(cnum2)//')'
	endif
c
	ERRFAC=1.0E-4		!DEFAULT
	FITTED=.TRUE.
	NFIX=0
	ndisp=20
	do j=1,20
	   jfix(j)=0	!INITIALISE
	enddo
	if(graphic) then
	   ans='N'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,'Fix any parameters',
     &     defolt,icw,cans)
	   call GETINPa(cans,ans)
	else
	   print 41
41	   format('&Fix any parameters [N] ? ')
	   read 101,ans
	endif
	if(UC(ans).eq.'Y') then
	   if(graphic) then
		call QDIALOG(1,' Number to be fixed [0]',
     &	' ',icw,cans)
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
     & 	    ': Fix parameter #',' ',icw,cans)
		   call GETINPi(cans,j)
		else
		   print 43,i
43		   FORMAT('&',1x,i3,'. Parameter number= ')
		   call INPUTi(j)
		endif
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
	IF(NCOMP.EQ.1) GOTO 142
c	print 302
c302	FORMAT('&Constrain amplitudes to be positive [Y] ? ')
c	read 101,ans
c	if(UC(ans).EQ.'N') AMPOS=.FALSE.
	ans='Y'
	call DEFOLTa(ans,defolt)
	call QDIALOG(1,'Constrain areas to be positive',
     &  defolt,icw,cans)
	call GETINPa(cans,ans)
	if(ans.eq.'N') AMPOS=.FALSE.
	if(discprt.and.ampos) write(8,3021)
3021	format(' Areas constrained to be positive')
142	continue
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
	print 151
151	FORMAT(' Type every Nth iteration [20]: N = ')
	call INPUTi(ndisp)
      print 32
      if(discprt) write(8,32)
32    FORMAT(' Initial guesses: ')
      do i=1,k
         print 131,i,theta(i)
	   if(discprt) write(8,131) i,theta(i)
131      FORMAT(I8,G13.6)
	enddo
	print 108
	if(discprt) write(8,108)
108	format(/)
C
	IF(.NOT.debug()) GOTO 31
342   print 340
340   FORMAT(' List some values of variable between YLO,YHI? ')
      read 101,ans
	if(UC(ans).EQ.'N') GOTO 31
	n1=1
	n2=10
	print 3421
3421	FORMAT(' Between values N1,N2= ')
	call INPUT2i(n1,n2)
	do i=n1,n2
	   if(discprt) write(8,131) i,Yval(i,1)
	   print 131,i,Yval(i,1)
	enddo
	print 108
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
	nevmax=-30000	!neg so iterations not printed to disc
	call SIMPLEX3(k,theta,stpfac,errfac,nev,nevmax,
     & 	  elmin,GAUFITs,ndisp,jfix,-1.,confac,irestrt,iconv)
	print 19
19	FORMAT( ' End of fitting')
	print 20,(theta(i),i=1,k)
20	format(4(5g13.6,/))
c	IF(K.LE.5) print 20,(THETA(I),I=1,K)
c20	FORMAT(1X,5G13.6)
c	IF(K.GT.5) print 21,(THETA(I),I=1,K)
c21	FORMAT(1X,5G13.6,/,5G13.6)
	ELMAX=-ELMIN
	if(discprt) write(8,942) NEV,ELMAX
	print 942,NEV,ELMAX
942	FORMAT(i5,' function evaluations.',/,
     & '  Maximum log(likelihood) = ',G13.6)
c
c Calculate amean etc (as in \ekdist\ptyp1)
	j=0
	sa=0.0
	do i=1,ncomp
	   j=j+1
	   amean(i)=theta(j)		! mean(i)
	   if(isdcon.eq.3) then
		if(i.eq.1) j=j+1
		sd(i)=theta(2)		!if SD fixed, otherwise redefined
	   else if(isdcon.eq.2) then
		if(i.eq.1) j=j+1
		sd(i)=theta(2)*amean(i)	!theta(2)=sdfac in this case
	   else				!isdcon=1
		j=j+1
		sd(i)=theta(j)
	   endif
	   if(i.lt.ncomp) then
		j=j+1
		area(i)=theta(j)
		sa=sa+area(i)
	   endif
	enddo
	area(ncomp)=1.0-sa
c
	call SORT4r(amean,area,sd,ncomp,10,.true.)
c
C Calc denominator=PROB(ylow<Y<yhigh)
c or, if range excluded, PROB(ylow<Y<yexclow.or.yexchigh<Y<yhigh)
	excreg=.false.
	st=sqrt(2.0)
	d=0.
	f1=0.0
	f2=0.0
	f3=0.0
	if(excreg) then
	 do i=1,ncomp
	   ulo=(ylow-amean(i))/(sd(i)*st)
	   uhi=(yhigh-amean(i))/(sd(i)*st)
	   uxlo=(yexclow-amean(i))/(sd(i)*st)
	   uxhi=(yexchigh-amean(i))/(sd(i)*st)
	   f1=f1 + area(i)*0.5*(1.0-erfs(ulo))			!prob(obs>ylow)
	   f2=f2 + area(i)*0.5*(1.0-erfs(uhi))			!prob(obs>yhigh)
	   f3=f3 + area(i)*0.5*(erfs(uxhi)-erfs(uxlo))	!prob(yxlo<obs<yxhigh)
	   d=d + area(i)*0.5*(erfs(uhi)-erfs(uxhi)+erfs(uxlo)-erfs(ulo))
	 enddo
	else		!no excluded region (as earlier version)
	 do i=1,ncomp
	   ulo=(ylow-amean(i))/(sd(i)*st)
	   uhi=(yhigh-amean(i))/(sd(i)*st)
	   f1=f1 + area(i)*0.5*(1.0-erfs(ulo))			!prob(obs>ylow)
	   f2=f2 + area(i)*0.5*(1.0-erfs(uhi))			!prob(obs>yhigh)
c	   d=d + area(i)*0.5*(erfs(uhi)-erfs(ulo))
	 enddo
	endif
	ANTRUE=float(nfit)/(f1-f2-f3)	!normal calc
c========debug
c	x=f1-f2-f3		!should=d
c then calc curves as in \ekdist\scvdu and caldis
c Calc curve now -then back to vhist
	ncalc=2048		!for VHIST3
	dxcalc=(xmax-xmin)/float(ncalc-1)	!increment
	pi=3.1415927
	c=sqrt(2.*pi)
c
	do i=1,ncalc
	   x=xmin+float(i-1)*dxcalc
	   xcal(i,1)=x
	   t=0.
	   do j=1,ncomp
		t1=(x-amean(j))/sd(j)
		t=t + area(j)*exp1(-0.5*t1*t1)/(c*sd(j))
	   enddo
	   Ycal(i,1)=antrue*xwbase*t
c	   FCALC(I)=ANTRUE*XWBASE*T
	enddo
128	CONTINUE
	fitted=.true.
	ncurvc=1
	icurvc(1)=1
	ncal(1)=ncalc
	iline(1)=0		!continuous
	goto 100		!display fit
c
99	continue
	DEALLOCATE(xval,yval,xcal,ycal)
	RETURN
	end

