	subroutine POINTAMP(imin,imax,ifreq1,ifreq2,ifreq3,calfac2,
     & revamp,nbin,xval,yval,xmin,xmax,ymin,ymax,xtic,ytic,
     & ifmin,ifmax,xwbase,idtype,ndimd)
c To calculate point amplitude histos in EKDIST (called from SCVDU)
c Based on SCAMHIST (see SCAN)
c
	integer*4 ifreq1(imin:imax),ifreq2(imin:imax),ifreq3(imin:imax)
	integer*4 ifmin(4),ifmax(4)
	logical logt,binset,revamp
	logical discprt
	character UC*1,ans*1,cnum1*11
c for VHIST
	real XVAL(0:511,ndimd),YVAL(0:511,ndimd)
c	real*4 XCAL(512,1),YCAL(512,1)
c for histo data
c	integer ndat(1),icurvd(1),ijoin(1)
c for calc curves
c	integer ncal(1),icurvc(1),iline(1)
c
	common/dp/discprt
c
101	format(a1)
	binset=.false.
c
c Find largest and smallest non-zero entries in ifreq()
	do i=imax,imin,-1
	   if(ifreq1(i).ne.0) then
		ifmax(1)=i
		goto 5
	   endif
	enddo
5	do i=imax,imin,-1
	   if(ifreq2(i).ne.0) then
		ifmax(2)=i
		goto 6
	   endif
	enddo
6	do i=imax,imin,-1
	   if(ifreq3(i).ne.0) then
		ifmax(3)=i
		goto 7
	   endif
	enddo
7	do i=imin,imax
	   if(ifreq1(i).ne.0) then
		ifmin(1)=i
		goto 8
	   endif
	enddo
8	do i=imin,imax
	   if(ifreq2(i).ne.0) then
		ifmin(2)=i
		goto 9
	   endif
	enddo
9	do i=imin,imax
	   if(ifreq3(i).ne.0) then
		ifmin(3)=i
		goto 10
	   endif
	enddo
10	continue
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
c
	iplot=idtype-1	!idtype=2 to 5 already specified in CDIST1
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
c  (re)set cmin every time so get correct xval(1,1) (values of xmin,xmax for
c display are set below, or defined from xmin1,xmax1 when binset=true)
	cmin=float(ifmin(iplot))*calfac2    !min pA
	cmax=float(ifmax(iplot))*calfac2    !max pA
	if(revamp) then		!reverse sign
	   if=ifmax(iplot)		!index for ifreq
	   inc=-1		!work back from ifreq(ifmax) down to ifreq(ifmin)
	   x=cmax		!save
	   cmax=-cmin
	   cmin=-x
	else
	   if=ifmin(iplot)		!index for ifreq
	   inc=1		!work up from ifreq(ifmin) up to ifreq(ifmax)
	endif
c
	nbin=1 + ifix((cmax-cmin)/dx)       !round up
	xval(1,1)=cmin
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
c	HI bin is from Xval(nbin+1,j) to Xval(nbin+2,j); freq in Yval(nbin+1,j)
c set NO lo/hi bin
	xval(0,1)=xval(1,1)-dx		!lo bin
	yval(0,1)=0.0			!lo bin
	xval(nbin+2,1)=xval(nbin+1,1)+dx	!hi bin
	yval(nbin+1,1)=0.0		!hi bin
c
	ilog=0
	logt=ilog.eq.6		!plot sqrt(y) vs log(t)
c	ixlo=-1
c
	ymin=0.0
	ymax=ymax*1.2	!max for display
	ymax=float(1+ifix(ymax))	!ROUND UP
	ytic=10.0**ifix(alog10((ymax-ymin)*0.5))
2101	if((ymax-ymin)/ytic.lt.5.) then
	   ytic=ytic/2.
	   goto 2101	!ensure at least 5 tics so get numerical label
	endif
2102	if((ymax-ymin)/ytic.gt.11.) then
	   ytic=ytic*2.
	   goto 2102	!ensure at least 5 tics so get numerical label
	endif
c	ytic=0.5*10.0**ifix(alog10((ymax-ymin)*0.5))
c2102	if((ymax-ymin)/ytic.gt.10.) goto 2101
c	ytic=ytic/2.
c	goto 2102	!ensure at least 5 tics so get numerical label
c2101	continue
	if(.not.logt) ytic=ytic*5.	!smaller tics for sqrt scale for now
	if(binset) then	!skip questions
c	   xmin=xmin1	!restore value set below for display
c	   xmax=xmax1	!restore value set below
	   binset=.false. !reset
	   goto 14
	endif
c round xmin down to nearest 1 pA for start point of histogram
	xmax=cmin+float(nbin-1)*dx		!xmax adj to nbin just defined
	xmax=float(ifix(xmax)+1)		!nearest pA above
c	xmin=0.1*float(ifix(10.*cmin)-1)	!nearest 0.1pA
	xmin=float(ifix(cmin)-1)		!nearest pA below
c	xtic=2.*xwbase	!number every 10th bin
c# For Lahey/Hgraph xtic refers to major, not minor tics so make
c bigger ( say 2-fold rather than 5-fold for now?- 5-fold faster!)
c	xtic=xtic*5.
	xtic=1.0		!major tics=1pA
	print 20,nbin,dx,xmin,xmax
20	format(1x,i3,
     & ' bins, width ',f9.3,' pA, from ',f9.3,' to ',f9.3,
     & ' pA: O.K. [Y] ')
	read 101,ans
16	continue
c    	call INTCONV(nbin,cnum)
c	call DCFORMAT(dx,7,2,cnum1)
c	call DCFORMAT(xmin,7,2,cnum2)
c	call DCFORMAT(xmax,7,2,cnum3)
c	text=charnb(cnum)//' bins, width '//charnb(cnum1)//'pA, from'
c	n=nblank(text)
c	text=text(1:n)//charnb(cnum2)//' to '//charnb(cnum3)
c	call WDIALOG(1,text,icw)
c      ans='Y'
c      call DEFOLTa(ans,defolt)
c      call QDIALOG(1,'O.K.',defolt,-icw,cans)
c	call GETINPa(cans,ans)
	if(UC(ans).eq.'N') then
         ans='Y'
c         call DEFOLTa(ans,defolt)
c         call QDIALOG(1,'Plot absolute amplitudes',defolt,-icw,cans)
c	   call GETINPa(cans,ans)
	   call DCASK('Plot absolute amplitudes','y',ans)
	   revamp=ans.eq.'Y'
	   cmin=float(ifmin(iplot))*calfac2    !min pA -restore orig
	   cmax=float(ifmax(iplot))*calfac2    !max pA
	   xmin=float(ifix(cmin)-1)		!nearest pA below
	   xmax=xmin+float(nbin-1)*dx		!xmax adj to nbin just defined
	   xmax=float(ifix(xmax)+1)		!nearest pA above
	   if(revamp) then
		x=xmin
		xmin=-xmax
		xmax=-x
	   endif
c    NB xmin,xmax here are for display (reset above to recalc histo) so
c    save values input here as xmin1,xmax1 and restore after histo calcs
c===============
c	   call DEFOLTr(xmin1,defolt)
c	   call QDIALOG(1,'Histogram to start at (pA)',defolt,icw,cans)
c	   call GETINPr(cans,xmin1)
c	   call DEFOLT2r(dx,xmax1,defolt)
c	   call QDIALOG(1,'Bin width, Last x value (pA)',defolt,icw,cans)
c	   call GETINP2r(cans,dx,xmax1)
	   print 18,xmin
18	   format(' Histogram to start at (pA) [',f5.2,'] = ')
	   call INPUTr(xmin)
	   print 19,dx,xmax
19	   format(' Bin width, Last x value (pA) [',f5.2,',',f5.2,'] = ')
	   call INPUT2r(dx,xmax)
	   idelt=ifixr(dx/calfac2)	!approx # of elements for 0.1 pA
	   dx=float(idelt)*calfac2	!exact pA per bin (i.e. per idelt elements)
	   xwbase=dx		!if only one bin width
cc need Qdialog here, otherwise dialog box cleared before next line can be read
	   call DCFORMAT(dx,9,5,cnum1)
c         ans='Y'
c         call DEFOLTa(ans,defolt)
c         call QDIALOG(1,'Bin width reset to '//charnb(cnum1)//' pA',
c     &	defolt,-icw,cans)
c	   call GETINPa(cans,ans)
	   call DCASK('Bin width reset to '//charnb(cnum1)//' pA','y',ans)
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
c===================another histo?
c NB histo type already specified in CDIST1 (though could change here?)
c      ans='N'
c      call DEFOLTa(ans,defolt)
c      call QDIALOG(1,'Another histogram',defolt,-icw,cans)
c	call GETINPa(cans,ans)
c	if(ans.eq.'Y') then
c	   iplot=iplot+1
c	   if(iplot.gt.4) iplot=1
c	   call WDIALOG(1,
c     &   '(1) All point histogram; (2) Shut point histogram',
c     &    icw)
c	   call WDIALOG(1,
c     &    '(3) Open point histogram; (4) Shut and open point',
c     &    icw)
c	   call DEFOLTi(iplot,defolt)
c	   call QDIALOG(1,'Option number',defolt,icw,cans)
c	   call GETINPi(cans,iplot)
c	   goto 12
c	endif
c
	RETURN
	end


