c======================================================================
	subroutine MULTRACE(yval,mono,xmin,xmax,ymin,ymax,idev,
     & ndatj,ijoinj,icolj,thj,ndv1,ndimd,ntrace,ytsep,
     & reclen,idraw1,ibk,iend,np1,np2,mulpage,adcfil,
     & csize,ifont,calfac,ioff,imode1)
c======================================================================

c To draw trace (eg single channel data) as several rows.
c Works for j=1 only, and assumes equally=spaced x values
c Call with
c	ibk=icol(71)=background col for graph, to erase trace
c	ijoinj=ijoin(1)
c	icolj=icol(1)
c	ilthj=ilth(1)
c	idraw1=idraw(1)
c [Params for DRAWDAT (lines 1-6 in call)]
c  IMODE=0 -asks for number of traces etc
c  IMODE=1 -draws traces only, using input values of ntrace, ytsep
c  IMODE=2 -ditto, but does not erase graph area first.
c  IMODE=3 -uses input ndat,ntrace,ytsep to calculate and draw the trace
c		and ask if it is OK (e.g. used for preparing for multi-page
c		plots in autplot)
c  reclen=duration of the whole trace = xval(ndat(1))-Xval(1) in call.
c	(reclen needed only for imode=0 and 3)
c
c ijoinj=ijoin(1) in call is now set to 0 internally because no points
c drawn here, only lines and if queued single channel record has dots
c only (as when a fitted curve is superimposed on it), so ijoin(1)=-1,
c get nothing at all here.  The value needs to be passed back to VPLOT
c so redrawn correctly.
c VERSION WITH RESCALING
c NB orig data may not cover whole width up to xmax -up to
c  xval(ndat(1)) only: to get new time calib must use original dx value
c
	real*4 YVAL(ndv1,ndimd)
	ALLOCATABLE::XVAL1
	real*4 Xval1(:)
	ALLOCATABLE idata
	integer*2 idata(:)

	logical mono,mulpage,screen,plot,restart,doframe,kbhit
	logical draft,quarter,present,one
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi
	character adcfil*33,string*75
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character ans*1,ans1*1,cnum1*11,cnum2*11
	character getch*1,ch*1
	character*33 oldfile,printfil
	character*11 kpchar

c
	imode=imode1   !so input value not changed (and OK to call with number)
	jval=1		!first trace only for now
	ijoinj=0    !solid line
	plot=.false.
	restart=.false.
	screen=.true.
	draft=.false.
	mpos=6
	quarter=.false.
	one=.false.
	npage=1
	istart=np1		!see vplot5
	xlos=xlo
	xhis=xhi
	ylos=ylo
	yhis=yhi
	ALLOCATE(idata(ndatj))
c
	if(imode.eq.0) then	!draw only
	   n=5
 	   call DEFOLTi(n,defolt)
	   call QDIALOG(1,
     &    'Split data for trace 1 into n rows: n',
     &	defolt,11,cans)
	   call GETINPi(cans,n)
	   ntrace=n
	endif
c
10	continue
	if(ntrace.eq.1) then
	   npt=ndatj
	   nptlast=npt
	else
	   npt=ndatj/ntrace + 1			!points per row
	   nptlast=ndatj-npt*(ntrace-1)
	endif
c Find min,max for first row
	ymin1=1.e37
	ymax1=-1.e37
	do i=1,npt
	   yv=yval(i,jval)
	   if(yv.lt.ymin1) ymin1=yv
	   if(yv.gt.ymax1) ymax1=yv
	enddo
c
c Calc gain and separation so ymin1=ymin, lowest displayable trace
c -but use ymin2 rather than ymin, to leave (10%) space at bottom for
c calibration bars.
c
	if(imode.eq.0.or.imode.eq.3) then
c Calc new ymin, ymax for rescale.  It is really only ymax-ymin that
c is defined; origin is arbitrary so can set ymin=0
	   if(imode.eq.0) ytsep=ymax1-ymin1
	   ymin=0.0
	   ymax=ymin+float(ntrace)*ytsep*1.1	!10% extra for cal bars
c Calc new xmin, xmax for rescale: reclen=length of row in whatever
c units were used for reclen=original xval(ndatj)-xval(1).
	   tfac=float(npt-1)/float(ndatj-1)
	   rowlen=reclen*tfac	   !length of row of npt points
	   xmin=0.0
	   xmax=rowlen
	   call gSCALE(xmin,xmax,ymin,ymax,is)
	   call spagra(vxlo,vylo,wxmin,wymin)
	endif
c
c Calc dx value for new Xval
	dx=(xmax-xmin)/float(npt-1)
c
	ALLOCATE(xval1(npt))
c Define xval1
	do i=1,npt
	   xval1(i)=xmin + float(i-1)*dx
	enddo
c
c Calc offset so that ymax1=ymax, largest displayable point
	yofftop=ymax-ymax1
c
c Delete current data set 1; call DRAWDAT with colour=background, ie
	if(imode.ne.2) then
	   ic=ibk
	   if(mono) ibk=0
	   call FILLWIN(0,0,639,387,ibk)		!graph area
	endif

c================================================================
c Now redraw as ntrace traces
c================================================================

12	continue			!return here to plot next page
	linetype=ijoinj
	iud=1				!draw line between points
	if(linetype.eq.-1) iud=0	!don't
	ic=icolj
	if(mono) ic=15		!bright white
	if(.not.mono) call lincols(ic,idev)
	call LINWID(thj)
	iy1=1		!element of yval to start at
	do j=1,ntrace
	   yoff=yofftop-float(j-1)*ytsep
	   n2=npt
	   if(j.eq.ntrace) n2=nptlast
	   jflag=0
	   do i=1,n2
		xv=xval1(i)
		i1=i+iy1-1
		yv=yoff + yval(i1,1)     !NB j=index of loop!
		if(xv.ge.xmin.and.xv.le.xmax.and.
     &		yv.ge.ymin.and.yv.le.ymax)then
		   if(jflag.eq.0) then
			call gramov(xv,yv)   !move to 1st point in range
		   else
			call broken(linetype)
	  		call gralin(xv,yv)   !join with line from last point
		   endif
		   jflag=1
		endif
	   enddo
	   iy1=iy1+npt
	enddo
	if(doframe) then
	  call LINWID(0.20)
	  call graspa(xmin,ymin,sxmin,symin)
	  call graspa(xmax,ymax,sxmax,symax)
	  call DFRAME(sxmin,sxmax,symin,symax,0)	!linetype=0
	endif
	if(mulpage) then
c        write page number and file name onto hard copy (like writqnum)
	   call INTCONV(istart,cnum1)
	   call INTCONV(iend,cnum2)
	   call CHAFNT(2)
	   call LINWID(0.15)
	   n=nblank3(adcfil,33)
	   string=
     & adcfil(1:n)//': points '//charnb(cnum1)//' to '//charnb(cnum2)
	   n=nblank1(string)
	   if(screen) then
	   	xstr=10.
		ystr=10.
	   else
	   	xstr=15.
		ystr=1.
	   endif
	   call JUSTIFYSTRING(xstr,ystr,string(1:n),
     &	 0.,csize,-1)	!3=base justification
c        and write calibration
	   call DCFORMAT(rowlen,7,2,cnum1)
	   call DCFORMAT(ytsep,7,1,cnum2)
	   call GBLANK(cnum1,11,n1,n2)
	   string='Line length = '//cnum1(n1:n2)//' ms: Line sep = '
	   call GBLANK(cnum2,11,n1,n2)
	   string=charnb(string)//cnum2(n1:n2)//' pA'
	   n=nblank1(string)
	   if(screen) then
	   	xstr=10.
		ystr=1.
	   else
	   	xstr=125.
		ystr=1.
	   endif
	   call JUSTIFYSTRING(xstr,ystr,string(1:n),
     &	 0.,csize,-1)	!3=base justification
c
	   if(screen) then
		one=.false.
	      if(imode.eq.1.or.imode.eq.2) goto 99		!draw only
	      ans='Y'
 	      call DEFOLTa(ans,defolt)
	      call QDIALOG(1,
     &      'Display O.K. as shown',defolt,11,cans)
	      call GETINPa(cans,ans)
	 	if(ans.eq.'N') then
 	   	  call DEFOLTi(ntrace,defolt)
	   	  call QDIALOG(1,
     &        'Data split into n rows: n',defolt,11,cans)
	        call GETINPi(cans,ntrace)
 	        call DEFOLTr(ytsep,defolt)
	        call QDIALOG(1,
     &        'Separation of rows (pA)',defolt,11,cans)
	        call GETINPr(cans,ytsep)
	        DEALLOCATE(xval1)
	        imode=3
	        goto 10
		endif
	      ind=3
	      call INTCONV(npage,cnum1)
 	      call DEFOLTi(ind,defolt)
		call WDIALOG(1,
     &     '1.Plot this page ['//cnum1(1:3)//']  2.Plot all pages',11)
		if(istart.eq.np2) then
	         call QDIALOG(1,
     &     '3.Last page ['//cnum1(1:3)//']   =   4.Exit',defolt,11,cans)
		else
	         call INTCONV(npage+1,cnum1)
	         call QDIALOG(1,
     &     '3.See next page ['//cnum1(1:3)//']   4.Exit',defolt,11,cans)
		endif
	      call GETINPi(cans,ind)
		if(ind.lt.1.or.ind.gt.4) ind=3
		if(ind.eq.4) goto 99
	      ans='N'
 	      call DEFOLTa(ans,defolt)
	      call QDIALOG(1,
     &      'Draw frame round traces',defolt,11,cans)
	      call GETINPa(cans,ans)
	      doframe=ans.eq.'Y'
		if(ind.eq.1) then
		   restart=.true.
		   screen=.false.
		   mpos=6
		   one=.true.
		   call mode(18)
		   call OPENDIALOG(1,7,.true.)		!draw dialog box #1
		   goto 123
		else if  (ind.eq.2) then
		   ans1='N'             !default for plot on screen
		   iend=np1-1		!so istart=np1 below
		   npage=1
		   restart=.true.
		   screen=.false.
		   mpos=6
		   istart=np1
		   call mode(18)
		   call OPENDIALOG(1,7,.true.)		!draw dialog box #1
		   goto 124
		endif
	   else
		call OPENDIALOG(1,7,.true.)		!draw dialog box #1
	      call INTCONV(npage,cnum1)
		call WDIALOG(1,'Plotting page '//charnb(cnum1),11)
		screen=.false.
		if(kbhit()) then
		   ch=GETCH(ktype)
		   ival=ichar(ch)
		   if(ival.eq.27) then	!ESC typed
		   	DEALLOCATE(idata)		!when done
	   		call VGA	!back to screen
			call gsetcols(0)
			call mode(18)
			call errswi(-1)
			call brkswi(1)
			screen=.true.
			idev=0
	            call GRAPHBOUNDARY(xlos,xhis,ylos,yhis)
			call gSCALE(xmin,xmax,ymin,ymax,is)
	   		goto 99
		   endif
		endif
	   endif
	   if(.not.screen) then
		call devend
		call vga
	      call errswi(-1)
	      call brkswi(1)
	      call chaswi(1)
	      call gsetcols(0)
	      call grfmod (1)
	      call harcha
	      call mode(3)
		if(idev.eq.3.or.idev.eq.4) then
			oldfile='EPS.OUT'
		else if(idev.eq.5) then
			oldfile='HPLJR.OUT'
		else if(idev.eq.6) then
			oldfile='DJ500C.OUT'
		endif
		INQUIRE(file=oldfile,exist=present,flen=len)
		if(present) then
		   kp=kp+10
		call intconv(kp,kpchar)
		nc1=nblank1(kpchar)
		printfil='fit'//kpchar(1:nc1)//'.out'
		INQUIRE(file=printfil,exist=present)
		if(present) then
		   call ERASE(printfil)	!erase old printfil, before rename
		endif
		call RENAME(oldfile,printfil)
		Print 70,
     &	charnb(oldfile)//' renamed '//charnb(printfil)
70		format(1x,a34)
		   print 71,
     &	' Copying '//charnb(printfil)//' to printer . . .'
71		   format(a40)
		print 72,float(len)/1.e6
72		format('  (length = ',f9.3,' Mb)')
		if(idev.eq.3.or.idev.eq.4) then
		   call COPY (printfil,'lpt1')
		   else if(idev.eq.5)then
		   call COPY (printfil,'lpt1')
c	   	   call system('copy/b hpljr.out lpt1:')
		else if(idev.eq.6) then
		   call COPY (printfil,'lpt1')
c	   	   call system('copy/b dj500c.out lpt1:')
		   endif
		else
		   call BELL(1)
		   print 73,oldfile
73		   format(' Printer file not found: ',a33)
		   pause
		endif
	   endif
c        Get next page
	   npage=npage+1
	   istart=iend+1
	   mpos=mpos+1
	   if(istart.gt.np2.or.one) then	!finished
	      call mode(18)
		idev=0
		screen=.true.
	      call GRAPHBOUNDARY(xlos,xhis,ylos,yhis)
		call gSCALE(xmin,xmax,ymin,ymax,is)
	      call FILLWIN(0,388,639,479,1)			!dialog area
	      call OPENDIALOG(1,7,.true.)		!draw dialog box #1
	      call OPENDIALOG(2,0,.true.)		!draw dialog box #2
	      call WDIALOG(2,'F1=HELP',ict)
	      call WDIALOG(2,'F2=HELP INDEX',ict)
		if (istart.gt.np2) goto 99
	   endif
124	   iend=istart+ndatj-1
	   if(iend.gt.np2) iend=np2
	   iread=iend-istart+1		!normally=ndatj
c        Read in new yval()
	   OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &       access='TRANSPARENT')
	   irec=ioff + (2*istart-1)    !record # for 1st value to be read
	   read(14,rec=irec) (idata(i),i=1,iread)
	   CLOSE(unit=14)
	   do i=1,iread
		Yval(i,1)=calfac*float(idata(i))
	   enddo
123	   if(restart) then
		restart=.false.
	      plot=.true.
		screen=.false.
	      scalfac=1.0		!graphboundary reset so no need to scale up
	      ip=1
	      call PLOTAUT(draft,screen,quarter,ifont,csize,thbig,idev,
     &      mpos,ip) 	!sets 'thick'=thbig in common for VPLOTQ
		if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) then
			mono=.true.
			if(idev.eq.3) then
			   XPP=210.
			   YPP=297.
			else if (idev.eq.4) then
			   XPP=297.
			   YPP=210.
			else if(idev.eq.5) then
			   XPP=280.5
			   YPP=196.7
			endif
		else
			if(idev.eq.6) then
			   XPP=266.7
			   YPP=203.5
			endif
			mono=.false.
		endif
		pxlo=0
		pxhi=xpp
		pylo=0
		pyhi=ypp
c	      call GRAPHBOUNDARY(pxlo,pxhi,pylo,pyhi)
	      call gSCALE(xmin,xmax,ymin,ymax,is)
		call mode(18)
	      goto 12
	   endif
	   if(.not.screen) then
		call FLUSH(7)
		call INTCONV(npage,cnum1)
		call WDIALOG(1,'Assembling page '//charnb(cnum1),11)
	      call PLOTAUT(draft,screen,quarter,ifont,csize,thbig,idev,
     &      mpos,ip) 	!sets 'thick'=thbig in common for VPLOTQ
		if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) then
			mono=.true.
			if(idev.eq.3) then
			   XPP=210.
			   YPP=297.
			else if (idev.eq.4) then
			   XPP=297.
			   YPP=210.
			else if(idev.eq.5) then
			   XPP=280.5
			   YPP=196.7
			endif
		else
			if(idev.eq.6) then
			   XPP=266.7
			   YPP=203.5
			endif
			mono=.false.
		endif
		pxlo=0
		pxhi=xpp
		pylo=0
		pyhi=ypp
c	      call GRAPHBOUNDARY(pxlo,pxhi,pylo,pyhi)
	      call gSCALE(xmin,xmax,ymin,ymax,is)
		call mode(18)
	   else
	      call FILLWIN(0,0,639,387,ibk)		!graph area
		mono=.false.
	   endif
	   goto 12
	endif
99	continue

	xlo=xlos
	xhi=xhis
	ylo=ylos
	yhi=yhis
	mulpage=.false.	!reset before leaving
	if(allocated(idata)) DEALLOCATE(idata)		!when done
	DEALLOCATE(xval1)
	RETURN
	end
