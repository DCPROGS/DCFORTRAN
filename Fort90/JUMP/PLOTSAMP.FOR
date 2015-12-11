	program PLOTSAMP
c To plot all or part of CONSAM file on multiple pages (as also available
c in AUTPLOT if a single channel record is queued)
c Initially draw first page on screen -then options to draw next (or any)
c page and options to plot one or all pages
	integer videotyp
	integer icol(100)
	character filnam*32,prtport*4		!for winprint
	character cdate*11,cdate1*11,ctime*11,adctim1*8		!read from consam
	character adcfil*33,cnum0*11,cnum1*11,cnum2*11
	character ans*1,UC*1
	character getch*1,ch*1
	character*33 oldfile,printfil
	character*11 kpchar
	character string*75
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character*78 titles,strings(10),helps(10)		!for popmenu
	logical noconsam,newform,landplot

	logical present,autplt,plotcols
	ALLOCATABLE::idata,yval,xval
	integer*2 idata(:)
	real*4 yval(:),xval(:)
	logical mono,doframe,kbhit
	character title*76
c for cjump.dat
	logical cjdat
	logical discprt
	character*40 mtitle		!machine name
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi
	COMMON/mtrace/ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil
	common/dpp/filnam,prtport,ndisc,jcol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	common/dp/discprt
	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls
c
	call mode(3)
      filnam='PLOTSAMP.PRT'
	call WINPRINT	!print file control
      OPEN(unit=7,file=prtport,iostat=nerr)
	print 102
	if(discprt) write(8,102)
102	format(' AUTPLOT- Automatic plotting program',/)
	call GETSPEC('autplot.exe')	!print details
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 1,cdate,ctime(1:8),mtitle
	if(discprt) write(8,1) cdate,ctime(1:8),mtitle
1	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
c	print 399
c399	format(
c     & ' SCROLL LOCK on for printing',/,
c     & ' CAPS LOCK on for debugging',/)
c
	INQUIRE(file='plotsamp.ini',exist=present,flen=len)
	if(present.and.len.gt.0) then
         OPEN(unit=12,file='PLOTSAMP.INI',status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=128)
	   read(12,rec=1) adcfil
	else
         OPEN(unit=12,file='PLOTSAMP.INI',status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=128)
	   adcfil='F:\consmjc3.006'
	   write(12,rec=1) adcfil
	endif
	CLOSE(unit=12)
c
	print 30
30	format(
     & ' (1) Continuous data (CONSAM or AXON)',/,
     & ' (2) CJUMP data (not yet fixed)',/,
     & ' Option number [1] = ')
	iopt=1
	call INPUTi(iopt)
	cjdat=iopt.eq.2
c Values for popmenu
	nxlo=50	!pixels
	nylo=-1
	nyhi=370    !pixels
c Lahey colours:
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
	ictm=15		!text white
	ibkm=1		!background dark blue
	icfm=11		!frame/title light blue
	icupm=12		!upper case red
c Get consam/axon file name
	call GETCONS(adcfil,noconsam,title,cdate1,adctim1,nsam,
     & srate,cjdat,irecfst,newform,ioffset,calfac,ffilt,idest)
	ioff=ioffset
c	ndatj=nsam
	if(noconsam) goto 99

c	INQUIRE(file=adcfil,exist=present,flen=len)
	OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &       access='TRANSPARENT')
c===code from AUTPLOT
	dx=1000./srate		!ms between points
	tsamlen=float(nsam)/srate	!seconds
c
	np1=1
	np2=nsam
361	print 36,nsam,tsamlen,np1,np2
36	format(
     &  ' CONSAM contains ',i10,' points: length = ',g13.6,' seconds',/,
     &  ' Plot from point n1 to n2: n1,n2 [',i9,',',i9,'] = ')
	call INPUT2i(np1,np2)
	if(np2.lt.1.or.np2.gt.nsam) goto 361
39	print 37
37	format(' Number of seconds per page [1.0] = ')
	secpage=1.0
	call INPUTr(secpage)
	secpage=secpage*1000.	!ms/page
	npuse=np2-np1+1
	samlen1=dx*(float(npuse-1))
	npag=1+ifix((samlen1-1.)/secpage)
	print 38,npag
38	format(
     &' This corresponds to ',i5,' pages for whole sample: O.K. [Y] ? ')
	ans='Y'
	call INPUTa(ans)
c	if(UC(ans).eq.'N') goto 39
	if(UC(ans).eq.'N') goto 361
	ntrace=10		!default value
	print 370,ntrace
370	format(' Number of traces per page [',i3,'] = ')
	call INPUTi(ntrace)
c Read in first page and use min/max to get default line separation
	istart=np1	!to read first page in readsq1
	nppage=ifixr(secpage/dx)		!points/page
	if(nppage.gt.nsam) nppage=nsam
	if(allocated(idata)) DEALLOCATE(idata,yval,xval)
	ALLOCATE(idata(nppage),yval(nppage),xval(nppage))
	npage=1
c
	istart=np1+(npage-1)*nppage
	iend=istart+nppage-1
	iread=iend-istart+1		!normally=nppage
	OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &       access='TRANSPARENT')
	irec=ioff + (2*istart-1)    !record # for 1st value to be read
	read(14,rec=irec) (idata(i),i=1,iread)
	CLOSE(unit=14)
	ymin1=1.e37
	ymax1=-1.e37
	do i=1,iread
	   Yv=calfac*float(idata(i))
	   yval(i)=yv
	   if(yv.lt.ymin1) ymin1=yv
	   if(yv.gt.ymax1) ymax1=yv
	enddo
	ytsep=1.3*abs(ymax1-ymin1)	!default
	print 371,ytsep
371	format(' Vertical separation of traces (pA) [',f7.2,'] = ')
	call INPUTr(ytsep)
c
c===code modified from multrace
	ijoinj=0		!solid line
	thj=0.0		!?
	xlos=xlo
	xhis=xhi
	ylos=ylo
	yhis=yhi
c
10	continue
	if(ntrace.eq.1) then
	   npt=nppage
	   nptlast=npt
	else
	   npt=nppage/ntrace			!points per row
	   nptlast=nppage-npt*(ntrace-1)
	endif
c
c Calc gain and separation so ymin1=ymin, lowest displayable trace
c -but use ymin2 rather than ymin, to leave (10%) space at bottom for
c calibration bars.
c
c Calc dx value for new Xval
	ymin=0.0
	ymax=ymin+float(ntrace)*ytsep*1.1	!10% extra for cal bars
c	tfac=float(npt-1)/float(ndatj-1)
c	rowlen=reclen*tfac	   !length of row of npt points
	rowlen=float(npt)*dx
	xmin=0.0
	xmax=rowlen
	dx=(xmax-xmin)/float(npt-1)
c
c Define xval
	do i=1,npt
	   xval(i)=xmin + float(i-1)*dx
	enddo
c
125	continue		!return here after plotting to display npage
	if(VIDEOTYP().ne.18) then
	   call GINO
	   idev=0			!screen
	   call VGA
	   call gsetcols(0)
	   call errswi(-1)
	   call brkswi(1)
	   call chaswi(1)
	   call grfmod (1)
	   call harcha
	   call papenq(xp,yp,ipap)
	   vxlo=0	! for VIEWPORT
	   vxhi=xp
	   vylo=0
	   vyhi=yp
c	   xlo=0		! for graphboundary
c	   xhi=xp
c	   ylo=0
c	   yhi=yp
	   xlo=0.05*xp			! set display location on screen
	   xhi=0.95*xp
	   ylo=0.05*yp
	   yhi=0.8*yp	!lower to leav room for dialog box
c	   call pixpos(0,53,xsc,ysc)
c	   ymenu=yp-ysc
	   call mode(18)
	   icb2=0	!background colour for dialog box 1,2
c	   icf=7		!frame colour for dialog box 1
c	   ict=11	!text colour for dialog box 1
	endif
	call DEFDIALOG(1,1,2,4,60,icb2)	!define dialog box #1
	call axiset
	call GRAPHBOUNDARY(xlo,xhi,ylo,yhi)
	autplt=.false.
	plotcols=.true.
	do i=1,100
	   icol(i)=-1
	enddo
	isetcol=0
	call setcls(mono,icol,autplt,plotcols,isetcol)
	if(.not.mono) then
c	    call FILLWIN(0,55,639,387,icol(71))		!graph area
	    call FILLWIN(0,0,639,387,icol(71))		!graph area
	    call FILLWIN(0,388,639,479,1)			!dialog area
	endif
c	call HLINE(0,639,54,15)		!line to mark bottom of data area
	call HLINE(0,639,0,15)		!line to mark bottom of data area
	call HLINE(0,639,389,15)	!line to mark top of data area
	call OPENDIALOG(1,7,.true.)		!draw dialog box #1
c
	ans='N'
 	call DEFOLTa(ans,defolt)
	call QDIALOG(1,
     &      'Draw frame round traces',defolt,11,cans)
	call GETINPa(cans,ans)
	doframe=ans.eq.'Y'
c
c	if(VIDEOTYP().ne.18) then
c	   call VGA
c	   call gsetcols(0)
c	   call errswi(-1)
c	   call brkswi(1)
c	   call chaswi(1)
c	   call grfmod (1)
c	   call harcha
c	   call mode(18)
c	endif
	is=1
	call gSCALE(xmin,xmax,ymin,ymax,is)
	call spagra(vxlo,vylo,wxmin,wymin)
c
c Calc offset so that ymax1=ymax, largest displayable point
	yofftop=ymax-ymax1
c
c Delete current data set 1; call DRAWDAT with colour=background, ie
c===	if(imode.ne.2) then
c	   ic=ibk
c	   if(mono) ibk=0
c	   call FILLWIN(0,0,639,387,ibk)		!graph area
c===	endif

c================================================================
c Now draw as ntrace traces
c================================================================

12	continue			!return here to plot next page
	linetype=ijoinj
c	iud=1				!draw line between points
c	if(linetype.eq.-1) iud=0	!don't
	ic=9
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
		xv=xval(i)
		i1=i+iy1-1
		yv=yoff + yval(i1)     !NB j=index of loop!
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
c===
c  Write page number and file name onto hard copy (like writqnum)
	call INTCONV(npage,cnum0)
	call INTCONV(istart,cnum1)
	call INTCONV(iend,cnum2)
	call CHAFNT(2)
	call LINWID(0.2)
	n=nblank3(adcfil,33)
	string='Page '//charnb(cnum0)//'  '//
     & adcfil(1:n)//': points '//charnb(cnum1)//' to '//charnb(cnum2)
	n=nblank1(string)
	xstr=10.
	ystr=7.
	csize=2.5
	call JUSTIFYSTRING(xstr,ystr,string(1:n),
     &	 0.,csize,-1)	!3=base justification
c   and write calibration
	call DCFORMAT(rowlen,7,2,cnum1)
	call DCFORMAT(ytsep,7,1,cnum2)
	call GBLANK(cnum1,11,n1,n2)
	string='Line length = '//cnum1(n1:n2)//' ms: Line sep = '
	call GBLANK(cnum2,11,n1,n2)
	string=charnb(string)//cnum2(n1:n2)//' pA'
	n=nblank1(string)
	xstr=10.
	ystr=1.
	call JUSTIFYSTRING(xstr,ystr,string(1:n),
     &	 0.,csize,-1)	!3=base justification
c
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
c	      DEALLOCATE(xval)
c	      imode=3
	      goto 10
	endif
c	ind=3
c 	call DEFOLTi(ind,defolt)
c	call WDIALOG(1,'1.Plot this page  2.Plot all pages',11)
c	if(istart.eq.np2) then
c	   call QDIALOG(1,'3.Last page   =   4.Exit',defolt,11,cans)
c	else
c	   call QDIALOG(1,'3.See next page   4.Exit',defolt,11,cans)
c	endif
c	call GETINPi(cans,ind)
c	if(ind.lt.1.or.ind.gt.4) ind=3
c	if(ind.eq.4) goto 99
103	iline=4
	TITLEs='  PLOT OPTIONS    '
	strings(1)='1. Plot this page'		!ind=1 in old version
	strings(2)='2. plot Specified pages'	!ind=1? in old version
	strings(3)='3. plot All pages'		!ind=2 in old version
	strings(4)='4. view Next page'		!ind=3 in old version
	if(npage.eq.npag-1) then
	   strings(4)='4. view Last page'
	endif
	strings(5)='5. View specified page'	!ind=3? in old version
	strings(6)='6. Exit'               	!ind=4 in old version
	nval=6
	nhelp=0
	call POPMENU(nxlo,nylo,nyhi,strings,nval,ictm,icfm,
     & icupm,ibkm,titles,helps,nhelp,iline,charout,ival)
	iopt=iline
	if(iopt.eq.0.or.iopt.eq.6) then	!iline=0 for ESC=cancel
	   goto 99
	endif
	if(iopt.eq.1.or.iopt.eq.2.or.iopt.eq.3) then
	   ans='Y'
c	   call DCASK(' Is the printer ready',ans,ans)
 	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     &      ' Is the printer ready',defolt,11,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'N') goto 103
	endif
	if(iopt.eq.1) then
	   npage1=npage
	   npage2=npage
	else if(iopt.eq.2) then
	   npage1=1
	   npage2=npag		!default
 	   call DEFOLT2i(npage1,npage2,defolt)
	   call QDIALOG(1,'Plot pages n1 to n2: n1,n2',defolt,11,cans)
	   call GETINP2i(cans,npage1,npage2)
	else if(iopt.eq.3) then
	   npage1=1
	   npage2=npag		!default
	else if(iopt.eq.4) then
	   npage=npage+1	!default
	else if(iopt.eq.5) then
	   npage1=npage+1	!default
	   if(npage1.gt.npag) npage=npag
 	   call DEFOLTi(npage1,defolt)
	   call QDIALOG(1,'Show page number',defolt,11,cans)
	   call GETINPi(cans,npage1)
	   npage=npage1
	endif
c
	if(iopt.eq.1.or.iopt.eq.2.or.iopt.eq.3) goto 100	!sep plotting section
c
c  Still on screen -get the page specified above
	istart=1+(npage-1)*nppage
	iend=istart+nppage-1
	if(istart.gt.np2) then	!finished
	   call mode(18)
	   idev=0
	   call GRAPHBOUNDARY(xlos,xhis,ylos,yhis)
	   call gSCALE(xmin,xmax,ymin,ymax,is)
	   call FILLWIN(0,388,639,479,1)			!dialog area
	   call FILLWIN(0,0,639,387,icol(71))		!graph area
	   call OPENDIALOG(1,7,.true.)		!draw dialog box #1
c	   call OPENDIALOG(2,0,.true.)		!draw dialog box #2
c	   call WDIALOG(2,'F1=HELP',ict)
c	   call WDIALOG(2,'F2=HELP INDEX',ict)
	   if(istart.gt.np2) goto 99
	endif
124	continue
	iread=iend-istart+1		!normally=nppage
c  Read in new yval()
	OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &       access='TRANSPARENT')
	irec=ioff + (2*istart-1)    !record # for 1st value to be read
	read(14,rec=irec) (idata(i),i=1,iread)
	CLOSE(unit=14)
	do i=1,iread
	   Yval(i)=calfac*float(idata(i))
	enddo
	call FILLWIN(0,0,639,387,ibk)		!graph area
	call FILLWIN(0,0,639,387,icol(71))		!graph area
	mono=.false.
	goto 12
c
c SEPARATE SECTION FOR HARD COPY PLOTTING (iopt=1,2,3)
c Plot all pages from npage1 to npage2, defined above
100	continue
 	call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
c
c Decide which plotter to use and set boundaries
c	ip=1
c	mpos=6
c	call PLOTAUT(.false.,.false.,screen,quarter,ifont,csize,thbig,
c     &      idev,mpos,ip) 	!sets 'thick'=thbig in common for VPLOTQ
	print 2
2	format(
     &   ' (1) Postscript printer',/,
     &   ' (2) Laserjet printer',/,
     &   ' (3) Colour Deskjet printer',/,
     &   ' Option number [1] = ')
	iprinter=1
	call INPUTi(iprinter)
	print 3
3	format(
     & ' (1) Landscape',/,
     & ' (2) Portrait',/,
     & ' Option number [1] = ')
	 iopt=1
	 landplot=.true.
	 call INPUTi(iopt)
	 if(iopt.eq.2) landplot=.false.
c idev=0: screen
c idev=1: .wmf
c idev=2: .cgm
c idev=3,4: postscript printer
c idev=5  : laserjet
c idev=6  : color deskjet
	if(iprinter.eq.1) then
	    if(landplot) then
	 	idev=4
	    else
	 	idev=3
	    endif
	else if(iprinter.eq.2) then
	    idev=5
	else if(iprinter.eq.3) then
	    idev=6
	endif
c
c Start the loop to plot all pages (NB need to restart GINO for each page
c and reset matgins etc
	kp=0		!to number output files
	do 200 npage=npage1,npage2
	   if(idev.eq.3) then     	! portrait postscript
		call eps(1,5.,5.,210.,297.,210.,297.)
	   else if(idev.eq.4) then ! landscape postscript
		call eps(1,5.,5.,297.,210.,297.,210.)
	   else if(idev.eq.5) then
		call hpljr           ! laserjet
	   else if(idev.eq.6) then
		call DJ500C       ! color deskjet->dj500c.out
	   endif
	   call errswi(-1)
	   call brkswi(1)
	   call papenq(xp,yp,itype)
	   if(iprinter.eq.2.and.(.not.landplot)) then
		xp=196.7
		yp=280.5
		call devpap(196.7,280.5,itype)
	   endif
	   if(iprinter.eq.3.and.landplot) then
		xp=266.7
		yp=203.5
		call devpap(266.7,203.5,itype)
	   endif
	   vxlo=0
	   vxhi=xp
	   vylo=0
	   vyhi=yp
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
c	   pxlo=0
c	   pxhi=xpp
c	   pylo=0
c	   pyhi=ypp
c	   call GRAPHBOUNDARY(pxlo,pxhi,pylo,pyhi)
	   call gSCALE(xmin,xmax,ymin,ymax,is)
	   call mode(18)
	   linetype=ijoinj
	   ic=9
	   if(mono) ic=15		!bright white
	   if(.not.mono) call lincols(ic,idev)
c
	   call FLUSH(7)
	   print 201,npage
201	   format(' Assembling page ',i4)
c	   call INTCONV(npage,cnum1)
c	   call WDIALOG(1,'Assembling page '//charnb(cnum1),11)
	   if(kbhit()) then
		ch=GETCH(ktype)
		ival=ichar(ch)
		if(ival.eq.27) then	!ESC typed
c	   	   call VGA	!back to screen
c		   call gsetcols(0)
c		   call mode(18)
c		   call errswi(-1)
c		   call brkswi(1)
c		   idev=0
c	         call GRAPHBOUNDARY(xlos,xhis,ylos,yhis)
c		   call gSCALE(xmin,xmax,ymin,ymax,is)
	   	   goto 99
		endif
	   endif
c Read in data for page to be plotted
	   istart=1+(npage-1)*nppage
	   iend=istart+nppage-1
	   iread=iend-istart+1		!normally=nppage
c  Read in new yval()
	   OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &       access='TRANSPARENT')
	   irec=ioff + (2*istart-1)    !record # for 1st value to be read
	   read(14,rec=irec) (idata(i),i=1,iread)
	   CLOSE(unit=14)
	   do i=1,iread
		Yval(i)=calfac*float(idata(i))
	   enddo
c Plot this page
	   call LINWID(thj)
	   iy1=1		!element of yval to start at
	   do j=1,ntrace
		yoff=yofftop-float(j-1)*ytsep
		n2=npt
		if(j.eq.ntrace) n2=nptlast
		jflag=0
		do i=1,n2
		   xv=xval(i)
		   i1=i+iy1-1
		   yv=yoff + yval(i1)     !NB j=index of loop!
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
	   enddo	!end of j=1,ntrace
	   if(doframe) then
	      call LINWID(0.20)
	      call graspa(xmin,ymin,sxmin,symin)
	      call graspa(xmax,ymax,sxmax,symax)
	      call DFRAME(sxmin,sxmax,symin,symax,0)	!linetype=0
	   endif
c
c  Write page number and file name onto hard copy (like writqnum)
	   call INTCONV(npage,cnum0)
	   call INTCONV(istart,cnum1)
	   call INTCONV(iend,cnum2)
	   call CHAFNT(2)
	   call LINWID(0.4)
	   n=nblank3(adcfil,33)
	   string='Page '//charnb(cnum0)//'  '//
     & adcfil(1:n)//': points '//charnb(cnum1)//' to '//charnb(cnum2)
         n=nblank1(string)
	   csize=2.5
	   xstr=15.
	   ystr=5.
	   call JUSTIFYSTRING(xstr,ystr,string(1:n),
     &	 0.,csize,-1)	!3=base justification
c   and write calibration
	   call DCFORMAT(rowlen,7,2,cnum1)
	   call DCFORMAT(ytsep,7,1,cnum2)
	   call GBLANK(cnum1,11,n1,n2)
	   string='Line length = '//cnum1(n1:n2)//' ms: Line sep = '
	   call GBLANK(cnum2,11,n1,n2)
	   string=charnb(string)//cnum2(n1:n2)//' pA'
	   n=nblank1(string)
	   xstr=125.
	   ystr=5.
	   call JUSTIFYSTRING(xstr,ystr,string(1:n),
     &	 0.,csize,-1)	!3=base justification
c
c Put out the current page
	   call devend
	   call timer(iticks1)
	   call GINO
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
666	   INQUIRE(file=oldfile,exist=present,flen=len)
	   if(present) then
		kp=kp+1
c          At present haveen't got enough file handles (and disk space?) for
c          more than 23 output files, so for 24'th erase fit1.out (and hope
c	     that by this time it has already been plotted)
		if(kp.gt.23) kp=1
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
c   Leave time=30 seconds!
		call timer(iticks2)
		if((iticks2-iticks1).lt.3000) goto 666
		print 73,oldfile
73		format(' Printer file not found: ',a33)
		pause
	   endif
200	continue	!end of npage=npage1,npage2 loop
c
c Finished
	call mode(18)
	idev=0
	call GRAPHBOUNDARY(xlos,xhis,ylos,yhis)
	call gSCALE(xmin,xmax,ymin,ymax,is)
	call FILLWIN(0,388,639,479,1)			!dialog area
	call FILLWIN(0,0,639,387,icol(71))		!graph area
	call OPENDIALOG(1,7,.true.)		!draw dialog box #1
	if(istart.gt.np2) goto 99
	call FILLWIN(0,0,639,387,ibk)		!graph area
	call FILLWIN(0,0,639,387,icol(71))		!graph area
	mono=.false.
	npage=npage1	!redisplay (first) page in plotted range
	istart=np1+(npage-1)*nppage
	iend=istart+nppage-1
	iread=iend-istart+1		!normally=nppage
	OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &       access='TRANSPARENT')
	irec=ioff + (2*istart-1)    !record # for 1st value to be read
	read(14,rec=irec) (idata(i),i=1,iread)
	CLOSE(unit=14)
	do i=1,iread
	   Yv=calfac*float(idata(i))
	   yval(i)=yv
	enddo
	goto 125	!redisplay
c
99	continue
	if(allocated(idata)) DEALLOCATE(idata,yval,xval)		!when done
      OPEN(unit=12,file='PLOTSAMP.INI',status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=128)
	write(12,rec=1) adcfil
	CLOSE(unit=12)
c	call DEVEND
 	call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	call NUMCLR		!numlock off
	end
