	program PLOTSAMP
c To plot all or part of CONSAM file on multiple pages (as also available
c in AUTPLOT if a single channel record is queued)
c Initially draw first page on screen -then options to draw next (or any)
c page and options to plot one or all pages
	integer videotyp
	integer*4 icol(100)
	character filnam*32,prtport*4		!for winprint
	character cdate*11,cdate1*11,ctime*11,adctim1*8		!read from consam
	character adcfil*33,cnum0*11,cnum1*11,cnum2*11
	character ans*1,UC*1
	character getch*1,ch*1
	character*33 oldfile,printfil,wmetafil,metafil
	character*11 kpchar
	character string*75
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character*78 titles,strings(10),helps(10)		!for popmenu
	logical noconsam,newform,landplot,meta,badname,wmeta
	logical present,autplt,plotcols
	ALLOCATABLE::idata,yval,xval,popen,xpval
	integer*2 idata(:)
	real*4 yval(:,:),xval(:,:),popen(:),xpval(:)
	real*4 pgrid(10)
	logical mono,doframe,kbhit,lastpage
	character title*76
c for cjump.dat
	logical discprt
c for P(open) plot
	logical poplot,setbase,offset
c
	character*40 mtitle		!machine name
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi
	logical adcdef,cjdat
	COMMON/mtrace/adcfil,adcdef,ntrace,ytsep,calfac,srate,
     &  ioff,nsam,cjdat
c	COMMON/mtrace/ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil
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
102	format(' PLOTSAMP- Plotting of single channel data',/)
	call GETSPEC('plotsamp.exe')	!print details
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 1,cdate,ctime(1:8),mtitle
	if(discprt) write(8,1) cdate,ctime(1:8),mtitle
1	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
c	print 399
c399	format(
c     & ' CAPS LOCK on for debugging',/)
c
	setbase=.false.
	delay=0.0
	INQUIRE(file='plotsamp.ini',exist=present,flen=len)
	if(present.and.len.gt.0) then
         OPEN(unit=12,file='PLOTSAMP.INI',status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=128)
	   read(12,rec=1) adcfil,delay,poplot,tseg,ngrid,
     &	secline
	else
         OPEN(unit=12,file='PLOTSAMP.INI',status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=128)
	   adcfil='F:\consmjc3.006'
	   delay=60.
c delay = time between pages in seconds per megabyte
	   write(12,rec=1) adcfil,delay,poplot,tseg,ngrid,
     &	secline
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
	dtx=1000./srate		!ms between points
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
	npuse=np2-np1+1
c39	print 37
c	print 37
c37	format(' Number of seconds per page [1.0] = ')
c 	secpage=1.0
c	call INPUTr(secpage)

c Add option to interleave trace with Popen plot
	ans='N'
	if(poplot) ans='Y'
	call DCASK('Interleave traces with plot of P(open)',ans,ans)
	poplot=ans.eq.'Y'
	if(poplot) then
	   itseg=ifixr(tseg)
47	   continue	!return here for another run
	   print 52,itseg
52	   format(
     &' Length of segment over which P(open) measured (ms) [',i3,'] = ')
	   call INPUTi(itseg)
	   tseg=float(itseg)
	   npseg=ifixr(tseg/dtx)	!points/segment
c532	   nstep=nval/2
c	   print 53,nstep
c53	   format('&Increment for running average [',i3,'] = ')
c	   call INPUTi(nstep)
c	   if(nstep.gt.nval) then
c		print 531
c531		format(' Must not be greater than number averaged')
c		call BELL(1)
c		goto 532
c	   endif
c	   tcrit=-1.	!neg=no exclusions
c	   ans='N'
c	   call DCASK('Exclude long intervals',ans,ans)
c	   if(UC(ans).eq.'Y') then
c		print 55
c55		format(
c     & '&Exclude an open and shut time pair if either is longer',
c     & ' than Tcrit (ms) = ')
c		call INPUTr(tcrit)
c	   endif
c	   print 56,nval
c56	   format(
c     & '&Restart group of',i4,' open periods if bad gap found [N] ? ')
c	   ans='N'
c	   call INPUTa(ans)
c	   restart=UC(ans).eq.'Y'
c
c	   if(discprt) write(8,13) nval,nstep
c13	   format(
c     & ' Sections of',i5,' open periods averaged. Start increment= ',i5)
c	   if(tcrit.gt.0.) then
c		if(discprt) write(8,14) tcrit
c14		format(' All intervals longer than',f7.2,' ms omitted')
c	   endif
c	   if(restart) then
c		print 57,nval
c		if(discprt) write(8,57) nval
c57	   format(
c     & '&Group of',i4,' open periods restarted if bad gap found')
c	   else
c		print 58
c		if(discprt) write(8,58)
c58		format(' Bad gaps omitted without restarting group')
c	   endif
	   ans='Y'
	   call DCASK('Add gridlines to the P(open) plot',ans,ans)
c	   ngrid=0
	   if(ans.eq.'Y') then
c		ngrid=1
		print 59,ngrid
59		format(
     &   ' (1) One line at P=0.5',/,
     &   ' (2) Three lines at P = 0.25, 0.5, 0.75',/,
     &   ' (3) Specify lines',/,
     &   ' Option number [',i2,'] = ')
		call INPUTi(ngrid)
		ngrid1=ngrid	!save for .ini
		if(ngrid.eq.1) then
		   pgrid(1)=0.5
		else if(ngrid.eq.2) then
		   ngrid=3
		   pgrid(1)=0.25
		   pgrid(2)=0.5
		   pgrid(3)=0.75
		else if(ngrid.eq.2) then
		   print 60
60		   format(' Number of gridlines = ')
		   call INPUTi(ngrid)
		   do i=1,ngrid
611			print 61,i
61			format(' Gridline #',i2,' at P(open) = ')
			call INPUTr(pgrid(i))
			if(pgrid(i).gt.1.) goto 611
		   enddo
		endif
	   endif
c===log(y) scale option?
	endif		!end of Popen settings
c
	print 37,secline
37	format(' Number of seconds per line [',f6.1,'] = ')
c 	secline=1.0
	call INPUTr(secline)
	secline=secline*1000.	!ms/line
	ntrace=10		!default value
	if(poplot) ntrace=5
	print 370,ntrace
370	format(' Number of traces per page [',i3,'] = ')
	call INPUTi(ntrace)
c
	npline=ifixr(secline/dtx)		!points/line
	nppage=ntrace*npline			!points/page
	secpage=float(ntrace)*secline
	samlen1=dtx*(float(npuse-1))
	npagtot=1+ifix((samlen1-1.)/secpage)
	print 38,npagtot
38	format(
     &' This corresponds to ',i5,' pages for whole sample: O.K. [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).eq.'N') goto 361
c
	if(nppage.gt.npuse) nppage=npuse
c For last page
	ntrtot=1 + (npuse-1)/npline		!total number of traces=lines
	ntrlast=mod(ntrtot,ntrace)		!number of traces on last page
	nptlast=npuse-npline*(ntrtot-1)	!number of points on last line
	if(poplot) then
	   nsegline=ifixr(float(npline)/float(npseg))	!popen segments per line
	   nseglast=ifixr(float(nptlast)/float(npseg))	!popen segments on last line
c Calculate length of Popen array = number of segments per page, and allocate it
	   npop=1+ifixr(float(nppage)/float(npseg))
	   if(.not.allocated(popen)) then
		ALLOCATE(popen(npop),xpval(npop))
	   endif
	endif
c
c Read in first page and use min/max to get default line separation
	istart=np1	!to read first page in readsq1
	if(allocated(idata)) DEALLOCATE(idata,yval,xval)
	ALLOCATE(idata(nppage),yval(nppage,1),xval(nppage,1))
c
	npage=1
c
c Read the first page
	istart=np1+(npage-1)*nppage
	iend=istart+nppage-1
	if(iend.gt.npuse) iend=npuse
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
	   yval(i,1)=yv
	   if(yv.lt.ymin1) ymin1=yv
	   if(yv.gt.ymax1) ymax1=yv
	enddo
	if(poplot) then
	   offset=.false. 	!data NOT offset to zero baseline yet
	   s=0.0
	   do i=1,20
		s=s+yval(i,1)
	   enddo
	   yshut=s/20.		!guess for baseline position (or use ymin?)
c	   yshut=ymax1-0.15
	   ybase=yshut		!shut level in original data (no offset)
         yopen=ymin1+0.15		!initial default for poplot
	endif
	ytsep=1.3*abs(ymax1-ymin1)	!default
	print 371,ytsep
371	format(' Vertical separation of traces (pA) [',f7.2,'] = ')
	call INPUTr(ytsep)
c
c===code modified from multrace
	ijoinj=0		!solid line
	thj=0.0		!set line thickness=0 for screen (or too slow)
	xlos=xlo
	xhis=xhi
	ylos=ylo
	yhis=yhi
c
10	continue
c
c Calc gain and separation so ymin1=ymin, lowest displayable trace
c -but use ymin2 rather than ymin, to leave (10%) space at bottom for
c calibration bars.
c
c Calc dx value for new Xval
	if(poplot) then
	   ntrace2=2*ntrace
	else
	   ntrace2=ntrace
	endif
	ymin=0.0
	ymax=ymin+float(ntrace2)*ytsep*1.1	!10% extra for cal bars, in pA
c	rowlen=float(npline)*dtx       !=secline: dtx=ms between points at this stage
	xmin=0.0
	xmax=secline
	dx=(xmax-xmin)/float(npline-1)
c
c Define xval
	do i=1,npline
	   xval(i,1)=xmin + float(i-1)*dx
	enddo
c Get baseline for raw data when poplot
	if(poplot) then	!define shut and open levels
	   imode=1
	   call GETPOP(xval,yval,iread,ymin,ymax,xmin,xmax,
     &   yopen,yshut,offset,tseg,npseg,dtx,npline,popen,xpval,
     &   npops,npop,imode,nppage)
	   ybase=yshut		!baseline for raw data
c     change yshut, yopen for next display
	   yopen=yopen-yshut
	   yshut=0.
	endif

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
c If P(open) interleaved, then (first page only?) set the shut and open levels
c in GETPOP
	if(poplot.and.setbase) then	!define shut and open levels
	   imode=1
	   ysav=yshut
	   call GETPOP(xval,yval,iread,ymin,ymax,xmin,xmax,
     &   yopen,yshut,offset,tseg,npseg,dtx,npline,popen,xpval,
     &   npops,npop,imode,nppage)
	   ybase=ybase + (yshut-ysav)		!adjust ybase for drift
	endif
c
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
c
	ans='N'
 	call DEFOLTa(ans,defolt)
	call QDIALOG(1,
     &      'Draw frame round traces',defolt,11,cans)
	call GETINPa(cans,ans)
	doframe=ans.eq.'Y'

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
	if(poplot) then
c  Calc Popen values for current page
	   imode=0
	   call GETPOP(xval,yval,iread,ymin,ymax,xmin,xmax,
     &   yopen,yshut,offset,tseg,npseg,dtx,npline,popen,xpval,
     &   npops,npop,imode,nppage)
	endif
	linetype=ijoinj
c	iud=1				!draw line between points
c	if(linetype.eq.-1) iud=0	!don't
	ic=9
	ic=0				!black
	if(mono) ic=15		!bright white
	if(idev.ge.3.and.idev.le.5) ic=15
	if(.not.mono) call lincols(ic,idev)
	thj=0.0		!set line thickness=0 for screen (or too slow)
	call LINWID(thj)
	ip=0		!index for popen(ip), xpval(ip), ip=1,..., npops
	iy1=1		!element of yval to start at
	yoff=yofftop + ytsep	!so yoff=yofftop for first plot
	ntrace1=ntrace
	lastpage=npage.eq.npagtot
	if(lastpage) ntrace1=ntrlast
	do j=1,ntrace1
c	   yoff=yofftop-float(j-1)*ytsep
	   yoff=yoff-ytsep
	   n2=npline
	   if(lastpage.and.(j.eq.ntrace1)) n2=nptlast	!for last trace on last page
	   jflag=0
	   do i=1,n2
		xv=xval(i,1)
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
	   iy1=iy1+npline
	   if(poplot) then
		yoff=yoff-ytsep
		yboxtop=yoff+0.2*ytsep		!top of box (Popen=1)
		yboxh=0.8*ytsep		!box height
		yboxbot=yboxtop-yboxh	!bottom of box (Popen=0)
c        Draw box
		call broken(0)
		call gramov(0.,yboxtop)
		call gralin(0.,yboxbot)
		call gralin(xmax,yboxbot)
		call gralin(xmax,yboxtop)
		call gralin(0.,yboxtop)
c   add grid lines
		call broken(7)
		do i=1,ngrid
		   y=yboxbot+pgrid(i)*yboxh
		   call gramov(0.,y)
		   call gralin(xmax,y)
		enddo
		call broken(0)
c Plot Popen for current line in the box -
		xv=0.		!each line starts at x=0, and goes up to xmax
c At present xpval start at 0 and both xpval and popen go up to end of PAGE,
c not end of LINE
		if(.not.mono) call lincols(12,idev)
		xv=0.		!each line starts at x=0, and goes up to xmax
		ns=nsegline
		if(lastpage.and.(j.eq.ntrace1)) ns=nseglast	!for last trace on last page
		do i=1,ns
		   ip=ip+1
		   yv=yboxbot + popen(ip)*yboxh
		   if(i.eq.1) then
			call gramov(xv,yv)
		   else
			call gralin(xv,yv)
		   endif
		   call gralin(xv+tseg,yv)
		   xv=xv+tseg
		enddo
		if(.not.mono) call lincols(ic,idev)
	   endif
	enddo		!end of j=1,ntrace1
c
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
	call DCFORMAT(secline,8,1,cnum1)
	call DCFORMAT(ytsep,7,1,cnum2)
	call GBLANK(cnum1,11,n1,n2)
	string='     Line length = '//cnum1(n1:n2)//' ms: Line sep = '
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
	if(.not.poplot) then
	   strings(1)='1. Plot this page'		!ind=1 in old version
	   strings(2)='2. plot Specified pages'	!ind=1? in old version
	   strings(3)='3. plot All pages'		!ind=2 in old version
	   strings(4)='4. view Next page'		!ind=3 in old version
	   if(npage.eq.npagtot-1) then
		strings(4)='4. view Last page'
	   endif
	   strings(5)='5. View specified page'	!ind=3? in old version
	   strings(6)='6. Exit'               	!ind=4 in old version
	   nval=6
	else
	   strings(1)='1. Plot this page'		!ind=1 in old version
	   strings(2)='2. plot Specified pages'	!ind=1? in old version
	   strings(3)='3. plot All pages'		!ind=2 in old version
	   strings(4)='4. view Next page'		!ind=3 in old version
	   if(npage.eq.npagtot-1) then
		strings(4)='4. view Last page'
	   endif
	   strings(5)='5. View specified page'	!ind=3? in old version
	   strings(6)='6. Reset shut & open levels'               	!ind=4 in old version
	   strings(7)='7. Exit'               	!ind=4 in old version
	   nval=7
	endif
	nhelp=0
	call POPMENU(nxlo,nylo,nyhi,strings,nval,ictm,icfm,
     & icupm,ibkm,titles,helps,nhelp,iline,charout,ival)
	iopt=iline

	setbase=.false.
	if(iopt.eq.0) goto 99	!iline=0 for ESC=cancel
	if(.not.poplot) then
	   if(iopt.eq.6) goto 99
	else
	   if(iopt.eq.7) goto 99
	endif
	if(iopt.eq.1) then
	   npage1=npage
	   npage2=npage
	else if(iopt.eq.2) then
	   npage1=1
	   npage2=npagtot		!default
 	   call DEFOLT2i(npage1,npage2,defolt)
	   call QDIALOG(1,'Plot pages n1 to n2: n1,n2',defolt,11,cans)
	   call GETINP2i(cans,npage1,npage2)
	else if(iopt.eq.3) then
	   npage1=1
	   npage2=npagtot		!default
	else if(iopt.eq.4) then
	   npage=npage+1	!default
	else if(iopt.eq.5) then
	   npage1=npage+1	!default
	   if(npage1.gt.npagtot) npage=npagtot
 	   call DEFOLTi(npage1,defolt)
	   call QDIALOG(1,'Show page number',defolt,11,cans)
	   call GETINPi(cans,npage1)
	   npage=npage1
	else if(poplot.and.iopt.eq.6) then
	   setbase=.true.
c==	   call FILLWIN(0,0,639,387,icol(71))		!graph area
	   goto 125		!redisplay same page
	endif
c
	if(iopt.eq.1.or.iopt.eq.2.or.iopt.eq.3) goto 100	!sep plotting section
c
c  Still on screen -get the page specified above
	istart=np1+(npage-1)*nppage
	iend=istart+nppage-1
	if(iend.gt.npuse) iend=npuse
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
	   Yval(i,1)=calfac*float(idata(i))
	enddo
	if(setbase) goto 125	!use raw data to reset baseline?
	if(poplot) then
	   do i=1,iread
		Yval(i,1)=Yval(i,1) - ybase
	   enddo
	   offset=.true.		!offset to zero baseline
	endif
	call FILLWIN(0,0,639,387,ibk)		!graph area
	call FILLWIN(0,0,639,387,icol(71))		!graph area
	mono=.false.
	goto 12
c
c SEPARATE SECTION FOR HARD COPY PLOTTING (iopt=1,2,3)
c Plot all pages from npage1 to npage2, defined above
100	continue
	strings(1)='1. Printer'
	strings(2)='2. Metafile (*.cgm)'
	strings(3)='3. Windows metafile (*.wmf)'
	strings(4)='4. Exit '
777	nhelp=0
	call POPMENU(nxlo,nylo,nyhi,strings,nval,ictm,icfm,
     & icupm,ibkm,titles,helps,nhelp,line1,charout,ival)
      if(line1.ge.1.and.line1.le.nval) then	!iline=0 for ESC=cancel
	   ihelp=line1
	else
	   ihelp=8
	endif
444   continue
	if(ihelp.eq.1) then
	   ans='Y'
 	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     &      ' Is the printer ready',defolt,11,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'N') goto 103
	   goto 110
      else if (ihelp.eq.2) then
	   meta=.true.
	   idev=2
      else if (ihelp.eq.3) then
	   wmeta=.true.
	   mono=.false.
	   idev=1
	else
	   goto 99
	endif
	goto 400
110 	call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
c
c Decide which plotter to use and set boundaries
c	ip=1
c	mpos=6
c	call PLOTAUT(.false.,.false.,screen,quarter,ifont,csize,thbig,
c     &      idev,mpos,ip) 	!sets 'thick'=thbig in common for VPLOTQ
	print 2
2	format(' [NB postscript files may be large and slow to print]',/,
c     &   ' (1) Postscript printer',/,
c     &   ' (2) Laserjet printer',/,
c     &   ' (3) Colour Deskjet printer',/,
     &   ' (1) Laserjet printer',/,
     &   ' (2) Colour Deskjet printer',/,
     &   ' (3) Postscript printer',/,
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
c
	print 4
4	format(' Line thickness for plot (0=fastest to 1 = thick) [0] = ')
	thjp=0.0
	call INPUTr(thjp)
c
c idev=0: screen
c idev=1: .wmf
c idev=2: .cgm
c idev=3,4: postscript printer
c idev=5  : laserjet
c idev=6  : color deskjet
c
	if(iprinter.eq.3) then
	    if(landplot) then
	 	idev=4
	    else
	 	idev=3
	    endif
	else if(iprinter.eq.1) then
	    idev=5
	else if(iprinter.eq.2) then
	    idev=6
	endif
c
c
	if(npage2.gt.npage1) then
	   print 7,delay
7	   format(
     &' Time to leave between pages (sec/megabyte) [',f6.2,'] = ')
	   call INPUTr(delay)
	endif
c
c Start the loop to plot all pages (NB need to restart GINO for each page
c and reset margins etc
400	kp=0		!to number output files
	do 200 npage=npage1,npage2
	   if(meta) then
		if(npage.eq.npage1) then
		   metafil='plot.cgm'
		   CALL WDIALOG(1,'OPTIONS FOR FILES NAMES ',11)
765		   continue
		   call TDIALOG(1,
     &		'Name for CGM metafile: (at lear 3 characters)',
     &    		 metafil,33,.false.,11)
		   nc=nblank1(metafil)
		   ndot=0
		   badname=.false.
		   do i=1,nc
			if(metafil(i:i).eq.'.') then
			   ndot=ndot+1
			   if(i.gt.9.or.i.lt.4) badname=.true.
			endif
		   enddo
		   if(ndot.gt.1) badname=.true.
		   if(badname) then
		      call wdialog(1,'YOU ENTERED AN INVALID FILE NAME',11)
		      goto 765
		   endif
		   call WDIALOG(1,
     &   'Assembling the metafile. . .',11)
		else
		   kp=kp+1
	         call INTCONV(npage,cnum1)
		   nm=nblank1(cnum1)
		   metafil=metafil(1:3)//'_'//cnum1(1:nm)//'.cgm'
		endif
		call devend
		ICGMDV=14
	 	OPEN(UNIT=ICGMDV,FILE=metafil,STATUS='UNKNOWN')
		CALL CGMBi
		CALL DEVICE(ICGMDV,0)
		call errswi(-1)
		call brkswi(1)
     		call gsetcols(0)
		vxloh=vxlo
		vxhih=vxhi
		vyloh=vylo
		vyhih=vyhi
		xloh=xlo
		xhih=xhi
		yloh=ylo
		yhih=yhi
		call papenq(xpm,ypm,itype)
		call GRAPHBOUNDARY(0.,xpm,0.,ypm)
		call gSCALE(xmin,xmax,ymin,ymax,is)
		call spagra(0.,0.,wxmin,wymin)
		call spagra(xpm,ypm,wxmax,wymax)
		call mode(18)
		call WDIALOG(1,'Metafile in progress . . .',12)
		goto 320
	   else if(wmeta) then
		if(npage.eq.npage1) then
		   wmetafil='plot.wmf'
		   CALL WDIALOG(1,'OPTIONS FOR FILES NAMES ',11)
7151 		   continue
		   call TDIALOG(1,
     &	'Name for windows metafile:(at least 3 characters)',
     &     wmetafil,33,.false.,11)
		   nc=nblank1(wmetafil)
		   ndot=0
		   badname=.false.
		   do i=1,nc
			if(wmetafil(i:i).eq.'.') then
			   ndot=ndot+1
			   if(i.gt.9.or.i.lt.4) badname=.true.
			endif
		   enddo
		   if(ndot.gt.1) badname=.true.
		   if(badname) then
		      call wdialog(1,'YOU ENTERED AN INVALID FILE NAME',11)
		      goto 7151
		   endif
		   call WDIALOG(1,
     &   'Assembling the wmetafile. . .',11)
		else
		   kp=kp+1
	         call INTCONV(npage,cnum1)
		   nm=nblank1(cnum1)
		   wmetafil=wmetafil(1:3)//'_'//cnum1(1:nm)//'.wmf'
		endif
		call devend
		wMETA=.TRUE.
		idpi=600
		ixoff=0
		iyoff=0
		iwid=4800
		ihei=3600
		call wmfp(idpi,ixoff,iyoff,iwid,ihei)
		call DEVFIL(wmetafil,0)
		call errswi(-1)
		call brkswi(1)
		call chaswi(1)
		call grfmod (1)
		call harcha
		call gsetcols(0)
		call GRAPHBOUNDARY(xlo,xhi,ylo,yhi)
		call gSCALE(xmin,xmax,ymin,ymax,is)
		call spagra(vxlo,vylo,wxmin,wymin)
		call spagra(vxhi,vyhi,wxmax,wymax)
		call mode(18)
		call WDIALOG(1,'Wmetafile in progress . . .',12)
		goto 320
	   endif
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
	   if(iprinter.eq.1.and.(.not.landplot)) then
		xp=196.7
		yp=280.5
		call devpap(196.7,280.5,itype)
	   endif
	   if(iprinter.eq.2.and.landplot) then
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
	   xlop=0.05*xpp			! set display location on screen
	   xhip=0.90*xpp
	   ylop=0.0*ypp
	   yhip=0.95*ypp	!lower to leav room for dialog box
	   call GRAPHBOUNDARY(xlop,xhip,ylop,yhip)
	   call gSCALE(xmin,xmax,ymin,ymax,is)
	   call mode(18)
	   linetype=ijoinj
	   ic=9
	   ic=0	!black
	   if(mono) ic=15		!bright white
	   if(idev.ge.3.and.idev.le.5) ic=15
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
320      continue
	   istart=np1+(npage-1)*nppage
c	   istart=1+(npage-1)*nppage
	   iend=istart+nppage-1
	   if(iend.gt.npuse) iend=npuse
	   iread=iend-istart+1		!normally=nppage
c  Read in new yval()
	   OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &       access='TRANSPARENT')
	   irec=ioff + (2*istart-1)    !record # for 1st value to be read
	   read(14,rec=irec) (idata(i),i=1,iread)
	   CLOSE(unit=14)
	   do i=1,iread
		Yval(i,1)=calfac*float(idata(i))
	   enddo
	   if(poplot) then
		do i=1,iread
		   Yval(i,1)=Yval(i,1) - ybase
		enddo
		offset=.true.		!offset to zero baseline
	   endif
	   if(poplot) then
c  Calc Popen values for current page
		imode=0
		call GETPOP(xval,yval,iread,ymin,ymax,xmin,xmax,
     &       yopen,yshut,offset,tseg,npseg,dtx,npline,popen,xpval,
     &       npops,npop,imode,nppage)
	   endif
c Plot this page
	   iy1=1		!element of yval to start at
	   ip=0
	   yoff=yofftop + ytsep	!so yoff=yofftop for first plot
	   ntrace1=ntrace
	   lastpage=npage.eq.npagtot
	   if(lastpage) ntrace1=ntrlast
	   do j=1,ntrace1
		yoff=yoff-ytsep
	      call LINWID(thjp)
		n2=npline
		if(lastpage.and.(j.eq.ntrace1)) n2=nptlast	!for last trace on last page
		jflag=0
		do i=1,n2
		   xv=xval(i,1)
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
	      iy1=iy1+npline
c
		if(poplot) then
		   yoff=yoff-ytsep
		   yboxtop=yoff+0.2*ytsep		!top of box (Popen=1)
		   yboxh=0.8*ytsep		!box height
		   yboxbot=yboxtop-yboxh	!bottom of box (Popen=0)
c        Draw box
		   call broken(0)
		   call gramov(0.,yboxtop)
		   call gralin(0.,yboxbot)
		   call gralin(xmax,yboxbot)
		   call gralin(xmax,yboxtop)
		   call gralin(0.,yboxtop)
c   add grid lines
		   call broken(7)
		   do i=1,ngrid
			y=yboxbot+pgrid(i)*yboxh
			call gramov(0.,y)
			call gralin(xmax,y)
		   enddo
		   call broken(0)
c Plot Popen for current line in the box -
		   xv=0.		!each line starts at x=0, and goes up to xmax
c At present xpval start at 0 and both xpval and popen go up to end of PAGE,
c not end of LINE
		   if(.not.mono) call lincols(12,idev)
		   xv=0.		!each line starts at x=0, and goes up to xmax
		   ns=nsegline
		   if(lastpage.and.(j.eq.ntrace1)) ns=nseglast	!for last trace on last page
		   do i=1,ns
			ip=ip+1
			yv=yboxbot + popen(ip)*yboxh
			if(i.eq.1) then
			   call gramov(xv,yv)
			else
			   call gralin(xv,yv)
			endif
			call gralin(xv+tseg,yv)
			xv=xv+tseg
		   enddo
		   if(.not.mono) call lincols(ic,idev)
	      endif
	   enddo	!end of j=1,ntrace1
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
	   call DCFORMAT(secline,8,1,cnum1)
	   call DCFORMAT(ytsep,7,1,cnum2)
	   call GBLANK(cnum1,11,n1,n2)
	   string='     Line length = '//cnum1(n1:n2)//' ms: Line sep = '
	   call GBLANK(cnum2,11,n1,n2)
	   string=charnb(string)//cnum2(n1:n2)//' pA'
	   n=nblank1(string)
	   xstr=160.
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
	   if(meta.or.wmeta) then
		call mode(18)
		goto 200
	   endif
	   call mode(3)
	   if(idev.eq.3.or.idev.eq.4) then
		oldfile='EPS.OUT'
	   else if(idev.eq.5) then
		oldfile='HPLJR.OUT'
	   else if(idev.eq.6) then
		oldfile='DJ500C.OUT'
	   endif
666	   INQUIRE(file=oldfile,exist=present,flen=len)
	   psize=float(len)/1.e6	!megabytes
	   idelay=1 + 100*ifix(delay*psize)	!number of 10 ms ticks
	   if(npage2.eq.npage1) idelay=0
	   if(present) then
		kp=kp+1
c          At present haven't got enough file handles (and disk space?) for
c          more than 23 output files, so for 24'th erase fit1.out (and hope
c	     that by this time it has already been plotted)
		if(kp.gt.20) kp=1
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
c		   call COPY (printfil,'lpt1')
	   	   call SYSTEM('copy/b '//charnb(printfil)//' lpt1')
		else if(idev.eq.5)then
c		   call COPY (printfil,'lpt1')
c	   	   call system('copy/b hpljr.out lpt1:')
	   	   call SYSTEM('copy/b '//charnb(printfil)//' lpt1')
		else if(idev.eq.6) then
c		   call COPY (printfil,'lpt1')
c	   	   call system('copy/b dj500c.out lpt1:')
	   	   call SYSTEM('copy/b '//charnb(printfil)//' lpt1')
		endif
		if(npage.eq.npage2) idelay=0	!no delay after last page
		call TIMER(itick1)
		itick2=itick1
		do while((itick2-itick1).lt.idelay)
		   call TIMER(itick2)
		enddo
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
	if(iend.gt.npuse) iend=npuse
	iread=iend-istart+1		!normally=nppage
	OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &       access='TRANSPARENT')
	irec=ioff + (2*istart-1)    !record # for 1st value to be read
	read(14,rec=irec) (idata(i),i=1,iread)
	CLOSE(unit=14)
	do i=1,iread
	   Yv=calfac*float(idata(i))
	   yval(i,1)=yv
	enddo
	if(poplot) then
	   do i=1,iread
		Yval(i,1)=Yval(i,1) - ybase
	   enddo
	   offset=.true.		!offset to zero baseline
	endif
	goto 125	!redisplay
c
99	continue
	if(allocated(idata)) DEALLOCATE(idata,yval,xval)		!when done
	secline=secline/1000.	!back to seconds
	ngrid=ngrid1		!for .ini
      OPEN(unit=12,file='PLOTSAMP.INI',status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=128)
	write(12,rec=1) adcfil,delay,poplot,tseg,ngrid,secline
	CLOSE(unit=12)
c	call DEVEND
 	call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	call NUMCLR		!numlock off
	end
