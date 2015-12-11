	subroutine VSAMP(i1,i2,ib1,ib2,consamdef,adcfil,nline,yline,
     &  ybase,amp,imode,colseq,j1,j2,line1,line2,line3,reject,
     &  stop,qfile,nsam,srate,irecfst,calfac,ioffset)
c To show a single channel record on two time scales
c To show bursts from a single channel data file in EKDIST
c i1,i2=index in CONSAM of first last point for the long display
c ib1,ib2=index in CONSAM of first last point for the expanded display
c nline=number of dashed amplitude marker lines to be drawn, at the
c    levels stored in yline (should have correct sign)
c ybase=baseline level (shut level, pA), from which lines are drawn
c imode=0 interprets ybase as float(ibaselin) and uses for baseline
c imode=1 causes baseline to be estimated internally
c amp=abs(mean amp) of channels to use to calculate vertical gain
c	 (if neg then vertical scaling judged from data).  To keep
c      vertical scaling constant then call with the same (positive)
c	 value of amp each time.
c colseq=false, j1, j2 ignored
c colseq=true: points j1 to j2 coloured purple on bottom trace
c line=message to be written in dialog box (call with '' if no message wanted)
c reject=input: if reject=true when called, key 1 is labelled 'reject'  and
c		exits from VSAMP
c	   output: reject=true if exit via key 1; =false if exit via key 9
c stop=input: if stop=true when called, key 11 is labelled 'stop display'  and
c		exits from VSAMP
c	   output: stop=true if exit via key 15; =false if exit via key 1 or 9
c
c Modif 07/01/00 01:13pm
c  (1) 2nd and 3rd message (line2,line3) added (not written if call with '')
c
c Modif 05/01/99 05:08pm by adding STOP to call and by removing call MODE(18)
c at start, and call VIDEOMOD(3)at end so now expects to be in graphics mode
c before call, and remains in VGA after leaving
c
c Modif 05/02/99 10:20pm so GETCONS call can be made BEFORE the call to VSAMP
c (so whole of VSAMP can be in graphics mode) -GETCONS not called here if
c consamdef=true in call
c Therefore must add to parameters nsam, srate, irecfst, ioffset
c
	ALLOCATABLE::idata,yval,xval,xval1,xbot,ybot
	integer*2 idata(:)
	real*4 yval(:),xval(:) 		!top row
	real*4 xval1(:),xbot(:),ybot(:)	!bottom (expanded) row
	character line1*(*),line2*(*),line3*(*),titlex*40,titley*40
	character adcfil*33,ch*1,ans*1
	character cnum0*11,cnum1*11,cnum2*11,cnum3*11
	character tstr*11,tstr1*11
	logical noconsam,newform,cjdat,mono,first,opendown
	logical consamdef,noline,colseq,reject,rejin,stop,stopin
	logical autplt,plotcols
	integer videotyp
	integer icol(100)
	integer iax(10),iay(10)	!for CROSSH
	character title1*64,title*76	!plot title
	character*40 qfile

	character defolt*30,cans*30		!to hold text & result of QDIALOG

c For amp marker lines
	real*4 yline(nline),xhb(10),xhe(10),yhline(10)
c
	character*55 helpst(22)		!help strings
C For DCMENU
	integer Lb(30)
	character*78 text(30)
	common/DMENU/ifonb,csizem,ifont2,nboxlast,nblast	!for DCMENU

	common/popvalx/mxlo,mylo,myhi,ictx,ibkx,icfx	!values for poptext calls
	common/dialval1/ictd,icfd,icbd  !colours for dialog box #1 (text,frame,back)
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi
c	common/samp/xmin1,xmax1,ymin1,ymax1
c
	ioff=ioffset
	opendown=.true.
	noline=.false.
	rejin=reject
	stopin=stop
	stop=.false.
	reject=.false.
c defs for DCMENU
	icol1=14	!yellow text/border for boxes in DCMENU
	icol2=8	!grey background for boxes
c	icol3=12	!red text/border
c	icol4=15	!white fill
c	if(mono) then
c	   icol3=15		!white text/border
c	   icol4=0		!black fill
c	endif
	ifonb=0
	ifont2=0 	!copy for common/dmenu/
	csizem=2.
	icf=7		!frame colour for dialog box 1
c	icf2=12	!frame colour for dialog box 2
c	ict=11	!text colour for dialog box 1
	nboxlast=15		!in case attempt to delete boxes before any drawn
	nblast=5
c Values for popmenu
c	nxlo=50	!pixels
c	nylo=-1
c	nyhi=370    !pixels
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
c Lahey colours:
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
	ibk1=1           	!BACKGROUND COLOR for upper data area
	ibk2=2           	!BACKGROUND COLOR for lower data area
	ibk=3           	!BACKGROUND COLOR for outside data area
	icw=11		!colour to write in dialog box (light blue)
c	icw1=12		!red to write in dialog box for warnings
c	common/dialval1/ictd,icfd,icbd  !colours for dialog box #1 (text,frame,back)
	ictd=icw		!icw is older notation -better use value in common/dialval/
	icbd=0			!background col for dialog box (0 until fixed)
c	ictm=15		!text white
c	ibkm=1		!background dark blue
c	icfm=11		!frame/title light blue
c	icupm=12		!upper case red
	iyd1=127	!lowest point on screen (Y pixels) for display of channels
c	iyd2=457	!highest point on screen (Y pixels) for display of channels
	iyd2=479
	yd1=float(iyd1)
c	yd2=float(iyd2)
	iydm=(iyd1+iyd2)/2		!divide two windows
c
	autplt=.false.
	plotcols=.true.
	do i=1,100
	   icol(i)=-1
	enddo
	isetcol=0
	call setcls(mono,icol,autplt,plotcols,isetcol)
	ic=15
	if(mono) ic=15		!bright white
	if(.not.consamdef) then
c	   print 30
c30	   format(
c     & ' (1) Continuous data (CONSAM or AXON)',/,
c     & ' (2) CJUMP data (not yet fixed)',/,
c     & ' Option number [1] = ')
c	   iopt=1
c	   call INPUTi(iopt)
c	   cjdat=iopt.eq.2
	   cjdat=.false.
c Get consam/axon file name
	   call GETCONS(adcfil,noconsam,title,cdate1,adctim1,nsam,
     &    srate,cjdat,irecfst,newform,ioffset,calfac,ffilt,idest)
	   ioff=ioffset
c	   ndatj=nsam
	   if(noconsam) goto 99
	   consamdef=.true.
c	   INQUIRE(file=adcfil,exist=present,flen=len)
c	   OPEN(unit=14,file=adcfil,status='UNKNOWN',
c     &       access='TRANSPARENT')
c	   dx=1000./srate		!ms between points
c	   tsamlen=float(nsam)/srate	!seconds
c
c	   np1=1
c	   np2=nsam
c361	   print 36,nsam,tsamlen,np1,np2
c36	   format(
c     &  ' CONSAM contains ',i10,' points: length = ',g13.6,' seconds',/,
c     &  ' Plot from point n1 to n2: n1,n2 [',i9,',',i9,'] = ')
	endif
c If consam file is defined, start here
	ntop=i2-i1+1
	nbot=ib2-ib1+1
	if(allocated(idata)) DEALLOCATE(idata,yval,xval,xval1)
	ALLOCATE(idata(ntop),yval(ntop),xval(ntop),xval1(ntop))
c
	istart=i1
	iread=ntop
	OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &       access='TRANSPARENT')
	irec=ioff + (2*istart-1)    !record # for 1st value to be read
	read(14,rec=irec) (idata(i),i=1,iread)
	CLOSE(unit=14)
	ymin=1.e37
	ymax=-1.e37
	do i=1,iread
	   Yv=calfac*float(idata(i))
	   yval(i)=yv
	   if(yv.lt.ymin) ymin=yv
	   if(yv.gt.ymax) ymax=yv
	enddo
c
c Define xval, xval1 for the two rows -but simpler to use one long yval() array
c Can have only one scale (in ms) for the whole graph, so it can't be right
c for both displays.  Make the X scale in ms for the long display dx=ms between
c points.  For lower (expanded) display acual time between points is same, but
c use dx1 such that display fills the screen
	dx=1000./srate		!ms between points
	fac=float(ntop)/float(nbot)
	dx1=fac*dx
	xmin=0
	xmax=float(ntop)*dx
	do i=1,ntop
	   xval(i)=xmin + float(i-1)*dx
	enddo
	do i=1,nbot
	   xval1(i)=xmin + float(i-1)*dx1
	enddo
c
	thj=0.0		!?
c
c If imode=1 then try to estimate baseline here -look for points that
c are close to ymax (if openings downwards) -start at ymax, and move an
c amp window downwards, averaging all points within the window -take
c the average tha has largest number of points as the baseline estimate
c Baseline noise is usually about 0.2 pA so take window width=0.8 pA
c or 0.6 pA?
	if(imode.eq.0) then
	   ybase=ybase*calfac
	else if(imode.eq.1) then
	 first=.true.
	 if(opendown) then
	   winwid=0.2
	   winhi=ymax
	   winlo=ymax-winwid
	   nlast=-1
	   ntry=0
21	   n=0
	   ytot=0.0
	   do i=1,ntop
		yv=yval(i)
		if(yv.le.winhi.and.yv.gt.winlo) then
		   n=n+1
		   ytot=ytot+yv
		endif
	   enddo
	   if(n.gt.0) ybase=ytot/float(n)
	   if(n.lt.nlast) then	!can't be true first time
		ybase=ylast
	   else
  		first=.false.
		nlast=n
		ylast=ybase
		winhi=winlo
		winlo=winhi-winwid
		ntry=ntry+1
		if(ntry.lt.21) goto 21
	   endif
	 endif
	endif
c
c Normalise data to baseline=0
	do i=1,iread
	   yval(i)=yval(i)-ybase
	enddo
	ymin=ymin-ybase
	ymax=ymax-ybase
	ybase1=0.
c
c Screen like SCAN, but divide into two background coloure regions, one
c for each trace? Or change colour as mouse moves fom one reion to other?
c Start graphics
1000  continue
	if(VIDEOTYP().ne.18) then
	   idev=0			!screen
	   call gsetcols(0)
	   call errswi(-1)
	   call brkswi(1)
	   call chaswi(1)
	   call grfmod (1)
	   call harcha
	   call papenq(xp,yp,ipap)
c Want to set graphboundary to correspond to data window, bottom of which
c is at iyd1=127 pixels, so need graph units which correspond to this
	   ix=0
c	   call PIXGRA(ix,iyd1,xg,yg)
	   vxlo=0	! for VIEWPORT
	   vxhi=xp
	   vylo=0
c	   vylo=yg
	   vyhi=yp
	   xlo=0		! for graphboundary
	   xhi=xp
	   ylo=0
	   yhi=yp
c	   xlo=0.05*xp			! set display location on screen
c	   xhi=0.95*xp
c	   ylo=0.05*yp
c	   yhi=0.8*yp	!lower to leav room for dialog box
c	   call mode(18)
c	   icf=7		!frame colour for dialog box 1
c	   ict=11	!text colour for dialog box 1
	endif
	call axiset
	call GRAPHBOUNDARY(xlo,xhi,ylo,yhi)
	if(.not.mono) call lincols(ic,idev)
	call LINWID(thj)
c
	call FILLWIN(0,0,639,iyd1-1,ibk)	!fill screen with light blue (for outside data area)
251	continue
c	call FILLWIN(0,iyd1,639,iyd2,ibk1)	!fill data area with colour ibk
	call FILLWIN(0,iydm+1,639,iyd2-1,ibk1)	!fill upper data area with colour ibk
	call FILLWIN(0,iyd1+1,639,iydm-1,ibk2)	!fill lower data area with colour ibk
	call HLINE(0,639,iyd1,14)		!line to mark bottom of data area
	call HLINE(0,639,iyd2,14)	!line to mark top of data area
	call HLINE(0,639,iydm,14)	!divide two areas
c Open a 3 row dialog box top=row 23; cols 2-58
c	subroutine OPENDIALOG(ibox,icf,shadow)
	call DEFDIALOG(1,23,2,3,68,icbd)
	call DEFDIALOG(1,23,2,3,68,icbd)
	call OPENDIALOG(1,7,.true.)	!shadow border (blue=1) (7=white/grey)
	nstr=nblank(line1)
	if(nstr.gt.0) then
	   call WDIALOG(1,line1(1:nstr),icw)
	endif
	nstr=nblank(line2)
	if(nstr.gt.0) then
	   call WDIALOG(1,line2(1:nstr),icw)
	endif
	nstr=nblank(line3)
	if(nstr.gt.0) then
	   call WDIALOG(1,line3(1:nstr),icw)
	endif
c	subroutine DEFBOX(ibox,irhi,iclo,nrow,ncol,icb)
	call DEFBOX(1,9,2,1,11,icbd)        !for length of top trace
	call DEFBOX(2,20,2,1,11,icbd)        !for length of bottom trace
	call OPENBOX(1,7,.true.)
	call OPENBOX(2,7,.true.)
c Calculate offset for each trace
c The vertical scale for the screen, ymins, ymaxs, must allow room for
c both traces within the data area
c Data area is (in pixels) from yd1=127 to 479, and make this 4*channel height
c
	if(amp.lt.0.) then
	   delt=ymax-ymin
	else
	   delt=amp
	endif
	dfac=yd1/480.
	ymins=0.0
	ymaxs=(1.+dfac)*4.0*delt	!each data window is 2*delt in height
	is=3
	call gSCALE(xmin,xmax,ymins,ymaxs,is)
	call spagra(vxlo,vylo,wxmin,wymin)
	call PIXGRA(ix,iyd1,xoff,yoff1)	!yoff corresp to data area above iyd1
	call PIXGRA(ix,iydm,xoff,yoffm)	!yoff corresp to data area above iydm
c=	yofftop=yoffm-ymin + 0.5*delt
c=	yoffbot=yoff1-ymin + 0.5*delt
c Baseline is now zero, so offset it to 3/4 of way up data window (each of
c which has height=2*delt)
	yofftop=yoffm + 1.5*delt
	yoffbot=yoff1 + 1.5*delt

c
c Debug -check ymin, ymax
c	call lincols(12,idev)
c	call gramov(xmin,yoff1)   !move to 1st point in range
c	call broken(linetype)
c	call gralin(xmin,ymaxs)   !join with line from last point
c	call gralin(xmax,ymaxs)   !join with line from last point
c	call gralin(xmax,yoff1)   !join with line from last point
c	call gralin(xmin,yoff1)   !join with line from last point

c	call lincols(13,idev)
c	call gramov(xmin,ymins)   !move to 1st point in range
c	call broken(linetype)
c	call gralin(xmin,ymaxs)   !join with line from last point
c	call gralin(xmax,ymaxs)   !join with line from last point
c	call gralin(xmax,ymins)   !join with line from last point
c	call gralin(xmin,ymins)   !join with line from last point
c
	call lincols(ic,idev)
c
c	ishift=50
	ishift=nbot/10
	if(ishift.lt.1) ishift=1
	expfac=2.
	linetype=0
c
c Draw tics at 1 pA intervals starting at estimated baseline
	call DOTIC(xmin,xmax,yoffm,yoff1,ybase1,yoffbot)
	call lincols(ic,idev)	!reset
c
c Draw top curve
301	continue	!return to redraw after cursors used
	ttop=float(ntop)*dx	!in ms
	call DCFORMAT(ttop,8,1,cnum0)
	n=NBLANK(cnum0)
	tstr1=cnum0(1:n)//' ms'
	call OPENBOX(1,7,.true.)
	call WRITBOX(1,tstr1,1,10)
	do i=1,ntop
	   xv=xval(i)
	   yv=yofftop + yval(i)
	   if(yv.ge.ymins.and.yv.le.ymaxs) then	!on screen
		if(i.eq.1) then
		   call gramov(xv,yv)   !move to 1st point in range
		else
		   call broken(linetype)
	  	   call gralin(xv,yv)   !join with line from last point
		endif
	   endif
	enddo
c
300	continue	!return to redraw after expand/shift
c
c Overwrite the expanded part of the top panel in red
	call lincols(12,idev)
	ioff1=ib1-i1
	ioff2=ib2-i1
	if(ioff1.lt.1) ioff1=1
	if(ioff2.gt.ntop) ioff2=ntop
	first=.true.
	do i=ioff1,ioff2
	   xv=xval(i)
	   yv=yofftop + yval(i)
	   if(yv.ge.ymins.and.yv.le.ymaxs) then	!on screen
		if(first) then
		   call gramov(xv,yv)   !move to 1st point in range
		   first=.false.
		else
		   call broken(linetype)
	  	   call gralin(xv,yv)   !join with line from last point
		endif
	   endif
	enddo

c Draw bottom trace. This needs points ib1 to ib2, and idata(1) starts at
c point consam(i1)
302	continue	!return to redraw after remove amp lines
	call lincols(ic,idev)	!reset line colour
	call DOTIC(xmin,xmax,yoffm,yoff1,ybase1,yoffbot)
	call lincols(ic,idev)	!reset
c
	ioff1=ib1-i1
	ioff2=ib2-i1
	if(ioff1.lt.1) ioff1=1
	if(ioff2.gt.ntop) ioff2=ntop
	nbval=ioff2-ioff1+1
	if(allocated(xbot)) deallocate(xbot,ybot)
	allocate(xbot(nbval),ybot(nbval))
	first=.true.
	tbot=float(nbot)*dx	!in ms
	dxb=tbot/(xmax-xmin)
	j=1
	do i=ioff1,ioff2
	   xv=xval1(i-ioff1+1)		!start at xval1(1)
	   yv=yoffbot + yval(i)
c	   xbot(j)=xv
c	   ybot(j)=yv
	   xbot(j)=xv*dxb
	   ybot(j)=yval(i)
	   if(yv.ge.ymins.and.yv.le.ymaxs) then	!on screen
		if(first) then
		   call gramov(xv,yv)   !move to 1st point in range
		   first=.false.
		else
		   call broken(linetype)
	  	   call gralin(xv,yv)   !join with line from last point
		endif
	   endif
	   j=j+1
	enddo
c If colseq, then overwrite points j1 to j2 of lower trace in purple
	if(colseq) then
	   call lincols(13,idev)
	   joff1=j1-ib1+1
	   joff2=j2-ib1+1
	   if(joff1.lt.1) joff1=1
	   if(joff2.gt.nbot) joff2=nbot
	   first=.true.
	   do i=joff1,joff2
		xv=xval1(i)			!start at xval(j1-ib1)
		yv=yoffbot + yval(ioff1 + i -1)
		if(yv.ge.ymins.and.yv.le.ymaxs) then	!on screen
		   if(first) then
			call gramov(xv,yv)   !move to 1st point in range
			first=.false.
		   else
			call broken(linetype)
			call gralin(xv,yv)   !join with line from last point
		   endif
		endif
	   enddo
	endif
c Draw amplitude marker lines on bottom trace, if any
	if(nline.gt.0.and.(.not.noline)) then
c Draw estimated baseline for bottom trace
	   call lincols(13,idev)
	   call broken(2)
	   call gramov(xmin,yoffbot+ybase1)
	   call gralin(xmax,yoffbot+ybase1)
c
c      draw the amp lines
	   call lincols(11,idev)
	   ltype=1
	   do i=1,nline
		yv=yoffbot + ybase1 + yline(i)
		call gramov(xmin,yv)
		call broken(ltype)
		call gralin(xmax,yv)
	   enddo
	endif
c Draw box for bottom window (after lines so they don't overwrite it)
	call DCFORMAT(tbot,8,3,cnum1)
	n=NBLANK(cnum1)
	tstr=cnum1(1:n)//' ms'
	call OPENBOX(2,7,.true.)
	call WRITBOX(2,tstr,1,10)
c
	call broken(linetype)
	call lincols(ic,idev)	!reset line colour
c
c Initial display finished -now adjust it?
c Put up menu
	call pixpos(0,53,xsc,ysc)
	call papenq(xp,yp,ipap)
	ymenu=yp-ysc	!needed in common for position of DCMENU
	call DCMENU(0,5,Lb,text,ibk,ibk)  !delete all (to non-data background col)
150	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	if(.not.rejin) LB(1)=0
	if(nline.eq.0) LB(8)=0
	LB(13)=0
	LB(14)=0
	if(.not.stopin) LB(15)=0
	call NUMSET			!set num lock on ready for response
151	continue
	text(1)='1: REJECT '
	text(2)='2: CONTRACT  '
	text(3)='3: EXPAND    '
	text(4)='4. USE CURSOR '
	text(5)='5:SHIFT LEFT '
	text(6)='6:SHIFT RIGHT'
	text(7)='7:SHOW INDEX'
	if(noline) then
	   text(8)='8:SHOW AMP LINES'
	else
	   text(8)='8:HIDE AMP LINES'
	endif
	text(9)='9: ACCEPT/NEXT '
	text(10)='0:QUEUE PLOT'
	text(11)='+:MORE SENSITIVE'
	text(12)='-:LESS SENSITIVE'
	text(13)='*:              '
	text(14)='/:SCROLL DIALOG'
	text(15)='.:STOP DISPLAY'
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
c	call DCMENU(-4,5,Lb,text,icol3,icol4)	!highlight set baseline
c
30	continue
152	call CKEY(ch,ikey)
c	if(ikey.eq.-1.or.ikey.eq.15) then		!F1=help
	if(ikey.eq.-1) then		!F1=help
	   i=0
	   i=i+1
	   helpst(i)=
     &     'Shift the lower curve left or right with keys 5 & 6 '
	   i=i+1
	   helpst(i)=
     &     '  or with arrow keys.  Sensitivity toggled with INS '
	   i=i+1
	   helpst(i)=
     &     '1.REJECT -when this shows, the burst is rejected '
	   i=i+1
	   helpst(i)=
     &     '2.CONTRACT: Contract the display (+,- control amount)'
	   i=i+1
	   helpst(i)=
     &     '3.EXPAND: Expand the display     (+,- control amount)'
	   i=i+1
	   helpst(i)=
     &     '4.USE CURSOR: Define region to expand on the top trace '
	   i=i+1
	   helpst(i)=
     &     ' by moving cursor to mark start and end point          '
	   i=i+1
	   helpst(i)=
     &     '5.SHIFT LEFT: Move back          (+,- control amount)'
	   i=i+1
	   helpst(i)=
     &     '6.SHIFT RIGHT: Move forward     (+,- control amount)'
	   i=i+1
	   helpst(i)=
     &     '7.SHOW INDEX: Display index of displayed point'
	   i=i+1
	   helpst(i)=
     &     '9.ACCEPT/NEXT: Leave this display -show next'
	   i=i+1
	   helpst(i)=
     &     ' (when 1.REJECT shows, this accepts burst)  '
	   i=i+1
	   helpst(i)=
     &     '0.QUEUE PLOT: Queue the display in lower window'
	   i=i+1
	   helpst(i)=
     &     '  for plotting later using AUTPLOT'
	   ntext=i
	   i=i+1
	   helpst(i)=
     &     '.:STOP DISPLAY: exit and show no more displays'
	   call POPTEXT(mxlo,mylo,myhi,helpst,ntext,ictx,ibkx,icfx)
	   goto 152	!carry on
	endif
c
	if(ikey.eq.2.or.(ikey.ge.3.and.ikey.le.6)) then
c Clear lower data area
	   call FILLWIN(0,iyd1+1,639,iydm-1,ibk2)	!fill lower data area with colour ibk
c Overwrite red bit on upper trace
	   call lincols(ic,idev)
	   first=.true.
	   do i=ioff1,ioff2
		xv=xval(i)
		yv=yofftop + yval(i)
		if(yv.ge.ymins.and.yv.le.ymaxs) then	!on screen
		   if(first) then
			call gramov(xv,yv)   !move to 1st point in range
			first=.false.
		   else
			call broken(linetype)
	  		call gralin(xv,yv)   !join with line from last point
		   endif
		endif
	   enddo
	endif

	SELECT CASE(ikey)
	   case(1)
		if(rejin) then	!key 1 not visible when rejin=F
		   call BELL(1)
		   ans='Y'
 		   call DEFOLTa(ans,defolt)
		   call QDIALOG(1,
     &	   'Reject this event',defolt,12,cans)
		   call GETINPa(cans,ans)
		   if(ans.eq.'Y') then
			reject=.true.
			goto 99
		   endif
		endif

	   case(2)		!contract
		imid=(ib2+ib1)/2		!midpoint of bottom trace
		n=ifix(float(nbot)*expfac)
		if(n.le.ntop) nbot=n
c		ib2=ib1+nbot-1		!ib1 not changed
c		if(ib2.gt.i2) then
c		   ib2=i2	!can't go beyond data in upper trace
c		endif
c  Try contracting around centre point
		np1=nbot/2
		ib1=imid-np1
		ib2=imid+np1
		if(ib1.lt.i1) ib1=i1	!can't go beyond data in upper trace
		if(ib2.gt.i2) ib2=i2	!can't go beyond data in upper trace
		nbot=ib2-ib1+1
		fac=float(ntop)/float(nbot)
		dx1=fac*dx
		do i=1,nbot
		   xval1(i)=xmin + float(i-1)*dx1
		enddo
		goto 300	!redraw

	   case(3)		!expand
		imid=(ib2+ib1)/2		!midpoint of bottom trace
		nbot=ifix(float(nbot)/expfac)
c		ib2=ib1+nbot-1		!ib1 not changed
c     expand around centre point
		np1=nbot/2
		ib1=imid-np1
		ib2=imid+np1
		if(ib1.lt.i1) ib1=i1	!can't go beyond data in upper trace
		if(ib2.gt.i2) ib2=i2	!can't go beyond data in upper trace
		nbot=ib2-ib1+1
		fac=float(ntop)/float(nbot)
		dx1=fac*dx
		do i=1,nbot
		   xval1(i)=xmin + float(i-1)*dx1
		enddo
		goto 300	!redraw

	   case(4)
20	      ix=300		!initial pos for cursors (pixels)
	      iy=0
	      icf=2
	      itype=0
	      line=2
	      ndraw=1
	      islope=0
	      ixlo=1
	      iylo=iydm
	      ixhi=639
	      iyhi=iyd2
	      ieps=1	!sensitivity
		call WDIALOG(1,
     &'Move cursor to start of bit to be enlarged on top trace',icw)
		call WDIALOG(1,
     &' -then click left mouse button',icw)
c	subroutine crosspix(ix,iy,ixlo,iylo,ixhi,iyhi,ibk,icf,itype,
c     &	line,ndraw,islope,iax,iay,ieps)
	      call CROSSPIX(ix,iy,ixlo,iylo,ixhi,iyhi,ibk2,icf,itype,
     &	line,ndraw,islope,iax,iay,ieps)
c iax() is in pixels
		ix1=iax(1)
		ix=ix1+100
		call WDIALOG(1,
     &'Click on end of bit to be enlarged on top trace',-icw)
	      call CROSSPIX(ix,iy,ixlo,iylo,ixhi,iyhi,ibk2,icf,itype,
     &	line,ndraw,islope,iax,iay,ieps)
c		ix2=iax(2)
		ix2=iax(1)
		call PIXGRA(ix1,iydum,x1,ydum)	!x1 in msec on top trace
		call PIXGRA(ix2,iydum,x2,ydum)	!x1 in msec on top trace
		ib1=i1 + x1/dx
		ib2=i1 + x2/dx
		if(ib1.lt.i1) ib1=i1
		if(ib2.gt.i2) ib2=i2
		nbot=ib2-ib1+1
		if(nbot.lt.5) then
		   call BELL(2)
		   call WDIALOG(1,'Not enough points: try again',12)
		   goto 20
		endif
		fac=float(ntop)/float(nbot)
		dx1=fac*dx
		do i=1,nbot
		   xval1(i)=xmin + float(i-1)*dx1
		enddo
		call FILLWIN(0,iydm+1,639,iyd2-1,ibk1)  !fill upper data area with colour ibk
		call OPENBOX(1,7,.true.)
		call WRITBOX(1,tstr1,1,10)
		goto 301	!redraw all to erase cursors

	   case(5)		!shift left
		ib2=ib2 + ishift
		if(ib2.gt.i2) then
		   ib2=i2	!can't go beyond data in upper trace
		endif
		ib1=ib2-nbot+1
		goto 300	!redraw

	   case(6)		!shift right
		ib1=ib1 - ishift
		if(ib1.lt.i1) then
		   ib1=i1	!can't go beyond data in upper trace
		endif
		ib2=nbot+ib1-1	!nbot not changed
		goto 300	!redraw

	   case(7)
		call INTCONV(i1,cnum0)
		call INTCONV(i2,cnum1)
		call INTCONV(ib1,cnum2)
		call INTCONV(ib2,cnum3)
		call WDIALOG(1,
     & 'Top trace from '//charnb(cnum0)//' to '//charnb(cnum1),icw)
		call WDIALOG(1,
     & 'Bottom trace from '//charnb(cnum2)//' to '//charnb(cnum3),icw)

	   case(8)
		noline=.not.noline
		call FILLWIN(0,iyd1+1,639,iydm-1,ibk2)	!fill lower data area with colour ibk
		goto 302

	   case(9)
		reject=.false.
		goto 99

	   case(10)
		iptype=33
		if(colseq) then
c		  jbff1=joff1-ioff1+1
c		  jbff2=joff2-ioff1+1
		  jbff1=joff1
		  jbff2=joff2
		else
		   jbff1=1
		   jbff2=1
		endif
		icp=13
	      xminb=0.0
	      xmaxb=tbot
	      xlo1=0.05*xhi
	      xhi1=0.95*xhi
	      ylo1=0.2*yhi			! location
	      yhi1=0.75*yhi			! screen
		titlex=' ms'
		titley=' pA'
		title1=charnb(line1)		!default title
		nhline=nline+1		!add one for baseline
		if(nhline.gt.10) nhline=10
		yhline(1)=ybase1
c		xhb(1)=xmaxb*0.03
c		xhe(1)=xmaxb*0.97
		xhb(1)=0.
		xhe(1)=xmaxb
	      do i=1,nline
		   i1=i+1
		   yhline(i1)=yline(i)
	         xhb(i1)=0.
	         xhe(i1)=xmaxb
		enddo
		call SORTr(yhline,nhline,10,.false.)
	      call VSPLQ(iptype,xbot,ybot,nbval,jbff1,jbff2,
     &      consamdef,nhline,yhline,xhb,xhe,ltype,colseq,ic,icp,
     &      xminb,xmaxb,ymin,ymax,XLO1,XHI1,YLO1,YHI1,
     &      title1,titlex,titley,qfile)

	   case(11)		!more sens
		expfac=expfac*2.
		ishift=ishift*2
		if(ishift.gt.ntop/4) ishift=ntop/4

	   case(12)		!less sens
		expfac=expfac/2.
		if(expfac.lt.1.1) expfac=1.1
		ishift=ishift/2
		if(ishift.lt.1) ishift=1	!min shift=1 point

	   case(15)
		if(stopin) then	!key 1 not visible when rejin=F
		   call BELL(1)
		   ans='Y'
 		   call DEFOLTa(ans,defolt)
		   call QDIALOG(1,
     &	   'Stop displaying bursts',defolt,12,cans)
		   call GETINPa(cans,ans)
		   if(ans.eq.'Y') then
			stop=.true.
			goto 99
		   endif
		endif

	END SELECT
	goto 152		!no valid option
c
c
99	continue
c	pause
	if(allocated(idata)) DEALLOCATE(idata,yval,xval,xval1)
	call NUMCLR()
c 	call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	RETURN
	end

	subroutine DOTIC(xmin,xmax,yoffm,yoff1,ybase,yoffbot)
c
	xtic=0.01*(xmax-xmin)
c	call PIXGRA(ix,iyd2,xoff,yoff2)	!yoff corresp to iyd2
	call lincols(14,idev)
	call gramov(xmin,yoff1)
	call gralin(xmin,yoffm)
	ybase1=ybase+yoffbot
	call gramov(xmin,ybase1)
	call gralin(xmin+xtic,ybase1)	!tic at ybase
	ntic=ifix(ybase1-yoff1)	!no of 1 pA tics below baseline in bottom window
	y=ybase1
	do i=1,ntic
	   y=y-1.
	   call gramov(xmin,y)
	   call gralin(xmin+xtic,y)	!tic at ybase
	enddo
c
	RETURN
	end

