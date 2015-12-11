	subroutine AVER_ACT(nburst,indfst,indend,consfil,nlen,
     & avsamp,tpre,nsamp,nsamp1,dfinter,tcrit,idest,yval,ndimd,ndv1,
     & icfil,ncfil,nmean)
c
c=======================================================================
c
c Much modifed version of REAVER3 to average lined-up bursts/activations
c from a steady-state CONSAM record, using the fitting stored in
c SCANTEMP.DAT and SCANDAT.DAT (files located in GETSCFIL)
c  Before coming here, size of segments to be averaged specified
c in GETBURST (avsamp, ms), and first and last point in consam located
c in indfst, indend.
c
c Modif 09/17/97 03:39pm so totcur now in yval(i,2); this means that
c for huge arrays it is possible to use ndimd=2 rather than 3 (so
c residuals plot not possible in this case)
c
c Modif 05/05/97 08:53pm so that totals now kept in yval(i,3) rather
c than totcur, because need to keep totals so more can be added if
c we go back for another consam file -yval is already an argument so
c it is kept
c===TO DO
c (2) drawing of arrows -remove
c (3) Add calibration marks as in SCAN
c (5) Use ibline to set offset? -remove ibline altogether -add dragging to
c	 redo offset (also in reaver2, 3)
c (6) Add help for menu options
c (7) NB -may not have ncrit points after end of burst (and if nsamp
c	set smaller than longest burst, then ndat>nsamp)) -catch these!
c  Totals kept now in local array TOTCUR
c Lahey colours:
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c
	integer*4 indfst(nburst),indend(nburst)
	character consfil*40		!path for consam.dat file
	character ch*1,getch*1
	real*8 dfinter
	real*4 Yval(ndv1,ndimd)
	ALLOCATABLE::jcol,jcolz		!local in REAVER2, for IDCPIX1
	integer*1 jcol(:),jcolz(:)
	ALLOCATABLE::ixdisp,iydisp,iydat,iyavc
	ALLOCATABLE::ixdispz,iydispz			   !for zoomed data
	integer*2 ixdisp(:),iydisp(:),iydat(:),iyavc(:)
	integer*2 ixdispz(:),iydispz(:)
	ALLOCATABLE::Yfill		!for alignment
	real*4 yfill(:),ax(10),ay(10)
	logical zoomed,mono,discprt,pon,slock,showt0,allocated
c for read of consam header
	integer*2 idt,inchan,id1,id2,iver
	character cdate*11,adctime*8,title*76,title1*70		!read from consam
	character tapeID*24,expnum*9,expdate*11
	character cs*3
	logical present
	integer*4 videotyp
c
c For DCMENU
	integer Lb(30)
	character*78 text(30)
	common/DMENU/ifonb,csizem,ifont2,nboxlast,nblast		!for DCMENU
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi
	common/hlp/help		!for QDIALOG
c For dialog
	character defolt*30,cans*30		!to hold text & result of QDIALOG
c For IDCPIX/SETACT
	integer iact(0:15),iact1(0:15)
	character cnum*11,cnum1*11,string*60
c For mouse (drag to change vertical position)
	logical mouse_on,mous_set,kbhit
	common/mousval/mouse_on,nbutton
	integer icol(100)
	COMMON/cols/icol,mono
c
	common/dp/discprt
c
	pon()=slock()
c
	jx=320
	jy=240
	nboxlast=10		!in case attempt to delete boxes before any drawn
	nblast=4
	ibk=icol(71)
	icf=2
	itype=0
	line=0
	idraw=-1
	jxlo=1
	jylo=56
	jxhi=639
	jyhi=386
	jy0=300
	eps=1.
	ifont2=0
	csizem=2.	!for dcmenu
	mono=.false.
	showt0=.false.
	ix1=64    !default position for zoom cursors
	iy1=473
	ix2=256
	iy2=473
c Allocate local arrays
	nsamp0=nsamp-nsamp1		!number of points before t=0 (see GETBURST)
	ALLOCATE(ixdisp(nsamp),iydisp(nsamp),iydat(nsamp),jcol(nsamp))
	ALLOCATE(iyavc(nsamp))
	idest=0
	zoomed=.false.
	fint=sngl(1.d-3*dfinter)	!ms per point
	ncrit=ifixr(tcrit/fint)		!# of points for tcrit ms
	ncrit=ncrit-4			!for safety
	ALLOCATE(Yfill(ncrit))
c
c Start graphics
	if(videotyp().ne.18) then
	  idev=0					!screen
	  call VGA
	  call gsetcols(0)
	  call errswi(-1)
	  call brkswi(1)
	  call chaswi(1)
	  call grfmod (1)
	  call harcha
	  call mode(18)
	endif
	  call papenq(xp,yp,ipap)
	  vxlo=0	! for VIEWPORT
	  vxhi=xp
	  vylo=0
	  vyhi=yp
	  xlo=0.		! set
	  xhi=xp		! display
	  ylo=0.
	  yhi=yp		! screen
	  xmin=0.
	  xmax=639.
	  ymin=0.
	  ymax=479.
	  call GRAPHBOUNDARY(xlo,xhi,ylo,yhi)
	  call gSCALE(xmin,xmax,ymin,ymax,is)
	  call spagra(vxlo,vylo,wxmin,wymin)
	  call spagra(vxhi,vyhi,wxmax,wymax)
	call pixpos(0,53,xsc,ysc)
	ymenu=yp-ysc
c
	icw=11		!colour to write in dialog box (light blue)
	icw1=12		!red to write in dialog box for warnings
c	common/dialval1/ictd,icfd,icbd  !colours for dialog box #1 (text,frame,back)
	ictd=icw		!icw is older notation -better use value in common/dialval/
	icfd=7		!frame col for dialog box 1
	icbd=0		!background col for dialog box (0 until fixed)
c
	ibk=3			!background outside data area
	ibk1=1		!background for data area dark blue (or 8=grey?)
	if(mono) then
	   ibk=0	!for monochrome
	   ibk1=1	!for monochrome
	   icw=15
	   icw1=15
	endif
	icfd2=11	!frame colour for dialog box 2
c Scaling for Ioana graphics:
c Scaling: 1401 units are -32768 t0 +32752 in 16 unit increments; dividing
c by 16 gives range as -2048 to +2047 (as for CED502) =-5.0V to +4.9976V
c So 32768/5=6553.6 ADC units/volt
c Use Y coords (0-479) between iyd1, iyd2 for now, to span 65536 ADC units
	iyd1=55
	iyd2=387
	yd1=float(iyd1)
	yd2=float(iyd2)
c
c Now open the CONSAM file, and get calibration factor (calfac is from read14
c for jumps, but from consam here).
	INQUIRE(file=consfil,exist=present,flen=len)
	OPEN(unit=14,file=consfil,status='UNKNOWN',
     &      access='TRANSPARENT')
c don't overwrite CDATE!
c  - from inscan
	read(14,rec=1) iver
	if(iver.eq.1002) then	!already have expdate etc
	    read(14,rec=1)iver,title1,cdate,adctime,idt,ioff,ilen,
     &    inchan,id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,
     &    expdate,defname,tapeID,ipatch,npatch,Emem,temper
	else
	    read(14,rec=1) title,cdate,adctime,idt,ioff,ilen,inchan,
     &      id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,iver
	    title1=title(1:70)
	    if(iver.ne.1001) then
		 call BELL(2)
		 print 311
311	       format(' ERROR IN CONSAM VERSION NUMBER')
	   endif
	endif
c	read(14,rec=1) title,cdate,adctime,idt,ioff,ilen,inchan,
c     &   id1,id2,cs(1:3),calfac,srate,ffilt,ffilt1,calfac1,iver
c
	sfac1=(yd2-yd1)/65536.	!scal fac for ADC to pixel units
	sfac3=sfac1/calfac		!converts pA to pixel units
c Define y-offset for display after data defined
	iyoff=ifixr(yd1 + 0.5*(yd2-yd1))	!zero in centre?
	iyoff1=iyoff+60				!offset for mean
c
c Fill background
	call FILLWIN(0,iyd1,639,iyd2,ibk1)	!fill data area with colour ibk
	call HLINE(0,639,iyd1-1,14)		!line to mark bottom of data area
	call HLINE(0,639,iyd2+1,14)	!line to mark top of data area
	call DEFDIALOG(1,1,2,4,60,icbd)	!define dialog box #1
	call DEFDIALOG(2,1,65,4,14,icbd)	!define dialog box #1
	call OPENDIALOG(1,icfd,.true.)	!draw dialog box #1
	call OPENDIALOG(2,icfd2,.true.)	!draw dialog box #2
c
	ndelt=1	!display every point now
	if(nsamp.gt.2048) then		!suggest point omission
	   ndelt=1 + (nsamp-1)/2048
         call INTCONV(nsamp,cnum)
	   n=NBLANK(cnum)
 	   call DEFOLTi(ndelt,defolt)
	   call QDIALOG(1,
     &	'Sample = '//cnum(1:n)//' points: plot every nth'
     &	,defolt,12,cans)
	   call GETINPi(cans,ndelt)
	endif
c
c Make ymin smaller than any actual data, to leave margin at bottom
c for the Drawbox boxes, and narrower margin at top for messages
c Data are already scaled to pA etc, but better convert back to volts
c (at input of 1401) for display.
c
c Put up boxes
c defs for DCMENU
c	nbox=10	!number of boxes for DCMENU
	icol1=14	!yellow text/border for boxes in DCMENU
	icol2=8	!grey background for boxes
	ifonb=0
	csizb=1.7
	ifont=4
	csize=2.
	nbline=5		!define to prevent error when deleting dcmenu
	nbox=9	!define to prevent error when deleting dcmenu
	call SETLBOX(nbox,Lb,0)
	call NUMSET			!set num lock on ready for response
	do i=1,9
	   Lb(i)=1
	enddo
	Lb(6)=0
	call FILLWIN(0,0,639,iyd1-2,ibk)	!fill DCmenu area with colour ibk
	if(showt0) then
	   text(1)='1. HIDE t=0'
	else
	   text(1)='1. SHOW t=0'
	endif
	text(2)='2. REJECT   '
	text(3)='3. ACCEPT   '
	if(.not.zoomed) then
	   text(4)='4. ZOOM     '
	else
	   text(4)='4. UNZOOM   '
	endif
	text(5)='5. REDO FILL  '
	text(7)='7. OFFSET TRACE'
	text(8)='8. OFFSET MEAN '
	text(9)='9. FINISH   '
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
c
c Initialise TOTCUR
	if(icfil.eq.1) then
	   nmean=0		!number of sweeps accepted for average
	   do i=1,nsamp
c		TOTCUR(i)=0.0
		yval(i,2)=0.0
	   enddo
	endif
c
c calculate x values
	dx=(639.)/float(nsamp)
	i1=0
	do i=1,nsamp,ndelt
	   x=float(i-1)*dx
	   i1=i1+1
	   ixdisp(i1)=int2(ifixr(x))
	   if(i.lt.nsamp0) ndisp0=i1
	   if(i.lt.nsamp0) ndisp0=i1	!# of displayed points before t=0
	enddo
	ndisp=i1	!=number to display on exit
c
	ix0=ixdisp(nsamp0+1)	  !x value for first point in burst (for Vline)
c
c  Now loop through bursts
	do 13 ibst=1,nburst
15	   continue
	   istart=indfst(ibst)
	   iend=indend(ibst)
	   ndat1=iend-istart+1		!number of points for burst
	   ndat=ndat1+nsamp0		!ditto for burst+preburst baseline
	   if(ndat.gt.nsamp) ndat=nsamp
	   nread=ndat+ncrit	!up to end of burst, + next ncrit points
	   if(nread.gt.nsamp) then	!make bigger if nec
		DEALLOCATE(iydat)
		ALLOCATE(iydat(nread))
	   endif
	   ndatend=nsamp-ndat		!# of points to be filled after end of burst
	   irec=ioff + (2*istart-1)    !record # for 1st value to be read
	   irec=irec - 2*nsamp0	   !start read nsamp0 points before start of burst
	   lastrec=irec+2*nread
	   if(irec.lt.ioff+1) irec=ioff+1
	   if(lastrec.gt.len) goto 111		!end of data!
c Need read data up to end of burst, plus next ncrit points, needed for fill
	   read(14,rec=irec) (iydat(i),i=1,nread)
c Fill Yval up to the end of the burst
	   if(ndat.gt.nsamp) then	!no fill needed!
		do i=1,nsamp
		   Yval(i,1)=calfac*float(iydat(i))	!current in pA
		enddo
		goto 27		!skip fill
	   endif
	   do i=1,ndat
		Yval(i,1)=calfac*float(iydat(i))	!current in pA
	   enddo
	   do i=1,ncrit
		Yfill(i)=calfac*float(iydat(i+ndat)) !ncrit points after end of burst
	   enddo
c and fill rest of Yval array by using the tcrit points that follow the burst,
c repeated if necessary to fill the array (there should be no openings for
c tcrit ms after the end of the burst)
	   nc1=0
	   do i=ndat+1,nsamp
		k=i-ndat-nc1
		if(k.gt.ncrit) then
		   k=k-ncrit
		   nc1=nc1+ncrit	!so the ncrit points are recycled
		endif
		Yval(i,1)=Yfill(k)
	   enddo
c Make Ydisp array for display of current (+ average)
27	   continue		!jump here if burst extends beyond screen (no fill)
26	   continue		!return here after changing offset of traces
	   i=0
	   do i1=1,nsamp,ndelt
		i=i+1		!in case ndelt.ne.1
		iydisp(i)=int2(ifixr(Yval(i1,1)*sfac3))+iyoff  !sfac3=pA to pixel
		if(i1.lt.ndat) ndisp1=i	!# of displayed points up to end of burst
	   enddo
c Display mean
	   if(nmean.ge.1) then
		en=float(nmean)
		sfac4=sfac3/en    !takes mean
		i=0
		do i1=1,nsamp,ndelt
		   i=i+1
c		   iyavc(i)=int2(ifixr(sfac4*TOTCUR(i1))) + iyoff1
		   iyavc(i)=int2(ifixr(sfac4*yval(i1,2))) + iyoff1
		enddo
	   endif
	   call INTCONV(ibst,cnum)
	   call CLRDIALOG(2,icbd)
	   call WDIALOG(2,'Cluster #'//charnb(cnum),10)
	   if(nmean.ge.1) then
		call INTCONV(nmean,cnum)
		call WDIALOG(2,'Mean of '//charnb(cnum),12)
	   endif
c Display data for current sweep
22	   continue
c
	   do i=0,15
		iact(i)=i		!so iact ignored and only jcol used
	   enddo
	   do i=1,ndisp0
		jcol(i)=6			!brown before t=0
	   enddo
	   do i=ndisp0+1,ndisp1
		jcol(i)=10			!bright green for burst
	   enddo
	   do i=ndisp1+1,nsamp
		jcol(i)=6			!brown after end of burst
	   enddo
	   call IDCPIX2(ixdisp,iydisp,ndisp,iact,jcol)
	   if(showt0) call VLINE(ix0,iyd1,iyd2,2)   !line to mark start of burst
	   if(nmean.ge.1) then
		call SETACT(iact1,12)		!draw red
		call IDCPIX2(ixdisp,iyavc,ndisp,iact1,jcol)
	   endif
c
60	   continue          !return here for next char until accepted or rejected
	   call NUMSET
	   ch=GETCH(ktype)
	   ival=ichar(ch)
	   ikey=-1
	   if(ktype.eq.3) ikey=ival-48
	   call NUMCLR
c
	   if(ikey.eq.9) goto 111	!finish
c
	   if(ikey.eq.1) then
	      call DCMENU(-1,5,Lb,text,0,0)	!delete box 1 only
		if(showt0) then
		   showt0=.false.
		   text(1)='1. SHOW t=0'
		else
		   showt0=.true.
		   text(1)='1. HIDE t=0'
		endif
	      call DCMENU(-1,5,Lb,text,icol1,icol2)	!draw box 1 only (italic)
		call WDIALOG(1,'Now accept, reject or finish',ictd)
	      goto 60
	   endif
c Accept
	   if(ikey.eq.3) then
c		if(discprt) write(8,211)
c211		format(' Accepted')
	      nmean=nmean+1
c	  Accumulate TOTCUR as above, except ndelt=1 (average all nsamp points)
	      do i=1,nsamp
c		   TOTCUR(i)=TOTCUR(i) + yval(i,1)
		   yval(i,2)=yval(i,2) + yval(i,1)
		enddo
c Can accept/reject while zoomed, but reset here for next trace
	      zoomed=.false.
	      call DCMENU(-4,5,Lb,text,0,0)	!delete box 4 only
	      text(4)='4.  ZOOM    '
	      call DCMENU(-4,5,Lb,text,icol1,icol2)	!draw box 4 only (italic)
		goto 19		!next event
	   endif
c
c Reject
	   if(ikey.eq.2) then
c		if(discprt) write(8,210)
c210		format(' Rejected')
c Can accept/reject while zoomed, but reset here for next trace
		zoomed=.false.
		call DCMENU(-4,5,Lb,text,0,0)	!delete box 4 only
		text(4)='4.  ZOOM    '
		call DCMENU(-4,5,Lb,text,icol1,icol2)	!draw box 4 only (italic)
		goto 19		!next event
	endif
c
c  Zoom/unzoom (for single trace only at present) -put new data into special
c allocated array so original data unchanged
	   if(ikey.eq.4) then
		if(zoomed) then		!unzoom
		   zoomed=.false.
		   call DCMENU(-4,5,Lb,text,0,0)	!delete box 4 only
		   text(4)='4.  ZOOM    '
		   call DCMENU(-4,5,Lb,text,icol1,icol2)	!draw box 4 only (italic)
c	         erase data
		   call FILLWIN(0,iyd1,639,iyd2,ibk1)  !fill data area with col ibk
c              restore original data
		   call IDCPIX2(ixdisp,iydisp,ndisp,iact,jcol)
		   if(showt0) call VLINE(ix0,iyd1,iyd2,2)	!line to mark start of burst
c              restore mean also
		   if(nmean.ge.1) then
			call SETACT(iact1,12)		!draw red
			call IDCPIX2(ixdisp,iyavc,ndisp,iact1,jcol)
		   endif
c
		   call WDIALOG(1,'Now accept, reject or finish',ictd)
		   goto 60
c
		else		!zoom
		   jx=320
		   jy=240
		   zoomed=.true.
		   call DCMENU(-4,5,Lb,text,0,0)	!delete box 4 only
		   text(4)='4.  UNZOOM   '
		   call DCMENU(-4,5,Lb,text,icol1,icol2)	!draw box 4 only (italic)
		   call WDIALOG(1,
     &	    'Mark start of section to be zoomed',ictd)
	         idraw=2
	         line=0
		   islope=0
	         call crossh(jx,jy,jxlo,jylo,jxhi,jyhi,ibk1,icf,
     &	   itype,line,idraw,islope,ax,ay,6,70,6,1.)
		   x1=ax(1)
		   iltype=7       !dash-dot
		   call broken(iltype)
		   call graMOV(x1,yd1)
		   call gralin(x1,yd2)
		   time=x1           !in pixels -640 pixels=nsamp points
		   index1=1 + ifixr((time/640.)*float(ndisp))	!index in iydisp
		   if(index1.lt.1) index1=1
		   x2=ax(2)
		   call graMOV(x2,yd1)
		   call gralin(x2,yd2)
		   time=x2
		   index2=1 + ifixr((time/640.)*float(ndisp))	!index in iydisp
		   if(index2.gt.ndisp) index2=ndisp
		   nzoom=index2-index1+1	!length of chunk for filling
		   ALLOCATE(ixdispz(nzoom),iydispz(nzoom),jcolz(nzoom))
		   dx=(640.)/float(nzoom-1)
		   i1=0
		   do i=1,nzoom
		 	x=float(i-1)*dx
			ixdispz(i)=int2(ifixr(x))
			iydispz(i)=iydisp(i+index1-1)
			jcolz(i)=jcol(i+index1-1)
		   enddo
c	    	   erase data
		   call FILLWIN(0,iyd1,639,iyd2,ibk1)	!fill data area with colour ibk
c              draw zoomed data
		   icol=14				!yellow when zoomed
		   if(mono) icol=15 			!bright white for monochrome
		   call IDCPIX2(ixdispz,iydispz,nzoom,iact,jcolz)
		   DEALLOCATE(ixdispz,iydispz,jcolz)
		   call WDIALOG(1,'Now accept, reject or finish',ictd)
		   goto 60
		endif
	   endif	!end of ikey=4
c
c FIX FILL
	   if(ikey.eq.5) then
c Redraw current burst jump showing ALL data, to choose fill area from
c	    erase data
		call FILLWIN(0,iyd1,639,iyd2,ibk1)  !fill data area with col ibk
c         read all nsamp points
		read(14,rec=irec) (iydat(i),i=1,nsamp)
		do i=1,nsamp
		   Yval(i,1)=calfac*float(iydat(i))	!current in pA
		enddo
c        make new display array
		i=0
		do i1=1,nsamp,ndelt
		   i=i+1		!in case ndelt.ne.1
		   iydisp(i)=int2(ifixr(Yval(i1,1)*sfac3))+iyoff  !sfac3=pA to pixel
		enddo
	      call IDCPIX2(ixdisp,iydisp,ndisp,iact,jcol)
c         now use cursors to define are to be used for fill
		call WDIALOG(1,
     &'Mark START of section to use to fill after end of cluster)',ictd)
		jx=320
		jy=240
	      idraw=2
		islope=0
	      line=0
	      call crossh(jx,jy,jxlo,jylo,jxhi,jyhi,ibk1,icf,itype,
     &	line,idraw,islope,ax,ay,6,70,6,1.)
	      x1=ax(1)
		iltype=2       !dash
		call broken(iltype)
		call graMOV(x1,yd1)
		call gralin(x1,yd2)
		time=x1           !in pixels (640 pixels=nsamp points)
		index1=1+ifixr((time/640.)*float(nsamp))	!index in Yval
		x2=ax(2)
		iltype=7       !dash-dot
		call broken(iltype)
		call graMOV(x2,yd1)
		call gralin(x2,yd2)
		time=x2
		index2=1+ifixr((time/640.)*float(nsamp))	!index in Yval
c Now make new display array with shifted data.  Start with point that
c corresponds with latency-tzero (so still have points tzero ms before jump)
c so if latency=0 than first point on display is yval(1,jc).  Need to redefine
c Yval(), because this is what is averaged, then make new iydisp().
c First copy bit of baseline to be used to fill ends into a temporary
c array, yfill().
		nfill=index2-index1+1	!length of chunk for filling
		DEALLOCATE(yfill)
		ALLOCATE(yfill(nfill))
		do i=1,nfill
		   yfill(i)=Yval(i+index1-1,1)
		enddo
		nc1=0
		do i=ndat+1,nsamp
		   k=i-ndat-nc1
		   if(k.gt.nfill) then
			k=k-nfill
			nc1=nc1+nfill	!so the ncrit points are recycled
		   endif
		   Yval(i,1)=Yfill(k)
		enddo
		DEALLOCATE(yfill)
		ALLOCATE(Yfill(ncrit))	!back to normal
c	    erase data
		call FILLWIN(0,iyd1,639,iyd2,ibk1)  !fill data area with col ibk
c         remake display array
		i=0
		do i1=1,nsamp,ndelt
		   i=i+1		!in case ndelt.ne.1
		   iydisp(i)=int2(ifixr(Yval(i1,1)*sfac3))+iyoff
		enddo
		call broken(0)
		goto 22	!back to normal display
	   endif	!end of ikey=5
c Fix offset
c With mouse method, want first point in iydisp to appear at value of iy
c returned by mouse click -present position is at iydisp(1) -want to move
c it to iy, ie change it by iy-iydisp1
	   if(ikey.eq.7.or.ikey.eq.8) then
		if(mouse_on) then
		   call SHOW_MOUSE()
		   call WDIALOG(1,
     &	'Click mouse at vertical position wanted for trace',ictd)
25		   call get_mouse(ibutton,ix,iy)
		   if(ibutton.gt.0) then
			if(ikey.eq.7) then
			   iyoff=iyoff+iy-iydisp(1)
			else
			   iyoff1=iyoff1+iy-iyavc(1)
			endif
		   else
			goto 25
		   endif
		   call HIDE_MOUSE()
		else
	 	   call DEFOLTi(iyoff,defolt)
		   call QDIALOG(1,
     &	'Vertical offset for display of current trace'
     &	,defolt,ictd,cans)
	         call GETINPi(cans,iyoff)
	 	   call DEFOLTi(iyoff1,defolt)
		   call QDIALOG(1,
     &	'Vertical offset for display of mean trace'
     &	,defolt,ictd,cans)
		   call GETINPi(cans,iyoff1)
		endif
		call WDIALOG(1,'Now accept, reject or finish',ictd)
c=		goto 60
		call FILLWIN(0,iyd1,639,iyd2,ibk1)	!fill data area with colour ibk
		goto 26	!redraw with new offset
	    endif
c
	goto 60		!get another character if none of above done
c
19	continue
	call FILLWIN(0,iyd1,639,iyd2,ibk1)	!fill data area with colour ibk
13	continue	!end of loop for each event
c
c Finish:
111	continue
	CLOSE(unit=14)
	call VIDEOMOD(3)		!utility lib- this makes graph go for good!
c Now have TOTAL of NMEAN events, in yval(i,2) (previously TOTCUR)
	if(nmean.eq.0) then
	   call BELL(1)
	   print 252
252	   format(' Zero events averaged')
	   idest=4
	   goto 999		!deallocate local arrays and return with idest=4
	endif
	en=float(nmean)
	do i=1,nsamp
c	   Yval(i,1)=TOTCUR(i)/en
	   Yval(i,1)=yval(i,2)/en
	enddo
c
      if(pon()) write(7,30) nmean
      if(discprt) write(8,30) nmean
30	format(
     & 1x,i5,' clusters from CONSAM.DAT averaged',/)
	iopt=1
c
c If another CONSAM to be done then go back now to find it -in this case
c need to save nmean,totcur=yval(i,2) to add more values to them
	if(ncfil.gt.1.and.icfil.lt.ncfil) then
	   goto 999		!get next consam file
	endif
c
	print 31,nmean,nsamp,iopt
31	format(
     & ' Average of ',i5,' clusters, with ',i7, ' points:',/,
     & ' (1) Fit and/or plot it',/,
     & ' (2) Abandon this one and calculate another average',/,
     & ' (3) Exit now (average trace not kept)',/,
     & '  Option number [',i2,'] = ')
	call INPUTi(iopt)
	idest=iopt
c
	if(iopt.eq.2) then
	   idest=6
	   goto 999		!back to REAVER1 with idest=6
	endif
c
999	continue
c Dealloc local arrays before leaving
	DEALLOCATE(ixdisp,iydisp,iyavc,jcol)
	DEALLOCATE(yfill)
	if(allocated(jcolz)) DEALLOCATE(jcolz)
	if(allocated(iydat)) DEALLOCATE(iydat)
	if(allocated(ixdispz)) DEALLOCATE(ixdispz)
	if(allocated(iydispz)) DEALLOCATE(iydispz)
c
	RETURN
	end

