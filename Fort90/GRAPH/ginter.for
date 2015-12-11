c======================================================================
	subroutine gplot3d(xmid1,ymid1,z1,nbinx,nbiny,
     &	xtitle,ytitle,ztitle,title1,idrawt,idrawx,idrawy,idrawz)
c======================================================================
c Scaling problems
c (1) the data provided seem to be plotted over the entire range
c	of the axes, regardless of the numbers on the axes. Thus if
c	data cover X range 0-1, we get the same graph even if the
c	axes are changed so they are labelled -50 to +50!  Thus to
c	get correct numbering must (a) not use FIXAX, and (b) can
c	alter xmin,xmax etc, only by changing data (c) xmin, xmax etc must
c	be set to actual observed values of x, not any value.
c (2) The array z(i,j) in call to PERPRJ must be declared as having dimensions
c	that are the current values of nbinx, nbiny (since this subroutine
c	has not got separate arguments to specify its declared dimensions)
c	so must keep original arrays as z1, xmid1,ymid1 and then allocate
c	z, xmid, ymid with the right dimensions, and copy required elements
c	to them
c
c (3)	tic control : length only by CHASIZ
c			: number by PRJSCA

	allocatable::gridz,az,ax,ay,w,IFILLZ,IFILLG
	allocatable::gax,gay
 	INTEGER*4 IFILLZ(:,:),IFILLG(:,:)
	integer Lb(30),icol(100)		!for DCMENU
c=====================

	real*4 z1(nbinx,nbiny),gax(:),gay(:)
	real*4 xmid1(nbinx),ymid1(nbiny)
	allocatable::z,xmid,ymid
	real*4 z(:,:),xmid(:),ymid(:)

c========================
	real*4 gridz(:,:),az(:),ax(:),ay(:),w(:)
	character*75 xtitle,ytitle,ztitle,str
	character*75 title1
	character*78 text(30)
	character defolt*30,cans*30,kpchar*5,ans,UC,getch		!to hold text & result of QDIALOG
	character*12 cxmin,cxmax,cymin,cymax,czmin,czmax,cnumx,cnumy
	character*12 ci,cj,ck,cx,cy,cz
	character*11 cnum1,cnum2,cnum3
	logical all,cross,after
	logical mono,plot,fill,inter,meta,present,landplot,axes,help
	character*33 titles,pstring,filnam
	character*33 metafil,printfil,oldfile,printer,strings(10)
      INTEGER ISTYLE(6),ISUP(6)
	common/DMENU/ifonb,csizeb,ifont2,nboxlast,nblast		!for DCMENU
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi
	common/hlp/help		!for QDIALOG
	COMMON/cols/icol,mono
	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls
	logical usedef
	common/def/usedef		!for qdialog
c
c Set things that re not to be changed after rescale
	idev=0
	ifnt=4
	ifnl=4
	alfa=0.
	beta=0.
	gama=0.
	delta=90.
	ijust=0	!title centred?
	ijusx=-1
	ijusy=1
	ijusz=0
	ang=5.
	deltax=10.
	deltay=10.
	call VGA
	call gsetcols(0)
c 	call errswi(-1)
c 	call errmax(-1)
 	call brkswi(1)
 	call chaswi(1)
 	call grfmod (1)
 	call harcha
 	call mode(18)
      CALL PICCLE
	vxlo=0.	! for VIEWPORT
	vxhi=1080.
	vylo=0.
	vyhi=810.
	vxlo1=vxlo
	vxhi1=vxhi
	vylo1=vylo
	vyhi1=vyhi
	icf=1
	icf2=1
	ict=11
	icb2=0
	ifont=2
	ifont2=0
	nboxlast=10		!in case attempt to delete boxes before any drawn
	nblast=4
	icol1=14	!yellow text/border for boxes in DRAWBOX
	icol2=8	!grey background for boxes
	icol0=7	!white text/border for altered boxes
	icb2=0	!background colour for dialog box 1,2
	icf=7		!frame colour for dialog box 1
	icf2=12	!frame colour for dialog box 2
	ict=11	!text colour for dialog box 1
	call DEFDIALOG(1,1,2,4,60,icb2)	!define dialog box #1
	call DEFDIALOG(2,1,65,4,14,icb2)	!define dialog box #2
c
c Now save inputs and define z(i,j)
c
	nby1=1
	nby2=nbiny
	nbx1=1
	nbx2=nbinx
	nbxsav=nbinx      !save so can be restored before RETURN
	nbysav=nbiny
c
c===============trick if nbinx changed by debugger/rescale
c===========YES -SEEMS TO WORK! IF NBINX CHANGED TO RESCALE THEN
C===========MUST REALLOCATE Z SO IT HAS THE CORRECT SIZE -MUST BE THAT
C========== CALL TO PERPRJ MUST HAVE NBINX=DECLARED DIMENSION OF Z
C===ASK BRADLEY TO ADD EXTRA PARAMS -THE DECLARED DIMENSIONS OF Z!!
	allocate(z(nbinx,nbiny),xmid(nbinx),ymid(nbiny))
	xmin=1.e36
	xmax=-1.e36
	ymin=1.e36
	ymax=-1.e36
	zmin=1.e36
	zmax=-1.e36
	do i=1,nbinx
	   xmid(i)=xmid1(i)		!copy input
	   if(xmid(i).gt.xmax) xmax=xmid(i)
	   if(xmid(i).lt.xmin) xmin=xmid(i)
	   do j=1,nbiny
		z(i,j)=z1(i,j)		!copy input
		if(z(i,j).gt.zmax) zmax=z(i,j)
		if(z(i,j).lt.zmin) zmin=z(i,j)
	   enddo
	enddo
	do j=1,nbiny
	   ymid(j)=ymid1(j)		!copy input
	   if(ymid(j).gt.ymax) ymax=ymid(j)
	   if(ymid(j).lt.ymin) ymin=ymid(j)
	enddo

	numx=nbinx
	numy=nbiny

c====================================================================
c Return here to redraw with new nbinx, nbiny after rescaling x,y
306	continue
	zmin1=zmin
	zmax1=zmax
c Must NOT use FIXAX on X or Y axes (see notes at top)
	call FIXAX(zmin1,zmax1,zmin,zmax,ztic,0)  !new zmin etc
1000  continue

	cross=.false.
	all=.false.
	call broken(0)
	RATIO=0.35
	RAT=1.0
      RADIUS=500.0
      THETA=30.0
      PHI=30.0
	IFRAM=2
	XWMIN=100.
	XWMAX=1050.
	YWMIN=50.
	YWMAX=620.
	FILL=.FALSE.
	INTER=.FALSE.
	AXES=.TRUE.
	NUMAXI=0
	NWIDTH=5
	NPOW=1
	NPLACE=1
	nxstep=1
	nystep=1
c     Fill background colour
	ncol=9
	ncont=9
1002	if(.not.mono) then
	    call FILLWIN(0,55,639,387,icol(71))		!graph area
	    call FILLWIN(0,388,639,479,1)			!dialog area
	endif
	if(.not.plot) then
	   call HLINE(0,639,54,15)		!line to mark bottom of data area
	   call HLINE(0,639,389,15)	!line to mark top of data area
	endif

c
c	PLOT GRAPH
c
      CALL SURF		!Surf banner
	CALL PAPENQ(XPAP,YPAP,IPAPTY)
1003  CALL SETVP2(vxlo,vxhi,vylo,vyhi,0.0,XPAP,0.0,YPAP)	!screen->grey
	ymenu=vyhi/480.*53.
	xp=xpap
	yp=ypap
	xlo=0.2*vxhi
	xhi=0.8*vxhi
	ylo=0.2*vyhi
	yhi=0.72*vyhi
	xlo1=xlo
	xhi1=xhi
	ylo1=ylo
	yhi1=yhi

	csize=vyhi/80. !copy for common/dmenu/
	csizeb=csize
	sizet=csize*2.5
	sizel=csize*1.7

	call OPENDIALOG(1,icf,.true.)		!draw dialog box #1
	call OPENDIALOG(2,icf2,.true.)		!draw dialog box #2
	call WDIALOG(2,'F1=HELP',ict)
	call WDIALOG(2,'F2=HELP INDEX',ict)
c=====================================================================

100   continue
	idim=nbinx*nbiny
	if(allocated(az)) then
	   deallocate(az,ax,ay)
	endif
	allocate(az(idim),ax(idim),ay(idim))
	if(allocated(ifillz)) then
	   deallocate(ifillz)
	endif
	ALLOCATE(ifillZ(nBINx,nBINy))
	k=1
	do i=1,6
	   istyle(i)=3
	   isup(i)=0
	enddo
	k=1
	i1=0
	do i=nbx1,nbx2
	   i1=i1+1
	   j1=0
	   do j=nby1,nby2
		j1=j1+1
		ax(k)=xmid(i1)
		ay(k)=ymid(j1)
		az(k)=z(i1,j1)
		k=k+1
	   enddo
	enddo

C
C  INITIALISE VARIABLES
C
      xlow=xmid(1)
      xhigh=xmid(nbinx)
	dx=xmid(2)-xmid(1)
      ylow=ymid(1)
      yhigh=ymid(nbiny)
	dy=ymid(2)-ymid(1)
c
	np=nbinx*nbiny
C
C  MINIMUM WORKSPACE SIZES ARE :
C
      nw = max(2*numx*numy+4*np,numx*numy+20*max(numx,numy))
	if(allocated(w)) DEALLOCATE(w)
	allocate(w(nw))
	if(allocated(gridz)) then
	   DEALLOCATE(gridz)
	endif
	allocate(gridz(numx,numy))
      CALL RANGRD(NP,AX,AY,AZ,NUMX,XLOW,XHIGH,NUMY,YLOW,YHIGH,
     +   	GRIDZ,NW,W)
	gzmin=1.e36
	gzmax=0.
	zgmin=1.e36
	zgmax=0.
	k=1
	do i=1,numx
	   do j=1,numy
		if(gridz(i,j).gt.gzmax) gzmax=gridz(i,j)
		if(gridz(i,j).lt.gzmin) gzmin=gridz(i,j)
	   enddo
	enddo
	do i=1,nbinx
	   do j=1,nbiny
		if(z(i,j).gt.zgmax) zgmax=z(i,j)
		if(z(i,j).lt.zgmin) zgmin=z(i,j)
	   enddo
	enddo
	bzw=(zgmax-zgmin)/float(ncont)
	bw=(gzmax-gzmin)/float(ncont)
 	if(allocated(gax)) then
 	   deALLOCATE(gax)
 	endif
 	allocate(gax(numx*numy))
 	if(allocated(gay)) then
 	   deALLOCATE(gay)
 	endif
 	allocate(gay(numx*numy))
 	dix=(ax(nbinx*nbiny)-ax(1))/numx
 	diy=(ay(nbiny)-ay(1))/numy
 	k=1
 	do i=1,numx
 	   do j=1,numy
 		gax(k)=ax(1)+i*dix
 		gay(k)=ay(1)+j*diy
 		k=k+1
 	   enddo
 	enddo
      if(allocated(ifillg)) then
	   DEALLOCATE(ifillg)
	endif
      ALLOCATE(ifillg(numx-1,numy-1))
      DO I=1,NUMX-1
	   DO J=1,NUMY-1
		do k=1,ncont
		   blo=gzmin+float(k-1)*bw
		   bhi=gzmin+float(k)*bw
		   zv=gridz(i,j)
		   if(zv.ge.blo.and.zv.le.bhi) ifillg(i,j)=k+1	!avoid 1
		   if(kcol.eq.2) ifillg(i,j)=ncol
		enddo
	   enddo
      enddo
	call setfnt(ifnl)
      CALL CHASIZ(csize,csize)
      if(plot) then
      	CALL PRJLIN(100,100,100)
	else if(meta.and.cross) then
      	CALL PRJLIN(1000,100,100)
	else
      	CALL PRJLIN(1000,100,100)
	endif
	if((cross.and..not.meta.and..not.plot.and..not.after)) goto 444
	if(all) then
		CALL PRJBND(XLO1,XHI1,YLO1,YHI1)
	else
		CALL PRJBND(XLO,XHI,YLO,YHI)
	endif
	if(cross.and.meta.or.cross.and.plot.or.cross.and.after)
     &	call linvis(0)
      CALL PRJFRA(IFRAM)
	call prjgrd(nxstep,nystep)
      CALL WINDO2(XWMIN,XWMAX,YWMIN,YWMAX)
      CALL PRJSCA(xtic,ytic,ztic)
      CALL PRJAXI(ISTYLE,ISUP)
      CALL HEIRAT(RATIO)
      CALL XYRAT(RAT)
	call PRJLEV(zmin,zmax)
	call perset(xmin,xmax,ymin,ymax,zmin,zmax,radius,theta,phi)
	IF(INTER) THEN
		if(fill) then
    	         CALL PERFIL(NUMX,XMIN,XMAX ,NUMY,YMIN,YMAX ,GRIDZ,
     +             RADIUS,THETA,PHI,IFILLG,NW,W)	!grid displayed
		else
      	   CALL PERPRJ(NUMX,XMIN,XMAX ,NUMY,YMIN,YMAX ,GRIDZ,
     +              RADIUS,THETA,PHI,NW,W)	!grid displayed
		endif
	ELSE
		if(fill) then
   			CALL PERFIL(NBINX,XMIN,XMAX,NBINY,YMIN,YMAX,Z,
     &              RADIUS,THETA,PHI,IFILLG,NW,W)
		else
   			CALL PERPRJ(NBINX,XMIN,XMAX,NBINY,YMIN,YMAX,Z,
     &              RADIUS,THETA,PHI,NW,W)
		endif
	ENDIF
C  	ADD 3D AXES
      CALL WINDOW(2)
	if(cross) goto 444
	if(axes) then
	   call ANNDIG(1,NWIDTH,NPLACE,NPOWER)
	   call ANNDIG(2,NWIDTH,NPLACE,NPOWER)
	   call ANNDIG(3,NWIDTH,NPLACE,NPOWER)
	   call ANNSTR(1,'            ')
	   call ANNSTR(2,'            ')
     	   call ANNSTR(3,'            ')
         CALL PERAXE(NW,W)		!axes displayed
	   if(iperf.ne.-100) then
	      call perspa((xmin+xmax)/2,ymin-4*dy,zmin,xspx,yspx)
	      call perspa(xmin-4*dx,(ymin+ymax)/2,zmin,xspy,yspy)
	      call perspa(xmax+3*dx,ymin-3*dy,(zmin+zmax)/2,xspz,yspz)
	   endif
	   call lincols(icol(23),idev)
	   call setfnt(ifnl)
	   if(idrawx.eq.1) call JUSTIFYSTRING(xspx,yspx,xtitle,beta,
     &      sizel,ijusx)
	   if(idrawy.eq.1) call JUSTIFYSTRING(xspy,yspy,ytitle,gama,
     &      sizel,ijusy)
	   if(idrawz.eq.1) call JUSTIFYSTRING(xspz,yspz,ztitle,delta,
     &	sizel,ijusz)
	endif
c	ADD TITLE
	call lincols(ICOL(25),idev)
	call setfnt(ifnt)
	if(iperf.ne.-100) call perspa(xmax,ymax,1.2*zmax,xst,yst)
	if(idrawt.eq.1) call JUSTIFYSTRING(xst,yst,title1,alfa,
     &	sizet,ijust)
444	continue
	call linvis(1)
	if(cross) then
		if(all) then
			call CRSBND(0.55*vxhi,0.9*vxhi,ylo,yhi)
			call CRSSEC(x_miny,y_miny,x_maxy,y_maxy,nw,w)
			call CRSBND(0.1*vxhi,0.45*vxhi,ylo,yhi)
		else
			call CRSBND(xlo,xhi,ylo,yhi)
		endif
		call CRSSEC(x_min,y_min,x_max,y_max,nw,w)
	endif
888	continue
	if(meta) goto 5555
	if(plot) goto 1812		!return with plot=true?
	after=.false.
c
c  	DRAW MENU
c

150	continue
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,5,Lb,text,0,0)		!delete all
151	continue
c==============what does ONE VIEW mean??
	if(cross) then
	   if(all) then
	   	text(1)=' 1: ONE VIEW'
	   else
	   	text(1)=' 1: ALL '
	   endif
	else
		text(1) =' 1: RESCALE     '
	endif
	text(2)='  2: INTERPOLATE '
	text(3) =' 3: GRAPH SHAPE'
	text(4) =' 4: GRAPH AXES '
	text(5) =' 5: POSH OPTIONS'
	text(6) =' 6: GIVE TITLE '
	text(7) =' 7: PLOT NOW   '
	text(8) =' 8: QUEUE PLOT '
	text(9) =' 9: END DISPLAY'
	text(10)='10: REDRAW    '
	text(11)=' +: X AXIS LABEL'
	text(12)=' -: Y AXIS LABEL'
	text(13)=' x: Z AXIS LABEL'
	if(fill) then
	   text(14)='/: NO FILL'
	else
	   text(14)='/: FILL  '
	endif
	if(cross) then
	   text(15)=' .: 3D PROJECTION'
	else
	   text(15)=' .: CROSS SECTION '
	endif

	call SETLBOX(nbox,Lb,1)
	if(cross) then
	   do i=2,6
		Lb(i)=0
	   enddo
	   do i=11,14
		Lb(i)=0
	   enddo
	endif
	Lb(8)=0
	call broken(0)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
152	call CKEY(ch,ikey)
	if(ikey.lt.-2.or.ikey.gt.15) goto 151
	if(ikey.eq.-1) then	!F1 key
	   call VHELP(15)
	   goto 152		!another menu choice
	else if(ikey.eq.-2) then	!F2=help index
	   call VHELP(-3)
	   goto 152		!another menu choice
	endif

	goto(1,2,3,4,5,6,7,8,9,10,11,11,11,14,15)ikey


c========================================================================
C				RESCALE
c unfortunately this does not work properly.Gino does not have option
c to rescale on y and x.
c Y works, x not!!!!
c========================================================================
1 	CONTINUE
	if(cross) goto 15
	cross=.false.
c	if(inter) goto 1111
	RAT=1.0
	all=.false.
	call DCMENU(-1,5,Lb,text,0,0)	!delete box 1 only
	Lb(1)=-1
	call DCMENU(-1,5,Lb,text,icol0,icol2)	!draw box 1 only (italic)
211	call DEFOLT2r(xmin,xmax,defolt)
	call QDIALOG(1,'Xmin, Xmax',defolt,ict,cans)
	call GETINP2r(cans,xmina,xmaxa)
	if(xmina.eq.xmin.and.xmaxa.eq.xmax) then
	   goto 212
	endif
c==========================DC revisions to xmax etc
c NB xmin, xmax, ymin, ymax must correspond exactly to one of the
c existing x, y values so go through and look for the closest value
c and reset them to that, then redefine nbinx, nbiny to the appropriate
c value, reallocate z() to exactly that size and redrew completely
c For xmin take nearest  value below that requested (if there is one)
c For xmax take nearest  value above that requested (if there is one)
c Can we assume that xmid, ymid will always be in ascending order? Safer
c not to assume this? BUT NEXT BIT DOES (SKIP NEXT BIT IF XMIN NOT CHANGED)
c Must do this on z1,nbxsav, so can increase xmax after it has been reduced
	if(usedef) then	!defaults used (new common in qdialog)
	   nbx1=1
	   nbx2=nbinx
	   goto 212
	endif
	xmin=xmina
	xmax=xmaxa
	do i=1,nbxsav
	   if(xmid1(i).gt.xmin) then
		if(i.gt.1) then
		   xmin=xmid1(i-1)
		   nbx1=i-1
		else
		   xmin=xmid1(1)
		   nbx1=1
		endif
		goto 12
	   endif
	enddo
c get here if all xmid(i) are bigger then specified xmin, so use smallest
	xmin=xmid1(1)
	nbx1=1
12	continue
	do i=1,nbxsav
	   if(xmid1(i).gt.xmax) then
		if(i.le.nbxsav) then
		   xmax=xmid1(i)
		   nbx2=i
		else
		   xmax=xmid1(nbxsav)
		   nbx2=nbxsav
		endif
		goto 13
	   endif
	enddo
c get here if all xmid(i) are less then specified xmax, so use largest
	xmax=xmid1(nbxsav)
	nbx2=nbxsav
13	continue
	nbinx=nbx2-nbx1+1
	call REALTOCH(xmin,cnum1,11)
	call REALTOCH(xmax,cnum2,11)
	call INTCONV(nbinx,cnum3)
	ans='Y'
	call DEFOLTa(ans,defolt)
	call QDIALOG(1,'Nearest Xmin = '//charnb(cnum1)//', Xmax = '//
     &  charnb(cnum2)//', n = '//charnb(cnum3)//': OK',
     &   defolt,12,cans)
 	call GETINPa(cans,ans)
	if(ans.eq.'N') goto 211
c
c Now repeat above for ymin, ymax (if they are changed)
212	call DEFOLT2r(ymin,ymax,defolt)
	call QDIALOG(1,'Ymin, Ymax',defolt,ict,cans)
	call GETINP2r(cans,ymina,ymaxa)
	if(ymina.eq.ymin.and.ymaxa.eq.ymax) then
	   goto 1111
	endif
	if(usedef) then	!defaults used (new common in qdialog)
	   nby1=1
	   nby2=nbiny
	   goto 1111
	endif
	ymin=ymina
	ymax=ymaxa
	do j=1,nbysav
	   if(ymid1(j).gt.ymin) then
		if(j.gt.1) then
		   ymin=ymid1(j-1)
		   nby1=j-1
		else
		   ymin=ymid1(1)
		   nby1=1
		endif
		goto 121
	   endif
	enddo
c get here if all ymid(i) are bigger then specified ymin, so use smallest
	ymin=ymid1(1)
	nby1=1
121	continue
	do j=1,nbysav
	   if(ymid1(j).gt.ymax) then
		if(j.le.nbysav) then
		   ymax=ymid1(j)
		   nby2=j
		else
		   ymax=ymid1(nbysav)
		   nby2=nbysav
		endif
		goto 131
	   endif
	enddo
c get here if all xmid(i) are less then specified xmax, so use largest
	ymax=ymid1(nbysav)
	nby2=nbysav
131	continue
	nbiny=nby2-nby1+1
	call REALTOCH(ymin,cnum1,11)
	call REALTOCH(ymax,cnum2,11)
	call INTCONV(nbiny,cnum3)
	call WDIALOG(1,' Ymin = '//charnb(cnum1)//', Ymax = '//
     &  charnb(cnum2)//', n = '//charnb(cnum3),ict)
	ans='Y'
	call DEFOLTa(ans,defolt)
	call QDIALOG(1,'Nearest Ymin = '//charnb(cnum1)//', Ymax = '//
     &  charnb(cnum2)//', n = '//charnb(cnum3)//': OK',
     &   defolt,12,cans)
 	call GETINPa(cans,ans)
	if(ans.eq.'N') goto 212
	nbiny=nby2-nby1+1
c
1111	call DEFOLT2r(zmin,zmax,defolt)
	call QDIALOG(1,'Zmin, Zmax',defolt,ict,cans)
	call GETINP2r(cans,zmin,zmax)
c Reallocate z(i,j) to new size for new xmin,....
	DEALLOCATE(z,xmid,ymid)
	ALLOCATE(z(nbinx,nbiny),xmid(nbinx),ymid(nbiny))	!new size
	j1=0
	do j=nby1,nby2
	   j1=j1+1
	   ymid(j1)=ymid1(j)	!copy correct y values to ymid()
	enddo
	i1=0
	do i=nbx1,nbx2
	   i1=i1+1
	   xmid(i1)=xmid1(i)	!copy correct x values to xmid()
	   j1=0
	   do j=nby1,nby2
		j1=j1+1
		z(i1,j1)=z1(i,j)	!copy correct x values to z()
	   enddo
	enddo
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c========================================================================
c                           INTERPOLATE
c========================================================================
2	continue
	inter=.false.
	call DEFOLT2i(numx,numy,defolt)
	call QDIALOG(1,'Numx,Numy',defolt,ict,cans)
	call GETINP2i(cans,numx,numy)
	if(numx.eq.nbinx.and.numy.eq.nbiny) goto 222
	if(numx.lt.nbinx.or.numy.lt.nbiny) goto 2
	INTER=.TRUE.
222	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100


c========================================================================
c                           GRAPH SHAPE
c========================================================================
3	continue
250	continue
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,5,Lb,text,0,0)		!delete all
251	continue
	text(1) =' 1: ROTATE    '
	text(2)='  2: ELEVATION'
	text(3) =' 3: X/Y RATIO'
	text(4) =' 4: HEIGHT/BASE'
	text(7) =' 7: WINDOW'
	text(8) =' 8: GRID'
	text(9) =' 9: FRAME'
	text(10)='10: REDRAW    '
	text(11)=' +: CROSS SECTION'
	text(12)=' -: SETTINGS'
	DO I=11,15
		Lb(I)=0
	ENDDO
	Lb(5)=0
	Lb(6)=0
	call broken(0)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
252	call CKEY(ch,ikey)
	if(ikey.lt.-2.or.ikey.gt.12) goto 251
	if(ikey.eq.-1) then	!F1 key
	   call VHELP(16)
	   goto 252		!another menu choice
	else if(ikey.eq.-2) then	!F2=help index
	   call VHELP(-3)
	   goto 252		!another menu choice
	endif

	goto(101,102,103,104,105,106,107,108,109,110,111,112)ikey
c========================================================================
c                           ROTATE
c========================================================================
101	continue
	call DEFOLTr(phi,defolt)
	call QDIALOG(1,'Phi',defolt,ict,cans)
	call GETINPr(cans,phi)
	iperf=100
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c	goto 152

c========================================================================
c                           ELEVATION
c========================================================================
102	continue

	call DEFOLTr(theta,defolt)
	call QDIALOG(1,'Theta',defolt,ict,cans)
	call GETINPr(cans,theta)
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c	goto 152

c========================================================================
C		X/W RATIO
c========================================================================
103 	CONTINUE
	call DEFOLTr(rat,defolt)
	call QDIALOG(1,'X/Y Ratio',defolt,ict,cans)
	call GETINPr(cans,rat)
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100

c========================================================================
C		RATIO
c========================================================================
104 	CONTINUE
	call DEFOLTr(ratio,defolt)
	call QDIALOG(1,'HEIGHT/BASE Ratio',defolt,ict,cans)
	call GETINPr(cans,ratio)
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c========================================================================
c                           X,Y,Z
c========================================================================
105	continue
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c========================================================================
C		VIEWPORT
c========================================================================
106 	CONTINUE
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	GOTO 100
c========================================================================
C		WINDOW
c========================================================================
107	CONTINUE
	call DEFOLT2r(xwmin,xwmax,defolt)
	call QDIALOG(1,'Xwmin, Xwmax',defolt,ict,cans)
	call GETINP2r(cans,xwmin,xwmax)
	call DEFOLT2r(ywmin,ywmax,defolt)
	call QDIALOG(1,'Ywmin, Ywmax',defolt,ict,cans)
	call GETINP2r(cans,ywmin,ywmax)
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	GOTO 100
c========================================================================
C				  GRID
c========================================================================
108	CONTINUE
	call DEFOLT2i(nxstep,nystep,defolt)
	call QDIALOG(1,'nxstep, nystep',defolt,ict,cans)
	call GETINP2i(cans,nxstep,nystep)
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	GOTO 100

c========================================================================
c                          FRAME
c========================================================================
109	continue
	call DEFOLTI(IFRAM,defolt)
	call QDIALOG(1,'Frame',defolt,ict,cans)
	call GETINPi(cans,ifram)
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c	goto 152

c========================================================================
C				  REDRAW
c========================================================================
110	CONTINUE
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100

c========================================================================
C				  CROSS SECTION
c========================================================================
15	continue
111	continue
	if(cross) then
	   if(ikey.eq.15) then
		cross=.false.
		goto 987
	   else
c=		if(all) then
c=		   all=.false.
c=		else
c=		   all=.true.
c=		endif
		all=.not.all
	   endif
	else
	   all=.false.
	   cross=.true.
	endif
	x_min=xmin
	x_max=xmax
c==	y_min=ymid(nbiny/2)
c==	y_max=y_min
	y_min=ymin
	y_max=ymax
	if(all) then			!!!what is this for!!
	   x_miny=xmid(nbinx/2)
	   x_maxy=x_miny
	   y_miny=ymin
	   y_maxy=ymax
	   goto 987
	endif
88	call DEFOLT2r(x_min,y_min,defolt)
	call QDIALOG(1,'Draw from x1, y1',defolt,ict,cans)
	call GETINP2r(cans,x_min,y_min)
	call DEFOLT2r(x_max,y_max,defolt)
	call QDIALOG(1,' -to x2, y2',defolt,ict,cans)
	call GETINP2r(cans,x_max,y_max)
987	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c========================================================================
C				  SETTINGS
c========================================================================
112	continue
	call DEFOLT2r(xmin,xmax,defolt)
	call QDIALOG(1,'xmin, xmax',defolt,ict,cans)
	call GETINP2r(cans,xmin,xmax)
	call DEFOLT2r(xmin,xmax,defolt)
	call QDIALOG(1,'ymin,ymax ',defolt,ict,cans)
	call GETINP2r(cans,ymin,ymax)
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c========================================================================
c                           GRAPH AXES
c========================================================================
4	continue
450	continue
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,5,Lb,text,0,0)		!delete all
451	continue

	if(axes) then
	   text(1)='1: NO AXES'
	else
	   text(1)='1: AXES  '
	endif
	text(2)='2: DRAWING STYLE'
	text(3)='3: DATA SUPRESION'
	text(4)='4: ANNOTATION'
	text(5)='5: TICS NUMBER'
	text(6)='6: TICS SIZE'
	text(10)='10: REDRAW'
	DO I=7,9
		Lb(i)=0
	ENDDO
	DO I=11,15
		Lb(i)=0
	ENDDO
	call broken(0)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
452	call CKEY(ch,lkey)
	if(lkey.lt.-2.or.ikey.gt.10) goto 450
	if(lkey.eq.-1) then	!F1 key
	   call VHELP(17)
	   goto 452		!another menu choice
	else if(lkey.eq.-2) then	!F2=help index
	   call VHELP(-3)
	   goto 452		!another menu choice
	endif
	if(lkey.eq.1) then
		IF(axes) THEN
	   		axes=.false.
		ELSE
	   		axes=.true.
		ENDIF
	endif
	if(lkey.eq.2) then
		call DEFOLT2i(istyle(1),istyle(2),defolt)
		call QDIALOG(1,'Style Axis x1, y1',defolt,ict,cans)
		call Wdialog(1,'0:nothing ;1:axis; 2:grid; 3:axis+grid',ict)
		call GETINP2i(cans,istyle(1),istyle(2))
		call DEFOLT2i(istyle(3),istyle(4),defolt)
		call QDIALOG(1,'Style x2, y2',defolt,ict,cans)
		call GETINP2i(cans,istyle(3),istyle(4))
		call DEFOLT2i(istyle(5),istyle(6),defolt)
		call QDIALOG(1,'Style Axis x3, y3',defolt,ict,cans)
		call GETINP2i(cans,istyle(5),istyle(6))
	endif
	if(lkey.eq.3) then
		call DEFOLT2i(isup(1),isup(2),defolt)
		call QDIALOG(1,'supresion Axis x1, y1',defolt,ict,cans)
		call Wdialog(1,'0:all ;1:no lowest; 2:no highest; 3:none',ict)
		call GETINP2i(cans,isup(1),isup(2))
		call DEFOLT2i(isup(3),isup(4),defolt)
		call QDIALOG(1,'supresion  x2, y2',defolt,ict,cans)
		call GETINP2i(cans,isup(3),isup(4))
		call DEFOLT2i(isup(5),isup(6),defolt)
		call QDIALOG(1,'supresion  Axis x3, y3',defolt,ict,cans)
		call GETINP2i(cans,isup(5),isup(6))
	endif
	if(lkey.eq.4) then
		call DEFOLTi(numaxi,defolt)
		call QDIALOG(1,'annotation for axis',defolt,ict,cans)
		call GETINPi(cans,numaxi)
		call DEFOLTi(nwidth,defolt)
		call QDIALOG(1,'field width of the number',defolt,ict,cans)
		call GETINPi(cans,nwidth)
		call DEFOLTi(nplace,defolt)
		call QDIALOG(1,'decimal places',defolt,ict,cans)
		call GETINPi(cans,nplace)
		call DEFOLTi(npow,defolt)
		call QDIALOG(1,'power',defolt,ict,cans)
		call GETINPi(cans,npow)
	endif
	if(lkey.eq.5) then
		call DEFOLTR(XTIC,defolt)
		call QDIALOG(1,'X TIC',defolt,ict,cans)
		call GETINPR(cans,XTIC)
		call DEFOLTR(YTIC,defolt)
		call QDIALOG(1,'Y TIC',defolt,ict,cans)
		call GETINPR(cans,YTIC)
		call DEFOLTR(ZTIC  ,defolt)
		call QDIALOG(1,'Z TIC',defolt,ict,cans)
		call GETINPR(cans,ZTIC)
	endif
	if(lkey.eq.6) then
		call DEFOLTR(CSIZE,defolt)
		call QDIALOG(1,'SIZE ',defolt,ict,cans)
		call GETINPR(cans,CSIZE)
	endif
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c     goto 152

c========================================================================
c                           POSH OPTION
c========================================================================
5	continue
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,5,Lb,text,0,0)		!delete all
351	continue
	text(1) =' 1 : FIX TEXT'
	text(10) ='10: REDRAW'
	DO I=2,9
		Lb(i)=0
	ENDDO
	DO I=11,15
		Lb(i)=0
	ENDDO
	call broken(0)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
352	call CKEY(ch,jkey)
	if(jkey.eq.10) then
	      call FILLWIN(0,55,639,387,icol(71))		!graph area
		goto 100
	endif
	if(jkey.lt.-2.or.jkey.gt.10) goto 351
	if(jkey.eq.-1) then	!F1 key
	   call VHELP(10)
	   goto 352		!another menu choice
	else if(jkey.eq.-2) then	!F2=help index
	   call VHELP(-3)
	   goto 352		!another menu choice
	endif
c========================================================================
C		FIX TEXT
c========================================================================
601 	CONTINUE
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,5,Lb,text,0,0)		!delete all
661	continue
	text(1) =' 1: TITLE'
	text(2) =' 2: XLABEL'
	text(3) =' 3: YLABEL'
	text(4) =' 4: ZLABEL'
	text(10) ='10: REDRAW'
	do i=5,9
	   Lb(i)=0
	enddo
	do i=11,15
	   Lb(i)=0
	enddo
	call broken(0)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
662	call CKEY(ch,ikey)
	if(ikey.lt.-2.or.ikey.gt.10) goto 661
	if(ikey.eq.-1) then	!F1 key
	   call VHELP(10)
	   goto 662		!another menu choice
	else if(ikey.eq.-2) then	!F2=help index
	   call VHELP(-3)
	   goto 662		!another menu choice
	endif
	if(ikey.eq.10) goto 5
	if(ikey.eq.1) then
	   str=title1
	   angle=alfa
	   size=sizet
	   icolor=icol(25)
	   ifont=ifnt
	   xm=xst
	   ym=yst
	   ijus=ijust
	   idraw=idrawt
	else if(ikey.eq.2) then
	   str=xtitle
	   angle=beta
	   size=sizel
	   icolor=icol(23)
	   ifont=ifnl
	   xm=xspx
	   ym=yspx
	   ijus=ijusx
	   idraw=idrawx
	else if(ikey.eq.3) then
	   str=ytitle
	   angle=gama
	   size=sizel
	   icolor=icol(23)
	   ifont=ifnl
	   xm=xspy
	   ym=yspy
	   ijus=ijusy
	   idraw=idrawy
	else if(ikey.eq.4) then
	   str=ztitle
	   angle=delta
	   size=sizel
	   icolor=icol(23)
	   ifont=ifnl
	   xm=xspz
	   ym=yspz
	   ijus=ijusz
	   idraw=idrawz
	endif
	ibk=icol(71)
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,5,Lb,text,0,0)		!delete all
171	continue
	text(1) =' <-: MOVE LEFT'
	text(2) =' ->: MOVE RIGHT'
	text(3) =' v : MOVE DOWN'
	text(4) =' ^ : MOVE UP'
	text(5) =' S : SIZE   '
	text(6) =' C : COLOR  '
	text(7) =' F : FONT   '
	text(8) =' + : INC ANGLE'
	text(9) =' - : DEC ANGLE'
	text(10) ='10: REDRAW'
	do i=11,15
	   Lb(i)=0
	enddo
	call broken(0)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
33	ans=getch(ktype)
	ival=ichar(ans)
	if(ktype.eq.3.and.ival.eq.48) goto 400
	if(ktype.eq.8) then	!move menu with ARROW keys
	   if(ival.eq.77) then	!right
		call setfnt(ifont)
		call lincols(ibk,idev)
	      call linwid(1.0)
	      call justifystring(xm,ym,str,angle,size,ijus)
		if(xm-deltax.lt.xwmax) then
	         xm=xm+deltax
		endif
		call lincols(icolor,idev)
	      call justifystring(xm,ym,str,angle,size,ijus)
	   endif
	   if(ival.eq.75) then		!shift left
		call setfnt(ifont)
		call lincols(ibk,idev)
	      call linwid(1.0)
	      call justifystring(xm,ym,str,angle,size,ijus)
		if(xm+deltax.gt.xwmin) then
	         xm=xm-deltax
		endif
		call lincols(icolor,idev)
	      call justifystring(xm,ym,str,angle,size,ijus)
	   endif
	   if(ival.eq.72) then	!shift up
		call setfnt(ifont)
		call lincols(ibk,idev)
	      call linwid(1.0)
	      call justifystring(xm,ym,str,angle,size,ijus)
		if(ym-deltay.lt.ywmax) then
	         ym=ym+deltay
		endif
		call lincols(icolor,idev)
	      call justifystring(xm,ym,str,angle,size,ijus)
	   endif
	   if(ival.eq.80) then	!shift down
		call setfnt(ifont)
		call lincols(ibk,idev)
	      call linwid(1.0)
	      call justifystring(xm,ym,str,angle,size,ijus)
		if(ym+deltay.gt.ywmin) then
	         ym=ym-deltay
		endif
		call lincols(icolor,idev)
	      call justifystring(xm,ym,str,angle,size,ijus)
	   endif
	   goto 33
	else if(ktype.eq.3) then
	   if(ival.eq.43) then
		call setfnt(ifont)
		call lincols(ibk,idev)
	      call linwid(1.0)
	      call justifystring(xm,ym,str,angle,size,ijus)
	      angle=angle+ang
		call lincols(icolor,idev)
	      call justifystring(xm,ym,str,angle,size,ijus)
	   endif
	   if(ival.eq.45) then
		call setfnt(ifont)
		call lincols(ibk,idev)
	      call linwid(1.0)
	      call justifystring(xm,ym,str,angle,size,ijus)
	      angle=angle-ang
		call lincols(icolor,idev)
	      call justifystring(xm,ym,str,angle,size,ijus)
	   endif
	   goto 33
	else if(ktype.eq.2) then
	   if(ival.eq.99) then
 	   	call DEFOLTi(icolor,defolt)
	      call QDIALOG(1,'Color ',defolt,11,cans)
	   	call GETINPi(cans,icolor)
		call lincols(icolor,idev)
	      call justifystring(xm,ym,str,angle,size,ijus)
	   endif
	   if(ival.eq.102) then
		ifont0=ifont
 	   	call DEFOLTr(size,defolt)
	      call QDIALOG(1,'Size ',defolt,11,cans)
	   	call GETINPr(cans,size)
		call setfnt(ifont0)
		call lincols(ibk,idev)
	      call justifystring(xm,ym,str,angle,size0,ijus)
		call setfnt(ifont)
		call lincols(icolor,idev)
	      call justifystring(xm,ym,str,angle,size,ijus)
	   endif
	   if(ival.eq.115) then
		size0=size
 	   	call DEFOLTr(size,defolt)
	      call QDIALOG(1,'Size ',defolt,11,cans)
	   	call GETINPr(cans,size)
		call lincols(ibk,idev)
	      call justifystring(xm,ym,str,angle,size0,ijus)
		call lincols(icolor,idev)
	      call justifystring(xm,ym,str,angle,size,ijus)
	   endif
	   goto 33
	endif
	if(ktype.eq.16.and.ival.eq.27) then
   	   GOTO 400
	endif
400   continue
	if(ikey.eq.1) then
	     alfa    = angle
	     sizet   = size
	     icol(25)= icolor
	     ifnt    = ifont
	     xst     = xm
	     yst     = ym
	     ijust   = ijus
	     idrawt  = idraw
	else if(ikey.eq.2) then
	     beta    =angle
	     sizel   =size
	     icol(23)=icolor
	     ifnl    =ifont
	     xspx    =xm
	     yspx    =ym
	     ijusx   =ijus
	     idrawx  =idraw
	else if(ikey.eq.3) then
	     gama    =angle
	     sizel   =size
	     icol(23)=icolor
	     ifnl    =ifont
	     xspy    =xm
	     yspy    =ym
	     ijusy   =ijus
	     idrawy  =idraw
	else if(ikey.eq.4) then
	     delta    =angle
	     sizel    =size
	     icol(23) =icolor
	     ifnl     =ifont
	     xspz     =xm
	     yspz     =ym
	     ijusz    =ijus
	     idrawz   =idraw
	endif
	iperf=-100
   	GOTO 601

c========================================================================
c                           READ IN TITLE
c========================================================================
6	continue
	call DCMENU(-6,5,Lb,text,0,0)	!delete box 4 only
	Lb(6)=-1
	call DCMENU(-6,5,Lb,text,icol1,icol2)	!draw box 4 only (italic)
	call setfnt(ifnt)
	call lincols(icol(71),0)
	if(idrawt.eq.1) then	!delete existing title first
	   call JUSTIFYSTRING(xst,yst,title1,alfa,
     &	 sizet,ijust)
	endif
	call GSTRING(xst,yst,title1,ifnt,alfa,sizet,
     &   ijust,icol(25),icol(71),75,blank,nrealt)
	nlg=nblank1(title1)

	if (title1(nlg:nlg).eq.char(95)) title1(nlg:nlg)=char(32)
	if(nlg.gt.1) nlg=nlg-1
	if(idrawt.eq.0.and.nlg.gt.1) idrawt=1
	goto 100

c========================================================================
c                           PLOT NOW
c========================================================================
7	continue
c 	open a 2 row dialog box top=row 23; cols 2-58
	landplot=.true.		!landscape is default
c set whole page as default
c NB ixlo already set (and possibly reset by 'MOVE GRAPH') so do set here
	call SETLBOX(nbox,Lb,1)
	Lb(1)=-1		!whole page is default
	call DCMENU(0,5,Lb,text,0,0)		!delete all
1551	continue
	nxlo=8
	nylo=200
	nyhi=-1
	TITLEs     ='   PLOT OPTIONS    '
	strings(1)='1. Black&white postscript '
	strings(2)='2. Black&white laserjet '
	strings(3)='3. Color deskjet'
	strings(4)='4. Metafile color (*.cgm)'
	strings(5)='5. metafile Grey shades (*.cgm)'
	strings(6)='6. Ascii File   '
	strings(7)='7. End          '
	nval=7

	ictm=15		!text white
	ibkm=1		!background daek blue
	icfm=11		!frame/title light blue
	icupm=12		!upper case red
777	nhelp=0
	call POPMENU(nxlo,nylo,nyhi,strings,nval,ictm,icfm,
     & icupm,ibkm,titles,helps,nhelp,line1,charout,ival)
      if(line1.ge.1.and.line1.le.nval) then	!iline=0 for ESC=cancel
	   ihelp=line1
	else
	   ihelp=7
	endif

	if(ihelp.eq.1.or.ihelp.eq.2.or.ihelp.eq.3) then
	   plot=.true.
	   if(ihelp.eq.3) then
		idev=6
	   endif

	else if(ihelp.eq.4.or.ihelp.eq.5) then
	   meta=.true.
	   idev=2
	   metafil='cvfit.cgm'
	   CALL WDIALOG(1,'OPTIONS FOR FILES NAMES ',ict)
765	   continue
	   nc=nblank1(metafil)
	   call qdialog(1,'Name for metafile ',
     &   ' ['//metafil(1:nc-4)//'] = ',ict,cans)
	   if(cans.eq.' ') goto 654
	   nc1=nblank1(cans)
	   if(nc1.le.20) then
		metafil=cans(1:nc1)//'.cgm'
		goto 654
	   endif
	   call wdialog(1,'YOU ENTERED AN INVALID FILE NAME',ict)
	   goto 765
654      continue
	else if(ihelp.eq.6) then
	   call TDIALOG(1,'File name for ascii OUTPUT (+ path if nec)',
     &	filnam,33,.true.,ict)
	   INQUIRE(file=filnam,exist=present,flen=len)
	   if(present.and.len.gt.0) then
	      call BELL(2)
		ans='N'
 		call DEFOLTa(ans,defolt)
		call QDIALOG(1,
     &    ' The file '//charnb(filnam)//' already exists: overwrite it'
     &        ,defolt,12,cans)
 		call GETINPa(cans,ans)
	   endif
	   cnumx='        numx'
	   cnumy='        numy'
	   ci=   '  xaxis -> i'
	   cj=   '  yaxis -> j'
	   ck=   '  zaxis -> k'
	   cx=   '  x   values'
	   cy=   '  y   values'
	   cz=   '  z   values'
	   cxmin='        xmin'
	   cxmax='        xmax'
	   cymin='        ymin'
	   cymax='        ymax'
	   czmin='        zmin'
	   czmax='        zmax'
	   OPEN(unit=14,file=filnam,status='UNKNOWN',
     &    access='SEQUENTIAL',form='FORMATTED')
890	   FORMAT(2I12)
891	   FORMAT(6F12.5)
892	   FORMAT(3I12,3F12.5)
893	   FORMAT(2A12)
894	   FORMAT(6A12)
	   write(14,893) cnumx,cnumy
	   write(14,FMT=890) numx,numy
	   write(14,894) cxmin,cxmax,cymin,cymax,czmin,czmax
	   write(14,FMT=891) xmin,xmax,ymin,ymax,zmin,zmax
	   k=1
	   i=1
8931	   j=1
	   write(14,894) ci,cj,ck,cx,cy,cz
8930	   continue
	   if(inter) then
	      write(unit=14,fmt=892) I,J,K,GAX(k),GAY(k),GRIDZ(I,J)
	   else
	      write(unit=14,fmt=892) I,J,K,AX(k),AY(k),AZ(k)
	   endif
	   j=j+1
	   k=k+1
	   if(j.le.numy) goto 8930
	   i=i+1
	   if(i.le.numx) goto 8931
	   CLOSE(unit=14)
	   goto 777
	else if(ihelp.eq.7) then
	   plot=.false.
	   meta=.false.
	   goto 1814
	else
	   goto 777
	endif

c=====================================================================
	jopt=1
	call DEFOLTi(jopt,defolt)
	call QDIALOG(1,'(1) Plot now, or (2) Cancel plot',
     &  defolt,ict,cans)
	call GETINPi(cans,jopt)
	if(jopt.eq.1) then
	   if(ihelp.eq.1.or.ihelp.eq.2.or.ihelp.eq.3) then
	     nopt=1
	     call DEFOLTi(nopt,defolt)
	     call QDIALOG(1,'(1) Landscape, or (2) Portrait',
     &     defolt,ict,cans)
	     call GETINPi(cans,nopt)
	     if (nopt.eq.1) then
		  landplot=.true.
		  if(ihelp.eq.1) then
			idev=4
		  else if(ihelp.eq.2) then
			idev=5
		  else if(ihelp.eq.3) then
			idev=6
		  endif
	     else if (nopt.eq.2) then
		  landplot=.false.
	        vxlo=vylo		! for VIEWPORT
	        vxhi=vyhi
	        vylo=vxlo
	        vyhi=vxhi
		  if(ihelp.eq.1) then
		  	idev=3
	   	  else if(ihelp.eq.2) then
			idev=5
	   	  else if(ihelp.eq.3) then
			idev=6
		  endif
	     endif
	   endif
	else
	   plot=.false.
	   meta=.false.
	   goto 777
	endif

c=====================================================================
	if (meta) then
	   call WDIALOG(1,
     &   'Assembling the metafile. . .',ict)
	   call devend
	   META=.TRUE.
	   ICGMDV=14
	   OPEN(UNIT=ICGMDV,FILE=metafil,STATUS='UNKNOWN')
	   CALL CGMBi
	   CALL DEVICE(ICGMDV,0)
	   call rgbdef(0,0.0,0.0,0.0)
	   call errswi(-1)
	   call brkswi(1)
	   call gsetcols(0)
	   call mode(18)
	   call WDIALOG(1,'Metafile in progress . . .',12)
	else if (plot) then
	   if(ihelp.eq.2) then
	   	oldfile='HPLJR.OUT'
		if(landplot) then
			a=2.
			XPP=280.5
			YPP=196.7
		else
			a=1.4
			yPP=280.5
			xPP=196.7
			b=xp
			xp=yp
			yp=b
		endif
	   else if(ihelp.eq.3) then
	   	oldfile='DJ500C.OUT'
		if(.not.landplot) then
			a=1.4
			XPP=203.2
			YPP=266.7
			b=xp
			xp=yp
			yp=b
		else
			a=2.
			yPP=203.5
			xPP=266.7
		endif
	   else if(ihelp.eq.1) then
	   	oldfile='EPS.OUT'
		if(idev.eq.3)then
			XPP=210.
			YPP=297.
			b=xp
			xp=yp
			yp=b
			a=1.4
		else
			a=2.
			XPP=297.
			YPP=210.
		endif
	   endif
	   INQUIRE(file=oldfile,exist=present,flen=len)
	   if(present) call ERASE(oldfile)
	   call FILLWIN(0,0,639,387,icol(71))		!graph area
	   call FILLWIN(0,388,639,479,1)			!dialog area
	   jopt=1
	   call DEFOLTi(jopt,defolt)
	   call QDIALOG(1,' (1) Plot now, or (2) Cancel plot',
     &   defolt,ict,cans)
	   call GETINPi(cans,jopt)
	   if(jopt.ne.1) then
	      plot=.false.
	      meta=.false.
	      goto 1814
	   endif
	   call WDIALOG(1,
     &      'Assembling the plot . . .',ict)
	   call devend
	   if(ihelp.eq.1) then
	      if(landplot) then
		   printer='postscript landscape'
	   	   call eps(1,5.,5.,297.,210.,297.,210.)
	      else
		   printer='postscript portrait'
	   	   call eps(1,5.,5.,210.,297.,210.,297.)
	      endif
	   else if(ihelp.eq.2) then
		   if(landplot) then
		   	printer='laserjet landscape'
		   else
		   	printer='laserjet portrait'
		   endif
	   	   call hpljr
	   else if(ihelp.eq.3) then
		   if(landplot) then
		   	printer='color landscape'
		   else
		   	printer='color portrait'
		   endif
	   	   call DJ500C
	   endif
	   call errswi(-1)
	   call brkswi(1)
1042	   continue
	   call papenq(xpp,ypp,itype)
	   if(ihelp.eq.2.and..not.landplot) call devpap(196.7,280.5,itype)
	   if(ihelp.eq.3.and.landplot) call devpap(266.7,203.5,itype)
	   call WDIALOG(1,'Plotting in progress ..'//printer,12)
	   call mode(18)
	endif
	call FILLWIN(0,0,639,387,icol(71))		!graph area
	call FILLWIN(0,388,639,479,1)			!dialog area
	goto 1002	!do plot and return to 1812

c=================================================================================7

5555  continue
	call WDIALOG(1,'Metafile done. . .',12)
	meta=.false.
	CLOSE(UNIT=ICGMDV)
	CALL DEVEND
c==========================================
9876	continue
	call VGA
	call gsetcols(0)
 	call brkswi(1)
 	call chaswi(1)
 	call grfmod (1)
 	call harcha
	call mode(18)
c	call errswi(-1)
c	call brkswi(1)
	idev=0
	after=.true.
	goto 1002

c====================================================================================
c After plot finished:
1812	continue
	call DEVEND
	if(idev.eq.2) then
	   printfil='cvfit.out'
	   CALL WDIALOG(1,'OPTIONS FOR FILES NAMES ',ict)
7651	   continue
	   nc=nblank1(printfil)
	   call qdialog(1,'Name for printfile ',
     &   ' ['//printfil(1:nc-4)//'] = ',ict,cans)
	   if(cans.eq.' ') goto 6541
	   nc1=nblank1(cans)
	   if(nc1.le.20) then
		printfil=cans(1:nc1)//'.out'
		goto 6541
	   endif
	   call wdialog(2,'YOU ENTERED AN INVALID FILE NAME',ict)
	   call COPY(oldfile,printfil)
	   goto 7651
6541	   continue
	endif
	if(idev.eq.4.or.idev.eq.3.or.idev.eq.5.or.idev.eq.6) then
	   kp=kp+1
	   call intconv(kp,kpchar)
	   nc1=nblank1(kpchar)
	   if(ihelp.eq.1) then
	   	printfil='efit'//kpchar(1:nc1)//'.out'
	   	call COPY(oldfile,'lpt1')
	   else if(ihelp.eq.2 ) then
	   	printfil='lfit'//kpchar(1:nc1)//'.out'
	   	call system('copy/b hpljr.out lpt1:')
	   else if(ihelp.eq.3) then
	   	call system('copy/b dj500c.out lpt1:')
	   	printfil='cfit'//kpchar(1:nc1)//'.out'
		pstring='copy/b '//printfil//' lpt1:'
		np=nblank1(pstring)
	   endif
	endif

	call VGA
	call gsetcols(0)
 	call brkswi(1)
 	call chaswi(1)
 	call grfmod (1)
 	call harcha
	call mode(18)
c	call errswi(-1)
c	call brkswi(1)
c	call VGA
c	call errswi(-1)
c	call brkswi(1)
c	call gsetcols(0)
c	call mode(18)
1814	continue
	idev=0
	plot=.false.
c 	Restore idev=0 (and scalfac, if altered)
c	scalfac=1.0
	call setfnt(ifont)
	vxlo=vxlo1		! for VIEWPORT
	vxhi=vxhi1
	vylo=vylo1
	vyhi=vyhi1
1043	continue
	after=.true.
	goto 1002


c======================================================================
c				queue plot
c========================================================================
8  	goto 152

c======================================================================
c				END DISPLAY
c========================================================================
9  	goto 999

c==================================================================
c			REDRAW
c==================================================================
10	CONTINUE
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100


c======================================================================
c				AXIS LABELS
c========================================================================

c Section to read in new axis labels
11	continue
	call DCMENU(-ikey,5,Lb,text,0,0)	!delete box #ikey only
	Lb(ikey)=-1
	call DCMENU(-ikey,5,Lb,text,icol1,icol2)	!draw box #ikey only (italic)
	call WDIALOG(1,'Now write text: hit F1 anytime for HELP  ',ict)
	call setfnt(ifnl)
	call lincols(icol(71),0)
	if(ikey.eq.11) then
		if(idrawx.eq.1) then	!delete existing title first
	   		call JUSTIFYSTRING(xspx,yspx,xtitle,beta,
     &	 sizel,ijusx)
		endif
		call GSTRING(xspx,yspx,xtitle,ifnl,beta,sizel,
     &   	ijusx,icol(23),icol(71),75,blank,nrealt)
		if(idrawx.eq.0) idrawx=1
	else if(ikey.eq.12) then
		if(idrawy.eq.1) then	!delete existing title first
	   		call JUSTIFYSTRING(xspy,yspy,ytitle,gama,
     &	 sizel,ijusy)
		endif
		call GSTRING(xspy,yspy,ytitle,ifnl,gama,sizel,
     &   	ijusy,icol(23),icol(71),75,blank,nrealt)
		if(idrawy.eq.0) idrawy=1
	else if(ikey.eq.13) then
		if(idrawz.eq.1) then	!delete existing title first
	   		call JUSTIFYSTRING(xspz,yspz,ztitle,delta,
     &	 sizel,ijusz)
		endif
		call GSTRING(xspz,yspz,ztitle,ifnl,delta,sizel,
     &   	ijusz,icol(23),icol(71),75,blank,nrealt)
		if(idrawz.eq.0) idrawz=1
	endif
c==	ilabel=1		!so internal default labels not used
	goto 152


c========================================================================
c                           FILL
c========================================================================
14	continue
	IF(FILL) THEN
	   FILL=.FALSE.
	   if(allocated(ifillg)) then
		deALLOCATE(ifillg)
	   endif
 	   call FILLWIN(0,55,639,387,icol(71))	!graph area
	   goto 100
c	   GOTO 152
	ELSE
	   FILL=.TRUE.
	ENDIF
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,5,Lb,text,0,0)		!delete all
1514  continue
	text(1) =' 1 : CONTOURS  '
	text(2) =' 2 : ONE COLOUR'
	text(3) =' 3 : UP/DOWN   '
	text(10) ='10: REDRAW'
	call SETLBOX(nbox,Lb,1)
	do i=4,9
	   Lb(i)=0
	enddo
	do i=11,15
	   Lb(i)=0
	enddo
	call broken(0)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
1524	call CKEY(ch,ikey)
	if(ikey.lt.-2.or.ikey.gt.15) goto 1514
	if(ikey.eq.-1) then	!F1 key
	   call VHELP(15)
	   goto 1524		!another menu choice
	else if(ikey.eq.-2) then	!F2=help index
	   call VHELP(-3)
	   goto 1524		!another menu choice
	endif
	if(ikey.eq.1) then
	   kcol=1
	   call DEFOLTi(ncont,defolt)
	   call QDIALOG(1,'How many',defolt,ict,cans)
         call GETINPi(cans,ncont)
	   if(ncol.lt.2) ncont=2
      else if(ikey.eq.2) then
	   kcol=2
	   call DEFOLTi(ncol,defolt)
	   call QDIALOG(1,'Colour',defolt,ict,cans)
         call GETINPi(cans,ncol)
      else if(ikey.eq.3) then
	   kcol=3
	endif
C  	CALCULATE IFILL ARRAY FROM POINT LIGHT SOURCE
 	   call FILLWIN(0,55,639,387,icol(71))		!graph area
	   goto 100
c	GOTO 152




c========================================================================

c TIDY UP AND EXIT
999	continue
	iask=-1
	if(iask.gt.0) call DCMENU(0,5,Lb,text,0,0)	!delete boxes before exit
 	if(iask.lt.0)call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	if(allocated(az)) then
	   DEALLOCATE(az,ax,ay)
	endif
	if(allocated(ifillz)) then
	   DEALLOCATE(ifillz)
	endif
	if(allocated(ifillg)) then
	   DEALLOCATE(ifillg)
	endif
	if(allocated(gridz)) then
	   DEALLOCATE(gridz)
	endif
	vxlo=0.	! for VIEWPORT
	vxhi=xp
	vylo=0.
	vyhi=yp

c	CLOSE(unit=12)
	call mode(3)
c
	nbinx=nbxsav
	nbiny=nbysav
c===================
	DEALLOCATE(z)
c===================
c
	RETURN
	END

c========================================================================

      SUBROUTINE SCOL(NDC,ISET)
      DO 100 I=ISET,ISET+NDC
        CALL HLSDEF(I,80.0,
     +         float(I-ISET)/float(NDC*3)+0.2,0.8)
100   CONTINUE
      RETURN
      END
