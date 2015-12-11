c======================================================================
	subroutine gplot3d(xval1,yval1,z1,bad,nx,ny,ndx,ndy,
     &	xtitle,ytitle,ztitle,title1,idrawt,idrawx,idrawy,idrawz,
     &       kcol,posneg,isetcol,qfile)
c=====================================================================
c     19/02/1999
c-----------------------------------------------------------------------
c     now kcol,posneg in call:
c	kcol=0 + posneg=.false.	: fill=.false.    default
c	kcol=1 + posneg=.false. : fill=.true.	one colour
c	kcol=2 + posneg=.false	: fill=.true.	two colours (up/down)
c	kcol=2 + posneg=.true.	: fill=.true.	two colours (pos/neg)
c     kcol=4 + posneg=.true.	: fill=.true.     four colours(pos/neg)
c     kcol<0 + posneg=.false. : fill=.true.     contours (ncont=abs(kcol))
c-------------------------------------------------------------------------
c     isetcol
c	isetcol=2 3D plots : screen colours:
c		icol(1)=1	   !contour 1
c		icol(2)=9	   !cont 2
c		icol(3)=3	   !cont 3
c		icol(4)=11   !cont 4
c		icol(5)=2	   !cont 5
c		icol(6)=10   !cont 6
c		icol(7)=8	   !cont 7
c		icol(8)=5	   !cont 8
c		icol(9)=13   !cont 9
c		icol(10)=6	   !cont 10
c		icol(11)=4	   !cont 11
c		icol(12)=12   !cont 12
c		icol(13)=14   !cont 13
c		icol(14)=15   !cont 14
c		icol(15)=7	   !cont 15
c		icol(21)=11	!axes
c		icol(22)=0   !bad region
c		icol(23)=11	!labels
c		icol(25)=14	!title
c		icol(31)=14	!upper net
c		icol(32)=8	!lower net
c		icol(33)=9 	!upper surface
c		icol(34)=10	!lower surface
c		icol(35)=12	!positive
c		icol(36)=11	!negative
c		icol(37)=4	!positive lower
c		icol(38)=9	!negative lower
c		icol(71)=7
c-------------------------------------------------------------------------
c	4/12/97 modified all the menues to be more similar to vplot5.
c     mark bad regions is now an option when  the fill is on
c
c*******NB sometimes does not put zero at right place on Z axis -seems
c*******that must not use FIXAX on Zmin Zmax either (even without it can
c*******be wrong on first display
c
c
c  Modif 11/16/97 01:49pm by adding array bad(i,j)=true if a value is
c undefined. The four tiles that surround a bad point are filled, with
c colour icbad, if fillbad is set true.
c Note that ifill(i,j) is the tile to top right of z(i,j)
c NB x, y values must be equally-spaced to get correct labelling of x,y axes
c Also add ndx, ndy=declared dimensions of z1(), xval1, yval1, bad()
c  in calling prog
c
c Scaling problems
c (1) the data provided seem to be plotted over the entire range
c	of the axes, regardless of the numbers on the axes. Thus if
c	data cover X range 0-1, we get the same graph even if the
c	axes are changed so they are labelled -50 to +50!  Thus to
c	get correct numbering must (a) not use FIXAX, and (b) can
c	alter xmin,xmax etc, only by changing data (c) xmin, xmax etc must
c	be set to actual observed values of x, not any value.
c (2) The array z(i,j) in call to PERPRJ must be declared as having dimensions
c	that are the current values of nx, ny (since this subroutine
c	has not got separate arguments to specify its declared dimensions)
c	so must keep original arrays as z1, xval1,yval1 and then allocate
c	z, xval, yval with the right dimensions, and copy required elements
c	to them
c
c (3)	tic control : length only by CHASIZ
c			: number by PRJSCA
c
	real*4 z1(ndx,ndy)
	real*4 xval1(ndx),yval1(ndy)
c	real*4 z1(nx,ny)
c	real*4 xval1(nx),yval1(ny)
	logical bad(ndx,ndy)
c
	allocatable::gridz,az,ax,ay,w,IFILLZ,IFILLG
	real*4 gridz(:,:),az(:),ax(:),ay(:),w(:)
	allocatable::z,xval,yval
	real*4 z(:,:),xval(:),yval(:)
 	INTEGER*4 IFILLZ(:,:),IFILLG(:,:)
	allocatable::rax,ray
	real*4 rax(:),ray(:)
	allocatable::badg
	logical badg(:,:),mark
	logical autplt
      character qfile*40
	logical fillbad
	integer*4 Lb(30),icol(100),icolsav(100)	!for DCMENU
	integer*4 icord(15,2)		!for 2 alternative colour orders
	character*75 xtitle,ytitle,ztitle,str
	character*75 title1
	character*78 text(30)
	character defolt*30,cans*30,kpchar*5,ans,getch,UC		!to hold text & result of QDIALOG
	character*12 cxmin,cxmax,cymin,cymax,czmin,czmax,cnumx,cnumy
	character*12 ci,cj,ck,cx,cy,cz
	character*11 cnum1,cnum2,cnum3
	logical all,cross,after,posneg,monsav
	logical mono,plot,fill,inter,meta,present,landplot,axes,help
	logical wmeta,plotcols
	character*33 titles,pstring,filnam
	character*33 metafil,printfil,oldfile,printer,strings(10)
	character*33 wmetafil
      INTEGER ISTYLE(6),ISUP(6)
	common/DMENU/ifonb,csizeb,ifont2,nboxlast,nblast		!for DCMENU
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi
	common/hlp/help		!for QDIALOG
	COMMON/cols/icol,mono
	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls
	logical usedef
	common/def/usedef		!for qdialog
	common/help1/ihtype	!for VHELP
	common/plot3d/ifnt,ifnl,alfa,beta,gama,delta,
     &     ijust,ijusx,ijusy,ijusz,
     &     xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,numx,numy,
     &     xmin,xmax,ymin,ymax,zmin,zmax,xtic,ytic,ztic,
     &     ratio,rat,radius,theta,phi,ifram,numaxi,nwidth,npow,nplace,
     &     nxstep,nystep,istyle,isup,
     &     fill,inter,axes,fillbad,autplt,cross
c
c	qfile="C:\PLOTQ.DAT"
	ihtype=3	!tells VHELP that call was from GPLOT3D
c Set things that re not to be changed after rescale
	idev=0
	kdev=-1
	plot=.false.
	ncont=abs(kcol)
	if(autplt) goto 50
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
50    continue
	ang=5.
	deltax=10.
	deltay=10.
	call VGA
	call gsetcols(0)
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
	XWMIN=100.
	XWMAX=1050.
	YWMIN=50.
	YWMAX=620.
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
c====DC test different colour orders
c	icol(1)=1	!dark blue
c	icol(2)=9	!light blue
c	icol(3)=3	!dark cyan
c	icol(4)=11	!light cyan
c	icol(5)=2	!dark green
c	icol(6)=10	!light green
c	icol(7)=8	!dark grey
c	icol(8)=5	!dark magenta
c	icol(9)=13	!light magenta
c	icol(10)=6	!brown
c	icol(11)=4	!red-brown
c	icol(12)=12	!red
c	icol(13)=14 !yellow
c	icol(14)=15	!white
c	icol(15)=7	!light grey (usually background!)
c
c Now save inputs and define z(i,j)
c
	ny1=1
	ny2=ny
	nx1=1
	nx2=nx
	nxsav=nx      !save so can be restored before RETURN
	nysav=ny
c
c===============trick if nx changed by debugger/rescale
c===========YES -SEEMS TO WORK! IF nx CHANGED TO RESCALE THEN
C===========MUST REALLOCATE Z SO IT HAS THE CORRECT SIZE -MUST BE THAT
C========== CALL TO PERPRJ MUST HAVE nx=DECLARED DIMENSION OF Z
C===ASK BRADLEY TO ADD EXTRA PARAMS -THE DECLARED DIMENSIONS OF Z!!
	allocate(z(nx,ny),xval(nx),yval(ny))
	if(autplt) then
	   do i=1,nx
	      xval(i)=xval1(i)		!copy input
	      do j=1,ny
		   z(i,j)=z1(i,j)		!copy input
	         yval(j)=yval1(j)		!copy input
		enddo
	   enddo
	   iperf=-100
	   ncont=abs(kcol)
	   if(fillbad) mark=.true.
	   goto 1002
	endif
	numx=nx
	numy=ny
	xmin=1.e36
	xmax=-1.e36
	ymin=1.e36
	ymax=-1.e36
	zmin=1.e36
	zmax=-1.e36
	do i=1,nx
	   xval(i)=xval1(i)		!copy input
	   if(xval(i).gt.xmax) xmax=xval(i)
	   if(xval(i).lt.xmin) xmin=xval(i)
	   do j=1,ny
		z(i,j)=z1(i,j)		!copy input
		if(z(i,j).gt.zmax) zmax=z(i,j)
		if(z(i,j).lt.zmin) zmin=z(i,j)
	   enddo
	enddo
	do j=1,ny
	   yval(j)=yval1(j)		!copy input
	   if(yval(j).gt.ymax) ymax=yval(j)
	   if(yval(j).lt.ymin) ymin=yval(j)
	enddo
	ztic=(zmax-zmin)/10.
	xtic=(xmax-xmin)/10.
	ytic=(ymax-ymin)/10.
c
c Set fillbad=true initially if there are any bad values
	fillbad=.false.
	icol(22)=0
	do i=1,numx
	   do j=1,numy
		if(bad(i,j)) fillbad=.true.
	   enddo
	enddo
c
c	dx0=xval(2)-xval(1)	!grid spacing for input
c
c====================================================================
c Return here to redraw with new nx, ny after rescaling x,y
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
	IFRAM=3
	if(kcol.eq.0) then
		FILL=.FALSE.
	else
		FILL=.true.
	endif
	INTER=.FALSE.
	AXES=.TRUE.
	NUMAXI=0
	NWIDTH=5
	NPOW=0
	NPLACE=1
	nxstep=1
	nystep=1

	mono=.false.
	plotcols=.true.
	call SETCLS(mono,icol,autplt,plotcols,isetcol)
c
	kcol=2
	do i=1,6
	   istyle(i)=3
	   isup(i)=0
	enddo
1002	continue

c
c	PLOT GRAPH
c
      CALL SURF		!Surf banner
	CALL PAPENQ(XPAP,YPAP,IPAPTY)
      if(.not.mono.and.idev.eq.0) then
	    call FILLWIN(0,55,639,387,icol(71))		!graph area
	    call FILLWIN(0,388,639,479,1)			!dialog area
	endif
	if(.not.plot) then
	   call HLINE(0,639,54,15)		!line to mark bottom of data area
	   call HLINE(0,639,389,15)	!line to mark top of data area
	endif
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
	sizet=csize*2.
	sizel=csize*1.7
	width=1.0
	call OPENDIALOG(1,icf,.true.)		!draw dialog box #1
	call OPENDIALOG(2,icf2,.true.)		!draw dialog box #2
	call WDIALOG(2,'F1=HELP',ict)
	call WDIALOG(2,'F2=HELP INDEX',ict)
c=====================================================================
100   continue
c=	ncol=ncolj
	call linwid(width)
	if(idev.eq.0) then
         kdev=-1
	else if(idev.eq.6) then
         kdev=-6
	endif
	idim=nx*ny
	if(allocated(az)) then
	   deallocate(az,ax,ay)
	endif
	allocate(az(idim),ax(idim),ay(idim))
	k=1
	i1=0
	do i=nx1,nx2
	   i1=i1+1
	   j1=0
	   do j=ny1,ny2
		j1=j1+1
		ax(k)=xval(i1)
		ay(k)=yval(j1)
		az(k)=z(i1,j1)
		k=k+1
	   enddo
	enddo

C
C  INITIALISE VARIABLES
C
      xlow=xval(1)
      xhigh=xval(nx)
	dx=xval(2)-xval(1)
      ylow=yval(1)
      yhigh=yval(ny)
	dy=yval(2)-yval(1)
c
	np=nx*ny
C
C  MINIMUM WORKSPACE SIZES ARE :
C
      nw=max(2*numx*numy+4*np,numx*numy+20*max(numx,numy))
	if(allocated(w)) DEALLOCATE(w)
	allocate(w(nw))
	if(allocated(gridz)) then
	   DEALLOCATE(gridz)
	endif
	allocate(gridz(numx,numy))
      CALL RANGRD(NP,AX,AY,AZ,NUMX,XLOW,XHIGH,NUMY,YLOW,YHIGH,
     +   	GRIDZ,NW,W)
	if(fillbad) then
	   if(allocated(badg)) then
		deallocate(badg)
	   endif
	   ALLOCATE(badg(numx,numy))
	   do i=1,numx
		do j=1,numy
		   badg(i,j)=.false.	!initialise
		enddo
	   enddo
	endif
c For the purposes of colouring 'bad' points, need to know the i,j values
c in the new ifill() that correspond with each bad(i,j), if interpolation
c has been done -do this next.
c NB this version defines as bad only the 4 tiles round each bad point even
c when interpolated -if heavily interpolated the tiles are small so adjacent
c bad points appear as separated coloured areas -better to define as 'bad'
c enough tiles to make them non-separate -for this would need to look whether
c any adjacent point in the original data was bad, and if so fill all tiles
c between these adjacent bad points -not yet done
	if(inter.and.fillbad) then
c	   dx=gridz(2)-gridz(1)	!grid spacing for new grid
c	   fac=dx0/dx	!factor by which grid spacing reduced
	   facx=float(numx)/float(nxsav)  !factor by which grid spacing reduced
	   facy=float(numy)/float(nysav)  !factor by which grid spacing reduced
	   do i=1,nxsav
		do j=1,nysav
		   i1=ifixr(float(i-1)*facx)+1	!NB allow for origin=1
		   if(i1.gt.numx) i1=numx
		   if(i1.lt.1) i1=1
		   j1=ifixr(float(j-1)*facy)+1
		   if(j1.gt.numy) j1=numy
		   if(j1.lt.1) j1=1
		   if(bad(i,j)) badg(i1,j1)=.true.
		enddo
	   enddo
	endif
c
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
c	posneg=(gzmin.lt.0.0).and.(gzmax.gt.0.0)  !for contours
	do i=1,nx
	   do j=1,ny
		if(z(i,j).gt.zgmax) zgmax=z(i,j)
		if(z(i,j).lt.zgmin) zgmin=z(i,j)
	   enddo
	enddo
	bw=(gzmax-gzmin)/float(ncont)
 	if(allocated(rax)) then
 	   deALLOCATE(rax)
 	endif
 	allocate(rax(numx*numy))
 	if(allocated(ray)) then
 	   deALLOCATE(ray)
 	endif
 	allocate(ray(numx*numy))
 	dix=(ax(nx*ny)-ax(1))/float(numx)
 	diy=(ay(ny)-ay(1))/float(numy)
 	k=1
 	do i=1,numx
 	   do j=1,numy
 		rax(k)=ax(1)+i*dix
 		ray(k)=ay(1)+j*diy
 		k=k+1
 	   enddo
 	enddo
      if(allocated(ifillg)) then
	   DEALLOCATE(ifillg)
	endif
      if(allocated(ifillz)) then
	   DEALLOCATE(ifillz)
	endif
c   count as 'zero' any z value that is within 2.5% either side of zero
	z0=0.025*abs(gzmax-gzmin)
      ALLOCATE(ifillg(numx-1,numy-1))
      ALLOCATE(ifillz(numx-1,numy-1))
      DO I=1,NUMX-1
	   DO J=1,NUMY-1
		if(kcol.eq.1.or.kcol.eq.2) then
               if(kcol.eq.1) then
                  ncolone=icol(33)
                  call lincols(ncolone,kdev)
                  ifillg(i,j)=ncolone
		   else if(kcol.eq.2) then
			if(posneg) then
		   	   zv=gridz(i,j)
		         if(zv.le.0.0) then
		   	      ncoltwo=icol(36)
		            call lincols(ncoltwo,kdev)
		      	ifillz(i,j)=ncoltwo
		      	ifillg(i,j)=ncoltwo
		   	   else
		      	ncoltwo=icol(35)
		      	call lincols(ncoltwo,kdev)
		      	ifillz(i,j)=ncoltwo
		      	ifillg(i,j)=ncoltwo
		         endif
			else
                     ncolone=icol(33)
                     call lincols(ncolone,kdev)
                     ifillg(i,j)=ncolone
		         ncolone=icol(34)
		         call lincols(ncolone,kdev)
		         ifillz(i,j)=ncolone
			endif
		   endif
		else if(kcol.eq.4) then
		   	   zv=gridz(i,j)
		         if(zv.le.0.0) then
		   	   	ncoltwo=icol(38)
		            call lincols(ncoltwo,kdev)
		      	ifillz(i,j)=ncoltwo
		   	      ncoltwo=icol(36)
		            call lincols(ncoltwo,kdev)
		      	ifillg(i,j)=ncoltwo
		   	   else
		      	ncoltwo=icol(37)
		      	call lincols(ncoltwo,kdev)
		      	ifillz(i,j)=ncoltwo
		   	      ncoltwo=icol(35)
		            call lincols(ncoltwo,kdev)
		      	ifillg(i,j)=ncoltwo
		         endif
		else if(kcol.lt.0) then
		   do k=1,ncont
		      blo=gzmin+float(k-1)*bw
		      bhi=gzmin+float(k)*bw
		      zv=gridz(i,j)
		      if(zv.ge.blo.and.zv.le.bhi) then
			   ncolm=icol(k)
		         call lincols(ncolm,kdev)
			   ifillg(i,j)=ncolm !avoid 1
c		         ifillz(i,j)=ncolm+8
		         ifillz(i,j)=ncolm
			   goto 20
		      endif
		   enddo
		endif
20		continue
	   enddo
      enddo
	call setfnt(ifnl)
      CALL CHASIZ(csize,csize)
	if((cross.and..not.meta.and..not.plot.and..not.after.
     &   and..not.wmeta)) goto 444
	if(all) then
		CALL PRJBND(XLO1,XHI1,YLO1,YHI1)
	else
		CALL PRJBND(XLO,XHI,YLO,YHI)
	endif
	if(cross.and.meta.or.cross.and.plot.or.cross.and.after.or.cross.
     &	and.wmeta) call linvis(0)
      CALL PRJFRA(IFRAM)
	call prjgrd(nxstep,nystep)
      CALL WINDO2(XWMIN,XWMAX,YWMIN,YWMAX)
      CALL PRJSCA(xtic,ytic,ztic)
      CALL PRJAXI(ISTYLE,ISUP)
      CALL HEIRAT(RATIO)
      CALL XYRAT(RAT)
	call PRJLEV(zmin,zmax)
	call perset(xmin,xmax,ymin,ymax,zmin,zmax,radius,theta,phi)
	ngonu=icol(31)
	ngond=icol(32)
	ncolax=icol(21)
	call lincols(ngonu,kdev)
	call lincols(ngond,kdev)
	call lincols(ncolax,kdev)
	if(plot) then
      	CALL PRJLIN(ncolax,ngonu,ngond)
	else
      	CALL PRJLIN(ncolax,ngonu,ngond)
	endif
	IF(INTER) THEN
	   if(fill) then
		if(kcol.eq.2.or.kcol.eq.4) then
               CALL PERAXE(NW,W)		!axes displayed
		   call prjfra(4)
		   if(fillbad.and.mark) then
		      call SETBAD(badg,icol(22),numx,numy,numx,numy,
     &		 ifillz,numx-1,numy-1,kdev)
		   endif
    	         CALL PERFIL(NUMX,XMIN,XMAX ,NUMY,YMIN,YMAX,GRIDZ,
     +             RADIUS,THETA,PHI,IFILLZ,NW,W)	!grid displayed
		   call prjfra(2)
		endif
		if(fillbad.and.mark) then
		   call SETBAD(badg,icol(22),numx,numy,numx,numy,
     &	    ifillg,numx-1,numy-1,kdev)
		endif
    	      CALL PERFIL(NUMX,XMIN,XMAX ,NUMY,YMIN,YMAX,GRIDZ,
     +          RADIUS,THETA,PHI,IFILLG,NW,W)	!grid displayed
	   else
      	CALL PERPRJ(NUMX,XMIN,XMAX ,NUMY,YMIN,YMAX,GRIDZ,
     +              RADIUS,THETA,PHI,NW,W)	!grid displayed
	   endif
	ELSE		!not interpolated
	   if(fill) then
		if(kcol.eq.2.or.kcol.eq.4) then
               CALL PERAXE(NW,W)		!axes displayed
		   call prjfra(4)
		   if(fillbad.and.mark) then
			call SETBAD(bad,icol(22),nx,ny,ndx,ndy,
     &			ifillz,numx-1,numy-1,kdev)
		   endif
    	         CALL PERFIL(nx,XMIN,XMAX,ny,YMIN,YMAX,GRIDZ,
     +             RADIUS,THETA,PHI,IFILLZ,NW,W)	!grid displayed
			call prjfra(2)
		endif
		if(fillbad.and.mark) then
		   call SETBAD(bad,icol(22),nx,ny,ndx,ndy,
     &		 ifillg,numx-1,numy-1,kdev)
		endif
   		CALL PERFIL(nx,XMIN,XMAX,ny,YMIN,YMAX,Z,
     &          RADIUS,THETA,PHI,IFILLG,NW,W)
	   else	!no fill
   		CALL PERPRJ(nx,XMIN,XMAX,ny,YMIN,YMAX,Z,
     &         RADIUS,THETA,PHI,NW,W)
	   endif
	ENDIF
C  	ADD 3D AXES
      CALL WINDOW(2)
	if(cross) goto 444
	if(axes) then
	   call ANNDIG(NUMAXI,NWIDTH,NPLACE,NPOW)
	   call ANNSTR(1,'            ')
	   call ANNSTR(2,'            ')
     	   call ANNSTR(3,'            ')
	   call lincols(0,idev)
         if(fill.and.(kcol.eq.2.or.kcol.eq.4)) goto 6363
	   CALL PERAXE(NW,W)		!axes displayed
6363	   if(iperf.ne.-100) then
	      call perspa(0.4*(xmin+xmax),ymin-5*dy,zmin,xspx,yspx)
	      call perspa(xmin-4*dx,(ymin+ymax)/2,zmin,xspy,yspy)
c	      call perspa(xmax+3*dx,ymin-3*dy,(zmin+zmax)/2,xspz,yspz)
	      call perspa(xmin-4*dx,ymax,0.5*zmax,xspz,yspz)
	   endif
	   call lincols(icol(23),idev)
	   call linwid(1.0)
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
	if(iperf.ne.-100) then
	   call perspa((xmax+xmin)/2.,ymax+2*dx,1.8*zmax,xst,yst)
	endif
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
	if(wmeta) goto 5558
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
	text(2) ='2: (DEFAULTS)'
	text(3) ='3: GRAPH VIEW'
	text(4) ='4: GRAPH AXES '
	text(5) ='5: POSH OPTIONS'
	text(6) ='6: GIVE TITLE '
	text(7) ='7: PLOT NOW   '
	text(8) ='8: QUEUE PLOT'
	text(9) ='9: END DISPLAY'
	text(10)='0: REDRAW    '
	text(11)='+: X AXIS LABEL'
	text(12)='-: Y AXIS LABEL'
	text(13)='x: Z AXIS LABEL'
	if(cross) then
	   text(14)='/: 3D PROJECTION'
	else
	   text(14)='/: CROSS SECTION '
	endif
	text(15)='.: MORE OPTIONS'

	call SETLBOX(nbox,Lb,1)
	if(cross) then
	   do i=2,6
		Lb(i)=0
	   enddo
	   do i=11,13
		Lb(i)=0
	   enddo
	   Lb(8)=0
	endif
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
	if(cross) goto 14
	cross=.false.
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,5,Lb,text,0,0)		!delete all
	continue
	text(1)='1: XMIN, XMAX  '
	text(2)='2: YMIN, YMAX  '
	text(3)='3: ZMIN,ZMAX '
	text(4)='4: XTIC,YTIC,ZTIC'
	text(5)='5: TIC LAYOUT '
	text(10)='0: REDRAW    '
	do i=6,9
	   Lb(i)=0
	enddo
	do i=11,15
	   Lb(i)=0
	enddo
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
8112	call CKEY(ch,ikey)
	if(ikey.eq.-1) then	!F1 key
	   call VHELP(3)
	   goto 8112		!another menu choice
	else if(ikey.eq.-2) then	!F2=help index
	   call VHELP(-2)
	   goto 8112		!another menu choice
	endif
	RAT=1.0
	all=.false.
c==========================DC revisions to xmax etc
c NB xmin, xmax, ymin, ymax must correspond exactly to one of the
c existing x, y values so go through and look for the closest value
c and reset them to that, then redefine nx, ny to the appropriate
c value, reallocate z() to exactly that size and redrew completely
c For xmin take nearest  value below that requested (if there is one)
c For xmax take nearest  value above that requested (if there is one)
c Can we assume that xval, yval will always be in ascending order? Safer
c not to assume this? BUT NEXT BIT DOES (SKIP NEXT BIT IF XMIN NOT CHANGED)
c Must do this on z1,nxsav, so can increase xmax after it has been reduced
	if(ikey.eq.1) then
	   call DCMENU(-1,5,Lb,text,0,0)	!delete box 1 only
	   Lb(1)=-1
	   call DCMENU(-1,5,Lb,text,icol0,icol2)	!draw box 1 only (italic)
211	   call DEFOLT2r(xmin,xmax,defolt)
	   call QDIALOG(1,'Xmin, Xmax',defolt,ict,cans)
	   call GETINP2r(cans,xmina,xmaxa)
	   if(usedef) then	!defaults used (new common in qdialog)
	      nx1=1
	      nx2=nx
	      goto 8112
	   endif
	   xmin=xmina
	   xmax=xmaxa
	   do i=1,nxsav
	    if(xval1(i).gt.xmin) then
		if(i.gt.1) then
		   xmin=xval1(i-1)
		   nx1=i-1
		else
		   xmin=xval1(1)
		   nx1=1
		endif
		goto 12
	    endif
	   enddo
c        get here if all xval(i) are bigger then specified xmin, so use smallest
	   xmin=xval1(1)
	   nx1=1
12	   continue
	   do i=1,nxsav
	    if(xval1(i).gt.xmax) then
		if(i.le.nxsav) then
		   xmax=xval1(i)
		   nx2=i
		else
		   xmax=xval1(nxsav)
		   nx2=nxsav
		endif
		goto 13
	    endif
	   enddo
c        get here if all xval(i) are less then specified xmax, so use largest
	   xmax=xval1(nxsav)
	   nx2=nxsav
13	   continue
	   nx=nx2-nx1+1
	   call REALTOCH(xmin,cnum1,11)
	   call REALTOCH(xmax,cnum2,11)
	   call INTCONV(nx,cnum3)
	   ans='Y'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,'Nearest Xmin = '//charnb(cnum1)//', Xmax = '//
     &   charnb(cnum2)//', n = '//charnb(cnum3)//': OK',
     &   defolt,12,cans)
 	   call GETINPa(cans,ans)
	   if(ans.eq.'N') goto 211
	   goto 8112
	else if(ikey.eq.2) then
	   call DCMENU(-2,5,Lb,text,0,0)	!delete box 1 only
	   Lb(ikey)=-1
	   call DCMENU(-2,5,Lb,text,icol0,icol2)	!draw box 1 only (italic)
c        Now repeat above for ymin, ymax (if they are changed)
212	   call DEFOLT2r(ymin,ymax,defolt)
	   call QDIALOG(1,'Ymin, Ymax',defolt,ict,cans)
	   call GETINP2r(cans,ymina,ymaxa)
	   if(usedef) then	!defaults used (new common in qdialog)
	      ny1=1
	      ny2=ny
	      goto 8112
	   endif
	   ymin=ymina
	   ymax=ymaxa
	   do j=1,nysav
	    if(yval1(j).gt.ymin) then
		if(j.gt.1) then
		   ymin=yval1(j-1)
		   ny1=j-1
		else
		   ymin=yval1(1)
		   ny1=1
		endif
		goto 121
	    endif
	   enddo
c        get here if all yval(i) are bigger then specified ymin, so use smallest
	   ymin=yval1(1)
	   ny1=1
121	   continue
	   do j=1,nysav
	    if(yval1(j).gt.ymax) then
		if(j.le.nysav) then
		   ymax=yval1(j)
		   ny2=j
		else
		   ymax=yval1(nysav)
		   ny2=nysav
		endif
		goto 131
	    endif
	   enddo
c        get here if all xval(i) are less then specified xmax, so use largest
	   ymax=yval1(nysav)
	   ny2=nysav
131	   continue
	   ny=ny2-ny1+1
	   call REALTOCH(ymin,cnum1,11)
	   call REALTOCH(ymax,cnum2,11)
	   call INTCONV(ny,cnum3)
	   call WDIALOG(1,' Ymin = '//charnb(cnum1)//', Ymax = '//
     &   charnb(cnum2)//', n = '//charnb(cnum3),ict)
	   ans ='Y'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,'Nearest Ymin = '//charnb(cnum1)//', Ymax = '//
     &   charnb(cnum2)//', n = '//charnb(cnum3)//': OK',
     &   defolt,12,cans)
 	   call GETINPa(cans,ans)
	   if(ans.eq.'N') goto 212
	   ny=ny2-ny1+1
	   goto 8112
	else if(ikey.eq.3) then
	   call DCMENU(-3,5,Lb,text,0,0)	!delete box 1 only
	   Lb(ikey)=-1
	   call DCMENU(-3,5,Lb,text,icol0,icol2)	!draw box 1 only (italic)
	   call DEFOLT2r(zmin,zmax,defolt)
	   call QDIALOG(1,'Zmin, Zmax',defolt,ict,cans)
	   call GETINP2r(cans,zmin,zmax)
	   goto 8112
	else if(ikey.eq.4) then
	   call DCMENU(-4,5,Lb,text,0,0)	!delete box 1 only
	   Lb(ikey)=-1
	   call DCMENU(-4,5,Lb,text,icol0,icol2)	!draw box 1 only (italic)
		call DEFOLTR(XTIC,defolt)
		call QDIALOG(1,'X TIC',defolt,ict,cans)
		call GETINPR(cans,XTIC)
		call DEFOLTR(YTIC,defolt)
		call QDIALOG(1,'Y TIC',defolt,ict,cans)
		call GETINPR(cans,YTIC)
		call DEFOLTR(ZTIC  ,defolt)
		call QDIALOG(1,'Z TIC',defolt,ict,cans)
		call GETINPR(cans,ZTIC)
	      goto 8112
	else if(ikey.eq.5) then
	   call DCMENU(-5,5,Lb,text,0,0)	!delete box 1 only
	   Lb(ikey)=-1
	   call DCMENU(-5,5,Lb,text,icol0,icol2)	!draw box 1 only (italic)
		call DEFOLTR(CSIZE,defolt)
		call QDIALOG(1,'SIZE ',defolt,ict,cans)
		call GETINPR(cans,CSIZE)
	      goto 8112
	else if(ikey.eq.10) then
c        Reallocate z(i,j) to new size for new xmin,....
	   DEALLOCATE(z,xval,yval)
	   ALLOCATE(z(nx,ny),xval(nx),yval(ny))	!new size
	   j1=0
	   do j=ny1,ny2
	      j1=j1+1
	      yval(j1)=yval1(j)	!copy correct y values to yval()
	   enddo
	   i1=0
	   do i=nx1,nx2
	      i1=i1+1
	      xval(i1)=xval1(i)	!copy correct x values to xval()
	      j1=0
	      do j=ny1,ny2
		   j1=j1+1
		   z(i1,j1)=z1(i,j)	!copy correct x values to z()
	      enddo
	   enddo
	   call FILLWIN(0,55,639,387,icol(71))		!graph area
	   goto 100
	else
		goto 8112
	endif


c========================================================================
c                           DEFAULTS
c========================================================================
2	continue
	call FILLWIN(0,55,639,387,icol(71))		!graph area
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
	text(1) ='1: ROTATION ANGLE'
	text(2) ='2: VIEW DIRECTION'
	text(3) ='3: ASPECT RATIO X:Y'
	text(4) ='4: HEIGHT:BASE '
	text(5) ='5: INTERPOLATE'
	text(6) ='6: GRID DENSITY'
	text(7) ='7: SURFACES DRAWN'
	text(10)='0: REDRAW    '

	do i=8,15
		Lb(I)=0
	enddo
	Lb(10)=1
	call broken(0)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
252	call CKEY(ch,ikey)
	if(ikey.lt.-2.or.ikey.gt.11) goto 252
	if(ikey.eq.-1) then	!F1 key
	   call VHELP(18)
	   goto 252		!another menu choice
	else if(ikey.eq.-2) then	!F2=help index
	   call VHELP(-3)
	   goto 252		!another menu choice
	endif
	if(ikey.eq.8.or.ikey.eq.9) goto 252
	if(ikey.eq.10) goto 110
	call DCMENU(-ikey,5,Lb,text,0,0)	!delete box 1 only
	Lb(ikey)=-1
	call DCMENU(-ikey,5,Lb,text,icol0,icol2)	!draw box 1 only (italic)
	goto(101,102,103,104,105,106,107)ikey
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
c                           INTERPOLATE
c========================================================================
105	continue
	inter=.false.
	call DEFOLT2i(numx,numy,defolt)
	call QDIALOG(1,'Numx,Numy',defolt,ict,cans)
	call GETINP2i(cans,numx,numy)
	if(numx.eq.nx.and.numy.eq.ny) goto 222
	if(numx.lt.nx.or.numy.lt.ny) goto 105
	INTER=.TRUE.
222	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 100
c========================================================================
C				  GRID
c========================================================================
106	CONTINUE
	nxst=numx-1
	nyst=numy-1
	call intconv(nxst,cnum1)
	call intconv(nyst,cnum2)
	call wdialog(1,'For no grid enter '//cnum1(1:3)//','
     &	//cnum2(1:3),ict)
	call DEFOLT2i(nxstep,nystep,defolt)
	call QDIALOG(1,'jump x steps, y steps',defolt,ict,cans)
	call GETINP2i(cans,nxstep,nystep)
	call FILLWIN(0,55,639,387,icol(71))		!graph area
	GOTO 100

c========================================================================
c                          FRAME
c========================================================================
107	continue
	call WDIALOG(1,'Options: 0,1-base drawn ; 2-upper surface only',
     & ict)
	call WDIALOG(1,'Options: 3-both surfaces ; 4-lower surface only',
     & ict)
	call DEFOLTI(IFRAM,defolt)
	call QDIALOG(1,'Enter drawing option',defolt,ict,cans)
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
14	continue
111	continue
	if(cross) then
	   if(ikey.eq.14) then
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
c==	y_min=yval(ny/2)
c==	y_max=y_min
	y_min=ymin
	y_max=ymax
	if(all) then			!!!what is this for!!
	   x_miny=xval(nx/2)
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
	text(2)='2: AXES X1 '
	text(3)='3: AXES Y1 '
	text(4)='4: AXES Z1 '
	text(7)='7: AXES X2 '
	text(8)='8: AXES Y2 '
	text(9)='9: AXES Z2 '
	text(10)='0: REDRAW'
	DO I=5,6
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
	   if(axes) then
	   	axes=.false.
	   else
	   	axes=.true.
	   endif
	endif
	if(lkey.eq.10) then
	   call FILLWIN(0,55,639,387,icol(71))		!graph area
	   goto 100
	endif
	select case(lkey)
	   case(2)
	     ind=1
	     numaxi=1
	   case(3)
	     ind=2
	     numaxi=2
	   case(4)
	     ind=3
	     numaxi=3
	   case(7)
	     ind=4
	     numaxi=1
	   case(8)
	     ind=5
	     numaxi=2
	   case(9)
	     ind=6
	     numaxi=3
	end select
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,5,Lb,text,0,0)		!delete all
1450   continue
	text(1)='1: NO AXES '
	text(2)='2: AXES ONLY'
	text(3)='3: GRID ONLY'
	text(4)='4: AXES + GRID'
	text(5)='5: ALL NUMBERS'
	text(6)='6: NO LOW+HIGH'
	text(7)='7: NO LOWEST NR'
	text(8)='8: NO HIGHEST NR'
	text(10)='0: REDRAW'
	text(11)='+: EXPONENT'
	text(12)='-: NUMERIC'
	Lb(9)=0
	do i=13,15
		Lb(i)=0
	enddo
	call broken(0)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
1452	call CKEY(ch,kkey)
	if(kkey.lt.-2.or.kkey.gt.13.or.kkey.eq.9) goto 1452
	if(kkey.eq.-1) then	!F1 key
	   call VHELP(17)
	   goto 1452		!another menu choice
	else if(kkey.eq.-2) then	!F2=help index
	   call VHELP(-3)
	   goto 1452		!another menu choice
	endif
	call DCMENU(-kkey,5,Lb,text,0,0)	!delete box 1 only
	Lb(kkey)=-1
	call DCMENU(-kkey,5,Lb,text,icol0,icol2)	!draw box 1 only (italic)
	if(kkey.eq.1) then
	   istyle(ind)=0
	else if(kkey.eq.2) then
	   istyle(ind)=1
	else if(kkey.eq.3) then
	   istyle(ind)=2
	else if(kkey.eq.4) then
	   istyle(ind)=3
	else if(kkey.eq.5) then
	   isup(ind)=0
	else if(kkey.eq.6) then
	   isup(ind)=3
	else if(kkey.eq.7) then
	   isup(ind)=1
	else if(kkey.eq.8) then
	   isup(ind)=2
	else if(kkey.eq.12) then
	   call DEFOLTi(nwidth,defolt)
	   call QDIALOG(1,'field width of the number',defolt,ict,cans)
	   call GETINPi(cans,nwidth)
	   call DEFOLTi(nplace,defolt)
	   call QDIALOG(1,'decimal places',defolt,ict,cans)
	   call GETINPi(cans,nplace)
	   npow=1
	else if(kkey.eq.11) then
		call DEFOLTi(npow,defolt)
		call QDIALOG(1,'power',defolt,ict,cans)
		call GETINPi(cans,npow)
	else if(kkey.eq.10) then
	      call FILLWIN(0,55,639,387,icol(71))		!graph area
		goto 100
	endif
	goto 1452

c========================================================================
c                           POSH OPTIONS
c========================================================================
5	continue
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,0)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,5,Lb,text,0,0)		!delete all
351	continue
	Lb(2)=1
	Lb(10)=1
	Lb(11)=1
	Lb(12)=1
	text(2)  ='2: FIX TEXT'
	text(10) ='0: REDRAW'
	text(11)='+: FILL OPTIONS'
	if(fill.and..not.mark) then
	   text(12)=' -: MARK BAD REGION'
	else if(fill.and.mark) then
	   text(12)=' -: NO MARKING'
	else
	   Lb(12) = 0
	endif
	call broken(0)
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
352	call CKEY(ch,jkey)
	if(jkey.eq.10) then
	      call FILLWIN(0,55,639,387,icol(71))		!graph area
		goto 100
	endif
	if(jkey.lt.-2) goto 351
	if(jkey.ne.2.and.jkey.ne.10.and.jkey.ne.11.and.jkey.ne.12) then
	   goto 351
	endif
	if(jkey.eq.-1) then	!F1 key
	   call VHELP(20)
	   goto 352		!another menu choice
	else if(jkey.eq.-2) then	!F2=help index
	   call VHELP(-3)
	   goto 352		!another menu choice
	endif
	if(jkey.eq.11) then
	   goto 1114
	else if(jkey.eq.12) then
	   if(.not.mark) then
		mark=.true.
	   else
		mark=.false.
	   endif
 	   call FILLWIN(0,55,639,387,icol(71))		!graph area
	   goto 100	!redraw
	endif
c========================================================================
C		FIX TEXT
c========================================================================
601 	CONTINUE 	!get here if jkey=2
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,5,Lb,text,0,0)		!delete all
661	continue
	text(1) ='1: TITLE'
	text(2) ='2: X LABEL'
	text(3) ='3: Y LABEL'
	text(4) ='4: Z LABEL'
	text(10) ='0: REDRAW'
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
	if(ikey.eq.10) then
	   call FILLWIN(0,55,639,387,icol(71))		!graph area
	   goto 100
	endif
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
	text(10) =' 0: REDRAW'
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
	monsav=mono			!keep value before plot
	do i=1,100
	   icolsav(i)=icol(i)
	enddo
	landplot=.true.		!landscape is default
	nxlo=8
	nylo=200
	nyhi=-1
	TITLEs     ='   PLOT OPTIONS    '
	strings(1)='1. Black&white postscript '
	strings(2)='2. Black&white laserjet '
	strings(3)='3. Color deskjet'
	strings(4)='4. Metafile color (*.cgm)'
	strings(5)='5. metafile Grey shades (*.cgm)'
	strings(6)='6. Windows metafile (*.wmf)'
	strings(7)='7. Ascii File   '
	strings(8)='8. End          '
	nval=8

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
	   ihelp=8
	endif
4444   continue
	if(ihelp.eq.1.or.ihelp.eq.2.or.ihelp.eq.3) then
	   plot=.true.
	   if(ihelp.eq.3) then
		idev=6
         else if(ihelp.eq.1.or.ihelp.eq.2) then
		icol(25)=8
		icol(23)=8
		icol(71)=8
		icol(22)=8
	      icol(33)=3
	      icol(34)=2
		icol(35)=icol(33)
		icol(36)=icol(34)
	      icol(31)=7
	      icol(32)=7
	      icol(21)=8
		kdev=-2
	   endif
	else if(ihelp.eq.4.or.ihelp.eq.5) then
	   meta=.true.
	   idev=2
	   kdev=-2
	   if(ihelp.eq.5) then
		do i=1,100
		   icol(i)=0
		enddo
		icol(22)=0
	      icol(33)=7
	      icol(34)=8
		icol(35)=icol(33)
		icol(36)=icol(34)
	      icol(31)=0
	      icol(32)=0
	      icol(21)=0
 	   endif
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
	   wmeta=.true.
	   plot=.false.
	   mono=.false.
	   idev=1
	   kdev=-2
	   wmetafil='plot.wmf'
	   CALL WDIALOG(1,'OPTIONS FOR FILES NAMES ',ict)
7151	   continue
	   nc=nblank1(wmetafil)
	   call qdialog(1,'Name for wmetafile ',
     &   ' ['//wmetafil(1:nc-4)//'] = ',ict,cans)
	   if(cans.eq.' ') goto 6141
	   nc1=nblank1(cans)
	   if(nc1.le.20) then
		wmetafil=cans(1:nc1)//'.wmf'
		goto 6141
	   endif
	   call wdialog(1,'YOU ENTERED AN INVALID FILE NAME',ict)
	   goto 7151
6141	   continue
	else if(ihelp.eq.7) then
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
	      write(unit=14,fmt=892) I,J,K,raX(k),raY(k),GRIDZ(I,J)
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
	else if(ihelp.eq.8) then
	   plot=.false.
	   meta=.false.
	   wmeta=.false.
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
	   wmeta=.false.
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
c	   call rgbdef(0,0.0,0.0,0.0)
	   call errswi(-1)
	   call brkswi(1)
	   call gsetcols(0)
	   call mode(18)
	   call WDIALOG(1,'Metafile in progress . . .',12)
	else if(wmeta) then
	   call WDIALOG(1,
     &   'Assembling the wmetafile. . .',ict)
	   call devend
	   wMETA=.TRUE.
c###5/05/99
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
	   call mode(18)
	   call WDIALOG(1,'Wmetafile in progress . . .',12)
	else if (plot) then
	   if(ihelp.eq.2) then
	   	oldfile='HPLJR.OUT'
		if(landplot) then
c=			a=2.
			XPP=280.5
			YPP=196.7
		else
c			a=1.4
			yPP=280.5
			xPP=196.7
			b=xp
			xp=yp
			yp=b
		endif
	   else if(ihelp.eq.3) then
	   	oldfile='DJ500C.OUT'
		if(.not.landplot) then
c			a=1.4
			XPP=203.2
			YPP=266.7
			b=xp
			xp=yp
			yp=b
		else
c			a=2.
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
c			a=1.4
		else
c			a=2.
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
	      wmeta=.false.
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
c	   if(ihelp.eq.3.and.landplot) call devpap(297.,210.,itype)
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
	goto 9876
5558  continue
	call WDIALOG(1,'Windows metafile done. . .',12)
	CALL DEVEND
	wmeta=.false.
c==========================================
9876	continue
      do i=1,100
         icol(i)=icolsav(i)
	enddo
	call VGA
	call gsetcols(0)
	call errswi(-1)
	call brkswi(1)
	call chaswi(1)
	call grfmod (1)
	call harcha
	call mode(18)
	idev=0
	after=.true.
	goto 1002

c====================================================================================
c After plot finished:
1812	continue
	call DEVEND
	call SYSTEM('copy/b '//charnb(oldfile)//' lpt1')	!see autplot
c=	call COPY(oldfile,'lpt1')
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
	   else if(ihelp.eq.2 ) then
	   	printfil='lfit'//kpchar(1:nc1)//'.out'
	   else if(ihelp.eq.3) then
	   	printfil='cfit'//kpchar(1:nc1)//'.out'
	   endif
	   call COPY(oldfile,printfil)
	endif
c 	To eject page without picking up another, send 'Esc E' to laserjet (OK for
c     deskjet too?).  If this is NOT done then another plot can be put on same page.
c	write(7,*) char(27)//'E'
      call FLUSH(7)

1814	continue
 	do i=1,100
 	   icol(i)=icolsav(i)
 	enddo
      call VGA
	call errswi(-1)
	call brkswi(1)
	call chaswi(1)
	call gsetcols(0)
	call grfmod (1)
	call harcha
	call mode(18)
	idev=0
	plot=.false.
	call setfnt(ifont)
	mono=monsav		!restore
	vxlo=vxlo1		! for VIEWPORT
	vxhi=vxhi1
	vylo=vylo1
	vyhi=vyhi1
1043	continue
	after=.true.
	goto 1002


c======================================================================
c				Queue plot
c========================================================================
8  	CONTINUE
	iptype=42
	call gplotq(iptype,xval1,yval1,z1,bad,nx,ny,ndx,ndy,numx,numy,
     &     xtitle,ytitle,ztitle,title1,idrawt,idrawx,idrawy,idrawz,
     &     xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,
     &     ifnt,ifnl,alfa,beta,gama,delta,ijust,ijusx,ijusy,ijusz,
     &     xmin,xmax,ymin,ymax,zmin,zmax,xtic,ytic,ztic,
     &     ratio,rat,radius,theta,phi,ifram,numaxi,nwidth,npow,nplace,
     &     nxstep,nystep,istyle,isup,
     &     fill,inter,axes,fillbad,cross,kcol,posneg,qfile)

c	call FILLWIN(0,55,639,387,icol(71))		!graph area
	goto 152

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
c========================================================================
c		MORE OPTIONS
c========================================================================
15	continue
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,5,Lb,text,0,0)		!delete all
	Lb(14)=0
	Lb(15)=0
	if(mono) then
	   text(1)='1: COLOUR DISPLAY'
	else
	   text(1)='1: MONOCHROME  '
	endif
	text(2)='2: SCREEN COLOURS'
	text(3)='3: PLOT COLOURS'
	text(4)='4: COLOURS: MANUAL'
	text(5)='5: THICKER LINES '
	text(6)='6: THINNER LINES '
	text(7)='7: LINE THICKNESS'
	text(8)='8: SET TEXT SIZE'
	text(9)='9: SET FONT'
	text(10)='0: REDRAW    '
	do i=11,15
	   Lb(i)=0
	enddo
	call DCMENU(nbox,5,Lb,text,icol1,icol2)
3541	call CKEY(ch,ikey)
	if(ikey.eq.-16) then !main menu
 	   call FILLWIN(0,55,639,387,icol(71))		!graph area
	   goto 100
	endif
	if(ikey.eq.-1) then	!F1 key
	   call VHELP(15)
	   goto 15		!another menu choice
	else if(ikey.eq.-2) then	!F2=help index
	   call VHELP(-3)
	   goto 15		!another menu choice
	endif
	call DCMENU(-ikey,5,Lb,text,0,0)	!delete box #ikey1 only
	Lb(ikey)=-1
	call DCMENU(-ikey,5,Lb,text,icol0,icol2)	!draw box #ikey only (italic)
141	if(ikey.eq.1) then
	   mono=.not.mono
 	   do i=1,100
 	      icolsav(i)=icol(i)
 	   enddo
	   if(mono) then
	      call SETCLs(mono,icol,.false.,plotcols,isetcol)
	   else
	      call SETCLs(mono,icol,.false.,plotcols,isetcol)
	   endif
	else if(ikey.eq.2) then
c       set default colours even if AUTPLOT for now
	   mono=.false.
	   plotcols=.false.
	   call SETCLs(mono,icol,.false.,plotcols,isetcol)
	else if(ikey.eq.3) then
	   mono=.false.
	   plotcols=.true.
	   call SETCLS(mono,icol,.false.,plotcols,isetcol)
	else if(ikey.eq.4) then
	   nbox=15	!number of boxes for DCMENU
	   call SETLBOX(nbox,Lb,1)
	   call NUMSET			!set num lock on ready for response
	   call DCMENU(0,5,Lb,text,0,0)		!delete all
1714     continue
	   text(1) =' 1 : COLOUR FILL'
	   text(2) =' 2 : COLOUR NET'
	   text(3) =' 3 : COLOUR AXES'
	   text(4) =' 4 : COLOUR TEXT'
	   text(5) =' 5 : COLOUR POS/NEG'
	   text(6) =' 6 : COLOUR BAD'
	   text(10) =' 0: REDRAW'
	   call SETLBOX(nbox,Lb,1)
	   do i=7,9
	     Lb(i)=0
	   enddo
	   do i=11,15
	     Lb(i)=0
	   enddo
	   call broken(0)
	   call DCMENU(nbox,5,Lb,text,icol1,icol2)
1724	   call CKEY(ch,kkey)
	   if(kkey.lt.-2.or.ikey.gt.15) goto 1514
	   if(kkey.eq.-1) then	!F1 key
	      call VHELP(15)
	      goto 1724		!another menu choice
	   else if(kkey.eq.-2) then	!F2=help index
	      call VHELP(-3)
	      goto 1724		!another menu choice
	   endif
	   call DCMENU(-ikey,5,Lb,text,0,0)	!delete box 1 only
	   Lb(ikey)=-1
	   call DCMENU(-ikey,5,Lb,text,icol0,icol2)	!draw box 1 only (italic)
	   if(kkey.eq.1) then
	   	call DEFOLTi(icol(33),defolt)
	   	call QDIALOG(1,'Colour fill upper surface',defolt,ict,cans)
         	call GETINPi(cans,icol(33))
	   	call DEFOLTi(icol(34),defolt)
	   	call QDIALOG(1,'Colour fill lower surface',defolt,ict,cans)
         	call GETINPi(cans,icol(34))
	   else if(kkey.eq.2) then
	   	call DEFOLTi(icol(31),defolt)
	   	call QDIALOG(1,'Colour net upper surface',defolt,ict,cans)
         	call GETINPi(cans,icol(31))
	   	call DEFOLTi(icol(32),defolt)
	   	call QDIALOG(1,'Colour net lower surface',defolt,ict,cans)
         	call GETINPi(cans,icol(32))
	   else if(kkey.eq.3) then
	   	call DEFOLTi(icol(21),defolt)
	   	call QDIALOG(1,'Colour axes',defolt,ict,cans)
         	call GETINPi(cans,icol(21))
	   else if(kkey.eq.4) then
	      call DEFOLTi(icol(25),defolt)
	      call QDIALOG(1,'Colour title ',defolt,ict,cans)
            call GETINPi(cans,icol(25))
	      call DEFOLTi(icol(23),defolt)
	      call QDIALOG(1,'Colour labels',defolt,ict,cans)
            call GETINPi(cans,icol(23))
	   else if(kkey.eq.5) then
	      call DEFOLTi(icol(35),defolt)
	      call QDIALOG(1,'Colour positive values ',defolt,ict,cans)
            call GETINPi(cans,icol(35))
	      call DEFOLTi(icol(36),defolt)
	      call QDIALOG(1,'Colour negative values ',defolt,ict,cans)
            call GETINPi(cans,icol(36))
	      call DEFOLTi(icol(37),defolt)
	      call QDIALOG(1,'Colour positive values low',defolt,ict,cans)
            call GETINPi(cans,icol(37))
	      call DEFOLTi(icol(38),defolt)
	      call QDIALOG(1,'Colour negative values low',defolt,ict,cans)
            call GETINPi(cans,icol(38))
	   else if(kkey.eq.6) then
	      call DEFOLTi(icol(22),defolt)
	      call QDIALOG(1,'Colour bad region',defolt,ict,cans)
            call GETINPi(cans,icol(22))
	   else if(kkey.eq.10) then
 	      call FILLWIN(0,55,639,387,icol(71))		!graph area
	      goto 100	!redraw
	    endif
	    goto 1724
	else if(ikey.eq.5) then
	   width=width+0.5*width
	else if(ikey.eq.6) then
	   width=width-0.5*width
	else if(ikey.eq.7) then
 	   call DEFOLTr(width,defolt)
	   call QDIALOG(1,
     &	'Line thickness',
     &	defolt,ict,cans)
	   call GETINPr(cans,width)
	else if(ikey.eq.8) then
	   csfac=1.
 	   call DEFOLTr(csfac,defolt)
	   call QDIALOG(1,
     &	'Size factor for all text',
     &	defolt,ict,cans)
	   call GETINPr(cans,csfac)
	   csize=csfac*csize
	   sizel=1.7*csize
	   sizet=2.5*csize
	else if(ikey.eq.9) then
 	   call DEFOLTi(ifnt,defolt)
	   call QDIALOG(1,
     &	'Font number for all text',
     &	defolt,ict,cans)
	   call GETINPi(cans,ifnt)
	   ifnl=ifnt
	else if(ikey.eq.10) then
 	   call FILLWIN(0,55,639,387,icol(71))		!graph area
	   goto 100	!redraw
	endif
	goto 3541		!another option

c========================================================================
c                           FILL
c========================================================================
1114	continue
	nbox=15	!number of boxes for DCMENU
	call SETLBOX(nbox,Lb,1)
	call NUMSET			!set num lock on ready for response
	call DCMENU(0,5,Lb,text,0,0)		!delete all
1514  continue
	call SETLBOX(nbox,Lb,1)
	text(1) ='1: ONE COLOUR'
	text(2) ='2: UPPER/LOWER'
	text(3) ='3: POS/NEG, 2 COLS'
	text(4) ='4: POS/NEG, 4 COLS'
	text(5) ='5: FILL CONTOURS  '
	if(fill) then
	   text(6) ='6: NO FILL '
	else
	   Lb(6) = 0
	endif
	text(10) ='0: REDRAW'
	do i=7,9
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
	   call VHELP(21)
	   goto 1524		!another menu choice
	else if(ikey.eq.-2) then	!F2=help index
	   call VHELP(-3)
	   goto 1524		!another menu choice
	endif
	call DCMENU(-ikey,5,Lb,text,0,0)	!delete box 1 only
	Lb(ikey)=-1
	call DCMENU(-ikey,5,Lb,text,icol0,icol2)	!draw box 1 only (italic)
      if(ikey.eq.1) then
	   kcol=1
	   ncont=kcol
	   posneg=.false.
	   fill=.true.
	   goto 1524
      else if(ikey.eq.2) then
	   kcol=2
	   posneg=.false.
	   ncont=kcol
	   fill=.true.
	   goto 1524
      else if(ikey.eq.3) then
	   kcol=2
	   ncont=kcol
	   posneg=.true.
	   fill=.true.
	   goto 1524
      else if(ikey.eq.4) then
	   kcol=4
	   posneg=.true.
	   ncont=kcol
	   fill=.true.
	   goto 1524
	else if(ikey.eq.5) then
	   call DEFOLTi(ncont,defolt)
	   call QDIALOG(1,'How many',defolt,ict,cans)
         call GETINPi(cans,ncont)
	   if(ncont.lt.1) ncont=1
	   if(ncont.gt.15) ncont=15
	   posneg=.false.
	   kcol=-ncont
	   fill=.true.
	   goto 1524
      else if(ikey.eq.6) then
	   FILL=.FALSE.
	   if(allocated(ifillg)) then
		deALLOCATE(ifillg)
	   endif
	   if(allocated(ifillz)) then
		deALLOCATE(ifillz)
	   endif
 	   call FILLWIN(0,55,639,387,icol(71))	!graph area
	   goto 100
      else if(ikey.eq.10) then
 	   call FILLWIN(0,55,639,387,icol(71))		!graph area
	   goto 100
	endif

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
	if(allocated(badg)) then
	   DEALLOCATE(badg)
	endif
	vxlo=0.	! for VIEWPORT
	vxhi=xp
	vylo=0.
	vyhi=yp

c	CLOSE(unit=12)
	call mode(3)
c
	nx=nxsav
	ny=nysav
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

	subroutine SETBAD(bad,icone,numx,numy,ndx,ndy,ifill,nfx,nfy,kdev)
	integer ifill(nfx,nfy)
	logical bad(ndx,ndy)
c To define tiles to fill, in order to mark values that are set 'bad'
c NB ifill(i,j) refers to tile which is above/right of z(i,j).
	icbad=icone
	call lincols(icbad,kdev)
	do i=1,numx
	   do j=1,numy
		if(bad(i,j)) then
		   if(i.le.nfx) then
			if(j.le.nfy) ifill(i,j)=icbad
			if(j.gt.1) ifill(i,j-1)=icbad
		   endif
		   if(i.gt.1.and.j.le.nfy) ifill(i-1,j)=icbad
		   if(j.gt.1.and.i.gt.1) ifill(i-1,j-1)=icbad
		endif
	   enddo
	enddo
	RETURN
	end

