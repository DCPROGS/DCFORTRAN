c======================================================================
	subroutine gauto3d(iplotype,igraph,xval1,yval1,z1,bad,nx,ny,ndx,ndy,
     &	xtitle,ytitle,ztitle,title1,idrawt,idrawx,idrawy,idrawz,
     &    quarter,idev,plot,iplot,kcol,posneg,isetcol,icol,
     &	wxmin,wxmax,wymin,wymax,vxlop,vxhip,vylop,vyhip,
     &    xlop,xhip,ylop,yhip,main,ix,iy,graph1_1,
     &    GraphMainPanel1_1,graphics1_1,ipos,gfile,ixposv,iyposv,
     &	oldrecords,nplot,ifnt,ifnl,alfa,beta,gama,delta,
     &     ijust,ijusx,ijusy,ijusz,
     &     xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,numx,numy,
     &     xmin,xmax,ymin,ymax,zmin,zmax,xtic,ytic,ztic,
     &     ratio,rat,radius,theta,phi,ifram,numaxi,nwidth,npow,nplace,
     &     nxstep,nystep,istyle,isup,
     &     fill,inter,axis,fillbad,autplt,cross,jopen)
c======================================================================
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

	use menu_f90
	use gino_f90
	use surf_f90
	use hjcrecords
	integer jopen(200)
	real*4 z1(ndx,ndy)
	real*4 xval1(ndx),yval1(ndy)
	logical bad(ndx,ndy)
c
	allocatable::gridz,az,ax,ay,w,IFILLZ,IFILLG
	real*4 gridz(:,:),az(:),ax(:),ay(:),w(:)
	allocatable::z,xval3,yval3
	real*4 z(:,:),xval3(:),yval3(:)
 	INTEGER*4 IFILLZ(:,:),IFILLG(:,:)
	allocatable::rax,ray
	real*4 rax(:),ray(:),xrp(4),yrp(4)
	allocatable::badg
	logical badg(:,:),mark
	logical autplt
      character qfile*40
	logical fillbad
	integer*4 icol(100)
	character*75 xtitle,ytitle,ztitle
	character*75 title1
	logical all,cross,after,posneg,monsav
	logical mono,plot,fill,inter,present,landplot,axis
	logical quarter
      INTEGER ISTYLE(6),ISUP(6)
      

	integer :: Main
	integer :: Graph1_1(100)
	integer :: GraphMainPanel1_1(100)
	integer :: Graphics1_1(100)
	character :: cnumb*11, title_graph*60, gfile*60


	type (GLIMIT) :: Graphics_frame
		
	

	TYPE (RECORD_ATTRIBUTES) oldrecords(25)
	common/pixpos/ixgrid,iygrid,ixpix,iypix,wid,landscape
c	common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
c     & xlo,xhi,ylo,yhi

	common/cars/dxsa,dysa,graph
	logical usedef,landscape,exass,dsmooth,shbiv,shdep,shdif,shfit
	common/def/usedef,exass,dsmooth,shbiv,shdep,shdif,shfit		!for qdialog
	
	modplot=igraph
	qfile="C:\PLOTQ.DAT"
	ihtype=3	!tells VHELP that call was from GPLOT3D
c Set things that re not to be changed after rescale
!	idev=0
!	if(plot) idev=6
	ncont=abs(kcol)
	dxsa=1
	dysa=1
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
	if(fillbad) mark=.true.
	kdev=-1
c      CALL PICCLE
	CALL PAPENQ(XPAP,YPAP,IPAPTY)
	vxlo=0.	! for VIEWPORT
	vxhi=1080.
	vylo=0.
	vyhi=810.
c	vxlo=0.	! for VIEWPORT
c	vxhi=20.
c	vylo=0.
c	vyhi=20.
	xlo=0.2*vxhi
	xhi=0.8*vxhi
	ylo=0.2*vyhi
	yhi=0.8*vyhi
	vxlop=vxlo	! for VIEWPORT
	vxhip=vxhi
	vylop=vylo
	vyhip=vyhi
	xlop=0.2*vxhi
	xhip=0.8*vxhi
	ylop=0.2*vyhi
	yhip=0.8*vyhi
	xlo1=xlo
	xhi1=xhi
	ylo1=ylo
	yhi1=yhi
	if (quarter) then
	   x1=xlop-0.06*xpap
	   x2=xhip+0.06*xpap
	   y1=ylop-0.06*ypap
	   y2=yhip+0.06*ypap
	else
	   x1=vxlop
	   x2=vxhip
	   y1=vylop
	   y2=vyhip
	endif
	xrp(1)=x1
	yrp(1)=y1
	xrp(2)=x2
	yrp(2)=y1
	xrp(3)=x2
	yrp(3)=y2
	xrp(4)=x1
	yrp(4)=y2
	XWMIN=100.
	XWMAX=1050.
	YWMIN=50.
	YWMAX=620.
	icf=1
	icf2=1
	ict=11
	icb2=0
	icb2=0	!background colour for dialog box 1,2
	icf=7		!frame colour for dialog box 1
	icf2=12	!frame colour for dialog box 2
	ict=11	!text colour for dialog box 1
c====DC test different colour orders
c	ncont=14		!default number of colour bands
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
	allocate(z(nx,ny),xval3(nx),yval3(ny))
	   do i=1,nx
	      xval3(i)=xval1(i)		!copy input
	      do j=1,ny
		   z(i,j)=z1(i,j)		!copy input
	         yval3(j)=yval1(j)		!copy input
		enddo
	   enddo
	   iperf=-100
	   ncont=abs(kcol)
	   if(fillbad) mark=.true.
	if(autplt) goto 1002
	numx=nx
	numy=ny
	xmin=1.e36
	xmax=-1.e36
	ymin=1.e36
	ymax=-1.e36
	zmin=1.e36
	zmax=-1.e36
	do i=1,nx
	   xval3(i)=xval1(i)		!copy input
	   if(xval3(i).gt.xmax) xmax=xval3(i)
	   if(xval3(i).lt.xmin) xmin=xval3(i)
	   do j=1,ny
		z(i,j)=z1(i,j)		!copy input
		if(z(i,j).gt.zmax) zmax=z(i,j)
		if(z(i,j).lt.zmin) zmin=z(i,j)
	   enddo
	enddo
	do j=1,ny
	   yval3(j)=yval1(j)		!copy input
	   if(yval3(j).gt.ymax) ymax=yval3(j)
	   if(yval3(j).lt.ymin) ymin=yval3(j)
	enddo
	ztic=(zmax-zmin)/10.
	xtic=(xmax-xmin)/10.
	ytic=(ymax-ymin)/10.
c
c Set fillbad=true initially if there are any bad values
	fillbad=.false.
	icol(22)=48
	do i=1,numx
	   do j=1,numy
		if(bad(i,j)) fillbad=.true.
	   enddo
	enddo
	if(fillbad) mark=.true.
c
c	dx0=xval3(2)-xval3(1)	!grid spacing for input
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
c      RADIUS=500.0
	radius=250.
      THETA=30.0
      PHI=30.0
	IFRAM=3
	if(kcol.eq.0) then
		FILL=.FALSE.
	else
		FILL=.true.
	endif
	INTER=.FALSE.
	AXiS=.TRUE.
	NUMAXI=0
	NWIDTH=5
	NPOW=0
	NPLACE=1
	nxstep=1
	nystep=1

	mono=.false.
	plotcols=.true.
c===	call set_colours(mono,icol,autplt,plot,isetcol)
c	call SETCLS(mono,icol,autplt,plotcols,isetcol)
c
	kcol=2
	do i=1,6
	   istyle(i)=3
	   isup(i)=0
	enddo
	xst=(vxhi-vxlo)/2.
	yst=0.8*vyhi
	xspx=0.6*vxhi
	yspx=0.1*vyhi
	xspy=0.3*vxhi
	yspy=0.3*vyhi
	xspz=0.1*vxhi
	yspz=0.5*vyhi
	xspx=xst
	yspx=yspx+40.
	xspy=xspy-40.
	yspy=yspy-20.
1002	continue

c
c	PLOT GRAPH
c
 	if(quarter) then
 		xlop1=xlop-0.10*xpap
 		xhip1=xhip+0.07*xpap
 		ylop1=ylop-0.06*ypap
		yhip1=yhip+0.11*ypap
 	else
 		xlop1=vxlop
 		xhip1=vxhip
 		ylop1=vylop
		yhip1=vyhip
	
	endif

	if (idev.eq.3.or.idev.eq.4.or.idev.eq.5) then
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
	icw=15
	x20=x2-0.13*xpap
	if(idev.eq.6) then
         icw=0
	   x20=x2-0.17*xpap
	endif
c	call WRITQNUM(iplot,x20,y2-0.03*ypap,icw,idev)	!write plot queue # on screen/plot
1003	continue
c===	call errdev(10)

	call graph1(igraph,iplot,main,ix,iy,graph1_1,
     &	GraphMainPanel1_1,graphics1_1,
     & vxlo-20,vxhi,vylo-20,vyhi,ipos,gfile,plot,ixposv,iyposv)
c       CALL PAPENQ(XPAP,YPAP,IPAPTY)
c      	CALL SETVP2(vxlo-20.,vxhi,vylo-20.,vyhi,0.,xpap,0.,ypap)
	CALL SURF		!Surf banner
c===	call errdev(10)
	
      
	ymenu=vyhi/480.*53.
c	xp=xpap
c	yp=ypap

	csize=vyhi/80. !copy for common/dmenu/
	csizeb=csize
	sizet=csize*2.5
	sizel=csize*1.7
	width=1.0
c=====================================================================
100   continue
	call linwid(width)
	kdev=0
	if(idev.eq.0) then
    
        kdev=0
	
      else
        kdev=-idev
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
		ax(k)=xval3(i1)
		ay(k)=yval3(j1)
		az(k)=z(i1,j1)
		k=k+1
	   enddo
	enddo

C
C  INITIALISE VARIABLES
C
      xlow=xval3(1)
      xhigh=xval3(nx)
	dx=xval3(2)-xval3(1)
      ylow=yval3(1)
      yhigh=yval3(ny)
	dy=yval3(2)-yval3(1)
c
	np=nx*ny
C
C  MINIMUM WORKSPACE SIZES ARE :
C
      nw=max(2*numx*numy+4*np,numx*numy+20*max(numx,numy))
	nw=3*nw
	if(allocated(w)) DEALLOCATE(w)
	allocate(w(nw))
	if(allocated(gridz)) then
	   DEALLOCATE(gridz)
	endif
	allocate(gridz(numx,numy))
	call glincols(1,idev) !===
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
c=	bzw=(zgmax-zgmin)/float(ncont)
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
                  call glincols(ncolone,kdev)
                  ifillg(i,j)=ncolone
		   else if(kcol.eq.2) then
			if(posneg) then
		   	   zv=gridz(i,j)
		         if(zv.le.0.0) then
		   	      ncoltwo=icol(36)
		            call glincols(ncoltwo,kdev)
		      	ifillz(i,j)=ncoltwo
		      	ifillg(i,j)=ncoltwo
		   	   else
		      	ncoltwo=icol(35)
		      	call glincols(ncoltwo,kdev)
		      	ifillz(i,j)=ncoltwo
		      	ifillg(i,j)=ncoltwo
		         endif
			else
                     ncolone=icol(33)
                     call glincols(ncolone,kdev)
                     ifillg(i,j)=ncolone
		         ncolone=icol(34)
		         call glincols(ncolone,kdev)
		         ifillz(i,j)=ncolone
			endif
		   endif
		else if(kcol.eq.4) then
		   	   zv=gridz(i,j)
		         if(zv.le.0.0) then
		   	   	ncoltwo=icol(38)
		            call glincols(ncoltwo,kdev)
		      	ifillz(i,j)=ncoltwo
		   	      ncoltwo=icol(36)
		            call glincols(ncoltwo,kdev)
		      	ifillg(i,j)=ncoltwo
		   	   else
		      	ncoltwo=icol(37)
		      	call glincols(ncoltwo,kdev)
		      	ifillz(i,j)=ncoltwo
		   	      ncoltwo=icol(35)
		            call glincols(ncoltwo,kdev)
		      	ifillg(i,j)=ncoltwo
		         endif
		else if(kcol.lt.0) then
		   do k=1,ncont
		      blo=gzmin+float(k-1)*bw
		      bhi=gzmin+float(k)*bw
		      zv=gridz(i,j)
		      if(zv.ge.blo.and.zv.le.bhi) then
			   ncolm=icol(k)
		         call glincols(ncolm,kdev)
			   ifillg(i,j)=ncolm !avoid 1
		         ifillz(i,j)=ncolm+8
		      endif
		   enddo
		endif
	   enddo
      enddo
c	axes +graph	
	call setfnt(ifnl)
      CALL CHASIZ(csize,csize)
	if(cross) goto 444
 	if(all) then
 		CALL PRJBND(XLO1,XHI1,YLO1,YHI1)
 	else
 		CALL PRJBND(XLO,XHI,YLO,YHI)
 	endif
	if(cross) call linvis(0)
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
	call glincols(ngonu,kdev)
	call glincols(ngond,kdev)
	call glincols(ncolax,kdev)
	if(plot) then
      	CALL PRJLIN(ncolax,ngonu,ngond)
	else
      	CALL PRJLIN(ncolax,ngonu,ngond)
	endif
	CALL WINDOW(2)
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
c	iperf=9
	if(axis) then
	   call ANNDIG(NUMAXI,NWIDTH,NPLACE,NPOW)
	   call ANNSTR(1,'            ')
	   call ANNSTR(2,'            ')
     	   call ANNSTR(3,'            ')
	   call glincols(12,idev)!!!
c	   if(fill.and.(kcol.eq.2.or.kcol.eq.4)) goto 6363
	   CALL PERAXE(NW,W)		!axes displayed
6363	   if(iperf.ne.-100) then
	      call perspa((xmin+xmax)/2,ymin-4*dy,zmin,xspx,yspx)
	      call perspa(xmin-4*dx,(ymin+ymax)/2,zmin,xspy,yspy)
	      call perspa(xmax+3*dx,ymin-3*dy,(zmin+zmax)/2,xspz,yspz)
	   endif
	   call glincols(icol(23),idev)
	   call linwid(1.0)
	   call setfnt(ifnl)
c	size1=4
c	sizet=4
	   if(idrawx.eq.1) then
	   call JUSTIFYSTRING(xspx,yspx,xtitle,beta,
     &      sizel,ijusx)
		else
cx		call glincols(48,idev)
		call JUSTIFYSTRING(xspx+20,yspx-30,'x axis',beta,
     &      sizel,ijusx)
	   endif
	   if(idrawy.eq.1) then
	   call JUSTIFYSTRING(xspy-10,yspy,ytitle,gama,
     &      sizel,ijusy)
		else
			call JUSTIFYSTRING(xspy+10,yspy-30,'y axis',gama,
     &      sizel,ijusy)
		endif
	   if(idrawz.eq.1) call JUSTIFYSTRING(xspz,yspz,ztitle,delta,
     &	sizel,ijusz)
	

	endif
c	ADD TITLE
	call glincols(ICOL(25),idev)
	call setfnt(ifnt)
	if(iperf.ne.-100) call perspa(xmax,ymax,1.2*zmax,xst,yst)
	if(idrawt.eq.1) call JUSTIFYSTRING(xst,yst,title1,alfa,
     &	sizet,ijust)
	goto 999
c==============================================================
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
c=============================================================

c TIDY UP AND EXIT
999	continue
	call gFlushGraphics() 
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
	nx=nxsav
	ny=nysav
c===================
	DEALLOCATE(z,xval3,yval3)
c===================
c
c===	call errdev(10)
	call surend
	CALL guiwin
	call gsethardchars()
	call gsetbrokenlinemode(gon)
	
	call gSetCharFont(ifont0)
	call gSetLineEnd(GROUND)
	call gSetEscapeChar('£')
	call define_colours(1,.true.)
	call gseterrormode(gerroron)
c===	call errdev(10)
C      CALL SETVP2(vxlop,vxhip,vylop,vyhip,vxlop,vxhip,vylop,vyhip)	!screen->grey
	jopen(modplot)=1
	do i=1,100
        oldrecords(modplot)%attributes%ICOL(i)=iCOL(i)
      enddo
      oldrecords(modplot)%attributes%ICOL(100)=kcol
	oldrecords(modplot)%iplot=iplot
	oldrecords(modplot)%iplotype=iplotype
      
	oldrecords(modplot)%STR(1)=title1

	oldrecords(modplot)%STR(3)=xtitle
	oldrecords(modplot)%STR(4)=ytitle
	oldrecords(modplot)%STR(5)=ztitle
	oldrecords(modplot)%xmin=xmin
	oldrecords(modplot)%ymin=ymin
	oldrecords(modplot)%ymax=ymax
	oldrecords(modplot)%xmax=xmax
	oldrecords(modplot)%Wxmin=Wxmin
	oldrecords(modplot)%wymin=wymin
	oldrecords(modplot)%wymax=wymax
	oldrecords(modplot)%wxmax=wxmax
	
	oldrecords(modplot)%attributes%IFNT(1)=ifnt
	oldrecords(modplot)%attributes%IJUS(1)=ijust
	oldrecords(modplot)%attributes%angle(1)= alfa
	oldrecords(modplot)%attributes%Idraw(1)=idrawt
	oldrecords(modplot)%attributes%ICOL(1)=iCOL(25)
	oldrecords(modplot)%attributes%rx(1)=xst
			oldrecords(modplot)%attributes%ry(1)=yst

	oldrecords(modplot)%attributes%IFNT(3)=ifnl
	oldrecords(modplot)%attributes%IJUS(3)=ijusx
	oldrecords(modplot)%attributes%angle(3)= beta
	oldrecords(modplot)%attributes%Idraw(3)=idrawx
	oldrecords(modplot)%attributes%ICOL(3)=iCOL(23)
	oldrecords(modplot)%attributes%rx(3)=xspx
			oldrecords(modplot)%attributes%ry(3)=yspx
	
	oldrecords(modplot)%attributes%IFNT(4)=ifnl
	oldrecords(modplot)%attributes%IJUS(4)=ijusy
	oldrecords(modplot)%attributes%angle(4)= gama
	oldrecords(modplot)%attributes%Idraw(4)=idrawy
	oldrecords(modplot)%attributes%ICOL(4)=iCOL(23)
	oldrecords(modplot)%attributes%rx(4)=xspy
			oldrecords(modplot)%attributes%ry(4)=yspy
	
	oldrecords(modplot)%attributes%IFNT(5)=ifnl
	oldrecords(modplot)%attributes%IJUS(5)=ijusz
	oldrecords(modplot)%attributes%angle(5)= delta
	oldrecords(modplot)%attributes%Idraw(5)=idrawz
	oldrecords(modplot)%attributes%ICOL(5)=iCOL(23)
	oldrecords(modplot)%attributes%rx(5)=xspz
			oldrecords(modplot)%attributes%ry(5)=yspz
	
	oldrecords(modplot)%numbers%numbx=numx
	oldrecords(modplot)%numbers%numby=numy
	oldrecords(modplot)%numbers%inumx=nxstep
	oldrecords(modplot)%numbers%inumy=nystep
	oldrecords(modplot)%param_axis%ntx=nx
	oldrecords(modplot)%param_axis%nty=ny
	oldrecords(modplot)%param_axis%nx1=nx1
	oldrecords(modplot)%param_axis%ny1=ny1
	oldrecords(modplot)%param_axis%itx=ndx
	oldrecords(modplot)%param_axis%ity=ndy
	oldrecords(modplot)%param_axis%xtic=xtic
	oldrecords(modplot)%param_axis%ytic=ytic
	oldrecords(modplot)%param_axis%numaxi=numaxi
	oldrecords(modplot)%param_axis%nwidth=nwidth
	oldrecords(modplot)%param_axis%npow=npow
	oldrecords(modplot)%param_axis%nplace=nplace
      
	do i=1,ndx
	oldrecords(modplot)%xVal(i,1)=xval1(i)
	enddo
	do i=1,ndy
	oldrecords(modplot)%yVal(i,1)=yval1(i)
	enddo
	do i=1,ndx
	do j=1,ndy
	oldrecords(modplot)%g3dpar%bad(i,j)=bad(i,j)
	oldrecords(modplot)%g3dpar%z(i,j)=z1(i,j)
	enddo
	enddo
	oldrecords(modplot)%g3dpar%all=all
	oldrecords(modplot)%g3dpar%cross=cross
	oldrecords(modplot)%g3dpar%fill=fill
	oldrecords(modplot)%g3dpar%fillbad=fillbad
	oldrecords(modplot)%g3dpar%mark=mark
	oldrecords(modplot)%g3dpar%posneg=posneg
	oldrecords(modplot)%g3dpar%inter=inter
	oldrecords(modplot)%g3dpar%ratio=ratio
	oldrecords(modplot)%g3dpar%rat=rat
	oldrecords(modplot)%g3dpar%radius=radius
	oldrecords(modplot)%g3dpar%ifram=ifram
	oldrecords(modplot)%g3dpar%theta=theta
	oldrecords(modplot)%g3dpar%phi=phi
	do i=1,6
	oldrecords(modplot)%g3dpar%istyle(i)=istyle(i)
	oldrecords(modplot)%g3dpar%isup(i)=isup(i)
	enddo

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


