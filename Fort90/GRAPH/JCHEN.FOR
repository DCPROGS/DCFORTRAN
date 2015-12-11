	program jchen

      INTEGER ISTYLE(6),ISUP(6)
	integer*4 icord(15,2)		!for 2 alternative colour orders
	allocatable::gridz,az,ax,ay,w,IFILLG,ifillz
 	INTEGER*4 IFILLG(:,:),ifillz(:,:)
	character ans,getch
	character*5 betha
	character*6 alpha,gama
	character*30 delta
	logical fill,inter,axes
	real*4	ax(:),ay(:),az(:),gridz(:,:),w(:)

	allocate(ax(1500),ay(1500),az(1500))

	icord(1,1)=1	!dark blue
	icord(2,1)=9	!light blue
	icord(3,1)=3	!dark cyan
	icord(4,1)=11	!light cyan
	icord(5,1)=2	!dark green
	icord(6,1)=10	!light green
	icord(7,1)=8	!dark grey
	icord(8,1)=5	!dark magenta
	icord(9,1)=13	!light magenta
	icord(10,1)=6	!brown
	icord(11,1)=4	!red-brown
	icord(12,1)=12	!red
	icord(13,1)=14 !yellow
	icord(14,1)=15	!white
	icord(15,1)=7	!light grey (usually background!)
	OPEN(unit=11,file="m2c",status='UNKNOWN',
     &    access='DIRECT',form='FORMATTED',recl=55)
	xlow=0
	xhigh=0
	ylow=0
	yhigh=0
	zmin=0
	zmax=0
20	format(a6,i5,a5,a6,i6,f10.3,2f8.3)
	do i=1,1200
	    read(11,20) alpha,intg1,betha,gama,intg2,x,y,z
	    ax(i)=x
	    if(ax(i).lt.xlow) xlow=ax(i)
	    if(ax(i).gt.xhigh) xhigh=ax(i)
	    ay(i)=y
	    if(ay(i).lt.ylow) ylow=ay(i)
	    if(ay(i).gt.yhigh) yhigh=ay(i)
	    az(i)=z
	    if(az(i).lt.zmin) zmin=az(i)
	    if(az(i).gt.zmax) zmax =az(i)
	enddo
	xmin=xlow
	xmax=xhigh
	ymin=ylow
	ymax=yhigh
	ztic=(zmax-zmin)/10.
	xtic=(xmax-xmin)/10.
	ytic=(ymax-ymin)/10.
	zmin=zmin-3
	zmax=zmax+3
1000  continue
	call GINO
	   call VGA
	   call gsetcols(0)
	   call errswi(-1)
	   call brkswi(1)
	   call chaswi(1)
	   call grfmod (1)
	   call harcha
	   call mode(18)
	call piCCLE
	vxlo=0.	! for VIEWPORT
	vxhi=1080.
	vylo=0.
	vyhi=810.
	call broken(0)
	RATIO=0.35
	RAT=1.0
      RADIUS=500.0
      THETA=30.0
      PHI=30.0
	IFRAM=3
	XWMIN=100.
	XWMAX=1050.
	YWMIN=50.
	YWMAX=620.
	FILL=.FALSE.
	INTER=.FALSE.
	AXES=.TRUE.
	NUMAXI=0
	NWIDTH=5
	NPOW=0
	NPLACE=1
	nxstep=1
	nystep=1
c     Fill background colour
	ncolfu=1
	ncolfd=11
	ncolgu=14
	ncolgd=8
	ncola=11
	kcol=2
	do i=1,6
	   istyle(i)=3
	   isup(i)=0
	enddo

	call FILLWIN(0,55,639,387,5)		!graph area
c	call FILLWIN(0,388,639,479,5)			!dialog area
	call HLINE(0,639,54,15)		!line to mark bottom of data area
	call HLINE(0,639,389,15)	!line to mark top of data area

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
	idev=0
	csize=vyhi/80. !copy for common/dmenu/
	csizeb=csize
	sizet=csize*2.5
	sizel=csize*1.7
	width=1.0
	numx=75
	numy=16
c=====================================================================
100   continue
	call FILLWIN(0,55,639,387,5)		!graph area
	call linwid(width)
	if(idev.eq.0) then
         kdev=-1
	else if(idev.eq.6) then
         kdev=-6
	endif
	nx=numx
	ny=numy
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
      if(allocated(ifillg)) then
	   DEALLOCATE(ifillg)
	endif
      if(allocated(ifillz)) then
	   DEALLOCATE(ifillz)
	endif
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
c   count as 'zero' any z value that is within 2.5% either side of zero
	z0=0.025*abs(gzmax-gzmin)
	ncont=5
	bw=(gzmax-gzmin)/float(ncont)
      ALLOCATE(ifillg(numx-1,numy-1))
      ALLOCATE(ifillz(numx-1,numy-1))
      DO I=1,NUMX-1
	   DO J=1,NUMY-1
		if(kcol.eq.2.or.kcol.eq.3) then
		   ncolone=ncolfu
		   call lincols(ncolone,kdev)
		   ifillg(i,j)=ncolone
		   if(kcol.eq.3) then
		      ncolone=ncolfd
		      call lincols(ncolone,kdev)
		      ifillz(i,j)=ncolone
		   endif
		else
		   do k=1,ncont
		      blo=gzmin+float(k-1)*bw
		      bhi=gzmin+float(k)*bw
		      zv=gridz(i,j)
		      if(zv.ge.blo.and.zv.le.bhi) then
			   ncolm=icord(k,1)
		         call lincols(ncolm,kdev)
			   ifillg(i,j)=ncolm !avoid 1
		         ifillz(i,j)=ncolm+8
		      endif
		   enddo
		endif
	   enddo
      enddo
	CALL PRJBND(XLO,XHI,YLO,YHI)
      CALL PRJFRA(IFRAM)

	call prjgrd(nxstep,nystep)
      CALL WINDO2(XWMIN,XWMAX,YWMIN,YWMAX)
      CALL PRJSCA(xtic,ytic,ztic)
      CALL PRJAXI(ISTYLE,ISUP)
      CALL HEIRAT(RATIO)
      CALL XYRAT(RAT)
	call PRJLEV(zmin,zmax)
	ngonu=ncolgu
	ngond=ncolgd
	ncolax=ncola
	call lincols(ngonu,kdev)
	call lincols(ngond,kdev)
	call lincols(ncolax,kdev)
      CALL PRJLIN(ncolax,ngonu,ngond)
	call perset(xmin,xmax,ymin,ymax,zmin,zmax,radius,theta,phi)
	if(icount.ge.3) then
    	      CALL PERFIL(NUMX,XMIN,XMAX ,NUMY,YMIN,YMAX,GRIDZ,
     +          RADIUS,THETA,PHI,IFILLG,NW,W)	!grid displayed
	else
      CALL PERPRJ(NUMX,XMIN,XMAX ,NUMY,YMIN,YMAX,GRIDZ,
     +              RADIUS,THETA,PHI,NW,W)	!grid displayed

	endif
	CALL PERAXE(NW,W)		!axes displayed

	ans=getch(k)
	if(icount.eq.0) then
		numx=150
		numy=8
	else if(icount.eq.1) then
		numx=8
		numy=150
	else if(icount.eq.2) then
		numx=40
		numy=30
		kcol=4
	else if(icount.eq.3) then
		numx=33
		numy=33
		kcol=6
	endif
	icount=icount+1
c	call piccle
	if(icount.le.4) goto 100
	call mode(3)
	end
