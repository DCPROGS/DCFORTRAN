c======================================================================
	subroutine gauto3d(xval1,yval1,z1,bad,nx,ny,ndx,ndy,
     &	xtitle,ytitle,ztitle,title1,idrawt,idrawx,idrawy,idrawz,
     &       quarter,idev,plot,iplot,kcol,posneg)
c======================================================================
	real*4 z1(ndx,ndy)
	real*4 xval1(ndx),yval1(ndy)
	logical bad(ndx,ndy)
c
	allocatable::gridz,az,ax,ay,w,IFILLZ,IFILLG
	real*4 gridz(:,:),az(:),ax(:),ay(:),w(:)
	allocatable::z,xval,yval
	real*4 z(:,:),xval(:),yval(:)
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
	logical mono,plot,fill,inter,present,landplot,axes
	logical quarter
      INTEGER ISTYLE(6),ISUP(6)
      common/ginof/wxmin,wxmax,wymin,wymax,vxlop,vxhip,vylop,vyhip,
     & ymenu,
     & xlop,xhip,ylop,yhip
	common/hlp/help		!for QDIALOG
	COMMON/cols/icol,mono
	logical usedef
	common/def/usedef		!for qdialog
	common/plot3d/ifnt,ifnl,alfa,beta,gama,delta,
     &     ijust,ijusx,ijusy,ijusz,
     &     xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,numx,numy,
     &     xmin,xmax,ymin,ymax,zmin,zmax,xtic,ytic,ztic,
     &     ratio,rat,radius,theta,phi,ifram,numaxi,nwidth,npow,nplace,
     &     nxstep,nystep,istyle,isup,
     &     fill,inter,axes,fillbad,autplt,cross
c
	qfile="C:\PLOTQ.DAT"
	ihtype=3	!tells VHELP that call was from GPLOT3D
c Set things that re not to be changed after rescale
c	idev=0
	ncont=abs(kcol)
	if(fillbad) mark=.true.
	kdev=-1
c      CALL PICCLE
	CALL PAPENQ(XPAP,YPAP,IPAPTY)
	vxlo=0.	! for VIEWPORT
	vxhi=1080.
	vylo=0.
	vyhi=810.
	xlo=0.2*vxhi
	xhi=0.8*vxhi
	ylo=0.2*vyhi
	yhi=0.8*vyhi
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
	allocate(z(nx,ny),xval(nx),yval(ny))
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
	if(.not.mono.and.idev.eq.0) then
          call lincols(icol(71),idev)
	    call movto2(xrp(1),yrp(1))
	    call POFTO2(0,0,0,xrp,yrp,4)
c	    call lincols(15,idev)		!bright white
	else if (idev.eq.3.or.idev.eq.4.or.idev.eq.5) then
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
	call WRITQNUM(iplot,x20,y2-0.03*ypap,icw,idev)	!write plot queue # on screen/plot
	call errdev(10)
      CALL SURF		!Surf banner
	call errdev(10)
1003  CALL SETVP2(vxlo,vxhi,vylo,vyhi,xlop1,xhip1,ylop1,yhip1)	!screen->grey
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
		         ifillz(i,j)=ncolm+8
		      endif
		   enddo
		endif
	   enddo
      enddo
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
	      call perspa((xmin+xmax)/2,ymin-4*dy,zmin,xspx,yspx)
	      call perspa(xmin-4*dx,(ymin+ymax)/2,zmin,xspy,yspy)
	      call perspa(xmax+3*dx,ymin-3*dy,(zmin+zmax)/2,xspz,yspz)
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
	DEALLOCATE(z)
c===================
c
	call errdev(10)
	call surend
	call errdev(10)
      CALL SETVP2(vxlop,vxhip,vylop,vyhip,vxlop,vxhip,vylop,vyhip)	!screen->grey
	RETURN
	END


