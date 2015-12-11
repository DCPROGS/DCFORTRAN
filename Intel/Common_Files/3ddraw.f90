	call gcleargraphics
	call linwid(width)
	if(idev.eq.0) then
         kdev=-1
	else if(idev.eq.6) then
         kdev=-6
	endif
	idim=nx*ny
	
	kdev=0 !=== 
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
	nw=2*nw
	if(allocated(w)) DEALLOCATE(w)
	allocate(w(nw))
	if(allocated(gridz)) then
	   DEALLOCATE(gridz)
	endif
	allocate(gridz(numx,numy))
	call glincols(1,0) !===
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
	   if(idrawx.eq.1) call JUSTIFYSTRING(xspx,yspx,xtitle,beta,
     &      sizel,ijusx)
	
	   if(idrawy.eq.1) call JUSTIFYSTRING(xspy-10,yspy,ytitle,gama,
     &      sizel,ijusy)
	   if(idrawz.eq.1) call JUSTIFYSTRING(xspz,yspz,ztitle,delta,
     &	sizel,ijusz)
		call glincols(48,idev)
		call JUSTIFYSTRING(xspx+20,yspx-30,'x axis',beta,
     &      sizel,ijusx)
	call JUSTIFYSTRING(xspy+10,yspy-30,'y axis',gama,
     &      sizel,ijusy)
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
	DEALLOCATE(z)