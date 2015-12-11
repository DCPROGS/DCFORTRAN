	subroutine draw_axis(xmin,xmax,ymin,ymax,ymin1,ymax1,
     &	xtic,ytic,nx1,ny1,xcross,ycross,itx,ity,tlenx,tleny,
     &	logx,logy,sqrty,doframe,icol,ntx,nty,idev,thick,itype,
     &  calbarx,calbary,xbeg4,ybeg4,xend4,yend4,
     &     xbeg5,ybeg5,xend5,yend5)

	
	use gino_f90

	logical logx,logy,sqrty,doframe,calbarx,calbary,landscape
	integer icol(250),itype(250)
	real thick(250)

	common/pixpos/ixgrid,iygrid,ixpix,iypix,wid,landscape

	ycros1=ycross
	y1=ymin
	y2=ymax
	if(sqrty) then
	   y2=sqrt(ymax)	!for comparison with pos
	   y1=sqrt(ymin)	!display thinks this is ymin,ymax
	   ycros1=sqrt(ycross)
	endif

1	continue

	call linwid(0.)
	if(calbarx.or.calbary) then
c	if(calbarx) then
		call glincols(icol(44),idev)
		call DCALBAR(xbeg4,ybeg4,xend4,yend4,
     &     xbeg5,ybeg5,xend5,yend5,thick,idev,1,
     &     itx,ity,ntx,nty,tlenx,tleny,ibk)

      else	
		call glincols(icol(244),idev)
		call linwid(thick(244))
		if(doframe) then
			call DFRAME(xmin,xmax,y1,y2,0) 
		
		endif
		!ycros1=sqrt(ycross)
		call glincols(icol(245),idev)
	call linwid(thick(245))
	call broken(itype(245))
		call DXAXIS(xmin,xmax,xtic,ycros1,logx,nx1,tlenx,itx)
		call DYAXIS(ymin,ymax,ytic,xcross,logy,ny1,tleny,ity,sqrty)
	endif
	
		call gFlushGraphics()
	end

	subroutine DXAXIS(xmin,xmax,xtic,ycros1,logx,nx1,tlen,itx)
c-------------------------------------------------------------------------
	logical logx,equal

	EQUAL(xa,ya)=abs(xa-ya).lt.0.00001
	
	roff=0.0	!for now (cf laxes2.for)

	tmaj=2.*tlen
	tmin=tlen
	xticm=xtic/float(nx1)   !distance between minor tics
	xmin1=xmin
	xmax1=xmax
	n=ifix((xmax1-xmin1)/xtic)
	if(n.gt.50) then
		n=50
		xtic=(xmin1-xmax1)/float(n)
	endif
	xtic1=xtic
!	call broken(0)
	call movto2(xmin1,ycros1)
	call LINTO2(xmax1,ycros1)
	k=0
	if(logx) then
	   x1=AFIXR(xmin1)
	   x=x1	!in case goto 42 next
	   if(EQUAL(x1,xmin1)) goto 42
	   x=x1-1.0	!nearest integer BELOW xmin to start minor tics
	else
	   x1=xmin1	 !make x1= position of 1st major tic
	   z=amod(xmin1,xtic1)
	   if(xmin1.lt.0.) X1=xmin1-z
	   if(xmin1.gt.0.) X1=xmin1-z+xtic
	   if(xmin1.eq.0.) X1=xmin1
	   x=x1	!in case goto 42 next
	   if(EQUAL(x1,xmin1)) goto 42
	   x=x1-xtic1	!!to start minor tics preceding 1st major
	endif
	if(nx1.le.1.or.n.gt.25) goto 42
	do j=2,nx1
	   if(logx) then
	   	x2=x+alog10(float(j))
	   else
	      x2=x+float(j-1)*xticm
	   endif
	   if(x2.gt.xmin1.and.x2.le.xmax1) then
	      call movto2(x2,ycros1)
	      call linto2(x2,ycros1+roff+tmin)	!draw tic
	  endif
	enddo
	x=x1
42	continue
	do i=1,n+1
		if(x.lt.xmax1) then
	   call movto2(x,ycros1+roff)       !start pos to draw tic
	   call linto2(x,ycros1+roff+tmaj)	!draw tic
	   if(nx1.gt.1.and.n.le.25) then
	  	do j=2,nx1
	   	   if(logx) then
	      	x2=x+alog10(float(j))
	   	   else
	      	x2=x+float(j-1)*xticm
	   	   endif
	   	   if(x2.lt.xmax1) then
	   	   	call movto2(x2,ycros1+roff)       !start pos to draw tic
	   	   	call linto2(x2,ycros1+roff+tmin)	!draw tic
		   endif
	  	enddo
	   endif
	endif
	   k=k+1
	   if(logx) then
	      x=x1+float(i)
	   else
	   	x=x1+float(i)*xtic
	   endif
	enddo
	RETURN
	end

c=========================================================================

	subroutine DYAXIS(ymin,ymax,ytic,xcross,logy,ny1,tlen,ity,sqrty)
	logical logy,equal,sqrty

	EQUAL(xa,ya)=abs(xa-ya).lt.0.00001
	
	roff=0.0	!for now (cf laxes2.for)

	mi=0
	tmaj=2.*tlen
	tmin=tlen
	yticm=ytic/float(ny1)   !distance between minor tics
	ymin1=ymin
	ymax1=ymax
	ytic1=ytic
	if(sqrty) then
	   ymax1=sqrt(ymax)	!for comparison with pos
	   ymin1=sqrt(ymin)	!display thinks this is ymin,ymax
	   ytic1=sqrt(ytic)
	endif
	n=ifix((ymax1-ymin1)/ytic1)

c draw the axis

	call movto2(xcross,ymin1)
	call linto2(xcross,ymax1)
	k=0
	if(logy) then
	   y1=AFIXR(ymin1)
	   y=y1	!in case goto 42 next
	   if(EQUAL(y1,ymin1)) goto 42
	   y=y1-1.0	!nearest integer BELOW ymin1 to start minor tics
	else
	   y1=ymin1	 !make y1= position of 1st major tic
	   z=amod(ymin1,ytic1)
	   if(ymin1.lt.0.) Y1=ymin1-z
	   if(ymin1.gt.0.) Y1=ymin1-z+ytic1
	   if(ymin1.eq.0.) Y1=ymin1
	   y=y1	!in case goto 42 next
	   if(EQUAL(y1,ymin1)) goto 42
	   y=y1-ytic1	!!to start minor tics preceding 1st major
	endif
	if(ny1.le.1) goto 42
	if(n.gt.25) goto 42 ! do not draw minor tics
	do 43 i=2,ny1
	   if(logy) then
	      y2=y+alog10(float(i))
	   else if(sqrty) then
	      y2=y*y + float(i-1)*dy
	      y2=sqrt(y2)
	   else
	      y2=y+float(i-1)*yticm
	   endif
	   if(y2.gt.ymin1.and.y2.le.ymax1) then
	      call movto2(xcross+roff,y2)
	      call linto2(xcross+roff+tmin,y2)	!draw tic
	   endif
43	continue
	y=y1
42	continue
	mi=mi+1
	if(y.lt.ymin1) goto 66
	if(n.gt.50.and.mod(mi,2).eq.0) goto 66
	call movto2(xcross+roff,y)
	call linto2(xcross+roff+tmaj,y)	!draw tic
66	continue
	if(ny1.eq.1) goto 471
	if(sqrty) then
	   dy=(y+ytic1)**2 - y**2 !real distance from this tic to next eg 16-4=12
	   dy=dy/float(ny1)	!divide into equal (real unit) increments
	endif
	if(n.gt.25) goto 471  ! do not draw minor tics
	do 47 i=2,ny1
	   if(logy) then
	      y2=y+alog10(float(i))
	   else if(sqrty) then
	      y2=y*y + float(i-1)*dy
	      y2=sqrt(y2)
	   else
	      y2=y+float(i-1)*yticm
	   endif
	   if(y2.lt.ymin1) goto 47
	   if(y2.gt.ymax1) goto 48	!axis finished
	   call movto2(xcross+roff,y2)
	   call linto2(xcross+roff+tmin,y2)	!draw tic
47	continue
471	continue
	k=k+1
	if(logy) then
	   y=y1+float(k)
	else
	   y=y1+float(k)*ytic1
	endif
	if(y.le.ymax1) goto 42	!draw next cycle
48	continue

	
	RETURN
	end

