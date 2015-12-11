	subroutine draw_curves(xcal,ycal,icurvc,ncurvc,logity,logx,
     &	logy,sqrty,y0,yinf,x0,ilog,idev,
     &    wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,
     &	xmin,xmax,ymin,ymax,ncal,
     &	iline,icol,thick,ndc1,ndimc,jfirst,dxs,dys)

	use gino_f90
	USE DFLIB

	
	INTEGER*2 STS

	real*8 sq
	DIMENSION ICOL(250),ITYPE(250),THICK(250),IDRAW(250)
	real*4 XCAL(ndc1,ndimc),YCAL(ndc1,ndimc)
	integer*4 ncal(ndimc),icurvc(ndimc),iline(ndimc)
	allocatable::x1calc,x2calc
	real x1calc(:),x2calc(:),xminsav(20),xmaxsav(20)
	logical logx,logy,sqrty,logity,vert,down
	dimension zseq(10)
	character cnum0*11

	ytc=0
	ALLOCATE(x1calc(ndimc),x2calc(ndimc))
	yminq=ymin
	yrm=ymax
		ymin20=ymin
		ymax20=ymax
		if(sqrty) ymin20=sqrt(ymin)
		if(sqrty) ymax20=sqrt(ymax)
		xt=0.5*(xmin+xmax)
		yt=ymax20+0.15*(ymax20-ymin20)!
	if(sqrty) yminq=sqrt(ymin)
	if(ncurvc.gt.0) then
	   do i=1,ncurvc
		j=icurvc(i)
		x1calc(j)=xmin
		x2calc(j)=xmax
		if(logx) then
		x1calc(j)=10.**xmin
		x2calc(j)=10.**xmax
		endif
	   enddo
	endif
	yt=0.
	linetype=0		!always cont line- dashes synthesized via zseq
	x0=1.e-36		!smallest value for logs
	do 54 j1=1,ncurvc		!plot ncurvc curves
	j=icurvc(j1)			!curve # to be plotted
	if(logx) then
	   if(x1calc(j).gt.0) then
		x1=alog10(x1calc(j))
	   else
		goto 311
	   endif
	   if(x2calc(j).gt.0) then
	   	x2=alog10(x2calc(j))
	   else
		goto 311
	   endif
	else
	   x1=x1calc(j)
	   x2=x2calc(j)
	endif
	if(x1.lt.xmin) then
	   x1=xmin
	endif
	if(x2.gt.xmax) then
	   x2=xmax
	endif
!	if(x2calc(j).gt.xmax) then
!	   if(logx) then
!		x2calc(j)=10.**xmax		!always non-log
!	   else
!		x2calc(j)=xmax
!	   endif
!	endif
	if(jfirst.eq.0) then
	   xminsav(j)=x1calc(j)
	   xmaxsav(j)=x2calc(j)
	endif
	if(iline(j).lt.0) goto 54	!skip this curve (added 06/24/90 05:29pm)
!	call intconv(j,cnum0)
!	call write_string(cnum0,xmax+0.05*(xmax-xmin),ymax20-ytc,0.,0,0,
!     & 5.,icol(j+150),dxs,dys)
!	call movto2(xmax+0.1*(xmax-xmin),ymax20-ytc)						
!	ytc=ytc+0.07*(ymax20-ymin20)

	ij=iabs(iline(j))
	call SETBIGJ(j1,j0)
	call SETBIGJ(j,j0)
      call glincols(icol(j0+150),idev)
	call LINWID(thick(j0))
c      call glincols(j,idev)		!colour for jth calc curve
	if(ij.gt.0.and.ij.le.9) then
	   goto 310		!dashed calc curve
	else if(ij.ge.10) then
	   linetype=ij-10			!join points with straight line type #ij
!	   call linto2(xmax+0.1*(xmax-xmin),ymax20-ytc)
	endif
	Jflag=0
	
	do 220 k=1,ncal(j)
	if(k.gt.ndc1) goto 220
	xv=xcal(k,j)
	yv=ycal(k,j)
	if(logity) then
	   if(abs(yinf-yv).lt.1.e-30) goto 220	!for case of yv=yinf
	   if(yv.gt.yinf-x0) goto 220	!skip values above ymax!
	   if(yv.lt.y0+x0) goto 220	!skip values below yminq!
	   yv=(yv-y0)/(yinf-yv)	! Hill scale
	   if(yv.gt.x0) then
		yv=alog10(yv)
	   else
		goto 220
	   endif
	endif
	if(logx) then
	   if(xv.gt.x0) then
		xv=alog10(xv)
	   else
		goto 220
	   endif
	endif
	if(xv.gt.xmax) goto 220
	if(logy) then
	   if(yv.gt.x0) then
		yv=alog10(yv)
	   else
		goto 220
	   endif
	endif

	if(sqrty.and.yv.ge.0.) then
c		if (yv.lt.3.d-308) yv=1.d-307  
		if (yv.lt.1.d-154) yv=1.d-154      
		yv=sqrt(yv)
	yrm=sqrt(ymax)
	endif
	x0=1.e-36		!smallest value for taking logs
	if(ilog.eq.7) then
	   if(xv.lt.x0.or.yv.lt.x0) goto 220
	   xv=1./xv
	   yv=1./yv
	else if(ilog.eq.8) then
	   xv=ycal(k,j)
	   yv=ycal(k,j)/xcal(k,j)
	else if(ilog.eq.9) then
	   if(xv.lt.x0) goto 220
	   xv=ycal(k,j)/xcal(k,j)
	   yv=ycal(k,j)
	else if(ilog.eq.10) then
	   if(yv.lt.x0) goto 220
	   xv=xcal(k,j)
	   yv=xcal(k,j)/ycal(k,j)
	endif
	if(jflag.eq.0) then
	   xv0=xv
	   yv0=yv
	else
ccc	   goto 1111
	   call  clipping(x1,yminq,x2,yrm,xv0,yv0,xv,yv,iflag)
	   if(iflag.eq.0) then
		xv0=xv
		yv0=yv
		goto 220
	   endif
	   if(yv0.eq.yminq) then
		xv0=xv
		yv0=yv
		goto 220
	   endif
ccc1111	   continue
	   call movto2(xv0,yv0)	!1st point in range
	   call broken(linetype)
	   call linto2(xv,yv)	!1st point in range
	   xv0=xv
	   yv0=yv
	endif
	jflag=1
220	continue
	goto 311
310	continue   ! dashed
	goto(702,703,704,705,706),ij
702	kseq=2
	zseq(1)=0.4
	zseq(2)=1.
	goto 312
703	kseq=2
	zseq(1)=1.5
	zseq(2)=2.
	goto 312
704	kseq=2
	zseq(1)=5.
	zseq(2)=3.
	goto 312
705	kseq=4
	zseq(1)=6.
	zseq(2)=3.
	zseq(3)=2.5
	zseq(4)=3.
	goto 312
706	kseq=6
	zseq(1)=6.*dxs*1.2
	zseq(2)=3.*dxs*1.2
	zseq(3)=2.5*dxs*1.2
	zseq(4)=3.*dxs*1.2
	zseq(5)=2.5*dxs*1.2
	zseq(6)=3.*dxs*1.2
	goto 312
312	continue
	sfac=0.7		!O.K.?
	xr=0.01*(xmax-xmin)
	yr=0.01*(ymax-yminq)
	if(sqrty) yr=0.01*(sqrt(ymax)-sqrt(yminq))
	k=1
 	zleft=zseq(k)
	down=.true.
	x0=1.e-36		!smallest value for logs
	Jflag=0
	do 313 i=1,ncal(j)
	xv=xcal(i,j)
	yv=ycal(i,j)
	
	if(logity) then
	   if(abs(yinf-yv).lt.1.e-30) goto 313	!for case of yv=yinf
	   if(yv.gt.yinf-x0) goto 313	!skip values above ymax!
	   if(yv.lt.y0+x0) goto 313	!skip values below yminq!
	   yv=(yv-y0)/(yinf-yv)	! Hill scale
	   if(yv.gt.x0) then
		yv=alog10(yv)
	   else
		goto 313
	   endif
	endif
	if(logx) then
	   if(xv.gt.x0) then
		xlog=alog10(xv)
		xv=xlog
	   else
		goto 313
	   endif
	endif
	if(logy) then
	   if(yv.gt.x0) then
		yv=alog10(yv)
	   else
		goto 313
	   endif
	endif
c		if (yv.lt.3.d-308) yv=1.d-307  
		if (yv.lt.1.d-154) yv=1.d-154     
	if(yv.ge.0.and.sqrty) then
		yv=sqrt(yv)
		yrm=sqrt(ymax)
	endif
	x0=1.e-36		!smallest value for taking logs
	if(ilog.eq.7) then
	   if(xv.lt.x0.or.yv.lt.x0) goto 313
	   xv=1./xv
	   yv=1./yv
	else if(ilog.eq.8) then
	   xv=ycal(k,j)
	   yv=ycal(k,j)/xcal(k,j)
	else if(ilog.eq.9) then
	   if(xv.lt.x0) goto 313
	   xv=ycal(k,j)/xcal(k,j)
	   yv=ycal(k,j)
	else if(ilog.eq.10) then
	   if(yv.lt.x0) goto 313
	   xv=xcal(k,j)
	   yv=xcal(k,j)/ycal(k,j)
	endif
	if(xv.lt.xmin.or.xv.gt.xmax) goto 313
	if(xv.lt.x1.or.xv.gt.x2) goto 313
	if(yv.lt.yminq.or.yv.gt.yrm) goto 313
	if(jflag.eq.0) then
	   if(xv.lt.xmin) xv=xmin
	   if(xv.gt.xmax) xv=xmax
	   if(yv.lt.yminq) yv=yminq
	   if(yv.gt.yrm) yv=yrm
	   call movto2(xv,yv)	!move to 1st point in range!!!!!!!!!!!
	   jflag=1
	   goto 317
	endif
c     Now part done for all points after 1st in range
314	dxn=(xv-xvlast)/xr
	dyn=sfac*(yv-yvlast)/yr
	vert=(dxn.eq.0.).or.abs(dxn).lt.(1.e-20*abs(dyn))    !line is vertical
	if(vert) then
	   b=1.0
	   if(dyn.lt.0.) b=-1.0
	else
	   b=dyn/dxn	!slope of line
	endif
318	continue
	sq=dxn*dxn+dyn*dyn
	
	zn=sqrt(sq)
	
	if(zleft.ge.zn) then
315	   if(.not.down) call MOVto2(xv,yv)
	   if(down) then
		call broken(linetype)
            call linto2(xv,yv)
	   endif
	   zleft=zleft-zn	!amount of this segment still to be drawn
	   if(zleft.gt.0.) goto 317
	   down=.not.down	!zleft=0 i.e.segment reaches current point exactly
	   k=k+1
	   if(k.gt.kseq) k=1
	   zleft=zseq(k)
	else
	   if(vert) then
		xv1=xvlast
	    yv1=yvlast + b*zleft*yr/sfac	!b=-1 or +1 here
	   else
		sq1=1.+b*b
c		if (sq1.lt.3.d-308) sq1=1.d-307  
		if (sq1.lt.1.d-154) sq1=1.d-154     
c		if(sq1.le.0.0) sq1=0.01
		xv1=xvlast + zleft*xr/sqrt(sq1)
		yv1=yvlast + b*(xv1-xvlast)*yr/(sfac*xr)
	   endif
	   if(xv1.lt.xmin) xv1=xmin
	   if(xv1.gt.xmax) xv1=xmax
	   if(yv1.lt.yminq) yv1=yminq
	   if(yv1.gt.ymax) yv1=ymax
	   if(.not.down) call MOVto2(xv1,yv1)
	   if(down) then
		call broken(linetype)
          call linto2(xv1,yv1)
	   endif
	   dxn=(xv-xv1)/xr		!for dist from xv1,yv1 to current point
	   dyn=sfac*(yv-yv1)/yr
	   xvlast=xv1
	   yvlast=yv1
	   down=.not.down		!prepare for next segment
	   k=k+1
	   if(k.gt.kseq) k=1
	   zleft=zseq(k)
	   goto 318	!repeat until current point reached
	endif
317	xvlast=xv
	yvlast=yv
313	continue	!end of loop for points
311	continue
	
54	continue	!loop for each calc curve
      call gFlushGraphics()
	end