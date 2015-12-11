	subroutine DRAWGUES(ncalc,y0,yinf,
     & Xcal,Ycal,nsfit,juse,noguess,njset,ndc1,ndimc,ic,idev,jcset,
     & ifitmode)
c=======================================================================
	use gino_f90
	real*4 xcal(ndc1,ndimc),ycal(ndc1,ndimc)
	integer juse(njset)
	logical logx,logy,noguess(100),logity,sqrty
	common/logval/logx,logy,sqrty,logity

	
		common/limit/wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,
     &	xmin,xmax,ymin,ymax
	
	if(sqrty) then
	   ymaxq=sqrt(ymax)
	   yminq=sqrt(ymin)
	else
	   ymaxq=(ymax)
	   yminq=(ymin)
	endif
	xlth=xmax-xmin
	ylth=ymaxq-yminq
	wid=0.001*ylth
	call linwid(wid)
c To draw curves for initial guesses on graph after leaving VPLOT
c Code taken from VPLOT5 (after label 303)
	linetype=0		!continuous
	
	x0=1.e-36		!smallest value for logs
	do m=1,nsfit
	   j=juse(m)
	   if(ifitmode.eq.2.and.j.ne.jcset) goto 221
	   if(noguess(j)) goto 221	!skip this set
c???	   call LINWID(0.50)
	   call glincols(ic,idev)		!colour for jth calc curve
	   Jflag=0
	   do k=1,ncalc
		xv=xcal(k,j)
		yv=ycal(k,j)
		if(logx) then
		   if(xv.gt.x0) then
			xv=alog10(xv)
		   else
			goto 220
		   endif
		endif
		if(logy) then
		   if(yv.gt.x0) then
			yv=alog10(yv)
		   else
			goto 220
		   endif
		endif
		x0=1.e-36		!smallest value for taking logs
		if(logity) then
		   if(abs(yinf-yv).lt.1.e-30) goto 220	!for case of yv=yinf
		   if(yv.gt.yinf-x0) goto 220	!skip values above ymax!
		   if(yv.lt.y0+x0) goto 220	!skip values below ymin!
		   yv=(yv-y0)/(yinf-yv)	! Hill scale
		   if(yv.gt.x0) then
			yv=alog10(yv)
		   else
			goto 220
		   endif
		endif
		if(jflag.eq.0) then
		   xd=xv-wxmin
		   yd=yv-wymin
		  
		   call movto2(xv,yv)	!1st point in range
		 
		   !call MOVto2(xd,yd)	!1st point in range
		 
		else
		   if(xv.le.xmin.or.xv.ge.xmax.or.
     &		yv.lt.ymin.or.yv.gt.ymax) then
			xd=xv-wxmin
			yd=yv-wymin
		  
			call MOVto2(xv,yv)	!1st point in range
		  
			!call MOVto2(xd,yd)	!1st point in range
		   
		   else
			xd=xv-wxmin
			yd=yv-wymin
                  call broken(linetype)
		  
			call linto2(xv,yv)	!1st point in range
		  
			!call linto2(xd,yd)	!1st point in range
		  
		   endif
		endif
		jflag=1
220		continue
	   enddo
221	   continue
	enddo
	call gFlushGraphics()
	RETURN
	end


