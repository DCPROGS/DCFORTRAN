	subroutine DRAWDAT5(j,xval,yval,ndelt,
     & logity,logx,logy,sqrty,y0,yinf,x0,mono,ilog,
     & xmin,xmax,ymin,ymax,iud,tload,idev,
     & ndatj,ijoinj,icolj,isymj,symsj,rj0,r27,
     & r30,barleng,interp,npint,
     & Xint,Yint,nint,yp1,ypn,Y2int,ndv1,ndimd,
     & weight,kwi,kwj,icw,sx,sy,n1)
c=====================================================================
	use gino_f90
	real*4 XVAL(n1:ndv1,ndimd),YVAL(n1:ndv1,ndimd)
	real xint(2048),yint(2048),Y2int(2048)	!for interpolation
	logical tload,interp,logity,logx,logy,sqrty,mono,dobar
	real*4 weight(kwi,kwj)

c=====================================================================
	linetype=ijoinj
	
	
!	call gSetsetlinetypeLineMode(GSOFT)
!	linetype=j
!	linetype=7
	jflag=0
	do 215 i=1,ndatj,ndelt
	 if(icw.ge.0.and.weight(i,j).eq.0.) goto 215	!skip points with w=0
	 xv=xval(i,j)
	 yv=yval(i,j)
	 rnan=yv
  
	is_nan = ISNAN(rnan)
	if(is_nan) goto 215
	 if(logity) then
       !if yv=yinf then next line does not work since yinf-x0=yinf essentially
	   if(yv.gt.yinf-x0) goto 215	!skip values above ymax!
	   if(abs(yinf-yv).lt.1.e-30) goto 215	!for case of yv=yinf
	   if(yv.lt.y0+x0) goto 215	!skip values below ymin!
	   yv=(yv-y0)/(yinf-yv)	! Hill scale
	   if(yv.gt.x0) then
		yv=alog10(yv)
	   endif
	 endif
	 if(logx) then
	   if(xv.gt.x0) then
		xv=alog10(xv)
	   else
		xv=2.*xmax		!so not plotted
	   endif
	 endif
	 if(logy) then
	   if(yv.gt.x0) then
		yv=alog10(yv)
	   else
		yv=2.*ymax		!so not plotted
	   endif
	 endif
	 if(ilog.eq.7) then
		xv=1./xv
		yv=1./yv
	 else if(ilog.eq.8) then
		xv=yval(i,j)
		yv=yval(i,j)/xval(i,j)
	 else if(ilog.eq.9) then
		yv=yval(i,j)
		xv=yval(i,j)/xval(i,j)
	 else if(ilog.eq.10) then
		xv=xval(i,j)
		yv=xval(i,j)/yval(i,j)
	 endif
	 if(sqrty.and.yv.ge.0.) yv=sqrt(yv)
       !  xv,yv now defined but don't plot if outside min,max range
	 if(xv.lt.xmin.or.xv.gt.xmax) goto 215
	 if(yv.lt.ymin.or.yv.gt.ymax) goto 215
	 if(sqrty.and.ymin.ge.0.and.ymax.ge.0) then
	   if(yv.lt.sqrt(ymin).or.yv.gt.sqrt(ymax)) goto 215
	 endif

       ! Define arrays for interpolation (whether INTERP true or not, in case req below)
	 if(nint.le.2047) then
	   nint=nint+1
	   xint(nint)=xv
	   yint(nint)=yv
	   if(nint.gt.1) then
		if(xint(nint).le.xint(nint-1)) then
		   nint=100000
		   interp=.false.
		endif
	   endif
	 endif

       ! Now draw!
	 if(jflag.eq.0) call movto2(xv,yv)   !move to 1st point in range
	 
	 if(jflag.gt.0) then
	   if(interp.and.nint.le.2048) then
	     call movto2(xv,yv)   !join with line below
	   else
	    call linvis(iud)
	   
	!	 call genqlinewidthmode(isw)
	!	 call genqlinewidth(wid0)
		 call linwid(rj0)
	!	 call genqlinewidth(wid0)
!	  call gsetlinestyle(linetype)
c	     call setlinetype(ijoinj)
		call broken(ijoinj)
	     call linto2(xv,yv)   !join with line from last point
	   endif
	 endif
	 if(isymj.lt.99) then
	    if(mono) ic=15		!bright white
		call glincols(icolj,idev)
	    call linwid(0.)
	    call setlinetype(0)
	    call jSYMBOL(xv,yv,isymj,sx,sy,ic,idev)     !symbol if isym>0
	 endif

       ! Plot error bars
	 if(icw.eq.1) then
	   call linwid(0.)
	   sdmax=5.0*abs(ymax-ymin)	!largest sd to be plotted
	   if(logy) sdmax=5.0*abs(10.**ymax - 10.**ymin)
	   if(weight(i,j).gt.(1.0/sdmax**2)) then
			sdev=sqrt(1.0/weight(i,j))
			! Do lower limit first
			yv1=yval(i,j)-sdev			!lower limit
			dobar=.true.		!draw bar at top/bottom of sd bar
			linetype=0		!continuous
			if(logy) then
			if(yv1.gt.x0) then
			yv1=alog10(yv1)
			linetype=0		!continuous
			else
			yv1=ymin		!ymin is already log
			linetype=2		!short dash if lower limit undefined
			dobar=.false.
			endif
			else if(logity) then
		   yv1=(yv1-y0)/(yinf-yv1)	! Hill scale
		   if(yv1.gt.x0) then
			yv1=alog10(yv1)
			linetype=0		!continuous
		   else
			yv1=ymin		!ymin is already log
			linetype=2		!short dash if lower limit undefined
			dobar=.false.
		   endif
			else if(ilog.eq.7) then
		   yv1=1./yv1
		   if(yv1.lt.0.) then
			yv1=ymax
			linetype=2
		   endif
			else if(ilog.eq.8) then
		   yv1=yv1/xval(i,j)
		   if(yv1.lt.0.) then
			yv1=ymin
			linetype=2
		   endif
			else if(ilog.eq.9) then
		   yv1=yv1
		   if(yv1.lt.0.) then
			yv1=ymin
			linetype=2
		   endif
			else if(ilog.eq.10) then
		   yv1=xval(i,j)/yv1
		   if(yv1.lt.0.) then
			yv1=ymax
			linetype=2
		   endif
			endif
	      if(sqrty.and.yv1.ge.0.) then
			yv1=sqrt(yv1)
		      if(yv1.le.sqrt(ymin)) then
		         dobar=.false.
		         yv1=sqrt(ymin)
		      else if(yv1.ge.sqrt(ymax)) then
		         dobar=.false.
		         yv1=sqrt(ymax)
		     endif
		else
	     if(yv1.le.ymin) then
	        dobar=.false.
	        yv1=ymin
	     else if(yv1.ge.ymax) then
	        dobar=.false.
	        yv1=ymax
	     endif
		endif
	    call gsetbrokenline(linetype)
		call linto2(xv,yv1)
		if(dobar) then
		   call movto2(xv-barleng,yv1)
	    !   call gsetlinestyle(0)
	call gsetbrokenline(gsolid)
		   call linto2(xv+barleng,yv1)
		endif
		call movto2(xv,yv)
		! upper limit
		yv1=yval(i,j) + sdev		!upper limit
		dobar=.true.		!draw bar at top/bottom of sd bar
		linetype=0		!continuous
		if(logy) then
		   if(yv1.gt.x0) then
			yv1=alog10(yv1)
			linetype=0		!continuous
		   else
			yv1=ymin		!ymin is already log
			linetype=2		!short dash if lower limit undefined
			dobar=.false.
		   endif
		else if(logity) then
		   yv1=(yv1-y0)/(yinf-yv1)	! Hill scale
		   if(yv1.gt.x0) then
			yv1=alog10(yv1)
			linetype=0		!continuous
		   else
			yv1=ymax		!ymax is already log
			linetype=2		!short dash if upper limit undefined
			dobar=.false.
		   endif
		else if(ilog.eq.7) then
		   yv1=1./yv1
		else if(ilog.eq.8) then
		   yv1=yv1/xval(i,j)
		else if(ilog.eq.9) then
		   yv1=yv1
		else if(ilog.eq.10) then
		   yv1=xval(i,j)/yv1
		endif
	      if(sqrty.and.yv1.ge.0.) then
			yv1=sqrt(yv1)
		      if(yv1.le.sqrt(ymin)) then
		         dobar=.false.
		         yv1=sqrt(ymin)
		      else if(yv1.ge.sqrt(ymax)) then
	         dobar=.false.
		         yv1=sqrt(ymax)
		     endif
		else
	     if(yv1.le.ymin) then
	        dobar=.false.
	        yv1=ymin
	     else if(yv1.ge.ymax) then
	        dobar=.false.
	        yv1=ymax
	     endif
		endif
	    call gsetbrokenline(linetype)
		call linto2(xv,yv1)
		if(dobar) then
		   call movto2(xv-barleng,yv1)
	      ! call gsetlinestyle(0)
	call gsetbrokenline(gsolid)
		   call linto2(xv+barleng,yv1)
		endif
		call movto2(xv,yv)
	 	if(ilog.eq.8.or.ilog.eq.9) then
		   !Do lower limit for horizontal error
		   dobar=.true.		!draw bar at top/bottom of sd bar
		   linetype=0		!continuous
		   if(ilog.eq.8) then
			xv1=yval(i,j)-sdev		!lower limit
		   else if(ilog.eq.9) then
			xv1=(yval(i,j)-sdev)/xval(i,j)
		   endif
		   if(xv1.lt.0.) then
			xv1=xmin
			linetype=2
		   endif
		   if(xv1.le.xmin) then
			dobar=.false.
			xv1=xmin
		   else if(xv1.ge.xmax) then
			dobar=.false.
			xv1=xmax
		   endif
	       call gsetbrokenline(linetype)
		   call linto2(xv1,yv)
		   if(dobar) then
			 call movto2(xv1,yv-barleng)
	        ! call gsetlinestyle(0)
	call gsetbrokenline(gsolid)
		     call linto2(xv1,yv+barleng)
		   endif
		   call movto2(xv,yv)
             ! Now upper limit for horizontal error
		   if(ilog.eq.8) then
			xv1=yval(i,j)+sdev		!upper limit
		   else if(ilog.eq.9) then
			xv1=(yval(i,j)+sdev)/xval(i,j)
		   endif
		   dobar=.true.		!draw bar at top/bottom of sd bar
		   linetype=0		!continuous
		   if(xv1.le.xmin) then
			dobar=.false.
			xv1=xmin
		   else if(xv1.ge.xmax) then
			dobar=.false.
			xv1=xmax
		   endif
	       call gsetbrokenline(linetype)
		   call linto2(xv1,yv)
		   if(dobar) then
			call movto2(xv1,yv-barleng)
	         call gsetbrokenline(gsolid)
			call linto2(xv1,yv+barleng)
		   endif
		   call movto2(xv,yv)
		endif
	   endif
	 endif

	 linetype=0		!continuous
	 jflag=1
215	 continue

	! Add interpolated points here if interp=true (so they get plotted!)
	if (interp) then
	   call SPLINE(Xint,Yint,nint,yp1,ypn,Y2int)
	   call movto2(xint(1),yint(1))   !move to 1st point in range
	   x=float(npint+1)
	   do i=1,nint-1
	   do k=1,npint
	   xin=xint(i) + (xint(i+1)-xint(i))*float(k)/x	!divide the interval
	   klo=0		!full bisection for now
	   call SPLINT(Xint,Yint,Y2int,nint,klo,khi,xin,yout)
	   if(iud.eq.1) then
	      call LINWID(rj0)
	
	   endif
	   call linwid(rj0)
	   call gsetlinestyle(linetype)
	   call linvis(iud)
	   call linto2(xin,yout)   !join with line from last point
	   call linwid(0.)     
	   ic=icolj
	   if(mono) ic=15		!bright white
	   call jSYMBOL(xin,yout,isymj,sx,sy,ic,idev)     !symbol if isym>0
	   enddo
	   enddo
	endif

	RETURN
	end

