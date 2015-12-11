	subroutine DRAWDAT5(j,xval,yval,ndelt,
     & logity,logx,logy,sqrty,y0,yinf,x0,mono,ilog,
     & xmin,xmax,ymin,ymax,iud,tload,idev,
     & ndatj,ijoinj,icolj,isymj,symsj,rj0,r27,
     & r30,barleng,interp,npint,
     & Xint,Yint,nint,yp1,ypn,Y2int,ndv1,ndimd,
     & weight,kwi,kwj,icw)
c
	real*4 XVAL(ndv1,ndimd),YVAL(ndv1,ndimd)
	real xint(2048),yint(2048),Y2int(2048)	!for interpolation
	logical tload,interp,logity,logx,logy,sqrty,mono,dobar,colseq
	real*4 weight(kwi,kwj)
	common/ekplot/jbff1,jbff2,colseq,ice,icpe
	linetype=ijoinj
	jflag=0
	icolj0=icolj
	do 215 i=1,ndatj,ndelt
	if(icw.ge.0.and.weight(i,j).eq.0.) goto 215	!skip points with w=0
	 xv=xval(i,j)
	 yv=yval(i,j)
	 if(colseq.and.i.le.jbff2.and.i.ge.jbff1) then
	    icolj=icpe
	 else
	    icolj=icolj0
	 endif
	 if(logity) then
c NB if yv=yinf then next line does not work since yinf-x0=yinf essentially
	   if(yv.gt.yinf-x0) goto 215	!skip values above ymax!
	   if(abs(yinf-yv).lt.1.e-30) goto 215	!for case of yv=yinf
	   if(yv.lt.y0+x0) goto 215	!skip values below ymin!
	   yv=(yv-y0)/(yinf-yv)	! Hill scale
	   if(yv.gt.x0) then
		yv=alog10(yv)
c	   else
c	      nbadd=nbadd+1
c	      if(nbadd.le.25) xdbad(nbadd)=xv
c	      if(nbadd.le.25) ydbad(nbadd)=yv
	   endif
	 endif
	 if(logx) then
	   if(xv.gt.x0) then
		xv=alog10(xv)
	   else
		xv=2.*xmax		!so not plotted
c	      nbadd=nbadd+1
c	      if(nbadd.le.25) xdbad(nbadd)=xv
c	      if(nbadd.le.25) ydbad(nbadd)=yv
	   endif
	 endif
	 if(logy) then
	   if(yv.gt.x0) then
		yv=alog10(yv)
	   else
		yv=2.*ymax		!so not plotted
c	      nbadd=nbadd+1
c	      if(nbadd.le.25) xdbad(nbadd)=xv
c	      if(nbadd.le.25) ydbad(nbadd)=yv
	   endif
	 endif
c start of modif #1 07/23/96 10:52am
c      =7 for 1/y vs 1/x
c      =8 for y/x vs y 		!Scatchard
c      =9 for y vs y/x		!Eadie Hofstee
c      =10 for x/y vs x		!eponym?
c end of modif #1 07/23/96 10:52am
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
c	 if(((logy.or.logity).and.yv.le.x0).or.(logx.and.xv.le.x0)) then
c	   nbadd=nbadd+1
c	   xdbad(nbadd)=xv
c	   ydbad(nbadd)=yv
c	   if(nbad.eq.1) call BELL(1)
c	   call LOCATE(0,0)		!row=0,col 0
c	   print 2161,nbad,xv,yv
c2161	   format(' OBSERVED VALUE < 1.E-37 FOR LOG: n,xv,yv= ',i4,2g13.6)
c	 endif
	 if(sqrty.and.yv.ge.0.) yv=sqrt(yv)
c  xv,yv now defined but don't plot if outside min,max range
	 if(xv.lt.xmin.or.xv.gt.xmax) goto 215
	 if(yv.lt.ymin.or.yv.gt.ymax) goto 215
	 if(sqrty.and.ymin.ge.0.and.ymax.ge.0) then
	   if(yv.lt.sqrt(ymin).or.yv.gt.sqrt(ymax)) goto 215
	 endif

c Define arrays for interpolation (whether INTERP true or not, in case req below)
c	 if(nint.le.255) then
c If x() not strictly increasing, as required by spline, then set nint=100000
c so no more values recorded, and interp option not shown.
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
c
c Now draw!
	 if(jflag.eq.0) call gramov(xv,yv)   !move to 1st point in range
	 if(iud.eq.1.and.tload) then
	      call LINWID(rj0)
	 endif
c=?	 call LINWID(rj0)
	 if(jflag.gt.0) then
	   if(interp.and.nint.le.2048) then
	     call gramov(xv,yv)   !join with line below
	   else
	     call linvis(iud)
	     call broken(linetype)
	     call gralin(xv,yv)   !join with line from last point
	   endif
	 endif
c need thinner lines to draw symbols? and for error bars?
	 if(isymj.lt.99) then
	    if(isymj.ne.0.and.tload) then
	      call LINWID(r27)
	    endif
c=?	    call LINWID(r27)
	    ic=icolj
	    if(mono) ic=15		!bright white
	    call graspa(xv,yv,xsp,ysp)
	    call jSYMBOL(xsp,ysp,isymj,symsj,ic,idev)     !symbol if isym>0
	    if(.not.mono) call lincols(icolj,idev)	!restore colour after SYMBOL
	 endif
c	 call SYMBOL(xv,yv,isymj,symsj,ic)     !symbol if isym>0
c	 if(.not.mono) call COLTYP1(icolj)	!restore colour after SYMBOL
c
c Plot error bars
	 if(icw.eq.1) then
	      call LINWID(r30)
c	   sdmax=0.5*abs(ymax-ymin)	!largest sd to be plotted
	   sdmax=5.0*abs(ymax-ymin)	!largest sd to be plotted
c	   if(logy) sdmax=0.5*abs(10.**ymax - 10.**ymin)
	   if(logy) sdmax=5.0*abs(10.**ymax - 10.**ymin)
	   if(weight(i,j).gt.(1.0/sdmax**2)) then
	      sdev=sqrt(1.0/weight(i,j))
c Do lower limit first
		yv1=yval(i,j)-sdev			!lower limit
		dobar=.true.		!draw bar at top/bottom of sd bar
		linerr=0		!continuous
		if(logy) then
		   if(yv1.gt.x0) then
			yv1=alog10(yv1)
			linerr=0		!continuous
		   else
			yv1=ymin		!ymin is already log
			linerr=2		!short dash if lower limit undefined
			dobar=.false.
c=		      goto 22
		   endif
		else if(logity) then
		   yv1=(yv1-y0)/(yinf-yv1)	! Hill scale
		   if(yv1.gt.x0) then
			yv1=alog10(yv1)
			linerr=0		!continuous
		   else
			yv1=ymin		!ymin is already log
			linerr=2		!short dash if lower limit undefined
			dobar=.false.
c=		      goto 22
		   endif
		else if(ilog.eq.7) then
		   yv1=1./yv1
c  for double reciprocal plot, y-sdev gives upper limit -if neg set to ymax
		   if(yv1.lt.0.) then
			yv1=ymax
			linerr=2
		   endif
		else if(ilog.eq.8) then
		   yv1=yv1/xval(i,j)
		   if(yv1.lt.0.) then
			yv1=ymin
			linerr=2
		   endif
		else if(ilog.eq.9) then
		   yv1=yv1
		   if(yv1.lt.0.) then
			yv1=ymin
			linerr=2
		   endif
		else if(ilog.eq.10) then
		   yv1=xval(i,j)/yv1
		   if(yv1.lt.0.) then
			yv1=ymax
			linerr=2
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
	      call broken(linerr)
		call gralin(xv,yv1)
		if(dobar) then
		   call gramov(xv-barleng,yv1)
	         call broken(0)
		   call gralin(xv+barleng,yv1)
		endif
		call gramov(xv,yv)
c Now upper limit
22		yv1=yval(i,j) + sdev		!upper limit
		dobar=.true.		!draw bar at top/bottom of sd bar
		linerr=0		!continuous
		if(logy) then
		   if(yv1.gt.x0) then
			yv1=alog10(yv1)
			linerr=0		!continuous
		   else
			yv1=ymin		!ymin is already log
			linerr=2		!short dash if lower limit undefined
			dobar=.false.
		   endif
		else if(logity) then
		   yv1=(yv1-y0)/(yinf-yv1)	! Hill scale
		   if(yv1.gt.x0) then
			yv1=alog10(yv1)
			linerr=0		!continuous
		   else
			yv1=ymax		!ymax is already log
			linerr=2		!short dash if upper limit undefined
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
	      call broken(linerr)
		call gralin(xv,yv1)
		if(dobar) then
		   call gramov(xv-barleng,yv1)
	         call broken(linerr)
		   call gralin(xv+barleng,yv1)
		endif
		call gramov(xv,yv)
c ilog =8 for y/x vs y 		!Scatchard
c      =9 for y vs y/x		!Eadie Hofstee
c For ilog=8 or 9 plot horizontal error bars too
	 	if(ilog.eq.8.or.ilog.eq.9) then
c Do lower limit for horizontal error
		   dobar=.true.		!draw bar at top/bottom of sd bar
		   linerr=0		!continuous
		   if(ilog.eq.8) then
			xv1=yval(i,j)-sdev		!lower limit
		   else if(ilog.eq.9) then
			xv1=(yval(i,j)-sdev)/xval(i,j)
		   endif
		   if(xv1.lt.0.) then
			xv1=xmin
			linerr=2
		   endif
		   if(xv1.le.xmin) then
			dobar=.false.
			xv1=xmin
		   else if(xv1.ge.xmax) then
			dobar=.false.
			xv1=xmax
		   endif
	         call broken(linerr)
		   call gralin(xv1,yv)
		   if(dobar) then
			call gramov(xv1,yv-barleng)
	         call broken(0)
			call gralin(xv1,yv+barleng)
		   endif
		   call gramov(xv,yv)
c Now upper limit for horizontal error
		   if(ilog.eq.8) then
			xv1=yval(i,j)+sdev		!upper limit
		   else if(ilog.eq.9) then
			xv1=(yval(i,j)+sdev)/xval(i,j)
		   endif
		   dobar=.true.		!draw bar at top/bottom of sd bar
		   linerr=0		!continuous
		   if(xv1.le.xmin) then
			dobar=.false.
			xv1=xmin
		   else if(xv1.ge.xmax) then
			dobar=.false.
			xv1=xmax
		   endif
	         call broken(linerr)
		   call gralin(xv1,yv)
		   if(dobar) then
			call gramov(xv1,yv-barleng)
	         call broken(0)
			call gralin(xv1,yv+barleng)
		   endif
		   call gramov(xv,yv)
		endif
	   endif
	 endif
c
c	 linetype=0		!continuous
	 jflag=1
215	 continue
c
c Add interpolated points here if interp=true (so they get plotted!)
	if(interp) then
c	   print 700,6
	   call SPLINE(Xint,Yint,nint,yp1,ypn,Y2int)
	   call gramov(xint(1),yint(1))   !move to 1st point in range
	   x=float(npint+1)
	   do 4035 i=1,nint-1
	   do 4035 k=1,npint
	   xin=xint(i) + (xint(i+1)-xint(i))*float(k)/x	!divide the interval
	   klo=0		!full bisection for now
	   call SPLINT(Xint,Yint,Y2int,nint,klo,khi,xin,yout)
	   if(iud.eq.1) then
	      call LINWID(rj0)
	   endif
	      call LINWID(rj0)
c if interp then do not join lines here, but join interp points below
c	   call broken(linetype)
	   call linvis(iud)
	   call gralin(xin,yout)   !join with line from last point
	   call LINWID(r27)	!for symbols
	   ic=icolj
	   if(mono) ic=15		!bright white
	   call graspa(xin,yout,xsp,ysp)
	   call jSYMBOL(xsp,ysp,isymj,symsj,ic,idev)     !symbol if isym>0
c	   call SYMBOL(xin,yout,isymj,symsj,ic)     !symbol if isym>0
4035	   continue
	endif
c
	RETURN
	end

