	subroutine DRAWDAT(j,xval,yval,ndelt,isdev,
     & logity,logx,logy,sqrty,y0,yinf,x0,mono,
     & xmin,xmax,ymin,ymax,iud,tload,idev,
     & ndatj,ijoinj,icolj,isymj,symsj,thj,thj1,
     & th27,th271,th30,th301,barleng,interp,npint,
     & Xint,Yint,nint,yp1,ypn,Y2int,ndv1,ndimd)
c
	real*4 XVAL(ndv1,ndimd),YVAL(ndv1,ndimd)
	real xint(2048),yint(2048),Y2int(2048)	!for interpolation
	logical tload,interp,logity,logx,logy,sqrty,mono
	COMMON/SD/weight(100,10)  !set isdev=-1 in call if not needed
c
c To draw the data (+/- interpolation) in VPLOT
c Call as
c	subroutine DRAWDAT(j,xval,yval,ndelt,isdev,
c     & logity,logx,logy,sqrty,y0,yinf,x0,mono,
c     & xmin,xmax,ymin,ymax,iud,tload,idev,
c     & ndat(j),ijoin(j),icol(j),isym(j),syms(j),lth(j),lth(j)
c     & lth(27),lth(27),lth(30),lth(30),barleng,interp,npint,
c     & Xint,Yint,nint,yp1,ypn,Y2int,ndv1,ndimd)
c
	linetype=ijoinj
	jflag=0
	do 215 i=1,ndatj,ndelt
	if(isdev.ge.0.and.weight(i,j).eq.0.) goto 215	!skip points with w=0
	 xv=xval(i,j)
	 yv=yval(i,j)
	 if(logity) then
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
c Define arrays for interpolation (whether INTERP true or not, in case req below)
c	 if(nint.le.255) then
	 if(nint.le.2047) then
	   nint=nint+1
	   xint(nint)=xv
	   yint(nint)=yv
	 endif
	 if(jflag.eq.0) call MOVTO2(xv,yv)   !move to 1st point in range
	 if(iud.eq.1.and.tload) then
	    if(idev.ge.5) then
c	      call LOADVTB('2'//'W'//char(thj))      !data line
	    else
	      call LINWID(thj)
	    endif
	 endif
	 if(jflag.gt.0) then
	   call BROKEN(linetype)
	   if(interp.and.nint.le.2048) then
	     call MOVTO2(xv,yv)   !join with line below
	   else
	     call LINVIS(iud)
	     call LINTO2(xv,yv)   !join with line from last point
	   endif
	 endif
c need thinner lines to draw symbols? and for error bars?
	 if(isymj.ne.0.and.tload) then
	    if(idev.ge.5) then
c	      call LOADVTB('2'//'W'//char(th27))      !symbol thickness
	    else
	      call LINWID(th27)
	    endif
	 endif
	 ic=icolj
	 if(mono) ic=15		!bright white
	 call SYMBOL(xv,yv,isymj,symsj,ic)     !symbol if isym>0
	 if(.not.mono) call lincols(icolj)	!restore colour after SYMBOL
c plot error bars
	 if(isdev.eq.1) then
	   if(idev.ge.5) then
c	      call LOADVTB('2'//'W'//char(th30))
	   else
	      call LINWID(th30)
	   endif
	   sdmax=0.5*abs(ymax-ymin)	!largest sd to be plotted
	   if(logy) sdmax=0.5*abs(10.**ymax - 10.**ymin)
	   if(weight(i,j).gt.(1.0/sdmax**2)) then
	      sdev=sqrt(1.0/weight(i,j))
		yv1=yv-sdev
		if(logy) yv1=10.**yv-sdev
		if(logy.and.yv1.lt.1.e-37) goto 22
		if(logy) yv1=alog10(yv1)	!yv already logged
		call MOVTO2(xv-barleng,yv1)
		call LINTO2(xv+barleng,yv1)
		call MOVTO2(xv,yv1)
22		yv1=yv+sdev
		if(logy) yv1=alog10(10.**yv+sdev)
		call LINTO2(xv,yv1)
		call MOvTO2(xv-barleng,yv1)
		call LINTO2(xv+barleng,yv1)
		call MOVTO2(xv,yv)
	   endif
	 endif
	 jflag=1
215	 continue
c
c Add interpolated points here if interp=true (so they get plotted!)
	if(interp) then
c	   print 700,6
	   call SPLINE(Xint,Yint,nint,yp1,ypn,Y2int)
	   call MOVTO2(xint(1),yint(1))   !move to 1st point in range
	   x=float(npint+1)
	   do 4035 i=1,nint-1
	   do 4035 k=1,npint
	   xin=xint(i) + (xint(i+1)-xint(i))*float(k)/x	!divide the interval
	   klo=0		!full bisection for now
	   call SPLINT(Xint,Yint,Y2int,nint,klo,khi,xin,yout)
	   if(iud.eq.1) then
	     if(idev.ge.5) then
c	      call LOADVTB('2'//'W'//char(thj)) 	!data line
	     else

	      call LINWID(thj)
	     endif
	   endif
c if interp then do not join lines here, but join interp points below
	   call LINVIS(iud)
	   call BROKEN(linetype)
	   call LINTO2(xin,yout)   !join with line from last point
	   if(idev.ge.5) then
c	      call LOADVTB('2'//'W'//char(th27))
	   else
	      call LINWID(th27)
	   endif
	   ic=icolj
	   if(mono) ic=15		!bright white
	   call SYMBOL(xin,yout,isymj,symsj,ic)     !symbol if isym>0
4035	   continue
	endif
c
	RETURN
	end

