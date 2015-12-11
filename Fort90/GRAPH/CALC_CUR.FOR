c CALC CURVE SECTION.
303	continue
	if(ncurvc.le.0) goto 999
	linetype=0		!always cont line- dashes synthesized via zseq
c
c	nbadc=0
	x0=1.e-36		!smallest value for logs
	do 54 j1=1,ncurvc		!plot ncurvc curves
	   j=icurvc(j1)			!curve # to be plotted
	   if(iline(j).lt.0) goto 54	!skip this curve (added 06/24/90 05:29pm)
	   ij=iabs(iline(j))
	   call SETBIGJ(j,j0)
	   if(idev.ge.5) then
c	      call LOADVTB('2'//'W'//char(lth(j0+10)))
	      call LINWID(2*lth(j0+10))
	   else
	      call LINWID(lth(j0+10))
	   endif
	   if(.not.mono) call lincols(icol(j0+10))		!colour for jth calc curve
	   if(ij.gt.0.and.ij.le.9) then
c	      straight=.false.
	      goto 310		!dashed calc curve
	   else if(ij.ge.10) then
c	      straight=.true.
	      linetype=ij-10			!join points with straight line type #ij
	   endif
	   Jflag=0
	   do 220 k=1,ncal(j)
	      xv=xcal(k,j)
	      yv=ycal(k,j)
	      if(logity) then
		   if(abs(yinf-yv).lt.1.e-30) goto 220	!for case of yv=yinf
		   if(yv.gt.yinf-x0) goto 220	!skip values above ymax!
		   if(yv.lt.y0+x0) goto 220	!skip values below ymin!
		   yv=(yv-y0)/(yinf-yv)	! Hill scale
		   if(yv.gt.x0) then
		      yv=alog10(yv)
		   endif
	      endif
		if(logx) then
		   if(xv.gt.x0) then
		   xv=alog10(xv)
		   endif
		endif
		if(logy) then
		   if(yv.gt.x0) then
		   yv=alog10(yv)
		   endif
		endif
		if(sqrty.and.yv.ge.0.) yv=sqrt(yv)
		if(xv.lt.xmin.or.xv.gt.xmax) goto 220
		if(yv.lt.ymin.or.yv.gt.ymax) goto 220
		if(jflag.eq.0) call MOVTO2(xv,yv)	!1st point in range
		if(jflag.gt.0) then
c		   call broken(linetype)
		   call linTO2(xv,yv)	!rest of points
		endif
		jflag=1
220	   continue
	   goto 311
310	   continue
c
c          now section to draw dashed etc calc curves
c           define dash sequences: specify lengths of each repeating sequence
c           starting with bright (down=true). Lengths specified as percent
c           of length of the X axis.
	   goto(702,703,704,705,706),ij
702	   kseq=2
	   zseq(1)=0.3
	   zseq(2)=0.5
	   goto 312
703	   kseq=2
	   zseq(1)=1.5
	   zseq(2)=2.
	   goto 312
704	   kseq=2
	   zseq(1)=5.
	   zseq(2)=2.5
	   goto 312
705	   kseq=4
	   zseq(1)=6.
	   zseq(2)=2.
	   zseq(3)=1.5
	   zseq(4)=2.
	   goto 312
706	   kseq=6
	   zseq(1)=6.
	   zseq(2)=2.
	   zseq(3)=1.5
	   zseq(4)=2.
	   zseq(5)=1.5
	   zseq(6)=2.
	   goto 312
c
312	   continue
	   sfac=float(iyhi-iylo)/float(ixhi-ixlo)		!O.K.?
	   xr=0.01*(xmax-xmin)
	   yr=0.01*(ymax-ymin)
	   if(sqrty) yr=0.01*(sqrt(ymax)-sqrt(ymin))
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
	      if(yv.lt.y0+x0) goto 313	!skip values below ymin!
	      yv=(yv-y0)/(yinf-yv)	! Hill scale
	      if(yv.gt.x0) then
	   	yv=alog10(yv)
	      endif
	   endif
	   if(logx) then
	      if(xv.gt.x0) then
	   	xv=alog10(xv)
	      endif
	   endif
	   if(logy) then
	      if(yv.gt.x0) then
	   	yv=alog10(yv)
	      endif
	   endif
	   if(sqrty.and.yv.ge.0.) yv=sqrt(yv)
	   if(xv.lt.xmin.or.xv.gt.xmax) goto 313
	   if(yv.lt.ymin.or.yv.gt.ymax) goto 313
	   if(jflag.eq.0) then
	      call MOVTO2(xv,yv)	!move to 1st point in range
	      jflag=1
	      goto 317
	   endif
c        Now part done for all points after 1st in range
314	   dxn=(xv-xvlast)/xr
	   dyn=sfac*(yv-yvlast)/yr
	   vert=(dxn.eq.0.).or.abs(dxn).lt.(1.e-20*abs(dyn))    !line is vertical
c        fix here -> VHIST
	   if(vert) then
	      b=1.0
	      if(dyn.lt.0.) b=-1.0
	   else
	      b=dyn/dxn	!slope of line
	   endif
318	   zn=sqrt(dxn*dxn + dyn*dyn)
c        calc zn=dist from last point to current one in units= percent
c        of length of X axis
c        next bit when amount left to draw extends beyond (or exactly
c        up to) the current point- go to this point
	   if(zleft.ge.zn) then
315	      if(.not.down) call MOVTO2(xv,yv)
	      if(down) then
		   call BROKEN(linetype)
		   call MOVTO2(xv,yv)
	      endif
	      zleft=zleft-zn	!amount of this segment still to be drawn
	      if(zleft.gt.0.) goto 317
	      down=.not.down	!zleft=0 i.e.segment reaches current point exactly
	      k=k+1
	      if(k.gt.kseq) k=1
	      zleft=zseq(k)
	   else
c           next bit done when amount of line remaining to be drawn does
c           not extend as far as current point- keep drawing segments (with
c 		slope as between last point and current one) until it does.
	   	if(vert) then
		   xv1=xvlast
c			=======fix here -> VHIST
	         yv1=yvlast + b*zleft*yr/sfac	!b=-1 or +1 here
	   	else
		   xv1=xvlast + zleft*xr/sqrt(1.+b*b)
		   yv1=yvlast + b*(xv1-xvlast)*yr/(sfac*xr)
	      endif
	      if(.not.down) call MOVTO2(xv1,yv1)
	      if(down) then
		   call BROKEN(linetype)
		   call LINTO2(xv1,yv1)
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
317	   xvlast=xv
	   yvlast=yv
313	   continue	!end of loop for points
311	   continue
54	continue	!loop for each calc curve

	end
