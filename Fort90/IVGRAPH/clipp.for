	subroutine clipp(x1,y1,x2,y2,xlo,ylo,xhi,yhi)

	real*4	x(4),y(4),d(4),x0(4),y0(4)

	if(xlo.gt.xhi) then
	   temp=xlo
	   xlo=xhi
	   xhi=itemp
	endif
	if(ylo.gt.yhi) then
	   temp=ylo
	   ylo=yhi
	   yhi=temp
	endif
	if((x1.lt.xlo.and.x2.lt.xlo).or.
     &	(x1.gt.xhi.and.x2.gt.xhi)) goto 1
	if((y1.lt.ylo.and.y2.lt.ylo).or.
     &	(y1.gt.yhi.and.y2.gt.yhi)) goto 1
	if((x1.gt.xlo.and.x1.lt.xhi).and.
     &	(x2.lt.xhi.and.x2.gt.xlo).and.
     &      (y1.gt.ylo.and.y1.lt.yhi).and.
     &	(y2.gt.ylo.and.y2.lt.yhi)) goto 1
	x(1)=xlo
	y(1)=ylo
	x(2)=xhi
	y(2)=ylo
	x(3)=xhi
	y(3)=yhi
	x(4)=xlo
	y(4)=yhi
	do i=1,4
	   if(x1.eq.x2) then
	      d(i)=x(i)-x1
	   else
		d(i)=y(i)-(((y1-y2)*(x(i)-x1)-y1*x1+y1*x2)/
     &	(x2-x1))
	   endif
	enddo
	if((d(1).gt.0.and.d(2).gt.0.and.d(3).gt.0.and.d(4).gt.0).or.
     &   (d(1).lt.0.and.d(2).lt.0.and.d(3).lt.0.and.d(4).lt.0)) goto 1
	do i=1,4
	   x(i)=-100.
	   y(i)=-100.
	   x0(i)=-100.
	   y0(i)=-100.
	enddo
	xlx=xlo
	ylx=y1+(xlo-x1)*(y2-y1)/(x2-x1)
	if(ylx.ge.ylo.and.ylx.le.yhi) then
	   x(1)=xlx
	   y(1)=ylx
	endif
	xhx=xhi
	yhx=y1+(xhi-x1)*(y2-y1)/(x2-x1)
	if(yhx.ge.ylo.and.yhx.le.yhi) then
	   x(2)=xhx
	   y(2)=yhx
	endif
	yly=ylo
	xly=x1+(ylo-y1)*(x2-x1)/(y2-y1)
	if(xly.ge.xlo.and.xly.le.xhi) then
	   x(3)=xly
	   y(3)=yly
	endif
	yhy=yhi
	xhy=x1+(yhi-iy1)*(x2-x1)/(y2-y1)
	if(xhy.ge.xlo.and.xhy.le.xhi) then
	   x(4)=xhy
	   y(4)=yhy
	endif
	j=0
	do i=1,4
	   if(x(i).ne.-100.) then
		j=j+1
		x0(j)=x(i)
		y0(j)=y(i)
	    endif
	enddo
	x1=x0(1)
	y1=y0(1)
	if(x0(2).ne.-100.) then
		x2=x0(2)
		y2=y0(2)
	endif
1     return
	end
