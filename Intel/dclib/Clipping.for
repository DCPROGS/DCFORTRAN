	subroutine clipping(xlo,ylo,xhi,yhi,x1,y1,x2,y2,iflag)

	real*4 xb(4),yb(4)
	iflag=0
	xb(1)=xlo
	yb(1)=ylo
	xb(2)=xhi
	yb(2)=ylo
	xb(3)=xhi
	yb(3)=yhi
	xb(4)=xlo
	yb(4)=yhi
	call inside(x1,y1,xb,yb,iflag1)
	call inside(x2,y2,xb,yb,iflag2)
	if (iflag1.eq.1.and.iflag2.eq.1) then
	   iflag=1
	   goto 1
	endif
	if (iflag1.eq.0.and.iflag2.eq.0) then
	   if((x1.le.xlo.and.x2.le.xlo).or.(y1.le.ylo.and.y2.le.ylo).or.
     &   (x1.ge.xhi.and.x2.ge.xhi).or.(y1.ge.yhi.and.y2.ge.yhi)) goto 1
	endif
	if (x2.eq.x1) then
		a=-111.111
	else if (y2.eq.y1) then
		b=-111.111
	else
	   a=(y2-y1)/(x2-x1)
	   b=y1-x1*(y2-y1)/(x2-x1)
	endif
	if (iflag1.eq.1) then
	   call inters(xlo,ylo,xhi,yhi,a,b,x2,y2)
	   iflag=1
	   goto 1
	endif
	if (iflag2.eq.1) then
	   call inters(xlo,ylo,xhi,yhi,a,b,x1,y1)
	   iflag=1
	   goto 1
	endif
	call inters(xlo,ylo,xhi,yhi,a,b,x2,y2)
	call inters(xlo,ylo,xhi,yhi,a,b,x1,y1)
      iflag=1
1     end


	subroutine inters(xlo,ylo,xhi,yhi,a,b,x,y)
	if(a.eq.(-111.111)) then  ! x2=x1
	   if(y.lt.ylo) y=ylo
	   if(y.gt.yhi) y=yhi
	   goto 111
	else if(b.eq.(-111.111)) then  ! y2=y1
	   if(x.lt.xlo) x=xlo
	   if(x.gt.xhi) x=xhi
	   goto 111
	endif
	if((y.lt.ylo).and.(x.gt.xlo.and.x.lt.xhi)) then
	   ya=ylo
	   xa=(ylo-b)/a
	   x=xa
	   y=ya
	   goto 111
	endif
	if((y.gt.yhi).and.(x.gt.xlo.and.x.lt.xhi)) then
	   ya=yhi
	   xa=(yhi-b)/a
	   x=xa
	   y=ya
	   goto 111
	endif
	if((x.lt.xlo).and.(y.gt.ylo.and.y.lt.yhi)) then
	   xa=xlo
	   ya=a*xlo+b
	   x=xa
	   y=ya
	   goto 111
	endif
	if((x.gt.xhi).and.(y.gt.ylo.and.y.lt.yhi)) then
	   xa=xhi
	   ya=a*xhi+b
	   x=xa
	   y=ya
	   goto 111
	endif
	xa=xlo
	ya=a*xlo+b
	if(ya.gt.ylo.and.ya.lt.yhi.and.abs(x-xlo).lt.abs(x-xhi)) then
	   x=xa
	   y=ya
	else
	   xa=xhi
	   ya=a*xhi+b
	   if(ya.gt.ylo.and.ya.lt.yhi) then
	      x=xa
	      y=ya
	   else
		ya=ylo
		xa=(ylo-b)/a
		if(xa.gt.xlo.and.xa.lt.xhi.and.abs(y-ylo).lt.abs(y-yhi))then
	         x=xa
	         y=ya
		else
	         y=yhi
	         x=(yhi-b)/a
		endif
	   endif
	endif
111	continue
	end
