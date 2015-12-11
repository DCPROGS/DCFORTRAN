subroutine point_intersection(xb,yb,xe,ye,xmov,ymov,d,d1,iflag)	
use menu_f90
use gino_f90

x1=xb
y1=yb
x2=xe
y2=ye

if(xb.gt.xe) then
	x1=xe
	y1=ye
	x2=xb
	y2=yb
endif

if(x1.gt.x2-d.and.x2.lt.x2+d) then
	if(yb.gt.ye) then
		y1=ye
		y2=yb
	endif
	if((ymov.gt.y1+d.and.ymov.lt.y2-d).and.(xmov.gt.x1-d1.and.xmov.lt.x1+d1)) iflag=1
else
	a=(y2-y1)/(x2-x1)
	b=y1-x1*(y2-y1)/(x2-x1)
	if(xmov.gt.x1+d.and.xmov.lt.x2-d) then
		y=a*xmov+b
		if(ymov.gt.y-d1.and.ymov.lt.y+d1) iflag=1
    endif
endif


end