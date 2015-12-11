	subroutine zbar(x,y,length,iheigth,iwidth,icolor,icfill)

	integer*2 x,y,x1(4),y1(4),x2(4),y2(4),x3(4),y3(4)

	x1(1)=x
	y1(1)=y
	x1(2)=x
	y1(2)=y+iheigth
	x1(3)=x+length
	y1(3)=y+iheigth
	x1(4)=x+length
	y1(4)=y
	x2(1)=x1(3)
	y2(1)=y1(3)
	x2(2)=x1(4)
	y2(2)=y1(4)
	x2(3)=x1(4)+int(0.5*iwidth)
	y2(3)=y1(4)+int(0.5*iwidth)
	x2(4)=x1(3)+int(0.5*iwidth)
	y2(4)=y1(3)+int(0.5*iwidth)
	x3(1)=x2(4)
	y3(1)=y2(4)
	x3(2)=x2(1)
	y3(2)=y2(1)
	x3(3)=x1(2)
	y3(3)=y1(2)
	x3(4)=x3(3)+int(0.5*iwidth)
	y3(4)=y3(3)+int(0.5*iwidth)

	call fillpoly(x1,y1,4,icolor,icfill)
	call fillpoly(x2,y2,4,icolor,icfill+8)
	call fillpoly(x3,y3,4,icolor,icfill+8)
	end
