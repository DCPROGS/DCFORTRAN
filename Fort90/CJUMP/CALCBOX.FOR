	subroutine calcbox(ix,iy,lb,ihb,x,y)

	integer*4  x(4),y(4)

	x(1)=ix
	y(1)=iy
	x(2)=ix+lb
	y(2)=iy
	x(3)=ix+lb
	y(3)=iy+ihb
	x(4)=ix
	y(4)=iy+ihb

	end
