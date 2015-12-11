c *********************************************************************
	subroutine inside(x,y,ixbox,iybox,iflag)
c *********************************************************************
c     Given cursor position (x,y) and box coordinates (ixbox(4),iybox(4)),
c	check if cursor is inside box ; uses some REAL maths !!!!
c *********************************************************************

	real*4	ixbox(4),iybox(4),d(4),x,y

	do i=1,3
		if(ixbox(i+1).eq.ixbox(i)) then
			d(i)=x-ixbox(i)
		else
		d(i)=y-(((iybox(i+1)-iybox(i))*(x-ixbox(i))-
     &           iybox(i)*ixbox(i)+
     &           iybox(i)*ixbox(i+1))/(ixbox(i+1)-ixbox(i)))
		endif
	enddo
	if(ixbox(1).eq.ixbox(4)) then
		d(4)=x-ixbox(4)
	else
	d(4)=y-(((iybox(1)-iybox(4))*(x-ixbox(4))-iybox(4)*ixbox(4)+
     &     iybox(4)*ixbox(1))/(ixbox(1)-ixbox(4)))
	endif
	if((d(1).gt.0.and.d(3).gt.0).or.(d(1).lt.0.and.d(3).lt.0)) then
		iflag=0
		goto 1
	else
		if((d(2).gt.0.and.d(4).gt.0).or.(d(2).lt.0.and.d(4).lt.0))
     &      then
			iflag=0
			goto 1
		endif
	endif
	iflag=1
1     continue
	end

c *********************************************************************
