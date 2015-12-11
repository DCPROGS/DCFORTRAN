c *********************************************************************
	subroutine inside1(ix,iy,ixlo,iylo,ixhi,iyhi,flag)
c *********************************************************************
c     Given cursor position (ix,iy) and box coordinates (ixbox(4),iybox(4)),
c	check if cursor is inside box ; uses some REAL maths !!!!
c *********************************************************************

	integer*4	ixbox(4),iybox(4),d(4)

	ixbox(1)=ixlo
	iybox(1)=iylo
	ixbox(2)=ixhi
	iybox(2)=iylo
	ixbox(3)=ixhi
	iybox(3)=iyhi
	ixbox(4)=ixlo
	iybox(4)=iyhi
	do i=1,3
		if(ixbox(i+1).eq.ixbox(i)) then
			d(i)=ix-ixbox(i)
		else
		d(i)=iy-(((iybox(i+1)-iybox(i))*(ix-ixbox(i))-
     &           iybox(i)*ixbox(i)+
     &           iybox(i)*ixbox(i+1))/(ixbox(i+1)-ixbox(i)))
		endif
	enddo
	if(ixbox(1).eq.ixbox(4)) then
		d(4)=ix-ixbox(4)
	else
	d(4)=iy-(((iybox(1)-iybox(4))*(ix-ixbox(4))-iybox(4)*ixbox(4)+
     &     iybox(4)*ixbox(1))/(ixbox(1)-ixbox(4)))
	endif
	if((d(1).gt.0.and.d(3).gt.0).or.(d(1).lt.0.and.d(3).lt.0)) then
		flag=0
		goto 1
	else
		if((d(2).gt.0.and.d(4).gt.0).or.(d(2).lt.0.and.d(4).lt.0))
     &      then
			flag=0
			goto 1
		endif
	endif
	flag=1
1     continue
	end

c *********************************************************************
