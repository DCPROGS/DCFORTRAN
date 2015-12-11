C=======================================================================
	subroutine calcdim(xbox,ybox,n,ixlo,iylo,ixhi,iyhi)
C=======================================================================
C	INPUT 	:xbox,ybox,n
c	OUTPUT	:ixlo,iylo,ixhi,iyhi for read/write window!
C=======================================================================

	real	    xbox(n),ybox(n)
	integer*4 ixbox(n),iybox(n)
	do i=1,n
		call pospix(xbox(i),ybox(i),ixbox(i),iybox(i)) !screen-18
		iybox(i)=479-iybox(i)
	enddo
	ixlo=ixbox(1)
	do i=2,n
		if (ixbox(i).lt.ixlo) ixlo=ixbox(i)
	enddo
	ixhi=ixbox(1)
	do i=2,n
		if (ixbox(i).gt.ixhi) ixhi=ixbox(i)
	enddo
	iylo=iybox(1)
	do i=2,n
		if (iybox(i).lt.iylo) iylo=iybox(i)
	enddo
	iyhi=iybox(1)
	do i=2,n
		if (iybox(i).gt.iyhi) iyhi=iybox(i)
	enddo
	ixlo=ixlo-1
	ixhi=ixhi+1
	iylo=iylo-1
	iyhi=iyhi+1
	end

C=======================================================================
