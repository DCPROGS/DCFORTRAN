	program testin

	integer ixbox(4),iybox(4)
	ix=100
	iy=100
	do i=1,4
		print*,'ix',i,'='
		read*,ixbox(i)
		print*,'iy',i,'='
		read*,iybox(i)
	enddo
	call mode(18)
	pause
	call clears(0,480,1)
	pause
	call drawpoly(ixbox,iybox,4,15)
	pause
	call wpixel(ix,iy,14)
	pause
	call inside(ix,iy,ixbox,iybox,flag)
	print*,ix,iy,flag
	do i=1,4
		print*,'x',i,'=',ixbox(i)
		print*,'y',i,'=',iybox(i)
	enddo
	pause
	end


