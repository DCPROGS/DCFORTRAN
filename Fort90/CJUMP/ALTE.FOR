	subroutine draw_poly(ix,iy,n,ic)
	integer*2	ix(n)
	integer*2	iy(n)
	do i=1,n-1
		call iline(ix(i),iy(i),ix(i+1),iy(i+1),ic)
	enddo
	call iline(ix(1),iy(1),ix(n),iy(n),ic)
	end
