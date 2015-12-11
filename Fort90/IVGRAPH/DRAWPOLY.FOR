	subroutine drawpoly(ix,iy,n,ic)
	integer	ix(n),iy(n)
c Uses assembler routine ILINE
	do i=1,n-1
		call iline(ix(i),iy(i),ix(i+1),iy(i+1),ic)
	enddo
	call iline(ix(n),iy(n),ix(1),iy(1),ic)
	end
