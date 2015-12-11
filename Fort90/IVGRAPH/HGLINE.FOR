	subroutine hgline(inx,iny,icol)
	ix=int(inx*640/10000)
	iy=int((iny*480/7500))
	call hline(ix,ix+8,iy,icol)
	end
