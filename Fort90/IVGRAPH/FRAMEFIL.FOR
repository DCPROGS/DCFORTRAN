	subroutine FRAMEFILL(ix,iy,ixf,iyf,icolor,icfill)

	call fillwin(ix,iy,ixf,iyf,icfill)
	call hline(ix+2,ixf-2,iy+2,icolor)
	call hline(ix+4,ixf-4,iy+4,icolor)
	call hline(ix+4,ixf-4,iyf-4,icolor)
	call hline(ix+2,ixf-2,iyf-2,icolor)
	call vline(ix+2,iy+2,iyf-2,icolor)
	call vline(ix+4,iy+4,iyf-4,icolor)
	call vline(ixf-4,iy+4,iyf-4,icolor)
	call vline(ixf-2,iy+2,iyf-2,icolor)

	end
