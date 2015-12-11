	subroutine GRAPHBOUNDARY(xlo,xhi,ylo,yhi)
	call axipos(1,xlo,ylo,xhi-xlo,1)
	call axipos(1,xlo,ylo,yhi-ylo,2)
	end
