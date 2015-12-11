  	subroutine DFRAME(xlo,xhi,ylo,yhi,linetype)
c To draw box (was FRAME.FOR but this name clashes with HGRAPH)
c	linetype=0		!continuous line
	call BROKEN(linetype)
	call MOVTO2(xlo,ylo)
	call LINTO2(xhi,ylo)
	call LINTO2(xhi,yhi)
	call LINTO2(xlo,yhi)
	call LINTO2(xlo,ylo)
	end

