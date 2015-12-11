	subroutine pixtowd(ixpix,iypix,xwd,ywd)

c	subroutine to convert pixels to world coordinates

	x=float(ixpix)/0.0639
	y=float(iypix)/0.0639
	xwd=xworld(ifixr(x))
	ywd=yworld(ifixr(y))

	end
