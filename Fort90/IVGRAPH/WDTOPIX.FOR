	subroutine wdtopix(xwd,ywd,ixpix,iypix)

c	subroutine to convert world coordinates to pixels

	xpix=float(iscrx(xwd))*0.0639
	ypix=float(iscry(ywd))*0.0639
	ixpix=ifixr(xpix)
	iypix=ifixr(ypix)
	end
