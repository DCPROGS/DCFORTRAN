	subroutine hgtopix(ixhg,iyhg,ixpix,iypix)

c	subroutine to transform hg absolute device coordinates(0-10000;0-7500)
c 	in pixels;


	xpix=float(ixhg)*0.0639
	ypix=float(iyhg)*0.0639
	ixpix=ifixr(xpix)
	iypix=ifixr(ypix)

	end
