	subroutine gbtopix(ixgb,iygb,ixviewlo,iyviewlo,ixpix,iypix)
c subroutine to convert graphboundary coord in pixels taking into account
c the vieport

	xpix=float(ixgb+ixviewlo)*0.0639
	ypix=float(iygb+iyviewlo)*0.0639
	ixpix=ifixr(xpix)
	iypix=ifixr(ypix)
	end
