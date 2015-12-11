	subroutine pixtogb(ixgb,iygb,ixviewlo,iyviewlo,ixpix,iypix)
c subroutine to convert pixels into graphboundary coord taking into account
c the vieport
	x=float(ixpix)/0.0639
	y=float(iypix)/0.0639
	ixgb=ifix(x)-ixviewlo
	iygb=ifix(y)-iyviewlo
	end
