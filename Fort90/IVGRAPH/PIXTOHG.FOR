	subroutine pixtohg(ixpix,iypix,ixhg,iyhg)

c 	subroutine to convert pixels in hg coordinates :
	x=float(ixpix)/0.0639
	y=float(iypix)/0.0639
	ixhg=ifixr(x)
	iyhg=ifixr(y)

	end
