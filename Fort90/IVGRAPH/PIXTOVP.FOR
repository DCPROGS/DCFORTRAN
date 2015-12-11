	subroutine pixtovp(ixpix,iypix,ixhg,iyhg)

c subroutine to convert pixels to absolute hg coordinates

	ixhg=ixpix*15.625
	iyhg=iypix*15.625

	end
