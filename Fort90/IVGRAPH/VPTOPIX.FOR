	subroutine vptopix(ixhg,iyhg,ixpix,iypix)

c	subroutine to transform hg absolute device coordinates(0-10000;0-7500)
c 	in pixels;


	ixpix=ixhg*0.064
	iypix=iyhg*0.064

	end
