	subroutine hgtopix(ixhg,iyhg,ixviewlo,iyviewlo,ixpix,iypix,flag)

c 	subroutine to convert hgraph  coordinates in pixels: 
c     1.when you want to calculate graphboundary coordinates , flag=1
c	2.when you want to calculate viewport coordinates or to convert absolute
c	  hgraph coordinates into pixels, flag=0


	ixpix=(ixhg+ixviewlo*flag)*0.064
	iypix=(iyhg+iyviewlo*flag)*0.064

	end
