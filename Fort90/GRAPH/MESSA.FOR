	program messa

	xlo=100.
	xhi=300.
	ylo=100.
	yhi=300.
	x1=150.
	y1=150.
	x2=250.
	y2=250.
	call clipping(xlo,ylo,xhi,yhi,x1,y1,x2,y2,iflag)
	print*,x1,y1,x2,y2
	x1=100.
	y1=300.
	x2=100.
	y2=300.
	call clipping(xlo,ylo,xhi,yhi,x1,y1,x2,y2,iflag)
	print*,x1,y1,x2,y2,iflag
	x1=150.
	y1=50.
	x2=250.
	y2=350.
	call clipping(xlo,ylo,xhi,yhi,x1,y1,x2,y2,iflag)
	print*,x1,y1,x2,y2,iflag
	x1=100.
	y1=50.
	x2=50.
	y2=100.
	call clipping(xlo,ylo,xhi,yhi,x1,y1,x2,y2,iflag)
	print*,x1,y1,x2,y2,iflag
	pause
	end
