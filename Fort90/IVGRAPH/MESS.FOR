	program mess
      call gino
	call vga
	call gsetcols(0)
	call errswi(-1)
	call brkswi(1)
	call chaswi(1)
	call grfmod (1)
	call harcha
	call mode(18)
	xlo=50.
	ylo=50.
	xhi=100.
	yhi=100.
	x1=40.
	y1=30.
	x2=100.
	y2=130.
	call lincol(12)
	call movto2(xlo,ylo)
	call linto2(xlo,yhi)
	call linto2(xhi,yhi)
	call linto2(xhi,ylo)
	call linto2(xlo,ylo)

	call lincol(2)
	call movto2(x1,y1)
	call linto2(x2,y2)
	call cliplin(x1,y1,x2,y2,xlo,ylo,xhi,yhi)
	call lincol(12)
	call movto2(x1,y1)
	call linto2(x2,y2)
	print*,x1,y1,x2,y2
	x1=120.
	y1=80.
	x2=280.
	y2=130.
	call cliplin(x1,y1,x2,y2,xlo,ylo,xhi,yhi)
	print*,x1,y1,x2,y2
	x1=120.
	y1=80.
	x2=40.
	y2=90.
	call cliplin(x1,y1,x2,y2,xlo,ylo,xhi,yhi)
	print*,x1,y1,x2,y2
	x1=80.
	y1=120.
	x2=280.
	y2=170.
	call cliplin(x1,y1,x2,y2,xlo,ylo,xhi,yhi)
	print*,x1,y1,x2,y2
	pause
	call mode(3)
	end
