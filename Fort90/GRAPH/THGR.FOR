	print 1
1	format(' ix0,iy0 = ')
	read 2,ix0,iy0
2	format(2i8)
	call INIPLT(0,.false.,1.0)
1081	call VIEWPORT(0,10000,0,7500)
	ixlo=2000
	ixhi=8000
	iylo=2000
	iyhi=5000
c	call GRAPHBOUNDARY(ixlo,ixhi,iylo,iyhi)
	call GRAPHBOUNDARY(0,10000,0,7000)
	call SCALE(0.,1.,0.,1.)
	call MOVETO(0.,0.,0,0)
	call MOVETO(0.,1.,1,0)
	call MOVETO(1.,1.,1,0)
	call MOVETO(1.,0.,1,0)
	call MOVETO(0.,0.,1,0)
c	call COLBAR(0,ix0,iy0,ixlo,ixhi,iylo,iyhi)
	pause
	call ERASCR
c	call COLBAR(1,ix0,iy0,ixlo,ixhi,iylo,iyhi)
	pause
	call ENDPLT()
	end

