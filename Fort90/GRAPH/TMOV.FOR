	call INIPLT(0,.false.,1.0)
	xmin=-1.
	xmax=1.
	ymin=-1.
	ymax=1.
	call SUB1(xmin,xmax,ymin,ymax)
	pause
 	call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	end

	subroutine SUB1(xmin,xmax,ymin,ymax)
	call SUB2(xmin,xmax,ymin,ymax)
	return
	end

	subroutine SUB2(xmin,xmax,ymin,ymax)
	call SCALE(xmin,xmax,ymin,ymax)
	call COLTYP1(11)
	call MOVETO(xmin,ymin,0,0)
	call MOVETO(xmin,ymax,1,0)
	call MOVETO(xmax,ymax,1,0)
	call MOVETO(xmax,ymin,1,0)
	call MOVETO(xmin,ymin,1,0)
	return
	end


