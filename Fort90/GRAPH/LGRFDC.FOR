c LGRFDC.FOR
c This whole file makes up DC library for Lahey graphics LGRFDC.LIB
c It has names for subroutines that do not clash with Hgraph
c Colours:
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c

	subroutine INITLGRF
c Initialise common blocks for Lahey Graphics
	common/lscal/sx,sy,xoff,yoff		!for Lahey graphics
	common/lgrf/xpos,ypos,ipen		!ditto
	sx=1.0
	sy=1.0
	xoff=0.
	yoff=0.
	xpos=0.
	ypos=0.
	ipen=15	!bright white
	RETURN
	end

	subroutine SCALEL(xmin,xmax,ymin,ymax)
	common/lscal/sx,sy,xoff,yoff
c Equivalent of SCALE for Lahey screen graphics. But app no separate x,y
c scaling in Lahey, so calc factors sx,sy to use in calls below (or, quicker,
c use them to scale data BEFORE it is displayed so scaling multiplication does
c not need to be done at time display is drawn)  ADC data must be converted
c to REAL before display anyway.
c	This calculates scale factors such that the input xmin,... correspond
c to the whole plottable area of the screen. To leave margins around the
c graphics, simplest way is probably to call SCALEL with xmin,.. etc larger
c than actually occurs in data so margin left in which nothing is plotted.
c	Nominal scale x=0.0-11.0, y=0.0-8.5 but to show on Dell 425 must use
c range x=0.01-10.99, y=0.01-8.5.
	sx=(10.99-0.01)/(xmax-xmin)
	xoff=0.01-xmin*sx
	sy=(8.5-0.01)/(ymax-ymin)
	yoff=0.01-ymin*sy
	RETURN
	end

	subroutine MOVEL1(x,y)
c Lahey graphics move (unscaled)
	call PLOTL(x,y,3)
	return
	end

	subroutine DRAWL1(x,y)
c Lahey graphics draw (unscaled)
	call PLOTL(x,y,2)
	return
	end

	subroutine MOVEL(x,y)
c Lahey graphics move (scaled) with real arg
	common/lscal/sx,sy,xoff,yoff
	x1=x*sx + xoff
	y1=y*sy + yoff
	call PLOTL(x1,y1,3)
	return
	end

	subroutine DRAWL(x,y)
c Lahey graphics draw (scaled) with real arg
	common/lscal/sx,sy,xoff,yoff
	x1=x*sx + xoff
	y1=y*sy + yoff
	call PLOTL(x1,y1,2)
	return
	end

	subroutine DRAWLc(x,y,icol)
	common/lscal/sx,sy,xoff,yoff
c Lahey graphics draw (scaled): version that calls PLOTL with ic=-icol, so
c tests each pixel before drawing it, and does NOT alter any pixel unless it has
c the colour specified by ICOL in the call. E.g. to draw a line that doesn't
c cover any point that is already lit up call DRAWL2(x,y,0). And to delete
c a line already drawn by this routine with colour=14 say, set ipen=0
c and call DRAWL2(x,y,14) so only yellow points are deleted
	x1=x*sx + xoff
	y1=y*sy + yoff
	call PLOTL(x1,y1,-icol)
	return
	end

	subroutine IMOVEL(ix,iy)
c Lahey graphics move (scaled) with integer*2 arg
	integer*2 ix,iy
	common/lscal/sx,sy,xoff,yoff
	x=float(ix)*sx + xoff
	y=float(iy)*sy + yoff
	call PLOTL(x,y,3)
	return
	end


	subroutine IDRAWL(ix,iy)
c Lahey graphics draw (scaled) with integer*2 arg
	integer*2 ix,iy
	common/lscal/sx,sy,xoff,yoff
	x=float(ix)*sx + xoff
	y=float(iy)*sy + yoff
	call PLOTL(x,y,2)
	return
	end

	subroutine PLOTL(x,y,ic)
c Homemade routine to avoid use of call PLOT in Lahey graphics (which
c clashes with Hgraph), by using SETPIX. Follows Lahey convention; ic=2 for
c DRAW, ic=3 for MOVE.
c Modified 01/02/91 06:05pm so that if called with IC=0 to -15 then tests each
c pixel before drawing it, and does NOT alter any pixel unless it has
c the colour specified by ICOL in the call. E.g. to draw a line that doesn't
c cover any point that is already lit up call DRAWL2(x,y,0). And to delete
c a line already drawn by this routine with colour=14 say, set ipen=0
c and call DRAWL2(x,y,14) so only yellow points are deleted
c======needs fixing for lines that are not vertical or horizontal because
c======pixels are in slightly different positions according to whether
c======line drawn up or down eg fix so always drawn in same direction eg
c=======always draw from lower left (even if this is x,y rather than xpos,ypos)
c
c Draws from last position to x,y, which then becomes the
c current position, so need to keep xpos,ypos in common.  Also this
c routine has no way to know colour (icol in SETPIX presumably has
c precedence over NEWPEN call, so keep ICOL in common too.
c In X direction have 640/11=58.18181818 pixels/screen unit
c i.e. 0.0171875 screen units/pixel
c In Y direction have 480/8.5=56.470588235 pixels/s.u., ie 0.01770833 s.u./pixel
c To light every pixel along a line need to move in steps not bigger than this.
	logical coltest
	common/lgrf/xpos,ypos,ipen		!ditto
c
	if(ic.eq.3) then
	   xpos=x
	   ypos=y
	   RETURN
	endif
c
	dx=0.0171875
	dy=0.01770833
	y0=y		!define values so x,y,xpos,ypos not altered
	x0=x
	ypos0=ypos
	xpos0=xpos
c
	coltest=.false.
	if(ic.le.0) then
	   coltest=.true.
	   icol=-ic
	endif
c
	if(x.eq.xpos) then       !vertical
	   if(y.lt.ypos) dy=-dy
	   y1=ypos
	   n=ifixr(abs((y-ypos)/dy))
	   do 2 i=1,n
	   if(coltest) then
		call GETPIX(xpos,y1,icol1)
		call RPIXEL(X,Y,COLOR)
	      if(icol1.gt.15) icol1=0	!sometimes get crazy values!??
		if(icol1.ne.icol) goto 21
	   endif
	   call WPIXEL(X,Y,COLOR)    -draw a pixel at coord. x,y in color
	   call SETPIX(xpos,y1,ipen)
21	   y1=y1+dy
2	   continue
	   ypos=y
	   goto 9
	endif
	if(y.eq.ypos) then       !horizontal
	   if(x.lt.xpos) dx=-dx
	   x1=xpos
	   n=ifixr(abs((x-xpos)/dx))
	   do 3 i=1,n
	   if(coltest) then
		call GETPIX(x1,ypos,icol1)
		call RPIXEL(X,Y,COLOR)
	      if(icol1.gt.15) icol1=0	!sometimes get crazy values!??
		if(icol1.ne.icol) goto 31
	   endif
	   call SETPIX(x1,ypos,ipen)
	   call WPIXEL(X,Y,COLOR)    -draw a pixel at coord. x,y in color
31	   x1=x1+dx
3	   continue
	   xpos=x
	   goto 9
	endif
c Should set dx to smaller value if line is near vertical
c Draw line from xpos0,ypos0 to x0,y0
	b=(y0-ypos0)/(x0-xpos0)
	x1=xpos0
	y1=ypos0
	if(abs(b).lt.1.) then	!shallow so increment x
	      n=ifixr(abs((x0-xpos0)/dx))
		if(x0.lt.xpos0) dx=-dx		!so x1 goes from xpos to x
		do 4 i=1,n
	      if(coltest) then
		   call GETPIX(x1,y1,icol1)
		   call RPIXEL(X,Y,COLOR)
	         if(icol1.gt.15) icol1=0	!sometimes get crazy values!??
		   if(icol1.ne.icol) goto 41
		endif
		call SETPIX(x1,y1,ipen)
	      call WPIXEL(X,Y,COLOR)    -draw a pixel at coord. x,y in color
41		x1=x1+dx
		y1=ypos0+b*(x1-xpos0)
4		continue
		xpos=x		!reset current pos
		ypos=y
	else                         !steep so increment y
		n=ifixr(abs((y0-ypos0)/dy))
		if(y0.lt.ypos0) dy=-dy		!so y1 goes from ypos to y
		do 5 i=1,n
	      if(coltest) then
		   call GETPIX(x1,y1,icol1)
		   call RPIXEL(X,Y,COLOR)
	         if(icol1.gt.15) icol1=0	!sometimes get crazy values!??
		   if(icol1.ne.icol) goto 51
		endif
		call SETPIX(x1,y1,ipen)
51		y1=y1+dy
		x1=xpos0+(y1-ypos0)/b
5		continue
		xpos=x		!reset current pos
		ypos=y
	endif
c
9	if(coltest) then
	   call GETPIX(xpos0,ypos0,icol1)
	   call RPIXEL(X,Y,COLOR)
	   if(icol1.gt.15) icol1=0	!sometimes get crazy values!??
	   if(icol1.ne.icol) RETURN
	endif
	call SETPIX(xpos,ypos,ipen)	!last point
	call WPIXEL(X,Y,COLOR)    -draw a pixel at coord. x,y in color
	RETURN
	end

