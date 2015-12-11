	subroutine jSYMBOL(x,y,isym,csize,icol,idev)
c To plot symbol centred at x,y (world coord), and recentre
c GINO version
c Modif 04/06/95 06:04pm so out of range values corrected to something
c sensible.
c Modified for colour 02/17/93 01:46pm: icol added to arguments
c and FILL1, which uses Lahey colour numbering rather than Hgraph,
c substituted for FILL.
c ISYM=positive for hollow symbol; neg for filled symbol (where applic)
c ISYM=0  point (now one pixel (I think); was a small pentagon)
c ISYM=1  triangle (point up)
c ISYM=2  triangle (point down)
c ISYM=3  square
c ISYM=4  diamond
c ISYM=5  pentagon
c ISYM=6  hexagon
c ISYM=7  circle (20 side poly)
c ISYM=8  +
c ISYM=9  x
c ISYM > 99		no symbol
c
	real x1(20),y1(20)		!to hold coord of vertices of polygon
c	logical caplock
c
	if(isym.gt.99) RETURN
c
	call broken(0)	!solid lines for all
	call linvis(1)
	call lincols(icol,idev)
	call MOVTO2(x,y)		!go to centre point
c Calc size in world coord
	call getsize(csize,w,h)
	sx=w/2
	sy=h/2
	if(isym.eq.0) then
	   call linTO2(x,y+0.1*sy)		!draws 1 pixel?
	   goto 99
	endif
c	if(isym.eq.0) goto 5	!small pentagon
	start=0.	!angle (starting from north) for 1st point
	if(isym.lt.-7) then
	   i=iabs(isym)
	   i=1+mod(i-1,7)		!so 8,9,..->1,2...
	   isym=-i
	else if(isym.gt.9) then
	   isym=1+mod(isym-1,9)		!so 9,10,...->1,2...
	endif
	goto (1,2,3,4,5,6,7,8,9) iabs(isym)
c
1     continue
	nvert=3
	goto 50
2     continue
	nvert=3
	start=180.
	goto 50
3     continue
	nvert=4
	start=45.
	goto 50
4     continue
	nvert=4
	goto 50
5	continue
	nvert=5
	goto 50
6     continue
	nvert=6
	goto 50
7     continue
	nvert=20
	goto 50
8     continue
	call linvis(1)
	call MOVTO2(x+sx,y)
	call linTO2(x-sx,y)
	call MOVTO2(x,y-sy)
	call linTO2(x,y+sy)
	call MOVTO2(x,y)
	goto 99
9     continue
	call linvis(1)
	call MOVTO2(x-sx,y-sy)
	call linTO2(x+sx,y+sy)
	call MOVTO2(x-sx,y+sy)
	call linTO2(x+sx,y-sy)
	call MOVTO2(x,y)
	goto 99
c
50	continue
c Calc coord of vertices
	dang=360./float(nvert)
	dang=3.1415926536*(dang)/180.	!radians
	s=3.1415926536*(start)/180.	!radians
	do 51 i=1,nvert
	ang=float(i-1)*dang
	x1(i)=x+sx*sin(ang+s)
51	y1(i)=y+sy*cos(ang+s)
	if(isym.lt.0) goto 60
c Draw hollow symbol
	call MOVTO2(x1(1),y1(1))	!move to first vertex (dark)
	do 52 i=2,nvert
52	call linTO2(x1(i),y1(i))
	call linto2(x1(1),y1(1))	!move to first vertex (dark)
	goto 99
c Draw filled symbol
60	continue
	call movto2(x1(1),y1(1))
	call POFTO2(0,0,0,x1,y1,nvert)
c	call FILL1(x1,y1,nvert,icol,0,0,0)
c	call FILL(x1,y1,nvert,0,0,0,0)
99	continue
	call MOVTO2(x,y)		!back to centre
	call lincols(15,idev)		!bright white
	RETURN
	end


