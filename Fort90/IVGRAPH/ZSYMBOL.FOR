	subroutine ZSYMBOL(ix,iy,isym,isizex,isizey,icolor,icfill)
c To plot symbol centred at ix,iy (screen coord), and recentre
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
c ISYM=10 line
c ISYM=11 frame
c ISYM=12 dframe
c ISYM > 100	no symbol
c
	integer*2 x1(20),y1(20)		!to hold coord of vertices of polygon
	if(isym.gt.100) RETURN
	if(isym.eq.0) then
	   call wpixel(ix,iy,icolor)		!draws 1 pixel?
	   goto 99
	endif
	start=0.	!angle (starting from north) for 1st point
	goto (1,2,3,4,5,6,7,8,9,10,11,12) iabs(isym)
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
	call hline(ix-isizex,ix+isizex,iy,icolor)
	call vline(ix,iy-isizey,iy+isizey,icolor)
	goto 99
9     continue
	call iline(ix-isizex,iy-isizey,ix+isizex,iy+isizey,icolor)
	call iline(ix-isizex,iy+isizey,ix+isizex,iy-isizey,icolor)
	goto 99
10    continue
	call hline(ix-isizex,ix+isizey,iy,icolor)
	goto 99
11    continue
	call drawframe(ix-isizex,iy-isizey,ix+isizex,iy+isizey,icolor)
	goto 99
12    continue
	call drawframe(ix-isizex-2,iy-isizey-2,ix+isizex+2,iy+isizey+2,
     &		icolor)
	call drawframe(ix-isizex-1,iy-isizey-1,ix+isizex+1,iy+isizey+1,
     &		   icolor)
	goto 99
c
50	continue
c Calc coord of vertices
	dang=360./float(nvert)
	dang=3.1415926536*(dang)/180.	!radians
	s=3.1415926536*(start)/180.	!radians
	do 51 i=1,nvert
	ang=float(i-1)*dang
	x1(i)=ix+float(isizex)*sin(ang+s)
51	y1(i)=iy+float(isizey)*cos(ang+s)
	call drawpoly(x1,y1,nvert,icol)
	if(isym.lt.0) goto 60
c Draw hollow symbol
	goto 99
c Draw filled symbol
60	continue
	call fillpoly(x1,y1,nvert,icolor,icfill)
99	continue
	RETURN
	end


