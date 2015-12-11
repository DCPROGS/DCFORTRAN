	subroutine DCPIX1(x,y,n,ndisp,j1,inc,ipen,idat,first)
c Special version of DCPIX designed for a rolling display of long array
c At first call it draws the first frame x(i),y(i), i=1,ndisp in colour
c number=ipen (but does not overwrite any points that already have colours
c other than 0). It records which x(i) values are drawn by setting idat(i)=1
c (and idat(i)=0 for pixels that are already set so not drawn).  At subsequent
c calls the index in y(i) is incremented by INC so display starts at y(j1);
c the selective erasure of the previous display can be done by checking
c the idat(i) values, rather than using a call to GETPIX, thus saving time.
	real*4 x(ndisp),y(n)
	integer idat(ndisp)
	logical first
c
	do 1 i=1,ndisp
	 j=i+j1-1
c   Erase last trace (except for first call)
	 if(.not.first) then
	   jlast=j-inc
	   if(idat(i).ne.0) call SETPIX(x(i),y(jlast),0)	!erase old
	 endif
c
c Draw new trace
	 call GETPIX(x(i),y(j),ic)
	 if(ic.eq.0) then
	   call SETPIX(x(i),y(j),ipen)	!draw new
	   idat(i)=1
	 else
	   idat(i)=0	!pixel already set -don't draw
	 endif
1	continue
c
	first=.false.
	RETURN
	END


