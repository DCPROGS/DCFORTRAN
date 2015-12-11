	subroutine DCPIX3(x,y,n,ndisp,j1,xlast,ylast,ndlast,icol,ibcol,
     &	idel,first)
c Another version of DCPIX designed for a rolling display of long array, with
c xlast,ylast in the call (in case points to be erased were from a different
c section, and therefore no longer available in x,y).
c 	At first call it draws the first frame x(i),y(i), i=1,ndisp in colour
c number=icol (but does not overwrite any points that already have colours
c other than ibcol=background colour). It records which x(i) values are
c drawn by setting idel(i)=1 (and idel(i)=0 for pixels that are already set
c so not drawn).
c	At subsequent calls the index points defined by xlast, ylast are first
c erased (if idel=1), then new x,y points drawn, and idel redefined
c ready for next call).
c 	At every call xlast, ylast and ndlast are defined ready for next call.
	real*4 x(ndisp),y(n)
	real*4 xlast(ndisp),ylast(ndisp)
	integer idel(ndisp)
	logical first
c
c Erase xlast,ylast (if not first display)
	if(.not.first) then
	   do i=1,ndisp
	    if(idel(i).ne.0) call SETPIX(xlast(i),ylast(i),ibcol)	!erase old
	   enddo
	 endif
c
c Draw new trace
	do i=1,ndisp
	 j=i+j1-1
	 xlast(i)=x(i)			!ready for next call
	 ylast(i)=y(j)			!ready for next call
	 call GETPIX(x(i),y(j),ic)
	 if(ic.ne.ibcol) then
	   call SETPIX(x(i),y(j),icol)	!draw new
	   idel(i)=1
	 else
	   idel(i)=0	!pixel already set -don't draw
	 endif
	enddo
c
	ndlast=ndisp		!ready for next call
	first=.false.
	RETURN
	END


