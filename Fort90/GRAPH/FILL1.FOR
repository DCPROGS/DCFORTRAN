	subroutine FILL1(x,y,n,icol,line,jpat,inv)
	real x(n),y(n)
c Version of Hgraph FILL subroutine that uses Lahey colour numbering
c convention for ICOL in the call (see COLTYP1 for numbering)
c
c NB icol+icol1=15, except for black
c	icol1=15-icol
c	if(icol1.eq.15) icol1=-1	!black (for icol=0)
C	call FILL(x,y,n,icol1,line,idelt,itheta)
	call linvis(1)
	call lincols(icol,idev)
c	do i=1,n
c	   call graspa(x(i),y(i),xs,ys)
c	   x(i)=xs
c	   y(i)=ys
c	enddo
	call POFTO2(jpat,line,inv,x,y,n)
	RETURN
	end


