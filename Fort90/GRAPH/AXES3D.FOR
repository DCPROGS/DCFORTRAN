	subroutine AXES3D(xmin,xmax,deltx,ymin,ymax,delty,zval,
     & zmin,zmax,xz,yz)
c==================================================================
c Draws
c (a) an x, y plane (at vertical level=zval) with grid lines spaced
c	at delt,delty.  If deltx is negative, this is all omitted.
c (b) a vertical z axis line from zmin to zmax at coordinates xz,yz
	allocatable::faxis,xaxis,yaxis,xa,ya
	real*4 faxis(:,:),xaxis(:,:),yaxis(:,:),xa(:),ya(:)
	real faxisz(2,2),xaxisz(2,2),yaxisz(2,2),xaz(2),yaz(2)
	logical allocated
c
	if(deltx.lt.0.0) goto 1		!omit plane
c
	nx=1 + ifix(0.001+(xmax-xmin)/deltx)
	ny=1 + ifix(0.001+(ymax-ymin)/delty)
	ALLOCATE(faxis(ny,nx),xaxis(ny,nx),yaxis(ny,nx),xa(nx),ya(ny))
c
	do i=1,nx
	   xa(i)=xmin+float(i-1)*deltx
	enddo
	do i=1,ny
	   ya(i)=ymin+float(i-1)*delty
	enddo
c
	do i=1,ny
	   do j=1,nx
		faxis(i,j)=zval
	   enddo
	enddo
c DC addition -draw axes
      CALL PROJECT(faxis, xaxis, yaxis, xa, ya, nx, ny)
      CALL PLOT3D(xaxis, yaxis, nx, ny, 0)
c
c Draw vertical axis separately
1	continue
	xaz(1)=xz
	yaz(1)=yz
cc	xaz(2)=0.	!x and y must increase monotonically
cc	yaz(2)=0.
	xaz(2)=xz + 0.0001
	yaz(2)=yz + 0.0001
c
	faxisz(1,1)=zmin
	faxisz(2,1)=zmin
	faxisz(1,2)=zmax
	faxisz(2,2)=zmax
      CALL PROJECT(faxisz, xaxisz, yaxisz, xaz, yaz, 2, 2)
      CALL PLOT3D(xaxisz, yaxisz, 2, 2, 0) !!
	if(allocated(faxis)) then
	   DEALLOCATE(faxis,xaxis,yaxis,xa,ya)
	endif
	RETURN
	end


