	subroutine fill_arrow(ixh,iyh,idxh,idyh,idx,idy,icolor,icfill)
c
c	ixh,iyh	: coordinates of the arrow top
c	idxh	      : length head
c     idyh		: width  head
c	idx	      : length
c     idy		: width
c

	integer*2  ix(7),iy(7)
	n=7
	iy(1)=iyh-idy/2
	iy(2)=iyh+idy/2
	iy(3)=iyh+idy/2
	iy(4)=iyh+idyh/2
	iy(5)=iyh
	iy(6)=iyh-idyh/2
	iy(7)=iyh-idy/2
	ix(1)=ixh-idx
	ix(2)=ixh-idx
	ix(3)=ixh-idxh
	ix(4)=ixh-idxh
	ix(5)=ixh
	ix(6)=ixh-idxh
	ix(7)=ixh-idxh

	call fillpoly(ix,iy,n,icolor,icfill)
	end
