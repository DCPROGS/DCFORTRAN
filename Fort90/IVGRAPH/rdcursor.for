C************************************************************************
	subroutine rdcursor(ix,iy,ix1,iy1,ix2,iy2,arr1,arr2,nh,nv,itype)
c	read pixel values before drawing frame
c	works together with drawframe

	integer*1	arr1(nh),arr2(nv)
	if(itype.eq.0) then
		call rdgwin1(ix1,iy,ix2,iy,arr1)
		call rdgwin1(ix,iy1,ix,iy2,arr2)
	endif
	end


C************************************************************************
