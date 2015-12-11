C************************************************************************
	subroutine wrframe(ix,iy,ix1,iy1,ix2,iy2,arr1,arr2,nh,nv,itype)

c     restore array of pixels

	integer*1	arr1(nh),arr2(nv)
	if(itype.eq.0) then
		call wrgwin1(ix1,iy,ix2,iy,arr1)
		call wrgwin1(ix,iy1,ix,iy2,arr2)
	endif
	end
C************************************************************************
