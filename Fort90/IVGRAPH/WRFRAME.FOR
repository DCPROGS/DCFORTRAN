C************************************************************************
	subroutine wrframe(ix1,iy1,ix2,iy2,arr1,arr2,arr3,arr4,nh,nv)

c     restore array of pixels

	integer*1	arr1(nh),arr2(nh),arr3(nv),arr4(nv)

	call wrgwin1(ix1,iy1,ix2,iy1,arr1)
	call wrgwin1(ix1,iy2,ix2,iy2,arr2)
	call wrgwin1(ix1,iy1,ix1,iy2,arr3)
	call wrgwin1(ix2,iy1,ix2,iy2,arr4)
	end
C************************************************************************
