C************************************************************************
	subroutine rdframe(ix1,iy1,ix2,iy2,arr1,arr2,arr3,arr4,nh,nv)
c	read pixel values before drawing frame
c	works together with drawframe

	integer*1	arr1(nh),arr2(nh),arr3(nv),arr4(nv)

	call rdgwin1(ix1,iy1,ix2,iy1,arr1)
	call rdgwin1(ix1,iy2,ix2,iy2,arr2)
	call rdgwin1(ix1,iy1,ix1,iy2,arr3)
	call rdgwin1(ix2,iy1,ix2,iy2,arr4)
	end


C************************************************************************
