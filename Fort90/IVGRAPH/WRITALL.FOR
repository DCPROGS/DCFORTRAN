	subroutine writall(xlo,ylo,xhi,yhi,dim,array2,n,max)
	integer*4	xlo(n),ylo(n),xhi(n),yhi(n),dim(n)
	integer*1	array2(n,max),array1(:)
	allocatable array1

	do i=1,n
		allocate(array1(dim(i)))
		do j=1,dim(i)
		    array1(j)=array2(i,j)
		enddo
		call wrgwin1(xlo(i),ylo(i),xhi(i),yhi(i),array1)
		deallocate(array1)
	enddo
	end
