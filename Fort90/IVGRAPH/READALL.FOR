	subroutine readall(xlo,ylo,xhi,yhi,dim,array2,n,max)
	integer*4	xlo(n),ylo(n),xhi(n),yhi(n),dim(n)
	integer*1	array2(n,max),array1(:)
	allocatable array1

	do i=1,n
		allocate(array1(dim(i)))
		call rdgwin1(xlo(i),ylo(i),xhi(i),yhi(i),array1)
		do j=1,dim(i)
		    array2(i,j)=array1(j)
		enddo
		deallocate(array1)
	enddo
	end
