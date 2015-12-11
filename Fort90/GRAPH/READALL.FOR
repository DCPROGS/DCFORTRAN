	subroutine readall(ixlo,iylo,ixhi,iyhi,idim,array2,n,max)
	integer*4	ixlo(n),iylo(n),ixhi(n),iyhi(n),idim(n)
	integer*1	array2(n,max),array1(:)
	allocatable :: array1

	do i=1,n
		allocate(array1(idim(i)))
		call rdgwin1(ixlo(i),iylo(i),ixhi(i),iyhi(i),array1)
		do j=1,idim(i)
		    array2(i,j)=array1(j)
		enddo
		deallocate(array1)
	enddo
	end
