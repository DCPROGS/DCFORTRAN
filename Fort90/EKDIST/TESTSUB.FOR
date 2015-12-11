	PROGRAM testsub
c Can array be de-alloc, re-alloc inside a subroutine  -NO!
	allocatable::imat
	integer imat(:,:)
c
	n1=2
	n2=3
	allocate(imat(n1,n2))
	do i=1,n1
	   do j=1,n2
		imat(i,j)=i*j
	   enddo
	enddo
c
	call SUB(imat,n1,n2)
	END

	subroutine SUB(imat,n1,n2)
	allocatable::imat
c	integer imat(n1,n2)
	integer imat(:,:)
c
	n1=3
	n2=2
	DEALLOCATE(imat)
	allocate(imat(n1,n2))
	do i=1,n1
	   do j=1,n2
		imat(i,j)=i+j
	   enddo
	enddo
	RETURN
	end
