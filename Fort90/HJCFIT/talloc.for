	program TALLOC
c to test time taken by allocating arrays
	real*8 a1(100,100,100)
c	allocatable::a2
c	real*8 a2(:,:,:)
	character*11 ctime
c
	call TIME(ctime)
	print 1,ctime
1	format(/,' SUB1 started at ',a11)

	n=1 000 000
	do i=1,n
	   call SUB1(a1)
	enddo
	call TIME(ctime)
	print 2,ctime
2	format(/,' SUB1 ended at ',a11)
	do i=1,n
	   call SUB2(a1)
	enddo
	call TIME(ctime)
	print 3,ctime
3	format(/,' SUB2 ended at ',a11)
	end

	subroutine SUB1(a1)
	real*8 a1(100,100,100)
	RETURN
	end

	subroutine SUB2(a1)
	real*8 a1(100,100,100)
	allocatable::a2
	real*8 a2(:,:,:)
	allocate(a2(100,100,100))
	deallocate(a2)
	RETURN
	end
