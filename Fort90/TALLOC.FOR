	program TALLOC
	real*4 a(:,:),f(:)
	ALLOCATABLE :: a,f
c
	nr=1000           !nr=2000 gives ierr=89 even if stack inc to 1Mb!!
	nc=2048           ! with -stack 1000000 in .LNK file
	ALLOCATE(stat=ierr,a(1:nr,1:nc))
	print*,' ierr = ',ierr
	do 1 i=1,nr
	do 1 j=1,nc
	a(i,j)=float(i+j)
1	continue
	do 3 i=1,nr
	do 3 j=1,nc
c	print 2,a(i,j)
2	format(f5.1)
3	continue
	call sub(a,nr,nc)
c
	call sub1(f,nf)
	do i=1,5
	 print* f(i)
	enddo
	end

	subroutine sub(b,nr,nc)
	real*4 b(nr,nc)
	do 3 i=1,nr
	do 3 j=1,nc
c	print 2,b(i,j)
2	format(f5.1)
3	continue
c
	return
	end

	subroutine SUB1(f,nf)
	real*4 f(:)
	ALLOCATABLE :: f

	nf=5
	ALLOCATE(stat=ierr,f(1:nf))
	print*,' ierr = ',ierr
	do i=1,5
	 f(i)=5.-float(i)
	enddo
	RETURN
	end

