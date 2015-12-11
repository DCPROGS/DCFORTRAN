	Program tQwrite
	allocatable Qmat
	real*8 Qmat(:,:)
	integer irate(4),jrate(4)

	allocate(Qmat(3,3))
	do i=1,3
	   do j=1,3
		qmat(i,j)=0.
	   enddo
	enddo
	qmat(1,2)=12.
	qmat(2,1)=21.
	qmat(2,3)=23.
	qmat(3,2)=32.
	nrconst=4
	irate(1)=1
	jrate(1)=2
	irate(2)=2
	jrate(2)=1
	irate(3)=2
	jrate(3)=3
	irate(4)=3
	jrate(4)=2
	OPEN(unit=19,file='test.dat',status='UNKNOWN',
     & access='TRANSPARENT')
c======
c Versions where number if i values to be read depends on j
c First version OK, 2nd gives allocatable array bound error
	n=0
	write(unit=19,rec=1) (qmat(irate(i),jrate(i)),i=1,nrconst),
     & (irate(i),i=1,n)
	close(unit=19)
c
	do i=1,3
	   do j=1,3
		qmat(i,j)=0.
	   enddo
	enddo
	OPEN(unit=19,file='test.dat',status='UNKNOWN',
     & access='TRANSPARENT')		!new .ini
	read(unit=19,rec=1) (qmat(irate(i),jrate(i)),i=1,nrconst),
     & (irate(i),i=1,n)
	close(unit=19)
	end

