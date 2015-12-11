	Program test
	allocatable jmat
	integer jmat(:,:)
	integer n(3)

	allocate(jmat(2,3))
	jmat(1,1)=1
	jmat(1,2)=2
	jmat(1,3)=3
	jmat(2,1)=4
	jmat(2,2)=5
	jmat(2,3)=6
	n(1)=2
	n(2)=2
	n(3)=2
	OPEN(unit=19,file='test.dat',status='UNKNOWN',
     & access='TRANSPARENT')		!new .ini
c======
c Versions where number if i values to be read depends on j
c First version OK, 2nd gives allocatable array bound error
	write(unit=19,rec=1) ((jmat(i,j),i=1,n(j)),j=1,3)
c*	write(unit=19,rec=1) ((jmat(i,j),j=1,3),i=1,n(j))
c========
c==	write(unit=19,rec=1) ((jmat(i,j),i=1,2),j=1,3)
c NB if write as above but read(unit=19) ((jmat(i,j),j=1,3),i=1,2) then
c  read back is wrong
c==	write(unit=19,rec=1) ((jmat(i,j),j=1,3),i=1,2)
	close(unit=19)
	jmat(1,1)=0
	jmat(1,2)=0
	jmat(1,3)=0
	jmat(2,1)=0
	jmat(2,2)=0
	jmat(2,3)=0
	i=20
	j=30
	OPEN(unit=19,file='test.dat',status='UNKNOWN',
     & access='TRANSPARENT')		!new .ini
c in next two lines
c only the first read is correct when write(unit=19,rec=1) ((jmat(i,j),j=1,3),i=1,2)
c only the 2nd read is correct when write(unit=19,rec=1) ((jmat(i,j),i=1,2),j=1,3)
	read(unit=19,rec=1) ((jmat(i,j),j=1,3),i=1,2)
	read(unit=19,rec=1) ((jmat(i,j),i=1,2),j=1,3)
c Version with n(j)
	read(unit=19,rec=1) ((jmat(i,j),i=1,n(j)),j=1,3)
	close(unit=19)
	end

