	program TREAD
c To test writing in records, followed by reading as transparent
	integer*2 idata(128),idata1(32)
c
	do i=1,128
	   idata(i)=i
	enddo
c
      OPEN(unit=14,file='test.dat',status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=64)	!64 bytes=32 int*2
	n=1
	nrec=4
	do j=1,nrec
	   m=n+31
	   write(14,rec=j) (idata(i),i=n,m)
	   n=n+32
	enddo
	CLOSE(unit=14)
c
	print 1
1	format(' Read section number = ')
	read*,isec
	nbuf=32
	in1=1 + (isec-1)*nbuf	!index in idata() of 1st point to read
	irec=1+2*(in1-1)		!byte # to read this number
	OPEN(unit=14,file='test.dat',status='UNKNOWN',
     &	access='TRANSPARENT')
	   read(14,rec=irec) (idata1(i),i=1,32)
	CLOSE(unit=14)
c
	print 2,isec,irec
2	format(' isec,irec = ',2i8)
	do i=1,32
	   print 3,i,idata1(i)
3	   format(2i8)
	enddo
c
	end

