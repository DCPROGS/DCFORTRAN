	integer inta(100)
	integer*1 idum(1)
c
	idum(1)=0
	n=100
c	n=256
	do i=1,n
	   inta(i)=i
	enddo
c
      OPEN(unit=12,file='test.dat',status='UNKNOWN',
     & access='TRANSPARENT')
	INQUIRE(file='test.dat',flen=nlen)
	write(12,rec=1025) inta
	INQUIRE(file='test.dat',flen=nlen)
c nlen=1424 here
c if file is to be reopened with 1024 byte records, then must make up its
c length to bultiple of 1024 bytes
	write(12,rec=nlen+1) (idum(1),i=nlen+1,2048)
	INQUIRE(file='test.dat',flen=nlen)
c nlen=2048 here
	CLOSE(unit=12)
c
	do i=1,n
	   inta(i)=0
	enddo
c NB if only 100 integers written get error here, as compiler tries to read
c WHOLE of record 2 (bytes 1025-2048), and file is too short.
c With above fill so nlen=2048, record 2 read OK in next bit.
      OPEN(unit=12,file='test.dat',status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=1024)
	read(12,rec=2) inta
	CLOSE(unit=12)
c
	do i=1,n
	   inta(i)=0
	enddo
      OPEN(unit=12,file='test.dat',status='UNKNOWN',
     & access='TRANSPARENT')
	read(12,rec=1025) inta
	CLOSE(unit=12)
c
	end
