	subroutine SIMWRT(tint,iampl,kt,ndim)
c To write enormous simulation output to special file
c
	real*4 tint(ndim)
	integer*2 iampl(ndim)
c
      OPEN(unit=12,file='C:\SIMDAT.DAT',status='UNKNOWN',
     & ACCESS='DIRECT',FORM='UNFORMATTED',RECL=8192)
C NOTE: RECSIZE=2048 real, =4096 integers, =8192 bytes
c
c write TINT
	nrect=1 + (kt-1)/2048
	n=1
	do 1 irec=1,nrect
	m=n+2047
	write(12,rec=irec) (TINT(i),i=n,m)
	n=n+2048
1	continue
c
	nreci=1 + (kt-1)/4096
	n=1
	do 2 irec=nrect+1,nrect+nreci
	m=n+4095
	write(12,rec=irec) (IAMPL(i),i=n,m)
	n=n+4096
2	continue
c
	close(unit=12)
	RETURN
	END

