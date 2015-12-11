	program TREAD
c To test read_ere and read_evl
	character*33 pfile(20,10)	!path names for .EDE files
	integer nfile(10),nintt(10)
	integer nval(20,10),irecs(20,10)
	real*4 calfacs2(20,10),ffilt(10)
c
	nset=1
	nfile(1)=1
	pfile(1,1)='test1.ede'
	call READ_EDE(n,kfile,pfile,nset,nfile,nval,irecs,
     &	calfacs2,ffilt,nintt,ntot,nmax)
	pause
	pfile(1,1)='1level.evl'
	call READ_EVL(n,kfile,pfile,nset,nfile,nval,irecs,
     &	calfacs2,ffilt,nintt,ntot,nmax)
	end

