	program TASC1
c To test ASCIODC
	real*4 Xnum1(20480)
	character header*79,datfil*33
	logical read
	allocatable::Xnum
	real*4 Xnum(:,:)
c
	read=.true.
	ndim=20480
	ioff=0
c	ilinhead=5	!5 lines of header before data
	ilinhead=-1
	call ASCIODC(read,ioff,ilinhead,ncols,nrows,ntot,Xnum1,ndim,
     &  datfil,header)
	pause
c
	ioff=0
	ilinhead=-1
	call ASCIO1(ioff,ilinhead,ncols,nrows,len,
     & ndev,datfil,header)
	nd1=nrows
	nd2=ncols
	ALLOCATE(xnum(nd1,nd2))
	call ASCIO2(ioff,ncols,nrows,len,
     & datfil,xnum,nd1,nd2)
	call ATYPE2(xnum,'       ',.false.,nrows,ncols,nd1,nd2)
c
	pause
	DEALLOCATE(xnum)
	end


