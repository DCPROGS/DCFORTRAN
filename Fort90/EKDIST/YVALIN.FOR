	subroutine YVALIN(title,yval,nyval,pon,thist,obhist,
     &  ahist,idtype,ndim)
	real*4 yval(ndim)
	character*40 title
	logical thist,obhist,ahist
	logical pon,discprt
	common/dp/discprt
	common/sblk/sbin,shist,sres,sexp
c
      OPEN(unit=23,file='C:\YVAL.DAT',status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=4096)
c NB recl is in bytes 4096 bytes=1024 real, but Yval is 81920 now!
	read(23,rec=1) nytype,iopt,title,nyval
c
	if(nytype.ne.1) goto 2
c Data from SIMCHAN0 (nytype=1)
	nyr=0
	if(nyval.le.993) then
	  read(23,rec=1) nytype,iopt,title,nyval,(yval(i),i=1,nyval)
	else
	  read(23,rec=1) nytype,iopt,title,nyval,(yval(i),i=1,993)
	  nyr=nyval-993
	  nrec=1 + (nyr-1)/1024		!1024 reals per record
	  n1=994
	  do 1 j=2,nrec+1
	    n2=n1+1023
	    read(23,rec=j) (yval(i),i=n1,n2)
	    n1=n1+1024
1	  continue
	endif
      close(unit=23)
	goto 11
c
2	continue
c next nytype
c
11    CONTINUE
c Reverse sign?
c	do i=1,nyval
c	   yval(i)=-yval(i)
c	enddo
c
      print 12,title,nyval
      if(pon) write(7,12) title,nyval
      if(discprt) write(8,12) title,nyval
12	format(
     & ' Data for ',a40,/,
     & 1x,i8,' values read from C:\YVAL.DAT')
c Default is iopt=0 (time histo, idtype=50)
	thist=.true.
	idtype=50		!special value for YVAL.DAT data
	if(iopt.eq.1.or.iopt.eq.7.or.iopt.eq.13) then
		thist=.false.
		obhist=.true.
		idtype=51	!special value for YVAL.DAT data
	else if(iopt.eq.6.or.iopt.eq.12) then
		idtype=14		!as for regular burst Popen plot
		thist=.false.
		obhist=.false.          !Popen dist
		idtype=52	!special value for YVAL.DAT data
	else if(iopt.eq.3) then
		thist=.false.
		obhist=.false.	!Amplitude distribution
		ahist=.true.
		idtype=53	!special value for YVAL.DAT data
	endif
c
	RETURN
	end
