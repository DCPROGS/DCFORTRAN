	program PCCONV
c	real*4 YVAL(2048,15)
	real*4 YVAL(2048,15),Yval1(30720)		!equivalenced
c	real*4 YVAL(200000)
	character ndev*2,ndev1*2,datfil1*15
	character*40 title
	logical discprt
	common/dp/discprt
c
	EQUIVALENCE (YVAL,Yval1)
c To test reading of array output by MathCad (for use in CJFIT1)
	ndev1='C:'		!default = C
	DATFIL1=ndev1//'\YVAL.DAT'
c
	discprt=.false.
1	ndev='A:'
	call READPC(ndev,nsamp,irate,Yval1,ierr)
	if(ierr.ne.0) goto 1
c	print 2,nsamp
c2	format(' nsamp = ',i8)
	do 3 i=1,10
3	print 5,i,yval1(i)
	do 4 i=nsamp-9,nsamp
4	print 5,i,yval1(i)
5	format(i8,2x,g13.6)
c
c Write yval1 to disk
c
	nytype=1
	iopt=3		!for amplitude distribution in ekdist
c Enter title for experiment
c
	call TITENT0(' Enter experiment title:',title,40,.true.)
c
      OPEN(unit=23,file=DATFIL1,status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=4096)
c NB recl is in bytes 4096 bytes=1024 real, but
c	Yval1 is 20480 now!
	write(23,rec=1) nytype,iopt,title,nsamp
c
c	if(nytype.ne.1) goto 2
c Data from SIMCHAN0 (nytype=1) or from pclamp (Mark's amplitudes)
	nyr=0
	if(nsamp.le.993) then
	  write(23,rec=1) nytype,iopt,title,nsamp,(yval1(i),i=1,nsamp)
	else
	  write(23,rec=1) nytype,iopt,title,nsamp,(yval1(i),i=1,993)
	  nyr=nsamp-993
	  nrec=1 + (nyr-1)/1024		!1024 reals per record
	  n1=994
	  do 22 j=2,nrec+1
	    n2=n1+1023
	    write(23,rec=j) (yval1(i),i=n1,n2)
	    n1=n1+1024
22	  continue
	endif
      close(unit=23)
	print 23,nsamp
23	format(1x,i8,' values written to C:\YVAL.DAT')
	end


