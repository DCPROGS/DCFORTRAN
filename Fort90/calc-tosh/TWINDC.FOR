	PROGRAM twindc
c
	character*11 cdata(50,2)
	real*4 data(50)
	character*79 heading,title
	character*11 cnum
c
c
	npar=5
	ni0=npar		!rows
	nj0=2		!columns
	nidisp=npar
	if(npar.gt.20) nidisp=20
	nr1=-1		!so window initially in default position
	iconst=0	!no constraint
	iflagc=1	!show first column=names
	call INTCONV(npar,cnum)
	title=charnb(cnum)//' rate constants'
	heading='   param name   value '
c	names already set in cdata(i,1) after first call to INWINDc, now
c     enter values
	do i=1,npar
	   call INTCONV(i,cnum)
	   cdata(i,1)=' k'//charnb(cnum)
	   cdata(i,2)='          '
	enddo
	nd1=50	!dimensions of cdata
	nd2=2
	call INWINDc(cdata,ni0,nj0,nidisp,title,heading,
     & 	nr1,nc1,iflagc,iconst,nd1,nd2)
	do m=1,npar
	  call CHTOREAL(cdata(m,2),data(m))
====fix index of cdata in QDEFINE
	  print 1,cdata(m,1),data(m)
1	  format(1x,a11,3x,g13.6)
	enddo
	pause
c
	end


