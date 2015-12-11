	program tasc2
	real*4 xval(10,3),yval(10,3),xcal(50,2),ycal(50,2)
	allocatable::data,colhead
	real*4 data(:,:)
	character*20 colhead(:)
	character*70 text
	character anum*15
	character cnum*11
	integer ndat(3),ncal(2)
c
	ndat(1)=10
	ndat(2)=5
	ncal(1)=50
	ncal(2)=40
c
	do j=1,3
	   do i=1,ndat(j)
		xval(i,j)=float(i+j)
		yval(i,j)=10.*float(i+j)
	   enddo
	enddo
	do j=1,2
	   do i=1,ncal(j)
		xcal(i,j)=float(i)*0.2
		ycal(i,j)=2.*float(i)
	   enddo
	enddo
c Put 1.e37 for undefined values
	do j=1,3
	   do i=ndat(j)+1,10
		xval(i,j)=1.e37
		yval(i,j)=1.e37
	   enddo
	enddo
	do j=1,2
	   do i=ncal(j)+1,50
		xcal(i,j)=1.e37
		ycal(i,j)=1.e37
	   enddo
	enddo

c
	ncurvd=3
	ncurvc=2
c Ideally would like a different number of values in each col, ie write
c ascii blank once ndat(j) exceeded
c    x1  y1  x2  y2
c	1  10  5   10
c	2  20  15  20
c	3  30
c Want to write ncurvd values (columns) of xval, yval
c and ncurvc values (columns) for xcal, ycal
	OPEN(unit=12,file='test.asc',status='UNKNOWN',
     & access='SEQUENTIAL',form='FORMATTED')
	write(12,FMT=800) ncurvd
800	FORMAT(I10)
801	FORMAT(2F15.5)
	do i=1,10
	   write(unit=12,fmt=8) (Xval(i,j),Yval(i,j),j=1,ncurvd),
     &		(Xcal(i,j),Ycal(i,j),j=1,ncurvc)
c8	   format(50f15.5)
8	   format(50g15.6)
	enddo
	CLOSE(unit=12)
	OPEN(unit=12,file='test.asc',status='UNKNOWN',
     & access='SEQUENTIAL',form='FORMATTED')
	ncol=ncurvd+ncurvc
	do i=1,10
	   do j=1,ncol  

		read(12,81) anum
81		format(a15)
		if(anum.eq.'   0.100000E+38') then
		   write(12,81) '              '
		endif
	   enddo
	enddo
	CLOSE(unit=12)

c	stop
c
	ndmax=0
	do j=1,ncurvd
	   if(ndat(j).gt.ndmax) ndmax=ndat(j)
	enddo
	ncmax=0
	do j=1,ncurvc
	   if(ncal(j).gt.ncmax) ncmax=ncal(j)
	enddo
	nrow=ndmax
	ncol=2*ncurvd	!for x and y
	ALLOCATE(data(ndmax,ncol))
	ALLOCATE(colhead(ncol))
	i1=0
	do i=1,ncol,2
	  i1=i1+1
	  call INTCONV(i1,cnum)
	  colhead(i)='X'//charnb(cnum)
	  colhead(i+1)='Y'//charnb(cnum)
	enddo
	ndim1=ndmax
	ndim2=ncol
	text='test data'
	lentext=70
c
	do j=1,ncurvd
	   do i=1,ndat(j)
		data(i,j)=0.0
	   enddo
	enddo
c copy obs and calc to data()
	do j=1,ncurvd
	   do i=1,ndat(j)
		j1=2*j-1
		data(i,j1)=xval(i,j)
		j1=j1+1
		data(i,j1)=yval(i,j)
	   enddo
	enddo
	ic=1
	call ASCWRITE(data,nrow,ncol,text,ndim1,ndim2,lentext,
     & icolwid,colhead,ic)

	end

