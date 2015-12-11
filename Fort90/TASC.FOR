	program TASC
c To test ASCIO
	real*4 data(2048,3)
	character colhead*30(10)

c
	ndim=3	!dimension of DATA aray
c	ndat=5	!5 lines of data
	ndat=1000	!lines of data
	jcol=2	!two entries in each line
	icolwid=13		!the minimum col width
	colhead(1)='time'
	colhead(2)='current'
c Test data
c  Col 1=time
	do i=1,1000
	   data(i,1)=0.1*float(i-1)
	   data(i,2)=0.01*float(i-1)
	enddo
c	data(1,1)=-0.01e-4
c	data(2,1)=-0.00012345678
c	data(3,1)=0.1234568
c	data(4,1)=1.12345678
c	data(5,1)=1112345.12345678
c  Col 2=current
c	data(1,2)=-1.23456e-13
c	data(2,2)=0.15
c	data(3,2)=1.23456e-13
c	data(4,2)=1.23456e3
c	data(5,2)=1.23456e30
	call ASCIO(data,ndat,jcol,ndim,.true.,icolwid,colhead)
	end

