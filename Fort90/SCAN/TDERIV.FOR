	program TDERIV
c to test derivsch

	real*4 data(2048)
	integer*2 ix(2048)

	data(1)=0.
	data(2)=0.2
	data(3)=0.4
	data(4)=0.6
	data(5)=0.8
	data(6)=1.0
	data(7)=0.8
	data(8)=0.6
	data(9)=0.4
	data(10)=0.2
	data(11)=0.
	data(12)=-0.2
	data(13)=-0.4
	data(14)=-0.6
	data(15)=-0.8
	data(16)=-1.0
	data(17)=-0.8
	data(18)=-0.6
	data(19)=-0.4
	data(20)=-0.2
	do i=21,40
	   data(i)=data(i-20)
	enddo
	if1=1
	if2=40
	dx=1.
	call DERIVSCH(data,ix,dx,if1,if2,ibk2,vard,
     & .true.,.false.)
	end

