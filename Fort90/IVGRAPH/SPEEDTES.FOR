	program speedtest

	integer*1	arra(50000)
	character*1	a,getch

	call mode(18)
	call TIMER(iticks1)
	do i=1,20
		call clears(0,480,i)
	enddo
	call TIMER(iticks2)
	idelta=(iticks2-iticks1)
	print*,'time clears:',idelta
	call TIMER(iticks1)
	do i=1,5000
		call dhline(100,500,30,14,22)
	enddo
	call TIMER(iticks2)
	idelta=(iticks2-iticks1)
	print*,'time dhline:',idelta
	call TIMER(iticks1)
	do i=1,10000
		call wpixel(10,50,13)
		call rpixel(10,50,ic)
	enddo
	call TIMER(iticks2)
	idelta=(iticks2-iticks1)
	print*,'time r/wpixel:',idelta
	call TIMER(iticks1)
	do i=1,20
		call fillwin(100,100,500,400,i)
	enddo
	call TIMER(iticks2)
	idelta=(iticks2-iticks1)
	print*,'time fillwin:',idelta
	call TIMER(iticks1)
	do i=1,5000
		call hline(100,500,400,1)
	enddo
	call TIMER(iticks2)
	idelta=(iticks2-iticks1)
	print*,'time hline:',idelta
	call TIMER(iticks1)
	do i=1,5
		call rdgwin1(200,200,400,400,arra)
		call wrgwin1(200,200,400,400,arra)
	enddo
	call TIMER(iticks2)
	idelta=(iticks2-iticks1)
	print*,'time rd/wrgwin1:',idelta
	a=getch(b)

	end







