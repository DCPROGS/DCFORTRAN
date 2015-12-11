	program testgstr
	character    ans,getch
	character*40 text(10)
	real		 y(10)
      call GINO
	call VGA
	do i=1,10
	   y(i)=150.-i*10.
	enddo
	call GSTRING(100.,y(1),text(1),1,0.,3.,1,1,0,40,.true.)
	call GSTRING(100.,y(2),text(2),1,0.,3.,0,2,0,40,.true.)
	call GSTRING(100.,y(3),text(3),1,0.,3.,-1,3,0,40,.true.)

	ans=getch(ib)
	call devend
	end
