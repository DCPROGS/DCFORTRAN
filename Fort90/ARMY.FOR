	program army
	real a(10)
	n=10
	a(1)=2.1
	a(2)=2.2
	a(3)=3.3
	a(4)=3.3
	a(5)=7.
	a(6)=3.3
	a(7)=4.
	a(8)=4.
	a(9)=1.
	a(10)=8.
	j=1
	do i=1,n-1
	   do k=i+1,n
	      if(a(i).eq.a(k)) a(i)=-100
	   enddo
	   if(a(i).ne.-100) then
		a(j)=a(i)
		j=j+1
	   endif
	enddo
	m=j-1
	print*,'result:'
	do j=1,m
	   print*,a(j)
	enddo
	end
