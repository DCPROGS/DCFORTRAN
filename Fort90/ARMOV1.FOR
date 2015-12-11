	program armov1
	integer a(10)
	n=10
	a(1)=1
	a(2)=1
	a(3)=1
	a(4)=2
	a(5)=2
	a(6)=3
	a(7)=3
	a(8)=1
	a(9)=1
	a(10)=1
	j=1
	do i=1,n
	   do k=i,n
	      if((k.ne.i).and.(a(i).eq.a(k))) a(k)=-100
	   enddo
	   if(a(i).ne.-100) then
		a(j)=a(i)
		j=j+1
	   endif
	enddo
	m=j-1
	do j=1,m
	   print*,a(j)
	enddo
	end
