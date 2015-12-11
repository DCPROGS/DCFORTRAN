	program arly
	real a(10)
	n=10
	a(1)=1.1
	a(2)=1.
	a(3)=1.1
	a(4)=2.3
	a(5)=2.3
	a(6)=3.
	a(7)=3.
	a(8)=-1.
	a(9)=1.
	a(10)=1.1
	i=1
	m=n
	do while(i.le.m)
	   k=i+1
	   do while(k.le.m)
	      if(a(i).eq.a(k)) then
		   do while(a(k).eq.a(m).and.m.gt.k)
			m=m-1
		   enddo
		   t=a(k)
		   a(k)=a(m)
		   a(m)=t
		   m=m-1
		endif
		k=k+1
	   enddo
	   i=i+1
	enddo
	print*,'result:',m
	do j=1,m
	   print*,a(j)
	enddo
	print*,'z='
	read*,z
	k=0
	if(abs(z).gt.1.) then
	   y=abs(z)
	   do while(y.gt.1)
		y=y/10.
		k=k+1
	   enddo
	   a1=y*10.
	   if (z.lt.0.) a1=-y*10.
	   b1= k-1
	   goto 1
	else if(abs(z).gt.0.and.abs(z).lt.1) then
	   y=abs(z)
	   do while(y.lt.1)
		y=y*10.
		k=k+1
	   enddo
	   a1=y
	   if (z.lt.0.) a1=-y
	   b1=-k
	   goto 1
	endif
1	print*,a1,' ',b1
	end
