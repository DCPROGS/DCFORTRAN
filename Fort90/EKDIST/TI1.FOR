	integer*1 ip(4000),i1
	integer*4 i4
c
c	j1=32
	i1=32
	i4=32
	ip(3416)=32
	do i=1,4000
	   ip(i)=0
	enddo
	i1=ip(3416)
	i4=ip(3416)
	print 1,ip(3416),i1,i4
1	format(' ip, i1, i4 = ',3i4)
	call GETBITS(ip(3416),i4)
	end

