	logical btest
	integer*4 ibset,ibclr
	integer*1 j1,j2,j3,j4
	integer*1 i1,i2,i3,i4,ival
c
	ival=0
	print*,'bit #'
	do i=0,7
	   i1=ibset(ival,i)        !set ith bit
	   j1=i1
	   print *,i,i1,j1
	enddo
	i1=ibset(ival,0)
	i2=ibset(i1,1)
	i3=ibset(i2,2)
	i4=ibset(i3,3)
	j1=i1
	j2=i2
	j3=i3
	j4=i4
	print *,i1,i2,i3,i4,btest(i1,1),btest(i4,3)	!should be F,T
	print *,j1,j2,j3,j4,btest(i1,1),btest(i4,3)	!should be F,T
	i5=ibclr(i4,3)
	j5=i5
	print*,i5,btest(i5,3)
	print*,j5,btest(i5,3)
	end

