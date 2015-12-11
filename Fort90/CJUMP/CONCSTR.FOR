	program concstr

	character*1 chary(0:10)
	character*10 stry1,stry2,name,array1,array2

	do i=1,2
		name=array//realtoch(i)
		print*,name
	enddo
	chary(0)='2'
	chary(1)='5'
	chary(2)='5'
	chary(3)=' '
	chary(4)='4'
	chary(5)=' '
	chary(6)=' '
	chary(7)=' '
	chary(8)=' '
	chary(9)=' '
	stry1(1:1)=chary(0)
	do i=1,9
		if (chary(i).eq.' ') then
			stry1(1:i)=stry1(1:i)
			goto 1
		else
			stry1(1:i+1)=stry1(1:i)//chary(i)
		endif
	enddo
1     n1=i+1
	print*,n1
	stry2(1:1)=chary(n1)
	j=1
	do i=n1+1,9
		if (chary(i).eq.' ') then
			stry2(1:j)=stry2(1:j)
			goto 2
		else
			stry2(1:j+1)=stry2(1:j)//chary(i)
		endif
	j=j+1
	enddo
2	print*,stry1
	print*,stry2
	end
