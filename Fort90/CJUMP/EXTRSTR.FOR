	subroutine extrstr(chary,stry1,stry2,n)

	character*1 chary(0:n)
	character*(*) stry1,stry2
	stry1(1:1)=chary(0)
	do i=1,n
		if (chary(i).eq.' ') then
			stry1(1:i)=stry1(1:i)
			goto 1
		else
			stry1(1:i+1)=stry1(1:i)//chary(i)
		endif
	enddo
1     n1=i+1
	stry2(1:1)=chary(n1)
	j=1
	do i=n1+1,n
		if (chary(i).eq.' ') then
			stry2(1:j)=stry2(1:j)
			goto 2
		else
			stry2(1:j+1)=stry2(1:j)//chary(i)
		endif
	j=j+1
	enddo
2     continue
	end
