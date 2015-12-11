	subroutine WAIT(n)
c Delay for n tick of 10 ms each
	call TIMER(n1)
	n2=n1+n
	do while(n1.lt.n2)
	   call TIMER(n1)
	enddo
	RETURN
	end

