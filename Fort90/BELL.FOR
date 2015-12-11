	subroutine BELL(n)
c New version that uses Spindrift routine (old=BELL1.FOR)
	do i=1,n
	   call TONE(440,15)    !150 ms
	   if(n.gt.1) call TONE(0,5)      !50 ms gap
	enddo

	return
	end


