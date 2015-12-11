c	subroutine BELL(n)
	subroutine BELL1(n)
c Th1s version behaves oddly when in graphics mode (because of 'print'
c statement?) -better use current version that calls Spindrift routine.
	do i=1,n
	   print 3,char(7)
3	   format('&',a1)	!prevents new line for every bleep
	   if(n.gt.1) call WAIT(7)	!70 ms between beeps
	enddo
	return
	end


