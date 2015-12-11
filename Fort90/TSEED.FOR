1	call RANSEED()
	do i=1,10
	   u=drandom()
	   print 2,i,u
2	   format(i6,g13.6)
	enddo
	pause
	goto 1
	end
