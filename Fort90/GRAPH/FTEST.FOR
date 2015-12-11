	program FTEST
c
	integer*2 i
c
	call PLOTS(0,1,0)		!start Lahey graphics
	call TEST1(i)
	print 1,i
	pause
	call PLOT(0.,0.,999)
	print 1,i
1	format(' i = ',i8)
	pause
	end

