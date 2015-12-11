	program TPIX
c
c Colours:
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
	integer*2 i1,i2
c
	call PLOTS(0,1,0)		!start Lahey graphics
	call TADDR(i1,i2)
c	pause
	call PLOT(0.,0.,999)
C	print *,i1,i2
	print 1,i1,i2
1	format(' i1,i2 = ',2i8)
	end

