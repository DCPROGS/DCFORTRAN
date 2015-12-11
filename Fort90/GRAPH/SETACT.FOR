	subroutine SETACT(iact,ic)
c To initialise all iact(i)=ic, ie pixel drawn in colour #ic (for ic=0,...,15),
c  (or not drawn at all if ic=-1), whatever original colour of pixel was.
c Colours:
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
	integer iact(0:15)
c
	do 1 i=0,15
1	iact(i)=ic
	RETURN
	end

