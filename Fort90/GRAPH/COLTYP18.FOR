	subroutine COLTYP18(icol)
c To set colour for Hgraph using the Lahay graphics colour numbering
c Lahey colours:
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c
c Hgraph colours
c -1=black; 0=white; 1=yellow (bright); 2=purple (bright); 3=red (bright)
c 4= pale blue (bright); 5=green (bright); 6=dark blue (bright);
c 7=dark gray; 8=pale gray; 9=brown (bright); 10=purple; 11=brown;
c 12=pale blue; 13=green; 14=dark blue
c
c NB icol+icol1=15, except for black
	icol1=15-icol
	if(icol1.eq.15) icol1=-1	!black (for icol=0)
c  	call COLTYP(icol1)
c     ??????????????
  	call lincols(icol1)
	RETURN
	end


