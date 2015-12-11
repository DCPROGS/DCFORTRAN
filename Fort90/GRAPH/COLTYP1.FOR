	subroutine COLTYP1(icol)
c To set colour for Hgraph using the Lahey graphics/IV graphics colour numbers
c Lahey/Ioana colours:
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c For HP550C plotting colours should be
c 15=black  (NB black/white interchanged compared with screen numbering)
c 14=yellow; 13=magenta; 12=red; 11=cyan; 10=green; 9=blue; 8=white; 7=white
c and 0-6 are same as 8-14.
c
c Hgraph colours
c -1=black; 0=white; 1=yellow (bright); 2=purple (bright); 3=red (bright)
c 4= pale blue (bright); 5=green (bright); 6=dark blue (bright);
c 7=dark gray; 8=pale gray; 9=brown (bright); 10=purple; 11=brown;
c 12=pale blue; 13=green; 14=dark blue
c For HP550C plotting colours should be
c 0=black  (NB black/white interchanged compared with screen numbering)
c 1=yellow; 2=magenta; 3=red; 4=cyan; 5=green; 6=blue; 7=white; -1=white
c and 8-14 are same as 0-6.
c
c NB icol+icol1=15, except for black
	icol1=15-icol
	if(icol1.eq.15) icol1=-1	!black (for icol=0)
	call COLTYP(icol1)
c	call COLTYPTT(icol1)		!screen only?
	RETURN
	end


