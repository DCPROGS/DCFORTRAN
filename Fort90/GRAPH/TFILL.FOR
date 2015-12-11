	program TFILL
c To test FILL with new Hgraph
c For HP550C plotting colours should be
c 0=black  (NB black/white interchanged compared with screen numbering)
c 1=yellow; 2=magenta; 3=red; 4=cyan; 5=green; 6=blue; 7=white; -1=white
c and 8-14 are same as 0-6.
c
	real x(4),y(4)
	character*1 ans
	logical hplot
c
101	format(a1)
1	continue
	idev=0
	hplot=.false.
	print 2
2	format(' Plot on 550C ? ')
	read 101,ans
	if(ans.eq.'y') then
	   idev=4
	   hplot=.true.
	endif
	call INIPLT(idev,.false.,1.0)
	call VIEWPORT(0,10000,0,7000)
 	call GRAPHBOUNDARY(0,10000,0,7000)
	call SCALE(0.,100.,0.,100.)
	call LINWID(30)
	ixlo=1000
	ixhi=9000
	iylo=1000
	iyhi=6000
	icol1=14	!yellow
	icol2=8	!grey
c Fill in the background colour=icol2
	x(1)=xworld(ixlo)
	y(1)=yworld(iylo)
	x(2)=xworld(ixlo)
	y(2)=yworld(iyhi)
	x(3)=xworld(ixhi)
	y(3)=yworld(iyhi)
	x(4)=xworld(ixhi)
	y(4)=yworld(iylo)
	n=4
	call COLTYP1(icol2)
	call FILL1(x,y,n,icol2,0,0,0)
c	call FILL(x,y,n,3,0,0,0)
c Draw frame in icol1
	call COLTYP1(icol1)
	call PLOT(ixlo,iylo,0)
	call PLOT(ixlo,iyhi,1)
	call PLOT(ixhi,iyhi,1)
	call PLOT(ixhi,iylo,1)
	call PLOT(ixlo,iylo,1)
c	pause
c	call FILL1(x,y,n,icol2,0,0,0)
	if(hplot) call ENDPLT
	pause
	goto 1
c
	end

	subroutine FILL1(x,y,n,icol,line,idelt,itheta)
	real x(n),y(n)
c Version of Hgraph FILL subroutine that uses Lahey colour numbering
c convention for ICOL in the call (see COLTYP1 for numbering)
c
c NB icol+icol1=15, except for black
	icol1=15-icol
	if(icol1.eq.15) icol1=-1	!black (for icol=0)
	call FILL(x,y,n,icol1,line,idelt,itheta)
	RETURN
	end

	subroutine COLTYP1(icol)
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


