	subroutine SETCOLS(mono,icol,autplt,plot)
c To set default colours in AUTPLOT, VPLOT, VHIST
c Default colours set if mono=false on entry and not AUTPLOT (for autplot
c use the queued colours if mono=false)
c If PLOT=true then default colours for plotter are set, for display
c on the screen; black/white are interchanged so the display on screen looks
c like that on plotter, with a (pale) white background with black lines on it.
c  The Hgraph driver for HP550C interchanges black and white on the assumption
c that screen will have white lines on black background, but better to see it
c on screen as near as possible to what will be on plotter.  Therefore the
c colours set here must have black/white interchanged before actually
c plotting (ie if idev=>3).  This is done by subroutine REVCOL(icol)
	integer icol(100)
	logical mono,autplt,plot
c
	if(mono) then
	  do i=1,100
	    icol(i)=15		!bright white
	  enddo
	  icol(71)=0	!background=black
	  RETURN
	endif
c Lahey colours:
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c For HP550C plotting colours should be (Lahey numbering)
c 15=black  (NB black/white interchanged compared with screen numbering)
c 14=yellow; 13=magenta; 12=red; 11=cyan; 10=green; 9=blue; 8=white; 7=white
c and 0-6 are same as 8-14.
c
c Colours in icol() are numbered as line thickness, except colours for symbols,
c SD bars and lines joining data points are all set to 'data set' colours
c (icol(1)-icol(10))
c 1-10= data sets (#1-10) (and colour for symbols etc);
c 11-20=calc curves (#1-10);
c 21=axes (or calibration bars); 22=frame; 23=axis labels (same for X,Y)
c 24=axis numbers (same for X,Y); 25=plot title; 26=param value text;
c 27=symbols (line thickness only; colour as for data sets);
c 28=jump bar (logo) for C-jumps
c 29=jump bar (logo) for V-jumps
c 30=SD bars
c 31-50=extra text (#1-20); 51-60=arrows (#1-10); 61-70=lines (#1-10)
c icol(71)=background colour for graph
c 72-100 =spare for future

	if((.not.mono).and.(.not.autplt)) then	!set default colours
	   if(.not.plot) then	!screen colours
		icol(1)=14	!yellow	data sets 1:10
		icol(2)=12	!red
		icol(3)=10	!green
		icol(4)=13	!magenta
		icol(5)=11	!cyan
		icol(6)=9	!dark blue (bright)
		icol(7)=6	!brown
		icol(8)=2	!green (dark)
		icol(9)=1	!blue (dark)
c		icol(10)=15	!white
		icol(10)=4	!red (dark) (used for lo/hi bins of histo)
		do i=1,10
		   icol(i+10)=icol(i)	!calc curve same colour as data
		   icol(i+30)=14		!extra text = yellow
		   icol(i+40)=14		!extra text = yellow
		   icol(i+50)=12		!arrows = red
		   icol(i+60)=11		!extra lines = light blue
		   icol(i+71)=11		!horizontal lines
		   icol(i+81)=11		!vertical lines
		enddo
		icol(21)=11	!light blue
		icol(22)=11	!light blue
		icol(23)=11	!light blue
		icol(24)=11	!light blue
		icol(25)=14	!yellow
		icol(26)=14	!yellow
		icol(28)=10	!green
		icol(29)=12	!red
		icol(71)=8	!background=grey
	   else if(plot) then	!plotter colours
		icol(1)=9
		icol(2)=12
		icol(3)=10
		icol(4)=13
		icol(5)=11
		icol(6)=0
		icol(7)=14
		icol(8)=9
		icol(9)=11
		icol(10)=12	!red (used for lo/hi bins of histo
		do i=1,10
		   icol(i+10)=icol(i)	!calc curve same colour as data
		   icol(i+30)=12		!extra text = red
		   icol(i+40)=12		!extra text = yellow
		   icol(i+50)=12		!arrows = red
		   icol(i+60)=11		!extra lines = light blue
		   icol(i+71)=11		!horizontal lines
		   icol(i+81)=11		!vertical lines
		enddo
		icol(21)=0
		icol(22)=0
		icol(23)=0
		icol(24)=0
		icol(25)=11
		icol(26)=10		!param values
		icol(28)=10
		icol(29)=12
		icol(71)=7	!background=white
	   endif
	endif
	RETURN
	end

