	subroutine SETCLs(mono,icol,autplt,plot,isetcol)
c=====================================================================
c To set default colours in AUTPLOT, VPLOT, VHIST
c SETCOL is version of SETCOLS for VPLOT3 that has extra param, isetcol
c     If isetcol=0 sets defaults unless autplt=true
c	If ISETCOL=1 then all colours to default if ICOL(i)=-1
c	on entry, but if ICOL(i) is a valid colour on entry, default not
c	set.
c Define logical function -invalid(i)=true if icol(i) is not a valid
c colour (0-15)
c Lahey colours:
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c For HP550C plotting colours should be (Lahey numbering)
c 15=black  (NB black/white interchanged compared with screen numbering)
c 14=yellow; 13=magenta; 12=red; 11=cyan; 10=green; 9=blue; 8=white; 7=white
c and 0-6 are same as 8-14.
c=================================================================
C
C IDRAW,ICOL,THICK,ITYPE,IFNT,ANGLE,IJUS,RXBOX,RYBOX,RX,RY,SIZES
c
c         1 = title,	       2 = parameter values
C	    3 = x label, 	       4 = y label               5 = z label
C	 6-30 = x numbers    31-55 = y numbers         56-80 = z numbers
C     81-100= extra text
c
c IDRAW,ICOL,THICK,ITYPE
c
c    101-150= data/symbols 151-200= calc curves,
c    201-210= arrows       211-220= lines
c    221-230= h lines      231-240= v lines
c        241= cj bar          242= vj bar               243= sd bar
c        244= frame		245= axes
c
c  XBEG,YBEG,XEND,YEND
c      1- 10= arrows       11- 20= lines
c     21- 30= h lines      31- 40= v lines
c         41= cj bar           42= vj bar                43= sd bar
c
c=====================================================================

	integer icol(250)
	logical mono,autplt,plot,invalid
	invalid(i)=icol(i).lt.0
	if(mono) then
	  do i=1,100
	    icol(i)=115		!bright white
	    if(isetcol.eq.2) icol(i)=103
	  enddo
	  icol(100)=100	!background=black
	  if(isetcol.eq.2) icol(100)=8
	  RETURN
	endif
c     Setting when NOT mono
	if(isetcol.eq.1) goto 1
c     Section for isetcol=0
	if(.not.autplt) then	!set default colours
	   if(.not.plot) then	!screen colours
		icol(101)=114	!yellow	data sets 1:10
		icol(102)=112	!red
		icol(103)=110	!green
		icol(104)=113	!magenta
		icol(105)=111	!cyan
		icol(106)=109	!dark blue (bright)
		icol(107)=106	!brown
		icol(108)=102	!green (dark)
		icol(109)=101	!blue (dark)
		icol(110)=104	!red (dark) (used for lo/hi bins of histo)
		icol(151)=114	!yellow	data sets 1:10
		icol(152)=112	!red
		icol(153)=110	!green
		icol(154)=113	!magenta
		icol(155)=111	!cyan
		icol(156)=109	!dark blue (bright)
		icol(157)=106	!brown
		icol(158)=102	!green (dark)
		icol(159)=101	!blue (dark)
		icol(160)=104	!red (dark) (used for lo/hi bins of histo)
		do i=1,10
		   icol(i+80)=114		!extra text = yellow
		   icol(i+90)=114		!extra text = yellow
		   icol(i+150)=icol(i+100)	!calc curve same colour as data
		   icol(i+200)=112		!arrows = red
		   icol(i+210)=106		!extra lines = brown
		   icol(i+220)=106		!extra h-lines
		   icol(i+230)=106		!extra v-lines
		enddo
		icol(1)=114		!yellow  title
 	      icol(2)=114		!yellow  p value
		icol(3)=111		!light blue  label
		icol(4)=111		!light blue  label
		icol(5)=111		!light blue  label
		do i=1,75
		   icol(i+5)=111
		enddo
		icol(241)=110	!green   cj
		icol(242)=112	!red     vj
		icol(244)=111	!light blue  farme
		icol(245)=111		!light blue  axes
		icol(250)=108	!background=grey
	   else if(plot) then	!plotter colours
		icol(101)=109
		icol(102)=112
		icol(103)=110
		icol(104)=113
		icol(105)=111
		icol(106)=100
		icol(107)=102
		icol(108)=101
		icol(109)=106
		icol(110)=104	!dark red (used for lo/hi bins of histo
		do i=1,10
		   icol(i+80)= 112		!extra text = red
		   icol(i+90)= 112		!extra text = red
		   icol(i+150)=icol(i+100)	!calc curve same colour as data
		   icol(i+200)=112		!arrows = red
		   icol(i+210)=106		!extra lines = brown
		   icol(i+220)=106		!extra h-lines
		   icol(i+230)=106		!extra v-lines
		enddo
		icol(1)=100
		icol(2)=100		!param values -black
		icol(3)=100
		icol(4)=100
		icol(5)=100
		do i=1,75
		   icol(i+5)=100
		enddo
		icol(241)=102
		icol(242)=112
		icol(244)=100
		icol(245)=100
		icol(250)=107	!background=light grey
	   endif
	endif
	RETURN
c
c Section for isetcol=1
1	continue
	   if(.not.plot) then	!screen colours
		if(invalid(101))  icol(101)=114	!yellow	data sets 1:10
		if(invalid(102))  icol(102)=112	!red
		if(invalid(103))  icol(103)=110	!green
		if(invalid(104))  icol(104)=113	!magenta
		if(invalid(105))  icol(105)=111	!cyan
		if(invalid(106))  icol(106)=109	!dark blue (bright)
		if(invalid(107))  icol(107)=106	!brown
		if(invalid(108))  icol(108)=102	!green (dark)
		if(invalid(109))  icol(109)=101	!blue (dark)
		if(invalid(110))  icol(110)=104	!red (dark) (used for lo/hi bins of histo)
		do i=1,10
		   if(invalid(i+80))  icol(i+80)=114		!extra text = yellow
		   if(invalid(i+90))  icol(i+90)=114		!extra text = yellow
		   if(invalid(i+150)) icol(i+150)=icol(i+100)		!calc curve same colour as da
		   if(invalid(i+200)) icol(i+200)=112		!arrows = red
		   if(invalid(i+210)) icol(i+210)=106		!extra lines = brown
		   if(invalid(i+220)) icol(i+220)=106		!extra h-lines
		   if(invalid(i+230)) icol(i+230)=106		!extra v-lines
		enddo
		if(invalid(1))   icol(1)=114		!yellow  title
		if(invalid(2))   icol(2)=114		!yellow  p value
		if(invalid(3))   icol(3)=111		!light blue  label
		if(invalid(4))   icol(4)=111		!light blue  label
		if(invalid(5))   icol(5)=111		!light blue  label
		do i=1,75
		   if(invalid(i+5)) icol(i+5)=111
		enddo
		if(invalid(241)) icol(241)=110	!green   cj
		if(invalid(242)) icol(242)=112	!red     vj
		if(invalid(244)) icol(244)=111	!light blue  farme
		if(invalid(245)) icol(245)=111		!light blue  axes
		if(invalid(250)) icol(250)=108	!red     vj
	   else if(plot) then
		if(invalid(101))  icol(101)=109
		if(invalid(102))  icol(102)=112
		if(invalid(103))  icol(103)=110
		if(invalid(104))  icol(104)=113
		if(invalid(105))  icol(105)=111
		if(invalid(106))  icol(106)=100
		if(invalid(107))  icol(107)=102
		if(invalid(108))  icol(108)=101
		if(invalid(109))  icol(109)=106
		if(invalid(110))  icol(110)=104
		do i=1,10
		   if(invalid(i+80))  icol(i+80)= 112	!extra text = yellow
		   if(invalid(i+90))  icol(i+90)= 112	!extra text = yellow
		   if(invalid(i+150)) icol(i+150)=icol(i+100)	!calc curve same colour as da
		   if(invalid(i+200)) icol(i+200)=112	!arrows = red
		   if(invalid(i+210)) icol(i+210)=106	!extra lines = brown
		   if(invalid(i+220)) icol(i+220)=106	!extra h-lines
		   if(invalid(i+230)) icol(i+230)=106	!extra v-lines
		enddo
		if(invalid(1))   icol(1)=100		!yellow  title
		if(invalid(2))   icol(2)=100		!yellow  p value
		if(invalid(3))   icol(3)=100		!light blue  label
		if(invalid(4))   icol(4)=100		!light blue  label
		if(invalid(5))   icol(5)=100		!light blue  label
		do i=1,75
		   if(invalid(i+5)) icol(i+5)=100
		enddo
		if(invalid(241)) icol(241)=102	!green   cj
		if(invalid(242)) icol(242)=112	!red     vj
		if(invalid(244)) icol(244)=100	!light blue  farme
		if(invalid(245)) icol(245)=100		!light blue  axes
		if(invalid(250)) icol(250)=107	!red     vj
	   endif
	RETURN
	end

