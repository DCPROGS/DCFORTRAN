                       
	subroutine define_colours(iset,plot)

	use gino_f90
	use menu_f90
	logical plot

if(plot) then
	call gDefineRGB(0,1.,1.,0.98)
	call gDefineRGB(48,0.,0.,0.)	!black
else
	call gDefineRGB(48,0.,0.,0.)	!black
	call gDefineRGB(0,0.73,0.78,0.82)
endif

call gDefineRGB(1,0.,0.,0.50)	!blue
call gDefineRGB(2,0.,0.50,0.)	!green
call gDefineRGB(3,0.,0.25,0.50)	!cyan
call gDefineRGB(4,0.75,0.0,0.0)	!red
call gDefineRGB(5,0.5,0.,0.5)	!magenta
call gDefineRGB(6,0.5,0.25,0.)	!brown
call gDefineRGB(7,0.5,0.5,0.5)	!grey
call gDefineRGB(8,0.75,0.75,0.75)!grey
call gDefineRGB(9,0.,0.,1.)		!blue
call gDefineRGB(10,0.,1.,0.)	!green
call gDefineRGB(11,0.,1.,1.)	!cyan
call gDefineRGB(12,1.,0.,0.)	!red
call gDefineRGB(13,1.,0.,1.)	!magenta
call gDefineRGB(14,1.,1.,0.)	!yellow
call gDefineRGB(15,1.,1.,1.)	!white
call gDefineRGB(16,0.25,0.0,0.25)	! plum
call gDefineRGB(17,0.,0.,0.63)	!blue
call gDefineRGB(18,0.,0.5,0.25)	!green fir
call gDefineRGB(19,0.,0.5,0.75)	!cyan
call gDefineRGB(20,0.5,0.,0.25)	!burgundy
call gDefineRGB(21,1.,0.,0.5)	!fuschia
call gDefineRGB(22,0.5,0.,0.)	!brown
call gDefineRGB(23,0.,1.,0.25)	!green
call gDefineRGB(24,0.,0.5,0.5)	!petrol
call gDefineRGB(25,0.,0.5,1.)	!blue
 
call gDefineRGB(27,0.5,1.,1.)	!cyan
call gDefineRGB(28,1.,0.5,0.5)	!pink
call gDefineRGB(29,1.,0.5,0.75)	!fresia
call gDefineRGB(30,1.,1.,0.5)	!yellow
call gDefineRGB(31,0.5,0.25,0.25)!brown
call gDefineRGB(32,0.25,0.,0.5)	!ink
call gDefineRGB(33,0.,0.,0.25)	!indigo
call gDefineRGB(34,0.5,1.,0.)	!dark green
call gDefineRGB(35,0.5,0.5,1.)	!turquoise
call gDefineRGB(36,0.5,0.,1.)	!blue
call gDefineRGB(37,1.,0.5,1.)	!pink
call gDefineRGB(38,1.,0.5,0.25)	!orange
call gDefineRGB(39,0.25,0.5,0.5)!cyan
call gDefineRGB(40,0.,0.25,0.25)!petrol
call gDefineRGB(41,0.5,0.5,0.75)!lilac
call gDefineRGB(42,0.5,1.,0.5)	!light green
call gDefineRGB(43,0.,0.25,0.)	!dark green
call gDefineRGB(44,0.5,0.5,0.25)!dark olive
call gDefineRGB(45,0.5,0.5,0.)	!olive
call gDefineRGB(46,1.,0.5,0.)	!orange
call gDefineRGB(47,0.25,0.,0.)	!very dark brown
call gDefineRGB(50,1.,0.80,0.8)	!PINK
call gDefineRGB(51,0.8,1.,0.8)	!GREEN
	call gDefineRGB(50,1.,0.80,0.8)	!orange
    call gDefineRGB(51,0.8,1.0,0.8)	!very dark brown
   call gDefineRGB(70, 0.000000, 0.250980, 0.501961)!
   call gDefineRGB(71, 1.000000, 1.000000, 0.768627)!
   call gDefineRGB(72, 1.000000, 1.000000, 0.792157)!
   call gDefineRGB(73, 1.000000, 1.000000, 0.800000)!

! Define user defined colour indices used
   call gDefineRGB(120, 0.705882, 0.827451, 0.905882)!
   call gDefineRGB(121, 0.250980, 0.501961, 0.501961)!
   call gDefineRGB(122, 0.568627, 0.784314, 0.784314)!
   call gDefineRGB(123, 0.768627, 0.882353, 0.882353)!
   call gDefineRGB(124, 0.780392, 0.886275, 0.886275)!
   call gDefineRGB(125,0.819608, 1.000000, 0.800000)
   call gDefineRGB(126,0.874510, 1.000000, 0.866667)
   call gDefineRGB(127, 0.909804, 1.000000, 0.909804)

! Define user defined colour indices used
   call gDefineRGB(130, 1.000000, 0.792157, 0.584314)
   call gDefineRGB(131, 1.000000, 0.776471, 0.549020)

   call gDefineRGB(140, 0.011765, 0.827451, 0.803922)
   call gDefineRGB(141, 0.192157, 0.984314, 0.964706)
   call gDefineRGB(142, 0.286275, 0.784314, 0.803922)
   call gDefineRGB(143, 0.011765, 0.827451, 0.764706)
   call gDefineRGB(144, 1.000000, 0.611765, 0.431373)
   call gDefineRGB(145, 1.000000, 0.737255, 0.611765)
   call gDefineRGB(146, 0.992157, 0.839216, 0.729412)

	call gDefineRGB(150, 0.992157, 0.984314, 0.764706)! light yellow
   call gDefineRGB(151, 0.501961, 0.247059, 0.247059)
   call gDefineRGB(152, 1.000000, 1.000000, 0.501961)

end

subroutine select_colour(red,green,blue,icol)
	use gino_f90
	use menu_f90

if(red.ge.0.00.and.red.le.0.05) then
	if(green.ge.0.00.and.green.le.0.05) then
		if(blue.ge.0.00.and.green.le.0.05) then
			icol=48
		else if(blue.ge.0.20.and.blue.le.0.30) then
			icol=33
		else if(blue.ge.0.45.and.blue.le.0.55) then
		else if(blue.ge.0.58.and.blue.le.0.68) then
			icol=17
		else if(blue.ge.0.70.and.blue.le. 0.80) then
		else if(blue.ge.0.95.and.blue.le.1.00) then
			icol=9
		endif
	else if(green.ge.0.20.and.green.le.0.30) then
		if(blue.ge.0.00.and.green.le.0.05) then
			icol=43
		else if(blue.ge.0.20.and.blue.le.0.30) then
			icol=40
		else if(blue.ge.0.45.and.blue.le.0.55) then
			icol=3
		else if(blue.ge.0.58.and.blue.le.0.68) then
		else if(blue.ge.0.70.and.blue.le. 0.80) then
		else if(blue.ge.0.95.and.blue.le.1.00) then
		endif
	else if(green.ge.0.45.and.green.le.0.55) then
		if(blue.ge.0.00.and.green.le.0.05) then
			icol=2
		else if(blue.ge.0.20.and.blue.le.0.30) then
			icol=18
		else if(blue.ge.0.45.and.blue.le.0.55) then
			icol=24
		else if(blue.ge.0.58.and.blue.le.0.68) then
			 
		else if(blue.ge.0.70.and.blue.le. 0.80) then
			icol=19
		else if(blue.ge.0.95.and.blue.le.1.00) then
		    icol=25
		endif
	else if(green.ge.0.58.and.green.le.0.68) then
		if(blue.ge.0.00.and.green.le.0.05) then

		else if(blue.ge.0.20.and.blue.le.0.30) then
		else if(blue.ge.0.45.and.blue.le.0.55) then
		else if(blue.ge.0.58.and.blue.le.0.68) then
		else if(blue.ge.0.70.and.blue.le. 0.80) then
		else if(blue.ge.0.95.and.blue.le.1.00) then
		endif
	else if(green.ge.0.70.and.green.le. 0.80) then
		if(blue.ge.0.00.and.green.le.0.05) then

		else if(blue.ge.0.20.and.blue.le.0.30) then
		else if(blue.ge.0.45.and.blue.le.0.55) then
		else if(blue.ge.0.58.and.blue.le.0.68) then
		else if(blue.ge.0.70.and.blue.le. 0.80) then
		else if(blue.ge.0.95.and.blue.le.1.00) then
		endif
	else if(green.ge.0.95.and.green.le.1.00) then
		if(blue.ge.0.00.and.green.le.0.05) then
			icol=10
		else if(blue.ge.0.20.and.blue.le.0.30) then
		else if(blue.ge.0.45.and.blue.le.0.55) then
			icol=26
		else if(blue.ge.0.58.and.blue.le.0.68) then
			icol=23
		else if(blue.ge.0.70.and.blue.le. 0.80) then
			
		else if(blue.ge.0.95.and.blue.le.1.00) then
			icol=11
		endif
	endif
else if(red.ge.0.20.and.red.le.0.30) then
	if(green.ge.0.00.and.green.le.0.05) then
	if(blue.ge.0.00.and.green.le.0.05) then
		icol=47
		else if(blue.ge.0.20.and.blue.le.0.30) then
		icol=16
		else if(blue.ge.0.45.and.blue.le.0.55) then
		icol=32
		else if(blue.ge.0.58.and.blue.le.0.68) then
		else if(blue.ge.0.70.and.blue.le. 0.80) then
		else if(blue.ge.0.95.and.blue.le.1.00) then
		endif
	else if(green.ge.0.20.and.green.le.0.30) then
	else if(green.ge.0.45.and.green.le.0.55) then
	if(blue.ge.0.00.and.green.le.0.05) then

		else if(blue.ge.0.20.and.blue.le.0.30) then
		else if(blue.ge.0.45.and.blue.le.0.55) then
		icol=39
		else if(blue.ge.0.58.and.blue.le.0.68) then
		else if(blue.ge.0.70.and.blue.le. 0.80) then
		else if(blue.ge.0.95.and.blue.le.1.00) then
		endif
	else if(green.ge.0.58.and.green.le.0.68) then
	else if(green.ge.0.70.and.green.le. 0.80) then
	else if(green.ge.0.95.and.green.le.1.00) then
	endif
else if(red.ge.0.45.and.red.le.0.55) then
	if(green.ge.0.00.and.green.le.0.05) then
	if(blue.ge.0.00.and.green.le.0.05) then
		icol=22
		else if(blue.ge.0.20.and.blue.le.0.30) then
		icol=20
		else if(blue.ge.0.45.and.blue.le.0.55) then
		icol=5
		else if(blue.ge.0.58.and.blue.le.0.68) then
		else if(blue.ge.0.70.and.blue.le. 0.80) then
		else if(blue.ge.0.95.and.blue.le.1.00) then
		icol=36
		endif
	else if(green.ge.0.20.and.green.le.0.30) then
	if(blue.ge.0.00.and.green.le.0.05) then
		icol=6
		else if(blue.ge.0.20.and.blue.le.0.30) then
		icol=31
		else if(blue.ge.0.45.and.blue.le.0.55) then
		else if(blue.ge.0.58.and.blue.le.0.68) then
		else if(blue.ge.0.70.and.blue.le. 0.80) then
		else if(blue.ge.0.95.and.blue.le.1.00) then
		endif
	else if(green.ge.0.45.and.green.le.0.55) then
	if(blue.ge.0.00.and.green.le.0.05) then
		icol=45
		else if(blue.ge.0.20.and.blue.le.0.30) then
		icol=44
		else if(blue.ge.0.45.and.blue.le.0.55) then
		icol=7
		else if(blue.ge.0.58.and.blue.le.0.68) then
		else if(blue.ge.0.70.and.blue.le. 0.80) then
		icol=41
		else if(blue.ge.0.95.and.blue.le.1.00) then
		icol=35
		endif
	else if(green.ge.0.58.and.green.le.0.68) then
	else if(green.ge.0.70.and.green.le. 0.80) then
	else if(green.ge.0.95.and.green.le.1.00) then
	if(blue.ge.0.00.and.green.le.0.05) then
		icol=34
		else if(blue.ge.0.20.and.blue.le.0.30) then
		else if(blue.ge.0.45.and.blue.le.0.55) then
		icol=42
		else if(blue.ge.0.58.and.blue.le.0.68) then
		else if(blue.ge.0.70.and.blue.le. 0.80) then
		else if(blue.ge.0.95.and.blue.le.1.00) then
		icol=27
		endif
	endif
else if(red.ge.0.58.and.red.le.0.68) then
	if(green.ge.0.00.and.green.le.0.05) then
	else if(green.ge.0.20.and.green.le.0.30) then
	else if(green.ge.0.45.and.green.le.0.55) then
	else if(green.ge.0.58.and.green.le.0.68) then
	else if(green.ge.0.70.and.green.le. 0.80) then
	else if(green.ge.0.95.and.green.le.1.00) then
	endif
else if(red.ge.0.70.and.red.le. 0.80) then
	if(green.ge.0.00.and.green.le.0.05) then
	if(blue.ge.0.00.and.green.le.0.05) then
		icol=4
		else if(blue.ge.0.20.and.blue.le.0.30) then
		else if(blue.ge.0.45.and.blue.le.0.55) then
		else if(blue.ge.0.58.and.blue.le.0.68) then
		else if(blue.ge.0.70.and.blue.le. 0.80) then
		else if(blue.ge.0.95.and.blue.le.1.00) then
		
		endif
	else if(green.ge.0.20.and.green.le.0.30) then
	else if(green.ge.0.45.and.green.le.0.55) then
	else if(green.ge.0.58.and.green.le.0.68) then
	else if(green.ge.0.70.and.green.le. 0.80) then
	if(blue.ge.0.00.and.green.le.0.05) then
		
		else if(blue.ge.0.20.and.blue.le.0.30) then
		else if(blue.ge.0.45.and.blue.le.0.55) then
		else if(blue.ge.0.58.and.blue.le.0.68) then
		else if(blue.ge.0.70.and.blue.le. 0.80) then
		icol=8
		else if(blue.ge.0.95.and.blue.le.1.00) then
		
		endif
	else if(green.ge.0.95.and.green.le.1.00) then
	
	endif
else if(red.ge.0.95.and.red.le.1.00) then
	if(green.ge.0.00.and.green.le.0.05) then
	if(blue.ge.0.00.and.green.le.0.05) then
		icol=12
		else if(blue.ge.0.20.and.blue.le.0.30) then
		else if(blue.ge.0.45.and.blue.le.0.55) then
		icol=21
		else if(blue.ge.0.58.and.blue.le.0.68) then
		else if(blue.ge.0.70.and.blue.le. 0.80) then
		else if(blue.ge.0.95.and.blue.le.1.00) then
		icol=13
		endif
	else if(green.ge.0.20.and.green.le.0.30) then
	else if(green.ge.0.45.and.green.le.0.55) then
	if(blue.ge.0.00.and.green.le.0.05) then
		icol=46
		else if(blue.ge.0.20.and.blue.le.0.30) then
		icol=38
		else if(blue.ge.0.45.and.blue.le.0.55) then
		icol=28
		else if(blue.ge.0.58.and.blue.le.0.68) then
		else if(blue.ge.0.70.and.blue.le. 0.80) then
		icol=29
		else if(blue.ge.0.95.and.blue.le.1.00) then
		icol=37
		endif
	else if(green.ge.0.58.and.green.le.0.68) then
	else if(green.ge.0.70.and.green.le. 0.80) then
	else if(green.ge.0.95.and.green.le.1.00) then
	if(blue.ge.0.00.and.green.le.0.05) then
		icol=14
		else if(blue.ge.0.20.and.blue.le.0.30) then
		else if(blue.ge.0.45.and.blue.le.0.55) then
		icol=30
		else if(blue.ge.0.58.and.blue.le.0.68) then
		else if(blue.ge.0.70.and.blue.le. 0.80) then
		else if(blue.ge.0.95.and.blue.le.1.00) then
		icol=15
		endif
	endif
endif



end


subroutine set_colours(mono,icol,autplt,plot,isetcol)
	
	integer icol(250)

	logical mono,autplt,plot,invalid(250)
	do i=1,250
	invalid(i)=icol(i).le.0
	enddo
	if(mono) then
	  do i=1,100
	    icol(i)=15		!bright white
	    if(isetcol.eq.2) icol(i)=103
	  enddo
	  icol(100)=48	!background=black
	  if(isetcol.eq.2) icol(100)=8
	  RETURN
	endif

	if(isetcol.eq.1) goto 1

	if(.not.autplt) then	!set default colours
	   if(.not.plot) then	!screen colours
		icol(101)=14	!yellow	data sets 1:10
		icol(102)=12	!red
		icol(103)=10	!green
		icol(104)=13	!magenta
		icol(105)=11	!cyan
		icol(106)=9	!dark blue (bright)
		icol(107)=6	!brown
		icol(108)=2	!green (dark)
		icol(109)=1	!blue (dark)
		icol(110)=4	!red (dark) (used for lo/hi bins of histo)
		icol(111)=14	!yellow	data sets 1:10
		icol(122)=12	!red
		icol(113)=2	!green
		icol(114)=13	!magenta
		icol(115)=11	!cyan
		icol(116)=9	!dark blue (bright)
		icol(117)=6	!brown
		icol(118)=2	!green (dark)
		icol(119)=1	!blue (dark)
		icol(120)=4	
		icol(151)=14	!yellow	data sets 1:10
		icol(152)=12	!red
		icol(153)=10	!green
		icol(154)=13	!magenta
		icol(155)=11	!cyan
		icol(156)=9	!dark blue (bright)
		icol(157)=6	!brown
		icol(158)=2	!green (dark)
		icol(159)=1	!blue (dark)
		icol(160)=4	!red (dark) (used for lo/hi bins of histo)
		do i=1,10
		 icol(120+i)= icol(100+i)
		 icol(170+i)= icol(100+i)
		   icol(i+80)=14		!extra text = yellow
		   icol(i+90)=14		!extra text = yellow
		   icol(i+150)=icol(i+100)	!calc curve same colour as data
		   icol(i+200)=12		!arrows = red
		   icol(i+210)=6		!extra lines = brown
		   icol(i+220)=6		!extra h-lines
		   icol(i+230)=6		!extra v-lines
		enddo
		do i=11,20
			 if(invalid(i+150)) icol(i+150)=icol(i+100)	
		enddo
		icol(1)=14		!yellow  title
 	    icol(2)=14		!yellow  p value
		icol(3)=11		!light blue  label
		icol(4)=11		!light blue  label
		icol(5)=11		!light blue  label
		do i=1,75
		   icol(i+5)=11
		enddo
		icol(241)=10	!green   cj
		icol(242)=12	!red     vj
		icol(244)=11	!light blue  farme
		icol(245)=11		!light blue  axes
		icol(250)=8	!background=grey
	   else if(plot) then	!plotter colours
		icol(101)=9
		icol(102)=12
		icol(103)=10
		icol(104)=13
		icol(105)=11
		icol(106)=10
		icol(107)=2
		icol(108)=1
		icol(109)=6
		icol(110)=4	!dark red (used for lo/hi bins of histo
	
		icol(111)=9
		icol(112)=12
		icol(113)=10
		icol(114)=13
		icol(115)=11
		icol(116)=10
		icol(117)=2
		icol(118)=1
		icol(119)=6
		icol(120)=4	!d
		if(isetcol.eq.4) then
			icol(111)=9
			icol(112)=12
			icol(113)=2
			icol(114)=13
			icol(115)=11
			icol(116)=10
			icol(117)=2
			icol(118)=1
			icol(119)=6
			icol(120)=4	
		endif
		do i=1,10
		 icol(120+i)= icol(100+i)
		 icol(170+i)= icol(100+i)
		   icol(i+80)= 12		!extra text = red
		   icol(i+90)= 12		!extra text = red
		   icol(i+150)=icol(i+100)	!calc curve same colour as data
		   icol(i+200)=12		!arrows = red
		   icol(i+210)=6		!extra lines = brown
		   icol(i+220)=6		!extra h-lines
		   icol(i+230)=6		!extra v-lines
		enddo
		do i=11,20
			 if(invalid(i+150)) icol(i+150)=icol(i+100)	
		enddo
		icol(1)=48
		icol(2)=48		!param values -black
		icol(3)=48
		icol(4)=48
		icol(5)=48
		do i=1,75
		   icol(i+5)=48
		enddo
		icol(241)=2
		icol(242)=12
		icol(244)=48
		icol(245)=48
		icol(250)=7	!background=light grey
	   endif
	endif
	RETURN

1	continue
	   if(.not.plot) then	!screen colours
		if(invalid(101))  icol(101)=14	!yellow	data sets 1:10
		if(invalid(102))  icol(102)=12	!red
		if(invalid(103))  icol(103)=10	!green
		if(invalid(104))  icol(104)=13	!magenta
		if(invalid(105))  icol(105)=11	!cyan
		if(invalid(106))  icol(106)=9	!dark blue (bright)
		if(invalid(107))  icol(107)=6	!brown
		if(invalid(108))  icol(108)=2	!green (dark)
		if(invalid(109))  icol(109)=1	!blue (dark)
		if(invalid(110))  icol(110)=4	!red (dark) (used for lo/hi bins of histo)
		if(invalid(111))  icol(111)=14	!yellow	data sets 1:10
		if(invalid(112))  icol(112)=12	!red
		if(invalid(113))  icol(113)=2	!green
		if(invalid(114))  icol(114)=13	!magenta
		if(invalid(115))  icol(115)=11	!cyan
		if(invalid(116))  icol(116)=9	!dark blue (bright)
		if(invalid(117))  icol(117)=6	!brown
		if(invalid(118))  icol(118)=2	!green (dark)
		if(invalid(119))  icol(119)=1	!blue (dark)
		if(invalid(120))  icol(120)=4
		do i=1,10
		 icol(120+i)= icol(100+i)
		 icol(170+i)= icol(100+i)
		   if(invalid(i+80))  icol(i+80)=14		!extra text = yellow
		   if(invalid(i+90))  icol(i+90)=14		!extra text = yellow
		   if(invalid(i+150)) icol(i+150)=icol(i+100)		!calc curve same colour as da
		   if(invalid(i+200)) icol(i+200)=12		!arrows = red
		   if(invalid(i+210)) icol(i+210)=6		!extra lines = brown
		   if(invalid(i+220)) icol(i+220)=6		!extra h-lines
		   if(invalid(i+230)) icol(i+230)=6		!extra v-lines
		enddo
		if(invalid(1))   icol(1)=14		!yellow  title
		if(invalid(2))   icol(2)=14		!yellow  p value
		if(invalid(3))   icol(3)=11		!light blue  label
		if(invalid(4))   icol(4)=11		!light blue  label
		if(invalid(5))   icol(5)=11		!light blue  label
		do i=1,75
		   if(invalid(i+5)) icol(i+5)=11
		enddo
		if(invalid(241)) icol(241)=10	!green   cj
		if(invalid(242)) icol(242)=12	!red     vj
		if(invalid(244)) icol(244)=11	!light blue  farme
		if(invalid(245)) icol(245)=11		!light blue  axes
		if(invalid(250)) icol(250)=8	!red     vj
	   else if(plot) then
		if(invalid(101))  icol(101)=9
		if(invalid(102))  icol(102)=12
		if(invalid(103))  icol(103)=10
		if(invalid(104))  icol(104)=13
		if(invalid(105))  icol(105)=11
		if(invalid(106))  icol(106)=0
		if(invalid(107))  icol(107)=2
		if(invalid(108))  icol(108)=1
		if(invalid(109))  icol(109)=6
		if(invalid(110))  icol(110)=4
		if(invalid(111))  icol(111)=9
		if(invalid(112))  icol(112)=12
		if(invalid(113))  icol(113)=2
		if(invalid(114))  icol(114)=13
		if(invalid(115))  icol(115)=11
		if(invalid(116))  icol(116)=0
		if(invalid(117))  icol(117)=2
		if(invalid(118))  icol(118)=1
		if(invalid(119))  icol(119)=6
		if(invalid(120))  icol(120)=4
		do i=1,10
		 icol(120+i)= icol(100+i)
		 icol(170+i)= icol(100+i)
		   if(invalid(i+80))  icol(i+80)= 12	!extra text = yellow
		   if(invalid(i+90))  icol(i+90)= 12	!extra text = yellow
		   if(invalid(i+150)) icol(i+150)=icol(i+100)	!calc curve same colour as da
		   if(invalid(i+200)) icol(i+200)=12	!arrows = red
		   if(invalid(i+210)) icol(i+210)=6	!extra lines = brown
		   if(invalid(i+220)) icol(i+220)=6	!extra h-lines
		   if(invalid(i+230)) icol(i+230)=6	!extra v-lines
		enddo
		do i=11,20
			 if(invalid(i+150)) icol(i+150)=icol(i+100)	
		enddo
		if(invalid(1))   icol(1)=48		!yellow  title
		if(invalid(2))   icol(2)=48		!yellow  p value
		if(invalid(3))   icol(3)=48		!light blue  label
		if(invalid(4))   icol(4)=48		!light blue  label
		if(invalid(5))   icol(5)=48		!light blue  label
		do i=1,75
		   if(invalid(i+5)) icol(i+5)=48
		enddo
		if(invalid(241)) icol(241)=2	!green   cj
		if(invalid(242)) icol(242)=12	!red     vj
		if(invalid(244)) icol(244)=48	!light blue  farme
		if(invalid(245)) icol(245)=48		!light blue  axes
		if(invalid(250)) icol(250)=7	!red     vj
		
	   endif
	do i=1,250
		if(icol(i).eq.14) icol(i)=46 
	enddo
	RETURN
	



end


subroutine glincols(ic,idev)
use gino_f90
	use menu_f90
	call gDefineRGB(28,1.,0.5,0.5)	!pink
	call gDefineRGB(42,0.5,1.,0.5)	!light green
! idev=0: screen
! idev=1: .wmf
! idev=2: .bmp
! idev=3,4: postscript printer
! idev=5  : laserjet
! idev=6  : color deskjet
! See also GSETCOLS.FOR
	if(idev.ge.3.and.idev.le.6) call gDefineRGB(48,0.,0.,0.)	!black

	if(idev.eq.0) then
		call lincol(ic)
	else
	    kap=0
	    if(idev.lt.0) then
	        kap=idev
	        idev=-idev
	        if(idev.eq.6)idev=7
	    endif    
	
	    select case(ic)
		case(38,46)
			if(idev.eq.1) ica=3
			if(idev.eq.2) ica=3
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=46
			if(idev.eq.7) ica=3
			
		case(48)

			if(idev.eq.1) ica=1
			if(idev.eq.2) ica=1
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=48
			if(idev.eq.7) ica=48
		case(0)
			if(idev.eq.1) ica=1
			if(idev.eq.2) ica=1
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=0
			if(idev.eq.7) ica=0
		case(1,17,24,25,32,33,36,40)
			if(idev.eq.1) ica=7
			if(idev.eq.2) ica=7
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=1
			if(idev.eq.7) ica=1
		case(2,18,23,43,44,45)
			if(idev.eq.1) ica=5
			if(idev.eq.2) ica=5
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=2
			if(idev.eq.7) ica=2
		case(3,19,27,35,39)
		    if(idev.eq.1) ica=6
			if(idev.eq.2) ica=6
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=3
			if(idev.eq.7) ica=3
		case(4)
			if(idev.eq.1) ica=2
			if(idev.eq.2) ica=2
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=4
			if(idev.eq.7) ica=4
		case(5,16,20)
			if(idev.eq.1) ica=8
			if(idev.eq.2) ica=8
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=5
			if(idev.eq.7) ica=5
		case(6,22,31,47)
			if(idev.eq.1) ica=9
			if(idev.eq.2) ica=9
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=6
			if(idev.eq.7) ica=9
		case(7)
			if(idev.eq.1) ica=15
			if(idev.eq.2) ica=15
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=48
			if(idev.eq.7) ica=48
		case(8)
			if(idev.eq.1) ica=15
			if(idev.eq.2) ica=15
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=48
			if(idev.eq.7) ica=48
		case(9)
			if(idev.eq.1) ica=7
			if(idev.eq.2) ica=7
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=9
			if(idev.eq.7) ica=9
		case(10,26,34,41,42)
			if(idev.eq.1) ica=42 !ica=5
			if(idev.eq.2) ica=42 !ica=5
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=10
			if(idev.eq.7) ica=10
		case(11)
			if(idev.eq.1) ica=6
			if(idev.eq.2) ica=6
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=11
			if(idev.eq.7) ica=11
		case(12)
			if(idev.eq.1) ica=28 !ica=2
			if(idev.eq.2) ica=28 !ica=2
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=12
			if(idev.eq.7) ica=12
		case(13,21,28,29,37)
			if(idev.eq.1) ica=8
			if(idev.eq.2) ica=8
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=13
			if(idev.eq.7) ica=13
		case(14,30)
			if(idev.eq.1) ica=4
			if(idev.eq.2) ica=4
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=14
			if(idev.eq.7) ica=14
		
		case(15)
			if(idev.eq.1) ica=10
			if(idev.eq.2) ica=10
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=0
			if(idev.eq.7) ica=0
	    end select
	
	    if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
	
	    if(kap.lt.0) then
	        ic=ica
	        idev=kap
	
	    else
	        call lincol(ica)
	    endif
	endif
	end

subroutine lincols1(ic)
use gino_f90
	use menu_f90
! idev=0: screen
! idev=1: .wmf
! idev=2: .bmp
! idev=3,4: postscript printer
! idev=5  : laserjet
! idev=6  : color deskjet
! See also GSETCOLS.FOR
logical bold,italik,underline
common/text_attr/bold,italik,underline,ipow,idev
	if(idev.ge.3.and.idev.le.6) call gDefineRGB(48,0.,0.,0.)	!black

	if(idev.eq.0) then
		call lincol(ic)
	else
	
	kap=0
	if(idev.eq.-1) then
	   idev=0
	   kap=-1
	endif
	if(idev.eq.-6) then
	   idev=6
	   kap=-6
	endif
	select case(ic)
		case(38,46)
			if(idev.eq.1) ica=3
			if(idev.eq.2) ica=3
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=46
		case(48)

			if(idev.eq.1) ica=1
			if(idev.eq.2) ica=1
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=48
		case(0)
			if(idev.eq.1) ica=1
			if(idev.eq.2) ica=1
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=0
		case(1,17,24,25,32,33,36,40)
			if(idev.eq.1) ica=7
			if(idev.eq.2) ica=7
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=1
		case(2,18,23,43,44,45)
			if(idev.eq.1) ica=5
			if(idev.eq.2) ica=5
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=2
		case(3,19,27,35,39)
		    if(idev.eq.1) ica=6
			if(idev.eq.2) ica=6
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=3
		case(4)
			if(idev.eq.1) ica=2
			if(idev.eq.2) ica=2
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=4
		case(5,16,20)
			if(idev.eq.1) ica=8
			if(idev.eq.2) ica=8
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=5
		case(6,22,31,47)
			if(idev.eq.1) ica=9
			if(idev.eq.2) ica=9
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=6
		case(7)
			if(idev.eq.1) ica=15
			if(idev.eq.2) ica=15
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=48
		case(8)
			if(idev.eq.1) ica=15
			if(idev.eq.2) ica=15
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=48
		case(9)
			if(idev.eq.1) ica=7
			if(idev.eq.2) ica=7
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=9
		case(10,26,34,41,42)
			if(idev.eq.1) ica=5
			if(idev.eq.2) ica=5
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=10
		case(11)
			if(idev.eq.1) ica=6
			if(idev.eq.2) ica=6
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=11
		case(12)
			if(idev.eq.1) ica=2
			if(idev.eq.2) ica=2
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=12
		case(13,21,28,29,37)
			if(idev.eq.1) ica=8
			if(idev.eq.2) ica=8
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=13
		case(14,30)
			if(idev.eq.1) ica=4
			if(idev.eq.2) ica=4
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=14
		
		case(15)
			if(idev.eq.1) ica=10
			if(idev.eq.2) ica=10
			if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
			if(idev.eq.6) ica=0
	end select
	
	if(idev.eq.3.or.idev.eq.4.or.idev.eq.5) ica=48
	
	if(kap.eq.-1) then
	   ic=ica
	   idev=-1
	else if(kap.eq.-6) then
	   ic=ica
	   idev=-6
	else
	   call lincol(ica)
	endif
	endif
	end

