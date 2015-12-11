	subroutine VHELP(ihelp)
c Subroutine to define POPTEXT strings for help in VPLOT/VHIST/GPLOT3D
c Modif 11/15/97 04:55pm to add COMMON/HELP1/ihtype
c  ihtype=1	if called from VPLOT
c  ihtype=2	if called from VHIST
c  ihtype=3	if called from GPLOT3D
c
c ihelp=-2 for index of those listed below
c ihelp=-3 for index of GPLOT3D help
c ihelp=1  Main menu options
c ihelp=2
c ihelp=3  Rescale menu
c ihelp=4  loG scale menu
c ihelp=5  Graph shape menu
c ihelp=6  Symbol types
c ihelp=7  Straight line type
c ihelp=8  Curve line types
c ihelp=9  Text entry options
c ihelp=10 Fix text options
c ihelp=11 Maths symbols
c ihelp=12 Color types
c ihelp=13 Font table
c ihelp=14 Fixtext2 help
c ihelp=15 main menu for GPLOT3D
c ihelp=16 rescale menu for GPLOT3D
c ihelp=17 graph axes for GPLOT3D
c ihelp=18 graph view for GPLOT3D
c ihelp=19 More options menu for VPLOT/VHIST
c ihelp=20 Posh options for GPLOT3D
c ihelp=21 Fill options for GPLOT3D
c
c Lahey/Ioana colours:
c 0=black; 1=dark blue; 2=green; 3=light blue; 4=red; 5=purple
c 6=brown; 7=white; 8=pale white (grey); 9=dark blue -bright; 10=green -bright
c 11=light blue -bright; 12=red -bright; 13=purple -bright; 14=yellow -bright
c 15=white -bright
c
	allocatable arra
	integer*1 arra(:)
	character*56 helpst(24)		!help strings
	character*20 title
      character    charout,ans,getch		!for popmenu
	character*21 strings(21)
      character*5  helps*55         !for popmenu
	common/help1/ihtype
c
c
	ALLOCATE(arra(40000))
c
	if(ihelp.eq.-2) then 		!HELP INDEX
	   nxlo=100
	   nylo=-1
	   nyhi=470
	   ictm=15		!text white
	   ibkm=8		!background dark grey
	   icfm=1		!frame dark blue
	   icupm=12		!upper case red
	   TITLE=' VPLOT HELP INDEX'
	   strings(1)='1. main menu Options'
         strings(2)='2.                  '
	   strings(3)='3. Rescale menu     '
	   strings(4)='4. Log scale menu   '
	   strings(5)='5. Graph shape menu '
	   strings(6)='6. Symbol types     '
	   strings(7)='7. straight liNe type'
	   strings(8)='8. curVe line types  '
	   strings(9)='9. text Entry options'
	   strings(10)='10.fix Text options'
	   strings(11)='11.Maths symbols'
	   strings(12)='12.Color types'
	   strings(13)='13.Font types'
	   nval=13
c	   strings(i)='       NOT YET DONE '
c	   i=i+1
c	   strings(i)='2. Tics on the axes '
c==	   helps(5)=' value=more sensitive).   '
c==	   nhelp=5
	   nhelp=0
	   call POPMENU(nxlo,nylo,nyhi,strings,nval,ictm,icfm,icupm,
     &	 ibkm,title,helps,nhelp,iline,charout,ival)
	   if(iline.ge.1.and.iline.le.nval) then	!iline=0 for ESC=cancel
		ihelp=iline
	   else
		goto 99	!RETURN
	   endif
	else if(ihelp.eq.-3) then 		!HELP INDEX
	   nxlo=100
	   nylo=-1
	   nyhi=470
	   ictm=15		!text white
	   ibkm=8		!background dark grey
	   icfm=1		!frame dark blue
	   icupm=12		!upper case red
	   TITLE=' 3D PLOT HELP INDEX'
	   strings(1)='1. Main menu Options'
         strings(2)='2. Rescale menu     '
	   strings(3)='3. graph Axes   '
	   strings(4)='4. graph View   '
	   strings(5)='5. Posh options'
	   strings(6)='6. Fill options'
	   strings(7)='7.Maths symbols'
	   strings(8)='8.Color types'
	   strings(9)='9.Font types'
	   nval=9
	   nhelp=0
	   call POPMENU(nxlo,nylo,nyhi,strings,nval,ictm,icfm,icupm,
     &	 ibkm,title,helps,nhelp,iline,charout,ival)
	   if(iline.ge.1.and.iline.le.nval) then	!iline=0 for ESC=cancel
		jhelp=iline
	   else
		goto 99	!RETURN
	   endif
c ihelp=15 main menu for GPLOT3D
c ihelp=16 rescale menu for GPLOT3D
c ihelp=17 graph axes for GPLOT3D
c ihelp=18 graph view for GPLOT3D
c ihelp=19 More options menu for VPLOT/VHIST
c ihelp=20 Posh options for GPLOT3D
c ihelp=21 Fill options for GPLOT3D
	   if(jhelp.eq.1) then
		ihelp=15
	   else if(jhelp.eq.2) then
		ihelp=16
	   else if(jhelp.eq.3) then
		ihelp=17
	   else if(jhelp.eq.4) then
		ihelp=18
	   else if(jhelp.eq.5) then
		ihelp=20
	   else if(jhelp.eq.6) then
		ihelp=21
	   else if(jhelp.eq.7) then
		ihelp=11
	   else if(jhelp.eq.8) then
		ihelp=12
	   else if(jhelp.eq.9) then
		ihelp=13
	   endif
	endif

c
100	continue
	SELECT CASE(IHELP)
	CASE(1)
	   i=1
	   helpst(i)=
     &   '                      MAIN MENU                        '
	   i=i+1
	   helpst(i)=
     &   '1: RESCALE. Set Xmin, Xmax, tics/calibration bars etc. '
	   i=i+1
	   helpst(i)=
     &   '2: ZOOM. Enlarge/restore part of display def with cursor'
	   i=i+1
	   helpst(i)=
     &   '3: GRAPH SHAPE. Change aspect ratio.  NB do this before '
	   i=i+1
	   helpst(i)=
     &   '    adding extra text, arrows, lines (also for LOG AXES)'
	   i=i+1
	   helpst(i)=
     &   '4: GRAPH AXES. Set log scales, axis number format       '
c	   if(ihtype.eq.1) then		!vplot only
c	      i=i+1
c	      helpst(i)=
c     &   '  and multiple line trace plots.                        '
c	   endif
	   i=i+1
	   helpst(i)=
     &   '5: POSH OPTIONS. Fix text, arrows, lines, frame, SD bars'
	   i=i+1
	   helpst(i)=
     &   '6: GIVE TITLE. Define the title above the graph.        '
	   i=i+1
	   helpst(i)=
     &   '7: PLOT NOW. Output the graph to plotter now.           '
	   i=i+1
	   helpst(i)=
     &   '8: QUEUE PLOT. Keep graph to plot later in AUTPLOT.     '
	   i=i+1
	   helpst(i)=
     &   '9: END DISPLAY. Carry on with rest of the program.      '
	   i=i+1
	   helpst(i)=
     &   '0: REDRAW. Redraw screen to clean it up.                '
	   i=i+1
	   helpst(i)=
     &   '+: X AXIS LABEL. Define label on the abscissa.          '
	   i=i+1
	   helpst(i)=
     &   '-: Y AXIS LABEL. Define label on the ordinate.          '
	   if(ihtype.eq.1) then		!vplot only
	      i=i+1
	      helpst(i)=
     &   '*: INTERPOLATE. Add more points by spline interpolation.'
	      i=i+1
	      helpst(i)=
     &   '/: ALL POINTS/OMIT POINTS. Plot all points/omit ever nth'
	   endif
	   i=i+1
	   helpst(i)=
     &   '.: MORE OPTIONS. Set colours and line thickness.        '

	CASE(2)
	   goto 99		!return

	CASE(3)
	   i=1
	   helpst(i)=
     &   '                    RESCALE MENU                       '
	   i=i+1
	   helpst(i)=
     &   '1/2: Change smallest and largest X/Y value plotted.    '
	   i=i+1
	   helpst(i)=
     &   '3: AXES: Change the tick spacing                       '
	   i=i+1
	   helpst(i)=
     &   '4: Set values at which  the X and Y axes cross         '
	   if(ihtype.eq.1) then		!vplot only
	      i=i+1
	      helpst(i)=
     &   '3/4: CALIB BARS: change length of calibration bars     '
	      i=i+1
	      helpst(i)=
     &   '   and their position on the graph.                    '
	   endif
	   i=i+1
	   helpst(i)=
     &   '5: Change layout of tics on axes (above, centred etc.) '
	   if(ihtype.eq.1) then		!vplot only
		i=i+1
	   	helpst(i)=
     &   '6: SCALE/OFFSET: multiply or add a constant to Y values'
		i=i+1
	   	helpst(i)=
     &   '8/9: Change position of t=0 for jump experiments.      '
		i=i+1
	   	helpst(i)=
     &   '-: DECIMATE DATA -omit every nth point (eg to queue    '
		i=i+1
	   	helpst(i)=
     &   '   with a reduced sample rate)                         '
	   endif

	CASE(4)
	   i=1
	   helpst(i)=
     &   '                   GRAPH AXES MENU                     '
	   i=i+1
	   helpst(i)=
     &   '1: With log axes, toggles swaps log/arithmetic axes    '
	   i=i+1
	   helpst(i)=
     &   '1:  -with no log axes, toggles axes/calibration bars   '
	   i=i+1
	   helpst(i)=
     &   '2: Convert to semilog plot: y against log(x)           '
	   i=i+1
	   helpst(i)=
     &   '3: Convert to semilog plot: log(y) against x           '
	   i=i+1
	   helpst(i)=
     &   '4: Convert to double log plot: log(y) against log(x)   '
	   i=i+1
	   helpst(i)=
     &   '5: Convert to Hill plot (you must specify y0 and Ymax) '
	   i=i+1
	   helpst(i)=
     &   '6: Convert to square root of Y versus x                '
	   i=i+1
	   helpst(i)=
     &   '7: Convert to square root of Y versus log(x)           '
	   i=i+1
	   helpst(i)=
     &   '8: QUEUE PLOT. Keep graph to plot later in AUTPLOT.    '
	   i=i+1
	   helpst(i)=
     &   '+: Split single trace into several rows                '
	   i=i+1
	   helpst(i)=
     &   '-: Draw log X axis numbers in fixed/exponential format '
	   i=i+1
	   helpst(i)=
     &   ' OR allow/disallow scaling of X numbers by powers of 10'
	   i=i+1
	   helpst(i)=
     &   '*: As key 8, but for Y axis numbers                    '

	CASE(5)
	   i=1
	   helpst(i)=
     &   '                   GRAPH SHAPE MENU                    '
	   i=i+1
	   helpst(i)=
     &   '1: Use cursors to define height/width of the graph.    '
	   i=i+1
	   helpst(i)=
     &   '2: Specify height/width as percentage of page size.    '
	   i=i+1
	   helpst(i)=
     &   '3: Revert to the normal default height/width.          '
	   i=i+1
	   helpst(i)=
     &   '4: Make graph with equal length for X and Y axes.      '
	   i=i+1
	   helpst(i)=
     &   '5: Portrait shape: make graph taller than it is wide.  '
	   i=i+1
	   helpst(i)=
     &   '6: Graph whole width of page (e.g. for single channels)'
	   i=i+1
	   helpst(i)=
     &   '7: Set shape etc for I/V plot (portrait shape, axes    '
	   i=i+1
	   helpst(i)=
     &   '  cross at zero, tics centred).                        '

	CASE(6)
	   i=1
	   helpst(i)=
     &   '                    SYMBOL TYPES                       '
	   i=i+1
	   helpst(i)=
     &   'Positive for hollow symbol, negative for filled symbol.'
	   i=i+1
	   helpst(i)=
     &   '0=point; 1=triangle up; 2=triangle down; 3=square      '
	   i=i+1
	   helpst(i)=
     &   '4=diamond; 5=pentagon; 6=hexagon; 7=circle; 8= +; 9= x '
	   i=i+1
	   helpst(i)=
     &   '100 (0r greater) = no symbol to be plotted.            '


	CASE(7)
	   i=1
	   helpst(i)=
     &   '                    LINE TYPES                         '
	   i=i+1
	   helpst(i)=
     &   'Straight line types are:                               '
	   i=i+1
	   helpst(i)=
     &   '   -1=no line; 0=solid line; 1=dotted line;            '
	   i=i+1
	   helpst(i)=
     &   '   2-6 dashed (increasing dash length);                '
	   i=i+1
	   helpst(i)=
     &   '   7=dash-dot; 8=dash-dot-dot.                         '
	   i=i+1
	   helpst(i)=
     &   'Curved line types (for fitted curves) are:             '
	   i=i+1
	   helpst(i)=
     &   '   -1=no line; 0=solid; 1=dotted; 2=short dash;        '
	   i=i+1
	   helpst(i)=
     &   '   3=long dash; 4=long-short dash; 5=long-short-short.  '

	CASE(8)

	CASE(9)		!Help for HGSTRING
	   helpst(1)=
     &   '                TEXT ENTRY OPTIONS                     '
	   helpst(2)='^A   :align (resets index,exponent,etc)'
	   helpst(3)='^G   :greek (toggle on/off)'
	   helpst(4)='^Sm  :maths symbols (m=A-Z;a-z); Help=F2'
	   helpst(5)='^Fn  :change font (n=0-9)'
	   helpst(6)='^T   :maths table (toggle on/off)'
	   helpst(7)='^R   :revert to default font'
	   helpst(8)='^I   :italic (toggle on/off)'
	   helpst(9)='^N   :new line'
	   helpst(10)='^U   :up:exponent/superscript '
	   helpst(11)='^D   :down:index/subscript '
	   helpst(12)='^L   :underline'
	   helpst(13)='^E   :end'
	   helpst(14)='backspace: delete last char'
	   helpst(15)='<ENTER> or <ESC> when text done'
	   helpst(16)='(use FIX TEXT to change size,'
	   helpst(17)='   position, angle etc of text)'
	   helpst(18)='General GINO fonts : ^F then 0=draft'
         helpst(19)='  0=courier; 1=helvetica; 2=times; 3=avantgarde;'
         helpst(20)='  4=lublin; 5=newcentury; 6=souvenir; 7=palatino;'
         helpst(21)='  8=script; 9=italic; 10=greek 1; 11=greek 2'
	   i=21

	CASE(10)		!Help for FIXTEXT2
	   helpst(1)=
     &   '                FIX TEXT OPTIONS                       '
	   helpst(2)='Left mouse button pressed:'
	   helpst(3)='  -inside box: drag selected text'
	   helpst(4)='  -outside box: show all boxes'
	   helpst(5)='Centre mouse button -escape'
	   helpst(6)='Right button to select box to fix with keys:  '
	   helpst(7)='<CR> To Exit'
	   i=7

	CASE(11)
	   helpst(1)='   MATHS SYMBOLS : ^S  then                '
	   helpst(2)='C = *F070C*FR  D = *F070D*FR  E = *F070E*FR'
         HELPST(3)='G = *F070G*FR  I = *F070I*FR  J = *F070J*FR'
         helpst(4)='K = *F070K*FR  M = *F070M*FR  P = *F070P*FR'
         helpst(5)='U = *F070U*FR  W = *F070W*FR  X = *F070X*FR'
         helpst(6)='e = *F070e*FR  f = *F070f*FR  l = *F070l*FR'
         helpst(7)='m = *F070m*FR  o = *F070o*FR  q = *F070q   '
	   i=7

	CASE(12)

	CASE(13)
         helpst(19)='  0=courier; 1=helvetica; 2=times; 3=avantgarde;'
         helpst(20)='  4=lublin; 5=newcentury; 6=souvenir; 7=palatino;'
         helpst(21)='  8=script; 9=italic; 10=greek; 11=greek'

	   helpst(1)='Fonts (Size 12)'
	   helpst(2)=' 0.Courier'       !100
         helpst(3)=' 1.Helvetica'     !101
	   helpst(4)=' 2.Times '        !102
	   helpst(5)=' 3.AvantGarde'    !103
	   helpst(6)=' 4.Lublin '       !104
         helpst(7)=' 5.Newcentury  '  !105
	   helpst(8)=' 6.Souvenir'      !106
	   helpst(9)=' 7.Palatino'      !107
	   helpst(10)=' 8.Script'       !  8
	   helpst(11)=' 9.Italic'       !  6
	   helpst(12)='10.Greek '      ! 10
	   helpst(13)='11.Greek 2'      ! 23
	   helpst(14)='12.Dutch Solid'  ! 16
	   helpst(15)='13.Western'      ! 17
	   helpst(16)='14.Cyrillic'     ! 14
	   helpst(17)='15.Latin  '      ! 20
	   i=12

	CASE(14)		!Help for FIXTEXT2
	   helpst(1)='       MOVE OPTIONS                  '
	   helpst(2)='Arrow keys : Move text               '
	   helpst(3)='Ins key    : Toggle fine/raw movement'
	   helpst(4)='F1         : Help                    '
	   i=4
c
	CASE(15)
	   i=1
	   helpst(i)=
     &   '                   3D MAIN MENU                        '
	   i=i+1
	   helpst(i)=
     &   '1: RESCALE. Set Xmin, Xmax, Ymin, Ymax, Zmin, Zmax     '
	   i=i+1
	   helpst(i)=
     &   '2: DEFAULTS. Store/restore current values for colours,  '
	   i=i+1
	   helpst(i)=
     &   '    line thickness and fill [not working yet]            '
	   i=i+1
	   helpst(i)=
     &   '3: GRAPH VIEW. Change aspect ratio, view angle,         '
	   i=i+1
	   helpst(i)=
     &   '   rotate, change frame, interpolate more grid lines,   '
	   i=i+1
	   helpst(i)=
     &   '   hide some grid lines.   '
	   i=i+1
	   helpst(i)=
     &   '4: GRAPH AXIS. Suppress axes, change axis annotation,   '
	   i=i+1
	   helpst(i)=
     &   '    tic size and number of tics                         '
	   i=i+1
	   helpst(i)=
     &   '5: POSH OPTIONS. Move or alter text; set the type of    '
	   i=i+1
	   helpst(i)=
     &   '   colour fill and colour bands (contours)              '
	   i=i+1
	   helpst(i)=
     &   '6: GIVE TITLE. Define the title above the graph.        '
	   i=i+1
	   helpst(i)=
     &   '7: PLOT NOW. Output the graph to plotter now.           '
	   i=i+1
	   helpst(i)=
     &   '8: QUEUE PLOT. Keep graph to plot later in AUTPLOT      '
	   i=i+1
	   helpst(i)=
     &   '9: END DISPLAY. Carry on with rest of the program.      '
	   i=i+1
	   helpst(i)=
     &   '0: REDRAW. Redraw screen to clean it up.                '
	   i=i+1
	   helpst(i)=
     &   '+: X AXIS LABEL. Define label on the abscissa.          '
	   i=i+1
	   helpst(i)=
     &   '-: Y AXIS LABEL. Define label on the ordinate.          '
	   i=i+1
	   helpst(i)=
     &   'x: Z AXIS LABEL. Define label                           '
	   i=i+1
	   helpst(i)=
     &   '/: CROSS SECTION. Draw cross section of the 3D plot     '
	   i=i+1
	   helpst(i)=
     &   '    or revert from cross section to 3D plot             '
	   i=i+1
	   helpst(i)=
     &   '.: MORE OPTIONS. Set colours, line thickness etc       '
c	   i=18

	CASE(16)		!rescale menu for GPLOT3D
	   i=1
	   helpst(i)=
     &   '                   3D RESCALE MENU                        '
c	   goto 99		!return

	CASE(17)
	   i=1
	   helpst(i)=
     &   '                   3D GRAPH AXIS                        '
	   i=i+1
	   helpst(i)=
     &   '1: AXIS/NO AXIS. Draw axis/Erase axis                   '
	   i=i+1
	   helpst(i)=
     &   '2: DRAWING STYLE. Provides control over which axes are  '
	   i=i+1
	   helpst(i)=
     &   '   drawn and the display of grid line on this axis      '
	   i=i+1
	   helpst(i)=
     &   '   0, nothing drawn; 1, axis drawn; 2, grid drawn;      '
	   i=i+1
	   helpst(i)=
     &   '   3, axis and grid drawn                               '
	   i=i+1
	   helpst(i)=
     &   '3: DATA SUPPRESSION. Determines the suppression of      '
	   i=i+1
	   helpst(i)=
     &   '   annotation upon an individual axis : 0, no suppress; '
	   i=i+1
	   helpst(i)=
     &   '   1, suppress lowest data value; 2, suppress highest   '
	   i=i+1
	   helpst(i)=
     &   '   value; suppress the lowest and highest value         '
	   i=i+1
	   helpst(i)=
     &   '4: ANNOTATIONS. Sets user-specified format for axes     '
	   i=i+1
	   helpst(i)=
     &   '   NUMAXI: the axis to be annotated : 0, default; 1, x  '
	   i=i+1
	   helpst(i)=
     &   '   axis; 2, y axis; 3, z axis; 4, all axes              '
	   i=i+1
	   helpst(i)=
     &   '   NWIDTH: the total field width of the number (max 9)  '
	   i=i+1
	   helpst(i)=
     &   '   NPLACE: the number of decimal places                 '
	   i=i+1
	   helpst(i)=
     &   '   NPOW  : the power to which annotation is forced      '
	   i=i+1
	   helpst(i)=
     &   '5: TIC NUMBER: Number of tics on each axis              '
	   i=i+1
	   helpst(i)=
     &   '6: TIC SIZE.                                            '
	   i=i+1
	   helpst(i)=
     &   '0: REDRAW. Redraw screen to clean it up.                '
c	   i=18

	CASE(18)
	   i=1
	   helpst(i)=
     &   '                   3D GRAPH VIEW                        '
	   i=i+1
	   helpst(i)=
     &   '1: ROTATE. Sets the angle of rotation about a vertical  '
	   i=i+1
	   helpst(i)=
     &   '   axis in degrees anticlockwise                        '
	   i=i+1
	   helpst(i)=
     &   '2: VIEW DIRECTION. Sets angle of elevation in degrees of'
	   i=i+1
	   helpst(i)=
     &   '   the view direction above the horizontal              '
	   i=i+1
	   helpst(i)=
     &   '3: ASPECT RATIO X:Y. Change aspect ratio in X:Y plane   '
	   i=i+1
	   helpst(i)=
     &   '4: HEIGHT/BASE ratio                                    '
	   i=i+1
	   helpst(i)=
     &   '5: INTERPOLATE. Add extra grid lines -specify the number'
	   i=i+1
	   helpst(i)=
     &   '    of grid lines in X and Y directions.                '
	   i=i+1
	   helpst(i)=
     &   '6: GRID DENSITY. Allows the user to skip drawing of some'
	   i=i+1
	   helpst(i)=
     &   '   or all grid lines on the surface                     '
	   i=i+1
	   helpst(i)=
     &   '7: SURFACES DRAWN. Set to draw one or both surfaces     '
	   i=i+1
	   helpst(i)=
     &   '0: REDRAW. Redraw screen to clean it up.                '
c	   i=14

	CASE(19)
	   i=1
	   helpst(i)=
     &   '                MORE OPTIONS MENU                       '
	   i=i+1
	   helpst(i)=
     &   '1: MONOCHROME/COLOUR; toggles the colour                '
	   i=i+1
	   helpst(i)=
     &   '2: SCREEN COLOURS; sets default colours (light on dark) '
	   i=i+1
	   helpst(i)=
     &   '3: PLOT COLOURS; Sets dark on white colours for plotting'
	   i=i+1
	   helpst(i)=
     &   '4: COLOURS, MANUAL; set colours separately for each part'
	   i=i+1
	   helpst(i)=
     &   '5: THICKER LINES; Makes all lines thicker               '
	   i=i+1
	   helpst(i)=
     &   '6: THINNER LINES; Makes all lines thinner               '
	   i=i+1
	   helpst(i)=
     &   '7: LINE THICKNESS; set line thicknesses for each part   '
	   i=i+1
	   helpst(i)=
     &   '8: SET TEXT SIZE; set size (points) for all text        '
	   i=i+1
	   helpst(i)=
     &   '9: SET FONT; set font number for all text               '
	   i=i+1
	   helpst(i)=
     &   '0: REDRAW. Redraw screen to see effects, once all set   '
c	   i=i+1
c	   helpst(i)=
c     &   '+: SET SMALL PLOT           '
	   i=i+1
	   helpst(i)=
     &   '-: STORE DEFAULTS; store the current line thicknesses,  '
	   i=i+1
	   helpst(i)=
     &   ' fonts+size and colours for future recall (in vplot.ini)'
	   i=i+1
	   helpst(i)=
     &   'x: READ DEFAULTS; recall the stored line thicknesses,   '
	   i=i+1
	   helpst(i)=
     &   '   fonts + sizes, and colours.                          '
		i=i+1
	   	helpst(i)=
     &   '+: MULTIPLE TRACE: view/plot a continuous single channel'
		i=i+1
	   	helpst(i)=
     &   '   record with several lines per page, with specified   '                 '
		i=i+1
	   	helpst(i)=
     &   '   spacing between lines, and number of seconds per page'

	CASE(20)
	   i=1
	   helpst(i)=
     &   '                POSH OPTIONS (3D)                       '
	   i=i+1
	   helpst(i)=
     &   '2: FIX TEXT: move labels to desired positions with arrow'
	   i=i+1
	   helpst(i)=
     &   '   keys (no mouse dragging yet)                         '
	   i=i+1
	   helpst(i)=
     &   '+. FILL OPTIONS: set colour fill of surface, contours  '
	   i=i+1
	   helpst(i)=
     &   '-. MARK/UNMARK BAD REGION: fills (black) regions of    '
	   i=i+1
	   helpst(i)=
     &   '    surface that are undefined                          '

	CASE(21)
	   i=1
	   helpst(i)=
     &   '                FILL OPTIONS (3D)                       '
	   i=i+1
	   helpst(i)=
     &   '1: ONE COLOUR. Fill the whole surface with one colour   '
	   i=i+1
	   helpst(i)=
     &   '2: UPPER/LOWER. Fills the upper and lower surfaces with '              '
	   i=i+1
	   helpst(i)=
     &   '   two different colours                                '
	   i=i+1
	   helpst(i)=
     &   '3: POS/NEG, 2 COLS. Fills areas with positive and      '
	   i=i+1
	   helpst(i)=
     &   '    negative z values diff. colours  (default red/blue) '
	   i=i+1
	   helpst(i)=
     &   '4: POS/NEG, 4 COLS. As 3, but lower surface different  '
	   i=i+1
	   helpst(i)=
     &   '    colours  (default =not fixed yet)                   '
	   i=i+1
	   helpst(i)=
     &   '5: FILL CONTOURS.  Makes coloured contours shaded      '
	   i=i+1
	   helpst(i)=
     &   '    according to z value (with specified no of colours) '
	   i=i+1
	   helpst(i)=
     &   '6: NO FILL.  Remove the colour fill                    '
	   i=i+1
	   helpst(i)=
     &   '0: REDRAW. Redraw screen to clean it up.                '

	END SELECT

300	continue
	nhelp=i
	ixloh=60    !position for help box
	iyhih=430
c	subroutine POPTEXT(ixlo,iylo,iyhi,strings,n,ic,ibk,icf)
	if(ihelp.eq.7) then
	  call rdgwin1(0,100,100,400,arra)
	  call typebar(1)
	  ans=getch(k)
	  call wrgwin1(0,100,100,400,arra)
	else if(ihelp.eq.8) then
	  call rdgwin1(0,100,100,400,arra)
	  call typebar(2)
	  ans=getch(k)
	  call wrgwin1(0,100,100,400,arra)
	else if(ihelp.eq.11) then
	  ic=-2
	  if(jhelp.eq.6) ic=-3
	  call POPTEXT(ixloh,-1,iyhih,helpst,nhelp,ic,0,14)
	else if(ihelp.eq.12) then
	  call rdgwin1(0,100,100,400,arra)
	  call colbar(1)
	  ans=getch(k)
	  call wrgwin1(0,100,100,400,arra)
	else if(ihelp.eq.13) then
	  if(jhelp.eq.8) ic=-3
	  call POPTEXT(500,-1,460,helpst,nhelp,ic,15,0)
	else
	  call POPTEXT(ixloh,-1,iyhih,helpst,nhelp,15,8,1)
	endif
c	IC         : color of writing
c	IBK        : color of background
C	ICF        : color frame
c	subroutine POPTEXT(ixlo,iylo,iyhi,strings,n,ic,ibk,icf)
c
99	continue
	DEALLOCATE(arra)
	RETURN
	end

