      subroutine texthelp(main,ihelp,ki,helps,nhelp,iprogram_type)
	use menu_f90
      character*70  helps(50)
	integer ithg(50)

      select case(ki)
	 CASE(1)
	   helps(1)=' Curve Fitting Program'
	   helps(2)=
     &   ' The CVFIT program is designed for weighted least-squares'
	   helps(3)=
     &   ' fitting of various equations to relatively small amounts of'
	   helps(4)=
     &   ' data that can be typed in, and to calculate errors and plot'
	   helps(5)=
     &   ' the results. It is possible to fit several data sets'
	   helps(6)=
     &   ' simultaneously with the same equation, or to fit potency'
	   helps(7)=
     &   ' ratios (or dose ratios constrained by the Schild equation)'
	   helps(8)=
     &   ' to sets of dose-response curves'
	   nhelp=8
	 case(2)
	   helps(1)=' ARROWS'
	   helps(2)=
     &   ' Draws an arrow between the points market with the mouse.'
	   helps(3)=
     &   ' When selected from the icon menu, moves the arrow at a'
	   helps(4)=
     &   ' new position'
	   nhelp=4
	 case(3)
	   helps(1)=' Axes'
	   helps(2)=
     &   ' Option to modify drawing attributes for the axes '
	   nhelp=2
	 case(4)
	   helps(1)=' Frame'
	   helps(2)=
     &   ' Option to modify drawing attributes for the frame '
	   nhelp=2
	 case(5)
	   helps(1)=' Scale'
	   helps(2)=
     & ' The data can be shown with various scales.  It is important'
	   helps(3)=
     & ' to notice that this affects only the display - the data for'
	   helps(4)=
     & ' fitting are not transformed. Normally the data are typed in'
	   helps(5)=
     & ' as their raw untransformed values, and they stay that '
	   helps(6)=
     & ' however you display them. Thus, for example, if you plot '
	   helps(7 )=
     & ' log(y) against log(x), and the result looks like a more or '
	   helps(8 )=
     & ' less as a straight line, you do not choose a straight line '
	   helps(9 )=
     & ' (eq.1) to fit to it, but a power function (eq. 23) because '
	   helps(10)=
     & ' a power function relationship between x and y is what gives'
	   helps(11)=
     & '  rise to a linear relationship between log(y) against log(x).'
	   helps(12)=
     & ' In most cases this is the best way to do fitting, especially '
	   helps(13)=
     & ' if you provide appropriate weights for the points. If you do'
	   helps(14)=
     & ' not wish to do it this way, then you must enter the '
	   helps(15)=
     & ' transformed values as data; in the example above you '
	   helps(16)=
     & ' would enter the numerical values of log(y) and log(x), '
	   helps(17)=
     & ' choose the arithmetic scales (not log-log) for display,  '
	   helps(18)=
     & ' and choose the straight line equation to do the fit.'
	   nhelp=18
	 case(6)
	   helps(1)=' Shape'
	   helps(2)=' The graph can be displayed in different shapes:'
	   helps(3)=' - landscape'
	   helps(4)=' - portrait'
	   helps(5)=' - square'
	   helps(6)=' - full width'
	   helps(7)=' - I-V plot: imposes the typical format of a' 
	   helps(8)=
     &   ' current-voltage plot (no frame, portrait shape,with axes'
	   helps(9)=
     &   ' crossing at 0,0 '
	   nhelp=9
	 case(7)
	   helps(1)=' Curves'
	   helps(2)=
     & ' Option to modify drawing attributes for the data curves'
	   nhelp=2
	 case(8)
	   helps(1)=' Data'
	   helps(2)=
     & ' Option to modify drawing attributes for the data points'
	   nhelp=2
	 case(9,10,11)
         helps(1)=' File'
	   helps(2)=' New data file/record:'
         helps(3)=
     &   ' - creates a new data file or insert a new record in an '
	   helps(4)=
     &   ' existing data file'
	   helps(5)=' Old data file:'
	   helps(6)=
     &   ' - reads back data stored on disc, reads data from an ASCII'
	   helps(7)=
     & ' file or reads data from a plot queue.'
	   nhelp=7
	 
	 case(12,13,14,15)
	   helps(1)=' Graph'
	   helps(2)=' Rescale'
	   helps(3)=
     &   ' - this option can be achieved using the pull down menu: '
	   helps(4)=
     &   '   in this case the user has to insert the new values for'
	   helps(5)=
     &   '   xmin,ymin,xmax,ymax'
	   helps(6)=
     &   '   When selected from the icon menu the new coordinates '
	   helps(7)=
     &   '   are choosen with the mouse'

	   helps(8)=' Save'
	   helps(9)=
     &   ' - option to save the graph as image file (*.cgm,*.bmp,*.wmf)'
	   helps(10)=' Queue as binary file'
	   helps(11)=' - adds the data to a plotq file'
	   nhelp=11
	 case(16)
	   helps(1)=' Interpolate'
	   helps(2)=
     &   ' Option to add points between the observed ones using cubic'
	   helps(3)=
     &   ' spline interpolation'
	   nhelp=3
	 case(17,18,19,20)
	   helps(1)=' Lines'
	   helps(2)=
     &	 ' Draws a line between the points market with the mouse.'
	   helps(3)=
     &	 ' When selected from the icon menu, moves the line at a'
	   helps(4)=
     &	 ' new position'
	   nhelp=4
	 case(21)
	   helps(1)=' Parameters'
	   helps(2)=
     & ' Option to modify drawing attributes for parameters'
	   nhelp=2
	 case(22)
	   helps(1)=' Text'
	   helps(2)=
     & ' Option to insert new text and to modify drawing attributes'
	   helps(3)=
     & ' for the existing one'
	   nhelp=3
	 case(23)
	   helps(1)=' Title'
	   helps(2)=
     & ' Option to insert new title and to modify drawing attributes'
	   helps(3)=
     & ' for the existing one'
	   nhelp=3
	case(24)
	   helps(1)=' Fit'
	   nhelp=2
	 case(25)
           helps(1)=' Fitting modes:'
           helps(2)=' (1) Fit one data set only'
           helps(3)=' (2) Fit selected data sets separately:'
	   helps(4)=
     &   ' - does a separate fit for each data set'
           helps(5)=
     &   ' (3) Fit selected data sets simultaneously with one equation:'
	   helps(6)=' - fits all the data with one equation as though all'
	   helps(7)=
     &	 ' the data were from a single set.'
	   helps(8)=
     &   ' (4) Fit selected data sets to  estimate relative potencies'
	   helps(9 )=
     &   ' (5) Fit selected data sets to estimate antagonist KB: '
	   helps(10)=
     &   ' - the data sets consist of a set of putatively-parallel (on'
	   helps(11)=
     &	'  log scale) concentration-response curves, each set being'
	   helps(12)=
     &  '  done, for example, with a different agonist (option 4), or'
	   helps(13)=
     &	'  with the same agonist but with a different antagonist'
	   helps(14)=
     &  '  concentration for each.In the latter case the antagonist'
	   helps(15)=
     &  '  concentration must be specified for each set as the'
	   helps(16)=
     &   '  set variable when the data are entered.The fitting can be'
	   helps(17)=
     &   '  in principal, with any equation that describes the curves: '
	   helps(18)=
     &   '  usually it will be a Hill equation, Langmuir equation or'
	   helps(19)=' power function.'
	   nhelp=19
	 case(28)
	   helps(1)=' Initial guesses'
	   helps(2)=
     &   ' All iterative methods require that initial guesses should be'
	   helps(3)=
     &   ' provided for each parameter. If the guesses are very bad '
	   helps(4)=
     &   ' the minimisation may not converge, and may well crash as a '
	   helps(5)=
     &	' result, for example, of some parameter shooting off to'
	   helps(6)=
     &	' infinity. But if the guesses are reasonable, the results'
	   helps(7)=
     &	' should, of course, be independent of what guesses you'
	   helps(8)=' use'
	   nhelp=8
	case(27)
	   helps(1 )=' Error estimates'
	   helps(2 )=
     &  ' Two methods are provided for expressing the precision of '
	   helps(3 )=
     &   ' the parameter estimates: approximate standard deviations'
	   helps(4 )=
     &   ' and likelihood intervals. (These methods are discussed'
	   helps(5 )=
     &   ' by Colquhoun & Sigworth - 1995)'
	   helps(6 )=
     &   ''
	   helps(7 )=
     &   ''
	   helps(8 )=
     &   ' '
	   helps(9 )=
     &   ''
	   helps(10)=
     &   ''
	   helps(11)=
     &   ''
	   nhelp=5
	case(26)
	   helps(1)=' Equations'
   	   helps(2)=' Polynomial (inc. straight line)'
	   helps(3)=' Langmuir hyperbola(s) (inc. or dec.) '
	   helps(4)=' Hill equation(s) (inc. or dec./common K or max)'
	   helps(5)=' Langmuir hyperbola(s) plus straight line'
	   helps(6)=' Hill equation(s) plus straight line'
	   helps(7)=
     &     ' Power function (linear on log-log plot)'
	   helps(8)=' Binding inhibition curve (parameter=KB)'
	   helps(9)=' Exponential binding onset curves-'
	   helps(10)='General fit of sums of exponentials'
	   helps(11)='General fit of sums of geometrics'
	   nhelp=11
	end select

	
	ndhelp=nhelp+4
	if(ndhelp.gt.25) ndhelp=25
	ihelp = gmCreateComplexDialogueBox(Main, 14,2, 16, 
     &	ndhelp,gall, 'Curve Fitting Help ',gmVpos=Gtop, 
     &gmIconFormat=GDLLICON,
     & gmIconFile='MBIG1052')

! Create main panel for form
	ihelPanel=gmCreatePanel(ihelp, 0, 0, 16, ndhelp-1, 
     &gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON,gmType=GNOBOUNDARY, 
     &gmLineCol=0, gmLineBorder=0, gmFillCol=14, gmFillBorder=0)

      	    call widmin(14,1)
	        call widmax(14,1)
	        CALL TEXATR(0,0,2,-1)
		    call texent(ihelpanel,1,1,helps(1),70,1,ithg(1))
	        CALL TEXATR(0,0,0,-1)
		    call texsiz(85)
		    do i=2,nhelp
		     call texent(ihelpanel,1,1+i,helps(i),70,1,ithg(I))
		    enddo
             
		    call texsiz(100)
	call gmdrawwindow(ihelp)	  
	end

