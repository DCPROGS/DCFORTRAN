subroutine plot_prop(main,iplot,iplotf)
use menu_f90
if(iplot.eq.3) then
	iy=450
	iy0=120
	iyb=420
else
	iy=590
	iy0=0
	iyb=560
endif

! Set up master window iplotf
   iplotf = gmCreateMasterWindow(380, 214, 383, iy, GALL, 'Plot properties', gmVpos=GTOP, gmInitState=GNORMAL, gmIconFormat=GDLLICON, &
      gmIconFile='Gee')

! Create main panel for form
   iplotf_MainPanel=gmCreatePanel(iplotf, 0, 0, 383, iy, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)
if(iplot.eq.3) then
! Create radio box child iplotf_Radio4 child of iplotf_MainPanel
   iplotf_Radio4 = gmCreateRadioBox(iplotf_MainPanel, 0, 0, 380, 120, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='Display', gmVpos=GTOP)

! Create toggle button iplotf_Toggle11 child of iplotf_Radio4
   iplotf_Toggle11 = gmCreateToggleButton(iplotf_Radio4, 20, 30, 100, 25, &
   'Show open times conditional on PRECEDING shut time', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button iplotf_Toggle12 child of iplotf_Radio4
   iplotf_Toggle12 = gmCreateToggleButton(iplotf_Radio4, 20, 60, 100, 25,&
    'Show open times conditional on FOLLOWING shut time', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button iplotf_Toggle13 child of iplotf_Radio4
   iplotf_Toggle13 = gmCreateToggleButton(iplotf_Radio4, 20, 90, 100, 25, &
   'Show open times conditional on EITHER shut time', 0, gmType=G3DRADIO, gmVpos=GTOP)

endif
! Create radio box child iplotf_Radio1 child of iplotf_MainPanel
   iplotf_Radio1 = gmCreateRadioBox(iplotf_MainPanel, 0, iy0, 380, 180, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='Distribution of duration', gmVpos=GTOP)

! Create toggle button iplotf_Toggle1 child of iplotf_Radio1
   iplotf_Toggle1 = gmCreateToggleButton(iplotf_Radio1, 20, 30, 350, 25, &
   'Distribution of log durations', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button iplotf_Toggle2 child of iplotf_Radio1
   iplotf_Toggle2 = gmCreateToggleButton(iplotf_Radio1, 20, 60, 350, 25, &
   'Distribution of durations- set bins manually', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button iplotf_Toggle3 child of iplotf_Radio1
   iplotf_Toggle3 = gmCreateToggleButton(iplotf_Radio1, 20, 90, 350, 25, &
   ' Distribution of durations- 20 bins', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button iplotf_Toggle4 child of iplotf_Radio1
   iplotf_Toggle4 = gmCreateToggleButton(iplotf_Radio1, 20, 120, 350, 25, &
   ' Distribution of durations- 40 bins', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button iplotf_Toggle5 child of iplotf_Radio1
   iplotf_Toggle5 = gmCreateToggleButton(iplotf_Radio1, 20, 150, 350, 25, &
   ' Distribution of durations- 60 bins', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create panel iplotf_Panel1 child of iplotf_MainPanel
   iplotf_Panel1 = gmCreatePanel(iplotf_MainPanel, 0, iy0+180, 380, 120, gmTitle='', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget iplotf_Text1 child of iplotf_Panel1
   iplotf_Text1 = gmCreateTextEntry(iplotf_Panel1, 20, 20, 230, 25,'Histogram to start at', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget iplotf_Text2 child of iplotf_Panel1
   iplotf_Text2 = gmCreateTextEntry(iplotf_Panel1, 20, 50, 240, 25,'Number of bins/decade ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget iplotf_Text3 child of iplotf_Panel1
   iplotf_Text3 = gmCreateTextEntry(iplotf_Panel1, 20, 80, 230, 25,'Last x value (ms) ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create value entry iplotf_Value1 child of iplotf_Panel1
   iplotf_Value1 = gmCreateValueEntry(iplotf_Panel1, 270, 20, 100, 25, 0.000000, 8, 3, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create value entry iplotf_Value2 child of iplotf_Panel1
   iplotf_Value2 = gmCreateValueEntry(iplotf_Panel1, 270, 50, 100, 25, 0.000000, 8, 3, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create value entry iplotf_Value3 child of iplotf_Panel1
   iplotf_Value3 = gmCreateValueEntry(iplotf_Panel1, 270, 80, 100, 25, 0.000000, 8, 3, GEDIT, gmVmin=0.000000, gmVmax=1000.00, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)


if(iplot.eq.1.or.iplot.eq.2) then
! Create radio box child iplotf_Radio4 child of iplotf_MainPanel
   iplotf_Radio5 = gmCreateRadioBox(iplotf_MainPanel, 0, iy0+300, 380, 120, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='Display', gmVpos=GTOP)

! Create toggle button iplotf_Toggle11 child of iplotf_Radio4
   iplotf_Toggle11 = gmCreateToggleButton(iplotf_Radio5, 20, 30, 100, 25, &
   'Show ''exact'' pdf only (2*exact + asymptotic)', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button iplotf_Toggle12 child of iplotf_Radio4
   iplotf_Toggle12 = gmCreateToggleButton(iplotf_Radio5, 20, 60, 100, 25,&
    'Show exact AND asymptotic distributions', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button iplotf_Toggle13 child of iplotf_Radio4
   iplotf_Toggle13 = gmCreateToggleButton(iplotf_Radio5, 20, 90, 100, 25, &
   'Show asymptotic distribution and its components also', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create radio box child iplotf_Radio2 child of iplotf_MainPanel
   iplotf_Radio2 = gmCreateRadioBox(iplotf_MainPanel, 0, iy0+420, 380, 70, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='Show the pdf without missed events', gmVpos=GTOP)

! Create toggle button iplotf_Toggle7 child of iplotf_Radio2
   iplotf_Toggle7 = gmCreateToggleButton(iplotf_Radio2, 20, 30, 100, 25, 'Yes', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button iplotf_Toggle8 child of iplotf_Radio2
   iplotf_Toggle8 = gmCreateToggleButton(iplotf_Radio2, 150, 30, 100, 25, 'No', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create radio box child iplotf_Radio3 child of iplotf_MainPanel
   iplotf_Radio3 = gmCreateRadioBox(iplotf_MainPanel, 0, iy0+510, 380, 70, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='Rescale this pdf to unit area above t = tres', gmVpos=GTOP)

! Create toggle button iplotf_Toggle9 child of iplotf_Radio3
   iplotf_Toggle9 = gmCreateToggleButton(iplotf_Radio3, 20, 30, 100, 25, 'Yes', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create toggle button iplotf_Toggle10 child of iplotf_Radio3
   iplotf_Toggle10 = gmCreateToggleButton(iplotf_Radio3, 150, 30, 100, 25, 'No', 0, gmType=G3DRADIO, gmVpos=GTOP)

! Create button iplotf_Button1 child of iplotf_MainPanel
   iplotf_Button1 = gmCreatePushButton(iplotf_MainPanel, 190, iyb, 190, 25, 'OK', gmVpos=GTOP)

! Create button iplotf_Button2 child of iplotf_MainPanel
   iplotf_Button2 = gmCreatePushButton(iplotf_MainPanel, 0, iyb, 190, 25, 'Skip histogram', gmVpos=GTOP)
endif
return
end
