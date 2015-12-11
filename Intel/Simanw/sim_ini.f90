!     Last change:  D    11 Mar 105    4:44 pm
subroutine sim_ini(main,initwin,intoggle,itcall,pfile,initext,inifile,initext1,append,ivaln)


use menu_f90

character*50 pfile 
character*60 inifile

character*(60) text_tog(100)
integer intoggle(100)
logical append
 
call gDefineRGB(140, 0.011765, 0.827451, 0.803922)
call gDefineRGB(141, 0.192157, 0.984314, 0.964706)
call gDefineRGB(142, 0.286275, 0.784314, 0.803922)
call gmsetguigridmode(goff)


 initwin = gmCreateComplexDialogueBox(main,300,100,480,655, GALL, 'Welcome ', &
              	gmhpos=gleft,gmvpos=gtop,gmIconFormat=GDLLICON,gmIconFile='MBIG1052')

! Create main panel for form
   initwin_MainPanel=gmCreatePanel(initwin, 0, 0, 480, 655, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE,gmFillCol=140)
  initwin_Panel10 = gmCreatePanel(initwin, 30,30 , 420, 150, gmTitle='', gmType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=141, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)


 inittext8 = gmCreateTextEntry(initwin_Panel10, 70, 20, 300, 25,'WELCOME TO SIMAN', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=151 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
  
  
  
 
 
   initialw_Text1 = gmCreateTextEntry(initwin_Panel10, 40, 50, 300, 25,'Analyse binary output from simulations in HJCFIT', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=151 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
  ! call gmSetFontAttribs(initialw_Text1, gmFontFace='Times New Roman', gmPointSize=16)

! Create text widget initialw_Text2 child of initialw_Panel1
   initialw_Text2 = gmCreateTextEntry(initwin_Panel10, 100, 80, 300, 25,'Version 1.00 (Intel)', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget initialw_Text3 child of initialw_Panel1
   initialw_Text3 = gmCreateTextEntry(initwin_Panel10, 70, 110, 300, 25,'Copyright 2008 D. Colquhoun & I. Vais ', 255, GDISPLAY, &
      gmBack1Col=0, gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
   !call gmSetFontAttribs(initialw_Text3, gmPointSize=8)
   

 initwin_Panel20 = gmCreatePanel(initwin_MainPanel, 30,190 , 420, 160, gmTitle='Inifile', gmType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=141, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget initwin_Text2 child of initwin_Panel2
  iniText1 = gmCreateTextEntry(initwin_Panel20, 20, 30, 280, 25,inifile, 255, GEDIT, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create button initwin_Button3 child of initwin_Panel2
   initwin_Button3 = gmCreatePushButton(initwin_Panel20, 315, 30, 75, 25, 'Browse', &
   gmVpos=GTOP,gmcallback=-7)

    initwin_Radio20 = gmCreateRadioBox(initwin_Panel20, 20, 70, 380, 80, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=142, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)

 
! Create toggle button initwin_Toggle3 child of initwin_Radio2
   inToggle(14) = gmCreateToggleButton(initwin_Radio20, 20, 10, 355, 25, 'Use inifile ', 0, gmType=G3DRADIO, &
      gmVpos=GTOP,gmCallback=-8)
   
! Create toggle button initwin_Toggle4 child of initwin_Radio2
   intoggle(15) = gmCreateToggleButton(initwin_Radio20, 20, 40, 230, 25, 'Use default values (no inifile)', 1, gmType=G3DRADIO, &
      gmVpos=GTOP,gmCallback=-9)
  
     initwin_Panel2 = gmCreatePanel(initwin_MainPanel, 30,360 , 420, 190, gmTitle='Printout file', gmType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=141, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget initwin_Text2 child of initwin_Panel2
   iniText = gmCreateTextEntry(initwin_Panel2, 20, 30, 280, 25,pfile, 255, GEDIT, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create button initwin_Button3 child of initwin_Panel2
   initwin_Button3 = gmCreatePushButton(initwin_Panel2, 315, 30, 75, 25, 'Browse', &
   gmVpos=GTOP,gmcallback=-5)
   initwin_Radio2 = gmCreateRadioBox(initwin_Panel2, 20, 70, 380, 110, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=142, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)

 
! Create toggle button initwin_Toggle3 child of initwin_Radio2
   inToggle(11) = gmCreateToggleButton(initwin_Radio2, 70, 10, 230, 25, 'Append to existing print file', 0, gmType=G3DRADIO, &
      gmVpos=GTOP,gmCallback=-2)

! Create toggle button initwin_Toggle4 child of initwin_Radio2
   intoggle(12) = gmCreateToggleButton(initwin_Radio2, 70, 40, 230, 25, 'Overwrite/new printout file', 0, gmType=G3DRADIO, &
      gmVpos=GTOP,gmCallback=-3)

! Create toggle button initwin_Toggle5 child of initwin_Radio2
   inToggle(13) = gmCreateToggleButton(initwin_Radio2, 70, 70, 230, 25, 'No printout file', 0, &
   gmType=G3DRADIO, gmVpos=GTOP,gmCallback=-4)
if(append) then 
call gmsettoggleswitch(intoggle(11),gon)
else
call gmsettoggleswitch(intoggle(12),gon)
endif

 initwin_Panel30 = gmCreatePanel(initwin_MainPanel, 30,560 , 420, 45, gmTitle='', gmType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=141, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)


 inittext = gmCreateTextEntry(initwin_Panel30, 20, 10, 280, 25,'Number of files to be pooled [max 10] :', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=0 , gmVpos=GTOP)
 val=1.0
 ivaln=gmCreateValueEntry(initwin_Panel30, 300, 10, 60, 25, val, 3, 0, GEDIT,&
	  gmVpos=GTOP)
 
!
	   iy=620
 
   itogButton = gmCreatePushButton(initwin_MainPanel, 250, 10, 200, 25, 'Continue', &
   gmVpos=Gbottom,gmType=GDEFAULTBUTTON,gmcallback=itcall) !-40)
  call gmSetWidgetStatus(itogbutton, GSELECTABLE)
   itogButton = gmCreatePushButton(initwin_MainPanel, 30, 10, 200, 25, 'Exit', &
   gmVpos=Gbottom,gmType=GDEFAULTBUTTON,gmcallback=-6) !-40)
  call gmSetWidgetStatus(itogbutton, GSELECTABLE)
  ! call gmSetWidgetStatus(initwin_Button6, GCHECKED)
   
   call gmdrawwindow(initwin)
   call gmSetGuiGridMode(GOn)
   call gmSetFontAttribs(initText8, gmFontFace='Times New Roman', gmPointSize=22)
return
end

