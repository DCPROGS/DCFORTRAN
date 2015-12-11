subroutine autplot_ini(main,initwin,intoggle,itcall,itogbutton,pfilem,initwin_Text2)

use dfport
use dflib
use menu_f90

 
 character	drives*26,text1*40,text2*40,pfilem*40
character*(100) text_tog(100)
integer intoggle(100)
common/details/text1,text2,drives
imes= hostnam(text1)
call getlog(text2)
drives=getdrivesqq() 

call gDefineRGB(140, 0.011765, 0.827451, 0.803922)
   call gDefineRGB(141, 0.192157, 0.984314, 0.964706)
   call gDefineRGB(142, 0.286275, 0.784314, 0.803922)
call gmsetguigridmode(goff)
! Set up complex dialogue box initwin child of imecform1
 initwin = gmCreateComplexDialogueBox(main,150,100,421,440, GALL, 'Welcome ', &
              	gmhpos=gleft,gmvpos=gtop,gmIconFormat=GDLLICON,gmIconFile='MBIG1052')

! Create main panel for form
   initwin_MainPanel=gmCreatePanel(initwin, 0, 0, 421, 440, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE,gmFillCol=141)
! Create radio box child initwin_Radio3 child of initwin_MainPanel
  
  
! Create text widget initialw_Text1 child of initialw_Panel1
   initialw_Text1 = gmCreateTextEntry(initwin_mainPanel, 150, 10, 300, 25,'AUTPLOT', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=151 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
   call gmSetFontAttribs(initialw_Text1, gmFontFace='Times New Roman', gmPointSize=22)

! Create text widget initialw_Text2 child of initialw_Panel1
   initialw_Text2 = gmCreateTextEntry(initwin_mainPanel, 140, 40, 130, 25,'Version 1.00 (Intel)', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget initialw_Text3 child of initialw_Panel1
   initialw_Text3 = gmCreateTextEntry(initwin_mainPanel, 120, 60, 200, 25,'Copyright 2003 D. Colquhoun & I. Vais ', 255, GDISPLAY, &
      gmBack1Col=0, gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
   call gmSetFontAttribs(initialw_Text3, gmPointSize=8)

  
   
 !Create panel initwin_Panel2 child of initwin_MainPanel
   initwin_Panel2 = gmCreatePanel(initwin_MainPanel, 0,95 , 420, 190, gmTitle='Printout file', gmType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=140, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget initwin_Text2 child of initwin_Panel2
   initwin_Text2 = gmCreateTextEntry(initwin_Panel2, 20, 30, 200, 25,pfilem, 255, GEDIT, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create button initwin_Button3 child of initwin_Panel2
   initwin_Button3 = gmCreatePushButton(initwin_Panel2, 240, 30, 75, 25, 'Browse', &
   gmVpos=GTOP,gmcallback=950)
   initwin_Radio2 = gmCreateRadioBox(initwin_Panel2, 20, 70, 380, 110, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=141, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)

! Create toggle button initwin_Toggle3 child of initwin_Radio2
   inToggle(11) = gmCreateToggleButton(initwin_Radio2, 70, 10, 230, 25, 'Append to existing print file', 0, gmType=G3DRADIO, &
      gmVpos=GTOP,gmcallback=9001)

! Create toggle button initwin_Toggle4 child of initwin_Radio2
   intoggle(12) = gmCreateToggleButton(initwin_Radio2, 70, 40, 230, 25, 'Overwrite/new printout file', 1, gmType=G3DRADIO, &
      gmVpos=GTOP,gmcallback=9002)

! Create toggle button initwin_Toggle5 child of initwin_Radio2
   inToggle(13) = gmCreateToggleButton(initwin_Radio2, 70, 70, 230, 25, 'No printout file', 0, gmType=G3DRADIO,&
    gmVpos=GTOP,gmcallback=9003)

initwin_Panel3 = gmCreatePanel(initwin_MainPanel, 0,285 , 420, 130, gmTitle='Setup for this computer', gmType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=141, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget initwin_Text2 child of initwin_Panel2
 iniTextq = gmCreateTextEntry(initwin_Panel3, 60, 30, 280, 25,'Computer : '//text1, 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=Ghorizontal, gmVpos=GTOP)
  
 iniTextq = gmCreateTextEntry(initwin_Panel3, 60, 60, 280, 25,'User : '//text2, 255, GDIsplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=Ghorizontal, gmVpos=GTOP)
 
 iniTextq = gmCreateTextEntry(initwin_Panel3, 60, 90, 280, 25,'Drives : '//drives, 255, GDIsplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=Ghorizontal, gmVpos=GTOP)

! Create button initwin_Button6 child of initwin_MainPanel
   itogButton = gmCreatePushButton(initwin_MainPanel, 210, 415, 210, 25, 'Continue', &
   gmVpos=GTOP,gmType=GDEFAULTBUTTON,gmcallback=itcall) !-40)
  call gmSetWidgetStatus(itogbutton, GSELECTABLE)
   itogButton = gmCreatePushButton(initwin_MainPanel, 0, 415, 210, 25, 'Exit', &
   gmVpos=GTOP,gmType=GDEFAULTBUTTON,gmcallback=-1000) !-40)
  call gmSetWidgetStatus(itogbutton, GSELECTABLE)
  ! call gmSetWidgetStatus(initwin_Button6, GCHECKED)
   call gmSetGuiGridMode(GOn)
   call gmdrawwindow(initwin)
return
end

