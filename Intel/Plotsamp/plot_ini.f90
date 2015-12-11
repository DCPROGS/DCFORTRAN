!     Last change:  D    11 Mar 105    4:44 pm
subroutine plot_ini(iform,initwin,ntog,text_tog,intoggle,itcall,itogbutton,pfile,&
	initext,inifile,initext1,append,itogrec)


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
! Set up complex dialogue box initwin child of imecform1
!iy=310
iy=640
iy=460
IF(itcall.lE.0) iy=150
 initwin = gmCreateComplexDialogueBox(iform,350,100,480,iy+30*(ntog+1), GALL, 'Welcome ', &
              	gmhpos=gleft,gmvpos=gtop,gmIconFormat=GDLLICON,gmIconFile='MBIG1052')

! Create main panel for form
   initwin_MainPanel=gmCreatePanel(initwin, 0, 0, 480, iy+30*(ntog+1), gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE,gmFillCol=220)
! Create radio box child initwin_Radio3 child of initwin_MainPanel
  
  
! Create text widget initialw_Text1 child of initialw_Panel1
 

  
   initwin_Radio3 = gmCreateRadioBox(initwin_MainPanel, 30, 30, 420, 30*(ntog+1)+5, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=113, gmFillBorder=GOUTEREDGE, gmTitle='Choose file type', gmVpos=GTOP)

! Create toggle button initwin_Toggle6 child of initwin_Radio3

do i=1,ntog
   intoggle(i) = gmCreateToggleButton(initwin_Radio3, 40, 30*i, 290, 25, text_tog(i), 0, &
   gmType=G3DRADIO, gmVpos=GTOP)
enddo
call gmsettoggleswitch(intoggle(itogrec),gon)
IF(itcall.gt.0) then
 initwin_Panel20 = gmCreatePanel(initwin_MainPanel, 30,30*(ntog+1)+50 , 420, 160, gmTitle='Inifile', gmType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=111, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget initwin_Text2 child of initwin_Panel2
  iniText1 = gmCreateTextEntry(initwin_Panel20, 20, 30, 280, 25,inifile, 255, GEDIT, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create button initwin_Button3 child of initwin_Panel2
   initwin_Button3 = gmCreatePushButton(initwin_Panel20, 315, 30, 75, 25, 'Browse', &
   gmVpos=GTOP,gmcallback=50)

    initwin_Radio20 = gmCreateRadioBox(initwin_Panel20, 20, 70, 380, 80, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=222, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)

 
! Create toggle button initwin_Toggle3 child of initwin_Radio2
   inToggle(14) = gmCreateToggleButton(initwin_Radio20, 20, 10, 355, 25, 'Use inifile (only if you want to analyse a previous file)', 0, gmType=G3DRADIO, &
      gmVpos=GTOP,gmCallback=18)
   
! Create toggle button initwin_Toggle4 child of initwin_Radio2
   intoggle(15) = gmCreateToggleButton(initwin_Radio20, 20, 40, 230, 25, 'Use default values (no inifile)', 1, gmType=G3DRADIO, &
      gmVpos=GTOP,gmCallback=19)
  
     initwin_Panel2 = gmCreatePanel(initwin_MainPanel, 30,30*(ntog+1)+220 , 420, 190, gmTitle='Printout file', gmType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=112, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget initwin_Text2 child of initwin_Panel2
   iniText = gmCreateTextEntry(initwin_Panel2, 20, 30, 280, 25,pfile, 255, GEDIT, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create button initwin_Button3 child of initwin_Panel2
   initwin_Button3 = gmCreatePushButton(initwin_Panel2, 315, 30, 75, 25, 'Browse', &
   gmVpos=GTOP,gmcallback=5)
   initwin_Radio2 = gmCreateRadioBox(initwin_Panel2, 20, 70, 380, 110, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=222, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)

 
! Create toggle button initwin_Toggle3 child of initwin_Radio2
   inToggle(11) = gmCreateToggleButton(initwin_Radio2, 70, 10, 230, 25, 'Append to existing print file', 0, gmType=G3DRADIO, &
      gmVpos=GTOP,gmCallback=2)

! Create toggle button initwin_Toggle4 child of initwin_Radio2
   intoggle(12) = gmCreateToggleButton(initwin_Radio2, 70, 40, 230, 25, 'Overwrite/new printout file', 0, gmType=G3DRADIO, &
      gmVpos=GTOP,gmCallback=3)

! Create toggle button initwin_Toggle5 child of initwin_Radio2
   inToggle(13) = gmCreateToggleButton(initwin_Radio2, 70, 70, 230, 25, 'No printout file', 0, &
   gmType=G3DRADIO, gmVpos=GTOP,gmCallback=4)
if(append) then 
call gmsettoggleswitch(intoggle(11),gon)
else
call gmsettoggleswitch(intoggle(12),gon)
endif

 
! iniTextq = gmCreateTextEntry(initwin_Panel3, 60, 90, 280, 25,'Drives : '//drives, 255, GDIsplay, gmBack1Col=0, gmBack2Col=0, &
 !     gmTextCol=16 ,gmScrollMode=Ghorizontal, gmVpos=GTOP)
  endif
 
  IF(itcall.le.0) then
  !	itcall=1
        iy=95
  else
       ! iy=285
	   iy=620
  endif
  call gmSetWidgetStatus(intoggle(3), GSELECTABLE)
  ! call gmSetWidgetStatus(intoggle(4), GunSELECTABLE)
   call gmSetWidgetStatus(intoggle(5), GunSELECTABLE)
! Create button initwin_Button6 child of initwin_MainPanel
   itogButton = gmCreatePushButton(initwin_MainPanel, 250, 10, 200, 25, 'Continue', &
   gmVpos=Gbottom,gmType=GDEFAULTBUTTON,gmcallback=itcall) !-40)
  call gmSetWidgetStatus(itogbutton, GSELECTABLE)
   itogButton = gmCreatePushButton(initwin_MainPanel, 30, 10, 200, 25, 'Exit', &
   gmVpos=Gbottom,gmType=GDEFAULTBUTTON,gmcallback=6) !-40)
  call gmSetWidgetStatus(itogbutton, GSELECTABLE)
  ! call gmSetWidgetStatus(initwin_Button6, GCHECKED)
   call gmSetGuiGridMode(GOn)
   call gmdrawwindow(initwin)
   call gmsettoggleswitch(intoggle(itogrec),gon)
return
end

