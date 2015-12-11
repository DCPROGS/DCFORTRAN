subroutine change_pathm(main,initwin2,itextwin2,text,qmec,icallid)
use menu_f90
character*60 text,qmec
character*40 qfilem
call gDefineRGB(101, 0.831372, 0.901960, 0.901960)
   call gDefineRGB(102, 0.772549, 0.850980, 0.850980)
   call gDefineRGB(38,1.,0.5,0.25)	!orange
 call gmSetGuiGridMode(GOff)
   initwin2 = gmCreateComplexDialogueBox(Main,200,200,421,120, Gminimize, 'Change path for mechanism file: ', &
              	gmhpos=gleft,gmvpos=gtop,gmIconFormat=GDLLICON,gmIconFile='MBIG1052')

! Create main panel for form
   initwin_MainPanel=gmCreatePanel(initwin2, 0, 0, 421,120 , gmHpos=GCENTRE, gmVpos=Gtop, gmExpand=GON, gmType=GNOBOUNDARY, &
   gmLineBorder=GOUTEREDGE, gmFillCol=101, gmFillBorder=GOUTEREDGE)
initText = gmCreateTextEntry(initwin_mainPanel, 20, 20, 100, 25,'Mechanism file', 255, gdisplay, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 , gmVpos=GTOP)
iTextwin2 = gmCreateTextEntry(initwin_mainPanel, 130, 20, 270, 25,qmec, 255, gedit, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 , gmVpos=GTOP)
!initwin_Button6 = gmCreatePushButton(initwin_MainPanel, 10, 0, 190, 25, 'Save changes to ini  file', &
!    gmVpos=Gbottom,gmType=GDEFAULTBUTTON,gmcallback=8004) !-40)

!iTextwin2 = gmCreateTextEntry(initwin_mainPanel, 140, 50, 260, 25,text, 255, GEDIT, gmBack1Col=0, gmBack2Col=0, &
 !     gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
!iText = gmCreateTextEntry(initwin_mainPanel, 20, 50, 120, 25,'Path:(e.g. D:\hjcfit\ )', 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
 !     gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

initwin2_Button_m = gmCreatePushButton(initwin_MainPanel, 250, 50, 80, 25, 'Browse', &
    gmVpos=Gtop,gmType=GDEFAULTBUTTON,gmcallback=-236) !-40)

 initwin2_Button6 = gmCreatePushButton(initwin_MainPanel, 10, 0, 190, 25, 'Save changes to ini  file', &
    gmVpos=Gbottom,gmType=GDEFAULTBUTTON,gmcallback=8007) !-40)
initwin2_Button7 = gmCreatePushButton(initwin_MainPanel, 220, 0, 190, 25, 'Continue', &
    gmVpos=Gbottom,gmType=Guserdefined,gmOffcol=38,gmcallback=icallid) !-40)
 call gmSetGuiGridMode(GOn)
   call gmdrawwindow(initwin2)
  end