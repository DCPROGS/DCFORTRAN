subroutine samp_ini(iform,isampw,tempfile,istoggle1,istoggle2,istext)
use dfport
use dflib
use menu_f90
character	drives*26,text1*40,text2*40
character*60 tempfile 

integer panel20

common/details/text1,text2,drives
imes= hostnam(text1)
call getlog(text2)
drives=getdrivesqq()  

isampw = gmCreateComplexDialogueBox(iform,12,4,24,24, GALL, ' ', &
              	gmhpos=gleft,gmvpos=gtop,gmIconFormat=GDLLICON,gmIconFile='MBIG1052')

initwin_Panel20 = gmCreatePanel(isampw, 0,0 , 24, 24, gmTitle='', gmType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=101, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)


 inittext = gmCreateTextEntry(initwin_Panel20, 6, 2, 12, 1,'WELCOME TO PLOTSAMP', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=151 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
  
  
  call gmSetFontAttribs(initText, gmFontFace='Times New Roman', gmPointSize=22)
 
 
   initialw_Text1 = gmCreateTextEntry(initwin_Panel20, 6, 4, 12, 1,'PLOT AND FILTER SAMPLE', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=151 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
   call gmSetFontAttribs(initialw_Text1, gmFontFace='Times New Roman', gmPointSize=22)

! Create text widget initialw_Text2 child of initialw_Panel1
   initialw_Text2 = gmCreateTextEntry(initwin_Panel20, 9, 5, 6, 1,'Version 1.00 (Intel)', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget initialw_Text3 child of initialw_Panel1
   initialw_Text3 = gmCreateTextEntry(initwin_Panel20, 8, 6, 10, 1,'Copyright 2003 D. Colquhoun & I. Vais ', 255, GDISPLAY, &
      gmBack1Col=0, gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
   call gmSetFontAttribs(initialw_Text3, gmPointSize=8)

 
 inittext1 = gmCreateTextEntry(initwin_Panel20, 5, 7, 15, 1,'(Please read the Help provided to get started)', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=151 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
  
  
  call gmSetFontAttribs(initText1, gmFontFace='Times New Roman', gmPointSize=18)
  
Panel20 = gmCreatePanel(initwin_Panel20, 3,9 , 18, 10, gmTitle='Auto-restart of previous analysis?', gmType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillCol=104, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)


! Create text widget initwin_Text2 child of initwin_Panel2
  istext = gmCreateTextEntry(Panel20, 1, 2, 12, 1,tempfile, 255, GEDIT, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=Ghorizontal, gmVpos=GTOP)

! Create button initwin_Button3 child of initwin_Panel2
   initwin_Button3 = gmCreatePushButton(Panel20, 14, 2, 3, 1, 'Browse', &
   gmVpos=GTOP,gmcallback=-3)

    initwin_Radio20 = gmCreateRadioBox(Panel20,1, 4,16 , 4, gmType=GFREEFORM, gmBorderType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=222, gmFillBorder=GOUTEREDGE, gmVpos=GTOP)

 
! Create toggle button initwin_Toggle3 child of initwin_Radio2
   isToggle1= gmCreateToggleButton(initwin_Radio20, 1, 1, 14, 1, &
   'Yes(if a temp file exist for the file you intend to analyse)',&
	0, gmType=G3DRADIO, &
      gmVpos=GTOP)

! Create toggle button initwin_Toggle4 child of initwin_Radio2
   istoggle2 = gmCreateToggleButton(initwin_Radio20, 1, 2, 3, 1, 'No', 0, gmType=G3DRADIO, &
      gmVpos=GTOP)
  
  isButton = gmCreatePushButton(Panel20, 6, 8, 6, 1, 'Continue', &
              	gmType=GUSERDEFINED, gmAccel=0, &
              	gmOncol=0, gmOffCol=225, gmTextCol=0, &
              	gmVpos=Gtop, gmExpand=GOFF, gmCallback=-2) ! 110 
					

! Create text widget initwin_Text2 child of initwin_Panel2
 iniTextq = gmCreateTextEntry(initwin_Panel20, 9, 20, 10, 1,'Computer : '//text1, 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=Ghorizontal, gmVpos=GTOP)
  
 iniTextq = gmCreateTextEntry(initwin_Panel20, 9, 21, 10, 1,'User : '//text2, 255, GDIsplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=Ghorizontal, gmVpos=GTOP)
 call gmdrawwindow(isampw)
end