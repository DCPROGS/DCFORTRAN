subroutine main_consam(iform1,ipanel1_1,icounter,countval,graph,static1_14,val1_25,tval1_25,iedit1_1,itedit1_1,&
	iedit1_2,itedit1_2,ibutton1,ibutton2,edit1_1,tedit1_1,edit1_2,tedit1_2,edit1_4,tedit1_4,&
	val1_3,tval1_3,val1_6,tval1_6,val1_7,tval1_7,val1_11,tval1_11,val1_12,tval1_12,&
	button1,toggle1_5,toggle1_8,toggle1_9,edit2_2,tedit2_2,edit2_3a,tedit2_3a,edit2_3,tedit2_3,&
	edit2_4,tedit2_4,edit2_5,tedit2_5,edit2_7,tedit2_7,edit2_8,tedit2_8,edit2_12,tedit2_12,&
	edit2_13a,tedit2_13a,edit2_13,tedit2_13,&
	edit2_14,tedit2_14,edit2_15,tedit2_15,edit2_17,tedit2_17,edit2_18,tedit2_18,&
	iform4,button4,iform5,toggle5,iform6,toggle6,iform7)

use gino_f90
use menu_f90
integer :: iform1
integer :: iform1MainPanel
integer :: Panel1
integer :: Panel1_1
integer :: Panel1_1_1
integer :: Panel1_2
integer :: Static0
integer :: Static1
integer :: Static1_1 
integer :: Static1_2 
integer :: Static1_3 
integer :: Static1_4 
integer :: Static1_5 
integer :: Static1_6 
integer :: Static1_7 
integer :: Static1_8 
integer :: Static1_9 
integer :: Static1_10 
integer :: Static1_11 
integer :: Static1_12
integer :: Static1_13
integer :: Static1_14

integer :: Edit1_1 
integer :: Edit1_2 
integer :: val1_3 
integer :: Edit1_4 
integer :: val1_6 
integer :: val1_7 
integer :: Edit1_10 
integer :: val1_11 
integer :: val1_25 
integer :: val1_12 
integer :: Toggle1_5(5)
integer :: Toggle1_8(5)
integer :: Button1(5)
integer  Toggle1_9(3)
integer :: Button1_10
integer :: Edit2_2 
integer :: Edit2_3a 
integer :: Edit2_3 
integer :: Edit2_4 
integer :: Edit2_5
integer :: Edit2_6 
integer :: Edit2_7 
integer :: Edit2_8 
integer :: Edit2_9 

integer :: Edit2_12 
integer :: Edit2_13a 
integer :: Edit2_13 
integer :: Edit2_14 
integer :: Edit2_15
integer :: Edit2_16 
integer :: Edit2_17 
integer :: Edit2_18 
integer :: Toggle2_6(2)
integer :: Toggle2_16(2)
integer :: Panel2
integer :: Static2_00
integer :: Static2_01
integer :: Static2_1 
integer :: Static2_2 
integer :: Static2_3 
integer :: Static2_3a 
integer :: Static2_4 
integer :: Static2_5 
integer :: Static2_6 
integer :: Static2_7 
integer :: Static2_8 
integer :: Static2_9 
integer :: Static2_11 
integer :: Static2_12
integer :: Static2_13 
integer :: Static2_14 
integer :: Static2_15 
integer :: Static2_16 
integer :: Static2_17 
integer :: Static2_18 
integer :: Static2_19 

real :: tedit2_2, tedit2_12
real :: tedit2_3a, tedit2_13a
real :: tedit2_3, tedit2_13
real :: tedit2_4, tedit2_14
real :: tedit2_5, tedit2_15
real :: tedit2_7, tedit2_17
real :: tedit2_8, tedit2_18
integer :: graph
type (GLIMIT)    :: limits 
type (GWIDGET)   :: grframe

integer :: Sliderh1
type(GARRAYCELL) arrayattribs
integer :: ISTBAR(32)
integer :: callid
type (GWIDGET) :: Slider_attribs
type (GACTION) :: actlst
character*70 tEdit1_1,tedit1_2,tedit1_4,tedit1_10, itEdit1_1,itedit1_2
integer :: iform3
integer :: iform3MainPanel
integer :: Panel3
integer :: Static3_1 
integer :: Static3_2 
integer :: Static3_3 
integer :: Static3_4 
integer :: Static3_5 
integer :: Static3_6
integer :: Button3(2)

integer :: iform4,iform2
integer :: iform4MainPanel
integer :: Panel4
integer :: Static4 
integer :: Button4

integer :: iform5
integer :: iform5MainPanel
integer :: Panel5
integer :: Static5 
integer :: Toggle5(2)

integer :: iform6
integer :: iform6MainPanel
integer :: Panel6
integer :: Static6_1
integer :: Toggle6(2) 

integer :: iform7
integer :: iform7MainPanel
integer :: Panel7
integer :: Static7_1
integer :: Static7_2
integer :: Static7_3
integer :: Static7_4

! Set up master window iform1
   iform1 = gmCreateMasterWindow(1, 2, 36, 30, GALL, &
   'CONTINUOUS SAMPLING  OF ADC DATA TO DISK (Copyright 2003 D. Colquhoun & I. Vais)', &
              	gmVpos=GTOP, gmIconFormat=GDLLICON,gmIconFile='MBIG1005')

! Create main panel for form
   iform1MainPanel=gmCreatePanel(iform1, 0, 0, 36, 30, &
              	gmHpos=GLEFT, gmVpos=Gmiddle, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=15, gmFillBorder=0)

! Create panel Panel1 child of Form1MainPanel
   Panel1 = gmCreatePanel(iform1MainPanel, 0, 0, 36, 30, &
              	gmType=GCHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=227, gmFillBorder=0, &
              	gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)

  
	icon_help1= gmCreateIcon(Panel1,3,25,1,1,'MSML1064',GDLLICON,0,gmCallback=100, gmhelp='Help')
   


! Create panel Panel1 child of Form1MainPanel
   
	
! Create panel Panel1 child of Form1MainPanel
   Panel1_1 = gmCreatePanel(Panel1, 0, 3, 24, 4, &
              	gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=221, &                                                 
				gmFillBorder=0,&
              	gmHpos=GLEFT, gmVpos=GTOP, gmExpand=GOFF)


! Create text entry Static1_13 child of Panel6
   Static1_13 = gmCreateTextEntry(Panel1_1, 10, 2, 14, 1,'Note: Press "Esc" key to stop sampling before the end.', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)
 
  Static1_14 = gmCreateTextEntry(Panel1_1, 10, 1, 14, 1,'Fill in/check experimental details', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=gblue, &
              	gmHpos=Gcentre, gmVpos=GTOP, gmExpand=GOFF)

  Icon0 = gmCreateIcon(Panel1_1,1,1,2,2 ,'MBIG1064',  &
    			GDLLICON,0,&
           gmHpos=Gleft, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'HELP',gmcallback = 200)
              
 ICOUNTER = gmCreateValueEntry(Panel1, 1, 1, 6, 1,COUNTVAL, 32768,2, GEDIT, &
              	gmType=GSTANDARD, gmJustify=Gcentre, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=115)

! Create text entry Static1_14 child of Panel6
   !  iStatic1_14 = gmCreateTextEntry(Panel1_1, 1, 5, 20, 1,'TO STOP SAMPLING PRESS THE ESCAPE "Esc" KEY ', 32768, GDISPLAY, &
  !            	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=Gleft, &
   !           	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
    !          	gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)

   ! Create panel Panel1 child of Form1MainPanel
    val1_25 = gmCreateValueEntry(Panel1, 31, 1, 3, 1,tval1_25, 32768,0, GDIsplay, &
              	gmType=GSTANDARD, gmJustify=Gright, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmHpos=Gleft,gmVpos=Gtop, gmExpand=GOFF)

   Panel1_1_1 = gmCreatePanel(Panel1, 10, 1, 20, 1, &
              	gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=12, gmFillBorder=0, &
              	gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)

       
		  limits%xmin=0.
		  limits%xmax=20.
		  limits%ymin=0.
		  limits%ymax=1.
		  graph=gmCreateGraphicsFrame(Panel1_1_1,0,0,20,1,limits, &
          gmHpos=GCENTRE, gmVpos=GTOP, gmExpand=GOFF, &
                                gmCallback=-4)
		  call gmEnqWidgetInfo(graph,grframe)
          call gmActivateGraphicsFrame(graph)
          call gFillRect(GSOLID,GWHITE,limits)
		  call gsetlinecolour(GBLACK)
          call gmoveto2d(0.,0.)
          call gdrawlineto2d(20.,0.)
          call gdrawlineto2d(20.,1.)                                                               
          call gdrawlineto2d(0.,1.)
          call gdrawlineto2d(0.,0.)
		  call gFlushGraphics

iPanel1 = gmCreatePanel(Panel1, 0, 7, 24, 5, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, gmFillcol=223, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text entry Static1_1 child of Panel1
   IStatic1_1 = gmCreateTextEntry(iPanel1, 1, 1, 3, 1,'Inifile', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
! Create text entry Edit1_1 child of Panel1
  
   IEdit1_1 = gmCreateTextEntry(iPanel1, 4, 1, 13, 1,itedit1_1, 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=101)
! Create text entry Static1_1 child of Panel1
   IStatic1_2 = gmCreateTextEntry(iPanel1, 1, 3, 3, 1,'Data file ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
! Create text entry Edit1_1 child of Panel1
  
   IEdit1_2 = gmCreateTextEntry(iPanel1, 4, 3, 13, 1,itedit1_2, 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=102)
IStatic1_2 = gmCreateTextEntry(iPanel1, 4, 4, 18, 1,&
'[path+name(max 8 characters)+extension(.ssd) i.e.:D:\010105C1.ssd]', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
 iButton1 = gmCreatePushButton(iPanel1, 18, 1, 5, 1, 'Browse', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=41)

Button1(5) = gmCreatePushButton(iPanel1, 18, 2, 5, 1, 'Save settings', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=17)

! Create button Button1_4 child of Panel1
   iButton2 = gmCreatePushButton(iPanel1, 18, 3, 5, 1, 'Browse', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=42)



 iPanel2 = gmCreatePanel(Panel1, 0, 12, 24, 10, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, gmFillcol=220, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text entry Static1 child of Panel1
   Static1 = gmCreateTextEntry(iPanel2, 6, 1, 8, 1,'EXPERIMENT/ PATCH DETAILS ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
! Create text entry Static1_2 child of Panel1
   Static1_2 = gmCreateTextEntry(iPanel2, 1, 2, 4, 1,'Tape details ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit1-2 child of Panel1
   
  Edit1_2 = gmCreateTextEntry(iPanel2, 6, 2, 17, 1,tedit1_2, 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=108)
! Create text entry Static1_1 child of Panel1
   Static1_1 = gmCreateTextEntry(iPanel2, 1, 3, 10, 1,'Date [dd-mmm-yyyy; i.e. 01-Jan-2001] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
! Create text entry Edit1_1 child of Panel1
  
   Edit1_1 = gmCreateTextEntry(iPanel2, 11, 3, 4, 1,tedit1_1, 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=103)


! Create text entry Static1_3 child of Panel1
   Static1_3 = gmCreateTextEntry(iPanel2, 16, 3, 4, 1,'Patch number ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit1_3 child of Panel1
  	
   val1_3 = gmCreateValueEntry(iPanel2, 20, 3, 3, 1,tval1_3, 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=104)

! Create text entry Static1_4 child of Panel1
   Static1_4 = gmCreateTextEntry(iPanel2, 1, 4, 5, 1,'Patch name ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit1_4 child of Panel1
   
   Edit1_4 = gmCreateTextEntry(iPanel2, 6, 4, 17, 1,tedit1_4, 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=105)


! Create text entry Static1_6 child of Panel1
   Static1_6 = gmCreateTextEntry(iPanel2, 1, 5, 7, 1,'Membrane potential [mV] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit16 child of Panel1
   
   val1_6 = gmCreateValueEntry(iPanel2, 8, 5, 3, 1,tval1_6, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=106)

! Create text entry Static1_7 child of Panel1
   Static1_7 = gmCreateTextEntry(iPanel2, 12, 5, 5, 1,'Temperature [o C] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit1_7 child of Panel1
   
   val1_7 = gmCreateValueEntry(iPanel2, 18, 5, 5, 1,tval1_7, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=107)

! Create text entry Static1_5 child of Panel1
   Static1_5 = gmCreateTextEntry(iPanel2, 1, 6, 3, 1,'Patch type: ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)


! Create toggle button Toggle1_5 child of Panel1
   Toggle1_5(1) = gmCreateToggleButton(iPanel2, 1, 7, 4, 1, 'Outside-out', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=151)

! Create toggle button Toggle1_5 child of Panel1
   Toggle1_5(2) = gmCreateToggleButton(iPanel2, 5, 7, 4, 1, 'Inside-out', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=152)

! Create toggle button Toggle1_5 child of Panel1
   
   Toggle1_5(3) = gmCreateToggleButton(iPanel2, 9, 7, 4, 1, 'Cell-attached', itogon, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=153)

! Create toggle button Toggle1_5 child of Panel1
   Toggle1_5(4) = gmCreateToggleButton(iPanel2, 14, 7, 4, 1, 'Whole-cell', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=154)

! Create toggle button Toggle1_5 child of Panel1
   Toggle1_5(5) = gmCreateToggleButton(iPanel2, 18, 7, 5, 1, 'Simulated data', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=155)


 iPanel3 = gmCreatePanel(Panel1, 0, 21, 24, 6, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, gmFillcol=222, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)


! Create text entry Static1_8 child of Panel1
   Static1_8 = gmCreateTextEntry(iPanel3, 10, 1, 4, 1,'SAMPLING ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
! Create toggle button Toggle1_8 child of Panel1
   Toggle1_8(3) = gmCreateToggleButton(iPanel3, 1, 2, 10, 1, 'Sample live', itogon, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=262)

! Create toggle button Toggle1-8 child of Panel1
   Toggle1_8(4) = gmCreateToggleButton(iPanel3, 12, 2, 11, 1, 'Sample from tape ', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=261)


! Create toggle button Toggle1_8 child of Panel1
   Toggle1_8(1) = gmCreateToggleButton(iPanel3, 1, 4, 10, 1, 'Sample one channel only  (ADC0)', itogon, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=181)

! Create toggle button Toggle1-8 child of Panel1
   Toggle1_8(2) = gmCreateToggleButton(iPanel3, 12, 4, 11, 1, 'Sample two channels  (ADC0 and ADC1)', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=182)

! Create text entry Static1_11 child of Panel1
   Static1_11 = gmCreateTextEntry(iPanel3, 1, 3, 7, 1,'Sampling rate (integer) [ Hz] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit1_11 child of Panel1
   val1_11 = gmCreateValueEntry(iPanel3, 8, 3, 3, 1,tval1_11, 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=109)

! Create text entry Static1_12 child of Panel1
   Static1_12 = gmCreateTextEntry(iPanel3, 12, 3, 7, 1,'Sample duration [seconds] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit1_12 child of Panel1
   val1_12 = gmCreateValueEntry(iPanel3, 20, 3, 3, 1,tval1_12, 32768,2, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=110)



! Create button Button1_10 child of Panel1
!   Button1_10 = gmCreatePushButton(Panel1, 3, 24, 8, 1, 'Save as CONSAM file', &
!              	gmType=GSTANDARD, gmAccel=0, &
!              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=110)




   Button1(1) = gmCreatePushButton(Panel1, 25, 28, 7, 1, 'Clear all', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=11)

! Create button Button1_2 child of Panel1
   Button1(2) = gmCreatePushButton(Panel1,18 , 28, 7, 1, 'Quit', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=12)
 
! Create button Button1_3 child of Panel1
   Button1(3) = gmCreatePushButton(Panel1, 11, 28, 7, 1, 'Wait for trigger (on Ev 4)', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=13)

! Create button Button1_4 child of Panel1
   Button1(4) = gmCreatePushButton(Panel1, 4, 28, 7, 1, 'Sampling', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=14)

! Create button Button1_5 child of Panel1
!   Button1(5) = gmCreatePushButton(Panel1, 10, 25, 5, 1, 'Stop sampling', &
!              	gmType=GSTANDARD, gmAccel=0, &
!              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
!              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=15,gmSelect=-15)


				! Create panel Panel2 child of Form2MainPanel
   Panel2 = gmCreatePanel(Panel1, 24, 3, 12, 24, &
              	gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=38, gmFillBorder=0, &
              	gmHpos=Gleft, gmVpos=Gtop, gmExpand=GOFF)
! Create text entry Static1_9 child of Panel1
   Static1_9 = gmCreateTextEntry(Panel2, 4, 1, 4, 1,'CALIBRATION ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle button Toggle1_9 child of Panel1
   Toggle1_9(1) = gmCreateToggleButton(Panel2, 1, 2, 7, itogon, 'Overall calibration factor', itogon, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=191)

! Create toggle button Toggle1_9 child of Panel1
   Toggle1_9(2) = gmCreateToggleButton(Panel2, 1, 3, 7, 1, 'Separate clamp gain', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=192)

! Create toggle button Toggle1_9 child of Panel1
   Toggle1_9(3) = gmCreateToggleButton(Panel2, 1, 4, 5, 1, 'No calibration', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=5, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=193)



! Create text entry Static2_00 child of Panel2
   Static2_00 = gmCreateTextEntry(Panel2, 4, 6, 4, 1,'ADC0 ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static2_2 child of Panel2
   Static2_2 = gmCreateTextEntry(Panel2, 1, 7, 6, 1,'Calibration [V/pA] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
   
! Create text entry Edit2_2 child of Panel2
   Edit2_2 = gmCreateValueEntry(Panel2, 7, 7, 4, 1,tedit2_2, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=202)

! Create text entry Static2_2 child of Panel2
   Static2_3a = gmCreateTextEntry(Panel2, 1, 8, 6, 1,'Clamp setting [mV/pA] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
   
! Create text entry Edit2_2 child of Panel2
   Edit2_3a = gmCreateValueEntry(Panel2, 7, 8, 4, 1,tedit2_3a, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=2031)

! Create text entry Static2_3 child of Panel2
   Static2_3 = gmCreateTextEntry(Panel2, 1, 9, 4, 1,'Amplifier gain ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_3 child of Panel2
   Edit2_3 = gmCreateValueEntry(Panel2, 5, 9, 6, 1,tedit2_3, 32768, 3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=203)

! Create text entry Static2_4 child of Panel2
   Static2_4 = gmCreateTextEntry(Panel2, 1, 10, 4, 1,'Error factor ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_4 child of Panel2
   Edit2_4 = gmCreateValueEntry(Panel2, 5, 10, 6, 1,tedit2_4, 32768, 3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=204)

! Create text entry Static2_5 child of Panel2
   Static2_5 = gmCreateTextEntry(Panel2, 1, 11, 6, 1,'Filter setting (-3dB) [Hz] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_6 child of Panel2
   Edit2_5 = gmCreateVAlueEntry(Panel2, 7, 11, 4, 1,tedit2_5, 32768, 3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=205)


! Create text entry Static2_7 child of Panel2
   Static2_7 = gmCreateTextEntry(Panel2, 1, 12, 4, 1,'Tape recorder ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_7 child of Panel2
   Edit2_7 = gmCreateValueEntry(Panel2, 5, 12, 6, 1,tedit2_7, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=207)

! Create text entry Static2_8 child of Panel2
   Static2_8 = gmCreateTextEntry(Panel2, 1, 13, 5, 1,'Tape speed factor ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_8 child of Panel2
   Edit2_8 = gmCreateValueEntry(Panel2, 6, 13, 5, 1,tedit2_8, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=208)
!
!	ADC1
!

! Create text entry Static2_01 child of Panel2
   Static2_01 = gmCreateTextEntry(Panel2, 4, 15, 4, 1,'ADC1 ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static2_12 child of Panel2
   Static2_12 = gmCreateTextEntry(Panel2, 1, 16, 6, 1,'Calibration [V/pA] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_12 child of Panel2
   Edit2_12 = gmCreateValueEntry(Panel2, 7, 16, 4, 1,tedit2_12, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=212)

! Create text entry Static2_12 child of Panel2
   Static2_13a = gmCreateTextEntry(Panel2, 1, 17, 6, 1,'Clamp setting [mV/pA] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_12 child of Panel2
   Edit2_13a = gmCreateValueEntry(Panel2, 7, 17, 4, 1,tedit2_13a, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=2131)

! Create text entry Static2_13 child of Panel2
   Static2_13= gmCreateTextEntry(Panel2, 1, 18, 4, 1,'Amplifier gain ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_13 child of Panel3
   Edit2_13 = gmCreateValueEntry(Panel2, 5, 18, 6, 1,tedit2_13, 32768, 3,GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=213)

! Create text entry Static2_14 child of Panel2
   Static2_14 = gmCreateTextEntry(Panel2, 1, 19, 4, 1,'Error factor ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_14 child of Panel2
   Edit2_14 = gmCreateValueEntry(Panel2, 5, 19, 6, 1,tedit2_14, 32768, 3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=214)

! Create text entry Static2_15 child of Panel2
   Static2_15 = gmCreateTextEntry(Panel2, 1, 20, 6, 1,'Filter setting (-3dB) [Hz] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_15 child of Panel2
   Edit2_15 = gmCreateValueEntry(Panel2, 7, 20, 4, 1,tedit2_15, 32768, 3,GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=215)


! Create text entry Static2_17 child of Panel2
   Static2_17 = gmCreateTextEntry(Panel2, 1, 21, 4, 1,'Tape recorder ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_17 child of Panel2
   Edit2_17 = gmCreateValueEntry(Panel2, 5, 21, 6, 1,tedit2_17, 32768, 3,GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=217)

! Create text entry Static2_18 child of Panel2
   Static2_18 = gmCreateTextEntry(Panel2, 1, 22, 5, 1,'Tape speed factor ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_18 child of Panel2
   Edit2_18 = gmCreateValueEntry(Panel2, 6, 22, 5, 1,tedit2_18, 32768, 3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=218)
 call gmSetWidgetStatus(Edit2_2,GUNSELECTABLE)
   call gmSetWidgetStatus(Edit2_5,GUNSELECTABLE)
   call gmSetWidgetStatus(Edit2_7,GUNSELECTABLE)
   call gmSetWidgetStatus(Edit2_8,GUNSELECTABLE)
   call gmSetWidgetStatus(Edit2_17,GUNSELECTABLE)
   call gmSetWidgetStatus(Edit2_18,GUNSELECTABLE)
  
    call gmSetWidgetStatus(Edit2_3,GUNSELECTABLE)
	  call gmSetWidgetStatus(Edit2_3a,GUNSELECTABLE)
	  call gmSetWidgetStatus(Edit2_4,GUNSELECTABLE)
	 
		   call gmSetWidgetStatus(Edit2_12,GUNSELECTABLE)
   call gmSetWidgetStatus(Edit2_15,GUNSELECTABLE)
	  call gmSetWidgetStatus(Edit2_13,GUNSELECTABLE)
	  call gmSetWidgetStatus(Edit2_13a,GUNSELECTABLE)
	  call gmSetWidgetStatus(Edit2_14,GUNSELECTABLE)
	  
		  
!----------------------------------------------------------

! Set up complex dialogue box Form4 child of Form1
   iform4 = gmCreateComplexDialogueBox(iform1, 13, 12, 16, 5, GALL, 'Stop', &
              	gmVpos=GTOP, gmIconFormat=GDLLICON,gmIconFile='MBIG1079')

! Create main panel for form
   iform4MainPanel=gmCreatePanel(iform4, 0, 0, 16, 5, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=14, gmFillBorder=0)

! Create text entry Static4 of Form4MainPanel
   Static4 = gmCreateTextEntry(iform4MainPanel, 1, 1, 14, 1,'Please fill all the fields correctly before proceeding ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=2, gmBack2Col=0, gmTextCol=10, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create button Button4_1 child of Form4MainPanel
   Button4 = gmCreatePushButton(iform4MainPanel, 7, 3, 2, 1, 'OK', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=40)

! Set up complex dialogue box Form5 child of Form1
   iform5 = gmCreateComplexDialogueBox(iform1, 15, 10, 16, 5, GALL, 'Stop sampling', &
              	gmVpos=GTOP, gmIconFormat=GDLLICON,gmIconFile='MBIG1080')

! Create main panel for form
   iform5MainPanel=gmCreatePanel(iform5, 0, 0, 16, 5, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=14, gmFillBorder=0)

! Create text entry Static5 child of iform5MainPanel
   Static5 = gmCreateTextEntry(iform5MainPanel, 8, 1, 12, 1,'Are you sure you want to stop sampling ? ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=2, gmBack2Col=0, gmTextCol=10, &
              	gmHpos=GCENTRE, gmVpos=GTOP, gmExpand=GOFF)

! Create toggle button Toggle5_1 child of iform5MainPanel
   Toggle5(1) = gmCreateToggleButton(iform5MainPanel, 5, 3, 4, 1, 'Yes', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=51)

! Create toggle button Toggle5_1 child of iform5MainPanel
   Toggle5(2) = gmCreateToggleButton(iform5MainPanel, 9, 3, 4, 1, 'No', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=52)

! Set up complex dialogue box iform6 child of iform1
   iform6 = gmCreateComplexDialogueBox(iform1, 9, 6, 16, 5, GALL, 'Quit', &
              	gmVpos=GTOP, gmIconFormat=GDLLICON,gmIconFile='MBIG1077')

! Create main panel for iform
   iform6MainPanel=gmCreatePanel(iform6, 0, 0, 16, 5, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

! Create panel Panel6 child of iform6MainPanel
   Panel6 = gmCreatePanel(iform6MainPanel, 0, 0, 16, 5, &
              	gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=14, gmFillBorder=0, &
              	gmHpos=GCENTRE, gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static6_1 child of Panel6
   Static6_1 = gmCreateTextEntry(Panel6, 2, 1, 12, 1,'Do you really want to quit? ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=2, gmBack2Col=0, gmTextCol=4, &
              	gmHpos=GCENTRE, gmVpos=GTOP, gmExpand=GOFF)

! Create toggle button Toggle6_1 child of iform6MainPanel
   Toggle6(1) = gmCreateToggleButton(Panel6, 5, 3, 4, 1, 'Yes', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=61)

! Create toggle button Toggle6_1 child of iform6MainPanel
   Toggle6(2) = gmCreateToggleButton(Panel6, 9, 3, 4, 1, 'No', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=62)

! Set up complex dialogue box iform7 child of iform1
   iform7 = gmCreateComplexDialogueBox(iform1, 9, 6, 14, 8, GALL, 'About', &
              	gmVpos=GTOP, gmIconFormat=GDLLICON,gmIconFile='MBIG1077')
! Create main panel 7
   iform7MainPanel=gmCreatePanel(iform7, 0, 0, 14, 8, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)


! Create panel Panel1 child of iform7MainPanel
   Panel7 = gmCreatePanel(iform7MainPanel, 0, 0, 14, 8, &
              	gmTitle='                  CONSAM  FOR  WINDOWS  V1.0', gmType=GPROJECTED, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=12, gmFillBorder=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static1 child of Panel7
   Static7_1 = gmCreateTextEntry(Panel7, 1, 1, 12, 1,'General-purpose Continuous Sampling to Disk ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static7-2 child of Panel7
   Static7_2 = gmCreateTextEntry(Panel7, 1, 4, 12, 1,'David Colquhoun and Ioana Vais ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static3 child of Panel7
   Static7_3 = gmCreateTextEntry(Panel7, 1, 5, 12, 1,'University College London ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=12, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static4 child of Panel7
   Static7_4 = gmCreateTextEntry(Panel7, 1, 6, 12, 1,'http://www.ucl.ac.uk/Pharmacology ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create icon Icon1 child of Panel1
   Icon7 = gmCreateIcon(Panel7, 6, 2, 2, 2,'MBIG1035', GDLLICON,0,gmVpos=GTOP)

end