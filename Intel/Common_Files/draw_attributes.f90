subroutine draw_attributes()

use menu_f90

integer :: Main
integer :: Drawing
integer :: DrawingMainPanel
integer :: Panel4_1
integer :: Panel4_2
integer :: Panel4_3
integer :: Panel4_4
integer :: Icon4_1
integer :: Icon4_2
integer :: Icon4_3
integer :: Icon4_4
integer :: Toggle4(20)
integer :: Graphics4_1
integer :: Graphics4_2

type (GLIMIT) :: Graphics_frame



! Set up complex dialogue box Drawing child of Main
   Drawing = gmCreateComplexDialogueBox(Main, 11, 5, 15, 15, GALL, 'Drawing Atributes', &
              	 gmIconFormat=GDLLICON,gmIconFile='MBIG1070',gmvpos=gmiddle)

! Create main panel for form
   DrawingMainPanel=gmCreatePanel(Drawing, 0, 0, 15, 15, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)


! Create panel Panel15 child of DrawingMainPanel
   Panel4_2 = gmCreatePanel(DrawingMainPanel, 0, 0, 15, 1, &
              	gmTitle='Panel 15', gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create icon Icon29 child of Panel15
   Icon4_1 = gmCreateIcon(Panel4_2, 3, 0, 1, 1,'MSML1036', 4, 1, &
              	gmType=GBUTTON, gmVpos=GTOP, gmExpand=GOFF)

! Create icon Icon30 child of Panel15
   Icon4_2 = gmCreateIcon(Panel4_2, 1, 0, 1, 1,'MSML1035', 4, 1, &
              	gmType=GBUTTON, gmVpos=GTOP, gmExpand=GOFF)

! Create icon Icon31 child of Panel15
   Icon4_3 = gmCreateIcon(Panel4_2, 2, 0, 1, 1,'MSML1004', 4, 1, &
              	gmType=GBUTTON, gmVpos=GTOP, gmExpand=GOFF)

! Create icon Icon32 child of Panel15
   Icon4_4 = gmCreateIcon(Panel4_2, 0, 0, 1, 1,'MSML1022', 4, 1, &
              	gmType=GBUTTON, gmVpos=GTOP, gmExpand=GOFF)

! Create panel Panel6 child of DrawingMainPanel
   Panel4_1 = gmCreatePanel(DrawingMainPanel, 0, 1, 15, 14, &
              	gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=15, gmFillBorder=0, &
              	gmVpos=GTOP, gmExpand=GOFF)



! Create panel panel4_3 child of Panel6
   Panel4_3 = gmCreatePanel(Panel4_1, 1, 1, 6, 12, &
              	gmTitle='Type', gmType=GCHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=14, gmFillBorder=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle4( button toggle4(1 child of panel4_3
   toggle4(1) = gmCreatetoggleButton(panel4_3, 1, 1, 1, 1, '', 0, &
              	gmType=G3DCHECKBOX, gmSize=80, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle4( button toggle4(2 child of panel4_3
   toggle4(2) = gmCreatetoggleButton(panel4_3, 1, 2, 1, 1, '', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle4( button toggle4(3 child of panel4_3
   toggle4(3) = gmCreatetoggleButton(panel4_3, 1, 3, 1, 1, '', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle4( button toggle4(4 child of panel4_3
   toggle4(4) = gmCreatetoggleButton(panel4_3, 1, 4, 1, 1, '', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle4( button toggle4(5 child of panel4_3
   toggle4(5) = gmCreatetoggleButton(panel4_3, 1, 5, 1, 1, '', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle4( button toggle4(6 child of panel4_3
   toggle4(6) = gmCreatetoggleButton(panel4_3, 1, 6, 1, 1, '', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle4( button toggle4(7 child of panel4_3
   toggle4(7) = gmCreatetoggleButton(panel4_3, 1, 7, 1, 1, '', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle4( button toggle4(8 child of panel4_3
   toggle4(8) = gmCreatetoggleButton(panel4_3, 1, 8, 1, 1, '', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle4( button toggle4(9 child of panel4_3
   toggle4(9) = gmCreatetoggleButton(panel4_3, 1, 9, 1, 1, '', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle4( button toggle4(10 child of panel4_3
   toggle4(10) = gmCreatetoggleButton(panel4_3, 1, 10, 1, 1, '', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create graphics frame Graphics1 child of panel4_3

! Define Graphics Frame limits
   Graphics_frame%xmin = 0.000000
   Graphics_frame%xmax = 40.000000
   Graphics_frame%ymin = 0.000000
   Graphics_frame%ymax = 10.000000
   Graphics4_1 = gmCreateGraphicsFrame(panel4_3, 2, 1, 4, 10, Graphics_frame, &
              	gmVpos=GTOP, gmExpand=GOFF)
   call gmClearGraphicsFrame(Graphics4_1)

 
   call gSetBrokenLine(0)
   call gMoveTo2D(0.0,0.5)
   call gDrawLineTo2D(40.0,0.5)
   call gSetBrokenLine(1)
   call gMoveTo2D(0.0,1.5)
   call gDrawLineTo2D(40.0,1.5)
   call gSetBrokenLine(2)
   call gMoveTo2D(0.0,2.5)
   call gDrawLineTo2D(40.0,2.5)
   call gSetBrokenLine(3)
   call gMoveTo2D(0.0,3.5)
   call gDrawLineTo2D(40.0,3.5)
   call gSetBrokenLine(4)
   call gMoveTo2D(0.0,4.5)
   call gDrawLineTo2D(40.0,4.5)
   call gSetBrokenLine(5)
   call gMoveTo2D(0.0,5.5)
   call gDrawLineTo2D(40.0,5.5)
   call gSetBrokenLine(6)
   call gMoveTo2D(0.0,6.5)
   call gDrawLineTo2D(40.0,6.5)
   call gSetBrokenLine(7)
   call gMoveTo2D(0.0,7.5)
   call gDrawLineTo2D(40.0,7.5)
   call gSetBrokenLine(8)
   call gMoveTo2D(0.0,8.5)
   call gDrawLineTo2D(40.0,8.5)
   call gSetBrokenLine(9)
   call gMoveTo2D(0.0,9.5)
   call gDrawLineTo2D(40.0,9.5)
! Create panel panel4_4 child of Panel6
   Panel4_4 = gmCreatePanel(Panel4_1, 8, 1, 6, 12, &
              	gmTitle='Width', gmType=GCHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=14, gmFillBorder=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle4( button toggle4(11 child of panel4_4
   toggle4(11) = gmCreatetoggleButton(panel4_4, 1, 1, 1, 1, '', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle4( button toggle4(12 child of panel4_4
   toggle4(12) = gmCreatetoggleButton(panel4_4, 1, 2, 1, 1, '', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle4( button toggle4(13 child of panel4_4
   toggle4(13) = gmCreatetoggleButton(panel4_4, 1, 3, 1, 1, '', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle4( button toggle4(14 child of panel4_4
   toggle4(14) = gmCreatetoggleButton(panel4_4, 1, 4, 1, 1, '', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle4( button toggle4(15 child of panel4_4
   toggle4(15) = gmCreatetoggleButton(panel4_4, 1, 5, 1, 1, '', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle4( button toggle4(16 child of panel4_4
   toggle4(16) = gmCreatetoggleButton(panel4_4, 1, 6, 1, 1, '', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle4( button toggle4(17 child of panel4_4
   toggle4(17) = gmCreatetoggleButton(panel4_4, 1, 7, 1, 1, '', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle4( button toggle4(18 child of panel4_4
   toggle4(18) = gmCreatetoggleButton(panel4_4, 1, 8, 1, 1, '', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle4( button toggle4(19 child of panel4_4
   toggle4(19) = gmCreatetoggleButton(panel4_4, 1, 9, 1, 1, '', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle4( button toggle4(20 child of panel4_4
   toggle4(20) = gmCreatetoggleButton(panel4_4, 1, 10, 1, 1, '', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create graphics frame Graphics2 child of panel4_4

! Define Graphics Frame limits
   Graphics_frame%xmin = 0.000000
   Graphics_frame%xmax = 10.000000
   Graphics_frame%ymin = 0.000000
   Graphics_frame%ymax = 10.000000
   Graphics4_2 = gmCreateGraphicsFrame(Panel4_4, 2, 1, 4, 10, Graphics_frame, &
              	gmVpos=GTOP, gmExpand=GOFF)
   call gmClearGraphicsFrame(Graphics4_2)

call gSetBrokenLine(0)
   call gSetLineWidth(0.)
   call gMoveTo2D(0.0,0.5)
   call gDrawLineTo2D(10.0,0.5)
   call gSetLineWidth(0.05)
   call gMoveTo2D(0.0,1.5)
   call gDrawLineTo2D(10.0,1.5)
   call gSetLineWidth(0.1)
   call gMoveTo2D(0.0,2.5)
   call gDrawLineTo2D(10.0,2.5)
   call gSetLineWidth(0.15)
   call gMoveTo2D(0.0,3.5)
   call gDrawLineTo2D(10.0,3.5)
   call gSetLineWidth(0.2)
   call gMoveTo2D(0.0,4.5)
   call gDrawLineTo2D(10.0,4.5)
   call gSetLineWidth(0.25)
   call gMoveTo2D(0.0,5.5)
   call gDrawLineTo2D(10.0,5.5)
   call gSetLineWidth(0.3)
   call gMoveTo2D(0.0,6.5)
   call gDrawLineTo2D(10.0,6.5)
   call gSetLineWidth(0.35)
   call gMoveTo2D(0.0,7.5)
   call gDrawLineTo2D(10.0,7.5)
   call gSetLineWidth(0.4)
   call gMoveTo2D(0.0,8.5)
   call gDrawLineTo2D(10.0,8.5)
   call gSetLineWidth(0.45)
   call gMoveTo2D(0.0,9.5)
   call gDrawLineTo2D(10.0,9.5)
call gmdrawwindow(drawing)
end