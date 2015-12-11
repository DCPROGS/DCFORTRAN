subroutine text_attributes(Main,text,textmainpanel,panel3_1,icon3_1,icon3_2)

use menu_f90

integer :: Main
integer :: Text
integer :: TextMainPanel
integer :: Panel3_1
integer :: Combo3_1
integer :: Combo3_2
integer :: Combo3_3
integer :: Icon3_1
integer :: Icon3_2
integer :: Icon3_3
integer :: Icon3_4
integer :: Icon3_5
integer :: Icon3_6
integer :: Icon3_7
integer :: Button3_1
integer :: Button3_2
integer :: Button3_3
integer :: Static3_1


! Set up complex dialogue box Form2 child of Form1
   Text = gmCreateComplexDialogueBox(Main, 7, 8, 26, 1, GALL, 'Text Attributes', &
              	 gmIconFormat=GDLLICON,gmIconFile='MSML1037',gmvpos=gtop)

! Create main panel for form
   TextMainPanel=gmCreatePanel(Text, 0, 0, 26, 1, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

! Create panel panel3_1 child of Form2MainPanel
   Panel3_1 = gmCreatePanel(TextMainPanel, 0, 0, 26, 1, &
              	gmTitle='Panel 5', gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create combo box combo3_1 child of panel3_1
   Combo3_1 = gmCreateComboBox(Panel3_1, 0, 0, 6, 4, GNONE, 1, &
              	gmSort=GUNSORTED, gmVpos=GTOP, gmExpand=GOFF)

! Create entries for combo box combo3_1
   call gmSetListEntry(combo3_1, GADD, gmString='AvantGarde')
   call gmSetListEntry(combo3_1, GADD, gmString='Courier')
   call gmSetListEntry(combo3_1, GADD, gmString='Cyrillic')
   call gmSetListEntry(combo3_1, GADD, gmString='Dutch')
   call gmSetListEntry(combo3_1, GADD, gmString='Greek')
   call gmSetListEntry(combo3_1, GADD, gmString='Helvetica')
   call gmSetListEntry(combo3_1, GADD, gmString='Lublin')
   call gmSetListEntry(combo3_1, GADD, gmString='New Century')
   call gmSetListEntry(combo3_1, GADD, gmString='Palatino')
   call gmSetListEntry(combo3_1, GADD, gmString='Script')
   call gmSetListEntry(combo3_1, GADD, gmString='Souvenir')
   call gmSetListEntry(combo3_1, GADD, gmString='Symbol')
   call gmSetListEntry(combo3_1, GADD, gmString='Times New Roman')

! Select first entry in list for display
   call gmSetListEntry(combo3_1,GSELECT,gmEntry=1)

! Create combo box combo3_2 child of panel3_1
   combo3_2 = gmCreateComboBox(Panel3_1, 6, 0, 2, 4, GNONE, 1, &
              	gmSort=GUNSORTED, gmVpos=GTOP, gmExpand=GOFF)

! Create entries for combo box combo3_2
   call gmSetListEntry(combo3_2, GADD, gmString='8')
   call gmSetListEntry(combo3_2, GADD, gmString='9')
   call gmSetListEntry(combo3_2, GADD, gmString='10')
   call gmSetListEntry(combo3_2, GADD, gmString='11')
   call gmSetListEntry(combo3_2, GADD, gmString='12')
   call gmSetListEntry(combo3_2, GADD, gmString='14')
   call gmSetListEntry(combo3_2, GADD, gmString='16')
   call gmSetListEntry(combo3_2, GADD, gmString='18')
   call gmSetListEntry(combo3_2, GADD, gmString='20')
   call gmSetListEntry(combo3_2, GADD, gmString='22')
   call gmSetListEntry(combo3_2, GADD, gmString='24')
   call gmSetListEntry(combo3_2, GADD, gmString='26')

! Select first entry in list for display
   call gmSetListEntry(combo3_2,GSELECT,gmEntry=1)



! Create icon Icon24 child of panel3_1
   Icon3_1 = gmCreateIcon(panel3_1, 8, 0, 1, 1,'BOLD', 4, 1, &
              	gmType=GBUTTON, gmVpos=GTOP, gmExpand=GOFF, &
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Bold text',&
                 gmcallback = 300)

! Create icon Icon25 child of panel3_1
   Icon3_2 = gmCreateIcon(panel3_1, 9, 0, 1, 1,'ITALIC', 4, 1, &
              	gmType=GBUTTON, gmVpos=GTOP, gmExpand=GOFF, &
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Italic ',&
                 gmcallback = 300)

! Create icon Icon26 child of panel3_1
   Icon3_3 = gmCreateIcon(panel3_1, 10, 0, 1, 1,'UNDER', 4, 1, &
              	gmType=GBUTTON, gmVpos=GTOP, gmExpand=GOFF, &
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Underline ',&
                 gmcallback = 300)
! Create icon Icon19 child of panel3_1
   Icon3_4 = gmCreateIcon(panel3_1, 11, 0, 1, 1,'LEFT', 4, 1, &
              	gmType=GBUTTON, gmVpos=GTOP, gmExpand=GOFF, &
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Left Justify ',&
                 gmcallback = 300)

! Create icon Icon20 child of panel3_1
   Icon3_5 = gmCreateIcon(panel3_1, 12, 0, 1, 1,'CENTRE', 4, 1, &
              	gmType=GBUTTON, gmVpos=GTOP, gmExpand=GOFF, &
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Centre Justify ',&
                 gmcallback = 300)
! Create icon Icon27 child of panel3_1
   Icon3_6 = gmCreateIcon(panel3_1, 13, 0, 1, 1,'RIGHT', 4, 1, &
              	gmType=GBUTTON, gmVpos=GTOP, gmExpand=GOFF, &
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Right Justify ',&
                 gmcallback = 300)
! Create icon Icon27 child of panel3_1
   Icon3_7 = gmCreateIcon(panel3_1, 14, 0, 1, 1,'MSML1035', 4, 1, &
              	gmType=GBUTTON, gmVpos=GTOP, gmExpand=GOFF, &
				 gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Color Selector ',&
                 gmcallback = 300)

! Create text entry Static1 child of panel3_1
   Static3_1 = gmCreateTextEntry(panel3_1, 16, 0, 2, 1,'Rotate ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmSize=80, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create combo box combo3_3 child of panel3_1
   combo3_3 = gmCreateComboBox(panel3_1, 18, 0, 2, 4, GNONE, 1, &
              	gmSort=GUNSORTED, gmVpos=GTOP, gmExpand=GOFF)

! Create entries for combo box combo3_3
   call gmSetListEntry(combo3_3, GADD, gmString='0')
   call gmSetListEntry(combo3_3, GADD, gmString='15')
   call gmSetListEntry(combo3_3, GADD, gmString='30')
   call gmSetListEntry(combo3_3, GADD, gmString='45')
   call gmSetListEntry(combo3_3, GADD, gmString='60')
   call gmSetListEntry(combo3_3, GADD, gmString='90')
   call gmSetListEntry(combo3_3, GADD, gmString='-15')
   call gmSetListEntry(combo3_3, GADD, gmString='-30')
   call gmSetListEntry(combo3_3, GADD, gmString='-45')
   call gmSetListEntry(combo3_3, GADD, gmString='-60')
   call gmSetListEntry(combo3_3, GADD, gmString='-90')

! Select first entry in list for display
   call gmSetListEntry(combo3_3,GSELECT,gmEntry=1)

! Create button Button4 child of panel3_1
   Button3_1 = gmCreatePushButton(panel3_1, 20, 0, 2, 1, 'Delete', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create button Button5 child of panel3_1
   Button3_2 = gmCreatePushButton(panel3_1, 22, 0, 2, 1, 'Move', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create button Button6 child of panel3_1
   Button3_3 = gmCreatePushButton(panel3_1, 24, 0, 2, 1, 'New', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

call gmdrawwindow(text)
end
