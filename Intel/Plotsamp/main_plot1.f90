subroutine main_plot(iform,ipanel0,ipanel,ipanel2,iedit,tedit,ival,tval,itoggle0,itoggle1,itoggle2,&
itoggle3,ibutton100,ibutton2,icombo2,ibutton)
use menu_f90 
 
 character*20 itextst(10),itextsv(20)
integer*4 iedit(10),ival(20)
character*60 tedit(10)

real tval(20),tval1(10),tval2(10)
 call gDefineRGB(227, 1.000000, 0.458824, 0.458824)
 call gDefineRGB(228, 1.000000, 1.000000, 0.517647)
  call gDefineRGB(225, 1.000000, 0.866667, 0.733333)
   call gDefineRGB(226, 0.996078, 0.780392, 0.705882)
    call gDefineRGB(220, 1.000000, 1.000000, 0.807843)
   call gDefineRGB(221, 0.796078, 1.000000, 0.592157)
   call gDefineRGB(222, 0.682353, 0.843137, 1.000000)
   call gDefineRGB(223, 1.000000, 0.909804, 0.909804)
  call gDefineRGB(141, 0.192157, 0.984314, 0.964706)
  call gDefineRGB(229, 0.836078, 0.950000, 0.592157)
  itextst(1)='File name:'

itextst(2)='File Date:'
itextst(3)='File Time:'
itextst(4)='Experiment title:'
itextst(5)='Experiment date:'
itextst(6)='Tape details:'
itextst(7)='Patch type:'

itextsv(1)='File length (bytes):'
itextsv(2)='Points in sample:'
itextsv(3)='No of ADC channels:'

itextsv(4)='Sample rate (Hz):'
itextsv(5)='Mem potential (mV):'
itextsv(6)='Temperature (o C):'

itextsv(8)='Gain(pA/V):'
itextsv(9)='Calibration(V/pA):'
itextsv(10)='Filter(Hz):'

itextsv(12)='Calibration(V/pA):'
itextsv(13)='Filter(Hz):'
itextsv(11)='Gain(pA/V):'


iForm = gmCreateMasterWindow(4, 2, 40, 31, GALL, 'PLOT / FILTER SAMPLE (Copyright 2005 D. Colquhoun & I. Vais) ', &
              	gmVpos=GTOP, gmIconFormat=GDLLICON,gmIconFile='MBIG1005',gmSelect=100)

iPanel00=gmCreatePanel(iform, 0, 0, 40, 31, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmExpand=GON, gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=221, gmFillBorder=0) !227

iPanel0=gmCreatePanel(ipanel00, 0, 0, 38, 31, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmExpand=GON, gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=221, gmFillBorder=0) !227
iStatic = gmCreateTextEntry(iPanel0, 1, 1, 3, 1,itextst(1), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1,&
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
	iEdit(1) = gmCreateTextEntry(iPanel0, 4, 1, 11, 1,tedit(1), 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmscrollable=gon,&
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=10)
	iButton = gmCreatePushButton(iPanel0, 1, 3, 3, 1, 'Browse', &
              	gmType=Guserdefined, gmAccel=0, &
              	gmOncol=0, gmOffCol=27, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=9)
				call gmsetwidgetstatus(ibutton,GUNSELECTABLE)
	iStatic = gmCreateTextEntry(iPanel0, 5, 3, 5, 1,itextsv(1), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   ival(1) = gmCreateValueEntry(iPanel0, 10, 3, 5, 1,tval(1), 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
   
              	gmVpos=GTOP, gmExpand=GOFF)
   iPanel=gmCreatePanel(ipanel0, 0, 5, 16, 26, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmExpand=GON, gmType=GINVERSECHISEL, &
              	gmtitle='File Details:',gmLineCol=0, gmLineBorder=0, gmFillCol=27,&
				 gmFillBorder=0)

	iStatic = gmCreateTextEntry(iPanel, 1, 2, 4, 1,'File type:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   iEdit(10) = gmCreateTextEntry(iPanel, 6, 2, 9, 1,tedit(10), 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

   iStatic = gmCreateTextEntry(iPanel, 1, 3, 4, 1,itextst(2), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   iEdit(2) = gmCreateTextEntry(iPanel, 6, 3, 9, 1,tedit(2), 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
   iStatic = gmCreateTextEntry(iPanel, 1, 4, 3, 1,itextst(3), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   iEdit(3) = gmCreateTextEntry(iPanel, 6, 4, 9, 1,tedit(3), 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
	 iStatic = gmCreateTextEntry(iPanel, 1, 5, 5, 1,itextst(4), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   iEdit(4) = gmCreateTextEntry(iPanel, 6, 5, 9, 1,tedit(4), 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
   iStatic = gmCreateTextEntry(iPanel, 1, 6, 4, 1,itextst(6), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   iEdit(6) = gmCreateTextEntry(iPanel, 6, 6, 9, 1,tedit(6), 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
    iStatic = gmCreateTextEntry(iPanel, 1, 7, 5, 1,itextst(5), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   iEdit(5) = gmCreateTextEntry(iPanel, 6, 7, 9, 1,tedit(5), 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
   iStatic = gmCreateTextEntry(iPanel, 1, 8, 3, 1,itextst(7), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
     
  icombo2 = gmCreateComboBox(ipanel, 6, 8, 9, 10, GNONE, 1, &
              	gmSort=GUNSORTED, gmHpos=Gleft,  gmVpos=GTOP, gmExpand=GOFF, gmcallback=19)

! Create entries for combo box Combo1_5
   	call gmSetListEntry(iCombo2, GADD, gmString='Outside-out')
	call gmSetListEntry(iCombo2, GADD, gmString='Inside-out')
	call gmSetListEntry(iCombo2, GADD, gmString='Cell attached')
	call gmSetListEntry(iCombo2, GADD, gmString='Whole cell')
	call gmSetListEntry(iCombo2, GADD, gmString='Simulated data')
   
   
   do i=2,6
   iStatic = gmCreateTextEntry(iPanel, 1, i+7, 5, 1,itextsv(i), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   ival(i) = gmCreateValueEntry(iPanel, 6, i+7, 9, 1,tval(i), 20,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

   enddo
 
   
  iPanel01=gmCreatePanel(ipanel, 2, 15, 12, 5, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmExpand=GON, gmType=GINVERSECHISEL, &
              	gmtitle='ADC0',gmLineCol=0, gmLineBorder=0, gmFillCol=141, gmFillBorder=0)
 
 do i=8,10
   iStatic = gmCreateTextEntry(iPanel01, 1, i-7, 5, 1,itextsv(i), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   
   ival(i) = gmCreateValueEntry(iPanel01, 6, i-7, 4, 1,tval(i), 20,9, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
   
   enddo
    iPanel02=gmCreatePanel(ipanel, 2, 20, 12, 5, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmExpand=GON, gmType=GINVERSECHISEL, &
              	gmtitle='ADC1',gmLineCol=0, gmLineBorder=0, gmFillCol=141, gmFillBorder=0)
 
 do i=11,13
   iStatic = gmCreateTextEntry(iPanel02, 1, i-10, 5, 1,itextsv(i), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   
   ival(i) = gmCreateValueEntry(iPanel02, 6, i-10, 4, 1,tval(i), 20,9, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
   
   enddo
   
iPanel2=gmCreatePanel(ipanel0, 16, 0, 22, 31, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmExpand=GON, gmType=GINVERSECHISEL, &
              	gmtitle='Processing details',gmLineCol=0, gmLineBorder=0, gmFillCol=221, gmFillBorder=0)

iRadio1 = gmCreateRadioBox(iPanel2, 1, 1, 20,6 , gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='', gmFillCol=229,gmVpos=GTOP)
iToggle0 = gmCreateToggleButton(iRadio1, 1, 1, 18, 1, &
  'Display sample', 1, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP,gmCallback=20)


  iToggle1 = gmCreateToggleButton(iRadio1, 1, 2, 18, 1, &
  'Filter with Gaussian filter, and omit points if necessary', 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP,gmCallback=21)

  
! Create toggle button ittfont_Toggle1 child of ittfont_Radio1
   iToggle2 = gmCreateToggleButton(iradio1, 1, 3, 18, 1, 'Write subsection of CONSAM file to disk', 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP,gmCallback=22)

! Create toggle button ittfont_Toggle2 child of ittfont_Radio1
   iToggle3 = gmCreateToggleButton(iRadio1, 1, 4, 18, 1, &
   'Omit points from output file, and write to disk', 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP,gmCallback=23)
! Create toggle button ittfont_Toggle7 child of ittfont_Radio1
  
	ibutton3 = gmCreateIcon(ipanel00,0,0,2,2 ,'MBIG1064',  &
			GDLLICON,0,&
            gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Help',gmcallback =8 )

		call gmsetwidgetstatus(ibutton3,GSELECTABLE)
     
	 
	 iButton2 = gmCreateIcon(ipanel00,0,2,2,2 ,'MBIG1007',  &
			GDLLICON,0,&
            gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Exit',gmCallback=6)




 
 !call gmsetwidgetstatus(ibutton2,GUNSELECTABLE)
    
end
