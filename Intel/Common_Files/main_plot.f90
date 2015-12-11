subroutine main_plot(iform,iedit,tedit,ival,ival1,ival2,tval,tval1,tval2,&
iTogg11,itogg12,iTogg13,itogg14,itogg21,itogg22,itoggle0,itoggle1,itoggle2,itoggle3,ibutton1,ibutton2,&
icombo1)
use menu_f90 
 
 character*20 itextst(10),itextsv(20)
integer*4 iedit(10),ival(20),ival1(10),ival2(10)
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




iForm = gmCreateMasterWindow(4, 2, 42, 28, GALL, 'PLOT / FILTER SAMPLE (Copyright 2005 D. Colquhoun & I. Vais) ', &
              	gmVpos=GTOP, gmIconFormat=GDLLICON,gmIconFile='MBIG1005')

  iPanel0=gmCreatePanel(iform, 0, 0, 42, 28, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmExpand=GON, gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=227, gmFillBorder=0)	
iStatic = gmCreateTextEntry(iPanel0, 1, 1, 4, 1,itextst(1), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
	iEdit(1) = gmCreateTextEntry(iPanel0, 5, 1, 16, 1,tedit(1), 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=10)
	iButton = gmCreatePushButton(iPanel0, 22, 1, 5, 1, 'Browse', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=9)
	iStatic = gmCreateTextEntry(iPanel0, 28, 1, 5, 1,itextsv(1), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   ival(1) = gmCreateValueEntry(iPanel0, 33, 1, 5, 1,tval(1), 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
   
              	gmVpos=GTOP, gmExpand=GOFF)
   iPanel=gmCreatePanel(iForm, 0, 3, 24, 22, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmExpand=GON, gmType=GINVERSECHISEL, &
              	gmtitle='File Details:',gmLineCol=0, gmLineBorder=0, gmFillCol=220, gmFillBorder=0)
   iStatic = gmCreateTextEntry(iPanel, 1, 2, 4, 1,'File type:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   iEdit(10) = gmCreateTextEntry(iPanel, 5, 2, 15, 1,tedit(10), 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

   iStatic = gmCreateTextEntry(iPanel, 1, 3, 4, 1,itextst(2), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   iEdit(2) = gmCreateTextEntry(iPanel, 5, 3, 5, 1,tedit(2), 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
   iStatic = gmCreateTextEntry(iPanel, 12, 3, 3, 1,itextst(3), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   iEdit(3) = gmCreateTextEntry(iPanel, 15, 3, 5, 1,tedit(3), 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
	 iStatic = gmCreateTextEntry(iPanel, 1, 4, 5, 1,itextst(4), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   iEdit(4) = gmCreateTextEntry(iPanel, 6, 4, 14, 1,tedit(4), 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
   iStatic = gmCreateTextEntry(iPanel, 1, 5, 4, 1,itextst(6), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   iEdit(6) = gmCreateTextEntry(iPanel, 5, 5, 15, 1,tedit(6), 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
    iStatic = gmCreateTextEntry(iPanel, 1, 6, 5, 1,itextst(5), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   iEdit(5) = gmCreateTextEntry(iPanel, 6, 6, 5, 1,tedit(5), 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
   iStatic = gmCreateTextEntry(iPanel, 12, 6, 3, 1,itextst(7), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
     
  icombo1 = gmCreateComboBox(ipanel, 15, 6, 5, 10, GNONE, 1, &
              	gmSort=GUNSORTED, gmHpos=Gleft,  gmVpos=GTOP, gmExpand=GOFF, gmcallback=80)

! Create entries for combo box Combo1_5
   	call gmSetListEntry(iCombo1, GADD, gmString='Outside-out')
	call gmSetListEntry(iCombo1, GADD, gmString='Inside-out')
	call gmSetListEntry(iCombo1, GADD, gmString='Cell attached')
	call gmSetListEntry(iCombo1, GADD, gmString='Whole cell')
	call gmSetListEntry(iCombo1, GADD, gmString='Simulated data')
   
   
   do i=2,4
   iStatic = gmCreateTextEntry(iPanel, 1, i+6, 5, 1,itextsv(i), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   ival(i) = gmCreateValueEntry(iPanel, 6, i+6, 4, 1,tval(i), 20,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

   enddo
 do i=5,6
   iStatic = gmCreateTextEntry(iPanel, 11, i+3, 5, 1,itextsv(i), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   ival(i) = gmCreateValueEntry(iPanel, 16, i+3, 4, 1,tval(i), 20,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

   enddo
   
  iPanel01=gmCreatePanel(ipanel, 1, 12, 11, 6, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmExpand=GON, gmType=GINVERSECHISEL, &
              	gmtitle='ADC0',gmLineCol=0, gmLineBorder=0, gmFillCol=38, gmFillBorder=0)
 
 do i=8,10
   iStatic = gmCreateTextEntry(iPanel01, 1, i-6, 5, 1,itextsv(i), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   
   ival(i) = gmCreateValueEntry(iPanel01, 6, i-6, 4, 1,tval(i), 20,9, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
   
   enddo
    iPanel02=gmCreatePanel(ipanel, 12, 12, 11, 6, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmExpand=GON, gmType=GINVERSECHISEL, &
              	gmtitle='ADC1',gmLineCol=0, gmLineBorder=0, gmFillCol=38, gmFillBorder=0)
 
 do i=11,13
   iStatic = gmCreateTextEntry(iPanel02, 1, i-9, 5, 1,itextsv(i), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   
   ival(i) = gmCreateValueEntry(iPanel02, 6, i-9, 4, 1,tval(i), 20,9, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
   
   enddo
   



iPanel2=gmCreatePanel(ipanel0, 24, 3, 18, 22, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmExpand=GON, gmType=GINVERSECHISEL, &
              	gmtitle='Processing details',gmLineCol=0, gmLineBorder=0, gmFillCol=221, gmFillBorder=0)

iStatic = gmCreateTextEntry(iPanel2, 1, 2, 4, 1,'Use from point:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ival1(1) = gmCreateValueEntry(iPanel2, 5, 2, 4, 1,tval1(1), 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(iPanel2, 10, 2, 3, 1,'To point:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ival1(2) = gmCreateValueEntry(iPanel2, 13, 2, 4, 1,tval1(2), 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

iRadio1 = gmCreateRadioBox(iPanel2, 1, 4, 16,6 , gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='', gmFillCol=220,gmVpos=GTOP)
iToggle0 = gmCreateToggleButton(iRadio1, 1, 1, 15, 1, &
  'Display sample', 1, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)


  iToggle1 = gmCreateToggleButton(iRadio1, 1, 2, 15, 1, &
  'Filter with Gaussian filter, amd omit points if nec', 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)

  
! Create toggle button ittfont_Toggle1 child of ittfont_Radio1
   iToggle2 = gmCreateToggleButton(iradio1, 1, 3, 15, 1, 'Write subsection of CONSAM file to disk', 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)

! Create toggle button ittfont_Toggle2 child of ittfont_Radio1
   iToggle3 = gmCreateToggleButton(iRadio1, 1, 4, 15, 1, &
   'Omit points from output file, and write to disk', 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)
! Create toggle button ittfont_Toggle7 child of ittfont_Radio1
  


iStatic = gmCreateTextEntry(iPanel2, 1, 11, 4, 1,'Invert the data :', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
iTogg21 = gmCreateToggleButton(ipanel2, 5, 11, 2, 1, 'Yes', 0, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)

iTogg22 = gmCreateToggleButton(ipanel2, 7, 11, 2, 1, 'No', 1, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)

iStatic = gmCreateTextEntry(iPanel2, 1, 12, 6, 1,'Amplify data by: gain', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ival2(3) = gmCreateValueEntry(iPanel2, 7, 12, 2, 1,tval2(3), 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
iStatic = gmCreateTextEntry(iPanel2, 1, 13, 4, 1,'Points in buffer:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ival2(4) = gmCreateValueEntry(iPanel2, 5, 13, 4, 1,tval2(4), 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(iPanel2, 1, 14, 5, 1,'Points in overlap:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ival2(5) = gmCreateValueEntry(iPanel2, 6, 14, 3, 1,tval2(5), 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)	

iStatic = gmCreateTextEntry(iPanel2, 1, 16, 10, 1,'Interleave traces with plot of P(open) :', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
iTogg11 = gmCreateToggleButton(ipanel2, 11, 16, 3, 1, 'Yes', 0, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)

iTogg12 = gmCreateToggleButton(ipanel2, 14, 16, 3, 1, 'No', 0, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)

iStatic = gmCreateTextEntry(iPanel2, 1, 17, 5, 1,'Seconds per line', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ival1(3) = gmCreateValueEntry(iPanel2, 6, 17, 3, 1,tval1(3), 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
				              
iStatic = gmCreateTextEntry(iPanel2, 1, 18, 5, 1,' Traces per page ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ival1(4) = gmCreateValueEntry(iPanel2, 6, 18, 3, 1,tval1(4), 32768,0, GEDIT, &

              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)



iStatic = gmCreateTextEntry(iPanel2, 1, 19, 16, 1,' Vertical separation of traces (pA) ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ival1(5) = gmCreateValueEntry(iPanel2, 11, 19, 4, 1,tval1(5), 20,10, GEDIT, &

              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
				! Create main panel for form
iStatic = gmCreateTextEntry(iPanel2, 1, 20, 8, 1,'Draw frame round traces :', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
iTogg13 = gmCreateToggleButton(ipanel2, 9, 20, 3, 1, 'Yes', 0, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)

iTogg14 = gmCreateToggleButton(ipanel2, 12, 20, 3, 1, 'No', 1, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)

 
     iButton1 = gmCreatePushButton(iPanel0, 12, 26, 9, 1, 'Continue', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=227, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=31)
	 call gmsetwidgetstatus(ibutton1,GUNSELECTABLE)
	 
	 iButton2 = gmCreatePushButton(ipanel0, 21, 26, 9, 1, 'Quit', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=227, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=52)
 call gmsetwidgetstatus(ibutton2,GUNSELECTABLE)
    
	 
end