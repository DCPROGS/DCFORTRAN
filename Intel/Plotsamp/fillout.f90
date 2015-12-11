subroutine fillout(iform,ifillwin,adcfil1,ieditout,teditout,ivalout,tvalout,isw,adcfil)

use menu_f90 

character*60 adcfil1,adcfil
 character*20 itextst(10),itextsv(20)
 integer*4 ieditout(10),ivalout(20)
character*60 teditout(10)
real tvalout(20)
call gDefineRGB(226, 0.996078, 0.780392, 0.705882)
 call gDefineRGB(227, 1.000000, 0.458824, 0.458824)
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

itextsv(9)='Gain(pA/V):'
itextsv(8)='Calibration(V/pA):'
itextsv(10)='Filter(Hz):'

itextsv(11)='Calibration(V/pA):'
itextsv(13)='Filter(Hz):'
itextsv(12)='Gain(pA/V):'
itextsv(14)='input Header (ioff):'
ihi=14
itextsv(20)='Fill data'
if(isw.eq.-1) then
	ihi=ihi+5
	itextsv(20)='File conversion '
endif
ifillwin = gmCreateComplexDialogueBox(iform, 15,10 , 16,ihi , GALL, '', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
	
ifillPanel=gmCreatePanel(ifillwin, 0, 0, 16, ihi, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmtitle=itextsv(20),gmExpand=GOff, gmType=GNOBOUNDARY, &
            	gmLineCol=0, gmLineBorder=0, gmFillCol=226, gmFillBorder=0)

if(isw.eq.-1) iStatic = gmCreateTextEntry(ifillpanel, 4, 1, 10, 1,'Input file:'//adcfil, 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)


iStatic = gmCreateTextEntry(ifillpanel, 1, 1, 10, 1,'Output file:'//adcfil1, 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)

iStatic = gmCreateTextEntry(ifillpanel, 1, 2, 4, 1,itextst(6), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   ieditout(6) = gmCreateTextEntry(ifillpanel, 6, 2, 9, 1,teditout(6), 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
    iStatic = gmCreateTextEntry(ifillpanel, 1, 3, 5, 1,itextst(5), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   ieditout(5) = gmCreateTextEntry(ifillpanel, 6, 3, 9, 1,teditout(5), 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
   iStatic = gmCreateTextEntry(ifillpanel, 1, 4, 3, 1,itextst(7), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
     
  icombo2 = gmCreateComboBox(ifillpanel, 6, 4, 9, 10, GNONE, 1, &
              	gmSort=GUNSORTED, gmHpos=Gleft,  gmVpos=GTOP, gmExpand=GOFF, gmcallback=19)

! Create entries for combo box Combo1_5
   	call gmSetListEntry(iCombo2, GADD, gmString='Outside-out')
	call gmSetListEntry(iCombo2, GADD, gmString='Inside-out')
	call gmSetListEntry(iCombo2, GADD, gmString='Cell attached')
	call gmSetListEntry(iCombo2, GADD, gmString='Whole cell')
	call gmSetListEntry(iCombo2, GADD, gmString='Simulated data')
    call gmSetListEntry(iCombo2,GSELECT,gmEntry=1)
do i=5,6
   iStatic = gmCreateTextEntry(ifillPanel, 1, i, 5, 1,itextsv(i), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   ivalout(i) = gmCreateValueEntry(ifillPanel, 6, i, 9, 1,tvalout(i), 20,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

 enddo
iniy=8
if(isw.eq.-1) iniy=9
 do i=iniy,10
   iStatic = gmCreateTextEntry(ifillpanel, 1, i, 5, 1,itextsv(i), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
   if(i.eq.9) then
   ivalout(i) = gmCreateValueEntry(ifillpanel, 6, i, 4, 1,tvalout(i), 20,2, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
   else
   
   ivalout(i) = gmCreateValueEntry(ifillpanel, 6, i, 4, 1,tvalout(i), 20,2, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
   endif
enddo   
if(isw.eq.-1) then  
iStatic = gmCreateTextEntry(ifillpanel, 1, 12, 4, 1,itextst(4), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ieditout(4) = gmCreateTextEntry(ifillpanel, 6, 12, 9, 1,teditout(4), 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
iStatic = gmCreateTextEntry(ifillpanel, 1, 13, 5, 1,itextsv(1), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ivalout(1) = gmCreateValueEntry(ifillpanel, 6, 13, 4, 1,tvalout(1), 20,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
iStatic = gmCreateTextEntry(ifillpanel, 1, 7, 5, 1,itextsv(4), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ivalout(4) = gmCreateValueEntry(ifillpanel, 6, 7, 4, 1,tvalout(4), 20,3, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
if(teditout(4).ne.'ASCII') then              	
iStatic = gmCreateTextEntry(ifillpanel, 1, 14, 7, 1,'Header length for input file:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ivalout(14) = gmCreateValueEntry(ifillpanel, 8, 14, 4, 1,tvalout(14), 20,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
              	call gmsetvaluesetting(ivalout(14),tvalout(14))
endif
iStatic = gmCreateTextEntry(ifillpanel, 1, 15, 3, 1,'From point:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ivalout(19) = gmCreateValueEntry(ifillpanel, 4, 15, 3, 1,tvalout(19), 20,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
iStatic = gmCreateTextEntry(ifillpanel, 8, 15, 2, 1,'to point:', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=1, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
ivalout(20) = gmCreateValueEntry(ifillpanel, 10, 15, 3, 1,tvalout(20), 20,0, GEDIT, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
endif
ibutfin2=gmCreatePushButton(ifillpanel,6,0, 5, 1,'CONTINUE',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=215) !67)
call gmdrawwindow(ifillwin)
end