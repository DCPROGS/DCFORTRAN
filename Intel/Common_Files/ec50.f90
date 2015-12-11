subroutine cec50(main,nfixec50,lign,iec50,iec50_yes,iec50_no, iec50_i, iec50_j,&
	iec50_ec50, iec50_x1,iec50_x2,iec50_penfac,penfac,i50a,j50a,ec50a,x1a,x2a)

use menu_f90


! Widget identifier definitions
integer :: i50a(10),j50a(10),iec50_ec50(10), iec50_x1(10),iec50_x2(10),iec50_i(10), iec50_j(10)
real ec50a(10),x1a(10),x2a(10)
integer iec50_panel2(10)
character*20 lign(10)
 call gmSetGuiGridMode(GOff)

! Set up master window iec50
   iec50 = gmCreateComplexDialogueBox(Main,100,30, 200+100*nfixec50, 310, GALL, 'EC50', &
              	 gmIconFormat=GDLLICON,gmIconFile='MBIG1034',gmvpos=gmiddle)



! Create panel iec50_Panel1 child of iec50_MainPanel
   iec50_Panel1 = gmCreatePanel(iec50, 0, 0, 200+100*nfixec50, 310, gmTitle=' ', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=30, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)
 
  iec50_Text0 = gmCreateTextEntry(iec50_Panel1, 20, 10, 100, 25,'DC Mechanism?', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create toggle button iec50_Toggle2 child of iec50_Panel1
   iec50_yes = gmCreateToggleButton(iec50_Panel1, 130, 10, 70, 25, 'Yes', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button iec50_Toggle3 child of iec50_Panel1
   iec50_no = gmCreateToggleButton(iec50_Panel1, 210, 10, 70, 25, 'No', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create text widget iec50_Text7 child of iec50_Panel1
   iec50_Text7 = gmCreateTextEntry(iec50_Panel1, 20, 40, 150, 25,'Factor for penalty function ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create value entry iec50_Value6 child of iec50_Panel1
   iec50_penfac = gmCreateValueEntry(iec50_Panel1, 180, 40, 100, 25, penfac, 8, 3, GEDIT,  &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create text widget iec50_Text1 child of iec50_Panel1
   iec50_Text01 = gmCreateTextEntry(iec50_Panel1, 20, 80, 80, 25,'Ligand ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)


! Create text widget iec50_Text1 child of iec50_Panel1
   iec50_Text1 = gmCreateTextEntry(iec50_Panel1, 20, 110, 80, 25,'EC50 ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget iec50_Text2 child of iec50_Panel1
   iec50_Text2 = gmCreateTextEntry(iec50_Panel1, 20, 140, 160, 25,'Constrain rate constant', 255, GDISPLAY, &
      gmBack1Col=0, gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
! Create text widget iec50_Text3 child of iec50_Panel1
   iec50_text3 = gmCreateTextEntry(iec50_Panel1, 20, 170, 160, 25,'from state i to state j', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
! Create text widget iec50_Text5 child of iec50_Panel1
   iec50_Text5 = gmCreateTextEntry(iec50_Panel1, 20, 200, 160, 25,'Lower limit ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget iec50_Text6 child of iec50_Panel1
   iec50_Text6 = gmCreateTextEntry(iec50_Panel1, 20, 230, 160, 25,'Upper limit ', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

do i=1,nfixec50	
	  iec50_Panel2(i) = gmCreatePanel(iec50_Panel1, 180+(i-1)*100, 70, 100, 200, gmTitle='', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=38, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

	iec50_Text = gmCreateTextEntry(iec50_Panel2(i), 20, 10, 80, 25,lign(i), 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)



! Create value entry iec50_Value1 child of iec50_Panel1
   iec50_ec50(i) = gmCreateValueEntry(iec50_Panel2(i), 10, 40, 80, 25,ec50a(i) , 8, 3, GEDIT,  &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)



! Create value entry iec50_Value2 child of iec50_Panel1
   iec50_i(i) = gmCreateValueEntry(iec50_Panel2(i), 10, 100, 35, 25, i50a(i), 8, 3, GEDIT,  &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)


! Create value entry iec50_Value3 child of iec50_Panel1
   iec50_j(i) = gmCreateValueEntry(iec50_Panel2(i), 55, 100, 35, 25, j50a(i), 8, 3, GEDIT,  &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)


! Create value entry iec50_Value4 child of iec50_Panel1
   iec50_x1(i) = gmCreateValueEntry(iec50_Panel2(i), 10, 130, 80, 25, x1a(i), 8, 3, GEDIT,  &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create value entry iec50_Value5 child of iec50_Panel1
   iec50_x2(i) = gmCreateValueEntry(iec50_Panel2(i), 10, 160, 80, 25,x2a(i) , 8, 3, GEDIT,  &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)




enddo

! Create button iec50_Button1 child of iec50_MainPanel
   iec50_Button1 = gmCreatePushButton(iec50_Panel1, 20, 275, 180+100*nfixec50, 25, 'OK', gmVpos=GTOP,&
   gmcallback=710)
 call gmSetGuiGridMode(GOn)
 call gmdrawwindow(iec50)
return
end
