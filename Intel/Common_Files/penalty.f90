!     Last change:  D    15 Jan 104    4:06 pm
subroutine penaly(main,ipeny,ipeny_yes,ipeny_no,ipeny_xs,xs,ipeny_xs1,xs1,ipeny_fac,fac)

use menu_f90


! Widget identifier definitions
 call gmSetGuiGridMode(GOff)
 ilg=160
if(fac.eq.-1.) then
ilg=100
endif
! Set up master window ipeny
   ipeny = gmCreateComplexDialogueBox(Main,100,100,400 , ilg, GALL, '', &
              	 gmIconFormat=GDLLICON,gmIconFile='MBIG1034',gmvpos=gtop)



! Create panel ipeny_Panel1 child of ipeny_MainPanel
   ipeny_Panel1 = gmCreatePanel(ipeny, 0, 0, 400, ilg, gmTitle=' ', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=30, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)
 

 
 ipeny_Text8 = gmCreateTextEntry(ipeny_Panel1, 20, 10, 250, 25,&
 'Max value for association rate constant',&
  255, GDISPLAY, gmBack1Col=0, &
   gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
  ipeny_xs = gmCreateValueEntry(ipeny_Panel1, 280, 10, 100, 25, xs, 15, 1, GEDIT,  &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
ipeny_Text9 = gmCreateTextEntry(ipeny_Panel1, 20, 40, 250, 25,'Max value for any other fitted rate constant', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
 ipeny_xs1 = gmCreateValueEntry(ipeny_Panel1, 280, 40, 100, 25, xs1, 15, 1, GEDIT,  &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
 if(fac.eq.-1.) goto 99
! Create text widget ipeny_Text7 child of ipeny_Panel1
   ipeny_Text7 = gmCreateTextEntry(ipeny_Panel1, 20, 70, 250, 25,'Factor for penalty function ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create value entry ipeny_Value6 child of ipeny_Panel1
   ipeny_fac = gmCreateValueEntry(ipeny_Panel1, 280, 70, 100, 25, fac, 15, 1, GEDIT, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
   ipeny_Text0 = gmCreateTextEntry(ipeny_Panel1, 20, 100, 100, 25,'DC Mechanism?', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create toggle button ipeny_Toggle2 child of ipeny_Panel1
   ipeny_yes = gmCreateToggleButton(ipeny_Panel1, 130, 100, 70, 25, 'Yes', 1, gmType=G3DCHECKBOX, gmVpos=GTOP)
	
! Create toggle button ipeny_Toggle3 child of ipeny_Panel1
   ipeny_no = gmCreateToggleButton(ipeny_Panel1, 210, 100, 70, 25, 'No', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)


	  ! Create button iec50_Button1 child of iec50_MainPanel
99	continue
	icl=4005
	if(fac.eq.-1.) icl=-175
   ipeny_Button1 = gmCreatePushButton(ipeny_Panel1, 15, ilg-30, 180, 25, 'Cancel', gmVpos=GTOP,&
   gmcallback=icl) !710)
 ipeny_Button1 = gmCreatePushButton(ipeny_Panel1, 205, ilg-30, 180, 25, 'OK', gmVpos=GTOP,&
   gmcallback=icl)
  call gmSetGuiGridMode(GOn)
 call gmdrawwindow(ipeny)
return
end