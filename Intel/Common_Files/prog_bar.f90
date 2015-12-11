


subroutine prog_bar(main,ipbar,icho,ipbar_Progress2)

use menu_f90

! Widget identifier definitions
integer :: ipbar
integer :: ipbar_MainPanel
integer :: ipbar_Panel1
integer :: ipbar_Progress2
integer :: ipbar_Text1
integer :: ipbar_Text2
integer :: ipbar_Text3
integer :: ipbar_Text4
integer :: ipbar_Text5
integer :: ipbar_Text6
integer :: ipbar_Text7
integer :: ipbar_Text8
integer :: ipbar_Text9
integer :: ipbar_Text10

type (GLIMIT) :: progress_frame

	progress_frame%xmin = 0.
    progress_frame%xmax = 960.
    progress_frame%ymin = 0.
    progress_frame%ymax = 20.
! Set up master window ipbar
	call gmSetGuiGridMode(GOFF)  
   ipbar = gmCreateComplexDialogueBox(Main,10,0, 980, 51, GALL,'Progress Bar', &
              	 gmIconFormat=GDLLICON,gmIconFile='MBIG1021',gmvpos=gtop)
 
! Create panel ipbar_Panel1 child of ipbar_MainPanel
   ipbar_Panel1 = gmCreatePanel(ipbar, 0, 0, 980, 51, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
   gmFillCol=71, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=Gtop)

! Create progress bar ipbar_Progress2 child of ipbar_Panel1
   !ipbar_Progress2 = gmCreateProgressBar(ipbar_Panel1, 10, 30, 800, 20, gmMinimum=0, gmMaximum=100, gmIntValue=0, &
    !  gmProgressType=Gsmooth, gmVpos=GTOP)
	 ipbar_progress2=gmCreateGraphicsFrame(ipbar_panel1, 10, 30, 960, 20, progress_frame, &
              	gmVpos=GTOP)
			
        
! Create text widget ipbar_Text1 child of ipbar_Panel1
   ipbar_Text1 = gmCreateTextEntry(ipbar_Panel1, 10, 1, 80, 25,'Mechanism ', 255, GDISPLAY, gmBack1Col=71, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ipbar_Text2 child of ipbar_Panel1
   ipbar_Text2 = gmCreateTextEntry(ipbar_Panel1, 90, 1, 80, 25,'States ', 255, GDISPLAY, gmBack1Col=71, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ipbar_Text3 child of ipbar_Panel1
   ipbar_Text3 = gmCreateTextEntry(ipbar_Panel1, 170, 1, 80, 25,'Rates ', 255, GDISPLAY, gmBack1Col=71, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ipbar_Text4 child of ipbar_Panel1
   ipbar_Text4 = gmCreateTextEntry(ipbar_Panel1, 250, 1, 80, 25,'MR ', 255, GDISPLAY, gmBack1Col=71, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
! Create text widget ipbar_Text8 child of ipbar_Panel1
   ipbar_Text8 = gmCreateTextEntry(ipbar_Panel1, 330, 1, 80, 25,'Constraints ', 255, GDISPLAY, gmBack1Col=71, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ipbar_Text9 child of ipbar_Panel1
   ipbar_Text9 = gmCreateTextEntry(ipbar_Panel1, 410, 1, 80, 25,'EC50 ', 255, GDISPLAY, gmBack1Col=71, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

if(icho.eq.3) then
ipbar_Text5 = gmCreateTextEntry(ipbar_Panel1, 490, 1, 80, 25,'Exp/Model ', 255, GDISPLAY, gmBack1Col=71, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

else
! Create text widget ipbar_Text5 child of ipbar_Panel1
   ipbar_Text5 = gmCreateTextEntry(ipbar_Panel1, 490, 1, 80, 25,'Exp/Model ', 255, GDISPLAY, gmBack1Col=71, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
endif	 
! Create text widget ipbar_Text6 child of ipbar_Panel1
   ipbar_Text6 = gmCreateTextEntry(ipbar_Panel1, 570, 1, 80, 25,'Resolution ', 255, GDISPLAY, gmBack1Col=71, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ipbar_Text7 child of ipbar_Panel1
   ipbar_Text7 = gmCreateTextEntry(ipbar_Panel1, 650, 1, 80, 25,'Groups', 255, GDISPLAY, gmBack1Col=71, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)



! Create text widget ipbar_Text10 child of ipbar_Panel1
   ipbar_Text10 = gmCreateTextEntry(ipbar_Panel1, 730, 1, 80, 25,'Fitting ', 255, GDISPLAY, gmBack1Col=71, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
	  	call gmSetGuiGridMode(GOn)
call gmdrawwindow(ipbar)
return
end
