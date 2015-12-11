subroutine ttfont(main,ittfont,icall,idev,ittfont_Toggle)
! Last created on Mon Oct 25 11:54:25 2004


use menu_f90


! Widget identifier definitions
integer :: ittfont
integer :: ittfont_MainPanel
integer :: ittfont_Radio1
integer :: ittfont_Toggle(60)
integer :: ittfont_Toggle2

integer :: ittfont_Radio2

integer :: ittfont_Button1,listfont(10)
integer :: callid,program_type
character*11 cnum0
common/type/program_type,nset
call gEnqHardFontList(listfont, 10, nfontmax)
iy=350
if(idev.ge.3) iy=iy+50
if(program_type.eq.1.and.idev.eq.0) then
iy=iy+100
!iy=iy+200  !!!not possible because if more records????
endif
call gDefineRGB(140, 0.011765, 0.827451, 0.803922)
 call gDefineRGB(1,0.,0.,0.50)	!blue
call gmSetGuiGridMode(GOff)
! Create main panel for form
ittfont = gmCreateComplexDialogueBox(Main, 400, 150, 400, iy, GALL, '', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
	
   ittfont_MainPanel=gmCreatePanel(ittfont, 0, 0, 400, iy, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)

! Create radio box child ittfont_Radio1 child of ittfont_MainPanel
   ittfont_Radio1 = gmCreateRadioBox(ittfont_MainPanel, 0, 0, 200, 179, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='Font type:', gmVpos=GTOP)


  ittfont_Toggle(1) = gmCreateToggleButton(ittfont_Radio1, 80, 25, 120, 25, 'Courier', 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)

  
! Create toggle button ittfont_Toggle1 child of ittfont_Radio1
   ittfont_Toggle(2) = gmCreateToggleButton(ittfont_Radio1, 80, 50, 120, 25, 'Helvetica', 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)

! Create toggle button ittfont_Toggle2 child of ittfont_Radio1
   ittfont_Toggle(3) = gmCreateToggleButton(ittfont_Radio1, 80, 75, 120, 25, 'Palatino', 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)
! Create toggle button ittfont_Toggle7 child of ittfont_Radio1
   ittfont_Toggle(4) = gmCreateToggleButton(ittfont_Radio1, 80, 100, 120, 25, 'Arial', 0,&
    gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)

! Create toggle button ittfont_Toggle8 child of ittfont_Radio1
   ittfont_Toggle(5) = gmCreateToggleButton(ittfont_Radio1, 80, 125, 120, 25, 'Times roman', 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)
 ittfont_Toggle(6) = gmCreateToggleButton(ittfont_Radio1, 80, 150, 120, 25, 'Default', 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)

   call gmsetwidgetstatus(ittfont_Toggle(6),gselectable)

if(idev.eq.1) then
	do i=1,5
		call gmsetwidgetstatus(ittfont_Toggle(i),gselectable)
		call gmsettoggleswitch(ittfont_Toggle(2),gon)
	enddo
else 
	do i=1,3
		call gmsetwidgetstatus(ittfont_Toggle(i),gselectable)
		call gmsettoggleswitch(ittfont_Toggle(2),gon)
	enddo
	do i=4,5
		call gmsetwidgetstatus(ittfont_Toggle(i),gunselectable)
	enddo
endif
  

 ittfont_Radio2 = gmCreateRadioBox(ittfont_MainPanel, 200, 0, 200, 179, gmType=GFREEFORM, &
   gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='Font Size', gmVpos=GTOP)

! Create toggle button ittfont_Toggle10 child of ittfont_Radio2
  
! Create toggle button ittfont_Toggle11 child of ittfont_Radio2
   ittfont_Toggle(11) = gmCreateToggleButton(ittfont_Radio2, 80, 25, 120, 25, '14 [3.5 mm]', 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)


! Create toggle button ittfont_Toggle13 child of ittfont_Radio2
   ittfont_Toggle(12) = gmCreateToggleButton(ittfont_Radio2, 80, 50, 120, 25, '16 [4 mm]', 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)


! Create toggle button ittfont_Toggle15 child of ittfont_Radio2
   ittfont_Toggle(13) = gmCreateToggleButton(ittfont_Radio2, 80, 75, 120, 25, '18 [4.5 mm]', 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)

! Create toggle button ittfont_Toggle16 child of ittfont_Radio2
   ittfont_Toggle(14) = gmCreateToggleButton(ittfont_Radio2, 80, 100, 120, 25, '20 [5 mm]', 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)
 ittfont_Toggle(15) = gmCreateToggleButton(ittfont_Radio2, 80, 125, 120, 25, 'Default', 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)
call gmsettoggleswitch(ittfont_Toggle(13),gon)

   ittfont_Radio3 = gmCreateRadioBox(ittfont_MainPanel, 0, 180, 200, 104, gmType=GFREEFORM, &
   gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='Line Thickness', gmVpos=GTOP)
  ittfont_Toggle(21) = gmCreateToggleButton(ittfont_Radio3, 80, 25, 120, 25, 'Thin', 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)


! Create toggle button ittfont_Toggle13 child of ittfont_Radio2
   ittfont_Toggle(22) = gmCreateToggleButton(ittfont_Radio3, 80, 50, 120, 25, 'Medium', 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)


! Create toggle button ittfont_Toggle15 child of ittfont_Radio2
   ittfont_Toggle(23) = gmCreateToggleButton(ittfont_Radio3, 80, 75, 120, 25, 'Thick', 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)
	 call gmsettoggleswitch(ittfont_Toggle(22),gon)

	   ittfont_Radio4 = gmCreateRadioBox(ittfont_MainPanel, 200, 180, 200, 104, gmType=GFREEFORM, &
   gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='Output Options ', gmVpos=GTOP)
  ittfont_Toggle(26) = gmCreateToggleButton(ittfont_Radio4, 80, 25, 120, 25, 'Colour', 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)


! Create toggle button ittfont_Toggle13 child of ittfont_Radio2
   ittfont_Toggle(27) = gmCreateToggleButton(ittfont_Radio4, 80, 50, 120, 25, 'Black and white', 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)

	 call gmsettoggleswitch(ittfont_Toggle(26),gon)
!	 if(idev.lt.3) call gmsetwidgetstatus(ittfont_Toggle(27),gunselectable)
! Create button ittfont_Button1 child of ittfont_MainPanel
 
 
 ittfont_Radio5 = gmCreateRadioBox(ittfont_MainPanel, 0, 285, 400, 35, gmType=GFREEFORM, &
   gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='Show curves legend:', gmVpos=GTOP)
  ittfont_Toggle(16) = gmCreateToggleButton(ittfont_Radio5, 210, 3, 50, 25, 'Yes', 1, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)


! Create toggle button ittfont_Toggle13 child of ittfont_Radio2
   ittfont_Toggle(17) = gmCreateToggleButton(ittfont_Radio5, 270, 3, 50, 25, 'No', 0, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)


if(idev.ge.3) then
	 itt_MainPanel=gmCreatePanel(ittfont_mainpanel, 0, 25, 400, 50, gmHpos=Gleft, gmVpos=Gbottom, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)
! Create radio box child itt_Radio3 child of itt_MainPanel
  
  call gDefineRGB(1,0.,0.,0.50)	!blue
! Create text widget initialw_Text1 child of initialw_Panel1
   initialw_Text1 = gmCreateTextEntry(itt_mainPanel, 15, 10, 380, 25,'Do not forget to set your printer orientation to LANDSCAPE', 255, GDISPLAY, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=2 ,gmScrollMode=GVERTICAL, gmVpos=Gbottom)

endif
if(program_type.eq.1.and.idev.eq.0) then
	init=gmCreatePanel(ittfont_mainpanel, 0, 320, 400, 100, gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE,gmScrollMode=GVERTICAL)
	 
	  ittfont_Radio6 = gmCreateRadioBox(init, 0, 0, 400, 100, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='Scale:', gmVpos=GTOP)


  ittfont_Toggle(31) = gmCreateToggleButton(ittfont_Radio6, 60, 15, 120, 25, 'Linear', 0, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)

  
! Create toggle button ittfont_Toggle1 child of ittfont_Radio1
   ittfont_Toggle(32) = gmCreateToggleButton(ittfont_Radio6, 60, 40, 120, 25, 'LogX/Y', 0, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)

! Create toggle button ittfont_Toggle2 child of ittfont_Radio1
   ittfont_Toggle(33) = gmCreateToggleButton(ittfont_Radio6, 60, 65, 120, 25, 'X/LogY', 0, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)
! Create toggle button ittfont_Toggle7 child of ittfont_Radio1
   ittfont_Toggle(34) = gmCreateToggleButton(ittfont_Radio6, 210, 15, 120, 25, 'LogX/LogY', 0,&
    gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)

! Create toggle button ittfont_Toggle8 child of ittfont_Radio1
   ittfont_Toggle(35) = gmCreateToggleButton(ittfont_Radio6, 210, 40, 120, 25, 'Hill Plot', 0, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)
 ittfont_Toggle(36) = gmCreateToggleButton(ittfont_Radio6, 210, 65, 120, 25, 'Default', 1, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)

! ittfont_Radio7 = gmCreateRadioBox(init, 0, 100, 400, (nset+1)*30, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
 !    gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='Sets:', gmVpos=GTOP)
 	goto 11
	ittfont_Radio7=gmCreatePanel(init,0, 100, 400,(nset+1)*30, gmTitle='Sets:', gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON,gmBorderType=GPROJECTED , &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)
j=0
 do i=1,nset,2
 if(nset.lt.21) then
  call intconv(i,cnum0)
  j=j+1
  ittfont_Toggle(40+i) = gmCreateToggleButton(ittfont_Radio7, 60, 25*j-10, 120, 25, 'Set:'//cnum0, 0, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmcallback=5400+i)
endif
 enddo
 j=0
 do i=2,nset,2
 if(nset.lt.22) then
  call intconv(i,cnum0)
  j=j+1
  ittfont_Toggle(40+i) = gmCreateToggleButton(ittfont_Radio7, 210, 25*j-10, 120, 25, 'Set:'//cnum0, 0, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmcallback=5400+i)
  endif
 enddo
 ia=int(25.*(nset+1))/2.
 ittfont_Toggle(40) = gmCreateToggleButton(ittfont_Radio7, 210, ia-10, 120, 25, 'All', 1, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmcallback=5400)
11 continue
endif
   ittfont_Button1 = gmCreatePushButton(ittfont_MainPanel, 0, 0, 400, 25, 'OK', gmVpos=Gbottom,&
   gmcallback=icall)
call gmSetGuiGridMode(GOn)
call gmdrawwindow(ittfont)
return
end
