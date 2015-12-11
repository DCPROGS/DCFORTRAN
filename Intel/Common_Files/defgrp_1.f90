subroutine defgrp(main,igroup,nset,vgroup1,vgroup2,vgroup3,vgroup4,vgroup5,group_editval1,group_editval2,&
			group_editval3,group_editval4,group_editval5,group_Toggle0,group_Toggle1,group_Toggle2,group_Toggle3,&
			group_Toggle4,group_Toggle5,inipage,onechan,setbad,chsvec,badend,&
			ns,ligname,igroup_text,nlig,vgroup4_1,group_editval4_1)
use menu_f90

! Widget identifier definitions
logical onechan,chsvec(10),badend(10),setbad(2,10)
character*20 ligname(10)
character*100 xtext0
integer :: igroup
integer :: group_MainPanel
integer :: group_Panel1
integer :: group_Panel2(10)
integer :: group_Panel3(10)
integer :: group_Button1
integer :: group_Button2
integer :: group_Static2
integer :: group_Static1
integer :: group_Static3
integer :: group_Static4
integer :: group_Static5
integer :: group_Static6
integer :: group_Editval1(10)
integer :: group_Editval2(10)
integer :: group_Editval3(10)
integer :: group_Editval4(10)
integer :: group_Editval4_1(10)

integer :: group_Editval5(10)
integer :: group_Toggle0(10)
integer :: group_Toggle1(10)
integer :: group_Toggle2(10)
integer :: group_Toggle3(10)
integer :: group_Toggle4(10)
integer :: group_Toggle5(10)
real vgroup1(10)
real vgroup2(10),vgroup4_1(10)
real vgroup3(10),vgroup4(10),vgroup5(10)

 call gmSetGuiGridMode(GOff)
nset0=nset
if(nlig.gt.0) then
			
				xtext0=ligname(1)
				do ij=2,nlig
				nb=nblank1(xtext0)
			
				xtext0=xtext0(1:nb)//','//ligname(ij)
				enddo
endif
j1=1
j2=nset
nset1=nset
if(inipage.eq.-100) then
	j1=ns
	j2=ns
	nset1=1
endif
if(nset1.eq.1) iwid=380
if(nset1.eq.2) iwid=530
if(nset1.eq.3) iwid=680
if(nset1.eq.4) iwid=830
iwid=380+(nset1-1)*150
ihe=330
ihe1=0
if(inipage.eq.-100) ihe1=150
if (inipage.eq.-100) ihe=450
! Set up master window group
   igroup = gmCreateComplexDialogueBox(Main,120, 100, iwid, ihe, Gall, 'Groups', gmIconFormat=GDLLICON, &
      gmIconFile='Gee',gmvpos=gtop)

! Create main panel for form
   group_MainPanel=gmCreatePanel(igroup, 0, 0, iwid, ihe, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)

! Create panel group_Panel1 child of group_MainPanel
   group_Panel1 = gmCreatePanel(group_MainPanel, 0, 0,iwid, ihe, gmTitle='DEFINE GROUPS OF OPENINGS', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=150, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)
! Create text widget group_Static5 child of group_Panel1
   group_Static1 = gmCreateTextEntry(group_Panel1, 10, 40, 200, 25,'All openings from one channel ', 255, GDISPLAY, &
      gmBack1Col=0, gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget group_Static2 child of group_Panel1
   group_Static2 = gmCreateTextEntry(group_Panel1, 10, 110, 200, 25,'Use CHS vectors ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget group_Static1 child of group_Panel1
   group_Static3 = gmCreateTextEntry(group_Panel1, 10, 80, 200, 25,'Critical shut time (ms) ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget group_Static3 child of group_Panel1
   group_Static4 = gmCreateTextEntry(group_Panel1, 10, 140, 200, 25,'Bad shutting ends group ', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget group_Static4 child of group_Panel1
   group_Static5 = gmCreateTextEntry(group_Panel1, 10, 190, 200, 25,'Set long shut times ''bad'' (ms) ', 255, GDISPLAY, &
      gmBack1Col=0, gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)


! Create text widget group_Static6 child of group_Panel1
   group_Static6 = gmCreateTextEntry(group_Panel1, 10, 220, 200, 20,'Set long open times ''bad'' (ms) ', 255, GDISPLAY, &
      gmBack1Col=0, gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
if(inipage.eq.-100) then
   group_Static8 = gmCreateTextEntry(group_Panel1, 10, 255, 200, 20,'Resolution (microsec) ', 255, GDISPLAY, &
      gmBack1Col=0, gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
! Create text widget group_Static5 child of group_Panel1
   group_Static7 = gmCreateTextEntry(group_Panel1, 10, 290, 200, 25,'Concentration 1(micromolar) ', 255, GDISPLAY, &
      gmBack1Col=0, gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget group_Static6 child of group_Panel1

 igroup_Static8 = gmCreateTextEntry(group_Panel1, 10, 327, 70, 20,'Ligand 1', 255, GDISPLAY, &
      gmBack1Col=0, gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
 

endif

do j=j1,j2

if(inipage.eq.-100) then
k=1
else
k=j
endif
! Create panel group_Panel3 child of group_Panel1
  group_Panel2(k) = gmCreatePanel(group_Panel1, 220+150*(k-1), 20, 150, 240+ihe1, gmTitle='SET: '//char(48+j), gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillCol=150, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)
	! Create panel group_Panel2 child of group_Panel3
! Create toggle button group_Toggle4 child of group_Panel2
 if(onechan) then
   group_Toggle0(j) = gmCreateToggleButton(group_Panel2(k), 10, 20, 40, 25, 'Y', 1, &
   gmType=G3DCHECKBOX, gmVpos=GTOP,gmcallback=3100+j)

   group_Toggle1(j) = gmCreateToggleButton(group_Panel2(k), 80, 20, 40, 25, 'N', 0, &

   gmType=G3DCHECKBOX, gmVpos=GTOP,gmcallback=3110+j)
 !  vgroup1(j)=3.1536e10	
   group_Panel3(j) = gmCreatePanel(group_Panel2(k), 10, 50, 130, 100, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, gmFillCol=152, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

   group_Editval1(j) = gmCreateValueEntry(group_Panel3(j), 10, 10, 110, 25, vgroup1(j), 10, 1, GEDIT, gmVmin=0.000000, &
      gmType=Gdecimal, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
		call gmSetWidgetStatus(group_editval1(j), Gunselectable)
   group_Toggle4(j) = gmCreateToggleButton(group_Panel3(j), 40, 40, 40, 25, '', 0, &
   gmType=G3DCHECKBOX, gmVpos=GTOP,gmcallback=3140+j)
	call gmSetWidgetStatus(group_toggle4(j), Gunselectable)

! Create toggle button group_Toggle19 child of group_Panel3
   group_Toggle5(j) = gmCreateToggleButton(group_Panel3(j), 40, 70, 40, 25, '', 1, &
   gmType=G3DCHECKBOX, gmVpos=GTOP,gmcallback=3150+j)
   	call gmSetWidgetStatus(group_toggle5(j), Gunselectable)

else
	group_Toggle0(j) = gmCreateToggleButton(group_Panel2(k), 10, 20, 40, 25, 'Y', 0, &
   gmType=G3DCHECKBOX, gmVpos=GTOP,gmcallback=3100+j)

   group_Toggle1(j) = gmCreateToggleButton(group_Panel2(k), 80, 20, 40, 25, 'N', 1, &
   gmType=G3DCHECKBOX, gmVpos=GTOP,gmcallback=3110+j)
   
   group_Panel3(j) = gmCreatePanel(group_Panel2(k), 10, 50, 130, 100, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, gmFillCol=152, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

   group_Editval1(j) = gmCreateValueEntry(group_Panel3(j), 10, 10, 110, 25, vgroup1(j), 10, 3, GEDIT, gmVmin=0.000000, &
   gmType=Gdecimal, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

   call gmSetWidgetStatus(group_editval1(j), Gselectable)
   if(chsvec(j)) then
  	  group_Toggle4(j) = gmCreateToggleButton(group_Panel3(j), 40, 40, 40, 25, '', 1, &
	  gmType=G3DCHECKBOX, gmVpos=GTOP,gmcallback=3140+j)
	 
	  call gmSetWidgetStatus(group_toggle4(j), Gselectable)
	   call gmSettoggleswitch(group_toggle4(j), Gon)
  else
	group_Toggle4(j) = gmCreateToggleButton(group_Panel3(j), 40, 40, 40, 25, '', 0, &
    gmType=G3DCHECKBOX, gmVpos=GTOP,gmcallback=3140+j)
	call gmSetWidgetStatus(group_toggle4(j), Gselectable)
  endif
! Create toggle button group_Toggle19 child of group_Panel3
  if(badend(j)) then
	group_Toggle5(j) = gmCreateToggleButton(group_Panel3(j), 40, 70, 40, 25, '', 1, &
    gmType=G3DCHECKBOX, gmVpos=GTOP,gmcallback=3150+j)
   	call gmSetWidgetStatus(group_toggle5(j), Gselectable)
	call gmSettoggleswitch(group_toggle5(j), Gon)
  else
   group_Toggle5(j) = gmCreateToggleButton(group_Panel3(j), 40, 70, 40, 25, '', 0, &
   gmType=G3DCHECKBOX, gmVpos=GTOP,gmcallback=3150+j)
   call gmSetWidgetStatus(group_toggle5(j), Gselectable)
  endif
endif
  
 

! Create value entry group_Editval13 child of group_Panel3
   group_Editval2(j) = gmCreateValueEntry(group_Panel2(k), 40, 170, 100, 25, vgroup2(j), 12, 0, GEDIT, gmVmin=0.000000, &
       gmType=Gdecimal, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
!	

! Create toggle button group_Toggle20 child of group_Panel3
   group_Toggle2(j) = gmCreateToggleButton(group_Panel2(k), 10, 170, 20, 25, '', 0, &
   gmType=G3DCHECKBOX, gmVpos=GTOP,gmcallback=3120+j)
!	

! Create value entry group_Editval14 child of group_Panel3
   group_Editval3(j) = gmCreateValueEntry(group_Panel2(k), 40, 200, 100, 25, vgroup3(j), 12, 0, GEDIT, gmVmin=0.000000, &
       gmType=Gdecimal, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
!	

! Create toggle button group_Toggle21 child of group_Panel3
   group_Toggle3(j) = gmCreateToggleButton(group_Panel2(k), 10, 200, 20, 25, '', 0, &
   gmType=G3DCHECKBOX, gmVpos=GTOP,gmcallback=3130+j)
!	
if(badend(j) )then
    call gmSetWidgetStatus(group_editval2(j), Gselectable)
	call gmSetWidgetStatus(group_toggle2(j), Gselectable)
	call gmSetWidgetStatus(group_editval3(j), Gselectable)
	call gmSetWidgetStatus(group_toggle3(j), Gselectable)

else
	call gmSetWidgetStatus(group_editval2(j), Gunselectable)
	call gmSetWidgetStatus(group_toggle2(j), Gunselectable)
	call gmSetWidgetStatus(group_editval3(j), Gunselectable)
	call gmSetWidgetStatus(group_toggle3(j), Gunselectable)

endif
if(inipage.eq.-100) then

 group_Editval4(j) = gmCreateValueEntry(group_Panel2(k), 40, 270, 100, 25, vgroup4(j), 12, 9, GEDIT, gmVmin=0.000000, &
       gmType=Gdecimal, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

 group_Editval5(j) = gmCreateValueEntry(group_Panel2(k), 40, 240, 100, 25, vgroup5(j), 10, 3, GEDIT, gmVmin=0.000000, &
       gmType=Gdecimal, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
igroup_text = gmCreateTextEntry(group_Panel2(k), 10, 300, 130, 25,ligname(1), 255, Gedit, &
      gmBack1Col=0, gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
if(nlig.eq.2) then
group_Editval4_1(j) = gmCreateValueEntry(group_Panel2(k), 40, 330, 100, 25, vgroup4_1(j), 12, 9, GEDIT, gmVmin=0.000000, &
       gmType=Gdecimal, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
igroup_text2 = gmCreateTextEntry(group_Panel2(k), 10, 360, 130, 25,ligname(2), 255, Gedit, &
      gmBack1Col=0, gmBack2Col=12, gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
endif

endif

enddo

! Create button group_Button1 child of group_Panel1
   group_Button1 = gmCreatePushButton(group_Panel1, 180, 0, 75, 25, 'Help', gmVpos=Gbottom,&
   gmhpos=gright,gmcallback=3162)
! Create button group_Button2 child of group_Panel1
   
   group_Button2 = gmCreatePushButton(group_Panel1, 95, 0, 75, 25, 'Cancel', &
   gmhpos=gright,gmVpos=Gbottom,gmcallback=3163)

! Create button group_Button2 child of group_Panel1
   group_Button2 = gmCreatePushButton(group_Panel1, 10, 0, 75, 25, 'All done', &
   gmhpos=gright,gmVpos=Gbottom,gmcallback=3161)
 


call gmSetGuiGridMode(GOn)
call gmdrawwindow(igroup)
!nset=nset0
return

end
