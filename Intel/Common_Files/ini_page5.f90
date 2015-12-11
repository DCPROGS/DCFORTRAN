subroutine ini_page(main,nset,npar,ixgrid,iygrid,nfileb,curvonly,autosim,ksim,&
readini,initmec,nlig,initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1,ini_Value2_1,ini_TxtArray2,&
	ini_Value3_1,ini_TxtArray3,ini_Value4_1 ,ini_Value4_2,ini_Value5_1,ini_TxtArray5, &
	ini_Value6_1 ,ini_Value6_3, ini_Value6_7 ,ini_Value6_8,ini_Value6_9, ini_TxtArray7,&
	ini_Toggle1_1,ini_Toggle2_1,ini_Toggle3_1,ini_Toggle4_1,ini_Toggle4_2,ini_Toggle5_1,&
	ini_Toggle6_1,ini_Toggle6_2,ini_Toggle6_4,ini_Toggle6_5,ini_Toggle6_7,ini_Toggle6_8,&
	ini_Toggle7_1,ini_Toggle7_2,ini_Text7_1,ini_Value7_2,ini_Value7_3,ini_Value7_4,ini_TxtArray4_1,ini_TxtArray4_2 ,&
	inipage,isetu,ini_Button8_1,ini_Button8_2,ini_Button8_3,ini_Value6_0,ini_Button16_3,&
	ini_Button16_4,ini_text6_01,ini_panel2,iniyes,inino,ini_text6_02,ini_Value6_12,ini_Toggle6_10,&
	ini_Value6_10,ini_Toggle8)

use menu_f90

integer nfileb(10)
logical curvonly,autosim,readini
character*100  xtext,title
character*2 cnum
!logical page_open(25)
type(GARRAYCELL) arrayattribs


initialpage=0
ini_MainPanel=0
ini_panel1=0
ini_panel2=0
ini_panel3=0
ini_panel4=0
ini_panel5=0
ini_panel6=0
ini_panel7=0
ini_panel8=0
ini_panel9=0
ini_panel10=10
ini_panel1_1=0
ini_Text1_1=0
ini_Value2_1=0
ini_TxtArray2=0
ini_Value3_1=0
ini_TxtArray3=0
ini_Value4_1=0
ini_Value4_2=0
ini_Value5_1=0
ini_TxtArray5=0
ini_Value6_1=0
ini_Value6_3=0
ini_Value6_7=0
ini_Value6_8=0
ini_Value6_9=0
ini_TxtArray7=0
ini_Toggle1_1=0
ini_Toggle2_1=0
ini_Toggle3_1=0
ini_Toggle4_1=0
ini_Toggle4_2=0
ini_Toggle5_1=0
ini_Toggle6_1=0
ini_Toggle6_2=0
ini_Toggle6_4=0
ini_Toggle6_5=0
ini_Toggle6_7=0
ini_Toggle6_8=0
ini_Toggle7_1=0
ini_Toggle7_2=0
ini_Text7_1=0
ini_Value7_2=0
ini_Value7_3=0
ini_Value7_4=0
ini_TxtArray4_1=0
ini_TxtArray4_2=0


ini_Button8_1=0
ini_Button8_2=0
ini_Button8_3=0
ini_Value6_0=0
ini_Button16_3=0
ini_Button16_4=0
ini_text6_01=0
ini_panel2=0
iniyes=0
inino=0
ini_text6_02=0
ini_Value6_12=0
icbk=0
! Define user defined colour indices used
   call gDefineRGB(220, 1.000000, 1.000000, 0.807843)
   call gDefineRGB(221, 0.796078, 1.000000, 0.592157)
   call gDefineRGB(222, 0.682353, 0.843137, 1.000000)
   call gDefineRGB(223, 1.000000, 0.909804, 0.909804)
   call gDefineRGB(224, 0.792157, 1.000000, 0.792157)
   call gDefineRGB(225, 1.000000, 0.866667, 0.733333)
   call gDefineRGB(226, 0.996078, 0.780392, 0.705882)
   call gDefineRGB(227, 1.000000, 0.458824, 0.458824)
   call gDefineRGB(228, 1.000000, 1.000000, 0.517647)
   call gDefineRGB(229, 0.576471, 0.992157, 0.839216)
   call gDefineRGB(230, 0.501961, 1.000000, 0.619608)
   call gDefineRGB(231, 0.549020, 1.000000, 0.658824)
   call gDefineRGB(232, 0.713726, 1.000000, 0.784314)
   call gDefineRGB(233, 0.686275, 1.000000, 1.000000)
   call gDefineRGB(234, 0.694118, 0.901961, 0.992157)
   call gDefineRGB(235, 0.866667, 1.000000, 0.866667)
   call gDefineRGB(236, 0.839216, 0.992157, 0.858824)
   call gDefineRGB(237, 1.000000, 0.274510, 0.274510)
   call gDefineRGB(238, 1.000000, 0.658824, 0.658824)
   call gDefineRGB(239, 1.000000, 0.556863, 0.121569)
   call gDefineRGB(240, 0.949020, 0.792157, 0.549020)
   call gDefineRGB(241, 0.176471, 0.584314, 1.000000)
   call gDefineRGB(242, 0.000000, 0.501961, 0.749020)
   call gDefineRGB(243, 0.000000, 0.576471, 0.847059)
   call gDefineRGB(244, 0.000000, 0.764706, 0.764706)
   call gDefineRGB(245, 1.000000, 1.000000, 0.501961)
   call gDefineRGB(246, 1.000000, 1.000000, 0.392157)
   call gDefineRGB(247, 0.921569, 0.937255, 0.937255)
   call gDefineRGB(248, 1.000000, 0.909804, 0.866667)
! grey palette
   call gDefineRGB(101, 0.831372, 0.901960, 0.901960)
   call gDefineRGB(102, 0.772549, 0.850980, 0.850980)
   call gDefineRGB(103, 0.745098, 0.819607, 0.819607)
   call gDefineRGB(104, 0.704117, 0.760784, 0.760784)
   call gDefineRGB(105, 0.662745, 0.729411, 0.729411)
! pink palette
	 call gDefineRGB(112,0.96,0.82,0.82)
	 call gDefineRGB(113,0.93,0.78,0.78)
	 call gDefineRGB(114,0.90,0.74,0.74)
	 call gDefineRGB(115,0.87,0.70,0.70)
	 call gDefineRGB(111,0.98,0.92,0.92)


! blue green palette

	 call gDefineRGB(122,0.80,0.89,0.87)
	 call gDefineRGB(123,0.73,0.82,0.79)
	 call gDefineRGB(124,0.66,0.97,0.87)
	 call gDefineRGB(125,0.89,0.97,0.94)
	 call gDefineRGB(121,0.87,0.97,0.94)

	 ! green palette

	 call gDefineRGB(161,0.80,1.,0.8)
	 call gDefineRGB(162,0.80,1.,0.8)
	 call gDefineRGB(163,0.44,0.87,0.44)
	 
	call gDefineRGB(171, 0.6, 0.99, 0.99)
	call gDefineRGB(172, 0.44, 0.73, 0.73)
nfilemax=0
do i=1,10
	if(nfileb(i).gt.nfilemax) nfilemax=nfileb(i)
enddo

!iniwid=960
!inipan=245
iniwid=1020
inipan=285
inilast=720
if(curvonly.or.ksim.eq.-1) then
iniwid=800
inilast=480

endif
title='Initial settings'
icolm=171
icolm1=222
ixma=5
iyma=25
if(autosim) then 
	title='Mechanism to be fitted'
	ixma=100
    iyma=150
endif
call gDefineRGB(10,0.,1.,0.)	!green
call gDefineRGB(26,0.,1.,0.5)	!green
ihtr=742
if(autosim.and.ksim.eq.-1) then
title='True reaction mechanism to be simulated'
ixma=50
iyma=100
icolm=26
icolm1=26
ihtr=550
endif
! Set up master window ini
 call gmSetGuiGridMode(GOFF) 
   initialpage = gmCreateComplexDialogueBox(main,ixma, iyma, iniwid, ihtr, GALL,title , &
        gmVpos=GTOP, gmInitState=GNORMAL)
   ixp=0
   iyp=0
   ini_MainPanel=gmCreatePanel(initialpage, ixp, iyp, iniwid, ihtr, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
        gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmScrollMode=GbothBARS)
   
! Create main panel for form
  

! Create panel ini_Panel1 child of ini_MainPanel
   ini_Panel1 = gmCreatePanel(ini_MainPanel, ixp, iyp, 460, 330, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, gmFillcol=icolm, &
      gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create button ini_Button1 child of ini_Panel1
   ini_Button1_2 = gmCreatePushButton(ini_Panel1, 380, 40, 60, 25, 'Edit', gmType=GUSERDEFINED, gmOffcol=228, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-112)

! Create panel ini_Panel9 child of ini_Panel1
   ini_Panel1_1 = gmCreatePanel(ini_Panel1, 10, 40, 360, 260, gmTitle='', gmType=GINVERSECHISEL, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmScrollMode=GbothBARS, gmVpos=GTOP)

   ini_Button1_1 = gmCreatePushButton(ini_Panel1, 380, 80, 60, 25, 'Enlarge', gmType=GUSERDEFINED, gmOffcol=224, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-111)

   ini_Text1 = gmCreateTextEntry(ini_Panel1, 20, 1, 80, 18,'Mechanism:', 255, gdisplay, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

   ini_Text1_1 = gmCreateTextEntry(ini_Panel1, 20, 20, 420, 20,' ', 255, gdisplay, gmBack1Col=0, gmBack2Col=0, gmTextCol=1 , &
     gmScrollable=Gon, gmVpos=GTOP)

if(ksim.eq.-1) then	
ini_Panel2 = gmCreatePanel(ini_MainPanel, 0, 310, 460, 215, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmFillcol=icolm, gmFillBorder=GOUTEREDGE, gmScrollMode=Gvertical, gmVpos=GTOP)
else
! Create panel ini_Panel2 child of ini_MainPanel
   ini_Panel2 = gmCreatePanel(ini_MainPanel, 0, 310, 460, 140, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmFillcol=icolm, gmFillBorder=GOUTEREDGE, gmScrollMode=GnoBARS, gmVpos=GTOP)
 ini_Text2 = gmCreateTextEntry(ini_Panel2, 20, 10, 220, 25,'# set by microscopic reversibility =', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP,gmhpos=Gleft)

   ini_Button2_2 = gmCreatePushButton(ini_Panel2, 380, 45, 60, 25, 'Edit', gmType=GUSERDEFINED, gmOffcol=228, gmTextCol=0, &
      gmVpos=GTOP,gmhpos=Gleft,gmCallback=-122)

   ini_Value2_1 = gmCreateValueEntry(ini_Panel2, 240, 10, 40, 25, 0.000000, 8, 0, gdisplay,  &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP,gmhpos=Gleft)
	
! Create text array ini_TxtArray2 child of ini_Panel2
ncolp=4
nrowp=10
   xtext='No '//char(124)//'Q(i,j) '//char(124)//'Name '//char(124)//'Cycle'
   ini_TxtArray2 = gmCreateTextArray(ini_Panel2, 10, 45, 350, 90, ncolp, nrowp, gmAxisW=40, gmAxisH=25, gmXtext=xtext, &
   gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=GBOTHBARS, gmVpos=GTOP,gmhpos=gleft)
!   call gmEnqCellAttribs(ini_TxtArray2, 1, 1, arrayattribs)
   arrayattribs.height=22
   
   arrayattribs.backcol=101
   arrayattribs.display=gdisplay
   do i=1,10
   arrayattribs.width=40
   call gmSetCellAttribs(ini_TxtArray2, 1, i, arrayattribs)
   arrayattribs.width=70
   call gmSetCellAttribs(ini_TxtArray2, 2, i, arrayattribs)
   enddo
   arrayattribs.width=90
   arrayattribs.backcol=102
    do i=1,10
   call gmSetCellAttribs(ini_TxtArray2, 3, i, arrayattribs)
    enddo
!   call gmSetCellAttribs(ini_TxtArray2, 3, 1, arrayattribs)
    arrayattribs.width=110
	arrayattribs.backcol=103
	 do i=1,10
   call gmsetCellAttribs(ini_TxtArray2, 4, i, arrayattribs)
   enddo

! Create panel ini_Panel3 child of ini_MainPanel
   ini_Panel3 = gmCreatePanel(ini_MainPanel, 0, 450, 460, 140, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmFillcol=icolm, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP,gmhpos=Gleft)

! Create text array ini_TxtArray2 child of ini_Panel3
xtext='Q(i,j) '//char(124)//'Name'//char(124)//'Factor'//char(124)//'Q(i,j) '//char(124)//'Name'
   ini_TxtArray3 = gmCreateTextArray(ini_Panel3, 10, 40, 440, 90, 5, 20, gmAxisW=40, gmAxisH=25, gmXtext=xtext, &
      gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=GBOTHBARS, gmVpos=GTOP)
   
   arrayattribs.width=70
   arrayattribs.backcol=101
   arrayattribs.display=gdisplay
   arrayattribs.height=22
   do j=1,20
   call gmSetCellAttribs(ini_TxtArray3, 1, j, arrayattribs)
   enddo
   arrayattribs.backcol=102
   arrayattribs.display=gdisplay
   arrayattribs.width=90
   do j=1,20
   call gmSetCellAttribs(ini_TxtArray3, 2, j, arrayattribs)
   enddo
    arrayattribs.backcol=103
   arrayattribs.display=gdisplay
   arrayattribs.width=70
   arrayattribs.ndp=3
   do i=1,20
   call gmSetCellAttribs(ini_TxtArray3, 3, i, arrayattribs)
   enddo
   arrayattribs.backcol=104
   arrayattribs.display=gdisplay
   arrayattribs.width=70
   do i=1,20
   call gmSetCellAttribs(ini_TxtArray3, 4, i, arrayattribs)
   enddo
   arrayattribs.backcol=105
   arrayattribs.display=gdisplay
   arrayattribs.width=90
   do i=1,20
   call gmSetCellAttribs(ini_TxtArray3, 5, i, arrayattribs)
   enddo
! Create text widget ini_Text3 child of ini_Panel3
   ini_Text3 = gmCreateTextEntry(ini_Panel3, 20, 10, 170, 25,'# of constrained rates =', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP,gmhpos=Gleft)

   ini_Button3_2 = gmCreatePushButton(ini_Panel3, 380, 10, 60, 25, 'Edit', gmType=GUSERDEFINED, gmOffcol=228, gmTextCol=0, &
      gmVpos=GTOP,gmhpos=Gleft,gmCallback=-132)

   ini_Value3_1 = gmCreateValueEntry(ini_Panel3, 180, 10, 50, 25, 0.000000, 8, 0, gdisplay, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP,gmhpos=Gleft)

! Create panel ini_Panel4 child of ini_MainPanel
   ini_Panel4 = gmCreatePanel(ini_MainPanel, 0, 590, 460, 140, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmFillcol=icolm, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

   ini_Text4_1 = gmCreateTextEntry(ini_Panel4, 20, 10, 100, 25,'# of fixed rates=', 255, gdisplay, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

   ini_Value4_1 = gmCreateValueEntry(ini_Panel4, 120, 10, 40, 25, 0.000000, 8, 0, gdisplay,&
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

xtext='Q(i,j) '//char(124)//'Name '
   ini_TxtArray4_1 = gmCreateTextArray(ini_Panel4, 10, 40, 220, 90, 2, 20, gmAxisW=40, gmAxisH=25, gmXtext=xtext, &
      gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=GBOTHBARS, gmVpos=GTOP)
   call gmEnqCellAttribs(ini_TxtArray4_1, 1, 1, arrayattribs)
 
    arrayattribs.height=22
	arrayattribs.display=gdisplay
	  arrayattribs.width=70
	  arrayattribs.backcol=101
   
   do j=1,20
   call gmSetCellAttribs(ini_TxtArray4_1, 1, j, arrayattribs)
   enddo

   arrayattribs.display=gdisplay
	  arrayattribs.width=90
	  arrayattribs.backcol=102
   
   do j=1,20
   call gmSetCellAttribs(ini_TxtArray4_1, 2, j, arrayattribs)
   enddo
! Create button ini_Button8 child of ini_Panel4
   ini_Button4_12 = gmCreatePushButton(ini_Panel4, 160, 10, 55, 25, 'Edit', gmType=GUSERDEFINED, gmOffcol=228, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-142)
! Create text widget ini_Text5 child of ini_Panel4

   ini_Text4_2 = gmCreateTextEntry(ini_Panel4, 230, 10, 140, 25,'# constrained by EC50=', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
! Create value entry ini_Value4 child of ini_Panel4
   ini_Value4_2 = gmCreateValueEntry(ini_Panel4, 370, 10, 25, 25, 0.000000, 8, 0, gdisplay,  &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
	call gmsetvaluesetting(ini_Value4_1,nfix)
	xtext='Q(i,j) '//char(124)//'Name '
   ini_TxtArray4_2 = gmCreateTextArray(ini_Panel4, 235, 40, 220, 90, 2, 20, gmAxisW=40, gmAxisH=25, gmXtext=xtext, &
      gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=GBOTHBARS, gmVpos=GTOP)
   call gmEnqCellAttribs(ini_TxtArray4_2, 1, 1, arrayattribs)
   arrayattribs.width=90
   call gmSetCellAttribs(ini_TxtArray4_2, 1, 1, arrayattribs)
    arrayattribs.display=gdisplay
	  arrayattribs.width=70
	  arrayattribs.backcol=101
   arrayattribs.height=22
   do j=1,20
   call gmSetCellAttribs(ini_TxtArray4_2, 1, j, arrayattribs)
   enddo
   arrayattribs.display=gdisplay
	  arrayattribs.width=90
	  arrayattribs.backcol=102
   
   do j=1,20
   call gmSetCellAttribs(ini_TxtArray4_2, 2, j, arrayattribs)
   enddo

   ini_Button4_22 = gmCreatePushButton(ini_Panel4, 395, 10, 55, 25, 'Edit', gmType=GUSERDEFINED, gmOffcol=228, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-144)

! Create panel ini_Panel5 child of ini_MainPanel
endif
if(.not.curvonly.and.ksim.ne.-1) then
   ini_Panel5 = gmCreatePanel(ini_MainPanel, 460, 0, 260, 280, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmFillcol=161, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text array ini_TxtArray5 child of ini_Panel5
if(autosim) then
		xtext='Intervals '//char(124)//'Intervals '
		nmax=2
	else
	do j=1,nset
	do i=1,nfileb(j)	
    if(nfileb(j).gt.nmax) nmax=nfileb(j)
	enddo
	enddo 
	if(nmax.eq.1) nmax=2
	nfile=nmax
	
	if(nmax.eq.2) then
			xtext='file 1'//char(124)//'file 2'
	else if(nmax.eq.3) then
			xtext='file 1'//char(124)//'file 2'//char(124)//'file 3'
	
	else if(nmax.eq.4) then
			xtext='file 1'//char(124)//'file 2'//char(124)//'file 3'&
			//char(124)//'file 4'
	else if(nmax.eq.5) then
			xtext='file 1'//char(124)//'file 2'//char(124)//'file 3'&
			//char(124)//'file 4'//char(124)//'file 5'
	else if(nmax.eq.6) then
			xtext='file 1'//char(124)//'file 2'//char(124)//'file 3'&
			//char(124)//'file 4'//char(124)//'file 5'//char(124)//'file 6'
	else if(nmax.eq.7) then
			xtext='file 1'//char(124)//'file 2'//char(124)//'file 3'&
			//char(124)//'file 4'//char(124)//'file 5'//char(124)//'file 6'&
			//char(124)//'file 7'
	else if(nmax.eq.8) then
			xtext='file 1'//char(124)//'file 2'//char(124)//'file 3'&
			//char(124)//'file 4'//char(124)//'file 5'//char(124)//'file 6'&
			//char(124)//'file 7'//char(124)//'file 8'
	else if(nmax.eq.9) then
			xtext='file 1'//char(124)//'file 2'//char(124)//'file 3'&
			//char(124)//'file 4'//char(124)//'file 5'//char(124)//'file 6'&
			//char(124)//'file 7'//char(124)//'file 8'//char(124)//'file 9'
	else if(nmax.ge.10) then
			xtext='file 1'//char(124)//'file 2'//char(124)//'file 3'&
			//char(124)//'file 4'//char(124)//'file 5'//char(124)//'file 6'&
			//char(124)//'file 7'//char(124)//'file 8'//char(124)//'file 9'&
			//char(124)//'file 10'
	endif
	endif
   ini_TxtArray5 = gmCreateTextArray(ini_Panel5, 5, 90, 250, 140, nmax, 10, gmAxisW=30, gmAxisH=25, gmXtext=xtext, &
      gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=GBOTHBARS, gmVpos=GTOP)
  
  
   arrayattribs%width=150
  
     arrayattribs%height=22
	 arrayattribs%display=gdisplay
	 
	  
	 do j=1,10
	    
	   do i=1,nmax
	   arrayattribs%backcol=100+i
		call gmSetCellAttribs(ini_TxtArray5, i, j, arrayattribs)
     enddo
   enddo
  
   
! Create text widget ini_Text6 child of ini_Panel5
   ini_Text5 = gmCreateTextEntry(ini_Panel5, 20, 10, 50, 25,'Data ', 255, gdisplay, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text7 child of ini_Panel5
   ini_Text5_1 = gmCreateTextEntry(ini_Panel5, 20, 50, 130, 25,'Number of sets=', 255, gdisplay, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create button ini_Button12 child of ini_Panel5
   ini_Button5_2 = gmCreatePushButton(ini_Panel5, 100, 10, 100, 25, 'Browse/Edit', gmType=GUSERDEFINED, gmOffcol=228, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-152)
	
	ini_Button5_3 = gmCreatePushButton(ini_Panel5, 100, 250, 100, 25, 'View Details', gmType=GUSERDEFINED, gmOffcol=0, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-153)
! Create value entry ini_Value5 child of ini_Panel5
   
   ini_Value5_1 = gmCreateValueEntry(ini_Panel5, 160, 50, 50, 25, 0.000000, 8, 0, gdisplay, &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

! Create panel ini_Panel6 child of ini_MainPanel
   ini_Panel6 = gmCreatePanel(ini_MainPanel, 460, 280, 260, 400, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmFillcol=161, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text widget ini_Text8 child of ini_Panel6
   ini_Text6 = gmCreateTextEntry(ini_Panel6, 20, 10, 60, 25,'Set = ', 255, gdisplay, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
   call gmSetFontAttribs(ini_Text6, gmBold=1)

   ini_Button6_2 = gmCreatePushButton(ini_Panel6, 150, 10, 60, 25, 'Edit', gmType=GUSERDEFINED, gmOffcol=228, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-162)

! Create text widget ini_Text9 child of ini_Panel6
ini_Text6_010 = gmCreateTextEntry(ini_Panel6, 20, 45, 100, 20,'Ligand:', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)


ini_Text6_1 = gmCreateTextEntry(ini_Panel6, 150, 45, 110, 20,'Concentration ', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
ini_Text6_1 = gmCreateTextEntry(ini_Panel6, 150, 60, 110, 15,' [micromolar]', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
! Create text widget ini_Text10 child of ini_Panel6
ini_Text6_01 = gmCreateTextEntry(ini_Panel6, 20, 80, 100, 20,'', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

ini_Value6_1 = gmCreateValueEntry(ini_Panel6, 160, 80, 80, 25, 0.000000, 11, 4, gdisplay, &
      gmType=Gdecimal, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
if(nlig.eq.2) then
ini_Text6_02 = gmCreateTextEntry(ini_Panel6, 20, 100, 100, 20,'', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

ini_Value6_12 = gmCreateValueEntry(ini_Panel6, 160, 100, 80, 20, 0.000000, 11, 4, gdisplay,  &
      gmType=Gdecimal, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
endif
   ini_Text6_2 = gmCreateTextEntry(ini_Panel6, 20, 120, 140, 25,'One channel only ', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text11 child of ini_Panel6
   ini_Text6_3 = gmCreateTextEntry(ini_Panel6, 20, 150, 130, 25,'Critical shut time (ms)=', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text12 child of ini_Panel6
   ini_Text6_4 = gmCreateTextEntry(ini_Panel6, 20, 180, 140, 25,'Use CHS vectors', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text13 child of ini_Panel6
   ini_Text6_5 = gmCreateTextEntry(ini_Panel6, 20, 210, 150, 25,'Bad shutting ends group ', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text14 child of ini_Panel6
   ini_Text6_6 = gmCreateTextEntry(ini_Panel6, 20, 250, 200, 25,'Set intervals bad if longer than:', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create toggle button ini_Toggle7 child of ini_Panel6
   ini_Toggle6_7 = gmCreateToggleButton(ini_Panel6, 110, 280, 20, 25, '', 0,&
    gmType=G3DCHECKBOX, gmVpos=GTOP,gmstatus=gunselectable)

! Create toggle button ini_Toggle8 child of ini_Panel6
   ini_Toggle6_8 = gmCreateToggleButton(ini_Panel6, 110, 310, 20, 25, '', 0, gmType=G3DCHECKBOX, &
   gmVpos=GTOP,gmstatus=gunselectable)
   ini_Text6_5 = gmCreateTextEntry(ini_Panel6, 130, 280, 115, 25,'[shut times (ms)]', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

! Create text widget ini_Text14 child of ini_Panel6
   ini_Text6_6 = gmCreateTextEntry(ini_Panel6, 130, 310, 115, 25,'[open times (ms)]', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
 ini_Text6_9 = gmCreateTextEntry(ini_Panel6, 20, 340, 140, 20,'Resolution (microsec) =', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)


! Create button ini_Button14 child of ini_Panel6
   ini_Button16_3 = gmCreatePushButton(ini_Panel6, 150, 370, 75, 20, 'Next set', gmVpos=GTOP,gmCallback=-163)
	if(nset.eq.1) call gmsetwidgetstatus(ini_Button16_3,gunselectable)
! Create button ini_Button15 child of ini_Panel6
   ini_Button16_4 = gmCreatePushButton(ini_Panel6, 70, 370, 75, 20, 'Prev. set', gmVpos=GTOP,gmCallback=-164)
call gmsetwidgetstatus(ini_Button16_4,gunselectable)


! Create toggle button ini_Toggle10 child of ini_Panel6
   ini_Toggle6_2 = gmCreateToggleButton(ini_Panel6, 180, 120, 30, 25, '', 0, &
   gmType=G3DCHECKBOX, gmVpos=GTOP,gmstatus=gunselectable)

! Create toggle button ini_Toggle11 child of ini_Panel6
   ini_Toggle6_4 = gmCreateToggleButton(ini_Panel6, 180, 180, 30, 25, '', 0, &
   gmType=G3DCHECKBOX, gmVpos=GTOP,gmstatus=gunselectable)
 ! ini_Toggle2_1 = gmCreateToggleButton(ini_Panel2, 360, 70, 25, 25, '', 0, gmType=G3DCHECKBOX, gmVpos=GTOP)

! Create toggle button ini_Toggle12 child of ini_Panel6
   ini_Toggle6_5 = gmCreateToggleButton(ini_Panel6, 180, 210, 30, 25, '', 0, &
   gmType=G3DCHECKBOX, gmVpos=GTOP,gmstatus=gunselectable)
ini_Value6_0 = gmCreateValueEntry(ini_Panel6, 90, 10, 60, 25, 1, 5, 0, gdisplay, &
      gmType=Gdecimal, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
   ini_Value6_3 = gmCreateValueEntry(ini_Panel6, 180, 150, 80, 25, 0.000000, 4, 1, gdisplay, &
      gmType=Gdecimal, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
   ini_Value6_7 = gmCreateValueEntry(ini_Panel6, 20, 280, 80, 25, 0.000000, 8, 2, gdisplay, &
      gmType=Gdecimal, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
	call gmsetvaluesetting(ini_Value6_7,0.)
ini_Value6_8 = gmCreateValueEntry(ini_Panel6, 20, 310, 80, 25, 0.000000, 8, 2, gdisplay, &
      gmType=Gdecimal, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
	call gmsetvaluesetting(ini_Value6_8,0.)
	ini_Value6_9 = gmCreateValueEntry(ini_Panel6, 170, 340, 80, 25, 0.000000, 6, 1, gdisplay, &
      gmType=Gdecimal, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

ini_Panel60 = gmCreatePanel(ini_MainPanel, 460, 680, 260, 50, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmFillcol=161, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)
ini_Text6_10 = gmCreateTextEntry(ini_Panel60, 20, 5, 140, 20,'Constrain max Popen  ', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
ini_Toggle6_10 = gmCreateToggleButton(ini_Panel60, 20, 25, 30, 20, '', 0, &
   gmType=G3DCHECKBOX, gmVpos=GTOP,gmstatus=gunselectable)
   ini_Value6_10 = gmCreateValueEntry(ini_Panel60, 60, 25, 100, 20, 0.000000, 8, 4, gdisplay, &
      gmType=Gdecimal, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
    ini_Button6_10 = gmCreatePushButton(ini_Panel60, 170, 10, 60, 25, 'Edit', gmType=GUSERDEFINED, gmOffcol=228, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-165)   
endif

! Create panel ini_Panel7 child of ini_MainPanel
   ini_Panel7 = gmCreatePanel(ini_MainPanel, inilast, 0, inipan, 450, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmFillcol=icolm1, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

! Create text array ini_TxtArray7 child of ini_Panel7
	xtext='Q(i,j)'//char(124)//'Rate name '//char(124)//'Value'
   ini_TxtArray7 = gmCreateTextArray(ini_Panel7, 10, 90, inipan-15, 350, 3, npar, gmAxisW=40, gmAxisH=25, gmXtext=xtext, &
      gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=GBOTHBARS, gmVpos=GTOP)
   call gmEnqCellAttribs(ini_TxtArray7, 1, 1, arrayattribs)
   arrayattribs.height=22
   arrayattribs.width=60
   
  arrayattribs.backcol=121
   arrayattribs.display=gdisplay
   do i=1,npar
   call gmSetCellAttribs(ini_TxtArray7, 1, i, arrayattribs)
   enddo
   arrayattribs.width=70
     arrayattribs.backcol=122
   arrayattribs.display=gdisplay
   do i=1,npar
   call gmSetCellAttribs(ini_TxtArray7, 2, i, arrayattribs)
   enddo
   arrayattribs.backcol=123
   arrayattribs.width=90
   arrayattribs.display=gedit

   do i=1,npar
   call gmSetCellAttribs(ini_TxtArray7, 3, i, arrayattribs)
   enddo
! Create text widget ini_Text16 child of ini_Panel7
if(autosim.and.ksim.eq.-1) then
ini_Text7 = gmCreateTextEntry(ini_Panel7, 10, 10, 170, 25,'True rates ', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
	  icbk=-172
else
   ini_Text7 = gmCreateTextEntry(ini_Panel7, 10, 10, 180, 25,'Initial guesses;check if correct ', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=2 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
	icbk=-172
endif

   ini_Button7_2 = gmCreatePushButton(ini_Panel7, inipan-70, 10, 60, 25, 'Edit', gmType=GUSERDEFINED, gmOffcol=228, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=icbk)


   ini_Text7_0 = gmCreateTextEntry(ini_Panel7, 10, 35, 50, 25,'File: ', 255, gdisplay, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
ini_Text7_1 = gmCreateTextEntry(ini_Panel7, 10, 60, 260, 25,'', 255, gdisplay, gmBack1Col=0, gmBack2Col=0, gmTextCol=1 , &
     gmScrollable=Gon, gmVpos=GTOP)
 if(ksim.ne.-1) then
 ini_Panel10 = gmCreatePanel(ini_MainPanel, inilast, 450, inipan, 105, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmFillcol=icolm1, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)

	 ini_Text7_2 = gmCreateTextEntry(ini_Panel10, 10, 10, 150, 25,'Maximum values for:', 255, gdisplay, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
ini_Text7_3 = gmCreateTextEntry(ini_Panel10, 10, 40, 110, 25,'association rates=', 255, gdisplay, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
ini_Text7_4 = gmCreateTextEntry(ini_Panel10, 10, 70, 110, 25,'all other rates=', 255, gdisplay, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
 ini_Value7_3 = gmCreateValueEntry(ini_Panel10, 130, 40, 90, 25, 0.000000, 8, 3, gdisplay, &
      gmType=Gscientific, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

   ini_Value7_4 = gmCreateValueEntry(ini_Panel10, 130, 70, 90, 25, 0.000000, 8, 3, gdisplay, &
      gmType=Gscientific, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
  ini_Button7_4 = gmCreatePushButton(ini_Panel10, inipan-70, 10, 60, 25, 'Edit', gmType=GUSERDEFINED, gmOffcol=228, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-174)

 endif
call gDefineRGB(131,0.89,0.85,1.)!petrol

call gDefineRGB(132,0.75,0.67,1.)!lilac

call gDefineRGB(133,0.65,0.53,1.)!cyan

if (curvonly) then
 
 
 
ini_Panel6 = gmCreatePanel(ini_MainPanel, inilast, 570, inipan, 80, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmFillcol=223, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)
ini_Text6_9 = gmCreateTextEntry(ini_Panel6, 20, 2, 110, 18,'Resolution [us]=', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 , gmVpos=GTOP)

ini_Value6_9 = gmCreateValueEntry(ini_Panel6, 130, 2, 70, 18, 0.000000, 6, 1, gdisplay,  &
      gmType=Gdecimal, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

ini_Button6_2 = gmCreatePushButton(ini_Panel6, 210, 2, 60, 18, 'Edit', gmType=GUSERDEFINED, gmOffcol=228, gmTextCol=0, &
      gmVpos=GTOP,gmCallback=-162)
ini_Text6_010 = gmCreateTextEntry(ini_Panel6, 20, 20, 100, 20,'Ligand:', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 , gmVpos=GTOP)

ini_Text6_1 = gmCreateTextEntry(ini_Panel6, 150, 20, 130, 20,'Concentration [uM]', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
ini_Text6_01 = gmCreateTextEntry(ini_Panel6, 20, 40, 100, 20,'', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
ini_Value6_1 = gmCreateValueEntry(ini_Panel6, 160, 40, 80, 20, 0.000000, 11, 3, gdisplay,  &
      gmType=Gdecimal, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)

if(nlig.eq.2) then
ini_Text6_02 = gmCreateTextEntry(ini_Panel6, 20, 60, 100, 20,'', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

ini_Value6_12 = gmCreateValueEntry(ini_Panel6, 160, 60, 80, 20, 0.000000, 11, 4, gdisplay, &
      gmType=Gdecimal, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
endif

if(ksim.ne.-1) then
	  ini_Panel8 = gmCreatePanel(ini_MainPanel, inilast,650 , inipan, 80, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmFillcol=101, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)
 	  iniyes = gmCreatePushButton(ini_Panel8, 10, 5,inipan/2-20 , 20, 'Save to ini file',&
      gmType=GUSERDEFINED, gmOffcol=161, &
      gmTextCol=1,gmhpos=gleft, gmVpos=GTOP,gmCallback=8004)
	  imecyes = gmCreatePushButton(ini_Panel8, inipan/2, 5,inipan/2-20 , 20, 'Save to mec file',&
      gmType=GUSERDEFINED, gmOffcol=162, &
      gmTextCol=1,gmhpos=gleft, gmVpos=GTOP,gmCallback=4434)!1881
      ini_Text8 = gmCreateTextEntry(ini_Panel8, 10, 30, 210, 20,'All checked; proceed now: ', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextcol=1 , gmVpos=GTOP)
      call gmSetFontAttribs(ini_Text8, gmBold=1)
      ini_Button8_2 = gmCreatePushButton(ini_Panel8, 10, 55,inipan-20 , 25, 'Curves only',&
      gmType=GUSERDEFINED, gmOffcol=142, &
      gmTextCol=1, gmVpos=GTOP,gmCallback=-148)
endif
else
! Create panel ini_Panel8 child of ini_MainPanel
if(ksim.eq.-1) then

   ini_Button8_1 = gmCreatePushButton(ini_mainPanel, inilast, 475, inipan, 50, 'Continue ', gmType=GUSERDEFINED, &
   gmOffcol=38, gmTextCol=1, &
      gmVpos=GTOP,gmCallback=-37)
else
!ini_Toggle8 = gmCreateToggleButton(ini_MainPanel, inilast+10, 555, 210, 25,'  Calculate likelihood surface', 0, &
 !  gmType=G3DCHECKBOX, gmTextcol=2, gmVpos=GTOP)

   ini_Panel8 = gmCreatePanel(ini_MainPanel, inilast, 550, inipan, 175, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, &
      gmFillcol=101, gmFillBorder=GOUTEREDGE, gmScrollMode=GNOBARS, gmVpos=GTOP)
       iniyes = gmCreatePushButton(ini_Panel8, 10, 5,inipan/2-20 , 20, 'Save to ini file',&
      gmType=GUSERDEFINED, gmOffcol=161, &
      gmTextCol=1,gmhpos=gleft, gmVpos=GTOP,gmCallback=8004)
	  
	  imecyes = gmCreatePushButton(ini_Panel8, inipan/2, 5,inipan/2-20 , 20, 'Save to mec file',&
      gmType=GUSERDEFINED, gmOffcol=162, &
      gmTextCol=1,gmhpos=gleft, gmVpos=GTOP,gmCallback=4434)
! Create text widget ini_Text18 child of ini_Panel8
   ini_Text8 = gmCreateTextEntry(ini_Panel8, 10, 30, 210, 25,'All checked; proceed now: ', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextcol=1 , gmVpos=GTOP)
  
   ini_Button8_1 = gmCreatePushButton(ini_Panel8, 10, 60, inipan-20, 25, 'Fit ', gmType=GUSERDEFINED, &
   gmOffcol=141, gmTextCol=1, &
      gmVpos=GTOP,gmCallback=-147)

! Create button ini_Button20 child of ini_Panel8
   ini_Button8_2 = gmCreatePushButton(ini_Panel8, 10, 90,inipan-20 , 25, 'Show data and specified model (no fit)',&
    gmType=GUSERDEFINED, gmOffcol=142, &
      gmTextCol=1, gmVpos=GTOP,gmCallback=-148)
  if(.not.autosim) then
call gmSetFontAttribs(ini_Text8, gmBold=1)
      ini_Button8_2 = gmCreatePushButton(ini_Panel8, 10, 120,inipan-20 , 25, 'Show only curves for specified model',&
      gmType=GUSERDEFINED, gmOffcol=142, &
      gmTextCol=1, gmVpos=GTOP,gmCallback=-149)
      ini_Button8_4 = gmCreatePushButton(ini_Panel8, 10, 150,inipan-20 , 25, 'Calculate likelihood surface',&
      gmType=GUSERDEFINED, gmOffcol=162, &
      gmTextCol=1, gmVpos=GTOP,gmCallback=-146)
  endif
  endif 
  endif
 if(autosim.and.ksim.ne.-1.and.readini.and.initmec.eq.0) goto 11	  
  call gmdrawwindow(initialpage)
  if(autosim.and.ksim.eq.-1) then
 endif
11 return

end
