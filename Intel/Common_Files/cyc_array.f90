!     Last change:  D    15 Jan 104    4:06 pm
 subroutine cyc_array(imodel,main,itwin1,n,m,text_box,itext_box,icall,&
 icyc_form,icmr,comboname,ncomboname)
! TEXT-ARRAY
	use menu_f90
	integer :: icyc_Form(500),ictype2(10)
	integer :: iwcell(10),ncomboname(10)
	character*20 comboname(10,100)
	character*30 text_box(100,10),name(4),name1(4)
	integer itext_box(100,10),ititle(10),icmr(50)
	character*100 xtext
	character*50 title
	character cnum*11
	type(GARRAYCELL) arrayattribs
	call gmSetGuiGridMode(GOFF)
	ix=100
	ncols=m
	nrows=n
	 call gDefineRGB(122,0.80,0.89,0.87)
	 call gDefineRGB(123,0.73,0.82,0.79)
	 call gDefineRGB(124,0.66,0.97,0.87)
	 call gDefineRGB(125,0.89,0.97,0.94)
	 call gDefineRGB(121,0.87,0.97,0.94)
	  call gDefineRGB(112,0.96,0.82,0.82)
	 call gDefineRGB(113,0.93,0.78,0.78)
	 call gDefineRGB(114,0.90,0.74,0.74)
	 call gDefineRGB(115,0.87,0.70,0.70)
	 call gDefineRGB(111,0.98,0.92,0.92)
	if(icall.eq.706) then
		xtext='Rate 1='//char(124)//'factor'//char(124)//'x Rate 2'//char(124)//'Obs'
		title='Multiplicative Constraints'
		do i=1,ncols
			iwcell(i)=120
			ix=ix+iwcell(i)
		enddo
		icall1=707
	else if(icall.eq.2022.or.icall.eq.2013.or.icall.eq.2011.or.icall.eq.2018.or.icall.eq.2019) then
		xtext='Cycle'//char(124)//'No States'//char(124)//'States'//char(124)//'Mic rev'//char(124)//'i,j'
		title='Specify i,j for the rate constant to be set by  MR'
		do i=1,ncols
			iwcell(i)=55
			ix=ix+iwcell(i)
		enddo
		iwcell(3)=100
		nn=4
		icall1=708
		name(1)='yes'
		name(2)='no'
		name(4)='auto'
		name(3)='bad'
		if(icall.eq.2013) nn=3
		if(icall.eq.2011) nn=2
	else
		xtext='Link'//char(124)//'Cycles'//char(124)//'MR'
		do i=1,ncols
			iwcell(i)=150
			ix=ix+iwcell(i)
		enddo
		title='Spanning tree method'
	!	icall1=708
	endif

	ncols=m
	nrows=n
	ix=ix+20
	call intconv(nrows,cnum)

	itwin1= gmCreateComplexDialogueBox(Main,200,10, ix, 14*24, GALL, title, &
              	 gmIconFormat=GDLLICON,gmIconFile='Gee',gmvpos=gmiddle)

	if(icall.eq.706) iText1 = gmCreateTextEntry(itwin1, 0, 0, 200, 20,'Initial number of rates ='//cnum,&
	  255, GEDIT, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)

    ipanelcy = gmCreatePanel(itwin1, 0, 24, ix, 12*24, &
              	gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text array Form1_TxtArray1 child of Form1_Panel1
   icyc_form(imodel) = gmCreateTextArray(iPanelcy, 0, 0, ix, 12*24, ncols, nrows, &
              	gmAxisW=50, gmAxisH=25, gmXtext=xtext, gmYtext='*digits', &
              	gmXjust=Gleft, gmYjust=GCENTRE, gmScrollMode=GBOTHBARS, gmExpand=GOFF,gmcallback=icall1)

   ictype1 = gmCreateComboBox(icyc_Form(imodel),0,0,iwcell(3),96,Gnone, 1, &
          gmSort=GunSORTED, gmVpos=GTOP, gmExpand=GOFF)
  
   do i=1,nn
	 call gmSetListEntry(ictype1, GADD, gmString=name(i))
   enddo
   	do j=1,n
!	iheight=25*ncomboname(j) to accommoDATEW ncomboname(j) entries in drop down list
!  -actually any large number will do: the list trims itself to the number of entries
	iheight=500
 	ictype2(j) = gmCreateComboBox(icyc_Form(imodel),0,0,iwcell(3),iheight,Gnone, 1, &
          gmSort=GunSORTED, gmVpos=GTOP, gmExpand=GOFF)
!	ictype2(j) = gmCreateComboBox(icyc_Form(imodel),0,0,iwcell(3),96,Gnone, 1, &
!         gmSort=GunSORTED, gmVpos=GTOP, gmExpand=GOFF)
	do i=1,ncomboname(j)
		call gmSetListEntry(ictype2(j), GADD, gmString=comboname(j,i))
	enddo
	enddo
	call gmenqcellattribs(icyc_form(imodel),1,1,arrayattribs)
	call gmSetListEntry(ictype1, GADD, gmString=name1(1))
	call gmSetListEntry(ictype1, GADD, gmString=name1(2))
	
	
	do i=1,ncols
		arrayattribs%width=iwcell(i)
			arrayattribs%flag = 0
			arrayattribs%backcol=110+i
			if(i.gt.3) then
				arrayattribs%textcol=7
				arrayattribs%display=gedit
			else
				arrayattribs%textcol=0
				arrayattribs%display=gdisplay
			endif
		do j=1,nrows
		
			call gmSetCellAttribs(icyc_form(imodel), i, j, arrayattribs)
			if(icall.eq.2022) then
			if(i.eq.4) then
				call gmSetCellSetting(icyc_form(imodel), i,j ,gmString=text_box(j,i))
				arrayattribs%flag = ictype1
				call gmSetCellAttribs(icyc_form(imodel), i, j, arrayattribs)
				call gmSetListEntry(ictype1,GSELECT,gmString=text_box(j,4))
			else if(i.eq.5) then
				call gmSetCellSetting(icyc_form(imodel), i,j ,gmString=text_box(j,i))
				arrayattribs%flag = ictype2(j)
				call gmSetCellAttribs(icyc_form(imodel), i, j, arrayattribs)
				call gmSetListEntry(ictype2(j),GSELECT,gmString=text_box(j,5))
			else
				call gmSetCellSetting(icyc_form(imodel), i,j ,gmString=text_box(j,i))
			endif
			else
				call gmSetCellSetting(icyc_form(imodel), i,j ,gmString=text_box(j,i))
			endif
			
		enddo
   enddo
   if(icall.eq.2011.or.icall.eq.2018.or.icall.eq.2019) then
   ivb=gmCreatePushButton(itwin1,ix/3,0, ix/3, 24,'All cycles obey MR',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=2011)
   ivb=gmCreatePushButton(itwin1,2*ix/3,0, ix/3, 24,'No cycles obey MR',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=2019)
   if(icall.eq.2018) ivb=gmCreatePushButton(itwin1,0,0, ix/3, 24,'Use same MR',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=2018)
   else
   ivb=gmCreatePushButton(itwin1,ix/2,0, ix/2, 24,'OK',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=icall)
 ivb=gmCreatePushButton(itwin1,0,0, ix/2, 24,'Cancel',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=-127)
   endif
   call gmSetGuiGridMode(GOn)
   call gmdrawwindow(itwin1)
   ! all for enquiry cell setting!!!!
   end