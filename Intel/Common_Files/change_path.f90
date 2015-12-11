subroutine change_path(main,initwin1,initwin1_TxtArray,nset,icallid,&
pfiles,nfileb,isw)
use menu_f90
character*60 textb
character*60 pfiles(20,10)
integer nfileb(10)
type (GLIMIT) :: Graphics_frame
type(GARRAYCELL) arrayattribsss
call gDefineRGB(101, 0.831372, 0.901960, 0.901960)
call gDefineRGB(102, 0.772549, 0.850980, 0.850980)
call gDefineRGB(38,1.,0.5,0.25)	!orange
call gDefineRGB(21,1.,0.,0.5)	!fuschia
call gmSetGuiGridMode(GOff)
nmax=1
do j=1,nset
do i=1,nfileb(j)	
    if(nfileb(j).gt.nmax) nmax=nfileb(j)
enddo
enddo 
if(nmax.eq.1) then
		
			textb='file 1'
	else if(nmax.eq.2) then
			textb='file 1'//char(124)//'file 2'
	else if(nmax.eq.3) then
			textb='file 1'//char(124)//'file 2'//char(124)//'file 3'
	
	else if(nmax.eq.4) then
			textb='file 1'//char(124)//'file 2'//char(124)//'file 3'&
			//char(124)//'file 4'
	else if(nmax.eq.5) then
			textb='file 1'//char(124)//'file 2'//char(124)//'file 3'&
			//char(124)//'file 4'//char(124)//'file 5'
	else if(nmax.eq.6) then
			textb='file 1'//char(124)//'file 2'//char(124)//'file 3'&
			//char(124)//'file 4'//char(124)//'file 5'//char(124)//'file 6'
	else if(nmax.eq.7) then
			textb='file 1'//char(124)//'file 2'//char(124)//'file 3'&
			//char(124)//'file 4'//char(124)//'file 5'//char(124)//'file 6'&
			//char(124)//'file 7'
	else if(nmax.eq.8) then
			textb='file 1'//char(124)//'file 2'//char(124)//'file 3'&
			//char(124)//'file 4'//char(124)//'file 5'//char(124)//'file 6'&
			//char(124)//'file 7'//char(124)//'file 8'
	else if(nmax.eq.9) then
			textb='file 1'//char(124)//'file 2'//char(124)//'file 3'&
			//char(124)//'file 4'//char(124)//'file 5'//char(124)//'file 6'&
			//char(124)//'file 7'//char(124)//'file 8'//char(124)//'file 9'
	else if(nmax.ge.10) then
			textb='file 1'//char(124)//'file 2'//char(124)//'file 3'&
			//char(124)//'file 4'//char(124)//'file 5'//char(124)//'file 6'&
			//char(124)//'file 7'//char(124)//'file 8'//char(124)//'file 9'&
			//char(124)//'file 10'
	endif
iwm=21+(nmax+1)*150
if(iwm.gt.1000) iwm=1000
if(iwm.lt.360) iwm=360
ihm=240
if(nset.gt.4) ihm=ihm+ (nset-4)*30
 if(isw.eq.-1) then
 initwin1 = gmCreateComplexDialogueBox(Main,200,200,iwm,ihm, Gminimize, 'Enter/Change data files: ', &
              	gmhpos=gleft,gmvpos=gtop,gmIconFormat=GDLLICON,gmIconFile='MBIG1052')
 else
   initwin1 = gmCreateComplexDialogueBox(Main,200,200,iwm,ihm, Gminimize, 'Change path for data files: ', &
              	gmhpos=gleft,gmvpos=gtop,gmIconFormat=GDLLICON,gmIconFile='MBIG1052')
 endif
! Create main panel for form
   initwin1_MainPanel=gmCreatePanel(initwin1, 0, 0, iwm,ihm , gmHpos=GCENTRE, gmVpos=Gtop, gmExpand=GON, gmType=GNOBOUNDARY, &
   gmLineBorder=GOUTEREDGE, gmFillCol=101, gmFillBorder=GOUTEREDGE)
   if(nmax.eq.1) nmax=nmax+1
   initwin1_TxtArray = gmCreateTextArray(initwin1_mainPanel, 10, 20, iwm-21, ihm-100, nmax, 10, gmAxisW=30, gmAxisH=25, &
			 gmXtext=textb,gmYtext='*digits', gmXjust=Gcentre, gmYjust=GCENTRE, &
			 gmScrollMode=GBOTHBARS, gmhpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
             call gmEnqCellAttribs(initwin1_TxtArray, 1, 1, arrayattribsss)
  
			arrayattribsss%width=200
  
			arrayattribsss%height=22
			arrayattribsss%display=gedit
	 
			do i=1,2
				arrayattribsss%backcol=0
				do j=1,10
				call gmSetCellAttribs(initwin1_TxtArray, i, j, arrayattribsss)
				enddo
			 enddo
			do j=1,nset
				do i=1,nfileb(j)	
				call gmSetCellSetting(initwin1_TxtArray, i,j ,gmString=pfiles(i,j))
				call gmdrawwidget(initwin1_TxtArray)
				enddo
			enddo
    initwin1_Button_m = gmCreatePushButton(initwin1_MainPanel, 0, 0, 180, 25, 'Browse', &
    gmVpos=Gbottom,gmType=Guserdefined,gmOffcol=21,gmcallback=-220) !-40)


 !initwin1_Button6 = gmCreatePushButton(initwin1_MainPanel, 0, 0, 180, 25, 'Save changes to ini  file', &
 !   gmVpos=Gbottom,gmType=GDEFAULTBUTTON,gmcallback=8006) !-40)
initwin1_Button7 = gmCreatePushButton(initwin1_MainPanel, 180, 0, 180, 25, 'Continue', &
    gmVpos=Gbottom,gmType=Guserdefined,gmOffcol=38,gmcallback=icallid) !-40)
 call gmSetGuiGridMode(GOn)
   call gmdrawwindow(initwin1)
  end