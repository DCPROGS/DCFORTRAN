subroutine values_list(Main,iData_list,ixd,iyd,ValArray6,button6,jset,iplot,ncols,nrows,&
           xdata,stitle,niobs,njset,nplot,mtitle,static6,nset,gfile,newfile,iDataMainPanel)

use menu_f90

integer :: Main
integer :: iData_list
integer :: DataMainPanel
integer :: ValArray6(50,50)
integer :: Static6
integer :: Button6(10)
real :: xdata(nplot,njset,ncols,niobs)
character*60 stitle
character*70 mtitle
character*11 cnumb,stext
character*60 xtext,gfile
logical newfile
type(GARRAYCELL) arrayattribs

ixunit=15
if(newfile) then
	stext='Save only'
	ind=5800
	ind1=5600
else
	stext='Plot only'
	ind1=5500
	ind=5700
endif
if(mtitle(1:5).eq.'Model') then
	xtext='pr0 '//char(124)//' amp'//char(124)//' grp'//&
	char(124)//'S.D.'
else
	xtext='X obs'//char(124)//' Y obs'//char(124)//' Weights'//&
	char(124)//'S.D.'
endif

if(jset.eq.1) then
    nl=len_trim(gfile)
	CALL INTCONV(iplot,cnumb)
	mtitle=	gfile(1:nl)//'Record '//cnumb(1:3)
	if(nset.eq.1) then
		ix_data=ixunit
		iy_data=10
		ixb=5
	else if(nset.eq.2) then
		ix_data=2*ixunit
		iy_data=10
		ixb=10
	else if(nset.eq.3.or.nset.eq.4) then
		ix_data=2*ixunit
		iy_data=20
		ixb=10
	else
	    ix_data=2*ixunit
		iy_data=28
		ixb=10
	endif
	iData_list = gmCreateComplexDialogueBox(Main, 8+ixd, iyd, ix_data+1, iy_data+1, GALL, mtitle, &
				 gmMaxWidth=32,gmMaxHeight=20,gmvpos=gtop,gmhpos=gleft,&
              	 gmIconFormat=GDLLICON,gmIconFile='MBIG1040',gmCallBack=-61,gmSelect=-62)

iDataMainPanel=gmCreatePanel(iData_list, 0, 1, ix_data+1, iy_data, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmExpand=GOff, gmType=GNOBOUNDARY,gmscrollmode=gvertical, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)


   
   ! Create text entry Static4 child of DataMainPanel
   
   Button6(1)=gmCreatePushButton(iData_list,0,0, ixb, 1,'Save and plot',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=GTOp, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=ind1+iplot)

   Button6(2)=gmCreatePushButton(iData_list,ixb,0, ixb, 1,stext,&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=GTOp, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=ind+iplot)

   Button6(3)=gmCreatePushButton(iData_list,2*ixb,0, ixb, 1,'Reset',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=GTOp, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=5900+iplot)

   


endif


! Create main panel for form
	if(mod(jset,2).eq.0) then
		ixm=ixunit
		iym=1+8*((jset-2)/2) 
	!!	if(iym.gt.20) iym=20
	else
		ixm=0	
		iym=1+8*((jset-1)/2)  
	!!	if(iym.gt.20) iym=20
	endif

   DataMainPanel=gmCreatePanel(iDataMainPanel, ixm, iym, ixunit, 8, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmExpand=GOff, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
   Static6 = gmCreateTextEntry(DataMainPanel, 0, 0, 16, 1,stitle, 32768, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
! Create value array ValArray6 child of DataMainPanel
   ValArray6(iplot,jset) = gmCreateValueArray(DataMainPanel, 0, 1, ixunit, 7, ncols, nrows, &
              	gmAxisW=40, gmAxisH=24, gmXtext=xtext, gmYtext='*digits', &
              	gmXjust=GCENTRE, gmYjust=GCENTRE, gmVpos=GTOP, gmExpand=GOFF)
   call gmSetGuiGridMode(GOFF)
   call gmEnqCellAttribs(ValArray6(iplot,jset), 1, 1, arrayattribs)
   arrayattribs%width=76
   arrayattribs%height=22
   arrayattribs%justify=GRIGHT
   arrayattribs%format=gscientific

   
   do i=1,ncols
      do j=1,nrows
		call gmSetCellAttribs(ValArray6(iplot,jset), i,j , arrayattribs)
		x=xdata(iplot,jset,i,j)
		call gmSetCellSetting(ValArray6(iplot,jset), i,j ,gmValue= x )
      enddo
   enddo
   
   
   !call gmSetValueArray(valarray6,1,1,ncols,nrows,xdata)

call gmSetGuiGridMode(GON)
call gmdrawwindow(idatA_list)
end