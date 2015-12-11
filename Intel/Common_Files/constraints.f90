 subroutine constraints(imodel,main,itwin2,n,m,text_box2,iconform,icall)

	use menu_f90
	integer :: iconForm
	integer :: iwcell(10)
	character*30 text_box2(100,10),title
	character*100 xtext
	character cnum*11
	type(GARRAYCELL) arrayattribs
	call gmSetGuiGridMode(GOFF)
	ix=100
	ncols=m
	nrows=n
	xtext='Rate 1='//char(124)//'Total'//char(124)//'- Rate 2'//char(124)//'Obs'
	title='Additive Constraints'
	do i=1,ncols
			iwcell(i)=120
			ix=ix+iwcell(i)
	enddo
	

	itwin2= gmCreateComplexDialogueBox(Main,200,150, ix, 14*24, GALL, title, &
              	 gmIconFormat=GDLLICON,gmvpos=gtop,gmIconFile='Gee')

    ipanel2 = gmCreatePanel(itwin2, 0, 24, ix, 12*24, &
              	gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
	iconform = gmCreateTextArray(iPanel2, 0, 0, ix, 12*24, ncols, nrows, &
              	gmAxisW=50, gmAxisH=25, gmXtext=xtext, gmYtext='*digits', &
              	gmXjust=Gleft, gmYjust=GCENTRE, gmScrollMode=GBOTHBARS, gmExpand=GOFF,gmcallback=icall1)


	call gmenqcellattribs(iconform,1,1,arrayattribs)
	do i=1,ncols
		arrayattribs%width=iwcell(i)
		do j=1,nrows
			call gmSetCellAttribs(iconform, i, j, arrayattribs)
			call gmSetCellSetting(iconform, i,j ,gmString=text_box2(j,i))
		enddo
	enddo
	ivb=gmCreatePushButton(itwin2,0,0, ix, 24,'OK',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=icall)
    call gmSetGuiGridMode(GOn)
 call gmdrawwindow(itwin2)
	end
