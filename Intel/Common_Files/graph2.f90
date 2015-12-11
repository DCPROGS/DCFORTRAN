subroutine graph2(ix,iy,jgraph,imodel,main,graph1_2,GraphMainPanel1_2,graphics1_2,&
			nxmodel,nymodel,mtype,title,mod_create,initialpage,ini_MainPanel,&
			ini_panel1,ini_panel1_1,ini_Text1_1)
! routine to draw the mechanism
! mod_create=-1 modified model
! mod_create=0 New model or ini_page
! mod_create=1 old model
! mod_create=5 enlarge model

use menu_f90
use gino_f90
integer :: Main
integer :: Graph1_2(100)
integer :: GraphMainPanel1_2(100)
integer :: Graphics1_2(100)
character cnum*11,title*80,mectit*80
type (GLIMIT) :: Graphics_frame
type (gpoint) :: pts(4)
logical plot
common/switch/iswicon,igraphText(100),ibutton1,icongmod


! Set up complex dialogue box Graph child of Main
call intconv(imodel,cnum)
call gDefineRGB(101, 0.831372, 0.901960, 0.901960)
call gDefineRGB(102, 0.772549, 0.850980, 0.850980)
call gDefineRGB(129,1.,0.,0.)	!red
call gDefineRGB(38,1.,0.5,0.25)	!orange
call gDefineRGB(37,1.,0.65,0.40)	!orange
	nhw=nymodel+2

	!if(nymodel.gt.32) nhw=32
	
	if(ix.ne.-100) then
	if(mod_create.eq.1) then
		mectit='Mechanism:'//cnum(1:3)
	else if(mod_create.eq.-1) then
	    mectit='For rates :you can copy & paste from original rates table'
	   
	else if(mod_create.eq.0) then
		mectit='New mechanism design'
	else if(mod_create.eq.5) then
		mectit='Enlarged:You can modify states names'
	endif

   Graph1_2(jgraph) = gmCreateComplexDialogueBox(Main, ix, iy, nxmodel+1,nhw , GALL,& 
   mectit,gmhpos=gleft,gmvpos=gtop,&
   gmIconFormat=GDLLICON,gmIconFile='MBIG1034',gmSelect=2400+jgraph)
   igraphText(jgraph) = gmCreateTextEntry(Graph1_2(jgraph), 2, 0, nxmodel-2, 1,title, 255, gdisplay, gmBack1Col=0, gmBack2Col=12, &
   gmTextCol=1 , gmVpos=GTOP)

   GraphMainPanel1_2(jgraph)=gmCreatePanel(Graph1_2(jgraph), 1, 1, nxmodel, nymodel, &
              	gmHpos=Gleft, gmVpos=Gtop, gmExpand=GOff, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0,gmMove=2300+jgraph,&
				 gmSelect=2400+jgraph)
	if(iswicon.ne.-100.and.mod_create.ne.5) then
		if(mod_create.eq.1) then
		iButton1 = gmCreatePushButton(Graph1_2(jgraph),1,0 , 3, 1, 'Modify',gmType=GUSERDEFINED, &
		gmhpos=gleft,gmVpos=Gbottom,gmOffcol=37, gmTextCol=1,&
		gmcallback=1600+jgraph)
		iButton1 = gmCreatePushButton(Graph1_2(jgraph),4,0 ,3, 1, 'Continue',gmType=GUSERDEFINED, &
		gmhpos=gleft,gmVpos=Gbottom,gmOffcol=38, gmTextCol=1,&
		gmcallback=2051)
		if(iswicon.eq.-1) then
	    iButton1 = gmCreatePushButton(Graph1_2(jgraph),7,0 ,3, 1, 'Cancel',gmType=GUSERDEFINED, &
		gmhpos=gleft,gmVpos=Gbottom,gmOffcol=37, gmTextCol=1,&
		gmcallback=2055)
		endif
		else
		iButton1 = gmCreatePushButton(Graph1_2(jgraph),1,0 , nxmodel-1, 1, 'Continue',gmType=GUSERDEFINED, &
		gmhpos=gleft,gmVpos=Gbottom,gmOffcol=38, gmTextCol=1,&
		gmcallback=2051)
 		endif
		
	endif
	endif

! Define Graphics Frame limits
	Graphics_frame%xmin = 0.000000
	Graphics_frame%xmax = float(nxmodel)
	Graphics_frame%ymin = 0.000000
	Graphics_frame%ymax = float(nymodel)
	if(ix.eq.-100) then
		Graphics1_2(jgraph) = gmCreateGraphicsFrame(ini_panel1_1, 5, 5, nxmodel*16, &
				 nymodel*16, Graphics_frame,&
              	gmVpos=GTOP, gmExpand=GOff)

	else
		Graphics1_2(jgraph) = gmCreateGraphicsFrame(GraphMainPanel1_2(jgraph), 0, 0, nxmodel, &
				 nymodel, Graphics_frame,&
              	gmVpos=GTOP, gmExpand=GOff,gmMove=2300+jgraph , gmSelect=2400+jgraph)

	endif

	if(ix.ne.-100.and.mod_create.ne.5) then
	IF(MOD_CREATE.nE.1) then
    Icon5g = gmCreateIcon(Graph1_2(jgraph),0,4,1,1 ,'Ostate.ICO',  &
			GDLLICON,0,&
            gmHpos=Gleft, gmVpos=Gtop,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Add an open state',gmcallback =1161)
	Icon6g = gmCreateIcon(Graph1_2(jgraph),0,5,1,1 ,'cstate.ICO',  &
			GICOfile,0,&
            gmHpos=Gleft, gmVpos=Gtop,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Add a close state',gmcallback =1162)
	Icon7g = gmCreateIcon(Graph1_2(jgraph),0,6,1,1 ,'MBIG1054',  &
			GDLLICON,0,&
            gmHpos=gleft, gmVpos=Gtop,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Create link between states',gmcallback =1163)
	Icon8g = gmCreateIcon(Graph1_2(jgraph),0,7,1,1 ,'MBIG1022',  &
			GDLLICON,0,&
            gmHpos=gleft, gmVpos=Gtop,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Move state/link',gmcallback =1164)
	Icon9g = gmCreateIcon(Graph1_2(jgraph),0,8,1,1 ,'MBIG1004',  &
			GDLLICON,0,&
            gmHpos=gleft, gmVpos=Gtop,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Delete state/link',gmcallback =1165)
	endif
	if(iswicon.ne.-100) then
	if(mod_create.eq.1) then
		if(ini_text1_1.ne.-100) then
	!		Icongmod = gmCreateIcon(Graph1_2(jgraph),0,4,1,1 ,'MBIG1036',  &
    !			Gdllicon,0,&
	!		gmHpos=gleft, gmVpos=Gtop,gmHFlag = GBUBBLEANDSTATUSBAR,&
	!		gmHelp = 'Modify model: add new states,etc',gmcallback = 1600 +jgraph )!7000+jgraph)
    
		endif
    else
		Icon0g = gmCreateIcon(Graph1_2(jgraph),0,9,1,1 ,'states.ico',  &
    			GICOfile,0,&
           gmHpos=gleft, gmVpos=Gtop,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Edit states',gmcallback =1300+jgraph) ! 2100+imodel)
		Icon01g = gmCreateIcon(Graph1_2(jgraph),0,10,1,1 ,'rates.ico',  &
    			GICOfile,0,&
           gmHpos=gleft, gmVpos=Gtop,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Edit rate',gmcallback = 1400 +jgraph) !2600+imodel)
   endif
   Icon1g = gmCreateIcon(Graph1_2(jgraph),0,3,1,1 ,'MBIG1055',  &
			GDLLICON,0,&
            gmHpos=gleft, gmVpos=Gtop,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Print',gmcallback =1850 +jgraph) ! 4000+imodel)

   Icon2g = gmCreateIcon(Graph1_2(jgraph),0,2, 1,1,'MBIG1033', &
			GDLLICON,0,&
	        gmHpos=gleft, gmVpos=Gtop,gmHFlag = GBUBBLEANDSTATUSBAR,&
		
			gmHelp = 'BMP/WMF',gmcallback =1900 +jgraph) !8000+imodel)
	Icon3g = gmCreateIcon(Graph1_2(jgraph),0,1, 1,1,'MBIG1005', &
			GDLLICON,0,&
	        gmHpos=gleft, gmVpos=Gtop,gmHFlag = GBUBBLEANDSTATUSBAR,&
		
			gmHelp = 'Save on disk',gmcallback =1800+jgraph)
			 ! 4500+imodel)
	Icon33g = gmCreateIcon(Graph1_2(jgraph),0,0, 1,1,'MBIG1007', &
			GDLLICON,0,&
	        gmHpos=gleft, gmVpos=Gtop,gmHFlag = GBUBBLEANDSTATUSBAR,&
		
			gmHelp = 'Exit',gmcallback =2052) 
	endif

	endif
	if(mod_create.eq.5) then
		Icon0g = gmCreateIcon(Graph1_2(jgraph),0,4,1,1 ,'states.ico',  &
    			GICOfile,0,&
           gmHpos=gleft, gmVpos=Gtop,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Edit states',gmcallback =1300+jgraph) ! 2100+imodel)
		Icon1g = gmCreateIcon(Graph1_2(jgraph),0,3,1,1 ,'MBIG1055',  &
			GDLLICON,0,&
            gmHpos=gleft, gmVpos=Gtop,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Print',gmcallback =1850 +jgraph) ! 4000+imodel)

		Icon2g = gmCreateIcon(Graph1_2(jgraph),0,2, 1,1,'MBIG1033', &
			GDLLICON,0,&
	        gmHpos=gleft, gmVpos=Gtop,gmHFlag = GBUBBLEANDSTATUSBAR,&
		
			gmHelp = 'BMP/WMF',gmcallback =1900 +jgraph) !8000+imodel)
		Icon3g = gmCreateIcon(Graph1_2(jgraph),0,1, 1,1,'MBIG1005', &
			GDLLICON,0,&
	        gmHpos=gleft, gmVpos=Gtop,gmHFlag = GBUBBLEANDSTATUSBAR,&
		
			gmHelp = 'Save on disk',gmcallback =1800+jgraph) ! 4500+imodel)
			iButton1 = gmCreatePushButton(Graph1_2(jgraph),1,0 , nxmodel-1, 1, 'Close and continue',gmType=GUSERDEFINED, &
			gmhpos=gleft,gmVpos=Gbottom,gmOffcol=38, gmTextCol=1,&
			gmcallback=2060)
	endif
! Define Graphics Frame limits
   Graphics_frame%xmin = 0.000000
   Graphics_frame%xmax = float(nxmodel)
   Graphics_frame%ymin = 0.000000
   Graphics_frame%ymax = float(nymodel)

   call gmClearGraphicsFrame(Graphics1_2(jgraph))

   if(mtype.eq.1) then
     
		pts(1)%x=0.0
		pts(1)%y=0.0
		pts(2)%x=float(nxmodel)
		pts(2)%y=0.0
		pts(3)%x=float(nxmodel)
		pts(3)%y=float(nymodel)
		pts(4)%x=0.0
		pts(4)%y=float(nymodel)
		call LinCol(11)
		if(mod_create.eq.1) then
			call gFillPolygonTo2D(gsolid,1,GAREA,4,pts)
		else
			call gFillPolygonTo2D(gsolid,2,GAREA,4,pts)
		endif
	
	else

		do j=1,nymodel,2
		do i=1,nxmodel,2
			pts(1)%x=float(i-1)
			pts(1)%y=float(j-1)
			pts(2)%x=float(i)
			pts(2)%y=float(j-1)
			pts(3)%x=float(i)
			pts(3)%y=float(j)
			pts(4)%x=float(i-1)
			pts(4)%y=float(j)
			call LinCol(11)
			call gMoveTo2D(pts(1)%x,pts(1)%y)
			call gFillPolygonTo2D(gsolid,11,GAREA,4,pts)
		enddo 
		enddo

		do j=2,nymodel,2
		do i=2,nxmodel,2
			pts(1)%x=float(i-1)
			pts(1)%y=float(j-1)
			pts(2)%x=float(i)
			pts(2)%y=float(j-1)
			pts(3)%x=float(i)
			pts(3)%y=float(j)
			pts(4)%x=float(i-1)
			pts(4)%y=float(j)
			call LinCol(11)
			call gMoveTo2D(pts(1)%x,pts(1)%y)
			call gFillPolygonTo2D(gsolid,11,GAREA,4,pts)
		enddo
		enddo

	endif
	
	if(ix.ne.-100) then 
		call gmdrawwindow(Graph1_2(jgraph))
	!	if(mod_create.ne.5.and.mod_create.ne.0)	call gmsetwidgetstatus(icongmod,gunselectable)

	endif
	call gFlushGraphics()
	
end
