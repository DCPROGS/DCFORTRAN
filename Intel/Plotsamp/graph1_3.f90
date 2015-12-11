subroutine graph1(iplot,jplot,iform,ix,iy,graph1_1,GraphMainPanel1_1,graphics1_1,&
			wxmin,wxmax,wymin,wymax,ipos,gfile,plot,ixposv,iyposv,npage)

use gino_f90
use menu_f90
logical plot,printer,landscape,poplot
integer :: iform,listfont(10),ibut(10),igtog(10)
integer :: Graph1_1(100)
integer :: GraphMainPanel1_1(100)
integer :: Graphics1_1(100)
character*11 cnumb
character*60 title_graph, gfile

logical quarter
type (GLIMIT) :: Graphics_frame
common/pixpos/ixgrid,iygrid,ixpix,iypix,wid,landscape
common/plotopen/icombo1,poplot,&
itext1,itext2,itext_1,itext_2,itext_3,itext_4,ivalp1,ivalp2,ivalp3,ivalp4,&
iradiop1,iradiop2,iradiop3,iradiop4

call gEnqHardFontList(listfont, 10, nfontmax)
 call gDefineRGB(227, 1.000000, 0.458824, 0.458824)
 call gDefineRGB(228, 1.000000, 1.000000, 0.517647)
iplot0=iplot
 call gDefineRGB(122,0.80,0.89,0.87)
	 call gDefineRGB(123,0.73,0.82,0.79)
	 call gDefineRGB(124,0.66,0.97,0.87)
	 call gDefineRGB(125,0.89,0.97,0.94)
	 call gDefineRGB(121,0.87,0.97,0.94)
	 call gDefineRGB(20,0.5,0.,0.25)	!burgundy
	 call gDefineRGB(12,1.,0.,0.)	!red
if(plot) then
	call gDefineRGB(0,1.,1.,1.)
	CALL INTCONV(jplot,cnumb)
	nl=len_trim(gfile)
	title_graph=gfile(1:nl)
	 if(ipos.le.1) then
		CALL PAPENQ(XPAP,YPAP,IPAPTY)
		CALL VPTSWI(2)
		
     endif
	 xbegg=0.1*xpap
	 ybegg=0.1*ypap
	 xendg=0.92*xpap
	 yendg=0.92*ypap
	 xbegg=0.0
	 ybegg=0.0
	 xendg=xpap
	 yendg=ypap
	 select case(ipos)
		case(1)
			xbegg=0.05*xpap
			ybegg=0.52*ypap
			xendg=0.48*xpap
			yendg=0.95*ypap
		case(2)
			xbegg=0.52*xpap
			ybegg=0.52*ypap
			xendg=0.95*xpap
			yendg=0.95*ypap
		case(3)
			xbegg=0.05*xpap
			ybegg=0.05*ypap
			xendg=0.48*xpap
			yendg=0.48*ypap
		case(4)
			xbegg=0.52*xpap
			ybegg=0.05*ypap
			xendg=0.95*xpap
			yendg=0.48*ypap
	 end select
	 if(landscape) then
	
	 CALL SETVP2(wxmin,wxmax,wymin,wymax,xbegg,xendg,ybegg,yendg)
	 else	  
	CALL SETVP2(wxmin,wxmax,wymin,wymax,xbegg,xendg,ybegg,yendg)
	endif
	call gSetLineWidth(0.0)
	call gEnqSelectedPen(icol,wid0,itype) ! This is the actual thickness
    call gSetLineWidth(wid0*5.0*100.0)

	call gsethardchars()
	call gsetlinewidthmode(ghardware)
	call gSetLineEnd(GROUND	)
	call gseterrormode(gerroron) 
!	call write_string(gfile,wxmax/2,1,0.,0,0,5,1,1,1)
!	if(ipos.le.1) 
!	endif
else
    ipx=0
    if(ixposv.eq.-1.or.ixposv.eq.-2) then
		ixgraph=36 !24 !28
		iygraph=27 !18 !23
		if(ixposv.eq.-2) iygraph=26
	else
		ixgraph=30 !24 !28
		iygraph=22 !18 !23
	endif
	ixbegg=0
	iybegg=1
	ixp=ix
	iyp=iy
   CALL INTCONV(jplot,cnumb)
   	if(iyposv.eq.-1) then
   
		IPLOT0=IPLOT
		goto 22
	endif
	if(ixposv.eq.-2) iplot0=iplot
   if(ipos.le.1) then
   
	IPLOT0=IPLOT
	if(ipos.ne.-1) then
	nl=len_trim(gfile)
	title_graph=gfile(1:nl)
 	call gmSetGuiGridMode(GOFF)
	! Set up complex dialogue box Graph child of Main
	 if(ixposv.eq.-2.or.ixposv.eq.-3) then
	 Graph1_1(iplot) = gmCreateComplexDialogueBox(iform, ix, iy, ixgraph*ixpix, (iygraph+1)*iypix, Gtitle,&
    title_graph,gmvpos=gtop, gmhpos=gleft,&
    gmIconFormat=GDLLICON,gmIconFile='MBIG1085',gmCallBack=iplot+7000,gmSelect=iplot+7200) !,gmSelect=iplot+7200
	 else
	 Graph1_1(iplot) = gmCreateComplexDialogueBox(iform, ix, iy, ixgraph*ixpix, (iygraph+1)*iypix, GALL,&
    title_graph,gmvpos=gtop, gmhpos=gleft,&
    gmIconFormat=GDLLICON,gmIconFile='MBIG1085',gmCallBack=iplot+7000,gmSelect=iplot+7200)
	 endif
	
	  
	! Create main panel for graph
	GraphMainPanel1_1(iplot)=gmCreatePanel(Graph1_1(iplot), 0, 0, (ixgraph)*ixpix,(iygraph+1)*iypix, &
              	gmHpos=GCENTRE, gmVpos=GTOP, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=122, gmFillCol=0, gmFillBorder=0)
			!	gmMove=iplot+7100,gmSelect=iplot+7200)
   	call gmSetGuiGridMode(GON) 
	
	if(ixposv.eq.-1) then



		icombo1 = gmCreateComboBox(GraphMainPanel1_1(iplot), 5, 0, 3, 10, GNONE, 1, &
              	gmSort=GUNSORTED, gmHpos=Gleft,  gmVpos=GTOP, gmExpand=GOFF, &
				gmcallback=122)

		! Create entries for combo box Combo1_5
		call gmSetListEntry(iCombo1, GADD, gmString='Page')
		do i=1,npage
			call intconv(i,cnumb)
			call gmSetListEntry(iCombo1, GADD, gmstring=cnumb)
		enddo
		call gmSetListEntry(iCombo1,GSELECT,gmEntry=1)
			Icon0g = gmCreateIcon(GraphMainPanel1_1(iplot),10,0,1,1 ,'Msml1036',  &
    			GDLLICON,0,&
           gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'View text file',gmcallback = 501)
		Icono = gmCreateicon(GraphMainPanel1_1(iplot),9,0,1,1, 'p_open.ico', &
              	Gicofile,0,&
           gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Choose segment',&
                 gmcallback = 141)
		Icon00 = gmCreateIcon(GraphMainPanel1_1(iplot),8,0,1,1 ,'npoint.ico',  &
    			Gicofile,0,&
           gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Keep any nth point',gmcallback = 126)
	
		Icon0g = gmCreateIcon(GraphMainPanel1_1(iplot),7,0,1,1 ,'vsep.ico',  &
    			GDLLICON,0,&
           gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Vertical separation',gmcallback = 125)
		Icon0g = gmCreateIcon(GraphMainPanel1_1(iplot),6,0,1,1 ,'dframe.ico',  &
    			GDLLICON,0,&
           gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Draw frame',gmcallback = 121)
  
		Icon1g = gmCreateIcon(GraphMainPanel1_1(iplot),4,0,1,1 ,'Msml1014',  &
			GDLLICON,0,&
            gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Next page',gmcallback =123 )!17)

		Icon2g = gmCreateIcon(GraphMainPanel1_1(iplot),5,0, 1,1,'Msml1015', &
			GDLLICON,0,&
	        gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
		
			gmHelp = 'Previous page',gmcallback = 124) !18)
		Icon1g = gmCreateIcon(GraphMainPanel1_1(iplot),3,0,1,1 ,'MBIG1055',  &
			GDLLICON,0,&
            gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Print',gmcallback =128 )!17)

		Icon2g = gmCreateIcon(GraphMainPanel1_1(iplot),2,0, 1,1,'MBIG1033', &
			GDLLICON,0,&
	        gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
		
			gmHelp = 'BMP/WMF',gmcallback = 129) !18)
		Icon = gmCreateicon(GraphMainPanel1_1(iplot),0,0,1,1,'MSML1064',  &
			GDLLICON,0,gmHpos=Gright, gmVpos=GTOP,&
              gmHFlag = GBUBBLEANDSTATUSBAR,gmHelp = 'Help',gmcallback =401)
    else if(ixposv.eq.-2) then
	
	
	itextt= gmCreateTextEntry(Graph1_1(iplot), 0, 1, 35, 1,'To START:select line with left mouse button;&
	  MOVE with mouse or keyboard arrows;END selection pressing',100, Gdisplay, &
              	gmType=GSTANDARD, gmFont=2, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=20, &
              	gmVpos=gtop, gmscrollable=gon, gmExpand=GOFF)
		itextt= gmCreateTextEntry(Graph1_1(iplot), 0, 2, 36, 1,'left button again or RETURN key;for ZOOM press &
	  right button.To FINISH PRESS "A" TO ACCEPT or "R" TO REJECT ',100, Gdisplay, &
              	gmType=GSTANDARD, gmFont=2, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=20, &
              	gmVpos=gtop, gmscrollable=gon, gmExpand=GOFF)

	
	 
	  Icon0g = gmCreateIcon(GraphMainPanel1_1(iplot),2,0,1,1 ,'Msml1053',  &
    			GDLLICON,0,&
           gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Reject',gmcallback = 191)
		Icon0g = gmCreateIcon(GraphMainPanel1_1(iplot),1,0,1,1 ,'Msml1021',  &
    			GDLLICON,0,&
           gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Accept',gmcallback = 192)
			Icon = gmCreateicon(GraphMainPanel1_1(iplot),0,0,1,1,'MSML1064',  &
			GDLLICON,0,gmHpos=Gright, gmVpos=GTOP,&
              gmHFlag = GBUBBLEANDSTATUSBAR,gmHelp = 'Help',gmcallback =402)
  else if(ixposv.eq.-3) then
		Icon0g = gmCreateIcon(GraphMainPanel1_1(iplot),4,0,2,1 ,'Msml1053',  &
    			GDLLICON,0,&
           gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Reject',gmcallback = 181)
		Icon0g = gmCreateIcon(GraphMainPanel1_1(iplot),2,0,2,1 ,'Msml1021',  &
    			GDLLICON,0,&
           gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Continue',gmcallback = 182)
			  
   Icon = gmCreateicon(GraphMainPanel1_1(iplot),0,0,2,1,'MSML1064',  &
   GDLLICON,0,gmHpos=Gright, gmVpos=GTOP,&
              gmHFlag = GBUBBLEANDSTATUSBAR,gmHelp = 'Help',gmcallback =403) 
  else
		Icon0g = gmCreateIcon(GraphMainPanel1_1(iplot),0,0,1,1 ,'MBIG1085',  &
    			GDLLICON,0,&
           gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Select',gmcallback = 7200+iplot)
		Icon1g = gmCreateIcon(GraphMainPanel1_1(iplot),3,0,1,1 ,'MBIG1055',  &
			GDLLICON,0,&
            gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Print',gmcallback =5150+iplot )!17)

		Icon2g = gmCreateIcon(GraphMainPanel1_1(iplot),2,0, 1,1,'MBIG1033', &
			GDLLICON,0,&
	        gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
		
			gmHelp = 'BMP/WMF',gmcallback = 5200+iplot) !18)
   
		Icon3g = gmCreateIcon(GraphMainPanel1_1(iplot),1,0, 1,1,'MBIG1005', &
			GDLLICON,0,&
	        gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
		
			gmHelp = 'Save as plotq',gmcallback = 5100+iplot) !15)
  endif

   
   if(ixposv.eq.-1) then
		title_graph=' Page :'
		ilte=3
   else
		ilte=8
		title_graph=' Record :'//cnumb(1:3)
   endif
   endif
   else
    ng=len_trim(title_graph)
   	title_graph=title_graph(1:ng)//';'//cnumb(1:3)
   endif
22 continue
! Define Graphics Frame limits
	dx=abs(wxmax-wxmin)
	dy=abs(wymax-wymin)
   if(ixposv.eq.-2) then
	wxmin = wxmin+0.1*dx
	wxmax = wxmax-0.1*dx
   !Graphics_frame%ymin = wymin-0.1*dy
   !Graphics_frame%ymax = wymax+0.1*dy
  endif
   Graphics_frame%xmin = wxmin
   Graphics_frame%xmax = wxmax
   Graphics_frame%ymin = wymin
   Graphics_frame%ymax = wymax
   
   select case(ipos)
	case(1)
		ixbegg=0
		iybegg=1
		ixgraph=ixgraph/2
		iygraph=iygraph/2
	case(2)
		ixbegg=ixgraph/2
		iybegg=1
		ixgraph=ixgraph/2
		iygraph=iygraph/2
	case(3)
		ixbegg=0
		iybegg=iygraph/2
		ixgraph=ixgraph/2
		iygraph=iygraph/2
	case(4)
		ixbegg=ixgraph/2
		iybegg=iygraph/2
		ixgraph=ixgraph/2
		iygraph=iygraph/2
		
   end select

   call gDefineRGB(0,1.,1.,0.94)
   if(ixposv.eq.-2) then
	!ixgraph=ixgraph-5
	iybegg=3
   endif
   call gmSetGuiGridMode(GOff) 
if(ixposv.eq.-2) then
   iplot0=iplot
   Graphics1_1(iplot0) = gmCreateGraphicsFrame(GraphMainPanel1_1(iplot0), ixbegg*ixpix, iybegg*iypix, &
   ixgraph*ixpix, iygraph*iypix, Graphics_frame, &
              	gmhpos=gleft,gmVpos=Gtop, gmExpand=GOFF,gmMove=7100+iplot0,gmSelect=iplot0+7200 )!,gmSelect=7200+iplot
  else
Graphics1_1(iplot) = gmCreateGraphicsFrame(GraphMainPanel1_1(iplot0), ixbegg*ixpix, iybegg*iypix, &
   ixgraph*ixpix, iygraph*iypix, Graphics_frame, &
              	gmhpos=gleft,gmVpos=Gtop, gmExpand=GOFF,gmMove=7100+iplot,gmSelect=7200+iplot )
  endif
  call gmSetGuiGridMode(GON) 
  
  if(ixposv.ne.-2) then
   itext= gmCreateTextEntry(graph1_1(iplot0), 1, 0, ilte, 1,title_graph, 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=gtop, gmExpand=GOFF)
  endif
	if(ixposv.eq.-1) then
   itext= gmCreateTextEntry(graph1_1(iplot0), 9, 0, 3, 1,'of '//cnumb(1:3),10, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=gtop, gmExpand=GOFF)
itext1= gmCreateTextEntry(graph1_1(iplot0), 13, 0, 4, 1,'',10, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
	gmVpos=gtop, gmExpand=GOFF)
 itext2= gmCreateTextEntry(graph1_1(iplot0), 18, 0, 4, 1,'',10, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=gtop, gmExpand=GOFF)
  else if(ixposv.eq.-2) then
  itext_1= gmCreateTextEntry(graph1_1(iplot), 1, 0, 3, 1,'',10, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=gtop, gmExpand=GOFF)
 itext_2= gmCreateTextEntry(graph1_1(iplot), 4, 0, 3, 1,'',10, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=gtop, gmExpand=GOFF)
					itextt= gmCreateTextEntry(graph1_1(iplot), 8, 0, 2, 1,'Y open=',10, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=20, &
              gmVpos=gtop, gmExpand=GOFF)
		ivalp1 = gmCreateValueEntry(graph1_1(iplot), 10, 0, 3, 1,0, 20,6, Gedit, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=20, &
              gmVpos=GTOP, gmExpand=GOFF)
        itextt= gmCreateTextEntry(graph1_1(iplot), 14, 0, 2, 1,'Y shut=',10, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=20, &
              gmVpos=gtop, gmExpand=GOFF)
		ivalp2 = gmCreateValueEntry(graph1_1(iplot), 16, 0, 3, 1,0, 20,6, Gedit, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=20, &
              gmVpos=GTOP, gmExpand=GOFF)
        itextt= gmCreateTextEntry(graph1_1(iplot), 20, 0, 2, 1,'X beg=',10, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=20, &
              	gmVpos=gtop, gmExpand=GOFF)	
		ivalp3 = gmCreateValueEntry(graph1_1(iplot), 22, 0, 3, 1,0, 20,6, Gedit, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=20, &
              	gmVpos=GTOP, gmExpand=GOFF)
		itextt= gmCreateTextEntry(graph1_1(iplot), 26, 0, 2, 1,'X end=',10, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=20, &
              	gmHpos=Gleft,gmVpos=gtop, gmExpand=GOFF)
		ivalp4 = gmCreateValueEntry(graph1_1(iplot), 28, 0, 3, 1,0, 20,6, Gedit, &
              	gmType=GSTANDARD, gmhpos=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=20, &
              	gmVpos=GTOP, gmExpand=GOFF)
  else if(ixposv.eq.-3) then

	itext_3= gmCreateTextEntry(graph1_1(iplot), 0, 0, 4, 1,'',10, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
					gmVpos=gtop, gmExpand=GOFF)
	itext_4= gmCreateTextEntry(graph1_1(iplot), 4, 0, 4, 1,'',10, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=gtop, gmExpand=GOFF)
				
  endif	                                                     
   call gmClearGraphicsFrame(Graphics1_1(iplot))
	
   if(ipos.le.1) call gmdrawwindow(graph1_1(iplot))

call gsetmixedchars()
	call gsetlinewidthmode(ghardware)
	call gSetLineEnd(GROUND	)
	call gseterrormode(gerroron)
endif  
end