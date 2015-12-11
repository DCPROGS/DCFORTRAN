subroutine graph1(iplot,jplot,main,ix,iy,graph1_1,GraphMainPanel1_1,graphics1_1,&
			wxmin,wxmax,wymin,wymax,ipos,gfile,plot,ixposv,iyposv)

use gino_f90
use menu_f90
logical plot,printer,landscape,poplot
integer :: Main,listfont(10),ibut(10)
integer :: Graph1_1(100)
integer :: GraphMainPanel1_1(100)
integer :: Graphics1_1(100)
character :: cnumb*11, title_graph*60, gfile*60

logical quarter
type (GLIMIT) :: Graphics_frame
common/pixpos/ixgrid,iygrid,ixpix,iypix,wid,landscape,icombo1,poplot,itext1,itext2,&
itext_1,itext_2,itext_3,itext_4,ibut

call gEnqHardFontList(listfont, 10, nfontmax)
call gDefineRGB(227, 1.000000, 0.458824, 0.458824)
call gDefineRGB(228, 1.000000, 1.000000, 0.517647)


call gDefineRGB(122,0.80,0.89,0.87)
call gDefineRGB(123,0.73,0.82,0.79)
call gDefineRGB(124,0.66,0.97,0.87)
call gDefineRGB(125,0.89,0.97,0.94)
call gDefineRGB(121,0.87,0.97,0.94)
call gDefineRGB(38,1.,0.5,0.25)	!orange

iplot0=iplot
if(plot) then
    call gDefineRGB(0,1.,1.,1.)
	CALL INTCONV(jplot,cnumb)
	nl=len_trim(gfile)
	title_graph=gfile(1:nl)
!	if(ipos.le.1) then
		CALL PAPENQ(XPAP,YPAP,IPAPTY)
		CALL VPTSWI(2)
		
!    endif
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

	!call gsethardchars()
	!call gsetlinewidthmode(ghardware)
	call gSetLineEnd(GROUND	)
	call gseterrormode(gerroron) 

else
    if(ixposv.eq.-1.or.ixposv.eq.-2) then
		ixgraph=36 !24 !28
		iygraph=27 !18 !23
	else
		ixgraph=30 !24 !28
		iygraph=22 !18 !23
		!if(ipos.gt.0) iygraph=23
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
   if(ipos.le.1) then
   
	IPLOT0=IPLOT
   if(ipos.ne.-1) then
   nl=len_trim(gfile)
   title_graph=gfile(1:nl)
 	call gmSetGuiGridMode(GOFF)
! Set up complex dialogue box Graph child of Main
   Graph1_1(iplot) = gmCreateComplexDialogueBox(Main, ix, iy, ixgraph*ixpix, (iygraph+1)*iypix, GALL,&
    title_graph,gmvpos=gtop, gmhpos=gleft,&
    gmIconFormat=GDLLICON,gmIconFile='MBIG1085',gmCallBack=iplot+7000,gmSelect=iplot+7200)
	  
! Create main panel for graph
   GraphMainPanel1_1(iplot)=gmCreatePanel(Graph1_1(iplot), 0, iypix, ixgraph*ixpix,(iygraph)*iypix, &
              	gmHpos=Gleft, gmVpos=GTOP, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0,gmCallBack=iplot+7000,gmSelect=iplot+7200)
			!	gmMove=iplot+7100,gmSelect=iplot+7200)
   	call gmSetGuiGridMode(GON) 
	if(ixposv.eq.-1) then



        icombo1 = gmCreateComboBox(GraphMainPanel1_1(iplot), 5, 0, 3, 10, GNONE, 1, &
              	gmSort=GUNSORTED, gmHpos=Gleft,  gmVpos=GTOP, gmExpand=GOFF, gmcallback=80)

        ! Create entries for combo box Combo1_5
        call gmSetListEntry(iCombo1, GADD, gmString='Page')
        do i=1,iyposv
            call intconv(i,cnumb)
            call gmSetListEntry(iCombo1, GADD, gmstring=cnumb)
	    enddo
	call gmSetListEntry(iCombo1,GSELECT,gmEntry=1)
    if(poplot) Icono = gmCreateicon(GraphMainPanel1_1(iplot),9,0,1,1, 'p_open.ico', &
              	Gicofile,0,&
           gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
                 gmHelp = 'Reset shut & open levels',&
                 gmcallback = 99)
	Icon00 = gmCreateIcon(GraphMainPanel1_1(iplot),8,0,1,1 ,'npoint.ico',  &
    			Gicofile,0,&
           gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Keep any nth point',gmcallback = 110)
	
	Icon0g = gmCreateIcon(GraphMainPanel1_1(iplot),7,0,1,1 ,'vsep.ico',  &
    			GDLLICON,0,&
           gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Vertical separation',gmcallback = 81)
	Icon0g = gmCreateIcon(GraphMainPanel1_1(iplot),6,0,1,1 ,'dframe.ico',  &
    			GDLLICON,0,&
           gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Draw frame',gmcallback = 82)
  
   Icon1g = gmCreateIcon(GraphMainPanel1_1(iplot),4,0,1,1 ,'Msml1014',  &
			GDLLICON,0,&
            gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Next page',gmcallback =83 )!17)

   Icon2g = gmCreateIcon(GraphMainPanel1_1(iplot),5,0, 1,1,'Msml1015', &
			GDLLICON,0,&
	        gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
		
			gmHelp = 'Previous page',gmcallback = 84) !18)
   Icon1g = gmCreateIcon(GraphMainPanel1_1(iplot),3,0,1,1 ,'MBIG1055',  &
			GDLLICON,0,&
            gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Print',gmcallback =85 )!17)

   Icon2g = gmCreateIcon(GraphMainPanel1_1(iplot),2,0, 1,1,'MBIG1033', &
			GDLLICON,0,&
	        gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
		
			gmHelp = 'BMP/WMF',gmcallback = 86) !18)
   
 
  else if(ixposv.eq.-2) then
  
	Ibut(3) = gmCreatepushbutton(GraphMainPanel1_1(iplot),16,0,4,1 ,'adjust xbeg',  &
    		gmtype=guserdefined,gmoncol=123,gmoffcol=121,&
           gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'zoom',gmcallback = 381)
	Ibut(4) = gmCreatepushbutton(GraphMainPanel1_1(iplot),12,0,4,1 ,'adjust xend',  &
    		gmtype=guserdefined,gmoncol=123,gmoffcol=121,&
           gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'zoom',gmcallback = 382)
	Ibut(1) = gmCreatepushbutton(GraphMainPanel1_1(iplot),8,0,4,1 ,'adjust y open',  &
    		gmtype=guserdefined,gmoncol=123,gmoffcol=121,&
           gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Adjust Yopen',gmcallback = 37)
		
	Ibut(2) = gmCreatepushbutton(GraphMainPanel1_1(iplot),4,0,4,1 ,'adjust y shut',  &
    				gmtype=guserdefined,gmoncol=123,gmoffcol=121,&	
           gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Adjust Yshut',gmcallback = 38)
	
	Ibut(5) = gmCreatepushbutton(GraphMainPanel1_1(iplot),2,0,2,1 ,'reject',  &
    		gmtype=guserdefined,gmoncol=123,gmoffcol=121,&
           gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'reject',gmcallback = 311)
		
	Ibut(6) = gmCreatepushbutton(GraphMainPanel1_1(iplot),0,0,2,1 ,'accept',  &
    				gmtype=guserdefined,gmoncol=123,gmoffcol=121,&	
           gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'accept',gmcallback = 31)

  else if(ixposv.eq.-3) then
	Icon0g = gmCreateIcon(GraphMainPanel1_1(iplot),2,0,2,1 ,'Msml1053',  &
    			GDLLICON,0,&
           gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Reject',gmcallback = 401)
	Icon0g = gmCreateIcon(GraphMainPanel1_1(iplot),0,0,2,1 ,'Msml1021',  &
    			GDLLICON,0,&
           gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Continue',gmcallback = 402)
  else
  Icon0g = gmCreateIcon(GraphMainPanel1_1(iplot),4,0,1,1 ,'MBIG1085',  &
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
   if(ipos.eq.0) then
   Icon3g = gmCreateIcon(GraphMainPanel1_1(iplot),1,0, 1,1,'MBIG1005', &
			GDLLICON,0,&
	        gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
			gmHelp = 'Save as plotq',gmcallback = 5100+iplot) !15)
	Icon3g = gmCreateIcon(GraphMainPanel1_1(iplot),0,0, 1,1,'MBIG1037', &
			GDLLICON,0,&
	        gmHpos=Gright, gmVpos=GTOP,gmHFlag = GBUBBLEANDSTATUSBAR,&
				gmHelp = 'Save as ASCII',gmcallback = 5500+iplot)
    endif
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
		iplot0=iplot-1
	case(3)
		ixbegg=0
		iybegg=iygraph/2
		ixgraph=ixgraph/2
		iygraph=iygraph/2
		iplot0=iplot-2
	case(4)
		ixbegg=ixgraph/2
		iybegg=iygraph/2
		ixgraph=ixgraph/2
		iygraph=iygraph/2
		iplot0=iplot-3
   end select

   call gDefineRGB(0,1.,1.,0.94)
  
   call gmSetGuiGridMode(GOff) 
   Graphics1_1(iplot) = gmCreateGraphicsFrame(GraphMainPanel1_1(iplot0), ixbegg*ixpix, iybegg*iypix, &
   ixgraph*ixpix, iygraph*iypix, Graphics_frame, &
              	gmhpos=gleft,gmVpos=Gtop, gmExpand=GOFF,gmMove=7100+iplot ,gmSelect=7200+iplot)
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
  itext_1= gmCreateTextEntry(graph1_1(iplot), 1, 0, 4, 1,'',10, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=gtop, gmExpand=GOFF)

itext_2= gmCreateTextEntry(graph1_1(iplot), 6, 0, 4, 1,'',10, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=gtop, gmExpand=GOFF)
  else if(ixposv.eq.-3) then
  itext_3= gmCreateTextEntry(graph1_1(iplot), 1, 0, 4, 1,'',10, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=gtop, gmExpand=GOFF)
  itext_4= gmCreateTextEntry(graph1_1(iplot), 6, 0, 4, 1,'',10, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=gtop, gmExpand=GOFF)
  endif	                                                     
   call gmClearGraphicsFrame(Graphics1_1(iplot))
	
   if(ipos.le.1) call gmdrawwindow(graph1_1(iplot))
!   	call gsethardchars()
call gsetmixedchars()
	call gsetlinewidthmode(ghardware)
	call gSetLineEnd(GROUND	)
	call gseterrormode(gerroron)
endif  
end