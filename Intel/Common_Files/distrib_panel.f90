subroutine distrib_panel(main,idradio,nradio,radio_text,idradio_Toggle,icall)
use menu_f90
integer iradiox(20),iradioy(20),idradio_toggle(20)
real radiox(20),radioy(20)
character*(*) radio_text(20)
character*60 title
iwid=18
nrad=nradio


call gDefineRGB(141, 0.192157, 0.984314, 0.964706)
 call gDefineRGB(122,0.80,0.89,0.87)

idradio = gmCreateComplexDialogueBox(Main,25 ,7 ,iwid , nrad+6, GALL, radio_text(20), &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
	
idradio_MainPanel=gmCreatePanel(idradio, 0, 0,iwid , nrad+8, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE)

idradio_Radio1 = gmCreateRadioBox(idradio_MainPanel, 1, 1, iwid-2, nradio+2, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmFillCol=141,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='', gmVpos=GTOP)

idradio_Toggle(1) = gmCreateToggleButton(idradio_Radio1, 1, 1 , iwid-4, 1, radio_text(1), 1, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)
do i=2,nradio
   idradio_Toggle(i) = gmCreateToggleButton(idradio_Radio1, 1, i , iwid-4, 1, radio_text(i), 0, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)

enddo
idradio_mainpanel1 = gmCreatepanel(idradio_MainPanel,1, nradio+3, iwid-2, 4, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmFillCol=122,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='', gmVpos=GTOP)

iniText = gmCreateTextEntry(idradio_MainPanel1,1 , 1, iwid-7, 1,'Show the pdf without missed events', &
	255, GDIsplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=Ghorizontal, gmVpos=GTOP)

!idradio_Radio2 = gmCreateRadioBox(idradio_MainPanel, iwid-6, nradio+3, 5, 1, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
!      gmFillCol=0,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='', gmVpos=GTOP)

 
idradio_Toggle(4) = gmCreateToggleButton(idradio_MainPanel1, iwid-6, 1 , 2, 1, 'Y', 1, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOp)


idradio_Toggle(5) = gmCreateToggleButton(idradio_MainPanel1,iwid-4 , 1, 2,1, 'N', 0, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)


iniText = gmCreateTextEntry(idradio_MainPanel1,1 , 2, iwid-7, 1,'Rescale this pdf to unit area above t=tres',&
	 255, GDIsplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=16 ,gmScrollMode=Ghorizontal, gmVpos=GTOP)

!idradio_Radio3 = gmCreateRadioBox(idradio_MainPanel, iwid-6, nradio+4, 7, 1, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
!      gmFillCol=0,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='', gmVpos=GTOP)

idradio_Toggle(6) = gmCreateToggleButton(idradio_MainPanel1, iwid-6, 2 , 2, 1, 'Y', 1, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOp)


idradio_Toggle(7) = gmCreateToggleButton(idradio_MainPanel1, iwid-4, 2 , 2, 1, 'N', 0, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)


idradio_Button1 = gmCreatePushButton(idradio_MainPanel,iwid/2,0 , iwid, 1, 'OK', &
   gmhpos=gmiddle,gmVpos=Gbottom,&
   gmcallback=icall)
   call gmdrawwindow(idradio)
end