subroutine radio_panel(main,iradio,nradio,iradio_toggle,radio_text,idata,&
iradiox,radiox,iradioy,radioy,icall,title)
use menu_f90
integer iradiox(200),iradioy(200),iradio_toggle(200)
real radiox(200),radioy(200)
character*(*) radio_text(200)
character*60 title
iwid=22
nrad=nradio
ip1=120

if(icall.eq.4859) then
	iwid=18
	ip1=16
endif
if(idata.eq.3) then
	ip1=6
	iwid=ip1+2
    nrad=nradio
else if(idata.eq.4) then
	ip1=6
	iwid=ip1+2
    nrad=nradio+4	
else if (idata.eq.2) then
    ip1=6
	ip2=10
	iwid=ip1+ip2+2
	nrad=nradio+3
	
else if(idata.eq.1) then
	ip1=10
	ip2=5
	iwid=ip1+ip2+2
else if(idata.eq.5) then
	ip1=10
	iwid=ip1+2
    nrad=nradio-3
endif
if(idata.lt.0) ind=abs(idata)
if(ind.eq.0) ind=1
icbk=icall
if(icall.eq.-44) then
	icall0=icall
	icbk=1500
endif
ihpan=nrad+5
if(ihpan.gt.30) ihpan=30

if(icall.ge.301.and.icall.le.309) icbk=330
 call gDefineRGB(141, 0.192157, 0.984314, 0.964706)
iradio = gmCreateComplexDialogueBox(Main,25 ,4 ,iwid , ihpan, GALL, title, &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
	
iradio_MainPanel=gmCreatePanel(iradio, 0, 0,iwid , nrad+5, gmHpos=GCENTRE, gmVpos=Gleft, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE,gmScrollMode=Gvertical)

iradio_Radio1 = gmCreateRadioBox(iradio_MainPanel, 1, 1, ip1, nradio+2, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmFillCol=141,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='', gmVpos=GTOP)

do i=1,nradio
   if(ind.eq.i) then
   iradio_Toggle(i) = gmCreateToggleButton(iradio_Radio1, 1, i , ip1-2, 1, radio_text(i), 1, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP,gmcallback=icbk+i)
   else
   iradio_Toggle(i) = gmCreateToggleButton(iradio_Radio1, 1, i , ip1-2, 1, radio_text(i), 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP,gmcallback=icbk+i)
   endif
   
   if(idata.gt.0.and.idata.lt.3) then
     iradio_MainPanel1=gmCreatePanel(iradio_MainPanel, ip1+1, 1,ip2 , nradio+2, gmHpos=Gleft, &
      gmVpos=Gtop, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillCol=141,gmFillBorder=GOUTEREDGE)

	iradiox(i)=gmCreateValueEntry(iradio_mainpanel1,1,i,3,1,radiox(i),10,5,gedit,&
	gmVpos=GTOP, gmExpand=GOFF)
	IF(radiox(I).EQ.0.) CALL GMSETVALUESETTING(iradiox(I),0)
    if(idata.gt.1) then
	iradioy(i)=gmCreateValueEntry(iradio_mainpanel1,5,i,3,1,radioy(i),10,5,gedit,&
	gmVpos=GTOP, gmExpand=GOFF)
	IF(radioy(I).EQ.0.) CALL GMSETVALUESETTING(iradioy(I),0)
	endif
   endif
enddo
if(icall.eq.80) icall=256


if(icall.ge.100.and.icall.lt.200) then
	it=gmCreateTextEntry(iradio_mainpanel,0,nradio+4,iwid,3,'To change colour,line type or delete. &
	click on the icons on the toolbar. When you finished click OK button',&
	60,gdisplay,gmhpos=gleft,gmVpos=Gtop)

endif
if(icall0.ne.-44) then
if(icall.eq.210.or.icall.eq.220.or.icall.eq.230.or.icall.eq.200) then 
	icall=200
else if(icall.eq.-43) then
iradio_Button1 = gmCreatePushButton(iradio_MainPanel,0,1 , iwid, 1, 'Continue', &
   gmhpos=gleft,gmVpos=Gbottom,&
   gmcallback=icall)

else
   iradio_Button1 = gmCreatePushButton(iradio_MainPanel,0,1 , iwid/2, 1, 'Cancel', &
   gmhpos=gleft,gmVpos=Gbottom,&
   gmcallback=1426)
   iradio_Button1 = gmCreatePushButton(iradio_MainPanel,iwid/2,1 , iwid/2, 1, 'OK', &
   gmhpos=gleft,gmVpos=Gbottom,&
   gmcallback=icall)
endif
else

	iradio_Button1 = gmCreatePushButton(iradio_MainPanel,0,1 , iwid/2, 1, 'Read another mechanism', &
   gmhpos=gleft,gmVpos=Gbottom,&
   gmcallback=8002)
   iradio_Button1 = gmCreatePushButton(iradio_MainPanel,iwid/2,1 , iwid/2, 1, 'Continue', &
   gmhpos=gleft,gmVpos=Gbottom,&
   gmcallback=1500)
endif
call gmSetGuiGridMode(GOn)
call gmdrawwindow(iradio)
end