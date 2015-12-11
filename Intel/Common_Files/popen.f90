subroutine popen(main,ipopen,iopth,ivalx1,x1,ivalx2,x2,iptog,ivaldat,ndat)

use menu_f90
integer iptog(5)
call gDefineRGB(122,0.80,0.89,0.87)
call gDefineRGB(123,0.73,0.82,0.79)
call gDefineRGB(124,0.66,0.97,0.87)
call gDefineRGB(38,1.,0.5,0.25)	!orange
iplotwid=15
iplothigh=5
if(iopth.eq.1) iplothigh=15

ipopen = gmCreateComplexDialogueBox(main,10, 3, iplotwid, iplothigh, GALL, 'Observed P(open) values', &
   gmVpos=GTOP, gmIconFormat=GDLLICON, gmIconFile='Gee')
ipopenp = gmCreatePanel(ipopen, 0, 0, iplotwid, iplothigh, gmType=GINVERSECHISEL, gmLineBorder=GOUTEREDGE, gmFillcol=122, &
      gmFillBorder=GOUTEREDGE, gmVpos=GTOP)

itext=gmCreateTextEntry(ipopenp,1,1,10,1,'Plot between conc (micromolar):', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 , gmVpos=GTOP)

itext1=gmCreateTextEntry(ipopenp,1,2,2,1,'Xmin=', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 , gmVpos=GTOP)

ivalx1=gmCreateValueEntry(ipopenp, 3, 2, 4, 1, x1, 10, 3, gedit,&
	gmVpos=GTOP)

itext2=gmCreateTextEntry(ipopenp,8,2,2,1,'Xmax=', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmVpos=GTOP)

ivalx2=gmCreateValueEntry(ipopenp, 10, 2, 4, 1, x2, 10, 3, gedit,&
	gmVpos=GTOP)


if(iopth.eq.1) then
	 iradio = gmCreateRadioBox(ipopenp, 1, 4, iplotwid-2, 5, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
     gmFillCol=124,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE,&
	 gmTitle=' Add observed P(open) points to the graph ?',gmVpos=GTOP)
	 iptog(1) = gmCreateToggleButton(iRadio, 1, 1 , iplotwid-4, 1, 'No points', 0, &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmcallback=4516)
	 ipTog(2) = gmCreateToggleButton(iRadio, 1, 2 ,iplotwid-4 , 1, &
	 'Read P(open) values from a CVFIT file', 0, &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmcallback=4517)
	 ipTog(3) = gmCreateToggleButton(iRadio, 1, 3 , iplotwid-4, 1, &
	 ' Type in the P(open) values ', 0, &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmcallback=4518)
	 call gmSetToggleSwitch(ipTog(1),Gon)
	 itext3=gmCreateTextEntry(ipopenp,1,10,8,1,' Number of points to be added = ',&
	 255, gdisplay, gmBack1Col=0, gmBack2Col=12, gmTextCol=1 , gmVpos=GTOP)
	ivaldat=gmCreateValueEntry(ipopenp, 10, 10, 4, 1, ndat, 10, 0, gedit,&
	gmVpos=GTOP)
	call gmsetwidgetstatus(ivaldat,gunselectable)
	 itext3=gmCreateTextEntry(ipopenp,1,12,6,1,'Specify SD for each point',&
	 255, gdisplay, gmBack1Col=0, gmBack2Col=12, gmTextCol=1 , gmVpos=GTOP)
	 iradio1 = gmCreateRadioBox(ipopenp,8 , 11,6 , 3, gmType=GFREEFORM, gmBorderType=goff, &
     gmFillCol=122,gmLineBorder=Gouteredge, gmFillBorder=GOUTEREDGE,&
	 gmTitle='',gmVpos=GTOP)
	 iptog(4) = gmCreateToggleButton(iRadio1, 1, 1 , 2, 1, 'Yes', 0, &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)
	 ipTog(5) = gmCreateToggleButton(iRadio1, 4, 1 ,2 , 1, 'No', 1, &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)
	
endif
   iradB = gmCreatePushButton(ipopenp,0,0 , iplotwid, 1, 'Continue', gmType=GUSERDEFINED,&
   gmhpos=gleft,gmVpos=Gbottom,gmOffcol=38, gmTextCol=1,&
   gmcallback=4511)
   call gmdrawwindow(ipopen)
   if(iopth.eq.1) then
    
	 	call gmsetwidgetstatus(iptog(4),gunselectable)
			call gmsetwidgetstatus(iptog(5),gunselectable)
	endif
end