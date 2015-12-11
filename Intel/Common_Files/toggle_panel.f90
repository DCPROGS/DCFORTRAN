subroutine toggle_panel(Main,ITOGGLE,ittoglepanel,ntog,text_tog,intoggle,itcall,valdat,idat,itogbutton,itype)

use menu_f90
character*(*) text_tog(100)
real valdat(100)
integer intoggle(100),idat(100),itype(50)
integer :: itogButton
if(itcall.eq.-1)then
	ixpos=0
	iypos=1	
	ilg=ntog
else if(itcall.eq.-8.or.itcall.eq.8051) then
	ixpos=23
	iypos=15
	ilg=ntog+2	
else
	ixpos=23
	iypos=4
	ilg=ntog+2
endif

if(itcall.eq.6085.or.itcall.eq.1500.or.itcall.eq.4900.or.itcall.eq.4905&
.or.itcall.eq.4660.or.itcall.eq.4851.or.itcall.eq.70.or.itcall.eq.8051) then
	iwtog=12
	iwid=12
else if(itcall.eq.4700&
.or.itcall.eq.6091.or.itcall.eq.6200) then
iwtog=6
iwid=16
else if(itcall.eq.-200) then
    iwtog=10
	iwid=16
else
	iwtog=8
	iwid=8
	if(itcall.eq.2035.or.itcall.eq.200.or.itcall.eq.210.or.&
	itcall.eq.220.or.itcall.eq.230.or.itcall.eq.-1) then
		iwtog=3
		iwid=3
	endif
endif

if(itcall.eq.-1) then
 iwid=4
 iwtog=4
! itoggle = gmCreateComplexDialogueBox(Main, ixpos, iypos, iwid, ilg, GALL, ' ', &
!              gmVpos=GTOP,	gmIconFormat=GDLLICON,gmIconFile='MBIG1052')
 itogglepanel=gmCreatePanel(main, 0, 0, iwid, ilg, &
              	gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmborderType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

else
! Set up complex dialogue box Eqs child of Main
   itoggle = gmCreateComplexDialogueBox(Main, ixpos, iypos, iwid, ilg, GALL, text_tog(100), &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
    


	itogglePanel=gmCreatePanel(itoggle, 0, 0, iwid, ilg, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

! Create toggle button Toggle21 child of EqsMainPanel
endif

if(itcall.eq.6085.or.itcall.eq.-8.or.itcall.eq.8051) then
call gDefineRGB(224, 0.792157, 1.000000, 0.792157)
iradio_Radio1 = gmCreateRadioBox(itogglePanel, 0, 0, iwid, ilg-1, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmFillCol=224,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='', gmVpos=GTOP)
do i=1,ntog
 
 inToggle(i) = gmCreateToggleButton(iradio_Radio1 , 1, i-1, iwtog, 1, text_tog(i), 0, &
              	gmType=G3Dradio,gmVpos=GTOP, gmExpand=GOFF, gmCallBAck=itcall+i)
enddo
else
do i=1,ntog
   if(i.eq.1.and.itcall.eq.6200) then
   itcall=6091
   istat=1
  
   else
   if(itcall.eq.6200) itcall=6091
   istat=0
   if(i.eq.1) istat=1
   endif
   
   inToggle(i) = gmCreateToggleButton(itogglePanel, 0, i-1, iwtog, 1, text_tog(i), istat, &
              	gmType=G3Dradio,gmVpos=GTOP, gmExpand=GOFF, gmCallBAck=itcall+i)
   if(i.eq.1.and.itcall.eq.6200) then
   itcall=6091
   istat=1
   call gmSetToggleSwitch(inToggle(i), Gon)
   else
   if(i.eq.1.and.itcall.eq.6200) then
   itcall=6091
   istat=1
   call gmSetToggleSwitch(inToggle(i), Goff)
   else
   if(itcall.eq.6200) itcall=6091
   istat=0
   endif
   if(itcall.eq.6200) itcall=6091
   istat=0
   endif
   if(itcall.eq.2035.or.itcall.eq.80.or.itcall.eq.150.or.itcall.eq.200.or.itcall.eq.210.or.itcall.eq.220.&
	or.itcall.eq.230.or.itcall.eq.100.or.itcall.eq.1500.or.itcall.eq.-1.or.itcall.eq.4510.or.&
	itcall.eq.4700.or.itcall.eq.4900.or.itcall.eq.4905.or.itcall.eq.80.or.itcall.eq.4660&
	.or.itcall.eq.4851.or.itcall.eq.-125.or.itcall.eq.-113) then
	
   else			
		idat(i)=gmCreateValueEntry(itogglepanel,iwtog+1,i-1,4,1,valdat(i),10,itype(i),gedit,&
		gmVpos=GTOP, gmExpand=GOFF)
			IF(VALDAT(I).EQ.0.) CALL GMSETVALUESETTING(IDAT(I),0)
   endif
enddo

if(itcall.eq.2035) itcall=2036
if(itcall.eq.80.or.itcall.eq.150.or.itcall.eq.200.or.itcall.eq.210.or.itcall.eq.220.&
or.itcall.eq.230.or.itcall.eq.100) itcall=400
if(itcall.eq.80.or.(itcall.ge.271.and.itcall.le.276)) itcall=-1
if(itcall.ne.-1) then
if(itcall.eq.6010) itcall=6022
endif
endif

iw2=iwid

if(itcall.eq.-125.or.itcall.eq.-112) then
	call gmSetToggleSwitch(inToggle(1), Gon)
	iw2=iwid/2
	itogButton=gmCreatePushButton(itogglePanel,0,ntog+1, iwid/2, 1,'Cancel',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=GTOp, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=-126)
endif
itogButton=gmCreatePushButton(itogglePanel,iwid-iw2,ntog+1, iw2, 1,'Continue',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=GTOp, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=itcall)
call gmdrawwindow(itoggle)
if(itcall.eq.8051) call gmSetToggleSwitch(inToggle(5), Gon)
!call gmdrawwindow(itogglepanel)

end