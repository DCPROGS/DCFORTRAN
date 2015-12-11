subroutine question_box(main,iqbox,nq,iqtext,iqtoggle,icall)
use menu_f90
character*100 iqtext(10)
integer	iqtoggle(10,2)
call gDefineRGB(38,1.,0.5,0.25)	!orange
iqbox = gmCreateComplexDialogueBox(main,15, 8, 14,3*nq+1 , GALL, '', &
   gmVpos=GTOP, gmIconFormat=GDLLICON, gmIconFile='Gee')

do i=1,nq

   iradio0 = gmCreateRadioBox(iqbox, 0,3*(i-1), 14, 3, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
		gmFillCol=51,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle=iqtext(i),&
		gmVpos=Gtop)
   iqToggle(i,1) = gmCreateToggleButton(iradio0, 4, 1 , 3, 1, 'Yes', 0, &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)
   iqToggle(i,2) = gmCreateToggleButton(iradio0, 7, 1 , 3, 1, 'No', 1, &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)
enddo
iradio_Button1 = gmCreatePushButton(iqbox,1,0 , 12, 1, 'Continue',gmType=GUSERDEFINED, &
   gmhpos=gleft,gmVpos=Gbottom,gmOffcol=38, gmTextCol=1,&
   gmcallback=icall)

  call gmdrawwindow(IQBOX)
end