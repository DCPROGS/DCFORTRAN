subroutine change_axis(main,iax_entry,icall,mtext1,x1,y1,x2,y2,xt,yt,&
ivalx1,ivaly1,ivalx2,ivaly2,ivaltx,ivalty)
use menu_f90
integer:: iax_entry
character*80 mtext1
character*20 text1,text2,text3,text4
if(icall.eq.245) then
iy=11
else
iy=9
endif
	iax_entry = gmCreateComplexDialogueBox(Main, 22, 4, 17, iy, GALL, mtext1, &
				 gmMaxWidth=32,gmMaxHeight=20,gmvpos=gtop,&
              	 gmIconFormat=GDLLICON,gmIconFile='MBIG1040')

	k=4
	if(icall.eq.245.or.icall.eq.263) then
		it=gmCreateTextEntry(iax_entry,1,1,3,1,'x axis',80,gdisplay,gmhpos=gleft,gmVpos=GTOP)
		it=gmCreateTextEntry(iax_entry,1,3,3,1,'y axis',80,gdisplay,gmhpos=gleft,gmVpos=GTOP)
		
		ixt=gmCreateTextEntry(iax_entry,8,2,2,1,'xmax =',80,gdisplay,gmhpos=gleft,gmVpos=GTOP)
	
		ivalx2=gmCreateValueEntry(iax_entry,10 , 2, 4, 1, x2, 20, k, gedit,&
	gscientific,gmhpos=gleft,&
	gmVpos=GTOP)
	
			ixt=gmCreateTextEntry(iax_entry,1,2,2,1,'xmin =',80,gdisplay,gmhpos=gleft,gmVpos=GTOP)
	
		ivalx1=gmCreateValueEntry(iax_entry,3 , 2, 4, 1, x1, 20, k, gedit,&
		gscientific,gmhpos=gleft,&
		gmVpos=GTOP)

		call gmsetvaluesetting(ivalx1,x1)
		iyt=gmCreateTextEntry(iax_entry,1,4,2,1,'ymin=',80,gdisplay,gmhpos=gleft,gmVpos=GTOP)
		ivaly1=gmCreateValueEntry(iax_entry, 3, 4, 4, 1, y1, 20, k, gedit,gmentryformat=gscientific,gmhpos=gleft,&
		gmVpos=GTOP)
		call gmsetvaluesetting(ivaly1,y1)
			iyt=gmCreateTextEntry(iax_entry,8,4,2,1,'ymax=',80,gdisplay,gmhpos=gleft,gmVpos=GTOP)
		ivaly2=gmCreateValueEntry(iax_entry, 10, 4, 4, 1, y2, 20, k, gedit,&
	gmentryformat=gscientific,gmhpos=gleft,gmVpos=GTOP)
	
		
		ixt=gmCreateTextEntry(iax_entry,1,6,2,1,'x tic=',60,gdisplay,gmhpos=gleft,gmVpos=GTOP)
	
		ivaltx=gmCreateValueEntry(iax_entry,3 , 6, 4, 1, xt, 20, k, gedit,&
	gscientific,gmhpos=gleft,&
	gmVpos=GTOP)
	
		iyt=gmCreateTextEntry(iax_entry,8,6,2,1,'y tic=',80,gdisplay,gmhpos=gleft,gmVpos=GTOP)
		ivalty=gmCreateValueEntry(iax_entry, 10, 6, 4, 1, yt, 20, k, gedit,&
	gmentryformat=gscientific,gmhpos=gleft,gmVpos=GTOP)
	else
		it=gmCreateTextEntry(iax_entry,1,1,3,1,'beginning',100,gdisplay,gmhpos=gleft,gmVpos=GTOP)
		it=gmCreateTextEntry(iax_entry,1,3,3,1,'end',60,gdisplay,gmhpos=gleft,gmVpos=GTOP)
		ixt=gmCreateTextEntry(iax_entry,1,2,1,1,'x =',60,gdisplay,gmhpos=gleft,gmVpos=GTOP)
	
		ivalx1=gmCreateValueEntry(iax_entry,3 , 2, 4, 1, x1, 20, k, gedit,&
		gscientific,gmhpos=gleft,&
		gmVpos=GTOP)
		call gmsetvaluesetting(ivalx1,x1)
		iyt=gmCreateTextEntry(iax_entry,8,2,1,1,'y=',60,gdisplay,gmhpos=gleft,gmVpos=GTOP)
		ivaly1=gmCreateValueEntry(iax_entry, 10, 2, 4, 1, y1, 20, k, gedit,gmentryformat=gscientific,gmhpos=gleft,&
		gmVpos=GTOP)
		call gmsetvaluesetting(ivaly1,y1)
		ixt=gmCreateTextEntry(iax_entry,1,4,1,1,'x =',60,gdisplay,gmhpos=gleft,gmVpos=GTOP)
	
		ivalx2=gmCreateValueEntry(iax_entry,3 , 4, 4, 1, x2, 20, k, gedit,&
		gscientific,gmhpos=gleft,&
		gmVpos=GTOP)
		call gmsetvaluesetting(ivalx2,x2)
		iyt=gmCreateTextEntry(iax_entry,8,4,1,1,'y=',60,gdisplay,gmhpos=gleft,gmVpos=GTOP)
		ivaly2=gmCreateValueEntry(iax_entry, 10, 4, 4, 1, y2, 20, k, gedit,gmentryformat=gscientific,gmhpos=gleft,&
		gmVpos=GTOP)
		call gmsetvaluesetting(ivaly2,y2)
	endif
	
	

	
	it=gmCreateTextEntry(iax_entry,1,2,15,1,'For other attributes please click on the icons on the toolbar',&
	60,gdisplay,gmhpos=gleft,gmVpos=Gbottom)
	it=gmCreateTextEntry(iax_entry,1,1,15,1,'When you finished click OK button',60,gdisplay,gmhpos=gleft,gmVpos=Gbottom)
	if(icall.eq.245.or.icall.eq.263) then
		call gmDefineKeyselectCallback(13,246)
	ibu=246
	else
	ibu=248
		call gmDefineKeyselectCallback(13,248)
	endif
	ivb=gmCreatePushButton(iax_entry,13,0, 2, 1,'OK',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=ibu)
	call gmdrawwindow(iax_entry)

end