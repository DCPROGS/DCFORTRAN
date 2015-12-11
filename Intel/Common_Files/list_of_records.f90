subroutine list_of_records(Main,Records,List7_1,ncol,nrow,label7,text7,iwid, & 
		   ifiltype,Button7,wtitle)


   use menu_f90
   integer:: Main
   integer:: Records
   integer:: RecordMainPanel
   integer :: iwid(ncol)
   integer:: Static7(ncol),Panel7(ncol),Button7(10)
   character*15 label7(ncol),textbut7(10)
   character*(*) text7(nrow)
   CHARACTER*80 wtitle
   textbut7(1)='Display 1/page'
   textbut7(2)='display 4/page'
   
   textbut7(3)='Print 1/page'
   textbut7(4)='Print 4/page'
   if(ifiltype.eq.2) then
	textbut7(5)='WMF 1/page'
	textbut7(6)='WMF all'
   
	textbut7(7)='BMP 1/page'
	textbut7(8)='BMP 4/page'
   endif
   
   lwin=nrow+4
   if(lwin.gt.18) lwin=18
   lat=20
   lwin1=lwin-3
   if(ifiltype.eq.2) then
		lat=20
		lwin=lwin+2
		lwin1=lwin-4
	endif
   Records = gmCreateComplexDialogueBox(Main, 5, 7, lat, lwin, GALL, wtitle, gmvpos=gtop,&
            gmhpos=gleft,gmIconFormat=GDLLICON,gmIconFile='MBIG1062',gmCallBack=-71,gmSelect=-72)
! Create main panel for form
   RecordMainPanel=gmCreatePanel(Records, 0, 0, lat, LWIN, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

! Create panel Panel11 child of RecordMainPanel
   List7_1 = gmCreateListBox(RecordMainPanel, 0, 1, lat, LWIN1, list=GNONE, gmType=GMULTIPLECHOICE2, &
			gmVpos=GTOP, gmExpand=GOFF)
   
   do i=1,nrow
	 call gmSetListEntry(List7_1,action=GADD,gmString=text7(i))	
   enddo
	
   
   Panel7(1)=gmCreatePanel(Records,0,0 ,3, 1, &
              	gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmType=GCHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
	
   ltype=2*ifiltype
   
   Panel7(2)=gmCreatePanel(Records,3,0 ,ltype, 1, &
              	gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmType=GCHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
  
   if(ncol.eq.3) then
	  Panel7(ncol)=gmCreatePanel(Records,ltype+3,0 ,lat-ltype-3, 1, &
              	gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmType=GCHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
   endif
   do i=1,ncol
	  Static7(i)=gmCreateTextEntry(Panel7(i), 0, 0, 2, 1,label7(i), 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=Gleft, gmSize=80,&
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
   enddo
   IF (IFILTYPE.EQ.10) THEN
   Button7(1)=gmCreatePushButton(RecordMainPanel, lat-8,LWIN-1 , 4, 1,'Open',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=GTOp, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=1201) !37)
   Button7(2)=gmCreatePushButton(RecordMainPanel, lat-4,LWIN-1 , 4, 1,'Cancel',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
             	gmVpos=GTOp, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=1209) !42)
	else IF (IFILTYPE.EQ.11) THEN
   Button7(1)=gmCreatePushButton(RecordMainPanel, lat-8,LWIN-1 , 4, 1,'Open',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=GTOp, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=-91) !37)
	ifiltype=10
	ELSE	
   
   if(ifiltype.eq.2) then
   do i=1,2
     Button7(i)=gmCreatePushButton(RecordMainPanel, 5*(i-1),LWIN-2 , 5, 1,textbut7(i),&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=GTOp, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=5010+i)
 
	  
   enddo
   Button7(5)=gmCreatePushButton(RecordMainPanel, 10,LWIN-2 , 5, 1,textbut7(5),&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=GTOp, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=5015)
   Button7(6)=gmCreatePushButton(RecordMainPanel, 15,LWIN-2 , 5, 1,textbut7(6),&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=GTOp, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=5019)
    Button7(3)=gmCreatePushButton(RecordMainPanel, 0,0 , 5, 1,textbut7(3),&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=5013)
	Button7(4)=gmCreatePushButton(RecordMainPanel, 5,0 , 5, 1,textbut7(4),&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=5014)
    Button7(9)=gmCreatePushButton(RecordMainPanel, 10,0 , 5, 1,'Print all (1/page)',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=5025)
	Button7(10)=gmCreatePushButton(RecordMainPanel, 15,0 , 5, 1,'Print all (4/page)',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=5026)
   else
   if(wtitle.eq.'cvdat for popen') then
   Button7(1)=gmCreatePushButton(RecordMainPanel, 1,LWIN-1 , 5, 1,'Continue',&
              	gmType=GSTANDARD, gmSize=100,gmTextCol=0, &
              	gmVpos=GTOp, gmHpos=Gright,gmExpand=GOFF,gmCallback=4522)
   else
	do i=1,4
	  Button7(i)=gmCreatePushButton(RecordMainPanel, 5*(i-1),LWIN-1 , 5, 1,textbut7(i),&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=GTOp, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=5010+i)
   enddo
  endif
   endif
ENDIF
call gmdrawwindow(records)
end