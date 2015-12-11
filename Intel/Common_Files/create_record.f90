subroutine create_record(Main,NEW_data_list,TextArray,TextButton,nfile)

use menu_f90


integer :: Main
integer :: new_Data_list
integer :: new_DataMainPanel
integer :: textArray
integer :: textButton
character*60 nfile,xtext
INTEGER*4 jstrec(100),lstrec(100)

LOGICAL PRESENT
type(GARRAYCELL) arrayattribs

xtext='Title for record'//char(124)//' X label'//char(124)//' Y Label'//&
	char(124)//'Sets'

INQUIRE(file=nfile,exist=present)
if(.not.present) then
	do i=1,100
		jstrec(i)=0
		lstrec(i)=0
	enddo
	iver=1003
	nplot=0
    OPEN(unit=12,file=NFILe,status='UNKNOWN',access='DIRECT', form='BINARY',RECL=1)
	write(12,rec=1) nplot,jstrec,lstrec,iver
	CLOSE(unit=12)
else
	Icall=gmDisplayMessageBox('Stop ','The file already exists.Append your record to it?',&
		GEXCLAMATION,GYESNO)
	if (icall.eq.gyesbutton) then
		OPEN(unit=12,file=NFILe,status='UNKNOWN',access='DIRECT', form='BINARY',RECL=1)
		read(12,rec=1) nplot,jstrec,lstrec,iver1
		CLOSE(unit=12)
	    if(iver1.lt.1003) then
			Icall=gmDisplayMessageBox(' ','This is an old file.Enter a new name for file',GinforMATION,Gok)
			goto 1
	    endif
	else
		Icall=gmDisplayMessageBox(' ','Enter a new name for file',GinforMATION,Gok)
		goto 1
	endif		
endif

New_Data_list = gmCreateComplexDialogueBox(Main, 2, 4, 26, 10, GALL, 'New',gmvpos=gtop)

! Create main panel for form
 
New_DataMainPanel=gmCreatePanel(New_Data_list, 0, 0, 26, 10, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

! Create value array ValArray6 child of DataMainPanel
   TextArray= gmCreateTextArray(nEW_DataMainPanel, 0, 0, 26, 9, 4, 10, &
              	gmAxisW=50, gmAxisH=25, gmXtext=xtext, gmYtext='*digits', &
              	gmXjust=GCENTRE, gmYjust=GCENTRE, gmVpos=GTOP, gmExpand=GOFF)
   call gmSetGuiGridMode(GOFF)
   call gmEnqCellAttribs(TextArray, 1, 1, arrayattribs)
   arrayattribs%width=160
   arrayattribs%height=24
   arrayattribs%justify=GLEFT

   do i=1,3
   do j=1,10
	call gmSetCellAttribs(TextArray, i,j , arrayattribs)
   enddo
   enddo
   arrayattribs%width=30
   arrayattribs%height=24
   do j=1,10
	call gmSetCellAttribs(TextArray, 4,j , arrayattribs)
   enddo	
   call gmSetGuiGridMode(GON)
   TextButton=gmCreatePushButton(New_DataMainPanel,22,9, 3, 1,'OK',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=GTOp, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=5004)!900

call gmdrawwindow(new_data_list)
1  continue

end