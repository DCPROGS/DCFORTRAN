 subroutine helpcvfit(main,indlist,indlista,list,nlist,idkbutt,idsbutt,icall)
 use menu_f90
 
 CHARACTER*70 list(50)

 call gDefineRGB(20,0.5,0.,0.25)	!burgundy
 
 if(icall.eq.981) then
	list(1)=' About Curve Fitting'
    list(2)=' Arrows'
	list(3)=' Axes'
	list(4)='     frame'
	list(5)='     scale'
	list(6)='     shape'
	list(7)=' Curves'
	list(8)=' Data'
	list(9)=' File'
	list(10)='     new data file/record'
	list(11)='     open old data file'

	list(12)=' Graph'
	list(13)='     queue as binary file'
	list(14)='     rescale'
	list(15)='     save as image file' 
	list(16)=' Interpolate'
	list(17)=' Lines'
	list(18)='     steep'
	list(19)='     vertical'
	list(20)='     horizontal'
	list(21)=' Parameters'
	list(22)=' Text'
	list(23)=' Title'
	list(24)=' Fit'
	list(25)='     fitting modes'
	list(26)='     equations'
	list(27)='     errors'
	list(28)='     initial guesses'
	nlist=28
	icnext=982
else if(icall.eq.961) then
	list(1)=' Start'
    list(2)=' - normal model'
	list(3)=' - independent model'
	list(4)='  Add states'
	list(5)='  Make the link between states'
	list(6)='  View/edit state properties'
	list(7)='  View/edit rate properties'
	list(8)='  Finalise the independent model'
	list(9)='  Save the model'
	list(10)='  Print'
	nlist=10
	icnext=962
else if(icall.eq.951) then

    list(1)=' Arrows'
	list(2)=' Axes'
	list(3)='     frame'
	list(4)='     scale'
	list(5)='     shape'
	list(6)=' Curves'
	list(7)=' Data'
	list(8)=' File'
	list(9)='     new data file/record'
	list(10)='     open old data file'
	list(11)=' Graph'
	list(12)='     queue as binary file'
	list(13)='     rescale'
	list(14)='     save as image file' 
	list(15)=' Interpolate'
	list(16)=' Lines'
	list(17)='     steep'
	list(18)='     vertical'
	list(19)='     horizontal'
	list(20)=' Parameters'
	list(21)=' Text'
	list(22)=' Title'
	nlist=22
	icnext=982
else if(icall.eq.971) then
	list(1)=' Modify '
    list(2)='   entire model'
	list(3)='   microscopic reversibility'
	list(4)='   state parameters'
	list(5)='   rate parameters'

	nlist=5
	icnext=972
endif
	indlist = gmCreateComplexDialogueBox(Main, 4,4, 17, &
    20,GALL, 'Index',gmvpos=gtop, gmIconFormat=GDLLICON,&
    gmIconFile='MBIG1052')

 iStatic = gmCreateTextEntry(indlist, 1 ,0, 13, 7,'To modify the drawing attributes, &
  select the graph element from the pop up menus "Graph" or "Edit 2D" ; &
 in some cases, a new window will open where you can modify the coordinates; &
 to change : colour, type,thickness,&
 font attributes or to delete click on the corresponding icon on the menu bar;&
 also for delete you can do it by choosing the background colour from the colour palette;&
 when you finish click the OK button'&
 , 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GCENTRE, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=20, &
              	gmVpos=GTOP, gmExpand=GOFF)

ilistPanel=gmCreatePanel(indlist, 0, 7, 15, 13, &
              	gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

! Create main panel for form
	indlista=gmCreatelistbox(ilistpanel, 1, 1, 13, 11,0, &
     gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON)
 
	   do i=1,nlist
            CALL LISSET(INDLISTA,i,list(i),1)
	   enddo
	   idkbutt=gmCreatePushButton(ilistpanel,9,0, 4, 1,'Cancel',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=GBOTTOM, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=998)

    idsbutt=gmCreatePushButton(ilistpanel,5,0, 4, 1,'Display',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=GBOTTOM, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=icnext)
call gmdrawwindow(indlist)    
end	   
