subroutine plot_options(main,iplot0,iplot,icall,nlig,nrange,hjmcalc,ligname,ylo,yhi,conc,&
           iplot_Value2,iplot_text,iplot_TxtArray2,iplot_Toggle1,iplot_Toggle3,iplot_Toggle4,&
           title1)
use menu_f90

real*4 conc(10,10)
real*4 ylo(20),yhi(20)
integer iplot_Toggle0(10),iplot_Toggle1(10),iplot_Toggle3(10),iplot_Toggle4(10),iplot_text(10)
character*64 title1
character*(80) radio_text1(10)
character*(80) radio_text3(10)
character*100  xtext,text
character*12 text1,text2
character*11 cnum,cnum0
character*20 ligname(10)
logical hjmcalc
type (GLIMIT) :: Graphics_frame
type(GARRAYCELL) arrayattribs

id1=1
! grey blue palette
   call gDefineRGB(101, 0.831372, 0.901960, 0.901960)
   call gDefineRGB(102, 0.772549, 0.850980, 0.850980)
   call gDefineRGB(103, 0.745098, 0.819607, 0.819607)
   call gDefineRGB(104, 0.704117, 0.760784, 0.760784)
   call gDefineRGB(105, 0.662745, 0.729411, 0.729411)
   call gDefineRGB(38,1.,0.5,0.25)	!orange
! pink palette
	 call gDefineRGB(112,0.96,0.82,0.82)
	 call gDefineRGB(113,0.93,0.78,0.78)
	 call gDefineRGB(114,0.90,0.74,0.74)
	 call gDefineRGB(115,0.87,0.70,0.70)
	 call gDefineRGB(111,0.98,0.92,0.92)


! green palette
! pink palette
	 call gDefineRGB(122,0.80,0.89,0.87)
	 call gDefineRGB(123,0.73,0.82,0.79)
	 call gDefineRGB(124,0.66,0.97,0.87)
	 call gDefineRGB(125,0.89,0.97,0.94)
	 call gDefineRGB(121,0.87,0.97,0.94)
	 call gDefineRGB(41,0.5,0.5,0.75)!lilac
	 radio_text3(1)= ' (1) Distribution of log durations'
     radio_text3(2)= ' (2) Distribution of durations- set bins manually'
     radio_text3(3)= ' (3) Distribution of durations- 20 bins'
     radio_text3(4)= ' (4) Distribution of durations- 40 bins'
     radio_text3(5)= ' (5) Distribution of durations- 60 bins'
   

iplotwid=19
iplothigh= 10
if(iplot.eq.6) then
	radio_text1(1)=' (1) Plot P(open) against concentration'
		radio_text1(2)=' (2) Plot Hill slope for (1) against concentration'
		radio_text1(3)=' (3) Plot Hill slope for (1) against P(open)'
	iplothigh= 9
	if(nlig.gt.1.and..not.hjmcalc) iplothigh= 12
else if (iplot.eq.1.or.iplot.eq.2) then
	iplothigh= 19
	 radio_text1(1)='(1) Show ''exact'' pdf only (2*exact + asymptotic)'
     radio_text1(2)='(2) Show exact AND asymptotic distributions '
     radio_text1(3)='(3) Show asymptotic distribution and its components also '

else if (iplot.eq.5) then
	radio_text1(1)=' (1) Distributions of open period-shut times'
	radio_text1(2)=' (2) Distributions of shut time-shut time'
	radio_text1(3)=' (3) Distributions of open period-open period'
	 if(id1.eq.1) then
	   text1='open period'
	   text2='shut time  '
	else if(id1.eq.2) then
	   text1='shut time  '
	   text2='shut time  '
	else if(id1.eq.2) then
	   text1='open period'
	   text2='open period'
	endif
	id2=3

	radio_text3(1)=' (1) '//text1(1:12)//' and following '//text2
	radio_text3(2)=' (2) '//text1(1:12)//' and preceding '//text2(1:12)
	radio_text3(3)=' (3) '//text1(1:12)//' and both following and preceding '//text2
	radio_text3(4)=' (4) '//text1(1:12)//' and following '//text2(1:12)//' MINUS'//&
    text1(1:12)//' and preceding '//text2
	iplothigh= 15
else if (iplot.eq.3) then
		radio_text1(1)=' (1) Show open times conditional on PRECEDING shut time'
		radio_text1(2)=' (2) Show open times conditional on FOLLOWING shut time'
		radio_text1(3)=' (3) Show open times conditional on EITHER shut time'
	iplothigh= 23
else if (iplot.eq.4) then
	radio_text1(1)=' (1) Show observations only for adjacent gap'
	radio_text1(2)=' (2) Show all'
	iplothigh= 23
endif
ippx=10
ippy=3
call intconv(iplot,cnum)
! Set up master window iplot
   iplot0 = gmCreateComplexDialogueBox(main,ippx, ippy, iplotwid, iplothigh, GALL, 'Options for plot='//cnum(1:2), &
   gmVpos=GTOP, gmIconFormat=GDLLICON, gmIconFile='Gee')
imx=0
imy=0

! Create main panel for form
   iplot_MainPanel=gmCreatePanel(iplot0, imx, imy, iplotwid, iplothigh, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmFillCol=220, gmScrollMode=Gnobars)

 if(iplot.eq.1.or.iplot.eq.3.or.iplot.eq.2.or.iplot.gt.10.or.iplot.eq.-1) then
	icbk=4810
	if(iplot.gt.10) icbk=50
	if(iplot.eq.3) i3=20
 	iradio3 = gmCreateRadioBox(iplot_mainPanel, 1, 1, iplotwid-2, 7, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmFillCol=41,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='Choose distribution', gmVpos=GTOP)

	do i=1,5
		iplot_Toggle3(i) = gmCreateToggleButton(iRadio3, 1, i , iplotwid-4, 1, radio_text3(i), 0, &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmcallback=icbk+i)
	enddo
	call gmSetToggleSwitch(	iplot_Toggle3(1),Gon)
endif
if(iplot.eq.1.or.iplot.eq.2) then
	
	iradio4 = gmCreateRadioBox(iplot_mainPanel, 1, 9, iplotwid-2, 5, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmFillCol=113,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='Choose distribution', gmVpos=GTOP)

	do i=1,3
		iplot_Toggle4(i) = gmCreateToggleButton(iRadio4, 1, i , iplotwid-4, 1, radio_text1(i), 0, &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)
	enddo
	call gmSetToggleSwitch(	iplot_Toggle4(1),Gon)
	
    iradio5 = gmCreateRadioBox(iplot_mainPanel, 1, 15, iplotwid-2, 1, gmType=GFREEFORM, gmBorderType=Gnone, &
    gmFillCol=220,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='Show the pdf without missed events', gmVpos=GTOP)
 if(iplot.ne.-1) then
	iplot_Toggle4(4) = gmCreateToggleButton(iradio5, iplotwid-6, 0 , 2, 1, 'Y', 1, &
	gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOp)
	iplot_Toggle4(5) = gmCreateToggleButton(iradio5,iplotwid-4 , 0, 2,1, 'N', 0, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)

 iradio6 = gmCreateRadioBox(iplot_mainPanel, 1, 16, iplotwid-2, 1, gmType=GFREEFORM, gmBorderType=Gnone, &
    gmFillCol=220,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='Rescale this pdf to unit area above t = tres', gmVpos=GTOP)


	iplot_Toggle4(6) = gmCreateToggleButton(iradio6, iplotwid-6, 0 , 2, 1, 'Y', 1, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOp)


	iplot_Toggle4(7) = gmCreateToggleButton(iradio6, iplotwid-4, 0 , 2, 1, 'N', 0, &
   gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)
endif
endif 
if(iplot.eq.3.or.iplot.eq.5.or.iplot.eq.4.or.iplot.eq.6) then
	ip=1
	if(iplot.eq.3) ip=9
	iradio1 = gmCreateRadioBox(iplot_mainPanel, 1, ip, iplotwid-2, 5, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmFillCol=113,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='Plot:', gmVpos=GTOP)
	jt=3
	if(iplot.eq.4) then
	 jt=2
	do i=1,2 
   iplot_Toggle1(i) = gmCreateToggleButton(iRadio1, 1, i , iplotwid-4, 1, radio_text1(i), 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)
    enddo
	 else
	do i=1,jt

   iplot_Toggle1(i) = gmCreateToggleButton(iRadio1, 1, i , iplotwid-4, 1, radio_text1(i), 0, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP,gmcallback=4900+i)
	enddo
	endif
	if(iplot.eq.3) then
		call gmSetToggleSwitch(	iplot_Toggle1(3),Gon)
	else
		call gmSetToggleSwitch(	iplot_Toggle1(1),Gon)
	endif
endif

if(iplot.eq.3.or.iplot.eq.4) then	
	i2=7
	if(iplot.eq.3) i2=15
	!i2=7
	iplot_Text2 = gmCreateTextEntry(iplot_mainPanel, 4, i2, 6, 1,'# of shut time ranges =', 255, gdisplay, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)


   iplot_Value2 = gmCreateValueEntry(iplot_mainPanel, 10, i2, 2, 1, nrange, 8, 0, gedit,  &
      gmType=GDECIMAL, gmBack1Col=0, gmBack2Col=0, gmTextCol=0, gmVpos=GTOP)
   xtext='Low t '//char(124)//'High t '
   if(iplot.eq.3) then
		nrowmax=1
		iha=3
   else
		nrowmax=20
		iha=7
   endif
   iplot_TxtArray2 = gmCreateTextArray(iplot_mainPanel, 4, i2+1, 11,iha , 2, nrowmax, gmXtext=xtext, &
      gmYtext='*digits', gmXjust=GCENTRE, gmYjust=GCENTRE, gmScrollMode=Gvertical, gmVpos=GTOP)
   call gmSetGuiGridMode(GOFF)
   call gmEnqCellAttribs(iplot_TxtArray2, 1, 1, arrayattribs)
   arrayattribs%height=22
   arrayattribs%width=90
   arrayattribs%backcol=101
   arrayattribs%display=gedit
    arrayattribs%ndp=3
   do i=1,nrowmax
   call gmSetCellAttribs(iplot_TxtArray2, 1, i, arrayattribs)
   enddo
   do i=1,nrange
		call gmSetCellSetting(iplot_TxtArray2, 1, i,gmValue=ylo(i))
   enddo
   arrayattribs%width=110
   arrayattribs%backcol=102
    do i=1,nrowmax
   call gmSetCellAttribs(iplot_TxtArray2, 2, i, arrayattribs)
    enddo
	do i=1,nrange
		call gmSetCellSetting(iplot_TxtArray2, 2, i,gmValue=yhi(i))
   enddo	
  call gmSetGuiGridMode(GOn)
endif

if(iplot.eq.4) then
	iradio3 = gmCreateRadioBox(iplot_mainPanel, 1, 17, iplotwid-2, 3, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmFillCol=41,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, &
	  gmTitle= 'Show also the continuous (theoretical) relationship?', gmVpos=GTOP)
    iplot_Toggle3(1) = gmCreateToggleButton(iRadio3, 4, 1 , 2, 1, 'Yes', 0, &
	gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)
	iplot_Toggle3(2) = gmCreateToggleButton(iRadio3, 7, 1 , 2, 1, 'No', 0, &
	gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)
	call gmSetToggleSwitch(	iplot_Toggle3(1),Gon)
endif


if(iplot.eq.5) then
i3=7
iradio3 = gmCreateRadioBox(iplot_mainPanel, 1, i3, 2, 6, gmType=GFREEFORM, gmBorderType=Gnone, &
      gmFillCol=41,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='',&
	   gmVpos=GTOP)
iradio3p = gmCreatepanel(iplot_mainPanel, 3, i3, iplotwid-4, 6, gmType=GFREEFORM, gmBorderType=Gnone, &
      gmFillCol=41,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='',&
	   gmVpos=GTOP)
do i=1,4
    iplot_Toggle3(i) = gmCreateToggleButton(iRadio3, 1, i , 1, 1, '', 0, &
	gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)
	iplot_Text(i) = gmCreateTextEntry(iradio3p,0 , i, iplotwid-5, 1,radio_text3(i),&
	 255, GDIsplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=0 ,gmScrollMode=Ghorizontal, gmVpos=GTOP)
enddo
	call gmSetToggleSwitch(	iplot_Toggle3(3),Gon)
endif
if(iplot.eq.6.and.nlig.gt.1.and..not.hjmcalc) then	!which ligand conc to be plotted
	iradio3 = gmCreateRadioBox(iplot_mainPanel, 1, 7, iplotwid-2, 4, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
    gmFillCol=41,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, &
	gmTitle= 'Ligand number to plotted', gmVpos=GTOP)
    do i=1,2
		iplot_Toggle3(i) = gmCreateToggleButton(iRadio3, 4, i , 2, 1, ligname(i), 0, &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)
	enddo
	
endif
if(iplot.le.6.and.iplot.ne.5) then
!iplotText = gmCreateTextEntry(iplot_MainPanel,2,1 , 3, 1,'Enter title:',&
!	 255, GDIsplay, gmBack1Col=0, gmBack2Col=0, &
 !     gmTextCol=0 ,gmScrollMode=Ghorizontal, gmhpos=gleft,gmVpos=Gbottom)
!iplot_Text(10) = gmCreateTextEntry(iplot_MainPanel,5 , 1, iplotwid-6, 1,title1,&
!	 255, Gedit, gmBack1Col=0, gmBack2Col=0, &
!      gmTextCol=0 ,gmScrollMode=Ghorizontal, gmhpos=gleft,gmVpos=Gbottom)
endif
iradio_Button1 = gmCreatePushButton(iplot_MainPanel,10,0 , 8, 1, 'Continue',gmType=GUSERDEFINED, &
   gmhpos=gleft,gmVpos=Gbottom,gmOffcol=38, gmTextCol=1,&
   gmcallback=icall)
   iradio_Button1 = gmCreatePushButton(iplot_MainPanel,1,0 , 8, 1, 'Exit plots', gmType=GUSERDEFINED,&
   gmhpos=gleft,gmVpos=Gbottom,gmOffcol=123, gmTextCol=1,&
   gmcallback=icall+1)
   call gmdrawwindow(iplot0)
end









