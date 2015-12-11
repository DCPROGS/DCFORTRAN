subroutine equations(Main,Eqs,EqsMainPanel,Toggle2,titmod,icallt)
!callbacks 201 -213
use menu_f90

integer :: Main
integer :: Eqs
integer :: EqsMainPanel
integer :: Graphics2,graphics2_1
integer :: Toggle2(20)
character*60 titmod(40)
type (GLIMIT) :: Graphics_frame,text_frame

call gsetmixedchars()

! Set up complex dialogue box Eqs child of Main
   Eqs = gmCreateComplexDialogueBox(Main, 20, 3, 24, 12, GALL, 'Equations', gmVpos=GTOP, &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmCallBack=-21,gmSelect=-22)

! Create main panel for form
   EqsMainPanel=gmCreatePanel(Eqs, 0, 0, 24, 12, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

! Create toggle button Toggle21 child of EqsMainPanel
iradEqsMainPanel= gmCreateRadioBox(eqsMainPanel, 0, 0, 24, 11, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle=' ', gmVpos=GTOP)
if(icallt.ne.8002) then
do i=1,5
   Toggle2(i) = gmCreateToggleButton(iradEqsMainPanel, 0, 2*i-1, 1, 1, '', 0, &
              	gmType=G3dRADIO, gmVpos=GTOP, gmExpand=GOFF, gmCallBAck=6050+i)
enddo	
TextButton=gmCreatePushButton(EqsMainPanel,0,0, 24, 1,'OK',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=6050)
endif		
! Create graphics frame Graphics4 child of EqsMainPanel
! Initialization code
! Define Graphics Frame limits
   Graphics_frame%xmin = 0.000000
   Graphics_frame%xmax = 12.000000
   Graphics_frame%ymin = 0.000000
   Graphics_frame%ymax = 11.000000

   text_frame%xmin = 0.000000
   text_frame%xmax = 8.000000
   text_frame%ymin = 0.000000
   text_frame%ymax = 11.000000

  ! call rgbdef(0,0.65,0.75,0.81)
   ! call rgbdef(0,0.616,0.721,0.78)
   Graphics2_1 = gmCreateGraphicsFrame(EqsMainPanel, 1, 0, 11, 11, &
         text_frame, &
   gmVpos=GTOP, gmExpand=GON)
   call gmClearGraphicsFrame(Graphics2_1)
   call gSetLineColour(1)
   call gSetCharFont(5)
   call gSetStrJustify(-1)	
 !  call gSetCharSize(0.26,0.40)
   call gSetCharSize(0.21,0.40)
   do i=1,10
		call gMoveTo2D(0.5,11.5-2*i)
		call gDisplayStr(titmod(i)(1:40))
   enddo
   call gFlushGraphics() 
   Graphics2 = gmCreateGraphicsFrame(EqsMainPanel, 12, 0, 12, 11, &
         Graphics_frame, &
   gmVpos=GTOP, gmExpand=GON)
   
   call gmClearGraphicsFrame(Graphics2)
      	
	call gSetLineColour(1)
	call gSetCharFont(1)
	call gSetStrJustify(-1)	
   call gSetCharSize(0.40,0.40)
  
   call gMoveTo2D(0.5,9.5)
   call gDisplayStr(&
   '£F003Y(x)=Y(0)+£A£F070S£B£I£I£I£F003i=1£B£E£E£E£En£Aa£Ii£Ax£Ei')
   	
   call gMoveTo2D(0.5,7.5)
   call gDisplayStr(&
   '£F003Y(x)=Y(0)+£A£F070S£B£I£I£I£F003r=1£B£E£E£E£Ek')
  
   call gMoveTo2D(5.5,8.)
   call gDisplayStr('£F003Y£Ir£E£Emax£A(x/K£ir£A)')
  
   call gMoveTo2D(5.5,7.)
   call gDisplayStr('£F0031+(x/K£ir£A)')
  
   call gMoveTo2D(5.0,7.7)
   call gDrawLineTo2D(10.0,7.7)

   call gMoveTo2D(0.5,5.5)
   call gDisplayStr(&
   '£F003Y(x)=Y(0)+£A£F070S£B£I£I£I£F003r=1£B£E£E£E£Ek')
   
   call gMoveTo2D(5.5,6.)
   call gDisplayStr('£F003Y£Ir£E£Emax£A(x/K£ir£A)£E£En£IH')
   
   call gMoveTo2D(5.5,5.)
   call gDisplayStr('£F0031+(x/K£ir£A)£E£En£IH')
   
   call gMoveTo2D(5.0,5.7)
   call gDrawLineTo2D(10.0,5.7)
   
   call gMoveTo2D(0.5,3.5)
   call gDisplayStr(&
'£F003Y(x)=Y(0)+£A£F070S£B£I£I£I£F003r=1£B£E£E£E£Ek')
   
   call gMoveTo2D(5.5,4.)
   call gDisplayStr('£F003Y£Ir£E£Emax£A(x/K£ir£A)')
   
   call gMoveTo2D(5.5,3.)
   call gDisplayStr('£F0031+(x/K£ir£A)')
   
   call gMoveTo2D(5.0,3.7)
   call gDrawLineTo2D(10.0,3.7)

   call gMoveTo2D(10.0,3.5)
   call gDisplayStr('£F003+ax')

   call gMoveTo2D(0.5,1.5)
   call gDisplayStr(&
   '£F003Y(x)=Y(0)+£A£F070S£B£I£I£I£F003r=1£B£E£E£E£Ek')
   
   call gMoveTo2D(5.5,2.)
   call gDisplayStr('£F003Y£Ir£E£Emax£A(x/K£ir£A)£E£En£IH')
   
   call gMoveTo2D(5.5,1.)
   call gDisplayStr('£F0031+(x/K£ir£A)£E£En£IH')
   
   call gMoveTo2D(5.0,1.7)
   call gDrawLineTo2D(10.0,1.7)
   
   call gMoveTo2D(10.0,1.5)
   call gDisplayStr('£F003+ax')

  
   

 call gmdrawwindow(eqs)
call gFlushGraphics() 
end