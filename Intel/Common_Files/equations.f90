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
   Eqs = gmCreateComplexDialogueBox(Main, 20, 3, 24, 22, GALL, 'Equations', gmVpos=GTOP, &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmCallBack=-21,gmSelect=-22)

! Create main panel for form
   EqsMainPanel=gmCreatePanel(Eqs, 0, 0, 24, 22, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

! Create toggle button Toggle21 child of EqsMainPanel
iradEqsMainPanel= gmCreateRadioBox(eqsMainPanel, 0, 0, 2, 22, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle=' ', gmVpos=GTOP)
if(icallt.ne.8002) then
do i=1,10
   Toggle2(i) = gmCreateToggleButton(iradEqsMainPanel, 0, 2*i-1, 1, 1, '', 0, &
              	gmType=G3dRADIO, gmVpos=GTOP, gmExpand=GOFF, gmCallBAck=6050+i)
enddo	
TextButton=gmCreatePushButton(EqsMainPanel,0,21, 24, 1,'OK',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=GTOp, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=6050)
endif		
! Create graphics frame Graphics4 child of EqsMainPanel
! Initialization code
! Define Graphics Frame limits
   Graphics_frame%xmin = 0.000000
   Graphics_frame%xmax = 12.000000
   Graphics_frame%ymin = 0.000000
   Graphics_frame%ymax = 21.000000

   text_frame%xmin = 0.000000
   text_frame%xmax = 8.000000
   text_frame%ymin = 0.000000
   text_frame%ymax = 21.000000

  ! call rgbdef(0,0.65,0.75,0.81)
   ! call rgbdef(0,0.616,0.721,0.78)
   Graphics2_1 = gmCreateGraphicsFrame(EqsMainPanel, 1, 0, 11, 21, &
         text_frame, &
   gmVpos=GTOP, gmExpand=GON)
   call gmClearGraphicsFrame(Graphics2_1)
   call gSetLineColour(1)
   call gSetCharFont(5)
   call gSetStrJustify(-1)	
 !  call gSetCharSize(0.26,0.40)
   call gSetCharSize(0.21,0.40)
   do i=1,10
		call gMoveTo2D(0.5,21.5-2*i)
		call gDisplayStr(titmod(i)(1:40))
   enddo
   call gFlushGraphics() 
   Graphics2 = gmCreateGraphicsFrame(EqsMainPanel, 12, 0, 12, 21, &
         Graphics_frame, &
   gmVpos=GTOP, gmExpand=GON)
   
   call gmClearGraphicsFrame(Graphics2)
      	
	call gSetLineColour(1)
	call gSetCharFont(1)
	call gSetStrJustify(-1)	
   call gSetCharSize(0.40,0.40)
   call gMoveTo2D(0.5,19.5)
   call gDisplayStr(&
   '£F003Y(x)=Y(0)+£A£F070S£B£I£I£I£F003i=1£B£E£E£E£En£Aa£Ii£Ax£Ei')
   	
   call gMoveTo2D(0.5,17.5)
   call gDisplayStr(&
   '£F003Y(x)=Y(0)+£A£F070S£B£I£I£I£F003r=1£B£E£E£E£Ek')
   call gMoveTo2D(5.5,18.)
   call gDisplayStr('£F003Y£Ir£E£Emax£A(x/K£ir£A)')
   call gMoveTo2D(5.5,17.)
   call gDisplayStr('£F0031+(x/K£ir£A)')
   call gMoveTo2D(5.0,17.7)
   call gDrawLineTo2D(10.0,17.7)

   call gMoveTo2D(0.5,15.5)
   call gDisplayStr(&
   '£F003Y(x)=Y(0)+£A£F070S£B£I£I£I£F003r=1£B£E£E£E£Ek')
   call gMoveTo2D(5.5,16.)
   call gDisplayStr('£F003Y£Ir£E£Emax£A(x/K£ir£A)£E£En£IH')
   call gMoveTo2D(5.5,15.)
   call gDisplayStr('£F0031+(x/K£ir£A)£E£En£IH')
   call gMoveTo2D(5.0,15.7)
   call gDrawLineTo2D(10.0,15.7)
   
   call gMoveTo2D(0.5,13.5)
   call gDisplayStr(&
'£F003Y(x)=Y(0)+£A£F070S£B£I£I£I£F003r=1£B£E£E£E£Ek')
   call gMoveTo2D(5.5,14.)
   call gDisplayStr('£F003Y£Ir£E£Emax£A(x/K£ir£A)')
   call gMoveTo2D(5.5,13.)
   call gDisplayStr('£F0031+(x/K£ir£A)')
   call gMoveTo2D(5.0,13.7)
   call gDrawLineTo2D(10.0,13.7)

   call gMoveTo2D(10.0,13.5)
   call gDisplayStr('£F003+ax')

   call gMoveTo2D(0.5,11.5)
   call gDisplayStr(&
   '£F003Y(x)=Y(0)+£A£F070S£B£I£I£I£F003r=1£B£E£E£E£Ek')
   call gMoveTo2D(5.5,12.)
   call gDisplayStr('£F003Y£Ir£E£Emax£A(x/K£ir£A)£E£En£IH')
   call gMoveTo2D(5.5,11.)
   call gDisplayStr('£F0031+(x/K£ir£A)£E£En£IH')
   call gMoveTo2D(5.0,11.7)
   call gDrawLineTo2D(10.0,11.7)
   call gMoveTo2D(10.0,11.5)
   call gDisplayStr('£F003+ax')

   call gMoveTo2D(0.5, 9.5)
   call gDisplayStr('£F003Y=£AY£B£E£E-£A(x/x£Io£A)£En')

   call gMoveTo2D(0.5, 7.5)
   call gDisplayStr('£F003Y=Y(£F070U£F003)+Y(0)')
   call gMoveTo2D(5.0, 7.7)
   call gDrawLineTo2D(10.0, 7.7)
   call gMoveTo2D(6.5, 8.)
   call gDisplayStr('£F0031+C£IA')
   call gMoveTo2D(5.5, 7.)	
   call gDisplayStr('£F0031+C£IA£A+x/K£IB')

   

   call gMoveTo2D(0.5, 5.5)
   call gDisplayStr('£F003Y(t)=Y£Imax£Aexp(-(K£I+1£Ac+K£I-1£A)x)')

   call gMoveTo2D(0.5,3.5)
   call gDisplayStr(&
 '£F003Y=Y(£F070U£F003)+£A£F070S£B£I£I£I£F003i=1£B£E£E£E£En£Aa£Ii£Aexp'&
      //'(-x/£F023t£F003£Ii£A)')

   call gMoveTo2D(0.5,1.5)
   call gDisplayStr(&
   '£F003Y=Y(£F070U£F003)+£A£F070S£B£I£I£I£F003i=1£B£E£E£E£En'&
   //'£Aa£Ii£A(1-1/£F023m£F003£Ii£A)£Ex')

  ! call gMoveTo2D(11.9, 0.0)
  ! call gDrawLineTo2D(11.9, 21.0)

! Create toggle button Toggle21 child of EqsMainPanel
!   Toggle2(1) = gmCreateToggleButton(EqsMainPanel, 0, 1, 5, 1, 'Polinomial', 0, &
!              	gmType=G3DCHECKBOX, gmAccel=0, &
!              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
!              	gmVpos=GTOP, gmExpand=GOFF, gmCallBAck=201)

! Create toggle button Toggle22 child of EqsMainPanel
!   Toggle2(2) = gmCreateToggleButton(EqsMainPanel, 0, 3, 5, 1, 'Langmuir', 0, &
!              	gmType=G3DCHECKBOX, gmAccel=0, &
!              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
!              	gmVpos=GTOP, gmExpand=GOFF, gmCallBAck=202)

! Create toggle button Toggle23 child of EqsMainPanel
!   Toggle2(3) = gmCreateToggleButton(EqsMainPanel, 0, 5, 5, 1, 'Hill', 0, &
!              	gmType=G3DCHECKBOX, gmAccel=0, &
!              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
!              	gmVpos=GTOP, gmExpand=GOFF, gmCallBAck=203)

! Create toggle button Toggle24 child of EqsMainPanel
!   Toggle2(4) = gmCreateToggleButton(EqsMainPanel, 0, 7, 5, 1, 'Langmuir + Line', 0, &
!              	gmType=G3DCHECKBOX, gmAccel=0, &
!              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
!              	gmVpos=GTOP, gmExpand=GOFF, gmCallBAck=204)

! Create toggle button Toggle25 child of EqsMainPanel
!   Toggle2(5) = gmCreateToggleButton(EqsMainPanel, 0, 9, 5, 1, 'Hill + Line', 0, &
!              	gmType=G3DCHECKBOX, gmAccel=0, &
!              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
!              	gmVpos=GTOP, gmExpand=GOFF, gmCallBAck=205)

! Create toggle button Toggle26 child of EqsMainPanel
!   Toggle2(6) = gmCreateToggleButton(EqsMainPanel, 0, 11, 5, 1, 'Power function', 0, &
!              	gmType=G3DCHECKBOX, gmAccel=0, &
!              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
!              	gmVpos=GTOP, gmExpand=GOFF, gmCallBAck=206)

! Create toggle button Toggle27 child of EqsMainPanel
!   Toggle2(7) = gmCreateToggleButton(EqsMainPanel, 0, 13, 5, 1, 'Binding inhibition', 0, &
!              	gmType=G3DCHECKBOX, gmAccel=0, &
!              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
!              	gmVpos=GTOP, gmExpand=GOFF, gmCallBAck=207)

! Create toggle button Toggle28 child of EqsMainPanel
!   Toggle2(8) = gmCreateToggleButton(EqsMainPanel, 0, 15, 6, 1, 'Exponential binding', 0, &
!              	gmType=G3DCHECKBOX, gmAccel=0, &
!              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
!              	gmVpos=GTOP, gmExpand=GOFF, gmCallBAck=208)

! Create toggle button Toggle29 child of EqsMainPanel
!   Toggle2(9) = gmCreateToggleButton(EqsMainPanel, 0, 17, 6, 1, 'Sum of exponentials', 0, &
!              	gmType=G3DCHECKBOX, gmAccel=0, &
!              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
!              	gmVpos=GTOP, gmExpand=GOFF, gmCallBAck=209)

! Create toggle button Toggle30 child of EqsMainPanel
!   Toggle2(10) = gmCreateToggleButton(EqsMainPanel, 0, 19, 6, 1, 'Sum of geometrics', 0, &
!              	gmType=G3DCHECKBOX, gmAccel=0, &
!              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
!              	gmVpos=GTOP, gmExpand=GOFF, gmCallBAck=210)

! Create toggle button Toggle31 child of EqsMainPanel
!   Toggle2(11) = gmCreateToggleButton(EqsMainPanel, 18, 1, 6, 1, 'Exponential voltage', 0, &
!              	gmType=G3DCHECKBOX, gmAccel=0, &
!              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
!              	gmVpos=GTOP, gmExpand=GOFF, gmCallBAck=211)

! Create toggle button Toggle32 child of EqsMainPanel
!   Toggle2(12) = gmCreateToggleButton(EqsMainPanel,18 , 3, 6, 1, 'P(open) curves', 0, &
!              	gmType=G3DCHECKBOX, gmAccel=0, &
!              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
!              	gmVpos=GTOP, gmExpand=GOFF, gmCallBAck=212)

! Create toggle button Toggle33 child of EqsMainPanel
!   Toggle2(13) = gmCreateToggleButton(EqsMainPanel, 18, 5, 4, 1, 'I/V curves', 0, &
!              	gmType=G3DCHECKBOX, gmAccel=0, &
!              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
!              	gmVpos=GTOP, gmExpand=GOFF, gmCallBAck=213)

 call gmdrawwindow(eqs)
call gFlushGraphics() 
end