Program consam
USE DFLIB
use gino_f90
use menu_f90


INTERFACE
INTEGER FUNCTION adcdisk [stdcall,alias:'_AdcDisk@52'] (adcdfil, ioff, nbyte, cnchan, cctrig, &
   ndiv1, ndiv2, CancelFunc, cdir, nbuffer,icounter, ibutton,sec)
CHARACTER*(*) adcdfil,cnchan,cctrig,cdir
!DEC$ ATTRIBUTES REFERENCE :: adcdfil,cnchan,cctrig
INTEGER*4 nbyte,ndiv1,ndiv2,ioff,nbuffer, icounter,ibutton
real*4 sec
!INTEGER, POINTER :: CancelFunc
INTEGER :: CancelFunc
END FUNCTION adcdisk

!INTEGER FUNCTION CancelFunc [stdcall, alias:'_CancelFunc@4'] (lDone)
!INTEGER*8 lDone
!END FUNCTION CancelFunc

INTEGER*2 FUNCTION U14Open1401[c,alias:'_U14Open1401@4'](n1401)
!DEC$ ATTRIBUTES DLLIMPORT :: U14Open1401     
INTEGER*2 n1401
!DEC$ ATTRIBUTES VALUE :: n1401   
END FUNCTION U14Open1401

INTEGER*2 FUNCTION U14Close1401[c,alias:'_U14Close1401@4'](oErr)
!DEC$ ATTRIBUTES DLLIMPORT :: U14Open1401     
INTEGER*2 oErr
!DEC$ ATTRIBUTES VALUE :: oErr  
END FUNCTION U14Close1401

INTEGER*2 FUNCTION U14TypeOf1401[c,alias:'_U14TypeOf1401@4'](oErr)
!DEC$ ATTRIBUTES DLLIMPORT :: U14Open1401     
INTEGER*2 oErr
!DEC$ ATTRIBUTES VALUE :: oErr   
END FUNCTION U14TypeOf1401

END INTERFACE

CHARACTER adcfil*30,cnchan*11,cctrig*11,charint*11,adcdfil*50
!INTEGER, POINTER :: CancelFunc => NULL()
INTEGER :: CancelFunc = 1
INTEGER*4 nbyte,ndiv1,ndiv2,ioff

! Widget identifier definitions

integer :: Form1
integer :: Form1MainPanel
integer :: Panel1
integer :: Panel1_1
integer :: Panel1_1_1
integer :: Panel1_2
integer :: Static0
integer :: Static1
integer :: Static1_1 
integer :: Static1_2 
integer :: Static1_3 
integer :: Static1_4 
integer :: Static1_5 
integer :: Static1_6 
integer :: Static1_7 
integer :: Static1_8 
integer :: Static1_9 
integer :: Static1_10 
integer :: Static1_11 
integer :: Static1_12
integer :: Static1_13
integer :: Static1_14

integer :: Edit1_1 
integer :: Edit1_2 
integer :: val1_3 
integer :: Edit1_4 
integer :: val1_6 
integer :: val1_7 
integer :: Edit1_10 
integer :: val1_11 
integer :: val1_25 
integer :: val1_12 
integer :: Toggle1_5(5)
integer :: Toggle1_8(2)
integer :: Button1(5)
integer :: Toggle1_9(3)
integer :: Button1_10
integer :: Edit2_2 
integer :: Edit2_3a 
integer :: Edit2_3 
integer :: Edit2_4 
integer :: Edit2_5
integer :: Edit2_6 
integer :: Edit2_7 
integer :: Edit2_8 
integer :: Edit2_9 

integer :: Edit2_12 
integer :: Edit2_13a 
integer :: Edit2_13 
integer :: Edit2_14 
integer :: Edit2_15
integer :: Edit2_16 
integer :: Edit2_17 
integer :: Edit2_18 
integer :: Toggle2_6(2)
integer :: Toggle2_16(2)
integer :: Panel2
integer :: Static2_00
integer :: Static2_01
integer :: Static2_1 
integer :: Static2_2 
integer :: Static2_3 
integer :: Static2_3a 
integer :: Static2_4 
integer :: Static2_5 
integer :: Static2_6 
integer :: Static2_7 
integer :: Static2_8 
integer :: Static2_9 
integer :: Static2_11 
integer :: Static2_12
integer :: Static2_13 
integer :: Static2_14 
integer :: Static2_15 
integer :: Static2_16 
integer :: Static2_17 
integer :: Static2_18 
integer :: Static2_19 

real :: tedit2_2=0.5, tedit2_12=0.5
real :: tedit2_3a=50, tedit2_13a=50
real :: tedit2_3=10, tedit2_13=10
real :: tedit2_4=1.0, tedit2_14=1.0
real :: tedit2_5=100000., tedit2_15=100000.
real :: tedit2_7=1, tedit2_17=1
real :: tedit2_8=1, tedit2_18=1
integer :: graph
type (GLIMIT)    :: limits 
type (GWIDGET)   :: grframe

integer :: Sliderh1
type(GARRAYCELL) arrayattribs
integer :: ISTBAR(32)
integer :: callid
type (GWIDGET) :: Slider_attribs
type (GACTION) :: actlst
character*30 tEdit1_1,tedit1_2,tedit1_4,tedit1_10
character*80 wdir,cdir



integer :: Form3
integer :: Form3MainPanel
integer :: Panel3
integer :: Static3_1 
integer :: Static3_2 
integer :: Static3_3 
integer :: Static3_4 
integer :: Static3_5 
integer :: Static3_6
integer :: Button3(2)

integer :: Form4,Form2
integer :: Form4MainPanel
integer :: Panel4
integer :: Static4 
integer :: Button4

integer :: Form5
integer :: Form5MainPanel
integer :: Panel5
integer :: Static5 
integer :: Toggle5(2)

integer :: Form6
integer :: Form6MainPanel
integer :: Panel6
integer :: Static6_1
integer :: Toggle6(2) 

integer :: Form7
integer :: Form7MainPanel
integer :: Panel7
integer :: Static7_1
integer :: Static7_2
integer :: Static7_3
integer :: Static7_4


character*11 cdate,expdate,CNPATCH
character*14 ptype(5)
character*24 tapeID
character*6  defname
character*70 title,messg
character cs*3,adctime*8,qdate*9, dname*2
CHARACTER*30 SDIR,SFILT,SFILE
character*50 PFILE
character*11 cnbyte,cnd1,cnd2
integer nprime(1900)	!holds primes up to 16381 (see PRIMGEN.FOR)
integer*2 idt,inchan,id1,id2,irev,iver
integer*4 ilen


integer :: iErr
integer*2 oErr,n1401,iType,sErr



logical sampled, start1401

common/sendvalue/ICOUNTER,Form1,Form1MainPanel,Panel1
nbuffer=400000
inchan=1
nchan=1
val1_25=0
call DATE(qdate)
   cdate=qdate(1:7)//'20'//qdate(8:9)
   if(qdate(4:6).eq.'JAN') dname='01'
   if(qdate(4:6).eq.'FEB') dname='02'
   if(qdate(4:6).eq.'MAR') dname='03'
   if(qdate(4:6).eq.'APR') dname='04'
   if(qdate(4:6).eq.'MAY') dname='05'
   if(qdate(4:6).eq.'JUN') dname='06'
   if(qdate(4:6).eq.'JUL') dname='07'
   if(qdate(4:6).eq.'AUG') dname='08'
   if(qdate(4:6).eq.'SEP') dname='09'
   if(qdate(4:6).eq.'OCT') dname='10'
   if(qdate(4:6).eq.'NOV') dname='11'
   if(qdate(4:6).eq.'DEC') dname='12'

   adcfil=qdate(1:2)//dname//qdate(8:9)//'C'//'.ssd'
   cdate=qdate(1:7)//'20'//qdate(8:9)
   
   call TIME(adctime)

 idir=-1
! Initialise Gino, Device & Menu
   call gOpenGino
   call gGuiwin
   call gmInitializeMenu
   call gmSetGuiGridMode(GON)


! Set up master window Form1
	Form1 = gmCreateMasterWindow(1, 2, 36, 27, GALL, 'Program Consam', &
              	gmVpos=GTOP, gmIconFormat=GDLLICON,gmIconFile='MBIG1005')

! Create main panel for form
   Form1MainPanel=gmCreatePanel(Form1, 0, 0, 36, 27, &
              	gmHpos=GLEFT, gmVpos=Gmiddle, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=15, gmFillBorder=0)

! Create panel Panel1 child of Form1MainPanel
   Panel1 = gmCreatePanel(Form1MainPanel, 0, 0, 36, 27, &
              	gmType=GCHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=12, gmFillBorder=0, &
              	gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)

  ! Create text entry Static0 child of Panel1
   Static0 = gmCreateTextEntry(Panel1, 0, 0, 24, 1,'       CONTINUOUS SAMPLING  OF ADC DATA TO DISK ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=2, gmBack2Col=0, gmTextCol=4, &
              	gmVpos=GTOP, gmExpand=GOFF)
				 
	icon_star1= gmCreateIcon(Panel1,2,25,1,1,'MSML1078',GDLLICON,0,gmCallback=13,GMHELP='Trigger')
    icon_start1= gmCreateIcon(Panel1,1,25,1,1,'MSML1061',GDLLICON,0,gmCallback=14, gmhelp='Start')
!    icon_stop1= gmCreateIcon(Panel1,2,25,1,1,'MSML1080',GDLLICON,0,gmCallback=15)
	icon_help1= gmCreateIcon(Panel1,3,25,1,1,'MSML1064',GDLLICON,0,gmCallback=100, gmhelp='Help')
    icon_exit1= gmCreateIcon(Panel1,4,25,1,1,'MSML1007',GDLLICON,0,gmCallback=-1)


! Create panel Panel1 child of Form1MainPanel
   Panel1_2 = gmCreatePanel(Panel1, 18, 1, 5, 1, &
              	gmType=GPRESSED, &
              	gmLineCol=GRED, gmLineBorder=0, gmFillCol=GWHITE, gmFillBorder=GRED, &
              	gmHpos=GLEFT, gmVpos=GTOP, gmExpand=GOFF)
   ICOUNTER = gmCreateValueEntry(Panel1, 18, 1, 5, 1,COUNTVAL, 32768,2, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=115)
	
! Create panel Panel1 child of Form1MainPanel
   Panel1_1 = gmCreatePanel(Panel1, 1, 2, 22, 5, &
              	gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=14, &                                                 
				gmFillBorder=0,&
              	gmHpos=GLEFT, gmVpos=GTOP, gmExpand=GOFF)


! Create text entry Static1_13 child of Panel6
   Static1_13 = gmCreateTextEntry(Panel1_1, 1, 1, 20, 1,'CONSAM file ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
				gmHpos=GCENTRE, gmVpos=GTOP, gmExpand=GOFF)
               


! Create text entry Static1_14 child of Panel6
   Static1_14 = gmCreateTextEntry(Panel1_1, 1, 2, 20, 1,'WAITING TO FILL IN DATA; To start press the button "7" ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmHpos=GCENTRE, gmVpos=GTOP, gmExpand=GOFF)
   ! Create panel Panel1 child of Form1MainPanel
    val1_25 = gmCreateValueEntry(Panel1_1, 18, 0, 4, 1,tval1_25, 32768,0, GDIsplay, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmHpos=Gleft,gmVpos=Gbottom, gmExpand=GOFF)

   Panel1_1_1 = gmCreatePanel(Panel1_1, 1, 3, 20, 1, &
              	gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=12, gmFillBorder=0, &
              	gmHpos=GCENTRE, gmVpos=GTOP, gmExpand=GOFF)

       
		  limits%xmin=0.
		  limits%xmax=20.
		  limits%ymin=0.
		  limits%ymax=1.
		  graph=gmCreateGraphicsFrame(Panel1_1_1,0,0,20,1,limits, &
          gmHpos=GCENTRE, gmVpos=GTOP, gmExpand=GOFF, &
                                gmCallback=-4)
		  call gmEnqWidgetInfo(graph,grframe)
          call gmActivateGraphicsFrame(graph)
          call gFillRect(GSOLID,15,limits)
		  call gsetlinecolour(GBLACK)
          call gmoveto2d(0.,0.)
          call gdrawlineto2d(20.,0.)
          call gdrawlineto2d(20.,1.)                                                               
          call gdrawlineto2d(0.,1.)
          call gdrawlineto2d(0.,0.)
		  call gFlushGraphics


! Create text entry Static1 child of Panel1
   Static1 = gmCreateTextEntry(Panel1, 1, 7, 8, 1,'Experiment / Patch details ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static1_1 child of Panel1
   Static1_1 = gmCreateTextEntry(Panel1, 1, 8, 10, 1,'Date [dd-mmm-yyyy; i.e. 01-Jan-2001] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
! Create text entry Edit1_1 child of Panel1
  
   Edit1_1 = gmCreateTextEntry(Panel1, 11, 8, 12, 1,tedit1_1, 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=101)


! Create text entry Static1_2 child of Panel1
   Static1_2 = gmCreateTextEntry(Panel1, 1, 9, 4, 1,'Tape details ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
! Create text entry Edit1-2 child of Panel1
   
   Edit1_2 = gmCreateTextEntry(Panel1, 5, 9, 18, 1,tedit1_2, 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=102)

! Create text entry Static1_3 child of Panel1
   Static1_3 = gmCreateTextEntry(Panel1, 1, 10, 4, 1,'Patch number ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit1_3 child of Panel1
  	
   val1_3 = gmCreateValueEntry(Panel1, 5, 10, 18, 1,tval1_3, 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=103)

! Create text entry Static1_4 child of Panel1
   Static1_4 = gmCreateTextEntry(Panel1, 1, 11, 4, 1,'Patch name ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit1_4 child of Panel1
   
   Edit1_4 = gmCreateTextEntry(Panel1, 5, 11, 18, 1,tedit1_4, 32768, GEDIT, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=104)


! Create text entry Static1_5 child of Panel1
   Static1_5 = gmCreateTextEntry(Panel1, 1, 12, 3, 1,'Patch type ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)


! Create toggle button Toggle1_5 child of Panel1
   Toggle1_5(1) = gmCreateToggleButton(Panel1, 1, 13, 4, 1, 'Outside-out', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=151)

! Create toggle button Toggle1_5 child of Panel1
   Toggle1_5(2) = gmCreateToggleButton(Panel1, 5, 13, 4, 1, 'Inside-out', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=152)

! Create toggle button Toggle1_5 child of Panel1
   
   Toggle1_5(3) = gmCreateToggleButton(Panel1, 9, 13, 4, 1, 'Cell-attached', itogon, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=153)

! Create toggle button Toggle1_5 child of Panel1
   Toggle1_5(4) = gmCreateToggleButton(Panel1, 14, 13, 4, 1, 'Whole-cell', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=154)

! Create toggle button Toggle1_5 child of Panel1
   Toggle1_5(5) = gmCreateToggleButton(Panel1, 18, 13, 6, 1, 'Simulated data', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=155)

! Create text entry Static1_6 child of Panel1
   Static1_6 = gmCreateTextEntry(Panel1, 1, 14, 7, 1,'Membrane potential [mV] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit16 child of Panel1
   
   val1_6 = gmCreateValueEntry(Panel1, 8, 14, 15, 1,tval1_6, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=106)

! Create text entry Static1_7 child of Panel1
   Static1_7 = gmCreateTextEntry(Panel1, 1, 15, 5, 1,'Temperature [o C] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit1_7 child of Panel1
   
   val1_7 = gmCreateValueEntry(Panel1, 7, 15, 16, 1,tval1_7, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=107)

! Create text entry Static1_8 child of Panel1
   Static1_8 = gmCreateTextEntry(Panel1, 1, 17, 4, 1,'Sampling ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle button Toggle1_8 child of Panel1
   Toggle1_8(1) = gmCreateToggleButton(Panel1, 1, 18, 10, 1, 'Sample one channel only  (ADC0)', itogon, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=181)

! Create toggle button Toggle1-8 child of Panel1
   Toggle1_8(2) = gmCreateToggleButton(Panel1, 12, 18, 11, 1, 'Sample two channels  (ADC0 and ADC1)', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=182)


! Create text entry Static1_11 child of Panel1
   Static1_11 = gmCreateTextEntry(Panel1, 1, 20, 7, 1,'Sampling rate (integer) [ Hz] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit1_11 child of Panel1
   val1_11 = gmCreateValueEntry(Panel1, 8, 20, 15, 1,tval1_11, 32768,0, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=111)

! Create text entry Static1_12 child of Panel1
   Static1_12 = gmCreateTextEntry(Panel1, 1, 21, 7, 1,'Sample duration [seconds] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit1_12 child of Panel1
   val1_12 = gmCreateValueEntry(Panel1, 8, 21, 15, 1,tval1_12, 32768,2, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=112)


! Create button Button1_10 child of Panel1
!   Button1_10 = gmCreatePushButton(Panel1, 3, 24, 8, 1, 'Save as CONSAM file', &
!              	gmType=GSTANDARD, gmAccel=0, &
!              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=110)

! Create button Button1_1 child of Panel1
   Button1(1) = gmCreatePushButton(Panel1, 16, 23, 4, 1, 'Clear all', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=11)

! Create button Button1_2 child of Panel1
   Button1(2) = gmCreatePushButton(Panel1,20 , 23, 2, 1, 'Quit', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=12)
 
! Create button Button1_3 child of Panel1
   Button1(3) = gmCreatePushButton(Panel1, 8, 23, 8, 1, 'Wait for trigger (on Ev 4)', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=13)

! Create button Button1_4 child of Panel1
   Button1(4) = gmCreatePushButton(Panel1, 2, 23, 6, 1, 'Continue', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=14)

! Create button Button1_5 child of Panel1
!   Button1(5) = gmCreatePushButton(Panel1, 10, 25, 5, 1, 'Stop sampling', &
!              	gmType=GSTANDARD, gmAccel=0, &
!              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
!              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=15,gmSelect=-15)

iStatic1_14 = gmCreateTextEntry(Panel1, 4, 25, 22, 1,'TO STOP SAMPLING PRESS THE ESCAPE "Esc" KEY ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=Gleft, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)
!========================================

				! Create panel Panel2 child of Form2MainPanel
   Panel2 = gmCreatePanel(Panel1, 24, 0, 12, 27, &
              	gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=14, gmFillBorder=0, &
              	gmHpos=Gleft, gmVpos=Gtop, gmExpand=GOFF)
! Create text entry Static1_9 child of Panel1
   Static1_9 = gmCreateTextEntry(Panel2, 4, 1, 4, 1,'CALIBRATION ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle button Toggle1_9 child of Panel1
   Toggle1_9(1) = gmCreateToggleButton(Panel2, 1, 2, 7, itogon, 'Overall calibration factor', itogon, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=191)

! Create toggle button Toggle1_9 child of Panel1
   Toggle1_9(2) = gmCreateToggleButton(Panel2, 1, 3, 7, 1, 'Separate clamp gain', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=192)

! Create toggle button Toggle1_9 child of Panel1
   Toggle1_9(3) = gmCreateToggleButton(Panel2, 1, 4, 5, 1, 'No calibration', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=5, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=193)



! Create text entry Static2_00 child of Panel2
   Static2_00 = gmCreateTextEntry(Panel2, 4, 5, 4, 1,'ADC0 ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static2_2 child of Panel2
   Static2_2 = gmCreateTextEntry(Panel2, 1, 6, 6, 1,'Calibration [V/pA] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
   
! Create text entry Edit2_2 child of Panel2
   Edit2_2 = gmCreateValueEntry(Panel2, 7, 6, 4, 1,tedit2_2, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=202)

! Create text entry Static2_2 child of Panel2
   Static2_3a = gmCreateTextEntry(Panel2, 1, 7, 6, 1,'Clamp setting [mV/pA] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)
   
! Create text entry Edit2_2 child of Panel2
   Edit2_3a = gmCreateValueEntry(Panel2, 7, 7, 7, 1,tedit2_3a, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=2031)

! Create text entry Static2_3 child of Panel2
   Static2_3 = gmCreateTextEntry(Panel2, 1, 8, 4, 1,'Amplifier gain ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_3 child of Panel2
   Edit2_3 = gmCreateValueEntry(Panel2, 5, 8, 6, 1,tedit2_3, 32768, 3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=203)

! Create text entry Static2_4 child of Panel2
   Static2_4 = gmCreateTextEntry(Panel2, 1, 9, 4, 1,'Error factor ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_4 child of Panel2
   Edit2_4 = gmCreateValueEntry(Panel2, 5, 9, 6, 1,tedit2_4, 32768, 3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=204)

! Create text entry Static2_5 child of Panel2
   Static2_5 = gmCreateTextEntry(Panel2, 1, 10, 6, 1,'Filter setting (-3dB) [Hz] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_6 child of Panel2
   Edit2_5 = gmCreateVAlueEntry(Panel2, 7, 10, 4, 1,tedit2_5, 32768, 3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=205)

! Create text entry Static2_6 child of Panel2
   Static2_6 = gmCreateTextEntry(Panel2, 1, 11, 5, 1,'Sample from tape ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle button Toggle2_6 child of Panel2
   Toggle2_6(1) = gmCreateToggleButton(Panel2, 1, 12, 2, 1, 'Yes', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=261)

! Create toggle button Toggle2_6 child of Panel2
   Toggle2_6(2) = gmCreateToggleButton(Panel2, 4, 12, 2, 1, 'No', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=262)

! Create text entry Static2_7 child of Panel2
   Static2_7 = gmCreateTextEntry(Panel2, 1, 13, 4, 1,'Tape recorder ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_7 child of Panel2
   Edit2_7 = gmCreateValueEntry(Panel2, 5, 13, 6, 1,tedit2_7, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=207)

! Create text entry Static2_8 child of Panel2
   Static2_8 = gmCreateTextEntry(Panel2, 1, 14, 5, 1,'Tape speed factor ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_8 child of Panel2
   Edit2_8 = gmCreateValueEntry(Panel2, 6, 14, 5, 1,tedit2_8, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=208)
!
!	ADC1
!

! Create text entry Static2_01 child of Panel2
   Static2_01 = gmCreateTextEntry(Panel2, 4, 15, 4, 1,'ADC1 ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static2_12 child of Panel2
   Static2_12 = gmCreateTextEntry(Panel2, 1, 16, 6, 1,'Calibration [V/pA] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_12 child of Panel2
   Edit2_12 = gmCreateValueEntry(Panel2, 7, 16, 4, 1,tedit2_12, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=212)

! Create text entry Static2_12 child of Panel2
   Static2_13a = gmCreateTextEntry(Panel2, 1, 17, 6, 1,'Clamp setting [mV/pA] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_12 child of Panel2
   Edit2_13a = gmCreateValueEntry(Panel2, 7, 17, 4, 1,tedit2_13a, 32768,3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=2131)

! Create text entry Static2_13 child of Panel2
   Static2_13= gmCreateTextEntry(Panel2, 1, 18, 4, 1,'Amplifier gain ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_13 child of Panel3
   Edit2_13 = gmCreateValueEntry(Panel2, 5, 18, 6, 1,tedit2_13, 32768, 3,GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=213)

! Create text entry Static2_14 child of Panel2
   Static2_14 = gmCreateTextEntry(Panel2, 1, 19, 4, 1,'Error factor ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_14 child of Panel2
   Edit2_14 = gmCreateValueEntry(Panel2, 5, 19, 6, 1,tedit2_14, 32768, 3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=214)

! Create text entry Static2_15 child of Panel2
   Static2_15 = gmCreateTextEntry(Panel2, 1, 20, 6, 1,'Filter setting (-3dB) [Hz] ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_15 child of Panel2
   Edit2_15 = gmCreateValueEntry(Panel2, 7, 20, 4, 1,tedit2_15, 32768, 3,GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=215)

! Create text entry Static2_16 child of Panel2
   Static2_16 = gmCreateTextEntry(Panel2, 1, 21, 5, 1,'Sample from tape ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create toggle button Toggle2_16 child of Panel2
   Toggle2_16(1) = gmCreateToggleButton(Panel2, 1, 22, 2, 1, 'Yes', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=261) 
				

! Create toggle button Toggle2_16 child of Panel2
   Toggle2_16(2) = gmCreateToggleButton(Panel2, 4, 22, 2, 1, 'No', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=262)

! Create text entry Static2_17 child of Panel2
   Static2_17 = gmCreateTextEntry(Panel2, 1, 23, 4, 1,'Tape recorder ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_17 child of Panel2
   Edit2_17 = gmCreateValueEntry(Panel2, 5, 23, 6, 1,tedit2_17, 32768, 3,GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=217)

! Create text entry Static2_18 child of Panel2
   Static2_18 = gmCreateTextEntry(Panel2, 1, 24, 5, 1,'Tape speed factor ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=7, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Edit2_18 child of Panel2
   Edit2_18 = gmCreateValueEntry(Panel2, 6, 24, 5, 1,tedit2_18, 32768, 3, GEDIT, &
              	gmType=GSTANDARD, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=218)
 call gmSetWidgetStatus(Edit2_2,GUNSELECTABLE)
   call gmSetWidgetStatus(Edit2_5,GUNSELECTABLE)
   call gmSetWidgetStatus(Edit2_7,GUNSELECTABLE)
   call gmSetWidgetStatus(Edit2_8,GUNSELECTABLE)
   call gmSetWidgetStatus(Edit2_17,GUNSELECTABLE)
   call gmSetWidgetStatus(Edit2_18,GUNSELECTABLE)
  
    call gmSetWidgetStatus(Edit2_3,GUNSELECTABLE)
	  call gmSetWidgetStatus(Edit2_3a,GUNSELECTABLE)
	  call gmSetWidgetStatus(Edit2_4,GUNSELECTABLE)
	 
	  call gmSetWidgetStatus(Toggle2_6(1),GUNSELECTABLE)
	  call gmSetWidgetStatus(Toggle2_6(2),GUNSELECTABLE)
	   call gmSetWidgetStatus(Edit2_12,GUNSELECTABLE)
   call gmSetWidgetStatus(Edit2_15,GUNSELECTABLE)
	  call gmSetWidgetStatus(Edit2_13,GUNSELECTABLE)
	  call gmSetWidgetStatus(Edit2_13a,GUNSELECTABLE)
	  call gmSetWidgetStatus(Edit2_14,GUNSELECTABLE)
	  
	  call gmSetWidgetStatus(Toggle2_16(1),GUNSELECTABLE)
	  call gmSetWidgetStatus(Toggle2_16(2),GUNSELECTABLE)
		  
!----------------------------------------------------------

! Set up complex dialogue box Form4 child of Form1
   Form4 = gmCreateComplexDialogueBox(Form1, 13, 12, 16, 5, GALL, 'Stop', &
              	gmVpos=GTOP, gmIconFormat=GDLLICON,gmIconFile='MBIG1079')

! Create main panel for form
   Form4MainPanel=gmCreatePanel(Form4, 0, 0, 16, 5, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=14, gmFillBorder=0)

! Create text entry Static4 of Form4MainPanel
   Static4 = gmCreateTextEntry(Form4MainPanel, 1, 1, 14, 1,'Please fill all the fields correctly before proceeding ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=2, gmBack2Col=0, gmTextCol=10, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create button Button4_1 child of Form4MainPanel
   Button4 = gmCreatePushButton(Form4MainPanel, 7, 3, 2, 1, 'OK', &
              	gmType=GSTANDARD, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=40)

! Set up complex dialogue box Form5 child of Form1
   Form5 = gmCreateComplexDialogueBox(Form1, 15, 10, 16, 5, GALL, 'Stop sampling', &
              	gmVpos=GTOP, gmIconFormat=GDLLICON,gmIconFile='MBIG1080')

! Create main panel for form
   Form5MainPanel=gmCreatePanel(Form5, 0, 0, 16, 5, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=14, gmFillBorder=0)

! Create text entry Static5 child of Form5MainPanel
   Static5 = gmCreateTextEntry(Form5MainPanel, 8, 1, 12, 1,'Are you sure you want to stop sampling ? ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=2, gmBack2Col=0, gmTextCol=10, &
              	gmHpos=GCENTRE, gmVpos=GTOP, gmExpand=GOFF)

! Create toggle button Toggle5_1 child of Form5MainPanel
   Toggle5(1) = gmCreateToggleButton(Form5MainPanel, 5, 3, 4, 1, 'Yes', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=51)

! Create toggle button Toggle5_1 child of Form5MainPanel
   Toggle5(2) = gmCreateToggleButton(Form5MainPanel, 9, 3, 4, 1, 'No', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=52)

! Set up complex dialogue box Form6 child of Form1
   Form6 = gmCreateComplexDialogueBox(Form1, 9, 6, 16, 5, GALL, 'Quit', &
              	gmVpos=GTOP, gmIconFormat=GDLLICON,gmIconFile='MBIG1077')

! Create main panel for form
   Form6MainPanel=gmCreatePanel(Form6, 0, 0, 16, 5, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

! Create panel Panel6 child of Form6MainPanel
   Panel6 = gmCreatePanel(Form6MainPanel, 0, 0, 16, 5, &
              	gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=14, gmFillBorder=0, &
              	gmHpos=GCENTRE, gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static6_1 child of Panel6
   Static6_1 = gmCreateTextEntry(Panel6, 2, 1, 12, 1,'Do you really want to quit? ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=2, gmBack2Col=0, gmTextCol=4, &
              	gmHpos=GCENTRE, gmVpos=GTOP, gmExpand=GOFF)

! Create toggle button Toggle6_1 child of Form6MainPanel
   Toggle6(1) = gmCreateToggleButton(Panel6, 5, 3, 4, 1, 'Yes', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=61)

! Create toggle button Toggle6_1 child of Form6MainPanel
   Toggle6(2) = gmCreateToggleButton(Panel6, 9, 3, 4, 1, 'No', 0, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF, gmCallback=62)

! Set up complex dialogue box Form7 child of Form1
   Form7 = gmCreateComplexDialogueBox(Form1, 9, 6, 14, 8, GALL, 'About', &
              	gmVpos=GTOP, gmIconFormat=GDLLICON,gmIconFile='MBIG1077')
! Create main panel 7
   Form7MainPanel=gmCreatePanel(Form7, 0, 0, 14, 8, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)


! Create panel Panel1 child of Form7MainPanel
   Panel7 = gmCreatePanel(Form7MainPanel, 0, 0, 14, 8, &
              	gmTitle='                  CONSAM  FOR  WINDOWS  V1.0', gmType=GPROJECTED, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=12, gmFillBorder=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static1 child of Panel7
   Static7_1 = gmCreateTextEntry(Panel7, 1, 1, 12, 1,'General-purpose Continuous Sampling to Disk ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static7-2 child of Panel7
   Static7_2 = gmCreateTextEntry(Panel7, 1, 4, 12, 1,'David Colquhoun and Ioana Vais ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static3 child of Panel7
   Static7_3 = gmCreateTextEntry(Panel7, 1, 5, 12, 1,'University College London ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=12, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create text entry Static4 child of Panel7
   Static7_4 = gmCreateTextEntry(Panel7, 1, 6, 12, 1,'http://www.ucl.ac.uk/Pharmacology ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

! Create icon Icon1 child of Panel1
   Icon7 = gmCreateIcon(Panel7, 6, 2, 2, 2,'MBIG1035', GDLLICON,0,gmVpos=GTOP)

   ptype(1)='Outside-out'
   ptype(2)='Inside-out'	
   ptype(3)='Cell-attached'
   ptype(4)='Whole-cell'
   ptype(5)='Simulated data'

	itape=0
	ftape=1.0
	itape1=0
	ftape1=1.0

   
   ioff=512
   n1401=0
   
! Start management
   call gmManage
   status=gmDisplayMessageBox(' ','Sampling Live', &
			                 GQUESTION,GYESNO)	
   
   if (STATUS.EQ.GYESBUTTON) then	
		tedit1_1=qdate
		tedit1_2='no details'
		tval1_3=1
		tedit1_4='Live'
		ipatch=3
		tval1_6=100.
		tval1_7=23.
		itogon=1
		nchan=1
		if(nchan.eq.1) cnchan='0'
		if(nchan.eq.2) cnchan='0 1'
		call gmSetTextSetting(edit1_1,tedit1_1)	
		call gmSetTextSetting(edit1_2,tedit1_2)
		call gmSetValueSetting(val1_3,tval1_3)	
		call gmSetTextSetting(edit1_4,tedit1_4)
		call gmSetValueSetting(edit2_7,tedit2_7)
		call gmSetValueSetting(edit2_8,tedit2_8)
		call gmSetValueSetting(val1_6,tval1_6)	
		call gmSetValueSetting(val1_7,tval1_7)	
		call gmSetToggleSwitch(Toggle1_5(3),GON)
		call gmSetToggleSwitch(Toggle1_8(1),GON)
		call gmSetToggleSwitch(Toggle1_9(1),GON)
		iall=100
		if(iall.eq.100) then
			call gmSetWidgetStatus(Edit2_2,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_5,GSELECTABLE)
			
		
			call gmSetToggleSwitch(Toggle2_6(2),GON)
		
			if(nchan.eq.2) then
				call gmSetWidgetStatus(Edit2_12,GSELECTABLE)
				
			call gmSetWidgetStatus(Edit2_15,GSELECTABLE)
				
				call gmSetToggleSwitch(Toggle2_16(2),GON)
			
			endif
   
        endif
   else
	call gmSetToggleSwitch(Toggle2_6(1),GON)
	if(nchan.eq.2) call gmSetToggleSwitch(Toggle2_16(1),GON)
   endif
   

! Set required callbacks for forms

! Action loop
1   do while (gmAction(callid) /= -1)
	
	select case(callid)
		
!		case(110)
		   
		case(151:155)
		   status=gmEnqToggleSwitch(Toggle1_5(callid-150))
		   if(status.eq.GOFF) then
		      call gmSetToggleSwitch(Toggle1_5(callid-150),GOFF)
		   else	
		     if((callid-150).ne.ipatch) sampled=.false.
		     ipatch=callid-150
			 do i=1,5
			    if(i.ne.callid-150) call gmSetToggleSwitch(Toggle1_5(i),GOFF)
			 enddo
		   endif	
		case(181:182)
		   status=gmEnqToggleSwitch(Toggle1_8(callid-180))
		   if(status.eq.GOFF) then
		      call gmSetToggleSwitch(Toggle1_8(callid-180),GOFF)
		   else	
		     if((callid-180).ne.nchan) sampled=.false.
		     nchan=callid-180
			 
		    if(nchan.eq.1) cnchan='0'
			if(nchan.eq.2) cnchan='0 1'
			 do i=1,2
			    if(i.ne.callid-180) call gmSetToggleSwitch(Toggle1_8(i),GOFF)
			 enddo
		   endif	
		   
		case(191,192)
		 status=gmEnqToggleSwitch(Toggle1_9(callid-190))
		 if(status.eq.GOFF) then
		      call gmSetToggleSwitch(Toggle1_9(callid-190),GOFF)
		 else	
		   if(callid.eq.191) then
			 call gmSetToggleSwitch(Toggle1_9(2),GOFF)
			iall=100
		   else
		    iall=0
			call gmSetToggleSwitch(Toggle1_9(1),GOFF)
		   endif 
		   call gmSetToggleSwitch(Toggle1_9(3),GOFF) 
		   if(nchan.eq.0) then
				status=gmDisplayMessageBox(' ','Please enter number of channels',gstop,gok)
		   else 
				inchan=int2(nchan)
				call gmSetWidgetStatus(Edit2_2,GSELECTABLE)
				call gmSetWidgetStatus(Edit2_5,GSELECTABLE)
				call gmSetwidgetstatus(Toggle2_6(1),Gselectable)
				call gmSetwidgetstatus(Toggle2_6(2),Gselectable)
			!	call gmSetToggleSwitch(Toggle2_6(1),Goff)
			!	call gmSetToggleSwitch(Toggle2_6(2),Goff)
	  			if(nchan.eq.2) then
						call gmSetWidgetStatus(Edit2_12,GSELECTABLE)
						call gmSetWidgetStatus(Edit2_15,GSELECTABLE)
							call gmSetwidgetstatus(Toggle2_16(1),Gselectable)
							call gmSetwidgetstatus(Toggle2_16(2),Gselectable)
			!			call gmSetToggleSwitch(Toggle2_16(1),Goff)
			!			call gmSetToggleSwitch(Toggle2_16(2),Goff)
				endif
				if(iall.eq.100) then
				call gmSetwidgetstatus(Toggle2_6(1),Gunselectable)
				call gmSetwidgetstatus(Toggle2_6(2),Gunselectable)
					call gmSetWidgetStatus(Edit2_7,GUNSELECTABLE)
					call gmSetWidgetStatus(Edit2_8,GUNSELECTABLE)
						call gmSetwidgetstatus(Toggle2_16(1),Gunselectable)
							call gmSetwidgetstatus(Toggle2_16(2),Gunselectable)
					call gmSetWidgetStatus(Edit2_17,GUNSELECTABLE)
					call gmSetWidgetStatus(Edit2_18,GUNSELECTABLE)
  
					call gmSetWidgetStatus(Edit2_3,GUNSELECTABLE)
					call gmSetWidgetStatus(Edit2_3a,GUNSELECTABLE)
					call gmSetWidgetStatus(Edit2_4,GUNSELECTABLE)
	 
					
					call gmSetWidgetStatus(Edit2_12,GUNSELECTABLE)
					call gmSetWidgetStatus(Edit2_15,GUNSELECTABLE)
					call gmSetWidgetStatus(Edit2_13,GUNSELECTABLE)
					call gmSetWidgetStatus(Edit2_13a,GUNSELECTABLE)
					call gmSetWidgetStatus(Edit2_14,GUNSELECTABLE)
	  
					
					
				else
					call gmSetWidgetStatus(Edit2_2,GunSELECTABLE)
					call gmSetWidgetStatus(Edit2_7,GSELECTABLE)
					call gmSetWidgetStatus(Edit2_8,GSELECTABLE)
				
					call gmSetWidgetStatus(Edit2_3,GSELECTABLE)
					call gmSetWidgetStatus(Edit2_3a,GSELECTABLE)
					call gmSetWidgetStatus(Edit2_4,GSELECTABLE)
					if(nchan.eq.2) then
				
					call gmSetWidgetStatus(Edit2_12,GunSELECTABLE)
					call gmSetWidgetStatus(Edit2_15,GSELECTABLE)
					call gmSetWidgetStatus(Edit2_13,GSELECTABLE)
					call gmSetWidgetStatus(Edit2_13a,GSELECTABLE)
					call gmSetWidgetStatus(Edit2_14,GSELECTABLE)
	  				call gmSetWidgetStatus(Edit2_17,GSELECTABLE)
					call gmSetWidgetStatus(Edit2_18,GSELECTABLE)
  
					endif		
				endif
		    		 
		   endif
		 endif
		case(193)
		iall=-1
		 call gmSetWidgetStatus(Edit2_2,GUNSELECTABLE)
		 call gmSetWidgetStatus(Edit2_5,GSELECTABLE)
		 call gmSetWidgetStatus(Edit2_12,GUNSELECTABLE)
		call gmSetWidgetStatus(Edit2_15,GUNSELECTABLE)

         call gmSetWidgetStatus(Edit2_7,GUNSELECTABLE)
        call gmSetWidgetStatus(Edit2_8,GUNSELECTABLE)
		call gmSetWidgetStatus(Edit2_17,GUNSELECTABLE)
		call gmSetWidgetStatus(Edit2_18,GUNSELECTABLE)
  
		call gmSetWidgetStatus(Edit2_3,GUNSELECTABLE)
		call gmSetWidgetStatus(Edit2_3a,GUNSELECTABLE)
		call gmSetWidgetStatus(Edit2_4,GUNSELECTABLE)
	 
		call gmSetWidgetStatus(Toggle2_6(1),GUNSELECTABLE)
		call gmSetWidgetStatus(Toggle2_6(2),GUNSELECTABLE)
		
		call gmSetWidgetStatus(Edit2_12,GUNSELECTABLE)
		if(nchan.eq.2) call gmSetWidgetStatus(Edit2_15,GSELECTABLE)
		call gmSetWidgetStatus(Edit2_13,GUNSELECTABLE)
		call gmSetWidgetStatus(Edit2_13a,GUNSELECTABLE)
		call gmSetWidgetStatus(Edit2_14,GUNSELECTABLE)
	  
		call gmSetWidgetStatus(Toggle2_16(1),GUNSELECTABLE)
		call gmSetWidgetStatus(Toggle2_16(2),GUNSELECTABLE)
		
		status=gmEnqToggleSwitch(Toggle1_9(3))
		   if(status.eq.GOFF) then
		      call gmSetToggleSwitch(Toggle1_9(3),GOFF)
		   else	
		  do i=1,2
			call gmSetToggleSwitch(Toggle1_9(i),GOFF)
          enddo
		  calfac=-1.
		  calfac1=-1.
		  if(nchan.eq.0) then

		  status=gmDisplayMessageBox(' ','Please enter number of channels',&
			                 GSTOP,GOK)
          endif
		  endif
		case(11) 
		 sampled=.false.  
		 call gmSetTextSetting(edit1_1,'')	
		 call gmSetTextSetting(edit1_2,'')
		 call gmSetValueSetting(val1_3,0.)	
		 call gmSetTextSetting(edit1_4,'')
		 call gmSetValueSetting(val1_6,0.)	
		 call gmSetValueSetting(val1_7,0.)
		 call gmSetValueSetting(val1_11,0.)	
		 call gmSetValueSetting(val1_12,0.)	
		 call gmSetValueSetting(icounter,0.)			
		 
		do i=1,5
			call gmSetToggleSwitch(Toggle1_5(i),GOFF)
		enddo
		do i=1,2
			call gmSetToggleSwitch(Toggle1_8(i),GOFF)
		enddo
		do i=1,3
			call gmSetToggleSwitch(Toggle1_9(i),GOFF)
		enddo
		if(iall.eq.100) then
		   
		    call gmSetValueSetting(Edit2_2,0.)
			
			if(nchan.eq.2) then
			 call gmSetValueSetting(Edit2_12,0.)
			
			endif
		  else	
		   
			call gmSetValueSetting(Edit2_3a,0.)
			call gmSetValueSetting(Edit2_3,0.)
			call gmSetValueSetting(Edit2_4,0.)
			call gmSetValueSetting(Edit2_5,0.)
			call gmSetValueSetting(Edit2_7,0.)
			call gmSetValueSetting(Edit2_8,0.)
			
			if(nchan.eq.2) then
				call gmSetValueSetting(Edit2_13a,0.)
				call gmSetValueSetting(Edit2_13,0.)
				call gmSetValueSetting(Edit2_14,0.)
				call gmSetValueSetting(Edit2_15,0.)
				call gmSetValueSetting(Edit2_17,0.)
				call gmSetValueSetting(Edit2_18,0.)
			
			endif
		  endif	
		  
		! Create text entry Static1_13 child of Panel6
		call gmsettextsetting(static1_13,'CONSAM file ')
	

! Create text entry Static1_14 child of Panel6
			call gmsettextsetting(static1_14,'WAITING TO FILL IN DATA; To start press the button "Start sampling" ')
	
	
		call gFillRect(GSOLID,15,limits)
		call gFlushGraphics
		case(12)
			CALL WINDRA(FORM6)
			CALL ACTWIN(FORM6)
		    
		case(13,14) !sampling

		tedit2_5= gmEnqValueSetting(edit2_5)
		if(tedit2_5.ne.0) then
			  if(tedit2_5.ne.filt) sampled=.false.
			  filt=tedit2_5
			  calfac=-1
		      if(nchan.eq.2) then
				tedit2_15= gmEnqValueSetting(edit2_15)
		        if(tedit2_15.ne.0) then
					if(tedit2_15.ne.filt1) sampled=.false.
					filt1=tedit2_15
					calfac1=-1
				else
					status=gmDisplayMessageBox(' ','Please enter filter setting', &
			                 GSTOP,GOK)
					goto 1
				endif
			  endif
		else
				status=gmDisplayMessageBox(' ','Please enter filter setting', &
			                 GSTOP,GOK)
				goto 1
		endif
		if(iall.eq.100) then		  		  
			  val= gmEnqValueSetting(edit2_2)
			  if(val.ne.0.) then
		        if(val.ne.tedit2_2) sampled=.false.
			    tedit2_2=val
		        VpA=tedit2_2
			    amVpA=VpA*1000.		!mV (at 1401) per pA
			    pAV=1.0/VpA			!pA per volt
	            calfac=pAV/6553.6	!
              else
			   status=gmDisplayMessageBox(' ','Please enter calibration', &
			                 GSTOP,GOK)
				goto 1
			  endif
           	  if(nchan.eq.2) then
				val= gmEnqValueSetting(edit2_12)
				if(val.ne.0) then
				    if(val.ne.tedit2_12) sampled=.false.
					tedit2_12=val
					VpA1=val
					amVpA1=VpA1*1000.		!mV (at 1401) per pA
					pAV1=1.0/VpA1			!pA per volt
					calfac1=pAV1/6553.6	!=
				else
					status=gmDisplayMessageBox(' ','Please enter calibration', &
			                 GSTOP,GOK)
					goto 1
				endif
			  endif
			
		else
			i=ifixr(amVpA)
			val= gmEnqValueSetting(edit2_3a)
			if(val.ne.0) then
		     if(val.ne.tedit2_3a) sampled=.false.
			 tedit2_3a=val
		     amVpA=val
            else
				status=gmDisplayMessageBox(' ','Please enter clamp setting', &
			                 GSTOP,GOK)
				goto 1
			endif
			val= gmEnqValueSetting(edit2_3)
			if(val.ne.0) then
		     if(val.ne.tedit2_3) sampled=.false.
			 tedit2_3=val
		     gain=val
            else
				status=gmDisplayMessageBox(' ','Please enter amplifier gain', &
			                 GSTOP,GOK)
				goto 1
			endif
			istatus1=gmEnqToggleSwitch(Toggle2_6(1)) 
			istatus2=gmEnqToggleSwitch(Toggle2_6(2))
			if(istatus1.eq.GOFF.and.istatus2.eq.GOFF) then
				status=gmDisplayMessageBox(' ','Please enter cosaming way', &
			                 GSTOP,GOK)
				goto 1
			else 
				if(istatus1.eq.GON) itape=1
				if(itape.eq.1) then
					val= gmEnqValueSetting(edit2_7)
					if(val.ne.0.) then
						if(val.ne.tedit2_7) sampled=.false.
						tedit2_7=val
						ftape=val
					else
					status=gmDisplayMessageBox(' ','Please enter tape (V peak)', &
			                 GSTOP,GOK)
						goto 1
					endif
				endif
			    val= gmEnqValueSetting(edit2_4)
			    if(val.ne.0) then
					if(val.ne.tedit2_4) sampled=.false.
					tedit2_4=val
					errfac=val
					VPa=amVpA/1000.		!V (from clamp)/pA
					pAV=1.0/VpA		!pA per V (from clamp)
					pAV=pAV*ftape	!correct for tape
					pAV=pAV/gain	
					pAV=pAV/errfac	!pA per Volt in computer
					calfac=pAV/6553.6	
                else
				status=gmDisplayMessageBox(' ','Please enter error factor', &
			                 GSTOP,GOK)
					goto 1
				endif
				if(itape.eq.1) then
					val= gmEnqValueSetting(edit2_8)
					if(val.ne.0) then
						if(val.ne.tedit2_8) sampled=.false.
						tedit2_8=val
						ifac=val
						tapfac=float(ifac)
					else
						status=gmDisplayMessageBox(' ','Please enter tape speed factor', &
			                 GSTOP,GOK)
						goto 1				
					endif
               	endif
			endif
			if(nchan.eq.2) then
			i=ifixr(amVpA1)
			val= gmEnqValueSetting(edit2_13a)
			if(val.ne.0) then
		     if(val.ne.amVpA1) sampled=.false.
		     amVpA1=val
            else
			status=gmDisplayMessageBox(' ','Please enter clamp setting', &
			                 GSTOP,GOK)
				goto 1
			endif
			val= gmEnqValueSetting(edit2_13)
			if(val.ne.0) then
		     if(val.ne.gain1) sampled=.false.
		     gain1=val
            else
				status=gmDisplayMessageBox(' ','Please enter amplifier gain', &
			                 GSTOP,GOK)
				goto 1
			endif
			istatus1=gmEnqToggleSwitch(Toggle2_16(1)) 
			istatus2=gmEnqToggleSwitch(Toggle2_16(2))
			if(istatus1.eq.GOFF.and.istatus2.eq.GOFF) then
				goto 1
			else 
				if(istatus1.eq.GON) itape=1
				if(itape.eq.1) then
					val= gmEnqValueSetting(edit2_17)
					if(val.ne.0.) then
						if(val.ne.tedit2_17) sampled=.false.
						tedit2_17=val
						ftape1=val
					else
					status=gmDisplayMessageBox(' ','Please enter tape recorder (V peak)', &
			                 GSTOP,GOK)
						goto 1
					endif
				endif
			    val= gmEnqValueSetting(edit2_14)
			    if(val.ne.0) then
					if(val.ne.errfac1) sampled=.false.
					errfac1=val
					VPa1=amVpA1/1000.		!V (from clamp)/pA
					pAV1=1.0/VpA1	!pA per V (from clamp)
					pAV1=pAV1*ftape1	!correct for tape
					pAV1=pAV1/gain1	
					pAV1=pAV1/errfac1	!pA per Volt in computer
					calfac1=pAV1/6553.6	
                else
				status=gmDisplayMessageBox(' ','Please enter error factor', &
			                 GSTOP,GOK)
					goto 1
				endif
				if(itape.eq.1) then
					val= gmEnqValueSetting(edit2_18)
					if(val.ne.0) then
						if(val.ne.ifac1) sampled=.false.
						ifac1=val
						tapfac1=float(ifac1)
					else
					status=gmDisplayMessageBox(' ','Please enter tape speed factor', &
			                 GSTOP,GOK)
						goto 1				
					endif
               	endif
			endif
			endif
		endif
		
		call gmEnqTextSetting(edit1_1,tedit1_1)
		  if(tedit1_1.ne.'    ')  then
		    if(tedit1_1(1:11).ne.expdate) sampled=.false.
			expdate=tedit1_1(1:11) 
		    call gmEnqTextSetting(edit1_2,tedit1_2)
		    if(tedit1_2.ne.'   ') then
			  if(tedit1_2(1:24).ne.tapeid) sampled=.false.
			  tapeid=tedit1_2(1:24)
		      tval1_3= gmEnqValueSetting(val1_3)
		      if(tval1_3.ne.0) then
			    if(tval1_3.ne.npatch) sampled=.false.  
		        npatch=Tval1_3 	
		        call gmEnqTextSetting(edit1_4,tedit1_4)
		        if(tedit1_4.ne.'   ') then
				  if(tedit1_4(1:6).ne.defname) sampled=.false.
		          defname=tedit1_4(1:6) 
				  title=tedit1_4
			      tval1_6= gmEnqValueSetting(val1_6)
		          if(tval1_6.ne.0) then
				    if(tval1_6.ne.emem) sampled=.false.
					Emem=tval1_6
					tval1_7= gmEnqValueSetting(val1_7)
		            if(tval1_7.ne.0) then
					  if(tval1_7.ne.temp) sampled=.false.
					  Temp=tval1_7
                      tval1_11= gmEnqValueSetting(val1_11)
		              if(tval1_11.ne.0) then
					    if(tval1_11.ne.irate) sampled=.false.
						irate=tval1_11
                        tval1_12= gmEnqValueSetting(val1_12)
		                if(tval1_12.ne.0) then
						if(tval1_12.ne.sec) sample=.false.
					    sec=tval1_12
						if(ipatch.ne.0) then
						  if(nchan.ne.0) then
						    call intconv(npatch,cnpatch)
                            IF(Npatch.le.9) THEN
							  SFILE=ADCFIL(1:7)//CNPATCH(1:1)//'.SSD'
							ELSE
                              SFILE=ADCFIL(1:7)//'S.SSD'
                            ENDIF
							! check ipatch and calibration
						    nosel=-1
			                do k=1,3
							  istatus=gmEnqToggleSwitch(toggle1_9(k))
			                  if(istatus.eq.1) nosel=1
		                    enddo	
				            if(nosel.eq.1) then
		                       
							   STATUS=GYESBUTTON
							   if(sampled) then
							    sampled=.false.
								call gmSetValueSetting(icounter,0.0)
								
								call gFillRect(GSOLID,15,limits)
								call gFlushGraphics
                                status=gmDisplayMessageBox(' ','Do you want to repeat the same sequence ?', GQUESTION,GYESNO)
                	
		                       endif
		                       IF(STATUS.EQ.GYESBUTTON) THEN
								if(callid.eq.13) cctrig='HT'
								if(callid.eq.14) cctrig='H'
		 
		 						call sampleinfo(Form1,Form3,Form3MainPanel,Panel3, &
								nchan,irate,sec,nbyte,ndiv1,ndiv2,srate,srate1,ndiv)
								if(itape.eq.1) then
									srate=tapfac*srate
									if(ifac.ne.1) then
										status=gmDisplayMessageBox(' ','Filtered after taping ?', &
										GQUESTION,GYESNO)
										if(status.eq.GYESBUTTON) then
											filt=tapfac*filt
											filt1=tapfac*filt1
										endif
									endif
				     
								endif
								CALL WINDRA(FORM3)
								CALL ACTWIN(FORM3)
							  
							   ELSE
									status=gmDisplayMessageBox(' ','Press RESET and restart', GINFORMATION,GOK)
                	
							   ENDIF
	
							else
							   status=gmDisplayMessageBox(' ','Please enter calibration',&
			                   GSTOP,GOK)
							endif
				
				          else
							 status=gmDisplayMessageBox(' ','Please enter number of channels',&
			                 GSTOP,GOK)
		                  endif
                        else
						   status=gmDisplayMessageBox(' ','Please enter patch type', &
			                 GSTOP,GOK)
						endif
                      else
					   status=gmDisplayMessageBox(' ','Please enter sample duration', &
			                 GSTOP,GOK)
					  endif  
                    else
					 status=gmDisplayMessageBox(' ','Please enter sampling rate', &
			                 GSTOP,GOK)
					endif
                  else
				   status=gmDisplayMessageBox(' ','Please enter temperature', &
			                 GSTOP,GOK)
				  endif
                else
				 status=gmDisplayMessageBox(' ','Please enter membrane potential', &
			                 GSTOP,GOK)
				endif
              else
			   status=gmDisplayMessageBox(' ','Please enter patch name', &
			                 GSTOP,GOK)
			  endif
			else
			 status=gmDisplayMessageBox(' ','Please enter patch number', &
			                 GSTOP,GOK)
			endif 
		  else
		    status=gmDisplayMessageBox(' ','Please enter tape details', &
			                 GSTOP,GOK) 
		  endif
		  else
		    status=gmDisplayMessageBox(' ','Please enter date', &
			                 GSTOP,GOK)
		  endif  	
		 
		case(15)
		    if(isamp.eq.0) then
				status=gmDisplayMessageBox(' ','You are not sampling now', &
			                 GEXCLAMATION,GOK)
		    else 
				CALL WINDRA(FORM5)
				CALL ACTWIN(FORM5)
            endif
		
		case(261)
		    call gmSetToggleSwitch(Toggle2_6(2),GOFF)
			call gmSetWidgetStatus(Edit2_7,GSELECTABLE)
			call gmSetWidgetStatus(Edit2_8,GSELECTABLE)
			if(nchan.eq.2) then
			   call gmSetWidgetStatus(Edit2_17,GSELECTABLE)
			   call gmSetWidgetStatus(Edit2_18,GSELECTABLE)
			   call gmSetToggleSwitch(Toggle2_16(2),GOFF)
			endif
			itape=1
		case(262)
			call gmSetToggleSwitch(Toggle2_6(1),GOFF)
			itape=0
			ftape=1.0
			if(nchan.eq.2) then
			call gmSetToggleSwitch(Toggle2_16(1),GOFF)
			itape1=0
			ftape1=1.0
			endif
		
		
		case(31)
		  CALL WINREM(Form3)
	      CALL ACTWIN(fORM1)
		case(32)
          
		  CALL WINREM(Form3)
	      CALL ACTWIN(fORM1)
		  if(idir.eq.-1) call gEnqWorkingDir(wdir)
		  idir=1
		  SDIR=wdir(1:1)//':\'
		  SFILT='SSD'
		  CALL gmFileBrowser(SFILE,SDIR,SFILT,gmType=GOUTPUT, &
		  gmTitle='Consam files')
		  ns=nblank1(sfile)
		  if (sfile.ne.'     ') then
		  ADCFIL=SFILE
		  oErr=U14Open1401(n1401)
          if(oErr.eq.0) then
	        START1401=.TRUE.
	        iType = U14TypeOf1401(oErr)	  
	        if(itype.eq.0) then
				messg='1401 STANDARD '
				nbuffer=400000
	        else if(itype.eq.1) then
				messg='1401 PLUS '
				nbuffer=400000
			else
				messg='1401 Power '
				nbuffer=1000000
			endif
			sErr=U14Close1401(oErr)
		!	IF(NBUFFER.GT.NBYTE) NBUFFER=NBYTE
		  else
			select case(oErr)
			case (-500)
	     		messg='1401 SWITCHED OFF ; PLEASE SWITCH ON'
	   		case (-501)
	     		messg='1401 NOT CONNECTED: CHECK AND RESTART'
			case (-502)
	     		messg='1401 ILL ( CALL THE DOCTOR ? ): '// &
         		'SWITCH OFF/ON 1401 AND REBOOT '
	   		case (-503)
	     		messg='I/F CARD MISSING: CHECK AND RESTART'
	   		case (-505)
	     		messg='I/F CARD BAD SWITCHES: '// &
          		'CHECK AND RESTART '
	   		case (-506)
	     		messg='1401+ FAILED TO COME READY : '// &
          		'SWITCH OFF/ON 1401 AND REBOOT'
			case (-508)
	     		messg='1401 IS ALREADY IN USE : PLEASE WAIT'
	            sErr=U14Close1401(%val(oErr))
				call gTimeDelay(3000) 
	            		
	   		case (-509)
	     		messg='COULD NOT GET DMA CHANNELS : SWITCH OFF/ON &
         		1401 AND REBOOT'
			case(-580)
	     		messg='NOT 386 ENHANCED MODE: CHECK AND RESTART'
	   		case(-581)
	     		messg='NO DEVICE DRIVER : CHECK AND RESTART'
			case(-582)
	     		messg='DEVICE DRIVER TOO OLD : NO COMMENTS !'
			end select

			messg='SORRY:'//MESSG
			START1401=.FALSE.
	  
!			stop
          endif
		  status=gmDisplayMessageBox(' ',messg, GINFORMATION,GOK)

		  if(oErr.eq.0) then
		 
		  isec=int(sec)
         ! dvalue=0.1*sec
		  dvalue=50000.
		  if(dvalue.ge.float(nbyte)) dvalue=float(nbyte)
		  !intval=10
	      limits%xmin=0.
		 ! limits%xmax=sec
		  limits%xmax=float(nbyte)
		  limits%ymin=0.
		  limits%ymax=1.
		  call gmSetGraphicsFrameBounds(graph,limits)

		!  call gmEnqWidgetInfo(graph,grframe)
        !
		  call gmActivateGraphicsFrame(graph)
          call gFillRect(GSOLID,GWHITE,limits)
	      call gsetlinecolour(GBLACK)
          call gmoveto2d(0.,0.)
          call gdrawlineto2d(float(nbyte),0.)
  
          call gdrawlineto2d(float(nbyte),1.)
  
          call gdrawlineto2d(0.,1.)
   
          call gdrawlineto2d(0.,0.)
		  x=0.
		  do while(x.lt.float(nbyte))
             x=x+dvalue
		     call gmoveto2d(x,0.)
		     call gdrawlineto2d(x,0.3)
		  enddo
		  
		  CALL gmSetValueSetting(ICOUNTER,0.) 
			CALL gmSetValueSetting(val1_25,nbyte) 
          call gFlushGraphics
		   call gEnqWorkingDir(wdir)
		 
	      ND=NBLANK1(WDIR)
		  if(wdir(nd:nd).eq.'\')then
		   ADCDFIL=WDIR(1:ND)//adcfil
		  else
		   ADCDFIL=WDIR(1:ND)//'\'//adcfil
		  endif
	!	call gmsettextsetting(static1_13,'CONSAM file '//adcdfil)
		  
   call gmsettextsetting(static1_14,'SAMPLING NOW; To STOP sampling press the ESCAPE "Esc" KEY ')
		 
		  
		  status=gmDisplayMessageBox(' ','START SAMPLING', GINFORMATION,GOKCANCEL)
		  
		  if(status.eq.gokbutton) then
		    
		  idirerr=gSetWorkingDir('C:')
		
		  iErr = adcdisk(adcdfil, ioff, nbyte, cnchan, cctrig, ndiv1, ndiv2, &
		         CancelFunc,cdir, nbuffer, icounter, button1(5),sec)
		  iflen=ierr+ioff	
		  call intconv(iflen,charint)
		  if(iErr.gt.0.and.iErr.le.nbyte) then
		  sampled=.true.
		    clock=4000000.
		    iver=1002
		   ! ilen=nbyte+ioff
		    ilen=ierr
	        idt=ndiv1*ndiv2
	        id1=int2(ndiv1)
	        id2=int2(ndiv2)  
			inchan=int2(nchan)
            OPEN(unit=14,file=ADCDFIL,status='UNKNOWN', &
            access='DIRECT', form='BINARY', RECL=1)
            
	        WRITE(14,REC=1) iver,title,cdate,adctime,idt,ioff,ilen,inchan, & !!ierr or iflen?
	        id1,id2,cctrig(1:3),calfac,srate,filt,filt1,calfac1, &
	        expdate,defname,tapeID,ipatch,npatch,Emem,temp
	        CLOSE(unit=14)
	        nlp=nblank1(adcdfil)
	        PFILE=ADCDFIL(1:nlp-4)//'.txt'
	        OPEN(unit=8,file=PFILE,status='UNKNOWN', &
            access='APPEND',form='FORMATTED')
            REWIND(unit=8)
		    WRITE(8,800) 
800			format('CONSAM - Continuous sampling on disk')
		    WRITE(8,801) cdate,adctime
801	        format('Date of analysis: ',a11,' Time of analysis: ',a8)
	        WRITE(8,802)
802         FORMAT('CALIBRATION')	
            IF(IALL.NE.100)THEN
               write(8,803) amVpA1,ftape,gain
803            format(' Clamp setting (mV/pA)= ',g13.6,/, &
               '  tape (V peak) = ',g13.6,/, &
               '  amplifier gain= ',g13.6)
	        ENDIF
	        write(8,804) pAV,calfac
804	        format( &
            ' Calibration factor (pA per Volt in computer)= ',g13.6,/, &
            ' Calibration factor (pA per ADC unit        )= ',g13.6,/)
            IF(NCHAN.EQ.2) THEN
		      IF(IALL.NE.100)THEN
			  write(8,803) amVpA11,ftape1,gain1
		      ENDIF
		      write(8,804) pAV1,calfac1
	        ENDIF
	        IF(ITAPE.EQ.1.AND.IFAC.NE.1) THEN
				WRITE(8,805) IFAC,SRATE,FILT
805				FORMAT(' Tape slowed ',i3,' times,so effective sampling rate (Hz)=',&
				f13.6,/,'effective filter setting for ADC0(-3dB, Hz) =',f13.6)
				if(nchan.eq.2) then
					write(8,806)
806					format('effective filter setting for ADC1 (-3dB,hz) =',f13.6)
				endif
			ENDIF
			write(8,807) irate,clock/float(ndiv),ndiv,&
			ndiv1*ndiv2,ndiv1,ndiv2,srate,inchan
807			format(&
			' Requested sample rate (Hz) = ',i8,/,&
			' Nearest possible sample rate (Hz) = ',g15.8,'( n= ',i8,')',/,&
			' Nearest actual divisor is n1*n2 = ',i8,' (n1,n2= ',2i8,')',/,&
			' Actual sample rate (Hz) = ',g15.8,/,&
			' Channels = ',i8)
			nint=ifixr(sec*srate)
			write(8,808) filt,nint,sec
808			format(' Filter (-3db, Hz) for ADC0 = ',f10.1,/,&
			' Sample of ',i9,' points on ADC0',/,&
			' Sample duration (sec) = ',f12.2)
			if(nchan.eq.2) then
				write(8,809) irate,nchan,irate1,clock/float(ndiv),&
				ndiv,ndiv1*ndiv2,ndiv1,ndiv2,srate1,srate
	
809				format(&
				' Requested sample rate (Hz) = ',i8,/,&
				' Sample rate needed for ',i2,' channels(Hz) = ',i8,/,&
				' Nearest possible sample rate (Hz) = ',g15.8,'( n= ',i8,')',/,&
				' Nearest actual divisor is n1*n2 = ',i8,' (n1,n2= ',2i8,')',/,&
				' Actual total sample rate (Hz) = ',g15.8,/,&
				' Actual sample rate per channel (Hz) = ',g15.8)

				write(8,810) filt,filt1,nint,sec
810				format(&
				' Filter (-3db, Hz) for ADC0 = ',f10.1,/,': for ADC1 = ',f10.1,/,&
				' Samples of ',i9,' points on ADC0 and ADC1',/,&
				' Sample duration (sec) = ',f12.2,/,' Title:')
			endif
			WRITE(8,811)
811			FORMAT('EXPERIMENT')
			write(8,812) 	title,expdate,tapeid,npatch,ptype(ipatch),emem,temp
812			format(&
			' Patch title :',a70,/,&
			' Date :',a11,/,&
			' Tape details :',a24,/,&
			' Patch number :',i5,/,&
			' Patch type :',a14,/,&
			' Membrane potential [mV] :',g10.2,/,&
			' Temperature (Celsius):',g10.2)
			if(ierr.gt.0.and.ierr.lt.nbyte) then
				write(8,813) float(ierr)/2.
813				format( ' Sample stopped at: ',g10.2)
			endif
			CLOSE(unit=8)
	  
		  endif
		  if(iErr.eq.nbyte) then
		     COUNTVAL=FLOAT(IERR)
			 CALL GSETSTRJUSTIFY(GCENTRE)
		     CALL gmSetValueSetting(ICOUNTER,COUNTVAL) 
   		     Static1_14 = gmCreateTextEntry(Panel1_1, 1, 2, 20, 1,'SAMPLING FINISHED: To RESTART press the button "RESET" ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmHpos=GCENTRE, gmVpos=GTOP, gmExpand=GOFF)
				sampled=.true.
				 limits%ymin=0.5
				call gFillRect(GSOLID,GRED,limits) 
			 call gFlushGraphics
			 status=gmDisplayMessageBox(' ','Done: File length='//charint, GINFORMATION,GOK)
		     
		  else if(ierr.gt.0.and.ierr.lt.nbyte) then
		     call intconv(iflen,charint)
		
		     Static1_14 = gmCreateTextEntry(Panel1_1, 1, 2, 20, 1,'SAMPLING CANCELLED: To restart press the button "Start sampling" ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmHpos=GCENTRE, gmVpos=GTOP, gmExpand=GOFF)
			
				COUNTVAL=FLOAT(IERR)
				CALL GSETSTRJUSTIFY(GCENTRE)
				CALL gmSetValueSetting(ICOUNTER,COUNTVAL) 
   		    
				!limits%xmax=sec*(float(ierr)/float(nbyte))
				limits%xmax=float(ierr)
				limits%ymin=0.5
                call gFillRect(GSOLID,GRED,limits)
				call gFlushGraphics
			    status=gmDisplayMessageBox(' ','Sample cancelled at ='//charint, GINFORMATION,GOK)
		  else 
		  
			    messg='UNKNOWN'	   
		  select case(iErr)
			case (-540)
	     		messg='COMMAND FILE NOT FOUND'              
	   		case (-1000)
	     		messg='CREATE FILE FAILED'                    
			case (-1001)
	     		messg='BAD WRITE TO FILE'                   
         	
	   		case (-1002)
	     		messg='BAD SEEK'                           
	   		case (-1003)
	     		messg='SAMPLING TOO FAST'          
          	
	   		case (-1004)
	     		messg='SAMPLING ABORTED'                
          		
			case (-1005)
	     		messg='BAD PARAMETER'                       
	            
	   		case (-541)
	     		messg='ERROR READING COMMAND FILE'
			case(-542)
	     		messg='UNKNOWN COMMAND'                         
	   		case(-560)
	     		messg='TOHOST/1401 FAILED'                  
			case(-543)
	     		messg='NOT ENOUGH HOST SPACE' 
			             
			end select

		     call intconv(ierr,charint)
			 nm=nblank1(messg)
!			 messg=messg(1:nm)//'Error='//charint

			 status=gmDisplayMessageBox(' ',messg, GINFORMATION,GOK)
		     Static1_14 = gmCreateTextEntry(Panel1_1, 1, 2, 20, 1,'SAMPLING ABORTED: To restart press the button "Start sampling" ', 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmHpos=GCENTRE, gmVpos=GTOP, gmExpand=GOFF)
		  endif
		  endif
		  endif
		  endif
		case(40)
		  CALL WINREM(Form4)
	      CALL ACTWIN(0)
		case(51)
		  CALL WINREM(FORM5)
		  CALL ACTWIN(0)
		  call gmsettextsetting(static1_13,'CONSAM file '//adcdfil)
          call gmsettextsetting(static1_14,'SAMPLING CANCELLED: To restart press the button "Start sampling" ')

				CancelFunc=1
		case(52)
			CALL WINREM(FORM5)
			CALL ACTWIN(0)
		case(61)
			CALL WINREM(FORM6)
			CALL ACTWIN(0)
			call Exit
		case(62)
			CALL WINREM(FORM6)
			CALL ACTWIN(0)
        case(100)
			
			CALL WINDRA(FORM7)
			
	end select
! Code to handle callbacks

   end do


! Close device MENU and GINO, and end program
   call gmCloseMenu
   call gCloseDevice
   call gCloseGino

stop
end


