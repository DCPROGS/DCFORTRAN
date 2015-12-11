integer :: toolbar1_1
integer :: toolbar1_2
integer :: toolbar1_3
integer :: toolbar1_4
integer :: toolbar1_5

integer i_array_form(10)
 character*20 itextst(10),itextsv(20)
 integer*4 ieditout(10),ivalout(20)
character*60 teditout(10)
real tvalout(20)
integer :: initial_toggle(10),ivalbox(10),ivalb(10),iformText(25,4)
integer :: initwin_Toggle_1(10),initwin_Toggle_2(10)
real array_value(10,20)
character*30 array_text(10,20)
integer iptog(5)
integer iyes(20),ino(20),iyesno(20,2),i_array(10)
character*60 textyn(20)
character*100 iqtext(10)
integer	iqtoggle(10,2)
integer iplot_Toggle0(10),iplot_Toggle1(10),iplot_Toggle3(10),iplot_Toggle4(10)
logical page_open(25)
integer iradiox(200),iradioy(200),iplot_text(10),mtoggle(10)
real radiox(200),radioy(200)
integer ival2(10,20)
integer :: ittfont_Toggle(60)
integer iradio_data(20),iradio_toggle(200)
real radio_data(20)
character*(60) radio_text(200),titlerp
real val_box(10,20)
real valcell1(10,100)
integer ival_box(10,20),idradio_toggle(20)
integer info_pan(20)
character*60 ititle2(20)
! main_window
type(GARRAYCELL) arrayattribs
integer ::  Main
integer ::  MainMenubar
integer ::  MainMainPanel
integer ::  NEW_file
integer ::  Open_file
integer ::  Import_file
integer ::  Save_file
integer ::  Export_file
integer ::  Print_file
integer ::  Exit_file
integer ::  Title_record
integer ::  Parameters
integer ::  Labels
integer ::  Label_x
integer ::  Label_y
integer ::  Label_z
integer ::  Number_x
integer ::  Number_y
integer ::  Number_z

integer ::  view_Record
integer ::  view_Data
integer ::  view_Equations
integer ::  view_Another
!integer ::  Axis
integer ::  Frame


integer ::  Studio
integer ::  About
integer modelw(20)
integer :: Icon1_1
integer :: Icon1_2
integer :: Icon1_3
integer :: Icon1_4
integer :: Icon1_5
integer :: Icon1_6
integer :: Icon1_7
integer :: Icon1_8
integer :: Icon1_9
integer :: Icon1_10
integer :: Icon1_11
integer :: Icon1_12
integer :: Icon1_13
integer :: Icon1_14
integer :: Icon1_15

integer :: Icon1_16
integer :: Icon1_17
integer :: Icon1_18
integer :: Icon1_19 
integer :: Icon1_20
integer :: Icon1_21
integer :: Icon1_22
integer :: Icon1_23
integer :: Icon1_24
integer :: Icon1_25
integer :: Icon1_30
integer :: Icon1_26
integer :: Icon1_27
integer :: Icon1_28
integer :: Icon1_29
integer :: Combo1_1,combo1_11,combo1_12
integer :: Combo1_2
integer :: Combo1_3
integer :: Combo1_4
integer :: Combo1_5
integer :: Combo1_6
integer :: Combo1_7
integer :: Combo1_8
integer :: Combo1_9
integer :: Combo1_10
integer :: Status_Bar1

integer ::  iZoom
integer ::  Interpolate

! equations 
integer :: Eqs
integer :: EqsMainPanel
integer :: Graphics2
integer :: Toggle2(20)

! text_attributes
integer :: Text_record
integer :: TextMainPanel
integer :: Panel3_1
integer :: Combo3_1
integer :: Combo3_2
integer :: Combo3_3
integer :: Icon3_1
integer :: Icon3_2
integer :: Icon3_3
integer :: Icon3_4
integer :: Icon3_5
integer :: Icon3_6
integer :: Icon3_
integer :: Button3_1
integer :: Button3_2
integer :: Button3_3

! draw_attributes

integer :: Drawing
integer :: DrawingMainPanel
integer :: Panel4_1
integer :: Panel4_2
integer :: Panel4_3
integer :: Panel4_4
integer :: Icon4_1
integer :: Icon4_2
integer :: Icon4_3
integer :: Icon4_4
integer :: Toggle4(20)
integer :: Graphics4_1
integer :: Graphics4_2

! Help
integer :: Help_Dialog
integer :: HelpMainPanel

integer :: Graph1_1(100)
integer :: GraphMainPanel1_1(100)
integer :: Graphics1_1(100)
integer :: Graph1_2(100)
integer :: GraphMainPanel1_2(100)
integer :: Graphics1_2(100)
integer :: Graph1_3
integer :: GraphMainPanel1_3
integer :: Graphics1_3
integer :: Graph1_4
integer :: GraphMainPanel1_4
integer :: Graphics1_4

! Records list

integer :: DataMainPanel
integer :: ValArray6(50,50)
integer :: Button6(10)
integer :: static6
integer:: Records
integer:: RecordMainPanel
integer:: Table7_1
integer:: Static7_1
integer :: Button7(10)
type (GLIMIT) :: Graphics_frame 

integer :: Form8MainPanel
integer :: Panel8
integer :: Toggle8(5)
integer :: Editval8(4)
integer :: Button8(2)

integer :: Fit
integer :: eqfit(10)
integer chess(20,20)

integer :: new_Data_list
integer :: new_DataMainPanel
integer :: textArray
integer :: textButton

integer :: select_set,DATA_SETS,itogset(20)
integer :: Form12
integer :: Form12MainPanel
integer :: Panel12
integer :: Static12_1
integer :: Static12_2
integer :: Static12_3
integer :: Static12_4
integer :: Icon12

type (GPIXEL) :: pix

integer :: intoggle(100),idatg(100)
integer :: icyc_Form(500)

integer iq1(20),iqt1(20)
	character*60 textiq1(20),textiqt1(20)
	integer :: form1(25,4),form1_panel1(25,4),Form1_TxtArray1(25,4)
	integer isubpanel(20),isub(10,20),jcons(20),iopens(10,20),inum(10,20),istart(10,20)
real val_2(10,20)

integer panel1,hjcfitform,panel2(10)
	integer edit1(20,10),edit2(20,10),edit3(20,10),edit4(20,10),edit5(20,10),edit6(20,10)
	character*40 tedit1(20,10),tedit2(20,10),tedit3(20,10),tedit4(20,10),tedit5(20,10),tedit6(20,10)
	integer val1(20,10),val2(20,10),val3(20,10),val4(20,10),val5(20,10),val6(20,10)
	real tval1(20,10),tval2(20,10),tval3(20,10),tval4(20,10),tval5(20,10),tval6(20,10)
	integer toggle7_1(10,10),toggle7_2(10,10),toggle7_3(10,10),toggle7_4(10,10),toggle7_5(10,10)
	integer toggle14_1(10),toggle14_2(10),toggle14_3(10),valt(5)
	
	real tvalt(5),tval7(20,10),tval8(20,10),tval9(20,10)
	integer val8(20,10),val7(20,10),panel14

integer valres1(10),valres2(10),valres3(10),valres4(10),valres5(10)
	integer valres6(10),valres7(10),valres8(10),valres9(10)
	real tvalres1(10),tvalres2(10),tvalres3(10),tvalres4(10),tvalres5(10)
	real tvalres6(10),tvalres7(10),tvalres8(10),tvalres9(10)
	integer toggleres1(10),toggleres2(10),toggleres3(10)
integer :: group_Editval1(10),igroup_Editval1(10)
integer :: group_Editval2(10),igroup_Editval2(10)
integer :: group_Editval3(10)
integer :: group_Editval4(10),group_Editval4_1(10)
integer :: group_Editval5(10)
integer :: group_toggle0(10),igroup_toggle0(10)
integer :: group_Toggle1(10),igroup_toggle1(10)
integer :: group_Toggle2(10)
integer :: group_Toggle3(10)
integer :: group_Toggle4(10)
integer :: group_Toggle5(10)
real vgroup1(10),vigroup1(10)
real vgroup2(10),vigroup2(10)
real vgroup4(10),vgroup5(10),vgroup4_1(10)
real vgroup3(10)

character*30 text_box2(100,10)
integer :: i50a(10),j50a(10),iec50_ec50(10), iec50_x1(10),iec50_x2(10),iec50_i(10), iec50_j(10)
real ec50a(10),x1a(10),x2a(10)
integer ncomboname(10),ncombocell(10),ictype(10),icellarray(10),ifixmr(200)
character*20 ctitle1(10),textcell1(10,100),textcell2(10,100),comboname(10,100)
character	celltitle*30
logical icombo(10)
