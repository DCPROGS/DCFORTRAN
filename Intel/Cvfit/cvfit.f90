
Program cvfit
!	CALLBACKS:

!   
!	-100 TO -10 INITIALIZATION
!	-1	EXIT

!	1:1000 - ATTRIBUTES ( LIKE IN VPLOT AND VHIST)
!---------------------------------------------------------

!   1:400 2d attributes in subroutine graph_attributes
!	400:500 scale,shape
!   501-800 3D ATTRIBUTES
!	801-850  SAVE IMAGE
!	851-900  SAVE FILE ON DISC /EXPORT (PLOTQ)
!	901-950  PRINT
!	951-1000  HELP
!----------------------------------------------------------
!	5001- 6000 READING AND DRAWING AUTPLOT AND CVFIT FILES
!	6001- 7000 FITTING FOR CVFIT
!----------------------------------------------------------
!   7001-7100 graph callback
!	7100-7200 graph move
!   7201-7300 graph select
!----------------------------------------------------------- 
!This program is a combined version of HJCFIT and AUTPLOT
! PROGRAM_TYPE=-1  ALL
! PROGRAM_TYPE= 1  CVFIT
! PROGRAM_TYPE=2   AUTPLOT
! PROGRAM_TYPE=3   HCJFIT
! program_type=4 ekdist
!
! Modified to support 25 graphs 
! Reads plot queues can represent the first 100 records in the file on 
! multiple windows
! change and fits data form plotq
! improves and prints graphs representing plotq

! STORES information about  a record in a structure TYPE (RECORD_ATTRIBUTES) newrecords(:),oldrecords(:)
!	necessarly for multiple windowing -ideally we should work only with this structure - 
!   but this involves to replace all the arguments in the old routines:maybe in a next life!

!	PARAMETER NUMPOINTS=2048
!	parameter nump=100
!	PARAMETER NaSETS=20
!
!	TYPE AXES 
!		INTEGER ITX
!		INTEGER ITY
!		INTEGER NTX
!		INTEGER NTY
!		INTEGER NX1
!		INTEGER NY1
!		REAL XTIC
!		REAL YTIC
!		REAL XCROSS
!		REAL YCROSS
!		REAL TLENX
!		REAL TLENY
!		LOGICAL DOFRAME
!		logical calbarx
!		logical calbary
!	END TYPE AXES
!	TYPE (AXES) PARAM_AXIS

!	TYPE NUMBER
!		INTEGER NUMBX
!		INTEGER NUMBY
!		INTEGER INUMX
!		INTEGER INUMY
!		INTEGER INTXY
!		integer ntext
!	END TYPE NUMBER
!	TYPE (NUMBER) NUMBERS

!	TYPE GENERAL_ATTRIBUTES
!		INTEGER IFNT(100)
!		INTEGER IJUS(100)
!		INTEGER IDRAW(250)
!		INTEGER ICOL(250)
!		INTEGER ITYPE(250)
!		REAL SIZETEXT(100)
!		REAL ANGLE(100)
!		REAL THICK(250)
!		REAL RX(100)
!		REAL RY(100)
!		REAL RXBOX(4,100)
!		REAL RYBOX(4,100)
!	END TYPE GENERAL_ATTRIBUTES
!	TYPE (GENERAL_ATTRIBUTES) ATTRIBUTES

!	TYPE LINE
!		INTEGER NARROW
!		INTEGER NLINE
!		INTEGER NHLINE
!		INTEGER NVLINE
!		REAL XBEG(50)
!		REAL YBEG(50)
!		REAL XEND(50)
!		REAL YEND(50)
!	END TYPE LINE
!	TYPE (LINE) LINES
	
!	TYPE RECORD_ATTRIBUTES
!		LOGICAL hdisp
!		CHARACTER*150 STR(100)
!		integer iplotype
!		INTEGER IXP
!		INTEGER IYP
!		INTEGER IPOS
!		integer ILOG
!		INTEGER	NUMSETS
!		INTEGER NCURVd
!		INTEGER NCURVC
!		INTEGER NSFIT
!		INTEGER	ICURVd(NaSETS)
!		INTEGER	ICURVw(NaSETS)
!		INTEGER	jmiss(NaSETS)
!		INTEGER NdAt(NaSETS)
!		INTEGER NJ(NaSETS)
!		INTEGER JUSE(NaSETS)
!		integer isym(nasets)
!		INTEGER ijoin(nasets)
!		INTEGER NCAL(NaSETS)
!		integer iline(nasets)
!		INTEGER	ICURVC(NaSETS)
!		INTEGER NDV1
!		INTEGER NDIMD
!		INTEGER NDC1
!		INTEGER NDIMC
!		integer kwi
!		integer kwj
!		REAL XMIN
!		REAL YMIN
!		REAL XMAX
!		REAL YMAX
!		REAL WXMIN
!		REAL WYMIN
!		REAL WXMAX
!		REAL WYMAX
!		real dxs
!		real dys
!		REAL X0
!		REAL Y0
!		REAL YINF
!		REAL XVAL(NUMPoints,NaSETS)
!		REAL YVAL(NUMPoints,NaSETS)
!		real w(numpoints,nasets)
!		REAL XCAL(NUMPOINTS,NaSETS)
!		REAL YCAL(NUMPOINTS,NaSETS)
!		real symsiz(nasets)
!		TYPE (AXES) PARAM_AXIS
!		TYPE (NUMBER) NUMBERS
!		TYPE (LINE) LINES
!		TYPE (GENERAL_ATTRIBUTES) ATTRIBUTES
!	END TYPE RECORD_ATTRIBUTES

	
!IDRAW(i) = 1 to draw (without box) at the position that has been already
!			defined in rx,ry,rxbox,rybox (no need to define box again)
!	     = 0 to omit text
!	     =-1 to draw with box (imode=1), as for idraw=1 or underline(imode=2)
!	     =-2 when text position not yet defined; there are two sorts
!	     = 2 underline
!	     = 3 italic
!         
!
! IDRAW,ICOL,THICK,ITYPE,IFNT,ANGLE,IJUS,RXBOX,RYBOX,RX,RY,SIZES
!
!    1 = title,				
!	 2 = parameter values
!	 3 = x label, 			
!	 4 = y label
!    5 = z label
!	 6:30 = x numbers  (601:625)		
!    31:55 = y numbers  (701:725)       
!    56:80 = z numbers  (801:825
!    81:100 = extra text (901:920)
!
! IDRAW,ICOL,THICK,ITYPE
!
!    101-150= data/symbols 151-200= calc curves,
!    201-210= arrows       211-220= lines
!    221-230= h lines      231-240= v lines
!        241= cj bar          242= vj bar               243= sd bar
!        244= frame		245= axes
!
!  XBEG,YBEG,XEND,YEND
!      1- 10= arrows       11- 20= lines
!     21- 30= h lines      31- 40= v lines
!         41= cj bar           42= vj bar                43= sd bar
!
!***********************************************

 
	USE DFLIB
	use gino_f90
	use menu_f90
use hjcrecords
	include '\intel\common_files\cvfit_definitions.f90'
	include '\intel\common_files\graphics_definitions.f90'
	
ALLOCATABLE::CURVES,newrecords,oldrecords
	TYPE (RECORD_ATTRIBUTES) newrecords(:),oldrecords(:)
	TYPE (CALCULATED_CURVE) CURVES(:)
	TYPE (FILE$INFO) info

	type (GACTION) :: actlst
	type (Gwidget) :: widget
	

	common/ptext/ameant,areat
	common/limit/wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
	xmin,xmax,ymin,ymax
	COMMON/TPOS/IDRAW,ICOL,THICK,C_THICK,ITYPE,IFNT,ANGLE,IJUS,&
     SIZEtext,RXBOX,RYBOX,&
     RX,RY,NARROW,NLINE,NHLINE,NVLINE, XBEG,YBEG,XEND,YEND,&
     NTEXT,NEWTEXT,CNUMX,CNUMY,CNUMZ,CEXPX,CEXPY,CEXPZ,&
     NUMBX,NUMBY,NUMBZ,IHLINREL,IVLINREL
	common/calib/calbarx,calbary,xbeg4,ybeg4,xend4,yend4,&
         xbeg5,ybeg5,xend5,yend5

	common/logval/logx,logy,sqrty,logity

	COMMON/JLOGOS/t1c,t2c,t1v,t2v,xoff1,y1v,y2v,y1c,y2c,ncjump,nvjump,ivplot
	COMMON/mtrace/ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil
	
	common/text_attr/bold,italik,underline,ipow,idev,nolabx,ifontrue

	
		common/plotopt/ipl1,ipl2,ipl3,ipl4,ipl5,ipl6,icprev
	common/iniset/nodata,nofit,autosim,curvonly
	COMMON/BLOCK2/castar,X1,X2,iequiv,ip1,ip2
	COMMON/BLOCK3/logyfit,norm,xnorm
	common/fix/fixratio,ir1,ir2,rval	!to fix ratio of 2 parameters
	common/potrat/jfirst,iset,kmax1	
	common/dp/discprt
	common/abt/abort
	common/pwrfunc/ybarp
	common/pixpos/ixgrid,iygrid,ixpix,iypix,wid,landscape
	common/other/iparfirst	
	logical rescalex
    common/tty/ittypanel,itty

	common/pen/penalty,penfunc,penfac 

				!for SIMPLEX, HJCLIK
	common/nblk/ngp(10),an(10),nscal(10),first    !ngp(j) for set j (for hjclik only)

	common/setblk/oneset,iset9	!for HJCLIK,DISP to specify one set

	COMMON/BLOCK1/constr,nset,nfit,nsfit,Xv,kmax,ncomp,&
	nmod,fline,nomit,jomit,jset,ifitmode
	common/lcomp/nset1,fcomp				!for SIMPLEX, HJCLIK
	common/CBLK/nset0,conc,jsetlast
	common/KBLK/kA,kB,kC,kD
	common/QDBLK1/QT,QD
	common/QDBLK2/npar1,IQf,irate1,jrate1,nlig   !hjclik, qset_hjc only
	common/root/rootA,rootF	!hjclik,popadj,fcrqt,fmrqt,popadjc,depend,mopadjc
	common/inroot/s1A,s2A,s1F,s2F	  !for init guesses for roots; hjclik only
	common/eigsav/eigAAsav,eigFFsav
	common/rootsav/rootAsav,rootFsav		!hjclik only
	common/inroots/s1Asav,s2Asav,s1Fsav,s2Fsav !for init guesses for roots (hjclik only)
	common/detw2/tres,km,nerr	!for DETWA,DETWF, hjclik, hjcasymp, hjcexact
	common/detw1/QAA,QAF,QFF,QFA,EXPQF,EXPQA   !for DETWA,DETWF,dARSdS -now param
	common/detw3/WA,WF
	common/queue/qfile	!for vplot and vhist
	common/resblk/tresd	!for hjclik only
	common/rblck/treso5,tresg5,acrit5,avamp5
	
	common/tpf/titpfree,iptit 
	common/amax/assmax,icdep,ratemax    
	common/cpar/ncdep,IX,JX,x
	common/rblck/treso,tresg,acrit,avamp
		real*8 det
	COMMON/determ/det			!for matinv
	common/perr/nerr2
		common/LIG/nligsav,IL(100)
	common/attributes_flag/d_line,d_hline,d_vline,d_arrow,d_poly,d_text,d_zoom,o_state,&
		c_state,s_text,s_line,s_arrow,zoom,link,delete_state,move_state                  
	
common/details/textcomp,textid,drives
common/type/program_type,nsetin
common/plotatr/new_file,open_file,import_file,izoomy, &
           isave_bmp,isave_wmf,export_file,print_file,exit_file,view_record,&
		   view_data,title_record,iparameters,labels,&
		   jtitle_record,jparameters,jlabels,jnewtext,jnumbers,jaxis,jframe,&
		   jlines,jarrows,jraw_data,jcurves,label_x,label_y, &
		   label_z,number_x,number_y,number_z,Icon1_1,Icon1_2,Icon1_3,Icon1_4,&
		   Icon1_5,Icon1_6,Icon1_7,Icon1_8,Icon1_9,Icon1_10,Icon1_11,Icon1_12,&
		   Combo1_1,Combo1_2,Combo1_3,Combo1_4,Combo1_5,Combo1_6, combo1_8,combo1_9,combo1_10,&
		   toolbar1_1,toolbar1_2,toolbar1_3,Toolbar1_4,&
		   new_text,new_lines,new_arrows,new_lines_v,new_lines_h,&
		   i3d,irot,iview3d,iaratxy,iarathb,igridd,isurfdr,icross,ifill3d,imarkbad

     nx=1
     ny=1
     ndx=1
     ndy=1
     iyd=4
	 ixd=4                           
!	pfilem='cvfit.txt'
	km=100	!in COMMON -dimension of QD, QT etc
	kAM=10	!max number of open states
	kFm=90	!max number of shut states
	nlvar=-1	!until defined
	nsims=1	!number of simulated expts fitted =1,2,...,nsim
	ireset=1	!for hjclik
	perfac=0.05d0	!maximum random perturbation
	ngpcheck=100
	excopen=.false.
	rescale=.false.
	do id=1,100
		idraw(id)=-2
	enddo
	mono=.false.
	program_type=1
	if(program_type.eq.2) dfilt=dfiltq
	igraph=0
	ngraph=0
	igrorig=0
	titw(1)='Weights constant. Error from residuals.'
	titw(2)='Weights from specified s(Y): errors from weights'
	titw(3)='Calculate weights from s(Y) = a + b*X'
	titw(4)='Calculate weights from s(Y) = a + b*Y '
	titw(5)='Use specified weights for fit, but error from residuals'

	idev=0
	titmod(1)='Polynomial (inc. straight line)'			!nmod=99
	titmod(2)='Langmuir hyperbola(s) (inc. or dec.)'		!nmod=1/26
	titmod(3)='Hill equation(s) (inc. or dec.)'!nmod=-1/27
	titmod(4)='Langmuir hyperbola(s) + straight line'	!nmod=26			!nmod=2
	 titmod(5)='Hill equation(s) + straight line'  		!nmod=27			!nmod=2
	 titmod(6)='Power function (linear on log-log plot)'
	 titmod(7)='Binding inhibition curve (parameter=KB)'		!nmod=5
	titmod(8)='Exponential binding onset curves'

	titmod(9)='General fit of sums of exponentials'
	titmod(10)='General fit of sums of geometrics'
	titmod(12)='Exponential voltage dependence of rate constant'
	titmod(20)='Equations for P(open) curves'
	titmod(30)='Equation for I/V curves'

	titmod(21)='Popen for KM2 model+ block. K1=K2'
	titmod(22)='Popen for KM2 + block (BETA as param). K1=K2'
	titmod(23)='Popen for KM2 at equilibrium + block: SEP K1,K2'
	titmod(24)='Popen for KM2 + block (BETA as param)'
	titmod(25)='Popen for 2 independent KM1 subunits'
	titmod(26)='Popen for KM2 (Coop and nonequiv) + block'
	titmod(27)='Popen for KM2 (Coop and nonequiv)+block (beta as param)'
    titmod(28)='Bind n mols + opening: # of channels same for all sets'
    titmod(29)='MWC mechanism (any n): # of channels same for all sets'
	titmod(31)='Total current (I/V) for KM2 at equil + block (K1=K2)'
	titmod(32)='I/V with exponential voltage-dependence of current'
	titmod(33)='Ditto with saturation (beta/alpha V-dep)'
	titmod(34)='Ditto with saturation (affinity V-dep)'
	titmod(35)='I/V for GHK with Mg block'
	titmod(40)='Woda equation'
	
	SCARA(1)='Arithmetic'
     	SCARA(2)='LogX/Y'
     	SCARA(3)='X/LogY'
     	SCARA(4)='LogX/LogY'
     	SCARA(5)='Hill Plot'
     	SCARA(6)='SquareY/X'
     	SCARA(7)= 'SquareY/LogX'

	jmodel=300
	indr=0
	ijplot=200
	sdim=3.0
	do i=1,100
		nplot_on(i)=-1
	enddo
	iflag=0
	ndth=100
	ncalc=501
	ncurvc=0
	autplt=.false.
	plotcols=.true.
	idev=0
	angit=5.0
		ifont0=2
	mplot=0
	do i=1,100
	  ANGLE(i)=0.
	  ifnt(i)=2
	  sizetext(i)=4.0
	  istat(i)=-1
	  ifstat(i)=-1
	  iopen(i)=-1
	enddo
	do i=1,250
	  idraw(i)=-2		!until defined
	
	  c_thick=1.
	  itype(i)=0
	  icol(i)=-1
	enddo
	idraw(243)=1		!c-jump logo
	idraw(244)=1		!v-jump logo
	ijus(2)= 0
	sizetext(2) =12./4.	!12 point for param values
	ijus(3)=0		!for x-axis label
	ijus(4)=0
	ANGLE(4)=90.		!for y-axis label
	itx=1
	ity=1
	ilog=0
	izoom=0
	super=.false.	!last fit not superimposed
	ivplot=.false.
	inumx=0
	inumy=0
	iscal=1		!scale internally
	xlo=-1		!whole screen
	ntx=5
	nty=5
	ijus(6)=0
	ijus(31)=-1							
	ixgraph=24
	iygraph=18
	ncols=4
	nrows=20
	NJSET=20
	NIOBS=100
	ndimc=20
	ndc1=ncalc
	ndv1=niobs
	ndimd=njset !!
							
	kmax=20
	maxplot=100
	saveandplot=.false.
	if(njset.gt.20) ndimc=njset
	IW=1
	if(idest.ne.2) idest=0
	ixm=20
	iym=4
	ix=160
	iy=80
	ALLOCATE(xobs(Niobs,NJSET),yobs(Niobs,NJSET),w(NIOBS,NJSET))
!	ALLOCATE(xval(NIOBS,NJSET),yval(NIOBS,NJSET))
	!ALLOCATE(xval1(NIOBS,NJSET),yval1(NIOBS,NJSET))
	ALLOCATE(xvalold(NIOBS,NJSET),yvalold(NIOBS,NJSET))
    ALLOCATE(nj(NJSET),jmiss(njset),juse(njset),ndat(njset),ncal(ndimc))	!for normalised data
    ALLOCATE(setx(njset),njbase(njset),icurvw(njset),icurvc1(njset))
	allocate( icurvd(njset),icurvc(njset))
!	allocate(isym(njset),symsiz(njset),ijoin(njset),iline(njset))
	!allocate(isym1(njset),isym1(njset),syms1(njset),ijoin1(njset),iline1(njset))
	allocate(xnum(20,3),xdata(10,10,4,10),ndatset(200,20))
	allocate(newrecords(25),oldrecords(25),curves(25))
!	allocate(theta(100),thetgues(100,njset))
	allocate (theta1(100),pdata(100))  
!    ALLOCATE(xcal(ndc1,ndimc),ycal(ndc1,ndimc))
	allocate(pixbuf(24000))
! Initialise Gino, Device & Menu
	do i=1,ndimc
		ncal(i)=0
	enddo
	
nplot=1
IFILTYPE=1
iplotype=1
iptype=1
	nab=0		!for save of alpha and beta values
	sbin=.false.	!for HJCDAT
	shist=.false.	!for HJCDAT
	sres=.false.	!for HJCDAT
	sexp=.false.	!for HJCDAT
	idiskq=-1		!until defined
	nset=1		!default number of data sets

	idebug=1		!no debugging
	idsav=1	
	autosim=.false.
	nofit=.false.
	nodata=.false.
	liksurf=.false.
	prtec50=.false.
	penalty=.false.
	logfit=.false. 	!except when hjclik called from simplex

	curvonly=nofit.and.nodata

	nfix=0
	neq=0
	ncyc=0
	do i=1,200		!ktd=200
	  jfix(i)=0		!to mark fixed params
	 
	enddo

   call gOpenGino
   call gGuiwin
   
   call gmInitializeMenu
   call gmSetGuiGridMode(GON)
   call gmenqguigrid(ixgrid,iygrid,ixpix,iypix)
   if(ixgrid.lt.36) then
	ixng=48
	iyng=36
   else if (ixgrid.ge.36.and.ixgrid.lt.43) then
	ixng=40
	iyng=30
   else if (ixgrid.ge.44.and.ixgrid.lt.50) then
	ixng=36
	iyng=27
   else
	ixng=32
	iyng=24
   endif
   ifitplot=10
   call gmdefineguigrid(ixng,iyng)
 call gmenqguigrid(ixgrid,iygrid,ixpix,iypix)
 !  call gsetsoftchars()
  call gsethardchars()
   call gsetbrokenlinemode(gon)
	iwidth_main=40
	iheight_main=32
   call gSetEscapeChar('£')
   call define_colours(1,.true.)
   call gSetCharFont(ifont0)
   call gSetLineEnd(GROUND)
call gseterrormode(gerroron)
   call main_window(program_type,Main,imainpanel,cdatew,modelw,eqfit,Status_bar1)
   
     call ACTWIN(Main)	

   call gmManage
  
   call gmDefineKeyselectCallback(13,-11) 
goto 909
			ntog=10
			text_tog(1)='Open/New'
			text_tog(2)='Display/Enter values'
			text_tog(3)='Graph'
			text_tog(4)='MR'
			text_tog(5)='Constraints'
			text_tog(6)='Fix par'
			text_tog(7)='Open f/m'
			text_tog(8)='Fit'
			text_tog(9)='Display'
			text_tog(10)=''

ilg=ntog+2
iwidp=6
iwtog=4
itcall=7300
infolength=iwidth_main-11
itogglepanel=gmCreatePanel(imainpanel,1 , 1, iwidp, ilg, &
              	gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmborderType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=150, gmFillBorder=0)
do i=1,ntog
inToggle(i) = gmCreateToggleButton(itogglePanel, 1, i, iwtog, 1, text_tog(i), istat11, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)!, gmCallBAck=itcall+i)
enddo


infopanel=gmCreatePanel(imainpanel, 8, 1, iwidth_main-17, ilg, &
              	gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmborderType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=150, gmFillBorder=0)

ittypanel=gmCreatePanel(imainpanel, 1, ilg+2, iwidth_main-9, iheight_main-ilg-5, &
              	gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmborderType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

itty= gmCreateTTYEntry(ittypanel, 0, 0, iwidth_main-9, iheight_main-ilg-5, 100,'',&
				gmoffcol=1,gmoncol=1,gmtextcol=14) 

909 continue
infopanel=gmCreatePanel(imainpanel, 2, 2, iwidth_main-4, iheight_main-6, &
              	gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmborderType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=150, gmFillBorder=0)
iwel=-100
!call progress_table(main)
current_child = title_record
itcall=5001
ishow=-1
filnam='cvfit.txt'
iresult = FULLPATHQQ ('cvfit.exe', adir)
	nb=nblank1(adir)

	pfilem=adir(1:nb-9)//filnam
	pfilt='*.txt'//char(124)//'Print file (txt)'//char(124)//&
						  '*.prt'//char(124)//'Old print files'
pdir=adir(1:nb-9)
show_curve=.true.
call cvfit_ini(main,initwin,intoggle,itcall,itogbutton,pfilem,initwin_Text2)


1	continue
! Action loop
   do while (gmAction(callid) /= -1)
   call gmEnqWidgetInfo(main,Widget)
   ixmain=widget%xpos
   iymain=widget%ypos
	call gmEnqActionState(actlst)
!	if(actlst%status.eq.) iopen(
	if(actlst%key.eq.13.and.iwel.eq.-100) then
	callid=-11
	iwel=0
	goto 2
!		if (istate.ge.1.and.lframe.eq.graphics1_2(modplot)) then
!			call show_par_mechanism(modplot,xmov,ymov,models,ixp,iyp,pixbuf,show)
!		endif
	endif

2	continue
	
		
    
	select case(callid)
	
	! INITIALIZE
	!=====================================================================================
		case(-2)
			m_mouse=.true.
			r_mouse=.false.
		case(-3)
			r_mouse=.true.
			m_mouse=.false.
			link=.false.
			call gmSetGuiCursor(lframe,Gdefault,GDEFAULT)
			ilink=0
		
	
	

!=================================================================================
	!Attributes
  	!(part of vplot5 SECTION)

!=================================================================================
! GRAPH ATTRIBUTES
!===================================================================================	
				
		  case(1:400) ! edit,fonts,colour,size,box,delete,etc
			
			call graph_attributes(main,callid,modplot,lframe,jindex,ind_1,oldrecords,readrec,newfile,&
		graphics1_1,combo1_6,combo1_4,combo1_5,combo1_8,combo1_9,combo1_10,istate,hdisp,izoom,xtitle,ytitle,&
		xmin0,xmax0,ymin0,ymax0,itogglepanel,itext_entry,new_text_entry,ikplot,iptype,rescalex,&
		isym1,symsiz1,combo1_11,combo1_12,imodax,iaminax,iamaxax,iatic,&
		imtext3,irxx,iryy,&
		calbarx,calbary,xbeg4,ybeg4,xend4,yend4,&
		ivalx1,ivaly1,ivalx2,ivaly2,ivaltx,ivalty,iax_entry)
	
		if(callid.eq.247.or.callid.eq.262) then
			callid=405
			goto 2
		endif
			if(callid.eq.407) goto 2
		  case(401,402,405) ! Shape,scale
		
		  if(callid.eq.401) then
			do i=2,6
					ifstat(i)=gmEnqListEntry(Combo1_1,i,textcombo)
					if(ifstat(i).eq.2) ishp=i-2
			enddo
			IF(ISHP.EQ.4) THEN
				IVPLOT=.TRUE.
			ELSE
				IVPLOT=.FALSE.
				DOFRAME=.TRUE.
			ENDIF
		  else  if(callid.eq.402) then ! scale
			do i=2,8
					ifstat(i)=gmEnqListEntry(Combo1_2,i,textcombo)
					if(ifstat(i).eq.2) ilog=i-2
			enddo
          endif
		   	
			if(readrec.and.modplot.gt.0.and.oldrecords(modplot)%ipos.eq.0) then
			if(allocated(xval)) deallocate(xval,yval,w)
						if(allocated(xcal)) DEALLOCATE(Xcal,Ycal)
						if(allocated(icurvw)) DEALLOCATE(icurvw)
						if(allocated(ndat)) DEALLOCATE(ndat,isym,ijoin,symsiz)
						if(allocated(ncal)) DEALLOCATE(ncal,iline)
						if(allocated(icurvd)) DEALLOCATE(icurvd,icurvc)
						kwi=oldrecords(modplot)%kwi
						kwj=oldrecords(modplot)%kwj
						ndv1=oldrecords(modplot)%ndv1
						ndimd=oldrecords(modplot)%ndimd
						ndc1=oldrecords(modplot)%ndc1
						ndimc=oldrecords(modplot)%ndimc
						ipos=oldrecords(modplot)%ipos
						n1=1
						if(oldrecords(modplot)%hdisp) n1=0
						ALLOCATE(Xval(n1:ndv1,ndimd),Yval(n1:ndv1,ndimd),Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
						ALLOCATE(icurvw(ndimd),w(kwi,kwj))
						ALLOCATE(ndat(ndimd),isym(ndimd),ijoin(ndimd),symsiz(ndimd))
						ALLOCATE(ncal(ndimc),iline(ndimc))
						ALLOCATE(icurvd(ndimd),icurvc(ndimc))
						isens=0
			
				call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xval,yval,w,nj,niobs,njset,nplot,nset,&
				juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
				wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
				ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
				xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,oldrecords,&
				dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,parval,ifitype)
				
				xbeg4=xbeg(44)
						xend4=xend(44)
						ybeg4=ybeg(44)
						yend4=yend(44)
						xbeg5=xbeg(45)
						xend5=xend(45)
						ybeg5=ybeg(45)
						yend5=yend(45)
				titlex=xtitle(1:40)
				titley=ytitle(1:40)
			!!	ndimd=njset
					if(callid.eq.405) then
				redrawn=.true.	!graph will have xtitle etc drawn in LAXES
				autplt=.false.
				izoom=1
			    iscal=1
				rescale=.true.
			else
				autplt=.false.
				rescale=.true.
		 redrawn=.false.
				izoom=0
				iscal=1
				if(callid.eq.402) iscal=5
			iscal=5
			endif
		
		  if(readrec.and.oldrecords(modplot)%IPOS.eq.0) then
		    rescale=.true.
			autplt=.false.
			n1=1
			mono=.false.
			call calc_default(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,symsiz,&
				ndimd,XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,&
				XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,&
				XLO1,XHI1,YLO1,YHI1,y0,yinf,inumx,inumy,&
				titlex,titley,ilabel,doframe,autplt,itit,title1,&
				ISHP,ifont,landscap,ndv1,ndc1,w,kwi,kwj,icurvw,kmax,iver,&
				redrawn,plot,redo,pcgm,pwmf,npint,hdisp,n1,tlenx,tleny,nx1,ny1,izoom,&
				xtitle,ytitle,rescale,dxs,dys,ifitype,parval,theta,ncomp,isval,sval,&
								ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil)	
				ipos0=ipos
			
				call gmRemoveWindow(graph1_1(modplot))
			
				ipos0=ipos
			!	call gmActivateGraphicsFrame(graphics1_1(modplot))
				jplot=oldrecords(modplot)%iplot
				call graph1(modplot,jplot,main,ixp,iyp,graph1_1,GraphMainPanel1_1,graphics1_1,&
					wxmin,wxmax,wymin,wymax,ipos,gfile,.false.,ixposv,iyposv)
			
				ipos=ipos0

				call draw_axis(xmin,xmax,ymin,ymax,ymin1,ymax1,xtic,ytic,&
					nx1,ny1,xcross,ycross,itx,ity,tlenx,tleny,logx,logy,sqrty,doframe,&
					icol,ntx,nty,idev,thick,itype,calbarx,calbary,xbeg4,ybeg4,xend4,yend4,&
                    xbeg5,ybeg5,xend5,yend5)
				call draw_text(parval,xtitle,ytitle,ztitle,title1,cnumx,cnumy,&
					cnumz,cexpx,cexpy,cexpz,newtext,numbx,numby,numbz,ntext,inumx,&
					inumy,intx,angle,sizetext,ijus,thick,rx,ry,idraw,icol,ifnt,str,dxs,dys)
				if(hdisp) then
					call draw_hist(xval,yval,icurvd,ncurvd,ndelt,logity,logx,&
						logy,sqrty,y0,yinf,x0,ilog,xmin,xmax,ymin,ymax,idev,ndat,ijoin,&
						icol,thick,ndv1,ndimd,xwbase,lt2)
								
                else
					call draw_data(xval,yval,icurvd,ncurvd,ndelt,logity,logx,logy,sqrty,&
						y0,yinf,x0,ilog,idev,ndat,ijoin,icol,isym,&
						symsiz,thick,barleng,interp,npint,Xint,Yint,nint,yp1,ypn,&
						Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw,&
						ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil)
				endif
				call draw_curves(xcal,ycal,icurvc,ncurvc,logity,logx,&
					logy,sqrty,y0,yinf,x0,ilog,idev,&
						wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
						xmin,xmax,ymin,ymax,ncal,&
					iline,icol,thick,ndc1,ndimc,jfirst,dxs,dys)
               	if(idev.eq.3) show_curve=.false.
               		if(show_curve) then
					ymin20=ymin
							ymax20=ymax
							if(sqrty) ymin20=sqrt(ymin)
							if(sqrty) ymax20=sqrt(ymax)
							xt=0.5*(xmin+xmax)
							yt=ymax20+0.15*(ymax20-ymin20)
							ytc=0.07*(ymax20-ymin20)
								if(ncurvd.gt.0) call write_string('Data:',xmax+0.11*(xmax-xmin),ymax20+0.01*(ymax20-ymin20),0.,0,101,&
								3.,icol(151),dxs,dys)
								do j1=1,ncurvd
								if(iline(j1).ne.-1) then
								j=icurvd(j1)
								
								sx0=symsiz(j)*0.7*dxs
								sy0=symsiz(j)*0.7*dys
							    call intconv(j,cnum0)
								call write_string(cnum0,xmax+0.05*(xmax-xmin),ymax20-ytc,0.,0,101,&
								3.,icol(j+100),dxs,dys)
							
								call jSYMBOL(xmax+0.1*(xmax-xmin),ymax20-ytc,isym(j),sx0,sy0,icol(j+100),idev)   
767								ytc=ytc+0.07*(ymax20-ymin20)
								
								endif
								enddo
								
					endif
			    call draw_lines(IDRAW,ICOL,THICK,C_THICK,NARROW,NLINE,NHLINE,NVLINE, &
				XBEG,YBEG,XEND,YEND,y0,yinf,x0,dxs,dys)
				isens=1
				call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xval,yval,w,nj,niobs,njset,nplot,nset,&
				juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
				wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
				ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
				xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,oldrecords,&
				dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,parval,ifitype)
				endif
				zoom=.false.
				izoom=1
				ipos=0
				rescale=.false.
				iscal=0
	
				endif
case(407)
		if(allocated(xval)) deallocate(xval,yval,w)
						if(allocated(xcal)) DEALLOCATE(Xcal,Ycal)
						if(allocated(icurvw)) DEALLOCATE(icurvw)
						if(allocated(ndat)) DEALLOCATE(ndat,isym,ijoin,symsiz)
						if(allocated(ncal)) DEALLOCATE(ncal,iline)
						if(allocated(icurvd)) DEALLOCATE(icurvd,icurvc)
						kwi=oldrecords(modplot)%kwi
						kwj=oldrecords(modplot)%kwj
						ndv1=oldrecords(modplot)%ndv1
						ndimd=oldrecords(modplot)%ndimd
						ndc1=oldrecords(modplot)%ndc1
						ndimc=oldrecords(modplot)%ndimc
						ipos=oldrecords(modplot)%ipos
						n1=1
						if(oldrecords(modplot)%hdisp) n1=0
						ALLOCATE(Xval(n1:ndv1,ndimd),Yval(n1:ndv1,ndimd),Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
						ALLOCATE(icurvw(ndimd),w(kwi,kwj))
						ALLOCATE(ndat(ndimd),isym(ndimd),ijoin(ndimd),symsiz(ndimd))
						ALLOCATE(ncal(ndimc),iline(ndimc))
						ALLOCATE(icurvd(ndimd),icurvc(ndimc))
						xbeg4=xbeg(44)
						xend4=xend(44)
						ybeg4=ybeg(44)
						yend4=yend(44)
						xbeg5=xbeg(45)
						xend5=xend(45)
						ybeg5=ybeg(45)
						yend5=yend(45)
				titlex=xtitle(1:40)
				titley=ytitle(1:40)
		isens=0
		call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xval,yval,w,nj,niobs,njset,nplot,nset,&
				juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
				wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
				ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
				xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,oldrecords,&
				dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,parval,ifitype)
		if(jindex.le.150) then	
			
			if(hdisp) then
				call draw_hist(xval,yval,icurvd,ncurvd,ndelt,logity,logx,&
				logy,sqrty,y0,yinf,x0,ilog,xmin,xmax,ymin,ymax,idev,ndat,ijoin,&
				icol,thick,ndv1,ndimd,xwbase,lt2)
								
            else
			icol(jindex)=0

				call draw_data(xval,yval,icurvd,ncurvd,ndelt,logity,logx,logy,sqrty,&
				y0,yinf,x0,ilog,idev,ndat,ijoin,icol,isym,&
				symsiz,thick,barleng,interp,npint,Xint,Yint,nint,yp1,ypn,&
				Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw,&
						ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil)
				icol(jindex)=oldrecords(modplot)%attributes%icol(jindex)
			
				call draw_data(xval,yval,icurvd,ncurvd,ndelt,logity,logx,logy,sqrty,&
				y0,yinf,x0,ilog,idev,ndat,ijoin,icol,isym,&
				symsiz,thick,barleng,interp,npint,Xint,Yint,nint,yp1,ypn,&
				Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw,&
						ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil)
			endif
		else if(jindex.ge.151.and.jindex.le.200) then
			icol(jindex)=0
			call draw_curves(xcal,ycal,icurvc,ncurvc,logity,logx,&
			logy,sqrty,y0,yinf,x0,ilog,idev,&
			wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
			xmin,xmax,ymin,ymax,ncal,&
			iline,icol,thick,ndc1,ndimc,jfirst,dxs,dys)
			icol(jindex)=oldrecords(modplot)%attributes%icol(jindex)
			
			call draw_curves(xcal,ycal,icurvc,ncurvc,logity,logx,&
			logy,sqrty,y0,yinf,x0,ilog,idev,&
			wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
			xmin,xmax,ymin,ymax,ncal,&
			iline,icol,thick,ndc1,ndimc,jfirst,dxs,dys)
		if(idev.eq.3) show_curve=.false.
		if(show_curve) then
			ymin20=ymin
							ymax20=ymax
							if(sqrty) ymin20=sqrt(ymin)
							if(sqrty) ymax20=sqrt(ymax)
							xt=0.5*(xmin+xmax)
							yt=ymax20+0.15*(ymax20-ymin20)
							ytc=0.07*(ymax20-ymin20)
								if(ncurvd.gt.0) call write_string('Data:',xmax+0.11*(xmax-xmin),ymax20+0.01*(ymax20-ymin20),0.,0,101,&
								3.,icol(151),dxs,dys)
								do j1=1,ncurvd
								if(iline(j1).ne.-1) then
								j=icurvd(j1)
								
								sx0=symsiz(j)*0.7*dxs
								sy0=symsiz(j)*0.7*dys
							    call intconv(j,cnum0)
								call write_string(cnum0,xmax+0.05*(xmax-xmin),ymax20-ytc,0.,0,101,&
								3.,icol(j+100),dxs,dys)
							
								call jSYMBOL(xmax+0.1*(xmax-xmin),ymax20-ytc,isym(j),sx0,sy0,icol(j+100),idev)   
								ytc=ytc+0.07*(ymax20-ymin20)
								
							endif
								enddo
								
							endif
		else if(jindex.ge.201.and.jindex.le.240) then
		call draw_lines(IDRAW,ICOL,THICK,C_THICK,NARROW,NLINE,NHLINE,NVLINE, &
		XBEG,YBEG,XEND,YEND,y0,yinf,x0,dxs,dys)

		else if (jindex.eq.244.or.jindex.eq.245) then
		icol(jindex)=0
			call draw_axis(xmin,xmax,ymin,ymax,ymin1,ymax1,xtic,ytic,&
			nx1,ny1,xcross,ycross,itx,ity,tlenx,tleny,logx,logy,sqrty,doframe,&
			icol,ntx,nty,idev,thick,itype,&
            calbarx,calbary,xbeg4,ybeg4,xend4,yend4,xbeg5,ybeg5,xend5,yend5)
            
			icol(jindex)=oldrecords(modplot)%attributes%icol(jindex)
	
			call draw_axis(xmin,xmax,ymin,ymax,ymin1,ymax1,xtic,ytic,&
			nx1,ny1,xcross,ycross,itx,ity,tlenx,tleny,logx,logy,sqrty,doframe,&
			icol,ntx,nty,idev,thick,itype,&
            calbarx,calbary,xbeg4,ybeg4,xend4,yend4,xbeg5,ybeg5,xend5,yend5)
		endif

    case(801) !(14,20,18)    ! Save as bmp/wmf
	if(readrec) then
			if(modplot.gt.0.and.lframe.eq.graphics1_1(modplot)) then
					callid=modplot+5200
					goto 2
			else
				imessy=gmDisplayMessageBox('',&
				'Select graph',Gexclamation,gok)	
			endif
    else
			imessy=gmDisplayMessageBox('','No record on screen/Not available for this record',Gexclamation,gok)
	endif


	case(901)! print
	
	if(readrec) then
			if(modplot.gt.0.and.lframe.eq.graphics1_1(modplot)) then
					callid=modplot+5200
				    goto 2
			else
				imessy=gmDisplayMessageBox('',&
				'Select graph',Gexclamation,gok)	
			endif
    else
			imessy=gmDisplayMessageBox('','No record on screen/Not available for this record',Gexclamation,gok)
	endif


		case(950)
		CALL gmFileBrowser(pFILEm,pDIR,pFILT,gmType=GinPUT, &
				gmTitle='Print files')
		continue
		nl=nblank1(pdir)
		pfilem=pdir(1:nl)//'\'//pfilem
		call gmsettextsetting(initwin_Text2,pfilem)		
		

	case(951) !(611)
		 call helpcvfit(main,indlist,indlista,list,nlist,idkbutt,idsbutt,951)		
	
	case(961) !(613)
		call helpcvfit(main,indlist,indlista,list,nlist,idkbutt,idsbutt,961)
	case(962) !(614)
		ISTAT12=0
	   DO KI=1,Nlist
           CALL LISENQ(INDLISTA,KI,LIST(KI),ISTAT2)
           IF(ISTAT2.EQ.2)THEN  ! SELECTED :
	!	   call mechelp(main,ihelp1,ki,0)
           goto 1
	     ENDIF
	   ENDDO
	case(971) !(610)
		 call helpcvfit(main,indlist,indlista,list,nlist,idkbutt,idsbutt,971)
	case(972) !(615)
	   ISTAT13=0
	   DO KI=1,Nlist
           CALL LISENQ(INDLISTA,KI,LIST(KI),ISTAT2)
           IF(ISTAT2.EQ.2)THEN  ! SELECTED :
		 !  call mechelp(main,ihelp1,ki,615)
           goto 1
	     ENDIF
	   ENDDO
	case(981) !(602)
		 call helpcvfit(main,indlist,indlista,list,nlist,idkbutt,idsbutt,981)
	case(982) !(603)
	   ISTAT1=0
	   DO KI=1,Nlist
           CALL LISENQ(INDLISTA,KI,LIST(KI),ISTAT1)
           IF(ISTAT1.EQ.2)THEN  ! SELECTED :
		   call texthelp(main,ihelp,ki,helps,nhelp,program_type)
           goto 1
	     ENDIF
	   ENDDO
	   GOTO 1
    


	
	CASE(998) !(604)
	    CALL WINREM(INDLIST)
	    CALL ACTWIN(0)
	    GOTO 1

	case(999)	!(601) !help
		call about_ucl(Main,Form12,title,iprogram_type)


!===========================================================================


! import files(graphs) for display  from plotqs
!=================================================================================

	case(5001) !13	!Open
	if(ishow.eq.-1) then
			
			istat2=gmenqtoggleswitch(inToggle(11))
			if(istat2.eq.gon) then 
				discprt=.true.
				apfile=.true.
			else
				istat2=gmenqtoggleswitch(inToggle(12))
				if(istat2.eq.gon) then 
					discprt=.true.
					apfile=.false.
				else
					discprt=.false.
					apfile=.false.
				endif
			endif
		
			call gmremovewindow(initwin)
			ishow=0
	endif
			newfile=.false.
			if(.not.allocated(curves)) allocate(curves(25))
			if(newfile.and.saveandplot) then
				do i=1,100
					icfit(i)=0
				enddo
				dfile=nwfile
				ddir=ndir
			else
				do i=1,100
					icfit(i)=0
					iopen(i)=-1
					jopen(i)=-1
				enddo
				CALL gmFileBROWSER(DFILE,DDIR,DFILT,gmBrowseType=0)
			
			endif
			present=.false.
        	INQUIRE (FILE=DFILE,EXIST=PRESENT,&
            ACCESS=FACCESS,FORM=FFORM,RECORDTYPE=FRTYPE,RECL=IRCL) 
            if(PRESENT) then
			    IF(dFILE.ne.' ') then
		

			    if(allocated(xdata).and..not.newfile) DEALLOCATE(xdata)
			    do i=1,njset
				jmiss(njset)=0
			    enddo
			    call gmSetListEntry(combo1_3,GSELECT,gmEntry=1)
			    if(discprt) then
		
			    OPEN(unit=7,file=pfilem,status='UNKNOWN',access='APPEND',form='FORMATTED',carriagecontrol='FORTRAN')
			    if(.not.apfile) REWIND(unit=7)
			
			    write(7,102) cdatew
102			    format(' Curve fitting program; Date of analysis: ',a11,/)
				write(7,103) textcomp,textid,drives
103		        format(' Computer : ',a40,/, ' User : ',a40,/,' Drives : ',a26,/)
				open7=.true.
			    endif
			    call openfile(MAIN,RECORDS,nplot,IFILTYPE,iplotype,iptype,dFILE,FOPEN_11,FOPEN_12,&
			    APFILE,DISCPRT,njset,ncolr,nrowr,label,ftitle,nset,iwid,List7_1, &
			    ftitle1, button7,button6,saveandplot,program_type)
			    if(ifiltype.eq.3) nplot=1
			
			    ALLOCATE(xdata(nplot,njset,ncols,niobs)) 
			    import=.false.
				if(saveandplot) then
				if(igraph.lt.1) igraph=1
				modplot=igraph
				nentry=nplot
				istat(modplot)=2
				iopen(modplot)=2
				callid=5011
			
				goto 2
			    endif
			    endif
			else
			    nwfile=dfile
			    IF(nWFILE.ne.' ') then
				    call create_record(Main,NEW_DATA_LIST,TextArray,TextButton,nWfile)
				!	igraph=0
				    newfile=.true.
				    newfirst=0
				    ifiltype=1
				    !igraph=igraph+1
				    igrorig=igrorig+1
				    igraph=igrorig
				    modplot=igraph
				    do i=1,100
				    iopen(i)=0
				    enddo
				    call gmSetListEntry(combo1_3,GSELECT,gmEntry=1)
			    endif 
	
			endif
			
	case(5003)	!Create New file
			do i=1,njset
				jmiss(njset)=0
			enddo
			
			CALL gmFileBROWSER(nWFILE,nDIR,nFILTsim,gmBrowseType=0)
			IF(nWFILE.ne.' ') then
				call create_record(Main,NEW_DATA_LIST,TextArray,TextButton,nWfile)
				!	igraph=0
				newfile=.true.
				newfirst=0
				ifiltype=1
				!igraph=igraph+1
				igrorig=igrorig+1
				igraph=igrorig
				modplot=igraph
				do i=1,100
				iopen(i)=0
				enddo
				call gmSetListEntry(combo1_3,GSELECT,gmEntry=1)
			endif
		case(5004) ! enter new file with records
			
			ifiltype=1
			
			autplt=.false.
			hdisp=.false.
			n1=1
			ncols=4
			nrows=20
		
			if(hdisp) ncol=1
			ndimc=20
			ndc1=2048
			kmax=20
			ndv1=niobs
			
			kwi=niobs   !dimensions of weight()
			kwj=njset
			i=0
			nplot=0
			textcellW(1:3)='100'
			do while (textcellw(1:3).ne.'  ')
			    i=i+1 
				call gmEnqCellSetting(textarray, 1,i ,R1 ,newrecords(i)%STR(1))
				call gmEnqCellSetting(textarray, 2,i ,R2 ,newrecords(i)%STR(3))
				call gmEnqCellSetting(textarray, 3,i ,R3 ,newrecords(i)%STR(4))
				title1=newrecords(i)%STR(1)
				xtitle=newrecords(i)%STR(3)
				ytitle=newrecords(i)%STR(4)
				call gmEnqCellSetting(textarray, 4,i ,R4,textcellW)
				call chtoint(textcellW(1:11),newrecords(i)%numsets)
				nplot=nplot+1
            enddo
			nplot=nplot-1
			nplotnew=nplot
			if(allocated(xdata)) DEALLOCATE(xdata)
			allocate(xdata(nplot,njset,ncols,niobs))
			do iplot=1,nplot
				numset(iplot)=newrecords(iplot)%numsets
				do jset=1,newrecords(iplot)%numsets
					CALL INTCONV(jset,cset)
					stitle(jset,iplot)='set '//cset
					do i=1,2
						do j=1,niobs
							xdata(iplot,jset,i,j)=0.0
						enddo
					enddo
					do i=3,4
						do j=1,niobs
							xdata(iplot,jset,i,j)=1.0
						enddo
					enddo                                        
				enddo
			enddo
			call gmEraseWidget(New_Data_list)
			do k=1,nplot
			  iplot=nplot-k+1
			  do J=1,newrecords(iplot)%numsets
			    ixd=k+1
				iyd=k+1
				call values_list(Main,iData_list,ixd,iyd,ValArray6,button6,j,iplot,ncols,nrows,xdata,&
				stitle(j,iplot),niobs,njset,nplot,mtitle(j,iplot),static6,numset(iplot),&
				nwfile,newfile,iDataMainPanel)
		 	  enddo
			enddo
		

		  

!=====================================================================
	case(5400)
		do i=1,nset 
		jmiss(i)=0
		icurvd(i)=i
		call gmsetToggleswitch(ittfont_Toggle(40+i),goff)
		enddo
		
	case(5401:5420)
		jmiss(callid-5400)=0
		
		call gmsetToggleswitch(ittfont_Toggle(40),goff)
	case(5020)
		do i=1,7
			text_tog(i)=scara(i)
		enddo
		ntog=7
		itcall=5020
		 call toggle_panel(Main,ITOGGLE,ittoglepanel,ntog,text_tog,intoggle,itcall,valdat,idat,itogbutton)
	case(5021:5027)
		ilog=callid-5021
	
	
	
	case(5011,5012,5013,5014)	! Read record and display
		ipage=callid-5010
		ifontrue=101
		nsetin=nset
		do i=1,nset 
				jmiss(i)=0
				icurvd(i)=i
		enddo
		call ttfont(main,ittfont,5010,idev,ittfont_Toggle)
	case(5010)
		do i=1,6
				istat1=gmenqtoggleswitch(ittfont_Toggle(i))
				if(istat1.eq.gon) ifontref=i
			enddo
			select case(ifontref)
				case(1)
					ifontrue=100
				case(2)
					ifontrue=101
				case(3)
					ifontrue=102
				case(4)
					ifontrue=150
				case(5)
					ifontrue=151
				case(6)
					ifontrue=0
			end select
			do i=1,5
				istat2=gmenqtoggleswitch(ittfont_Toggle(i+10))
				if(istat2.eq.gon) sizefac=3+0.5*i
			enddo
			do i=1,3
				istat2=gmenqtoggleswitch(ittfont_Toggle(i+20))
				if(istat2.eq.gon) widfac=0.25*(i+2)
			enddo
			
			
				
			
			istat2c=gmenqtoggleswitch(ittfont_Toggle(16))
				if(istat2c.eq.gon) then
					show_curve=.true.
				else
					show_curve=.false.
				endif
			istat2=gmenqtoggleswitch(ittfont_Toggle(27))
				if(istat2.eq.gon) then
					idev=3
					idevp=3
				endif
				
			!endif
			ilog0=ilog
			do i=1,5
				istat2=gmenqtoggleswitch(ittfont_Toggle(i+30))
				if(istat2.eq.gon) ilog=i-1
			enddo
			istat2=gmenqtoggleswitch(ittfont_Toggle(36))
				if(istat2.eq.gon) ilog=ilog0
			goto 11
			istat4=gmenqtoggleswitch(ittfont_Toggle(40))
			if(istat4.eq.goff) then
				do i=1,nset
					jmiss(i)=1
				enddo
				do i=1,nset
				istat3=gmenqtoggleswitch(ittfont_Toggle(i+40))
				if(istat3.eq.gon) jmiss(i)=0
				enddo
				do i=1,nset
					jmiss0(i)=jmiss(i)
				enddo
			else
					do i=1,nset
					jmiss0(i)=0
					enddo
			endif
11 continue
			istat2=gmenqtoggleswitch(ittfont_Toggle(36))
				if(istat2.eq.gon) ilog=ilog0
			call gmremovewindow(ittfont)
	    callid=ipage+5010
		
		if(callid.eq.5020) callid=5011
			idev=0
			if(callid.eq.5013) then
				callid=5011
				plot=.true.
			
			else if(callid.eq.5014) then
				callid=5012
				plot=.true.
				else
				plot=.false.
			endif
		
			if(ifiltype.gt.0.and.ifiltype.lt.4.and.fopen_11) then
			
			if(.not.saveandplot) then
					ixp0=-1
					iyp0=-1
					call gmEnqListStatus(List7_1,nentry,nselect,nfirst)
			endif
				
			if(nentry.gt.0) then
				 parval=' '
			  ncomp=0
			  do ith=1,100
			!	theta(ith)=-10000
			!	if(ith.le.20)	jmiss(ith)=0
			  enddo

				!new
				if(plot) then
					CALL GUIPRT(0,ISTATus)
					IF(ISTATus.NE.0)THEN
						
						idevp=6
						imessy=gmDisplayMessageBox('','Black & White ?',Gquestion,gyesno)
						if(imessy.eq.gyesbutton) idevp=4
						
					else
						plot=.false.
							CALL GUIPRT(1,ISTATus)
							imessy=gmDisplayMessageBox('','Printer not available',Gexclamation,gok)
			
					endif
				endif
				ipos=0
				do i=1,nentry
				call gSetCharFont(ifont0)
					if(.not.saveandplot) then
							istat(i)=gmEnqListEntry(List7_1,i,ftitle1(i))
					endif
					if(istat(i).eq.2) then
					ishp=0
				        ntext=0
						iopen(i)=2
						zoom=.false.
						izoom=0
						readrec=.true.
						iplot=i
						mplot=mplot+1
						nplot_on(mplot)=iplot
						!modplot=iplot
						if(callid.eq.5012) then
								ipos=ipos+1
								if(ipos.eq.5) then
									ipos=1

								endif
								iprintq=4
                        else
								iprintq=0
                        endif
						if(ipos.le.1) then
								ijplot=ijplot+24
								ixp=ixp0+ijplot
								iyp=iyp0+ijplot
								ixd=ixd+1
								iyd=iyd+1
							
								!new			
								if(plot) istatp=gmPrinterControl(gnewpage)
						endif
						if(.not.plot) then
						do k=1,ngraph
							if(oldrecords(k)%ipos.le.1) then
							call gmEnqWidgetInfo(Graph1_1(k),widget)
							ivisual=widget%visual
							if(ivisual.eq.0) then
								jopen(k)=-1
							!	igraph=k
							igrorig=k
								goto 333
							endif
							endif
						enddo
						endif
					!	if(.not.plot)igraph=igraph+1
						
					if(.not.plot) igrorig=igrorig+1
						if(.not.plot.and.igraph.gt.10) then
							imessy=gmDisplayMessageBox('','Maximum 10 plots on screen',Gexclamation,gok)
							goto 1
						endif
							

333						continue
						igraph=igrorig
						ngraph=igraph
						if(.not.plot) then
							jopen(igraph)=1
						
							lframe=graphics1_1(igraph)
							oldrecords(igraph)%ipos=ipos
					
						endif
							modplot=igraph
							if(allocated(xval)) DEALLOCATE(Xval,Yval)
							if(allocated(xcal)) DEALLOCATE(Xcal,Ycal)
							if(allocated(icurvw)) DEALLOCATE(icurvw)
							if(allocated(theta)) DEALLOCATE(theta)
							if(allocated(thetgues)) DEALLOCATE(thetgues)
							if(allocated(ndat)) DEALLOCATE(ndat)
							if(allocated(isym)) DEALLOCATE(isym)
							if(allocated(ijoin)) DEALLOCATE(ijoin)
							if(allocated(symsiz)) DEALLOCATE(symsiz)
							if(allocated(ncal)) DEALLOCATE(ncal)
							if(allocated(iline)) DEALLOCATE(iline)
								if(allocated(w)) DEALLOCATE(w)
							if(allocated(icurvd)) DEALLOCATE(icurvd)
							if(allocated(icurvc)) DEALLOCATE(icurvc)
							hdisp=.false.
							autplt=.false.
							
							if(ifiltype.eq.1) allocate(w(niobs,njset))
						
							call read_record(ISTREC,Main,iData_list,iplot,ifiltype,&
							iplotype,njset,nset,nsfit,&
							xnum,nrows,niobs,njobs,stitle,hdisp,xobs,yobs,w,nj,&
							titlex,titley,ilabel,title1,iptype,ndv1,ndimd,&
							ndc1,ndimc,kwi,kwj,kmax,ncurvd,ncurvc,dfile,nval1,&
							useconsam,filtered,istr1,colseq,nhline,adcfil,&
							itit,ndev,ib,istart,iend,srate,calfac,ioff,calfac2,&
							cdatew,adctime,adcf1,isdfst,ndisp1,nrec1,jdat,base,&
							ntrans,y0,t0,nfilter,dtf,tif1,tif2,dt,stepamp,filt,&
							ndelt,ffilt,fcz,fczoom,njump,idest,krn,titled,setx)
							
						
							if(ifiltype.eq.1) then  !cvdat
								iplotype=-1
								n1=1
				if(plot) idev=idevp
									if(.not.plot) oldrecords(modplot)%iplotype=-1
								gfile=dfile
									kwi=niobs
									kwj=njobs
								ndimc=20
								ndc1=501
								kmax=100
								ndimd=nset
								ndv1=niobs
								ndimd=njset !!!!   
								kwi=niobs
								kwj=njset      
								ALLOCATE(Xval(ndv1,ndimd),Yval(ndv1,ndimd),&
								Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
							
								ALLOCATE(icurvw(njset))
								if(kmax.lt.1) kmax=1		!must allocate theta whether used or not
								ALLOCATE(theta(kmax))
								ALLOCATE(thetgues(100,nset))
								ALLOCATE(ndat(njset),isym(njset),ijoin(njset),symsiz(njset))
								ALLOCATE(ncal(ndimc),iline(ndimc))
								ALLOCATE(icurvd(njset),icurvc(ndimc))
								 
								ncurvd=0
								numset(iplot)=nset
								do j=1,ndimc
									ncal(J)=0
									iline(j)=0
									icurvc(j)=0
								enddo
								do j=1,ndimd
								ndat(j)=0
								
								enddo
								!do j=1,njset
								  do j=1,nset  
								
	   								ndat(j)=nj(j)
									ndatset(iplot,j)=ndat(j)
	   								icurvd(j)=j		!display all sets
									
	   								icurvw(j)=isdev		!new for VPLOT5!
	   								isign=(-1)**j
	   								isym(j)=isign*(j+2)	!alternate filled/hollow symbols
	   								if(ndat(j).le.10) then
	      								symsiz(j)=3.0
	   								else if(ndat(j).gt.10.and.ndat(j).le.30) then
	      								symsiz(j)=2.0
	   								else
	      								symsiz(j)=1.5
	   								endif
	   								ijoin(j)=-1		!points not joined
	   								do k=1,nj(j)
										xval(k,j)=xobs(k,j)
										yval(k,j)=yobs(k,j)
	   								enddo
									if(jmiss(j).ne.1) then	!set omitted
										ncurvd=ncurvd+1
										icurvd(ncurvd)=j
									endif
								enddo
								iparfirst=-100
								n1=1
								autplot=.false.
								ncurvc=0
								landscape=.true.
								mono=.false.
								call calc_default(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,symsiz,&
								ndimd,XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,&
								XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,&
								XLO1,XHI1,YLO1,YHI1,y0,yinf,inumx,inumy,&
								titlex,titley,ilabel,doframe,autplt,itit,title1,&
								ISHP,ifont,landscap,ndv1,ndc1,w,kwi,kwj,icurvw,kmax,iver,&
								redrawn,plot,redo,pbmp,pwmf,npint,hdisp,n1,tlenx,tleny,nx1,ny1,izoom,&
								xtitle,ytitle,rescale,dxs,dys,ifitype,parval,theta,ncomp,isval,sval,&
								ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil)
								n1=1
								ncol=4
								do j=31,31+numby
									ijus(j)=1
								!	rx(j)=rx(j)-2.5*dxs
								!	ry(j)=ry(j)+dys
							    enddo
								if(hdisp) ncol=1
								
								ndimd=njset
								do ns=1,nset
								!	jmiss(ns)=0
								enddo
								nsfit=0
								if(nset.eq.1) jset=1
								do j=1,nset
									if(jmiss(j).eq.0) then
										nsfit=nsfit+1
										juse(nsfit)=j
									endif
								enddo
								if(nset.eq.1) then
									jset=1
									nsfit=1
									juse(1)=1
								endif
								do J=1,nset
									
									nd1=nj(j)+20	!extra rows in case lines added in inwindv
	    
									NROWS=NJ(J)
									NCOLS=NCOL
									if(.not.plot) then
		 							call format_list(ifiltype,ixd,iyd,xobs,yobs,w,nj(j),xnum,niobs,&
									njset,Main,iData_list,j,iplot,nplot,xdata,ncols,nrows,&
									stitle(j,iplot),hdisp,icallasc,n1,button6,&
									ValArray6,nset,gfile,newfile,iDataMainPanel)
									endif
								enddo
								isens=1
								if(.not.plot.and.igraph.le.20) then
								n1=1
									xmin10=xmin1
								ymin10=ymin1
								xmax10=xmax1
								ymax10=ymax1
								call store_record(iplotype,igraph,iplot,ixp,iyp,ipos,xobs,yobs,w,&
								nj,niobs,njset,nplot,nset,&
								juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
								wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
								ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
								xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,oldrecords,&
								dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,parval,ifitype)
								endif
								if(ifontrue.ne.0) then
								do ib=1,100
									oldrecords(modplot)%attributes%IFNT(ib)=ifontrue
									ifnt(ib)=ifontrue
								enddo
								oldrecords(modplot)%attributes%IFNT(4)=15
									ifnt(4)=15
								endif
			
		
								if(sizefac.le.5) then
								do ib=1,5
								oldrecords(modplot)%attributes%SIZETEXT(ib)=sizefac
								SIZETEXT(ib)=sizefac
								enddo
								do ib=6,100
									oldrecords(modplot)%attributes%SIZETEXT(ib)=sizefac-0.5
									SIZETEXT(ib)=sizefac
								enddo
								oldrecords(modplot)%attributes%SIZETEXT(2)=sizefac-1
								SIZETEXT(2)=sizefac-1
								endif
							else if (ifiltype.eq.2) then ! plotq
								
								if(import)	then 
									gfile=ifile
								else
									gfile=dfile
								endif
							
								if(iplotype.eq.1) then
									if(.not.plot) oldrecords(modplot)%iplotype=1
									n1=1
								else if(iplotype.eq.2) then
									
									if(.not.plot) oldrecords(modplot)%iplotype=2
									n1=0
									kwi=1	!no SD for single channels
									kwj=1
								else if(iplotype.eq.3) then
									n1=1
									kwi=1	!no SD for single channels
									kwj=1
									kmax=1
									if (iptype.eq.33) then
									if(.not.plot) oldrecords(modplot)%iplotype=33
										ncurvc=0
										ncurvd=1
										ndv1=nval1
										ndimd=1
										ndc1=1
										ndimc=1
										itx=1
										ity=1
									else
										if(.not.plot) oldrecords(modplot)%iplotype=3
										
										ndv1=jdat
										ndimd=1
										!ndc1=20480
										ndc1=2048
										ndimc=1	
									endif
								endif
								
								
								if(iplotype.eq.3.or.iplotype.eq.1) then
								ALLOCATE(Xval(1:ndv1,ndimd),Yval(1:ndv1,ndimd))
								
								else if(iplotype.eq.2) then
								ALLOCATE(Xval(n1:ndv1,ndimd),Yval(n1:ndv1,ndimd))
								endif
								if(iplotype.le.3) then
								ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
								ALLOCATE(icurvw(ndimd))
									allocate(w(kwi,kwj))
								if(kmax.lt.1) kmax=1		!must allocate theta whether used or not
								ALLOCATE(theta(kmax))
								ALLOCATE(ndat(ndimd),isym(ndimd),ijoin(ndimd),symsiz(ndimd))
								ALLOCATE(ncal(ndimc),iline(ndimc))
								ALLOCATE(icurvd(ndimd),icurvc(ndimc))

								do j=1,ndimd
									ndat(j)=0
									isym(j)=0
									symsiz(j)=0.0
									ijoin(j)=0
									icurvw(j)=0
									if(Iplotype.eq.3) icurvw(j)=-1
									do k=1,ndv1
									xval(k,j)=0.0
									yval(k,j)=0.0
									enddo
								enddo
								do j=1,ndimc
									ncal(j)=0
									iline(j)=0
									icurvc(j)=0
									do k=1,ndc1
									xcal(k,j)=0.0
									ycal(k,j)=0.0
									enddo
								enddo
								endif		
								if(iplotype.eq.1) then	!vplot
					  			
								call Read_plotq(istrec,xval,yval,xcal,ycal,ndimd,ndimc,ncurvd,&
								ndat,isym,ijoin,ncurvc,ncal,iline,symsiz,xmin,xmax,ymin,ymax,&
								xcross,ycross,xtic,ytic,ntx,nty,itx,ity,xlo,xhi,ylo,yhi,&
								itit,title1,ilog,iscal,doframe,titlex,titley,ilabel,&
								inumx,inumy,sval,theta,ifitype,ncomp,isdev,w,y0,yinf,iptype,& 
								ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,y1v,y2v,y1c,y2c,ivplot,&
								interp,screen,colplotter,itrace,ntrace,ytsep,ndv1,ndc1,&
								kwi,kwj,icurvw,kmax,iver,jmiss)
								nset=ncurvd
								autplt=.true.
								ncol=4
								if(hdisp) ncol=1
								numset(iplot)=ndimd
								do j=1,ndimd
									icurvd(j)=j
									NROWS=Ndat(J)
									nj(j)=ndat(j)
									ndatset(iplot,j)=ndat(j)
									NCOLS=4
								
								enddo
								do j=1,ncurvc
									icurvc(j)=j
								enddo
								

								else if(iplotype.eq.2) then	!vhist
								call read_histq(istrec,xval,yval,xcal,ycal,ndimd,ndimc,ncurvd,&
								ndat,ijoin,ncurvc,ncal,iline,xmin,xmax,ymin,ymax,&
								xcross,ycross,xtic,ytic,ntx,nty,itx,ity,xlo,xhi,ylo,yhi,&
								itit,title1,ilog,iscal,doframe,titlex,titley,ilabel,&
								inumx,inumy,theta,ifitype,ncomp,sval,isval,xwbase,lt2,&
								screen,colplotter,iptype,ndv1,ndc1,kmax,iver,idev,dxs,dys)
								hdisp=.true.
								autplt=.true.
								nset=ncurvd
								n1=0
								ncol=4
								if(hdisp) ncol=1

								numset(iplot)=ndimd
								
								do j=1,ndimd
									icurvd(j)=j
									NROWS=Ndat(J)
									ndatset(iplot,j)=ndat(j)
									nj(j)=ndat(j)
									NCOLS=1
								enddo
								do j=1,ncurvc
									icurvc(j)=j
								enddo
								
								else if(iplotype.eq.3) then ! single channel
								if(iptype.eq.33) then
									icurvd(1)=1
									icurvc(1)=1
									ndat(1)=nval1
									if(allocated(xval3)) deallocate(xval3,yval3)
									ALLOCATE(Xval3(nval1),Yval3(nval1))
									read(11,rec=istr1) (XVAL3(k),k=1,nval1),&
									(YVAL3(k),k=1,nval1),(yhline(k),k=1,nhline),&
									(xhlb(k),k=1,nhline),(xhle(k),k=1,nhline)
									do k=1,nhline
										ilhtype(k)=ltype
									enddo
									do in=1,nval1
										xval(in,1)=xval3(in)	!start at xval1(1)
										yval(in,1)=yval3(in)
									enddo
								else
									if(idest.ne.99) then
									call READSCQ1(iptype,jdat,inp,krn,nrec1,dx,srate,&
									xmin,xmax,useconsam,adcfil,ioff,istart,calfac,calfac2,ndisp1,&
									ncurvd,ncurvc,icurvd,icurvc,xval,yval,xcal,ycal,ndat,isym,&
									ncal,ijoin,symsiz,&
									ycalc,y0,ntrans,DT,ke,filt,nfilter,stepamp,dtf,tif1,tif2,t0,&
									ntx,nty,itx,ity,titlex,titley,ilabel,&
									ndv1,ndimd,ndc1,ndimc)
									nset=ncurvd
									xlo=-1
									endif
									   
								endif !(if(iplotype))
								if(iplotype.le.3) then
								interp=.false.
								ilog=0
								ncjump=0
								nvjump=0
								ivplot=.false.
								cbig=2.5
								ifont=2
								landscap=.true.
								fitted=.false.
								draft=.false.
								doframe=.false.
								itit=1
								ifitype=0
								isdev=-1
								isetcol=1	!new for VPLOT3
								do in=1,100
									icol(in)=-1		!so default colours used
								enddo
								icol(1)=9	!blue for data set #1
								icol(11)=12	!red for calc curve #1
								icol(25)=0	!black for plot title
								icol(72)=2	!green for baseline (first horizontal line)
								icol(73)=2	!dashed green for first marker
								icol(74)=1	!dashed blue for 2nd marker
								icol(75)=5	!dashed purple for 3rd marker
								icol(76)=6	!dashed brown for 4th marker
								icol(77)=4	!dashed red for 5th marker
								icol(78)=4	!dashed yellow for 6th marker
								autplt=.false.
								ntx=-1000	!cal bars
								nty=-1000	!cal bars
								inumx=-1	!see notes in LAXES2
								inumy=-1	!see notes in LAXES2
								iscal=2		!use only xmin, xmax -others internal
								do ila=1,100		!no positions defined from queue
									angle(ila)=0
									idraw(ila)=-2		!until defined
									rx(ila)=0.0		!NB world coord may be neg so cant tell if defined yet
								enddo	
								endif
								endif
								autplt=.true.
								if(iplotype.eq.3) autplt=.false.
							
								if(idest.ne.99.and.iplotype.le.3) then
									iparfirst=-100
								call calc_default(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,symsiz,&
								ndimd,XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,&
								XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,&
								XLO1,XHI1,YLO1,YHI1,y0,yinf,inumx,inumy,&
								titlex,titley,ilabel,doframe,autplt,itit,title1,&
								ISHP,ifont,landscap,ndv1,ndc1,w,kwi,kwj,icurvw,kmax,iver,&
								redrawn,plot,redo,pcgm,pwmf,npint,hdisp,n1,tlenx,tleny,nx1,ny1,izoom,&
								xtitle,ytitle,rescale,dxs,dys,ifitype,parval,theta,ncomp,isval,sval,&
								ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil)
								isens=1
								do j=31,31+numby
									ijus(j)=1
								!	rx(j)=rx(j)-2.5*dxs
								!	ry(j)=ry(j)+dys
								enddo
								if(ncomp.eq.0) idraw(2)=-2
								isens=1
									if(ifontrue.ne.0) then
								do ib=1,100
								!	oldrecords(modplot)%attributes%IFNT(ib)=ifontrue
									ifnt(ib)=ifontrue
								enddo
								ifnt(4)=15
								endif
			
		
								if(sizefac.le.5) then
								do ib=1,5
							!	oldrecords(modplot)%attributes%SIZETEXT(ib)=sizefac
								SIZETEXT(ib)=sizefac
								enddo
								do ib=6,100
								!	oldrecords(modplot)%attributes%SIZETEXT(ib)=sizefac-0.5
									SIZETEXT(ib)=sizefac
								enddo
							!	oldrecords(modplot)%attributes%SIZETEXT(2)=sizefac-1
								SIZETEXT(2)=sizefac-1
								endif
								if(.not.plot.and.igraph.le.20) then
								!if(iplotype.ne.3) ndimc=20
								if(ndv1.gt.niobs) niobs=ndv1
								do j=1,nset
									nj(j)=ndat(j)
								enddo
								n1=1
								if(hdisp) n1=0
								xmin10=xmin1
								ymin10=ymin1
								xmax10=xmax1
								ymax10=ymax1
							
								if(ifontrue.ne.0) then
								do ib=1,100
									oldrecords(modplot)%attributes%IFNT(ib)=ifontrue
									ifnt(ib)=ifontrue
								enddo
									oldrecords(modplot)%attributes%IFNT(4)=15
									ifnt(4)=15
							
								endif
			
		
								if(sizefac.le.5) then
								do ib=1,5
								oldrecords(modplot)%attributes%SIZETEXT(ib)=sizefac
								SIZETEXT(ib)=sizefac
								enddo
								do ib=6,100
									oldrecords(modplot)%attributes%SIZETEXT(ib)=sizefac-0.5
									SIZETEXT(ib)=sizefac
								enddo
								oldrecords(modplot)%attributes%SIZETEXT(2)=sizefac-1
								SIZETEXT(2)=sizefac-1
								endif
								call store_record(iplotype,igraph,iplot,&
								ixp,iyp,ipos,xval,yval,w,&
								nj,niobs,njset,nplot,nset,&
								juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
								wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
								ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
								xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,oldrecords,&
								dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,&
								parval,ifitype)
								endif !not plot
							
								endif !idest
								
							
							else if (ifiltype.eq.3) then ! ascii file		
	  						endif ! end ifiltype
							
							if(idest.ne.99.and.iplotype.le.3) then
							if(plot) idev=idevp
							call graph1(igraph,iplot,main,ixp,iyp,graph1_1,GraphMainPanel1_1,graphics1_1,&
								wxmin,wxmax,wymin,wymax,ipos,gfile,plot,ixposv,iyposv)
							
							call draw_axis(xmin,xmax,ymin,ymax,ymin1,ymax1,&
							    xtic,ytic,nx1,ny1,xcross,ycross,itx,ity,tlenx,tleny,&
								logx,logy,sqrty,doframe,icol,ntx,nty,idev,thick,itype,&
            calbarx,calbary,xbeg4,ybeg4,xend4,yend4,xbeg5,ybeg5,xend5,yend5)
                            do ik=1,100
                                idraw(ik)=1
                            enddo
							call draw_text(parval,xtitle,ytitle,ztitle,title1,cnumx,cnumy,&
								cnumz,cexpx,cexpy,cexpz,newtext,numbx,numby,numbz,ntext,inumx,&
								inumy,intx,angle,sizetext,ijus,thick,rx,ry,idraw,&
								icol,ifnt,str,dxs,dys)
							!	kwi=niobs
							!	kwj=njset
							if(iplotype.eq.1) then
								text_tog(1)=' plot'
							else if(iplotype.eq.2) then
							text_tog(1)=' histogram'
							else
							text_tog(1)=' single channel'
							endif
							if(discprt) write(7,1111) iplot,iplotype,title1,text_tog(1)(1:20)
1111						format('- plot:',i4,'- plot type:',i2,' - title:',a60,'(',a20,')'/)
							text_tog(1)='   '
							if(ipage.eq.1) then 
								ypf=1.1*ry(1)
							else
								ypf=1.4*ry(1)
								
							endif
							ymin20=ymin
							ymax20=ymax
							if(sqrty) ymin20=sqrt(ymin)
							if(sqrty) ymax20=sqrt(ymax)
							xt=0.5*(xmin+xmax)
							yt=ymax20+0.15*(ymax20-ymin20)!new
							nl=nblank1(gfile)
							call intconv(iplot,cnum0)
								icp=0
							if(idev.ge.3) icp=48
								theg=2.5
							if(ipos.gt.0) theg=3.
							if(plot) call write_string(gfile(1:nl)//'-plot:'//cnum0,xmax,yt,0.,0,101,theg,icp,dxs,dys)
						
							if(hdisp) then
								call draw_hist(xval,yval,icurvd,ncurvd,ndelt,logity,logx,&
									logy,sqrty,y0,yinf,x0,ilog,xmin,xmax,ymin,ymax,idev,ndat,ijoin,&
									icol,thick,ndv1,ndimd,xwbase,lt2)
								
                            else
								call draw_data(xval,yval,icurvd,ncurvd,ndelt,logity,logx,logy,sqrty,&
									y0,yinf,x0,ilog,idev,ndat,ijoin,icol,isym,&
									symsiz,thick,barleng,interp,npint,Xint,Yint,nint,yp1,ypn,&
									Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw,&
									ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil)
							endif
							
							call draw_curves(xcal,ycal,icurvc,ncurvc,logity,logx,&
								logy,sqrty,y0,yinf,x0,ilog,idev,&
								wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
								xmin,xmax,ymin,ymax,ncal,&
								iline,icol,thick,ndc1,ndimc,jfirst,dxs,dys)
						if(idev.eq.3) show_curve=.false.
							if(show_curve) then
								ytc=0.07*(ymax20-ymin20)
								if(ncurvd.gt.0) call write_string('Data:',xmax+0.11*(xmax-xmin),ymax20+0.01*(ymax20-ymin20),0.,0,101,&
								3.,icol(151),dxs,dys)
								do j1=1,ncurvd
								j=icurvd(j1)
								
								sx0=symsiz(j)*0.7*dxs
								sy0=symsiz(j)*0.7*dys
							    call intconv(j,cnum0)
								call write_string(cnum0,xmax+0.05*(xmax-xmin),ymax20-ytc,0.,0,101,&
								3.,icol(j+100),dxs,dys)
							
								call jSYMBOL(xmax+0.1*(xmax-xmin),ymax20-ytc,isym(j),sx0,sy0,icol(j+100),idev)   
								ytc=ytc+0.07*(ymax20-ymin20)
								
							
								enddo
							endif
							call draw_lines(IDRAW,ICOL,THICK,C_THICK,NARROW,NLINE,NHLINE,NVLINE, &
							XBEG,YBEG,XEND,YEND,y0,yinf,x0,dxs,dys)
							xmin0=xmin1
							xmax0=xmax1
							ymin0=ymin1
							ymax0=ymax1
							idest=0
					
						else if(ifiltype.eq.2.and.iplotype.eq.4) then
						
						do i3=1,100
							i3col(i3)=48           !so default colours used
						enddo
						if(iptype.eq.40) then
								read(11,rec=istrec)iptype,title3,nx,ny,ndx,ndy,numx,numy,&
								xtitle3,ytitle3,ztitle3,idrawt,idrawx,idrawy,idrawz,&
								xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,&
								ifnt3,ifnl,alfa,beta,gama,delta,&
								ijust,ijustx,ijusty,ijustz,&
								xmin3,xmax3,ymin3,ymax3,zmin3,zmax3,xtic3,ytic3,ztic3,&
							ratio,rat,radius,theta3,phi,ifram,numaxi,nwidth,npow,nplace,&
							nxstep,nystep,ncolfu,ncolfd,ncolgu,ncolgd,kcol,&
							icol25,icol23,icol71,istyle,isup,&
							fill,inter,axese,fillbad,cross,ioffs
							if(kcol.eq.4) then
								posneg=.true.
								kcol=2
							else if(kcol.eq.1) then  ! contour
								posneg=.false.
								ncont=14
								kcol=-ncont
							else if(kcol.eq.2) then  ! one
								kcol=1
								posneg=.false.
							else if(kcol.eq.3) then  ! two
								kcol=2
								posneg=.false.
							endif
							i3col(1)=1          !contour 1
							i3col(2)=9          !cont 2
							i3col(3)=3          !cont 3
							i3col(4)=11   !cont 4
							i3col(5)=2          !cont 5
							i3col(6)=10   !cont 6
							i3col(7)=8          !cont 7
							i3col(8)=5          !cont 8
							i3col(9)=13   !cont 9
							i3col(10)=6         !cont 10
							i3col(11)=4         !cont 11
							i3col(12)=12   !cont 12
							i3col(13)=14   !cont 13
							i3col(14)=15   !cont 14
							i3col(15)=7         !cont 15
							i3col(21)=11     !axes
							i3col(22)=0   !bad region
							i3col(33)=ncolfu
							i3col(34)=ncolfd
							i3col(31)=ncolgu
							i3col(32)=ncolgd
							i3col(25)=icol25
							i3col(23)=icol23
							i3col(71)=icol71
							i3col(22)=0
							i3col(35)=12        !positive
							i3col(36)=9 !negative
						else if(iptype.eq.41) then
							read(11,rec=istrec)iptype,title3,nx,ny,ndx,ndy,numx,numy,&
							xtitle3,ytitle3,ztitle3,idrawt,idrawx,idrawy,idrawz,&
							xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,&
							ifnt3,ifnl,alfa,beta,gama,delta,&
							ijust,ijustx,ijusty,ijustz,&
							xmin3,xmax3,ymin3,ymax3,zmin3,zmax3,xtic3,ytic3,ztic3,&
							ratio,rat,radius,theta3,phi,ifram,numaxi,nwidth,npow,nplace,&
							nxstep,nystep,ncolfu,ncolfd,ncolgu,ncolgd,ncolmp,ncolmn,kcol,&
							icol25,icol23,icol71,istyle,isup,&
          fill,inter,axese,fillbad,cross,ioffs,icbad
             if(kcol.eq.4) then
                  posneg=.true.
                  kcol=2
             else if(kcol.eq.1) then  ! contour
                  ncont=14
                  kcol=-ncont
                  posneg=.false.
             else if(kcol.eq.2) then  ! one
                  kcol=1
                  posneg=.false.
             else if(kcol.eq.3) then  ! two
                  kcol=2
                  posneg=.false.
             endif
                i3col(1)=1          !contour 1
                i3col(2)=9          !cont 2
                i3col(3)=3          !cont 3
                i3col(4)=11   !cont 4
                i3col(5)=2          !cont 5
                i3col(6)=10   !cont 6
                i3col(7)=8          !cont 7
                i3col(8)=5          !cont 8
                i3col(9)=13   !cont 9
                i3col(10)=6         !cont 10
                i3col(11)=4         !cont 11
                i3col(12)=12   !cont 12
                i3col(13)=14   !cont 13
                i3col(14)=15   !cont 14
                i3col(15)=7         !cont 15
                i3col(21)=11     !axes
                i3col(22)=0   !bad region
             i3col(33)=ncolfu
             i3col(34)=ncolfd
             i3col(31)=ncolgu
             i3col(32)=ncolgd
             i3col(25)=icol25
             i3col(23)=icol23
             i3col(71)=icol71
             i3col(22)=icbad
             i3col(35)=ncolmp    !positive
							i3col(36)=ncolmn    !negative
						else if(iptype.eq.42) then
							read(11,rec=istrec)iptype,title3,nx,ny,ndx,ndy,numx,numy,&
							xtitle3,ytitle3,ztitle3,idrawt,idrawx,idrawy,idrawz,&
							xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,&
							ifnt3,ifnl,alfa,beta,gama,delta,&
							ijust,ijustx,ijusty,ijustz,&
							xmin3,xmax3,ymin3,ymax3,zmin3,zmax3,xtic3,ytic3,ztic3,&
							ratio,rat,radius,theta3,phi,ifram,numaxi,nwidth,npow,nplace,&
							nxstep,nystep,istyle,isup,kcol,&
							fill,inter,axese,fillbad,cross,posneg,ioffs
						endif
						if(allocated(xval3)) DEALLOCATE(Xval3)
						if(allocated(yval3)) DEALLOCATE(Yval3)
						if(allocated(z3)) DEALLOCATE(z3)
						if(allocated(bad)) DEALLOCATE(bad)
						allocate(xval3(ndx),yval3(ndy),z3(ndx,ndy),bad(ndx,ndy))
						istr1=istrec+ioffs
						indk=ndx*ndy
						allocate(zeta1(indk+1),zeta2(indk+1))
						if(iptype.eq.42) then
							read(11,rec=istr1) (i3col(k),k=1,100),(XVAL3(k),k=1,ndx),&
							(YVAL3(k),k=1,ndy),(zeta1(k),k=1,indk),(zeta2(k),k=1,indk)
						else
							read(11,rec=istr1) (XVAL3(k),k=1,ndx),&
							(YVAL3(k),k=1,ndy),(zeta1(k),k=1,indk),(zeta2(k),k=1,indk)
						endif
						k=1
						do i3=1,ndx
							do j=1,ndy
								z3(i3,j)=zeta1(k)
								bad(i3,j)=zeta2(k)
								k=k+1
							enddo
						enddo
						DEALLOCATE(zeta1,zeta2)
      
							
						isetcol=2
					    autplt=.true.
						mono=.false.
						do m=1,100
							if(i3col(m).eq.0) i3col(m)=48
					    enddo
						call gauto3d(igraph,xval3,yval3,z3,bad,nx,ny,ndx,ndy,&
     						xtitle3,ytitle3,ztitle3,title3,idrawt,idrawx,idrawy,idrawz,&
							quarter,idev,plot,iplot,kcol,posneg,isetcol,i3col,&
							wxmin,wxmax,wymin,wymax,vxlop,vxhip,vylop,vyhip,&
							xlop,xhip,ylop,yhip,main,ix,iy,graph1_1,&
							GraphMainPanel1_1,graphics1_1,ipos,gfile,ixposv,iyposv&
							oldrecords,nplot,ifont,ifnl,alfa,beta,gama,delta,&
                            ijust,ijusx,ijusy,ijusz,xst,yst,xspx,yspx,xspy,yspy,xspz,&
                            yspz,numx,numy,xmin,xmax,ymin,ymax,zmin,zmax,xtic,ytic,ztic,&
                            ratio,rat,radius,thetag,phi,ifram,numaxi,nwidth,npow,nplace,&
                            nxstep,nystep,istyle,isup,fill,inter,axis,fillbad,autplt,cross)
						endif !end iplotype=4
					endif ! end of istat(i)=2	
			enddo
			!new
			if(plot) then
						plot=.false.
						CALL GUIPRT(1,ISTATus)
			endif	
		endif
				
		endif


case(5028:5039)
	ifontrue=callid-4930
    if(ifontrue.eq.98) ifontrue=150
	if(ifontrue.eq.99) ifontrue=151
case(5041:5047)
	isizetrue=2*((callid-5040)-1)+8	
	
case(5050:5052)			
	ithicktrue=callid-5050
	do i=101,250
	thick(i)=ithicktrue*thick(i)
	enddo
case(5100)
if(readrec) then
		if(modplot.gt.0.and.lframe.eq.graphics1_1(modplot)) then
		callid=5100+modplot
		goto 2
	else
			imessy=gmDisplayMessageBox('','Select a graph',Gexclamation,gok)
			
	endif
	else
		imessy=gmDisplayMessageBox('','No record on screen/Not available for this record',Gexclamation,gok)
			
	endif
case (5101:5125) ! plotq
		modplot=callid-5100
		igraph=modplot
		if(readrec.and.modplot.gt.0) then
555			continue		
				CALL gmFileBROWSER(eFILE,eDIR,eFILT,gmBrowseType=1)
				IF(eFILE.ne.' '.and.iplotype.le.4) then
				nef=nblank1(efile)
				iegal=1
				do i=1,nef-4
					if(efile(i:i).ne.dfile(i:i)) iegal=0
				enddo
				if(iegal.eq.1) then
					if(fopen_11) then
						close(unit=11)
						fopen_11=.false.
						CALL gmEnqWidgetInfo(records,Widget)
						ivisual=widget%visual
						if(ivisual.eq.1) call gmremovewindow(records)
					endif
				endif
				isens=0
					iverq=1100
INQUIRE(file=efile,exist=present)
	
				if(.not.present) then
	   
					nplot=0
					iverq=1200	!set here in case reset by read from old format queus
					do 482 i=1,200
					jstrec2(i)=0
482					lstrec2(i)=0
					OPEN(unit=11,file=efile,status='UNKNOWN',access='DIRECT',&
					form='BINARY',RECL=1)
					write(11,rec=1) nplot,jstrec2,lstrec2,iverq
					CLOSE(UNIT=11)
				else
					OPEN(unit=11,file=efile,status='UNKNOWN',access='DIRECT',form='BINARY',RECL=1)
					READ(11,REC=1,IOSTAT=I_VAR) NPLOT,JSTREC2,LSTREC2,IVERq          
					IF(I_VAR.NE.0) THEN
						CLOSE(UNIT=11)
						Icall=gmDisplayMessageBox('Stop ','Not a proper data file',GEXCLAMATION,GOK)
						goto 555
					else
						if(iverq.ne.1100.and.iverq.ne.1001.and.iverq.ne.1200) then
						Icall=gmDisplayMessageBox('Stop ','Old version data file',	GEXCLAMATION,GOK)
						CLOSE(UNIT=11)
						goto 555
						endif
					endif
				endif
			else
					goto 1
			endif
			iqwin = gmCreateComplexDialogueBox(Main, 15, 5, 12, 6, GALL, 'Plot queue', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
	
			iqPanel=gmCreatePanel(iqwin, 0, 0, 12,6, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmtitle=' ',gmExpand=GOff, gmType=GNOBOUNDARY, &
            	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
	    
			itextq = gmCreateTextEntry(iqPanel, 1,1, 7, 1,'File:'//efile, 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
			iqplot=nplot+1
			if(iqplot.eq.0) iqplot=1
			call intconv(iqplot,cnum5)
			itextq = gmCreateTextEntry(iqPanel, 1,2, 11, 1,'Last queue number='//cnum5, 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
			
			itextq = gmCreateTextEntry(iqPanel, 1,3, 5, 1,'Queue number=', 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
				
            ivalq=gmCreateValueEntry(iqpanel, 7, 3, 4, 1,iqplot , 4, 0, gedit,&
				gmVpos=GTOP)
            
			ivb=gmCreatePushButton(iqpanel,1,0, 5, 1,'Cancel',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=5128)
			
			ivb=gmCreatePushButton(iqpanel,6,0, 5, 1,'Queue',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=5127)
			
			call gmdrawwindow(iqwin)
			else
				imessy=gmDisplayMessageBox('','No record on screen/Not available for this record',Gexclamation,gok)
			endif
	case(5128)
		call gmremovewindow(iqwin)
case(5127)
			iqplot=gmenqvaluesetting(ivalq)
			if(iqplot.lt.1) iqplot=1
			if(nplot.gt.0.and.iqplot.gt.nplot+1) iqplot=nplot+1
			call gmremovewindow(iqwin)

				if(allocated(xval)) deallocate(xval,yval)
						if(allocated(xcal)) DEALLOCATE(Xcal,Ycal)
						if(allocated(icurvw)) DEALLOCATE(icurvw)
								if(allocated(w)) DEALLOCATE(w)
					
						if(allocated(ndat)) DEALLOCATE(ndat,isym,ijoin,symsiz)
						if(allocated(ncal)) DEALLOCATE(ncal,iline)
						if(allocated(icurvd)) DEALLOCATE(icurvd,icurvc)
						kwi=oldrecords(modplot)%kwi
						kwj=oldrecords(modplot)%kwj
						ndv1=oldrecords(modplot)%ndv1
						ndimd=oldrecords(modplot)%ndimd
						ndc1=oldrecords(modplot)%ndc1
						ndimc=oldrecords(modplot)%ndimc
						ipos=oldrecords(modplot)%ipos
						n1=1
						if(oldrecords(modplot)%hdisp) n1=0
						ALLOCATE(Xval(n1:ndv1,ndimd),Yval(n1:ndv1,ndimd),Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
						ALLOCATE(icurvw(ndimd),w(kwi,kwj))
						ALLOCATE(ndat(njset),isym(njset),ijoin(njset),symsiz(njset))
						ALLOCATE(ncal(ndimc),iline(ndimc))
						ALLOCATE(icurvd(ndimd),icurvc(ndimc))
						isens=0
				call store_record(iplotype,igraph,iplot,&
								ixp,iyp,ipos,xval,yval,w,&
								nj,niobs,njset,nplot,nset,&
								juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
								wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
								ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
								xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,oldrecords,&
								dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,&
								parval,ifitype)
			
				call write_plotq(iqplot,iplotype,xval,yval,xcal,ycal,ndimd,ndimc,&
				ncurvd,ndat,icurvd,isym,ijoin,ncurvc,ncal,icurvc,iline,syms,&
				xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ntx,nty,itx,ity,&
				xlo,xhi,ylo,yhi,itit,title1,ilog,iscal,doframe,&
				titlex,titley,ilabel,inumx,inumy,efile,sval,&
				ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,y1v,y2v,y1c,y2c,ivplot,&
				theta,ifitype,ncomp,idest,interp,&
				RLTH,y0,yinf,ntrace,ytsep,ndv1,ndc1,w,kwi,kwj,icurvw,kmax,n1,jmiss,&
				xwbase,isval,lt2,mono,ifont0,ameant,areat&
				title3,xval3,yval3,z3,bad,nx,ny,ndx,ndy,&
     	        xtitle3,ytitle3,ztitle3,idrawt,idrawx,idrawy,idrawz,&
                kcol,posneg,i3col,ifnt3,ifnl,alfa,beta,gama,delta,&
                ijust,ijusx,ijusy,ijusz,&
                xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,numx,numy,&
                xmin3,xmax3,ymin3,ymax3,zmin3,zmax3,xtic3,ytic3,ztic3,&
                ratio,rat,radius,theta3,phi,ifram,numaxi,nwidth,npow,nplace,&
                nxstep,nystep,istyle,isup,&
                fill,inter,axis,fillbad,cross,ioffs)
				
			
			


case(5150)
	if(readrec) then
		if(modplot.gt.0.and.lframe.eq.graphics1_1(modplot)) then
		callid=5150+modplot
		goto 2
	else
			imessy=gmDisplayMessageBox('','Select a graph',Gexclamation,gok)
			
	endif
	else
		imessy=gmDisplayMessageBox('','No record on screen/Not available for this record',Gexclamation,gok)
			
	endif
	

case (5151:5175) ! print
            landscape=.true.
			modplot=callid-5150
			ifontrue=101
			igraph=modplot
			idev=6
			call ttfont(main,ittfont,5126,idev,ittfont_Toggle)
case(5126)
				do i=1,6
				istat1=gmenqtoggleswitch(ittfont_Toggle(i))
				if(istat1.eq.gon) ifontref=i
			enddo
			select case(ifontref)
				case(1)
					ifontrue=100
				case(2)
					ifontrue=101
				case(3)
					ifontrue=102
				case(4)
					ifontrue=150
				case(5)
					ifontrue=151
				case(6)
					ifontrue=0
			end select
			do i=1,5
				istat2=gmenqtoggleswitch(ittfont_Toggle(i+10))
				if(istat2.eq.gon) sizefac=3+0.5*i
			enddo
			do i=1,3
				istat2=gmenqtoggleswitch(ittfont_Toggle(i+20))
				if(istat2.eq.gon) widfac=0.25*(i+2)
			enddo
				
				istat2=gmenqtoggleswitch(ittfont_Toggle(26))
				if(istat2.eq.gon) then
					idev=6
				else
					idev=3
				endif
				istat2c=gmenqtoggleswitch(ittfont_Toggle(16))
				if(istat2c.eq.gon) then
					show_curve=.true.
				else
					show_curve=.false.
				endif
				istat2=gmenqtoggleswitch(ittfont_Toggle(27))
				if(istat2.eq.gon) then
					idev=3
					idevp=3
				endif
				
			call gmremovewindow(ittfont)

			if(readrec) then
			if(modplot.gt.0) then
			if(ifontrue.ne.0) then
								do ib=1,100
									oldrecords(modplot)%attributes%IFNT(ib)=ifontrue
									ifnt(ib)=ifontrue
								enddo
								oldrecords(modplot)%attributes%IFNT(4)=15
									ifnt(4)=15
								endif
			
		
								if(sizefac.le.5) then
								do ib=1,5
								oldrecords(modplot)%attributes%SIZETEXT(ib)=sizefac
								SIZETEXT(ib)=sizefac
								enddo
								do ib=6,100
									oldrecords(modplot)%attributes%SIZETEXT(ib)=sizefac-0.5
									SIZETEXT(ib)=sizefac
								enddo
								oldrecords(modplot)%attributes%SIZETEXT(2)=sizefac-1
								SIZETEXT(2)=sizefac-1
								endif
				mplot=0
			
				istatus=gmprintercontrol(gprintersetup)
				if(istatus.eq.0) goto 1 
				istatus=gmprintercontrol(GOPENPRINTER)
				IF(ISTATus.NE.0)THEN
				
					plot=.true.
					!idev=6
					m1=1
			
					m1=modplot
					if(oldrecords(modplot)%IPOS.eq.0) then
						m1=modplot
						msplot=1
					else
						
					
						do i=m1,m1+3
							if(jopen(i).eq.1) msplot=msplot+1
						enddo 
					endif	
				
					
					icallprev=5150
					
					callid=5250
				
					goto 2
				endif
				
				endif
			else
				imessy=gmDisplayMessageBox('','No record on screen/Not available for this record',Gexclamation,gok)
		
			endif
	

case(5200) !bmp
	if(readrec) then
		if(modplot.gt.0.and.lframe.eq.graphics1_1(modplot)) then
		callid=5200+modplot
		goto 2
	else
			imessy=gmDisplayMessageBox('','Select a graph',Gexclamation,gok)
			
	endif
	else
		imessy=gmDisplayMessageBox('','No record on screen/Not available for this record',Gexclamation,gok)
			
	endif

case (5201:5225) ! bmp,wmf
        landscape=.false.
		modplot=callid-5200
		igraph=modplot
		ifontrue=101
		callid=5226
		goto 2
case(5226)
	
		if(readrec) then
		if(modplot.gt.0.and.lframe.eq.graphics1_1(modplot)) then
			sfile='*.wmf'
			CALL gmFileBROWSER(sFILE,sDIR,sFILT,gmBrowseType=1)
	
			IF(sFILE.ne.' ') then
					msplot=0
			    msplot=1
				if(oldrecords(modplot)%IPOS.eq.1) then
				
					do i=modplot+1,modplot+3
						if(oldrecords(i)%IPOS.eq.1) goto 45
						if(jopen(i).eq.1) msplot=msplot+1
					enddo 
				endif	
45				continue
					icallprev=5200
					ns=nblank1(sfile)
					if (sfile(ns-3:ns).eq.'.wmf') then
					    idev=1
	   					pwmf=.true.
	   				
	   					idpi=600
	   					ixoff=0
	   					iyoff=0
	   					iwidi=4800
	   					ihei=3300 !3600
	   				ifontrue=101
					!else if (sfile(ns-3:ns).eq.'.bmp') then
					else
	   					pbmp=.true.
	   				
	   					idev=2
	   			ifontrue=0
	   				endif 
					
				call ttfont(main,ittfont,5250,idev,ittfont_Toggle)
			endif
		else
				imessy=gmDisplayMessageBox('',&
				'Select a model/graph',Gexclamation,gok)	
		endif
		else
			imessy=gmDisplayMessageBox('','No record on screen/Not available for this record',Gexclamation,gok)
		endif
	
case(5250) ! PRINT,WMF,BMP
	if(pbmp.or.pwmf) then
		do i=1,6
				istat1=gmenqtoggleswitch(ittfont_Toggle(i))
				if(istat1.eq.gon) ifontref=i
			enddo
			select case(ifontref)
				case(1)
					ifontrue=100
				case(2)
					ifontrue=101
				case(3)
					ifontrue=102
				case(4)
					ifontrue=150
				case(5)
					ifontrue=151
				case(6)
					ifontrue=0
			end select
			do i=1,5
				istat2=gmenqtoggleswitch(ittfont_Toggle(i+10))
				if(istat2.eq.gon) sizefac=3+0.5*i
			enddo
			do i=1,3
				istat2=gmenqtoggleswitch(ittfont_Toggle(i+20))
				if(istat2.eq.gon) widfac=0.25*(i+2)
			enddo
		
			istat2c=gmenqtoggleswitch(ittfont_Toggle(16))
				if(istat2c.eq.gon) then
					show_curve=.true.
				else
					show_curve=.false.
				endif
				istat2=gmenqtoggleswitch(ittfont_Toggle(27))
				if(istat2.eq.gon) then
					idev=3
					idevp=3
				endif
				
	call gmremovewindow(ittfont)
	if(ifontrue.ne.0) then
								do ib=1,100
									oldrecords(modplot)%attributes%IFNT(ib)=ifontrue
									ifnt(ib)=ifontrue
								enddo
							!	oldrecords(modplot)%attributes%IFNT(4)=15
							!		ifnt(4)=15
								endif
			
		
								if(sizefac.le.5) then
								do ib=1,5
								oldrecords(modplot)%attributes%SIZETEXT(ib)=sizefac
								SIZETEXT(ib)=sizefac
								enddo
								do ib=6,100
									oldrecords(modplot)%attributes%SIZETEXT(ib)=sizefac-0.5
									SIZETEXT(ib)=sizefac
								enddo
								oldrecords(modplot)%attributes%SIZETEXT(2)=sizefac-1
								SIZETEXT(2)=sizefac-1
								endif
		call devsus 

	endif
			if(readrec) then
				mplot=0
				ifirstprint=0
				do i=modplot,modplot+msplot-1
					if(jopen(i).eq.1) then
						modplot=i
						igraph=i
						mplot=mplot+1
						if(allocated(xval)) deallocate(xval,yval)
						if(allocated(xcal)) DEALLOCATE(Xcal,Ycal)
						if(allocated(icurvw)) DEALLOCATE(icurvw)
								if(allocated(w)) DEALLOCATE(w)
					
						if(allocated(ndat)) DEALLOCATE(ndat,isym,ijoin,symsiz)
						if(allocated(ncal)) DEALLOCATE(ncal,iline)
						if(allocated(icurvd)) DEALLOCATE(icurvd,icurvc)
						kwi=oldrecords(modplot)%kwi
						kwj=oldrecords(modplot)%kwj
						ndv1=oldrecords(modplot)%ndv1
						ndimd=oldrecords(modplot)%ndimd
						ndc1=oldrecords(modplot)%ndc1
						ndimc=oldrecords(modplot)%ndimc
						ipos=oldrecords(modplot)%ipos
						n1=1
						if(oldrecords(modplot)%hdisp) n1=0
						ALLOCATE(Xval(n1:ndv1,ndimd),Yval(n1:ndv1,ndimd),Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
						ALLOCATE(icurvw(ndimd),w(kwi,kwj))
						ALLOCATE(ndat(njset),isym(njset),ijoin(njset),symsiz(njset))
						ALLOCATE(ncal(ndimc),iline(ndimc))
						ALLOCATE(icurvd(ndimd),icurvc(ndimc))
						isens=0
						
						call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xval,yval,w,nj,niobs,njset,nplot,nset,&
								juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
								wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
								ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
								xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,oldrecords,&
								dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,&
								parval,ifitype)
						
						if((pbmp.or.pwmf).and.ipos.le.1) then
								  if(icallprev.ne.5200) then
									call intconv(i,cnum0)
									nlp=nblank1(dfile)
									sfile=dfile(1:nlp-4)//'_'//cnum0
									npl=nblank1(sfile)
									if(pwmf) then
										sfile=sfile(1:nlp)//'.wmf'
									else if(pbmp) then
										sfile=sfile(1:nlp)//'.bmp'
								    endif
							      endif
								  if(mplot.eq.1) call devsus
								  if(pwmf) call wmfp(idpi,ixoff,iyoff,iwidi,ihei)
								  if(pbmp) call bmp
								  call devfil(sfile,0)
								  plot=.true.
						endif
						call genqlinewidthmode(isw9)
						call genqlinewidth(wid9) 
						call genqselectedpen(icol9,wid9,itip9)
						call gEnqLineWidthScaling(scale9) 
						xbeg4=xbeg(44)
						xend4=xend(44)
						ybeg4=ybeg(44)
						yend4=yend(44)
						xbeg5=xbeg(45)
						xend5=xend(45)
						ybeg5=ybeg(45)
						yend5=yend(45)
						jplot=oldrecords(modplot)%iplot	
						call graph1(modplot,jplot,main,ixp,iyp,graph1_1,GraphMainPanel1_1,graphics1_1,&
						wxmin,wxmax,wymin,wymax,ipos,gfile,plot,ixposv,iyposv)
						
						call draw_axis(xmin,xmax,ymin,ymax,ymin1,ymax1,xtic,ytic,&
						nx1,ny1,xcross,ycross,itx,ity,tlenx,tleny,logx,logy,sqrty,doframe,&
						icol,ntx,nty,idev,thick,itype,&
            calbarx,calbary,xbeg4,ybeg4,xend4,yend4,xbeg5,ybeg5,xend5,yend5)
						call draw_text(parval,xtitle,ytitle,ztitle,title1,cnumx,cnumy,&
						cnumz,cexpx,cexpy,cexpz,newtext,numbx,numby,numbz,ntext,inumx,&
						inumy,intx,angle,sizetext,ijus,thick,rx,ry,idraw,icol,ifnt,str,&
						dxs,dys)
						yfn=1.1*ry(1)
						!if(sqrty) yfn=1.2*sqrt(ymax)
							ymin20=ymin
							ymax20=ymax
							if(sqrty) ymin20=sqrt(ymin)
							if(sqrty) ymax20=sqrt(ymax)
							xt=0.5*(xmin+xmax)
							yt=ymax20+0.2*(ymax20-ymin20)!new
							nl=nblank1(gfile)
							call intconv(iplot,cnum0)
							icp=0
							if(idev.ge.3) icp=48
							theg=3.
							if(ipos.gt.0) theg=3.5
							call write_string(gfile(1:nl)//'-plot:'//cnum0,xmax,yt,0.,0,0,theg,icp,dxs,dys)
			
					!	call write_string(gfile,xmax,ry(1),0.,0,0,2.,0,dxs,dys)
					
						if(hdisp) then
						call draw_hist(xval,yval,icurvd,ncurvd,ndelt,logity,logx,&
						logy,sqrty,y0,yinf,x0,ilog,xmin,xmax,ymin,ymax,idev,ndat,ijoin,&
						icol,thick,ndv1,ndimd,xwbase,lt2)
								
						else
						call draw_data(xval,yval,icurvd,ncurvd,ndelt,logity,logx,logy,sqrty,&
						y0,yinf,x0,ilog,idev,ndat,ijoin,icol,isym,&
						symsiz,thick,barleng,interp,npint,Xint,Yint,nint,yp1,ypn,&
						Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw,&
									ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil)
						endif
						call draw_curves(xcal,ycal,icurvc,ncurvc,logity,logx,&
						logy,sqrty,y0,yinf,x0,ilog,idev,&
						wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
						xmin,xmax,ymin,ymax,ncal,&
						iline,icol,thick,ndc1,ndimc,jfirst,dxs,dys)
					
						if(idev.eq.3) show_curve=.false.
						if(show_curve) then
						ymin20=ymin
							ymax20=ymax
							if(sqrty) ymin20=sqrt(ymin)
							if(sqrty) ymax20=sqrt(ymax)
							xt=0.5*(xmin+xmax)
							yt=ymax20+0.15*(ymax20-ymin20)
								ytc=0.05*(ymax20-ymin20)
								if(ncurvc.gt.0) call write_string('Data:',xmax+0.11*(xmax-xmin),ymax20+0.01*(ymax20-ymin20),0.,0,101,&
								3.,icol(151),dxs,dys)
								
						do j1=1,ncurvc
									j=icurvc(j1)
									if(iline(j).ge.0) then
									call intconv(j,cnum0)
									call write_string(cnum0,xmax+0.05*(xmax-xmin),ymax20-ytc,0.,0,101,&
									3.,icol(j+150),dxs,dys)
									ij=iabs(iline(j))
									linetype=ij
									if(ij.gt.0.and.ij.le.9) then
									call dotted_lines(ij,xmax+0.1*(xmax-xmin),xmax+0.2*(xmax-xmin),&
									ymax20-ytc,dxs)
								    goto 768
									
									else if(ij.ge.10) then
									linetype=ij-10			!join points with straight line type #ij
									endif
									call broken(linetype)
									call movto2(xmax+0.1*(xmax-xmin),ymax20-ytc)
									call linto2(xmax+0.2*(xmax-xmin),ymax20-ytc)
768									ytc=ytc+0.05*(ymax20-ymin20)
									endif
								enddo		
					      
						endif
							call draw_lines(IDRAW,ICOL,THICK,C_THICK,NARROW,NLINE,NHLINE,NVLINE, &
						XBEG,YBEG,XEND,YEND,y0,yinf,x0,dxs,dys)
						xmin0=xmin1
						xmax0=xmax1
						ymin0=ymin1
						ymax0=ymax1
						idest=0
						
					endif
					
				enddo
				
				if(pwmf) then
						pwmf=.false.
						CALL DEVEND
      					CALL guiwin
				else if(pbmp) then
						pbmp=.false.
						CALL DEVEND
      					CALL guiwin
				else
						CALL GUIPRT(1,ISTATus)
						plot=.false.
				
				endif
				
				idev=0
				icallprev=0
                
			else
				imessy=gmDisplayMessageBox('','No record on screen/Not available for this record',Gexclamation,gok)
			
			endif
			
		



!case(403) !no of sets
case(5303) !no of sets

			IF(READREC.AND.NSET.GT.1.and.oldrecords(modplot)%IPOS.ne.4) then
				iks=1
				do i=1,nset
					ikset(i)=-1
				enddo
				do i=1,2
					ifstat(i)=gmEnqListEntry(Combo1_3,i,textcombo)
				enddo
				if(ifstat(2).eq.2) then
				    call select_sets(main,nset,select_set,itogset)
					ifstat(2)=0 
					callid=0
				else if(ifstat(1).eq.2) then
					do i=1,nset
						jmiss(i)=0
					enddo
					callid=5310
					ifstat(1)=0
					goto 2
				endif
			ENDIF
		case(5311:5330)
			ikset(iks)=callid-5310
			iks=iks+1
			inkset=ikset(1)
        case(5310)
			nsfit=0
			do i=1,nset
				if(ikset(i).ne.-1) then
					nsfit=nsfit+1
				endif
			enddo
		 
			if(nsfit.lt.nset.and.nsfit.ge.1) then
				do i=1,nset
					jmiss(i)=1	!miss all
				enddo
				nmiss=nset-nsfit
				if(nmiss.gt.0) then
					do j=1,nsfit
						jmiss(ikset(j))=0		!fit set j1
					enddo
				endif
			else		!fit all
				nmiss=0
				do i=1,nset
					jmiss(i)=0	!fit all
				enddo
			endif
			nsfit=0
			do j=1,nset
				if(jmiss(j).eq.0) then
					nsfit=nsfit+1
					juse(nsfit)=j
				endif
			enddo
			if(nset.eq.1) then
				jset=1
				nsfit=1
				juse(1)=1
			endif
			call SORTI(juse,nsfit,.true.,njset)
			call draw_data(xval,yval,icurvd,ncurvd,ndelt,logity,logx,logy,sqrty,&
				y0,yinf,x0,ilog,idev,ndat,ijoin,icol,isym,&
				symsiz,thick,barleng,interp,npint,Xint,Yint,nint,yp1,ypn,&
				Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw,&
									ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil)			
case(5501:5999) ! change values or read new values for newfile
		  if(ifiltype.eq.1) then
		
			if(callid.ge.5501.and.callid.le.5600) then
			 modplot=callid-5500
			 isvpl=1
			else if(callid.ge.5601.and.callid.le.5700) then
			 if(newfirst.ne.-1) then
			 newfile=.true. 
			 saveandplot=.true.
			 endif
			 isvpl=1
			 modplot=callid-5600
			 
			else if(callid.ge.5701.and.callid.le.5800) then
				modplot=callid-5700
				isvpl=6
				
			else if(callid.ge.5801.and.callid.le.5900) then
				modplot=callid-1800
				isvpl=6
				newfile=.true.
				saveandplot=.true.
			else if(callid.ge.5901.and.callid.le.5999) then
			 modplot=callid-5900
			 isvpl=7
			endif
			if(.not.newfile) then
				isens=0
				call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xobs,yobs,w,nj,niobs,njset,nplot,nset,&
				juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
				wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
				ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
				xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,oldrecords,&
				dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,parval,ifitype)  
		
				nset=oldrecords(modplot)%numsets
				do j=1,nset
					ndatset(modplot,j)=oldrecords(modplot)%nj(j)
				enddo
			endif
		    if(isvpl.eq.7) then
			    do jset=1,numset(modplot)
			
				do j=1,nrows
				do i=1,ncols
				if(newfile) then
				call gmSetCellSetting(ValArray6(modplot,jset), i,j ,gmValue= 0. )
				else
			
				call gmSetCellSetting(ValArray6(modplot,jset), 1,j ,xobs(j,jset))
				call gmSetCellSetting(ValArray6(modplot,jset), 2,j ,yobs(j,jset))
				call gmSetCellSetting(ValArray6(modplot,jset), 3,j ,w(j,jset))
				call gmSetCellSetting(ValArray6(modplot,jset), 4,j ,w(j,jset))
				endif
				enddo
			    enddo
			    enddo
			    goto 1
			endif
			if(isvpl.eq.1.or.isvpl.eq.6) then
			    if(newfile) then
			    inewplot=inewplot+1
                if(nplot.gt.1) then
				    if(inewplot.ne.modplot) then
					imessy=gmDisplayMessageBox('','Please save records in order',Gexclamation,gok)
					inewplot=inewplot-1
					goto 1
					endif
				endif
			    do k=1,numset(modplot)
					ij=2
					do i=2,20
						call gmEnqCellSetting(ValArray6(modplot,k), 1,i,xdata(modplot,k,1,i),textcell)
						if(xdata(modplot,k,1,i).ne.0.) ij=ij+1
					enddo
					ndatset(modplot,k)=ij-1
					if (ij.le.1) then
				    imessy=gmDisplayMessageBox('','Please fill in the data',Gexclamation,gok)
		
					goto 1
					endif
				enddo
			    endif
			    do k=1,numset(modplot)
				if(newfile)then
				  call gmEnqTextSetting(static6,stitle(k,modplot))
				  
				endif
				do i=1,ncols
					do j=1,ndatset(modplot,k)
						call gmEnqCellSetting(ValArray6(modplot,k), i,j , &
						xdata(modplot,k,i,j),textcell)
					enddo
				enddo
			    enddo
			endif		
			n1=1
			
			iqwin = gmCreateComplexDialogueBox(Main, 15, 5, 12, 6, GALL, 'Save record', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
	
			iqPanel=gmCreatePanel(iqwin, 0, 0, 12,6, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmtitle=' ',gmExpand=GOff, gmType=GNOBOUNDARY, &
            	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
	    
			itextq = gmCreateTextEntry(iqPanel, 1,1, 7, 1,'File:'//dfile, 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
			iqplot=nplot+1
			if(iqplot.eq.0) iqplot=1
			call intconv(nplot,cnum5)
			itextq = gmCreateTextEntry(iqPanel, 1,2, 11, 1,'file has nplots='//cnum5, 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
			
			itextq = gmCreateTextEntry(iqPanel, 1,3, 5, 1,'Queue as number=', 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
				
            ivalq=gmCreateValueEntry(iqpanel, 7, 3, 4, 1,iqplot , 4, 0, gedit,&
				gmVpos=GTOP)
            
			ivb=gmCreatePushButton(iqpanel,1,0, 5, 1,'Cancel',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=5228)
			
			ivb=gmCreatePushButton(iqpanel,6,0, 5, 1,'Save',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=5227)
			
			call gmdrawwindow(iqwin)
			else
				imessy=gmDisplayMessageBox('','No record on screen/Not available for this record',Gexclamation,gok)
			endif
			
	case(5227)
		    call gmremovewindow(iqwin)
			if(newfile) then
				title1(1:60)=newrecords(modplot)%STR(1)
				titlex=newrecords(modplot)%STR(3)
				titley=newrecords(modplot)%STR(4)
				nset=newrecords(modplot)%numsets	
				call read_new_values(xdata,njset,ncols,niobs,xobs,yobs,ndatset,numset(modplot),ndv1,&
				w,kwi,kwj,n1,hdisp,nplot,modplot,Data_list,newfile,xvalold,yvalold)
				iunit=12
				
				call write_record(modplot,ifiltype,numset(modplot),nsfit,nrows,niobs,&
				njobs,stitle,hdisp,xobs,yobs,w,ndatset,nwfile,title1,titlex,titley,iw,setx,&
				setvar,ilabel,iunit,fopen_11,fopen_12)	
			else
			
				call read_new_values(xdata,njset,ncols,niobs,xval,yval,ndatset,ndimd,ndv1,&
				w,kwi,kwj,n1,hdisp,nplot,modplot,Data_list,newfile,xvalold,yvalold)
				do j=1,oldrecords(modplot)%numsets
				do k=1,oldrecords(modplot)%nj(j)
					xobs(k,j)=xval(k,j)
					yobs(k,j)=yval(k,j)
	   			enddo
				enddo
				if(isvpl.eq.1) then
					iunit=11
					title1(1:60)=oldrecords(modplot)%STR(1)
					titlex=oldrecords(modplot)%STR(3)
					titley=oldrecords(modplot)%STR(4)
					
					call write_record(modplot,ifiltype,nset,nsfit,nrows,niobs,&
					njobs,stitle,hdisp,xobs,yobs,w,ndatset,gfile,title1,titlex,titley,iw,setx,&
					setvar,ilabel,iunit,fopen_11,fopen_12)
				endif
			endif
			if(newfile) then
			ncurvc=0
				if(allocated(xval)) deallocate(xval,yval)
				if(allocated(xcal)) DEALLOCATE(Xcal,Ycal)
				if(allocated(icurvw)) DEALLOCATE(icurvw)
				if(allocated(theta)) DEALLOCATE(theta)
				if(allocated(thetgues)) DEALLOCATE(thetgues)
				if(allocated(ndat)) DEALLOCATE(ndat)
				if(allocated(isym)) DEALLOCATE(isym)
				if(allocated(ijoin)) DEALLOCATE(ijoin)
				if(allocated(symsiz)) DEALLOCATE(symsiz)
				if(allocated(ncal)) DEALLOCATE(ncal)
				if(allocated(iline)) DEALLOCATE(iline)
				if(allocated(icurvd)) DEALLOCATE(icurvd)
				if(allocated(icurvc)) DEALLOCATE(icurvc)
				hdisp=.false.
				autplt=.false.
				ALLOCATE(Xval(ndv1,njset),Yval(ndv1,njset),&
				Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
							
				ALLOCATE(icurvw(njset))
				if(kmax.lt.1) kmax=1		!must allocate theta whether used or not
				ALLOCATE(theta(kmax))
				ALLOCATE(thetgues(100,nset))
				ALLOCATE(ndat(njset),isym(njset),ijoin(njset),symsiz(njset))
				ALLOCATE(ncal(ndimc),iline(ndimc))
				ALLOCATE(icurvd(njset),icurvc(ndimc))
				ncurvd=0
								
				do j=1,numset(modplot)
					
					iline(j)=0
					ncal(j)=0
					icurvc(j)=j
	   				ndat(j)=ndatset(modplot,j)
					nj(j)=ndat(j)

					icurvd(j)=j		!display all sets
	   				icurvw(j)=isdev		!new for VPLOT5!
	   				isign=(-1)**j
	   				isym(j)=isign*(j+2)	!alternate filled/hollow symbols
	   				if(ndat(j).le.10) then
	      				symsiz(j)=3.0
	   				else if(ndat(j).gt.10.and.ndat(j).le.30) then
	      				symsiz(j)=2.0
	   				else
	      				symsiz(j)=1.5
	   				endif
	   				ijoin(j)=-1		!points not joined
	   				do k=1,ndatset(modplot,j)
						xval(k,j)=xobs(k,j)
						yval(k,j)=yobs(k,j)
	   				enddo
					if(jmiss(j).ne.1) then	!set omitted
						ncurvd=ncurvd+1
						icurvd(ncurvd)=j
					endif
				enddo
				n1=1
				call calc_default(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,symsiz,&
				ndimd,XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,&
				XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,&
				XLO1,XHI1,YLO1,YHI1,y0,yinf,inumx,inumy,&
				titlex,titley,ilabel,doframe,autplt,itit,title1,&
				ISHP,ifont,landscap,ndv1,ndc1,w,kwi,kwj,icurvw,kmax,iver,&
				redrawn,plot,redo,pcgm,pwmf,npint,hdisp,n1,tlenx,tleny,nx1,ny1,izoom,&
				xtitle,ytitle,rescale,dxs,dys,ifitype,parval,theta,ncomp,isval,sval,&
								ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil)
				ixp=24*(modplot-1)
				iyp=24*(modplot-1)
				ipos=0
				ndimd=njset
				do ns=1,nset
					jmiss(ns)=0
				enddo
				nsfit=0
				if(nset.eq.1) jset=1
				do j=1,nset
					if(jmiss(j).eq.0) then
						nsfit=nsfit+1
						juse(nsfit)=j
					endif
				enddo
				if(nset.eq.1) then
					jset=1
					nsfit=1
					juse(1)=1
				endif
				if(isvpl.eq.1) then
				readrec=.true.
				iopen(modplot)=2
				jopen(modplot)=1
				oldrecords(modplot)%iplot=modplot
				jplot=oldrecords(modplot)%iplot
				ixp=80+modplot
				iyp=80 +modplot
				call graph1(modplot,jplot,main,ixp,iyp,graph1_1,GraphMainPanel1_1,graphics1_1,&
								wxmin,wxmax,wymin,wymax,ipos,gfile,plot,ixposv,iyposv)
							
				call draw_axis(xmin,xmax,ymin,ymax,ymin1,ymax1,&
							    xtic,ytic,nx1,ny1,xcross,ycross,itx,ity,tlenx,tleny,&
								logx,logy,sqrty,doframe,icol,ntx,nty,idev,thick,itype,&
            calbarx,calbary,xbeg4,ybeg4,xend4,yend4,xbeg5,ybeg5,xend5,yend5)
				call draw_text(parval,xtitle,ytitle,ztitle,title1,cnumx,cnumy,&
								cnumz,cexpx,cexpy,cexpz,newtext,numbx,numby,numbz,ntext,inumx,&
								inumy,intx,angle,sizetext,ijus,thick,rx,ry,idraw,&
								icol,ifnt,str,dxs,dys)
				endif					
			else
			    
				dxs=oldrecords(modplot)%dxs
				dys=oldrecords(modplot)%dys
				nset=oldrecords(modplot)%numsets
				do j=1,oldrecords(modplot)%numsets
				do k=1,oldrecords(modplot)%nj(j)
					xvalold(k,j)=oldrecords(modplot)%xval(k,j)
					yvalold(k,j)=oldrecords(modplot)%yval(k,j)
				enddo
				enddo

				do i=101,150
					icoltemp(i)=icol(i)
					icol(i)=0
				enddo
				call gmActivateGraphicsFrame(graphics1_1(modplot))
				if(hdisp) then
				call draw_hist(xvalold,yvalold,icurvd,ncurvd,ndelt,logity,logx,&
				logy,sqrty,y0,yinf,x0,ilog,xmin,xmax,ymin,ymax,idev,ndat,ijoin,&
				icol,thick,ndv1,ndimd,xwbase,lt2)
								
				else
				call draw_data(xvalold,yvalold,icurvd,ncurvd,ndelt,logity,logx,logy,sqrty,&
				y0,yinf,x0,ilog,idev,ndat,ijoin,icol,isym,&
				symsiz,thick,barleng,interp,npint,Xint,Yint,nint,yp1,ypn,&
				Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw,&
									ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil)
				endif
				do i=101,150
				icol(i)=icoltemp(i)					
				enddo
			
            endif
			isens=1
			call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xobs,yobs,w,nj,niobs,njset,nplot,nset,&
				juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
				wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
				ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
				xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,oldrecords,&
				dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,parval,ifitype)  
			if(newfile) then
			if(inewplot.eq.nplotnew) then
				newfile=.false.
				newfirst=-1
				gfile=nwfile
				iunit=11
				inewplot=0
			endif
			if(isvpl.ne.1) goto 1
			endif
			if(hdisp) then
				call draw_hist(xval,yval,icurvd,ncurvd,ndelt,logity,logx,&
				logy,sqrty,y0,yinf,x0,ilog,xmin,xmax,ymin,ymax,idev,ndat,ijoin,&
				icol,thick,ndv1,ndimd,xwbase,lt2)
								
			else	
				call draw_data(xval,yval,icurvd,ncurvd,ndelt,logity,logx,logy,sqrty,&
				y0,yinf,x0,ilog,idev,ndat,ijoin,icol,isym,&
				symsiz,thick,barleng,interp,npint,Xint,Yint,nint,yp1,ypn,&
				Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw,&
									ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil)
			endif
			
		

!=====================================================================	
	
!	case(701:706,710) !fitmode
	case(6001:6006,6010) !fitmode
	    iflag=0
		if(modplot.le.10.and.readrec.and.ifiltype.eq.1.and.oldrecords(modplot)%IPOS.eq.0) then

			if(allocated(xval)) deallocate(xval,yval)
						if(allocated(xcal)) DEALLOCATE(Xcal,Ycal)
						if(allocated(icurvw)) DEALLOCATE(icurvw)
						if(allocated(ndat)) DEALLOCATE(ndat,isym,ijoin)
						if(allocated(ncal)) DEALLOCATE(ncal,iline)
						if(allocated(icurvd)) DEALLOCATE(icurvd,icurvc)
						ndv1=oldrecords(modplot)%ndv1
						ndimd=oldrecords(modplot)%ndimd
						ndc1=oldrecords(modplot)%ndc1
						ndimc=oldrecords(modplot)%ndimc
						n1=1
						if(hdisp) n1=0			
						ALLOCATE(Xval(n1:ndv1,ndimd),Yval(n1:ndv1,ndimd),Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
						ALLOCATE(icurvw(ndimd))
						ALLOCATE(ndat(ndimd),isym(ndimd),ijoin(ndimd))
						ALLOCATE(ncal(ndimc),iline(ndimc))
						ALLOCATE(icurvd(ndimd),icurvc(ndimc))
						isens=0.
				call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xobs,yobs,w,nj,niobs,njset,nplot,nset,&
				juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
				wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
				ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
				xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,&
				oldrecords,dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,&
				parval,ifitype)
						
	
		        nset=oldrecords(modplot)%numsets
				iks=1
				if(nset.gt.1) then
				do i=1,nset
					ikset(i)=-1
				
			!		jmiss(i)=1
			!		icurvd(i)=i
					iline(i)=-1
				enddo
				endif
			if(nset.eq.1) then
				ifitmode=1
				jcset=1
				callid=6022
				goto 2
			endif
			if (callid.eq.6001.and.nset.gt.1) then
				ifitmode=1
				nsfit=1
				itcall=6010
				ntog=nset+1
				do i=2,nset+1
				    call intconv(i-1,cnum0)
					text_tog(i)='set: '//cnum0(1:2)
				enddo
				text_tog(1)='All'
			    call toggle_panel(Main,ITOGGLE,ittoglepanel,ntog,text_tog,intoggle,itcall,valdat,idat,itogbutton)		
				do i=1,nset
				!	if(jmiss(i).eq.1) call gmSetWidgetStatus(intoggle(i+1), GunSELECTABLE)
				enddo
				call gmSetWidgetStatus(intoggle(1), GunSELECTABLE)
			else if(callid.eq.6002) then
				ifitmode=2
				sep=.true.
				sameq=.true.
				ikset(1)=inkset
				itcall=6010
				ntog=nset+1
				do i=2,nset+1
				    call intconv(i-1,cnum0)
					text_tog(i)='set: '//cnum0(1:2)
				enddo
				text_tog(1)='All'
			    call toggle_panel(Main,ITOGGLE,ittoglepanel,ntog,text_tog,intoggle,itcall,valdat,idat,itogbutton)		
				do i=1,nset
				!	if(jmiss(i).eq.1) call gmSetWidgetStatus(intoggle(i+1), GunSELECTABLE)
				enddo
				call gmsetToggleswitch(inToggle(1),gon)
			!	 callid=6021
			!	goto 2
			 else if(callid.eq.6010) then
			    itcall=6010
				ntog=nset+1
				do i=2,nset+1
				    call intconv(i-1,cnum0)
					text_tog(i)='set: '//cnum0(1:2)
				enddo
				ifitmode=2
				sep=.true.
				sameq=.false.
				text_tog(1)='All'
			    call toggle_panel(Main,ITOGGLE,ittoglepanel,ntog,text_tog,intoggle,itcall,valdat,idat,itogbutton)		
			 	call gmsetToggleswitch(inToggle(1),gon)
				do i=1,nset
				!	if(jmiss(i).eq.1) call gmSetWidgetStatus(intoggle(i+1), GunSELECTABLE)
				enddo
			 else
				IFITMODE=callid-6000
				sep=.false.
				if(ifitmode.eq.4.or.ifitmode.eq.5) then
				
					istatusf=gmDisplayMessageBox(' ','Not available for this version' ,GSTOP,GOK)
				else
				
				IF(IFITMODE.NE.1) then
					 itcall=6010
				     ntog=nset+1
					 	text_tog(1)='All'
					do i=2,nset+1
				    call intconv(i-1,cnum0)
					text_tog(i)='set: '//cnum0(1:2)

					enddo
				
					call toggle_panel(Main,ITOGGLE,ittoglepanel,ntog,text_tog,intoggle,itcall,valdat,idat,itogbutton)		
					do i=1,nset
				!	if(jmiss(i).eq.1) call gmSetWidgetStatus(intoggle(i+1), GunSELECTABLE)
				enddo
					call gmsetToggleswitch(inToggle(1),gon)
				else
					callid=6022
					goto 2
				endif
				endif 	
			 endif
		else
			imessy=gmDisplayMessageBox('','No record on screen/Not available for this record',Gexclamation,gok)
		
		endif
		case(6012:6021)
			if(ifitmode.eq.1) then
				
				j=callid-6011
				do i=1,nset
					jmiss(i)=1
				    iline(i)=-1
					ikset(i)=-1
					if(i.ne.j) call gmsetToggleswitch(inToggle(i+1),goff)
				enddo
				jmiss(j)=0
				iline(j)=0
				juse(1)=j
				ikset(1)=callid-6011
				jcset=j
			else
		
			j=callid-6011
		!	icurvd(iks)=callid-6011
		    idb=0
		
			do i=1,iks-1
				if(ikset(i).eq.j) idb=i
			enddo 
			if(idb.ne.0) then
				
				jmiss(idb)=1
				iline(idb)=-1
				iks=iks-1
				do i=1,iks-1
					if(ikset(i).eq.j) then
					   do l=i,iks-1
						ikset(l)=ikset(l+1)
					   enddo
					   goto 111
					endif
				enddo
111				continue
			else
				ikset(iks)=callid-6011		
				iks=iks+1
			
				jmiss(callid-6011)=0
				iline(callid-6011)=0
			endif
			inkset=ikset(1)
			call gmsetToggleswitch(inToggle(1),goff)
			endif
		case(6011)
			do i=1,nset 
				if(jmiss(i).eq.0) then
					icurvd(i)=i
					iline(icurvd(i))=0
				endif
				call gmsetToggleswitch(inToggle(i+1),goff)
			enddo
		

		case(6022)
		    if(nset.gt.1) call gmremovewindow(itoggle)
			nsfit=0
			do i=1,nset
				if(ikset(i).ne.-1) then
					nsfit=nsfit+1
				endif
			enddo
		   
			if(nsfit.lt.nset.and.nsfit.ge.1) then
				do i=1,nset
					jmiss(i)=1	!miss all
				enddo
				nmiss=nset-nsfit
				if(nmiss.gt.0) then
					do j=1,nsfit
						jmiss(ikset(j))=0		!fit set j1
						iline(ikset(j))=0
					enddo
				endif
			else		!fit all
				nmiss=0
				do i=1,nset
				if(jmiss(i).eq.0) then
					icurvd(i)=i
					iline(icurvd(i))=0
				endif
				!	jmiss(i)=0	!fit all
				!	iline(i)=0
				enddo
			endif
			nsfit=0
			do j=1,nset
				if(jmiss(j).eq.0) then
					nsfit=nsfit+1
					juse(nsfit)=j
				endif
			enddo
			if(ifitmode.eq.1) then
			if(nset.eq.1) then
				jset=1
				nsfit=1
				juse(1)=1
			else
				jset=jcset
				nsfit=1
				juse(1)=jcset
			endif
			endif
			do i=1,nset
				oldrecords(modplot)%jmiss(i)=jmiss(i)
			enddo
			call SORTI(juse,nsfit,.true.,njset)
	!	if(ifitmode.eq.2.and.sameq.eq..false.) call gmerasewidget(itoggle)
		
			super=.false.
		call fitting_mode(ifitmode,nset,niobs,njset,ndimc,xobs,yobs,w,ncalc,printed,iflag,&
			dcurve,nodata,shortprt,inkset,ncurvc,ncal,icurvc,jfix,noguess,fixset,fitted,&
			sameq,sep,nj,juse,ikset,iver,title1(1:60),nsfit,jmiss,norm1)
			if(norm.and.ifitmode.eq.1) then			!normalised to common X value
	   ncurvd=2
	   icurvd(1)=2
	   icurvd(2)=3
	   icurvw(1)=0
	   if(iw.eq.1) then
	      icurvw(2)=-1
	   else if(iw.eq.2) then
	      icurvw(2)=1
	   endif
	   icurvw(3)=-1
	   isym(1)=-3		!filled squares for indiv points
	   symsiz(1)=2.0

	   ijoin(1)=-1		!points not joined
	   isym(2)=-7		!filled circles for means
	   symsiz(2)=3.0

    	   ijoin(2)=-1		!points not joined
	   ndat(3)=1
	   isym(3)=7		!open circles for normalisation point
	   xval(1,3)=xobs(1,3)
	   yval(1,3)=yobs(1,3)
	   symsiz(3)=4.0
	   icol(3)=12		!red for norm point
    	   ijoin(3)=-1		!points not joined

	else if(norm.and.ifitmode.ne.1) then	!draw norm point
	   ns1=nsfit+1
	   ncurvd=ns1		!extra set for norm point
	   ndat(ns1)=1 		!one point only
	   icurvd(ns1)=ns1
	   xval(1,ns1)=xobs(1,ns1)
	   yval(1,ns1)=yobs(1,ns1)
	   icurvw(ns1)=-1
	   isym(ns1)=7		!open circles for normalisation point
	   symsiz(ns1)=4.0
	   icol(ns1)=12		!red for norm point
    	   ijoin(ns1)=-1		!points not joined

	endif
	if(norm1.and.ifitmode.eq.1) then
	   mode=1		!alread normalised
	   j1=1		!normalised data in yval(i,1) etc
	   call NORMDAT(j1,xval,yval,w,theta,mode,ncurvd,icurvd,ndat,icurvw,logyfit,juse,nsfit,ymax1,&
       ndv1,ndimd,niobs,njset,ndth)
	   isym(1)=-7
	   isym(2)=-3
	   
	endif
			if(icfit(modplot).eq.1) then
				!imessy=gmDisplayMessageBox('','superimpose',gquestion,Gyesno)
			
			endif
			!if(imessy.eq.gyesbutton) then 
			!	super=.true.
			!else
				do i=150,200
					icolsave(i)=0
				enddo
				super=.false.
				call draw_curves(xcal,ycal,icurvc,ncurvc,logity,logx,&
     			logy,sqrty,y0,yinf,x0,ilog,idev,&
				wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
				xmin,xmax,ymin,ymax,ncal,&
     			iline,icolsave,thick,ndc1,ndimc,jfirst,dxs,dys)
			!endif
				mcset=0
				callid=6023
				ipass=0
				goto 2
		case(6023)
			if(ifitmode.eq.2) then
				if(mcset.lt.nsfit) then
					mcset=mcset+1
					jcset=juse(mcset)
					do i=1,nsfit
						noguess(juse(i))=.true.
					enddo
					noguess(jcset)=.false.
				else
					goto 1
				endif
			if(ipass.eq.-1) then
				call intconv(jcset,cnum5)
				imes=gmdisplaymessagebox('','Now fit set # '//cnum5,ginformation,gok)
				if(sameq) then
		        imod=imodprev
				callid=6050
				goto 2
				endif
			endif
			endif

			 call equations(Main,Eqs,EqsMainPanel,Toggle2,titmod,0)
		case(6051:6080)  ! equations
			if(readrec.and.ifiltype.eq.1) then
		!	call gmPopWindow(graph1_1(modplot))
			IMOD=CALLid-6050
			goto 1
			else
			imessy=gmDisplayMessageBox('Info','No record on screen/Not available for this record',Gexclamation,gok)
			endif
					
		case(6050)
			if(readrec.and.ifiltype.eq.1) then
			if(sameq.and.ipass.eq.-1) then
			else
			    imodprev=imod
			    call gmEraseWidget(Eqs)
			endif
				ifcall=6050
				ncurvc0=ncurvc
				do i=1,20
					icurvc1(i)=icurvc(i)
					ncal(i)=ncalc
				enddo
			if(imod.le.0) imod=3	
				call select_equation(main,ITOGGLE,ittoglepanel,intoggle,itogbutton,imod,nmod,norm,titlep,ptitle,iuse,icuse,kmax,&
				ncomp,ip1,ip2,logyfit,fline,nodata,ifcall,njset,ifitmode,nsfit,iw,&
				nset,mset,t1,titmod,titw,juse,jmiss)
			!	if(ifcall.eq.785) then
			!!
			 if(ifcall.eq.6089) then
					callid=ifcall
					goto 2
				endif
			else
				imessy=gmDisplayMessageBox('Info','No record on screen/Not available for this record',Gexclamation,gok)
			endif
			goto 1	
		
		case(6086:6089) ! 
			iopt=callid-6085
			if(callid.eq.6089) then
				callid=6085
				ierase=-9
				goto 2
			endif 
        case(6085)
		    
		    if(ierase.ne.-9) call gmerasewidget(itoggle)
			ierase=0
			if(nmod.eq.-1.and.ifitmode.eq.3) then
				ip2=0			!separate nH
				call guimsg(' ','Hill slope same for all sets?',1,3,istatus)
				if(istatus.eq.6) then
					ip2=1
				else if(istatus.eq.3) then
					ip2=0
				endif
			endif
			nfix=0
			call gmActivateGraphicsFrame(graphics1_1(modplot))
			
			call get_guesses(main,ITOGGLE,itogglepanel,iopt,Xobs,Yobs,nj,juse,niobs,njset,&
			theta,ndth,thetgues,nset1,ybarp,ndimc,&
			setx,jfix,nfix,sepmax,guesdone,noguess,kfix,&
			nodata,sameq,sep,jcset,mset,ikset,iflag,callid,&
			xcal,ycal,ncal,ndc1,ncalc,idatg,pdata,intoggle,itogbutton,&
			autosep,hillang,titlep,idev,iuse,thetsav)
			if(callid.eq.6091) goto 2
        case(6090,6091) ! 1055(initial guesses)+524(790) 791=continue,790 no questions
			if(callid.eq.6091) then
				do i=1,kmax
					call valenq(idatg(i),pdata(i))
					if(theta(i).ne.pdata(i)) theta(i)=pdata(i)
				enddo
				fixset=.true.
				fitted=.true.
				dcurve=.false.
				ndisp=20
				skipq=.true.
				negpar=.false.
				do i=1,kmax
					if(theta(i).lt.0.0) negpar=.true.
				enddo
				if(negpar.or.nmod.eq.21) then
					constr=.false.
				else
					constr=.true.
				endif
			endif
			call gmEraseWidget(ITOGGLE)
		    ncurvc=ncurvc0
				do i=1,20
					icurvc(i)=icurvc1(i)
					ncal(i)=ncalc
				enddo
			call calculate_fit(Main,Xobs,Yobs,w,nj,juse,niobs,njset,iparsav,&
			theta,ndth,thetgues,nset1,ndimc,setx,jfix,nfix,sepmax,kfix,&
		    nodata,sameq,sep,jcset,mset,ikset,iflag,hillang,isepfit,autosep,&
		    titlep,ndc1,ncalc,xcal,ycal,iw,titled,ncurvc,icurvc,ncal,jmiss)
			call gmActivateGraphicsFrame(graphics1_1(modplot))
			if(icfit(modplot).eq.1.and.super) then
				do i=1,ncurvc
					iline(i)=iline(i)+1
				enddo
			endif
			ipos=ipos0
		    if(ifitmode.eq.2.and.iagain.eq.-1) goto 44
			ifitplot=ifitplot+1
			if(ifitplot.gt.25) then
				imessy=gmDisplayMessageBox('','Maximum 15 plots on screen',Gexclamation,gok)
				goto 1
			endif
		!	iplot=ifitplot
			modplot=ifitplot
			igraph=modplot
			jplot=iplot
			call graph1(ifitplot,jplot,main,ix,iy,graph1_1,GraphMainPanel1_1,graphics1_1,&
			wxmin,wxmax,wymin,wymax,ipos,gfile,plot,ixposv,iyposv)
			!	call gmcleargraphicsframe(Graphics1_1(modplot))
		
				call draw_axis(xmin,xmax,ymin,ymax,ymin1,ymax1,xtic,ytic,&
					nx1,ny1,xcross,ycross,itx,ity,tlenx,tleny,logx,logy,sqrty,doframe,&
					icol,ntx,nty,idev,thick,itype,&
            calbarx,calbary,xbeg4,ybeg4,xend4,yend4,xbeg5,ybeg5,xend5,yend5)
				call draw_text(parval,xtitle,ytitle,ztitle,title1,cnumx,cnumy,&
					cnumz,cexpx,cexpy,cexpz,newtext,numbx,numby,numbz,ntext,inumx,&
					inumy,intx,angle,sizetext,ijus,thick,rx,ry,idraw,icol,ifnt,str,dxs,dys)
				if(hdisp) then
					call draw_hist(xobs,yobs,icurvd,ncurvd,ndelt,logity,logx,&
						logy,sqrty,y0,yinf,x0,ilog,xmin,xmax,ymin,ymax,idev,ndat,ijoin,&
						icol,thick,ndv1,ndimd,xwbase,lt2)
								
                else
					call draw_data(xobs,yobs,icurvd,ncurvd,ndelt,logity,logx,logy,sqrty,&
						y0,yinf,x0,ilog,idev,ndat,ijoin,icol,isym,&
						symsiz,thick,barleng,interp,npint,Xint,Yint,nint,yp1,ypn,&
						Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw,&
									ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil)
				endif
				if(idev.eq.3) show_curve=.false.
					if(show_curve) then
					ymin20=ymin
							ymax20=ymax
							if(sqrty) ymin20=sqrt(ymin)
							if(sqrty) ymax20=sqrt(ymax)
							xt=0.5*(xmin+xmax)
							yt=ymax20+0.15*(ymax20-ymin20)
							ytc=0.07*(ymax20-ymin20)
								if(ncurvd.gt.0) call write_string('Data:',xmax+0.11*(xmax-xmin),ymax20+0.01*(ymax20-ymin20),0.,0,101,&
								3.,icol(151),dxs,dys)
								do j1=1,ncurvd
								if(iline(j1).ne.-1) then
								j=icurvd(j1)
								
								sx0=symsiz(j)*0.7*dxs
								sy0=symsiz(j)*0.7*dys
							    call intconv(j,cnum0)
								call write_string(cnum0,xmax+0.05*(xmax-xmin),ymax20-ytc,0.,0,101,&
								3.,icol(j+100),dxs,dys)
							
								call jSYMBOL(xmax+0.1*(xmax-xmin),ymax20-ytc,isym(j),sx0,sy0,icol(j+100),idev)   
								ytc=ytc+0.07*(ymax20-ymin20)
								
								endif
								enddo
								
				endif
			    call draw_lines(IDRAW,ICOL,THICK,C_THICK,NARROW,NLINE,NHLINE,NVLINE, &
				XBEG,YBEG,XEND,YEND,y0,yinf,x0,dxs,dys)
			!endif
44			if(ifitmode.eq.2) then
				iagain=-1
				do i=1,nset
					iline(i)=-1
				enddo
				iline(jcset)=0
			endif
			call draw_curves(xcal,ycal,icurvc,ncurvc,logity,logx,&
			logy,sqrty,y0,yinf,x0,ilog,idev,&
			wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
			xmin,xmax,ymin,ymax,ncal,&
			iline,icol,thick,ndc1,ndimc,jfirst,dxs,dys)
          
			nl=nblank1(parval)
				isens=1
			call store_record(iplotype,ifitplot,iplot,IXP,IYP,IPOS,xobs,yobs,w,nj,niobs,njset,nplot,nset,&
				juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
				wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
				ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
				xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,&
				oldrecords,dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,&
				parval,ifitype)
		
			if(ifitmode.eq.2.and.mcset.lt.nsfit) then
				callid=6023
				ipass=-1
				goto 2
			endif	
			if(idest.eq.1) then	!after display with fit, calc likelihood int?
				if(iparsav(1).lt.0.) then
					MESSAGE='Calculate likelihood intervals'
				else
					MESSAGE='Calculate likelihood intervals for simultaneous fit'
				endif

   			else if(idest.eq.2) then
				guesdone=.true.	!values for sep fits are in thetgues(i,j)
				autosep=.false.
				mset=1  ! vezi param 
			endif
		
	!	case(792:899)
			case(6092:6199)
		
		
			jfix(callid-6091)=1
			nfix=nfix+1
	!===============================================================================				
	! MOVE MOUSE -SELECT FRAME/MODEL
!===================================================================================		 
		case(7101:7200)   ! move
		  if(readrec) then
		    
			if(oldrecords(modplot)%IPOS.eq.0) then
				modplot=callid-7100
				igraph=modplot
				call gmActivateGraphicsFrame(graphics1_1(modplot))
				lframe=graphics1_1(modplot)
				ilog=oldrecords(modplot)%ilog
				logx=ilog.eq.1.or.ilog.eq.3.or.ilog.eq.4.or.ilog.eq.6
			logy=ilog.eq.2.or.ilog.eq.3
			logity=ilog.eq.4		!for Hill plot
			sqrty=ilog.eq.5.or.ilog.eq.6
			else
				goto 1
			endif
          
			CALL ACTENQ(CALLid,IDENT,IKEY,ISTATmove,Xac,Yac,acTEXT,nacargs,ARGS,IARGS)
			xtrue=xac
			ytrue=yac
			xmov=xtrue
			ymov=ytrue
			if(callid.le.7150) then
			if(logx) xtrue=10**(xac)
			if(logy) ytrue=10**(yac)
			if(sqrty.and.yac.ge.0.) ytrue=(yac)**2
			endif
			call valtyp(1,0)
			
			CALL REALTOCH(XTRUE,CXTRUE,11)
			CALL REALTOCH(YTRUE,CYTRUE,11)
			call gmSetStatusBarText(Status_bar1,2,CXTRUE)
			call gmSetStatusBarText(Status_bar1,3,CYTRUE)
 
			call valtyp(0,0)
			if(s_text.and.lframe.eq.graphics1_1(modplot)) then
			!	call show_boxes(xmov,ymov,mm,str,rx,ry,angle,ijus,sizetext,icol,itemp,&
			!	idraw,inewpos,dxs,dys)
			endif
		!	xmov0=xmov 
		!	ymov0=ymov
		  ENDIF		
		  case(7201:7300)	   !select
		  
			if(readrec) then
				modplot=callid-7200
				igraph=modplot
				call gmActivateGraphicsFrame(graphics1_1(modplot))
				lframe=graphics1_1(modplot)
			
			!	IND_1=nrmodel(modplot)
				jindex=ind_1
			
				CALL gmEnqWidgetInfo(graph1_1(modplot),Widget)
				ixp=widget%xpos
				iyp=widget%ypos
				if(oldrecords(modplot)%IPOS.eq.0) then
				if(d_line.or.d_hline.or.d_vline.or.d_arrow.or.&
				d_poly.or.zoom.or.d_text.or.s_text) then
				if(d_line.or.d_hline.or.d_vline.or.d_arrow.or.d_poly.or.zoom) then
				
				if(imark.ge.1.and.(xmov.eq.xmov0.and.ymov.eq.ymov0)) goto 1
				endif
				do i=1,50
					xbeg(i)=oldrecords(modplot)%lines%xbeg(i)
					ybeg(i)=oldrecords(modplot)%lines%ybeg(i)
					xend(i)=oldrecords(modplot)%lines%xend(i)
					yend(i)=oldrecords(modplot)%lines%yend(i)
			
				enddo
				nline=oldrecords(modplot)%lines%nline
				nhline=oldrecords(modplot)%lines%nhline
				nvline=oldrecords(modplot)%lines%nvline
				narrow=oldrecords(modplot)%lines%narrow
				ntext=oldrecords(modplot)%numbers%ntext
				do j=1,100
					str(j)=oldrecords(modplot)%STR(j)
					ifnt(j)=oldrecords(modplot)%attributes%IFNT(j)
					ijus(j)=oldrecords(modplot)%attributes%IJUS(j)
					sizetext(j)=oldrecords(modplot)%attributes%SIZETEXT(j)
					angle(j)=oldrecords(modplot)%attributes%angle(j)
					rx(j)=oldrecords(modplot)%attributes%rx(j)
					ry(j)=oldrecords(modplot)%attributes%ry(j)
					do k=1,4
						rxbox(k,j)=oldrecords(modplot)%attributes%rxbox(k,j)
						rybox(k,j)=oldrecords(modplot)%attributes%rybox(k,j)
					enddo
				enddo
					 
				call mouse_select(ind_1,npoint,temp,d_line, d_hline, d_vline, d_arrow, &
				d_poly, zoom,d_text, o_state,c_state, s_text,xmov,ymov,xmov0,ymov0,&
				imark,izoom,mm,itemp,inewpos,str,lframe,istate,ilink,link,delete_state,&
				move_state,models,ind_m,imove,ind_s,ind_e,mod_create,dxs,dys)
				
				oldrecords(modplot)%lines%nline=nline
				oldrecords(modplot)%lines%nhline=nhline
				oldrecords(modplot)%lines%nvline=nvline
				oldrecords(modplot)%lines%narrow=narrow
				do i=1,50
					oldrecords(modplot)%lines%xbeg(i)=xbeg(i)
					oldrecords(modplot)%lines%ybeg(i)=ybeg(i)
				oldrecords(modplot)%lines%xend(i)=xend(i)
					oldrecords(modplot)%lines%yend(i)=yend(i)
		
				enddo
				oldrecords(modplot)%numbers%ntext=ntext
				do j=1,100
					oldrecords(modplot)%STR(j)=str(j)
					oldrecords(modplot)%attributes%rx(j)=rx(j)
					oldrecords(modplot)%attributes%ry(j)=ry(j)
					do k=1,4
						oldrecords(modplot)%attributes%rxbox(k,j)=rxbox(k,j)
						oldrecords(modplot)%attributes%rybox(k,j)=rybox(k,j)
					enddo
				enddo
				if(izoom.eq.1) then
				oldrecords(modplot)%xmin=xmin
			oldrecords(modplot)%ymin=ymin
			oldrecords(modplot)%ymax=ymax
			oldrecords(modplot)%xmax=xmax
			oldrecords(modplot)%xmin1=xmin1
			oldrecords(modplot)%ymin1=ymin1
			oldrecords(modplot)%ymax1=ymax1
			oldrecords(modplot)%xmax1=xmax1
				callid=405
				goto 2
				endif
				else
				mi=0
				do m=1,5
					do l=1,4
						rxbox1(l)=rxbox(l,m)
						rybox1(l)=rybox(l,m)
					enddo
					call inside(xmov,ymov,rxbox1,rybox1,iflag)
					if(iflag.eq.1) mi=m
				enddo
				if(mi.ne.0) then
					ind_1=mi
					jindex=ind_1
				!	call write_string(oldrecords(modplot)%str(jindex),oldrecords(modplot)%attributes%rx(jindex),&
				!	oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
				!	oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
				!	oldrecords(modplot)%attributes%sizetext(jindex),12,&
				!	oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
					callid=jindex
					goto 2
				endif
				endif
				endif
		
			endif
			
			
	case(-1000)
	goto 1000	

	case(7001:7100)
		call gmEnqActionState(actlst)	
		if(actlst%status.eq.2) jopen(callid-7000)=-1 ! window closed  	
    case(8001)
		isens=0
		if(igraph.gt.0) then
								n1=1
								xmin10=xmin1
								ymin10=ymin1
								xmax10=xmax1
								ymax10=ymax1
								call store_record(iplotype,igraph,iplot,ixp,iyp,ipos,xobs,yobs,w,&
								nj,niobs,njset,nplot,nset,&
								juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
								wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
								ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
								xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,oldrecords,&
								dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,parval,ifitype)
			do J=1,nset
									
									nd1=nj(j)+20	!extra rows in case lines added in inwindv
	    
									NROWS=NJ(J)
									NCOLS=NCOL
									
		 							call format_list(ifiltype,ixd,iyd,xobs,yobs,w,nj(j),xnum,niobs,&
									njset,Main,iData_list,j,iplot,nplot,xdata,ncols,nrows,&
									stitle(j,iplot),hdisp,icallasc,n1,button6,&
									ValArray6,nset,gfile,newfile,iDataMainPanel)
									
			enddo
		endif
							
						
	case(8002)
		call equations(Main,Eqs,EqsMainPanel,Toggle2,titmod,8002)
	case(8003)
	if(open7) then
		comm='c:\windows\notepad '// pfilem
		res=systemqq(comm)
	endif

   end select
  enddo
1000 continue
if(open7) then
 	imes=gmdisplaymessagebox('','View text file',gquestion,gyesno)
	if(imes.eq.gyesbutton) then
			comm='c:\windows\notepad '// pfilem
			res=systemqq(comm)
			
	endif
  endif		 
! Close device MENU and GINO, and end program
   call gmCloseMenu
   call gCloseDevice
   call gCloseGino
  
 
if(open7) close(unit=7)
if(allocated(pixbuf)) deallocate(pixbuf)
if(allocated(xobs)) deALLOCATE(xobs,yobs,w)
if(allocated(xval)) deALLOCATE(xval,yval)
if(allocated(xvalold)) deALLOCATE(xvalold,yvalold)
if(allocated(nj)) deALLOCATE(nj,jmiss,juse,ndat,ncal)	!for normalised data
if(allocated(setx)) deALLOCATE(setx,njbase,icurvd,icurvw,icurvc)
if(allocated(isym)) deallocate(isym,symsiz,ijoin,iline)
if(allocated(xnum)) deallocate(xnum,xdata,ndatset)
	
if(allocated(theta)) deallocate(theta,theta1)
if(allocated(xcal)) deALLOCATE(xcal,ycal)
if(allocated(newrecords)) deallocate(newrecords,oldrecords)
stop

end


