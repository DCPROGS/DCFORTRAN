Program hjcfit

!This program is a combined version of HJCFIT and AUTPLOT
! PROGRAM_TYPE=-1  ALL
! PROGRAM_TYPE= 1  CVFIT
! PROGRAM_TYPE=2   AUTPLOT
! PROGRAM_TYPE=3   HCJFIT
! program_type=4 ekdist
! Modified to support 100 graphs 
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
!	11 - 19  Dealing with files 
!	21 - 29  Edit
!   31 - 39
!	Case
!	1
!	2
!	3
 USE DFLIB
	use gino_f90
	use menu_f90

	
	include '\compaq\common_files\cvfit_definitions.f90'
	include '\compaq\common_files\hjcfit_definitions.f90'
	include '\compaq\common_files\graphics_definitions.f90'

	TYPE (FILE$INFO) info

	type (GACTION) :: actlst
	type (Gwidget) :: widget
	character*11 cstring

	common/ptext/ameant(10),areat(10)
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
	
	common/text_attr/bold,italik,underline,ipow,idev

	
	COMMON/BLOCK2/castar,X1,X2,iequiv,ip1,ip2
	COMMON/BLOCK3/logyfit,norm,xnorm
	common/fix/fixratio,ir1,ir2,rval	!to fix ratio of 2 parameters
	common/potrat/jfirst,iset,kmax1	
	common/dp/discprt
	common/abt/abort
	common/pwrfunc/ybarp
	common/pixpos/ixgrid,iygrid,ixpix,iypix
	common/other/iparfirst	
	common/plot3d/ifnt3,ifnl,alfa,beta,gama,delta,&
          ijust,ijusx,ijusy,ijusz,&
          xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,numx,numy,&
          xmin3,xmax3,ymin3,ymax3,zmin3,zmax3,xtic3,ytic3,ztic3,&
          ratio,rat,radius,theta3,phi,ifram,numaxi,nwidth,npow,nplace,&
          nxstep,nystep,istyle,isup,&
          fill,inter,axese,fillbad,autplt,cross
	common/exop/iexcop,gaplo,gaphi,nskip
	COMMON/HJCBLK/Nint,tcrit,burst,chsvec,badend
	common/cube/ncube,cubedef,cubecyc,cubext	!for getrev
	COMMON/MPAR/NCYC,NSC,IM,JM
	COMMON/QPAR/NCON,IC
	common/mr3/extcyc
	common/mr/obeymr,automr
	common/mr1/isetmr
	common/ir/irate(200),jrate(200)
	common/np1/npar
	common /a/ mabel(100)		!common needed for CIRCUIT.for
    common /b/ matrix(100,100),n  !common needed for CIRCUIT.for
    COMMON/EBLK/NEQ,IE,JE,IF,JF,EFAC
	COMMON/indblk/nsetq,ieq,jeq,ifq,jfq,efacq(200) 
	common/ec/fixec50,nmod9,ec50,i50,j50,m50,prtec50,xqlo,xqhi,dcmodel	!nmod=imod0 (already in commom/model)
	common/ec2/monot,nlvar,vhold,vref,cfacd,dgamma !for ec50_hjc, qset_hjc
	common/ec3/ligname		!for qset_hjc
	
	common/ec4/nfixec50,nlvar2,ec502,i502,j502,m502,xqlo2,xqhi2	 !for 2nd fixed ec50


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
	common/QDBLK2/npar9,IQf,irate1,jrate1,nlig   !hjclik, qset_hjc only
	common/root/rootA,rootF	!hjclik,popadj,fcrqt,fmrqt,popadjc,depend,mopadjc
	common/inroot/s1A,s2A,s1F,s2F	  !for init guesses for roots; hjclik only
	common/eigsav/eigAAsav,eigFFsav
	common/rootsav/rootAsav,rootFsav		!hjclik only
	common/inroots/s1Asav,s2Asav,s1Fsav,s2Fsav !for init guesses for roots (hjclik only)
	common/detw2/tres,km,nerr	!for DETWA,DETWF, hjclik, hjcasymp, hjcexact
	common/queue/qfile	!for vplot and vhist
	common/resblk/tresd	!for hjclik only




	pfilem='hjcfit.txt'
	km=100	!in COMMON -dimension of QD, QT etc
	kAM=10	!max number of open states
	kFm=90	!max number of shut states
	nlvar=-1	!until defined
	nsims=1	!number of simulated expts fitted =1,2,...,nsim
	ireset=1	!for hjclik
	perfac=0.05d0	!maximum random perturbation
	ngpcheck=100
	excopen=.false.
	OPEN(unit=7,file=PFILEm,status='UNKNOWN',&
         access='APPEND',form='FORMATTED',carriagecontrol='FORTRAN')
	if(.not.apfile) REWIND(unit=7)
	DISCPRT=.TRUE.
	open7=.true.

	program_type=3
	if(program_type.eq.2) dfilt=dfiltq
	igraph=0
	ngraph=0
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
	jmodel=300
	indr=0
	do i=1,500
	do k=1,200
				ratcons(i)%ligant(k)=' '
				ratcons(i)%value(k)=0.
				ratcons(i)%iconc(k)=0
				ratcons(i)%titlep(k)=' '
				ratcons(i)%qij(k)=' '
	enddo
	enddo
	sdim=3.0
	do i=1,100
		nplot_on(i)=-1
	enddo
	ndth=100
	ncalc=501
	autplt=.false.
	plotcols=.true.
	idev=0
	angit=5.0
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
	!  thick(i)=1.
	!  thick0(i)=1.
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
	if(njset.gt.20) ndimc=njset
	IW=1
	if(idest.ne.2) idest=0
	ALLOCATE(xobs(Niobs,NJSET),yobs(Niobs,NJSET),w(NIOBS,NJSET))
	ALLOCATE(xval(NIOBS,NJSET),yval(NIOBS,NJSET))
	!ALLOCATE(xval1(NIOBS,NJSET),yval1(NIOBS,NJSET))
	ALLOCATE(xvalold(NIOBS,NJSET),yvalold(NIOBS,NJSET))
    ALLOCATE(nj(NJSET),jmiss(njset),juse(njset),ndat(njset),ncal(ndimc))	!for normalised data
    ALLOCATE(setx(njset),njbase(njset),icurvd(njset),icurvw(njset),icurvc(njset),icurvc1(njset))
	allocate(isym(njset),symsiz(njset),ijoin(njset),iline(njset))
	!allocate(isym1(njset),isym1(njset),syms1(njset),ijoin1(njset),iline1(njset))
	allocate(xnum(nrows,3),xdata(10,10,4,10),ndatset(200,20))
	allocate(newrecords(25),oldrecords(25),curves(25))
	allocate(theta(100),theta1(100),pdata(100),thetgues(100,njset))
	   
    ALLOCATE(xcal(ndc1,ndimc),ycal(ndc1,ndimc))
	allocate(pixbuf(24000))
! Initialise Gino, Device & Menu
	do i=1,ndimc
		ncal(i)=0
	enddo
	

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
	if(iopt.eq.2.or.iopt.eq.3) nofit=.true.
	if(iopt.eq.3) nodata=.true.
	curvonly=nofit.and.nodata

	nfix=0
	neq=0
	ncyc=0
	do i=1,200		!ktd=200
	  jfix(i)=0		!to mark fixed params
	  jfix1(i)=0	!all zero, for Simplex call
	  jcon(i)=0		!to mark constrained params
	  jmic(i)=0		!to mark micro rev params
	enddo

   call gOpenGino
   call gGuiwin
   
   call gmInitializeMenu
   call gmSetGuiGridMode(GON)
   call gmenqguigrid(ixgrid,iygrid,ixpix,iypix)
   if(ixgrid.lt.36) then
	ixng=48
	iyng=36
   else if (ixgrid.ge.36.and.ixgrid.lt.41) then
	ixng=40
	iyng=30
   else if (ixgrid.ge.41.and.ixgrid.lt.50) then
	ixng=40
	iyng=30
   else
	ixng=32
	iyng=24
   endif
   call gmdefineguigrid(ixng,iyng)
   call gsetsoftchars()
   call gsetbrokenlinemode(gon)
 
   call gSetEscapeChar('£')
   call define_colours(1,.true.)
   call main_window(program_type,Main,modelw,new_file,open_file,import_file, &
           isave_bmp,isave_wmf,export_file,print_file,exit_file,eqfit,view_record,&
		   view_data,title_record,iparameters,labels,inewtext,&
		   jtitle_record,jparameters,jlabels,jnewtext,jnumbers,jaxis,jframe,&
		   jlines,jarrows,raw_data,curves,label_x,label_y, &
		   label_z,number_x,number_y,number_z,Icon1_1,Icon1_2,Icon1_3,Icon1_4,&
		   Icon1_5,Icon1_6,Icon1_7,Icon1_8,Icon1_9,Icon1_10,Icon1_11,Icon1_12,&
		   Combo1_1,Combo1_2,Combo1_3,Combo1_4,Combo1_5,Combo1_6, &
		   toolbar1_1,toolbar1_2,toolbar1_3,Toolbar1_4,Status_bar1,&
		   new_text,new_lines,new_arrows,new_data,new_curves)
   
  
   call ACTWIN(Main)	
! Start management
   call gmManage
   call gmDefineKeyCallback(-2,-200)
   call gmDefineKeyCallback(-3,-300)
   call gmDefineKeyselectCallback(13,-40)

!	call initialize(main,initwin,initwin_Toggle3,initwin_Toggle4,initwin_Toggle5,&
!	initwin_Toggle9,initwin_Toggle10)
!call usage(main,iusage,iUsage_Toggle1,iUsage_Toggle2)
inifile='hjcfit.ini'
call welcome(main,initialw,inifile, initialw_Text5)
 iwel=-100
   !call progress_table(main)
current_child = title_record
1	continue
! Action loop
   do while (gmAction(callid) /= -1)
   call gmEnqWidgetInfo(main,Widget)
   ixmain=widget%xpos
   iymain=widget%ypos
	call gmEnqActionState(actlst)
!	if(actlst%status.eq.) iopen(
	if(actlst%key.eq.13.and.iwel.eq.-100) then
	callid=-40
	iwel=0
	goto 2
!		if (istate.ge.1.and.lframe.eq.graphics1_2(modplot)) then
!			call show_par_mechanism(modplot,xmov,ymov,models,ixp,iyp,pixbuf,show)
!		endif
	endif

2	continue
	
		
    
	select case(callid)
		case(-200)
			m_mouse=.true.
			r_mouse=.false.
		case(-300)
			r_mouse=.true.
			m_mouse=.false.
			link=.false.
			call gmSetGuiCursor(lframe,Gdefault,GDEFAULT)
			ilink=0
			if (istate.ge.1.and.lframe.eq.graphics1_2(modplot2)) then
		!	call show_par_mec(main,Data_list,ixd,iyd,PalArray6,button6,modplot,models,qfilem)
			!	call showmecpar(main,GraphMainPanel1_2,modplot,xmov,ymov,models,ixp,iyp,pixbuf,show)
			endif
		case(-20,-30)
		readini=.false.
			istatus1=gmEnqToggleSwitch(iusage_Toggle1)
			istatus2=gmEnqToggleSwitch(iusage_Toggle2)
			if(istatus1.eq.gon) then
			call gmRemoveWindow(iusage)	
			call initialize(main,initwin,initwin_Toggle5,initwin_Toggle6,initwin_Toggle7,initwin_Toggle8,&
			initwin_Toggle9,initwin_Toggle10)
			else if(istatus2.eq.gon) then
			call gmRemoveWindow(iusage)	
		
			!	call gmSetWidgetStatus(new_file, gSELECTABLE)
			!	call gmSetWidgetStatus(open_file, gSELECTABLE)
				call gmSetWidgetStatus(import_file, gSELECTABLE)
				do j=1,2
				call gmSetWidgetStatus(modelw(j), GSELECTABLE)
				enddo
				!call gmSetWidgetStatus(modelw(2), GSELECTABLE)
			
	
			endif
		case(-40,-60) 
				
			call gmRemoveWindow(initialw)
			
			call gmDefineKeyselectCallback(13,0)
			call gmDefineKeyselectCallback(13,-10)
			if(callid.eq.-40) then
			INQUIRE (FILE=iniFILE,EXIST=PRESENT,&
			ACCESS=FACCESS,FORM=FFORM,RECORDTYPE=FRTYPE,RECL=IRCL) 
			if(PRESENT) then
			 ihandle=FILE$FIRST
			 length = GETFILEINFOQQ(inifile, info, ihandle)
			 nLEN=info%length
	      
			readini=.true.
			IRECL=10240
			OPEN(UNIT=19,FILE=iniFILE,STATUS='UNKNOWN',&
            ACCESS='DIRECT',FORM='UNFORMATTED',RECL=irecl)
		    INQUIRE(IOLENGTH=LEN) iniFILE
		   ! nlen=len    
		close(unit=19)
		!readini.here
		call read_ini(inifile,pfileb,nfix,jfix,neq,IE,JE,EFAC,&
        IF,JF,jcon,IM,JM,jmic,ndisp,irecq,ans1,yloj,yhij,nrange,idiskq,&
        nfileb,kfile,nset,conc,tcrit,burst,&
        idatyp,qfile,imodold,setbad,tcbad,onechan,&
         nbad1,isbad,iebad,tresolb,ans3,ans4,nlvar,&
         fixec50,ec50,i50,j50,m50,xqlo,xqhi,kAm,kFm,&
         chsvec,ncyc,nsc,qfilem,nsim,irect,logsav,imodolds,&
         badend,excop,gaplo,gaphi,dcmod,nlen)
				imodsav=imodold		!in case imodold changed in getqd
				imod0=imodsav
				if(irecq.lt.1) irecq=1		!temp!
				do j=1,nset
					tresol(j)=tresolb(j)
					do i=1,10
						conc1(i,j)=conc(i,j) !save
					enddo
				enddo
		else
			imodsav=0
			imod0=0
			readini=.false.
			ista=gmDisplayMessageBox('','No INI file. Try again (yes) ; Continue (no)?',Gquestion,gyesno)
			
			if(ista.eq.gyesbutton) then
				call welcome(main,initialw,inifile, initialw_Text5)
				goto 1 
			endif
		endif
		else
			readini=.false.
			readmec=.false.
		endif
			call initialize(main,initwin,initwin_Toggle5,initwin_Toggle6,initwin_Toggle7,initwin_Toggle8,&
			    initwin_Toggle9,initwin_Toggle10)
			

		case(-50)
			
			CALL gmFileBROWSER(iniFILE,inipath,inidef,gmBrowseType=0)
			 IF(iniFILE.ne.' ') then
			 nl=nblank1(inipath)
			 inifile=inipath(1:nl)//'\'//inifile
			 call gmsettextsetting( initialw_Text5,inifile)
			 endif

		case(-10)
			
		
		autosim=.false.
		nofit=.false.
		nodata=.false.
		liksurf=.false.
		curvonly=.false.
			istatus13=gmEnqToggleSwitch(initwin_Toggle5)
				if(istatus13.eq.gon) discprt=.false.
			istatus3=gmEnqToggleSwitch(initwin_Toggle6)
			istatus4=gmEnqToggleSwitch(initwin_Toggle7)
			istatus5=gmEnqToggleSwitch(initwin_Toggle8)
		
			if(istatus3.eq.gon) then
				istatus9=gmEnqToggleSwitch(initwin_Toggle9)
				istatus10=gmEnqToggleSwitch(initwin_Toggle10)
				if(istatus10.eq.gon) autosim=.true.
					if(istatus10.eq.gon) then 
					autosim=.true.
					call gmSetWidgetStatus(new_file, GSELECTABLE)
				else if(istatus9.eq.gon) then
					autosim=.false.
					call gmSetWidgetStatus(open_file, GSELECTABLE)
		
				endif
			!	call gmSetWidgetStatus(new_file, GSELECTABLE)
			!	call gmSetWidgetStatus(open_file, GSELECTABLE)
	
				call gmSetWidgetStatus(modelw(1), GSELECTABLE)
				call gmSetWidgetStatus(modelw(2), GSELECTABLE)
				
			 
				call gFlushGraphics()
				im=-1
				mwtitle='Define mechanism to fit' 	
				icho=1
			else if(istatus4.eq.gon) then
				istatus9=gmEnqToggleSwitch(initwin_Toggle9)
				istatus10=gmEnqToggleSwitch(initwin_Toggle10)
				if(istatus10.eq.gon) then 
					autosim=.true.
					call gmSetWidgetStatus(new_file, GSELECTABLE)
				else if(istatus9.eq.gon) then
					autosim=.false.
					call gmSetWidgetStatus(open_file, GSELECTABLE)
		
				endif
				im=-1
				mwtitle='Define mechanism to fit' 	
			!	call gmSetWidgetStatus(open_file, GSELECTABLE)
	
				call gmSetWidgetStatus(modelw(1), GSELECTABLE)
				call gmSetWidgetStatus(modelw(2), GSELECTABLE)
				nofit=.true.
				icho=2	
			else if(istatus5.eq.gon) then
				call gmSetWidgetStatus(modelw(1), GSELECTABLE)
				call gmSetWidgetStatus(modelw(2), GSELECTABLE)
			
				mwtitle='Define mechanism to be simulated'
				im=1 
				nodata=.true.
				curvonly=.true.
				icho=3
				nofit=.true.
					
			else
			endif
		
			call gmRemoveWindow(initwin)
			call prog_bar(main,ipbar,icho,ipbar_Progress2)
			call gmDefineKeyselectCallback(13,0)
	        call gmDefineKeyselectCallback(13,-15)
			if(readini) then
				INQUIRE (FILE=qFILEm,EXIST=PRESENT)
				if(present) then
				readmec=.true.
			!	OPEN(unit=15,file=qfilem,status='UNKNOWN',access='DIRECT', form='BINARY', RECL=1)
			!	read(unit=15,rec=1) iver,nrecs,nextrec,ireclast,jstart
	
			!	irec=jstart(irecq)	!start byte for data in record #irecq
			!	read(unit=15,rec=irec) iver1,imod,mtitle,k, kA, kB, kC, kD
			!	close(unit=15)
			!	call gmSetWidgetStatus(imecform1_Toggle1, GSELECTABLE)
			call intconv(imodold,mtitle)
		
				else
					mtitle='Not present'
					readmec=.false.
			!	call gmSetWidgetStatus(imecform1_Toggle1, GunSELECTABLE)

				endif
			else
				readmec=.false.
				mtitle=' '
				qfilem=' '
			!	call gmSetWidgetStatus(imecform1_Toggle1, GunSELECTABLE)

			endif
			call mechw(main,imecform1,mtitle,qfilem,im,imecform1_Toggle1,imecform1_Toggle2,&
			imecform1_Toggle3,imecform1_Toggle4,imecform1_Toggle5,imecform1_Toggle6,readmec)
			
		case(-11)
			call gmSetWidgetStatus(initwin_Toggle9, GSELECTABLE)
			call gmSetWidgetStatus(initwin_Toggle10, GSELECTABLE)

	   
		case(-12)
			call gmSetWidgetStatus(initwin_Toggle9, GSELECTABLE)
			call gmSetWidgetStatus(initwin_Toggle10, GSELECTABLE)
	   
		case(-13)
			call gmSetWidgetStatus(initwin_Toggle9, GUNSELECTABLE)
			call gmSetWidgetStatus(initwin_Toggle10, GUNSELECTABLE)
        case(-15) 
			call gmDefineKeyCallback(13,0)

		
			istatus1=gmEnqToggleSwitch(imecform1_Toggle1)
			istatus2=gmEnqToggleSwitch(imecform1_Toggle2)
			
			istatus3=gmEnqToggleSwitch(imecform1_Toggle3)
		!	istatus4=gmEnqToggleSwitch(imecform1_Toggle4)
		!	istatus5=gmEnqToggleSwitch(imecform1_Toggle5)
		!	istatus6=gmEnqToggleSwitch(imecform1_Toggle6)
			call gmDefineKeyCallback(13,0)
	if(istatus1.eq.gon) samec=.true.
				call gmSetWidgetStatus(new_file, gSELECTABLE)
				call gmSetWidgetStatus(open_file, gSELECTABLE)
				call gmSetWidgetStatus(import_file, gSELECTABLE)
				do j=1,10
				call gmSetWidgetStatus(modelw(j), GSELECTABLE)
				enddo
		
			if(istatus3.eq.gon) then
				callid=35 !new
					call gmRemoveWindow(imecform1)
					imodsav=0
					imod0=0	
				!	call prog_bar(main,ipbar,icho,ipbar_Progress2)
				goto 2
			else if(istatus2.eq.gon) then
					callid=36  ! old
					call gmRemoveWindow(imecform1)
					imodsav=0
					imod0=0	
				!	call prog_bar(main,ipbar,icho,ipbar_Progress2)
				goto 2
            else if(istatus1.eq.gon) then !same 
				imodel=imodold
				call gmRemoveWindow(imecform1)
			!	call prog_bar(main,ipbar,icho,ipbar_Progress2)	
					igraph2=igraph2+1
						ixm=ixm+1
						iym=iym+1
						if(igraph2.gt.25) then
								imessy=gmDisplayMessageBox('','Maximum 25 models on screen',Gexclamation,gok)
								igraph2=igraph2-1
								goto 1
						endif
						ngraph2=igraph2
						

						jopen2(igraph2)=1
						modplot2=igraph2
						lframe=graphics1_2(modplot2)
							readrec=.true.
						!modplot=imodel
						cnum=text7(i)(1:3)
		                call chtoint(cnum,imod)
						
						irc=i
						mod_create=-irecq
					
						call create_model(ixm,iym,igraph2,imodel,main,form1,form1_txtarray1,&
						graph1_2,GraphMainPanel1_2,graphics1_2,&
						nxmodel,nymodel,mtype,models,mod_create,irc,qfilem,text7(i),&
						dxs,dys,ijmod,ka,kf,ic,indrat,pfilem,open7,apfile,ipbar_progress2,indwin)
						nrmodel(igraph2)=imodel
						call gmSetProgressValue(ipbar_Progress2, 20)

			endif
	!		if(istatus1.eq.gon) samec=.true.
	    case(1:5,80)! edit text
		if(readrec) then
		     jindex=callid
			icoltempo=oldrecords(modplot)%attributes%icol(jindex)
			oldrecords(modplot)%attributes%icol(jindex)=12
			if(jindex.lt.6) then
			call write_string(oldrecords(modplot)%str(jindex),oldrecords(modplot)%attributes%rx(jindex),&
			oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),&
			oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			endif
			if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
			call string_box(oldrecords(modplot)%str(jindex),oldrecords(modplot)%attributes%rx(jindex),&
			oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),&
     		oldrecords(modplot)%attributes%sizetext(jindex),&
			oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			endif
			ident1=gmCreateMDIComplexDialogueBox(main,2,2,16,3,GALL,'Text Editor')
			ident2=gmCreateTextEntry(ident1,0,0,16,2,oldrecords(modplot)%str(jindex),4096,GEDIT, &
                         gmHpos=Gleft,gmVpos=Gtop,gmExpand=GON,gmCallback=731)
			ident3=gmCreatePushButton(ident1,0,3, 16, 1,'OK',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=GTOp, gmHpos=GLEFT,gmExpand=GOFF,gmCallback=731)

		endif
		case(731)
	
		    call gmActivateGraphicsFrame(graphics1_1(modplot))
			
			oldrecords(modplot)%attributes%icol(jindex)=0
			if(jindex.lt.6) then
			call write_string(oldrecords(modplot)%str(jindex),oldrecords(modplot)%attributes%rx(jindex),&
			oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			endif
			if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
			call string_box(oldrecords(modplot)%str(jindex),oldrecords(modplot)%attributes%rx(jindex),&
			oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),oldrecords(modplot)%attributes%ijus(jindex),&
     		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),&
			imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			endif
			call gmEnqTextSetting(ident2,oldrecords(modplot)%str(jindex))
			call winrem(ident1)
			oldrecords(modplot)%attributes%icol(jindex)=icoltempo
			if(jindex.lt.6) then
			call write_string(oldrecords(modplot)%str(jindex),oldrecords(modplot)%attributes%rx(jindex),&
			oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			endif
			if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
			call string_box(oldrecords(modplot)%str(jindex),oldrecords(modplot)%attributes%rx(jindex),&
			oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),&
     		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),&
			imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			endif
		case(11)	!Simulate data
			autosim=.true.
			simulat=.false.
			nofit=.false.
			do i=1,njset
				jmiss(njset)=0
			enddo
			if(.not.(allocated(thtrue))) then
				allocate(thtrue(200))
				do i=1,200
					thtrue(i)=0.d0
				enddo
			endif
			n=1
			val(1)=1
			vtext(1)='Number of sets/concentrations'
			call value_table(Main,ivwin,n,vtext,ival,val,9511)
        case(12)	! Open file
			if(models(imodel)%indmod) call gmRemovewindow(indwin)
		    nofit=.true.
			simulat=.false.
			autosim=.false.
			nodata=.false.
			if(.not.(allocated(thtrue))) allocate(thtrue(1))
			n=1
			if(readini) then
				val(1)=nset
			else
				val(1)=1
			endif
			vtext(1)='Number of sets/concentrations'
			call value_table(Main,ivwin,n,vtext,ival,val,9511)
		case(9511)
			val(1)=gmenqvaluesetting(ival(1))
			nset=int(val(1))
			nset1=nset		!for COMMON/LCOMP/
			nset2=nset
			nset0=nset
			nset3=nset
			call gmRemoveWindow(ivwin)	
			if(autosim) then
			nmax=1
			do j=1,nset
				nfile(j)=1	!always for simulation
				tedit1(1,j)='simval.dat'
			tedit2(1,j)=' '
			tedit3(1,j)='simulated data'
			tedit4(1,j)=' '

			tval1(1,j)=0.
			tval2(1,j)=0.
			tval3(1,j)=0.
			tval4(1,j)=0.
			tval5(1,j)=0.
			tval6(1,j)=0.
			tval7(1,j)=0.
			tval8(1,j)=0.
			enddo
			call gmSetWidgetStatus(view_record, GSELECTABLE)
			call hjcfit_table(main,hjcfitform,nset,nfile,pfiles,tedit1,tedit2,&
			tedit3,tedit4,tval1,tval2,tval3,tval4,tval5,tval6,tval7,tval8,tval9,tvalt,tedit5,&
			fc,ffilt,trise,autosim)
			else
			if(readini) then
			do j=1,nset
			  nfile(j)=nfileb(j)
				title_box(j)='Set:'//char(48+j)
				do i=1,nfileb(j)
				INQUIRE (FILE=pfileb(i,j),EXIST=PRESENT) 
				if(PRESENT) then 
					text_box(i,j)=pfileb(i,j)
				else
					ians=gmdisplaymessagebox(pfileb(i,j),'is not in the path specified by ini file',&
					ginformation,gok)
					text_box(i,j)=' '
				endif
				enddo
			enddo
			else
			do i=1,nset
				title_box(i)='Set:'//char(48+i)
				nfile(i)=0
				
				title_box=' '
				do j=1,10
				text_box(j,i)=''
				enddo
			enddo
			endif
			call table_box(main,itwin,10,nset,title_box,text_box,itext_box,9511)
			! open window with sets
			!callid=9512
			!goto 2
			endif
		case(9521:9530)
			import=.false.
		!	do j=1,nset
		!	do k=1,10
		!	call gmEnqWidgetInfo(itext_box(k,j),widget)
		!	iselect=widget%select
        !   i=iselect
		!	enddo
		!	enddo
			kset=callid-9520	
		!	if(newfile.and.saveandplot) then
		!		do i=1,100
		!			icfit(i)=0
		!		enddo
		!		dfile=nwfile
		!		ddir=ndir
		!	else
			dfilt='*.scn'//char(124)//'Scan Files (SCN)'//char(124)//&
			'*.dat'//char(124)//'Data File (DAT)'//char(124)//'*.*'//char(124)//'All Files'

				do i=1,100
					icfit(i)=0
					iopen(i)=-1
					jopen(i)=-1
				enddo
				CALL gmFileBROWSER(DFILE,DDIR,DFILT,gmBrowseType=gmultipleinput)
			
		!	endif
        	
		IF(DFILE.ne.' ') then
			call gmSetListEntry(combo1_3,GSELECT,gmEntry=1)
			call file_open(MAIN,RECORDS,nplot,IFILTYPE,iplotype,iptype,DFILE,FOPEN_11,FOPEN_12,&
			APFILE,DISCPRT,njset,ncolr,nrowr,label,ftitle,nset,iwid,List7_1, &
			ftitle1, button7,button6,saveandplot)
			if(ifiltype.eq.0.and.program_type.eq.3) then
				callid=9512
				goto 2
			endif
				do i=1,njset
				jmiss(njset)=0
			enddo
			if(ifiltype.eq.5.or.ifiltype.eq.6) then
				if(ifiltype.eq.5) idatyp=1
				if(ifiltype.eq.6) idatyp=2
				n=1
				val(1)=kset
				!	vtext(1)='Add to set'
				!	call value_table(Main,ivwin,n,vtext,ival,val,9513)
				callid=9513
			
				goto 2
			endif
			if(allocated(xdata).and..not.newfile) DEALLOCATE(xdata)
		
			
			
			
			if(ifiltype.eq.3) nplot=1
			if(.not.newfile) ALLOCATE(xdata(nplot,njset,ncols,niobs)) 
			newfile=.false.
		
			if(saveandplot) then
				nentry=nplot
				istat(modplot)=2
				iopen(modplot)=2
				callid=71
			
				goto 2
			endif
			endif
		case(9513)
		
		!	val(1)=gmenqvaluesetting(ival(1))
		!	i=int(val(1))
		!	call gmRemoveWindow(ivwin)
			j=kset
			nfile(j)=nfile(j)+1
			if(nfile(j).gt.1) then
			do i=1,nfile(j)-1
				call gmenqtextsetting(itext_box(i,j),text_box(i,j))
			
				if(text_box(i,j).eq.' ') then
						nfile(j)=nfile(j)-1
				endif
			enddo
			endif
			nd=nblank1(ddir)
			pfiles(j,nfile(j))=ddir(1:nd)//'\'//dfile
		!	call gmsettextsetting(itext_box(nfile(i),i),dfile)
			call gmsettextsetting(itext_box(nfile(j),j),pfiles(j,nfile(j)))
			!actualize
			!imessy=gmDisplayMessageBox('','Continue to pool files?',Gquestion,gyesno)
			!if(imessy.eq.gyesbutton) then
			!endif
			
		case(9514) ! finish
			do j=1,nset
			do i=1,nfile(j)
				call gmenqtextsetting(itext_box(i,j),text_box(i,j))
				pfiles(i,j)=text_box(i,j)
				inquire(file=pfiles(i,j),EXIST=PRESENT)
				if(.not.present) then
					imessy=gmDisplayMessageBox('',&
			'File does not exit;pool another one',Gstop,gok)
				goto 1
				endif
			enddo
			enddo
			call gmRemoveWindow(itwin)
			
		!	call hjcfit_table(main,hjcfitform,nset,nfile,pfiles)
			call HJCDATw(main,hjcfitform,idatyp,nfile,kfile,pfiles,nval,irecs,calfacs2,&
     nintt,avamp,rms,ffilt,trise,fc,iscan,readini,ndev,idest,&
     name,idiscn,treso,tresg,nset,conc,nlig,ligname,nmax,ntot,&
     qfile2,adcfil,nfileb,pfileb,npatch,defname,samexp,&
     cjump,nsweep,tzero,tsamp,autosim,ipatch,ptype,temp,tedit1,tedit2,&
     tedit3,tedit4,tval1,tval2,tval3,tval4,tval5,tval6,tval7,tval8,&
     tval9,tvalt,tedit5)
	
			goto 1
	case(9531)
	! after reading data
		call gmsetprogressvalue(ipbar_Progress2,50)
		if(autosim) then
		nsims=1
		nsim=1
		npar=1
		nlig=nset !!!!! query for autosim -after read model -----------
		goto 1887
		 do j=1,nset
		  do i=1,nfile(j)
		  	pfiles(i,j)=tedit1(i,j)
			expdate=tedit2(i,j)
			title=tedit3(i,j)
			tapeid=tedit4(i,j)
			kt=tval1(i,j)
			npatch=tval2(i,j)
			emem=tval3(i,j)
			temperature=tval4(i,j)
			ffilt(j)=tval5(i,j)
			avamp(j)=tval6(i,j)
			rms(j)=tval7(i,j)
			calfac2=tval8(i,j)
			conc(i,j)=tval9(i,j)
			ptype(ipatch)=tedit5(i,j)
		 enddo	
		 enddo
1887		 continue		
		do j=1,nset

			do i=1,nlig
				conc1(i,j)=conc(i,j)
			enddo
		enddo
		restart=nsims.gt.1
		allocate(thetval(npar,nsim),nintval(nset,nsim),ixval(nsim),iyval(nsim),&
		izval(nsim),elmval(nsim),elmset(nset,nsim),nevals(nsim))
		else
		simulat=iscan.eq.-103.or.iscan.eq.-3001
		if(allocated(tint0)) DEALLOCATE(tint0,iampl0,iprops0,ampl0,index)
		allocate(tint0(nmax,nset),iampl0(nmax,nset),iprops0(nmax,nset),&
		ampl0(nmax,nset),index(nmax,nset))
		cjump=.false.
		nsweep=1
		allocate(kjumps0(nsweep),kjumps(nsweep))
		!call hjcdat2()
	
		CALL HJCDATw2(tint0,iampl0,ampl0,iprops0,iscan,&
        nintt,nfile,kfile,pfiles,calfacs2,nval,irecs,nmax,nset)
	   !resolution now
	   	allocate(tint(nmax,nset),ampl(nmax,nset),iprops(nmax,nset))
		nd1=nmax
		nd2=nmax
		if(simulat) then
			imessy=gmDisplayMessageBox('',&
			'Resolution already set',Gquestion,gyesno)
		
		if(imessy.eq.gyesbutton) goto 1
		endif
		do i=1,nset
		if(.not.cjump) then
			imessy=gmDisplayMessageBox('',&
			'Calculate resolution to exclude sublevels (Howe method)',Gquestion,gyesno)
			if(imessy.eq.gyesbutton) then
			treso=tresol(i)
		!	call AAMAXSUB(trise(i),treso,aamaxo)
			treso=0.001*treso		!in ms
			tresg=treso			!in ms
			aamaxg=aamaxo
			zo=1000.*treso/trise(i)		!tres in ms, trise in mus
			zg=zo
			call FALSEV1(tresg,fc,rms,avamp(i),frate)
			fratg=frato
			endif	
		endif
					
		enddo
		endif
		do i=1,nset
			tvalres1(i)=0.0
			tvalres2(i)=0.0
			if(.not.readini) tresol(i)=40.
			tvalres3(i)=tresol(i)
			tvalres4(i)=avamp(i)
			tvalres5(i)=acrit(i)
		enddo
		call gmRemoveWindow(hjcfitform)
	call resolution(main,iresoform,nset,tvalres1,tvalres2,tvalres3,tvalres4,tvalres5,&
	tvalres6,tvalres7,tvalres8,tvalres9,valres1,valres2,valres3,valres4,valres5,&
	valres6,valres7,valres8,valres9)
	case(9601:9610)
		i=callid-9600
		tvalres1(i)=gmenqvaluesetting(valres1(i))
		tvalres2(i)=gmenqvaluesetting(valres2(i))
		tvalres3(i)=gmenqvaluesetting(valres3(i))
		tvalres4(i)=gmenqvaluesetting(valres4(i))
		tvalres5(i)=gmenqvaluesetting(valres5(i))
		alo(i)=tvalres1(i)
		ahi(i)=tvalres2(i)
		avamp(i)=tvalres4(i)
		acrit(i)=tvalres5(i)
		tresol(i)=tvalres3(i)
		treso=0.001*tresol(i)		!in ms (default is in ms already)
		
		tresg=treso
		if(ffilt(i).gt.0.) then
	    call FALSEV1(treso,fc,rms,avamp(i),frato)
	    zo=1000.*treso/trise(i)		!tres in ms, trise in mus
	    aamaxo=erfs(0.88604*zo)
	    call FALSEV1(tresg,fc,rms,avamp(i),fratg)
	    zg=1000.*tresg/trise(i)		!tres in ms, trise in mus
	    aamaxg=erfs(0.88604*zg)
	    tvalres6(i)=frato
	    tvalres7(i)=aamaxo
	    tvalres8(i)=fratg
	    tvalres9(i)=aamaxg
		call gmsetvaluesetting(valres6(i),tvalres6(i))
		call gmsetvaluesetting(valres7(i),tvalres7(i))
		call gmsetvaluesetting(valres8(i),tvalres8(i))
		call gmsetvaluesetting(valres9(i),tvalres9(i))
		endif
	case(9611) ! next stage resolution
		do j=1,nset
		treso=0.001*tresol(j)
		tresg=treso
		call resolution2(tint0(1,j),tint(1,j),ampl0(1,j),ampl(1,j),&
     	 iprops0(1,j),iprops(1,j),nint(j),nintt(j),&
     	 ffilt(j),fc(j),rms(j),trise(j),nmax,nd1,index(1,j),&
     	 cjump,nsweep,kjumps0,kjumps,autosim,nsims,&
     	 sim,sres,sexp,excamp,alo(j),ahi(j),treso,tresg,acrit(j),avamp(j))
		 tresol(j)=1.e3*treso	!keep (possibly altered) res in mus for .ini
		 tresd(j)=dble(treso)*1.0d-3 	!same as tresg (but in seconds)
		  call realtoch(tresol(j),cstring,11) 
	      Icall= gmDisplayMessageBox('',&
     	  'Resolution for set '//char(48+i)//' (microseconds)='//cstring,Ginformation,GOK)
		enddo
		if(discprt) write(7,43)
43		format(/,' Resolution for HJC calculations')
		do j=1,nset
	   
        if(discprt) write(7,430) j,tresd(j)*1.d6
430	   format('   Set ',i3,': ',f10.3,' microseconds')
		enddo
	   call gmsetprogressvalue(ipbar_Progress2,60)
	   do j=1,nset
	   if(readini.and.nbad1(j).ge.1) then
		samex=.true.
		diff=abs(tresol(j)-tresolb(j))
		if(diff.gt.0.1) samex=.false.	 !0.1 microsec
		if(nfile(j).ne.nfileb(j)) samex=.false.
		do i=1,nfile(j)
		   call PARSNAME(pfiles(i,j),path1,ndev,pname,suffix,&
     		nopath,33)
		   call PARSNAME(pfileb(i,j),path1,ndev,pnameb,suffixb,&
     		nopath,33)
		   if(pname//'.'//suffix.ne.pnameb//'.'//suffixb) then
			samex=.false.
		   endif
		enddo
	 	if(.not.samex) then
	         
		  ians=gmdisplaymessagebox('set:'//char(48+j),&
		  ' The bad sections on disc are for different experiments and/or resolution',&
		  ginformation,gok)
		  stabcut(j)=.false.       	!don't remove them

		else if(samex) then
		   ians=gmdisplaymessagebox('set:'//char(48+j),&
		   ' bad sections have already been defined by stability plot for',&
			ginformation,gok)
    	   do i=1,nbad1(j)
		
			if(discprt) write(7,371) i,isbad(i,j),iebad(i,j)
371			format(5x,i3,16x,i9,10x,i9)
		   enddo
	       ians=gmdisplaymessagebox('set:'//char(48+j),&
		    ' Remove these bad sections before analysis [Y] ? ',Gquestion,gyesno)
		  
		   if(ians.eq.gyesbutton) then
			nbad(j)=nbad1(j)
			stabcut(j)=.true.
		
		      call REMBAD(tint(1,j),ampl(1,j),iprops(1,j),nint(j),&
     		nmax,nbad(j),isbad(1,j),iebad(1,j),index(1,j),.false.)
		   else
			stabcut(j)=.false.       	!don't remove them
			nbad(j)=0		!don't remove bad bits (but nbad1 preserved)
			
			if(discprt) write(7,331) j
331	    		format(/,' Set ',i3,': bad sections (from stability plot) are NOT omitted',/)
		   endif
		endif

	   else
		stabcut(j)=.false.       	!don't remove them
		nbad(j)=0		!don't remove bad bits (but nbad1 preserved)
	   endif
	   enddo
		tcfac=2.0
		if(.not.readini) then
			do j=1,10
			burst(j)=.false.
			chsvec(j)=.false.
			badend(j)=.true.
			enddo
		endif
		callid=9700
		goto 2
	case(9541:9550)
		j=callid-9540
		trise(j)=332.1/fc(j)
	case(9551:9560)
		j=callid-9550
		n=2
		val(1)=0.
		val(2)=0.
		vtext(1)='-3dB frequency for tape recorder (kHz) = '
		vtext(2)='-3dB frequency for tape recorder (kHz) = '
		call value_table(Main,ivwin,n,vtext,ival,val,9532)
				

	case(9561:9570)
		j=callid-9560
		n=1
		val(1)=0.
		vtext(1)='Rise time (microseconds) ='
		call value_table(Main,ivwin,n,vtext,ival,val,9533)
	
	case(9532)
		do i=1,2
			val(i)=gmEnqvalueSetting(ival(i))
		
		enddo
		fc1=val(1)
		fc2=val(2)
		call gmRemoveWindow(ivwin)
		 fc(j)=1.0/sqrt(1./fc(j)**2 + 1./fc1**2 + 1./fc2**2)
	     trise(j)=332.1/fc(j)		!rise time (microsec)
	case(9533)
		val(1)=gmEnqvalueSetting(ival(1))
		trise(j)=val(1)
		 fc(j)=332.1/trise(j)	!corresponding fc (-3dB)
		call gmRemoveWindow(ivwin)

	case(9515) ! reset
	case(9700)
		do j=1,nset
			vgroup1(j)=tcrit(j)
			vgroup2(j)=tcbad(1,j)!shut
			vgroup3(j)=tcbad(2,j)!open
		enddo
		call gmRemoveWindow(iresoform)
		call defgrp(main,igroup,nset,vgroup1,vgroup2,vgroup3,group_editval1,group_editval2,&
			group_editval3,group_Toggle0,group_Toggle1,group_Toggle2,group_Toggle3,&
			group_Toggle4,group_Toggle5)
	case(9701:9710)
				j=callid-9700
				istatus0=gmEnqToggleSwitch(group_Toggle0(j))
				if(istatus0.eq.gon) then
			
				burst(j)=.false.
				chsvec(j)=.false.
				badend(j)=.true.
				tcrit(j)=3.1536e10
				setbad(1,j)=.false.
				setbad(2,j)=.false.
				call gmSetWidgetStatus(group_toggle5(j), Gchecked)
				call gmSetWidgetStatus(group_editval1(j), Gselectable)
				call gmSetWidgetStatus(group_editval2(j), Gselectable)
				call gmSetWidgetStatus(group_editval3(j), Gselectable)
				call gmSetWidgetStatus(group_toggle5(j), Gunselectable)
				call gmSetWidgetStatus(group_toggle2(j), Gselectable)
				call gmSetWidgetStatus(group_toggle3(j), Gselectable)
				call gmSetWidgetStatus(group_toggle4(j), Gunselectable)
			!	call gmSetWidgetStatus(group_toggle1(j),gunchecked)
				endif
	case(9711:9720)
				j=callid-9710
				istatus1=gmEnqToggleSwitch(group_Toggle1(j))
				if(istatus1.eq.gon) then 
				
				burst(j)=.true.
			
				badend(j)=.false.
				
				setbad(1,j)=.true. 
				call gmSetWidgetStatus(group_editval1(j), Gselectable)
				call gmSetWidgetStatus(group_toggle5(j), Gselectable)
				call gmSetWidgetStatus(group_toggle4(j), Gselectable)
				call gmSetWidgetStatus(group_toggle2(j), Gunselectable)
				call gmSetWidgetStatus(group_toggle3(j), Gunselectable)
			!	call gmSetWidgetStatus(group_toggle1(j),gunchecked)
				endif
		case(9721:9730)
				j=callid-9720
				istatus2=gmEnqToggleSwitch(group_Toggle2(j))
				if(istatus2.eq.gon) then
				setbad(1,j)=.true.
				call gmSetWidgetStatus(group_editval2(j), Gselectable)
				endif
		case(9731:9740)
				j=callid-9730
				istatus3=gmEnqToggleSwitch(group_Toggle3(j))
				call gmSetWidgetStatus(group_editval3(j), Gselectable)
				if(istatus3.eq.gon) then
				setbad(2,j)=.true.
				endif
		case(9741:9750)
				j=callid-9740
				istatus4=gmEnqToggleSwitch(group_Toggle4(j))
				if(istatus4.eq.gon) then
				chsvec(j)=.true.
				endif
		case(9751:9760)
				j=callid-9750
				istatus5=gmEnqToggleSwitch(group_Toggle5(j))
				if(istatus5.eq.gon) then
				badend(j)=.true.
				endif
		case(9761)
		call gmsetprogressvalue(ipbar_Progress2,70)
			do j=1,nset
				vgroup1(j)=gmenqvaluesetting(group_editval1(j))
				vgroup2(j)=gmenqvaluesetting(group_editval2(j))
				vgroup3(j)=gmenqvaluesetting(group_editval3(j))
				tcrit(j)=vgroup1(j)
				tcbad(1,j)=vgroup2(j)
				tcbad(2,j)=vgroup3(j)
				
				if(setbad(1,j)) then
				n=0
				do i=1,nint(j)
				if(tint(i,j).ge.tcbad(1,j).and.ampl(i,j).eq.0.) then
				iprops(i,j)=IBSET(iprops(i,j),3)	!gap unusable; set bit 3 ='8'
				n=n+1
				endif
				enddo
				if(discprt) write(7,47) j,n,tcbad(1,j)
47				format(' Set ',i3,': ',i5,' shut times longer than ',g12.5,' ms were set as bad')
				ncbad(1,j)=n
				endif
				
				if(burst(j)) then
				if(discprt) write(7,84) j,tcrit(j)
84				format(/,' SET ',i3,': Critical gap length to define end of group = ',f8.1,&
				' milliseconds',/,'  (defined so that all openings in a group prob come from same channel)',/)
				endif
				
				if(chsvec(j)) then
      			if(discprt) write(7,431)
431				format(' Initial and final vectors for bursts calculated as in C.,',/,&
				' Hawkes & Srodzinski, (1996, eqs 5.8, 5.11).',/)
				else
				if(discprt) write(7,432)
432				format(/,' Initial and final vectors for are calculated as for   ',/,&
				' steady state openings and shuttings (this involves a slight',/,&
				' approximation at start and end of bursts that are defined',/,&
				' by shut times that have been set as bad).',/)
				endif
				
				if(badend(j)) then
				if(discprt) write(7,4321)
4321			format(' A bad gap ends a group, but does not eliminate the whole group',/)
  				else
				if(discprt) write(7,4322)
4322			format(' A bad gap results in elimination of the whole group in which it occurs',/)
				endif
				
				if(setbad(2,j)) then
				n=0
				do i=1,nint(j)
				if(tint(i,j).ge.tcbad(2,j).and.ampl(i,j).ne.0.) then
				iprops(i,j)=IBSET(iprops(i,j),3)	!gap unusable; set bit 3 ='8'
				n=n+1
	   			endif
				enddo
				if(discprt) write(7,471) j,n,tcbad(2,j)
471				format(' Set ',i3,': ',i5,' open times longer than ',g12.5,' ms were set as bad')
				ncbad(2,j)=n
				endif

			enddo
			iexpr=0
			if(excopen.and.(.not.autosim)) then	 !excopen set under advanced options now
			ians=gmdisplaymessagebox('','Exclude any open times',gexclamation,gyesno)
			if(ians.eq.gnobutton) then
				do j=1,nset
					iexcop(j)=0
				enddo
				iexpr=-1
				callid=9765
				goto 2
			else 
			do j=1,nset
				vgroup1(j)=gaplo(j)
				vgroup2(j)=gaphi(j)
			enddo
			call exclude_open(main,nset,vigroup1,vigroup2,group_editval1,group_editval2,&
			group_Toggle0,group_Toggle1)
			endif
			else
				iexpr=-1
				callid=9765
				goto 2
			endif
			
		
	case(9771:9780)
				j=callid-9770
				 iexcop(j)=1
				istatus0=gmEnqToggleSwitch(igroup_Toggle0(j))
				if(istatus0.eq.gon) then
			
			
				call gmSetWidgetStatus(igroup_editval1(j), Gselectable)
				call gmSetWidgetStatus(igroup_editval2(j), Gselectable)
			!	call gmSetWidgetStatus(group_toggle1(j),gunchecked)
				endif
	case(9781:9790)
				j=callid-9780
				 iexcop(j)=2
				istatus2=gmEnqToggleSwitch(igroup_Toggle1(j))
				if(istatus2.eq.gon) then
			
				call gmSetWidgetStatus(igroup_editval1(j), Gselectable)
				call gmSetWidgetStatus(igroup_editval2(j), Gselectable)
			!	if(burst(j) then
				gaplo(j)=tcrit(j)
			    gaphi(j)=3.1536e10
				vigroup1(j)=gaplo(j)
				vigroup2(j)=gaphi(j)
				call gmsetvaluesetting(igroup_editval1(j),vigroup1(j))
				call gmsetvaluesetting(igroup_editval2(j),vigroup2(j))
				if(discprt) write(7,134) j,gaplo(j)
134				format(/,' SET ',i3,/,' Open period excluded if shut time on BOTH sides is above',/,&
				'  tcrit = ',g13.6,' ms, so all isolated openings excluded.')
			!	endif	
				!	call gmSetWidgetStatus(group_toggle1(j),gunchecked)
				endif
	
	case(9765)
			if(iexpr.ne.-1) then
			do j=1,nset
				vigroup1(j)=gmenqvaluesetting(igroup_editval1(j))
				vigroup2(j)=gmenqvaluesetting(igroup_editval2(j))
				gaplo(j)=vigroup1(j)
				gaphi(j)=vigroup2(j)
				if(gaphi(j).lt.gaplo(j)) then
					a1=gaphi(j)		!swap
					gaphi(j)=gaplo(j)
					gaplo(j)=a1
	      		endif
	            if(discprt.and.iexcop(j).eq.1) write(7,131) j,gaplo(j),gaphi(j)
131			format(/,' SET ',i3,/,' Open period excluded if shut time on BOTH sides is',/,&
         '  between ',g13.6,' and ',g13.6,' ms')
			enddo
			endif
			if(.not.nodata) then
				do jset=1,nset
				call GETOPER(jset,tint,ampl,iprops,nint,nd1,nd2)
				enddo
			endif
			if(discprt) write(7,138)
138			format(' ------------------------------------------------------------')
			do j=1,nset
			if(discprt) write(7,741) j
741			format(' Set ',i3)
				nlig=models(imodel)%nlig
				do jk=1,nlig
					ligname(jk)=models(imodel)%ligname(jk)
				enddo
			do i=1,nlig
			if(discprt) write(7,761) ligname(i),conc(i,j)*1.e6
761			format('  Concentration of ',a10,' (micromolar) = ',g13.6)
			enddo
			enddo
				      
		

			ncon=models(imodel)%ncon
			indmod=models(imodel)%indmod
			k=models(imodel)%n
			if(indmod) then
				npar=models(imodel)%npar
			else
				npar=2*ncon
			endif
			nrateq=2*ncon
				kj=1	
			do m=1,ncon
				ic(1,m)=models(imodel)%ic(1,m)
				ic(2,m)=models(imodel)%ic(2,m)
				i=ic(1,m)
				j=ic(2,m)
				irate(kj)=i
				jrate(kj)=j
				irate(kj+1)=j
				jrate(kj+1)=i
				kj=kj+2
			enddo
			i1=0
			nrateq=2*ncon
			do m=1,npar
				i=irate(m)
				j=jrate(m)
				qt(i,j) = ratcons(indrat)%qt(i,j)
				i1=i1+1
				iQ(i,j)=i1
				i1=i1+1
				IQ(j,i)=i1
				theta0(m)=qt(i,j)
				thsav(m)=theta0(m)
			enddo
			do i=1,nrateq
			
			if(i.le.100) qt(irate(i),jrate(i))=ratcons(indrat)%qt(i,j)
			enddo
			if(indmod) then
			do i=1,k
			do j=1,k
				qt(i,j)=ratcons(indrat)%qt(i,j)
			enddo
			enddo
			endif
			do i=1,200
				jmic(i)=0
			enddo
			ncyc=ratcons(indrat)%ncyc
			do i=1,ncyc
				nsc(i)=ratcons(indrat)%nsc(i)
			enddo
			if(ncyc.gt.0) then
				do i=1,ncyc
				
					if(obeymr(i).and.(.not.automr(i))) then
					!	m=IQ(im(i,1),jm(i,1))
					!	jmic(m)=1
						nmr=nmr+1
					endif
				enddo
			endif
			if(discprt) write(7,138)
			if(nlig.gt.0) then
				if(discprt) write(7,332)
332				format(' Q matrix')
				call atypd(qt,'  QT   ',k,k,km,km)
			endif
			do j=1,3
				call gmSetWidgetStatus(eqfit(j), GSELECTABLE)

			enddo
			call gmSetWidgetStatus(view_record, GSELECTABLE)

			kAm=kA
			kFm=kF
			if(.not.allocated(Z00A)) then
				ALLOCATE(Z00A(kAm,kFm,km),Z10A(kAm,kFm,km),Z11A(kAm,kFm,km))
				ALLOCATE(Z00F(kFm,kAm,km),Z10F(kFm,kAm,km),Z11F(kFm,kAm,km))
				ALLOCATE(XAF(kAm,kFm,kAm),XFA(kFm,kAm,kFm),QEXPQA(kFm,kAM),QEXPQF(kAm,kFm))
			endif
			call gmremovewindow(igroup)
			imys=gmdisplaymessagebox('','Now start fitting (Menu bar:Fit)',&
			ginformation,gok)
	case(701) ! constrains
	!	imess=gmdisplaymessagebox('','Have you set the MR?',gquestion,gyesno)
	!	if(imess.eq.gnobutton) goto 1
		npar=models(imodel)%npar
		ncyc=ratcons(indrat)%ncyc
		indk=indrat
		do i=1,ncyc	
		nsc(i)=ratcons(indk)%nsc(i)
		enddo
		do l=1,ncyc
		do m=1,nsc(l)-1
		im(l,m)=ratcons(indk)%icyc(l,m)
		jm(l,m)=ratcons(indk)%icyc(l,m+1)
		enddo
		im(l,nsc(l))=ratcons(indk)%icyc(l,nsc(l))
		jm(l,nsc(l))=ratcons(indk)%icyc(l,1)
		enddo
		if(indmod.and.readini.and.imod0.eq.imodsav) then
			if(neq.gt.0) then
			! show ie,je,etc
			endif	!end of neq.ne.0
		else		!not readini
			neq=0
			do i=1,npar
			jcon(i)=0
			efac(i)=0.0
			enddo
		endif
			neq1=0
		do i=1,neq
			if(efac(i).lt.0.) neq1=neq1+1	!default
		enddo
		do i=1,km		!km=100 at present
			jcon(i)=0		!in case old jcon read from disc
		enddo
		nlig=models(imodel)%nlig
		ncols=5
		celltitle='Rates available for constraints:'
		ctitle1(1)='Rate 1'
		ctitle1(2)='Constraint'
		ctitle1(3)='Factor x/ Total -'
		ctitle1(4)='Rate 2'
		ctitle1(5)='Observations'
		if(nlig.gt.0) then
			ctitle1(3)='Factor x/ Total -/ EC50'
			ctitle1(4)='Rate 2/ Ligand(ec50)'
			ctitle1(5)='xlo (ec50)'
			ctitle1(6)='xhi (ec50)'
			ctitle1(7)='Observations'
			ncols=7
		endif
		nrows=0
		do ir=1,models(imodel)%npar
			do m=1,ncyc
			!	if(obeymr(m)) then
					if(irate(ir).eq.im(m,1).and.jrate(ir).eq.jm(m,1)) then
					ifixmr(ir)=-1 !mc
			!	endif
				endif	
			enddo
	!		if(irate(ir).eq.i50.and.jrate(ir).eq.j50) ifixmr(i)=-2
	!		if(irate(ir).eq.i502.and.jrate(ir).eq.j502) ifixmr(i)=-2
		enddo
		do i=1,npar
			if(ifixmr(i).ne.-1) then
		
				nrows=nrows+1
				irate2(nrows)=irate(i)
				jrate2(nrows)=jrate(i)
				textcell1(1,nrows)=ratcons(indrat)%qij(i)
				textcell1(2,nrows)='None'
				do j=1,neq0
				if(ie(j).eq.irate(i).and.je(j).eq.jrate(i)) textcell1(2,nrows)='Factor *'
				enddo
				do j=1,neq1
				if(ie(j+neq0).eq.irate(i).and.je(j+neq0).eq.jrate(i)) textcell1(2,nrows)='Total -'
				enddo
				if(i50.eq.irate(i).and.j50.eq.jrate(i)) textcell1(2,nrows)='Ec50'
				if(i502.eq.irate(i).and.j502.eq.jrate(i)) textcell1(2,nrows)='Ec50'
			
			endif
			
		enddo
		ncombo=1
		nlig=models(imodel)%nlig
		do i=1,nlig
			ligname(i)=	models(imodel)%ligname(i)
		enddo
		ncomboname(1)=3	
		comboname(1,1)='None'
		comboname(1,2)='Multiplicative'
		comboname(1,3)='Additive'
		if(nlig.gt.0) then
		ncomboname(1)=4
			comboname(1,4)='EC50'
		endif
	
	
		do i=1,7
			iwcell(i)=120
		enddo
		if(nlig.gt.0) then
		iwcell(3)=180
		iwcell(4)=150
		endif
		do i=1,7
			icombo(i)=.false.
		enddo
		
		icombo(2)=.true.
		call cellarray(main,icell,icellarray,celltitle,nrows,ncols,ctitle1,textcell1,&
		iwcell,ncombo,comboname,ncomboname,icombo,734,1)
		!n=1
		!val(1)=neq
		!vtext(1)='Nr of rate constants to constrain'
		!call value_table(Main,ivwin,n,vtext,ival,val,704)
    case(734)
		neq0=0
		neq1=0
		iwr1=0
		do i=1,nrows
			call gmEnqCellSetting(icellarray(1), 2,i ,R1 ,textcell1(2,i))
			if(textcell1(2,i).eq.'Multiplicative') then
				
				neq0=neq0+1
				call gmEnqCellSetting(icellarray(1), 3,i ,R1 ,textcell1(3,i))
				call gmEnqCellSetting(icellarray(1), 4,i ,R1 ,textcell1(4,i))
				call chtoreal(textcell1(3,i),efac(neq0))
				ie(neq0)=irate2(i)
				je(neq0)=jrate2(i)
				nl1=nblank1(textcell1(4,i))
				cnum51=textcell1(4,i)(1:nl1)
				call chatonos(cnum51,num0,nk)
				if(neq0)=num0(1)
				jf(neq0)=num0(2)
				i1=if(neq0)
				j1=jf(neq0)
			endif
		enddo
		do i=1,nrows
			call gmEnqCellSetting(icellarray(1), 2,i ,R1 ,textcell1(2,i))
			if(textcell1(2,i).eq.'Additive') then
				neq1=neq1+1
				call gmEnqCellSetting(icellarray(1), 4,i ,R1 ,textcell1(4,i))
				call chtoreal(textcell1(3,i),efac(neq0+neq1))
				ie(neq0+neq1)=irate2(i)
				je(neq0+neq1)=jrate2(i)
				nl1=nblank1(textcell1(4,i))
				cnum51=textcell1(4,i)(1:nl1)
				call chatonos(cnum51,num0,nk)
				if(neq0+neq1)=num0(1)
				jf(neq0+neq1)=num0(2)
				i1=if(neq1+neq0)
				j1=jf(neq1+neq0)
				if(i1.ne.0.and.j1.ne.0) then
				m1=IQ(i1,j1)
				if(m1.gt.0) then
					if (efac(neq1+neq0).lt.theta0(m1)) then
						text_box2(7,i)='wrong value'
						iwr1=-1
					endif
				endif
				endif
				efac(neq1+neq0)=-efac(neq1+neq0)
			endif
		enddo
		neq=neq0+neq1
		nlig2=0
		do i=1,nrows
			call gmEnqCellSetting(icellarray(1), 2,i ,R1 ,textcell1(2,i))
			if(textcell1(2,i).eq.'EC50') then
				call gmEnqCellSetting(icellarray(1), 4,i ,R1 ,textcell1(4,i))
				call chtoint(textcell1(4,1),ilig)
				call gmEnqCellSetting(icellarray(1), 3,i ,R1 ,textcell1(3,i))
				call chtoreal(textcell1(3,i),xs)
			
				nlig2=nlig2+1
				if(nlig2.eq.1) then
					i50=irate2(i)
					j50=jrate2(i)
					ec50=dble(xs*1.e-6)
				else if(nlig2.eq.2) then
					i502=irate2(i)
					j502=jrate2(i)
					ec502=dble(xs*1.e-6)
				endif
				nfixec50=nlig2
			endif
		enddo
		if(iwr1.eq.-1) then
			call gmdisplaymessagebox('','Wrong values.Try again!',gstop,gok)
		
		else
			n=1
				ncon=models(imodel)%ncon
				!val(1)=ncon-nmc-nac-nec50 ! nmc=set by mult const,nac=additive constraint
				nc=neq0+neq1+nfixec50
				do l=1,neq0
					icspec(1,l)=ie(l)
					icspec(2,l)=je(l)
				enddo
				do l=neq0+1,neq
					icspec(1,l)=ie(l)
					icspec(2,l)=je(l)
				enddo
		
				if(nfixec50.eq.1) then
					l=l+1
					icspec(1,l)=i50
					icspec(2,l)=j50
				endif
				if(nfixec50.eq.2) then
					l=l+1
					icspec(1,l)=i50
					icspec(2,l)=j50
				endif
			call gmremovewindow(icell)
			call gmdisplaymessagebox('','OK.Proceed with MR/Fitting',ginformation,gok)
		
		endif

	
	 case(1001) !MR
	
		if(indmod.eq..false.) then
			nmr=models(imodel)%npar-models(imodel)%n+1
			if(nmr.gt.0) then
			useprim=.true.
			do ir=1,models(imodel)%npar
			do m=1,ncyc
			!	if(obeymr(m)) then
					if(irate(ir).eq.im(m,1).and.jrate(ir).eq.jm(m,1)) then
					ifixmr(ir)=-1 !mc
			!	endif
				endif	
			enddo

			enddo
   			
				call intconv(nc,cnum5)
				imessc=gmdisplaymessagebox('Number of routes:',&
				cnum5//' rates already constraint.Include/Exclude more?',gquestion,gyesno)
				if(imessc.eq.gyesbutton) then
					celltitle='Routes to be included/excluded in spaning tree'
					nrows=npar-nc
					ncols=3
					ctitle1(1)='Rate'
					ctitle1(2)='Option'
					ctitle1(3)='Observations'
					ncombo=1
					ncomboname(1)=3
					comboname(1,1)='None'
					comboname(1,2)='S-Tree'
					comboname(1,3)='MR'

					icombo(2)=.true.
					do ir=1,npar
					do j=1,nc
					if(irate(ir).eq.icspec(1,j).and.jrate(ir).eq.icspec(2,j)) ifixmr(ir)=-2 ! mc+ec50
					enddo
					enddo
					nrows=0
					do ir=1,npar
						if(ifixmr(ir).ne.-2) then
							nrows=nrows+1
							textcell2(1,nrows)=ratcons(indrat)%qij(ir)
							textcell2(2,nrows)='None'
							if(ifixmr(ir).eq.-1) textcell2(2,nrows)='MR'
							irate2(nrows)=irate(ir)
							jrate2(nrows)=jrate(ir)
						endif
					enddo
					ipso=-1		
					call cellarray(main,icell,icellarray,celltitle,nrows,ncols,ctitle1,textcell2,&
					iwcell,ncombo,comboname,ncomboname,icombo,1003,2)
	
				else
					ipso=0
					callid=1002
					goto 2
				endif
			!	val(1)=0
			!	vtext(1)='Extra routes Not to be set by MR '
			!	call value_table(Main,ivwin,n,vtext,ival,val,1002)
			endif
	else
		imymy=gmdisplaymessagebox('','No MR for this model',ginformation,gok)
	endif
	case(1003)
		nc0=0
		ncout=0
		do i=1,nrows
			call gmEnqCellSetting(icellarray(2), 2,i ,R1 ,textcell2(2,i))
			if(textcell2(2,i).eq.'S-Tree') then
				nc0=nc0+1
				icspec(1,nc0+nc)=irate2(i)
				icspec(2,nc0+nc)=jrate2(i)
			else if (textcell2(2,i).eq.'MR') then
				ncout=ncout+1
				ICout(1,i)=irate2(i)
				ICout(2,i)=jrate2(i)
			endif
		enddo
	
		nc=nc+nc0
		callid=1002
		goto 2
	case(1002) ! MR
		ndim=100
		is1=1
		
		imod=imodel
		k=models(imod)%n
		do i=1,2
		do j=1,100
		ict(i,j)=0
		enddo
		enddo
		
		if(nc.gt.0) then
		
			nd1=200
			nd2=100
			call IC_JCON(ICspec,ncon,nd1,Jtree,k,nd2)
			call NCONS(Jtree,k,ncon1,nstate,ndim)
	   
			write(7,5) nstate,ncon1
5			format(' Specified routes have ',i3,' states, ',i3,' connections')
			if(ncon1.gt.nstate-1) then
				write(7,161)
161				format(' These routes contain cycle(s): invalid, try again')
				goto 1
			endif
			ncmax=k	!max cycle size to be found
			call CYCQ1(k,nc,icspec,ncyc3,nsc0,im1,jm1,ncmax)
			if(ncyc3.ne.0) then
				write(7,16) ncyc
16				format(' From CYCQ1, these routes contain ',i3,' cycle(s): invalid, try again')
				call gmdisplaymessagebox('Error','This routes contain cycles;try again',gstop,gok)
				goto 1
			else
				if(ipso.eq.-1) then
				call gmremovewindow(icell)
					ipso=0
				endif
				
			endif
		else
				if(ipso.eq.-1) then
					call gmremovewindow(icell)
					ipso=0
				endif
			
	
		endif
			is1=1
			callid=1004
		goto 2
!=====================
case(1004)
		call PRIM3(models(imodel)%link,ICspec,nc,ICout,ncout,Iedge,nedge,&
		models(imodel)%n,ndim)
		write(7,121)
121		format(/,' Edges in spanning tree')
		do j=1,nedge
	  		if(discprt) write(7,110) j,iedge(1,j),iedge(2,j)
110			format(' ',i3,': ',i3,' - ',i3)
		enddo
		ncon=nedge

		nd1=200
		nd2=100
		ncon=nedge
		call IC_JCON(Iedge,ncon,nd1,Jtree,k,nd2)
		write(7,24)
24		format(/,/)
		write(7,12)
12		format(' Connections for spanning tree ')
		do i=1,k
			if(discprt) write(7,19) (jtree(i,j),j=1,k)
		enddo
19		format(20i3)
		nmr=0
		ncont=0	!ncon for tree connections
! initialise ic() array for tree
		do i=1,2
			do j=1,100
				ict(i,j)=0
			enddo
		enddo

		do i=2,k	!check lower triangle only
		do j=1,i-1
		if(jtree(i,j).eq.1) then
		   ncont=ncont+1
		   ict(1,ncont)=i
		   ict(2,ncont)=j
		endif
		enddo
		enddo

		nd1=200
		nd2=100
		call NCONS(Jtree,k,ncon1,nstate,ndim)
		write(7,51) nstate,ncon1
51		format(' Final tree has ',i3,' states, ',i3,' connections')
		ncmax=k	!max cycle size to be found
		call CYCQ1(k,ncont,ict,ncyc3,nsc0,im1,jm1,ncmax)
	
		write(7,21) ncyc3
21		format(' Number of cycles (from cycq) in spanning tree = ',i3)
	
		write(7,33)
33		format(/,' Links missing in tree (=MR routes)',/)
		l=1
		do i=2,k	!check lower triangle only
		do j=1,i-1
		if(models(imodel)%link(i,j).eq.1.and.jtree(i,j).eq.0) then
		   nmr=nmr+1
		   
		   write(7,4) nmr,i,j
4		   format(/,' MR route ',i3,': i = ',i3,'   j = ',i3)
		   call intconv(i,cnum51)
		   text_box(l,1)=cnum51
		   call intconv(j,cnum51)
		   nt=nblank1(text_box(l,1))
		   text_box(l,1)=text_box(l,1)(1:nt)//','//cnum51
		   
		   ncont1=ncont+1
		   ict(1,ncont1)=i
		   ict(2,ncont1)=j
		   call CYCQ1(k,ncont1,ict,ncyc3,nsc3,im3,jm3,ncmax)
		   do n=1,ncyc3
	      	
			if(discprt) write(7,1311)im3(n,1),jm3(n,1)
1311		format(2i3,'  (calc by micro rev)')
			
			if(discprt) write(7,128)(im3(n,m),jm3(n,m),m=2,nsc3(n))
128			format(2(5(2i3,4x)))
			cnum5=' '
			cnum51=' '
			if(ncyc3.gt.0) then
				call intconv(im3(n,1),cnum5)
				text_box(l,3)=cnum5
			
				call intconv(im3(n,2),cnum51)
				nt5=nblank1(text_box(l,3))
				text_box(l,3)=text_box(l,3)(1:nt5)//','//cnum51
				text_box(l,2)=text_box(l,3)
				ratcons(indrat)%nsc(l)=nsc3(n)
				ratcons(indrat)%icyc(l,1)=im3(n,1)
				ratcons(indrat)%icyc(l,2)=im3(n,2)
		
				do m=3,nsc3(n)
					cnum5=' '
					call intconv(im3(n,m),cnum5)
					nt=nblank1(text_box(l,2))
					text_box(l,2)=text_box(l,2)(1:nt)//','//cnum5
				enddo
				l=l+1
			endif
		   enddo
		   call CIRCUIT(i,j,incirc,ncirc)
		   
		   write(7,41) (incirc(n),n=1,ncirc)
41		   format(' MR circuit: ',10i4)
		endif
		enddo
		enddo

		ntot=nmr
		n=l-1
		m=3
		imr1=-1
		call cyc_array(imodel,main,itwin1,n,m,text_box,itext_box,-234,&
		icyc_form,icmr)
		
	case(-234)
		call gmremovewindow(itwin1)
			imess=gmdisplaymessagebox('','Now start fitting',ginformation,gok)
	
	case(703) ! start fitting
	imess=gmdisplaymessagebox('','Have you set the constraints?',gquestion,gyesno)
		if(imess.eq.gnobutton) goto 1
		penfac=10.0d0
		xs=sngl(assmax)
		fac=sngl(penfac)
		call penaly(main,ipeny,ipeny_yes,ipeny_no,ipeny_xs,xs,ipeny_fac,fac)
	case(710)
		istat1=gmEnqToggleSwitch(ipeny_yes)
		istat2=gmEnqToggleSwitch(ipeny_no)
			if(istat1.eq.gon) then
				dcmodel=.true.
			else
				dcmodel=.false.
			endif
			fac=gmenqvaluesetting(ipeny_xs)
			xs=gmenqvaluesetting(ipeny_fac)
			assmax=dble(xs)
			penfac=dble(fac)
			call gmremovewindow(ipeny)
			do ir=1,npar
				if(irate(ir).eq.i50.and.jrate(ir).eq.j50) ratcons(indrat)%micro(ir)='EC50 '
				if(irate(ir).eq.i502.and.jrate(ir).eq.j502) ratcons(indrat)%micro(ir)='EC50 '
				do i=1,neq0
				if(irate(ir).eq.ie(i).and.jrate(ir).eq.je(i)) ratcons(indrat)%micro(ir)='Multiplicative'
				enddo
				do i=1,neq1
					if(irate(ir).eq.ie(i).and.jrate(ir).eq.je(i)) ratcons(indrat)%micro(ir)='Additive'
			
				enddo
				do i=1,ncyc
					
					if(irate(ir).eq.im(i,1).and.jrate(ir).eq.im(i,2)) ratcons(indrat)%micro(ir)='MR'
				
				enddo
			
			enddo
			! here getqd2

			do i=1,100		!ditto
			do j=1,100
				IQ(i,j)=0
			enddo
			enddo
			do m=1,npar
				i=irate(m)
				j=jrate(m)
				IQ(i,j)=m
			enddo
			jset_1=1

			imode=1
			icons=0				!apply constraints (if any)
			if(icon.eq.3) icons=1		!don't apply constraints to fitted rates
			k=models(imodel)%n
			call QSET_TRU(QT,theta0,jset_1,conc,npar,irate,jrate,IQ,imode,icons,k)
			do j=1,npar
			!	call gmSetCellSetting(Form1_TxtArray1(imodel,2), 6,j ,&
			!	gmString=ratcons(indrat)%micro(j))
				
			!	xtrue =theta0(j)
			!	if (xtrue.lt.10e-5.or.xtrue.gt.10e5) then
			!		arrayattribs%format=gscientific
			!	else
			!		arrayattribs%format=0
			!	endif
			!	arrayattribs%ndp=5
				!call gmSetCellAttribs(Form1_TxtArray1(imodel,2), 7,j, arrayattribs)
			!	call gmSetCellSetting(Form1_TxtArray1(imodel,2), 7,j ,gmvalue=xtrue)
			!	call gFlushGraphics() 
			enddo
			isw=3
			ncols=5
			nrows=npar
			xtext2='i,j '//char(124)//'Rate name'//char(124)//'Value'//char(124)//'Constraint'//char(124)//'Fix'
			itcall=704
			do i=1,7
			iwcell(i)=100

			enddo
			ihcell=24
			call text_array(imodel,Main,form1,Form1_TxtArray1,ncols,nrows,iwcell,&
			ihcell,xtext2,isw,&
			models(imodel)%name_link,ratcons(indrat)%iconc,ratcons(indrat)%qij,&
			theta0,models(imodel)%title_model,ratcons(indrat)%titlep,NBOUND,ligname,nlig,&
			ratcons(indrat)%micro,ratcons(indrat)%ligant)
			
	case(704)
	   do m=1,npar
			call gmEnqCellSetting(Form1_TxtArray1(imodel,3), 3,m ,xs,textcell)
			theta0(m)=dble(xs)
		    QT(irate(m),jrate(m))=theta0(m)
			thsav(m)=theta0(m)  !re-save initial guesses in case altered in getqd2
			call gmEnqCellSetting(Form1_TxtArray1(imodel,3), 5,m ,r1,textcell)
			if(textcell.eq.'yes') then
				nfix=nfix+1
				jfix(m)=1
			endif
	   enddo
	   kfit=npar-nfix-neq-nmr	!nmr=# of cycles constrained by mr (.le.ncyc)
	   if(fixec50) kfit=kfit-nfixec50
	   ik=0		!use to check vs kfit
	   do m=1,npar
			if(jfix(m).eq.0.and.jcon(m).eq.0.and.jmic(m).eq.0) then
			if((.not.(fixec50.and.m.eq.m50)).and.(.not.(nfixec50.eq.2.and.m.eq.m502))) then
				ik=ik+1
				i=irate(m)
				j=jrate(m)
				irate1(ik)=i
				jrate1(ik)=j
				icdep(ik)=0		!numbering for FITTED params
				do n=1,ncdep
					if((ix(n).eq.i.and.jx(n).eq.j)) then
						icdep(ik)=1
					endif
				enddo
				IQf(i,j)=m
				theta(ik)=QT(i,j)
			endif
			endif
		enddo
		val(1)=npar
		val(2)=nfix
		val(3)=neq
		val(4)=nmr
		val(5)=nfixec50
		val(6)=kfit
		vtext(1)='Total number of rates = '
        vtext(2)='Number that are fixed       = '
        vtext(3)='Number that are constrained = '
        vtext(4)='Number set by micro rev     = '
        vtext(5)='Number set by fixed EC50    = '
        vtext(6)='Number of free rates to be estimated = '

		call value_table(Main,ivwin,6,vtext,ival,val,705)
    case(705)
		!write to ini file
	case(706)
		
	case(708)
		 first=.true.		!get openings/group etc in HJCLIK
		 oneset=.false.	!
		 prtec50=.true.		!print initial ec50 (whether fixec50 or not)


   		 iasymp = gmCreateMDIComplexDialogueBox(Main,2 , 1, 24,20 , GALL, ' ', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052')
		 iasympPanel=gmCreatePanel(iasymp, 0, 0, 24, 20, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmtitle=title,gmExpand=GOff, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
		 ibasymp=gmCreatePushButton(iasymppanel,22,1, 2, 1,'OK',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=709)
		 k=models(imodel)%n
		 ka=models(imodel)%ka
		 kb=models(imodel)%kb
		 kf=k-ka
		 kc=kf-kb
		 elmax=HJCLIK(iasymppanel,kfit,thsav,tint,ampl,iprops,nd1,nd2,Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,&
         XAF,XFA,QexpQA,QexpQF,alpha2,beta2,aloglik,kab,kAm,kFm,km)
	     prtec50=.false.

		! fit
    case(709)
				
	case(702) 

	case(732)
 
	

	case(13)	! Import
			newfile=.false.
		   
			CALL gmFileBROWSER(iFILE,iDIR,iFILT,gmBrowseType=0)
			 IF(iFILE.ne.' ') then
			 !	igraph=0

			if(allocated(xdata).and..not.newfile) DEALLOCATE(xdata)
			do i=1,njset
				jmiss(njset)=0
			enddo
			call gmSetListEntry(combo1_3,GSELECT,gmEntry=1)
		
			call file_open(MAIN,RECORDS,nplot,IFILTYPE,iplotype,iptype,iFILE,FOPEN_11,FOPEN_12,&
			APFILE,DISCPRT,njset,ncolr,nrowr,label,ftitle,nset,iwid,List7_1, &
			ftitle1, button7,button6,saveandplot)
			if(ifiltype.eq.3) nplot=1
			ALLOCATE(xdata(nplot,njset,ncols,niobs)) 
			import=.true.
			endif
		case(14,20,18)    ! Save as
			if(readrec) then
			if(modplot.gt.0) then
			   	nlp=nblank1(dfile)
				msplot=0
				if(callid.eq.18) then 
				CALL gmFileBROWSER(sFILE,sDIR,sFILT,gmBrowseType=1)
				else
				if(callid.eq.14) sfile=dfile(1:nlp-4)//'.bmp'
				if(callid.eq.20) sfile=dfile(1:nlp-4)//'.wmf'
				istatsel=gmDisplayMessageBox('',&
					'All graphs on screen will be saved',&
					Gexclamation,gok)
				endif
				IF(sFILE.ne.' ') then
					m1=1
					if(callid.eq.18) then
						
					if(oldrecords(modplot)%IPOS.eq.0) then
						m1=modplot
						msplot=1
					else
					!	do i=1,ngraph
					!		if(jopen(i).eq.1.and.oldrecords(i)%IPOS.eq.1) then
					!		m1=i
					!		exit
					!		endif
					!	enddo
						do i=m1,m1+3
							if(jopen(i).eq.1) msplot=msplot+1
						enddo 
					endif
					else
						do i=1,ngraph
							if(jopen(i).eq.1) msplot=msplot+1
						enddo 
					endif
					icallprev=callid
					ns=nblank1(sfile)
					if (sfile(ns-3:ns).eq.'.wmf') then
					    idev=1
	   					pwmf=.true.
	   				
	   					idpi=600
	   					ixoff=0
	   					iyoff=0
	   					iwidi=4800
	   					ihei=3600
	   					
					!else if (sfile(ns-3:ns).eq.'.bmp') then
					else
	   					pbmp=.true.
	   				
	   					idev=2
	   				
	   				endif 
					call devsus  
               		callid=500                   
					goto 2
				endif
				else 
					if(modplot2.gt.0.and.imodel.gt.0) then
						callid=imodel+8000
						goto 2
					endif
				endif
			else
			imessy=gmDisplayMessageBox('','No record on screen/Not available for this record',Gexclamation,gok)
			endif
			
		case(15)	! Export
			if(readrec.and.modplot.gt.0) then
		
				CALL gmFileBROWSER(eFILE,eDIR,eFILT,gmBrowseType=1)
				IF(eFILE.ne.' ') then
				call write_plotq(xval,yval,xcal,ycal,ndimd,ndimc,&
				ncurvd,ndat,icurvd,isym,ijoin,ncurvc,ncal,icurvc,iline,syms,&
				xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ntx,nty,itx,ity,&
				xlo,xhi,ylo,yhi,itit,title1,ilog,iscal,doframe,&
				titlex,titley,ilabel,inumx,inumy,efile,sval,&
				ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,y1v,y2v,y1c,y2c,ivplot,&
				theta,ifitype,ncomp,idest,interp,&
				RLTH,y0,yinf,ntrace,ytsep,ndv1,ndc1,w,kwi,kwj,icurvw,kmax)
				endif
			
			else
				imessy=gmDisplayMessageBox('','No record on screen/Not available for this record',Gexclamation,gok)
			endif
			
		case(16,17)	! Print
			
			if(readrec) then
			if(modplot.gt.0) then
				mplot=0
				if(callid.eq.16) then
				istatsel=gmDisplayMessageBox('',&
					'All graphs on screen will be printed',&
					Gexclamation,gok)
					endif
				CALL GUIPRT(0,ISTATus)
				IF(ISTATus.NE.0)THEN
					plot=.true.
					idev=6
					m1=1
					if(callid.eq.17) then
					m1=modplot
					if(oldrecords(modplot)%IPOS.eq.0) then
						m1=modplot
						msplot=1
					else
						
					!	do i=1,ngraph
					!		if(jopen(i).eq.1.and.oldrecords(i)%IPOS.eq.1) then
					!		m1=i
					!		exit
					!		endif
					!	enddo
						do i=m1,m1+3
							if(jopen(i).eq.1) msplot=msplot+1
						enddo 
					endif	
					else
						do i=1,ngraph
							if(jopen(i).eq.1) msplot=msplot+1
						enddo 
					endif
					
					icallprev=callid
					
					callid=500
					imessy=gmDisplayMessageBox('','Black & White ?',Gquestion,gyesno)
					if(imessy.eq.gyesbutton) idev=4
					goto 2
				endif
				else 
					if(modplot2.gt.0.and.imodel.gt.0) then
						callid=imodel+4000
						goto 2
					endif
				endif
			else
				imessy=gmDisplayMessageBox('','No record on screen/Not available for this record',Gexclamation,gok)
		
			endif
		case(500) ! PRINT,WMF,BMP
			if(readrec) then
				mplot=0
				ifirstprint=0
				do i=m1,ngraph
					if(jopen(i).eq.1) then
						modplot=i
						mplot=mplot+1
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
								dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,&
								parval,ifitype)
						
						
						if(ipos.eq.1.and.ifirstprint.eq.100) then
							if(pbmp.or.pwmf) then
								call devend
							else 
							!	istatp=gmPrinterControl(gnewpage)
							endif
						endif
						if((pbmp.or.pwmf).and.ipos.le.1) then
								  if(icallprev.ne.18) then
									call intconv(i,cnum0)
									nlp=nblank1(dfile)
									sfile=dfile(1:nlp-4)//cnum0
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
						icol,ntx,nty,idev)
						call draw_text(parval,xtitle,ytitle,ztitle,title1,cnumx,cnumy,&
						cnumz,cexpx,cexpy,cexpz,newtext,numbx,numby,numbz,ntext,inumx,&
						inumy,logx,logy,intx,angle,sizetext,ijus,thick,rx,ry,idraw,icol,ifnt,str,dxs,dys,idev)
											
						if(hdisp) then
						call draw_hist(xval,yval,icurvd,ncurvd,ndelt,logity,logx,&
						logy,sqrty,y0,yinf,x0,ilog,xmin,xmax,ymin,ymax,idev,ndat,ijoin,&
						icol,thick,ndv1,ndimd,xwbase,lt2)
								
						else
						call draw_data(xval,yval,icurvd,ncurvd,ndelt,logity,logx,logy,sqrty,&
						y0,yinf,x0,ilog,idev,ndat,ijoin,icol,isym,&
						symsiz,thick,barleng,interp,npint,Xint,Yint,nint,yp1,ypn,&
						Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw)
						endif
						call draw_curves(xcal,ycal,icurvc,ncurvc,logity,logx,&
						logy,sqrty,y0,yinf,x0,ilog,idev,&
						wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
						xmin,xmax,ymin,ymax,ncal,&
						iline,icol,thick,ndc1,ndimc,jfirst)
						call draw_lines(IDRAW,ICOL,THICK,C_THICK,NARROW,NLINE,NHLINE,NVLINE, &
						XBEG,YBEG,XEND,YEND,y0,yinf,x0,dxs,dys)
						if(ipos.eq.0.or.ipos.eq.4) then
							if(pbmp.or.pwmf) then
								call devend
							else 
								istatp=gmPrinterControl(gnewpage)
							endif
						else if(ipos.eq.1) then
							ifirstprint=100
						endif
						
						if(icallprev.eq.17.or.icallprev.eq.18)then
							if(ipos.eq.0.or.msplot.eq.mplot) exit
						endif
						!msplot=msplot+1
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

		case(19)	!                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
	
		case(21:29)	! Edit Text
			jindex=callid-20
			if(jindex.eq.7) jindex=31
			if(jindex.eq.8) jindex=56
        if(jindex.eq.9) index=81
				
		case(31)	! Records List	
		  if(PROGRAM_TYPE.EQ.3) then
		 goto 8787
		  do j=1,nset
		  do i=1,nfile(j)
		  	tedit1(i,j)=pfiles(i,j)
			tedit2(i,j)=expdate
			tedit3(i,j)=title
			tedit4(i,j)=tapeid
			tval1(i,j)=kt
			tval2(i,j)=npatch
			tval3(i,j)=emem
			tval4(i,j)=temperature
			tval5(i,j)=ffilt(j)
			tval6(i,j)=avamp(j)
			tval7(i,j)=rms(j)
			tval8(i,j)=calfac2
			tval9(i,j)=conc(i,j)
			tedit5(i,j)=ptype(ipatch)
		 enddo	
		 enddo		
8787			call hjcfit_table(main,hjcfitform,nset,nfile,pfiles,tedit1,tedit2,&
			tedit3,tedit4,tval1,tval2,tval3,tval4,tval5,tval6,tval7,tval8,tval9,tvalt,tedit5,&
			fc,ffilt,trise,autosim)
		  else
		  if(fopen_11) then
			call list_of_records(Main,Records,List7_1,ncolr,nrowr,label,ftitle1,iwid, & 
		    ifiltype,Button7)
		  else
			
          endif
		  endif	
		case(32)	! Experimental Data

		if(readrec) then
		  do jset=1,nset
			call format_list(ifiltype,ixd,iyd,xobs,yobs,w,nj(jset),xnum,niobs,njset,Main,&
			Data_list,jset,iplot,nplot,xdata(iplot,jset,ncols,nrows), &
            ncols,nrows,stitle(jset,iplot),hdisp,icallasc,n1,button6,ValArray6,&
		    nset,gfile,newfile)

	     enddo
		else
		
		endif
		case(33)	! Equations
			call equations(Main,Eqs,EqsMainPanel,Toggle2,titmod)
		
		case(34)	! Another Record
			if(readrec) then
			jplot=oldrecords(modplot)%iplot
			call graph1(modplot,jplot,main,ixp,iyp,graph1_1,GraphMainPanel1_1,graphics1_1,&
			wxmin,wxmax,wymin,wymax,ipos,gfile,.true.,ixposv,iyposv)

			else
			
			endif
		case(35)	!edit model
		 d_arrow=.false.
		 d_line=.false.
		 d_vline=.false.
		 d_hline=.false.
		 s_text=.false.
		 s_line=.false.
		 s_arrow=.false.
		 c_state=.false.
		 o_state=.false.
		 link=.false.
		icalprevc=0
			jmodel=jmodel+1
			nxmodel=24
			nymodel=18
            mtype=1
			mod_create=0
			do k=1,ngraph2
				call gmEnqWidgetInfo(Graph1_2(k),widget)
				ivisual=widget%visual
				if(ivisual.eq.0) then
					jopen2(k)=-1
					igraph2=k
					goto 335
				endif
			enddo
						
			igraph2=igraph2+1
			ixm=ixm+1
			iym=iym+1
			if(igraph2.gt.25) then
				imessy=gmDisplayMessageBox('','Maximum 25 models on screen',Gexclamation,gok)
				igraph2=igraph2-1
				goto 1
			endif
			ngraph2=igraph2
						

335			jopen2(igraph2)=1
			modplot2=igraph2
				indmod=.false.
			imessy=gmDisplayMessageBox('','Independent ?',Gexclamation,gyesno)
			do i=1,5
				textiqt1(i)=' '
			enddo
			ka=0
			kf=0
			kb=0
			k=0
			nsub=0
			kstat0=0
			nlig=0
			kcon=0
			imodel=jmodel
			models(jmodel)%ka=0
			models(jmodel)%kb=0
			models(jmodel)%n=0
			models(jmodel)%nsub=0
			models(jmodel)%kstat0=0
			models(jmodel)%kcon=0
			models(jmodel)%nlig=0
				models(jmodel)%title_model=' '
			if(imessy.eq.gyesbutton) then
				models(jmodel)%indmod=.true.
				indmod=.true.
				indmod=.true.
				textiq1(1)='Name '
				textiq1(2)='concerted states'
				textiq1(3)='Subunits'
				textiq1(4)='States in subunit'
				textiq1(5)='Number of ligands'
				call text_table(Main,iqwin,'Independent Model',3,textiq1,iqt1,textiqt1,59)
			else
			models(jmodel)%indmod=.false.
			indmod=.false.
			textiq1(1)='Name '
			
			textiq1(2)='Open states'
			textiq1(3)='Shut states'
			textiq1(4)='Short-lived shut states'
			textiq1(5)='Number of ligands'
			endif
			call text_table(Main,iqwin,'Model definition',5,textiq1,iqt1,textiqt1,38)

						
	case(38)
			
			do i=1,5
			call gmEnqTextSetting(iqt1(i),textiqt1(i))
		
			enddo
			call gmRemoveWindow(iqwin)	
			imodel=jmodel
			titlem=textiqt1(1)

			models(imodel)%title_model=titlem
			if(indmod) then
			
			cmodel=textiqt1(2)
			call chtoint(cmodel,kcon)
			models(imodel)%kcon=kcon
		
			cmodel=textiqt1(3)
			call chtoint(cmodel,nsub)
			models(imodel)%nsub=nsub
		
			cmodel=textiqt1(4)
			call chtoint(cmodel,kstat0)
			models(imodel)%kstat0=kstat0
			if(nsub.eq.0) nsub=1
			kstat=nsub*kstat0+kcon
			models(imodel)%kstat=kstat
			!kstat1=kstat+kcon
			kcon=models(imodel)%kcon
			nsub=models(imodel)%nsub
			kstat0=models(imodel)%kstat0
			kstat=models(imodel)%kstat
			nxmodel=4*(kstat0+2)
			nymodel=8*nsub+4
			models(imodel)%n=kcon+kstat0*kstat0
			models(imodel)%ka=kcon
			kf=nsub*kstat0
			else
			cmodel=textiqt1(2)
			call chtoint(cmodel,kA)
			models(imodel)%ka=ka
		
			cmodel=textiqt1(3)
			call chtoint(cmodel,kF)
			models(imodel)%n=ka+kf
			do j=1,100
				models(imodel)%statname(j)='          '
				models(imodel)%dgamma(j)=0.
			enddo
			do j=1,ka
				models(imodel)%dgamma(j)=50.
			enddo
			models(imodel)%kstat=ka+kf
			cmodel=textiqt1(4)
			call chtoint(cmodel,kB)
			models(imodel)%kb=kb
		!	cmodel=textiqt1(5)
		!	call chtoint(cmodel,nchan)
			nchan=1
			models(imodel)%nchan=nchan
			
		!	cmodel=textiqt1(6)
		!	call chtoreal(cmodel,vref)
			vref=-100
			models(imodel)%vref=vref
			endif
			cmodel=textiqt1(5)
			call chtoint(cmodel,nlig)
			models(imodel)%nlig=nlig
			do i=1,5
				textiqt1(i)=' '
			enddo
			if(nlig.gt.0) then
				mlig=nlig
				do i=1,nlig
					textiqt1(i)=' '
					call intconv(i,cnum1)
					textiq1(i)='Ligand'//cnum1(1:2)
				enddo
				call text_table(Main,iqwin,'Name for ligands',nlig,textiq1,iqt1,textiqt1,39)
			else
				if(models(imodel)%indmod.eq..true.) then
					callid=59
				else
					callid=41
				endif
			
				goto 2
            endif
	  case(39)
		do i=1,nlig
			call gmEnqTextSetting(iqt1(i),textiqt1(i))
			ligname(i)=textiqt1(i)
			
			models(imodel)%ligname(i)=ligname(i)
			do j=1,models(imodel)%n
				models(imodel)%nbound(j,i)=0
			enddo
			
		enddo
		call gmRemoveWindow(iqwin)	
		if(indmod) then
		callid=59
		else
		callid=41
		endif
		goto 2
	  case(40)
	    if (kcon.gt.0) then
		do i=1,kcon
			call gmEnqTextSetting(icons(i),models(imodel)%con_states(i))
			statname(i)=models(imodel)%con_states(i)
		enddo
		endif
		call ind_model(Main,imodel,models,isub)
	case(59)
			
			if(kcon.gt.0) then
			indwin1 = gmCreateMDIComplexDialogueBox(Main, 5, 3,8,kcon+3, GALL, 'Basic States ', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052')
			
				iconPanel1=gmCreatePanel(indwin1, 0, 0,8,kcon+1, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmtitle='Concerted States',gmExpand=GOff, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
	    
				do i=1,kcon
					models(imodel)%con_states(i)=' '
					icons(i) = gmCreateTextEntry(iconPanel1,1, i, 6, 1,models(imodel)%con_states(i), 60, Gedit, &
              		gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              		gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              		gmVpos=GTOP, gmExpand=GOFF)
				enddo
				
			
			
			
			iqb1=gmCreatePushButton(indwin1,3,0, 2, 1,'OK',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=40)
			else
			callid=40
			goto 2
			endif
	case(41)
		lmodel=jmodel
		igraph2=jmodel-300
		nrmodel(igraph2)=jmodel
		if(models(lmodel)%indmod.eq..true.) then
			kmodel=lmodel
		   
			do i=1,models(kmodel)%nsub
            	imodel=kmodel+i
				jmodel=imodel
			models(imodel)%indmod=.false.
			call intconv(i,cnum1)
			models(imodel)%title_model='Subunit: '//cnum1
		
			models(imodel)%n=models(kmodel)%kstat0
			models(imodel)%ka=0
			models(imodel)%kb=0
			models(imodel)%nsub=0
			models(imodel)%kcon=0
			models(imodel)%kstat0=0
			kf=models(imodel)%n
				models(imodel)%nlig=models(kmodel)%nlig
				do j=1,models(imodel)%nlig
				models(imodel)%ligname(j)=models(kmodel)%ligname(j)
				enddo
			ka=0
			if(models(imodel)%n.le.16) then
				nxmodel=20
				nymodel=models(imodel)%n+4
			else
				nxmodel=36
				nymodel=0.5*(models(imodel)%n)+4
			endif
			
			ixm=ixm+1
			iym=iym+1
			mod_create=0
			do j=1,models(kmodel)%kstat0
				models(imodel)%statname(j)=models(kmodel)%sub_states(i,j)
				call intconv(j,cnum1)
				models(imodel)%name(j)=cnum1(1:3)
			enddo
			igraph2=imodel-300	
			modplot2=igraph2
			call create_model(ixm,iym,igraph2,imodel,main,form1,form1_txtarray1,&
			graph1_2,GraphMainPanel1_2,graphics1_2,&
			nxmodel,nymodel,mtype,models,mod_create,0,qfilem,	models(imodel)%title_model,dxs,dys,ijmod,kA,kF,&
			ic,indrat,pfilem,open7,apfile,ipbar_progress2,indwin)
		
			readrec=.true.
			!modplot=imodel
			ifiltype=10
			lframe=graphics1_2(modplot2)
			nrmodel(igraph2)=imodel
			enddo
		else
		do i=1,100
			
			models(imodel)%statname(i)=' '
			call intconv(i,cnum1)
			models(imodel)%name(i)=cnum1(1:3)
			text_box(i,1)=cnum1
		
		enddo
		title_box(1)='State'
		title_box(2)='Name'
		!call table_box(main,models(imodel)%n,2,title_box,text_box,itext_box,3513)
		
			if(models(imodel)%n.gt.48) nxmodel=42	
		call create_model(ixm,iym,igraph2,imodel,main,form1,form1_txtarray1,&
			graph1_2,GraphMainPanel1_2,graphics1_2,&
			nxmodel,nymodel,mtype,models,mod_create,0,qfilem,models(imodel)%title_model,dxs,dys,ijmod,kA,kF,&
			ic,indrat,pfilem,open7,apfile,ipbar_progress2,indwin)
			
			readrec=.true.
			!modplot=imodel
			ifiltype=10
			lframe=graphics1_2(modplot2)
			nrmodel(igraph2)=imodel
		endif	
			
		do ijs=3,10
			call gmSetWidgetStatus(modelw(ijs), GSELECTABLE)
	
		enddo
							
					
					
		case(317)
			if(kmodel.ne.0.and.models(kmodel)%indmod.eq..true.) then
			imodel=kmodel
			igraph2=imodel-300	
			modplot2=igraph2
			
		
			if(models(imodel)%n.le.64) then
			call create_model(ixm,iym,igraph2,imodel,main,form1,form1_txtarray1,&
			graph1_2,GraphMainPanel1_2,graphics1_2,&
			nxmodel,nymodel,mtype,models,mod_create,0,qfilem,titlem,dxs,dys,ijmod,kA,kF,&
			ic,indrat,pfilem,open7,apfile,ipbar_progress2,indwin)
			else
			endif
			endif	
        case(36)
			icallprevm=-1
			call read_model(Main,Records,List7_1,Button7,TEXT7,lINDEX,qfilem,ijmod,rtitles,&
			nrecs)
			ifiltype=10
			mtype=1
			do i=1,200
				modopen(i)=-1
				modify(i)=.false.
			enddo
			icallprevm=callid
        CASE(37,42)
		 d_arrow=.false.
		 d_line=.false.
		 d_vline=.false.
		 d_hline=.false.
		 s_text=.false.
		 s_line=.false.
		 s_arrow=.false.
		 c_state=.false.
		 o_state=.false.
		 link=.false.
			if(ifiltype.EQ.10) then
			    call gmEnqListStatus(List7_1,nentry,nselect,nfirst)

				if(nentry.gt.0) then
				    jselect=-1
					do i=1,nentry
					    kf=0
						istat(i)=gmEnqListEntry(List7_1,i,TEXT7(i))
						if(istat(i).eq.2) then
						if(jselect.eq.-1) then
							do ijs=3,10
									call gmSetWidgetStatus(modelw(ijs), GSELECTABLE)
	
							enddo
							jselect=0
						endif
						if(callid.eq.42) then
							Cnum0=TEXT7(i)(1:3)
							CALL CHTOINT(Cnum0,IMODEL)
							mod_create=2
							cnum=text7(i)(1:3)
		                call chtoint(cnum,imod)
						readrec=.true.
						irc=i
						call create_model(ixm,iym,igraph2,imodel,main,form1,form1_txtarray1,&
						graph1_2,GraphMainPanel1_2,graphics1_2,&
						nxmodel,nymodel,mtype,models,mod_create,irc,qfilem,text7(i),&
						dxs,dys,ijmod,ka,kf,ic,indrat,pfilem,open7,apfile,ipbar_progress2,indwin)
						call gmSetProgressValue(ipbar_Progress2, 10)

						jmodel=jmodel+1
						newmodel=jmodel
						igraph2=igraph2+1
						jopen2(igraph2)=1

						modplot2=igraph2
	
						models(newmodel)%jgraph=igraph2
						nrmodel(modplot2)=newmodel
						call copy_model(models,imodel,newmodel)
						mod_create=0
						call draw_model(igraph2,newmodel,main,graph1_2,GraphMainPanel1_2,graphics1_2,&
						models,.false.,0,0,0)
						if(callid.eq.42) modify(imodel)=.true.
						else
						if(modopen(i).ne.2) then
							Cnum0=TEXT7(i)(1:3)
							CALL CHTOINT(Cnum0,IMODEL)
							mod_create=1
							modopen(i)=2
						    
						    do k=1,ngraph2
							call gmEnqWidgetInfo(Graph1_2(k),widget)
							ivisual=widget%visual
							if(ivisual.eq.0) then
								jopen2(k)=-1
								igraph2=k
								goto 334
							endif
						    enddo
						
						igraph2=igraph2+1
						ixm=ixm+1
						iym=iym+1
						if(igraph2.gt.25) then
								imessy=gmDisplayMessageBox('','Maximum 25 models on screen',Gexclamation,gok)
								igraph2=igraph2-1
								goto 1
						endif
						ngraph2=igraph2
						

334						jopen2(igraph2)=1
						modplot2=igraph2
						lframe=graphics1_2(modplot2)
							readrec=.true.
						!modplot=imodel
						cnum=text7(i)(1:3)
		                call chtoint(cnum,imod)
						
						irc=i
						call create_model(ixm,iym,igraph2,imodel,main,form1,form1_txtarray1,&
						graph1_2,GraphMainPanel1_2,graphics1_2,&
						nxmodel,nymodel,mtype,models,mod_create,irc,qfilem,text7(i),&
						dxs,dys,ijmod,ka,kf,ic,indrat,pfilem,open7,apfile,ipbar_progress2,indwin)
						nrmodel(igraph2)=imodel
						call gmSetProgressValue(ipbar_Progress2, 20)

						continue
						endif
						endif
						endif
					ENDDO
				
				
				ENDIF
				call gmremovewindow(records)
			ENDIF
	 	
	  
					
       case(43:46,51,52)
	  
	   if(readrec.and.oldrecords(modplot)%IPOS.ne.4) then
		npoint=2
		if(callid.eq.43) then  !arrow
		move_state=.false.
		zoom=.false.
		 d_arrow=.true.
		 d_line=.false.
		 d_vline=.false.
		 d_hline=.false.
		 s_text=.false.
		 s_line=.false.
		 s_arrow=.false.
		 c_state=.false.
		 o_state=.false.
		 link=.false.
		 
		 ind_1=narrow+1
	!	 call widsta(new_arrows(ind_1),1)
	   else if(callid.eq.44) then  ! line
	   	zoom=.false.
		 d_line=.true.
		 d_hline=.false.
		 d_vline=.false.
		 d_arrow=.false.
		 s_text=.false.
		 s_line=.false.
		 s_arrow=.false.
		 c_state=.false.
		 o_state=.false.
		 link=.false.
		 
		 ind_1=nline+11
		 move_state=.false.
	!	 call widsta(new_lines(ind_1),1)
	   else if(callid.eq.45) then  ! h line
	   	zoom=.false.
		move_state=.false.
		 d_hline=.true.
		 d_line=.false.
		 d_vline=.false.
		 d_arrow=.false.
		 s_text=.false.
		 s_line=.false.
		 s_arrow=.false.
		 s_arrow=.false.
		 c_state=.false.
		 o_state=.false.
		 link=.false.
		 
		 ind_1=nhline+21

	!	 call widsta(new_lines(ind_1+10),1)
	   else if(callid.eq.46) then ! v line
	   	zoom=.false.
		 d_vline=.true.
		 d_line=.false.
		 d_hline=.false.
		 d_arrow=.false.
		 s_text=.false.
		 s_line=.false.
		 s_arrow=.false.
		 s_arrow=.false.
		 c_state=.false.
		 o_state=.false.
		 link=.false.
		 move_state=.false.
		 
		 ind_1=nvline+31
	!	 call widsta(new_lines(ind_1+20),1)
        else if (callid.eq.51) then
		 zoom=.true.
		 d_vline=.false.
		 d_line=.false.
		 d_hline=.false.
		 d_arrow=.false.
		 s_text=.false.
		 s_line=.false.
		 s_arrow=.false.
		 s_arrow=.false.
		 c_state=.false.
		 o_state=.false.
		 move_state=.false.
		 link=.false.
		 ind_1=45
	    else if (callid.eq.52) then
			zoom=.false.
			move_state=.false.
			if(izoom.eq.1) then
				xmin1=xmin0
				xmax1=xmax0 
				ymin1=ymin0
				ymax1=ymax0
				callid=405
				goto 2 
			endif
				endif
				endif
 case(314,315,316)
 
		if(callid.eq.314) then  !open state
		if(modplot2.gt.0)	call gmSetGuiCursor(lframe,Gdefault,GDEFAULT)
		 zoom=.false.
		 o_state=.true.
		 c_state=.false.
		 link=.false.
		 d_vline=.false.
		 d_line=.false.
		 d_hline=.false.
		 d_arrow=.false.
		 s_text=.false.
		 s_line=.false.
		 s_arrow=.false.
		 move_state=.false.
		else if(callid.eq.315) then   !close state
			if(modplot2.gt.0)	call gmSetGuiCursor(lframe,Gdefault,GDEFAULT)
		 zoom=.false.
		 c_state=.true.
		 o_state=.false.
		 link=.false.
		 d_vline=.false.
		 d_line=.false.
		 d_hline=.false.
		 d_arrow=.false.
		 s_text=.false.
		 s_line=.false.
		 s_arrow=.false.
		 move_state=.false.
		else if(callid.eq.316) then   !link
		if(modplot2.gt.0) then
		imessy=gmDisplayMessageBox('','Did you enter all the states',gquestion,Gyesno)
			if(imessy.eq.gnobutton) goto 1 
		 zoom=.false.
		 c_state=.false.
		 o_state=.false.
		 link=.true.
		 d_vline=.false.
		 d_line=.false.
		 d_hline=.false.
		 d_arrow=.false.
		 s_text=.false.
		 s_line=.false.
		 s_arrow=.false.
		 move_state=.false.
		 if(models(imodel)%indmod.eq..true.) then
		!	imessy=gmDisplayMessageBox('','Change states numeroration ?',Gexclamation,gyesno)
			
			if(imessy.eq.gyesbutton) then
			endif
		 endif
		 endif
	   endif
		
	
			
		case(71,72,73,74)	! Read record and display
			!new
			if(callid.eq.73) then
				callid=71
				plot=.true.
			
			else if(callid.eq.74) then
				callid=72
				plot=.true.
			endif
		
			if(ifiltype.gt.0.and.ifiltype.lt.4.and.fopen_11) then
			
			if(.not.saveandplot) then
					ixp0=-1
					iyp0=-1
					call gmEnqListStatus(List7_1,nentry,nselect,nfirst)
			endif
				
			if(nentry.gt.0) then
				
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
					if(.not.saveandplot) then
							istat(i)=gmEnqListEntry(List7_1,i,ftitle1(i))
					endif
					if(istat(i).eq.2) then
					!	if(iopen(i).eq.2) then
					!		imessy=gmDisplayMessageBox('','Record already on screen',Gexclamation,gok)
			!
					!		goto 1
					!	endif
								! Set up complex dialogue box Graph child of Main
						iopen(i)=2
						zoom=.false.
						izoom=0
						readrec=.true.
						iplot=i
						mplot=mplot+1
						nplot_on(mplot)=iplot
						!modplot=iplot
						if(callid.eq.72) then
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
								igraph=k
								goto 333
							endif
							endif
						enddo
						endif
						igraph=igraph+1
						
					
						if(.not.plot.and.igraph.gt.20) then
									imessy=gmDisplayMessageBox('','Maximum 20 plots on screen',Gexclamation,gok)
							
								goto 1
						endif
						ngraph=igraph

333						jopen(igraph)=1
						modplot=igraph
							if(.not.plot) oldrecords(modplot)%ipos=ipos
						
							if(allocated(xval)) DEALLOCATE(Xval,Yval,w)
							if(allocated(xcal)) DEALLOCATE(Xcal,Ycal)
							if(allocated(icurvw)) DEALLOCATE(icurvw)
							if(allocated(theta)) DEALLOCATE(theta)
							if(allocated(thetgues)) DEALLOCATE(thetgues)
							if(allocated(ndat)) DEALLOCATE(ndat,isym,ijoin,symsiz)
							if(allocated(ncal)) DEALLOCATE(ncal,iline)
							if(allocated(icurvd)) DEALLOCATE(icurvd,icurvc)
							hdisp=.false.
							autplt=.false.
							if(ifiltype.eq.1) allocate(w(niobs,njset))
						
							call read_record(ISTREC,Main,Data_list,iplot,ifiltype,&
							iplotype,njset,nset,nsfit,&
							xnum,nrows,niobs,njobs,stitle,hdisp,xobs,yobs,w,nj,&
							titlex,titley,ilabel,title1,iptype,ndv1,ndimd,&
							ndc1,ndimc,kwi,kwj,kmax,ncurvd,ncurvc,dfile,nval1,&
							useconsam,filtered,istr1,colseq,nhline,adcfil,&
							itit,ndev,ib,istart,iend,srate,calfac,ioff,calfac2,&
							cdate,adctime,adcf1,isdfst,ndisp1,nrec1,jdat,base,&
							ntrans,y0,t0,nfilter,dtf,tif1,tif2,dt,stepamp,filt,&
							ndelt,ffilt,fcz,fczoom,njump,idest,krn,titled)
							
						
							if(ifiltype.eq.1) then  !cvdat
								oldrecords(modplot)%iplotype=-1
								gfile=dfile
									kwi=niobs
									kwj=njobs
								ndimc=20
								ndc1=501
								kmax=20
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
								do j=1,njset
								    
								
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
								call calc_default(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,symsiz,&
								ndimd,XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,&
								XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,&
								XLO1,XHI1,YLO1,YHI1,y0,yinf,inumx,inumy,&
								titlex,titley,ilabel,doframe,autplt,itit,title1,&
								ISHP,ifnt,landscap,ndv1,ndc1,w,kwi,kwj,icurvw,kmax,iver,&
								redrawn,plot,redo,pbmp,pwmf,npint,hdisp,n1,tlenx,tleny,nx1,ny1,izoom,&
								xtitle,ytitle,rescale,dxs,dys,ifitype,parval,theta,ncomp)
								n1=1
								ncol=4
								if(hdisp) ncol=1
								
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
								do J=1,nset
									
									nd1=nj(j)+20	!extra rows in case lines added in inwindv
	    
									NROWS=NJ(J)
									NCOLS=NCOL
									if(.not.plot) then
		 							call format_list(ifiltype,ixd,iyd,xobs,yobs,w,nj(j),xnum,niobs,&
									njset,Main,Data_list,j,iplot,nplot,xdata,ncols,nrows,&
									stitle(j,iplot),hdisp,icallasc,n1,button6,&
									ValArray6,nset,gfile,newfile)
									endif
								enddo
								isens=1
								if(.not.plot.and.igraph.le.20) then
								n1=1
								call store_record(iplotype,igraph,iplot,ixp,iyp,ipos,xobs,yobs,w,&
								nj,niobs,njset,nplot,nset,&
								juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
								wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
								ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
								xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,oldrecords,&
								dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,parval,ifitype)
								endif
								
							else if (ifiltype.eq.2) then ! plotq
								
								if(import)	then 
									gfile=ifile
								else
									gfile=dfile
								endif
							
								if(iplotype.eq.1) then
									oldrecords(modplot)%iplotype=1
									n1=1
								else if(iplotype.eq.2) then
									
									oldrecords(modplot)%iplotype=2
									n1=0
									kwi=1	!no SD for single channels
									kwj=1
								else if(iplotype.eq.3) then
									n1=1
									kwi=1	!no SD for single channels
									kwj=1
									kmax=1
									if (iptype.eq.33) then
									oldrecords(modplot)%iplotype=33
										ncurvc=0
										ncurvd=1
										ndv1=nval1
										ndimd=1
										ndc1=1
										ndimc=1
										itx=1
										ity=1
									else
										oldrecords(modplot)%iplotype=3
										
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
								ALLOCATE(icurvw(ndimd),w(kwi,kwj))
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
								kwi,kwj,icurvw,kmax,iver)
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
									
								!	call format_list(ifiltype,xval,yval,w,ndat(j),xnum,ndv1,njset,&
								!	Main,Data_list,j,iplot,nplot,xdata,&
								!	ncols,nrows,stitle(j,iplot),hdisp,icallasc,n1,button6,&
								!   ValArray6,ndimd,gfile)
								
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
								screen,colplotter,iptype,ndv1,ndc1,kmax,iver,idev)
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
								!	call format_list(ifiltype,xval1,yval1,w,ndat(j),xnum,ndv1,njset,&
								!	Main,Data_list,j,iplot,nplot,xdata, &
								!	ncols,nrows,stitle(j,iplot),hdisp,icallasc,n1,button6,&
								!   ValArray6,ndimd,gfile)
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
								if(idest.ne.99.and.iplotype.le.3) then
									iparfirst=-100
								call calc_default(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,symsiz,&
								ndimd,XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,&
								XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,&
								XLO1,XHI1,YLO1,YHI1,y0,yinf,inumx,inumy,&
								titlex,titley,ilabel,doframe,autplt,itit,title1,&
								ISHP,ifnt,landscap,ndv1,ndc1,w,kwi,kwj,icurvw,kmax,iver,&
								redrawn,plot,redo,pcgm,pwmf,npint,hdisp,n1,tlenx,tleny,nx1,ny1,izoom,&
								xtitle,ytitle,rescale,dxs,dys,ifitype,parval,theta,ncomp)
								isens=1
								if(.not.plot.and.igraph.le.20) then
								!if(iplotype.ne.3) ndimc=20
								if(ndv1.gt.niobs) niobs=ndv1
								do j=1,nset
									nj(j)=ndat(j)
								enddo
								n1=1
								if(hdisp) n1=0
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
								logx,logy,sqrty,doframe,icol,ntx,nty,idev)
							call draw_text(parval,xtitle,ytitle,ztitle,title1,cnumx,cnumy,&
								cnumz,cexpx,cexpy,cexpz,newtext,numbx,numby,numbz,ntext,inumx,&
								inumy,logx,logy,intx,angle,sizetext,ijus,thick,rx,ry,idraw,&
								icol,ifnt,str,dxs,dys,idev)
							!	kwi=niobs
							!	kwj=njset
							if(hdisp) then
								call draw_hist(xval,yval,icurvd,ncurvd,ndelt,logity,logx,&
									logy,sqrty,y0,yinf,x0,ilog,xmin,xmax,ymin,ymax,idev,ndat,ijoin,&
									icol,thick,ndv1,ndimd,xwbase,lt2)
								
                            else
								call draw_data(xval,yval,icurvd,ncurvd,ndelt,logity,logx,logy,sqrty,&
									y0,yinf,x0,ilog,idev,ndat,ijoin,icol,isym,&
									symsiz,thick,barleng,interp,npint,Xint,Yint,nint,yp1,ypn,&
									Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw)
							endif
							
							call draw_curves(xcal,ycal,icurvc,ncurvc,logity,logx,&
								logy,sqrty,y0,yinf,x0,ilog,idev,&
								wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
								xmin,xmax,ymin,ymax,ncal,&
								iline,icol,thick,ndc1,ndimc,jfirst)
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
							oldrecords,nplot)
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
		case(47,48,943:946,980)
		if(readrec) then
			ntog=0
			select case(callid)
				case(980)
					itcall=80
					ntog=oldrecords(modplot)%numbers%ntext
					do i=1,ntog
						text_tog(i)='Text '//CHAR(48+I)
					enddo
				case(47)
					itcall=100
					ntog=	oldrecords(modplot)%ncurvd
					do i=1,ntog
						text_tog(i)='Data line '//CHAR(48+I)
					enddo
				case(48)
					itcall=150
					ntog=	oldrecords(modplot)%ncurvc
					do i=1,ntog
						text_tog(i)='Curve '//CHAR(48+I)
					enddo
				case(943)
					itcall=200
					ntog=oldrecords(modplot)%lines%narrow
					do i=1,ntog
						text_tog(i)='Arrow '//CHAR(48+I)
					enddo
				case(944)
					itcall=210
					ntog=oldrecords(modplot)%lines%nline
					do i=1,ntog
						text_tog(i)='Line '//CHAR(48+I)
					enddo
				case(945)
					itcall=220
					ntog=oldrecords(modplot)%lines%nhline
					do i=1,ntog
						text_tog(i)='Horizontal line '//CHAR(48+I)
					enddo
				case(946)
					itcall=230
					ntog=oldrecords(modplot)%lines%nvline
					do i=1,ntog
						text_tog(i)='Vertical line '//CHAR(48+I)
					enddo
			end select
			if(ntog.gt.0) then
		
			call toggle_panel(Main,ITOGGLE,ntog,text_tog,intoggle,itcall,valdat,idat,itogbutton)
			else
			imessy=gmDisplayMessageBox('','No item to be selected',Gexclamation,gok)
		
			endif
			else
			imessy=gmDisplayMessageBox('','No record on screen',Gexclamation,gok)
		
			endif
		case(81:245)	! Data, Curves, Axis, Frame, Lines, Arrows
			index = callid
		case(901) 
			call widrem(itoggle)
		case(251:255,258) !write title,label,text,parameters
		     if((readrec.or.newfile).and.oldrecords(modplot)%IPOS.ne.4) then
				jindex=callid-250
			
				iy=1
				if(callid.eq.251) then
				mtext1='Enter title:'
				mtext2=title1
				jindex=1
				else if(callid.eq.252) then
				mtext1='Enter parameters:'
				mtext2=parval
				iy=5
				jindex=2
				if (lframe.eq.graphics1_2(modplot2).and.istate.gt.0) then
					write_par=.true.
				endif
				else if(callid.eq.253) then
				mtext1='Enter x label:'
				mtext2=xtitle
				jindex=3
				else if(callid.eq.254) then
				mtext2=ytitle
				mtext1='Enter y label:'
				jindex=4
				else if(callid.eq.258) then
				mtext1='Enter new text:'
				mtext2=' '
				iy=5
				ntext=ntext+1
				jindex=80+ntext
				endif
				continue
				!if(write_par) then
				!else
				call add_new_text(main,callid,mtext1,mtext2,iy,itext_entry,new_text_entry)
			
				ind_1=jindex
			!	call widsta(new_text(ind_1),1)
			!	endif
			endif
		case(290) ! write new text
			call gmEnqTextSetting(new_text_entry,mtext2)
			if(jindex.ge.81) then
				newtext(ntext)=mtext2
				oldrecords(modplot)%str(ntext+80)=mtext2
				d_text=.true.
			else
			itempcol=oldrecords(modplot)%attributes%icol(jindex)
			oldrecords(modplot)%attributes%icol(jindex)=0
			call write_string(oldrecords(modplot)%str(jindex),oldrecords(modplot)%attributes%rx(jindex),&
			oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
			call string_box(oldrecords(modplot)%str(jindex),	oldrecords(modplot)%attributes%rx(jindex),&
				oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
				oldrecords(modplot)%attributes%ijus(jindex),&
     		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			endif
			if(jindex.eq.1) then
				title1=mtext2
				oldrecords(modplot)%str(1)=mtext2
			else if(jindex.eq.2) then
				parval=mtext2
				oldrecords(modplot)%str(2)=mtext2
			else if(jindex.eq.3) then
				titlex=mtext2
				oldrecords(modplot)%str(3)=mtext2
			else if(jindex.eq.4) then
				titley=mtext2
				oldrecords(modplot)%str(4)=mtext2		
			endif
				
		  	oldrecords(modplot)%attributes%icol(jindex)=itempcol
			call write_string(oldrecords(modplot)%str(jindex),	oldrecords(modplot)%attributes%rx(jindex),&
			oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
			call string_box(oldrecords(modplot)%str(jindex),	oldrecords(modplot)%attributes%rx(jindex),&
			oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),&
     		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),&
			imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			endif
		  endif
		!	index=0
		case (301)	! Bold
		  if(jindex.ge.1.and.jindex.le.100) then
		  itempcol=oldrecords(modplot)%attributes%icol(jindex)
		  oldrecords(modplot)%attributes%icol(jindex)=0
		  k=1
		  if(jindex.ge.6.and.jindex.le.30) k=numbx 
		  if(jindex.ge.31.and.jindex.le.55) k=numby
		  do i=1,k
		  	call write_string(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
			oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
			call string_box(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),&
			oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),&
     		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			endif
		  enddo
		  if(oldrecords(modplot)%attributes%thick(jindex).eq.0.) then
			!thick(jindex)=dxs
          else
			oldrecords(modplot)%attributes%thick(jindex)=0.	
          endif
		  oldrecords(modplot)%attributes%icol(jindex)=itempcol	
		  oldrecords(modplot)%attributes%ifnt(jindex)=15
		  ifnt(jindex)=15
		  do i=1,k
			call write_string(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),&
				oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
			call string_box(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
			oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),&
     		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			endif
		  enddo
		  endif	
        case (302)	! Italic
		  if(jindex.ge.1.and.jindex.le.100) then
		  itempcol=oldrecords(modplot)%attributes%icol(jindex)
		  oldrecords(modplot)%attributes%icol(jindex)=0
		  k=1
		  if(jindex.ge.6.and.jindex.le.30) k=numbx 
		  if(jindex.ge.31.and.jindex.le.55) k=numby
		  do i=1,k
			call write_string(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),&
			oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
		  
			if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
			call string_box(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
			oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),&
     		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			endif
         enddo
	     if(oldrecords(modplot)%attributes%ifnt(jindex).eq.6) then
			oldrecords(modplot)%attributes%ifnt(jindex)=2
		ifnt(jindex)=2
          else
			oldrecords(modplot)%attributes%ifnt(jindex)=6
			ifnt(jindex)=6
		
          endif
		  oldrecords(modplot)%attributes%icol(jindex)=itempcol
		  do i=1,k
			call write_string(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
				oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
			call string_box(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
				oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),oldrecords(modplot)%attributes%ijus(jindex),&
     		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			endif		 
		  enddo	
		  
		  call gsetitalicangle(0.0)
		  endif	
        case (303)	! Underline
		if(jindex.ge.1.and.jindex.le.100) then
		  if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1)then
		     if(imode.eq.1) then
				imode=2
	         else if(imode.eq.2) then

			    imode=-1
			endif			
		  else
			oldrecords(modplot)%attributes%idraw(jindex)=-1
			imode=2
			idraw(jindex)=-1
          endif
		  k=1
		  if(jindex.ge.6.and.jindex.le.30) k=numbx 
		  if(jindex.ge.31.and.jindex.le.55) k=numby
		  do i=1,k
			if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
			call string_box(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
				oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),oldrecords(modplot)%attributes%ijus(jindex),&
     		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
		    endif
		   enddo
		endif
		case (304,305,306)	! justify
		  k=1
		  if(jindex.ge.6.and.jindex.le.30) k=numbx 
		  if(jindex.ge.31.and.jindex.le.55) k=numby
		  if(jindex.ge.1.and.jindex.le.100) then
		  itempcol=oldrecords(modplot)%attributes%icol(jindex)
		  oldrecords(modplot)%attributes%icol(jindex)=0
		  do i=1,k
		  call write_string(oldrecords(modplot)%str(jindex+i-1),	&
			oldrecords(modplot)%attributes%rx(jindex+i-1),&
		  	oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
		  		 
		  if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
			call string_box(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),&
				oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
				oldrecords(modplot)%attributes%ijus(jindex),&
     		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			endif
		  enddo	  
          if(callid.eq.304) then
			oldrecords(modplot)%attributes%ijus(jindex)=1
			ijus(jindex)=1
          else if(callid.eq.305) then
			oldrecords(modplot)%attributes%ijus(jindex)=0
			ijus(jindex)=0
		  else
			oldrecords(modplot)%attributes%ijus(jindex)=-1
			ijus(jindex)=-1		
          endif
		  
		  oldrecords(modplot)%attributes%icol(jindex)=itempcol
		  do i=1,k
			call write_string(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),&
				oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
		  		 	
			if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
			call string_box(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
				oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),oldrecords(modplot)%attributes%ijus(jindex),&
     		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			endif
          enddo
		endif
		case (307)  ! colour
		if (modplot.gt.0.and.lframe.eq.graphics1_1(modplot))then
			k=1
			if(jindex.ge.6.and.jindex.le.30) k=numbx 
			if(jindex.ge.31.and.jindex.le.55) k=numby
			newcol=-1
			call gmColourControl(RED,GREEN,BLUE)
			call select_colour(red,green,blue,newcol)
			if(newcol.eq.-1) then
			newcol=200+indcol
			call gDefineRGB(newcol,RED,GREEN,BLUE)
			indcol=indcol+1
			endif
		if(jindex.ge.1.and.jindex.le.250) then
		oldrecords(modplot)%attributes%icol(jindex)=newcol
		icol(jindex)=newcol
		call gmActivateGraphicsFrame(graphics1_1(modplot))
		if(jindex.ge.1.and.jindex.le.100) then
		    do i=1,k
			call write_string(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),&
			oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
			call string_box(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
			oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),&
     		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			endif  	
			enddo	
		else if(jindex.ge.101.and.jindex.le.150) then
			if(hdisp) then
				call draw_hist(xval,yval,icurvd,ncurvd,ndelt,logity,logx,&
				logy,sqrty,y0,yinf,x0,ilog,xmin,xmax,ymin,ymax,idev,ndat,ijoin,&
				icol,thick,ndv1,ndimd,xwbase,lt2)
								
            else
				call draw_data(xval,yval,icurvd,ncurvd,ndelt,logity,logx,logy,sqrty,&
				y0,yinf,x0,ilog,idev,ndat,ijoin,icol,isym,&
				symsiz,thick,barleng,interp,npint,Xint,Yint,nint,yp1,ypn,&
				Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw)
			endif
		else if(jindex.ge.151.and.jindex.le.200) then
			call draw_curves(xcal,ycal,icurvc,ncurvc,logity,logx,&
			logy,sqrty,y0,yinf,x0,ilog,idev,&
			wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
			xmin,xmax,ymin,ymax,ncal,&
			iline,icol,thick,ndc1,ndimc,jfirst)
		else if(jindex.ge.201.and.jindex.le.240) then
		call draw_lines(IDRAW,ICOL,THICK,C_THICK,NARROW,NLINE,NHLINE,NVLINE, &
		XBEG,YBEG,XEND,YEND,y0,yinf,x0,dxs,dys)

		else if (jindex.eq.244.or.jindex.eq.245) then
			call draw_axis(xmin,xmax,ymin,ymax,ymin1,ymax1,xtic,ytic,&
			nx1,ny1,xcross,ycross,itx,ity,tlenx,tleny,logx,logy,sqrty,doframe,&
			icol,ntx,nty,idev)
		endif
		endif
		endif		    	    
		case(308)	!delete
		if (modplot.gt.0.and.lframe.eq.graphics1_1(modplot)) then
			call gmSetGuiCursor(lframe,Gdefault,GDEFAULT)
	
			if(jindex.ge.1.and.jindex.le.100) then	
			oldrecords(modplot)%attributes%icol(jindex)=0
			k=1
		    if(jindex.ge.6.and.jindex.le.30) k=numbx 
		    if(jindex.ge.31.and.jindex.le.55) k=numby
		    do i=1,k
		 	call write_string(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
				oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
			call string_box(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
				oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),oldrecords(modplot)%attributes%ijus(jindex),&
     		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			endif
			enddo
			endif
		else if (modplot2.gt.0.and.lframe.eq.graphics1_2(modplot2).and.models(nrmodel(modplot2))%n.gt.0) then
					call gmSetGuiCursor(lframe,Gdefault,GDEFAULT)
	
				delete_state=.true.
				zoom=.false.
				c_state=.false.
				o_state=.false.
				link=.false.
				d_vline=.false.
				d_line=.false.
				d_hline=.false.
				d_arrow=.false.
				s_arrow=.false.
				s_text=.false.
				s_line=.false.
				s_arrow=.false.
				move_state=.false.
			endif
		case(309)   ! move
		if (modplot2.gt.0.and.lframe.eq.graphics1_2(modplot2).and.models(nrmodel(modplot2))%n.gt.0) then
			
			move_state=.true.
			delete_state=.false.
			zoom=.false.
			c_state=.false.
			o_state=.false.
			link=.false.
			d_vline=.false.
			d_line=.false.
			d_hline=.false.
			d_arrow=.false.
			s_text=.false.
			s_line=.false.
			s_arrow=.false.
				else
			s_text=.true.
			d_line=.false.
			d_hline=.false.
			d_vline=.false.
			d_arrow=.false.
		
			s_line=.false.
			s_arrow=.false.
			
			endif
		case(310)	! box
		if(jindex.ge.1.and.jindex.le.100.and.modplot.gt.0) then
			call gmActivateGraphicsFrame(graphics1_1(modplot))
			k=1
		    if(jindex.ge.6.and.jindex.le.30) k=numbx 
		    if(jindex.ge.31.and.jindex.le.55) k=numby
		    if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
				imode=-1
				do i=1,k
					call string_box(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
						oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
						oldrecords(modplot)%attributes%ijus(jindex),&
     				oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
				enddo
				oldrecords(modplot)%attributes%idraw(jindex)= 1
			else
				oldrecords(modplot)%attributes%idraw(jindex)=-1
				imode=1
				do i=1,k
					call string_box(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
						oldrecords(modplot)%attributes%ry(jindex+i-1),&
						oldrecords(modplot)%attributes%angle(jindex),oldrecords(modplot)%attributes%ijus(jindex),&
     				oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
				enddo
			endif
			
        endif
		case(311) !rotate
		if(jindex.ge.1.and.jindex.le.100.and.modplot.gt.0) then
		  itempcol=oldrecords(modplot)%attributes%icol(jindex)
		  oldrecords(modplot)%attributes%icol(jindex)=0
		  k=1
		  if(jindex.ge.6.and.jindex.le.30) k=numbx 
		  if(jindex.ge.31.and.jindex.le.55) k=numby
		  do i=1,k
		  	 call write_string(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
			 	oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
		    if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
		  	call string_box(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
			oldrecords(modplot)%attributes%ry(jindex+i-1),&
			oldrecords(modplot)%attributes%angle(jindex),oldrecords(modplot)%attributes%ijus(jindex),&
     		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			endif
		   enddo
			do i=1,6
			ifstat(i)=gmEnqListEntry(Combo1_6,i,textcombo)
			if(ifstat(i).eq.2)  oldrecords(modplot)%attributes%angle(jindex)=float(i-1)*15.
			enddo
          
		  oldrecords(modplot)%attributes%icol(jindex)=itempcol
		  do i=1,k
			call write_string(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),&
			oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
		 
			if(idraw(jindex).eq.-1) then
		  	call string_box(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),&
			oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),oldrecords(modplot)%attributes%ijus(jindex),&
     		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			endif
		  enddo		
		  endif

		  case(312)  !line style

		  case(313)  ! thickness

		  

		  case(319) !chaNge font
			if(jindex.ge.1.and.jindex.le.100.and.modplot.gt.0) then
			itempcol=oldrecords(modplot)%attributes%icol(jindex)
		    oldrecords(modplot)%attributes%icol(jindex)=0
				k=1
		  if(jindex.ge.6.and.jindex.le.30) k=numbx 
		  if(jindex.ge.31.and.jindex.le.55) k=numby
		  do i=1,k
			call write_string(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),&
			oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
		 
		    if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
		  	call string_box(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),&
			oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),oldrecords(modplot)%attributes%ijus(jindex),&
     		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			endif
		   enddo		
			do i=1,28
			ifstat(i)=gmEnqListEntry(Combo1_4,i,textcombo)
			if(ifstat(i).eq.2)  oldrecords(modplot)%attributes%ifnt(jindex)=i
			enddo
			oldrecords(modplot)%attributes%icol(jindex)=itempcol
			do i=1,k
			call write_string(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),&
			oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
		 	if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
		  	call string_box(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),oldrecords(modplot)%attributes%ry(jindex+i-1),&
			oldrecords(modplot)%attributes%angle(jindex),oldrecords(modplot)%attributes%ijus(jindex),&
     		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
			endif	
			enddo
		    endif
		case(320) !change size
			if(jindex.ge.1.and.jindex.le.100.and.modplot.gt.0) then
				itempcol=oldrecords(modplot)%attributes%icol(jindex)
				oldrecords(modplot)%attributes%icol(jindex)=0
				k=1
				if(jindex.ge.6.and.jindex.le.30) k=numbx 
				if(jindex.ge.31.and.jindex.le.55) k=numby
				do i=1,k
				call write_string(oldrecords(modplot)%str(jindex+i-1),&
				oldrecords(modplot)%attributes%rx(jindex+i-1),oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
				oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
				oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
				if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
		  		call string_box(oldrecords(modplot)%str(jindex+i-1),&
				oldrecords(modplot)%attributes%rx(jindex+i-1),oldrecords(modplot)%attributes%ry(jindex+i-1),&
				oldrecords(modplot)%attributes%angle(jindex),oldrecords(modplot)%attributes%ijus(jindex),&
     			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
				endif	
				enddo
				do i=1,12
					ifstat(i)=gmEnqListEntry(Combo1_5,i,textcombo)
					if(ifstat(i).eq.2) oldrecords(modplot)%attributes%sizetext(jindex)=select_size(i,sdim)
				enddo
				oldrecords(modplot)%attributes%icol(jindex)=itempcol
				do  i=1,k
				call write_string(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),oldrecords(modplot)%attributes%ry(jindex+i-1),&
				oldrecords(modplot)%attributes%angle(jindex),&
				oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
				oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)	
				if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
		  		call string_box(oldrecords(modplot)%str(jindex+i-1),&
				oldrecords(modplot)%attributes%rx(jindex+i-1),oldrecords(modplot)%attributes%ry(jindex+i-1),&
				oldrecords(modplot)%attributes%angle(jindex),oldrecords(modplot)%attributes%ijus(jindex),&
     			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)
				endif	
		 		enddo
			endif
		  case(401,402,405) ! Shape,scale
		
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
				
				endif
		  if(callid.eq.401) then
			do i=1,5
					ifstat(i)=gmEnqListEntry(Combo1_1,i,textcombo)
					if(ifstat(i).eq.2) ishp=i-1
			enddo
			IF(ISHP.EQ.4) THEN
				IVPLOT=.TRUE.
			ELSE
				IVPLOT=.FALSE.
				DOFRAME=.TRUE.
			ENDIF
		  else  if(callid.eq.402) then ! scale
			do i=1,7
					ifstat(i)=gmEnqListEntry(Combo1_2,i,textcombo)
					if(ifstat(i).eq.2) ilog=i-1
			enddo
          endif
		   	
		  if(readrec.and.oldrecords(modplot)%IPOS.eq.0) then
		    rescale=.true.
			autplt=.false.
			call calc_default(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,symsiz,&
				ndimd,XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,&
				XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,&
				XLO1,XHI1,YLO1,YHI1,y0,yinf,inumx,inumy,&
				titlex,titley,ilabel,doframe,autplt,itit,title1,&
				ISHP,ifnt,landscap,ndv1,ndc1,w,kwi,kwj,icurvw,kmax,iver,&
				redrawn,plot,redo,pcgm,pwmf,npint,hdisp,n1,tlenx,tleny,nx1,ny1,izoom,&
				xtitle,ytitle,rescale,dxs,dys,ifitype,parval,theta,ncomp)	
				ipos=0
				rescale=.false.
				call gmSetGuiGridMode(GOFF)
				call gmEnqWidgetInfo(main,Widget)
				ixp_main=widget%xpos
				iyp_main=widget%ypos
				call gmEnqWidgetInfo(graph1_1(modplot),Widget)
			
				ixp=widget%xpos-ixp_main-2
				iyp=widget%ypos-iyp_main-3*iypix-2
				
				call gmSetGuiGridMode(GON)
				call gmRemoveWindow(graph1_1(modplot))
			!	ixp=int(ixp/ixpix)
			!	iYp=int(iYp(modplot)/iypix)
			!	call gmRemoveWindow(graphics1_1(modplot))
				ipos0=ipos
				call gmActivateGraphicsFrame(graphics1_1(modplot))
				jplot=oldrecords(modplot)%iplot
				call graph1(modplot,jplot,main,ixp,iyp,graph1_1,GraphMainPanel1_1,graphics1_1,&
					wxmin,wxmax,wymin,wymax,ipos,gfile,.false.,ixposv,iyposv)
			
				ipos=ipos0

				call draw_axis(xmin,xmax,ymin,ymax,ymin1,ymax1,xtic,ytic,&
					nx1,ny1,xcross,ycross,itx,ity,tlenx,tleny,logx,logy,sqrty,doframe,&
					icol,ntx,nty,idev)
				call draw_text(parval,xtitle,ytitle,ztitle,title1,cnumx,cnumy,&
					cnumz,cexpx,cexpy,cexpz,newtext,numbx,numby,numbz,ntext,inumx,&
					inumy,logx,logy,intx,angle,sizetext,ijus,thick,rx,ry,idraw,icol,ifnt,str,dxs,dys,idev)
				if(hdisp) then
					call draw_hist(xval,yval,icurvd,ncurvd,ndelt,logity,logx,&
						logy,sqrty,y0,yinf,x0,ilog,xmin,xmax,ymin,ymax,idev,ndat,ijoin,&
						icol,thick,ndv1,ndimd,xwbase,lt2)
								
                else
					call draw_data(xval,yval,icurvd,ncurvd,ndelt,logity,logx,logy,sqrty,&
						y0,yinf,x0,ilog,idev,ndat,ijoin,icol,isym,&
						symsiz,thick,barleng,interp,npint,Xint,Yint,nint,yp1,ypn,&
						Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw)
				endif
				call draw_curves(xcal,ycal,icurvc,ncurvc,logity,logx,&
					logy,sqrty,y0,yinf,x0,ilog,idev,&
						wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
						xmin,xmax,ymin,ymax,ncal,&
					iline,icol,thick,ndc1,ndimc,jfirst)
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
		 case(403) !no of sets
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
						jmiss(nset)=0
					enddo
					callid=410
					ifstat(1)=0
					goto 2
				endif
			ENDIF
		case(411:430)
			ikset(iks)=callid-410
			iks=iks+1
			inkset=ikset(1)
        case(410)
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
				Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw)
			
		
        case(601)
			call about(Main,Form12,'Hjcfit',program_type)
		case(602)
		 call helpcvfit(main,indlist,indlista,list,nlist,idkbutt,idsbutt,602)
	case(610)
		 call helpcvfit(main,indlist,indlista,list,nlist,idkbutt,idsbutt,610)
			case(611)
		 call helpcvfit(main,indlist,indlista,list,nlist,idkbutt,idsbutt,611)
		case(603)
	   ISTAT1=0
	   DO KI=1,Nlist
           CALL LISENQ(INDLISTA,KI,LIST(KI),ISTAT1)
           IF(ISTAT1.EQ.2)THEN  ! SELECTED :
		   call texthelp(main,ihelp,ki,helps,nhelp,program_type)
           goto 1
	     ENDIF
	   ENDDO
	   GOTO 1
    case(613)
	
	   
	call helpcvfit(main,indlist,indlista,list,nlist,idkbutt,idsbutt,613)
	case(614)
	ISTAT12=0
	   DO KI=1,Nlist
           CALL LISENQ(INDLISTA,KI,LIST(KI),ISTAT2)
           IF(ISTAT2.EQ.2)THEN  ! SELECTED :
		   call mechelp(main,ihelp1,ki,0)
           goto 1
	     ENDIF
	   ENDDO
	   case(615)
	   ISTAT13=0
	   DO KI=1,Nlist
           CALL LISENQ(INDLISTA,KI,LIST(KI),ISTAT2)
           IF(ISTAT2.EQ.2)THEN  ! SELECTED :
		   call mechelp(main,ihelp1,ki,615)
           goto 1
	     ENDIF
	   ENDDO
	   	
	CASE(604)
	    CALL WINREM(INDLIST)
	    CALL ACTWIN(0)
	    GOTO 1
	
		case(711:720)
		 
		
				jcset=callid-710
				jset=jcset
				ikset(1)=callid-710
				do i=1,nset
					noguess(i)=.true.
				enddo
			  	noguess(ikset(1))=.false.
		case(721)
		if(ifitmode.eq.2.and.sameq.eq..false.) call gmerasewidget(itoggle)
		isens=0
		super=.false.
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
				call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xobs,yobs,w,nj,niobs,njset,nplot,nset,&
				juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
				wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
				ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
				xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,&
				oldrecords,dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,&
				parval,ifitype)
						
		call fitting_mode(ifitmode,nset,niobs,njset,ndimc,xobs,yobs,w,ncalc,printed,iflag,&
			dcurve,nodata,shortprt,inkset,ncurvc,ncal,icurvc,jfix,noguess,fixset,fitted,&
			sameq,sep,nj,juse,ikset,iver,title1(1:60),nsfit)
			if(icfit(modplot).eq.1) then
			imessy=gmDisplayMessageBox('','superimpose',gquestion,Gyesno)
			
			endif
			if(imessy.eq.gyesbutton) then 
			super=.true.
			else
				do i=150,200
					icolsave(i)=0
				enddo
				super=.false.
				call draw_curves(xcal,ycal,icurvc,ncurvc,logity,logx,&
     		logy,sqrty,y0,yinf,x0,ilog,idev,&
				wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
						xmin,xmax,ymin,ymax,ncal,&
     		iline,icolsave,thick,ndc1,ndimc,jfirst)
			endif
			call equations(Main,Eqs,EqsMainPanel,Toggle2,titmod)
		case(751:780)  ! equations
			if(readrec.and.ifiltype.eq.1) then
			call gmPopWindow(graph1_1(modplot))
			IMOD=CALLid-750
			goto 1
			else
			imessy=gmDisplayMessageBox('Info','No record on screen/Not available for this record',Gexclamation,gok)
			endif
					
		case(750)
			if(readrec.and.ifiltype.eq.1) then
				call gmEraseWidget(Eqs)
				ifcall=750
				ncurvc0=ncurvc
				do i=1,20
					icurvc1(i)=icurvc(i)
					ncal(i)=ncalc
				enddo
				
				call select_equation(main,ITOGGLE,intoggle,itogbutton,imod,nmod,norm,titlep,ptitle,iuse,icuse,kmax,&
				ncomp,ip1,ip2,logyfit,fline,nodata,ifcall,njset,ifitmode,nsfit,iw,&
				nset,mset,t1,titmod,titw,juse,jmiss)
			!	if(ifcall.eq.785) then
			!!
			 if(ifcall.eq.789) then
					callid=ifcall
					goto 2
				endif
			else
				imessy=gmDisplayMessageBox('Info','No record on screen/Not available for this record',Gexclamation,gok)
			endif
			goto 1	
		
		case(786:789) ! 
			iopt=callid-785
			if(callid.eq.789) then
				callid=785
				ierase=-9
				goto 2
			endif 
        case(785)
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
			call get_guesses(main,ITOGGLE,iopt,Xobs,Yobs,nj,juse,niobs,njset,&
			theta,ndth,thetgues,nset1,ybarp,ndimc,&
			setx,jfix,nfix,sepmax,guesdone,noguess,kfix,&
			nodata,sameq,sep,jcset,mset,ikset,iflag,callid,&
			xcal,ycal,ncal,ndc1,ncalc,idatg,pdata,intoggle,itogbutton,&
			autosep,hillang,titlep,idev,iuse)
		  if(callid.eq.791) goto 2
        case(790,791) ! 1055(initial guesses)+524(790) 791=continue,790 no questions
			if(callid.eq.791) then
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
		    titlep,ndc1,ncalc,xcal,ycal,iw,titled,ncurvc,icurvc,ncal)
			call gmActivateGraphicsFrame(graphics1_1(modplot))
			if(icfit(modplot).eq.1.and.super) then
				do i=1,ncurvc
					iline(i)=iline(i)+1
				enddo
			endif
		
			call draw_curves(xcal,ycal,icurvc,ncurvc,logity,logx,&
     		logy,sqrty,y0,yinf,x0,ilog,idev,&
				wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
						xmin,xmax,ymin,ymax,ncal,&
     		iline,icol,thick,ndc1,ndimc,jfirst)
			nl=nblank1(parval)

			if(nl.gt.0) call write_string(parval,oldrecords(modplot)%attributes%rx(2),&
			oldrecords(modplot)%attributes%ry(2),oldrecords(modplot)%attributes%angle(2),&
			oldrecords(modplot)%attributes%ijus(2),&
			oldrecords(modplot)%attributes%ifnt(2),oldrecords(modplot)%attributes%sizetext(2),&
			oldrecords(modplot)%attributes%icol(2),oldrecords(modplot)%dxs,oldrecords(modplot)%dys,idev)

			icfit(modplot)=1
			oldrecords(modplot)%ncurvc=ncurvc
				oldrecords(modplot)%STR(2)=PARVAL

			if(ncurvc.gt.0) then
			do i=1,ncurvc
				oldrecords(modplot)%icurvc(i)=icurvc(i)
				oldrecords(modplot)%ncal(i)=ncal(i)
				oldrecords(modplot)%iline(i)=iline(i)
				do j=1,ncal(i)
					oldrecords(modplot)%xcal(j,i)=xcal(j,i)
					oldrecords(modplot)%ycal(j,i)=ycal(j,i)
				enddo
			enddo
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
		case(792:899)
		
			jfix(callid-791)=1
			nfix=nfix+1
		case(900) ! enter new file with records
			
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
			    ixd=k-1
				iyd=k-1
				call values_list(Main,Data_list,ixd,iyd,ValArray6,button6,j,iplot,ncols,nrows,xdata,&
				stitle(j,iplot),niobs,njset,nplot,mtitle(j,iplot),static6,numset(iplot),&
				nwfile,newfile)
		 	  enddo
			enddo
		

		  case(1501:1999) ! change values or read new values for newfile
		  if((readrec.or.newfile).and.oldrecords(modplot)%IPOS.ne.4.and.ifiltype.eq.1) then
		
			if(callid.ge.1501.and.callid.le.1600) then
			 modplot=callid-1500
			 isvpl=1
			else if(callid.ge.1601.and.callid.le.1700) then
			 if(newfirst.ne.-1) then
			 newfile=.true. 
			 saveandplot=.true.
			 endif
			 isvpl=1
			 modplot=callid-1600
			 
			else if(callid.ge.1701.and.callid.le.1800) then
				modplot=callid-1700
				isvpl=6
				
			else if(callid.ge.1801.and.callid.le.1900) then
				modplot=callid-1800
				isvpl=6
				newfile=.true.
				saveandplot=.true.
			else if(callid.ge.1901.and.callid.le.1999) then
			 modplot=callid-1900
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
						call gmEnqCellSetting(ValArray6(modplot,k), 1,i,xdata(modplot,k,1,i),celltitle)
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
						xdata(modplot,k,i,j),celltitle)
					enddo
				enddo
			enddo
					
			n1=1
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
					setvar,ilabel,iunit,fopen_11,fope_12)
				endif
			endif
			if(newfile) then
				if(allocated(xval)) deallocate(xval,yval)
				if(allocated(xcal)) DEALLOCATE(Xcal,Ycal)
				if(allocated(icurvw)) DEALLOCATE(icurvw)
				if(allocated(theta)) DEALLOCATE(theta)
				if(allocated(thetgues)) DEALLOCATE(thetgues)
				if(allocated(ndat)) DEALLOCATE(ndat,isym,ijoin,symsiz)
				if(allocated(ncal)) DEALLOCATE(ncal,iline)
				if(allocated(icurvd)) DEALLOCATE(icurvd,icurvc)
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
				
				call calc_default(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,symsiz,&
				ndimd,XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,&
				XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,&
				XLO1,XHI1,YLO1,YHI1,y0,yinf,inumx,inumy,&
				titlex,titley,ilabel,doframe,autplt,itit,title1,&
				ISHP,ifnt,landscap,ndv1,ndc1,w,kwi,kwj,icurvw,kmax,iver,&
				redrawn,plot,redo,pcgm,pwmf,npint,hdisp,n1,tlenx,tleny,nx1,ny1,izoom,&
				xtitle,ytitle,rescale,dxs,dys,ifitype,parval,theta,ncomp)
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
				jplot=oldrecords(modplot)%iplot
				call graph1(modplot,jplot,main,ixp,iyp,graph1_1,GraphMainPanel1_1,graphics1_1,&
								wxmin,wxmax,wymin,wymax,ipos,gfile,plot,ixposv,iyposv)
							
				call draw_axis(xmin,xmax,ymin,ymax,ymin1,ymax1,&
							    xtic,ytic,nx1,ny1,xcross,ycross,itx,ity,tlenx,tleny,&
								logx,logy,sqrty,doframe,icol,ntx,nty,idev)
				call draw_text(parval,xtitle,ytitle,ztitle,title1,cnumx,cnumy,&
								cnumz,cexpx,cexpy,cexpz,newtext,numbx,numby,numbz,ntext,inumx,&
								inumy,logx,logy,intx,angle,sizetext,ijus,thick,rx,ry,idraw,&
								icol,ifnt,str,dxs,dys,idev)
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
				Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw)
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
				Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw)
			endif
			
		endif
			
		endif
		case(1101:1200,1301:1400)   ! move
		  if(readrec) then
		    if(callid.le.1200) then
			if(oldrecords(modplot)%IPOS.eq.0) then
				modplot=callid-1100
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
            else
				modplot2=callid-1300
				call gmActivateGraphicsFrame(graphics1_2(modplot2))
				lframe=graphics1_2(modplot2)
				imodel=nrmodel(modplot2)
				istate=models(imodel)%n
			endif
			CALL ACTENQ(CALLid,IDENT,IKEY,ISTATmove,Xac,Yac,acTEXT,nacargs,ARGS,IARGS)
			xtrue=xac
			ytrue=yac
			xmov=xtrue
			ymov=ytrue
			if(callid.le.1150) then
			if(logx) xtrue=10**(xac)
			if(logy) ytrue=10**(yac)
			if(sqrty) ytrue=(yac)**2
			endif
			call valtyp(1,0)
			
			CALL REALTOCH(XTRUE,CXTRUE,11)
			CALL REALTOCH(YTRUE,CYTRUE,11)
			call gmSetStatusBarText(Status_bar1,2,CXTRUE)
			call gmSetStatusBarText(Status_bar1,3,CYTRUE)
 
			call valtyp(0,0)
			if(callid.le.1200.and.s_text.and.lframe.eq.graphics1_1(modplot)) then
				call show_boxes(xmov,ymov,mm,str,rx,ry,angle,ijus,sizetext,icol,itemp,&
				idraw,inewpos,dxs,dys)
			else if (istate.ge.1.and.lframe.eq.graphics1_2(modplot2)) then
			!	call show_par_mechanism(modplot,xmov,ymov,models,ixp,iyp,pixbuf,show)
			endif
		!	xmov0=xmov 
		!	ymov0=ymov
		  ENDIF		
		  case(1201:1300,1401:1500)	   !select
		  
			if(callid.le.1300.and.readrec) then
				modplot=callid-1200
				call gmActivateGraphicsFrame(graphics1_1(modplot))
				lframe=graphics1_1(modplot)
				CALL gmEnqWidgetInfo(graph1_1(modplot),Widget)
				ixp=widget%xpos
				iyp=widget%ypos
				if(oldrecords(modplot)%IPOS.eq.0) then
				if(d_line.or.d_hline.or.d_vline.or.d_arrow.or.&
				d_poly.or.zoom.or.d_text.or.s_text) then
				do i=1,50
					xbeg(i)=oldrecords(modplot)%lines%xbeg(i)
					ybeg(i)=oldrecords(modplot)%lines%ybeg(i)
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
				callid=405
				goto 2
				endif
				endif
				endif
			else
				modplot2=callid-1400
				call gmActivateGraphicsFrame(graphics1_2(modplot2))
				lframe=graphics1_2(modplot2)
				IND_1=nrmodel(modplot2)
				jindex=ind_1
				call mouse_select(ind_1,npoint,temp,d_line, d_hline, d_vline, d_arrow, &
				d_poly, zoom,d_text, o_state,c_state, s_text,xmov,ymov,xmov0,ymov0,&
				imark,izoom,mm,itemp,inewpos,str,lframe,istate,ilink,link,delete_state,&
				move_state,models,ind_m,imove,ind_s,ind_e,mod_create,dxs,dys)
				if(o_state.or.c_state.or.delete_state) then
					delete_state=.false.
					o_state=.false.
				    c_state=.false.
					if(istatab(ind_1).eq.9) then
					call gmEnqWidgetInfo(Form1_TxtArray1(ind_1,1),widget)
					ivisual=widget%visual
				
					if(ivisual.ne.0) then
					do j=1,models(ind_1)%n
					call gmSetCellSetting(Form1_TxtArray1(ind_1,1), 1,j ,gmString=models(ind_1)%statname(j))
					rex=models(ind_1)%dgamma(j)
					call gmSetCellSetting(Form1_TxtArray1(ind_1,1), 2,j ,gmValue=rex)
					rex=models(ind_1)%inter_link(j)
					!arrayattribs%format=0
					!arrayattribs%ndp=0
					call gmSetCellSetting(Form1_TxtArray1(ind_1,1), 3,j ,gmValue=rex)
					call gmSetCellSetting(Form1_TxtArray1(ind_1,1), 4,j ,gmString=models(ind_1)%name_link(j))
					
					enddo
					call gmDrawwidget(Form1_TxtArray1(ind_1,1))
					endif
					endif
				endif
			endif
			
			
		
	case(2001:2100)
		call gmEnqActionState(actlst)	
		if(actlst%status.eq.2) jopen(callid-2000)=-1 ! window closed  
    case(2101:2600)	!state properties
		!	IND_1=nrmodel(modplot2)
		imodel=callid-2100
		link=.false.
		ncols=4
		nrows=ka+kf
		nrows=models(imodel)%n
		NLIG=models(imodel)%nLIG
		isw=1
		ihcell=24
		iwcell(1)=110
		iwcell(2)=100
		iwcell(3)=70
		iwcell(4)=140
		iwcell(5)=110
		do i=6,10
		iwcell(i)=110
		enddo
		!models(imodel)%indmod=.false.
		call gmsetprogressvalue(ipbar_Progress2,10)
		if(models(imodel)%indmod.eq..true.) then
		do i=1,models(imodel)%n
		!	if(models(imodel)%inter_link(i).gt.2) three_d=.true.
		enddo
		if(three_d) then
		else	
		do i=1,kcon
			models(imodel)%statname(i)=models(imodel)%con_states(i)
		enddo
			l=1+kcon
			do i=1,kstat0
				do j=1,kstat0
				cnum5=models(imodel)%sub_states(1,i)
				cnum51=models(imodel)%sub_states(2,j)
				n1=nblank1(cnum5)
				n2=nblank1(cnum51)
				!n1=n1-1
				!n2=n2-1
				if(nsub.gt.0) then
				models(imodel)%statname(l)=cnum5(1:n1)//'-'//cnum51(1:n2)
				else
				models(imodel)%statname(l)='         '
				endif
				l=l+1
				enddo
			enddo
		!do ind_m=1,models(imodel)%n
		!do l=1,10
		!if(models(imodel)%statname(ind_m)(l:l).eq.'*') models(imodel)%statname(ind_m)(l:l)='x'
		!enddo	
		!call write_string(models(imodel)%statname(ind_m),models(imodel)%X(ind_m)-0.1,&
		!	models(imodel)%y(ind_m)-0.5,0.,0,1,0.35,14,0.6,0.8,idev)
			
		!enddo
		
			models(imodel)%n=models(imodel)%kcon+models(imodel)%kstat0*models(imodel)%kstat0
			nrows=models(imodel)%n
			callid=6506
			goto 2	
	   endif
	   else
	   do i=1,nrows
		!	models(imodel)%statname(i)=' '	
		enddo
	   callid=6506
	   goto 2
	   endif
	
	
	case(6506)
	    if(	models(imodel)%nlig.eq.1) then
		ncols=5
		xtext='State name '//char(124)//'Conductance'//char(124)//'Nr links'//&
		char(124)//'Link to'//char(124)//models(imodel)%ligname(1)
			else if(models(imodel)%nlig.eq.2) then
		xtext='State name '//char(124)//'Conductance'//char(124)//'Nr links'//&
		char(124)//'Link to'//char(124)//models(imodel)%ligname(1)//&
		char(124)//models(imodel)%ligname(2)
		ncols=6
		else
		ncols=4
		xtext='State name '//char(124)//'Conductance'//char(124)//'Nr links'//&
		char(124)//'Link to'
	
		endif
		nrows=models(imodel)%n		
		
		   j=1
			do k=1,models(imodel)%n
				do l=k+1,models(imodel)%n
				if(models(imodel)%link(k,l).eq.1) then
				ic(1,j)=k
				ic(2,j)=l
				j=j+1
				endif
				enddo
			enddo
			
			
			NCON=(J-1)
			do m=1,ncon
				models(imodel)%ic(1,m)=ic(1,m)
				models(imodel)%ic(2,m)=ic(2,m)
	
			enddo
		
			models(imodel)%ncon=ncon
		
			models(imodel)%npar=npar
			if(models(imodel)%indmod) then
			  k=1
			  kstat0=models(imodel)%kstat0
			  kstat=models(imodel)%kstat
			  kcon=models(imodel)%kcon
			  npar0=2*(models(imodel)%kstat0-1)
			  npar=2*models(imodel)%kcon+models(imodel)%nsub*npar0
			  nsetq=2*npar
			  
			  models(imodel)%npar=npar
			  m=1
			  
			  do i=1,kstat0-1
					models(imodel)%ic(1,m)=models(imodel)%n-kstat0*i+1
				    models(imodel)%ic(2,m)=models(imodel)%n-kstat0*i+1-kstat0
					m=m+1
					
			  enddo
			  do i=1,kstat0-1
					models(imodel)%ic(1,m)=kcon+kstat0+1-i
					models(imodel)%ic(2,m)=kcon+kstat0-i
					m=m+1
					
			  enddo
			  do i=1,kcon
				do j=kcon+1,models(imodel)%n
				if (models(imodel)%link(i,j).eq.1) then
					models(imodel)%ic(1,m)=i
					models(imodel)%ic(2,m)=j
					m=m+1
				endif
				enddo
			 enddo

			endif
		
		call text_array(imodel,Main,form1,Form1_TxtArray1,ncols,nrows,iwcell,ihcell,xtext,isw,&
        models(imodel)%name_link,models(imodel)%inter_link,models(imodel)%statname,&
		models(imodel)%dgamma,models(imodel)%title_model,tij,models(imodel)%NBOUND,&
		models(imodel)%ligname,models(imodel)%nlig,ratcons(indrat)%micro,ratcons(indrat)%ligant)
		istatab(imodel)=9
		callid=2600+imodel
		goto 2
	case(2601:3100) ! rate
		link=.false.
		imod=callid-2600			    
		imodel=imod
		call gmsetprogressvalue(ipbar_Progress2,20)
		if(imod.lt.301.and.modify(imod).eq..false.) then
		lt=0
		do k=1,nrecs
			ratcons(k)%title=rtitles(k)
			ratcons(k)%imod=0
			ratcons(k)%irec=k
		enddo
		
		do k=1,nrecs
						
		if(ijmod(k).eq.imod) then
			lt=lt+1
			ratcons(k)%imod=imod
			text_tog(lt)=rtitles(k)
			indrec(lt)=k
		endif
		enddo
		if(lt.gt.0) then
			call toggle_panel(Main,ITOGGLE,lt,text_tog,intoggle,3200,valdat,idat,itogbutton)

		else
		!=============
		lt=1
		irecm0=irecq
		indrat=irecm0
	    ratcons(indrat)%imod=imod
		indrec(1)=indrat
			
	  call read_rates(Main,form1,Form1_TxtArray1,irecm0,imod,qfilem,nrows,indrat,&
	  ratcons,models,ic,ipbar_progress2)
		endif
		else
			imessy=gmDisplayMessageBox('','Did you fill the states properties tabel first ?',Gquestion,gyesno)
			if(imessy.eq.gyesbutton) then
			indr=indr+1
			indrat=300+indr	
	       ! ratcons(indrat)%imod=joldmodel
			ncols=5
			nrows=0
			nrows=2*models(imod)%n
			
			isw=2
			ihcell=24
			iwcell(1)=100
			iwcell(2)=100
			iwcell(3)=100
			iwcell(4)=100
			iwcell(5)=100
			do i=6,10
				iwcell(i)=100
			enddo
			
		
			j=1
			do k=1,models(imod)%n
				do l=k+1,models(imod)%n
				if(models(imod)%link(k,l).eq.1) then
				ic(1,j)=k
				ic(2,j)=l
				j=j+1
				endif
				enddo
			enddo
			
			
			NCON=(J-1)
			do m=1,ncon
				models(imod)%ic(1,m)=ic(1,m)
				models(imod)%ic(2,m)=ic(2,m)
	
			enddo
			npar=2*ncon
			models(imod)%ncon=ncon
			nrateq=2*ncon
			models(imod)%npar=npar
			!nsetq=2*npar
			ratcons(indrat)%nsetq=0
		
			NROWS=Npar
			
			k=1
			do m=1,ncon
				i=ic(1,m)
				j=ic(2,m)
				call intconv(i,cnum(1))
				call intconv(j,cnum1)
				nl=nblank1(cnum(1))
				nl1=nblank1(cnum1)
				ratcons(indrat)%qij(k)='q('//cnum(1)(1:2)//','//cnum1(1:2)//')'                                                        
				ratcons(indrat)%qij(k+1)='q('//cnum1(1:2)//','//cnum(1)(1:2)//')'
				if(nsetq.gt.0) then
				ratcons(indrat)%ieq(k)=0
				ratcons(indrat)%jeq(k)=0
				ratcons(indrat)%ifq(k)=i
				ratcons(indrat)%jfq(k)=j
				ratcons(indrat)%efacq(k)=1.
				
				ratcons(indrat)%ieq(k+1)=0
				ratcons(indrat)%jeq(k+1)=0
				ratcons(indrat)%ifq(k+1)=j
				ratcons(indrat)%jfq(k+1)=i
				ratcons(indrat)%efacq(k+1)=1.
				endif
					k=k+2
			enddo
			do m=1,npar
			ratcons(indrat)%ligant(m)='none'
			ratcons(indrat)%iconc(m)=0
			enddo
			if(models(imodel)%indmod.and..not.three_d) then
			  k=1
			  kstat0=models(imod)%kstat0
			  kstat=models(imod)%kstat
			  kcon=models(imod)%kcon
			  npar0=2*(models(imod)%kstat0-1)
			  npar=2*models(imod)%kcon+models(imod)%nsub*npar0
			  nsetq=2*npar
			  ratcons(indrat)%nsetq=nsetq
			 
			  nrows=npar
			  models(imodel)%npar=npar
			  m=1
			  ij=1
			  ik=(kstat0-1)*2
			  do i=1,kstat0-1
					ic(1,m)=models(imodel)%n-kstat0*i+1
				    ic(2,m)=models(imodel)%n-kstat0*i+1-kstat0
				!	call intconv(models(imodel)%n-kcon-kstat0*(i-1),cnum(1))
				!	call intconv(models(imodel)%n-kcon-(kstat0)*(i),cnum1)
					call intconv(models(imodel)%n-kstat0*i+1,cnum(1))
					call intconv(models(imodel)%n-kstat0*i+1-kstat0,cnum1)
					ratcons(indrat)%qij(k)='q('//cnum(1)(1:2)//','//cnum1(1:2)//')'                                                        
					ratcons(indrat)%qij(k+1)='q('//cnum1(1:2)//','//cnum(1)(1:2)//')'
					ratcons(indrat)%titlep(k)=ratcons(indrat-nsub)%titlep(ik)
					ratcons(indrat)%titlep(k+1)=ratcons(indrat-nsub)%titlep(ik-1)
					ratcons(indrat)%value(k)=ratcons(indrat-nsub)%value(ik)
					ratcons(indrat)%value(k+1)=ratcons(indrat-nsub)%value(ik-1)
					ratcons(indrat)%ligant(k)=ratcons(indrat-nsub)%ligant(ik)
					ratcons(indrat)%ligant(k+1)=ratcons(indrat-nsub)%ligant(ik-1)
					do j=1,kstat0-1
						ratcons(indrat)%ifq(j+(i-1)*nsub*(kstat0-1))=ic(1,m)
						ratcons(indrat)%jfq(j+(i-1)*nsub*(kstat0-1))=ic(2,m)
						ratcons(indrat)%ifq(j+(i-1)*nsub*(kstat0-1)+kstat0-1)=ic(2,m)
						ratcons(indrat)%jfq(j+(i-1)*nsub*(kstat0-1)+kstat0-1)=ic(1,m)
					enddo
					do j=1,kstat0-1
						ic(1,2*(kstat0-1)*j+kcon+m)=models(imodel)%n-kstat0*i+1+j
						ic(2,2*(kstat0-1)*j+kcon+m)=models(imodel)%n-kstat0*i+1-kstat0+j
						ratcons(indrat)%ieq(j+(i-1)*nsub*(kstat0-1))=models(imodel)%n-kstat0*i+1+j
						ratcons(indrat)%jeq(j+(i-1)*nsub*(kstat0-1))=models(imodel)%n-kstat0*i+1-kstat0+j
						ratcons(indrat)%ieq(j+(i-1)*nsub*(kstat0-1)+kstat0-1)=models(imodel)%n-kstat0*i+1-kstat0+j
						ratcons(indrat)%jeq(j+(i-1)*nsub*(kstat0-1)+kstat0-1)=models(imodel)%n-kstat0*i+1+j
					enddo
					m=m+1
					ik=ik-2
					k=k+2	
			  enddo
			  ik=(kstat0-1)*2
			  do i=1,kstat0-1
					ic(1,m)=kcon+kstat0+1-i
					ic(2,m)=kcon+kstat0-i
					call intconv(kcon+kstat0+1-i,cnum(1))
					call intconv(kcon+kstat0-i,cnum1)
					ratcons(indrat)%qij(k)='q('//cnum(1)(1:2)//','//cnum1(1:2)//')'                                                        
					ratcons(indrat)%qij(k+1)='q('//cnum1(1:2)//','//cnum(1)(1:2)//')'
					ratcons(indrat)%titlep(k)=ratcons(indrat-nsub+1)%titlep(ik)
					ratcons(indrat)%titlep(k+1)=ratcons(indrat-nsub+1)%titlep(ik-1)
					ratcons(indrat)%value(k)=ratcons(indrat-nsub+1)%value(ik)
					ratcons(indrat)%value(k+1)=ratcons(indrat-nsub+1)%value(ik-1)
					ratcons(indrat)%ligant(k)=ratcons(indrat-nsub+1)%ligant(ik)
					ratcons(indrat)%ligant(k+1)=ratcons(indrat-nsub+1)%ligant(ik-1)
					do j=1,kstat0-1
					ratcons(indrat)%ifq(j+(i-1)*nsub*(kstat0-1)+2*(kstat0-1)*(kstat0-1))=ic(1,m)
					ratcons(indrat)%jfq(j+(i-1)*nsub*(kstat0-1)+2*(kstat0-1)*(kstat0-1))=ic(2,m)
					ratcons(indrat)%ifq(j+(i-1)*nsub*(kstat0-1)+kstat0-1+2*(kstat0-1)*(kstat0-1))=ic(2,m)
					ratcons(indrat)%jfq(j+(i-1)*nsub*(kstat0-1)+kstat0-1+2*(kstat0-1)*(kstat0-1))=ic(1,m)
					enddo
					do j=1,kstat0-1
						ic(1,2*(kstat0-1)*j+kcon+m)=kcon+kstat0+1-i+kstat0*j
						ic(2,2*(kstat0-1)*j+kcon+m)=kcon+kstat0-i+kstat0*j
						ratcons(indrat)%ieq(j+(i-1)*nsub*(kstat0-1)+2*(kstat0-1)*(kstat0-1))=&
						kcon+kstat0+1-i+kstat0*j
						ratcons(indrat)%jeq(j+(i-1)*nsub*(kstat0-1)+2*(kstat0-1)*(kstat0-1))=&
						kcon+kstat0-i+kstat0*j
						ratcons(indrat)%ieq(j+(i-1)*nsub*(kstat0-1)+kstat0-1+2*(kstat0-1)*(kstat0-1))=&
						kcon+kstat0-i+kstat0*j
						ratcons(indrat)%jeq(j+(i-1)*nsub*(kstat0-1)+kstat0-1+2*(kstat0-1)*(kstat0-1))=&
						kcon+kstat0+1-i+kstat0*j
					enddo
					m=m+1
					ik=ik-2
					k=k+2	
			  enddo
			  
			  ksum=k-1
			  do i=1,kcon
				do j=kcon+1,models(imodel)%n
				if (models(imodel)%link(i,j).eq.1) then
					ic(1,m)=i
					ic(2,m)=j
					m=m+1
					call intconv(i,cnum(1))
					call intconv(j,cnum1)
					ratcons(indrat)%qij(k)='q('//cnum(1)(1:2)//','//cnum1(1:2)//')'                                                        
					ratcons(indrat)%qij(k+1)='q('//cnum1(1:2)//','//cnum(1)(1:2)//')'
					k=k+2
				
				endif

				enddo
			  enddo
			  ksum1=k-1
			  nrows=ksum1
          	  ncon=kcon+nsub*kstat0*(kstat0-1)
			  do m=1,ncon
				models(imodel)%ic(1,m)=ic(1,m)
				models(imodel)%ic(2,m)=ic(2,m)
			  enddo
			  do j=1,nrows
				if (ratcons(indrat)%ligant(j).eq.'none') then
					ratcons(indrat)%iconc(j)=0
				else
				do k=1,models(imod)%nlig
					if(ratcons(indrat)%ligant(j).eq.models(imod)%ligname(k)) ratcons(indrat)%iconc(j)=k
				enddo
				endif
			  enddo
			  
			endif
			call generate_charmod(imodel,models,ilast,jlast,1)
			  do i=1,ilast
				write(7,671) (models(imodel)%charmod(i,j),j=1,jlast)
671				format(4x,35a2)
			  enddo
			
111			continue
			if(models(imodel)%indmod) then
				ratcons(indrat)%ncyc=0
				callid=3511
				goto 2
			else
				ncyc=0
				ratcons(indrat)%ncyc=0
				ncon=models(imod)%ncon
			

				callid=3517
				goto 2
			endif
			imes0=gmDisplayMessageBox('','Calculate cycles old metod?',Gquestion,gyesno)
			if(imes0.eq.gyesbutton) then
				imessy=gmDisplayMessageBox('','Calculate cycles automatically ?',Gquestion,gyesno)
				if(imessy.eq.gyesbutton) then
			
				n=1
				ncmax=4
				val(1)=ncmax
				vtext(1)='Max Nr of states in a cycle'
				call value_table(Main,ivwin,n,vtext,ival,val,3517)
			!call CYCQ(models(imod)%n,models(imod)%ncon,models(imod)%ic,&
			!ncyc,nsc,im,jm,ncmax)
			
				else
			
				n=1
				val(1)=ncyc
				vtext(1)='Number of cycles'
				call value_table(Main,ivwin,n,vtext,ival,val,3510)
				endif
			else
				callid=3511
				icalcyc=-1
				goto 2
			endif
			endif
		endif
		
	case(3521,3522)
			imod=imodel
			indk=indrat
			if(models(imod)%indmod.eq..true.) then
				imy=gmdisplaymessagebox(' ','No MR for this model',gexclamation,gstop)
				goto 1
			endif
			if(indrat.ne.0.and.imod.ne.0) then
			if (callid.eq.3521) then
				if(ratcons(indrat)%imod.eq.0)then
				imy=gmdisplaymessagebox(' ','Open rates table first',gexclamation,gstop)
				goto 1
				
				endif
			endif
			icalprevc=callid
			ncyc=ratcons(indrat)%ncyc
		
			imes0=gmDisplayMessageBox('','Calculate cycles old metod?',Gquestion,gyesno)
			if(imes0.eq.gyesbutton) then
			imessy=gmDisplayMessageBox('','Calculate cycles automatically ?',Gquestion,gyesno)
			if(imessy.eq.gyesbutton) then
				n=1
				ncmax=4
				val(1)=ncmax
				vtext(1)='Max Nr of states in a cycle'
				call value_table(Main,ivwin,n,vtext,ival,val,3517)
			else
		!	icalprevc=3521
				n=1
				val(1)=ncyc
				vtext(1)='Number of cycles'
				call value_table(Main,ivwin,n,vtext,ival,val,3510)
			endif
			else
				callid=3511
				icalcyc=-1
				goto 2
			endif
			endif
	case(3517)
		
			ncon=models(imod)%ncon
			do m=1,ncon
				ic(1,m)=models(imodel)%ic(1,m)
				ic(2,m)=models(imodel)%ic(2,m)
			enddo
			if(icalcyc.eq.-1)  then
				ncyc=ratcons(indrat)%ncyc
				do i=1,ncyc
					nsc(i)=ratcons(indrat)%nsc(i)
					do m=1,nsc(i)-1
					im(i,m)=ratcons(indrat)%icyc(i,m)
					jm(i,m)=ratcons(indrat)%icyc(i,m+1)
					enddo
					im(i,nsc(i))=ratcons(indrat)%icyc(i,nsc(i))
					jm(i,nsc(i))=ratcons(indrat)%icyc(i,1) 
				
				enddo
				goto 441
			endif
			!val(1)=gmenqvaluesetting(ival(1))
			!ncmax=int(val(1))
			!call gmRemoveWindow(ivwin)	
			ncyc=0
			ncmax=4
			call find_cyc(models(imod)%n,models(imod)%link,ncyc,im,jm,nsc,ncmax)
			
			
441			i6=0
			icalcyc=0
			do i=1,ncyc
				if(nsc(i).ne.4) i6=1
			enddo
			
			if(i6.eq.0) then
				call findcube()
			endif
			
			call GETREV(ncyc,nsc,im,jm,ncyc1,nsc1,im1,jm1,useini,nccub,automr1)
			if(ncyc.gt.0) then
			ratcons(indrat)%ncyc=ncyc
			ncyc1=ncyc
			goto 1111
			do i=1,ncyc	
			ratcons(indrat)%nsc(i)=nsc(i)
			enddo	
			do l=1,ncyc
		
			do m=1,nsc(l)-1
			ratcons(indrat)%icyc(l,m)=im(l,m)
			ratcons(indrat)%icyc(l,m+1)=jm(l,m)
			enddo
			ratcons(indrat)%icyc(l,nsc(l))=im(l,nsc(l))
			ratcons(indrat)%icyc(l,1)=jm(l,nsc(l))
			ratcons(indrat)%im(l)=ratcons(indrat)%icyc(l,1)
			ratcons(indrat)%jm(l)=ratcons(indrat)%icyc(l,2)
			enddo
			endif
				
1111		continue			
			if(ncyc1.gt.0) then
			ratcons(indrat)%ncyc=ncyc1
			
			do i=1,ncyc1	
			ratcons(indrat)%nsc(i)=nsc1(i)
			enddo	
			do l=1,ncyc1
		
			do m=1,nsc1(l)-1
			ratcons(indrat)%icyc(l,m)=im1(l,m)
			ratcons(indrat)%icyc(l,m+1)=jm1(l,m)
			enddo
			ratcons(indrat)%icyc(l,nsc1(l))=im1(l,nsc1(l))
			ratcons(indrat)%icyc(l,1)=jm1(l,nsc1(l))
			ratcons(indrat)%im(l)=ratcons(indrat)%icyc(l,1)
			ratcons(indrat)%jm(l)=ratcons(indrat)%icyc(l,2)
			enddo
			endif
		!	icalprevc=3521
			n=1
			val(1)=ncyc
			vtext(1)='Number of cycles'
			call value_table(Main,ivwin,n,vtext,ival,val,3510)
			
	case(3510)
			val(1)=gmenqvaluesetting(ival(1))
			ratcons(indrat)%ncyc=int(val(1))
			call gmRemoveWindow(ivwin)	
			m=5
			do i=1,50
			icmr(i)=50
			enddo
			if(ratcons(indrat)%ncyc.gt.0) then
				title_box(1)='Cycle'
				title_box(2)='Nr of states'
				title_box(3)='States in order'
				title_box(4)='Use MR'
				title_box(5)='i,j'
				!call value_table(Main,ivwin,ratcons(indrat)%ncyc,vtext,ival,val,3511)
				
				do i=1,ratcons(indrat)%ncyc
					call intconv(isetmr(i),cnum(1))
					text_box(i,1)=cnum(1)
					call intconv(ratcons(indrat)%nsc(i),cnum(1))
					text_box(i,2)=cnum(1)
					!call intconv(im(i,1),cnum5)
					call intconv(ratcons(indrat)%icyc(i,1),cnum5)
					ir1(i)=ratcons(indrat)%icyc(i,1)
					nt5=nblank1(cnum5)
					cnum(2)=cnum5
					text_box(i,3)=cnum5(1:nt5)
					do j=2,ratcons(indrat)%nsc(i)
					!	call intconv(im(i,j),cnum51)
						call intconv(ratcons(indrat)%icyc(i,j),cnum51)
						nt=nblank1(text_box(i,3))
						text_box(i,3)=text_box(i,3)(1:nt)//','//cnum51
					enddo
				!	call intconv(ratcons(indrat)%icyc(i,1),cnum(2))
					call intconv(ratcons(indrat)%icyc(i,2),cnum51)
					jr1(i)=ratcons(indrat)%icyc(i,2)
					nt5=nblank1(cnum(2))
					nt51=nblank1(cnum51)
					if(automr1(i)) then
						text_box(i,4)='auto'
						
						text_box(i,5)=' '
					else
						do k=1,i-1
							if(ir1(i).eq.ir1(k).and.jr1(i).eq.jr1(k).or.&
							ir1(i).eq.jr1(k).and.jr1(i).eq.ir1(k)) then
							do j2=2,nsc1(i)		!check ALL routes in current cycle, #m
								ir2=im1(i,j2)
								jr2=jm1(i,j2)
								do n1=1,i-1
									if(ir2.eq.ir1(n1).and.jr2.eq.jr1(n1).OR.&
     								ir2.eq.jr1(n1).and.jr2.eq.ir1(n1)) then
									badcyc(j2)=.true.	
									else
									badcyc(j2)=.false.
									endif
								
								enddo
								if(badcyc(j2).eq..false.) then
									ir1(i)=ir2
									jr1(i)=jr2
									call intconv(ir1(i),cnum(2))
									call intconv(jr1(i),cnum51)
									nt5=nblank1(cnum(2))
									nt51=nblank1(cnum51)
									icmr(i)=7
									text_box(i,4)='yes'
									text_box(i,5)=cnum(2)(1:nt5)//','//cnum51(1:nt51)
									text_box(i,3)=text_box(i,5)
									do m=1,nsc1(i)
										icyc(m)=im1(i,m)
									enddo
									call order_states(i,ir2,jr2,icyc,nsc1(i),ncyc,im1,jm1)
									nsc(isetmr(i))=nsc1(i)
									do m=1,nsc1(i)
										im(isetmr(i),m)=im1(i,m)
										jm(isetmr(i),m)=jm1(i,m)
									enddo
									do l=3,nsc1(i)
										call intconv(im1(i,l),cnum51)
										nt=nblank1(text_box(i,3))
										text_box(i,3)=text_box(i,3)(1:nt)//','//cnum51
									enddo
									goto 444
								endif
							enddo ! end j2
							
							icmr(i)=2
							done(isetmr(i))=.false.
							text_box(i,4)='none'
							text_box(i,5)=cnum(2)(1:nt5)//','//cnum51(1:nt51)
							
							endif		
						enddo		
						text_box(i,4)='yes'
						text_box(i,5)=cnum(2)(1:nt5)//','//cnum51(1:nt51)
						
					endif
444					continue
				enddo
			!	call table_box(main,ratcons(indrat)%ncyc,m,title_box,text_box,itext_box,3511)
				call cyc_array(imod,main,itwin1,ratcons(indrat)%ncyc,5,text_box,&
				itext_box,3511,icyc_form,icmr)

			else
				callid=3511
				goto 2
			endif
	case(3511)
	
	
		if(ratcons(indrat)%ncyc.gt.0) then
			do j=1,models(imod)%npar
			ratcons(indrat)%micro(j)=' '	
			enddo
			do jq=1,nrows
					ratcons(indrat)%micro(jq)=' '
			enddo
			do i=1,ratcons(indrat)%ncyc
				ir=isetmr(i)
				do j=2,5	
					call gmEnqCellSetting(icyc_Form(imod), j,i,rval,text_box(i,j))
				enddo
				call chtoint(text_box(i,2),ratcons(indrat)%nsc(isetmr(i)))
				cnum0=text_box(i,3)
				nlc=nblank1(cnum0)
				k=1
				l=1
				ij=1

				do m=ij,nlc
					if(cnum0(m:m).eq.',') then
						cnum5=' '
						cnum5=cnum0(l:m-1)
						call chtoint(cnum5,num1)
						ratcons(indrat)%icyc(isetmr(i),k)=num1
						k=k+1
						l=m+1
					endif
				enddo		 
				cnum5=cnum0(l:nlc)
				call chtoint(cnum5,num1)
				ratcons(indrat)%icyc(isetmr(i),k)=num1
				nl1=nblank1(text_box(i,5))
				cnum51=text_box(i,5)(1:nl1)
				call chatonos(cnum51,num0,nk)
				
				if(text_box(i,4).eq.'yes') then
				do jq=1,nrows
				
					nl=nblank1(ratcons(indrat)%qij(jq))
					cnum5=ratcons(indrat)%qij(jq)(3:nl-1)
					call chatonos(cnum5,num,nk)
				
					if(num0(1).eq.num(1).and.num0(2).eq.num(2)) then
						ratcons(indrat)%micro(jq)='MR'
					endif
				enddo
				endif
	
			enddo
			call gmRemoveWindow(itwin1)
		
			
		else
			if(models(imodel)%indmod.eq..false.) then
			do j=1,nrows
			ratcons(indrat)%micro(j)=' '
			enddo
		
			endif
		endif
		ratcons(indrat)%ncyc=ncyc
		do i=1,ncyc
			ratcons(indrat)%nsc(i)=nsc(i)
			do j=1,nsc(i)
			ratcons(indrat)%icyc(i,j)=im(i,j)
			enddo
			ratcons(indrat)%im(i)=im(i,1)
			ratcons(indrat)%jm(i)=jm(i,1)
		enddo
		if(imodel.gt.300) then
		!	do j=1,nrows
		!		call gmSetCellSetting(form1_txtarray1(imodel,2), 6,j ,&
		!		gmString=ratcons(indrat)%micro(j))
		!		call gFlushGraphics()
		!	enddo
			icalprevc=0
		!	icalcyc=0
			rtitle=ratcons(indrat)%title
			do i=1,7
		!	iwcell(i)=
			enddo
			ncols=5
				isw=2
			ncols=5
			xtext1='i,j '//char(124)//'Rate name'//char(124)//'Rate value'//char(124)//&
			'Conc dep.'//char(124)//'Volt dep'
			if(.not.three_d) then	
			call text_array(imod,Main,form1,Form1_TxtArray1,ncols,nrows,iwcell,ihcell,xtext1,isw,&
			models(imod)%name_link,ratcons(indrat)%iconc,ratcons(indrat)%qij,ratcons(indrat)%value,&
			models(imod)%title_model,ratcons(indrat)%titlep,models(imod)%NBOUND,&
			models(imod)%ligname,models(imod)%nlig,ratcons(indrat)%micro,ratcons(indrat)%ligant)
			endif
		endif
	case(3501)
		if(modplot2.gt.0) then
			callid=nrmodel(modplot2)+2100
		goto 2
		else
		imessy=gmDisplayMessageBox('','No model on screen',Gexclamation,gok)
		
		endif
	case(3502)
	if(modplot2.gt.0) then
		callid=nrmodel(modplot2)+2600
		goto 2
		else
		imessy=gmDisplayMessageBox('','No model on screen',Gexclamation,gok)
		
		endif
	case(3503)
	if(modplot2.gt.0) then
		callid=nrmodel(modplot2)+4500
		goto 2
		else
		imessy=gmDisplayMessageBox('','No model on screen',Gexclamation,gok)
		
		endif
	case(3509)
	if(modplot2.gt.0) then
		callid=nrmodel(modplot2)+8500
		goto 2
		else
		imessy=gmDisplayMessageBox('','No model on screen',Gexclamation,gok)
		
		endif
	case(3201:3300)
	  lt=callid-3200
	case(3200)
	  indrat=indrec(lt)
	  imod=ratcons(indrat)%imod
	  irecm0=indrat
	call gmremovewindow(itoggle)
	  call read_rates(Main,form1,Form1_TxtArray1,irecm0,imod,qfilem,nrows,indrat,&
	  ratcons,models,ic,ipbar_progress2)
	case(4001:4500)
		imod=callid-4000
		CALL GUIPRT(0,ISTATus)
		IF(ISTATus.NE.0.and.imod.gt.0)THEN
			plot=.true.
			idev=6
			ipos=0
			imessy=gmDisplayMessageBox('','Black & White ?',Gquestion,gyesno)
			if(imessy.eq.gyesbutton) idev=6
			jgraph=models(imod)%jgraph
			call draw_model(jgraph,imod,main,graph1_2,GraphMainPanel1_2,graphics1_2,&
			models,plot,ipos,mod_create,idev)
			plot=.false.
			CALL GUIPRT(1,ISTATus)
			idev=0
		else
				imessy=gmDisplayMessageBox('','No model on screen',Gexclamation,gok)
		
			
		endif
	case(4501:5000)
		imod=callid-4500
		if(imod.gt.0) then
	!	if (ratcons(indrat)%title.eq.' ') then
	!		imessy=gmDisplayMessageBox('','You have no title for rate.Continue?',Gexclamation,gyesno)
	!		if(imessy.eq.gnobutton) goto 1
	!	endif
	imessy=gmDisplayMessageBox('','Did you SAVE ALL THE CHANGES ?',Gquestion,gyesno)
			if(imessy.eq.gyesbutton) then
		efilt='*.mec'//char(124)//'Mechs.mec'//char(124)//&
		'*.dat'//char(124)//'Old files'//char(124)//&
			   '*.*'//char(124)//'All Files'
		CALL gmFileBROWSER(eFILE,eDIR,eFILT,gmBrowseType=1)
		
				IF(eFILE.ne.' ') then
				
				call write_model(imod,models,indrat,ratcons,irecm0,efile,pfilem)

				endif
				efilt='*.plq'//char(124)//'Plot Queue (PLQ)'//char(124)//&
			   '*.*'//char(124)//'All Files'
			   ENDIF
		else
				imessy=gmDisplayMessageBox('','No model on screen',Gexclamation,gok)
			
		endif
	case(5001:5500)
		imod=callid-5000
		do j=1,	models(imod)%n	
		call gmEnqCellSetting(Form1_TxtArray1(imod,1), 1,j,rval,models(imod)%statname(j))
		call gmEnqCellSetting(Form1_TxtArray1(imod,1), 2,j,models(imod)%dgamma(j),actext)
		call gmEnqCellSetting(Form1_TxtArray1(imod,1), 4,j,rval,models(imod)%name_link(j))
		call gmEnqCellSetting(Form1_TxtArray1(imod,1), 3,j,models(imod)%inter_link(j),actext)
		!!! nbound?
		
		
		enddo 
		modplot2=models(imod)%jgraph
		call gmActivateGraphicsFrame(graphics1_2(modplot2))
		if(models(imod)%indmod) then
		    kstat=models(imod)%kstat
			kcon=models(imod)%kcon
			nsub=models(imod)%nsub
			kstat0=models(imod)%kstat0
			do j=1,models(imod)%kcon
				statname(j)=models(imod)%con_states(j)
			enddo
			do i=1,models(imod)%nsub
	
			do j=1,models(imod)%kstat0
				statname(kcon+j+kstat0*(i-1))=models(imod)%sub_states(i,j)
			enddo
			enddo
			if(nsub.eq.0)  then
				i=1
				do j=1,kstat-kcon
					statname(kcon+j)=models(imod)%sub_states(i,j)
				enddo
			endif
			do j=1,models(imod)%kcon	
			do l=1,10
		!		if(models(imod)%statname(j)(l:l).eq.'*') models(imod)%statname(j)(l:l)='x'
			enddo
			call write_string(models(imod)%statname(j)(1:6),&
			models(imod)%X(j)-0.1,&
			models(imod)%y(j)-0.2,0.,0,1,0.35,14,0.6,0.8,idev)
			call write_string(models(imod)%statname(j)(7:12),&
			models(imod)%X(j)-0.1,&
			models(imod)%y(j)-0.7,0.,0,1,0.35,14,0.6,0.8,idev)
		
			enddo	
			l=1+models(imod)%kcon
		
			do j=l,models(imod)%n
			do lk=1,10
		
			if(models(imod)%statname(j)(lk:lk).eq.char(45)) then
			i1=lk
			endif
			enddo
			if(i1.ne.0) then
			call write_string(models(imod)%statname(j)(1:i1-1),models(imod)%X(j)-0.1,&
			models(imod)%y(j)-0.2,0.,0,1,0.45,14,0.6,0.8,idev)	
			call write_string(models(imod)%statname(j)(i1+1:i1+4),models(imod)%X(j)-0.1,&
			models(imod)%y(j)-0.7,0.,0,1,0.45,14,0.6,0.8,idev)
			else
			call write_string(models(imod)%statname(j)(1:6),models(imod)%X(j)-0.1,&
			models(imod)%y(j)-0.2,0.,0,1,0.45,14,0.6,0.8,idev)	
			endif
			
			enddo	
		else
		do j=1,models(imod)%n
			
			statname(j)=models(imod)%statname(j)
			
			do l=1,10
		!		if(models(imod)%statname(j)(l:l).eq.'*') models(imod)%statname(j)(l:l)='x'
			enddo
			call write_string(models(imod)%statname(j)(1:6),&
			models(imod)%X(j)-0.1,&
			models(imod)%y(j)-0.2,0.,0,1,0.35,14,0.6,0.8,idev)
			call write_string(models(imod)%statname(j)(7:12),&
			models(imod)%X(j)-0.1,&
			models(imod)%y(j)-0.7,0.,0,1,0.35,14,0.6,0.8,idev)
		enddo
		endif
		
        	
		if(models(imod)%nlig.gt.0) then
		k=1
		do 	i=5,4+models(imod)%nlig
			do j=1,models(imod)%n
				call gmEnqCellSetting(Form1_TxtArray1(imod,1), I,j ,&
				models(imod)%nbound(j,k),actext)
			enddo
			K=K+1
		enddo
		endif
		call gFlushGraphics()

	case(5501:6000)
		imod=callid-5500
		
		do j=1,	nrows	
		call gmEnqCellSetting(Form1_TxtArray1(imod,2), 1,j,rval,ratcons(indrat)%qij(j))
		call gmEnqCellSetting(Form1_TxtArray1(imod,2), 2,j,rval,ratcons(indrat)%titlep(j))
		call gmEnqCellSetting(Form1_TxtArray1(imod,2), 3,j,ratcons(indrat)%value(j),actext)
	!	call gmEnqCellSetting(Form1_TxtArray1(imod,2), 5,j,ratcons(indrat)%iconc(j),actext)
		
		call gmEnqCellSetting(Form1_TxtArray1(imod,2), 4,j,rval,ratcons(indrat)%ligant(j))
		if (ratcons(indrat)%ligant(j).eq.'none') then
			ratcons(indrat)%iconc(j)=0
		else
			do k=1,models(imod)%nlig
				if(ratcons(indrat)%ligant(j).eq.models(imod)%ligname(k)) ratcons(indrat)%iconc(j)=k
			enddo
			
		endif
	!	if (ratcons(indrat)%iconc(j).ne.0) then
	!		k=ratcons(indrat)%iconc(j)
	!		ratcons(indrat)%ligant(j)=models(imod)%ligname(k)
	!		call gmSetCellSetting(Form1_TxtArray1(imod,2), 4,j ,gmString=ratcons(indrat)%ligant(j))
	 !   endif
		
		enddo
		do k=1,npar
!			titlep(k)=ratcons(indrat)%titlep(k)
		!	qt(irate(k),jrate(k))=ratcons(indrat)%value(k)
		enddo
		if(indmod) then
			do i=1,nsub
			enddo
		endif
		call gFlushGraphics()
	case(6001:6500)
		imod=callid-6000
		istatsel=gmDisplayMessageBox('',&
					'Not yes implemented',&
					Gexclamation,gok)
		goto 1
		do i=1,models(imod)%n
			call intconv(i,cnum1)
			textiq1(i)=cnum1(1:3)//'becomes :'
		enddo
			
			call text_table(Main,iqwin,'State number',models(imod)%n,textiq1,iqt1,textiqt1,6501)

						
	case(6501)
		do i=1,models(imod)%n
			call gmEnqTextSetting(iqt1(i),textiqt1(i))
			call chtoint(textiqt1(i),k)
			if(i.ne.k) then
			endif
		enddo

case(6502)
if(imodel.gt.0) then
	textiqt1(1)=' '
	
	textiqt1(1)=models(imodel)%title_model
	call text_table(Main,iqwin,'',1,'name:',iqt1,textiqt1,6503)
	endif
case(6504)
if(indrat.gt.0) then
		textiqt1(1)=' '
	textiqt1(1)=ratcons(indrat)%title
	call text_table(Main,iqwin,'',1,'rate:',iqt1,textiqt1,6507)
	
	endif
case(6503,6507)
	call gmEnqTextSetting(iqt1(1),textiqt1(1))
	if(callid.eq.6503) then
		models(imodel)%title_model=textiqt1(1)
	else
		ratcons(indrat)%title=textiqt1(1)
	
	endif
	call gmRemoveWindow(iqwin)
case(6505) 	   
	
	if(nsub.gt.0) then
		do i=1,nsub
		do j=1,kstat0
		call gmEnqTextSetting(isub(i,j),models(imodel)%sub_states(i,j))
		statname(kcon+j+kstat0*(i-1))=models(imodel)%sub_states(i,j)
		enddo
	    enddo
	endif
	l=1+kcon
	do i=1,kstat0
		do j=1,kstat0
				cnum5=models(imodel)%sub_states(1,i)
				cnum51=models(imodel)%sub_states(2,j)
				n1=nblank1(cnum5)
				n2=nblank1(cnum51)
				n1=n1-1
				n2=n2-1
				if(nsub.gt.0) then
				models(imodel)%statname(l)=cnum5(1:n1)//cnum51(1:n2)
				else
				models(imodel)%statname(l)='         '
				endif
				l=l+1
		enddo
	enddo
	if(nsub.eq.0) then
		do l=1+kcon,models(imodel)%n
			models(imodel)%statname(l)='         '
		enddo
	endif
	!kmodel=imodel
	!do i=1,nsub
	!	kmodel=kmodel+1
	!enddo
	callid=41
	goto 2
   case(7001:7500)
    joldgraph=callid-7000
	joldmodel=nrmodel(joldgraph)	
	!joldmodel=callid-7000
	jmodel=jmodel+1
	newmodel=jmodel
	igraph2=igraph2+1
	jopen2(igraph2)=1
!	ratcons(indk)%imod=joldmodel
	modplot2=igraph2
	
	models(newmodel)%jgraph=igraph2
	nrmodel(modplot2)=newmodel
	call copy_model(models,joldmodel,newmodel)
	models(newmodel)%title_model=' '
	n=1
	val(1)=models(newmodel)%nlig
	vtext(1)='Number of ligands'
	call value_table(Main,ivwin,n,vtext,ival,val,7502)
			
	case(7502)
	val(1)=gmenqvaluesetting(ival(1))
	models(newmodel)%nlig=val(1)
	call gmRemoveWindow(ivwin)	
	if(models(newmodel)%nlig.gt.0) then
				
				do i=1,models(newmodel)%nlig
					textiqt1(i)=models(newmodel)%ligname(i)
					call intconv(i,cnum1)
					textiq1(i)='Ligand'//cnum1(1:2)
				enddo
				call text_table(Main,iqwin,'Name for ligands',models(newmodel)%nlig,textiq1,iqt1,textiqt1,7501)
		
	else
		callid=7501
		goto 2
	endif
	case(7501)
	if(models(newmodel)%nlig.gt.0) then
	do i=1,models(newmodel)%nlig
	call gmEnqTextSetting(iqt1(i),textiqt1(i))
			ligname(i)=textiqt1(i)
			
			models(newmodel)%ligname(i)=ligname(i)
	enddo
		call gmRemoveWindow(iqwin)	
	endif 
	textiq1(1)='New Title'
	textiq1(2)='Model number'
	call text_table(Main,iqwin,' ',1,textiq1,iqt1,textiqt1,7503)
   CASE(7503)
   	
	call gmEnqTextSetting(iqt1(1),textiqt1(1))
	models(newmodel)%title_model=textiqt1(1)
	!call gmEnqTextSetting(iqt1(2),textiqt1(2))
	!call chatoint(textiqt1(2),imod1)
	mod_create=0
	call gmRemoveWindow(iqwin)	
	call draw_model(igraph2,newmodel,main,graph1_2,GraphMainPanel1_2,graphics1_2,&
		models,.false.,0,0,0)
	
	case(8001:8500)
		imod=callid-8000
		if(imod.gt.0) then
		CALL gmFileBROWSER(sFILE,sDIR,sFILT,gmBrowseType=1)
		if(sfile.ne.' ' ) then
		jgraph=models(imod)%jgraph
		ns=nblank1(sfile)
		if (sfile(ns-3:ns).eq.'.wmf') then
			idev=1
	   		pwmf=.true.
	   				
	   		idpi=600
	   		ixoff=0
	   		iyoff=0
	   		iwidi=4800
	   		ihei=3600
	   					
					!else if (sfile(ns-3:ns).eq.'.bmp') then
		else
	   		pbmp=.true.
	   		idev=2
	   				
	   	endif 
		call devsus  
		ipos=0
		plot=.true.
		if(pwmf) call wmfp(idpi,ixoff,iyoff,iwidi,ihei)
		if(pbmp) call bmp
		call devfil(sfile,0)
		call draw_model(jgraph,imod,main,graph1_2,GraphMainPanel1_2,graphics1_2,&
			models,plot,ipos,mod_create,idev)
		if(pwmf) then
						pwmf=.false.
						CALL DEVEND
      					CALL guiwin
		else if(pbmp) then
						pbmp=.false.
						CALL DEVEND
      					CALL guiwin
		endif	
	    idev=0
		plot=.false.
		endif
		else
				imessy=gmDisplayMessageBox('','No model on screen',Gexclamation,gok)
		
			
		endif
	case(8501:9000)
		imod=callid-8500
		if(imod.gt.0) then
		    nodata=.true.
			nofit=.true.
			autosim=.false.
			curvonly=.true.
			jgraph=models(imod)%jgraph
			textiq1(1)='Nr concentrations'
			textiq1(2)='Resolution '
			nset=1
			treso=0.
			!call intconv(nset,cum(1))
			!call realtoch(treso,cnum(2))
			textiqt1(1)='1'
			textiqt1(2)='0.0'
			call text_table(Main,iqwin,' ',2,textiq1,iqt1,textiqt1,9501)
		
		else
				imessy=gmDisplayMessageBox('','No model on screen',Gexclamation,gok)
		
		endif
	case(9501)
		do i=1,2
		call gmEnqTextSetting(iqt1(i),textiqt1(i))
		enddo
		call gmRemoveWindow(iqwin)	
		call chtoint(textiqt1(1),nset)
		call chtoreal(textiqt1(2),treso)
		!call getconc(conc,nset,nlig,ligname,readini,2)
		tresol(1)=treso	!keep (possibly altered) res in microsec for .ini
	    treso=treso*1.e-3	!ms
	    tresd(1)=dble(treso)*1.0d-3 	!same as tresg (but in seconds)
	    tresol(1)=1.e3*treso	!keep (possibly altered) res in mus for .ini
	    do j=2,nset
		tresol(j)=tresol(1)	!same for all at moment
		tresd(j)=tresd(1)
	    enddo
	    if(.not.(allocated(thtrue))) allocate(thtrue(1))	!not used?
	    nd1=1
	    nd2=1
	    if(.not.allocated(tint)) then
		ALLOCATE(tint(nd1,nd2),ampl(nd1,nd2),iprops(nd1,nd2))
	   endif
	!goto 330
  
   end select
  enddo

! Close device MENU and GINO, and end program
   call gmCloseMenu
   call gCloseDevice
   call gCloseGino

if(open7) close(unit=7)
deallocate(pixbuf)
deALLOCATE(xobs,yobs,w)
deALLOCATE(xval,yval)
deALLOCATE(xvalold,yvalold)
deALLOCATE(nj,jmiss,juse,ndat,ncal)	!for normalised data
deALLOCATE(setx,njbase,icurvd,icurvw,icurvc)
deallocate(isym,symsiz,ijoin,iline)
deallocate(xnum,xdata,ndatset)
	
deallocate(theta,theta1)
deALLOCATE(xcal,ycal)
deallocate(newrecords,oldrecords)
stop

end


