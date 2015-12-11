!     Last change:  D    17 Jan 104    1:09 pm
Program hjcfit
!	inifile: im2,jm2,nsc2,ncyc2
!   model: im,jm,ncyc,nsc
!   working : im1,jm1,ncyc1,nsc1
!c=====================================================================
!
!	xmin,xmax,ymin,ymax		- coordinates depending on scale
!	xmin1,xmax1,ymin1,ymax1	- absolute (arithmetic) coordinates
!	xmin2,xmax2,ymin2,ymax2	- fixed absolute (arithmetic) coordinates
!	xmin3,xmax3,ymin3,ymax3	- coordinates depending on scale
!
!????? look for logt!!!! 
!=====================================================================

! last mod -Popen curves !!! hjcdisp
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
!---------------------------------------------------------	
!	mechanism:
!   -----------------------
!   subroutine mechanism:
!		1001-1099		default mechanism as in readini+readmec(used:
!		1100 -	1199	create new mechanism(used:
!		1200 -	1299	read old mechanism(used:
!		1300 -	1399	View/Edit states properties(used: 
!		1400 -	1499	View/Edit rates properties (used:
!		2001-2100	   constraints (used:
!	2100- 2300		curve only
!	2301-2400      move mouse (used:
!   2401-2500      select mouse(used:
!
!---------------------------------------------------------

!	3001- 3300 READING HJCFIT FILES (SCN)
!	3301- 3500 simulate data
!   3501- 4000 stability plots
!-------------------------------------------
!	
!	4001- 5000 FITTING FOR HJCFIT:
!	
!	4001-4003 fix parameters
!	4005 write to ini file
!	4011-4500 subroutine fitting
!	4501-4600 skip fit -plot initial parameters
!   4601-5000 plot
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

 
	USE DFLIB
	use gino_f90
	use menu_f90

	include '\fortwin\common_files\cvfit_definitions.f90'
	include '\fortwin\common_files\graphics_definitions.f90'
	include '\fortwin\hjcfit\hjcfit_definitions.f90'
	
	TYPE (FILE$INFO) info

	type (GACTION) :: actlst
	type (Gwidget) :: widget
	character*11 cstring1,cstring2
	character*22 cstring0

	common/ptext/ameant(100),areat(100)
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

	common/plotopt/ipl1,ipl2,ipl3,ipl4,ipl5,ipl6,icprev
	common/iniset/nodata,nofit,autosim,curvonly
	COMMON/BLOCK2/castar,X1,X2,iequiv,ip1,ip2
	COMMON/BLOCK3/logyfit,norm,xnorm
	common/fix/fixratio,ir1,ir2,rval	!to fix ratio of 2 parameters
	common/potrat/jfirst,iset,kmax1	
	common/dp/discprt,append
	common/abt/abort
	common/pwrfunc/ybarp
	common/pixpos/ixgrid,iygrid,ixpix,iypix,wid,landscape
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
	common/fitblk/eigen,g00A,g10A,g11A,g00F,g10F,g11F		!from HJCLIK
	common/fitblk1/ampA,ampF
	common/cube/ncube,cubedef,cubecyc,cubext	!for getrev
	COMMON/MPAR/NCYC,NSC,IM,JM
	COMMON/QPAR/NCON,IC
	common/mr3/extcyc
	common/mr/obeymr,automr

	common/mr1/isetmr
	common/ir/irate(200),jrate(200)
!	common/np1/npar
	common /a/ mabel(100)		!common needed for CIRCUIT.for
    common /b/ matrix(100,100),n  !common needed for CIRCUIT.for
    COMMON/EBLK/NEQ,IE,JE,IF,JF,EFAC
	COMMON/indblk/nsetq,ieq,jeq,ifq,jfq,efacq(200) 
	common/ec/fixec50,nmod9,ec50,i50,j50,m50,prtec50,xqlo,xqhi,dcmodel	!nmod=imod0 (already in commom/model)
	common/ec2/monot,nlvar,vhold,vref,cfacd,dgamma !for ec50_hjc, qset_hjc
	common/ec3/ligname		!for qset_hjc
	
	common/ec4/nfixec50,nlvar2,ec502,i502,j502,m502,xqlo2,xqhi2	 !for 2nd fixed ec50
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
	
	common/mech_coord/ixm,iym,newmodel,oldmodel,nxmodel,nymodel,mtype,mod_create,&
	       ngraph2,modplot2,jgraph,ipos,lframe,isw,dxsm,dysm,jmodel,igraph0,newmr,newcons,irecfin(500)

	common/mech_graphics/form1,form1_txtarray1,graph1_2,GraphMainPanel1_2,graphics1_2,&
		jopen2,List7_1,ipbar_progress2,indwin,iqt1,textiqt1,iqwin,modelw,iwcell,&
		icellarray,textcell1,itoggle,lt,ipeny,ipeny_yes,ipeny_no,ipeny_xs,ipeny_fac,&
		icyc_form,itwin1,text_box,ivwin,button7,eqfit,Status_bar1,infolength,infopanel,&
		infoind,intoggle
common/details/textcomp,textid,drives
	common/mech_param/k,kf,nsub,kstat,ncon1,statname,icspec,icout,&
		jcon,ncin,nd1,nd2,nstate,ndim,iedge,jtree,jmic,irate2,jrate2,&
		neq0,neq1,nfix,kfit,fac,xs,&
		rtitles,iq,theta0,jfix
     common/ini_pag/ ini,ini_panel1,ini_panel1_1,ini_Text1_1,ini_Value2_1,ini_TxtArray2,&
	ini_Value3_1,ini_TxtArray3,ini_Value4_1 ,ini_Value4_2,ini_Value5_1,ini_TxtArray5, &
	ini_Value6_1 ,ini_Value6_3, ini_Value6_7 ,ini_Value6_8,ini_Value6_9, ini_TxtArray7,&
ini_Toggle1_1,ini_Toggle2_1,ini_Toggle3_1,ini_Toggle4_1,ini_Toggle4_2,ini_Toggle5_1,&
ini_Toggle6_1,ini_Toggle6_2,ini_Toggle6_4,ini_Toggle6_5,ini_Toggle6_7,ini_Toggle6_8,&
ini_Toggle7_1,ini_Toggle7_2,ini_Text7_1,ini_Value7_2,ini_Value7_3,ini_Value7_4,ini_TxtArray4_1,ini_TxtArray4_2 ,&
inipage,isetu,page_open 
ijplot=200                        
	filnam='hjcfit.txt'
	km=100	!in COMMON -dimension of QD, QT etc
	kAM=10	!max number of open states
	kFm=90	!max number of shut states
	nlvar=-1	!until defined
	nsims=1	!number of simulated expts fitted =1,2,...,nsim
	ireset=1	!for hjclik
	perfac=0.05d0	!maximum random perturbation
	ngpcheck=100
	excopen=.false.
	iresult = FULLPATHQQ ('hjcfit.exe', adir)
	nb=nblank1(adir)
	iniinifile=adir(1:nb-10)//'hjcini.ini'
	inifile=adir(1:nb-10)//'hjcfit.ini'
			pfilem=adir(1:nb-10)//filnam
			nodata=.false.
			curvonly=.false.
			nofit=.false.
			autosim=.false.
				discprt=.true.
			append=.false.
    INQUIRE (FILE=iniiniFILE,EXIST=PRESENT)
	if(PRESENT) then
			ihandle=FILE$FIRST
			length = GETFILEINFOQQ(iniinifile, info, ihandle)
			nLEN=info%length
			readini=.true.
			IRECL=10240
			OPEN(UNIT=19,FILE=iniiniFILE,&
            ACCESS='DIRECT',FORM='UNFORMATTED',RECL=irecl)
		    INQUIRE(IOLENGTH=LEN) iniiniFILE	
    	    if(nlen.gt.0) then
				read(19,rec=1) inifile,pfilem,nodata,curvonly,nofit,autosim,discprt,append
			else
			
				write(19,rec=1) inifile,pfilem,nodata,curvonly,nofit,autosim,discprt,append
			endif			
    else	
			IRECL=10240
			OPEN(UNIT=19,FILE=iniiniFILE,&
            ACCESS='DIRECT',FORM='UNFORMATTED',RECL=irecl)
			readini=.true.
			
			write(19,rec=1) inifile,pfilem,nodata,curvonly,nofit,autosim,discprt,append
    endif
	close(unit=19)


!	pfilem=adir(1:nb-10)//filnam
	pfilt='*.txt'//char(124)//'Print file (txt)'//char(124)//&
						  '*.prt'//char(124)//'Old print files'



pdir=adir(1:nb-10)
show_curve=.true.
INQUIRE (FILE=pfilem,EXIST=PRESENT)
	if(.not.PRESENT) then
		pfilem=adir(1:nb-10)//filnam
	endif
INQUIRE (FILE=inifile,EXIST=PRESENT)
	if(.not.PRESENT) then
		inifile=adir(1:nb-10)//'hjcfit.ini'	
	!	inifile=' '
	endif
	OPEN(UNIT=19,FILE=iniiniFILE,&
            ACCESS='DIRECT',FORM='UNFORMATTED',RECL=irecl)
			readini=.true.
			
			write(19,rec=1) inifile,pfilem,nodata,curvonly,nofit,autosim,discprt,append
close(unit=19)

	do i=1,100
		jopen2(i)=-1
	enddo
	program_type=3
	dfilt=dfilth
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
	do i=1,km
	   ucol(i,1)=1.d0
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
		ifont0=2
	
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
	ixm=16
	iym=4
	ix=96
	iy=48
	ALLOCATE(xobs(Niobs,NJSET),yobs(Niobs,NJSET),w(NIOBS,NJSET))
!	ALLOCATE(xval(NIOBS,NJSET),yval(NIOBS,NJSET))
	!ALLOCATE(xval1(NIOBS,NJSET),yval1(NIOBS,NJSET))
	ALLOCATE(xvalold(NIOBS,NJSET),yvalold(NIOBS,NJSET))
    ALLOCATE(nj(NJSET),jmiss(njset),juse(njset),ndat(njset),ncal(ndimc))	!for normalised data
    ALLOCATE(setx(njset),njbase(njset),icurvw(njset),icurvc1(njset))
!	allocate icurvd(njset),icurvc(njset))
!	allocate(isym(njset),symsiz(njset),ijoin(njset),iline(njset))
	!allocate(isym1(njset),isym1(njset),syms1(njset),ijoin1(njset),iline1(njset))
	allocate(xnum(nrows,3),xdata(10,10,4,10),ndatset(200,20))
!	allocate(newrecords(25),oldrecords(25),curves(25))
!	allocate(theta(100),thetgues(100,njset))
	allocate (theta1(100),pdata(100))  
!    ALLOCATE(xcal(ndc1,ndimc),ycal(ndc1,ndimc))
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
!	autosim=.false.
!	nofit=.false.
!	nodata=.false.
	liksurf=.false.
	prtec50=.false.
	penalty=.false.
	logfit=.false. 	!except when hjclik called from simplex

	curvonly=nofit.and.nodata
	onechan=.true.
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
   else if (ixgrid.ge.36.and.ixgrid.le.43) then
	ixng=40
	iyng=30
   else if (ixgrid.ge.44.and.ixgrid.lt.50) then
	ixng=36
	iyng=27
   else
	ixng=32
	iyng=24
   endif
  ixng=ixgrid
  iyng=iygrid
   call gmdefineguigrid(ixng,iyng)
 call gmenqguigrid(ixgrid,iygrid,ixpix,iypix)
 !  call gsetsoftchars()
  call gsethardchars()
   call gsetbrokenlinemode(gon)
	iwidth_main=40
	iheight_main=32
!	iwidth_main=ixgrid-1
!	iheight_main=iygrid-1
   call gSetEscapeChar('£')
   call define_colours(1,.true.)
   call gSetCharFont(ifont0)
   call gSetLineEnd(GROUND)
call gseterrormode(gerroron)
call main_window(program_type,Main,imainpanel,modelw,new_file,open_file,import_file, &
           isave_bmp,isave_wmf,export_file,print_file,exit_file,eqfit,view_record,&
		   view_data,title_record,iparameters,labels,&
		   jtitle_record,jparameters,jlabels,jnewtext,jnumbers,jaxis,jframe,&
		   jlines,jarrows,jraw_data,jcurves,label_x,label_y, &
		   label_z,number_x,number_y,number_z,Icon1_1,Icon1_2,Icon1_3,Icon1_4,&
		   Icon1_5,Icon1_6,Icon1_7,Icon1_8,Icon1_9,Icon1_10,Icon1_11,Icon1_12,&
		   Combo1_1,Combo1_2,Combo1_3,Combo1_4,Combo1_5,Combo1_6, combo1_8,combo1_9,combo1_10,&
		   toolbar1_1,toolbar1_2,toolbar1_3,Toolbar1_4,Status_bar1,&
		   new_text,new_lines,new_arrows,new_lines_v,new_lines_h,&
		   i3d,irot,iview3d,iaratxy,iarathb,igridd,isurfdr,cdate) 

  
		
		
		
   call ACTWIN(Main)	

   call gmManage
 
			ntog=10
			text_tog(1)='Fill model info'
			text_tog(2)='Display model'
			text_tog(3)='Fill/check states'
			text_tog(4)='Fill/check rates'
			text_tog(5)='MR+Constraints'
			text_tog(6)='Save model'
			text_tog(7)='Open file'
			text_tog(8)='Fit'
			text_tog(9)='Plot'
			text_tog(10)='Save plot'
ilg=ntog+2
iwidp=8
iwtog=6
itcall=7300
infolength=12
isetu=2
show_curve=.true.
if(isetu.eq.1) then
itogglepanel=gmCreatePanel(imainpanel,1 , 1, iwidp, ilg, &
              	gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmborderType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=150, gmFillBorder=0)
do i=1,ntog
inToggle(i) = gmCreateToggleButton(itogglePanel, 1, i, iwtog, 1, text_tog(i), istat11, &
              	gmType=G3DCHECKBOX, gmAccel=0, &
              	gmOncol=0, gmOffCol=0, gmTextCol=1, &
              	gmVpos=GTOP, gmExpand=GOFF)!, gmCallBAck=itcall+i)
enddo

infow=iwidth_main-19
infoh=ilg
infoy=1
infox=11
else
infow=iwidth_main-9
infoh=ilg-2
infoy=4
infox=1
Graphics_frame%xmin = 0.
   Graphics_frame%xmax = 1260.
   Graphics_frame%ymin = 0.
   Graphics_frame%ymax = 2.
!iprog_info = gmCreateGraphicsFrame(imainpanel, 1, 1,iwidth_main-2 , 2, Graphics_frame, &
!             	gmVpos=GTOP,gmhpos=gleft, gmExpand=GOFF)
do i=1,10
	infx=(i-1)*4
	info_pan(i)=gmCreatePanel(imainpanel, infx, 1, 4, 1,gmtitle=text_tog(i), &
              	gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmborderType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=150, gmFillBorder=0)
	itextinfo=gmCreateTextEntry(info_Pan(i), 0, 0, 4, 1,text_tog(i), 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=12, gmTextCol=1 , gmVpos=GTOP,gmsize=80)
enddo
endif

infopanel=gmCreatePanel(imainpanel, iwidth_main-14, 4, 14, iheight_main-7, &
              	gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmborderType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=150, gmFillBorder=0, gmScrollMode=Gvertical)

ittypanel=gmCreatePanel(imainpanel, 1, 4, iwidth_main-15, iheight_main-7, &
              	gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmborderType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

itty= gmCreateTTYEntry(ittypanel, 0, 0, iwidth_main-15, iheight_main-7, 100,'',&
				gmoffcol=1,gmoncol=1,gmtextcol=14) 
call gmsettextsetting(itty,'Display for simplex')

call welcome(main,initialw,initialw_MainPanel,inifile, initialw_Text5)
call windra(initialw)
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
	
		case(-10)
		initi=0
			call gmDefineKeyselectCallback(13,-11)
			call welcome(main,initialw,initialw_MainPanel,inifile, initialw_Text5)
			call windra(initialw)
		 
		case(-11,-12,-30) !(-40,-60) 
			
			call gmenqtextsetting(initialw_Text5,inifile)
			if(callid.eq.-30) then
			    inifile='hjcdemo.ini' 
				ni=nblank1(inifile)
				if(inifile(ni-10:ni).ne.'hjcdemo.ini') inifile(ni-10:ni)='hjcdemo.ini'	
			endif
			call gmRemoveWindow(initialw)
			
			call gmDefineKeyselectCallback(13,0)
		
			if(callid.eq.-11.or.callid.eq.-30) then
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
		neqold=0
		call read_ini(inifile,pfileb,nfix,jfix,neq,IE,JE,EFAC,&
        IF,JF,jcon,IM2,JM2,jmic,ndisp,irecq,ans1,ylo,yhi,nrange,idiskq,&
        nfileb,kfile,nset,conc,tcrit,burst,&
        idatyp,qfile,imodold,setbad,tcbad,onechan,&
         nbad1,isbad,iebad,tresolb,ans3,ans4,nlvar,&
         fixec50,ec50,i50,j50,m50,xqlo,xqhi,kAm,kFm,&
         chsvec,ncyc2,nsc2,qfilem,nsim,irect,logsav,imodolds,&
         badend,excop,gaplo,gaphi,dcmod,nlen,&
		 nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,useprim)
		 neqold=neq
		 nrange4=nrange
		 do i=1,20
			ylod(i)=ylo(i)
			yhid(i)=yhi(i)
		 enddo
		 	if(gaphi(1).eq.0.) then
				do j=1,nset
					gaplo(j)=tcrit(j)
					gaphi(j)=3.1536e10
				enddo
			endif     
		   useprim0=useprim
		   imodsav=imodold		!in case imodold changed in getqd
		   imod0=imodsav
		   if(irecq.lt.1) irecq=1		!temp!
				do j=1,nset
					tresol(j)=tresolb(j)
					do i=1,10
						conc1(i,j)=conc(i,j) !save
					enddo
				enddo
       	if(callid.eq.-30) then
		!	autosim=.false.
		!	nofit=.false.
		!	nodata=.false.
		!	liksurf=.false.
		!	curvonly=.false.
		!	discprt=.true.
		!	append=.false.
			if(open7) close(unit=7)
			OPEN(unit=7,file=PFILEm,status='UNKNOWN',&
				access='APPEND',form='FORMATTED',carriagecontrol='FORTRAN')
				if(.not.append) REWIND(unit=7)
				DISCPRT=.TRUE.

				open7=.true.
				if(discprt) write(7,91)  
				if(discprt) write(7,92) 
				if(discprt) write(7,93) cdate
			INQUIRE (FILE=qFILEm,EXIST=PRESENT)
			if(present) then
					readmec=.true.
					callid=-22
					goto 2
			else
				ista=gmDisplayMessageBox(qfilem,'Not present.Use menu bar',ginformation,gok)
				readini=.false.
				goto 1
			endif
		
		 endif
		else
			imodsav=0
			imod0=0
			readini=.false.
			ista=gmDisplayMessageBox('','No INI file. Try again (yes) ; Continue (no)?',Gquestion,gyesno)
			
			if(ista.eq.gyesbutton) then
				call welcome(main,initialw,initialw_MainPanel,inifile, initialw_Text5)
				goto 1 
			endif
		endif
		else
			readini=.false.
			readmec=.false.
			useprim0=.false.
			useprim=.false.
			useini=.false.
			imodold=0
			imodsav=imodold		!in case imodold changed in getqd
			imod0=imodsav
			
		endif
			call gmDefineKeyselectCallback(13,-21) !-10)
			call ini_file(main,initwin,initwin_Toggle3,initwin_Toggle4,&
			initwin_Toggle5,initwin_Toggle6,initwin_Toggle7,initwin_Toggle8,&
			    initwin_Toggle9,initwin_Toggle10,initwin_Toggle11,pfilem)
			

		case(-13) !(-50)
			
			CALL gmFileBROWSER(iniFILE,inipath,inidef,gmBrowseType=0)
			 IF(iniFILE.ne.' ') then
			 nl=nblank1(inipath)
			 inifile=inipath(1:nl)//'\'//inifile
			 call gmsettextsetting( initialw_Text5,inifile)
			 endif

		case(-14) !help
		
		case(-21,-4) !(-10)
			
		
			autosim=.false.
			nofit=.false.
			nodata=.false.
			liksurf=.false.
			curvonly=.false.
			istatus11=gmEnqToggleSwitch(initwin_Toggle3)
			istatus12=gmEnqToggleSwitch(initwin_Toggle4)
			istatus13=gmEnqToggleSwitch(initwin_Toggle5)
			if(istatus13.eq.gon) then 
				discprt=.false.
				append=.false.
			else if(istatus11.eq.gon) then 
				discprt=.true.
				append=.true.
			else if(istatus11.eq.gon) then 
				discprt=.true.
				append=.false.
			endif
			if(discprt) then
				if(open7) close(unit=7)
				OPEN(unit=7,file=PFILEm,status='UNKNOWN',&
				access='APPEND',form='FORMATTED',carriagecontrol='FORTRAN')
				if(.not.append) REWIND(unit=7)
				DISCPRT=.TRUE.

				open7=.true.
				if(discprt) write(7,91)  
				if(discprt) write(7,92) 
				if(discprt) write(7,93) cdate
91				 format(/,' Copyright D. Colquhoun, I. Vais, University College London 2004',/,&
     ' All rights reserved. (LF90/Gino version)',/,&
     ' Please cite: http://www.ucl.ac.uk/Pharmacology/dc.html')
	 
92	format(/, ' HJCFIT: Fit of model to open-shut times with missed events',/,&
     '  (Uses HJC distributions, exact for 1st 2 deadtimes then',/,&
     '  asymptotic, to calculate likelihood of record')

93	format(/,'Date of analysis=',a20)
			endif
			if(ibib.eq.-1)  then
			autosim=.false.
			nofit=.true.
			im=-1
			!call gmRemoveWindow(initwin)
			goto 112
			endif
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
112				continue					

			if(callid.eq.-4) then
			OPEN(UNIT=19,FILE=iniiniFILE,&
            ACCESS='DIRECT',FORM='UNFORMATTED',RECL=irecl)
		
			
			write(19,rec=1) inifile,pfilem,nodata,curvonly,nofit,autosim,discprt,append
    
			close(unit=19)
			goto 1
			else
			call gmRemoveWindow(initwin)
!			call prog_bar(main,ipbar,icho,ipbar_Progress2)
		
		
			call gmDefineKeyselectCallback(13,0)
	        call gmDefineKeyselectCallback(13,-50)
				do i=1,60
						pathdat1(i:i)=' '
					enddo
			if(readini) then
				INQUIRE (FILE=qFILEm,EXIST=PRESENT)
				if(present) then
					readmec=.true.
					callid=-22
					goto 2
				else
				
				ista=gmDisplayMessageBox(qfilem,'Not present.Change directory/file',ginformation,gok)
					readmec=.false.
				!call gmSetWidgetStatus(imecform1_Toggle1, GunSELECTABLE)
				
				iplen = FULLPATHQQ(qfilem, pathdat1)
				nbl=nblank1(qfilem)
				do i=1,nbl
					if(qfilem(i:i).eq.'\') ilp=i
				enddo
				filepure=qfilem(ilp+1:nbl)
				filepure='   '
				CALL gmFileBROWSER(filepure,qDIR,qFILT,gmBrowseType=0)
				if(filepure.ne.'              ') then
				INQUIRE (FILE=filepure,EXIST=PRESENT)
				if(present) then	
					callid=-22
					readmec=.true.
					qfilem=filepure
					goto 2
				endif
				endif
				ista=gmDisplayMessageBox(qfilem,'Not present.Use menu bar',ginformation,gok)
				readini=.false.
				goto 1
				iplen = FULLPATHQQ(filepure, pathdat1)
				INQUIRE (FILE=pathdat1,EXIST=PRESENT)
				if(present) then
					qfilem=pathdat1
				else
					CALL gmFileBROWSER(qfilem,qDIR,qFILT,gmBrowseType=0)
					do i=1,nbl
						if(qfilem(i:i).eq.'\') ilp=i
					enddo
					filepure1=qfilem(ilp+1:nbl)
					if(filepure.eq.filepure1) then
						iplen = FULLPATHQQ(filepure1, pathdat1)
			
						qfilem=pathdat1
					else
						readmec=.false.
						mtitle=' '
						qfilem=' '
						goto 111
					endif
				endif
				callid=-22
				readmec=.true.
				goto 2
			

				endif
			else
				readmec=.false.
				mtitle=' '
				qfilem=' '
			!	call gmSetWidgetStatus(imecform1_Toggle1, GunSELECTABLE)

			endif
111			call ini_mechanism(main,imecform1,mtitle,qfilem,im,imecform1_Toggle1,imecform1_Toggle2,&
			imecform1_Toggle3,imecform1_Toggle4,imecform1_Toggle5,imecform1_Toggle6,readmec)
		endif	
case(-22)
					call intconv(imodold,mtitle)
				
					i=1
					irc=i
					mod_create=-irecq
					newmr=.false.
					newcons=.false.
					ixm=-100
					do k=1,25
							if(jopen2(k).eq.-1) then
								igraph2=k
								goto 334
							endif
						    enddo
					igraph2=igraph2+1
334					jgraph=igraph2
					jopen2(igraph2)=1
					npar=20
						
					OPEN(unit=15,file=qfilem,status='UNKNOWN',access='DIRECT', form='BINARY', RECL=1)
					read(unit=15,rec=1) iver5,nrecs,nextrec,ireclast,jstart
					do irq=1,nrecs
						irec=jstart(irq)	!previous nextrec
						read(unit=15,rec=irec) iver5,imod,mtitle11,k,kA,kB,kC,kD,rtitle
					
						ijmod(irq)=imod
						rtitles(irq)=rtitle
	   
					enddo
						irec=jstart(irecq)
				
					read(unit=15,rec=irec) iver5,imod,mtitle11,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrateq,&
     				ncon,ncdep,nlig,chardef,boundef,ncyc,vref,nvdep,kmfast,&
     				indmod,npar
					close(unit=15)
				    inipage=-100
					call ini_page(main,npar,ixgrid,iygrid)
					call create_model(ixm,iym,igraph2,imodel,main,form1,form1_txtarray1,&
						graph1_2,GraphMainPanel1_2,graphics1_2,&
						nxmodel,nymodel,mtype,models,mod_create,irc,qfilem,text7(i),&
						dxs,dys,ijmod,ka,kf,ic,indrat,pfilem,open7,apfile,ipbar_progress2,indwin,ncdep)
						nrmodel(igraph2)=imodel
					ixm=1
					irecm0=irecq
					indrat=irecm0
					indk=indrat
					ratcons(indrat)%imod=imod
					indrec(1)=indrat
					ijmod(irecq)=imodel
					call read_rates(-1,form1,Form1_TxtArray1,irecm0,imod,qfilem,nrows,indrat,&
					ratcons,models,ic,ipbar_progress2,igraph2,nmr)
					npar=models(igraph2)%npar
							call gmsetvaluesetting(ini_Value5_1,nset)

					do i=1,60
						pathdat1(i:i)=' '
					enddo

					do j=1,nset
						do i=1,nfileb(j)
							nfile(j)=nfileb(j)
							INQUIRE (FILE=pfileb(i,j),EXIST=PRESENT)
							if(.not.present) then
								ista=gmDisplayMessageBox(pfileb(i,j),'Not present.Change directory/file',ginformation,gok)
			
								nbl=nblank1(pfileb(i,j))
								do k=1,nbl
									if(pfileb(i,j)(k:k).eq.'\') ilp=k
								enddo
								filepure(1:20)=pfileb(i,j)(ilp+1:nbl)
							    filepure='    '
								CALL gmFileBROWSER(filepure,DDIR,DFILT,gmBrowseType=0)
								if(filepure.ne.'              ') then
								pfileb(i,j)=filepure
								iplen = FULLPATHQQ(filepure, pathdat1)
								endif
								!if(iplen.ge.nbl-ilp) pfileb(i,j)=pathdat1
							endif
						call gmSetCellSetting(ini_TxtArray5, 1,j ,gmString=pfileb(i,j))
						pfiles(i,j)=pfileb(i,j)
						enddo
					enddo
					 call gmdrawwidget(ini_TxtArray5)
					
					
					
					indk=indrat		
					callid=-23
					goto 2

	case(-23)
				indk=indrat
				irecm0=indrat
				nbl=nblank1(qfilem)
				do i=1,nbl
					if(qfilem(i:i).eq.'\') ilp=i
				enddo
			!	filepure=qfilem(ilp+1:nbl)				
					call gmsettextsetting(ini_Text1_1,qfilem(ilp+1:nbl)//':'//models(igraph2)%title_model)
					!	call gmSetWidgetStatus(ini_text1_1, GunSELECTABLE)
					call gmsetvaluesetting(ini_Value2_1,nmr)
					!	call gmSetWidgetStatus(ini_value2_1, GunSELECTABLE)
					call gmsetvaluesetting(ini_Value3_1,neq)
					!	call gmSetWidgetStatus(ini_value3_1, GunSELECTABLE)
					call gmsetvaluesetting(ini_Value4_1,nfix)
					!	call gmSetWidgetStatus(ini_value4_1, GunSELECTABLE)
					call gmsetvaluesetting(ini_Value4_2,nfixec50)
					!	call gmSetWidgetStatus(ini_value4_2, GunSELECTABLE)
				
					call gmsettextsetting(ini_Text7_1,ratcons(indk)%title)
				!	call gmSetWidgetStatus(ini_text7_1, GunSELECTABLE)
					penfac=10.0d0
					xs=sngl(assmax)
					fac=sngl(penfac)
					xs1=sngl(ratemax)
					call gmsetvaluesetting(ini_Value7_3,xs)
					call gmsetvaluesetting(ini_Value7_4,xs1)



					!	call gmSetWidgetStatus(ini_value7_2, GunSELECTABLE)
					!		call gmSetWidgetStatus(ini_value7_3, GunSELECTABLE)
				
					imr=0
					ico=0
					ifi=0
				
					do j=1,models(igraph2)%npar
						call gmSetCellSetting(ini_TxtArray7, 1,j,gmString=ratcons(indk)%qij(j))
					
						call gmSetCellSetting(ini_TxtArray7, 2,j,gmString=ratcons(indk)%titlep(j))
						xvreal=ratcons(indk)%value(j)
						call gmSetCellSetting(ini_TxtArray7, 3,j,gmvalue=xvreal)
						if(nfix.gt.0) then
						if(jfix(j).eq.1) then
							ifi=ifi+1
							call gmSetCellSetting(ini_TxtArray4_1, 2,ifi ,gmString=ratcons(indk)%titlep(j))
							call gmSetCellSetting(ini_TxtArray4_1, 1,ifi,gmString=ratcons(indk)%qij(j))
						endif
						endif

					enddo
					
				!	nmr=0
					if(neq.gt.0) then
		
					do j=1,models(igraph2)%npar
				
					do m=1,neq
						if(irate(j).eq.ie(m).and.jrate(j).eq.je(m)) then
						ico=ico+1
						call gmSetCellSetting(ini_TxtArray3, 1,ico,gmString=ratcons(indk)%qij(j))
						call gmSetCellSetting(ini_TxtArray3, 2,ico ,gmString=ratcons(indk)%titlep(j))
						call gmSetCellSetting(ini_TxtArray3, 3,ico ,gmvalue=efac(ico))
					!	ratcons(indk)%efac(j)=efac(ico)
						do l=1,models(igraph2)%npar
							if(irate(l).eq.if(ico).and.jrate(l).eq.jf(ico)) then
								call gmSetCellSetting(ini_TxtArray3, 4,ico,gmString=ratcons(indk)%qij(l))
								call gmSetCellSetting(ini_TxtArray3, 5,ico ,gmString=ratcons(indk)%titlep(l))
							
							!	ratcons(indk)%titlec(j)=ratcons(indk)%titlep(l)
							endif
						enddo
						endif
					enddo
					
					enddo
					endif
				    nmr=0
					if(ncyc.gt.0) then
					do l=1,models(igraph2)%npar
					do j=1,ncyc
					isetmr(j)=j
					if(irate(l).eq.im(j,1).and.jrate(l).eq.jm(j,1)) then
						imr=imr+1
						call gmSetCellSetting(ini_TxtArray2, 1,imr,gmString=ratcons(indk)%qij(l))
					
						call gmSetCellSetting(ini_TxtArray2, 2,imr,gmString=ratcons(indk)%titlep(l))
						call intconv(im(j,1),cnum5)
					
						nt5=nblank1(cnum5)
						textbox=cnum5(1:nt5)
						do jo=2,nsc(j)
				
						call intconv(im(j,jo),cnum51)
						nt=nblank1(textbox)
						textbox=textbox(1:nt)//','//cnum51
						enddo
						call gmSetCellSetting(ini_TxtArray2, 3,imr,gmstring=textbox)
					!
						obeymr(j)=.true.
					    nmr=nmr+1
					endif
					enddo
					enddo
					endif
					
					if(nfixec50.gt.0)then
						do l=1,models(igraph2)%npar
						if(irate(l).eq.i50.and.jrate(l).eq.j50) then
							ieci=ieci+1
							call gmSetCellSetting(ini_TxtArray4_2, 1,ieci,gmString=ratcons(indk)%qij(l))
							call gmSetCellSetting(ini_TxtArray4_2, 1,ieci ,gmString=ratcons(indk)%titlep(l))
						endif
						enddo
					endif	
					
					call gmdrawwidget(ini_TxtArray3)
					call gmdrawwidget(ini_TxtArray2)
					call gmdrawwidget(ini_TxtArray7)
						call gmdrawwidget(ini_TxtArray4_1)
					call gmdrawwidget(ini_TxtArray4_2)
					call gFlushGraphics()
					call gmSetGuiGridMode(GOn)
			!	arrayattribs%display=gdisplay
			!		call gmSetCellAttribs(ini_TxtArray2, 1, 1, arrayattribs)
					callid=-24
					goto 2
			
		
		
		case(-24)
					
					if(onechan) call gmSetToggleSwitch(ini_Toggle6_2,Gon)
					if(chsvec(1)) call gmSetToggleSwitch(ini_Toggle6_4,Gon)
				    if(badend(1)) call gmSetToggleSwitch(ini_Toggle6_5,Gon)
					if(setbad(1,1).eq..true.) call gmSetToggleSwitch(ini_Toggle6_7,Gon)
					if(setbad(2,1).eq..true.) call gmSetToggleSwitch(ini_Toggle6_8,Gon)
					!	call gmSetWidgetStatus(ini_value5_1, GunSELECTABLE)
					call gmsetvaluesetting(ini_Value6_1,conc(1,1))
					!	call gmSetWidgetStatus(ini_value6_1, GunSELECTABLE)
					call gmsetvaluesetting(ini_Value6_3,tcrit(1))
					!	call gmSetWidgetStatus(ini_value6_3, GunSELECTABLE)
					call gmsetvaluesetting(ini_Value6_7,tcbad(1,1))
					!	call gmSetWidgetStatus(ini_value6_7, GunSELECTABLE)
					call gmsetvaluesetting(ini_Value6_8,tcbad(2,1))
					!	call gmSetWidgetStatus(ini_value6_8, GunSELECTABLE)
				call gmsetvaluesetting(ini_Value6_9,tresol(1))
				
				
				!	call gFlushGraphics()
					initi=1
					ns=1
					do i=1,7
						
						infx=(i-1)*4
						info_pan(i)=gmCreatePanel(imainpanel, infx, 1, 4, 1,gmtitle=text_tog(i), &
              			gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmborderType=GINVERSECHISEL, &
              			gmLineCol=0, gmLineBorder=0, gmFillCol=221, gmFillBorder=0)
						ict=4
					!	if(i.eq.2.or.i.eq.4.or.i.eq.5.or.i.eq.7) then
						itextinfo=gmCreateTextEntry(info_Pan(i), 0, 0, 4, 1,text_tog(i), 255, GDISPLAY, gmBack1Col=0, &
						gmBack2Col=12, gmTextCol=1 , gmVpos=GTOP,gmsize=80)
					!		if(i.eq.1.or.i.eq.3.or.i.eq.6) then
					!	itextinfo=gmCreateTextEntry(info_Pan(i), 0, 0, 4, 1,text_tog(i), 255, GDISPLAY, gmBack1Col=0, &
					!	gmBack2Col=12, gmTextCol=221 , gmVpos=GTOP,gmsize=80)
					!	endif
					enddo
					goto 1
		case(-25)
			call gmSetWidgetStatus(initwin_Toggle9, GSELECTABLE)
			call gmSetWidgetStatus(initwin_Toggle10, GSELECTABLE)

	   
		case(-26)
		!	call gmDefineKeyselectCallback(13,0)
			call gmSetWidgetStatus(initwin_Toggle9, GSELECTABLE)
			call gmSetWidgetStatus(initwin_Toggle10, GSELECTABLE)
	   
		case(-27)
		!	call gmDefineKeyselectCallback(13,0)
			call gmSetWidgetStatus(initwin_Toggle9, GUNSELECTABLE)
			call gmSetWidgetStatus(initwin_Toggle10, GUNSELECTABLE)
		case(-29)
			call gmDefineKeyselectCallback(13,0)
			call gmRemoveWindow(initialw)
			readini=.false.
			readmec=.false.
			useprim0=.false.
			useprim=.false.
			useini=.false.
			imodold=0
			imodsav=imodold		!in case imodold changed in getqd
			imod0=imodsav
			
	
			call gmDefineKeyselectCallback(13,-21) !-10)
			ibib=-1
			call ini_file(main,initwin,initwin_Toggle3,initwin_Toggle4,&
			initwin_Toggle5,initwin_Toggle6,initwin_Toggle7,initwin_Toggle8,&
			    initwin_Toggle9,initwin_Toggle10,-1,pfilem)
			
		case(-50) 
			call gmDefineKeyCallback(13,0)

		
			istatus1=gmEnqToggleSwitch(imecform1_Toggle1)
			istatus2=gmEnqToggleSwitch(imecform1_Toggle2)
			
			istatus3=gmEnqToggleSwitch(imecform1_Toggle3)
		
			call gmDefineKeyCallback(13,0)
	if(istatus1.eq.gon) samec=.true.
				call gmSetWidgetStatus(new_file, gSELECTABLE)
				call gmSetWidgetStatus(open_file, gSELECTABLE)
				call gmSetWidgetStatus(import_file, gSELECTABLE)
				do j=4,15
				call gmSetWidgetStatus(modelw(j), GSELECTABLE)
				enddo
		
			if(istatus3.eq.gon) then
				callid=1100 !35 !new
					call gmRemoveWindow(imecform1)
					imodsav=0
					imod0=0	
					inipage=0
				!	call prog_bar(main,ipbar,icho,ipbar_Progress2)
				goto 2
			else if(istatus2.eq.gon) then
					inipage=0
					callid=1200 !36  ! old
					call gmRemoveWindow(imecform1)
					imodsav=0
					imod0=0	
				!	call prog_bar(main,ipbar,icho,ipbar_Progress2)
				goto 2
            else if(istatus1.eq.gon) then !same 
				imodel=imodold
				call gmRemoveWindow(imecform1)
				
				callid=1001
				goto 2
		
				!		if(istatus1.eq.gon) samec=.true.
			endif
		case(-99)
		call gmSetToggleSwitch(ini_Toggle1_1, Goff)
		call gmSetToggleSwitch(ini_Toggle2_1, Goff)
		call gmSetToggleSwitch(ini_Toggle3_1, Goff)
		call gmSetToggleSwitch(ini_Toggle4_1, Goff)
		call gmSetToggleSwitch(ini_Toggle4_2, Goff)
		call gmSetToggleSwitch(ini_Toggle5_1, Goff)
		call gmSetToggleSwitch(ini_Toggle6_1, Goff)
		call gmSetToggleSwitch(ini_Toggle7_1, Goff)
			call gmSetToggleSwitch(ini_Toggle7_2, Goff)
	case(-100)
		call gmSetToggleSwitch(ini_Toggle1_1, Gon)
		call gmSetToggleSwitch(ini_Toggle2_1, Gon)
		call gmSetToggleSwitch(ini_Toggle3_1, Gon)
		call gmSetToggleSwitch(ini_Toggle4_1, Gon)
		call gmSetToggleSwitch(ini_Toggle4_2, Gon)
		call gmSetToggleSwitch(ini_Toggle5_1, Gon)
		call gmSetToggleSwitch(ini_Toggle6_1, Gon)
		call gmSetToggleSwitch(ini_Toggle7_1, Gon)
		call gmSetToggleSwitch(ini_Toggle7_2, Gon)
	case(-101,-102)
		isetu=isetu0
		ncon=models(igraph2)%ncon
		k=models(igraph2)%n
		npar=models(igraph2)%npar
		kj=1
		do m=1,ncon
				ic(1,m)=models(igraph2)%ic(1,m)
				ic(2,m)=models(igraph2)%ic(2,m)
				i=ic(1,m)
				j=ic(2,m)
				irate(kj)=i
				jrate(kj)=j
				irate(kj+1)=j
				jrate(kj+1)=i
				irate1(kj)=i
				jrate1(kj)=j
				irate1(kj+1)=j
				jrate1(kj+1)=i
				kj=kj+2
		enddo
		do m=1,models(igraph2)%npar
				i=irate(m)
				j=jrate(m)
				IQ(i,j)=m
				qt(i,j) = ratcons(indrat)%qt(i,j)
				theta0(m)=qt(i,j)
				thsav(m)=theta0(m)
		enddo
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
				thetaf(ik)=QT(i,j)

				jfix1(ik)=0
			endif
			endif
		enddo
		call HJCDATw(-1,hjcfitform,idatyp,nfile,kfile,pfiles,nval,irecs,calfacs2,&
     nintt,avamp,rms,ffilt,trise,fc,iscan,readini,ndev,idest,&
     name,idiscn,treso,tresg,nset,conc,nlig,ligname,nmax,ntot,&
     qfile2,adcfil,nfileb,pfileb,npatch,defname,samexp,&
     cjump,nsweep,tzero,tsamp,autosim,ipatch,ptype,temp,tedit1,tedit2,&
     tedit3,tedit4,tval1,tval2,tval3,tval4,tval5,tval6,tval7,tval8,&
     tval9,tvalt,tedit5)
	simulat=iscan.eq.-103.or.iscan.eq.-3001
		if(allocated(tint0)) DEALLOCATE(tint0)
		if(allocated(iampl0)) DEALLOCATE(iampl0)
		if(allocated(iprops0)) DEALLOCATE(iprops0)
		if(allocated(ampl0)) DEALLOCATE(ampl0)
		if(allocated(index)) DEALLOCATE(index)
		allocate(tint0(nmax,nset),iampl0(nmax,nset),iprops0(nmax,nset),&
		ampl0(nmax,nset),index(nmax,nset))
		cjump=.false.
		nsweep=1
		if(allocated(tint)) DEALLOCATE(tint)
		if(allocated(ampl)) DEALLOCATE(ampl)
		if(allocated(iprops)) DEALLOCATE(iprops)
		allocate(kjumps0(nsweep),kjumps(nsweep))
		CALL HJCDATw2(tint0,iampl0,ampl0,iprops0,iscan,&
        nintt,nfile,kfile,pfiles,calfacs2,nval,irecs,nmax,nset)
		allocate(tint(nmax,nset),ampl(nmax,nset),iprops(nmax,nset))
		nd1=nmax
		nd2=nset
		
	
		do j=1,nset
		   do i=1,nlig
				conc1(i,j)=conc(i,j)
		   enddo
		   nint(j)=nintt(j)
		   do i=1,nint(j)
			tint(i,j)=tint0(i,j)
			ampl(i,j)=ampl0(i,j)
			iprops(i,j)=iprops0(i,j)
		  enddo
		enddo
			do j=1,nset
		treso=0.001*tresol(j)
		tresg=treso
		treso5=treso
		 tresg5=tresg
		 acrit5=acrit(j)
		 avamp5=avamp(j)
			sim=simulat
		call resolution2(tint0(1,j),tint(1,j),ampl0(1,j),ampl(1,j),&
     	 iprops0(1,j),iprops(1,j),nint(j),nintt(j),&
     	 ffilt(j),fc(j),rms(j),trise(j),nmax,nd1,index(1,j),&
     	 cjump,nsweep,kjumps0,kjumps,autosim,nsims,&
     	 sim,sres,sexp,excamp,alo(j),ahi(j))
		 
		 tresol(j)=1.e3*treso	!keep (possibly altered) res in mus for .ini
		 tresd(j)=dble(treso)*1.0d-3 	!same as tresg (but in seconds)
		 enddo
		 	infoind=infoind+1
			call intconv(nset,cnum5)
				itxtinfo=gmcreatetextentry(infopanel,1,infoind,12,1,&
						'Number of sets= '//cnum5,60, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						infoind=infoind+1
		
			do j=1,nset
			do i=1,nfileb(j)
		
			   itxtinfo=gmcreatetextentry(infopanel,1,infoind,12,1,&
						'Experiment file:'//pfileb(i,j)(1:30),60, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						infoind=infoind+1
			    call realtoch(conc(i,j),cnum0,11)
				itxtinfo=gmcreatetextentry(infopanel,1,infoind,12,1,&
						'Concentration= '//cnum0,60, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)						
				infoind=infoind+1
			enddo
			enddo
		call gmRemoveWindow(ini)
	
		if(callid.eq.-101) then
			callid=4001
				icprev=0
			goto 2
		else
		!	imesa=gmdisplaymessagebox('','Choose a plot type!',ginformation,gok)
		     call gmSetWidgetStatus(ipl1, GSELECTABLE)
		     call gmSetWidgetStatus(ipl2, GSELECTABLE)
		     call gmSetWidgetStatus(ipl3, GSELECTABLE)
		     call gmSetWidgetStatus(ipl4, GSELECTABLE)
		     call gmSetWidgetStatus(ipl5, GSELECTABLE)
             call gmSetWidgetStatus(ipl6, GSELECTABLE)
			 callid=4001
			icprev=-102
			 goto 2
		endif

	case(-111)
	!	call gmSetToggleSwitch(ini_Toggle1_1, Gon)

	
	case(-112)
	!	call gmSetToggleSwitch(ini_Toggle1_1, Goff)
			call gmSetWidgetStatus(ini_text1_1, GSELECTABLE)
			ntog=2
			text_tog(1)='Display old model'
			text_tog(2)='Create new model'
			call toggle_panel(Main,ITOGGLE,ittoglepanel,ntog,text_tog,intoggle,-115,valdat,idat,itogbutton)
	case(-115)
		    istatus0=gmEnqToggleSwitch(inToggle(1))
			if(istatus0.eq.gon) then
				callid=1200
				call gmRemoveWindow(itoggle)	
				call gmRemoveWindow(ini)
				jopen2(igraph2)=-1
				inipage=-200
				goto 2
			endif
			istatus0=gmEnqToggleSwitch(inToggle(2))
			if(istatus0.eq.gon) then
				callid=1100
				call gmRemoveWindow(itoggle)	
				call gmRemoveWindow(ini)
				inipage=-300
				jopen2(igraph2)=-1
				goto 2
			endif
			goto 1
	case(-121)
		!call gmSetToggleSwitch(ini_Toggle2_1, Gon)
	
	case(-122)
	!	call gmSetToggleSwitch(ini_Toggle2_1, Goff)	
	    call gmSetWidgetStatus(ini_value2_1, GSELECTABLE)
		text_tog(1)='Spanning Tree Method'
		text_tog(2)='4 Cycles Method'
	    call toggle_panel(Main,ITOGGLE,itogglepanel,2,text_tog,intoggle,-125,valdat,idat,itogbutton)
 	
	case(-124)
		indmr=2010
		call gmSetToggleSwitch(intoggle(2), Goff)	
	case(-123)
		indmr=2020
	call gmSetToggleSwitch(intoggle(1), Goff)	
	case(-125)
		if(indmr.ne.2010.and.indmr.ne.2020) indmr=2020
		callid=indmr
		inipage=-100
		goto 2
	case(-131)
		call gmSetToggleSwitch(ini_Toggle3_1, Gon)
	case(-132)
	!	call gmSetToggleSwitch(ini_Toggle3_1, Goff)
		call gmSetWidgetStatus(ini_value3_1, GSELECTABLE)
		inipage=-100
		callid=2030
		goto 2		
	case(-141)
	!	call gmSetToggleSwitch(ini_Toggle4_1, Gon)
	case(-142,-147,-148)	
	!	call gmSetToggleSwitch(ini_Toggle4_1, Goff)
		call gmSetWidgetStatus(ini_value4_1, GSELECTABLE)
		if(callid.ne.-142) then
				istatus0=gmEnqToggleSwitch(ini_Toggle6_7)
		if(istatus0.eq.gon) then
			setbad(1,ns)=.true.
		else
		setbad(1,ns)=.false.
		endif
		istatus0=gmEnqToggleSwitch(ini_Toggle6_8)
		if(istatus0.eq.gon) then
			setbad(2,ns)=.true.
		else
				setbad(2,ns)=.false.
		endif
		istatus0=gmEnqToggleSwitch(ini_Toggle6_2)
		if(istatus0.eq.gon) then
				onechan=.true.
		else
			onechan=.false.
		endif
		istatus0=gmEnqToggleSwitch(ini_Toggle6_4)
		if(istatus0.eq.gon) then
			chsvec(ns)=.true.
		else
			chsvec(ns)=.false.
		endif
		istatus0=gmEnqToggleSwitch(ini_Toggle6_5)
		if(istatus0.eq.gon) then
			badend(ns)=.true.
		else
			badend(ns)=.false.
		endif
		endif
		if(callid.ne.-142) inipage=-300
		if(callid.eq.-147) isetu=7
		if(callid.eq.-148) isetu=8
		callid=2040
		inipage=-100
		isetu0=isetu
		
		goto 2
				
	case(-143)
	!	call gmSetToggleSwitch(ini_Toggle4_2, Gon)
	case(-144)
	!	call gmSetToggleSwitch(ini_Toggle4_2, Goff)
		call gmSetWidgetStatus(ini_value4_2, GSELECTABLE)
		inipage=-100
		callid=2030
		goto 2		
	case(-151)
	!	call gmSetToggleSwitch(ini_Toggle5_1, Gon)
	case(-152)
	!	call gmSetToggleSwitch(ini_Toggle5_1, Goff)
			call gmSetWidgetStatus(ini_value5_1, GSELECTABLE)
		inipage=-100
		callid=3001	
		goto 2	
	case(-161)
	!	call gmSetToggleSwitch(ini_Toggle6_1, Gon)
		istatus0=gmEnqToggleSwitch(ini_Toggle6_7)
		if(istatus0.eq.gon) then
			setbad(1,ns)=.true.
		else
		setbad(1,ns)=.false.
		endif
		istatus0=gmEnqToggleSwitch(ini_Toggle6_8)
		if(istatus0.eq.gon) then
			setbad(2,ns)=.true.
		else
				setbad(2,ns)=.false.
		endif
		istatus0=gmEnqToggleSwitch(ini_Toggle6_2)
		if(istatus0.eq.gon) then
				onechan=.true.
		else
			onechan=.false.
		endif
		istatus0=gmEnqToggleSwitch(ini_Toggle6_4)
		if(istatus0.eq.gon) then
			chsvec(ns)=.true.
		else
			chsvec(ns)=.false.
		endif
		istatus0=gmEnqToggleSwitch(ini_Toggle6_5)
		if(istatus0.eq.gon) then
			badend(ns)=.true.
		else
			badend(ns)=.false.
		endif
	case(-162)
	!	call gmSetToggleSwitch(ini_Toggle6_1, Goff)
		!	call gmSetWidgetStatus(ini_value6_1, GSELECTABLE)
		!		call gmSetWidgetStatus(ini_value6_3, GSELECTABLE)
		!	call gmSetWidgetStatus(ini_value6_7, GSELECTABLE)
		!	call gmSetWidgetStatus(ini_value6_8, GSELECTABLE)
		
		inipage=-100
			j=ns
			conc(1,j)= gmenqvaluesetting(ini_Value6_1)
			tcrit(j)=gmenqvaluesetting(ini_Value6_3)
			tcbad(1,j)=gmenqvaluesetting(ini_Value6_7)
			tcbad(2,j)=gmenqvaluesetting(ini_Value6_8)
			tresol(j)=gmenqvaluesetting(ini_Value6_9)
			vgroup1(j)=tcrit(j)
			vgroup2(j)=tcbad(1,j)!shut
			vgroup3(j)=tcbad(2,j)!open
			vgroup4(j)=conc(1,j)
			vgroup5(j)=tresol(j)
			istt=gmEnqToggleSwitch(ini_Toggle6_4)
			if(istt) chsvec(j)=.true.
			istt=gmEnqToggleSwitch(ini_Toggle6_5)
			if(istt) badend(j)=.true.
			istt=gmEnqToggleSwitch(ini_Toggle6_7)
			if(istt) setbad(1,j)=.true.
			istt=gmEnqToggleSwitch(ini_Toggle6_8)
			if(istt) setbad(2,j)=.true.
			istt=gmEnqToggleSwitch(ini_Toggle6_2)
			if(istt) onechan=.true.
		
		call defgrp(main,igroup,nset,vgroup1,vgroup2,vgroup3,vgroup4,vgroup5,group_editval1,group_editval2,&
			group_editval3,group_editval4,group_editval5,group_Toggle0,group_Toggle1,group_Toggle2,group_Toggle3,&
			group_Toggle4,group_Toggle5,inipage,onechan,setbad,chsvec,badend,ns)
				
	case(-163)
			if(ns.lt.nset) then
		    ns=ns+1
			call gmsetvaluesetting(ini_Value6_1,conc(1,ns))
			call gmsetvaluesetting(ini_Value6_3,tcrit(ns))
			call gmsetvaluesetting(ini_Value6_7,tcbad(1,ns))
			call gmsetvaluesetting(ini_Value6_8,tcbad(2,ns))
			
			if(onechan) call gmSetToggleSwitch(ini_Toggle6_2, Gon)
			if(chsvec(ns).eq..true.)	call gmSetToggleSwitch(ini_Toggle6_4, Gon)
			if(badend(ns).eq..true.)	call gmSetToggleSwitch(ini_Toggle6_5, Gon)
			if(setbad(1,ns).eq..true.)	call gmSetToggleSwitch(ini_Toggle6_7, Gon)
			if(setbad(2,ns).eq..true.)	call gmSetToggleSwitch(ini_Toggle6_8, Gon)
				call gmsetvaluesetting(ini_Value6_9,tresol(ns))
	
			call gFlushGraphics()
			endif
	case(-164)
		if(ns.gt.1) then
		ns=ns-1
		call gmsetvaluesetting(ini_Value6_1,conc(1,ns))
			call gmsetvaluesetting(ini_Value6_3,tcrit(ns))
			call gmsetvaluesetting(ini_Value6_7,tcbad(1,ns))
			call gmsetvaluesetting(ini_Value6_8,tcbad(2,ns))
	call gmsetvaluesetting(ini_Value6_9,tresol(ns))
				
			if(onechan) call gmSetToggleSwitch(ini_Toggle6_2, Gon)
			if(chsvec(ns).eq..true.)	call gmSetToggleSwitch(ini_Toggle6_4, Gon)
			if(badend(ns).eq..true.)	call gmSetToggleSwitch(ini_Toggle6_5, Gon)
			if(setbad(1,ns).eq..true.)	call gmSetToggleSwitch(ini_Toggle6_7, Gon)
			if(setbad(2,ns).eq..true.)	call gmSetToggleSwitch(ini_Toggle6_8, Gon)
		
			call gFlushGraphics()
		endif
	case(-171)
		call gmSetToggleSwitch(ini_Toggle7_1, Gon)
	case(-172)
		call gmSetToggleSwitch(ini_Toggle7_1, Goff)
			call gmSetWidgetStatus(ini_value7_4, GSELECTABLE)
			call gmSetWidgetStatus(ini_value7_3, GSELECTABLE)
		call gmSetWidgetStatus(ini_text7_1, GSELECTABLE)
	inipage=-100					
	callid=igraph2+1400
	goto 2
	case(-173)
			call gmSetToggleSwitch(ini_Toggle7_2, Gon)
	case(-174)
		
	!	call gmSetToggleSwitch(ini_Toggle7_2, Goff)
		xs1=sngl(ratemax)
		xs=sngl(assmax)
		call penaly(main,ipeny,ipeny_yes,ipeny_no,ipeny_xs,xs,ipeny_xs1,xs1,ipeny_fac,-1.)
	case(-175)
			xs=gmenqvaluesetting(ipeny_xs)
			
			xs1=gmenqvaluesetting(ipeny_xs1)
			assmax=dble(xs)
			ratemax=dble(xs1)
			
			call gmremovewindow(ipeny)
			call gmsetvaluesetting(ini_Value7_3,assmax)
			call gmsetvaluesetting(ini_Value7_4,ratemax)

!=================================================================================
	!Attributes
  	!(part of vplot5 SECTION)

!=================================================================================
! GRAPH ATTRIBUTES
!===================================================================================	
				
		  case(1:400) ! edit,fonts,colour,size,box,delete,etc
			
			call graph_attributes(main,callid,modplot,lframe,jindex,ind_1,oldrecords,readrec,newfile,&
		graphics1_1,combo1_6,combo1_4,combo1_5,combo1_8,combo1_9,combo1_10,istate,hdisp,izoom,xtitle,ytitle,&
		xmin0,xmax0,ymin0,ymax0,itogglepanel,itext_entry,new_text_entry)
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
		   
			call calc_default(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,symsiz,&
				ndimd,XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,&
				XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,&
				XLO1,XHI1,YLO1,YHI1,y0,yinf,inumx,inumy,&
				titlex,titley,ilabel,doframe,autplt,itit,title1,&
				ISHP,ifnt,landscap,ndv1,ndc1,w,kwi,kwj,icurvw,kmax,iver,&
				redrawn,plot,redo,pcgm,pwmf,npint,hdisp,n1,tlenx,tleny,nx1,ny1,izoom,&
				xtitle,ytitle,rescale,dxs,dys,ifitype,parval,theta,ncomp)	
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
					icol,ntx,nty,idev,thick,itype)
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
						Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw)
				endif
				call draw_curves(xcal,ycal,icurvc,ncurvc,logity,logx,&
					logy,sqrty,y0,yinf,x0,ilog,idev,&
						wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
						xmin,xmax,ymin,ymax,ncal,&
					iline,icol,thick,ndc1,ndimc,jfirst,dxs,dys)
               	if(show_curve) then
					ymin20=ymin
							ymax20=ymax
							if(sqrty) ymin20=sqrt(ymin)
							if(sqrty) ymax20=sqrt(ymax)
							xt=0.5*(xmin+xmax)
							yt=ymax20+0.15*(ymax20-ymin20)
								ytc=0.05*(ymax20-ymin20)
								if(ncurvc.gt.0) call write_string('Curves:',xmax+0.11*(xmax-xmin),ymax20+0.01*(ymax20-ymin20),0.,0,101,&
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
								    goto 766
								else if(ij.ge.10) then
									linetype=ij-10			!join points with straight line type #ij
								endif
								call broken(linetype)
								call movto2(xmax+0.1*(xmax-xmin),ymax20-ytc)
								call linto2(xmax+0.2*(xmax-xmin),ymax20-ytc)
766								ytc=ytc+0.05*(ymax20-ymin20)
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
				Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw)
				icol(jindex)=oldrecords(modplot)%attributes%icol(jindex)
			
				call draw_data(xval,yval,icurvd,ncurvd,ndelt,logity,logx,logy,sqrty,&
				y0,yinf,x0,ilog,idev,ndat,ijoin,icol,isym,&
				symsiz,thick,barleng,interp,npint,Xint,Yint,nint,yp1,ypn,&
				Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw)
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
		if(show_curve) then
					ymin20=ymin
							ymax20=ymax
							if(sqrty) ymin20=sqrt(ymin)
							if(sqrty) ymax20=sqrt(ymax)
							xt=0.5*(xmin+xmax)
							yt=ymax20+0.15*(ymax20-ymin20)
								ytc=0.05*(ymax20-ymin20)
								if(ncurvc.gt.0) call write_string('Curves:',xmax+0.11*(xmax-xmin),ymax20+0.01*(ymax20-ymin20),0.,0,101,&
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
								    goto 767
								else if(ij.ge.10) then
									linetype=ij-10			!join points with straight line type #ij
								endif
								call broken(linetype)
								call movto2(xmax+0.1*(xmax-xmin),ymax20-ytc)
								call linto2(xmax+0.2*(xmax-xmin),ymax20-ytc)
767								ytc=ytc+0.05*(ymax20-ymin20)
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
			icol,ntx,nty,idev,thick,itype)
            
			icol(jindex)=oldrecords(modplot)%attributes%icol(jindex)
			
	
			call draw_axis(xmin,xmax,ymin,ymax,ymin1,ymax1,xtic,ytic,&
			nx1,ny1,xcross,ycross,itx,ity,tlenx,tleny,logx,logy,sqrty,doframe,&
			icol,ntx,nty,idev,thick,itype)
		endif

    case(801) !(14,20,18)    ! Save as bmp/wmf
	if(readrec) then
			if(modplot.gt.0.and.lframe.eq.graphics1_1(modplot)) then
					callid=modplot+5200
					goto 2
			else if(modplot2.gt.0.and.lframe.eq.graphics1_2(igraph2)) then
					callid=igraph2+1900
					goto 2
			else
				imessy=gmDisplayMessageBox('',&
				'Select a model/graph',Gexclamation,gok)	
			endif
    else
			imessy=gmDisplayMessageBox('','No record on screen/Not available for this record',Gexclamation,gok)
	endif


	case(901)! print
	
	if(readrec) then
			if(modplot.gt.0.and.lframe.eq.graphics1_1(modplot)) then
					callid=modplot+5200
				    goto 2
			else if(modplot2.gt.0.and.lframe.eq.graphics1_2(igraph2)) then
					callid=igraph2+1850
					goto 2
			else
				imessy=gmDisplayMessageBox('',&
				'Select a model/graph',Gexclamation,gok)	
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
		   call mechelp(main,ihelp1,ki,0)
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
		   call mechelp(main,ihelp1,ki,615)
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
		call about(Main,Form12,title,iprogram_type)


!===========================================================================

	!MECHANISM

!==========================================================================
	case(1001:2100) ! readini+readmec=true
	
		call mechanism(callid,igraph2,main,models,imodel,imodold,&
		ratcons,indrat,irecq,readini,&
		pfilem,qfilem,efile,text7,records,nrecs,ijmod,nmr,cxtrue,cytrue,readrec,&
		thetaf,jfix1,imove,neqold)
		if(callid.eq.-23.or.callid.eq.-101.or.callid.eq.-102) goto 2
	
		if(callid.eq.-22) goto 2
	case(2101:2125)
		igraph2=callid-1300
		imodel=models(igraph2)%model
		callid=2100
		goto 2	
	case(2150)	!Show curve
		if(igraph2.gt.0) then
	 do i=1,npar
		thetaf(i)=theta0(i)
		thsav(i)=theta0(i)
	      irate1(i)=irate(i)	!now called irate1(i)
	      jrate1(i)=jrate(i)
	   enddo
	   kfit=npar
		    nodata=.true.
			nofit=.true.
			autosim=.false.
			curvonly=.true.
		
			textiq1(1)='Nr concentrations'
			textiq1(2)='Resolution '
			nset=1
			treso=0.
		
			textiqt1(1)='1'
			textiqt1(2)='0.0'
			call text_table(Main,iqwin,' ',2,textiq1,iqt1,textiqt1,2151)
		
		else
			imessy=gmDisplayMessageBox('','No model on screen',Gexclamation,gok)		
		endif
	case(2151) ! curve only
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
	
	case(2301:2400)   ! move
		  
		   
				modplot2=callid-2300
				igraph2=modplot2
				if(readrec.and.igraph2.gt.0) then
				call gmActivateGraphicsFrame(graphics1_2(modplot2))
				lframe=graphics1_2(modplot2)
				
				istate=models(igraph2)%n
			
			CALL ACTENQ(CALLid,IDENT,IKEY,ISTATmove,Xac,Yac,acTEXT,nacargs,ARGS,IARGS)
			xtrue=xac
			ytrue=yac
			xmov=xtrue
			ymov=ytrue
			
			call valtyp(1,0)
			
			CALL REALTOCH(XTRUE,CXTRUE,11)
			CALL REALTOCH(YTRUE,CYTRUE,11)
			call gmSetStatusBarText(Status_bar1,2,CXTRUE)
			call gmSetStatusBarText(Status_bar1,3,CYTRUE)
 
			call valtyp(0,0)
			
		  ENDIF		
		  case(2401:2500)	   !select
		  
			
				modplot2=callid-2400
				igraph2=modplot2
				if(igraph2.gt.0) then
				call gmActivateGraphicsFrame(graphics1_2(modplot2))
				lframe=graphics1_2(modplot2)
				IND_1=modplot2
				jindex=ind_1
				call mouse_select(ind_1,npoint,temp,d_line, d_hline, d_vline, d_arrow, &
				d_poly, zoom,d_text, o_state,c_state, s_text,xmov,ymov,xmov0,ymov0,&
				imark,izoom,mm,itemp,inewpos,str,lframe,istate,ilink,link,delete_state,&
				move_state,models,ind_m,imove,ind_s,ind_e,mod_create,dxsm,dysm)
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
			
	
! READ DATA
!==================================================================================


    
     case(3001)!12	! Open file
	!		if(models(igraph2)%indmod) call gmRemovewindow(indwin)
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
			itypeval(1)=1
			vtext(1)='Number of sets/concentrations'
			call value_table(Main,ivwin,n,vtext,ival,val,3002,itypeval)
	
	
	!show data tabel
 !===========================================================================
	case(3301) !11	!Simulate data
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
			itypeval(1)=1
			vtext(1)='Number of sets/concentrations'
			call value_table(Main,ivwin,n,vtext,ival,val,3002,itypeval)
		case(3002)
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
			if(readini.and.inipage.ne.-100) then
			do j=1,nset
			  nfile(j)=nfileb(j)
				title_box(j)='Set:'//char(48+j)
				do i=1,nfileb(j)
				INQUIRE (FILE=pfileb(i,j),EXIST=PRESENT) 
				if(PRESENT) then 
					text_boxf(i,j)=pfileb(i,j)
				else
					ians=gmdisplaymessagebox(pfileb(i,j),'is not in the path specified by ini file',&
					ginformation,gok)
					text_boxf(i,j)=' '
				endif
				enddo
				do i=nfileb(j)+1,10
					text_boxf(i,j)=' '
				enddo
			enddo
			
			else
			do i=1,nset
				title_box(i)='Set:'//char(48+i)
				nfile(i)=0
				do j=1,10
				text_boxf(j,i)=' '
			
				enddo
			enddo
			
			endif
			call table_box(main,itwin,10,nset,title_box,text_boxf,itext_boxf,3011)
			endif
		case(3021:3030)
			import=.false.
		
			kset=callid-3020	
		
			dfilt='*.scn'//char(124)//'Scan Files (SCN)'//char(124)//&
			'*.dat'//char(124)//'Data File (DAT)'//char(124)//'*.*'//char(124)//'All Files'

				do i=1,100
					icfit(i)=0
					iopen(i)=-1
					jopen(i)=-1
				enddo
				CALL gmFileBROWSER(DFILE,DDIR,DFILT,gmBrowseType=gmultipleinput)
	 	
		IF(DFILE.ne.' ') then
			call gmSetListEntry(combo1_3,GSELECT,gmEntry=1)
			call file_open(MAIN,RECORDS,nplot,IFILTYPE,iplotype,iptype,DFILE,FOPEN_11,FOPEN_12,&
			APFILE,DISCPRT,njset,ncolr,nrowr,label,ftitle,nset,iwid,List7_1, &
			ftitle1, button7,button6,saveandplot,program_type)
			call gmsetwidgetstatus(intoggle(7),gchecked)
			if(njset.gt.0) then
				do i=1,njset
					jmiss(njset)=0
				enddo
			endif
			if(ifiltype.eq.5.or.ifiltype.eq.6) then
				if(ifiltype.eq.5) idatyp=1
				if(ifiltype.eq.6) idatyp=2
				n=1
				val(1)=kset
				callid=3013
				goto 2
			endif
			if(allocated(xdata).and..not.newfile) DEALLOCATE(xdata)
			if(ifiltype.eq.3) nplot=1
			if(.not.newfile) ALLOCATE(xdata(nplot,njset,ncols,niobs)) 
			newfile=.false.
		
			
			endif
		case(3013)
			j=kset
			nfile(j)=nfile(j)+1
			if(nfile(j).gt.1) then
			do i=1,nfile(j)-1
				call gmenqtextsetting(itext_boxf(i,j),text_boxf(i,j))
			
				if(text_boxf(i,j).eq.' ') then
						nfile(j)=nfile(j)-1
				endif
			enddo
			endif
			nd=nblank1(ddir)
			pfiles(j,nfile(j))=ddir(1:nd)//'\'//dfile
		!	call gmsettextsetting(itext_box(nfile(i),i),dfile)
			call gmsettextsetting(itext_boxf(nfile(j),j),pfiles(j,nfile(j)))
			
		case(3014) ! finish
			do j=1,nset
			nfileb(j)=nfile(j)
			do i=1,nfile(j)
				call gmenqtextsetting(itext_boxf(i,j),text_boxf(i,j))
				pfiles(i,j)=text_boxf(i,j)
				inquire(file=pfiles(i,j),EXIST=PRESENT)
				if(.not.present) then
					imessy=gmDisplayMessageBox('',&
			'File does not exit;pool another one',Gstop,gok)
				goto 1
				endif
			enddo
			enddo
			call gmRemoveWindow(itwin)
			if(inipage.eq.-100) then
			do j=1,nset
			do i=1,nfile(j)
				call gmSetCellSetting(ini_TxtArray5, 1,j ,gmString=pfiles(i,j))
				call gmdrawwidget(ini_TxtArray5)
				pfileb(i,j)=pfiles(i,j)
			enddo
			enddo
			goto 1
			else
		!	call hjcfit_table(main,hjcfitform,nset,nfile,pfiles)
			call HJCDATw(main,hjcfitform,idatyp,nfile,kfile,pfiles,nval,irecs,calfacs2,&
     nintt,avamp,rms,ffilt,trise,fc,iscan,readini,ndev,idest,&
     name,idiscn,treso,tresg,nset,conc,nlig,ligname,nmax,ntot,&
     qfile2,adcfil,nfileb,pfileb,npatch,defname,samexp,&
     cjump,nsweep,tzero,tsamp,autosim,ipatch,ptype,temp,tedit1,tedit2,&
     tedit3,tedit4,tval1,tval2,tval3,tval4,tval5,tval6,tval7,tval8,&
     tval9,tvalt,tedit5)
	
			goto 1
			endif
	case(3031)  !9531 see hjcfit_table for changes
	! after reading data
	!	call gmsetprogressvalue(ipbar_Progress2,50)
		if(autosim) then
		nsims=1
		nsim=1
		npar=1
		nlig=nset !!!!! query for autosim -after read model -----------
		goto 1887
			call intconv(nset,cnum5)
				itxtinfo=gmcreatetextentry(infopanel,1,infoind,12,1,&
						'Number of sets= '//cnum5,60, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						infoind=infoind+1
		
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
			infoind=infoind+1
		
		enddo
		enddo

1887		 continue	
	call intconv(nset,cnum5)
	itxtinfo=gmcreatetextentry(infopanel,1,infoind,12,1,&
						'Number of sets= '//cnum5,60, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						infoind=infoind+1
			
	do j=1,nset
		
			 do i=1,nfile(j)
			infoind=infoind+1
			itxtinfo=gmcreatetextentry(infopanel,1,infoind,12,1,&
						'Experiment file: '//pfiles(i,j),60, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
				infoind=infoind+1			
			call realtoch(conc(i,j),cnum0,11)
				itxtinfo=gmcreatetextentry(infopanel,1,infoind,12,1,&
						'Concentration= '//cnum0,60, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)						
			enddo
			
		


			do i=1,nlig
				conc1(i,j)=conc(i,j)
			enddo
		enddo
		call gmremovewindow(hjcfitform)
			call gmsetwidgetstatus(intoggle(7),gchecked)
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
		if(.not.allocated(kjumps0)) allocate(kjumps0(nsweep),kjumps(nsweep))
		!call hjcdat2()
	
		CALL HJCDATw2(tint0,iampl0,ampl0,iprops0,iscan,&
        nintt,nfile,kfile,pfiles,calfacs2,nval,irecs,nmax,nset)
	   !resolution now
	   	if(.not.allocated(tint)) allocate(tint(nmax,nset),ampl(nmax,nset),iprops(nmax,nset))
		nd1=nmax
		nd2=nset
		call intconv(nset,cnum5)
	   itxtinfo=gmcreatetextentry(infopanel,1,infoind,12,1,&
						'Number of sets= '//cnum5,60, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						infoind=infoind+1

		do j=1,nset
			
		
			 do i=1,nfile(j)
		
		  		 
			infoind=infoind+1
			itxtinfo=gmcreatetextentry(infopanel,1,infoind,12,1,&
						'Experiment file: '//pfileb(i,j),60, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
			infoind=infoind+1
			call realtoch(conc(i,j),cnum0,11)
				itxtinfo=gmcreatetextentry(infopanel,1,infoind,12,1,&
						'Concentration= '//cnum0,60, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)						
			
			
		
		 enddo	
		 
		   nint(j)=nintt(j)
		   do i=1,nint(j)
			tint(i,j)=tint0(i,j)
			ampl(i,j)=ampl0(i,j)
			iprops(i,j)=iprops0(i,j)
		   enddo
		enddo
		call gmremovewindow(hjcfitform)
		call gmsetwidgetstatus(intoggle(7),gchecked)
		if(simulat) then
			do j=1,nset
				n=2
				val(1)=treso*1000.
				val(2)=tresol(j)
				vtext(1)='Resolution (microsec) already imposed in SCSIM'
				vtext(2)='Resolution (microsec) used in last run'
				vtext(50)='Resolution'
				icall=3032
				itypeval(1)=0
				itypeval(2)=0
			    call value_table(Main,ivwin,n,vtext,ival,val,icall,itypeval)
				tres=dble(tresol(j))*1.0d-6
				tresd(j)=tres
			
			enddo
		
		else
			callid=3033
			goto 2
		endif
		endif
case(3032)
			call gmremovewindow(ivwin)
		callid=3052
		goto 2
case(3033)
			call gmremovewindow(ivwin)
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
		
		do i=1,nset
			tvalres1(i)=0.0
			tvalres2(i)=0.0
			if(.not.readini) tresol(i)=40.
			tvalres3(i)=tresol(i)
			tvalres4(i)=avamp(i)
			tvalres5(i)=acrit(i)
		enddo
	!	call gmRemoveWindow(hjcfitform)
	call resolution(main,iresoform,nset,tvalres1,tvalres2,tvalres3,tvalres4,tvalres5,&
	tvalres6,tvalres7,tvalres8,tvalres9,valres1,valres2,valres3,valres4,valres5,&
	valres6,valres7,valres8,valres9)

!reso

	case(3041:3050)
		i=callid-3040
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
                !=======================????????
	case(3051) ! next stage resolution
	    call gmremovewindow(iresoform)
		do j=1,nset
		treso=0.001*tresol(j)
		tresg=treso
		treso5=treso
		 tresg5=tresg
		 acrit5=acrit(j)
		 avamp5=avamp(j)
			sim=simulat
		call resolution2(tint0(1,j),tint(1,j),ampl0(1,j),ampl(1,j),&
     	 iprops0(1,j),iprops(1,j),nint(j),nintt(j),&
     	 ffilt(j),fc(j),rms(j),trise(j),nmax,nd1,index(1,j),&
     	 cjump,nsweep,kjumps0,kjumps,autosim,nsims,&
     	 sim,sres,sexp,excamp,alo(j),ahi(j))
		 
		 tresol(j)=1.e3*treso	!keep (possibly altered) res in mus for .ini
		 tresd(j)=dble(treso)*1.0d-3 	!same as tresg (but in seconds)

!!!!! change this
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
		callid=3052
		goto 2
	   !call gmsetprogressvalue(ipbar_Progress2,60)
  case(3052)

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
		callid=3100 !9700
		if(.not.simulat) call gmRemoveWindow(iresoform)
		goto 2
                !!!!!!!!!!!!!!!!!!!!
	case(3061:3070)!(9541:9550)
		j=callid-3060
		trise(j)=332.1/fc(j)
	CASE(3071:3080)!(9551:9560)
		j=callid-3070
		n=2
		val(1)=0.
		val(2)=0.
		vtext(1)='-3dB frequency for tape recorder (kHz) = '
		vtext(2)='-3dB frequency for tape recorder (kHz) = '
		itypeval(1)=0
		itypeval(2)=0
		call value_table(Main,ivwin,n,vtext,ival,val,3091,itypeval) !9532)
				

	CASE(3081:3090)!(9561:9570)
		j=callid-3080
		n=1
		val(1)=0.
		vtext(1)='Rise time (microseconds) ='
		itypeval(1)=0
		call value_table(Main,ivwin,n,vtext,ival,val,3092,itypeval) !9533)
	
	CASE(3091)!(9532)
		do i=1,2
			val(i)=gmEnqvalueSetting(ival(i))
		
		enddo
		fc1=val(1)
		fc2=val(2)
		call gmRemoveWindow(ivwin)
		 fc(j)=1.0/sqrt(1./fc(j)**2 + 1./fc1**2 + 1./fc2**2)
	     trise(j)=332.1/fc(j)		!rise time (microsec)
	CASE(3092)!(9533)
		val(1)=gmEnqvalueSetting(ival(1))
		trise(j)=val(1)
		 fc(j)=332.1/trise(j)	!corresponding fc (-3dB)
		call gmRemoveWindow(ivwin)

	CASE(3095)!(9515) ! reset
	CASE(3100)!(9700)
		do j=1,nset
			vgroup1(j)=tcrit(j)
			vgroup2(j)=tcbad(1,j)!shut
			vgroup3(j)=tcbad(2,j)!open
		enddo
		inipage=0
		call defgrp(main,igroup,nset,vgroup1,vgroup2,vgroup3,vgroup4,vgroup5,group_editval1,group_editval2,&
			group_editval3,group_editval4,group_editval5,group_Toggle0,group_Toggle1,group_Toggle2,group_Toggle3,&
			group_Toggle4,group_Toggle5,inipage,onechan,setbad,chsvec,badend,ns)
	CASE(3101:3110) !(9701:9710)
				j=callid-3100 !9700
				istatus0=gmEnqToggleSwitch(group_Toggle0(j))
				if(istatus0.eq.gon) then
				onechan=.true.
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
				call gmSetToggleSwitch(group_toggle1(j),goff)
				endif
	CASE(3111:3120)!(9711:9720)
				j=callid-3110!9710
				istatus1=gmEnqToggleSwitch(group_Toggle1(j))
				if(istatus1.eq.gon) then 
					onechan=.false.
				burst(j)=.true.
			
				badend(j)=.false.
				
				setbad(1,j)=.true. 
				call gmSetWidgetStatus(group_editval1(j), Gselectable)
				call gmSetWidgetStatus(group_toggle5(j), Gselectable)
				call gmSetWidgetStatus(group_toggle4(j), Gselectable)
				call gmSetWidgetStatus(group_toggle2(j), Gunselectable)
				call gmSetWidgetStatus(group_toggle3(j), Gunselectable)
				call gmSetToggleSwitch(group_toggle0(j),goff)
				endif
		CASE(3121:3130) !(9721:9730)
				j=callid-3120!9720
				istatus2=gmEnqToggleSwitch(group_Toggle2(j))
				if(istatus2.eq.gon) then
				setbad(1,j)=.true.
				call gmSetWidgetStatus(group_editval2(j), Gselectable)
				endif
		case(3131:3140)
				j=callid-3130
				istatus3=gmEnqToggleSwitch(group_Toggle3(j))
				call gmSetWidgetStatus(group_editval3(j), Gselectable)
				if(istatus3.eq.gon) then
				setbad(2,j)=.true.
				endif
		case(3141:3150)
				j=callid-3140
				istatus4=gmEnqToggleSwitch(group_Toggle4(j))
				if(istatus4.eq.gon) then
				chsvec(j)=.true.
				endif
		case(3151:3160)
				j=callid-3150
				istatus5=gmEnqToggleSwitch(group_Toggle5(j))
				if(istatus5.eq.gon) then
					badend(j)=.true.
					call gmSetWidgetStatus(group_Toggle2(j), Gselectable)
						call gmSetWidgetStatus(group_Toggle3(j), Gselectable)
						call gmSetWidgetStatus(group_editval2(j), Gselectable)
						call gmSetWidgetStatus(group_editval3(j), Gselectable)
				else
					badend(j)=.false.
					call gmSetWidgetStatus(group_Toggle2(j), Gunselectable)
						call gmSetWidgetStatus(group_Toggle3(j), Gunselectable)
						call gmSetWidgetStatus(group_editval2(j), Gunselectable)
						call gmSetWidgetStatus(group_editval3(j), Gunselectable)
				endif
		case(3161)
	!	call gmsetprogressvalue(ipbar_Progress2,70)
			do j=1,nset
				vgroup1(j)=gmenqvaluesetting(group_editval1(j))
				vgroup2(j)=gmenqvaluesetting(group_editval2(j))
				vgroup3(j)=gmenqvaluesetting(group_editval3(j))
				vgroup4(j)=gmenqvaluesetting(group_editval4(j))
				vgroup5(j)=gmenqvaluesetting(group_editval5(j))
				tcrit(j)=vgroup1(j)
				tcbad(1,j)=vgroup2(j)
				tcbad(2,j)=vgroup3(j)
			    if(inipage.eq.-100) then
					conc(1,j)=vgroup4(j)
					tresol(j)=vgroup5(j)
				
				endif	
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
			if(gaphi(1).eq.0.) then
				do j=1,nset
					gaplo(j)=tcrit(j)
					gaphi(j)=3.1536e10
				enddo
			endif
			if(inipage.eq.-100) then
						call gmremovewindow(igroup)
					callid=-24
					goto 2
			endif
			if(excopen.and.(.not.autosim)) then	 !excopen set under advanced options now
			ians=gmdisplaymessagebox('','Exclude any open times',gexclamation,gyesno)
			if(ians.eq.gnobutton) then
				do j=1,nset
					iexcop(j)=0
				enddo
				iexpr=-1
				callid=3165
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
				callid=3165
				goto 2
			endif
			
		
	case(3171:3180)
				j=callid-3170
				 iexcop(j)=1
				istatus0=gmEnqToggleSwitch(igroup_Toggle0(j))
				if(istatus0.eq.gon) then
			
			
				call gmSetWidgetStatus(igroup_editval1(j), Gselectable)
				call gmSetWidgetStatus(igroup_editval2(j), Gselectable)
			!	call gmSetWidgetStatus(group_toggle1(j),gunchecked)
				endif
	case(3181:3190)
				j=callid-3180
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
	case(3164) !help
	case(3165)
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
				nlig=models(igraph2)%nlig
				do jk=1,nlig
					ligname(jk)=models(igraph2)%ligname(jk)
				enddo
			do i=1,nlig
			if(discprt) write(7,761) ligname(i),conc(i,j)*1.e6
761			format('  Concentration of ',a10,' (micromolar) = ',g13.6)
			enddo
			enddo
				      
			ncon=models(igraph2)%ncon
			indmod=models(igraph2)%indmod
			k=models(igraph2)%n
			if(indmod) then
				npar=models(igraph2)%npar
			else
				npar=2*ncon
			endif
			nrateq=2*ncon
				kj=1	
			do m=1,ncon
				ic(1,m)=models(igraph2)%ic(1,m)
				ic(2,m)=models(igraph2)%ic(2,m)
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
			
		!	if(i.le.100) qt(irate(i),jrate(i))=ratcons(indrat)%qt(i,j)
			enddo
			if(indmod) then
			do i=1,k
			do j=1,k
				qt(i,j)=ratcons(indrat)%qt(i,j)
			enddo
			enddo
			endif
			do i=1,200
			!	jmic(i)=0
			enddo
			!ncyc=ratcons(indrat)%ncyc
			do i=1,ncyc
			!	nsc(i)=ratcons(indrat)%nsc(i)
			enddo
			if(ncyc.gt.0) then
				do i=1,ncyc
				
					if(obeymr(i).and.(.not.automr(i))) then
					!	m=IQ(im(i,1),jm(i,1))
					!	jmic(m)=1
					!	nmr=nmr+1
					endif
				enddo
			endif
			if(discprt) write(7,138)
			if(nlig.gt.0) then
				if(discprt) write(7,332)
332				format(' Q matrix')
				call atypd(qt,'  QT   ',k,k,km,km)
			endif
			do j=1,2
				call gmSetWidgetStatus(eqfit(j), GSELECTABLE)

			enddo
			call gmSetWidgetStatus(view_record, GSELECTABLE)

			kAm=kA
			kFm=kF
		
			call gmremovewindow(igroup)
			imys=gmdisplaymessagebox('','Now start fitting (Menu bar:Fit)',&
			ginformation,gok)
! view parameters
!==========================================================================
	
case(3501)	! show parameters	
		  if(PROGRAM_TYPE.EQ.3) then
		 
     	 call hjcfit_table(main,hjcfitform,nset,nfile,pfiles,tedit1,tedit2,&
			tedit3,tedit4,tval1,tval2,tval3,tval4,tval5,tval6,tval7,tval8,tval9,tvalt,tedit5,&
			fc,ffilt,trise,autosim)
		 
		  endif

case(3511) ! stability plots
case(3611) !stability plots

! FITTING
!===================================================================================
	

case(4001)
		
		 npar=models(igraph2)%npar
	     kam=models(igraph2)%ka
	     kbm=models(igraph2)%kb
		
		 k=models(igraph2)%n
		 nsub=models(igraph2)%nsub
	     ka=models(igraph2)%ka
	     kb=models(igraph2)%kb
		 kf=k-ka
		 kfm=kf
		 kstat=k
		 ncon1=models(igraph2)%ncon
		  npar=models(igraph2)%npar
		  npar1=npar
		  do i=1,k
	     statname(i)=models(igraph2)%statname(i)
		 enddo
!		 if(inifix.ne.-1) then	
!		 imess=gmdisplaymessagebox('','Did you fix the parameters?',gquestion,gyesno)
!		 inifix=-1
!		 if(imess.eq.gnobutton) then
!			callid=2040
!			goto 2
!		 endif
!		 endif
		 kfit=npar-nfix-neq-nmr
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
				thetaf(ik)=QT(i,j)
				titlep(m)=ratcons(indrat)%titlep(m)
				titpfree(ik)=titlep(m)
				jfix1(ik)=0
			endif
			endif
		enddo
		if(icprev.eq.-102) then
			callid=4002
			goto 2
		endif
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
		vtext(50)='Rates'
		do i=1,6
			itypeval(i)=1
			enddo
		call value_table(Main,ivwin,6,vtext,ival,val,4002,itypeval)
	case(4002)
	!	call gmsetwidgetstatus(intoggle(6),gchecked)
         if(icprev.ne.-102) call gmRemoveWindow(ivwin)
		 kab=1
		 if(.not.allocated(alpha2)) allocate(alpha2(kab),beta2(kab),aloglik(kab))
		 if(.not.allocated(Z00A)) then
				ALLOCATE(Z00A(kAm,kFm,km),Z10A(kAm,kFm,km),Z11A(kAm,kFm,km))
				ALLOCATE(Z00F(kFm,kAm,km),Z10F(kFm,kAm,km),Z11F(kFm,kAm,km))
				ALLOCATE(XAF(kAm,kFm,kAm),XFA(kFm,kAm,kFm),QEXPQA(kFm,kAM),QEXPQF(kAm,kFm))
		 endif
		      !====================later	
			  infoind=infoind+1
		
			itxtinfo=gmcreatetextentry(infopanel,1,infoind,12,1,&
			'Mechanism file='//qfilem(1:40),32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						 infoind=infoind+1
		
			itxtinfo=gmcreatetextentry(infopanel,1,infoind,12,1,&
			'Name='//models(igraph2)%title_model(1:40),32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						 infoind=infoind+1
		
			itxtinfo=gmcreatetextentry(infopanel,1,infoind,12,1,&
			'Rate title='//ratcons(indk)%title(1:40),32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
			infoind=infoind+1

			call intconv(npar,cstring)
			nlv=nblank1(vtext(1))
			itxtinfo=gmcreatetextentry(infopanel,1,infoind,12,1,&
			vtext(1)(1:nlv)//cstring(1:3),32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
			nlv=nblank1(vtext(2))		
			infoind=infoind+1
			call intconv(nfix,cstring)
			itxtinfo=gmcreatetextentry(infopanel,1,infoind,12,1,&
			vtext(2)(1:nlv)//cstring(1:3),32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
			infoind=infoind+1
			nlv=nblank1(vtext(3))
			call intconv(neq,cstring)
			itxtinfo=gmcreatetextentry(infopanel,1,infoind,12,1,&
			vtext(3)(1:nlv)//cstring(1:3),32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
			nlv=nblank1(vtext(4))
			infoind=infoind+1
			call intconv(nmr,cstring)
			itxtinfo=gmcreatetextentry(infopanel,1,infoind,12,1,&
			vtext(4)(1:nlv)//cstring(1:3),32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
			infoind=infoind+1
			nlv=nblank1(vtext(5))
			call intconv(nfixec50,cstring)
			itxtinfo=gmcreatetextentry(infopanel,1,infoind,12,1,&
			vtext(5)(1:nlv)//cstring(1:3),32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
			
			infoind=infoind+1
			call intconv(kfit,cstring)
			nlv=nblank1(vtext(6))
			itxtinfo=gmcreatetextentry(infopanel,1,infoind,12,1,&
			vtext(6)(1:nlv)//cstring(1:3),32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
		penfac=10.0d0
		
		if(initi.eq.1.or.icprev.eq.-102) then
		 callid=4011
		 goto 2
		 initi=0
		endif
		xs=sngl(assmax)
		fac=sngl(penfac)
		xs1=sngl(ratemax)
		call gmremovewindow(ivwin)
		call gmremovewindow(Form1(igraph2,1))
		call gmremovewindow(Form1(igraph2,2))
		call penaly(main,ipeny,ipeny_yes,ipeny_no,ipeny_xs,xs,ipeny_xs1,xs1,ipeny_fac,fac)
	case(4003)		
		imess=gmdisplaymessagebox('','Now reset Constraints/MR/Fix',ginformation,gok)
	CASE(4005) !(710)
		istat1=gmEnqToggleSwitch(ipeny_yes)
		istat2=gmEnqToggleSwitch(ipeny_no)
			if(istat1.eq.gon) then
				dcmodel=.true.
			else
				dcmodel=.false.
			endif
			xs=gmenqvaluesetting(ipeny_xs)
			fac=gmenqvaluesetting(ipeny_fac)
			xs1=gmenqvaluesetting(ipeny_xs1)
			assmax=dble(xs)
			penfac=dble(fac)
			ratemax=dble(xs1)
			call gmremovewindow(ipeny)
!			call gmdisplaymessagebox('','OK.Proceed with MR/Fitting',ginformation,gok)
		
		

		 callid=4011
		 goto 2
	case(4011:4499)		  
		 call fitting(callid,igraph2,main,Form1,Form1_TxtArray1,thetaf,jfix1,tint,ampl,iprops,&
			Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,XAF,XFA,QexpQA,QexpQF,&
			alpha2,beta2,aloglik,kab,kAm,kFm,km,irt,nset,idestf,eqfit,nsim,&
			abort,autosim,endsim,neq,nsetq,thsav,titlep,infopanel,infoind,infolength,indk,ratcons)
		    infx=28
						info_pan(8)=gmCreatePanel(imainpanel, infx, 1, 4, 1,gmtitle=text_tog(8), &
              			gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmborderType=GINVERSECHISEL, &
              			gmLineCol=0, gmLineBorder=0, gmFillCol=221, gmFillBorder=0)
						ict=4
					!	if(i.eq.2.or.i.eq.4.or.i.eq.5.or.i.eq.7) then
						itextinfo=gmCreateTextEntry(info_Pan(8), 0, 0, 4, 1,text_tog(8), 255, GDISPLAY, gmBack1Col=0, &
						gmBack2Col=12, gmTextCol=1 , gmVpos=GTOP,gmsize=80)
!		 if(idestf.eq.2) then
!			callid=4501
!			goto 2
!		 endif

case(4500)
	! skip fit
case(4501:4506,4511:4515,4521:4522,4531:4533,4551:4553) ! plot  param
 !!!   iplot=1
 	logt=.false.
	iopt=1
 !	 call radio_panel(Main,Iradio,6,iradio_toggle,radio_text,0,iradio_data,radio_data,4653)

	iplot=callid-4500
	if(iplot.ge.11.and.iplot.le.15) iplot=1
	if(iplot.ge.21.and.iplot.le.25) iplot=2
	if(iplot.ge.31.and.iplot.le.33) iplot=3
	if(iplot.ge.51.and.iplot.le.53) iplot=5
	if(nodata) then	!must allocate tint anyway before call
	   nd1=1
	   nd2=1
	   if(.not.allocated(tint)) then
		ALLOCATE(tint(1,1),ampl(1,1),iprops(1,1))
	   endif
	endif
	kF=kB+kC+kD
	if(models(igraph2)%n.ne.kA+kF) then
	 
	endif

	

	ONESET=.true.	!so HJCLIK calcs pdf for set #iset ONLY
	iset=1
	tres=tresd(iset)
	tres1=sngl(1.d3*tres)	!tres in msec
	if(tres.eq.0.d0) tres1=0.0001
	ONESET=.true.	!so HJCLIK calcs pdf for set #iset ONLY
	iset=1
	iset9=iset
	tres=tresd(iset)
	tres1=sngl(1.d3*tres)	!tres in msec
	if(tres.eq.0.d0) tres1=0.0001
	treso=tres1
	tresg=tres1
	l=1
	if(iplot.eq.6.and.(nlig.eq.1.or.nset.eq.1)) then
	   iopth=1	!defaults
	   hjmcalc=.false.	!hjc Popen not yet calculated4690
	   callid=4850	!!!86
	   goto 2
	endif
	if(nset.gt.1) then
	   do j=1,nset
		
		radio_text(j)='set '//char(48+j)//',ligands:'//ligname(1)//'+'// ligname(2)
	   enddo
	   radio_text(20)=' '
	call radio_panel(main,iradio,nset,iradio_toggle,radio_text,0,&
	iradiox,radiox,iradioy,radioy,4510)

	   !call toggle_panel(Main,ITOGGLE,itogglepanel,nset,text_tog,intoggle,4510,valdat,idat,itogbutton)
 
	else
	 iset=1
	 iset9=iset
	 
	 callid=4510
	 goto 2  
	endif
    case(4510)
	if(nset.gt.1) then
	do i=1,nset
   	 istatust=gmEnqToggleSwitch(iradio_toggle(i))
	 if (istatust.eq.gon) then
	 iset=i
	 call gmremovewindow(iradio)
	 goto 55
	 endif
	enddo
	endif
	iset9=iset
55	tres=tresd(iset)
	call gmsettextsetting(itty,' Plot for set'//char(48+iset))
	do i=1,nlig
         write(stringtty,fmt='(a16,a10,g13.6)') 'concentration of:',ligname(i),conc(i,iset)*1.e6
		 CALL GMSETTEXTSETTING(ITTY,stringtty)
         if(discprt) write(7,67) ligname(i),conc(i,iset)*1.e6
67       format(/,'   concentration of ',a10,' = ',g13.6,/)
	enddo
	interp=.false.
	replot=.false.
	nbo=0		!not used here (only in hjclik)
	nbg=0

	tres1=sngl(1.d3*tres)	!tres in msec
	if(tres.eq.0.d0) tres1=0.0001
	   treso=tres1
	   tresg=tres1

	corprev=.false.	!correl'n dep on prev gap (used for iplot=3)
	cornext=.false.	!correl'n dep on next gap (used for iplot=3)
	
    iset9=iset
	call QNEW_HJC(QT,iset,conc,QD,ncdep,nlig,IL,IX,JX,k,km,km)
	if(kfit.eq.0) kfit=models(jgraph)%npar-nfix-neq-nmr-nfixec50
	sm=HJCLIK(-1,kfit,THETAf,tint,ampl,iprops,nd1,nd2,Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,&
    XAF,XFA,QexpQA,QexpQF,alpha2,beta2,aloglik,kab,kAm,kFm,km,irt,npar,IQf,irate1,jrate1,nlig)
	ncalc=512
		select case(iplot)
			case(1)
			callid=4600
			hdisp=.true.
			if(callid.eq.11) iopt=1
			if(callid.eq.12) iopt=2
			if(callid.eq.13) iopt=3
			if(callid.eq.14) iopt=4
			if(callid.eq.15) iopt=5
			goto 2
		case(2)
		hdisp=.true.
			callid=4600
			if(callid.eq.21) iopt=1
			if(callid.eq.22) iopt=2
			if(callid.eq.23) iopt=3
			if(callid.eq.24) iopt=4
			if(callid.eq.25) iopt=5
			goto 2
		case(3)
		hdisp=.true.
		  nrange=1
		  itype3=3
		  if(callid.eq.31) itype3=1
			if(callid.eq.32) itype3=2
			if(callid.eq.33) itype3=3
		  !radio_text(1)=' (1) Show open times conditional on PRECEDING shut time'
		  !radio_text(2)=' (2) Show open times conditional on FOLLOWING shut time'
          !radio_text(3)=' (3) Show open times conditional on EITHER shut time'
		  !	radio_text(20)=' '
!	      call toggle_panel(Main,ITOGGLE,itogglepanel,3,text_tog,intoggle,4700,valdat,idat,itogbutton)
 !call radio_panel(main,iradio,3,iradio_toggle,radio_text,0,iradio_data,radio_data,4700)
			callid=4700
			goto 2
		case(4)
		 if(readini) then
			nrange=nrange4
	
			do i=1,nrange
				ylo(i)=ylod(i)
				yhi(i)=yhid(i)
		 
			enddo

		    nrange1=nrange		!signals use of default ranges
	        text_tog(1)=' Number of gap length ranges (<= 20)  '
			val(1)=nrange1
	     else
			nrange1=1		!so doesn't think default wanted
			 text_tog(1)=' Number of gap length ranges (<= 20)  '
			val(1)=nrange
	   endif
	  
	   nvt=1
	   itypeval(1)=1
       call value_table(Main,ivwin,nvt,text_tog,ival,val,4800,itypeval)

		case(5)
			id1=1
				if(callid.eq.51) id1=1
			if(callid.eq.52) id1=2
			if(callid.eq.53) id1=3
			radio_text(1)=' (1) Distributions of open period-shut times'
			radio_text(2)=' (2) Distributions of shut time-shut time'
			radio_text(3)=' (3) Distributions of open period-open period'
			radio_text(20)=' '
             callid=4900
			 goto 2		
		!	  call toggle_panel(Main,ITOGGLE,itogglepanel,3,text_tog,intoggle,4900,valdat,idat,itogbutton)
 !call radio_panel(main,iradio,3,iradio_toggle,radio_text,idata,iradio_data,radio_data,4900)
	end select

case (4600)
	if(allocated(nopen)) DEALLOCATE(nopen)
	ALLOCATE(nopen(nint(iset)))
	if(allocated(tval)) DEALLOCATE(tval)
	ALLOCATE(tval(nint(iset)))
	in=1		!counter for intervals
	ng=0			!to count number of groups
	nopen(1)=0		!number of openings in group #ng
	j=0			!index in tval()
	islast=0		!index in tint() of last shut period
	call FINDOPEN(in,iset,iop,ibad,nbo,nbg,ampl,iprops,nd1,nd2)
	if(ibad.eq.-1) then
		callid=4651
		goto 2	!no good openings!
	endif
	in=iop		!make the opening the current obs
	nr0=0			!count number not on any range for iplot=4
	callid=4601
	goto 2

case(4601)
	topen=tint(in,iset)
	open=ampl(in,iset).ne.0.
!	if(.not.open) call GMSETTEXTSETTING(ITTY,'Error 1 in hjcdisp')
	bad0=BTEST(iprops(in,iset),3)	!tint(i) was unusable (bit 3='8' set)
	if(bad0) then	!bad opening, so end group
	   if(burst(iset)) then
		if(badend(iset)) then
		   callid=4611 !91	!end group at end of prev shutting
		goto 2
		else
		   callid=4613 !93	!abandon whole burst & look for next
		goto 2
		endif
	   else
		callid=4611 !91	!end group at end of prev shutting
	 goto 2
	   endif
	endif
	iopeni=in	!index in tint of the current opening
	inext=in+1
	if((corprev.or.iplot.eq.4).and.islast.gt.0.and.iopeni.eq.islast+1) then
	   call GETRANGE(tshut,topen,ylo,yhi,nrange,sy0,syy0,ny0,&
        sy2,syy2,ny2,sx0,sxx0,nx0,sx2,sxx2,nx2,deb,nr)
	   if(nr.eq.0) nr0=nr0+1	!tgap not in any range
	   	if(tshut.ge.ylo(1).and.tshut.lt.yhi(1)) then		!in range for pdf
		j=j+1
		tval(j)=topen		!for iplot=3 (prev gap, or both)
	   endif
	endif
! Record values for pdf of all openings (iplot=1)
	if(iplot.eq.1) then
	   j=j+1
	   tval(j)=topen		!in msec
	endif
	nopen(ng+1)=nopen(ng+1) + 1
	if(inext.gt.nint(iset)) then
	callid=4612
	goto 2
	endif	 !92
	in=inext		!should be shut -check, for debug anyway!
	open=ampl(in,iset).ne.0.
	if(open) then
	 call GMSETTEXTSETTING(itty,' INTERVAL  should be shut')
	endif
! Check for 2 adjacent gaps, or bad gap
	bad0=BTEST(iprops(in,iset),3)	!tint(i) was unusable (bit 3='8' set)
	if(in.lt.nint(iset)) then
	   bad1=ampl(in+1,iset).eq.0.    !also bad if next interval is shut too
	endif
	if(bad0.or.bad1) then
	   if(burst(iset)) then
		if(badend(iset)) then
		callid=4612
		   goto 2 !92	!end present group with prev opening
		else
		callid=4613
		   goto 2 !93	!abandon whole burst & look for next
		endif
	   else
	   callid=4612
		goto 2 !92	 !end present group with prev opening
	   endif
	endif
	tshut=tint(in,iset)
	if(burst(iset).and.tshut.gt.tcrit(iset)) then
	   if(cornext.or.iplot.eq.4)then
	   if(burst(iset).and.in.eq.iopeni+1) then

		call GETRANGE(tshut,topen,ylo,yhi,nrange,sy1,syy1,ny1,&
            sy2,syy2,ny2,sx1,sxx1,nx1,sx2,sxx2,nx2,deb,nr)
		if(nr.eq.0) nr0=nr0+1	!tgap not in any range
		
	      if(tshut.ge.ylo(1).and.tshut.lt.yhi(1)) then		!in range for pdf
		   j=j+1
		   tval(j)=topen		!for iplot=3 (following gap, or both)
	      endif
	   endif
	  
	endif
	 islast=in	!index in tint() of last shut period
	   callid= 4612 !92	!end present group with prev opening
		goto 2
	endif
	tshut=tint(in,iset)

	islast=in	!index in tint() of last shut period
	if(cornext.or.iplot.eq.4)then
	if(islast.eq.iopeni+1) then
	   call GETRANGE(tshut,topen,ylo,yhi,nrange,sy1,syy1,ny1,&
        sy2,syy2,ny2,sx1,sxx1,nx1,sx2,sxx2,nx2,deb,nr)
	   if(nr.eq.0) nr0=nr0+1	!tgap not in any range
	  
	   if(tshut.ge.ylo(1).and.tshut.lt.yhi(1)) then		!in range for pdf
		j=j+1
		tval(j)=topen		!for iplot=3 (following gap, or both)
	   endif
	endif
	endif
	if(iplot.eq.2) then
	   j=j+1
	   tval(j)=tshut		!in msec
	endif

	in=in+1
	if(in.gt.nint(iset)) then
	callid=4611 !91		!end of data -group ends with shutting
	goto 2
	endif
	open=ampl(in,iset).ne.0.
	if(.not.open) then
	call GMSETTEXTSETTING(ITTY,' INTERVAL  should be open')
	endif
	callid= 4601 !90
	goto 2	
case(4611)
	if(burst(iset).and.(.not.badend(iset))) then
	call GMSETTEXTSETTING(ITTY,' ERROR: burst cannot end with shut time')
	if(discprt) write(7,911) ng+1,nopen(ng+1)
911	   format(' ERROR: burst cannot end with shut time: Group ',i5,' nopen = ',i6)
	endif
	if(nopen(ng+1).eq.0) goto 73	!skip this group -look for next
	ng=ng+1		!update number of groups now #ng has ended
	if(in.ge.nint(iset)) then
	callid= 4651
	goto 2
	endif
73	nopen(ng+1)=0	!initialise number of openings in next group
	call FINDGAP(in,iset,is,ibad,nbo,nbg,ampl,iprops,nd1,nd2)
	if(ibad.eq.-1) then
	callid= 4651
	goto 2
	endif
	!? in=is		!index of the gap
	call FINDOPEN(in,iset,iop,ibad,nbo,nbg,ampl,iprops,nd1,nd2) !look for good opening
	if(ibad.eq.-1) then
	callid= 4651
	goto 2
	endif
	in=iop			!index of the opening
	callid=4601			!star
	goto 2
case(4612)
if(nopen(ng+1).eq.0) goto 74	!skip this group -look for next
	ng=ng+1		!update number of groups now #ng has ended
	if(in.ge.nint(iset)) then
	callid= 4651
	goto 2
	endif
74	nopen(ng+1)=0	!initialise number of openings in next group
	call FINDOPEN(in,iset,iop,ibad,nbo,nbg,ampl,iprops,nd1,nd2) !look for good opening
	if(ibad.eq.-1) then
	callid= 4651
	goto 2
	endif
	in=iop			!index of the opening
522	continue
	callid=4601
	goto 2		
case(4613)
	if(.not.burst(iset)) then		!should be!!
	  call GMSETTEXTSETTING(ITTY,' ERROR: should get here only for bursts')
	endif
	if(badend(iset)) pause 'Should not get here when bad gap ends grp'
933	call FINDGAP(in,iset,igap,ibad,nbo,nbg,ampl,iprops,nd1,nd2)
	if(ibad.eq.-1) then
	callid= 4651
	goto 2
	endif
	if(tint(igap,iset).lt.tcrit(iset)) then   !look again
	   in=igap+1
	   goto 933
	endif
	in=igap		!index of the gap
	call FINDOPEN(in,iset,iop,ibad,nbo,nbg,ampl,iprops,nd1,nd2) !look for good opening
	if(ibad.eq.-1) then
	callid= 4651
	goto 2
	endif
	in=iop			!index of the opening
	nopen(ng+1)=0	!reinitialise number of openings in next group
	callid=4601	
	goto 2
case(4651)
	nyval=j
	if(corprev.or.iplot.eq.4) then
	   title='Mean open time conditional on PRECEDING shut time'
	   CALL GMSETTEXTSETTING(ITTY,'Mean open time conditional on PRECEDING shut time')
	   call PRANGE(title,ylo,yhi,nrange,sy0,syy0,ny0,sx0,sxx0,nx0,&
     	yval,xval,ndv1,ndimd1,1,w,kwi,kwj,icurvw)
	endif
	if(cornext.or.iplot.eq.4) then
	   title='Mean open time conditional on FOLLOWING shut time'
		CALL GMSETTEXTSETTING(ITTY,'Mean open time conditional on FOLLOWING shut time')
	   call PRANGE(title,ylo,yhi,nrange,sy1,syy1,ny1,sx1,sxx1,nx1,&
     	yval,xval,ndv1,ndimd1,2,w,kwi,kwj,icurvw)
	endif
	if((corprev.and.cornext).or.iplot.eq.4) then
	   title='Mean open time conditional on EITHER shut time'
	   CALL GMSETTEXTSETTING(ITTY,'Mean open time conditional on EITHER shut time')
	   call PRANGE(title,ylo,yhi,nrange,sy2,syy2,ny2,sx2,sxx2,nx2,&
     	yval,xval,ndv1,ndimd1,3,w,kwi,kwj,icurvw)
	endif
	if(iplot.eq.4) then
	  
	   if(discprt) write(7,79) nr0
79	   format(' Number of shut times not in any range = ',i7,/)
	   CALL GMSETTEXTSETTING(ITTY,' Number of shut times not in any range')
	   callid=4690 !ok
	   goto 2
	endif
	if(allocated(nopen)) DEALLOCATE(nopen)
	callid=4652
	goto 2

case(4652)!99
	ibad=0
	if(ng.eq.0) then
	CALL GMSETTEXTSETTING(ITTY,' NO GROUPS FOUND')
	endif
	if(.not.sbin) NBIN=-1		!TO SIGNAL THAT NOT YET SET
	if(nyval.eq.0) then
	 imes=gmdisplaymessagebox('',' NO VALUES FOR DISTRIBUTION',gstop,gok)
	 goto 1
	endif
	call VARV1(tval,Nyval,xbar,varx,xmin,xmax)

	if(discprt) write(7,413) NYVAL,xbar,sqrt(varx),xmin,xmax
413	FORMAT(/,' Mean and SD of ',i5,' values= ',G13.6,' +/-',g13.6,&
     /,' Range from ',g13.6,' to ',g13.6)
	  write(string,fmt='(a20,i5,a10,g13.6,a4,g13.6,a12,g13.6,a4,g13.6)') &
	  ' Mean and SD of ',nyval,' values= ',xbar,'+/-',sqrt(varx),' Range from ',xmin,'to ',xmax
     
	  CALL GMSETTEXTSETTING(ITTY,string)
	xmin=0.0		!reset for histogram
	setmax=.false.	!xmax set automatically, not manually
	if(.not.sbin) then
	if(iplot.eq.3) then
		logt=.false.
		iopt=1
		  radio_text(1)= ' (1) Distribution of log durations'
     radio_text(2)= ' (2) Distribution of durations- set bins manually'
     radio_text(3)= ' (3) Distribution of durations- 20 bins'
     radio_text(4)= ' (4) Distribution of durations- 40 bins'
     radio_text(5)= ' (5) Distribution of durations- 60 bins'
     radio_text(6)= ' (6) Skip histogram'
	 call radio_panel(Main,Iradio,6,iradio_toggle,radio_text,0,&
	 iradiox,radiox,iradioy,radioy,4653)

	else
	callid=4653
	goto 2
	endif
!	logt=.false.
!	iopt=1
 !    radio_text(1)= ' (1) Distribution of log durations'
 !    radio_text(2)= ' (2) Distribution of durations- set bins manually'
 !    radio_text(3)= ' (3) Distribution of durations- 20 bins'
 !    radio_text(4)= ' (4) Distribution of durations- 40 bins'
!     radio_text(5)= ' (5) Distribution of durations- 60 bins'
!     radio_text(6)= ' (6) Skip histogram'
!	 call radio_panel(Main,Iradio,6,iradio_toggle,radio_text,0,iradio_data,radio_data,4653)

	else
		callid=4680
		goto 2
	endif
case(4654:4659)
	iopt=callid-4653
 case(4653)
 !	call gmremovewindow(iradio)
	if(iopt.eq.6) then
	   nbin=0
	   if(allocated(thetas)) deallocate(thetas)
		goto 1
	endif

	if(iopt.eq.1) logt=.true.
	nbw=1
	if(logt.or.iopt.ge.3) then
	callid=4662
	goto 2
	endif
    text_tog(1)=' Number of different bin widths (-1 to skip histo)'
    itypeval(1)=1
	val(1)=1
	call value_table(Main,ivwin,1,text_tog,ival,val,4661,itypeval)
case(4661)
	val(1)=gmenqvaluesetting(ival(i))
	nbw=int(val(1)) 
	callid=4662
	itypeval(1)=0
	call gmremovewindow(ivwin)
	goto 2
case(4662)
	if(nbw.eq.0) nbw=1
	if(nbw.le.-1) then
	   nbin=0
	   if(allocated(thetas)) deallocate(thetas)
	   goto 1
	endif
	tres=tresd(iset)
	xaxis(1)=sngl(tres*1.d3)	!in msec
	if(iopt.lt.3) then
	text_tog(1)=' Histogram to start at '
	if(logt.and.xaxis(1).le.0) text_tog(1)=' Histogram (log) to start at '
	val(1)=xaxis(1)
	if(logt.and.xaxis(1).le.0) val(1)=0.01
	call value_table(Main,ivwin,1,text_tog,ival,val,4663,itype)
	else
		callid=4664
		goto 2
	endif 
case (4663)
	val(1)=gmenqvaluesetting(ival(1))
	xaxis(1)=val(1) 
		if(logt.and.xaxis(1).le.0) xaxis(1)=0.01
	callid=4664
	itypeval(1)=0
	call gmremovewindow(ivwin)
	goto 2
case(4664)
	m=1
	if(logt) then
	   m=1
	   if(sbin) then
	     dx=exp(alog(10.)/float(nbdec))
		 xwbase=alog10(dx)
		 callid=4680
		 goto 2
	   else
		if(nyval.le.300) nbdec=5				!default
		if(nyval.gt.300.and.nyval.le.1000) nbdec=8	!default
		if(nyval.gt.1000.and.nyval.le.3000) nbdec=10	!default
		if(nyval.gt.3000) nbdec=12				!default
		text_tog(1)=' Number of bins/decade '
		val(1)=nbdec
		xendo=1.+xmax-amod(xmax,1.)
		text_tog(2)=' Last x value (ms) '
		val(2)=xendo
		call value_table(Main,ivwin,2,text_tog,ival,val,4665,itypeval)
		endif
	else
		m=1
		text_tog(1)=' Bin width (ms)'
		text_tog(2)='Last x value (pA etc)= '
		itypeval(1)=5
		itypeval(2)=5
	
		do i=1,nbw
		if(.not.sbin) then

		
		if(iopt.lt.3) then
			setmax=.true.		!xmax set manually
	   		mLAST=m
			val_2(1,i)=0.
			val_2(2,i)=0.
		
		else
			nbin=20
			if(iopt.eq.4) nbin=40
			if(iopt.eq.5) nbin=60
			dx=(xmax-xmin)/float(nbin)
			call SETTIC(dx)
			mlast=1
			do j=1,nbin
			    m=m+1
				xaxis(m)=xaxis(mlast)+(float(j))*dx
			enddo
			xsav(i,1)=float(nbin)
			xsav(i,2)=dx
			xsav(i,3)=xaxis(mlast)
			xsav(i,4)=xaxis(m)
		endif
		endif
		enddo
		if(iopt.ge.3) then
		callid=4678
		goto 2
		else
		call value_table_2(Main,iv2win,2,nbw,text_tog,ititle2,ival2,val_2,4670,itypeval)
		endif

	endif
case(4665)
	val(1)=gmenqvaluesetting(ival(1))
	val(2)=gmenqvaluesetting(ival(2))
	nbdec=int(val(1))
	if(val(2).ne.xendo) setmax=.true.
	xendo=val(2)
	dx=exp(alog(10.)/float(nbdec))
	nbin=1+ifix(alog(xendo/xaxis(1))/alog(dx))
	xendo=(dx**nbin)*xaxis(1)
	
	if(setmax) xmax=xendo
	do i=1,nbin
		xaxis(i+1)=xaxis(1)*(dx**i)
	enddo
	mlast=nbin+1
	xwbase=alog(dx)
	callid=4680
	goto 2
case(4670)
	do j=1,nbw
		setmax=.true.
		val_2(1,j)=gmenqvaluesetting(ival2(1,j))
		val_2(2,j)=gmenqvaluesetting(ival2(2,j))
		dx=val_2(1,j)
		xendo=val_2(2,j)
		xn=(xendo-xaxis(mlast))/dx

		itempg=IFIX(0.5+xn)
	   	if(abs(xn-float(itempg)).ge.dx*.01) then
			itemp=IFIXr(xn)
	   		xendo=xaxis(mlast) + (float(itempg))*dx
			call gmsetvaluesetting(ival2(2,j),xendo)
			call gmdrawwidget(ival2(2,j))
		endif	   
		nbin=IFIX(0.5+xn)
		mlast=m
			if(setmax) xmax=xendo
		if(nbin.gt.0) then
		do l=1,nbin
			m=m+1
			XAXIS(m)=XAXIS(mLAST)+(float(l))*DX
			xsav(j,1)=float(nbin)
			xsav(j,2)=dx
			xsav(j,3)=xaxis(mlast)
			xsav(j,4)=xaxis(m)
		enddo
		call gmsetfontattribs(ival2(1,j),gmtextcol=11)
		call gmsetfontattribs(ival2(2,j),gmtextcol=11)
		else
			nwrongbin=-1
			call gmsetfontattribs(ival2(1,j),gmtextcol=12)
		    call gmsetfontattribs(ival2(2,j),gmtextcol=12)
	
		endif 
			call gmsetvaluesetting(ival2(1,j),dx)
			call gmdrawwidget(ival2(1,j))
			call gmsetvaluesetting(ival2(2,j),xendo)
			call gmdrawwidget(ival2(2,j))
	enddo
	if(m.gt.501) then
			imes=gmdisplaymessagebox('','Too many bins',gstop,gok)
			! remove win
		callid=4653
		goto 2
	endif
	if(nwrongbin.eq.-1) then
		nwrongbin=0
		imes=gmdisplaymessagebox('','Please correct the values in red',gstop,gok)
	else
		! remove window
	
		callid=4678
		goto 2
	endif
case(4678)
	if(.not.sbin) then
		nbin=m-1
		xwbase=dx
		if(nbw.eq.1) then
			callid=4680
			goto 2
		else
			text_tog(1)=' Base width for frequency density= '
			val(1)=xwbase
			call value_table(Main,ivwin,1,text_tog,ival,val,4679,itypeval)	
		endif
    endif
case(4679)
	xwbase=gmenqvaluesetting(ival(1))
	callid=4680
	goto 2
case(4680)
	 
	 if(logt) then
	 if(discprt) write(7,1045) nbin,dx
1045 format(' Distribution of log(t) displayed- ',i6,' bins, factor= ',g11.4)
	 else
	 do i=1,nbw
	   if(discprt) write(7,2111) (xsav(i,j),j=1,4)
	
2111  format(1x,f5.0,' bins: width= ',g12.5,' from ',g12.5,' to ',g12.5)
	 enddo
	 if(discprt) write(7,1071) xwbase
1071 FORMAT(' Base width for frequency density= ',g13.6)
	 endif

	!yvalues
	!--------------------------------------------
	if(shist.and.sbin.and.sexp) goto 4071
	flo=0.
	fhi=0.
	do i=1,510
	   freq(i)=0.0
	enddo

	do 223 i=1,nyval
	yv=tval(i)
	if(yv.lt.xaxis(1)) then
	   flo=flo+1.0
	   goto 223
	endif
	if(yv.gt.xaxis(nbin+1)) then
	   fhi=fhi+1.0
	   goto 223
	endif
	do 221 j=1,nbin
	if(yv.ge.xaxis(j).and.yv.lt.xaxis(j+1)) freq(j)=freq(j)+1.0
221	continue	!j loop (bins)
	if(yv.eq.xaxis(nbin+1)) freq(nbin)=freq(nbin)+1.0
223	continue	!i loop (obs)
	if(allocated(tval)) DEALLOCATE(tval)

	if(flo.lt.0.1) flo=-2.	!do not plot if flo=0
	if(fhi.lt.0.1) fhi=-2.	!ditto
	fmax=0.
	do 218 j=1,nbin
	if(.not.logt) freq(j)=freq(j)*xwbase/(xaxis(j+1)-xaxis(j))
	if(freq(j).gt.fmax) fmax=freq(j)
218	continue
	if(flo.gt.fmax) fmax=flo
	if(fhi.gt.fmax) fmax=fhi
	i=ifix(0.1+flo)
	if(flo.lt.0.) i=0	!for print
	j=ifix(0.1+fhi)
	if(fhi.lt.0.) j=0

	if(discprt) write(7,222) i,j
222	FORMAT(' No of values below Xlow= ',i8,' No of values above Xhigh= ',i8)
	call intconv(i,cnum5)
	
	string=' No of values below Xlow= '//cnum5
	CALL GMSETTEXTSETTING(ITTY,string)
	call intconv(j,cnum5)
	string=' No of values above Xhigh= '//cnum5
	CALL GMSETTEXTSETTING(ITTY,string)
4071 continue
!	imes=gmdisplaymessagebox('',' Type frequency densities ',gquestion,gyesno)
!	if(imes.eq.gyes) then
!	endif
	if(flo.lt.0.) flo=0.
	if(fhi.lt.0.) fhi=0.
	callid=4690
	goto 2

case(4690)  !!!! 4062
!	if(iplot.eq.6) goto 86
	ilabel=1
	titley='Frequency '
	xw=xwbase  !so Y axis labelled 'per 10 ms' (reset below as nec)
	if(logt) xw=0.	!not so labelled for log-bins
	if(nodata) then
	   titley='Probability density'
	   xw=0.
	endif

	if(iplot.eq.1) then
	   titlex='Apparent open time (ms) '
	else if(iplot.eq.2) then
	   if(.not.burst(iset)) then
	      titlex='Apparent shut time (ms) '
	   else if(burst(iset)) then
	      titlex='App. shut time | t < tcrit (ms)'
	   endif
	else if(iplot.eq.3) then
	   if(itype3.eq.1) titlex='App open time (preceded by spec gap)'
	   if(itype3.eq.2) titlex='App open time (followed by spec gap)'
	   if(itype3.eq.3) titlex='App open time (adj to spec gap)'
	else if(iplot.eq.4) then
	   titlex='Adjacent shut time (mean)'
	   titley='Mean open time (adj to spec gap range) '
	   xw=0.
	endif
	callid=4850 ! 86 vplot 
	goto 2

case(4701:4703)
	itype3=callid-4700
case(4700) ! iplot=3
	! call gmremovewindow(iradio)
     if(itype3.eq.1.or.itype3.eq.3) corprev=.true. !correl'n dep on prev gap
	 if(itype3.eq.2.or.itype3.eq.3) cornext=.true. !correl'n dep on next gap
	 if(itype3.eq.1) then
	     
	     if(discprt) write(7,3031)
3031	     format(/,&
      ' Display pdf of durations of apparent openings that are',/,&
      '  PRECEDED by gaps with durations in following range (ms)',/)
	   else if(itype3.eq.2) then
	     
	     if(discprt) write(7,3032)
3032	     format(/,&
      ' Display pdf of durations of apparent openings that are',/,&
      '  FOLLOWED by gaps with durations in following range (ms)',/)
	   else if(itype3.eq.3) then
	     
	     if(discprt) write(7,3033)
3033	     format(/,&
      ' Display pdf of durations of apparent openings that are',/,&
      '  ADJACENT to gaps with durations in following range (ms)',/)
	   endif
		callid=4801
		goto 2
case(4800) ! iplot=4	
	  val(1)=gmenqvaluesetting(ival(1))
	  if(readini) then
		if(val(1).gt.20) then
		callid=4800
		goto 2
		endif
		nrange1=val(1)
		if(nrange1.ge.1) nrange=nrange1
	  else 
		if(val(1).lt.1) then
		callid=4800		
		goto 2
		endif
		nrange=val(1)
	  endif
       call gmremovewindow(ivwin)
	  if(discprt) write(7,2571)
2571	   FORMAT(/,' Mean durations of individual apparent openings that are',/,&
       '  adjacent to gaps with durations in following ranges (ms)',/)
		callid=4801
		goto 2
case(4801)
	   ndv1=nrange
	   ndimd=5
	   ndc1=ncalc	!need ncalc if theoretical curve plotted for iplot=4
	   ndimc=2
	
if(iplot.eq.4) then
		ncurvd=1
		iscal=1		!scale internally
	
endif
		kwi=100
		kwj=10
	   ndimd1=ndimd
	   ndimc1=ndimc
	if(allocated(xval)) DEALLOCATE(Xval,Yval)
	if(allocated(xcal)) DEALLOCATE(Xcal,Ycal)
	if(allocated(w)) DEALLOCATE(w)
	if(allocated(weight)) DEALLOCATE(weight)
	if(allocated(icurvd)) DEALLOCATE(icurvd)
	if(allocated(icurvc)) DEALLOCATE(icurvc)
	if(allocated(icurvw)) DEALLOCATE(icurvw)
	if(allocated(ndat)) DEALLOCATE(ndat)
	if(allocated(isym)) DEALLOCATE(isym)
	if(allocated(ijoin)) DEALLOCATE(ijoin)
	if(allocated(symsiz)) DEALLOCATE(symsiz)
	if(allocated(iline)) DEALLOCATE(iline)
	if(allocated(ncal)) DEALLOCATE(ncal)

	   ALLOCATE(XVAL(ndv1,ndimd1),YVAL(ndv1,ndimd1),XCAL(ndc1,ndimc1),YCAL(ndc1,ndimc1))
	  	  	ALLOCATE(icurvw(ndimd),w(kwi,kwj),weight(kwi,kwj))
	if(kmax.lt.1) kmax=1		!must allocate theta whether used or not
	
	ALLOCATE(ndat(ndimd),isym(ndimd),ijoin(ndimd),symsiz(ndimd))
	ALLOCATE(ncal(ndimc),iline(ndimc))
	ALLOCATE(icurvd(ndimd),icurvc(ndimc))
	   do i=1,ndimd1
		icurvw(i)=-1	!no SD unless reset below
	   enddo
	   kwi=100							!ditto
	   kwj=10							!ditto
	   kmax=100
	   do i=1,nrange
		sy0(i)=0.		!initialise
		syy0(i)=0.
		ny0(i)=0
		sy1(i)=0.
		syy1(i)=0.
		ny1(i)=0
		sy2(i)=0.
		syy2(i)=0.
		ny2(i)=0
		sx0(i)=0.		!initialise
		sxx0(i)=0.
		nx0(i)=0
		sx1(i)=0.
		sxx1(i)=0.
		nx1(i)=0
		sx2(i)=0.
		sxx2(i)=0.
		nx2(i)=0
	
	 enddo   
	 if(iplot.eq.4.and.nrange1.eq.0) then
		callid=4803
		goto 1
	 endif 
	 text_tog(1)='low t '
	 text_tog(2)='high t'
	 itype(1)=5
	 itype(2)=5
	 do i=1,nrange
	    call intconv(i,cnum5)
	    ititle2(i)=cnum5(1:3)
		val_2(1,i)=ylo(i)
		val_2(2,i)=yhi(i)
	 enddo
	 itcall=4802
	 call value_table_2(Main,iv2win,2,nrange,text_tog,ititle2,ival2,val_2,itcall,itype)
    
case(4802)
    
    iwrong=0
    do i=1,nrange
		val_2(1,i)=gmenqvaluesetting(ival2(1,i))
		val_2(2,i)=gmenqvaluesetting(ival2(2,i))
	    ylo(i)=val_2(1,i)
		yhi(i)=val_2(2,i)
	    if(ylo(i).gt.yhi(i)) then
		   a1=ylo(i)
	       ylo(i)=yhi(i)
		   yhi(i)=a1
		endif
	
		if(ylo(i).lt.tres1-0.00001) then
		ylo(i)=tres1	
	
		iwrong=1
			call gmsetfontattribs(ival2(1,i),gmtextcol=12)
				call gmsetvaluesetting(ival2(1,i),ylo(i))
		   
		endif
		if((iplot.eq.3.or.iplot.eq.4).and.burst(iset).and.&
     		  ylo(i).gt.tcrit(iset))then
		 ylo(i)=tcrit(iset)
		 	call gmsetfontattribs(ival2(1,i),gmtextcol=12)
				call gmsetvaluesetting(ival2(1,i),ylo(i))
		
		 iwrong=1
	    if(discprt) write(7,342) ylo(i),tcrit(iset)
342		   format(/,&
      ' WARNING: the lower limit, ',f10.2,' ms is above tcrit = ',&
        g13.6,' ms',/,&
      ' and so uses shut times that are not predicted by fit when',&
      ' fitting bursts',/)
		endif
	enddo
	if(iwrong.eq.1) then 
	 ime=gmdisplaymessagebox('','Low t was < resolution / >tcrit.Accept red values ?',&
	 gquestion,gyesno)
	
	if(ime.eq.gnobutton) then
	 goto 1
    endif
	endif
	call gmremovewindow(ivalwin) 
	callid=4803
	goto 2
	
case(4803)
     if(iplot.eq.4) then
	   do i=1,20
		ylod(i)=ylo(i)
		yhid(i)=yhi(i)
		nranged=nrange
	   enddo
	endif   
	callid=4600
	goto 2   

case(4850) ! vplot+vhist iplot=1,2,3,4,6	! 86
	autplt=.false.
	draft=.false.
	plotonly=.false.
	doframe=.true.
	landscap=.true.
	fitted=.true.
	cbig=2.5
	ifont=4
	isval=0	!no arrow
	xcross=0.0
	ycross=0.0
	inumx=-1	!X axis in fixed format if log scale
	ntx=5		!if not logt set initial input values
	nty=5
	itx=1
	ity=1
	iask=-2	!do not ask before leaving display; delete graph
	xlo=-1
		if(iplot.eq.4.or.iplot.eq.6) then	!use VPLOT for mean open vs adj gap
	 !!!! if(replot) goto 861
	   replot=.false.
	   tres1=sngl(1.d3*tres)    !reset here (may have beeen set to log value)
	   isetcol=1	!use icol() for colour if not -1
	   do i=1,100
		icol(i)=-1	!default colour
	   enddo
	   hdisp=.false.
       callid=4890 ! vplot
	   goto 2
	
	endif
	 
	
	if(allocated(xval)) DEALLOCATE(Xval,Yval)
	if(allocated(xcal)) DEALLOCATE(Xcal,Ycal)
	if(allocated(w)) DEALLOCATE(w)
	if(allocated(icurvd)) DEALLOCATE(icurvd)
	if(allocated(icurvc)) DEALLOCATE(icurvc)
	if(allocated(icurvw)) DEALLOCATE(icurvw)
	if(allocated(ndat)) DEALLOCATE(ndat)
	if(allocated(isym)) DEALLOCATE(isym)
	if(allocated(ijoin)) DEALLOCATE(ijoin)
	if(allocated(symsiz)) DEALLOCATE(symsiz)
	if(allocated(iline)) DEALLOCATE(iline)
	if(allocated(ncal)) DEALLOCATE(ncal)

	ndimd=1
	ndimc=10
	ifitype=0
	ncurvd=1
		iscal=1		!scale internally
	ndv1=511	!dimensions as for earlier versions
	ndimd=1
	ndc1=2048
	ndimc=10
	ALLOCATE(Xval(n1:ndv1,ndimd),Yval(n1:ndv1,ndimd))
	ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
	ALLOCATE(icurvw(ndimd),w(kwi,kwj))
	if(kmax.lt.1) kmax=1		!must allocate theta whether used or not
	
	ALLOCATE(ndat(ndimd),isym(ndimd),ijoin(ndimd),symsiz(ndimd))
	ALLOCATE(ncal(ndimc),iline(ndimc))
	ALLOCATE(icurvd(ndimd),icurvc(ndimc))
	if(nodata) then
	   ncurvd=0
	else
	   icurvd(1)=1
	   ndat(1)=nbin
	   ijoin(1)=0
	   do i=1,nbin+1
		xval(i,1)=xaxis(i)
	   enddo
	   dx=xaxis(nbin+1) - xaxis(nbin)
	   xval(nbin+2,1)=xval(nbin+1,1) + dx		!extra bin in case fhi>0
	   if(setmax) xmax=xval(nbin+2,1)
	   do i=1,nbin
		yval(i,1)=freq(i)
	   enddo
	endif
	icomp=1
	nores=.false.
	if(iplot.eq.1.or.iplot.eq.2) then
	 text_tog(1)='(1) Show ''exact'' pdf only (2*exact + asymptotic)'
     text_tog(2)='(2) Show exact AND asymptotic distributions '
     text_tog(3)='(3) Show asymptotic distribution and its components also '
	 text_tog(4)='Show the pdf without missed events'
	 text_tog(5)='Rescale this pdf to unit area above t=tres'
	 
	 call toggle_panel(Main,ITOGGLE,ittoglepanel,5,text_tog,intoggle,4851,valdat,idat,itogbutton)
	! imes=gmdisplaymessagebox('','Show the pdf without missed events',gquestion,gyesno)
	! if(imes.eq.gyesbutton) then
	!	nores=.true.
	!	imes1=gmdisplaymessagebox('','Rescale this pdf to unit area above t=tres',gquestion,gyesno)
	!	if(imes1.eq.gyesbutton) scale=.true.
	 !endif
	
!call radio_panel(main,iradio,3,iradio_toggle,radio_text,idata,iradio_data,radio_data,4851)
	else
	callid=4851
	goto 2
	endif
case(4852,4853,4854)
if(callid.eq.4852) then
	imes=0
	istatus0=gmEnqToggleSwitch(inToggle(1))
	if(istatus0.eq.gon) then 
		call gmSetToggleSwitch(inToggle(2), Goff)
		call gmSetToggleSwitch(inToggle(3), Goff)
		icomp=1
	endif
else if(callid.eq.4853) then
	imes=0
	istatus0=gmEnqToggleSwitch(inToggle(2))
	if(istatus0.eq.gon) then 
		call gmSetToggleSwitch(inToggle(1), Goff)
		call gmSetToggleSwitch(inToggle(3), Goff)
		icomp=2
	endif
else
	imes=0
	istatus0=gmEnqToggleSwitch(inToggle(3))
	if(istatus0.eq.gon) then 
		call gmSetToggleSwitch(inToggle(1), Goff)
		call gmSetToggleSwitch(inToggle(2), Goff)
		icomp=3
	endif
endif
	
case(4855)
imes=0
	istatus0=gmEnqToggleSwitch(inToggle(4))
	if(istatus0.eq.gon) then 
    
		nores=.true.
		call gmSetToggleSwitch(inToggle(4), Gon)
	else
		nores=.false.
		scale=.false.
		call gmSetToggleSwitch(inToggle(4), Goff)
		call gmSetToggleSwitch(inToggle(5), Goff)
	endif
case(4856)
	if(nores) then
		scale=.true.
		call gmSetToggleSwitch(inToggle(5), Gon)
	else
		scale=.false.
		call gmSetToggleSwitch(inToggle(5), Goff)
	endif
case(4851)
	if(iplot.eq.1.or.iplot.eq.2) call gmremovewindow(itoggle)
	xmin=0.
	if(nodata) then	!get xmax, for calculation of xcal()
	  imes=gmdisplaymessagebox('','Plot p.d.f. of log(t)',gquestion,gyesno)
	   if(imes.eq.gyesbutton) logt=.true.
	   if(iplot.eq.1.or.iplot.eq.3) then		!open
		xmax=5.0*sngl(-1.d3/rootA(kA))		!slowest=last
	   else if(iplot.eq.2) then		!shut
		xmax=5.0*sngl(-1.d3/rootF(kF))		!slowest=last
	   endif
	endif
	if(logt) then
	   xmin=0.010001	!10 microseconds should be below any tres!
	   xmax=2.*xmax	!make bigger for log display
	   xmin=alog10(xmin)
	   xmax=alog10(xmax)
	endif
	
	if(iplot.eq.1.or.iplot.eq.3) then	!open
	   ncomp=kA
	   do m=1,kA
		tau(m)=sngl(-1.d3/rootA(m))	!define tau for asymptotic pdf
		amp(m)=sngl(ampA(m))		!define amplitude for ditto
	   enddo
	   do m=1,k
		g00(m)=g00A(m)
		g10(m)=g10A(m)
		g11(m)=g11A(m)
	   enddo
	else if(iplot.eq.2) then
	   ncomp=kF
	   do m=1,kF
		tau(m)=sngl(-1.d3/rootF(m))	!define tau for asymptotic pdf
		amp(m)=sngl(ampF(m))		!define amplitude for ditto
	   enddo
	   do m=1,k
		g00(m)=g00F(m)
		g10(m)=g10F(m)
		g11(m)=g11F(m)
	   enddo
	endif
19	continue
	tres1=sngl(1.d3*tres)	!tres in msec
	if(tres.eq.0.d0) tres1=0.0001
	tres2=2.*tres1
	tres3=3.*tres1
	if(logt) then		!xmax already in log units
	   dx1=(xmax-alog10(tres1))/float(ncalc)
	   tres1=alog10(tres1)
	   tres2=alog10(tres2)
	   tres3=alog10(tres3)
	else
	   dx1=(xmax-tres1)/float(ncalc)
	endif
	ires2=1+ifix(1.0+(tres2-tres1)/dx1)	!index of point just below tres2
	ires3=1+ifix(1.0+(tres3-tres1)/dx1)	!index of point just below tres3
	xcal(1,1)=tres1
	do i=2,ncalc
	   xcal(i,1)=tres1+(float(i-2))*dx1      !so xcal(2,1)=tres1 also
	enddo
	if(logt) then
	   tres1=10.**tres1
	   tres2=10.**tres2
	   tres3=10.**tres3
	   do i=1,ncalc
	      xcal(i,1)=10.**xcal(i,1)
	   enddo
	endif
	fac=(float(nyval))*xwbase/1000.
	if(nodata) fac=1.0	!to get p.d.f.

	if(burst(iset).and.iplot.eq.2) then
	   tcrit2=dble(tcrit(iset))*1.d-3	!in sec
	   Ptc=FTCt(tcrit2,tres,k,kA,kF,ucol,Z00F,Z10F,Z11F,XFA,kAm,kFm,km)
	   fac=fac/sngl(ptc)
	   if(discprt) write(7,603) tcrit(iset),ptc
603	   format(' Prob[observed shut time < ',f8.3,' ms] = ',g13.6)
	   if(ptc.gt.1.d0.or.ptc.lt.0.d0) then
	 imes=gmdisplaymessagebox('',' ILLEGAL PROBABILITY!',ginformation,gok)
		if(discprt) write(7,6031)
6031		format(' ILLEGAL PROBABILITY!')
		pause
	   endif
	endif
	j0=1		!for unconditional pdf
	if(iplot.eq.3) then
	   j1=2
	   j2=3
	   call POPADJ(tres,ylo,yhi,ncalc,k,kA,kF,ycal,xcal,j1,j2,den1,&
         QEXPQA,QEXPQF,Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,&
         XAF,XFA,kAm,kFm,kAx,km)
	   j0=1	!for unconditional pdf
	endif
	ycal(1,j0)=0.0	!to draw vertical line

	do i=2,ncalc
	   t=xcal(i,j0)
	   time0=dble(t)
	   if(i.le.ires2) then
		ycal(i,j0)=sngl(F0HJC(time0,g00,eigen,tres,k,km))
	   else if(i.gt.ires2.and.i.le.ires3) then
		ycal(i,j0)=sngl(F1HJC(time0,g00,g10,g11,eigen,tres,k,km))
	   else         		!asymptotic pdf
		f=0.0
		do m=1,ncomp
		   fc0=amp(m)*exp(-(t-tres1)/tau(m))	!NB needs excess time here
		   f=f + fc0
		enddo
		ycal(i,j0)=f
	   endif
	enddo		!do i=2,ncalc

	jval=1
	if(iplot.eq.3) jval=3   !uncond + conditional (pre=gap + post-gap)
	do j=1,jval
	 if(logt) then
	   do i=1,ncalc
		ycal(i,j)=fac*ycal(i,j)*xcal(i,j)*2.30259	!f(log10(t))=2.3*t*f(t)
	   enddo
	 else
	   do i=1,ncalc
		ycal(i,j)=fac*ycal(i,j)
	   enddo
	 endif
	enddo
	if(iplot.eq.1.or.iplot.eq.2) then
	   isetcol=1
	   icol(1)=9	!blue for histogram
	   icol(11)=9	!blue for HJC dist
	   ja=0
	   jz=0
	   if(icomp.eq.1) then
		jz=2	!for zero res
		ncurvc=1
		iline(1)=0		!total curve continuous
		if(nores) ncurvc=2
	   else if(icomp.eq.2) then
		ja=2	!for asymptotic
		jz=3	!for zero res
		ncurvc=2
		if(nores) ncurvc=3
	   else if(icomp.eq.3) then
		ja=2	!for asymptotic
		m1=3		!first col for components (icomp=3 only)
		m2=ncomp+2	!last col for components (icomp=3 only)
		do i=m1,m2
		   iline(i)=2		!short dash for components
		   icol(i+10)=i
		enddo
		jz=ncomp+3	!for zero res
		ncurvc=ncomp+2
		if(nores) ncurvc=ncomp+3
	   endif
	   if(jz.gt.0) then
		iline(jz)=3		!long dash for zero res
		icol(jz+10)=12		!red for zero res
	   endif
	   if(ja.gt.0) then
		icol(ja+10)=10		!green for asymptotic
		iline(ja)=1		!dotted for asymptotic
	   endif
	   do i=1,ncurvc
		icurvc(i)=i		!total
		ncal(i)=ncalc
	   enddo
	endif

	if(iplot.le.2.and.NORES) then	!calc pdfs by CH82 methods
	   call EQOC_HJC(QD,peq1,k,km,km)
	   call PHIo1(QD,Peq1,phio,km)	
	   if(iplot.eq.1) then
	 	kX=kA
	 	call PDFopen(QD,phio,area,tau1,kA,km)
	   else if(iplot.eq.2) then
		kX=kF
		call PDFshut(QD,phio,area,tau1,kF,km)

	endif

	   fac1=fac
	   if(scale) then
		f=0.0
	      do m=1,kX
		   f=f + area(m)*exp(-tres1/tau1(m))	!area above tres
		enddo
		fac1=fac1/f					!new scale factor
	   endif
	   xmin1=0.001001	  !calc from 1 microsec for zero res
	   if(logt) xmin1=alog10(xmin1)	!xmax already log in this case
	   dx2=(xmax-xmin1)/float(ncalc)
	   do i=1,ncalc
		t=xmin1+(float(i-1))*dx2       !start at 1 microsec
		if(logt) t=10.**t
		xcal(i,jz)=t
		f=0.0
		do m=1,kX
		   f=f + (area(m)/tau1(m))*exp(-t/tau1(m))
		enddo
		if(logt) f=f*t*2.30259		!f(log10(t))=2.3*t*f(t)
		ycal(i,jz)=f*fac1*1000.
	   enddo
	endif		!end of zero res calcs
	if(icomp.eq.2.or.icomp.eq.3) then  !define separate xcal for asymp
	   dx2=(xmax-xmin)/float(ncalc)
	   
	   do i=1,ncalc
		t=xmin+(float(i-1))*dx2       !start at xmin
		if(logt) t=10.**t
		xcal(i,ja)=t
		if(icomp.eq.3) then
		   do m=m1,m2
			xcal(i,m)=xcal(i,ja)
		   enddo
		endif
	   enddo
	   do i=1,ncalc
		f=0.0
		t=xcal(i,ja)
		t1t=t-tres1		!needs EXCESS time here
		do m=1,ncomp
		   fc0=amp(m)*exp(-t1t/tau(m))	!NB needs excess time here
		   f=f + fc0
		   if(logt) fc=fc0*t*2.30259		!f(log10(t))=2.3*t*f(t)
		   if(icomp.eq.3) then
			ycal(i,m+2)=fc0*fac	!components in ycal(i,3),ycal(i,4),...
		   endif
		enddo
		if(logt) f=f*t*2.30259		!f(log10(t))=2.3*t*f(t)
		ycal(i,ja)=f*fac                !total asymptotic in ycal(i,ja)
	   enddo
	endif
	if(iplot.eq.3) then
	hdisp=.true.
	!	   (1)=uncond; (2)=prec gap; (3)=following gap (see POPADJ)
	   isetcol=1
	   icol(1)=9	!blue for histogram
	   icol(11)=12	!red for unconditional
	   icol(12)=9	!blue for fitted curve
	   icol(13)=9	!blue for fitted curve
	   do i=1,3
		ncal(i)=ncalc
	   enddo
	   iline(1)=2           !short dash for unconditional
	   ncurvc=2
	   if(itype3.eq.1.or.itype3.eq.3) then
	     icurvc(1)=1				!unconditional
	     icurvc(2)=2				!prec gap
	     iline(1)=2
	     iline(2)=0           !continuous for preceding gap
	     
	     imes=gmdisplaymessagebox('Calculated curves shown:',&
     ' (1) Unconditional pdf of apparent openings (short dash) and &
       (2) pdf for openings preceded by spec gaps (contin line)',gquestion,gyesno)
     
	   else if(itype3.eq.2) then
	   	icurvc(1)=1		!unconditional
		icurvc(2)=3		!following gap
	      iline(1)=2
		iline(3)=0           !contin for following gap
		imes=gmdisplaymessagebox('Calculated curves shown:',&
     ' (1) Unconditional pdf of apparent openings (short dash) and &
       (2) pdf for openings followed by spec gaps (contin line)',gquestion,gyesno)
    
	   endif
	   if(imes.eq.gyesbutton) then
			callid=4860
			goto 2
	   endif
	   iline(1)=2           !short dash for unconditional
	   iline(2)=0           !continuous for preceding gap
	   iline(3)=3           !long dash for following gap
	   
       radio_text(20)=' Options for calculated curves: choose 1 to 3'
       radio_text(1)=' (1) Unconditional pdf of apparent openings (short dashes)'
       radio_text(2)=' (2) pdf for openings preceded by spec gaps (contin line)'
       radio_text(3)='(3) pdf for openings followed by spec gaps (long dashes)'
       call radio_panel(main,iradio,3,iradio_toggle,radio_text,0,&
	   iradiox,radiox,iradioy,radioy,4859)
	else
	hdisp=.true.
	callid=4860
	goto 2
	endif
	
!	call toggle_panel(Main,ITOGGLE,ittoglepanel,3,text_tog,intoggle,4855,valdat,idat,itogbutton)


case (4859)
    ncurvc=0
	do i=1,3
		istat5=gmenqtoggleswitch(iradio_toggle(i))
	if(istat5.ne.0) then
		ncurvc=ncurvc+1
		icurvc(i)=i
	endif
	enddo
	call gmremovewindow(iradio)
	callid=4860
	hdisp=.true.
	goto 2

case(4890)
	if(iplot.eq.4) then
	iscal=1
	   isym(1)=-7	!circle for data set 1 -preceding gap range
	   isym(2)=-3	!square for data set 2 -following gap range
	   isym(3)=-4	!diamond for data set 3 -both
	   ijoin(1)=0	!cont line joins data pnts
	   ijoin(2)=0	!cont line joins data pnts
	   ijoin(3)=0	!cont line joins data pnts
	   do j=1,5
	 	ndat(j)=nrange
!!!	      syms(j)=3.0		!internal default=2.5
	   enddo
	   ijoin(4)=-1   !not joimed
	   ijoin(5)=-1   !not joimed
! Hollow symbols for calc points
	   isym(4)=7	!circle for calc set 1 -preceding gap range
	   isym(5)=-3	!solid square for calc set 2 -following gap range

	  
	   if(nodata) then
		ncurvd=0
	   else
	     ians=gmdisplaymessagebox('',&
       ' Show observations only for adjacent (before OR after) gap',gquestion,gyesno)
	    if(ians.eq.gyesbutton) then
		   ncurvd=3
		   do j=1,ncurvd
			icurvd(j)=j+2		!=3,4,5
		   enddo
		   icol(1)=10	!green for data set 3  -both
		   icol(2)=11	!light blue for calc set 1 -preceding gap range
		   icol(3)=9	!dark blue for calc set 2    -following gap range
		else
		   ncurvd=5
		   do j=1,ncurvd
	 		icurvd(j)=j
		   enddo
		   icol(1)=14	!yellow for data set 1 -preceding gap range
		   icol(2)=12	!red for data set 2    -following gap range
		   icol(3)=10	!green for data set 3  -both
		   icol(4)=11	!light blue for calc set 1 -preceding gap range
		   icol(5)=9	!dark blue for calc set 2    -following gap range
	      endif
	   endif
! Define Ycal1(i,1), xcal1(i,1), i=1,nrange -
	   tres1=sngl(1.d3*tres)	!tres in msec
	   do i=1,nrange	!restore original ranges in case reset for another set
		ylo(i)=ylod(i)
		yhi(i)=yhid(i)
	   enddo
	   if(ylo(1).lt.tres1-0.00001) then
	        ylo(1)=tres1
	
	   endif

	   j1=4
	   j2=5
	   call MOPADJ(tres,ylo,yhi,nrange,QD,k,kA,kF,&
     	yval,xval,ndv1,ndimd1,den1,j1,j2,&
     	QEXPQA,QEXPQF,Z00F,Z10F,Z11F,XFA,kAm,kFm,km)
	   title=' CALCULATED values for mean open given preceding gap range'
	   call PCRANGE(title,ylo,yhi,nrange,ny0,den1,yval,xval,ndv1,ndimd1,j1)
	   title=' CALCULATED values for mean open given following gap range'
	   call PCRANGE(title,ylo,yhi,nrange,ny1,den1,yval,xval,ndv1,ndimd1,j2)
! Calc theoretical relationship in xcal1(i,j), ycal1(i,j) if req.
	   
	   ians=gmdisplaymessagebox('',&
     ' Show also the continuous (theoretical) relationship',gquestion,gyesno)
	   
	   ncurvc=0
	   if(ians.eq.gyesbutton) then
		ncurvc=2
		icurvc(1)=1
		icurvc(2)=2
		ncal(1)=ncalc
		ncal(2)=ncalc
		iline(1)=0		!contin
		iline(2)=3		!dashed
		icol(11)=14	!yellow for calc set 3 -preceding gap (continuous)
		icol(12)=12	!red for calc set 4    -following gap (continuous)
		j1=1
		j2=2
! Define xcal1(i,j1) in ms for calc curve (equally spaced on log scale)
		tmax=yhi(nrange)
		dx=(alog10(tmax) - alog10(tres1))/float(ncalc)
		do i=1,ncalc
		   tr=tres1
		   if(tres1.eq.0.0) tr=0.0001
		   x=alog10(tr) + float(i-1)*dx
		   xcal(i,j1)=10.0**x
		enddo
		call MOPADJc(tres,QD,k,kA,kF,ycal,xcal,ncalc,ndc1,ndimc,j1,j2,&
      QEXPQA,QEXPQF,Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,XFA,kAm,kFm,km)
	
	   endif

	   if(ncurvd.eq.5) then
          
          if(discprt) write(7,8)
8	    format(/,' Observations -preceding gap yellow, filled circles, cont. line'&
        ,/,' Observations -following gap red, filled squares, cont. line'&
        ,/,' Observations -adjacent gap green, filled diamonds, cont. line'&
        ,/,' Calculated -preceding gap light blue, open circles'&
        ,/,' Calculated -preceding gap dark blue, solid squares'&
        ,/,' Theoretical continuous relationship:',/,&
      '   preceding gap yellow curve, following gap red curve.',/)
	   else if(ncurvd.eq.3) then
          
          if(discprt) write(8,81)
81	    format(/,' Observations -adjacent gap green, filled diamonds, cont. line'&
        ,/,' Calculated -preceding gap light blue, open circles'&
        ,/,' Calculated -preceding gap dark blue, solid squares'&
        ,/,' Theoretical continuous relationship:',/,&
      '   preceding gap yellow curve, following gap red curve.',/)

	   endif
 
	endif	!end if iplot.eq.4
    autplt=.false.
     ilog=1		!y vs log(x)
	   iscal=1		!scale internally
	   itrace=0		!not multiple traces
	   if(nset.gt.1) then
		itit=1
		call INTCONV(iset,cnum5)
		title1(1:6)='Set '//cnum5(1:2)
	   endif
	   izoom=0
	   	redrawn=.false.
		hdisp=.false.
		callid=4862
		goto 2
case(4891)
case(4860)
       
			autplt=.false.
			!	rescale=.true.
				redrawn=.false.
				izoom=0
				iscal=1
		fmin=0.
		fmax=fmax*1.2		     !value from SETBIN
		itemp0=1+ifix(fmax)
		fmax=float(itemp0)	!ROUND UP
		ftic=0.5*10.0**ifix(alog10((fmax-fmin)*0.5))
2102	if((fmax-fmin)/ftic.gt.10.) goto 2101
		ftic=ftic/2.
		goto 2102	!ensure at least 5 tics so get numerical label
2101	xtic=2.*xwbase	!number every 10th bin
		xtic=xtic*5.
		if(.not.logt) ftic=ftic*5.
		lt2=2       !short dash for lo,hi bins
		ilog=0
		if(logt) ilog=6		!plot sqrt(y) vs log(t)
		if(logt) then
		do j=1,ncurvc
			do kl=1,ncal(j)
			if(ycal(kl,j).gt.0)	ycal(kl,j)=ycal(kl,j)/2.30259
			enddo
		enddo
		endif
		iscal=1		!scale internally
		ndv1=511	!dimensions as for earlier versions
		ndimd=1
		ndc1=2048
		ndimc=10
		if(nset.gt.1) then
			itit=1
			call INTCONV(iset,cnum5)
			title1='Set '//cnum5(1:2)
		endif
		! vhist
		! hdisp=.true.
		iparfirst=-100
		!autplt=.true.
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
		callid=4862
		goto 2
	case(4862)					
		call calc_default(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,symsiz,&
		ndimd,XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,&
								XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,&
								XLO1,XHI1,YLO1,YHI1,y0,yinf,inumx,inumy,&
								titlex,titley,ilabel,doframe,autplt,itit,title1,&
								ISHP,ifnt,landscap,ndv1,ndc1,w,kwi,kwj,icurvw,kmax,iver,&
								redrawn,plot,redo,pcgm,pwmf,npint,hdisp,n1,tlenx,tleny,nx1,ny1,izoom,&
								xtitle,ytitle,rescale,dxs,dys,ifitype,parval,theta,ncomp)
								isens=1
	igraph=igraph+1
	modplot=igraph
	do i=1,100
		idraw(i)=1
	enddo
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
						        
								ijplot=ijplot+24
								ixp=ixp0+ijplot
								iyp=iyp0+ijplot
								
							
						
	call graph1(igraph,iplot,main,ixp,iyp,graph1_1,GraphMainPanel1_1,graphics1_1,&
								wxmin,wxmax,wymin,wymax,ipos,gfile,plot,ixposv,iyposv)
							
							call draw_axis(xmin,xmax,ymin,ymax,ymin1,ymax1,&
							    xtic,ytic,nx1,ny1,xcross,ycross,itx,ity,tlenx,tleny,&
								logx,logy,sqrty,doframe,icol,ntx,nty,idev,thick,itype)
							call draw_text(parval,xtitle,ytitle,ztitle,title1,cnumx,cnumy,&
								cnumz,cexpx,cexpy,cexpz,newtext,numbx,numby,numbz,ntext,inumx,&
								inumy,intx,angle,sizetext,ijus,thick,rx,ry,idraw,&
								icol,ifnt,str,dxs,dys)
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
								iline,icol,thick,ndc1,ndimc,jfirst,dxs,dys)
							call draw_lines(IDRAW,ICOL,THICK,C_THICK,NARROW,NLINE,NHLINE,NVLINE, &
							XBEG,YBEG,XEND,YEND,y0,yinf,x0,dxs,dys)
							xmin0=xmin1
							xmax0=xmax1
							ymin0=ymin1
							ymax0=ymax1
							idest=0
							readrec=.true.				
	
	!	call gmSetWidgetStatus(izoom, GSELECTABLE)
	!	call gmSetWidgetStatus(interpolate, GSELECTABLE)
		call gmSetWidgetStatus(isave_bmp, GSELECTABLE)
		call gmSetWidgetStatus(export_file, GSELECTABLE)
		call gmSetWidgetStatus(print_file, GSELECTABLE)

			call gmSetWidgetStatus(jtitle_record, GSELECTABLE)
		call gmSetWidgetStatus(jparameters, GSELECTABLE)
		call gmSetWidgetStatus(jlabels, GSELECTABLE)
		call gmSetWidgetStatus(jnewtext, GSELECTABLE)
		call gmSetWidgetStatus(jnumbers, GSELECTABLE)
		call gmSetWidgetStatus(jaxis, GSELECTABLE)
		call gmSetWidgetStatus(jframe, GSELECTABLE)
		
		call gmSetWidgetStatus(jlines, GSELECTABLE)
		call gmSetWidgetStatus(new_lines_h, GSELECTABLE)
		call gmSetWidgetStatus(new_lines_V, GSELECTABLE)
		call gmSetWidgetStatus(jarrows, GSELECTABLE)
		call gmSetWidgetStatus(jraw_data, GSELECTABLE)
		call gmSetWidgetStatus(jcurves, GSELECTABLE)
    
	    call gmSetWidgetStatus(new_lines, GSELECTABLE)
		call gmSetWidgetStatus(new_text, GSELECTABLE)
		
endif
case(4901:4903)! type 5 iplot=5
     id1=callid-4900

case(4900)

!call gmremovewindow(iradio)
	
	if(id1.gt.0) then
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
	radio_text(20)=' '
	radio_text(1)=' (1) '//text1(1:12)//' and following '//text2
	radio_text(2)=' (2) '//text1(1:12)//' and preceding '//text2(1:12)
	radio_text(3)=' (3) '//text1(1:12)//' and both following and preceding '//text2
	radio_text(4)=' (4) '//text1(1:12)//' and following '//text2(1:12)//' MINUS'//&
    text1(1:12)//' and preceding '//text2
!call toggle_panel(Main,ITOGGLE,itogglepanel,4,text_tog,intoggle,4905,valdat,idat,itogbutton)
 call radio_panel(main,iradio,4,iradio_toggle,radio_text,0,&
 iradiox,radiox,iradioy,radioy,4905)
 endif
 case(4906:4909)
 id2=callid-4905
case(4905)

	 call gmremovewindow(iradio)
	
	if(id2.gt.0) then
	
	
		p0min=1.
		lag=1
	    xaxis(1)=tresg
		yaxis(1)=treso
	    nbdec=4		!default
		text_tog(1)=' X axis to start at X='
		text_tog(2)=' Y axis to start at Y= '
		text_tog(3)=' Number of bins/decade '
		
		text_tog(4)='lag'
		text_tog(5)=' Minimum value for expected number in bin  '
		do i=1,10
		itypeval(i)=0
		enddo
		itypeval(3)=1
		itypeval(4)=1
		val(1)=xaxis(1)
		val(2)=yaxis(1)
		val(3)=nbdec
		val(4)=lag
		val(5)=p0min
		  call value_table(Main,ivwin,5,text_tog,ival,val,4910,itypeval)
	endif
case(4910)
       
	   call CDIST3D(tint,ampl,iprops,nint,nintt,ndimy,iscan,treso,tresg,tres,idsav,&
	   idiskq,id1,id2,lag,nbdec,xaxis,yaxis,p0min,&
	   igraph,main,ix,iy,graph1_1,GraphMainPanel1_1,graphics1_1,ipos,&
     	oldrecords,nplot,itty)
		modplot=igraph
		 call gmSetWidgetStatus(icross, GunSELECTABLE)
 call gmSetWidgetStatus(i3d, GSELECTABLE)
 call gmSetWidgetStatus(irot, GSELECTABLE)
 call gmSetWidgetStatus(iview3d, GSELECTABLE)
 call gmSetWidgetStatus(iaratxy, GSELECTABLE)
 call gmSetWidgetStatus(iarathb, GSELECTABLE)
 call gmSetWidgetStatus(igridd, GSELECTABLE)
  call gmSetWidgetStatus(isurfdr, GSELECTABLE)
   
case (4911)
	  !fitted dep plot
	   xmin=tres1
	   xmax=10000.*xmin
	   ymin=tres1
	   ymax=10000.*ymin
	   nvdec=10
	   text_tog(1)='Minimum shut time (ms)'
	   text_tog(2)='Maximum shut time (ms)'
	   text_tog(3)='Minimum open time (ms)'
	   text_tog(4)='Maximum open time (ms)'
	   text_tog(5)='Values per decade'
	   val(1)=xmin
	   val(2)=xmax
       val(3)=ymin
	   val(4)=ymax
	   val(5)=nvdec
	   do i=1,5
		itypeval(i)=0
	   enddo
		itypeval(5)=1
	   call value_table(Main,ivwin,5,text_tog,ival,val,4912,itypeval)
	  
case(4912) ! iplot=5
! enq val table
		do i=1,5
		 val(i)=gmenqvaluesetting(ival(i))
		enddo
		xmin=val(1)
		xmax=val(2)
		ymin=val(3)
		ymax=val(4)
		nvdec=val(5)
		call gmremovewindow(ivwin)
	   xmin=alog10(xmin)
	   xmax=alog10(xmax)
	   ymin=alog10(ymin)
	   ymax=alog10(ymax)
	 
	   ns=1 + nvdec*ifixr(xmax-xmin)
	   no=1 + nvdec*ifixr(ymax-ymin)
	   nx=ns
	   ny=no
	   ALLOCATE(fdep(nx,ny),znew(nx,ny),shutt(nx),opent(ny))
	   if(allocated(badval)) DEALLOCATE(badval)
	   ALLOCATE(badval(nx,ny))
	   do i=1,nx
		do j=1,ny
		   badval(i,j)=.false.
		enddo
	   enddo

	   delt=(xmax-xmin)/float(ns-1)
	   do i=1,ns
		shutt(i)=xmin + (float(i-1))*delt		!log values
	   enddo
	   delt=(ymax-ymin)/float(no-1)
	   do i=1,no
		opent(i)=ymin + (float(i-1))*delt		!log values
	   enddo

	   call DEPEND(tres,k,kA,kF,ns,no,shutt,opent,fDEP,zmin,zmax,&
     	 Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,XAF,XFA,QexpQA,QexpQF,kAm,kFm,km)

	   z=ifixr(zmin)
	   if(z.gt.zmin) then
		zmin=z - 1.0
	   else
		zmin=z
	   endif
	   z=ifixr(zmax)
	   if(z.lt.zmax) then
		zmax=z + 1.0
	   else
		zmax=z
	   endif

	   idev=0		!screen
	  
	   ians=gmdisplaymessagebox('','Use 3D smoothing on plot',gquestion,gyesno)
	   if(ians.eq.gyesbutton) then
		    power=1.0
     		call SMOOTH3D(fdep,znew,nx,ny,power,nx,ny)
	   else
		do i=1,nx
		   do j=1,ny
			znew(i,j)=fdep(i,j)
		   enddo
		enddo
	   endif

	   idrawt=1
	   idrawx=1
	   idrawy=1
	   idrawz=1
	   icol(23)=0		      !in common for gplot3d
	   icol(25)=9
	   icol(71)=7
	   xtitle='shut time'
	   ytitle='yopen time'
	   ztitle='dependency'
	   title2='Fitted dependency plot'
	   ndx=nx
	   ndy=ny


	   kcol=2
	   posneg=.true.
	   isetcol=2
	   ! gauto3d

! import files(graphs) for display  from plotqs
!=================================================================================

	case(5001) !13	! Import
			newfile=.false.
			import=.true.
		    !if(.not.allocated(newrecords)) allocate(newrecords(25),oldrecords(25),curves(25))
			if(.not.allocated(curves)) allocate(curves(25))
			
			do i=1,100
					icfit(i)=0
					iopen(i)=-1
					jopen(i)=-1
				enddo
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
			ftitle1, button7,button6,saveandplot,program_type)
			if(ifiltype.eq.3) nplot=1
			ALLOCATE(xdata(nplot,njset,ncols,niobs)) 
			import=.true.
			endif

!=====================================================================
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
			
			
				istat2=gmenqtoggleswitch(ittfont_Toggle(27))
				if(istat2.eq.gon) then
					idev=3
					idevp=3
				endif
			
			istat2c=gmenqtoggleswitch(ittfont_Toggle(16))
				if(istat2c.eq.gon) then
					show_curve=.true.
				else
					show_curve=.false.
				endif
			
			!endif
			ilog0=ilog
			do i=1,5
				istat2=gmenqtoggleswitch(ittfont_Toggle(i+30))
				if(istat2.eq.gon) ilog=i-1
			enddo
			istat2=gmenqtoggleswitch(ittfont_Toggle(36))
				if(istat2.eq.gon) ilog=ilog0
			call gmremovewindow(ittfont)
	    callid=ipage+5010
			!new
			idev=0
			if(callid.eq.5020) callid=5011
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
								igraph=k
								goto 333
							endif
							endif
						enddo
						endif
						if(.not.plot) igraph=igraph+1
						
					
						if(.not.plot.and.igraph.gt.20) then
									imessy=gmDisplayMessageBox('','Maximum 20 plots on screen',Gexclamation,gok)
							
								goto 1
						endif

333						continue
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
						
							call read_record(ISTREC,Main,Data_list,iplot,ifiltype,&
							iplotype,njset,nset,nsfit,&
							xnum,nrows,niobs,njobs,stitle,hdisp,xobs,yobs,w,nj,&
							titlex,titley,ilabel,title1,iptype,ndv1,ndimd,&
							ndc1,ndimc,kwi,kwj,kmax,ncurvd,ncurvc,ifile,nval1,&
							useconsam,filtered,istr1,colseq,nhline,adcfil,&
							itit,ndev,ib,istart,iend,srate,calfac,ioff,calfac2,&
							cdate,adctime,adcf1,isdfst,ndisp1,nrec1,jdat,base,&
							ntrans,y0,t0,nfilter,dtf,tif1,tif2,dt,stepamp,filt,&
							ndelt,ffilt,fcz,fczoom,njump,idest,krn,titled,setx)
							
						
							if(ifiltype.eq.1) then  !cvdat
							iplotype=-1
							if(plot) idev=idevp
									if(.not.plot) oldrecords(modplot)%iplotype=-1
								gfile=ifile
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
								do j=31,31+numby
									ijus(j)=1
								!	rx(j)=rx(j)-2.5*dxs
								!	ry(j)=ry(j)+dys
							    enddo
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
								endif
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
								logx,logy,sqrty,doframe,icol,ntx,nty,idev,thick,itype)
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
									Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw)
							endif
							
							call draw_curves(xcal,ycal,icurvc,ncurvc,logity,logx,&
								logy,sqrty,y0,yinf,x0,ilog,idev,&
								wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
								xmin,xmax,ymin,ymax,ncal,&
								iline,icol,thick,ndc1,ndimc,jfirst,dxs,dys)
							if(show_curve) then
					ymin20=ymin
							ymax20=ymax
							if(sqrty) ymin20=sqrt(ymin)
							if(sqrty) ymax20=sqrt(ymax)
							xt=0.5*(xmin+xmax)
							yt=ymax20+0.15*(ymax20-ymin20)
								ytc=0.05*(ymax20-ymin20)
								if(ncurvc.gt.0) call write_string('Curves:',xmax+0.11*(xmax-xmin),ymax20+0.01*(ymax20-ymin20),0.,0,101,&
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
768								ytc=ytc+0.05*(ymax20-ymin20)
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
		
				CALL gmFileBROWSER(eFILE,eDIR,eFILT,gmBrowseType=1)
				IF(eFILE.ne.' ') then
				if(iplotype.le.4) then
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
						ALLOCATE(ndat(ndimd),isym(ndimd),ijoin(ndimd),symsiz(ndimd))
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
			
				call write_plotq(iplotype,xval,yval,xcal,ycal,ndimd,ndimc,&
				ncurvd,ndat,icurvd,isym,ijoin,ncurvc,ncal,icurvc,iline,syms,&
				xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ntx,nty,itx,ity,&
				xlo,xhi,ylo,yhi,itit,title1,ilog,iscal,doframe,&
				titlex,titley,ilabel,inumx,inumy,efile,sval,&
				ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,y1v,y2v,y1c,y2c,ivplot,&
				theta,ifitype,ncomp,idest,interp,&
				RLTH,y0,yinf,ntrace,ytsep,ndv1,ndc1,w,kwi,kwj,icurvw,kmax,n1,jmiss)
				endif
			
			else
			endif
			else
				imessy=gmDisplayMessageBox('','No record on screen/Not available for this record',Gexclamation,gok)
			endif
			
		
	
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
			modplot=callid-5150
	         idev=0
			 	modplot=callid-5150
			ifontrue=101
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
			
			!	CALL GUIPRT(0,ISTATus)
				istatus=gmprintercontrol(gprintersetup)
				if(istatus.eq.0) goto 1 
				istatus=gmprintercontrol(GOPENPRINTER)
				IF(ISTATus.NE.0)THEN
					plot=.true.
					idev=6
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
					imessy=gmDisplayMessageBox('','Black & White ?',Gquestion,gyesno)
					if(imessy.eq.gyesbutton) idev=4
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

case (5201:5225) ! bmp
landscape=.false.
		modplot=callid-5200
		ifontrue=0
		callid=5226
		goto 2
	
case(5226)
	
		if(readrec) then
		if(modplot.gt.0.and.lframe.eq.graphics1_1(modplot)) then
			CALL gmFileBROWSER(sFILE,sDIR,sFILT,gmBrowseType=1)
	
			IF(sFILE.ne.' ') then
					m1=1
					if(oldrecords(modplot)%IPOS.eq.0) then
						m1=modplot
						msplot=1
					else
					
						do i=m1,m1+3
							if(jopen(i).eq.1) msplot=msplot+1
						enddo 
					endif
				
					icallprev=5200
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
		!	istat2=gmenqtoggleswitch(ittfont_Toggle(26))
		!	if(istat2.eq.2) then
		!	idev=6
		!	else
		!	idev=3
		!	endif
			istat2c=gmenqtoggleswitch(ittfont_Toggle(16))
				if(istat2c.eq.gon) then
					show_curve=.true.
				else
					show_curve=.false.
				endif
	call gmremovewindow(ittfont)
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
		call devsus 

	endif
			if(readrec) then
				mplot=0
				ifirstprint=0
				do i=m1,ngraph
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
							!	call devend
							else 
							!	istatp=gmPrinterControl(gnewpage)
							endif
						endif
						if((pbmp.or.pwmf).and.ipos.le.1) then
								  if(icallprev.ne.5200) then
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
						icol,ntx,nty,idev,thick,itype)
						call draw_text(parval,xtitle,ytitle,ztitle,title1,cnumx,cnumy,&
						cnumz,cexpx,cexpy,cexpz,newtext,numbx,numby,numbz,ntext,inumx,&
						inumy,intx,angle,sizetext,ijus,thick,rx,ry,idraw,icol,ifnt,str,dxs,dys)
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
						iline,icol,thick,ndc1,ndimc,jfirst,dxs,dys)
						if(show_curve) then
					ymin20=ymin
							ymax20=ymax
							if(sqrty) ymin20=sqrt(ymin)
							if(sqrty) ymax20=sqrt(ymax)
							xt=0.5*(xmin+xmax)
							yt=ymax20+0.15*(ymax20-ymin20)
								ytc=0.05*(ymax20-ymin20)
								if(ncurvc.gt.0) call write_string('Curves:',xmax+0.11*(xmax-xmin),ymax20+0.01*(ymax20-ymin20),0.,0,101,&
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
								    goto 769
								else if(ij.ge.10) then
									linetype=ij-10			!join points with straight line type #ij
								endif
								call broken(linetype)
								call movto2(xmax+0.1*(xmax-xmin),ymax20-ytc)
								call linto2(xmax+0.2*(xmax-xmin),ymax20-ytc)
769								ytc=ytc+0.05*(ymax20-ymin20)
								endif
							enddo
					endif
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
						
						if(icallprev.eq.5200.or.icallprev.eq.5150)then
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
			if(sqrty) ytrue=(yac)**2
			endif
			call valtyp(1,0)
			
			CALL REALTOCH(XTRUE,CXTRUE,11)
			CALL REALTOCH(YTRUE,CYTRUE,11)
			call gmSetStatusBarText(Status_bar1,2,CXTRUE)
			call gmSetStatusBarText(Status_bar1,3,CYTRUE)
 
			call valtyp(0,0)
			if(s_text.and.lframe.eq.graphics1_1(modplot)) then
				call show_boxes(xmov,ymov,mm,str,rx,ry,angle,ijus,sizetext,icol,itemp,&
				idraw,inewpos,dxs,dys)
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
!if(allocated(newrecords)) deallocate(newrecords,oldrecords)
stop

end


