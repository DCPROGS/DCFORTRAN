Program hjcfit
! check extra calls for special features
! to do : increase size inifile
!        :check arrays

! qfilem -mec file no path
! pathmec -path mechanism file
! qmec -mech file with path
! pfileb -data file no path
! pathdata 
! pfiles -data file +path
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
!	6001- 7000 Reading simval.dat
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
	USE IFCORE
!	USE IFQWIN
	USE IFPORT
	use gino_f90
	use menu_f90
    use hjcrecords
	
	include '\intel\common_files\graphics_definitions.f90'
	include '\intel\hjcfit\hjcfit_definitions.f90'

	TYPE (FILE$INFO) info
	type (GACTION) :: actlst
	type (Gdim) :: dim
	type (Gwidget) :: widget
	TYPE (RECORD_ATTRIBUTES) newrecords(:),oldrecords(:)
	type(rate_constant) ratcons(:) 
	TYPE (MODEL) MODELS(:)
	ALLOCATABLE  newrecords,oldrecords,ratcons,models
logical zero
	common/dex/zero	
	common/phiblk/phiF,phiA
	common/sets/setlik,nset2
	common/fitblk/eigen,g00A,g10A,g11A,g00F,g10F,g11F
	common/text_attr/bold,italik,underline,ipow,idev,nolabx,ifontrue
	common/modele0/imod0,charmod,jlast,ilast,mtitle,imodold
	common/modele/imods,charmods,jlasts,ilasts,mtitles,imodolds
	common/limit/wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
	xmin,xmax,ymin,ymax
	COMMON/TPOS/IDRAW,ICOL,THICK,C_THICK,ITYPE,IFNT,ANGLE,IJUS,&
     SIZEtext,RXBOX,RYBOX,&
     RX,RY,NARROW,NLINE,NHLINE,NVLINE, XBEG,YBEG,XEND,YEND,&
     NTEXT,NEWTEXT,CNUMX,CNUMY,CNUMZ,CEXPX,CEXPY,CEXPZ,&
     NUMBX,NUMBY,NUMBZ,IHLINREL,IVLINREL
	common/logval/logx,logy,sqrty,logity
    COMMON/JLOGOS/t1c,t2c,t1v,t2v,xoff1,y1v,y2v,y1c,y2c,ncjump,nvjump,ivplot
!	COMMON/mtrace/ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil
	common/iniset/nodata,nofit,autosim,curvonly
	COMMON/BLOCK2/castar,X1,X2,iequiv,ip1,ip2
	COMMON/BLOCK3/logyfit,norm,xnorm
	common/fix/fixratio,ir1,ir2,rval	!to fix ratio of 2 parameters
	!common/potrat/jfirst,iset,kmax1	
	common/dp/discprt,append
	common/abt/ABORTW
	!common/pwrfunc/ybarp
	common/pixpos/ixgrid,iygrid,ixpix,iypix,wid,landscape
		
	!common/exop/iexcop,gaplo,gaphi,nskip
	COMMON/HJCBLK/Nint,tcrit,burst,chsvec,badend
	common/fitblk1/ampA,ampF
	
	common/ece/ec50d,pop0,pmax,curmax,concmax,monotd
	!common/cube/ncube,cubedef,cubecyc,cubext	!for getrev
	COMMON/MPAR/NCYC,NSC,IM,JM
	COMMON/MPAR1/NCYC1,NSC1,IM1,JM1
	COMMON/MPAR2/NCYC2,NSC2,IM2,JM2
	COMMON/QPAR/NCON,IC
	
	common/mr/obeymr,automr
    common/absave/nab,jalpha,jbeta
	common/mr1/isetmr,ncyc0
	common/ir/irate(200),jrate(200)
	common /a/ mabel(100)		!common needed for CIRCUIT.for
    common /b/ matrix(100,100),n  !common needed for CIRCUIT.for
    COMMON/EBLK/NEQ,IE,JE,IF,JF,EFAC
	COMMON/indblk/nsetq,ieq,jeq,ifq,jfq,efacq(200) 
	common/ec/fixec50,nmod9,ec501,i50,j50,m50,prtec50,xqlo,xqhi,dcmodel	!nmod=imod0 (already in commom/model)
    common/pert/ perfac
	common/ec2/monot,nlvar,vhold,vref,cfacd,dgamma !for ec50_hjc, qset_hjc
	common/ec3/ligname		!for qset_hjc
	common/ec1/nfixec51,fixec51,qval,ec5011,ec50out,i501,j501,x1s,x2s,&
     fx1,fx2,qval2,ec5012,ec50out2,i5012,j5012,x1s2,x2s2,fx12,fx22  !for print in SIMPHJC
	common/ec4/nfixec50,nlvar2,ec502,i502,j502,m502,xqlo2,xqhi2,conc_ec1,conc_ec2	 !for 2nd fixed ec50
	common/tty/ittypanel,itty
	common/deb/idebug,idebug1
	common/pen/penalty,penfunc,penfac,fixpmax,pomax,pmaxcalc 
	!common/ires/ireset 
    common/nblk/ngp(10),an(10),nscal(10),first    !ngp(j) for set j (for hjclik only)

	common/setblk/oneset,iset9,itogrec	!for HJCLIK,DISP to specify one set
	common/switch/iswicon,igraphText(100),ibutton1,icongmod
	COMMON/BLOCK1/constr,nset,nfit,nsfit,Xv,kmax,ncomp,&
	nmod,fline,nomit,jomit,jset,ifitmode
	!for HJCLIK, fitting.f90
	common/CBLK/nset0,conc,jsetlast
	common/KBLK/kA,kB,kC,kD
	common/det4/ndscale,idsign
	!common/QDBLK1/QT,QD
	common/QDBLK2/npar1,IQf,irate1,jrate1,nlig   !hjclik, qset_hjc only
	common/root/rootA,rootF	!hjclik,popadj,fcrqt,fmrqt,popadjc,depend,mopadjc
	common/inroot/s1A,s2A,s1F,s2F	  !for init guesses for roots; hjclik only
	common/eigsav/eigAAsav,eigFFsav
	common/rootsav/rootAsav,rootFsav		!hjclik only
	common/inroots/s1Asav,s2Asav,s1Fsav,s2Fsav !for init guesses for roots (hjclik only)
	common/detw2/tres,km,nerr	!for DETWA,DETWF, hjclik, hjcasymp, hjcexact
	common/detw3/WA,WF
	
	common/resblk/tresd	!for hjclik only
	common/rblck/treso5,tresg5,acrit5,avamp5
	common/amax/assmax,icdep,ratemax    
	common/cpar/ncdep,IX,JX,x
	COMMON/determ/det			!for matinv
    common/perr/nerr2
	common/rand/ixr1,iyr1,izr1
	common/grp1/slopsch,checkgrp,grouped,rcrit,ngpcheck
	common/LIG/nligsav,IL(100)
	common/attributes_flag/d_line,d_hline,d_vline,d_arrow,d_poly,d_text,d_zoom,o_state,&
		   c_state,s_text,s_line,s_arrow,zoom,link,delete_state,move_state                  
	common/lim/good,goodx	
	common/def/usedef,exass,dsmooth,shbiv,shdep,shdif,shfit
	common/stab/stable,nstab1,gfac1,nstab2,gfac2,istab	!for hjcfit
	common/cvsd/sdlog,sdlog1
	common/mech_param/k,kf,nsub,kstat,ncon1,statname,icspec,icout,&
		jcon,ncin,nd1,nd2,nstate,ndim,iedge,jtree,jmic,irate2,jrate2,&
		neq0,neq1,nfix,kfit,fac,xs,&
		rtitles,iq,theta0,jfix,nfileb
	!common/detwmod/EXPQF,EXPQA		
	
	common/KBLKs/kAs,kBs,kCs,kDs,ks
	common/cpars/ncdeps,IXs(100),JXs(100),xss
	common/LIGs/nligs,ILs(100)
	common/QPARs/ncons1,ICs(2,200)
	logical*4 fastblk,newmec
	real*4 aKB
	integer*4 ifb(20),jfb(20),mfb(20),iflig
	common/fblck/fastblk,aKB,nfblock,iflig,ifb,jfb ,mfb
    
!definitions in case ini file not read
	rescalex=.false.	!set true when x axis rescaled for shut times
	ijplot=200  
	ixg=80
	iyg=80
	narrow=0
	fastblk=.false.
	do i=1,20
	    ylo(i)=0.0
	    yhi(i)=0.0
	enddo
	do i=1,2
		do j=1,10
			setbad(i,j)=.false.
		enddo
	enddo
	resdebug=.false.
    ncdep=0
	show_curve=.true.                      
!	filnam='hjcfit.rtf'
	filnam='hjcfit.txt'
		simfile1='SIMVAL.DAT'
		simfile='SIMVAL.TXT'
		simfile3='milsim.TXT'
	inipage=0
	initialpage=0
	ini_MainPanel=0
	ini_panel1=0
	ini_panel1_1=0
	ini_Text1_1=0
	km=100	!in COMMON -dimension of QD, QT etc
	kAM=10	!max number of open states
	kFm=90	!max number of shut states
	nlvar=-1	!until defined
	nsims=1	!number of simulated expts fitted =1,2,...,nsim
	ireset=1	!for hjclik
	perfac=0.05d0	!maximum random perturbation
	ngpcheck=100
	excopen=.false.
	km=100	!in COMMON -dimension of QD, QT etc
	kAM=10	!max number of open states
	kFm=90	!max number of shut states
	nlvar=-1	!until defined
	nsims=1	!number of simulated expts fitted =1,2,...,nsim
	ishp=0
    penfac=0.0
    pomax=0.
	nab=0		!for save of alpha and beta values
    open7 =.false.
	sbin=.false.	!for HJCDAT
	shist=.false.	!for HJCDAT
	sres=.false.	!for HJCDAT
	sexp=.false.	!for HJCDAT
	idiskq=-1		!until defined
	nset=1		!default number of data sets
	do j=1,10
	   iexcop(j)=0
	enddo
	XLO1=0.
	XHI1=0.
	YLO1=0.
	YHI1=0.
	y0=0.
	yinf=0.
	ntrace=0
    itrace=0
	isave_wmf=0
	isave_bmp=0
	imes=0
	iver1=0
	iscan1=0
	iscan=0
	istatw1=0
	istatw2=0
	istatw3=0
	nselect=0
	list7_1=0
	ncol=0
	ifiltype=0
	imd=0
	icallid=0
	imodhelp=0
	npars=0
	aamaxg=0.
	zg=0.
	aamax0=0.
	z0=0.0
	istat1=0
	ipeny_fac=0
	ipeny_xs=0
	ipeny_xs1=0
	ipeny_yes=0
	ipeny_no=0
	ipeny=0
	xmin0=0.
	xmax0=0.
	ymin0=0.
	ymax0=0.
    nplot=0
    kwi=0
	xtic=0.0
	ytic=0.0
	istatus1=0
	istatus2=0
	istatus3=0
	istatus4=0
	istatus5=0
	istatus6=0
	istatus8=0
	istatus9=0
	istatus10=0
	iver=0
	dxs=0.0
	dys=0.0
	iplot=0
	iarca=0
	iradio=0
	icallprev=0
	nfirst=0
	nentry=0
	nmods=0
	indx=0
	imodk=0
	imodmax=0
	ncyc2=-1
	np0min=100
	np0max=10000	!ditto for fine search
	npmin=50
	npmax=500000	!upper limit of number of search points for coarse search
	idebug=1		!no debugging
	idebug1=1		!no debugging
	!!igraph2=1
	idebug2=0		!no print in chekmr (except at end)
	idebug3=0
	idsav=1		!ditto
	slopsch=.false.
	checkgrp=.false.
	grouped=.false.
	issim=1
	rcrit=1.05d0	!identical within 5%
	nstab1=200		!200 evals since last rootsch call
	gfac1=0.1d0	!guesses = rootF +/- 10% after nstab1 evaluations
	nstab2=500		!500 evals since last rootsch call
	gfac2=0.02d0	!guesses = rootF +/- 2% after nstab2 evaluations
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
	stable=.false.
	debprt=.false.
	liksurf=.false.
	autosim=.false.
	nofit=.false.
	nodata=.false.
	prtec50=.false.
	penalty=.false.
	logfit=.false. 	!except when hjclik called from simplex
	
	nfix=0
	neq=0
	ncyc=0
	nmr=0			!number of rates actually set by MR
	do i=1,200		!ktd=200
	  jfix(i)=0		!to mark fixed params
	  jfix1(i)=0	!all zero, for Simplex call
	  jcon(i)=0		!to mark constrained params
	  jmic(i)=0		!to mark micro rev params
	enddo
	ksim=0
	filasc='ascfil.txt'
	iresult = FULLPATHQQ ('hjcfit.exe', adir)
	nb=len_trim(adir)
	iniinifile=adir(1:nb-10)//'hjcini.ini'
	inifile=adir(1:nb-10)//'hjcfit.ini'
			pfilem=filnam
			nodata=.false.
			curvonly=.false.
			nofit=.false.
			autosim=.false.
			discprt=.true.
			append=.false.
			itogrec=1
			xwbase=0
    INQUIRE (FILE=iniiniFILE,EXIST=PRESENT)
	if(PRESENT) then
			ihandle=FILE$FIRST
			length = GETFILEINFOQQ(iniinifile, info, ihandle)
			nLEN=info%length
			readini=.true.
			IRECL=10240
			OPEN(UNIT=13,FILE=iniiniFILE,&
            ACCESS='DIRECT',FORM='UNFORMATTED',RECL=irecl)
		    INQUIRE(IOLENGTH=LEN) iniiniFILE	
    	    if(nlen.gt.0) then
				read(13,rec=1) itogrec1
				if(itogrec1.le.10) then
					read(13,rec=1) itogrec,inifile,pfilem,nodata,curvonly,nofit,autosim,discprt,append
			    else
					
					read(13,rec=1) inifile,pfilem,nodata,curvonly,nofit,autosim,discprt,append
					write(13,rec=1) itogrec,inifile,pfilem,nodata,curvonly,nofit,autosim,discprt,append
				endif
			else
			
				write(13,rec=1) itogrec,inifile,pfilem,nodata,curvonly,nofit,autosim,discprt,append
			endif			
    else	
			IRECL=10240
			OPEN(UNIT=13,FILE=iniiniFILE,&
            ACCESS='DIRECT',FORM='UNFORMATTED',RECL=irecl)
			readini=.true.
			
			write(13,rec=1) itogrec,inifile,pfilem,nodata,curvonly,nofit,autosim,discprt,append
    endif
	close(unit=13)


!	pfilem=adir(1:nb-10)//filnam
	pfilt='*.txt'//char(124)//'Print file (txt)'//char(124)//&
	'*.rtf'//char(124)//'Print file (rtf)'//char(124)//&
						  '*.prt'//char(124)//'Old print files'



pdir=adir(1:nb-10)
show_curve=.true.
INQUIRE (FILE=pfilem,EXIST=PRESENT)
	if(.not.PRESENT) then
		pfilem=adir(1:nb-10)//filnam
	else
		nb=len_trim(pfilem)
		if(pfilem(nb-3:nb).ne.'.txt') pfilem(nb-3:nb)='.txt'
	endif
INQUIRE (FILE=inifile,EXIST=PRESENT)
	if(.not.PRESENT) then
		inifile=adir(1:nb-10)//'hjcfit.ini'	
	!	inifile=' '
	endif
	OPEN(UNIT=13,FILE=iniiniFILE,&
            ACCESS='DIRECT',FORM='UNFORMATTED',RECL=irecl)
			readini=.true.
			
	write(13,rec=1) itogrec,inifile,pfilem,nodata,curvonly,nofit,autosim,discprt,append
     close(unit=13)

	do i=1,100
		jopen2(i)=-1
	enddo

	nodata=.false.
	curvonly=.false.
	nofit=.false.
	autosim=.false.
	inifile0=inifile
	program_type=3
	dfilt=dfilth
	igraph=0
	ngraph=0
	narrows=0
	idev=0
	
	jmodel=300
	indr=0
	readrec=.true.
	do i=1,km
	   ucol(i,1)=1.d0
	enddo
	sdim=3.0
	do i=1,100
		nplot_on(i)=-1
	enddo
	ndth=100
	ncalc=512
	autplt=.false.
	plotcols=.true.
	idev=0
	angit=5.0
		ifont0=101
	idatyp=0
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
	idest=0
	!if(njset.gt.20) ndimc=njset
	IW=1
	!if(idest.ne.2) idest=0
	ixm=2
	iym=4
	ix=96
	iy=48
	ifirstsim=-1

	nab=0		!for save of alpha and beta values
	sbin=.false.	!for HJCDAT
	shist=.false.	!for HJCDAT
	sres=.false.	!for HJCDAT
	sexp=.false.	!for HJCDAT
	idiskq=-1		!until defined
	nset=1		!default number of data sets
	

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
 ! ixng=ixgrid
 ! iyng=iygrid
   call gmdefineguigrid(ixng,iyng)
 call gmenqguigrid(ixgrid,iygrid,ixpix,iypix)
 !  call gsetsoftchars()
	call gsethardchars()
	call gsetbrokenlinemode(gon)
	iwidth_main=40
	iheight_main=32
	iresnth=1
!	iwidth_main=ixgrid-1
!	iheight_main=iygrid-1
	call gSetEscapeChar('£')
	call define_colours(1,.true.)
	call gSetCharFont(ifont0)
	call gSetLineEnd(GROUND)
	call gseterrormode(gerroron)
	call main_window(program_type,Main,imainpanel,cDATEW,modelw,eqfit,Status_bar1,&
	new_file,open_file,import_file,izoomy, &
           isave_bmp,isave_wmf,export_file,print_file,exit_file,view_record,&
		   view_data,title_record,iparameters,labels,&
		   jtitle_record,jparameters,jlabels,jnewtext,jnumbers,jaxis,jframe,&
		   jlines,jarrows,jraw_data,jcurves,label_x,label_y, &
		   label_z,number_x,number_y,number_z,Icon1_1,Icon1_2,Icon1_3,Icon1_4,&
		   Icon1_5,Icon1_6,Icon1_7,Icon1_8,Icon1_9,Icon1_10,Icon1_11,Icon1_12,&
		   Combo1_1,Combo1_2,Combo1_3,Combo1_4,Combo1_5,Combo1_6, combo1_8,combo1_9,combo1_10,&
		   toolbar1_1,toolbar1_2,toolbar1_3,Toolbar1_4,&
		   new_text,new_lines,new_arrows,new_lines_v,new_lines_h,&
		   i3d,irot,iview3d,iaratxy,iarathb,igridd,isurfdr,icross,ifill3d,imarkbad,&
		   combo1_11,combo1_12,ipl1,ipl2,ipl3,ipl4,ipl5,ipl6,icprev) 

    ALLOCATE(xobs(Niobs,NJSET),yobs(Niobs,NJSET),w(NIOBS,NJSET))
	ALLOCATE(xvalold(NIOBS,NJSET),yvalold(NIOBS,NJSET))
    ALLOCATE(nj(NJSET),jmiss(njset),juse(njset),ndat(njset),ncal(ndimc))	!for normalised data
    ALLOCATE(setx(njset),njbase(njset),icurvw(njset),icurvc1(njset))
	allocate(xnum(nrows,3),xdata(10,10,4,10),ndatset(200,20))
	allocate(newrecords(25),oldrecords(25))
	allocate (theta1(100),pdata(100),models(25),ratcons(500))  
    allocate(QD(100,100),QT(100,100),Peq(100))
    do imodel=1,25
	do istate=1,200
	models(imodel)%statname(ISTATE)='               '
     models(imodel)%name_link(istate)='              '
     models(imodel)%dgamma(istate)=0.d0   
            
    enddo
    enddo
    do i=1,100
    statname(i)='                '  
    peq(i)=0.d0
    do j=1,100
        qd(i,j)=0.d0
        
        qt(i,j)=0.d0
    
    enddo
    enddo
    
 do i=1,njset
		jmiss(i)=0
	enddo
    do i=1,500
	do k=1,200
				ratcons(i)%ligant(k)=' '
				ratcons(i)%value(k)=0.
				ratcons(i)%iconc(k)=0
				ratcons(i)%titlep(k)=' '
				ratcons(i)%qij(k)=' '
	enddo
	enddo
	do i=1,25
	do k=1,200
	    models(i)%inter_link(k)=0
	    do l=1,10
	    models(i)%nbound(k,l)=0
	    enddo
	enddo
	enddo
	penfac=10.0d0
	do i=1,ndimc
		ncal(i)=0
	enddo
	do j=1,10
		burst(j)=.false.
		chsvec(j)=.false.
		badend(j)=.true.
	  
	enddo
	assmax=1.e10
	ratemax=1.e6
	conc(1,1)=0.05e-6
	tcrit(1)=3.5
	tresol(1)=25.0
	idebug=1		!no debugging
	idsav=1	
!	autosim=.false.
!	nofit=.false.
!	nodata=.false.
    issims=0
	liksurf=.false.
	prtec50=.false.
	penalty=.false.
	logfit=.false. 	!except when hjclik called from simplex
	fixpmax=.false.
	ns=1
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
		
	nfast=0	
		
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
qfile='.plq'
ilg=ntog+2
iwidp=8
iwtog=6
itcall=10000
infolength=12
isetu=2
show_curve=.true.
itextinfo=gmCreateTextEntry(imainpanel, 1, 0, 20, 1,'Note:the program generates automatically a jpg/wmf &
image of the model(s) used', 255, GDISPLAY, gmBack1Col=0, &
      gmBack2Col=0, gmTextCol=2 , gmVpos=GTOP,gmsize=80)
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

!infopanel=gmCreatePanel(imainpanel, iwidth_main-14, 4, 14, iheight_main-7, &
!              	gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmborderType=GINVERSECHISEL, &
!              	gmLineCol=0, gmLineBorder=0, gmFillCol=150, gmFillBorder=0, gmScrollMode=Gvertical)

!ittypanel=gmCreatePanel(imainpanel, 1, 4, iwidth_main-15, iheight_main-7, &
!              	gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmborderType=GINVERSECHISEL, &
!              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
infopanel=gmCreatePanel(imainpanel, 1, 0,iwidth_main-2, 6, &
              	gmHpos=Gleft, gmVpos=Gbottom, gmExpand=GON, gmborderType=Gnone, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=150, gmFillBorder=0, gmScrollMode=Gbothbars)

ittypanel=gmCreatePanel(imainpanel, 1, 3, iwidth_main-2, iheight_main-12, &
              	gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmborderType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
linestty=100
itty= gmCreateTTYEntry(ittypanel, 0, 0, iwidth_main-2, iheight_main-12, linestty,'',&
				gmoffcol=1,gmoncol=1,gmtextcol=14) 
!call gmsettextsetting(itty,'Display for simplex')

call welcome(main,initialw,initialw_MainPanel,inifile, initialw_Text5,initial_toggle,&
initialw_Toggle3,initialw_Toggle4,initialw_Toggle5,initialw_Text2,pfilem,&
textcomp,textid,drives)
! go to -11 when press continue

call gmSetToggleSwitch(initial_Toggle(itogrec),Gon)
if(itogrec.eq.8.or.itogrec.eq.2.or.itogrec.eq.3.or.itogrec.eq.4) &
call gmSettextsetting(initialw_Text5,'')
call windra(initialw)
iwel=-100
icalprevm=0
!call progress_table(main)
call gmDefineKeySelectCallback(13,-11)
current_child = title_record
1	continue
! Action loop
   do while (gmAction(callid) /= -1)
   call gmEnqWidgetInfo(main,Widget)
!! inactivate 'enter key' here
!! call gmDefineKeyselectCallback(13,0)
   ixmain=widget%xpos
   iymain=widget%ypos
   call gmEnqActionState(actlst)

!!	if(actlst%key.eq.13.and.iwel.eq.-100) then
!!	callid=-11
!!	iwel=0
!!	goto 2

!!!	endif

2	continue
	
		
    
	select case(callid)
	
	! INITIALIZE
	!=====================================================================================
		case(-2) ! mouse settings
			m_mouse=.true.
			r_mouse=.false.
		case(-3) ! mouse settings
			r_mouse=.true.
			m_mouse=.false.
			link=.false.
			call gmSetGuiCursor(lframe,Gdefault,GDEFAULT)
			ilink=0
		case(-5) ! davance settings in ini_file
		ntoglik=1
		itcall=-200
		text_tog(1)='Print resolution results every nth transition:'
		valdat(1)=1
		itype(1)=0
		call toggle_panel(Main,ITOGGLE,ittoglepanel,ntoglik,text_tog,intoggle,itcall,&
		valdat,idat,itogbutton,itype)

		!imesa=gmdisplaymessagebox('','under construction !',ginformation,gok)
! idebug1=-1
        case(-200)
            
		    do i=1,ntoglik
		        istatus=gmEnqToggleSwitch(inToggle(i))
				if(istatus.eq.1) itogadv=i
		    enddo
		    if(itogadv.eq.1) then 
		        resdebug=.true.
		        iresnth=gmenqvaluesetting(idat(1))
		    endif
		    call gmremovewindow(itoggle)
		    continue
		
		case(-8)
		liksurf=.false.
		 
		do i=1,ntoglik
		istatus=gmEnqToggleSwitch(inToggle(i))
				if(istatus.eq.1) itogadv=i
		enddo
		if(itogadv.eq.1) liksurf=.true.
		call gmremovewindow(itoggle)
		continue
		case(-9)
		call gmremovewindow(iplotlik)
		case(-10) ! use ini file called from menu_bar
			initi=0
			call gmDefineKeyselectCallback(13,-11)
			autosim=.false.
		!	call welcome(main,initialw,initialw_MainPanel,inifile, initialw_Text5,initial_toggle)
			call welcome(main,initialw,initialw_MainPanel,inifile, initialw_Text5,initial_toggle,&
			initialw_Toggle3,initialw_Toggle4,initialw_Toggle5,initialw_Text2,pfilem,&
            textcomp,textid,drives)
			call windra(initialw)
		    call gmSetToggleSwitch(initial_Toggle(itogrec1),Gon)
		 case(-57:-50) ! choices from welcom (ini,not ini,etc)
			itogrec=58+callid
			itogrec1=itogrec
			if(itogrec.eq.1) then ! use ini
		!		inifile=inifile0
			!	call gmsettextsetting(initialw_Text5,inifile)
					ni=len_trim(inifile)
			!	if(inifile(ni-9:ni).ne.'hjcfit.ini') inifile(ni-9:ni)='hjcfit.ini'	
			    call gmsettextsetting(initialw_Text5,inifile)
				nofit=.false.
			nodata=.false.
			curvonly=.false.
			else if (itogrec.eq.2) then !no ini
		!	    inifile='     ' 
				call gmsettextsetting(initialw_Text5,' ')
				readini=.false.
				nofit=.false.
			nodata=.false.
			curvonly=.false.
			else if (itogrec.eq.3.or.itogrec.eq.4) then ! Create new mec,view old mech
		!	    inifile='     ' 
				call gmsettextsetting(initialw_Text5,' ')
				nofit=.true.
			nodata=.true.
			curvonly=.true.
				readini=.false.
			
		
			else if (itogrec.eq.8) then ! view autosim
				itogrec=7
				call gmsettextsetting(initialw_Text5,' ')
				nofit=.true.
				nodata=.true.
		inifile=' ' 
				readini=.false.
			else if (itogrec.eq.5) then
		
			nofit=.false.
			nodata=.false.
			curvonly=.false.
				inifile='AChdemo.ini' 
			!	ni=len_trim(inifile)
			!	if(inifile(ni-10:ni).ne.'hjcdemo.ini') inifile(ni-10:ni)='hjcdemo.ini'
				call gmsettextsetting(initialw_Text5,inifile)
			else if (itogrec.eq.6) then ! run demo glycine
			
			nofit=.false.
			nodata=.false.
			curvonly=.false.
!			inifile='bgtv2a.ini' 
!			inifile='glyhet8.ini' 
			inifile='Glydemo.ini' 
				ni=len_trim(inifile)
!				if(inifile(ni-9:ni).ne.'bgtv2a.ini') inifile(ni-9:ni)='bgtv2a.ini'	
			    call gmsettextsetting(initialw_Text5,inifile)
			else if (itogrec.eq.7) then ! run demo autosim
				itogrec=8
				autosim=.true.
				idatyp=3
				inifile='HJCsimCK.ini' 
				 call gmsettextsetting(initialw_Text5,inifile)
			!	imko=gmdisplaymessagebox('','Not yet done',ginformation,gok)
			endif
		
		case(-11,-12,-30,-28) !(-40,-60)  ! -11 direct from welcome
		   
		   	call gmDefineKeyselectCallback(13,0)
		    do i=1,8
			
				istatus=gmEnqToggleSwitch(initial_Toggle(i))
				if(istatus.eq.1) itogrec=i
			enddo
			itogrec1=itogrec
			if(itogrec.eq.8) then
			 itogrec=7
		
			else if(itogrec.eq.7) then
			itogrec=8
			endif
			!itogrec=1 browse for ini
			!itogrec=2 not ini
			!itogrec=3 create model
			!itogrec=4 view model
			!itogrec=5 demo nicotinic
			!itogrec=6 demo glycine
			!itogrec=7 simulated data
			!itogrec=8 demo simulation
			call gmDefineKeySelectCallback(13,0)
			istatus13=gmEnqToggleSwitch(initialw_Toggle3)
			istatus14=gmEnqToggleSwitch(initialw_Toggle4)
			istatus15=gmEnqToggleSwitch(initialw_Toggle5)
			

			if(istatus15.eq.gon) then 
				discprt=.false.
				append=.false.
			else if(istatus13.eq.gon) then 
				discprt=.true.
				append=.true.
			else if(istatus14.eq.gon) then 
				discprt=.true.
				append=.false.
			endif
			call gmenqtextsetting(initialw_Text2,pfilem)
			nb=len_trim(pfilem)
			if(nb.le.2) pfilem='hjcfit.txt'
			nb=len_trim(pfilem)
			if(pfilem(nb-3:nb).ne.'.txt') pfilem(nb-3:nb)='.txt'

			if(discprt) then
				if(open7) close(unit=7)
				OPEN(unit=7,file=PFILEm,status='UNKNOWN',&
				access='APPEND',form='FORMATTED',carriagecontrol='FORTRAN',iostat=i_var)
				if(i_var.ne.0) then
			    close(unit=7)
				imes=gmdisplaymessagebox('','Invalid path/file name or file already open',gstop,gok)
				discprt=.false.
				open7=.false.
				goto 149
				endif
				!close(unit=7)
				if(.not.append) REWIND(unit=7)
				DISCPRT=.TRUE.

				open7=.true.
					if(discprt) write(7,90)  
				if(discprt) write(7,91)  
				if(discprt) write(7,92) 
				if(discprt) write(7,93) cDATEW
90				format(/,'  Program HJCFIT Windows Version (Beta)')
91				 format(/,' Copyright D. Colquhoun, I. Vais, University College London 2004',/,&
				' All rights reserved.(Intel Visual Fortran/Gino version)',/,&
				' Please cite: http://www.ucl.ac.uk/Pharmacology/dc.html')
	 
92				format(/, ' HJCFIT: Fit of model to open-shut times with missed events',/,&
				'  (Uses HJC distributions, exact for 1st 2 deadtimes then',/,&
				'  asymptotic, to calculate likelihood of record')

93				format(/,' DATE of analysis = ',a20)
				if(readini) write(7,94) inifile
94				format(/,' Initialisation file = ',a40)
			endif
149			continue
			if(itogrec.eq.7) then ! autosim:view last analyses

				call gmRemoveWindow(initialw)
				!autosim=.true.
				!callid=-133	
				readini=.false.
				nofit=.false.
				nodata=.false.
				curvonly=.false.		
				call ini_file(main,initwin,nset,&
				itogrec,pfiles,nfileb,qmec,mtitle11,rtitle,pfilem,-22,&
				initwin_Toggle3,initwin_Toggle4,&
				initwin_Toggle5,initwin_Toggle6,initwin_Toggle7,initwin_Toggle8,&
			    initwin_Toggle9,initwin_Toggle10,initwin_Toggle11,initwin_Text2,&
				initwin_Toggle_1,initwin_Toggle_2,initwin_TxtArray,initwin_text1)
				call gmSetToggleSwitch(initwin_Toggle_1(4), Gon)
				
				goto 1

			
			endif
		
			call gmenqtextsetting(initialw_Text5,inifile)
			
			
			call gmRemoveWindow(initialw)
			
			call gmDefineKeyselectCallback(13,0)
			if(itogrec.eq.2) callid=-12    ! no ini
			if(itogrec.eq.3.or.itogrec.eq.4) then ! create mech/read only
			    curvonly=.true.
				nodata=.true.
				callid=-29
				goto 2
			endif
			if(itogrec.eq.5) callid=-30 ! demo 1
			if(itogrec.eq.6) callid=-28 ! demo 4
			if(itogrec.eq.8) callid=-31 ! demo autosim
			if(callid.eq.-11.or.callid.eq.-30.or.callid.eq.-28.or.callid.eq.-31) then
			INQUIRE (FILE=iniFILE,EXIST=PRESENT,&
			ACCESS=FACCESS,FORM=FFORM,RECORDTYPE=FRTYPE,RECL=IRCL) 
			if(PRESENT) then
			    ihandle=FILE$FIRST
			    length = GETFILEINFOQQ(inifile, info, ihandle)
			    nLEN=info%length
			    if(nlen.le.100) then
			        imodsav=0
			        imod0=0
			        readini=.false.
			        ista=gmDisplayMessageBox('','No INI file. Try again ',Ginformation,gok)
			
			        autosim=.false.
			!	    call welcome(main,initialw,initialw_MainPanel,inifile, initialw_Text5,initial_toggle)
				    call welcome(main,initialw,initialw_MainPanel,inifile, initialw_Text5,initial_toggle,&
			        initialw_Toggle3,initialw_Toggle4,initialw_Toggle5,initialw_Text2,pfilem,&
                    textcomp,textid,drives)
					      
			    else if(nlen.gt.100.and.nlen.le.20480) then	!old file	  
		            OPEN(unit=19,file=inifile,status='UNKNOWN',access='DIRECT',form='UNFORMATTED',recl=10240)
		            read(19,rec=1) itogini
		            close(unit=19)
		           ! if(itogini.le.10) then
                   !     ista=gmDisplayMessageBox('','No proper INI file. Try again',Ginformation,gok)
			
			         !   autosim=.false.
				     !   call welcome(main,initialw,initialw_MainPanel,inifile, initialw_Text5,initial_toggle,&
			         !   initialw_Toggle3,initialw_Toggle4,initialw_Toggle5,initialw_Text2,pfilem,&
                      !  textcomp,textid,drives)
				      !  goto 1 
		            
                  !  endif
			
			    
			    else 
			        open(unit=19,file=inifile,status='UNKNOWN',access='DIRECT',form='BINARY',recl=1)
                    read(19,rec=1) iftype
                    close(unit=19)
                    !if(iftype.ne.100) then
                   ! ista=gmDisplayMessageBox('','No proper INI file. Try again ',Ginformation,gok)
			
			      !  autosim=.false.
				  !  call welcome(main,initialw,initialw_MainPanel,inifile, initialw_Text5,initial_toggle,&
			      !  initialw_Toggle3,initialw_Toggle4,initialw_Toggle5,initialw_Text2,pfilem,&
                   ! textcomp,textid,drives)
				  !  goto 1 
			      !  endif
			    endif
			
			
			readini=.true.
			
			!readini.here
			neqold=0
			autosim=.false.
			
			call read_ini(inifile,pfileb,nfix,jfix,neq,IE,JE,EFAC,&
			IF,JF,jcon,IM2,JM2,jmic,ndisp,irecq,ans1,ylo,yhi,nrange,idiskq,&
			nfileb,kfile,nset,conc,tcrit,burst,&
			idatyp,qfile,imodold,setbad,tcbad,onechan,&
			nbad1,isbad,iebad,tresolb,ans3,ans4,nlvar,&
			fixec50,ec501,i50,j50,m50,xqlo,xqhi,kAm,kFm,&
			chsvec,ncyc2,nsc2,qfilem,nsim,irect,logsav,imodolds,&
			badend,excop,dcmod,nlen,&
			nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,useprim,pfiles,qmec,nmr,nlig,&
			conc_ec1,conc_ec2,iftype,iexcop,gaplo,gaphi,nskip)
			ksim=-2
			irecqi=irecq
			neqold=neq
			nranged=nrange
			if(idatyp.eq.1) then
					exten='.SCN'
			else if(idatyp.eq.2) then
				exten='.DAT'
			else if(idatyp.eq.4) then
				exten='.EDE'
				iscan=201	!signals .ede file (but not in file of course)
			else if(idatyp.eq.5) then
				iscan=301	!signals .evl file (but not in file of course)
				exten='.EVL'
			endif
			if(idatyp.eq.3) autosim=.true.
		 	if(iftype.ne.100) then
				nbl=len_trim(qfilem)
				ilp=1
				do i=1,nbl
					if(qfilem(i:i).eq.'\') ilp=i
				enddo
				if(ilp.eq.1) then
					iplen = FULLPATHQQ(qfilem, pathdat2)
					nbl1=len_trim(pathdat2)
					ilp1=1
					do i=1,nbl1
						if(pathdat2(i:i).eq.'\') ilp1=i
					enddo
					pathmec=pathdat2(1:ilp1)
					qmec=pathdat2
				
				else
					pathmec=qfilem(1:ilp)
					qmec=qfilem
					text2=qfilem(ilp+1:nbl)
					qfilem=' '
					qfilem=text2(1:40)
					
				endif
				if(.not.autosim) then
                do j=1,nset
					do l=1,nfileb(j)
					nbl=len_trim(pfileb(l,j))
				    ilp=1
				    do i=1,nbl
						if(pfileb(l,j)(i:i).eq.'\') ilp=i
					enddo
					if(ilp.eq.1) then
					iplen = FULLPATHQQ(pfileb(l,j), pathdat1)
					nbl1=len_trim(pathdat1)
					ilp1=1
					do i=1,nbl1
						if(pathdat1(i:i).eq.'\') ilp1=i
					enddo
					pfiles(l,j)=pathdat1
				
				    else
					pathdata=pfileb(l,j)(1:ilp)
					
					text1=pfileb(l,j)(ilp+1:nbl)
					pfiles(l,j)=' '
					pfiles(l,j)=pfileb(l,j)
					pfileb(l,j)=text1
					endif
					enddo
					enddo
				endif
				
			endif
			
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
			nmod=imod0
			
			if(irecq.lt.1) irecq=1		!temp!
				do j=1,nset
					tresol(j)=tresolb(j)
					do i=1,10
						conc1(i,j)=conc(i,j) !save
					enddo
				enddo
       	!	if(callid.eq.-30.or.callid.eq.-28) then
	
				if(open7) close(unit=7)
				if(discprt) then
				OPEN(unit=7,file=PFILEm,status='UNKNOWN',&
				access='APPEND',form='FORMATTED',carriagecontrol='FORTRAN',iostat=i_var)
				if(i_var.ne.0) then
			    close(unit=7)
				imes=gmdisplaymessagebox('','Invalid path/file name or file already open',gstop,gok)
				discprt=.false.
				open7=.false.
				goto 1999
	
				endif
				if(.not.append) REWIND(unit=7)
			!	DISCPRT=.TRUE.

				open7=.true.
                ENDIF
1999			continue
				if(discprt) write(7,90)  	
				if(discprt) write(7,91)  
				if(discprt) write(7,92) 
				if(discprt) write(7,93) cDATEW
				if(discprt) write(7,94) inifile  
			   
		
		   !	endif
		  else
			imodsav=0
			imod0=0
			readini=.false.
			ista=gmDisplayMessageBox('','No INI file. Try again',Ginformation,gok)
			
			autosim=.false.
			!	call welcome(main,initialw,initialw_MainPanel,inifile, initialw_Text5,initial_toggle)
				call welcome(main,initialw,initialw_MainPanel,inifile, initialw_Text5,initial_toggle,&
			initialw_Toggle3,initialw_Toggle4,initialw_Toggle5,initialw_Text2,pfilem,&
            textcomp,textid,drives)
			goto 1 
			
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
		
		
		
			if(readini) then
				present=.false.
				
				INQUIRE (FILE=qmec,EXIST=PRESENT,Recl=ircl,iostat=i_var)
				if(i_var.ne.0) then
					goto 1
				endif
				if(present) then
					ihandle=FILE$FIRST
					length = GETFILEINFOQQ(qmec, info, ihandle)
					nLEN=info%length 
			
					if(nlen.gt.1000) then
				
							readmec=.true.
							
							callid=-44 ! if mechanism file exist
							goto 2	
							
					else
							call gmDefineKeySelectCallback(13,-235)
							call change_pathm(main,initwin2,itextwin2,pathmec,qmec,-235)
							
					endif
				else
					call gmDefineKeySelectCallback(13,-235)
					
					call change_pathm(main,initwin2,itextwin2,pathmec,qmec,-235)
					
				endif
			else
 				readmec=.false.
				mtitles=' '
				rtitle=' '
				callid=-44
			    
				itogrec=2
				goto 2
			endif
case(-133)
			CALL gmFileBROWSER(simFILe1,ndir,nfiltsim,gmBrowseType=0)
			if(simfile1.ne.'  ') then
				INQUIRE (FILE=simFILE1,EXIST=PRESENT,&
				ACCESS=FACCESS,FORM=FFORM,RECORDTYPE=FRTYPE,RECL=IRCL) 
				if(PRESENT) then
					ihandle=FILE$FIRST
					length = GETFILEINFOQQ(simfile1, info, ihandle)
					nLEN=info%length
					if(nlen.lt.50) then
						imko=gmdisplaymessagebox('','Not a proper simval.dat file',ginformation,gok)
						goto 1
					else
					OPEN(unit=10,file=simfile1,status='UNKNOWN',&
     				access='DIRECT', form='BINARY',RECL=1)
					read(unit=10,rec=1) iver1,nsims1
					if(iver1.le.101) then
						read(unit=10,rec=1) iver1,nsims1,ABORTW,npar,nset
						callid=6001
						close(unit=10)
						goto 2
					else if(iver1.le.106) then
						read(unit=10,rec=1) iver1,nsims1,npar,nset
						callid=6001
						close(unit=10)
						goto 2
					else
						close(unit=10)
						imko=gmdisplaymessagebox('','Not a proper simval.dat file',ginformation,gok)
						goto 1
					endif
					endif
				else
					imko=gmdisplaymessagebox('','Not present',ginformation,gok)
					goto 1
				endif
		    else
				goto 1
			endif
		
case(-44) ! read ini from -11 after welcome
		call gmDefineKeyselectCallback(13,0)
		icalnext=0
        if(itogrec.ne.2) then
		   
		   INQUIRE (FILE=qmec,EXIST=PRESENT,Recl=ircl,iostat=i_var)
		   if(i_var.ne.0) then
				goto 1
		   endif
		   if(present) then	
				ihandle=FILE$FIRST
		        length = GETFILEINFOQQ(qmec, info, ihandle)
				nLEN=info%length 
			
				if(nlen.gt.1000) then
					readmec=.true.
							
				else
					ista=gmDisplayMessageBox(qmec,'File too small/corrupt.Try again?',&
					gquestion,gyesno)
					if(ista.eq.gnobutton) then
					readmec=.false.
					mtitle11=''
					rtitle=' '
					itogrec=2
					else
					icalnext=-44
						
					callid=-233
					goto 2
					endif
				!	readini=.false.			
				endif
			else
				ista=gmDisplayMessageBox(qmec,': File not present.Try again',gquestion,gyesno)
				if(ista.eq.gnobutton) then
				readmec=.false.
				mtitle11=''
				rtitle=' '       
				itogrec=2
				else
					icalnext=-44
						
					callid=-233
					goto 2
				endif
			!	readini=.false.		
			endif
			endif
			if(readmec) then
					irecm0=irecq
					OPEN(unit=15,file=qmec,status='UNKNOWN',access='DIRECT', &
					form='BINARY', RECL=1,iostat=i_var)
					if(i_var.ne.0) then
					    close(unit=15)
						goto 1
					endif	
					read(unit=15,rec=1) iver5,nrecs,nextrec,ireclast,jstart
					close(unit=15)		
					if(iver5.gt.10000.or.jstart(1).gt.40000) then
					    							
						 ista=gmDisplayMessageBox(qmec(1:n),'Mechanism file.'//qmec(1:n)//'.seems to be corrupted. Try another .mec file?',gquestion,gyesno)							! check that mechanism file is valid -if not then say "mechanism fle 'qmec; is not as expected, brows for the right .mec file'					
						 icalnext=-44						
						 callid=-233
						 goto 2
					endif
					IWRONG=0
					if(autosim) then
						OPEN(unit=15,file=qmec,status='UNKNOWN',access='DIRECT', form='BINARY', RECL=1)
			
						irec=jstart(irect)
					    if(irect.ge.1.and.irect.le.nrecs.and.irec.gt.0) then										
						   read(unit=15,rec=irec) iver5,imod11,mtitle11,kg,kAg,kBg,kCg,kDg,rtitle1
						   close(unit=15)
						    kfg=kbg+kcg+kdg
							if(kam.ne.kag.or.kfm.ne.kbg+kcg+kdg) then
								call intconv(irect,cnum5)
								Iwrong=-1
								kaz=kag
								kfz=kfg
							endif	
						else						
						  
						    call intconv(irect,cnum5)
						    Iwrong=-1
							kaz=-1
							kfz=-1
						endif
						if(irecq.ge.1.and.irecq.le.nrecs) then
							irec=jstart(irecq)
							OPEN(unit=15,file=qmec,status='UNKNOWN',access='DIRECT', form='BINARY', RECL=1)
			
							read(unit=15,rec=irec) iver5,imod,mtitles,k,kA,kB,kC,kD,rtitle
							close(unit=15)
							kf=kb+kc+kd
							if(kam.ne.ka.or.kfm.ne.kb+kc+kd) then
								call intconv(irecq,cnum1)
								if(iwrong.eq.-1) then
									iwrong=-3
								else
									iwrong=-2
								endif
								kaz1=ka
								kfz1=kf
							endif	
						else
							n=len_trim(qmec)
							call intconv(irecq,cnum1)
							if(iwrong.eq.-1) then
									iwrong=-3
								else
									iwrong=-2
								endif
								kaz1=-1
								kfz1=-1
							
						endif
						if(iwrong.ne.0) then
						call wrong_mec(main,itogwrong,itogglewr1,itogglewr2,imod,&
							imodold,irect,qmec,kam,kfm,kaz,kfz,irecq,kam,kfm,kaz1,kfz1,rtitle,itogglewr3,itogglewr4)
						goto 1
						endif
					else
						if(irecq.ge.1.and.irecq.le.nrecs) then
							irec=jstart(irecq)
							OPEN(unit=15,file=qmec,status='UNKNOWN',access='DIRECT', form='BINARY', RECL=1)
			
							read(unit=15,rec=irec) iver5,imod,mtitles,k,kA,kB,kC,kD,rtitle
							close(unit=15)
							kf=kb+kc+kd
							if(kam.ne.ka.or.kfm.ne.kb+kc+kd) then
								call intconv(irecq,cnum5)
									call wrong_mec(main,itogwrong,itogglewr1,itogglewr2,imod,&
								imodold,irecq,qmec,kam,kfm,ka,kf,-1,ka,kf,kaz1,kfz1,rtitle,itogglewr3,itogglewr4)
								iwrong=-1
								goto 1
							endif	
						else
							n=len_trim(qmec)
							call intconv(irecq,cnum5)
							call wrong_mec(main,itogwrong,itogglewr1,itogglewr2,imod,&
							imodold,irecq,qmec,kam,kfm,-1,-1,-1,ka,kf,kaz1,kfz1,rtitle,itogglewr3,itogglewr4)
							iwrong=-1
							goto 1
							
						endif
					endif
			endif
			callid=-441
			goto 2
case(-441)			
			if(itogrec.eq.1.or.itogrec.eq.5.or.itogrec.eq.6) then
			if(.not.autosim) then
				do j=1,nset
				do i=1,nfileb(j)
				nfile(j)=nfileb(j)
				iplen = FULLPATHQQ(pfiles(i,j), pathdat1)
				present=.false.
				INQUIRE(FILE=pfiles(i,j),EXIST=PRESENT)
				nlen=0
				if(PRESENT) then
					ihandle=FILE$FIRST
					length = GETFILEINFOQQ(pfiles(i,j), info, ihandle)
					nLEN=info%length 
					if(nlen.lt.1000) then
					call gmDefineKeyselectCallback(13,0)
						imes=gmdisplaymessagebox('','File does not exist. Change path ',gstop,gok)
						icalnext=-45
						
						callid=-223
				   
					goto 2
					endif
				else
				!	imes=gmdisplaymessagebox('','File does not exist. Change path ',gstop,gok)
					call gmDefineKeyselectCallback(13,0)
				
					callid=-223
				   icalnext=-45
					goto 2			
				endif
				
			enddo
		enddo
		endif
		endif

		callid=-45
		goto 2
case(-90) ! wrong mechanism in autosim
		istatw1=gmEnqToggleSwitch(iTogglewr1)
		istatw2=gmEnqToggleSwitch(iTogglewr2) !look for right mechanism
		istatw3=gmEnqToggleSwitch(iTogglewr3)
		call gmremovewindow(itogwrong)
		if(istatw1.eq.1) then
			icalnext=-44
			callid=-233
			goto 2
		else if(istatw2.eq.1) then
		
			OPEN(unit=15,file=qmec,status='UNKNOWN',access='DIRECT', form='BINARY', RECL=1)

	
			read(unit=15,rec=1) iver,nrecs,nextrec,ireclast,jstart
			imodmax=0
			do irq=1,nrecs
			irec=jstart(irq)	!previous nextrec
			read(unit=15,rec=irec) iver,imodk,mtitlek,k,kA,kB,kC,kD,rtitle
			if(imodk.gt.imodmax) imodmax=imodk
			mtits(imodk)=mtitlek	!so mtits(i)=title of model #i
			ijmod(irq)=imodk
			rtitles(irq)=rtitle
	   
			enddo
			nmods=0
			do irq=1,nrecs
			imodk=ijmod(irq)	!just defined
		
			repeat=.false.
			if(irq.gt.1) then	!has imod occurred already?
			do j=1,irq-1
				if(ijmod(j).eq.imodk) repeat=.true.	!imod already occurred
				if(nratek(imodk).lt.20) then
				nratek(imodk)=nratek(imodk)+1
				ratetitle(nratek(imodk),imodk)=rtitles(j)
				endif
			enddo
			endif
			if(.not.repeat) nmods=nmods+1
			enddo
	
			irec=ireclast
			indx=0	
			do imd=1,imodmax  !list all the model titles (and details when iread=5)
			present=.false.
			do j=1,nrecs
			if(imd.eq.ijmod(j)) then
				present=.true.     	!model #im is present
			endif
			enddo
			if(present) then
				indx=indx+1
				CALL INTCONV(IMD,CNUM0)
				text7(indx)=cnum0(1:3)//MTITS(iMD)
			endif
			enddo
		
			ifiltype=11
			ncol=2
			label7(1)='Model'
			label7(2)='Title'
			iwidk(1)=80
			iwidk(2)=240
			close(unit=15)
			call PARSNAME(qmec,path1,ndev,pname,suffix,&
     		nopath,60)
			nb=len_trim(pname)
			do i=1,80
			wtitle(i:i)=char(32)
			enddo
			wtitle='Choose a mechanism from '//pname(1:nb)//'.'//suffix(1:3)
			call list_of_records(Main,Records,List7_1,2,indx,label7,text7,&
			iwidk,ifiltype,Button7,wtitle)
		
			goto 1
		else if(istatw3.eq.1) then
			if(itogrec.eq.1.or.itogrec.eq.5.or.itogrec.eq.6) then
			if(.not.autosim) then
				do j=1,nset
				do i=1,nfileb(j)
				nfile(j)=nfileb(j)
				iplen = FULLPATHQQ(pfiles(i,j), pathdat1)
				present=.false.
				INQUIRE(FILE=pfiles(i,j),EXIST=PRESENT)
				nlen=0
				if(PRESENT) then
					ihandle=FILE$FIRST
					length = GETFILEINFOQQ(pfiles(i,j), info, ihandle)
					nLEN=info%length 
					if(nlen.lt.1000) then
					call gmDefineKeyselectCallback(13,0)
						imes=gmdisplaymessagebox('','File does not exist. Change path ',gstop,gok)
						icalnext=-45
						
						callid=-223
				   
					goto 2
					endif
				else
				!	imes=gmdisplaymessagebox('','File does not exist. Change path ',gstop,gok)
					call gmDefineKeyselectCallback(13,0)
				
					callid=-223
				    icalnext=-45
					goto 2			
				endif
				
				enddo
				enddo
			endif
			endif

			callid=-45
			goto 2
		else
			callid=-10
			goto 2
		endif
case(-91) ! for autosim
	call gmEnqListStatus(List7_1,nentry,nselect,nfirst)
				
	if(nentry.gt.0) then
		jselect=-1
		
		do i=1,nentry
					
		kf=0
		istat(i)=gmEnqListEntry(List7_1,i,TEXT7(i))
		if(istat(i).eq.2) then
			Cnum0=TEXT7(i)(1:3)
			CALL CHTOINT(Cnum0,IMODEL)
			irc=i
			imod=imodel
			lt=0
			do k=1,nrecs
						
							if(ijmod(k).eq.imod) then
								irecq=k
								lt=lt+1
								ratcons(k)%imod=imod
								!text_tog(lt)=rtitles(k)
								call intconv(k,cnum5)
								radio_text(lt)=cnum5(1:3)//':'//rtitles(k)
								indrec1(lt)=k
								indrat=indrec1(lt)
								if(ksim.eq.-2) then

								irect=indrat
								else
								irecqi=indrat
								endif
							endif
			enddo
			iarca=0
			imod0=imodel
			if(lt.gt.1) then
				iarca=-2
				nmod=imod
				titlerp='Titles for stored rate constants:'
				call radio_panel(main,iradio,lt,iradio_toggle,radio_text,0,&
						iradiox,radiox,iradioy,radioy,1500,titlerp)
				
				goto 1
			else				
				indk=indrat
				if(ksim.eq.-2) then
					irecm0=irect
				else
					irecm0=irecqi
				endif
			endif
		endif
		enddo
	endif	
	if(.not.autosim) then		
	OPEN(unit=15,file=qmec,status='UNKNOWN',access='DIRECT', form='BINARY', RECL=1)

	irec=jstart(irecq)
	read(unit=15,rec=irec) iver5,imod,mtitles,k,kA,kB,kC,kD,rtitle
	close(unit=15)
	endif					
	if(itogrec.eq.1.or.itogrec.eq.5.or.itogrec.eq.6) then
			if(.not.autosim) then
				do j=1,nset
				do i=1,nfileb(j)
				nfile(j)=nfileb(j)
				iplen = FULLPATHQQ(pfiles(i,j), pathdat1)
				present=.false.
				INQUIRE(FILE=pfiles(i,j),EXIST=PRESENT)
				nlen=0
				if(PRESENT) then
					ihandle=FILE$FIRST
					length = GETFILEINFOQQ(pfiles(i,j), info, ihandle)
					nLEN=info%length 
					if(nlen.lt.1000) then
					call gmDefineKeyselectCallback(13,0)
						imes=gmdisplaymessagebox('','File does not exist. Change path ',gstop,gok)
						icalnext=-45
						
						callid=-223
				   
					goto 2
					endif
				else
				!	imes=gmdisplaymessagebox('','File does not exist. Change path ',gstop,gok)
					call gmDefineKeyselectCallback(13,0)
				
					callid=-223
				   icalnext=-45
					goto 2			
				endif
				
				enddo
				enddo
			endif
			endif

			callid=-45
			goto 2
case(-45) ! continue for readini
			call gmDefineKeyselectCallback(13,0)
			if(itogrec.eq.1.or.itogrec.eq.2.or.itogrec.eq.5.or.itogrec.eq.6) then ! demo+ini
				call gmDefineKeyselectCallback(13,-22) !-10)
				callid=-22
				
			endif
		!	autosim=.false.
		icalnext=0
			if(readini.and.idatyp.eq.3) then ! autosim
				autosim=.true.
				call intconv(imod,cnum(3))
				if(imod11.eq.0) imod11=imod
				call intconv(imod11,cnum51)
				call intconv(irect,cnum(1))
				call intconv(irecq,cnum(2))
				if(itogrec.eq.8) then  ! demo sim
					callid=-291
					goto 2
				else
				call ini_mechanism(main,imecform1,qmec,im,imecform1_Toggle1,imecform1_Toggle2,&
				imecform1_Toggle3,imecform1_Toggle4,imecform1_Toggle5,imecform1_Toggle6,readmec,&
				imod,cnum(3)(1:3)//':'//mtitles,cnum(2)(1:3)//':'//rtitle,imod11,cnum51(1:3)//':'//mtitle11,cnum(1)(1:3)//':'//rtitle1)
				endif
		    else
		        if (nset.lt.1) nset=1
		        icalini=-22
			    call ini_file(main,initwin,nset,itogrec,pfiles,nfileb,qmec,mtitle11,&
			    rtitle,pfilem,icalini,initwin_Toggle3,initwin_Toggle4,&
				initwin_Toggle5,initwin_Toggle6,initwin_Toggle7,initwin_Toggle8,&
			    initwin_Toggle9,initwin_Toggle10,initwin_Toggle11,initwin_Text2,&
				initwin_Toggle_1,initwin_Toggle_2,initwin_TxtArray,initwin_text1)
		        
				if (idatyp.lt.1) idatyp=1

				if(itogrec.eq.2) then
					call gmSetToggleSwitch(initwin_Toggle_1(idatyp+1), Gon)
				else if(itogrec.eq.7) then
					call gmSetToggleSwitch(initwin_Toggle_1(4), Gon)
			
				else
				    !nb=len_trim(text1)
					nb=0
					call gmSetToggleSwitch(initwin_Toggle_1(1), Gon)
					do j=1,nset
						do i=1,nfileb(j)
					
						call gmSetCellSetting(initwin_TxtArray, i,j ,gmString=pfiles(i,j))
						call gmdrawwidget(initwin_TxtArray)
						enddo
					enddo
				endif
				
			
		endif
		case(-13) !(-50) ! change inifile
			
			CALL gmFileBROWSER(iniFILE,inipath,inidef,gmBrowseType=0)
			 IF(iniFILE.ne.' ') then
			 nl=len_trim(inipath)
			 inifile=inipath(1:nl)//'\'//inifile
			 call gmsettextsetting( initialw_Text5,inifile)
			 inifile0=inifile
			 endif
		case(-35) ! change print file
			CALL gmFileBROWSER(pfilem,pdir,pfilt,gmBrowseType=0)
			 IF(pfilem.ne.' ') then
			 nl=len_trim(pdir)
			 pfilem=pdir(1:nl)//'\'//pfilem
			 call gmsettextsetting( initialw_Text2,pfilem)
			 endif
		case(-14) !help
			imesh=gmdisplaymessagebox('','For now,please use the toolbar help',ginformation,gok)
		case(-4) ! save as default in inini
			autosim=.false.
			nofit=.false.
			nodata=.false.
			liksurf=.false.
			curvonly=.false.
			istatus13=gmEnqToggleSwitch(initialw_Toggle3)
			istatus14=gmEnqToggleSwitch(initialw_Toggle4)
			istatus15=gmEnqToggleSwitch(initialw_Toggle5)
			if(istatus15.eq.gon) then 
				discprt=.false.
				append=.false.
			else if(istatus13.eq.gon) then 
				discprt=.true.
				append=.true.
			else if(istatus14.eq.gon) then 
				discprt=.true.
				append=.false.
			endif
		    OPEN(UNIT=13,FILE=iniiniFILE,&
            ACCESS='DIRECT',FORM='UNFORMATTED',RECL=irecl)
		
			
			write(13,rec=1) itogrec,inifile,pfilem,nodata,curvonly,nofit,autosim,discprt,append
    
			close(unit=13)
		case(-21) !(-10)  ! use ini file
			! option in initial settings
		
			autosim=.false.
		!	nofit=.false.
		!	nodata=.false.
		!	liksurf=.false.
		!	curvonly=.false.
			
199			continue
			if(ibib.eq.-1)  then
				autosim=.false.
				nofit=.true.
				curvonly=.true.
				im=-1
				!call gmRemoveWindow(initwin)
				goto 112
			endif

			istatus3=gmEnqToggleSwitch(initwin_Toggle6)
		
			istatus5=gmEnqToggleSwitch(initwin_Toggle8)
		!!! check this !!!
			if(istatus3.eq.gon) then 
				do i=1,7
					istatus9=gmEnqToggleSwitch(initwin_Toggle_1(i))
					if(istatus9.eq.gon) iopt=i
				enddo
				autosim=.false.
				if(iopt.eq.1) then
					idatyp=1
				else if(iopt.eq.2) then
					idatyp=2 		!keep as in earlier versions
				else if(iopt.eq.3) then
					idatyp=3
					autosim=.true.
				else if(iopt.eq.4) then
					idatyp=4		!Dempster .EDE file
				else if(iopt.eq.5) then
					idatyp=5		!Axon .EVL file
				endif
				
				if(iopt.eq.3) then 
					autosim=.true.
					call gmSetWidgetStatus(new_file, GSELECTABLE)
					if(allocated(thtrue)) deallocate(thtrue)
						allocate(thtrue(200))
						do i=1,200
							thtrue(i)=0.d0
						enddo
						if(allocated(qttrue)) deallocate(qttrue,qtsav)
						allocate(QTtrue(100,100))
						allocate(QTsav(100,100))	!to check for changes
						do i=1,k
						do j=1,k
							QTtrue(i,j)=0.d0
							QTsav(i,j)=0.d0
						enddo
						enddo
					
				
				else 
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
				idatyp=3
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
				OPEN(UNIT=13,FILE=iniiniFILE,&
				ACCESS='DIRECT',FORM='UNFORMATTED',RECL=irecl)
		
			
				write(13,rec=1) itogrec,inifile,pfilem,nodata,curvonly,nofit,autosim,discprt,append
    
				close(unit=13)
				goto 1
			else
				if(ibib.ne.-1) call gmRemoveWindow(initwin)
!				call prog_bar(main,ipbar,icho,ipbar_Progress2)
		
		
				call gmDefineKeyselectCallback(13,0)
			!	call gmDefineKeyselectCallback(13,-50)
				do i=1,60
						pathdat1(i:i)=' '
		
	   			enddo
             endif
			 if(curvonly) then
				if(readini) then
						call intconv(imod,cnum(3))
				if(imod11.eq.0) imod11=imod
				call intconv(imod11,cnum51)
				call intconv(irect,cnum(1))
				call intconv(irecq,cnum(2))
				else
					readmec=.false.
					mtitles=' '
					qfilem=' '
					qmec=''
					do i=1,4
					cnum(i)=' '
					enddo
					cnum51=' '
				endif
			
				call ini_mechanism(main,imecform1,qmec,im,imecform1_Toggle1,imecform1_Toggle2,&
					imecform1_Toggle3,imecform1_Toggle4,imecform1_Toggle5,imecform1_Toggle6,readmec,&
	imod,cnum(3)(1:3)//':'//mtitles,cnum(2)(1:3)//':'//rtitle,imod11,cnum51(1:3)//':'//mtitle11,cnum(1)(1:3)//':'//rtitle1)

			
			 else


				if(readini) then
				    
					if(autosim) then			
				
					else
				!		callid=
				!		goto 2
					endif	
				else
					if(autosim) then			
				
					else
				!		callid=
				!		goto 2
					endif
				endif
			endif

			if(readini) then
				INQUIRE (FILE=qmec,EXIST=PRESENT)
				ihandle=FILE$FIRST
			 length = GETFILEINFOQQ(qmec, info, ihandle)
			 nLEN=info%length
			
				if(present.and.nlen.gt.500) then
					readmec=.true.
					callid=-22
					goto 2
				else
				
				ista=gmDisplayMessageBox('',qmec//':Not present.Change directory/file',ginformation,gok)
					readmec=.false.
					
				iplen = FULLPATHQQ(qfilem, pathdat1)
				nbl=len_trim(qfilem)
				do i=1,nbl
					if(qfilem(i:i).eq.'\') ilp=i
				enddo
				filepure=qfilem(ilp+1:nbl)
				filepure='   '
				CALL gmFileBROWSER(filepure,qDIR,qFILT,gmBrowseType=0)
				if(filepure.ne.'              ') then
				INQUIRE (FILE=filepure,EXIST=PRESENT)
				ihandle=FILE$FIRST
			 length = GETFILEINFOQQ(filepure, info, ihandle)
			 nLEN=info%length
			
				if(present.and.nlen.gt.500) then	
					callid=-22
					readmec=.true.
					qfilem=filepure
					goto 2
				endif
				endif
				ista=gmDisplayMessageBox('',qmec//':Not present.Use menu bar',ginformation,gok)
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
111			call ini_mechanism(main,imecform1,qmec,im,imecform1_Toggle1,imecform1_Toggle2,&
			imecform1_Toggle3,imecform1_Toggle4,imecform1_Toggle5,imecform1_Toggle6,readmec,&
	imod,mtitles,rtitle,imod11,mtitle11,rtitle1)

	!	endif	

			


				
case(-22) ! continue with reading files
			newmodel=.false.
			newdata=.false.
			! continue from initila settings
			! iopt for data type
			! ioptm for mech same,new,old
			! itogrec for ini,not ini,demo,autosim,etc
			call gmDefineKeySelectCallback(13,0)
			if(itogrec.ne.3.and.itogrec.ne.4) then ! change new/old
				
				istatus6=gmEnqToggleSwitch(initwin_Toggle6) !fit data
				istatus8=gmEnqToggleSwitch(initwin_Toggle8) ! curves only
			
			
				if(istatus8.eq.gon) then
					nodata=.true.
					nofit=.true.
					curvonly=.true.
				endif
				
			    if(itogrec.eq.2.or.itogrec.eq.7) then
					do i=2,7
					istatus=gmEnqToggleSwitch(initwin_Toggle_1(i))
					if(istatus.eq.gon) iopt=i
					enddo
					idatyp=iopt-1
				!	if(iopt.gt.5.or.iopt.eq.3) then
					if(iopt.eq.3) then
					!	imes=gmdisplaymessagebox('','Not yet done',ginformation,gok)
					!	goto 1
					endif
				else
				!	call gmenqtextsetting(initwin_text1,text1)
					
				
					do i=1,7
					istatus=gmEnqToggleSwitch(initwin_Toggle_1(i))
					if(istatus.eq.gon) iopt=i
					enddo
					idatyp=iopt-1
					if(iopt.eq.3) then
						!imes=gmdisplaymessagebox('','Not yet done',ginformation,gok)
						!goto 1
					endif
					if(iopt.eq.1) then
					    nb=len_trim(text1)
					!	pathdata=text1
						do j=1,nset
						do i=1,nfileb(j)
						call gmenqcellsetting(initwin_TxtArray, i,j ,valr,text1)
						if(text1.eq.' ') goto 1
						pfiles(i,j)=text1
						enddo
						enddo
		
					endif
					endif
				endif
			
				if(itogrec.eq.2.or.itogrec.eq.7) then
					do i=3,4
					istatusm=gmEnqToggleSwitch(initwin_Toggle_2(i))
					if(istatusm.eq.gon) ioptm=i
					enddo
				else
					call gmenqtextsetting(initwin_text2,text2)
					if (text2.eq.' ') goto 1
					do i=1,4
					istatusm=gmEnqToggleSwitch(initwin_Toggle_2(i))
					if(istatusm.eq.gon) ioptm=i
					enddo
				endif
				if(ioptm.eq.1.or.ioptm.eq.2) then
				
				
				if(ioptm.eq.2) call help_modify(main,imodhelp)
				nb=len_trim(text2)	
				qmec=text2
				endif
			
		
			!endif
			call gmremovewindow(initwin)
			if(autosim.and.ksim.eq.2) goto 1912
			if(iopt.eq.4) then
			    idatyp=3
				autosim=.true.
				ksim=-1
				readini=.false.
			    goto 1912
                itinfos=gmcreatetextentry(imainpanel,1,2,10,1,&
						'',&
						60, Gedit, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              			gmVpos=GTOP, gmExpand=GOFF)
				itinfoP=gmcreatetextentry(imainpanel,12,2,12,1,&
						'TO PAUSE FIT/SIMULATION: Press "P" or "p"',&
						100, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              			gmVpos=GTOP, gmExpand=GOFF)
				itinfoC=gmcreatetextentry(imainpanel,24,2,14,1,&
						'TO ABORT FIT/SIMULATION: Press "ESC" or "Q" or "q"',100, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              			gmVpos=GTOP, gmExpand=GOFF)
			
1912        continue
			ENDIF
			if(iopt.eq.5) then
				callid=-133
			else
				callid=-222  ! read data for all 
			endif
			goto 2
		
	case(-220,-219) ! read new scan files
	   
		sdir=ddir
		if(idatyp.eq.5) then
			    text1='axon.evl' 
			    dfilth='*.evl'//char(124)//'Axon files (EVL)'//char(124)//'*.*'//char(124)//'All Files'
	
		endif
		if(idatyp.eq.6) then
			    text1='*.ede' 
			    dfilth='*.EDE'//char(124)//'Dempster files (EDE)'//char(124)//'*.*'//char(124)//'All Files'
	
		endif
		if(idatyp.eq.2) then
			    text1='*.dat' 
			    dfilth='*.DAT'//char(124)//'Old scan files (DAT)'//char(124)//'*.*'//char(124)//'All Files'
	
		endif
		do j=1,nset
		do i=1,nfileb(j)
			nb=len_trim(pfiles(i,j))
			ilp=1
			do m=1,nb
			if(pfiles(i,j)(m:m).eq.'\') ilp=m
			enddo
			if(ilp.eq.1) then
			text1=pfiles(i,j)
			else
			text1=pfiles(i,j)(ilp+1:nb)
			endif
			
			CALL gmFileBROWSER(text1,sDIR,DFILTh,gmBrowseType=0)
			if(text1.ne.' ') then
			nb=len_trim(sdir)
			pfiles(i,j)=sdir(1:nb)//'\'//text1
			pfileb(i,j)=text1
			endif
			if(callid.eq.-220) then
				call gmSetCellSetting(initwin1_TxtArray, i,j ,gmString=pfiles(i,j))
				call gmdrawwidget(initwin1_TxtArray)
			

			else
				call gmsettextsetting(initwin_text1,text1)
			endif
			enddo
		enddo
		goto 1
	!	callid=-221
	!	goto 2						
		
	case(-221) ! change path

	
		call gmDefineKeySelectCallback(13,0)
		ibady=0
		do j=1,nset
		do i=1,nfileb(j)
			call gmenqcellsetting(initwin1_TxtArray, i,j,valr,text1)
			present=.false.
		
			if (text1.eq.' ') then
			ibady=-1
		
			endif
			INQUIRE(FILE=text1,EXIST=PRESENT)
			if(.not.present) ibady=-1
			nb=len_trim(text1)	
			pfiles(i,j)=text1
		enddo
		enddo
		if(ibady.eq.-1) then
			imsgb=gmdisplaymessagebox('','One or more files do not exist.Try again',&
			ginformation,gok)
			ibady=0
			GOTO 1
		endif
		call gmremovewindow(initwin1)
		if(icalnext.eq.-45) then
			icalnext=0
			callid=-45
		else if (icalnext.eq.-152) then
			icalnext=0
			if(inipage.eq.-100) then
			call gmsetvaluesetting(ini_value5_1,nset)
			do j=1,nset
			do i=1,nfile(j)
				if(icalprev.ne.-44) then
				
				call gmSetCellSetting(ini_TxtArray5, i,j ,gmString=pfiles(i,j))
				call gmdrawwidget(ini_TxtArray5)
				else 
				call gmSetCellSetting(initwin_TxtArray, i,j ,gmString=pfiles(i,j))
				call gmdrawwidget(initwin_TxtArray)
				endif
			!	pfileb(i,j)=pfiles(i,j)
			enddo
			enddo
			callid=3040
			goto 2
			else if(itogrec.eq.2) then
			callid=3040
			goto 2
			else
			if(ioptm.eq.3.or.ioptm.eq.4) then
			    if(readini) then
			        nfix=0
			        ec501=0.
			        fixec50=.false.
			        nfixec50=0
			    endif
				if(ioptm.eq.3) then
					callid=1100 !35 !new
					newmec=.true.
					newcons=.true.
			        newmr=.true.
					imodsav=0
					imod0=0	
					inipage=0
			
					goto 2
			   else if(ioptm.eq.4) then
					inipage=0
					callid=1200 !36  ! old
			!	call help_modify(main,imodhelp)
					imodsav=0
					imod0=0	
			
			
				    goto 2
				endif
			else if(ioptm.eq.2) then
				ixm=1
				call create_model(ixm,iym,igraph2,imodel,main,form1,form1_txtarray1,&
						graph1_2,GraphMainPanel1_2,graphics1_2,&
						nxmodel,nymodel,mtype,models,mod_create,irc,qmec,text7(i),&
						dxs,dys,ijmod,ka,kf,ic,indrat,pfilem,open7,append,ipbar_progress2,&
						indwin,ncdep,iformText,autosim,ksim,&
		initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
				imod0=imodel
					call read_rates(main,form1,Form1_TxtArray1,irecm0,imod,qmec,nrows,indrat,&
					ratcons,models,ic,ipbar_progress2,igraph2,nmr,conc,iformText)
					npar=models(igraph2)%npar
					goto 1	
			else if(ioptm.eq.1) then
					callid=-230
					goto 2
			endif
		
			endif
		else
			callid=3040
			goto 2
			!callid=-222
		endif
		goto 2
	case(-223) !change path
			call gmDefineKeySelectCallback(13,0)
			icallid=-221
			call change_path(main,initwin1,initwin1_TxtArray,nset,&
				icallid,pfiles,nfileb,0)
				call gmDefineKeySelectCallback(13,-221)
			
	case(-227)
		call gmSetWidgetStatus(initwin_text2, GSELECTABLE)
	case(-228)
	if(itogrec.ne.2.and.itogrec.ne.7) then
		call gmSetWidgetStatus(initwin_text2, GunSELECTABLE)
	endif
	case(-229)
		call gmSetWidgetStatus(initwin_text2, GunSELECTABLE)
	case(-233)
	     icallid=-234
		 call gmDefineKeySelectCallback(13,-234)
					
		 call change_pathm(main,initwin2,itextwin2,pathmec,qmec,icallid)
    case(-236,-237) ! change path mec
		CALL gmFileBROWSER(qfilem,qDIR,qFILT,gmBrowseType=0)
		if(qfilem.ne.' ') then
		nb=len_trim(qdir)
		pathmec=qdir(1:nb)//'\'
		nb1=len_trim(pathmec)
		qmec=pathmec(1:nb1)//qfilem
		endif
		if(callid.eq.-236) then
		call gmsettextsetting(itextwin2,qmec)
		else
		call gmsettextsetting(initwin_text2,qmec)
		endif
	case(-234,-235)
	call gmDefineKeySelectCallback(13,0)
	call gmenqtextsetting(itextwin2,text2)

		if (text2.eq.' ') goto 1
		call gmremovewindow(initwin2)
	
		
		ilp=0
	    do k=1,nbl
	!			if(qfilem(k:k).eq.'\') ilp=k
		enddo
	!	filepure=qfilem(ilp+1:nbl)
		nb=len_trim(text2)
	!	pathmec=text2	
		qmec=text2
		if(callid.eq.-235.or.icalnext.eq.-44) then
			callid=-44
			icalnext=0
		else		
			callid=-222
		endif
		goto 2
	case(-222) ! read data
		if(istatus6.eq.gon) then
		if(autosim.and.ksim.eq.2) goto 3232
		if(readini) then
	
		do j=1,nset
			do i=1,nfileb(j)
				nfile(j)=nfileb(j)
				iplen = FULLPATHQQ(pfiles(i,j), pathdat1)
				present=.false.
				INQUIRE(FILE=pfiles(i,j),EXIST=PRESENT)
				nlen=0
				if(PRESENT) then
					ihandle=FILE$FIRST
					length = GETFILEINFOQQ(pfiles(i,j), info, ihandle)
					nLEN=info%length 
					if(nlen.lt.100) then
						imes=gmdisplaymessagebox('','File does not exist. Change path ',gstop,gok)
						callid=-223
				   
					goto 2
					endif
					nb=len_trim(pfiles(i,j))
					ilp=1
					do m=1,nb
						if(pfiles(i,j)(m:m).eq.'\') ilp=m
					enddo
					if(ilp.eq.1) then
						pfileb(i,j)=pfiles(i,j)
					else
						pfileb(i,j)=pfiles(i,j)(ilp+1:nb)
					endif
	
				else
				!	imes=gmdisplaymessagebox('','File does not exist. Change path ',gstop,gok)
					callid=-223
				   
					goto 2			
				endif
				
			enddo
		enddo
		
	
				present=.false.
				INQUIRE(FILE=qmec,EXIST=PRESENT)
				nlen=0

				if(PRESENT) then
					ihandle=FILE$FIRST
					length = GETFILEINFOQQ(qmec, info, ihandle)
					nLENm=info%length 
					if(nlenm.lt.100) then
					!	imes=gmdisplaymessagebox('','File does not exist. Change path ',gstop,gok)
						callid=-233
				   
					goto 2
					endif
				else
					imes=gmdisplaymessagebox('','File does not exist. Change path ',gstop,gok)
					callid=-233
				   
					goto 2			
	
				endif
	
	else
	    
	endif
	
	
	else
	    if(.not.readini) then
	        if(ioptm.eq.3) itogrec=3
	        if(ioptm.eq.4) itogrec=4
	        callid=-29
	        goto 2
	    endif
	endif
	if(icalnext.eq.-45) then
		   callid=-45
		   icalnext=0
	endif
3232    continue	
		   if(iopt.eq.2.or.iopt.eq.4.or.iopt.eq.6.or.iopt.eq.8.or.iopt.eq.7.or.iopt.eq.3) then !read new data or simulate
			    inipage=-500
				!!!! readini=.false.
				 if(iopt.eq.2.or.iopt.eq.6.or.iopt.eq.8.or.iopt.eq.7.or.iopt.eq.3) then
				callid=-152
				goto 2
				else
					if(itogrec.eq.1) ioptm=4
				endif
			endif
			if(ioptm.eq.3.or.ioptm.eq.4) then
			    readmec=.false.
			    if(readini) then
			        nfix=0
			        ec501=0.
			        fixec50=.false.
			        nfixec50=0
			    endif
				if(ioptm.eq.3) then
					callid=1100 !35 !new
					newmec=.true.
					newcons=.true.
			        newmr=.true.
					imodsav=0
					imod0=0	
					inipage=0
			
					goto 2
			   else if(ioptm.eq.4) then
					inipage=0
					callid=1200 !36  ! old
			!	call help_modify(main,imodhelp)
					imodsav=0
					imod0=0	
				    goto 2
				endif
			else if(ioptm.eq.2) then
					call intconv(imodold,mtitle)
				
					i=1
					irc=i
				
					newmr=.false.
					newcons=.false.
				
					do k=1,25
							if(jopen2(k).eq.-1) then
								igraph2=k
								goto 337
							endif
						    enddo
					igraph2=igraph2+1
337					jgraph=igraph2
					jopen2(igraph2)=1
					
				OPEN(unit=15,file=qmec,status='UNKNOWN',access='DIRECT', form='BINARY', RECL=1)
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
			
			!	igraph2=1
				mod_create=-irecq
				call create_model(ixm,iym,igraph2,imodel,main,form1,form1_txtarray1,&
						graph1_2,GraphMainPanel1_2,graphics1_2,&
						nxmodel,nymodel,mtype,models,mod_create,irc,qmec,text7(i),&
						dxs,dys,ijmod,ka,kf,ic,indrat,pfilem,open7,append,ipbar_progress2,&
						indwin,ncdep,iformText,autosim,ksim,&
		initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
					imod0=imodel
					imodsav=imod0
					ixm=1
					irecm0=irecq
					indrat=irecm0
					indk=indrat
					ratcons(indrat)%imod=imod
					indrec(1)=indrat
					ijmod(irecq)=imodel
					call read_rates(main,form1,Form1_TxtArray1,irecm0,imod,qmec,nrows,indrat,&
					ratcons,models,ic,ipbar_progress2,igraph2,nmr,conc,iformText)
					npar=models(igraph2)%npar
					
			goto 1	
			endif		
		callid=-230
		goto 2
				
case(-230) ! after reading show main page with data and mechanism or for autosim 
	!welcome
		if(autosim.and.ksim.eq.0) then
			if(initmec.eq.1100) then
				callid=1100 !35 !new
				goto 2
			else if(initmec.eq.1200) then
				callid=1200 !35 !old
				goto 2
			endif

		endif
		call intconv(imodold,mtitle)
				
		i=1
		irc=i
					
		irrate=irecq ! initial
		if(autosim) then
			if(ksim.eq.-1) then
				irrate=irect ! true
			else

				ix1sav=ixr1	!keep random seeds at start of run
				iy1sav=iyr1
				iz1sav=izr1
				goto 1952
				itinfos=gmcreatetextentry(imainpanel,1,2,10,1,&
						'',&
						60, Gedit, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              			gmVpos=GTOP, gmExpand=GOFF)
				itinfoP=gmcreatetextentry(imainpanel,12,2,12,1,&
						'TO PAUSE FIT/SIMULATION: Press "P" or "p"',&
						100, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              			gmVpos=GTOP, gmExpand=GOFF)
				itinfoC=gmcreatetextentry(imainpanel,24,2,14,1,&
						'TO ABORT FIT/SIMULATION: Press "ESC" or "Q" or "q"',100, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              			gmVpos=GTOP, gmExpand=GOFF)
				
1952				 kAs=models(jgraph)%kA 	!save values for 'true' model. and go round again for fit model
				kBs=models(jgraph)%kB
			    k=models(jgraph)%n
				ks=models(jgraph)%n
				kcs=ks-kas-kbs
				npars=models(jgraph)%npar
				
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
				thtrue(m)=theta0(m)
				enddo
				do i=1,kAs
				dgammas(i)=dgamma(i)
				enddo
				npars=npar
				do m=1,npars
				irates(m)=irate(m)
				jrates(m)=jrate(m)
				i=irates(m)
				j=jrates(m)
				QTtrue(i,j)=qt(i,j)
				thtrue(m)=QTtrue(i,j)
				enddo
				ncdeps=ncdep
				nligs=nlig
				do i=1,ncdeps
				IXs(i)=IX(i)
				JXs(i)=JX(i)
				ILs(i)=IL(i)
				enddo
				ncons1=ncon
				do j=1,ncons1
				ICs(1,j)=IC(1,j)
				ICs(2,j)=IC(2,j)
				enddo
				do i=1,ks
				do j=1,ks
					IQs(i,j)=IQ(i,j)
				enddo
				enddo
				mtitle11=mtitles
				rtitle1=rtitle
				imod11=imod
	  
			endif
			callid=-205
			goto 2
		else 
			do i=1,60
						pathdat1(i:i)=' '
			enddo
			if(.not.curvonly.and..not.autosim) then
				do j=1,nset
					do i=1,nfileb(j)
							nfile(j)=nfileb(j)
							present=.false.
							INQUIRE (FILE=pfiles(i,j),EXIST=PRESENT)
							if(PRESENT) then
								ihandle=FILE$FIRST
								length = GETFILEINFOQQ(pfiles(i,j), info, ihandle)
								nLEN=info%length 
								
							endif
							nbl=len_trim(pfiles(i,j))
								ilp=0
								do k=1,nbl
									if(pfiles(i,j)(k:k).eq.'\') ilp=k
								enddo
								filepure(1:20)=pfiles(i,j)(ilp+1:nbl)
									pfileb(i,j)=filepure
							if(.not.present.or.nlen.lt.100) then
								
						     !   if(nset.eq.1) then
2233							CALL gmFileBROWSER(filepure,DDIR,DFILT,gmBrowseType=0)
								if(filepure.ne.'              ') then
									nb=len_trim(ddir)
									pfiles(i,j)=ddir(1:nb)//'\'//filepure
									INQUIRE (FILE=pfiles(i,j),EXIST=PRESENT)
									if(PRESENT) then
										ihandle=FILE$FIRST
										length = GETFILEINFOQQ(filepure, info, ihandle)
										nLEN=info%length 
									endif
									
									if(.not.present.or.nlen.lt.100) goto 2233
									pfileb(i,j)=filepure
								
								endif
							!	else
								
							!	endif
							endif
				!			call gmSetCellSetting(ini_TxtArray5, i,j ,gmString=pfileb(i,j))
				!			call gmdrawwidget(ini_TxtArray5)
						!	pfiles(i,j)=pfileb(i,j)
					enddo
				enddo
				
				inipage=-100		
				callid=3040
				goto 2	
			endif
			if(curvonly) then
			! check mec exist???
				call intconv(imodold,mtitle)
				
					i=1
					irc=i
				
					newmr=.false.
					newcons=.false.
				
					do k=1,25
							if(jopen2(k).eq.-1) then
								igraph2=k
								goto 339
							endif
						    enddo
					igraph2=igraph2+1
339					jgraph=igraph2
					jopen2(igraph2)=1
					
				OPEN(unit=15,file=qmec,status='UNKNOWN',access='DIRECT', form='BINARY', RECL=1)
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
			
			!	igraph2=1
				mod_create=-irecq
				call create_model(ixm,iym,igraph2,imodel,main,form1,form1_txtarray1,&
						graph1_2,GraphMainPanel1_2,graphics1_2,&
						nxmodel,nymodel,mtype,models,mod_create,irc,qmec,text7(i),&
						dxs,dys,ijmod,ka,kf,ic,indrat,pfilem,open7,append,ipbar_progress2,&
						indwin,ncdep,iformText,autosim,ksim,&
		initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
					imod0=imodel
					imodsav=imod0
					ixm=1
					irecm0=irecq
					indrat=irecm0
					indk=indrat
					ratcons(indrat)%imod=imod
					indrec(1)=indrat
					ijmod(irecq)=imodel
					call read_rates(main,form1,Form1_TxtArray1,irecm0,imod,qmec,nrows,indrat,&
					ratcons,models,ic,ipbar_progress2,igraph2,nmr,conc,iformText)
					npar=models(igraph2)%npar
					
			endif
		
		endif	
	
	case(-205)
		i=1
		irc=i
		
		if(autosim) then
			if(ksim.eq.0) then
				irrate=irecq
				inipage=0
			else
			irrate=irect
				inipage=0
			endif
		else
		    if(readini.and.ioptm.eq.3)then
		        callid=-206
		        goto 2
		    else
			irrate=irecq
			inipage=0
			endif
		endif
		mod_create=-irrate
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
334		jgraph=igraph2
		jopen2(igraph2)=1
		npar=20
						
		OPEN(unit=15,file=qmec,status='UNKNOWN',access='DIRECT', form='BINARY', RECL=1)
		read(unit=15,rec=1) iver5,nrecs,nextrec,ireclast,jstart
		do irq=1,nrecs
			irec=jstart(irq)	!previous nextrec
			read(unit=15,rec=irec) iver5,imod,mtitle11,k,kA,kB,kC,kD,rtitle
					
			ijmod(irq)=imod
			rtitles(irq)=rtitle
	   
		enddo
		
		callid=-206
		close(unit=15)
		goto 2	
case(-206)
		if(readini.and.ioptm.eq.3)goto 77
		if(.NOT.AUTOSIM.OR.(autosim.and.ksim.eq.0)) then
			irrate=irecq		
		endif
		
		OPEN(unit=15,file=qmec,status='UNKNOWN',access='DIRECT', form='BINARY', RECL=1)
		read(unit=15,rec=1) iver5,nrecs,nextrec,ireclast,jstart
	
		irec=jstart(irrate)
		read(unit=15,rec=irec) iver5,imod,mtitle11,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrateq,&
     	ncon,ncdep,nlig,chardef,boundef,ncyc,vref,nvdep,kmfast,&
     	indmod,npar
		close(unit=15)
	

77      continue	
		inion=0
		call ini_page(main,nset,npar,ixgrid,iygrid,nfileb,curvonly,&
		autosim,ksim,readini,initmec,nlig,initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1,ini_Value2_1,ini_TxtArray2,&
	ini_Value3_1,ini_TxtArray3,ini_Value4_1 ,ini_Value4_2,ini_Value5_1,ini_TxtArray5, &
	ini_Value6_1 ,ini_Value6_3, ini_Value6_7 ,ini_Value6_8,ini_Value6_9, ini_TxtArray7,&
	ini_Toggle1_1,ini_Toggle2_1,ini_Toggle3_1,ini_Toggle4_1,ini_Toggle4_2,ini_Toggle5_1,&
	ini_Toggle6_1,ini_Toggle6_2,ini_Toggle6_4,ini_Toggle6_5,ini_Toggle6_7,ini_Toggle6_8,&
	ini_Toggle7_1,ini_Toggle7_2,ini_Text7_1,ini_Value7_2,ini_Value7_3,ini_Value7_4,ini_TxtArray4_1,ini_TxtArray4_2 ,&
	inipage,isetu,ini_Button8_1,ini_Button8_2,ini_Button8_3,ini_Value6_0,ini_Button16_3,&
	ini_Button16_4,ini_text6_01,ini_panel2,iniyes,inino,ini_text6_02,ini_Value6_12,ini_Toggle6_10,&
	ini_Value6_10,ini_Toggle8,nfblock,ini_Value4_5,ini_TxtArray4_5,ini_Toggle1_3)

		if(autosim.and.ksim.eq.-1) then
			inisim=ini
		else
			inipage=-100
		endif
		call gmEnqWidgetInfo(initialpage,widget)
		if(readini.and.ioptm.eq.3)then
		    models(igraph2)%ix=-100
		    call draw_model(igraph2,imodel,main,graph1_2,GraphMainPanel1_2,graphics1_2,&
		    models,plot,ipos,mod_create,initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
		else
		imodel=imod
		
		dcmod=.false.
	!	if(dcmod) call gmsettoggleswitch(ini_Toggle1_3,gon)
		call create_model(ixm,iym,igraph2,imodel,main,form1,form1_txtarray1,&
			graph1_2,GraphMainPanel1_2,graphics1_2,&
			nxmodel,nymodel,mtype,models,mod_create,irc,qmec,text7(i),&
			dxs,dys,ijmod,ka,kf,ic,indrat,pfilem,open7,append,ipbar_progress2,&
			indwin,ncdep,iformText,autosim,ksim,&
		initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
		endif	
			nrmodel(igraph2)=imodel
            imod=imodel
		imod0=imodel
		if(.not.curvonly.and..not.autosim) then
				do j=1,nset
					do i=1,nfileb(j)
					call gmSetCellSetting(ini_TxtArray5, i,j ,gmString=pfileb(i,j))
					enddo
				enddo
					call gmdrawwidget(ini_TxtArray5)
	!	if(dcmod) call gmsettoggleswitch(ini_Toggle1_3,gon)
		endif
		do i=4,15
				call gmSetWidgetStatus(modelw(i), GSELECTABLE)
	
		enddo
		do j=1,2
				call gmSetWidgetStatus(eqfit(j), GSELECTABLE)

		enddo
		call intconv(imod,cnum51)
			! here generates an image file of the mechanism
		sfile='model'//cnum51(1:3)//'.wmf'
		nsk=len_trim(sfile)
		if (sfile(nsk-3:nsk).eq.'.wmf') then
				idev=1
	   			pwmf=.true.
	   				
	   			idpi=600
	   			ixoff=0
	   			iyoff=0
	   			iwidi=4800
	   			ihei=3600
				idpi=300
	   			ixoff=0
	   			iyoff=0
	   			iwidi=1800
	   			ihei=1800
				call devsus  
	   			call wmfp(idpi,ixoff,iyoff,iwidi,ihei)			
					!else if (sfile(ns-3:ns).eq.'.bmp') then
		else if(sfile(nsk-3:nsk).eq.'.jpg') then
				call devsus  
				idev=2
				call gjpeg
				call genqdrawinglimits(dim,idimtype)
				dim%xpap=0.7*dim%ypap
				dim%ypap=0.7*dim%ypap
				call gsetdrawinglimits(dim,idimtype)
		else
				call devsus  
	   			pbmp=.true.
	   			idev=2
	   			call bmp
				call genqdrawinglimits(dim,idimtype)
				dim%xpap=0.7*dim%ypap
				dim%ypap=0.7*dim%ypap
				call gsetdrawinglimits(dim,idimtype)		
	   	endif 
	
		ipos=0
		plot=.true.
		
		call gsetdevicefilename(sfile,0)
		call draw_model(igraph2,imodel,main,graph1_2,GraphMainPanel1_2,graphics1_2,&
			models,plot,ipos,mod_create,initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
		if(pwmf) then
						pwmf=.false.
					
		else if(pbmp) then
						pbmp=.false.
						
		endif
		CALL DEVEND
      	CALL guiwin	
		idev=0
		plot=.false.
		ixm=1
		if(readini.and.ioptm.eq.3) then
		if(readini) then
			        nfix=0
			        ec501=0.
			        fixec50=.false.
			        nfixec50=0
			    endif
		else
		irecm0=irrate
		indrat=irecm0
		indk=indrat
		ratcons(indrat)%imod=imod
		indrec(1)=indrat
					!ijmod(irecq)=imodel
		ijmod(irrate)=imodel
		call read_rates(-1,form1,Form1_TxtArray1,irecm0,imod,qmec,nrows,indrat,&
			ratcons,models,ic,ipbar_progress2,igraph2,nmr,conc,iformText)
			npar=models(igraph2)%npar
		endif
		call gmSetGuiGridMode(GOn)
		
		if(autosim.and.readini.and.ksim.ne.-1) then
		    initmec=0
		    if(itogrec.eq.8.or.itogrec.eq.1) then
		    call gmdrawwindow(initialpage)
		    else
			
			ilasts=ilast
			jlasts=jlast
			ix1sav=ixr1	!keep random seeds at start of run
			iy1sav=iyr1
			iz1sav=izr1
			if(discprt) write(7,143) nsims
143			format(/,' Simulation of experiment no:',i5)
			do i=1,nmax
				do j=1,nset
				tint0(i,j)=0.0
			    ampl0(i,j)=0.0
			    iprops0(i,j)=0
				enddo
			enddo
			
	itinfos=gmcreatetextentry(imainpanel,1,2,10,1,&
						'',&
						60, Gedit, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              			gmVpos=GTOP, gmExpand=GOFF)
				itinfoP=gmcreatetextentry(imainpanel,12,2,12,1,&
						'TO PAUSE FIT/SIMULATION: Press "P" or "p"',&
						100, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              			gmVpos=GTOP, gmExpand=GOFF)
				itinfoC=gmcreatetextentry(imainpanel,24,2,14,1,&
						'TO ABORT FIT/SIMULATION: Press "ESC" or "Q" or "q"',100, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              			gmVpos=GTOP, gmExpand=GOFF)
			call HJCSIM2(main,thtrue,tint0,ampl0,iprops0,conc,nintt,&
				irates,jrates,npars,nmax,nset,dgammas,nsims,ks,imainpanel,itinfos,nchan)
			do j=1,nset
				treso=0.001*tresol(j)
			!	tresol(j)=1.e-3*tresol(j)
				tresg=treso
				treso5=treso
				tresg5=tresg
				acrit5=acrit(j)
				avamp5=avamp(j)
				!sim=simulat
				sim=autosim.or.simulat
				call resolution2(tint0(1,j),tint(1,j),ampl0(1,j),ampl(1,j),&
     			iprops0(1,j),iprops(1,j),nint(j),nintt(j),&
     			ffilt(j),fc(j),rms(j),trise(j),nmax,nd1,index(1,j),&
     			cjump,nsweep,kjumps0,kjumps,autosim,nsims,&
     			sim,sres,sexp,excamp,alo(j),ahi(j),resdebug,iresnth)
		!!!!! write tint 
				tresol(j)=1.e3*treso	!keep (possibly altered) res in mus for .ini
				tresd(j)=dble(treso)*1.0d-3 	!same as tresg (but in seconds)
			enddo

			call gmdrawwindow(initialpage)
		    endif
		endif
			
		indk=indrat	
		if(inipage.eq.-600) then
			callid=-172
			inipage=-100
			goto 2
		else
			inipage=-100	
			callid=-23	! go anfd fill initial settings window
			goto 2
		endif
	
	case(-231)
		
			indk=indrat		
			callid=-23
			goto 2
	case(-232)
		inion=0
		nlig=models(jgraph)%nlig
		call ini_page(main,nset,npar,ixgrid,iygrid,nfileb,curvonly,autosim,ksim,&
		readini,initmec,nlig,initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1,ini_Value2_1,ini_TxtArray2,&
	ini_Value3_1,ini_TxtArray3,ini_Value4_1 ,ini_Value4_2,ini_Value5_1,ini_TxtArray5, &
	ini_Value6_1 ,ini_Value6_3, ini_Value6_7 ,ini_Value6_8,ini_Value6_9, ini_TxtArray7,&
	ini_Toggle1_1,ini_Toggle2_1,ini_Toggle3_1,ini_Toggle4_1,ini_Toggle4_2,ini_Toggle5_1,&
	ini_Toggle6_1,ini_Toggle6_2,ini_Toggle6_4,ini_Toggle6_5,ini_Toggle6_7,ini_Toggle6_8,&
	ini_Toggle7_1,ini_Toggle7_2,ini_Text7_1,ini_Value7_2,ini_Value7_3,ini_Value7_4,ini_TxtArray4_1,ini_TxtArray4_2 ,&
	inipage,isetu,ini_Button8_1,ini_Button8_2,ini_Button8_3,ini_Value6_0,ini_Button16_3,&
	ini_Button16_4,ini_text6_01,ini_panel2,iniyes,inino,ini_text6_02,ini_Value6_12,ini_Toggle6_10,&
	ini_Value6_10,ini_Toggle8,nfblock,ini_Value4_5,ini_TxtArray4_5,ini_Toggle1_3)
		ixmm=models(jgraph)%ix
		plot=.false.
		models(jgraph)%ix=-100
	call gmdrawwindow(initialpage)
	!if(dcmod) call gmsettoggleswitch(ini_Toggle1_3,gon)
	if(ksim.ne.-1) inipage=-100
	
	!!!!! new model here 
		call draw_model(igraph2,imodel,main,graph1_2,GraphMainPanel1_2,graphics1_2,&
		models,plot,ipos,mod_create,initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
		call graph2(ixm,iym,igraph2,imodel,main,graph1_2,GraphMainPanel1_2,graphics1_2,&
		nxmodel,nymodel,mtype,mtext1,mod_create,initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
		models(jgraph)%ix=ixmm
		call gmSetGuiGridMode(GOn)
		do i=4,15
			call gmSetWidgetStatus(modelw(i), GSELECTABLE)
	
		enddo
		do j=1,2
			call gmSetWidgetStatus(eqfit(j), GSELECTABLE)

		enddo
		callid=-23
		goto 2
	case(-23) ! fill inipage with properties,settings
				
				if(.not.curvonly.and..not.autosim) then
				do j=1,nset
					do i=1,nfileb(j)
					call gmSetCellSetting(ini_TxtArray5, i,j ,gmString=pfileb(i,j))
					enddo
				enddo
					call gmdrawwidget(ini_TxtArray5)
				endif
				indk=indrat
				irecm0=indrat
				nbl=len_trim(qmec)
					ilp=1
				do i=1,nbl
					if(qmec(i:i).eq.'\') ilp=i
				enddo
				if(ilp.eq.1) then
					iplen = FULLPATHQQ(qmec, pathdat2)
					nbl1=len_trim(pathdat2)
					ilp1=1
					do i=1,nbl1
						if(pathdat2(i:i).eq.'\') ilp1=i
					enddo
					pathmec=pathdat2(1:ilp1)
				    qfilem=pathdat2(1+ilp1:nbl1)
				else
					pathmec=qmec(1:ilp)
					text2=qmec(ilp+1:nbl)
					qfilem=' '
					qfilem=text2(1:40)
				endif

				nbl=len_trim(qfilem)
				if(ratcons(indk)%ncyc.eq.0) nmr=0
			!	filepure=qfilem(ilp+1:nbl)				
					call gmsettextsetting(ini_Text1_1,qfilem(1:nbl)//':'//models(igraph2)%title_model)
					!	call gmSetWidgetStatus(ini_text1_1, GunSELECTABLE)
					if(ksim.ne.-1) then
					call gmsetvaluesetting(ini_Value2_1,nmr)
					!	call gmSetWidgetStatus(ini_value2_1, GunSELECTABLE)
					call gmsetvaluesetting(ini_Value3_1,neq)
					!	call gmSetWidgetStatus(ini_value3_1, GunSELECTABLE)
					call gmsetvaluesetting(ini_Value4_1,nfix)
					!	call gmSetWidgetStatus(ini_value4_1, GunSELECTABLE)
					call gmsetvaluesetting(ini_Value4_2,nfixec50)
					!	call gmSetWidgetStatus(ini_value4_2, GunSELECTABLE)
				endif
					call gmsettextsetting(ini_Text7_1,ratcons(indk)%title)
				!	call gmSetWidgetStatus(ini_text7_1, GunSELECTABLE)
!					penfac=10.0d0 don't overwrite the set value
					xs=sngl(assmax)
					fac=sngl(penfac)
					xs1=sngl(ratemax)
					
					if(ksim.ne.-1) then
					call gmsetvaluesetting(ini_Value7_3,xs)
					call gmsetvaluesetting(ini_Value7_4,xs1)
					endif
		
					imr=0
					ico=0
					ifi=0
					if(ksim.ne.-1) then
					do jfi=1,20
					call gmSetCellSetting(ini_TxtArray4_1, 2,jfi ,gmString='')
					call gmSetCellSetting(ini_TxtArray4_1, 1,jfi,gmString='')
					enddo
					endif
					jfi=0
					ifi=0
					iffi=0
				    ka2=2*models(igraph2)%ka
					do j=1,models(igraph2)%npar
						call gmSetCellSetting(ini_TxtArray7, 1,j,gmString=ratcons(indk)%qij(j))
					
						call gmSetCellSetting(ini_TxtArray7, 2,j,gmString=ratcons(indk)%titlep(j))
						titlep(j)=ratcons(indk)%titlep(j)
						xvreal=ratcons(indk)%value(j)
						call gmSetCellSetting(ini_TxtArray7, 3,j,gmvalue=xvreal)
					
						if(nfix.gt.0) then
						if(jfix(j).eq.1) then
							ifi=ifi+1
							if(ksim.ne.-1) then
							call gmSetCellSetting(ini_TxtArray4_1, 2,ifi ,gmString=ratcons(indk)%titlep(j))
							call gmSetCellSetting(ini_TxtArray4_1, 1,ifi,gmString=ratcons(indk)%qij(j))
						endif
						endif
						if(nfblock.gt.0)then
						    do ip=1,nfblock
						    
						    if(abs(mfb(ip)).eq.j) then
						    iffi=iffi+1
						    call gmSetCellSetting(ini_TxtArray4_5, 2,iffi ,gmString=ratcons(indk)%titlep(j))
							call gmSetCellSetting(ini_TxtArray4_5, 1,iffi,gmString=ratcons(indk)%qij(j))
							endif
							enddo
							
						endif
					endif
					enddo
					if(nfblock.gt.0) then 
					    call gmsetvaluesetting(ini_Value4_5,nfblock)
					    call gmdrawwidget(ini_TxtArray4_5)
				    endif
				!	nmr=0
				   	if(ksim.ne.-1) then
					do ico1=1,20
						call gmSetCellSetting(ini_TxtArray3, 1,ico1,gmString=' ')
						call gmSetCellSetting(ini_TxtArray3, 2,ico1 ,gmString=' ')
						call gmSetCellSetting(ini_TxtArray3, 3,ico1 ,gmstring=' ')
						call gmSetCellSetting(ini_TxtArray3, 4,ico1,gmString=' ')
						call gmSetCellSetting(ini_TxtArray3, 5,ico1 ,gmString=' ')
					enddo
					endif
					ico=0
					do m=1,neq
					do j=1,models(igraph2)%npar
				
				
						if(irate(j).eq.ie(m).and.jrate(j).eq.je(m)) then
						ico=ico+1
							if(ksim.ne.-1) then
						
						
						call gmSetCellSetting(ini_TxtArray3, 1,ico,gmString=ratcons(indk)%qij(j))
						call gmSetCellSetting(ini_TxtArray3, 2,ico ,gmString=ratcons(indk)%titlep(j))
						call gmSetCellSetting(ini_TxtArray3, 3,ico ,gmvalue=efac(ico))
							endif
					!	ratcons(indk)%efac(j)=efac(ico)
						do l=1,models(igraph2)%npar
							if(irate(l).eq.if(ico).and.jrate(l).eq.jf(ico)) then
								if(ksim.ne.-1) then
							call gmSetCellSetting(ini_TxtArray3, 4,ico,gmString=ratcons(indk)%qij(l))
							call gmSetCellSetting(ini_TxtArray3, 5,ico ,gmString=ratcons(indk)%titlep(l))
							endif
							if(efac(ico).gt.0.) then
       						if(discprt) write(7,2222) j,ratcons(indk)%qij(j),ratcons(indk)%titlep(j),&
							efac(ico),l,ratcons(indk)%qij(l),ratcons(indk)%titlep(l)
2222	 						format(' rate ',i3,3x,a10,1x,a10,2x,'is constrained to be',&
							/,g13.6,' times rate ',i3,3x,a10,1x,a10)
							else
       						if(discprt) write(7,2223) j,ratcons(indk)%qij(j),ratcons(indk)%titlep(j),&
							-efac(ico),l,ratcons(indk)%qij(l),ratcons(indk)%titlep(l)
2223	 						format(' rate ',&
							i3,3x,a10,1x,a10,2x,'is constrained to be',&
							/,g13.6,' minus rate ',i3,3x,a10,1x,a10)
							!	ratcons(indk)%titlec(j)=ratcons(indk)%titlep(l)
							ENDIF
							endif
						enddo
					
						
						endif
					enddo
					
					enddo
				    
					if(readmec) then
					if(readini) then
						ncyc=ncyc2 ! ncyc current= ncyc2 from ini
						ncyc1=ratcons(indk)%ncyc ! ncyc1 from mechanism
						if((ncyc1.ne.ncyc2).and.(ioptm.eq.4.or.ioptm.eq.3)) then
						    ncyc=ncyc1
						    do L=1,ncyc
						 nsc(L)=ratcons(indk)%nsc(L)
						 nsc1(L)=ratcons(indk)%nsc(L)
						 nsc2(L)=nsc(L)
							do m=1,nsc(L)
							   
								im(L,m)=ratcons(indk)%im(L,m)
								jm(L,m)=ratcons(indk)%jm(L,m)
								im1(L,m)=im(L,m)
								jm1(L,m)=jm(L,m)
								im2(L,m)=im(L,m)
								jm2(L,m)=jm(L,m)
							enddo
							
							
						enddo
						else
						do L=1,ncyc
						obeymr(l)=.true.
					    nsc(L)=nsc2(L)
						nsc1(L)=ratcons(indk)%nsc(L)
						do m=1,nsc(L)
							im(L,m)=im2(L,m)
							jm(L,m)=jm2(L,m)
							im1(L,m)=ratcons(indk)%im(L,m)
							jm1(L,m)=ratcons(indk)%jm(L,m)
						enddo
					
						enddo
						endif
					else
						if(ncyc2.gt.0) then
						ncyc=ncyc2 ! ncyc current= ncyc2 from ini
						ncyc1=ratcons(indk)%ncyc ! ncyc1 from mechanism
						do L=1,ncyc
						obeymr(l)=.true.
					    nsc(L)=nsc2(L)
						nsc1(L)=ratcons(indk)%nsc(L)
						do m=1,nsc(L)
							im(L,m)=im2(L,m)
							jm(L,m)=jm2(L,m)
							im1(L,m)=ratcons(indk)%im(L,m)
							jm1(L,m)=ratcons(indk)%jm(L,m)
						enddo
					
						enddo
						else
						ncyc=ratcons(indk)%ncyc
						do L=1,ncyc
						 nsc(L)=ratcons(indk)%nsc(L)
						 nsc1(L)=ratcons(indk)%nsc(L)
						 nsc2(L)=nsc(L)
							do m=1,nsc(L)
							   
								im(L,m)=ratcons(indk)%im(L,m)
								jm(L,m)=ratcons(indk)%jm(L,m)
								im1(L,m)=im(L,m)
								jm1(L,m)=jm(L,m)
								im2(L,m)=im(L,m)
								jm2(L,m)=jm(L,m)
							enddo
							
							
						enddo
						ncyc2=ncyc
						ncyc1=ncyc
						endif	
					endif
					ligname(1)=models(jgraph)%ligname(1)
					ligname(2)=models(jgraph)%ligname(2)
					if(ncyc.eq.0) then ! verify
						ncyc=ratcons(indk)%ncyc
						do L=1,ncyc
							nsc(L)=ratcons(indk)%nsc(L)
							nsc2(L)=nsc(L)
							nsc1(L)=nsc(L)
							do m=1,nsc(L)
							    im1(L,m)=ratcons(indk)%im(L,m)
								jm1(L,m)=ratcons(indk)%jm(L,m)
								im(L,m)=ratcons(indk)%im(L,m)
								jm(L,m)=ratcons(indk)%jm(L,m)
								im2(L,m)=im(L,m)
								jm2(L,m)=jm(L,m)
							enddo
							
							
						enddo
						ncyc2=ncyc
						ncyc1=ncyc	
					endif
					endif
				    nmr=0
					if(ncyc.gt.0) then
					if(ksim.eq.-1) then
						call gmSetGuiGridMode(GOn)
						imrtext=gmcreatetextentry(ini_panel2,1,1, 10, 1,&
						'Check if the following cycles obey MR :',32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmhpos=gleft,gmVpos=GTOP, gmExpand=GOFF)
					endif
					do l=1,models(igraph2)%npar
					if(ratcons(indk)%micro(l).eq.'MR') ratcons(indk)%micro(l)='  '
					jmic(l)=0
					do j=1,ncyc
					isetmr(j)=j
					if(obeymr(j)) then
					call intconv(j,cnum5)
					if(irate(l).eq.im(j,1).and.jrate(l).eq.jm(j,1)) then
						imr=imr+1
						if(ksim.ne.-1) call gmSetCellSetting(ini_TxtArray2, 1,imr,gmString=cnum5)
					
						if(ksim.ne.-1) call gmSetCellSetting(ini_TxtArray2, 2,imr,gmString=ratcons(indk)%qij(l))
						ratcons(indk)%micro(l)='MR'
						jmic(l)=1
							if(ksim.ne.-1) call gmSetCellSetting(ini_TxtArray2, 3,imr,gmString=ratcons(indk)%titlep(l))
						if(discprt) write(7,2225) l,ratcons(indk)%qij(l),ratcons(indk)%titlep(l)
2225 						format(' rate ',i3,3x,a10,1x,a10,2x,'calculated by MR')
						call intconv(im(j,1),cnum5)
					
						nt5=len_trim(cnum5)
						textbox=cnum5(1:nt5)
						prod=ratcons(indk)%qt(im(j,1),jm(j,1))
						prod1=ratcons(indk)%qt(jm(j,1),im(j,1))
						do jo=2,nsc(j)
				
						call intconv(im(j,jo),cnum51)
						nt=len_trim(textbox)
						textbox=textbox(1:nt)//','//cnum51
						prod=prod*ratcons(indk)%qt(im(j,jo),jm(j,jo))
							prod1=prod1*ratcons(indk)%qt(jm(j,jo),im(j,jo))
						enddo
					
						if(ksim.eq.-1) then
							nk=len_trim(textbox)
					    if(prod.eq.prod1) then
						
							textbox=textbox(1:nk)//': obeys MR'
						else
							textbox=textbox(1:nk)//': does not obey MR'
						endif
						call intconv(j,cnum(1))
						textmr='Cycle no:'//cnum(1)(1:3)//'='//textbox
						imrtext=gmcreatetextentry(ini_panel2,1,j+1, 10, 1,&
						textmr,32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmhpos=gleft,gmVpos=GTOP, gmExpand=GOFF)
							!	call gmdrawwidget(imrtext)
						else
							call gmSetCellSetting(ini_TxtArray2, 4,imr,gmstring=textbox)
						endif	
						obeymr(j)=.true.

					    nmr=nmr+1
					endif
					else
						call gmSetCellSetting(ini_TxtArray2, 1,j,gmstring='')
						call gmSetCellSetting(ini_TxtArray2, 2,j,gmstring='')
						call gmSetCellSetting(ini_TxtArray2, 3,j,gmstring='')
							call gmSetCellSetting(ini_TxtArray2, 4,j,gmstring='')
					endif
					enddo
					
					enddo
					else
					if(ksim.eq.-1) then
						call gmSetGuiGridMode(GOn)
						textmr='This model has no cycles '
						imrtext=gmcreatetextentry(ini_panel2,1,1, 10, 1,&
						textmr,32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmhpos=gleft,gmVpos=GTOP, gmExpand=GOFF)
					endif
					endif
					do iec=1,20
						if(ksim.ne.-1) then
						call gmSetCellSetting(ini_TxtArray4_2, 1,iec,gmString=' ')
						call gmSetCellSetting(ini_TxtArray4_2, 2,iec ,gmString=' ')
						endif
					enddo
					ieci=0	
					if(nfixec50.gt.0)then
						do l=1,models(igraph2)%npar
						if(irate(l).eq.i50.and.jrate(l).eq.j50) then
							ieci=ieci+1
							if(ksim.ne.-1) then
							call gmSetCellSetting(ini_TxtArray4_2, 1,ieci,gmString=ratcons(indk)%qij(l))
							call gmSetCellSetting(ini_TxtArray4_2, 2,ieci ,gmString=ratcons(indk)%titlep(l))
							endif
						endif
						enddo
						do ijfix=1,nfixec50
		                if(ijfix.eq.1) then
		                    n=nlvar
		                    i=i50
		                    j=j50
		                    m=m50
		                    i501=i50
		                    j501=j50
		                    ec=ec501
		                   ! ec50=ec501
		                    m501=m50
		                    ec5011=ec501
		                else
		                    n=nlvar2
		                    i=i502
		                    j=j502
		                    m=m502
		                    ec=ec502
		                endif
	                    if(discprt) write(7,26) m,i,j,titlep(m),ligname(n),ec*1.d6,penfac
26		                format(/,' Rate ',i3,3x,' q(',i2,',',i2,') =',1x,a10,' is constrained to give',/,&
                        '  EC50 for ',a10,' = ',g16.6,' micromolar,',/,&
                        ' using penalty factor = ',g13.6,' if needed')
		                if(nlig.gt.1) then
		                if(discprt) write(7,261)
261		                format('    Concentration of other ligands for EC50 determination')
		                do i=1,nlig
			            if(i.ne.n) then
			            if(ijfix.eq.1) then
				            x=1.e6*conc_ec1(i)
			            else if(ijfix.eq.2) then
				            x=1.e6*conc_ec2(i)
			            endif
			            if(discprt) write(7,325) ligname(i)(1:10),x
325			            format('   Conc of ',a10,' at which EC50 was determined (micromolar) = ',f9.4)
			            endif
		                enddo
		                endif
	                    enddo		!ifix=1,nfixec50
	
					endif	
						call gmdrawwidget(ini_TxtArray7)
						if(ksim.ne.-1) then
							call gmdrawwidget(ini_TxtArray3)
							call gmdrawwidget(ini_TxtArray2)
				
							call gmdrawwidget(ini_TxtArray4_1)
							call gmdrawwidget(ini_TxtArray4_2)
						endif
				!	call gFlushGraphics()
					if(ksim.ne.-1) then
					call gmsetvaluesetting(ini_Value2_1,nmr)
					!	call gmSetWidgetStatus(ini_value2_1, GunSELECTABLE)
					call gmsetvaluesetting(ini_Value3_1,neq)
					!	call gmSetWidgetStatus(ini_value3_1, GunSELECTABLE)
					call gmsetvaluesetting(ini_Value4_1,nfix)
					!	call gmSetWidgetStatus(ini_value4_1, GunSELECTABLE)
					call gmsetvaluesetting(ini_Value4_2,nfixec50)
					!	call gmSetWidgetStatus(ini_value4_2, GunSELECTABLE)
				endif
					call gmSetGuiGridMode(GOn)
					if(curvonly) then
					call gmSetWidgetStatus(ini_Button8_2, GSELECTABLE)
					else
					if(ksim.ne.-1) then
					call gmSetWidgetStatus(ini_Button8_1, GSELECTABLE)
					call gmSetWidgetStatus(ini_Button8_2, GSELECTABLE)
					call gmsetvaluesetting(ini_Value5_1,nset)
					endif
					endif
				
				
				call gmSetWidgetStatus(view_record, GSELECTABLE)
				if(autosim) then
				if(ksim.eq.-1) then 
					if(allocated(thtrue)) deallocate(thtrue)
						allocate(thtrue(200))
						do i=1,200
							thtrue(i)=0.d0
						enddo
						if(allocated(qttrue)) deallocate(qttrue,qtsav)
						allocate(QTtrue(100,100))
						allocate(QTsav(100,100))	!to check for changes
						do i=1,k
						do j=1,k
							QTtrue(i,j)=0.d0
							QTsav(i,j)=0.d0
						enddo
						enddo
						callid=1200 
						kAs=models(jgraph)%kA 	!save values for 'true' model. and go round again for fit model
						kBs=models(jgraph)%kB
				!	kCs=kC
				!	kDs=kD
						ks=models(jgraph)%n
						npar=models(jgraph)%npar
	   					do m=1,models(jgraph)%npar
						i=irate(m)
						j=jrate(m)
						qt(i,j) = ratcons(indrat)%qt(i,j)
				i1=i1+1
				iQ(i,j)=i1
				i1=i1+1
				IQ(j,i)=i1
				theta0(m)=qt(i,j)
				thsav(m)=theta0(m)
				thtrue(m)=theta0(m)
	    enddo
	   do i=1,kAs
		dgammas(i)=dgamma(i)
	   enddo
	   npars=npar
	   do m=1,npars
		irates(m)=irate(m)
		jrates(m)=jrate(m)
		i=irates(m)
		j=jrates(m)
		QTtrue(i,j)=qt(i,j)
		thtrue(m)=QTtrue(i,j)
	   enddo
	   ncdeps=ncdep
	   nligs=nlig
	   do i=1,ncdeps
		IXs(i)=IX(i)
		JXs(i)=JX(i)
		ILs(i)=IL(i)
	   enddo
	   ncons1=models(jgraph)%ncon
	   do j=1,ncons1
		ICs(1,j)=IC(1,j)
		ICs(2,j)=IC(2,j)
	   enddo
	   do i=1,ks
		do j=1,ks
		   IQs(i,j)=IQ(i,j)
		enddo
	   enddo
	   mtitle11=mtitles
	   rtitle1=rtitle
	   ilasts=models(jgraph)%ilast
	   jlasts=models(jgraph)%jlast
					if(readini) then
					
						!	ksim=0
							 imods=imod0
						endif
				!	call gmSetGuiGridMode(GOn)
					goto 1
				else
					do j=1,nset
									
					call gmSetCellSetting(ini_TxtArray5, 1,j ,gminteger=nintt(j))
					enddo
					call gmdrawwidget(ini_TxtArray5)
				!	callid=3001
					
				endif
				endif

				callid=-24 ! go and fill data 
				goto 2

case(-37)	! de la true mechanism ini_page
	if(ksim.eq.-1) ksim=0
	call gmremovewindow(initialpage)
	
		itxtinfo=gmcreatetextentry(infopanel,1,4,5,1,&
			'True Mechanism ',32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						 infoind=infoind+1
		

			itxtinfo=gmcreatetextentry(infopanel,6,4,8,1,&
			'File:'//qfilem(1:40),32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						 infoind=infoind+1


			itxtinfo=gmcreatetextentry(infopanel,14,4,14,1,&
			'Name:'//models(igraph2)%title_model,32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						 infoind=infoind+1
		
			itxtinfo=gmcreatetextentry(infopanel,28,4,12,1,&
			'Rate title:'//ratcons(indk)%title,32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						callid=3001
					goto 2
			


	case(-24) ! fill ini page
				if(.not.curvonly.and..not.autosim) then
				do j=1,nset
					do i=1,nfileb(j)
					call gmSetCellSetting(ini_TxtArray5, i,j ,gmString=pfileb(i,j))
					enddo
				enddo
				endif
				k=models(igraph2)%n
				if(curvonly) then
					j=1
					nset=1
					if(nlig.gt.0) then
					ligname(1)=models(jgraph)%ligname(1)
					call gmsettextsetting(ini_Text6_01,ligname(1))
					call gmsetvaluesetting(ini_Value6_9,tresol(j))
					call gmsetvaluesetting(ini_Value6_1,conc(1,j)*1e6)
					if(nlig.eq.2) then
						ligname(2)=models(jgraph)%ligname(2)
						call gmsettextsetting(ini_Text6_02,ligname(2))
						call gmsetvaluesetting(ini_Value6_12,conc(2,j)*1e6)
					endif
					endif
				    
				
					initi=1
					if(ns.eq.0) ns=1
					callid=0
					icall=0
				else
					
					do i=1,7
						
						infx=(i-1)*4
						info_pan(i)=gmCreatePanel(imainpanel, infx, 1, 4, 1,gmtitle=text_tog(i), &
              			gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmborderType=GINVERSECHISEL, &
              			gmLineCol=0, gmLineBorder=0, gmFillCol=221, gmFillBorder=0)
						ict=4
					!	if(i.eq.2.or.i.eq.4.or.i.eq.5.or.i.eq.7) then
						itextinfo=gmCreateTextEntry(info_Pan(i), 0, 0, 4, 1,text_tog(i), 255, GDISPLAY, gmBack1Col=0, &
						gmBack2Col=12, gmTextCol=1 , gmVpos=GTOP,gmsize=80)
					
				    enddo
					if(ns.lt.1) ns=1
					j=ns
					if(j.lt.1) ns=1
					if(onechan) then 
						call gmSetToggleSwitch(ini_Toggle6_2,Gon)
					else
						call gmSetToggleSwitch(ini_Toggle6_2,Goff)
					endif
					if(chsvec(j)) then
						call gmSetToggleSwitch(ini_Toggle6_4,Gon)
				    else
						call gmSetToggleSwitch(ini_Toggle6_4,Goff)
					endif
					if(badend(j)) then
						call gmSetToggleSwitch(ini_Toggle6_5,Gon)
					else
						call gmSetToggleSwitch(ini_Toggle6_5,Goff)
					endif
					if(setbad(1,j).eq..true.) then
						call gmSetToggleSwitch(ini_Toggle6_7,Gon)
					else
						call gmSetToggleSwitch(ini_Toggle6_7,Goff)
					endif
					if(setbad(2,j).eq..true.) then
					 call gmSetToggleSwitch(ini_Toggle6_8,Gon) 
					else
						call gmSetToggleSwitch(ini_Toggle6_8,Goff)
					endif
					!	call gmSetWidgetStatus(ini_value5_1, GunSELECTABLE)
					
				
					call gmsetvaluesetting(ini_Value6_7,tcbad(1,j))
					!	call gmSetWidgetStatus(ini_value6_7, GunSELECTABLE)
					call gmsetvaluesetting(ini_Value6_8,tcbad(2,j))
			
					call gmsetvaluesetting(ini_Value6_3,tcrit(j))
					!	call gmSetWidgetStatus(ini_value6_3, GunSELECTABLE)
					if(nlig.gt.0) then
					ligname(1)=models(jgraph)%ligname(1)
					call gmsettextsetting(ini_Text6_01,ligname(1))
					
					call gmsetvaluesetting(ini_Value6_1,conc(1,j)*1e6)
					if(nlig.eq.2) then
						ligname(2)=models(jgraph)%ligname(2)
						call gmsettextsetting(ini_Text6_02,ligname(2))
						call gmsetvaluesetting(ini_Value6_12,conc(2,j)*1e6)
					endif
					call gmsetvaluesetting(ini_Value6_10,pomax)
					endif
					if(fixpmax) then
					    
					    call gmSetToggleSwitch(ini_Toggle6_10,Gon)
					else
					    
					    call gmSetToggleSwitch(ini_Toggle6_10,Goff)
				    endif
				    
				call gmsetvaluesetting(ini_Value6_9,tresol(j))
				
				initi=1
				if(ns.eq.0) ns=1
				callid=0
				icall=0
				
				endif
case(-323)
				
				do i=1,7
						
						infx=(i-1)*4
						info_pan(i)=gmCreatePanel(imainpanel, infx, 1, 4, 1,gmtitle=text_tog(i), &
              			gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmborderType=GINVERSECHISEL, &
              			gmLineCol=0, gmLineBorder=0, gmFillCol=221, gmFillBorder=0)
						ict=4
					!	if(i.eq.2.or.i.eq.4.or.i.eq.5.or.i.eq.7) then
						itextinfo=gmCreateTextEntry(info_Pan(i), 0, 0, 4, 1,text_tog(i), 255, GDISPLAY, gmBack1Col=0, &
						gmBack2Col=12, gmTextCol=1 , gmVpos=GTOP,gmsize=80)
					
				    enddo
					if(ns.lt.1) ns=1
					j=ns
					if(j.lt.1) ns=1
					if(onechan) then 
						call gmSetToggleSwitch(ini_Toggle6_2,Gon)
					else
						call gmSetToggleSwitch(ini_Toggle6_2,Goff)
					endif
					if(chsvec(j)) then
						call gmSetToggleSwitch(ini_Toggle6_4,Gon)
				    else
						call gmSetToggleSwitch(ini_Toggle6_4,Goff)
					endif
					if(badend(j)) then
						call gmSetToggleSwitch(ini_Toggle6_5,Gon)
					else
						call gmSetToggleSwitch(ini_Toggle6_5,Goff)
					endif
					if(setbad(1,j).eq..true.) then
						call gmSetToggleSwitch(ini_Toggle6_7,Gon)
					else
						call gmSetToggleSwitch(ini_Toggle6_7,Goff)
					endif
					if(setbad(2,j).eq..true.) then
					 call gmSetToggleSwitch(ini_Toggle6_8,Gon) 
					else
						call gmSetToggleSwitch(ini_Toggle6_8,Goff)
					endif
					!	call gmSetWidgetStatus(ini_value5_1, GunSELECTABLE)
					
				
					call gmsetvaluesetting(ini_Value6_7,tcbad(1,j))
					!	call gmSetWidgetStatus(ini_value6_7, GunSELECTABLE)
					call gmsetvaluesetting(ini_Value6_8,tcbad(2,j))
			
					call gmsetvaluesetting(ini_Value6_3,tcrit(j))
					!	call gmSetWidgetStatus(ini_value6_3, GunSELECTABLE)
			
				if(nlig.gt.0) then
					ligname(1)=models(jgraph)%ligname(1)
					call gmsettextsetting(ini_Text6_01,ligname(1))
					call gmsetvaluesetting(ini_Value6_9,tresol(j))
					call gmsetvaluesetting(ini_Value6_1,conc(1,j)*1e6)
					if(nlig.eq.2) then
						ligname(2)=models(jgraph)%ligname(2)
						call gmsettextsetting(ini_Text6_02,ligname(2))
						call gmsetvaluesetting(ini_Value6_12,conc(2,j)*1e6)
					endif
				endif
				
				
				initi=1
				if(ns.eq.0) ns=1
				callid=0
				icall=0
			!	goto 1
		case(-25)
		    init=1
		    if(itogrec.eq.2) init=2
		
			do i=init,2
				call gmSetWidgetStatus(initwin_Toggle_1(i), GSELECTABLE)
			enddo
			call gmSetWidgetStatus(initwin_Toggle_1(4), GSELECTABLE)
		
            nofit=.false.
			nodata=.false.
			curvonly=.false.
		
	   
		case(-26)
		!	call gmDefineKeyselectCallback(13,0)
			call gmSetWidgetStatus(initwin_Toggle9, GSELECTABLE)
			call gmSetWidgetStatus(initwin_Toggle10, GSELECTABLE)
	        
		case(-27)
			 init=1
		    if(itogrec.eq.2) init=2
		
			do i=init,7
				call gmSetWidgetStatus(initwin_Toggle_1(i), GunSELECTABLE)
			enddo
			call gmSetWidgetStatus(initwin_Toggle_1(4), GunSELECTABLE)
			nofit=.true.
			nodata=.true.
			curvonly=.true.
			
		case(-29)
		
			call gmRemoveWindow(initialw)
			call gmDefineKeyselectCallback(13,0)
			readini=.false.
			readmec=.false.
			useprim0=.false.
			useprim=.false.
			useini=.false.
			imodold=0
			imodsav=imodold		!in case imodold changed in getqd
			imod0=imodsav
			
	        text1=' '
	!		call gmDefineKeyselectCallback(13,-21) !-10)
			ibib=-1
			callid=-291
			goto 2
			
		case(-291)  ! come from -45
			call gmDefineKeyCallback(13,0)
			if(autosim.and.itogrec.eq.8) then ! demo
				istatus1=gon
				istatus4=gon
				goto 689
				itinfos=gmcreatetextentry(imainpanel,1,2,10,1,&
						'',&
						60, Gedit, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              			gmVpos=GTOP, gmExpand=GOFF)
				    itinfoP=gmcreatetextentry(imainpanel,12,2,12,1,&
						'TO PAUSE FIT/SIMULATION: Press "P" or "p"',&
						100, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              			gmVpos=GTOP, gmExpand=GOFF)
				    itinfoC=gmcreatetextentry(imainpanel,24,2,14,1,&
						'TO ABORT FIT/SIMULATION: Press "ESC" or "Q" or "q"',100, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              			gmVpos=GTOP, gmExpand=GOFF)
				goto 689
			endif
			if(ibib.ne.-1) then
		
				istatus1=gmEnqToggleSwitch(imecform1_Toggle1)
				istatus2=gmEnqToggleSwitch(imecform1_Toggle2)
			
				istatus3=gmEnqToggleSwitch(imecform1_Toggle3)
				istatus4=gmEnqToggleSwitch(imecform1_Toggle4)
				istatus5=gmEnqToggleSwitch(imecform1_Toggle5)
			
				istatus6=gmEnqToggleSwitch(imecform1_Toggle6)
				call gmRemoveWindow(imecform1)

				call gmDefineKeyCallback(13,0)
				if(istatus1.eq.gon) samec=.true.
			endif
689				continue
				call gmSetWidgetStatus(new_file, gSELECTABLE)
				call gmSetWidgetStatus(open_file, gSELECTABLE)
			!	call gmSetWidgetStatus(import_file, gSELECTABLE)
				do j=4,15
				call gmSetWidgetStatus(modelw(j), GSELECTABLE)
				enddo
		
			if(autosim) then
			
				irect0=irect
				irecq0=irecq
				irqsav=irecq
				if(istatus4.eq.gon) then ! same mec
						initmec=0
						ioptm=1
				else if(istatus5.eq.gon) then
				ioptm=4
						initmec=1200
				else if(istatus6.eq.gon) then
						initmec=1100
						ioptm=3
				endif
				if(istatus1.eq.gon) then
				   	
					ksim=-1
					callid=-230
					irrate=irect
					goto 2
				else if(istatus2.eq.gon) then
				    ksim=-1
					callid=1200 !35 !new
!					call help_modify(main,imodhelp)
					goto 2
				else if(istatus3.eq.gon) then
					ksim=-1
					callid=1100 !35 !new
					goto 2
				endif
			else
			if(istatus3.eq.gon.or.itogrec.eq.3) then
					callid=1100 !35 !new
					
					imodsav=0
					imod0=0	
					inipage=0
				!	call prog_bar(main,ipbar,icho,ipbar_Progress2)
				goto 2
			else if(istatus2.eq.gon.or.itogrec.eq.4) then
					inipage=0
					callid=1200 !36  ! old
!				call help_modify(main,imodhelp)
					imodsav=0
					imod0=0	
				!	call prog_bar(main,ipbar,icho,ipbar_Progress2)
				goto 2
            else if(istatus1.eq.gon) then !same 
				imodel=imodold
			
				
				callid=1001
				goto 2
		
				!		if(istatus1.eq.gon) samec=.true.
			endif
			endif
   case(-99) !obsolete
		call gmSetToggleSwitch(ini_Toggle1_1, Goff)
		call gmSetToggleSwitch(ini_Toggle2_1, Goff)
		call gmSetToggleSwitch(ini_Toggle3_1, Goff)
		call gmSetToggleSwitch(ini_Toggle4_1, Goff)
		call gmSetToggleSwitch(ini_Toggle4_2, Goff)
		call gmSetToggleSwitch(ini_Toggle5_1, Goff)
		call gmSetToggleSwitch(ini_Toggle6_1, Goff)
		call gmSetToggleSwitch(ini_Toggle7_1, Goff)
		call gmSetToggleSwitch(ini_Toggle7_2, Goff)
	case(-100) ! obsolete
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
		! return from mechanism -after reading models
		
		icallidp=callid
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
	!	textyn(1)='Save changes (if any) to mechanism file?'
	!	 n=1
	!	 iyesno(1,1)=0
	!	 iyesno(1,2)=1
	!	 call gmDefineKeySelectCallback(13,-105)


	!	  call yesno_table(main,iynwin,n,textyn,iyes,iyn,iyesno,-105)
	callid=-106
	goto 2

case(-105) ! save changes to mec	
		call gmDefineKeySelectCallback(13,0)
	    istat1=gmenqtoggleswitch(iyes(1))
				if(istat1.eq.gon) then

	
	!	imessy=gmDisplayMessageBox('','Save changes (if any) to mechanism file?',Gquestion,gyesno)
	!	if(imessy.eq.gyesbutton) then
				efile=qfilem

				efilt='*.mec'//char(124)//'Mechs.mec'//char(124)//&
				'*.dat'//char(124)//'Old files'//char(124)//&
			   '*.*'//char(124)//'All Files'
				CALL gmFileBROWSER(eFILE,eDIR,eFILT,gmBrowseType=1)
		
				IF(eFILE.ne.' ') then
						nb=len_trim(edir)
					efile=edir(1:nb)//'\'//efile
					call write_model(main,imod,models,indrat,ratcons,irecm0,efile,pfilem,&
					 igraph2,irec,theta0,nvdep,ifit,ncyc0,imodmf)
					irecq=irecm0
					ifit=0
				endif
				efilt='*.plq'//char(124)//'Plot Queue (PLQ)'//char(124)//&
			   '*.*'//char(124)//'All Files'
			   
	ENDIF
	callid=-106
	call gmremovewindow(iynwin)
	goto 2
case(-106)
	 
		if(curvonly) then
			treso=tresol(1)	!keep (possibly altered) res in microsec for .ini
			treso=treso*1.e-3	!ms
			tresd(1)=dble(treso)*1.0d-3 	!same as tresg (but in seconds)
			tresol(1)=1.e3*treso	!keep (possibly altered) res in mus for .ini
			do j=2,nset
				tresol(j)=tresol(1)	!same for all at moment
				tresd(j)=tresd(1)
			enddo
			nd1=1
			nd2=1
			if(.not.allocated(tint)) then
				ALLOCATE(tint(nd1,nd2),ampl(nd1,nd2),iprops(nd1,nd2))
			    
			endif
			 if(inion.ne.-1) then
				call gmremovewindow(initialpage)
				inion=-1
				endif
				callid=4001
				goto 2
		else
			if(autosim) then
			call gmRemoveWindow(initialpage)
			if(icallidp.eq.-101) then
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

			else
		
			nd1=nmax
			nd2=nset
			nmod9=nmod
!			call PRNTEC50(QT,conc,k,nlig,nset,npar,ncdep)
	
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
			sim=simulat.or.autosim
			call resolution2(tint0(1,j),tint(1,j),ampl0(1,j),ampl(1,j),&
     		iprops0(1,j),iprops(1,j),nint(j),nintt(j),&
     		ffilt(j),fc(j),rms(j),trise(j),nmax,nd1,index(1,j),&
     		cjump,nsweep,kjumps0,kjumps,autosim,nsims,&
     		sim,sres,sexp,excamp,alo(j),ahi(j),resdebug,iresnth)
		 
			tresol(j)=1.e3*treso	!keep (possibly altered) res in mus for .ini
			tresd(j)=dble(treso)*1.0d-3 	!same as tresg (but in seconds)
			enddo
		!	infoind=infoind+1
			call intconv(nset,cnum5)
			itxtinfo=gmcreatetextentry(infopanel,1,0,5,1,&
						'Number of sets= '//cnum5(1:3),60, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
				itxtinfo=gmcreatetextentry(infopanel,1,1,8,1,&
						'Concentration(s)[micromolar]:',100, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)					
		
			do j=1,nset
		!	do i=1,nfileb(j)
		
			   itxtinfo=gmcreatetextentry(infopanel,6+4*(j-1),0,4,1,&
						pfileb(1,j)(1:30),60, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
			!	infoind=infoind+1
			    call realtoch(conc(1,j)*1.e6,cnum0,11)
				itxtinfo=gmcreatetextentry(infopanel,9+4*(j-1),1,4,1,&
						cnum0,60, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)						
			enddo
        	
			call gmRemoveWindow(initialpage)
			if(discprt) write(7,1001)
1001		format(/,' EC50 FOR INITIAL GUESSES:')
	    
			
			do j=1,nset
			if(ffilt(j).le.0.) then
			callid=-119
			goto 2
			endif
			enddo
			iropt=1	
			call gmDefineKeySelectCallback(13,-118)
			call rise_time(main,irise,fca,fcb,trise,nset,irise1,irise2,iTogg1,iTogg2,iTogg3)
		endif
		endif
	case(-31,-32,-33)
		if(callid.eq.-31) then
			iropt=1
			call gmSetToggleSwitch(	itogg1,Gon)
			call gmSetToggleSwitch(	itogg2,Goff)
			call gmSetToggleSwitch(	itogg3,Goff)
		else if(callid.eq.-32) then
			iropt=2
			call gmSetToggleSwitch(	itogg2,Gon)
			call gmSetToggleSwitch(	itogg1,Goff)
			call gmSetToggleSwitch(	itogg3,Goff)
		else
			call gmSetToggleSwitch(	itogg3,Gon)
			call gmSetToggleSwitch(	itogg2,Goff)
			call gmSetToggleSwitch(	itogg1,Goff)
			iropt=3
		endif
	case(-118) ! rise time
	    call gmDefineKeySelectCallback(13,0)
		if(iropt.eq.2) then
		do j=1,nset
		    call gmEnqCellSetting(irise1, 1, i,fca(j),cnum5)
			call gmEnqCellSetting(irise1, 2, i,fcb(j),cnum5)
			fc(j)=1.0/sqrt(1./fc(j)**2 + 1./fca(j)**2 + 1./fcb(j)**2)
			trise(j)=332.1/fc(j)
		enddo
		else if(iropt.eq.2) then
			do j=1,nset
				 call gmEnqCellSetting(irise2, 1, i,trise(j),cnum5)
				fc(j)=332.1/trise(j)
			enddo
		endif
		call gmRemoveWindow(irise)
		do j=1,nset	 
			
				call FALSEV1(treso,fc(j),rms(j),avamp(j),frato)
				zo=1000.*treso/trise(j)		!tres in ms, trise in mus
				aamaxo=erfs(0.88604*zo)
				call FALSEV1(tresg,fc(j),rms(j),avamp(j),fratg)
				zg=1000.*tresg/trise(j)		!tres in ms, trise in mus
				aamaxg=erfs(0.88604*zg)
				if(discprt) write(7,530) 1000.*treso,zo,aamaxo,1000.*tresg,zg,aamaxg
				if(discprt) write(7,532) frato,fratg
		
		enddo

530	format(/,' Resolution (microsec):',/,&
      '   for openings = ',f8.2,' (= ',g11.4,' risetimes, A/Amax = ',g12.5,' )',/,&
      '   for shuttings= ',f8.2,' (= ',g11.4,' risetimes, A/Amax = ',g12.5,' )',/)
532	format('  false event rate (per sec) for openings  = ',g13.6,/,'			      for shuttings = ',g13.6)
callid=-119
goto 2
case(-119) ! resolution
		 	
	
		do j=1,nset
		
			if(setbad(1,j).eq..true.) then
				n=0
				do i=1,nint(j)
				if(tint(i,j).ge.tcbad(1,j).and.ampl(i,j).eq.0.) then
				iprops(i,j)=IBSET(iprops(i,j),3)	!gap unusable; set bit 3 ='8'
				n=n+1
				endif
				enddo
		  
				if(discprt) write(7,47) j,n,tcbad(1,j)
		        ncbad(1,j)=n
		     endif
			 if(setbad(2,j).eq..true.) then
				n=0
				do i=1,nint(j)
				if(tint(i,j).ge.tcbad(2,j).and.ampl(i,j).ne.0.) then
				iprops(i,j)=IBSET(iprops(i,j),3)	!gap unusable; set bit 3 ='8'
				n=n+1
	   			endif
				enddo
				if(discprt) write(7,471) j,n,tcbad(2,j)
				ncbad(2,j)=n
			endif
			 
			if(burst(j)) then
				if(discprt) write(7,84) j,tcrit(j)
	        endif
	        if(chsvec(j)) then
      
      	    if(discprt) write(7,431)
			else
			if(discprt) write(7,432)
	 	    endif
			if(badend(j)) then
 			if(discprt) write(7,4321)
			else
 			if(discprt) write(7,4322)
			endif

		
		enddo
		if((.not.nodata).and.(.not.autosim)) then
			do jset=1,nset
				call GETOPER(jset,tint,ampl,iprops,nint,nd1,nd2,iexcop,gaplo,gaphi,nskip)
			enddo
		endif
		if(icallidp.eq.-101) then
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

	case(-111) ! enlarge
	!	call gmSetToggleSwitch(ini_Toggle1_1, Gon)
	 !   ixm=-101
	   
	    icprev=-111
		callid=1001
		goto 2
	case(-113)
		!	call gmSetToggleSwitch(ini_Toggle1_1, Goff)
			call gmSetWidgetStatus(ini_text1_1, GSELECTABLE)
			icalprev=-113
			ntog=2
			text_tog(1)='Yes'
			text_tog(2)='No'
			text_tog(100)='DC Model'
			call toggle_panel(Main,ITOGGLE,ittoglepanel,ntog,text_tog,intoggle,-139,valdat,idat,itogbutton,itype)
	case(-139)
		    istatus0=gmEnqToggleSwitch(inToggle(1))
			if(istatus0.eq.gon) then
				dcmod=.true.
				dcmodel=.true.
				call gmSetToggleSwitch(ini_Toggle1_3, Gon)
			else
			    dcmod=.false.
				dcmodel=.false.
				call gmSetToggleSwitch(ini_Toggle1_3, Goff)
			endif
			call gmRemoveWindow(itoggle)	
	case(-112)
		!	call gmSetToggleSwitch(ini_Toggle1_1, Goff)
			call gmSetWidgetStatus(ini_text1_1, GSELECTABLE)
			icalprev=-112
			ntog=2
			text_tog(1)='Display old model'
			text_tog(2)='Create new model'
			call toggle_panel(Main,ITOGGLE,ittoglepanel,ntog,text_tog,intoggle,-115,valdat,idat,itogbutton,itype)
	case(-115)
		    istatus0=gmEnqToggleSwitch(inToggle(1))
			if(istatus0.eq.gon) then
				callid=1200
				call gmRemoveWindow(itoggle)	
				call gmRemoveWindow(initialpage)
				jopen2(igraph2)=-1
				inipage=-200
	!			call help_modify(main,imodhelp)
				goto 2
			endif
			istatus0=gmEnqToggleSwitch(inToggle(2))
			if(istatus0.eq.gon) then
				callid=1100
				call gmRemoveWindow(itoggle)	
				call gmRemoveWindow(initialpage)
				inipage=-300
				jopen2(igraph2)=-1
				goto 2
			endif
			goto 1
	case(-121)
		!call gmSetToggleSwitch(ini_Toggle2_1, Gon)
	
	case(-122) !mr
	!	call gmSetToggleSwitch(ini_Toggle2_1, Goff)	
	    call gmSetWidgetStatus(ini_value2_1, GSELECTABLE)
	    if(ncyc.lt.0) ncyc=0
		
		if(ncyc.eq.0) then

		    text_tog(2)='Spanning Tree Method'
		    text_tog(1)='4 Cycles Method'
	        call toggle_panel(Main,ITOGGLE,itogglepanel,2,text_tog,intoggle,-125,valdat,idat,itogbutton,itype)
 		    call gmsetwidgetstatus(intoggle(2),gunselectable)
		else
			callid=2023
			goto 2
		endif
	case(-123)
		indmr=2010
		call gmSetToggleSwitch(intoggle(2), Goff)	
	case(-124)
		indmr=2020
	    call gmSetToggleSwitch(intoggle(1), Goff)	
	case(-125)
		if(indmr.ne.2010.and.indmr.ne.2020) indmr=2020
		callid=indmr
		inipage=-100
		call gmremovewindow(itoggle)
		goto 2
	case(-126)
		call gmremovewindow(itoggle)
		case(-127)
		call gmremovewindow(itwin1)
	case(-131)
		call gmSetToggleSwitch(ini_Toggle3_1, Gon)
	case(-132)
	!	call gmSetToggleSwitch(ini_Toggle3_1, Goff)
		call gmSetWidgetStatus(ini_value3_1, GSELECTABLE)
		inipage=-100
		callid=2030
		!callid=2040 ! FIT OR SHOW DATA
		!call gmSetWidgetStatus(ini_value4_1, GSELECTABLE)
		!isetu0=isetu
		goto 2		
	case(-141) 
	!	call gmSetToggleSwitch(ini_Toggle4_1, Gon)
	case(-142,-147,-148,-149,-146)	
		! -142  cHANGE
		! -147	fIT
		! -148 SHOW DATA ONLY
		!istatus0=gmEnqToggleSwitch(ini_Toggle1_3)
		!if(istatus0.eq.gon) then
		!	    dcmod=.true.
		!else
		!		dcmod=.false.
		!endif
		dcmodel=dcmod
		if(callid.eq.-149) then
			
			nofit=.true.
			nodata=.true.
			curvonly=.true.
		endif
		if(callid.eq.-148) then
			
			nofit=.true.
			
		!	curvonly=.true.
		endif
		if(callid.eq.-146) then
		    liksurf=.true.
		endif
		call gmSetWidgetStatus(ini_value4_1, GSELECTABLE)
		
		if(callid.ne.-142) then
		penalty=fixec50.or.fixpmax
		if(.not.curvonly) then
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
			aval=gmenqvaluesetting(ini_Value6_1)
			conc(1,ns)=aval*1e-6
			if(ncdep.gt.0) then
			if(conc(1,ns).eq.0.) then
				imes=gmdisplaymessagebox('ERROR','Concentration=0',gstop,gok)
				goto 1
			endif
			endif
			tcrit(ns)=gmenqvaluesetting(ini_Value6_3)
			if(tcrit(ns).eq.0.) then
			imes=gmdisplaymessagebox('ERROR','Tcrit = 0',gstop,gok)
		
			goto 1
			endif
			aval=gmenqvaluesetting(ini_Value6_9)
			tresol(ns)=aval
			if(tresol(ns).eq.0.) then
			    imes=gmdisplaymessagebox('ERROR','Resolution = 0',gstop,gok)
			    goto 1
			endif
			endif
			xs=gmenqvaluesetting(ini_Value7_3)
			if(xs.eq.0.) then
			imes=gmdisplaymessagebox('ERROR','Assoc rate = 0',gstop,gok)
		
			goto 1
			endif
			xs1=gmenqvaluesetting(ini_Value7_4)
			if(xs1.eq.0.) then
			imes=gmdisplaymessagebox('ERROR','Fitted rate = 0',gstop,gok)
		
			goto 1
			
			endif
			do j=1,models(igraph2)%npar
			call gmEnqCellSetting(ini_TxtArray7, 3,j,ratcons(indrat)%value(j),actext)
			enddo
            if(ioptm.eq.3.or.ioptm.eq.4) then
            ncyc2=ncyc
		    do i=1,ncyc
		        nsc2(i)=nsc(i)
		        do j=1,nsc(i)
		        im2(i,j)=im(i,j)
		        jm2(i,j)=jm(i,j)
		        enddo
		    enddo    
            endif   
		endif
		if(callid.ne.-142) inipage=-300
		if(autosim.and.(callid.eq.-147.or.callid.eq.-148)) then
		
		
		endif
		if(callid.eq.-147) isetu=7
		if(callid.eq.-148.or.callid.eq.-149.or.callid.eq.-146) isetu=8
		callid=2040 ! FIT OR SHOW DATA
		inipage=-100
		isetu0=isetu
		
		goto 2
				
	case(-143)
	!	call gmSetToggleSwitch(ini_Toggle4_2, Gon)
	case(-144) ! ec50
	!	call gmSetToggleSwitch(ini_Toggle4_2, Goff)
		call gmSetWidgetStatus(ini_value4_2, GSELECTABLE)
		inipage=-100
		callid=2035
		goto 2		
	case(-151)
	!	call gmSetToggleSwitch(ini_Toggle5_1, Gon)
	case(-152)! read new data
        icallprev=-152
		if(inipage.ne.-500) call gmSetWidgetStatus(ini_value5_1, GSELECTABLE)
		if(inipage.ne.-500) inipage=-100
		if(autosim) then
		else
		callid=3001	
		endif
		goto 2	
	case(-153)
		icall=-154
		if(autosim) then
		 do j=1,nset
			tedit1(1,j)=simfile1
			tedit2(1,j)=cDATEW
			
			tval6(1,j)=avamp(j)
		
			tval9(1,j)=conc(1,j)*10**6
		enddo
		endif
		call hjcfit_table(main,hjcfitform,nset,nfile,pfiles,tedit1,tedit2,&
	tedit3,tedit4,tval1,tval2,tval3,tval4,tval5,tval6,tval7,tval8,tval9,tvalt,tedit5,&
	fc,ffilt,trise,autosim,icall,val9,idatyp)
	case(-154)
		call gmremovewindow(hjcfitform)
	case(-161) ! CHANGE SETTINGS
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
	case(-162) ! CHANGE RESOLUTION,CONCETRATION,ETC
	!	call gmSetToggleSwitch(ini_Toggle6_1, Goff)
		!	call gmSetWidgetStatus(ini_value6_1, GSELECTABLE)
		!		call gmSetWidgetStatus(ini_value6_3, GSELECTABLE)
		!	call gmSetWidgetStatus(ini_value6_7, GSELECTABLE)
		!	call gmSetWidgetStatus(ini_value6_8, GSELECTABLE)
		
		    inipage=-100
			j=ns
			aval= gmenqvaluesetting(ini_Value6_1)
			conc(1,j)=aval*1e-6
			aval=gmenqvaluesetting(ini_Value6_9)
			tresol(j)=aval
			icallprev=-162
			
			if(.not.curvonly) then
			if(readini) then
				tcrit(j)=gmenqvaluesetting(ini_Value6_3)
				tcbad(1,j)=gmenqvaluesetting(ini_Value6_7)
				tcbad(2,j)=gmenqvaluesetting(ini_Value6_8)
			endif
			call gmenqtextsetting(ini_Text6_01,ligname(1))
			if(nlig.eq.2) then
			call gmenqtextsetting(ini_Text6_02,ligname(2))
			endif
			vgroup1(j)=tcrit(j)
			vgroup2(j)=tcbad(1,j)!shut
			vgroup3(j)=tcbad(2,j)!open
			vgroup4(j)=conc(1,j)*10**6
			vgroup4_1(j)=conc(2,j)*10**6
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
			group_Toggle4,group_Toggle5,inipage,onechan,setbad,chsvec,badend,&
			ns,ligname,igroup_text,nlig,vgroup4_1,group_editval4_1,igroup_text2)
	
		else
			val(2)=conc(1,j)*10**6
			val(1)=tresol(j)
			vtext(2)='Concentration,'//ligname(1)
			vtext(1)='Resolution'
			itypeval(1)=7
			itypeval(2)=7
			itypeval(3)=7
			nbb=2
			if(nlig.eq.2) then
			nbb=3
			val(3)=conc(2,j)*10**6
				vtext(3)='Concentration,'//ligname(2)
			endif
			call value_table(Main,ivwin,nbb,vtext,ival,val,-166,itypeval)
		endif		
	case(-166)
			aval= gmenqvaluesetting(ival(2))
			conc(1,j)=aval*1e-6
			aval=gmenqvaluesetting(ival(1))
			tresol(j)=aval
			
			call gmsetvaluesetting(ini_Value6_1,conc(1,j)*1e6)
			call gmsetvaluesetting(ini_Value6_9,tresol(j))

			if(nlig.eq.2) then
			aval= gmenqvaluesetting(ival(3))
			conc(2,j)=aval*1e-6
			call gmsetvaluesetting(ini_Value6_12,conc(2,j)*1e6)
			endif
			call gmremovewindow(ivwin)

	case(-163)
			if(ns.lt.nset) then
		    ns=ns+1
			call gmsetvaluesetting(ini_Value6_0,ns)
			call gmsetvaluesetting(ini_Value6_1,conc(1,ns)*1e6)
			call gmsetvaluesetting(ini_Value6_3,tcrit(ns))
			call gmsetvaluesetting(ini_Value6_7,tcbad(1,ns))
			call gmsetvaluesetting(ini_Value6_8,tcbad(2,ns))
			if(onechan) then 
						call gmSetToggleSwitch(ini_Toggle6_2,Gon)
					else
						call gmSetToggleSwitch(ini_Toggle6_2,Goff)
					endif
					if(chsvec(ns)) then
						call gmSetToggleSwitch(ini_Toggle6_4,Gon)
				    else
						call gmSetToggleSwitch(ini_Toggle6_4,Goff)
					endif
					if(badend(ns)) then
						call gmSetToggleSwitch(ini_Toggle6_5,Gon)
					else
						call gmSetToggleSwitch(ini_Toggle6_5,Goff)
					endif
					if(setbad(1,ns).eq..true.) then
						call gmSetToggleSwitch(ini_Toggle6_7,Gon)
					else
						call gmSetToggleSwitch(ini_Toggle6_7,Goff)
					endif
					if(setbad(2,ns).eq..true.) then
					 call gmSetToggleSwitch(ini_Toggle6_8,Gon) 
					else
						call gmSetToggleSwitch(ini_Toggle6_8,Goff)
					endif
		
			call gmsetvaluesetting(ini_Value6_9,tresol(ns))
			call gmsetwidgetstatus(ini_Button16_4,gselectable)
			if(ns.eq.nset) then
				call gmsetwidgetstatus(ini_Button16_3,gunselectable)
			else
					call gmsetwidgetstatus(ini_Button16_3,gselectable)
			endif
			call gFlushGraphics()
			endif
	case(-164) !change reso,conc,etc
		if(ns.gt.1) then
			ns=ns-1
			call gmsetvaluesetting(ini_Value6_0,ns)
			call gmsetvaluesetting(ini_Value6_1,conc(1,ns)*1e6)
			if(nlig.eq.2) call gmsetvaluesetting(ini_Value6_12,conc(2,ns)*1e6)
			call gmsetvaluesetting(ini_Value6_3,tcrit(ns))
			call gmsetvaluesetting(ini_Value6_7,tcbad(1,ns))
			call gmsetvaluesetting(ini_Value6_8,tcbad(2,ns))
			call gmsetvaluesetting(ini_Value6_9,tresol(ns))
			
			if(onechan) then 
						call gmSetToggleSwitch(ini_Toggle6_2,Gon)
					else
						call gmSetToggleSwitch(ini_Toggle6_2,Goff)
					endif
					if(chsvec(ns)) then
						call gmSetToggleSwitch(ini_Toggle6_4,Gon)
				    else
						call gmSetToggleSwitch(ini_Toggle6_4,Goff)
					endif
					if(badend(ns)) then
						call gmSetToggleSwitch(ini_Toggle6_5,Gon)
					else
						call gmSetToggleSwitch(ini_Toggle6_5,Goff)
					endif
					if(setbad(1,ns).eq..true.) then
						call gmSetToggleSwitch(ini_Toggle6_7,Gon)
					else
						call gmSetToggleSwitch(ini_Toggle6_7,Goff)
					endif
					if(setbad(2,ns).eq..true.) then
					 call gmSetToggleSwitch(ini_Toggle6_8,Gon) 
					else
						call gmSetToggleSwitch(ini_Toggle6_8,Goff)
					endif
					
			
			
			
		
			call gmsetwidgetstatus(ini_Button16_3,gselectable)
			if(ns.eq.1) then
				call gmsetwidgetstatus(ini_Button16_4,gunselectable)
				
			else
					call gmsetwidgetstatus(ini_Button16_4,gselectable)
					
			endif
			call gFlushGraphics()
		endif
	case(-165)  ! constrain Popen
	    icallp=-170
	    call popenmax(main,ipopen,fixpmax,pomax,penfac,icallp,ipval1,ipval2,ioptog1,ioptog2)
	    if(fixpmax)then
        call gmSetWidgetStatus(ipval1, Gselectable)
		call gmSetWidgetStatus(ipval2, Gselectable)
else
        call gmSetWidgetStatus(ipval1, Gunselectable)
		call gmSetWidgetStatus(ipval2, Gunselectable)
endif
	case(-170)  ! continue
	    if(fixpmax) then
	        xp1=gmenqvaluesetting(ipval1)
	        xp2=gmenqvaluesetting(ipval2)
	        pomax=dble(xp1)
	        penfac=dble(xp2)
	        penalty=.true.
	        call gmsetvaluesetting(ini_Value6_10,sngl(pomax))
			call gmSetToggleSwitch(ini_Toggle6_10,Gon)
				
	    else
	        penalty=.false.
	        if(fixec50) penalty=.true.
	        
			call gmSetToggleSwitch(ini_Toggle6_10,Goff)
				   
	    endif
	    call gmremovewindow(ipopen)
	case(-167)  ! cancel
	    call gmremovewindow(ipopen)
	case(-168)  ! fix
	    fixpmax=.true.
	    call gmSetWidgetStatus(ipval1, Gselectable)
		call gmSetWidgetStatus(ipval2, Gselectable)
		
	case(-169)  ! no fix
	    fixpmax=.false.
	    call gmSetWidgetStatus(ipval1, Gunselectable)
		call gmSetWidgetStatus(ipval2, Gunselectable)
		
	case(-171)
		call gmSetToggleSwitch(ini_Toggle7_1, Gon)
	case(-172,-179)
	
		call gmSetWidgetStatus(ini_value7_4, GSELECTABLE)
		call gmSetWidgetStatus(ini_value7_3, GSELECTABLE)
		call gmSetWidgetStatus(ini_text7_1, GSELECTABLE)
		inipage=-100
		kgraph=igraph2
		if(callid.eq.-179) kgraph=igraph2-1					
		callid=kgraph+1400
		goto 2
	case(-173)
			call gmSetToggleSwitch(ini_Toggle7_2, Gon)
	case(-174) ! change ratemax,etc
		
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
		if(modplot.gt.0) then
			xmaxsav=xmax1	!in common: record xmax to detect if it is changed in graph_attrib
			
			rescalex=.false.
	  jrplot=oldrecords(modplot)%iplot	
			call graph_attributes(main,callid,modplot,lframe,jindex,ind_1,oldrecords,readrec,newfile,&
		graphics1_1,combo1_6,combo1_4,combo1_5,combo1_8,combo1_9,combo1_10,istate,hdisp,izoom,xtitle,ytitle,&
		xmin0,xmax0,ymin0,ymax0,itogglepanel,itext_entry,new_text_entry,jrplot,iptype,rescalex,&
		isym,symsiz,combo1_11,combo1_12,imodax,iaminax,iamaxax,iatic,imtext3,irxx,iryy,&
		calbarx,calbary,xbeg4,ybeg4,xend4,yend4,&
		ivalx1,ivaly1,ivalx2,ivaly2,ivaltx,ivalty,iax_entry)
		!,&
        !xbeg5,ybeg5,xend5,yend5,d_line,d_hline,d_vline,d_arrow,d_poly,d_text,d_zoom,o_state,&
		 !  c_state,s_text,s_line,s_arrow,zoom,link,delete_state,move_state)
!DC March 19 2007 add iptype to parameters, though not clear that it is defined at time of call

	!  When x axis is rescaled, then, on 2nd time through graph attributes,, 
	!  at this point the new xmin and xmax are set in xmin1 and xmax1 (via 
	!  common/limits/ and in oldrecords(modplot)%xmin, oldrecords(modplot)%xmax  
	!   but xmin, xmax not reset until call to store_record with isens=0, which defines
	!   xmin, xmax from oldrecords()
	!  NB in case where x axis has log scale, xmin1, xmin2 are in original
	!  units but oldrecords() are in log units.  #
	!  For purpose of recalculating data when xmax changed on shut time plot
	!  will be simpler if xmin, xmax (in log units) defined now
	  
		if(jrplot.eq.2.and.rescalex) then
				!shut times
			inext=1
			xmaxsav=xmax
			ymaxsav=ymax
			xmaxsav0=xmax
			ymaxsav0=ymax
			xmin=oldrecords(modplot)%xmin
			xmax=oldrecords(modplot)%xmax
			ymin=oldrecords(modplot)%ymin
			ymax=oldrecords(modplot)%ymax
			
		!	xmin=oldrecords(modplot)%xmin
		!	xmax=oldrecords(modplot)%xmax
		   
		 !  if(abs(xmax-xmaxsav).gt.0.0001) then !xmax and xmaxsav in log units if logt
		!		rescalex=.true.		!x axis rescaled, so recalculate data
				if(logx) xmax=10.**xmax
				if(logy) ymax=10.**ymax
				ymaxsav=ymax
		!		if(rescalex) then !mark tcrit with arrow
				isval=1
				sval=tcrit(iset)
		!		endif
				callid=4600		!recalc data and curves over new range
				goto 2
		   !endif
		endif

		if(callid.eq.247.or.callid.eq.262) then
			callid=405
			goto 2

		endif
		if(callid.eq.407) goto 2


else
		!imesr=gmdisplaymessagebox('','No graph on display',ginformation,gok)
		!callid=0
endif
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
				!		ALLOCATE(ndat(ndimd),isym(ndimd),ijoin(ndimd),symsiz(ndimd))
							ALLOCATE(ndat(njset),isym(njset),ijoin(njset),symsiz(njset))
						
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
		   mono=.false.
			call calc_default(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,symsiz,&
				ndimd,XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,&
				XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,&
				XLO1,XHI1,YLO1,YHI1,y0,yinf,inumx,inumy,&
				titlex,titley,ilabel,doframe,autplt,itit,title1,&
				ISHP,ifont,landscap,ndv1,ndc1,w,kwi,kwj,icurvw,kmax,iver,&
				redrawn,plot,redo,pcgm,pwmf,npint,hdisp,n1,tlenx,tleny,nx1,ny1,izoom,&
				xtitle,ytitle,rescale,dxs,dys,ifitype,parval,theta,ncomp,isval,sval,iplot,&
		calbarx,calbary,xbeg4,ybeg4,xend4,yend4,&
        xbeg5,ybeg5,xend5,yend5,iparfirst,ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil,xwbase)	
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
					icol,ntx,nty,idev,thick,itype,&
		calbarx,calbary,xbeg4,ybeg4,xend4,yend4,&
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
						symsiz,thick,barleng,interp,npint,Xint,Yint,nintg,yp1,ypn,&
						Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw,&
					ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil)
				endif
				call draw_curves(xcal,ycal,icurvc,ncurvc,logity,logx,&
					logy,sqrty,y0,yinf,x0,ilog,idev,&
						wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
						xmin,xmax,ymin,ymax,ncal,&
					iline,icol,thick,ndc1,ndimc,jfirst,dxs,dys)
					show_curve=.true.
					if(idev.eq.3) show_curve=.false.
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
					!	ALLOCATE(ndat(ndimd),isym(ndimd),ijoin(ndimd),symsiz(ndimd))
							ALLOCATE(ndat(njset),isym(njset),ijoin(njset),symsiz(njset))
						
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
				symsiz,thick,barleng,interp,npint,Xint,Yint,nintg,yp1,ypn,&
				Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw,&
					ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil)
				icol(jindex)=oldrecords(modplot)%attributes%icol(jindex)
			
				call draw_data(xval,yval,icurvd,ncurvd,ndelt,logity,logx,logy,sqrty,&
				y0,yinf,x0,ilog,idev,ndat,ijoin,icol,isym,&
				symsiz,thick,barleng,interp,npint,Xint,Yint,nintg,yp1,ypn,&
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
			show_curve=.true.
			if(idev.eq.3) show_curve=.false.
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
			icol,ntx,nty,idev,thick,itype,&
		calbarx,calbary,xbeg4,ybeg4,xend4,yend4,&
        xbeg5,ybeg5,xend5,yend5)
            
			icol(jindex)=oldrecords(modplot)%attributes%icol(jindex)
			
	
			call draw_axis(xmin,xmax,ymin,ymax,ymin1,ymax1,xtic,ytic,&
			nx1,ny1,xcross,ycross,itx,ity,tlenx,tleny,logx,logy,sqrty,doframe,&
			icol,ntx,nty,idev,thick,itype,&
		calbarx,calbary,xbeg4,ybeg4,xend4,yend4,&
        xbeg5,ybeg5,xend5,yend5)
		endif
	case(501:507) ! 3D drawing
		

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
		if(pfilem.ne.' ') then
		nl=len_trim(pdir)
		pfilem=pdir(1:nl)//'\'//pfilem
		call gmsettextsetting(initialw_Text2,pfilem)
		endif		
	case(951) !(611)
		 call helpcvfit(main,indlist,indlista,list,nlist,idkbutt,idsbutt,951)		
	
	case(961) !(613) help
		!call helpcvfit(main,indlist,indlista,list,nlist,idkbutt,idsbutt,961)
		
		call page_help(main,indmod,iphelp)
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
		 call help_modify(main,imodhelp)
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
		call about_ucl(Main,Form12,title,3)


!===========================================================================

	!MECHANISM

!==========================================================================
	case(1001:2100) ! readini+readmec=true
	    
		call mechanism(callid,igraph2,main,ifiltype,models,imodel,imodold,&
		ratcons,indrat,irecq,readini,iqs,&
		pfilem,qmec,efile,text7,records,nrecs,ijmod,nmr,cxtrue,cytrue,readrec,&
		thetaf,jfix1,imove,neqold,icprev,conc,iformtext,ioptm,ksim,irqsav,&
		initmec,irect,iarca,indrec1,idestf,readmec,nvdep,form1,form1_txtarray1,graph1_2,GraphMainPanel1_2,graphics1_2,&
		jopen2,List7_1,ipbar_progress2,indwin,iqt1,textiqt1,iqwin,modelw,iwcell,&
		icellarray,textcell1,itoggle,lt,ipeny,ipeny_yes,ipeny_no,ipeny_xs,ipeny_fac,&
		icyc_form,itwin1,text_box,ivwin,button7,eqfit,Status_bar1,infolength,infopanel,&
		infoind,intoggle,iradio,ixm,iym,newmodel,joldmodel,nxmodel,nymodel,mtype,mod_create,&
		ngraph2,modplot2,jgraph,ipos,lframe,isw,dxsm,dysm,jmodel,igraph0,newmr,newcons,irecfin,&
		initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1,inipage,isetu,&
		ini_Value2_1,ini_TxtArray2,ini_TxtArray7,ini_TxtArray4_1,ini_TxtArray4_2,irc,iqbox,&
		iqtoggle,icell,iec50,nrows,ncube,cubedef,cubecyc,cubext,qt,liksurf,extcyc,irates,jrates,inion)
		!&
		!ini_Value2_1,ini_TxtArray2,&
	!ini_Value3_1,ini_TxtArray3,ini_Value4_1 ,ini_Value4_2,ini_Value5_1,ini_TxtArray5, &
	!ini_Value6_1 ,ini_Value6_3, ini_Value6_7 ,ini_Value6_8,ini_Value6_9, ini_TxtArray7,&
	!ini_Toggle1_1,ini_Toggle2_1,ini_Toggle3_1,ini_Toggle4_1,ini_Toggle4_2,ini_Toggle5_1,&
	!ini_Toggle6_1,ini_Toggle6_2,ini_Toggle6_4,ini_Toggle6_5,ini_Toggle6_7,ini_Toggle6_8,&
	!ini_Toggle7_1,ini_Toggle7_2,ini_Text7_1,ini_Value7_2,ini_Value7_3,ini_Value7_4,ini_TxtArray4_1,ini_TxtArray4_2 ,&
	!ini_Button8_1,ini_Button8_2,ini_Button8_3,ini_Value6_0,ini_Button16_3,&
	!ini_Button16_4,ini_text6_01,ini_panel2,iniyes,inino,ini_text6_02,ini_Value6_12)
	!,d_line,d_hline,d_vline,d_arrow,d_poly,d_text,d_zoom,o_state,&
	!	   c_state,s_text,s_line,s_arrow,zoom,link,delete_state,move_state)
	!	if(callid.eq.-101.or.callid.eq.-102) ec50=ec50d
	if(callid.eq.3301) then
	    goto 2
	endif
		if(callid.eq.-2301.or.callid.eq.-2302) then
		npar=models(igraph2)%npar
		if(ioptm.gt.1.and.callid.eq.-2302.and.ksim.ne.-1.and.readini) then
		        callid=3040
		        inipage=-100
		        goto 2
		    endif    
		call ini_page(main,nset,npar,ixgrid,iygrid,nfileb,curvonly,autosim,ksim,&
			readini,initmec,nlig,initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1,ini_Value2_1,ini_TxtArray2,&
	ini_Value3_1,ini_TxtArray3,ini_Value4_1 ,ini_Value4_2,ini_Value5_1,ini_TxtArray5, &
	ini_Value6_1 ,ini_Value6_3, ini_Value6_7 ,ini_Value6_8,ini_Value6_9, ini_TxtArray7,&
	ini_Toggle1_1,ini_Toggle2_1,ini_Toggle3_1,ini_Toggle4_1,ini_Toggle4_2,ini_Toggle5_1,&
	ini_Toggle6_1,ini_Toggle6_2,ini_Toggle6_4,ini_Toggle6_5,ini_Toggle6_7,ini_Toggle6_8,&
	ini_Toggle7_1,ini_Toggle7_2,ini_Text7_1,ini_Value7_2,ini_Value7_3,ini_Value7_4,ini_TxtArray4_1,ini_TxtArray4_2 ,&
	inipage,isetu,ini_Button8_1,ini_Button8_2,ini_Button8_3,ini_Value6_0,ini_Button16_3,&
	ini_Button16_4,ini_text6_01,ini_panel2,iniyes,inino,ini_text6_02,ini_Value6_12,ini_Toggle6_10,&
	ini_Value6_10,ini_Toggle8,nfblock,ini_Value4_5,ini_TxtArray4_5,ini_Toggle1_3)
	!ini_page=-100
			plot=.false.
			jgraph=igraph2
			nlig=models(jgraph)%nlig
		    ixmm=models(jgraph)%ix
		    plot=.false.
		    models(jgraph)%ix=-100
		    ibk=1
		    if(callid.eq.-2302.and.ksim.ne.-1) inipage=-100
			call draw_model(igraph2,imodel,main,graph1_2,GraphMainPanel1_2,graphics1_2,&
			models,plot,ipos,mod_create,initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
		    models(jgraph)%ix=ixmm
		    
		    callid=-23
		!    if(dcmod) call gmsettoggleswitch(ini_Toggle1_3,gon)
		endif
		if(callid.eq.-23.or.callid.eq.-101.or.callid.eq.-102.or.callid.eq.-232.or.&
		callid.eq.-22.or.callid.eq.3001.or.callid.eq.-44.or.callid.eq.-206.or.callid.eq.8049) goto 2
		goto 1

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
				call mouse_select(ind_1,npoint,jtemp,d_line, d_hline, d_vline, d_arrow, &
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


   
     case(3001,3005)!12	! Open file from menu bar
	!		if(models(igraph2)%indmod) call gmRemovewindow(indwin)
	!	    nofit=.true.
	!		simulat=.false.
	!		autosim=.false.
	!		nodata=.false.
			if(callid.eq.3005) then
				idatyp=-1
				icalprev=3005
			endif
			if(.not.(allocated(thtrue))) allocate(thtrue(1))
			n=2
			nmaxf=1
			if(readini) then
				val(1)=nset
				do jj=1,nset
				do i=1,nfileb(j)	
				if(nfileb(jj).gt.nmaxf) nmaxf=nfileb(jj)
				enddo

				enddo
				val(2)=nmaxf
			else
				val(1)=1
				val(2)=1
			endif
			vtext(50)='Specify number of data sets'          !title of window
          	itypeval(1)=0
			vtext(1)='Number of sets/concentrations'
			itypeval(2)=0
			vtext(2)='Maximum number of files per set'
			call value_table(Main,ivwin,1,vtext,ival,val,3002,itypeval)
	
	
	!show data tabel
 !===========================================================================
	case(3301) !11	!Simulate data
			autosim=.true.
			idatyp=3
			simulat=.false.
			nofit=.false.
			do i=1,njset
				jmiss(njset)=0
			enddo
			readini=.false.
			radio_text(1)='Read old mechanism'
		
			radio_text(2)='Create new mechanism'

			
			iradio1 = gmCreateComplexDialogueBox(Main,18 ,8 ,10 , 5, GALL, 'Mechanism to be simulated', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
			
				iradio_Radio1 = gmCreateRadioBox(iradio1, 0, 0, 10, 3, gmType=GFREEFORM, gmBorderType=Gnoboundary, &
				gmFillCol=141,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='', gmVpos=GTOP)


				nradio=2

				do i=1,nradio
					jk=0
					if(i.eq.1) jk=1
					iradio_Toggle(i) = gmCreateToggleButton(iradio_Radio1, 1, i-1 , 9, 1, radio_text(i), jk, &
					gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)
				enddo
			
				iradio_Button1 = gmCreatePushButton(iradio1,0,0 , 10, 1, 'Continue', &
				gmhpos=gleft,gmVpos=Gbottom,&
				gmcallback=3302)

			
				call gmdrawwindow(iradio1)
			
case(3302) !simulate data
			imtg=gmenqtoggleswitch(iradio_toggle(1))
			call gmremovewindow(iradio1)
			ksim=-1
			if(imtg.eq.0) then
			
					callid=1100 !35 !new
					goto 2
			    else
					
					callid=1200 !36  ! old
				!    call help_modify(main,imodhelp)
			
				    goto 2
				endif	
			
	!		n=1
	!		val(1)=1
	!		itypeval(1)=1
	!		vtext(1)='Number of sets/concentrations'
	!		call value_table(Main,ivwin,n,vtext,ival,val,3002,itypeval)
		case(3002)
			val(1)=gmenqvaluesetting(ival(1))
			nset=int(val(1))
				call gmRemoveWindow(ivwin)	
			vtext(50)='Specify number of files/data sets'          !title of window
          	do i=1,nset
				itypeval(i)=0
				call intconv(i,cnum0)
				vtext(i)='Set'//cnum0
				val(i)=1
			enddo
			call value_table(Main,ivwin,nset,vtext,ival,val,3003,itypeval)
		case(3003)
			nmaxf=1
			do i=1,nset
				val(i)=gmenqvaluesetting(ival(i))
				nfileb(i)=int(val(i))
				if(nfileb(i).lt.1) nfileb(i)=1
				if(nfileb(i).gt.nmaxf) nmaxf=nfileb(i)
			enddo

			nset1=nset		!for COMMON/LCOMP/
			nset2=nset
			nset0=nset
			nset3=nset
			call gmRemoveWindow(ivwin)	
			if(autosim) then
			nmax=1
			nmaxf=1
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
	!		icall=3031
			call gmSetWidgetStatus(view_record, GSELECTABLE)
	!		call hjcfit_table(main,hjcfitform,nset,nfile,pfiles,tedit1,tedit2,&
	!		tedit3,tedit4,tval1,tval2,tval3,tval4,tval5,tval6,tval7,tval8,tval9,tvalt,tedit5,&
	!		fc,ffilt,trise,autosim,icall)
			nsim=1000		!default
		
			
			!icall=3019
			callid=3033
			sim=simulat.or.autosim
			goto 2
			do i=1,nset
				nintt(i)=20000	!default
				conc(1,nset)=conc(1,1)
				resw(i)=5.00
			enddo
			call autosim1(main,iautosim,nset,nintt,nsim,simfile1,simfile,conc,nlig,&
			iatext1,iatext2,nvalc,ivals,ivalsim,ligname,isimtog,simfile3,iatext3,&
			iresw,resw,itogrec)
			
			
			else
			if(inipage.eq.-100.or.inipage.eq.-500) then 
				icalnext=-152 
			endif
				if(readini) then
				do j=1,nset
				
					if(nfileb(j).lt.1) nfileb(j)=1
						nfile(j)=nfileb(j)
				enddo
				else
					do j=1,nset
					
					nfile(j)=nfileb(j)
					enddo
				endif
			
				icallid=-221
				call change_path(main,initwin1,initwin1_TxtArray,nset,icallid,&
				pfiles,nfileb,-1)
			endif
		case(3015)
			call gmremovewindow(itwin)
		case(3016)
		    istat2=gmenqtoggleswitch(isimtog)
		    if(istat2.eq.gon) then
		    do i=1,nset
		        call gmsetwidgetstatus(iresw(i),gselectable)
		        
		        enddo
		    else
		     do i=1,nset
		      call gmsetwidgetstatus(iresw(i),gunselectable)
		      enddo
		    endif
		case(3019) ! simulate data
			call gmenqtextsetting(iatext1,simfile1)
			call gmenqtextsetting(iatext2,simfile)
			call gmenqtextsetting(iatext3,simfile3)
			
			if(itogrec.eq.8) then
			istat1=goff
			else
			istat1=gmenqtoggleswitch(isimtog)
		    endif
		    simwascii=.false.
		    if(istat1.eq.gon) simwascii=.true.
			do j=1,nset
				nintt(j)=gmenqvaluesetting(ivals(j))
				if(nintt(j).gt.nmax) nmax=nintt(j)
				do i=1,nlig
					xs=	gmenqvaluesetting(nvalc(i,j))
					conc(i,j)=1.e-6*xs
				enddo
			enddo
			if(simwascii) then
			do j=1,nset
				resw(j)=gmenqvaluesetting(iresw(j))
			enddo
			endif
			nsim=gmenqvaluesetting(ivalsim)
			nd1=nmax
			nd2=nset
			call gmremovewindow(iautosim)
			call intconv(nset,cnum5)
			itxtinfo=gmcreatetextentry(infopanel,1,0,5,1,&
						'Number of sets:'//cnum5(1:3),60, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
	
			itxtinfo=gmcreatetextentry(infopanel,1,1,8,1,&
						'Concentration(s)[micromolar]:',100, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)					
		
			do j=1,nset
		!	do i=1,nfileb(j)
		
			   itxtinfo=gmcreatevalueentry(infopanel,6+4*(j-1),0,4,1,&
						nintt(j),10,0, GDISPLAY, &
             			gmType=gdecimal, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP)
			!	infoind=infoind+1
			    call realtoch(conc(1,j)*1.e6,cnum0,11)
				itxtinfo=gmcreatetextentry(infopanel,9+4*(j-1),1,4,1,&
						cnum0,60, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)	
			enddo
			
		restart=nsims.gt.1

			if(allocated(tint0)) deallocate(tint0,iprops0,ampl0,index)
			ALLOCATE(tint0(nmax,nset),iprops0(nmax,nset),ampl0(nmax,nset),index(nmax,nset))
			 if(allocated(tint)) deallocate(tint,iprops,ampl,kjumps,kjumps0)
			ALLOCATE(tint(nmax,nset),iprops(nmax,nset),ampl(nmax,nset))
			cjump=.false.
			nsweep=1
			allocate(kjumps0(nsweep),kjumps(nsweep))
			  nint(j)=nintt(j)
			do j=1,nset
			do i=1,nint(j)
			tint0(i,j)=0.0
			ampl0(i,j)=0.0
			iprops0(i,j)=0
			tint(i,j)=tint0(i,j)
			ampl(i,j)=ampl0(i,j)
			iprops(i,j)=iprops0(i,j)
			enddo
			enddo
			do j=1,nset
			do i=1,nlig
			conc1(i,j)=conc(i,j)
			enddo
			enddo
			restart=nsims.gt.1
			if(allocated(thetval)) deallocate(thetval,nintval,ixval,iyval,izval,elmval,&
			elmset,nevals,ec50val)
			ALLOCATE(thetval(npar,nsim),nintval(nset,nsim),&
			ixval(nsim),iyval(nsim),izval(nsim),elmval(nsim),&
     		elmset(nset,nsim),nevals(nsim))
			ALLOCATE(ec50val(nsim))
	
    
			sim=simulat.or.autosim
			cjump=.false.
			nsweep=1
		    kAs=kA 	!save values for 'true' model. and go round again for fit model
	        kBs=kB
	        kCs=kC
	        kDs=kD
	        ks=k
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
				thtrue(m)=theta0(m)
	        enddo
	   do i=1,kAs
		dgammas(i)=dgamma(i)
	   enddo
	   npars=npar
	   do m=1,npars
		irates(m)=irate(m)
		jrates(m)=jrate(m)
		i=irates(m)
		j=jrates(m)
		QTtrue(i,j)=qt(i,j)
		thtrue(m)=QTtrue(i,j)
	   enddo
	   ncdeps=ncdep
	   nligs=nlig
	   do i=1,ncdeps
		IXs(i)=IX(i)
		JXs(i)=JX(i)
		ILs(i)=IL(i)
	   enddo
	   ncons1=ncon
	   do j=1,ncons1
		ICs(1,j)=IC(1,j)
		ICs(2,j)=IC(2,j)
	   enddo
	   do i=1,ks
		do j=1,ks
		   IQs(i,j)=IQ(i,j)
		enddo
	   enddo
	   mtitle11=mtitles
	   rtitle1=rtitle
	   imod11=imod
	   call RANDSKv1(ixr1,iyr1,izr1,0,repeat,main)
	   icall=0
	   apfile=.false.
	   nsims0=0
	   INQUIRE(file=SIMFILE1,exist=present)
			if(present) then
			ihandle=FILE$FIRST
			length = GETFILEINFOQQ(simfile1, info, ihandle)
			nLEN=info%length
			if(nlen.gt.0) then
				OPEN(unit=10,file=simfile1,status='UNKNOWN',access='DIRECT', form='BINARY', RECL=1)
				read(unit=10,rec=1) iver,nsims1,ABORTW
				CLOSE(unit=10)
				apfile=.true.
				radio_text(1)='Append results to:'//simfile1
		
				radio_text(2)='Overwrite :'//simfile1

				radio_text(3)='Write results to new file:'
				iradio = gmCreateComplexDialogueBox(Main,18 ,8 ,16 , 6, GALL, 'Save simulations in:'//simfile1, &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
			
				initwin_Panel2=gmCreatePanel(iradio, 0, 0,16 , 5, gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmType=Gprojected, &
				gmLineBorder=GOUTEREDGE,gmfillcol=141, gmFillBorder=GOUTEREDGE)
				iradio_Radio1 = gmCreateRadioBox(initwin_Panel2, 0, 1, 10, 4, gmType=GFREEFORM, gmBorderType=Gnoboundary, &
				gmFillCol=141,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='', gmVpos=GTOP)


				nradio=3

				do i=1,nradio
					jk=0
					if(i.eq.1) jk=1
					iradio_Toggle(i) = gmCreateToggleButton(iradio_Radio1, 1, i-1 , 9, 1, radio_text(i), jk, &
					gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)
				enddo
				initText = gmCreateTextEntry(initwin_Panel2, 10, 3, 5, 1,simfile1, 255, gedit, gmBack1Col=0, gmBack2Col=12, &
				gmTextCol=1 , gmVpos=GTOP)

				iradio_Button1 = gmCreatePushButton(iradio,0,0 , 16, 1, 'Continue', &
				gmhpos=gleft,gmVpos=Gbottom,&
				gmcallback=3056)
			
				call gmdrawwindow(iradio)
			
			else
				callid=3055
				goto 2		
			endif
			else
				callid=3055
				goto 2		
			endif


case(3056) ! simulate data
	   
	   istatus=gmEnqToggleSwitch(iradio_Toggle(1))
	   if(istatus.eq.1) then
			apfile=.true.
	   else
		    apfile=.false.
			istatus=gmEnqToggleSwitch(iradio_Toggle(3))
			if(istatus.eq.1) then
				call gmEnqTextSetting(inittext,simfile1)
				nb=len_trim(simfile1)
			!	simfile=simfile1(1:nb-3)//'txt'
			endif
	   endif
	!   INQUIRE(file=SIMFILE,exist=present)
	!	if(apfile.and.(.not.present) )then
		!	apfile=.false.		
	   
	!	endif
		call gmremovewindow(iradio)
		callid=3055
		goto 2		
case(3055) ! simulate data


		if(.not.apfile) then
			nsims=1
		else if(apfile) then
	
			nsims=nsims1+1	!number of simulated expts fitted =1,2,...,nsim
			nsims0=nsims1
		endif
        nsim0=nsim+nsims1
		if(apfile) then
		if(discprt) write(7,110) simfile,simfile1
110		format(' Results of simulation will be appended as text to ',a40,/,&
		' and as binary to ',a40)
		else
		if(discprt) write(7,210) simfile,simfile1
210		format(' Results of simulation will be written as text to ',a40,/,&
		 ' and as binary to ',a40)
		endif
    !!!!!!new auto    callid=3033
        callid=8001
		goto 2
		case(3021:3030,3201:3210)
			import=.false.
		    if(callid.gt.3200) then
				kset=callid-3200
				ivalba=gmenqvaluesetting(ivalbox(kset))
			    if(ivalba.lt.1) ivalba=1
				if(ivalba.gt.nfile(kset)) ivalba=nfile(kset)
			else
			    ivalba=-1
				kset=callid-3020	
			endif
			if(idatyp.eq.1) then
			dfilt='*.scn'//char(124)//'Scan Files (SCN)'//char(124)//&
			'*.*'//char(124)//'All Files'
			else if(idatyp.eq.2) then
			dfilt='*.dat'//char(124)//'Data File (DAT)'//char(124)//&
			'*.*'//char(124)//'All Files'
			else if(idatyp.eq.4) then
			dfilt='*.ede'//char(124)//'Data File (DAT)'//char(124)//&
			'*.*'//char(124)//'All Files'
			else if(idatyp.eq.5) then
			dfilt='*.evl'//char(124)//'Data File (DAT)'//char(124)//&
			'*.*'//char(124)//'All Files'
			else if(idatyp.eq.7) then
			dfilt='*.ede'//char(124)//'Data File (DAT)'//char(124)//&
			'*.*'//char(124)//'All Files'
			else if(idatyp.eq.6) then
			dfilt='*.ede'//char(124)//'Data File (DAT)'//char(124)//&
			'*.*'//char(124)//'All Files'
			else
			dfilt='*.scn'//char(124)//'Scan Files (SCN)'//char(124)//&
			'*.dat'//char(124)//'Data File (DAT)'//char(124)//&
			'*.ede'//char(124)//'Data File (DAT)'//char(124)//&
			'*.evl'//char(124)//'Data File (DAT)'//char(124)//&
			'*.*'//char(124)//'All Files'
			endif
				do i=1,100
					icfit(i)=0
					iopen(i)=-1
					jopen(i)=-1
				enddo
				CALL gmFileBROWSER(DFILE,DDIR,DFILT,gmBrowseType=gmultipleinput)
	 	
		IF(DFILE.ne.' ') then
			call gmSetListEntry(combo1_3,GSELECT,gmEntry=1)
			if(idatyp.eq.-1) then
				nlb=len_trim(dfile)
				if(dfile(nlb-3:nlb).eq.'.ede') then
					idatyp=7
				else if(dfile(nlb-3:nlb).eq.'.evl') then
					idatyp=6
				else if(dfile(nlb-3:nlb).eq.'.dat') then
					idatyp=2
				else if(dfile(nlb-3:nlb).eq.'.scn') then
					idatyp=1
				else if(dfile(nlb-3:nlb).eq.'.dwt') then
					idatyp=7
				endif
			endif
			if(idatyp.eq.1.or.idatyp.eq.2) then
			call openfile(MAIN,RECORDS,nplot,IFILTYPE,iplotype,iptype,DFILE,FOPEN_11,FOPEN_12,&
			APpend,DISCPRT,njset,ncolr,nrowr,label,ftitle,nset,iwid,List7_1, &
			ftitle1, button7,button6,saveandplot,program_type)
		!	call gmsetwidgetstatus(intoggle(7),gchecked)
			else if (idatyp.eq.6) then
			 !   call READ_EDE(n,kfile,pfile,nset,nfile,nval,irecs,&
			!	calfacs2,ffilt,nintt,ntot,nmax)
			iscan=201
			else if (idatyp.eq.5) then
				call READ_EVL(n,kfile,pfile,nset,nfile,nval,irecs,&
				srate,calfacs2,ffilt,nintt,ntot,nmax,main,hjcfitform,avamp,rms,fc,trise,&
                tedit1,tedit2,tedit3,tedit4,tval1,tval2,tval3,tval4,tval5,tval6,tval7,tval8,&
                tval9,tvalt,tedit5,val9,clampex)
                iscan=301
			else if (idatyp.eq.7) then
			!	call READ_DWT(n,kfile,pfile,nset,nfile,nval,irecs,&
			!	srate,calfacs2,ffilt,nintt,ntot,nmax)
			call READ_EDE(n,kfile,pfiles,nset,nfile,nval,irecs,&
				srate,calfacs2,ffilt,nintt,ntot,nmax,main,hjcfitform,&
                avamp,rms,fc,trise,tedit1,tedit2,&
                tedit3,tedit4,tval1,tval2,tval3,tval4,tval5,tval6,tval7,tval8,&
                tval9,tvalt,tedit5,val9)
			endif
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
			if(ivalba.gt.0) then
			   nfilc=ivalba
			else
			nfile(j)=nfile(j)+1
			if(nfile(j).gt.1) then
			do i=1,nfile(j)-1
				call gmenqtextsetting(itext_boxf(i,j),text_boxf(i,j))
			
				if(text_boxf(i,j).eq.' ') then
						nfile(j)=nfile(j)-1
				endif
			enddo
			endif
			nfilc=nfile(j)
			endif
			nd=len_trim(ddir)
			pfiles(j,nfilc)=ddir(1:nd)//'\'//dfile
		!	call gmsettextsetting(itext_box(nfile(i),i),dfile)
			call gmsettextsetting(itext_boxf(nfilc,j),pfiles(j,nfilc))
			
		case(3014) ! finish read data
			do j=1,nset
			nfileb(j)=nfile(j)
			do i=1,nfile(j)
				call gmenqtextsetting(itext_boxf(i,j),text_boxf(i,j))
				pfiles(i,j)=text_boxf(i,j)
			!	pfileb(i,j)=text_boxf(i,j)
				inquire(file=pfiles(i,j),EXIST=PRESENT)
				if(.not.present) then
					imessy=gmDisplayMessageBox('',&
			'File does not exist;pool another one',Gstop,gok)
				goto 1
				endif
			enddo
			enddo
			call gmRemoveWindow(itwin)
			if(inipage.eq.-100) then
			do j=1,nset
			do i=1,nfile(j)
				if(icalprev.ne.-44) then
				
				call gmSetCellSetting(ini_TxtArray5, i,j ,gmString=pfiles(i,j))
				call gmdrawwidget(ini_TxtArray5)
				else 
				call gmSetCellSetting(initwin_TxtArray, i,j ,gmString=pfiles(i,j))
				call gmdrawwidget(initwin_TxtArray)
				endif
			!	pfileb(i,j)=pfiles(i,j)
			enddo
			enddo
			call gmdrawwidget(ini_TxtArray5)
			call gmSetWidgetStatus(ini_Button8_1, GSELECTABLE)
			call gmSetWidgetStatus(ini_Button8_2, GSELECTABLE)
			goto 1
			else
	
			callid=3030
			goto 2
			endif
	case(3040)
		if(ipatch.eq.0) ipatch=1
		if(iopt.eq.1) then
		if((itogrec.eq.5.or.itogrec.eq.6).and.icallprev.ne.-152) then
		iman0=-1
		call HJCDATw(iman0,hjcfitform,idatyp,nfile,kfile,pfiles,nval,&
		irecs,calfacs2,stpfac,nintt,&
		avamp,rms,ffilt,trise,fc,iscan,readini,ndev,idest,&
		name,idiscn,treso,tresg,nset,conc,nlig,ligname,nmax,ntot,&
		qfile2,adcfil,nfileb,pfiles,npatch,defname,samexp,&
		cjump,nsweep,tzero,tsamp,autosim,ipatch,ptype,temp,tedit1,tedit2,&
		tedit3,tedit4,tval1,tval2,tval3,tval4,tval5,tval6,tval7,tval8,&
		tval9,tvalt,tedit5,val9)
	!	idest=0
		callid=3031
		goto 2
		else
		call HJCDATw(main,hjcfitform,idatyp,nfile,kfile,pfiles,nval,irecs,calfacs2,stpfac,&
		nintt,avamp,rms,ffilt,trise,fc,iscan,readini,ndev,idest,&
		name,idiscn,treso,tresg,nset,conc,nlig,ligname,nmax,ntot,&
		qfile2,adcfil,nfileb,pfiles,npatch,defname,samexp,&
		cjump,nsweep,tzero,tsamp,autosim,ipatch,ptype,temp,tedit1,tedit2,&
		tedit3,tedit4,tval1,tval2,tval3,tval4,tval5,tval6,tval7,tval8,&
		tval9,tvalt,tedit5,val9)
		endif
		else if (iopt.eq.2.or.iopt.eq.3)then
		call HJCDATw(main,hjcfitform,idatyp,nfile,kfile,pfiles,nval,irecs,calfacs2,stpfac,&
		nintt,avamp,rms,ffilt,trise,fc,iscan,readini,ndev,idest,&
		name,idiscn,treso,tresg,nset,conc,nlig,ligname,nmax,ntot,&
		qfile2,adcfil,nfileb,pfiles,npatch,defname,samexp,&
		cjump,nsweep,tzero,tsamp,autosim,ipatch,ptype,temp,tedit1,tedit2,&
		tedit3,tedit4,tval1,tval2,tval3,tval4,tval5,tval6,tval7,tval8,&
		tval9,tvalt,tedit5,val9)
		endif
		if(iopt.eq.6) then
		call READ_EVL(n,kfile,pfiles,nset,nfile,nval,irecs,&
				srate,calfacs2,ffilt,nintt,ntot,nmax,main,hjcfitform,&
       avamp,rms,fc,trise,tedit1,tedit2,&
       tedit3,tedit4,tval1,tval2,tval3,tval4,tval5,tval6,tval7,tval8,&
       tval9,tvalt,tedit5,val9,clampex)
		endif
		if(iopt.eq.7) then
		call READ_EDE(n,kfile,pfiles,nset,nfile,nval,irecs,&
				srate,calfacs2,ffilt,nintt,ntot,nmax,main,hjcfitform,&
       avamp,rms,fc,trise,tedit1,tedit2,&
       tedit3,tedit4,tval1,tval2,tval3,tval4,tval5,tval6,tval7,tval8,&
       tval9,tvalt,tedit5,val9)
		endif
		if(iopt.eq.8) then
		call READ_DWT(n,kfile,pfiles,nset,nfile,nval,irecs,&
				srate,calfacs2,ffilt,nintt,ntot,nmax)
		endif
	case(3031) 
		simulat=iscan.eq.-103.or.iscan.eq.-3001
		if(allocated(tint0)) DEALLOCATE(tint0,iampl0,iprops0,ampl0,index)
		allocate(tint0(nmax,nset),iampl0(nmax,nset),iprops0(nmax,nset),&
		ampl0(nmax,nset),index(nmax,nset))
		do j=1,nset
			do i=1,nmax
			tint0(i,j)=0.0
			ampl0(i,j)=0.0
			iprops0(i,j)=0
		enddo
		enddo
		cjump=.false.
		nsweep=1
		if(.not.allocated(kjumps0)) allocate(kjumps0(nsweep),kjumps(nsweep))
		!call hjcdat2()
		if(iopt.eq.1.and.(itogrec.eq.5.or.itogrec.eq.6).and.icallprev.ne.-152) then
		else
		do j=1,nset
		do i=1,nfile(j)
		tval9(i,j)=gmenqvaluesetting(val9(i,j))
		conc(i,j)=tval9(i,j)*1e-6
		enddo
		enddo
		endif
		if(idatyp.eq.5) iscan=301
		
		if(idatyp.eq.6) iscan=201
		CALL HJCDATw2(tint0,iampl0,ampl0,iprops0,iscan,&
        nintt,nfile,kfile,pfiles,calfacs2,nval,irecs,nmax,nset,clampex)
	   !resolution now  `
		if(allocated(tint)) deallocate(tint,ampl,iprops)
	   	allocate(tint(nmax,nset),ampl(nmax,nset),iprops(nmax,nset))
		nd1=nmax
		nd2=nset
	
		do j=1,nset
		   nint(j)=nintt(j)
		   do i=1,nint(j)
			tint(i,j)=tint0(i,j)
			ampl(i,j)=ampl0(i,j)
			iprops(i,j)=iprops0(i,j)
		   enddo
		enddo
		if(iopt.eq.1.and.(itogrec.eq.5.or.itogrec.eq.6).and.icallprev.ne.-152) then
		else
		call gmremovewindow(hjcfitform)
		endif
	!	call gmsetwidgetstatus(intoggle(7),gchecked)
		if(simulat) then
			if(iopt.eq.1.and.(itogrec.eq.5.or.itogrec.eq.6).and.icallprev.ne.-152) then
			
			callid=3052
			goto 2
			
			else
			callid=3033
			goto 2
			do j=1,nset
				n=2
				val(1)=treso*1000.
				val(2)=tresol(j)
				vtext(1)='Resolution (microsec) imposed in SCSIM'
				vtext(2)='Resolution (microsec) used in last run'
				vtext(50)='Resolution'
				icall=3032
				itypeval(1)=9
				itypeval(2)=9
			    call value_table(Main,ivwin,n,vtext,ival,val,icall,itypeval)
				tres=dble(tresol(j))*1.0d-6
				tresd(j)=tres
			
			enddo
			endif
		else
			callid=3033 ! if not shown the hjcfit_table go straight to 3033
			goto 2
		endif
		
case(3032)
		call gmremovewindow(ivwin)
		do j=1,nset
		   nint(j)=nintt(j)
		   do i=1,nint(j)
			tint(i,j)=tint0(i,j)
			ampl(i,j)=ampl0(i,j)
			iprops(i,j)=iprops0(i,j)
		   enddo
		enddo

		callid=3052
		goto 2
case(3033) !resolution
		if(iopt.eq.1.and.(itogrec.eq.5.or.itogrec.eq.6).and.icallprev.ne.-152) then
			

			callid=3052
			goto 2
		endif
		nwin=nset
		if(autosim) then
		nwin=nset
		gmax=0.0
		do i=1,kA
		g=sngl(dgamma(i))
		if(g.gt.gmax) gmax=g
		enddo
		do j=1,nset	!do for each set
		avamp(j)=1.e12*gmax*0.1	!as in hjcsim2
			treso=tresol(j)	!default resolution from .ini in microsec in common
		avamp1=avamp(j)	!in common/rblck/
		enddo
		
		
		endif
	

		do i=1,nset
			tvalres1(i)=0.0
			tvalres2(i)=0.0
			if(tresol(i).le.0) tresol(i)=30.
			if(.not.readini) tresol(i)=30.0
			tvalres3(i)=tresol(i)
			tvalres4(i)=avamp(i)
			tvalres5(i)=acrit(i)
			tvalres8(i)=ffilt(i)
			tvalres9(i)=rms(i)
		enddo
	
		do i=1,nset
		if(ffilt(i).gt.0.) then
			treso=0.001*tresol(i)
			call FALSEV1(treso,fc(i),rms(i),avamp(i),frato)
			zo=1000.*treso/trise(i)		!tres in ms, trise in mus
			aamaxo=erfs(0.88604*zo)
				tresg=treso
			call FALSEV1(tresg,fc(i),rms(i),avamp(i),fratg)
			zg=1000.*tresg/trise(i)		!tres in ms, trise in mus
			aamaxg=erfs(0.88604*zg)
	    else
		   frato=0.
			aamaxo=0.
			fratg=0.
			aamaxg=0.
		endif	
		tvalres6(i)=frato		!false event rate
!	    tvalres7(i)=aamaxo		!rise time (aamax should be printed in reolution2, but isn't
	    tvalres7(i)=trise(i)
		enddo
	 	excamp=.false.
		if(autosim) then
		  !  if(itogrec.eq.8) then ! demo
			!    callid=3052
			 !   goto 2
			!else
			
			    kAs=models(jgraph)%kA 	!save values for 'true' model. and go round again for fit model
				kBs=models(jgraph)%kB
			
				ks=models(jgraph)%n
				kcs=ks-kas-kbs
				npars=models(jgraph)%npar
				do m=1,npars
				irates(m)=irate(m)
				jrates(m)=jrate(m)
				i=irates(m)
				j=jrates(m)
				QTtrue(i,j)=qt(i,j)
				thtrue(m)=QTtrue(i,j)
				enddo
				ncdeps=ncdep
				nligs=nlig
				onechan=.true.
				nchan=1
				do i=1,nset
				nintt(i)=20000	!default
				conc(1,nset)=conc(1,1)
				resw(i)=5.00
			    enddo
			    call autosim1(main,iautosim,nset,nintt,nsim,simfile1,simfile,conc,nlig,&
			    iatext1,iatext2,nvalc,ivals,ivalsim,ligname,isimtog,simfile3,iatext3,&
			    iresw,resw,itogrec)
			
		!	endif
		
		else
		    callid=3303
		    goto 2
        endif
case(3303) 
        if(itogrec.eq.8) then
        do j=1,nset
		treso=0.001*tresol(j)
		tresg=treso
		treso5=treso
		tresg5=tresg
		acrit5=acrit(j)
		avamp5=avamp(j)
		sim=simulat.or.autosim
		!if(.not.autosim) then
		call resolution2(tint0(1,j),tint(1,j),ampl0(1,j),ampl(1,j),&
     	 iprops0(1,j),iprops(1,j),nint(j),nintt(j),&
     	 ffilt(j),fc(j),rms(j),trise(j),nmax,nd1,index(1,j),&
     	 cjump,nsweep,kjumps0,kjumps,autosim,nsims,&
     	 sim,sres,sexp,excamp,alo(j),ahi(j),resdebug,iresnth)
		 !endif
		 tresol(j)=1.e3*treso	!keep (possibly altered) res in mus for .ini
		 tresd(j)=dble(treso)*1.0d-3 	!same as tresg (but in seconds)

		enddo
		if(discprt) write(7,43)
43		format(/,' Resolution for HJC calculations')
		do j=1,nset
	   
        if(discprt) write(7,430) j,tresd(j)*1.d6
430	   format('   Set ',i3,': ',f10.3,' microseconds')
		enddo
		callid=3052
		!callid=8022
		goto 2
        endif
		call resolution(main,iresoform,nwin,tvalres1,tvalres2,tvalres3,tvalres4,tvalres5,&
		tvalres6,tvalres7,tvalres8,tvalres9,valres1,valres2,valres3,valres4,valres5,&
		valres6,valres7,valres8,valres9,autosim)



	case(3041,3042)
		!i=callid-3040
		if(callid.eq.3041) then
		do i=1,nset
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
	    call FALSEV1(treso,fc(i),rms(i),avamp(i),frato)
	    zo=1000.*treso/trise(i)		!tres in ms, trise in mus
	    aamaxo=erfs(0.88604*zo)
	    call FALSEV1(tresg,fc(i),rms(i),avamp(i),fratg)
	    zg=1000.*tresg/trise(i)		!tres in ms, trise in mus
	    aamaxg=erfs(0.88604*zg)
	    
		tvalres6(i)=frato
!	    tvalres7(i)=aamaxo
	    tvalres7(i)=trise(i)
	  !  tvalres8(i)=fratg
	  !  tvalres9(i)=aamaxg
		call gmsetvaluesetting(valres6(i),tvalres6(i))
		call gmsetvaluesetting(valres7(i),tvalres7(i))
	!	call gmsetvaluesetting(valres8(i),tvalres8(i))
	!	call gmsetvaluesetting(valres9(i),tvalres9(i))
		endif
		enddo
		else
			frato=0.
			aamaxo=0.
			fratg=0.
			aamaxg=0.
			do i=1,nset
			tvalres6(i)=frato
!			tvalres7(i)=aamaxo
			tvalres7(i)=trise(i)
!			tvalres8(i)=fratg
!			tvalres9(i)=aamaxg
			call gmsetvaluesetting(valres6(i),tvalres6(i))
			call gmsetvaluesetting(valres7(i),tvalres7(i))
		enddo
		endif
	            !=======================????????
	case(3043)
	    excamp=.true.
	case(3044)
	    excamp=.false.
	
	case(3051) ! next stage resolution
	!	if(autosim) then
		do i=1,nset
		tvalres3(i)=gmenqvaluesetting(valres3(i))
		tvalres4(i)=gmenqvaluesetting(valres4(i))
	!	tvalres5(i)=gmenqvaluesetting(valres5(i))
			avamp(i)=tvalres4(i)
	!	acrit(i)=tvalres5(i)
		tresol(i)=tvalres3(i)
			treso=0.001*tresol(i)		!in ms (default is in ms already)
		
		tresg=treso
		enddo
	!	endif
	    call gmremovewindow(iresoform)
		do j=1,nset
		treso=0.001*tresol(j)
		tresg=treso
		treso5=treso
		tresg5=tresg
		acrit5=acrit(j)
		avamp5=avamp(j)
		sim=simulat.or.autosim
		!if(.not.autosim) then
		call resolution2(tint0(1,j),tint(1,j),ampl0(1,j),ampl(1,j),&
     	 iprops0(1,j),iprops(1,j),nint(j),nintt(j),&
     	 ffilt(j),fc(j),rms(j),trise(j),nmax,nd1,index(1,j),&
     	 cjump,nsweep,kjumps0,kjumps,autosim,nsims,&
     	 sim,sres,sexp,excamp,alo(j),ahi(j),resdebug,iresnth)
		 !endif
		 tresol(j)=1.e3*treso	!keep (possibly altered) res in mus for .ini
		 tresd(j)=dble(treso)*1.0d-3 	!same as tresg (but in seconds)

		enddo
		if(discprt) write(7,43)
!43		format(/,' Resolution for HJC calculations')
		do j=1,nset
	   
        if(discprt) write(7,430) j,tresd(j)*1.d6
!430	   format('   Set ',i3,': ',f10.3,' microseconds')
		enddo
		callid=3052
		!callid=8022
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
			
			if(discprt) write(7,3311) j
3311	    		format(/,' Set ',i3,': bad sections (from stability plot) are NOT omitted',/)
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
		itypeval(1)=9
		itypeval(2)=9
		call value_table(Main,ivwin,n,vtext,ival,val,3091,itypeval) !9532)
				

	CASE(3081:3090)!(9561:9570)
		j=callid-3080
		n=1
		val(1)=0.
		vtext(1)='Rise time (microseconds) ='
		itypeval(1)=9
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
	CASE(3100)!(9700) ! set reso,concentration
		if(iopt.eq.1.and.(itogrec.eq.5.or.itogrec.eq.6).and.icallprev.ne.-152) then
			callid=3045
			goto 2
		else if(autosim.and.itogrec.eq.8) then !demo
			callid=3045
			goto 2	 
		else
		 	
		 do j=1,nset
			if(.not.readini) then
			onechan=.false.
			if(autosim) onechan=.true.
			if(onechan) then
				burst(j)=.false.
				chsvec(j)=.false.		!irrelevant in this case!
				badend(j)=.true.		!bad gap is valid end of group
				tcrit(j)=0.0		!msec=1 year! -see PRANGE
			else
				burst(j)=.true.
				chsvec(j)=.true.		!irrelevant in this case!
				badend(j)=.true.		!bad gap is valid end of group
				tcrit(j)=0.0
			endif
			endif
			vgroup1(j)=tcrit(j)
			vgroup2(j)=tcbad(1,j)!shut
			vgroup3(j)=tcbad(2,j)!open
			vgroup4(j)=conc(1,j)*1e6
			vgroup4_1(j)=conc(2,j)*1e6
			vgroup5(j)=tresol(j)
		enddo
		
		ns=0
	
		call defgrp(main,igroup,nset,vgroup1,vgroup2,vgroup3,vgroup4,vgroup5,group_editval1,group_editval2,&
			group_editval3,group_editval4,group_editval5,group_Toggle0,group_Toggle1,group_Toggle2,group_Toggle3,&
			group_Toggle4,group_Toggle5,inipage,onechan,setbad,chsvec,badend,&
			ns,ligname,igroup_text,nlig,vgroup4_1,group_editval4_1,igroup_text2)
		endif
	CASE(3101:3110) !(9701:9710)
				j=callid-3100 !9700
				istatus0=gmEnqToggleSwitch(group_Toggle0(j))
				if(istatus0.eq.gon) then
					onechan=.true.
				    tcrit(j)=3.1536e10
					call gmSetWidgetStatus(group_toggle5(j), Gchecked)
					call gmSetvaluesetting(group_editval1(j),3.1536e10)
					call gmSetWidgetStatus(group_editval1(j), Gunselectable)
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
					call gmSetvaluesetting(group_editval1(j),tcrit(j))
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
		case(3161) ! reading of changes in defgroup:conc,etc
	        j1=1
			j2=nset
			if(inipage.eq.-100.and.icallprev.eq.-162) then
				j1=ns
				j2=ns
			endif
			recres=0.
			do j=j1,j2
				vgroup1(j)=gmenqvaluesetting(group_editval1(j))
				vgroup2(j)=gmenqvaluesetting(group_editval2(j))
				vgroup3(j)=gmenqvaluesetting(group_editval3(j))
				if(inipage.eq.-100) then
					vgroup4(j)=gmenqvaluesetting(group_editval4(j))
					if(nlig.eq.2) vgroup4_1(j)=gmenqvaluesetting(group_editval4_1(j))
					vgroup5(j)=gmenqvaluesetting(group_editval5(j))
				!	call gmenqtextsetting(igroup_text,xtext1)
			
				endif
				tcrit(j)=vgroup1(j)
				tcbad(1,j)=vgroup2(j)
				tcbad(2,j)=vgroup3(j)
				istatus2=gmEnqToggleSwitch(group_Toggle2(j))
			
				if(istatus2.eq.gon) then
					setbad(1,j)=.true.
				else
					setbad(1,j)=.false.
				endif
				istatus3=gmEnqToggleSwitch(group_Toggle3(j))
			
				if(istatus3.eq.gon) then
					setbad(2,j)=.true.
				else
					setbad(2,j)=.false.
				endif
			    istatus4=gmEnqToggleSwitch(group_Toggle4(j))
				if(istatus4.eq.gon) then
					chsvec(j)=.true.
				else
					chsvec(j)=.false.
				endif
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
				
				
				istatus0=gmEnqToggleSwitch(group_Toggle0(j))
				if(istatus0.eq.gon) then
					onechan=.true.
					burst(j)=.false.
					chsvec(j)=.false.		!irrelevant in this case!
					badend(j)=.true.		!bad gap is valid end of group
					tcrit(j)=3.1536e10
				else		!msec=1 year! -see PRANGE
					burst(j)=.true. !!!!!!!!!!!!!!!1oare
				endif
			if(inipage.eq.-100) then
				conc(1,j)=vgroup4(j)/10**6
				if(icallprev.eq.-162) then
				call gmenqtextsetting(igroup_text,ligname(1))
			    models(jgraph)%ligname(1)=ligname(1)
				endif
				if(nlig.eq.2) then
					conc(2,j)=vgroup4_1(j)/10**6
				if(icallprev.eq.-162) then
				call gmenqtextsetting(igroup_text2,ligname(2))
				models(jgraph)%ligname(2)=ligname(2)
				endif
				endif
				
			endif
			prevtres=tresol(j)
			tresol(j)=vgroup5(j)
			dtres=abs(tresol(j)-prevtres)
			if(dtres.gt.0.01*tresol(j)) recres=-1.
			enddo	
				call gmremovewindow(igroup)
				if(recres.eq.-1)then
				    do j=1,nset
				        treso=0.001*tresol(j)
				        ! tresol(j)=1.e-3*tresol(j) !keep (possibly altered) res in mus for .ini !!!!!
				        tresg=treso
				        treso5=treso
				        tresg5=tresg
				        acrit5=acrit(j)
				        avamp5=avamp(j)
				        sim=simulat.or.autosim
				        call resolution2(tint0(1,j),tint(1,j),ampl0(1,j),ampl(1,j),&
     			        iprops0(1,j),iprops(1,j),nint(j),nintt(j),&
     			        ffilt(j),fc(j),rms(j),trise(j),nmax,nd1,index(1,j),&
     			        cjump,nsweep,kjumps0,kjumps,autosim,nsims,&
     			        sim,sres,sexp,excamp,alo(j),ahi(j),resdebug,iresnth)
				        tresol(j)=1.e3*treso	!keep (possibly altered) res in mus for .ini
				        tresd(j)=dble(treso)*1.0d-3 	!same as tresg (but in seconds)
				    enddo
				
				endif
				callid=3045
				goto 2
case(3045)
				do j=1,nset
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
			if(iopt.eq.1.and.(itogrec.eq.5.or.itogrec.eq.6).and.icallprev.ne.-152) then
			
			iexpr=-1
			callid=3165
			goto 2
			
			endif
			if(inipage.eq.-100.and.icallprev.eq.-162) then
			iexpr=-1
			callid=3165
			goto 2
			endif
			iexpr=0
			if(gaphi(1).eq.0.) then
				do j=1,nset
					gaplo(j)=tcrit(j)
					gaphi(j)=3.1536e10

				enddo
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
				call exclude_open(main,igroup,nset,vigroup1,vigroup2,igroup_editval1,igroup_editval2,&
				igroup_Toggle0,igroup_Toggle1)
				endif
			else
				iexpr=-1
				callid=3165
				goto 2
			endif
			
	
	case(3163)
		call gmremovewindow(igroup)	
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
			if(.not.nodata.and.(.not.autosim)) then
				do jset=1,nset
			
				call GETOPER(jset,tint,ampl,iprops,nint,nd1,nd2,iexcop,gaplo,gaphi,nskip)
				enddo
			endif
			if(discprt) write(7,138)
138			format(' ------------------------------------------------------------')
			do j=1,nset
			if(discprt) write(7,741) j
741			format(' Set ',i3)
			if(igraph2.gt.0) then
				nlig=models(igraph2)%nlig
				do jk=1,nlig
					ligname(jk)=models(igraph2)%ligname(jk)
				enddo
			endif
			do i=1,nlig
			if(discprt) write(7,761) ligname(i),conc(i,j)*1.e6
761			format('  Concentration of ',a10,' (micromolar) = ',g13.6)
			enddo
			enddo
			if(inipage.eq.-100) then
				if(icallprev.eq.-162.or.icallprev.eq.-152) then
					if(igraph2.gt.0) then
					    
						callid=-24
						
					else
						callid=-205
						
					!	igraph2=1
					endif
				else
					callid=-205
					!igraph2=1
				endif
				goto 2
			endif
			if(icalprev.eq.3005) then ! read from menu bar
				icalprev=0
				goto 1
			endif
			if(igraph2.gt.0) then	      
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
			
				if(indmod) then
				do i=1,k
				do j=1,k
				qt(i,j)=ratcons(indrat)%qt(i,j)
				enddo
				enddo
				endif
		
				
				if(discprt) write(7,138)
				if(nlig.gt.0) then
				if(discprt) write(7,332)
				call atypd(qt,'  QT   ',k,k,km,km)
				endif
				do j=1,2
				call gmSetWidgetStatus(eqfit(j), GSELECTABLE)

				enddo
				call gmSetWidgetStatus(view_record, GSELECTABLE)

				kAm=kA
				kFm=kF
			endif		

			if(autosim) then
				if(readini.and.ioptm.eq.1) then
					callid=-230
					goto 2
			
				endif
				call gmSetGuiGridMode(GOn)
				ksim=2
			
				ilasts=ilast
				jlasts=jlast
					
					!if(discprt) write(7,143)
					goto 8998
					itinfos=gmcreatetextentry(imainpanel,1,2,10,1,&
						'',&
						60, Gedit, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              			gmVpos=GTOP, gmExpand=GOFF)
				    itinfoP=gmcreatetextentry(imainpanel,12,2,12,1,&
						'TO PAUSE FIT/SIMULATION: Press "P" or "p"',&
						100, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              			gmVpos=GTOP, gmExpand=GOFF)
				    itinfoC=gmcreatetextentry(imainpanel,24,2,14,1,&
						'TO ABORT FIT/SIMULATION: Press "ESC" or "Q" or "q"',100, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              			gmVpos=GTOP, gmExpand=GOFF)
8998				if(discprt) write(7,143)
				!  ks=kAs+kBs+kCs+kDs
				kAs=models(jgraph)%kA 	!save values for 'true' model. and go round again for fit model
				kBs=models(jgraph)%kB
			
				ks=models(jgraph)%n
				 kcs=ks-kas-kbs
		
				callid=8022
				goto 2
			else
			if(readini.and.igraph2.gt.0) then
				imys=gmdisplaymessagebox('','Now start fitting (Menu bar:Fit)',&
				ginformation,gok)
			else
			if(ioptm.eq.3.or.ioptm.eq.4) then
				if(ioptm.eq.3) then
					callid=1100 !35 !new
					
					imodsav=0
					imod0=0	
					inipage=0
			        newmec=.true.
			        newcons=.true.
			        newmr=.true.
					goto 2
			   else if(ioptm.eq.4) then
					inipage=0
					callid=1200 !36  ! old
			!	call help_modify(main,imodhelp)
					imodsav=0
					imod0=0	
			
			
				    goto 2
				endif
			else if(ioptm.eq.2) then
				ixm=1
				call create_model(ixm,iym,igraph2,imodel,main,form1,form1_txtarray1,&
						graph1_2,GraphMainPanel1_2,graphics1_2,&
						nxmodel,nymodel,mtype,models,mod_create,irc,qmec,text7(i),&
						dxs,dys,ijmod,ka,kf,ic,indrat,pfilem,open7,append,ipbar_progress2,&
						indwin,ncdep,iformText,autosim,ksim,&
		initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
				imod0=imodel
					call read_rates(main,form1,Form1_TxtArray1,irecm0,imod,qmec,nrows,indrat,&
					ratcons,models,ic,ipbar_progress2,igraph2,nmr,conc,iformText)
					npar=models(igraph2)%npar
					goto 1	
			else if(ioptm.eq.1) then
					callid=-230
					goto 2
			
			endif
			endif
			endif
case(3039)
			if(igraph2.gt.0) then	      
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
			endif		
			call gmremovewindow(igroup)
				
			if(autosim) then
				if(readini.and.ioptm.eq.1) then
					callid=-230
					goto 2
			
				endif
			call gmSetGuiGridMode(GOn)
			ksim=2
			
			ilasts=ilast
			jlasts=jlast
					
					if(discprt) write(7,143)
					itinfos=gmcreatetextentry(imainpanel,1,2,10,1,&
						'',&
						60, Gedit, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              			gmVpos=GTOP, gmExpand=GOFF)
				itinfoP=gmcreatetextentry(imainpanel,12,2,12,1,&
						'TO PAUSE FIT/SIMULATION: Press "P" or "p"',&
						100, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              			gmVpos=GTOP, gmExpand=GOFF)
				itinfoC=gmcreatetextentry(imainpanel,24,2,14,1,&
						'TO ABORT FIT/SIMULATION: Press "ESC" or "Q" or "q"',100, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              			gmVpos=GTOP, gmExpand=GOFF)
				if(discprt) write(7,143)
				!  ks=kAs+kBs+kCs+kDs
				kAs=models(jgraph)%kA 	!save values for 'true' model. and go round again for fit model
				kBs=models(jgraph)%kB
			
				ks=models(jgraph)%n
				 kcs=ks-kas-kbs
				callid=8001
				goto 2
				endif
			if(readini.and.igraph2.gt.0) then
			imys=gmdisplaymessagebox('','Now start fitting (Menu bar:Fit)',&
			ginformation,gok)
			else
				if(ioptm.eq.3.or.ioptm.eq.4) then
				if(ioptm.eq.3) then
					callid=1100 !35 !new
					
					imodsav=0
					imod0=0	
					inipage=0
			        newmec=.true.
			        newcons=.true.
			        newmr=.true.
					goto 2
			   else if(ioptm.eq.4) then
					inipage=0
					callid=1200 !36  ! old
			!	call help_modify(main,imodhelp)
					imodsav=0
					imod0=0	
			
			
				    goto 2
				endif
				else if(ioptm.eq.2) then
				ixm=1
				call create_model(ixm,iym,igraph2,imodel,main,form1,form1_txtarray1,&
						graph1_2,GraphMainPanel1_2,graphics1_2,&
						nxmodel,nymodel,mtype,models,mod_create,irc,qmec,text7(i),&
						dxs,dys,ijmod,ka,kf,ic,indrat,pfilem,open7,append,ipbar_progress2,&
						indwin,ncdep,iformText,autosim,ksim,&
		initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
				imod0=imodel
					call read_rates(main,form1,Form1_TxtArray1,irecm0,imod,qmec,nrows,indrat,&
					ratcons,models,ic,ipbar_progress2,igraph2,nmr,conc,iformText)
					npar=models(igraph2)%npar
					goto 1	
				else if(ioptm.eq.1) then
					callid=-230
					goto 2
				endif
			endif

case(8001) ! simulate data
				ix1sav=ixr1	!keep random seeds at start of run
				iy1sav=iyr1
				iz1sav=izr1
			    onechan=.true.
			    nchan=1
			    kAs=models(jgraph)%kA 	!save values for 'true' model. and go round again for fit model
				kBs=models(jgraph)%kB
			    k=models(jgraph)%n
				ks=models(jgraph)%n
				kcs=ks-kas-kbs
				npars=models(jgraph)%npar
				if(idestf.eq.5) goto 1219
				itinfos=gmcreatetextentry(imainpanel,1,2,10,1,&
						'',&
						60, Gedit, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              			gmVpos=GTOP, gmExpand=GOFF)
				itinfoP=gmcreatetextentry(imainpanel,12,2,12,1,&
						'TO PAUSE FIT/SIMULATION: Press "P" or "p"',&
						100, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              			gmVpos=GTOP, gmExpand=GOFF)
				itinfoC=gmcreatetextentry(imainpanel,24,2,14,1,&
						'TO ABORT FIT/SIMULATION: Press "ESC" or "Q" or "q"',100, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              			gmVpos=GTOP, gmExpand=GOFF)
1219                continue
			    call HJCSIM2(main,thtrue,tint0,ampl0,iprops0,conc,nintt,&
				irates,jrates,npars,nmax,nset,dgammas,nsims,ks,imainpanel,&
				itinfos,ifirstsim,nchan)
				ifirstsim=1
				
				nls=len_trim(simfile3)
				indpc=6
				if(nls.lt.10) then
				    do nks=1,nls-3
				    if(simfile3(nks:nks).eq.'.') indpc=nks
				    enddo
				endif
				simdwt=simfile3(1:indpc-1)//'.dat'
				INQUIRE(file=simdwt,exist=present)
	
				if(.not.present) then
				    nsimsd=0
                    iverd=1
				    OPEN(unit=10,file=simdwt,status='UNKNOWN',access='DIRECT', form='BINARY',RECL=1)
	                issims=issims+1
	                write (unit=10,rec=1) iverd,issims
	                
	                    
	            else
	                ihandle=FILE$FIRST
					length = GETFILEINFOQQ(qfile, info, ihandle)
					nLEN=info%length 
					OPEN(unit=10,file=simdwt,status='UNKNOWN',access='DIRECT', form='BINARY',RECL=1)
					if(nlen.lt.5) then
					    nsimsd=0
                        iverd=1
                        issims=issims+1
					    write (unit=10,rec=1) iverd,issims
	                    
	                else  
	                    read (unit=10,rec=1) iverd,issims
	                    issims=issims+1
	                endif   
	            endif
	            call writeseeds(simfile3,issims,nset,nlig,ks,npars,imod0,ix1sav,iy1sav,iz1sav,conc)
 
	         
 				do j=1,nset
				call intconv(j,cnum5)
				call realtoch(conc(1,j),cnum0,11)
				filasc=simfile3(1:indpc-1)//cnum5
				nlf=len_trim(filasc)
				filasc=filasc(1:nlf)//'.dwt'
				if(simwascii) then
				   treso=1.e-3*resw(j)
				  
				    tresg=treso
				    treso5=treso
				    tresg5=tresg
				        
				    acrit5=acrit(j)
				    avamp5=avamp(j)
				    sim=simulat.or.autosim
				    if(discprt) write(7,505)
505				    format(' Resolution applied for MIL',/)
				    call resolution2(tint0(1,j),tint(1,j),ampl0(1,j),ampl(1,j),&
     			    iprops0(1,j),iprops(1,j),nint(j),nintt(j),&
     			    ffilt(j),fc(j),rms(j),trise(j),nmax,nd1,index(1,j),&
     			    cjump,nsweep,kjumps0,kjumps,autosim,nsims,&
     			    sim,sres,sexp,excamp,alo(j),ahi(j),resdebug,iresnth)
				    samy1=resw(j)*1.e-5
                    isamy1=0
                    samy2=0.2
                    isamy2=2
                    isamy3=1
                    indkl=nint(j)
                    if(tint(indkl,j).lt.0.0) indkl=indkl-1
				    OPEN(unit=18,file=filasc,status='UNKNOWN',&
				    access='APPEND',form='FORMATTED',carriagecontrol='FORTRAN',iostat=i_var)
                    write(18,8007) 'Segment:',issims,'Dwells:',indkl,'Sampling(ms):',samy1,&
                    'Start(ms):',isamy1,'ClassCount:',isamy2,isamy1,samy2,isamy3,samy2
                    do kl=1,indkl
                        iampli=0
                        if(ampl(kl,j).gt.0.0) iampli=1
                        write(18,802) iampli,char(9),tint(kl,j)  
                    enddo
                    close(18)
                    
             	endif
				goto 7891    
				treso=0.001*tresol(j)
				! tresol(j)=1.e-3*tresol(j) !keep (possibly altered) res in mus for .ini !!!!!
				tresg=treso
				treso5=treso
				tresg5=tresg
				acrit5=acrit(j)
				avamp5=avamp(j)
				sim=simulat.or.autosim
				call resolution2(tint0(1,j),tint(1,j),ampl0(1,j),ampl(1,j),&
     			iprops0(1,j),iprops(1,j),nint(j),nintt(j),&
     			ffilt(j),fc(j),rms(j),trise(j),nmax,nd1,index(1,j),&
     			cjump,nsweep,kjumps0,kjumps,autosim,nsims,&
     			sim,sres,sexp,excamp,alo(j),ahi(j),resdebug,iresnth)
				tresol(j)=1.e3*treso	!keep (possibly altered) res in mus for .ini
				tresd(j)=dble(treso)*1.0d-3 	!same as tresg (but in seconds)
               
7891			continue	
                enddo
8007            format(a9,i4,1x,a7,1x,i8,1x,a12,1x,f10.5,1x,a10,1x,i1,1x,a11,1x,i1,1x,i1,1x,f4.2,1x,i1,1x,f4.2)	
802             format(i2,a1,f15.7)
                if(simwascii) then
                
                write (unit=10,rec=1) iverd,issims
                close(unit=10)
                endif
               
				if(idestf.eq.5) then
				
				    do j=1,nset
				        treso=0.001*tresol(j)
				        ! tresol(j)=1.e-3*tresol(j) !keep (possibly altered) res in mus for .ini !!!!!
				        tresg=treso
				        treso5=treso
				        tresg5=tresg
				        acrit5=acrit(j)
				        avamp5=avamp(j)
				        sim=simulat.or.autosim
				        call resolution2(tint0(1,j),tint(1,j),ampl0(1,j),ampl(1,j),&
     			        iprops0(1,j),iprops(1,j),nint(j),nintt(j),&
     			        ffilt(j),fc(j),rms(j),trise(j),nmax,nd1,index(1,j),&
     			        cjump,nsweep,kjumps0,kjumps,autosim,nsims,&
     			        sim,sres,sexp,excamp,alo(j),ahi(j),resdebug,iresnth)
				        tresol(j)=1.e3*treso	!keep (possibly altered) res in mus for .ini
				        tresd(j)=dble(treso)*1.0d-3 	!same as tresg (but in seconds)
				    enddo
					apfile=.true.
					initmec=-1
					plot=.false.
					models(jgraph)%ix=-100
					models(jgraph)%ix=ixmm
					callid=2040
					isetu=7
					isetu0=isetu
					inipage=-100
					callid=4001
					goto 2
				else
				    callid=3303
				    goto 2
				endif
                
case(8022)
				    radio_text(1)='Use the same as for simulation'
				    radio_text(2)='Use different mechanism'
				    titlerp='Define mechanism to be fitted:'
				    call radio_panel(main,iradio,2,iradio_toggle,radio_text,0,&
					iradiox,radiox,iradioy,radioy,-43,titlerp)
                				
	
    case(-43)
			imtg=gmenqtoggleswitch(iradio_toggle(1))
			call gmremovewindow(iradio)
			
			if(imtg.eq.0) then
			
			
				if(ioptm.eq.3) then
					callid=1100 !35 !new
					newmec=.true.
					newcons=.true.
			        newmr=.true.
					goto 2
			    else if(ioptm.eq.4) then
	
					callid=1200 !36  ! old
			!	call help_modify(main,imodhelp)
			
				    goto 2
				endif
				
			else
						OPEN(unit=15,file=qmec,status='UNKNOWN',access='DIRECT', form='BINARY', RECL=1)
						read(unit=15,rec=1) iver,nrecs,nextrec,ireclast,jstart
						imod=imodel
						lt=0
						do k=1,nrecs
							irec=jstart(k)
							read(unit=15,rec=irec) iver4,imod4,mtitle4,k4,kA4,kB4,kC4,kD4,rtitle4
							if(imod4.eq.imod) then
								
								lt=lt+1
							
								call intconv(k,cnum5)
								radio_text(lt)=cnum5(1:3)//':'//rtitle4
								indrec(lt)=k
								
							endif
						enddo
						close(unit=15)
						iarca=-1
						titlerp='Titles for stored rate constants'
						call radio_panel(main,iradio,lt,iradio_toggle,radio_text,0,&
						iradiox,radiox,iradioy,radioy,-44,titlerp)
			endif
	
	case (8002) !simulate data
			call gmremovewindow(iradio)
			if(ioptm.eq.3) then
			        newmec=.true.
			        newcons=.true.
			        newmr=.true.
					callid=1100 !35 !new
					goto 2
			    else if(ioptm.eq.4) then

					
					callid=1200 !36  ! old
!				call help_modify(main,imodhelp)
			
				    goto 2
				endif
			

! view parameters
!==========================================================================
	
case(3501)	! show parameters	
		  if(PROGRAM_TYPE.EQ.3) then
		 icall=-154
		 if(autosim) then
		 do j=1,nset
			tedit1(1,j)=simfile1
			tedit2(1,j)=cDATEW
			
			tval6(1,j)=avamp(j)
		
			tval9(1,j)=conc(1,j)*10**6
		enddo
		endif
	
     	 call hjcfit_table(main,hjcfitform,nset,nfile,pfiles,tedit1,tedit2,&
			tedit3,tedit4,tval1,tval2,tval3,tval4,tval5,tval6,tval7,tval8,tval9,tvalt,tedit5,&
			fc,ffilt,trise,autosim,icall,val9,idatyp)
		 
		  endif

case(3511) ! stability plots
case(3611) !stability plots

! FITTING
!===================================================================================
	

case(4001) ! prepare for fitting
		

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
				titpfree(ik)='          '
				ich=ichar(titpfree(ik)(7:7))
				titpfree(ik)=titlep(m)
				jfix1(ik)=0
			endif
			endif
		enddo
	!	if(icprev.eq.-102) then
	!		callid=4002
	!		goto 2
	!
	!	endif
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
			itypeval(i)=0
			enddo
			nmod=imod0
	
	 if(liksurf) then
        do m=1,npar
	        
	         WRITE(STRING,fmt='(i5,2x,a10,g18.6,5x,a10)') m,ratcons(indk)%qij(m),&
	         theta0(m),ratcons(indk)%titlep(m)(1:10)
	         
	
	        call gmsettextsetting(itty,string)
	    enddo
        callid=8049
        goto 2
        endif
	if(idestf.eq.5) then
		initi=0
		! idestf=0
		callid=4020
		 goto 2
		 
	else
	call gmDefineKeySelectCallback(13,4002)
		call value_table(Main,ivwin,6,vtext,ival,val,4002,itypeval)
	endif
	case(4002)
	!	call gmsetwidgetstatus(intoggle(6),gchecked)
       !  if(icprev.ne.-102) 
call gmDefineKeySelectCallback(13,0)
		 call gmRemoveWindow(ivwin)
		 if(discprt) then
			write(7,776) npar,nfix,neq,nmr,nfixec50,kfit

		 endif
776	format(' Total number of rates = ',i5,/,' Number that are fixed       = ',i5,/,&
		' Number that are constrained = ',i5,/,' Number set by micro rev     = ',i5,/,&
' Number set by fixed EC50    = ',i5,/,' Number of free rates to be estimated = ',i5,/)
		 kab=1
		 if(.not.allocated(alpha2)) allocate(alpha2(kab),beta2(kab),aloglik(kab))
		 if(.not.allocated(Z00A)) then
		        irt=max(kam,kfm)
!		        ALLOCATE(Z00A(irt,irt,km),Z10A(irt,irt,km),Z11A(irt,irt,km))
!				ALLOCATE(Z00F(irt,irt,km),Z10F(irt,irt,km),Z11F(irt,irt,km))
!				ALLOCATE(XAF(irt,irt,irt),XFA(irt,irt,irt),QEXPQA(irt,irt),QEXPQF(irt,irt))
				
				ALLOCATE(Z00A(kAm,kFm,km),Z10A(kAm,kFm,km),Z11A(kAm,kFm,km))
				ALLOCATE(Z00F(kFm,kAm,km),Z10F(kFm,kAm,km),Z11F(kFm,kAm,km))
				ALLOCATE(XAF(kAm,kFm,kAm),XFA(kFm,kAm,kFm),QEXPQA(kFm,kAM),QEXPQF(kAm,kFm))
				allocate(EXPQA(20,20),EXPQF(100,100))
		 endif
		      !====================later	
			  infoind=infoind+1
		
			itxtinfo=gmcreatetextentry(infopanel,1,2,5,1,&
			'Mechanism ',32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						 infoind=infoind+1
		

			itxtinfo=gmcreatetextentry(infopanel,6,2,8,1,&
			'File:'//qmec(1:40),32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						 infoind=infoind+1


			itxtinfo=gmcreatetextentry(infopanel,14,2,14,1,&
			'Name:'//models(igraph2)%title_model,32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						 infoind=infoind+1
		
			itxtinfo=gmcreatetextentry(infopanel,28,2,12,1,&
			'Rate title:'//ratcons(indk)%title,32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
			infoind=infoind+1

			call intconv(npar,cstring)
			nlv=len_trim(vtext(1))
			itxtinfo=gmcreatetextentry(infopanel,6,3,7,1,&
			vtext(1)(1:nlv)//cstring(1:3),32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
			nlv=len_trim(vtext(2))		
			infoind=infoind+1
			
			call intconv(nfix,cstring)
			itxtinfo=gmcreatetextentry(infopanel,13,3,7,1,&
			vtext(2)(1:nlv)//cstring(1:3),32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
			infoind=infoind+1
			nlv=len_trim(vtext(3))
			call intconv(neq,cstring)
			itxtinfo=gmcreatetextentry(infopanel,20,3,8,1,&
			vtext(3)(1:nlv)//cstring(1:3),32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
			
		
			nlv=len_trim(vtext(4))
			infoind=infoind+1
			call intconv(nmr,cstring)
			itxtinfo=gmcreatetextentry(infopanel,28,3,8,1,&
			vtext(4)(1:nlv)//cstring(1:3),32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
			goto 3233
			infoind=infoind+1
			nlv=len_trim(vtext(5))
			call intconv(nfixec50,cstring)
			itxtinfo=gmcreatetextentry(infopanel,1,infoind,12,1,&
			vtext(5)(1:nlv)//cstring(1:3),32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
			
			infoind=infoind+1
			call intconv(kfit,cstring)
			nlv=len_trim(vtext(6))
			itxtinfo=gmcreatetextentry(infopanel,1,infoind,12,1,&
			vtext(6)(1:nlv)//cstring(1:3),32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
		
3233    continue
!		penfac=10.0d0
		
		if(initi.eq.1.or.icprev.eq.-102) then
		 callid=4020
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
!			jkstatus=gmdisplaymessagebox('','OK.Proceed with MR/Fitting',ginformation,gok)
		
		

		 callid=4011
		 goto 2
	case(4011:4429)	!fitting
		first=.true.
			nmod=imodel
			nmod9=imodel
			ifirst3=-1
	ifirst4=-1
	ylo3=0.025
	yhi3=0.100
	nligsav=nlig
	ich=ichar(titpfree(1)(7:7))
		ka=models(igraph2)%ka
	    k=models(igraph2)%n
	    kf=k-ka
	    kc=kf-kb
	    kam=ka
	    kfm=kf
	
! Modified 21-04-07, remove covar etc from arguments
	call fitting(callid,igraph2,main,imainpanel,ivwin9,iynwin,Form1,Form1_TxtArray1,thetaf,jfix1,tint,ampl,iprops,&
			Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,XAF,XFA,QexpQA,QexpQF,&
			alpha2,beta2,aloglik,kab,kAm,kFm,km,irt,nset,idestf,eqfit,nsim,&
			endsim,neq,nsetq,thsav,titlep,infopanel,infoind,infolength,&
			indk,ratcons,miplot0,mtoggle,tresol,iplot_Toggle0,elmax,&
			nev,smin,inifile,inifile0,pfileb,pfiles,qfilem,qmec,nsims,thtrue,&
			simfile,simfile1,cDATEW,ctimew,npars,mtitles,imod0,imods,nevalsn,&
			ix1sav,iy1sav,iz1sav,nsims0,apfile,stpfac,errfac,delmin,confac,irestrt,resfac,&
	        iconv,ipl1,ipl2,ipl3,ipl4,ipl5,ipl6,icprev,isw,titpfree,iptit,EXPQF,EXPQA,&
	        qt,qd,liksurf,simwascii)
			ifit=0
			if(callid.eq.4406) then
			ifit=-1
		!	irecmf=-1
		    irecmf=irecm0
			callid=4431
			goto 2
				
			endif
		   
		   if(callid.eq.-148) goto 2
		   
		    first=.false.
			plot=.false.
			infx=28
						info_pan(8)=gmCreatePanel(imainpanel, infx, 1, 4, 1,gmtitle=text_tog(8), &
              			gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmborderType=GINVERSECHISEL, &
              			gmLineCol=0, gmLineBorder=0, gmFillCol=221, gmFillBorder=0)
						ict=4
					!	if(i.eq.2.or.i.eq.4.or.i.eq.5.or.i.eq.7) then
						itextinfo=gmCreateTextEntry(info_Pan(8), 0, 0, 4, 1,text_tog(8), 255, GDISPLAY, gmBack1Col=0, &
						gmBack2Col=12, gmTextCol=1 , gmVpos=GTOP,gmsize=80)
		

		 if(idestf.eq.2.or.idestf.eq.3.) then
		
			nlig=models(jgraph)%nlig
			do i=1,nlig
				ligname(i)=models(jgraph)%ligname(i)
			enddo
			!imess=gmdisplaymessagebox('','Estimate errors for parameters before plotting?',gquestion,gyesno)
		    if(idestf.eq.3) then
				callid=4450
				plotdone=.false.			
				goto 2
			else
				plotdone=.true.
				title1='  '
				call gmDefineKeySelectCallback(13,4509)
				call main_plots(main,miplot0,mtoggle,1,1,iplot_Toggle0,nlig,ligname,conc,nset)
			endif

		 
		 else if(idestf.eq.5) then

			callid=8001
			goto 2
		 else if(idestf.eq.10) then
		 	OPEN(unit=10,file=simfile1,status='UNKNOWN',&
     				access='DIRECT', form='BINARY',RECL=1)
			read(unit=10,rec=1) iver1,nsims1
			if(iver1.le.101) then
					read(unit=10,rec=1) iver1,nsims1,ABORTW,npar,nset
							
			else if(iver1.le.106) then
					read(unit=10,rec=1) iver1,nsims1,npar,nset
						
			endif
						
			close(unit=10)

			callid=6001
			goto 2
		else if(idestf.eq.6) then

			callid=8049
			goto 2
		 endif
case(4430)
	irecmf=irecm0
	callid=4431
	goto 2
case(4432)
	irecfm=-1
	callid=4431
	goto 2
case(4431,4433,4434)
	if(callid.eq.4433) then
	do j=1,models(igraph2)%npar
		call gmEnqCellSetting(Form1_TxtArray1(igraph2,4), 3,j,ratcons(indrat)%value(j),actext)
    enddo
	else if(callid.eq.4434) then
	do j=1,models(igraph2)%npar
		call gmEnqCellSetting(ini_TxtArray7, 3,j,ratcons(indrat)%value(j),actext)
    enddo
	endif
				efile=qmec

				efilt='*.mec'//char(124)//'Mechs.mec'//char(124)//&
				'*.dat'//char(124)//'Old files'//char(124)//&
			   '*.*'//char(124)//'All Files'
				CALL gmFileBROWSER(eFILE,eDIR,eFILT,gmBrowseType=1)
		
				IF(eFILE.ne.' ') then
				nb=len_trim(edir)
					efile=edir(1:nb)//'\'//efile
				    imod=imod0
					call write_model(main,imod,models,indrat,ratcons,irecmf,efile,pfilem,&
					igraph2,irec,theta0,nvdep,ifit,ncyc0,imodmf)
					!!irecq=irecm0
					ifit=0	
				endif
				efilt='*.plq'//char(124)//'Plot Queue (PLQ)'//char(124)//&
			   '*.*'//char(124)//'All Files'
			   qmec=efile
			   irecq=irecmf
			   nb=len_trim(efile)
			   nb1=len_trim(edir)
			   qfilem=efile(nb1+1:nb)
			   imod0=imodmf
			   imod=imod0
			   icalprevm=-9
			goto 1
case(4450) ! calculate errors

    
	   if(allocated(QTsav)) DEALLOCATE(QTsav)
	   if(allocated(Qdsav)) DEALLOCATE(QDsav)
	    if(allocated(thsav1)) DEALLOCATE(thsav1)
	   ALLOCATE(QTsav(100,100),QDsav(100,100),thsav1(200))
	  
	   do i=1,k
		thsav1(i)=theta0(i)
		do j=1,k
		    QTsav(i,j)=QT(i,j)
		    QDsav(i,j)=QD(i,j)
		enddo
	   enddo
	itype(1)=3
	val(1)=0.1
	vtext(1)=' For Hessian use step size that decreases log(lik) by'
	
	call val_table(Main,ivwin9,1,vtext,ival,val,4451,itype,iplot_Toggle0)

case(4451) !calculate errors
		val(1)=gmenqvaluesetting(ival(1))
		if(val(i).le.0.0) val(i)=0.1
		deltam=val(1)
		call realtoch(deltam,cnum(1),11)
		call gmremovewindow(ivwin9)
		string=' For Hessian use step size that decreases log(lik) by delta ='//cnum(1)
		call gmsettextsetting(itty,string)
		call gmflushcallbackqueue()

! 21-4-07 make allocatable covar,covlog,var,varlog,badpar
		k=kA+kF		!in case not defined
		if(.not.allocated(covar)) then
			allocate(covar(npar,npar),covlog(npar,npar))
			allocate(var(npar),varlog(npar))
			allocate(badpar(npar))
		endif
	    call HMAT_HJC(thetaf,var,elmax,kfit,badpar,kgood,covar,covlog,varlog,npar,tint,&
	   ampl,iprops,nd1,nd2,Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,XAF,XFA,QexpQA,QexpQF,&
		alpha2,beta2,aloglik,kab,kAm,kFm,km,deltam,irt,EXPQF,EXPQA,qt,qd)

	   do i=1,k
		theta0(i)=thsav1(i)
		do j=1,k
		    QT(i,j)=QTsav(i,j)
		    QD(i,j)=QDsav(i,j)
		enddo
	   enddo

	   i1=0
	   i2=0
	   do m=1,npar
		jcov(m)=-1
		jbad(m)=0
		sd(m)=-1.0		!unless good value defined
		sdlog(m)=-1.0		!unless good value defined
		if(jcon(m).eq.0.and.jmic(m).eq.0.and.jfix(m).eq.0) then	!free
		   if(.not.(fixec50.and.m.eq.m50)) then		!free
			i1=i1+1
	      	if(badpar(i1)) jbad(m)=1
			if(.not.badpar(i1)) then
			   i2=i2+1
			   if(var(i2).gt.0.0d0) then
				sd(m)=sngl(dsqrt(var(i2)))
				sdlog(m)=sngl(dsqrt(varlog(i2)))
				jcov(m)=i2
			   endif
			endif
		   endif
		endif
	   enddo
!tty
	   if(discprt) write(7,37111)
37111	   FORMAT(/,' Correlation matrix= ')
	
	if(discprt) then
	   npr=10
	   ir=0
	   m=0
	   do j=1,npar
		if(jbad(j).eq.0.and.sd(j).gt.0.0) then
		   m=m+1	
		   if(m.eq.(npr*ir+1)) then
				write(7,58) titlep(j)(1:6)
				ir=ir+1
		   else	
				write(7,57) titlep(j)(1:6)
		   endif	
		endif
	   enddo
58	  format(/,12x,a6,$)
57    format(2x,a6,$)
56    format(/,2x,a6,$)
	  ii=0
	  do j=1,npar
		   if(jbad(j).eq.0.and.sd(j).gt.0.0) then
		   
				write(7,56) titlep(j)(1:6)
				ir=0
				m=0
				ii=ii+1
				jj=0
				do i=1,npar
					if(jbad(i).eq.0.and.sd(i).gt.0.0) then
					    
						jj=jj+1
						ci=covar(ii,ii)
						cj=covar(jj,jj)
						if(ci.le.1.d300/cj.and.ci.gt.1.d-300/cj) then
			 				den=dsqrt(covar(ii,ii)*covar(jj,jj))
							if(den.gt.1.d-30) then
								cor=covar(ii,jj)/den
				 
							else
				  
								cor=-1.
							endif
						else
				
								cor=-1.
						endif
						m=m+1	
						if(m.eq.(npr*ir+1)) then
							if(cor.ne.-1) then
								write(7,59) cor
							else
								write(7,61)
							endif
							ir=ir+1
						else	
							if(cor.ne.-1) then
								write(7,60) cor
							else
								write(7,62)
							endif
						endif	
					endif
				enddo
				
			endif
	 	
	  enddo
59	  format(\,4x,f6.3,$)
60    format(2x,f6.3,$)
61    format(\,4x,'n.d.',$)
62    format(2x,' n.d.',$)
	endif

	
         if(discprt) write(7,1373) nev,-smin
1373	   format(/,/,' FINAL VALUES OF RATE CONSTANTS WITH ERRORS',/,&
     ' Number of evaluations = ',i8,' Max log(likelihood) = ',g14.7)
	   do j=1,nset
		tres1=1.d6*tresd(j)
		if(nodata) then
		   burst(j)=.false.
		   chsvec(j)=.false.
	      endif
      	
		if(discprt) write(7,350) j
350		format(/,' Set ',i3)
		if(burst(j)) then
		   do i=1,nlig
			
			if(discprt) write(7,335) conc(i,j)*1.e6,ligname(i)
		   enddo
335			format(3x,g13.6,' micromolar of ',a20)
		   if(discprt) write(7,351) tcrit(j),tres1
351		   format('    Analysed in bursts, tcrit (ms) = ',g13.6,/,'    Resolution (microsec) = ',g13.6)
		else
		   do i=1,nlig
			
			if(discprt) write(7,335) conc(i,j)*1.e6,ligname(i)
		   enddo
		   
      	   if(discprt) write(7,341) tres1
341		 format('    Analysed as if from one channel only',/,&
		  '    Resolution (microsec) = ',g13.6)

		endif
		if(logsav) then
		     	   if(discprt) write(7,3411)
3411	format(' Simplex used log(rate constant) for searching')
		endif
		if(setbad(1,i)) then
      	   if(discprt) write(7,342) ncbad(1,i),tcbad(1,i)
		endif
342		 format('      ',i5,' shuttings longer than ',g13.6,' ms set bad')
		if(setbad(2,i)) then
      	   if(discprt) write(7,343) ncbad(2,i),tcbad(2,i)
		endif
343		format('      ',i5,' openings longer than ',g13.6,' ms set bad')
		if(iexcop(j).eq.1) then
		   if(discprt) write(7,1311) j,nskip(j),gaplo(j),gaphi(j)
		else if(iexcop(j).eq.2) then
		   if(discprt) write(7,1312) j,nskip(j)
	   endif
1311	format(/,' SET ',i3,/,   1x,i5,' open periods excluded because shut time on BOTH sides',&
		/,'  is between ',g13.6,' and ',g13.6,' ms')
1312	 format(/,' SET ',i3,/,&
        1x,i5,' open periods excluded because they are in group with',&
     	/,' only one open period')
	   enddo
!tty !!!!
	   if(.not.autosim.or.(autosim.and.imod0.ne.imods)) then
      	if(discprt) write(7,30)
30		format(/,23x,'  initial         final       approx SD       CV(%)')
	   else
      
      	if(discprt) write(7,31)
31		format(/,33x,'  true        initial        final      approx SD       CV(%)')
	   endif
	   do m=1,npar
		i=irate(m)
		j=jrate(m)
		theta0(m)=QT(i,j)
		cv=100.*sd(m)/sngl(theta0(m))
		if(jbad(m).eq.0.and.sd(m).gt.0.0) then
		 if(.not.autosim.or.(autosim.and.imod0.ne.imods)) then
	         if(discprt) write(7,32) m,i,j,titlep(m),thsav(m),theta0(m),sd(m),cv
32		   format(i3,1x,' q(',i2,',',i2,')=',1x,a10,t25,g13.6,t38,g13.6,t51,g13.6,t65,f8.2)
		 else
       	   if(discprt) write(7,35) m,i,j,titlep(m),thtrue(m),thsav(m),theta0(m),sd(m),cv
35	 	   format(i3,1x,' q(',i2,',',i2,')=',1x,a10,t25,g13.6,t38,g13.6,t51,g13.6,t65,g13.6,f8.2)
		 endif
		else if(jbad(m).eq.1) then
		 if(.not.autosim.or.(autosim.and.imod0.ne.imods)) then
	         if(discprt) write(7,36) m,i,j,titlep(m),thsav(m),theta0(m)
36		   format(i3,1x,' q(',i2,',',i2,')=',1x,a10,t25,g13.6,t38,g13.6,t54,'  not def   ')
!		  Print 'not def' only if can't get 2nd deriv for Hessian
		 else
      	   if(discprt) write(7,38) m,i,j,titlep(m),thtrue(m),thsav(m),theta0(m)
38	 	   format(i3,1x,' q(',i2,',',i2,')=',1x,a10,3g13.6,t54,' not def  ')
		 endif
!		if constrained, print the error for the value that is constrained

		else
		 if(.not.autosim.or.(autosim.and.imod0.ne.imods)) then
	         if(discprt) write(7,361) m,i,j,titlep(m),thsav(m),theta0(m)
361		   format(i3,1x,' q(',i2,',',i2,')=',1x,a10,t25,g13.6,t38,g13.6,t54,'  -   ',$)
!		for rates that are constrained or fixed print '-'
		 else
		    if(discprt) write(7,381) m,i,j,titlep(m),thtrue(m),thsav(m),theta0(m)
381	 	   format(i3,1x,' q(',i2,',',i2,')=',1x,a10,3g13.6,t65,'  -   ',$)
		 endif
		endif
		if(jmic(m).eq.1) then
      	   if(discprt) write(7,1212)
1212		   format('$(micro-rev)',/)
		else if(jcon(m).eq.1) then
      	   if(discprt) write(7,1213)
1213		   format('$(constrained)',/)
		 else if(fixec50.and.m.eq.m50) then
	         if(discprt) write(7,1215) 1.d6*ec501
1215		   format('$from EC50=',f8.3,' muM)',/)
		 else if(fixec50.and.m.eq.m502) then
	         if(discprt) write(7,1215) 1.d6*ec502
!1215		   format('$from EC50=',f8.3,' muM)',/)
		else if(jfix(m).eq.1) then
      	   if(discprt) write(7,1214)
1214		   format('$(fixed)',/)
		endif
	   enddo

! Print estimates and SD
	   first=.true.	 !calc covar(rate); when false, calc covar(log(rate))
	   if(.not.autosim.or.(autosim.and.imod0.ne.imods)) then
      	if(discprt) write(7,301)
301		format(/,' Errors from log(rate) means that 2nd derivs in Hessian',/,&
        ' are multiplied by theta(i)*theta(j), to get 2nd deriv',/,' with respect to log(rate)',/,/,&
          '  FINAL VALUES WITH ERRORS FROM log(rate)',/,23x,&
          '  initial         final         CV(%)       SD')
	   else
      	
      	if(discprt) write(7,311)
311	     format(/,' Errors from log(rate) means that 2nd derivs in Hessian',/,&
         ' are multiplied by theta(i)*theta(j), to get 2nd deriv',/,&
         ' with respect to log(rate)',/,/,&
         '  FINAL VALUES WITH ERRORS FROM log(rate)',/,23x,&
          '  true        initial        final         CV(%)     SD')
	   endif
	   do m=1,npar
		i=irate(m)
		j=jrate(m)
		theta0(m)=QT(i,j)
		if(jmic(m).eq.1) then	!calc CV for MR rates
!		   ktd=200
! npar substituted for ktd, for dimension of covlog
		    call MRVAR(i,j,covlog,jcov,kgood,obeymr,IQ,cvmr,npar)
			sdlog(m)=cvmr
		endif
		   

		if(jbad(m).eq.0.and.sdlog(m).gt.0.0) then
		 cv=100.*sdlog(m)
		 sx=sdlog(m)*sngl(theta0(m))
		 if(.not.autosim.or.(autosim.and.imod0.ne.imods)) then
	         
	         if(discprt) write(7,313) m,i,j,titlep(m),thsav(m),theta0(m),cv,sx
313		   format(i3,1x,' q(',i2,',',i2,')=',1x,a10,t25,g13.6,t38,g13.6,t51,g13.6,t65,g13.6)
		 else
      	   if(discprt) write(7,315) m,i,j,titlep(m),thtrue(m),thsav(m),theta0(m),cv,sx
315	 	   format(i3,1x,' q(',i2,',',i2,')=',1x,a10,t25,g13.6,t38,g13.6,t51,g13.6,t65,f8.2,g13.6)
		 endif
		else if(jbad(m).eq.1) then
		 if(.not.autosim.or.(autosim.and.imod0.ne.imods)) then
	         if(discprt) write(7,36) m,i,j,titlep(m),thsav(m),theta0(m)
		 else
      	   if(discprt) write(7,38) m,i,j,titlep(m),thtrue(m),thsav(m),theta0(m)
		 endif
		else
		 if(.not.autosim.or.(autosim.and.imod0.ne.imods)) then
	         if(discprt) write(7,361) m,i,j,titlep(m),thsav(m),theta0(m)
		 else
      	   if(discprt) write(7,381) m,i,j,titlep(m),thtrue(m),thsav(m),theta0(m)
		 endif
		endif
		if(jmic(m).eq.1) then
      	   if(discprt) write(7,1212)
!1212		   format('&(micro-rev)',/)
		else if(jcon(m).eq.1) then
      	   if(discprt) write(7,1213)
!1213		   format('&(constrained)',/)
		 else if(fixec50.and.m.eq.m50) then
	         if(discprt) write(7,1215) 1.d6*ec501
!1215		   format('&(from EC50=',f8.3,' muM)',/)
		 else if(fixec50.and.m.eq.m502) then
	         if(discprt) write(7,1215) 1.d6*ec502
!1215		   format('&(from EC50=',f8.3,' muM)',/)
		else if(jfix(m).eq.1) then
      	   if(discprt) write(7,1214)
!1214		   format('&(fixed)',/)
		endif
	   enddo

 		if(allocated(covar)) then
			deallocate(covar,covlog)
			deallocate(var,varlog)
			deallocate(badpar)
		endif

!  Repeat with direct calculation of COVLOG as test
	   if(first) then
		first=.false.
		textyn(1)='Check errors by direct calculation from log(rates)'
		 n=1
		 iyesno(1,1)=0
		 iyesno(1,2)=1
		 call gmDefineKeySelectCallback(13,4459)
		 call yesno_table(main,iynwin,n,textyn,iyes,iyn,iyesno,4459)

		else
			callid=4456
			goto 2
		endif
case(4459) 
		call gmDefineKeySelectCallback(13,0)
	    istat1=gmenqtoggleswitch(iyes(1))
		call gmremovewindow(iynwin)
		if(istat1.eq.gon) then	 
		 	itype(1)=3
			val(1)=0.1
			vtext(1)=' For Hessian use step size that decreases log(lik) by delta'	
			call val_table(Main,ivwin9,1,vtext,ival,val,4455,itype,iplot_Toggle0)		
		else
			callid=4456
			goto 2
		endif
case(4455) !calculate errors using log rates directly
		val(1)=gmenqvaluesetting(ival(1))
		if(val(i).le.0.0) val(i)=0.1
		deltam=val(1)
		call gmremovewindow(ivwin9)
		call realtoch(deltam,cnum(1),11)
		
		string=' For Hessian use step size that decreases log(lik) by delta ='//cnum(1)
		call gmsettextsetting(itty,string)
		call gmflushcallbackqueue()
! 21-4-07 make allocatable covar,covlog,var,varlog,badpar
		k=kA+kF		!in case not defined
		if(.not.allocated(covlog)) then
			allocate(covlog(npar,npar))
			allocate(varlog(npar))
			allocate(badpar(npar))
		endif
		call HMAT_LOG(thetaf,varlog,elmax,kfit,badpar,kgood,&
      	   COVLOG,tint,ampl,iprops,nd1,nd2,&
      	   Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,XAF,XFA,QexpQA,QexpQF,&
      	   alpha2,beta2,aloglik,kab,kAm,kFm,km,deltam,irt,EXPQF,EXPQA,qt,qd)
		if(discprt) write(7,48)
48		   format(/,' Approx CV (%) calc directly from log(rates)')
		   do i=1,kgood
			if(varlog(i).gt.0.0d0) then
			   sdlog1(i)=sngl(dsqrt(varlog(i)))
			   if(discprt) write(7,40) i,100.*sdlog1(i)
40			   format(' ',i3,' CV (%) = ',f9.3)
			endif
		   enddo
 		if(allocated(covar)) then
			deallocate(covlog)
			deallocate(varlog)
			deallocate(badpar)
		endif
		
	   	callid=4456
		goto 2

case(4456) !calculate errors
	   DEALLOCATE(QTsav,QDsav,thsav1)
	  
	  textyn(1)='Another error calculation?'
		 n=1
		 iyesno(1,1)=0
		 iyesno(1,2)=1
		 call gmDefineKeySelectCallback(13,4461)


		  call yesno_table(main,iynwin,n,textyn,iyes,iyn,iyesno,4461)
	
       
	
		
		
case(4461) 
		call gmDefineKeySelectCallback(13,0)
	    istat1=gmenqtoggleswitch(iyes(1))
		call gmremovewindow(iynwin)
		if(istat1.eq.gon) then
			callid=4450
			goto 2
		else
			isw=-4
			xtext='Q(i,j) '//char(124)//'Name'//char(124)//'Initial guess'//char(124)//&
			'Final value'//char(124)//'CV(%)'//char(124)//'SD'//char(124)//'Constraint/Fix'//&
			char(124)//'Factor'
			ihcell=24
			do i=1,10
				iwcell(i)=110
			enddo
!!			if(discprt) write(7,313) m,i,j,titlep(m),thsav(m),theta0(m),cv,sx
			call text_array(igraph2,Main,Form1,Form1_TxtArray1,8,npar,iwcell,ihcell,xtext,isw,&
            namelink,ratcons(indk)%iconc,ratcons(indk)%qij,&
			thsav(1:100),titlem,ratcons(indk)%titlep,NBOUND,ligname,nlig,&
			ratcons(indk)%micro,ratcons(indk)%ligant,jfix,theta0,iformText)	
			call gmSetWidgetStatus(modelw(16), GSELECTABLE)
		endif

case (4460) ! show plots
	if(.not.plotdone) then
		iset=1
		iplot=1
		kset=1
		kplot=1
		call gmDefineKeySelectCallback(13,4509)
		title1='  '
		call main_plots(main,miplot0,mtoggle,1,1,iplot_Toggle0,nlig,ligname,conc,nset)
	endif
case(4500)
	! skip fit
case(4501:4506) ! choose the plot 
    do i=1,20
        ylo(i)=0.0
        yhi(i)=0.0
    enddo
	!' (1) Plot histogram of apparent OPEN periods, with fit'
	!' (2) Plot histogram of apparent SHUT times, with fit'
	!' (3) Open time pdf conditional on adjacent gap'
	!' (4) Mean open vs adjacent gap plot, with fit' 
	!' (5) Dependency plot'
	!' (6) P(open) curves (ideal and HJC)'
	ncomp=0
	iplot=callid-4500
    kplot=iplot
	mcount=iplot    
	if(nodata) then	!must allocate tint anyway before call

	   nd1=1
	   nd2=1
	   if(.not.allocated(tint)) then
		ALLOCATE(tint(1,1),ampl(1,1),iprops(1,1))
	   endif
	endif
 	logt=.false.
	titlex='               '
	titley='               '
	ztitle='               '
	titlez='               '
	title1='               '
	title2='               '
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

    icomp=1
	iopt=1
    id1=1
	itype3=3

	kth=1			!must allocate for vplot call, but not used

	kF=kB+kC+kD
	
	tres1=sngl(1.d3*tres)	!tres in msec
	if(tres.eq.0.d0) tres1=0.0001
	interp=.false.
	replot=.false.
	nbo=0		!not used here (only in hjclik)
	nbg=0
	ONESET=.true.	!so HJCLIK calcs pdf for set #iset ONLY
!	iset=1
	if(iset.lt.1.and.iset.gt.nset) iset=1
	iset9=iset
	tres=tresd(iset)
	tres1=sngl(1.d3*tres)	!tres in msec
	if(tres.eq.0.d0) tres1=0.0001
	treso=tres1
	tresg=tres1
	nlvar=1	!initial defaults for iplot=6
	!iset=1
	nlvlast=0
	isetlast=0

	corprev=.false.	!correl'n dep on prev gap (used for iplot=3)
	cornext=.false.	!correl'n dep on next gap (used for iplot=3)

	nores=.false.
	scale=.false.           
	hdisp=.false.
	if(iplot.eq.1.or.iplot.eq.2) then
	  	nores=.true.
		scale=.true.        
	endif
	if(iplot.eq.3) then
	   nrange=1
		ylo(1)=tres1
		yhi(1)=0.1
	endif
	if(iplot.eq.4) then
	   if(readini) then
	   nrange=nranged
		do i=1,nrange
		   ylo(i)=ylod(i)
		   yhi(i)=yhid(i)
		  
		enddo

	   else
		nrange1=1		!so doesn't think default wanted

	   endif
	  
	   if(discprt) write(7,2571)
2571	   FORMAT(/,' Mean durations of individual apparent openings that are',/,&
      '  adjacent to gaps with durations in following ranges (ms)',/)
	endif
	do i=1,nlig
		ligname(i)=ratcons(indk)%ligant(i)
	enddo
    if(iplot.eq.6) then
	   iopth=1	
	   hjmcalc=.false.
	   if(allocated(concsav)) deallocate(concsav)
	   ALLOCATE(concsav(10,10))
	   do i=1,nlig
		do j=1,nset
		   concsav(i,j)=conc(i,j)	!save conc
		enddo
	   enddo
	   if(.not.hjmcalc) then	!ec50, ycal1() etc already calculated
	   do nlv=1,nlig
		call EC50_HJ2(EC50,concmax,pop0,pmax,nlv,iset,QT,conc,k,nerr,monot,decline,&
      	  tres,kA,kF,kAm,kFm,km,p0hjc,pmaxhjc,ec50hjc)

		if(nerr.ne.0) then
		    !tty
		   if(discprt) write(7,590) nerr
590		   format('  Error in bisection for EC50 (EC50_HJ2): nerr = ',i2)
		endif

		if(discprt) write(7,593) ligname(nlv),pop0,p0hjc
593		format(/,' At zero concentration of ',a10,/,&
     	', P(open) (ideal) = ',g12.5,', P(open) (HJC) = ',g12.5)
		if(monot) then

      	   if(discprt) write(7,594) ligname(nlv)(1:10),pmax,pmaxhjc,&
     	   ligname(nlv)(1:10),ec50*1.e6,ec50hjc*1.e6
594		   format( ' Equilibrium response-concentration for ',a10,' (monotonic)',/,&
			' Maximum P(open) (ideal) = ',g11.4, ' Maximum Popen (HJC) = ',f9.6,/,&
			'    EC50 for ',a10,/,'    EC50 (ideal) = ',g11.4,'    EC50 (HJC) = ',g11.4)

		endif
		if(nlig.gt.1) then	!print conc of other ligands
		   do n=1,nlig
			if(n.ne.nlv) then
		
			   if(discprt) write(7,5941) ligname(n)(1:10),concsav(n,iset)*1.e6
5941			   format(' (conc of ',a10,' = ',g11.4,')')
			endif
		   enddo
		endif
	   enddo	!nlv=1, nlig

	   endif

	ENDIF
	call gmDefineKeySelectCallback(13,4507)
	icall=4507
	iplotin=iplot
	title1='   '
	call plot_options(main,iplot0,iplot,icall,nlig,nrange,hjmcalc,ligname,ylo,yhi,conc,&
           iplot_Value2,iplot_text,iplot_TxtArray2,iplot_Toggle1,iplot_Toggle3,iplot_Toggle4,&
           title1)
 

 case(4508)
	
	call gmremovewindow(iplot0)
 case(4509)
 call gmDefineKeySelectCallback(13,0)
    do i=1,nset
   		istatus2=gmEnqToggleSwitch(iplot_toggle0(i))
		if (istatus2.eq.gon) iset=i
	enddo
	do i=1,6
   		istatus1=gmEnqToggleSwitch(mtoggle(i))
		if (istatus1.eq.gon) then 
		    if(i.eq.5.and.nodata) then
	        imees=gmdisplaymessagebox('',' Not valid in this context',ginformation,gok)
	        goto 1
	        endif
	
			callid=4500+i
			call gmremovewindow(miplot0)
			goto 2
		endif
		
	enddo
	
		

case(4510) ! estimate errors
	
   call gmremovewindow(miplot0)
   imess=gmdisplaymessagebox('','Estimate errors for parameters before plotting?',gquestion,gyesno)
    if(imess.eq.gyesbutton) then
		callid=4450
		plotdone=.true.			
		goto 2
	endif
 case(4507) ! setting for plots

 call gmDefineKeySelectCallback(13,0)
    iplot=iplotin
	if(nset.eq.1) then 
		jset=1
		iset=1
		iset9=iset
	endif
		title1='     '
	if(iplot.ne.5) then
	!    call gmenqtextsetting(iplot_Text(10),title1)
	endif
	kset=iset
	jset=iset
	if(discprt) write(7,3711) jset
3711 FORMAT(/,' Plot for set number= ',i2,/)
    hdisp=.false.
	corprev=.false.
	cornext=.false.
	good=.false.
	islast=0
	iopeni=0
	do i=1,250
		idraw(i)=-2
		if(i.le.100) ijus(i)=0
	enddo
	inumx=0
	inumy=0
		l=1

	iset9=iset
	tres=tresd(iset)
	nbo=0		!not used here (only in hjclik)
	nbg=0

	tres1=sngl(1.d3*tres)	!tres in msec
	if(tres.eq.0.d0) tres1=0.0001
	treso=tres1
	tresg=tres1

	call gmsettextsetting(itty,' Plot for set'//char(48+iset))
	
	do i=1,nlig
	ligname(i)=models(igraph2)%ligname(i)
         write(stringtty,fmt='(a16,a10,g13.6)') 'concentration of:',ligname(i),conc(i,iset)*1.e6
		 CALL GMSETTEXTSETTING(ITTY,stringtty)
         if(discprt) write(7,67) ligname(i),conc(i,iset)*1.e6
67       format(/,'   concentration of ',a10,' = ',g13.6,/)
	enddo
	ncalc=512
	ndc1=ncalc
	ndimc=10
	ndimd=10
	ndv1=512
	kwi=512
	kwj=10
	ndimd1=ndimd
	ndimc1=ndimc
	!' (1) Plot histogram of apparent OPEN periods, with fit'
	!' (2) Plot histogram of apparent SHUT times, with fit'
	!' (3) Open time pdf conditional on adjacent gap'
	!' (4) Mean open vs adjacent gap plot, with fit' 
	!' (5) Dependency plot'
	!' (6) P(open) curves (ideal and HJC)'

	
	n1=1
	if(allocated(tintset)) DEALLOCATE(tintset,amplset,ipropsset)

	if(allocated(xval)) DEALLOCATE(Xval,Yval)
	if(allocated(xcal)) DEALLOCATE(Xcal,Ycal)
	if(allocated(w)) DEALLOCATE(w)
!	if(allocated(weight)) DEALLOCATE(weight)
	if(allocated(icurvd)) DEALLOCATE(icurvd)
	if(allocated(icurvc)) DEALLOCATE(icurvc)
	if(allocated(icurvw)) DEALLOCATE(icurvw)
	if(allocated(ndat)) DEALLOCATE(ndat)
	if(allocated(isym)) DEALLOCATE(isym)
	if(allocated(ijoin)) DEALLOCATE(ijoin)
	if(allocated(symsiz)) DEALLOCATE(symsiz)
	if(allocated(iline)) DEALLOCATE(iline)
	if(allocated(ncal)) DEALLOCATE(ncal)
	if(allocated(theta)) DEALLOCATE(theta)
    maxplot=100
	kmax=100 
	allocate(theta(kmax))
	ALLOCATE(icurvw(ndimd))
	
	njset=ndimd
	
	if(iplot.eq.6) then
	    iopth=1
	    if(nlig.eq.1.or.nset.eq.1) goto 2323 
	
	endif
	ka=models(jgraph)%ka
	k=models(jgraph)%n
	call QNEW_HJC(QT,iset,conc,QD,ncdep,nlig,IL,IX,JX,k,km,km)
	if(kfit.eq.0) kfit=models(jgraph)%npar-nfix-neq-nmr-nfixec50
	oneset=.true.
	nerr=0
    ka=models(igraph2)%ka
	k=models(igraph2)%n
	kf=k-ka
	kc=kf-kb
	kam=ka
	kfm=kf
	idebug=8
	first=.true.
!	if(ka.lt.kf) then
	sm=HJCLIK(-1,kfit,THETAf,tint,ampl,iprops,nd1,nd2,Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,&
    XAF,XFA,QexpQA,QexpQF,alpha2,beta2,aloglik,kab,kAm,kFm,km,irt,npar,IQf,&
    irate1,jrate1,nlig,EXPQF,EXPQA,qt,qd)
 !   endif
	ncalc=512
	idebug=0

	if(iplot.eq.4) then
		ncurvd=0
		idec=0
		do i=1,2
   			istatus1=gmEnqToggleSwitch(iplot_toggle1(i)) ! instead of 0
			if (istatus1.eq.gon) idec=i
		enddo
		if(idec.eq.1) ncurvd=3
		if(idec.eq.2) ncurvd=5
		ncurvc=0
		do i=1,2
   			istatus1=gmEnqToggleSwitch(iplot_toggle3(i)) ! instead of 0
			if (istatus1.eq.gon) idec=i
		enddo
		
		if(idec.eq.1) ncurvc=2
	endif
2323 if(iplot.eq.3.or.iplot.eq.5.or.iplot.eq.6) then
	 do i=1,3
   		istatus1=gmEnqToggleSwitch(iplot_toggle1(i)) ! instead of 0
		if (istatus1.eq.gon) idec=i
	 enddo
	if(iplot.eq.3) then
		itype3=idec
		if(itype3.eq.1.or.itype3.eq.3) corprev=.true. !correl'n dep on prev gap
		if(itype3.eq.2.or.itype3.eq.3) cornext=.true. !correl'n dep on next gap
		if(itype3.eq.1) then

	     if(discprt) write(7,331)
331	     format(/,' Display pdf of durations of apparent openings that are',/,&
      '  PRECEDED by gaps with durations in following range (ms)',/)
		else if(itype3.eq.2) then
	    
	     if(discprt) write(7,3321)
3321	     format(/,' Display pdf of durations of apparent openings that are',/,&
      '  FOLLOWED by gaps with durations in following range (ms)',/)
		else if(itype3.eq.3) then
	    
	     if(discprt) write(7,3331)
3331	     format(/, ' Display pdf of durations of apparent openings that are',/,&
      '  ADJACENT to gaps with durations in following range (ms)',/)
		endif
	endif

	if(iplot.eq.5) then !3d
		id1=idec
		do i=1,4
   		istatus3=gmEnqToggleSwitch(iplot_toggle3(i))
		if (istatus3.eq.gon) id2=i
		
		enddo
		callid=4900
		call gmremovewindow(iplot0)
		goto 2
	endif
	if(iplot.eq.6) then !popen
 
	   iopth=idec	!defaults
	  
		endif
	endif
    if(iplot.eq.3.or.iplot.eq.4) then
		nrange=gmenqvaluesetting( iplot_Value2)
		if(nrange.eq.0) then
		  jkstatus=gmdisplaymessagebox('','nrange must be greater than 0',gstop,gok)
		  goto 1
		endif
		do i=1,nrange
			call gmEnqCellSetting(iplot_TxtArray2, 1, i,ylo(i),cnum5)
			call gmEnqCellSetting(iplot_TxtArray2, 2, i,yhi(i),cnum5)
			if(ylo(i).gt.yhi(i)) then
			a1=ylo(i)
			ylo(i)=yhi(i)
			yhi(i)=a1
			endif
		enddo
		do i=1,nrange
		if(ylo(i).lt.tres1-0.00001) then
		   
		   call realtoch(tres1,cnum(1),11)
		   jkstatus=gmdisplaymessagebox('','Lower limit must not be less than resolution = '//cnum(1),gstop,gok)
		   goto 1
		endif
		if(burst(iset).and.ylo(i).gt.tcrit(iset))then
			call realtoch(tcrit(iset),cnum(1),11)
			jkstatus=gmdisplaymessagebox('','shut times longer than tcrit= '//cnum(1),ginformation,gok)
		endif
		enddo
		if(iplot.eq.4) then
		imes=gmdisplaymessagebox('Plot info','Blue = obs + SDM; cyan = predicted for shut time range;| red = theoretical continuous relationship',ginformation,gok)
	    call gmDefineKeyselectCallback(13,0)

		do i=1,20
			ylod(i)=ylo(i)
			yhid(i)=yhi(i)
			nranged=nrange
		enddo
		
		endif
		do i=1,nrange
		sy0(i)=0.		!initialise
		syy0(i)=0.
		ny0(i)=0
		sy1(i)=0.
		syy1(i)=0.
		nyy1(i)=0
		sy2(i)=0.
		syy2(i)=0.
		ny2(i)=0
		sx0(i)=0.		!initialise
		sxx0(i)=0.
		nx0(i)=0
		sx1(i)=0.
		sxx1(i)=0.
		nxx1(i)=0
		sx2(i)=0.
		sxx2(i)=0.
		nx2(i)=0
		enddo
	    
	
	endif
	
    if(kmax.lt.1) kmax=1		!must allocate theta whether used or not
  
	
	do i=1,ndimd
		icurvw(i)=-1	!no SD unless reset below
	enddo
	
	if(iplot.eq.1.or.iplot.eq.3.or.iplot.eq.2) then
		hdisp=.true.
		ioptlog=1
		istatus3=0
		do i=1,5
   		istatus3=gmEnqToggleSwitch(iplot_toggle3(i))
		if (istatus3.eq.gon) ioptlog=i
		
		enddo
		logt=ioptlog.eq.1
		nbw=1
		if(iplot.eq.1.or.iplot.eq.2) then
			istatus1=gmEnqToggleSwitch(iplot_Toggle4(1))
			if(istatus1.eq.gon) icomp=1
			istatus2=gmEnqToggleSwitch(iplot_Toggle4(2))
			if(istatus2.eq.gon) icomp=2
			istatus3=gmEnqToggleSwitch(iplot_Toggle4(3))
			if(istatus3.eq.gon) icomp=3
			istatus4=gmEnqToggleSwitch(iplot_Toggle4(4))
			if(istatus4.eq.gon) then 
				nores=.true.
				istatus6=gmEnqToggleSwitch(iplot_Toggle4(6))
				if(istatus6.eq.gon) then 
					scale=.true.
	
				else
					scale=.false.
		
				endif
			else
				nores=.false.
				scale=.false.
			
		
			endif
		endif
	else if(iplot.eq.6.and.nlig.gt.1.and..not.hjmcalc) then
		do i=1,2
   		istatus3=gmEnqToggleSwitch(iplot_toggle3(i))
		if (istatus3.eq.gon) nlvar=i
		
		enddo
		
		if(nlvar.ne.nlvlast) hjmcalc=.false.	!recalc means
		nlvlast=nlvar
	   
	endif
	call gmremovewindow(iplot0)

	if(iplot.eq.6) then
	   
		

		hjmcalc=.false.	!hjc Popen not yet calculated
		xmin=sngl(ec50out)/100.	!molar
		xmax=sngl(ec50out)*100.
		x1pop=xmin*1.e6			!micromolar
		x2pop=xmax*1.e6			!micromolar
		if(xmin.lt.1.e-12) then
			x1pop=0.1
			x2pop=1000.
		endif
	    ndatad=0
		call gmDefineKeySelectCallback(13,4511)
	    call popen(main,ipopen,iopth,ivalx1,x1pop,ivalx2,x2pop,iptog,ivaldat,ndatad)
	else
		!if(iplot.ne.5) then
	ALLOCATE(XVAL(ndv1,ndimd),YVAL(ndv1,ndimd),XCAL(ndc1,ndimc),YCAL(ndc1,ndimc))
	ALLOCATE(w(kwi,kwj))
	! allocate weight(kwi,kwj) 
    ALLOCATE(ndat(njset),isym(njset),ijoin(njset),symsiz(njset))					
	ALLOCATE(ncal(ndimc),iline(ndimc),icurvc(ndimc))
	ALLOCATE(icurvd(ndimd))
	!endif
		callid=4600
		if(nodata) callid= 4690	
		goto 2
	endif

!	call gmSetToggleSwitch(iplot_Toggle4(7),Gon)
 case(4516:4518)
	if(iopth.eq.1) then
		ip=callid-4515
		if(ip.eq.3) then
			call gmsetwidgetstatus(ivaldat,gselectable)
			call gmsetwidgetstatus(iptog(4),gselectable)
			call gmsetwidgetstatus(iptog(5),gselectable)
			call gmSetToggleSwitch(ipTog(5),Gon)
		else
			call gmsetwidgetstatus(ivaldat,gunselectable)
			call gmsetwidgetstatus(iptog(4),gunselectable)
			call gmsetwidgetstatus(iptog(5),gunselectable)
		endif	
	endif

	case(4511) ! plot=6
	        call gmDefineKeySelectCallback(13,0)
		    x1pop=gmenqvaluesetting(ivalx1)
			x2pop=gmenqvaluesetting(ivalx2)
			xmin=x1pop*1.e-6	!molar
			xmax=x2pop*1.e-6
			dx=(alog10(xmax)-alog10(xmin))/(float(ncalc-1))
		ip=1
3355    continue
	  	if(iopth.eq.1) then
			do i=1,3
				istat5=gmenqtoggleswitch(iptog(i))
				if(istat5.ne.0) ip=i
			enddo
			if(ip.eq.3) then
				ndatad=gmenqvaluesetting(ivaldat)
				istat4=gmenqtoggleswitch(iptog(4))
				if(istat4.ne.0) icurvw(1)=1
			else if(ip.eq.2) then
			!	imes=gmdisplaymessagebox('','not yet done',ginformation,gok)
			!	goto 1
			endif
		endif
		
		call gmremovewindow(ipopen)
		iplot=kplot
		ncalc=512
	    ndc1=ncalc
	    ndimc=10
	    ndimd=10
	    ndv1=512
	    kwi=512
	    kwj=10
	    ndimd1=ndimd
	    ndimc1=ndimc
	    njset=ndimd
	  !  goto 9191
	    if(allocated(xcal)) DEALLOCATE(Xcal,Ycal)
	    if(allocated(ncal)) DEALLOCATE(ncal,iline,icurvc)
	    if(allocated(xval)) DEALLOCATE(Xval,Yval)
	    if(allocated(w)) DEALLOCATE(w)
	   ! if(allocated(weight)) DEALLOCATE(weight)
	    if(allocated(icurvd)) DEALLOCATE(icurvd)
	    if(allocated(icurvc)) DEALLOCATE(icurvc)
	
	    if(allocated(ndat)) DEALLOCATE(ndat)
	    if(allocated(isym)) DEALLOCATE(isym)
	    if(allocated(ijoin)) DEALLOCATE(ijoin)
	    if(allocated(symsiz)) DEALLOCATE(symsiz)
	    if(allocated(iline)) DEALLOCATE(iline)
	    if(allocated(ncal)) DEALLOCATE(ncal)
	    
	    
	    ALLOCATE(XVAL(ndv1,ndimd),YVAL(ndv1,ndimd),XCAL(ndc1,ndimc),YCAL(ndc1,ndimc))
	    ALLOCATE(w(kwi,kwj))
	    !allocate weight(kwi,kwj)
        ALLOCATE(ndat(njset),isym(njset),ijoin(njset),symsiz(njset))					
	    ALLOCATE(ncal(ndimc),iline(ndimc),icurvc(ndimc))
	    ALLOCATE(icurvd(ndimd))
9191    continue
	    if(.not.hjmcalc) then	!values not yet calc
	    do i=1,ncalc
			xcal(i,1)=0.0
			ycal(i,1)=0.0
			xcal(i,2)=0.0
			ycal(i,2)=0.0
		enddo
		do i=1,ncalc
		   x=xmin*(10.**(float(i-1)*dx))	!molar
		   xcal(i,1)=1.e6*x		!micromolar for axes
		   xcal(i,2)=1.e6*x		!micromolar
		   conc(nlvar,iset)=x		!molar for calculations
		   call QNEW_HJC(QT,iset,conc,QD,ncdep,nlig,IL,IX,JX,k,km,km)
		   
		   call HJCMEAN(hmopen,hmshut,QD,tres,k,kA,kF,kAm,kFm,km)
		   ycal(i,1)=sngl(hmopen/(hmopen+hmshut))	!HJC Popen
		   call EQOC_HJC(QD,peq1,k,km,km)
		   s=0.0
		   do m=1,kA
			s=s+ sngl(peq1(m))
		   enddo
		   ycal(i,2)=s		!ideal Popen
	     enddo
		  nligsav=nlig
		  call EC50_HJ2(EC50,concmax,pop0,&
      	  pmax,nlvar,iset,QT,conc,k,nerr,monot,decline,&
      	  tres,kA,kF,kAm,kFm,km,p0hjc,pmaxhjc,ec50hjc)
	      call HJC_Hill(xcal,ycal,ncalc,nchill,h50,h50hjc,&
          decline,pmax,pop0,ec50,p0hjc,pmaxhjc,ec50hjc,ndc1,ndimc1)
		  hjmcalc=.true.		!so calcs not repeated
	   endif
	   
	   if(discprt) write(7,592) iset,h50,h50hjc
592	   format(' SET #',i2,':  Hill slope at EC50 = ',f9.4,' (ideal), or ',f9.4,' (HJC)')
       do i=1,nlig
		do j=1,nset
		   conc(i,j)=concsav(i,j)	!restore conc
		enddo
	   enddo
	   if(allocated(concsav)) deallocate(concsav)
	   ncurvd=0
	   ncurvc=2
	   title1='                          '
	   if(iopth.eq.1) then
		icurvc1(1)=1
		icurvc1(2)=2
		titlex='Conc (micromolar): '//ligname(nlvar)
		titley='    P(open) '                            
	   else if(iopth.eq.2) then
		icurvc1(1)=3
		icurvc1(2)=4
		titlex='Conc (micromolar): '//ligname(nlvar)
		titley='     Hill slope '   
	   else if(iopth.eq.3) then
		icurvc1(1)=5
		icurvc1(2)=6
		titley=' Hill slope '                            
		titlex='     P(open)'         
	   endif

	   ncal(1)=ncalc
	   ncal(2)=ncalc
	   ncal(3)=nchill
	   ncal(4)=nchill
	   ncal(5)=nchill
	   ncal(6)=nchill
	   iline(1)=0		!contin
	   iline(2)=3		!dashed
	   iline(3)=0		!contin
	   iline(4)=3		!dashed
	   iline(5)=0		!contin
	   iline(6)=3		!dashed
	   icol(151)=9	!yellow for calc set 3 -preceding gap (continuous)
	   icol(152)=12	!red for calc set 4    -following gap (continuous)
	   icol(153)=9	!yellow for calc set 3 -preceding gap (continuous)
	   icol(154)=12	!red for calc set 4    -following gap (continuous)
	   icol(155)=9	!yellow for calc set 3 -preceding gap (continuous)
	   icol(156)=12	!red for calc set 4    -following gap (continuous)

	   ncurvd=0
	   ndat(1)=0
	   isym(1)=-7	!circle for data
	   ijoin(1)=-1	!not joined

			ippop=ip
			if(ip.eq.2) then
				callid=4521
				goto 2
			
			    	!call CVREAD(xval,yval,weight,icurvw,ndat1,ndv1,ndimd1,kwi,kwj)
			
			else if(ip.eq.3) then
				ndatad=gmenqvaluesetting(ivaldat)
				ndat(1)=ndatad
				ncurvd=1
				ndcol=2
				ndrow=ndat(1)
				xtext='Concentration'//char(124)//' P(open)'
				if(icurvw(1).eq.1) then
					ndcol=3
					xtext='Concentration'//char(124)//' P(open)'//char(124)//' SD'
				endif
				do i=1,ncol
					itypeval(i)=3
				enddo
				nsetcv=1
				do j=1,20
					array_value(1,j)=0.0
					array_value(2,j)=0.0
					array_value(3,j)=0.0
				enddo
				call array_table(main,10,3,i_array_form,i_array,xtext,ndcol,&
				ndrow,array_value,array_text,itypeval,4531,ixgrid,iygrid)
				
			else
				callid=4850
		
				goto 2
			endif
case(4521)
		dfilt='*.dat'//char(124)//'CVDAT Files (DAT)'//char(124)//'All Files'
		CALL gmFileBROWSER(nwFILE,dDIR,dFILT,gmBrowseType=0)
        	
		IF(nwFILE.ne.' ') then
					
call openfile(MAIN,RECORDS,nplotcv,IFILTYPE1,iplotype1,iptype1,nwFILE,FOPEN_11,FOPEN_12,&
					APpend,DISCPRT,njset,ncolr,nrowr,label,ftitle,nsetcv,iwid,List7_1, &
					ftitle1, button7,button6,saveandplot,-1)

					
					
				
		else
			
		endif
		program_type=3
		dfilt=dfilth
			
	
case(4522) ! read cvdat
			nentry=nplotcv
			
			if(ifiltype1.eq.1) then  !cvdat
				if(nentry.gt.0) then
					do i=1,nentry
					
						istat(i)=gmEnqListEntry(List7_1,i,ftitle1(i))
					
						if(istat(i).eq.2) iplotcv=i
			
					enddo
					call gmremovewindow(records)
					
					read(11,rec=1) nplotcv1,jstrec,lstrec,iver
					if(iplotcv.lt.1) iplotcv=1
					if(iver.le.1002) then
							nrect=lstrec(IPLOTcv)-jstrec(IPLOTcv)+1		!total number of records
							irec=jstrec(IPLOTcv)
							IREC=((JSTREC(iplotcv))-1)*1024 + 1
					else
							nrect=lstrec(IPLOTcv)-jstrec(IPLOTcv)+1		!total number of records
							irec=jstrec(IPLOTcv)
					endif
					nrecd=nrect-1				!number of records for Xval etc
					istrec=irec
					if(iver.le.1002) then

						if(iver.eq.1001) then
							read(11,rec=irec) (titled(K),K=1,10),nsetcv,(ndat(k),k=1,10)
						else
							read(11,rec=irec) titlef,titleds,nsetcv,(ndat(k),k=1,10)
	            
						endif
		    
	   					ALLOCATE(datcop(3000))
						n=1
						do j=1,nrecd
							m=n+255
							irec=irec+1024
							read(11,rec=irec) (DATCOP(i),i=n,m)
						    n=n+256
						enddo
					else
						read(11,rec=irec) nsetcv,titlef,(titled(j),j=1,nsetcv),&
      					(ndat(j),j=1,nsetcv),(setx(j),j=1,nsetcv),setvar,iw,&
                 titlex,titley,ilabel,hdisp,ioffsetcv,kcvA
						ntot=0
				
						do j=1,nsetcv
						 ntot=ntot+ndat(j)
						enddo
	   					ALLOCATE(datcop(3*ntot))
						read(11,rec=irec+ioffsetcv) (DATCOP(i),i=1,kcvA)
			
					endif
					kcv=0		!index for datcop
					if(allocated(xval)) deallocate(xval,yval,w)
					
					allocate(xval(100,nsetcv),yval(100,nsetcv),w(100,nsetcv))
					do j=1,nsetcv
	         
							n=ndat(j)
							do i=1,n
							kcv=kcv+1
							Xval(i,j)=datcop(kcv)
							enddo
	         				do i=1,n
							kcv=kcv+1
							Yval(i,j)=datcop(kcv)
							enddo
	   						do i=1,n
							kcv=kcv+1
							w(i,j)=datcop(kcv)
							enddo
	          
					enddo
					DEALLOCATE(datcop)
					close(unit=11)
					
				ncurvd=1
			    icurvw(1)=1
				
				xtext='Concentration'//char(124)//' P(open)'
				if(icurvw(1).eq.1) then
					ndcol=3
					xtext='Concentration'//char(124)//' P(open)'//char(124)//' Weight'
				endif
				do i=1,ncol
					itypeval(i)=3
				enddo
			    do i=1,nsetcv
				ndrow=ndat(i)
				if(ndrow.gt.20) ndrow=20
				do j=1,ndrow
					array_value(1,j)=xval(j,i)
					array_value(2,j)=yval(j,i)
					array_value(3,j)=w(j,i)
				enddo
				call array_table(main,10+i,3+i,i_array_form,i_array,xtext,ndcol,ndrow,array_value,array_text,&
				itypeval,4530+i,ixgrid,iygrid)
				enddo
					ncurvd=1
					icurvd(1)=1
				endif
			else
				call gmremovewindow(records)
			endif

case(4523)		
		ncurvd=1
		icurvd(1)=1
			
		callid=4850
		
		goto 2	
case(4531:4540)	! ADD POINTS
ind=callid-4530
		do j=1,ndrow
			call gmEnqCellSetting(i_array(ind), 1,j,xval(j,1),cnum5)
			call gmEnqCellSetting(i_array(ind), 2,j,yval(j,1),cnum5)
			if(ndcol.eq.3) then
			if(ip.eq.2) then
			call gmEnqCellSetting(i_array(ind), 3,j,w(j,1),cnum5)
			else
			call gmEnqCellSetting(i_array(ind), 3,j,sod,cnum5)
	
			if(sod.gt.1.e-18) then
				   w(j,1)=1.0/(sod*sod)
			else
				   w(j,1)=0.0
			endif
			endif
			endif
		enddo
		do j=1,ndrow
		if(xval(j,1).eq.0.0) l=l+1
		enddo
		if(l.ge.ndrow-2) then
			ip=1
			ncurvd=0
		endif
		do i=1,nsetcv
		call gmremovewindow(i_array_form(i))
		enddo
		callid=4850
		goto 2
case(4544) ! save to ini
case(4545) ! help
	itextform=  gmCreateComplexDialogueBox(Main,10,6, 14, 6, GALL, 'Help ', &
              	 gmhpos=gleft,gmvpos=gtop,gmIconFormat=GDLLICON,gmIconFile='Gee')
 
	 itextar1 = gmCreateTextEntry(itextform, 0, 0, 14, 1,&
			'To paste from other documents, place the mouse in the ', &
			 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=Gtop, gmExpand=GOFF)
	itextar1 = gmCreateTextEntry(itextform, 0, 1, 14, 1,&
			 'table top left corner and then click the right button', 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=Gtop, gmExpand=GOFF)
	itextar2 = gmCreateTextEntry(itextform, 0, 2, 14, 1,&
			'To copy/paste highlight the required area in the table with  ',&
			32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=Gtop, gmExpand=GOFF)
	itextar2 = gmCreateTextEntry(itextform, 0, 3, 14, 1,&
			  'the mouse (left button) and then click the right button', 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              	gmVpos=Gtop, gmExpand=GOFF)
	iradB = gmCreatePushButton(itextform,5,0 ,4 , 1, 'OK', gmType=GUSERDEFINED,&
		gmhpos=gright,gmVpos=Gbottom,gmOffcol=38, gmTextCol=1,&
		gmcallback=4520)
	call gmdrawwindow(itextform)
case(4520)
	call gmremovewindow(itextform)
case (4600) ! plot=1,2,3,4
    if(.not.rescalex)iplot=kplot
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

case(4601) ! plot 1,2,3,4
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
	open=.false.
	open=ampl(in,iset).ne.0.
	if(open.and.in.eq.1) then
!!!	 call GMSETTEXTSETTING(itty,' INTERVAL  should be shut')
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

				call GETRANGE(tshut,topen,ylo,yhi,nrange,sy1,syy1,nyy1,&
				sy2,syy2,ny2,sx1,sxx1,nxx1,sx2,sxx2,nx2,deb,nr)
				if(nr.eq.0) nr0=nr0+1	!tgap not in any range
		
				if(tshut.ge.ylo(1).and.tshut.lt.yhi(1)) then		!in range for pdf
					j=j+1
					tval(j)=topen		!for iplot=3 (following gap, or both)
				endif
			endif
	  
		endif

! if xmax increased then count all shut times, not just thos < trcrit
		islast=in	!index in tint() of last shut period
	    if(rescalex) then
			if(tshut.gt.xmax) then	!goto 92 -end present group with prev opening
				 callid= 4612 !92	!end present group with prev opening
				 goto 2
			endif
		else
		   callid= 4612 !92	!end present group with prev opening (!	goto 92)
		   goto 2
	    endif
	endif

! Now have a good shut time, in tshut say
! At this point the length of the last good open period will still be in topen
! so it can be used for iplot=3,4 as long as the last opening in this open
! period was adjacent to the gap just found (eg haven't skipped a bad burst in
! between), which will be so if islast=iolast+1. In this case can accum sy1 etc
! which have data for 'following gap'

	tshut=tint(in,iset)

	islast=in	!index in tint() of last shut period
	if(cornext.or.iplot.eq.4)then
		if(islast.eq.iopeni+1) then
			call GETRANGE(tshut,topen,ylo,yhi,nrange,sy1,syy1,nyy1,&
			sy2,syy2,ny2,sx1,sxx1,nxx1,sx2,sxx2,nx2,deb,nr)
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
	ng=ng+1		!upDATEW number of groups now #ng has ended
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
	ng=ng+1		!upDATEW number of groups now #ng has ended
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
	if(badend(iset)) then
	call GMSETTEXTSETTING(ITTY,'Should not get here when bad gap ends grp')
	endif
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
	   mtitlek='Mean open time conditional on PRECEDING shut time'
	   CALL GMSETTEXTSETTING(ITTY,'Mean open time conditional on PRECEDING shut time')
	   call PRANGE(mtitlek,ylo,yhi,nrange,sy0,syy0,ny0,sx0,sxx0,nx0,&
     	yval,xval,ndv1,ndimd,1,w,kwi,kwj,icurvw)
	endif
	if(cornext.or.iplot.eq.4) then
	   mtitlek='Mean open time conditional on FOLLOWING shut time'
		CALL GMSETTEXTSETTING(ITTY,'Mean open time conditional on FOLLOWING shut time')
	   call PRANGE(mtitlek,ylo,yhi,nrange,sy1,syy1,nyy1,sx1,sxx1,nxx1,&
     	yval,xval,ndv1,ndimd,2,w,kwi,kwj,icurvw)
	endif
	if((corprev.and.cornext).or.iplot.eq.4) then
	   mtitlek='Mean open time conditional on EITHER shut time'
	   CALL GMSETTEXTSETTING(ITTY,'Mean open time conditional on EITHER shut time')
	   call PRANGE(mtitlek,ylo,yhi,nrange,sy2,syy2,ny2,sx2,sxx2,nx2,&
     	yval,xval,ndv1,ndimd,3,w,kwi,kwj,icurvw)
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

case(4652)!99  ! settings for histograms
! end of data
	ibad=0
	if(ng.eq.0) then
		CALL GMSETTEXTSETTING(ITTY,' NO GROUPS FOUND')
	endif
	if(.not.sbin) NBIN=-1
	if(nyval.eq.0) then
		imes=gmdisplaymessagebox('',' NO VALUES FOR DISTRIBUTION',gstop,gok)
		goto 1
	endif
! For pdfs, variable is in X axis so calc as xmin,xmax
	if(rescalex) xmaxsav=xmax	!don't let varv1 change specified xmax

	call VARV1(tval,Nyval,xbar,varx,xmin,xmax)
	xmin=0.0		!reset for histogram

	if(rescalex) xmax=xmaxsav	!restore after VARV1

	if(discprt) write(7,413) NYVAL,xbar,sqrt(varx),xmin,xmax
413	FORMAT(/,' Mean and SD of ',i5,' values= ',G13.6,' +/-',g13.6,&
     /,' Range from ',g13.6,' to ',g13.6)
	  write(string,fmt='(a20,i5,a10,g13.6,a4,g13.6,a12,g13.6,a4,g13.6)') &
	  ' Mean and SD of ',nyval,' values= ',xbar,'+/-',sqrt(varx),' Range from ',xmin,'to ',xmax
     
	CALL GMSETTEXTSETTING(ITTY,string)
	xmin=0.0		!reset for histogram
	setmax=.false.	!xmax set automatically, not manually
	if(sbin) then
		callid=4680
		goto 2
	endif
	if(ioptlog.eq.6) then
	   nbin=0
	   if(allocated(thetas)) deallocate(thetas)
	   goto 1
	endif

	if(ioptlog.eq.1) logt=.true.

	nbw=1
	tres=tresd(iset)
	xaxis(1)=sngl(tres*1.d3)	!in msec
	if(nyval.le.300) nbdec=5				!default
	if(nyval.gt.300.and.nyval.le.1000) nbdec=8	!default
	if(nyval.gt.1000.and.nyval.le.3000) nbdec=10	!default
	if(nyval.gt.3000) nbdec=12				!default
	xendo=1.+xmax-amod(xmax,1.)
	if(ioptlog.ge.3) then
		if(.not.sbin) then
		m=1
		do i=1,nbw
	

			nbin=20
			if(ioptlog.eq.4) nbin=40
			if(ioptlog.eq.5) nbin=60
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
			xwbase=dx
	
		enddo
		callid=4680
		goto 2	
		endif
	endif
		vtext(50)=' Histogram settings:'
	if(ioptlog.eq.1) then
	     
	    itypeval(1)=0   
		if(sbin) then
			val(1)=xaxis(1)
			vtext(1)=' Histogram to start at (ms)'
			dx=exp(alog(10.)/float(nbdec))
			xwbase=alog10(dx)
			ntog=1  
			itypeval(1)=9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
		else
			val(1)=xaxis(1)
			vtext(1)=' Histogram to start at (ms)'
			if(logt.and.xaxis(1).le.0) vtext(1)=' Histogram (log) to start at '
			if(nbdec.eq.0) nbdec=5
			val(2)=nbdec
			vtext(2)=' Number of bins/decade '
			val(3)=xendo
			vtext(3)=' Last x value (ms) '
			ntog=3
			itypeval(1)=9  
			itypeval(2)=0
			itypeval(3)=9
		endif
	else
			
			if(sbin) then
				val(1)=xaxis(1)
				vtext(1)=' Histogram to start at (ms)'
				ntog=1
					itypeval(1)=9  
			else

				val(1)=xaxis(1)
				vtext(1)=' Histogram to start at (ms)'
				vtext(2)=' Number of different bin widths (-1 to skip histo)'
				itypeval(2)=0
				itypeval(1)=9
				val(2)=1
				val(1)=xaxis(1)
				ntog=2
			endif
		
	endif
	!call gmDefineKeySelectCallback(13,4661)
	itcall=4661
	call value_table(Main,ivwin,ntog,vtext,ival,val,itcall,itypeval)
case(4661)
    !call gmDefineKeySelectCallback(13,0)
	do i=1,ntog
		val(i)=gmenqvaluesetting(ival(i))
	enddo
	xaxis(1)=val(1)

	if(ioptlog.eq.1) then
		if(.not.sbin) then
			xaxis(1)=val(1)
			nbdec=int(val(2))
			xendo=val(3)
		endif 
	else
		if(.not.sbin) then
			xaxis(1)=val(1)
			nbw=int(val(2))
		else
			xaxis(1)=val(1)
		endif 
	endif
	call gmremovewindow(ivwin)
	if(logt.and.xaxis(1).le.0) xaxis(1)=0.01
	if(nbw.eq.0) nbw=1
	if(nbw.le.-1) then
	   nbin=0
	   if(allocated(thetas)) deallocate(thetas)
	   goto 1
	endif
	
	
	if(ioptlog.eq.1) then
		dx=exp(alog(10.)/float(nbdec))
		nbin=1+ifix(alog(xendo/xaxis(1))/alog(dx))
		xendo=(dx**nbin)*xaxis(1)
	
		if(setmax) xmax=xendo
		do i=1,nbin
			xaxis(i+1)=xaxis(1)*(dx**i)
		enddo
		mlast=nbin+1
		xwbase=alog10(dx)
		callid=4680
		goto 2
		
	else
		if(.not.sbin)then
		val(1)=dx
		val(2)=xmax
	    dx0=(xmax-xmin)/float(20)
		vtext(1)=' Bin width (ms)'
		vtext(2)='Last x value (pA etc)= '
		itypeval(1)=5
		itypeval(2)=5
	
		do i=1,nbw
		    call intconv(i,cnum5)
			ititle2(i)='width:'//cnum5(1:3)
			setmax=.true.		!xmax set manually
	   		mLAST=m
			val_2(1,i)=dx0
			val_2(2,i)=xendo
		
		enddo
		
		call value_table_2(Main,iv2win,2,nbw,vtext,ititle2,ival2,val_2,4670,itypeval)
		endif

	endif
	
case(4670)
    mlast=1
	m=1
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
			call gmRemoveWindow(iv2win)
		callid=4652
		goto 2
	endif
	if(nwrongbin.eq.-1) then
		nwrongbin=0
		imes=gmdisplaymessagebox('','Please correct the values in red',gstop,gok)
	else
	nbin=m-1
		xwbase=dx
		!xwbase=gmenqvaluesetting(ival(1)) !!!!dec2006
		callid=4680
		call gmRemoveWindow(iv2win)
		if(nbw.gt.1) then
			
		endif
		goto 2
	endif

    

case(4680) ! settings for histogram
	 
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
    if(.not.rescalex) iplot=kplot
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




case(4850) ! vplot+vhist iplot=1,2,3,4,6	! 86
!	if(iplot.eq.6) goto 1
	autplt=.false.
	
	if(rescalex) then
		autplt=.true.
		isval=1
	else
		isval=0
	endif

	draft=.false.
	plotonly=.false.
	doframe=.true.
	landscap=.true.
	fitted=.true.
	cbig=2.5
	ifont=ifont0

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
	

	ndimd=1
	ndv1=512
	ifitype=0
	ncurvd=1
	iscal=1		!scale internally
	n1=0	!dimensions as for earlier versions
	
	ALLOCATE(Xval(n1:ndv1,ndimd),Yval(n1:ndv1,ndimd))
	
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
	
	callid=4852
	goto 2

case(4852) ! continued the histogram setting
       do i=1,ncurvc
		icurvc(i)=i		!total
		ncal(i)=ncalc
	   enddo
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
	   if(.not.rescalex) then
		  xmax=2.*xmax	!make bigger for log display
	   endif
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
	   ttop2=dble(tcrit(iset))*1.d-3	!in sec
	   if(rescalex) then
	     ttop2=dble(10.**xmax)*1.d-3	!in sec (NB log(xmax) taken above)
	   endif
	   Ptc=FTCt(ttop2,tres,k,kA,kF,ucol,Z00F,Z10F,Z11F,XFA,kAm,kFm,km)
	   fac=fac/sngl(ptc)
	   ttop=tcrit(iset)
	   if(rescalex) then
	     ttop=10.**xmax		!NB log(xmax) taken above
	   endif
!	   if(discprt) write(7,603) tcrit(iset),ptc
	   if(discprt) write(7,603) ttop,ptc
603	   format(' Prob[observed shut time < ',f8.3,' ms] = ',g13.6)
	   if(ptc.gt.1.d0.or.ptc.lt.0.d0) then
	 imes=gmdisplaymessagebox('',' ILLEGAL PROBABILITY!',ginformation,gok)
		if(discprt) write(7,6031)
6031		format(' ILLEGAL PROBABILITY!')
!		pause
	   endif
	endif
	j0=1		!for unconditional pdf
	if(iplot.eq.3) then
	   j1=2
	   j2=3
	   call POPADJ(tres,ylo,yhi,ncalc,k,kA,kF,ycal,xcal,j1,j2,den1,&
         QEXPQA,QEXPQF,Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,&
         XAF,XFA,kAm,kFm,kAx,km,ndc1,irt)
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
	   icol(101)=9	!blue for histogram
	   icol(151)=9	!blue for HJC dist
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
		   icol(i+150)=i
		enddo
		jz=ncomp+3	!for zero res
		ncurvc=ncomp+2
		if(nores) ncurvc=ncomp+3
	   endif
	   if(jz.gt.0) then
		iline(jz)=3		!long dash for zero res
		icol(jz+150)=12		!red for zero res
	   endif
	   if(ja.gt.0) then
		icol(ja+150)=10		!green for asymptotic
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
		nckf=kf
		call PDFshut(QD,phio,area,tau1,nckF,km)

		endif

	   fac1=fac
	   if(scale) then
		f=0.0
	      do m=1,kX
		   f=f + area(m)*exp(-tres1/tau1(m))	!area above tres
		enddo
		fac1=fac1/f					!new scale factor
	   endif
	   xmin10=0.001001	  !calc from 1 microsec for zero res
	   if(logt) xmin10=alog10(xmin10)	!xmax already log in this case
	    dx2=(xmax-xmin10)/float(ncalc)
	    !mmm dx2=(xmax-xmin)/float(ncalc)
	   do i=1,ncalc
	    t=xmin10+(float(i-1))*dx2    
		!mmt=xmin+(float(i-1))*dx2     !start at 1 microsec
		if(logt) t=10.**t
		xcal(i,jz)=t
		f=0.0
		do m=1,kX
		   f=f + (area(m)/tau1(m))*exp(-t/tau1(m))
		enddo
! where in earth did 1.4142 come from?!
!		if(logt) f=1.4142*f*t*2.30259 ! multiplid with 1.4142		!f(log10(t))=2.3*t*f(t)
		if(logt) f=f*t*2.30259 !f(log10(t))=2.3*t*f(t)
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
		   fc0=amp(m)*exp(-t1t/tau(m)) !change from tau to tau1
		   !!!fc0=amp(m)*exp(-t1t/tau1(m))	!NB needs excess time here
		   f=f + fc0
		   if(logt) fc0=fc0*t*2.30259		!f(log10(t))=2.3*t*f(t)
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
	   icol(101)=9	!blue for histogram
	   icol(151)=12	!red for unconditional
	   icol(152)=9	!blue for fitted curve
	   icol(153)=9	!blue for fitted curve
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
     ' (1) Unconditional pdf of apparent openings (short dash, green) and &
       (2) pdf for openings preceded by spec gaps (contin line, blue)',Gquestion,Gyesno)
     
	   else if(itype3.eq.2) then
	   	icurvc(1)=1		!unconditional
		icurvc(2)=3		!following gap
	    iline(1)=2
		iline(3)=0           !contin for following gap
		imes=gmdisplaymessagebox('Calculated curves shown:',&
     ' (1) Unconditional pdf of apparent openings (short dash) and &
       (2) pdf for openings followed by spec gaps (contin line)',Gquestion,Gyesno)
    
	   endif
	   
	   if(imes.eq.gyesbutton) then
			callid=4860
			goto 2
	   endif
	   iline(1)=2           !short dash for unconditional
	   iline(2)=0           !continuous for preceding gap
	   iline(3)=3        !long dash for following gap
	   
       titlerp=' Options for calculated curves: choose 1 to 3'
       radio_text(1)=' (1) Unconditional pdf of apparent openings (short dashes)'
       radio_text(2)=' (2) pdf for openings preceded by spec gaps (contin line)'
       radio_text(3)='(3) pdf for openings followed by spec gaps (long dashes)'
       call gmDefineKeySelectCallback(13,4859)
	   call radio_panel(main,iradio,3,iradio_toggle,radio_text,0,&
	   iradiox,radiox,iradioy,radioy,4859,titlerp)
	else
	hdisp=.true.
	callid=4860
	goto 2
	endif
	
!	call toggle_panel(Main,ITOGGLE,ittoglepanel,3,text_tog,intoggle,4855,valdat,idat,itogbutton,itype)


case (4859)
	call gmDefineKeySelectCallback(13,0)
    ncurvc=0
	do i=1,3
		istat5=gmenqtoggleswitch(iradio_toggle(i))
		if(istat5.ne.0) then
		ncurvc=ncurvc+1
		icurvc(i)=i
		goto 566
	endif
	enddo
566	call gmremovewindow(iradio)
	callid=4860
	hdisp=.true.
	goto 2

case(4890) ! vplot
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
	   !  ians=gmdisplaymessagebox('',&
       !' Show observations only for adjacent (before OR after) gap',gquestion,gyesno)
	    if(ncurvd.eq.3) then
		   ncurvd=3
		   do j=1,ncurvd
			icurvd(j)=j+2		!=3,4,5
		   enddo
		   !icol(1)=10	!green for data set 3  -both
		   !icol(2)=11	!light blue for calc set 1 -preceding gap range
		   !icol(3)=9	!dark blue for calc set 2    -following gap range
		else if(ncurvd.eq.5) then
		   ncurvd=5
		   do j=1,ncurvd
	 		icurvd(j)=j
		   enddo
		  ! icol(1)=14	!yellow for data set 1 -preceding gap range
		  ! icol(2)=12	!red for data set 2    -following gap range
		  ! icol(3)=10	!green for data set 3  -both
		  ! icol(4)=11	!light blue for calc set 1 -preceding gap range
		  ! icol(5)=9	!dark blue for calc set 2    -following gap range
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
     	yval,xval,ndv1,ndimd,den1,j1,j2,&
     	QEXPQA,QEXPQF,Z00F,Z10F,Z11F,XFA,kAm,kFm,km,EXPQF,irt)
	 
	   mtitlek=' CALCULATED values for mean open given preceding gap range'
	 
	   call PCRANGE(mtitlek,ylo,yhi,nrange,ny0,den1,yval,xval,ndv1,ndimd,j1)
	 
	   mtitlek=' CALCULATED values for mean open given following gap range'
	 
	   call PCRANGE(mtitlek,ylo,yhi,nrange,nyy1,den1,yval,xval,ndv1,ndimd,j2)
! Calc theoretical relationship in xcal1(i,j), ycal1(i,j) if req.
	   
!	   ians=gmdisplaymessagebox('',&
 !    ' Show also the continuous (theoretical) relationship',gquestion,gyesno)
	   
	  ! ncurvc=0
	   if(ncurvc.eq.2) then
		ncurvc=2
		icurvc(1)=1
		icurvc(2)=2
		ncal(1)=ncalc
		ncal(2)=ncalc
		iline(1)=0		!contin
		iline(2)=3		!dashed
		!icol(11)=14	!yellow for calc set 3 -preceding gap (continuous)
		!icol(12)=12	!red for calc set 4    -following gap (continuous)
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
      QEXPQA,QEXPQF,Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,XFA,kAm,kFm,km,EXPQF,irt)
	
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
          
          if(discprt) write(7,81)
81	    format(/,' Observations -adjacent gap green, filled diamonds, cont. line'&
        ,/,' Calculated -preceding gap light blue, open circles'&
        ,/,' Calculated -preceding gap dark blue, solid squares'&
        ,/,' Theoretical continuous relationship:',/,&
      '   preceding gap yellow curve, following gap red curve.',/)

	   endif
 
	endif	!end if iplot.eq.4
	 
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!
			
	if(iplot.eq.6) then
	   
	   ncurvc=2
	   if(iopth.eq.1) then
		icurvc(1)=1
		icurvc(2)=2
	   else if(iopth.eq.2) then
		icurvc(1)=3
		icurvc(2)=4
	   else if(iopth.eq.3) then
		icurvc(1)=5
		icurvc(2)=6
	   endif
	    ncal(1)=ncalc
	   ncal(2)=ncalc
	   ncal(3)=nchill
	   ncal(4)=nchill
	   ncal(5)=nchill
	   ncal(6)=nchill
	   iline(1)=0		!contin
	   iline(2)=3		!dashed
	   iline(3)=0		!contin
	   iline(4)=3		!dashed
	   iline(5)=0		!contin
	   iline(6)=3		!dashed
	   !icol(11)=9	!yellow for calc set 3 -preceding gap (continuous)
	   !icol(12)=12	!red for calc set 4    -following gap (continuous)
	   !icol(13)=9	!yellow for calc set 3 -preceding gap (continuous)
	   !icol(14)=12	!red for calc set 4    -following gap (continuous)
	   !icol(15)=9	!yellow for calc set 3 -preceding gap (continuous)
	   !icol(16)=12	!red for calc set 4    -following gap (continuous)
	 !  ncurvd=0
	  ! ndat(1)=0
	   isym(1)=-7	!circle for data
	   ijoin(1)=-1	!not joined
	   if(ippop.ne.1) then
			ncurvd=1
			icurvd(1)=1
	   endif
	endif
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
case(4860) ! hist+vplot
       
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
!		do j=1,ncurvc
!			do kl=1,ncal(j)
!			if(ycal(kl,j).gt.0)	ycal(kl,j)=ycal(kl,j)/2.30259
!			enddo
!		enddo
		endif
		iscal=1		!scale internally
		!ndv1=511	!dimensions as for earlier versions
		!ndimd=1
		!ndc1=2048
		!ndimc=10
		if(nset.gt.1) then
			itit=1
			call INTCONV(iset,cnum5)
			title1='Set '//cnum5(1:2)
		endif
		! vhist
		! hdisp=.true.
		iparfirst=-100
		!autplt=.true.
	!!!	nset=ncurvd
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
	
	case(4862)	!now draw plot/histogram:	
		ifontrue=101	
	!	n1=1
	    if(iplot.eq.4) then
			izoom9=izoom
			izoom=-1
			
		
		endif	
		ymaxsav=ymax
		mono=.false.
		if(hdisp) then
		    n1=0
		else
		    n1=1
		endif
		do i=1,100
		    idraw(i)=-2
		enddo
		call calc_default(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,symsiz,&
		ndimd,XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,&
		XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,&
	    XLO1,XHI1,YLO1,YHI1,y0,yinf,inumx,inumy,&
		titlex,titley,ilabel,doframe,autplt,itit,title1,&
		ISHP,ifont,landscap,ndv1,ndc1,w,kwi,kwj,icurvw,kmax,iver,&
		redrawn,plot,redo,pcgm,pwmf,npint,hdisp,n1,tlenx,tleny,nx1,ny1,izoom,&
		xtitle,ytitle,rescale,dxs,dys,ifitype,parval,theta,ncomp,isval,sval,iplot,&
		calbarx,calbary,xbeg4,ybeg4,xend4,yend4,&
        xbeg5,ybeg5,xend5,yend5,iparfirst,ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil,xwbase)
		if(rescalex) ymax=ymaxsav 
		isens=1
		
		modplot=modplot+1
		igraph=modplot
		ngraph=modplot
		
		jopen(modplot)=1
		! the standard colours are set in calc_default calling set_colours (in colour_management)
		! the special are set after the call to calc_default:
		if(iplot.eq.6) then !(P open curves)
			iline(1)=0		!contin
			iline(2)=3		!dashed
			iline(3)=0		!contin
			iline(4)=3		!dashed
			iline(5)=0		!contin
			iline(6)=3		!dashed
			icol(151)=9	!yellow for calc set 3 -preceding gap (continuous)
			icol(152)=12	!red for calc set 4    -following gap (continuous)
			icol(153)=9	!yellow for calc set 3 -preceding gap (continuous)
			icol(154)=12	!red for calc set 4    -following gap (continuous)
			icol(155)=9	!yellow for calc set 3 -preceding gap (continuous)
			icol(156)=12	!red for calc set 4    -following gap (continuous)
	 
			isym(1)=-7	!circle for data
			ijoin(1)=-1	!not joined
		else if(iplot.eq.4) then ! mean open vs adjacent gap
			isym(1)=-7	!circle for data set 1 -preceding gap range
			isym(2)=-3	!square for data set 2 -following gap range
			isym(3)=-4	!diamond for data set 3 -both
			ijoin(1)=0	!cont line joins data pnts
			ijoin(2)=0	!cont line joins data pnts
			ijoin(3)=0	!cont line joins data pnts
	   
			ijoin(4)=-1   !not joimed
			ijoin(5)=-1   !not joimed
! Hollow symbols for calc points
			isym(4)=7	!circle for calc set 1 -preceding gap range
			isym(5)=-3	!solid square for calc set 2 -following gap range

	  
	  
			if(ncurvd.eq.3) then
		  
				icol(101)=10	!green for data set 3  -both
				icol(102)=11	!light blue for calc set 1 -preceding gap range
				icol(103)=9	!dark blue for calc set 2    -following gap range
			else if(ncurvd.eq.5) then
		  
				icol(101)=14	!yellow for data set 1 -preceding gap range
				icol(102)=12	!red for data set 2    -following gap range
				icol(103)=10	!green for data set 3  -both
				icol(104)=11	!light blue for calc set 1 -preceding gap range
				icol(105)=9	!dark blue for calc set 2    -following gap range
			endif
			 if(ncurvc.eq.2) then
		
				iline(1)=0		!contin
				iline(2)=3		!dashed
				icol(151)=14	!yellow for calc set 3 -preceding gap (continuous)
				icol(152)=12
			endif
		else if(iplot.eq.3) then ! open time pdf
		   isetcol=1
			icol(101)=9	!blue for histogram
			icol(151)=2	!dark green for unconditional
			icol(152)=9	!blue for fitted curve	Conditional pdf
			icol(153)=9	!blue for fitted curve
	   
			iline(1)=2           !short dash for unconditional
	  
			if(itype3.eq.1.or.itype3.eq.3) then
				iline(1)=2
				iline(2)=0           !continuous for preceding gap
	  	    else if(itype3.eq.2) then
	   	
				iline(1)=2
				iline(3)=0           !contin for following gap
			endif
			iline(1)=2           !short dash for unconditional
			iline(2)=0           !continuous for preceding gap
			iline(3)=3  
		
		endif
		if(iplot.eq.1.or.iplot.eq.2) then
	   isetcol=1
	   icol(101)=9	!blue for histogram
	   icol(151)=9	!blue for HJC dist
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
		   icol(i+150)=i
		enddo
		jz=ncomp+3	!for zero res
		ncurvc=ncomp+2
		if(nores) ncurvc=ncomp+3
	   endif
	   if(jz.gt.0) then
		iline(jz)=3		!long dash for zero res
		icol(jz+150)=12		!red for zero res
	   endif
	   if(ja.gt.0) then
		icol(ja+150)=10		!green for asymptotic
		iline(ja)=1		!dotted for asymptotic
	   endif
	   do i=1,ncurvc
		icurvc(i)=i		!total
		ncal(i)=ncalc
	   enddo
	endif
		do i=1,100
			idraw(i)=1
		enddo
		if(igraph.le.20) then
								!if(iplotype.ne.3) ndimc=20
			if(ndv1.gt.niobs) niobs=ndv1
		!	do j=1,nset
			do j=1,ncurvd
				nj(j)=ndat(j)
			enddo
			do ml=1,ncurvc
				if(iline(ml).lt.0) iline(ml)=0
			enddo
			n1=1
			if(hdisp) n1=0
			isens=1
			iplotype=1	
			if(hdisp)iplotype=2		
			call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xval,yval,w,nj,niobs,njset,nplot,nset,&
			juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
			wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
			ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
			xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,oldrecords,&
			dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,&
			parval,ifitype)
					
							
			ijplot=ijplot+24
			ixp=ixp0+ijplot
			iyp=iyp0+ijplot
								
			oldrecords(modplot)%IxP=IxP
			oldrecords(modplot)%IYP=IYP				
						
			call graph1(igraph,iplot,main,ixp,iyp,graph1_1,GraphMainPanel1_1,graphics1_1,&
			wxmin,wxmax,wymin,wymax,ipos,gfile,plot,ixposv,iyposv)
		!	CALL GMSETTEXTSETTING(ITTY,'after graph')				
			call draw_axis(xmin,xmax,ymin,ymax,ymin1,ymax1,&
			xtic,ytic,nx1,ny1,xcross,ycross,itx,ity,tlenx,tleny,&
			logx,logy,sqrty,doframe,icol,ntx,nty,idev,thick,itype,&
		    calbarx,calbary,xbeg4,ybeg4,xend4,yend4,&
            xbeg5,ybeg5,xend5,yend5)
		!	CALL GMSETTEXTSETTING(ITTY,'after axes')
			if(icol(1).eq.15) then
			    do i=1,80
			        icol(i)=48
			    enddo
			    do i=81,100
			        icol(i)=12
			    enddo
			endif	
			call draw_text(parval,xtitle,ytitle,ztitle,title1,cnumx,cnumy,&
			cnumz,cexpx,cexpy,cexpz,newtext,numbx,numby,numbz,ntext,inumx,&
			inumy,intx,angle,sizetext,ijus,thick,rx,ry,idraw,&
			icol,ifnt,str,dxs,dys)	
		!	CALL GMSETTEXTSETTING(ITTY,'after text')			
			!	kwi=niobs
			!	kwj=njset
			if(hdisp) then
			xmaxsav=xmax
	if(rescalex) then !mark tcrit with arrow
	   isval=1
	   sval=tcrit(iset)
	else
	   isval=0
	endif
				call draw_hist(xval,yval,icurvd,ncurvd,ndelt,logity,logx,&
					logy,sqrty,y0,yinf,x0,ilog,xmin,xmax,ymin,ymax,idev,ndat,ijoin,&
					icol,thick,ndv1,ndimd,xwbase,lt2)
					rescalex=.false.			!reset
             else
							
				call draw_data(xval,yval,icurvd,ncurvd,ndelt,logity,logx,logy,sqrty,&
					y0,yinf,x0,ilog,idev,ndat,ijoin,icol,isym,&
					symsiz,thick,barleng,interp,npint,Xint,Yint,nintg,yp1,ypn,&
					Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw,&
					ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil)
			  endif
				call draw_curves(xcal,ycal,icurvc,ncurvc,logity,logx,&
					logy,sqrty,y0,yinf,x0,ilog,idev,&
					wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
					xmin,xmax,ymin,ymax,ncal,&
					iline,icol,thick,ndc1,ndimc,jfirst,dxs,dys)
					
				show_curve=.true.
				if(idev.eq.3) show_curve=.false.
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
								    goto 777
								else if(ij.ge.10) then
									linetype=ij-10			!join points with straight line type #ij
								endif
								call broken(linetype)
								call movto2(xmax+0.1*(xmax-xmin),ymax20-ytc)
								call linto2(xmax+0.2*(xmax-xmin),ymax20-ytc)
777						ytc=ytc+0.05*(ymax20-ymin20)
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
							readrec=.true.				
	
			call gmSetWidgetStatus(izoomy, GSELECTABLE)

			call gmSetWidgetStatus(isave_bmp, GSELECTABLE)
			call gmSetWidgetStatus(export_file, GSELECTABLE)
			call gmSetWidgetStatus(print_file, GSELECTABLE)
 
			call gmSetWidgetStatus(jtitle_record, GSELECTABLE)
		!	call gmSetWidgetStatus(jparameters, GSELECTABLE)
			call gmSetWidgetStatus(jlabels, GSELECTABLE)
			call gmSetWidgetStatus(jnewtext, GSELECTABLE)
			call gmSetWidgetStatus(jnumbers, GSELECTABLE)
			call gmSetWidgetStatus(jaxis, GSELECTABLE)
			call gmSetWidgetStatus(jframe, GSELECTABLE)
		
			!call gmSetWidgetStatus(jlines, GSELECTABLE)
			call gmSetWidgetStatus(new_lines_h, GSELECTABLE)
			call gmSetWidgetStatus(new_lines_V, GSELECTABLE)
			!call gmSetWidgetStatus(jarrows, GSELECTABLE)
			!call gmSetWidgetStatus(jraw_data, GSELECTABLE)
			!call gmSetWidgetStatus(jcurves, GSELECTABLE)
            call gmSetWidgetStatus(new_arrows, GSELECTABLE)
			call gmSetWidgetStatus(new_lines, GSELECTABLE)
			call gmSetWidgetStatus(new_text, GSELECTABLE)
			
			if(kset.lt.nset) then 
				kset=kset+1
				
			else
				kplot=kplot+1
				if(kplot.eq.7) kplot=1
				kset=1
				iplot=kplot
			endif
			iset=kset
			iplot=kplot
			nlig=models(jgraph)%nlig
			do i=1,nlig
			ligname(i)=models(jgraph)%ligname(i)
		    enddo
			call gmDefineKeySelectCallback(13,4509)
			title1='  '
			call main_plots(main,miplot0,mtoggle,kplot,kset,iplot_Toggle0,nlig,ligname,conc,nset)
			isval=0
			
	endif
case(4901:4903)! type 5 iplot=5 3D
if(iplot.eq.5) then
    id1=callid-4900

    if(id1.eq.1) then
	   text1='open period'
	   text2='shut time  '
	else if(id1.eq.2) then
	   text1='shut time  '
	   text2='shut time  '
	else if(id1.eq.3) then
	   text1='open period'
	   text2='open period'
	endif
	id2=3

	radio_text(1)=' (1) '//text1(1:12)//' and following '//text2
	radio_text(2)=' (2) '//text1(1:12)//' and preceding '//text2(1:12)
	radio_text(3)=' (3) '//text1(1:12)//' and both following and preceding '//text2
!next string is too long (89 vs 60 declared
!	radio_text(4)=' (4) '//text1(1:12)//' and following '//text2(1:12)//' MINUS '//text1(1:12)//' and preceding '//text2
    radio_text(4)=' (4) '//text1(1:8)//' & fol '//text2(1:8)//' MINUS '&
	//text1(1:8)//' & prec '//text2(1:8)
	do i=1,4
		  call gmsettextsetting(iplot_text(i),radio_text(i))
	enddo
	!call gmSetToggleSwitch(	iplot_Toggle3(3),Gon)
	endif
case(4900) ! 3D plot

	
	if(id2.gt.0) then
	
	
		p0min=1.
		lag=1
	    xaxis(1)=tresg
		yaxis(1)=treso
	    nbdec=4		!default
		vtext(1)=' X axis to start at X='
		vtext(2)=' Y axis to start at Y= '
		vtext(3)=' Number of bins/decade '
		
		vtext(4)='lag'
		vtext(5)=' Minimum value for expected number in bin  '
		do i=1,10
		itypeval(i)=9
		enddo
		itypeval(3)=0
		itypeval(4)=0
		val(1)=xaxis(1)
		val(2)=yaxis(1)
		val(3)=nbdec
		val(4)=lag
		val(5)=p0min
		call gmDefineKeySelectCallback(13,4905)
		  call value_table(Main,ivwin,5,vtext,ival,val,4905,itypeval)
	endif
case(4905)! 3d plot
call gmDefineKeySelectCallback(13,0)
		do i=1,5
		 val(i)=gmenqvaluesetting(ival(i))
		enddo
		xaxis(1)=val(1)
		yaxis(1)=val(2)
		nbdec=val(3)
		lag=val(4)
		p0min=val(5)
		call gmremovewindow(ivwin)
	   allocate(tintset(nintt(iset)),amplset(nintt(iset)),ipropsset(nintt(iset)))
       do i=1,nintt(iset)
		tintset(i)=tint(i,iset)
		amplset(i)=ampl(i,iset)
		ipropsset(i)=iprops(i,iset)

	   enddo
	   ifnt3=101
	   ifnl=101
	   n=6
	   icall=4910
	   exass=.false.
	   if(iscan.eq.-1003.or.iscan.ge.100) then
		textyn(1)='Exclude openings with ''dubious'' amps ? '
	   else
		textyn(1)=' Exclude openings with ''assumed'' amps ? '
	   endif
	    dsmooth=.false.
	    shbiv=.false.
	    shdep=.false.
	    shdif=.false.
		shfit=.false.
		
	   textyn(2)='Use 3D smoothing before display ?'
	   textyn(3)='Show bivariate distribution ?'
	   textyn(4)='Show dependency plot ?'
	   textyn(5)='Show dependency difference plot ?'
	   textyn(6)='Show fitted dependency plot ?'
	   do i=1,n
			iyesno(i,1)=1
			iyesno(i,2)=0
	   enddo
	  ! iyesno(6,1)=0
	  ! iyesno(6,2)=1
	   if(nofit) then
	        iyesno(5,1)=0
			iyesno(5,2)=1
	        iyesno(6,1)=0
			iyesno(6,2)=1
	   endif
	   call gmDefineKeySelectCallback(13,4910)
	   call yesno_table(main,iynwin,n,textyn,iyes,iyn,iyesno,icall)
case(4811:4815)
		iopt3d=callid-4810		
case(4910) ! 3d plot
call gmDefineKeySelectCallback(13,0)
       do i=1,6
	   enddo
	   	istat1=gmenqtoggleswitch(iyes(1))
		if(istat1.eq.gon) exass=.true.
		istat2=gmenqtoggleswitch(iyes(2))
		if(istat2.eq.gon) dsmooth=.true.
		istat3=gmenqtoggleswitch(iyes(3))
		if(istat3.eq.gon) shbiv=.true.
		istat4=gmenqtoggleswitch(iyes(4))
		if(istat4.eq.gon) then 
		    shdep=.true.
		    istat5=gmenqtoggleswitch(iyes(5))
		    if(istat5.eq.gon) shdif=.true.
		else
		    shdep=.false.
		    shdif=.false.
		endif
		
		istat6=gmenqtoggleswitch(iyes(6))
		if(istat6.eq.gon) shfit=.true.
		call gmremovewindow(iynwin)
        iplotype=4
	   call CDIST3D(iplotype,tintset,amplset,ipropsset,nint(iset),nintt(iset),ndimy,iscan,treso,tresg,tres,idsav,&
	    idiskq,id1,id2,lag,nbdec,xaxis,yaxis,p0min,&
	    igraph,main,ixg,iyg,graph1_1,GraphMainPanel1_1,graphics1_1,ipos,&
        oldrecords,nplot,itty,Z00A,Z10A,Z11A,&
        Z00F,Z10F,Z11F,XAF,XFA,QexpQA,QexpQF,kAm,kFm,km,jopen,&
        ifnt3,ifnl,alfa,beta,gama,delta,&
        ijust,ijusx,ijusy,ijusz,&
        xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,numx3,numy3,&
        xmin3,xmax3,ymin3,ymax3,zmin3,zmax3,xtic3,ytic3,ztic3,&
        ratio,rat,radius,theta3,phi,ifram,numaxi,nwidth,npow,nplace,&
        nxstep,nystep,istyle,isup,&
        fill,inter,axis,fillbad,autplt,cross,iplot,irt)
		modplot=igraph
		ngraph=igraph
	!	call gmSetWidgetStatus(icross, GSELECTABLE)
	!	call gmSetWidgetStatus(i3d, GSELECTABLE)
	!	call gmSetWidgetStatus(irot, GSELECTABLE)
	!	call gmSetWidgetStatus(iview3d, GSELECTABLE)
	!	call gmSetWidgetStatus(iaratxy, GSELECTABLE)
	!	call gmSetWidgetStatus(iarathb, GSELECTABLE)
	!	call gmSetWidgetStatus(igridd, GSELECTABLE)
	!	call gmSetWidgetStatus(isurfdr, GSELECTABLE)
	!	call gmSetWidgetStatus(ifill3d, GSELECTABLE)
	!	call gmSetWidgetStatus(imarkbad, GSELECTABLE)
		ncomp=0
!		ians=gmdisplaymessagebox('','Show fitted dependency plot',gquestion,&
!		gyesno)
!	 if(ians.eq.gnobutton) then
	if(.not.shfit) then
		    if(iset.lt.nset) then 
				kset=kset+1
			else
				kplot=kplot+1
				iplot=kplot
				kset=1
				if(kplot.eq.7) kplot=1
			endif
		    iset=kset
			iplot=kplot
			nlig=models(jgraph)%nlig
			do i=1,nlig
			ligname(i)=models(jgraph)%ligname(i)
		    enddo
		call gmDefineKeySelectCallback(13,4509)
		title1='  '
		call main_plots(main,miplot0,mtoggle,kplot,kset,iplot_Toggle0,nlig,ligname,conc,nset)
  
  else
        iplot=kplot
        iset=kset
        nlig=models(jgraph)%nlig
		do i=1,nlig
			ligname(i)=models(jgraph)%ligname(i)
		enddo
        callid=4911
        goto 2
  endif
case (4911)
	  !fitted dep plot
	   xmin3=tres1
!	   xmax=10000.*xmin
	   ymin3=tres1
!	   ymax=10000.*ymin
	   nvdec=10
	   vtext(1)='Minimum shut time (ms)'
	   vtext(2)='Maximum shut time (ms)'
	   vtext(3)='Minimum open time (ms)'
	   vtext(4)='Maximum open time (ms)'
	   vtext(5)='Values per decade'
	   val(1)=xmin3
	   val(2)=xmax3
       val(3)=ymin3
	   val(4)=ymax3
	   val(5)=nvdec
	   do i=1,5
		itypeval(i)=9
	   enddo
		itypeval(5)=0
		vtext(50)='Fitted dependency plot'
	   call value_table(Main,ivwin,5,vtext,ival,val,4912,itypeval)
	  
case(4912) ! iplot=5
! enq val table
		do i=1,5
		 val(i)=gmenqvaluesetting(ival(i))
		enddo
		iplotype=4
		xmin3=val(1)
		xmax3=val(2)
		ymin3=val(3)
		ymax3=val(4)
		nvdec=val(5)
		call gmremovewindow(ivwin)
	   xmin3=alog10(xmin3)
	   xmax3=alog10(xmax3)
	   ymin3=alog10(ymin3)
	   ymax3=alog10(ymax3)
	 
	   ns=1 + nvdec*ifixr(xmax3-xmin3)
	   no=1 + nvdec*ifixr(ymax3-ymin3)
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

	   delt=(xmax3-xmin3)/float(ns-1)
	   do i=1,ns
		shutt(i)=xmin3 + (float(i-1))*delt		!log values
	   enddo
	   delt=(ymax3-ymin3)/float(no-1)
	   do i=1,no
		opent(i)=ymin3 + (float(i-1))*delt		!log values
	   enddo
       
	   call DEPEND(tres*0.001,k,kA,kF,ns,no,shutt,opent,fDEP,zmin3,zmax3,&
     	 Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,XAF,XFA,QexpQA,QexpQF,kAm,kFm,km,irt)

	   z=ifixr(zmin3)
	   if(z.gt.zmin3) then
		zmin3=z - 1.0
	   else
		zmin3=z
	   endif
	   z=ifixr(zmax3)
	   if(z.lt.zmax3) then
		zmax3=z + 1.0
	   else
		zmax3=z
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
       xtitle='shut time'
	   ytitle='yopen time'
	   ztitle='dependency'
	   title3='Fitted dependency plot'
	   ndx=nx
	   ndy=ny
	   i3col(1)=1	   !contour 1
		i3col(2)=9	   !cont 2
		i3col(3)=3	   !cont 3
		i3col(4)=11   !cont 4
		i3col(5)=2	   !cont 5
		i3col(6)=10   !cont 6
		i3col(7)=8	   !cont 7
		i3col(8)=5	   !cont 8
		i3col(9)=13   !cont 9
		i3col(10)=6	   !cont 10
		i3col(11)=4	   !cont 11
		i3col(12)=12   !cont 12
		i3col(13)=14   !cont 13
		i3col(14)=15   !cont 14
		i3col(15)=7	   !cont 15
		i3col(21)=48	!axes
		i3col(22)=0   !bad region
		i3col(23)=1	!labels
		i3col(25)=1	!title
		i3col(31)=14	!upper net
		i3col(32)=8	!lower net
		i3col(33)=9 	!upper surface
		i3col(34)=10	!lower surface
		i3col(35)=12	!positive
		i3col(36)=11	!negative
		i3col(37)=4	!positive lower
		i3col(38)=9	!negative lower
		i3col(71)=7
	      !in common for gplot3d
		i3col(25)=9
		i3col(71)=7

	   kcol=2
	   posneg=.true.
	   isetcol=2
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
              ncolfu=9 
			ncolfd=10
			ncolgu=14
			ncolgd=8
			ncolmp =12 
			ncolmn =9
			icol23=1
			icol25=1
			icol71=7
			icbad=12
             i3col(33)=ncolfu
             i3col(34)=ncolfd
             i3col(31)=ncolgu
             i3col(32)=ncolgd
             i3col(25)=icol25
             i3col(23)=icol23
             i3col(71)=icol71
             i3col(22)=icbad
             i3col(35)=ncolmp    !positive
			i3col(36)=ncolmn   !negative
		kcol=2
		posneg=.true.
		isetcol=2
		modplot=modplot+1
		igraph=modplot
			ngraph=modplot
		
		iplotype=4
		
	   	call gauto3d(iplotype,igraph,shutt,opent,znew,badval,nx,ny,ndx,ndy,&
     	xtitle,ytitle,ztitle,title3,idrawt,idrawx,idrawy,idrawz,&
        quarter,idev,plot,iplot,kcol,posneg,isetcol,i3col,&
     	wxmin,wxmax,wymin,wymax,vxlop,vxhip,vylop,vyhip,&
        xlop,xhip,ylop,yhip,main,ixg,iyg,graph1_1,&
        GraphMainPanel1_1,graphics1_1,ipos,gfile,ixposv,iyposv,&
     	oldrecords,nplot,ifnt3,ifnl,alfa,beta,gama,delta,&
          ijust,ijusx,ijusy,ijusz,&
          xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,numx3,numy3,&
          xmin3,xmax3,ymin3,ymax3,zmin3,zmax3,xtic3,ytic3,ztic3,&
          ratio,rat,radius,theta3,phi,ifram,numaxi,nwidth,npow,nplace,&
          nxstep,nystep,istyle,isup,&
          fill,inter,axis,fillbad,autplt,cross,jopen)
		DEALLOCATE(fdep,znew,shutt,opent,badval)
		ncomp=0
		
		if(iset.lt.nset) then 
				kset=kset+1
				iset=kset
			else
				kplot=kplot+1
				kset=1
				if(kplot.eq.7) then
				     kplot=1
					 
				endif
			endif
			iset=kset
			iplot=kplot
			nlig=models(jgraph)%nlig
			do i=1,nlig
			ligname(i)=models(jgraph)%ligname(i)
		enddo
		title1='  '
		call main_plots(main,miplot0,mtoggle,kplot,kset,iplot_Toggle0,nlig,ligname,conc,nset)
	
		continue
		! gauto3d
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
case (5101:5125) ! save files to plotq
	modplot=callid-5100
		igraph=modplot
			if(readrec.and.modplot.gt.0) then
555			continue		
             
				CALL gmFileBROWSER(qFILE,eDIR,eFILT,gmBrowseType=1)
				IF(qFILE.ne.' '.and.iplotype.le.4) then
			
				nef=len_trim(qfile)
				iegal=1
				do i=1,nef-4
					if(qfile(i:i).ne.dfile(i:i)) iegal=0
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
				
				INQUIRE(file=qfile,exist=present)
	
				if(.not.present) then
					iverq=1200
					nplot=0
					iverq=1200	!set here in case reset by read from old format queus
					do  i=1,200
					jstrec2(i)=0
					lstrec2(i)=0
					enddo
					OPEN(unit=11,file=qfile,status='UNKNOWN',access='DIRECT',&
					form='BINARY',RECL=1)
					write(11,rec=1) nplot,jstrec2,lstrec2,iverq
					CLOSE(UNIT=11)
				else
					ihandle=FILE$FIRST
					length = GETFILEINFOQQ(qfile, info, ihandle)
					nLEN=info%length 
					if(nlen.lt.10) then
					iverq=1200
					nplot=0
					iverq=1200	!set here in case reset by read from old format queus
					do  i=1,200
					jstrec2(i)=0
					lstrec2(i)=0
					enddo
					OPEN(unit=11,file=qfile,status='UNKNOWN',access='DIRECT',&
					form='BINARY',RECL=1)
					write(11,rec=1) nplot,jstrec2,lstrec2,iverq
					CLOSE(UNIT=11)
					else
					OPEN(unit=11,file=qfile,status='UNKNOWN',access='DIRECT',form='BINARY',RECL=1)
					READ(11,REC=1,IOSTAT=I_VAR) NPLOT,JSTREC2,LSTREC2,IVERq          
					IF(I_VAR.NE.0) THEN
						CLOSE(UNIT=11)
						Icall=gmDisplayMessageBox('Stop ','Not a proper data file',GEXCLAMATION,GOK)
						goto 555
					else
						if(iverq.ne.1200) then
						Icall=gmDisplayMessageBox('Stop ','Old version data file',	GEXCLAMATION,GOK)
						CLOSE(UNIT=11)
						goto 555
						endif
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
	    
			itextq = gmCreateTextEntry(iqPanel, 1,1, 7, 1,'File:'//qfile, 32768, Gdisplay, &
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
		iplotype=oldrecords(modplot)%iplotype
		if(iplotype.eq.4) then
			ndx=oldrecords(modplot)%param_axis%itx
	        ndy=oldrecords(modplot)%param_axis%ity
			if(allocated(shutt)) deallocate(shutt,opent,badval,znew)
			allocate(shutt(ndx),opent(ndy),znew(ndx,ndy),badval(ndx,ndy))
			isens=0
			
            call store_record3d(isens,iplotype,modplot,shutt,opent,znew,badval,nx,ny,ndx,ndy,&
     	            xtitle3,ytitle3,ztitle3,title3,idrawt,idrawx,idrawy,idrawz,&
                    quarter,idev,plot,iplot,kcol,posneg,isetcol,i3col,&
     	            wxmin,wxmax,wymin,wymax,vxlop,vxhip,vylop,vyhip,&
                    xlop,xhip,ylop,yhip,main,ixg,iyg,ipos,ixposv,iyposv,&
     	            oldrecords,nplot,ifnt3,ifnl,alfa,beta,gama,delta,&
                    ijust,ijusx,ijusy,ijusz,&
                    xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,numx,numy,&
                    xmin3,xmax3,ymin3,ymax3,zmin3,zmax3,xtic3,ytic3,ztic3,&
                    ratio,rat,radius,theta3,phi,ifram,numaxi,nwidth,npow,nplace,&
                    nxstep,nystep,istyle,isup,&
                    fill,inter,axis,fillbad,autplt,cross,mark)
		else
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
			!	ALLOCATE(ndat(ndimd),isym(ndimd),ijoin(ndimd),symsiz(ndimd))
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
			iplotype=1
			if(hdisp) iplotype=2
		endif
		call write_plotq(iqplot,iplotype,xval,yval,xcal,ycal,ndimd,ndimc,&
				ncurvd,ndat,icurvd,isym,ijoin,ncurvc,ncal,icurvc,iline,symsiz,&
				xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ntx,nty,itx,ity,&
				xlo,xhi,ylo,yhi,itit,title1,ilog,iscal,doframe,&
				titlex,titley,ilabel,inumx,inumy,qfile,sval,&
				ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,y1v,y2v,y1c,y2c,ivplot,&
				theta,ifitype,ncomp,idest,interp,&
				RLTH,y0,yinf,ntrace,ytsep,ndv1,ndc1,w,kwi,kwj,icurvw,kmax,n1,jmiss,&
				xwbase,isval,lt2,mono,ifont0,ameant,areat,&
				title3,shutt,opent,znew,badval,nx,ny,ndx,ndy,&
     	        xtitle3,ytitle3,ztitle3,idrawt,idrawx,idrawy,idrawz,&
                kcol,posneg,i3col,ifnt3,ifnl,alfa,beta,gama,delta,&
                ijust,ijusx,ijusy,ijusz,&
                xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,numx,numy,&
                xmin3,xmax3,ymin3,ymax3,zmin3,zmax3,xtic3,ytic3,ztic3,&
                ratio,rat,radius,theta3,phi,ifram,numaxi,nwidth,npow,nplace,&
                nxstep,nystep,istyle,isup,&
                fill,inter,axis,fillbad,cross,ioffs)
				
		continue
	
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
			idev=6
			call ttfont(main,ittfont,5126,idev,ittfont_Toggle)
case(5126) !print
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
						!		oldrecords(modplot)%attributes%IFNT(4)=15
						!			ifnt(4)=15
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
				msplot=1
				if(oldrecords(modplot)%IPOS.eq.1) then
				
					do i=modplot+1,modplot+3
						if(oldrecords(i)%IPOS.eq.1) goto 44
						if(jopen(i).eq.1) msplot=msplot+1
					enddo 
				endif	
44				continue
			
				istatus=gmprintercontrol(gprintersetup)
				if(istatus.eq.0) then
				    goto 1 
				else
				istatus=gmprintercontrol(GOPENPRINTER)
				IF(ISTATus.NE.0)THEN
					plot=.true.
					icallprev=5150
						callid=5250
						goto 2
					endif
					
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
	
case(5226) ! print
	
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
					ns=len_trim(sfile)
					if (sfile(ns-3:ns).eq.'.wmf') then
					    idev=1
	   					pwmf=.true.
	   				
	   					idpi=600
	   					ixoff=0
	   					iyoff=0
	   					iwidi=4800
	   					ihei=3300 !3600
	   					
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
		readrec=.true.
		if(readrec) then
			mplot=0
			ifirstprint=0
			do i=modplot,modplot+msplot-1
				if(jopen(i).eq.1) then
					modplot=i
					igraph=i
					mplot=mplot+1
					iplotype=oldrecords(modplot)%iplotype
			        if(iplotype.eq.4) then
			        ndx=oldrecords(modplot)%param_axis%itx
	                ndy=oldrecords(modplot)%param_axis%ity
			        if(allocated(shutt)) deallocate(shutt,opent,badval,znew)
			        allocate(shutt(ndx),opent(ndy),znew(ndx,ndy),badval(ndx,ndy))
			        isens=0
			        call store_record3d(isens,iplotype,modplot,shutt,opent,znew,badval,nx,ny,ndx,ndy,&
     	            xtitle3,ytitle3,ztitle3,title3,idrawt,idrawx,idrawy,idrawz,&
                    quarter,idev,plot,iplot,kcol,posneg,isetcol,i3col,&
     	            wxmin,wxmax,wymin,wymax,vxlop,vxhip,vylop,vyhip,&
                    xlop,xhip,ylop,yhip,main,ixg,iyg,ipos,ixposv,iyposv,&
     	            oldrecords,nplot,ifnt3,ifnl,alfa,beta,gama,delta,&
                    ijust,ijusx,ijusy,ijusz,&
                    xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,numx,numy,&
                    xmin3,xmax3,ymin3,ymax3,zmin3,zmax3,xtic3,ytic3,ztic3,&
                    ratio,rat,radius,theta3,phi,ifram,numaxi,nwidth,npow,nplace,&
                    nxstep,nystep,istyle,isup,&
                    fill,inter,axis,fillbad,autplt,cross,mark)
			        else
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
					!	ALLOCATE(ndat(ndimd),isym(ndimd),ijoin(ndimd),symsiz(ndimd))
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
					endif
					if((pbmp.or.pwmf).and.ipos.le.1) then
						if(icallprev.ne.5200) then
									call intconv(i,cnum0)
									nlp=len_trim(dfile)
									sfile=dfile(1:nlp-4)//cnum0
									npl=len_trim(sfile)
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
					if(iplotype.eq.4) then
						isetcol=2
					 !   autplt=.true.
					    autplt=.false.
						mono=.false.
						plot=.true.
						do m=1,100
							if(i3col(m).eq.0) i3col(m)=48
					    enddo
						call gauto3d(iplotype,igraph,shutt,opent,znew,badval,nx,ny,ndx,ndy,&
     						xtitle3,ytitle3,ztitle3,title3,idrawt,idrawx,idrawy,idrawz,&
							quarter,idev,plot,iplot,kcol,posneg,isetcol,i3col,&
							wxmin,wxmax,wymin,wymax,vxlop,vxhip,vylop,vyhip,&
							xlop,xhip,ylop,yhip,main,ixp,iyp,graph1_1,&
							GraphMainPanel1_1,graphics1_1,ipos,gfile,ixposv,iyposv,&
							oldrecords,nplot,ifnt3,ifnl,alfa,beta,gama,delta,&
                            ijust,ijusx,ijusy,ijusz,&
                            xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,numx3,numy3,&
                            xmin3,xmax3,ymin3,ymax3,zmin3,zmax3,xtic3,ytic3,ztic3,&
                            ratio,rat,radius,theta3,phi,ifram,numaxi,nwidth,npow,nplace,&
                            nxstep,nystep,istyle,isup,&
                            fill,inter,axis,fillbad,autplt,cross,jopen)
						    plot=.false.
					else
						call graph1(modplot,jplot,main,ixp,iyp,graph1_1,GraphMainPanel1_1,graphics1_1,&
						wxmin,wxmax,wymin,wymax,ipos,gfile,plot,ixposv,iyposv)
						
						call draw_axis(xmin,xmax,ymin,ymax,ymin1,ymax1,xtic,ytic,&
						nx1,ny1,xcross,ycross,itx,ity,tlenx,tleny,logx,logy,sqrty,doframe,&
						icol,ntx,nty,idev,thick,itype,&
		                calbarx,calbary,xbeg4,ybeg4,xend4,yend4,&
                        xbeg5,ybeg5,xend5,yend5)
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
							nl=len_trim(gfile)
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
						symsiz,thick,barleng,interp,npint,Xint,Yint,nintg,yp1,ypn,&
						Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw,&
					    ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil)
						endif
						call draw_curves(xcal,ycal,icurvc,ncurvc,logity,logx,&
						logy,sqrty,y0,yinf,x0,ilog,idev,&
						wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
						xmin,xmax,ymin,ymax,ncal,&
						iline,icol,thick,ndc1,ndimc,jfirst,dxs,dys)
						show_curve=.true.
						if(idev.eq.3) show_curve=.false.
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
						xmin0=xmin1
						xmax0=xmax1
						ymin0=ymin1
						ymax0=ymax1
						idest=0
						
					endif
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
	case(5501:5525) ! ascii
	   modplot=callid-5500
        igraph=modplot
        if(modplot.gt.0) then
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
					!	ALLOCATE(ndat(ndimd),isym(ndimd),ijoin(ndimd),symsiz(ndimd))
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
		CALL gmFileBROWSER(filasc,ascpath,ascdef,gmBrowseType=1)
		if(filasc.ne.'  ') then
				INQUIRE (FILE=filasc,EXIST=PRESENT,&
				ACCESS=FACCESS,FORM=FFORM,RECORDTYPE=FRTYPE,RECL=IRCL) 
				if(PRESENT) then
					ihandle=FILE$FIRST
					length = GETFILEINFOQQ(filasc, info, ihandle)
					nLEN=info%length
					if(nlen.lt.50) then
						imko=gmdisplaymessagebox('','Not a proper ASCII file',ginformation,gok)
						goto 1
					else
							imko=gmdisplaymessagebox('','File already exist.Overwrite it?',&
							gquestion,gyesno)
							if(imko.eq.gnobutton) goto 1
			
					endif
				endif
				OPEN(unit=12,file=filasc,status='UNKNOWN',&
                access='SEQUENTIAL',form='FORMATTED')
	            if(ncurvd.gt.0) then
		        write(12,FMT=8000) ' number of data curves = ',ncurvd
8000		    format(a25,i10)
	            j=1
		do j1=1,ncurvd
		   j=icurvd(j1)
		   write(unit=12,fmt=800) ndat(j)
800		   format(i10)
		   do i=1,ndat(j)
			write(unit=12,fmt=801) Xval(i,j),Yval(i,j)
801			format(2f15.5)
		   enddo
		enddo
	   endif
	   if(ncurvc.gt.0) then
		write(12,FMT=8000) ' number of calc curves = ',ncurvc
	      j=1
		do j1=1,ncurvc
		   j=icurvc(j1)
		   write(unit=12,fmt=800) ncal(j)

		   do i=1,ncal(j)
			write(unit=12,fmt=801) Xcal(i,j),Ycal(i,j)

		   enddo
		enddo
	   endif
	   CLOSE(unit=12)
		endif		
		endif
			
!===================================================================================		 
		case(6001)   ! 'Analysis of repeated simulated fits' SIMAN
			nsims=nsims1
			if(allocated(thetval)) deallocate(thetval)
			if(allocated(nintval)) deallocate(nintval)
			if(allocated(ixval)) deallocate(ixval,iyval,izval)
			if(allocated(elmval)) deallocate(elmval)
			if(allocated(elmset)) deallocate(elmset)
			if(allocated(nevals)) deallocate(nevals)
			if(allocated(ec50val)) deallocate(ec50val)
			if(allocated(ytemp)) deallocate(ytemp)
			if(allocated(iomit)) deallocate(iomit)
			ALLOCATE(thetval(npar,nsims),nintval(nset,nsims),&
		    ixval(nsims),iyval(nsims),izval(nsims),elmval(nsims),&
			elmset(nset,nsims),nevals(nsims),ec50val(nsims))
			ALLOCATE(ytemp(nsims),iomit(nsims))
			if(allocated(thtrue)) deallocate(thtrue)
						allocate(thtrue(200))
						do i=1,200
							thtrue(i)=0.d0
						enddo
			if(idestf.eq.10) then
				icallsim=0
			else
				icallsim=5
				call gmremovewindow(initialw)
					call gmDefineKeyselectCallback(13,0)
			endif
			call gmSetWidgetStatus(jtitle_record, GSELECTABLE)
		
			call gmSetWidgetStatus(jlabels, GSELECTABLE)
			call gmSetWidgetStatus(jnewtext, GSELECTABLE)
			call gmSetWidgetStatus(jnumbers, GSELECTABLE)
			call gmSetWidgetStatus(jaxis, GSELECTABLE)
			call gmSetWidgetStatus(jframe, GSELECTABLE)
		
			call gmSetWidgetStatus(iSave_bmp, GSELECTABLE)
			call gmSetWidgetStatus(Export_file, GSELECTABLE)
			call gmSetWidgetStatus(Print_file, GSELECTABLE)
			
			call simanw(main,thetval,nintval,ixval,iyval,izval,elmval,elmset,nevals,ec50val,&
			nsims,npar,nset,imod0,ndimd,ndimc,titlep,simfile1,thtrue,id,iomit,nomit,&
			icallsim,newrecords,igraphsim,combo1_1,COMBO1_2)
			!iplot=igraph
			!modplot=igraph
			if(icallsim.eq.5) then
				icall=0
				icallsim=0
				call welcome(main,initialw,initialw_MainPanel,inifile, initialw_Text5,initial_toggle,&
				initialw_Toggle3,initialw_Toggle4,initialw_Toggle5,initialw_Text2,pfilem,&
                textcomp,textid,drives)
				call gmSetToggleSwitch(initial_Toggle(1),Gon)
				call windra(initialw)
			endif	
		
!===================================================================================		 
		! MOVE MOUSE -SELECT FRAME/MODEL
		case(7101:7200)   ! move mouse
		  if(readrec.and.modplot.gt.0) then
		   
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
			if(mflag.eq.1.and.move_text) then
			call write_string(oldrecords(modplot)%str(jindex),oldrecords(modplot)%attributes%rx(jindex),&
			oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),&
			0,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
			if(jindex.ge.6.and.jindex.le.30) then
				do m=6,30
					call write_string(oldrecords(modplot)%str(m),oldrecords(modplot)%attributes%rx(m),&
			oldrecords(modplot)%attributes%ry(m),oldrecords(modplot)%attributes%angle(6),&
			oldrecords(modplot)%attributes%ijus(6),oldrecords(modplot)%attributes%ifnt(6),&
			oldrecords(modplot)%attributes%sizetext(6),&
			0,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
			
				enddo
				else if (jindex.ge.31.and.jindex.le.55) then
					do m=31,55
					call write_string(oldrecords(modplot)%str(m),oldrecords(modplot)%attributes%rx(m),&
			oldrecords(modplot)%attributes%ry(m),oldrecords(modplot)%attributes%angle(31),&
			oldrecords(modplot)%attributes%ijus(m),oldrecords(modplot)%attributes%ifnt(31),&
			oldrecords(modplot)%attributes%sizetext(m),&
			0,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
		
				enddo
				endif
		
				dxm=xmov-rx(jindex)
				dym=ymov-ry(jindex)
				oldrecords(modplot)%attributes%rx(jindex)=xmov
				oldrecords(modplot)%attributes%ry(jindex)=ymov
				rx(jindex)=xmov
				ry(jindex)=ymov
					if(jindex.ge.6.and.jindex.le.30) then
					do m=6,30
					if(m.ne.jindeX) then
						rx(m)=rx(m)+dxm
						ry(m)=ry(m)+dym
						oldrecords(modplot)%attributes%rx(m)=rx(m)
				oldrecords(modplot)%attributes%ry(m)=ry(m)
			
				endif
					enddo
				endif
					if(jindex.ge.31.and.jindex.le.55) then
					do m=31,55
					if(m.ne.jindeX) then
						rx(m)=rx(m)+dxm
						ry(m)=ry(m)+dym
						oldrecords(modplot)%attributes%rx(m)=rx(m)
				oldrecords(modplot)%attributes%ry(m)=ry(m)
			
				endif
					enddo
				endif
				call draw_axis(xmin,xmax,ymin,ymax,ymin1,ymax1,xtic,ytic,&
					nx1,ny1,xcross,ycross,itx,ity,tlenx,tleny,logx,logy,sqrty,doframe,&
					icol,ntx,nty,idev,thick,itype,&
		            calbarx,calbary,xbeg4,ybeg4,xend4,yend4,&
                    xbeg5,ybeg5,xend5,yend5)
					if(hdisp) then
					call draw_hist(xval,yval,icurvd,ncurvd,ndelt,logity,logx,&
						logy,sqrty,y0,yinf,x0,ilog,xmin,xmax,ymin,ymax,idev,ndat,ijoin,&
						icol,thick,ndv1,ndimd,xwbase,lt2)
								
                else
					call draw_data(xval,yval,icurvd,ncurvd,ndelt,logity,logx,logy,sqrty,&
						y0,yinf,x0,ilog,idev,ndat,ijoin,icol,isym,&
						symsiz,thick,barleng,interp,npint,Xint,Yint,nintg,yp1,ypn,&
						Y2int,ndv1,ndimd,w,kwi,kwj,dxs,dys,icurvw,jmiss,n1,idraw,&
					ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil)
				endif
				call draw_curves(xcal,ycal,icurvc,ncurvc,logity,logx,&
					logy,sqrty,y0,yinf,x0,ilog,idev,&
						wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
						xmin,xmax,ymin,ymax,ncal,&
					iline,icol,thick,ndc1,ndimc,jfirst,dxs,dys)
				
			    call draw_lines(IDRAW,ICOL,THICK,C_THICK,NARROW,NLINE,NHLINE,NVLINE, &
				XBEG,YBEG,XEND,YEND,y0,yinf,x0,dxs,dys) 
					call draw_text(parval,xtitle,ytitle,ztitle,title1,cnumx,cnumy,&
					cnumz,cexpx,cexpy,cexpz,newtext,numbx,numby,numbz,ntext,inumx,&
					inumy,intx,angle,sizetext,ijus,thick,rx,ry,idraw,icol,ifnt,str,dxs,dys)
		
			endif
			if(s_text.and.lframe.eq.graphics1_1(modplot)) then
			!	call show_boxes(xmov,ymov,mm,str,rx,ry,angle,ijus,sizetext,icol,itemp,&
			!	idraw,inewpos,dxs,dys)
			endif
		!	xmov0=xmov 
		!	ymov0=ymov
		  ENDIF		
		  case(7201:7300)	   !mouse select graph or what to do on graph
		  
			if(readrec.and.modplot.gt.0) then
				modplot=callid-7200
				igraph=modplot
				call gmActivateGraphicsFrame(graphics1_1(modplot))
				lframe=graphics1_1(modplot)
			
			!	IND_1=nrmodel(modplot)
			    jindex=ind_1
				if(oldrecords(modplot)%IPOS.eq.0) then
				CALL gmEnqWidgetInfo(graph1_1(modplot),Widget)
				ixp=widget%xpos
				iyp=widget%ypos
				endif
				if(oldrecords(modplot)%IPOS.eq.0) then
					isens=0
				igraph=modplot
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
					!	ALLOCATE(ndat(ndimd),isym(ndimd),ijoin(ndimd),symsiz(ndimd))
						ALLOCATE(ndat(njset),isym(njset),ijoin(njset),symsiz(njset))
						ALLOCATE(ncal(ndimc),iline(ndimc))
						ALLOCATE(icurvd(ndimd),icurvc(ndimc))
				call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xval,yval,w,nj,niobs,njset,nplot,nset,&
				juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
				wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
				ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
				xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,oldrecords,&
				dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,parval,ifitype)
		
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
				CALL ACTENQ(CALLid,IDENT,IKEY,ISTATmove,Xac,Yac,acTEXT,nacargs,ARGS,IARGS)
			if (d_text.or.s_text) then
				if(ikey.eq.-1)	then
				move_text=.true.
				if(mflag.eq.1) then
					mflag=0
					s_text=.false.
				!	indx=0
				else if(mflag.eq.0) then
					mflag=1
					s_text=.true.
				endif	
				endif
				if(ikey.eq.-3) then
					mflag=0
					s_text=.false.
				endif
				goto 1
			endif 	 
				call mouse_select(ind_1,npoint,jtemp,d_line, d_hline, d_vline, d_arrow, &
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
			!	mi=0
				if(mi.eq.0) then
				do m=1,5
					do l=1,4
						rxbox1(l)=rxbox(l,m)
						rybox1(l)=rybox(l,m)
					enddo
					call inside(xmov,ymov,rxbox1,rybox1,iflag)
					if(iflag.eq.1) mi=m
				enddo
				endif
				if(mi.ne.0) then
					ind_1=mi
					jindex=ind_1
					if(mflag.eq.1) then
						call gmDefineKeySelectCallback(533,0) !left Sets up escape key as escape key!
						call gmDefineKeySelectCallback(534,0) !right
						call gmDefineKeySelectCallback(535,0) !up
						call gmDefineKeySelectCallback(536,0) !down
						!left Sets up escape key as escape key!
						call gmDefineKeySelectCallback(13,0)
						mflag=0
						mi=0
						call gmSetGuiCursor(lframe,Gdefault,gdefault)
						call string_box(oldrecords(modplot)%str(jindex),oldrecords(modplot)%attributes%rx(jindex),&
						oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
						oldrecords(modplot)%attributes%ijus(jindex),&
						oldrecords(modplot)%attributes%sizetext(jindex),0,0,xbox,ybox,&
						oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
						move_text=.false.
						icol(jindex)=oldrecords(modplot)%attributes%icol(jindex)
						do l=1,4
						rxbox(l,jindex)=xbox(l)
						rybox(l,jindex)=ybox(l)
						oldrecords(modplot)%attributes%rxbox(l,jindex)=xbox(l)
						oldrecords(modplot)%attributes%rybox(l,jindex)=ybox(l)
						enddo
						if(jindex.ge.6.and.jindex.le.30) then
		
						do m=6,30
						call string_box(oldrecords(modplot)%str(m),oldrecords(modplot)%attributes%rx(m),&
						oldrecords(modplot)%attributes%ry(m),oldrecords(modplot)%attributes%angle(6),&
						oldrecords(modplot)%attributes%ijus(6),&
						oldrecords(modplot)%attributes%sizetext(6),0,0,xbox,ybox,&
						oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
					

						do l=1,4
						rxbox(l,m)=xbox(l)
						rybox(l,m)=ybox(l)
						oldrecords(modplot)%attributes%rxbox(l,m)=xbox(l)
						oldrecords(modplot)%attributes%rybox(l,m)=ybox(l)
						enddo
						icol(m)=oldrecords(modplot)%attributes%icol(m)
						call write_string(oldrecords(modplot)%str(m),oldrecords(modplot)%attributes%rx(m),&
						oldrecords(modplot)%attributes%ry(m),oldrecords(modplot)%attributes%angle(6),&
						oldrecords(modplot)%attributes%ijus(6),oldrecords(modplot)%attributes%ifnt(6),&
						oldrecords(modplot)%attributes%sizetext(6),&
						icol(m),oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
				
						enddo
						else if (jindex.ge.31.and.jindex.le.55) then
				
						do m=31,55
						call string_box(oldrecords(modplot)%str(m),oldrecords(modplot)%attributes%rx(m),&
						oldrecords(modplot)%attributes%ry(m),oldrecords(modplot)%attributes%angle(31),&
						oldrecords(modplot)%attributes%ijus(31),&
						oldrecords(modplot)%attributes%sizetext(31),0,0,xbox,ybox,&
						oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
					

						do l=1,4
						rxbox(l,m)=xbox(l)
						rybox(l,m)=ybox(l)
						oldrecords(modplot)%attributes%rxbox(l,m)=xbox(l)
						oldrecords(modplot)%attributes%rybox(l,m)=ybox(l)
						enddo
						icol(m)=oldrecords(modplot)%attributes%icol(m)
						call write_string(oldrecords(modplot)%str(m),oldrecords(modplot)%attributes%rx(m),&
						oldrecords(modplot)%attributes%ry(m),oldrecords(modplot)%attributes%angle(31),&
						oldrecords(modplot)%attributes%ijus(m),oldrecords(modplot)%attributes%ifnt(31),&
						oldrecords(modplot)%attributes%sizetext(m),&
						icol(m),oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
				
						enddo
						endif
						call write_string(oldrecords(modplot)%str(jindex),oldrecords(modplot)%attributes%rx(jindex),&
						oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
						oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
						oldrecords(modplot)%attributes%sizetext(jindex),&
						oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
		
					else
					

						mflag=1	
						call gmDefineKeySelectCallback(533,7301) !left Sets up escape key as escape key!
						call gmDefineKeySelectCallback(534,7302) !right
						call gmDefineKeySelectCallback(535,7303) !up
						call gmDefineKeySelectCallback(536,7304) !down
						call gmDefineKeySelectCallback(13,7305)
		
						if(jindex.ge.6.and.jindex.le.30) then
						do m=6,30
						call write_string(oldrecords(modplot)%str(m),oldrecords(modplot)%attributes%rx(m),&
						oldrecords(modplot)%attributes%ry(m),oldrecords(modplot)%attributes%angle(6),&
						oldrecords(modplot)%attributes%ijus(6),oldrecords(modplot)%attributes%ifnt(6),&
						oldrecords(modplot)%attributes%sizetext(6),&
						12,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
						icol(m)=12
						enddo
						else if (jindex.ge.31.and.jindex.le.55) then
						do m=31,55
						call write_string(oldrecords(modplot)%str(m),oldrecords(modplot)%attributes%rx(m),&
						oldrecords(modplot)%attributes%ry(m),oldrecords(modplot)%attributes%angle(31),&
						oldrecords(modplot)%attributes%ijus(m),oldrecords(modplot)%attributes%ifnt(31),&
						oldrecords(modplot)%attributes%sizetext(m),&
						12,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
						icol(m)=12
						enddo
						else	
						call write_string(oldrecords(modplot)%str(jindex),oldrecords(modplot)%attributes%rx(jindex),&
						oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
						oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
						oldrecords(modplot)%attributes%sizetext(jindex),&
						12,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
						icol(jindex)=12
						endif
				!	call write_string(oldrecords(modplot)%str(jindex),oldrecords(modplot)%attributes%rx(jindex),&
				!	oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
				!	oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
				!	oldrecords(modplot)%attributes%sizetext(jindex),12,&
				!	oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
				!	callid=jindex
				!	goto 2
						move_text=.true.
					endif
				endif
				endif
				endif
			endif
			
			
	case(-1000) ! view text file
	if(open7) then
 		imes=gmdisplaymessagebox('','View text file',gquestion,gyesno)
		if(imes.eq.gyesbutton) then
	    icp=-1000
		callid=8003
		goto 2
		else
			goto 1000
		endif
	endif
	case(7001:7100)
		call gmEnqActionState(actlst)	
		if(actlst%status.eq.2) jopen(callid-7000)=-1 ! window closed  
	case(8005) ! view text file
		call gmremovewindow(inote)
		iopinote=0
		if(icp.eq.-1000) goto 1000
	case(8006)
		do j=1,nset
		do i=1,nfileb(j)	
				call gmenqCellSetting(initwin1_TxtArray, i,j ,rval,pfiles(i,j))
				
				if(pfiles(i,j).eq.' ') goto 1
					NB=len_trim(pfiles(i,j))
			ilp=1
			do m=1,nb
			if(pfiles(i,j)(m:m).eq.'\') ilp=m

			enddo
			pfileb(i,j)=pfiles(i,j)(ilp+1:nb)

		enddo
		enddo
			
			callid=8004
			icalprevm=-8
			goto 2
	case(8007)
		call gmenqtextsetting(iTextwin2,text1)
		
		if(text1.ne.' ') then
			!nb=len_trim(pathmec)
			qmec=text1
			NB=len_trim(qmec)
			ilp=1
			do i=1,nb
			if(qmec(i:i).eq.'\') ilp=i

			enddo
			qfilem=qmec(ilp+1:nb)
			callid=8004
			icalprevm=-8
			goto 2
		endif
    case(8040)
        call gmenqtextsetting(initwin_Text2,text1)
		
		if(text1.ne.' ') then
			!nb=len_trim(pathmec)
			qmec=text1
			NB=len_trim(qmec)
			ilp=1
			do i=1,nb
			if(qmec(i:i).eq.'\') ilp=i

			enddo
			qfilem=qmec(ilp+1:nb)
		endif
		do j=1,nset
		do i=1,nfileb(j)	
		
				call gmenqCellSetting(initwin_TxtArray, i,j ,rval,pfiles(i,j))
				
				if(pfiles(i,j).eq.' ') goto 1
					NB=len_trim(pfiles(i,j))
			ilp=1
			do m=1,nb
			if(pfiles(i,j)(m:m).eq.'\') ilp=m

			enddo
			pfileb(i,j)=pfiles(i,j)(ilp+1:nb)

		enddo
		enddo
        callid=8004
        icalprevm=-8
		goto 2
	case(8004) ! save to ini
	if(ioptm.eq.3.and.icalprevm.ne.-8)then
	    imessy=gmDisplayMessageBox('','Did you save the new mechanism first ?',Gquestion,gyesno)
	    
	    if(imessy.eq.gnobutton) then 
	        callid=4434
	        goto 2 
	    else
	        
	    endif       
	endif
	do j=1,nset
		tresolb(j)=tresol(j)
	enddo
	if(jgraph.gt.0) then
		ka=models(jgraph)%ka
		k=models(jgraph)%n
		kf=k-ka
	endif
		CALL gmFileBROWSER(iniFILE,inipath,inidef,gmBrowseType=1)
			 IF(iniFILE.ne.' ') then
			 nl=len_trim(inipath)
			 inifile=inipath(1:nl)//'\'//inifile
			 call gmsettextsetting( initialw_Text5,inifile)
			 inifile0=inifile
			 endif
		INQUIRE (FILE=iniFILE,EXIST=PRESENT,&
			ACCESS=FACCESS,FORM=FFORM,RECORDTYPE=FRTYPE,RECL=IRCL) 

	if(PRESENT) then
	   
		ihandle=FILE$FIRST
		length = GETFILEINFOQQ(inifile, info, ihandle)
		nLEN=info%length
	    if(nlen.lt.100) then  
		imes=gmdisplaymessagebox('','File does not exist.Create a new file',ginformation,gok)
		iftype=100
		 open(unit=19,file=inifile,status='UNKNOWN',&
            access='DIRECT',form='BINARY',recl=1)
		 write(19,rec=1) iftype
		
		close(unit=19)
        else
		if(nlen.le.20480) then	
			    
			    img=gmdisplaymessagebox('','Overwrite dos ini file?',gquestion,gyesno)
	            if(img.eq.gnobutton) goto 1
		        iftype=100
		        endif	
		endif
	else
	!	if(inifile.eq.'  ') goto 1
		imes=gmdisplaymessagebox('','File does not exist.Create a new file',ginformation,gok)
		open(unit=19,file=inifile,status='UNKNOWN',&
        access='DIRECT',form='BINARY',recl=1) 
		 write(19,rec=1) iftype
		
		close(unit=19)
	endif
		kf=k-ka
		goto 2297
		ncyc2=ncyc
		do i=1,ncyc
		nsc2(i)=nsc(i)
		do j=1,nsc(i)
		    im2(i,j)=im(i,j)
		    jm2(i,j)=jm(i,j)
		enddo
		enddo
2297    continue
        if(ioptm.eq.3.or.ioptm.eq.4) then
        ncyc2=ncyc
		do i=1,ncyc
		nsc2(i)=nsc(i)
		do j=1,nsc(i)
		    im2(i,j)=im(i,j)
		    jm2(i,j)=jm(i,j)
		enddo
		enddo    
        endif   
        
		call write_ini(inifile,pfileb,nfix,jfix,neq,IE,JE,EFAC,&
        IF,JF,jcon,IM2,JM2,jmic,ndisp,irecq,ans1,ylo,yhi,nrange,idiskq,&
        nfileb,kfile,nset,conc,tcrit,burst,&
        idatyp,qfile,imod0,setbad,tcbad,onechan,&
        nbad1,isbad,iebad,tresolb,ans3,ans4,nlvar,&
        fixec50,ec501,i50,j50,m50,xqlo,xqhi,kA,kF,&
        chsvec,ncyc2,nsc2,qfilem,nsim,irect,logsav,imods,&
        badend,excop,gaplo,gaphi,dcmod,nlen, &
     	nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,useprim,pfiles,qmec,nmr,nlig,&
        conc_ec1,conc_ec2,iftype)
		!	if(icprev.eq.-14) then
		!	callid=-77
		!	icprev=0
		!	goto 2
		!	endif
	case(8003) ! view text
		nb=len_trim(pfilem)
		i=1
		do while (pfilem(I:i).eq.' ')
			i=i+1
				
		enddo
		if(i.le.1) then
			k=1
        else
		k=i+2
		endif
		if(iopinote.eq.-1) then 
				call gmremovewindow(inote)
				iopinote=0
			endif
	
			inote=gmCreateComplexDialogueBox(Main,10,2, 24, 25, GALL, '', &
              	 gmhpos=gleft,gmvpos=gtop,gmIconFormat=GDLLICON,gmIconFile='Gee')
			ipanelnote= gmCreatePanel(inote, 0, 0, 24, 25, &
              	gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0, &
              	gmScrollMode=GnoBARS, gmVpos=GTOP, gmExpand=GOFF)
			inotetext=gmCreateTextEntry(ipanelnote,0,1, 23, 22,'HJCFIT', 32768, Gdisplay, &
              	gmType=Grichedit, gmFont=GDEFAULT, gmJustify=Gleft, &
              	gmBack1Col=150, gmBack2Col=0, gmTextCol=0,gmscrollmode=gvertical, &
              	gmhpos=gleft,gmVpos=GTOP, gmExpand=GOFF)
			inoteButton = gmCreatePushButton(ipanelnote, 0, 0, 24, 1, 'Close', &
            gmVpos=Gbottom,gmType=GDEFAULTBUTTON ,gmcallback=8005)
			call gmdrawwindow(inote)
			string=pfilem(k:nb)
			imes=gmgettextfromfile(inotetext,string)
			
		!	call gmdrawwindow(inote)
			iopinote=-1
   case(8049)
   call gmremovewindow(initialpage)
   nmod=imodel
			nmod9=imodel
			ifirst3=-1
	ifirst4=-1
	ylo3=0.025
	yhi3=0.100
	nligsav=nlig
	ich=ichar(titpfree(1)(7:7))
    call calc_liksurf(main,iplotlik,ivalk1,ivalk1_1,ivalk1_2,ivalk2,ivalk2_1,ivalk2_2,&
           ivalk3,iTogglek_1,iTogglek_2)
   case(8050) !Calculation of log(likelihood)
        ipp1=gmenqvaluesetting(ivalk1)
        p1l=gmenqvaluesetting(ivalk1_1)
        p1h=gmenqvaluesetting(ivalk1_2)
        ipp2=gmenqvaluesetting(ivalk2)
        p2l=gmenqvaluesetting(ivalk2_1)
        p2h=gmenqvaluesetting(ivalk2_2)
        ncalclik=gmenqvaluesetting(ivalk3)
        jalpha=ipp1
        jbeta=ipp2
        idebug=11
        alow=dble(p1l)
	    ahigh=dble(p1h)
	    blow=dble(p2l)
	    bhigh=dble(p2h)
	    iliksw=0
	    kab=ncalclik*ncalclik
	    if(jalpha.le.0.or.jbeta.le.0.or.ncalclik.eq.0) iliksw=-1
	    if(alow.eq.ahigh.or.blow.eq.bhigh) iliksw=-1
	    if(iliksw.eq.-1) then
	        imyss=gmdisplaymessagebox('','wrong values !',gstop,gok)
	        iliksw=0
	        goto 1
	    else
	    
	        istatus=gmEnqToggleSwitch(iTogglek_1)
	       
	        if(istatus.eq.gon) then
	            logsurf=.true.
	            da=(dlog10(ahigh)-dlog10(alow))/dfloat(ncalc-1)
		        db=(dlog10(bhigh)-dlog10(blow))/dfloat(ncalc-1)
	        else
		        logsurf=.false.
	            da=(ahigh-alow)/dfloat(ncalc-1)
	            db=(bhigh-blow)/dfloat(ncalc-1)
	        endif
	        first=.true.	
	        call gmremovewindow(iplotlik)
	    !    call gmRemoveWindow(Form1(igraph2,3))
	        oneset=.false.	!so calcs done for ALL data sets in HJCLIK
	        kflik=npar
	         if(allocated(alpha2)) deallocate(alpha2,beta2,aloglik)
	        ALLOCATE(alpha2(kab),beta2(kab),aloglik(kab))
	        goto 4554
	        imaintty=gmCreateComplexDialogueBox(main,5, 5, 20, 22, GALL, &
	'Calculation of log(likelihood) for range of values of 2 parameters', &
    gmVpos=GTOP, gmIconFormat=GDLLICON, gmIconFile='Gee')
	        ittypanel2=gmCreatePanel(imaintty, 0, 0,20 ,22 , &
              	gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmborderType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=41, gmFillBorder=0)
            linestty=100
            itty2= gmCreateTTYEntry(ittypanel2, 1, 1, 18, 18, linestty,'',&
				gmoffcol=1,gmoncol=1,gmtextcol=14) 
!				iButtontty1 = gmCreatePushButton(ittyPanel2,1,0 , 8, 1, 'Plot',gmType=GUSERDEFINED, &
!	gmhpos=gleft,gmVpos=Gbottom,gmOffcol=38, gmTextCol=1,&
!	gmcallback=8051)
            iButtontty2 = gmCreatePushButton(ittyPanel2,10,0 , 8, 1, 'Exit', gmType=GUSERDEFINED,&
	gmhpos=gleft,gmVpos=Gbottom,gmOffcol=123, gmTextCol=1,&
	gmcallback=8052)
	        call gmdrawwindow(imaintty)
4554        continue
           nset1=nset
           nset2=nset
            ci=theta0(jalpha)
            cj=theta0(jbeta)
             if(.not.allocated(Z00A)) then
             irt=max(kam,kfm)
		      !  ALLOCATE(Z00A(irt,irt,km),Z10A(irt,irt,km),Z11A(irt,irt,km))
				!ALLOCATE(Z00F(irt,irt,km),Z10F(irt,irt,km),Z11F(irt,irt,km))
				!ALLOCATE(XAF(irt,irt,irt),XFA(irt,irt,irt),QEXPQA(irt,irt),QEXPQF(irt,irt))
				ALLOCATE(Z00A(kAm,kFm,km),Z10A(kAm,kFm,km),Z11A(kAm,kFm,km))
				ALLOCATE(Z00F(kFm,kAm,km),Z10F(kFm,kAm,km),Z11F(kFm,kAm,km))
				ALLOCATE(XAF(kAm,kFm,kAm),XFA(kFm,kAm,kFm),QEXPQA(kFm,kAM),QEXPQF(kAm,kFm))
				allocate(EXPQA(20,20),EXPQF(100,100))
		    endif
	        do i=1,ncalclik
		        if(logsurf) then
		            theta0(jalpha)=alow*(10d0**(dfloat(i-1)*da))
		        else
		            theta0(jalpha)=alow + dfloat(i-1)*da
		        endif
		      
		        WRITE(string,fmt='(a22,i3,a18,i3,a3,g13.6)') 'Calculating for rate ',jbeta,' values for rate ',jalpha,'  =',&
		            theta0(jalpha)
			
		        call gmsettextsetting(itty,string)
		        do j=1,ncalclik
		            if(logsurf) then
			            theta0(jbeta)=blow*(10d0**(dfloat(j-1)*db))
		            else
			            theta0(jbeta)=blow + dfloat(j-1)*db
		            endif
		            elmax=HJCLIK(-2,kflik,theta0,tint,ampl,iprops,nd1,nd2,Z00A,Z10A,Z11A,&
		            Z00F,Z10F,Z11F,XAF,XFA,QexpQA,QexpQF,alpha2,beta2,aloglik,kab,kA,kF,km,&
		            irt,npar,IQf,irate1,jrate1,nlig,EXPQF,EXPQA,qt,qd)
		           WRITE(string,fmt='(a15,g13.6,a15,g13.6,a5,g13.6)') titlep(jalpha)(1:10)//'=',theta0(jalpha),&
		           titlep(jbeta)(1:10)//'=',theta0(jbeta),'LL= ',elmax
		           call gmsettextsetting(itty,string)
		        enddo
		        continue
		        enddo	
	            
	            
		        CALL gmFileBROWSER(filasc,pdir,pfilt,gmBrowseType=1)
		        if(filasc.ne.' ') then
		        OPEN(unit=12,file=filasc,status='UNKNOWN',access='SEQUENTIAL',form='FORMATTED')
	            write(unit=12,fmt=804) nab
804	            format(i11)

	            if(logsurf) then
		        do i=1,nab
		            write(unit=12,fmt=805) alpha2(i),beta2(i),aloglik(i)
805		           format(3f15.5)
		        enddo
	            else
		        do i=1,nab
		            write(unit=12,fmt=805) dlog10(alpha2(i)),dlog10(beta2(i)),aloglik(i)
		        enddo
	            endif
                !write the values of the other npar-2 rates that are constant
	            do m=1,npar
		        if(m.ne.jalpha.and.m.ne.jbeta) then
		            write(unit=12,fmt=8051) titlep(m),theta0(m)
8051		        format(a10,f15.5)
		        endif
	            enddo

	            CLOSE(unit=12)
	            if(discprt) then
		            write(7,807) ncalc,alow,ahigh,ncalc,blow,bhigh,ncalclik*ncalclik,filasc
807		            format(&
                    1x,i5,' values of alpha2, in range ',g13.6,' to ',g13.6,/,&
                    1x,i5,' values of beta2,  in range ',g13.6,' to ',g13.6,/,&
                    1x,i5,' values written to ascii file ',a40,/)
		            if(logsurf) then
		                write(7,8071)
8071		            format(' Values logarithmically spaced')
		            endif
	            endif
	            endif
	            ntoglik=5
	            itcall=8051
	            text_tog(1)='Plot surface'
	            text_tog(2)='Fit '
	            text_tog(3)='Show data and specified model (no fit)'
	            text_tog(4)='Show only curves for specified model'
	            text_tog(5)='Exit'
	            call toggle_panel(Main,ITOGGLE,ittoglepanel,ntoglik,text_tog,intoggle,itcall,valdat,idat,itogbutton,itype)
            endif

case(8051)
                
	            do i=1,ntoglik
		            istatus=gmEnqToggleSwitch(inToggle(i))
				    if(istatus.eq.1) ioptlik=i
		        enddo
		        call gmremovewindow(itoggle)
	            if (ioptlik.eq.1) then
	            else
	                liksurf=.false.
	                deallocate(alpha2,beta2,aloglik)
	           	!must allocate anyway
	                if(ioptlik.eq.5) then
	                    goto 1000
	                else
	                    theta0(jalpha)=ci
	                    theta0(jbeta)=cj
	                    kab=1	
	                    if(.not.allocated(alpha2)) then
		                    ALLOCATE(alpha2(kab),beta2(kab),aloglik(kab))
		                endif
		                if(ioptlik.eq.2) callid=-147
		                if(ioptlik.eq.3) callid=-148
		                if(ioptlik.eq.4) callid=-149
		                goto 2
		            endif
	            endif
	    
  
   end select
   
enddo
	
if(open7) then
 	imes=gmdisplaymessagebox('','View text file',gquestion,gyesno)
	if(imes.eq.gyesbutton) then
			comm='c:\windows\notepad '// pfilem
			resultcom=systemqq(comm)
			
	endif
endif


1000 imes=gmdisplaymessagebox('','Write to ini file',gquestion,gyesno)
    
	if(jgraph.gt.0) then
	    ka=models(jgraph)%ka
	    k=models(jgraph)%n
	
	    kf=k-ka
	endif
	do j=1,nset
		tresolb(j)=tresol(j)
	enddo
    if(imes.eq.gyesbutton) then	
    if(ioptm.eq.3.and.icalprevm.ne.-9)then
	    imessy=gmDisplayMessageBox('','Did you save the new mechanism first ?',Gquestion,gyesno)
	    if(imessy.eq.gnobutton) then 
	        callid=4434
	        goto 2 
	     
	    endif       
	endif
	CALL gmFileBROWSER(iniFILE,inipath,inidef,gmBrowseType=1)
    IF(iniFILE(1:10).ne.'         ') then
			 nl=len_trim(inipath)
			 inifile=inipath(1:nl)//'\'//inifile
			 call gmsettextsetting( initialw_Text5,inifile)
			 inifile0=inifile
			 
		INQUIRE (FILE=iniFILE,EXIST=PRESENT,&
			ACCESS=FACCESS,FORM=FFORM,RECORDTYPE=FRTYPE,RECL=IRCL) 

	  if(PRESENT) then
	
		ihandle=FILE$FIRST
		length = GETFILEINFOQQ(inifile, info, ihandle)
		nLEN=info%length
	   if(nlen.lt.100) then
			imes=gmdisplaymessagebox('','File does not exist.Create a new file',ginformation,gok)
		iftype=100
		endif
		readini=.true.
		IRECL=10240
		OPEN(UNIT=19,FILE=iniFILE,STATUS='UNKNOWN',&
        ACCESS='DIRECT',FORM='UNFORMATTED',RECL=irecl)
		INQUIRE(IOLENGTH=LEN) iniFILE
		   
		close(unit=19)
		goto 2299
		ncyc2=ncyc
		do i=1,ncyc
		nsc2(i)=nsc(i)
		do j=1,nsc(i)
		    im2(i,j)=im(i,j)
		    jm2(i,j)=im(i,j)
		enddo
		enddo
2299    continue	
        if(ioptm.eq.3.or.ioptm.eq.4) then
        ncyc2=ncyc
		do i=1,ncyc
		nsc2(i)=nsc(i)
		do j=1,nsc(i)
		    im2(i,j)=im(i,j)
		    jm2(i,j)=jm(i,j)
		enddo
		enddo    
        endif   	
		call write_ini(inifile,pfileb,nfix,jfix,neq,IE,JE,EFAC,&
        IF,JF,jcon,IM2,JM2,jmic,ndisp,irecq,ans1,ylo,yhi,nrange,idiskq,&
        nfileb,kfile,nset,conc,tcrit,burst,&
        idatyp,qfile,imod0,setbad,tcbad,onechan,&
        nbad1,isbad,iebad,tresolb,ans3,ans4,nlvar,&
        fixec50,ec501,i50,j50,m50,xqlo,xqhi,kA,kF,&
        chsvec,ncyc2,nsc2,qfilem,nsim,irect,logsav,imods,&
        badend,excop,gaplo,gaphi,dcmod,nlen, &
     	nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,useprim,pfiles,qmec,nmr,nlig,&
		conc_ec1,conc_ec2,iftype)
	else
	    !if(inifile.eq.' ') goto 1
		imes=gmdisplaymessagebox('','File does not exist.Create a new file',ginformation,gok)
		!inifile='hjcfit.ini'
		iftype=100
		IRECL=10240
		OPEN(UNIT=19,FILE=iniFILE,STATUS='UNKNOWN',&
        ACCESS='DIRECT',FORM='UNFORMATTED',RECL=irecl)
		 write(19,rec=1) iftype
		 goto 2298
	ncyc2=ncyc
		do i=1,ncyc
		nsc2(i)=nsc(i)
		do j=1,nsc(i)
		    im2(i,j)=im(i,j)
		    jm2(i,j)=im(i,j)
		enddo
		enddo
2298    continue
		close(unit=19)
		call write_ini(inifile,pfileb,nfix,jfix,neq,IE,JE,EFAC,&
        IF,JF,jcon,IM2,JM2,jmic,ndisp,irecq,ans1,ylo,yhi,nrange,idiskq,&
        nfileb,kfile,nset,conc,tcrit,burst,&
        idatyp,qfile,imod0,setbad,tcbad,onechan,&
        nbad1,isbad,iebad,tresolb,ans3,ans4,nlvar,&
        fixec50,ec501,i50,j50,m50,xqlo,xqhi,kAm,kFm,&
        chsvec,ncyc2,nsc2,qfilem,nsim,irect,logsav,imods,&
        badend,excop,gaplo,gaphi,dcmod,nlen, &
     	nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,useprim,pfiles,qmec,nmr,nlig,&
		conc_ec1,conc_ec2,iftype)
	endif
	else
	    goto 1
	endif
endif 

INQUIRE (FILE=iniiniFILE,EXIST=PRESENT)
if(PRESENT) then
			ihandle=FILE$FIRST
			length = GETFILEINFOQQ(iniinifile, info, ihandle)
			nLEN=info%length
			readini=.true.
			IRECL=10240
			OPEN(UNIT=13,FILE=iniiniFILE,&
            ACCESS='DIRECT',FORM='UNFORMATTED',RECL=irecl)
		    write(13,rec=1) itogrec1,inifile,pfilem,nodata,curvonly,nofit,autosim,discprt,append
			close(unit=13)
endif

! Close device MENU and GINO, and end program
   call gmCloseMenu
   call gCloseDevice
   call gCloseGino

if(open7) close(unit=7)

!if(allocated(pixbuf)) deallocate(pixbuf)
if(allocated(xobs)) deALLOCATE(xobs,yobs,w)
if(allocated(xval)) deALLOCATE(xval,yval)
if(allocated(xvalold)) deALLOCATE(xvalold)
if(allocated(yvalold))deALLOCATE(yvalold)
if(allocated(nj)) deALLOCATE(nj)
if(allocated(jmiss)) deALLOCATE(jmiss)
if(allocated(juse)) deALLOCATE(juse)
if(allocated(ndat)) deALLOCATE(ndat)
if(allocated(ncal)) deALLOCATE(ncal)	!for normalised data
if(allocated(setx)) deALLOCATE(setx)
if(allocated(njbase)) deALLOCATE(njbase)
if(allocated(icurvd)) deALLOCATE(icurvd)
if(allocated(icurvw)) deALLOCATE(icurvw)
if(allocated(icurvc)) deALLOCATE(icurvc)
if(allocated(isym)) deallocate(isym,symsiz,ijoin,iline)
if(allocated(xnum)) deallocate(xnum,xdata,ndatset)
	
if(allocated(theta)) deallocate(theta,theta1)
if(allocated(xcal)) deALLOCATE(xcal,ycal)
if(allocated(newrecords)) deallocate(newrecords)
if(allocated(oldrecords)) deallocate(oldrecords)
if(allocated(models)) deallocate(models)
if(allocated(ratcons)) deallocate(ratcons)
if (allocated(expqa)) deallocate(EXPQA,EXPQF,qexpqa,qexpqf)
if (allocated(Z00A)) DEALLOCATE(Z00A,Z10A,Z11A,Z00F,Z10F,Z11F)
if (allocated(qd)) deallocate(QD,QT,Peq)
stop
			
end


