integer indrec1(100)
character*10 label7(2)
integer::iwidk(2),irecfin(500)
integer nratek(500)
character*74 ratetitle(20,500)
character*74 mtitlek	!title for model
character*74 mtits(100)	
allocatable::ytemp,iomit
integer*4 iomit(:)
real*4 ytemp(:)
character*11 cdate2,ctime2
integer pixbufh(800,40)
integer pixbufv(40,800)
character title2j*79
integer kfile(20,10),nfile(10),nfileb(10),nintt(10)
logical vjump,sampv,control,abortw,resultcom,move_text
CHARACTER*100 jtitle(1000),textmr
integer*4 irecfst(1000)
character qdate*9
character*60 iniinifile
character*60 pathdata,pathmec
character*80 gifile
	character*8 ftype
	character*11 cstring
	character*26 drives
  	character inifile*40,filnam*40,inifile0*40,filasc*60
	character*40 :: inipath='*.*',textcomp,textid
	character*40 :: inidef='*.ini'//char(124)//'Initialization file (INI)'//&
	char(124)//'*.*'//char(124)//'All Files'
	character*40 :: ascpath='*.*'
	character*40 :: ascdef='*.txt'//char(124)//'ASCII file (txt)'//&
	char(124)//'*.*'//char(124)//'All Files'

	integer jstart(500),itypeval(50)
	real val(50)
	character*100 comm
	integer ival(50),num(10),num0(10)
	character*20 filepure,filepure1
	integer istatab(500)
	character*60 vtext(50),text1,text2,text3,text4,text5,pathdat1,pathdat2
	character*30 text_box(100,10),textbox
	character*60 text_boxf(20,10),title_box(10)
	integer itext_box(100,10),ititle(10),itext_boxf(20,10)
	logical three_d,modify(200),scale
	integer program_type
	character*33 qfile0
	allocatable::concsav
	real*4 concsav(:,:)
	logical import,export,rescale
	integer*1 ihlinrel(10)	!=1 if line pos rel to line 1
	integer*1 ivlinrel(10)	!=1 if line pos rel to line 1
	character*60 wtext(100)
    character*80 actext
	 character*74 rtitle,rtitles(500),rtitle1
    integer iargs(80)
    real args(80)
	integer itemp(100)
	character*33 adcfil
	logical screen,colplotter,calbarx,calbary
	ALLOCATABLE:: ndat,icurvd,icurvw,isym,ijoin,symsiz,theta,theta1
	ALLOCATABLE:: cormat
	real*4 cormat(:,:)
	real*4 ymaxj(100)
	real*4 syms(20)
	real ameant(100),areat(100)
	REAL SDEV(100),SDEVSAV(100),symsiz1(20)
	integer ndat(:),icurvd(:),icurvw(:),isym(:),ijoin(:),nsets(20),isym1(20)
	real symsiz(:)			
	ALLOCATABLE::xgrp,ybar,sdy,sdm,ngrp,nxval
	real*4 xgrp(:,:),ybar(:,:),sdy(:,:),sdm(:,:),xcal(:,:),ycal(:,:)
	integer ngrp(:,:),nxval(:),flag
	integer nkset(20),nkxset(20)
	ALLOCATABLE::xcalsav,ycalsav,xcal,ycal
	real*4 xcalsav(:,:),ycalsav(:,:),theta(:),theta1(:)
	ALLOCATABLE::ncalsav,icrvcsav,ilinesav,isline,ndatset
	integer ncalsav(:),icrvcsav(:),ilinesav(:),ndatset(:,:)
	ALLOCATABLE::xdata			
	real xdata(:,:,:,:)
	ALLOCATABLE::xobs,yobs,w,nj,xval,yval,njbase,weight,xvalold,yvalold
	ALLOCATABLE datcop,z3,bad,zeta1,zeta2
	real*4 z3(:,:),zeta1(:)
	logical bad(:,:),zeta2(:)
	logical fill,inter,axese,fillbad,cross,posneg,all,quarter,after,monsav,landplot
      INTEGER ISTYLE(6),ISUP(6)		
	ALLOCATABLE pdata
	real pdata(:),xvalold(:,:),yvalold(:,:)
	real datcop(:)
	real*4 xobs(:,:),yobs(:,:),w(:,:),xval(:,:),yval(:,:),weight(:,:)
	integer*4 nj(:),njbase(:)
	ALLOCATABLE setx,jmiss,juse
	integer jmiss(:),juse(:),iuse(100),icuse(100),jmiss0(20)
	character*20 TITLEP(200),ptitle(100)		!names of params for fit
	character*10 t1(100),titlep1(100),qij(100),tij(200)
integer data_list(50,50)
	!real*8 temp(100)
	character*60 titw(5)		!names of weighting methods
	real setx(:)
	character*200 parval
	character*100 xtext,ptext,xtext1,xtext2
	ALLOCATABLE::colhead
	character*20 colhead(:)

	logical dcfile,ascinput,HDISP,CJDAT,setsd,saved,fopen,pbmp
    logical fopen_11,fopen_12,twodisp,append,pcgm,pwmf,interp,viewed,overw,readrec
	logical d_line,d_hline,d_vline,d_arrow,d_poly,d_text,d_zoom,o_state,c_state,zoom
	logical s_text,s_line,s_arrow,sepy,newname
	real xint(2048),yint(2048),Y2int(2048)	!for interpolation
	allocatable::Xnum,ncal,icurvc,iline,icurvc1
	character*40 titlex,titley,titlez
	real*4 Xnum(:,:)
    INTEGER*4 jstrec(100),lstrec(100),jstrec1(100),lstrec1(100)
	integer*2 jstrec2(200),lstrec2(200),ititw(5)		!for old queue
	integer ncal(:),icurvc(:),iline(:),isline(:),icurvc1(:)		!for calc curve
	CHARACTER ascfil*33,stringf(80),string*256
!	allocatable::string
	logical allmode,sepmax,present,redo,sep
    character*18 ascnum
    CHARACTER*60 TITLE
	character*200 str(100)
	character*64 title1
	character datew*11,timew*8
    character*60 infil,ipfil,PFILE
    character*75 xtitle3,ytitle3,ztitle3
	character*75 title3
	dimension iasct(10),ifit(10),IPOINT(5),ikset(20),IMT(10)
    character*(60) text_tog(100)

    CHARACTER*70 TEXT,savtext(100)
    CHARACTER*40 MESSAGE,MESSAGE1,MESSAGE2
    CHARACTER*60  TITLEF,TITLED(20),TITMOD(40)
    CHARACTER*40 TITLEDS(10)
	CHARACTER*60 FTITLE(200),STITLE(10,200)
	character*70 mtitle(10,200)
	character*74 mtitle11,mtitle4,rtitle4
	character*100 ftitle1(200)
	CHARACTER*11 CHSET,cnkset(20)
	LOGICAL NEWFORM,norm,norm1,doframe,landscap,autplt,plotrue,super
	logical redrawn,plotcols,mono,ivplot,plot,entered,titprint
	character*10 cnumx(25),cnumy(25),cexpx(25),cexpy(25),cnumz(25)
	character*10 cexpz(25)
	integer  IPANEQ(40),itm(10),itgm(20),IHGM(20),indrec(100)
	LOGICAL GRAPH,logyfit,use1,skipq,negpar,cluster
	integer JFIX(200),jomit(20),jfixsav(100),iparsav(10)
	!integer jfix1(200),jcon(200),jmic(200),jbad(200)
	real xbox(4),ybox(4)
	LOGICAL CONSTR,FLINE,SETVAR,defolt1,fitted,dcurve,logt,badset
	logical xgt0,noerr,fixmax,fix0,readini,allocated,specsy
	logical nodata,hillang,autosep,neweq,guesdone,fixset,sameq
	logical noguess(100),apfile,DISCPRT,fixratio
	ALLOCATABLE::xobs1,yobs1,w1,nj1,xnorms !for normalised data
	real*4 xobs1(:,:),yobs1(:,:),w1(:,:),xnorms(:)
	integer*4 nj1(:),nplot_on(100)
	logical printed,shortprt,nodisp
	allocatable::ymaxsep
	real*4 ymaxsep(:)
	allocatable :: thetgues
	real*4 thetgues(:,:)      !100,nset1
	LOGICAL logx,logy,logity,sqrty,gino50
	logical bold,italik,underline
	character*75 xtitle,ytitle,ztitle	!output from LAXES
	character      tring*11
    CHARACTER*12 FACCESS,FBINARY,FFORM,FRTYPE,FACTION

	
	CHARACTER*80 MTEXT1,MTEXT2
    CHARACTER*80 NEWTEXT(20),MTEXT(10),param(100)
    DIMENSION IFTOG(20),ICTOG(32),IFONTS(20),IFENT(20),ICENT(32)
	DIMENSION ISCARA(10),ISHAPE(10),IEQN(40),IQMEN(10),IaSET(10)
	CHARACTER*7  TCOLS(32)
    CHARACTER*15 SCARA(7),SHAPE(7)
	CHARACTER*22 CNUM(10),CNUM1
    CHARACTER*15 TFONTS(20)

    DIMENSION Ititbad(100),Idat(100),itog(100),rxprev(100),ryprev(100)
    DIMENSION IFNT(100),IJUS(100),SIZEtext(100),ANGLE(100)
	DIMENSION RX(100),RY(100),RXBOX(4,100),RYBOX(4,100),itgset(20),rxbox1(4),rybox1(4)
    DIMENSION ICOL(250),ITYPE(250),THICK(250),IDRAW(250),thick0(250)
	DIMENSION XBEG(50),YBEG(50),XEND(50),YEND(50),IEQFRAME(50)
	DIMENSION C_THICK(250)
	
	character*60 :: ddir='.'
	character*60 :: adir='                                '
	character*90 :: dfilt='*.dat'//char(124)//'Data Files (DAT)'//char(124)//&
			'*.plq'//char(124)//'Plot Queue (PLQ)'//char(124)//'*.*'//char(124)//'All Files'
	character*90 :: dfiltq='*.plq'//char(124)//'Plot Queue (PLQ)'//char(124)//&
			'*.dat'//char(124)//'Data Files (DAT)'//char(124)//'*.*'//char(124)//'All Files'
	
	character*90 :: dfiltj='*.cjd'//char(124)//'CJUMP (CJD)'//char(124)//&
			'*.dat'//char(124)//'Data Files (DAT)'//char(124)//'*.*'//char(124)//'All Files'
	
	character*90 :: dfilth='*.scn'//char(124)//'Scan files (SCN)'//char(124)//'*.*'//char(124)//'All Files'
	
	character*60 :: dfile=' '
	character*60 :: ndir='.'
	character*90 :: nfiltsim='*.dat'
	character*60 :: nWfile=' '
	character*90 :: sdir='.'
	character*90 :: sfilt='*.wmf'//char(124)//'Windows Metafile (WMF)'//char(124)//&
						  '*.bmp'//char(124)//'Windows Bitmap Format (BMP)'//char(124)//&
                          '*.*'//char(124)//'All Files'
	character*90 :: sfile=' '
	character*60 :: idir='.'
	character*90 :: ifilt='*.plq'//char(124)//'Plot Queue (PLQ)'//char(124)//&
               '*.cjm'//char(124)//'Jump Files (CJM)'//char(124)//&
               '*.*'//char(124)//'All Files'
	character*60 :: ifile=' '
	character*60 :: edir='.'
	character*120 :: efilt='*.plq'//char(124)//'Plot Queue (PLQ)'//char(124)//&
			   '*.*'//char(124)//'All Files'
	character*60 :: efile
	character*60 :: gfile
	real xb(10),yb(10),xe(10),ye(10)		!up to 10 arrows
	real xlb(10),ylb(10),xle(10),yle(10)	!up to 10 extra lines
	integer iltype(10)				!line type for ditto
	
	real*4 t1v(10),t2v(10),t1c(10),t2c(10)	!for jump logos and t=0 def.
	real*4 yhline(10)		!record y value
	real*4 xhlb(10),xhle(10)     !start/end of HLINE
	integer ilhtype(10)	!line type for horizontal lines
	
	real*4 xvline(10)		!record x value
	real*4 yvlb(10),yvle(10)     !start/end of VLINE
	integer ilvtype(10)	!line type for vertical lines
	real thetsav(100)
	logical newfile,axis
	integer :: iwcell(10)
	integer pixbuf(:)
	allocatable pixbuf
	ALLOCATABLE::xval3,yval3
	real*4 xval3(:),yval3(:)

	real*4 stepamp(50),dt(49),filt(640)
	character ndev*2,adcf1*30,cdatew*11,adctime*8 
	character*44 title2	!for queues from SCAN
	real*4 ycalc(5120)
	logical useconsam,colseq
	logical consamdef,usecons,filtered

	
	integer :: callid,i3col(100),modopen(200),nrmodel(100)
	INTEGER :: current_window, current_child,icfit(100)
    CHARACTER*70 list(50),helps(50),ithg(50),textcell,textcombo,TEXTCELLW
	character*20 pathdat
    DIMENSION INTARR(10)
    DIMENSION icoltemp(250),icolsave(250)
   	CHARACTER*11 CXTRUE,CYTRUE,cset,cnum0
	character*15 label(10)
	character*30 pdir,pfilt
	integer :: iwid(10),istat(200),ifstat(200),numset(200),iopen(200),jopen(200),jopen2(200)
	logical r_mouse,l_mouse,m_mouse,delete_state,move_state,write_par,show,link
	logical saveandplot,open7,cjump,show_curve
	CHARACTER*80 text7(500),togtext(100,200)
	
	real valdat(100)
	character cnum5*5,cnum51*5
	character rfile*40,qfilem*40,titlem*80,cmodel*11,ctitle*60,pfilem*40
	character*60 qmec
	real RLTH(100) 
integer temp(100,100)
PARAMETER  N_STATES=200
	TYPE MODEL
		integer irecm
	    INTEGER N
		integer ka
		integer kb
		integer kstat
		integer kstat0
		integer nsub
		integer kcon
		integer ncon
		integer npar
		logical indmod
		logical chardef
		integer ix
		integer iy
		integer model
		character*80 title_model
		real X(N_STATES)
		real Y(N_STATES)
		INTEGER COLOUR(N_STATES)
		LOGICAL STATUS(N_STATES)
		LOGICAL DRAW(N_STATES)
		CHARACTER*3 NAME(N_STATES)
		CHARACTER*15 statNAME(N_STATES)
		character*10 con_states(20)
		character*10 sub_states(10,20)
		character*10 open_states(10,20)
		character*10 start_states(10,20)
		integer	num_states(10,20)
		integer link(n_states,n_states)
		integer inter_link(n_states)
		character*40 name_link(n_states)
		character*20 ligname(10)
		integer nlig
		integer nbound(n_states,10)
		integer nchan
		real vref
		real*8 dgamma(n_states)
		integer nwidth
		integer nheight
		character*2 charmod(25,40)
		integer ilast
		integer jlast
		integer ic(2,200)
		integer index(n_states)
	END TYPE MODEL

	TYPE (MODEL) models(25)
	
