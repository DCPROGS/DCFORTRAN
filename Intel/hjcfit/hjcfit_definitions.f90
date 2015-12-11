logical resdebug
! 21-4-07 make allocatable covar,covlog,var,varlog,sd,sdlog,sdlog1.badpar
logical axis
character*20 simdwt 
real ameant(100),areat(100)
real*8 det
real*8 ec50d
integer jtemp(100,100)
real*8 eigen(100),eig1,hmopen,hmshut
real*8 WA(20,20),WF(100,100)
real*8 Peq1(100),phio(1,100),time0
real*8 g00A(100),g10A(100),g11A(100),g00F(100),g10F(100),g11F(100)
real*8 g00(100),g10(100),g11(100)
real*8 F0HJC,F1HJC		!functions
logical indmod,clampex
real EFAC(200)
real*4 amp(100),tau(100)

real*4 tau1(100),area(100)
real conc_ec1(10),conc_ec2(10)
real*4 fca(10),fcb(10)
real*4 ytemp(:)
real val(50)
real xb(10),yb(10),xe(10),ye(10)		!up to 10 arrows
real xlb(10),ylb(10),xle(10),yle(10)	!up to 10 extra lines
real*4 t1v(10),t2v(10),t1c(10),t2c(10)	!for jump logos and t=0 def.
real*4 yhline(10)		!record y value
real*4 xhlb(10),xhle(10)     !start/end of HLINE
real*4 xvline(10)		!record x value
real*4 yvlb(10),yvle(10)     !start/end of VLINE
real rxprev(100),ryprev(100)
   
real RX(100),RY(100),RXBOX(4,100),RYBOX(4,100),rxbox1(4),rybox1(4)
    
real XBEG(50),YBEG(50),XEND(50),YEND(50)
real C_THICK(250),ANGLE(100),thick0(250),THICK(250),SIZEtext(100)
integer ICOL(250),ITYPE(250),IDRAW(250)
integer IFNT(100),IJUS(100),itgset(20)
integer Ititbad(100),Idat(100),itog(100),IEQFRAME(50)
integer IE(200),JE(200),IF(200),JF(200)
integer ilhtype(10)	!line type for horizontal lines	
integer jstart(500),itypeval(50)
integer ilvtype(10)	!line type for vertical lines
integer iltype(10)				!line type for ditto
integer indrec1(100)
integer::iwidk(2),irecfin(500)
integer nratek(500)
integer pixbufh(800,40)
integer pixbufv(40,800)
integer*4 iomit(:)
integer NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles	
integer ieq(200),jeq(200),ifq(200),jfq(200)
integer nbound(100,10),ijmod(500)

logical monotd
logical goodx
logical newfile
logical flat,decline

character wtitle*80
character*11 cstring1,cstring2
character*22 cstring0
character*10 label7(2)
character*74 ratetitle(20,500)
character*74 mtitlek	!title for model
character*74 mtits(100)	
character*11 cdate2,ctime2
character*79 title2j
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
character*100 comm


allocatable::ytemp,iomit

integer kfile(20,10),nfile(10),nfileb(10),nintt(10),nint(10),iresw(10)
logical vjump,sampv,control,abortw,resultcom,move_text

character*100 textmr
integer*4 irecfst(1000)

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
	logical screen,colplotter,calbarx,calbary
	ALLOCATABLE:: ndat,icurvd,icurvw,isym,ijoin,symsiz,theta,theta1
	ALLOCATABLE:: cormat
	real*4 cormat(:,:)
	real*4 ymaxj(100)
	REAL SDEV(100),SDEVSAV(100)
	integer ndat(:),icurvd(:),icurvw(:),isym(:),ijoin(:),nsets(20)
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
	ALLOCATABLE::xobs,yobs,w,nj,xval,yval,njbase,xvalold,yvalold
	!allocatable weight
	ALLOCATABLE datcop,z3,bad,zeta1,zeta2
	real*4 z3(:,:),zeta1(:)
	logical bad(:,:),zeta2(:)
	logical fill,inter,axese,fillbad,cross,posneg,all,quarter,after,monsav,landplot
    INTEGER ISTYLE(6),ISUP(6)		
	ALLOCATABLE pdata
	real pdata(:),xvalold(:,:),yvalold(:,:)
	real datcop(:)
	real*4 xobs(:,:),yobs(:,:),w(:,:),xval(:,:),yval(:,:)   !,weight(:,:)
	integer*4 nj(:),njbase(:)
	ALLOCATABLE setx,jmiss,juse
	integer jmiss(:),juse(:),iuse(100),icuse(100),jmiss0(20)
	character*10 TITLEP(200),ptitle(100)		!names of params for fit
	character*10 t1(100),titlep1(100),qij(100),tij(200)
    character*10 titpfree(200) 
	
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
!	dimension iasct(10),ifit(10),IPOINT(5),ikset(20),IMT(10)
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
	integer JFIX(200),jomit(20),jfixsav(100),iparsav(10),THETSAV(:)
	!integer jfix1(200),jcon(200),jmic(200),jbad(200)
	real xbox(4),ybox(4)
	LOGICAL CONSTR,FLINE,SETVAR,defolt1,fitted,dcurve,logt,badset
	logical xgt0,noerr,fixmax,fix0,readini,allocated,specsy
	logical nodata,hillang,autosep,neweq,guesdone,fixset,sameq
	logical noguess(100),apfile,DISCPRT,fixratio
	ALLOCATABLE::xobs1,yobs1,w1,nj1,xnorms,thetsav	!for normalised data
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
	character *10 statname(100)	
	
	integer :: iwcell(10)
	integer pixbuf(:)
	allocatable pixbuf
	ALLOCATABLE::xval3,yval3
	real*4 xval3(:),yval3(:)

	real*4 stepamp(50),dt(49),filt(640)
	character ndev*2,adcf1*30,cdatew*11,adctime*8,adcfil*33 
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
allocatable::covar,covlog,var,varlog
real*8 covar(:,:),covlog(:,:),var(:),varlog(:)
allocatable::badpar
logical badpar(:)
real*8 elmax,cor,den

real*4 sdlog(200),sdlog1(200)
real*4 sd(200)

PARAMETER kAx=20 	!max number of open states fir following

real*8 pop0,p0hjc,pmaxhjc,ec50hjc
real*8 qval,x1s,x2s,fx1,fx2
real*8 qval2,ec5012,x1s2,x2s2,fx12,fx22
integer :: nvalc(10,10),ivals(10),val9(20,10),IQs(100,100)
	logical plotdone							!for HMAT
integer*4 jcov(200)
character*40 namelink(100)
character*74 mtitles

allocatable:: thetas
real*4 thetas(:)
character*60 :: qdir='.'
character*120 :: qfilt='*.mec'//char(124)//'Mechanism Files(MEC)'//&
        char(124)//'*.dat'//char(124)//'Old Files (DAT)'//&
     	char(124)//'*.*'//char(124)//'All Files'
logical corprev,cornext,setmax,open,badi,bad0,bad1,nores,hjmcalc,deb
	ALLOCATABLE:: tval,nopen	!now alloc/dealloc as needed
	integer*2 nopen(:)
	real*4 tval(:)
	real*4 den1(20)
	real*4 sy0(20),syy0(20),sy1(20),syy1(20),sy2(20),syy2(20)
	real*4 sx0(20),sxx0(20),sx1(20),sxx1(20),sx2(20),sxx2(20)
	integer ny0(20),nyy1(20),ny2(20),nx0(20),nxx1(20),nx2(20)
	real*4 FREQ(510),xsav(5,4)
	real*4 xaxis(501),yaxis(501)
	allocatable::fdep,ZNEW,shutt,opent
	real*4 fdep(:,:),ZNEW(:,:),shutt(:),opent(:)
	allocatable::badval
	logical badval(:,:)
	real*4 ylod(20),yhid(20)
	character*256 stringtty
!	real*8 FTCT,tcrit2,ptc,ucol(100,1)
	real*8 FTCT,ttop2,ptc,ucol(100,1)
	logical rescalex
	real*8 ratemax,sm
	logical silent,exass,dsmooth,shbiv,shdep,shdif,shfit
	
	integer jfix1(200),jcon(200),jmic(200),jbad(200)
	integer ICout(2,200),icin(2,200)	!IC for connections to be NOT IN tree
	integer icspec(2,200)	!IC for specified connections
	integer iedge(2,200)	!IC for specified connections
	integer incirc(100)	
	integer Jtree(100,100)
	character mwtitle*60
	allocatable::tint,tint0
	allocatable::iampl0,ampl0,ampl
	allocatable::iprops,iprops0,ipropsset,tintset,amplset
	integer ifixmr1(200)
	real*4 tint(:,:),tint0(:,:),ampl0(:,:),ampl(:,:),tintset(:),amplset(:),ipropsset(:)
	integer*2 iampl0(:,:)
	integer*1 iprops(:,:),iprops0(:,:)
	real*8 thtrue,thsav1
	allocatable::thtrue(:),thsav1(:)

	real*8 tresd(10)
	real*4 tcrit(10),tresol(10),tresolb(10),acrit(10)
	real*8 eigAAsav(kAx,10),eigFFsav(100,10)
	character*33 pfileb(20,10)	!path names for .SCN files
	character*60 pfiles(20,10)
	logical sbin,shist,sres,sexp,samexp,newmr,newcons
	real*4 calfacs2(20,10),alo(10),ahi(10)
	integer nval(20,10),irecs(20,10)
	character*14 ptype(5)
	character cdate1*11,filtfile*20
	character*35 qfile1	!as in scan.dat
	character*40 qfile	!as in vplot and vhist
	character*40 qfile2	!use input -ignore value from disk!
	real*4 avamp(10),rms(10),fc(10),ffilt(10),trise(10)
	character defname*6, name*12
	real*4 conc(10,10)
	logical autosim,simwascii
	real*4 yloj(20),yhij(20),resw(10)		!up to 20 gap ranges
logical replot
logical slopsch,checkgrp,grouped,stable,readmec
	real*8 rcrit,gfac1,gfac2,ampA(100),ampF(100)

	logical excopen
	integer*4 iexcop(10)
	integer isbad(20,10),iebad(20,10)	!for bad bits (see stabplot)
	integer nbad(10),nbad1(10)
	real*4 ampval(10)	!to store amplitudes marked on stability plot
	logical samex,stabcut(10)
	logical liksurf,logsurf
	real*8 alow,ahigh,blow,bhigh,da,db

	logical badcyc(50),samec
	allocatable::kjumps0,kjumps
	integer*4 kjumps0(:),kjumps(:)



	
	
	real*4 conc1(10,10)	!to hold concentration ith ligand, jth set
	
	logical burst(10),chsvec(10),badend(10),onechan
	real*4 tcbad(2,10)	!1=shut, 2=open
	integer ncbad(2,10)
	logical iopenh,jopenh,bind	!to print equilib constants
	character*7 kname 	!to print equilib constants
	logical first,errflag,setbad(2,10)
	real*8 theta0(200),thsav(200),thetaf(200)
		real*8 setlik(10)		!likelihood for each data set (e.g.conc)
			!for dspout in simphjc

	real*8 thetval,elmval,elmset
	allocatable thetval(:,:),elmval(:),elmset(:,:)	!for simulations
	allocatable::nintval,ixval,iyval,izval,nevals
	integer nintval(:,:),ixval(:),iyval(:),izval(:),nevals(:)
	allocatable::ec50val
	real*8 ec50val(:)

	real*8 stpfac,confac,resfac,errfac,delmin,smin	!for simplex
	real*8 hjclik	!function now real*8
	real*4 ylo(20),yhi(20)		!up to 20 gap ranges
	allocatable QDsav,QTsav,qttrue,qd,qt,peq
	real*8 QDsav(:,:),QTsav(:,:),QTtrue(:,:)		!QT has not got conc in
	real*8 QD(:,:),QT(:,:),Peq(:)         !QT has not got conc in
	real*8 dgamma(100)
	real*8 tres
	logical excamp	!for reshjc1
	logical monot	!for ec50
	real*8 rootA(100),rootF(100)
	real*8 s1A(100),s2A(100),s1F(100),s2F(100)	!init guesses for roots
	real*8 s1Asav(kAx,10),s2Asav(kAx,10),s1Fsav(100,10),s2Fsav(100,10)
	real*8 rootAsav(kAx,10),rootFsav(100,10)
	real*8 phiA(1,100),phiF(1,100)

	real*8 ci,cj

	
	real*8 cfacd,vknd
	
	character*11 ctimew
	character*2 charmod(25,40)	!to print model
	
	integer IX(100),JX(100)
	integer IQ(100,100)
	integer IQf(100,100)
	integer IS(100),JS(100)	!declare for disc-write-not used now
	
	logical oneset
	integer index(:,:)
	character*1 ans,UC,ans1,ans2,ans3,ans4,ans5
	
	LOGICAL KMFAST		!for QSETD
	logical nofit
	logical curvonly
	
	logical debprt,dpsav

	character path1*30,pname*8,suffix*3	!for parsname
	character pnameb*8,suffixb*3
	logical nopath
	logical csfound,digchar
	allocatable::index

	character prtfil*40
	character prefix*40,command*100
	character*2 charmods(25,40)	!to print sim model

	real*8 fcomp(10)		!j=1,nset (for hjclik only)
	
	
	allocatable alpha2,beta2,aloglik
	real*8 alpha2(:),beta2(:),aloglik(:)
	logical dcmodel,dcmod,fixec50,prtec50
	real*4 xs
	real*8 ec50,xqlo,xqhi,ec50out,pmax,ec501,ec502,ec50out1,ec50out2
	logical penalty,fixpmax
	real*8 penfunc,penfac,xqlo2,xqhi2,pomax
	allocatable::Z00A,Z10A,Z11A,Z00F,Z10F,Z11F
	real*8 Z00A(:,:,:),Z10A(:,:,:),Z11A(:,:,:)
	real*8 Z00F(:,:,:),Z10F(:,:,:),Z11F(:,:,:)
	allocatable::XAF,XFA,QEXPQA,QEXPQF,expqa,expqf
	real*8 EXPQA(:,:),EXPQF(:,:)
	real*8 XAF(:,:,:),XFA(:,:,:)
	real*8 QEXPQA(:,:),QEXPQF(:,:)
	ALLOCATABLE::jfix2,ie2,je2,efac2,if2,jf2
	ALLOCATABLE::jcon2,jmic2,conca,concb
	integer jfix2(:),ie2(:),je2(:),if2(:),jf2(:),jcon2(:),jmic2(:)
	real*4 efac2(:),conca(:),concb(:)
    
	real*8 assmax
	logical useprim,useprim0
	logical repeat,simulat,sim,endsim,restart
	character*40 simfile,simfile1,simfile3	!file names for output of simulation
	
	
	integer irates(200),jrates(200),irate1(200),jrate1(200),icdep(200),irate2(200),jrate2(200)
	real*8 dgammas(100)
	logical good
	real*8 stpsav
	logical logfit,logsav
	
	real*8 perfac

	real*4 gaplo(10),gaphi(10)
	integer nskip(10)
	logical excop(10)
	integer*4 cubedef(100,8),cubecyc(100,6),cubext(100)
	logical extcyc(50)
	logical useini,set5
	logical automr(50),automr1(50),done(50)
	integer nsc1(50),im1(50,100),jm1(50,100),ir1(50),jr1(50),icmr(50)
	integer nsc3(50),im3(50,100),jm3(50,100),nsc2(50),im2(50,100),jm2(50,100)
	integer isetmr(50),icyc(50)
	integer ict(2,200)	!IC for tree
character tapeID*24,expdate*11
character*20 lign(10)
	
		character*20 ligname(10)
			logical obeymr(50),allmr
integer ic(2,200)
	
	
	TYPE rate_constant 
		integer irec
		integer imod
		character*74 title
		character*10 titlep(200)
		real*8 value(200)
		character*15 qij(200)
		integer iconc(200)
		character*20 ligant(200)
		integer nsetq
		integer ieq(200)
		integer jeq(200)
		integer ifq(200)
		integer jfq(200)
		real efacq(200)
		real*8 qt(100,100)
		character*20 micro(200)
		integer ncyc
		integer nsc(50)
		integer im(50,50)
		integer jm(50,50)
	END TYPE rate_constant
	
	
	
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
		REAL X(N_STATES)
		REAL Y(N_STATES)
		INTEGER COLOUR(N_STATES)
		LOGICAL STATUS(N_STATES)
		LOGICAL DRAW(N_STATES)
		CHARACTER*3 NAME(N_STATES)
		CHARACTER*15 STATNAME(N_STATES)
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

	

	