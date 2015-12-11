subroutine simanw(main,thetval,nintval,ixval,iyval,izval,elmval,elmset,nevals,ec50val,&
		nsims,npar,nset,imod0,ndimd,ndimc,titlep,simfile1,thtrue,id,iomit,nomit,icallsim,&
		newrecords,igraph,combo1_1,COMBO1_2)
! case 21 to 27 main window : list header,etc....
! case 40 - distribution
	USE IFCORE

	USE IFPORT
	use gino_f90
	use menu_f90
	
	use hjcrecords
!	include '\intel\common_files\graphics_definitions.f90'
	TYPE (FILE$INFO) info
	real RLTH(100) 
	real ameant(100),areat(100)
	character*40 :: ascpath='*.*'
    character*40 :: ascdef='*.txt'//char(124)//'ASCII file (txt)'//&
	char(124)//'*.*'//char(124)//'All Files'
	integer*2 jstrec2(200),lstrec2(200)
	logical present
	character*40 qfile,filasc
	character*60 :: edir='.'
	character*120 :: efilt='*.plq'//char(124)//'Plot Queue (PLQ)'//char(124)//&
			   '*.*'//char(124)//'All Files'
	real*8 thtrue(200),thtrsav(200),thsav(200),true,pv1,pv2
	real*8 stpfac,confac,errfac,ec50,tresdum,elmax
    integer juse(ndimd)
    real*4 symsiz(ndimd)	
    integer*4 jmiss(ndimd)
    CHARACTER*200 PARVAL
    character*200  str(100)
    character*11 cnum0
    character*33 adcfil,sfile
    character*75 xtitle,ytitle,ztitle
    character*64 title1m
	real*8 ec50out,penfunc
	logical fixec50,scaled,shist,sbin,exclude
	logical ABORTW,allocated
	character*11 cDATEW,ctimew,cnum1,cnum2
	character*10 titlep(200)
	character*40 simfile,simfile1,mtitles1
	real*8 thetval(npar,nsims),elmval(nsims),elmset(nset,nsims)	!for simulations
	real*8 ec50val(nsims)
	integer nintval(nset,nsims),ixval(nsims),iyval(nsims),izval(nsims),nevals(nsims)
	real*4 ytemp(nsims)
	character*200 string
	integer iomit(nsims),icol(250)
	logical dcmod
	character*74 mtitles1w,xtext2,xtext3,xtext4
	character*40 qfilem
	real*8 p1,p2,pv,sy,syy,fi,one,var,sd,sdm,cv
	real*8 a2,E2,E1a,E1b,ak2
	character mtitle1*40,filnam*32,prtport*4		!for WINPRINT
	character*40 mtitles11 		!for sim model (v 104 only)
	character*74 mtitles
	logical discprt,dprt
	common/dp/discprt
	character*(90) radio_text1(10),itx1(5),radio_text2(10),itx2(5)
	integer iplot_Toggle0(10)
	integer isim_toggle1(10),isim_toggle2(10)
	integer iplot_toggle1(10),iplot_text(10)
	integer iplot_toggle3(10),iplot_toggle4(10)
	common/tty/ittypanel,itty
	logical assoc,eqconst	!function
	character*40 titlex,titley,titlex1,titlex2
	real*4 FREQ(510),XAXIS(510),xsav(5,4)
	allocatable::ypar
	real*4 ypar(:)
	character*64 title1,titsav
	character*11 cnum,cnum5
	allocatable:: thetas
	real*4 thetas(:)	
	character*10 ch1,ch2
	logical landscap,autplt,draft,plotonly,ivplot,doframe,interp,mono
	logical logt,setmax,fitted
	
	integer ncal(ndimc),icurvc(ndimc),iline(ndimc)
	integer ndat(ndimd),icurvd(ndimd),ijoin(ndimd)
	
	ALLOCATABLE:: XVAL,YVAL,XCAL,YCAL
	real*4 XVAL(:,:),YVAL(:,:)
	real*4 XCAL(:,:),YCAL(:,:)
	
	ALLOCATABLE:: XVAL1,YVAL1,XCAL1,YCAL1,weight,icurvw,w
	real*4 XVAL1(:,:),YVAL1(:,:)
	real*4 XCAL1(:,:),YCAL1(:,:)
	real*4 weight(:,:),w(:,:)
	integer*4 icurvw(:),nj(20)
	integer ndat1(5),icurvd1(5),ijoin1(5),isym(5),jopen(1000),iline1(5)
	real*4 syms(5)
	character*20 ligname(10)
	logical hjmcalc,hdisp
	real*4 conc(10,10)
	integer ival(50),itext(50),itypeval(50)
	
	real val(50)
	integer ival21(10,20)
	character*60 text_tog(100),ititle2(20)
	character*60 vtext(50)
	real val_2(10,20)
	CHARACTER*10 CNUMX(25),CNUMY(25),CEXPX(25),CEXPY(25),CNUMZ(25),CEXPZ(25)
    CHARACTER*80 NEWTEXT(20)
	logical logx,logy,sqrty,logity,calbarx,calbary
	DIMENSION IFNT(100),IJUS(100),SIZEtext(100),ANGLE(100)
	DIMENSION RX(100),RY(100),RXBOX(4,100),RYBOX(4,100)
    DIMENSION ITYPE(250),THICK(250),idraw(250)
	DIMENSION XBEG(50),YBEG(50),XEND(50),YEND(50)
	DIMENSION C_THICK(250)
    COMMON/TPOS/IDRAW,ICOL,THICK,C_THICK,ITYPE,IFNT,ANGLE,IJUS,&
      SIZEtext,RXBOX,RYBOX,&
      RX,RY,NARROW,NLINE,NHLINE,NVLINE, XBEG,YBEG,XEND,YEND,&
      NTEXT,NEWTEXT,CNUMX,CNUMY,CNUMZ,CEXPX,CEXPY,CEXPZ,&
      NUMBX,NUMBY,NUMBZ,IHLINREL(10),IVLINREL(10)
	type(GARRAYCELL) arrayattribs
	
	integer :: Graph1_1(100)
    integer :: GraphMainPanel1_1(100)
    integer :: Graphics1_1(100)
    
	character :: cnumb*11, title_graph*60, gfile*60
    common/limit/wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,xmin,xmax,ymin,ymax
    
    logical plot,redrawn,redo,rescale,pcgm,readrec
    real theta(1)
    real xint(2048),yint(2048),Y2int(2048)
    integer ncal1(ndimc),icurvc1(ndimc)
    real*4 ylo(20),yhi(20)
	
	logical pbmp,pwmf
	integer intoggle(100)
logical newfile
integer :: itogButton,callid,ifstat(200),COMBO1_1,COMBO1_2,nintg(10)
integer iradio_toggle(200),iradiox(200),iradioy(200),isym1(20)
real radiox(200),radioy(200),symsiz1(20)
character*(60) radio_text(200),titlerp
character*200 mtext3
logical logscale		!for modify_axis
logical rescalex
integer combo1_6,combo1_4,combo1_5,combo1_8,combo1_9,combo1_10
integer combo1_11,combo1_12
logical r_mouse,l_mouse,m_mouse,delete_state,move_state,write_par,show,link
CHARACTER*70 textcombo
TYPE (RECORD_ATTRIBUTES) newrecords(25)
common/logval/logx,logy,sqrty,logity
logical bold,italik,underline,nolabx,ifontrue
logical d_line,d_hline,d_vline,d_arrow,d_poly,d_text,d_zoom,o_state,c_state,zoom
logical s_text,s_line,s_arrow,sepy,newname
COMMON/JLOGOS/t1c(10),t2c(10),t1v(10),t2v(10),&
    xoff1,y1v,y2v,y1c,y2c,ncjump,nvjump,ivplot

common/attributes_flag/d_line,d_hline,d_vline,d_arrow,d_poly,d_text,d_zoom,o_state,&
		c_state,s_text,s_line,s_arrow,zoom,link,delete_state,move_state                  
common/text_attr/bold,italik,underline,ipow,idev,nolabx,ifontrue
common/pixpos/ixgrid,iygrid,ixpix,iypix,wid,landscape

	call gDefineRGB(220, 1.000000, 1.000000, 0.807843)
    call gDefineRGB(221, 0.796078, 1.000000, 0.592157)
    
    ishp=0
    ipos=0	
	ixposv=0
	iyposv=0		
    
    ncomp=1
	dprt=discprt
	scaled=.false.
	allocate(XVAL(0:511,ndimd),YVAL(0:511,ndimd))
	allocate(XCAL(2048,ndimc),YCAL(2048,ndimc))
	ndimd1=1
	ndimc1=1
	ndv1=nsims
	ndc1=1
	kmax=1		
	ALLOCATE(XVAL1(ndv1,ndimd1),YVAL1(ndv1,ndimd1),XCAL1(ndc1,ndimc1),YCAL1(ndc1,ndimc1))
	kwi=100
	kwj=10
	kwid=100
	kwjd=10
	allocate(weight(100,10),w(100,10))
	do i=1,100
				do j=1,10
				    weight(i,j)=1.
				enddo
				enddo	
	scalefac=1.
	iyp0=50
	ixp0=10
	!iplot=10
	iplot=igraph
	xwbase=0.
	OPEN(unit=10,file=simfile1,status='UNKNOWN',access='DIRECT', form='BINARY',RECL=1)
	read(unit=10,rec=1) iver1,nsims1
	if(iver1.le.101) then
	   read(unit=10,rec=1) iver,nsims1,ABORTW,npar,nset,ix1sav,iy1sav,iz1sav,&
         tresdum,cDATEW,ctimew,mtitle1,simfile,k,kfit,imod0,nlig,&
         fixec50,ec50,stpfac,confac,errfac,nevm,&
         (thsav(i),i=1,npar),(thtrue(i),i=1,npar),(titlep(i),i=1,npar),&
         ((thetval(i,j),i=1,npar),j=1,nsims1)
	   treso=sngl(tresdum)		!correct error
	else if(iver1.eq.102) then
	   read(unit=10,rec=1) iver,nsims1,npar,nset,treso,cDATEW,ctimew,mtitle1,simfile,k,kfit,imod0,nlig,&
         fixec50,ec50,stpfac,confac,errfac,nevm,(thsav(i),i=1,npar),(thtrue(i),i=1,npar),&
		 (titlep(i),i=1,npar),((thetval(i,j),i=1,npar),j=1,nsims1),((conc(i,j),i=1,nlig),j=1,nset),&
         ((nintval(i,j),i=1,nset),j=1,nsims1),(ixval(i),i=1,nsims1),&
         (iyval(i),i=1,nsims1),(izval(i),i=1,nsims1),(elmval(i),i=1,nsims1)
	else if(iver1.eq.103) then
	   read(unit=10,rec=1) iver,nsims1,npar,nset,treso,cDATEW,ctimew,mtitle1,simfile,k,kfit,imod0,nlig,&
         fixec50,ec50,stpfac,confac,errfac,nevm,(thsav(i),i=1,npar),(thtrue(i),i=1,npar),&
		 (titlep(i),i=1,npar),((thetval(i,j),i=1,npar),j=1,nsims1),((conc(i,j),i=1,nlig),j=1,nset),&
         ((nintval(i,j),i=1,nset),j=1,nsims1),(ixval(i),i=1,nsims1),(iyval(i),i=1,nsims1),&
         (izval(i),i=1,nsims1),(elmval(i),i=1,nsims1),((elmset(i,j),i=1,nset),j=1,nsims1),ix1,iy1,iz1
	else if(iver1.eq.104) then
	   read(unit=10,rec=1) iver,nsims1,npar,nset,imods,mtitles1,npars,treso,cDATEW,ctimew,mtitle1,&
	     simfile,k,kfit,imod0,nlig,fixec50,ec50,stpfac,confac,errfac,nevm,&
         (thsav(i),i=1,npar),(thtrue(i),i=1,npars),(titlep(i),i=1,npar),&
         ((thetval(i,j),i=1,npar),j=1,nsims1),((conc(i,j),i=1,nlig),j=1,nset),&
         ((nintval(i,j),i=1,nset),j=1,nsims1),(ixval(i),i=1,nsims1),(iyval(i),i=1,nsims1),&
         (izval(i),i=1,nsims1),(elmval(i),i=1,nsims1),((elmset(i,j),i=1,nset),j=1,nsims1),&
         ix1,iy1,iz1,(nevals(i),i=1,nsims1)
		mtitles=mtitles1
	else if(iver1.eq.105) then
	   read(unit=10,rec=1) iver,nsims1,npar,nset,imods,mtitles1,npars,treso,cDATEW,ctimew,mtitle1,simfile,&
		 k,kfit,imod0,nlig,fixec50,ec50,stpfac,confac,errfac,nevm,(thsav(i),i=1,npar),&
	     (thtrue(i),i=1,npars),(titlep(i),i=1,npar),((thetval(i,j),i=1,npar),j=1,nsims1),&
         ((conc(i,j),i=1,nlig),j=1,nset),((nintval(i,j),i=1,nset),j=1,nsims1),(ixval(i),i=1,nsims1),&
         (iyval(i),i=1,nsims1),(izval(i),i=1,nsims1),(elmval(i),i=1,nsims1),&
         ((elmset(i,j),i=1,nset),j=1,nsims1),ix1,iy1,iz1,(nevals(i),i=1,nsims1)
		mtitles=mtitles1
	else if(iver1.eq.106) then
	    read(unit=10,rec=1) iver,nsims1,npar,nset,imods,mtitles,npars,treso,cDATEW,ctimew,mtitle1,simfile ,&
		 k,kfit,imod0,nlig,fixec50,ec50,stpfac,confac,errfac,nevm,qfilem,(thsav(i),i=1,npar),&
	     (thtrue(i),i=1,npars) ,(titlep(i),i=1,npar) ,((thetval(i,j),i=1,npar),j=1,nsims1) ,&
      	 ((conc(i,j),i=1,nlig),j=1,nset),((nintval(i,j),i=1,nset),j=1,nsims1),(ixval(i),i=1,nsims1),&
     	 (iyval(i),i=1,nsims1),(izval(i),i=1,nsims1),(elmval(i),i=1,nsims1),((elmset(i,j),i=1,nset),j=1,nsims1),&
         ix1,iy1,iz1,(nevals(i),i=1,nsims1),(ec50val(i),i=1,nsims1)

		
	endif
	close(unit=10)
	id=0
	nomit=0
	do i=1,nsims
	   iomit(i)=0
	enddo
	iopt=0
	njset=20
	if(imod0.eq.29) iopt=1
	if(imod0.eq.39) iopt=2
	if(imod0.eq.36) iopt=3

     ! ' Confirm which mechanism was used for FITTING:'
    itx1(1)=' (0) None of those below'
    itx1(2)=' (1) Milone-type ACh scheme (3 open, 4 shut states)'
    itx1(3)=' (2) Ditto plus one desens state  (3 open, 5 shut states)'
    itx1(4)=' (3) Ditto plus extra brief shut state (mod 36)'
      
	if(iopt.eq.0) imod0=0
	if(iopt.eq.1) imod0=29
	if(iopt.eq.2) imod0=39
	if(iopt.eq.3) imod0=36
	if(imod0.eq.29.or.imod0.eq.36.or.imod0.eq.39) then
	   dcmod=.true.
	else
	   dcmod=.false.
	endif
	
	if(iver.ge.104) then
	   if(imod0.eq.29.and.imods.eq.39) then
		n=0
		do i=1,npars		!npars=16 for mod 39
		   thtrsav(i)=thtrue(i)		!keep original for print
		   if(i.ne.3.and.i.ne.4) then
			n=n+1
			thtrue(n)=thtrue(i)
		   endif
		enddo
	   else
		do i=1,npars		!npars=16 for mod 39 and mod 36
		   thtrsav(i)=thtrue(i)		!keep original for print
		enddo
	   endif
	endif

	if(dcmod) then
	   titlep(npar+1)=' E2=b2/a2 '
	   titlep(npar+2)=' E1a      '
	   titlep(npar+3)=' E1b      '
	   titlep(npar+4)='Tot k(-2) '
	   titlep(npar+5)=' K2b (->a)'
	   titlep(npar+6)=' K2a (->b)'
	   titlep(npar+7)=' K1a      '
	   titlep(npar+8)=' K1b      '
	   titlep(npar+9)=' Lmax     '
	   titlep(npar+10)=' N eval   '
	else
	   titlep(npar+1)=' Lmax     '
	   titlep(npar+2)=' N eval   '
	endif


	if(imod0.eq.39.or.imod0.eq.36) then
	   id=2	!all rates apart from 1, 2 have index increased by 2
	else if(imod0.eq.29) then
	   id=0
	endif
	
	call gDefineRGB(41,0.5,0.5,0.75)!lilac
	call gDefineRGB(42,0.65,0.65,0.85)!lilac
	radio_text1(1)=' (1) List header'
	radio_text1(2)=' (2) List true values and initial guesses and param names'
	radio_text1(3)=' (3) List Lmax, alpha2, E2, E1a, E1b, total diss'
	radio_text1(4)=' (4) For each run, list parameter estimates'
	radio_text1(5)=' (5) Mean and SD of parameter estimates in a specified range'
	radio_text1(6)=' (6) List all param estimates with param #j in a spec range'
	radio_text1(7)=' (7) Plot distributions'
	radio_text2(1)=' (1) Distribution of parameter estimates '
	radio_text2(2)=' (2) Plot relation between two parameters'
    radio_text2(3)=' (3) Distribution of sum of two parameters'
    radio_text2(4)=' (4) Distribution of difference between two parameters'
    radio_text2(5)=' (5) Distribution of product of two parameters'
    radio_text2(6)=' (6) Distribution of ratio of two parameters'
	if(iver.le.103.and.imod0.eq.29) then 	!fudge! (imods not kept)
	iplothigh=26
	iplotwid=19 
	else
	iplothigh=18
	iplotwid=19
	endif
	n1=1
	n2=nsims
	ip=1
	ip1=1
	p1=0.0
	p2=1.e10
	p1s=sngl(p1)
	p2s=sngl(p2)

	ndimd=1
	ndimc=1
    ndv1=511	!dimensions as for earlier versions

	ndc1=1	!no calc curves
	isetcol=0
	do i=1,100
	   icol(i)=-1	!default colour
	enddo
	shist=.false.
	sbin=.false.
	ncalc=1
	itit=1	!so title drawn
	ipp=1 		!initial defaults
	ip1=1
	ip2=2
	itipe=1
	allocate(icurvw(ndimd))
	scaled=.false.
	n11=0
	ns=len_trim(simfile1)
	do i=ns,1,-1
	   if(simfile1(i:i).eq.'\') then
		n11=i
		goto 2
	   endif
	enddo
2	n5=ns-n11
    do i=1,40
        title1(i:i)=' '
    enddo
	title1(1:n5)=simfile1(n11+1:ns)
	do i=n5+1,40
	    title1(i:i)=char(0)
	enddo
	titsav=title1
    nl=len_trim(title1)
	allocate(ypar(nsims))
	kth=1			!must allocate for vhist/vplot call, but not used
	allocate(thetas(kth))

	iplotsim = gmCreateComplexDialogueBox(main,22, 4, iplotwid, iplothigh, GALL, &
	'Analyse binary output from simulations in HJCFIT', &
    gmVpos=GTOP, gmIconFormat=GDLLICON, gmIconFile='Gee')

    iplot_MainPanel=gmCreatePanel(iplotsim, 0, 0, iplotwid, iplothigh, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
    gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmFillCol=41, gmScrollMode=Gnobars)
	


	iradio1 = gmCreateRadioBox(iplot_mainPanel, 1, 1, 2, 15, gmType=GFREEFORM, gmBorderType=Gnone, &
      gmFillCol=42,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='', gmHpos=Gleft, gmVpos=GTOP)
	iplotpanel1=gmCreatePanel(iplot_mainpanel, 3, 1, iplotwid-4, 15, gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmType=GNOBOUNDARY, &
		gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmFillCol=42, gmScrollMode=Gnobars)
	
	isim_toggle1(1) = gmCreateToggleButton(iRadio1, 1, 1 , 1, 1,'', 0, &
		gmType=G3DRADIO, gmHpos=Gcentre, gmVpos=GTOP)
	iText = gmCreateTextEntry(iplotPanel1, 0, 1, iplotwid-4, 1,radio_text1(1), 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
		gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP, gmhpos=Gleft)
    
	isim_toggle1(2) = gmCreateToggleButton(iradio1, 1, 2 , 1, 1,'', 0, &
		gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)
	iText = gmCreateTextEntry(iplotPanel1, 0, 2, iplotwid-4, 1,radio_text1(2), 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
	
	isim_toggle1(3) = gmCreateToggleButton(iRadio1, 1, 3 , 1, 1,'', 0, &
		gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)
    iText = gmCreateTextEntry(iplotPanel1, 0, 3, iplotwid-4, 1,radio_text1(3), 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)

	iText = gmCreateTextEntry(iplotPanel1, 1, 4, 2, 1,'n1 = ', 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
		gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
    iText = gmCreateTextEntry(iplotPanel1, 8, 4, 2, 1,'n2 = ' , 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
	ival3_1=gmCreateValueEntry(iplotpanel1, 3, 4, 3, 1,n1, 9, 0, gedit,&
	  gmVpos=GTOP)
	ival3_2=gmCreateValueEntry(iplotpanel1, 10, 4, 3, 1,n2 , 9, 0, gedit,&
	  gmVpos=GTOP)
	isim_toggle1(4) = gmCreateToggleButton(iRadio1, 1, 5 , 1, 1,'', 0, &
		gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)
	iText = gmCreateTextEntry(iplotPanel1, 0, 5, iplotwid-4, 1,radio_text1(4), 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)

	iText = gmCreateTextEntry(iplotPanel1, 1, 6, 2, 1,'n1 = ', 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
		gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
    iText = gmCreateTextEntry(iplotPanel1, 8, 6, 2, 1,'n2 = ' , 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
	ival4_1=gmCreateValueEntry(iplotpanel1, 3, 6, 3, 1,n1 , 9, 0, gedit,&
	  gmVpos=GTOP)
	ival4_2=gmCreateValueEntry(iplotpanel1, 10, 6, 3,1, n2 , 9, 0, gedit,&
	  gmVpos=GTOP)
	isim_toggle1(5) = gmCreateToggleButton(iRadio1, 1, 7 , 1, 1,'', 0, &
		gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)
    iText = gmCreateTextEntry(iplotPanel1, 0, 7, iplotwid-4, 1,radio_text1(5), 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
	iText = gmCreateTextEntry(iplotPanel1, 1, 8, 11, 1,&
		'Parameter number for which range specified', 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
		gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
	ival5_1=gmCreateValueEntry(iplotpanel1, 12, 8, 2, 1,ip , 6, 0, gedit,&
	  gmVpos=GTOP)
	
	iText = gmCreateTextEntry(iplotPanel1, 1, 9, 7, 1,&
		'Range of values to be used', 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
		gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
	ival5_2=gmCreateValueEntry(iplotpanel1, 8, 9, 3, 1,p1s , 13, 5, gedit,&
	  gmVpos=GTOP)
	  call gmsetvaluesetting(ival5_2,p1s)
	ival5_3=gmCreateValueEntry(iplotpanel1, 11, 9, 3, 1,p2s, 13, 5, gedit,&
	  gmVpos=GTOP)
	
	
	isim_toggle1(6) = gmCreateToggleButton(iRadio1, 1, 10 , 1, 1,'', 0, &
		gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)
	iText = gmCreateTextEntry(iplotPanel1, 0, 10, iplotwid-1, 1,radio_text1(6), 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP, gmhpos=Gleft)
	iText = gmCreateTextEntry(iplotPanel1, 1, 11, iplotwid-3, 1,&
		'Parameter number for which range specified', 255, Gdisplay, gmBack1Col=0, &
		gmBack2Col=0, gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP, gmhpos=Gleft)
	ival6_1=gmCreateValueEntry(iplotpanel1, 12, 11, 2, 1,ip, 6, 0, gedit,&
	  gmVpos=GTOP)
	iText = gmCreateTextEntry(iplotPanel1, 1, 12, 7, 1,&
		'Range of values to be used', 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
		gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP, gmhpos=Gleft)
	ival6_2=gmCreateValueEntry(iplotpanel1, 8, 12, 3, 1,p1s , 13, 5, gedit,&
	  gmVpos=GTOP)
	  call gmsetvaluesetting(ival6_2,p1s)
	ival6_3=gmCreateValueEntry(iplotpanel1, 11, 12, 3, 1,p2s , 13, 5, gedit,&
	  gmVpos=GTOP)
	isim_toggle1(7) = gmCreateToggleButton(iRadio1, 1, 13 , 1, 1,'', 0, &
		gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)
	iText = gmCreateTextEntry(iplotPanel1, 0, 13, iplotwid-1, 1,radio_text1(7), 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP, gmhpos=Gleft)
100 continue	
	if(iver.le.103.and.imod0.eq.29) then 
	  iplotPanel=gmCreatePanel(iplotsim, 1, 15, iplotwid-2, 8, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmFillCol=42, gmScrollMode=Gnobars)
	  iradio2 = gmCreateRadioBox(iplotPanel, 1, 1, iplotwid-4, 2, gmType=GFREEFORM, gmBorderType=GPROJECTED, &
      gmFillCol=42,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, &
	  gmTitle='Did mechanism used for simulation have desensitised state', gmVpos=GTOP)
	  iToggle1 = gmCreateToggleButton(iRadio2, 5, 1 , 2, 1, 'Yes', 0, &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmcallback=9002)
	  iToggle2 = gmCreateToggleButton(iRadio2, 9, 1 , 2, 1, 'No', 0, &
		gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP,gmcallback=9003)
	  call DBLETOCH(thtrue(3),cnum1,11)
	  call DBLETOCH(thtrue(3),cnum2,11)
	  iText = gmCreateTextEntry(iplotPanel, 1, 4, 5, 1,'k(+d) = '//cnum1, 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
	  iText = gmCreateTextEntry(iplotPanel, 7, 4, 5, 1,'k(-d) = '//cnum2 , 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
	  iText = gmCreateTextEntry(iplotPanel, 1, 5, 6, 1,' true value of parameter 15, k(-1b) = ', 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
	  iText = gmCreateTextEntry(iplotPanel, 1, 6, 6, 1,' true value of parameter 16, k(+1b) = ', 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
      gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP)
	  
      ival1=gmCreateValueEntry(iplotpanel, 7, 5, 4, 1,10000. , 13, 5, gedit,&
	  gmVpos=GTOP)
	  ival2=gmCreateValueEntry(iplotpanel, 7, 6, 4, 1,4.0e8 , 13, 5, gedit,&
	  gmVpos=GTOP)
	endif
	iradio_Button1 = gmCreatePushButton(iplot_MainPanel,10,0 , 8, 1, 'Display',gmType=GUSERDEFINED, &
	gmhpos=gleft,gmVpos=Gbottom,gmOffcol=38, gmTextCol=1,&
	gmcallback=9004)
	iradio_Button1 = gmCreatePushButton(iplot_MainPanel,1,0 , 8, 1, 'Exit analyses ', gmType=GUSERDEFINED,&
	gmhpos=gleft,gmVpos=Gbottom,gmOffcol=123, gmTextCol=1,&
	gmcallback=-9001)

	call gmdrawwindow(iplotsim)

	if(iver.le.103.and.imod0.eq.29) then 
	if(thtrue(3).ge.4.9.and.thtrue(3).le.5.1) then
			call gmSetToggleSwitch(iToggle1,Gon)
			n=0
			do i=1,16		!npars=16 for mod 39
				thtrsav(i)=thtrue(i)		!keep original for print
				if(i.ne.3.and.i.ne.4) then
				n=n+1
				thtrue(n)=thtrue(i)
				endif
			enddo   


	else
			call gmSetToggleSwitch(iToggle2,Gon)
			call gmsetwidgetstatus(ival1,gunselectable)
			call gmsetwidgetstatus(ival2,gunselectable)
	endif
	endif
	call gmSetToggleSwitch(isim_toggle1(1),Gon)
	icall=0
	nopt=7
!	call gmflushcallbackqueue()
    do j=1,nset
	
		if(iver.ge.102) then		!not available in v101

			do i=1,nsims
				ytemp(i)=float(nintval(j,i))
			enddo
			call VARV1(ytemp,nsims,xbar,varx,xmin,xmax)
			if(varx.gt.0.) sd1=sqrt(varx)
				
		endif
		if(iver.ge.105) then
			if(fixec50) then
				n=0
				do i=1,nsims1

				enddo
			endif
		endif
	enddo
9001	continue
	do while (gmAction(icall) /=-9001)
9009   continue
	select case(icall)
		 case(-1)
	        icall=9005
	        goto 9009
		case(9002)
			call gmsetwidgetstatus(ival1,gselectable)
			call gmsetwidgetstatus(ival2,gselectable)
			goto 9001
		case(9003)	
			call gmsetwidgetstatus(ival1,gunselectable)
			call gmsetwidgetstatus(ival2,gunselectable) 
		    goto 9001
		case(9004) ! enquire main window simulation
		    if(iver.le.103.and.imod0.eq.29) then 
			imes1=gmenqtoggleswitch(iToggle1)
			if(imes1.eq.1) then
				thtrue(13)=gmenqvaluesetting(ival1)
				thtrue(14)=gmenqvaluesetting(ival2)
			endif
			endif
			do i=1,7
				imes=gmenqtoggleswitch(isim_toggle1(i))
				if(imes.eq.1) iopt=i
			enddo
			
			if(iopt.eq.1) then
				ICALL=9021
				goto 9009	
			else if(iopt.eq.2) then
				ICALL=9022
				goto 9009	
			else if(iopt.eq.3) then
			    n1=gmenqvaluesetting(ival3_1)
				n2=gmenqvaluesetting(ival3_2)
				ICALL=9023
				goto 9009	
			else if(iopt.eq.4) then
				n1=gmenqvaluesetting(ival4_1)
				n2=gmenqvaluesetting(ival4_2)
				ICALL=9024
				goto 9009	
			else if(iopt.eq.5) then
				ip=gmenqvaluesetting(ival5_1)
				p1s=gmenqvaluesetting(ival5_2)
				p2s=gmenqvaluesetting(ival5_3)
				ICALL=9025
				goto 9009	
			else if(iopt.eq.6) then
				ip=gmenqvaluesetting(ival6_1)
				p1s=gmenqvaluesetting(ival6_2)
				p2s=gmenqvaluesetting(ival6_3)
				ICALL=9026
				goto 9009	
			else if(iopt.eq.7) then
				ICALL=9027
				goto 9009	
			endif
			goto 9001
		case(9005)
			call gmremovewindow(iplotsim)
		!	icallsim=5
			goto 9010
		case(9021) !'List header'
	
			write(string,fmt='(a34,i8)') 'Number of simulations recorded = ',nsims
			CALL GMSETTEXTSETTING(ITTY,string)
			write(string,fmt='(a10,i4,a15,g13.6,a12,a12)')' Version = ',iver,&
			' resolution = ',treso,cDATEW,ctimew
		    CALL GMSETTEXTSETTING(ITTY,string)
			string='Output file= '//simfile//'machine ='//mtitle1
			CALL GMSETTEXTSETTING(ITTY,string)
			write(string,fmt='(a5,i3,a15,i4,a12,i4,a10,i3)') ' k = ',k,', # of rates = ',npar,&
			', # fitted = ',kfit,' mech = ',imod0
			CALL GMSETTEXTSETTING(ITTY,string)
			write(string,fmt='(a12,i3,a15,l4,a10,g13.6)') ' # ligands = ',nlig,', ec50 fixed?=',&
			fixec50,', ec50 = ',ec50
			CALL GMSETTEXTSETTING(ITTY,string)
			write(string,fmt='(a20,g13.6,a10,g13.6,a10,g13.6,a15,i10)')' For simplex: step = ',stpfac,&
			', confac = ',confac,', errfac = ',errfac,', max eval = ',nevm
			CALL GMSETTEXTSETTING(ITTY,string)
			write(string,fmt='(a18,i8)')'Number of sets = ',nset
			CALL GMSETTEXTSETTING(ITTY,string)
			if(discprt) write(7,12) nsims,iver,treso,cDATEW,ctimew,mtitle1,&
            simfile,k,npar,kfit,imod0,nlig,fixec50,ec50,&
            stpfac,confac,errfac,nevm,nset

			do j=1,nset
	
				if(discprt) write(7,13) j,(1.e6*conc(i,j),i=1,nlig)

				if(iver.ge.102) then		!not available in v101

				do i=1,nsims
				ytemp(i)=float(nintval(j,i))
				enddo
				call VARV1(ytemp,nsims,xbar,varx,xmin,xmax)
				if(varx.gt.0.) sd1=sqrt(varx)
				
				if(discprt) write(7,131) xbar,sd1,xmin,xmax


				endif
				if(iver.ge.105) then
				if(fixec50) then
				n=0
				do i=1,nsims1

				enddo
				endif
				endif
			enddo
            call gmSetToggleSwitch(isim_toggle1(2),Gon)
			goto 9001
			case(9022)!' (2) List true values and initial guesses and param names'
	
				ihc=npar*30+30
				if(npar.gt.20) ihc=760
				if(discprt) write(7,14)
				xtext2='Title '//char(124)//'True'//char(124)//'Guess'		
				call gmSetGuiGridMode(GOFF)
				icell2=gmCreateComplexDialogueBox(Main,24,30,400 ,ihc , GALL,'', &
              	gmIconFormat=GDLLICON,gmIconFile='Gee',gmhpos=gmiddle,gmvpos=gtop)
				
				icellarray2 = gmCreateTextArray(icell2, 0, 0, 380, ihc-30, 3, npar, &
              	gmAxisW=50, gmAxisH=25, gmXtext=xtext2, gmYtext='*digits', &
              	gmXjust=Gright, gmYjust=Gcentre, gmScrollMode=GBOTHBARS, gmExpand=GOFF,&
				gmhpos=left,gmvpos=gtop)
				
			
			    call gmenqcellattribs(icellarray2,1,1,arrayattribs)
				arrayattribs%width=100
				arrayattribs%backcol=42
				do j=1,npar
						if(discprt) write(7,15) i,titlep(i),thtrue(i),thsav(i)
						call gmSetCellAttribs(icellarray2, 1, j, arrayattribs)
						call gmSetCellSetting(icellarray2, 1,j ,gmString=titlep(j))
						call gmSetCellAttribs(icellarray2, 2, j, arrayattribs)
						call gmSetCellSetting(icellarray2, 2,j ,gmvalue=sngl(thtrue(j)))
						call gmSetCellAttribs(icellarray2, 3, j, arrayattribs)
						call gmSetCellSetting(icellarray2, 3,j ,gmvalue=sngl(thsav(j)))
						if(discprt) write(7,15) j,titlep(j),thtrue(j),thsav(j)
						
				enddo
				if(iver.ge.104) then
					write(string,fmt='(a30,i3,a10,a40,a8,i3,a15)') &
					'For simulation, model # =',imods,' title: ',mtitles,&
					' with ',npars,' rates, viz.'
					CALL GMSETTEXTSETTING(ITTY,string)
				
					m1=1
					do m=1,npars,10
					    mn=m1+9
						if(mn.gt.npars) mn=npars
						write(string,fmt='(10(g13.6))') (thtrsav(i),i=m1,mn)
						CALL GMSETTEXTSETTING(ITTY,string)
						m1=m1+10
					enddo
					if(discprt) write(7,121)imods,mtitles,npars,(thtrsav(i),i=1,npars)
				endif	
				ivb=gmCreatePushButton(icell2,30,0, 360, 24,'Exit',&
              	gmType=Guserdefined, gmSize=80,gmoffcol=38,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=9032)
				call gmSetGuiGridMode(GOn)
				call gmdrawwindow(icell2)
				call gmSetToggleSwitch(isim_toggle1(3),Gon)
				goto 9001
			case(9023) !(3) List Lmax, alpha2, E2, E1a, E1b, total diss'
	
				if(.not.dcmod.or.iver.lt.102) then
					imes=gmdisplaymessagebox('',' Option not available for this mechanism',ginformation,gok)
					if(iopt.lt.nopt) iopt=iopt+1
					call gmSetToggleSwitch(isim_toggle1(4),Gon)
					goto 9001	!another option
				endif
				if(discprt) write(7,171)
				xtext3='L max '//char(124)//'alpha2'//char(124)//'E2'//char(124)//&
					   'E1(a) '//char(124)//'E1(b)'//char(124)//'k-2a+k-2b'//char(124)//&
					   'Lmax set 1'		
				call gmSetGuiGridMode(GOFF)
				if(n2-n1.le.5) then
					ihc=200
				else if(n2-n1.ge.6.and.n2-n1.le.20) then
					ihc=(n2-n1)*30+100
				else
					ihc=760
				endif
			
				icell3=gmCreateComplexDialogueBox(Main,25,30,700 ,ihc , GALL,'', &
              	gmIconFormat=GDLLICON,gmIconFile='Gee',gmhpos=gmiddle,gmvpos=gtop)
				
				icellarray3 = gmCreateTextArray(icell3, 0, 0, 670, ihc-30, nset+6, n2-n1+1, &
              	gmAxisW=50, gmAxisH=25, gmXtext=xtext3, gmYtext='*digits', &
              	gmXjust=Gcentre, gmYjust=Gcentre, gmScrollMode=GBOTHBARS, gmExpand=GOFF,&
				gmhpos=left,gmvpos=gtop)
			    call gmenqcellattribs(icellarray3,1,1,arrayattribs)
				arrayattribs%width=90
				arrayattribs%backcol=42
				call gmSetCellAttribs(icellarray3, 0, 0, arrayattribs)
				do n=n1,n2
					a2=thetval(1,n)
					E2=thetval(2,n)/thetval(1,n)
					E1a=thetval(4+id,n)/thetval(3+id,n)
					E1b=thetval(6+id,n)/thetval(5+id,n)
					ak2=thetval(7+id,n) + thetval(9+id,n)
					if(discprt) write(7,17)elmval(n),a2,E2,E1a,E1b,ak2,(elmset(i,n),i=1,nset)
					call gmSetCellSetting(icellarray3, 1,n ,gmvalue=sngl(elmval(n)))
					call gmSetCellSetting(icellarray3, 2,n ,gmvalue=sngl(a2))
					call gmSetCellSetting(icellarray3, 3,n ,gmvalue=sngl(e2))
					call gmSetCellSetting(icellarray3, 4,n ,gmvalue=sngl(e1a))
					call gmSetCellSetting(icellarray3, 5,n ,gmvalue=sngl(e1b))
					call gmSetCellSetting(icellarray3, 6,n ,gmvalue=sngl(ak2))
					call gmSetCellSetting(icellarray3, 7,n ,gmvalue=sngl(elmset(1,n)))
					if(discprt) write(7,17)elmval(n),a2,E2,E1a,E1b,ak2,(elmset(i,n),i=1,nset)
					if(nset.gt.1) then
						do i=2,nset
							call gmSetCellSetting(icellarray3, i+7,n ,gmvalue=sngl(elmset(i,n)))
						enddo
					endif
				enddo
				ivb=gmCreatePushButton(icell3,200,0, 300, 24,'Exit',&
              	gmType=Guserdefined, gmSize=80,gmoffcol=38,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=9033)
				call gmSetGuiGridMode(GOn)
				call gmdrawwindow(icell3)
				call gmSetToggleSwitch(isim_toggle1(4),Gon)
				goto 9001
			case(9024)!' (4) For each run, list parameter estimates'
	
				if(.not.dcmod) then
					imes=gmdisplaymessagebox('',' Option not available for this mechanism',ginformation,gok)
					if(iopt.lt.nopt) iopt=iopt+1
					call gmSetToggleSwitch(isim_toggle1(5),Gon)
					goto 9001	!another option
				endif
				if(n2-n1.le.5) then
					ihc=200
				else if(n2-n1.ge.6.and.n2-n1.le.20) then
					ihc=(n2-n1)*30+100
				else
					ihc=760
				endif

				if(discprt) write(7,161) (titlep(i),i=1,npar)
				if(discprt) write(7,1611)
				xtext4='E2'//char(124)//'E1(a) '//char(124)//'E1(b)'//char(124)//'k-2a+k-2b'//&
						char(124)//'ABORTWed'
				call gmSetGuiGridMode(GOFF)
				icell4=gmCreateComplexDialogueBox(Main,26,30,500 ,ihc , GALL,'', &
              	gmIconFormat=GDLLICON,gmIconFile='Gee',gmhpos=gmiddle,gmvpos=gtop)
				
				icellarray4 = gmCreateTextArray(icell4, 0, 0, 470, ihc-30, 5, n2-n1+1, &
              	gmAxisW=50, gmAxisH=25, gmXtext=xtext4, gmYtext='*digits', &
              	gmXjust=Gcentre, gmYjust=Gcentre, gmScrollMode=GBOTHBARS, gmExpand=GOFF,&
				gmhpos=left,gmvpos=gtop)
			    call gmenqcellattribs(icellarray4,1,1,arrayattribs)
				arrayattribs%width=90
				arrayattribs%backcol=42
				call gmSetCellAttribs(icellarray4, 0, 0, arrayattribs)
				do n=n1,n2
					
					write(string,fmt='(a18,i8)')'Theval: Run = ',n
					CALL GMSETTEXTSETTING(ITTY,string)
					m1=1
					do m=1,npar,10
					    mn=m1+9
						if(mn.gt.npar) mn=npar
						write(string,fmt='(10(g13.6))') (thetval(i,n),i=m1,mn)
						CALL GMSETTEXTSETTING(ITTY,string)
						m1=m1+10
					enddo
					
					ABORTW=ixval(n).lt.0	
					E2=thetval(2,n)/thetval(1,n)
					E1a=thetval(4+id,n)/thetval(3+id,n)
					E1b=thetval(6+id,n)/thetval(5+id,n)
					ak2=thetval(7+id,n) + thetval(9+id,n)
					if(discprt) write(7,184) n
					if(discprt) write(7,18) (thetval(i,n),i=1,npar)
					if(discprt) write(7,181) E2,E1a,E1b,ak2
					if(discprt.and.ABORTW) write(7,19)
					call gmSetCellSetting(icellarray4, 1,n ,gmvalue=sngl(e2))
					call gmSetCellSetting(icellarray4, 2,n ,gmvalue=sngl(e1a))
					call gmSetCellSetting(icellarray4, 3,n ,gmvalue=sngl(e1b))
					call gmSetCellSetting(icellarray4, 4,n ,gmvalue=sngl(ak2))
					if(ABORTW) call gmSetCellSetting(icellarray4, 5,n ,gmString='yes')
				enddo
				ivb=gmCreatePushButton(icell4,100,0, 300, 24,'Exit',&
              	gmType=Guserdefined, gmSize=80,gmoffcol=38,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=9034)
				call gmSetGuiGridMode(GOn)
				call gmdrawwindow(icell4)
				call gmSetToggleSwitch(isim_toggle1(5),Gon)
				goto 9001
			case(9025)!' (5) Mean and SD of parameter estimates in a specified range'
	
			
				do i=1,npar
					if(discprt) write(7,20) i,titlep(i),thtrue(i)
				enddo
	            if(dcmod) then
				if(discprt) write(7,30) npar+1,npar+2,npar+3,npar+4,&
     				npar+5,npar+6,npar+7,npar+8
				endif
				write(string,fmt='(a4,i3,a20)') '(',npar+1,') E2 = beta2/alpha2'
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a24)') '(',npar+2,') E1a = beta1a/alpha1a'
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a24)') '(',npar+3,') E1b = beta1b/alpha1b'
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a35)') '(',npar+4,') Total dissoc rate k(-2)a+k(-2)b'
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a24)') '(',npar+5,') K2b (old) =K2a (new)'
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a24)') '(',npar+6,') K2a (old) =K2b (new)'
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a8)') '(',npar+7,') K1a '
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a8)') '(',npar+8,') K1b '
				CALL GMSETTEXTSETTING(ITTY,string)
				p1=dble(p1s)
				p2=dble(p2s)
				if(discprt) write(7,33) ip,p1,p2
				n=0
				one=1.0d0
	   do j=1,nsims
	    if(imod0.eq.0) then
		if(ip.le.npar) then
		   pv=thetval(ip,j)
		endif
		if(imod0.eq.29.or.imod0.eq.36.or.imod0.eq.39) then
		 if(ip.eq.npar+1) then
		   pv=thetval(2,j)/thetval(1,j)
		 else if(ip.eq.npar+2) then
		   pv=thetval(4+id,j)/thetval(3+id,j)
		 else if(ip.eq.npar+3) then
		   pv=thetval(6+id,j)/thetval(5+id,j)
		 else if(ip.eq.npar+4) then
		   pv=thetval(7+id,j) + thetval(9+id,j)
	       else if(ip.eq.npar+5) then
		   pv=thetval(7+id,j)/thetval(8+id,j)	!K2b (old) =K2a (new)
      	 else if(ip.eq.npar+6) then
		   pv=thetval(8+id,j)/thetval(10+id,j)	!K2a (old) =K2b (new)
	       else if(ip.eq.npar+7) then
		   pv=thetval(11+id,j)/thetval(12+id,j)	!K1a
      	 else if(ip.eq.npar+8) then
		   pv=thetval(13+id,j)/thetval(14+id,j)	!K1b
		 endif
		endif
		if(pv.ge.p1.and.pv.lt.p2) then
		   n=n+1
		   if(n.eq.1) then
			sy=pv
			syy=0.d0
		   else
			fi=dfloat(n)
			sy=sy + pv
			syy=syy + (fi*pv-sy)**2/(fi*(fi-one))
		   endif
		endif
	    endif
	   enddo
	   if(n.ge.1) then
		ybar=sy/dfloat(n)
		var=syy/dfloat(n-1)
		if(var.ge.0.d0) then
		   sd=dsqrt(var)
		   cv=100.d0*sd/ybar
		   sdm=sd/dsqrt(dfloat(n))
		endif
	   endif
				titlep(npar+1)=' E2=b2/a2 '
				if(discprt) write(7,23) ip,titlep(ip),n,nsims,&
     			100.*float(n)/float(nsims),p1,p2,ybar,sd,cv,sdm
				write(string,fmt='(a15,i3,a4,a10)') ' For parameter # ',ip,' = ',titlep(ip)
    			CALL GMSETTEXTSETTING(ITTY,string)
				write(string,&
				fmt='(i4,a20,i4,a4,f6.2,a15,g13.6,a5,g13.6,a8,g13.6,a5,g13.6,a5,g13.6)') n,&
				'values out of ',nsims,'(',&
				100.*float(n)/float(nsims),' %) are between ',p1,' and ',p2,&
				' SD = ',sd,' CV = ',cv,' SDM = ',sdm
				CALL GMSETTEXTSETTING(ITTY,string)
				call gmSetToggleSwitch(isim_toggle1(6),Gon)
				goto 9001
			case(9026)!' (6) List all param estimates with param #j in a spec range'
	
			    ihc=nsims*30+100
				if(nsims.gt.20) ihc=760
				xtext4='E2'//char(124)//'E1(a) '//char(124)//'E1(b)'//char(124)//'k-2a+k-2b'//&
						char(124)//'ABORTWed'
				if(discprt) write(7,20) i,titlep(i),thtrue(i)	
				write(string,fmt='(a4,i3,a20)') '(',npar+1,') E2 = beta2/alpha2'
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a24)') '(',npar+2,') E1a = beta1a/alpha1a'
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a24)') '(',npar+3,') E1b = beta1b/alpha1b'
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a35)') '(',npar+4,') Total dissoc rate k(-2)a+k(-2)b'
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a24)') '(',npar+5,') K2b (old) =K2a (new)'
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a24)') '(',npar+6,') K2a (old) =K2b (new)'
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a8)') '(',npar+7,') K1a '
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a8)') '(',npar+8,') K1b '
				CALL GMSETTEXTSETTING(ITTY,string)
				p1=dble(p1s)
				p2=dble(p2s)
				if(discprt) write(7,33) ip,p1,p2
				if(discprt) write(7,161) (titlep(i),i=1,npar)
				if(dcmod) then
					if(discprt) write(7,30) npar+1,npar+2,npar+3,npar+4,npar+5,npar+6,npar+7,npar+8
			
					if(discprt) write(7,1611)
					call gmSetGuiGridMode(GOFF)
					icell6=gmCreateComplexDialogueBox(Main,26,30,500 ,ihc , GALL,'', &
              		gmIconFormat=GDLLICON,gmIconFile='Gee',gmhpos=gmiddle,gmvpos=gtop)
				
					icellarray6 = gmCreateTextArray(icell6, 0, 0, 470, ihc-30, 5, nsims, &
              		gmAxisW=50, gmAxisH=25, gmXtext=xtext4, gmYtext='*digits', &
              		gmXjust=Gcentre, gmYjust=Gcentre, gmScrollMode=GBOTHBARS, gmExpand=GOFF,&
					gmhpos=left,gmvpos=gtop)
					call gmenqcellattribs(icellarray6,1,1,arrayattribs)
					arrayattribs%width=90
					arrayattribs%backcol=41
					call gmSetCellAttribs(icellarray6, 0, 0, arrayattribs)

				endif
				n=0
				one=1.0d0

				
				do j=1,nsims
					if(ip.le.npar) then
					pv=thetval(ip,j)
					endif
					if(dcmod) then
					if(ip.eq.npar+1) then
					pv=thetval(2,j)/thetval(1,j)
					else if(ip.eq.npar+2) then
					pv=thetval(4+id,j)/thetval(3+id,j)
					else if(ip.eq.npar+3) then
					pv=thetval(6+id,j)/thetval(5+id,j)
					else if(ip.eq.npar+4) then
					pv=thetval(7+id,j) + thetval(9+id,j)
      				else if(ip.eq.npar+5) then
					pv=thetval(7+id,j)/thetval(8+id,j)	!K2b (old) =K2a (new)
      				else if(ip.eq.npar+6) then
					pv=thetval(8+id,j)/thetval(10+id,j)	!K2a (old) =K2b (new)
      				else if(ip.eq.npar+7) then
					pv=thetval(11+id,j)/thetval(12+id,j)	!K1a
      				else if(ip.eq.npar+8) then
					pv=thetval(13+id,j)/thetval(14+id,j)	!K1b
					endif
					endif
					if(pv.ge.p1.and.pv.lt.p2) then
					n=n+1
					ABORTW=ixval(n).lt.0	!code used to indicate ABORTWed run in simwrt
					if(dcmod) then
					E2=thetval(2,j)/thetval(1,j)
					E1a=thetval(4+id,j)/thetval(3+id,j)
					E1b=thetval(6+id,j)/thetval(5+id,j)
					ak2=thetval(7+id,j) + thetval(9+id,j)
					endif
					if(discprt) write(7,182) n,j

					if(discprt) write(7,18) (thetval(i,j),i=1,npar)
					write(string,fmt='(a18,i8)')'Theval: Run = ',n
					CALL GMSETTEXTSETTING(ITTY,string)
					m1=1
					do m=1,npar,10
					    mn=m1+9
						if(mn.gt.npar) mn=npar
						write(string,fmt='(10(g13.6))') (thetval(i,J),i=m1,mn)
						CALL GMSETTEXTSETTING(ITTY,string)
						m1=m1+10
					enddo
					if(dcmod) then
		
						if(discprt) write(7,181) E2,E1a,E1b,ak2
						call gmSetCellSetting(icellarray6, 1,j ,gmvalue=sngl(e2))
						call gmSetCellSetting(icellarray6, 2,j ,gmvalue=sngl(e1a))
						call gmSetCellSetting(icellarray6, 3,j ,gmvalue=sngl(e1b))
						call gmSetCellSetting(icellarray6, 4,j ,gmvalue=sngl(ak2))
						if(ABORTW) call gmSetCellSetting(icellarray6, 5,j ,gmString='yes')
			
					endif

					if(ABORTW) then
			
						if(discprt) write(7,19)

					endif
					endif
				enddo
				if(dcmod) then
				ivb=gmCreatePushButton(icell6,100,0, 300, 24,'Exit',&
              	gmType=Guserdefined, gmSize=80,gmoffcol=38,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=9036)
				call gmSetGuiGridMode(GOn)
				call gmdrawwindow(icell6)
				endif
				call gmSetToggleSwitch(isim_toggle1(7),Gon)
				goto 9001
			case(9027) !' (7) Plot distributions'
				

				if(imod0.eq.29.or.imod0.eq.36.or.imod0.eq.39) then
				do i=npar+1,npar+8
				if(i.eq.npar+1) then
					thtrue(npar+1)=thtrue(2)/thtrue(1)
				else if(i.eq.npar+2) then
					thtrue(npar+2)=thtrue(4+id)/thtrue(3+id)
				else if(i.eq.npar+3) then
					thtrue(npar+3)=thtrue(6+id)/thtrue(5+id)
				else if(i.eq.npar+4) then
					thtrue(npar+4)=thtrue(7+id) + thtrue(9+id)
				else if(i.eq.npar+5) then
					thtrue(npar+5)=1.d6*thtrue(7+id)/thtrue(8+id)	!K2b (old) =K2a (new)
				else if(i.eq.npar+6) then
					thtrue(npar+6)=1.d6*thtrue(9+id)/thtrue(10+id)	!K2a (old) =K2b (new)
				else if(i.eq.npar+7) then
					thtrue(npar+7)=1.d6*thtrue(11+id)/thtrue(12+id)	!K1a
				else if(i.eq.npar+8) then
					thtrue(npar+8)=1.d6*thtrue(13+id)/thtrue(14+id)	!K1b
				endif
				enddo
				endif
				do i=1,npar
	
					if(dprt) write(7,20) i,titlep(i),thtrue(i)

				enddo
				if(imod0.eq.0) then
				write(string,fmt='(a4,i3,a9)') '(',npar+1,') L(max) '
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a35)') '(',npar+2,') Number of function evaluations '
				CALL GMSETTEXTSETTING(ITTY,string)
					if(dprt) write(7,301) npar+1,npar+2

					ipmax=npar+2
					il1=2 	!Lmax in thtrue(npar+2)
				else
					il1=9 	!Lmax in thtrue(npar+9)
	
	 				if(dprt) write(7,303) npar+1,thtrue(npar+1),npar+2,&
     				thtrue(npar+2),npar+3,thtrue(npar+3),npar+4,thtrue(npar+4),&
     				npar+5,thtrue(npar+5),npar+6,thtrue(npar+6),&
     				npar+7,thtrue(npar+7),npar+8,thtrue(npar+8),&
     				npar+9,npar+10
				write(string,fmt='(a4,i3,a27,g13.6)') '(',npar+1,') E2 = beta2/alpha2; true=',thtrue(npar+1)
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a31,g13.6)') '(',npar+2,') E1a = beta1a/alpha1a; true=',thtrue(npar+2)
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a31,g13.6)') '(',npar+3,') E1b = beta1b/alpha1b; true=',thtrue(npar+3)
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a42,g13.6)') '(',npar+4,') Total dissoc rate k(-2)a+k(-2)b; true=',thtrue(npar+4)
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a31,g13.6)') '(',npar+5,') K2b (old) =K2a (new); true=',thtrue(npar+5)
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a31,g13.6)') '(',npar+6,') K2a (old) =K2b (new); true=',thtrue(npar+6)
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a15,g13.6)') '(',npar+7,') K1a ; true=',thtrue(npar+7)
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a15,g13.6)') '(',npar+8,') K1b ; true=',thtrue(npar+8)
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a8)') '(',npar+9,') L(max) '
				CALL GMSETTEXTSETTING(ITTY,string)
				write(string,fmt='(a4,i3,a35)') '(',npar+10,') Number of function evaluations '
				CALL GMSETTEXTSETTING(ITTY,string)
					dprt=.false.
					ipmax=npar+10
				endif
				icall=9028
				goto 9009
			case(9028)
				iplotwid=21
				ih2=3
				iplotopt = gmCreateComplexDialogueBox(main,14, 4, iplotwid,27-ih2 , GALL, &
				'Plot distributions', &
                gmVpos=GTOP, gmIconFormat=GDLLICON, gmIconFile='Gee')

                iplot_MainPanel=gmCreatePanel(iplotopt, 0, 0, iplotwid, 27-ih2, gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
                gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmFillCol=42, gmScrollMode=Gnobars)
	
	            iradio2 = gmCreateRadioBox(iplot_mainPanel, 1, 1, 2, 17-ih2, gmType=GFREEFORM, gmBorderType=Gnone, &
				gmFillCol=41,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='', gmVpos=GTOP)
				
				iplotpanel2=gmCreatePanel(iplot_mainpanel, 3, 1, iplotwid-4, 17-ih2, gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmType=GNOBOUNDARY, &
				gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmFillCol=41, gmScrollMode=Gnobars)
	
				isim_toggle2(1) = gmCreateToggleButton(iRadio2, 1, 1 , 1, 1,'', 0, &
				gmType=G3DRADIO, gmHpos=Gcentre, gmVpos=GTOP)
				iText = gmCreateTextEntry(iplotPanel2, 0, 1, iplotwid-4, 1,radio_text2(1), 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
				gmTextCol=1 ,gmScrollMode=GVERTICAL, gmVpos=GTOP, gmhpos=Gleft)
				iText = gmCreateTextEntry(iplotPanel2, 2, 2, 10, 1,'Parameter number to be plotted= ' , 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
				gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
				jval1_1=gmCreateValueEntry(iplotpanel2, 12, 2, 2, 1,ipp, 9, 0, gedit,&
				gmVpos=GTOP)
				if(nset.gt.1) then
				iText = gmCreateTextEntry(iplotPanel2, 2, 3, 10, 1,'Plot L(max) for set # (0 for total)= ' , 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
				gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
				jval1_2=gmCreateValueEntry(iplotpanel2, 12, 3, 2, 1,ips1, 9, 0, gedit,&
				gmVpos=GTOP)
				ih1=0
				endif
				ih1=1
				isim_toggle2(2) = gmCreateToggleButton(iradio2, 1, 4-ih1 , 1, 1,'', 0, &
				gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)
				iText = gmCreateTextEntry(iplotPanel2, 0, 4-ih1, iplotwid-4, 1,radio_text2(2), 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
				gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
				iText = gmCreateTextEntry(iplotPanel2, 2, 5-ih1, 10, 1,'Parameter numbers to be plotted (x,y)= ' , 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
				gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
				jval2_1=gmCreateValueEntry(iplotpanel2, 12, 5-ih1, 2, 1,ip1, 9, 0, gedit,&
				gmVpos=GTOP)
				jval2_11=gmCreateValueEntry(iplotpanel2, 14, 5-ih1, 2, 1,ip2, 9, 0, gedit,&
				gmVpos=GTOP)
				if(nset.gt.1) then
				iText = gmCreateTextEntry(iplotPanel2, 2, 6, 10, 1,'Plot L(max) as X for set # (0 for total)= ' , 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
				gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
				jval2_2=gmCreateValueEntry(iplotpanel2, 12, 6, 2, 1,ips1, 9, 0, gedit,&
				gmVpos=GTOP)
				iText = gmCreateTextEntry(iplotPanel2, 2, 7, 10, 1,'Plot L(max) as Y for set # (0 for total)= ' , 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
				gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
				jval2_21=gmCreateValueEntry(iplotpanel2, 12, 7, 2, 1,ips2, 9, 0, gedit,&
				gmVpos=GTOP)
				ih2=0
				endif
				ih2=3
				isim_toggle2(3) = gmCreateToggleButton(iRadio2, 1, 8-ih2 , 1, 1,'', 0, &
				gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)
				iText = gmCreateTextEntry(iplotPanel2, 0, 8-ih2, iplotwid-4, 1,radio_text2(3), 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
				gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
				iText = gmCreateTextEntry(iplotPanel2, 2, 9-ih2, 10, 1,'Parameter numbers to be plotted (x+y)= ' , 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
				gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
				jval1=gmCreateValueEntry(iplotpanel2, 12, 9-ih2, 2, 1,ip1, 9, 0, gedit,&
				gmVpos=GTOP)
				jval2=gmCreateValueEntry(iplotpanel2, 14, 9-ih2, 2, 1,ip2, 9, 0, gedit,&
				gmVpos=GTOP)

				isim_toggle2(4) = gmCreateToggleButton(iRadio2, 1, 10-ih2 , 1, 1,'', 0, &
				gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)
				iText = gmCreateTextEntry(iplotPanel2, 0, 10-ih2, iplotwid-4, 1,radio_text2(4), 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
				gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
				iText = gmCreateTextEntry(iplotPanel2, 2, 11-ih2, 10, 1,'Parameter numbers to be plotted (x-y)= ' , 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
				gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
				jval1=gmCreateValueEntry(iplotpanel2, 12, 11-ih2, 2, 1,ip1, 9, 0, gedit,&
				gmVpos=GTOP)
				jval2=gmCreateValueEntry(iplotpanel2, 14, 11-ih2, 2, 1,ip2, 9, 0, gedit,&
				gmVpos=GTOP)
				
				
				isim_toggle2(5) = gmCreateToggleButton(iRadio2, 1, 12-ih2 , 1, 1,'', 0, &
				gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)
				iText = gmCreateTextEntry(iplotPanel2, 0, 12-ih2, iplotwid-4, 1,radio_text2(5), 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
				gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
				iText = gmCreateTextEntry(iplotPanel2, 2, 13-ih2, 10, 1,'Parameter numbers to be plotted (x*y)= ' , 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
				gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
				jval1=gmCreateValueEntry(iplotpanel2, 12, 13-ih2, 2, 1,ip1, 9, 0, gedit,&
				gmVpos=GTOP)
				jval2=gmCreateValueEntry(iplotpanel2, 14, 13-ih2, 2, 1,ip2, 9, 0, gedit,&
				gmVpos=GTOP)

				isim_toggle2(6) = gmCreateToggleButton(iRadio2, 1, 14-ih2 , 1, 1,'', 0, &
				gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP)
				iText = gmCreateTextEntry(iplotPanel2, 0, 14-ih2, iplotwid-4, 1,radio_text2(6), 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
				gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
				iText = gmCreateTextEntry(iplotPanel2, 2, 15-ih2, 10, 1,'Parameter numbers to be plotted (x/y)= ' , 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
				gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
				jval1=gmCreateValueEntry(iplotpanel2, 12, 15-ih2, 2, 1,ip1, 9, 0, gedit,&
				gmVpos=GTOP)
				jval2=gmCreateValueEntry(iplotpanel2, 14, 15-ih2, 2, 1,ip2, 9, 0, gedit,&
				gmVpos=GTOP)
				
				
				do i=1,6 
				!	isim_toggle2(i) = gmCreateToggleButton(iRadio2, 1, i , iplotwid-4, 1, radio_text2(i), 0, &
				!	gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP) !,gmcallback=20+i)
				enddo
				iplotpanel3=gmCreatePanel(iplot_mainpanel, 1, 19-ih2, iplotwid-2, 6, gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmType=GNOBOUNDARY, &
				gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmFillCol=41, gmScrollMode=Gnobars)
	
				iradio3 = gmCreateRadioBox(iplotPanel3, 1, 1, iplotwid-4, 2, gmType=GFREEFORM, gmBorderType=Gnone, &
				gmFillCol=41,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, &
				gmTitle='Include only values for which specified rate is in given range?', gmVpos=GTOP)
			
				iToggle3_1 = gmCreateToggleButton(iRadio3, 4, 1 , 3, 1,'Yes', 1, &
					gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP) !,gmcallback=20+i)
				iToggle3_2 = gmCreateToggleButton(iRadio3, 8, 1 , 3, 1,'No', 0, &
					gmType=G3DRADIO, gmHpos=Gleft, gmVpos=GTOP)
			
				ipex=1
			
				iText = gmCreateTextEntry(iplotPanel3, 2, 3, 11, 1,&
				'Parameter number for which range specified', 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
				gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
			
				kval1=gmCreateValueEntry(iplotpanel3, 13, 3, 2, 1,ipex , 6, 0, gedit,&
				gmVpos=GTOP)
	
				iText = gmCreateTextEntry(iplotPanel3, 2, 4, 7, 1,&
				'Range of values to be used', 255, Gdisplay, gmBack1Col=0, gmBack2Col=0, &
				gmTextCol=1 ,gmScrollMode=GVERTICAL, gmhpos=Gleft, gmVpos=GTOP)
				kval2=gmCreateValueEntry(iplotpanel3, 9, 4, 3, 1,p1s , 13, 5, gedit,&
				gmVpos=GTOP)
				call gmsetvaluesetting(kval2,p1s)
				kval3=gmCreateValueEntry(iplotpanel3, 12, 4, 3, 1,p2s, 13, 5, gedit,&
				gmVpos=GTOP)

				iradio_Button1 = gmCreatePushButton(iplot_MainPanel,10,0 , 8, 1, 'Display',gmType=GUSERDEFINED, &
				gmhpos=gleft,gmVpos=Gbottom,gmOffcol=38, gmTextCol=1,&
				gmcallback=9040)
				iradio_Button1 = gmCreatePushButton(iplot_MainPanel,1,0 , 8, 1, 'Exit', gmType=GUSERDEFINED,&
				gmhpos=gleft,gmVpos=Gbottom,gmOffcol=0, gmTextCol=1,&
				gmcallback=9037)
				call gmdrawwindow(iplotopt)
				indxt=indxt+1
				if(indxt.le.6) call gmsettoggleswitch(isim_toggle2(indxt),gon)
				goto 9001
			case(9032)
				call gmremovewindow(icell2)
				goto 9001
			case(9033)
				call gmremovewindow(icell3)
				goto 9001
			case(9034)
				call gmremovewindow(icell4)
				goto 9001
			case(9036)
				call gmremovewindow(icell6)
				goto 9001
			case(9037)
				call gmremovewindow(iplotopt)
				goto 9001
			case(9040) ! ok second window plot distributions

				do i=1,6
					imes2=gmenqtoggleswitch(isim_toggle2(i))
					if(imes2.eq.1) itipe=i
				enddo
		 		call gmremovewindow(iplotopt)
				if(itipe.eq.1) then
					ipp=gmenqvaluesetting(jval1_1)
					if(nset.gt.1) then
					ips=gmenqvaluesetting(jval1_2)
					endif
				else if (itipe.eq.2) then
					ip1=gmenqvaluesetting(jval2_1)
					ip2=gmenqvaluesetting(jval2_11)
					if(nset.gt.1) then
					ips1=gmenqvaluesetting(jval2_2)
					ips2=gmenqvaluesetting(jval2_21)
					endif
				else
					ip1=gmenqvaluesetting(jval1)
					ip2=gmenqvaluesetting(jval2)
				endif
				imes2=gmenqtoggleswitch(iToggle3_1)
				if(imes2.eq.1) then
					ipex=gmenqvaluesetting(kval1)
					p1s=gmenqvaluesetting(kval2)
					p2s=gmenqvaluesetting(kval3)
					exclude=.true.
					p1=dble(p1s)
					p2=dble(p2s)
					p1s1=dROUND(p1,0)	!zero figs after dec point
					p2s1=dROUND(p2,0)	!zero figs after dec point
					call REALTOCH(p1s1,ch1,10)
					call REALTOCH(p2s1,ch2,10)
					n=len_trim(title1)	!must exclude ascii 0
					n11=len_trim(titlep(ipex))
					title1=title1(1:n)//' '//ch1//' < '//&
     				titlep(ipex)(1:n11)//' < '//ch2
				else
					exclude=.false.
				endif
				do i=1,40
					titlex(i:i)=' '
					titlex1(i:i)=' '
					titlex2(i:i)=' '
					titley(i:i)=' '
				enddo
				if(itipe.eq.1)  then !(1) Distribution of parameter estimates '
	
					true=thtrue(ipp)
					if(scaled) true=true/scalefac
	 				if(discprt) write(7,105) ipp,titlep(ipp),true
105					format(/,' Plot parameter ',i3,', ',a10,' = ',g13.6)
					write(string,fmt='(a15,i3,a10,a3,g13.6)')' Plot parameter ' ,ipp,titlep(ipp),'=',true
					CALL GMSETTEXTSETTING(ITTY,string)
			
					if(exclude) then
					if(discprt) write(7,115) ipex,titlep(ipex),p1,p2
115					format('  Plot only values for which parameter ',i3,', '&
     				,a10,' is between ',g13.6,' and ',g13.6)
					endif
					call PAR(ypar,ipp,npar,nsims,thetval,nsim1,exclude,imod0,&
					ipex,p1,p2,elmval,elmset,ixval,nABORTW,nevals,nset,ips1,id,&
					iomit,nomit)		!get values in ypar(i)
					if(nABORTW.gt.0) then
					if(discprt) write(7,34) nABORTW
34					format(' Exclude ',i3,' fits that were ABORTWed')
					write(string,fmt='(a10,i3,a30)') ' Exclude ',nABORTW,' fits that were ABORTWed'
				    CALL GMSETTEXTSETTING(ITTY,string)
					
					endif
					nyval=nsim1

				if(.not.sbin) NBIN=-1		!TO SIGNAL THAT NOT YET SET
				if(nyval.eq.0) then
				    imes=gmdisplaymessagebox('',' NO VALUES FOR DISTRIBUTION',ginformation,gok)
	 
				    goto 9001
				else
				    icall=9200 !hist
				    goto 9009
				endif
					
				else if(itipe.ge.3.and.itipe.le.6) then
				
				    !(3) Distribution of sum of two parameters'
				
                    !(4) Distribution of difference between two parameters'
                    !(5) Distribution of product of two parameters'
                    !(6) Distribution of ratio of two parameters'
					titlex1(1:10)=titlep(ip1)
					if(ASSOC(ip1,id,imod0)) then
						n=len_trim(titlep(ip1))	!must exclude ascii 0
						titlex1(1:18)=titlep(ip1)(1:n)//' x 10^-8'
					endif
					if(EQCONST(ip1,npar,imod0)) then
						n=len_trim(titlep(ip1))	!must exclude ascii 0
						titlex1(1:18)=titlep(ip1)(1:n)//' (muM)  '
					endif
					titlex2(1:10)=titlep(ip2)
					if(ASSOC(ip2,id,imod0)) then
					n=len_trim(titlep(ip2))	!must exclude ascii 0
					titlex2(1:18)=titlep(ip2)(1:n)//' x 10^-8'
					endif
					if(EQCONST(ip2,npar,imod0)) then
					n=len_trim(titlep(ip2))	!must exclude ascii 0
					titlex2(1:18)=titlep(ip2)(1:n)//' (muM)  '
					endif
					pv1=thtrue(ip1)
	   if(ASSOC(ip1,id,imod0)) pv1=pv1*1.d-8
	   if(EQCONST(ip1,npar,imod0)) pv1=pv1*1.d6
	   pv2=thtrue(ip2)
	   if(ASSOC(ip2,id,imod0)) pv2=pv2*1.d-8
	   if(EQCONST(ip2,npar,imod0)) pv2=pv2*1.d6
	   n11=len_trim(titlex1)
	   n12=len_trim(titlex2)
	   n=n11+n12+3
	   if(itipe.eq.3) then
		true=pv1+pv2
		titlex(1:n)=titlex1(1:n11)//' + '//titlex2(1:n12)
	   else if(itipe.eq.4) then
		true=pv1-pv2
		titlex(1:n)=titlex1(1:n11)//' - '//titlex2(1:n12)
	   else if(itipe.eq.5) then
		true=pv1*pv2
		titlex(1:n)=titlex1(1:n11)//' x '//titlex2(1:n12)
	   else if(itipe.eq.6) then
		true=pv1/pv2
		titlex(1:n)=titlex1(1:n11)//' / '//titlex2(1:n12)
	   endif
	   do i=n+1,40
		titlex(i:i)=' '
	   enddo
	  
	   if(discprt) write(7,1011) ip1,ip2,titlex(1:n),true
1011   format(/,' Plot parameters ',i3,', ',i3,': ',a40,' = ',g13.6)
	   write(string,fmt='(a15,i3,a3,i3,a10,a3,g13.6)')' Plot parameters ' ,ip1,',',ip2,titlex(1:n),'=',true
	   CALL GMSETTEXTSETTING(ITTY,string)
	   if(exclude) then
			if(discprt) write(7,11) ipex,titlep(ipex),p1,p2
			
	   endif
11		format('  Plot only values for which parameter ',i3,', '&
		,a10,' is between ',g13.6,' and ',g13.6)
	   if(nomit.gt.0) then
	   endif

	   call PAR2(ypar,ip1,ip2,npar,nsims,thetval,nsim1,exclude,imod0,&
	   	ipex,p1,p2,ixval,elmval,elmset,nevals,nABORTW,nset,ips,itipe,id,&
         iomit,nomit)
	   if(nABORTW.gt.0) then
		write(string,fmt='(a10,i3,a30)') ' Exclude ',nABORTW,' fits that were ABORTWed'
		CALL GMSETTEXTSETTING(ITTY,string)
		if(discprt) write(7,34) nABORTW

	   endif
				nyval=nsim1

				if(.not.sbin) NBIN=-1		!TO SIGNAL THAT NOT YET SET
				if(nyval.eq.0) then
				    imes=gmdisplaymessagebox('',' NO VALUES FOR DISTRIBUTION',ginformation,gok)
	 
				    goto 9001
				else
				    icall=9200 !hist
				    goto 9009
				endif
				else if(itipe.eq.2) then    !!(2) Plot relation between two parameters'
					icall=9100
					goto 9009
				endif	
							
			goto 9001
	
		case(9100) !plots !(2) Plot relation between two parameters'
		    hdisp=.false.
		!	ICALL=28
		!	goto 9
			
			do i=1,ndimd1
				icurvw(i)=-1	!no SD unless reset below
			enddo
		!	kwi=1							!ditto
		!	kwj=1							!ditto

			!Parameter # ip1 = x variable
			call PAR(ypar,ip1,npar,nsims,thetval,nsim1,exclude,imod0,&
			ipex,p1,p2,elmval,elmset,ixval,nABORTW,nevals,nset,ips1,id,iomit,nomit)
			if(nABORTW.gt.0) then
	   
			if(discprt) write(7,201) nABORTW
			endif
			do i=1,nsim1
				xval1(i,1)=ypar(i)
			enddo
			if(ip1.le.npar) then
			titlex(1:10)=titlep(ip1)
			if(ASSOC(ip1,id,imod0)) then
			nb=len_trim(titlep(ip1))	!must exclude ascii 0
			titlex(1:18)=titlep(ip1)(1:nb)//' x 10^-8'
			endif
			if(EQCONST(ip1,npar,imod0)) then
			nb=len_trim(titlep(ip1))	!must exclude ascii 0
			titlex(1:18)=titlep(ip1)(1:nb)//' (muM)  '
			endif
			endif
			if(imod0.eq.0) then
			if(ip1.eq.npar+1) then
			if(ips1.eq.0) then
			titlex=' L(max)      '
			else
			call INTCONV(ips1,cnum)
			titlex=' L(max) for set '//cnum
			endif
			else if(ip1.eq.npar+2) then
			titlex=' number of evaluations       '
			 endif
			else if(imod0.eq.29.or.imod0.eq.36.or.imod0.eq.39) then
			if(ip1.eq.npar+1) then
			titlex=' E2 = beta2/alpha2                     '
			else if(ip1.eq.npar+2) then
			titlex=' E1a = beta1a/alpha1a                  '
			else if(ip1.eq.npar+3) then
			titlex=' E1b = beta1b/alpha1b                  '
			else if(ip1.eq.npar+4) then
			titlex=' Total dissoc rate k(-2)a+k(-2)b       '
			else if(ip1.eq.npar+5) then
			titlex=' K2b (old) = K2a (new)                 '
			else if(ip1.eq.npar+6) then
			titlex=' K2a (old) = K2b(new)                  '
			else if(ip1.eq.npar+7) then
			titlex=' K1a                                   '
			else if(ip1.eq.npar+8) then
			titlex=' K1b                                   '
			else if(ip1.eq.npar+9) then
			if(ips1.eq.0) then
			titlex=' L(max)      '
			else
			call INTCONV(ips1,cnum)
			titlex=' L(max) for set '//cnum
			endif
			else if(ip1.eq.npar+10) then
			titlex=' number of evaluations       '
			endif
			endif
			! Parameter # ip2 = y variable
			call PAR(ypar,ip2,npar,nsims,thetval,nsim1,exclude,imod0,&
			ipex,p1,p2,elmval,elmset,ixval,nABORTW,nevals,nset,ips2,id,&
			iomit,nomit)
			if(nABORTW.gt.0) then
		
			if(discprt) write(7,201) nABORTW
			endif
			do i=1,nsim1
			yval1(i,1)=ypar(i)
			enddo
			if(ip2.le.npar) then
	   titley(1:10)=titlep(ip2)
	   if(ASSOC(ip2,id,imod0)) then
	      nb=len_trim(titlep(ip2))	!must exclude ascii 0
	      titley(1:18)=titlep(ip2)(1:nb)//' x 10^-8'
	   endif
	   if(EQCONST(ip2,npar,imod0)) then
	      nb=len_trim(titlep(ip2))	!must exclude ascii 0
	      titley(1:18)=titlep(ip2)(1:nb)//' (muM)  '
	   endif
	endif
	if(imod0.eq.0) then
	 if(ip2.eq.npar+1) then
	   if(ips2.eq.0) then
		titley=' L(max)                           '
	   else
		call INTCONV(ips2,cnum)
		titley=' L(max) for set '//cnum
	   endif
	 else if(ip2.eq.npar+2) then
	   titley=' number of evaluations                '
	 endif
	else if(imod0.eq.29.or.imod0.eq.36.or.imod0.eq.39) then
	 if(ip2.eq.npar+1) then
	   titley=' E2 = beta2/alpha2                     '
	 else if(ip2.eq.npar+2) then
	   titley=' E1a = beta1a/alpha1a                  '
	 else if(ip2.eq.npar+3) then
	   titley=' E1b = beta1b/alpha1b                  '
	 else if(ip2.eq.npar+4) then
	   titley=' Total dissoc rate k(-2)a+k(-2)b       '
	 else if(ip2.eq.npar+5) then
	   titley=' K2b (old) = K2a (new)                 '
	 else if(ip2.eq.npar+6) then
	   titley=' K2a (old) = K2b(new)                  '
	 else if(ip2.eq.npar+7) then
	   titley=' K1a                                   '
	 else if(ip2.eq.npar+8) then
	   titley=' K1b                                   '
	 else if(ip2.eq.npar+9) then
	   if(ips2.eq.0) then
		titley=' L(max)                              '
	   else
		call INTCONV(ips2,cnum)
		titley=' L(max) for set '//cnum
	   endif
	 else if(ip2.eq.npar+10) then
	   titley=' number of evaluations                  '
	 endif
	endif
	
	if(discprt) write(7,41) ip1,titlep(ip1),ip2,titlep(ip2)
	if(exclude) then
	   
	   if(discprt) write(7,11) ipex,titlep(ipex),p1,p2
	endif
41	format(/,' Plot parameter ',i3,' (x), ',a10,' against parameter ',i3,' (y), ',a10)
! Calculate correlation coefficient  Sxy/sqrt(var

	scalfacx=1.0
	scalfacy=1.0
	scaled=.false.
1500	continue
		call VARV1(xval1(1,1),Nsim1,xbar,varx,xmin,xmax)
		call VARV1(yval1(1,1),Nsim1,ybar,vary,ymin,ymax)
		if(xbar.gt.1.e5.or.ybar.gt.1.e5) then
				n=ifixr(alog10(xbar))-1
				itypeval(1)=0
				val(1)=n1
				vtext(1)='Scale down the X values'
				val(2)=n2
				vtext(2)='Scale down the y values'
				itypeval(2)=0
				call val_table(Main,ivwin9,2,vtext,ival,val,9111,itypeval,iplot_Toggle0)
		
		else
				icall=9112
				goto 9009
		endif

case(9111)
	    val(1)=gmenqvaluesetting(ival(1))
	    if(val(1).le.1) val(1)=1
		n1=val(1)
		val(2)=gmenqvaluesetting(ival(2))
	    if(val(2).le.1) val(2)=1
		n2=val(2)
	    scalfacx=10**n1
		scaled=.true.
		do i=1,nsim1
		   xval1(i,1)=xval1(i,1)/scalfacx
		enddo
		scalfacy=10**n2
	
		do i=1,nsim1
		   yval1(i,1)=yval1(i,1)/scalfacy
		enddo
		
		call gmremovewindow(ivwin9)
		call VARV1(xval1(1,1),Nsim1,xbar,varx,xmin,xmax)
		call VARV1(yval1(1,1),Nsim1,ybar,vary,ymin,ymax)
		icall=9112
		goto 9009
case(9112)

	Sx=0.0
	Sy=0.0
	Sxy=0.0
	do i=1,nsim1
	  Sx=Sx+xval1(i,1)
	  Sy=Sy+yval1(i,1)
	  Sxy=Sxy+xval1(i,1)*yval1(i,1)
	enddo
	Sxy=Sxy - (Sx*Sy)/float(nsim1)
	Sxy=Sxy/float(nsim1-1)
	r=Sxy/sqrt(varx*vary)
	
	if(discprt) write(7,24) xbar,ybar,r
24	format(' mean X value = ',g13.6,', mean Y value = ',g13.6,/,' Correlation coefficient = ',g13.6)
		icall=9150
		goto 9009
case(9150)       !plots
		ncurvd=1
		icurvd1(1)=1
		ijoin1(1)=-1	!no line
		isym(1)=-7
		syms(1)=1.
		ndat1(1)=nsim1

		ilabel=1
		ncurvc=0
		ilog=0		!y vs x
		iscal=1		!scale internally
		itrace=0		!not multiple traces

		autplt=.false.
		draft=.false.
		plotonly=.false.
		doframe=.true.
		landscap=.true.
		fitted=.false.
		ifitype=0
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
		ndv1=nsims
		ndc1=1
		ifontrue=101	
	!kwi=100
	!kwj=10
		call calc_default(XVAL1,YVAL1,NDAT1,icurvd,ncurvd,ijoin1,symsiz,&
		ndimd1,XCAL1,YCAL1,NCAL1,icurvc1,ncurvc,iline1,ndimc1,ISYM,ILOG,ISCAL,&
		XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,&
	    XLO1,XHI1,YLO1,YHI1,y0,yinf,inumx,inumy,&
		titlex,titley,ilabel,doframe,autplt,itit,title1,&
		ISHP,ifont,landscap,ndv1,ndc1,weight,kwi,kwj,icurvw,kmax,iver,&
		redrawn,plot,redo,pcgm,pwmf,npint,hdisp,n1,tlenx,tleny,nx1,ny1,izoom,&
		xtitle,ytitle,rescale,dxs,dys,ifitype,parval,theta,ncomp,isval,sval,iplot,&
		calbarx,calbary,xbeg4,ybeg4,xend4,yend4,&
        xbeg5,ybeg5,xend5,yend5,iparfirst,ntrace,ytsep,calfac,ioff,iend,&
        np1,np2,adcfil,xwbase)
		isens=1
		igraph=igraph+1
		ngraph=ngraph+1
		iplot=iplot+1
		modplot=igraph
		jopen(modplot)=1
	!	ymax=1.1*ymax
		do i=1,100
			idraw(i)=1
		enddo
		if(igraph.le.20) then
			iplotype=1					!if(iplotype.ne.3) ndimc=20
			if(ndv1.gt.niobs) niobs=ndv1
		!	do j=1,nset
			do j=1,ncurvd
				nj(j)=ndat1(j)
			enddo
			do ml=1,ncurvc
				if(iline1(ml).lt.0) iline1(ml)=0
			enddo
			n1=1
			if(hdisp) n1=0
			isens=1
						
			call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xval1,yval1,weight,nj,niobs,njset,nplot,nset,&
			juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
			wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
			ndat1,ncurvd,icurvd1,ndv1,ndimd1,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
			xcal1,ycal1,ncal1,icurvc1,ncurvc,ndc1,ndimc1,isym,symsiz,ijoin1,iline1,isens,newrecords,&
			dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,&
			parval,ifitype)
					
							
			ijplot=ijplot+24
			ixp=ixp0+ijplot
			iyp=iyp0+ijplot
								
			newrecords(modplot)%IxP=IxP
			newrecords(modplot)%IYP=IYP				
						
			call graph1(igraph,iplot,main,ixp,iyp,graph1_1,GraphMainPanel1_1,graphics1_1,&
			wxmin,wxmax,wymin,wymax,ipos,gfile,plot,ixposv,iyposv)
							
			call draw_axis(xmin,xmax,ymin,ymax,ymin1,ymax1,&
			xtic,ytic,nx1,ny1,xcross,ycross,itx,ity,tlenx,tleny,&
			logx,logy,sqrty,doframe,icol,ntx,nty,idev,thick,itype,&
		calbarx,calbary,xbeg4,ybeg4,xend4,yend4,&
        xbeg5,ybeg5,xend5,yend5)
		
			call draw_text(parval,xtitle,ytitle,ztitle,title1,cnumx,cnumy,&
			cnumz,cexpx,cexpy,cexpz,newtext,numbx,numby,numbz,ntext,inumx,&
			inumy,intx,angle,sizetext,ijus,thick,rx,ry,idraw,&
			icol,ifnt,str,dxs,dys)				
							!	kwi=niobs
							!	kwj=njset
			
				
				kwid=100
				kwjd=10		
					
				call draw_data(xval1,yval1,icurvd1,ncurvd,ndelt,logity,logx,logy,sqrty,&
				y0,yinf,x0,ilog,idev,ndat1,ijoin1,icol,isym,&
				symsiz,thick,barleng,interp,npint,Xint,Yint,nintg,yp1,ypn,&
				Y2int,ndv1,ndimd1,weight,kwid,kwjd,dxs,dys,icurvw,jmiss,n1,idraw,&
				ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil)
			
							
			call draw_curves(xcal1,ycal1,icurvc1,ncurvc,logity,logx,&
			logy,sqrty,y0,yinf,x0,ilog,idev,&
			wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
			xmin,xmax,ymin,ymax,ncal1,&
			iline1,icol,thick,ndc1,ndimc1,jfirst,dxs,dys)
			
			call draw_lines(IDRAW,ICOL,THICK,C_THICK,NARROW,NLINE,NHLINE,NVLINE, &
			XBEG,YBEG,XEND,YEND,y0,yinf,x0,dxs,dys)
			xmin0=xmin1
			xmax0=xmax1
			ymin0=ymin1
			ymax0=ymax1
			idest=0
		!	readrec=.true.	
		endif
		icall=9028
		goto 9009		
		case(9200) ! histo
		!(3) Distribution of sum of two parameters'
				
                !(4) Distribution of difference between two parameters'
                !(5) Distribution of product of two parameters'
                !(6) Distribution of ratio of two parameters'
	ndimd=1
	ndimc=1
    ndv1=511	!dimensions as for earlier versions

	ndc1=1	!no calc curves
	
	isetcol=0
	do i=1,100
	   icol(i)=-1	!default colour
	enddo
	shist=.false.
	sbin=.false.
	ncalc=1
			hdisp=.true.
			iopt=5
			if(nyval.le.500) iopt=4
			if(nyval.le.100) iopt=3
			scalefac=1.0
			scaled=.false.
200			continue
		
			call VARV1(ypar,Nyval,xbar,varx,xmin,xmax)
			if(xmax.gt.1.e5) then
				n=ifixr(alog10(xbar))-1
				itypeval(1)=0
				val(1)=n
				vtext(1)='Scale down observations by factor of 10^n '
				call val_table(Main,ivwin9,1,vtext,ival,val,9221,itypeval,iplot_Toggle0)
			else
				icall=9222
				goto 9009
			endif
	case(9221)
			val(1)=gmenqvaluesetting(ival(1))
			if(val(1).le.1) val(1)=1
			n=val(1)
	
		    call gmremovewindow(ivwin9)
			scalefac=10**n
			scaled=.true.
			do i=1,nyval
			ypar(i)=ypar(i)/scalefac
			enddo
			if(discprt) write(7,119) scalefac
119			format(/,' Observations scaled down by factor = ',g13.6)
    		call VARV1(ypar,Nyval,xbar,varx,xmin,xmax)
			icall=9222
			goto 9009
	case(9222)
			if(varx.gt.0.0) sd=sqrt(varx)
			cv=100.*sd/xbar
			en=float(nyval)
			sdm=sd/sqrt(en)
			cvm=cv/sqrt(en)
			write(string,fmt='(a20,i5,a10,g13.6,a4,g13.6,a12,g13.6,a4,g13.6)') &
	       ' Mean and SD of ',nyval,' values= ',xbar,'+/-',sd,&
		   ' Range from ',xmin,'to ',xmax
     
	        CALL GMSETTEXTSETTING(ITTY,string)
			write(string,fmt='(a6,g13.6,a2,a6,g13.6,a6,g13.6)')&
			' CV = ',cv,'%',' SDM = ',sdm,' CVM = ',cvm		
			CALL GMSETTEXTSETTING(ITTY,string)
		
			if(discprt) write(7,413) NYVAL,xbar,sd,xmin,xmax,cv,sdm,cvm
413			FORMAT(/,' Mean and SD of ',i5,' values= ',G13.6,' +/-',g13.6,&
			/,' Range from ',g13.6,' to ',g13.6,/,&
			' CV = ',g13.6,'%',/,' SDM = ',g13.6,',  CVM = ',g13.6,'%')
			if(dabs(true).gt.1.d-10) then
			if(scaled) then
				bias=1.d2*(xbar-true/scalefac)/(true/scalefac)
			else
				bias=1.d2*(xbar-true)/true
			endif
			write(string,fmt='(a30,g13.6)') ' Bias = (mean-true)/true = ',bias
			CALL GMSETTEXTSETTING(ITTY,string)
			if(discprt) write(7,4131) bias
4131		format(' Bias = (mean-true)/true = ',g16.6,'%')
			

		
			endif
			if(sbin) then
			 icall= 9202
			 goto 9009
			endif
			
				
			icall=9201
			goto 9009
	case(9201)
			if(itipe.ne.4) xmin=0.0		!reset for histogram
			setmax=.false.	!xmax set automatically, not manually
			if(.not.sbin) logt=.false.
			iplot=iplot+1
			iplotp=-1
			icall=9202
			call plot_options(main,iplot0,iplotp,icall,nlig,nrange,hjmcalc,ligname,ylo,yhi,conc,&
            iplot_Value2,iplot_text,iplot_TxtArray2,iplot_Toggle1,iplot_Toggle3,iplot_Toggle4,&
            title1)
           
			call gmSetToggleSwitch(	iplot_Toggle3(3),Gon)
			goto 9001
  	case(9051:9055)
			iopt7=icall-50
			goto 9001
  case(9203)
		call gmremovewindow(iplot0)
	
	    nbin=0
	    goto 9001
  case(9202)
	!	goto 1
	if(sbin) goto 77
		do i=1,5
   			istatus1=gmEnqToggleSwitch(iplot_toggle3(i)) ! instead of 0
			if (istatus1.eq.gon) iopt7=i
		enddo
		if(iopt7.eq.1) logt=.true.

		nbw=1
		call gmremovewindow(iplot0)
		if(nyval.le.300) nbdec=5				!default
		if(nyval.gt.300.and.nyval.le.1000) nbdec=8	!default
		if(nyval.gt.1000.and.nyval.le.3000) nbdec=10	!default
		if(nyval.gt.3000) nbdec=12				!default
		vtext(50)=' Histogram settings:'
		xendk=1. + xmax - amod(xmax,1.)
		if(iopt7.eq.1) then
			xaxis(1)=0.01
			itypeval(1)=9  
			if(sbin) then
			val(1)=xaxis(1)
			vtext(1)=' Histogram to start at (ms)'
			dx=exp(alog(10.)/float(nbdec))
			xwbase=alog10(dx)
			ntog=1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
			else
			val(1)=xaxis(1)
			vtext(1)=' Histogram to start at (ms)'
			if(logt.and.xaxis(1).le.0) vtext(1)=' Histogram (log) to start at '
			val(2)=nbdec
				itypeval(1)=9  
			itypeval(2)=0
				itypeval(3)=9  
			vtext(2)=' Number of bins/decade '
			val(3)=xendk
			vtext(3)=' Last x value (ms) '
			ntog=3
			endif
		else if(iopt7.eq.2) then
			xaxis(1)=0.
			if(sbin) then
				val(1)=xaxis(1)
				vtext(1)=' Histogram to start at (ms)'
				itypeval(1)=9
				ntog=1
			else
				val(1)=xaxis(1)
				vtext(1)=' Histogram to start at (ms)'
				vtext(2)=' Number of different bin widths (-1 to skip histo)'
				itypeval(2)=0
				itypeval(1)=9
				val(1)=xaxis(1)
				val(2)=1
				ntog=2
			endif
		
		endif
77		if(iopt7.ge.3) then
		if(.not.sbin) then
		m=1
		do i=1,nbw
	

			nbin=20
			if(iopt7.eq.4) nbin=40
			if(iopt7.eq.5) nbin=60
			dxd=abs(xmax-xmin)
			dx=dxd/float(nbin)
			if(dx.ne.0)call SETTIC(dx)
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
		icall=9210
		goto 9009	
		endif
		else
				
				call value_table(Main,ivwin,ntog,vtext,ival,val,9204,itypeval)
		endif
		goto 9001
	case(9204)
		do i=1,ntog
			val(i)=gmenqvaluesetting(ival(i))
		enddo
		xaxis(1)=val(1)

		if(iopt7.eq.1) then
		if(.not.sbin) then
			nbdec=int(val(2))
			xendk=val(3)
		endif 
		else
		if(.not.sbin) then
			nbw=int(val(2))
		
		endif 
		endif
		call gmremovewindow(ivwin)
		if(logt.and.xaxis(1).le.0.) xaxis(1)=0.01
		if(nbw.eq.0) nbw=1
		if(nbw.le.-1) then
		nbin=0
		if(allocated(thetas)) deallocate(thetas)
		goto 9001
		endif
	
	
		if(iopt7.eq.1) then
			dx=exp(alog(10.)/float(nbdec))
			nbin=1+ifix(alog(xendk/xaxis(1))/alog(dx))
			xendk=(dx**nbin)*xaxis(1)
	
			if(setmax) xmax=xendk
			do i=1,nbin
			xaxis(i+1)=xaxis(1)*(dx**i)
			enddo
			mlast=nbin+1
			xwbase=alog10(dx)
			icall=9210
			goto 9009
		
		else
			if(.not.sbin)then
		    vtext(1)=' Base width for frequency density= '
			val(1)=xwbase
		    val(1)=dx
			val(2)=xmax	 
			vtext(1)=' Bin width (ms)'
			vtext(2)='Last x value (pA etc)= '
			itypeval(1)=5
			itypeval(2)=5
	
			do i=1,nbw
	
			setmax=.true.		!xmax set manually
	   		mLAST=m
			val_2(1,i)=0.
			val_2(2,i)=0.
		
			enddo
		
			call value_table_2(Main,iv2win,2,nbw,vtext,ititle2,ival21,val_2,9205,itypeval)
		endif

	endif
	goto 9001
	case(9205)
	do j=1,nbw
		setmax=.true.
		val_2(1,j)=gmenqvaluesetting(ival21(1,j))
		val_2(2,j)=gmenqvaluesetting(ival21(2,j))
		dx=val_2(1,j)
		if(dx.le.0.0) dx=1.
		xendk=val_2(2,j)
		xn=(xendk-xaxis(mlast))/dx

		itempg=IFIX(0.5+xn)
	   	if(abs(xn-float(itempg)).ge.dx*.01) then
			itemp=IFIXr(xn)
	   		xendk=xaxis(mlast) + (float(itempg))*dx
			call gmsetvaluesetting(ival21(2,j),xendk)
			call gmdrawwidget(ival21(2,j))
		endif	   
		nbin=IFIX(0.5+xn)
		mlast=m
		if(setmax) xmax=xendk
		if(nbin.gt.0) then
		do l=1,nbin
			m=m+1
			XAXIS(m)=XAXIS(mLAST)+(float(l))*DX
			xsav(j,1)=float(nbin)
			xsav(j,2)=dx
			xsav(j,3)=xaxis(mlast)
			xsav(j,4)=xaxis(m)
		enddo
		call gmsetfontattribs(ival21(2,j),gmtextcol=11)
		else
			nwrongbin=-1
			call gmsetfontattribs(ival21(1,j),gmtextcol=12)
		    call gmsetfontattribs(ival21(2,j),gmtextcol=12)
	
		endif 
			call gmsetvaluesetting(ival21(1,j),dx)
			call gmdrawwidget(ival21(1,j))
			call gmsetvaluesetting(ival21(2,j),xendk)
			call gmdrawwidget(ival21(2,j))
	enddo
	if(m.gt.501) then
			imes=gmdisplaymessagebox('','Too many bins',gstop,gok)
			call gmRemoveWindow(iv2win)
			! remove win
		icall=9202
		goto 9009
	endif
	if(nwrongbin.eq.-1) then
		nwrongbin=0
		imes=gmdisplaymessagebox('','Please correct the values in red',gstop,gok)
	else
		nbin=m-1
		xwbase=dx
			call gmRemoveWindow(iv2win)
		if(nbw.gt.1) then
			
		endif
		icall=9210
		goto 9009
	endif
	case(9210)
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
	 if(shist.and.sbin) goto 4071
	flo=0.
	fhi=0.
	do i=1,510
	   freq(i)=0.0
	enddo

	do i=1,nyval
	   yv=YPAR(i)		!array defined in call PAR() above
	   if(yv.lt.xaxis(1)) then
		flo=flo+1.0
	   else if(yv.gt.xaxis(nbin+1)) then
		fhi=fhi+1.0
	   else
	    do j=1,nbin
		if(yv.ge.xaxis(j).and.yv.lt.xaxis(j+1)) freq(j)=freq(j)+1.0
	    enddo
	    if(yv.eq.xaxis(nbin+1)) freq(nbin)=freq(nbin)+1.0
	   endif
	enddo
	if(flo.lt.0.1) flo=-2.	!do not plot if flo=0
	if(fhi.lt.0.1) fhi=-2.	!ditto
	fmax=0.
	do j=1,nbin
	   if(.not.logt) freq(j)=freq(j)*xwbase/(xaxis(j+1)-xaxis(j))
	   if(freq(j).gt.fmax) fmax=freq(j)
	enddo
	if(flo.gt.fmax) fmax=flo
	if(fhi.gt.fmax) fmax=fhi
	i=ifix(0.1+flo)
	if(flo.lt.0.) i=0	!for print
	j=ifix(0.1+fhi)
	if(fhi.lt.0.) j=0
	string=' No of values below Xlow= '//cnum5
	CALL GMSETTEXTSETTING(ITTY,string)
	call intconv(j,cnum5)
	string=' No of values above Xhigh= '//cnum5
	CALL GMSETTEXTSETTING(ITTY,string)
	if(discprt) write(7,222) i,j
222	FORMAT(' No of values below Xlow= ',i8,' No of values above Xhigh= ',i8)
4071 continue
	! type freq dens???
	if(flo.lt.0.) flo=0.
	if(fhi.lt.0.) fhi=0.
	icall=9211
	goto 9009
case(9211)
	ilabel=1
	titley='Frequency density '
	xw=xwbase  !so Y axis labelled 'per 10 ms' (reset below as nec)
	if(logt) xw=0.	!not so labelled for log-bins

	titley='Frequency density'
	if(itipe.eq.1) then
	   if(ipp.le.npar) then
		titlex(1:10)=titlep(ipp)
		if(ipp.eq.8+id.or.ipp.eq.10+id.or.ipp.eq.12+id.or.ipp.eq.14+id) then
		   nb=len_trim(titlep(ipp))	!must exclude ascii 0
	         titlex(1:18)=titlep(ipp)(1:nb)//' x 10^-8'
		endif
	   endif

	   if (imod0.eq.0) then
	    if(ipp.eq.npar+1) then
		if(ips1.eq.0) then
		   titlex=' L(max)      '
		else
		   call INTCONV(ips1,cnum)
		   titlex=' L(max) for set '//cnum
		endif
	    else if(ipp.eq.npar+2) then
		titlex=' number of evaluations       '
	    endif
	   else if(imod0.eq.29.or.imod0.eq.36.or.imod0.eq.39) then
	    if(ipp.eq.npar+1) then
		titlex=' E2 = beta2/alpha2                     '
	    else if(ipp.eq.npar+2) then
		titlex=' E1a = beta1a/alpha1a                  '
	    else if(ipp.eq.npar+3) then
		titlex=' E1b = beta1b/alpha1b                  '
	    else if(ipp.eq.npar+4) then
		titlex=' Total dissoc rate k(-2)a+k(-2)b       '
	    else if(ipp.eq.npar+5) then
		titlex=' K2b (old) =K2a (new)  (muM)           '
	    else if(ipp.eq.npar+6) then
		titlex=' K2a (old) =K2b (new)  (muM)           '
	    else if(ipp.eq.npar+7) then
		titlex=' K1a  (muM)                            '
	    else if(ipp.eq.npar+8) then
		titlex=' K1b  (muM)                            '
	    else if(ipp.eq.npar+9) then
		if(ips1.eq.0) then
		   titlex=' L(max)      '
		else
		   call INTCONV(ips1,cnum)
		   titlex=' L(max) for set '//cnum
		endif
	    else if(ipp.eq.npar+10) then
		titlex=' number of evaluations       '
	    endif
	   endif

	endif
	icall=9213
	goto 9009
case(9213)
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

	ifitype=0

	lt2=2       !short dash for lo,hi bins
	ilog=0
	if(logt) ilog=6		!plot sqrt(y) vs log(t)
	iscal=0
	ncurvd=1
	icurvd(1)=1
	ndat(1)=nbin+2
	ijoin(1)=0
	do i=1,nbin+1
	    xval(i,1)=xaxis(i)
	enddo
	dx=xaxis(nbin+1) - xaxis(nbin)
	xval(nbin+2,1)=xval(nbin+1,1) + dx		!extra bin in case fhi>0
	if(setmax) xmax=xval(nbin+2,1)
	do i=1,nbin+2
	   yval(i,1)=freq(i)
	enddo	
	ncurvc=0
	icurvc(1)=1		!total
	fmin=0.
	fmax=fmax*1.2		     !value from SETBIN
	itemp=1+ifix(fmax)
	fmax=float(itemp)	!ROUND UP
	ftic=0.5*10.0**ifix(alog10((fmax-fmin)*0.5))
2102	if((fmax-fmin)/ftic.gt.10.) goto 2101
	ftic=ftic/2.
	goto 2102	!ensure at least 5 tics so get numerical label
2101	xtic=2.*xwbase	!number every 10th bin
	xtic=xtic*5.
	if(.not.logt) ftic=ftic*5.	!smaller tics for sqrt scale for now

	yval(0,1)=flo		!set whether 0 or not- it is checked in VHIST
	yval(nbin+1,1)=fhi	!=0. if no hi bin

	if(itipe.eq.1.and.ip.le.npar+8) then
	   sval=sngl(thtrue(ipp))
	   if(scaled) sval=sval/scalefac
	   isval=1
	   if(ASSOC(ip,id,imod0)) then
		sval=sval*1.e-8
	   endif
	else if(itipe.ge.3.and.itipe.le.6) then
	   sval=sngl(true)
	   if(scaled) sval=sval/scalefac
	   isval=1
	else
	   isval=0
	endif
	icall=9250		
	goto 9009
!	goto 1
case(9250)
		! histo
		ifontrue=101	
		call calc_default(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,symsiz,&
		ndimd,XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,&
		XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,&
	    XLO1,XHI1,YLO1,YHI1,y0,yinf,inumx,inumy,&
		titlex,titley,ilabel,doframe,autplt,itit,title1,&
		ISHP,ifont,landscap,ndv1,ndc1,weight,kwi,kwj,icurvw,kmax,iver,&
		redrawn,plot,redo,pcgm,pwmf,npint,hdisp,n1,tlenx,tleny,nx1,ny1,izoom,&
		xtitle,ytitle,rescale,dxs,dys,ifitype,parval,theta,ncomp,isval,sval,iplot,&
		calbarx,calbary,xbeg4,ybeg4,xend4,yend4,&
        xbeg5,ybeg5,xend5,yend5,iparfirst,ntrace,ytsep,calfac,ioff,iend,&
        np1,np2,adcfil,xwbase)
		isens=1
		igraph=igraph+1
	
		ngraph=ngraph+1
		modplot=igraph
		jopen(modplot)=1
	!	ymax=1.1*ymax
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
			iplotype=2
				kwid=100
				kwjd=10				
			call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xval,yval,weight,nj,niobs,njset,nplot,nset,&
			juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
			wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
			ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
			xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,newrecords,&
			dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,&
			parval,ifitype)
					
			ijplot=ijplot+24
			ixp=ixp0+ijplot
			iyp=iyp0+ijplot
								
			newrecords(modplot)%IxP=IxP
			newrecords(modplot)%IYP=IYP				
						
			call graph1(igraph,iplot,main,ixp,iyp,graph1_1,GraphMainPanel1_1,graphics1_1,&
			wxmin,wxmax,wymin,wymax,ipos,gfile,plot,ixposv,iyposv)
							
5			call draw_axis(xmin,xmax,ymin,ymax,ymin1,ymax1,&
			xtic,ytic,nx1,ny1,xcross,ycross,itx,ity,tlenx,tleny,&
			logx,logy,sqrty,doframe,icol,ntx,nty,idev,thick,itype,&
		calbarx,calbary,xbeg4,ybeg4,xend4,yend4,&
        xbeg5,ybeg5,xend5,yend5)
		
			call draw_text(parval,xtitle,ytitle,ztitle,title1,cnumx,cnumy,&
			cnumz,cexpx,cexpy,cexpz,newtext,numbx,numby,numbz,ntext,inumx,&
			inumy,intx,angle,sizetext,ijus,thick,rx,ry,idraw,&
			icol,ifnt,str,dxs,dys)				
							!	kwi=niobs
							!	kwj=njset
			
				call draw_hist(xval,yval,icurvd,ncurvd,ndelt,logity,logx,&
				logy,sqrty,y0,yinf,x0,ilog,xmin,xmax,ymin,ymax,idev,ndat,ijoin,&
				icol,thick,ndv1,ndimd,xwbase,lt2)
								
            
							
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
		!	readrec=.true.	
			icall=9028
			goto 9009			
		!	goto 1
		endif
	case (5101:5125) ! save files to plotq 
	       modplot=icall-5100
		   igraph=modplot
		   iplot=modplot
555			continue	
		   CALL gmFileBROWSER(qFILE,eDIR,eFILT,gmBrowseType=1)
				IF(qFILE.ne.' ') then
			
				
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
					goto 9001
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
			
	case(5128)
		call gmremovewindow(iqwin)
	case(5127)
		iqplot=gmenqvaluesetting(ivalq)
		if(iqplot.lt.1) iqplot=1
		if(nplot.gt.0.and.iqplot.gt.nplot+1) iqplot=nplot+1
		call gmremovewindow(iqwin)
		
		kwi=newrecords(modplot)%kwi
						kwj=newrecords(modplot)%kwj
						ndv1=newrecords(modplot)%ndv1
						ndimd=newrecords(modplot)%ndimd
						ndc1=newrecords(modplot)%ndc1
						ndimc=newrecords(modplot)%ndimc
						ipos=newrecords(modplot)%ipos
						n1=1
						if(newrecords(modplot)%hdisp) n1=0
						hdisp=newrecords(modplot)%hdisp
						isens=0
						if(hdisp) then
						iplotype=2
						call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xval,yval,weight,nj,niobs,njset,nplot,nset,&
			juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
			wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
			ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
			xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,newrecords,&
			dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,&
			parval,ifitype)
						call write_plotqred(iqplot,iplotype,xval,yval,xcal,ycal,ndimd,ndimc,&
				ncurvd,ndat,icurvd,isym,ijoin,ncurvc,ncal,icurvc,iline,symsiz,&
				xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ntx,nty,itx,ity,&
				xlo1,xhi1,ylo1,yhi1,itit,title1,ilog,iscal,doframe,&
				titlex,titley,ilabel,inumx,inumy,qfile,sval,&
				ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,y1v,y2v,y1c,y2c,ivplot,&
				theta,ifitype,ncomp,idest,interp,&
				RLTH,y0,yinf,ntrace,ytsep,ndv1,ndc1,w,kwi,kwj,icurvw,kmax,n1,jmiss,&
				xwbase,isval,lt2,mono,ifont0,ameant,areat,ioffs)
						else
						iplotype=1
						call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xval1,yval1,weight,nj,niobs,njset,nplot,nset,&
			juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
			wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
			ndat1,ncurvd,icurvd1,ndv1,ndimd1,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
			xcal1,ycal1,ncal1,icurvc1,ncurvc,ndc1,ndimc1,isym,symsiz,ijoin1,iline1,isens,newrecords,&
			dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,&
			parval,ifitype)
						call write_plotqred(iqplot,iplotype,xval1,yval1,xcal1,ycal1,ndimd1,ndimc1,&
				ncurvd,ndat1,icurvd1,isym,ijoin1,ncurvc,ncal1,icurvc1,iline1,symsiz,&
				xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ntx,nty,itx,ity,&
				xlo1,xhi1,ylo1,yhi1,itit,title1,ilog,iscal,doframe,&
				titlex,titley,ilabel,inumx,inumy,qfile,sval,&
				ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,y1v,y2v,y1c,y2c,ivplot,&
				theta,ifitype,ncomp,idest,interp,&
				RLTH,y0,yinf,ntrace,ytsep,ndv1,ndc1,w,kwi,kwj,icurvw,kmax,n1,jmiss,&
				xwbase,isval,lt2,mono,ifont0,ameant,areat,ioffs)
						endif
						
						
				
		continue
	case (5151:5175)	
		    landscape=.true.
			modplot=icall-5150
			iplot=modplot
		    igraph=modplot
			ifontrue=101
			idev=6
			istatus=gmprintercontrol(gprintersetup)
				if(istatus.eq.0) then
				    goto 9001 
				else
				istatus=gmprintercontrol(GOPENPRINTER)
				IF(ISTATus.NE.0)THEN
					plot=.true.
					icallprev=5150
						icall=5250
						goto 9009
					endif
					
				endif
	case (5201:5225) ! bmp,wmf
            landscape=.false.
		    modplot=icall-5200
		    ifontrue=0
		    iplot=modplot
		    igraph=modplot
		    pwmf=.true.
		            call intconv(iplot,cnum0)
						    sfile='sim'//cnum0(1:3)
						    nlp=len_trim(sfile)
						    if(pwmf) sfile=sfile(1:nlp)//'.wmf'
						    if(pbmp) sfile=sfile(1:nlp)//'.bmp'
						    
		           if (pwmf) then
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
	   				icall=5250
	   				CALL GMSETTEXTSETTING(ITTY,'writing to wmf file')
	   				imesw=gmdisplaymessagebox('','Writing to:'//sfile,ginformation,gok)
	   				goto 9009
		
	case(5250)
                        kwi=newrecords(modplot)%kwi
						kwj=newrecords(modplot)%kwj
						ndv1=newrecords(modplot)%ndv1
						ndimd=newrecords(modplot)%ndimd
						ndc1=newrecords(modplot)%ndc1
						ndimc=newrecords(modplot)%ndimc
						ipos=newrecords(modplot)%ipos
						n1=1
						if(newrecords(modplot)%hdisp) n1=0
						hdisp=newrecords(modplot)%hdisp
						isens=0
						if(hdisp) then
						call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xval,yval,weight,nj,niobs,njset,nplot,nset,&
			juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
			wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
			ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
			xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,newrecords,&
			dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,&
			parval,ifitype)
						else
						call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xval1,yval1,weight,nj,niobs,njset,nplot,nset,&
			juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
			wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
			ndat1,ncurvd,icurvd1,ndv1,ndimd1,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
			xcal1,ycal1,ncal1,icurvc1,ncurvc,ndc1,ndimc1,isym,symsiz,ijoin1,iline1,isens,newrecords,&
			dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,&
			parval,ifitype)
						endif
						if(pwmf) call wmfp(idpi,ixoff,iyoff,iwidi,ihei)
						if(pbmp) call bmp
						if(pwmf.or.pbmp) then
						!    call devsus 
						    call devfil(sfile,0)
						endif    
						plot=.true.
				call graph1(igraph,iplot,main,ixp,iyp,graph1_1,GraphMainPanel1_1,graphics1_1,&
			wxmin,wxmax,wymin,wymax,ipos,gfile,plot,ixposv,iyposv)
							
			call draw_axis(xmin,xmax,ymin,ymax,ymin1,ymax1,&
			xtic,ytic,nx1,ny1,xcross,ycross,itx,ity,tlenx,tleny,&
			logx,logy,sqrty,doframe,icol,ntx,nty,idev,thick,itype,&
		calbarx,calbary,xbeg4,ybeg4,xend4,yend4,&
        xbeg5,ybeg5,xend5,yend5)
		
			call draw_text(parval,xtitle,ytitle,ztitle,title1,cnumx,cnumy,&
			cnumz,cexpx,cexpy,cexpz,newtext,numbx,numby,numbz,ntext,inumx,&
			inumy,intx,angle,sizetext,ijus,thick,rx,ry,idraw,&
			icol,ifnt,str,dxs,dys)				
				if(hdisp) then
				
				call draw_hist(xval,yval,icurvd,ncurvd,ndelt,logity,logx,&
				logy,sqrty,y0,yinf,x0,ilog,xmin,xmax,ymin,ymax,idev,ndat,ijoin,&
				icol,thick,ndv1,ndimd,xwbase,lt2)
				else
				call draw_data(xval1,yval1,icurvd1,ncurvd,ndelt,logity,logx,logy,sqrty,&
				y0,yinf,x0,ilog,idev,ndat1,ijoin1,icol,isym,&
				symsiz,thick,barleng,interp,npint,Xint,Yint,nintg,yp1,ypn,&
				Y2int,ndv1,ndimd1,weight,kwid,kwjd,dxs,dys,icurvw,jmiss,n1,idraw,&
				ntrace,ytsep,calfac,ioff,iend,np1,np2,adcfil)
				endif
				call draw_curves(xcal,ycal,icurvc,ncurvc,logity,logx,&
			logy,sqrty,y0,yinf,x0,ilog,idev,&
			wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
			xmin,xmax,ymin,ymax,ncal,&
			iline,icol,thick,ndc1,ndimc,jfirst,dxs,dys)
			
			call draw_lines(IDRAW,ICOL,THICK,C_THICK,NARROW,NLINE,NHLINE,NVLINE, &
			XBEG,YBEG,XEND,YEND,y0,yinf,x0,dxs,dys)
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
	
	case(5501:5525) ! ascii
	   modplot=icall-5500
        igraph=modplot
        iplot=modplot
        if(modplot.gt.0) then
        
						kwi=newrecords(modplot)%kwi
						kwj=newrecords(modplot)%kwj
						ndv1=newrecords(modplot)%ndv1
						ndimd=newrecords(modplot)%ndimd
						ndc1=newrecords(modplot)%ndc1
						ndimc=newrecords(modplot)%ndimc
						ipos=newrecords(modplot)%ipos
						n1=1
						if(newrecords(modplot)%hdisp) then
						    n1=0
						    hdisp=.true.
						endif
						isens=0
						if(hdisp) then
						iplotype=2
						call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xval,yval,weight,nj,niobs,njset,nplot,nset,&
			juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
			wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
			ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
			xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,newrecords,&
			dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,&
			parval,ifitype)
			        else
			        iplotype=1
						call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xval1,yval1,weight,nj,niobs,njset,nplot,nset,&
			juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
			wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
			ndat1,ncurvd,icurvd1,ndv1,ndimd1,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
			xcal1,ycal1,ncal1,icurvc1,ncurvc,ndc1,ndimc1,isym,symsiz,ijoin1,iline1,isens,newrecords,&
			dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,&
			parval,ifitype)
			        endif
						
		CALL gmFileBROWSER(filasc,ascpath,ascdef,gmBrowseType=1)
		if(filasc.ne.'  ') then
				INQUIRE (FILE=filasc,EXIST=PRESENT)
				if(PRESENT) then
					ihandle=FILE$FIRST
					length = GETFILEINFOQQ(filasc, info, ihandle)
					nLEN=info%length
					if(nlen.lt.50) then
						imko=gmdisplaymessagebox('','Not a proper ASCII file',ginformation,gok)
						goto 9001
					else
							imko=gmdisplaymessagebox('','File already exist.Overwrite it?',&
							gquestion,gyesno)
							if(imko.eq.gnobutton) goto 9001
			
					endif
				endif
				OPEN(unit=12,file=filasc,status='UNKNOWN',&
                access='SEQUENTIAL',form='FORMATTED')
      if(hdisp) then
	     if(ncurvd.gt.0) then
		 write(12,FMT=8000) ' number of data curves = ',ncurvd
8000     format(a25,i10)
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
	   else
	    if(ncurvd.gt.0) then
		        write(12,FMT=8000) ' number of data curves = ',ncurvd
		 j=1
	   do j1=1,ncurvd
		   j=icurvd1(j1)
		   write(unit=12,fmt=800) ndat1(j)
!800		   format(i10)
		   do i=1,ndat1(j)
			write(unit=12,fmt=801) Xval1(i,j),Yval1(i,j)
!801			format(2f15.5)
		   enddo
		enddo
	   endif
	   if(ncurvc.gt.0) then
		write(12,FMT=8000) ' number of calc curves = ',ncurvc
	      j=1
		do j1=1,ncurvc
		   j=icurvc1(j1)
		   write(unit=12,fmt=800) ncal1(j)

		   do i=1,ncal1(j)
			write(unit=12,fmt=801) Xcal1(i,j),Ycal1(i,j)

		   enddo
		enddo
	   endif
	   endif
	   CLOSE(unit=12)
		endif		
		endif
	case(7201:7300)
	            modplot=icall-7200
				igraph=modplot
				call gmActivateGraphicsFrame(graphics1_1(modplot))
				lframe=graphics1_1(modplot)
	
	case(1:400) ! edit,fonts,colour,size,box,delete,etc
		if(modplot.gt.0) then
			xmaxsav=xmax1	!in common: record xmax to detect if it is changed in graph_attrib
			
			rescalex=.false.
	        jrplot=newrecords(modplot)%iplot	
	        readrec=.true.
			call graph_attributes(main,icall,modplot,lframe,jindex,ind_1,newrecords,readrec,newfile,&
		graphics1_1,combo1_6,combo1_4,combo1_5,combo1_8,combo1_9,combo1_10,istate,hdisp,izoom,xtitle,ytitle,&
		xmin0,xmax0,ymin0,ymax0,itogglepanel,itext_entry,new_text_entry,jrplot,iptype,rescalex,&
		isym,symsiz,combo1_11,combo1_12,imodax,iaminax,iamaxax,iatic,imtext3,irxx,iryy,&
		calbarx,calbary,xbeg4,ybeg4,xend4,yend4,&
		ivalx1,ivaly1,ivalx2,ivaly2,ivaltx,ivalty,iax_entry)
		if(icall.eq.247.or.icall.eq.262) then
			icall=405
			goto 9009

		endif
		if(icall.eq.407) goto 9009


else
		!imesr=gmdisplaymessagebox('','No graph on display',ginformation,gok)
		!icall=0
endif
		  case(401,402,405) ! Shape,scale
		
		  if(icall.eq.401) then
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
		  else  if(icall.eq.402) then ! scale
			do i=2,8
					ifstat(i)=gmEnqListEntry(Combo1_2,i,textcombo)
					if(ifstat(i).eq.2) ilog=i-2
			enddo
          endif
		   	
			if(modplot.gt.0) then
			
						kwi=newrecords(modplot)%kwi
						kwj=newrecords(modplot)%kwj
						ndv1=newrecords(modplot)%ndv1
						ndimd=newrecords(modplot)%ndimd
						ndc1=newrecords(modplot)%ndc1
						ndimc=newrecords(modplot)%ndimc
						ipos=newrecords(modplot)%ipos
						
						n1=1
						if(newrecords(modplot)%hdisp) then
						    n1=0
						    hdisp=.true.
						endif
						isens=0
						if(hdisp) then
						iplotype=2
						call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xval,yval,weight,nj,niobs,njset,nplot,nset,&
			juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
			wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
			ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
			xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,newrecords,&
			dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,&
			parval,ifitype)
			        else
			        iplotype=1
						
			
				call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xval,yval,w,nj,niobs,njset,nplot,nset,&
				juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
				wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
				ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
				xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,newrecords,&
				dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,parval,ifitype)
				endif
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
			if(icall.eq.405) then
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
					if(icall.eq.402) iscal=5
			iscal=5
			endif
		
		
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
				jplot=newrecords(modplot)%iplot
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
				call draw_lines(IDRAW,ICOL,THICK,C_THICK,NARROW,NLINE,NHLINE,NVLINE, &
				XBEG,YBEG,XEND,YEND,y0,yinf,x0,dxs,dys)
				isens=1
				if(hdisp)then
				call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xval,yval,w,nj,niobs,njset,nplot,nset,&
				juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
				wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
				ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
				xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,newrecords,&
				dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,parval,ifitype)
				else
				call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xval,yval,w,nj,niobs,njset,nplot,nset,&
				juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
				wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
				ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
				xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,newrecords,&
				dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,parval,ifitype)
			
				
				zoom=.false.
				izoom=1
				ipos=0
				rescale=.false.
				iscal=0
	endif
				endif
case(407)
        
						kwi=newrecords(modplot)%kwi
						kwj=newrecords(modplot)%kwj
						ndv1=newrecords(modplot)%ndv1
						ndimd=newrecords(modplot)%ndimd
						ndc1=newrecords(modplot)%ndc1
						ndimc=newrecords(modplot)%ndimc
						ipos=newrecords(modplot)%ipos
						n1=1
						if(newrecords(modplot)%hdisp) then
						    n1=0
						    hdisp=.true.
						endif
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
		if(hdisp) then
		iplotype=2
		call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xval,yval,w,nj,niobs,njset,nplot,nset,&
				juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
				wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
				ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
				xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,newrecords,&
				dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,parval,ifitype)
		else
		iplotype=1
		call store_record(iplotype,igraph,iplot,IXP,IYP,IPOS,xval,yval,w,nj,niobs,njset,nplot,nset,&
				juse,nsfit,jset,xtitle,ytitle,title1,xmin,ymin,xmax,ymax,xmin1,ymin1,xmax1,ymax1,&
				wxmin,wymin,wxmax,wymax,xtic,ytic,nTx,nTy,xcross,ycross,itx,ity,tlenx,tleny,&
				ndat,ncurvd,icurvd,ndv1,ndimd,logx,logy,sqrty,logity,doframe,inumx,inumy,y0,yinf,x0,&
				xcal,ycal,ncal,icurvc,ncurvc,ndc1,ndimc,isym,symsiz,ijoin,iline,isens,newrecords,&
				dxs,dys,hdisp,kwi,kwj,jmiss,icurvw,n1,calbarx,calbary,nx1,ny1,parval,ifitype)
		endif
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
				icol(jindex)=newrecords(modplot)%attributes%icol(jindex)
			
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
			icol(jindex)=newrecords(modplot)%attributes%icol(jindex)
		
			call draw_curves(xcal,ycal,icurvc,ncurvc,logity,logx,&
			logy,sqrty,y0,yinf,x0,ilog,idev,&
			wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
			xmin,xmax,ymin,ymax,ncal,&
			iline,icol,thick,ndc1,ndimc,jfirst,dxs,dys)
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
            
			icol(jindex)=newrecords(modplot)%attributes%icol(jindex)
			
	
			call draw_axis(xmin,xmax,ymin,ymax,ymin1,ymax1,xtic,ytic,&
			nx1,ny1,xcross,ycross,itx,ity,tlenx,tleny,logx,logy,sqrty,doframe,&
			icol,ntx,nty,idev,thick,itype,&
		calbarx,calbary,xbeg4,ybeg4,xend4,yend4,&
        xbeg5,ybeg5,xend5,yend5)
		endif
	end select
	
	
	enddo



9010	continue
		call gmremovewindow(iplotsim)
!		icallsim=5
		icall=0
	call gmflushcallbackqueue()
12	   format(/,/,' Number of simulations recorded = ',i8,/,&
          ' Version = ',i4,' resolution = ',f8.3,2x,a11,2x,a11,/,&
          ' machine: ',a40,'   output file: ',a40,/,&
          ' k = ',i3,&
          ' # of rates = ',i4,' # fitted = ',i4,' mech = ',i3,/,&
          ' # ligands = ',i3, ' ec50 fixed ?',L4,' ec50 = ',g13.6,/,&
          ' For simplex: step = ',g13.6,' confac = ',g13.6,' errfac = ',&
       	    g13.6, ' max eval = ',i10,/,&
          ' number of sets = ',1x,i8)
13		format(' SET ',i2,': Conc = ',10g13.6)
131		format('  No. of resolved intervals: mean = ',g13.6,' SD = ',g13.6,/,&
     		'   (range from ',g13.6,' to ',g13.6,')')
17		format(6(1x,g13.6),10(1x,g13.6))
161		format(/,30(6(2x,a10,1x),/))
1611	format(8x,'    E2          E1a           E1b            k-2a+k-2b    ')

184		format(' Run #',i5)
18		format(30(6g13.6,/))
181		format(4(1x,g13.6))
19		format('&    (ABORTWed)')
20		format(1x,i3,2x,a10,'  true = ',g13.6)
30		format(/,' (',i3,') E2 = beta2/alpha2',/,' (',i3,') E1a = beta1a/alpha1a',/,&
        ' (',i3,') E1b = beta1b/alpha1b',/,' (',i3,') Total dissoc rate k(-2)a+k(-2)b',/,&
        ' (',i3,') K2b (old) =K2a (new)',/,' (',i3,') K2a (old) =K2b (new) ',/,&
        ' (',i3,') K1a ',/,' (',i3,') K1b ',/)
303		format(/,' (',i3,') E2 = beta2/alpha2',/,' (',i3,') E1a = beta1a/alpha1a',/,&
        ' (',i3,') E1b = beta1b/alpha1b',/,' (',i3,') Total dissoc rate k(-2)a+k(-2)b',/,&
        ' (',i3,') K2b (old) =K2a (new)',/,' (',i3,') K2a (old) =K2b (new) ',/,&
        ' (',i3,') K1a ',/,' (',i3,') K1b ',/' (',i3,') L(max) ',/,&
        ' (',i3,') Number of function evaluations ',/)
14		format(' #    Name            true        guess')
15	   	format(i3,1x,a10,2x,g13.6,2x,g13.6)
121		format(' For simulation, model # = ',i3,' title: ',a40,/,&
         ' with ',i3,' rates, viz.',/,30(6g13.6,/))
171		format('    Lmax         alpha2        E2           E1(a)         E1(b)',&
		'          k-2a+k-2b    Lmax for each set')
33	   format(' Values for parameter ',i3,' in range ',g13.6,' to ',g13.6)
23	   format(/,' For parameter # ',i3,' = ',a10,/,&
         1x,i4,' values out of ',i4,' (',f6.2,' %) are between ',&
         g13.6,' and ',g13.6,/,&
        ' mean = ',g13.6,' SD = ',g13.6,' CV = ',g13.6,' SDM = ',g13.6)
182					format(1x,i5,': Run #',i5)
301		format(' (',i3,') L(max) ',/,' (',i3,') Number of function evaluations ',/)		   
201		format(1x,i3,2x,a10)	
	deallocate(ypar,thetas,xval,yval,xcal,ycal,xval1,yval1,xcal1,ycal1,weight)
	end	
