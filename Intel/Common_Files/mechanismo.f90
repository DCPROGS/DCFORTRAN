subroutine mechanism(callid,igraph2,main,ifiltype,models,imodel,imodold,&
	ratcons,indrat,irecq,readini,iqs,pfilem,qmec,efile,text7,records,nrecs,ijmod,nmr,cxtrue,cytrue,&
	readrec,thetaf,jfix1,imove,neqold,icprev,conc,iformtext,ioptm,ksim,irqsav,initmec,&
	irect,iarca,indrec1,idestf,readmec,nvdep,form1,form1_txtarray1,graph1_2,GraphMainPanel1_2,&
	graphics1_2,jopen2,List7_1,ipbar_progress2,indwin,iqt1,textiqt1,iqwin,modelw,iwcell,&
	icellarray,textcell1,itoggle,lt,ipeny,ipeny_yes,ipeny_no,ipeny_xs,ipeny_fac,&
	icyc_form,itwin1,text_box,ivwin,button7,eqfit,Status_bar1,infolength,infopanel,&
	infoind,intoggle,iradio,ixm,iym,newmodel,joldmodel,nxmodel,nymodel,mtype,mod_create,&
	ngraph2,modplot2,jgraph,ipos,lframe,isw,dxsm,dysm,jmodel,igraph0,newmr,newcons,irecfin,&
	initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1,inipage,isetu,&
	ini_Value2_1,ini_TxtArray2,ini_TxtArray7,ini_TxtArray4_1,ini_TxtArray4_2,irc,iqbox,&
	iqtoggle,icell,iec50,nrows,ncube,cubedef,cubecyc,cubext,qt,liksurf,extcyc,&
	irates,jrates,inion)
	
!======================================================================================
! subroutine to deal with 
!	- new and old mechanism
!	- constraints
!	- microscopic reversibility
!======================================================================================
!	Icallid:
!   1001-1099			default mechanism as in readini+readmec
!	1100 -	1199	  create new mechanism
!   1200 -	1299		read old mechanism
!   1300 -	1399		View/Edit states properties 
!   1400 -	1499		View/Edit rates properties 

!	2001-2100		constraints

!===============================================================================
! already used:
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
!!IDRAW(i) = 1 to draw (without box) at the position that has been already
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
! PROGRAM_TYPE=-1  ALL
! PROGRAM_TYPE= 1  CVFIT
! PROGRAM_TYPE=2   AUTPLOT
! PROGRAM_TYPE=3   HCJFIT
! program_type=4 ekdist
!	

	USE DFLIB
!	use gino_f90
	use menu_f90


	include '\intel\common_files\graphics_definitions.f90'
!	include '\fortwin\hjcfit\hjcfit_definitions.f90'
	
	type (GLIMIT)    :: limits 
	TYPE (FILE$INFO) info

	type (GACTION) :: actlst
	type (Gwidget) :: widget
	logical open7
	character*11 cstring,cnum(10),cnum0,cnum2
	character cnum5*5,cnum51*5
	character*22 cnum1,cmodel
	character*10 t1(100),titlep1(100),titlep(200),tij(200)
	character*80 textcell
	character*10 qij(100)
	character*74 rtitle,rtitles(500),mtitle11
	CHARACTER*80 titlem	!title to describe rate constants
	character*30 text_box(100,10),title_box(10)
	CHARACTER*80 text7(500),togtext(100,200)
	character*(60) text_tog(100)
	character*60 vtext(50)
	character*80 actext,gititle
	integer itext_box(100,10),ititle(10),jopen2(200),nrmodel(100),istat(200),modopen(200)
	character*100 xtext,ptext,xtext1,xtext2
	character*60 :: edir,sdir
	character*120 :: efilt
	character*90 sfilt
	character*60 :: efile,sfile
	character*60 :: gfile,textec50
	character qmec*60,pfilem*40
	integer indrec(100),indrec1(100)
	real*8 ec50d,pmax
	logical monotd,indmod,liksurf
	common/ece/ec50d,pop0,pmax,curmax,concmax,monotd
	integer	ic2(2,200)
	integer ival(50),num(10),num0(10)

	integer istatab(500)
    integer mabel(100),matrix(100,100)
	integer :: iwcell(10),callid,irecfin(500)
	real val(50)
	real valdat(100)
    integer idat(100)
    integer nsc0(50)
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
		character*15 statname(n_states)
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

	TYPE (MODEL) MODELS(25)
	
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
	
	type(rate_constant) ratcons(500)
	real*8 ec501,xqlo,xqhi,ec502,xqlo2,xqhi2
		integer jfix1(200),jcon(200),jmic(200),jbad(200)
	integer ICout(2,200),icin(2,200)	!IC for connections to be NOT IN tree
	integer icspec(2,200)	!IC for specified connections
	integer iedge(2,200)	!IC for specified connections
	integer incirc(100)	
	integer Jtree(100,100)
	real*4 conc(10,10)
	character*20 lign(10)
	integer NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
		
	integer ieq(200),jeq(200),ifq(200),jfq(200)
	integer nbound(100,10),ijmod(500)
	character*20 ligname(10)
	logical obeymr(50),allmr,apfile
	integer ic(2,200)
	character *10 statname(100)	
	integer IE(200),JE(200),IF(200),JF(200),itypeval(50)
	real EFAC(200)
		real*8 assmax
	logical useprim,useprim0
	logical repeat,simulat,sim,endsim,restart
	character*40 simfile,simfile1!file names for output of simulation
	logical ABORTW,dcmod
	logical btest,autosim,dcmodel,prtec50
	integer irates(200),jrates(200),irate1(200),jrate1(200),icdep(200)
	integer irate2(200),jrate2(200)
	real*8 dgammas(200)
	logical good
	real*8 stpsav
	logical logfit,logsav
	
	real*8 perfac

	real*4 gaplo(10),gaphi(10)
	integer nskip(10)
	logical excop(10),fixec50
	integer*4 cubedef(100,8),cubecyc(100,6),cubext(100)
	logical extcyc(50)
	logical useini,set5
	logical automr(50),automr1(50),done(50)
	integer nsc1(50),im1(50,100),jm1(50,100),ir1(50),jr1(50),icmr(50)
	integer nsc3(50),im3(50,100),jm3(50,100),nsc2(50),im2(50,100),jm2(50,100)
	integer isetmr(50),icyc(50)
	integer ict(2,200)	!IC for tree
	real*8 theta0(200),thsav(200),thetaf(200)
	character*2 charmod(25,40)	!to print model
	real*8 QT(100,100)         !QT has not got conc in
	real*8 QTtrue(100,100)
	real*8 thtrue(200)
	real*8 QTsav(100,100)
	real*8 dgamma(100)
	common/LIG/nligsav,IL(100)
	common/tty/ittypanel,itty
	character*256 string
	integer IX(100),JX(100),IQs(100,100)
	integer IQ(100,100)
	integer IQf(100,100)
	integer IS(100),JS(100)	!declare for disc-write-not used now
		real*8 ec50,ec5
	real conc_ec1(10),conc_ec2(10)
	logical oneset
	logical cjump,badcyc(50),readmec
	logical newfile,discprt,pwmf,pbmp,plot,readini,readrec
	logical three_d,modify(500)
	logical d_line,d_hline,d_vline,d_arrow,d_poly,d_text,d_zoom,o_state,c_state
	logical s_text,s_line,s_arrow,delete_state,move_state
	CHARACTER*11 CXTRUE,CYTRUE
    common/modele0/imod0
	common/modele/imods
	common/KBLKs/kAs,kBs,kCs,kDs,ks
	common/cpars/ncdeps,IXs(100),JXs(100),xss
	common/LIGs/nligs,ILs(100)

	common/QPARs/ncons1,ICs(2,200)
   ! common/cube/ncube,cubedef,cubecyc,cubext	!for getrev
	COMMON/MPAR/NCYC,NSC,IM,JM
	COMMON/QPAR/NCON,IC
	
	common/mr/obeymr,automr
	common/mr1/isetmr,ncyc0
	common/ir/irate(200),jrate(200)
	logical nodata,nofit,curvonly
	common/iniset/nodata,nofit,autosim,curvonly
!	common/np1/npar
	logical penalty
	real*8 penfunc,penfac
    COMMON/EBLK/NEQ,IE,JE,IF,JF,EFAC
	COMMON/indblk/nsetq,ieq,jeq,ifq,jfq,efacq(200)
	real*8 qval,ec50out,x1s,x2s,fx1,fx2,cfacd
	real*8 qval2,ec5012,ec50out2,x1s2,x2s2,fx12,fx22 
	common/ec1/nfixec51,fixec51,qval,ec5011,ec50out,i501,j501,x1s,x2s,&
     fx1,fx2,qval2,ec5012,ec50out2,i5012,j5012,x1s2,x2s2,fx12,fx22  !for print in SIMPHJC
    logical monot
	common/ec/fixec50,nmod9,ec501,i50,j50,m50,prtec50,xqlo,xqhi,dcmodel	!nmod=imod0 (already in commom/model)
	common/ec2/monot,nlvar,vhold,vref,cfacd,dgamma !for ec50_hjc, qset_hjc
	common/ec3/ligname		!for qset_hjc
	common/KBLK/kA,kB,kC,kD
!	common/QDBLK1/QT,QD
	common/ec4/nfixec50,nlvar2,ec502,i502,j502,m502,xqlo2,xqhi2,conc_ec1,conc_ec2	 !for 2nd fixed ec50
    COMMON/MPAR1/NCYC1,NSC1,IM1,JM1
	COMMON/MPAR2/NCYC2,NSC2,IM2,JM2
	common/amax/assmax,icdep
    common/cpar/ncdep,IX,JX,x
    real*8 pomax,pmaxcalc
	logical fixpmax
	common/pen/penalty,penfunc,penfac,fixpmax,pomax,pmaxcalc 
	common/dp/discprt
	common/QDBLK2/npar,IQf,irate1,jrate1,nlig   !hjclik, qset_hjc only
	common/attributes_flag/d_line,d_hline,d_vline,d_arrow,d_poly,d_text,d_zoom,o_state,&
		c_state,s_text,s_line,s_arrow,zoom,link,delete_state,move_state                  

	common/mech_param/k,kf,nsub,kstat,ncon1,statname,icspec,icout,&
		jcon,ncin,nd1,nd2,nstate,ndim,iedge,jtree,jmic,irate2,jrate2,&
		neq0,neq1,nfix,kfit,fac,xs,&
		rtitles,iq,theta0,jfix(200),nfileb(10)
		
common/ecg/ival1a,ival2a,ival3a,iqbta,iqbta1,ibkta

common/text_attr/bold,italik,underline,ipow,idev,nolabx,ifontrue
common/switch/iswicon,igraphText(100),ibutton1,icongmod

logical*4 fastblk
	real*4 aKB
	integer*4 ifb(20),jfb(20),mfb(20),iflig
	common/fblck/fastblk,aKB,nfblock,iflig,ifb,jfb ,mfb
! Code starts here
	km=100
    dxs=0.6
	dys=0.8
	imod=imodel
	jgraph=igraph2
	call gDefineRGB(50,1.,0.80,0.8)	!pink
	call gDefineRGB(51,0.8,1.0,0.8)	!green
	call gDefineRGB(48,0.,0.,0.)
	! this is wrong leave it like it is!	
! Define IQ
!	do m=1,npar
!		i=irate(m)
!		j=jrate(m)
!		IQ(i,j)=m
!	enddo

2	continue
    select case(callid)	
	case(1001) !readini+readmec=true
			!	call prog_bar(main,ipbar,icho,ipbar_Progress2)	
				!	igraph2=igraph2+1
						ixm=ixm+1
						iym=iym+1
						if(igraph2.gt.25) then
								imessy=gmDisplayMessageBox('','Maximum 25 models on screen',Gexclamation,gok)
								igraph2=igraph2-1
								goto 1
						endif
					!	ngraph2=igraph2
						if(icprev.eq.-111) ipbar_Progress2=-100
						

					!	jopen2(igraph2)=1
						
					!	lframe=graphics1_2(igraph2)
					!	readrec=.true.
					
						indk=indrat
				        indkn=300
						igraph3=25
					    models(igraph3)%model=imodel
						igraph0=igraph2

						call copy_model(models,imodel,imodel,igraph2,igraph3,ratcons,indk,indkn)
						call draw_model(igraph3,imodel,main,graph1_2,GraphMainPanel1_2,graphics1_2,&
						models,.false.,0,5,initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
						call gFlushGraphics()
						
						goto 3313
						if(autosim.and.ksim.eq.-1) irecq=irect
						i=1
						irc=i
						mod_create=-irecq
						newmr=.false.
						newcons=.false.
						
						call create_model(ixm,iym,igraph2,imodel,main,form1,form1_txtarray1,&
						graph1_2,GraphMainPanel1_2,graphics1_2,&
						nxmodel,nymodel,mtype,models,mod_create,irc,qmec,text7(i),&
						dxs,dys,ijmod,ka,kf,ic,indrat,pfilem,open7,apfile,ipbar_progress2,&
						indwin,ncdep,iformText,autosim,ksim,&
		initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
						nrmodel(igraph2)=imodel
						imod0=imodel
						call gmSetGuiGridMode(GOFF)
						ipbar_Progress2=0
						icprev=0
						limits%xmax=160.
						limits%ymin=0
						limits%xmin=0.
						limits%ymax=20.
						

						!call gmActivateGraphicsFrame(ipbar_progress2)
						!call gFillRect(GSOLID,GRED,limits)
						!call gFlushGraphics
							call gmSetGuiGridMode(GOn)
						!	call gmSetWidgetStatus(eqfit(1), GSELECTABLE)

						!call gmSetProgressValue(ipbar_Progress2, 20)
3313						continue
						infoind=infoind+1
						imod=imodel
						itxtinfo=gmcreatetextentry(infopanel,1,infoind,infolength,1,&
						'Mechanisms file: '//qmec,32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						infoind=infoind+1
						call intconv(imod,cstring)
						itxtinfo=gmcreatetextentry(infopanel,1,infoind,infolength,1,&
						'Model nr= '//cstring(1:4)//models(igraph2)%title_model,32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
					!	 if(isetu.eq.1) call gmsetwidgetstatus(intoggle(1),gchecked)
					!	 if(isetu.eq.1) call gmsetwidgetstatus(intoggle(2),gchecked)

		case(1010)
			i=1
			irc=i
			mod_create=-irecq
			newmr=.false.
			newcons=.false.
			ixm=-100
			igraph2=igraph2+1
			call create_model(ixm,iym,igraph2,imodel,main,form1,form1_txtarray1,&
						graph1_2,GraphMainPanel1_2,graphics1_2,&
						nxmodel,nymodel,mtype,models,mod_create,irc,qmec,text7(i),&
						dxs,dys,ijmod,ka,kf,ic,indrat,pfilem,open7,apfile,ipbar_progress2,&
						indwin,ncdep,iformText,autosim,ksim,&
		initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
						nrmodel(igraph2)=imodel
						imod0=imodel
		case(1100) !35)	!NEW MODEL
		if(ihelpnew.eq.0) then
		call page_help(main,indmod,iphelp)
		ihelpnew=-1
		endif
		
		inipage=0
		ixm=2
		iym=4
		indk0=-1
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
			do k=1,25
			if(jopen2(k).eq.-1) then
			!	call gmEnqWidgetInfo(Graph1_2(k),widget)
			!	ivisual=widget%visual
			!	if(ivisual.eq.0) then
			!		jopen2(k)=-1
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
			igraph2=igraph2
				indmod=.false.
		!	imessy=gmDisplayMessageBox('','Independent ?',Gexclamation,gyesno)
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
			models(IGRAPH2)%ka=0
			models(igraph2)%kb=0
			models(igraph2)%n=0
			models(igraph2)%nsub=0
			models(igraph2)%kstat0=0
			models(igraph2)%kcon=0
			models(igraph2)%nlig=0
			models(igraph2)%model=jmodel
			models(igraph2)%title_model=' '
			iqtext(1)='Define a model with n independent subunits ?'
			nq=1
			icall=1101
			call question_box(main,iqbox,nq,iqtext,iqtoggle,icall)
	    case(1101)
			imessy=gmenqtoggleswitch(iqtoggle(1,1))
			call gmremovewindow(iqbox)
			if(imessy.eq.gon) then
				models(igraph2)%indmod=.true.
				indmod=.true.
				indmod=.true.
				textiq1(1)='Name '
				textiq1(2)='concerted states'
				textiq1(3)='Subunits'
				textiq1(4)='States in subunit'
				textiq1(5)='Number of ligands'
				nlm=5
				textiq1(20)='Independent Model'
				call text_table(Main,iqwin,textiq1(20),nlm,textiq1,iqt1,textiqt1,1110) !59)
			else
				models(igraph2)%indmod=.false.
				indmod=.false.
				textiq1(1)='Name '
			
				textiq1(2)='Open states'
				textiq1(3)='Shut states'
		!		textiq1(4)='Short-lived shut states'
				textiq1(4)='Number of ligands'
				nlm=4
				textiq1(20)='Model definition'
					call text_table(Main,iqwin,textiq1(20),nlm,textiq1,iqt1,textiqt1,1110) !38)
			endif
		

						
	case(1110) !38) !new model
	        nlm=4
			if(models(igraph2)%indmod.eq..true.) nlm=5
			do i=1,nlm
			call gmEnqTextSetting(iqt1(i),textiqt1(i))
		
			enddo
			call gmRemoveWindow(iqwin)	
			imodel=jmodel
			titlem=textiqt1(1)

			models(igraph2)%title_model=titlem
			if(indmod) then
			
			cmodel=textiqt1(2)
			call chtoint(cmodel,kcon)
			models(igraph2)%kcon=kcon
		
			cmodel=textiqt1(3)
			call chtoint(cmodel,nsub)
			models(igraph2)%nsub=nsub
		
			cmodel=textiqt1(4)
			call chtoint(cmodel,kstat0)
			models(igraph2)%kstat0=kstat0
			if(nsub.eq.0) nsub=1
			kstat=nsub*kstat0+kcon
			models(igraph2)%kstat=kstat
			!kstat1=kstat+kcon
			kcon=models(igraph2)%kcon
			nsub=models(igraph2)%nsub
			kstat0=models(igraph2)%kstat0
			kstat=models(igraph2)%kstat
			nxmodel=4*(kstat0+2)
			nymodel=8*nsub+4
			models(igraph2)%n=kcon+kstat0*kstat0
			models(igraph2)%ka=kcon
			kf=nsub*kstat0
			
			else
			cmodel=textiqt1(2)
			call chtoint(cmodel,kA)
			models(igraph2)%ka=ka
		
			cmodel=textiqt1(3)
			call chtoint(cmodel,kF)
			models(igraph2)%n=ka+kf
			do j=1,100
				models(igraph2)%statname(j)='          '
				models(igraph2)%dgamma(j)=0.
			enddo
			do j=1,ka
				models(igraph2)%dgamma(j)=50.
			enddo
			models(igraph2)%kstat=ka+kf
		!!	cmodel=textiqt1(4)
		!!	call chtoint(cmodel,kB)
			kb=0	
			models(igraph2)%kb=kb
		!	cmodel=textiqt1(5)
		!	call chtoint(cmodel,nchan)
			nchan=1
			models(igraph2)%nchan=nchan
			
		!	cmodel=textiqt1(6)
		!	call chtoreal(cmodel,vref)
			vref=-100
			models(igraph2)%vref=vref
			endif
			cmodel=textiqt1(4)
			call chtoint(cmodel,nlig)
			models(igraph2)%nlig=nlig
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
				textiq1(20)='Name for ligands'
				call text_table(Main,iqwin,textiq1(20),nlig,textiq1,iqt1,textiqt1,1111) !39)
			else
				if(models(igraph2)%indmod.eq..true.) then
					callid=1120 !59
				else
					callid=1130 !41
				endif
			
				goto 2
            endif
	  case(1111) !39) ! new model
		do i=1,nlig
			call gmEnqTextSetting(iqt1(i),textiqt1(i))
			ligname(i)=textiqt1(i)
			
			models(igraph2)%ligname(i)=ligname(i)
			do j=1,models(igraph2)%n
				models(igraph2)%nbound(j,i)=0
			enddo
			
		enddo
		call gmRemoveWindow(iqwin)	
		if(indmod) then
			callid=1120 !59
		else
			callid=1130 !41
		endif
		goto 2
   
	case(1120) !59) ! inmod new model
			if(kcon.gt.0) then
			indwin1 = gmCreateComplexDialogueBox(Main, 5, 5,8,kcon+3, GALL, ' ', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
			
				iconPanel1=gmCreatePanel(indwin1, 0, 0,8,kcon+1, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmtitle='Concerted States',gmExpand=GOff, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
	    
			do i=1,kcon
					models(igraph2)%con_states(i)=' '
					jcons(i) = gmCreateTextEntry(iconPanel1,1, i, 6, 1,models(igraph2)%con_states(i), 60, Gedit, &
              		gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              		gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              		gmVpos=GTOP, gmExpand=GOFF)
			enddo
				
			
			
			
			iqb1=gmCreatePushButton(indwin1,3,0, 2, 1,'OK',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=1121) !40)
				call gmdrawwindow(indwin1)
			else
				callid=1121 !40
			goto 2
			endif
	case(1121 ) ! new model!40
	    if (kcon.gt.0) then
		do i=1,kcon
			call gmEnqTextSetting(jcons(i),models(igraph2)%con_states(i))
			statname(i)=models(igraph2)%con_states(i)
		enddo
		endif
		call ind_model(Main,igraph2,models,isub)
	case(1122) !6505) 	   
	
	if(nsub.gt.0) then
		do i=1,nsub
		do j=1,kstat0
		call gmEnqTextSetting(isub(i,j),models(igraph2)%sub_states(i,j))
		statname(kcon+j+kstat0*(i-1))=models(igraph2)%sub_states(i,j)
		enddo
	    enddo
	endif
	l=1+kcon
	do i=1,kstat0
		do j=1,kstat0
				cnum5=models(igraph2)%sub_states(1,i)
				cnum51=models(igraph2)%sub_states(2,j)
				n1=len_trim(cnum5)
				n2=len_trim(cnum51)
				n1=n1-1
				n2=n2-1
				if(nsub.gt.0) then
				models(igraph2)%statname(l)=cnum5(1:n1)//cnum51(1:n2)
				else
				models(igraph2)%statname(l)='         '
				endif
				l=l+1
		enddo
	enddo
	if(nsub.eq.0) then
		do l=1+kcon,models(igraph2)%n
			models(igraph2)%statname(l)='         '
		enddo
	endif
	callid=1130 !41
	goto 2
  
 
	case(1130) ! new model
		lmodel=jmodel
		
		nrmodel(igraph2)=jmodel
		
		models(igraph2)%model=jmodel
		if(models(igraph2)%indmod.eq..true.) then
		  kmodel=lmodel
		  igraph0=igraph2
		  do i=1,models(igraph0)%nsub
			igraph2=igraph0+i
		    imodel=kmodel+i
			jmodel=imodel
			nrmodel(igraph2)=jmodel
			models(igraph2)%indmod=.false.
			call intconv(i,cnum5)
			models(igraph2)%title_model='Subunit: '//cnum5
		
			models(igraph2)%n=models(igraph0)%kstat0
			models(igraph2)%ka=0
			models(igraph2)%kb=0
			models(igraph2)%nsub=0
			models(igraph2)%kcon=0
			models(igraph2)%kstat0=0
			kf=models(igraph2)%n
				models(igraph2)%nlig=models(igraph0)%nlig
				do j=1,models(igraph2)%nlig
				models(igraph2)%ligname(j)=models(igraph0)%ligname(j)
				enddo
			ka=0
			if(models(igraph2)%n.le.16) then
				nxmodel=20
				nymodel=models(igraph2)%n+4
			else
				nxmodel=36
				nymodel=0.5*(models(igraph2)%n)+4
			endif
			
			ixm=ixm+1
			iym=iym+1
			mod_create=0
			do j=1,models(igraph0)%kstat0
				models(igraph2)%statname(j)=models(igraph0)%sub_states(i,j)
				call intconv(j,cnum5)
				models(igraph2)%name(j)=cnum5(1:3)
			enddo
			!igraph2=imodel-300	
			!igraph2=igraph2
			
			readrec=.true.
			!modplot=imodel
			ifiltype=10
			lframe=graphics1_2(igraph2)
			nrmodel(igraph2)=imodel
				newmr=.false.
						newcons=.false.
			call create_model(ixm,iym,igraph2,imodel,main,form1,form1_txtarray1,&
			graph1_2,GraphMainPanel1_2,graphics1_2,&
			nxmodel,nymodel,mtype,models,mod_create,0,qmec,	models(igraph2)%title_model,dxs,dys,ijmod,kA,kF,&
			ic,indrat,pfilem,open7,apfile,ipbar_progress2,indwin,ncdep,iformText,autosim,ksim,&
		initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
			models(igraph2)%model=imodel
			imod0=imodel
			if(mod_create.ne.1) call gmSetWidgetStatus(ibutton1, GunSELECTABLE)
		 enddo
	!	 call page_help(main,indmod,iphelp)
		else
		do i=1,100
			
			models(igraph2)%statname(i)=' '
			call intconv(i,cnum1)
			models(igraph2)%name(i)=cnum1(1:3)
			text_box(i,1)=cnum1
		
		enddo
		title_box(1)='State'
		title_box(2)='Name'
		!call table_box(main,models(igraph2)%n,2,title_box,text_box,itext_box,3513)
		
			if(models(igraph2)%n.gt.48) nxmodel=42	
	
			readrec=.true.
			!modplot=imodel
			ifiltype=10
			lframe=graphics1_2(igraph2)
			nrmodel(igraph2)=imodel
				newmr=.false.
						newcons=.false.
		call create_model(ixm,iym,igraph2,imodel,main,form1,form1_txtarray1,&
			graph1_2,GraphMainPanel1_2,graphics1_2,&
			nxmodel,nymodel,mtype,models,mod_create,0,qmec,models(igraph2)%title_model,dxs,dys,ijmod,kA,kF,&
			ic,indrat,pfilem,open7,apfile,ipbar_progress2,indwin,ncdep,iformText,autosim,ksim,&
		initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
			if(mod_create.eq.0) call gmSetWidgetStatus(ibutton1, GunSELECTABLE)
		infoind=infoind+1
		imod=imodel
		imod0=imodel
						itxtinfo=gmcreatetextentry(infopanel,1,infoind,infolength,1,&
						'Mechanisms file: '//qmec,32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						infoind=infoind+1
						call intconv(imod,cstring)
						itxtinfo=gmcreatetextentry(infopanel,1,infoind,infolength,1,&
						'Model nr= '//cstring(1:4)//models(igraph2)%title_model,32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
					!	call gmsetwidgetstatus(intoggle(1),gchecked)
	!	call page_help(main,indmod,iphelp)
		endif	
			
		do ijs=4,15
			call gmSetWidgetStatus(modelw(ijs), GSELECTABLE)
	
		enddo
							
		! independent model			
		!	call gmSetWidgetStatus(eqfit(1), GSELECTABLE)			
		case(1150 )!317
			if(igraph0.gt.0) then
			if(models(igraph0)%indmod.eq..true.) then
			do i=1,models(igraph0)%nsub
			call gmremovewindow(graph1_2(igraph0+i))
!			call gmremovewindow(graph1_2(igraph0+i))
			enddo
			imodel=models(igraph0)%model
		
			if(models(igraph0)%n.le.64) then
				newmr=.false.
						newcons=.false.
			call create_model(ixm,iym,igraph0,imodel,main,form1,form1_txtarray1,&
			graph1_2,GraphMainPanel1_2,graphics1_2,&
			nxmodel,nymodel,mtype,models,mod_create,0,qmec,titlem,dxs,dys,ijmod,kA,kF,&
			ic,indrat,pfilem,open7,apfile,ipbar_progress2,indwin,ncdep,iformText,autosim,ksim,&
		initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
			infoind=infoind+1
			imod=imodel
			imod0=imodel
						itxtinfo=gmcreatetextentry(infopanel,1,infoind,infolength,1,&
						'Mechanisms file: '//qmec,32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						infoind=infoind+1
						call intconv(imod,cstring)
						itxtinfo=gmcreatetextentry(infopanel,1,infoind,infolength,1,&
						'Model nr= '//cstring(1:4)//models(igraph0)%title_model,32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						 if(isetu.eq.1) call gmsetwidgetstatus(intoggle(1),gchecked)
						 if(isetu.eq.1) call gmsetwidgetstatus(intoggle(2),gchecked)
			!call gmSetWidgetStatus(eqfit(1), GSELECTABLE)
			else
			endif
			endif	
			endif
	case(1161:1165)
 
		if(callid.eq.1161 ) then  !314!open state
		if(igraph2.gt.0)	call gmSetGuiCursor(lframe,Gdefault,GDEFAULT)
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
		else if(callid.eq.1162 ) then   !315!close state
			if(igraph2.gt.0)	call gmSetGuiCursor(lframe,Gdefault,GDEFAULT)
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
		else if(callid.eq.1163 ) then   !316!link
		if(igraph2.gt.0) then
		!	imessy=gmDisplayMessageBox('','Did you enter all the states',gquestion,Gyesno)
		!	if(imessy.eq.gnobutton) goto 1 
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
		 
		 endif
		 else if(callid.eq.1164 ) then   !move
		 if (igraph2.gt.0) then
			
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
		
		endif
		else if(callid.eq.1165 ) then   !delete
		if(igraph2.gt.0) then
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
	   endif
	
case(1200 ) ! display old model	!36
		!	call help_modify(main,imodhelp)
			do i=1,500
				irecfin(i)=-1
			enddo
		    readmec=.true.
			icallprevm=-1
			gititle='Browse for mechanism (.mec) file'
			if(autosim.and.ksim.eq.2) inipage=0
			if(autosim.and.inipage.ge.-200) then
			if(readini) then
			
			    if(ksim.eq.0.or.ksim.eq.-2) then
			    readmec=.true.
			    gititle='Browse for the true reaction mechanism to be simulated'
		!	    ksim=-1
			    else
			    gititle='DEFINE THE MECHANISM TO BE FITTED'
		!	    ksim=0
			    endif
			else
			    if(ksim.eq.-1) then
			    gititle='Browse for the true reaction mechanism to be simulated'
		!	    ksim=-1
			    else
			    gititle='DEFINE THE MECHANISM TO BE FITTED'
		!	    ksim=0
			    endif
			endif
			endif
			!inipage=0
			call read_model(Main,Records,List7_1,Button7,TEXT7,INdX,qmec,ijmod,rtitles,&
			nrecs,gititle,ifiltype)
			ifiltype=10
			mtype=1
			do i=1,200
				modopen(i)=-1
				modify(i)=.false.
			enddo
			icallprevm=callid

    case(1209)
	CASE(1201,1202 ) ! old model !37,42
		if(callid.eq.1202) call help_modify(main,imodhelp)
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
	!	 ifiltype=10
		 if(ifiltype.EQ.10) then
			    call gmEnqListStatus(List7_1,nentry,nselect,nfirst)
				
				if(nentry.gt.0) then
				    jselect=-1
		!			i=i+1
					do i=1,nentry
						if(irecfin(i).eq.-100) goto 333
					    kf=0
						istat(i)=gmEnqListEntry(List7_1,i,TEXT7(i))
						if(istat(i).eq.2) then
						if(jselect.eq.-1) then
							do ijs=4,15
									call gmSetWidgetStatus(modelw(ijs), GSELECTABLE)
	
							enddo
							jselect=0
						endif
						if(callid.eq.1202) then
							Cnum0=TEXT7(i)(1:3)
							CALL CHTOINT(Cnum0,IMODEL)
							mod_create=2
							cnum=text7(i)(1:3)
							call chtoint(cnum,imod)
							readrec=.true.
							irc=i
							newmr=.false.
							newcons=.false.
						
						ixm=ixm+1
						iym=iym+1
						do k=1,25
							if(jopen2(k).eq.-1) then
					
								igraph2=k
								goto 339
							endif
						    enddo
						
						igraph2=igraph2+1
339						call create_model(ixm,iym,igraph2,imodel,main,form1,form1_txtarray1,&
						graph1_2,GraphMainPanel1_2,graphics1_2,&
						nxmodel,nymodel,mtype,models,mod_create,irc,qmec,text7(i),&
						dxs,dys,ijmod,ka,kf,ic,indrat,pfilem,open7,apfile,ipbar_progress2,&
						indwin,ncdep,iformtext,autosim,ksim,&
		initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
					imod0=imodel
						imod=imodel
						infoind=infoind+1
						itxtinfo=gmcreatetextentry(infopanel,1,infoind,infolength,1,&
						'Mechanisms file: '//qmec,32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						infoind=infoind+1
						call intconv(imod,cstring)
						itxtinfo=gmcreatetextentry(infopanel,1,infoind,infolength,1,&
						'Model nr= '//cstring(1:4)//models(igraph2)%title_model,32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						 if(isetu.eq.1) call gmsetwidgetstatus(intoggle(1),gchecked)
						 if(isetu.eq.1) call gmsetwidgetstatus(intoggle(2),gchecked)
						jmodel=jmodel+1
						newmodel=jmodel
						igraph2=igraph2+1
						jopen2(igraph2)=1

						jgraph2=igraph2
						indkn=jmodel
						models(igraph2)%model=newmodel
						nrmodel(igraph2)=newmodel
						call copy_model(models,imodel,newmodel,igraph2-1,igraph2,ratcons,indk,indkn)
						
						mod_create=0
						call draw_model(igraph2,newmodel,main,graph1_2,GraphMainPanel1_2,graphics1_2,&
						models,.false.,0,0,initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
						if(callid.eq.1202) modify(imodel)=.true.
						else
						if(modopen(i).ne.2) then
							Cnum0=TEXT7(i)(1:3)
							CALL CHTOINT(Cnum0,IMODEL)
							mod_create=1
							modopen(i)=2
						    
						    do k=1,25
							if(jopen2(k).eq.-1) then
						!	call gmEnqWidgetInfo(Graph1_2(k),widget)
						!	ivisual=widget%visual
						!	if(ivisual.eq.0) then
						!		jopen2(k)=-1
								igraph2=k
								goto 334
							endif
						    enddo
						
						if(inipage.ge.0) igraph2=igraph2+1
						ixm=ixm+1
						iym=iym+1
						if(igraph2.gt.25) then
								imessy=gmDisplayMessageBox('','Maximum 25 models on screen',Gexclamation,gok)
								igraph2=igraph2-1
								goto 1
						endif
						ngraph2=igraph2
						

334						jopen2(igraph2)=1
						igraph2=igraph2
						lframe=graphics1_2(igraph2)
							readrec=.true.
						!modplot=imodel
						cnum=text7(i)(1:3)
		                call chtoint(cnum,imod)
						
						irc=i
							newmr=.false.
						newcons=.false.
					
						lt=0
						do k=1,nrecs
						
							if(ijmod(k).eq.imod) then
								irecq=k
								lt=lt+1
								ratcons(k)%imod=imod
								!text_tog(lt)=rtitles(k)
								call intconv(k,cnum5)
								radio_text(lt)=cnum5(1:3)//':'//rtitles(k)
								indrec(lt)=k
								indrat=indrec(lt)
	 
								irecq=indrat
							endif
						enddo
						iarca=0
						if(lt.gt.1) then
						iarca=-1
						nmod=imodel
						if(nmod.eq.1.and.npar.eq.10) dcmodel=.true.
						if(nmod.eq.14.and.npar.eq.4) dcmodel=.true.
	if(nmod.eq.9.and.npar.eq.6) dcmodel=.true.
	if(nmod.eq.10.and.npar.eq.8) dcmodel=.true.
	if(nmod.eq.11.and.npar.eq.14) dcmodel=.true.
	if(nmod.eq.29.and.npar.eq.14) dcmodel=.true.
	if(nmod.eq.33.and.npar.eq.20) dcmodel=.true.
	if(nmod.eq.34.and.npar.eq.12) dcmodel=.true.
	if(nmod.eq.35.and.npar.eq.16) dcmodel=.true.
	if(nmod.eq.36.and.npar.eq.16) dcmodel=.true.
	if(nmod.eq.37.and.npar.eq.18) dcmodel=.true.
	if(nmod.eq.38.and.npar.eq.18) dcmodel=.true.
	if(nmod.eq.39.and.npar.eq.18) dcmodel=.true.
	if(nmod.eq.40.and.npar.eq.20) dcmodel=.true.
	if(nmod.eq.41.and.npar.eq.8) dcmodel=.true.
	dcmod=dcmodel
					!	call toggle_panel(Main,ITOGGLE,itogglepanel,lt,text_tog,intoggle,1500,valdat,idat,itogbutton)
					   	titlerp='Titles for stored rate constants'
					   nradio=lt
					   call radio_panel(main,iradio,lt,iradio_toggle,radio_text,-1,&
						iradiox,radiox,iradioy,radioy,1500,titlerp)
				
						goto 1
					
						else
							if(inipage.eq.-200.or.inipage.eq.-300) then
							callid=-22
							inipage=0
							neq=0
							fixec50=.false.
							nmr=0
							nfixec50=0
							nfix=0
							do jk=1,200
							jfix(jk)=0
							enddo
						
							call gmremovewindow(records)
							goto 1
							endif
							
						endif
						jgraph=igraph2
						mod_create=-irecq
						iswicon0=iswicon
						iswicon=ksim
						call create_model(ixm,iym,igraph2,imodel,main,form1,form1_txtarray1,&
						graph1_2,GraphMainPanel1_2,graphics1_2,&
						nxmodel,nymodel,mtype,models,mod_create,irc,qmec,text7(i),&
						dxs,dys,ijmod,ka,kf,ic,indrat,pfilem,open7,apfile,ipbar_progress2,&
						indwin,ncdep,iformtext,autosim,ksim,&
		initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
						nrmodel(igraph2)=imodel
					imod0=imodel
					    iswicon=iswicon0
						if(lt.eq.1) then
						indk=indrat
						irecm0=irecq
						jgraph=igraph2
						irecfin(irc)=-100
						if(autosim.and.ksim.eq.-1) irect=irecq
						call read_rates(Main,form1,Form1_TxtArray1,irecm0,imod,qmec,nrows,indk,&
						ratcons,models,ic,ipbar_Progress2,jgraph,nmr,conc,iformText)
						endif
						infoind=infoind+1
						imod=imodel
						itxtinfo=gmcreatetextentry(infopanel,1,infoind,infolength,1,&
						'Mechanisms file: '//qmec,32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						infoind=infoind+1
						call intconv(imod,cstring)
						itxtinfo=gmcreatetextentry(infopanel,1,infoind,infolength,1,&
						'Model nr= '//cstring(1:4)//models(igraph2)%title_model,32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						if(isetu.eq.1) call gmsetwidgetstatus(intoggle(1),gchecked)
						if(isetu.eq.1) call gmsetwidgetstatus(intoggle(2),gchecked)
						continue
						endif
						endif
						endif
333					continue
					ENDDO
				
				!	call gmSetWidgetStatus(eqfit(1), GSELECTABLE)
				ENDIF
				call gmremovewindow(records)
			ENDIF
	 	
		! States
	!=================================================================

	case(1300) ! read states table
		if(igraph2.gt.0) then
			callid=igraph2+1300
		goto 2
		else
		imessy=gmDisplayMessageBox('','No model on screen',Gexclamation,gok)
		
		endif	  	
case(1301:1325)	!state properties	2101:2600
		!	IND_1=nrmodel(igraph2)
		igraph2=callid-1300
		imodel=models(igraph2)%model
		link=.false.
		ncols=4
		nrows=ka+kf
		nrows=models(igraph2)%n
		NLIG=models(igraph2)%nLIG
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
		!models(igraph2)%indmod=.false.
		limits%xmax=80.
						limits%ymin=0
						limits%xmin=0.
						limits%ymax=20.
							call gmSetGuiGridMode(GOFF)
				!		call gmActivateGraphicsFrame(ipbar_progress2)
				!		call gFillRect(GSOLID,GRED,limits)
				!		call gFlushGraphics
							call gmSetGuiGridMode(GOn)
		!call gmsetprogressvalue(ipbar_Progress2,10)
		 if(isetu.eq.1) call gmsetwidgetstatus(intoggle(2),gchecked)
		if(models(igraph2)%indmod.eq..true.) then
!DC three_d not defined here so uncomment line in next loop that defines it
		do i=1,models(igraph2)%n
!			if(models(igraph2)%inter_link(i).gt.2) three_d=.true.
			if(models(igraph2)%inter_link(i).gt.2) three_d=.true.
		enddo
		if(three_d) then
		else	
		do i=1,kcon
			models(igraph2)%statname(i)=models(igraph2)%con_states(i)
		enddo
			l=1+kcon
			do i=1,kstat0
				do j=1,kstat0
				cnum5=models(igraph2)%sub_states(1,i)
				cnum51=models(igraph2)%sub_states(2,j)
				n1=len_trim(cnum5)
				n2=len_trim(cnum51)
				!n1=n1-1
				!n2=n2-1
				if(nsub.gt.0) then
				models(igraph2)%statname(l)=cnum5(1:n1)//'-'//cnum51(1:n2)
				else
				models(igraph2)%statname(l)='         '
				endif
				l=l+1
				enddo
			enddo
			models(igraph2)%n=models(igraph2)%kcon+models(igraph2)%kstat0*models(igraph2)%kstat0
			nrows=models(igraph2)%n
			callid=1330 !6506
			goto 2	
	   endif
	   else
	   do i=1,nrows
		!	models(igraph2)%statname(i)=' '	
		enddo
	   callid=1330 !6506
	   goto 2
	   endif
	
	
	case(1330)
	    if(	models(igraph2)%nlig.ge.1) then
			ncols=5
			if(models(igraph2)%ligname(1).eq.'') models(igraph2)%ligname(1)='lig 1'
		    if(models(igraph2)%ligname(2).eq.'') models(igraph2)%ligname(2)='lig 2'
			ligname(1)=models(igraph2)%ligname(1)
			ligname(2)=models(igraph2)%ligname(2)
			n1=len_trim(ligname(1))
			n2=len_trim(ligname(2))	
			if(models(igraph2)%nlig.eq.1) then
				xtext='State name '//char(124)//'Conductance'//char(124)//'Nr links'//&
				char(124)//'Link to'//char(124)//ligname(1)(1:n1)
			else 
				xtext='State name '//char(124)//'Conductance'//char(124)//'Nr links'//&
				char(124)//'Link to'//char(124)//ligname(1)(1:n1)//&
				char(124)//ligname(2)(1:n2)
				ncols=6
			endif
		else
			ncols=4
			xtext='State name '//char(124)//'Conductance'//char(124)//'Nr links'//&
			char(124)//'Link to'
	
		endif
		nrows=models(igraph2)%n		
		
		   j=1
			do k=1,models(igraph2)%n
				do l=k+1,models(igraph2)%n
				if(models(igraph2)%link(k,l).eq.1) then
				ic(1,j)=k
				ic(2,j)=l
				j=j+1
				endif
				enddo
			enddo
			
			
			NCON=(J-1)
			do m=1,ncon
				models(igraph2)%ic(1,m)=ic(1,m)
				models(igraph2)%ic(2,m)=ic(2,m)
	
			enddo
		
			models(igraph2)%ncon=ncon

			npar=2*ncon
			nsetq=0
			nrateq=npar
			models(igraph2)%npar=npar
			
			if(models(igraph2)%indmod) then
			  nrateq=2*ncon
			  k=1
			  kstat0=models(igraph2)%kstat0
			  kstat=models(igraph2)%kstat
			  kcon=models(igraph2)%kcon
			  npar0=2*(models(igraph2)%kstat0-1)
			  npar=2*models(igraph2)%kcon+models(igraph2)%nsub*npar0
			  !nsetq=2*npar
			  nsetq=nrateq-npar
			  models(igraph2)%npar=npar
			  m=1
			  
			  do i=1,kstat0-1
					models(igraph2)%ic(1,m)=models(igraph2)%n-kstat0*i+1
				    models(igraph2)%ic(2,m)=models(igraph2)%n-kstat0*i+1-kstat0
					m=m+1
					
			  enddo
			  do i=1,kstat0-1
					models(igraph2)%ic(1,m)=kcon+kstat0+1-i
					models(igraph2)%ic(2,m)=kcon+kstat0-i
					m=m+1
					
			  enddo
			  do i=1,kcon
				do j=kcon+1,models(igraph2)%n
				if (models(igraph2)%link(i,j).eq.1) then
					models(igraph2)%ic(1,m)=i
					models(igraph2)%ic(2,m)=j
					m=m+1
				endif
				enddo
			 enddo

			endif
		
		call text_array(igraph2,Main,form1,Form1_TxtArray1,ncols,nrows,iwcell,ihcell,xtext,isw,&
        models(igraph2)%name_link,models(igraph2)%inter_link,models(igraph2)%statname,&
		models(igraph2)%dgamma,models(igraph2)%title_model,tij,models(igraph2)%NBOUND,&
		models(igraph2)%ligname,models(igraph2)%nlig,ratcons(indrat)%micro,&
		ratcons(indrat)%ligant,jfix,theta0,iformText)
		istatab(imodel)=9
		
		if(imodel.lt.300.and.igraph2.ne.25) then
		imees=gmdisplaymessagebox('','Display rates table now',ginformation,gok)
		if(imees.eq.gyesbutton) then
			callid=1400+igraph2
		goto 2
		endif
		endif
	
	case(2060)
	 !   call gmActivateGraphicsFrame(graphics1_2(igraph2))
	 if(icp.eq.25) then

		do j=1,	models(igraph2)%n	
		call gmEnqCellSetting(Form1_TxtArray1(igraph2,1), 1,j,rval,models(igraph0)%statname(j))
		call gmEnqCellSetting(Form1_TxtArray1(igraph2,1), 2,j,models(igraph0)%dgamma(j),actext)
		call gmEnqCellSetting(Form1_TxtArray1(igraph2,1), 4,j,rval,models(igraph0)%name_link(j))
		call gmEnqCellSetting(Form1_TxtArray1(igraph2,1), 3,j,models(igraph0)%inter_link(j),actext)
		call gmEnqCellSetting(Form1_TxtArray1(igraph2,1), 5,j,models(igraph0)%nbound(j,1),actext)

		!!! nbound?
		
		
		enddo 
		
	
		call gmremovewindow(Form1(igraph2,1))
	icp=0
	endif
			call gmremovewindow(Graph1_2(igraph2))
		igraph2=igraph0
		call gmActivateGraphicsFrame(graphics1_2(igraph2))
		do j=1,models(igraph2)%n
			
			statname(j)=models(igraph2)%statname(j)
			
			do l=1,10
		!		if(models(igraph2)%statname(j)(l:l).eq.'*') models(igraph2)%statname(j)(l:l)='x'
			enddo
			call lincol(models(igraph2)%colour(j))
		    call jSYMBOL(models(igraph2)%X(j),models(igraph2)%y(j),-3,1.2,1.2,13,idev)
				call write_string(models(igraph2)%name(j),models(igraph2)%X(j)-0.1,&
		    models(igraph2)%y(j)+0.4,0.,0,1,0.35,48,dxs,dys)
	
			
			call write_string(models(igraph2)%statname(j)(1:6),&
			models(igraph2)%X(j)-0.1,&
			models(igraph2)%y(j)-0.2,0.,0,1,0.35,48,0.6,0.8)
			call write_string(models(igraph2)%statname(j)(7:12),&
			models(igraph2)%X(j)-0.1,&
			models(igraph2)%y(j)-0.7,0.,0,1,0.35,48,0.6,0.8)
		enddo
	    

	case(1351:1375) !5001:5500! save states
		
		igraph2=callid-1350
		imod=models(igraph2)%model
		icp=igraph2
		do j=1,	models(igraph2)%n	
		call gmEnqCellSetting(Form1_TxtArray1(igraph2,1), 1,j,rval,models(igraph2)%statname(j))
		call gmEnqCellSetting(Form1_TxtArray1(igraph2,1), 2,j,models(igraph2)%dgamma(j),actext)
		call gmEnqCellSetting(Form1_TxtArray1(igraph2,1), 4,j,rval,models(igraph2)%name_link(j))
		call gmEnqCellSetting(Form1_TxtArray1(igraph2,1), 3,j,models(igraph2)%inter_link(j),actext)
		!!! nbound?
			call gmEnqCellSetting(Form1_TxtArray1(igraph2,1), 5,j,models(igraph2)%nbound(j,1),actext)

		
		enddo 
		
		call gmActivateGraphicsFrame(graphics1_2(igraph2))
		if(models(igraph2)%indmod) then
		    kstat=models(igraph2)%kstat
			kcon=models(igraph2)%kcon
			nsub=models(igraph2)%nsub
			kstat0=models(igraph2)%kstat0
			do j=1,models(igraph2)%kcon
				statname(j)=models(igraph2)%con_states(j)
			enddo
			do i=1,models(igraph2)%nsub
	
			do j=1,models(igraph2)%kstat0
				statname(kcon+j+kstat0*(i-1))=models(igraph2)%sub_states(i,j)
			enddo
			enddo
			if(nsub.eq.0)  then
				i=1
				do j=1,kstat-kcon
					statname(kcon+j)=models(igraph2)%sub_states(i,j)
				enddo
			endif
			do j=1,models(igraph2)%kcon	
			do l=1,10
		!		if(models(igraph2)%statname(j)(l:l).eq.'*') models(igraph2)%statname(j)(l:l)='x'
			enddo
			call write_string(models(igraph2)%statname(j)(1:6),&
			models(igraph2)%X(j)-0.1,&
			models(igraph2)%y(j)-0.2,0.,0,1,0.35,48,0.6,0.8)
			call write_string(models(igraph2)%statname(j)(7:12),&
			models(igraph2)%X(j)-0.1,&
			models(igraph2)%y(j)-0.7,0.,0,1,0.35,48,0.6,0.8)
		
			enddo	
			l=1+models(igraph2)%kcon
		
			do j=l,models(igraph2)%n
			do lk=1,10
		
			if(models(igraph2)%statname(j)(lk:lk).eq.char(45)) then
			i1=lk
			endif
			enddo
			if(i1.ne.0) then
			call write_string(models(igraph2)%statname(j)(1:i1-1),models(igraph2)%X(j)-0.1,&
			models(igraph2)%y(j)-0.2,0.,0,1,0.35,48,0.6,0.8)	
			call write_string(models(igraph2)%statname(j)(i1+1:i1+4),models(igraph2)%X(j)-0.1,&
			models(igraph2)%y(j)-0.7,0.,0,1,0.35,48,0.6,0.8)
			else
			call write_string(models(igraph2)%statname(j)(1:6),models(igraph2)%X(j)-0.1,&
			models(igraph2)%y(j)-0.2,0.,0,1,0.35,48,0.6,0.8)	
			endif
			
			enddo	
		else
		do j=1,models(igraph2)%n
			
			statname(j)=models(igraph2)%statname(j)
			
			do l=1,10
		!		if(models(igraph2)%statname(j)(l:l).eq.'*') models(igraph2)%statname(j)(l:l)='x'
			enddo
			call lincol(models(igraph2)%colour(j))
		    call jSYMBOL(models(igraph2)%X(j),models(igraph2)%y(j),-3,1.2,1.2,13,idev)
				call write_string(models(igraph2)%name(j),models(igraph2)%X(j)-0.1,&
		models(igraph2)%y(j)+0.4,0.,0,1,0.35,48,dxs,dys)
	
			
			call write_string(models(igraph2)%statname(j)(1:6),&
			models(igraph2)%X(j)-0.1,&
			models(igraph2)%y(j)-0.2,0.,0,1,0.35,48,0.6,0.8)
			call write_string(models(igraph2)%statname(j)(7:12),&
			models(igraph2)%X(j)-0.1,&
			models(igraph2)%y(j)-0.7,0.,0,1,0.35,48,0.6,0.8)
		enddo
		
		endif
		
        	
		if(models(igraph2)%nlig.gt.0) then
		k=1
		do 	i=5,4+models(igraph2)%nlig
			do j=1,models(igraph2)%n
				call gmEnqCellSetting(Form1_TxtArray1(igraph2,1), I,j ,&
				models(igraph2)%nbound(j,k),actext)
			enddo
			K=K+1
		enddo
		endif
		call gFlushGraphics()

         case(1400 )!3502

	if(igraph2.gt.0) then ! read rates (icon)
		callid=igraph2+1400
		goto 2
		else
		imessy=gmDisplayMessageBox('','No model on screen',Gexclamation,gok)
		
		endif
	
	
	
	
	case(1426)
		call gmremovewindow(iradio)
	case(1401:1425) ! rate	2601:3100
	  link=.false.
	  igraph2=callid-1400
	  imod=models(igraph2)%model
	  			    
	  imodel=imod
	 
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
		!	text_tog(lt)=rtitles(k)
		call intconv(k,cnum5)
				radio_text(lt)=cnum5(1:3)//':'//rtitles(k)
			indrec(lt)=k
			indrec(lt)=k
								indrat=indrec(lt)
	 
								irecq=indrat
		endif
		enddo
		
		if(lt.gt.0) then
		do ml=1,lt
			if(indrat.eq.indrec(ml)) indratprev=ml
		enddo
	titlerp='Titles for stored rate constants'
		!	call toggle_panel(Main,ITOGGLE,itogglepanel,lt,text_tog,intoggle,1500,valdat,idat,itogbutton)
		nradio=lt
		call radio_panel(main,iradio,lt,iradio_toggle,radio_text,-1,&
		iradiox,radiox,iradioy,radioy,1500,titlerp)
		else
		
		
			irecm0=irecq
			indrat=irecm0
			ratcons(indrat)%imod=imod
			indrec(1)=indrat
			
			call read_rates(Main,form1,Form1_TxtArray1,irecm0,imod,qmec,nrows,indrat,&
			ratcons,models,ic,ipbar_progress2,igraph2,nmr,conc,iformText)

		endif
	  else
			imessy=gmDisplayMessageBox('','Did you fill in the states properties table first ?',Gquestion,gyesno)
			if(imessy.eq.gyesbutton) then
		!	indr=indr+1
		!	indrat=indr	
	       ! ratcons(indrat)%imod=joldmodel
			ncols=5
			nrows=0
			nrows=2*models(igraph2)%n
			
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
			do k=1,models(igraph2)%n
				do l=k+1,models(igraph2)%n
				if(models(igraph2)%link(k,l).eq.1) then
				ic(1,j)=k
				ic(2,j)=l
				j=j+1
				endif
				enddo
			enddo
			
			
			NCON=(J-1)
			do m=1,ncon
				models(igraph2)%ic(1,m)=ic(1,m)
				models(igraph2)%ic(2,m)=ic(2,m)
	
			enddo
			npar=2*ncon
			models(igraph2)%ncon=ncon
			nrateq=2*ncon
				models(igraph2)%npar=npar
			!nsetq=2*npar
			ratcons(indrat)%nsetq=0
			nsetq=0
			NROWS=Npar
			k=1
			do m=1,ncon
				i=ic(1,m)
				j=ic(2,m)
				call intconv(i,cnum(1))
				call intconv(j,cnum1)
				nl=len_trim(cnum(1))
				nl1=len_trim(cnum1)
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
			ratcons(indrat)%titlep(m)=''
			ratcons(indrat)%value(m)=0.
			enddo
			!if modified:
			if(indk0.gt.0) then
			do m=1,npar
				do nn=1,models(igraph2-1)%npar
				if(	ratcons(indrat)%qij(m).eq.ratcons(indk0)%qij(nn) )then
				ratcons(indrat)%titlep(m)=ratcons(indk0)%titlep(nn)
				ratcons(indrat)%value(m)=ratcons(indk0)%value(nn)
					ratcons(indrat)%iconc(m)=ratcons(indk0)%iconc(nn)
					ratcons(indrat)%ligant(m)=ratcons(indk0)%ligant(nn)
				endif
				enddo
			enddo
			endif
			if(models(igraph2)%indmod.and..not.three_d) then
			  k=1
			  kstat0=models(igraph2)%kstat0
			  kstat=models(igraph2)%kstat
			  kcon=models(igraph2)%kcon
			  npar0=2*(models(igraph2)%kstat0-1)
			  npar=2*models(igraph2)%kcon+models(igraph2)%nsub*npar0
			  nrateq=2*ncon
			  nsetq=nrateq-npar
			  ratcons(indrat)%nsetq=nsetq
			 
			  nrows=npar
			  models(igraph2)%npar=npar
			  m=1
			  ij=1
			  ik=(kstat0-1)*2
			  do i=1,kstat0-1
					ic(1,m)=models(igraph2)%n-kstat0*i+1
				    ic(2,m)=models(igraph2)%n-kstat0*i+1-kstat0
				
					call intconv(models(igraph2)%n-kstat0*i+1,cnum(1))
					call intconv(models(igraph2)%n-kstat0*i+1-kstat0,cnum1)
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
						ic(1,2*(kstat0-1)*j+kcon+m)=models(igraph2)%n-kstat0*i+1+j
						ic(2,2*(kstat0-1)*j+kcon+m)=models(igraph2)%n-kstat0*i+1-kstat0+j
						ratcons(indrat)%ieq(j+(i-1)*nsub*(kstat0-1))=models(igraph2)%n-kstat0*i+1+j
						ratcons(indrat)%jeq(j+(i-1)*nsub*(kstat0-1))=models(igraph2)%n-kstat0*i+1-kstat0+j
						ratcons(indrat)%ieq(j+(i-1)*nsub*(kstat0-1)+kstat0-1)=models(igraph2)%n-kstat0*i+1-kstat0+j
						ratcons(indrat)%jeq(j+(i-1)*nsub*(kstat0-1)+kstat0-1)=models(igraph2)%n-kstat0*i+1+j
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
				do j=kcon+1,models(igraph2)%n
				if (models(igraph2)%link(i,j).eq.1) then
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
				models(igraph2)%ic(1,m)=ic(1,m)
				models(igraph2)%ic(2,m)=ic(2,m)
			  enddo
			  do j=1,nrows
				if (ratcons(indrat)%ligant(j).eq.'none') then
					ratcons(indrat)%iconc(j)=0
				else
				do k=1,models(igraph2)%nlig
					if(ratcons(indrat)%ligant(j).eq.models(igraph2)%ligname(k)) ratcons(indrat)%iconc(j)=k
				enddo
				endif
			  enddo
			  
			endif
			!call generate_charmod(imodel,models,ilast,jlast,1)
			!  do i=1,ilast
			!	write(7,671) (models(igraph2)%charmod(i,j),j=1,jlast)
671				format(4x,35a2)
			!  enddo
			
111			continue
		
		
			icalprevc=0
		!	icalcyc=0
			rtitle=ratcons(indrat)%title
			do i=1,7
		!	iwcell(i)=
			enddo
			
			isw=2
			ncols=6
			xtext1='i,j '//char(124)//'Rate name'//char(124)//'Rate value'//char(124)//&
			'Conc dep.'//char(124)//'Volt dep'//char(124)//'Obs'
			if(.not.three_d) then	
			call text_array(igraph2,Main,form1,Form1_TxtArray1,ncols,nrows,iwcell,ihcell,xtext1,isw,&
			models(igraph2)%name_link,ratcons(indrat)%iconc,ratcons(indrat)%qij,ratcons(indrat)%value,&
			models(igraph2)%title_model,ratcons(indrat)%titlep,models(igraph2)%NBOUND,&
			models(igraph2)%ligname,models(igraph2)%nlig,ratcons(indrat)%micro,&
			ratcons(indrat)%ligant,jfix,theta0,iformText)
		  if(isetu.eq.1) call gmsetwidgetstatus(intoggle(3),gchecked)
				
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
			
		endif		
	   endif
	   endif


	case(1501:1599) ! read rates (icon- more then one for a mechanism)
	  lt=callid-1500
	case(1500) !3200
		do i=1,nradio
		istg=gmEnqToggleSwitch(iradio_Toggle(i))
		if(istg.eq.gon) lt=i
		enddo
	  if(iarca.eq.-2) then
		indrat=indrec1(lt)
		do i=1,100
			indrec(i)=indrec1(i)
		enddo
	  else
	  indrat=indrec(lt)
	  endif
	  indratprev=lt
	  imod=ratcons(indrat)%imod
	  irecm0=indrat
	!  irecfin(irc)=irecm0
	  call gmremovewindow(iradio)
	!!!  if(autosim) inipage=0

	  if(inipage.eq.-100) then 
		call read_rates(-1,form1,Form1_TxtArray1,irecm0,imod,qmec,nrows,indrat,&
		ratcons,models,ic,ipbar_progress2,igraph2,nmr,conc,iformText)
	  
		callid=-23
		
	  else if(iarca.eq.-1) then
						indk=indrat
						irecq=irecm0
						if(inipage.eq.-200.or.inipage.eq.-300) then
								call gmremovewindow(records)
							
								neq=0
							fixec50=.false.
							nmr=0
							nfixec50=0
							nfix=0
							callid=-22
							do jk=1,200
							jfix(jk)=0
							enddo
							inipage=0
						!!	goto 1
						endif
						mod_create=-irecq
						jgraph=igraph2
						call create_model(ixm,iym,igraph2,imodel,main,form1,form1_txtarray1,&
						graph1_2,GraphMainPanel1_2,graphics1_2,&
						nxmodel,nymodel,mtype,models,mod_create,irc,qmec,text7(i),&
						dxs,dys,ijmod,ka,kf,ic,indrat,pfilem,open7,apfile,ipbar_progress2,&
						indwin,ncdep,iformText,autosim,ksim,&
		initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
						nrmodel(igraph2)=imodel
						imod0=imodel
						call read_rates(Main,form1,Form1_TxtArray1,irecm0,imod,qmec,nrows,indk,&
						ratcons,models,ic,ipbar_Progress2,jgraph,nmr,conc,iformText)
						
						infoind=infoind+1
						imod=imodel
						itxtinfo=gmcreatetextentry(infopanel,1,infoind,infolength,1,&
						'Mechanisms file: '//qmec,32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						infoind=infoind+1
						call intconv(imod,cstring)
						itxtinfo=gmcreatetextentry(infopanel,1,infoind,infolength,1,&
						'Model nr= '//cstring(1:4)//models(igraph2)%title_model,32768, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=9, &
              			gmVpos=GTOP, gmExpand=GOFF)
						irecfin(irc)=-100
						callid=1201
						iarca=0
						goto 2
	 else if(iarca.eq.-2) then
		iarca=0	
		indk=indrat
						
	    call gmremovewindow(records)
		if(autosim) then
			if(ksim.ne.0) then
			irect=irecm0
			callid=-44
			else
			irecq=irecm0
			callid=-206
			irrate=irecm0
			endif
		else
			irecq=irecm0
			callid=-44
		endif
	
		continue
		
	  else
	  call read_rates(Main,form1,Form1_TxtArray1,irecm0,imod,qmec,nrows,indrat,&
	  ratcons,models,ic,ipbar_progress2,igraph2,nmr,conc,iformText)
	  endif
		 

	case(1451:1475) ! save rates	5501:6000
		igraph2=callid-1450
		imod=models(igraph2)%model
		nrows=models(igraph2)%npar
		do j=1,	models(igraph2)%npar	
		call gmEnqCellSetting(Form1_TxtArray1(igraph2,2), 1,j,rval,ratcons(indrat)%qij(j))
		call gmEnqCellSetting(Form1_TxtArray1(igraph2,2), 2,j,rval,ratcons(indrat)%titlep(j))
		call gmEnqCellSetting(Form1_TxtArray1(igraph2,2), 3,j,ratcons(indrat)%value(j),actext)
	!	call gmEnqCellSetting(Form1_TxtArray1(imod,2), 5,j,ratcons(indrat)%iconc(j),actext)
		
		call gmEnqCellSetting(Form1_TxtArray1(igraph2,2), 4,j,rval,ratcons(indrat)%ligant(j))
		if (ratcons(indrat)%ligant(j).eq.'none') then
			ratcons(indrat)%iconc(j)=0
		else
			do k=1,models(igraph2)%nlig
				if(ratcons(indrat)%ligant(j).eq.models(igraph2)%ligname(k)) ratcons(indrat)%iconc(j)=k
			enddo
			
		endif
		
		enddo
		call gFlushGraphics()
		
		if(indmod.eq..false.) then
				nmr=ncon-k+1
				if(nmr.gt.0) then
		!		imi=gmdisplaymessagebox(' ','Get MR if you have not done it yet',gexclamation,gstop)
				else
		!		imi=gmdisplaymessagebox(' ','Get constraints if you have not done it yet',gexclamation,gstop)
				endif		
		endif
		call gmSetWidgetStatus(ibutton1, GSELECTABLE)

case(1600)	
    if(ihelpmod.ne.-1) then
	call help_modify(main,imodhelp)
	ihelpmod=-1
!	call gmsetwidgetstatus(icongmod,gselectable)
	endif
 case(1601:1625) ! modify old model
    if(ihelpmod.ne.-1) then
	call help_modify(main,imodhelp)
	ihelpmod=-1
	
	endif
    joldgraph=callid-1600
	icallmod=callid
	joldmodel=nrmodel(joldgraph)	
	!joldmodel=callid-7000
	jmodel=jmodel+1
	newmodel=jmodel
	igraph2=igraph2+1
	jopen2(igraph2)=1
	models(igraph2)%model=newmodel
	nrmodel(igraph2)=newmodel
	indkn=newmodel
    mod_create=0
    if(indk.le.1.or.indk.gt.500) indk=indrat
	
	if(joldmodel.eq.0) then
		joldmodel=imod
		nrmodel(joldgraph)=joldgraph
	endif
	readrec=.true.
	indk0=indk
	call copy_model(models,joldmodel,newmodel,joldgraph,igraph2,ratcons,indk,indkn)
	indk=indkn
	indrat=indk
	imod=newmodel
	do k=1,models(igraph2)%npar
		do i=1,2
		ic2(i,k)=models(igraph2)%ic(i,k)

		enddo
	!	val2(ic(1,k),ic(2,k))=ratcons(indk)%value(k)
	!	tit2(ic(1,k),ic(2,k))=ratcons(indk)%titlep(k)
	enddo
	models(igraph2)%title_model=' '

	iqwin = gmCreateComplexDialogueBox(Main, 15, 10, 15, 8, GALL,'Modified mechanism', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
	
	iqPanel=gmCreatePanel(iqwin, 0, 0, 15, 8, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmtitle=' ',gmExpand=GOff, gmType=GNOBOUNDARY, &
            	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

	itext = gmCreateTextEntry(iqPanel, 1, 1, 3,1,'New title=', 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
    iqt1(3) = gmCreateTextEntry(iqPanel, 4, 1, 10, 1,textiqt1(3), 60, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT,gmscrollable=gon, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
	jgraph2=igraph2			
	do i=1,models(igraph2)%nlig
		textiqt1(i)=models(igraph2)%ligname(i)
				
	enddo

	
	iqradio=gmCreateRadioBox(iqPanel, 0, 2,14 , 2, gmType=GFREEFORM, gmBorderType=Gnone, &
      gmFillCol=0,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, &
	  gmTitle='Number of ligands bound', gmVpos=GTOP)

	inToggle(1) = gmCreateToggleButton(iqradio, 1, 1, 2, 1, 'None', 0, &
              	gmType=G3Dradio,gmVpos=GTOP, gmExpand=GOFF, gmCallBAck=1651)
  
	inToggle(2) = gmCreateToggleButton(iqradio, 4, 1, 2, 1, 'One', 0, &
              	gmType=G3Dradio,gmVpos=GTOP, gmExpand=GOFF, gmCallBAck=1652)
	inToggle(3) = gmCreateToggleButton(iqradio, 7, 1, 2, 1, 'Two', 0, &
              	gmType=G3Dradio,gmVpos=GTOP, gmExpand=GOFF, gmCallBAck=1653)
	
	call gmsettoggleswitch(intoggle(models(igraph2)%nlig+1),gon)
	itext = gmCreateTextEntry(iqPanel, 1, 4, 3, 1,'Ligand 1', 32768, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
			
	iqt1(1) = gmCreateTextEntry(iqPanel, 4, 4, 10, 1,textiqt1(1), 60, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT,gmscrollable=gon, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
	itext = gmCreateTextEntry(iqPanel, 1, 5, 3, 1,'Ligand 2', 40, Gdisplay, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)
			
	iqt1(2) = gmCreateTextEntry(iqPanel, 4, 5, 10, 1,textiqt1(2), 60, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT,gmscrollable=gon, &
              	gmBack1Col=0, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

	iqb=gmCreatePushButton(iqpanel,1,0, 13, 1,'Continue',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=1650)
	call gmdrawwindow(iqwin)
	if(nlig.eq.0) then
			call gmsetwidgetstatus(iqt1(1),gunselectable)
			call gmsetwidgetstatus(iqt1(2),gunselectable)
	else if(nlig.eq.1) then
			call gmsetwidgetstatus(iqt1(2),gunselectable)
	endif

	case(1651)
		models(jgraph2)%nlig=0
		call gmsetwidgetstatus(iqt1(1),gunselectable)
			call gmsetwidgetstatus(iqt1(2),gunselectable)
	case(1652)
		models(jgraph2)%nlig=1
		call gmsetwidgetstatus(iqt1(1),Gselectable)
			call gmsetwidgetstatus(iqt1(2),Gunselectable)
	case(1653)
		models(jgraph2)%nlig=2
		call gmsetwidgetstatus(iqt1(1),gselectable)
			call gmsetwidgetstatus(iqt1(2),gselectable)
	
	case(1650)
			if(models(jgraph2)%nlig.gt.0) then
				do i=1,models(jgraph2)%nlig
				call gmEnqTextSetting(iqt1(i),textiqt1(i))
				ligname(i)=textiqt1(i)
			
				models(igraph2)%ligname(i)=ligname(i)
				enddo
			endif
			call gmEnqTextSetting(iqt1(3),textiqt1(3))
			models(jgraph2)%title_model=textiqt1(3)

			mod_create=0
			call gmRemoveWindow(iqwin)	
			call gmRemoveWindow(Graph1_2(jgraph2-1))
			call gmRemoveWindow(Form1(jgraph2-1,1))

			call gmsetwidgetstatus(iqbta,gunselectable)
			call gmsetwidgetstatus(iqbta1,gunselectable)
			igraph2=jgraph2
			call draw_model(igraph2,newmodel,main,graph1_2,GraphMainPanel1_2,graphics1_2,&
			models,.false.,0,-1,initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
				if(mod_create.ne.1) call gmSetWidgetStatus(ibutton1, GunSELECTABLE)
			ibkta=5
	case(1626)

	val(1)=gmenqvaluesetting(ival(1))
	models(jgraph2)%nlig=val(1)
	call gmRemoveWindow(ivvwin)	
	if(models(jgraph2)%nlig.gt.0) then
				
				do i=1,models(igraph2)%nlig
					textiqt1(i)=models(jgraph2)%ligname(i)
					call intconv(i,cnum1)
					textiq1(i)='Ligand bound'//cnum1(1:2)
				enddo
				textiq1(20)='Name for ligands'
				call text_table(Main,iqwin,textiq1(20),models(jgraph2)%nlig,textiq1,iqt1,textiqt1,1627)
		
	else
		callid=1627
		goto 2
	endif

	
	case(1627) ! new title modified model

	if(models(jgraph2)%nlig.gt.0) then
	do i=1,models(jgraph2)%nlig
	call gmEnqTextSetting(iqt1(i),textiqt1(i))
			ligname(i)=textiqt1(i)
			
			models(igraph2)%ligname(i)=ligname(i)
	enddo
		call gmRemoveWindow(iqwin)	
	endif 
	textiq1(1)='New Title'
	textiq1(2)='Model number'
	textiq1(20)='Enter model title'
	call text_table(Main,iqwin,textiq1(20),1,textiq1,iqt1,textiqt1,1628)
   CASE(1628)
   	
	call gmEnqTextSetting(iqt1(1),textiqt1(1))
	models(jgraph2)%title_model=textiqt1(1)
	!call gmEnqTextSetting(iqt1(2),textiqt1(2))
	!call chatoint(textiqt1(2),imod1)
	mod_create=0
	call gmRemoveWindow(iqwin)	
	call gmRemoveWindow(Graph1_2(jgraph2-1))
	call gmRemoveWindow(Form1(jgraph2-1,1))
!	call gmRemoveWindow(Form1(jgraph2-1,2))
	call gmsetwidgetstatus(iqbta,gunselectable)
	call gmsetwidgetstatus(iqbta1,gunselectable)
	igraph2=jgraph2
	call draw_model(igraph2,newmodel,main,graph1_2,GraphMainPanel1_2,graphics1_2,&
		models,.false.,0,-1,initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
				if(mod_create.ne.1) call gmSetWidgetStatus(ibutton1, GunSELECTABLE)
	ibkta=5
!	call page_help(main,indmod,iphelp)
case(1800)
	if(igraph2.gt.0) then
	callid=igraph2+1800
	goto 2
	endif
case(1801:1825)!(4501:5000) ! save on disk (icon)
		if(igraph2.gt.0) then
		igraph2=callid-1800
		imod=models(igraph2)%model
		
		do j=1,	models(igraph2)%n	
		call gmEnqCellSetting(Form1_TxtArray1(igraph2,1), 1,j,rval,models(igraph2)%statname(j))
		call gmEnqCellSetting(Form1_TxtArray1(igraph2,1), 2,j,models(igraph2)%dgamma(j),actext)
		call gmEnqCellSetting(Form1_TxtArray1(igraph2,1), 4,j,rval,models(igraph2)%name_link(j))
		call gmEnqCellSetting(Form1_TxtArray1(igraph2,1), 3,j,models(igraph2)%inter_link(j),actext)
		!!! nbound?
		
		
		enddo 
		
		call gmActivateGraphicsFrame(graphics1_2(igraph2))
		if(models(igraph2)%indmod) then
		    kstat=models(igraph2)%kstat
			kcon=models(igraph2)%kcon
			nsub=models(igraph2)%nsub
			kstat0=models(igraph2)%kstat0
			do j=1,models(igraph2)%kcon
				statname(j)=models(igraph2)%con_states(j)
			enddo
			do i=1,models(igraph2)%nsub
	
			do j=1,models(igraph2)%kstat0
				statname(kcon+j+kstat0*(i-1))=models(igraph2)%sub_states(i,j)
			enddo
			enddo
			if(nsub.eq.0)  then
				i=1
				do j=1,kstat-kcon
					statname(kcon+j)=models(igraph2)%sub_states(i,j)
				enddo
			endif
			do j=1,models(igraph2)%kcon	
			do l=1,10
		!		if(models(igraph2)%statname(j)(l:l).eq.'*') models(igraph2)%statname(j)(l:l)='x'
			enddo
			call write_string(models(igraph2)%statname(j)(1:6),&
			models(igraph2)%X(j)-0.1,&
			models(igraph2)%y(j)-0.2,0.,0,1,0.35,48,0.6,0.8)
			call write_string(models(igraph2)%statname(j)(7:12),&
			models(igraph2)%X(j)-0.1,&
			models(igraph2)%y(j)-0.7,0.,0,1,0.35,48,0.6,0.8)
		
			enddo	
			l=1+models(igraph2)%kcon
		
			do j=l,models(igraph2)%n
			do lk=1,10
		
			if(models(igraph2)%statname(j)(lk:lk).eq.char(45)) then
			i1=lk
			endif
			enddo
			if(i1.ne.0) then
			call write_string(models(igraph2)%statname(j)(1:i1-1),models(igraph2)%X(j)-0.1,&
			models(igraph2)%y(j)-0.2,0.,0,1,0.35,48,0.6,0.8)	
			call write_string(models(igraph2)%statname(j)(i1+1:i1+4),models(igraph2)%X(j)-0.1,&
			models(igraph2)%y(j)-0.7,0.,0,1,0.35,48,0.6,0.8)
			else
			call write_string(models(igraph2)%statname(j)(1:6),models(igraph2)%X(j)-0.1,&
			models(igraph2)%y(j)-0.2,0.,0,1,0.35,48,0.6,0.8)	
			endif
			
			enddo	
		else
		do j=1,models(igraph2)%n
			
			statname(j)=models(igraph2)%statname(j)
			
			do l=1,10
		!		if(models(igraph2)%statname(j)(l:l).eq.'*') models(igraph2)%statname(j)(l:l)='x'
			enddo
			call lincol(models(igraph2)%colour(j))
		    call jSYMBOL(models(igraph2)%X(j),models(igraph2)%y(j),-3,1.2,1.2,13,idev)
				call write_string(models(igraph2)%name(j),models(igraph2)%X(j)-0.1,&
		models(igraph2)%y(j)+0.4,0.,0,1,0.35,48,dxs,dys)
	
			
			call write_string(models(igraph2)%statname(j)(1:6),&
			models(igraph2)%X(j)-0.1,&
			models(igraph2)%y(j)-0.2,0.,0,1,0.35,48,0.6,0.8)
			call write_string(models(igraph2)%statname(j)(7:12),&
			models(igraph2)%X(j)-0.1,&
			models(igraph2)%y(j)-0.7,0.,0,1,0.35,48,0.6,0.8)
		enddo
		endif
	
        	
		if(models(igraph2)%nlig.gt.0) then
		k=1
		do 	i=5,4+models(igraph2)%nlig
			do j=1,models(igraph2)%n
				call gmEnqCellSetting(Form1_TxtArray1(igraph2,1), I,j ,&
				models(igraph2)%nbound(j,k),actext)
			enddo
			K=K+1
		enddo
		endif
			do j=1,	models(igraph2)%npar	
		call gmEnqCellSetting(Form1_TxtArray1(igraph2,2), 1,j,rval,ratcons(indrat)%qij(j))
		call gmEnqCellSetting(Form1_TxtArray1(igraph2,2), 2,j,rval,ratcons(indrat)%titlep(j))
		call gmEnqCellSetting(Form1_TxtArray1(igraph2,2), 3,j,ratcons(indrat)%value(j),actext)
	!	call gmEnqCellSetting(Form1_TxtArray1(imod,2), 5,j,ratcons(indrat)%iconc(j),actext)
		
		call gmEnqCellSetting(Form1_TxtArray1(igraph2,2), 4,j,rval,ratcons(indrat)%ligant(j))
		if (ratcons(indrat)%ligant(j).eq.'none') then
			ratcons(indrat)%iconc(j)=0
		else
			do k=1,models(igraph2)%nlig
				if(ratcons(indrat)%ligant(j).eq.models(igraph2)%ligname(k)) ratcons(indrat)%iconc(j)=k
			enddo
			
		endif
		
		enddo
	
		call gFlushGraphics()
		
		imod=models(igraph2)%model
	    call intconv(imod,cnum5)
		sfile='model'//cnum5(1:3)//'.jpg'
		ns=len_trim(sfile)
			call devsus  
		if (sfile(ns-3:ns).eq.'.wmf') then
			idev=1
	   		pwmf=.true.
	   				
	   		idpi=600
	   		ixoff=0
	   		iyoff=0
	   		iwidi=4800
	   		ihei=3600
	   		call wmfp(idpi,ixoff,iyoff,iwidi,ihei)			
					!else if (sfile(ns-3:ns).eq.'.bmp') then
		else if(sfile(ns-3:ns).eq.'.jpg') then
			
			idev=2
			call gjpeg
		
		else
	   		pbmp=.true.
	   		idev=2
	   		call bmp		
	   	endif 
	
		ipos=0
		plot=.true.
		 
		 
		call gsetdevicefilename(sfile,0)
		call draw_model(jgraph,imod,main,graph1_2,GraphMainPanel1_2,graphics1_2,&
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
        if(icalprev.eq.2051.and.readini) then
			if(ioptm.ne.3.and.ioptm.ne.4) then
 				imessy=gmDisplayMessageBox('','Keep MR and constraints as in ini file?',Gquestion,gyesno)
		 
		
				indk=indrat
				if(imessy.eq.gyesbutton) then
		
					ratcons(indk)%ncyc=ncyc2
					do l=1,ncyc2
					ratcons(indk)%nsc(l)=nsc2(l)
					do m=1,nsc2(l)
							   
					ratcons(indk)%im(l,m)=im2(1,m)
					ratcons(indk)%jm(l,m)=jm2(1,m)
					enddo
							
				
					enddo
 		
					ratcons(indk)%nsetq=neq
					do i=1,neq
					ratcons(indk)%ieq(i)=ie(i)
					ratcons(indk)%jeq(i)=je(i)
					ratcons(indk)%ifq(i)=if(i)
					ratcons(indk)%jfq(i)=jf(i)
					ratcons(indk)%efacq(i)=efac(i)
					enddo
					newmr=.false.
					newcons=.false.
					callid=1881
					goto 2
						
				else
					newcons=.true.
					newmr=.true.
					goto 1211
				endif
			
			else
			    indk=indrat
			    if(mod_create.ne.0) ncyc=ratcons(indk)%ncyc
			    if(ncyc.gt.0) then
		
				imessy=gmDisplayMessageBox('','Keep MR as in mechanism file?',Gquestion,gyesno)
		 
				if(imessy.eq.gyesbutton) then
					newmr=.false.
					newcons=.false.	
					callid=1881
				
					goto 2
			
				else
					newcons=.true.
					newmr=.true.
					goto 1211
				endif
			else
			    if(mod_create.eq.0) then
				ncmax=6
				ncon=models(igraph2)%ncon
				npar=models(igraph2)%npar
				call find_cyc(models(igraph2)%n,models(igraph2)%link,ncyc,im,jm,nsc,ncmax)
					ratcons(indk)%ncyc=ncyc
				do l=1,ncyc
				obeymr(l)=.true.
				ratcons(indk)%nsc(l)=nsc(l)
				do m=1,nsc(l)
							   
				ratcons(indk)%im(l,m)=im(l,m)
				ratcons(indk)%jm(l,m)=jm(l,m)
				enddo
					enddo
				endif
				inipage=-9
				if(.not.autosim)then
				
					callid=1881
					goto 2
				
				else
					callid=1881
					goto 2
				endif
			endif
			endif
		else if(.not.readini) then
		    indk=indrat
			if(mod_create.ne.0) ncyc=ratcons(indk)%ncyc
			if(ncyc.gt.0) then
		
				imessy=gmDisplayMessageBox('','Keep MR as in mechanism file?',Gquestion,gyesno)
		 
		
		
				if(imessy.eq.gyesbutton) then
					newmr=.false.
					newcons=.false.	
					callid=1881
					goto 2
			
				else
					newcons=.true.
					newmr=.true.
					goto 1211
				endif
			else
			    if(mod_create.eq.0) then
				ncmax=6
				ncon=models(igraph2)%ncon
				npar=models(igraph2)%npar
				call find_cyc(models(igraph2)%n,models(igraph2)%link,ncyc,im,jm,nsc,ncmax)
					ratcons(indk)%ncyc=ncyc
				do l=1,ncyc
				obeymr(l)=.true.
				ratcons(indk)%nsc(l)=nsc(l)
				do m=1,nsc(l)
							   
				ratcons(indk)%im(l,m)=im(l,m)
				ratcons(indk)%jm(l,m)=jm(l,m)
				enddo
					enddo
				endif
				inipage=-9
				if(.not.autosim)then
				!	messy2=gmDisplayMessageBox('','Define Constraints now?',Gquestion,gyesno)
				!	if(messy2.eq.gyesbutton) then
				!	callid=2030
				!	goto 2
				!	else
					callid=1881
					goto 2
				!	endif
				else
					callid=1881
					goto 2
				endif
			endif
		else
			callid=1881
			goto 2
		endif

1211    continue
        if(newcons) then
		if(.not.readini) then
			imessy2=gmDisplayMessageBox('','Define MR now?',Gquestion,gyesno)
			if(imessy2.eq.gyesbutton) then
				ncyc=0
				ncmax=6
				ncon=models(igraph2)%ncon
				npar=models(igraph2)%npar
				call find_cyc(models(igraph2)%n,models(igraph2)%link,ncyc,im,jm,nsc,ncmax)
					ratcons(indk)%ncyc=ncyc
				do l=1,ncyc
					obeymr(l)=.true.
				ratcons(indk)%nsc(l)=nsc(l)
				do m=1,nsc(l)-1
							   
				ratcons(indk)%im(l,m)=im(1,m)
				ratcons(indk)%jm(l,m)=jm(1,m)
				enddo
					enddo
				if(ncyc.gt.0) then
					inipage=-9
					text_tog(2)='Spanning Tree Method'
					text_tog(1)='4 Cycles Method'
					
					call toggle_panel(Main,ITOGGLE,itogglepanel,2,text_tog,intoggle,1882,valdat,idat,itogbutton,itype)
 	                call gmsetwidgetstatus(intoggle(2),gunselectable)

				else
					inipage=-9
					if(.not.autosim) then
					!	messy2=gmDisplayMessageBox('','Define Constraints now?',Gquestion,gyesno)
					!	if(messy2.eq.gyesbutton) then
					!		callid=2030
					!		goto 2
					!	else
							callid=1881
							goto 2
					!	endif
					else
						callid=1881
						goto 2
					endif
				endif
			else
			    imessy2=gmDisplayMessageBox('','MR found automatically;can be changed on the main page',Gexclamation,gok)
				inipage=-9
				if(.not.autosim) then
             !   messy2=gmDisplayMessageBox('','Define Constraints now?',Gquestion,gyesno)
		      !  if(messy2.eq.gyesbutton) then
			!		callid=2030
			!		goto 2
			!	else
					callid=1881
					goto 2
			!	endif
				else
				callid=1881
				goto 2
				endif
			endif
		else
		if(ioptm.ne.4) then
				indk=indrat
				neq0=0
				neq=0
 				ratcons(indk)%ncyc=0
				ratcons(indk)%nsetq=0
				callid=1881
				nsetq=0
				nfixec50=0
				nfix=0
					goto 2
		else
		   callid=1881
				goto 2 
		endif
		endif
		endif
		else
				imessy=gmDisplayMessageBox('','No model on screen',Gexclamation,gok)
			
		endif
		
	case(1884) ! spanning
		indmr=2010
		call gmSetToggleSwitch(intoggle(2), Goff)	
	case(1883)
		indmr=2020
	    call gmSetToggleSwitch(intoggle(1), Goff)	
case(1882)
	if(indmr.ne.2010.and.indmr.ne.2020) indmr=2020
		callid=indmr
	
		call gmremovewindow(itoggle)
		goto 2
case(1881)

		!imessy=gmDisplayMessageBox('','SAVE CHANGES (IF ANY) TO MECHANISM FILE NOW?',Gquestion,gyesno)
		goto 1166
		if(imessy.eq.gyesbutton) then
				efile=qmec

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
1166    continue

		if(icalprev.eq.2051) then
					icalprev=0
					callid=2053
					goto 2
		else if(icalprev.eq.2052) then
					icalprev=0
					goto 1
		endif
		if(inipage.eq.-300) then
					callid=-22
					irecq=irecm0
					inipage=0
					qmec=efile
					jopen2(igraph2)=-1
					call gmremovewindow(Graph1_2(igraph2))
					call gmremovewindow(Form1(igraph2,1))
					call gmremovewindow(Form1(igraph2,2))
					goto 1
		endif
		
case(1880)
	
case(1850)	
	if(igraph2.gt.0) then
	callid=igraph2+1850
	goto 2
	endif
case(1851:1875) ! print model (icon) !4001:4500
		igraph2=callid-1850
	

		imod=models(igraph2)%model
		istatus=gmprintercontrol(gprintersetup)
		istatus=gmprintercontrol(GOPENPRINTER)
	!	CALL GUIPRT(0,ISTATus)
		IF(ISTATus.NE.0.and.imod.gt.0)THEN
			plot=.true.
			idev=6
			ipos=0
			imessy=gmDisplayMessageBox('','Black & White ?',Gquestion,gyesno)
			if(imessy.eq.gyesbutton) idev=4
			imod=models(igraph2)%model
			call draw_model(jgraph,imod,main,graph1_2,GraphMainPanel1_2,graphics1_2,&
			models,plot,ipos,mod_create,initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
			plot=.false.
			CALL GUIPRT(1,ISTATus)
			idev=0
		else
				imessy=gmDisplayMessageBox('','No model on screen',Gexclamation,gok)
		
			
		endif

	case(1900)	
	if(igraph2.gt.0) then
	callid=igraph2+1900
	goto 2
	endif
	
	case(1901:1925) !(8001:8500) ! save image model
		igraph2=callid-1900
		imod=models(igraph2)%model
		if(igraph2.gt.0) then
		jgraph=igraph2
		sfile='hjcfit.wmf'
		sfilt='*.wmf'//char(124)//'Windows Metafile (WMF)'//char(124)//&
		      '*.bmp'//char(124)//'Windows Bitmap Format (BMP)'
		CALL gmFileBROWSER(sFILE,sDIR,sFILT,gmBrowseType=1)
		if(sfile.ne.' ' ) then
		imod=models(igraph2)%model
		ns=len_trim(sfile)
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
			models,plot,ipos,mod_create,initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
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
	


     case(1950) ! change title6502
if(imodel.gt.0) then
	textiqt1(1)=' '
	textiqt1(1)=models(igraph2)%title_model
	textiqt1(20)='name:'
	textiqt1(19)=' '
	call text_table(Main,iqwin,textiqt1(19),1,textiqt1(20),iqt1,textiqt1,1951)
	endif
case(1960) ! change rate title6504
if(indrat.gt.0) then
		textiqt1(1)=' '
	textiqt1(1)=ratcons(indrat)%title
	textiqt1(20)='rate:'
	textiqt1(19)=' '
	call text_table(Main,iqwin,textiqt1(19),1,textiqt1(20),iqt1,textiqt1,1961)
	
	endif
case(1951,1961)
	call gmEnqTextSetting(iqt1(1),textiqt1(1))
	if(callid.eq.1951) then
		models(igraph2)%title_model=textiqt1(1)
		call gmsetTextSetting(iformText(igraph2,1),textiqt1(1))
		call gmsetTextSetting(igraphText(igraph2),textiqt1(1))
		titlem=textiqt1(1)
	else
		ratcons(indrat)%title=textiqt1(1)
		rtitle=textiqt1(1)
		call gmsetTextSetting(iformText(igraph2,2),textiqt1(1))
	endif

	call gmRemoveWindow(iqwin)
	
	
	
!CONSTRAINTS+MR
!================================
	

	
	 CASE(2010) !(1001) !MR -stree
		if(igraph2.gt.0) then
		if(models(igraph2)%indmod) then
			jkstatus=gmdisplaymessagebox('','Independent model.No MR at this stage',gexclamation,gok)
			goto 1
	   	endif
		if(igraph2.le.0) then
				imy=gmdisplaymessagebox(' ','No model on screen',gexclamation,gstop)
				goto 1
		endif
		if(nmr.le.0) then
		!		imy=gmdisplaymessagebox(' ','No MR for this model',gexclamation,gstop)
		!		goto 1
		    endif
		newmr=.true.	
	    npar=models(igraph2)%npar
		ncon=models(igraph2)%ncon
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
		nd1=200
		nd2=100
		ncon=models(igraph2)%ncon
		k=models(igraph2)%n
		call ic_jcon(ic,ncon,nd1,jcon,k,nd2)
		nmr=models(igraph2)%ncon-models(igraph2)%n+1
	
		    useprim=.true.
			imes=gmdisplaymessagebox('','Did you open rates table first',gquestion,gyesno)
			if(imes.eq.gnobutton) goto 1
			imes=gmdisplaymessagebox('','Did you get the constraints first',gquestion,gyesno)
			if(imes.eq.gnobutton) then
				callid=2030 !701
				goto 2
		    endif
			
			if(readini) then
				if(useprim.and.imod.eq.imodold) then
					useini=.true.
				else
					useini=.false.
				endif
			else
				useini=.false.
			endif
			ical=2011
			if(useini) then
			    ical=2018
			    if(nsc2(i).gt.1.and.nsc2(i).lt.models(igraph2)%n) then
					ncyc=ncyc2
					do i=1,ncyc
					nsc(i)=nsc2(i)
					do j=1,nsc(i)
						im(i,j)=im2(i,j)
						jm(i,j)=jm2(i,j)
					enddo
					enddo
				else
					ncyc=ratcons(indrat)%ncyc
					if(ncyc.gt.0) then
					do i=1,ncyc
					nsc(i)=ratcons(indrat)%nsc(i)
					do j=1,nsc(i)
						im(i,j)=ratcons(indrat)%im(i,j)
						jm(i,j)=ratcons(indrat)%jm(i,j)
					enddo
				
					enddo
					endif
				endif
					
				do ir=1,models(igraph2)%npar
				do m=1,ncyc
		
				if(irate(ir).eq.im(m,1).and.jrate(ir).eq.jm(m,1)) then
					ifixmr(ir)=-1 !mc
			
				endif	
				enddo

				enddo
			else
					ncyc=ratcons(indrat)%ncyc
					if(ncyc.gt.0) then
					do i=1,ncyc
					nsc(i)=ratcons(indrat)%nsc(i)
					do j=1,nsc(i)
						im(i,j)=ratcons(indrat)%im(i,j)
						jm(i,j)=ratcons(indrat)%jm(i,j)
					enddo
				
					enddo
					endif
			endif
			if(ncyc.gt.0) then
				if(jm(1,1).lt.1.and.jm(1,1).gt.models(igraph2)%n) then
					do i=1,ncyc
					do j=1,nsc(i)
					jm(i,j)=im(i,j+1)
					enddo
					jm(i,nsc(i))=im(i,1)
					enddo
				endif
			else
				ncmax=6
				call find_cyc(models(igraph2)%n,models(igraph2)%link,ncyc,im,jm,nsc,ncmax)
				
			endif
			allmr=.false.
			do i=1,ncmax
				obeymr(i)=.false.
				automr(i)=.false.	!irrelevant here but need to set false for qset_hjc
				isetmr(i)=i		!order irrelevant here
			enddo
			do ir=1,npar
					do j=1,nc
					if(irate(ir).eq.icspec(1,j).and.jrate(ir).eq.icspec(2,j)) then
					ifixmr(ir)=-2 ! mc+ec50
				!	if(ir.lt.npar) ifixmr1(ir+1)=-2
					endif
					enddo
			enddo
			do i=1,ncyc
					call intconv(i,cnum(1))
					text_box(i,1)=cnum(1)
					call intconv(nsc(i),cnum(1))
					text_box(i,2)=cnum(1)
					!call intconv(im(i,1),cnum5)
					call intconv(im(i,1),cnum5)
					
					nt5=len_trim(cnum5)
					cnum(2)=cnum5
					text_box(i,3)=cnum5(1:nt5)
					if(useini) then
					i1=im(i,1)		!default
					j1=jm(i,1)
					do j=1,npar
						if(irate(j).eq.i1.and.jrate(j).eq.j1) then
						text_box(i,4)=cnum5(1:nt5)	
						endif
					enddo
				
					endif
					do j=2,nsc(i)
				
						call intconv(im(i,j),cnum51)
						nt=len_trim(text_box(i,3))
						text_box(i,3)=text_box(i,3)(1:nt)//','//cnum51
					enddo
					
		    enddo
				ncombo=ncyc
				do i=1,ncyc
					ncomboname(i)=nsc(i)
					kl=1
					do j=1,ncomboname(i)
					    do m=1,npar
							if(im1(i,j).eq.irate(m).and.jm1(i,j).eq.jrate(m)) then
								if(jfix(m).eq.1) goto 881
								do mk=1,neq
									if(im1(i,j).eq.ie(mk).and.jm1(i,j).eq.je(mk)) goto 881
								enddo
							endif
					    enddo
						if(im1(i,j).eq.ie(m).and.jm1(i,j).eq.je(m)) goto 881
						call intconv(im1(i,j),cnum(2))
						call intconv(jm1(i,j),cnum51)
						nt5=len_trim(cnum(2))
						nt51=len_trim(cnum51)
						comboname(i,kl)=cnum(2)(1:nt5)//','//cnum51(1:nt51)
						kl=kl+1
881						continue
					enddo
					text_box(i,5)=comboname(i,1)
					ncomboname(i)=kl-1
				enddo	
			
			call cyc_array(imodel,main,itwin1,ncyc,4,text_box,itext_box,ical,&
				icyc_form,icmr,comboname,ncomboname)
			


	
	else
		imymy=gmdisplaymessagebox('','No MR for this model',ginformation,gok)
	endif
                
	CASE(2011,2018,2019)  !(1004)
		if(callid.eq.2018) then
			goto 1
		else if (callid.eq.2019) then
			ncyc=0
			nmr=0
			allmr=.false.
			do i=1,ncmax
				obeymr(i)=.false.
				automr(i)=.false.
				isetmr(i)=i
			enddo
			goto 1
		endif
		iyes=0
		nmr=ncyc
		allmr=.true.
		call intconv(nc,cnum5)
	!	imessc=gmdisplaymessagebox('Number of routes:',&
	!	cnum5//' rates already constraint.Exclude more?',gquestion,gyesno)
	!	if(imessc.eq.gyesbutton) then
					celltitle='Spanning tree'
					nrows=npar-nc
					ncols=3
					ctitle1(1)='Route/edge'
					ctitle1(2)='Option'
					ctitle1(3)='Observations'
					ncombo=1
					ncomboname(1)=3
					comboname(1,1)='MR'
					comboname(1,2)='S-Tree'
					comboname(1,3)='None'
					ncols=3
					icombo(2)=.true.
					nrows=0
					ncombo=1
					do i=1,ncyc
						!if(isetmr(i).eq.-1) then
						do j=1,nsc(i)
						ik=im(i,j)
						jk=jm(i,j)
						do ir=1,npar
							if(ik.eq.irate(ir).and.jk.eq.jrate(ir)) then
							if(ifixmr(ir).ne.-2) then
							    do l=1,nrows-1
									if(ik.eq.irate2(l).and.jk.eq.jrate2(l).or.&
									ik.eq.jrate2(l).and.jk.eq.irate2(l)) goto 3535
								enddo
								nrows=nrows+1
								irate2(nrows)=ik
								jrate2(nrows)=jk
								textcell2(2,nrows)='None'
								if(ifixmr(ir).eq.-1) textcell2(3,nrows)='old MR'
								call intconv(ik,cnum(1))
								call intconv(jk,cnum51)
							
								textcell2(1,nrows)=cnum(1)(1:2)//','//cnum51(1:2)
							endif
							endif
						enddo
						continue
3535					enddo
						!endif
					enddo
				    do j=1,nrows-1
					
						if(irate2(nrows).eq.irate2(j).and.jrate2(nrows).eq.jrate2(j).or.&
						irate2(nrows).eq.jrate2(j).and.jrate2(nrows).eq.irate2(j)) nrows=nrows-1
					enddo
				
					goto 3636
					do ir=1,npar
						if(ifixmr(ir).ne.-2.and.ifixmr1(ir).ne.-2) then
						good=.false.
						do ik=1,ncyc
						    if(isetmr(ik).eq.-1) then
							do jk=1,nsc(ik)
							  if(irate(ir).eq.im(ik,jk).and.jrate(ir).eq.jm(ik,jk)) then
								nrows=nrows+1
						
								textcell2(1,nrows)=ratcons(indrat)%qij(ir)
								textcell2(2,nrows)='MR'
								if(ifixmr(ir).eq.-1) textcell2(3,nrows)='old MR'
								irate2(nrows)=irate(ir)
								jrate2(nrows)=jrate(ir)
								goto 1771
							  endif
							enddo
							endif
						enddo
						
						endif
1771					continue
					enddo
					ipso=-1		

3636				do i=1,5
						iwcell(i)=100
					enddo
					ka=models(igraph2)%ka
					call cellarray(main,icell,icellarray,celltitle,nrows,ncols,ctitle1,textcell2,&
					iwcell,ncombo,comboname,ncomboname,icombo,2012,2,valcell1,ka,irate)
	
	!else
	!	imymy=gmdisplaymessagebox('','No MR for this model',ginformation,gok)
	!endif
	case(2012)
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
				ICout(1,ncout)=irate2(i)
				ICout(2,ncout)=jrate2(i)
			endif
		enddo
		if(ncout.eq.nrows) allmr=.true.
		nc=nc+nc0
		ncin=nc
		ndim=100
		k=models(igraph2)%n
		nd1=200
		nd2=100
		if(ncin.gt.0) then
			call IC_JCON(icspec,ncin,nd1,jtree,k,nd2)
			call NCONS(Jtree,k,ncon1,nstate,ndim)
	   
			write(7,577) nstate,ncon1
577			format(' Specified routes have ',i3,' states, ',i3,' connections')
			if(ncon1.gt.nstate-1) then
				write(7,161)
161				format(' The excluded routes contain cycle(s): invalid, try again')
				imes=gmdisplaymessagebox('','The constraints contain cycle(s)',gstop,gok)
				call gmremovewindow(icell)
				goto 1
			else
			ncmax=k	!max cycle size to be found
		
			call CYCQ1(k,nc,icspec,ncyc2,nsc0,im,jm,ncmax,ncyc0)
			if(ncyc2.ne.0) then
				write(7,16) ncyc
				imes=gmdisplaymessagebox('','The constraints contain cycle(s)',gstop,gok)
16				format(' From CYCQ1, these routes contain ',i3,' cycle(s): invalid, try again')
				jkstatus=gmdisplaymessagebox('Error','This routes contain cycles;try again',gstop,gok)
				call gmremovewindow(icell)
				goto 1
		
			endif
			endif
		endif
		ndim=100
		call prim3(models(igraph2)%link,ICspec,ncin,ICout,ncout,Iedge,nedge,nerr1,nerr2,nerr3,nerr4,&
		models(igraph2)%n,ndim,mabel,matrix,n)
	nd1=200
	nd2=100
		nc1=nedge
		call IC_JCON(Iedge,nc1,nd1,Jtree,k,nd2)
		call NCONS(Jtree,k,ncon1,nstate,ndim)
		
		if(ncon1.ne.nstate-1) then
	 
			if(discprt) write(7,19) ncon1,nstate
19			format(' ERROR: number of connections in spanning tree = ',i3,&
     		' is not number of states - 1')

	   endif

	   if(nerr3.ne.0) then
	 
			if(discprt) write(7,20)
20			format(' The routes that are specified to be in the tree are NOT all in')
			imes=gmdisplaymessagebox('',&
			'The routes that are specified to be in the tree are NOT all in',ginformation,gok)
	   else if(nerr4.ne.0) then
	   
			if(discprt) write(7,21)
21			format(' The routes that are specified to be not in the tree are NOT all excluded')
			imes=gmdisplaymessagebox('',&
			'The routes that are excluded are NOT all excluded',ginformation,gok)
	 
	   endif

	
	   write(7,121)
121	   format(/,' Edges in spanning tree')
	   do j=1,nedge
	   
			if(discprt) write(7,110) j,iedge(1,j),iedge(2,j)
110			format(' ',i3,': ',i3,' - ',i3)
	   enddo
	   if(ncyc.gt.0.and.useini) then
			imes=gmdisplaymessagebox('','Use old MR?',gquestion,gyesno)
			if (imes.eq.gyesbutton) then
				do jq=1,models(igraph2)%npar
					ratcons(indrat)%micro(jq)=''
				enddo
				do i=1,ncyc
				do jq=1,models(igraph2)%npar
				
					nl=len_trim(ratcons(indrat)%qij(jq))
					cnum5=ratcons(indrat)%qij(jq)(3:nl-1)
					call chatonos(cnum5,num,nk)
				
					if(im(i,1).eq.num(1).and.im(i,2).eq.num(2)) then
						ratcons(indrat)%micro(jq)='MR'
					endif
				enddo
				enddo
					jkstatus=gmdisplaymessagebox('','Constraints/ Fix Paramters',ginformation,gok)

				goto 1
            endif
		endif
		
	   nm=0
	   do i=2,models(igraph2)%n	!check lower triangle only
		do j=1,i-1
		if(models(igraph2)%link(i,j).eq.1.and.jtree(i,j).eq.0) then
		   nm=nm+1
		   call CIRCUIT(i,j,incirc,ncirc)
		   call intconv(nm,cnum(1))
		   text_box(nm,1)=cnum(1)
		   call intconv(ncirc,cnum(1))
		   text_box(nm,2)=cnum(1)
		
		   call intconv(incirc(ncirc),cnum5)
				
		   nt5=len_trim(cnum5)
		   cnum(2)=cnum5
		   text_box(nm,3)=cnum5(1:nt5)
		   do jk=ncirc-1,1,-1
				call intconv(incirc(jk),cnum51)
				nt=len_trim(text_box(nm,3))
				text_box(nm,3)=text_box(nm,3)(1:nt)//','//cnum51
		   enddo
		   if(incirc(ncirc).ne.i.or.incirc(1).ne.j) then
		    text_box(nm,4)='bad'
			if(discprt) write(7,1441) i,j,(incirc(n),n=1,ncirc)
1441			format(' Error in circuit: i, j = ',i3,',',i3,/,5(20i3,/))
		   endif
		   nsc(nm)=ncirc
		   call GETM(i,j,m1,npar,irate,jrate)
		   call GETM(j,i,m2,npar,irate,jrate)
			call intconv(i,cnum1)
			 nt=len_trim(cnum1)
			 call intconv(j,cnum5)
			 nt5=len_trim(cnum5)
           if(allmr) then
			 text_box(nm,4)='yes'
			 if(im(nm,1).eq.j.and.jm(nm,i).eq.j) then
				text_box(nm,5)=cnum5(1:nt5)//','//cnum1(1:nt)
			 else
				text_box(nm,5)=cnum1(1:nt)//','//cnum5(1:nt5)
			 endif
		   else
		     text_box(nm,4)='yes'
			 if(im(nm,1).eq.j.and.jm(nm,i).eq.j) then
				text_box(nm,5)=cnum5(1:nt5)//','//cnum1(1:nt)
			 else
				text_box(nm,5)=cnum1(1:nt)//','//cnum5(1:nt5)
			 endif
			 if(.not.obeymr(nm))  text_box(nm,4)='no'
		   endif

		endif
		enddo
	  enddo   	
447   continue
	ncols=5	
	ncombo=ncyc
				do i=1,ncyc
					ncomboname(i)=nsc(i)
					kl=1
					do j=1,ncomboname(i)
					    do m=1,npar
							if(im1(i,j).eq.irate(m).and.jm1(i,j).eq.jrate(m)) then
								if(jfix(m).eq.1) goto 882
								do mk=1,neq
									if(im1(i,j).eq.ie(mk).and.jm1(i,j).eq.je(mk)) goto 882
								enddo
							endif
					    enddo
						if(im1(i,j).eq.ie(m).and.jm1(i,j).eq.je(m)) goto 882
						call intconv(im1(i,j),cnum(2))
						call intconv(jm1(i,j),cnum51)
						nt5=len_trim(cnum(2))
						nt51=len_trim(cnum51)
						comboname(i,kl)=cnum(2)(1:nt5)//','//cnum51(1:nt51)
						kl=kl+1
882						continue
					enddo
					text_box(i,5)=comboname(i,1)
					ncomboname(i)=kl-1
				enddo	
			
		call cyc_array(imodel,main,itwin1,nm,5,text_box,itext_box,2013,&
		icyc_form,icmr,comboname,ncomboname)
			

	case(2013)
		do jq=1,models(igraph2)%npar
			if(ratcons(indrat)%micro(jq).eq.'MR')	ratcons(indrat)%micro(jq)=''
        enddo
		do i=1,ncyc
		isetmr(i)=i
			do j=4,5	
				call gmEnqCellSetting(icyc_Form(imod), j,i,rval,text_box(i,j))
			enddo
			nl1=len_trim(text_box(i,5))
			cnum0=text_box(i,5)(1:nl1)
			call chatonos(cnum0,num0,nk)
			call chatonos(text_box(i,5),num0,nk)
			im(n,1)=num0(1)
			jm(n,1)=num0(2)
			if(im(n,1).eq.incirc(n)) then
			do m=2,nsc(n)
			jm(n,m)=incirc(m)
			im(n,m)=jm(n,m-1)
			enddo
			else
			do m=2,nsc(n)
			jm(n,m)=incirc(ncirc-m+1)
			im(n,m)=jm(n,m-1)
			enddo
			endif
			if(text_box(i,4).eq.'yes') then
				obeymr(i)=.true.
				do jq=1,models(igraph2)%npar
				
					nl=len_trim(ratcons(indrat)%qij(jq))
					cnum5=ratcons(indrat)%qij(jq)(3:nl-1)
					call chatonos(cnum5,num,nk)
				
					if(num0(1).eq.num(1).and.num0(2).eq.num(2)) then
						ratcons(indrat)%micro(jq)='MR'
					endif
				enddo
			else
				obeymr(i)=.false.
				
				do jq=1,models(igraph2)%npar
				
					nl=len_trim(ratcons(indrat)%qij(jq))
					cnum5=ratcons(indrat)%qij(jq)(3:nl-1)
					call chatonos(cnum5,num,nk)
				
					if(num0(1).eq.num(1).and.num0(2).eq.num(2)) then
						ratcons(indrat)%micro(jq)=' '
					endif
				enddo
			endif
			if(obeymr(i)) then
				call GETM(im(n,1),jm(n,1),m,npar,irate,jrate)
  
	   			if(discprt) write(7,1311) n,im(n,1),jm(n,1),nsc(n),titlep(m),im(n,1),jm(n,1)
1311			format(1x,i3,': Microscopic reversibility route ',i3,'-',i3,&
				' found from ',i3,'-state cycle',/,' ',a10,' = ',' q(',i3,',',i3,') calc by micro rev')
		
				if(discprt) write(7,128)(im(n,m),jm(n,m),m=2,nsc(n))
128				format(20(5(i3,'-',i3,3x)))
			else
		
				if(discprt) write(7,129)n,(im(n,m),jm(n,m),m=1,nsc(n))
129				format(' Cycle ',i3,' NOT set by micro rev',/,20(5(i3,'-',i3,3x)),/)
			endif
		enddo


	do i=1,200
	   jmic(i)=0	!zero in case mic rev parameter has been changed
	enddo
	nmr=0			!number of cycles that obey m.r. (.le.ncyc)
	! where is iq?????
	do m=1,npar
		i=irate(m)
		j=jrate(m)
		IQ(i,j)=m
	enddo
	if(ncyc.gt.0) then
	   do i1=1,ncyc
		i=isetmr(i1)	!actual cycle number
		if(obeymr(i).and.(.not.automr(i))) then	!if cycle #i does not obey mr (see GETREV)
		   m=IQ(im(i,1),jm(i,1))
		   jmic(m)=1		!fixed by micro rev
		   nmr=nmr+1
		endif
	   enddo
	endif
	call gmremovewindow(itwin1)
	limits%xmax=320.
	limits%ymin=0
	limits%xmin=240.
	limits%ymax=20.
	call gmSetGuiGridMode(GOFF)

	jkstatus=gmdisplaymessagebox('','Constraints/ Fix Paramters',ginformation,gok)
	if(isetu.eq.1) call gmsetwidgetstatus(intoggle(4),gchecked)
		!	imess=gmdisplaymessagebox('','Now start fitting',ginformation,gok)
	call gmSetGuiGridMode(GOn)



	case (2020) !(3521,3522) ! 4 cycles method
			if(igraph2.le.0) then
				imy=gmdisplaymessagebox(' ','No model on screen',gexclamation,gstop)
				goto 1
		    endif
			
		   if(models(igraph2)%indmod) then
			jkstatus=gmdisplaymessagebox('','Independent model.No MR at this stage',ginformation,gok)
			goto 1
	   	   endif
			
			newmr=.true.
			imod=imodel
			indk=indrat
			nmr=models(igraph2)%ncon-models(igraph2)%n+1
		
			ncyc=0
			ncmax=6
			ncon=models(igraph2)%ncon
			npar=models(igraph2)%npar
			call find_cyc(models(igraph2)%n,models(igraph2)%link,ncyc,im,jm,nsc,ncmax)
			i6=0
			icalcyc=0
			do i=1,ncyc
					if(nsc(i).gt.4) i6=1
			enddo
			if(i6.eq.1) then
				
				imes=gmdisplaymessagebox('','Nsc > 4.Use spanning tree model',ginformation,gok)
				goto 1
			endif
			
			call findcube(ncube,cubedef,cubecyc,cubext,extcyc)
			
			call GETREV(ncyc,nsc,im,jm,ncyc1,nsc1,im1,jm1,useini,nccub,ncube,cubedef,cubecyc,cubext,extcyc)
			!im working array
			!im1 -new order
			!im2= ini
			n=1
			val(1)=ncyc
			vtext(1)='Number of cycles'
			do i=1,50
				itypeval(i)=9
			enddo
			itypeval(1)=0
			call value_table(Main,ivwin,n,vtext,ival,val,2021,itypeval)
			
	case(2023)
	     !   if(readini.and.ioptm.eq.3) igraph2=1
	        kj=1
		    k=models(igraph2)%n
		    npar=models(igraph2)%npar
		    ncon=models(igraph2)%ncon
		    do m=1,models(igraph2)%ncon
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
			m=5
			do i=1,50
				icmr(i)=50
			enddo
			do m=1,npar
				i=irate(m)
				j=jrate(m)
				IQ(i,j)=m
			enddo
			if(ncyc.gt.0) then
				title_box(1)='Cycle'
				title_box(2)='Nr of states'
				title_box(3)='States in order'
				title_box(4)='Use MR'
				title_box(5)='i,j'
				do j=1,ncyc
				isetmr(j)=j
				if(obeymr(j)) then
				
					call intconv(j,cnum(1))
					text_box(j,1)=cnum(1)	
					call intconv(nsc(j),cnum(1))
					text_box(j,2)=cnum(1)
					call intconv(im(j,1),cnum5)
					
					nt5=len_trim(cnum5)
					text_box(j,3)=cnum5(1:nt5)
					do jo=2,nsc(j)
				
						call intconv(im(j,jo),cnum51)
						nt=len_trim(text_box(j,3))
						text_box(j,3)=text_box(j,3)(1:nt)//','//cnum51
					enddo
					if(automr(j)) then
						text_box(j,4)='auto'
					else
						text_box(j,4)='yes'
					endif
						
					call intconv(im(j,1),cnum(2))
					call intconv(jm(j,1),cnum51)
					nt5=len_trim(cnum(2))
					nt51=len_trim(cnum51)
					icmr(j)=7
									
					text_box(j,5)=cnum(2)(1:nt5)//','//cnum51(1:nt51)
					
					ncomboname(j)=nsc(j)
					kl=0
					do jk=1,nsc(j)
						i1=im(j,jk)		!current rate in cycle #i, i1,j1 and j1,i1
						j1=jm(j,jk)
						m1=IQ(i1,j1)	!param number for 'forward' rate
						m2=IQ(j1,i1)	!param number for 'reverse' rate						
						if(jfix(m1).eq.0.and.jcon(m1).eq.0) then
							call intconv(i1,cnum(2))
							call intconv(j1,cnum51)
							nt5=len_trim(cnum(2))
							nt51=len_trim(cnum51)
							kl=kl+1
							comboname(j,kl)=cnum(2)(1:nt5)//','//cnum51(1:nt51)

						endif
						if(jfix(m2).eq.0.and.jcon(m2).eq.0) then
							call intconv(j1,cnum(2))
							call intconv(i1,cnum51)
							nt5=len_trim(cnum(2))
							nt51=len_trim(cnum51)
							kl=kl+1
							comboname(j,kl)=cnum(2)(1:nt5)//','//cnum51(1:nt51)
						endif
884						continue
					enddo	!end of j=1,nsc(i)
					ncomboname(j)=kl
					text_box(j,5)=comboname(j,1)
				endif
				enddo	!end of i=1,ncyc
				ncombo=ncyc
				
				call cyc_array(imod,main,itwin1,ncyc,5,text_box,&
					itext_box,2022,icyc_form,icmr,comboname,ncomboname)
				endif

	case(2021) !(3510)
			val(1)=gmenqvaluesetting(ival(1))
			ncyc=int(val(1))
			call gmRemoveWindow(ivwin)	
			callid=2023
			goto 2
	
			m=5
			do i=1,50
				icmr(i)=50
			enddo
			if(ncyc.gt.0) then
				title_box(1)='Cycle'
				title_box(2)='Nr of states'
				title_box(3)='States in order'
				title_box(4)='Use MR'
				title_box(5)='i,j'
				!call value_table(Main,ivwin,ratcons(indrat)%ncyc,vtext,ival,val,3511)
				
				do i=1,ncyc
					call intconv(isetmr(i),cnum(1))
					text_box(isetmr(i),1)=cnum(1)
					call intconv(nsc1(i),cnum(1))
					text_box(isetmr(i),2)=cnum(1)
					!call intconv(im(i,1),cnum5)
					call intconv(im1(i,1),cnum5)
					ir1(i)=im1(i,1)
					nt5=len_trim(cnum5)
					cnum(2)=cnum5
					text_box(isetmr(i),3)=cnum5(1:nt5)
					do j=2,nsc(i)
					!	call intconv(im(i,j),cnum51)
						call intconv(im1(i,j),cnum51)
						nt=len_trim(text_box(isetmr(i),3))
						text_box(isetmr(i),3)=text_box(isetmr(i),3)(1:nt)//','//cnum51
					enddo
					call intconv(jm1(i,1),cnum51)
					jr1(i)=jm1(i,1)
					nt5=len_trim(cnum(2))
					nt51=len_trim(cnum51)
					if(automr(isetmr(i))) then
						text_box(isetmr(i),4)='auto'
					else
						text_box(isetmr(i),4)='yes'
					endif
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
									nt5=len_trim(cnum(2))
									nt51=len_trim(cnum51)
									icmr(i)=7
									
									text_box(isetmr(i),5)=cnum(2)(1:nt5)//','//cnum51(1:nt51)
									text_box(isetmr(i),3)=text_box(isetmr(i),5)
									do m=1,nsc1(i)
										icyc(m)=im1(i,m)
									enddo
									call order_states(i,ir2,jr2,icyc,nsc1(i),ncyc1,im1,jm1)
									nsc(isetmr(i))=nsc1(i)
									do m=1,nsc1(i)
										im(isetmr(i),m)=im1(i,m)
										jm(isetmr(i),m)=jm1(i,m)
									enddo
									do l=3,nsc1(i)
										call intconv(im1(i,l),cnum51)
										nt=len_trim(text_box(i,3))
										text_box(isetmr(i),3)=text_box(i,3)(1:nt)//','//cnum51
									enddo
									goto 444
								endif
							enddo ! end j2
							
							icmr(i)=2
							done(isetmr(i))=.false.
							text_box(isetmr(i),4)='no'
							text_box(isetmr(i),5)=cnum(2)(1:nt5)//','//cnum51(1:nt51)
							
						endif			
						enddo		
					!	text_box(i,4)='yes'
						text_box(isetmr(i),5)=cnum(2)(1:nt5)//','//cnum51(1:nt51)
						
				
444					continue
				enddo
!DC version
! For each cycle find which of the 8 (usually) rates in the cycle can be set validly by MR
! i.e. rate not fixed
!      rate not on LHS of constraimnt (ie(),je())
!		rate not set in an adjacent cycle
			! i preferred kl- not k or k1 dangerous!!!
			do m=1,npar
				i=irate(m)
				j=jrate(m)
				IQ(i,j)=m
			enddo
				do i=1,ncyc
					ncomboname(isetmr(i))=nsc(i)
					kl=0
					do j=1,nsc(i)
						i1=im1(i,j)		!current rate in cycle #i, i1,j1 and j1,i1
						j1=jm1(i,j)
						m1=IQ(i1,j1)	!param number for 'forward' rate
						m2=IQ(j1,i1)	!param number for 'reverse' rate						
						if(jfix(m1).eq.0.and.jcon(m1).eq.0) then
							call intconv(i1,cnum(2))
							call intconv(j1,cnum51)
							nt5=len_trim(cnum(2))
							nt51=len_trim(cnum51)
							kl=kl+1
							comboname(isetmr(i),kl)=cnum(2)(1:nt5)//','//cnum51(1:nt51)

						endif
						if(jfix(m2).eq.0.and.jcon(m2).eq.0) then
							call intconv(j1,cnum(2))
							call intconv(i1,cnum51)
							nt5=len_trim(cnum(2))
							nt51=len_trim(cnum51)
							kl=kl+1
							comboname(isetmr(i),kl)=cnum(2)(1:nt5)//','//cnum51(1:nt51)
						endif
883						continue
					enddo	!end of j=1,nsc(i)
					ncomboname(isetmr(i))=kl
					text_box(isetmr(i),5)=comboname(isetmr(i),1)
				enddo	!end of i=1,ncyc
				ncombo=ncyc
					
!end of DC version
!				ncombo=ncyc
!				do i=1,ncyc
!					ncomboname(i)=nsc(i)
!					do j=1,ncomboname(i)
!					    do m=1,npar
!							if(im1(i,j).eq.irate(m).and.jm1(i,j).eq.jrate(m)) then
!								if(jfix(m).eq.1) goto 88
!								do mk=1,neq
!									if(im1(i,j).eq.ie(mk).and.jm1(i,j).eq.je(mk)) goto 88
!								enddo
!							endif
!					    enddo
!						if(im1(i,j).eq.ie(m).and.jm1(i,j).eq.je(m)) goto 88
!						call intconv(im1(i,j),cnum(2))
!						nt5=len_trim(cnum(2))
!						nt51=len_trim(cnum51)
!						comboname(i,kl)=cnum(2)(1:nt5)//','//cnum51(1:nt51)
!						kl=kl+1
!88						continue
!					enddo
!					text_box(i,5)=comboname(i,1)
!					ncomboname(i)=kl-1
!				enddo	
!			
				!! combobox here!!
			!	call table_box(main,ratcons(indrat)%ncyc,m,title_box,text_box,itext_box,3511)
				call cyc_array(imod,main,itwin1,ncyc,5,text_box,&
				itext_box,2022,icyc_form,icmr,comboname,ncomboname)

			else
				callid=2022 !3511
				goto 2
			endif
				
	case(2022)			!micro rev
	
	!!!!! calculate nmr again
	    npar=models(igraph2)%npar
		ncon=models(igraph2)%ncon
		!ncyc=ratcons(indrat)%ncyc
		if(ncyc.gt.0) then
		ratcons(indrat)%ncyc=ncyc
			newmr=0
			do j=1,models(igraph2)%npar
				if(ratcons(indrat)%micro(j).eq.'MR') ratcons(indrat)%micro(j)=' '
	
			enddo
		
			do i=1,ncyc
			!	jmic(i)=0
				obeymr(i)=.false.
				isetmr(i)=i
				ir=isetmr(i)
				do j=2,5	
					call gmEnqCellSetting(icyc_Form(imod), j,i,rval,text_box(i,j))
				enddo
			
				call chtoint(text_box(i,2),nsc((i)))
				cnum0=text_box(i,3)
				nlc=len_trim(cnum0)
				nlc=len_trim(text_box(i,3))
				k=1
				l=1
				ij=1
goto 83
				do m=ij,nlc
					if(cnum0(m:m).eq.',') then
						cnum5=' '
						cnum5=cnum0(l:m-1)
						call chtoint(cnum5,num1)
		
					!	ratcons(indrat)%im(isetmr(i),k)=num1
						im(i,k)=num1
						
						k=k+1
						l=m+1
					endif

				enddo	
83  continue			
				nl1=len_trim(text_box(i,5))
				cnum0=text_box(i,5)(1:nl1)
				call chatonos(text_box(i,5)(1:20),num0,nk)
				ir1(i)=num0(1)
				jr1(i)=num0(2)
			   	indnmr=0
				indnmrj=0
				do k=1,nsc((i))
				  if(ir1(i).eq.im(i,k)) then
			
					indnmr=k
				  endif
				  if(jr1(i).eq.im(i,k)) then
					
					indnmrj=k
				  endif
				enddo

				if(indnmr.gt.0.and.indnmrj.gt.0) good=.true.
				
				if(good) then
					
				    igo=0
					if(indnmr.gt.indnmrj) then ! reverse cycle
						do j=1,nsc(i)
						
							if(ir1(i).eq.im(i,j).and.jr1(i).eq.jm(i,j)) then
								igo=-1	
							endif
						enddo
					    if(igo.ne.-1) then
						do j=1,nsc(i)
						im2(i,j) = im(i,nsc(i)-j+1)
						enddo
						else
						do j=1,nsc(i)
						im2(i,j) = im(i,j)
						enddo  
						endif
					else
						do j=1,nsc(i)
						
							if(ir1(i).eq.im(i,j).and.jr1(i).eq.jm(i,j)) then
								igo=-1	
							endif
						enddo
						if(igo.ne.-1) then
						do j=1,nsc(i)
						im2(i,j) = im(i,nsc(i)-j+1)
						enddo
						else
						do j=1,nsc(i)
						im2(i,j) = im(i,j)
						enddo 
						endif 
					endif
					
					
					do j=1,nsc(i)-1
					   jm2(i,j) = im2(i,j+1)
					enddo
					jm2(i,nsc(i)) = im2(i,1)
				
				
					indnmr=0
					indnmrj=0
					do k=1,nsc(i)
						if(ir1(i).eq.im2(i,k)) then
			
						indnmr=k
						endif
						if(jr1(i).eq.im2(i,k)) then
					
						indnmrj=k
						endif
					enddo

					im(i,1)=ir1(i)
					jm(i,1)=jr1(i)
					if(text_box(i,4).eq.'yes'.or.text_box(i,4).eq.'auto') then
						obeymr(i)=.true.
						ll=2
						ijk=indnmr+1
						if(ijk.gt.nsc(i)) indnmr=0
						do k=indnmr+1,nsc(i)
						im(i,ll)=im2(i,k)
				
						ll=ll+1
						enddo
						do k=1,indnmr-1
						im(i,ll)=im2(i,k)
					
						ll=ll+1
						enddo
						do j=1,nsc(i)-1
					   jm(i,j) = im(i,j+1)
						enddo
						jm(isetmr(i),nsc(isetmr(i))) = im(isetmr(i),1)

			
						do jq=1,models(igraph2)%npar
				
						nl=len_trim(ratcons(indrat)%qij(jq))
						cnum0=ratcons(indrat)%qij(jq)(3:nl-1)
						call chatonos(cnum0,num,nk)
				
						if(num0(1).eq.num(1).and.num0(2).eq.num(2)) then
						ratcons(indrat)%micro(jq)='MR'
						newmr=newmr+1
						endif
						enddo
				
					else
						do jq=1,models(igraph2)%npar
				
						nl=len_trim(ratcons(indrat)%qij(jq))
						cnum0=ratcons(indrat)%qij(jq)(3:nl-1)
						call chatonos(cnum0,num,nk)
				
						if(num0(1).eq.num(1).and.num0(2).eq.num(2)) then
						ratcons(indrat)%micro(jq)=''
						
						endif
						enddo
						obeymr(i)=.false.
					endif
					
				endif
			enddo
		
		
			ncyc2=ncyc
			do i=1,ncyc
			if(obeymr(i)) then
				nsc2(i)=nsc(i)
				do j=1,nsc(i)
				im2(i,j)=im(i,j)
		
				jm2(i,j)=jm(i,j)
				enddo
			endif
			
			enddo
			if(indk.le.1.or.indk.gt.500) indk=indrat
			ratcons(indk)%ncyc=ncyc
			ncyc1=ncyc
			do l=1,ncyc

			if(obeymr(i)) then
			ratcons(indk)%nsc(l)=nsc(l)
		    nsc1(l)=nsc(l)
		!	jmic(l)=1
			do m=1,nsc(l)
		
		ratcons(indk)%im(l,m)=im(l,m)
		ratcons(indk)%jm(l,m)=jm(l,m)
		im1(l,m)=im(l,m)
		jm1(l,m)=jm(l,m)
			enddo
			endif
			enddo
			nmr=newmr
		
		else
			if(models(igraph2)%indmod.eq..false.) then
			do j=1,nrows
			ratcons(indrat)%micro(j)=' '
			enddo
		
			endif
			nmr=0
		endif
		
		call gmRemoveWindow(itwin1)
		
		if(isetu.eq.1) call gmsetwidgetstatus(intoggle(4),gchecked)
		!	jkstatus=gmdisplaymessagebox('','Constraints/ Fix Parameters',ginformation,gok)
		if(inipage.eq.-100) then

			call gmsetvaluesetting(ini_Value2_1,nmr)
		    do i=1,10
			call gmSetCellSetting(ini_TxtArray2, 1,i,gmString='')
			call gmSetCellSetting(ini_TxtArray2, 2,i,gmString='')
			call gmSetCellSetting(ini_TxtArray2, 3,i,gmString='')
			call gmSetCellSetting(ini_TxtArray2, 4,i,gmString='')
			enddo
			imk=0
			do imr=1,ncyc
			nb=len_trim(text_box(imr,5))
			if(text_box(imr,4).eq.'yes') then
			imk=imk+1
		    call gmSetCellSetting(ini_TxtArray2, 1,imk,gmString=text_box(imr,1))
			call gmSetCellSetting(ini_TxtArray2, 2,imk,gmString='q('//text_box(imr,5)(1:nb)//')')
			do j=1,npar
				ratcons(indrat)%micro(j)=' '
				if(im(imr,1).eq.irate(j).and.jm(imr,1).eq.jrate(j) )then
				call gmSetCellSetting(ini_TxtArray2, 3,imk,gmString=ratcons(indk)%titlep(j))
				ratcons(indrat)%micro(j)='MR'
				endif
			enddo
			call intconv(im(imr,1),cnum5)
		    text_box(imr,3)=cnum5
			do jo=2,nsc(imr)
				
						call intconv(im(imr,jo),cnum51)
						nt=len_trim(text_box(imr,3))
						text_box(imr,3)=text_box(imr,3)(1:nt)//','//cnum51
			enddo
			call gmSetCellSetting(ini_TxtArray2, 4,imk,gmString=text_box(imr,3))
			endif
			enddo
		
			call gmdrawwidget(ini_txtarray2)
			goto 1
		else
			!imy=gmdisplaymessagebox(' ','Define new constraints now',gquestion,gyesno)
			!if(imy.eq.gyesbutton) then 
			!	inipage=-9
			!	callid=2030
			!	goto 2
			!else
				callid=1881
				goto 2
			!endif
		endif
	
	
	case(2030,2035) !(701) ! constraints
		do i=1,10
			ctitle1(i)=' '
		enddo
! DC 21 Jan 07.  Make sure IQ(defined) (shouldn't be needed every time!)
		kj=1
		k=models(igraph2)%n
		npar=models(igraph2)%npar
		ncon=models(igraph2)%ncon
		do m=1,models(igraph2)%ncon
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
		do m=1,npar
			i=irate(m)
			j=jrate(m)
			IQ(i,j)=m
		enddo
		if(inipage.eq.-100) goto 22
		if(neq.gt.0.or.nfixec50.gt.0) then
	!			imy=gmdisplaymessagebox(' ','Keep old constraints',gquestion,gyesno)
	!			if(imy.eq.gyesbutton) goto 1
		endif
22		continue
		if(callid.eq.2035) then

		if(ncdep.le.0) then
			imy=gmdisplaymessagebox(' ','No EC50 for this model',ginformation,gok)
			fixec50=.false.
			nfixec50=0
			goto 1
        else
			fixec50=.true.	
		endif
		endif
		
		if(callid.eq. 2030) then 
		!	newcons=.true.

		!	neq=0
		!	neq0=0
		!	neq1=0
			do i=1,npar
		!		jcon(i)=0
		!		efac(i)=0.0
			enddo
			
		!	neq1=0
			do i=1,neq
		!		if(efac(i).gt.0.) neq1=neq1+1	!default
			enddo
			do i=1,km		!km=100 at present
				jcon(i)=0		!in case old jcon read from disc
			enddo
		else
		!	nfixec50=0
		endif
		npar=models(igraph2)%npar
		ncon=models(igraph2)%ncon
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
		indk=indrat
			
		nlig=models(igraph2)%nlig
		
		nrows=0
				
		if(useprim.eq..false.) then
		
		do ir=1,models(igraph2)%npar
			do m=1,ncyc		
				if(irate(ir).eq.im(m,1).and.jrate(ir).eq.jm(m,1)) then
					ifixmr(ir)=-1 !mc
				endif	
			enddo
		enddo
		endif
		ico=0
		ipo=0
		nrows=0
		if(neq0.eq.0) neq0=neq
			ml=0
			do i=1,npar
				if(i.le.100.and.ratcons(indk)%micro(i).ne.'MR') then
					ml=ml+1
					comboname(2,ml)=ratcons(indk)%qij(i)
				endif
			enddo
			ncomboname(2)=ml
		do i=1,npar						 		
				nrows=nrows+1
				irate2(nrows)=irate(i)
				jrate2(nrows)=jrate(i)
				textcell1(1,nrows)=ratcons(indrat)%qij(i)
				textcell1(2,nrows)=ratcons(indk)%titlep(i)
				textcell1(3,nrows)='None'
				
				if(callid.eq.2030) then	
				    if(jfix(i).eq.1) then
				    textcell1(7,nrows)='Yes'
							else
					textcell1(7,nrows)='No'
					endif
					textcell1(8,nrows)='No'
					textcell1(10,nrows)='None'
				    if(ifixmr(i).eq.-1) textcell1(3,nrows)='MR'
					if((i50.eq.irate(i).and.j50.eq.jrate(i)).or.(i502.eq.irate(i).and.j502.eq.jrate(i))) then
						textcell1(3,nrows)='ec50'
					endif
!					textcell1(5,nrows)='none'		
					textcell1(5,nrows)='   '		
					textcell1(6,nrows)='    '		

					if(neq0.gt.0) then
					
						do j=1,neq0
							if(ie(j).eq.irate(i).and.je(j).eq.jrate(i)) then
								textcell1(3,nrows)='Multiplicative'
								ico=ico+1
!				DC 20 Jan 06; this is constraint #j	not constraint #ico
!								valcell1(4,nrows)=efac(ico) 
								valcell1(4,nrows)=efac(j) 
! DC 20 Jan 07. The next loop is specifying the wrong q(i,j) for the right
! hand side of the constraint -should beQ[if(j),.jf(j)] 
!								do l=1,models(igraph2)%npar
!									if(irate(l).eq.if(ico).and.jrate(l).eq.jf(ico)) then
!										textcell1(5,nrows)	=ratcons(indk)%qij(l)
!									endif
!								enddo
! DC correction	-simpler!!
								call intconv(if(j),cnum0)
 								call intconv(jf(j),cnum2)
								m=IQ(if(j),jf(j))
!								textcell1(5,nrows)='q('//cnum0(1:2)//','//cnum2(1:2)//')'
! as long as IQ() defined, next line gives same result as previous
								textcell1(5,nrows)=ratcons(indk)%qij(m)
! add the name for rate const on right side of constraint in column 6
								textcell1(6,nrows)=ratcons(indk)%titlep(m)
					! end of DC correction
! add another column here with name of this q(i,j)
							else
							!	textcell1(5,nrows)=comboname(2,nrows) !!!good???
							endif
						enddo
					endif
				
					if(neq1.gt.0) then
						do j=1,neq1
						if(ie(j+neq0).eq.irate(i).and.je(j+neq0).eq.jrate(i)) then
							textcell1(3,nrows)='Additive'
							ipo=ipo+1
							valcell1(4,nrows)=efac(ico)
							do l=1,models(igraph2)%npar
							if(irate(l).eq.if(ipo).and.jrate(l).eq.jf(ipo)) then
								textcell1(5,nrows)	=ratcons(indk)%qij(l)
							endif
							enddo
						endif
						enddo
					endif
				
				
408					continue
				else
					if(ie(j).eq.irate(i).and.je(j).eq.jrate(i)) then
						nrows=nrows-1
						goto 409
					endif
					if(i50.eq.irate(i).and.j50.eq.jrate(i)) textcell1(2,nrows)='Ec501'
					if(i502.eq.irate(i).and.j502.eq.jrate(i)) textcell1(2,nrows)='Ec502'
409					continue				
				endif
			!endif
			
		enddo
		if(nrows.eq.0) then
			imes=gmdisplaymessagebox('','No rates to constrain;start fitting',ginformation,gok)
			goto 1
		endif
		nlig=models(igraph2)%nlig
		do i=1,nlig
			ligname(i)=	models(igraph2)%ligname(i)
		enddo
		
		do i=1,7
			icombo(i)=.false.
		enddo
		
		
		if(callid.eq.2030) then
			celltitle='Rates available for constraints:'
			ctitle1(1)='Q(i,j)'
			ctitle1(2)='Name'
			ctitle1(3)='Constraint'
			ctitle1(4)='Factor x/ Total -'
			ctitle1(5)='Q(i,j)'
			ctitle1(6)='Name'
			ctitle1(7)='Fixed'
			ctitle1(8)='Fast block leads'
			ctitle1(9)='FB eq constant'
			ctitle1(10)='Ligand'
!			ncols=5
			ncols=10
			ncombo=5
			icombo(3)=.true.
			ncomboname(1)=3	
			comboname(1,1)='None'
			comboname(1,2)='Multiplicative'
			comboname(1,3)='Additive'
			comboname(1,4)='MR'
			comboname(1,5)='ec50'
			icombo(5)=.true.
			ncomboname(2)=npar	
			icombo(7)=.true.
			ncomboname(3)=2
			comboname(3,1)='No'
			comboname(3,2)='Yes'
			icombo(8)=.true.
			ncomboname(4)=3
			comboname(4,1)='No'
			comboname(4,2)='from open state'
			comboname(4,3)='from fast state'
			icombo(10)=.true.
			ncomboname(5)=nlig+1
			comboname(5,1)='None'
			nlig=models(jgraph)%nlig
			do i=1,nlig
			comboname(5,i+1)=ligname(i)
			enddo
			do i=1,7
				iwcell(i)=80
			enddo
	        iwcell(3)=90
	        iwcell(4)=60
	        iwcell(7)=60
	        iwcell(8)=100
	        iwcell(9)=100
	        iwcell(10)=100
	!ictype1 = gmCreateComboBox(Form1_TxtArray1(igraph2,isw),0,0,iwcell(5),2*32,Gnone, 1, &
     !     gmSort=GunSORTED, gmVpos=GTOP, gmExpand=GOFF)
	!	call gmSetListEntry(ictype1, GADD, gmString='no')
	!	call gmSetListEntry(ictype1, GADD, gmString='fixed')
	ka=models(igraph2)%ka
			call cellarray(main,icell,icellarray,celltitle,nrows,ncols,ctitle1,textcell1,&
			iwcell,ncombo,comboname,ncomboname,icombo,callid+1,1,valcell1,ka,irate)	
		else
			if(nlig.eq.0.or.ncdep.eq.0) then
				imes=gmdisplaymessagebox('','No EC50 : nlig=0 or ncdep=0',gquestion,gyesno)
				goto 1
			endif
			if(nlig.eq.1) then
				nlvar=1
				nfixec50=1		!number fixed by ec50 (0 or 1 or 2)
			else if(nlig.gt.1) then
				imes=gmdisplaymessagebox('','Constrain EC50 for TWO agonists',gquestion,gyesno)
				if(imes.eq.gnobutton) then
					nfixec50=1		!number fixed by ec50 (0 or 1 or 2)
				else
					nfixec50=2		!number fixed by ec50 (0 or 1 or 2)
				endif
			endif
			j=0
			do i=1,npar
				if(i.le.100.and.ratcons(indk)%micro(i).ne.'MR'.and.ratcons(indk)%micro(i).ne.&
					'constraint'.and.ratcons(indk)%micro(i).ne.'Multiplicative'.and.&
							ratcons(indk)%micro(i).ne.'Additive') then
					j=j+1
					comboname(1,j)=ratcons(indk)%qij(i)
				endif
			enddo
			j=j+1
			comboname(1,j)='none'
			ncomboname(1)=j	

			ieci=0
			if(nfixec50.gt.0)then
						do l=1,models(igraph2)%npar
						if(irate(l).eq.i50.and.jrate(l).eq.j50) then
							ieci=l
						
							textec50=ratcons(indk)%qij(l)
								
						endif
						enddo
			endif
			lec50=1
			do j=1,ncomboname(1)
				if(comboname(1,j).eq.textec50) lec50=j 
			enddo
			do ifix=1,nfixec50
				if(ifix.eq.1) then

					n=nlvar
					if(n.eq.0) n=1
					xs=sngl(ec501*1.d6)		!micromolar default
					val(1)=xs
				else
					n=nlvar2
					if(n.eq.0) then  	!pick a value different from nlvar
					do j=1,nlig
						if(j.ne.nlvar) then
						n=j
						goto 2712
						endif
					enddo
					endif
2712				xs=sngl(ec502*1.d6)		!micromolar default
					val(11)=xs
				endif
271				continue

				n=ifix
		 	    if(ifix.eq.1) then
					nlvar=n
		
					if(nlig.gt.1) then	!set conc of other ligands for ec50
					do i=1,nlig
						if(i.ne.nlvar) then
						x=1.e6*conc_ec1(i)
						val(2)=x
						endif
					enddo
					endif
				else if(ifix.eq.2) then
					nlvar2=n
					if(nlig.gt.1) then	!set conc of other ligands for ec50
					do i=1,nlig
						if(i.ne.nlvar2) then
						x=1.e6*conc_ec2(i)
						val(12)=x
						endif
					enddo
					endif
				endif
					if(ifix.eq.1) then
						i=i50
						j=j50
						i501=i50
						j501=j50
					else
						i=i502
						j=j502
					endif
					if(ifix.eq.1) then
						x1=sngl(xqlo)
						x2=sngl(xqhi)
						if(x2.eq.0.) x2=100000000.
						val(3)=x1
						val(4)=x2
					else
						x1=sngl(xqlo2)
						x2=sngl(xqhi2)
						val(13)=x1
						val(14)=x2
					endif
					val(10)=sngl(penfac)
		  
			enddo		!ifix=1,nfixec50
			call setec50(main,iec50,nfixec50,nlig,ligname,ival,val,comboname,npar,2036,iecctype,lec50)		 		
		endif

	case(2032)	
		call gmremovewindow(icell)
    case (2031,2036,2037) !(734)
		nrows=models(igraph2)%npar
		npar=models(igraph2)%npar
		if(callid.eq.2031) then
		neq0=0
		neq1=0
		iwr1=0
		
		nrows=models(igraph2)%npar
		npar=models(igraph2)%npar
		do i=1,models(igraph2)%npar
			call gmEnqCellSetting(icellarray(1), 3,i ,R1 ,textcell1(3,i))
			if(textcell1(3,i).eq.'Multiplicative') then
				call gmEnqCellSetting(icellarray(1), 1,i ,R1 ,textcell1(1,i))
				neq0=neq0+1
				call gmEnqCellSetting(icellarray(1), 4,i ,R1 ,textcell1(4,i))
				call gmEnqCellSetting(icellarray(1), 5,i ,R1 ,textcell1(5,i))
				call chtoreal(textcell1(4,i),efac(neq0))
				ie(neq0)=irate2(i)
				je(neq0)=jrate2(i)
				nl1=len_trim(textcell1(5,i))
				cnum0=textcell1(5,i)(3:nl1-1)
				call chatonos(cnum0,num0,nk)
				if(neq0)=num0(1)
				jf(neq0)=num0(2)
				i1=if(neq0)
				j1=jf(neq0)
			!	ratcons(indrat)%qt(i1,j1)
			!	ratcons(indrat)%qt(irate2(i),jrate2(i))=efac(neq0)*ratcons(indrat)%qt(i1,j1)
				
				do m=1,npar
				if(textcell1(5,i).eq.ratcons(indrat)%qij(m)) then
					xvalue=ratcons(indrat)%value(m)
					xvalue=efac(neq0)*xvalue
				endif
				enddo
				
				do m=1,npar
				if(textcell1(1,i).eq.ratcons(indrat)%qij(m)) then
					ratcons(indrat)%micro(m)='Multiplicative'
				
					ratcons(indrat)%value(m)=xvalue
					if(inipage.eq.-100) call gmSetCellSetting(ini_TxtArray7, 3,m,gmvalue=xvalue)
				endif
				enddo
				
			endif
		enddo
		do i=1,nrows
			call gmEnqCellSetting(icellarray(1), 3,i ,R1 ,textcell1(3,i))
			if(textcell1(3,i).eq.'Additive') then
				neq1=neq1+1
				call gmEnqCellSetting(icellarray(1), 4,i ,R1 ,textcell1(4,i))
				call chtoreal(textcell1(4,i),efac(neq0+neq1))
				ie(neq0+neq1)=irate2(i)
				je(neq0+neq1)=jrate2(i)
				call gmEnqCellSetting(icellarray(1), 5,i ,R1 ,textcell1(5,i))
			
				nl1=len_trim(textcell1(5,i))
				cnum0=textcell1(5,i)(3:nl1-1)
				call chatonos(cnum0,num0,nk)
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
				do m=1,npar
				if(textcell1(1,i).eq.ratcons(indrat)%qij(m)) then
					ratcons(indrat)%micro(m)='Additive'
					xvalue=ratcons(indrat)%qt(irate2(i),jrate2(i))
					ratcons(indrat)%value(m)=xvalue
					if(inipage.eq.-100) call gmSetCellSetting(ini_TxtArray7, 3,m,gmvalue=xvalue)
				endif
				enddo
			endif
		enddo
		neq=neq0+neq1
	
		continue
		nfix=0
	   
	   do m=1,models(igraph2)%npar
			if(isetu.ge.7) then
			call gmEnqCellSetting(Form1_TxtArray1(igraph2,4), 3,m ,xs,textcell)
			else
			
			call gmEnqCellSetting(icellarray(1), 7,m ,R1 ,textcell1(7,m))
			
			endif
			
			if(textcell1(7,m).eq.'Yes') then
				nfix=nfix+1
				jfix(m)=1
					ratcons(indk)%micro(m)='fixed'
            else
				jfix(m)=0
			endif
				
	   enddo
	   nblk=0
	   do m=1,models(igraph2)%npar
	        i=irate(m)
	        j=jrate(m)
	        call gmEnqCellSetting(icellarray(1), 8,m ,R1 ,textcell1(8,m))
	        if(textcell1(8,m).eq.'from open state') then
	            nblk=nblk+1
		        ifb(nblk)=i
		        jfb(nblk)=j
		        mfb(nblk)=m
		        ratcons(indk)%micro(m)='fast block'
		        call gmEnqCellSetting(icellarray(1), 9,m ,R1 ,textcell1(9,m))
		        akb=r1
		        aKB=1.e-6*aKB	!molar
		        call gmEnqCellSetting(icellarray(1), 10,m ,R1 ,textcell1(10,m))
		        cnum1=textcell1(10,m)
		        
		        xvalue=ratcons(indrat)%value(m)
		        xvalue=(1/(1+cB))*xvalue
	        else if (textcell1(8,m).eq.'from fast state') then
	            nblk=nblk+1
		        ifb(nblk)=i
		        jfb(nblk)=j
		        mfb(nblk)=-m
		        call gmEnqCellSetting(icellarray(1), 9,m ,R1 ,textcell1(9,m))
		        akb=r1
		        aKB=1.e-6*aKB	!molar
		        call gmEnqCellSetting(icellarray(1), 10,m ,R1 ,textcell1(10,m))
		        xvalue=ratcons(indrat)%value(m)
		        xvalue=(cB/(1+cB))*xvalue
		        cnum1=textcell1(10,m)
		        ratcons(indk)%micro(m)='fast block'
	        endif
	        
	   enddo
	   fastblk=.false.
	   nfblock=nblk
	   if(nfblock.gt.0) then
	   !ini_Value4_5
	        fastblk=.true.
	        if(discprt) write(7,3)
3	        format(/,     ' FAST OPEN CHANNEL BLOCK ASSUMED, cB = xB/KB',/)
	        do n1=1,nblk
		    if(mfb(n1).gt.0) then
		        m=iabs(mfb(n1))
		   
		        if(discprt) write(7,4) titlep(m),m,ifb(n1),jfb(n1)
4		        format(/,1x,a10,     ': rate ',i2,' = q(',i2,',',i2,' is multiplied by 1/(1+cB))')
		    else
		        m=iabs(mfb(n1))
		   
		        if(discprt) write(7,5) titlep(m),m,ifb(n1),jfb(n1)
5		        format(/,1x,a10,    ': rate ',i2,' = q(',i2,',',i2,' is multiplied by cB/(1+cB))')
		    endif
	        enddo
	        iflig=1
	        if(nlig.gt.1) then
	        if(discprt) write(7,51) 1.e6*aKB,cnum1
51	        format(' Equilibrium constant for open channel block (micromolar) = ',&
            g13.6,/,' Blocker = ',a10)
            iflig=1
            nlig=models(jgraph)%nlig
            do inlig=1,nlig
                if(cnum1.eq.ligname(inlig)) iflig=inlig
            enddo
            endif
	   endif
	  
	   kfit=npar-nfix-neq-nmr	!nmr=# of cycles constrained by mr (.le.ncyc)
	   if(fixec50) kfit=kfit-nfixec50
	   ik=0		!use to check vs kfit
	   do m=1,npar
				i=irate(m)
				j=jrate(m)
				QT(i,j)=theta0(m)
				ratcons(indrat)%qt(i,j)=qt(i,j)
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
				thetaf(ik)=theta0(m)
			!	thetaf(ik)=QT(i,j)
				
				jfix1(ik)=0
			endif
			endif
		enddo
		do i=1,m
			xvreal=theta0(m)
			call gmSetCellSetting(ini_TxtArray7, 3,m,gmvalue=xvreal)
		enddo	
			nmod=imodel	
		if(nmod.eq.1.and.npar.eq.10) dcmodel=.true.
		if(nmod.eq.14.and.npar.eq.4) dcmodel=.true.
	if(nmod.eq.9.and.npar.eq.6) dcmodel=.true.
	if(nmod.eq.10.and.npar.eq.8) dcmodel=.true.
	if(nmod.eq.11.and.npar.eq.14) dcmodel=.true.
	if(nmod.eq.29.and.npar.eq.14) dcmodel=.true.
	if(nmod.eq.33.and.npar.eq.20) dcmodel=.true.
	if(nmod.eq.34.and.npar.eq.12) dcmodel=.true.
	if(nmod.eq.35.and.npar.eq.16) dcmodel=.true.
	if(nmod.eq.36.and.npar.eq.16) dcmodel=.true.
	if(nmod.eq.37.and.npar.eq.18) dcmodel=.true.
	if(nmod.eq.38.and.npar.eq.18) dcmodel=.true.
	if(nmod.eq.39.and.npar.eq.18) dcmodel=.true.
	if(nmod.eq.40.and.npar.eq.20) dcmodel=.true.
	if(nmod.eq.41.and.npar.eq.8) dcmodel=.true.
	dcmod=dcmodel
	   
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
	
!		imess=gmdisplaymessagebox('','Now start fitting (if you already opened the file)',ginformation,gok)
		
		callid=2043
		goto 2
		else
			if( callid.eq.2037) then
				call gmremovewindow(iec50)
				fixec50=.false.
				nfixec50=0
				i50=0
				j50=0
				i502=0
				j502=0
				goto 1
			endif
			val(1)=gmenqvaluesetting(ival(1))
			ec501=dble(val(1)*1.e-6)
			val(3)=gmenqvaluesetting(ival(3))
			
			val(4)=gmenqvaluesetting(ival(4))
			xqlo=dble(val(3))
			xqhi=dble(val(4))
			val(10)=gmenqvaluesetting(ival(10))
			penfac=dble(val(10))
			
			if(nlig.gt.1) then
				val(2)=gmenqvaluesetting(ival(2))
				conc_ec1(2)=val(2)*1.e-6
			endif
			if(nfixec50.gt.1) then
				val(11)=gmenqvaluesetting(ival(11))
				ec502=dble(val(11)*1.e-6)
				val(13)=gmenqvaluesetting(ival(13))
				val(14)=gmenqvaluesetting(ival(14))
				xqlo2=dble(val(13))
			    xqhi2=dble(val(14))
				if(nlig.gt.1) then
					val(12)=gmenqvaluesetting(ival(12))
					conc_ec2(1)=val(12)*1.e-6
				endif
			endif
			fixec50=.false.
			do i=1,	ncomboname(1)	
					ifst=gmEnqListEntry(iecctype,i,comboname(1,i))
					if(ifst.eq.2)  goto 2222
			enddo
2222		l=i
			nl1=len_trim(comboname(1,l))
		    cnum0=comboname(1,l)(3:nl1)
		    if(cnum0.eq.'none') then
				nfixec50=0
				fixec50=.false.
			else
		        call chatonos(cnum0,num0,nk)
		        i50=num0(1)
		        j50=num0(2)
			    if(ec501.gt.0.and.xqlo.ge.0.and.xqhi.gt.0.and.i50.gt.0.and.j50.gt.0) then
				fixec50=.true.
				nfixec50=1
				
				m50=IQ(i50,j50)		!OK so define param #
			    m5=m50
				fixec501=.true.
				nfixec501=1
				
				m501=IQ(i50,j50)		!OK so define param #
			   
				call gmSetCellSetting(ini_TxtArray4_2, 1,1,gmString=comboname(1,l))
			    endif
			endif 
		if(fixec50) then
	    do ifix=1,nfixec50
		if(ifix.eq.1) then
		   n=nlvar
		   i=i50
		   j=j50
		   m=m50
		   ec=ec501
		else
		   n=nlvar2
		   i=i502
		   j=j502
		   m=m502
		   ec=ec502
		endif
	    if(discprt) write(7,26) m,i,j,titlep(m),ligname(n),ec*1.d6,penfac
26		format(/,' Rate ',i3,3x,' q(',i2,',',i2,') =',1x,a10,' is constrained to give',/,&
      '  EC50 for ',a10,' = ',g16.6,' micromolar,',/,&
      ' using penalty factor = ',g13.6,' if needed')
		if(nlig.gt.1) then
		   if(discprt) write(7,261)
261		   format('    Concentration of other ligands for EC50 determination')
		   do i=1,nlig
			if(i.ne.n) then
			   if(ifix.eq.1) then
				x=1.e6*conc_ec1(i)
			   else if(ifix.eq.2) then
				x=1.e6*conc_ec2(i)
			   endif
			   if(discprt) write(7,325) ligname(i)(1:10),x
325			   format('   Conc of ',a10,' at which EC50 was determined (micromolar) = ',f9.4)
			endif
		   enddo
		endif
	  enddo		!ifix=1,nfixec50
	endif
			do i=1,	ncomboname(1)	
					ifst=gmEnqListEntry(iecctype,i,comboname(1,i))
					if(ifst.eq.2)  goto 2223
			enddo
2223	    l=i
		    nl1=len_trim(comboname(1,l))
		    cnum0=comboname(1,l)(3:nl1)
		    call chatonos(cnum0,num0,nk)
		    i502=num0(1)
		    j502=num0(2)
			if(fixec50.and.ec502.gt.0.and.xqlo2.gt.0.and.xqhi2.gt.0.and.i502.gt.0.and.j502.gt.0) then
				fixec50=.true.
				nfixec50=2
			
				m502=IQ(i502,j502)		!OK so define param #
			    m5=m502

				call gmSetCellSetting(ini_TxtArray4_2, 1,2,gmString=comboname(1,l))
		
			endif 
			if(fixec50.eq..false.) then
				nfixec50=0
				i50=0
				j50=0
				i501=0
				j501=0
				i502=0
				j502=0
				call gmSetCellSetting(ini_TxtArray4_2, 1,1,gmString=' ')
				call gmSetCellSetting(ini_TxtArray4_2, 1,2,gmString=' ')
				call gmSetCellSetting(ini_TxtArray4_2, 2,1 ,gmString=' ')
				call gmSetCellSetting(ini_TxtArray4_2, 2,2,gmString=' ')
			endif
		endif
		if(iwr1.eq.-1) then
			jkstatus=gmdisplaymessagebox('','Wrong values.Try again!',gstop,gok)
		
		else
			
			if( callid.eq.2031) call gmremovewindow(icell)
			if( callid.eq.2036) call gmremovewindow(iec50)
			n=1
				ncon=models(igraph2)%ncon
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
			
		
	if(isetu.eq.1) call gmsetwidgetstatus(intoggle(5),gchecked)
	if(inipage.eq.-100) then
		callid=-23
		goto 1
	else if(inipage.eq.-9) then
		callid=1881
		goto 2
	else
		callid=1881
		goto 2
	!jkstatus=gmdisplaymessagebox('','Calculate MR/ Fix Paramters',ginformation,gok)
	endif	

     endif
	
	case(2040) !(703) ! fix parameters0
	
			kj=1
			k=models(igraph2)%n
			npar=models(igraph2)%npar
			ncon=models(igraph2)%ncon
			do m=1,models(igraph2)%ncon
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
			do i=1,100		!ditto
			do j=1,100
				IQ(i,j)=0
			enddo
		enddo
		do m=1,models(igraph2)%npar
				i=irate(m)
				j=jrate(m)
				IQ(i,j)=m
				qt(i,j) = ratcons(indrat)%qt(i,j)
		enddo
	
		jset_1=1

		imode=1
		icons=0				!apply constraints (if any)
!***icon should be defined as in DOS getqd2
!         if(imods.eq.imod0.and.neq.gt.0) then
!            print 81,neq,icon
!81          format(
!     &      ' Constraints have been specified for ',i3,' rates:',/,
!     &      ' (1) Apply constraints to both true rates and fitted',/,
!     &      ' (2) Apply constraints only to fitted rates',/,
!     &      ' (3) Apply constraints only to true rates',/,
!     &      ' Option number [',i2,'] = ')
!            call INPUTi(icon)
		icon=1		!for now ***
		if(icon.eq.3) icons=1		!don't apply constraints to fitted rates
		k=models(igraph2)%n
	
		npar=models(igraph2)%npar
		ncon=models(igraph2)%ncon
		ncyc=ratcons(indrat)%ncyc
		do i=1,ncyc
			nsc(i)=ratcons(indrat)%nsc(i)
		enddo
		do ir=1,models(igraph2)%npar
			theta0(ir)=ratcons(indrat)%value(ir)
		enddo
		imode=0	
		call QSET_TRU(QT,theta0,jset_1,conc,npar,irate,jrate,IQ,imode,icons,k)
		ik=0
		m=0
		do m=1,npar
		  QT(irate(m),jrate(m))=theta0(m)
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
			endif
		  endif
		enddo

	do ir=1,models(igraph2)%npar
			if(nfixec50.ge.1) then
			   if(irate(ir).eq.i50.and.jrate(ir).eq.j50) ratcons(indrat)%micro(ir)='EC50 '
				if(nfixec50.eq.2) then
					if(irate(ir).eq.i502.and.jrate(ir).eq.j502) ratcons(indrat)%micro(ir)='EC50 '
				endif
			endif
			if(neq0.eq.0) neq0=neq
			if(neq0.gt.0) then
			do i=1,neq0
				if(irate(ir).eq.ie(i).and.jrate(ir).eq.je(i)) ratcons(indrat)%micro(ir)='Multiplicative'
			enddo
			endif
			if(neq1.gt.0) then
			do i=1,neq1
				if(irate(ir).eq.ie(i).and.jrate(ir).eq.je(i)) ratcons(indrat)%micro(ir)='Additive'
			
			enddo
			endif
		enddo
77	    continue
		do ir=1,models(igraph2)%npar
			do i=1,ratcons(indrat)%ncyc
			    if(obeymr(i)) then
				if(irate(ir).eq.im(i,1).and.jrate(ir).eq.im(i,2)) ratcons(indrat)%micro(ir)='MR'
				endif
			enddo
		!	if(jfix(Ir).eq.1) ratcons(indrat)%micro(ir)='yes'
		enddo
		! here getqd2
79      continue
		call gmSetWidgetStatus(eqfit(1), GSELECTABLE)
		call gmSetWidgetStatus(eqfit(2), GSELECTABLE)
		xtext2='i,j '//char(124)//'Rate name'//char(124)//'Value'//char(124)//'Constraint'//char(124)//'Fix'
		isw=3
		if(isetu.ge.7) isw=4
		
		ncols=5
		nrows=models(igraph2)%npar
		do i=1,7
			iwcell(i)=100
		enddo
		ihcell=24
		if(idestf.ne.5) then
	call gmDefineKeySelectCallback(13,2042)
	!	if(isetu.eq.7) then
	nmod9=imod0
				  call PRNTEC50(QT,conc,k,nlig,nset,npar,ncdep,ndisp0)
	!		endif
	    ec50=ec501
	    if(i501.eq.0) i501=i50
	    if(j501.eq.0) j501=j50
		call text_array(igraph2,Main,form1,Form1_TxtArray1,ncols,nrows,iwcell,&
			ihcell,xtext2,isw,&
			models(igraph2)%name_link,ratcons(indrat)%iconc,ratcons(indrat)%qij,&
			theta0,models(igraph2)%title_model,ratcons(indrat)%titlep,NBOUND,ligname,nlig,&
			ratcons(indrat)%micro,ratcons(indrat)%ligant,jfix,theta0,iformText)
			ncon=models(igraph2)%ncon
		k=models(igraph2)%n
	!	if(isetu.ge.1) call gmsetwidgetstatus(intoggle(3),gchecked)	
	!	isw=0	
		else
		endif
	case(2045)
		call gmremovewindow(Form1(igraph2,isw))	
	case(2042,2044)
	call gmDefineKeySelectCallback(13,0)
	   nfix=0
	   nmr=0
	   do m=1,npar
			if(isetu.ge.7) then
			call gmEnqCellSetting(Form1_TxtArray1(igraph2,4), 3,m ,xs,textcell)
			else
			call gmEnqCellSetting(Form1_TxtArray1(igraph2,3), 3,m ,xs,textcell)
			endif
			theta0(m)=dble(xs)
			indk=indrat
			ratcons(indk)%value(m)=theta0(m)
			 		
		    QT(irate(m),jrate(m))=theta0(m)
			thsav(m)=theta0(m)  !re-save initial guesses in case altered in getqd2
			ratcons(indk)%qt(irate(m),jrate(m))=theta0(m)
			if(isetu.ge.7) then
				call gmEnqCellSetting(Form1_TxtArray1(igraph2,4), 5,m ,r1,textcell)
			else
			call gmEnqCellSetting(Form1_TxtArray1(igraph2,3), 5,m ,r1,textcell)
			endif
		
			if(textcell.eq.'fixed') then
				nfix=nfix+1
				jfix(m)=1
					ratcons(indk)%micro(m)='fixed'
            else
				jfix(m)=0
			endif
				if(isetu.ge.7) then
					call gmEnqCellSetting(Form1_TxtArray1(igraph2,4), 4,m ,r1,textcell)
			else
			call gmEnqCellSetting(Form1_TxtArray1(igraph2,3), 4,m ,r1,textcell)
			endif
			jmic(m)=0
			if(textcell.eq.'MR') then
				nmr=nmr+1
				jmic(m)=1
					ratcons(indk)%micro(m)=textcell

			else if(textcell.eq.'multiplicative'.or.textcell.eq.'Multiplicative'.or.&
			textcell.eq.'Aditive'.or.textcell.eq.'aditive') then
			jcon(m)=1
				ratcons(indk)%micro(m)=textcell
			endif
	   enddo
	   if(callid.eq.2044) then
	   do i=1,k
	   do j=1,k
		QTsav(i,j)=QT(i,j)
	   enddo
	   enddo
	   		imode=0	
		call QSET_TRU(QT,theta0,jset_1,conc,npar,irate,jrate,IQ,imode,icons,k)

	   recheck=.false.
	   call PRNTEC50(QT,conc,k,nlig,nset,npar,ncdep,ndisp0)
	   	call gmsetvaluesetting(ival1a,pop0)
		call gmsetvaluesetting(ival2a,ec50d*1.d6)
		call gmsetvaluesetting(ival3a,pmax)
	   goto 1
	   endif
	   
	   kfit=npar-nfix-neq-nmr	!nmr=# of cycles constrained by mr (.le.ncyc)
	   if(fixec50) kfit=kfit-nfixec50
	   ik=0		!use to check vs kfit
	   do m=1,npar
				i=irate(m)
				j=jrate(m)
				QT(i,j)=theta0(m)
				ratcons(indrat)%qt(i,j)=qt(i,j)
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
				thetaf(ik)=theta0(m)
			!	thetaf(ik)=QT(i,j)
				
				jfix1(ik)=0
			endif
			endif
		enddo
		do i=1,m
			xvreal=theta0(m)
			if(xvreal.gt.0.0000000000000000000) call gmSetCellSetting(ini_TxtArray7, 3,m,gmvalue=xvreal)
		enddo	
			nmod=imodel	
		if(nmod.eq.1.and.npar.eq.10) dcmodel=.true.
		if(nmod.eq.14.and.npar.eq.4) dcmodel=.true.
	if(nmod.eq.9.and.npar.eq.6) dcmodel=.true.
	if(nmod.eq.10.and.npar.eq.8) dcmodel=.true.
	if(nmod.eq.11.and.npar.eq.14) dcmodel=.true.
	if(nmod.eq.29.and.npar.eq.14) dcmodel=.true.
	if(nmod.eq.33.and.npar.eq.20) dcmodel=.true.
	if(nmod.eq.34.and.npar.eq.12) dcmodel=.true.
	if(nmod.eq.35.and.npar.eq.16) dcmodel=.true.
	if(nmod.eq.36.and.npar.eq.16) dcmodel=.true.
	if(nmod.eq.37.and.npar.eq.18) dcmodel=.true.
	if(nmod.eq.38.and.npar.eq.18) dcmodel=.true.
	if(nmod.eq.39.and.npar.eq.18) dcmodel=.true.
	if(nmod.eq.40.and.npar.eq.20) dcmodel=.true.
	if(nmod.eq.41.and.npar.eq.8) dcmodel=.true.
	dcmod=dcmodel
	   
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
	
!		imess=gmdisplaymessagebox('','Now start fitting (if you already opened the file)',ginformation,gok)
		
		callid=2043
		goto 2
		do i=1,6
			itypeval(i)=0
		enddo
		call value_table(Main,ivwin,6,vtext,ival,val,2043,itypeval)
    case(2043)
    
		if(isetu.ge.7) then
		call gmRemoveWindow(Form1(igraph2,4))
		else
		call gmRemoveWindow(icell)
		endif
		
	!	call gmRemoveWindow(ivwin)
	 	if(inipage.eq.-100) then
			callid=-23
			if(isetu.eq.7) callid=-101
			if(isetu.eq.8) callid=-102
			goto 1
		else
			imess=gmdisplaymessagebox('','Now start fitting (if you already opened the file)',ginformation,gok)
			if(isetu.eq.7) callid=-101
			if(isetu.eq.8) callid=-102
			goto 1
		endif
	case(2051,2052)
	    icalprev=callid
		if(icallmod.gt.1600.and.icallmod.le.1625) then
		call gmRemoveWindow(Form1(jgraph2-1,2))
		icallmod=0
		endif
		call gmRemoveWindow(Graph1_2(igraph2))
		call gmRemoveWindow(Form1(igraph2,2))
		call gmRemoveWindow(Form1(igraph2,1))
		ibkta=0
		if(ihelpmod.eq.-1) then
			call gmRemoveWindow(imodhelp)
			ihelpmod=0
		endif	
		if(ihelpnew.eq.-1) then
			call gmRemoveWindow(iphelp)
			ihelpnew=0
		endif
			callid=1800+igraph2
		goto 2
    case(2053)
		if(autosim) then
		 if(ksim.eq.-1) then
			imods=imod
			irect=irecq	!for simulated at end of 1st run
			irecq=irqsav	!restore for 2nd run to get fit model
			kAs=models(jgraph)%kA 	!save values for 'true' model. and go round again for fit model
			kBs=models(jgraph)%kB
			kCs=kC
			kDs=kD
			ks=models(jgraph)%n
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
			mtitle11=models(jgraph)%title_model
			ilasts=models(jgraph)%ilast
			jlasts=models(jgraph)%jlast
			nlig=models(jgraph)%nlig
			! ks=kAs+kBs+kCs+kDs
			kcs=ks-kas-kbs
			inion=0
			if(ksim.ne.-1) inipage=-100
			ixmm=models(jgraph)%ix
			plot=.false.
			models(jgraph)%ix=-100
			ibk=1
	!   		call ini_page(main,nset,npar,ixgrid,iygrid,nfileb,curvonly,autosim,ksim,&
	!		readini,initmec,nlig,initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1,ini_Value2_1,ini_TxtArray2,&
	!ini_Value3_1,ini_TxtArray3,ini_Value4_1 ,ini_Value4_2,ini_Value5_1,ini_TxtArray5, &
	!ini_Value6_1 ,ini_Value6_3, ini_Value6_7 ,ini_Value6_8,ini_Value6_9, ini_TxtArray7,&
!	ini_Toggle1_1,ini_Toggle2_1,ini_Toggle3_1,ini_Toggle4_1,ini_Toggle4_2,ini_Toggle5_1,&
!	ini_Toggle6_1,ini_Toggle6_2,ini_Toggle6_4,ini_Toggle6_5,ini_Toggle6_7,ini_Toggle6_8,&
!	ini_Toggle7_1,ini_Toggle7_2,ini_Text7_1,ini_Value7_2,ini_Value7_3,ini_Value7_4,ini_TxtArray4_1,ini_TxtArray4_2 ,&
!	inipage,isetu,ini_Button8_1,ini_Button8_2,ini_Button8_3,ini_Value6_0,ini_Button16_3,&
!	ini_Button16_4,ini_text6_01,ini_panel2,iniyes,inino,ini_text6_02,ini_Value6_12)
			
		!	call draw_model(igraph2,imodel,main,graph1_2,GraphMainPanel1_2,graphics1_2,&
		!	models,plot,ipos,mod_create,initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
		!	models(jgraph)%ix=ixmm
			do i=4,15
			call gmSetWidgetStatus(modelw(i), GSELECTABLE)
	
			enddo
			callid=-2301
			do j=1,2
				call gmSetWidgetStatus(eqfit(j), GSELECTABLE)
            enddo
			goto 1
		 else
			callid=-232
			call gmSetGuiGridMode(Goff)
			goto 1
		 endif
		endif
		inion=0
		nlig=models(jgraph)%nlig
		ixmm=models(jgraph)%ix
		plot=.false.
		models(jgraph)%ix=-100
		ibk=1
		if(ksim.ne.-1) inipage=-100
!		call ini_page(main,nset,npar,ixgrid,iygrid,nfileb,curvonly,autosim,ksim,&
!		readini,initmec,nlig,initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1,ini_Value2_1,ini_TxtArray2,&
!	ini_Value3_1,ini_TxtArray3,ini_Value4_1 ,ini_Value4_2,ini_Value5_1,ini_TxtArray5, &
!	ini_Value6_1 ,ini_Value6_3, ini_Value6_7 ,ini_Value6_8,ini_Value6_9, ini_TxtArray7,&
!	ini_Toggle1_1,ini_Toggle2_1,ini_Toggle3_1,ini_Toggle4_1,ini_Toggle4_2,ini_Toggle5_1,&
!	ini_Toggle6_1,ini_Toggle6_2,ini_Toggle6_4,ini_Toggle6_5,ini_Toggle6_7,ini_Toggle6_8,&
!	ini_Toggle7_1,ini_Toggle7_2,ini_Text7_1,ini_Value7_2,ini_Value7_3,ini_Value7_4,ini_TxtArray4_1,ini_TxtArray4_2 ,&
!	inipage,isetu,ini_Button8_1,ini_Button8_2,ini_Button8_3,ini_Value6_0,ini_Button16_3,&
!	ini_Button16_4,ini_text6_01,ini_panel2,iniyes,inino,ini_text6_02,ini_Value6_12)
		
!		call draw_model(igraph2,imodel,main,graph1_2,GraphMainPanel1_2,graphics1_2,&
!		models,plot,ipos,mod_create,initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
	
		models(jgraph)%ix=ixmm
		do i=4,15
						call gmSetWidgetStatus(modelw(i), GSELECTABLE)
	
					enddo
					do j=1,2
						call gmSetWidgetStatus(eqfit(j), GSELECTABLE)

					enddo
		callid=-2302
		goto 1
			
case(2055)
    if(autosim.and.ksim.eq.-1) then
    call gmRemoveWindow(Graph1_2(igraph2))
		call gmRemoveWindow(Form1(igraph2,2))
		call gmRemoveWindow(Form1(igraph2,1))
		ibkta=0
		if(ihelpmod.eq.-1) then
			call gmRemoveWindow(imodhelp)
			ihelpmod=0
		endif	
		if(ihelpnew.eq.-1) then
			call gmRemoveWindow(iphelp)
			ihelpnew=0
		endif
    callid=3301
    goto 1	
    endif
	end select

1 end