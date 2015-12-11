subroutine fitting(callid,igraph2,main,imainpanel,Form1,Form1_TxtArray1,theta,jfix1,tint,ampl,iprops,&
     Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,XAF,XFA,QexpQA,QexpQF,&
     alpha2,beta2,aloglik,kab,kAm,kFm,km,irt,nset,idestf,eqfit,nsim,&
	 abort,endsim,neq,nsetq,thsav,titlep,infopanel,infoind,infolength,&
	 indk,ratcons,miplot0,mtoggle,tresol,iplot_Toggle0,elmax,var,covar,badpar,cor,&
	 den,nev,smin,inifile,inifile0,pfileb,pfiles,qfilem,qmec,nsims,thtrue,&
	 simfile,simfile1,cdate,ctime,npars,mtitles,imods,nevals,ix1sav,iy1sav,iz1sav,nsims0,apfile)

! subroutine to deal with fitting
! callid 4011 to 4499
	use DFLIB
!	use dfwin
	use menu_f90
	use gino_f90
!! IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	
	PARAMETER kAx=60 	!max number of open states for following
	character*100 xtext
	character*40 mtitle1
	character*74 mtitles
	integer iwcell(10),nevals(nsims0+nsim)
	integer mToggle(10)
	integer :: Form1(25,4)
integer :: Form1_Panel1(25,4)

integer :: Form1_Txtarray1(25,4),iformText(25,4)
	CHARACTER*80 titlem
	character ch
  	TYPE (FILE$INFO) info
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
		integer icyc(50,50)
		integer im(50)
		integer jm(50)
	END TYPE rate_constant
	
	type(rate_constant) ratcons(500)
	character*60 textyn(20)
    integer iyes(20),ino(20),iyesno(20,2)
	logical nodata,dprt
	logical monot,flat,decline
	character*256 string						!for HMAT
	real*8 var(200),covar(200,200),elmax,cor,den			!for HMAT
	logical badpar(200)							!for Hmat
	real*8 delmin,smin	!for simplex
	real*8 hjclik	!function now real*8
	real*8 theta(200),thetaf(200)
	real*8 stpfac,confac,resfac,errfac
	character*74 rtitle,rtitles(500)
	real*8 eigAAsav(kAx,10),eigFFsav(100,10)
	character*60 vtext(60),ctimes,ctimef
	real*8 absmin,thmin(200),step(200)
	real*4 tcrit(10),val(50)
	logical burst(10),chsvec(10),badend(10)
	real*8 tres,sum,assmax,fmax,tcrit1,one
	common/tty/ittypanel,itty
	real*4 tint(nd1,nd2),ampl(nd1,nd2)
	integer*1 iprops(nd1,nd2)
	integer nint(10),il(10),ival(50),itypeval(50)
	real*4 conc(10,10),tresol(10)		!to hold concentration ith ligand, jth set
	logical open,bad,bad1,good,first,oneset,pcalc
	logical btest,autosim,dcmodel,apfile
	logical setbad(2,10)
	real*8 theta0(200),elmset(nset,nsims0+nsim)
	real*8 setlik(10)		!likelihood for each data set (e.g.conc)
	integer ic(2,200),jcon(200),jmic(200),jbad(200)
	real*8 QT(100,100),QD(100,100)	!QD has conc in
	integer jfix(200),jfix1(200)
	integer IQ(100,100),irate1(200),jrate1(200),iqf(100,100)
	integer NSC(50),IM(50,100),JM(50,100),IX(100),JX(100)
	INTEGER AA,AF,FA,FF
	real*8 Z00A(kAm,kFm,km),Z10A(kAm,kFm,km),Z11A(kAm,kFm,km)
	real*8 Z00F(kFm,kAm,km),Z10F(kFm,kAm,km),Z11F(kFm,kAm,km)
	logical logfit,logsav,silent
	real*8 QEXPQA(kFm,kAM),QEXPQF(kAm,kFm)
	real*8 XAF(kAm,kFm,kAm),XFA(kFm,kAm,kFm)
	real*8 QAF(kAx,100),QFA(100,kAx),QAA(kAx,kAx),QFF(100,100)
	real*8 EXPQA(kAx,kAx),EXPQF(100,100)
	real*8 WA(kAx,kAx),WF(100,100)
	real*8 alpha2(kab),beta2(kab),aloglik(kab)
	common/absave/nab,jalpha,jbeta		!in hjclik, hjcdisp
	real*8 eigen(100),eig1
	real*8 g00A(100),g10A(100),g11A(100),g00F(100),g10F(100),g11F(100)
	real*4 w(100)
	real*8 Q1(100,100)
	real*8 rowA(100),rowF(100)
	real*8 phiA(1,100),phiF(1,100)
	real*8 phibst(1,100),endbst(100,1)
	real*8 ucol(100,1)
	real*8 rootA(100),rootF(100)
!	real*8 s1A(100),s2A(100),s1F(100),s2F(100)	!init guesses for roots
!	real*8 rootAsav(kAx,10),rootFsav(100,10)
!	real*8 s1Asav(kAx,10),s2Asav(kAx,10),s1Fsav(100,10),s2Fsav(100,10)
	character*33 ascfil
	type (GACTION) :: actlst
character*40 namelink(100)
	type (Gwidget) :: widget
!	common/rootsav/rootAsav,rootFsav
	real*8 fcomp(10)
	common/lcomp/nset1,fcomp				!for SIMPLEX
	common/nblk/ngp(10),an(10),nscal(10),first    !ngp(j) etc for set j
	COMMON/HJCBLK/Nint,tcrit,burst,chsvec,badend	!for HJCLIK/hjcdisp
	common/setblk/oneset,iset	!for HJCLIK,DISP to specify one set
	common/CBLK/nset0,conc,jsetlast		!for HJCLIK
	character*11 cstring,cnum(10),cnum0
	character cnum5*5,cnum51*5
	character*22 cnum1,cmodel
	character*10 t1(100),titlep1(100),tij(200)
	character*10 titlep(200)
	character text*80
	character*10 qij(100)
	real*8 tresd(10)
	integer :: callid,eqfit(3),iexcop(10)
	real*4 gaplo(10),gaphi(10)
	integer nskip(10)
	logical excop(10)
	real*4 tcbad(2,10)	!1=shut, 2=open
	integer ncbad(2,10)
	integer nbound(100,10)
	character*10 str1,str2
	logical repeat,simulat,sim,restart
	character*40 simfile,simfile1	!file names for output of simulation
	character*11 cdate,ctime
	logical newfile,discprt,plot,readini,fixec50,endsim,abort,penalty
	logical bindo,nofit,prtec50,stable
	character*20 ligname(10),kname
	common/eigsav/eigAAsav,eigFFsav
	real*8 thtrue(200),thsav(200),thtrue1(200)
	!common/detw2/tres,km1,nerr
	COMMON/MPAR/NCYC,NSC,IM,JM
	COMMON/QPAR/NCON,IC
	common/mr3/extcyc
	logical obeymr(50),automr(50)
	common/mr/obeymr,automr
	common/mr1/isetmr(50)
	common/ir/irate(200),jrate(200)
	real*8 ec502,xqlo2,xqhi2
	real*8 ec50,pmax,ec501,xqlo,xqhi,ec5
	common/LIG/nligsav,IL
!	common /a/ mabel(100)		!common needed for CIRCUIT.for
!    common /b/ matrix(100,100),n  !common needed for CIRCUIT.for
!    COMMON/EBLK/NEQ,IE,JE,IF,JF,EFAC
!	COMMON/indblk/nsetq,ieq,jeq,ifq,jfq,efacq(200) 
	common/ec/fixec50,nmod9,ec50,i50,j50,m50,prtec50,xqlo,xqhi,dcmodel	!nmod=imod0 (already in commom/model)
	common/ec2/monot,nlvar,vhold,vref,cfacd,dgamma !for ec50_hjc, qset_hjc
	common/ec3/ligname		!for qset_hjc
	common/KBLK/kA,kB,kC,kD
	common/QDBLK1/QT,QD
	common/det4/ndscale,idsign
	common/ec4/nfixec50,nlvar2,ec502,i502,j502,m502,xqlo2,xqhi2,conc_ec1,conc_ec2	 !for 2nd fixed ec50
	common/detw1/QAA,QAF,QFF,QFA,EXPQF,EXPQA   !for DETWA,DETWF,dARSdS -now param
	common/detw3/WA,WF			  !for DETWA,DETWF -now params
    common/QDBLK2/npar,IQf,irate1,jrate1,nlig   !hjclik, qset_hjc only
	common/detw2/tres,km1,nerr				   !for DETWA,DETWF
	common/pen/penalty,penfunc,penfac 
	common/dp/discprt
    real*8 ratemax
	common/amax/assmax,icdep(200),RATEMAX
	common/cpar/ncdep,IX,JX,x
	common/reset/absmin,thmin,step			!for HJCLIK
	common/eval/neval1,niter	!neval1=neval, for print in hjcasymp
	common/logf/logfit
	character *10 statname(100)	
	integer ICout(2,200),icin(2,200)	!IC for connections to be NOT IN tree
	integer icspec(2,200),iplot_toggle0(10)	!IC for specified connections
	integer iedge(2,200)	!IC for specified connections
	real*8 det
	integer*2 istas
	common/extras/thtrue1
	COMMON/determ/det			!for matinv
		common/resblk/tresd
	common/perr/nerr2
	integer Jtree(100,100)
	common/deb/idebug,idebug1
	character*10 titpfree(200) 
	common/tpf/titpfree,iptit
	common/plotopt/ipl1,ipl2,ipl3,ipl4,ipl5,ipl6,icprev
	real*8 rcrit,gfac1,gfac2
	common/stab/stable,nstab1,gfac1,nstab2,gfac2,istab	!for hjcfit
real*4 conc_ec1(10),conc_ec2(10)
!	common/inroots/s1Asav,s2Asav,s1Fsav,s2Fsav
	common/mech_param/k,kf,nsub,kstat,ncon1,statname,icspec,icout,&
		jcon,ncin,nd1,nd2,nstate,ndim,iedge,jtree,jmic,irate2(200),jrate2(200),&
		neq0,neq1,nfix,kfit,fac,xs,&
		rtitles,iq,theta0,jfix,nfileb(10)
		common/deb2/idebug2
	logical curvonly
	common/iniset/nodata,nofit,autosim,curvonly
common/rand/ixr1,iyr1,izr1
common/ranlast/ixlast,iylast,izlast
		integer jfixsav(100),iparsav(10)
	integer nbad(10),nbad1(10)
	real*4 tresolb(10)
	real*8 tres1

	character*1 ans,UC,ans1,ans2,ans3,ans4,ans5
	character*2 ndev
	real*4 ylo(20),yhi(20)		!up to 20 gap ranges

	logical excopen
	
	logical slopsch,checkgrp,grouped
	logical onechan,useprim
	

	integer isbad(20,10),iebad(20,10)	!for bad bits (see stabplot)
	character*33 pfileb(20,10)	!path names for .SCN files
	character*40 qfilem,qfile
	character*60 pfiles(20,10),qmec
	integer kfile(20,10)
	integer NSC2(50),IM2(50,100),JM2(50,100)	!up to 50 cycles
	integer IE(200),JE(200),IF(200),JF(200)
	logical allmr
	real EFAC(200)
	character*40 inifile
	logical fix502,present
	real*4 xs
	integer isgood1(10),iegood1(10) 
	character*8 ftype
	common/grp1/slopsch,checkgrp,grouped,rcrit,ngpcheck
	character*40 :: inipath='hjcfit.ini',inifile0
	character*40 :: inidef='*.ini'//char(124)//'Initialization file (INI)'//&
	char(124)//'*.*'//char(124)//'All Files'
	character*60 pathdata,pathmec
common/sets/setlik,nset2
real*8 thetval(npar,nsims0+nsim)
	integer nintval(nset,nsims0+nsim)
	integer ixval(nsims0+nsim)
	integer iyval(nsims0+nsim)
	integer izval(nsims0+nsim)
	
	real*8 elmval(nsims0+nsim)
	
	real*8 ec50val(nsims0+nsim)
	nset1=nset
2 continue

select case(callid)

	case(4011)
		 if(icprev.eq.-102.or.curvonly) nofit=.true.
	     nligsav=nlig
		
	!	 textyn(1)='Save settings to ini file?'
	!	 n=1
		 if(.not.nofit) then
	!	imes=gmdisplaymessagebox('','Print iterations to printout file?',gquestion,gyesno)
	!	if(imes.eq.gyesbutton) then	
	!		dprt=.true.
	!		else
	!		dprt=.false.
	!		 endif
		 textyn(1)='Print iterations to printout file?'
		 n=1
		 iyesno(1,1)=0
		 iyesno(1,2)=1
		 call gmDefineKeySelectCallback(13,4021)
		  call yesno_table(main,iynwin,n,textyn,iyes,iyn,iyesno,4021)
		 else
	!	 iyesno(1,1)=0
	!	 iyesno(1,2)=1
	!	  iyesno(2,1)=0
	!	 iyesno(2,2)=1
		
		callid=4020
		goto 2
		endif
	case(4021)
	call gmDefineKeySelectCallback(13,0)
	    istat1=gmenqtoggleswitch(iyes(1))
		dprt=.false.
		if(istat1.eq.gon) dprt=.true.
		
		 call gmremovewindow(iynwin)
		 	callid=4020
		  goto 2
	case(4020)
		 first=.true.		!get openings/group etc in HJCLIK
		 oneset=.false.	!
		 prtec50=.true.		!print initial ec50 (whether fixec50 or not)
		 if(autosim) then
			prtec50=.false.
			dcmodel=.true.
		endif
		 kf=k-ka
		 kc=kf-kb
		 nerr=0
		 	if(discprt) write(7,112)
112		format(/,' For initial guesses: ')
!		 idebug=-10
		 elmax=HJCLIK(1,kfit,theta,tint,ampl,iprops,nd1,nd2,Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,&
         XAF,XFA,QexpQA,QexpQF,alpha2,beta2,aloglik,kab,kAm,kFm,km,irt,npar,IQf,irate1,jrate1,nlig)
	     prtec50=.false.
!!		 dcmodel=.true.
         if(discprt) write(7,1420)
1420	   format(/,' EQUILIBRIUM CALCULATIONS FOR INITIAL GUESSES')
	       call PRNTEC50(QT,conc,k,nlig,nset,npar,ncdep)
		 idebug=0
         if(discprt) write(7,1121) -elmax
1121	 format(' log(likelihood) = ',g14.7)
		 do j=1,nset
      		if(discprt) write(7,12) j,fcomp(j),ngp(j),an(j),nscal(j)
12	   		format(' Set ',i3,': likelihood = ',g14.7,/,&
     		1x,i4,' groups: mean no of openings/group = ',g13.6,/,&
			' (likelihood scaled down ',i4,' times by 1.e-100)')
		 enddo
		 if(discprt) write(7,113)
113	   format(/,'  Note: if a particular set of parameter values causes the',/,&
		' likelihood for a group to appear <0, the log(lik) for this',/,&
		' group is taken as zero, thus penalising these parameters',/)
		if(discprt) write(7,1351) kfit,(titpfree(i)(1:8),theta(i),i=1,kfit)
1351	format(/,' Fit ',i3,' parameters: initial theta() = ',20(/,5g13.6))
	
		write(string,fmt='(a20,g13.6)') 'Likelihood:',-elmax
		 CALL GMSETTEXTSETTING(ITTY,string)
		if(nerr.ne.0) then
			imes=gmdisplaymessagebox('','Error. Likelihood set to 0; try new params',gstop,gok)
		!	call gmremoveWindow(iasymp)
			goto 99
		 endif
		do j=1,nset
			call intconv(j,cnum(1))
				write(string,fmt='(a5,i3,a20,g13.6)') 'Set:',j,'Likelihood:',fcomp(j)
			CALL GMSETTEXTSETTING(ITTY,string)
			  write(string,fmt='(a10,i6)') 'Groups:',ngp(j)
		  CALL GMSETTEXTSETTING(ITTY,string)
		  	   write(string,fmt='(a30,g13.6)') 'mean no of openings/group :',an(j)
		   CALL GMSETTEXTSETTING(ITTY,string) 
	!		ival=gmCreateValueEntry(iasymppanel,7, irt, 5, 1, nscal(j), 6, 0, gdisplay,gmVpos=GTOP)
	
			call realtoch(fcomp(j),cnum(2),11)
			call intconv(ngp(j),cnum(3))
			call realtoch(an(j),cnum(4),11)
			call intconv(nscal(j),cnum(5))
			  write(string,fmt='(a30,i10,a15)') 'likelihood scaled down:',nscal(j),' times by 1.e-100'
		   CALL GMSETTEXTSETTING(ITTY,string) 
			
!			text='Set '//cnum(1)(1:3)//':likelihood='//cnum(2)//','//cnum(3)(1:5)//&
!			' groups: mean no of openings/group = '//cnum(4)//' (likelihood scaled down '//&
!			cnum(5)(1:4)//' times by 1.e-100)'
		enddo
		do j=1,kfit
			call intconv(j,cnum(1))
			reale=sngl(theta(j))
			call realtoch(reale,cnum(2),11)
				  write(string,fmt='(a10,i3,a5,g13.6)') 'Theta(',j,')=',reale
		   CALL GMSETTEXTSETTING(ITTY,string) 
		
		enddo
		call gmSetWidgetStatus(eqfit(2), GSELECTABLE)
		if(autosim.and.idestf.eq.5) then
			idestf=0
			callid=4015
			ik=0
			do m=1,npar
			if(jfix(m).eq.0.and.jcon(m).eq.0.and.jmic(m).eq.0) then
			if(.not.(fixec50.and.m.eq.m50)) then
			ik=ik+1
			theta(ik)=thsav(m)
			endif
		 endif
		enddo
			goto 2
		endif
		if(icprev.eq.-102.or.curvonly) then
			callid=4400
			icprev=0
		!	idestf=2
			nofit=.true.
			goto 2
		endif
!!!		 imess=gmdisplaymessagebox('','Skip fit and plot initial guesses?',gquestion,gyesno)
		 	callid=4012
			goto 2
!!!		 if(imess.eq.gyesbutton) then
!!!			callid=4400
!!!				idestf=2
!!!				nofit=.true.
!!!			goto 2
!!!			 else
!!!			callid=4012
!!!			goto 2
!!!		 endif
case(4012)
    !call gmremoveWindow(iasymp)
	errfac=1.d-3
	delmin=-1.d0		!do not use delmin for convergence

!	imes1=gmdisplaymessagebox('','Use log(rate constant) in Simplex',gquestion,gyesno)
	logsav=.true.
!	if(imes1.eq.gyesbutton) 	logsav=.true.


	if(x.gt.1.e-10) errfac=dble(x)
	confac=0.5d0		!parameter for simplex3 -for bad guesses
	stpfac=0.2d0		!better for bad guesses?
	stpsav=stpfac
	if(logsav) stpfac=5.d0
	if(logsav) then
	   stpsav=dlog(stpfac)
	endif
	resfac=10.0d0		!better for bad guesses?
	irestrt=3
	errfac=0.001
	
	iconv=1
	iconvsav=1

	n=6
	do i=1,10
		val(i)=0.0
		itypeval(i)=0
	enddo
	val(1)=sngl(stpfac)
	val(2)=sngl(confac)
	val(3)=sngl(resfac)
	val(4)=irestrt
	itypeval(1)=2
	itypeval(2)=2
	itypeval(3)=2
	itypeval(4)=0
	itypeval(6)=0
	itypeval(7)=0
	itypeval(5)=6
	val(6)=10
	val(5)=sngl(errfac)
	vtext(50)='Settings for Simplex'
	vtext(1)='Initial step size factor'
	vtext(2)='Simplex contraction factor (0-1)'
	vtext(3)='Restart step size=resfac*critstep: resfac'
	vtext(4)=' Limit number of restarts '
	
	vtext(5)='Relative error [0.001]'
	vtext(6)= 'Display every Nth estimate '
	vtext(7)= 'Print every Nth estimate '
	icall=4013
	
	call gmDefineKeySelectCallback(13,4013)
	call val_table(Main,ivwin,n,vtext,ival,val,icall,itypeval,iplot_Toggle0)
	do i=1,10
		itypeval(i)=0
	enddo
	
case(4013)
call gmDefineKeySelectCallback(13,0)
	do i=1,6
		val(i)=0.0	 
		val(i)=gmenqvaluesetting(ival(i))
		
	enddo
	ista=gmenqtoggleswitch(iplot_Toggle0(1))
	if(ista.eq.gon) then
		logsav=.true.
	endif
	stpfac=	dble(val(1))
	if(logsav) then
	   stpsav=dlog(stpfac)
	endif
	confac=dble(val(2))
	resfac=dble(val(3))
	irestrt=int(val(4))
	ndisp=int(val(6))
	nprint=int(val(7))
	err4=0.0
	err4=val(5)
	errfac=err4
		call gmremovewindow(ivwin)
      callid=4015
	  goto 2
case(4015)
	nevm=-20000
	abort=.false.
	logfit=logsav 	!logfit false except when hjclik called from simplex
	if(logfit) then
	   stpfac=stpsav
	   iconv=iconvsav		!restore
	   do i=1,kfit
		theta(i)=dlog(theta(i))
	   enddo
	endif

	
!	ch=getcharqq() 
	call gmflushcallbackqueue()
	if(autosim) then
		dpsav=discprt
		if(nevm.lt.0) discprt=.false.	!switch off during fit
		call gmsettextsetting(itty,'Starting simulation')	
	endif
	if(discprt) write(7,111) stpfac,confac,resfac,irestrt,ndisp,errfac
111 format(/,' Initial step size factor=',f15.9,/,' Simplex contraction factor (0-1)=',f10.6,/,&
    ' Restart step size=resfac*critstep: resfac=',f15.9,/,' Limit number of restarts=',i5,/,&
	' Print every Nth estimate=',i5,/,' Relative error=',f10.6,/)
 
	first=.true.
	idebug=0
!	if(.not.silent) call gmsettextsetting(itty,' F1 key to ABORT')
	call TIME(ctimes)
    call gmsettextsetting(itty,'Simplex started at:'//ctimes)
	if(discprt) write(7,131) ctimes
131 format(/,' Simplex started at:',a10)
!	istas=0
!	istas=GetAsyncKeyState(27)
!	if(istas.ne.0) stop
    if(ndisp.eq.-1000) silent=.true.
	itinfoP=gmcreatetextentry(imainpanel,1,2,10,1,&
						'TO PAUSE FIT: Press "P" or "p"',&
						100, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              			gmVpos=GTOP, gmExpand=GOFF)
	itinfoC=gmcreatetextentry(imainpanel,12,2,14,1,&
						'TO ABORT FIT: Press "ESC" or "Q" or "q"',100, GDISPLAY, &
             			gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=0, gmBack2Col=0, gmTextCol=2, &
              			gmVpos=GTOP, gmExpand=GOFF)
!	ivb=gmCreatePushButton(imainpanel,1,3,24,1,&
!		'TO PAUSE FIT: PRESS THE LEFT MOUSE BUTTON ON THE PROGRAM TITLE BAR',&
!		  gmType=GSTANDARD, gmSize=80,gmTextCol=2, &
!          gmVpos=Gtop, gmHpos=Gleft,gmExpand=GOFF,gmCallback=4320)
	     callid=4300
		 goto 2
case(4300:4320)
	

	call SIMPHJC(main,kfit,THETA,stpfac,errfac,nev,nevm,&
      smin,HJCLIK,Ndisp,jfix1,delmin,confac,irestrt,resfac,iconv,&
      tint,ampl,iprops,nd1,nd2,Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,&
      XAF,XFA,QexpQA,QexpQF,alpha2,beta2,aloglik,kab,autosim,nsims,kAm,kFm,km,&
	  irt,npar,IQf,irate1,jrate1,nlig,dprt)
	


	if(logfit) then
	  do i=1,kfit
		theta(i)=dexp(theta(i))	!non-log from here on
	  enddo
	  logfit=.false.		!so later calls to hjclik don't expect log rates
	 endif
	 call TIME(ctimef)
!!	  call gmsettextsetting(itty,'Simplex started at:'//ctimes)
	 call gmsettextsetting(itty,' and Simplex finished at:'//ctimef)
!	 if(discprt) write(7,131) ctimes
	 if(discprt) write(7,132) ctimef
132 format(/,'  and Simplex finished at:',a10)
     if(autosim) discprt=dpsav	!switch back on

	endsim=.false.

	if(abort.and.autosim.and.nsims.lt.nsim+nsims0) then
	   call intconv(nsims-1,cstring)
       imes=gmdisplaymessagebox('',cstring(1:5)//' sims completed. End the simulations now?',&
	   gquestion,gyesno)
	   if(gyesbutton) endsim=.true.
	endif
	 call intconv(nev,cstring)
	call gmsettextsetting(itty,'  number of evaluations = '//cstring(1:5))
!	do j=1,kfit
!		write(string,fmt='(a8,i3,i3,g13.6)') 'theta(',j,')=',theta(j)
!			call gmsettextsetting(itty,string)
!	enddo
    if(discprt) write(7,24) nev,(theta(j),j=1,kfit)
24	format('  number of evaluations = ',i7,' theta = ',20(/,5g13.6))
     WRITE(STRING,fmt='(a20,g14.7)') ' Max log(likelihood) = ',-smin
	
	 call gmsettextsetting(itty,string)
      if(discprt) write(7,23) nev,-smin
23	format(&
     ' Number of evaluations = ',i8,' Max log(likelihood) = ',g14.7,/,&
     '  Press any key to continue')
	callid=4400
	goto 2
case(4095) ! ini file
	CALL gmFileBROWSER(iniFILE,inipath,inidef,gmBrowseType=1)
			 IF(iniFILE.ne.' ') then
			 nl=nblank1(inipath)
			 inifile=inipath(1:nl)//'\'//inifile
			 call gmsettextsetting( initialw_Text5,inifile)
			 inifile0=inifile
			 endif
		INQUIRE (FILE=iniFILE,EXIST=PRESENT) 

	if(PRESENT) then
	    
		ihandle=FILE$FIRST
		length = GETFILEINFOQQ(inifile, info, ihandle)
		nLEN=info%length
	    if(nlen.lt.100) then  
		imes=gmdisplaymessagebox('','File does not exit.Create a new file',ginformation,gok)
		iftype=100
		IRECL=10240
		OPEN(UNIT=19,FILE=iniFILE,STATUS='UNKNOWN',&
        ACCESS='DIRECT',FORM='UNFORMATTED',RECL=irecl)
		 write(19,rec=1) iftype
		
		close(unit=19)
		endif
	else
		imes=gmdisplaymessagebox('','File does not exit.Create a new file',ginformation,gok)
		iftype=100
		IRECL=10240
		OPEN(UNIT=19,FILE=iniFILE,STATUS='UNKNOWN',&
        ACCESS='DIRECT',FORM='UNFORMATTED',RECL=irecl)
		 write(19,rec=1) iftype
		
		close(unit=19)
	endif
		call write_ini(inifile,pfileb,nfix,jfix,neq,IE,JE,EFAC,&
        IF,JF,jcon,IM2,JM2,jmic,ndisp,irecq,ans1,ylo,yhi,nrange,idiskq,&
        nfileb,kfile,nset,conc,tcrit,burst,&
        idatyp,qfile,imodold,setbad,tcbad,onechan,&
        nbad1,isbad,iebad,tresolb,ans3,ans4,nlvar,&
        fixec50,ec50,i50,j50,m50,xqlo,xqhi,kAm,kFm,&
        chsvec,ncyc2,nsc2,qfilem,nsim,irect,logsav,imodolds,&
        badend,excop,gaplo,gaphi,dcmod,nlen, &
     	nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,useprim,pfiles,qmec,&
		nmr,nlig,conc_ec1,conc_ec2,iftype)
    callid=4020
	goto 2
case(4400) ! prepare plot
    jset=1
	call QSET_HJC(-1,jset,theta,QT,QD,kfit,k,irt,npar,IQf,irate1,jrate1,nlig )	!QT,QD no longer in common/qblk
	if(autosim) then	!set theta0
	   jset=1
	   imode=1	 !so sets mr in QT and outputs theta0 with mr set
	   icons=0				!apply constraints (if any)
	   if(icon.eq.3) icons=1	!don't apply constraints to fitted rates
	   call QSET_TRU(QT,theta0,jset,conc,npar,irate,jrate,IQ,imode,icons,k)
	   do m=1,npar
		i=irate(m)
		j=jrate(m)
		theta0(m)=QT(i,j)
	   enddo
	   elmax=-smin
	   if(nsims.ge.1) then
			callid=4023
			do m=1,npar
			thtrue1(m)=thtrue(m)
	   if(autosim.and.imod0.eq.29.and.imods.eq.39.and.m.gt.2) then
		th=thtrue(m+2)
		thtrue1(m)=thtrue(m+2)
		
	   endif
		enddo
			goto 2
		endif  !and then skip print (or, for now, get printout of final params?)
	endif

	call ATYPD(QT,'  QT    ',k,k,km,km)
	 textyn(1)='Type Q matrix for each data set?'
		 n=1
		 iyesno(1,1)=0
		 iyesno(1,2)=1
		 call gmDefineKeySelectCallback(13,4022)
		 call yesno_table(main,iynwin,n,textyn,iyes,iyn,iyesno,4022)
	case(4022)
	call gmDefineKeySelectCallback(13,0)
	    istat1=gmenqtoggleswitch(iyes(1))
		call gmremovewindow(iynwin)
			if(istat1.eq.gyesbutton) then
	   do j=1,nset
		call QNEW_HJC(QT,j,conc,QD,ncdep,nlig,IL,IX,JX,k,km,km)
      
	      if(discprt) write(7,3351) j
3351		format(/,' Set ',i3,': Q matrix at concentration(s) ')
		do i=1,nlig
      	 
		   if(discprt) write(7,335) conc(i,j)*1.e6,ligname(i)
335		   format(3x,g13.6,' micromolar of ',a20)
		enddo
	      call ATYPD(QD,'  QD    ',k,k,km,km)
	   enddo
	endif
if(curvonly) then
	   
		callid=4023
	   goto 2		!save ini?
	endif
    if(discprt) write(7,137)
137	format(/,' FINAL VALUES OF RATE CONSTANTS')
	do j=1,nset
	   tres1=1.d6*sngl(tresol(j)) !tresd(j) ?
	   tres1=sngl(tresol(j))
	   if(nodata) then
		burst(j)=.false.
		chsvec(j)=.false.
	   endif
      
	   if(discprt) write(7,340) j
340	   format(/,' Set ',i3)
	   if(burst(j)) then
		do i=1,nlig
		  
		   if(discprt) write(7,335) conc(i,j)*1.e6,ligname(i)

		enddo
		if(chsvec(j)) then
		   
      	   if(discprt) write(7,352) tcrit(j),tres1
352		   format(&
     	  '    Analysed in bursts, tcrit (ms) = ',g13.6,/,&
             '    CHS vector used for start and end of bursts',/,&
     	  '    Resolution (microsec) = ',g13.6)
		else
		  
      	   if(discprt) write(7,3521) tcrit(j),tres1
3521		   format(&
          '    Analysed in bursts, tcrit (ms) = ',g13.6,/,&
          '    Steady state vector used for start and end of bursts',/,&
          '    Resolution (microsec) = ',g13.6)
		endif
	   else
		do i=1,nlig
		  
		   if(discprt) write(7,335) conc(i,j)*1.e6,ligname(i)

		enddo
		
      	if(discprt) write(7,341) tres1
341		format(&
     	  '    Analysed as if from one channel only',/,&
     	  '    Resolution (microsec) = ',g13.6)
	   endif
	   if(badend(j)) then
	
 		if(discprt) write(7,4321)
4321		format(/,/&
     ' A bad gap ends a group, but does not eliminate the whole group')
	   else
	
 		if(discprt) write(7,4322)
4322		format(/,&
     ' A bad gap results in elimination of the whole group in which it occurs')
	   endif
	   if(logsav) then
	
      	if(discprt) write(7,3411)
3411		format(' Simplex used log(rate constant) for searching')
	   endif
	   if(setbad(1,j)) then
	
      	if(discprt) write(7,342) ncbad(1,j),tcbad(1,j)
342		format('      ',i5,' shuttings longer than ',g13.6,' ms set bad')
	   endif
	   if(setbad(2,j)) then
	
      	if(discprt) write(7,343) ncbad(2,j),tcbad(2,j)
343		format('      ',i5, ' openings longer than ',g13.6,' ms set bad')
	   endif
	   if(iexcop(j).eq.1) then
	
		if(discprt) write(7,1311) j,nskip(j),gaplo(j),gaphi(j)
1311	      format(/,' SET ',i3,/,1x,i5,' open periods excluded because shut time on BOTH sides',&
     	/,'  is between ',g13.6,' and ',g13.6,' ms')
	   else if(iexcop(j).eq.2) then
	
		if(discprt) write(7,1312) j,nskip(j)
1312	      format(/,' SET ',i3,/,1x,i5,' open periods excluded because they are in group with',&
     	/,' only one open period')
	   endif
	enddo

	kdim=100
	WRITE(STRING,fmt='(5x,a5,5x,a18,10x,a18,5x,a10,5x,a10)') 'nr','initial','final','rate','name'
	call gmsettextsetting(itty,string) 


	if(.not.autosim) then
        
         if(discprt) write(7,1371)
1371	   format(/,23x,'      initial        final')
	else

         
         if(discprt) write(7,1372)
1372	   format(/,27x,' true        initial       final')
	endif
	do m=1,npar
	 i=irate(m)
	 j=jrate(m)
	 theta0(m)=QT(i,j)
	

	 WRITE(STRING,fmt='(5x,i5,5x,g18.6,10x,g18.6,5x,a10,5x,a10)') m,thsav(m),theta0(m),ratcons(indk)%qij(m),&
	 ratcons(indk)%titlep(m)(1:10)
	
	 call gmsettextsetting(itty,string)
	 if(.not.autosim) then
      

         if(discprt) write(7,1210) m,i,j,titlep(m)(1:10),thsav(m),theta0(m)
1210	 format(1x,i4,1x,' q(',i2,',',i2,') = ',1x,a10,2g13.6)
	 else

	   th=thtrue(m)
	   thtrue1(m)=thtrue(m)
	   if(autosim.and.imod0.eq.29.and.imods.eq.39.and.m.gt.2) then
		th=thtrue(m+2)
		thtrue1(m)=thtrue(m+2)
	   endif
    
         if(discprt) write(7,1211) m,i,j,titlep(m)(1:10),th,thsav(m),&
     	theta0(m)
1211	 format(i3,1x,' q(',i2,',',i2,') = ',1x,a10,3g13.6)
	 endif
	 if(jmic(m).eq.1) then
        
         if(discprt) write(7,1212)
1212	   format('& (micro-rev)')
	 else if(jcon(m).eq.1) then
         
         if(discprt) write(7,1213)
1213	   format('& (constrained)')
	 else if(fixec50.and.m.eq.m50) then
         
         if(discprt) write(7,1215) 1.d6*ec50
1215	   format('& (from EC50=',f8.3,' muM)')
	 else if(fixec50.and.m.eq.m502) then
         
         if(discprt) write(7,1215) 1.d6*ec502

	 else if(jfix(m).eq.1) then
         
         if(discprt) write(7,1214)
1214	   format('& (fixed)')
	 endif
	enddo		!end of m=1,npar
      
	 
	if(istab.eq.0) then
	   
	   if(discprt) write(7,231)
231		format(' (Fit NOT ''stable'' at time of convergence)')
	else if(istab.eq.1) then
	   
	   if(discprt) write(7,232)
232		format(' (Fit ''stable'' at time of convergence)')
	   else if(istab.eq.2) then
	else if(istab.eq.2) then
	  
	   if(discprt) write(7,233)
233		format( ' (Fit ''very stable'' at time of convergence)')
	endif
!	Now print also equilibrium constants

	if(discprt) write(7,92)
92	format(/,' Equilibrium constants calculated for fitted rate constants')
	do m=1,ncon
	   isfac1=0		!statistical factors for assoc/dissoc
	   isfac2=0		!statistical factors for assoc/dissoc
	   i=IC(1,m)
	   j=IC(2,m)
	   iopen=i.le.kA
	   jopen=j.le.kA
	   if(iopen.and.(.not.jopen)) then
		if(QT(i,j).gt.1.d-6) then
		   eqK=sngl(QT(j,i)/QT(i,j))
		else
		   eqK=0.0	!not defined
		endif
		i1=j
		j1=i
		m2=IQ(i,j)
		m1=IQ(j,i)
		if(m1.eq.0.or.m2.eq.0) goto 9	  !skip duplicates for INDMOD
		kname='  E   '
	   else if(jopen.and.(.not.iopen)) then
		if(QT(j,i).gt.1.d-6) then
		   eqK=sngl(QT(j,i)/QT(j,i))
		else
		   eqK=0.0	!not defined
		endif
		i1=i
		j1=j
		m1=IQ(i,j)
		m2=IQ(j,i)
		if(m1.eq.0.or.m2.eq.0) goto 9	  !skip duplicates for INDMOD
		kname='  E   '
	   else	!open-open or shut-shut trans (bindoin/unbindoing or isomerisation

		bindo=.false.	!current i,j is open-open or shut-shut isomerisation
		do n=1,ncdep
		   if((ix(n).eq.i.and.jx(n).eq.j).or.(ix(n).eq.j.and.jx(n).eq.i)) then
			bindo=.true.
			jlig=IL(n)	!ligand bound/unbound for current step
		   endif
		enddo
		if(bindo) then	!current i,j is assoc-dissoc reaction
		   nbmax=0 !max number bound for ligand bound in current step
		   do n=1,k
			if(nbound(n,jlig).gt.nbmax) nbmax=nbound(n,jlig)
		   enddo
		   if((ix(n).eq.i.and.jx(n).eq.j)) then	!i->j is assoc
			if(QT(i,j).gt.1.d-6) then
			   eqK=1.e6*sngl(QT(j,i)/QT(i,j))
			else
			   eqK=0.0	!not defined
			endif
			kname=' K (uM)'
			i1=j
			j1=i
			m2=IQ(i,j)
			m1=IQ(j,i)
			if(m1.eq.0.or.m2.eq.0) goto 9	  !skip duplicates for INDMOD
			isfac1=nbmax-nbound(i,jlig)	!assoc
			isfac2=nbound(j,jlig)		!dissoc
		   else		!j->i is assoc
			if(QT(j,i).gt.1.d-6) then
		         eqK=1.e6*sngl(QT(i,j)/QT(j,i))
			else
			   eqK=0.0	!not defined
			endif
			i1=i
			j1=j
			m1=IQ(i,j)
			m2=IQ(j,i)
			if(m1.eq.0.or.m2.eq.0) goto 9	  !skip duplicates for INDMOD
			isfac1=nbmax-nbound(j,jlig)	!assoc
			isfac2=nbound(i,jlig)		!dissoc
			kname= ' K (uM)'
		   endif
		else			!current i,j is open-open or shut-shut isomerisation
		   if(QT(j,i).gt.1.d-6) then
			eqK=sngl(QT(i,j)/QT(j,i))
		   else
			eqK=0.0	!not defined
		   endif
		   i1=j  		!no way to decide which way up is best
		   j1=i
		   m2=IQ(i,j)
		   m1=IQ(j,i)
		   if(m1.eq.0.or.m2.eq.0) goto 9	  !skip duplicates for INDMOD
		   kname=' Kisom '
		endif
	   endif
	   if(discprt) write(7,7) kname,i1,j1,j1,i1,titlep(m1),titlep(m2),eqK
7	   format(1x,a7,' = q(',i2,',',i2,')/q(',i2,',',i2,') = ',&
     	a10,'/',a10,' = ',g13.6)
	   if(isfac1.gt.0) then
		sfac=float(isfac1)/float(isfac2)
!		print 5,isfac1,isfac2,sfac,sfac,kname,sfac*eqK
		if(discprt) write(7,5)isfac1,isfac2,sfac,sfac,kname,sfac*eqK
5		format('    Statistical factor = ',i2,'/',i2,' = ',f7.3,&
		': ',f7.3,'*',a7,' = ',g13.6)
	      isfac1=0		!reset statistical factors for assoc/dissoc
	      isfac2=0
	   endif
9	   continue		!skip to end of loop
	enddo 	!end of equilibrium constants
if(discprt) write(7,142)
!142	 format(/,' EQUILIBRIUM CALCULATIONS FOR FINAL RATES')
!!!	call PRNTEC50(QT,conc,k,nlig,nset,npar,ncdep)
! Print MR check here, after final rates
	i=idebug2	!save
	idebug2=2	!so prints to screen and disk
	call CHECKMR(QD)
	idebug2=i	!restore

! Now print ec50(s) for final parameters AFTER the rate constant estimates

      
      if(discprt) write(7,138)

138	format(' ------------------------------------------------------------')
	if(ncdep.gt.0) then
	 do ifix=1,nfixec50
	   if(ifix.eq.1) then
	    nlv=nlvar
	   else if(ifix.eq.2) then
	    nlv=nlvar2
	   endif
	  
         if(discprt) write(7,142) nlv,ligname(nlv)
142	   format(/,' EC50 calculations for final parameter values',/,&
     ' Equilibrium conc-response curve for ligand # ',i2,' = ',a10)
	   if(nlig.gt.1) then
		do j=1,nlig
		   if(j.ne.nlv) then
		
			if(discprt) write(7,143) j,ligname(j),conc(j,1)*1.e6
143			format('  (conc of ligand #',i2,',  ',a10,' = ',g13.6,' micromolar)')
		   endif
		enddo
	   endif
	   kdim=100
	   if(dcmodel.and.nlig.eq.1) then
		call MODEC50(imod0,QT,npar,kdim,pmax,ec50out1)
		call EC50_HJC(EC50out,curinfs,curmax,concmax,cur0,pop0,pmax,nlv,QT,iset,conc,k,kdim,nerr)
	   else
		call EC50_HJC(EC50out,curinfs,curmax,concmax,cur0,pop0,pmax,nlv,QT,iset,conc,k,kdim,nerr)
	   endif
	   if(nerr.ne.0) then
	
		if(discprt) write(7,590) nerr
590		format('  Error in bisection for EC50: nerr = ',i2)
	   endif
	   
	   if(discprt) write(7,593) ligname(nlv),cur0,pop0
593	   format(' At zero concentration of ',a10,', current (pA) = ',g12.5,', Popen = ',g12.5)
	   if(monot) then
		
      	if(discprt) write(7,594) curinfs,pmax,ligname(nlv)(1:10),ec50out*1.e6
594		format( ' Equilibrium response-concentration curve is monotonic',/,&
     ' Maximum response (pA) = ',g11.4,' Maximum Popen = ',f9.6,/,&
     '    Conc of ',a10,' for 50% of this maximum (EC50) (muM) = ',&
          g11.4,/,' --------------------------------------------------')
	   else	!not monotonic (never true for bindoing curve)
       	if(discprt) write(7,592) curmax,concmax*1.e6,pmax,&
     		ligname(nlv)(1:10),ec50out*1.e6,curinfs
592		format( ' Equilibrium response-concentration curve has maximum.',/,&
          '   Max equilib response = ',g12.5,' pA at ',g12.5,' muM',/,&
          ' Maximum Popen = ',f9.6,/,'   Conc of ',a10,&
         ' for 50% of this max. current (muM) (left of max) = ',&
             g12.5,/,    '   Response at conc -> infinity = ',g12.5,' pA',/,&
       ' --------------------------------------------------')
	   endif
   	   if(dcmodel.and.nlig.eq.1) then
	
		if(discprt) write(7,3461) 1.d6*ec50out1
3461		format(' Check: EC50 by explicit calculation = ',f9.3,' micromolar')
	   endif
	 enddo	!do ifix=1,nfixec50
	endif		!ncdep.gt.0

	if(fixec50) then
	   do ifix=1,nfixec50
		if(ifix.eq.1) then
		   n=nlvar
		   i=i50
		   j=j50

		   ec=ec50
		else
		   n=nlvar2
		   i=i502
		   j=j502

		   ec=ec502
		endif
		print 345,i50,j50,1.d6*ec50
		if(discprt) write(7,345) i,j,ligname(n),1.d6*ec
345	      format('    Q(',i2,',',i2,') calculated from EC50 (',a10,') = ',f9.3,' micromolar')
	   enddo		!ifix=1,nfixec50
	   if(penalty) then
	     
          if(discprt) write(7,344) penfunc
344	      format(' Penalty function in effect: ',f8.3,' log likelihood units')
	   endif
	endif
	callid=4023
	goto 2
case(4023)
	
	if(autosim) then
	   do i=1,nset
		elmset(i,nsims)=setlik(i)
	   enddo
	   if(abort) then
		imesg=gmdisplaymessagebox('',' Save the last (aborted) fit ?',gquestion,gyesno)
		if(imesg.eq.gnobutton) goto 513
	   endif

	   kdim=100
! For autosim need to fix to keep ec50s for 2 ligands
	   if(dcmodel.and.nlig.eq.1) then
		call MODEC50(imod0,QT,npar,kdim,pmax,ec50out)
	   else
		call EC50_HJC(EC50out,curinf,curmax,concmax,cur0,pop0,&
     	  pmax,nlvar,QT,iset,conc,k,kdim,nerr)
		if(nerr.ne.0) then
			imesg=gmdisplaymessagebox('','Error in bisection for EC50',ginformation,gok)	
		   if(discprt) write(7,590) nerr

		endif
	   endif
	!   	ix1sav=ixr1	!keep random seeds at start of run
	!	iy1sav=iyr1
	!	iz1sav=izr1
		do j=1,nset
	   tress1=1.d6*sngl(tresol(j)) !tresd(j) ?
	   tress1=sngl(tresol(j))
	   enddo
	  ! if(nsims.le.nsim+nsims0) apfile=.true.
	   call SIMWRT(nsims,simfile,simfile1,imod0,k,npar,nset,nlig,titlep,&
         thtrue,thsav,conc,kfit,fixec50,ec50,stpfac,confac,errfac,nevm,&
         cdate,ctime,apfile,tress1,mtitle1,nsim,&
        ix1sav,iy1sav,iz1sav,nint,nev,elmax,theta0,&
         thetval,nintval,ixval,iyval,izval,elmval,elmset,abort,&
         imods,npars,mtitles,nevals,ec50out,penfunc,ec50val,nsims0,qfilem)
513	   nsims=nsims+1
	   if(nsims.le.nsim+nsims0.and.(.not.endsim)) then

		ik=0
		do m=1,npar
		 if(jfix(m).eq.0.and.jcon(m).eq.0.and.jmic(m).eq.0) then
		   if(.not.(fixec50.and.m.eq.m50)) then
			ik=ik+1
			theta(ik)=thsav(m)
		   endif
		 endif
		enddo
		iwrite=2	!keep current  ix1 etc but don't change ixlast
		call RANDSKv1(ixr1,iyr1,izr1,iwrite,repeat,main)		!write IX,IY,IZ
	!	call intconv(nsims,cnum5)
	!	imes=gmdisplaymessagebox('','Continue simulation='//cnum5,gquestion,gyesno)
	!	if(imes.eq.gyesbutton) then
			idestf=5
			apfile=.true.
			goto 99 !500		!do next fit
	!   endif
	   else
	    idestf=0
		do j=1,nset
!		   tresol(j)=1.e3*treso	!keep res in mus for .ini
		enddo
		iwrite=1	!replace both ix1 and ixlast (unless repeat=T)
		call RANDSKv1(ixr1,iyr1,izr1,iwrite,repeat,main)		!write IX,IY,IZ
	   endif
	endif

      if(discprt) write(7,108)
108	format('/')
      
      if(discprt) write(7,109)
109	format(' VALUES CALCULATED FROM FINAL FIT')
	do jset=1,nset
         
         if(discprt) write(7,267) jset
267      format(/,' Equilibrium values for set number ',i3,': ',/,' concentration(s) =')
	   do i=1,nlig
	
		if(discprt) write(7,335) conc(i,jset)*1.e6,ligname(i)

	   enddo
!         print 138
         if(discprt) write(7,138)
	   call QNEW_HJC(QT,jset,conc,QD,ncdep,nlig,IL,IX,JX,k,km,km)

	   call EQOC_HJC(QD,peq,k,km,km)
	   call SLIFEd1(QD,Peq,km,km)
	enddo

		first=.false.
	   if(nodata) then
		first=.true.
	   endif
	   oneset=.false.
	   idebug=idsav			!save idebug
	    elmax=-smin
		idebug=8
	    elmax=-HJCLIK(-1,kfit,theta,tint,ampl,iprops,nd1,nd2,Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,&
        XAF,XFA,QexpQA,QexpQF,alpha2,beta2,aloglik,kab,kAm,kFm,km,irt,npar,IQf,irate1,jrate1,nlig)
	 	idebug=idsav			!restore idebug
!		elmax=-smin                                                            
	   ihcell=24
	   do i=1,10
		iwcell(i)=110
	   enddo
	 isw=-3
	 if(nofit) isw=-10
	 plot=.false.
	 if(autosim) then
	 	xtext='Q(i,j) '//char(124)//'Name'//char(124)//'Initial guess'//char(124)//&
			'Constrain/Fix'//char(124)//'Final value'//char(124)//'True value'

	 nrr=6
	 else
	xtext='Q(i,j) '//char(124)//'Name'//char(124)//'Initial guess'//char(124)//&
			'Constrain/Fix'//char(124)//'Final value'//char(124)//'Factor'
		
	nrr=5
	endif
	  call text_array(igraph2,Main,Form1,Form1_TxtArray1,nrr,npar,iwcell,ihcell,xtext,isw,&
            namelink,ratcons(indk)%iconc,ratcons(indk)%qij,&
			thsav(1:100),titlem,ratcons(indk)%titlep,NBOUND,ligname,nlig,&
			ratcons(indk)%micro,ratcons(indk)%ligant,jfix,theta0,iformText)
	
!	   callid=4401
!	   goto 2	
case(4401,4403)
        call gmSetWidgetStatus(ipl1, GSELECTABLE)
		call gmSetWidgetStatus(ipl2, GSELECTABLE)
		call gmSetWidgetStatus(ipl3, GSELECTABLE)
		call gmSetWidgetStatus(ipl4, GSELECTABLE)
		call gmSetWidgetStatus(ipl5, GSELECTABLE)
		call gmSetWidgetStatus(ipl5, GSELECTABLE)

!		call gmdisplaymessagebox('','Please choose type of plot or repeat fit',ginformation,gok)
		call gmSetWidgetStatus(eqfit(3), GSELECTABLE)
		idestf=2
		if(callid.eq.4403) then 
			call gmremovewindow(Form1(igraph2,isw))
			idestf=3
		endif
		plot=.false.
	!	call main_plots(main,miplot0,mtoggle,1,1,iplot_Toggle0,nlig,ligname,conc,nset)

case(4402)
isetu=8
!		callid=2040
!		inipage=-100
!		isetu0=isetu
!		isetu=7
	 
!		goto 99
		call gmremovewindow(Form1(igraph2,isw))
		callid=4020
		goto 2
end select

99 end