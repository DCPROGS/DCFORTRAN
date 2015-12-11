	program SCALCS
c To calculate macroscopic jumps and noise for values produced by QGEN
c Lahey V5.n version 10/07/93 03:34pm
c
c Modif 11/23/01 11:00am for obeymr(50). Rather than add this to .ini,
c set elements of nsc(i) negative (for .ini ONLY) if obeymr(i)=false
c
c MAJOR MODIF 01/05/01 09:50pm TO TAKE UP TO 100 STATES
c Note: GETQD now returns QT without conc (or diagonals), so rather than using
c QNEWC to change conc (only), use QSETD or QSETC to set whole Q matrix (does
c not need the 'standard' conc, cA1, cB1, used in old version)
c Changes
c (1) pre*3 (not pre*1)
c (2) remove calls to GETIJ and use
c		   i=irate(m)
c		   j=jrate(m)
c (3) note common sizes in getqd
c	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200)
c	character*2 charmod(25,40)	!to print model
c	common/model/imod0,charmod,jlast,ilast,mtitle,imodold
c
c
c NB QGEN now keeps all rate constants in a ONE
c  file, QDAT.DAT, rather than sep file for each model; also QGEN.INI
c  replaces QDISC.DAT, and CHARQ.DAT now renamed QMODEL.DAT).
c All three files (QGEN.INI, QDAT.DAT, QMODEL.DAT) now kept in current root
c segment (they need to be available to several different progs)
c
c Modif 06/17/99 11:12am so that micro rev need not be obeyed -obeymr=true
c should be same as before -in common/mr/obeymr which is in eqoccd, getqd

c Modif 12/29/97 08:02am so expfunc, expfunc1 are at end of main prog
c rather than in expmaxd, to prevent duplicate definition.
c Modif 03/05/95 06:13pm to print values of rate constants (with names
c defined in QGEN) and option to alter values).
c Modif 01/03/92 02:56pm to give option to calculate relaxation in response
c to a pulse as a single graph. Also now make real*8 the following
c variables: cur0(10),tau(10),curinf,cur0tot,cfacd.
c
c Calculate relaxations from
c p(t)= p(inf) + p(0)*sum[A(m)*exp(-t/tau(m)];  sum from m=2,..,k
c
c=	REAL*8 QD(100,100)	!not used in this version
	REAL*8 QT(100,100),PImat(100,100),Pinf(100),P0(100),dgamma(100)
	real*8 eigen0(100),p00(100),pinf0(100),bmj0(100,100)	!for ON in 'pulse' case
	real*8 Amat(100,100,100),eigen(100)
	real*8 Q1(100,100),pt0(100),pt(100)
	real*8 p00z(100),p00n(100),bmj0z(100,100),bmj0n(100,100)	!for prepulse calcs
	real*8 p0z(100),p0n(100),bmjz(100,100),bmjn(100,100)	!for prepulse calcs
	real*8 bm0(100),bmj(100,100),bm(100),bmrel(100)
	real*8 cm(100),pi,var1,s
	real*4 cv0(100),g0(100),area(100),fc(100)
c	real*4 cur(10),cur0(10),tau(10)
	real*8 cur0(100),tau(100),curinf,cur0tot,cfacd,cur(100),vknd
	real*8 cur00(100),tau0(100),curinf0	!ditto for 'on relaxn' for 'pulse'
	real*4 gamma(100)
c	real*4 xcal(2048,10),ycal(2048,10)
	ALLOCATABLE Xcal,Ycal
	real Xcal(:,:),Ycal(:,:)		!for VPLOT
	ALLOCATABLE Xcalsav,Ycalsav
	real Xcalsav(:),Ycalsav(:)		!to store last plot
	real*4 pstar(4)
c=	integer IX(10),JX(10),IL(10)		!NB 'ix' is used by RANDOM
c Declarations for modif 03/05/95 06:16pm
c=	character*10 titlep(50)
	character*10 titlep(200)
	character pre*3
	integer NSC(50),IM(50,100),JM(50,100),IX(100),JX(100)
	integer IQ(100,100)		!in common with getqd
	COMMON/KM2/AKA1,BA,PSTAR,KMCON(9),KMFAST,aka2,arat
	real*4 hpar
	COMMON/VPAR/NVDEP,IV(100),JV(100),HPAR(100)
	COMMON/CPAR/NCDEP,IX,JX,X
	COMMON/QPAR/NCON,IC(2,200)
	COMMON/MPAR/NCYC,NSC,IM,JM
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200)
	COMMON/LIG/nlig,IL(100)
c
c For new version that uses qmechs.dat
	real*4 conc0(10),conc00(10),conc(10),x,x0,x1
	real*4 conc000(10)	!conc before prepulse
	integer irate(200),jrate(200)
	character*20 ligname(10)
c Declarations for aligned bursts (added 09/20/97 06:15pm)
	integer BA,BB,CA,CB,EE
	real*8 Q2(100,100),Q3(100,100),GBA(100,100),phib(100),one,sum
	real*8 bmax(2),bmax1
	real*4 pop0,curr0,atot
c For mouse
	logical mouse_on,mous_set
	common/mousval/mouse_on,nbutton
c For expmax
c	real*4 ws(10),taus(10)
	real*4 curinfs
c=	common/exp/ ws,taus,curinfs,ncomp1
c for expmaxd
	real*8 ypeak,tpeak,texp,thigh,t0d,a0,atotd
	common/expd/ cur,tau,curinf,ncomp1
	real*8 tbig,rbig,rexp,rbiglast,rexplast,deltx,delty
c	real*8 radx
c	real*8 wmin
	real*8 wmax
	real*8 dround
c
	logical vjump,cjump,jump,noise,kmfast,calinit,bad,pulse,qprt
	logical varconc		!true for time-dep concentration calcs
	logical varate		!for d/r curves with x=rate constant
	logical prepulse,plotsav,drcurve,plotrate,prevar,plotocc,plotcur
	logical align		!true for aligned s-s bursts
	logical allocated,readini,present
	character*1 ans,UC,ans1
	character*11 cdate,ctime
	character*40 path
	LOGICAL debug,deb,prt,errflag
	logical caplock
c	character ndev*2,infil*14
	logical monot,decline	!for eqec50
c
c for binding curves -nbound in common in getqd, getbound (getnlig now obsolete)
	real*8 bound(100,10)
	integer nbound(100,10)
	common/nbnd/nbound
c
c for eqoccd, getqd
	character qfilem*40
	logical obeymr(50),automr(50)
	common/mr/obeymr,automr
	integer isetmr(50)
	common/mr1/isetmr
c For spanning tree
	logical useprim		!use spanning tree method
c
	logical discprt
	common/dp/discprt
	character*40 mtitle1*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,jcol,mtitle1 !for WINPRINT,ENDPRINT,DISCNUM
	common/KBLK/kA,kB,kC,kD
	COMMON/dimblk/K,KMAX,ir,nmod
c	common/Vval/vhold,vkin,vref,gamr,enchan		!V in mV
	COMMON/RBLCK/TRESO,TRESG,IACRIT,iw
c	common/ccalc/cmax,trise,tdec,nshape,ncomp,tdec2,wamp,tpuls
	common/db/ideb		!debug level
	common/qblk/IQ
	logical gaddum
	common/gad/gaddum,tauzero
	common/mod/imod
	character*2 charmod(25,40)	!to print model
	character*74 mtitle	!title for model
	character inifile*40
	common/model/imod0,charmod,jlast,ilast,mtitle,imodold

c Modif 06/25/03 09:06am in getqd to put npar and irate, jrate also in commons
c  Common for GETQD,calcmr/dfpmin/getrev
	common/np1/npar
	common/ir/irate,jrate
c
c  For getqd to enable read/write of models defined in Windows version
	character*3 snumw(100)		! state number
	integer*4 icolwin(100)		! colour for open/schut
	real*4 wposx(100),wposy(100)	! position
	integer*4 nwidwin,nhghtwin	!window dimensions
	common/winmod/snumw,icolwin,wposx,wposy,nwidwin,nhghtwin
	common/deb2/idebug2	!to control printing in checkmr
c
c
c	integer*2 videotyp
c
c	pon()=slock()	!replaced by .false. in all calls 02/08/04 06:50am
	debug()=caplock()
c
c
101	format(a1)
	call SETMOUSE()		!define values in common\mousval\ (in IVLIB)
      OPEN(unit=10,file='ERROR.GIN')
	call errdev(10)
	call GINO
	call errdev(10)
	call vga
	call errdev(10)
	call mode(3)
c4	format(g13.6)
	filnam='SCALCS.PRT'
	idebug2=2	!print to screen and .prt
	call WINPRINT	!print file control
      OPEN(unit=7,file=prtport,iostat=nerr)		!open printer
	print 1
	if(discprt) write(8,1)
1	FORMAT(' SCALCS: Macroscopic noise and jumps calculation',/)
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 2,cdate,ctime(1:8),mtitle1
	if(discprt) write(8,2) cdate,ctime(1:8),mtitle1
2	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 3
3	format(
     & ' CAPS LOCK on for debugging')
	print 398
398	format(
     &' This program uses the model specified by the Q matrix',/,
     &' (from qmechs.mec) to calc C-jump relaxations or noise spectra.',
     &/,' V-jumps not yet fixed in this program.')
	call BELL(1)
C
	icalc=3	!default=pulse for now
	iplot=1
	iprint=4
	nlvar=1
	nljump=1
	plotsav=.false.
	plotcur=.false.
c
c Use F90 routine to prevent underflow crashes??
	errflag=.true.
	call UNDFL(errflag)
c
452	readini=.false.
	print 450
450	format(' Read defaults from .ini file on disc [Y] ? ')
	ans='Y'
	call INPUTa(ans)
      if(ans.eq.'Y') then
	   inifile='SCALCS.INI'
	   call TITENT0(
     &    'Name for .ini file:',inifile,40,.false.)
	   INQUIRE(file=inifile,exist=present,flen=nlen,err=452)
	   if(.not.present.or.nlen.eq.0) then
		call BELL(1)
	 	print 453,inifile
453	 	format(' Cannot find ',a40)
	 	ans='N'
	 	goto 452
	   endif
	   if(discprt) write(8,411) inifile
411	   format(' Name of initialisation file: ',a40)
	   readini=.true.
	   if(nlen.eq.512) then		!old.ini
      	OPEN(unit=17,file=inifile,status='UNKNOWN',
     & 	access='DIRECT',form='UNFORMATTED',recl=512)
		read(17,rec=1)irecq,xA00,xA0,xA1,t0,iprint,icalc,iplot,tpre,
     &	xAs,ans1,npulse,tpgap,imodold,xB00,xB0,xB1
		CLOSE(unit=17)
		conc00(1)=xA00
		conc00(2)=xB00
		conc0(1)=xA0
		conc0(2)=xB0
		conc(1)=xA1
		conc(2)=xB1
	   else    	!read new ini
	     	OPEN(unit=16,file=inifile,status='UNKNOWN',

     &    	access='DIRECT',form='UNFORMATTED',recl=10240)
		read(16,rec=1)iver,irecq,imodold,conc,conc0,conc00,conc000,
     &	iprint,icalc,iplot,tpre,xAs,ans1,npulse,tpgap,
     &	nljump,nlvar,npr,nvdep,ncyc,vhold,vref,vkin,
     &	(nsc(i),i=1,ncyc),
     &	((im(i,j),j=1,nsc(i)),i=1,ncyc),
     &	((jm(i,j),j=1,nsc(i)),i=1,ncyc),
     &	qfilem,plotcur,useprim,(isetmr(i),i=1,ncyc)
		if(UC(qfilem(1:1)).ne.'Q') qfilem='qmechs.mec'
		do i=1,ncyc		!if nsc(i) is neg, set obeymr(i)=F and restore nsc
		   if(nsc(i).lt.0) then
			obeymr(i)=.false.
			nsc(i)=iabs(nsc(i))
		   else
			obeymr(i)=.true.
		   endif
		   if(isetmr(i).lt.0) then
			automr(i)=.true.
			isetmr(i)=iabs(isetmr(i))
		   else
			automr(i)=.false.
		   endif
		enddo
		CLOSE(unit=16)
	   endif
	endif
	if(ans1.ne.'Y'.and.ans1.ne.'N') ans1='N'
c
104	continue		!return here to start from beginning
c	call DCASK('Print Q matrices','y',ans)
c	qprt=ans.eq.'Y'
	print 56,iprint
56	format(' Set print-out level:',/,
     & ' (1) Print rate constants and final results only',/,
     & ' (2) Print also occupancies ',/,
     & ' (3) Print also lifetimes ',/,
     & ' (4) Print also Q and pi matrices',/,
     & '  Option number [',i2,'] = ')
	call INPUTi(iprint)
	qprt=iprint.eq.4
c
c Check whether to use macro rev before calling GETQD
	kmax=100		!dimension of Q
	km=100				!array dimensions
c
35	if(nlig.gt.0) then
	   do i=1,nlig
		xs=1.e6*conc(i)
		print 159,i,ligname(il(i)),xs
159		FORMAT(/,
     &     ' Ligand #',i2,1x,a20,': concentration (muM) [',g13.6,'] = ')
		call INPUTr(xs)
		conc(i)=xs*1.0e-6
	   enddo
	endif
c	V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
c=	call QSETD(conc,IL,V1,QT,QM,k,.false.)
c===	call QSETC(QT,conc,QM,ncdep,nlig,IL,IX,JX,k,km)		!sets conc only
c=	call EQOCCd(QM,k,k-1,km,Peq)		!CALC EQUILIB OCCS

c Get the Q matrix
	idest=0
105	continue
c Note GETQD now returns QT without conc (or diagonals)
	kflag=0
c Modif 06/25/03 09:06am in getqd to put npar and irate, jrate also in commons
	call GETQD(QT,nchan,dgamma,vkin,vhold,vref,
     &  titlep,ligname,iflag,iprint,readini,irecq,qfilem,
     &  useprim,kflag,idest)
c	call GETQD(QT,nchan,dgamma,vkin,vhold,vref,
c     &  titlep,ligname,iflag,iprint,readini,irecq,qfilem,
c     &  kflag,idest)
	if(nchan.le.0.or.nchan.gt.10000) nchan=1
c (debug -graphics call OK when done here)
	k=kA+kB+kC+kD
	gaddum=imod.eq.31.and.k.eq.3.and.titlep(1)(1:6).eq.'kA(-1)'
	noise=.false.
	calinit=.false.
c=	cA1=xA	!save conc at which QD calculated in case conc altered
c=	cB1=xB
	cfac=float(nchan)*(vkin*0.001)*1.e12	!*dgamma (Siemens) gives pA
c Get rid of rounding errors
	vknd=dble(vkin*float(nchan))
	vknd=DROUND(vknd,0)	!exact double precision as long as vkin is integer
c==!!	call DROUND(vknd,0)	!exact double precision as long as vkin is integer
	cfacd=vknd*1.d-3*1.d12
c Define also cfac2 which, when mult dgamma**2 gives pA**2
	cfac2=float(nchan)*(vkin*0.001)*(vkin*0.001)*1.e24
	do i=1,k
	   gamma(i)=1.e12*sngl(dgamma(i))		!in pS
c V is in mV
c	   cur(i)=float(nchan)*(vkin*0.001)*gamma(i)  !pA for nchan open channels
	enddo
	if(gaddum) then
	   aKA=sngl(qt(1,2)/qt(2,1))		!molar
	   aKB=sngl(qt(3,2)/qt(2,3))
	   print 77,1.e6*aKA,1.e6*aKB
	   if(discprt) write(8,77) 1.e6*aKA,1.e6*aKB
77	   format(/,' KA, KB (muM) = ',2g13.6)
	endif
c
281	continue
c If not cjump or vjump ask for init cond here anyway
291	   call BELL(3)
	   print 29,icalc
29	   format(/,
     &' MAIN MENU: CHOOSE WHAT TO CALCULATE',/,
     &' (1) Calculate a c-jump (assuming initial equilibrium)',/,
     &'      (or specify arbitrary initial vector)',/,
     &' (2) Calculate a c-jump (not assuming initial equilibrium)',/,
     &' (3) Calculate relaxation in response to a conc-pulse',/,
     &' (4) Calculate relaxation for conc-pulse with pre-pulse',/,
     &' (5) Calculate relaxation with time-dependent concentration',/,
     &' (6) Calculate noise spectrum',/,
     &' (7) As (1) but plot d/r curves for series of post-jump conc',/,
     &' (8) As (1) but plot binding curves for series of post-jump '
     & 'conc',/,
     &' (9) As (1) but for range of values of a rate constant',/,
     &' (10) Calculate response for a series of pulses ',/,
     & ' Option number [',i2,'] = ')
	   call INPUTi(icalc)
	   iopt=icalc
	   if(iopt.lt.1.or.iopt.gt.10) goto 291
	   vjump=.false.
	   cjump=.true.         !unless iopt=6
	   varconc=.false.	!unless iopt=5
	   prepulse=.false.     !unless iopt=4
	   drcurve=.false.
	   plotrate=.true.  !plot rate constant (rather than tau) for drcurve
	   prevar=.false.   !used only if nlig=2
	   plotocc=.false.
	   varate=.false.
	   if(iopt.ne.10) npulse=1
	   if(iopt.eq.1) then
		calinit=.false.
		pulse=.false.
		plotcur=.true.
	   else if(iopt.eq.2) then
		calinit=.true.
		pulse=.false.
		plotcur=.true.
	   else if(iopt.eq.3) then
		calinit=.true.
		pulse=.true.
		plotcur=.true.
	   else if(iopt.eq.4) then
		calinit=.true.
		pulse=.true.
		prepulse=.true.
		plotcur=.true.
	   else if(iopt.eq.5) then
		calinit=.true.
		pulse=.false.
		varconc=.true.
		plotcur=.true.
	   else if(iopt.eq.6) then
		cjump=.false.
		pulse=.false.
		npulse=0
		noise=.true.
		plotcur=.true.
	   else if(iopt.eq.7) then
		calinit=.false.
		pulse=.false.
		drcurve=.true.
		plotocc=.false.
		if(plotcur) then	!default from .ini
		   iopt=1
		else
		   iopt=2
		endif
		print 72,iopt
72		format(
     &	' (1) Plot current',/,
     &	' (2) Plot P(open)',/,
     & 	'  Option number [',i2,'] = ')
		call INPUTi(iopt)
		plotcur=iopt.eq.1
		print 73
73		format(/,
     & ' The plotting options allow either the predominant time',/,
     & ' constant (that with largest amplitude) or the predominant',/,
     & ' rate constant, to be plotted against concentration: ',/)
		ans='N'
		call DCASK(
     &	'Plot time constant (rather than rate constant)',ans,ans)
		plotrate=ans.eq.'N'
	   else if(iopt.eq.8) then
		calinit=.false.
		pulse=.false.
		drcurve=.true.
		plotocc=.true.
		plotcur=.false.
		ans='N'
		call DCASK(
     &	'Plot time constant (rather than rate constant)',ans,ans)
		plotrate=ans.eq.'N'
	   else if(iopt.eq.9) then
		calinit=.false.
		pulse=.false.
		drcurve=.true.
		varate=.true.
		do m=1,npar
		   pre=' '
c==		   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
		   i=irate(m)
		   j=jrate(m)
		   r=QT(i,j)
		   if(ncdep.gt.0) then
			do n=1,ncdep
			   if(ix(n).eq.i.and.jx(n).eq.j) then
				pre(2:2)='*'		!asterisk indicates rate to be mult by conc
				pre(3:3)=CHAR(il(n)+48)	!ligand number
c==				r=r/xA
			   endif
			enddo
		   endif
c check if either if i,j or j,i is a micro rev route
		   if(ncyc.gt.0) then
			do n=1,ncyc
			   if(im(n,1).eq.i.and.jm(n,1).eq.j) then
c==				pre=char(240)	!equiv sign '=q(1,2)' indicates micro rev route
				pre(1:1)=char(244)	!dagger sign '=q(1,2)' indicates micro rev route
			   endif
			enddo
		   endif
         	   print 122,m,pre,i,j,titlep(m),r
122	   	   format(i3,2x,a3,' q(',i2,',',i2,')=',3x,a10,2x,g13.6)
		enddo
1211		print 121,npar,npv
121		format(' Parameter number to vary (1 - ',i3,') [',i3,'] = ')
		call INPUTi(npv)
		if(npv.lt.1.or.npv.gt.npar) goto 1211
		if(plotcur) then	!default from .ini
		   iopt=1
		else
		   iopt=2
		endif
		print 72,iopt
c72		format(
c     &	' (1) Plot current',/,
c     &	' (2) Plot P(open)',/,
c     & 	'  Option number [',i2,'] = ')
		call INPUTi(iopt)
		plotcur=iopt.eq.1
	   else if(iopt.eq.10) then
		plotcur=.true.
		calinit=.true.
		pulse=.true.
		print 4,npulse
4		format(' Number of pulses [',i3,'] = ')
		call INPUTi(npulse)
		tpgap=tpgap*1000.
		print 5,tpgap
5		format(
     &	' Interval from start of one to start of next (ms) [',
     &	f9.1,'] = ')
		call INPUTr(tpgap)
		tpgap=tpgap*0.001
	   endif
c	endif
c
c ========temp debug
c	if(VIDEOTYP().ne.18) then
c	   call DEVEND
c	   call GINEND
c	   call GINO
c	   call VGA
c	   call errdev(10)
c	   call gsetcols(0)
c	   call errdev(10)
c	   call errswi(-1)
c	   call errdev(10)
c	   call brkswi(1)
c	   call errdev(10)
c	   call chaswi(1)
c	   call errdev(10)
c	   call grfmod (1)
c	   call errdev(10)
c	   call harcha
c	   call mode(18)
c	   call errdev(10)
c	endif
c ========temp debug
c
c For dr curves check conc of other ligands
c	if(iopt.eq.7.or.iopt.eq.8.or.iopt.eq.9) then
	if(drcurve) then
	   if(nlig.eq.1) then
		nlvar=1
	   else if(nlig.gt.1.and.ncdep.gt.0) then
	      print 66
66		format(/,' Which ligand concentration should be varied ?')
654	      do i=1,nlig
		   print 651,i,ligname(i)
651		   format(' Ligand (',i2,'): ',a20)
		enddo
	      print 653,nlig
653	      format(
     &	  ' Plot against concentration of ligand number (1-'
     &	   ,i2,') [1] = ')
		nlvar=1
		call INPUTi(nlvar)
		if(nlvar.lt.1.or.nlvar.gt.nlig) goto 654
	   endif	!end of nlig>1
	   print 661,nlvar,ligname(nlvar),nlvar,ligname(nlvar)
661	   format(' Use a range of concentrations for',/,
     &    ' (1) POST-jump conc of ligand #',i2,' (',a10,')',/,
     &    ' (2) PRE-jump conc of ligand #',i2,' (',a10,')',/,
     &    ' Option [1] = ')
		iopt1=1
	   call INPUTi(iopt1)
	   prevar=iopt1.eq.2
c
	   if(iopt.eq.8) then	!binding curves
		if(nlig.eq.1) then
		   nlplot=1
		else
652		   do i=1,nlig
			print 651,i,ligname(i)
c651			format(' Ligand (',i2,'): ',a20)
		   enddo
		   print 65,nlig
65		   format(
     &	   ' Plot binding of ligand number (1-',i2,') [1] = ')
		   nlplot=1
		   call INPUTi(nlplot)
		   if(nlplot.lt.1.or.nlplot.gt.nlig) goto 652
		endif
	   endif
c
c conc() arrays all molar; x, xv1 etc all micromolar
	   xlo=0.1
	   xhi=100.
	   if(prevar) then
	      print 11,nlvar,ligname(nlvar),xlo,xhi
11		format(
     & ' Conc range (pre-jump) for ligand #',i2,'(',a10,
     & ') (muM):',/,'  Low, high conc  [',g11.4,',',g11.4,'] = ')
	   else
		print 116,nlvar,ligname(nlvar),xlo,xhi
116		format(
     & ' Conc range (post-jump) for ligand #',i2,'(',a10,
     & ') (muM):',/,' Low, high conc  [',g11.4,',',g11.4,'] = ')
	    endif
	    call INPUT2r(xlo,xhi)
c
c=	    x0=conc0(nlvar)*1.e6
c=	    print 69,nlvar,ligname(nlvar),x0,xlo,xhi
c=	    if(discprt) write(8,69) nlvar,ligname(nlvar),x0,xlo,xhi
c=9	    format(' For ligand #',i2,'(',a10,')',/,
c=     & 	' Initial concentration (micromolar) = ',g11.4,/,
c=     &	' Range for post-jump concentration from ',
c=     &    g11.4,' to ',g11.4)
	    if(prevar) then
		 x=1.e6*conc(nlvar)
		 print 662,nlvar,ligname(nlvar),x
662		 format(' (Fixed) post-jump conc of ligand ',i2,' (',
     &	 a10,') (muM) [',g10.4,'] = ')
		 call INPUTr(x)
		 conc(nlvar)=1.e-6*x
c NB if prevar=T: fixed postjump conc already in conc(), change only conc0(nlvar)
c NB if prevar=F: fixed pre-jump conc already in conc0(), change only conc(nlvar)
	    else
		 x=1.e6*conc0(nlvar)
		 print 663,nlvar,ligname(nlvar),x
663		 format(' (Fixed) pre-jump conc of ligand ',i2,' (',
     &	  a10,') (muM) [',g10.4,'] = ')
		 call INPUTr(x)
		 conc0(nlvar)=1.e-6*x
	    endif
c     Now specify the fixed conc (for all jumps) of all the OTHER ligands
	   if(nlig.gt.1) then
		do i=1,nlig
		   if(i.ne.nlvar) then	!all the rest
			x=1.e6*conc0(i)
			print 664,i,ligname(i),x
664			format(' Conc of ligand ',i2,' (',
     &	      a10,') BEFORE jump (muM) [',g10.4,'] = ')
			call INPUTr(x)
			conc0(i)=1.e-6*x
			x=1.e6*conc(i)
			print 665,i,ligname(i),x
665			format(' Conc of ligand ',i2,' (',
     &	      a10,') AFTER jump (muM) [',g10.4,'] = ')
			call INPUTr(x)
			conc(i)=1.e-6*x
		   endif
		enddo
	   endif
	endif		!end of d/r curves
c
	jump=vjump.or.cjump
c
c Calcs for time dep conc in sep subroutine.  Need first to calc p(0) as below
	if(varconc) then
	   nljump=1
	   if(nlig.gt.1) then	!show the ligands
	      do i=1,nlig
		   print 651,i,ligname(i)
c651		   format(' Ligand (',i2,'): ',a10)
	      enddo
	   endif
	   print 601,nljump
601	   format(
     &    ' Which ligand time course to be calc: ligand # [',i2,'] = ')
	   call INPUTi(nljump)
c     Now get fixed conc of other ligands, if any
	   if(nlig.gt.1) then
		 do i=1,nlig
		   if(i.ne.nljump) then	!all the rest
			x=1.e6*conc0(i)
			print 669,i,ligname(i),x
669			format(' (Fixed) conc of ligand ',i2,' (',
     &	      a10,') throughout (muM) [',g10.4,'] = ')
			call INPUTr(x)
			conc0(i)=1.e-6*x
		   endif
		 enddo
	   endif
c
	   ndc1=2048
	   ndimc=k+1+1	!number of states +1 for tot current +1 for conc profile
	   if(ndimc.lt.11) ndimc=11
	   if(.not.allocated(xcal)) then
	      ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
	   endif
	   call VCONC1(QT,conc0,nljump,ligname,
     &  	k,km,qprt,xcal,ycal,ntime,dt,ndc1,ndimc)
	   goto 45	!jump to SCDISP with Xval,Yval=p(t) already calculated!
	endif		!end of var conc
c Concentrations from disc are in molar units (if QGEN2 was run with
c EPSCSIM option then conc=1.0M nominally, ie conc not incorporated)
c If c-jump, and calinit=true, then not at equilib with initial concentration
c so the initial conditions for the c-jump, p0() and cur0, must be recalculated
c here
c
c Similarly for V-jump enquire if system has come to equilibrium
c at the initial potential ======to be done (but have not got all nec data
c here to recalc Q at diff potentials!)
c
c Code below is getting congested! Better add section for a series
c of pulses as a separate subroutine
c Use same number of points for ON and OFF relaxations, ncal1 say -this is
c defined in SCALCS, before call of DPULSE, so xcal, ycal can be
c declared the right size
	if(npulse.gt.1) then
	   ndc1=2048
c NB (see dpulse) -as written at present, need ndimc=2+nlig+k)
c==	   ndimc=15
	   ndimc=2+nlig+k
	   ncal1=401
	   ndc1=2*ncal1*npulse
	   if(.not.allocated(xcal)) then
	      ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
	   endif
	   tpulse=t0	!default from scalcs.ini
	   call DPULSE(npulse,tpgap,QT,ncdep,nlig,ligname,
     &	IL,IX,JX,conc00,conc0,conc,nlvar,vhold,vref,
     &	dgamma,cfac,bound,nbound,tpulse,
     &  	ncal1,k,km,qprt,xcal,ycal,ntime,dt,ndc1,ndimc,iprint)
	   t0=tpulse	!notation for pulse length in rest of prog
	   goto 451	!jump to SCDISP with Xval,Yval=p(t) already calculated!
	endif
c
c For c-jump, check the concentrations
c For d/r curve cases all the fixed conc have been set above, now set the one
c that varies
102	if(cjump.and.drcurve) then
	   if(varate) then		!rate constant varied with fixed conc jump
		do i=1,nlig
		   x0=conc0(i)*1.e6
		   x1=conc(i)*1.e6
		   print 20,i,ligname(i),x0,x1
20		   format(
     & ' Initial and final conc. (muM) for ligand #',i2,' (',
     &	   a10,')',/,'  [',g11.4,',',g11.4,'] = ')
		   call INPUT2r(x0,x1)
		   conc0(i)=x0*1.E-6
		   conc(i)=x1*1.E-6
		enddo
		print 111,npv,titlep(npv),xlo,xhi
111		format(
     &	 ' Lower, upper values for range of rate constant: ',/,
     &	 '   for rate constant ',i3,': ',a10,' [',2g12.4,'] = ')
	      call INPUT2r(xlo,xhi)
            if(discprt) write(8,112) xa0*1.e6,xa1*1.e6,vkin,titlep(npv)
112	      format(
     &   ' Concentration jump from ',g13.6,' to ',g13.6,' muM',/,
     &   ' at V = ',f8.1,' mV.',/,
     &   ' Plot peak and equilibrium response against ',a10)
	   else
c           Print all values (read in above)
		if(prevar) then
		   if(discprt) write(8,67) nlvar,ligname(nlvar),
     &		 xlo,xhi,conc(nlvar)
67		   format(/,' Ligand #',i3,' (',a10,')'/,
     &		'  ligand concentration range BEFORE jump = ',
     &		g11.4,' to ',g11.4, ' muM',/,
     &		' Concentration AFTER jump (same for all) = ',g11.4)
		else
		   if(discprt) write(8,671) nlvar,ligname(nlvar),
     &		 xlo,xhi,conc(nlvar)
671		   format(/,' Ligand #',i3,' (',a10,')'/,
     &		'  ligand concentration range AFTER jump = ',
     &		g11.4,' to ',g11.4, ' muM',/,
     &		' Concentration BEFORE jump (same for all) = ',g11.4)
		endif
		do i=1,nlig
		   if(i.ne.nlvar) then	!all the rest
			x=1.e6*conc0(i)
			if(discprt) write(8,666) i,ligname(i),x
666			format(' (Fixed) conc of ligand ',i2,' (',
     &	         a10,') BEFORE jump (muM) = ',g11.4)
			   x=1.e6*conc(i)
			if(discprt) write(8,667)i,ligname(i),x
667			   format(' (Fixed) conc of ligand ',i2,' (',
     &	          a10,') AFTER jump (muM) = ',g11.4)
		   endif
		enddo
	   endif
	else if(cjump.and.(.not.pulse)) then
	   do i=1,nlig
		x0=conc0(i)*1.e6
		x1=conc(i)*1.e6
		print 674,i,ligname(i),x0,x1
674		format(
     & ' Initial and final conc. (muM) for ligand #',i2,' (',
     &	   a10,')',/,'  [',g11.4,',',g11.4,'] = ')
		call INPUT2r(x0,x1)
		conc0(i)=x0*1.e-6
		conc(i)=x1*1.e-6
		x=1.e6*conc0(i)
		if(discprt) write(8,672) i,ligname(i),x
672		format(/,' Conc of ligand ',i2,' (',
     &	   a10,') BEFORE jump (muM) = ',g11.4)
		x=1.e6*conc(i)
		if(discprt) write(8,673)i,ligname(i),x
673	      format(' Conc of ligand ',i2,' (',
     &	   a10,') AFTER jump (muM) = ',g11.4)
	   enddo
	else if(noise) then
	   print 6761
	   if(discprt) write(8,6761)
6761	   format(/,' NOISE calculation')
	   do i=1,nlig
		x1=conc(i)*1.e6
		print 675,i,ligname(i),x1
675		format(
     & ' Concentration (muM) for ligand #',i2,'(',
     &	   a10,') [',g11.4,'] = ')
		call INPUTr(x1)
		conc(i)=x1*1.e-6
		x=1.e6*conc(i)
		if(discprt) write(8,676)i,ligname(i),x
676	      format(' Concentration (muM) of ligand ',i2,' (',
     &	   a10,') = ',g11.4)
	   enddo
	endif
c
c Concentrations all defined for cases except iopt=1,2,3,4
c calinit=true if initial condition has to be calculated (not at equilib)
c (e.g. for iopt=2,3,4)
	if(cjump.and.(.not.calinit)) then
c==	   call QNEWC(QD,cA1,cB1,Q1,xA0,xB0,ncdep,nlig,IL,IX,JX,k,km)
	   V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	   call QSETD(conc0,IL,V1,QT,Q1,k,.false.)
	   call EQOCCd(Q1,k,k-1,km,p0)	!calc new init occs at xA0
	else if(cjump.and.calinit) then	!iopt=2,3,4
	   nljump=1
	   if(nlig.gt.1) then	!show the ligands
	      do i=1,nlig
		   print 651,i,ligname(i)
c651		   format(' Ligand (',i2,'): ',a10)
	      enddo
	   endif
	   print 6011,nljump
6011	   format(
     &     ' Which ligand conc to be jumped: ligand # [',i2,'] = ')
	   call INPUTi(nljump)
	   if(pulse) then		!PULSE case iopt=3,4
		if(prepulse) then
		   xa00=conc00(nljump)*1.e6
	         print 36,nljump,ligname(nljump),xA00
36		   format(
     &	' Concentration before prepulse assumed to be zero.',/,
     &	' (1) Concentration of #',i2,'(',a10,
     &		') during prepulse (muM) [',
     &	   f8.3,'] = ')
		   call INPUTr(xA00)
		   conc00(nljump)=xA00*1.e-6
		   tpre=tpre*1000.		!in ms
	         print 37,tpre
37		   format(
     &	   '& Duration of the prepulse (ms) [',f7.2,'] = ')
		   call INPUTr(tpre)
		else
		   xa00=conc00(nljump)*1.e6
	         print 312,nljump,ligname(nljump),xa00
312		   format(/,
     &     '&(1) Conc of #',i2,' (',a10,
     &	') (at equilib) BEFORE pulse (muM) [',f8.3,'] = ')
		   xA00=0.0
		   call INPUTr(xA00)
		   conc00(nljump)=xA00*1.e-6
		endif
c
		xa0=conc0(nljump)*1.e6
	      print 313,nljump,ligname(nljump),xa0
313		format(
     &     '&(2) Conc of #',i2,' (',a10,
     &	') DURING pulse (muM) [',f8.3,'] = ')
		call INPUTr(xA0)
		conc0(nljump)=xA0*1.e-6
c
		xa1=conc(nljump)*1.e6
	      print 314,nljump,ligname(nljump),xa1
314		format(
     &     '&(3) Conc of #',i2,' (',a10,
     &	') AFTER pulse (muM) [',f8.3,'] = ')
c		xA1=0.0
		call INPUTr(xA1)
		conc(nljump)=xA1*1.e-6
c
		t0=t0*1000.		!in ms
	      print 315,t0
315		format(
     &	'&(4) Duration of the pulse (ms) [',f7.2,'] = ')
		call INPUTr(t0)
c     Now get fixed conc of other ligands, if any
		if(nlig.gt.1) then
		 do i=1,nlig
		   if(i.ne.nljump) then	!all the rest
			x=conc00(i)*1.e6
			print 312,i,ligname(i),x
c312			format(/,
c     &     	'&(1) Conc of #',i2,' (',a10,
c     &    		') (at equilib) BEFORE pulse (muM) [',f8.3,'] = ')
c			x=0.0
			call INPUTr(x)
			conc00(i)=x*1.e-6
c
			x=conc0(i)*1.e6
	      	print 313,i,ligname(i),x
c313			format(
c     &     	'&(2) Conc of #',i2,' (',a10,
c     &    		') DURING pulse (muM) [',f8.3,'] = ')
			call INPUTr(x)
			conc0(i)=x*1.e-6
c
			x=conc(i)*1.e6
		      print 314,i,ligname(i),x
c314			format(
c     &    	 	'&(3) Conc of #',i2,' (',a10,
c     &		') AFTER pulse (muM) [',f8.3,'] = ')
c			xA1=0.0
			call INPUTr(x)
			conc(i)=x*1.e-6
		   endif
		 enddo
		 if(prepulse) then
		   print 668
668		   format(
     &     ' (1) Conc before prepulse zero for all of these ',/,
     &     ' (2) Conc before prepulse same as above for all but ',a10,/,
     &	    ' Option number [1] = ')
		   iopt2=1
		   call INPUTi(iopt2)
		   if(iopt2.eq.1) then
			do i=1,nlig
			   conc000(i)=0.0
			enddo
		   else
			do i=1,nlig
			   conc000(i)=conc00(i)
			enddo
			conc000(nljump)=0.0
		   endif
		 endif
		endif
c
		if(prepulse) then
	         print 38,nljump,ligname(nljump),tpre,xa00,t0,xa0,xa1
	         if(discprt) write(8,38) nljump,ligname(nljump),tpre,
     &		xa00,t0,xa0,xa1
38		   format(/,
     &     ' CONCENTRATION PULSE:',/,
     &     '   For ligand ',i2,' (',a10,')',/,
     &     '   Prepulse of ',g13.6,' ms at ',g13.6,' micromolar ',/,
     &     '   then pulse of ',g13.6,' ms at ',g13.6,' micromolar ',/,
     &     '   before jumping to ',g13.6,' micromolar ')
		else
	         print 316,nljump,ligname(nljump),xa00,t0,xa0,xa1
	         if(discprt) write(8,316) nljump,ligname(nljump),
     &		xa00,t0,xa0,xa1
316		   format(/,
     &	' CONCENTRATION PULSE:',/,
     &     '   For ligand ',i2,' (',a10,')',/,
     &	'   start at equilibrium with ',g13.6,' muM ',/,
     &	'   then ',g13.6,' ms at ',g13.6,' muM ',/,
     &	'   before jumping to ',g13.6,' muM ')
		endif
c		print conc of other ligands if any
c		print conc of other ligands if any
		do i=1,nlig
		   if(i.ne.nljump) then	!all the rest
			x=1.e6*conc00(i)
			print 6641, i,ligname(i),x
			if(discprt) write(8,6641) i,ligname(i),x
6641			format(/,' Conc of ligand ',i2,' (',
     &	      a10,') BEFORE pulse (muM) = ',g13.6)
			x=1.e6*conc0(i)
			print 6642, i,ligname(i),x
			if(discprt) write(8,6642) i,ligname(i),x
6642			format(' Conc of ligand ',i2,' (',
     &	      a10,') DURING pulse (muM) = ',g13.6)
			x=1.e6*conc(i)
			print 6643, i,ligname(i),x
			if(discprt) write(8,6643) i,ligname(i),x
6643			format(' Conc of ligand ',i2,' (',
     &	      a10,') AFTER pulse (muM) = ',g13.6)
		   endif
		enddo
c
		xA00=xA00*1.e-6	!molar
		xA0=xA0*1.e-6
		xA1=xA1*1.e-6
		t0=t0*0.001		!in seconds
		tpre=tpre*0.001		!in seconds
	   else	!cjump.and.calinit but not pulse  iopt=2
	      print 21,t0*1000.
21	      format(' Length of exposure to ',g13.6,' muM (ms) = ')
	      call INPUTr(t0)
	      t0=t0*0.001		!in seconds
		xA00=conc00(nljump)*1.e6
	      print 22,xA00
22	      format('&Concentration before this (muM) [',g11.4,'] = ')
	      call INPUTr(xA00)
		conc00(nljump)=xA00*1.e-6
c     Now get fixed conc of other ligands, if any
		if(nlig.gt.1) then
		 do i=1,nlig
		   if(i.ne.nljump) then	!all the rest
			x=1.e6*conc0(i)
			print 664,i,ligname(i),x
c664			format(' (Fixed) conc of ligand ',i2,' (',
c     &	      a10,') BEFORE jump (muM) [',g10.4,'] = ')
			call INPUTr(x)
			conc0(i)=1.e-6*x
			x=1.e6*conc(i)
			print 665,i,ligname(i),x
c665			format(' (Fixed) conc of ligand ',i2,' (',
c     &	      a10,') AFTER jump (muM) [',g10.4,'] = ')
			call INPUTr(x)
			conc(i)=1.e-6*x
		   endif
		 enddo
		endif
	      xA00=1.e-6*xA00	!molar
            if(discprt) write(8,23) xa0*1.e6,t0*1000.,xA00*1.e6
23	      format(/,
     &    ' Initial condition for conc jump not at equilibrium:',/,
     &    ' at ',g13.6,' muM for ',g13.6,' ms, starting from ',g13.6,
     &    ' muM')
c		print conc of other ligands if any
		if(nlig.gt.1) then
		 do i=1,nlig
		   if(i.ne.nljump) then	!all the rest
			x=1.e6*conc0(i)
			if(discprt) write(8,6651) i,ligname(i),x
6651			format(' (Fixed) conc of ligand ',i2,' (',
     &	      a10,') BEFORE jump (muM) = ',g13.6)
			x=1.e6*conc(i)
			if(discprt) write(8,6652) i,ligname(i),x
6652			format(' (Fixed) conc of ligand ',i2,' (',
     &	      a10,') AFTER jump (muM) = ',g13.6)
		   endif
		 enddo
		endif
	   endif
c
c Now recalculate the initial condition (before pulse starts)
c First get Q at conc=xA00. The QD from disc is for concentrations xA1,xB1
c so alter the conc dep rates proportionally- use subroutine QNEWC()
c NB Q at xA00 needed only as input for EQOCCD to get initial occupancies

	   if(debug()) call ATYPd(QT,'  QT    ',k,k,km,km)
c==	   call QNEWC(QD,cA1,cB1,Q1,xA00,xB00,ncdep,nlig,IL,IX,JX,k,km)
	   V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	   call QSETD(conc00,IL,V1,QT,Q1,k,.false.)
	   if(debug()) call ATYPd(Q1,'  Q00   ',k,k,km,km)
	   call RANK_SVD(Q1,k,k,km,km,irank)
	   if(irank.lt.k-1) then
		print 83, irank
	      if(discprt) write(8,83) irank
83		format(/,' WARNING',/,
     & ' The rank of the Q matrix seems to be ',i2,', less than k-1.',/,
     & ' This suggest that the mechanism has disconnected states,',/,
     & ' so equilibrium occupancies can''t be calculated.')
		STOP
	   endif
	   call EQOCC2(Q1,p00,k,km)
	   call EQOCCd(Q1,k,k-1,km,P00)		!calc equilib occs at xA00
	   xA00=conc00(nljump)
	   if(qprt) then
      	print 50,xa00*1.e6
      	if(discprt) write(8,50) xa00*1.e6
50		format(/,' Q matrix before t=0, at concentration = ',g13.6)
	      call ATYPD(Q1,' Q(00)  ',k,k,km,km)
		call CALCPI(Q1,PImat,k,km,km)
	      call ATYPD(pimat,' PI(00) ',k,k,km,km)
	   endif
	   if(iprint.ge.2) then
	      print 51,xa00*1.e6
	      if(discprt) write(8,51) xa00*1.e6
51		format(/,
     & ' Equilib occupancies before t=0, at concentration = ',g13.6)
		do 52 j=1,k
		print 53,j,p00(j)
	      if(discprt) write(8,53) j,p00(j)
53		format(' p00(',i2,') = ',g13.6)
52		continue
		if(iprint.ge.3) then
c		   call SLIFEd1(Q1,p00,km,km)
c		   call SLIFED2(Q1,P00,pon(),discprt,km,km)
		   call SLIFED2(Q1,P00,.false.,discprt,km,km)
		endif
	   endif
c
c In prepulse case the bit above gives equilibrium conditions (tpre -> infinity)
c for the prepulse; now do all calculations for prepulse in CALPREP. Q1 is
c Q matrix at prepulse conc -must get occs before prepulse (conc=0)
c in CALPREP.  Also get new P00() = occupancies at time=tpre, i.e. the
c initial condition for the pulse (need option to calc overall P00, or P00 for
c cases where (a) there are no openings during prepulse or (b) there is at
c least one opening during prepulse). Before call, p00 is equilib occ at
c the prepulse conc.  Returns occupancies at end of prepulse (=init
c condition for pulse) (a) overall in pt0(), copied to p00();
c (b) given no opening, in p00z() and (c) given at least one opening, in p00n()
	   if(prepulse) then
		kF=k-kA
		call CALPREP(tpre,QT,p00,pt0,nlig,IL,
     &	 conc000,conc00,conc0,conc,nljump,vhold,vref,
     &	 km,k,kA,kF,iprint,.false.)
c		call CALPREP(tpre,QT,p00,pt0,nlig,IL,
c     &	 conc000,conc00,conc0,conc,nljump,vhold,vref,
c     &	 km,k,kA,kF,iprint,pon())
c		call CALPREP(tpre,QT,p00,pt0,p00z,p00n,nlig,IL,
c     &	 conc000,conc00,conc0,conc,nljump,vhold,vref,
c     &	 km,k,kA,kF,iprint,pon())
c	      Copy pt0() into p00 so latter now contains initial occs for
c		the pulse in case of prepulse (as it does in simple pulse
c		case in which p00() are equilib occs at conc before pulse)
		do i=1,k
		   p00(i)=pt0(i)
		enddo
	   endif
c
c Calc Pinf at xA0, and Q matrix for this conc (conc during pulse, for pulse
c calcs)
c===	   call QNEWC(QD,cA1,cB1,Q1,xA0,xB0,ncdep,nlig,IL,IX,JX,k,km)
	   V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	   call QSETD(conc0,IL,V1,QT,Q1,k,.false.)
	   if(debug()) call ATYPD(Q1,' Q1     ',k,k,km,km)
	   call RANK_SVD(Q1,k,k,km,km,irank)
	   if(irank.lt.k-1) then
		print 83, irank
	      if(discprt) write(8,83) irank
c83		format(
c     & ' The rank of the Q matrix seems to be ',i2,', less than k-1.',/,
c     & ' This suggest that the mechanism has disconnected states,',/,
c     & ' so equilibrium occupancies can''t be calculated.')
		STOP
	   endif
	   call EQOCCd(Q1,k,k-1,km,Pinf0)	!calc equilib occs at xA0
	   if(qprt) then
		xA0=conc0(nljump)
      	print 54,xa0*1.e6
      	if(discprt) write(8,54) xa0*1.e6
54		format(/,' Q matrix at concentration = ',g13.6)
	      call ATYPD(Q1,'  Q(0)  ',k,k,km,km)
		call CALCPI(Q1,PImat,k,km,km)
	      call ATYPD(pimat,' PI(0)  ',k,k,km,km)
	   endif
	   if(iprint.ge.2) then
      	print 55,xa0*1.e6
      	if(discprt) write(8,55) xa0*1.e6
55  		format(/,' Equilibrium occupancies at concentration = ',g13.6)
		do j=1,k
		   print 57,j,pinf0(j)
	         if(discprt) write(8,57) j,pinf0(j)
57		   format(' pinf0(',i2,') = ',g13.6)
		enddo
		if(iprint.ge.3) then
c		   call SLIFED2(Q1,Pinf0,pon(),discprt,km,km)
		   call SLIFED2(Q1,Pinf0,.false.,discprt,km,km)
		endif
	   endif
	   call QMAT5(Q1,Amat,k,eigen0,ibad,km,km,km)
c NB Pinf0 should be same as P0 from disc, if xA0 not changed
	   if(ibad.ne.0) print 14,ibad
14	   format(' ***ERROR IN SPECTRAL EXPANSION: ibad= ',i4)
	   if(debug()) then
		print 700
700		format(' Print spectral expansion matrices [N] ? ')
	      ans='N'
	      call INPUTa(ans)
		if(UC(ans).eq.'Y') then
		   do m=1,k
			call ATYPD3(amat,m,'A matrix',k,k,km,km,km)
		   enddo
		endif
	   endif
c
c ON-JUMP FOR PULSES (+CALC OF INTIAL CONDITION FOR OFF-JUMP)
c Now calc p(t0)=initial condition for the jump, in pt0 (for pulses, this
c is initial condition for the off-jump). Returns also the
c coefficients b(m,j) of the mth exponential component for p(j)
c of the ON jump in case of pulse)
c NB next bit done for simple jump, or for pulse when at equilib before
c the pulse, but not for general prepulse case which is done separately
c below
	   if(.not.prepulse) then
		call PTCALC(pt0,t0,bmj0,p00,pinf0,eigen0,amat,k,km)
		do j=1,k
	         p0(j)=pt0(j)	!new initial occs (for OFF jump of pulse)
		enddo
		if(iprint.ge.2) then
	         print 581,t0*1000.,xa0*1.e6
	         if(discprt) write(8,581) t0*1000.,xa0*1.e6
581		   format(/,
     &' Occupancies at t0 = ',g13.6,' ms at concentration = ',g13.6)
		   do j=1,k
			print 60,j,p0(j)
			if(discprt) write(8,60) j,p0(j)
60			format(' p0(',i2,') = ',g13.6)
		   enddo
		endif
	   endif
c
c For pulses print tau etc for the ON relaxation (use coeffs in bmj0() found
c above in PTCALC)
c (do 3 times for prepulse case, for each init vector)
	   if(pulse) then
		if(.not.prepulse) then
		   print 331
	         if(discprt) write(8,331)
331		   format(/,' ON-relaxation')
		   call RELOUT(cfac,dgamma,pinf0,bmj0,eigen0,t0,bm0,tau0,
     &	    cur00,curinf0,cur0tot,bmrel,atotd,nlig,ligname,.false.)
c calculate area (including asymptote term) from t=0 to t=tpulse
		   t0d=1.d3*dble(t0)		!time in ms
		   a0=0.0d0
		   do m=1,k-1
			a0=a0 + cur00(m)*tau0(m)*(1.d0 - dexp1(-t0d/tau0(m)))
		   enddo
		   a0=a0 + curinf0*t0d		!add asymptote term

		else if(prepulse) then

c         -have eigen0 already, but need to define p0() (3 versions) and
c		 corresponding bmj0(); do unconditional case last
c		 so p0() and bmj0() refer to this case
		   iloop=1	!count 3 cycles
335		   continue
		   if(iloop.eq.1) then		!no ops during prepulse
  			call PTCALC(pt0,t0,bmj0z,p00z,pinf0,eigen0,amat,k,km)
			do j=1,k
	      	   p0z(j)=pt0(j)	!new initial occs (for OFF jump of pulse)
			enddo
			print 333
	      	if(discprt) write(8,333)
333			format(/,
     &		' ON-relaxation given NO openings during prepulse')
		      call RELOUT(cfac,dgamma,pinf0,bmj0z,eigen0,t0,bm0,
     &	       tau0,cur00,curinf0,cur0tot,bmrel,atotd,nlig,
     &		 ligname,.false.)
		   else if(iloop.eq.2) then
  			call PTCALC(pt0,t0,bmj0n,p00n,pinf0,eigen0,amat,k,km)
			do j=1,k
	      	   p0n(j)=pt0(j)	!new initial occs (for OFF jump of pulse)
			enddo
			print 334
	      	if(discprt) write(8,334)
334			format(/,
     &		' ON-relaxation given SOME openings during prepulse')
		      call RELOUT(cfac,dgamma,pinf0,bmj0n,eigen0,t0,bm0,
     &		 tau0,cur00,curinf0,cur0tot,bmrel,atotd,nlig,
     &		 ligname,.false.)
		   else if(iloop.eq.3) then
  			call PTCALC(pt0,t0,bmj0,p00,pinf0,eigen0,amat,k,km)
			do j=1,k
	      	   p0(j)=pt0(j)	!new initial occs (for OFF jump of pulse)
			enddo
			print 331
	      	if(discprt) write(8,331)
		      call RELOUT(cfac,dgamma,pinf0,bmj0,eigen0,t0,bm0,
     &		 tau0,cur00,curinf0,cur0tot,bmrel,atotd,nlig,
     &		 ligname,.false.)
		   endif
c
		   iloop=iloop+1
		   if(iloop.le.3) goto 335
		endif
c NB the real*8 cur00(100),tau0(100),curinf0 are values for 'on relaxation' that
c are kept separately for drawing display when 'pulse' is calc
	   endif
	endif		!end of "else if(cjump.and.calinit) then"
c
c For option 7 (d/r curves) calculate conc loop here (while values for QNEWC
c are available) and define xcal(), ycal() here, then skip to SCDISP
c -also make outer loop for range of k(+1) values (or other specified
c rate consts)?
c Modif 06/28/01 05:16pm to add Hill slope estimate in j=9
	if(drcurve) then
	   if(.not.prevar) then	!must re-calc initial condition for each conc
c        First print the initial condition (same for all conc) just calc
		print 76,(p0(j),j=1,k)
      	if(discprt) write(8,76)(p0(j),j=1,k)
76		format(' p(0) = ',10f8.5)
	   endif
	   ncalc=512
	   ndc1=ncalc
c===ndimc=??
c	   ndimc=15
	   ndimc=3+nlig+k
	   if(ndimc.lt.10) ndimc=10
	   if(.not.allocated(xcal)) then
		ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
		do i=1,ndc1
		   do j=1,ndimc
			xcal(i,j)=0.	!problem with undefined values?
			ycal(i,j)=0.	!problem with undefined values?
		   enddo
		enddo
	   endif
	   if(.not.allocated(xcalsav)) then
		ALLOCATE(Xcalsav(ndc1),Ycalsav(ndc1))
	   endif
	   if(plotocc) then
		do n=1,nlig
		   bmax(n)=-1.d200
		   do i=1,k
			bound(i,n)=dble(float(nbound(i,n)))
			if(bound(i,n).gt.bmax(n)) bmax(n)=bound(i,n)
		   enddo
		enddo
	   endif
c   First calc the conc to be used -say 512 values equally spaced on log scale
c    Find the domimant rate constant for each conc
c	-for Popen this is one with the largest positive amplitude (bm(m)
c     -for current, best choose as the largest amplitude with the same sign
c	 as cfac (i.e. opening rather than desens rate constant)
c Following code should be same for varying rate constant, except that only
c post-jump conce changes (p0 all same) whereas changoing rate constant
c menas that p0 must be recalc each time
c NB if prevar=T: fixed postjump conc already in conc(), change only conc0(nlvar)
c NB if prevar=F: fixed pre-jump conc already in conc0(), change only conc(nlvar)
	   axlo=alog10(xlo)
	   axhi=alog10(xhi)
	   adx=(axhi-axlo)/float(ncalc-1)
c	   radx=10.d0**adx
c	   radx=radx-1.d0
	   if(varate) then
c		call GETIJ(IQ,k,ipv,jpv,npv)		!get i,j for rate constant #npv
		ipv=irate(npv)
		jpv=jrate(npv)
	   else		!conc varies
		if(.not.prevar) then
	         V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	         call QSETD(conc0,IL,V1,QT,Q1,k,.false.)
	   call RANK_SVD(Q1,k,k,km,km,irank)
	   if(irank.lt.k-1) then
		print 83, irank
	      if(discprt) write(8,83) irank
c83		format(
c     & ' The rank of the Q matrix seems to be ',i2,', less than k-1.',/,
c     & ' This suggest that the mechanism has disconnected states,',/,
c     & ' so equilibrium occupancies can''t be calculated.')
		STOP
	   endif
		   call EQOCCd(Q1,k,k-1,km,p0)		!calc new init occs at xA0
		   print 76,(p0(j),j=1,k)
      	   if(discprt) write(8,76)(p0(j),j=1,k)
c76		   format(' p(0) = ',10f8.5)
		endif
	   endif
c  Loop for values of conc (or rate constant)
	   do i=1,ncalc
		x=axlo + float(i-1)*adx		!ith concentration/rate const value
c First do pre-jump -calculate p0()
		if(varate) then	!new value for rate constant
		   QT(ipv,jpv)=10.**x		!new rate constant
c==		   call QNEWC(QD,cA1,cB1,Q1,xA0,xB0,ncdep,nlig,IL,IX,JX,k,km)
		   V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
		   call QSETD(conc0,IL,V1,QT,Q1,k,.false.)
	   call RANK_SVD(Q1,k,k,km,km,irank)
	   if(irank.lt.k-1) then
		print 83, irank
	      if(discprt) write(8,83) irank
c83		format(
c     & ' The rank of the Q matrix seems to be ',i2,', less than k-1.',/,
c     & ' This suggest that the mechanism has disconnected states,',/,
c     & ' so equilibrium occupancies can''t be calculated.')
		STOP
	   endif
		   call EQOCCd(Q1,k,k-1,km,p0)	!calc new init occs at xA0
		else		!conc varied
		   if(prevar) then     !must re-calc initial condition for each conc
			x=10.**x	     !new initial conc
			conc0(nlvar)=x*1.e-6	!molar
			xV1=x			!for print
			V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
			call QSETD(conc0,IL,V1,QT,Q1,k,.false.)
	   call RANK_SVD(Q1,k,k,km,km,irank)
	   if(irank.lt.k-1) then
		print 83, irank
	      if(discprt) write(8,83) irank
c83		format(
c     & ' The rank of the Q matrix seems to be ',i2,', less than k-1.',/,
c     & ' This suggest that the mechanism has disconnected states,',/,
c     & ' so equilibrium occupancies can''t be calculated.')
		STOP
	   endif
			call EQOCCd(Q1,k,k-1,km,p0)		!calc new init occs at xA0
			print 76,(p0(j),j=1,k)
      		if(discprt) write(8,76)(p0(j),j=1,k)
c76			format(' p(0) = ',10f8.5)
		   endif
		   if(gaddum.and.nlig.eq.2.and.nlvar.eq.1) then
			tauzero=1000./(sngl(q1(2,3)) + sngl(q1(3,2)))
		   endif
		endif
c
c          Now post-jump
c if(.not.prevar) then post jump conc varies so set conc(nlvar) here
c NB if prevar=F: fixed pre-jump conc already in conc0(), change only conc(nlvar)
		if(.not.prevar) then     !must re-calc initial condition for each conc
		   x=10.**x	     !new initial conc
		   conc(nlvar)=x*1.e-6	!molar
		   xV1=x			!for print =muM
c===		   call QNEWC(QD,cA1,cB1,Q1,xA1,xB1,ncdep,nlig,IL,IX,JX,k,km)
		   V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
		   call QSETD(conc,IL,V1,QT,Q1,k,.false.)
		endif
	   call RANK_SVD(Q1,k,k,km,km,irank)
	   if(irank.lt.k-1) then
		print 83, irank
	      if(discprt) write(8,83) irank
c83		format(
c     & ' The rank of the Q matrix seems to be ',i2,', less than k-1.',/,
c     & ' This suggest that the mechanism has disconnected states,',/,
c     & ' so equilibrium occupancies can''t be calculated.')
		STOP
	   endif
		call EQOCCd(Q1,k,k-1,km,Pinf)	!calc equilib occs at xA1
		call QMAT5(Q1,Amat,k,eigen,ibad,km,km,km)
c==		call QMAT(Q1,Amat,k,eigen,ibad)
	      call PTCALC(pt,0.0,bmj,p0,pinf,eigen,amat,k,km)
		curinf=0.0d0	!actually total Popen here
		if(.not.plotocc) then	!calculate Popen
		   do j=1,kA
			if(plotcur) then
			   curinf=curinf+cfacd*dgamma(j)*pinf(j)	!current
			else
			   curinf=curinf + pinf(j)			!Popen
			endif
		   enddo
		else 				!calculate binding occupancy
		   n=nlplot		!the ligand to be plotted (=1 or 2)
		   curinf=0.0d0		!equilib binding occ
		   do j=1,k
			curinf=curinf + bound(j,n)*pinf(j)
		   enddo
		   curinf=curinf/bmax(n)
		endif
		curinfs=sngl(curinf)		!for expmax
		ncomp1=k-1				!for expmax
c		wmin=1.0d20
		wmax=-1.0d20
		do m=1,k-1
      	   tau(m)=-1.d3/eigen(m)
		   bm(m)=0.0d0
		   if(.not.plotocc) then		!calculate Popen
			do j=1,kA
			   if(plotcur) then
		  		bm(m)=bm(m) + dgamma(j)*bmj(m,j)	!for current
			   else
				bm(m)=bm(m) + bmj(m,j)			!for Popen
			   endif
			enddo
		   else			!calc coeffs for binding
			do j=1,k
			   bm(m)=bm(m) + bound(j,n)*bmj(m,j)
			enddo
			bm(m)=bm(m)/bmax(n)
		   endif
		   if(plotcur) then
			cur(m)=cfacd*bm(m)			!for current
		   else
			cur(m)=bm(m)				!for Popen or binding
		   endif
c At present this calculates the most negative amplitude -OK only for
c currents (at negative pots) and even then arguably abs amplitude matters more?
c=		   if(cur(m).lt.wmin) then
		   if(dabs(cur(m)).gt.wmax) then
			tbig=tau(m)
			wmax=cur(m)
		   endif
		enddo		!end of m=1,k-1
		deb=debug()
		prt=.false.
		thigh=-1.d0	!so set internally
c expmaxd uses cur(m), tau(m) curinf, ncomp1 in common/expd/ to find peak current/Popen
		call EXPMAXd(tpeak,ypeak,curinf,thigh,texp,
     &	   prt,discprt,deb,km,nerr,nerr1,nerr2,0)
		if(nerr.eq.-5) then
	         print 12,xv1,ypeak
	         if(discprt) write(8,12) xv1,ypeak
12		   format(' At conc = ',g13.6,
     &	' micromolar: monotonic, asymptote = ',g13.6)
c=		   pause
		else if(nerr.ne.0.or.nerr1.ne.0.or.nerr2.ne.0) then
	         print 13,nerr,nerr1,nerr2,xv1
	         if(discprt) write(8,13) nerr,nerr1,nerr2,xv1
13		   format(' Error: nerr,nerr1,nerr2 = ',3i3,
     &	    ' at conc = ',g13.6,' micromolar')
c=		   pause
		else if(nerr.eq.0) then
	         print 131,xv1,ypeak,tpeak
	         if(discprt) write(8,131) xv1,ypeak,tpeak
131		   format(
     &	  ' At c = ',g13.6,' muM, peak = ',g13.6,' at ',g13.6,' ms')
		endif
	      print 132,tbig,texp
	      if(discprt) write(8,132) tbig,texp
132		format(
     &	  ' tau(amax) = ',g13.6,' texp = ',g13.6)
		if(varate) then
		   do j=1,4
			Xcal(i,j)=sngl(qt(ipv,jpv))
		   enddo
		else
		   do j=1,4
			Xcal(i,j)=xv1            !conc in micromolar
		   enddo
		endif
		Ycal(i,1)=curinfs			!equilib Popen or current in j=1
		Ycal(i,2)=sngl(ypeak)		!peak Popen or current in j=2
		if(plotcur) then
		   Ycal(i,1)=abs(Ycal(i,1))	!plot current as positive
		   Ycal(i,2)=abs(Ycal(i,2))	!plot current as positive
		endif
		if(plotrate) then
		   rbig=1.d3/tbig			!1/sec
		   if(dabs(texp).gt.1.d-10) then
			rexp=1.d3/texp		!1/sec
		   else
			rexp=0.0d0
		   endif
		else
		   rbig=tbig			!ms
		   rexp=texp
		endif
		Ycal(i,3)=sngl(rbig) !predominant rate constant (1/sec), or tau (ms)
		Ycal(i,4)=sngl(rexp) !empirical 'rate constant' (1/sec), or 'tau' (ms)
		if(i.gt.1) then
		   xcal(i-1,5)=0.5*(xcal(i,1)+xcal(i-1,1))	!mid conc
		   deltx=xcal(i,1)-xcal(i-1,1)	! micromolar
c		   deltx=dble(xcal(i-1,1))*radx
		   delty=rbig-rbiglast		! 1/tbig; 1/sec
		   ycal(i-1,5)=sngl(delty/deltx)
		   xcal(i-1,6)=xcal(i-1,5)		!mid conc
		   delty=rexp-rexplast			! 1/texp; 1/sec
		   ycal(i-1,6)=sngl(delty/deltx)
		   if(gaddum) then
			xcal(i-1,7)=xcal(i-1,5)
			ycal(i-1,7)=tauzero/ycal(i-1,5)	!estimate of KA
			xcal(i-1,8)=xcal(i-1,5)
			ycal(i-1,8)=tauzero/ycal(i-1,6)	!estimate of KA
		   endif
		endif
		rbiglast=rbig
		rexplast=rexp
c	Now find dominant rate constant for each conc, for use in plotrate case
c	Amplitudes are in bm(m) or ws(m)
c     Also find empirically the time, texp, for 63.21% of equilib value to be achieved
c	Have PROBLEM if response goes through a peak -the largest amplitude
c       could be the decay phase!
	   enddo	!end of i=1,ncalc
c
c Try new EC50 subroutines
	   kdim=100
	   call EQEC50(EC50,curinfs,monot,curmax,concmax,decline,
     &  curr0,pop0,QT,conc,nlvar,vhold,vref,cfacd,dgamma,plotcur,k,kdim)
	   if(monot) then
		if(.not.plotocc) then
		   if(plotcur) then
			print 591,curinfs,ec50*1.e6
      		if(discprt) write(8,591) curinfs,ec50*1.e6
591	      	format(
     & ' Equilibrium response-concentration curve is monotonic',/,
     & ' Maximum response (pA) = ',g13.6,/,
     & '    Conc for 50% of this maximum (EC50) (muM) = ',g13.6,/)
		   else
			print 5912,curinfs,ec50*1.e6
      		if(discprt) write(8,5912) curinfs,ec50*1.e6
5912	      	format(/,
     & ' Equilibrium response-concentration curve is monotonic',/,
     & ' P(open) at ''infinite'' conc = ',g13.6,/,
     & '    Conc for 50% of this maximum (EC50) (muM) = ',g13.6,/)
		   endif
		else if(plotocc) then
		   curinfs=1.0	!always for binding
		   print 5911,curinfs,ec50*1.e6
      	   if(discprt) write(8,5911) curinfs,ec50*1.e6
5911	   	   format(/,
     & ' Equilibrium binding-concentration curve is monotonic',/,
     & ' Maximum occupancy = ',g13.6,/,
     & '    Conc for 50% of this maximum (EC50) = ',g13.6,/)
		endif
		ec50eq=ec50*1.e6	!micromolar
	   else	!not monotonic (never true for binding curve)
		if(plotcur) then
		   print 592,curmax,concmax*1.e6,ec50*1.e6,curinfs
      	   if(discprt)write(8,592)curmax,concmax*1.e6,ec50*1.e6,
     &		curinfs
592		   format(/,
     &    ' Equilibrium response-concentration curve has maximum.',/,
     &    '   Max equilib response = ',g12.5,' pA at ',g12.5,' muM',/,
     &    '   Conc for 50% of this max. current (muM) (left of max) = ',
     &     g12.5,/,
     &    '   Response at conc -> infinity = ',g12.5,' pA',/)
		else
		   print 5921,curmax,concmax*1.e6,ec50*1.e6,curinfs
      	   if(discprt)write(8,5921)curmax,concmax*1.e6,ec50*1.e6,
     &		curinfs
5921		   format(/,
     &    ' Equilibrium response-concentration curve has maximum.',/,
     &    '   Max equilib Popen = ',g12.5,' at ',g12.5,' muM',/,
     &    '   Conc for 50% of this max. Popen (muM) (left of max) = ',
     &     g12.5,/,
     &    '   Response at conc -> infinity = ',g12.5,' pA',/)
		endif
	   endif
c and same for peak d/r curve (if eqec50 indicates that curves not monotonic)
c No -do whether or not monotonic
c	   if(.not.monot) then
c	   if(.not.decline) then
	    call PEAKEC50(EC50,curinfs,monot,curmax,concmax,QT,
     &    p0,cfacd,dgamma,plotcur,conc,nlvar,vhold,vref,debug(),k,kdim)
	    if(abs(curinfs).lt.0.99*abs(curmax)) monot=.false.
	    if(monot) then
		if(.not.plotocc) then
		   if(plotcur) then
      		print 593,curinfs,ec50*1.e6
      		if(discprt) write(8,593) curinfs,ec50*1.e6
593	   		format(/,
     & ' Peak response-concentration curve is monotonic',/,
     & ' Maximum response (pA) = ',g13.6,/,
     & '    Conc for 50% of this maximum (EC50) = ',g13.6,' (muM)',/)
		   else
      		print 5932,curinfs,ec50*1.e6
      		if(discprt) write(8,5932) curinfs,ec50*1.e6
5932	   		format(/,
     & ' Peak response-concentration curve is monotonic',/,
     & ' Maximum Popen = ',g13.6,/,
     & '    Conc for 50% of this maximum (EC50) = ',g13.6,' (muM)',/)
		   endif
		else if(plotocc) then
		   curinfs=1.0	!always for binding
      	   print 5931,curinfs,ec50*1.e6
      	   if(discprt) write(8,5931) curinfs,ec50*1.e6
5931	   	   format(/,
     & ' Peak binding-concentration curve is monotonic',/,
     & ' Maximum occupancy = ',g13.6,/,
     & '    Conc for 50% of this maximum = ',g13.6,' (muM)',/)
		endif
	    else
		if(plotcur) then
		   print 594,curmax,concmax*1.e6,ec50*1.e6,curinfs
      	   if(discprt)write(8,594)curmax,concmax*1.e6,ec50*1.e6,
     &		curinfs
594		   format(/,
     &    ' Peak response-concentration curve has maximum.',/,
     &    '   Max peak response = ',g12.5,' pA at ',g12.5,' muM',/,
     &    '   Conc for 50% of this max. current (left of max) = ',
     &     g12.5,' muM',/,
     &    '   Response at conc -> infinity = ',g12.5,' pA',/)
		else
		   print 5942,curmax,concmax*1.e6,ec50*1.e6,curinfs
      	   if(discprt)write(8,5942)curmax,concmax*1.e6,ec50*1.e6,
     &		curinfs
5942		   format(/,
     &    ' Peak response-concentration curve has maximum.',/,
     &    '   Max Popen = ',g12.5,' at ',g12.5,' muM',/,
     &    '   Conc for 50% of this max. Popen (left of max) = ',
     &     g12.5,' muM',/,
     &    '   Response at conc -> infinity = ',g12.5,' pA',/)
		endif
	    endif
	    yinfp=abs(curmax)	!maximum peak response/binding (for Hill slopes)
	    if(decline) then
		yinfp=abs(curr0)
		y00=abs(curmax)
	    endif
c	   endif			!end of peakec50
	   yinfeq=abs(curinfs)	!maximum Popen/current/binding (for Hill slopes)
c	   y00=0.0			!minimum response/binding (for Hill slopes)
	   y00=abs(curr0)		!minimum Popen/current/binding (for Hill slopes)
	   if(decline) then
		if(plotcur) then
		   yinfeq=abs(curr0)
		else
		   yinfeq=abs(pop0)
		endif
		y00=abs(curinfs)
	   endif
	   ec50p=ec50*1.e6	!micromolar
c Put Hill slope into xcal(i,9),ycal(i,9) before SCDISP.  NB if monotonic
c peak and equlibrium curves will probably have same max (and should always
c have same max =1 for binding curves).
c Thus do Hill slopes only for equilib d/r curve (which will not sag normally,
c and will be same as peak curve for cases without desens/block) -in ycal(i,1)
	   n=0		!number of calc values
	   i50=0		!in case not defined
c skip points above 99% of max (gets lost in rounding errors)
	   do i=1,ncalc-1
		if((ycal(i,1).gt.1.e-10.and.xcal(i,1).gt.1.e-10).and.
     &	  (abs((yinfeq-ycal(i+1,1))/yinfeq).gt.0.01)) then
		   n=n+1
		   y1=alog((ycal(i,1)-y00)/(yinfeq-ycal(i,1)))
		   y2=alog((ycal(i+1,1)-y00)/(yinfeq-ycal(i+1,1)))
		   x1=xcal(i,1)
		   x2=xcal(i+1,1)
c		   if(x1.le.ec50eq.and.x2.ge.ec50eq) i50=i
c ===i50 corrected 12/10/04 12:27pm -it should be index in ycal(i,9)
		   if(x1.le.ec50eq.and.x2.ge.ec50eq) i50=n
		   x1=alog(x1)
		   x2=alog(x2)
		   ycal(n,9)=(y2-y1)/(x2-x1)
		   xcal(n,9)=(xcal(i,1)+xcal(i+1,1))/2.0		!not log
		endif
	   enddo
	   nchill=n		!number of calc values
c
	   if(i50.gt.0) then	!interpolate linearly
		b=(ycal(i50+1,9)-ycal(i50,9))/(xcal(i50+1,9)-xcal(i50,9))
		h50=ycal(i50,9) + b*(ec50eq-xcal(i50,9))
	      print 133,ec50eq,h50
	      if(discprt) write(8,133) ec50eq,h50
133		format(/,
     & ' Hill slope at equilibrium EC50 (',g13.6,' muM) = ',g13.6)
	   endif
c
c Repeat all of this for peak d/r in ycal(i,2), with result in yval(i,10)
	   if(.not.decline) then

	    n=0		!number of calc values
	    i50=0		!in case not defined
c skip points above 99% of max (gets lost in rounding errors)
	    do i=1,ncalc-1
		if((ycal(i,2).gt.1.e-10.and.xcal(i,2).gt.1.e-10).and.
     &	  ((yinfp-ycal(i+1,2))/yinfp.gt.0.01)) then
		   n=n+1
		   y1=alog((ycal(i,2)-y00)/(yinfp-ycal(i,2)))
		   y2=alog((ycal(i+1,2)-y00)/(yinfp-ycal(i+1,2)))
		   x1=xcal(i,2)
		   x2=xcal(i+1,2)
c ===i50 corrected 12/10/04 12:27pm -it should be index in ycal(i,10)
		   if(x1.le.ec50p.and.x2.ge.ec50p) i50=i
		   if(x1.le.ec50p.and.x2.ge.ec50p) i50=n
		   x1=alog(x1)
		   x2=alog(x2)
		   ycal(n,10)=(y2-y1)/(x2-x1)
		   xcal(n,10)=(xcal(i,2)+xcal(i+1,2))/2.0		!not log
		endif
	    enddo
	    nchill2=n		!number of calc values
c
	    if(i50.gt.0) then	!interpolate linearly
		b=(ycal(i50+1,10)-ycal(i50,10))/
     &		(xcal(i50+1,10)-xcal(i50,10))
		h50=ycal(i50,10) + b*(ec50p-xcal(i50,10))
	      print 1331,ec50p,h50
	      if(discprt) write(8,1331) ec50p,h50
1331		format(/,
     & ' Hill slope at EC50 for peak (',g13.6,' muM) = ',g13.6)
	    endif
	   endif	!end of 'if(.not.decline)'
c
	   goto 451		!straight to SCDISP
	endif		!end of drcurve section
c
c Recalc final occs at xA1,xB1 -now conc(i) -(in case conc changed), and
c expand Q (for a pulse these are for the final OFF-jump)
c==	call QNEWC(QD,cA1,cB1,Q1,xA1,xB1,ncdep,nlig,IL,IX,JX,k,km)
      V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
      call QSETD(conc,IL,V1,QT,Q1,k,.false.)
	   call RANK_SVD(Q1,k,k,km,km,irank)
	   if(irank.lt.k-1) then
		print 83, irank
	      if(discprt) write(8,83) irank
c83		format(
c     & ' The rank of the Q matrix seems to be ',i2,', less than k-1.',/,
c     & ' This suggest that the mechanism has disconnected states,',/,
c     & ' so equilibrium occupancies can''t be calculated.')
		STOP
	   endif
	call EQOCCd(Q1,k,k-1,km,Pinf)	!calc equilib occs at xA1
	if(qprt) then
61	   format(/,' Q matrix at concentration = ',g13.6)
	   call ATYPD(Q1,' Q(inf) ',k,k,km,km)
	   call CALCPI(Q1,PImat,k,km,km)
	   call ATYPD(pimat,' PI(inf)',k,k,km,km)
	endif
	if(iprint.ge.2) then
	   print 62
	   if(discprt) write(8,62)
62	   format(/,
     & ' Final equilibrium occupancies at concentration = ')
	   do i=1,nlig
		print 621,i,conc(i)*1.e6,ligname(i)
		if(discprt) write(8,621) i,conc(i)*1.e6,ligname(i)
621		format(1x,i2,':',g13.6,' micromolar ',a20)
	   enddo
	   do j=1,k
		print 64,j,pinf(j)
      	if(discprt) write(8,64) j,pinf(j)
64		format(' pinf(',i2,') = ',g13.6)
	   enddo
	   if(iprint.ge.3) then
c	      call SLIFEd1(Q1,pinf,km,km)
		call SLIFED2(Q1,Pinf,.false.,discprt,km,km)
	   endif
	endif
	call QMAT5(Q1,Amat,k,eigen,ibad,km,km,km)
c Test streamlined QMAT
c==	call QMAT(Q1,Amat,k,eigen,ibad)
c NB Pinf should be same as Pinf from disc, if xA1 not changed
	if(ibad.ne.0) print 14,ibad
	   if(debug()) then
		print 700
c700		format(' Print spectral expansion matrices [Y] ? ')
	      ans='Y'
	      call INPUTa(ans)
		if(UC(ans).ne.'N') then
		   do 702 m=1,k
702		   call ATYPD3(amat,m,'A matrix',k,k,km,km,km)
		endif
	   endif
c
c Now calc the jump (mostly same for both c-jump and v-jump)
c
	if(iopt.eq.1) then
	   print 216
216	   format(' Equilibrium initial occupancies:')
	   do j=1,k
	      print 217,j,p0(j)
217	      format(' p0(',i2,') = ',g13.6)
	   enddo
	   call DCASK('Replace this P(0) with a new one','n',ans)
	   if(ans.eq.'Y') then
	    s=0.0d0
	    do i=1,k-1
		print 113,i
113		format(' P0(',i2,') = ')
		call INPUTr(dum)
		p0(i)=dble(dum)
		s=s+p0(i)
	    enddo
	    p0(k)=1.0d0 - s
	   endif
	endif
c
    	if(jump) then
	   print 26
         if(discprt) write(8,26)
26	   format(/,' Initial and final occupancies:')
	   do 24 j=1,k
	   print 25,j,p0(j),j,pinf(j)
         if(discprt) write(8,25) j,p0(j),j,pinf(j)
25	   format(' p0(',i2,') = ',g13.6,3x,' pinf(',i2,') = ',g13.6)
24	   continue
	   if(ncdep.gt.0) then
		ans='N'
		call DCASK('Change the concentration',ans,ans)
		if(ans.eq.'Y') goto 102
	   endif
	else			!if noise (= not jump) do QMAT here
c===Qmat and pinf already done at line 1435 (above label 702)
c	   call QMAT5(QD,Amat,k,eigen,ibad,km,km,km)
c	   print 261
c         if(discprt) write(8,261)
c261	   format(' Equilibrium occupancies:')
c	   do 241 j=1,k
c	   print 251,j,pinf(j)
c         if(discprt) write(8,251) j,pinf(j)
c251	   format(' pinf(',i2,') = ',g13.6)
c241	   continue
	endif
c
c
c Also print relative amplitudes (and fc,areas for spectra) here
	if(jump) then
c	     Use PTCALC to get the bmj(m,j) coefficients for relaxation
c	     of p(j) (t value irrelevant here)
	   call PTCALC(pt,0.0,bmj,p0,pinf,eigen,amat,k,km)
c	     Calc the bm(m) coefficients for relaxation of current (CH77, eq 33)
c Print final current (=off jump for pulses)
	   if(pulse) then
		if(.not.prepulse) then
		   print 332
	         if(discprt) write(8,332)
332		   format(/,' OFF-relaxation')
	         call RELOUT(cfac,dgamma,pinf,bmj,eigen,0.0,bm,tau,
     &	    cur0,curinf,cur0tot,bmrel,atotd,nlig,
     &	    ligname,.false.)
   		   print 9,t0d,a0,t0d,atotd,a0+atotd
		   if(discprt) write(8,9) t0d,a0,t0d,atotd,a0+atotd
9	   	format(' Area (pC):',/,
     & 	   '  from t=0 to t = ',f8.2,' ms: = ',g13.6,/,
     &         '  after t = ',f8.2,' ms: = ',g13.6,/,
     & 	   '  total area = ',g13.6,/)

		else if(prepulse) then
		   iloop=1
336		   continue			!loop back for iloop=2,3
		   if(iloop.eq.1) then
			call PTCALC(pt,0.0,bmjz,p0z,pinf,eigen,amat,k,km)
			print 337
	      	if(discprt) write(8,337)
337			format(/,
     &		' OFF-relaxation given NO openings during prepulse')
	      	call RELOUT(cfac,dgamma,pinf,bmjz,eigen,0.0,bm,tau,
     &	 	 cur0,curinf,cur0tot,bmrel,atotd,nlig,
     &	       ligname,.false.)
		   else if(iloop.eq.2) then
			call PTCALC(pt,0.0,bmjn,p0n,pinf,eigen,amat,k,km)
			print 338
	      	if(discprt) write(8,338)
338			format(/,
     &		' OFF-relaxation given SOME openings during prepulse')
	      	call RELOUT(cfac,dgamma,pinf,bmjn,eigen,0.0,bm,tau,
     &	 	 cur0,curinf,cur0tot,bmrel,atotd,nlig,
     &	       ligname,.false.)
		   else if(iloop.eq.3) then
			print 332
	      	if(discprt) write(8,332)
	      	call RELOUT(cfac,dgamma,pinf,bmj,eigen,0.0,bm,tau,
     &	 	 cur0,curinf,cur0tot,bmrel,atotd,nlig,
     &	       ligname,.false.)
		   endif
c
		   iloop=iloop+1
		   if(iloop.le.3) goto 336
		endif
	   else
	      call RELOUT(cfac,dgamma,pinf,bmj,eigen,0.0,bm,tau,
     &	 cur0,curinf,cur0tot,bmrel,atotd,nlig,
     &	 ligname,.false.)
	   endif
	endif
c
c Might as well calc noise anyway, as long as final conc not zero
c Call the coefficients for the mth component (of autocovariance) cm(m)
c to distinguish them from jump coeffs in bm(m) (CH77,eq 41)
C=	xA1=conc(nljump)
C=	if(xA1.lt.1.e-10) goto 45
	curinf=0.0d0
	do j=1,kA
	   curinf=curinf+cfacd*dgamma(j)*pinf(j)
	enddo
	if(dabs(curinf).lt.1.d-10) goto 45 	!no current so skip noise
c Calc total variance directly
	var1=0.0d0
	s=0.0d0
	do i=1,kA
	   var1=var1 + pinf(i)*dgamma(i)*dgamma(i)
	   s=s + pinf(i)*dgamma(i)
	enddo
	var1=cfac2*sngl(var1 - s*s)
c CV(0) is same as AREA, as expected, so do not print it too
c Print time constants etc, then do displays
	pi=3.14159265359d0
	do m=1,k-1
	  fc(m)=-sngl(eigen(m)/(2.d0*pi))
	  tau(m)=-1.d3/eigen(m)
	enddo
	if(jump) then
	   print 421
         if(discprt) write(8,421)
421	   format(/,
     & ' For NOISE spectrum (at equlibrium with post-jump conc):',/,
     & ' Comp    fc(Hz)     tau (ms)   G(0)(pA*pA/Hz)  Area (pA*pA)   ',
     &   'Area (% total)')
	else
	   print 42
         if(discprt) write(8,42)
42	   format(/,' For NOISE spectrum at equilibrium:',/,
     & ' Comp    fc(Hz)     tau (ms)   G(0)(pA*pA/Hz)  Area (pA*pA)   ',
     &   'Area (% total)')
	endif
	cv0tot=0.0
	g0tot=0.0
	var=0.0
	sp=0.0
	do 40 m=1,k-1
	 cm(m)=0.0d0
	 do 41 i=1,kA
	 do 41 j=1,kA
	   cm(m)=cm(m) + pinf(i)*dgamma(i)*dgamma(j)*amat(i,j,m)
41	 continue
	 cv0(m)=cfac2*sngl(cm(m))	!autocovar at t=0
	 cv0tot=cv0tot+cv0(m)
	 G0(m)=4.0*cv0(m)/(-sngl(eigen(m)))       !G(0) for components
	 g0tot=g0tot + G0(m)
	 area(m)=sngl(pi/2.d0)*G0(m)*fc(m)
	 var=var+area(m)
	 p=100.*area(m)/var1
	 sp=sp+p
c	 print 43,m,fc(m),tau(m),G0(m),area(m),cv0(m)
	 print 43,m,fc(m),tau(m),G0(m),area(m),p
       if(discprt) write(8,43) m,fc(m),tau(m),G0(m),area(m),p
43	format(i5,3g13.6,2x,g13.6,2x,g13.6)
40	continue
c	print 44,g0tot,var,cv0tot
	print 44,g0tot,var,sp
      if(discprt) write(8,44) g0tot,var,sp
44	format(' Totals:',23x,g13.6,2x,g13.6,2x,g13.6)
c Print separately calc total variance
	print 47,var1
      if(discprt) write(8,47) var1
47	format(' Total variance (pA*pA) = ',g13.6)
	noise=.true.		!whatever it was set to above
c
c Variance and apparent single channel conductance for particular bandwidths
743	print 74
74	format(/,' Calculate variance between frequencies f1,f2 [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).ne.'Y') goto 45
	print 741
741	format(' f1,f2 (Hz) = ')
	call INPUT2r(fmin1,fmax1)
c	read 75,fmin1,fmax1
c      if(discprt) write(8,742) fmin1,fmax1
c742	format(/,' f1,f2 (Hz)= ',2g13.6)
75	format(2G13.6)
	print 732,fmin1,fmax1
      if(discprt) write(8,732)fmin1,fmax1
732	FORMAT(/,/,
     &' Variance between f1 = ',g13.6,' and f2 = ',g13.6,' Hz',/,
     &'  (and variance as % of total variance in same component)',/,/,
     &' Comp         Perfect filters       Butterworth 8 pole for f2',/,
     &'          variance    variance(%)    variance    variance(%)')
	sv=0.0
	sv1=0.0
	sv2=0.0
	do 731 m=1,k-1
	v=1.570796*G0(m)*fc(m)	!const=pi/2=total variance for this comp
c perfect filter
	v1=g0(m)*fc(m)*(ATAN(fmax1/fc(m))-ATAN(fmin1/fc(m)))
c fmax=Butterworth
	call LORFILT(fmax1,fc(m),R)
	v2=v/R					!var up to fmax
	v2=v2-g0(m)*fc(m)*ATAN(fmin1/fc(m))	  !minus var up to fmin
	sv=sv+v		!total -should be same as var, calc above
	sv1=sv1+v1
	sv2=sv2+v2
	print 733,m,v1,100.*v1/v,v2,100.*v2/v
	if(discprt) write(8,733) m,v1,100.*v1/v,v2,100.*v2/v
733	format(i4,4x,2g13.6,1x,2g13.6)
731	CONTINUE	!end of loop for components
c
c	popen=pinf(1)	!for kA=1 only
c	do 76 i=i,kA
c76	popen=popen+pinf(i)
c	pshut=1.0-popen
	g1=sv1/(curinf*vkin*0.001)
c	g1c=g1/pshut
	g2=sv2/(curinf*vkin*0.001)
c	g2c=g2/pshut
	print 473,fmin1,fmax1,
     & sv1,100.*sv1/sv,g1,100.*g1/gamma(1),
     & sv2,100.*sv2/sv,g2,100.*g2/gamma(1)
      if(discprt) write(8,473)fmin1,fmax1,
     & sv1,100.*sv1/sv,g1,100.*g1/gamma(1),
     & sv2,100.*sv2/sv,g2,100.*g2/gamma(1)
473	format(/,
     &' TOTAL Variance between f1 = ',g13.6,' and f2 = ',g13.6,' Hz',/,
     &' (1) for f2 = perfect filter: variance = ',g13.6,'(= ',
     & f8.3,'%)',/,
     & '   apparent single channel conductance (pS) = ',g13.6,'(= ',
     & f8.3,'%)',/,
     &' (2) for f2 = 8 pole Butterworth: variance = ',g13.6,'(= ',
     & f8.3,'%)',/,
     & '   apparent single channel conductance (pS) = ',g13.6,'(= ',
     & f8.3,'%)')
c
c Calculate apparent conductance corrected for P(open) if all open channels
c have the same conductance
	bad=.false.
	popen=0.0
	do 80 i=1,kA
	 if(i.gt.1.and.abs(gamma(i)-gamma(1)).gt.1.e-6) bad=.true.
	 popen=popen+pinf(i)
80	continue
	if(bad) then
         print 81
         if(discprt) write(8,81)
81	   format(/,
     & ' Cannot correct for P(open) when channel conductances differ')
	   goto 743
	endif
	pshut=1.0-popen
	g1c=g1/pshut
	g2c=g2/pshut
      print 82,g1c,100.*g1c/gamma(1),g2c,100.*g2c/gamma(1)
      if(discprt)write(8,82)g1c,100.*g1c/gamma(1),g2c,100.*g2c/gamma(1)
82	format(/,' Conductance corrected for P(open)',/,
     &' (1) for f2 = perfect filter:',/,
     & '   conductance corrected for P(open) (pS) = ',g13.6,'(= ',
     & f8.3,'%)',/,
     &' (2) for f2 = 8 pole Butterworth:',/,
     & '   conductance corrected for P(open) (pS) = ',g13.6,'(= ',
     & f8.3,'%)')
	call flush(7)
	goto 743
c
45	continue	!jump here to skip noise calcs
c
c In this version curinf,cur0,tau are now real*8
c NB the real*8 cur00(100),tau0(100),curinf0 are values for 'on relaxation' that
c are kept separately fro drawing display when 'pulse' is calc
	ndc1=2048
c=	ndimc=15
	ndimc=2+nlig+k
	if(ndimc.lt.10) ndimc=10
	if(.not.allocated(xcal)) then
	   ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
	   do i=1,ndc1
		do j=1,ndimc
		   xcal(i,j)=0.	!problem with undefined values?
		   ycal(i,j)=0.	!problem with undefined values?
		enddo
	   enddo
	endif
	if(.not.allocated(xcalsav)) then
	   ALLOCATE(Xcalsav(ndc1),Ycalsav(ndc1))
	   do i=1,ndc1
		xcalsav(i)=0.	!problem with undefined values?
		ycalsav(i)=0.	!problem with undefined values?
	   enddo
	endif
451	continue	!jump here for drcurve
	align=.false.	!reset
c Modif 06/28/01 06:14pm: y00 and yinf added for Hill plots
	if(ndimc.lt.15) ndimc=15
	call SCDISP(cjump,vjump,noise,pulse,cfac,pinf,bmj,eigen,
     & pinf0,bmj0,bmrel,eigen0,dgamma,t0,fc,G0,cv0,k,iplot,
     & varconc,xcal,ycal,ntime,dt,xcalsav,ycalsav,ncsav,plotsav,
     & prepulse,bmj0z,bmj0n,bmjz,bmjn,ndc1,ndimc,
     & ncalc,drcurve,Q1,align,bm,plotocc,plotcur,plotrate,
     & varate,titlep,npv,npulse,tpgap,nlig,y00,yinfeq,nchill,
     & nchill2)
c     & varate,titlep,npv,npulse,tpgap,nlig)
	if(allocated(xcal)) DEALLOCATE(xcal,ycal)
c
c
	call DCASK(
     & 'Calculate time course for aligned excised bursts',ans1,ans)
	ans1=ans
	if(ans.eq.'N') goto 99
c Calculations for aligned bursts (as in old scbst)
	ncalc=1024
	ndc1=ncalc
c===ndimc?
c==	ndimc=10
	ndimc=2+nlig+k
	if(.not.allocated(xcal)) then
	   ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
	endif
	print 381
	if(discprt) write(8,381)
381	format(/,
     &' TIME COURSE FOR AVERAGED ALIGNED EXCISED STEADY-STATE BURSTS')
202	continue
	print 201
	if(discprt) write(8,201)
201	format(
     & ' Concentration for steady-state record (muM):')
	if(nlig.gt.0) then
	   conc(1)=xAs		!from .ini
	   do i=1,nlig
		xAs=1.e6*conc(i)
		print 159,i,ligname(il(i)),xAs
c159		FORMAT(/,
c     &     ' Ligand #',i2,1x,a20,': concentration (muM) [',g13.6,'] = ')
		call INPUTr(xAs)
		conc(i)=xAs*1.0e-6
	   enddo
	endif
	V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	call QSETD(conc,IL,V1,QT,Q1,k,.false.)
c==	call QNEWC(QD,cA1,cB1,Q1,xAs,xB0,ncdep,nlig,IL,IX,JX,k,km)
	   call RANK_SVD(Q1,k,k,km,km,irank)
	   if(irank.lt.k-1) then
		print 83, irank
	      if(discprt) write(8,83) irank
c83		format(
c     & ' The rank of the Q matrix seems to be ',i2,', less than k-1.',/,
c     & ' This suggest that the mechanism has disconnected states,',/,
c     & ' so equilibrium occupancies can''t be calculated.')
		STOP
	   endif
	call EQOCCd(Q1,k,k-1,km,pinf)	!calc new init occs at xAs
	call SLIFED2(Q1,pinf,.false.,discprt,km,km)
	ans='Y'
	call DCASK('Is this concentration O.K.',ans,ans)
	if(ans.eq.'N') goto 202
	if(nlig.gt.0) then
	   do i=1,nlig
		if(discprt) write(8,203) i,ligname(il(i)),conc(i)*1.e6
203		FORMAT(/,
     &     ' Ligand #',i2,1x,a20,': concentration (muM) = ',g13.6)
	   enddo
	endif
c
c Calculate initial vector for bursts
	print 2012,kB,kC
2012	format(' kB,kC = ',2i5,'  O.K. [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(ans.eq.'N') then
	   print 2013
2013	   format(' Number of states in sets B,C [',i2,',',i2,'] = ')
	   call INPUT2i(kB,kC)
	endif
	kE=kA+kB
	BA=21
	BB=22
	CA=31
	CB=32
	EE=66		!6=code for E (=burst=A+B)
	one=1.0d0
      call SUBMAT(Q1,BB,Q2,km,km,km,km)		!QBB in Q2
	call MATINV(Q2,kB,km,Q3,km)			!inv(QBB) in Q3
	call SUBMAT(Q1,BA,Q2,km,km,km,km)		!QBA in Q2
	call MATMUL(Q3,Q2,GBA,kB,kB,kA,-one,
     & km,km,km,km,km,km)                      	!GBA=-INV(QBB)*QBA in GBA
c  now phib()
	call SUBMAT(Q1,CB,Q2,km,km,km,km)		!QCB IN Q2
	call MATMUL(Q2,GBA,Q3,kC,kB,kA,one,km,km,km,km,km,km)	!QCB*GBA IN Q3
	call SUBMAT(Q1,CA,Q2,km,km,km,km)		!QCA IN Q2
	do i=1,kC
	   do j=1,kA
		Q2(I,J)=Q3(I,J)+Q2(I,J)			!QCB*GBA+QCA IN Q2
	   enddo
	enddo
	sum=0.0d0
	do j=1,kA		!premult by pC(inf) to get phib
	   phib(j)=0.0d0
	   do n=1,kC
		phib(j)=phib(j)+pinf(n+kE)*Q2(n,j)
	   enddo
	   sum=sum+phib(j)
	enddo
	do j=1,kA
	   phib(j)=phib(j)/sum	!normalise phib
	enddo
	print 71,(phib(j),j=1,kA)
      if(discprt) write(8,71)(phib(j),j=1,kA)
71	format(' Initial vector for burst:',2x,8g13.6)
c
c  Do expansion of QEE
	call SUBMAT(Q1,EE,Q2,km,km,km,km)
	CALL QMAT5(Q2,Amat,kE,eigen,ibad,km,km,km)
	if(debug()) then
	   call ATYPD(Q2,'  QEE   ',kE,kE,km,km)
	   print 700
c700	   format(' Print spectral expansion matrices [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	   if(UC(ans).eq.'Y') then
		do m=1,kE
		 call ATYPD3(amat,m,'A matrix',kE,kE,km,km,km)
		enddo
	   endif
	endif
	IF(IBAD.NE.0) print 14,IBAD
	pop0=0.0
	atot=0.0
	bmax1=-1.d200
c amplitude=bm(m) (= w1(m) in other progs)
	do m=1,kE
	   bm(m)=0.0d0
	   do i=1,kA		!use AA subsection of exp(QEE.t)
		do j=1,kA
		   bm(m)=bm(m) + phib(i)*Amat(i,j,m)	!end vectur is uA
		enddo
	   enddo
	   if(dabs(bm(m)).gt.bmax1) bmax1=bm(m)
     	   tau(m)=-1.d3/eigen(m)
	   area(m)=sngl(bm(m)/dabs(eigen(m)))
	   pop0=pop0 + sngl(bm(m))	!total amp at t=0, exc asymptote
	   atot=atot + area(m)
	enddo
	do m=1,kE
	   bmrel(m)=bm(m)/bmax1		!as defined for others in relout
	enddo
c Print in same form as in RELOUT
	print 361
      if(discprt) write(8,361)
361	format(/,' Time course for P(open)',/,
     & ' Component  eigenvalue        tau (ms)      amp at t=0     ',
     & '     Area    ')
	do m=1,kE
	    print 39,m,sngl(eigen(m)),tau(m),bm(m),area(m)
          if(discprt) write(8,39) m,sngl(eigen(m)),tau(m),bm(m),
     &	area(m)
39	    format(i8,3x,g13.6,3x,g13.6,3x,g13.6,4x,g11.4)
	enddo
c Trivial to show that this result must asymptote to 0
	print 391,pop0,atot
      if(discprt) write(8,391) pop0,atot
391	format(
     & ' Total Popen at t=0 = ',g13.6,/,
     & ' Total Popen at t -> infinity = 0',/,
     & ' Total area = ',g11.4)
c
c Now display aligned bursts
	cjump=.false.
	vjump=.false.
	pulse=.false.
	align=.true.
	call SCDISP(cjump,vjump,noise,pulse,cfac,pinf,bmj,eigen,
     & pinf0,bmj0,bmrel,eigen0,dgamma,t0,fc,G0,cv0,k,iplot,
     & varconc,xcal,ycal,ntime,dt,xcalsav,ycalsav,ncsav,plotsav,
     & prepulse,bmj0z,bmj0n,bmjz,bmjn,ndc1,ndimc,
     & ncalc,drcurve,Q1,align,bm,plotocc,plotcur,plotrate,
     & varate,titlep,npv,npulse,tpgap,nlig,y00,yinfeq,nchill,
     & nchill2)
c     & varate,titlep,npv,npulse,tpgap,nlig)
	align=.false.	!reset
	DEALLOCATE(xcal,ycal)
c
c
99	call DCASK('Another run','y',ans)
	if(ans.eq.'Y') then
	   call DCASK('Modify present rate constants only','y',ans)
	   if(ans.eq.'Y') then
		iflag=3
      	if(discprt) write(8,106)
106 		format(/' New run with same mechanism',/,
     &		  ' ===========================')
		idest=105
		goto 105
	   else
		idest=0
		goto 104
	   endif
	endif
c
	print 108
108	format('/')
	call DCASK('Write defaults to disc file (*.INI)','Y',ans)
         if(ans.eq.'Y') then
c For .ini set nsc(i) temporarily negative to denote obeymr(i)=F
		call TITENT0(
     &    'Name for .INI file:',inifile,40,.false.)
	   imodold=imod0
c For .ini set nsc(i) temporarily negative to denote obeymr(i)=F
	   do i=1,ncyc
		if(.not.obeymr(i)) then
		   nsc(i)=-iabs(nsc(i))
		endif
	      if(automr(i)) then
		   isetmr(i)=-iabs(isetmr(i))
		endif
	   enddo
	   OPEN(unit=16,file=inifile,status='UNKNOWN',
     &     access='DIRECT',form='UNFORMATTED',recl=10240)
	   iver=101
	   write(16,rec=1) iver,irecq,imodold,conc,conc0,conc00,conc000,
     &     iprint,icalc,iplot,tpre,xAs,ans1,npulse,tpgap,
     &     nljump,nlvar,npr,nvdep,ncyc,vhold,vref,vkin,
     &     (nsc(i),i=1,ncyc),
     &     ((im(i,j),j=1,nsc(i)),i=1,ncyc),
     &     ((jm(i,j),j=1,nsc(i)),i=1,ncyc),
     &     qfilem,plotcur,useprim,(isetmr(i),i=1,ncyc)
	   do i=1,ncyc	     !reset nsc
		nsc(i)=iabs(nsc(i))
		isetmr(i)=iabs(isetmr(i))
	   enddo
	   CLOSE(unit=16)
	endif
c
	if(allocated(xcalsav)) then
	   DEALLOCATE(xcalsav,ycalsav)
	endif
c
	call DEVEND
	call GINEND
	call ENDPRINT
	call NUMCLR()
	end

c Functions for expmaxd:

	real*8 function EXPFUNC(t)
c Evaluate exponentials
	real*8 s,t,w,tau,yinf
	common/expd/ w(100),tau(100),yinf,k
c
	s=0.0
	do i=1,k
	   s=s + w(i)*dexp(-t/tau(i))
	enddo
	EXPFUNC=s+yinf
	RETURN
	end

	real*8 function EXPFUNC1(t)
c First derivative of exponentials
	real*8 s,t,w,tau,yinf
	common/expd/ w(100),tau(100),yinf,k
c
	s=0.0
	do i=1,k
	   s=s - (w(i)/tau(i))*dexp(-t/tau(i))
	enddo
	EXPFUNC1=s
	RETURN
	end


