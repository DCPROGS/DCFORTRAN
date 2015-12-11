	program SCALCS
c To calculate macroscopic jumps and noise for values produced by QGEN
c Lahey V5.n version 10/07/93 03:34pm
c
c MAJOR MODIF 01/05/01 09:50pm TO TAKE UP TO 100 STATES
c Note: GETQD now returns QT without conc (or diagonals), so rather than using
c QNEWC to change conc (only), use QSETD or QSETC to set whole Q matrix (does 
c not need the 'standard' conc, cA1, cB1, used in old version)
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
c variables: cur0(10),tau(10),curinf,cur0tot,cfac1.
c
c Calculate relaxations from
c p(t)= p(inf) + p(0)*sum[A(m)*exp(-t/tau(m)];  sum from m=2,..,k
c
	REAL*8 QD(100,100),PImat(100,100),Pinf(100),P0(100),dgamma(100)
	real*8 eigen0(100),p00(100),pinf0(100),bmj0(100,100)	!for ON in 'pulse' case
	real*8 Amat(100,100,100),eigen(100)
	real*8 Q1(100,100),pt0(100),pt(100)
	real*8 p00z(100),p00n(100),bmj0z(100,100),bmj0n(100,100)	!for prepulse calcs
	real*8 p0z(100),p0n(100),bmjz(100,100),bmjn(100,100)	!for prepulse calcs
	real*8 bm0(100),bmj(100,100),bm(100),bmrel(100)
	real*8 cm(100),pi,var1,s
	real*4 cv0(100),g0(100),area(100),fc(100)
c	real*4 cur(10),cur0(10),tau(10)
	real*8 cur0(100),tau(100),curinf,cur0tot,cfac1,cur(100)
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
	character*10 titlep(200),titlep1(200)
	character pre*1
	integer NSC(50),IM(50,100),JM(50,100),IX(100),JX(100)
	integer IQ(100,100)		!in common with getqd
	COMMON/KM2/AKA1,BA,PSTAR,KMCON(9),KMFAST,aka2,arat
	COMMON/VPAR/NVDEP,IV(100),JV(100),HPAR(100)
	COMMON/CPAR/NCDEP,IX,JX,X
	COMMON/QPAR/NCON,IC(2,200)
	COMMON/MPAR/NCYC,NSC,IM,JM
	COMMON/EBLK/NEQ,IE(100),JE(100),IF(100),JF(100),EFAC(100)
	COMMON/LIG/nlig,IL
c
c For new version that uses qmechs.dat
	real*4 conc(10)
	character*20 ligname(10)
	COMMON/LIG/nlig,IL(100)
c Declarations for aligned bursts (added 09/20/97 06:15pm)
	integer BA,BB,CA,CB,EE
	real*8 Q2(100,100),Q3(100,100),GBA(100,100),phib(100),one,sum
	real*8 bmax(2),bmax1
	real*4 pop0,atot
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
	LOGICAL pon,debug,deb,prt,errflag
	logical slock,caplock
c	character ndev*2,infil*14
	logical student,cluster
	logical monot
c
c for binding curves -nbound in common in getqd, getbound (getnlig now obsolete)
	real*8 bound(100,2)
	integer nbound(100,2)
	common/nbnd/nbound
c
c for eqoccd, getqd
	logical obeymr
	common/mr/obeymr		!true if microscopic reversibility to be obeyed

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
	common/user/student,cluster,iwindows
	common/qblk/IQ
	logical gaddum
	common/gad/gaddum,tauzero
	common/mod/imod
	character*2 charmod(20,30)	!to print model
	character*74 mtitle	!title for model
	common/model/imod0,charmod,jlast,ilast,mtitle,imodold
c
	pon()=slock()
	debug()=caplock()
c
c
101	format(a1)
	call SETMOUSE()		!define values in common\mousval\ (in IVLIB)
	call GINO
	call vga
	call mode(3)
c4	format(g13.6)
	filnam='SCALCS.PRT'
	cluster=.false.
	student=.false.
	call MYPATH(path)
	if(path(1:8).eq.'O:\CVFIT') cluster=.true.
	call WINPRINT	!print file control
      OPEN(unit=7,file=prtport,iostat=nerr)		!open printer
	print 1
	if(pon()) write(7,1)
	if(discprt) write(8,1)
1	FORMAT(' SCALCS: Macroscopic noise and jumps calculation',/)
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 2,cdate,ctime(1:8),mtitle1
	if(pon()) write(7,2) cdate,ctime(1:8),mtitle1
	if(discprt) write(8,2) cdate,ctime(1:8),mtitle1
2	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 3
3	format(
     & ' SCROLL LOCK on for printing'/
     & ' CAPS LOCK on for debugging')
	print 398
398	format(
     &' This program uses the model specified by the Q matrix',/,
     &' generated by QGEN to calc C-jump relaxations or noise spectra.',
     &/,' V-jumps not yet fixed in this program.')
	call BELL(1)
	print 400
400	format(/,
     & ' NB The following files are now expected to be present in',/,
     & ' the directory that contains this program: qgen.ini, ',/,
     & ' qmodel.dat, qdat.dat and (if number of models > 41) ',/,
     & ' then also qtitle.dat'/)
	pause
C
	icalc=3	!default=pulse for now
	iplot=1
	iprint=4
	plotsav=.false.
c Use F90 routine to prevent underflow crashes??
	errflag=.true.
	call UNDFL(errflag)
c
	readini=.false.
	INQUIRE(file='SCALCS.INI',exist=present,flen=nlen)
	if(present.and.nlen.gt.0) then
	   readini=.true.
	   if(nlen.eq.512) then		!old.ini
      	OPEN(unit=17,file='SCALCS.INI',status='UNKNOWN',
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
	     	OPEN(unit=16,file='SCALCS.INI',status='UNKNOWN',
     &    	access='DIRECT',form='UNFORMATTED',recl=10240)
===c 	    second two lines like old.ini (need changes?)
		read(16,rec=1)irecq,imodold,conc,conc0,conc00,
     &	iprint,icalc,iplot,tpre,
     &	xAs,ans1,npulse,tpgap,
     &	nvdep,ncyc,
     &	(nsc(i),i=1,ncyc),
     &	((im(i,j),j=1,nsc(i)),i=1,ncyc)
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
	ans='Y'
	obeymr=.true.
	call DCASK(
     & 'Constrain rates to obey microscopic reversibility',ans,ans)
	if(ans.eq.'N') then
	   print 114
	   if(discprt) write(8,114)
114	   format(/,'Rates do NOT obey microscopic reversibility',/)
	   obeymr=.false.
	   ncyc=0
	else
	   print 115
	   if(discprt) write(8,115)
115	   format(/,
     &	' Rates constrained to obey microscopic reversibility',/)
	endif
	kmax=100		!dimension of Q
	km=100				!array dimensions
========setting conc -bit from scbst
c
35	if(nlig.ge.1) then
	   do i=1,nlig
		xs=1.e6*conc(i)
		print 159,i,ligname(il(i)),xs
159		FORMAT(/,
     &     ' Ligand #',i2,1x,a20,': concentration (muM) [',g13.6,'] = ')
		call INPUTr(xs)
		conc(i)=xs*1.0e-6
	   enddo
	endif
	call QSETD(conc,IL,V1,QT,QM,k,.false.)
c=	call QSETC(QT,conc,QM,ncdep,nlig,IL,IX,JX,k,km)		!sets conc only
	call EQOCCd(QM,k,k-1,km,Peq)		!CALC EQUILIB OCCS
========end of bit from scbst

c Get the Q matrix defined at conc xA,xB
	idest=0
105	continue
c Note GETQD now returns QT without conc (or diagonals)
	call GETQD(QT,conc,nchan,dgamma,vkin,vhold,vref,
     &  npar,titlep,ligname,iflag,iprint,readini,irecq,0,idest)
===up to here
c
	k=kA+kB+kC+kD
	gaddum=imod.eq.31.and.k.eq.3.and.titlep(1)(1:6).eq.'kA(-1)'
	noise=.false.
	calinit=.false.
c=	cA1=xA	!save conc at which QD calculated in case conc altered
c=	cB1=xB
	cfac=float(nchan)*(vkin*0.001)*1.e12	!*dgamma (Siemens) gives pA
c Define also cfac2 which, when mult dgamma**2 gives pA**2
	cfac2=float(nchan)*(vkin*0.001)*(vkin*0.001)*1.e24
	do i=1,k
	   gamma(i)=1.e12*sngl(dgamma(i))		!in pS
c V is in mV
c	   cur(i)=float(nchan)*(vkin*0.001)*gamma(i)  !pA for nchan open channels
	enddo
	if(gaddum) then
	   aKA=sngl(qd(1,2)/qd(2,1))		!molar
	   aKB=sngl(qd(3,2)/qd(2,3))
	   print 77,1.e6*aKA,1.e6*aKB
	   if(discprt) write(8,77) 1.e6*aKA,1.e6*aKB
77	   format(/,' KA, KB (micromolar) = ',2g13.6)
	endif
c
281	continue
c If not cjump or vjump ask for init cond here anyway
c	if(vjump) then
291	   print 29,vkin,xa*1.e6,vkin,xa*1.e6,icalc
c     &' Data from QGEN is not for a jump;',/,
29	   format(/,
     & '  V = ',f8.1,' mV;  c=',g13.6,' microMolar',/
     &,' (1) Calculate a c-jump (assuming initial equilibrium)',/,
     &'      (or specify arbitrary initial vector)',/,
     &' (2) Calculate a c-jump (not assuming initial equilibrium)',/,
     &' (3) Calculate relaxation in response to a conc-pulse',/,
     &' (4) Calculate relaxation for conc-pulse with pre-pulse',/,
     &' (5) Calculate relaxation with time-dependent concentration',/,
     &' (6) Calculate noise spectrum (at ',f8.1,' mV; ',g13.6,' muM)',/,
     &' (7) As (1) but plot d/r curves for series of post jump conc',/,
     &' (8) As (1) but plot binding curves for series of post jump '
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
	   plotcur=.true.
	   plotocc=.false.
	   varate=.false.
	   if(iopt.ne.10) npulse=1
	   if(iopt.eq.1) then
		calinit=.false.
		pulse=.false.
	   else if(iopt.eq.2) then
		calinit=.true.
		pulse=.false.
	   else if(iopt.eq.3) then
		calinit=.true.
		pulse=.true.
	   else if(iopt.eq.4) then
		calinit=.true.
		pulse=.true.
		prepulse=.true.
	   else if(iopt.eq.5) then
		calinit=.true.
		pulse=.false.
		varconc=.true.
	   else if(iopt.eq.6) then
		cjump=.false.
		pulse=.false.
		noise=.true.
	   else if(iopt.eq.7) then
		calinit=.false.
		pulse=.false.
		drcurve=.true.
		plotocc=.false.
		ans='Y'
		call DCASK('Plot current (Y) or Popen (N)',ans,ans)
		plotcur=ans.eq.'Y'
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
=====no need for GETIJ and no need to divide by conc now
		do m=1,npar
		   pre=' '
		   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
		   r=QD(i,j)
		   if(ncdep.gt.0) then
			do n=1,ncdep
			   if(ix(n).eq.i.and.jx(n).eq.j) then
				pre='*'		!asterisk indicates rate to be mult by conc
				r=r/xA
			   endif
			enddo
		   endif
c check if either if i,j or j,i is a micro rev route
		   if(ncyc.gt.0) then
			do n=1,ncyc
			   if(im(n,1).eq.i.and.jm(n,1).eq.j) then
				pre=char(240)	!equiv sign '=q(1,2)' indicates micro rev route
			   endif
			enddo
		   endif
         	   print 122,m,pre,i,j,titlep(m),r
122	   	   format(i3,2x,a1,' q(',i2,',',i2,')=',3x,a10,2x,g13.6)
		enddo
1211		print 121,npar,npv
121		format(' Parameter number to vary (1 - ',i3,') [',i3,'] = ')
		call INPUTi(npv)
		if(npv.lt.1.or.npv.gt.npar) goto 1211
		ans='Y'
		call DCASK('Plot current (Y) or Popen (N)',ans,ans)
		plotcur=ans.eq.'Y'
	   else if(iopt.eq.10) then
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
====alter this for 10 ligands!
c For dr curves check conc of second ligand
	if(iopt.eq.7.or.iopt.eq.8.or.iopt.eq.9) then
	   if(nlig.eq.2) then
		if(iopt.eq.8) then	!binding curves
		   print 65
65		   format(
     &		' (1) Plot binding of ligand 1',/,
     &		' (2) Plot binding of ligand 2',/,
     &		' Option [1] = ')
		   nlplot=1
		   call INPUTi(nlplot)
		endif
	      print 66
66		format(
     &		' (1) Vary post-jump concentration of ligand 1',/,
     &		' (2) Vary post-jump concentration of ligand 2',/,
     &		' Option [1] = ')
		nlvar=1
		call INPUTi(nlvar)
		print 661,nlvar,nlvar
661		format(
     &    ' (1) Pre-jump concentration of ligand #',i2,' constant',/,
     &    ' (2) Pre-jump concentration of ligand #',i2,' = post-jump',/,
     &    ' Option [1] = ')
		iopt1=1
		call INPUTi(iopt1)
		prevar=iopt1.eq.2
	   endif
	endif		!end of nlig=2 for d/r curves
c
	jump=vjump.or.cjump
c
c Calcs for time dep conc in sep subroutine.  Need first to calc p(0) as below
	if(varconc) then
	   ndc1=2048
	   ndimc=15
	   if(.not.allocated(xcal)) then
	      ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
	   endif
	   call VCONC1(QD,cA1,cB1,ncdep,nlig,IL,IX,JX,
     &  	k,km,qprt,xcal,ycal,ntime,dt,ndc1,ndimc)
	   goto 45	!jump to SCDISP with Xval,Yval=p(t) already calculated!
	endif
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
	   ndimc=15
	   ncal1=401
	   ndc1=2*ncal1*npulse
	   if(.not.allocated(xcal)) then
	      ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
	   endif
	   tpulse=t0	!default from scalcs.ini
	   call DPULSE(npulse,tpgap,QD,cA1,cB1,ncdep,nlig,IL,IX,JX,
     &	dgamma,cfac,bound,nbound,tpulse,xA00,xA0,xA1,
     &  	ncal1,k,km,qprt,xcal,ycal,ntime,dt,ndc1,ndimc,iprint)
	   t0=tpulse	!notation for pulse length in rest of prog
	   goto 451	!jump to SCDISP with Xval,Yval=p(t) already calculated!
	endif
c
c For c-jump, check the concentrations
102	if(cjump.and.drcurve) then
	   if(varate) then
c		x0=0.0
c		x1=xa*1.e6
		if(nlig.eq.2) then
	         print 321
c321		   format(' For ligand #1:')
		endif
		x0=xA0*1.e6
		x1=xA1*1.e6
		print 20,x0,x1
c20		format(
c     & ' Initial and final conc. (micromolar) [',2g11.4,'] = ')
		call INPUT2r(x0,x1)
		xa0=x0*1.e-6
		xa1=x1*1.e-6
		if(nlig.eq.2) then
		   x0=xb0*1.e6
		   x1=xb1*1.e6
		   print 32
c32		   format(' For ligand #2:')
	         print 20,x0,x1
		   call INPUT2r(x0,x1)
	         xb0=x0*1.e-6
	         xb1=x1*1.e-6
		endif
		print 111,npv,titlep(npv),xlo,xhi
111		format(
     &	 ' Lower, upper values for range of rate constant: ',/,
     &	 '   for rate constant ',i3,': ',a10,' [',2g12.4,'] = ')
	      call INPUT2r(xlo,xhi)
            if(pon()) write(7,112) xa0*1.e6,xa1*1.e6,vkin,titlep(npv)
            if(discprt) write(8,112) xa0*1.e6,xa1*1.e6,vkin,titlep(npv)
112	      format(
     &   ' Concentration jump from ',g13.6,' to ',g13.6,' micromolar',/,
     &   ' at V = ',f8.1,' mV.',/,
     &   ' Plot peak and equilibrium response against ',a10)
	   else
		if(nlig.eq.1) then
	         x0=0.0
	         print 10,x0
10	         format(
     & ' Initial concentration (micromolar) [',g13.6,'] = ')
	         call INPUTr(x0)
		   xlo=0.1
		   xhi=100.
		   print 11,xlo,xhi
11		   format(
     & ' Low, high final conc range (micromolar) [',2g12.4,'] = ')
		   call INPUT2r(xlo,xhi)
		   print 69,x0,xlo,xhi
		   if(discprt) write(8,69) x0,xlo,xhi
69		   format(' Initial concentration (micromolar) = ',g11.4,/,
     &		' Range for post-jump concentration from ',
     &		g11.4,' to ',g11.4)
		   xa0=x0*1.e-6
		   xlo=xlo*1.e-6
		   xhi=xhi*1.e-6
		else if(nlig.eq.2) then
		   n=nlvar		!ligand conc that is to be changed
		   if(n.eq.1) then
			n0=2		!ligand that is same for all jumps
			x0=xB0*1.e6
			x1=xB1*1.e6
		   else if(n.eq.2) then
			n0=1		!ligand that is same for all jumps
			x0=xA0*1.e6
			x1=xA1*1.e6
		   endif
		   print 67,n0
		   if(discprt) write(8,67) n0
67		   format(/,' Ligand #',i3,' (same conc for all jumps)')
		   print 20,x0,x1
c20		   format(
c     &	 '   Initial and final conc. (micromolar) [',2g11.4,'] = ')
		   call INPUT2r(x0,x1)
		   if(discprt) write(8,20) x0,x1
		   if(n0.eq.1) then
			xa0=x0*1.e-6
			xa1=x1*1.e-6
		   else
			xB0=x0*1.e-6
			xB1=x1*1.e-6
		   endif
c           Now the conc to be varied
		   if(n.eq.1) then
			x0=xA0*1.e6
		   else if(n.eq.2) then
			x0=xB0*1.e6
		   endif
c	         x0=0.0
	         print 68,n
		   if(discprt) write(8,68) n
68	         format(/,' Ligand #',i3,' (post-jump conc varies)')
		   if(prevar) then
			print 682
			if(discprt) write(8,682)
682			format('  Pre-jump concentration same as post-jump')
		   else
	            print 681,x0
681		      format(
     &           '  Initial concentration (micromolar) [',g13.6,'] = ')
	            call INPUTr(x0)
			if(discprt) write(8,683) x0
683		      format(
     &           '  Initial concentration (micromolar) = ',g13.6)
		   endif
		   xlo=0.1
		   xhi=100.
		   print 11,xlo,xhi
c11		   format(
c     & ' Low, high final conc range (micromolar) [',2g12.4,'] = ')
		   call INPUT2r(xlo,xhi)
		   if(discprt) write(8,73) xlo,xhi
73		   format(
     &	  '  Low, high post-jump conc range (micromolar) = ',2g12.4)
		   if(n.eq.1) then
			xa0=x0*1.e-6
		   else if(n.eq.2) then
			xB0=x0*1.e-6
		   endif
		   xlo=xlo*1.e-6
		   xhi=xhi*1.e-6
		endif		!end of nlig=2
	   endif
	else if(cjump.and.(.not.pulse)) then
	   x0=xA0*1.e6
	   x1=xA1*1.e6
	   if(nlig.eq.2) then
	      print 321
321		format(' For ligand #1:')
	   endif
	   print 20,x0,x1
20	   format(
     & '   Initial and final conc. (micromolar) [',2g11.4,'] = ')
	   call INPUT2r(x0,x1)
	   xa0=x0*1.e-6
	   xa1=x1*1.e-6
	   if(nlig.eq.2) then
		x0=xb0*1.e6
		x1=xb1*1.e6
		print 32
32		format('  and for ligand #2:')
	      print 20,x0,x1
		call INPUT2r(x0,x1)
	      xb0=x0*1.e-6
	      xb1=x1*1.e-6
	   endif
	   if(nlig.eq.1) then
            if(discprt) write(8,311) xa0*1.e6,xa1*1.e6,vkin
311	      format(
     &   ' Concentration jump from ',g13.6,' to ',g13.6,' micromolar',/,
     &      ' at V = ',f8.1,' mV')
	   else if(nlig.eq.2) then
            if(discprt) write(8,3111) xa0*1.e6,xa1*1.e6,
     &		xb0*1.e6,xb1*1.e6,vkin
3111	      format(
     &   ' Ligand 1: jump from ',g13.6,' to ',g13.6,' micromolar',/,
     &   ' Ligand 2: jump from ',g13.6,' to ',g13.6,' micromolar',/,
     &      ' at V = ',f8.1,' mV')
	   endif
	endif
c
	if(cjump.and.(.not.calinit)) then
	   call QNEWC(QD,cA1,cB1,Q1,xA0,xB0,ncdep,nlig,IL,IX,JX,k,km)
	   call EQOCCd(Q1,k,k-1,km,p0)	!calc new init occs at xA0
	else if(cjump.and.calinit) then
	   if(pulse) then
		if(prepulse) then
		   xa00=xa00*1.e6
	         print 36,xA00
36		   format(
     &	' Concentration before prepulse assumed to be zero.',/,
     &	' (1) Concentration during prepulse (micromolar) [',
     &	   f8.3,'] = ')
		   call INPUTr(xA00)
		   if(nlig.eq.2) then
			xB00=xB00*1.e6
			print 33,xB00
33			format(
     &		'&   and for ligand #2, conc (muM) [',f8.3,'] = ')
			call INPUTr(xB00)
			xb00=xB00*1.e-6
			print 108
108			format(/)
		   endif
		   tpre=tpre*1000.		!in ms
	         print 37,tpre
37		   format(
     &	   '& Duration of the prepulse (ms) [',f7.2,'] = ')
		   call INPUTr(tpre)
		else
	         print 312
312		   format(
     &	   ' (1) Concentration (at equilib) before pulse',
     &	   ' (micromolar) [0.0] = ')
		   xA00=0.0
		   call INPUTr(xA00)
		endif
		if(nlig.eq.2) then
		   xB00=xB00*1.e6
		   print 33,xB00
c33		   format(
c     &	   '&   and for ligand #2, conc (muM) [',f8.3,'] = ')
		   call INPUTr(xB00)
		   xb00=xB00*1.e-6
		   print 108
c108		   format(/)
		endif
c
		xa0=xa0*1.e6
	      print 313,xa0
313		format(
     &     '&(2) Concentration during pulse (micromolar) [',f8.3,'] = ')
		call INPUTr(xA0)
		if(nlig.eq.2) then
		   xB0=xB0*1.e6
		   print 33,xB0
c33		   format(
c     &	   '&   and for ligand #2, conc (muM) [',f8.3,'] = ')
		   call INPUTr(xB0)
		   xb0=xB0*1.e-6
		   print 108
c108		   format(/)
		endif
	      print 314
314		format(
     &	'&(3) Concentration after pulse (micromolar) [0.0] = ')
		xA1=0.0
		call INPUTr(xA1)
		if(nlig.eq.2) then
		   xB1=xB1*1.e6
		   print 33,xB1
c33		   format(
c     &	   '&   and for ligand #2, conc (muM) [',f8.3,'] = ')
		   call INPUTr(xB1)
		   xb1=xB1*1.e-6
		   print 108
c108		   format(/)
		endif
		t0=t0*1000.		!in ms
	      print 315,t0
315		format(
     &	'&(4) Duration of the pulse (ms) [',f7.2,'] = ')
		call INPUTr(t0)
		if(prepulse) then
	         print 38,tpre,xa00,t0,xa0,xa1
	         if(pon()) write(7,38) tpre,xa00,t0,xa0,xa1
	         if(discprt) write(8,38) tpre,xa00,t0,xa0,xa1
38		   format(/,
     &     ' Concentration pulse:',/,
     &     '   Prepulse of ',g13.6,' ms at ',g13.6,' micromolar ',/,
     &     '   then pulse of ',g13.6,' ms at ',g13.6,' micromolar ',/,
     &     '   before jumping to ',g13.6,' micromolar ')
		   if(nlig.eq.2) then
			if(discprt) write(8,232) xB00*1.e6,xB0*1.e6,xB1*1.e6
232			format(
     &	   '   for ligand #2, conc = ',g13.6,',',g13.6,',',g13.6)
		   endif
		else
	         print 316,xa00,t0,xa0,xa1
	         if(pon()) write(7,316) xa00,t0,xa0,xa1
	         if(discprt) write(8,316) xa00,t0,xa0,xa1
316		   format(/,
     &	' Concentration pulse:',/,
     &	'   start at equilibrium with ',g13.6,' micromolar ',/,
     &	'   then ',g13.6,' ms at ',g13.6,' micromolar ',/,
     &	'   before jumping to ',g13.6,' micromolar ')
		   if(nlig.eq.2) then
			if(discprt) write(8,232) xB00*1.e6,xB0*1.e6,xB1*1.e6
c232			format(
c     &	   '   for ligand #2, conc = ',g13.6,',',g13.6,',',g13.6)
		   endif
		endif
		xA00=xA00*1.e-6	!molar
		xA0=xA0*1.e-6
		xA1=xA1*1.e-6
		t0=t0*0.001		!in seconds
		tpre=tpre*0.001		!in seconds
	   else	!cjump.and.calinit but not pulse
	      print 21,xa0*1.e6
21	      format(' Length of exposure to ',g13.6,' muM (ms) = ')
	      call INPUTr(t0)
	      t0=t0*0.001		!in seconds
	      print 22
22	      format('&Concentration before this (muM) [0.0] = ')
	      call INPUTr(xA00)
		if(nlig.eq.2) then
		   xB00=xB00*1.e6
		   print 33,xB00
c33		   format(
c     &	   '&   and for ligand #2, conc (muM) [',f8.3,'] = ')
		   call INPUTr(xB00)
		   xb00=xB00*1.e-6
		   print 108
c108		   format(/)
		endif
	      xA00=1.e-6*xA00	!molar
            if(discprt) write(8,23) xa0*1.e6,t0*1000.,xA00*1.e6
23	      format(
     &    ' Initial condition for conc jump not at equilibrium:',/,
     &    ' at ',g13.6,' muM for ',g13.6,' ms, starting from ',g13.6,
     &    ' muM')
		if(nlig.eq.2) then
		   if(discprt) write(8,231) xB0*1.e6,xB00*1.e6
231		   format('   for ligand #2, conc = ',g13.6,', ',g13.6)
		endif
	   endif
c
c Now recalculate the initial condition (before pulse starts)
c First get Q at conc=xA00. The QD from disc is for concentrations xA1,xB1
c so alter the conc dep rates proportionally- use subroutine QNEWC()
c NB Q at xA00 needed only as input for EQOCCD to get initial occupancies
	   if(debug()) call DATYP(QD,'  QD    ' ,pon(),k,k,km,km)
	   call QNEWC(QD,cA1,cB1,Q1,xA00,xB00,ncdep,nlig,IL,IX,JX,k,km)
	   call EQOCCd(Q1,k,k-1,km,P00)		!calc equilib occs at xA00
	   if(qprt) then
      	print 50,xa00*1.e6
      	if(pon()) write(7,50) xa00*1.e6
      	if(discprt) write(8,50) xa00*1.e6
50		format(/,' Q matrix before t=0, at concentration = ',g13.6)
	      call DATYP(Q1,' Q(00)  ' ,pon(),k,k,km,km)
		call CALCPI(Q1,PImat,k,km,km)
	      call DATYP(pimat,' PI(00) ' ,pon(),k,k,km,km)
	   endif
	   if(iprint.ge.2) then
	      print 51,xa00*1.e6
	      if(pon()) write(7,51) xa00*1.e6
	      if(discprt) write(8,51) xa00*1.e6
51		format(/,
     & ' Equilib occupancies before t=0, at concentration = ',g13.6)
		do 52 j=1,k
		print 53,j,p00(j)
	      if(pon()) write(7,53) j,p00(j)
	      if(discprt) write(8,53) j,p00(j)
53		format(' p00(',i2,') = ',g13.6)
52		continue
		if(iprint.ge.3) then
c		   call SLIFEd1(Q1,p00,km,km)
		   call SLIFED2(Q1,P00,pon(),discprt,km,km)
		endif
	   endif
c
c In prepulse case the bit above gives equilibrium conditions (tpre -> infinity)
c for the prepulse; now do all calculations for prepulse in CALPREP. Q1 is
c Q matrix at prepulse conc -must get occs before prepulse (conc=0)
c in CALPREP.  Also get new P00() = occupancies at time=tpre, i.e. the
c initial condition for the pulse (need option to calc overall P00, or P00 for
c cases where (a) there are no openings during prepulse or (b) there is at
c least one opening during prepulse). Before call p00 is equilib occ at
c the prepulse conc.  Returns occupancies at end of prepulse (=init
c condition for pulse) (a) overall in pt0(), copied to p00();
c (b) given no opening, in p00z() and (c) given at least one opening, in p00n()
	   if(prepulse) then
		kF=k-kA
		call CALPREP(tpre,Q1,p00,pt0,p00z,p00n,cA1,cB1,nlig,IL,
     &	 xA00,km,k,kA,kF,iprint,pon())
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
	   call QNEWC(QD,cA1,cB1,Q1,xA0,xB0,ncdep,nlig,IL,IX,JX,k,km)
	   if(debug()) call DATYP(Q1,' Q1     ' ,pon(),k,k,km,km)
	   call EQOCCd(Q1,k,k-1,km,Pinf0)	!calc equilib occs at xA0
	   if(qprt) then
      	print 54,xa0*1.e6
      	if(pon()) write(7,54) xa0*1.e6
      	if(discprt) write(8,54) xa0*1.e6
54		format(/,' Q matrix at concentration = ',g13.6)
	      call DATYP(Q1,'  Q(0)  ' ,pon(),k,k,km,km)
		call CALCPI(Q1,PImat,k,km,km)
	      call DATYP(pimat,' PI(0)  ' ,pon(),k,k,km,km)
	   endif
	   if(iprint.ge.2) then
      	print 55,xa0*1.e6
	      if(pon()) write(7,55) xa0*1.e6
      	if(discprt) write(8,55) xa0*1.e6
55  		format(/,' Equilibrium occupancies at concentration = ',g13.6)
		do j=1,k
		   print 57,j,pinf0(j)
      	   if(pon()) write(7,57) j,pinf0(j)
	         if(discprt) write(8,57) j,pinf0(j)
57		   format(' pinf0(',i2,') = ',g13.6)
		enddo
		if(iprint.ge.3) then
c		   call SLIFEd1(Q1,pinf0,km,km)
		   call SLIFED2(Q1,Pinf0,pon(),discprt,km,km)
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
			call DATYP3(amat,m,'A matrix',pon(),k,k,km,km)
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
      	   if(pon()) write(7,581) t0*1000.,xa0*1.e6
	         if(discprt) write(8,581) t0*1000.,xa0*1.e6
581		   format(/,
     &' Occupancies at t0 = ',g13.6,' ms at concentration = ',g13.6)
		   do j=1,k
			print 60,j,p0(j)
      		if(pon()) write(7,60) j,p0(j)
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
      	   if(pon()) write(7,331)
	         if(discprt) write(8,331)
331		   format(/,' ON-relaxation')
		   call RELOUT(cfac,dgamma,pinf0,bmj0,eigen0,t0,bm0,tau0,
     &		cur00,curinf0,cur0tot,bmrel,atotd,nlig,pon())
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
      		if(pon()) write(7,333)
	      	if(discprt) write(8,333)
333			format(/,
     &		' ON-relaxation given NO openings during prepulse')
		      call RELOUT(cfac,dgamma,pinf0,bmj0z,eigen0,t0,bm0,
     &		 tau0,cur00,curinf0,cur0tot,bmrel,atotd,nlig,pon())
		   else if(iloop.eq.2) then
  			call PTCALC(pt0,t0,bmj0n,p00n,pinf0,eigen0,amat,k,km)
			do j=1,k
	      	   p0n(j)=pt0(j)	!new initial occs (for OFF jump of pulse)
			enddo
			print 334
      		if(pon()) write(7,334)
	      	if(discprt) write(8,334)
334			format(/,
     &		' ON-relaxation given SOME openings during prepulse')
		      call RELOUT(cfac,dgamma,pinf0,bmj0n,eigen0,t0,bm0,
     &		 tau0,cur00,curinf0,cur0tot,bmrel,atotd,nlig,pon())
		   else if(iloop.eq.3) then
  			call PTCALC(pt0,t0,bmj0,p00,pinf0,eigen0,amat,k,km)
			do j=1,k
	      	   p0(j)=pt0(j)	!new initial occs (for OFF jump of pulse)
			enddo
			print 331
      		if(pon()) write(7,331)
	      	if(discprt) write(8,331)
		      call RELOUT(cfac,dgamma,pinf0,bmj0,eigen0,t0,bm0,
     &		 tau0,cur00,curinf0,cur0tot,bmrel,atotd,nlig,pon())
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
	if(drcurve) then
	   if(.not.prevar) then	!must re-calc initial condition for each conc
c        First print the initial condition (same for all conc) just calc
		print 76,(p0(j),j=1,k)
      	if(discprt) write(8,76)(p0(j),j=1,k)
76		format(' p(0) = ',10f8.5)
c	      print 216
c	      if(discprt) write(8,216)
cc216	      format(' Equilibrium initial occupancies:')
c	      do j=1,k
c		   print 217,j,p0(j)
c	         if(discprt) write(8,217) j,p0(j)
cc217	         format(' p0(',i2,') = ',g13.6)
c		enddo
	   endif
	   ncalc=256
	   ndc1=ncalc
	   ndimc=15
	   if(.not.allocated(xcal)) then
		ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
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
	   axlo=alog10(xlo)
	   axhi=alog10(xhi)
	   adx=(axhi-axlo)/float(ncalc-1)
c	   radx=10.d0**adx
c	   radx=radx-1.d0
	   if(varate) then
		call GETIJ(IQ,k,ipv,jpv,npv)		!get i,j for rate constant #npv
	   endif
	   do i=1,ncalc
		x=axlo + float(i-1)*adx
		if(varate) then	!new value for rate constant
		   qd(ipv,jpv)=10.**x
		   call QNEWC(QD,cA1,cB1,Q1,xA0,xB0,ncdep,nlig,IL,IX,JX,k,km)
		   call EQOCCd(Q1,k,k-1,km,p0)	!calc new init occs at xA0
		else
		   if(nlig.eq.1) then
			xA1=10.**x
			xv1=xA1		!define as the variable conc
		   else if(nlig.eq.2) then
			if(nlvar.eq.1) then
			   xA1=10.**x
			   xv1=xA1		!define as the variable conc
			   if(prevar) xA0=xA1
			   if(gaddum) then
				tauzero=1000./(sngl(q1(2,3)) + sngl(q1(3,2)))
			   endif
			else if(nlvar.eq.2) then
			   xB1=10.**x
			   xv1=xB1		!define as the variable conc
			   if(prevar) xB0=xB1
			endif
		   endif
		endif
		if(prevar) then	!must re-calc initial condition for each conc
		  call QNEWC(QD,cA1,cB1,Q1,xA0,xB0,ncdep,nlig,IL,IX,JX,k,km)
		  call EQOCCd(Q1,k,k-1,km,p0)		!calc new init occs at xA0
		  print 76,(p0(j),j=1,k)
      	  if(discprt) write(8,76)(p0(j),j=1,k)
c76		  format(' p(0) = ',10f8.5)
		endif
c          Now post-jump
		call QNEWC(QD,cA1,cB1,Q1,xA1,xB1,ncdep,nlig,IL,IX,JX,k,km)
		call EQOCCd(Q1,k,k-1,km,Pinf)	!calc equilib occs at xA1
		call QMAT(Q1,Amat,k,eigen,ibad)
	      call PTCALC(pt,0.0,bmj,p0,pinf,eigen,amat,k,km)
		cfac1=dble(cfac)
		curinf=0.0d0	!actually total Popen here
		if(.not.plotocc) then	!calculate Popen
		   do j=1,kA
			if(plotcur) then
			   curinf=curinf+cfac1*dgamma(j)*pinf(j)	!current
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
			cur(m)=cfac1*bm(m)			!for current
		   else
			cur(m)=bm(m)				!for Popen or binding
		   endif
c At present this calculates the most negaitive amplitude -OK only for
c currents (at negative pots) and even then arguably abs amplitude matters more?
c=		   if(cur(m).lt.wmin) then
		   if(dabs(cur(m)).gt.wmax) then
			tbig=tau(m)
			wmax=cur(m)
		   endif
		enddo		!end of m=1,k-1
		deb=debug()
		prt=pon()
		thigh=-1.d0	!so set internally
		call EXPMAXd(tpeak,ypeak,curinf,thigh,texp,
     &	   prt,discprt,deb,km,nerr,nerr1,nerr2,0)
		if(nerr.eq.-5) then
	         print 12,xv1*1.e6,ypeak
	         if(discprt) write(8,12) xv1*1.e6,ypeak
12		   format(' At conc = ',g13.6,
     &	' micromolar: monotonic, asymptote = ',g13.6)
c=		   pause
		else if(nerr.ne.0.or.nerr1.ne.0.or.nerr2.ne.0) then
	         print 13,nerr,nerr1,nerr2,xv1*1.e6
	         if(discprt) write(8,13) nerr,nerr1,nerr2,xv1*1.e6
13		   format(' Error: nerr,nerr1,nerr2 = ',3i3,
     &	    ' at conc = ',g13.6,' micromolar')
c=		   pause
		else if(nerr.eq.0) then
	         print 131,xv1*1.e6,ypeak,tpeak
	         if(discprt) write(8,131) xv1*1.e6,ypeak,tpeak
131		   format(
     &	  ' At c = ',g13.6,' muM, peak = ',g13.6,' at ',g13.6,' ms')
		endif
	      print 132,tbig,texp
      	if(pon()) write(7,132) tbig,texp
	      if(discprt) write(8,132) tbig,texp
132		format(
     &	  ' tau(amax) = ',g13.6,' texp = ',g13.6)
		if(varate) then
		   do j=1,4
			Xcal(i,j)=sngl(qd(ipv,jpv))
		   enddo
		else
		   do j=1,4
			Xcal(i,j)=xv1*1.e6            !conc in micromolar
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
c Try new EC50 subroutines
c First remove conc from QD and keep in Q1
	   if(debug()) call DATYP(QD,'  QD    ' ,pon(),k,k,km,km)
	   call QNEWC(QD,cA1,cB1,Q1,1.0,1.0,ncdep,nlig,IL,IX,JX,k,km)
	   if(debug()) call DATYP(Q1,'  Q1    ' ,pon(),k,k,km,km)
c	  Here gamma() is in pS, so env should have 1.e12 removed
	   env=cfac*1.e-12		!for current in pA when gamma() in pS
	   kdim=100
	   call EQEC50(EC50,curinf,monot,curmax,concmax,Q1,
     & 	env,gamma,k,kdim)
	   if(monot) then
      	print 591,curinf,ec50*1.e6
      	if(discprt) write(8,591) curinf,ec50*1.e6
591	   format(/,
     & ' Equilibrium response-concentration curve is monotonic',/,
     & ' Maximum response (pA) = ',g11.4,/,
     & '    Conc for 50% of this equilib. current (muM) = ',g11.4,/)
	   else
      	print 592,curmax,concmax*1.e6,ec50*1.e6,curinf
      	if(discprt) write(8,592)curmax,concmax*1.e6,ec50*1.e6,curinf
592		format(/,
     &    ' Equilibrium response-concentration curve has maximum.',/,
     &    '   Max equilib response = ',g12.5,' pA at ',g12.5,' muM',/,
     &    '   Conc for 50% of this max. current (muM) (left of max) = ',
     &     g12.5,/,
     &    '   Response at conc -> infinity = ',g12.5,' pA',/)
	   endif
c and same for peak d/r curve
	   call PEAKEC50(EC50,curinf,monot,curmax,concmax,Q1,
     & 	p0,env,gamma,plotcur,debug(),k,kdim)
	   if(monot) then
      	print 593,curinf,ec50*1.e6
      	if(discprt) write(8,593) curinf,ec50*1.e6
593	   format(/,
     & ' Peak response-concentration curve is monotonic',/,
     & ' Maximum response (pA) = ',g11.4,/,
     & '    Conc for 50% of this maximum current = ',g11.4,' (muM)',/)
	   else
      	print 594,curmax,concmax*1.e6,ec50*1.e6,curinf
      	if(discprt) write(8,594)curmax,concmax*1.e6,ec50*1.e6,curinf
594		format(/,
     &    ' Peak response-concentration curve has maximum.',/,
     &    '   Max peak response = ',g12.5,' pA at ',g12.5,' muM',/,
     &    '   Conc for 50% of this max. current (left of max) = ',
     &     g12.5,' muM',/,
     &    '   Response at conc -> infinity = ',g12.5,' pA',/)
	   endif
	   goto 451		!straight to SCDISP
	endif		!end of drcurve section
c
c Recalc final occs at xA1,xB1 (in case conc changed), and expand Q (for
c a pulse these are for the final OFF-jump)
	call QNEWC(QD,cA1,cB1,Q1,xA1,xB1,ncdep,nlig,IL,IX,JX,k,km)
	call EQOCCd(Q1,k,k-1,km,Pinf)	!calc equilib occs at xA1
	if(qprt) then
61	   format(/,' Q matrix at concentration = ',g13.6)
	   call DATYP(Q1,' Q(inf) ' ,pon(),k,k,km,km)
	   call CALCPI(Q1,PImat,k,km,km)
	   call DATYP(pimat,' PI(inf)' ,pon(),k,k,km,km)
	endif
	if(iprint.ge.2) then
         print 62,xa1*1.e6
         if(pon()) write(7,62) xa1*1.e6
         if(discprt) write(8,62) xa1*1.e6
62	   format(/,
     & ' Final equilibrium occupancies at concentration = ',g13.6)
	   do j=1,k
		print 64,j,pinf(j)
	      if(pon()) write(7,64) j,pinf(j)
      	if(discprt) write(8,64) j,pinf(j)
64		format(' pinf(',i2,') = ',g13.6)
	   enddo
	   if(iprint.ge.3) then
c	      call SLIFEd1(Q1,pinf,km,km)
		call SLIFED2(Q1,Pinf,pon(),discprt,km,km)
	   endif
	endif
c	call QMAT5(Q1,Amat,k,eigen,ibad,km,km,km)
c Test streamlined QMAT
	call QMAT(Q1,Amat,k,eigen,ibad)
c NB Pinf should be same as Pinf from disc, if xA1 not changed
	if(ibad.ne.0) print 14,ibad
	   if(debug()) then
		print 700
c700		format(' Print spectral expansion matrices [Y] ? ')
	      ans='Y'
	      call INPUTa(ans)
		if(UC(ans).ne.'N') then
		   do 702 m=1,k
702		   call DATYP3(amat,m,'A matrix',pon(),k,k,km,km)
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
         if(pon()) write(7,26)
         if(discprt) write(8,26)
26	   format(/,' Initial and final occupancies:')
	   do 24 j=1,k
	   print 25,j,p0(j),j,pinf(j)
         if(pon()) write(7,25) j,p0(j),j,pinf(j)
         if(discprt) write(8,25) j,p0(j),j,pinf(j)
25	   format(' p0(',i2,') = ',g13.6,3x,' pinf(',i2,') = ',g13.6)
24	   continue
	   if(ncdep.gt.0) then
		call DCASK('Change the concentration',ans,ans)
		if(ans.eq.'Y') goto 102
	   endif
	else			!if not jump do QMAT here
	   call QMAT5(QD,Amat,k,eigen,ibad,km,km,km)
	   print 261
         if(pon()) write(7,261)
         if(discprt) write(8,261)
261	   format(' Equilibrium occupancies:')
	   do 241 j=1,k
	   print 251,j,pinf(j)
         if(pon()) write(7,251) j,pinf(j)
         if(discprt) write(8,251) j,pinf(j)
251	   format(' peq(',i2,') = ',g13.6)
241	   continue
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
      	   if(pon()) write(7,332)
	         if(discprt) write(8,332)
332		   format(/,' OFF-relaxation')
	         call RELOUT(cfac,dgamma,pinf,bmj,eigen,0.0,bm,tau,
     &	    cur0,curinf,cur0tot,bmrel,atotd,nlig,pon())
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
      		if(pon()) write(7,337)
	      	if(discprt) write(8,337)
337			format(/,
     &		' OFF-relaxation given NO openings during prepulse')
	      	call RELOUT(cfac,dgamma,pinf,bmjz,eigen,0.0,bm,tau,
     &	 	 cur0,curinf,cur0tot,bmrel,atotd,nlig,pon())
		   else if(iloop.eq.2) then
			call PTCALC(pt,0.0,bmjn,p0n,pinf,eigen,amat,k,km)
			print 338
      		if(pon()) write(7,338)
	      	if(discprt) write(8,338)
338			format(/,
     &		' OFF-relaxation given SOME openings during prepulse')
	      	call RELOUT(cfac,dgamma,pinf,bmjn,eigen,0.0,bm,tau,
     &	 	 cur0,curinf,cur0tot,bmrel,atotd,nlig,pon())
		   else if(iloop.eq.3) then
			print 332
      		if(pon()) write(7,332)
	      	if(discprt) write(8,332)
	      	call RELOUT(cfac,dgamma,pinf,bmj,eigen,0.0,bm,tau,
     &	 	 cur0,curinf,cur0tot,bmrel,atotd,nlig,pon())
		   endif
c
		   iloop=iloop+1
		   if(iloop.le.3) goto 336
		endif
	   else
	      call RELOUT(cfac,dgamma,pinf,bmj,eigen,0.0,bm,tau,
     &	cur0,curinf,cur0tot,bmrel,atotd,nlig,pon())
	   endif
	endif
c
c Might as well calc noise anyway, as long as final conc not zero
c Call the coefficients for the mth component (of auocovariance) cm(m)
c to distinguish them from jump coeffs in bm(m) (CH77,eq 41)
	if(xA1.lt.1.e-10) goto 45
	cfac1=dble(cfac)
	curinf=0.0d0
	do 372 j=1,kA
372	curinf=curinf+cfac1*dgamma(j)*pinf(j)
c Calc total variance directly
	var1=0.0d0
	s=0.0d0
	do 46 i=1,kA
	var1=var1 + pinf(i)*dgamma(i)*dgamma(i)
	s=s + pinf(i)*dgamma(i)
46	continue
	var1=cfac2*sngl(var1 - s*s)
c CV(0) is same as AREA, as expected, so do not print it too
c Print time constants etc, then do displays
	pi=3.14159265359d0
	do 35 m=1,k-1
	  fc(m)=-sngl(eigen(m)/(2.d0*pi))
	  tau(m)=-1.d3/eigen(m)
35	continue
	print 42,xa1*1.e6
      if(pon()) write(7,42) xa1*1.e6
      if(discprt) write(8,42) xa1*1.e6
42	format(/,' For noise spectrum at ',g13.6,'muM:',/,
     &' Comp    fc(Hz)     tau (ms)   G(0)(pA*pA/Hz)  Area (pA*pA)   ',
     & 'Area (% total)')
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
       if(pon()) write(7,43) m,fc(m),tau(m),G0(m),area(m),p
       if(discprt) write(8,43) m,fc(m),tau(m),G0(m),area(m),p
43	format(i5,3g13.6,2x,g13.6,2x,g13.6)
40	continue
c	print 44,g0tot,var,cv0tot
	print 44,g0tot,var,sp
      if(pon()) write(7,44) g0tot,var,sp
      if(discprt) write(8,44) g0tot,var,sp
44	format(' Totals:',23x,g13.6,2x,g13.6,2x,g13.6)
c Print separately calc total variance
	print 47,var1
      if(pon()) write(7,47) var1
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
c	if(pon()) write(7,742) fmin1,fmax1
c      if(discprt) write(8,742) fmin1,fmax1
c742	format(/,' f1,f2 (Hz)= ',2g13.6)
75	format(2G13.6)
	print 732,fmin1,fmax1
	if(pon()) write(7,732)fmin1,fmax1
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
	if(pon()) write(7,733) m,v1,100.*v1/v,v2,100.*v2/v
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
	if(pon()) write(7,473)fmin1,fmax1,
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
         if(pon()) write(7,81)
         if(discprt) write(8,81)
81	   format(/,
     & ' Cannot correct for P(open) when channel conductances differ')
	   goto 743
	endif
	pshut=1.0-popen
	g1c=g1/pshut
	g2c=g2/pshut
      print 82,g1c,100.*g1c/gamma(1),g2c,100.*g2c/gamma(1)
      if(pon()) write(7,82) g1c,100.*g1c/gamma(1),g2c,100.*g2c/gamma(1)
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
	ndimc=15
	if(.not.allocated(xcal)) then
	   ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
	endif
	if(.not.allocated(xcalsav)) then
	   ALLOCATE(Xcalsav(ndc1),Ycalsav(ndc1))
	endif
451	continue	!jump here for drcurve
	align=.false.	!reset
	call SCDISP(cjump,vjump,noise,pulse,cfac,pinf,bmj,eigen,
     & pinf0,bmj0,bmrel,eigen0,dgamma,t0,fc,G0,cv0,k,iplot,
     & varconc,xcal,ycal,ntime,dt,xcalsav,ycalsav,ncsav,plotsav,
     & prepulse,bmj0z,bmj0n,bmjz,bmjn,ndc1,ndimc,
     & ncalc,drcurve,Q1,align,bm,plotocc,plotcur,plotrate,
     & varate,titlep,npv,npulse,tpgap,nlig)
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
	ndimc=10
	if(.not.allocated(xcal)) then
	   ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
	endif
	print 381
	if(pon()) write(7,381)
	if(discprt) write(8,381)
381	format(/,
     &' TIME COURSE FOR AVERAGED ALIGNED EXCISED STEADY-STATE BURSTS')
202	xAs=1.e6*xAs	!value from .ini
	print 201,xAs
201	format(
     & ' Concentration for steady-state record (micromolar) [',
     &  f9.3,'] = ')
	call INPUTr(xAs)
	xAs=xAs*1.e-6
	if(nlig.eq.2) then
	   print 32
c32		format('  and for ligand #2:')
	   print 201
	   call INPUTr(xB0)
	   xb0=xB0*1.e-6
	endif
	print 203,xAs*1.e6
	if(pon()) write(7,203) xAs*1.e6
	if(discprt) write(8,203) xAs*1.e6
203	format(
     & ' Concentration for steady-state record (micromolar) = ',g13.6)
	call QNEWC(QD,cA1,cB1,Q1,xAs,xB0,ncdep,nlig,IL,IX,JX,k,km)
	call EQOCCd(Q1,k,k-1,km,pinf)	!calc new init occs at xAs
	call SLIFED2(Q1,pinf,pon(),discprt,km,km)
	ans='Y'
	call DCASK('Is this concentration O.K.',ans,ans)
	if(ans.eq.'N') goto 202
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
	if(pon()) write(7,71)(phib(j),j=1,kA)
      if(discprt) write(8,71)(phib(j),j=1,kA)
71	format(' Initial vector for burst:',2x,8g13.6)
c
c  Do expansion of QEE
	call SUBMAT(Q1,EE,Q2,km,km,km,km)
	CALL QMAT5(Q2,Amat,kE,eigen,ibad,km,km,km)
	if(debug()) then
	   call DATYP(Q2,'  QEE   ',pon(),kE,kE,km,km)
	   print 700
c700	   format(' Print spectral expansion matrices [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	   if(UC(ans).eq.'Y') then
		do m=1,kE
		 call DATYP3(amat,m,'A matrix',pon(),kE,kE,km,km)
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
      if(pon()) write(7,361)
      if(discprt) write(8,361)
361	format(/,' Time course for P(open)',/,
     & ' Component  eigenvalue        tau (ms)      amp at t=0     ',
     & '     Area    ')
	do m=1,kE
	    print 39,m,sngl(eigen(m)),tau(m),bm(m),area(m)
          if(pon()) write(7,39) m,sngl(eigen(m)),tau(m),bm(m),area(m)
          if(discprt) write(8,39) m,sngl(eigen(m)),tau(m),bm(m),
     &	area(m)
39	    format(i8,3x,g13.6,3x,g13.6,3x,g13.6,4x,g11.4)
	enddo
c Trivial to show that this result must asymptote to 0
	print 391,pop0,atot
      if(pon()) write(7,391) pop0,atot
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
     & varate,titlep,npv,npulse,tpgap,nlig)
	align=.false.	!reset
	DEALLOCATE(xcal,ycal)
c
c
99	call DCASK('Another run','y',ans)
	if(ans.eq.'Y') then
	   call DCASK('Modify present rate constants only','y',ans)
	   if(ans.eq.'Y') then
		iflag=3
	      if(pon()) write(7,106)
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
      OPEN(unit=16,file='SCALCS.INI',status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=10240)
	imodold=imod0
=====new form of .ini to be written here -see above (10240 bytes OK?)
	write(16,rec=1) irecq,xA00,xA0,xA1,t0,iprint,icalc,iplot,tpre,
     & xAs,ans1,npulse,tpgap,imodold,xB00,xB0,xB1
	CLOSE(unit=16)
	if(allocated(xcalsav)) then
	   DEALLOCATE(xcalsav,ycalsav)
	endif
c
	call ENDPRINT
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


