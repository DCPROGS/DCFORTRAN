	program MECHFIT
c To fit a mechanism, defined by a Q matrix, to reduced experimental results
c as described by time constants and areas (for pdfs) or time constants
c and amplitudes for macroscopic currents.  The latter are used to calculate
c y values at a specified series of times and these are compared with the
c y values predicted by the Q matrix; the SSD for each sort of expt
c is suitably normalised to form an overall SSD which is minimised.
c
=========from SCALCS
	REAL*8 QD(10,10),PImat(10,10),Pinf(10),P0(10),dgamma(10)
	real*8 eigen0(10),p00(10),pinf0(10),bmj0(10,10)	!for ON in 'pulse' case
	real*8 Amat(10,10,10),eigen(10)
	real*8 Q1(10,10),pt0(10),pt(10)
	real*8 p00z(10),p00n(10),bmj0z(10,10),bmj0n(10,10)	!for prepulse calcs
	real*8 p0z(10),p0n(10),bmjz(10,10),bmjn(10,10)	!for prepulse calcs
	real*8 bm0(10),bmj(10,10),bm(10),bmrel(10)
	real*8 cm(10),pi,var1,s
	real*4 cv0(10),g0(10),area(10),fc(10)
c	real*4 cur(10),cur0(10),tau(10)
	real*8 cur0(10),tau(10),curinf,cur0tot,cfac1
	real*8 cur00(10),tau0(10),curinf0	!ditto for 'on relaxn' for 'pulse'
	real*4 gamma(10)
c	real*4 xcal(2048,10),ycal(2048,10)
	ALLOCATABLE Xcal,Ycal
	real Xcal(:,:),Ycal(:,:)		!for VPLOT
	real*4 pstar(4)
c=	integer IX(10),JX(10),IL(10)		!NB 'ix' is used by RANDOM
c Declarations for modif 03/05/95 06:16pm
	integer NSC(8),IM(8,15),JM(8,15),IX(10),JX(10),IL(10)
	COMMON/KM2/AKA1,BA,PSTAR,KMCON(9),KMFAST,aka2,arat
	COMMON/VPAR/NVDEP,IV(10),JV(10),HPAR(10)
	COMMON/CPAR/NCDEP,IX,JX,X
	COMMON/QPAR/NCON,IC(2,20)
	COMMON/MPAR/NCYC,NSC,IM,JM
	COMMON/EBLK/NEQ,IE(10),JE(10),IF(10),JF(10),EFAC(10)
	COMMON/LIG/nlig,IL
c end of new declarations
c For mouse
	logical mouse_on,mous_set
	common/mousval/mouse_on,nbutton
	logical vjump,cjump,jump,noise,kmfast,calinit,bad,pulse,qprt
	logical varconc		!true for time-dep concentration calcs
	logical prepulse
	logical allocated,readini,present
	character*1 ans,UC
	character*11 cdate,ctime
	LOGICAL pon,debug
	logical slock,caplock
c	character ndev*2,infil*14
	logical discprt
c
c
	common/dp/discprt
	character*40 mtitle1*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,jcol,mtitle1 !for WINPRINT,ENDPRINT,DISCNUM
	common/KBLK/kA,kB,kC,kD
	COMMON/dimblk/K,KMAX,ir,nmod
c	common/Vval/vhold,vkin,vref,gamr,enchan		!V in mV
	COMMON/RBLCK/TRESO,TRESG,IACRIT,iw
	common/ccalc/cmax,trise,tdec,nshape
	common/db/ideb		!debug level
c
	pon()=slock()
	debug()=caplock()
c
c
101	format(a1)
	call SETMOUSE()		!define values in common\mousval\ (in IVLIB)
c4	format(g13.6)
	filnam='MECHFIT.PRT'
	call WINPRINT	!print file control
      OPEN(unit=7,file=prtport,iostat=nerr)		!open printer
	print 1
	if(pon()) write(7,1)
	if(discprt) write(8,1)
1	FORMAT(' MECHFIT: Preliminary fit of model to reduced obs',/)
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
c
========
	readini=.false.
	INQUIRE(file='MECHFIT.INI',exist=present,flen=nlen)
	if(present.and.nlen.gt.0) then
	   readini=.true.
         OPEN(unit=16,file='MECHFIT.INI',status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=512)
	   read(16,rec=1)
===========irecq,xA00,xA0,xA1,t0,iprint,icalc,iplot,tpre
	   CLOSE(unit=16)
	endif
===========
	kmax=10		!dimension of Q
	km=10				!array dimensions
c
c First get the Q matrix defined at conc xA,xB to define the model
c and provide initial guesses for fitting
	idest=0
105	continue
	call GETQD(QD,xA,xB,nchan,dgamma,vkin,iflag,iprint,
     &  readini,irecq,idest)
===============
c Next get the summarised data in terms of time constants and areas/amps,
c for each type of expt to be fitted
============
	call GETOBS()
========set up simplex, and minimise SSD
========print (& plot?) results

