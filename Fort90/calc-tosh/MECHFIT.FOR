	program MECHFIT
c To fit a mechanism, defined by a Q matrix, to reduced experimental results
c as described by time constants and areas (for pdfs) or time constants
c and amplitudes for macroscopic currents.  The latter are used to calculate
c y values at a specified series of times and these are compared with the
c y values predicted by the Q matrix; the SSD for each sort of expt
c is suitably normalised to form an overall SSD which is minimised.
c
c NB Assoc rate constants in QD(), and theta(), are stored throughout
c multiplied by the 'standard' concentrations cA1,cB1 inherited from QGEN,
c and these are replaced by the appropriate concentration as neccesary.
c
c NB lowval(50),highval(50) are stored WITH conc (for c-dep rates)  (unlike
c loguess, higuess), because must have conc incorporated when used
c in simplex and ssdmf (standard conc, cA1).  Before storing in .INI
c the conc factor is removed
c Modif 06/29/99 06:22pm to remove all reference to cA1 -rates AND limits have
c now  NOT got concentration in them until needed in SSDMF and MFOUT for actual
c calcs.  In any case any conc used here would be arbitrary since actual
c conc may differ from one set to another
c
c Modif 06/17/99 11:12am so that micro rev need not be obeyed -obeymr=true
c should be same as before -in common/mr/obeymr which is in eqoccd, getqd
c
c For GETOBS
	character*50 titype(20)		!data types
	character*10 titlep(50)
	character*60 titlef	!file name
	character*60 titled(20)
	character*40 titlex,titley
	integer*4 idtype(20)
c	real*4 xA0(20),xA(20),xA1(20),tpulse(20),Yinf(20)
	integer*4 ncomp(20)
c	real*4 ncomp(20),Vmem(20)   !for (V-Vrev)
	real*4 tau(10,20),area(10,20)		!nc=component #; np=idtype
c	real*4 times(1000,20)
c	real*4 Yobs(1000,20)
c	real*4 weight(1000,20)
c	integer ntime(20)
c	integer juse(20)
c For GETQD
	real*8 QD(10,10),dgamma(10)
	real*4 gamma(10)
	common/qblk/IQ(10,10)		!from getqd
	logical renum
	common/ren/renum		!so can be kept in .ini if req
c from simplex/ssdmf/mfout
	logical bad,abort
	common/abt/abort,ikey		!from simplex4
c For RANDSK
	common/rand/ix1,iy1,iz1
c For mouse
c	logical mouse_on,mous_set
c	common/mousval/mouse_on,nbutton
c
	real*4 theta(50)
	real*4 loguess(50),higuess(50)
	logical readini,readat,present,auto,repeat
	logical concdep		!function
	character*1 ans
	character*11 cdate,ctime
	LOGICAL pon,debug
	logical slock,caplock
	character infil*33
c for MFSIMP, SSDMF
	real*4 lowval(50),highval(50),pertval(50)		!parameter
	common/parlim/lowval,highval,pertval
c
c For GETQD
c	common/Vval/vhold,vkin,vref,gamma(10)		!V in mV
	common/Vval/vhold,vkin,vref,gamma			!V in mV
	integer NSC(8),IM(8,15),JM(8,15)
	COMMON/KM2/AKA1,BA,PSTAR,KMCON(9),KMFAST,aka2,arat
	COMMON/VPAR/NVDEP,IV(10),JV(10),HPAR(10)
	COMMON/CPAR/NCDEP,IX(10),JX(10),X
	COMMON/QPAR/NCON,IC(2,20)
	COMMON/MPAR/NCYC,NSC,IM,JM
	COMMON/EBLK/NEQ,IE(10),JE(10),IF(10),JF(10),EFAC(10)
c /eblk/=51*4 bytes
	COMMON/LIG/nlig,IL(10)
c end of new declarations
	common/data2/nset,nsfit,juse(20),jmiss(20)
	common/data3/idtype
	common/data4/npar,cA1,cB1
	logical discprt
	common/dp/discprt
	character*40 mtitle1*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,jcol,mtitle1 !for WINPRINT,ENDPRINT,DISCNUM
	common/KBLK/kA,kB,kC,kD
c for eqoccd, getqd
	logical obeymr
	common/mr/obeymr		!true if microscopic reversibility to be obeyed
c
	pon()=slock()
	debug()=caplock()
c
101	format(a1)
c	call SETMOUSE()		!define values in common\mousval\ (in IVLIB)
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
	readini=.false.
	INQUIRE(file='MECHFIT.INI',exist=present,flen=nlen)
	if(present.and.nlen.gt.0) then
	   if(nlen.eq.512) then
           OPEN(unit=16,file='MECHFIT.INI',status='UNKNOWN',
     &      access='DIRECT',form='UNFORMATTED',recl=512)
	     read(16,rec=1) infil,irecq,kA,kB,kC,kD,ifile1,
     &	 neq,IE,JE,IF,JF,efac,loguess,higuess
	     CLOSE(unit=16)
           OPEN(unit=16,file='MECHFIT.INI',status='UNKNOWN',
     &      access='DIRECT',form='UNFORMATTED',recl=1024)
	     write(16,rec=1) infil,irecq,kA,kB,kC,kD,ifile1,
     &	 neq,IE,JE,IF,JF,efac,loguess,higuess,
     & 	lowval,highval,pertval,renum
	     STOP
	   endif
	   if(nlen.eq.1024) then
	     readini=.true.
           OPEN(unit=16,file='MECHFIT.INI',status='UNKNOWN',
     &      access='DIRECT',form='UNFORMATTED',recl=1024)
c bytes: 33 + 4*7 + 4*40 + 4 + 2*80 + 3*80
	     read(16,rec=1) infil,irecq,kA,kB,kC,kD,ifile1,
     &	 neq,IE,JE,IF,JF,efac
	   endif
	   if(nlen.eq.2048) then	!for 50 param
	     readini=.true.
           OPEN(unit=16,file='MECHFIT.INI',status='UNKNOWN',
     &      access='DIRECT',form='UNFORMATTED',recl=2048)
c bytes: 33 + 4*7 + 4*40 + 4 + 2*80 + 3*80
	     read(16,rec=1) infil,irecq,kA,kB,kC,kD,ifile1,
     &	 neq,IE,JE,IF,JF,efac,loguess,higuess,
     &	 lowval,highval,pertval,renum
	   endif
	endif
c
	kmax=10		!dimension of Q
	km=10				!array dimensions
	auto=.false.
c
c Get the summarised data in terms of time constants and areas/amps,
c for each type of expt to be fitted
	    call GETOBS(titlef,titled,titlex,titley,ifile,idtype,
     &    nfile,pon(),readini,readat,ndisc,infil,ifile1,
     &    titype,tau,area,ncomp,iver)
c
c Next get the Q matrix defined at conc xA,xB to define the model
c and provide initial guesses for fitting
c Check whether to use macro rev before calling GETQD
	ans='Y'
	obeymr=.true.
	call DCASK(
     &  'Constrain rates to obey microscopic reversibility',ans,ans)
	if(ans.eq.'N') then
	   print 11
	   if(discprt) write(8,11)
11	   format(/,'Rates do NOT obey microscopic reversibility',/)
	   obeymr=.false.
	   ncyc=0
	else
	   print 12
	   if(discprt) write(8,12)
12	   format(/,
     &	' Rates constrained to obey microscopic reversibility',/)
	endif
c
	idest=0
	iprint=5		!for now -see SCALCS
	kflag=2		!use kA,kB,kC,kD from .ini
	call GETQD(QD,cA1,cB1,nmod,nchan,dgamma,vkin,vhold,vref,
     &  npar,titlep,iflag,iprint,readini,irecq,kflag,idest)
	k=kA+kB+kC+kD
	do i=1,k
	   gamma(i)=sngl(dgamma(i))
	enddo
c cA,cB=conc at which QD is defined
	if(.not.obeymr) ncyc=0		!is this sufficient to remove constraint?
	call MFSIMP(QD,theta,titlep,titype,idtype,npar,readini,cA1,
     & cB1,auto,loguess,higuess,repeat,tau,area,ncomp)
c Keep .ini -first remove conc from lowval() etc-CONC NOT IN NOW
c	do m=1,npar
c	   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
c	   if(CONCDEP(i,j)) then
c		lowval(m)=lowval(m)/cA1
c		highval(m)=highval(m)/cA1
c		pertval(m)=pertval(m)/cA1
c	   endif
c	enddo
c
      OPEN(unit=16,file='MECHFIT.INI',status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=2048)
c     & access='DIRECT',form='UNFORMATTED',recl=1024)
	write(16,rec=1) infil,irecq,kA,kB,kC,kD,ifile1,
     &	 neq,IE,JE,IF,JF,efac,loguess,higuess,
     & 	lowval,highval,pertval,renum
	CLOSE(unit=16)
c
	if(auto) call RANDSK(ix1,iy1,iz1,1,repeat)		!write IX,IY,IZ
c
	call ENDPRINT
	end


c Functions for expmaxd:

	real*8 function EXPFUNC(t)
c Evaluate exponentials
	real*8 s,t,w,tau,yinf
	common/expd/ w(10),tau(10),yinf,k
c
	s=0.0d0
	do i=1,k
c=	   s=s + w(i)*dexp(-t/tau(i))
	   s=s + AEXPd1(w(i),-t/tau(i))	!avoid underflow
	enddo
	EXPFUNC=s+yinf
	RETURN
	end

	real*8 function EXPFUNC1(t)
c First derivative of exponentials
	real*8 s,t,w,tau,yinf
	common/expd/ w(10),tau(10),yinf,k
c
	s=0.0
	do i=1,k
c=	   s=s - (w(i)/tau(i))*dexp(-t/tau(i))
	   s=s - AEXPd1(w(i)/tau(i),-t/tau(i))	!avoid underflow
	enddo
	EXPFUNC1=s
	RETURN
	end



