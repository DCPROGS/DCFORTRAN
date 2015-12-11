	program SCJUMP
C PROG TO CALC OPEN TIMES,BURST LENGTHS ETC AFTER JUMP
c
c Modif 11/23/01 11:00am for obeymr(50). Rather than add this to .ini,
c set elements of nsc(i) negative (for .ini ONLY) if obeymr(i)=false
c
c V5.2 version 03/30/94 01:36pm
c Lahey Version 12/11/91 12:17pm
c Version for 11-73 Aug 1988
C
C PROG TO CALC OPEN TIMES,BURST LENGTHS ETC AFTER JUMP
C
C	A=SUBSET OF OPEN STATES
C	B=SUBSET OF INTRABURST GAP STATES
C	C=SUBSET OF INTER-BURST GAP STATES
C	E=(A,B)=BURST STATES (D IN OLD NOTES)
C	F=(B,C)=SHUT STATES
C	'BURST' HERE='APP BURST' IN C&H II
C
C TRY MAX DIMENSION=10,MAX IN A SUBSET=7, MAX KA=5
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*4 W(100),RATE(100),am,sd,x00,x0,x1
	real*4 area(100),amean(100),dum
	real*4 vhold,vkin	!for read from qdta
	real*4 xA0s,xA1s,xb0s,xb1s,xA00s,xB00s
	real*4 t0			!for PTCALC
	dimension P0(100)		!holds initial occs
	dimension Peq(100)			!holds final occs
	dimension phi0(100)		!INIT VECTOR AT t=0
	dimension dgamma(100)
	dimension p00(100),Peq0(100),pt0(100),bmj(100,100)
C AMAT,HMAT=SPECTRAL EXPANSION MATRICES
	dimension AMAT(100,100,100),Hmat(100,100,100)
c	dimension EM(10,10),EN(10,10),am1(10)
c	dimension PSI(1,10),END2(10,1),eigaa(10)
	dimension END2(100,1)
	dimension QT(100,100),QD(100,100)
	dimension Q0(100,100),Q1(100,100),Q2(100,100)
	dimension Q3(100,100),Q4(100,100)	!TEMP ARRAYS
	dimension GAB(100,100),GBA(100,100),HAA(100,100)
	dimension RAA(100,100),ZAA(100,100)
	dimension ROW1(1,100),ROW2(1,100),COL1(100,1),COL2(100,1)
	dimension UCOL(100,1)
	dimension PHI(1,100),END1(100,1)
C =START AND END VECTORS
	dimension SCALAR(1,1)
	dimension W1(100)
	dimension EIGHAA(100),EIGBB(100),EIGEN(100)
C    DECLARE INTEGER ARGUMENTS FOR SUBMAT
	INTEGER A,C,F,AA,AB,AC,BA,BB,BC,CA,CB,CC,EE,AF,FA,FF
	LOGICAL EQUIL,OPEN,GAP,SHORT,SKIP,noeq,onlyfst,QCCbad,QFFbad
c
c For new version that uses qmechs.dat
	real*4 conc(10),conc0(10),conc00(10)
	integer irate(200),jrate(200)
	character*20 ligname(10)
	character*10 titlep(200)
	logical jumpto0,agonist(10)
	COMMON/LIG/nlig,IL(100)
	integer ix(100),jx(100)
c=	integer ix1(100),jx1(100)
	COMMON/CPAR/NCDEP,IX,JX,X1
	common/mpar/ncyc,nsc(50),im(50,100),jm(50,100)
	real*4 hpar
	COMMON/VPAR/NVDEP,IV(100),JV(100),HPAR(100)
	integer IE(200),JE(200),IF(200),JF(200)
	real*4 EFAC(200)
	COMMON/EBLK/NEQ,IE,JE,IF,JF,EFAC
	character*2 charmod(25,40)	!to print model
	common/KBLK/kA,kB,kC,kD
c
	character*74 mtitle	!title for model
	logical readini,present

	integer ibnew(100),icnew(100),inew(100)
c=	common/QPAR/ncon,IC
c Misc
	character*11 cdate,ctime
	logical vjump,cjump,calinit
	logical is_route,newsetC
	character*1 ans,UC
	character*40 path
	logical caplock,debug
c	character ndev*2
	logical discprt
	logical student,cluster,win31
c For version with getqd
	character*40 mtitle1*40,filnam*32,prtport*4	!for WINPRINT
	common/model/imod0,charmod,jlast,ilast,mtitle,imodold
	logical renum
	common/ren/renum		!so can be kept in .ini if req
c
	COMMON/determ/det			!from MATINV,GMAT1
	common/dp/discprt
	common/dpp/filnam,prtport,ndisc,jcol,mtitle1 !for WINPRINT,ENDPRINT,DISCNUM
	common/user/student,cluster,win31

c for eqoccd, getqd
	character qfilem*40
	logical obeymr(50),automr(50)
	common/mr/obeymr,automr
c
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
c
	debug()=caplock()
c
C MISC DEFINITIONS
3	format(i8)
c
	cluster=.false.
	win31=.false.
	student=.false.
	call MYPATH(path)
	if(path(1:8).eq.'O:\CVFIT') cluster=.true.
	filnam='SCJUMP.PRT'
	call WINPRINT	!print file control
      OPEN(unit=7,file=prtport,iostat=nerr)             !open printer
	print 1
	if(discprt) write(8,1)
1	FORMAT(' SCJUMP: Single channel after a jump',/)
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 2,cdate,ctime(1:8),mtitle1
	if(discprt) write(8,2) cdate,ctime(1:8),mtitle1
2	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 36
36	format(
     & ' SCROLL LOCK on for printing'/
     & ' CAPS LOCK on for debugging')
c
c Initialisations
	km=100		!dimension of all arrays=100 now
	crit=1.0d-20	!BELOW WHICH COEFFS SET TO ZERO
	ZERO=0.0d0
	ONE=1.0d0
c	TWO=2.0d0
C SET UNIT COLUMN
	do i=1,km
	   ucol(i,1)=ONE
	enddo
c
C DEFINE AA,AB ETC AS ARGUMENTS FOR SUBMAT WHICH GETS A SUBMATRIX
C FROM QM. THEY ARE DECLARED AS INTEGERS AND VALUE IJ=ROW,COL- THEY
C ARE ARBITRARY INTEGERS INTERP BY SUBMAT TO GET REQ ROWS AND COLS
	A=1
	C=3
	F=5
	AA=11
	AB=12
	AC=13
	BA=21
	BB=22
	BC=23
	CA=31
	CB=32
	CC=33
	AF=15	!5=CODE FOR F(=B+C) SECTION
	FA=51
	FF=55
	EE=66		!6=CODE FOR E(=BURST=A+B)
c
cc Now modified input as in HJCFIT, SCBST etc
c
c Read the INIT file (tres kept in microsec here)
	readini=.false.
	INQUIRE(file='SCJUMP.INI',exist=present,flen=nlen)
	if(present.and.nlen.gt.0) then
	   print 48
48	   format(' Read defaults from last run from disc [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
         if(ans.eq.'Y') then
		readini=.true.
		if(nlen.eq.1024) then		!old.ini
      	   OPEN(unit=19,file='SCJUMP.INI',status='UNKNOWN',
     &         access='DIRECT',form='UNFORMATTED',recl=1024)
		   read(19,rec=1) ioptsav,xa0s,xa1s,cjump,xb0s,xb1s,calinit,
     &	   t0,xa00s,xB00s,imodold,irecq
	         if(irecq.lt.1) irecq=1		!temp!
		   conc(1)=xA1s
		   conc(2)=xB1s
		   conc0(1)=xA0s
		   conc0(2)=xB0s
		   conc00(1)=xA00s
		   conc00(2)=xB00s
	         CLOSE(unit=19)
c irecq=file # for rates
		else 		!NEW ini
      	   OPEN(unit=19,file='SCJUMP.INI',status='UNKNOWN',
     &          access='DIRECT',form='UNFORMATTED',recl=5120)
		   read(19,rec=1) iver
		   if(iver.eq.101) then
			read(19,rec=1) iver,irecq,nvdep,conc00,conc0,conc,
     & 		nmax,ncyc,ioptsav,cjump,calinit,t0,imodold,
     & 		(nsc(i),i=1,ncyc),
     &		((im(i,j),j=1,nsc(i)),i=1,ncyc),
     &		((jm(i,j),j=1,nsc(i)),i=1,ncyc),
     &		qfilem
			if(UC(qfilem(1:1)).ne.'Q') qfilem='qmechs.mec'
	      	CLOSE(unit=19)
			do i=1,ncyc		!if nsc(i) is neg, set obeymr(i)=F and restore nsc
			   if(nsc(i).lt.0) then
				obeymr(i)=.false.
				nsc(i)=iabs(nsc(i))
			   else
				obeymr(i)=.false.
			   endif
			enddo
		   endif
		endif
	   endif
	endif
c

43	continue
	kflag=0
c Modif 06/25/03 09:06am in getqd to put npar and irate, jrate also in commons
	call GETQD(QT,nchan,dgamma,vkin,vhold,vref,
     &  titlep,ligname,iflag,iprint,readini,irecq,qfilem,
     &  kflag,idest)
c	call GETQD(QT,irate,jrate,nchan,dgamma,vkin,vhold,vref,
c     &  npar,titlep,ligname,iflag,iprint,readini,irecq,qfilem,
c     &  kflag,idest)
c
	k=kA+kB+kC+kD
	kF=kB+kC+kD
	kE=kA+kB
c
c	if(nlig.gt.0) then
c	   do i=1,nlig
c		print 1591
c1591		format(54x,' Before jump    After jump')
c		x0=1.e6*conc0(i)
c		x1=1.e6*conc(i)
c		print 159,i,ligname(i),x0,x1
c159		FORMAT(/,
c     &     ' Ligand #',i2,1x,a20,': concentration (muM) = ',2g13.6)
c	   enddo
c	endif
c
	calinit=.false.
c
c Concentrations from disc are in molar units
c If c-jump, and calinit=true, then not at equilib with initial concentration
c so the initial conditions for the c-jump, p0() and cur0, must be recalculated
c here
c
c Similarly for V-jump enquire if system has come to equilibrium
c at the initial potential ======to be done (but have not got all nec data
c here to recalc Q at diff potentials!)
c
c For c-jump, check the concentrations
	iopt=ioptsav	!from .ini
	print 29,vkin,iopt
29	format(/,
     & '  V = ',f8.1,' mV.',/,
     & ' (1) Calculate a c-jump (assuming initial equilibrium)',/,
     & ' (2) Calculate a c-jump (not assuming initial equilibrium)',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(iopt)
	if(iopt.lt.1.or.iopt.gt.2) iopt=1
	vjump=.false.
	if(iopt.eq.1) then
	   cjump=.true.
	   calinit=.false.
	else if(iopt.eq.2) then
	   cjump=.true.
	   calinit=.true.
	endif
	ioptsav=iopt	!save for .ini
	if(nlig.gt.0) then
         if(discprt) write(8,3111)
3111	   format(/,' CONCENTRATION JUMP')
	   do i=1,nlig
		x0=conc0(i)*1.e6
		x1=conc(i)*1.e6
		print 20,i,ligname(i),x0,x1
20		format(
     & ' Initial and final conc (muM) for ligand #',i2,'(',
     &	   a10,')',/,'   [',g11.4,',',g11.4,'] = ')
		call INPUT2r(x0,x1)
		conc0(i)=x0*1.e-6
		conc(i)=x1*1.e-6
c     now print
            if(discprt) write(8,3112)i,ligname(i),x0,x1,vkin
3112	      format(
     &   ' Ligand ',i2,' (',a10,'): jump from ',g13.6,' to ',g13.6,
     &   ' micromolar',/, ' at V = ',f8.1,' mV')
	   enddo
c
	   if(calinit) then
		t0=t0*1000.
		print 21,x0,t0
21		format(
     &   ' Length of exposure to ',g13.6,' muM (msec) [',f8.2,'] = ')
		call INPUTr(t0)	!NB PTCALC expects t0 as real*4
		t0=t0*0.001		!in seconds
		do i=1,nlig
		   x00=conc00(i)*1.e6
		   print 22,i,ligname(i),x00
22		   format('&Concentration of ligand ',i2,' = ',a10,
     &		' before this (muM) [',f8.2,'] = ')
		   call INPUTr(x00)
		   conc00(i)=1.e-6*x00	!molar
		enddo
	   endif
c Insert section here (12/14/94 09:02am) to check on correct allocation
c to set C if final conc=0.  For every shut state, check whether there
c is a route to an open state (when xA=0).  If not, that shut state must
c be part of the closed set, C. Renumber states and reset kB, kC, if necessary
c to achieve this
c More generally (04/09/01 05:51am) need to check if conc of ALL agonists
c is zero -if all are zero set jumpto0=true
	   jumpto0=.true.
	   do i=1,nlig
		ans='N'	!default
		if(i.eq.1) ans='Y'	!default
c		if(agonist(i)) then	!default from .ini
c		   ans='Y'
c		else
c		   ans='N'	!default
c		endif
		print 221,i,ligname(i),ans
221		format(' Is ligand ',i2,' = ',a10,' an agonist [',a1,'] ? ')
		call INPUTa(ans)
		agonist(i)=ans.eq.'Y'
		if(agonist(i)) then
		   if(conc(i).gt.1.e-20) jumpto0=.false.
		endif
	   enddo
	   if(jumpto0) then
c	  Define IQ here when same Q used -IQ not needed here
c===if chardef??
		call CQLAST(charmod,ilast,jlast)		!get ilast,jlast from charmod
		call CHARQ(charmod,ilast,jlast,kA,kF,ncon,ic)	!get IC,kA,kF
		kBnew=0
		kCnew=0
		do i=kA+1,kA+kF
		   call GETROUTE(i,kA,kF,ix,jx,il,ncdep,
     &	.true.,.false.,is_route)
		   if(.not.is_route) then
			kCnew=kCnew+1
			icnew(kcnew)=i		!record this shut state for set C
		   else
			kBnew=kBnew+1
			iBnew(kBnew)=i		!record this shut state for set B
		   endif
		enddo
c check if the new set C is different from that already defined
c If kC not changed then check that elements in icnew are k,k-1 ...
c ie the highest-numbered states, ie check that thay are all above kA+1
		do i=1,k		!initialise INEW
		   inew(i)=i
		enddo
c   Order the values in icnew() in ascending order
		call SORTI(icnew,kcnew,.true.,km)
		call SORTI(ibnew,kbnew,.true.,km)
		newsetC=.false.
		kE=kB+kC
		if(kC.ne.kCnew) then
		   call BELL(3)
		   print 40
40		   format(' The following states form a closed set:')
		   print 41,(icnew(i),i=1,kcnew)
41		   format(10i4)
		   m=kcnew-1
		   do i=1,kcnew
			if(icnew(i).ne.k-m) newsetC=.true.
			m=m-1
		   enddo
		   if(.not.newsetC) then
			print 42
42			format(' Redefine kC, kB so these form set C [Y] ? ')
			ans='Y'
			call INPUTa(ans)
			if(ans.eq.'Y') then
			   kC=kcnew  	!new value
			   kB=k-kA-kC	!new value
			   kE=kA+kB		!new value
			endif
		   endif
		endif
		if(newsetC) then
		   kC=kcnew  	!new value
		   kB=k-kA-kC	!new value
		   kE=kA+kB		!new value
		   call BELL(3)
		   print 401
401		   format(
     & ' The following states form a closed set, and states must be',/,
     & ' re-numbered so they are the highest-numbered states (set C):')
		   print 41,(icnew(i),i=1,kcnew)
c41		   format(10i4)
c   If the states in set C are the kC highest numbered states then need
c   only reset kB and kC, but if an enlarged set C contains state numbers
c   other (lower numbers) than these then states must be re-numbered
c		New C states are numbered kE+1,...,k. Change state numbers
c		only when necessary!
		   do i=kE+1,k		!C states
			j1=i-kE		!index for icnew=1,2,...,kC
			do j=kE+1,k
			   if(j.eq.icnew(j1)) goto 90
			enddo
			inew(i)=icnew(j1)
90			continue
		   enddo
c             new B states=all shut states that are not in the new C
c			where B states are numbered kA+1, kA+kB.  States kA+1
c			to k are still the shut states whatever reordering is
c			done here, so go through all shut states, skipping those
c			that are in the new C set
		   do i=kA+1,kE		!B states
			j1=i-kA		!index for ibnew=1,2,...,kB
			do j=kA+1,kE
			   if(j.eq.ibnew(j1)) goto 91
			enddo
			inew(i)=ibnew(j1)
91			continue
		   enddo
c Print model with the new numbering
		   print 49
      	   if(discprt) write(8,49)
49		   format(/,
     & ' Subset C must be redefined so that it is now a closed set at',
     & /,'  zero conc. New numbering stored in RENUM.DAT.',/,
     & '   Set            Old state    New state',/,
     & ' Subset A:')
		   do i=1,kA
			print 45,i,inew(i)
		      if(discprt) write(8,45) i,inew(i)
45			format(
     & '                  ',i5,'       ',i5)
		   enddo
		   print 46
      	   if(discprt) write(8,46)
46		   format(' Subset B:')
		   do i=kA+1,kA+kB
			print 45,inew(i),i
		      if(discprt) write(8,45) inew(i),i
		   enddo
		   print 47
      	   if(discprt) write(8,47)
47		   format(' Subset C:')
		   do i=kA+kB+1,k
			print 45,inew(i),i
		      if(discprt) write(8,45) inew(i),i
		   enddo
		   call NEWMOD(charmod,inew,ilast,jlast,.false.)
		   renum=.true.
      	   OPEN(unit=14,file='RENUM.DAT',status='UNKNOWN',
     & 	    access='DIRECT',form='UNFORMATTED',recl=512)
		   idum=0
		   write(14,rec=1) idum,kA,kB,kC,kD,inew
      	   CLOSE(unit=14)
		   goto 43	!back to getqd for renumbering
		endif		!end of newsetC case
	   endif
c
44	   continue
	   if(nlig.eq.1) then
           if(discprt) write(8,311) x0,x1,vkin
311	     format(/,
     &     ' CONCENTRATION JUMP from ',g12.5,'to ',g12.5,'micromolar',
     &    ' at V = ',f7.1,'mV')
	   else
           if(discprt) write(8,312) vkin
312	     format(/,
     &     ' CONCENTRATION JUMP at V = ',f7.1,'mV',/,
     &	' concentration (muM)     before jump       after jump')
		do i=1,nlig
		   if(discprt) write(8,313)i,ligname(i),conc0(i)*1.e6,
     &	    conc(i)*1.e6
313		   format(1x,i2,1x,a20,2g13.6)
		enddo
	   endif
	endif		!end of nlig.gt.0
	if(cjump.and.(.not.calinit)) then
	   V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	   call QSETD(conc0,IL,V1,QT,Q0,k,.false.)
c==	   call QNEWC(QM,cA1,cB1,Q0,xA0,xB0,ncdep,nlig,IL,IX,JX,k,km)
	   call EQOCCd(Q0,k,k-1,km,p0)	!calc new init occs at xA0
	   call QSETD(conc0,IL,V1,QT,Q2,k,.false.)
	else if(cjump.and.calinit) then
	   if(nlig.eq.1) then
            if(discprt) write(8,23) x0*1.e6,t0*1000.,x00
23	      format(
     &  ' Initial condition for conc jump not at equilibrium:',/,
     &  '   at ',g13.6,' muM for ',g13.6,' ms, starting from ',g13.6,
     &  ' muM')
	   else
           if(discprt) write(8,314) t0
314	     format(/,
     &     ' Initial conditions not at equlibrium: before jump',/,
     &     ' start from '' start conc'', then ',g13.6,'ms at prejump',/,
     &     ' before jumping to post-jump conc (all micromolar)',/,
     &     ' ligand   start from     pre-jump       post-jump')
		do i=1,nlig
               if(discprt) write(8,315) i,ligname(i),conc00(i)*1.e6,
     &	   conc0(i)*1.e6,conc(i)*1.e6
315	         format(1x,i2,1x,a20,3g13.6)
		enddo
	   endif
c Now recalculate the initial condition
c First get Q at conc=xA00. The QM from disc is for concentrations xA1,xB1
c so alter the conc dep rates proportionally- use subroutine QNEWC()
c=	   call QNEWC(QM,cA1,cB1,Q0,xA00,xB00,ncdep,nlig,IL,IX,JX,k,km)
	   V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	   call QSETD(conc00,IL,V1,QT,Q0,k,.false.)
	   if(debug()) call ATYPD(Q0,'  Q(00)  ',k,k,km,km)
	   call EQOCCd(Q0,k,k-1,km,P00)		!calc equilib occs at xA00
c calc Peq at xA0, and Q matrix for this conc
	   V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	   call QSETD(conc0,IL,V1,QT,Q2,k,.false.)
c=	   call QNEWC(QM,cA1,cB1,Q2,xA0,xB0,ncdep,nlig,IL,IX,JX,k,km)
	   if(debug()) call ATYPD(Q2,' Q(0)    ',k,k,km,km)
	   call EQOCCd(Q2,k,k-1,km,Peq0)	!calc equilib occs at xA0
	   call QMAT5(Q2,Amat,k,eigen,ibad,km,km,km)
c NB Peq0 should be same as P0 from disc, if xA0 not changed
	   if(ibad.ne.0) print 14,ibad
14	   format(' ***ERROR IN SPECTRAL EXPANSION: ibad= ',i4)
	   if(debug()) then
		print 700
700		format(' Print spectral expansion matrices for Q [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		if(UC(ans).ne.'N') then
		   do m=1,k
			call ATYPD3(amat,m,'A matrix',k,k,km,km,km)
		   enddo
		endif
	   endif
c Now calc p(t0)=initial condition for the jump, in pt0
	   call PTCALC(pt0,t0,bmj,p00,Peq0,eigen,amat,k,km)
	   do j=1,k
		p0(j)=pt0(j)	!new initial occs
	   enddo
	endif
c
c Recalc final occs (in case conc changed), and expand QM
c=	call QNEWC(QM,cA1,cB1,Q1,xA1,xB1,ncdep,nlig,IL,IX,JX,k,km)
	V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	call QSETD(conc,IL,V1,QT,Q1,k,.false.)
	if(debug()) call ATYPD(Q1,' Q1     ',k,k,km,km)
	call EQOCCd(Q1,k,k-1,km,Peq)	!calc equilib occs at xA1
	call QMAT5(Q1,Amat,k,eigen,ibad,km,km,km)
c NB Peq should be same as Peq from disc, if xA1 not changed
	if(ibad.ne.0) print 14,ibad
	   if(debug()) then
		print 700
c700		format(' Print spectral expansion matrices [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		if(UC(ans).ne.'N') then
		   do m=1,k
			call ATYPD3(amat,m,'A matrix',k,k,km,km,km)
		   enddo
		endif
	   endif
c
c Print initial and final occs
	print 26
      if(discprt) write(8,26)
26	format(/,' Initial occupancy and final equilibrium occupancies:')
	do j=1,k
	   print 25,j,p0(j),j,Peq(j)
         if(discprt) write(8,25) j,p0(j),j,Peq(j)
25	   format(' p0(',i2,') = ',g13.6,3x,' p(inf)(',i2,') = ',g13.6)
	enddo
c END OF SECTION TO FIX INITIAL CONDITION
	print 110
110	FORMAT(' Replace this P(0) with a new one [N] ? ')
	ans='N'
	call INPUTa(ans)
	if(UC(ans).eq.'Y') then
	   do i=1,k-1
	      print 113,i
113	      FORMAT(' P0(',i2,')= ')
	      call INPUTr(dum)
	      p0(i)=dble(dum)
	   enddo
	endif
	print 348
348	FORMAT(' Type Q matrices [Y]? ')
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).ne.'N') then
	   call ATYPD(Q2,'Q(t < 0)',k,k,km,km)
	   call ATYPD(Q1,'Q(t > 0)',k,k,km,km)
	endif
c
c Copy the new post-jump Q matrix to QD for use in rest of prog
	do i=1,k
	   do j=1,k
		QD(i,j)=Q1(i,j)
	   enddo
	enddo
c
c
	SHORT=.FALSE.
c	itcor=0
	print 341
341	FORMAT(' Short version [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).ne.'N') SHORT=.TRUE.
c	IF(short) GOTO 902
c	print 1201
c1201	format(' (1) Long version',/,
c     & ' (2) short version (for correlation calcs)',/,
c     & ' (3) ultra-short version (ditto)',/,
c     & ' Option number [1] = ')
c	read 3,itcor
c	if(itcor.eq.0) itcor=1
c	itcor=itcor-1	!=0,1,2
c902	continue
C IF ITCOR=0 ORDINARY VERSION
C IF ITCOR=1 SHORT VERSION FOR CORRELATION TEST WITH PRINT OF ARRAYS
C IF ITCOR=2 ULTRA SHORT (RANK AND EIGENVALUES ONLY)
c	print 1021,itcor
c      if(discprt) write(8,1021)itcor
c1021	format(' itcor= ',i4)
c
902	continue
	call SLIFED2(QD,Peq,.false.,.false.,km,km) !screen only
      print 108		!new line
      if(discprt) write(8,108)
c
c For debug at least print ranks of relevant matrices
c	subroutine RANK(Q,KRQ,KCQ,KQ1,KQ2,IRANK)
	call SUBMAT(QD,AA,Q1,km,km,km,km)		!QAA in Q1
	call RANK_SVD(Q1,kA,kA,km,km,irA)
c=	call RANK(Q1,kA,kA,km,km,irA)
	call SUBMAT(QD,BB,Q1,km,km,km,km)		!QBB in Q1
	call RANK_SVD(Q1,kB,kB,km,km,irB)
	call SUBMAT(QD,CC,Q1,km,km,km,km)		!QCC in Q1
	call RANK_SVD(Q1,kC,kC,km,km,irC)
	call SUBMAT(QD,FF,Q1,km,km,km,km)		!QFF in Q1
	call RANK_SVD(Q1,kF,kF,km,km,irF)
	call SUBMAT(QD,EE,Q1,km,km,km,km)		!QEE in Q1
	call RANK_SVD(Q1,kE,kE,km,km,irE)
      print 85,kA,irA,kB,irB,kC,irC,kF,irF,kE,irE
      if(discprt) write(8,85) kA,irA,kB,irB,kC,irC,kF,irF,kE,irE
85	format(' Rank of submatrices at post-jump concentrations',/,
     & ' kA = ',i4,': Rank of QAA = ',i4,/,
     & ' kB = ',i4,': Rank of QBB = ',i4,/,
     & ' kC = ',i4,': Rank of QCC = ',i4,/,
     & ' kF = ',i4,': Rank of QFF = ',i4,/,
     & ' kE = ',i4,': Rank of QEE = ',i4,/)
	QCCbad=irC.lt.kC		!QCC singular
	QFFbad=irF.lt.kF		!QFF singular
c	if(xa1.lt.1.e-20) then
c         print 78
c         if(discprt) write(8,78)
c78	   format(
c     & ' For jump to zero conc, C should contain absorbing states',/,
c     & '  and B should contain non-absorbing shut states.')
c	endif
c
	print 201,kA
201	FORMAT('  Number of open (A) states, kA= ',i4)
	print 202,kB
202	FORMAT('  number of short shut (B) states, kB= ',i4)
	print 203,kC
203	FORMAT('  number of long-lived shut (C) states, kC= ',i4)
	print 204,kD
204	FORMAT(
     & '  number of very long (between cluster) shut states, kD= ',i4)
c	print 342,K,ka,kb,kc
c342	format(' k,kA,kB,kC= ',4i3)
	print 347
347	format(' (NB kD=0 for this program)',/,
     & ' Is this allocation of states to B and C sets O.K. [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).ne.'N') goto 343
346	continue
	kD=0
	print 344
344	format(' kA,kB,kC= ')
	read 345,ka,kb,kc
345	format(3i8)
	if(ka+kb+kc.ne.k) goto 346
	kE=kA+kB
	kF=kB+kC
	kD=0
	k=kA+kB+kC
	goto 902	!for approval
c print values
343	continue
	if(discprt) then
	   write(8,201)kA
	   write(8,202)kB
	   write(8,203)kC
	endif
	print 30
	if(discprt) write(8,30)
30	format(/,
     &' Analysis of state and subset lifetimes at post-jump conc')
	call SLIFED2(QD,Peq,.false.,discprt,km,km) !print it
C
C
C FIRST CALC OPEN AND SHUT TIME PDFS
C ITYPE=1 FOR OPEN AT t=0
C ITYPE=2 FOR SHUT AT t=0
C
C CALC INITIAL VECTOR FOR OPENINGS=PF(INF)*QFA/DEN
	print 81
	if(discprt) write(8,81)
81	FORMAT(/,
     & '    OPEN AND SHUT TIME DISTRIBUTIONS',/,
     & '***************************************')
	call SUBMAT(QD,FA,Q1,km,km,km,km)	!QFA IN Q1
c	call ATYPD(Q1,' QFA    ',kF,kA,km,km)
	SUM=ZERO		!PREMULT BY PF(INF)
	do j=1,kA
	   PHI(1,j)=ZERO
	   do L=1,kF
		PHI(1,j)=PHI(1,j)+Peq(L+kA)*Q1(L,j)
	   enddo
	   SUM=SUM+PHI(1,j)
	enddo
c
	if(sum.ge.1.d-20) then
	   noeq=.false.		!equilib pdf CAN be calc
         print 810
         if(discprt) write(8,810)
810	   format(
     & ' Equilibrium initial vector for opening, PHIo =')
	   do j=1,kA	!NORMALISE PHI
	      PHI(1,j)=PHI(1,j)/SUM
	   enddo
	   print 71,(PHI(1,j),j=1,kA)
         if(discprt) write(8,71)(PHI(1,j),j=1,kA)
71	   FORMAT(2X,5G13.6)
	else
	   noeq=.true.		!no equilib pdf can be calc
         print 811,phi(1,j),sum
         if(discprt) write(8,811) phi(1,j),sum
811	   format(
     & ' Cannot calc equilibrium initial vector for opening, PHIo,',/,
     & '  (numerator, denominator = ',2g13.6,')')
	endif
C
	print 200,nmax
200	format(
     & ' Calculate pdfs for N intervals after jump: n [',i3,'] = ')
	call INPUTi(nmax)
	if(nmax.le.0) goto 135		!skip straight to bursts
C
C CALC XAA=GAF*GFA IN Q1
      call GMAT1(AF,QD,Q4,km,km)	 !GAF in Q4
      call GMAT1(FA,QD,Q3,km,km)	 !GFA in Q3
	onlyfst=dabs(det).lt.1.d-10	!QFF singular so can calc 1st latencies only
	if(onlyfst) then
         print 812
         if(discprt) write(8,812)
812	   format(' QFF is singular so only first latencies can be found')
	endif
	if(.not.short) then
	   call ATYPD(Q4,' GAF    ',kA,kF,km,km)
	   call ATYPD(Q3,' GFA    ',kF,kA,km,km)
	endif
	call MATMUL(Q4,Q3,Q1,KA,KF,KA,ONE,km,km,km,km,km,km)	!GAF*GFA IN Q1
	if(.not.short) then
	   call ATYPD(Q3,'GAF*GFA ',kA,kA,km,km)
	endif
C
C
C OPEN TIME PDFS
      print 131
      if(discprt) write(8,131)
131	format(/,
     & '      OPEN TIME PDFS',/,
     & '     ================')
C FINAL VECTOR= -QAA*UA IN COL1- FOR OPEN TIME DISTS
	call MATMUL(QD,UCOL,COL1,KA,KA,1,-ONE,km,km,km,1,km,1)
	if(.not.short) then
         print 67,(col1(i,1),i=1,kA)
         if(discprt) write(8,67) (col1(i,1),i=1,kA)
67	   format(' End vector -QAA*uA =',7(1x,f10.6))
	endif
C EXPAND -QAA
	call SUBMAT(QD,AA,Q2,km,km,km,km)		!QAA IN Q2
c	CALL QMAT3(Q2,EM,EN,KA,EIGEN,IBAD,KAM,KAM,KMAX)
	call QMAT5(Q2,Amat,kA,eigen,IBAD,km,km,km)
C
	ITYPE=1
	open=.true.
	kX=kA
c Return to 133 to calculate shut time pdfs (with kX=kF, col1=-QFF*uF and
c amat, eigen for QFF)
133	gap=.not.open
C TYPE EIGS
      print 13
      if(discprt) write(8,13)
	do 61 m=1,kX
	rate(m)=sngl(eigen(m))
	if(dabs(eigen(m)).ge.crit) then
         print 7,m,rate(m),-1.0e3/rate(m)
         if(discprt) write(8,7) m,rate(m),-1.0e3/rate(m)
	else
         print 8,m,rate(m)
         if(discprt) write(8,8) m,rate(m)
	endif
61	continue
13	format( '    Eigenvalues             Tau (ms)')
7	format(2x,i2,3x,g13.6,3x,f13.6)
8	format(2x,i2,3x,g13.6)
C CALC phi0()=PHI(A) AT t=0=REL OCCS OF A STATES- FOR ITYPE=1
	call INVEC(A,phi0,row1,P0,skip,km)	!subset 1=A
C KEEP CURRENT INITIAL VECTOR IN ROW1(). INITIALLY phi0() FOR CASE
C WHERE OPEN AT t=0
	do j=1,kA
	   ROW2(1,j)=phi0(j)
	enddo
C CALC PDFS
      print 70
      if(discprt) write(8,70)
70	FORMAT(//,
     & ' CHANNEL OPEN AT t=0',/,
     & ' -------------------')
	if(skip.and.open) then
         print 83
         if(discprt) write(8,83)
83	   format(/,' phiA(0) undefined: cannot calculate open times')
	   goto 199		!STRAIGHT ON TO SHUT AT t=0
	endif
	if(.not.short) then
         print 69,(phi0(j),j=1,kA)
         if(discprt) write(8,69) (phi0(j),j=1,kA)
69	   format(/,' phiA(0) = pA(0)/pA(0)uA =',7(1x,f10.6))
c69	   format(' phiA(0) =',7(1x,f10.6))
	endif
C
1030	CONTINUE	!RETURN HERE FOR SHUT AT t=0
	if(onlyfst.and.open.and.itype.eq.2) goto 99	!skip pdfs
C
	do 100 nlag=1,nmax
	if(gap) call MATMUL(ROW1,Q4,ROW2,1,KA,KF,ONE,1,km,km,km,1,km)  !row1*GAF
C TYPE CURRENT INITIAL VECTOR
	if(open.and.itype.eq.1.and.nlag.eq.1) then
         print 75
         if(discprt) write(8,75)
75	   format(/,' Latency to the first shutting:')
	endif
      print 72,NLAG,(ROW2(1,j),j=1,kX)
      if(discprt) write(8,72) NLAG,(ROW2(1,j),j=1,kX)
72	format(' n =',i3,': Initial vector=',7(1x,f10.6))
	call RAC2(row2,col1,amat,kX,w,w1,km,km,km)
C
C TYPE PDF
C	CALL PDDTYP('  FO',W1,EIGEN,NLAG,-1,KA,0,KMAX,KMAX)
	call PDFOUTd('            ',-1,-1,w1,eigen,kX,am,sd,
     & km,.false.,.true.,discprt)
      print 108		!new line
      if(discprt) write(8,108)
108	format(/)
	if((onlyfst.and.open.and.itype.eq.1.and.nlag.eq.1).or.
     &   (onlyfst.and.gap .and.itype.eq.1.and.nlag.eq.1)) then
	   goto 99		!jump out of loop after 1st latency
	endif
C MULTIPLY INIT VECTOR BY XAA SO PHI*XAA**N ACCUM IN ROW1
	call MATMUL(row1,Q1,row1,1,kA,kA,one,1,km,km,km,1,km) !row1*XAA in row1
	do j=1,kA
	   row2(1,j)=row1(1,j)		!make copy
	enddo
c
100	continue		!end of nlag loop
c
99	continue
	IF(ITYPE.EQ.2) GOTO 98		!LAST TIME
C
C NOW REPEAT FOR CASE WHERE SHUT AT t=0. WANT PHIF(0)*GFA IN phi0
C CALC phi0()=row1=PHI(F) AT t=0=REL OCCS OF F STATES
199	ITYPE=2		!repeat for 'shut at t=0'
	call INVEC(F,phi0,row1,P0,skip,km)		!subset 5=F
	if(skip) then
         print 84
         if(discprt) write(8,84)
84	   format(' phiF(0) undefined: cannot calculate shut times')
	   goto 98		!STRAIGHT ON TO equilibrium distributions
	endif
	if(.not.short) then
         print 68,(phi0(j),j=1,kF)
         if(discprt) write(8,68) (phi0(j),j=1,kF)
68	   format(' phiF(0) =',7(1x,f10.6))
	endif
      print 721
      if(discprt) write(8,721)
721	format(//,
     & ' CHANNEL SHUT AT t=0',/,
     & ' -------------------')
c
C DIVERSION TO DO N=0 CASE
	if(gap) then
         print 74
         if(discprt) write(8,74)
74	   format(/,' Latency to the first opening:')
	   print 72,0,(row1(1,j),j=1,kF)
         if(discprt) write(8,72) 0,(row1(1,j),j=1,kF)
	   call RAC2(row1,col1,amat,kF,w,w1,km,km,km)
	   call PDFOUTd('            ',-1,-1,w1,eigen,kF,am,sd,
     &    km,.false.,.true.,discprt)
         print 108		!new line
         if(discprt) write(8,108)
	   if(onlyfst) goto 98	!straight to equilib dist after 1st latency
	endif
C Multiply phiF(0) by GFA in Q3 for lags >0
	call MATMUL(row1,Q3,row1,1,kF,kA,ONE,1,km,km,km,1,km)
	do j=1,kA
	   row2(1,j)=row1(1,j)
	enddo
c
	goto 1030         !do nlag=1,..,nmax for 'shut at t=0' case
C
98	CONTINUE
C LASTLY TYPE EQUILIBRIUM VECTOR AND PDF FOR OPEN TIMES
c	if(noeq) then
c	else
	if(.not.noeq) then
	   do j=1,kA
		row1(1,j)=phi(1,j)
	   enddo
c For shut times postmult phio by GAF (in Q4)
	   if(GAP) call MATMUL(ROW1,Q4,ROW1,1,KA,KF,ONE,1,km,km,km,1,km)
	   print 73,(row1(1,j),j=1,kX)
         if(discprt) write(8,73) (row1(1,j),j=1,kX)
73	   format(/,' Equilibrium distribution:',/,
     & ' Initial vector=',7(1x,f10.6))
	   call RAC2(row1,col1,amat,kX,w,w1,km,km,km)
	   call PDFOUTd('            ',-2,-1,w1,eigen,kX,am,sd,
     &    km,.false.,.true.,discprt)
	endif
      print 108		!new line
      if(discprt) write(8,108)
	if(GAP) goto 135	! go on to BURSTS
C
C
C SHUT TIMES NEXT
	itype=1           !Start with 'open at t=0'
	open=.false.
	kX=kF
C Put the final vector= -QFF*UF in COL1
	call SUBMAT(QD,FF,Q2,km,km,km,km)		!QFF IN Q2
	call MATMUL(Q2,UCOL,COL1,KF,KF,1,-ONE,km,km,km,1,km,1)
C Expand QFF
	call QMAT5(Q2,Amat,kF,eigen,IBAD,km,km,km)
      print 134
      if(discprt) write(8,134)
134	format(/,/,/,
     & '     SHUT TIME PDFS',/,
     & '    ================')
	if(.not.short) then
	   if(debug()) then
		print 703
703		format(' Print spectral expansion matrices for QFF [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		if(UC(ans).ne.'N') then
		   do 704 m=1,kF
704		   call ATYPD3(amat,m,'Amat-QFF',kF,kF,km,km,km)
		   do 705 i=1,kF
		   do 705 j=1,kF
		   Q2(i,j)=zero
		   do 705 m=1,kF
705		   Q2(i,j)=Q2(i,j)+amat(i,j,m)*eigen(m)
		   call ATYPD(Q2,'SumA*eig',kF,kF,km,km)
		endif
	   endif
         print 66,(col1(i,1),i=1,kF)
         if(discprt) write(8,66) (col1(i,1),i=1,kF)
66	   format(' End vector -QFF*uF =',7(1x,f10.6))
	endif
	goto 133		!return to 133 to do shut time pdfs
C
C
C BURST DISTRIBUTIONS.  DO EACH SEP. REPEAT 3 TIMES FOR:
C ITYPE=1 FOR OPEN AT t=0
C ITYPE=2 FOR SHUT AT t=0
C ITYPE=3 FOR IN C AT t=0
C NEED EQUILIB PHI-BURST,ZAA,PHI(A,0),PHI(F,0) AS ABOVE + PHI(C,0)
C AND FOR SHUT AT t=0 NEED GFA, GC(B)A
C FIRST CALCULATE PHI-BURST
C FIRST CALC START AND END VECTORS FOR BURST- NEEDED SEVERAL TIMES LATER
C   (1)INITIAL (1 X KA) VECTOR=PHI(1,J) SAY. USE GENERAL FORM
C	=PHI=PC(INF)(QCB*GBA+QCA)/SUM OVER A OF DITTO
c
135	CONTINUE
	print 82
	if(discprt) write(8,82)
82	FORMAT(/,/,
     & '      BURST DISTRIBUTIONS',/,
     & ' *****************************',/)
c
	if(QCCbad) then
         print 77
         if(discprt) write(8,77)
77	   format(/,
     & ' QCC is singular, so can calculate only',/,
     & ' (1) within-burst distributions for n=1 when ''open at t=0''',/,
     & ' (2) For ''shut'' or ''in C'' at t=0 can get only the gap',/,
     & '     distributions for n=0 (1st latency)')
	endif
c
	noeq=.false.
      call GMAT1(BA,QD,GBA,km,km)	 !GBA in GBA
	if(.not.short)call ATYPD(GBA,' GBA    ',kB,kA,km,km)
  	if(dabs(det).lt.1.e-10) then
	   noeq=.true.
         print 912,det
         if(discprt) write(8,912) det
912	   format(/,
     & ' Cannot calc equilibrium initial vector for burst, PHIb,',/,
     & '  (determinant of QBB = ',g15.8,')')
	   goto 913		!skip rest of calc
	endif
	call SUBMAT(QD,CB,Q1,km,km,km,km)		!QCB IN Q1
	call MATMUL(Q1,GBA,Q2,KC,KB,KA,ONE,km,km,km,km,km,km) !QCB*GBA IN Q2
	call SUBMAT(QD,CA,Q1,km,km,km,km)   !QCA IN Q1
	do i=1,kC
	   do j=1,kA
		Q1(i,j)=Q2(i,j)+Q1(i,j)		!QCB*GBA+QCA in Q1
	   enddo
	enddo
	SUM=ZERO
	do j=1,kA		!PREMULT BY PC(INF) TO GET PHI
	   PHI(1,j)=ZERO
	   do L=1,kC
		PHI(1,j)=PHI(1,j)+Peq(L+kE)*Q1(L,j)
	   enddo
	   SUM=SUM+PHI(1,j)
	enddo
c
	if(sum.ge.1.d-20) then
	   noeq=.false.		!equilib pdf CAN be calc
         print 910
         if(discprt) write(8,910)
910	   format(
     & ' Equilibrium initial vector for burst, PHIb =')
	   do j=1,kA
		PHI(1,j)=PHI(1,j)/SUM	!NORMALISE PHI
	   enddo
	   print 71,(PHI(1,j),j=1,kA)
         if(discprt) write(8,71)(PHI(1,j),j=1,kA)
	else
	   noeq=.true.		!no equilib pdf can be calc
         print 911,phi(1,j),sum
         if(discprt) write(8,911) phi(1,j),sum
911	   format(
     & ' Cannot calc equilibrium initial vector for burst, PHIb,',/,
     & '  (numerator, denominator = ',2g13.6,')')
	endif
913	continue
c
C  (2) CALC END VECTOR END1=(QAB*GBC+QAC)*UC
      call GMAT1(BC,QD,Q2,km,km)			!GBC in Q2
	call SUBMAT(QD,AB,Q1,km,km,km,km)		!QAB IN Q1
	call MATMUL(Q1,Q2,Q3,kA,kB,kC,ONE,km,km,km,km,km,km)	!QAB*GBC IN Q3
	call SUBMAT(QD,AC,Q1,km,km,km,km)			!QAC IN Q1
	do i=1,kA
	   do j=1,kC
		Q2(i,j)=Q3(i,j)+Q1(i,j)		!QAB*GBC+QAC IN Q2
	   enddo
	enddo

	call MATMUL(Q2,UCOL,END1,KA,KC,1,ONE,km,km,km,1,km,1)
      print 12
      if(discprt) write(8,12)
12	FORMAT( ' End vector (-QAA*Eb)=')
	print 71,(END1(I,1),I=1,KA)
      if(discprt) write(8,71)(END1(I,1),I=1,KA)
C
C CALC ZAA
C   CALC HAA=GAB*GBA
      call GMAT1(AB,QD,GAB,km,km)			!GAB in GAB
      call GMAT1(BA,QD,GBA,km,km)			!GBA in GBA
	call MATMUL(GAB,GBA,HAA,KA,KB,KA,ONE,km,km,km,km,km,km)	!HAA IN HAA
C
      call GMAT1(BC,QD,Q2,km,km)				!GBC in Q2
	call MATMUL(GAB,Q2,Q2,KA,KB,KC,ONE,km,km,km,km,km,km)	!GAB*GBC IN Q2
      call GMAT1(AC,QD,Q3,km,km)				!GAC in Q3
	if(.not.short) then
	   call ATYPD(GAB,' GAB    ',kA,kB,km,km)
	   call ATYPD(GBA,' GBA    ',kB,kA,km,km)
	   call ATYPD(HAA,'GAB*GBA ',kA,kA,km,km)
	   call ATYPD(GBC,' GBC    ',kB,kC,km,km)
	   call ATYPD(GAC,' GAC    ',kA,kC,km,km)
	endif
	do i=1,kA
	   do j=1,kC
		Q2(i,j)=Q2(i,j)+Q3(i,j)		!GAB*GBC+GAC IN Q2
	   enddo
	enddo
      print 727
      if(discprt) write(8,727)
727	FORMAT(/' GAB*GBC + GAC ')
	if(.not.short) call ATYPD(Q2,'        ',kA,kC,km,km)
c Calc eb=(GAB*GBC + GAC)*uC in end2, for use in SCJOFF
	call MATMUL(Q2,ucol,end2,kA,kC,1,one,km,km,km,1,km,1)
C
	do i=1,kA
	   do j=1,kA
		RAA(i,j)=-HAA(i,j)
		if(i.eq.j) RAA(i,j)=ONE-HAA(i,j)		!(I-HAA) IN RAA
	   enddo
	enddo
C INV(I-GAB*GBA) IN RAA
	call MATINV(RAA,kA,km,RAA,km)
C MULT FIRST TWO FACTORS IN ZAA- RESULT IN Q2=GA(B)C
	call MATMUL(RAA,Q2,Q2,KA,KA,KC,ONE,km,km,km,km,km,km)       !GA(B)C IN Q2
C
	if(QCCbad) goto 76
C NEXT NEED INV(I-GCB*GBC)
      call GMAT1(CB,QD,Q3,km,km)				!GCB in Q3
      call GMAT1(BC,QD,Q4,km,km)				!GBC in Q4
	call MATMUL(Q3,Q4,Q3,KC,KB,KC,ONE,km,km,km,km,km,km)	!GCB*GBC IN Q3
	do i=1,kC
	   do j=1,kC
		Q3(i,j)=-Q3(i,j)
		if(i.eq.j) Q3(i,j)=ONE+Q3(i,j)		!(I-GCB*GBC) IN Q3
	   enddo
	enddo
	call MATINV(Q3,kC,km,Q1,km)		!INVERSE IN Q1
      call GMAT1(CB,QD,Q3,km,km)				!GCB in Q3
	call MATMUL(Q3,GBA,Q3,KC,KB,KA,ONE,km,km,km,km,km,km) !GCB*GBA IN Q3
      call GMAT1(CA,QD,Q4,km,km)				!GCA in Q4
	do i=1,kC
	   do j=1,kA
		Q4(i,j)=Q3(i,j)+Q4(i,j)			!GCB*GBA+GCA IN Q4
	   enddo
	enddo
C GC(B)A=Q1*Q4 IN Q1
	call MATMUL(Q1,Q4,Q1,KC,KC,KA,ONE,km,km,km,km,km,km)
C COMPLETE CALC OF ZAA IN ZAA BY PREMULT OF THIS BY GA(B)C IN Q2
	call MATMUL(Q2,Q1,ZAA,KA,KC,KA,ONE,km,km,km,km,km,km)
	if(.not.short) then
         print 733
         if(discprt) write(8,733)
733	   format(/' GCB*GBA + GCA ')
	   call ATYPD(Q4,'GCB*GBA + GCA',kC,kA,km,km)
	   call ATYPD(Q2,' GA(B)C ',kA,kC,km,km)
	   call ATYPD(Q1,' GC(B)A ',kC,kA,km,km)
	   call ATYPD(ZAA,' ZAA    ',kA,kA,km,km)
	endif
76	continue	!jump here if QCC singular
c
      if(.not.QFFbad) call GMAT1(FA,QD,Q2,km,km)		!GFA in Q2
c
c****** NUMBER OF OPENINGS PER BURST **********************
c   FINAL VECTOR=(I-HAA)*UA IN COL1
	do i=1,kA
	   COL1(i,1)=ONE
	   do j=1,kA
		COL1(i,1)=COL1(i,1)-HAA(i,j)
	   enddo
	enddo
C SPEC EXP OF HAA
c	CALL QMAT3(HAA,EM,EN,KA,EIGHAA,IBAD,KAMAX,KAM,KAMAX)
	call QMAT5(HAA,Hmat,kA,EIGHAA,IBAD,km,km,km)
	IF(IBAD.NE.0) print 14,IBAD
C
C
C TYPE EIGENVALUES AND 'NORMALISED MEAN'
	print 200,nmax
	call INPUTi(nmax)
	if(nmax.le.0) goto 999		!finish
c
      print 321
      if(discprt) write(8,321)
321	FORMAT(//,
     & ' Number (r) of openings/burst ',/,
     & ' ============================ ',/,
     & '  m     lambda(m)     norm mean')
	do m=1,kA
	   amean(m)=sngl(one/(one-eighaa(m)))
	   print 323,m,EIGHAA(m),amean(m)
         if(discprt) write(8,323)m,EIGHAA(m),amean(m)
323	   format(i3,2x,2g13.6)
	enddo
C
	ITYPE=1
C CALC phi0()=PHI(A) AT t=0=REL OCCS OF A STATES- FOR ITYPE=1
	call INVEC(A,phi0,row1,P0,skip,km)	!subset 1=A
	print 70		!'OPEN AT t=0'
      if(discprt) write(8,70)
	if(skip) then
         print 83
         if(discprt) write(8,83)
c83	   format(/,' phiA(0) undefined: cannot calculate open times')
	   goto 142		!STRAIGHT ON TO itype=2 (shut at t=0)
	endif
	if(.not.short) then
         print 69,(phi0(j),j=1,kA)
         if(discprt) write(8,69) (phi0(j),j=1,kA)
c69	   format(' phiA(0) =',7(1x,f10.6))
	endif
C CALC PDFS
C
1031	CONTINUE	!RETURN HERE FOR SHUT AT t=0
C
	do nlag=1,nmax
C TYPE CURRENT INITIAL VECTOR
         print 108		!new line
         if(discprt) write(8,108)
	   print 72,NLAG,(ROW1(1,J),J=1,KA)
         if(discprt) write(8,72)NLAG,(ROW1(1,J),J=1,KA)
	   call RAC2(row1,col1,Hmat,kA,w,w1,km,km,km)
C TYPE PDF
c	   S=ZERO
	   do m=1,kA
		area(m)=sngl(w1(m)/(one-eighaa(m)))		!area
c		am1(m)=w1(m)/(one-eighaa(m))		!AREA
c		S=S+am1(m)/(one-eighaa(m))		!FOR MEAN
	   enddo
	   call PDrOUTs('  ',-1,-1,area,amean,kA,am,sd,
     &    km,.false.,.true.,discprt)
	   if(QCCbad) goto 144	!ZAA not defined so skip out after n=1
C MULTIPLY INIT VECTOR BY ZAA SO PHI*ZAA**N ACCUM IN ROW1
	   call MATMUL(row1,ZAA,row1,1,kA,kA,ONE,1,km,km,km,1,km)  !row1*ZAA in row1
	enddo
C
	GOTO(142,143,144) ITYPE
C
142	CONTINUE
	ITYPE=2
	call INVEC(F,phi0,row1,P0,skip,km)		!subset 5=F
	if(skip) then
         print 84
         if(discprt) write(8,84)
c84	   format(' phiF(0) undefined: cannot calculate shut times')
	   goto 143		!STRAIGHT ON TO 'in C at t=0'
	endif
	if(.not.short) then
         print 68,(phi0(j),j=1,kF)
         if(discprt) write(8,68) (phi0(j),j=1,kF)
c68	   format(' phiF(0) =',7(1x,f10.6))
	endif
	print 721			!'SHUT AT t=0'
      if(discprt) write(8,721)
C POSTMULT BY GFA (IN Q2)
	call MATMUL(ROW1,Q2,ROW1,1,KF,KA,ONE,1,km,km,km,1,km)
	goto 1031
C
143	CONTINUE
	ITYPE=3
	call INVEC(C,phi0,row1,P0,skip,km)		!subset 3=C
	if(skip) then
         print 841
         if(discprt) write(8,841)
841	   format(' phiC(0) undefined')
	   goto 144		!STRAIGHT ON TO equilib dist
	endif
	if(.not.short) then
         print 681,(phi0(j),j=1,kC)
         if(discprt) write(8,681) (phi0(j),j=1,kC)
681	   format(' phiC(0) =',7(1x,f10.6))
	endif
      print 722
      if(discprt) write(8,722)
722	FORMAT(//,
     & ' CHANNEL IN C at t=0',/,
     & ' -------------------')
c	print 72,999,(ROW1(1,J),J=1,KC)
c      if(discprt) write(8,72)999,(ROW1(1,J),J=1,KC)
C POSTMULT BY GC(B)A (IN Q1)
	call MATMUL(ROW1,Q1,ROW1,1,KC,KA,ONE,1,km,km,km,1,km)
	goto 1031
C
144	CONTINUE
C FINALLY EQUILIB DIST
	if(.not.noeq) then
	   print 73,(phi(1,j),j=1,kA)
         if(discprt) write(8,73) (phi(1,j),j=1,kA)
	   call RAC2(phi,col1,hmat,kA,w,w1,km,km,km)
C TYPE PDF
c	   S=ZERO
	   do m=1,kA
		area(m)=sngl(w1(m)/(one-eighaa(m)))		!area
	   enddo
	   call PDrOUTs('  ',-1,-1,area,amean,kA,am,sd,
     &    km,.false.,.true.,discprt)
	endif
C
C
C**** TOTAL OPEN PER BURST
      print 145
      if(discprt) write(8,145)
145	FORMAT(//' TOTAL OPEN TIME PER BURST ',/,
     &         ' =========================')
C Calc VAA
	do i=1,kA
	   do j=1,kA
		Q4(I,J)=-HAA(I,J)
		if(i.eq.j) Q4(i,j)=one-HAA(i,j)		!(I-HAA) in Q4
	   enddo
	enddo
	call SUBMAT(QD,AA,Q3,km,km,km,km)		!QAA IN Q3
	call MATMUL(Q3,Q4,Q3,KA,KA,KA,ONE,km,km,km,km,km,km)		!VAA IN Q3
C   Final vector=-VAA*uA in COL1
	call MATMUL(Q3,UCOL,COL1,KA,KA,1,-ONE,km,km,km,1,km,1)
C Spec exp of VAA
	call QMAT5(Q3,Amat,kA,eigen,IBAD,km,km,km)
	IF(IBAD.NE.0) print 14,IBAD
C TYPE EIGENVALUES
      print 13
      if(discprt) write(8,13)
	do m=1,kA
	   rate(m)=sngl(eigen(m))
	   if(dabs(eigen(m)).ge.crit) then
      	print 7,m,rate(m),-1.0e3/rate(m)
      	if(discprt) write(8,7) m,rate(m),-1.0e3/rate(m)
	   else
      	print 8,m,rate(m)
      	if(discprt) write(8,8) m,rate(m)
	   endif
	enddo
C
	ITYPE=1
C CALC phi0()=PHI(A) AT t=0=REL OCCS OF A STATES- FOR ITYPE=1
	call INVEC(A,phi0,row1,P0,skip,km)	!subset 1=A
	print 70		!'OPEN AT t=0'
      if(discprt) write(8,70)
	if(skip) then
         print 83
         if(discprt) write(8,83)
c83	   format(/,' phiA(0) undefined: cannot calculate open times')
	   goto 147		!STRAIGHT ON TO SHUT AT t=0
	endif
	if(.not.short) then
         print 69,(phi0(j),j=1,kA)
         if(discprt) write(8,69) (phi0(j),j=1,kA)
c69	   format(' phiA(0) =',7(1x,f10.6))
	endif
C CALC PDFS
C
1032	continue	!RETURN HERE FOR SHUT AT t=0
C
	do nlag=1,nmax
C TYPE CURRENT INITIAL VECTOR
	   print 72,NLAG,(ROW1(1,J),J=1,KA)
         if(discprt) write(8,72)NLAG,(ROW1(1,J),J=1,KA)
	   call RAC2(row1,col1,amat,kA,w,w1,km,km,km)
C TYPE PDF
	   call PDFOUTd('            ',-1,-1,w1,eigen,kA,am,sd,
     &    km,.false.,.true.,discprt)
         print 108		!new line
         if(discprt) write(8,108)
	   if(QCCbad) goto 149	!ZAA not defined so skip out after n=1
c NB If QCC and QFF both singular then skip to next distn type because can
c do only n=1 for 'open at t=0' for ALL types of distribution. But can do
c nothing for 'shut at t=0' or 'in C at t=0' except, in the case of gap pdf
c only, can do n=0 (1st latency). Note that skipped here if QCC is singular
c (not tested here whether QFF singular too)
C MULTIPLY INIT VECTOR BY ZAA SO PHI*ZAA**N ACCUM IN ROW1
	   call MATMUL(ROW1,ZAA,ROW1,1,KA,KA,ONE,1,km,km,km,1,km)   !row1*ZAA in row1
	enddo
C
	goto(147,148,149) itype
C
147	continue
	ITYPE=2
	call INVEC(F,phi0,row1,P0,skip,km)	!subset 5=F
	if(skip) then
         print 84
         if(discprt) write(8,84)
c84	   format(' phiF(0) undefined: cannot calculate shut times')
	   goto 148		!STRAIGHT ON TO 'in C at t=0'
	endif
	if(.not.short) then
         print 68,(phi0(j),j=1,kF)
         if(discprt) write(8,68) (phi0(j),j=1,kF)
c68	   format(' phiF(0) =',7(1x,f10.6))
	endif
	print 721	!'SHUT AT t=0'
      if(discprt) write(8,721)
C POSTMULT BY GFA (IN Q2)
	call MATMUL(ROW1,Q2,ROW1,1,KF,KA,ONE,1,km,km,km,1,km)
	goto 1032
C
148	continue
	ITYPE=3
	call INVEC(C,phi0,row1,P0,skip,km)	!subset 3=C
	if(skip) then
         print 841
         if(discprt) write(8,841)
c841	   format(' phiC(0) undefined')
	   goto 149		!STRAIGHT ON TO equilib dist
	endif
	if(.not.short) then
         print 681,(phi0(j),j=1,kC)
         if(discprt) write(8,681) (phi0(j),j=1,kC)
c681	   format(' phiC(0) =',7(1x,f10.6))
	endif
	print 722	!' IN C AT t=0'
      if(discprt) write(8,722)
C POSTMULT BY GC(B)A (IN Q1)
	call MATMUL(ROW1,Q1,ROW1,1,KC,KA,ONE,1,km,km,km,1,km)
	goto 1032
C
149	continue
C FINALLY EQUILIB DIST
	if(.not.noeq) then
	   print 73,(phi(1,j),j=1,kA)
         if(discprt) write(8,73) (phi(1,j),j=1,kA)
	   call RAC2(phi,col1,amat,kA,w,w1,km,km,km)
	   call PDFOUTd('           f',-2,-1,w1,eigen,kA,am,sd,
     &    km,.false.,.true.,discprt)
	endif
      print 108		!new line
      if(discprt) write(8,108)
C END OF OPEN TIME PER BURST
C
C**** TOTAL BURST LENGTH
      print 150
      if(discprt) write(8,150)
150	FORMAT(//,
     & ' TOTAL BURST LENGTH',/,
     & ' ==================')
C=C END VECTOR IS (-QAA)*END1 (CALC ABOVE)- IN COL1
C=	call SUBMAT(QD,AA,Q3,km,km,km,km)		!QAA IN Q3
C=	call MATMUL(Q3,END1,COL1,KA,KA,1,-ONE,km,km,km,1,km,1)
C Spectral expansion of QEE
	call SUBMAT(QD,EE,Q3,km,km,km,km)	!QEE IN Q3
C	CALL QMAT3(Q3,EM,EN,KE,EIGEN,IBAD,KAM,KAM,KAM)
	call QMAT5(Q3,Amat,kE,eigen,IBAD,km,km,km)
	IF(IBAD.NE.0) print 14,IBAD
C TYPE EIGENVALUES
      print 13
      if(discprt) write(8,13)
	do m=1,kE
	   rate(m)=sngl(eigen(m))
	   if(dabs(eigen(m)).ge.crit) then
	      print 7,m,rate(m),-1.0e3/rate(m)
      	if(discprt) write(8,7) m,rate(m),-1.0e3/rate(m)
	   else
      	print 8,m,rate(m)
	      if(discprt) write(8,8) m,rate(m)
	   endif
	enddo
C
	ITYPE=1
C CALC phi0()=PHI(A) AT t=0=REL OCCS OF A STATES- FOR ITYPE=1
	call INVEC(A,phi0,row1,P0,skip,km)	!subset 1=A
	print 70		!'OPEN AT t=0'
      if(discprt) write(8,70)
	if(skip) then
         print 83
         if(discprt) write(8,83)
c83	   format(/,' phiA(0) undefined: cannot calculate open times')
	   goto 152		!STRAIGHT ON TO itype=2 (shut at t=0)
	endif
C	if(.not.short) then
         print 69,(phi0(j),j=1,kA)
         if(discprt) write(8,69) (phi0(j),j=1,kA)
c69	   format(/,' phiA(0) = pA(0)/pA(0)uA =',7(1x,f10.6))
C	endif
C CALC PDFS
C
1033	CONTINUE	!RETURN HERE FOR SHUT AT t=0
C
	do nlag=1,nmax
C TYPE CURRENT INITIAL VECTOR
	   print 72,NLAG,(ROW1(1,J),J=1,KA)
         if(discprt) write(8,72)NLAG,(ROW1(1,J),J=1,KA)
C USE KA,NOT KE, IN RAC AS ONLY AA SECTION USED!!
	   call RAC3(row1,col1,amat,1,kA,1,kA,kE,w,w1,km,km,km)
C TYPE PDF
	   call PDFOUTd('            ',-1,-1,w1,eigen,kE,am,sd,
     &    km,.false.,.true.,discprt)
         print 108		!new line
         if(discprt) write(8,108)
	   if(QCCbad) goto 154	!ZAA not defined so skip out after n=1
c NB If QCC and QFF both singular then skip to next distn type because can
c do only n=1 for 'open at t=0' for ALL types of distribution. But can do
c nothing for 'shut at t=0' or 'in C at t=0' except, in the case of gap pdf
c only, can do n=0 (1st latency). Note that skipped here if QCC is singular
c (not tested here whether QFF singular too)
C MULTIPLY INIT VECTOR BY ZAA SO PHI*ZAA**N ACCUM IN ROW1
	   call MATMUL(ROW1,ZAA,ROW1,1,KA,KA,ONE,1,km,km,km,1,km)  !row1*ZAA in row1
	enddo
C
	GOTO(152,153,154) ITYPE
C
152	CONTINUE
	ITYPE=2
	call INVEC(F,phi0,row1,P0,skip,km)	!subset 5=F
	if(skip) then
         print 84
         if(discprt) write(8,84)
c84	   format(' phiF(0) undefined: cannot calculate shut times')
	   goto 153		!STRAIGHT ON TO 'in C at t=0'
	endif
	if(.not.short) then
         print 68,(phi0(j),j=1,kF)
         if(discprt) write(8,68) (phi0(j),j=1,kF)
c68	   format(' phiF(0) =',7(1x,f10.6))
	endif
	print 721	!'SHUT AT t=0'
      if(discprt) write(8,721)
C POSTMULT BY GFA (IN Q2)
	call MATMUL(ROW1,Q2,ROW1,1,KF,KA,ONE,1,km,km,km,1,km)
	goto 1033
C
153	CONTINUE
	ITYPE=3
	call INVEC(C,phi0,row1,P0,skip,km)	!subset 3=C
	if(skip) then
         print 841
         if(discprt) write(8,841)
c841	   format(' phiC(0) undefined')
	   goto 154		!STRAIGHT ON TO equilib dist
	endif
	if(.not.short) then
         print 681,(phi0(j),j=1,kC)
         if(discprt) write(8,681) (phi0(j),j=1,kC)
c681	   format(' phiC(0) =',7(1x,f10.6))
	endif
	print 722	!' IN C AT t=0'
      if(discprt) write(8,722)
C POSTMULT BY GC(B)A (IN Q1)
	call MATMUL(ROW1,Q1,ROW1,1,KC,KA,ONE,1,km,km,km,1,km)
	goto 1033
C
154	CONTINUE
C FINALLY EQUILIB DIST
	if(.not.noeq) then
	   print 73,(phi(1,j),j=1,kA)
         if(discprt) write(8,73) (phi(1,j),j=1,kA)
	   call RAC3(phi,end1,amat,1,kA,1,kA,kE,w,w1,km,km,km)
	   call PDFOUTd('           f',-2,-1,w1,eigen,kE,am,sd,
     &    km,.false.,.true.,discprt)
	endif
      print 108		!new line
      if(discprt) write(8,108)
C END OF BURST LENGTH
C
C********* LASTLY GAP BETWEEN BURSTS *************
C
C QBA*uA in col1
	call SUBMAT(QD,BA,Q3,km,km,km,km)
	call MATMUL(Q3,ucol,col1,kB,kA,1,ONE,km,km,km,1,km,1)
C Expand QBB in Q3
	call SUBMAT(QD,BB,Q3,km,km,km,km)
	call QMAT5(Q3,Amat,kB,eigBB,IBAD,km,km,km)
C Make matrix with cols (#1 to #kB)=A(m)*col1
	do m=1,kB	!=COL #
	   do i=1,kB
		Q3(i,m)=ZERO
		do j=1,kB
		   Q3(i,m)=Q3(i,m) + AMAT(i,j,m)*col1(j,1)
		enddo
	   enddo
	enddo
C Premult each column by -inv(I-H)*GAB
	call MATMUL(GAB,Q3,Q3,kA,kB,kB,ONE,km,km,km,km,km,km)
	call MATMUL(RAA,Q3,Q3,kA,kA,kB,-ONE,km,km,km,km,km,km) !QBB part is neg
C
	if(QFFbad) goto 171	!do only n=0 for 'shut' or 'in C' at t=0
C Repeat in Q4 for QFF part
C QFA*uA in col1
	call SUBMAT(QD,FA,Q4,km,km,km,km)
	call MATMUL(Q4,ucol,col1,kF,kA,1,ONE,km,km,km,1,km,1)
C Expand QFF in Q4
	call SUBMAT(QD,FF,Q4,km,km,km,km)
	call QMAT5(Q4,Amat,kF,eigen,IBAD,km,km,km)
C Copy eigenvalues of QBB into EIGEN(kF+1)...
	do i=1,kB
	   eigen(kF+i)=eigbb(i)
	enddo
C Make matrix with cols (#1 to #kF)=A(m)*col1
	do m=1,kF	!=COL #
	   do i=1,kF
		Q4(i,m)=ZERO
		do j=1,kF
		   Q4(i,m)=Q4(i,m) + AMAT(i,j,m)*col1(j,1)
		enddo
	   enddo
	enddo
C Premult each column by inv(I-H)*GAF
      call GMAT1(AF,QD,GAB,km,km)				!GAF in GAB
	call MATMUL(GAB,Q4,Q4,kA,kF,kF,ONE,km,km,km,km,km,km)
	call MATMUL(RAA,Q4,Q4,kA,kA,kF,ONE,km,km,km,km,km,km)
c
C Should now be able to calc w1(i) as PHI*Q4 (for QFF terms) and
C PHI*Q3 for QBB terms
C
      print 1451
      if(discprt) write(8,1451)
1451	FORMAT(//,
     & ' GAPS BETWEEN BURSTS ',/,
     & ' ===================')
	kX=kF+kB
C Type eigenvalues
      print 13
      if(discprt) write(8,13)
	do m=1,kX
	   rate(m)=sngl(eigen(m))
	   if(dabs(eigen(m)).ge.crit) then
	      print 7,m,rate(m),-1.0e3/rate(m)
      	if(discprt) write(8,7) m,rate(m),-1.0e3/rate(m)
	   else
      	print 8,m,rate(m)
	      if(discprt) write(8,8) m,rate(m)
	   endif
	enddo
c
	ITYPE=1 		!open at t=0
C Calc phi0()=PHI(A) at t=0, i.e.rel occs of A states- FOR ITYPE=1
	call INVEC(A,phi0,row1,P0,skip,km)	!subset 1=A
	print 70		!'OPEN AT t=0'
      if(discprt) write(8,70)
	if(skip) then
         print 83
         if(discprt) write(8,83)
c83	   format(/,' phiA(0) undefined: cannot calculate open times')
	   goto 171		!STRAIGHT ON TO itype=2 (shut at t=0)
	endif
	if(.not.short) then
         print 69,(phi0(j),j=1,kA)
         if(discprt) write(8,69) (phi0(j),j=1,kA)
c69	   format(' phiA(0) =',7(1x,f10.6))
	endif
C CALC PDFS
C
	EQUIL=.FALSE.
1034	CONTINUE	!RETURN HERE FOR SHUT AT t=0
C
	do nlag=1,nmax
C Type current initial vector
	   print 72,nlag,(row1(1,j),j=1,kA)
         if(discprt) write(8,72) nlag,(row1(1,j),j=1,kA)
178	   continue
C Calc first kF amplitudes
	   do m=1,kF
C   Set COL1= column #m of Q4
		do i=1,kA
		   COL1(i,1)=Q4(i,m)
		enddo
		call MATMUL(ROW1,COL1,SCALAR,1,kA,1,ONE,1,km,km,1,1,1)
		w1(m)=scalar(1,1)
	   enddo
C then last kB amplitudes
	   do m=1,kB
C   set col1= column #m of Q3
		do i=1,kA
		   COL1(i,1)=Q3(i,m)
		enddo
		call MATMUL(ROW1,COL1,SCALAR,1,KA,1,ONE,1,km,km,1,1,1)
		w1(kF+m)=scalar(1,1)
	   enddo
C TYPE PDF
	   call PDFOUTd('            ',-1,-1,w1,eigen,kX,am,sd,
     &    km,.false.,.true.,discprt)
         print 108		!new line
         if(discprt) write(8,108)
C MULTIPLY INIT VECTOR BY ZAA SO PHI*ZAA**N ACCUM IN ROW1
	   call MATMUL(ROW1,ZAA,ROW1,1,KA,KA,ONE,1,km,km,km,1,km)  !row1*ZAA in row1
	   if(itype.eq.3.and.nlag.eq.nmax.and.(.not.equil)) goto 175
	   GOTO 165
C INSERT EQUILIB DIST HERE AFTER LAST TIME ROUND LOOP
C REPLACE ROW1 BY PHI, AND GO ROUND LOOP ONCE MORE
175	   continue
	   if(.not.noeq) then
		print 73,(phi(1,j),j=1,kA)
      	if(discprt) write(8,73) (phi(1,j),j=1,kA)
		do j=1,kA
		   row1(1,j)=phi(1,j)
		enddo
	 	equil=.TRUE.		!SO FINISHES NEXT TIME
	      goto 178
	   endif
c
165	continue
	enddo          !End of nlag loop
c
c
	GOTO(171,172,173) ITYPE
C
171	CONTINUE
	ITYPE=2		!shut at t=0
	call INVEC(F,phi0,row1,P0,skip,km)		!subset 5=F
	if(skip) then
         print 84
         if(discprt) write(8,84)
c84	   format(' phiF(0) undefined: cannot calculate shut times')
	   goto 172		!STRAIGHT ON TO 'in C at t=0'
	endif
	if(.not.short) then
         print 68,(phi0(j),j=1,kF)
         if(discprt) write(8,68) (phi0(j),j=1,kF)
c68	   format(' phiF(0) =',7(1x,f10.6))
	endif
	print 721	!'SHUT AT t=0'
      if(discprt) write(8,721)
      print 174
      if(discprt) write(8,174)
174	FORMAT(//,
     &' Latency to start of first burst (n=0) as for 1st opening, above'
     & ,/)
	if(QFFbad) goto 172	!do only n=0 for 'shut' or 'in C' at t=0
C POSTMULT BY GFA (IN Q2)
	call MATMUL(ROW1,Q2,ROW1,1,KF,KA,ONE,1,km,km,km,1,km)
	GOTO 1034
C
172	CONTINUE
	ITYPE=3		!in C at t=0
	call INVEC(C,phi0,row1,P0,skip,km)		!subset 3=C
	if(skip) then
         print 841
         if(discprt) write(8,841)
c841	   format(' phiC(0) undefined')
	   goto 173		!STRAIGHT ON TO equilib dist
	endif
	if(.not.short) then
         print 681,(phi0(j),j=1,kC)
         if(discprt) write(8,681) (phi0(j),j=1,kC)
c681	   format(' phiC(0) =',7(1x,f10.6))
	endif
	print 722	!' IN C AT T=0'
      if(discprt) write(8,722)
C DIVERSION HERE FOR FIRST LATENCY (NOTE Q1-Q4 ALL IN USE!!)
C    F=PHI(C,0)*[PFF(T)]CF*QFA*UA
C    GFA*UA IN COL2
	print 72,0,(ROW1(1,J),J=1,KC)
      if(discprt) write(8,72)0,(ROW1(1,J),J=1,KC)
	call SUBMAT(QD,FA,GAB,km,km,km,km)		!QFA IN GAB
	call MATMUL(GAB,UCOL,COL2,KF,KA,1,ONE,km,km,km,1,km,1)
C    EXPAND QFF
	call SUBMAT(QD,FF,GAB,km,km,km,km)		!QFF IN GAB
	call QMAT5(GAB,Amat,kF,eigBB,IBAD,km,km,km)
C    FOR CF SUBSECTION OF PFF(T) USE LAST KC ROWS (#KB+1 TO #KF)
c	CALL RAC1(ROW1,COL2,EM,EN,KB+1,KF,1,KF,KF,W,W1,KAM,KAM,KAM)
	call RAC3(row1,col2,amat,kB+1,kF,1,kF,kF,w,w1,km,km,km)
C    TYPE PDF
	call PDFOUTd('            ',-1,-1,w1,eigbb,kF,am,sd,
     & km,.false.,.true.,discprt)
      print 108		!new line
      if(discprt) write(8,108)
C   END OF N=0 DIVERSION
	if(QCCbad) goto 173	!do only n=0 for 'shut' or 'in C' at t=0- DONE
C POSTMULT BY GC(B)A (IN Q1)
	call MATMUL(ROW1,Q1,ROW1,1,KC,KA,ONE,1,km,km,km,1,km)
	GOTO 1034
C
173	CONTINUE
C END OF GAP BETWEEN BURST
c
c
999	CONTINUE
c Now do the pdfs conditional on 'start in B at t=0' for case of jump to zero
c conc of agonist (for these C must contain all the absorbing shut states and
c B must contain all the non-absorbing shut states)
	if(QCCbad.or.QFFbad) then
	   call SCJOFF(QD,p0,HAA,eighaa,RAA,amat,hmat,end2,ucol,
     &	short,nmax,km)
	endif
c Write .INI
c iver= 101 is 5120 bytes
      OPEN(unit=19,file='SCJUMP.INI',status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=5120)
	imodold=imod0
c For .ini set nsc(i) temporarily negative to denote obeymr(i)=F
	do i=1,ncyc
	   if(.not.obeymr(i)) then
		nsc(i)=-iabs(nsc(i))
	   endif
	enddo
	iver=101
	write(19,rec=1) iver,irecq,nvdep,conc00,conc0,conc,
     & nmax,ncyc,ioptsav,cjump,calinit,t0,imodold,
     & (nsc(i),i=1,ncyc),
     & ((im(i,j),j=1,nsc(i)),i=1,ncyc),
     & ((jm(i,j),j=1,nsc(i)),i=1,ncyc),
     & qfilem
	CLOSE(unit=19)
	do i=1,ncyc	     !reset nsc
	   nsc(i)=iabs(nsc(i))
	enddo
c
	call ENDPRINT
	call NUMCLR()
C
	END
