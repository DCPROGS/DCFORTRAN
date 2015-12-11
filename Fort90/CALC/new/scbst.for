	program SCBST
c Lahey version for V5.n 02/22/94 06:51pm
c  SCBST is new version (preiously called SCBST1) -version which uses GETQD
c for more flexible definition of the Q matrix (as in SCALCS), so don't
c need to go back to QGEN every time something is to be changed.
c
c Modif 12/03/03 11:47am to display some of the distributions
c Keep area,tau and ncomp in areasav(i,j) etc for i=1,...,ncomp and
c j=1,..,ndtype so j=distribution type
c These can be allocated to max size needed -ncomp is biggest for either
c gap between bursts (ncomp=kF+kB), or for burst length (r>1 op/bst
c (ncomp=kE+kA).  Once model know size can be allocated
c Also allocate idtype() to specify conditional pdf
c
c Declare idtype(3,j) where
c idtype(1,j)=distribution type
c idtype(2,j)=condition variable 1 (=0 if unconditional)
c idtype(3,j)=condition variable 2 (=0 if unconditional)
c
c idtype(1,j)=1 for number of openings per burst
c 	idtype(1:3,j)=
c	1,i,0 for ops/burst conditional on start in open state #i (docond=T)
c idtype(1,j)=2 for pdf of open times
c	2,0,0 for unconditional open time
c	2,-1,0 for first opening in burst with 2 or more openings
c	2,i,0 for open time conditional on start in open state #i (docond=T)
c	2,0,k for kth open time in any burst (dok=T)
c	2,r,k for kth open time in burst with r openings (dokr=T)
c idtype(1,j)=3 for pdf of shut times
c	3,0,0 for all shut times
c	3,i,0 for shut time conditional on start in open state #i (docond=T)
c	3,0,k for kth shut time in any burst (dok=T)
c	3,r,k for kth shut time in burst with r openings (dokr=T)
c idtype(1,j)=4 for pdf of burst lengths
c	4,0,0 for all burst lengths
c	4,-1,0 for burst lengths for bursts with 2 or more openings
c	4,i,0 for burst lengths conditional on start in open state #i (docond=T)
c idtype(1,j)=5 for pdf of total open time per burst
c	5,0,0 for all total open/burst
c	5,i,0 for total open/bst conditional on start in open state #i (docond=T)
c idtype(1,j)=6 for pdf of total shut time per burst
c	6,0,0 for all total shut/burst
c	6,i,0 for total shut/bst conditional on start in open state #i (docond=T)
c idtype(1,j)=7 for pdf of gaps between bursts
c	7,0,0 for all gaps between bursts

c========
c
c Modif 11/23/01 11:00am for obeymr(50). Rather than add this to .ini,
c set elements of nsc(i) negative (for .ini ONLY) if obeymr(i)=false
c
c Major modif 01/18/01 02:35pm for new array sizes and new getqd/qmechs.dat
c
c Modif 06/17/99 11:12am so that micro rev need not be obeyed -obeymr=true
c should be same as before -in common/mr/obeymr which is in eqoccd, getqd
c
c Modified 01/03/98 06:53am to use tcrits (as in ekdist) rather than tcsub1
c to calculate tcrits in the last section
c
C *CALLS TO SUBMAT HAVE KA,KB,KC IN COMMON RATHER THAN BEING PARAMETERS
C
C PROG TO CALC BURST DISTNS ETC ACCORDING TO C&H III
C	A=SUBSET OF OPEN STATES
C	B=SUBSET OF INTRABURST GAP STATES
C	C=SUBSET OF INTER-BURST GAP STATES
C	E=(A,B)=BURST STATES (D IN OLD NOTES)
C	F=(B,C)=SHUT STATES
c
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c
	dimension AMAT(100,100,100),HMAT(100,100,100)
c	dimension EM(100,100),EN(100,100)
	dimension QM(100,100),QM1(100,100),QT(100,100)
	dimension Q1(100,100),Q2(100,100),Q3(100,100)	!TEMP ARRAYS
	dimension GAB(100,100),GBA(100,100),HAA(100,100),RAA(100,100)
	dimension ROW1(1,100),ROW2(1,100),COL1(100,1),COL2(100,1)
	dimension PHI(1,100),psi(1,100),END1(100,1),end2(100,1)
C =START AND END VECTORS
	dimension SCALAR(1,1)
	dimension W1(100),W(100),AMI(100)
	dimension EIGAA(100),EIGHAA(100),EIGBB(100),EIGEN(100)
	dimension PEQ(100),peq1(100),PR(100)
c======
	real*8 eigen1(100),w11(100)
	real*4 area1(100),tau1(100)
c======
c
c	REAL*4 W(10),RATE(10)
	real*4 tau(100),area(100),topen,tshut,tcrit,tcs,dtc,tce,fac
	real*4 ob,stb,otb,bl,tc1
	real*4 tbst,tobst,vhold,vkin
	real*4 amean(100),am,sd
	real*4 xa1,xb1,xs
c=	real*4 cA1,cB1
	real*4 taus,tauf
	real*4 amcrit,ambl,amtg,amtop,x1,x2,yout,eps,epsy
C    DECLARE INTEGER ARGUMENTS FOR SUBMAT
	INTEGER AA,AB,AC,BA,BB,BC,CB,CA,EE,AF,FA,FF,A,F
	LOGICAL OPEN,GAP,DOKR,DOK,DOCOND,SHORT,DONE,pon,debug,log,plotcur
	character*1 ans,UC
	character*11 cdate,ctime
	character*8 qname
	character*40 path
	logical caplock
	logical discprt
	real*4 tcvals(100,3)
c extras for disc read
	real*8 dgamma(100)
c For new version that uses qmechs.dat
	integer IX(100),JX(100)		!NB 'ix' is used by RANDOM
	real*4 conc(10)
	character*20 ligname(10)
	character*10 titlep(200)
	logical readini,noprint,present
	logical student,cluster
	COMMON/LIG/nlig,IL(100)
	COMMON/CPAR/NCDEP,IX,JX,X1
	common/mpar/ncyc,nsc(50),im(50,100),jm(50,100)
	integer im1(8,15),jm1(8,15)	!to read old scbst.ini
	real*4 hpar
	COMMON/VPAR/NVDEP,IV(100),JV(100),HPAR(100)
	integer IE(200),JE(200),IF(200),JF(200)
	real*4 EFAC(200)
	COMMON/EBLK/NEQ,IE,JE,IF,JF,EFAC
c
	character*40 mtitle1*40,filnam*32,prtport*4	!for WINPRINT
	common/dpp/filnam,prtport,ndisc,jcol,mtitle1 !for WINPRINT,ENDPRINT,DISCNUM
	common/dp/discprt
	common/KBLK/KA,KB,KC,KD
	common/tco/iop,amcrit,topen	!for tcopt
	common/user/student,cluster,iwindows
c
c	character*2 charmod(20,30)	!to print model
	character*2 charmod(25,40)	!to print model
	character*74 mtitle	!title for model
	common/model/imod0,charmod,jlast,ilast,mtitle,imodold
c To test pdfburst subroutine
	real*8 phib(1,100)
	real*8 endb1(100,1),endb2(100,1)
c for eqoccd, getqd
	character qfilem*40
	logical obeymr(50),automr(50)
	common/mr/obeymr,automr
	integer isetmr(50)
	common/mr1/isetmr
c For spanning tree
	logical useprim		!use spanning tree method
c For eqec50
	real*4 EC50,curmax,curinfs,concmax
	real*8 cfacd,vknd
	logical monot
c Commons for getqd, calcmr
	common/np1/npar
	integer irate(200),jrate(200)
	common/ir/irate,jrate
c
	character inifile*40
c  For getqd to enable read/write of models defined in Windows version
	character*3 snumw(100)		! state number
	integer*4 icolwin(100)		! colour for open/schut
	real*4 wposx(100),wposy(100)	! position
	integer*4 nwidwin,nhghtwin	!window dimensions
	common/winmod/snumw,icolwin,wposx,wposy,nwidwin,nhghtwin
c For parsname
	character path1*30,pname*8,suffix*3,ndev*2
	logical nopath
c To store area etc for display
	allocatable:: areasav,tausav,idtype,ncompsav
	real*4 areasav(:,:),tausav(:,:)
	integer*4 idtype(:,:),ncompsav(:)
c
	common/deb2/idebug2	!to control printing in checkmr
c
	EXTERNAL tcopt
C
C
	debug()=caplock()
c
C MISC DEFINITIONS
c4	format(g13.6)
	filnam='SCBST.PRT'
	idebug2=2	!print to screen and .prt
c	cluster=.false.
c	student=.false.
c	call MYPATH(path)
c	if(path(1:8).eq.'O:\CVFIT') cluster=.true.
	call WINPRINT	!print file control
      OPEN(unit=7,file=prtport,iostat=nerr)		!open printer
	print 1
	if(discprt) write(8,1)
1	FORMAT(' SCBST: Single channel burst pdfs etc ',/)
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 2,cdate,ctime(1:8),mtitle1
	if(discprt) write(8,2) cdate,ctime(1:8),mtitle1
2	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 3
3	format(
     & ' CAPS LOCK on for debugging')
c
c
	km=100		!dimension of all arrays=100 now
	kdim=100		!dimension of Q
c
c Check whether to use macro rev before calling GETQD
	ans='Y'
c
	idest=0
c106	continue

452	readini=.false.
	print 450
450	format(' Read defaults from .ini file on disc [Y] ? ')
	ans='Y'
	call INPUTa(ans)
      if(ans.eq.'Y') then
	   inifile='SCBST.INI'
	   call TITENT0(
     &    'Name for .ini file:',inifile,40,.false.)
	   INQUIRE(file=inifile,exist=present,flen=nlen,err=452)
	   if(.not.present.or.nlen.eq.0) then
		call BELL(1)
	 	print 451,inifile
451	 	format(' Cannot find ',a40)
	 	ans='N'
	 	goto 452
	   endif
	   if(discprt) write(8,111) inifile
111	   format(' Name of initialisation file: ',a40)
	   readini=.true.
	   if(nlen.eq.1024) then		!old.ini
      	OPEN(unit=17,file=inifile,status='UNKNOWN',
     &    	access='DIRECT',form='UNFORMATTED',recl=1024)
	      read(17,rec=1)irecq,nvdep,im1,jm1,
     &	 xA1,xB1,nmod,imodold
	      CLOSE(unit=17)
		conc(1)=xA1
		conc(2)=xB1
		do i=1,8
		   do j=1,15
			im(i,j)=im1(i,j)
			jm(i,j)=jm1(i,j)
		   enddo
		enddo
c   can't write back in new format until nsc(), ncyc know -write on exit
	   else
	     	OPEN(unit=16,file=inifile,status='UNKNOWN',
     &    	access='DIRECT',form='UNFORMATTED',recl=10240)
c==		read(16,rec=1)irecq,nvdep,conc,imodold,ncyc,
		read(16,rec=1)iver,irecq,nvdep,conc,imodold,ncyc,
     &	(nsc(i),i=1,ncyc),
     &	((im(i,j),j=1,nsc(i)),i=1,ncyc),
     &	((jm(i,j),j=1,nsc(i)),i=1,ncyc),
     &	qfilem,useprim,(isetmr(i),i=1,ncyc)
		call PARSNAME(qfilem,path1,ndev,pname,suffix,nopath,40)
c		if(suffix.ne.'mec') qfilem='qmechs.mec'	!problem with upper/lower case
c		if(UC(qfilem(1:1)).ne.'Q') qfilem='qmechs.mec'
		CLOSE(unit=16)
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
	   endif
	endif

104	continue		!return here to start from beginning
105	continue
c Can't check conc here because have conc from .ini (if there is one)
c but not nlig -better not print ec50 etc until after leaving getqd and asking
c for conc
	call BELL(1)
c	if(nlig.ge.1) then
c	   do i=1,nlig
c		xs=1.e6*conc(i)
c		print 159,i,ligname(i),xs
c159		FORMAT(/,
c     &     ' Ligand #',i2,1x,a20,': concentration (muM) [',g13.6,'] = ')
c		call INPUTr(xs)
c		conc(i)=xs*1.0e-6
c	   enddo
c	endif
	kflag=1
c npar, irate, jrate now in commons
	call GETQD(QT,nchan,dgamma,vkin,vhold,vref,
     &  titlep,ligname,iflag,iprint,readini,irecq,qfilem,
     &  useprim,kflag,idest)
c	call GETQD(QT,irate,jrate,nchan,dgamma,vkin,vhold,vref,
c     &  npar,titlep,ligname,iflag,iprint,readini,irecq,qfilem,
c     &  kflag,idest)
c
	k=kA+kB+kC+kD
c
35	if(nlig.ge.1) then
	   do i=1,nlig
		xs=1.e6*conc(i)
		print 159,i,ligname(i),xs
159		FORMAT(/,
     &     ' Ligand #',i2,1x,a20,': concentration (muM) [',g13.6,'] = ')
		call INPUTr(xs)
		conc(i)=xs*1.0e-6
	   enddo
	endif
	V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	call QSETD(conc,IL,V1,QT,QM,k,.false.)
c=	call QSETC(QT,conc,QM,ncdep,nlig,IL,IX,JX,k,km)		!sets conc only
	call EQOCCd(QM,k,k-1,km,Peq)		!CALC EQUILIB OCCS
c  Copy QM to QM1, and peq to peq1, in case original needed again
	do i=1,k
	   peq1(i)=peq(i)
	   do  j=1,k
		QM1(i,j)=QM(i,j)
	   enddo
	enddo
c
	call ATYPD(QM,'  Q     ' ,k,k,km,km)
      print 154
      if(discprt) write(8,154)
154	format(/
     & ' Equilibrium occupancies = ')
	call VTYPD(peq1,'       ',k,km)
c Insert here the calculation of EC50s (as in GETQD but conc of all ligands
c defined here
	vknd=dble(vkin*float(nchan))
	vknd=DROUND(vknd,0)	!exact double precision as long as vkin is integer
	cfacd=vknd*1.d-3*1.d12
	do i=1,nlig
	   nlvar=i
	   print 145,nlvar,ligname(nlvar)
         if(discprt) write(8,145) nlvar,ligname(nlvar)
145	   format(/,
     &  ' Equilibrium conc-response curve for ligand # ',i2,' = ',a10)
	   if(nlig.gt.1) then
		do j=1,nlig
		   if(j.ne.nlvar) then
			print 146,j,ligname(j),conc(j)*1.e6
			if(discprt) write(8,146) j,ligname(j),conc(j)*1.e6
146			format('  (conc of ligand #',i2,',  ',a10,' = ',g13.6,
     &			' micromolar)')
		   endif
		enddo
	   endif
	   plotcur=.false. 	!so returns Popen, not current
	   call EQEC50(EC50,curinfs,monot,curmax,concmax,cur0,pop0,QT,
     &   conc,nlvar,vhold,vref,cfacd,dgamma,plotcur,k,kdim)
	   print 593,cur0,pop0
	   if(discprt) write(8,593) cur0,pop0
593	   format(' At zero concentration, current (pA) = ',g12.5,
     &	', Popen = ',g12.5)
	   if(monot) then
		print 591,curinfs,ligname(nlvar)(1:10),ec50*1.e6
      	if(discprt) write(8,591) curinfs,ligname(nlvar)(1:10),
     &		ec50*1.e6
c     & ' Maximum response (pA) = ',g11.4,/, !plotcur now falso, so max Popne
591		format(
     & ' Equilibrium response-concentration curve is monotonic',/,
     & ' Maximum P(open) = ',g11.4,/,
     & '    Conc of ',a10,' for 50% of this maximum (EC50) (muM) = ',
     &     g11.4,/,
     &   ' --------------------------------------------------')
c		ec50eq=ec50*1.e6	!micromolar
	   else	!not monotonic (never true for binding curve)
      	print 592,curmax,concmax*1.e6,
     &		ligname(nlvar)(1:10),ec50*1.e6,curinfs
      	if(discprt) write(8,592) curmax,concmax*1.e6,
     &		ligname(nlvar)(1:10),ec50*1.e6,curinfs
592		format(
     &    ' Equilibrium response-concentration curve has maximum.',/,
     &    '   Max equilib response = ',g12.5,' pA at ',g12.5,' muM',/,
     &    '   Conc of ',a10,
     &    ' for 50% of this max. current (muM) (left of max) = ',
     &        g12.5,/,
     &    '   Response at conc -> infinity = ',g12.5,' pA',/,
     &   ' --------------------------------------------------')
	   endif
	enddo
c
	ans='N'
	if(nlig.gt.0) then
	   call DCASK('Use a different concentration(s)',ans,ans)
	   if(UC(ans).eq.'Y') goto 35
	endif
c	   do i=1,nlig
c		xs=1.e6*conc(i)
c		print 153,i,ligname(i),xs
c153		FORMAT(/,
c     &    ' Ligand #',i2,1x,a20,': new concentration (micromolar) [',
c     &      g13.6,'] = ')
c	   enddo
c	   call INPUTr(xs)
c	   conc(i)=xs*1.e-6	!molar
c	   call QNEWC(QM1,cA1,cB1,QM,xA1,xB1,ncdep,nlig,IL,IX,JX,k,km)
c	   call EQOCCd(QM,k,k-1,km,Peq)
c	endif
	if(nlig.gt.0) then
	   do i=1,nlig
		xs=1.e6*conc(i)
		print 160,i,ligname(i),xs
		if(discprt) write(8,160) i,ligname(i),xs
160		FORMAT(/,
     &     ' Ligand #',i2,1x,a20,': concentration (muM) = ',g13.6)
	   enddo
	endif
c
      print 154
      if(discprt) write(8,154)
c154	format(/
c     & ' Equilibrium occupancies = ')
	call VTYPD(peq,'       ',k,km)
c
	print 108
c108	format('/')
	ans='N'
	call DCASK(
     &  'Print Q matrix for the similar uncoupled model',ans,ans)
	if(ans.eq.'Y') then
	   call QUNC1(QM,k,ibad,km)
	endif
c
	ONE=1.0D0
	ZERO=0.0D0
	CRIT=1.0D-10	!BELOW WHICH COEFFS SET TO ZERO
c
c	SHORT=.FALSE.
c	itcor=0
c	print 341
c341	FORMAT( ' Short version [Y] ? ')
c	ans='Y'
c	call INPUTa(ans)
c	if(UC(ans).ne.'N') SHORT=.TRUE.
c	IF(short) GOTO 902
	docond=.false.
	dok=.false.
	dokr=.false.
	itcor=0
	short=.false.
	iopt=1
	print 1201,iopt
1201	format(/,' CHOOSE CALCULATION TYPE',/,
     & ' (1) Unconditional distributions only',/,
     & ' (2) also distributions conditional on starting state',/,
     & ' (3) also f(k) (kth event in any burst)',/,
     & ' (4) also f(k,r) and f(k) (kth event in burst with r ops)',/,
     & ' (5) short version (for correlation calcs)',/,
     & ' (6) ultra-short version (ditto)',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(iopt)
	if(iopt.eq.1) then
	   short=.true.
	else if(iopt.eq.2) then
	   docond=.true.
	else if(iopt.eq.3) then
	   dok=.true.
	else if(iopt.eq.4) then
	   dok=.true.
	   dokr=.true.
	else if(iopt.eq.5) then
	   itcor=1
	else if(iopt.eq.6) then
	   itcor=2
	endif
902	continue
C IF ITCOR=0 ORDINARY VERSION
C IF ITCOR=1 SHORT VERSION FOR CORRELATION TEST WITH PRINT OF ARRAYS
C IF ITCOR=2 ULTRA SHORT (RANK AND EIGENVALUES ONLY)
c	print 1021,itcor
c      if(discprt) write(8,1021)itcor
c1021	format(' itcor= ',i4)
	call SLIFED2(QM,Peq,.false.,.false.,km,km) !screen only
	print 201,kA
201	FORMAT(/,'  number of open states, kA= ',i4)
	print 202,kB
202	FORMAT('  number of short shut states, kB= ',i4)
	print 203,kC
203	FORMAT('  number of long-lived shut states C, kC= ',i4)
	print 204,kD
204	FORMAT(
     &   '  number of very long (between cluster) shut states, kD= ',i4)
c	print 342,K,ka,kb,kc
c342	format(' k,kA,kB,kC= ',4i3)
	print 347
347	format(' (NB kD=0 for this program)',/,' O.K. [Y] ? ')
	ans='Y'
	call INPUTa(ans)
	if(UC(ans).ne.'N') goto 343
346	continue
	kD=0
	print 344
344	format(' kA,kB,kC = ')
	read 345,ka,kb,kc
345	format(3i8)
	if(ka+kb+kc.ne.k) goto 346
	goto 902	!for approval
c print values
343	continue
	if(discprt) then
	   write(8,201)kA
	   write(8,202)kB
	   write(8,203)kC
	endif
	call SLIFED2(QM,Peq,.false.,discprt,km,km) !print it
c	write(7,342)K,ka,kb,kc
c      if(discprt) write(8,342)K,ka,kb,kc
	ans='N'
	print 34,ans
34	FORMAT(' Type Q matrix [',a1,']? ')
	call INPUTa(ans)
	if(UC(ans).EQ.'Y') call ATYPd(QM,'Q Matrix',K,K,km,km)
C
C DEFINE AA,AB ETC AS ARGUMENTS FOR SUBMAT WHICH GETS A SUBMATRIX
C FROM QM. THEY ARE DECLARED AS INTEGERS AND VALUE IJ=ROW,COL- THEY
C ARE ARBITRARY INTEGERS INTERP BY SUBMAT TO GET REQ ROWS AND COLS
c	A=1
	F=5
	AA=11
	AB=12
	AC=13
	BA=21
	BB=22
	BC=23
	CA=31
	CB=32
	AF=15	!5=CODE FOR F(=B+C) SECTION
	FA=51
	FF=55
	EE=66	!6=CODE FOR E(=BURST=A+B)
	KE=KA+KB
	KF=KB+KC
	kD=0
	K=KA+KB+KC
c Calculate maximum number of components needed for any distribution
c so arrays tausav() etc can be allocated (to store values for display)
c These can be allocated to max size needed -ncomp is biggest for either
c gap between bursts (ncomp=kF+kB), or for burst length (r>1 op/bst
c (ncomp=kE+kA).  Once model know size can be allocated
	ndim=kF+kB
	if(kE+kA.gt.ndim) ndim=kE+kA
	ndtmax=300		!max number of distribution types that can be stored
	if(allocated(tausav)) DEALLOCATE(tausav,areasav,idtype,ncompsav)
	ALLOCATE(tausav(ndim,ndtmax),areasav(ndim,ndtmax),
     &   idtype(4,ndtmax),ncompsav(ndtmax))
	jsav=0	!index for stored values, jsav=1,...,nsav
C
C
c13	FORMAT( '     Eigenvalues             Tau (ms)')
c7	FORMAT(2X,I2,3X,G13.6,3X,F13.5)
C
c If itcor=0 print also initial vectors for open and shut times, PHIo and
c PHIo*GAF
	if(itcor.eq.0) then
	   call SUBVEC(Peq,F,Pr,km,km)                  !pF(inf) in Pr
         call SUBMAT(QM,FA,Q1,km,km,km,km)		!QFA in Q1
         call MATMUL(Pr,Q1,row1,1,kF,kA,one,1,km,km,km,1,km)  !pF(inf)*QFA in row1
	   sum=zero
	   do 84 j=1,kA
84	   sum=sum+row1(1,j)
	   do 85 j=1,kA
85	   row1(1,j)=row1(1,j)/sum
	   print 83
	   if(discprt) write(8,83)
83	   format( /,' Initial vector for openings (phi(o))=')
	   print 71,(row1(1,j),j=1,kA)
         if(discprt) write(8,71)(row1(1,j),j=1,kA)
         call GMAT1(AF,QM,Q1,km,km)	 !GAF in Q1
         call MATMUL(row1,Q1,row2,1,kA,kF,one,1,km,km,km,1,km)  !phi(o)*GAF
	   print 86
	   if(discprt) write(8,86)
86	   format(' Initial vector for shuttings (phi(s)=phi(o)*GAF) =')
	   print 71,(row2(1,j),j=1,kF)
         if(discprt) write(8,71)(row2(1,j),j=1,kF)
	endif
C FIRST CALC START AND END VECTORS- NEEDED SEVERAL TIMES LATER
C   (1)INITIAL (1 X KA) VECTOR=PHI(1,J) SAY. FIRST USE GENERAL FORM
C	=PHI=PC(INF)(QCB*GBA+QCA)/SUM OVER A OF DITTO
C	FIRST NEED GBA=-INV(QBB)*QBA
	print 8
	if(discprt) write(8,8)
8	FORMAT( /,' Initial vector for burst (phi(b))=')
      call SUBMAT(QM,BB,Q1,km,km,km,km)		!QBB in Q1
	call MATINV(Q1,kB,km,Q3,km)			!inv(QBB) in Q3
	call SUBMAT(QM,BA,Q2,km,km,km,km)		!QBA in Q2
	call MATMUL(Q3,Q2,GBA,kB,kB,kA,-one,
     & km,km,km,km,km,km)                      	!GBA in GBA
c
c===nmod=5????
c	if(nmod.ne.5) goto 4
cC ALTERNATIVE CALC OF PHI FOR CASES IN WHICH ONLY ONE BURST STATE
cC CAN BE REACHED FROM C, AND THAT STATE IS IN B. FOR KM2A THAT STATE
cC IS AR=4 SO WANT 2ND ROW OF GBA SCALED TO SUM TO UNITY. -GBA IS IN Q3.
c	SUM=ZERO
c	DO 5 J=1,KA
c5	SUM=SUM+GBA(2,J)	!FOR ISCGEN=1 =KM2A MODEL
c	do j=1,kA
c	   PHI(1,J)=GBA(2,J)/SUM
c	enddo
c	print 71,(PHI(1,J),J=1,KA)
c      if(discprt) write(8,71)(PHI(1,J),J=1,KA)
c71	FORMAT(2X,8G13.6)
c
4	CONTINUE
	call SUBMAT(QM,CB,Q1,km,km,km,km)		!QCB IN Q1
	call MATMUL(Q1,GBA,Q2,KC,KB,KA,ONE,km,km,km,km,km,km)	!QCB*GBA IN Q2
	call SUBMAT(QM,CA,Q1,km,km,km,km)		!QCA IN Q1
	do i=1,kC
	   do j=1,kA
		Q1(I,J)=Q2(I,J)+Q1(I,J)	!QCB*GBA+QCA IN Q1
	   enddo
	enddo
	SUM=ZERO
	do j=1,kA		!PREMULT BY PC(INF) TO GET PHI
	   PHI(1,J)=ZERO
	   do L=1,kC
		PHI(1,J)=PHI(1,J)+PEQ(L+KE)*Q1(L,J)
	   enddo
	   SUM=SUM+PHI(1,J)
	enddo
	do j=1,kA
	   PHI(1,J)=PHI(1,J)/SUM	!NORMALISE PHI
	enddo
	print 71,(PHI(1,J),J=1,KA)
      if(discprt) write(8,71)(PHI(1,J),J=1,KA)
71	FORMAT(2X,20G13.6)
C  (2) CALC END VECTOR END1=(QAB*GBC+QAC)*UC WHERE GBC=-INV(QBB)*QBC
C	AND INV(QBB) IS STILL IN Q3
	call SUBMAT(QM,BC,Q1,km,km,km,km)		!QBC IN Q1
	call MATMUL(Q3,Q1,Q2,KB,KB,KC,-ONE,km,km,km,km,km,km)	!GBC IN Q2
	call SUBMAT(QM,AB,Q1,km,km,km,km)			!QAB IN Q1
	call MATMUL(Q1,Q2,Q3,KA,KB,KC,ONE,km,km,km,km,km,km)	!QAB*GBC IN Q3
	call SUBMAT(QM,AC,Q1,km,km,km,km)			!QAC IN Q1
c	DO 9 I=1,KA
c	DO 9 J=1,KC
c9	Q2(I,J)=Q3(I,J)+Q1(I,J)		!QAB*GBC+QAC IN Q2
	do i=1,kA
	   do j=1,kC
		Q2(I,J)=Q3(I,J)+Q1(I,J)		!QAB*GBC+QAC IN Q2
	   enddo
	enddo
	do i=1,kA		!POSTMULT BY UC TO GET END1
	   END1(I,1)=ZERO
	   do j=1,kC
		END1(I,1)=END1(I,1)+Q2(I,J)
	   enddo
	enddo

C  NOW END2=GAB*GBC+GAC=-INV(QAA)*END1
	call MATINV(QM,KA,km,Q3,km)		!INV(QAA) IN Q3
	if(debug()) then
c	   print 701
c701      FORMAT(/ ' inv(QAA)=')
	   CALL ATYPD(Q3,'inv(QAA)',KA,KA,km,km)
	endif
	call MATMUL(Q3,END1,END2,KA,KA,1,-ONE,km,km,km,1,km,1)   !-INV(QAA)*END1
	IF(SHORT.OR.(ITCOR.GT.1)) GOTO 121
	print 12
	if(discprt) write(8,12)
12	FORMAT( ' END VECTORS (Eb,-QAA*Eb)=')
	print 71,(END1(I,1),I=1,KA)
      if(discprt) write(8,71)(END1(I,1),I=1,KA)
	print 71,(END2(I,1),I=1,KA)
      if(discprt) write(8,71)(END2(I,1),I=1,KA)
C CALC GAB=-INV(QAA)*QAB. INV(QAA) STILL IN Q3.
121	call SUBMAT(QM,AB,Q1,km,km,km,km)		!QAB IN Q1
	call MATMUL(Q3,Q1,GAB,KA,KA,KB,-ONE,km,km,km,km,km,km)	!GAB IN GAB
	IF(.not.(SHORT.OR.(ITCOR.GT.1))) then
	   call ATYPd(GAB,'  GAB   ',kA,kB,km,km)
	   call ATYPd(GBA,'  GBA   ',kB,kA,km,km)
	endif
C CALC HAA=GAB*GBA (N.B. NOT SAME AS Z' IN OLD THEORY WHICH WAS GBA*GAB)
	call MATMUL(GAB,GBA,HAA,KA,KB,KA,ONE,km,km,km,km,km,km)	!HAA IN HAA
C CALC RAA=INV(I-H)
	do i=1,kA
	   do j=1,kA
		RAA(I,J)=-HAA(I,J)
		IF(I.EQ.J) RAA(I,J)=ONE+RAA(I,J)	!(I-HAA) IN RAA
	   enddo
	enddo
c
	IF(.not.(SHORT.OR.(ITCOR.GT.1))) then
	   print 724
	   if(discprt) write(8,724)
724	   FORMAT(/' GAB*GBA= ')
	   call ATYPD(HAA,'GAB*GBA ',kA,kA,km,km)
	   print 702
	   if(discprt) write(8,702)
702	   FORMAT(/ ' (I-GAB*GBA) = ')
	   CALL ATYPD(RAA,'        ',KA,KA,km,km)
	endif
	call MATINV(RAA,KA,km,RAA,km)		!INV(I-HAA) IN RAA
	IF(.not.(SHORT.OR.(ITCOR.GT.1))) then
	   print 703
	   if(discprt) write(8,703)
703	   FORMAT(/ ' inv(I-GAB*GBA) ')
	   CALL ATYPD(RAA,'        ',kA,kA,km,km)
	endif
c
C END OF PRELIM CALCULATIONS
C
c	DOCOND=.FALSE.
c	IF(SHORT.OR.(ITCOR.GT.0).OR.KA.EQ.1) GOTO 3512
c	print 3511
c3511	FORMAT(
c     & ' Type distributions conditional on starting state in A [N] ? ')
c	ans='N'
c	call INPUTa(ans)
c	if(UC(ans).EQ.'Y') DOCOND=.TRUE.
c3512	CONTINUE
C
C DISTRIBUTION OF NO OF OPENINGS PER BURST P(R)=PHI*(HAA**(R-1))*END2
C   FIRST DO SPECTRAL EXPANSION OF HAA IN AMAT(I,J,M).
c	CALL QMAT3(HAA,EM,EN,KA,EIGHAA,IBAD,KAmax,KAM,KAmax)
	CALL QMAT5(HAA,Hmat,kA,EIGHAA,IBAD,km,km,km)
	if(debug()) then
	   call ATYPD(HAA,'GAB*GBA ' ,kA,kA,km,km)
	   print 700
700	   format(' Print spectral expansion matrices [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	   if(UC(ans).eq.'Y') then
		do m=1,kA
      	 print 706,m,eighaa(m)
      	 if(discprt) write(8,706) m,eighaa(m)
706		 format(' eigenvalue ',i3,' of GAB*GBA = ',g13.6)
		 call ATYPD3(Hmat,m,'A matrix',kA,kA,km,km,km)
		enddo
	   endif
	endif
	if(debug()) print 60,(eighaa(i),i=1,ka)
60	format(' eigenvalues= ',/,5g13.6,/,5g13.6)
	IF(IBAD.NE.0) print 14,IBAD
14	FORMAT( ' ***ERROR IN SPECTRAL EXPANSION. IBAD= ',I4)
c	DO 704 M=1,KA
c	DO 704 I=1,KA
c	DO 704 J=1,KA
c704	HMAT(I,J,M)=EM(I,M)*EN(M,J)
C
C NEXT CALC HMAT(M)*END2 (KA*1) I'TH ROW OF THIS IS COEFF
C OF LAMBDA(M)**R-1 IN THE DISTN COND ON START IN I'TH A STATE
C KEEP RESULT IN Q1(I,M) FOR THE KA EIGENVALUES (NOT MINUS
C EIGENVALUES!) OF HAA
	do m=1,kA
	   do i=1,kA
	    do j=1,kA
		Q2(I,J)=HMAT(I,J,M)		!AMAT(M) IN Q2
	    enddo
	   enddo
	   call MATMUL(Q2,END2,COL1,KA,KA,1,ONE,km,km,km,1,km,1)	!A(M)*END2 IN COL1
	   do i=1,kA
		Q1(I,M)=COL1(I,1)
	   enddo
	enddo		!end of m loop
C  NOW GET (A) AREA=W(M)/(1-LAMBDA)
C  AND (B) NORMALISED MEAN=MEAN IF LAMBDA(M) WAS THE ONLY LAMBDA
C  FIRST TYPE  THE KA CONDITIONAL DISTNS AND, ON LAST CYCLE,THE
C  UNCONDITIONAL DIST
	IKA=KA+1		!DO LAST CYCLE (OVERALL) ONLY
	IF(DOCOND) IKA=1

	DO 351 I=IKA,KA+1

	IF(I.LE.KA) print 3211,I		!I=1,2,..,KA =A STATE NUMBERS
      if(discprt.and.i.le.ka) write(8,3211)i
3211	FORMAT(/
     &' No (r) of openings/burst conditional on start in open state #',
     & i2)
c     & i2,/,'  m      W(m)         A(m)       Lambda(m)     Norm mean')

	IF(I.EQ.KA+1) print 321
	IF(discprt.and.I.EQ.KA+1) write(8,321)
c321	FORMAT(/' Number (r) of openings/burst (unconditional)',/,
c     & '  m      W(m)         A(m)       Lambda(m)     Norm mean')
321	FORMAT(/' Number (r) of openings/burst (unconditional)')
	DO 322 M=1,KA
	IF(I.EQ.KA+1) GOTO 352	!FOR UNCOND CASE
	W1(M)=Q1(I,M)		!COEFF IN CONDL CASE
	GOTO 353
C NOW WANT COEFF FOR UNCOND CASE. PREMULT BY INITIAL VECTOR IN PHI(1,J)
352	W1(M)=ZERO
	DO 354 L=1,KA
354	W1(M)=W1(M)+PHI(1,L)*Q1(L,M)
353	continue
	den=one-eighaa(m)
	amean(m)=sngl(one/den)
	area(m)=sngl(w1(m)/den)
c	A=W1(M)/(ONE-EIGHAA(M))		!AREA-COND AND UNCOND
c	B=ONE/(ONE-EIGHAA(M))
c	print 323,M,W1(M),A,EIGHAA(M),B
c      if(discprt) write(8,323)M,W1(M),A,EIGHAA(M),B
c323	FORMAT(I3,2X,4G13.6)
322	CONTINUE	!END OF M LOOP
	call PDrOUTs('  ',-1,-1,area,amean,kA,am,sd,
     & km,.false.,.true.,discprt)
	amob=am
c   store values for plotting
	jsav=jsav+1
	idtype(1,jsav)=1		!for ops/burst
	idtype(2,jsav)=i		!for conditional on start in state #i
	if(i.eq.kA+1) idtype(2,jsav)=0		!for unconditional
	idtype(3,jsav)=0		!for unconditional
	ncompsav(jsav)=kA		!number of components
	do i1=1,kA
	   areasav(i1,jsav)=area(i1)
	   tausav(i1,jsav)=amean(i1)
	enddo
c
	IF(ITCOR.EQ.2) GOTO 99
	IF(.NOT.SHORT) print 324
	if(.not.short.and.discprt) write(8,324)
324	FORMAT(/'  r      Prob(r)*100')
C
C NOW CALC PROB(IR OPENINGS/APP BURST) AND STORE IN PR(IR) (IR=1,2,...)
c	AM1=ZERO
	SUM=ZERO
	DO 317 IR=1,10
	J=IR
	R=FLOAT(IR)
C    CALC P(R) FROM SPECTRAL EXPANSION
	PR(J)=ZERO
	DO 318 M=1,KA
	IF(EIGHAA(M).LT.CRIT) GOTO 3181
C NEXT LINE IS 0**0 FOR P(1) WHEN LAMBDA=0. SHOULD BE INTERP AS 1 NOT 0!
	PR(J)=PR(J)+W1(M)*(EIGHAA(M)**(R-ONE))
	GOTO 318
3181	IF(IR.EQ.1) PR(J)=PR(J)+W1(M)	!IF LAMBDA=0
318	CONTINUE
c	SUM=SUM+PR(J)
c	AM1=AM1+R*PR(J)
	IF(IR.LE.10.AND.(.NOT.SHORT)) print 326,IR,100.*PR(J)
      if(discprt.and.IR.LE.10.AND.(.NOT.SHORT))write(8,326)IR,100.*PR(J)
326	FORMAT(I3,4X,G13.6)
317	CONTINUE	!END OF IR LOOP
C
C CALC MEANS
c	print 329,SUM,AM1
c     if(discprt) write(8,329)SUM,AM1
c329	FORMAT( /' SUM AND MEAN FROM FIRST 20 TERMS= ',2G13.6)
	AM2=ZERO
	DO 327 M=1,KA
327	AM2=AM2+W1(M)/((ONE-EIGHAA(M))**2)
	IF(I.LE.KA) AMI(I)=AM2	!KEEP MEANS FOR CONDITIONAL DISTS
C AM2=MEAN OF OVERALL DIST ON EXIT
	print 330,AM2
      if(discprt) write(8,330)AM2
330	FORMAT( /' mean = ',G13.6)
351	CONTINUE	!END OF I LOOP

C  CHECK BY CALC MEAN DIRECTLY FROM GENERAL EXPRESSION=PHI*RAA*UA
	DO 19 I=1,KA
	   COL1(I,1)=ZERO
	   DO 20 J=1,KA
20		COL1(I,1)=COL1(I,1)+RAA(I,J)	!RAA*UA IN COL1
19	CONTINUE
	do i=1,kA
	   COL1(I,1)=ZERO
	   do j=1,KA
		COL1(I,1)=COL1(I,1)+RAA(I,J)	!RAA*UA IN COL1
	   enddo
	enddo
	call MATMUL(PHI,COL1,SCALAR,1,KA,1,ONE,1,km,km,1,1,1)	!mean in SCALAR
	print 331,SCALAR(1,1)
      if(discprt) write(8,331) SCALAR(1,1)
331	FORMAT( ' Mean from direct matrix calc= ',G13.6,//)
	IF(ITCOR.EQ.1) GOTO 99
C
C
C OPEN TIME DISTRIBUTIONS AND GAP TIME DISTRIBUTIONS. THESE HAVE
C MUCH IN COMMON SO BEST CALC BY 2 PASSES THROUGH SAME LOOP WITH
C LOGICAL VARIABLES=OPEN,GAP (FIRST PASS FOR OPEN TIMES). FIRST DOUBLE
C PASS FOR PDF COND ON K,R (ITYPE=1).  THEN REPEAT THIS DOUBLE PASS FOR
C PDF THAT ARE CONDITIONAL ONLY ON K (ITYPE=2), AND
C AGAIN FOR OVERALL (ITYPE=3)
c	DOKR=.FALSE.
c	DOK=.FALSE.
c	IF(SHORT.OR.KA.EQ.1) GOTO 50	!PDF SAME FOR ALL K,R
c	print 51
c51	FORMAT(
c     & ' Type f(k,r) and f(k) (kth event in burst with r ops) [N] ? ')
c	ans='N'
c	call INPUTa(ans)
c	if(UC(ans).ne.'Y') GOTO 5201
c	DOKR=.TRUE.
c	DOK=.TRUE.
c	GOTO 50
c5201	print 52
c52	FORMAT(' Type f(k) (conditional on k only) [N] ? ')
c	ans='N'
c	call INPUTa(ans)
c	if(UC(ans).EQ.'Y') DOK=.TRUE.
c50	CONTINUE
C
	ITYPE=3			!DEFAULT=OVERALL  ONLY
	IF(DOKR) ITYPE=1	!TYPE F(K,R) AND F(K)
	IF(DOK.AND.(.NOT.DOKR)) ITYPE=2		!TYPE F(K) ONLY
	IKA=KA+1		!OVERALL ONLY FOR ITYPE=3
	IF(DOCOND) IKA=1	!COND ON IKA FOR ITYPE=3
	DONE=.FALSE.
	OPEN=.TRUE.
1000	GAP=.NOT.OPEN		!RETURN HERE FOR 2ND PASS
C   FIRST CALC PAA(T) OR PBB(T)
	IF(GAP) GOTO 32
	KX=KA			!FOR OPEN
	call SUBMAT(QM,AA,Q3,km,km,km,km)      	!QAA IN Q3
	CALL QMAT5(Q3,amat,kA,EIGAA,IBAD,km,km,km)
	if(debug()) then
	   call ATYPD(Q3,'  QAA   ' ,kA,kA,km,km)
	   print 700
c700	   format(' Print spectral expansion matrices [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	   if(UC(ans).eq.'Y') then
		do m=1,kA
		 call ATYPD3(amat,m,'A matrix',kA,kA,km,km,km)
		enddo
	   endif
	endif
	if(debug()) print 60,(eigaa(i),i=1,ka)
	IF(IBAD.NE.0) print 14,IBAD
C
C INSERT BIT HERE TO CALC PDF OF FIRST OPENING IN A BURST WITH
C TWO OR MORE OPENINGS
	IF(DONE) GOTO 3361
C   GET HAA*UA IN COL2()
	DO i=1,kA
	   COL2(I,1)=ZERO
	   do j=1,kA		!POSTMULT BY UA
		COL2(I,1)=COL2(I,1)+HAA(I,J)
	   enddo
	enddo
C   POSTMULT FOR PAA(T) IS (-QAA)*HAA*UA. GET IN COL1
	call MATMUL(Q3,COL2,COL1,KA,KA,1,-ONE,km,km,km,1,km,1)	!-QAA*HAA*UA IN COL1
C   GET DEN=PHI*HAA*UA IN SCALAR(1,1)
	call MATMUL(PHI,COL2,SCALAR,1,KA,1,ONE,1,km,km,1,1,1)	!DEN IN SCALAR
	if(dabs(scalar(1,1)).lt.1.e-20) goto 3361
	DO 3351 M=1,KA
	W1(M)=ZERO
c	RATE(M)=SNGL(EIGAA(M))
	do L=1,kA
	   do N=1,kA
		W1(M)=W1(M)+PHI(1,L)*Amat(L,N,M)*COL1(N,1)
	   enddo
	enddo
c3362	W1(M)=W1(M)+PHI(1,L)*EM(L,M)*EN(M,N)*COL1(N,1)
c note:	AMAT(I,J,M)=EM(I,M)*EN(M,J)
c
C   DIV BY NORMALISING FACTOR IN DEN
	if(dabs(scalar(1,1)).lt.1.e-20) goto 3361
	W1(M)=W1(M)/SCALAR(1,1)
	x=dabs(eigaa(m))
	tau(m)=1000./sngl(x)
	area(m)=sngl(w1(m)/x)
c	W(M)=SNGL(W1(M))		!END OF M LOOP
3351	continue
	print 262
	if(discprt) write(8,262)
262	FORMAT(/,
     & ' PDF of FIRST OPENING IN A BURST WITH 2 OR MORE OPENINGS')
	call PDFOUTs(' f(open;r>1)',-1,-1,area,tau,kA,am,sd,
     & km,.false.,.true.,discprt)
c   store values for plotting
	jsav=jsav+1
	idtype(1,jsav)=2		!for openings
	idtype(2,jsav)=-1		!for burst with 2 or more openings
	idtype(3,jsav)=0
	ncompsav(jsav)=kA		!number of components
	do i1=1,kA
	   areasav(i1,jsav)=area(i1)
	   tausav(i1,jsav)=tau(i1)
	enddo
c
c  Could insert another extra here -pdf of open time for bursts with
c  more than one opening -not yet done
c
	DONE=.TRUE.	!DO NOT REPEAT ON NEXT CYCLE!
	GOTO 3361
C
32	kX=kB			!FOR GAP
	call SUBMAT(QM,BB,Q3,km,km,km,km)   !QBB in Q3
	CALL QMAT5(Q3,Amat,KB,EIGBB,IBAD,km,km,km)
	if(debug()) then
	   call ATYPD(Q3,'  QBB   ' ,kB,kB,km,km)
	   print 700
c700	   format(' Print spectral expansion matrices [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	   if(UC(ans).eq.'Y') then
		do m=1,kB
		 call ATYPD3(amat,m,'A matrix',kB,kB,km,km,km)
		enddo
	   endif
	endif
	if(debug()) print 60,(eigbb(i),i=1,kb)
	IF(IBAD.NE.0) print 14,IBAD
3361	CONTINUE
C (1) IK'TH OPENING (OR GAP) IN BURST WITH IR OPENINGS.
C	NO OF OPENINGS IN BURST=IR  (IR=1,..,INF)
C	POSITION OF OPENING(IK)=1,2,...,IR
C	POSITION OF GAP (IK)=1,2,...,IR-1
C
C	IF(GAP) GOTO 25
C TITLES FOR OPEN
	IF(OPEN) IR0=1
	IF(GAP) IR0=2	!NEED 2 OPENINGS TO HAVE A GAP
	IF (OPEN) print 21
	if(discprt.and.open) write(8,21)
21	FORMAT( //'      OPEN TIME DISTRIBUTIONS',/)
C NOW TITLES FOR GAPS
C25	IR0=2		!NEED 2 OPENINGS TO HAVE A GAP
	IF(GAP) print 27
	IF(discprt.and.GAP) write(8,27)
27	FORMAT( //'      GAP WITHIN BURST DISTRIBUTIONS',/)
C ASK HOW MANY
	IF(ITYPE.GT.2) GOTO 39
	print 332
332	FORMAT(' Max number of openings/burst = ')
	call INPUTi(irmax)
39	CONTINUE
C
261	CONTINUE
	IF(ITYPE.EQ.3) IRMAX=1		!SO GET ONE LOOP ONLY
	IF(ITYPE.GE.2) IR0=IRMAX	!ONLY IK VARIES
C
c	print 13		!PRINT OUT EIGENVALUES
c	if(discprt) write(8,13)		!PRINT OUT EIGENVALUES
	DO 61 M=1,KX
	EIG=EIGAA(M)
	IF(GAP) EIG=EIGBB(M)
	IF(DABS(EIG).LT.1.E-9) GOTO 61
c	print 7,M,EIG,-1.0E3/EIG
c     if(discprt) write(8,7)M,EIG,-1.0E3/EIG
61	CONTINUE
C
	DO 333 IR=IR0,IRMAX
	DO 334 IK=1,IR
	ir1=ir		!to prevent warnings
	ik1=ik
C  FOR OPEN CASE IK=IR IS LAST OPENING,BUT FOR GAPS IK=IR-1 IS LAST GAP
C FIRST CALC PREMULTIPLIER FOR P(T)
	IF(ITYPE.EQ.3) GOTO 48
	IF(GAP.AND.IK.EQ.IR) GOTO 333  !FOR GAPS IK=IR-1 IS LAST GAP
	R=FLOAT(IR)
C FOR OPENINGS FO(K,R) NEED PAA(T)*(-QAA) IN MIDDLE
C FOR GAPS FG(K,R) NEED     GAB*PBB(T)*QBA IN MIDDLE
C   CALC HAA**(K-1) IN Q1, UNLESS ITYPE=3

	call MATPOW(HMAT,EIGHAA,IK-1,Q1,KA,km,km,km)     !HAA**(K-1) IN Q1
	call MATMUL(PHI,Q1,ROW1,1,KA,KA,ONE,1,km,km,km,1,km) !PHI*H**(K-1) IN ROW1(1,J)
	GOTO 49
C SO FAR COMMON TO ALL FOR ITYPE=1,2. NEXT BIT FOR ITYPE=3:
48	IF(IKA.LE.KA) GOTO 481		!TO CALC COND
	call MATMUL(PHI,RAA,ROW1,1,KA,KA,ONE,1,km,km,km,1,km)!PHI*INV(I-HAA) IN ROW1(1,J)
	DEN=AM2			!FOR OPEN
	IF(GAP) DEN=AM2-ONE	!=MEAN NO OF GAPS/BURST
	GOTO 40
481	DO 482 J=1,KA
482	ROW1(1,J)=RAA(IKA,J)	!IKA'TH ROW OF RAA FOR COND DIST
	DEN=AMI(IKA)	!MEAN OP/BST GIVEN START IN IKA'TH A STATE
	IF(GAP) DEN=DEN-ONE
	GOTO 40
C  DIVERSION TO CALC DEN FOR ITYPE=2
C  DEN=PHI*(HAA**N)*UA WHERE N=IK-1 FOR OPEN AND N=IK FOR GAPS
49	IF(ITYPE.NE.2) GOTO 40
C  PHI*H**(K) IN ROW2(1,J) FOR GAPS
	IF(GAP) call MATMUL(ROW1,HAA,ROW2,1,KA,KA,ONE,1,km,km,km,1,km)
	DEN=ZERO
	DO 41 J=1,KA		!POSTMULT BY UA
	IF(OPEN) DEN=DEN+ROW1(1,J)
41	IF(GAP) DEN=DEN+ROW2(1,J)
40	CONTINUE		!END OF DIVERSION
C
	IF(GAP) call MATMUL(ROW1,GAB,ROW1,1,KA,KB,ONE,1,km,km,km,1,km) !PHI*H**(K-1)*GAB IN ROW1(1,J)
C				!OR,FOR ITYPE=3,PHI*INV(I-HAA)*GAB
C NOW HAVE PREMULT FOR P(T) IN ROW1(1,J) FOR ITYPE=1,2 OR 3
C
C NEXT CALC POSTMULT FOR P(T) IN COL1. THIS IS SAME FOR ITYPE=2,3. BUT
C FIRST CALC FOR ITYPE=1:
	IF(ITYPE.NE.1) GOTO 42
C FIRST GET HAA**(R-K) IN Q1.
	call MATPOW(HMAT,EIGHAA,IR1-IK1,Q1,KA,km,km,km)     !HAA**(R-K) IN Q1
	IF(GAP) GOTO 30
C  PREMULT BY -QAA
	call MATMUL(QM,Q1,Q3,KA,KA,KA,-ONE,km,km,km,km,km,km)	!-QAA*HAA**(R-K) IN Q3
	GOTO 31
30	call MATPOW(HMAT,EIGHAA,IR1-IK1-1,Q1,KA,km,km,km)    !!HAA**(R-K-1) in Q1
	call SUBMAT(QM,BA,Q2,km,km,km,km)	 		!FOR GAP CASE
	call MATMUL(Q2,Q1,Q3,KB,KA,KA,ONE,km,km,km,km,km,km)	!QBA*H**(R-K-1) IN Q3
C  POSTMULT BY END2
31	call MATMUL(Q3,END2,COL1,KX,KA,1,ONE,km,km,km,1,km,1)
	GOTO 43
C NEXT SECTION TO CALC POSTMULT FOR ITYPE=2,3. THIS IS QAF*UF FOR
C OPENINGS (CALC AS QAF*UF=QAB*UB+QAC*UC)=-QAA*UA, AND QBA*UA FOR GAPS
42	CONTINUE
	IF(GAP) GOTO 705
	DO 441 I=1,KA
	COL1(I,1)=ZERO
	DO 442 J=1,KA
442	COL1(I,1)=COL1(I,1)-QM(I,J)		!FORM -QAA*UA
441	CONTINUE
	GOTO 43
705	call SUBMAT(QM,BA,Q1,km,km,km,km)
	DO 44 I=1,KB
	COL1(I,1)=ZERO
	DO 44 J=1,KA
44	COL1(I,1)=COL1(I,1)+Q1(I,J)		!QBA*UA IN COL1
C
43	CONTINUE
C  NOW HAVE PRE- AND POSTMULTIPLIER FOR P(T) IN ROW1(1,J) AND COL1(I,1)
C
C CALC DENOM (NORMALISING FACTOR) FOR CURRENT PASS
C   (1) FOR F(K,R) DEN=P(R)=PROB OF BURST HAVING IR OPENINGS IS IN PR(IR)
	IF(ITYPE.EQ.1) DEN=PR(IR)
C FOR ITYPE=2,3 DEN ALREADY CALC ABOVE
C COEFFICIENT FOR EXP(-LAMBDA(M)*T) IN PDF IS THUS
C  CALC IN W1(M) AS FOLLOWS
	DO 335 M=1,KX
	W1(M)=ZERO
c	IF(OPEN) RATE(M)=SNGL(EIGAA(M))
c	IF(GAP) RATE(M)=SNGL(EIGBB(M))
	DO 336 L=1,KX
	DO 336 N=1,KX
336	W1(M)=W1(M)+ROW1(1,L)*Amat(L,N,M)*COL1(N,1)
c336	W1(M)=W1(M)+ROW1(1,L)*EM(L,M)*EN(M,N)*COL1(N,1)
c note:	AMAT(I,J,M)=EM(I,M)*EN(M,J)
C   DIV BY NORMALISING FACTOR IN DEN
	W1(M)=W1(M)/DEN
c	W(M)=SNGL(W1(M))		!END OF M LOOP
	if(open) x=dabs(eigaa(m))
	if(gap) x=dabs(eigbb(m))
	tau(m)=1000./sngl(x)
	area(m)=sngl(w1(m)/x)
335	continue
C TYPE PDF
	IF(ITYPE.NE.1) GOTO 45
	print 1041,ik1,ir1
	if(discprt) write(8,1041)ik1,ir1
1041	format(' For event #',i3, ' in a burst with',i3,' opening(s)')
	if(open) then
	   call PDFOUTs('     f(open)',ik1,ir1,area,tau,kA,am,sd,
     & 	km,.false.,.true.,discprt)
	   id1=2	!for plot save
	   ncomp=kA
	else if(gap) then
	   call PDFOUTs('     f(shut)',ik1,ir1,area,tau,kB,am,sd,
     & 	km,.false.,.true.,discprt)
	   id1=3	!for plot save
	   ncomp=kB
	endif
c	IF(OPEN) call PDFOUT('     f(open)',W,RATE,IK1,IR1,KA,0,km)
c	IF(GAP) call PDFOUT('     f(shut)',W,RATE,IK1,IR1,KB,0,km)
c   store values for plotting
	jsav=jsav+1
	idtype(1,jsav)=id1	!for 2/3 for open/shut
	idtype(2,jsav)=ir1
	idtype(3,jsav)=ik1
	ncompsav(jsav)=ncomp		!number of components
	do i1=1,ncomp
	   areasav(i1,jsav)=area(i1)
	   tausav(i1,jsav)=tau(i1)
	enddo
	GOTO 47
45	IF(ITYPE.NE.2) GOTO 46
	print 1051,ik1
	if(discprt) write(8,1051)ik1
1051	format(
     & ' For event #',i3, ' in a burst with any number of openings')
c	IF(OPEN) call PDFOUT('     f(open)',W,RATE,IK1,-1,KA,0,km)
c	IF(GAP) call PDFOUT('     f(shut)',W,RATE,IK1,-1,KB,0,km)
	if(open) then
	   call PDFOUTs('     f(open)',ik1,-1,area,tau,kA,am,sd,
     & 	km,.false.,.true.,discprt)
	   id1=2	!for plot save
	   ncomp=kA
	else if(gap) then
	   call PDFOUTs('     f(shut)',ik1,-1,area,tau,kB,am,sd,
     &	 km,.false.,.true.,discprt)
	   id1=3	!for plot save
	   ncomp=kB
	endif
c   store values for plotting
	jsav=jsav+1
	idtype(1,jsav)=id1	!for 2/3 for open/shut
	idtype(2,jsav)=0
	idtype(3,jsav)=ik1
	ncompsav(jsav)=ncomp		!number of components
	do i1=1,ncomp
	   areasav(i1,jsav)=area(i1)
	   tausav(i1,jsav)=tau(i1)
	enddo
	GOTO 47
46	continue          !itype=3 section
	if(ika.le.kA) then
	   print 26,ika		!ITYPE=3 SECTION
	   if(discprt) write(8,26) ika
26	   FORMAT( ' Conditional on start in A-state no ',i2)
	endif
	IF(OPEN) then
	   call PDFOUTs('     f(open)',-1,-1,area,tau,kA,am,sd,
     & km,.false.,.true.,discprt)
	   id1=2	!for plot save
	   ncomp=kA
c	   call PDFOUT('     f(open)',W,RATE,-1,-1,KA,0,km)
c save overall mean open time for Pocalc below
	   topen=am
c	   s=0.
c	   do 260 m=1,kA
c260	   s=s + 1000.*w(m)/(rate(m)*rate(m))	!=a(m)*tau(m)
c	   topen=s
	   if(itype.eq.3) then
		print 525		!'unconditional
		if(discprt) write(8,525)
	   endif
	endif
	if(gap) then
	   call PDFOUTs('     f(shut)',-1,-1,area,tau,kB,am,sd,
     &	 km,.false.,.true.,discprt)
	   id1=3	!for plot save
	   ncomp=kB
	endif
c	IF(GAP) call PDFOUT('     f(shut)',W,RATE,-1,-1,KB,0,km)
c   store values for plotting
	jsav=jsav+1
	idtype(1,jsav)=id1	!for 2/3 for open/shut
	idtype(2,jsav)=i		!conditional on start in open state #i (docond=T)
c Problem -unconditional pdf of gap within bursts is not distinguishable
c from unconditional pdf of all shut times at present (id1=3, id2=0, id3=0)
c so now set id2=-1 for former
	if(open) then
	   if(i.eq.kA+1) idtype(2,jsav)=0	!unconditional
	else if(gap) then
	   if(i.eq.kA+1) idtype(2,jsav)=-1	!unconditional pdf of gaps within bsts
	endif
	idtype(3,jsav)=0
	ncompsav(jsav)=ncomp		!number of components
	do i1=1,ncomp
	   areasav(i1,jsav)=area(i1)
	   tausav(i1,jsav)=tau(i1)
	enddo
47	CONTINUE	!END OF PRINT OF PDF
C
C NOW CALC MEANS. PRE AND POSTMULT ARE SAME BUT PXX(T) REPLACED
C BY QXX**(-2)
	IF(GAP) GOTO 37
	call MATINV(QM,KA,km,Q3,km)		!INV(QAA) IN Q3
	GOTO 38
37	call SUBMAT(QM,BB,Q1,km,km,km,km)
	call MATINV(Q1,KB,km,Q3,km)		!INV(QBB) IN Q3
38	call MATMUL(Q3,Q3,Q3,KX,KX,KX,ONE,km,km,km,km,km,km)   !QXX**(-2) IN Q3
C   COMPLETE MEAN AS ROW1*Q3*COL1
	call MATMUL(Q3,COL1,COL2,KX,KX,1,ONE,km,km,km,1,km,1) !Q3*COL1 IN COL2
	call MATMUL(ROW1,COL2,SCALAR,1,KX,1,ONE,1,km,km,1,1,1) !ROW1*Q3*COL1 IN SCALAR
	print 331,1000.*SCALAR(1,1)/DEN
      if(discprt) write(8,331)1000.*SCALAR(1,1)/DEN
C
334	CONTINUE	!END OF IK LOOP
333	CONTINUE	!END OF IR LOOP
c	IF(GAP.or.short) GOTO 22	!OPEN AND GAP COMPLETED FOR CURRENT ITYPE
	IF(GAP) GOTO 22	!OPEN AND GAP COMPLETED FOR CURRENT ITYPE
221	OPEN=.FALSE.		!RETURN TO DO GAPS
	goto 1000
C
22	OPEN=.TRUE.
	IF(ITYPE.EQ.3.AND.IKA.LE.KA) GOTO 23
	ITYPE=ITYPE+1	!GO ON TO NEXT DIST TYPE
	IF(ITYPE.LE.3) GOTO 1000
	GOTO 1001
23	IKA=IKA+1
	GOTO 1000
C
1001	CONTINUE
C
C NEXT DO TOTAL OPEN AND GAP TIME PER BURST AND TOTAL BURST LENGTH
C - CALL THESE ITYPE=4,5,6 RESPECTIVELY. FIRST CALC THE KA*1 VECTOR
C OF DIST CONDL ON THE STATE IN A IN WHICH THE BURST STARTS, I.E. OMIT
C TH INITIAL VECTOR,PHI. ALSO CALC INITIAL AND FINAL VECTORS IN
C ROW1(1,J) AND COL1(I,1) RESP.
C   FIRST, SECTION FOR OPEN TIME/BST. CALC UAA=QAA(I-H) IN Q2
	ITYPE=4
	DO 514 I=1,KA
514	COL1(I,1)=END1(I,1)	!FOR ITYPE=4
	print 522
	if(discprt) write(8,522)
522	FORMAT(/' TOTAL OPEN TIME PER BURST, fto(t)'/)
	DO 510 I=1,KA
	DO 510 J=1,KA
	Q1(I,J)=-HAA(I,J)
510	IF(I.EQ.J) Q1(I,J)=ONE+Q1(I,J)		!(I-HAA) IN Q1
	call MATMUL(QM,Q1,Q2,KA,KA,KA,ONE,km,km,km,km,km,km)	!UAA=QAA*(I-H) IN Q2
	KX=KA
	qname='  VAA   '
	GOTO 511
C
1002	IF(ITYPE.NE.5) GOTO 512
C   SECTION FOR GAP TIME/BST. FIRST CALC END VECTOR=COL1=QBA*END2
	print 523
	if(discprt) write(8,523)
523	FORMAT(/' TOTAL GAP TIME PER BURST (if at least 1 gap), ftg(t)'/)
	call SUBMAT(QM,BA,Q1,km,km,km,km)
	call MATMUL(Q1,END2,COL1,KB,KA,1,ONE,km,km,km,1,km,1)	!QBA*END2 IN COL1
C     NEXT CALC VBB=QBB*(I-GBA*GAB)
	call MATMUL(GBA,GAB,Q1,KB,KA,KB,ONE,km,km,km,km,km,km)	!GBA*GAB IN Q1
	DO 513 I=1,KB
	DO 513 J=1,KB
	Q1(I,J)=-Q1(I,J)
513	IF(I.EQ.J) Q1(I,J)=ONE+Q1(I,J)		!(I-GBA*GAB) IN Q1
	call SUBMAT(QM,BB,Q3,km,km,km,km)
	call MATMUL(Q3,Q1,Q2,KB,KB,KB,ONE,km,km,km,km,km,km)	!VBB IN Q2
	KX=KB
	qname='  VBB   '
	GOTO 511
C
C   SECTION FOR BURST LENGTH. GET QEE IN Q2
512	CONTINUE
	print 524
	if(discprt) write(8,524)
524	FORMAT( /' TOTAL BURST LENGTH, Fbst(t)'/)
	DO 5141 I=1,KA
5141	COL1(I,1)=END1(I,1)	!RESET END VECTOR FOR ITYPE=6
	call SUBMAT(QM,EE,Q2,km,km,km,km)
	KX=KE
	qname='  QEE   '
	GOTO 511

511	CONTINUE
C   GET SPECTRAL EXPANSION OF APPROPRIATE MATRIX (UAA,VBB, OR QEE)
c	CALL QMAT3(Q2,EM,EN,KX,EIGEN,IBAD,KAM,KAM,KMAX)
	CALL QMAT5(Q2,Amat,KX,EIGEN,IBAD,km,km,km)
	if(debug()) then
	   call ATYPD(Q2,qname,kx,kx,km,km)
	   print 700
c700	   format(' Print spectral expansion matrices [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	   if(UC(ans).eq.'Y') then
		do m=1,kx
		 call ATYPD3(amat,m,'A matrix',kx,kx,km,km,km)
		enddo
	   endif
	endif
	if(debug()) print 60,(eigen(i),i=1,kx)
	IF(IBAD.NE.0) print 14,IBAD
	KCOMP=KX	!NO OF EXPONENTIAL COMPONENTS IN PDF
	IF(ITYPE.EQ.6) KX=KA	!WANT ONLY AA PART OF PEE(T) NOW
C
c	print 13		!PRINT OUT EIGENVALUES
c	if(discprt) write(8,13)		!PRINT OUT EIGENVALUES)
c	DO 62 M=1,KCOMP
c	EIG=EIGEN(M)
c	IF(DABS(EIG).LT.1.E-9) GOTO 62
c	print 7,M,EIG,-1.0E3/EIG
c      if(discprt) write(8,7)M,EIG,-1.0E3/EIG
c62	CONTINUE
C
	DO 520 M=1,kdim
520	W1(M)=ZERO
C DO LOOP OVER ALL A STATES FOR COND DISTNS
c
	DO 515 I=1,KA
	DO 516 M=1,KCOMP
	i1=i		!to prevent warnings
	Q2(I,M)=ZERO
C   KEEP COEFF OF LAMBDA(M) TERM IN Q2(I,M) FOR PDF COND ON START IN
C   STATE I (<A)
c
	DO 517 J=1,KX
	IF(ITYPE.EQ.5) GOTO 518
c	Q2(I,M)=Q2(I,M)+EM(I,M)*EN(M,J)*COL1(J,1)	!POSTMULT BY COL1
	Q2(I,M)=Q2(I,M)+AMAT(I,J,M)*COL1(J,1)	!POSTMULT BY COL1
	GOTO 517
C     FOR TOTAL GAP CASE (ITYPE=5) P(T)=VBB(T) IS PREMULT BY GAB
518	continue
	DO 519 N=1,KB
c519	Q2(I,M)=Q2(I,M)+GAB(I,N)*EM(N,M)*EN(M,J)*COL1(J,1)
519	Q2(I,M)=Q2(I,M)+GAB(I,N)*AMAT(N,J,M)*COL1(J,1)
517	CONTINUE	!END OF J LOOP
c
c	RATE(M)=SNGL(EIGEN(M))
C   FOR TOTAL GAP DIV BY NORMALISING FACTOR=1-P(1) TO GET PDF
C   COND ON THERE BEING AT LEAST ONE GAP***BUT CORRECT NORM
C FACTOR FOR PDF COND ON START IN GIVEN A STATE IS 1-END2(I)
C BUT PRESERVE ORIG Q2(I,M) SO CAN DIV BY (1-P(O)) FOR OVERALL PDF
c	W(M)=SNGL(Q2(I,M))
	w(m)=Q2(i,m)            !w(m) now also real*8
c##	x=dabs(eigen(m))
c##	tau(m)=1000./sngl(x)
c##	area(m)=sngl(w1(m)/x)
	IF(ITYPE.NE.5) GOTO 5161
	   IF(DABS(ONE-END2(I,1)).GT.CRIT) GOTO 5162
	   print 5163, Q2(I,M),ONE-END2(I,1)
5163	   FORMAT(/' *** Q,1-END= ',2G13.6,' -RATIO TAKEN AS ZERO'/)
	   GOTO 5161
5162	   w(m)=Q2(i,m)/(ONE-END2(I,1))
C   ACCUMULATE COEFF FOR UNCOND PDF IN W1(M)
c5161	W1(M)=W1(M)+PHI(1,I)*Q2(I,M)	!PREMULT BY PHI(1,J)
5161	continue
	w1(m)=w1(m)+PHI(1,I)*Q2(I,M)	!PREMULT BY PHI(1,J)
516	CONTINUE	!END OF M LOOP
C CAN NOW TYPE CONDITIONAL PDFS (UNLESS KA=1 SO NO COND PDF)
	IF(.NOT.DOCOND) GOTO 515
	do 62 m=1,kcomp         !get area from w(m)
	x=dabs(eigen(m))
	tau(m)=1000./sngl(x)
	area(m)=sngl(w(m)/x)
62    continue
	print 26,i1		!conditional on start in A-state no
	if(discprt) write(8,26) i1
	if(itype.eq.4) then
	   call PDFOUTs('f(total op)',i1,-1,area,tau,kA,am,sd,
     &     km,.false.,.true.,discprt)
	   id1=5		!total open/bst
	   ncomp=kA
	else if(itype.eq.5) then
	   call PDFOUTs('f(total gap)',i1,-1,area,tau,kB,am,sd,
     &    km,.false.,.true.,discprt)
	   id1=6		!total shut/bst
	   ncomp=kB
	else if(itype.eq.6) then
	   call PDFOUTs('f(burst)',i1,-1,area,tau,kE,am,sd,
     & 	km,.false.,.true.,discprt)
	   id1=4		!burst length
	   ncomp=kE
	endif
c	IF(ITYPE.EQ.4) call PDFOUT(' f(total op)',W,RATE,I1,-1,KA,0,km)
c	IF(ITYPE.EQ.5) call PDFOUT('f(total gap)',W,RATE,I1,-1,KB,0,km)
c	IF(ITYPE.EQ.6) call PDFOUT('    f(burst)',W,RATE,I1,-1,KE,0,km)
c   store values for plotting
	jsav=jsav+1
	idtype(1,jsav)=id1	!for 2/3 for open/shut
	idtype(2,jsav)=i1		!conditional on start in open state #i (docond=T)
	idtype(3,jsav)=0
	ncompsav(jsav)=ncomp		!number of components
	do i1=1,ncomp
	   areasav(i1,jsav)=area(i1)
	   tausav(i1,jsav)=tau(i1)
	enddo
515	CONTINUE	!END OF I LOOP
c
C    TYPE UNCOND PDF
	SUM=ZERO
	DO 521 M=1,KCOMP
	x=dabs(eigen(m))
	tau(m)=1000./sngl(x)
	area(m)=sngl(w1(m)/x)
	if(itype.eq.5) area(m)=sngl(w1(m)/(x*(one-Pr(1))))
c	W(M)=SNGL(W1(M))
c	IF(ITYPE.EQ.5) W(M)=SNGL(W1(M)/(ONE-PR(1)))	!DIV BY 1-P(1)
C CALC MEANS FOR TOTAL OPEN AND BST LENGTH FOR CHECK OF GAP BET BST
	SUM=SUM+W1(M)/(EIGEN(M)*EIGEN(M))
521	CONTINUE
	IF(ITYPE.EQ.4) ATO=SUM
	IF(ITYPE.EQ.6) ABST=SUM
	print 525
	if(discprt) write(8,525)
525	FORMAT( ' Unconditional PDF =')
	IF(ITYPE.EQ.4) then
c	   call PDFOUT(' f(total op)',W,RATE,-1,-1,KA,0,km)
	   call PDFOUTs('f(total op)',-1,-1,area,tau,kA,am,sd,
     &    km,.false.,.true.,discprt)
	   tobst=am
	   amtop=am
c	   s=0.
c	   do 264 m=1,kA
c264	   s=s + 1000.*w(m)/(rate(m)*rate(m))	!=a(m)*tau(m)
c	   tobst=s         !keep mean burst length
	   print 526,(AM2-1.0)/ATO
         if(discprt) write(8,526)(AM2-1.0)/ATO
526	   FORMAT(/,' No of gaps within bst per unit open time= ',G13.6,/)
	else if(itype.eq.5) then
	   call PDFOUTs('f(total gap)',-1,-1,area,tau,kB,
     & 	am,sd,km,.false.,.true.,discprt)
c	   amtg=am
	else if(itype.eq.6) then
c	   call PDFOUT('    f(burst)',W,RATE,-1,-1,KE,0,km)
	   call PDFOUTs('f(burst)',-1,-1,area,tau,kE,am,sd,
     &    km,.false.,.true.,discprt)
	   tbst=am
	   ambl=am
c	   s=0.
c	   do 263 m=1,kE
c263	   s=s + 1000.*w(m)/(rate(m)*rate(m))	!=a(m)*tau(m)
c	   tbst=s         !keep mean burst length
	endif
C LOOP BACK FOR NEXT PDF
c	IF(SHORT) GOTO 123	!DO TOTAL OPEN/BST ONLY. GET PFF FIRST.
	if(short) then
	   if(itype.eq.4) then
		itype=6		!omit total shut but do burst length, if short
		goto 1002
	   endif
	else
	   itype=itype+1		!=4,5,6
	   if(itype.le.6) goto 1002
	endif
C
	print 265,tobst/tbst
	if(discprt) write(8,265) tobst/tbst
265	format(/,
     &' P(open) WITHIN BURST = (open time/bst)/(bst length) = ',g13.6,/)
c
c Insert section to test new routines for calc of burst length pdf

	print 53
	if(discprt) write(8,53)
53	format(/,' Check: unconditional distribution of burst length')
	call PHIb1(QM,Peq,phib,endb1,endb2,debug(),km)
	call PDFburst(QM,phib,endb1,area,tau,ncomp,debug(),km)
	call PDFOUTs('f(burst)',-1,-1,area,tau,ncomp,am,sd,
     &    km,.false.,.true.,discprt)
c   store values for plotting
	jsav=jsav+1
	idtype(1,jsav)=4		!for burst lengths
	idtype(2,jsav)=0		!for unconditional
	idtype(3,jsav)=0		!for unconditional
	ncompsav(jsav)=ncomp		!number of components
	do i=1,ncomp
	   areasav(i,jsav)=area(i)
	   tausav(i,jsav)=tau(i)
	enddo

c Insert section to test new routines for calc of total open/burst pdf
	print 54
	if(discprt) write(8,54)
54	format(/,' Check: distribution of total open time per burst')
	call PHIb1(QM,Peq,phib,endb1,endb2,debug(),km)
	call PDFtotop(QM,phib,endb1,area,tau,ncomp,debug(),km)
	call PDFOUTs('f(totop)',-1,-1,area,tau,kA,am,sd,
     &    km,.false.,.true.,discprt)
c   store values for plotting
	jsav=jsav+1
	idtype(1,jsav)=5		!for total open/burst
	idtype(2,jsav)=0		!for unconditional
	idtype(3,jsav)=0		!for unconditional
	ncompsav(jsav)=ncomp		!number of components
	do i=1,ncomp
	   areasav(i,jsav)=area(i)
	   tausav(i,jsav)=tau(i)
	enddo
c and bursts with r >=2
	print 55
	if(discprt) write(8,55)
55	format(/,
     &' Check: burst length pdf for bursts with two or more openings')
	call PHIb1(QM,Peq,phib,endb1,endb2,debug(),km)
	call PDFbst2(QM,phib,endb1,endb2,area,tau,ncomp,debug(),km)
	call PDFOUTs('f(bst>1)',-1,-1,area,tau,ncomp,am,sd,
     &    km,.false.,.true.,discprt)
c   store values for plotting
	jsav=jsav+1
	idtype(1,jsav)=4		!for burst lengths
	idtype(2,jsav)=-1		!for for bursts with 2 or more openings
	idtype(3,jsav)=0		!for unconditional
	ncompsav(jsav)=ncomp		!number of components
	do i=1,ncomp
	   areasav(i,jsav)=area(i)
	   tausav(i,jsav)=tau(i)
	enddo
C
C NEXT DO GAP BETWEEN BURSTS.
c NB has kcomp=kF+kB components! need larger arrays in some cases
C
C  PREMULT END1 BY PA(INF) TO GET DEN
	DEN=ZERO
	DO 812 I=1,KA
812	DEN=DEN+PEQ(I)*END1(I,1)
C DIV EACH ELEMENT OF PA(INF) BY DEN TO GET COMMON PART OF
C INIT VECTOR IN PHI(1,J)
	DO 814 I=1,KA
814	PSI(1,I)=PEQ(I)/DEN
C	print 71,(PSI(1,J),J=1,KA)
c	if(discprt) write(8,71)(PSI(1,J),J=1,KA)
C  HAVE 2 SORTS OF INIT VECTOR- GET PSI*QAB IN ROW1, AND
C PSI*QAF IN ROW2
	call SUBMAT(QM,AB,Q1,km,km,km,km)
	call MATMUL(PSI,Q1,ROW1,1,KA,KB,ONE,1,km,km,km,1,km)
	call SUBMAT(QM,AF,Q1,km,km,km,km)
	call MATMUL(PSI,Q1,ROW2,1,KA,KF,ONE,1,km,km,km,1,km)   !PSI*QAF IN ROW2
C HAVE 2 SORTS OF END VECTOR- GET QBA*UA IN COL1, AND QFA*UA IN COL2
	call SUBMAT(QM,BA,Q1,km,km,km,km)
	DO 815 I=1,KB
	COL1(I,1)=ZERO
	DO 815 J=1,KA
815	COL1(I,1)=COL1(I,1)+Q1(I,J)
	call SUBMAT(QM,FA,Q1,km,km,km,km)
	DO 816 I=1,KF
	COL2(I,1)=ZERO
	DO 816 J=1,KA
816	COL2(I,1)=COL2(I,1)+Q1(I,J)
C
	KCOMP=KF+KB	!NO OF EXP TERMS IN APP GAP PDF
	do i=1,kcomp
	   w11(i)=zero
	enddo
C
C FOR GAP BETWEEN BURSTS  NEED ONLY PBB(T), AND PFF(T).
C FIRST DO SPECTRAL EXPANSION ON QBB. NOTE THAT THESE COEFF
C HAVE NEG SIGN.
	call SUBMAT(QM,BB,Q1,km,km,km,km)
c	CALL QMAT3(Q1,EM,EN,KB,EIGBB,IBAD,KAM,KAM,KAM)
	CALL QMAT5(Q1,Amat,KB,EIGBB,IBAD,km,km,km)
	if(debug()) print 60,(eigbb(i),i=1,kb)
	DO 824 M=1,KB	!LAST KB COMPONENTS
	DO 825 I=1,KB
	DO 825 J=1,KB
c	AIJM=EM(I,M)*EN(M,J)
	TERM=ROW1(1,I)*Amat(I,J,M)*COL1(J,1)
	W11(M)=W11(M)-TERM
825	continue
	EIGEN1(M)=EIGBB(M)
	x=dabs(eigen1(m))
	tau1(m)=1000./sngl(x)
	area1(m)=sngl(w11(m)/x)
824	CONTINUE
C NOW QFF
123	continue
c	IF(KF.GT.KAM) print 1181,KF
c1181	FORMAT( ' ARRAYS TOO SMALL FOR KF= ',I5)
	call SUBMAT(QM,FF,Q1,km,km,km,km)
	CALL QMAT5(Q1,Amat,KF,EIGBB,IBAD,km,km,km)
	if(debug()) then
	   call ATYPD(Q1,'  QFF  ',kF,kF,km,km)
	   print 700
c700	   format(' Print spectral expansion matrices [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	   if(UC(ans).eq.'Y') then
		do m=1,kF
		 call ATYPD3(amat,m,'A matrix',kF,kF,km,km,km)
		enddo
	   endif
	endif
	if(debug()) print 60,(eigbb(i),i=1,kf)
c	IF(SHORT) GOTO 124
C  USE EIGBB HERE SO ALL KCOMP VALUES CAN BE KEPT IN EIGEN
C NOW ACCUMULATE THE COEFF OF LAMBDA(M) TERM IN W1(M)
	DO 819 M=1,KF
	M1=M+KB		!=KB+1,..,KCOMP
	DO 820 I=1,KF
	DO 820 J=1,KF
820	W11(M1)=W11(M1)+ROW2(1,I)*Amat(i,j,m)*COL2(J,1)
	EIGEN1(M1)=EIGBB(M)
	x=dabs(eigen1(m1))
	tau1(m1)=1000./sngl(x)
	area1(m1)=sngl(w11(m1)/x)
819	CONTINUE
C COEFFS FOR EIGENVALUES OF PFF NOW DONE.
C
C TYPE RESULT
	print 826
	if(discprt) write(8,826)
826	FORMAT(// ' PDF of GAP BETWEEN BURSTS'/)
c	call PDFOUT('      f(gap)',W,RATE,-1,-1,KCOMP,0,km)
	km1=50	!large arrays for gap between bursts
	call PDFOUTs('f(gap)',-1,-1,area1,tau1,kcomp,am,sd,
     &    km1,.false.,.true.,discprt)
c
c idtype(1,j)=7 for pdf of gaps between bursts
c	7,0,0 for all gaps between bursts
c   store values for plotting
	jsav=jsav+1
	idtype(1,jsav)=7	!gap between bursts
	idtype(2,jsav)=0		!conditional on start in open state #i (docond=T)
	idtype(3,jsav)=0
	ncompsav(jsav)=kcomp		!number of components
	do i1=1,ncomp
	   areasav(i1,jsav)=area1(i1)
	   tausav(i1,jsav)=tau1(i1)
	enddo
C
C DO CHECK AND PRINT IT
C  FIRST RECALC MEAN IN REAL*8.
	AGAP=ZERO
	DO 831 M=1,KCOMP
831	AGAP=AGAP+W11(M)/(EIGEN1(M)*EIGEN1(M))
c  CALC TOTAL OPEN
	PAINF=ZERO
	DO 827 I=1,KA
827	PAINF=PAINF+PEQ(I)	!=TOTAL PA(INF)
	print 832,PAINF
      if(discprt) write(8,832)PAINF
832	FORMAT(//,' PA(INF)= ',G13.6)
	PATEST=ATO/(ABST+AGAP)
	print 829
	if(discprt) write(8,829)
829	FORMAT(/,' (Total open/bst)/(bst length+gap length)= ')
	print 830,ATO,ABST,AGAP,PATEST
      if(discprt) write(8,830)ATO,ABST,AGAP,PATEST
830	FORMAT(G13.6,'/ (',G13.6,' + ',G13.6,')= ',G13.6)
C
C LASTLY PDF OF ALL SHUT TIMES-NEED PFF(T) AGAIN. EXPANSION IS
C STILL IN Amat (EN,EM),EIGBB. GET PA(INF)*QAF IN ROW1
124	call SUBMAT(QM,AF,Q1,km,km,km,km)
	DEN=ZERO
	DO 840 J=1,KF
	ROW1(1,J)=ZERO
	DO 841 L=1,KA
841	ROW1(1,J)=ROW1(1,J)+PEQ(L)*Q1(L,J)
840	DEN=DEN+ROW1(1,J)	!SUM OVER F
	DO 842 J=1,KF
842	ROW1(1,J)=ROW1(1,J)/DEN		!INIT VECTOR
C NOW FINAL VECTOR = -QFF*UF IN COL1
	call SUBMAT(QM,FF,Q1,km,km,km,km)
	DO 843 I=1,KF
	COL1(I,1)=ZERO
	DO 843 J=1,KF
843	COL1(I,1)=COL1(I,1)-Q1(I,J)
c
c Insert distribution of shut times conditional on starting state
c (take row1=1 for i=if, 0 otherwise i.e. need i=if only)
	if(.not.short) then
	   do if1=1,kf
		print 848,ka+if1
		if(discprt) write(8,848) ka+if1
848		FORMAT(//,
     &	' PDF of SHUT TIMES conditional on starting in state',i2,/)
	      do m=1,kf
		   w1(m)=zero
		   do j=1,kf
			w1(m)=w1(m) + amat(if1,j,m)*col1(j,1)
		   enddo
		   x=dabs(eigbb(m))
		   tau(m)=1000./sngl(x)
		   area(m)=sngl(w1(m)/x)
		enddo
		call PDFOUTs('f(shut)',if1,-1,area,tau,kF,tshut,sd,
     &	    km,.false.,.true.,discprt)
c   store values for plotting
		jsav=jsav+1
		idtype(1,jsav)=3	!shut times
		idtype(2,jsav)=kA+if1		!conditional on start in shut state #i (docond=T)
		idtype(3,jsav)=0
		ncompsav(jsav)=kF		!number of components
		do i1=1,ncomp
		   areasav(i1,jsav)=area(i1)
		   tausav(i1,jsav)=tau(i1)
		enddo
	   enddo
	endif
c Now unconditional shut time pdf
	DO 844 M=1,KF
	W1(M)=ZERO
	DO 845 I=1,KF
	DO 845 J=1,KF
845	W1(M)=W1(M)+ROW1(1,I)*Amat(i,j,m)*COL1(J,1)
c845	W1(M)=W1(M)+ROW1(1,I)*EM(I,M)*EN(M,J)*COL1(J,1)
	x=dabs(eigbb(m))
	tau(m)=1000./sngl(x)
	area(m)=sngl(w1(m)/x)
c	RATE(M)=SNGL(EIGBB(M))
c	W(M)=SNGL(W1(M))
844	CONTINUE
	print 846
	if(discprt) write(8,846)
846	FORMAT(// ' PDF of ALL SHUT TIMES'/)
c	print 13		!PRINT OUT EIGENVALUES
	DO 847 M=1,KF
	EIG=EIGBB(M)
	IF(DABS(EIG).LT.1.E-9) GOTO 847
c	print 7,M,EIG,-1.0E3/EIG
c      if(discprt) write(8,7)M,EIG,-1.0E3/EIG
847	CONTINUE
c	call PDFOUT('     f(shut)',W,RATE,-1,-1,KF,0,km)
	call PDFOUTs('f(shut)',-1,-1,area,tau,kF,tshut,sd,
     &    km,.false.,.true.,discprt)
c
c   store values for plotting
	jsav=jsav+1
	idtype(1,jsav)=3	!shut times
	idtype(2,jsav)=0
	idtype(3,jsav)=0
	ncompsav(jsav)=kF		!number of components
	do i1=1,ncomp
	   areasav(i1,jsav)=area(i1)
	   tausav(i1,jsav)=tau(i1)
	enddo
c keep areas,tau for Pocalc below
	ns=kF
c	s=0.
c	do 154 i=1,ns
c	  tau(i)=-1000./rate(i)
c	  area(i)=-w(i)/rate(i)
c	  s=s+area(i)*tau(i)
c154	continue
c	tshut=s	!overall mean
C
99	continue

c
c Add section to compare above results with use of tcrit method for
c defining bursts
	iop2=1
      print 90
      if(discprt) write(8,90)
90	format(/,/,
     &' CALCULATIONS BASED ON DIVISION INTO BURSTS BY ''EXPERIMENTAL''',
     & ' t(crit) METHOD')
c First print shut time times etc again
	print 1110
	if(discprt) write(8,1110)
1110	format(/,' Shut time distribution',/,
     & ' comp    tau (ms)       area')
	do 1111 i=1,ns
	print 1112,i,tau(i),area(i)
      if(discprt) write(8,1112)i,tau(i),area(i)
1112	format(i5,g13.6,4x,g13.6)
1111	continue
	print 122,tshut
      if(discprt) write(8,122)tshut
122	format(' Overall mean shut time (ms) = ',g13.6)
	print 1121,topen,topen/(tshut+topen)
      if(discprt) write(8,1121)topen,topen/(tshut+topen)
1121	format(' Overall mean open time (ms) = ',g13.6,/,
     & ' Popen (for large Tcrit)= ',g13.6)
c
c Calculate tcrit automatically between components k1 and k2 of shut time
c distribution, where k1,k2 are highest numbered B state and  lowest #
c C-state. There are kB+kC shut time components, so want components kB,kB+1
	k1=kB
	k2=k1+1
	tauf=tau(k1)		!define common for tcfunc
	taus=tau(k2)		!define common for tcfunc
c	areaf=area(k1)		!define common for tcfunc
c	areas=area(k2)		!define common for tcfunc
c	call TCSUB1(tc1,tc2,tc3,.true.)
c	call TCRITS1(amean,area,ncomp,kth,noprint)
	noprint=.false.
c	call TCRITS1(tau,area,ns,kB,noprint,tcvals)
	kth=0
	call TCRITS1(tau,area,ns,kth,noprint,tcvals,0,tc1)
	k1=kB
	kth=k1
	k2=k1+1
	tauf=tau(k1)		!define common for tcfunc
	taus=tau(k2)		!define common for tcfunc
	if(tcvals(kth,3).gt.0.0) then
	   tcrit=tcvals(kth,3)
	else
	   k1=ns-1
	   k2=ns
	   tcrit=tcvals(ns-1,3)
	   tauf=tau(k1)		!define common for tcfunc
	   taus=tau(k2)		!define common for tcfunc
	endif
      print 91,tcrit,k1,k2,tauf,taus
      if(discprt) write(8,91) tcrit,k1,k2,tauf,taus
91	format(/,/,' By the Jackson criterion t(crit) = ',g13.6,'ms',/,
     & ' (based on components ',i3,' and',i3,' of shut time pdf, viz',/,
     & ' tau = ',g13.6,' and',g13.6,'ms)')
c
	iopt=0
	ndec=4
93    print 92,tcrit,iopt
92	format(/,
     & ' (0) Plot distributions',/,
     & ' (1) Use this t(crit) value (',g13.6,' ms)',/,
     & ' (2) Calculate more t(crit) values, and print details',/,
     & ' (3) Do calculations for a range of t(crit) values',/,
     & ' (4) Calculate dependence of burst length etc on t(crit)',/,
     & ' (5) Find the optimum tcrit',/,
     & ' (6) End now',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(iopt)
c Do plots?
	nsav=jsav	!number of stored plots
	if(iopt.eq.0) then
	   call SCBDISP(areasav,tausav,ncompsav,idtype,nsav,
     &  docond,dok,dokr,irmax,ndim,ndtmax)
	   iopt=6
	   goto 93
	endif
	if(iopt.eq.6) goto 999
	if(iopt.eq.1.or.iopt.eq.2) then
	   if(iopt.eq.2) then
		print 1110		!print shut pdf again on screen
		do 94 i=1,ns
		print 1112,i,tau(i),area(i)
94		continue
cc		call TCSUB1(tc1,tc2,tc3,.false.)
c		noprint=.false.
c		call TCRITS1(tau,area,ns,kth,noprint,tcvals,0,tc1)
		print 205
205		format(/,' Use t(crit) (ms) = ')
		call INPUTr(tcrit)
	   endif
	   tcs=tcrit
	   dtc=1.
	   tce=tcrit	!so calc only one value
	   log=.false.
	   noprint=.false.
	   imode=0
	   call CALPO(tcrit,tcs,tce,dtc,log,fac,area,tau,ns,topen,
     &    ob,stb,otb,bl,.false.,discprt,noprint,imode)
	   call TCRITS1(tau,area,ns,kth,noprint,tcvals,1,tcrit)	!print miclassif
	   iopt=iopt+1
	   goto 93
	endif
	if(iopt.eq.3) then
	   print 1131
1131	   format(' Use Log spacing of t(crit) values [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	   log=ans.eq.'Y'
	   print 134
134	   format('&t(crit) (ms): initial, final values = ')
	   call INPUT2r(tcs,tce)
	   if(log) then
	      print 1341,ndec
1341	      format('&Number of values per decade [',i2,'] = ')
	      call INPUTi(ndec)
	      fac=10.**(1./float(ndec))
	   else
	      print 1113
1113	      format('&t(crit) (ms): step size = ')
	      call INPUTr(dtc)
		fac=0.0
	   endif
   	   print 133
         if(discprt) write(8,133)
133	   format(/,' CALCULATIONS FOR A RANGE OF Tcrit VALUES')
	   noprint=.false.
	   imode=0
	   call CALPO(tcrit,tcs,tce,dtc,log,fac,area,tau,ns,topen,
     &    ob,stb,otb,bl,.false.,discprt,noprint,imode)
	   iopt=iopt+1
	   goto 93
	endif
c
c Calculate dependence of burst length etc on tcrit
	if(iopt.eq.4) then
	   print 140,iop2
140	   format(' Calculate dependence on Tcrit for:',/,
     &	' (1) mean burst length',/,
     &	' (2) mean number of openings per burst',/,
     &	' (3) mean total open time per burst',/,
     &	' Option number [',i2,'] = ')
	   call INPUTi(iop2)
   	   print 144
         if(discprt) write(8,144)
144	   format(/,/,' CALCULATION OF DEPENDENCE ON Tcrit FOR')
	   if(iop2.eq.1) then
   	      print 141
            if(discprt) write(8,141)
141		format('& MEAN BURST LENGTH')
	   else if(iop2.eq.2) then
   	      print 142
            if(discprt) write(8,142)
142		format('& MEAN NUMBER OF OPENINGS/BURST')
	   else if(iop2.eq.3) then
		amcrit=amtop
   	      print 143
            if(discprt) write(8,143)
143		format('& MEAN TOTAL OPEN TIME/BURST')
	   endif
	   print 1131
c1131	   format(' Use Log spacing of t(crit) values [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	   log=ans.eq.'Y'
	   print 134
c134	   format('&t(crit) (ms): initial, final values = ')
	   call INPUT2r(tcs,tce)
	   if(log) then
	      print 1341,ndec
c1341	      format('&Number of values per decade [',i2,'] = ')
	      call INPUTi(ndec)
	      fac=10.**(1./float(ndec))
	   else
	      print 1113
c1113	      format('&t(crit) (ms): step size = ')
	      call INPUTr(dtc)
		fac=0.0
	   endif
	   imode=iop2
	   noprint=.false.
	   call CALPO(tcrit,tcs,tce,dtc,log,fac,area,tau,ns,topen,
     &       ob,stb,otb,bl,.false.,discprt,noprint,imode)
c
	   iopt=iopt+1
	   goto 93
	endif
c Find optimum tcrit
c  theoretical means are, respectively, ambl, amob, amtop
c Use bisection to find tcrit such that ambl=bl, amob=ob ,amtop=otb
	iop=1
95	if(iopt.eq.5) then
	   print 72,iop
72	   format(' Choose tcrit for agreement with theoretical',/,
     &	' (1) mean burst length',/,
     &	' (2) mean number of openings per burst',/,
     &	' (3) mean total open time per burst',/,
     &	' (4) no more',/,
     &	' Option number [',i2,'] = ')
	   call INPUTi(iop)
	   if(iop.eq.4) then
		iopt=iopt+1
		goto 93
	   endif
	   if(iop.eq.1) then
		amcrit=ambl
	   else if(iop.eq.2) then
		amcrit=amob
	   else if(iop.eq.3) then
		amcrit=amtop
	   endif
	   ndisp=-1	!NO print
	   if(debug()) ndisp=1
	   eps=1.e-4	! 0.1 microsec error
	   epsy=-1.
	   x1=0.1*tau(1)		!init guess
	   x2=tau(ns)		!init guess
	   call BISEC0(TCOPT,x1,x2,amcrit,tcrit,yout,eps,epsy,
     &		nerr,ndisp,.false.)
	   if(nerr.eq.0) then
		if(iop.eq.1) then
	   	   print 73,tcrit,ambl
	         if(discprt) write(8,73) tcrit,ambl
73		   format(/,/,' OPTIMUM Tcrit',/,
     & 	  ' Tcrit = ',g13.6,'ms gives correct mean burst length = ',
     &     		g13.6,'ms')
		else if(iop.eq.2) then
	   	   print 731,tcrit,amob
	         if(discprt) write(8,731) tcrit,amob
731		   format(/,/,' OPTIMUM Tcrit',/,
     & 	 ' Tcrit = ',g13.6,'ms gives correct # of openings/bst = ',
     &		g13.6)
		else if(iop.eq.3) then
	   	   print 732,tcrit,amtop
	         if(discprt) write(8,732) tcrit,amtop
732		   format(/,/,' OPTIMUM Tcrit',/,
     & 	' Tcrit = ',g13.6,'ms gives correct total open time/bst = ',
     &		g13.6,'ms')
		endif
c
		tcs=tcrit
		dtc=1.
		tce=tcrit	!so calc only one value
		log=.false.
	      noprint=.false.
	      imode=0
	      call CALPO(tcrit,tcs,tce,dtc,log,fac,area,tau,ns,topen,
     &       ob,stb,otb,bl,.false.,discprt,noprint,imode)
	      call TCRITS1(tau,area,ns,kth,noprint,tcvals,1,tcrit)	!print miclassif
	   else
		print 821,nerr
		if(discprt) write(8,821) nerr
821 		format(' BISECTION FAILED: error = ',i3)
	   endif
c
	   if(iop.lt.3) then
		iop=iop+1
		goto 95
	   else
		iopt=iopt+1
		goto 93
	   endif
	endif
c
999	continue
c
c Write .ini in new format.  To write whole of im(),j(m) would take 40kb, but
c that will never be necessary.  First see if old .ini is present, and if so
c rename it as scbst0.ini
	print 108
108	format('/')
	call DCASK('Write defaults to disc (*.INI file)','Y',ans)
      if(ans.eq.'Y') then
c For .ini set nsc(i) temporarily negative to denote obeymr(i)=F
	   call TITENT0(
     &    'Name for .INI file:',inifile,40,.false.)

c	   INQUIRE(file='SCBST.INI',exist=present,flen=nlen)
c	   if(present.and.nlen.eq.1024) then
c		call RENAME('scbst.ini','scbst0.ini')
c		print 13
c		if(discprt) write(8,13)
c13		format(/,
c     &' Your original SCBST.INI has been renamed SCBST0.INI, and',/,
c     &' replaced with the new, larger, scbst.ini')
c	   endif
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
     &    	access='DIRECT',form='UNFORMATTED',recl=10240)
	   iver=101
	   write(16,rec=1)iver,irecq,nvdep,conc,imodold,ncyc,
     &	(nsc(i),i=1,ncyc),
     &	((im(i,j),j=1,nsc(i)),i=1,ncyc),
     &	((jm(i,j),j=1,nsc(i)),i=1,ncyc),
     &	qfilem,useprim,(isetmr(i),i=1,ncyc)
	   CLOSE(unit=16)
	   do i=1,ncyc	     !reset nsc
		nsc(i)=iabs(nsc(i))
		isetmr(i)=iabs(isetmr(i))
	   enddo
	endif
c

	call DCASK('Another run','y',ans)
	if(ans.eq.'Y') then
	   call DCASK('Modify present rate constants only','y',ans)
	   if(ans.eq.'Y') then
		iflag=3
      	if(discprt) write(8,106)
106 		format(/' New run with same mechanism',/,
     &     '===============================================')
		idest=105
		goto 105
	   else
		idest=0
		goto 104
	   endif
	endif
c
	call ENDPRINT		!flush + form feed
	call NUMCLR()
c
	END

