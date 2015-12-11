	program SCCOR
c
C PROG TO CALC EXPECTED CORRELATIONS BETWEEN OPEN TIMES,BURST LENGTHS ETC
c
c Addition 05/05/01 05:16pm to calculate the scalar form for the geometric
c decay of the correlation coefficient. The number of components will be
c
c Modif 11/23/01 11:00am for obeymr(50). Rather than add this to .ini,
c set elements of nsc(i) negative (for .ini ONLY) if obeymr(i)=false
c
c 07/19/98 07:40am incorporate front end like scalcs, scbst that uses
c getqd to establish the Q matrix
c
c 06/16/90 02:41pm Lahey version
C
c Version for 11-73: Aug 88 modif for new SCQGEN progs which use
c QDISC.DAT for Q matrix etc (all real*8 now)
C
C
C	A=SUBSET OF OPEN STATES
C	B=SUBSET OF INTRABURST GAP STATES
C	C=SUBSET OF INTER-BURST GAP STATES
C	E=(A,B)=BURST STATES (D IN OLD NOTES)
C	F=(B,C)=SHUT STATES
C	'BURST' HERE='APP BURST' IN C&H II
C CAN CHAIN FROM SCGEN1 OR SCBST, OR READ DATA FROM DISC FOR SPECIFIC
C MODEL ALREADY WRITTEN TO DISK BY SCGEN1
C
C TRY MAX DIMENSION=10,MAX IN A SUBSET=7, MAX KA=5
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C AMAT=SPECTRAL EXPANSION MATRICES
	allocatable::cov
	real*8 cov(:)
	DIMENSION AMAT(100,100,100)
c	DIMENSION EM(10,10),EN(10,10)
	DIMENSION QM(100,100),QT(100,100)
	DIMENSION Q1(100,100),Q2(100,100),Q3(100,100),Q4(100,100)	!TEMP ARRAYS
	DIMENSION GAB(100,100),GBA(100,100),HAA(100,100),RAA(100,100)
	DIMENSION ROW1(1,100),COL1(100,1),COL2(100,1),UCOL(100,1)
	DIMENSION PHI(1,100),PSI(1,100),END1(100,1),END2(100,1)
	DIMENSION CORKR(200)
	dimension W1(100)
	real*8 peq1(100)
c For scalar coefficients
	real*8 coeff(10),eig(10)
C =START AND END VECTORS
C-ASSUMES KA NOT >5
c	DIMENSION SCALAR(1,1)
c	DIMENSION W1(10),AMI(10)
c	DIMENSION EIGAA(10),EIGHAA(10),EIGBB(10),EIGEN(10)
	DIMENSION PEQ(100),eigen(100)
C    DECLARE INTEGER ARGUMENTS FOR SUBMAT
	INTEGER AA,AB,AC,BA,BB,BC,CB,CA,EE,AF,FA,FF
c	character ndev*2,infil*14
	character*1 ans,UC
	LOGICAL SHORT
	logical discprt,debug,caplock
	character*11 cdate,ctime
	character*74 mtitle
c	character*74 mtits(41)	!to read all ditto from disc
c	integer jmod(40)
c For new getqd
	character qfilem*40
	real*4 conc(10)
	integer irate(200),jrate(200)
	character*20 ligname(10)
	character*2 charmod(25,40)	!to print model
c
	real*4 u11,u12,xs
	character inifile*40
c
c for eqoccd, getqd
	logical obeymr(50),automr(50)
	common/mr/obeymr,automr
c For spanning tree
	logical useprim		!use spanning tree method
	integer isetmr(50)
	common/mr1/isetmr
c
	real*8 dgamma(100)
	common/model/imod0,charmod,jlast,ilast,mtitle,imodold
	character*10 titlep(200)
	logical readini,present
c	logical student,cluster
	COMMON/LIG/nlig,IL(100)
	integer IX(100),JX(100)		!NB 'ix' is used by RANDOM
	COMMON/CPAR/NCDEP,IX,JX,X1
c	common/mpar/ncyc,nsc(8),im1(8,15),jm1(8,15)
	integer im1(8,15),jm1(8,15)		!to read old .ini
c	COMMON/VPAR/NVDEP,IV(10),JV(10),HPAR(10)
	common/mpar/ncyc,nsc(50),im(50,100),jm(50,100)
	real*4 hpar
	COMMON/VPAR/NVDEP,IV(100),JV(100),HPAR(100)
	real*4 xa1,xb1	!for read from qgen2
c=	real*4 cA1,cB1
c
	common/dp/discprt
	character*40 mtitle1*40,filnam*32,prtport*4      !for WINPRINT
	common/dpp/filnam,prtport,ndisc,jcol,mtitle1 !for WINPRINT,ENDPRINT,DISCNUM
	common/KBLK/KA,KB,KC,KD
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
	common/deb2/idebug2	!to control printing in checkmr
c
	debug()=caplock()
c
C MISC DEFINITIONS
120	FORMAT(I8)
101	format(a1)
c
	filnam='SCCOR.PRT'
	call WINPRINT   !print file control
      OPEN(unit=7,file=prtport,iostat=nerr)             !open printer
	print 1
	if(discprt) write(8,1)
1	FORMAT(' SCCOR: Single channel correlations',/)
	call DATE1(cdate)               !DC subroutine
	call TIME(ctime)
	print 2,cdate,ctime(1:8),mtitle1
	if(discprt) write(8,2) cdate,ctime(1:8),mtitle1
2       format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 3
3       format(
     & ' CAPS LOCK on for debugging')
c
	km=100		!dimension of all arrays=10 now
	idebug2=2	!print to screen and .prt
c
	ika=10		!default max lag (in .ini)
c
452	readini=.false.
	print 450
450	format(' Read defaults from .ini file on disc [Y] ? ')
	ans='Y'
	call INPUTa(ans)
      if(ans.eq.'Y') then
	   inifile='SCCOR.INI'
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
	   if(discprt) write(8,110) inifile
110	   format(' Name of initialisation file: ',a40)
	   readini=.true.
	   if(nlen.eq.1024) then		!old.ini
      	OPEN(unit=17,file=inifile,status='UNKNOWN',
     &       access='DIRECT',form='UNFORMATTED',recl=1024)
		read(17,rec=1)irecq,nvdep,im1,jm1,xA1,xB1,nmod,imodold,ika
		conc(1)=xA1
		conc(2)=xB1
		do i=1,8
		   do j=1,15
			im(i,j)=im1(i,j)
			jm(i,j)=jm1(i,j)
		   enddo
		enddo
		CLOSE(unit=17)
		if(ika.lt.1.or.ika.gt.1000) ika=10
	   else
	     	OPEN(unit=16,file=inifile,status='UNKNOWN',
     &    	access='DIRECT',form='UNFORMATTED',recl=10240)
		read(16,rec=1)iver,irecq,nvdep,conc,imodold,ncyc,ika,
     &	(nsc(i),i=1,ncyc),
     &	((im(i,j),j=1,nsc(i)),i=1,ncyc),
     &	((jm(i,j),j=1,nsc(i)),i=1,ncyc),
     &	qfilem,useprim,(isetmr(i),i=1,ncyc)
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
c Now use getqd
c
	idest=0
	kflag=0
c Modif 06/25/03 09:06am in getqd to put npar and irate, jrate also in commons
	call GETQD(QT,nchan,dgamma,vkin,vhold,vref,
     &  titlep,ligname,iflag,iprint,readini,irecq,qfilem,
     &  useprim,kflag,idest)
c	call GETQD(QT,nchan,dgamma,vkin,vhold,vref,
c     &  titlep,ligname,iflag,iprint,readini,irecq,qfilem,
c     &  kflag,idest)
c	call GETQD(QT,irate,jrate,nchan,dgamma,vkin,vhold,vref,
c     &  npar,titlep,ligname,iflag,iprint,readini,irecq,qfilem,
c     &  kflag,idest)
c
	k=kA+kB+kC+kD
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
	call EQOCCd(QM,k,k-1,km,Peq)		!CALC EQUILIB OCCS
c  Copy  peq to peq1, in case original needed again
	do i=1,k
	   peq1(i)=peq(i)
	enddo
c
      print 151
      if(discprt) write(8,151)
151	format(/
     & ' Equilibrium occupancies = ')
	call VTYPD(peq1,'       ',k,km)
c
	ans='N'
	if(nlig.gt.0) then
	   call DCASK('Use a different concentration(s)',ans,ans)
	   if(UC(ans).eq.'Y') goto 35
	endif
	if(nlig.gt.0) then
	   do i=1,nlig
		xs=1.e6*conc(i)
		print 153,i,ligname(i),xs
		if(discprt) write(8,153) i,ligname(i),xs
153		FORMAT(/,
     &     ' Ligand #',i2,1x,a20,': concentration (muM) = ',g13.6)
	   enddo
	endif
      print 151
      if(discprt) write(8,151)
c151	format(/
c     & ' Equilibrium occupancies = ')
	call VTYPD(peq,'       ',k,km)
      print 108
      if(discprt) write(8,108)
108	format(/)
	call ATYPD(QM,'  Q     ' ,k,k,km,km)
c
cC MISC DEFINITIONS
	ONE=1.0D0
	TWO=2.0D0
	ZERO=0.0D0
C
c	CRIT=1.0D-10	!BELOW WHICH COEFFS SET TO ZERO
C SET UNIT COLUMN
	do i=1,km
	   ucol(i,1)=ONE
	enddo
C
C
C DEFINE AA,AB ETC AS ARGUMENTS FOR SUBMAT WHICH GETS A SUBMATRIX
C FROM QM. THEY ARE DECLARED AS INTEGERS AND VALUE IJ=ROW,COL- THEY
C ARE ARBITRARY INTEGERS INTERP BY SUBMAT TO GET REQ ROWS AND COLS
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
	K=KA+KB+KC
C
	SHORT=.FALSE.
	itcor=0
	ans='Y'
	call DCASK('Short version',ans,ans)
	if(UC(ans).ne.'N') SHORT=.TRUE.
	if(short) goto 902
	print 1201
1201	format(' (1) Normal version',/,
     & ' (2) short version (for correlation calcs)',/,
     & ' (3) ultra-short version (ditto)',/,
     & ' Option number [1] = ')
	itcor=1
	call INPUTi(itcor)
	itcor=itcor-1	!=0,1,2
C IF ITCOR=0 ORDINARY VERSION (NO SWITCHES DOWN)
C IF ITCOR=1 SHORT VERSION FOR CORRELATION TEST WITH PRINT OF ARRAYS
C IF ITCOR=2 (BOTH SWITCHES) ULTRA SHORT (RANK AND EIGENVALUES ONLY)
901	IF(ITCOR.GT.0) GOTO 902
	ans='N'
	call DCASK('Type Q matrix and p(inf)',ans,ans)
	if(ans.eq.'Y') then
	   CALL ATYPD(QM,'Q matrix',K,K,km,km)
         print 151
         if(discprt) write(8,151)
c151	   format(/
c        & ' Equilibrium occupancies = ')
	   call VTYPD(peq,'       ',k,km)
	endif
902	CONTINUE
C
13	FORMAT( ' Eigenvalues             Tau (ms)')
7	FORMAT(2X,I2,3X,G13.6,3X,F13.5)
C
C FIRST CALC CORRELATIONS FOR OPEN TIMES
C CALC INITIAL VECTOR FOR OPENINGS=PF(INF)*QFA/DEN
	print 81
	if(discprt) write(8,81)
81	FORMAT(/, ' Initial vector for openings (PHIo)=')
	call SUBMAT(QM,FA,Q1,km,km,km,km)	!QFA IN Q1
c	CALL ATYPD(Q1,'    QFA ',kF,kA,km,km)
	SUM=ZERO		!PREMULT BY PF(INF)
	DO 100 J=1,KA
	PHI(1,J)=ZERO
	DO 99 L=1,KF
99	PHI(1,J)=PHI(1,J)+PEQ(L+KA)*Q1(L,J)
100	SUM=SUM+PHI(1,J)
	DO 1021 J=1,KA	!NORMALISE PHI
	PHI(1,J)=PHI(1,J)/SUM
1021	PSI(1,J)=PHI(1,J)	!COPY OF PHI(O) IN PSI
	print 71,(PHI(1,J),J=1,KA)
      if(discprt) write(8,71)(PHI(1,J),J=1,KA)
71	FORMAT(2X,10G13.6)
C
C
C CALC CORRELATION COEFFICIENTS.
C 1ST CYCLE FOR SHUT TIMES ITYP=1
C 2ND CYCLE FOR OPEN TIMES ITYP=2
C 3RD CYCLE FOR OPEN-SHUT CORRELATIONS ITYP=3
	ITYP=1
	print 202
	if(discprt) write(8,202)
202	FORMAT(/' SHUT-SHUT TIME CORRELATIONS',/,
     &        ' ***************************')
	KX=KF
1030	CONTINUE	!RETURN HERE FOR SHUT
C
C  NEED GAF*GFA. USE GMAT SUBROUTINE.
	call GMAT1(AF,QM,Q4,km,km)	!GAF IN Q4
	call GMAT1(FA,QM,Q3,km,km)	!GFA IN Q3
c=	call RANK(Q4,kA,kF,km,km,irAF)
	call RANK_SVD(Q4,kA,kF,km,km,irAF)
c=	call RANK(Q3,kF,kA,km,km,irFA)
	call RANK_SVD(Q3,kF,kA,km,km,irFA)
         print 852,kA,kF,irAF,irFA
         if(discprt) write(8,852) kA,kF,irAF,irFA
852	   format(
     &' kA, kF = ',i4,',',i4,': Rank of GAF = ',i4,' Rank of GAF = ',i4)
	IF(SHORT.OR.ITYP.GT.1) GOTO 591
	call ATYPD(Q4,'    GAF ',kA,kF,km,km)
	call ATYPD(Q3,'    GFA ',kF,kA,km,km)
591	CONTINUE
C
	if(ITYP.EQ.1) then
	   call MATMUL(Q3,Q4,Q4,KF,KA,KF,ONE,km,km,km,km,km,km)   !GFA*GAF IN Q4
	   call RANK_SVD(Q4,kF,kF,km,km,irF)
         print 851,kF,irF
         if(discprt) write(8,851) kF,irF
851	   format(
     & ' kF = ',i4,': Rank of GFA*GAF = ',i4)
	   ncomp=irF-1	!number of components in geometric decay (irF=irA normally)
c
	else if(ITYP.EQ.2.OR.ITYP.EQ.3) then
	   call MATMUL(Q4,Q3,Q4,KA,KF,KA,ONE,km,km,km,km,km,km)   !GAF*GFA IN Q4
	   call RANK_SVD(Q4,kA,kA,km,km,irA)
         print 85,kA,irA
         if(discprt) write(8,85) kA,irA
85	   format(
     & ' kA = ',i4,': Rank of GAF*GFA = ',i4)
	   if(irA.ne.irF) then
		print 74,irA,irF
            if(discprt) write(8,74) irA,irF
74		format(
     &' Rank of GAF*GFA = ',i3,' is not same as that of GFA*GAF = ',i3)
	   endif
	endif
	IF(SHORT) GOTO 60
	IF(ITYP.EQ.3) GOTO 207
	IF(ITYP.EQ.2) then
	   print 118
	   if(discprt) write(8,118)
118	   FORMAT( '  GAF*GFA=')
	else if(ityp.eq.1) then
	   print 1181
	   if(discprt) write(8,1181)
1181	   FORMAT( '  GFA*GAF=')
	endif
	call ATYPD(Q4,'        ',kx,kx,km,km)
C
C NOW DO SPECTRAL EXPANSION ON Q4 SO POWERS OF IT CAN BE CALC
60	continue
c	CALL QMAT3(Q4,EM,EN,KX,EIGEN,IBAD,KAM,KAM,KMAX)
	call QMAT5(Q4,Amat,kX,eigen,IBAD,km,km,km)
	IF(IBAD.NE.0) print 14,IBAD
14	FORMAT( ' ***ERROR IN SPECTRAL EXPANSION. IBAD= ',I4)
	IF(ITYP.EQ.3) GOTO 61
	print 119
	if(discprt) write(8,119)
119	FORMAT(' Eigenvalues of:')
	IF(ITYP.EQ.2) then
	   print 118
	   if(discprt) write(8,118)
	else if(ityp.eq.1) then
	   print 1181
	   if(discprt) write(8,1181)
	endif
c	print 71,(EIGEN(J),J=1,KX)
c      if(discprt) write(8,71)(EIGEN(J),J=1,KX)
c#71	FORMAT(2X,10G13.6)
	call VTYPD(eigen,'eigenval',kx,km)
	IF(SHORT) GOTO 61
	IF(ITCOR.GT.0) GOTO 2513
	ans='N'
	call DCASK('Type spectral expansion (A) matrices for Q',ans,ans)
	if(UC(ans).ne.'Y') GOTO 61
2513	continue
	do m=1,kx
	   call ATYPD3(Amat,m,'A matrix',kx,kx,km,km,km)
	enddo
61	CONTINUE
C
C
C FOR GAPS REPLACE PHI WITH PHI*GAF
	IF(ITYP.NE.1) GOTO 207
	call GMAT1(AF,QM,Q2,km,km)	!GAF IN Q2
	call MATMUL(PHI,Q2,PHI,1,KA,KF,ONE,1,km,km,km,1,km)
c	   PHI(S)=PHI(O)*GAF IN PHI
207	CONTINUE
	IF(SHORT) GOTO 216
	print 213
	if(discprt) write(8,213)
213	FORMAT('  Phi(X)  ')
	call ATYPD(phi,'phi(X)  ',1,kx,1,km)
216	CONTINUE
C
C KEEP UA*PHI(O) (KA x KA) IN Q2- EACH ROW=PHI
C OR UF*PHI(S) FOR GAPS=KF X KF)
	DO 103 I=1,KX
	DO 103 J=1,KX
103	Q2(I,J)=PHI(1,J)
	IF(SHORT) GOTO 217
	print 122
	if(discprt) write(8,122)
122	FORMAT( ' uX*Phi(X) ')
c	CALL DATYP1(Q2,1,KX,1,KX,KAM,KAM)
	call ATYPD(Q2,'uX*phiX ',kx,kx,km,km)
217	CONTINUE
C CALCULATE CONSTANT DENOMINATOR FOR R(K)=variance of intervals
c FIRST GET 2I-U*PHI IN Q1
	DO I=1,KX
	   DO J=1,KX
		Q1(I,J)=-Q2(I,J)
		IF(I.EQ.J) Q1(I,J)=TWO+Q1(I,J)
	   enddo
	enddo
c	print 112
c112	FORMAT( ' 2I-UX*PHI(X)=')
c	call ATYPD(Q1,'        ',kx,kx,km,km)
C
C KEEP -PHI(O)*INV(QAA) IN ROW1(1,J)
C  FOR GAPS USE -PHI(S)*INV(QFF)-
	IF(ITYP.EQ.3) GOTO 208		!ROW1,COL1 ALREADY CALC
	IF(ITYP.EQ.1) call SUBMAT(QM,FF,Q3,km,km,km,km) 	!QFF IN Q3
	IF(ITYP.GE.2) call SUBMAT(QM,AA,Q3,km,km,km,km)    	!QAA IN Q3
	call MATINV(Q3,KX,km,Q3,km)	    !INV(QXX) IN Q3
	call MATMUL(PHI,Q3,ROW1,1,KX,KX,-ONE,1,km,km,km,1,km)
	IF(SHORT) GOTO 581
	print 113
	if(discprt) write(8,113)
113	FORMAT(' -phi(x)*inv(Qxx)=')
	call ATYPD(row1,'        ',1,kx,1,km)
581	CONTINUE
C
C KEEP -INV(Qxx)*uX (kX x 1) IN COL1
	call MATMUL(Q3,UCOL,COL1,KX,KX,1,-ONE,km,km,km,1,km,1)
	IF(SHORT) GOTO 218
	print 214
	if(discprt) write(8,214)
214	FORMAT('  -inv(Qxx)*Ux= ')
	call ATYPD(col1,'        ',kx,1,km,1)
218	CONTINUE
C
C IF ITYP=1 KEEP COPY OF COL1 IN COL2 FOR USE IN ITYP=3
C IN WHICH CALC COL1=GAF*COL2=GAF*INV(-QFF)*UF
	IF(ITYP.NE.1) GOTO 208
	DO 209 I=1,KF
209	COL2(I,1)=COL1(I,1)
208	CONTINUE
	IF(SHORT) GOTO 219
	print 1102
	if(discprt) write(8,1102)
1102	FORMAT( ' row1= ')
	call ATYPD(row1,'        ',1,kx,1,km)
219	CONTINUE
C
C COMPLETE CALC OF variance now. DEN=ROW1*Q1*COL1
	if(ityp.le.2) then
	   call MATSCL(row1,Q1,col1,kX,var,km,km,km)
	   if(ityp.eq.1) then
		vshut=var
		print 114,vshut
		if(discprt) write(8,114) vshut
114		FORMAT(' Variance of shut times = ',g13.6)
	   else if(ityp.eq.2) then
		vopen=var
		print 115,vshut
		if(discprt) write(8,115) vshut
115		FORMAT(' Variance of open times = ',g13.6)
	   endif
	   den=var	!denominator for current cycle ityp=1,2
	endif
	IF(ITYP.EQ.3) den=DSQRT(vopen*vshut)
C

	IF(ITCOR.GT.0) GOTO 1051
c
c Addition 05/05/01 05:16pm to calculate the scalar form for the geometric
c decay of the correlation coefficient. The number of components will be
c the number of eigenvalues that are not ONE (or zero) = rank(GAF*GFA)-1
c usually -this value defined as ncomp, above
c Check which of the kx eigenvalues are neither zero nor 1, and for each
c of these calculate the numerator of the scalar coefficient as in Ch87 (2.12)
c Use	subroutine MATSCL3(phi,Amat,m,end,k,SCAL,kp,kq,ke)
	nc=0
	do m=1,kx
	   e=dabs(eigen(m))
	   if(e.gt.1.d-12.and.dabs(e-one).gt.1.d-12) then
		nc=nc+1
		call MATSCL3(row1,Amat,m,col1,kx,SCAL,km,km,km)
		coeff(nc)=scal/den
		eig(nc)=eigen(m)
	   endif
	enddo
	if(nc.ne.ncomp) then
	   call BELL(2)
	   print 70,nc,ncomp
	   if(discprt) write(8,70) nc,ncomp
70	   format(
     &' Number of components in decay,',i2,', is not R(GAF*GFA)-1 = ',
     &	i2)
	endif
	print 72,nc
	if(discprt) write(8,72) nc
72	format(/,' Scalar form for decay of correlation; sum of ',i2,
     & ' geometric components')
	print 73,(coeff(m),eig(m),m=1,nc)
	if(ityp.ne.3) then
	   if(discprt) write(8,73) (coeff(m),eig(m),m=1,nc)
73	   format(' r(k) = ',10(g13.6,'*',g13.6,'^k'))
	else
	   if(discprt) write(8,731) (coeff(m),eig(m),m=1,nc)
731	   format(' r(k) = ',10(g13.6,'*',g13.6,'^k-1'))
	endif
C
c Print autocorrelation coefficients
	print 105,ika
	if(discprt) write(8,105) ika
105	FORMAT(/,' Correlation, r(k), for lag, k, up to kmax [',i2,'] = ')
	call INPUTi(IKA)
1051	CONTINUE
	DO 106 IK=1,IKA
	IK1=IK
	IF(ITYP.EQ.3) IK1=IK-1		!FOR OPEN-SHUT
	call MATPOW(Amat,eigen,IK1,Q1,kX,km,km,km)     !Q4**k in Q1
C SUBTRACT UX*PHI(X)=Q2 FROM THIS- RESULT IN Q1
	do  i=1,kX
	   do j=1,kX
		Q1(i,j)=Q1(i,j)-Q2(i,j)
	   enddo
	enddo
C PREMULTIPLY BY ROW1 AND POSTMULT BY COL1 TO GET NUMERATOR IN SUM
C	IF(SHORT.OR.ITYP.NE.3) GOTO 1101
	IF(SHORT) GOTO 1101
	call ATYPD(Q1,'     Q1 ',kx,kx,km,km)
1101	CONTINUE
	call MATSCL(row1,Q1,col1,kX,sum,km,km,km)
c	check by calc from scalar form
	rk=0.d0
	do m=1,nc
	   rk=rk + coeff(m)*(eig(m)**ik1)
	enddo
	print 111,IK,SUM/DEN,rk
      if(discprt) write(8,111)IK,SUM/DEN,rk
111	FORMAT(' r(',I2,') = ',F9.6,'     (from scalar form,',f9.6,')')
106	CONTINUE
c Inserted 06/02/01 05:32am calculation of
c  (1) variance of mean of n open times
c  (2) variance of mean of n shut times
c  by variance = [n*var(t) - 2*(SUM all poss covariance pairs)]/n**2
c where var(t)=denominator of correlation coeff and there are binom(2,n)
c covariance pairs. However the inititial vector is the same for every
c opening so we have (n-1)*cov(1) + (n-2)*cov(2) + . . .+ cov(n-1), where
c cov(r)=covariance of open times for lag=r
c SUM[(n-i)] for i=1,..,n-1 is equal to n*(n-1)/2 = binom(2,n) so number of
c covariances is as expected
	ans='Y'
      print 108
      if(discprt) write(8,108)
	if(ityp.eq.1) then
	 call DCASK(' Calculate variance of mean of n shut times',ans,ans)
	else if(ityp.eq.2) then
	 call DCASK(' Calculate variance of mean of n open times',ans,ans)
	else if(ityp.eq.3) then
	   goto(203,204,205) ityp
	endif
	if(ans.eq.'Y') then
	   if(n.le.1) n=50
24	   print 20,n
20	   format(' Calculate variance for means of n [',i5,'] n = ')
	   call INPUTi(n)
	   ALLOCATE(cov(n-1))
	   covtot=zero		!sum the covariances
	   do i=1,n-1	!lag=i,...,n-1
		rk=0.d0
		do m=1,nc
		   rk=rk + coeff(m)*(eig(m)**i)
		enddo
		cov(i)=rk*den	!den=vshut or vopen
		if(cov(i).lt.1.d-300) goto 25	!jump out -correlaion =0 for bigger lags
		j=n-i		!=n-1 for lag 1, =n-2 for lag 2,...,=1 for lag n-1
		covtot=covtot + dfloat(j)*cov(i)
	   enddo
25	   if(ityp.eq.1) then		!shut
		en=dfloat(n)
		vtot=en*vshut + 2.d0*covtot
		vmean=vshut/en		!if independent
		vy=vshut
		vmean1=vtot/(en*en)	!if correlated
		if(vmean.gt.zero) then
		   sall=1000.d0*dsqrt(vshut)
		   smean=1000.d0*dsqrt(vmean)
		   smean1=1000.d0*dsqrt(vmean1)
		   print 21,sall,n
		   if(discprt) write(8,21) sall,n
21		   format(/,
     &	   ' SD of all shut times (ms) = ',g13.6,/,
     &	   ' For means of ',i5,' shut times:')
		endif
	   else if(ityp.eq.2) then	!open
		en=dfloat(n)
		vtot=en*vopen + 2.d0*covtot
		vy=vopen
		vmean=vopen/en		!if independent
		vmean1=vtot/(en*en)	!if correlated
		if(vmean.gt.zero) then
		   sall=1000.d0*dsqrt(vopen)
		   smean=1000.d0*dsqrt(vmean)
		   smean1=1000.d0*dsqrt(vmean1)
		   print 22,sall,n
		   if(discprt) write(8,22) sall,n
22		   format(/,
     &	   ' SD of all open times (ms) = ',g13.6,/,
     &	   ' For means of ',i5,' open times:')
		endif
	   endif
	   p=100*(smean1-smean)/smean
	   print 23,sall,n,smean,smean1,p
	   if(discprt) write(8,23) sall,n,smean,smean1,p
23	   format(
     &   '  SD of mean if uncorrelated (ms) = ',/,
     &   '   ',g13.6,'/sqrt(',i5,') = ',g13.6,/,
     &   '  Actual SD of mean (ms) = ',g13.6,/,
     &   '  Percent difference as result of correlation = ',g13.6)
c Calculate limit for large n
c Still have u*phi(X) in Q2 and X=GAF*GFA for open, X=GFA*GAF for shut, in Q4
c Need row*{SUM[A(m)*eig(m)/(1-eig(m))]}*col, where sum is over
c all non-unit eigenvalues. The last eigenvalue, eig(kx) is always the unit
c one so need SUM for m=1,kx-1
	   do i=1,kX
		do j=1,kX
		   Q1(i,j)=zero
		   do m=1,kX-1
			e=eigen(m)
			Q1(i,j)=Q1(i,j) + Amat(i,j,m)*e/(one - e)
		   enddo
		enddo
	   enddo
	   call MATSCL(row1,Q1,col1,kX,v2,km,km,km)
	   if(v2.gt.zero) then
		p=100*(dsqrt(one + two*v2/vy) -one)
		print 26, p
		if(discprt) write(8,26) p
26		format(
     &   ' Limiting value of percent difference for large n = ',g13.6)
	   endif
	   DEALLOCATE(cov)
         print 108
         if(discprt) write(8,108)
	   ans='Y'
	   call DCASK('Repeat for another value of n',ans,ans)
	   if(ans.eq.'Y') goto 24
	endif
C
	GOTO(203,204,205) ITYP
C
203	ITYP=2
	print 201
	if(discprt) write(8,201)
201	FORMAT(/' OPEN-OPEN TIME CORRELATIONS',/,
     &        ' ***************************')
	KX=KA
C NEED TO RESTORE PHI(O) TO PHI() FOR ITYP=2,3- CALC OF PHI NOT IN LOOP!
	DO 212 J=1,KA
212	PHI(1,J)=PSI(1,J)
	GOTO 1030	!REPEAT FOR OPEN COR
C
204	ITYP=3
	print 206
	if(discprt) write(8,206)
206	FORMAT(/' OPEN-SHUT CORRELATIONS',/,
     &        ' **********************')
C    PREMULT COL2=-INV(QFF)*UF BY GAF NOW, TO GIVE COL1 FOR ITYP=3
	call GMAT1(AF,QM,Q4,km,km)	!GAF IN Q4
	call MATMUL(Q4,COL2,COL1,KA,KF,1,ONE,km,km,km,1,km,1)
	KX=KA
	IF(SHORT) GOTO 1030
	print 215
	if(discprt) write(8,215)
215	FORMAT('  Col2, Col1= ')
	call ATYPD(col2,'   col1 ',kF,1,km,1)
	call ATYPD(col1,'   col1 ',kA,1,km,1)
	GOTO 1030	!REPEAT FOR OPEN-SHUT COR
C
C
C
C NOW CORRELATION OF 1ST AND KTH OPENING IN A BURST
C
205	CONTINUE
	print 123
	if(discprt) write(8,123)
123	FORMAT(/, ' CORRELATION OF 1ST AND KTH OPENING IN A BURST'/,
     & ' *******************************')
c	IKA=4
c	IRA=4
	ira=ika	!default
	IF(ITCOR.GT.0) GOTO 1241
	print 124,ika
124	FORMAT(' For k up to kmax [',i2,'] = ')
	call INPUTi(IKA)
	print 125,ira
125	FORMAT('&  and for bursts with up to r openings. r [',i2,'] = ')
	call INPUTi(IRA)
C FIRST CALCULATE PHI-BURST
C FIRST CALC START AND END VECTORS FOR BURST- NEEDED SEVERAL TIMES LATER
C   (1)INITIAL (1 X KA) VECTOR=PHI(1,J) SAY. FIRST USE GENERAL FORM
C	=PHI=PC(INF)(QCB*GBA+QCA)/SUM OVER A OF DITTO
1241	continue
	print 126
	if(discprt) write(8,126)
126	FORMAT( ' Initial vector for a burst (Phi)=')
	call GMAT1(BA,QM,GBA,km,km)	!GBA IN GBA
	call SUBMAT(QM,CB,Q1,km,km,km,km)		!QCB IN Q1
	call MATMUL(Q1,GBA,Q2,KC,KB,KA,ONE,km,km,km,km,km,km)	!QCB*GBA IN Q2
	call SUBMAT(QM,CA,Q1,km,km,km,km)		!QCA IN Q1
	do i=1,kC
	 do j=1,kA
	  Q1(I,J)=Q2(I,J)+Q1(I,J)		!QCB*GBA+QCA IN Q1
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
	DO 699 J=1,KA
699	PHI(1,J)=PHI(1,J)/SUM	!NORMALISE PHI
	print 71,(PHI(1,J),J=1,KA)
      if(discprt) write(8,71)(PHI(1,J),J=1,KA)
C    AND KEEP PHI*-INV(QAA) IN PSI(1,J)
	call MATINV(QM,KA,km,Q3,km)		!INV(QAA) IN Q3
	call MATMUL(PHI,Q3,PSI,1,KA,KA,-ONE,1,km,km,km,1,km)
c	print 129
c129	FORMAT(' -PHI*INV(QAA)=')
c	print 71,(PSI(1,J),J=1,KA)
C  (2) CALC END VECTOR END2=(GAB*GBC+GAC)*UC
	call GMAT1(AB,QM,GAB,km,km)	!GAB IN GAB
	call GMAT1(BC,QM,Q1,km,km)	!GBC IN Q1
	call MATMUL(GAB,Q1,Q1,KA,KB,KC,ONE,km,km,km,km,km,km)	!GAB*GBC IN Q1
	call GMAT1(AC,QM,Q2,km,km)	!GAC IN Q2
	DO 11 I=1,KA		!POSTMULT Q1+Q2 BY UC TO GET END2
	END2(I,1)=ZERO
	DO 11 J=1,KC
11	END2(I,1)=END2(I,1)+Q1(I,J)+Q2(I,J)
	print 12
	if(discprt) write(8,12)
12	FORMAT( ' End vector=')
	print 71,(END2(I,1),I=1,KA)
      if(discprt) write(8,71)(END2(I,1),I=1,KA)
C   (3)-INV(QAA) IS IN Q3. CALC -INV(QAA)*UA (KA X 1) IN END1
	call MATMUL(Q3,UCOL,END1,KA,KA,1,-ONE,km,km,km,1,km,1)
C   (4) KEEP PHI*QAA**(-2) IN ROW1
	call MATMUL(PSI,Q3,ROW1,1,KA,KA,-ONE,1,km,km,km,1,km)
C   (5) KEEP QAA**(-2)*UA IN COL1
	call MATMUL(Q3,END1,COL1,KA,KA,1,-ONE,km,km,km,1,km,1)
	IF(SHORT.OR.ITCOR.GT.0) GOTO 62
c	print 137
c137	FORMAT(/' GAB= ')
	call ATYPD(GAB,'    GAB ',kA,kB,km,km)
	call ATYPD(GBA,'    GBA ',kB,kA,km,km)
C CALC HAA=GAB*GBA
62	call MATMUL(GAB,GBA,HAA,KA,KB,KA,ONE,km,km,km,km,km,km)	!HAA IN HAA
	IF(SHORT) GOTO 63
	print 724
	if(discprt) write(8,724)
724	FORMAT(/' GAB*GBA= ')
	call ATYPD(HAA,'    HAA ',kA,kA,km,km)
C DO SPECTRAL EXPANSION OF HAA IN AMAT(I,J,M).
63	continue
	CALL QMAT5(HAA,Amat,kA,eigen,IBAD,km,km,km)
	IF(IBAD.NE.0) print 14,IBAD
C TYPE EIGENVALUES (AND AMAT?)
	print 131
	if(discprt) write(8,131)
131	FORMAT(' Eigenvalues of GAB*GBA=')
	print 71,(eigen(j),j=1,kA)
      if(discprt) write(8,71)(eigen(j),j=1,kA)
	IF(SHORT) GOTO 133
c	IF(ITCOR.GT.0) GOTO 133
	ans='N'
	call DCASK(
     &	'Type spectral expansion (A) matrices for GAB*GBA',ans,ans)
	if(ans.eq.'Y') then
	 do m=1,kA
	   call ATYPD3(Amat,m,'A matrix',kx,kx,km,km,km)
	 enddo
	endif
133	CONTINUE
C
C FOR EACK K (2 <= IK <=IKA) CALCULATE
C (1) R(K) FOR ANY BURST WITH K OR MORE OPENINGS
C (2) R(K;R) FOR BURST WITH EXACTLY R OPENINGS (K <= IR <= IRA)
C
C ****	N.B.KEEP INV(QAA) INTACT IN Q3!
C
	print 135
	if(discprt) write(8,135)
135	FORMAT(/,' Bsts with any          Bsts with r',/,
     &           ' # of openings           openings',/,
     &           '     r(k)                 r(k;r)')
	DO 127 IK=2,IKA
C
C   CALC HAA**(K-1) IN Q1
	call MATPOW(Amat,eigen,IK-1,Q1,kA,km,km,km)	!HAA**(K-1) IN Q1
C CALC PRK=PROB(R >= K)= PHI*Q1*UA
	call MATSCL(phi,Q1,ucol,kA,prk,km,km,km)
C CALC E(T1*TK)=ETTK=PSI*Q1*END1/PRK
	call MATSCL(psi,Q1,end1,kA,ettk,km,km,km)
	ETTK=ETTK/PRK
C CALC E(T1)=ET1=PSI*Q1*UA/PRK
	call MATSCL(psi,Q1,ucol,kA,et1,km,km,km)
	ET1=ET1/PRK
C CALC E(TK)=ETK=PHI*Q1*END1/PRK
	call MATSCL(phi,Q1,end1,kA,etk,km,km,km)
	ETK=ETK/PRK
C CALC E(T1**2)=ETT=2*ROW1*Q1*UA/PRK
	call MATSCL(row1,Q1,ucol,kA,ett,km,km,km)
	ETT=TWO*ETT/PRK
C CALC E(TK**2)=ETKTK=2*PHI*Q1*COL1/PRK
	call MATSCL(phi,Q1,col1,kA,etktk,km,km,km)
	ETKTK=TWO*ETKTK/PRK
C CALC R(K) FOR CURRENT K
c	print 1361,ETT,ET1,ETKTK,ETK
c1361	FORMAT(' ETT,ET1,ETKTK,ETK= ',4G13.6)
	CORK=(ETTK-ET1*ETK)/SQRT((ETT-ET1*ET1)*(ETKTK-ETK*ETK))
	print 136,IK,CORK
      if(discprt) write(8,136)IK,CORK
136	FORMAT(' r(',I2,')= ',F9.6)
C
	DO 128 IR=IK,IRA
C CALC H**(R-1) IN Q2  (STILL HAVE H**K-1 IN Q1)
	call MATPOW(Amat,eigen,ir-1,Q2,kA,km,km,km)	!HAA**(R-1) IN Q2
C CALC P(R)=PHI*H**(R-1)*END2 IN PR
	call MATSCL(phi,Q2,end2,kA,pr,km,km,km)
C DO OTHER CALCS THAT NEED H**(R-1) WHILE IT IS IN Q2
C CALC E(T1)=PSI*Q2*END2/PR IN ET1
	call MATSCL(psi,Q2,end2,kA,et1,km,km,km)
	ET1=ET1/PR
C CALC E(T1**2)=2*ROW1*Q2*END2/PR IN ETT
	call MATSCL(row1,Q2,end2,kA,ett,km,km,km)
	ETT=TWO*ETT/PR
C NOW KEEP H**(R-K) IN Q2
	call MATPOW(Amat,eigen,ir-ik,Q2,kA,km,km,km)	!HAA**(R-K) IN Q2
C  FOR FIXED R, CALC E(T1*TK)=ETTK=PSI*Q1*-Q3*Q2*END2/PR
	call MATMUL(Q1,Q3,RAA,KA,KA,KA,-ONE,km,km,km,km,km,km)	!-Q1*Q3 IN RAA
	call MATMUL(RAA,Q2,RAA,KA,KA,KA,ONE,km,km,km,km,km,km)	!-Q1*Q3*Q2 IN RAA
	call MATSCL(psi,RAA,end2,kA,ettk,km,km,km)
	ETTK=ETTK/PR
C CALC E(TK)=PHI*Q1*-Q3*Q2*END2/PR=PHI*RAA*END2/PR IN ETK
	call MATSCL(phi,RAA,end2,kA,etk,km,km,km)
	ETK=ETK/PR
C CALC E(TK**2)=2*PHI*Q1*(Q3**2)*Q2*END2/PR IN ETKTK
	call MATMUL(Q1,Q3,RAA,KA,KA,KA,ONE,km,km,km,km,km,km)	!Q1*Q3 IN RAA
	call MATMUL(RAA,Q3,RAA,KA,KA,KA,ONE,km,km,km,km,km,km) !Q1*Q3*Q3 IN RAA
	call MATMUL(RAA,Q2,RAA,KA,KA,KA,ONE,km,km,km,km,km,km) !Q1*(Q3**2)*Q2 IN RAA
	call MATSCL(phi,RAA,end2,kA,etktk,km,km,km)
	ETKTK=TWO*ETKTK/PR
C CALC R(K;R) FOR CURRENT K,R
	CORKR(IR)=(ETTK-ET1*ETK)/SQRT((ETT-ET1*ET1)*(ETKTK-ETK*ETK))
	print 134,IK,IR,CORKR(IR)
      if(discprt) write(8,134)IK,IR,CORKR(IR)
134	FORMAT(21X,'r(',I2,';',I2,')= ',F9.6)
C
128	CONTINUE	!END OF R LOOP
127	CONTINUE	!END OF K LOOP
C
c New section to predict adjacent interval analysis
c (1) Open-shut for lag=1. Say open=t, shut=u
c	f(t,u)=phi(o)*exp(QAA*t)*QAF*exp(QFF*u)*QFA*uA
c and integral from u1 to u2 is
c  f(t;u1,u2)=phi(o)*exp(QAA*t)*QAF*inv(QFF)*[exp(QFF*u2)-exp(QFF*u1)]*QFA*uA
c with mean =integral of f(t;u1,u2)*dt over t=0 to t=inf, viz
c  m=phi(o)*inv(-QAA)*GAF*inv(QFF)*[exp(QFF*u2)-exp(QFF*u1)]*QFA*uA
712	continue
	print 700
	if(discprt) write(8,700)
700	format(/,' OPEN TIMES ADJACENT TO SPECIFIED SHUT TIME-RANGE')
7041	print 704
704	format(' Shut time range from t1 to t2;  t1,t2 (ms) = ')
	call INPUT2r(u11,u12)
	if(u11.lt.0.0.or.u12.le.u11) then
	   call BELL(1)
	   goto 7041
	endif
c705	format(2g13.6)
	u1=dble(u11)
	u2=dble(u12)
	u1=1.d-3*u1		!sec
	u2=1.d-3*u2		!sec
C NB THIS IS FOR OPEN-SHUT (same for shut-open?)
c Recalc phi
	print 81
	if(discprt) write(8,81)
c#81	FORMAT(/, ' Initial vector for opening (PHI)=')
	call SUBMAT(QM,FA,Q1,km,km,km,km)	!QFA IN Q1
	SUM=ZERO		!PREMULT BY PF(INF)
	DO 702 J=1,KA
	PHI(1,J)=ZERO
	DO 701 L=1,KF
701	PHI(1,J)=PHI(1,J)+PEQ(L+KA)*Q1(L,J)
702	SUM=SUM+PHI(1,J)
	DO 703 J=1,KA	!NORMALISE PHI
703	PHI(1,J)=PHI(1,J)/SUM
	print 71,(PHI(1,J),J=1,KA)
      if(discprt) write(8,71)(PHI(1,J),J=1,KA)
	call MATMUL(Q1,ucol,col1,kF,kA,1,ONE,km,km,km,1,km,1) !QFA*uA in col1
C Calc exp(QFF*u2)-exp(QFF*u1) in Q2
	call SUBMAT(QM,FF,Q1,km,km,km,km)	!QFF IN Q1
	call QMAT5(Q4,Amat,kX,eigen,IBAD,km,km,km)
	call QMAT5(Q1,Amat,kF,eigen,IBAD,km,km,km)
	do 706 i=1,kF
	do 706 j=1,kF
	Q2(i,j)=zero
	do 706 m=1,kF
	Q2(i,j)=
     &  Q2(i,j) +amat(i,j,m)*(dexp1(eigen(m)*u2)-dexp1(eigen(m)*u1))
706	continue
	call MATMUL(Q2,col1,col2,kF,kF,1,ONE,km,km,km,1,km,1)
c [exp(QFF*u2)-exp(QFF*u1)]*QFA*uA in col2
c  f(t;u1,u2)=phi(o)*exp(QAA*t)*QAF*inv(QFF)*[exp(QFF*u2)-exp(QFF*u1)]*QFA*uA
c To normalise den=same thing with exp(QAA*t) replaced by inv(-QAA)
c i.e. inv(sI-QAA) with s=0
c Now premult by QAF*inv(QFF)
	call SUBMAT(QM,FF,Q1,km,km,km,km) 	!QFF IN Q1
	call MATINV(Q1,kF,km,Q1,km)	    !inv(QFF) in Q1
	call SUBMAT(QM,AF,Q2,km,km,km,km)	!QAF IN Q2
	call MATMUL(Q2,Q1,Q1,kA,kF,kF,one,km,km,km,km,km,km)  !QAF*inv(QFF) in Q1
	call MATMUL(Q1,col2,col1,kA,kF,1,one,km,km,km,1,km,1)
c Now COL1 contains the complete end vector viz
c COL1=QAF*inv(QFF)*[exp(QFF*u2)-exp(QFF*u1)]*QFA*uA  (kA x 1)
c and the initial vector is just PHI
c Calc denom and Print pdf
	call SUBMAT(QM,AA,Q1,km,km,km,km) 	!QAA IN Q1
	call MATINV(Q1,kA,km,Q2,km)	    !inv(QAA) in Q2 for den
	call MATSCL(phi,Q2,col1,kA,den,km,km,km)        !den=normalising factor
	den=-den			!as -QAA needed
	call QMAT5(Q1,Amat,kA,eigen,IBAD,km,km,km)      !expand QAA
c Calc coeffs
	do 707 m=1,kA
	w1(m)=zero
	do 708 i=1,kA
	do 708 j=1,kA
708	w1(m)=w1(m) + phi(1,i)*amat(i,j,m)*col1(j,1)
	w1(m)=w1(m)/den
707	continue
	print 709,1000.*u1,1000.*u2
	if(discprt) write(8,709) 1000.*u1,1000.*u2
709	format(' PDF of open times that precede shut times between ',
     & g13.6,' and ',g13.6,'ms')
c=	call PDOUT2(w1,eigen,kA,amean,sd,km,.false.,.true.,discprt)
	call PDFOUTd(' ',-1,-1,w1,eigen,kA,amean,sd,
     & km,.false.,.true.,discprt)
c And direct calc of mean
	call SUBMAT(QM,AA,Q1,km,km,km,km) 	!QAA IN Q1
	call MATINV(Q1,kA,km,Q1,km)	    !inv(QAA) in Q1
	call MATMUL(Q1,Q1,Q1,kA,kA,kA,one,km,km,km,km,km,km)  !inv(QAA)**2 in Q1
	call MATSCL(phi,Q1,col1,kA,am1,km,km,km)
	print 710,1000.*am1/den
	if(discprt) write(8,710) 1000.*am1/den
710	format(' Mean (ms) from direct calc = ',g13.6)
	ans='Y'
	call DCASK(
     &  'Calculate another adjacent interval distribution',ans,ans)
	if(UC(ans).ne.'N') goto 712
c
	print 108
c108	format('/')
	call DCASK('Write defaults to disc (*.INI file)','Y',ans)
      if(ans.eq.'Y') then
c For .ini set nsc(i) temporarily negative to denote obeymr(i)=F
	   call TITENT0(
     &    'Name for .INI file:',inifile,40,.false.)
c	   INQUIRE(file=inifile,exist=present,flen=nlen)
c	   if(present.and.nlen.eq.1024) then
c		call RENAME(inifile,'sccor0.ini')
c		print 15
c		if(discprt) write(8,15)
c15		format(/,
c     &' Your original SCCOR.INI has been renamed SCCOR0.INI, and',/,
c     &' replaced with the new, larger, SCCOR.INI')
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
	   write(16,rec=1)iver,irecq,nvdep,conc,imodold,ncyc,ika,
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
	call ENDPRINT
	call NUMCLR()
	END


