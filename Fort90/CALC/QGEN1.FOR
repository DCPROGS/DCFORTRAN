	PROGRAM QGEN
c
c 07/24/93 10:25am Lahey V5.n version of QGEN2
c (no longer has QMOD option, and now keeps all rate constants in a ONE
c  file, QDAT.DAT, rather than sep file for each model; also QGEN.INI
c  replaces QDISC.DAT, and CHARQ.DAT now renamed QMODEL.DAT).
c All three files (QGEN.INI, QDAT.DAT, QMODEL.DAT) now kept in current root
c segment (they need to be available to several different progs)
c
c 06/05/90 07:46am QGEN2 is new version of QGEN1 with simplified disk storage
c and character-input of model
c
c 06/20/89 07:51pm Lahey version
c*** March 1988.General purpose (single channel,noise,jump etc) Q input prog
c based on SCQGEN and SCSIM. Uses new subroutines QMOD2,QDEF2
c Define NLIG=number of ligands and IL(i)=ligand type for ith conc-dep
c rate (needed only if NLIG>1)
c
c July 1986: New version using QMOD,QDEF as for SCSIM to produce real*8 QD
C Version for 11-73. July 1986.
C
C SERIES OF PROGS FOR CALCULATION OF STOCHASTIC PROPS OF
C VARIOUS MECHANISMS. GETS VALUES FOR Q MATRIX. FOR ALL VERSIONS
C AFTER 21-OCT-81 STATE NUMBERING MUST BE AS FOLLOWS:
C	A=SUBSET OF OPEN STATES (STATES 1,..,KA)
C	B=SUBSET OF INTRABURST GAP STATES (STATES(KA+1,..,KA+KB)
C	C=SUBSET OF INTER-BURST GAP STATES (STATES KA+KB+1,..ETC)
C	D=VERY LONG INTER-CLUSTER STATES
C	E=(A,B)=BURST STATES	(CALLED D IN OLDER NOTES)
C	F=(B,C)=ALL SHUT STATES FOR BURST MODELS
C	G=(A,B,C)=CLUSTER STATES
C	T=ALL SHUT STATES=F FOR BURSTS,=(B.C.D) FOR CLUSTERS
C
C	'BURST' HERE='APP BURST' IN C&H II
C
	real*8 QT(10,10),QD(10,10),theta(50),pinf(10)
	real*4 PI(10,10)
	REAL*4 PSTAR(4)		!KM2 OCCS
	integer IQ(10,10)
	character*10 titlep(50)
	character*8 titles(10)
	REAL*8 gamma(10)
	INTEGER KMCON(9)
	INTEGER IC(2,20),NSC(8),IM(8,15),JM(8,15),IX(10),JX(10)
	integer IL(10)		!for ligand type
	character*11 cdate,ctime
	character ndev*2
c#	character*1 ndev(5)
c#	character*1 ans,UC
	character*32 filnam
	LOGICAL READP,pon,discprt,slock
	logical schan,noise,vjump,cjump,epsim,sameq,incond,kmfast
	character*40 mtitle		!for common/dpp/
	COMMON/KM2/AKA1,BA,PSTAR,KMCON,KMFAST,aka2,arat	!for km2occ
	COMMON/VPAR/NVDEP,IV(10),JV(10),HPAR(10)
	COMMON/KBLK/KA,KB,KC,KD
	COMMON/dimblk/K,KMAX,ir,nmod
c	common/Vval/vhold,vkin,vref,gamma,enchan		!V in mV
	common/Vval/gamma,vhold,vkin,vref,enchan		!V in mV
	COMMON/CPAR/NCDEP,IX,JX,X
	COMMON/QPAR/NCON,IC
	COMMON/MPAR/NCYC,NSC,IM,JM
	common/equil/ak1,ak2,ba2,bab,akb,akd	!for km2par,main,qdef2
	common/dp/discprt
	common/dpp/filnam,machine,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
C
	pon()=slock()
c
	filnam='QGEN.PRT'
      OPEN(unit=7,file='PRN',iostat=nerr)
	call WINPRINT	!print file control
C
1000	FORMAT(/)
101	format(a1)
2	FORMAT(I8)
c
	print 100
	if(pon()) write(7,100)
	if(discprt) write(8,100)
100	FORMAT(' QGEN: PROGRAM TO input Q MATRIX ',/)
	call DATE1(cdate)		!DC subroutine
	call TIME(ctime)
	print 3,cdate,ctime(1:8),mtitle
	if(pon()) write(7,3) cdate,ctime(1:8),mtitle
	if(discprt) write(8,3) cdate,ctime(1:8),mtitle
3	format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 399
399	format(
     & ' SCROLL LOCK on for printing'/
     & ' CAPS LOCK on for debugging')
	kmax=10		!dimension of Q
	schan=.false.
	noise=.false.
	vjump=.false.
	cjump=.false.
	epsim=.false.
	sameq=.false.
	ichan2=0	!control for 2-channel models
	print 400
400	FORMAT(' Calculation type',/,
     & ' (1) Single channel',/,
     & ' (2) Noise',/,
     & ' (3) Voltage jump',/,
     & ' (4) Concentration jump',/,
     & ' (5) EPSC simulation from single channels',/,
     & ' (6) EPC calculation(*** not done yet)',/,
     & ' (7) Repeated EPC calculation (*** not done yet)',/,
     & ' Option [1] = ')
	itype=1
	call INPUTi(itype)
	if(itype.eq.1) schan=.true.
	if(itype.eq.2) noise=.true.
	if(itype.eq.3) vjump=.true.
	if(itype.eq.4) cjump=.true.
c For EPSC simulation (EPSCSIM.FOR) conc varies with time -more convenient
c to define conc in EPSCIM than in QGEN2
	if(itype.eq.5) then
	   schan=.true.
	   epsim=.true.
	endif
c	if(itype.eq.6) epc=.true.
c	if(itype.eq.7) epcrep=.true.
	vref=-80.	!standard pot for rate constants on disc
1	continue
	call QDEF(QD,PI,pinf,QT,IQ,titlep,titles,npar,nlig,IL,
     & schan,vjump,cjump,epsim,sameq,incond,readp,ichan2,ndev,
     & machine,ndisc)
	if(ichan2.eq.2) goto 1	 !to write 2-channel Q matrix to disc
	call ENDPRINT
c
	END
