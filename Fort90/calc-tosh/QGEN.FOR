	program qgen

	integer IQ(10,10)
	integer KMCON(9)
	integer IC(2,20),NSC(8),IM(8,15),JM(8,15),IX(10),JX(10)
	integer IL(10)		!for ligand type
	real   PI(10,10)
	real   PSTAR(4)		!KM2 OCCS
	real*8 QT(10,10),QD(10,10),pinf(10)
	real*8 gamma(10)
	logical READP,pon,discprt,slock
	logical kmfast
	logical cluster,student
	character*10 titlep(50)
	character*8 titles(10)
	character*11 cdate,ctime
	character ndev*2,path*40
	character mtitle*40,filnam*32,prtport*4	!for WINPRINT
	common /KM2/AKA1,BA,PSTAR,KMCON,KMFAST,aka2,arat	!for km2occ
	common /VPAR/NVDEP,IV(10),JV(10),HPAR(10)
	common /KBLK/KA,KB,KC,KD
	common /dimblk/K,KMAX,ir,nmod
	common /Vval/gamma,vhold,vkin,vref,enchan		!V in mV
	common /CPAR/NCDEP,IX,JX,X
	common /QPAR/NCON,IC
	common /MPAR/NCYC,NSC,IM,JM
	common /equil/ak1,ak2,ba2,bab,akb,akd	!for km2par,main,qdef2
	common /dp/discprt
	common /dpp/filnam,prtport,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	common/user/student,cluster,iwindows
	pon()=slock()

	cluster=.false.
	student=.false.
	call MYPATH(path)
	if(path(1:8).eq.'O:\CVFIT') cluster=.true.

	filnam='QGEN.PRT'
	call SPININIT
	call WINPRINT	!print file control
      OPEN(unit=7,file=prtport,iostat=nerr)		!open printer
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
	vref=-80.	!standard pot for rate constants on disc
	call QDEF(QD,PI,pinf,QT,IQ,titlep,titles,npar,nlig,IL,
     & readp)
	call ENDPRINT
c
	call SPINWRAP
	END
