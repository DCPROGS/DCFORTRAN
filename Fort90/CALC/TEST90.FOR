	program test90

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
	pon()=slock()
	character ans,getch
	character*10 titlep(50)
	character*8 titles(10)
	character*11 cdate,ctime
	character ndev*2
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

	filnam='QGEN.PRT'
c	call winprint	!print file control
c      open(unit=7,file=prtport,iostat=nerr)		!open printer
c	print 100
c	if(pon()) write(7,100)
c100	format(' QGEN: PROGRAM TO input Q MATRIX ',/)
c	call ENDPRINT
c	call SPINWRAP
	call sub90(10.)
	call sub91(10)
	ans=getch(b)
	call mode(18)
	call fillwin(0,0,640,480,1)
	call fillwin(200,200,300,300,12)
	call wrstring18(100,100,'Hello world',14,1)
	ans=getch(b)
	call mode(3)
	call SPININIT
	call winprint
	ans=getch(b)
	end

