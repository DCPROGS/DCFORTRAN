	subroutine MODREC(imodin,imodout)
c To recognise a kinetic mechanism on the basis if number of states,
c connections etc.  Used to see whether model is one of those defined
c in MODEC50, even if it has a different number (imod)
c	imodin=model number with current QMECHS
c	imodout=model number in MODEC50 (based on CJH/DC QMECHS)
	COMMON/QPAR/NCON,IC(2,200)
	integer NSC(50),IM(50,100),JM(50,100),IX(100),JX(100)
	COMMON/MPAR/NCYC,NSC,IM,JM		!for hjclik, checkqd, qset_hjc
	common/cpar/ncdep,IX,JX,x	!for hjclik, checkqd, qset_hjc, hjcdisp
	COMMON/LIG/nligsav,IL(100)	!for hjclik, checkqd, qset_hjc
	integer nbound(100,10)
	common/nbnd/nbound
c
	if(kA.eq.2.and.kF.eq.3) then	!imod=1

	endif
c
	if(kA.eq.1.and.kF.eq.3) then	!imod=9

	endif
c
	if(kA.eq.1.and.kF.eq.4) then	!imod=10

	endif
c
	if(kA.eq.1.and.kF.eq.4) then	!imod=10

	endif
