	function TCOPT(tcrit)
c Function for BISEC0, to calculate optimum tcrit in SCBST
	common/tco/iop,amcrit,topen
	COMMON/TBLK/ tau(10),area(10),ns,kth	!as used in tcrits also
	logical noprint,log
c
	tc0=tcrit
	dtc=1.
	tc1=tcrit	!so calc only one value
	log=.false.
	fac=1.
	noprint=.true.
c	noprint=.false.	!for debug
	call CALPO(tcrit,tc0,tc1,dtc,log,fac,area,tau,ns,
     & topen,ob,stb,otb,bl,.false.,.false.,noprint)
c	pause		!for debug
	if(iop.eq.1) then
	   tcopt=bl
	else if(iop.eq.2) then
	   tcopt=ob
	else if(iop.eq.3) then
	   tcopt=otb
	endif
	RETURN
	end

