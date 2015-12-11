	subroutine PRTJ(j,titype,pprt,idtype,xA0,xA,xA1,
     & tpulse,nstau,Yinf,ncomp,Vmem,tau,area,npt,iver)
c To print data for set #j in MECHFIT
c
	integer*4 idtype(10)
	real*4 xA0(10),xA(10),xA1(10),tpulse(10),Yinf(10)
	integer ncomp(20),nstau(20)
	real*4 Vmem(20)   !for (V-Vrev)
	real*4 tau(10,10),area(10,10)		!nc=component #; np=idtype=set
	integer npt(10)
	integer ntime(10)
	character*50 titype(10)		!data types
	logical pprt,discprt
	common/dp/discprt
c
	id=idtype(j)
	print 1092,j,titype(id)
	if(pprt) write(7,1092) j,titype(id)
      if(discprt) write(8,1092) j,titype(id)
1092	FORMAT(/,'  Data set # ',i3,3x,a50)
	if(id.ge.3) then
	   print 39
	   if(pprt) write(7,39)
	   if(discprt) write(8,39)
39	   format(
     & ' Comp #      tau (ms)     area')
	else
	   print 391
	   if(pprt) write(7,391)
	   if(discprt) write(8,391)
391	   format(
     & ' Comp #      tau (ms)     amplitude')
	endif
	do i=1,ncomp(j)
	   print 1091,i,tau(i,j),area(i,j)
	   if(pprt)write(7,1091) i,tau(i,j),area(i,j)
	   if(discprt)write(8,1091) i,tau(i,j),area(i,j)
1091	   format(i4,3x,2(2x,G13.6))
	enddo
c Print ancillary variables
	if(idtype(j).eq.1) then
	   print 18,Yinf(j)
	   if(pprt) write(7,18) Yinf(j)
	   if(discprt) write(8,18) Yinf(j)
18	   format(
     &    ' Equilibrium current, Y(inf) (pA) = ',g13.6)
	   print 19,Vmem(j)
	   if(pprt) write(7,19) Vmem(j)
	   if(discprt) write(8,19) Vmem(j)
19	   format(
     &    ' Driving force, (V - Vrev) (mV) = ',g13.6)
	   print 14,xA0(j)
	   if(pprt) write(7,14) xA0(j)
	   if(discprt) write(8,14) xA0(j)
14	   format(
     &    ' agonist concentration (micromolar) before pulse = ',
     &	g13.6)
	   print 15,xA(j)
	   if(pprt) write(7,15) xA(j)
	   if(discprt) write(8,15) xA(j)
15	   format(' agonist concentration (micromolar) during pulse= ',
     &	g13.6)
	   print 16,xA1(j)
	   if(pprt) write(7,16) xA1(j)
	   if(discprt) write(8,16) xA1(j)
16	   format(
     &    ' agonist concentration (micromolar) after pulse = ',
     &	g13.6)
	   print 17,tpulse(j)
	   if(pprt) write(7,17) tpulse(j)
	   if(discprt) write(8,17) tpulse(j)
17	   format(' duration of pulse (ms) = ',g13.6)
	else if(idtype(j).eq.2) then
	   print 18,Yinf(j)
	   if(pprt) write(7,18) Yinf(j)
	   if(discprt) write(8,18) Yinf(j)
	   print 19,Vmem(j)
	   if(pprt) write(7,19) Vmem(j)
	   if(discprt) write(8,19) Vmem(j)
	   print 14,xA0(j)
	   if(pprt) write(7,14) xA0(j)
	   if(discprt) write(8,14) xA0(j)
	   print 16,xA1(j)
	   if(pprt) write(7,16) xA1(j)
	   if(discprt) write(8,16) xA1(j)
	else if(idtype(j).ge.3.and.idtype(j).le.5) then
	   print 12,xA(j)
	   if(pprt) write(7,12) xA(j)
	   if(discprt) write(8,12) xA(j)
12	   format(' agonist concentration (micromolar) = ',g13.6)
	   if(idtype(j).eq.5) then
		print 13,nstau(j)
		if(pprt) write(7,13) nstau(j)
		if(discprt) write(8,13) nstau(j)
13		format(
     &' Slowest ',i2,' time constants of shut time pdf multiplied ',
     &'by N_chan')
	   endif
	endif
	print 21,npt(j)
	if(pprt) write(7,21) npt(j)
	if(discprt) write(8,21) npt(j)
21	format(' Number of time points per time constant = ',i4)
c
	RETURN
	end

