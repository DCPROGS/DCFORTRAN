	real*4 function SSDMF(kt,theta)
c To calc SSD to be minimized in MECHFIT

c=======NB neither rates nor limits have conc incorp on entry now

c  idtype=1  Pulse response (macroscopic): conc before,during,after + length
c							  =xA0,xA,xA1,t0=tpuls.
c  idtype=2  Step response (macroscopic)    conc before, after=xA0,xA
c   [for macroscopic also need Y(inf), gamma(i) and (V-Vrev) + Nchan1=parameter]
c  idtype=3  First latency pdf for pulse    as for pulse response (1 channel)
c  idtype=4  First latency pdf for step     as for step response (1 channel)
c  idtype=5  Shut time pdf                  conc=xA  (+nchan2=parameter)
c  idtype=6  Open time pdf                  conc=xA
c  idtype=7  Steady state burst length      conc=xA
c   Omit bursts for now (see GETOBS)
c Modif 07/15/99 05:53pm to incorp burst length (but kB,kC at present
c same regardless of conc)
c
c See MFSIMP for method used to number the Nchan parameters in theta()
c
c Calculated area, tau, amps = areac(m,j),tauc(j),amps(m,j)
c For jumps amps(m,j) is amplitude for mth component and jth state
c
c Modif 06/29/99 06:22pm to remove all reference to cA1 -rates AND limits have
c now  NOT got concentration in them until needed in SSDMF and MFOUT for actual
c calcs.  In any case any conc used here would be arbitrary since actual
c conc may differ from one set to another
c QD is defined with all constraints etc, corresp to the current theta(), but
c NO CONC in either -for calcs define new matrices Q0, QX etc that have
c appropriate conc for the current dataa set -
c QD REMAINS UNALTERED THROUGHOUT, AND WITH NO CONC
c
c Modif by adding common/materr/ifail,ibad, also in QMAT5 and MFSIMP
c and by aborting (with ifail=-10) if dev>1.e12, and
c	common/fails/nfail,nfailmax	!to count failures in QMAT5 etc
c Params
	real*4 theta(50)
c Local declarations
	real*8 QT(10,10),QD(10,10),Q0(10,10),Qx(10,10),Qx1(10,10)
	real*8 phio(1,10),phiF0(1,10),phib(1,10)
	real*8 endb1(10,1),endb2(10,1)
	real*8 p0(10),pinf(10),eigen(10),amat(10,10,10)
	real*8 p01(10),ptp(10),pinf1(10),eigen1(10),amat1(10,10,10)
	real*4 p0s(10),Pinfs(10),amps(10,10),tauc(10),areac(10)
	real*4 p0s1(10),Pinfs1(10),amps1(10,10),tauc1(10),areac1(10)
	real*4 Ycalc(1000)
	real*4 SSD(20)
c
c From mfsimp
	real*4 lowval(50),highval(50),pertval(50)		!parameter
	common/parlim/lowval,highval,pertval
c kt=total number of states=kA+kB+kC+kD ??
c From GETOBS
	common/data1/times(1000,20),Yobs(1000,20),weight(1000,20),
     &	ntime(20),xA0(20),xA(20),xA1(20),tpulse(20),
     &	Vmem(20),nstau(20)
	common/data2/nset,nsfit,juse(20),jmiss(20)
	common/data3/idtype(20)
	common/data4/npar,cA1,cB1
c from GETQD
	common/Vval/vhold,vkin,vref,gamma(10)		!V in mV
	COMMON/KM2/AKA1,BA,PSTAR,KMCON(9),KMFAST,aka2,arat
	COMMON/VPAR/NVDEP,IV(10),JV(10),HPAR(10)
	COMMON/QPAR/NCON,IC(2,20)
	COMMON/CPAR/NCDEP,IX(10),JX(10),X
	COMMON/MPAR/NCYC,NSC(8),IM(8,15),JM(8,15)
	COMMON/EBLK/NEQ,IE(10),JE(10),IF(10),JF(10),EFAC(10)
	COMMON/LIG/nlig,IL(10)
	common/KBLK/kA,kB,kC,kD
	common/qblk/IQ(10,10)
c from SIMPLEX
	logical disp,bad,abort
	common/abt/abort,ikey		!from simplex4
	common/prt/idisp,disp
c from MFOUT
	common/ss/SSD
c from QMAT5, MFSIMP
	common/materr/ifail,ibad
	common/fails/nfail,nfailmax	!to count failures in QMAT5 etc
c
	logical obeymr
	common/mr/obeymr		!true if microscopic reversibility to be obeyed
c
	integer jfix(50)
	common/fix/jfix		!so fixed param not changed in SSDMF
c
	logical debug
c
	logical discprt
	common/dp/discprt
c
c	debug=.true.
	debug=.false.
	k=kA+kB+kC+kD
	kF=kB+kC+kD
	km=10
	kQ=k
c
c Reset theta() to min/max values BEFORE setting micro rev
c Insert constraint -all q(i,j) must be positive! (and all Nchan)
c -constrain to be at least 0.01 (rather then at least 1.d-25) to
c prevent problems with zero values in microscopic reversibility calcs.
c
c= Neither theta nor lowval/highval have conc in yet
	do m=1,kt
	   if(jfix(m).eq.0) then	!don't change if fixed!
		if(theta(m).lt.lowval(m)) then
		   theta(m)=lowval(m)
		else if(theta(m).gt.highval(m)) then
		   theta(m)=highval(m)
		endif
	   endif
	enddo
c
c Remake Q from theta, so that pdfs can be calc using the current
c parameter values which are in theta
c The array IQ defines correspondence between param # (index of TITLEP and
c THETA),and elements of Q: IQ(i,j)=m where theta(m) is the parameter that goes
c in QT(i,j) (though may be multiplied by a statistical factor in QT)
c Copy theta temporarily into QT
	call QZERO(QT,kQ)
	do m=1,npar
	   call GETIJ(IQ,kQ,i,j,m)		!get i,j for rate constant #m
	   QT(i,j)=dble(theta(m))
	enddo
c
c incorp constraints, micro rev etc into QD, but NOT concentration
	call QZERO(QD,kQ)
c call QSETD with epsim=true so concentrations ignored -they are already
c incorporated in Q.
c= Conc NOT now in QT or QD yet -appropriate conc for each data set are
c inserted in QD via calls to QNEWC (with cA1=1, cB1=1_)before calcs are
c done, below
	V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	call QSETD(xAdum,xBdum,IL,V1,QT,QD,kQ,.true.)	!define QD without conc
c
c Put any altered values from QSETD back into theta() here. Not done
c previously (eg so param are estimated at reference pot, and without conc
c dep incorporated); but print out of final theta does not show micro rev,
c or constraints if this is done (and PDFIT doesn't use conc or pot dep)
c so better put new values in theta here.
	do m=1,npar
	   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
	   theta(m)=sngl(QD(i,j))           !redefine theta() -still no conc
	enddo
c
c   In SIMPLEX constrained values should surely be treated as fixed, rather
c than variable: QD(ie,je)=efac*QD(if,jf) so only RHS value need be adjusted
c (no point in adjusting LHS as value is overwritten anyway in QSETD)
c
	SSDTOT=0.0
c
	kpar=npar	!for Nchan parameters -see MFSIMP
	do j1=1,nsfit
	   j=juse(j1)
	   id=idtype(j)
c Macroscopic pulse
	   if(id.eq.1) then
c		call QNEWC(QD,cA1,cB1,Q0,xA0(j),xB0,
		call QNEWC(QD,1.0,1.0,Q0,xA0(j),xB0,
     &		ncdep,nlig,IL,IX,JX,k,km)		!Get Q(xA0) in Q0
		call EQOCCd(Q0,k,k-1,km,p0)	!calc init occs at xA0 -> p0
c	     Calculate the 'on' step, as for idtype=2
		t=0.0		!in sec
c 	      Calc pinf(), Qx for jumps = Q and equilib occs at conc=xA,xB
		call QNEWC(QD,1.0,1.0,Qx,xA(j),xB,
     &		ncdep,nlig,IL,IX,JX,k,km)	!Q(xA) in Qx
		call EQOCCd(Qx,k,k-1,km,Pinf)		!get pinf()=occs at x=xA
		call QMAT5(Qx,Amat,k,eigen,ibad,km,km,km)	!get amat, eigen for Q(xA)
		if(ifail.ne.0.or.ibad.ne.0) nfail=nfail+1
c          Get initial vector for 'off' step = ptp() at t=tpulse
		tp=tpulse(j)*0.001	!pulse duration in seconds
		call PTCALCS(p01,tp,tauc,amps,pinfs,p0s,p0,pinf,
     &		eigen,amat,k,km)
c          Get spectral expansion for 'off' step at conc=xA1
		call QNEWC(QD,1.0,1.0,Qx1,xA1(j),xB,
     &		ncdep,nlig,IL,IX,JX,k,km)	!Q(xA1) in Qx1
		call EQOCCd(Qx1,k,k-1,km,Pinf1)	!get pinf1()=occs at x=xA1
c		  Get amat1, eigen1 for Q(xA1)
		call QMAT5(Qx1,Amat1,k,eigen1,ibad,km,km,km)
		if(ifail.ne.0.or.ibad.ne.0) nfail=nfail+1
c        Get tau1(), amps1() for 'off' jump (so t irrelevant ) so init vect=ptp()
		t=0.
		call PTCALCS(ptp,t,tauc1,amps1,pinfs1,p0s1,p01,pinf1,
     &		eigen1,amat1,k,km)
c Macroscopic step
	   else if(id.eq.2) then
		call QNEWC(QD,1.0,1.0,Q0,xA0(j),xB0,
     &		ncdep,nlig,IL,IX,JX,k,km)		!Get Q(xA0) in Q0
		call EQOCCd(Q0,k,k-1,km,p0)	!calc init occs at xA0 -> p0
c	      call PTCALCS to get tau(m),amps(m,j),p0s(),pinfs() (so t irrelevent)
		t=0.0		!in sec
c 	      Calc pinf(), Qx for jumps = Q and equilib occs at conc=xA,xB
		call QNEWC(QD,1.0,1.0,Qx,xA(j),xB,
     &		ncdep,nlig,IL,IX,JX,k,km)		!Q(xA) in Qx
		call EQOCCd(Qx,k,k-1,km,Pinf)			!get pinf()=occs at x=xA
		call QMAT5(Qx,Amat,k,eigen,ibad,km,km,km)	!get amat, eigen for Q(xA)
		if(ifail.ne.0.or.ibad.ne.0) nfail=nfail+1
		call PTCALCS(ptp,t,tau,amps,pinfs,p0s,p0,pinf,
     &		eigen,amat,k,km)
c
c First latency after pulse
	   else if(id.eq.3) then
		call QNEWC(QD,1.0,1.0,Q0,xA0(j),xB0,
     &		ncdep,nlig,IL,IX,JX,k,km)		!Get Q(xA0) in Q0
		call EQOCCd(Q0,k,k-1,km,p0)	!calc init occs at xA0 -> p0
		call QNEWC(QD,1.0,1.0,Qx,xA(j),xB,
     &		ncdep,nlig,IL,IX,JX,k,km)	!Q(xA) in Qx
		call EQOCCd(Qx,k,k-1,km,Pinf)			!get pinf()=occs at x=xA
		if(debug) then
      	  print 54,xa0(j)*1.e6,xa(j)*1.e6
      	  if(discprt) write(8,54) xa0(j)*1.e6,xa(j)*1.e6
54		  format(/,' QD and Q matrix at concentrations = ',2g13.6)
	        call DATYP(QD,'  QD    ' ,.false.,k,k,km,km)
	        call DATYP(Q0,'  Q(0)  ' ,.false.,k,k,km,km)
	        call DATYP(Qx,'  Q(x)  ' ,.false.,k,k,km,km)
		endif
		tp=tpulse(j)	!pulse duration in milliseconds
		call pdf_FL_p(Q0,Qx,p0,-1.,ft,tp,Prns,tauc,areac,
     &		tauc1,areac1,bad,km)	!call with t= -1. to get tauc() etc
		if(bad) then
		   print 10
	         if(discprt) write(8,10)
10		   format(' Error in PDF_FL_P in SSDMF')
		   nfail=nfail+1
c		   abort=.true.	!in case called fron simplex
c=		   RETURN
		endif
c		call EQOCCd(Q0,k,k-1,km,p0)	!calc init occs at xA0 -> p0
c=off-jump only -now defunct
cc	     Calculate the 'on' step, as for idtype=2
cc 	      Calc pinf(), Qx for jumps = Q and equilib occs at conc=xA,xB
c		call QNEWC(QD,1.0,1.0,Qx,xA(j),xB,
c     &		ncdep,nlig,IL,IX,JX,k,km)	!Q(xA) in Qx
c		call EQOCCd(Qx,k,k-1,km,Pinf)		!get pinf()=occs at x=xA
c		call QMAT5(Qx,Amat,k,eigen,ibad,km,km,km)	!get amat, eigen for Q(xA)
c		if(ifail.ne.0.or.ibad.ne.0) nfail=nfail+1
cc          Get initial vector for 'off' step = p01() at t=tpulse
c		tp=tpulse(j)*0.001	!pulse duration in seconds
c		call PTCALCS(p01,tp,tauc,amps,pinfs,p0s,p0,pinf,
c     &		eigen,amat,k,km)
cc          Get Q matrix for 'off' step at conc=xA1 in Qx1
c		call QNEWC(QD,1.0,1.0,Qx1,xA1(j),xB,
c     &		ncdep,nlig,IL,IX,JX,k,km)	!Q(xA1) in Qx1
cc		call EQOCCd(Qx1,k,k-1,km,Pinf1)	!get pinf1()=occs at x=xA1
cc		Calc phiF(0) from p01 -result in both phiF0(1:10) and p0(10)
cc		(-latter not used)
c		call INVEC(5,p0,phiF0,p01,skip,km)		!subset 5=F
c		call pdfLATs(Qx1,phiF0,tauc,areac,km)
c
c First latency after step
	   else if(id.eq.4) then
		call QNEWC(QD,1.0,1.0,Q0,xA0(j),xB0,
     &		ncdep,nlig,IL,IX,JX,k,km)		!Get Q(xA0) in Q0
		call EQOCCd(Q0,k,k-1,km,p0)	!calc init occs at xA0 -> p0
		call QNEWC(QD,1.0,1.0,Qx,xA(j),xB,
     &		ncdep,nlig,IL,IX,JX,k,km)	!Q(xA) in Qx
		call EQOCCd(Qx,k,k-1,km,Pinf)			!get pinf()=occs at x=xA
c		Calc phiF(0) from p0 -result in both phiF0(1:10) and p01(10)
c		(-latter not used)
		call INVEC(5,p01,phiF0,p0,skip,km)		!subset 5=F
		call pdfLATs(Qx,phiF0,tauc,areac,km)
c Shut times
	   else if(id.eq.5) then
c		call QNEWC(QD,cA1,cB1,Qx,xA(j),xB,
c     &		ncdep,nlig,IL,IX,JX,k,km)		!Q(xA) in Qx
		call QNEWC(QD,1.0,1.0,Qx,xA(j),xB,
     &		ncdep,nlig,IL,IX,JX,k,km)		!Q(xA) in Qx
		call EQOCCd(Qx,k,k-1,km,Pinf)			!get pinf()=occs at x=xA
		ifcall=0	!full printout of errors
		if(obeymr) call MRTEST(Qx,pinf,'SSDMF   ',ifcall,k,km)
		call PHIo1(Qx,pinf,phio,km)
	      call PDFshut(Qx,phio,areac,tauc,ncompc,km)
c Open times
	   else if(id.eq.6) then
		call QNEWC(QD,1.0,1.0,Qx,xA(j),xB,
     &		ncdep,nlig,IL,IX,JX,k,km)		!Q(xA) in Qx
		call EQOCCd(Qx,k,k-1,km,Pinf)			!get pinf()=occs at x=xA
		call PHIo1(Qx,pinf,phio,km)
		call PDFopen(Qx,phio,areac,tauc,ncompc,km)
	   else if(id.eq.7) then
c NB tcrit not needed here!! just need kA,kB,kC,kD (unless calc
c is based on 'observed' definition of bursts!)
		call QNEWC(QD,1.0,1.0,Qx,xA(j),xB,
     &		ncdep,nlig,IL,IX,JX,k,km)		!Q(xA) in Qx
		call EQOCCd(Qx,k,k-1,km,Pinf)			!get pinf()=occs at x=xA
c          Calc initial and final vectors for bursts
		call PHIb1(Qx,Pinf,phib,endb1,endb2,debug,km)
		call PDFburst(Qx,phib,endb1,areac,tauc,ncompc,debug,km)
	   else if(id.eq.8) then
		call QNEWC(QD,1.0,1.0,Qx,xA(j),xB,
     &		ncdep,nlig,IL,IX,JX,k,km)		!Q(xA) in Qx
		call EQOCCd(Qx,k,k-1,km,Pinf)			!get pinf()=occs at x=xA
		call PHIb1(Qx,Pinf,phib,endb1,endb2,debug,km)
		call PDFtotop(Qx,phib,endb1,areac,tauc,ncompc,debug,km)
	   endif
c	   else if(id.eq.8) then
c	   	call PDopbst(QM,phib,areac(j),tauc(j),ncompc(j),tcrit(j),km)
c	   else if(id.eq.9) then
c		call PDopbst(QM,phib,areac(j),tauc(j),ncompc(j),tcrit(j),km)
c	   else if(id.eq.10) then
c		call PDopbst(QM,phib,areac(j),tauc(j),ncompc(j),tcrit(j),km)
c
c Now have calculated tau, area/amp -use to get Ycalc() at the specified
c times; define enV=nchan*driving pot (in mV), and gamma is in Siemens,
c so env*1.e9*gamma = current in pA
	   if(id.eq.1) then	!pulse
		kpar=kpar+1
		enchan=theta(kpar)
c		if(enchan.lt.1.0) then
		enV=enchan*Vmem(j)*1.e9		!so enV*gamma is in pA
c		 Calc Yinf
		yinfc=0.0		!equilib for 'on' step
		yinfc1=0.0		!equilib for 'off' step
		do n=1,kA
		   yinfc=yinfc + env*gamma(n)*pinfs(n)  !pinf, gamma for state #n
		   yinfc1=yinfc1 + env*gamma(n)*pinfs1(n)  !pinf, gamma for state #n
		enddo
		do i=1,ntime(j)
		   tp=tpulse(j)	!pulse duration in milliseconds
		   t=times(i,j)
		   if(t.le.tp) then
			Ycalc(i)=CURRENT(t,amps,tauc,env,gamma,Yinfc,
     &  			kA,k,km)
		   else
			Ycalc(i)=CURRENT(t-tp,amps1,tauc1,env,gamma,Yinfc1,
     &  			kA,k,km)
		   endif
		enddo
c Macroscopic step
	   else if(id.eq.2) then	!step
c           To get current need to sum over the kA open states
		kpar=kpar+1
		enchan=theta(kpar)
		enV=enchan*Vmem(j)*1.e9		!so enV*gamma is in pA
c		 Calc Yinf
		s=0.0
		do n=1,kA
		   s=s + env*gamma(n)*pinfs(n)	!pinf, gamma for state #n
		enddo
		yinfc=s
		do i=1,ntime(j)
		   t=times(i,j)
		   Ycalc(i)=CURRENT(t,amps,tauc,env,gamma,Yinfc,kA,k,km)
		enddo
	   else if(id.eq.3) then	!pdf for first latency after pulse
		do i=1,ntime(j)
		   t=times(i,j)
		   call pdf_FL_p(Q0,Qx,p0,t,ft,tp,Prns,tauc,areac,
     &		tauc1,areac1,bad,km)	!call with t>0 to get f(t)=ft
		   if(bad) then
			print 10
		      if(discprt) write(8,10)
c10			format(' Error in PDF_FL_P in SSDMF')
			nfail=nfail+1
c			abort=.true.	!in case called fron simplex
c=			RETURN
		   endif
		   Ycalc(i)=ft
		enddo
	   else if(id.ge.4.and.id.le.8) then	!other pdf
		if(id.eq.5) then	!shut time pdf -correct slow tau for Nchan
		   kpar=kpar+1
		   enchan=theta(kpar)
		   n=nstau(j)	!correct the slowest n tau(i) values
		   do i=kF-n+1,kF
			tauc(i)=tauc(i)/enchan	!orig tauc is for 1 channel
		   enddo
		endif
		do i=1,ntime(j)
		   s=0.0
		   t=times(i,j)
		   do m=1,ncompc
		      a=areac(m)/tauc(m)
c			s=s + a*exp1(-t/tauc(m))
			s=s + AEXPS1(a,-t/tauc(m))	!avoid underflow
		   enddo
		   Ycalc(i)=s
		enddo
c	   else if(id.ge.8.and.id.le.10) then	!geometric
cc          In this case times(i,j) contains NOT times but, for
cc	     example 'number of ops/bst' and tau(m,j) are 'means' of components
c		do i=1,ntime(j)
c		   s=0.0
c		   x=times(i,j)         !ops/bst etc
c		   do m=1,ncompc
c		      a=areac(m)/tauc(m)          !tau='mean'
c			z=1.0 - (1.0/tauc(m))
c			s=s + a*(z**(x-1.0))
c		   enddo
c		   Ycalc(i)=s
c		enddo
	   endif
c
c Calc SSD for this set
	   SSD(j)=0.0
	   do i=1,ntime(j)
	      dev=Yobs(i,j) - Ycalc(i)
		if(abs(dev).lt.1.e12) then
c weight(i,j) now precalc in GETOBS -same throughout!
c	         w=1.0/(Yobs(i,j)*Yobs(i,j))
	         SSD(j)=SSD(j) + weight(i,j)*dev*dev
		else
		   nfail=nfail+1
		   SSD(j)=1.e30
		endif
	   enddo
c divide SSD by ntime(j) to normalise?
	   SSD(j)=SSD(j)/float(ntime(j))
	   SSDTOT=SSDTOT + SSD(j)
	   if(disp) then
		print 20,j,ssd(j)
20		format(' SSD(',i2,') = ',g13.6)
	   endif
c
	enddo		!end of j loop for sets
c
	SSDMF=SSDTOT
	if(nfail.gt.nfailmax) then
	   print 30, nfailmax
	   if(discprt) write(8,30) nfailmax
30	   format(' RUN ABORTED because nfail exceeded ',i3,/)
	   abort=.true.
	endif
c
	RETURN
	end

