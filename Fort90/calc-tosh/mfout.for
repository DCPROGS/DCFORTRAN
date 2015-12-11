	subroutine MFOUT(kt,theta,titype,titlep,jfix,pprt,
     &  smin,kfit,auto,idprt,prtcor,tau,area,ncomp)

c To print observed and fitted distributions in MECHFIT
c Recalculate QD, and hence the distributions from theta() output from
c SIMPLEX, as in SSDMF.
c
c Calculated area, tau, amps = areac(m,j),tauc(j),amps(m,j)
c For jumps amps(m,j) is amplitude for mth component and jth state
c Params
c========NB HESMAT and VMAT2 expect theta(20)
	real*4 theta(50),sdev(50)
	integer jfix(50)
c========NB HESMAT and VMAT2 expect theta(20)
	character*50 titype(20)		!data types
	character*10 titlep(50)
	logical pprt,discprt,debug,caplock,debprt,printj,auto,prtcor
	logical monot
	real*4 tau(10,20),area(10,20)		!nc=component #; np=idtype
	integer*4 ncomp(20)
	common/dp/discprt
c Local declarations
	character string*74,pre*2,post*13,ans*1
	real*8 r
	real*8 QT(10,10),QD(10,10),Q0(10,10),Qx(10,10),Qx1(10,10)
	real*8 phio(1,10),phiF0(1,10),phib(1,10)
	real*8 endb1(10,1),endb2(10,1)
	real*8 p0(10),pinf(10),eigen(10),amat(10,10,10)
	real*8 p01(10),ptp(10),pinf1(10),eigen1(10),amat1(10,10,10)
	real*4 p0s(10),Pinfs(10),amps(10,10),tauc(10),areac(10)
	real*4 p0s1(10),Pinfs1(10),amps1(10,10),tauc1(10),areac1(10)
	real*4 Ycalc(1000)
c
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
c
c from SIMPLEX/ssdmf
	logical bad,abort
	common/abt/abort,ikey		!from simplex4

	logical obeymr
	common/mr/obeymr		!true if microscopic reversibility to be obeyed
c
	EXTERNAL ssdmf
c
	debug()=caplock()
c
c First remake Q from theta, so that pdfs can be calc using the current
c parameter values which are in theta
c The array IQ defines correspondence between param # (index of TITLEP and
c THETA),and elements of Q: IQ(i,j)=m where theta(m) is the parameter that goes
c in QT(i,j) (though may be multiplied by a statistical factor in QT)
c
c NB IN THIS VERSION THETA() HAS NOT GOT CONC IN IT, AND NEITHER HAS QD
C WHICH STAYS UNCHANGED ONCE DEFINED, CONC BEING PUT INTO NEW MATRICES
C (Q0, Qx ETC) VIA CALLS TO QNEWC() BEFORE CALCS DONE.
c BEFORE CALCS

	k=kA+kB+kC+kD
	kF=k - kA
	km=10
	kQ=k
	call QZERO(QT,kQ)
	do m=1,npar
	   call GETIJ(IQ,kQ,i,j,m)		!get i,j for rate constant #m
	   QT(i,j)=dble(theta(m))
	enddo
c
c incorp constraints, micro rev etc into QD
	call QZERO(QD,kQ)
c call QSETD with epsim=true so concentrations ignored
	V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD
	call QSETD(xadum,xBdum,IL,V1,QT,QD,kQ,.true.)	!still no conc in QD
c Print rate constants (as in GETQD)
	string=
     & ' * = association rate; '//char(244)//' = calc by micro rev'
	print 13,string
      if(pprt) write(7,13) string
      if(discprt) write(8,13) string
13	format(/,' OUTPUT: Fitted values of rate constants',/,
     &'  (should be 1/s, or 1/(M.s) for association rate constants)',/,
     & a74)
	do m=1,npar
	   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
	   r=QD(i,j)
	   pre='  '
	   post='             '
	   if(jfix(m).eq.1) post='(fixed)'
	   if(neq.gt.0) then	!check if constrained
		do L=1,neq
		   if(i.eq.ie(L).and.j.eq.je(L)) then
			post='(constrained)'
		   endif
		enddo
	   endif
	   if(ncdep.gt.0) then
		do n=1,ncdep
		   if(ix(n).eq.i.and.jx(n).eq.j) then
			pre='* '		!asterisk indicates rate to be mult by conc
c=			r=r/cA1		!conc not incorp int theta(), QD now
		   endif
		enddo
	   endif
c check if either if i,j or j,i is a micro rev route
	   if(ncyc.gt.0) then
		do n=1,ncyc
		   if(im(n,1).eq.i.and.jm(n,1).eq.j) then
			pre(2:2)=char(244)	!dagger sign '=q(1,2)' indicates micro rev route
		   endif
		enddo
	   endif
         print 12,m,pre,i,j,titlep(m),r,post
         if(pprt) write(7,12)m,pre,i,j,titlep(m),r,post
	   if(discprt) write(8,12)m,pre,i,j,titlep(m),r,post
12	   format(i3,2x,a2,' q(',i2,',',i2,')=',3x,a10,2x,g13.6,2x,a13)
	enddo		!end of npar loop for q(i,j) values
	do m=npar+1,kt
	   enchan=theta(m)
	   print 27, enchan
	   if(pprt) write(7,27) enchan
	   if(discprt) write(8,27) enchan
27	   format(' Estimated number of channels = ',f9.4)
	enddo
c Print also the EC50 -NB enchan may not be defined (e.g. if burst pdf
c only is fitted) so take nchan=1
	nchan=1
	enV=float(nchan)*0.1*1.e9		!so enV*gamma is in pA at -100 mV
	kdim=10
	call EQEC50(EC50,curinf,monot,curmax,concmax,cur0,pop0,QD,
     & env,gamma,.false.,k,kdim)
	print 593,cur0,pop0
	if(discprt) write(8,593) cur0,pop0
593	format(/,' At zero concentration, current (pA) = ',g12.5,
     &	', Popen = ',g12.5)
	if(monot) then
         print 591,curinf,ec50*1.e6
         if(discprt) write(8,591) curinf,ec50*1.e6
591	format(/,
     & ' Equilibrium response-concentration curve is monotonic',/,
     & ' Maximum response (pA) = ',g11.4,/,
     & '    Conc for 50% of this equilib. current (muM) = ',g11.4,/)
	else
         print 592,curmax,concmax*1.e6,ec50*1.e6,curinf
         if(discprt) write(8,592)curmax,concmax*1.e6,ec50*1.e6,curinf
592	   format(/,
     &    ' Equilibrium response-concentration curve has maximum.',/,
     &    '   Max equilib response = ',g12.5,' pA at ',g12.5,' muM',/,
     &    '   Conc for 50% of this max. current (muM) (left of max) = ',
     &     g12.5,/,
     &    '   Response at conc -> infinity = ',g12.5,' pA',/)
	endif
c Need to define p(0) for peak EC50 calc, but if fits are only to steady
c state pdfs xA0(j) may not be defined -so here calc for xA0=0.0
	call QNEWC(QD,1.0,1.0,Q0,0.0,0.0,
     &		ncdep,nlig,IL,IX,JX,k,km)		!Get Q(xA0) in Q0
	call EQOCCd(Q0,k,k-1,km,p0)	!calc init occs at xA0 -> p0
	call PEAKEC50(EC50,curinf,monot,curmax,concmax,QD,
     &   p0,env,gamma,.true.,debug(),k,kdim)
	if(monot) then
         print 593,curinf,ec50*1.e6
         if(discprt) write(8,593) curinf,ec50*1.e6
593	format(/,
     & ' Peak response-concentration curve is monotonic',/,
     & ' Maximum response (pA) = ',g11.4,/,
     & '    Conc for 50% of this maximum current = ',g11.4,' (muM)',/)
	else
         print 594,curmax,concmax*1.e6,ec50*1.e6,curinf
         if(discprt) write(8,594)curmax,concmax*1.e6,ec50*1.e6,curinf
594	   format(/,
     &    ' Peak response-concentration curve has maximum.',/,
     &    '   Max peak response = ',g12.5,' pA at ',g12.5,' muM',/,
     &    '   Conc for 50% of this max. current (left of max) = ',
     &     g12.5,' muM',/,
     &    '   Response at conc -> infinity = ',g12.5,' pA',/)
	endif
c===	call CALCEC50(EC50,curmax,QD,env,gamma,k,kdim)
c	print 30,EC50*1.e6,curmax
c      if(discprt) write(8,30) EC50*1.e6,curmax
c30	format(/,' Predicted EC50 (micromolar) = ',g13.6,/,
c     & '(maximimum equilibrium current (pA at -100 mV) = ',g13.6,')',/)
c
	if(prtcor) then
	   print 29
         if(pprt) write(7,29)
         if(discprt) write(8,29)
29	   format(' OUTPUT: approx correlations for fitted parameters,',/,
     & '  i.e. states ')
	   do i=1,kt	!print state numbers
		if(jfix(i).eq.0) then
	         print 291,i
	         if(pprt) write(7,291) i
	         if(discprt) write(8,291) i
291	         format('&,',i3)
	 	endif
	    enddo
	    fract=0.1		!course value for derivs
	    call HESMAT(theta,sdev,nfix,jfix,SMIN,SSDMF,kt,kfit,fract)
	endif
c
	if(.not.auto) then
	   print 44
44	   format(/,
     & ' (1) Print distributions only',/,
     & ' (2) Print distributions + values at fitted time points',/,
     & ' (3) Print distributions + values at specified times',/,
     & ' Option number [1] = ')
	   idprt=1
	   call INPUTi(idprt)
	endif
	debprt=idprt.eq.3
	printj=.false.
c
	kpar=npar	!for Nchan parameters -see MFSIMP
	do j1=1,nsfit
	   j=juse(j1)
	   id=idtype(j)
	   print 1092,j,titype(id)
	   if(pprt) write(7,1092) j,titype(id)
         if(discprt) write(8,1092) j,titype(id)
1092	   FORMAT(/,'  FIT TO DATA SET # ',i3,3x,a50,/,
     &    ' =========================================')
c First print lifetimes and occupancies for the appropriate conc for
c current run
	   print 28,xA(j)*1.e6
         if(discprt) write(8,28) xA(j)*1.e6
28	   format(
     & ' OUTPUT: occupancies and lifetimes for fit at conc (muM) = ',
     &  g13.6)
	   call QNEWC(QD,1.0,1.0,Qx,xA(j),xB,
     &		ncdep,nlig,IL,IX,JX,k,km)	!Q(xA) in Qx
	   call EQOCCd(Qx,k,k-1,km,Pinf)			!get pinf()=occs at x=xA
	   call SLIFED2(Qx,Pinf,pprt,discprt,km,km)
c Macroscopic pulse
	   if(id.eq.1) then
		call QNEWC(QD,1.0,1.0,Q0,xA0(j),xB0,
     &		ncdep,nlig,IL,IX,JX,k,km)		!Get Q(xA0) in Q0
		call EQOCCd(Q0,k,k-1,km,p0)	!calc init occs at xA0 -> p0
c	     Calculate the 'on' step, as for idtype=2
		t=0.0		!in sec
c 	      Calc pinf(), Qx for jumps = Q and equilib occs at conc=xA,xB
		call QNEWC(QD,1.0,1.0,Qx,xA(j),xB,
     &		ncdep,nlig,IL,IX,JX,k,km)	!Q(xA) in Qx
		call EQOCCd(Qx,k,k-1,km,Pinf)		!get pinf()=occs at x=xA
		if(debug()) then
      	  print 54,xa(j)*1.e6
      	  if(pprt) write(7,54) xa(j)*1.e6
      	  if(discprt) write(8,54) xa(j)*1.e6
54		  format(/,' Q matrix at concentration = ',g13.6)
	        call DATYP(Qx,'  Q(x)  ' ,pprt,k,k,km,km)
		endif
		call QMAT5(Qx,Amat,k,eigen,ibad,km,km,km)	!get amat, eigen for Q(xA)
c          Get initial vector for 'off' step = ptp() at t=tpulse
		tp=tpulse(j)*0.001	!pulse duration in seconds
		call PTCALCS(p01,tp,tauc,amps,pinfs,p0s,p0,pinf,
     &		eigen,amat,k,km)
c          Get spectral expansion for 'off' step at conc=xA1
		call QNEWC(QD,1.0,1.0,Qx1,xA1(j),xB,
     &		ncdep,nlig,IL,IX,JX,k,km)	!Q(xA1) in Qx1
		call EQOCCd(Qx1,k,k-1,km,Pinf1)	!get pinf1()=occs at x=xA1
		if(debug()) then
      	  print 55,xa1(j)*1.e6
      	  if(pprt) write(7,55) xa1(j)*1.e6
      	  if(discprt) write(8,55) xa1(j)*1.e6
55		  format(/,' Q matrix at concentration = ',g13.6)
	        call DATYP(Qx1,'  Q(x1) ' ,pprt,k,k,km,km)
		endif
c		  Get amat1, eigen1 for Q(xA1)
		call QMAT5(Qx1,Amat1,k,eigen1,ibad,km,km,km)
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
		tp=tpulse(j)	!pulse duration in milliseconds
		call pdf_FL_p(Q0,Qx,p0,-1.,ft,tp,Prns,tauc,areac,
     &		tauc1,areac1,bad,km)	!call with t= -1. to get tauc() etc
		if(bad) then
		   print 10
	         if(discprt) write(8,10)
10		   format(' Error in PDF_FL_P in SSDMF')
		   abort=.true.	!in case called fron simplex
		   RETURN
		endif
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
		call PHIb1(Qx,Pinf,phib,endb1,endb2,debug(),km)
		call PDFburst(Qx,phib,endb1,areac,tauc,ncompc,debug(),km)
	   else if(id.eq.8) then
		call QNEWC(QD,1.0,1.0,Qx,xA(j),xB,
     &		ncdep,nlig,IL,IX,JX,k,km)		!Q(xA) in Qx
		call EQOCCd(Qx,k,k-1,km,Pinf)			!get pinf()=occs at x=xA
		call PHIb1(Qx,Pinf,phib,endb1,endb2,debug(),km)
		call PDFtotop(Qx,phib,endb1,areac,tauc,ncompc,debug(),km)
	   endif
c
c Now have calculated tau, area/amp -use to get Ycalc() at the specified
c times; define enV=nchan*driving pot (in mV), and gamma is in Siemens,
c so env*1.e9*gamma = current in pA
	   if(id.eq.1) then	!pulse
		kpar=kpar+1
		enchan=theta(kpar)
		post='             '
		if(jfix(kpar).eq.1) post='(fixed)'
		enV=enchan*Vmem(j)*1.e9		!so enV*gamma is in pA
c		 Calc Yinf
		yinfc=0.0		!equilib for 'on' step
		yinfc1=0.0		!equilib for 'off' step
		do n=1,kA
		   yinfc=yinfc + env*gamma(n)*pinfs(n)  !pinf, gamma for state #n
		   yinfc1=yinfc1 + env*gamma(n)*pinfs1(n)  !pinf, gamma for state #n
		enddo
c Print the results
	      print 37
	      if(pprt) write(7,37)
	      if(discprt) write(8,37)
37	      format(' On-jump')
	      print 39
	      if(pprt) write(7,39)
	      if(discprt) write(8,39)
39	      format(
     & 	' Comp #      tau (ms)     amplitude')
		do m=1,k-1
		   amp=0.0
		   do n=1,kA
			amp=amp + env*gamma(n)*amps(m,n) !amp for component m, state n
		   enddo
		   print 1091,m,tauc(m),amp
		   if(pprt)write(7,1091) m,tauc(m),amp
		   if(discprt)write(8,1091) m,tauc(m),amp
1091		   format(i4,3x,2(2x,G13.6))
		enddo
	      print 38
	      if(pprt) write(7,38)
	      if(discprt) write(8,38)
38	      format(' Off-jump')
	      print 39
	      if(pprt) write(7,39)
	      if(discprt) write(8,39)
c39	      format(
c     & 	' Comp #      tau (ms)     amplitude')
		do m=1,k-1
		   amp=0.0
		   do n=1,kA
			amp=amp + env*gamma(n)*amps1(m,n) !amp for component m, state n
		   enddo
		   print 1091,m,tauc1(m),amp
		   if(pprt)write(7,1091) m,tauc1(m),amp
		   if(discprt)write(8,1091) m,tauc1(m),amp
c1091		   format(i4,3x,2(2x,G13.6))
		enddo
	      print 49, enchan,post
	      if(pprt) write(7,49) enchan,post
	      if(discprt) write(8,49) enchan,post
49	      format(' Estimated number of channels = ',f9.4,2x,a13)
c  and print data for comparison
		print 92,j,titype(id)
		if(pprt) write(7,92) j,titype(id)
	      if(discprt) write(8,92) j,titype(id)
92		FORMAT(/,40x,'  Obs data set # ',i3,3x,a50)
		print 391
		if(pprt) write(7,391)
		if(discprt) write(8,391)
391	   	format(40x,
     &	 ' Comp #      tau (ms)     amplitude')
		do i=1,ncomp(j)
		   print 91,i,tau(i,j),area(i,j)
		   if(pprt)write(7,91) i,tau(i,j),area(i,j)
		   if(discprt)write(8,91) i,tau(i,j),area(i,j)
91		   format(40x,i4,3x,2(2x,G13.6))
		enddo
c======end of printing observed data
		if(idprt.eq.2) then
		 print 46
      	 if(pprt) write(7,46)
	       if(discprt) write(8,46)
46		 format(/,
     &	  ' time (ms)         amplitude (pA)           residual',
     &							     '       percent',/,
     &	  '                observed     fitted')
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
		   res=Yobs(i,j)-Ycalc(i)
		   if(abs(Yobs(i,j)).gt.1.e-20) then
		      res1=100.*res/Yobs(i,j)
		   else
			res1=0.0
		   endif
		   print 47,t,Yobs(i,j),Ycalc(i),res,res1
      	   if(pprt) write(7,47) t,Yobs(i,j),Ycalc(i),res,res1
	         if(discprt) write(8,47) t,Yobs(i,j),Ycalc(i),res,res1
47		   format(1x,f9.2,3(2x,g13.6),2x,f9.2)
		 enddo
		endif
c
c Macroscopic step
	   else if(id.eq.2) then	!step
c           To get current need to sum over the kA open states
		kpar=kpar+1
		enchan=theta(kpar)
		post='             '
		if(jfix(kpar).eq.1) post='(fixed)'
		enV=enchan*Vmem(j)*1.e9		!so enV*gamma is in pA
c		 Calc Yinf
		s=0.0
		do n=1,kA
		   s=s + env*gamma(n)*pinfs(n)	!pinf, gamma for state #n
		enddo
		yinfc=s
c Print the results
	      print 37
	      if(pprt) write(7,37)
	      if(discprt) write(8,37)
c37	      format(' On-jump')
	      print 39
	      if(pprt) write(7,39)
	      if(discprt) write(8,39)
c39	      format(
c     & 	' Comp #      tau (ms)     amplitude')
		do m=1,k-1
		   amp=0.0
		   do n=1,kA
			amp=amp + env*gamma(n)*amps(m,n) !amp for component m, state n
		   enddo
		   print 1091,m,tauc(m),amp
		   if(pprt)write(7,1091) m,tauc(m),amp
		   if(discprt)write(8,1091) m,tauc(m),amp
c1091		   format(i4,3x,2(2x,G13.6))
		enddo
	      print 49, enchan,post
	      if(pprt) write(7,49) enchan,post
	      if(discprt) write(8,49) enchan,post
c  and print data for comparison
		print 92,j,titype(id)
		if(pprt) write(7,92) j,titype(id)
	      if(discprt) write(8,92) j,titype(id)
c92		FORMAT(/,40x,'  Obs data set # ',i3,3x,a50)
		print 391
		if(pprt) write(7,391)
		if(discprt) write(8,391)
c391	   	format(40x,
c     &	 ' Comp #      tau (ms)     amplitude')
		do i=1,ncomp(j)
		   print 91,i,tau(i,j),area(i,j)
		   if(pprt)write(7,91) i,tau(i,j),area(i,j)
		   if(discprt)write(8,91) i,tau(i,j),area(i,j)
c91		   format(40x,i4,3x,2(2x,G13.6))
		enddo
c======end of printing observed data
		if(idprt.eq.2) then
		 print 46
      	 if(pprt) write(7,46)
	       if(discprt) write(8,46)
		 do i=1,ntime(j)
		   t=times(i,j)
		   Ycalc(i)=CURRENT(t,amps,tauc,env,gamma,Yinfc,kA,k,km)
		   res=Yobs(i,j)-Ycalc(i)
		   if(abs(Yobs(i,j)).gt.1.e-20) then
		      res1=100.*res/Yobs(i,j)
		   else
			res1=0.0
		   endif
		   print 47,t,Yobs(i,j),Ycalc(i),res,res1
      	   if(pprt) write(7,47) t,Yobs(i,j),Ycalc(i),res,res1
	         if(discprt) write(8,47) t,Yobs(i,j),Ycalc(i),res,res1
c47		   format(1x,f9.2,3(2x,g13.6),2x,f9.2)
		 enddo
		endif
c
	   else if(id.eq.3) then	!pdf for first latency after pulse
c Print the results
c NB area up to tp is SUM[area(m)] - SUM[area(m)exp(-tp/tau(m))] since
c areas for t<tp in isolation do not add to 1.0
c and area above tp is SUM[area1(m)] since integral is from tp to infinity
c for exp(-(t-tp)/tau1), i.e. from 0 to infinity for exp(-t/tau1)
	      print 37
	      if(pprt) write(7,37)
	      if(discprt) write(8,37)
c37	      format(' On-jump')
	      print 42,Prns
	      if(pprt) write(7,42) Prns
	      if(discprt) write(8,42) Prns
42	      format(
     & 	' Probability that at least one opening occurs = ',f9.6)
	      print 36
	      if(pprt) write(7,36)
	      if(discprt) write(8,36)
36	      format(
     & 	' Comp #      tau (ms)     area     ')
		tp=tpulse(j)	!pulse duration in milliseconds
		s=0.0
		sa=0.0
		do m=1,kF
		   print 1091,m,tauc(m),areac(m)
		   if(pprt)write(7,1091) m,tauc(m),areac(m)
		   if(discprt)write(8,1091) m,tauc(m),areac(m)
c		   s=s + areac(m)*exp(-tp/tauc(m))	!may underflow -use AEXPs1
		   s=s + AEXPS1(areac(m),-tp/tauc(m))	!avoid underflow
		   sa=sa + areac(m)
		enddo
c Get area up to tpulse
c===		s=1.0 - s
		s=sa - s
	      print 40,s
      	if(pprt) write(7,40) s
	      if(discprt) write(8,40) s
40		format(' Area up to end of pulse = ',g13.6)
	      print 38
	      if(pprt) write(7,38)
	      if(discprt) write(8,38)
c38	      format(' Off-jump')
	      print 36
	      if(pprt) write(7,36)
	      if(discprt) write(8,36)
		s1=0.0
		do m=1,kB
		   print 1091,m,tauc1(m),areac1(m)
		   if(pprt)write(7,1091) m,tauc1(m),areac1(m)
		   if(discprt)write(8,1091) m,tauc1(m),areac1(m)
		   s1=s1 + areac1(m)
		enddo
c
c Get area from tpulse upwards
	      print 41,s1,s+s1
      	if(pprt) write(7,41) s1,s+s1
	      if(discprt) write(8,41) s1,s+s1
41		format(' Area after pulse = ',g13.6,' Total area = ',g13.6)
c
c  and print data for comparison
		print 92,j,titype(id)
		if(pprt) write(7,92) j,titype(id)
	      if(discprt) write(8,92) j,titype(id)
c92		FORMAT(/,40x,'  Obs data set # ',i3,3x,a50)
		print 139
		if(pprt) write(7,139)
		if(discprt) write(8,139)
139		format(40x,
     &	 ' Comp #      tau (ms)     area')
		do i=1,ncomp(j)
		   print 91,i,tau(i,j),area(i,j)
		   if(pprt)write(7,91) i,tau(i,j),area(i,j)
		   if(discprt)write(8,91) i,tau(i,j),area(i,j)
c91		   format(40x,i4,3x,2(2x,G13.6))
		enddo
c
		if(idprt.eq.2) then
		 print 48
      	 if(pprt) write(7,48)
	       if(discprt) write(8,48)
48		 format(/,
     &	  ' time (ms)      probability density         residual',
     &							     '       percent',/,
     &	  '                observed     fitted')
		 do i=1,ntime(j)
		   t=times(i,j)
		   call pdf_FL_p(Q0,Qx,p0,t,ft,tp,Prns,tauc,areac,
     &		tauc1,areac1,bad,km)	!call with t>0 to get f(t)=ft
		   if(bad) then
			print 10
		      if(discprt) write(8,10)
c10			format(' Error in PDF_FL_P in SSDMF')
			abort=.true.	!in case called fron simplex
			RETURN
		   endif
		   Ycalc(i)=ft
		   res=Yobs(i,j)-Ycalc(i)
		   if(abs(Yobs(i,j)).gt.1.e-20) then
		      res1=100.*res/Yobs(i,j)
		   else
			res1=0.0
		   endif
		   print 47,t,Yobs(i,j),Ycalc(i),res,res1
      	   if(pprt) write(7,47) t,Yobs(i,j),Ycalc(i),res,res1
	         if(discprt) write(8,47) t,Yobs(i,j),Ycalc(i),res,res1
c47		   format(1x,f9.2,3(2x,g13.6),2x,f9.2)
		 enddo
		endif
c
	   else if(id.ge.4.and.id.le.8) then	!other pdf
		if(id.eq.5) then	!shut time pdf -correct slow tau for Nchan
		   kpar=kpar+1
		   enchan=theta(kpar)
		   post='             '
		   if(jfix(kpar).eq.1) post='(fixed)'
c=		   n=nstau(j)	!correct the slowest n tau(i) values
c=		   do i=kF-n+1,kF
c=			tauc(i)=tauc(i)/enchan	!orig tauc is for 1 channel
c=		   enddo
		endif
c Print the results
	      print 36
	      if(pprt) write(7,36)
	      if(discprt) write(8,36)
c36	      format(
c     & 	' Comp #      tau (ms)     area     ')
		s=0.0
		do m=1,ncompc
		   print 1091,m,tauc(m),areac(m)
		   if(pprt)write(7,1091) m,tauc(m),areac(m)
		   if(discprt)write(8,1091) m,tauc(m),areac(m)
		   s=s + areac(m)
		enddo
	      print 43,s
      	if(pprt) write(7,43) s
	      if(discprt) write(8,43) s
43		format(' Total area = ',g13.6)
		if(id.eq.5) then
	         print 49, enchan,post
	         if(pprt) write(7,49) enchan,post
	         if(discprt) write(8,49) enchan,post
		endif
c
c  and print data for comparison
		print 92,j,titype(id)
		if(pprt) write(7,92) j,titype(id)
	      if(discprt) write(8,92) j,titype(id)
c92		FORMAT(/,40x,'  Obs data set # ',i3,3x,a50)
		print 139
		if(pprt) write(7,139)
		if(discprt) write(8,139)
c139		format(40x,
c     &	 ' Comp #      tau (ms)     area')
		do i=1,ncomp(j)
		   print 91,i,tau(i,j),area(i,j)
		   if(pprt)write(7,91) i,tau(i,j),area(i,j)
		   if(discprt)write(8,91) i,tau(i,j),area(i,j)
c91		   format(40x,i4,3x,2(2x,G13.6))
		enddo
c
		if(idprt.eq.2) then
c		 print 48
c      	 if(pprt) write(7,48)
c	       if(discprt) write(8,48)
		 print 481
	       if(discprt) write(8,481)
481		 format(/,
     &	  ' time (ms)   probability density       residual',
     &					 '     weight     w*res*res/n',/,
     &	  '              observed     fitted')
		 ss=0.0
		 do i=1,ntime(j)
		   s=0.0
		   t=times(i,j)
		   do m=1,ncompc
		      a=areac(m)/tauc(m)
c			s=s + a*exp(-t/tauc(m))		!may underflow!
		      s=s + AEXPS1(a,-t/tauc(m))	!avoids underflow
		   enddo
		   Ycalc(i)=s
		   res=Yobs(i,j)-Ycalc(i)
c		   if(abs(Yobs(i,j)).gt.1.e-20) then
c		      res1=100.*res/Yobs(i,j)
c		   else
c			res1=0.0
c		   endif
c		   print 47,t,Yobs(i,j),Ycalc(i),res,res1
c      	   if(pprt) write(7,47) t,Yobs(i,j),Ycalc(i),res,res1
c	         if(discprt) write(8,47) t,Yobs(i,j),Ycalc(i),res,res1
cc47		   format(1x,f9.2,3(2x,g13.6),2x,f9.2)
		   w=weight(i,j)
		   res1=w*res*res/float(ntime(j))
		   ss=ss + res1
		   print 472,t,Yobs(i,j),Ycalc(i),res,w,res1
      	   if(pprt) write(7,472) t,Yobs(i,j),Ycalc(i),res,w,res1
	         if(discprt) write(8,472) t,Yobs(i,j),Ycalc(i),res,w,res1
472		   format(1x,f9.2,5(g13.6))
		 enddo
		 print 471,ss
	       if(discprt) write(8,471) ss
471		  format(' SUM(w*res*res) = ',g13.6)
		endif
	   endif
c
c Section to print values for each set
26	   if(debprt) then
		t1=-1.
		print 21
21		format(
     &	' Print values from t=t1 to t2 (ms): t1, t2 [skip] = ')
		call INPUT2r(t1,t2)
		printj=.false.
		if(t1.ge.0.) then
		   printj=.true.
		   print 22
22		   format('& time step (ms) = ')
		   call INPUTr(dt)
		endif
	   endif
	   if(id.eq.1.and.printj) then	!pulse
		t=t1
		tp=tpulse(j)	!pulse duration in milliseconds
		do while(t.le.t2)
		   if(t.le.tp) then
			ycal=CURRENT(t,amps,tauc,env,gamma,Yinfc,
     &  			kA,k,km)
		   else
			Ycal=CURRENT(t-tp,amps1,tauc1,env,gamma,Yinfc1,
     &  			kA,k,km)
		   endif
	         print 25,t,Ycal
      	   if(pprt) write(7,25) t,Ycal
	         if(discprt) write(8,25) t,Ycal
25		   format(1x,f9.2,2x,g13.6)
		   t=t + dt
		enddo
	   else if(id.eq.2.and.printj) then	!step
		t=t1
		do while(t.le.t2)
		   Ycal=CURRENT(t,amps,tauc,env,gamma,Yinfc,kA,k,km)
	         print 25,t,Ycal
      	   if(pprt) write(7,25) t,Ycal
	         if(discprt) write(8,25) t,Ycal
		   t=t + dt
		enddo
	   else if(id.eq.3.and.printj) then	!pdf for first latency after pulse
		t=t1
		do while(t.le.t2)
		   call pdf_FL_p(Q0,Qx,p0,t,ft,tp,Prns,tauc,areac,
     &		tauc1,areac1,bad,km)	!call with t>0 to get f(t)=ft
		   if(bad) then
			print 10
		      if(discprt) write(8,10)
c10			format(' Error in PDF_FL_P in SSDMF')
			abort=.true.	!in case called fron simplex
			RETURN
		   endif
		   Ycal=ft
	         print 25,t,Ycal
      	   if(pprt) write(7,25) t,Ycal
	         if(discprt) write(8,25) t,Ycal
		   t=t + dt
		enddo
	   else if(id.ge.4.and.id.le.6.and.printj) then	!other pdf
		t=t1
		do while(t.le.t2)
		   s=0.0
		   do m=1,ncompc
		      a=areac(m)/tauc(m)
c			s=s + a*exp(-t/tauc(m))		!may undeflow
		      s=s + AEXPS1(a,-t/tauc(m))	!avoids underflow
		   enddo
		   Ycal=s
	         print 25,t,Ycal
      	   if(pprt) write(7,25) t,Ycal
	         if(discprt) write(8,25) t,Ycal
		   t=t + dt
		enddo
	   endif
	   if(printj) goto 26	!print more values?
c
	enddo		!end of loop for sets
c
	RETURN
	end


