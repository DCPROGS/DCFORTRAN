	subroutine DPULSE(npulse,tpgap,QT,ncdep,nlig,ligname,
     &	IL,IX,JX,conc00,conc0,conc,nlvar,vhold,vref,
     &	dgamma,cfac,bound,nbound,tpulse,
     &  	ncal1,k,km,qprt,xcal,ycal,ntime,dt,ndc1,ndimc,iprint)
c
c To calculate response to a series of pulses in SCALCS
c Pre-equilibrate with conc=xA0 (often zero) and superimpose
c pulses on this background
c Cannot keep all of the coefficients so must calculate Xcal, Ycal here
c
c Modif 03/04/01 08:05pm for 100 states/10 ligands
c Remove args cA1,cB1,xA00,xA0,xA1
c Add args ligname,conc00,conc0,conc,nlvar,vhold,vref
c
c  Add option for v-jumps? -for these need volt00,volt0 and volt1()
c
c Allocation to columns is different from other cases
c For nlig=1
c   j=1 current
c   j=2 Popen
c   j=3 binding
c   j=4 to j=3+k =occupancy of all k states (state #=j-3)
c
c For nlig=2
c   j=1 current
c   j=2 Popen
c   j=3 binding of ligand 1
c   j=4 binding of ligand 2
c   j=5 to j=4+k =occupancy of all k states (state #=j-4)
c
c For nlig=n (up to 10)
c   j=1 current
c   j=2 Popen
c   j=3 to 3+nlig-1 binding of ligand number j-2
c   j=3+nlig to j=3+nlig+k-1=2+nlig+k=occupancy of all k states (state #=j-2-nlig)
c      (in this case maximum j required is 2+nlig+k =eg for nlig=4 and k=36
c       need j up 42 -rather big!)
c
c	subroutine RELOUT(cfac,dgamma,pinf,bmj,eigen,tpulse,bm,tau,
c     & cur0,curinf,cur0tot,bmrel,atotd,pon)
c relout prints details of current relaxation in SCALCS. All param up to
c tpulse are inputs, rest are outputs.
c
	REAL*8 QT(100,100)
	real*8 p0(100),pt(100),pt0(100),dgamma(100)
c	real*8 cfac1
c	real*8 a0
	real*4 tjump
	real*8 poptot
c	real*8 pop(100),btot,bmb(100),tjumpd
	real*8 ctot
	real*8 t1
	real*8 bound(100,10),bmax(10)
	integer nbound(100,10)
	character*20 ligname(10)
	real*4 conc0(10),conc00(10),conc(10)
c For conc=xA00
	real*8 pinf00(100),eigen00(100),Q00(100,100),pimat00(100,100)
	real*8 Amat00(100,100,100)
c For conc=xA0
	real*8 pinf0(100),eigen0(100),Q0(100,100),pimat0(100,100)
	real*8 Amat0(100,100,100)
c For conc=xA1
	real*8 pinf1(100),eigen1(100),Q1(100,100),pimat1(100,100)
	real*8 Amat1(100,100,100)
c For both conc
	real*8 pinf(100),eigen(100)
	real*8 Amat(100,100,100)
	real*8 bm(100),bmj(100,100)
	real*8 curinf,cur0(100),tau(100),curtot
c
	real*4 Xcal(ndc1,ndimc),Ycal(ndc1,ndimc)
	character*1 ans,UC
	integer IX(100),JX(100),IL(100)		!NB 'ix' is used by RANDOM
	logical Onrel
	LOGICAL pon,debug,qprt,discprt
	logical slock,caplock
	common/KBLK/kA,kB,kC,kD
	common/dp/discprt
c
	pon()=slock()
	debug()=caplock()
c
c Have only two conc -xA0=background conc, and xA1=pulse conc, so get equilib
c occs and Qmatrix for these two in pinf0, pinf1 and Q0, Q1 respectively
c before starting the loop so need not keep recalculating them
c and in fact can calc eigenvalues and eigenvectors for these two Q matrices
c in advance too -they don't change
c
c	cfac1=dble(cfac)
	do n=1,nlig
	   bmax(n)=-1.d200
	   do i=1,k
		bound(i,n)=dble(float(nbound(i,n)))
		if(bound(i,n).gt.bmax(n)) bmax(n)=bound(i,n)
	   enddo
	enddo
c
	nlvar=1
	if(nlig.gt.1) then
652	   do i=1,nlig
		print 651,i,ligname(i)
651		format(' Ligand (',i2,'): ',a10)
	   enddo
	endif
	print 1,nlvar
1	format(
     & ' Ligand number for which conc is to be pulsed [',i2,'] = ')
	call INPUTi(nlvar)
c now check on the conc off all the other ligands (at present these are
c same throughout, but no need to be -could ask for pre/post jump conc
c of each
	print 2
2	format(
     &' Now specify the (fixed) concentrations of the other ligands')
	do i=1,nlig
	   if(i.ne.nlvar) then	!all the rest
		x=1.e6*conc0(i)
		print 664,i,ligname(i),x
664		format(' (Fixed) conc of ligand ',i2,' (',
     &      a10,') BEFORE jump (micromolar) [',g10.4,'] = ')
		call INPUTr(x)
		conc0(i)=1.e-6*x

		x=1.e6*conc(i)
		print 665,i,ligname(i),x
665		format(' (Fixed) conc of ligand ',i2,' (',
     &      a10,') AFTER jump (micromolar) [',g10.4,'] = ')
		call INPUTr(x)
		conc(i)=1.e-6*x
	   endif
	enddo
	xA00=conc00(nlvar)*1.e6	!micromolar for input
	xA0=conc0(nlvar)*1.e6	!micromolar for input
	xA1=conc(nlvar)*1.e6	!micromolar for input
	tpulse=tpulse*1000.		!in ms
c
	print 312,ligname(nlvar)
	if(discprt) write(8,312)
312	format(
     &' The receptors are initially at equilibrium with a specified',/,
     &'  concentration of ',a10,' before the first pulse.',/,
     &' After the first pulse, all pulses are superimposed',/,
     &'  on a constant backround concentration (not necessarily',/,
     &'  the same as the initial conc).')
	print 316,xA00
316	format(
     & ' Initial concentration (micromolar) [',f9.4,'] = ')
	call INPUTr(xA00)
	print 313,xa1
313	format(
     & '& Concentration during pulse (micromolar) [',f11.5,'] = ')
	call INPUTr(xA1)
	print 317,xA0
317	format(
     & ' Background conc. after each pulse (micromolar) [',f9.4,'] = ')
	call INPUTr(xA0)
	print 315,tpulse
315	format(
     & '& Duration of the pulse (ms) [',f7.2,'] = ')
	call INPUTr(tpulse)
c Print what is done
      print 61,npulse,xA00,xa0,xA1,tpulse,tpgap*1000.
      if(pon()) write(7,61) npulse,xA00,xa0,xA1,tpulse,tpgap*1000.
      if(discprt) write(8,61) npulse,xA00,xa0,xA1,tpulse,tpgap*1000.
61	format(/,1x,
     & ' Train of ',i3,' pulses ',/,
     & ' Initial conc (muM)  = ',g11.4,/,
     & ' Background conc after each pulse (muM) = ',g11.4,/,
     & ' Concentration during pulse (muM) = ',g11.4,/,
     & ' Pulse duration (ms) = ',g11.4,/,
     & ' Time from start of one pulse to start of next (ms) = ',g11.4,/,
     & ' -----------------------------------------------------------')
c
	conc00(nlvar)=xA00*1.e-6
	conc0(nlvar)=xA0*1.e-6
	conc(nlvar)=xA1*1.e-6
	tpulse=tpulse*0.001		!in seconds
c
c First do calculations at conc=xA00
c
c===	call QNEWC(QD,cA1,cB1,Q00,xA00,xB00,ncdep,nlig,IL,IX,JX,k,km)

	V1=Vhold-Vref		!V1 dec with hyperpol- define for QSETD

	if(debug()) call ATYPD(QT,'  QT   ' ,k,k,km,km)
	call QSETD(conc00,IL,V1,QT,Q00,k,.false.)
	call EQOCCd(Q00,k,k-1,km,Pinf00)		!calc equilib occs at xA00
	if(qprt) then
         print 50,xa00*1.e6
         if(pon()) write(7,50) xa00*1.e6
         if(discprt) write(8,50) xa00*1.e6
c50	   format(/,' Q matrix at concentration = ',g13.6)
	   call ATYPD(Q00,' Q(00) ' ,k,k,km,km)
	   call CALCPI(Q00,PImat00,k,km,km)
	   call ATYPD(pimat00,' PI(00) ',k,k,km,km)
	endif
	if(iprint.ge.2) then
	   print 51,xa00*1.e6
	   if(pon()) write(7,51) xa00*1.e6
	   if(discprt) write(8,51) xa00*1.e6
c51	   format(/,
c     & ' Equilib occupancies at concentration = ',g13.6)
	   do j=1,k
		print 533,j,pinf00(j)
	      if(pon()) write(7,533) j,pinf00(j)
	      if(discprt) write(8,533) j,pinf00(j)
533		format(' pinf00(',i2,') = ',g13.6)
	   enddo
	   if(iprint.ge.3) then
		call SLIFED2(Q00,Pinf00,pon(),discprt,km,km)
	   endif
	endif
	call QMAT5(Q00,Amat00,k,eigen00,ibad,km,km,km)
	if(ibad.ne.0) print 14,ibad
c14	format(' ***ERROR IN SPECTRAL EXPANSION: ibad= ',i4)
	if(debug()) then
	   print 700
c700	   format(' Print spectral expansion matrices [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	   if(UC(ans).eq.'Y') then
		do m=1,k
		   call ATYPD3(amat,m,'A matrix',k,k,km,km,km)
		enddo
	   endif
	endif
c
c Now do calculations at conc=xA0
c
	call QSETD(conc0,IL,V1,QT,Q0,k,.false.)
c===	call QNEWC(QD,cA1,cB1,Q0,xA0,xB0,ncdep,nlig,IL,IX,JX,k,km)
	call EQOCCd(Q0,k,k-1,km,Pinf0)		!calc equilib occs at xA00
	if(qprt) then
         print 50,xa0*1.e6
         if(pon()) write(7,50) xa0*1.e6
         if(discprt) write(8,50) xa0*1.e6
50	   format(/,' Q matrix at concentration = ',g13.6)
	   call ATYPD(Q0,'  Q(0)  ',k,k,km,km)
	   call CALCPI(Q0,PImat0,k,km,km)
	   call ATYPD(pimat0,'  PI(0) ',k,k,km,km)
	endif
	if(iprint.ge.2) then
	   print 51,xa0*1.e6
	   if(pon()) write(7,51) xa0*1.e6
	   if(discprt) write(8,51) xa0*1.e6
51	   format(/,
     & ' Equilib occupancies at concentration = ',g13.6)
	   do j=1,k
		print 53,j,pinf0(j)
	      if(pon()) write(7,53) j,pinf0(j)
	      if(discprt) write(8,53) j,pinf0(j)
53		format(' pinf0(',i2,') = ',g13.6)
	   enddo
	   if(iprint.ge.3) then
		call SLIFED2(Q0,Pinf0,pon(),discprt,km,km)
	   endif
	endif
	call QMAT5(Q0,Amat0,k,eigen0,ibad,km,km,km)
	if(ibad.ne.0) print 14,ibad
14	format(' ***ERROR IN SPECTRAL EXPANSION: ibad= ',i4)
	if(debug()) then
	   print 700
700	   format(' Print spectral expansion matrices [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	   if(UC(ans).eq.'Y') then
		do m=1,k
		   call ATYPD3(amat,m,'A matrix',k,k,km,km,km)
		enddo
	   endif
	endif
c
c Now do calculations at conc=xA1
c
c===	call QNEWC(QD,cA1,cB1,Q1,xA1,xB1,ncdep,nlig,IL,IX,JX,k,km)
	call QSETD(conc,IL,V1,QT,Q1,k,.false.)
	call EQOCCd(Q1,k,k-1,km,Pinf1)		!calc equilib occs at xA1
	if(qprt) then
         print 50,xa1*1.e6
         if(pon()) write(7,50) xa1*1.e6
         if(discprt) write(8,50) xa1*1.e6
c50	   format(/,' Q matrix at concentration = ',g13.6)
	   call ATYPD(Q1,'  Q(1)  ' ,k,k,km,km)
	   call CALCPI(Q1,PImat1,k,km,km)
	   call ATYPD(pimat1,'  PI(1) ',k,k,km,km)
	endif
	if(iprint.ge.2) then
	   print 51,xa1*1.e6
	   if(pon()) write(7,51) xa1*1.e6
	   if(discprt) write(8,51) xa1*1.e6
c51	   format(/,
c     & ' Equilib occupancies at concentration = ',g13.6)
	   do j=1,k
		print 531,j,pinf1(j)
	      if(pon()) write(7,531) j,pinf1(j)
	      if(discprt) write(8,531) j,pinf1(j)
531		format(' pinf1(',i2,') = ',g13.6)
	   enddo
	   if(iprint.ge.3) then
		call SLIFED2(Q1,Pinf1,pon(),discprt,km,km)
	   endif
	endif
	call QMAT5(Q1,Amat1,k,eigen1,ibad,km,km,km)
	if(ibad.ne.0) print 14,ibad
c14	format(' ***ERROR IN SPECTRAL EXPANSION: ibad= ',i4)
	if(debug()) then
	   print 700
c700	   format(' Print spectral expansion matrices [N] ? ')
	   ans='N'
	   call INPUTa(ans)
	   if(UC(ans).eq.'Y') then
		do m=1,k
		   call ATYPD3(amat,m,'A matrix',k,k,km,km,km)
		enddo
	   endif
	endif
c Do the on jump of pulse and get state at end of pulse as initial cond
c    for gap between pulses
c
c Set up for the first (ON) jump
	ipulse=1		!count the pulses
	iobs=0		!count observations in Ycal, Xcal
	ilast=1
	tstart=0.
c	xmin=tstart
	ONrel=.true.	!on jump first
	do i=1,k
	   p0(i)=pinf00(i)		!init vector for first jump -xA00 for first
	enddo
c RETURN TO 100 FOR NEXT PULSE
100	continue
c

c ON-JUMP FOR PULSES (+CALC OF INTIAL CONDITION FOR OFF-JUMP)
c To calc occupancies p(t) at time t, in pt(j), given initial occs in p0,
c final occs in pinf, and the spectral expansion of Q for the post-jump
c conditions in eigen,amat. Returns also the coefficients b(m,j) of
c the mth exponential component for p(j), so once this is calc
c can calc new p(t), for range of t, quickly as
c pj(t)=pj(inf) + sum[b(m,j)*dexp(eigen(m)*t)]; sum from m=2,...,k
c	subroutine PTCALC(pt,t,bmj,p0,pinf,eigen,amat,k,km)
	if(ONrel) then
	   tjump1=tpulse
	   tjump=1.e3*(tpulse)	!gap between jumps in msec
	   tstart=float(ipulse-1)*tpgap*1000.	!in msec
	   tstart1=tstart
c	   tjumpd=dble(tjump)
	   do i=1,k
		eigen(i)=eigen1(i)
		pinf(i)=pinf1(i)
		do j=1,k
		   do m=1,k
			amat(i,j,m)=amat1(i,j,m)
		   enddo
		enddo
	   enddo
	   print 331,ipulse
         if(pon()) write(7,331) ipulse
	   if(discprt) write(8,331) ipulse
331	   format(/,' ON-relaxation: pulse ',i3)
	else
	   tjump1=tpgap-tpulse
	   tjump=1.e3*(tpgap-tpulse)	!gap between jumps in msec
c	   tjumpd=dble(tjump)
	   tstart=tstart+1000.*tpulse
	   do i=1,k
		eigen(i)=eigen0(i)
		pinf(i)=pinf0(i)
		do j=1,k
		   do m=1,k
			amat(i,j,m)=amat0(i,j,m)
		   enddo
		enddo
	   enddo
	   print 332,ipulse
         if(pon()) write(7,332) ipulse
	   if(discprt) write(8,332) ipulse
332	   format(/,' OFF-relaxation: pulse ',i3)
	endif
	call PTCALC(pt0,tjump1,bmj,p0,pinf,eigen,amat,k,km)
	do j=1,k
	    p0(j)=pt0(j)	!new initial occs (for OFF jump of pulse)
	enddo
	if(iprint.ge.2) then
	   print 581,ipulse,tjump,xa1*1.e6
         if(pon()) write(7,581) ipulse,tjump,xa1*1.e6
	   if(discprt) write(8,581) ipulse,tjump,xa1*1.e6
581	   format(/,
     &' Pulse ',i3,' Occupancies at t = ',g13.6,' ms at conc = ',g13.6)
	   do j=1,k
	      print 60,j,p0(j)
      	if(pon()) write(7,60) j,p0(j)
		if(discprt) write(8,60) j,p0(j)
60		format(' p0(',i2,') = ',g13.6)
	   enddo
	endif
c
c For pulses print tau etc for the ON relaxation (use coeffs in bmj0() found
c above in PTCALC)
c	subroutine RELOUT(cfac,dgamma,pinf,bmj,eigen,tpulse,bm,tau,
c     & cur0,curinf,cur0tot,bmrel,atotd,ligname,pon)
c Relout prints details of current relaxation in SCALCS. All param up to
c tpulse are inputs, rest are outputs.
	call RELOUT(cfac,dgamma,pinf,bmj,eigen,tpulse,bm,tau,
     &  cur0,curinf,curtot,bmrel,atotd,nlig,ligname,pon())
c calculate area (including asymptote term) from t=0 to t=tpulse
c===modify for off rel
c	a0=0.0d0
c	do m=1,k-1
c	   a0=a0 + cur0(m)*tau(m)*(1.d0 - dexp1(-tjumpd/tau(m)))
c	enddo
c	a0=a0 + curinf*tjumpd		!add asymptote term
c
c For pulse, define values for ON relaxation of current
c	curinf=0.0d0
c	popinf=0.0d0
c	do j=1,kA
c	   curinf=curinf+cfac1*dgamma(j)*pinf(j)
c	   popinf=popinf+pinf(j)
c	enddo
c	curtot=0.0d0
c	do m=1,k-1	!define bm0 etc for ON relaxation
c         tau(m)=-1.d3/eigen(m)		!for pulse
c	   bm(m)=0.0d0
c	   do j=1,kA
c	 	bm(m)=bm(m) + dgamma(j)*bmj(m,j)
c	   enddo
c	   cur0(m)=cfac1*bm(m)
c	   curtot=curtot + cur0(m)
c	enddo
cc Do same for Popen (tau are same)
c	poptot=0.0d0
c	do m=1,k-1	!define bm0 etc for ON relaxation
c	   pop(m)=0.0d0
c	   do j=1,kA
c	 	pop(m)=pop(m) + bmj(m,j)
c	   enddo
c	   poptot=poptot + pop(m)
c	enddo
cc Do same for binding
c	btot=0.0d0
c	do m=1,k-1	!define bm0 etc for ON relaxation
c	   bmb(m)=0.0d0
c	   do j=1,k
c	 	bmb(m)=bmb(m) + bound(j)*bmj(m,j)
c	   enddo
c	   bmb(m)=bmb(m)/bmax
c	   btot=btot + bmb(m)
c	enddo
c
c Current in j=1
	dx=tjump/float(ncal1-1)		!ncal1 points in msec
	do i=1,ncal1
	   iobs=iobs+1
	   t=float(i-1)*dx
	   tabs=tstart + t	!time for graph
c==	   do j=1,k+3		!times same for all graphs
	   do j=1,2+nlig+k	!times same for all graphs
	      Xcal(iobs,j)=tabs
	      Ycal(iobs,j)=0.0
	   enddo
	   t1=dble(t)		!time for calculation
c
c To define all state occs and to define binding need all k states
c For nlig=n (up to 10)
c   j=1 current
c   j=2 Popen
c   j=3 to 3+nlig-1 binding of ligand number #j-2
c   j=3+nlig to j=3+nlig+k-1=2+nlig+k=occupancy of all k states (state #=j-2-nlig)
c      (in this case maximum j required is 2+nlig+k =eg for nlig=4 and k=36
c       need j up 42 -rather big!)
	   do j=1,k
	      pt(j)=0.0d0
	      do m=1,k-1
	   	   pt(j)=pt(j) + bmj(m,j)*dexp(-t1/tau(m))	!component
	      enddo
	      pt(j)=pt(j)+pinf(j)
		do n=1,nlig
	         Ycal(iobs,n+2)=Ycal(iobs,n+2)+sngl(bound(j,n)*pt(j))   !binding in j=3,..
		enddo
		Ycal(iobs,j+nlig+2)=sngl(pt(j))	!state occs in j=3+nlig,..,
c=========old code for nlig=1 or 2
c=		if(nlig.eq.1) then
c=	         Ycal(iobs,3)=Ycal(iobs,3)+sngl(bound(j,1)*pt(j))   !binding in j=3
c=		   Ycal(iobs,j+3)=sngl(pt(j))	!state occs in j=4,..,
c=		else if(nlig.eq.2) then
c=	         Ycal(iobs,3)=Ycal(iobs,3)+sngl(bound(j,1)*pt(j))   !binding in j=3
c=	         Ycal(iobs,4)=Ycal(iobs,4)+sngl(bound(j,2)*pt(j))   !binding in j=3
c=		   Ycal(iobs,j+4)=sngl(pt(j))	!state occs in j=4,..,
c=		endif
	   enddo		!end of j=1,k
c
c convert all occupancies to get fractional occ of all sites
	   do n=1,nlig
		Ycal(iobs,2+n)=Ycal(iobs,2+n)/sngl(bmax(n))
	   enddo
c
c To define current and Popen need only the open states
	   ctot=0.0d0
	   poptot=0.0d0
c this version wrong! All kA states already incorporated in cur0(m) (in RELOUT)
c	   do j=1,kA
c		poptot=poptot + pt(j)	!sum of open state occupancies already calc
c		do m=1,k-1		!the k-1 components; j=index for Ycal
c		   ctot=ctot + cur0(m)*dexp(-t1/tau(m))
c		enddo
c	   enddo
c correct version
	   do j=1,kA
		poptot=poptot + pt(j)	!sum of open state occupancies already calc
	   enddo
c
	   do m=1,k-1		!the k-1 components; j=index for Ycal
		ctot=ctot + cur0(m)*dexp(-t1/tau(m))
	   enddo
	   Ycal(iobs,1)=sngl(ctot + curinf)		!total current
	   Ycal(iobs,2)=sngl(poptot)			!total Popen
	enddo		!end of i=1,ncal1

c For calculations, last point is at t=tpulse for on jump, and
c at t=tpgap-tpulse=gap length for off jump
c
c
c NOW DEFINE OFF relaxation usining Amat0 etc already calc
c
	if(ONrel) then
	   ONrel=.false.	!now loop back for OFF jump that follows pulse #ipulse
	   goto 100
	else
	   ONrel=.true.	!now loop back for next ON jump
c        Before next pulse, find the max response to the current pulse
	   cmin=0.0
	   do i=ilast,iobs
		y=Ycal(i,1)
		if(y.lt.cmin) then
		   cmin=y
		   tmin=xcal(i,1)-tstart1
		endif
	   enddo
         print 52,ipulse,cmin,tmin
         if(pon()) write(7,52) ipulse,cmin,tmin
         if(discprt) write(8,52) ipulse,cmin,tmin
52	   format(
     & ' Peak current (from calculated points -no interpolation yet)',/,
     & ' Pulse ',i3,': peak = ',g11.4,' pA at t = ',g11.4,' ms')
	   ilast=iobs
	   ipulse=ipulse+1
	   if(ipulse.le.npulse) goto 100		!DO NEXT
	endif
c
c All pulses now calculated, and xcal, ycal defined
	ntime=iobs
c
	RETURN
	end


