	subroutine DPULSE(npulse,tpgap,QD,cA1,cB1,ncdep,nlig,IL,IX,JX,
     &	dgamma,cfac,bound,nbound,tpulse,xA00,xA0,xA1,
     &  	ncal1,k,km,qprt,xcal,ycal,ntime,dt,ndc1,ndimc,iprint)
c To calculate response to a series of pulses in SCALCS
c Pre-equilibrate with conc=xA0 (often zero) and superimpose
c pulses on this background
c Cannot keep all of the coefficients so must calculate Xcal, Ycal here
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
c	subroutine RELOUT(cfac,dgamma,pinf,bmj,eigen,tpulse,bm,tau,
c     & cur0,curinf,cur0tot,bmrel,atotd,pon)
c relout prints details of current relaxation in SCALCS. All param up to
c tpulse are inputs, rest are outputs.
c
	REAL*8 QD(10,10)
	real*8 p0(10),pt(10),pt0(10),dgamma(10)
c	real*8 cfac1
c	real*8 a0
	real*4 tjump
	real*8 poptot
c	real*8 pop(10),btot,bmb(10),tjumpd
	real*8 ctot
	real*8 t1
	real*8 bound(10,2),bmax(2)
	integer nbound(10,2)
c For conc=xA00
	real*8 pinf00(10),eigen00(10),Q00(10,10),pimat00(10,10)
	real*8 Amat00(10,10,10)
c For conc=xA0
	real*8 pinf0(10),eigen0(10),Q0(10,10),pimat0(10,10)
	real*8 Amat0(10,10,10)
c For conc=xA1
	real*8 pinf1(10),eigen1(10),Q1(10,10),pimat1(10,10)
	real*8 Amat1(10,10,10)
c For both conc
	real*8 pinf(10),eigen(10)
	real*8 Amat(10,10,10)
	real*8 bm(10),bmj(10,10)
	real*8 curinf,cur0(10),tau(10),curtot
c
	real*4 Xcal(ndc1,ndimc),Ycal(ndc1,ndimc)
	character*1 ans,UC
	integer IX(10),JX(10),IL(10)		!NB 'ix' is used by RANDOM
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
c and in fact can calc eigebvalues and eigenvectors for these two Q matrices
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
	xA00=xa00*1.e6	!micromolar for input
	xA0=xa0*1.e6	!micromolar for input
	xA1=xa1*1.e6	!micromolar for input
	tpulse=tpulse*1000.		!in ms
c
	print 312
	if(discprt) write(8,312)
312	format(
     &' The receptors are initially at equilibrium with a specified',/,
     &' before the first pulse. After the first pulse, all pulses',/,
     &' are superimposed on a constant backround concentration',/,
     &' (not necessarily the same as the initial conc).')
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
	xA00=xA00*1.e-6
	xA0=xA0*1.e-6
	xA1=xA1*1.e-6
	tpulse=tpulse*0.001		!in seconds
c
c First do calculations at conc=xA00
c
	if(debug()) call DATYP(QD,'  QD    ' ,pon(),k,k,km,km)
	call QNEWC(QD,cA1,cB1,Q00,xA00,xB00,ncdep,nlig,IL,IX,JX,k,km)
	call EQOCCd(Q00,k,k-1,km,Pinf00)		!calc equilib occs at xA00
	if(qprt) then
         print 50,xa00*1.e6
         if(pon()) write(7,50) xa00*1.e6
         if(discprt) write(8,50) xa00*1.e6
c50	   format(/,' Q matrix at concentration = ',g13.6)
	   call DATYP(Q00,' Q(00) ' ,pon(),k,k,km,km)
	   call CALCPI(Q00,PImat00,k,km,km)
	   call DATYP(pimat00,' PI(00)' ,pon(),k,k,km,km)
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
		   call DATYP3(amat,m,'A matrix',pon(),k,k,km,km)
		enddo
	   endif
	endif
c
c Now do calculations at conc=xA0
c
	if(debug()) call DATYP(QD,'  QD    ' ,pon(),k,k,km,km)
	call QNEWC(QD,cA1,cB1,Q0,xA0,xB0,ncdep,nlig,IL,IX,JX,k,km)
	call EQOCCd(Q0,k,k-1,km,Pinf0)		!calc equilib occs at xA00
	if(qprt) then
         print 50,xa0*1.e6
         if(pon()) write(7,50) xa0*1.e6
         if(discprt) write(8,50) xa0*1.e6
50	   format(/,' Q matrix at concentration = ',g13.6)
	   call DATYP(Q0,' Q(0)  ' ,pon(),k,k,km,km)
	   call CALCPI(Q0,PImat0,k,km,km)
	   call DATYP(pimat0,' PI(0) ' ,pon(),k,k,km,km)
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
		   call DATYP3(amat,m,'A matrix',pon(),k,k,km,km)
		enddo
	   endif
	endif
c
c Now do calculations at conc=xA1
c
	call QNEWC(QD,cA1,cB1,Q1,xA1,xB1,ncdep,nlig,IL,IX,JX,k,km)
	call EQOCCd(Q1,k,k-1,km,Pinf1)		!calc equilib occs at xA1
	if(qprt) then
         print 50,xa1*1.e6
         if(pon()) write(7,50) xa1*1.e6
         if(discprt) write(8,50) xa1*1.e6
c50	   format(/,' Q matrix at concentration = ',g13.6)
	   call DATYP(Q1,' Q(1)  ' ,pon(),k,k,km,km)
	   call CALCPI(Q1,PImat1,k,km,km)
	   call DATYP(pimat1,' PI(1) ' ,pon(),k,k,km,km)
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
		   call DATYP3(amat,m,'A matrix',pon(),k,k,km,km)
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
c     & cur0,curinf,cur0tot,bmrel,atotd,pon)
c Relout prints details of current relaxation in SCALCS. All param up to
c tpulse are inputs, rest are outputs.
	call RELOUT(cfac,dgamma,pinf,bmj,eigen,tpulse,bm,tau,
     &  cur0,curinf,curtot,bmrel,atotd,nlig,pon())
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
	   do j=1,k+3		!times same for all graphs
	      Xcal(iobs,j)=tabs
	      Ycal(iobs,j)=0.0
	   enddo
	   t1=dble(t)		!time for calculation
c
c To define all state occs and to define binding need all k states
c If nlig=1, then Ycal(i,3)=binding for ligand 1, Ycal(i,j+3)=state occs
c If nlig=2, then Ycal(i,3)=binding for ligand 1, Ycal(i,4)=binding for
c    ligand 2, Ycal(i,j+4)=state occs
	   do j=1,k
	      pt(j)=0.0d0
	      do m=1,k-1
	   	   pt(j)=pt(j) + bmj(m,j)*dexp(-t1/tau(m))	!component
	      enddo
	      pt(j)=pt(j)+pinf(j)
		if(nlig.eq.1) then
	         Ycal(iobs,3)=Ycal(iobs,3)+sngl(bound(j,1)*pt(j))   !binding in j=3
		   Ycal(iobs,j+3)=sngl(pt(j))	!state occs in j=4,..,
		else if(nlig.eq.2) then
	         Ycal(iobs,3)=Ycal(iobs,3)+sngl(bound(j,1)*pt(j))   !binding in j=3
	         Ycal(iobs,4)=Ycal(iobs,3)+sngl(bound(j,2)*pt(j))   !binding in j=3
		   Ycal(iobs,j+4)=sngl(pt(j))	!state occs in j=4,..,
		endif
	   enddo
	   Ycal(iobs,3)=Ycal(iobs,3)/sngl(bmax(1))	!to get fractional occ of all sites
	   if(nlig.eq.2) then
	      Ycal(iobs,4)=Ycal(iobs,4)/sngl(bmax(2))	!to get fractional occ of all sites
	   endif
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


