	FUNCTION EXPLIK(k,THETA)
C
	REAL THETA(K)
	LOGICAL AMPOS,GAMDST,fastf,logt,excreg,constrained
c==	REAL R(10),A(10),W(10)	!LOCAL
	REAL*4 W(10)	!LOCAL
	integer jafix(10)
c for constraints when nset>1
	real*4 tau(10,10),area(10,10)
	integer itcon(10,10),ialoc(10,10),itloc(50)	!see below
	integer nyval(10),ncomp(10)
	integer nfit(10)
	real*4 ylow(10),yhigh(10)
	real*4 yexclow(10),yexchigh(10)
	integer*4 nset,neq,ieq(50),jeq(50),ieq0(50),jeq0(50)
	common/const/nset,neq,ieq0,jeq0,ieq,jeq,itcon,ialoc,itloc
	real*4 setlik(10)
	common/logliks/setlik	!likelihood for each set separately
c
	COMMON/FITBLK/yval(500000,10),xaxis(510),freq(510),Nyval,
     & ylow,yhigh,Ncomp,Nfit,ampos,pen,gamdst,isg,fastf,logt,nb1,nb2,
     & excreg,yexclow,yexchigh,isdcon,wfreq(51200),anfit,
     & nfitt,yvalt(5120),iconst
	common/FIXBLK/nfix,jfix(50)
c
c
c 06/03/89 06:33pm Lahey version of EXPLIK. Data now in COMMON
c Note even when call UNDER0 is in force can still get NDP underflow
c with numbers less than 3.4E-4932 (approx 3.4exp(-10115)) so this
c function can replace exp() to check
c
c Modif 12/05/00 02:17pm for simultaneous fits of nset sets of data
c with shared parameters.  At present this is done only for THIST and OBHIST
c and only tau (mean) can be shared, not areas.
c ncomp(j)=number of components for set j
c nfit(j)=number points to be fitted in set j
c ylow(j),yhigh(j) = fitted range
c tau(i,j), area(i,j) = mean,area for component i of set j (read guesses
c   into these and make initial theta() from them)
c neq=number of constraints tau(ieq(n),jeq(n))=tau(ieq0(n),jeq0(n)), n=1,neq
c Define 3 auxiliary arrays to speed up identification of elements of theta
c (1)itcon(i,j)
c   Each tau(i,j) is either
c   (a) free and so in theta(); for these itcon(i,j)=index in theta of tau(i,j)
c   (b) constrained, so not in theta; for these itcon(i,j)=0
c (2) ialoc(i,j)=index in theta() of area(i,j)
c (3) itloc(m)=0 if theta(m) is a time constant, =1 if it is an area, but =-1
c      for the last area in each set (# i=ncomp(j)-1)

c itcon=array to speed up identification of elements in theta()
c 	Each tau(i,j) is either
c (a) free and therefore in theta(); for these itcon(i,j)=index in theta of tau(i,j)
c (b) constrained, so not in theta; for these itcon(i,j)=0
c
c Modif 11/18/96 04:32pm for
c New constraints
c	iconst=1: 'Constrain f(0)=0 and f'(0)=>0'
c	iconst=2: 'Constrain f(0)=>0'
c	iconst=3: 'Constrain f(0)=>0 and f'(0)=>0'
c
c Version for EKDIS3 (file name= explk3) (Copy of this in EXP1 for overlay)
C DC version of EXPLK1 for 11-73. Aug 86.
C EXPLIK BUT WITH YVAL AS PARAMETER NOT IN COMMON(FOR EKDIST,PTSCH5)
C  **SPEEDED UP 14-APR-81 BY TAKING CALC OF D OUT OF LOOP
C CALCULATES THE MINUS LOG LIKELIHOOD FOR FITTING SUM
C OF NCOMP EXPONENTIALS TO RANDOM VARIABLE IN YVAL.
C 	DATA BETWEEN YLOW( =TLO) AND YHIGH (=THI) ARE USED
C	CONVENTION:PARAMETERS IN THETA ARE TAU(1),AREA(1)
C TAU(2),...,TAU(NCOMP-2),AREA(NCOMP-1).
C
C NEW VERSION FOR ANY NUMBER (NCOMP) OF EXPONENTIALS
c	DO 105 I=1,NFIT
c105	print 106,i,YVAL(I)
c106	FORMAT(' *',i8,G13.6)
c
	call UNDER0('true')	!so underflows set=0 (Lahey manual p12.8)
c
	IF(GAMDST) GOTO 36
c
c If(neq.gt.0) then check if the current tau is constrained -if so it is not
c present in theta so skip theta element and set tau(i,j) to be equal to
c whatever value is defined in ieq() etc -problem is that this value may
c not yet have been read from theta(), so must get all values from theta
c first
c Now corrected
c
c	m=0
c	do j=1,nset 	!log likelihood calc sep for each set, and summed at end
c	   do i=1,ncomp(j)
c		if(neq.gt.0) then		!check if the current tau is constrained
c		   do n=1,neq
c			if(i.eq.ieq(n).and.j.eq.jeq(n)) then
c			   tau(i,j)=tau(ieq0(n),jeq0(n))
c			   goto 21
c			endif
c		   enddo
c		endif
c		m=m+1
c		tau(i,j)=theta(m)
c21		continue
c		if(i.lt.ncomp(j)) then
c		   m=m+1
c		   area(i,j)=theta(m)
c		endif
c         enddo			!end of i=1,ncomp(j)
c	enddo			!end of j=1,nset
c
	if(nset.eq.1) then
	   m=0
	   do i=1,ncomp(1)
		m=m+1
		if(theta(m).lt.1.e-5) theta(m)=1.e-5	!0.01 microsec
		tau(i,1)=theta(m)
		if(i.lt.ncomp(1)) then
		   m=m+1
		   area(i,1)=theta(m)
		endif
	   enddo
	endif
c
c For nset>1, first set all the non-constrained tau(i) (and the areas,
c never constrained)
	if(nset.gt.1) then
	   m=0
	   do j=1,nset 	!log likelihood calc sep for each set, and summed at end
		do i=1,ncomp(j)
		   m=itcon(i,j)
		   if(m.ne.0) then
			tau(i,j)=theta(m)
		   endif
		   n=ialoc(i,j)
		   if(n.ne.0) then
			area(i,j)=theta(n)
		   endif
		enddo
	   enddo
c  now set the constrained tau(i,j)
	   m=0
	   do j=1,nset 	!log likelihood calc sep for each set, and summed at end
		do i=1,ncomp(j)
		   if(neq.gt.0) then		!check if the current tau is constrained
			do n=1,neq
			   constrained=i.eq.ieq(n).and.j.eq.jeq(n)
			   if(constrained) then
				tau(ieq(n),jeq(n))=tau(ieq0(n),jeq0(n))
			   endif
			enddo
		   endif
      	enddo		!end of i=1,ncomp(j)
	   enddo		!end of j=1,nset
	endif			!end of nset>1
c
c All tau(i,j) set, and all area(i,j) except for last -set that now for each set
c Now add likelihoods for each set individually
	explik=0.0	!initialise
c
	do j=1,nset
	   if(ncomp(j).eq.1) then
		area(1,j)=1.0
	   else
		sa=0.0	!total area exc last
		n=0
		if(nset.eq.1) then
		   do i=1,ncomp(j)
			if(tau(i,j).lt.1.0e-30) then
			   tau(i,j)=1.e-5		!0.01 microsec
			   m=itcon(i,j)
			   theta(m)=tau(i,j)	!reset corresponding theta
			endif
			if(ampos) then
			   if(area(i,j).gt.1.) area(i,j)=0.99999
			   if(area(i,j).lt.0.) area(i,j)=1.0e-6
			   if(i.lt.ncomp(j)) then	!last component not in theta
				m=ialoc(i,j)
				theta(m)=area(i,j)	!put altered value back in theta()
			   endif
			endif
			if(i.lt.ncomp(j)) then
			   sa=sa + area(i,j)
			endif
		   enddo
		else
		   do i=1,ncomp(j)
			if(itcon(i,j).ne.0) then	!tau free
			   if(tau(i,j).lt.1.0e-30) then
				tau(i,j)=1.e-5		!0.01 microsec
				m=itcon(i,j)
				theta(m)=tau(i,j)		!reset corresponding theta
			   endif
			endif
			if(ampos) then
			   if(area(i,j).gt.1.) area(i,j)=0.99999
			   if(area(i,j).lt.0.) area(i,j)=1.0e-6
			   if(i.lt.ncomp(j)) then	!last component not in theta
				m=ialoc(i,j)
				theta(m)=area(i,j)	!put altered value back in theta()
			   endif
			endif
			if(i.lt.ncomp(j)) then
			   sa=sa + area(i,j)
			endif
		   enddo	!end of i=1,ncomp(j)
c All free tau(ij), area(i,j) now defined. If any are constrained, set them now
		   if(neq.gt.0) then
			do n=1,neq
			   tau(ieq(n),jeq(n))=tau(ieq0(n),jeq0(n))
			enddo
		   endif		!end of if(nset,gt,1)
		endif
	   endif	!end of ncomp>1
c
C Each area (i=1,ncomp-1; sum=SA) is now between 0 and 1 if AMPOS. But if their
C sum is greater than 1 then scale all down so that their sum is 1, and
C thus AREA(ncomp)=0
c If an area has been fixed at a largish value (eg in iteration to get
c a likelihood interval for an area) then do not really want to sqeeze out
c AREA(ncomp) altogether: try scaling area(1)...area(ncomp) so they add
c to 0.99 and area(ncomp)=0.01
c NB Must NOT alter areas if they are FIXED parameters! To avoid this
c happening, firstly add all the fixed areas=safix, so have 0.99-safix as the
c total adjustable area; then add all the non-fixed areas=sadj, and scale
c them so sadj=0.99-safix.
c NB JAFIX,NAFIX defined by calling prog; nafix=number of fixed AREAS and
c JAFIX(i)=0,1 according as area(i), i=1,ncomp-1, is free, fixed resp.
c
c NB at present all areas are in theta() (whether free or fixed)

	   if(ampos) then
		nafix=0		!number of fixed areas (in current set j)
		if(nfix.gt.0) then		!some param fixed. Are any areas fixed?
		   do i=1,ncomp(j)-1
			jafix(i)=0
			m=ialoc(i,j)	!index in theta of area(i,j)
			if(jfix(m).eq.1) then
			   nafix=nafix+1	!number of fixed areas
			   jafix(i)=1	!area(i) is fixed
			endif
		   enddo
		endif		!end of nfix>0
		if(SA.ge.1.0) then
		   if(nafix.eq.0) then		!no fixed areas
			do i=1,ncomp(j)-1
			   area(i,j)=0.99*area(i,j)/SA
			   m=ialoc(i,j)
			   theta(m)=area(i,j)	!put altered value back in theta()
			enddo
			SA=0.99
		   else			!some areas fixed
			safix=0.0
			sadj=0.0
			do i=1,ncomp(j)-1
			   if(jafix(i).eq.0) then
				sadj=sadj+area(i,j)
			   else
				safix=safix+area(i,j)
			   endif
			enddo
			sa1=sadj/(0.99-safix)
			sa=0.0
			do i=1,ncomp(j)-1
			   if(jafix(i).eq.0) area(i,j)=area(i,j)/sa1
			   m=ialoc(i,j)
			   theta(m)=area(i,j)	!put altered value back in theta()
			   sa=sa+area(i,j)
			enddo
		   endif
		endif		!end if if(sa.gt.1.)
24	   endif	!end of 'if(ampos)'
c
	   area(ncomp(j),j)=1.0 - SA	!NOT A PARAMETER
c
c Define w(i) for current set
	   do i=1,ncomp(j)
		w(i)=area(i,j)/tau(i,j)		!w(i) for current set
	   enddo
C
c New constraints
c	iconst=1: 'Constrain f(0)=0 and f'(0)=>0'
c	iconst=2: 'Constrain f(0)=>0'
c	iconst=3: 'Constrain f(0)=>0 and f'(0)=>0'
c Insert constraint of f(0)=0 by calculating one of the tau values
c from the other parameters, say tau(1).
	   if(iconst.eq.1.and.ncomp(j).gt.1) then
		s=0.0
		do i=2,ncomp(j)
		   s=s+w(i)		!=f(0), exc 1st component
		enddo
		tau1=tau(1,j)/s
	      if(tau1.lt.1.e-30) tau1=1.e-5	!0.01 microsec
		m=itcon(1,j)
		theta(m)=tau1		!put result back into theta()
		w(1)=area(1,j)/tau1		!recalculate w(1)
	   endif
c
c Calculate denominator
	   d=0.0
	   do i=1,ncomp(j)
		w(i)=area(i,j)/tau(i,j)
		d=d + area(i,j)*(exp1(-ylow(j)/tau(i,j)) -
     &			exp1(-yhigh(j)/tau(i,j)))
	   enddo
c	   IF(D.LT.1.E-30) D=1.E-30
c
c Now calculate penalty factors
	   pf=0.0		!for f(0)
	   pf1=0.0		!for f'(0)
	   if(iconst.gt.0) then
		f0=0.
		f10=0.
		s=0.
		s1=0.
		do i=1,ncomp(j)
		   f0=f0+w(i)			!=f(0)
		   f10=f10-w(i)/tau(i,j)	!slope at t=0; w/tau=area/tau^2
		   s=s+abs(w(i))	!to scale f(0)
		   s1=s1+abs(w(i)/tau(i,j))	!to scale f1(0)
		enddo
		if(f0.lt.0.) then
		   pf=pen*abs(f0/s)
		else
		   pf=0.
	      endif
		if(f10.lt.0.) then
		   pf1=pen*abs(f10/s1)
		else
		   pf1=0.
		endif
	   endif
c
c Calc log(likelihood) for exponentials
	   if(fastf) goto 341
	   setlik(j)=0.0
	   do i=1,nfit(j)
		y=yval(i,j)
		t=0.
		do n=1,ncomp(j)
		   t=t + w(n)*exp1(-y/tau(n,j))
		enddo
		if(t.lt.1.e-30) t=1.e-30
		setlik(j)=setlik(j) - ALOG(t)
	   enddo
	   if(d.lt.1.e-37) then
		print 34,d
34		format(' ERROR in EXPLIK: d = ',g13.6)
		do i=1,ncomp(j)
		   print 35, i,area(i,j),w(i),tau(i,j)
35		   format(1x,i2,': area, w, tau = ',3g13.6)
	      enddo
	   endif
c
	   setlik(j)=setlik(j) + float(nfit(j))*ALOG(d)	!nfit is constant-but needed for errors
c
	   explik=explik + setlik(j)		!total likelihood to be maximized
c
	   if(iconst.eq.2) then
		if(pf.gt.0.) then
		   explik=explik + abs(setlik(j))*pf
		endif
	   else if(iconst.eq.1.or.iconst.eq.3) then
		if(pf1.gt.0.) then
		   explik=explik + abs(setlik(j))*pf1
		endif
	   endif
	enddo		!end of j=1,nset
	RETURN
c End of regular exponentials
c
c Section for log-lik for fast log-binned fit
341	s=0.
c	if(debug) print 800,fastf,nb1,nb2,D,tlo,thi,
c    &	(xaxis(i),i=1,8),(freq(i),i=1,8)
c800	format(l2,2i8,3g13.6,/,8f10.3,/,8f10.3)
	j=1		!defined only for nset=1
	do i=nb1,nb2
	   t=0.
	   do m=1,ncomp(j)
		a=area(m,j)
		r=1.0/tau(m,j)
		t=t+a*(exp1(-r*xaxis(i))-exp1(-r*xaxis(i+1)))
	   enddo
	   IF(T.LT.1.E-30) T=1.E-30
	   s=s-freq(i)*alog(t/d)
	enddo
	explik=s
	RETURN
C
C NOW SEP SECTION TO FIT GAMMA DISTRIBUTION (NCOMP=1) OR EQUIV WHEN
C OPEN TIME PDF IS SUM OF 2 EXP (SEE MLFIT1)
c isg=number if gaps/burst=number of open periods - 1
c Re-written 06/25/01 09:32am (see gamma-2exp.mcd and tgamma.for for tests)
c This section fixed only for nset=1
36	continue
c
	j=1
	if(ncomp(j).eq.1) then		!open times have one component
	   A=1.0
	   tau(1,j)=theta(1)
	   rate=1.0/tau(1,j)	!=1/MEAN LENGTH OF SINGLE OPENING
	   D=0.
	   n=isg+1				!number of openings
c  Cumulative form of gamma distribution is incomplete gamma function
c which gives Prob(t>T)
	   t1=ylow(j)
	   t2=yhigh(j)
	   D=GAMMAinc(t1,rate,n)-GAMMAinc(t2,rate,n)	!Prob[ylow < t < yhigh]
	   fact=1.0
	   do i=1,n-1
		fact=fact*float(i)	!factorial(n-1)
	   enddo
	   rn=rate**n
	   s=0.0
	   do i=1,nfit(j)
		t=yval(i,j)
		tn1=t**(n-1)
		pdf=rn*tn1*exp1(-rate*t)	!divide by fact outside loop
c==		pdf=gammapdf(t,rate,n)
		if(pdf.lt.1.e-30) pdf=1.e-30
		s=s - alog(pdf)
	   enddo
	   S=S + float(nfit(j))*alog(D*fact)
	else if(ncomp(j).eq.2) then		!open times have two components
	   if(theta(1).lt.1.e-30) theta(1)=1.e-4	!i.e. tau=0.1 musec
	   if(theta(3).lt.1.e-30) theta(3)=1.e-4	!i.e. tau=0.1 musec
	   R1=1./theta(1)
	   A1=theta(2)
	   R2=1./theta(3)
	   if(ampos) then
		if(A1.gt.1.) A1=0.99999
		if(A1.lt.0.) A1=1.0E-6
		theta(2)=A1
	   endif
	   A2=1.0-A1
	   e10=exp1(-R1*ylow(j))
	   e11=exp1(-R1*yhigh(j))
	   e20=exp1(-R2*ylow(j))
	   e21=exp1(-R2*yhigh(j))
c=======
	endif
	EXPLIK=S
	RETURN
c
c Original 1980s code!
c	IF(NCOMP(j).EQ.2) GOTO 38
c	A=1.0
c	R=1.0/tau(1,j)	!=1/MEAN LENGTH OF SINGLE OPENING
c	D=0.
c	DO 39 I=0,ISG
c	T=1.0
c	DO 524 n=1,I
c524	T=T*FLOAT(n)		!T=FACTORIAL(I)
c	D=D+(R**I)*((ylow(j)**I)*exp1(-R1*ylow(j))-
c     & (yhigh(j)**I)*exp1(-R1*yhigh(j)))/T
c39	CONTINUE
cC	IF(D.LT.1.E-30) D=1.E-30
cC END OF LOOP TO CALC D. ON EXIT T=FACTORIAL(ISG)
c	F=R/T	!=LAMBDA/ISG!  CONSTANT THROUGHOUT LOOP
c	S=0.
c	DO 37 I=1,NFIT(j)
c	Y=YVAL(I,j)
c	T=F*((R)**ISG)*exp1(-R*Y)
c	IF(T.LT.1.E-30) T=1.E-30
c	S=S-ALOG(T)
c37	CONTINUE
c	EXPLIK=S
c	RETURN
cC
cC NOW SECTION TO FIT CONVOLUTION OF TWO OPEN TIMES EACH OF WHICH
cC HAS 2-EXP PDF
c38	IF(THETA(1).LT.1.E-30) THETA(1)=1.E-4	!I.E. TAU=0.1 MUSEC
c	IF(THETA(3).LT.1.E-30) THETA(3)=1.E-4	!I.E. TAU=0.1 MUSEC
c	R1=1./THETA(1)
c	A1=THETA(2)
c	R2=1./THETA(3)
c	IF(.NOT.AMPOS) GOTO 41
c	IF(A1.GT.1.) A1=0.99999
c	IF(A1.LT.0.) A1=1.0E-6
c	THETA(2)=A1
c41	A2=1.0-A1
c	W(1)=exp1(-R1*ylow(j))	!USE W(I) AS SPARE VARIABLE NAMES
c	W(2)=exp1(-R1*yhigh(j))
c	W(3)=exp1(-R2*ylow(j))
c	W(4)=exp1(-R2*yhigh(j))
c	D=A1*A1*((1.+R1*ylow(j))*W(1)-(1.+R1*yhigh(j))*W(2))
c	D=D+A2*A2*((1.+R2*ylow(j))*W(3)-(1.+R2*yhigh(j))*W(4))
c	D=D+2.*A1*A2*(R2*(W(1)-W(2))-R1*(W(3)-W(4)))/(R2-R1)
cC DEFINE CONSTANTS IN W(I) FOR CALC OF LIK
c	W(1)=(A1*R1)**2
c	W(2)=(A2*R2)**2
c	W(3)=(2.*A1*A2*R1*R2)/(R2-R1)
c	S=0.
c	DO 40 I=1,NFIT(j)
c	Y=YVAL(I,j)
c	T=(W(1)*Y+W(3))*exp1(-R1*Y) + (W(2)*Y-W(3))*exp1(-R2*Y)
c	IF(T.LT.1.E-30) T=1.E-30
c	S=S-ALOG(T)
c40	CONTINUE
c	S=S+FLOAT(NFIT(j))*ALOG(D)
c	EXPLIK=S
c	RETURN
	END



