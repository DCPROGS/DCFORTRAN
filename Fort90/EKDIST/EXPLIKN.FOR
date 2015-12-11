	function EXPLIKn(k,theta)
c
	REAL THETA(K)
	LOGICAL AMPOS,GAMDST,fastf,logt,excreg
	REAL rate(10),area(10),W(10)	!LOCAL
	integer jafix(10)
	COMMON/FITBLK/yval(81920),xaxis(510),freq(510),Nyval,
     & Tlo,Thi,Ncomp,Nfit,ampos,pen,gamdst,isg,fastf,logt,nb1,nb2,
     & excreg,yexclow,yexchigh,isdcon,wfreq(81920),anfit,
     & nfitt,yvalt(5120)
	common/FIXBLK/nfix,jfix(20)
c
c EXPLIKn is version of EXPLIK for fitting first latencies with
c estimation of the proportion of (true) null sweeps.
c In this case there is an extra parameter (there are k=2*ncomp parameters
c to be estimated, rather than 2*ncomp-1 as usual).
c The first 2*ncomp-1 parameters are defined
c as normal and theta(2*ncomp)=area of delta function at infinity
c =Prob[true null], so the missing parameter, found from fact that areas
c sum to 1, is the area for the last exponential component.
c
c 06/03/89 06:33pm Lahey version of EXPLIK. Data now in COMMON
c Note even when call UNDER0 is in force can still get NDP underflow
c with numbers less than 3.4E-4932 (approx 3.4exp(-10115)) so this
c function can replace exp() to check
c
c
	call UNDER0('true')	!so underflows set=0 (Lahey manual p12.8)
c
	SA=0.	!TOTAL AREA EXCEPT FOR LAST COMP
	j=0
	do i=1,ncomp
	   j=j+1
	   if(theta(j).lt.1.e-30) theta(j)=1.e-4	!i.e. tau=0.1 musec
	   rate(i)=1.0/theta(j)		!= 1/tau(i) in recip msec
	   j=j+1
	   if(i.lt.ncomp) then	!LAST AREA NOT A PARAMETER
		area(i)=theta(j)
		if(ampos) then
		   if(area(i).gt.1.) area(i)=0.99999
		   if(area(i).lt.0.) area(i)=1.0e-6
		endif
		SA=SA+area(i)
	   endif
	enddo
	area(ncomp+1)=theta(k)		!=fraction of nulls
	sa=sa+area(ncomp+1)		!sa=sum of areas to be estimated
	area(ncomp)=1.0 - sa		!area for last exponential component
c
c Each area (i=1,ncomp-1; sum=SA) is now between 0 and 1 if AMPOS. But if their
c sum is greater than 1 then scale all down so that their sum is 1, and
c thus AREA(ncomp+1)=0 (-fraction of nulls).
c If an area has been fixed at a largish value (eg in iteration to get
c a likelihood interval for an area) then do not really want to sqeeze out
c AREA(ncomp) altogether: try scaling area(1)...area(ncomp) so they add
c to 0.99 and area(ncomp)=0.01
c NB Must NOT alter areas if they are FIXED parameters! To avoid this
c happening, firstly add all the fixed areas=safix, so have 0.99-safix as the
c total adjustable area; then add all the non-fixed areas=sadj, and scale
c them so sadj=0.99-safix.
c NB; nafix=number of fixed AREAS and
c JAFIX(i)=0,1 according as area(i), i=1,ncomp, is free, fixed resp.
c NB in this case have ncomp areas to be estimated, and area(ncomp+1)=
c the last theta value, theta(2*ncomp) (fixed if jfix(2*ncomp)=1)
c whereas area(ncomp) (that for last exponential component) is found
c by difference.
	IF(.NOT.AMPOS) GOTO 24
c
	if(SA.ge.1.0) then
	   nafix=0
	   if(nfix.gt.0) then		!some param fixed. Are any areas fixed?
c==		do 50 i=1,ncomp-1
		do 50 i=1,ncomp
		jafix(i)=0
		if(jfix(2*i).eq.1) then	!2*i=2,4,6,...2*ncomp
		   nafix=nafix+1	!number of fixed areas
		   jafix(i)=1	!area(i) is fixed
		endif
50		continue
	   endif
	   if(nafix.eq.0) then		!no fixed areas
c==		do 23 i=1,ncomp-1
		do 23 i=1,ncomp
23		   theta(2*1)=0.99*theta(2*i)/SA
		SA=0.99
	   else			!some areas fixed
		safix=0.0
		sadj=0.0
c==		do 51 i=1,ncomp-1
		do 51 i=1,ncomp
		 if(jafix(i).eq.0) then
		   sadj=sadj+theta(2*i)
		 else
		   safix=safix+theta(2*i)
		 endif
51		continue
		sa1=sadj/(0.99-safix)
		sa=0.0
c==		do 52 i=1,ncomp-1
		do 52 i=1,ncomp
		 if(jafix(i).eq.0) theta(2*i)=theta(2*i)/sa1
		 sa=sa+theta(2*i)
52		continue
	   endif
c Reassign any altered values of theta to area(i)
	   sa=0.0
	   do i=1,ncomp
		if(i.lt.ncomp) then	!LAST AREA NOT A PARAMETER
		   area(i)=theta(2*i)
		   sa=sa+area(i)
	 	endif
	   enddo
	   area(ncomp+1)=theta(k)		!=fraction of nulls
	   sa=sa+area(ncomp+1)		!sa=sum of areas to be estimated
	   area(ncomp)=1.0 - sa		!area for last exponential component
	endif
c
24	continue
c Calculate d=Prob[tlo < t <thi]
	d=0.0
	do i=1,ncomp
	   w(i)=area(i)*rate(i)
	   d=d + area(i)*(exp1(-rate(i)*tlo) - exp1(-rate(i)*thi))
	enddo
C	IF(D.LT.1.E-30) D=1.E-30
C
C INSERT SECTION HERE TO CONSTRAIN F(0)>=0 (IF NOT AMPOS THEN POSSIBLE
C TO GET A NEGATIVE DENSITY!). DEFINE A PENALTY FUNCTION FOR THIS.
	pf=0.0
	IF(AMPOS) GOTO 34
	F0=0.
	S=0.
	DO 35 I=1,NCOMP
	F0=F0+W(I)	!=F(0)
35	S=S+ABS(W(I))	!SCALES F(0)
	PF=0.0		!IF F(0)>=0.
	IF(F0.LT.0.) PF=PEN*ABS(F0)/S	!POSITIVE PENALTY FUNC
c
c Calc log(likelihood) for exponentials
34	continue
	s=0.
	do i=1,nfit
	   y=yval(i)
	   t=0.
	   do j=1,ncomp
		t=t + w(j)*exp1(-rate(j)*y)
	   enddo
	   if(t.lt.1.e-30) t=1.e-30
	   S=S - ALOG(t)
	enddo
	S=S + float(nfit)*ALOG(d)	!nfit is constant-but needed for errors
c Add bit for truncated y values (value >= yvalt) -say nfitt such values
c in yvalt(i)
	if(nfitt.gt.0) then
	   do i=1,nfitt
		y=yvalt(i)
		t=0.
		do j=1,ncomp
		   t=t + area(j)*exp1(-rate(j)*y)
		enddo
		t=t+area(ncomp+1)	!add area of delta function part
		if(t.lt.1.e-30) t=1.e-30
		S=S - ALOG(t)
	   enddo
	endif
c
	if(pf.gt.0.) then
	   explikn=S + abs(S)*pf
	else
	   explikn=S
	endif
	RETURN
c
	END



