	FUNCTION EXPFUNK(THETA)
C Version of explik to test amoeba
	REAL THETA(20)
	LOGICAL AMPOS,GAMDST,fastf,logt,excreg
	REAL R(10),A(10),W(10)	!LOCAL
	integer jafix(10)
	common/amtry/k
	COMMON/FITBLK/yval(81920),xaxis(510),freq(510),Nyval,
     & Tlo,Thi,Ncomp,Nfit,ampos,pen,gamdst,isg,fastf,logt,nb1,nb2,
     & excreg,yexclow,yexchigh,isdcon,wfreq(51200),anfit,
     & nfitt,yvalt(5120)
	common/FIXBLK/nfix,jfix(20)
c 06/03/89 06:33pm Lahey version of EXPLIK. Data now in COMMON
c Note even when call UNDER0 is in force can still get NDP underflow
c with numbers less than 3.4E-4932 (approx 3.4exp(-10115)) so this
c function can replace exp() to check
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
cD	DO 105 I=1,NFIT
cD105	print 106,i,YVAL(I)
cD106	FORMAT(' *',i8,G13.6)
c
	call UNDER0('true')	!so underflows set=0 (Lahey manual p12.8)
c
	IF(GAMDST) GOTO 36
	IF(NCOMP.GT.1) GOTO 30
	R(1)=1.0/THETA(1)	!=1/TAU(1)
	A(1)=1.0	!AREA
	W(1)=R(1)
	GOTO 31
30	CONTINUE	!NEXT BIT DONE ONLY FOR NCOMP>1
	SA=0.	!TOTAL AREA EXCEPT FOR LAST COMP
	J=0
	DO 20 I=1,NCOMP
	J=J+1
	IF(THETA(J).LT.1.E-30) THETA(J)=1.E-4	!I.E. TAU=0.1 MUSEC
	R(I)=1.0/THETA(J)	! 1/TAU(I) IN RECIP MSEC
	J=J+1
	IF(I.EQ.NCOMP) GOTO 20	!LAST AREA NOT A PARAMETER
	A(I)=THETA(J)
	IF(.NOT.AMPOS) GOTO 21
	IF(A(I).GT.1.) A(I)=0.99999
	IF(A(I).LT.0.) A(I)=1.0E-6
21	SA=SA+A(I)
20	CONTINUE
C
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
	IF(.NOT.AMPOS) GOTO 24
	nafix=0
	if(nfix.gt.0) then		!some param fixed. Are any areas fixed?
	   do 50 i=1,ncomp-1
	   jafix(i)=0
	   if(jfix(2*i).eq.1) then
		nafix=nafix+1	!number of fixed areas
		jafix(i)=1	!area(i) is fixed
	   endif
50	   continue
	endif
	if(SA.ge.1.0) then
	   if(nafix.eq.0) then		!no fixed areas
		do 23 i=1,ncomp-1
23		A(i)=0.99*A(i)/SA
		SA=0.99
	   else			!some areas fixed
		safix=0.0
		sadj=0.0
		do 51 i=1,ncomp-1
		if(jafix(i).eq.0) then
		   sadj=sadj+a(i)
		else
		   safix=safix+a(i)
		endif
51		continue
		sa1=sadj/(0.99-safix)
		sa=0.0
		do 52 i=1,ncomp-1
		if(jafix(i).eq.0) a(i)=a(i)/sa1
		sa=sa+A(i)
52		continue
	   endif
	endif
c	if(SA.lt.1.0) goto 22
c	do 23 i=1,ncomp-1
c23	A(I)=A(I)/SA
c	SA=1.0
22	continue
c Reassign any altered values to theta
	do 25 i=1,ncomp-1
25	theta(2*i)=A(i)		!REASSIGN ALTERED VALUE
C
24	A(ncomp)=1.0-SA	!NOT A PARAMETER
C
31	CONTINUE	!JUMP HERE IF NCOMP=1
	D=0.0
	DO 33 I=1,NCOMP
	W(I)=A(I)*R(I)
33	D=D+A(I)*(exp1(-R(I)*TLO)-exp1(-R(I)*THI))
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
C
c Calc log(likelihood) for exponentials
34	if(fastf) goto 341
cd	if(idig(3).eq.1) print 801,fastf,nfit,ncomp,d,(yval(i),i=1,8),
cd    &	(yval(i),i=nfit-7,nfit)
cd801	format(l2,2i8,g13.6,/,8f10.3,/,8f10.3)
	s=0.
	DO 13 I=1,NFIT
	Y=YVAL(I)
	T=0.
	DO 131 J=1,NCOMP
131	T=T+W(J)*exp1(-R(J)*Y)
	IF(T.LT.1.E-30) T=1.E-30
	S=S-ALOG(T)
13	CONTINUE
	S=S+FLOAT(NFIT)*ALOG(D)	!nfit is constant-but needed for errors
	expfunk=S+ABS(S)*PF
	RETURN
c
c Section for log-lik for fast log-binned fit
341	s=0.
cd	if(idig(3).eq.1) print 800,fastf,nb1,nb2,D,tlo,thi,
cd    &	(xaxis(i),i=1,8),(freq(i),i=1,8)
cd800	format(l2,2i8,3g13.6,/,8f10.3,/,8f10.3)
	do 342 i=nb1,nb2
	t=0.
	do 343 m=1,ncomp
343	t=t+a(m)*(exp1(-r(m)*xaxis(i))-exp1(-r(m)*xaxis(i+1)))
	IF(T.LT.1.E-30) T=1.E-30
	s=s-freq(i)*alog(t/d)
342	continue
	expfunk=s
	RETURN
C
C NOW SEP SECTION TO FIT GAMMA DISTRIBUTION (NCOMP=1) OR EQUIV WHEN
C OPEN TIME PDF IS SUM OF 2 EXP (SEE MLFIT1)
36	IF(NCOMP.EQ.2) GOTO 38
	A(1)=1.0
	R(1)=1.0/THETA(1)	!=1/MEAN LENGTH OF SINGLE OPENING
	D=0.
	DO 39 I=0,ISG
	T=1.0
	DO 524 J=1,I
524	T=T*FLOAT(J)		!T=FACTORIAL(I)
	D=D+(R(1)**I)*((TLO**I)*exp1(-R(1)*TLO)-
     & (THI**I)*exp1(-R(1)*THI))/T
39	CONTINUE
C	IF(D.LT.1.E-30) D=1.E-30
C END OF LOOP TO CALC D. ON EXIT T=FACTORIAL(ISG)
	F=R(1)/T	!=LAMBDA/ISG!  CONSTANT THROUGHOUT LOOP
	S=0.
	DO 37 I=1,NFIT
	Y=YVAL(I)
	T=F*((R(1)*Y)**ISG)*exp1(-R(1)*Y)
	IF(T.LT.1.E-30) T=1.E-30
	S=S-ALOG(T)
37	CONTINUE
	S=S+FLOAT(NFIT)*ALOG(D)
	expfunk=S
	RETURN
C
C NOW SECTION TO FIT CONVOLUTION OF TWO OPEN TIMES EACH OF WHICH
C HAS 2-EXP PDF
38	IF(THETA(1).LT.1.E-30) THETA(1)=1.E-4	!I.E. TAU=0.1 MUSEC
	IF(THETA(3).LT.1.E-30) THETA(3)=1.E-4	!I.E. TAU=0.1 MUSEC
	R(1)=1./THETA(1)
	A(1)=THETA(2)
	R(2)=1./THETA(3)
	IF(.NOT.AMPOS) GOTO 41
	IF(A(1).GT.1.) A(1)=0.99999
	IF(A(1).LT.0.) A(1)=1.0E-6
	THETA(2)=A(1)
41	A(2)=1.0-A(1)
	W(1)=exp1(-R(1)*TLO)	!USE W(I) AS SPARE VARIABLE NAMES
	W(2)=exp1(-R(1)*THI)
	W(3)=exp1(-R(2)*TLO)
	W(4)=exp1(-R(2)*THI)
	D=A(1)*A(1)*((1.+R(1)*TLO)*W(1)-(1.+R(1)*THI)*W(2))
	D=D+A(2)*A(2)*((1.+R(2)*TLO)*W(3)-(1.+R(2)*THI)*W(4))
	D=D+2.*A(1)*A(2)*(R(2)*(W(1)-W(2))-R(1)*(W(3)-W(4)))/(R(2)-R(1))
C DEFINE CONSTANTS IN W(I) FOR CALC OF LIK
	W(1)=(A(1)*R(1))**2
	W(2)=(A(2)*R(2))**2
	W(3)=2.*A(1)*A(2)*R(1)*R(2)/(R(2)-R(1))
	S=0.
	DO 40 I=1,NFIT
	Y=YVAL(I)
	T=(W(1)*Y+W(3))*exp1(-R(1)*Y) + (W(2)*Y-W(3))*exp1(-R(2)*Y)
	IF(T.LT.1.E-30) T=1.E-30
	S=S-ALOG(T)
40	CONTINUE
	S=S+FLOAT(NFIT)*ALOG(D)
	expfunk=S
	RETURN
	END



