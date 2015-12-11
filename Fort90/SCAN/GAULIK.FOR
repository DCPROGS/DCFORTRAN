	function GAULIK(k,theta)
c 06/03/89 05:56pm Lahey version with data now in COMMON as originally
c Modif Nov 86 for up to 5 components
	REAL THETA(K)
	integer ja(10)		!local
	REAL R(10),A(10),sd(10)	!LOCAL
	LOGICAL AMPOS,gamdst,fastf,logt
	integer jafix(10)
	common/FIXBLK/nfix,jfix(20)
	COMMON/FITBLK/yval(20480),xaxis(510),freq(510),Nyval,
     & Ylow,Yhigh,Ncomp,Nfit,ampos,pen,gamdst,isg,fastf,logt,nb1,nb2
C LIKELIHOOD FUNCT FOR FITTING GAUSSIANS
C N.B. A VALUES ARE REL AREAS OF COMPONENTS HERE
C***6-NOV-82 ORDER OF PARAMETERS CHANGED TO (MEAN(I),SD(I),AREA(I))...,
C I-1,NCOMP TO AGREE WITH NEW MLFIT
	C=2.5066283	!=SQRT(2*PI)
c
	IF(NCOMP.GT.1) GOTO 30
	R(1)=THETA(1)	!=mean(1)
	sd(1)=theta(2)	!sd
	A(1)=1.0	!AREA
	GOTO 31
c
30	CONTINUE	!NEXT BIT DONE ONLY FOR NCOMP>1
	SA=0.	!TOTAL AREA EXCEPT FOR LAST COMP
	J=0
	DO 20 I=1,NCOMP
	J=J+1
c	IF(THETA(J).LT.1.E-30) THETA(J)=1.E-7
	R(I)=THETA(J)		! mean(i)
c
	sd(i)=theta(2)		!if SD fixed, otherwise redefined
	if(i.gt.1.and.k.lt.3*ncomp-1) goto 371	!SD constrained equal
	J=J+1
	IF(THETA(J).LT.1.E-30) THETA(J)=1.E-7	!sd must be pos
	sd(i)=theta(j)
c
371	j=j+1
	ja(i)=j	!index in theta of area of component i (inc i=ncomp)
	IF(I.EQ.NCOMP) GOTO 20	!LAST AREA NOT A PARAMETER
	A(I)=THETA(J)
	IF(.NOT.AMPOS) GOTO 21
	IF(A(I).GT.1.) A(I)=0.99999
	IF(A(I).LT.0.) A(I)=1.0E-6
21	SA=SA+A(I)
20	CONTINUE
C
C EACH AREA (I=1,NCOMP-1) IS NOW BETWEEN 0 AND 1 IF AMPOS. BUT IF THEIR
C SUM IS GREATER THAN 1 THEN SCALE ALL DOWN SO THAT THEIR SUM IS 1 AND
C THUS AREA(NCOMP)=0
c	IF(.NOT.AMPOS) GOTO 24
c	IF(SA.LT.1.0) GOTO 22
c	DO 23 I=1,NCOMP-1
c23	A(I)=A(I)/SA
c	SA=1.0
c22	DO 25 I=1,NCOMP-1
c25	THETA(ja(i))=A(I)		!REASSIGN ALTERED VALUE
cc25	THETA(3*I)=A(I)		!REASSIGN ALTERED VALUE
C
c24	A(ncomp)=1.0-SA	!NOT A PARAMETER
c New version
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
	   if(jfix(ja(i)).eq.1) then
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
22	continue
c Reassign any altered values to theta
	do 25 i=1,ncomp-1
25	theta(ja(i))=A(i)		!REASSIGN ALTERED VALUE
C
24	A(ncomp)=1.0-SA	!NOT A PARAMETER
C
31	CONTINUE	!JUMP HERE IF NCOMP=1
c
C CALc DENOMINATOR=PROB(YLOW<Y<YHIGH)
1	CONTINUE
	T=SQRT(2.0)
	d=0.
	do 200 i=1,ncomp
	XLO=(YLOW-R(i))/(SD(i)*T)
	XHI=(YHIGH-R(i))/(SD(i)*T)
	D=D + a(i)*0.5*(ERFS(XHI)-ERFS(XLO))
200	continue
C
C NOW CALC MINUS LOG-LIKELIHOOD
111	CONTINUE
	s=0.
	DO 2 I=1,NFIT
	Y=YVAL(I)
	t=0.
	do 201 j=1,ncomp
	t1=(y-r(j))/sd(j)
	t=t + a(j)*exp(-0.5*t1*t1)/(c*sd(j))
201	continue
	IF(T.LT.1.E-30) T=1.E-30
	S=S-ALOG(T)
2	CONTINUE
	S=S+FLOAT(NFIT)*ALOG(D)
	GAULIK=S
	RETURN
	END

