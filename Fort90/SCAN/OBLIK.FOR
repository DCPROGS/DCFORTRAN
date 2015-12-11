	FUNCTION OBLIK(k,THETA)
C Version for EKDIS3 (file name=OBLIK3) Copy in OB1.FOR
	REAL THETA(K)
	real area(10),w(10),rho(10)	!local
	integer jafix(10)
	LOGICAL AMPOS,gamdst,fastf,logt
c	logical debug,caplock
	COMMON/FITBLK/yval(20480),xaxis(510),freq(510),Nyval,
     & Ylow,Yhigh,Ncomp,Nfit,ampos,pen,gamdst,isg,fastf,logt,nb1,nb2
	common/FIXBLK/nfix,jfix(20)
c
c 06/03/89 06:33pm Lahey version of OBLIK. Data now in COMMON
c
c*** Modif March 89 for any number of components
c	Mean no of openings/burst for component= theta 1,3,5,...
c	areas=theta 2,4,6,...
C VERSION OF OBLIK WITH YVAL AS PARAM NOT COMMON FOR EKDIST,PTSCH5
C CALCULATES THE MINUS LOG LIKELIHOOD FOR FITTING SUM
C OF 2 GEOMETRICS TO RANDOM VARIABLE IN YVAL.
C 	DATA BETWEEN YLOW AND YHIGH  (INCLUSIVE) ARE USED (SEE
C	MLFIT). **NB YLOW MUST NOT BE LESS THAN ONE OR CALC OF
C	PROB(Y<YLOW)IS RUBBISH- THIS NOW CHECKED IN SCVDU1
C
C NOTE THAT RHO(I) MUST BE BETWEEN 0 AND 1 SO MEAN(I) (MEAN FOR
C COMPONENT I ALONE) MUST BE GREATER THAN 1.
c	debug=caplock()
	if(ncomp.gt.1) goto 30
	IF(THETA(1).LT.1.0) THETA(1)=1.00001
	rho(1)=1.0-(1.0/THETA(1))		!RHO(1)
	area(1)=1.0
	goto 31
30	Sa=0.		!total area except for last comp
	j=0
	do 20 i=1,ncomp
	j=j+1
c	if(theta(j).lt.1.0) theta(i)=1.00001	!mean >= 1
	if(theta(j).lt.1.0) theta(j)=1.00001	!mean >= 1
	rho(i)=1.0 - (1.0/theta(j))
	j=j+1
	if(i.eq.ncomp) goto 20		!last area not a parameter
	area(i)=theta(j)
	if(.not.ampos) goto 21
	if(area(i).gt.1.0) area(i)=0.9999
	if(area(i).lt.0.) area(i)=1.e-6
21	sa=sa+area(i)
20	continue
c Check total area < 1 for 1st ncomp-1 components, as in EXPLIK
c	if(sa.lt.1.0) goto 22
c	do 23 i=1,ncomp-1
c23	area(i)=area(i)/sa	!scale to total=1
c	sa=1.0
c22	do 25 i=1,ncomp-1
c25	theta(2*i)=area(i)	!reassign altered areas
c	area(ncomp)=1.0 - sa
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
23		area(i)=0.99*area(i)/SA
		SA=0.99
	   else			!some areas fixed
		safix=0.0
		sadj=0.0
		do 51 i=1,ncomp-1
		if(jafix(i).eq.0) then
		   sadj=sadj+area(i)
		else
		   safix=safix+area(i)
		endif
51		continue
		sa1=sadj/(0.99-safix)
		sa=0.0
		do 52 i=1,ncomp-1
		if(jafix(i).eq.0) area(i)=area(i)/sa1
		sa=sa+area(i)
52		continue
	   endif
	endif
22	continue
c Reassign any altered values to theta
	do 25 i=1,ncomp-1
25	theta(2*i)=area(i)		!REASSIGN ALTERED VALUE
C
24	area(ncomp)=1.0-SA	!NOT A PARAMETER
C
c
31	continue		!jump here if ncomp=1
C NOTE THAT YVAL HAS BEEN REDEFINED IN MLFIT TO HOLD ONLY THE NFIT
C VALUES BETWEEN YLOW AND YHIGH INCLUSIVE. YVAL VALUES ARE NO OF
C OPENINGS/BURST, CONVERTED (IN GETDIS) TO FLOATING POINT.
C CALC DENOM=PROB(Y IS BETWEEN YLOW,YHIGH INCLUSIVE)
	d=0.0
	do 32 i=1,ncomp
	w(i)=area(i)*(1.0 - rho(i))
	y=ylow-1.0
c	if(debug) then
c	   print 700,i,rho(i),area(i),ylow,yhigh,y
c700	   format(i4,' rho(i),area(i),ylow,yhigh,y = ',5g13.6)
c	endif
	d=d + area(i)*((rho(i)**y) - (rho(i)**yhigh))
32	continue
C
	S=0.
	DO 12 I=1,NFIT
	Y=YVAL(I)-1.0		!=I'TH VALUE OF OP/BST-1
	T=0.0
	do 33 j=1,ncomp
33	T=T + w(j)*(rho(j)**y)
	IF(T.LT.1.E-30) T=1.E-30
	S=S-ALOG(T)
12	CONTINUE
	S=S+FLOAT(NFIT)*ALOG(D)
	OBLIK=S
	RETURN
C
	END

