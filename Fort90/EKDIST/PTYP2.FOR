	subroutine PTYP2(THETA,VAR,AREA,AMEAN,SD,
     & NCOMP,YHIGH,YLOW,NFIT,ANTRUE,idtype,
     & excreg,yexclow,yexchigh,isdcon,anfit)
c
c 06/07/89 05:36pm Lahey version
C
c NB This version (09/07/94 02:56pm) gets areas etc OK when SD constrained
c (isdcon=2,3) in fit of Gaussians- BUT HMAT2 not fixed to calc errors
c in these cases!!
c
C TO print FITTING RESULTS IN EKDIST AND TO ASSIGN AREA(),AMEAN() AND
C SD() FROM THETA, AND CALC ANTRUE.
C	PTYP1=VERSION IN SCDIST THAT DOES CALCS,AND PRINTS ONLY
C		IF ERRORS NOT WANTED-OTHERWISE LEAVE AND CHAIN
C		TO SCERR1 WHERE PRINTING DONE
C	PTYP2=VERSION IN EKERR. PRINTS PARAMETERS AND ERRORS
c NB This version should not be called if SD constrained to be
c equal in fit of Gaussians- errors should not be calc from
c Hessian in this case.
C N.B. HMAT1 HAS DEFINED ALL KMAX ELEMENTS OF VAR(I) WITH -1. IN
C ELEMENT CORRESP TO A FIXED PARAMETER. ALSO,IF NCOMP>1, HAS
C DEFINED VAR(KMAX+1) AS CALC VARIANCE FOR THE LAST AREA
C
	REAL THETA(50),VAR(50)
	REAL AREA(10),AMEAN(10),SD(10)
	LOGICAL AHIST,THIST,OBHIST,excreg
	logical usewfreq
	logical discprt,slock,pon
	common/dp/discprt
	COMMON/LOGBLK/ AHIST,THIST,OBHIST
C
	pon()=slock()
c
C FIRST ASSIGN THETA VALUES TO AREA(),AMEAN() AND SD() (PREV DONE
C IN MLFIT-NEEDED FOR CALC CURVE IN SCVDU).
CD	print 451,(VAR(I),I=1,KMAX+1)
CD451	FORMAT(2X,10G13.6)
	f1=0.
	f2=0.
	f3=0.
	usewfreq=ahist.and.idtype.eq.7
c
	IF(.NOT.THIST) GOTO 10
	J=0
	SA=0.
	DO 11 I=1,NCOMP
	J=J+1
	AMEAN(I)=THETA(J)
	IF(I.EQ.NCOMP) GOTO 11
	J=J+1
	AREA(I)=THETA(J)
	SA=SA+AREA(I)
11	CONTINUE
	AREA(NCOMP)=1.0-SA
C   CALC ANTRUE,F1,F2,DENOM=PROB(YLOW<OBS<YHIGH)
	DO 112 J=1,NCOMP
	F1=F1+AREA(J)*exp1(-YLOW/AMEAN(J))	!PROB(OBS>YLOW)
	F2=F2+AREA(J)*exp1(-YHIGH/AMEAN(J))	!PROB(OBS>YHIGH)
112	CONTINUE
	ANTRUE=FLOAT(NFIT)/(F1-F2)
	GOTO 100
C
10	IF(.NOT.(AHIST.or.idtype.eq.14.or.idtype.eq.52)) GOTO 30
c NB id=14 or 52 for Popen dists
C NOW AMPLITUDE FIT
	j=0
	sa=0.0
	do i=1,ncomp
	   j=j+1
	   amean(i)=theta(j)		! mean(i)
	   if(isdcon.eq.3) then
		if(i.eq.1) j=j+1
		sd(i)=theta(2)		!if SD fixed, otherwise redefined
	   else if(isdcon.eq.2) then
		if(i.eq.1) j=j+1
		sd(i)=theta(2)*amean(i)	!theta(2)=sdfac in this case
	   else				!isdcon=1
		j=j+1
		sd(i)=theta(j)
	   endif
	   if(i.lt.ncomp) then
		j=j+1
		area(i)=theta(j)
		sa=sa+area(i)
	   endif
	enddo
	area(ncomp)=1.0-sa
c
C Calc denominator=PROB(ylow<Y<yhigh)
c or, if range excluded, PROB(ylow<Y<yexclow.or.yexchigh<Y<yhigh)
	st=sqrt(2.0)
	d=0.
	if(excreg) then
	 do i=1,ncomp
	   ulo=(ylow-amean(i))/(sd(i)*st)
	   uhi=(yhigh-amean(i))/(sd(i)*st)
	   uxlo=(yexclow-amean(i))/(sd(i)*st)
	   uxhi=(yexchigh-amean(i))/(sd(i)*st)
	   f1=f1 + area(i)*0.5*(1.0-erfs(ulo))			!prob(obs>ylow)
	   f2=f2 + area(i)*0.5*(1.0-erfs(uhi))			!prob(obs>yhigh)
	   f3=f3 + area(i)*0.5*(erfs(uxhi)-erfs(uxlo))	!prob(yxlo<obs<yxhigh)
	   d=d + area(i)*0.5*(erfs(uhi)-erfs(uxhi)+erfs(uxlo)-erfs(ulo))
	 enddo
	else		!no excluded region (as earlier version)
	 do i=1,ncomp
	   ulo=(ylow-amean(i))/(sd(i)*st)
	   uhi=(yhigh-amean(i))/(sd(i)*st)
	   f1=f1 + area(i)*0.5*(1.0-erfs(ulo))			!prob(obs>ylow)
	   f2=f2 + area(i)*0.5*(1.0-erfs(uhi))			!prob(obs>yhigh)
c	   d=d + area(i)*0.5*(erfs(uhi)-erfs(ulo))
	 enddo
	endif
	if(usewfreq) then
	   ANTRUE=anfit/(f1-f2-f3)		!anfit calc in MLFITS
	else
	   ANTRUE=float(nfit)/(f1-f2-f3)	!normal calc
	endif
c=	ANTRUE=float(nfit)/(f1-f2-f3)
c========debug
c	x=f1-f2-f3		!should=d
c=======
	GOTO 101
C
30	CONTINUE
c
C NOW OP/BURST FIT
	Sa=0.		!total area except for last comp
	j=0
	do 32 i=1,ncomp
	j=j+1
	amean(i)=theta(j)
c	rho(i)=1.0 - (1.0/theta(j))
	j=j+1
	if(i.eq.ncomp) goto 32		!last area not a parameter
	area(i)=theta(j)
	sa=sa+area(i)
32	continue
	area(ncomp)=1.0 - sa
c
C   CALC ANTRUE,F1,F2,DENOM=PROB(YLOW<=OBS<=YHIGH)
	DO 1122 J=1,NCOMP
	X=1.0-(1.0/AMEAN(J))		!=RHO(J)
	F1=F1+AREA(J)*(X**(YLOW-1.0))	!PROB(OBS=>YLOW)
	F2=F2+AREA(J)*(X**(YHIGH))	!PROB(OBS>YHIGH)
1122	CONTINUE
	ANTRUE=FLOAT(NFIT)/(F1-F2)
	GOTO 102
C
C AREA(I) ETC DEFINED. NOW print RESULTS FOR THIST.
100	CONTINUE
	DO 40 I=1,NCOMP
	J=2*I-1		!=1,3,5,..KMAX= INDEX OF TAU VALUES
	IF(VAR(J).GT.0.0.and. pon()) write(7,41)I,AMEAN(I),
     & SQRT(VAR(J)),1000./AMEAN(I)
      if(VAR(J).GT.0.0.and.discprt) write(8,41)I,AMEAN(I),
     & SQRT(VAR(J)),1000./AMEAN(I)
	IF(VAR(J).GT.0.0) print 41,I,AMEAN(I),SQRT(VAR(J)),1000./AMEAN(I)
41	FORMAT( ' Tau(',I1,') (ms)= ',G13.6,'  approx SD= ',G13.6,
     & '   lambda (1/s)= ',G13.6)
	IF(VAR(J).LT.0.0.and.pon()) write(7,42)I,AMEAN(I),1000./AMEAN(I)
      if(VAR(J).lt.0.0.and.discprt) write(8,42)I,AMEAN(I),1000./AMEAN(I)
	IF(VAR(J).LT.0.0) print 42,I,AMEAN(I),1000./AMEAN(I)
42	FORMAT( ' Tau(',I1,') (ms)= ',G13.6,' (Not defined)',
     & '         lambda (1/s)= ',G13.6)
40	CONTINUE
	DO 45 I=1,NCOMP
	J=2*I	!=2,4,...,KMAX+1
	IF(VAR(J).GT.0.0.and.pon()) write(7,43)I,AREA(I),SQRT(VAR(J)),
     & (AREA(I)*1000./AMEAN(I))
      if(VAR(J).gt.0.0.and.discprt) write(8,43)I,AREA(I),SQRT(VAR(J)),
     & (AREA(I)*1000./AMEAN(I))
	IF(VAR(J).GT.0.0) print 43,I,AREA(I),SQRT(VAR(J)),
     & (AREA(I)*1000./AMEAN(I))
43	FORMAT( ' Area(',I1,')= ',G13.6,'  approx SD= ',G13.6,
     & '   amplitude (1/s)= ',G13.6)
	IF(VAR(J).LT.0.0.and.pon()) write(7,44)I,AREA(I),
     & (AREA(I)*1000./AMEAN(I))
      if(VAR(J).lt.0.0.and.discprt) write(8,44)I,AREA(I),
     & (AREA(I)*1000./AMEAN(I))
	IF(VAR(J).LT.0.0) print 44,I,AREA(I),
     & (AREA(I)*1000./AMEAN(I))
44	FORMAT( ' Area(',I1,')= ',G13.6,' (Not defined)',
     & '   amplitude (1/s)= ',G13.6)
45	CONTINUE
	GOTO 20		!CALC ANTRUE
C
101	CONTINUE
C NOW print AHIST RESULTS
	DO 52 I=1,NCOMP
	J=3*I-2		!=1,4,7= INDEX OF MEAN VALUES
	IF(VAR(J).GT.0.0.and.pon()) write(7,53)I,AMEAN(I),SQRT(VAR(J))
      if(VAR(J).gt.0.0.and.discprt) write(8,53)I,AMEAN(I),SQRT(VAR(J))
	IF(VAR(J).GT.0.0) print 53,I,AMEAN(I),SQRT(VAR(J))
53	FORMAT( ' Mean(',I1,') (pA)= ',G13.6,'  approx SD= ',G13.6)
	IF(VAR(J).LT.0.0.and.pon()) write(7,54)I,AMEAN(I)
      if(VAR(J).lt.0.0.and.discprt) write(8,54)I,AMEAN(I)
	IF(VAR(J).LT.0.0) print 54,I,AMEAN(I)
54	FORMAT( ' Mean(',I1,') (pA)= ',G13.6,' (Not defined)')
	J=J+1		!=2,5,8.=INDEX OF SD VALUES
	IF(VAR(J).GT.0.0.and.pon()) write(7,55)I,SD(I),SQRT(VAR(J))
      if(VAR(J).gt.0.0.and.discprt) write(8,55)I,SD(I),SQRT(VAR(J))
	IF(VAR(J).GT.0.0) print 55,I,SD(I),SQRT(VAR(J))
55	FORMAT( ' Std dev(',I1,') (pA)= ',G13.6,'  approx SD= ',G13.6)
	IF(VAR(J).LT.0.0.and.pon()) write(7,56)I,SD(I)
      if(VAR(J).lt.0.0.and.discprt) write(8,56)I,SD(I)
	IF(VAR(J).LT.0.0) print 56,I,SD(I)
56	FORMAT( ' Std dev(',I1,') (pA)= ',G13.6,' (Not defined)')
52	CONTINUE
	DO 57 I=1,NCOMP
	J=3*I		!=3,6,9= INDEX OF AREA VALUES
	IF(VAR(J).GT.0.0.and.pon()) write(7,58)I,AREA(I),SQRT(VAR(J))
      if(VAR(J).gt.0.0.and.discprt) write(8,58)I,AREA(I),SQRT(VAR(J))
	IF(VAR(J).GT.0.0) print 58,I,AREA(I),SQRT(VAR(J))
58	FORMAT( ' Area(',I1,')= ',G13.6,'  approx SD= ',G13.6)
	IF(VAR(J).LT.0.0.and.pon()) write(7,59)I,AREA(I)
      if(VAR(J).lt.0.0.and.discprt) write(8,59)I,AREA(I)
	IF(VAR(J).LT.0.0) print 59,I,AREA(I)
59	FORMAT( ' Area(',I1,')= ',G13.6,' (Not defined)')
57	CONTINUE
	GOTO 20
C
C NOW print OP/BURST FIT. FOR NCOMP=2 ONLY AT PRESENT
102	CONTINUE
	DO 60 I=1,NCOMP
	IF(VAR(I).GT.0.0.and.pon()) write(7,61)I,AMEAN(I),SQRT(VAR(I)),
     & 1.0-1.0/AMEAN(I)
      if(VAR(J).gt.0.0.and.discprt) write(8,61)I,AMEAN(I),SQRT(VAR(I)),
     & 1.0-1.0/AMEAN(I)
	IF(VAR(I).GT.0.0) print 61,I,AMEAN(I),SQRT(VAR(I)),
     & 1.0-1.0/AMEAN(I)
61	FORMAT( ' Mean(',I1,') (OP/BST)= ',G13.6,'  approx SD= ',G13.6,
     & '   rho= ',G13.6)
	IF(VAR(I).LT.0.0.and.pon()) write(7,62)I,AMEAN(I),
     & 1.0-1.0/AMEAN(I)
      if(VAR(J).lt.0.0.and.discprt) write(8,62)I,AMEAN(I),
     & 1.0-1.0/AMEAN(I)
	IF(VAR(I).LT.0.0) print 62,I,AMEAN(I),
     & 1.0-1.0/AMEAN(I)
62	FORMAT( ' Mean(',I1,') (OP/BST)= ',G13.6,' (Not defined) ',
     & '          rho= ',G13.6)
C
	J=I+NCOMP	!PARAMETER INDEX FOR AREAS
	IF(VAR(J).GT.0.0.and.pon()) write(7,63)I,AREA(I),SQRT(VAR(J)),
     & AREA(I)/AMEAN(I)
      if(VAR(J).gt.0.0.and.discprt) write(8,63)I,AREA(I),SQRT(VAR(J)),
     & AREA(I)/AMEAN(I)
	IF(VAR(J).GT.0.0) print 63,I,AREA(I),SQRT(VAR(J)),
     & AREA(I)/AMEAN(I)
63	FORMAT( ' Area(',I1,')= ',G13.6,'  approx SD= ',G13.6,
     & '   Amplitude= ',G13.6)
	IF(VAR(J).LT.0.0.and.pon()) write(7,64)I,AREA(I),
     & AREA(I)/AMEAN(I)
      if(VAR(J).lt.0.0.and.discprt) write(8,64)I,AREA(I),
     & AREA(I)/AMEAN(I)
	IF(VAR(J).LT.0.0) print 64,I,AREA(I),
     & AREA(I)/AMEAN(I)
64	FORMAT( ' Area(',I1,')= ',G13.6,' (Not defined) ',
     & '          Amplitude= ',G13.6)
60	CONTINUE
	GOTO 20
C
C NOW print NTRUE ETC
20	CONTINUE
	if(pon()) write(7,113)ANTRUE,NFIT,ANTRUE*(1.-F1),ANTRUE*F2
      if(discprt) write(8,113)ANTRUE,NFIT,ANTRUE*(1.-F1),ANTRUE*F2
	print 113,ANTRUE,NFIT,ANTRUE*(1.-F1),ANTRUE*F2
113	FORMAT( ' Predicted true number of events= ',G13.6,/
     & ,' =No fitted(',I5,') +no below Tlow= ',G13.6,
     & ' +no above Thigh= ',G13.6)
C
999	CONTINUE
	call flush(7)
	RETURN
	END


