	subroutine PTYP1(THETA,AREA,AMEAN,SD,KMAX,NCOMP,YHIGH,YLOW,NFIT,
     & ANTRUE,ERRORS,GAMDST,ISG,idtype,ahist,thist,obhist)
C
c 06/03/89 06:42pm Lahey version
c
C TO TYPE FITTING RESULTS IN EKDIST AND TO ASSIGN AREA(),AMEAN() AND
C SD() FROM THETA, AND CALC ANTRUE.
C	PTYP1=VERSION IN EKDIST THAT DOES CALCS, BUT PRINTS RESULTS ONLY
C		IF ERRORS NOT WANTED
C
	REAL THETA(20)
	REAL AREA(10),AMEAN(10),SD(10)
C
	LOGICAL AHIST,THIST,OBHIST,ERRORS,GAMDST,pon,slock
	logical discprt
	common/dp/discprt
C
	pon()=slock()
c
	if(nfit.gt.0) goto 13
	print 14
14	format(' No parameters to type')
	RETURN
C FIRST ASSIGN THETA VALUES TO AREA(),AMEAN() AND SD() (PREV DONE
C IN MLFIT-NEEDED FOR CALC CURVE IN SCVDU).
13	F1=0.
	F2=0.
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
	IF(GAMDST) GOTO 12
	DO 112 J=1,NCOMP
	F1=F1+AREA(J)*exp1(-YLOW/AMEAN(J))	!PROB(OBS>YLOW)
	F2=F2+AREA(J)*exp1(-YHIGH/AMEAN(J))	!PROB(OBS>YHIGH)
112	CONTINUE
	ANTRUE=FLOAT(NFIT)/(F1-F2)
	IF(ERRORS) GOTO 999
	GOTO 100
C  NOW ANTRUE FOR GAMMA (ETC) DIST (SEE MLFIT1)
12	CALL CUMGAM(NCOMP,ISG,YLOW,YHIGH,AMEAN,AREA,F1,F2)
	ANTRUE=FLOAT(NFIT)/(F1-F2)
	GOTO 100
C
10	IF(.NOT.(AHIST.or.idtype.eq.14.or.idtype.eq.52)) GOTO 30
c NB id=14 or 52 for Popen dists
C NOW AMPLITUDE FIT
	J=0
	SA=0.
	DO 51 I=1,NCOMP
	J=J+1
	AMEAN(I)=THETA(J)
	sd(i)=theta(2)		!SD constrained equal
	if(i.gt.1.and.kmax.lt.3*ncomp-1) goto 371
	J=J+1			!SD not constrained equal
	SD(I)=THETA(J)
371	IF(I.EQ.NCOMP) GOTO 51
	J=J+1
	AREA(I)=THETA(J)
	SA=SA+AREA(I)
51	CONTINUE
	AREA(NCOMP)=1.0-SA
C   CALC ANTRUE,F1,F2,DENOM=PROB(YLOW<OBS<YHIGH)
	DO 1121 J=1,NCOMP
	X=(YLOW-AMEAN(J))/(SD(J)*SQRT(2.0))
	F1=F1+AREA(J)*0.5*(1.0-ERFS(X))		!PROB(OBS>YLOW)
	X=(YHIGH-AMEAN(J))/(SD(J)*SQRT(2.0))
	F2=F2+AREA(J)*0.5*(1.0-ERFS(X))		!PROB(OBS>YHIGH)
1121	CONTINUE
	ANTRUE=FLOAT(NFIT)/(F1-F2)
	IF(ERRORS) GOTO 999
	GOTO 101
C
30	CONTINUE
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
	IF(ERRORS) GOTO 999
	GOTO 102
C
C AREA(I) ETC DEFINED. NOW TYPE RESULTS FOR THIST.
100	CONTINUE
	s=0.0
	DO 40 I=1,NCOMP
	if(pon()) write(7,41) I,AMEAN(I),1000./AMEAN(I)
	if(discprt) write(8,41) I,AMEAN(I),1000./AMEAN(I)
	print 41,I,AMEAN(I),1000./AMEAN(I)
41	FORMAT( ' Tau(',I1,') (ms)= ',G13.6,
     & '     lambda (1/s)= ',G13.6)
	w=area(i)*1000./amean(i)
	en=antrue*area(i)
	if(pon()) write(7,43) I,AREA(I),en,w
	if(discprt) write(8,43) I,AREA(I),en,w
	print 43,I,AREA(I),en,w
43	FORMAT(' Area(',I1,')= ',G13.6,':  number = ',g13.6,
     & ':  amplitude (1/s)= ',G13.6,/)
	s=s+area(i)*amean(i)
40	continue
      print 431,s
      if(pon()) write(7,431) s
      if(discprt) write(8,431) s
431	format(' Overall mean (ms) = ',g13.6)
	GOTO 20		!print ANTRUE
C
101	CONTINUE
C NOW print AHIST RESULTS
	DO 52 I=1,NCOMP
	J=3*I-2		!=1,4,7= INDEX OF MEAN VALUES
	if(pon()) write(7,53) I,AMEAN(I),I,SD(I)
	if(discprt) write(8,53) I,AMEAN(I),I,SD(I)
	print 53,I,AMEAN(I),I,SD(I)
53	FORMAT(
     & ' Mean (',I1,') (pA)       = ',G13.6,
     & ' Standard dev(',I1,') (pA)= ',G13.6)
c52	CONTINUE
c	DO 57 I=1,NCOMP
	if(pon()) write(7,58) I,AREA(I)
	if(discprt) write(8,58) I,AREA(I)
	print 58,I,AREA(I)
58	FORMAT(
     & ' Area(',I1,')             = ',G13.6,/)
c57	CONTINUE
52	continue
cC	GOTO 999
	GOTO 20
C
C NOW print OP/BURST FIT.
102	CONTINUE
	s=0.0
	DO 60 I=1,NCOMP
	if(pon()) write(7,61) I,AMEAN(I),1.0-1.0/AMEAN(I)
	if(discprt) write(8,61) I,AMEAN(I),1.0-1.0/AMEAN(I)
	print 61,I,AMEAN(I),1.0-1.0/AMEAN(I)
61	FORMAT(' ''Mean''(',I1,') (ops/bst)= ',G13.6,'.     rho= ',G13.6)
	w=area(i)/amean(i)
	en=antrue*area(i)
	if(pon()) write(7,43) I,AREA(I),en,w
	if(discprt) write(8,43) I,AREA(I),en,w
	print 43,I,AREA(I),en,w
c43	FORMAT(' Area(',I1,')= ',G13.6,':  number = ',g13.6,
c     & ':  amplitude (1/s)= ',G13.6,/)
	s=s+area(i)*amean(i)
c	if(pon()) write(7,63) I,AREA(I),AREA(I)/AMEAN(I)
c	if(discprt) write(8,63) I,AREA(I),AREA(I)/AMEAN(I)
c	print 63,I,AREA(I),AREA(I)/AMEAN(I)
c63	FORMAT( ' Area(',I1,')= ',G13.6,'.       amplitude= ',G13.6)
60	CONTINUE
      print 631,s
      if(pon()) write(7,631) s
      if(discprt) write(8,631) s
631	format(' Overall mean (ops/bst) = ',g13.6)
C
C NOW print NTRUE ETC
20	CONTINUE
	if(pon()) write(7,113) ANTRUE,NFIT,ANTRUE*(1.-F1),ANTRUE*F2
	if(discprt) write(8,113) ANTRUE,NFIT,ANTRUE*(1.-F1),ANTRUE*F2
	print 113,ANTRUE,NFIT,ANTRUE*(1.-F1),ANTRUE*F2
113	FORMAT( ' Predicted true number of events= ',G13.6,/
     & ,' =No fitted(',I5,') +no below Ylow= ',G13.6,
     & ' +no above Yhigh= ',G13.6,/)
C
999	CONTINUE
	call flush(7)
	RETURN
	END


