      SUBROUTINE ODEINT(YSTART,NVAR,X1,X2,EPS,H1,HMIN,NOK,NBAD,DERIVS,RK
     *QC)
c Double precision version of Num Recipes routine
	IMPLICIT real*8 (A-H,O-Z)
c      PARAMETER (MAXSTP=10000,NMAX=10,TWO=2.0d0,ZERO=0.0d0,TINY=1.d-30)
c      PARAMETER (MAXSTP=10000,NMAX=10,TWO=2.0d0,ZERO=0.0d0,TINY=1.d-200)
      PARAMETER (MAXSTP=10000,NMAX=100,TWO=2.0d0,ZERO=0.0d0,TINY=1.d-30)
c      COMMON /PATH/ KMAX,KOUNT,DXSAV,XP(200),YP(10,200)
	COMMON /PATH/KMAX,KOUNT,DXSAV,XP(1024),YP(100,1024)	!for ODEINT
c      DIMENSION YSTART(NVAR),YSCAL(NMAX),Y(NMAX),DYDX(NMAX)
      real*8 YSTART(NVAR),YSCAL(NMAX),Y(NMAX),DYDX(NMAX)
	external derivs,rkqc
c
	nshort=0	!DC -count number if steps 'too short'
      X=X1
      H=dSIGN(H1,X2-X1)
      NOK=0
      NBAD=0
      KOUNT=0
      DO 11 I=1,NVAR
        Y(I)=YSTART(I)
11    CONTINUE
      XSAV=X-DXSAV*TWO
      DO 16 NSTP=1,MAXSTP
        CALL DERIVS(X,Y,DYDX)
        DO 12 I=1,NVAR
          YSCAL(I)=dABS(Y(I))+dABS(H*DYDX(I))+TINY
12      CONTINUE
        IF(KMAX.GT.zero)THEN
          IF(dABS(X-XSAV).GT.dABS(DXSAV)) THEN
            IF(KOUNT.LT.KMAX-1)THEN
              KOUNT=KOUNT+1
              XP(KOUNT)=X
              DO 13 I=1,NVAR
                YP(I,KOUNT)=Y(I)
13            CONTINUE
              XSAV=X
            ENDIF
          ENDIF
        ENDIF
        IF((X+H-X2)*(X+H-X1).GT.ZERO) H=X2-X
        CALL RKQC(Y,DYDX,NVAR,X,H,EPS,YSCAL,HDID,HNEXT,DERIVS)
        IF(HDID.EQ.H)THEN
          NOK=NOK+1
        ELSE
          NBAD=NBAD+1
        ENDIF
        IF((X-X2)*(X2-X1).GE.ZERO)THEN
          DO 14 I=1,NVAR
            YSTART(I)=Y(I)
14        CONTINUE
          IF(KMAX.NE.0)THEN
            KOUNT=KOUNT+1
            XP(KOUNT)=X
            DO 15 I=1,NVAR
              YP(I,KOUNT)=Y(I)
15          CONTINUE
          ENDIF
          RETURN
        ENDIF
c       IF(ABS(HNEXT).LT.HMIN) PAUSE 'Step size smaller than minimum.'
        IF(ABS(HNEXT).LT.HMIN) then
		print 17
17	  	format('Step size smaller than minimum.')
		nshort=nshort+1
	  endif
        H=HNEXT
16    CONTINUE
      PAUSE 'Too many steps.'
      RETURN
      END
