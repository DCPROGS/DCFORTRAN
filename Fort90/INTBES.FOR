	SUBROUTINE INTBES(Y,X0,DX,N,Y1,DX1,N1,ISINGL,XVAL,YVAL,iout)
C
c INTBES is Lahey V5 version of INBES2 with extra param, iout, to show
c when single value interp is out of range (INBES1 is special version
c for single values only)
C SUBROUTINE FOR BESSEL INTERPOLATION AS IN MASSEY &
C KESTELMAN.
C	TO INTERPOLATE A WHOLE TABLE (ISINGL=0):
C SUPPLY Y(N) VALUES OF Y CORRESPONDING
C TO N VALUES OF X STARTING AT X0 AND SEP BY DX
C SO DATA IS: Y(I)=Y AT X=X0+(I-1)*DX, I=1,...,N.
C COMPUTES N1-2 VALUES OF Y1(J)=Y AT X=X0+(J-1)*DX1
C FOR J=2,..,N1-1.
c
c NB Range of x must be same for both arrays
c so xmax=x0+(n-1)*dx=x0+(n1-1)*dx1, so must have
C dx/dx1=(n1-1)/(n-1) i.e. n1=1+ (n-1)*dx/dx1,
c or dx1=dx*(n-1)/(n1-1)
C This prog SETS Y1(1)=Y(1) AND Y1(N1)=Y(N).
C  N.B. FOR CALC OF VALUES AT EACH END OF TABLE IT IS ASSUMED
C THAT Y(I)=Y(1) FOR I<1 AND Y(N) FOR I>N.
c IF ISINGL=0,XVAL,YVAL and iout ARE IGNORED
c
C	TO INTERPOLATE A SINGLE VALUE:
C SET ISINGL=1 AND SPECIFY X VALUE,XVAL,AT WHICH WHICH
C RESULT,YVAL, REQUIRED.
c Y(1)-Y(N) is array to be interp with y(i) corresp x=x0+float(i-1)*dx
c so y(1) is at x=x0. Output=YVAL= value of x at specified XVAL.
c This version returns y(1) if xval<x0 and sets iout=-1
c - returns y(n) if xval>xmax and sets iout=1
c If in range then returns with iout=0
C
C CALL INTBES(Y,X0,DX,N,YNEW,DXNEW,NNEW,ISINGL,XVAL,YVAL)
	real*4 Y1(n1),Y(n)
C
	if(isingl.eq.1) goto 20
	Y1(1)=Y(1)
	DO 1 I=2,N1-1
	X1=X0+FLOAT(I-1)*DX1	!TIME FOR NEW POINT
	J=1+IFIX(X1/DX)
C THIS IS INDEX IN Y OF ELEMENT THAT CORRESPONDS TO NEAREST
C X VALUE BELOW X1. THIS VALUE IS
	X=X0+FLOAT(J-1)*DX
C SO REQ X VALUE IS ABOVE THIS BY
	DELX=X1-X
C I.E. A FRACTION THETA OF TABULATED INTERVAL,DX,WHERE
	THETA=DELX/DX
C SO DEFINE
	F=Y(J)
	J1=J+1
	IF(J1.GT.N) J1=N
	F1=Y(J1)
	J2=J+2
	IF(J2.GT.N) J2=N
	F2=Y(J2)
	J0=J-1
	IF(J0.LT.1) J0=1
	F0=Y(J0)
C NOW INTERPOLATE
	Y1(I)=F+THETA*(F1-F)+0.25*THETA*(THETA-1.)*(F2-F1+F0-F)
1	CONTINUE
	Y1(N1)=Y(N)
	RETURN
c
c For a single value
20	continue
	iout=0
	xmax=x0+float(n-1)*dx
	if(xval.ge.x0.and.xval.le.xmax) goto 10	!in range
	if(xval.lt.x0) goto 2
	yval=y(n)
	iout=1
	RETURN
2	yval=y(1)
	iout=-1
	RETURN		!out of range
10	J=1+IFIX(Xval/DX)
C THIS IS INDEX IN Y OF ELEMENT THAT CORRESPONDS TO NEAREST
C X VALUE BELOW Xval. THIS VALUE IS
	X=X0+FLOAT(J-1)*DX
C SO REQ X VALUE IS ABOVE THIS BY
	DELX=Xval-X
C I.E. A FRACTION THETA OF TABULATED INTERVAL,DX,WHERE
	THETA=DELX/DX
C SO DEFINE
	F=Y(J)
	J1=J+1
	IF(J1.GT.N) J1=N
	F1=Y(J1)
	J2=J+2
	IF(J2.GT.N) J2=N
	F2=Y(J2)
	J0=J-1
	IF(J0.LT.1) J0=1
	F0=Y(J0)
C NOW INTERPOLATE
	Yval=F+THETA*(F1-F)+0.25*THETA*(THETA-1.)*(F2-F1+F0-F)
	return
c
	END



                                                                                                                                                                                                                                                              


                                                  
