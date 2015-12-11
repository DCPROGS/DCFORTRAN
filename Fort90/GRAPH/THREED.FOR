CCC
C  NOTE: THE ARRAYS MUST BE DIMENSIONED F(NY,NX) WHERE THE FIRST DIMENSION
C        IS ASSOCIATED WITH Y AND THE SECOND DIMENSION IS ASSOCIATED WITH X

      REAL F(21,21), XX(21,21), YY(21,21), X(21), Y(21), TH, XV, YV, ZV
      INTEGER I, J, IHID

C  Define surface Sin(x) * Cos(y^2) on [0,2] x [0,2]

      DO 10 I = 1, 21
        X(I) = 0.1 * (I - 1)
        TH = SIN(X(I))
        DO 10 J = 1, 21
          Y(J) = 0.1 * (J - 1)
          F(J,I) = TH * COS(Y(J)* Y(J))
10    CONTINUE

CCC
C  ihid = 0 --> all lines      
C  ihid = 1 --> upper surface  
C  ihid = 2 --> under surface  
C  ihid = 3 --> all visible surfaces

      WRITE(*,*) 'Input HID '
      READ(*,*) IHID
      XV = 34.0
      YV = 12.0
      ZV = 0.0

      CALL INIPLT(0, .false., 1.0)
      CALL SETORIGIN(0.0, 0.0, 0.0)
      CALL SETAPERTURE(0.0, 0.0, 0.0)
      CALL SETVIEWPT(xv, yv, zv)
      CALL SCALE3D(0.0, 2.0, 0.0, 2.0, -1.0, 1.0)
      CALL PROJECT(f, xx, yy, x, y, 21, 21)

C  draw projected image       

      CALL PLOT3D(xx, yy, 21, 21, ihid)
      CALL ENDPLT

      END
