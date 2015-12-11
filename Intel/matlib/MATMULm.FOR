	subroutine MATMUL(A,B,R,NA,N,NB,SCALAR,
     & KA1,KA2,KB1,KB2,KR1,KR2)
C***MODIF 7-OCT-81 SO THAT (1)OUTPUT ARRAY CAN BE SAME AS INPUT
C AND (2) RESULT IS MULTIPLIED BY SCALAR
C DOUBLE PRECISION SUBROUTINE FOR MATRIX MULTIPLICATION WITH
C MATRICES IN NORMAL 2-DIMENSIONAL FORM. CAN EQUALLY USE FOR
C VECTOR*MATRIX OR VECTOR*VECTOR IF VECTORS DECLARED AS B(1,J) ETC
C	CALCS R=A*B WHERE A IS (NA X N),B IS (N X NB) SO R IS (NA X NB).
C KA1,KB1,KR1 ETC=DIMENSIONS OF A,B,R IN CALLING PROG
c Examples  of calls to MATMUL etc
c	call MATMUL(GDB,GBD,HDD,kD,kB,kD,one,
c     & km,km,km,km,km,km)
c	call MATMUL(phi,GED,row2,1,kE,kD,one,1,km,km,km,1,km)
c	call MATMUL(QBD,end1,col1,kB,kD,1,one,km,km,km,1,km,1)
c	call VECMUL(row2,unit,den,kD,km,km)	!postmult by uD
C
	real*8 A(KA1,KA2),B(KB1,KB2),R(KR1,KR2)
c	real*8 temp(1000,1000)
c	real*8 temp(10,10)
	allocatable::temp,r1,a1,b1
	real*8 temp(:,:),r1(:,:),a1(:,:),b1(:,:)
	real*8 ZERO,SCALAR
C
c	nm1=1000
c	nm2=1000
c	nm1=10
c	nm2=10
c	if(na.gt.nm1.or.nb.gt.nm2) then
c	   call BELL(1)
c	   print 20,na,nb
c20	   format(' Arrays too big for MATMUL: na,nb = ',2i8)
c	endif
      nd=max(na,nb,n,kr1,kr2,ka1,ka2,kb1,kb2)
	ALLOCATE(temp(nd,nd),r1(nd,nd),a1(nd,nd),b1(nd,nd))
c
	ZERO=0.0D0
	do i=1,ka1
	do j=1,ka2
	  a1(i,j)=a(i,j)
	
	enddo
	enddo
	do i=1,kb1
	do j=1,kb2
	  b1(i,j)=b(i,j)
	
	enddo
	enddo
	
	DO 1 I=1,NA
	DO 1 J=1,NB
	TEMP(I,J)=ZERO
	DO 2 L=1,N
2	TEMP(I,J)=TEMP(I,J)+A1(I,L)*B1(L,J)
1	CONTINUE
	DO 3 I=1,NA
	DO 3 J=1,NB
3	R1(I,J)=TEMP(I,J)*SCALAR
      do i=1,kr1
        do j=1,kr2
            R(I,J)=R1(I,J)
        enddo
      enddo
C
	DEALLOCATE(temp,r1)
	RETURN
	END


