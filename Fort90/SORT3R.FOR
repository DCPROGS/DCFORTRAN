	SUBROUTINE SORT3R(A,B,N,KM,absval,up)
c 06/21/89 05:30pm Lahey version
c
c Sorts into ascending order of A if UP=true, otherwise descending order
c SORT3R is version of SORT3 in which 2nd array is real (B) not integer.
c Version of SORT1 that
c	 (a) uses abs values if ABSVAL true
c	 (b) sorts into descending order
c Single precision version of SORTD1
C VERSION OF SHSRTD IN WHICH DECLARED DIMENSION OF ARRAYS (=KM)
C IS GIVEN AS A PARAM. N=NO OF VALUES TO BE SORTED
	real*4 A(km),B(km)
	logical absval,up
C	FORTRAN TRANSLATION OF SHELLSORT (D.C. 28-JUN-77)
C	VERSION THAT SORTS INTEGER ARRAY (E.G.EXAM NUMBER OR NAME)
C	IN SAME ORDER AS ARRAY A-ascending order
C
C	DO 1 I=1,N,I
C1	M=2*I-1
C SHOULD GET I=1,2,4,8,16...
C NEXT BIT DOES SEEM TO WORK
	I1=1
	DO 1 I=1,N,I1
	I1=I
1	M=2*I-1
C
	if(absval) goto 10	!sep section for speed
2	M=M/2
	IF(M.EQ.0) GOTO 3
	K=N-M
	DO 4 J=1,K
	DO 5 I=J,1,-M
c in next line, (1) abs() for abs values,
c 		(2) .LT. for descending order (.GE. for ascending)
	IF(A(I+M).LT.A(I)) GOTO 4
	W=A(I)
	A(I)=A(I+M)
	A(I+M)=W
	bn=B(i)
	B(i)=B(i+m)
	B(i+m)=bn
5	CONTINUE	!I LOOP
4	CONTINUE	!J LOOP
	GOTO 2		!M LOOP
3	CONTINUE
	goto 99
c
10	continue
21	M=M/2
	IF(M.EQ.0) GOTO 31
	K=N-M
	DO 41 J=1,K
	DO 51 I=J,1,-M
c in next line, (1) abs() for abs values,
c 		(2) .LT. for descending order (.GE. for ascending)
	IF(abs(A(I+M)).LT.abs(A(I))) GOTO 41
	W=A(I)
	A(I)=A(I+M)
	A(I+M)=W
	bn=B(i)
	B(i)=B(i+m)
	B(i+m)=bn
51	CONTINUE	!I LOOP
41	CONTINUE	!J LOOP
	GOTO 21		!M LOOP
31	CONTINUE
c
c If ascending order required now reverse order
	if(.not.UP) RETURN
99	continue
	do 200 i=1,n/2
	i1=n-i+1	!=n,n-1,...,1
	a1=a(i)
	a(i)=a(i1)
	a(i1)=a1
	bn=B(i)
	B(i)=B(i1)
	B(i1)=bn
200	continue
	RETURN
	END
