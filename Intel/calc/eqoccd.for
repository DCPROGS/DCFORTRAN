	SUBROUTINE EQOCCD(Q,K,K1,KD,Pinf)
c
c 06/21/89 11:03am Lahey version
c
c This version differs from that used in CQFITV in that output
c is Real*8 also (and PINF is no longer virtual internally)
c
c Modif 11/22/01 10:11am to remove call to MRTEST, now that not
c all cycles are forced to obey MR (MRTEST better called in main progs)
c
c**EQOCDV.FOR uses DETV.FOR for determinant calc- needs
c calls to ARAYDV for storage mode. QM and WORK must be
c declared as 9,9 here and in PRNMIN to work.
c**EQOCDV.FOR contains real*8 files eqocc,prnmin,determ
c like EQOCCD, but has virtual arrays internally ( and
c as param QM,WORK)
c NB args of SCLPRD are virtual array ELEMENTS so must
c be assoc with non-virtual array in sclprd (see manual)
c	real*8 Q(10,10),QM(9,9),det(10)
c	integer nd(10)
c	real*8 pinf(10)
	real*8 Q(kd,kd),pinf(kd)
	allocatable::QM,det
	real*8 QM(:,:),det(:)
	allocatable lwork,mwork,nd
	integer lwork(:),mwork(:),nd(:)
	real*8 S,D,S1
c	logical fail
	character subname*8
	logical discprt
	common/dp/discprt
c	common/crtblk/crtmax,ifcall
c	Common/crtblk/ conveys m.r.errors to calling prog
c If EQOCC is called by a function subroutine set ifcall=1
c to prevent output within EQOCC (which will give error)
c otherwise ifcall=0
C
C TO CALCULATE EQUILIBRIUM OCCUPANCIES FROM Q MATRIX USING
C DETERMINANTS OF PRINCIPLE MINORS.
C CALLS DETERM (WHICH CALLS DETERM1 AND SCLPRD) AND PRNMIN
C***MODIF 21-JAN-85 SO Q,PINF ARE REAL*8
C***** MANY PROBLEMS WITH ARRAY DIMENSIONS MAINLY BECAUSE
C SCLPRD REQUIRES ARRAYS TO BE EXACT SIZE SO NO ZEROES IN
C LINEAR STORAGE SEQUENCE- SEEMS THAT FOR ADJUSTABLE ARRAY
C DIMENSIONS TO WORK PROPERLY, ARRAY MUST BE DECLARED
C ORIGINALLY IN MAIN PROG, NOT LOCALLY IN A SUBROUTINE SUCH
C AS THIS. SO QM NOW IN MAIN PROG AND CALL EQOCC WITH K1=K-1
C KD=DIMENSION OF Q AND QM AS DELARED IN CALLING PROG
c
	ALLOCATE(QM(kd,kd),det(kd),lwork(kd),mwork(kd),nd(kd))
cC COPY INPUT
c	do 4 i=1,k
c	do 4 j=1,k
c4	Q(i,j)=dble(QIN(i,j))
c	call atyp2(QIN,' QIN',.false.,k,k,kd,kd)
c
	DO 1 M=1,K
	I=M
	CALL PRNMIN(Q,QM,I,K,K-1,KD)
c	print 103
c103	FORMAT( '  IN EQOCC, QM(I,J)=')
c	DO 100 I=1,K-1
c100	print 101,(QM(I,J),J=1,K-1)
c101	FORMAT(1x,5G13.6)
c	CALL DETERM(QM,K-1,DET,WORK,IFAIL)
	CALL ARRAYD(2,K-1,K-1,KD-1,KD-1,QM,QM)	!convert to 1-D storage
c
c DETV altered to prevent overflow: value of DET
c which is output must be mult by 10**ndexp to get correct determinant
c NB may not cancel, as may not be same for every minor
	CALL DETV(QM,K-1,D,LWORK,MWORK,ndexp)
	nd(m)=ndexp
	det(m)=D
c QM destroyed- not needed again
c
1	CONTINUE
C
c Now check which is largest scaling factor in nd(m) and adjust all
c other scaling factors to same value (NB nd>=0)
	ndmax=0
	do 3 m=1,k
3	if(iabs(nd(m)).gt.ndmax) ndmax=nd(m)
c adjust so all DET are too small by 10**ndmax, so their sum is too
	s=0.0d0
	do 4 m=1,k
	det(m)=det(m)*(10.0**(nd(m)-ndmax))
	pinf(m)=det(m)		!temporarily
	s=s+det(m)
4	continue
c
	S1=0.0D0
cD	print 104,S
cD104	FORMAT( ' DENOM= ',G13.6)
	DO 2 M=1,K
	PINF(M)=PINF(M)/S	!factor of 10**ndmax cancels
2	S1=S1+PINF(M)
c	print 105,S1
c105	FORMAT( ' TOTAL OCC= ',D20.12)
c
C
C CHECK MICRO REV
	kdim=kd
c	if(obeymr) then
c	   ifcall=2		!simple printout only
c	   subname='EQOCCD  '
c	   call MRTEST(Q,pinf,subname,ifcall,k,kdim)
c	endif
c
	DEALLOCATE(QM,det,lwork,mwork,nd)
99	RETURN
C
	END



	SUBROUTINE PRNMIN(Q,QMIN,M,K,K1,KD)
C
C OUTPUTS IN QMIN THE M'TH PRINCIPLE MINOR OF Q(K,K)
C I.E. THE (K-1)*(K-1) MATRIX FOUND BY OMITTING THE M'TH
C ROW AND COLUMN OF Q.
C***MODIF 21-JAN-85 SO Q IS REAL*8
C QMIN IS REAL*8 FOR CALC OF DETERMINANT
C SET K1=K-1 IN CALL FOR DIMENSION OF QMIN
C IN CALLING PROG MUST HAVE Q(KD,KD) AND QMIN MUST HAVE
C BEEN DECLARED IN MAIN PROG FOR CORRECT FUNCTION
C
	real*8 Q(kd,kd),QMIN(kd-1,kd-1)
C
	I1=1
	DO 1 I=1,K
	IF(I.EQ.M) GOTO 1	!OMIT WHOLE MTH ROW
	J1=1
	DO 2 J=1,K
	IF(J.EQ.M) GOTO 2
C	QMIN(I1,J1)=DBLE(Q(I,J))
	QMIN(I1,J1)=Q(I,J)
	J1=J1+1
2	CONTINUE
	I1=I1+1
1	CONTINUE
	RETURN
	END



