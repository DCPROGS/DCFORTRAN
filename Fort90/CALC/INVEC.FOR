	subroutine INVEC(iset,Phi0,row1,P0,skip,kdim)
c Lahey version 12/11/91 06:14pm
c Altered Aug 88 for 11-73 so P0 now double prec
C TO CALC INIT VECTOR AT ZERO TIME FROM INIT OCCS IN P0()
C RESULT OUTPUT IN BOTH P0(J) AND IN ROW1(1,J) FOR USE IN SCJUMP
C   ISET=1 FOR A; iset=2 for B; iset=3 for C and iset=5 for F
	REAL*8 Phi0(KDIM),ROW1(1,KDIM),P0(100),S
	logical SKIP
	logical discprt
c	logical debug,caplock
c
	common/KBLK/kA,kB,kC,kD
	common/dp/discprt
C
c	debug=caplock()
	k=kA+kB+kC
C
	if(iset.eq.1) then
	  n1=1
	  n2=kA
	else if(iset.eq.2) then
	  n1=kA+1
	  n2=kA+kB
	else if(iset.eq.3) then
	  n1=kA+kB+1
	  n2=k
	else if(iset.eq.5) then
	  n1=kA+1
	  n2=k
	else
	   call BELL(2)
         print 3,iset
         if(discprt) write(8,3) iset
3	   format(' Invalid set number in INVEC = ',i8)
	   STOP
	endif
	S=0.0D0
	DO 651 I=N1,N2
651	S=S+P0(I)		!TOTAL OCC OF SUBSET
	SKIP=S.lt.1.0d-20
	if(skip) RETURN			!all occs zero
C
c	if(debug) then
c         print 1,S
c         if(discprt) write(8,1) S
c1	   format(' In INVEC, S = ',g13.6)
c	endif
c
	j=1
	do 661 i=n1,n2
	 Phi0(j)=0.0d0
	 if(S.gt.1.0d-20) Phi0(j)=P0(i)/S
	 ROW1(1,j)=Phi0(j)
	 j=j+1
c	if(debug) then
c         print 2,i,p0(i),phi0(j)
c         if(discprt) write(8,2) i,p0(i),phi0(j)
c2	   format(' In INVEC, i,num,num/S = ',i3,2g13.6)
c	endif
661	continue
c
	RETURN
	END

