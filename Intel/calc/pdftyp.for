	SUBROUTINE PDFTYP(F,W,R,LI,LJ,K,NOMEAN,KDIM)
C
C***MODIF 26-NOV-84 TO INCLUDE KDIM= DIMENSION OF W,R IN MAIN PROG
C***MODIF 16-OCT-81 SO LI,LJ SET NEGATIVE (RATHER THAN ZERO) IF NOT REQ
C	L1=I LJ=J GIVES F(I,J)
C	L1=I LJ=-1 GIVES F(I)
C	L1=-1 LJ=-1 GIVES F
C	L1=-2 LJ=-1 GIVES F(inf)
C
C SUBROUTINE TO TYPE A SINGLE PDF IN FORM
C	F=W(1)EXP(-R(1)) + W(2)EXP(-R(2)) +......
C F=NAME(A4) OF 4 CHAR. E.G.'  FA' IN CALL
C K=NO OF TERMS
C NOMEAN=0 FOR PDF BUT NON-ZERO FOR FUNCTIONS OF TIME THAT
C	ARE NOT PDFS SO MEAN AND AREA NOT CALC IN LATTER CASE
C LI,LJ USED FOR LABELLING (TO REVERT TO ORIG STATE NUMBERING
C IN SCGEN1, CALL WITH INEW(LI),INEW(LJ) RATHER THAN LI,LJ).
C FOR SINGLY COND CALL WITH J=NEG
C FOR UNCOND CALL WITH I,J BOTH NEG.
C
	REAL W(KDIM),R(KDIM)
	logical pon
	logical discprt
	common/dp/discprt
c
	pon=.false.
C
C TYPE LHS
	IF(LI.EQ.-1.AND.LJ.EQ.-1) print 1,F
	IF(LI.GE.0.AND.LJ.LT.0) print 2,F,LI
	IF(LI.GE.0.AND.LJ.GE.0) print 3,F,LI,LJ
	IF(LI.EQ.-2.AND.LJ.EQ.-1) print 4,F
	if(.not.pon) goto 50
	IF(LI.EQ.-1.AND.LJ.EQ.-1) write(7,1)F
      if(discprt.and.LI.eq.-1.and.LJ.eq.-1) write(7,1)F 
	IF(LI.GE.0.AND.LJ.LT.0) write(7,2)F,LI
      if(discprt.and.LI.ge.0.and.LJ.lt.0) write(7,2)F,LI 
	IF(LI.GE.0.AND.LJ.GE.0) write(7,3)F,LI,LJ
      if(discprt.and.LI.ge.0.and.LJ.ge.0) write(7,3)F,LI,LJ 
	IF(LI.EQ.-2.AND.LJ.EQ.-1) write(7,4)F
      if(discprt.and.LI.eq.-2.and.LJ.eq.-1) write(7,4)F 
1	FORMAT( 1X,A4,'=        ')
2	FORMAT( 1X,A4,'(',I2,')=    ')
3	FORMAT( 1X,A4,'(',I2,',',I2,')= ')
4	FORMAT( 1X,A4,'(inf)=    ')
C
C TEST INDETERMINACY- ALL W(M) WILL HAVE BEEN SET TO ZERO
50	DO 10 M=1,K
10	IF(ABS(W(M)).GT.1.E-10) GOTO 11	!OUT OF LOOP IF ANY W>0
	print 12
	if(pon) write(7,12)
	if(discprt) write(7,12)
12	FORMAT('  INDETERMINATE')
	RETURN
C
11	DO 41 M=1,K
	IF(M.EQ.K) GOTO 51
C ROOM FOR 2 TERMS/LINE. FIRST TERM AFTER TITLE-
	IF(M.EQ.1) print 6,W(M),R(M)
	IF(pon.and.M.EQ.1) write(7,6)W(M),R(M)
      if(discprt.and.M.EQ.1) write(7,6)W(M),R(M) 
6	FORMAT(1X,G12.5,'exp(',G10.3,'*T) + ')
C NEXT 2ND (LAST) TERM ON EACH LINE
	IF(MOD(M,2).EQ.0) print 61,W(M),R(M)
	IF(pon.and.MOD(M,2).EQ.0) write(7,61)W(M),R(M)
      if(discprt.and.MOD(M,2).EQ.0) write(7,61)W(M),R(M) 
61	FORMAT(1X,G12.5,'exp(',G10.3,'*T) + ')
C NEXT FIRST TERM ON ALL LINES BUT FIRST
	IF(M.GT.1.AND.MOD(M,2).EQ.1) print 62,W(M),R(M)
	IF(pon.and.M.GT.1.AND.MOD(M,2).EQ.1) write(7,62)W(M),R(M)
      if(discprt.and.M.GT.1.AND.MOD(M,2).EQ.1) write(7,62)W(M),R(M) 
62	FORMAT(14X,G12.5,'exp(',G10.3,'*T) + ')
	GOTO 41
C NEXT SECTION TO PRINT THE LAST TERM
51	IF(M.GT.1.AND.MOD(M,2).EQ.1) GOTO 52
	print 63,W(M),R(M)
	if(pon) write(7,63)W(M),R(M)
      if(discprt) write(7,63)W(M),R(M) 
63	FORMAT(1X,G12.5,'exp(',G10.3,'*T)')
	GOTO 41
52	print 64,W(M),R(M)
	if(pon) write(7,64)W(M),R(M)
      if(discprt) write(7,64)W(M),R(M) 
64	FORMAT(14X,G12.5,'exp(',G10.3,'*T)')
41	CONTINUE
C
	IF(NOMEAN.NE.0) RETURN
C	print 7
C7	FORMAT(/)
C
C CALC AREA AND MEAN AND TYPE THEM
	AREA=0.0
	AMEAN=0.0
	print 91
	if(pon) write(7,91)
	if(discprt) write(7,91)
91	FORMAT( '   Area= ')
	DO 8 M=1,K
	A=W(M)/ABS(R(M))
	IF(M.EQ.K) GOTO 92	!LAST TERM
	print 93,A
	if(pon) write(7,93)A
      if(discprt) write(7,93)A 
93	FORMAT(F8.4,' + ')
	IF(M.EQ.7) print 97	!MAX OF TERMS FIT ON ONE LINE
	IF(pon.and.M.EQ.7) write(7,97)	!MAX OF TERMS FIT ON ONE LINE
	IF(discprt.and.M.EQ.7) write(7,97)
97	FORMAT(/,9X)
	GOTO 95
92	IF(K.EQ.1) print 96
	IF(pon.and.K.EQ.1) write(7,96)
	IF(discprt.and.K.EQ.1) write(7,96)
96	FORMAT(' = ')
	IF(K.GT.1) print 94,A
	IF(pon.and.K.GT.1) write(7,94)A
      if(discprt.and.K.GT.1) write(7,94)A 
94	FORMAT(F8.4,' = ')
95	AREA=AREA+A
	AMEAN=AMEAN+W(M)/(R(M)*R(M))
8	CONTINUE
	print 9,AREA,AMEAN
	if(pon) write(7,9)AREA,AMEAN
      if(discprt) write(7,9)AREA,AMEAN 
9	FORMAT( F9.6/'   Mean= ',G13.6,/)
	RETURN
	END

