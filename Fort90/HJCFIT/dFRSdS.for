	subroutine dFRSdS(QD,tres,DFRS,kA,kF,kAm,kFm,km)
c To evaluate [-dFR(s)/ds] at s=0 for means etc of HJC shut time distributions.
c (Modified from dARSdS)
c [GAF,SFF,GFA do not need to be args]
c
c SAA=I-exp(QAA*tres).
c Result is output as the kFxkF matrix, DFRS
c
c Rest of notes are from dARSdS, so interchange A, F in them.
c
c First evaluate [dVA(s)/dS]s=0
c NOT: Q1=-inv(QAA)*GAF*SFF*GFA - GAF*SFF*inv(QFF)*GFA + tres*GAF*exp(QFF*tres)*GFA
c CORRECTION FOR SIGN OF LAST TERM (AGH 26-May-92) -should be
c Q1=-inv(QAA)*GAF*SFF*GFA - GAF*SFF*inv(QFF)*GFA - tres*GAF*exp(QFF*tres)*GFA
c Then DARS=inv(VA)*(QAA**-2) - inv(VA)*Q1*inv(VA)*inv(QAA)
c = inv(VA)[inv(QAA) - Q1*inv(VA)]*inv(QAA)
c where VA=I - GAF*SFF*GFA
c
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	PARAMETER(kAx=60)
	real*8 QD(km,km),one,det
c=	real*8 QAA(10,10),QFF(10,10),QAF(10,10),QFA(10,10),SFF(10,10)
c=	real*8 EXPQF(10,10),EXPQA(10,10)
	real*8 QAF(kAx,100),QFA(100,kAx),QAA(kAx,kAx),QFF(100,100)
	real*8 EXPQA(kAx,kAx),EXPQF(100,100)
c==	real*8 DARS(kAm,kAm)
	real*8 DFRS(kFm,kFm)
	common/detw1/QAA,QAF,QFF,QFA,EXPQF,EXPQA		  !for DETWA,DETWF,dARSdS
c All the rest are local apart from QD
c Should they be allocated? See TMEM.FOr and TMEMA -uses much less memory
c if allocated

	ALLOCATABLE::Q1,Q2,Q3,GAF,GFA,INVQAA,INVQFF,SAA
	real*8 Q1(:,:),Q2(:,:),Q3(:,:)
	real*8 GAF(:,:),GFA(:,:),INVQAA(:,:),INVQFF(:,:),SAA(:,:)
	integer AA,AF,FA,FF

c
c EXPQF=exp(QFF*tres), and EXPQA=exp(QAA*tres) already calc in HJCEXACT
c (and values are in common/detw1/ from last call to HJCLIK)
	AA=11
	AF=15	!5=CODE FOR F(=B+C) SECTION
	FA=51
	FF=55
	one=1.0d0
	ALLOCATE(Q1(km,km),Q2(km,km),Q3(km,km),
     & GAF(kAm,kFm),GFA(kFm,kAm),INVQAA(kAm,kAm),INVQFF(kFm,kFm),
     & SAA(kAm,kAm))
c
c Calculate GAF and GFA -do explicitly (GMATn gets complicated when arrays
c all have different declared sizes)
c all GMAT2 replaced by GMAT4
c	subroutine GMAT4(AB,QM,GAB,invQAA,km1,km2,kg1,kg2,ki1,ki2)
c	call GMAT4(FA,QD,GFA,INVQFF,km,km,kFm,kAm,kFm,kFm)
c Calc GFA
	call SUBMAT(QD,FF,QFF,km,km,km,km)
	call MATINV2(QFF,kF,km,invQFF,kFm,.false.,det,ndscale)
	call SUBMAT(QD,FA,Q1,km,km,km,km)
	call MATMUL(invQFF,Q1,GFA,kF,kF,kA,-one,
     &	kFm,kFm,km,km,kFm,kAm)
c	call ATYPD(GFA,'   GFA= ',kF,kA,kFm,kAm)
c calc GAF
	call SUBMAT(QD,AA,QAA,km,km,kAx,kAx)
	call MATINV2(QAA,kA,kAx,invQAA,kAm,.false.,det,ndscale)
	call SUBMAT(QD,AF,Q1,km,km,km,km)
	call MATMUL(invQAA,Q1,GAF,kA,kA,kF,-one,
     &	kAm,kAm,km,km,kAm,kFm)
c	call ATYPD(GAF,'   GAF= ',kA,kF,kAm,kFm)
c=== debug check
c===GMAT4may not work -declared dimensions all ok?
c	call MATMUL(QAA,invQAA,Q1,kA,kA,kA,one,
c     & kAx,kAx,kAm,kAm,km,km)
c	call ATYPD(Q1,'  unit= ',kA,kA,km,km)
c	call MATMUL(QFF,invQFF,Q1,kF,kF,kF,one,
c     & km,km,kFm,kFm,km,km)
c	call ATYPD(Q1,'  unit= ',kF,kF,km,km)
c
c	call ATYPD(GFA,'   GFA= ',kF,kA,kFm,kAm)
c calc SAA=I-EXPQA
	do i=1,kA
	   do j=1,kA
		SAA(i,j)=-EXPQA(i,j)
		if(i.eq.j) SAA(i,j)=SAA(i,j)+one
	   enddo
	enddo
c	call ATYPD(SAA,'   SAA= ',kA,kA,kAm,kAm)
	call MATMUL(expQA,GAF,Q1,kA,kA,kF,tres,	  !tres*expQA*GAF in Q1
     & km,km,kAm,kFm,km,km)
c	call ATYPD(Q1,'  Q1-1= ',kA,kF,km,km)
c Correct sign of 3rd term (see above)
	call MATMUL(GFA,Q1,Q1,kF,kA,kF,-one,  !-tres*GFA*expQA*GAF in Q1
     & kFm,kAm,km,km,km,km)
c	call ATYPD(Q1,'  Q1-2= ',kF,kF,km,km)
c 2nd term of Q1 above = GFA*SAA*inv(QAA)*GAF in Q2
	call MATMUL(GFA,SAA,Q2,kF,kA,kA,one,	  !GFA*SAA in Q2
     & kFm,kAm,kAm,kAm,km,km)
c	call ATYPD(Q2,'  Q2-1= ',kF,kA,km,km)
c	call ATYPD(invQAA,' invQAA ',kA,kA,kAm,kAm)

	call MATMUL(Q2,invQAA,Q2,kF,kA,kA,one,	  !GFA*SAA*inv(QAA) in Q2
     & km,km,kAm,kAm,km,km)
c	call ATYPD(Q2,'  Q2-2= ',kF,kA,km,km)
	call MATMUL(Q2,GAF,Q2,kF,kA,kF,-one,	  !-GFA*SAA*inv(QAA)*GAF in Q2
     & km,km,kAm,kFm,km,km)
c	call ATYPD(Q2,'   Q2-3 ',kF,kF,km,km)
c first term = -inv(QFF)*GFA*SAA*GAF in Q3
	call MATMUL(invQFF,GFA,Q3,kF,kF,kA,one,	  !inv(QFF)*GFA in Q3
     & kFm,kFm,kFm,kAm,km,km)
c	call ATYPD(Q3,'   Q3-1 ',kF,kA,km,km)
	call MATMUL(Q3,SAA,Q3,kF,kA,kA,one,		   !inv(QFF)*GFA*SAA in Q3
     & km,km,kAm,kAm,km,km)
c	call ATYPD(Q3,'   Q3-2 ',kF,kA,km,km)
	call MATMUL(Q3,GAF,Q3,kF,kA,kF,-one,	   !-inv(QFF)*GFA*SAA*GAF in Q3
     & km,km,kAm,kFm,km,km)
c	call ATYPD(Q3,'   Q3-3 ',kF,kF,km,km)
c Add Q1+Q2+Q3; result in Q1
      call MATADD(Q1,Q2,Q1,kF,kF,
     & km,km,km,km,km,km)
      call MATADD(Q1,Q3,Q1,kF,kF,
     & km,km,km,km,km,km)
c	call ATYPD(Q1,'   Q1-3 ',kF,kF,km,km)
c Calc VF=I - GFA*SAA*GAF in Q2
	call MATMUL(GFA,SAA,Q2,kF,kA,kA,one,
     & kFm,kAm,kAm,kAm,km,km)
c	call ATYPD(Q2,'   Q2-4 ',kF,kA,km,km)
	call MATMUL(Q2,GAF,Q2,kF,kA,kF,one,
     & km,km,kAm,kFm,km,km)
c	call ATYPD(Q2,'   Q2-5 ',kF,kF,km,km)
      call MATIMIN(Q2,Q2,kF,km)	!(I-Q2) in Q2
c	call ATYPD(Q2,'   Q2-6 ',kF,kF,km,km)
      call MATINV2(Q2,kF,km,Q2,km,.false.,det,ndscale)	!inv(VA) in Q2
c	call ATYPD(Q2,'   Q2-7 ',kF,kF,km,km)
c Calc DFRS=inv(VF)*(QFF**-2) - inv(VF)*Q1*inv(VF)*inv(QFF)
c = inv(VF)[inv(QFF) - Q1*inv(VF)]*inv(QFF)
	call MATMUL(Q1,Q2,Q3,kF,kF,kF,-one,
     & km,km,km,km,km,km)                       !-Q1*inv(VFF) in Q3
c	call ATYPD(Q3,'   Q3-4 ',kF,kF,km,km)
      call MATADD(invQFF,Q3,Q3,kF,kF,
     & kFm,kFm,km,km,km,km)                     !inv(QFF)-Q1*inv(VFF) in Q3
c	call ATYPD(Q3,'   Q3-5 ',kF,kF,km,km)
	call MATMUL(Q2,Q3,Q3,kF,kF,kF,one,
     & km,km,km,km,km,km)                       !inv(VF)*Q3 in Q3
c	call ATYPD(Q3,'   Q3-6 ',kF,kF,km,km)
	call MATMUL(Q3,invQFF,DFRS,kF,kF,kF,one,
     & km,km,kFm,kFm,kFm,kFm)                   !inv(VF)*Q3*invQFF in DFRS
c	call ATYPD(DARS,'  DFRS= ',kF,kF,kFm,kFm)
c
	DEALLOCATE(Q1,Q2,Q3,GAF,GFA,INVQAA,INVQFF)
	RETURN
	end


