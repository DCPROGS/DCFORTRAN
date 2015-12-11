	subroutine dARSdS(QD,tres,DARS,kA,kF,kAm,kFm,km,EXPQF)
c To evaluate [-dAR(s)/ds] at s=0 for means etc of HJC distributions.
c [GAF,SFF,GFA do not need to be args]
c SFF=I-exp(QFF*tres).
c Result is output as the kAxkA matrix, DARS
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
	PARAMETER(kAx=20)
	real*8 QD(km,km),one,det
c=	real*8 QAA(10,10),QFF(10,10),QAF(10,10),QFA(10,10),SFF(10,10)
c=	real*8 EXPQF(10,10),EXPQA(10,10)
	real*8 QAF(:,:),QFA(:,:),QAA(:,:),QFF(:,:)
	
	allocatable qaf,qfa,qaa,qff
	real*8 DARS(kAm,kAm)
	real*8 EXPQF(100,100)
	!common/detwmod/EXPQF,EXPQA		  !for DETWA,DETWF,dARSdS
c All the rest are local apart from QD
c Should they be allocated? See TMEM.FOr and TMEMA -uses much less memory
c if allocated

	ALLOCATABLE::Q1,Q2,Q3,GAF,GFA,INVQAA,INVQFF,SFF
	real*8 Q1(:,:),Q2(:,:),Q3(:,:)
	real*8 GAF(:,:),GFA(:,:),INVQAA(:,:),INVQFF(:,:),SFF(:,:)
	integer AA,AF,FA,FF

c
c EXPQF=exp(QFF*tres), and EXPQA=exp(QAA*tres) already calc in HJCEXACT
c (and values are in common/detw1/ from last call to HJCLIK)
	AA=11
	AF=15	!5=CODE FOR F(=B+C) SECTION
	FA=51
	FF=55
	one=1.0d0
	allocate(QAF(20,100),QFA(100,20),QAA(20,20),QFF(100,100))
	
	ALLOCATE(Q1(km,km),Q2(km,km),Q3(km,km),
     & GAF(kAm,kFm),GFA(kFm,kAm),INVQAA(kAm,kAm),INVQFF(kFm,kFm),
     & SFF(kFm,kFm))
c
c Calculate GAF and GFA -do explicitly (GMATn gets complicated when arrays
c all have different declared sizes)
c all GMAT2 replaced by GMAT4
c	subroutine GMAT4(AB,QM,GAB,invQAA,km1,km2,kg1,kg2,ki1,ki2)
c	call GMAT4(FA,QD,GFA,INVQFF,km,km,kFm,kAm,kFm,kFm)
	call SUBMAT(QD,AA,QAA,km,km,20,20)
	call MATINV2(QAA,kA,kAx,invQAA,kAm,.false.,det,ndscale)
	call SUBMAT(QD,AF,Q1,km,km,km,km)
	call MATMUL(invQAA,Q1,GAF,kA,kA,kF,-one,
     &	kAm,kAm,km,km,kAm,kFm)
c	call ATYPD(GAF,'   GAF= ',kA,kF,kAm,kFm)
	call SUBMAT(QD,FF,QFF,km,km,km,km)
	call MATINV2(QFF,kF,km,invQFF,kFm,.false.,det,ndscale)
	call SUBMAT(QD,FA,Q1,km,km,km,km)
	call MATMUL(invQFF,Q1,GFA,kF,kF,kA,-one,
     &	kFm,kFm,km,km,kFm,kAm)
c	call ATYPD(GFA,'   GFA= ',kF,kA,kFm,kAm)
c=== debug check
c===GMAT4may not work -declared dimensions all ok?
	call MATMUL(QAA,invQAA,Q1,kA,kA,kA,one,
     & kAx,kAx,kAm,kAm,km,km)
c	call ATYPD(Q1,'  unit= ',kA,kA,km,km)
	call MATMUL(QFF,invQFF,Q1,kF,kF,kF,one,
     & km,km,kFm,kFm,km,km)
c	call ATYPD(Q1,'  unit= ',kF,kF,km,km)
c===
c	call ATYPD(GFA,'   GFA= ',kF,kA,kFm,kAm)
c calc SFF=I-EXPQF
	do i=1,kF
	   do j=1,kF
		SFF(i,j)=-EXPQF(i,j)
		if(i.eq.j) SFF(i,j)=SFF(i,j)+one
	   enddo
	enddo
c	call ATYPD(SFF,'   SFF= ',kF,kF,kFm,kFm)
c Last term of Q1 above = tres*GAF*exp(QFF*tres)*GFA  in Q1
	call MATMUL(expQF,GFA,Q1,kF,kF,kA,tres,	  !tres*expQF*GFA in Q1
     & km,km,kFm,kAm,km,km)
c	call ATYPD(Q1,'  Q1-1= ',kF,kA,km,km)
c Correct sign of 3rd term (see above)
	call MATMUL(GAF,Q1,Q1,kA,kF,kA,-one,  !-tres*GAF*expQF*GFA in Q1
     & kAm,kFm,km,km,km,km)
c	call ATYPD(Q1,'  Q1-2= ',kA,kA,km,km)
c 2nd term of Q1 above = GAF*SFF*inv(QFF)*GFA in Q2
	call MATMUL(GAF,SFF,Q2,kA,kF,kF,one,	  !GAF*SFF in Q2
     & kAm,kFm,kFm,kFm,km,km)
c	call ATYPD(Q2,'  Q2-1= ',kA,kF,km,km)
c	call ATYPD(invQFF,' invQFF ',kF,kF,kFm,kFm)

	call MATMUL(Q2,invQFF,Q2,kA,kF,kF,one,	  !GAF*SFF*inv(QFF) in Q2
     & km,km,kFm,kFm,km,km)
c	call ATYPD(Q2,'  Q2-2= ',kA,kF,km,km)
	call MATMUL(Q2,GFA,Q2,kA,kF,kA,-one,	  !-GAF*SFF*inv(QFF)*GFA in Q2
     & km,km,kFm,kAm,km,km)
c	call ATYPD(Q2,'   Q2-3 ',kA,kA,km,km)
c first term = -inv(QAA)*GAF*SFF*GFA in Q3
	call MATMUL(invQAA,GAF,Q3,kA,kA,kF,one,	  !inv(QAA)*GAF in Q3
     & kAm,kAm,kAm,kFm,km,km)
c	call ATYPD(Q3,'   Q3-1 ',kA,kF,km,km)
	call MATMUL(Q3,SFF,Q3,kA,kF,kF,one,		   !inv(QAA)*GAF*SFF in Q3
     & km,km,kFm,kFm,km,km)
c	call ATYPD(Q3,'   Q3-2 ',kA,kF,km,km)
	call MATMUL(Q3,GFA,Q3,kA,kF,kA,-one,	   !-inv(QAA)*GAF*SFF*GFA in Q3
     & km,km,kFm,kAm,km,km)
c	call ATYPD(Q3,'   Q3-3 ',kA,kA,km,km)
c Add Q1+Q2+Q3; result in Q1
      call MATADD(Q1,Q2,Q1,kA,kA,
     & km,km,km,km,km,km)
      call MATADD(Q1,Q3,Q1,kA,kA,
     & km,km,km,km,km,km)
c	call ATYPD(Q1,'   Q1-3 ',kA,kA,km,km)
c Calc VA=I - GAF*SFF*GFA in Q2
	call MATMUL(GAF,SFF,Q2,kA,kF,kF,one,
     & kAm,kFm,kFm,kFm,km,km)
c	call ATYPD(Q2,'   Q2-4 ',kA,kF,km,km)
	call MATMUL(Q2,GFA,Q2,kA,kF,kA,one,
     & km,km,kFm,kAm,km,km)
c	call ATYPD(Q2,'   Q2-5 ',kA,kA,km,km)
      call MATIMIN(Q2,Q2,kA,km)	!(I-Q2) in Q2
c	call ATYPD(Q2,'   Q2-6 ',kA,kA,km,km)
      call MATINV2(Q2,kA,km,Q2,km,.false.,det,ndscale)	!inv(VA) in Q2
c	call ATYPD(Q2,'   Q2-7 ',kA,kA,km,km)
c Calc DARS=inv(VA)*(QAA**-2) - inv(VA)*Q1*inv(VA)*inv(QAA)
c = inv(VA)[inv(QAA) - Q1*inv(VA)]*inv(QAA)
	call MATMUL(Q1,Q2,Q3,kA,kA,kA,-one,
     & km,km,km,km,km,km)                       !-Q1*inv(VAA) in Q3
c	call ATYPD(Q3,'   Q3-4 ',kA,kA,km,km)
      call MATADD(invQAA,Q3,Q3,kA,kA,
     & kAm,kAm,km,km,km,km)                     !inv(QAA)-Q1*inv(VAA) in Q3
c	call ATYPD(Q3,'   Q3-5 ',kA,kA,km,km)
	call MATMUL(Q2,Q3,Q3,kA,kA,kA,one,
     & km,km,km,km,km,km)                       !inv(VA)*Q3 in Q3
c	call ATYPD(Q3,'   Q3-6 ',kA,kA,km,km)
	call MATMUL(Q3,invQAA,DARS,kA,kA,kA,one,
     & km,km,kAm,kAm,kAm,kAm)                   !inv(VA)*Q3*invQAA in DARS
c	call ATYPD(DARS,'  DARS= ',kA,kA,kAm,kAm)
c
	DEALLOCATE(Q1,Q2,Q3,GAF,GFA,INVQAA,INVQFF)
	RETURN
	end


