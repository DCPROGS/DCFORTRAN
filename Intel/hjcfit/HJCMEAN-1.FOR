	subroutine HJCMEAN(hmopen,hmshut,QD,tres,
     &   k,kA,kF,kAm,kFm,km)
c     all these args are defined in hjcdisp for call
c	subroutine HJCMEAN(g00,g10,g11,eigen,tres,k,km),===
c
c To calculate exact mean open or shut time from HJC pdf
c Called in HJCDISP

cc g00 (etc) in call is g00A for open times, g00F for shut times, for exact pdf
cc amp() tau() are for asymptotic pdf (calc in HJCDISP) -as in hjcmean.mcd
c NO -much better, and exact method, is to use dARSdS for open times (as
c in MOPADJC for conditional mean open times, but simpler!).
c And write equivalant dFRSdS for shut times.
c
c Modif 08/10/04 11:03am, so that QEXPQA, QEXPQF, phiA, phiF are all
c recalculated here, locally, from current QD (if conc changed, QD may not
c correspond to global values of these matrices).
c They are removed from arguments.
c Also make special versions of dARSdS and dARFdS that are local to
c this subroutine and have QAA, EXPQA atc as arguments, to avoid
c corruption of the global values in common/detw1/
c
c Output = overall mean open and shut times in real*8 hmopen, hmshut (msec)
c
c For mean open
c  E(topen) = phiA*{[-d((e)GAF(s)/ds]s=0}*uF
c  		= tres + phiA*{[-d(A)R(s)/ds]s=0}*QAF*exp(QFF*tres)*uF
c and analagously for for E(tshut)
c
c EXPQF=exp(QFF*tres), and EXPQA=exp(QAA*tres) already calc in HJCEXACT
c (and values are in common/detw1/ from last call to HJCLIK)
c
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 QD(100,100)
	real*8 ucol(100,1),col1(100,1)
	allocatable:: QAF,QFA,QAA,QFF
	real*8 QAF(:,:),QFA(:,:),QAA(:,:),QFF(:,:)
	allocatable::GAF,GFA
	real*8 GAF(:,:),GFA(:,:)
	allocatable::invQAA,invQFF
	real*8 invQAA(:,:),invQFF(:,:)

C=	common/phiblk/phiF(1,100),phiA(1,100)	!now local
	allocatable::DARS,DFRS,Q1,Q2
c
	allocatable::QEXPQA,QEXPQF,phiF,phiA		!local
	real*8 DARS(:,:),DFRS(:,:),Q1(:,:),Q2(:,:),phiF(:,:),phiA(:,:)
	real*8 QEXPQA(:,:),QEXPQF(:,:),tres
	allocatable EXPQA,EXPQF
	real*8 EXPQA(:,:),EXPQF(:,:)
c==	common/detw1/QAA,QAF,QFF,QFA,EXPQF,EXPQA		  !for DETWA,DETWF,dARSdS
	allocatable::FMAT,EIGFF,AMAT,EIGAA
	real*8 FMAT(:,:,:), EIGFF(:),AMAT(:,:,:), EIGAA(:),hmopen,hmshut
	INTEGER AF,FA,AA,FF
	logical deb
c
	deb=.false.
	AA=11
	AF=15		!5=CODE FOR F(=B+C) SECTION
	FA=51
	FF=55
	one=1.d0
	do i=1,km
	   ucol(i,1)=one
	enddo
	hmopen=0.d0
	hmshut=0.d0
	kXm=kam+kfm
	kmx=max(kam,kfm)
	ALLOCATE(DARS(kmx,kmx),DFRS(kmx,kmx))
	ALLOCATE(QEXPQA(kmx,kMx),QEXPQF(kmx,kmx),
     &	phiF(1,100),phiA(1,100))
	ALLOCATE(EXPQA(kmx,kmx),EXPQF(kmx,kmx))	!differnt from dARSdS
	ALLOCATE(FMAT(kmx,kmx,kmx),EIGFF(kmx),
     &	AMAT(kmx,kmx,kmx),EIGAA(kmx))
	ALLOCATE(GAF(kmx,kmx),GFA(kmx,kmx))
	ALLOCATE(invQAA(kmx,kmx),invQFF(kmx,kmx))
	ALLOCATE(QAF(kmx,kmx),QFA(kmx,kmx),QAA(kmx,kmx),QFF(kmx,kmx))
	
	ALLOCATE(Q1(kXm,kXm),Q2(kXm,kXm))
c

c First recalculate phiA, phiF from current QD, as in hjcasymp
c Calculate GAF and GFA -do explicitly (GMATn gets complicated when arrays
c all have different declared sizes)
	call SUBMAT(QD,AA,QAA,km,km,kmx,kmx)
	call SUBMAT(QD,AF,QAF,km,km,kmx,kmx)
	call MATINV2(QAA,kmx,kmx,invQAA,kmx,.false.,det,ndscale)
	call MATMUL(invQAA,QAF,GAF,kmx,kmx,kmx,-one,
     &	kmx,kmx,kmx,kmx,kmx,kmx)

	call SUBMAT(QD,FF,QFF,km,km,kmx,kmx)
	call SUBMAT(QD,FA,QFA,km,km,kmx,kmx)
	call MATINV2(QFF,kmx,kmx,invQFF,kmx,.false.,det,ndscale)
	call MATMUL(invQFF,QFA,GFA,kmx,kmx,kmx,-one,
     &	kmx,kmx,kmx,kmx,kmx,kmx)
c Expand QFF
	call QMAT5(QFF,Fmat,kmx,eigFF,ibad,kmx,kmx,kmx)
c Calc exp(QFF*tres) in EXPQF
	do i=1,kmx
	   do j=1,kmx
		EXPQF(i,j)=0.d0
		do m=1,kmx
		   EXPQF(i,j)=EXPQF(i,j) + FMAT(i,j,m)*dexp1(eigFF(m)*tres)
		enddo
	   enddo
	enddo
c Expand QAA
	call QMAT5(QAA,Amat,kmx,eigAA,ibad,kmx,kmx,kmx)
c Calc exp(QAA*tres) in EXPQA
	do i=1,kmx
	   do j=1,kmx
		EXPQA(i,j)=0.d0
		do m=1,kmx
		   EXPQA(i,j)=EXPQA(i,j) + AMAT(i,j,m)*dexp1(eigAA(m)*tres)
		enddo
	   enddo
	enddo
c Calc INITIAL VECTOR: first calc eGAF*eGFA
c Use subroutine to get eGAF in Q1
	call CeGAF(Q1,GAF,GFA,kmx,kmx,EXPQF,
     & kXm,kXm,kmx,kmx,kmx,kmx,kmx,kmx)
	if(deb) call ATYPD(Q1,' eGAF   ' ,kmx,kmx,kXm,kXm)
c Now get eGFA by repeating with A and F interchanged
c Use subroutine to get eGFA in Q2
	call CeGAF(Q2,GFA,GAF,kmx,kmx,EXPQA,
     & kXm,kXm,kmx,kmx,kmx,kmx,kmx,kmx)
	if(deb) call ATYPD(Q2,' eGFA   ' ,kmx,kmx,kXm,kXm)
	call HJCphi(Q1,Q2,kmx,kmx,phiA,kXm,kXm,km)
c
c Repeat for phiF
	call HJCphi(Q2,Q1,kmx,kmx,phiF,kXm,kXm,km)
c
c Second recalculate QEXPQA,QEXPQF from current QD
c Get QAF*EXPQF in QEXPQF: this is kmx*kmx
      call MATMUL(QAF,EXPQF,QEXPQF,kmx,kmx,kmx,one,
     &   kmx,kmx,kmx,kmx,kmx,kmx)
c Get QFA*EXPQA in QEXPQA -kmx*kmx
      call MATMUL(QFA,EXPQA,QEXPQA,kmx,kmx,kmx,one,
     &   kmx,kmx,kmx,kmx,kmx,kmx)
c
c Now ready to calculate means
c Mean open
c local version of darsds, see below
	call dARSdS1(QD,tres,DARS,QAA,QAF,QFF,QFA,EXPQF,EXPQA,
     &	kmx,kmx,kmx,kmx,km)
c Already have QAF*exp(QFF*tres) in QEXPQF (kmx*kmx)
      call MATMUL(DARS,QEXPQF,Q1,kmx,kmx,kmx,one,kmx,kmx,kmx,kmx,kXm,
     & kXm)
c Need Q1*uF -keep in col1 (kmx*1)
      call MATMUL(Q1,ucol,col1,kmx,kmx,1,one,kXm,kXm,km,1,km,1)
c premultiply my phiA
	call VECMUL(phiA,col1,hmopen,kmx,km,km)
c Add tres
	hmopen=tres + hmopen
c
c Now same for mean shut
	call dFRSdS1(QD,tres,DFRS,QAA,QAF,QFF,QFA,EXPQF,EXPQA,
     & kmx,kmx,kmx,kmx,km)
c Already have QFA*exp(QAA*tres) in QEXPQA (kmx*kmx)
      call MATMUL(DFRS,QEXPQA,Q1,kmx,kmx,kmx,one,kmx,kmx,kmx,kmx,kXm,
     & kXm)
c Need Q1*uA -keep in col1 (kmx*1)
      call MATMUL(Q1,ucol,col1,kmx,kmx,1,one,kXm,kXm,km,1,km,1)
c premultiply my phiF
	call VECMUL(phiF,col1,hmshut,kmx,km,km)
c Add tres
	hmshut=tres + hmshut
c
c Convert to milliseconds before return
	hmopen=1.d3*hmopen
	hmshut=1.d3*hmshut
c
	DEALLOCATE(DARS,DFRS,Q1,Q2,QEXPQA,QEXPQF,phiF,phiA)
	DEALLOCATE(EXPQA,EXPQF)
	DEALLOCATE(FMAT,EIGFF,AMAT,EIGAA)
	DEALLOCATE(GAF,GFA,invQAA,invQFF)
	DEALLOCATE(QAF,QFA,QAA,QFF)
	return
	end

	subroutine dARSdS1(QD,tres,DARS,QAA,QAF,QFF,QFA,EXPQF,EXPQA,
     & kmx,kmx1,kmx2,kmx3,km)

c To evaluate [-dAR(s)/ds] at s=0 for means etc of HJC distributions.
c [GAF,SFF,GFA do not need to be args]
c SFF=I-exp(QFF*tres).
c Result is output as the kmxxkmx matrix, DARS
c
c DARSDS1 is local version within HJCMEAN that has common/detw1/ removed
c and values passed as arguments (dimensions not same as in dARSdS)
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
c	PARAMETER(kmxx=60)		!arrays changed so not used now
	real*8 QD(100,100),one,det
c=	real*8 QAA(10,10),QFF(10,10),QAF(10,10),QFA(10,10),SFF(10,10)
c=	real*8 EXPQF(10,10),EXPQA(10,10)
c=	real*8 QAF(kmxx,100),QFA(100,kmxx),QAA(kmxx,kmxx),QFF(100,100)
c=	real*8 EXPQA(kmxx,kmxx),EXPQF(100,100)
	real*8 QAF(kmx,kmx),QFA(kmx,kmx),QAA(kmx,kmx),QFF(kmx,kmx)
	real*8 EXPQA(kmx,kmx),EXPQF(kmx,kmx)
	real*8 DARS(kmx,kmx)
c===	common/detw1/QAA,QAF,QFF,QFA,EXPQF,EXPQA		  !for DETWA,DETWF,dARSdS
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
	ALLOCATE(Q1(km,km),Q2(km,km),Q3(km,km),
     & GAF(kmx,kmx),GFA(kmx,kmx),INVQAA(kmx,kmx),INVQFF(kmx,kmx),
     & SFF(kmx,kmx))
c
c Calculate GAF and GFA -do explicitly (GMATn gets complicated when arrays
c all have different declared sizes)
c all GMAT2 replaced by GMAT4
c	subroutine GMAT4(AB,QM,GAB,invQAA,km1,km2,kg1,kg2,ki1,ki2)
c	call GMAT4(FA,QD,GFA,INVQFF,km,km,kmx,kmx,kmx,kmx)
	call SUBMAT(QD,AA,QAA,km,km,kmx,kmx)
	call MATINV2(QAA,kmx,kmx,invQAA,kmx,.false.,det,ndscale)
	call SUBMAT(QD,AF,Q1,km,km,km,km)
	call MATMUL(invQAA,Q1,GAF,kmx,kmx,kmx,-one,
     &	kmx,kmx,km,km,kmx,kmx)
c	call ATYPD(GAF,'   GAF= ',kmx,kmx,kmx,kmx)
	call SUBMAT(QD,FF,QFF,km,km,kmx,kmx)
	call MATINV2(QFF,kmx,kmx,invQFF,kmx,.false.,det,ndscale)
	call SUBMAT(QD,FA,Q1,km,km,km,km)
	call MATMUL(invQFF,Q1,GFA,kmx,kmx,kmx,-one,
     &	kmx,kmx,km,km,kmx,kmx)
c	call ATYPD(GFA,'   GFA= ',kmx,kmx,kmx,kmx)
c check
	call MATMUL(QAA,invQAA,Q1,kmx,kmx,kmx,one,
     & kmx,kmx,kmx,kmx,kmx,kmx)
c	call ATYPD(Q1,'  unit= ',kmx,kmx,km,km)
	call MATMUL(QFF,invQFF,Q1,kmx,kmx,kmx,one,
     & kmx,kmx,kmx,kmx,km,km)
c	call ATYPD(Q1,'  unit= ',kmx,kmx,km,km)
c
c	call ATYPD(GFA,'   GFA= ',kmx,kmx,kmx,kmx)
c calc SFF=I-EXPQF
	do i=1,kmx
	   do j=1,kmx
		SFF(i,j)=-EXPQF(i,j)
		if(i.eq.j) SFF(i,j)=SFF(i,j)+one
	   enddo
	enddo
c	call ATYPD(SFF,'   SFF= ',kmx,kmx,kmx,kmx)
c Last term of Q1 above = tres*GAF*exp(QFF*tres)*GFA  in Q1
	call MATMUL(expQF,GFA,Q1,kmx,kmx,kmx,tres,	  !tres*expQF*GFA in Q1
     & kmx,kmx,kmx,kmx,km,km)
c	call ATYPD(Q1,'  Q1-1= ',kmx,kmx,km,km)
c Correct sign of 3rd term (see above)
	call MATMUL(GAF,Q1,Q1,kmx,kmx,kmx,-one,  !-tres*GAF*expQF*GFA in Q1
     & kmx,kmx,km,km,km,km)
c	call ATYPD(Q1,'  Q1-2= ',kmx,kmx,km,km)
c 2nd term of Q1 above = GAF*SFF*inv(QFF)*GFA in Q2
	call MATMUL(GAF,SFF,Q2,kmx,kmx,kmx,one,	  !GAF*SFF in Q2
     & kmx,kmx,kmx,kmx,km,km)
c	call ATYPD(Q2,'  Q2-1= ',kmx,kmx,km,km)
c	call ATYPD(invQFF,' invQFF ',kmx,kmx,kmx,kmx)

	call MATMUL(Q2,invQFF,Q2,kmx,kmx,kmx,one,	  !GAF*SFF*inv(QFF) in Q2
     & km,km,kmx,kmx,km,km)
c	call ATYPD(Q2,'  Q2-2= ',kmx,kmx,km,km)
	call MATMUL(Q2,GFA,Q2,kmx,kmx,kmx,-one,	  !-GAF*SFF*inv(QFF)*GFA in Q2
     & km,km,kmx,kmx,km,km)
c	call ATYPD(Q2,'   Q2-3 ',kmx,kmx,km,km)
c first term = -inv(QAA)*GAF*SFF*GFA in Q3
	call MATMUL(invQAA,GAF,Q3,kmx,kmx,kmx,one,	  !inv(QAA)*GAF in Q3
     & kmx,kmx,kmx,kmx,km,km)
c	call ATYPD(Q3,'   Q3-1 ',kmx,kmx,km,km)
	call MATMUL(Q3,SFF,Q3,kmx,kmx,kmx,one,		   !inv(QAA)*GAF*SFF in Q3
     & km,km,kmx,kmx,km,km)
c	call ATYPD(Q3,'   Q3-2 ',kmx,kmx,km,km)
	call MATMUL(Q3,GFA,Q3,kmx,kmx,kmx,-one,	   !-inv(QAA)*GAF*SFF*GFA in Q3
     & km,km,kmx,kmx,km,km)
c	call ATYPD(Q3,'   Q3-3 ',kmx,kmx,km,km)
c Add Q1+Q2+Q3; result in Q1
      call MATADD(Q1,Q2,Q1,kmx,kmx,
     & km,km,km,km,km,km)
      call MATADD(Q1,Q3,Q1,kmx,kmx,
     & km,km,km,km,km,km)
c	call ATYPD(Q1,'   Q1-3 ',kmx,kmx,km,km)
c Calc VA=I - GAF*SFF*GFA in Q2
	call MATMUL(GAF,SFF,Q2,kmx,kmx,kmx,one,
     & kmx,kmx,kmx,kmx,km,km)
c	call ATYPD(Q2,'   Q2-4 ',kmx,kmx,km,km)
	call MATMUL(Q2,GFA,Q2,kmx,kmx,kmx,one,
     & km,km,kmx,kmx,km,km)
c	call ATYPD(Q2,'   Q2-5 ',kmx,kmx,km,km)
      call MATIMIN(Q2,Q2,kmx,km)	!(I-Q2) in Q2
c	call ATYPD(Q2,'   Q2-6 ',kmx,kmx,km,km)
      call MATINV2(Q2,kmx,km,Q2,km,.false.,det,ndscale)	!inv(VA) in Q2
c	call ATYPD(Q2,'   Q2-7 ',kmx,kmx,km,km)
c Calc DARS=inv(VA)*(QAA**-2) - inv(VA)*Q1*inv(VA)*inv(QAA)
c = inv(VA)[inv(QAA) - Q1*inv(VA)]*inv(QAA)
	call MATMUL(Q1,Q2,Q3,kmx,kmx,kmx,-one,
     & km,km,km,km,km,km)                       !-Q1*inv(VAA) in Q3
c	call ATYPD(Q3,'   Q3-4 ',kmx,kmx,km,km)
      call MATADD(invQAA,Q3,Q3,kmx,kmx,
     & kmx,kmx,km,km,km,km)                     !inv(QAA)-Q1*inv(VAA) in Q3
c	call ATYPD(Q3,'   Q3-5 ',kmx,kmx,km,km)
	call MATMUL(Q2,Q3,Q3,kmx,kmx,kmx,one,
     & km,km,km,km,km,km)                       !inv(VA)*Q3 in Q3
c	call ATYPD(Q3,'   Q3-6 ',kmx,kmx,km,km)
	call MATMUL(Q3,invQAA,DARS,kmx,kmx,kmx,one,
     & km,km,kmx,kmx,kmx,kmx)                   !inv(VA)*Q3*invQAA in DARS
c	call ATYPD(DARS,'  DARS= ',kmx,kmx,kmx,kmx)
c
	DEALLOCATE(Q1,Q2,Q3,GAF,GFA,INVQAA,INVQFF)
	RETURN
	end


	subroutine dFRSdS1(QD,tres,DFRS,QAA,QAF,QFF,QFA,EXPQF,EXPQA,
     & kmx,kmx1,kmx2,kmx3,km)
c To evaluate [-dFR(s)/ds] at s=0 for means etc of HJC shut time distributions.
c (Modified from dARSdS)
c [GAF,SFF,GFA do not need to be args]
c
c DFRSDS1 is local verion within HJCMEAN that has common/detw1/ removed
c and values passed as arguments (dimensions not same as in dARSdS)
c
c SAA=I-exp(QAA*tres).
c Result is output as the kmxxkmx matrix, DFRS
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
c	PARAMETER(kmxx=60)		!arrays changed so not used now
	real*8 QD(100,100),one,det
c=	real*8 QAA(10,10),QFF(10,10),QAF(10,10),QFA(10,10),SFF(10,10)
c=	real*8 EXPQF(10,10),EXPQA(10,10)
c=	real*8 QAF(kmxx,100),QFA(100,kmxx),QAA(kmxx,kmxx),QFF(100,100)
c=	real*8 EXPQA(kmxx,kmxx),EXPQF(100,100)
	real*8 QAF(kmx,kmx),QFA(kmx,kmx),QAA(kmx,kmx),QFF(kmx,kmx)
	real*8 EXPQA(kmx,kmx),EXPQF(kmx,kmx)
c==	real*8 DARS(kmx,kmx)
	real*8 DFRS(kmx,kmx)
c===	common/detw1/QAA,QAF,QFF,QFA,EXPQF,EXPQA		  !for DETWA,DETWF,dARSdS
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
     & GAF(kmx,kmx),GFA(kmx,kmx),INVQAA(kmx,kmx),INVQFF(kmx,kmx),
     & SAA(kmx,kmx))
c
c Calculate GAF and GFA -do explicitly (GMATn gets complicated when arrays
c all have different declared sizes)
c all GMAT2 replaced by GMAT4
c	subroutine GMAT4(AB,QM,GAB,invQAA,km1,km2,kg1,kg2,ki1,ki2)
c	call GMAT4(FA,QD,GFA,INVQFF,km,km,kmx,kmx,kmx,kmx)
c Calc GFA
	call SUBMAT(QD,FF,QFF,km,km,kmx,kmx)
	call MATINV2(QFF,kmx,kmx,invQFF,kmx,.false.,det,ndscale)
	call SUBMAT(QD,FA,Q1,km,km,km,km)
	call MATMUL(invQFF,Q1,GFA,kmx,kmx,kmx,-one,
     &	kmx,kmx,km,km,kmx,kmx)
c	call ATYPD(GFA,'   GFA= ',kmx,kmx,kmx,kmx)
c calc GAF
	call SUBMAT(QD,AA,QAA,km,km,kmx,kmx)
	call MATINV2(QAA,kmx,kmx,invQAA,kmx,.false.,det,ndscale)
	call SUBMAT(QD,AF,Q1,km,km,km,km)
	call MATMUL(invQAA,Q1,GAF,kmx,kmx,kmx,-one,
     &	kmx,kmx,km,km,kmx,kmx)
c	call ATYPD(GAF,'   GAF= ',kmx,kmx,kmx,kmx)
c
c	call MATMUL(QAA,invQAA,Q1,kmx,kmx,kmx,one,
c     & kmxx,kmxx,kmx,kmx,km,km)
c	call ATYPD(Q1,'  unit= ',kmx,kmx,km,km)
c	call MATMUL(QFF,invQFF,Q1,kmx,kmx,kmx,one,
c     & km,km,kmx,kmx,km,km)
c	call ATYPD(Q1,'  unit= ',kmx,kmx,km,km)
c
c	call ATYPD(GFA,'   GFA= ',kmx,kmx,kmx,kmx)
c calc SAA=I-EXPQA
	do i=1,kmx
	   do j=1,kmx
		SAA(i,j)=-EXPQA(i,j)
		if(i.eq.j) SAA(i,j)=SAA(i,j)+one
	   enddo
	enddo
c	call ATYPD(SAA,'   SAA= ',kmx,kmx,kmx,kmx)
	call MATMUL(expQA,GAF,Q1,kmx,kmx,kmx,tres,	  !tres*expQA*GAF in Q1
     & kmx,kmx,kmx,kmx,km,km)
c	call ATYPD(Q1,'  Q1-1= ',kmx,kmx,km,km)
c Correct sign of 3rd term (see above)
	call MATMUL(GFA,Q1,Q1,kmx,kmx,kmx,-one,  !-tres*GFA*expQA*GAF in Q1
     & kmx,kmx,km,km,km,km)
c	call ATYPD(Q1,'  Q1-2= ',kmx,kmx,km,km)
c 2nd term of Q1 above = GFA*SAA*inv(QAA)*GAF in Q2
	call MATMUL(GFA,SAA,Q2,kmx,kmx,kmx,one,	  !GFA*SAA in Q2
     & kmx,kmx,kmx,kmx,km,km)
c	call ATYPD(Q2,'  Q2-1= ',kmx,kmx,km,km)
c	call ATYPD(invQAA,' invQAA ',kmx,kmx,kmx,kmx)

	call MATMUL(Q2,invQAA,Q2,kmx,kmx,kmx,one,	  !GFA*SAA*inv(QAA) in Q2
     & km,km,kmx,kmx,km,km)
c	call ATYPD(Q2,'  Q2-2= ',kmx,kmx,km,km)
	call MATMUL(Q2,GAF,Q2,kmx,kmx,kmx,-one,	  !-GFA*SAA*inv(QAA)*GAF in Q2
     & km,km,kmx,kmx,km,km)
c	call ATYPD(Q2,'   Q2-3 ',kmx,kmx,km,km)
c first term = -inv(QFF)*GFA*SAA*GAF in Q3
	call MATMUL(invQFF,GFA,Q3,kmx,kmx,kmx,one,	  !inv(QFF)*GFA in Q3
     & kmx,kmx,kmx,kmx,km,km)
c	call ATYPD(Q3,'   Q3-1 ',kmx,kmx,km,km)
	call MATMUL(Q3,SAA,Q3,kmx,kmx,kmx,one,		   !inv(QFF)*GFA*SAA in Q3
     & km,km,kmx,kmx,km,km)
c	call ATYPD(Q3,'   Q3-2 ',kmx,kmx,km,km)
	call MATMUL(Q3,GAF,Q3,kmx,kmx,kmx,-one,	   !-inv(QFF)*GFA*SAA*GAF in Q3
     & km,km,kmx,kmx,km,km)
c	call ATYPD(Q3,'   Q3-3 ',kmx,kmx,km,km)
c Add Q1+Q2+Q3; result in Q1
      call MATADD(Q1,Q2,Q1,kmx,kmx,
     & km,km,km,km,km,km)
      call MATADD(Q1,Q3,Q1,kmx,kmx,
     & km,km,km,km,km,km)
c	call ATYPD(Q1,'   Q1-3 ',kmx,kmx,km,km)
c Calc VF=I - GFA*SAA*GAF in Q2
	call MATMUL(GFA,SAA,Q2,kmx,kmx,kmx,one,
     & kmx,kmx,kmx,kmx,km,km)
c	call ATYPD(Q2,'   Q2-4 ',kmx,kmx,km,km)
	call MATMUL(Q2,GAF,Q2,kmx,kmx,kmx,one,
     & km,km,kmx,kmx,km,km)
c	call ATYPD(Q2,'   Q2-5 ',kmx,kmx,km,km)
      call MATIMIN(Q2,Q2,kmx,km)	!(I-Q2) in Q2
c	call ATYPD(Q2,'   Q2-6 ',kmx,kmx,km,km)
      call MATINV2(Q2,kmx,km,Q2,km,.false.,det,ndscale)	!inv(VA) in Q2
c	call ATYPD(Q2,'   Q2-7 ',kmx,kmx,km,km)
c Calc DFRS=inv(VF)*(QFF**-2) - inv(VF)*Q1*inv(VF)*inv(QFF)
c = inv(VF)[inv(QFF) - Q1*inv(VF)]*inv(QFF)
	call MATMUL(Q1,Q2,Q3,kmx,kmx,kmx,-one,
     & km,km,km,km,km,km)                       !-Q1*inv(VFF) in Q3
c	call ATYPD(Q3,'   Q3-4 ',kmx,kmx,km,km)
      call MATADD(invQFF,Q3,Q3,kmx,kmx,
     & kmx,kmx,km,km,km,km)                     !inv(QFF)-Q1*inv(VFF) in Q3
c	call ATYPD(Q3,'   Q3-5 ',kmx,kmx,km,km)
	call MATMUL(Q2,Q3,Q3,kmx,kmx,kmx,one,
     & km,km,km,km,km,km)                       !inv(VF)*Q3 in Q3
c	call ATYPD(Q3,'   Q3-6 ',kmx,kmx,km,km)
	call MATMUL(Q3,invQFF,DFRS,kmx,kmx,kmx,one,
     & km,km,kmx,kmx,kmx,kmx)                   !inv(VF)*Q3*invQFF in DFRS
c	call ATYPD(DARS,'  DFRS= ',kmx,kmx,kmx,kmx)
c
	DEALLOCATE(Q1,Q2,Q3,GAF,GFA,INVQAA,INVQFF)
	RETURN
	end



