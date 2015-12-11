	real*8 function FTCt(t,tres,k,kA,kF,ucol,
     &  Z00F,Z10F,Z11F,XFA,kAm,kFm,km)
c To calculate the unconditional cumulative shut time HJC distribution
c FTc(t)=phiF*(F)CR(t-tres)*QFA*exp(QAA*tres)*uA
c ie same as shut time pdf except that (F)CR(t) replaces (F)R(t).
c
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	real*8 FCRQ(100,100)
	real*8 ucol(100,1)
	real*8 Z00F(kFm,kAm,km),Z10F(kFm,kAm,km),Z11F(kFm,kAm,km)
	real*8 XFA(kFm,kAm,kFm)
c
	real*8 phiA(1,100),phiF(1,100)
	common/phiblk/phiF,phiA
c
      ftct=0.d0
c Get FCRQ=(F)CR(t-tres)*QFA*exp(QAA*tres)
	call FCRQt(t-tres,tres,k,kA,kF,FCRQ,
     &  Z00F,Z10F,Z11F,XFA,kAm,kFm,km)
      call MATSCL2d(phiF,FCRQ,ucol,kF,kA,scal,km,km,km,km)
	FTCt=scal
	RETURN
	end


