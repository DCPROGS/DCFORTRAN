	subroutine HJCASYMP(QD,k,kA,kF,phiA,phiF,QEXPQA,QEXPQf,XAF,XFA,
     & rootA,rootF,rootAsav,rootFsav,jset,
     & s1A,s2A,s1F,s2F,first,kAm,kFm)
c
c     & QAF,QFA,QAA,QFF,EXPQF,EXPQA,WA,WF,kAm,kFm)
c To calculate matrices XAF(m) and XFA(m) needed to calc likelihoods from
c asymptotic HJC distributions for open and shut times (resp) in HJCFIT,
c and to return also initial vectors phiA(), phiF() (method taken from
c HJCDIST (in \CALC directory)
c NB XAF(m) and XFA(m) are the matrices denoted (A)R(m)*QAF*EXPQF and
c (F)R(m)*QFA*EXPQA where (A)R(m) is defined in CHS96 eq.2.29
c and EXPQA=exp(QAA*tres)
c
c Modified 08/07/03 09:17pm so make rootAsav(*,jset), rootFsav(*,jset), jset
c arguments because, at present, when 'previous root' is used in hjcasymp,
c the root from the last iteration was taken from the wrong set when nset>1
c Similarly the SAVEd array eigFFsav, must also no have jset as
c index (NB eigAAsav not used at present)
c
c Modif 01/30/03 04:46pm to allow option to test whether roots occur
c in groups of closely-spaced values (close = ratio between  subsequent
c values all below rcrit)
c Would be best to find highest and lowest root in each group  first, so that
c a detailed search could be made between these limits for the rest of the
c roots, but no obvious way to find first or last, so use bisection with large
c step (guesses each side of the group -halfway between end of one group and
c start of the next) to find ANY root in the group.
c If the group has n roots in it then length of group is <= rcrit^(n-1) so
c searching this distance either side of the first root found, with small
c steps, should find the rest (only problem is that the number of roots in
c a group, and the position of the group, comes from last iteration, so
c could be wrong).
c Look for point halfway betwen last root in 1 group and first in the next
c (halfway on log scale?)
c
c
c Modif 02/04/02 06:17pm: nerr=81 for DETWA error,=82 for DETWF error
c Need to add (random?) perturbation when theta reset to thmin, or change
c step length?, to reduce risk of infinite loop?
c
c Modif 01/21/02 07:08pm to improve error handling in (rare) cases where
c persistent overflow of determinant is a problem. Determinant is calc in DETV
c (at present, part of EQOCCD), and does not return an error code when it
c overflows. Problem arises when DETV overflows when called in DETWF (or DETWA),
c and these are called in 2 places, from ROOTSGH and from BISECD.
c DETWF now returns nerr=10 if it thinks DETV is about to overflow, but
c this is ignored in both ROOTSCH and BISECD. No action taken until return to
c here (HJCASYMP). DETV now replaced by DETERM() in DETWA/F and DETERM returns
c nerr=10 if overflow likely.
c
c Major modif 04/05/01 10:53am for 100 states/10 ligands
c Arrays made adjustable so k now included as argument as well as kA,kF
c
c Modif 01/26/00 09:41am to use geometric mean of prev roots for guesses
c
c    Modif 10/14/92 07:11pm for calc with several data sets.  This affects this
c  subroutine only in that
c  (1) with several data sets, call with first=true for every set (not
c     just the first set) to get initial guesses for each.
c  (2) Note that some of the calculations needed for plots are done here only
c   when idebug=8, as used for print of the final pdfs.  Modified now so that
c   these calcs are done also if ONESET=true (but pdfs not printed again)
c
c (NB several param are in common/detw1/km,QAA,QAF,QFF,QFA,EXPQF,EXPQA,
c WA,WF,tres (for DETWA,DETWF))
c For open times:
c	XAF(m)=colvec(m)*rowvec(m)*QAF*exp(QFF*tres)/denom
c where denom=rowvec(m)*W'(s(m))*colvec(m)
c so f(t)=phiA*SUM[XAF(m)*exp(-rootA(m)*t1)]uF
c thus amplitude(m)=phiA*XAF(m)*uF
c where sum is from 1 to kA, and t1=excess open time. Omission of phiA and uF
c gives the matrices to be multiplied to get likelihood.
c And similarly for SHUT times
c
c Also returns various matrices EXPQF,EXPQA etc needed for exact pdfs.
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	PARAMETER(kAx=60) 	!max number of open states for following
	real*8 one
	real*8 XAF(kAm,kFm,kAm),XFA(kFm,kAm,kFm)
	real*8 QEXPQA(kFm,kAM),QEXPQF(kAm,kFm)
	real*8 QD(100,100)
c===change plan again -keep commons /detw1 and /detw3 so make QAF etc fixed size
c=	real*8 QAF(kAm,kFm),QFA(kFm,kAm),QAA(kAm,kAm),QFF(kFm,kFm)
c=	real*8 EXPQF(kFm,kFm),EXPQA(kAm,kAm)
c=	real*8 WA(kAm,kAm),WF(kFm,kFm)
	real*8 QAF(kAx,100),QFA(100,kAx),QAA(kAx,kAx),QFF(100,100)
	real*8 EXPQA(kAx,kAx),EXPQF(100,100)
	real*8 WA(kAx,kAx),WF(100,100)
c	real*8 eigFFsav(100)
	real*8 eigFFsav(100,10)
	real*8 dexp1
	common/detw1/QAA,QAF,QFF,QFA,EXPQF,EXPQA   !for DETWA,DETWF,dARSdS -now param
	common/detw3/WA,WF			  !for DETWA,DETWF -now params
c
c=	real*8 GAF(kAM,kFm),GFA(kFm,kAm)
	allocatable::GAF,GFA
	real*8 GAF(:,:),GFA(:,:)

c=	real*8 INVQAA(10,10),INVQFF(10,10)
	allocatable::invQAA,invQFF
	real*8 invQAA(:,:),invQFF(:,:)

c=	real*8 rowvecA(10,10),rowvecF(10,10),colvecA(10,10),colvecF(10,10)
	allocatable::rowvecA,rowvecF,colvecA,colvecF
	real*8 rowvecA(:,:),rowvecF(:,:),colvecA(:,:),colvecF(:,:)

c=	real*8 Q1(10,10),Q2(10,10)
	allocatable:: Q1,Q2
	real*8 Q1(:,:),Q2(:,:)

c=	real*8 FMAT(10,10,10), EIGFF(10),AMAT(10,10,10), EIGAA(10)
	allocatable::FMAT,EIGFF,AMAT,EIGAA
	real*8 FMAT(:,:,:), EIGFF(:),AMAT(:,:,:), EIGAA(:)
c
	real*8 rootA(100),rootF(100),pdum(100),ucol(100,1)
	real*8 rootAsav(kAx,10),rootFsav(100,10)
	real*8 ampA(100),ampF(100)
	real*8 phiA(1,100),phiF(1,100),row1(1,100)
	real*8 s1A(100),s2A(100),s1F(100),s2F(100)	!init guesses for roots
	real*4 am1,sd1
	INTEGER AF,FA,AA,FF
	logical debug,caplock,discprt,first,newroot,oneset
	logical pcalc,docalc,deb,recheck
	real*8 detwA,detwF
c To abort fit
c	integer*2 ktype
c	character ch,getch
c	logical KBHIT		!Utility lib
	logical abort
	common/abt/abort
c from detwf, detwa
	common/det4/ndscale,idsign
c
	EXTERNAL detwA,detwF
	EXTERNAL gfuncA,gfuncF	!for Ball method
c
	logical debprt,dprt,dsav
	common/deb1/debprt
	common/deb/idebug,idebug1
	common/setblk/oneset,iset	!for HJCLIK,DISP to specify one set
	common/detw2/tres,km,nerr			!for HJCLIK,DETWA,DETWF
	common/fitblk1/ampA,ampF		!for HJCDISP
	common/perr/nerr2		!for errors in eqoc_hjc
	common/dp/discprt
c For dealing with 'identical' roots
	logical slopsch,checkgrp,grouped,gsav,odd,even,same
	logical stable
	allocatable::rootset
	logical rootset(:)
c	integer ngrp(100)		!number of roots in the ith group
	integer*4 n1(100),n2(100)
	common/grp1/slopsch,checkgrp,grouped,rcrit,ngpcheck
	common/grp2/ngroup,n1,n2
	common/stab/stable,nstab1,gfac1,nstab2,gfac2	!for hjcasymp
c
	logical scaled
	common/eval/neval,niter	!from SIMPHJC, for print in hjcasymp
c
	SAVE istab,nevrs,eigFFsav,nevlast
c
	debug()=caplock()
c
	ALLOCATE(GAF(kAm,kFm),GFA(kFm,kAm))
	ALLOCATE(invQAA(kAm,kAm),invQFF(kFm,kFm))
	ALLOCATE(rowvecA(kAm,kAm),rowvecF(kFm,kFm),
     &	colvecA(kAm,kAm),colvecF(kFm,kFm))
	ALLOCATE(FMAT(kFm,kFm,kFm),EIGFF(kFm),
     &	AMAT(kAm,kAm,kAm),EIGAA(kAm))
	kXm=kAm+kFm
	ALLOCATE(Q1(kXm,kXm),Q2(kXm,kXm))
	ALLOCATE(rootset(100))
c
c	checkgrp=.true.		!for now -set in hjcfit
	AA=11
	AF=15		!5=CODE FOR F(=B+C) SECTION
	FA=51
	FF=55
	k=kA+kF
	one=1.0d0
	do n=1,100
	   ucol(n,1)=one
	enddo
	zero=0.0d0
c	rcrit=1.02d0	!identical within 2%
c	rcrit=1.05d0	!identical within 5%	!now set at start of hjcfit
	scaled=.false.
c
c Set pcalc=true if extra calculations to be done AND printed; docalc=true
c if they are to be done (for plots) but not printed
	dprt=discprt.and.debprt	 !put problems in root-finding on disc as well as screen
	deb=debug()
	pcalc=deb.or.idebug.eq.8.or.idebug.eq.9
	docalc=pcalc.or.oneset
c
c Calculate GAF and GFA -do explicitly (GMATn gets complicated when arrays
c all have different declared sizes)
	call SUBMAT(QD,AA,QAA,km,km,kAx,kAx)
	call MATINV(QAA,kA,kAx,invQAA,kAm)
	call SUBMAT(QD,AF,Q1,km,km,kXm,kXm)
	call MATMUL(invQAA,Q1,GAF,kA,kA,kF,-one,
     &	kAm,kAm,kXm,kXm,kAm,kFm)

	call SUBMAT(QD,FF,QFF,km,km,km,km)
	call MATINV(QFF,kF,km,invQFF,kFm)
	call SUBMAT(QD,FA,Q1,km,km,kXm,kXm)
	call MATMUL(invQFF,Q1,GFA,kF,kF,kA,-one,
     &	kFm,kFm,kXm,kXm,kFm,kAm)
	if(deb) then
	   call ATYPD(GAF,' GAF    ' ,kA,kF,kAm,kFm)
	   call ATYPD(GFA,' GFA    ' ,kF,kA,kFm,kAm)
c	Calc normal initial vector (tres=0) using method below as check
	   call HJCphi(GAF,GFA,kA,kF,phiA,kAm,kFm,km)
c Repeat for phiF
	   call HJCphi(GFA,GAF,kF,kA,phiF,kFm,kAm,km)
         print 20,(phiA(1,j),j=1,kA)
         if(discprt) write(8,20) (phiA(1,j),j=1,kA)
         print 21,(phiF(1,j),j=1,kF)
         if(discprt) write(8,21) (phiF(1,j),j=1,kF)
	endif
c
c Expand QFF
	call QMAT5(QFF,Fmat,kF,eigFF,ibad,km,kFm,kFm)
c Check for crazy (e.g. near 0) eigenvalues here? -e.g. rate can reach 0 (or
c at least the min value allowed in HJCLIK); also if this param is in denom
c for calc of a micro rev parameter, latter will get very large, and roots
c cannot be found in ROOTSCH.  If this happens, replace current theta with
c the previous best value (which is in common/best/ -see simplex) and carry
c on (but with current step size). Eigaa(1) is biggest (most neg)
	if(eigFF(1).lt.-1.d10.or.eigFF(kF).gt.-1.d-10) then
         print 53,(eigFF(j),j=1,kF)
         if(discprt) write(8,53) (eigFF(j),j=1,kF)
         print 71
         if(dprt) write(8,71)
71	   format(
     &' QFF has eigenvalues >1e10 or <1e-10; current parameters')
	   nerr=8		!to signal replacement on return to HJCLIK
	   goto 999		!deallocate and RETURN
	endif
	if(deb) then
         print 53,(eigFF(j),j=1,kF)
         if(discprt) write(8,53) (eigFF(j),j=1,kF)
	endif
c ===debug
c	if(idebug.eq.13) then
	nevlast=neval
	if(.not.first.and.idebug1.gt.1) then
	   print 263,neval,niter
	   if(dprt) write(8,263) neval,niter
263	   format(
     &' QFF at the start of function evaluation # ',i6,' Niter = '
     & i6,/,'    m      prev eig(QFF)   eig(QFF)   scalefac')
	   do m=1,kF
		scalefac=eigFF(m)/eigFFsav(m,jset)
		print 264,m,eigFFsav(m,jset),eigFF(m),scalefac
		if(dprt) write(8,264) m,eigFFsav(m,jset),eigFF(m),scalefac
264		format(3x,i3,3(3x,g13.6))
	   enddo
	endif
c ===end debug
c Calc exp(QFF*tres) in EXPQF
	do i=1,kF
	   do j=1,kF
		EXPQF(i,j)=0.d0
		do m=1,kF
		   EXPQF(i,j)=EXPQF(i,j) + FMAT(i,j,m)*dexp1(eigFF(m)*tres)
		enddo
	   enddo
	enddo
	if(deb) then
	   call ATYPD(expqF,'expQFFtr' ,kF,kF,km,km)
	   do m=1,kF
		call ATYPD3(Fmat,m,' FMAT   ',kF,kF,kFm,kFm,kFm)
	   enddo
	endif
C
c Expand QAA
	call QMAT5(QAA,Amat,kA,eigAA,ibad,kAx,kAm,kAm)
c Check for crazy (e.g. near 0) eigenvalues as above.
	if(eigAA(1).lt.-1.d10.or.eigAA(kA).gt.-1.d-10) then
         print 510,nroot,(eigAA(j),j=1,kA)
         if(dprt) write(8,510) nroot,(eigAA(j),j=1,kA)
         print 70
         if(dprt) write(8,70)
70	   format(
     &' QAA has eigenvalues >1e10 or <1e-10; current parameters')
	   nerr=8		!to signal replacement on return to HJCLIK
	   goto 999		!deallocate and RETURN
	endif
	if(deb) then
         print 510,nroot,(eigAA(j),j=1,kA)
         if(dprt) write(8,510) nroot,(eigAA(j),j=1,kA)
	endif
c Calc exp(QAA*tres) in EXPQA
	do i=1,kA
	   do j=1,kA
		EXPQA(i,j)=0.d0
		do m=1,kA
		   EXPQA(i,j)=EXPQA(i,j) + AMAT(i,j,m)*dexp1(eigAA(m)*tres)
		enddo
	   enddo
	enddo
	if(deb) then
	   call ATYPD(expqa,'expQAAtr' ,kA,kA,kAx,kAx)
	   do m=1,kA
		call ATYPD3(amat,m,' AMAT   ',kA,kA,kAm,kAm,kAm)
	   enddo
	endif
c
c Calc INITIAL VECTOR: first calc eGAF*eGFA
c Use subroutine to get eGAF in Q1
	call CeGAF(Q1,GAF,GFA,kA,kF,EXPQF,
     & kXm,kXm,kAM,kFm,kFm,kAm,km,km)
	if(deb) call ATYPD(Q1,' eGAF   ' ,kA,kF,kXm,kXm)
c Now get eGFA by repeating with A and F interchanged
c Use subroutine to get eGFA in Q2
	call CeGAF(Q2,GFA,GAF,kF,kA,EXPQA,
     & kXm,kXm,kFm,kAm,kAm,kFm,kAx,kAx)
c     & kq1,kq2,kAF1,kAF2,kFA1,kFA2,ke1,ke2)
	if(deb) call ATYPD(Q2,' eGFA   ' ,kF,kA,kXm,kXm)
	call HJCphi(Q1,Q2,kA,kF,phiA,kXm,kXm,km)
c
c Repeat for phiF
	call HJCphi(Q2,Q1,kF,kA,phiF,kXm,kXm,km)
	if(pcalc) then
         print 20,(phiA(1,j),j=1,kA)
         if(discprt) write(8,20) (phiA(1,j),j=1,kA)
20	   format(
     & ' Initial vector for open times = ',/,10g13.6)
         print 21,(phiF(1,j),j=1,kF)
         if(discprt) write(8,21) (phiF(1,j),j=1,kF)
21	   format(
     & ' Initial vector for shut times = ',/,10g13.6)
	endif
c
c OPEN TIMES
c
c Everything from here on is function of s.  Need to (a) calculate W(s) at
c range of s values to locate roots, but once roots found need W'(s) and
c eigenvectors of H(s) to be evaluated ONLY at the particular s=root(m).
c Calc (sI-QFF),inv(sI-QFF), and exp[-(sI-QFF)*tres] for chosen s values,
c to get SFF(s),GAF(s) and hence W(s) to get roots s(m), then c(s(m)),
c r(s(m)) and W'(s) to get areas of pdf.
c
c Find roots by bisection
c s=recip seconds; to specify  accuracy might be better to BISECT with 1/s
c than s itself, but for now use s, and set error, epsx=1.e-4*initial guess
cc Use eigenvalues with tres=0 as bounds, but make slightly (*1.01) wider to
cc ensure prog works with tres=0)
c 02/19/92 10:05am New root-location method. When called for first time,
c eg with initial guesses, call ROOTSCH to locate the roots 'by steam'.
c Therafter use these roots r1,r2,...,rk say, (r1=fastest) to set init guesses
c for next iteration, by bisecting intervals between them, so use 2*r1 (say) to
c (r1+r2)/2 as interval for 1st root, (r1+r2)/2 to (r2+r3)/2 for 2nd root,
c ...,(r(k-1)+rk)/2 to 0 for slowest root.  If BISECT reports an error
c then call ROOTSCH again to sort it out. Initial guesses held in arrays
c s1A(m),s2A(m) for open, and s1F(m),s2F(m) for shut
c Modif 01/26/00 09:41am to use geometric mean of prev roots for guesses
c Find roots for open times -copy eigAA to rootA the first time as initial
c guesses when nothing else available
	if(first) then
	   do m=1,kA
		rootA(m)=eigAA(m)
	   enddo
	   if(idebug1.gt.0) then
		print 79
		if(dprt) write(8,79)
79		format(
     &' Locating guesses for OPEN time asymptotic roots (Ball method)')
c79		format(' Searching for OPEN time asymptotic roots')
	   endif
c	   call ROOTSCH(DETWA,kA,eigAA,rootA,s1A,s2A,nroot,kAm)
c Use Frank Ball's method to find guesses (in s1, s2)  for bisection of each
c root. For ROOT_FB start the first time with a very wide range that should
c include all roots
	   sa=-100000.d0
	   sb=-0.001d0
	   call ROOT_FB(sa,sb,GfuncA,kA,s1A,s2A,nroot,kAm,detwA)
	   if(abort) goto 999	!kbhit now in rootsch
	   if(nroot.ne.kA) then
c	     if(dprt) call ATYPD(QD,'  QD    ' ,k,k,km,km)	!print current QD if problem
           print 51,kA,(rootA(j),j=1,kA)
           if(dprt) write(8,51) kA,(rootA(j),j=1,kA)
51	     format(
     &  ' ROOTSCH failed to locate all ',i3,' open time roots for',
     &  ' initial guesses',/,
     &  ' rootA:',10(5g13.6,/))
	     nerr=5
	     goto 999		!deallocate and RETURN
	   endif
	else		!get init guesses from roots found in last iteration
c	   s1A(1)=2.d0*rootA(1)
	   s1A(1)=3.d0*rootA(1)
	   if(kA.ge.2) then
c	      s2A(1)=(rootA(1)+rootA(2))/2.d0
	      s2A(1)=-dsqrt(rootA(1)*rootA(2))
	   else
		s2A(1)=-1.d-30	!if kA=1
	   endif
	   if(kA.ge.2) then
		do 43 m=2,kA
c		 s1A(m)=(rootA(m-1)+rootA(m))/2.d0
		 s1A(m)=-dsqrt(rootA(m-1)*rootA(m))
		 if(m.lt.kA) then
c		    s2A(m)=(rootA(m+1)+rootA(m))/2.d0
		    s2A(m)=-dsqrt(rootA(m+1)*rootA(m))
		 else
		   s2A(m)=-1.d-30 	!if m=kA
		 endif
43		continue
	   endif
	endif
c Initial guesses set
	Y=0.0d0
	ndisp=-1
	if(deb) ndisp=1
	epsy=-1.d0	  !so ignored
	newroot=.false.
c NB can get into infinite loop if rootsch thinks it has found all roots
c but bisecd fails.  This can happen if lower lim for root 1 is not
c low enough so it is doubled below
42	continue	!return to try again after bisection error
	do 25 m=1,kA
	 epsx=1.d-8*dabs(s1A(m))	!1.d-10 seems much the same
	 s1sav=s1A(m)
	 s2sav=s2A(m)

	 call BISECd(DETWA,s1A(m),s2A(m),Y,sout,yout,epsx,epsy,
     &  nerr1,ndisp,.false.)
	 if(iabs(nerr1).eq.1.or.iabs(nerr1).eq.2) then
         print 40,m,nerr1,s1sav,s2sav,DETWA(s1sav),DETWA(s2sav)
         if(dprt) write(8,40) m,nerr1,s1sav,s2sav,DETWA(s1sav),
     &	DETWA(s2sav)
40	   format(/,
     & ' Error in open time root #',i3,'; nerr = ',i3,/,
     & '  guesses, s1, s2 = ',2g13.6,/,
     & ' f(s1) = ',g13.6,',  f(s2) = ',g13.6,/,
     & '  locate roots again (Ball method)')
c     & '  searching for roots again')
c print the current QD before re-searching
c	   dsav=discprt	!to control discprt in ATYPD
c	   discprt=dprt
c	   call ATYPD(QD,'  QD    ' ,k,k,km,km)
c	   discprt=dsav
         print 510,nroot,(eigAA(j),j=1,kA)
         if(dprt) write(8,510) nroot,(eigAA(j),j=1,kA)
510	   format(' nroot = ',i3,' EigAA = ',25g13.6)
	   s1A1sav=s1A(1)
	   if(idebug1.gt.0) then
		print 79
		if(dprt) write(8,79)
c79		format(
c     &' Locating guesses for OPEN time asymptotic roots (Ball method)')
	   endif
c	   call ROOTSCH(DETWA,kA,eigAA,rootA,s1A,s2A,nroot,kAm)	!get new guesses for roots
c Use Frank Ball's method to find guesses (in s1, s2)  for bisection of each
c root. For ROOT_FB start the first time with a very wide range that should
c include all roots
	   sa=-100000.d0
	   sb=-0.001d0
	   call ROOT_FB(sa,sb,GfuncA,kA,s1A,s2A,nroot,kAm,detwA)
	   if(abort) goto 999	!kbhit now in rootsch
c if bisecd failed on root 1, it is probably because s1A(1) is not low
c enough (this can give infinite loop back to 42) so double it each time
c NB in loop rootsch will produce same value each time so ignore output
c from rootsch
	   if(m.eq.1) then
		s1A(1)=2.d0*s1A1sav
	   endif
	   if(nroot.ne.kA) then
	     print 73,nroot,kA
	     write(8,73) nroot,kA
73	     format(
     &     ' ROOT_FB found only ',i3,' of the ',i3,' open time roots')
c     &     ' ROOTSCH found only ',i3,' of the ',i3,' open time roots')
	     nerr=5
	     goto 999		!deallocate and RETURN
	   endif
	   newroot=.true.
	   goto 42			!start again
	 endif
	 rootA(m)=sout
25	continue
	if(newroot) then
         print 251,(rootA(m),m=1,kA)
         if(dprt) write(8,251) (rootA(m),m=1,kA)
251	   format(' Open time roots:',5(5g13.6))
	   newroot=.false.
	endif
	if(idebug1.gt.1) then
	   print 260,kA,neval,niter
	   if(dprt) write(8,260) kA,neval,niter
260	   format(
     &' All ',i2,' open time roots found for function evaluation # ',
     & i6,/,' iteration number = ',i6,/,
     & '    m      eig(QAA)     rootA(m)   ')
	   do m=1,kA
		print 261,m,eigaa(m),rootA(m)
		if(dprt) write(8,261) m,eigaa(m),rootA(m)
261		format(3x,i3,3x,g13.6,2x,g13.6)
	   enddo
	endif
c
c	Y=0.0d0
c	ndisp=-1
c	if(deb) ndisp=1
c	do 25 m=1,kA
c	 s1=eigaa(m)*1.01d0
c	 if(m.lt.kA) then
c	  s2=eigaa(m+1)*1.01d0
c	 else
c	  s2=0.0d0
c	 endif
cc	 epsx=1.d-4*dabs(s1)
cc	 epsx=1.d-5*dabs(s1)
c	 epsx=1.d-8*dabs(s1)	!1.d-10 seems much the same
c	 epsy=-1.	  !so ignored
c	 call BISECd(DETWA,s1,s2,Y,sout,yout,epsx,epsy,
c     &  nerr,ndisp,.false.)
c	 if(iabs(nerr).eq.1.or.iabs(nerr).eq.2) then
c         print 40,m,(eigaa(m1),m1=1,kA)
c         if(dprt) write(8,40) m,(eigaa(m1),m1=1,kA)
c40	   format(
c     & ' Error in open time root #',i2,' eig(QAA) = ',/,8g13.6)
c	   RETURN
c	 endif
c	 rootA(m)=sout
c25	continue
c Now get left (row) eigenvectors for each root:
c First OPEN: keep as rows of ROWVECA
	do 32 m=1,kA
	 dum=DETWA(rootA(m))	!called to return WA(s(m)) via common
	 if(nerr.ne.0) goto 999		!deallocate and RETURN
c It is possible (though rare) to a zero determinant in eqoc_hjc (output not
c defined in this case. Now signalled by nerr2.ne.0, in common/perr
	 call EQOC_hjc(WA,pdum,kA,kAx,km)
c=	 call EQOCC2(WA,pdum,kA,km)
c==      call EQOCC1(WA,pdum,kA,km)
	 if(nerr2.ne.0) then
	   nerr=81		!to signal replacement if theta on return to HJCLIK
	   goto 999		!deallocate and RETURN
	 endif
	 do 33 j=1,kA
33	  rowvecA(m,j)=pdum(j)	!copy to rowvec
	  if(deb) then		!check pdum*WA=[0 0 ...]
	   do 321 j=1,kA
321	   row1(1,j)=pdum(j)
         call MATMUL(row1,WA,row1,1,kA,kA,one,1,km,kAx,kAx,1,km)
         print 322,(pdum(j),j=1,kA)
         if(discprt) write(8,322) (pdum(j),j=1,kA)
322	   format(' r(m) = ',10g13.6)
         print 323,(row1(1,j),j=1,kA)
         if(discprt) write(8,323) (row1(1,j),j=1,kA)
323	   format(' r(m)*W(m) = ',10g13.6)	!should be [0 0 ...]
	  endif
c Get right col vectors by direct solution c'(m)W'(s(m))=0 where '=transpose
c=	   call MATTRANS(WA,Q1,kA,kA,km)	!transpose of WF in Q1
	   call MATTRAN1(WA,Q1,kA,kA,kAx,kXm)	!transpose of WF in Q1
c It is possible (though rare) to a zero determinant in eqoc_hjc (output not
c defined in this case. Now signalled by nerr2.ne.0, in common/perr
	   call EQOC_HJC(Q1,pdum,kA,kXm,km)		!c'(m) in pdum
	   if(nerr2.ne.0) then
		nerr=81		!to signal replacement if theta on return to HJCLIK
		goto 999		!deallocate and RETURN
	   endif
	   do i=1,kA
		colvecA(i,m)=pdum(i)	!copy to column m of colvec
	   enddo
32	continue	!end of m loop
c===c Get right (column) eigenvectors in colvecA by inversion
c===      call MATINV(rowvecA,kA,km,colvecA,km)
	if(deb) then
	  call ATYPD(colveca,'ColvecA ' ,kA,kA,kAm,kAm)
	endif
c
c Now calc areas
c  OPEN times
c   calc part of numerator same for all components
c Get QAF*EXPQF in QEXPQF: this is kA*kF
      call MATMUL(QAF,EXPQF,QEXPQF,kA,kF,kF,one,kAx,km,km,km,kAm,kFm)
c      call MATMUL(EXPQF,ucol,col1,kF,kF,1,one,km,km,km,1,km,1)
c      call MATMUL(QAF,col1,col1,kA,kF,1,one,km,km,km,1,km,1)
cc now have QAF*expQF*uF in col1 -copy to colA for use in exact dist
c	do 371 i=1,kA
c371	colA(i,1)=col1(i,1)
c   calc c(m)*r(m) =kA*kA
	do 36 m=1,kA
	 do 37 i=1,kA
	 do 37 j=1,kA
37      Q1(i,j)=colvecA(i,m)*rowvecA(m,j)
 	 if(deb) then
	   call ATYPD(Q1,'col*row ' ,kA,kA,kXm,kXm)
	 endif
      call MATMUL(Q1,QEXPQF,Q1,kA,kA,kF,one,
     & kXm,kXm,kAm,kFm,kXm,kXm)	!col(m)*row(m)*QAF*EXPF
c  Now denominator =r(m)*W'(s(m))*c(m)
	 s=rootA(m)
	 call DENOMA(m,s,QAF,QFF,QFA,EXPQF,rowvecA,colvecA,
     & tres,kA,kF,denA,kAx,kAm,kFm,km)
c=     &   tres,kA,kF,km,den)
c Copy Q1 to XAF(m), dividing by denom at same time
	do i=1,kA
	   do j=1,kF
		XAF(i,j,m)=Q1(i,j)/denA
	   enddo
	enddo
	if(docalc) then
         call MATMUL(phiA,Q1,row1,1,kA,kF,1.d0/denA,
     &	1,km,kXm,kXm,1,km)
         call VECMUL(row1,ucol,ampA(m),kF,km,km)	!numerator for amplitude
	 endif
36	continue	!end of m loop
	if(pcalc) then
	   call PDFOUTd(' ASYMPTOTIC OPEN TIME DISTRIBUTION',-1,-1,ampA,
     &	rootA,kA,am1,sd1,km,.false.,.true.,discprt)
	   call ACONVd(ampA,rootA,kA,tres,discprt,km)	!renormalise areas
	else if(docalc) then
	   call ACONVd(ampA,rootA,kA,tres,.false.,km)	!renormalise areas
	endif
c
c
c NOW SHUT TIMES
c Find roots for shut times -copy eigFF to rootF the first time as initial
c guesses when nothing else available -do it eparately here so can return to
c to 1 in case of problems without setting rootF=eigFF
c NB set first=false after label 100 in HJCLIK (so it is still
c set true for each data set, the first time through)
c
c Need another variable, recheck, so that can return to do 'if(first)' section
c while within hjcasymp
c
c At this point eigFF is for current paramater values, and rootF (and eigFFssav)
c are from previous parameter values, so why not try scaling rootF with eigFF
c from the start (except when first=T so eigFFsav not yet defined)?
c
	if(first) then
	   do m=1,kF
		rootset(m)=.false.
		rootF(m)=eigFF(m)
	   enddo
	endif
c
c Find the guesses for bisection
1	continue 	!return here to re-do 'first'
	if(first.or.recheck) then
	   do m=1,kF
		rootset(m)=.false.
	   enddo
	   grouped=.false.	!first time
	   nevrs=neval	!number of function eval for last call to rootsch
	   if(idebug1.gt.0) then
		print 791
		if(dprt) write(8,791)
c791		format(' Searching for SHUT time asymptotic roots')
791		format(
     &' Locating guesses for SHUT time asymptotic roots (Ball method)')
	   endif
c	   call ROOTSCH(DETWF,kF,eigFF,rootF,s1F,s2F,nroot,kFm)

c Use Frank Ball's method to find guesses (in s1, s2)  for bisection of each
c root. For ROOT_FB start the first time with a very wide range that should
c include all roots
	   sa=-100000.d0
	   sb=-0.001d0
	   call ROOT_FB(sa,sb,GfuncF,kF,s1F,s2F,nroot,kFm,detwF)

	   if(abort) goto 999	!kbhit now in rootsch
	   if(nroot.ne.kF) then
c	     if(dprt) call ATYPD(QD,'  QD    ',k,k,km,km)	!print current QD if problem
           print 531,kF,(rootF(j),j=1,kF)
           if(dprt) write(8,531) kF,(rootF(j),j=1,kF)
531	     format(
     &  ' ROOTSCH failed to locate all ',i3,' shut time roots for',
     &  ' initial guesses',/,
     &  ' rootF:',10(5g13.6,/))
	     nerr=5
	     goto 999		!deallocate and RETURN
	   endif
	   scaled=.true. 		!have exact rootF so no need to scale
c	   first=.false.		!set false in HJCLIK
	   recheck=.false.
c
c  Are roots grouped?
c NB ROOTSCH does not output values if rootF, but just the guesses for
c the bisection that gives rootF.  To get ALL roots must set grouped=false
c BEFORE calling ROOTSCH.  If roots are very close, bisection may cause
c problems so may be sensible to set grouped=T/F on basis of search for
c all roots (grouped=F) in rootsch, before going on to bisection. To do
c this call GETGROUP with geometric mean of guesses in rootF as approx
c to real rootF from bisection which will replace these values later
c
	else	  !NOT FIRST -get init guesses from roots found in last iteration
c Modif 01/26/00 09:41am to use geometric mean of prev roots for guesses
c But if roots are grouped then must allow for this -e.g if roots 1-4
c are grouped as 'identical' then upper limit for root 1 must be
c -dsqrt(root(1)*root(5)) where root(5) is the next different root
c If 1st root is in a group it must be in group #1
c  For grouped case, need only one pair of guesses per group, so use
c 'do while' here, so that m can be incremented within the loop
c .NOT.FIRST section -first define guesses
c Modified 03/13/03 08:54am to scale guesses by same factor as change
c in eig(QFF) -this is done before the loop, because to get guesses need
c not only the scaled rootF(m) but also rootF(m+1)
	   if(.not.scaled) then
		do m=1,kF
		   rootset(m)=.false.
c scale the rootF from last iteration
		   scalefac=eigFF(m)/eigFFsav(m,jset)
c		   rsav=rootF(m)
		   rsav=rootFsav(m,jset)
		   rootF(m)=rsav*scalefac
		   scaled=.true. 	!values for current neval now scaled
		   if(idebug1.gt.1) then
			print 2641,m,rsav,scalefac,rootF(m),eigFF(m)
			if(dprt) write(8,2641) m,rsav,scalefac,
     &			rootF(m),eigFF(m)
2641			format(3x,i3,
     &		 ' prev root=',g13.6,' scal=',g13.6,
     &		 ' root*scal=',g13.6,' eigFF= ',g13.6)
		   endif
		enddo
	   endif
c
c===	   do m=1,kF
	   m=1
	   ng=1 		!numbers groups of 'identical' roots
	   do while(m.le.kF)
		if(.not.grouped) then
		   if(m.eq.1) then
c			s1F(1)=2.d0*rootF(1)
			s1F(1)=3.d0*rootF(1)
		   else
			s1F(m)=-dsqrt(rootF(m-1)*rootF(m))
		   endif
		   if(m.lt.kF) then
			s2F(m)=-dsqrt(rootF(m+1)*rootF(m))
		   else
c			s2F(kF)=-1.d-9 	!if m=kF  (1 nanosecond)
			s2F(kF)=rootF(kF)/5.d0
		   endif
		else if(grouped) then	!define guesses for group #ng
c1		   continue
		   if(m.eq.1) then
c			s1F(1)=2.d0*rootF(1)		!NB n1(1)=1 always
			s1F(1)=3.d0*rootF(1)
		   else
			s1F(m)=-dsqrt(rootF(n1(ng)-1)*rootF(n1(ng)))
		   endif
		   if(m.lt.kF) then
			s2F(m)=-dsqrt(rootF(n1(ng))*rootF(n2(ng)+1))
		   else
c			s2F(kF)=-1.d-9 	!if m=kF  (1 nanosecond)
			s2F(kF)=rootF(kF)/5.d0
		   endif
c
c		  A group may have an even or odd number of roots (e.g.get 3 equal
c		  roots with 3 identical, non-intercommunicating open states)
c		  If number is odd, then detW(s) should have opposite sign at each
c		  of the group and s1F(m) and s2F(m) should be OK as guesses
c		  for bisection.
c             If the group has an even number of roots, detW should have
c		  same sign on each side of the group, i.e. detW(s1f(m) and
c		  detW(s2F(m)) just found should have same sign if the number
c		  of roots is even. Thus s1F(m) and s2F(m) cannot be used
c  		  as start guesses for
c		  bisection, and FINDGRP must be called to locate
c		  suitable values for s1F(m) and s2F(m) for bisection below
c		  (or if they cannot be found in reasonable time, use the
c		  values of sout (corresponds to min/max in detw if isignf=-1)
c             First check number of roots in current group
		   ngrp=n2(ng)-n1(ng)+1
		   odd=mod(ngrp,2).ne.0
		   even=.not.odd
		   d1=DETWF(s1F(m))
		   nds1=ndscale		!scale from common/det4/
		   ids1=idsign		!sign of det from common/det4/
		   d2=DETWF(s2F(m))
		   nds2=ndscale		!scale from common/det4/
		   ids2=idsign		!sign of det from common/det4/
c		   same=(d1.gt.zero.and.d2.gt.zero).OR.
c     &		(d1.lt.zero.and.d2.lt.zero)		!same sign
		   same=(ids1*ids2.gt.0)		!same sign
c
		   if(.not.same.and.odd) then
			continue		!gueses OK for bisection
		   else if(same.and.even) then
c============fix FINDGRP
			call FINDGRP(detWF,kF,s1F(m),d1,s2F(m),d2,isignf,
     & 		 sout1,sout2,sout,detout,nerr)
			if(isignf.eq.1) then 	!sighn change found so bisect
			   s1F(m)=sout1
			   s2F(m)=sout2
 			else if(isignf.le.0) then
			   rootF(m)=sout		!skip bisection
			   rootset(m)=.true.	!root already set in FINDGRP
			endif
		   else
			print 74,ngrp,ng,s1F(m),s2F(m),d1,d2
			if(dprt) write(8,74) ngrp,ng,s1F(m),s2F(m),d1,d2
74			format(
     & ' WARNING -signs of det(s1) and det(s2) disagree with ',/,
     & ' number of roots  (',i2,') expected in group #',i2,/,
     & ' s1 = ',g13.6,' s2 = ',g13.6,/,
     & ' detW(s1) = ',g13.6,' detW(s2) = ',g13.6)
c Options at this point.  If there are not many short openings, then
c rootF may not be too far from eigQFF, so another option is to use
c that as guesses  -not used now
c Better idea -use rootF(m) from last iteration, scaled by ratio of last eigFF
c to current eigFF, in hope that even current rootF is not very close
c to eigFF, it will change in similar proportion from one iteration to
c the next.
c NB rootF already scaled in this way when not first, so don't want
c to scale more than once!
		      print 741
			if(dprt) write(8,741)
741			format(
     & ' Repeat full search, looking for ALL roots',/)
			recheck=.true.	!redo full rootsch
			goto 1
		   endif	!end of if(same.and.even) etc
c Set repeated values if more than one root in group
		   if(m.eq.n1(ng)) then
			s1sav=s1F(m)
			s2sav=s2F(m)
			rsav=rootF(m)
			if(ngrp.gt.1) then
			   do m1=n1(ng)+1,n2(ng)
				s1F(m1)=s1sav
				s2F(m1)=s2sav
				if(rootset(m)) rootF(m1)=rsav
				m=m+1
			   enddo
			endif
			m=n2(ng)	!ready to increment for next root at start of loop
			ng=ng+1	!use next group next time
		   endif
		endif	!end of 'if (grouped)'
		m=m+1		!next root
	   enddo		!end of do while(m.le.kF)
c   Make the guesses narrower if fit has become 'stable' (check that this
c   does not make them wider!)
	   if(stable) then	!can use narrower guesses
		do m=1,kF
		   d1=dabs(gfac*rootF(m))		!width of narrower guess
		   d=dabs(s1F(m)-rootF(m))	!width of lower guess
		   if(d1.lt.d) s1F(m)=rootF(m)-d1
		   d=dabs(s2F(m)-rootF(m))	!width of upper guess
		   if(d1.lt.d) s2F(m)=rootF(m)+d1
		enddo
	   endif
	endif		!end of guesses if(.not.first)
c
c INITIAL GUESSES NOW SET -do bisection to get roots
	Y=0.0d0
	ndisp=-1
	if(deb) ndisp=1
	epsy=-1.d0	  !so ignored
	newroot=.false.
52	continue	!return to try again after bisection error
c	do m=1,kF  !need to alter m in loop for identical roots, so use 'do while'
	ng=1 		!numbers groups of 'identical' roots
	m=1		!increment m at END of do-while loop
	do while (m.le.kF)
	   if(rootset(m)) then
		if(dprt) write(8,75) m,rootF(m)
		print 75,m,rootF(m)
75		format(' Shut time root ',i2,' already set to ',g13.6)
		goto 9		!rootF(m) already set -go on to next
	   endif
c
	   epsx=1.d-8*dabs(s1F(m))	!1.d-10 seems much the same
	   s1sav=s1F(m)
	   s2sav=s2F(m)
	   call BISECd(DETWF,s1F(m),s2F(m),Y,sout,yout,epsx,epsy,
     &     nerr1,ndisp,.false.)
c
	   if(iabs(nerr1).eq.1.or.iabs(nerr1).eq.2) then
      	print 41,m,nerr1,neval,s1sav,s2sav,
     &	 DETWF(s1sav),DETWF(s2sav)
      	if(dprt) write(8,41) m,nerr1,neval,s1sav,s2sav,
     &	 DETWF(s1sav),DETWF(s2sav)
41		format(/,
     &	 ' Error in shut time root #',i3,'; nerr = ',i3,
     &	 ' (function eval # = ',i6,')',/,
     &	 '  guesses, s1, s2 = ',2g13.6,/,
     &	 ' f(s1) = ',g13.6,',  f(s2) = ',g13.6,/,
     &	 '  Locating roots again')
c     &	 '  Searching for roots again')
c print the current QD before re-searching
c		dsav=discprt	!to control discprt in DATYP
c		discprt=dprt
c		call DATYP(QD,'  QD    ' ,.false.,k,k,km,km)
c		discprt=dsav
      	print 53,(eigFF(j),j=1,kF)
      	if(dprt) write(8,53) (eigFF(j),j=1,kF)
53		format(' Eigs of QFF:',10(5g13.6,/))
		if(grouped) then
		   print 721
		   if(dprt) write(8,721)
721 		   format(
     & '     - NOT assumimg ''identical'' roots')
		endif
		recheck=.true.	!redo full rootsch
		goto 1
	   endif	!end of bit done if BISEC fails

c Now go back to 1 to call rootsch -may need parameters to make rootsch
c do better next time -will we just get same values?
c
c Bisection OK so assign the root(s)
	   rootF(m)=sout
	   if(grouped.and.n1(ng).eq.m) then
		if(ngrp.gt.1) then
		   do m1=n1(ng),n2(ng)
			rootF(m1)=sout
			m=m+1
		   enddo
		endif
		m=n2(ng)	!ready to increment for next root at start of loop
		ng=ng+1	!use next group next time
	   endif
9	   continue		!skip to here if rootF(m) already set
	   m=m+1
	enddo	!end of m=1,kF loop
c
c  Set stable=true if rootsch has not been called for a long time (so narrower
c  guesses can then be used for bisection) (another criterion would be that
c  a parameter has not changed by more than 10% per iteration for several
c  iterations (at present, guesses set 10% each side of previous rootF)
	nstab1=200		!300 evals since last rootsch call
	nstab2=500		!300 evals since last rootsch call
	stable=.false.
	istab1=istab
	if(neval-nevrs.ge.nstab1) then	!nstab1 evals since last rootsch call
	   stable=.true.
	   istab=1
	   gfac=gfac1	!guesses = rootF +/- 10%
	endif
	if(neval-nevrs.ge.nstab2) then
	   stable=.true.
	   istab=2
	   gfac=gfac2	!guesses = rootF +/- 2%
	endif
	if(.not.stable) istab=0
	if(stable.and.istab.ne.istab1) then
	   print 76,gfac
	   if(dprt) write(8,76) gfac
76	   format(
     & ' Fit deemed ''stable'' so narrow guesses used (',f7.2,'*root)')
c	   pause
	else if(.not.stable.and.istab.ne.istab1) then
	   print 77
	   if(dprt) write(8,77)
77	   format(' Fit no longer deemed ''stable'' ')
c	   pause
	endif
c
c Check for groups of near-identical roots
c Need to check periodically that groups are still appropriate -after
c grouping started, the rootF() going into GETGROUP already has grouped
c values set identical so they obvioulsy get labelled as grouped again.
c Check every 100 (or ngpcheck=100) function evaluations by doing
c a full rootsch again (if not 'stable') OR if previous grouping is
c changed by call to GETGROUP
	if(checkgrp) then
	   gsav=grouped
	   call GETGROUP(rootF,kF,km)
	   if(grouped.and.(.not.gsav)) nevgg=neval	!eval # at which grouped set true
c	   ngpcheck=100		!now set at start
	   if(grouped.and.neval-nevgg.gt.ngpcheck) then
		grouped=.false.
		nevgg=neval
		Y=0.0d0
		ndisp=-1
		if(deb) ndisp=1
		epsy=-1.d0	  !so ignored
		newroot=.true.
		print 78,neval
		if(dprt) write(8,78) neval
78		format(' Check that groups of roots still appropriate',/,
     &       ' by full search after evaluation #',i6)
		recheck=.true.
		goto 1
c		goto 52
	   endif
	   if(grouped) then
		do i=1,ngroup
		   if(n2(i).gt.n1(i)) then
			print 80,i,n1(i),n2(i)
			if(dprt) write(8,80) i,n1(i),n2(i)
80			format(
     &	' Group ',i2,': asymptotic roots ',i2,' to ',
     &		i2,' treated as equal')
		   endif
		enddo
	   endif
	endif
c
	if(newroot) then
         print 271,(rootF(m),m=1,kF)
         if(dprt) write(8,271) (rootF(m),m=1,kF)
271	   format(' Shut time roots:',10(5g13.6,/))
	   newroot=.false.
c=== Test code -check spacing of roots (abs diff and ratio
	   print 700, (dabs(rootF(m)-rootF(m+1)),m=1,kF-1)
	   if(dprt) write(8,700) (dabs(rootF(m)-rootF(m+1)),m=1,kF-1)
700	   format(' difference     :',10(5g13.6,/))
	   print 701, (dabs(rootF(m)/rootF(m+1)),m=1,kF-1)
	   if(dprt) write(8,701) (dabs(rootF(m)/rootF(m+1)),m=1,kF-1)
701	   format(' ratio          :',10(5g13.6,/))
c===
	endif
c
	if(deb) then
	   do m=1,kA
		sout=rootA(m)
      	print 26,m,sout,-1000.d0/sout
      	if(discprt) write(8,26) m,sout,-1000.d0/sout
26		format(' root, tau ',i2,' for open times = ',2g13.6)
	   enddo
	   do m=1,kF
		sout=rootF(m)
      	print 28,m,sout,-1000.d0/sout
      	if(discprt) write(8,28) m,sout,-1000.d0/sout
28		format(' root, tau ',i2,' for shut times = ',2g13.6)
	   enddo
	endif
c
	if(idebug1.gt.1) then
	   print 262,kF,neval
	   if(dprt) write(8,262) kF,neval
262	   format(
     &' All ',i2,' shut time roots found for function evaluation # ',
     & i6,/,'    m      eig(QFF)     rootF(m)   ')
	   do m=1,kF
	   print 261,m,eigFF(m),rootF(m)
	   if(dprt) write(8,261) m,eigFF(m),rootF(m)
c261	   format(3x,i3,3x,g13.6,2x,g13.6)
	   enddo
	endif
c
c Now get left (row) eigenvectors for each root:
c Then SHUT: keep as rows of ROWVECF
	do m=1,kF
	   dum=DETWF(rootF(m))	!returns WF(s(m)) via common
	   if(nerr.ne.0) goto 999	!dealloc and return
c It is possible (though rare) to a zero determinant in eqoc_hjc (output not
c defined in this case. Now signalled by nerr2.ne.0, in common/perr
	   call EQOC_hjc(WF,pdum,kF,km,km)		!wf(100,100) where km=100
	   if(nerr2.ne.0) then
		nerr=82		!to signal replacement if theta on return to HJCLIK
		goto 999		!deallocate and RETURN
	   endif
	   do j=1,kF
	 	rowvecF(m,j)=pdum(j)	!copy to row j of rowvec
	   enddo
c Get right col vectors by direct solution c'(m)W'(s(m))=0 where '=transpose
c=	   call MATTRANS(WF,Q1,kF,kF,km)	!transpose of WF in Q1
	   call MATTRAN1(WF,Q1,kF,kF,km,kXm)	!transpose of WF in Q1
c It is possible (though rare) to a zero determinant in eqoc_hjc (output not
c defined in this case. Now signalled by nerr2.ne.0, in common/perr
	   call EQOC_hjc(Q1,pdum,kF,kXm,km)		!Q1(100,100) where km=100
	   if(nerr2.ne.0) then
		nerr=82		!to signal replacement if theta on return to HJCLIK
		goto 999		!deallocate and RETURN
	   endif
	   do i=1,kF
		colvecF(i,m)=pdum(i)	!copy to column m of colvec
	   enddo
	enddo
c===c Get right (column) eigenvectors in colvecF by inversion
c===      call MATINV(rowvecF,kF,km,colvecF,km)
	if(deb) then
	  call ATYPD(colvecf,'ColvecF ' ,kF,kF,kFm,kFm)
	endif
c
c Now areas for SHUT times
c   calc part of numerator same for all components
c Get QFA*EXPQA in QEXPQA -kF*kA
      call MATMUL(QFA,EXPQA,QEXPQA,kF,kA,kA,one,
     &	km,kAx, kAx,kAx, kFm,kAm)
c   calc c(m)*r(m) =kF*kF
	do m=1,kF
	   do i=1,kF
		do j=1,kF
		   Q1(i,j)=colvecF(i,m)*rowvecF(m,j)
		enddo
	   enddo

	   if(deb) then
		call DATYP(Q1,'col*row ',.false.,kF,kF,kXm,kXm)
	   endif
         call MATMUL(Q1,QEXPQA,Q1,kF,kF,kA,one,
     &    kXm,kXm, kFm,kAm, kXm,kXm)	!col(m)*row(m)*QFA*EXPQA in Q1
c  Now denominator =r(m)*W'(s(m))*c(m)
	   s=rootF(m)
	   call DENOMF(m,s,QFA,QAA,QAF,EXPQA,rowvecF,colvecF,
     &    tres,kA,kF,denF,kAx,kAm,kFm,km)
c=     &   tres,kA,kF,km,den)
c Copy Q1 to XFA(m), dividing by denom at same time
	   do i=1,kF
		do j=1,kA
		   XFA(i,j,m)=Q1(i,j)/denF
		enddo
	   enddo
	   if(docalc) then
            call MATMUL(phiF,Q1,row1,1,kF,kA,1.d0/denF,
     &		1,km, kXm,kXm, 1,km)
            call VECMUL(row1,ucol,ampF(m),kA,km,km)	!numerator for amplitude
	   endif
	enddo		!end of m loop
c
	if(pcalc) then
	   call PDFOUTd(' ASYMPTOTIC SHUT TIME DISTRIBUTION',-1,-1,ampF,
     &	rootF,kF,am1,sd1,km,.false.,.true.,discprt)
	   call ACONVd(ampF,rootF,kF,tres,discprt,km)	!renormalise areas
	else if(docalc) then
	   call ACONVd(ampF,rootF,kF,tres,.false.,km)	!renormalise areas
	endif
c
c END OF ASYMPOTIC DISTRIBUTION
999	continue		!exit
c  Keep eigFF from last iteration, to use for scaling guess for rootF on
c next iteration
	nevlast=neval
	do m=1,kF
	   eigFFsav(m,jset)=eigFF(m)
	enddo
c
	DEALLOCATE(GAF,GFA,invQAA,invQFF)
	DEALLOCATE(rowvecA,rowvecF,colvecA,colvecF)
	DEALLOCATE(FMAT,EIGFF,AMAT,EIGAA)
	DEALLOCATE(Q1,Q2)
	DEALLOCATE(rootset)
	RETURN
	end


