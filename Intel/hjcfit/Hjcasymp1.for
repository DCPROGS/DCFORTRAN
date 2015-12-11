	subroutine HJCASYMP(iasympPanel,QD,k,kA,kF,phiA,phiF,QEXPQA,QEXPQf,
     & XAF,XFA,
     & rootA,rootF,rootAsav,rootFsav,jset,
     & s1A,s2A,s1F,s2F,first,kAm,kFm,irt,QAA,QAF,QFF,QFA,EXPQF,EXPQA)
	use menu_f90
	use gino_f90

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
c Modified 09-02-06 to use eqoc_red (rather than eqoc_hjc) to solve row*W=0

c Modified 08/07/03 09:17pm so make rootAsav(*,jset), rootFsav(*,jset), jset
c arguments of HJCASYMP, because, at previously, when 'previous root' was used
c in hjcasymp, the root from the last iteration was taken from the wrong set
c when nset>1.
c Similarly the array eigFFsav, must also now have jset as
c index.  Also setting of guesses for open states now made the same as for
c shut states (apart from 'grouped' option which is not avalaible for ope
c states -this requires definition of eigAAsav too.
c
c Modifed 08/04/03 to use Ball method in ROOT_FB, rather than ROOTSCH,
c to locate guesses for bisection of roots.
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

c	PARAMETER kAx=60 	!max number of open states for following
	PARAMETER kAx=20 	!max number of open states for following

	real*8 one,det
	real*8 XAF(kAm,kFm,kAm),XFA(kFm,kAm,kFm)
	real*8 QEXPQA(kFm,kAM),QEXPQF(kAm,kFm)
	real*8 QD(100,100)
	character*256 string
c===change plan again -keep commons /detw1 and /detw3 so make QAF etc fixed size
c=	real*8 QAF(kAm,kFm),QFA(kFm,kAm),QAA(kAm,kAm),QFF(kFm,kFm)
c=	real*8 EXPQF(kFm,kFm),EXPQA(kAm,kAm)
c=	real*8 WA(kAm,kAm),WF(kFm,kFm)
	real*8 QAF(20,100),QFA(100,20),QAA(20,20),QFF(100,100)
	real*8 EXPQA(20,20),EXPQF(100,100)
	real*8 WA(20,20),WF(100,100)
	real*8 dexp1
c	common/detw1/QAA,QAF,QFF,QFA,EXPQF,EXPQA   !for DETWA,DETWF,dARSdS -now param
	common/detw3/WA,WF			  !for DETWA,DETWF -now params
c Local (but saved)
c	real*8 eigFFsav(100)
	real*8 eigAAsav(kAx,10),eigFFsav(100,10)
	common/eigsav/eigAAsav,eigFFsav
	
c=	real*8 GAF(kAM,kFm),GFA(kFm,kAm)
	allocatable::GAF,GFA
	real*8 GAF(:,:),GFA(:,:)
	character*22 rstring
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
	logical discprt,first,newroot,oneset
	logical pcalc,docalc,deb,recheck
	real*8 detwA,detwF
	real*8 tres,sum,amax,fmax,tcrit1
c To ABORT fit
c	integer*2 ktype
c	character ch,getch
c	logical KBHIT		!Utility lib
	logical ABORTW
	common/abt/ABORTW
c from detwf, detwa
	common/det4/ndscale,idsign
c
	EXTERNAL detwA,detwF
	EXTERNAL gfuncA,gfuncF	!for Ball method
c
	logical debprt,dprt
c	logical dsav
c change
	
	common/deb/idebug,idebug1
	
	common/setblk/oneset,iset	!for HJCLIK,DISP to specify one set
	common/detw2/tres,km,nerr			!for HJCLIK,DETWA,DETWF
	common/fitblk1/ampA,ampF		!for HJCDISP
	common/perr/nerr2		!for errors in eqoc_hjc
	common/dp/discprt,deb
c For dealing with 'identical' roots
	logical slopsch,checkgrp,grouped,gsav,odd,even,same
	logical stable,stableA,stableF
	allocatable::rootset
	logical rootset(:)
c	integer ngrp(100)		!number of roots in the ith group
	integer*4 n1(100),n2(100)
	common/grp1/slopsch,checkgrp,grouped,rcrit,ngpcheck
	common/grp2/ngroup,n1,n2
	real*8 rcrit,gfac1,gfac2
	common/stab/stable,nstab1,gfac1,nstab2,gfac2,istab	!for hjcfit
c
	logical scaled
	common/eval/neval,niter	!from SIMPHJC, for print in hjcasymp
c
	common/timer/idebugt,itick,itlast,ndeb
	common/LIG/nligsav,IL(100)
	
	character*11 cstring1,cstring2
	character*22 cstring3
	SAVE nevrs,nevlast
	SAVE nloopA,s1Alast,s2Alast
	SAVE nloopF,s1Flast,s2Flast
	COMMON/determ/det	
	common/tty/ittypanel,itty
	common/KBLK/kAn,kB,kC,kD
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
ccq	det=0.
	if(.not.checkgrp) then
	   grouped=.false.		!checkgrp -set in hjcfit
	   do m=1,kF
		rootset(m)=.false.
	   enddo
	endif
	goto 222
	do i=1,kam
		eigaa(i)=0.0
		do j=1,kam
		do kl=1,kam
		amat(i,j,kl)=0.0
		enddo
		enddo
	enddo
	do i=1,kfm
		eigff(i)=0.0
		do j=1,kfm
		do kl=1,kfm
		fmat(i,j,kl)=0.0
		enddo
		enddo
	enddo
222	AA=11
	AF=15		!5=CODE FOR F(=B+C) SECTION
	FA=51
	FF=55
	kan=ka
	k=kA+kF
	one=1.0d0
	do n=1,100
	   ucol(n,1)=one
	enddo
	scaled=.false.
c
c Set pcalc=true if extra calculations to be done AND printed; docalc=true
c if they are to be done (for plots) but not printed
	dprt=discprt.and.debprt	 !put problems in root-finding on disc as well as screen
	
	pcalc=deb.or.idebug.eq.8.or.idebug.eq.9
	docalc=pcalc.or.oneset
c
c	if(.not.first.and.niter.eq.1) then
c	   if(discprt) write(7,2632) jset,neval,niter
c2632	   format(/,
c     &	' Entering HJCASYMP for set ',i3,/,
c     &	' at start of function evaluation # ',i6,' iteration # ',i5)
c	endif
c Calculate GAF and GFA -do explicitly (GMATn gets complicated when arrays
c all have different declared sizes)
	
	call SUBMAT(QD,AA,QAA,km,km,kAx,kAx)
	call MATINV2(QAA,kA,kAx,invQAA,kAm,.false.,det,ndscale)
	call SUBMAT(QD,AF,Q1,km,km,kXm,kXm)
	call MATMUL(invQAA,Q1,GAF,kA,kA,kF,-one,
     &	kAm,kAm,kXm,kXm,kAm,kFm)

	call SUBMAT(QD,FF,QFF,km,km,km,km)
	call MATINV2(QFF,kF,km,invQFF,kFm,.false.,det,ndscale)
	call SUBMAT(QD,FA,Q1,km,km,kXm,kXm)
	call MATMUL(invQFF,Q1,GFA,kF,kF,kA,-one,
     &	kFm,kFm,kXm,kXm,kFm,kAm)
	
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
cpp         print 53,jset,(eigFF(j),j=1,kF)
		if(discprt) write(7,53) jset,(eigFF(j),j=1,kF)
cpp         print 71
		if(dprt) write(7,71)
		CALL GMSETTEXTSETTING(ITTY,
     &	' QFF has eigenvalues >1e10 or <1e-10; current parameters')
    
71		format(
     &	' QFF has eigenvalues >1e10 or <1e-10; current parameters')
		nerr=8		!to signal replacement on return to HJCLIK
		goto 999		!deallocate and RETURN
	endif
	if(deb) then
c         print 53,jset, (eigFF(j),j=1,kF)
         if(discprt) write(7,53) jset,(eigFF(j),j=1,kF)
	endif
c ===debug
c	if(idebug.eq.13) then
	nevlast=neval
	
	if(.not.first.and.idebug1.gt.1) then
c	   print 263,jset,neval,niter
	   if(dprt) write(7,263) jset,neval,niter
263	   format(
     &	' QFF for set ',i3,' at start of function evaluation # ',
     &	i6,' Niter = 'i6,/,
     &	'    m      prev eig(QFF)   eig(QFF)   scalefac')
		do m=1,kF
			scalefac=eigFF(m)/eigFFsav(m,jset)
c			print 264,m,eigFFsav(m,jset),eigFF(m),scalefac
			if(dprt) write(7,264) m,eigFFsav(m,jset),eigFF(m),scalefac
264			format(3x,i3,3(3x,g13.6))
	   enddo
	endif
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
cpp         print 510,jset,nroot,(eigAA(j),j=1,kA)
         if(dprt) write(7,510) jset,nroot,(eigAA(j),j=1,kA)
510	   format(' SET ',i3,': nroot = ',i3,' EigAA = ',10(5g13.6,/))
cpp         print 70
         if(dprt) write(7,70)
70	   format(
     &' QAA has eigenvalues >1e10 or <1e-10; current parameters')
	   nerr=8		!to signal replacement on return to HJCLIK
	   	
		CALL GMSETTEXTSETTING(ITTY,
     &' Qaa has eigenvalues >1e10 or <1e-10; current parameters')
   
	   goto 999		!deallocate and RETURN

	endif
	if(deb) then
cp         print 510,jset,nroot,(eigAA(j),j=1,kA)
         if(dprt) write(7,510) jset,nroot,(eigAA(j),j=1,kA)
	endif
	if(.not.first.and.idebug1.gt.1) then
cp	   print 2631,jset,neval,niter
	   if(dprt) write(7,2631) jset,neval,niter
2631	   format(
     &' QAA for set ',i3,' at start of function evaluation # ',
     &  i6,' Niter = '
     & i6,/,'    m      prev eig(QAA)   eig(QAA)   scalefac')
	   do m=1,kA
		scalefac=eigAA(m)/eigAAsav(m,jset)
cp		print 264,m,eigAAsav(m,jset),eigAA(m),scalefac
		if(dprt) write(7,264) m,eigAAsav(m,jset),eigAA(m),scalefac
c264		format(3x,i3,3(3x,g13.6))
	   enddo
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
!!!	if (first) pcalc=.true.
!	pcalc=.false.
	if(pcalc) then
	
		write(string,fmt='(a40)') 
     &	'HJC Initial vector for open times ='
		   CALL GMSETTEXTSETTING(ITTY,string) 
		
		write(string,fmt='(10g13.6)') (phia(1,j),j=1,kA)
	
    
		CALL GMSETTEXTSETTING(ITTY,string) 
		
	
cpp         print 20,(phiA(1,j),j=1,kA)
         if(discprt) write(7,20) (phiA(1,j),j=1,kA)
20	   format(
     & ' HJC Initial vector for open times = ',/,10g13.6)
cpp         print 21,(phiF(1,j),j=1,kF)
         
	   if(discprt) write(7,21) (phiF(1,j),j=1,kF)
	
		write(string,fmt='(a40)') 
     &	'HJC Initial vector for shut times ='
		   CALL GMSETTEXTSETTING(ITTY,string) 
	
		write(string,fmt='(10g13.6)') (phif(1,j),j=1,kf)
	    CALL GMSETTEXTSETTING(ITTY,string) 
21	   format(
     & ' HJC Initial vector for shut times = ',/,10g13.6)
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
c		rootset(m)=.false.
		rootA(m)=eigAA(m)
	   enddo
	endif
c
c	Find the guesses for bisection
11	continue 	!return here to re-do 'first'
	if(first.or.recheck) then

	   nevrs=neval	!number of function eval for last call to rootsch
	   sa=-100000.d0
cc	   sa=-10000.d0
	   sb=-10.d0
	   call ROOT_FB(sa,sb,GfuncA,kA,s1A,s2A,nroot,kAm,detwA,nerr,
     &   QAA,QAF,QFF,QFA,EXPQF,EXPQA)
	   if(ABORTW) goto 999	!kbhit now in rootsch
	   call intconv(nroot,cstring1)
	   call intconv(ka,cstring2)
	   if(nroot.ne.kA) then
			CALL GMSETTEXTSETTING(ITTY,
     &		'ROOT_FB failed to locate all open time roots')
		
			if(dprt) write(7,51) kA,(rootA(j),j=1,kA)
51			 format(
     &		' ROOT_FB failed to locate all ',i3,' open time roots for',
     &		' initial guesses',/,
     &		' rootA:',10(5g13.6,/))
			 nerr=5
			goto 999		!deallocate and RETURN
	   
		 
		else

	
ccc			CALL GMSETTEXTSETTING(ITTy,'ROOT_FB located all open time roots')

	   endif
	   scaled=.true. 		!have exact rootA so no need to scale
	   recheck=.false.
	else		!NOT first -get init guesses from roots found in last iteration
	   scaled=.true.
	   if(.not.scaled) then
		do m=1,kA
		   rootset(m)=.false.
c		   scale the rootF from last iteration
		   scalefac=eigAA(m)/eigAAsav(m,jset)
		   rsav=rootAsav(m,jset)
		   rootA(m)=rsav*scalefac
		   scaled=.true. 	!values for current neval now scaled
		   
	   enddo
	   endif
	   m=1
	   do while(m.le.kA)
		if(m.eq.1) then
c---		   s1A(1)=5.d0*rootA(1)
		   s1A(1)=3.d0*rootA(1)
		else
		   s1A(m)=-dsqrt(rootA(m-1)*rootA(m))
		endif
		if(m.lt.kA) then
		   s2A(m)=-dsqrt(rootA(m+1)*rootA(m))
		else
		   s2A(kA)=-1.d-9 	!if m=kA  (1 nanosecond)
c---		   s2A(kA)=rootA(kA)/5.d0
		endif
		m=m+1		!next root
	   enddo		!end of do while(m.le.kA)
	endif		!end of if(first.or.recheck)

c INITIAL GUESSES NOW SET -do bisection to get roots
	
	Y=0.0d0
	ndisp=-1
	if(deb) ndisp=1
	epsy=-1.d0	  !so ignored
	ng=1 		!numbers groups of 'identical' roots
	m=1		!increment m at END of do-while loop
	
	do while (m.le.kA)
c
c	   epsx=1.d-8*dabs(s1A(m))	!1.d-10 seems much the same
	   epsx=1.d-10*dabs(s1A(m))	!1.d-10 seems much the same
	   nsmax=100	!max number of bisection steps
	   s1sav=s1A(m)
	   s2sav=s2A(m)
c 02/07/06 12:05pm NB the determinant, det(W), may be VERY sensitive
c to small changes in W.  A convergence criterion that gives the root
c to quite high accuracy may give det(W) that seems 'too far' from zero. But
c the subsequent solution for right and left eigenvectors assumes that det(W)=0
c 1.d08 should do with bisechjc, which increases number of steps when nec
c	   call BISECd(DETWA,s1A(m),s2A(m),Y,sout,yout,epsx,epsy,
c     &    nerr1,ndisp,.false.)
	   call BISEChjc(DETWA,s1A(m),s2A(m),Y,sout,yout,epsx,epsy,nsmax,
     &    nerr1,ndisp,.false.,QAA,QAF,QFF,QFA,EXPQF,EXPQA)
	   if(iabs(nerr1).eq.1.or.iabs(nerr1).eq.2) then
cpp      	print 40,m,nerr1,s1sav,s2sav,DETWA(s1sav),DETWA(s2sav)
       	if(dprt) write(7,40) m,nerr1,s1sav,s2sav,DETWA(s1sav,
     & QAA,QAF,QFF,QFA,EXPQF,EXPQA),
     &	 DETWA(s2sav,QAA,QAF,QFF,QFA,EXPQF,EXPQA)
40		format(/,
     & ' Error in open time root #',i3,'; nerr = ',i3,/,
     & '  guesses, s1, s2 = ',2g13.6,/,
     & ' f(s1) = ',g13.6,',  f(s2) = ',g13.6,/,
     & '  locate roots again (Ball method)')
		if(s1sav.eq.s1Alast.and.s2sav.eq.s2Alast) then
		   nloopA=nloopA+1
		   if(nloopA.ge.10) then
			CALL GMSETTEXTSETTING(ITTy,'Same guesses')

			nloopA=0
			nerr=7	!loop problem
			goto 999		!deallocate and RETURN
		   endif
		else
		   nloopA=0
		   s1Alast=s1sav
		   s2Alast=s2sav
		endif
cpp      	print 52,jset,(eigAA(j),j=1,kA)
      	if(dprt) write(7,52) jset,(eigAA(j),j=1,kA)
52		format(' SET ',i3,' Eigs of QAA:',/,10(5g13.6,/))
		recheck=.true.	!redo full rootsch
		goto 11		!recheck
	   endif	!end of bit done if BISEC fails
c
c Bisection OK so assign the root(s)
	   rootA(m)=sout
		if(first.and.iasymppanel.ne.-2) then
		WRITE(STRING,fmt='(a6,g13.6)') 'root=', sout
		r4=sngl(sout)
		CALL GMSETTEXTSETTING(ITTY,string)	
		endif
	
	   m=m+1
	enddo	!end of m=1,kA loop

c  Set stable=true if rootsch has not been called for a long time (so narrower
c  guesses can then be used for bisection) (another criterion would be that
c  a parameter has not changed by more than 10% per iteration for several
c  iterations (at present, guesses set 10% each side of previous rootF)
c	nstab1=200		!evals since last rootsch call -set in hjcfit
c	nstab2=500		!evals since last rootsch call
	stable=.false.
	stableA=.false.
	istabsav=istab
	if(neval-nevrs.ge.nstab1) then	!nstab1 evals since last rootsch call
	   stableA=.true.
	   istab=1
	   gfac=gfac1	!guesses = rootF +/- 10%
	endif
	if(neval-nevrs.ge.nstab2) then
	   stableA=.true.
	   istab=2
	   gfac=gfac2	!guesses = rootF +/- 2%
	endif
	stable=stableA.and.stableF
	if(.not.stable) istab=0
	if(stable.and.istab.ne.istabsav) then
	   if(istab.eq.1) then
cpp		print 76,gfac
		if(dprt) write(7,76) gfac
c76		format(
c     & ' Fit deemed ''stable'' so narrow guesses used (',f7.2,'*root)')
c		pause
	   else if(istab.eq.2) then
cpp		print 761,gfac
		if(dprt) write(7,761) gfac
c761		format(
c     & ' Fit deemed ''very stable'' so narrower guesses used ('
c     & ,f7.2,'*root)')
	   endif
c	   pause
	else if(.not.stable.and.istab.ne.istabsav) then
cpp	   print 77
	   if(dprt) write(7,77)
c77	   format(' Fit no longer deemed ''stable'' ')
c	   pause
	endif
c
c Now get left (row) eigenvectors for each root:
c These are needed only to calculate areas for the asymptotoc pdf
c NB if kA=1 the area must be one whether renrmalised or not. In fact
c eqoc_red crashes for one component because reduced matrix is 0 x 0
c Next bit doesn't work, becasue it skips the calculation of XAF
c so get eGAF(t)= 0 later. Problem is in solving rWA=0, ru=1 when kA=1
c In this case row(1) and WA are both scalar ws WA=0 at the root,
c Hence any value of row(1) is a solution but since ru=1, row(1)=1 is what's
c needed. Likewise col(1)=1. So these values can be set, and eqoc-re skipped
c      if(kA.eq.1) then
c         ampA(1)=rootA(1)     !so area=1
c         goto 13
c      endif
c First OPEN: keep as rows of ROWVECA
	nerr=0
	do 32 m=1,kA
	 dum=DETWA(rootA(m),QAA,QAF,QFF,QFA,EXPQF,EXPQA)	!called to return WA(s(m)) via common
	 if(nerr.ne.0) goto 999		!deallocate and RETURN
c It is possible (though rare) to a zero determinant in eqoc_hjc (output not
c defined in this case. Now signalled by nerr2.ne.0, in common/perr
c	 call EQOC_hjc(WA,pdum,kA,60,km)
	if(kA.gt.1) then
	 call EQOC_red(WA,pdum,kA,kAx,km)
	else
		pdum(1)=1.0d0
	endif
	 if(pcalc.or.idebug3.eq.1) then
	    imode=1
	    root=rootA(m)
c		kAx=60
c		kAx=20
	    call checkrW(WA,pdum,kA,kAx,km,pcalc,imode,root,m,nerr3)
	 endif

	 if(nerr2.ne.0) then

c It may be too drastic to reset parameters for nerr2=4 (unit matrix error>0.01) in eqoc
c so try doing this only if nerr2=1 (virtually never)
c	    if(nerr2.eq.4) then	!severe error
	    if(nerr2.eq.1) then	!severe error
c***		 print 406,kA
		 if(discprt) write(7,406) kA
		 nerr=81		!to signal replacement if theta on return to HJCLIK
		 goto 999		!deallocate and RETURN
	    endif
	 endif
	
	 do 33 j=1,kA
33	  rowvecA(m,j)=pdum(j)	!copy to rowvec
	  if(deb) then		!check pdum*WA=[0 0 ...]
	   do 321 j=1,kA
321	   row1(1,j)=pdum(j)
         call MATMUL(row1,WA,row1,1,kA,kA,one,1,km,kAx,kAx,1,km)
cp         print 322,(pdum(j),j=1,kA)
         if(discprt) write(7,322) (pdum(j),j=1,kA)
322	   format(' r(m) = ',10g13.6)
cp        print 323,(row1(1,j),j=1,kA)
         if(discprt) write(7,323) (row1(1,j),j=1,kA)
323	   format(' r(m)*W(m) = ',10g13.6)	!should be [0 0 ...]
	  endif
c Get right col vectors by direct solution c'(m)W'(s(m))=0 where '=transpose
c=	  call MATTRANS(WA,Q1,kA,kA,km)	!transpose of WF in Q1
	  call MATTRAN1(WA,Q1,kA,kA,kAx,kXm)	!transpose of WF in Q1
c It is possible (though rare) to a zero determinant in eqoc_hjc (output not
c defined in this case. Now signalled by nerr2.ne.0, in common/perr
c	  call EQOC_HJC(Q1,pdum,kA,kXm,km)		!c'(m) in pdum

	  if(kA.gt.1) then
		 call EQOC_red(Q1,pdum,kA,kXm,km)
	  else
		pdum(1)=1.0d0
	  endif

	  if(pcalc.or.idebug3.eq.1) then
	     imode=2
	     root=rootA(m)
	     call checkrW(Q1,pdum,kA,kXm,km,pcalc,imode,root,m,nerr3)
	   endif

c It may be too drastic to reset parameters for nerr2=4 (unit matrix error>0.01) in eqoc
c so try doing this only if nerr2=1 (virtually never)
c	   if(nerr2.eq.4) then	!severe error
	   if(nerr2.eq.1) then	!severe error
c***		   print 406,kA
		   if(discprt) write(7,406) kA
		   nerr=81		!to signal replacement if theta on return to HJCLIK
		   goto 999		!deallocate and RETURN
	   endif
c
c
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
c
cc Jump to here if kA=1 (with ampA(1) set too rootA(1) so area=1)
c13    continue -not used now
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
ccq	det=0.
	scaled=.false.	!reset for shut times
	if(first) then
	   do m=1,kF
		rootset(m)=.false.
		rootF(m)=eigFF(m)
	   enddo
	endif
c
c Find the guesses for bisection
	nerr=0
1	continue 	!return here to re-do 'first'
	if(first.or.recheck) then
	   do m=1,kF
		rootset(m)=.false.
	   enddo
	   grouped=.false.	!first time
	   nevrs=neval	!number of function eval for last call to rootsch
	   if(idebug1.gt.0) then
cp		print 791
		if(dprt) write(7,791)
c791		format(' Searching for SHUT time asymptotic roots')
791		format(
     &' Locating guesses for SHUT time asymptotic roots (Ball method)')
	   endif

	   sa=-100000.d0
	   sb=-0.01d0
	   call ROOT_FB(sa,sb,GfuncF,kF,s1F,s2F,nroot,kFm,detwF,nerr,
     &   QAA,QAF,QFF,QFA,EXPQF,EXPQA)
	   if(ABORTW) goto 999	!kbhit now in rootsch
	   call intconv(kf,cstring1)
	   if(nroot.ne.kF) then

		CALL GMSETTEXTSETTING(ITTY,
     &	' ROOT_FB failed to locate all shut time roots ')	

           if(dprt) write(7,531) jset,kF,(rootF(j),j=1,kF)
531	     format(' SET ',i3,/,
     &  ' ROOTSCH failed to locate all ',i3,' shut time roots for',
     &  ' initial guesses',/,
     &  ' rootF:',10(5g13.6,/))
	     nerr=5
	     goto 999		!deallocate and RETURN
		else
	
ccc	if(first) 
ccc     &CALL GMSETTEXTSETTING(ITTY,' ROOT_FB located all shut time roots')
c		irt=irt+1

	   endif
	   scaled=.true. 		!have exact rootF so no need to scale
	   recheck=.false.
	else	  !NOT FIRST -get init guesses from roots found in last iteration
ccc
ccc		scaled=.true.
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
cp			print 2641,m,rsav,scalefac,rootF(m),eigFF(m)
			if(dprt) write(7,2641) m,rsav,scalefac,
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
		   d1=DETWF(s1F(m),QAA,QAF,QFF,QFA,EXPQF,EXPQA)
		   nds1=ndscale		!scale from common/det4/
		   ids1=idsign		!sign of det from common/det4/
		   d2=DETWF(s2F(m),QAA,QAF,QFF,QFA,EXPQF,EXPQA)
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
cpp			print 74,ngrp,ng,s1F(m),s2F(m),d1,d2
			if(dprt) write(7,74) ngrp,ng,s1F(m),s2F(m),d1,d2
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
cpp		      print 741
			if(dprt) write(7,741)
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
c52	continue	!return to try again after bisection error
c	do m=1,kF  !need to alter m in loop for identical roots, so use 'do while'
	ng=1 		!numbers groups of 'identical' roots
	m=1		!increment m at END of do-while loop
	do while (m.le.kF)
	   if(rootset(m)) then
		if(dprt) write(7,75) m,rootF(m)
cpp		print 75,m,rootF(m)
75		format(' Shut time root ',i2,' already set to ',g13.6)
		goto 9		!rootF(m) already set -go on to next
	   endif
c
c	   epsx=1.d-8*dabs(s1F(m))	!1.d-10 seems much the same
	   epsx=1.d-10*dabs(s1F(m))	!1.d-10 seems much the same
	   nsmax=100		!max number of bisection steps
	   s1sav=s1F(m)
	   s2sav=s2F(m)
c	   call BISECd(DETWF,s1F(m),s2F(m),Y,sout,yout,epsx,epsy,
c     &     nerr1,ndisp,.false.)
	   call BISEChjc(DETWF,s1F(m),s2F(m),Y,sout,yout,epsx,epsy,nsmax,
     &     nerr1,ndisp,.false.,QAA,QAF,QFF,QFA,EXPQF,EXPQA)


c
	   if(iabs(nerr1).eq.1.or.iabs(nerr1).eq.2) then
c     	print 41,m,nerr1,neval,s1sav,s2sav,
c    &	 DETWF(s1sav),DETWF(s2sav)
      	if(dprt) write(7,41) m,nerr1,neval,s1sav,s2sav,
     &	 DETWF(s1sav,QAA,QAF,QFF,QFA,EXPQF,EXPQA),
     &     DETWF(s2sav,QAA,QAF,QFF,QFA,EXPQF,EXPQA)
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
cpp      	print 53,jset,(eigFF(j),j=1,kF)
		if(s1sav.eq.s1Flast.and.s2sav.eq.s2Flast) then
		   nloopF=nloopF+1
		   if(nloopF.ge.10) then
c			print 405,nloopF
c405			format(' Same guesses for ',i3,' tries -in loop')
			CALL GMSETTEXTSETTING(ITTY,'same guesses')
			nloopF=0
			nerr=7	!loop problem
c			pause		!for debug
			goto 999		!deallocate and RETURN
		   endif
		else
		   nloopF=0
		   s1Flast=s1sav
		   s2Flast=s2sav
		endif
      	if(dprt) write(7,53) jset,(eigFF(j),j=1,kF)
53		format(' SET ',i3,' Eigs of QFF:',/,10(5g13.6,/))
		if(grouped) then
cpp		   print 721
		   if(dprt) write(7,721)
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
	
		if(first.and.iasymppanel.ne.-2) then
		WRITE(STRING,fmt='(a6,g13.6)') 'root=', sout
		CALL GMSETTEXTSETTING(ITTY,string)
		endif
		
	   m=m+1
	enddo	!end of m=1,kF loop
	
c
c  Set stable=true if rootsch has not been called for a long time (so narrower
c  guesses can then be used for bisection) (another criterion would be that
c  a parameter has not changed by more than 10% per iteration for several
c  iterations (at present, guesses set 10% each side of previous rootF)
c	nstab1=200		!evals since last rootsch call -set in hjcfit
c	nstab2=500		!evals since last rootsch call
	stable=.false.
	stableF=.false.
	istabsav=istab
	if(neval-nevrs.ge.nstab1) then	!nstab1 evals since last rootsch call
	   stableF=.true.
	   istab=1
	   gfac=gfac1	!guesses = rootF +/- 10%
	endif
	if(neval-nevrs.ge.nstab2) then
	   stableF=.true.
	   istab=2
	   gfac=gfac2	!guesses = rootF +/- 2%
	endif
	stable=stableA.and.stableF
	if(.not.stable) istab=0
	if(stable.and.istab.ne.istabsav) then
	   if(istab.eq.1) then
cpp		print 76,gfac
		if(dprt) write(7,76) gfac
76		format(
     & ' Fit deemed ''stable'' so narrow guesses used (',f7.2,'*root)')
c		pause
	   else if(istab.eq.2) then
cpp		print 761,gfac
		if(dprt) write(7,761) gfac
761		format(
     & ' Fit deemed ''very stable'' so narrower guesses used ('
     & ,f7.2,'*root)')
	   endif
c	   pause
	else if(.not.stable.and.istab.ne.istabsav) then
cpp	   print 77
	   if(dprt) write(7,77)
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
	   call GETGROUP1(rootF,kF,km)
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
cpp		print 78,neval
		if(dprt) write(7,78) neval
78		format(' Check that groups of roots still appropriate',/,
     &       ' by full search after evaluation #',i6)
		recheck=.true.
		goto 1
c		goto 52
	   endif
	   if(grouped) then
		do i=1,ngroup
		   if(n2(i).gt.n1(i)) then
cpp			print 80,i,n1(i),n2(i)
			if(dprt) write(7,80) i,n1(i),n2(i)
80			format(
     &	' Group ',i2,': asymptotic roots ',i2,' to ',
     &		i2,' treated as equal')
		   endif
		enddo
	   endif
	endif
c
	if(newroot) then
cpp         print 271,(rootF(m),m=1,kF)
         if(dprt) write(7,271) (rootF(m),m=1,kF)
271	   format(' Shut time roots:',10(5g13.6,/))
	   newroot=.false.
c=== Test code -check spacing of roots (abs diff and ratio
cpp	   print 700, (dabs(rootF(m)-rootF(m+1)),m=1,kF-1)
	   if(dprt) write(7,700) (dabs(rootF(m)-rootF(m+1)),m=1,kF-1)
700	   format(' difference     :',10(5g13.6,/))
cpp	   print 701, (dabs(rootF(m)/rootF(m+1)),m=1,kF-1)
	   if(dprt) write(7,701) (dabs(rootF(m)/rootF(m+1)),m=1,kF-1)
701	   format(' ratio          :',10(5g13.6,/))
c===
	endif
c
	if(deb) then
	   do m=1,kA
		sout=rootA(m)
cp      	print 26,m,sout,-1000.d0/sout
      	if(discprt) write(7,26) m,sout,-1000.d0/sout
26		format(' root, tau ',i2,' for open times = ',2g13.6)
	   enddo
	   do m=1,kF
		sout=rootF(m)
cp      	print 28,m,sout,-1000.d0/sout
      	if(discprt) write(7,28) m,sout,-1000.d0/sout
28		format(' root, tau ',i2,' for shut times = ',2g13.6)
	   enddo
	endif
c
	
c
c	Now get left (row) eigenvectors for each root:
c	Then SHUT: keep as rows of ROWVECF de verificat aici
c These are needed only to calculate areas for the asymptotoc pdf
c NB if kF=1 the area must be one whether renrmalised or not. In fact
c eqoc_red crashes for one component because reduced matrix is 0 x 0
c
	do m=1,kF
	   dum=DETWF(rootF(m),QAA,QAF,QFF,QFA,EXPQF,EXPQA)	!returns WF(s(m)) via common
	   if(nerr.ne.0) goto 999	!dealloc and return
c		It is possible (though rare) to a zero determinant in eqoc_hjc (output not
c		defined in this case. Now signalled by nerr2.ne.0, in common/perr
cccc		idebug=0
		
c	   call EQOC_hjc(WF,pdum,kF,km,km)		!wf(100,100) where km=100
ccc		   idebug=0
	   if(kF.gt.1) then
		 call EQOC_red(WF,pdum,kF,km,km)
	   else
		 pdum(1)=1.0d0
	   endif

	   if(pcalc.or.idebug3.eq.1) then
	      imode=3
	      root=rootF(m)
	      call checkrW(WF,pdum,kF,km,km,pcalc,imode,root,m,nerr3)
	   endif
	   
c It may be too drastic to reset parameters for nerr2=4 (unit matrix error>0.01) in eqoc
c so try doing this only if nerr2=1 (virtually never)
c	    if(nerr2.eq.4) then	!severe error
	    if(nerr2.eq.1) then	!severe error
c		   print 406,kF
		   write(7,406),kF
406		   format(
     &   ' Matrix inversion bad in solving rW=0: k = ',i3)
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
c	 call EQOC_hjc(Q1,pdum,kF,kXm,km)		!Q1(100,100) where km=100
	if(kF.gt.1) then
	    call EQOC_red(Q1,pdum,kF,kXm,km)
	else
		pdum(1)=1.0d0
	endif
	 
	 if(pcalc.or.idebug3.eq.1) then
	    imode=4
	    root=rootF(m)
	    call checkrW(Q1,pdum,kF,kXm,km,pcalc,imode,root,m,nerr3)
	 endif
c It may be too drastic to reset parameters for nerr2=4 (unit matrix error>0.01) in eqoc
c so try doing this only if nerr2=1 (virtually never)
c	 if(nerr2.eq.4) then	!severe error
	 if(nerr2.eq.1) then	!severe error
c	    print 406,kF
		write(7,406),kF
c406		format(
c     &   ' Matrix inversion bad in solving rW=0: k = ',i3)
		 nerr=82		!to signal replacement if theta on return to HJCLIK
		 goto 999		!deallocate and RETURN
	 endif

	   do i=1,kF
		colvecF(i,m)=pdum(i)	!copy to column m of colvec
	   enddo
	enddo
c===c Get right (column) eigenvectors in colvecF by inversion
c===  call MATINV(rowvecF,kF,km,colvecF,km)
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
c Jump to here if kF=1 (with ampF(1) set too rootF(1) so area=1)
c14    continue (not used now)
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
c next iteration, and ditto for eigAAsav
	nevlast=neval
	do m=1,kA
	   eigAAsav(m,jset)=eigAA(m)
	enddo
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


