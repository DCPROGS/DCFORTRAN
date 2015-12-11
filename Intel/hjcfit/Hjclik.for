c FILE NAME-was HJCLIK1.FOR but now renamed HJCLIK.FOR

	real*8 function HJCLIK(iasympPanel,kfit,theta,
     & tint,ampl,iprops,nd1,nd2,Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & XAF,XFA,QexpQA,QexpQF,alpha2,beta2,aloglik,kab,kAm,kFm,km,
     & irt,npar,IQf,irate1,jrate1,nlig,EXPQF,EXPQA,qt,qd)

c	real*8 function HJCLIK(kfit,theta,
c     & tint,ampl,iprops,nd1,nd2,kn)
c At earlier stage following were parameters but now in common /detw1 and /detw3
c QAF,QFA,QAA,QFF,EXPQF,EXPQA,WA,WF,
c
c Version with data as parameters (called from SIMPHJC)
c	subroutine HJCLIK(k,theta,hlik)
c To calculate likelihood for a series of open and shut times using HJC missed
c events pdfs (1st 2 dead times exact, then asymptotic).
c Converted to Lahey V5.n 07/09/93 03:00pm (from HJCLIK1 version for old Lahey)
c
c Modified 08/07/03 09:17pm so make rootAsav(*,jset), rootFsav(*,jset), jset
c arguments of HJCASYMP, because, at previously, when 'previous root' was used
c in hjcasymp, the root from the last iteration was taken from the wrong set
c when nset>1.
c Similarly the SAVEd array eigFFsav, must also now have jset as
c index.  Also setting of guesses for open states now made the same as for
c shut states (apart from 'grouped' option which is not avalaible for ope
c states -this requires definition of eigAAsav.
c
c Modified 06/04/02 05:02pm for version where tint() etc contains open periods
c alternating with shut times already (done in call to GETOPER)
c (option to remove shut times bordered on both sides by specified shut is
c already implement in getoper, before getting here)
c
c Modified 03/01/02 08:33am so that when parameters are reset to 'previous
c best', in thmin, and random perturbation is added (see ranpert.for) to
c prevent looping
c
c Modif 02/28/02 05:10pm by adding common/ires/ireset where ireset =0
c to reset neg paramters to 1.d-15, and ireset=1 to set to thmin
c
c Modif 01/28/02 08:21am so that negative rate constants reset to previous
c best value, not to 'zero'
c
c Modif 01/16/02 12:34pm to fit log of rate constants (so steps in simplex
c can be geometrically spaced). If logfit=true then theta(), called from
c SIMPHJC contains log(rate constant) so must take antilog here, before
c using values, and take log again before returning.
c
c Modified 01/14/02 04:51pm by adding line 'if (nodata) RETURN' so that
c HJCLIK can be used to calculate onlt the distributions for specified theta().
c using value of NODATA in common
c
c MAJOR MODIF 03/28/01 06:05pm TO TAKE UP TO 100 STATES/10 LIGANDS
c ARRAY SIZES: NB real*8 (100x100x100) is 8 Mb, so make restriction
c of up to 10 open states, and make, for example
c instead of original
c	real*8 XAF(10,10,10),XFA(10,10,10)
c	real*8 QEXPQA(10,10),QEXPQF(10,10)
c now have
c	real*8 XAF(10,100,10),XFA(100,10,100)
c	real*8 QEXPQA(100,10),QEXPQF(10,100)
c	real*8 XAF(kAmax,kFmax,kAmax),XFA(kFmax,kAmax,kFmax)
c	real*8 QEXPQA(100,10),QEXPQF(10,100)
c RULE: keep most (all?) 1D arrays, and also QT and QD, as fixed
c size, kdq=100, and leave in commons when possible.  All 2D and 3D arrays
c (except QT and QD) now allocated in main porog, and so must be passed
c as arguments
c Keep commons /detw1 and /detw3 so make QAF etc fixed size
c They are used in hjclik, hjcasymp, detwa, detwf and (/detw1/ only) darsds
c  Need values of kAm,kFm,km
c NB need Z00a etc in call to hjclik  now for transmission to hjcdisp, where
c they are needed for calls to mopadj etc
c HJCLIK used in:
c hjcfit,
c hjcdisp,
c vmat_hjc,
c simphjc
c HJCASYMP is called only from HJCLIK
c
c Modif 01/31/03 08:50am so set grouped=false after label 100 (so it is still
c  set true for each data set, the first time through)
c
c Modif 03/21/00 08:10am Add common/bad/badlik, to signal invalid
c   calc of likelihood (for error calcs)
c Modif 03/20/00 05:42am Convert theta, simplex hjclik etc to real*8
c
c Modif 10/12/92 09:18am by addition of common/reset/ here, and in SIMPLEX
c   so that simplex can be restarted at previous best values (absmin, thmin)
c   from here; at present does this if QAA or QFF give silly eigenvalues in
c   HJCASYMP (at present silly = abs value of smallest below 1.d9 or abs
c   value of largest above 1.d9)
c Modif 10/09/92 03:00pm so all rates constrained > 1.e-9 (prev 1.e-20)
c   i.e. tau<1.e9 sec (32 years)
c Modif 10/08/92 09:02am by adding common/lcomp/ so lik for each set can
c   be printed in simplex
c Modif 10/07/92 07:14pm so set first=false after label 100 (so it is still
c  set true for each data set, the first time through)

c 10/01/92 10:55am Version of HJCLIK for fitting several sets (e.g. conc)
c   simultaneously (with oneset=false normally).  Also used to calculate
c   values for plots in HJCDISP -in this case set ONESET=true, so calcs
c   done for the set to be plotted only (=#iset, in common/setblk/).
c   Note that some of the calculations needed for plots are done here only
c   when idebug=8, as used for print of the final pdfs.  Modified now so that
c   these calcs are done also if ONESET=true (but pdfs not printed again)
c
c    Lik=phi*eGAF(t1)*eGFA(t2)*eGAF(t3)*.....*eGAF(tn)*uF
c where t1,t3,...,tn are open times; t2,t4,...,t(n-1) are shut times.
c Gaps > tcrit are treated as unusable (eg contain double or bad bit of record,
c or desens gaps that are not in the model, or gaps so long that next opening
c may not be from the same channel). However this calculation DOES assume that
c all the shut times predicted by the model are present within each group, so
c ideally have only one group (presence of bad gaps etc will inevitably cause
c some bias). The series of multiplied likelihoods is terminated at the
c end of the opening before such a gap (multiplied by uF to give Lik(1) say.
c A new series is then started, using appropriate initial vector
c (same equilib phiA at present) to give Lik(2),.... At end these are
c multiplied to give final likelihood.
c
c BURSTS. Modified 02/14/92 12:19pm so that when chsvec=true we calculate
c    Lik=phibst*eGAF(t1)*eGFA(t2)*eGAF(t3)*.....*eGAF(tn)*endbst
c where phibst (1xkA) and endbst (kFx1) are the start and end of burst vectors
c suggested by AGH
c
c BAD SHUTTINGS or openings: If NOT burst calcs then end the group, and
c restart a new group at the next good opening (must cause some bias as
c length of missed bit not known).  For burst=true calcs the whole burst
c that contains the bad interval must be ignored, ie overwrite the part
c of the burst already calculated and look forward for the next good gap>tcrit
c and restart the burst at the opening following it, ie go on to next burst.
c
c	SCALING: to prevent likelihood for a group overflowing (esp if it is
c a long group) the current rowA() is checked, and if max element getting too
c big (<1.d50) is multiplied by 1.d-100.  Scale factors done separately
c in EVERY iteration (if fit is good, so likelihood bigger, may need more
c scaling). The number, nscal, of factors of 1.d-100 that are used is counted.
c  At the end, after logs taken, add 100*nscal to the log10(likelihood) so
c each iteration gives the unscaled log-likelihood. Actually ln(likelihood)
c used so add 230.2585093*nscal, where 230.258..=ln(1.e100)
c	NB fitted values are all rate constants of the model, and so must
c all be constrained to be positive
c
c FURTHER NOTES ON METHOD
c (1) Likelihood calc as phiA*eGAF(t1)*eGFA(t2)*eGAF(t3)*...*eGAF(tn)*uF for
c	a group of open and shut times (t1,t2,t3,...,tn) that start and end
c	with an opening
c (2) Likelihood is calculated for a series ('group') of open and shut times,
c	all of which are supposed to come from the same individual channel.
c	The group is ended (a) when a shut time > tcrit is encountered or
c	(b) when a bad opening or gap is encountered, and the product summed
c	over states by multiplying by uA (or uF) to give OLIK(n), the likelihood
c	for the nth group.  Then a new group is started at the next (good) opening,
c	using, at present, the normal equilibrium initial vector (though may be
c	possible to use better initial vector eg if we 'know' that next opening
c	occurs after emerging from desensitized state(s).  At the end all
c	the OLIK(n) are multiplied to give the overall likelihood.
c (3) After a bad gap, look for the next opening (so if, in error, there are
c	several gaps in succession, it should not matter)
c (4) If there are two or more contiguous shut times (in error) then treat
c	as unusable, and look for next opening
c (5) Unusable open times should not occur (fix in new SCAN!), but if they
c	do, end the group and start again at the next (good) opening
c (6) Contiguous open times are added and the whole open period used for
c	likelihood calcs
c
	use menu_f90
	!PARAMETER kAx=60
	PARAMETER kAx=20 	!max number of open states fir following
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      allocatable :: olik,nopen
c=	real*4 theta(20)
	real*8 theta(200)
c
	real*8 eigAAsav(kAx,10),eigFFsav(100,10)
	common/eigsav/eigAAsav,eigFFsav
c
	real*8 absmin,thmin(200),step(200)
	real*4 tcrit(10)
	logical burst(10),chsvec(10),badend(10)
	real*8 tres,sum,amax,fmax,tcrit1,one
	real*8 t1,t2,ex1(100),ex2(100),EG(100,100)	!for calc of eGAF(t) etc
c Declare allocated arrays
	real*4 tint(nd1,nd2),ampl(nd1,nd2)
	integer*1 iprops(nd1,nd2)
c=c Declare fixed dimension arrays (for use in common/hjcblk/ and /ampblk/)
	integer nint(10)
	real*4 conc(10,10)		!to hold concentration ith ligand, jth set
	logical open,bad,bad1,good,first,oneset,pcalc
	logical btest
	integer*2 nopen(:)	!number of openings per group
	real*8 OLIK(:)	!likelihood for group #ng
	real*8 setlik(10)		!likelihood for each data set (e.g.conc)
c for PHIo1,pdfopen etc (zero resolution calcs for printing at end)
	real*8 Peq1(100),phio(1,100)
	real*4 tau1(100),area(100),amo,ams,sd
	real*8 hmopen,hmshut
cc for ABORTW
c	integer*2 ktype
c	logical KBHIT		!Utility lib
c	character ch,getch
	logical ABORTW
	common/abt/ABORTW
c For setting QD
	real*8 QT(100,100),QD(100,100)	!QD has conc in
	integer jfix(100)
	integer IQf(100,100),irate1(200),jrate1(200)
c==	common/QDBLK/QT,QD,npar,IQf,irate1,jrate1,nlig
!	common/QDBLK1/QT,QD
!	common/QDBLK2/npar,IQf,irate1,jrate1,nlig   !hjclik, qset_hjc only
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200)
	integer NSC(50),IM(50,100),JM(50,100),IX(100),JX(100)
	INTEGER AA,AF,FA,FF

	real*8 Z00A(kAm,kFm,km),Z10A(kAm,kFm,km),Z11A(kAm,kFm,km)
	real*8 Z00F(kFm,kAm,km),Z10F(kFm,kAm,km),Z11F(kFm,kAm,km)
c	common/qexpblk/QEXPQA,QEXPQF		!now parameter
	real*8 QEXPQA(kFm,kAM),QEXPQF(kAm,kFm)
	real*8 XAF(kAm,kFm,kAm),XFA(kFm,kAm,kFm)
!	real*8 QAF(20,100),QFA(100,20),QAA(20,20),QFF(100,100)
	real*8 qaf,qaa,qfa,qff
	allocatable qaf(:,:),qaa(:,:),qfa(:,:),qff(:,:)
	
	real*8 EXPQA(20,20),EXPQF(100,100)
	real*8 WA(kAx,kAx),WF(100,100)
c	common/detw1/QAA,QAF,QFF,QFA,EXPQF,EXPQA   !for DETWA,DETWF,dARSdS -now param
	common/detw3/WA,WF			  !for DETWA,DETWF -now params
c to store alpha, beta to allow check on correlation
c	common/absave/nab,alpha2,jalpha,beta2,jbeta,aloglik	!in hjclik, hjcdisp
  	real*8 alpha2(kab),beta2(kab),aloglik(kab)
	common/absave/nab,jalpha,jbeta		!in hjclik, hjcdisp
c Fixed size arrays (local or common)
	real*8 eigen(100),eig1
	real*8 g00A(100),g10A(100),g11A(100),g00F(100),g10F(100),g11F(100)
	real*4 w(100)
	real*8 Q1(100,100)
	real*8 rowA(100),rowF(100)
	real*8 phiA(1,100),phiF(1,100)
	real*8 phibst(1,100),endbst(100,1)
	real*8 ucol(100,1)
	real*8 rootA(100),rootF(100)
c	real*8 s1A(kAm),s2A(kAm),s1F(kFm),s2F(kFm)	!init guesses for roots
	real*8 s1A(100),s2A(100),s1F(100),s2F(100)	!init guesses for roots
c Keep roots and init guesses for them separately for each data set so
c make arrays 2D rootA(i,j)= ith root for data set j
c	real*8 rootAsav(kAm,10),rootFsav(kFm,10)
c	real*8 s1Asav(kAm,10),s2Asav(kAm,10),s1Fsav(kFm,10),s2Fsav(kFm,10)
	real*8 rootAsav(kAx,10),rootFsav(100,10)
	real*8 s1Asav(kAx,10),s2Asav(kAx,10),s1Fsav(100,10),s2Fsav(100,10)
!	common/detwmod/
c Array sizes
c Z00A(kA,kF,k)
c Z00F(kF,kA,k)
c Z10A(kA,kF,k)
c Z10A(kA,kF,k)
c Z11A(kA,kF,k),
c Z11F(kF,kA,k)
c
c D(kA,kA,k)
c D(kF,kF,k)	!later
c Ci10(kA,kA,k)
c Ci10(kF,kF,k)	!later
c Ci11(kA,kA,k)
c Ci11(kF,kF,k)	!later
c Ci00(kF,kF,k)	!used?
c
	REAL*8 SCAL
	logical caplock,deb,discprt
	real*8 dexp1
	real*4 an,sum1
	real*8 ampA(100),ampF(100)
c  In arrays below, dimension=10 refers to i=1,2,...,nset
c   -now make dimension nsmax? But several arrays with this dimension
c    are in common so not easy to make max number of sets adjustable
c
	real*8 tresd(10)
cc	real*8 tresd(nsmax)
	common/resblk/tresd	!for hjclik
	logical badlik
	common/bad/badlik		!for vmat_hjc/hjclik
	logical debprt,dprt
	
	common/dp/discprt,deb
	common/reset/absmin,thmin,step			!from SIMPLEX
	common/nblk/ngp(10),an(10),nscal(10),first    !ngp(j) etc for set j
	COMMON/HJCBLK/Nint,tcrit,burst,chsvec,badend	!for HJCLIK/hjcdisp
	real*8 fcomp(10)
	common/lcomp/nset,fcomp				!for SIMPLEX
	common/setblk/oneset,iset,itogrec	!for HJCLIK,DISP to specify one set
	common/CBLK/nset1,conc,jsetlast		!for HJCLIK
	COMMON/MPAR/NCYC,NSC,IM,JM
	common/cpar/ncdep,IX,JX,x
	
	
	common/LIG/nligsav,IL(100)
	common/KBLK/kA,kB,kC,kD
	common/FIXBLK/nfix,jfix
	common/root/rootA,rootF
	common/inroot/s1A,s2A,s1F,s2F	  !for init guesses for roots
	common/rootsav/rootAsav,rootFsav
	common/inroots/s1Asav,s2Asav,s1Fsav,s2Fsav	  !for init guesses for roots
	common/fitblk/eigen,g00A,g10A,g11A,g00F,g10F,g11F	!for HJCDISP,FCRTQ
c COMMON detw1,2 needed for HJCASYMP
	common/detw2/tres,km1,nerr			!for DETWA,DETWF (km1=km)
	common/phiblk/phiF,phiA
	common/fitblk1/ampA,ampF	
	common/sets/setlik,nset2		!for dspout in simphjc
	logical penalty
	real*8 penfunc,penfac,pomax,pmaxcalc
	logical fixpmax
	common/pen/penalty,penfunc,penfac,fixpmax,pomax,pmaxcalc 
c For max assoc rate
	real*8 assmax,ratemax,xqlo,xqhi
	common/amax/assmax,icdep(200),ratemax		!for hjclik
c For case where there is no data
	logical nodata
c	common/iniset/nodata
	common/iniset/nodata,nofit,autosim,curvonly
c For fitting log (rate constant)
	logical logfit,stable
	common/logf/logfit
	logical fixec50
	real*8 ec501
	common/ec/fixec50,nmod9,ec501,i50,j50,m50,prtec50,
     &	xqlo,xqhi,dcmodel
c Recent additions
	common/ires/ireset
	common/deb/idebug,idebug1
	real*8 u2,drandom,perfac
	common/pert/ perfac
	common/tty/ittypanel,itty
		real*8 det
	COMMON/determ/det			!for matinv
	common/perr/nerr2
c
	character*11 cstring,cstring1
	character string*256
	real*8 rcrit,gfac1,gfac2
	common/stab/stable,nstab1,gfac1,nstab2,gfac2,istab	!for hjcfit
	common/det4/ndscale,idsign

c Define functions
c	debug()=caplock()
c
c***	call UNDER0('true')	!so underflows set=0 (Lahey manual p12.8)
!	kax=60
!	kAx=20

      badlik=.false.
c	irt=irt+1
	if(nset.eq.0) nset=nset2
	nset1=nset
	
	
	do i=1,10
		an(i)=0.
	enddo
	
c	30/11/2006	
	deb=.false.
	allocate(QAF(20,100),QFA(100,20),QAA(20,20),QFF(100,100))
	
c	real*8 Z00A(kAm,kFm,km),Z10A(kAm,kFm,km),Z11A(kAm,kFm,km)
c	real*8 Z00F(kFm,kAm,km),Z10F(kFm,kAm,km),Z11F(kFm,kAm,km)
	goto 222
	do i=1,kam
		do j=1,kfm
		do l=1,km
			Z00A(i,j,l)=0.
			Z10A(i,j,l)=0.
			Z11A(i,j,l)=0.
		
			Z00f(j,i,l)=0.
			Z10f(j,i,l)=0.
			Z11f(j,i,l)=0.
	
		enddo
		do l=1,kam
		    xaf(i,j,l)=0.
		enddo
		do l=1,kfm
			xfa(j,i,l)=0.
		enddo
	enddo
	enddo

c	real*8 XAF(kAm,kFm,kAm),XFA(kFm,kAm,kFm)
c	real*8 QAF(60,100),QFA(100,60),QAA(60,60),QFF(100,100)
c	real*8 EXPQA(60,60),EXPQF(100,100)
!	ikey=GetAsyncKeyState(27)
c Use real (not log) rates THROUHOUT hjclik -if logfit=T then take logs again
c before leaving  hjclik for simplex (NB must have logfit=T ONLY during simplex
c not for any other calls to hjclik)
c	real*8 QAF(60,100),QFA(100,60),QAA(60,60),QFF(100,100)
	do i=1,kAx
		do j=1,100
			qaf(i,j)=0.
			qfa(j,i)=0.
		enddo
		do j=1,kAx
			qaa(i,j)=0.
	    enddo
	enddo
	do i=1,100
	do j=1,100
		qff(i,j)=0.
	enddo
	enddo
222	if(logfit) then
	   do i=1,kfit
		theta(i)=dexp(theta(i))	!non-log rate
		thmin(i)=dexp(thmin(i))	!non-log rate
	   enddo
	endif
c

	allocate(olik(20480),nopen(20480))
	do i=1,20480
		nopen(i)=0
		olik(i)=0.d0
	enddo
	kF=kB+kC
	k=kA+kF
	nset1=nset		!for COMMON/LCOMP/
	nset2=nset		!for COMMON/sets/
	km1=km		!for common/detw2
	dprt=discprt.and.debprt
	if(idebug.eq.12) dprt=.true.
c
c
	do i=1,kfit
	   if(theta(i).lt.1.d-15) then
		if(ireset.eq.0) then
cp		   print 5,i,theta(i)
5		   format(' theta(',i2,') = ',g13.6,
     &		' reset to 1.e-15 in HJCLIK')
		   theta(i)=1.d-15
		else
cp		   print 502,i,theta(i)
502		   format(
     &     ' theta(',i2,') = ',g13.6,' reset to = ')
c===		   theta(i)=thmin(i)
c            Reset only theta(i), not the other theta(), as in RANPERT
		   u2=drandom()			!uniform(0,1)
		   theta(i)=thmin(i)*(u2*2.d0*perfac + (1.d0-perfac))
cp		   print 503,theta(i)
503		   format('&',g13.6)
		endif
	   endif
	   if(icdep(i).eq.1) then
		if(theta(i).gt.assmax) then
cp		print 501,i,theta(i),assmax
		 write(string,fmt='(a20,i4,a14,g13.6)')
     &	'Assoc rate theta(',i,') reset to=',assmax
    
		   CALL GMSETTEXTSETTING(ITTY,string)
501		format(' Assoc rate theta(',i2,') = ',g13.6,' reset to ',
     &	   g13.6,' in HJCLIK')
		theta(i)=assmax
		endif
		else		!not conc dep
		if(theta(i).gt.ratemax) then
cp		print 505,i,theta(i),ratemax
505		format(' Rate theta(',i2,') = ',g13.6,' reset to ',
     &	   g13.6,' in HJCLIK')
	 write(string,fmt='(a20,i4,a14,g13.6)')
     &	'rate theta(',i,') reset to=',ratemax
    
		   CALL GMSETTEXTSETTING(ITTY,string)
		theta(i)=ratemax
		endif
	   endif
	enddo
c
c Start outer loop for each set here
	jset1=1		!normally
	jset2=nset		!normally
	if(oneset) then	!calcs for set #iset only (e.g. for plots)
	   jset1=iset
	   jset2=iset
	endif
	if(idebug.eq.12) then
	   noper=0		!no of open periods in current set, for debug
	endif
c
c START LOOP OVER DATA SETS (and first set tres for current set)
c	do 100 jset=1,nset


c*********************************

	do 100 jset=jset1,jset2
c

	tres=tresd(jset)
c
c Print titles (if idebug=8 to get print-out)
	if(idebug.eq.8) then
cp        print 26,jset
        if(discprt) write(7,26) jset
26      format(/,
     &' Distributions for set number ',i3,
     &/,' ------------------------------------------------------------')
	  if(ncdep.gt.0) then
	   do i=1,nlig
cp	     print 267,i,conc(i,jset)*1.e6
	     if(discprt) write(7,267) i,conc(i,jset)*1.e6
267	     format(
     &  '    Concentration of ligand',i2,' (micromolar) = ',g13.6)
	   enddo
	  endif
	endif
c  First restore roots and guesses for current set
	do i=1,kA
	   rootA(i)=rootAsav(i,jset)
	   s1A(i)=s1Asav(i,jset)
	   s2A(i)=s2Asav(i,jset)
	enddo
103	continue
	do i=1,kF
	   rootF(i)=rootFsav(i,jset)
	   s1F(i)=s1Fsav(i,jset)
	   s2F(i)=s2Fsav(i,jset)
	enddo
c Make a Q matrix from the current set of parameters.  Make it from QT by
c inserting concentrations etc, and put values to be used into QD()
c  Fixed parameters are already in QT (QD, QT are no longer in common/qdblk/)
	call QSET_HJC(iasympPanel,jset,theta,QT,QD,kfit,k,irt,npar,IQf,
     & irate1,jrate1,nlig,itogrec)
c
	if(idebug.eq.7) then
cp	  print 109,jset
	  if(discprt) write(7,109) jset
109	  format(' Set number ',i3)
c	  call ATYPD(QD,'new QD  ',k,k,km,km)
	endif
c
c Initialise for observations here (in case jump out of HJCLIK because
c of error in HJCASYMP etc
	in=1		!counter for intervals
	ng=0			!to count number of groups
	nopen(1)=0		!number of openings in group #ng
	OLIK(1)=0.d0	!initialise ready for next group
	nscal(jset)=0		!index for scale factors
	nbo=0
	nbg=0
c
c Q MATRIX IS NOW SET -USE IT TO GET COEFFICIENTS FOR THE PDFs
c
c Calculate all the constants for the present set of parameters
c  First initial vector (and some things needed for other calcs)
c
	AA=11
	AF=15		!5=CODE FOR F(=B+C) SECTION
	FA=51
	FF=55
	call SUBMAT(QD,AA,QAA,km,km,kAx,kAx)		!QAA in QAA etc
	call SUBMAT(QD,AF,QAF,km,km,kAx,km)		!QAF
	call SUBMAT(QD,FA,QFA,km,km,km,kAx)		!QFA
	call SUBMAT(QD,FF,QFF,km,km,km,km)		!QFF
c=	if(debug()) then
	one=1.0d0
	do i=1,km
	   ucol(i,1)=1.d0
	enddo
c=	endif
c
c Get open and shut ASYMPTOTIC
c (NB several param are in common/detw1/km,QAA,QAF,QFF,QFA,EXPQF,EXPQA,
c WA,WF,tres (for DETWA,DETWF))
c Modified 08/07/03 09:17pm so make rootsav(*,jset) an argument because at
c present, when 'previous root' is used in hjcasymp, at present the root from
c the last iteration was taken from the wrong set when nset>1
cccccccccccccccccccccc
	nerr=0
	if(deb) write(7,1141)
1141	format(' before HJCASYMP')
	call HJCASYMP(iasympPanel,QD,k,kA,kF,phiA,phiF,QEXPQA,QEXPQf,XAF,
     & XFA,
     & rootA,rootF,rootAsav,rootFsav,jset,
     & s1A,s2A,s1F,s2F,first,kAm,kFm,irt,QAA,QAF,QFF,QFA,EXPQF,EXPQA)
		if(deb) write(7,1142)
1142	format(' after HJCASYMP')
	if(ABORTW) then
	   goto 999		!returns to SIMPLEX during fitting
	endif
	if(nerr.eq.7.or.nerr.eq.8.or.nerr.eq.81.or.nerr.eq.82) then	!replace theta with prev best value
	   badlik=.true.		!signal bad value for error calcs
cp	   print 6,nerr
		 CALL GMSETTEXTSETTING(ITTY,'Error in HJCASYMP: all theta reset')

	   if(discprt) write(7,6) nerr
6	   format(' Error ',i2,
     & ' in HJCASYMP: all theta reset')
cp	   print 53,(theta(i),i=1,kfit)
	   if(discprt) write(7,53) (theta(i),i=1,kfit)
53	   format(' theta = ',/,20(5g13.6,/))
	   call RANPERT(theta,thmin,kfit,perfac)
cp	   print 531,100.d0*perfac,(theta(i),i=1,kfit)
	   if(discprt) write(7,531) 100.d0*perfac,(theta(i),i=1,kfit)
531	   format(
     &	' thmin + random perturbation within +/- ',f7.2,'% = ',/,
     &	20(5g13.6,/))
c	   do i=1,kfit		!now use ranpert
c		theta(i)=thmin(i)
c	   enddo
	   HJCLIK=absmin
	   deallocate(olik,nopen)
	   if(logfit) then
		do i=1,kfit
		   theta(i)=dlog(theta(i))	!restore log before returning
		   thmin(i)=dlog(thmin(i))	!restore log before returning
		enddo
	   endif
	deallocate(QAF,QFA,QAA,QFF)
	
	
	   RETURN
	else if(nerr.ne.0) then
cp	   print 1502,nerr
	
	CALL GMSETTEXTSETTING(ITTY,
     & 'Error in HJCASYMP. Likelihood set to 0; try new params')
     
	   if(discprt) write(7,1502) nerr
1502	   format(
     & ' Error ',i2,' in HJCASYMP. Likelihood set to 0; try new params')
c	   if(dprt) call DATYP(QD,'Q matrix',.false.,k,k,km,km)
	   HJCLIK=0.0
	   deallocate(olik,nopen)
	   badlik=.true.
	   if(logfit) then
		do i=1,kfit
		   theta(i)=dlog(theta(i))	!restore log before returning
		   thmin(i)=dlog(thmin(i))	!restore log before returning
		enddo
	   endif
	deallocate(QAF,QFA,QAA,QFF)
	
	   RETURN
	endif
c Calculate initial and final burst vectors if needed
c   get in Q1=SUM[XFA(m)*(-1/rootF(m))*exp((tcrit-tres)*rootF(m))]  (kF*kA)
	if(chsvec(jset)) then
	   tcrit1=dble(tcrit(jset))*1.0d-3		!in seconds
	   do m=1,kF
		ex1(m)=dexp1(rootF(m)*(tcrit1-tres))	!NB rootF() already negative
	   enddo
	   do i=1,kF
		do j=1,kA
		   Q1(i,j)=0.0d0
		   do m=1,kF
			Q1(i,j)=Q1(i,j) + (XFA(i,j,m)*ex1(m))/(-rootF(m))
		   enddo
		enddo
	   enddo
c
c  Calc endbst()
         call MATMUL(Q1,ucol,endbst,kF,kA,1,one,km,km,km,1,km,1)
c To get phibst() want row of Q1 scaled to unit sum (should be similar for
c all rows -test this)
	   do i=1,kF	!rows
		sum=0.d0
		do j=1,kA	!row sum (for row #i)
		   sum=sum + Q1(i,j)
		enddo
		do j=1,kA	!normalise row
		   phibst(1,j)=Q1(i,j)/sum
		enddo
	   enddo
	   if(first.or.idebug.eq.8.and.iasymppanel.ne.-2) then
            if(discprt) write(7,79) jset,(phibst(1,j),j=1,kA)
79	      format(' Set ',i2,': Initial CHS vector for burst = ',20g13.6)
		  if(first) then
			CALL GMSETTEXTSETTING(ITTY,' Initial CHS vector for burst')
			write(string,fmt='(10g13.6)') (phibst(1,j),j=1,kA)
			CALL GMSETTEXTSETTING(ITTY,string)
			endif
	   endif
	   if(first.or.idebug.eq.8.and.iasymppanel.ne.-2) then
      	if(discprt) write(7,81) jset,(endbst(i,1),i=1,kF)
81		format('set ',i2,': End CHS vector for burst = ',/,8g13.6)
		if(first) then
			CALL GMSETTEXTSETTING(ITTY,' End CHS vector for burst')
			write(string,fmt='(10g13.6)') (endbst(j,1),j=1,kF)
			CALL GMSETTEXTSETTING(ITTY,string)
		endif
	   endif
	endif
c Insert debug to calc asymp areas etc as test -amplitudes done in HJCASYMP
	if(idebug.eq.7.or.idebug.eq.8) then
cp	   print 140,(rootA(j),j=1,kA)
	   if(discprt) write(7,140) (rootA(j),j=1,kA)
140	   format(' Open time roots (1/sec) = ',5g13.6,/,4(5g13.6))
cp	   print 141,(rootF(j),j=1,kF)
	   if(discprt) write(7,141) (rootF(j),j=1,kF)
141	   format(' shut time roots (1/sec) = ',5g13.6,/,4(5g13.6))
	endif
c
	if(deb) write(7,1144)
1144	format(' before HJCEXACT')
	call HJCEXACT(QD,k,kA,kF,EXPQF,QEXPQF,EXPQA,QEXPQA,QAF,QFA,
     & Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,eigen,kAm,kFm,kAx,ibad,eig1,irt)
	if(deb) write(7,1145)
1145	format(' after HJCEXACT')
	if(ibad.ne.0) then
cp	   print 1501
	CALL GMSETTEXTSETTING(ITTY,	
     & 'Error in HJCEXACT. Likelihood set to 0; try new params')
     
1501	   format(
     & ' Error in HJCEXACT. Likelihood set to 0; try new params')
	   HJCLIK=0.0
	   deallocate(olik,nopen)
	   badlik=.true.
	   if(logfit) then
		do i=1,kfit
		   theta(i)=dlog(theta(i))	!restore log before returning
		   thmin(i)=dlog(thmin(i))	!restore log before returning
		enddo
	   endif
	deallocate(QAF,QFA,QAA,QFF)
	
	   RETURN
	endif
c  Insert debug to calc g00,g10,g11 from Znn as test
c Also used as final calculations for display in HJCDISP so done
c when ONESET=true
c	if(debug()) then
	pcalc=idebug.eq.2.or.idebug.eq.6.or.idebug.eq.8.or.idebug.eq.9
!!	pcalc=.true.
!	if(first) pcalc=.true.
	if(oneset.or.pcalc) then
	   call RAC3d(phiA,ucol,Z00A,1,kA,1,kF,k,w,g00A,
     &	km, km, kAm,kFm,km, km)
	   call RAC3d(phiA,ucol,Z10A,1,kA,1,kF,k,w,g10A,
     &	km, km, kAm,kFm,km, km)
	   call RAC3d(phiA,ucol,Z11A,1,kA,1,kF,k,w,g11A,
     &	km, km, kAm,kFm,km, km)
	endif
	
	if(pcalc) then		!print them
c
cp         print 150
         if(discprt) write(7,150)
150	   format(/,' EXACT SOLUTIONS FOR OPEN TIMES')
c	In HJCEXACT, if eigen(1) > threshold 1.d-15 it is set to 0 exactly
c	   if(dabs(eig1).gt.1.d-15) then
c		eig1=eigen(1)	!for printout
c		eigen(1)=0.0d0
c	   endif
c*************************
	   if(dabs(eig1).gt.1.d-15) then	!threshold 1.d-15 set in HJCEXACT
cp		call BELL(1)
cp		print 154,-eig1		!NB eigen contains eigs of -Q
      	if(discprt) write(7,154) -eig1
154		format(' ******',
     &	' In HJCEXACT, eigen(1) of Q reset from ',g13.6,' to 0',/)
	   endif
cccccccccccccccccccccc
cp         print 1511,(eigen(m),m=1,k)
         if(discprt) write(7,1511)(eigen(m),m=1,k)
1511	   format(' eigen = ',6g13.6,/,10(7g13.6))
cp        print 151,(g00A(m),m=1,k)
         if(discprt) write(7,151)(g00A(m),m=1,k)
151	   format(' g00(m) = ',6g13.6,/,10(7g13.6))
cp         print 152,(g10A(m),m=1,k)
         if(discprt) write(7,152)(g10A(m),m=1,k)
152	   format(' g10(m) = ',6g13.6,/,10(7g13.6))
cp         print 153,(g11A(m),m=1,k)
         if(discprt) write(7,153)(g11A(m),m=1,k)
153	   format(' g11(m) = ',6g13.6,/,10(7g13.6))
	endif
	if(oneset.or.pcalc) then
	   call RAC3d(phiF,ucol,Z00F,1,kF,1,kA,k,w,g00F,
     &	km, km, kFm,kAm,km, km)
	   call RAC3d(phiF,ucol,Z10F,1,kF,1,kA,k,w,g10F,
     &	km, km, kFm,kAm,km, km)
	   call RAC3d(phiF,ucol,Z11F,1,kF,1,kA,k,w,g11F,
     &	km, km, kFm,kAm,km, km)
	endif
	if(pcalc) then		!print them
cp         print 149
         if(discprt) write(7,149)
149	   format(/,' EXACT SOLUTIONS FOR SHUT TIMES')
cp         print 151,(g00F(m),m=1,k)
         if(discprt) write(7,151)(g00F(m),m=1,k)
cp         print 152,(g10F(m),m=1,k)
         if(discprt) write(7,152)(g10F(m),m=1,k)
cp         print 153,(g11F(m),m=1,k)
         if(discprt) write(7,153)(g11F(m),m=1,k)
c insert here print of ideal distributions too (08/03/04 12:50pm)
	   call EQOC_HJC(QD,peq1,k,km,km)
	   call PHIo1(QD,Peq1,phio,km)		!calc init vector
c
         nckf=kf
	   call PDFshut(QD,phio,area,tau1,nckF,km)
	   call PDFOUTs('Shut time pdf with zero resolution',
     &	-1,-1,area,tau1,kF,ams,sd,km,.false.,.false.,discprt)
c
	   call PDFopen(QD,phio,area,tau1,kA,km)
c	   print 108
	   if(discprt) write(7,108)
	   call PDFOUTs('Open time pdf with zero resolution',
     &	-1,-1,area,tau1,kA,amo,sd,km,.false.,.false.,discprt)
c
         nckf=kf
	   call PDFshut(QD,phio,area,tau1,nckF,km)
c	   print 108
	   if(discprt) write(7,108)
	   call PDFOUTs('Shut time pdf with zero resolution',
     &	-1,-1,area,tau1,kF,ams,sd,km,.false.,.false.,discprt)
c Also calculate HJC exact mean open and shut time, and overall P(open)
	  if(deb) then
		call DATYP(QD,'QD =    ',.false.,k,k,km,km)
	   endif
	   call HJCMEAN(hmopen,hmshut,QD,tres,
     &   k,kA,kF,kAm,kFm,km)
c	   print 50,
c     &	amo,ams,amo/(amo+ams),
c     &	hmopen,hmshut,hmopen/(hmopen+hmshut)
	   if(discprt) write(7,50)
     &	amo,ams,amo/(amo+ams),
     &	hmopen,hmshut,hmopen/(hmopen+hmshut)
50	   format(/,
     &   ' Ideal (zero resolution)',/,
     &   ' Mean open time (ms) = ',g13.6,
     &   ' Mean shut time (ms) = ',g13.6,/,
     &   '   P(open) (ideal) = ',f11.5,/,/,
     &   ' HJC distributions (exact)',/,
     &   ' Mean open time (ms) = ',g13.6,
     &   ' Mean shut time (ms) = ',g13.6,/,
     &   '   P(open) (HJC) = ',f11.5,/)
	
	endif
c
c	tres1=tres			!tres in sec (real*8)
	tres2=2.*sngl(tres)
	tres3=3.*sngl(tres)
c
	if(nodata) then
	   if(logfit) then
		do i=1,kfit
		   theta(i)=dlog(theta(i))	!restore log before returning
		   thmin(i)=dlog(thmin(i))	!restore log before returning
		enddo
	   endif
	   if(jset.le.jset2) then
c		goto 100		!print next set
		goto 1001		!save roots and print next set
	   else
		deallocate(olik,nopen)
	deallocate(QAF,QFA,QAA,QFF)
	
		RETURN
	   endif
	endif
c
c	pause 'before likelihood calc'
c
c GO THROUGH OBSERVATIONS TO CALC LIKELIHOOD
c (1) Likelihood calc as phiA*eGAF(t1)*eGFA(t2)*eGAF(t3)*...*eGAF(tn)*uF for
c	a group of open and shut times (t1,t2,t3,...,tn) that start and end
c	with an opening
c (2) Likelihood is calculated for a series ('group') of open and shut times,
c	all of which are supposed to come from the same individual channel.
c	The group is ended (a) when a shut time > tcrit is encountered or
c	(b) when a bad opening or gap is encountered, and the product summed
c	over states by multiplying by uA (or uF) to give OLIK(n), the likelihood
c	for the nth group.  Then a new group is started at the next (good) opening,
c	using, at present, the normal equilibrium initial vector (though may be
c	possible to use better initial vector eg if we 'know' that next opening
c	occurs after emerging from desensitized state(s).  At the end all
c	the OLIK(n) are multiplied to give the overall likelihood.
c (3) After a bad gap, look for the next opening (so if, in error, there are
c	several gaps in succession, it should not matter)
c (4) If there are two or more contiguous shut times (in error) then treat
c	as unusable, and look for next opening
c (5) Unusable open times should not occur (fix in new SCAN!), but if they
c	do, end the group and start again at the next (good) opening
c (6) Contiguous open times are added and the whole open period used for
c	likelihood calcs
c
	idebugt=0		!for now
c	
c	if(idebugt.eq.1) then
c	   call TIMER(itick)
c	   itobs=itick	!time of entry start of obs
c	endif
	in=1		!counter for intervals
	ng=0			!to count number of groups
	nopen(1)=0		!number of open periods in group #ng
	OLIK(1)=0.d0	!initialise ready for next group
	nscal(jset)=0		!index for scale factors
	do j=1,kA
	   if(chsvec(jset)) then
		rowA(j)=phibst(1,j)		!start with phibst
	   else
		rowA(j)=phiA(1,j)		!start with phiA
	   endif
	enddo
c Start at first (good) opening
c NB Modified 06/04/02 05:02pm for version where tint() etc contains open periods
c alternating with shut times already (done in call to GETOPER). But shut times
c may still be set as bad

	call FINDOPEN(in,jset,iop,ibad,nbo,nbg,ampl,iprops,nd1,nd2)
	if(ibad.eq.-1) goto 99		!no good openings!
	in=iop			!make the opening the current obs
	!!!! ce e asta
	do i=1,20480
c?		nopen(i)=0
c?		OLIK(i)=0
	enddo
c
90	continue		!return here for next opening
c	deb=debug()
c	deb=.true.
c	deb=.false.
c GET LENGTH OF OPEN PERIOD-
	topen=tint(in,jset)
	open=ampl(in,jset).ne.0.
!	if(.not.open) imes=gmdisplaymessagebox('','Error 1 in hjclik1',
 !    & gexclamation,gok)
!!!	CALL GMSETTEXTSETTING(ITTY,'Error 1 in hjclik1')
	bad=BTEST(iprops(in,jset),3)	!tint(i) was unusable (bit 3='8' set)
	if(bad) then	!bad opening, so end group
	   if(burst(jset)) then
		if(badend(jset)) then
		   goto 91	!end group at end of prev shutting
		else
		   goto 93	!abandon whole burst & look for next
		endif
	   else
		goto 91	!end group at end of prev shutting
	   endif
	endif
	inext=in+1	!index of shutting that ends open period
c
c Get here if last opening reached (and it is good), but in this case
c a gap is not found and inext not upDATEWd. Therefore set inext=nint+1 to
c signal (below) that end reached
c===?	inext=nint(jset)+1
c===c
c===52	continue	!shutting found (index=inext), so jumped out
c Now have an open period, length=topen, so get likelihood term.
c EG(10,10) contains eGAF(t) (for open times) or eGFA(t) (for shut times)
c Calculate this, and then replace rowA(j) (1*kA) with
C rowF(j)=rowA(j)*eGAF(t) (1*kF)
c CALCULATE eGAF(t)
c   Count number of openings in current group (ng not upDATEWd until group ends)
	nopen(ng+1)=nopen(ng+1) + 1
c Put topen into seconds
	if(idebug.eq.12) then
	   noper=noper+1
cp	   print 20,noper,topen
	   if(discprt) write(7,20) noper,topen
20	   format(' ',i6,' open period = ',g13.6)
	endif
	topen=topen*1.e-3			!seconds
	if(topen.le.tres2) then		!exact -first dead time
	   t1=dble(topen)-tres		!sec
	   do m=1,k
		ex1(m)=dexp1(-eigen(m)*t1)
	   enddo
c
	   do i=1,kA
		do j=1,kF
		   EG(i,j)=0.d0
		   do m=1,k
			EG(i,j)=EG(i,j)+Z00A(i,j,m)*ex1(m)
		   enddo
		enddo
	   enddo
c
	else if(topen.gt.tres2.and.topen.le.tres3) then		!exact -2nd deadtime
	   t1=dble(topen)-tres		!sec
	   t2=t1-tres	    		!sec
	   do m=1,k
		ex1(m)=dexp1(-eigen(m)*t1)
		ex2(m)=dexp1(-eigen(m)*t2)
	   enddo
c
	   do i=1,kA
		do j=1,kF
		   sum=0.d0
		   do m=1,k
			sum=sum+Z00A(i,j,m)*ex1(m)
			sum=sum - (Z10A(i,j,m)+Z11A(i,j,m)*t2)*ex2(m)
		   enddo
		   EG(i,j)=sum
	   	enddo
	   enddo
c
	else			!asymptotic
	   t1=dble(topen)-tres		!sec
	   do m=1,kA
		ex1(m)=dexp1(rootA(m)*t1)		!NB rootA() already negative
	   enddo
c
	   do i=1,kA
		do j=1,kF
		   EG(i,j)=0.d0
		   do m=1,kA
			EG(i,j)=EG(i,j)+XAF(i,j,m)*ex1(m)
		   enddo
		enddo
	   enddo
	endif
c END OF eGAF(t) CALC
c
c CALCULATE NEW rowF()
c	amax=0.d0
c Can get overflow at 'rowF=' when, e.g.  ROWA(1)=-0.209743621444781D+308,
c and EG(1,1)=56.8492703741689  -surely rowa() should have been scaled
c down below!  Try inserting scale for rowF() also, here
	fmax=0.0d0
	do j=1,kF
	   rowF(j)=0.0d0
	   do n=1,kA
		rowF(j)=rowF(j)+rowA(n)*EG(n,j)
	   enddo
	   if(dabs(rowF(j)).gt.fmax) fmax=dabs(rowF(j))
	enddo
	if(fmax.gt.1.d50) then
	   do j=1,kF
		rowF(j)=rowF(j)*1.0d-100	!scale down
	   enddo
	   nscal(jset)=nscal(jset)+1
	endif
	if(idebug.eq.3.or.idebug.eq.5.or.idebug.eq.6) then
c	   print 108
c	   if(discprt) write(7,108)
	   call ATYPD(EG,' eGAF(t)',kA,kF,km,km)
	endif
	if(idebug.eq.4.or.idebug.eq.5.or.idebug.eq.6) then
cp	   print 801,in,ampl(in,jset),1.e3*topen,(rowA(j),j=1,kA)
	   if(discprt)write(7,801)in,ampl(in,jset),1.e3*topen,
     &	(rowA(j),j=1,kA)
801	   format(' i, amp(i), topen(ms) =',i8,2g13.6,/,' rowA = ',
     &	8g13.6,/,10(8g13.6))
cp	   print 8011,(rowF(j),j=1,kF)
	   if(discprt) write(7,8011) (rowF(j),j=1,kF)
8011	   format(' New rowF=rowA*eGAF(t) ',
     &	8g13.6,/,10(8g13.6))
	   sum=0.d0
	   do j=1,kF
		sum=sum+rowF(j)
	   enddo
	   if(sum.gt.1.d-100) then
cp	      print 809,(rowF(j)/sum,j=1,kF)
	      if(discprt) write(7,809) (rowF(j)/sum,j=1,kF)
809	      format(' Normalised rowF ',
     &	8g13.6,/,10(8g13.6))
	   endif
cp	   print 108
	   if(discprt) write(7,108)
108	   format(/)
	endif
	if(idebug.eq.9) then
	   sum=0.d0
	   do j=1,kF
		sum=sum+rowF(j)
	   enddo
	   if(sum.gt.1.d-100) then
cp	     print 812,in,1.e3*topen,(rowF(j)/sum,j=1,kF)
	     if(discprt)write(7,812)in,1.e3*topen,(rowF(j)/sum,j=1,kF)
812	     format(
     &   1x,i8,': open time (ms) = ',g13.6,' New init vector (rowF)=',/,
     &	8g13.6,/,10(8g13.6),/)
	   endif
	endif
c
c NOW GET THE NEXT SHUT TIME (or end the group)
c At present in=index of 1st opening in open period
c If last interval reached, and it is open, then no more gaps, and inext was
c set to nint+1, above, so end the last group at this opening
	if(inext.gt.nint(jset)) goto 92
	in=inext		!should be shut -check, for debug anyway!
	open=.false.
	open=ampl(in,jset).ne.0.
	indop=0
	if(open) then
	indop=indop+1
	if(indop.eq.100) then
	call intconv(in,cstring)
!	imes=gmdisplaymessagebox('',
 !    & 'INTERVAL '//cstring//' should be shut',ginformation,gok)
	 CALL GMSETTEXTSETTING(ITTY,'INTERVAL '//cstring//' should be shut')
cp	   call BELL(2)
cp	   print 61,in
61	   format(' INTERVAL # ',i5,' should be shut')
	endif
	endif
c Check for 2 adjacent gaps, or bad gap
c=	bad=tint(in,jset).lt.0.0		!gap marked bad
	bad=BTEST(iprops(in,jset),3)	!tint(i) was unusable (bit 3='8' set)
	if(in.lt.nint(jset)) then
	   bad1=ampl(in+1,jset).eq.0.      !also bad if next interval is shut too
	endif
	if(bad.or.bad1) then
	   if(burst(jset)) then
		if(badend(jset)) then
		   goto 92	!end present group with prev opening
		else
		   goto 93	!abandon whole burst & look for next
		endif
	   else
		goto 92	 !end present group with prev opening
	   endif
	endif
	if(tint(in,jset).gt.tcrit(jset)) goto 92	!end present group with prev opening
c
c Now have a good shut time, in tshut say so calculate eGFA(t)
	ts=tint(in,jset)
c
c CALCULATE eGFA(t)
	tshut=ts*1.e-3			!seconds
	if(tshut.le.tres2) then		!exact -first dead time
	   t1=dble(tshut)-tres		!sec
	   do m=1,k
	 	ex1(m)=dexp1(-eigen(m)*t1)
	   enddo
c
	   do i=1,kF
	      do j=1,kA
		  EG(i,j)=0.d0
		   do m=1,k
			EG(i,j)=EG(i,j)+Z00F(i,j,m)*ex1(m)
		   enddo
		enddo
	   enddo
c
	else if(tshut.gt.tres2.and.tshut.le.tres3) then		!exact -2nd deadtime
	   t1=dble(tshut)-tres		!sec
	   t2=t1-tres			!sec
	   do m=1,k
		ex1(m)=dexp1(-eigen(m)*t1)
		ex2(m)=dexp1(-eigen(m)*t2)
	   enddo
c
	   do i=1,kF
		do j=1,kA
		   sum=0.0d0
		   do m=1,k
			sum=sum + Z00F(i,j,m)*ex1(m)
			sum=sum - (Z10F(i,j,m)+Z11F(i,j,m)*t2)*ex2(m)
		   enddo
		   EG(i,j)=sum
		enddo
	   enddo
c
	else			!asymptotic
	   t1=dble(tshut)-tres		!sec
	   do m=1,kF
		ex1(m)=dexp1(rootF(m)*t1)		!NB rootA() already negative
	   enddo
c
	   do i=1,kF
		do j=1,kA
		   EG(i,j)=0.d0
		   do m=1,kF
			EG(i,j)=EG(i,j)+XFA(i,j,m)*ex1(m)
		   enddo
	      enddo
	   enddo
	endif
c END OF eGFA(t) CALC
c
c CALCULATE NEW rowA(), and scale if necessary
	amax=0.d0
	do j=1,kA
	   rowA(j)=0.0d0
	   do n=1,kF
		rowA(j)=rowA(j)+rowF(n)*EG(n,j)
	   enddo
	   if(dabs(rowA(j)).gt.amax) amax=dabs(rowA(j))
	enddo
	if(amax.gt.1.d50) then
	   do j=1,kA
		rowA(j)=rowA(j)*1.0d-100	!scale down
	   enddo
	   nscal(jset)=nscal(jset)+1
	endif
c	if(deb) then
	if(idebug.eq.3.or.idebug.eq.5.or.idebug.eq.6) then
	   call ATYPd(EG,' eGFA(t)',kF,kA,km,km)
	endif
	if(idebug.eq.4.or.idebug.eq.5.or.idebug.eq.6) then
cp	   print 802,in,ampl(in,jset),1.e3*tshut,(rowF(j),j=1,kF)
	   if(discprt)write(7,802)in,ampl(in,jset),1.e3*tshut,
     &	(rowF(j),j=1,kF)
802	   format(' i, amp(i), tshut(ms) =',i8,2g13.6,/,' rowF = ',
     &	8g13.6,/,10(8g13.6))
cp	   print 8021,(rowA(j),j=1,kA)
	   if(discprt) write(7,8021) (rowA(j),j=1,kA)
8021	   format(' New rowA=rowF*eGFA(t) ',
     &	8g13.6,/,10(8g13.6))
	   sum=0.d0
	   do j=1,kA
		sum=sum+rowA(j)
	   enddo
	   if(sum.gt.1.d-100) then
cp	      print 811,(rowA(j)/sum,j=1,kA)
	      if(discprt) write(7,811) (rowA(j)/sum,j=1,kA)
811	      format(' Normalised rowA ',
     &	8g13.6,/,10(8g13.6))
	   endif
cp	   print 108
	   if(discprt) write(7,108)
c108	   format(/)
	   call MATSCL2(phiF,EG,ucol,kF,kA,scal,km,km,km)
cp	   print 8061,scal
	   if(discprt) write(7,8061) scal
8061	   format(' f(tshut) = phiF*eGFA(t)*uA = ',g13.6)
	endif
	if(idebug.eq.9) then
	   sum=0.d0
	   do j=1,kA
		sum=sum+rowA(j)
	   enddo
	   if(sum.gt.1.d-100) then
cp	      print 815,in,1.e3*tshut,(rowA(j)/sum,j=1,kA)
	      if(discprt)write(7,815)in,1.e3*tshut,(rowA(j)/sum,j=1,kA)
815	      format(
     &   1x,i8,': shut time (ms) = ',g13.6,' New init vector (rowA)=',/,
     &	8g13.6,/,10(8g13.6))
	   endif
	call MATSCL2(phiF,EG,ucol,kF,kA,scal,km,km,km)
	  
	   if(discprt) write(7,8061) scal
c8061	   format(' f(tshut) = phiF*eGFA(t)*uA = ',g13.6)
	endif
c
c End of shutting.  Next interval should be an opening
	in=in+1
	if(in.gt.nint(jset)) then
	   goto 91		!end of data -group ends with shutting
	endif
	open=ampl(in,jset).ne.0.
	indop=0
	if(.not.open) then
	indop=indop+1
	if(indop.eq.100) then
	call intconv(in,cstring)
!	imes=gmdisplaymessagebox('','Interval'//cstring//'should be open'
 !    &	,gstop,gok)
	CALL GMSETTEXTSETTING(ITTY,'Interval'//cstring//'should be open')
cp	   call BELL(2)
cp	   print 611,in
611	   format(' INTERVAL # ',i5,' should be open')
	endif
	endif
	goto 90		!get next open period -continue with group
c
c END OF GROUP (#ng) when group ends with a shutting (either because a bad
c opening is found, or because last interval in the data is a (good) shutting
c (neither of these should happen with real data!)
91	continue
	if(burst(jset).and.(.not.badend(jset))) then
cp	   call BELL(3)
cp         print 911,ng+1,nopen(ng+1)		!ng not upDATEWd yet
!	imess=gmdisplaymessagebox('',
 !    &' ERROR: burst cannot end with shut time',gstop,gok)
	CALL GMSETTEXTSETTING(ITTY,' ERROR: burst cannot end with shut time')
         if(discprt) write(7,911) ng+1,nopen(ng+1)
911	   format(
     & ' ERROR: burst cannot end with shut time: Group ',i5,
     &	' nopen = ',i6)
	endif
	if(nopen(ng+1).eq.0) goto 73	!skip this group -look for next
	ng=ng+1		!upDATEW number of groups now #ng has ended
c NB olik(ng) is always zero at this point so olik(ng)=rowA*uA
	do j=1,kA
	   OLIK(ng)=OLIK(ng)+rowA(j)	!likelihood for this group
	enddo
c	if(deb) then
	if(idebug.eq.4.or.idebug.eq.5.or.idebug.eq.6.or.
     &	idebug.eq.12) then
cp	   print 799,ng,nopen(ng),olik(ng),inext,
cp     &	ampl(inext,jset),tint(inext,jset)
	   if(discprt) write(7,799) ng,nopen(ng),olik(ng),
     &	inext,ampl(inext,jset),tint(inext,jset)
799	   format(/,
     & ' GROUP # ',i6,': ',i3,' openings, likelihood = ',g14.7,/,
     & '  index, amp, length = ',i8,2g13.6,' Last interval=shut',/)
	endif
c Now find next good opening and start new group (unless end of data reached)
	if(in.ge.nint(jset)) goto 99
73	nopen(ng+1)=0	!initialise number of openings in next group
	OLIK(ng+1)=0.d0	!initialise ready for next group
c must first find a gap that precedes next good opening
	call FINDGAP(in,jset,is,ibad,nbo,nbg,ampl,iprops,nd1,nd2)
	if(ibad.eq.-1) goto 99
	in=is		!index of the gap
	call FINDOPEN(in,jset,iop,ibad,nbo,nbg,ampl,iprops,nd1,nd2) !now look for good opening
	if(ibad.eq.-1) goto 99
	in=iop			!index of the opening
	do j=1,kA		!reset rowA() to initial vector (is phiA best?)
	   if(chsvec(jset)) then
		rowA(j)=phibst(1,j)		!start with phibst
	   else
		rowA(j)=phiA(1,j)		!start with phiA
	   endif
	enddo
	goto 90			!start new group with the next good opening
c
c
c END OF GROUP (#ng) when group ends with an OPENING (normal way)
92	continue
	if(nopen(ng+1).eq.0) goto 74	!skip this group -look for next
	ng=ng+1		!upDATEW number of groups now #ng has ended
c NB olik(ng) is always zero at this point so olik(ng)=rowF*uF, or rowF*endbst
	if(chsvec(jset)) then
         call VECMUL(rowF,endbst,OLIK(ng),kF,km,km)	!OLIK=rowF*endbst
	else
	   do j=1,kF			!mult by uF
		OLIK(ng)=OLIK(ng)+rowF(j)	!likelihood for this group=rowF*uF
	   enddo
	endif
c	if(deb) then
	if(idebug.eq.4.or.idebug.eq.5.or.idebug.eq.6.or.
     &	idebug.eq.12) then
cp	   print 800,ng,nopen(ng),olik(ng),inext,
cp     &   ampl(inext,jset),tint(inext,jset)
	   if(discprt) write(7,800) ng,nopen(ng),olik(ng),
     &	inext,ampl(inext,jset),tint(inext,jset)
800	   format(/,
     & ' GROUP # ',i6,': ',i3,' openings, likelihood = ',g14.7,/,
     & '  i, amp(i), length of gap that ends group= ',i8,2g13.6,/)
	endif
c Now find next good opening and start new group (unless end of data reached)
	if(in.ge.nint(jset)) goto 99
74	nopen(ng+1)=0	!initialise number of openings in next group
	OLIK(ng+1)=0.d0	!initialise ready for next group
c Interval #in should be shut at this point, so now find next good opening
c to start a new group
	call FINDOPEN(in,jset,iop,ibad,nbo,nbg,ampl,iprops,nd1,nd2) !now look for good opening
	if(ibad.eq.-1) goto 99
	in=iop			!index of the opening
	do j=1,kA		!reset rowA() to initial vector
	   if(chsvec(jset)) then
		rowA(j)=phibst(1,j)		!start with phibst
	   else
		rowA(j)=phiA(1,j)		!start with phiA
	   endif
	enddo
	goto 90			!start new group with the next good opening
c
c ABANDON WHOLE BURST and look for next (without incrementing ng) if bad
c interval is found within a burst
93	continue
	if(.not.burst(jset)) then		!should be!!
!		imes=gmdisplaymessagebox('',
 !    &	' ERROR: should get here only for ''bursts''',gstop,gok)
	CALL GMSETTEXTSETTING(ITTY,
     &' ERROR: should get here only for ''bursts''')
cp	   call BELL(3)
cp	   print 932
932	   format(' ERROR: should get here only for ''bursts''')
	endif
c Look for next burst: find a good gap>tcrit
c In case that badend() is true should not get here?
	if(badend(jset)) pause 'Should not get here when bad gap ends grp'
933	call FINDGAP(in,jset,igap,ibad,nbo,nbg,ampl,iprops,nd1,nd2)
	if(ibad.eq.-1) goto 99
	if(tint(igap,jset).lt.tcrit(jset)) then   !look again
	   in=igap+1
	   goto 933
	endif
c Gap #igap is > tcrit- get next good opening after it
	in=igap		!index of the gap
	call FINDOPEN(in,jset,iop,ibad,nbo,nbg,ampl,iprops,nd1,nd2) !now look for good opening
	if(ibad.eq.-1) goto 99
	in=iop			!index of the opening
c reset initial vector
	do j=1,kA		!reset rowA() to initial vector
	   rowA(j)=phibst(1,j)		!start with phibst
	enddo
c
	nopen(ng+1)=0	!reinitialise number of openings in next group
	OLIK(ng+1)=0.d0	!reinitialise ready for next group
	if(first) then
	call intconv(ng+1,cstring)
	call intconv(in,cstring1)
!	imes=gmdisplaymessagebox('',
 !    &' Burst #'//cstring//' abandoned: restart at open period #'
  !   &	//cstring1,gstop,gok)
	CALL GMSETTEXTSETTING(ITTY,
     &' Burst #'//cstring//' abandoned: restart at open period #'
     &	//cstring1)
cp       print 934,ng+1,in
c      if(dprt) write(7,934) ng+1,in
       if(discprt) write(7,934) ng+1,in
934	 format(' Burst #',i5,' abandoned: restart at open period #',i6)
	endif
c
	goto 90			!start new group with the next good opening
c
c
c END OF DATA REACHED -CALCULATE OVERALL LIKELIHOOD
99	continue
c
c Calc mean number of openings/group (printed in main prog via common/nblk)
	ngp(jset)=ng
	if(first.and.ng.gt.0) then	!ie at first call
	   sum1=0.0
	   do j=1,ng
	     sum1=sum1+float(nopen(j))
	   enddo
	   an(jset)=sum1/float(ng)
c Calc SD also?
	endif
c
c CALCULATE OVERALL LIKELIHOOD
	ibad=0
	if(ng.eq.0) then
cp	   call BELL(5)
cp	   print 991
	CALL GMSETTEXTSETTING(ITTY,	'NO GROUPS FOUND')
991	   format(' NO GROUPS FOUND')
	else if(ng.ge.1) then
c Try adding logs at this stage
c	   if(deb) then
	   if(idebug.eq.4.or.idebug.eq.5.or.idebug.eq.6) then
	      do i=1,ng
cp		   print 803,i,nopen(i),olik(i)
      	   if(discprt) write(7,803) i,nopen(i),olik(i)
803		   format(2i8,3x,g13.6)
	      enddo
	   endif
c	   HJCLIK=0.0
	   SETLIK(jset)=0.0d0

c*************
	   do i=1,ng
		if(olik(i).gt.1.0d-300) then
c=		   HJCLIK=HJCLIK+alog(sngl(OLIK(i)))
		   SETLIK(jset)=SETLIK(jset)+dlog(OLIK(i))
		   badlik=.false.
		else
cp		   print 807,i,nopen(i),jset,olik(i)
		   if(i.le.10) then
		   write(string,fmt='(a20,i4,a1,i6,a16,i3,a8,g13.6)')
     &	  'Problem with group ',i,'(',nopen(i),' openings, set ',jset,
     &	'): Lik = ',olik(i)
		   CALL GMSETTEXTSETTING(ITTY,string)
	CALL GMSETTEXTSETTING(ITTY,'printed only first 10 rows for debug')
		   if(discprt) write(7,807)i,nopen(i),jset,olik(i)
807		   format(
     & ' Problem with group ',i4,'(',i6,' openings, set ',i3,
     &	'): Lik = ',g13.6)
			if(discprt) write(7,808)
808		   format('printed only first 10 rows for debug')
			endif
		    badlik=.true.
		endif
	   enddo
3333		continue
c	   HJCLIK=HJCLIK + dble(float(nscal))*2.302585093d2
	   SETLIK(jset)=SETLIK(jset) +
     &		dble(float(nscal(jset)))*2.302585093d2
c Get unscaled log likelihood add nscal*dlog(1.d100)=nscal*230.258...
	endif
c
	if(idebug.eq.7) then
c=	if(nset.gt.1) then
cp	   print 142,jset,setlik(jset),nscal(jset)
	   if(discprt) write(7,142) jset,setlik(jset),nscal(jset)
142	   format(
     &' Set ',i3,': log likelihood = ',g14.7,' (scaled ',i4,' times)')
	endif
c
1001	continue
c Lastly save roots, and guesses for them, for the current set
	do i=1,kA
	   rootAsav(i,jset)=rootA(i)
	   s1Asav(i,jset)=s1A(i)
	   s2Asav(i,jset)=s2A(i)
	enddo
	do i=1,kF
	   rootFsav(i,jset)=rootF(i)
	   s1Fsav(i,jset)=s1F(i)
	   s2Fsav(i,jset)=s2F(i)
	enddo
107	continue
c	if(idebugt.eq.1) then
c	   call TIMER(itick)
c	   print 404,10*(itick-itobs)
c	   if(discprt) write(7,404) 10*(itick-itobs)
c404	   format(
c     &   '    time going through obs: time (ms) = ',i10)
c	endif
c
100	continue	!end of jset=1,nset loop for each data set
c
	first=.false.
c Now calc HJCLIK as sum of log likelihoods for each set
	if(nset.eq.1) then
c=	   HJCLIK=sngl(SETLIK(1))
	   HJCLIK=SETLIK(1)
	   fcomp(1)=HJCLIK
	else
	   HJCLIK=0.0
	   do jset=1,nset
c=		fcomp(jset)=sngl(SETLIK(jset))		!for common/lcomp
		fcomp(jset)=SETLIK(jset)		!for common/lcomp
		HJCLIK=HJCLIK + fcomp(jset)
		if(idebug.eq.7) then
cp      	   print 105,jset,SETLIK(jset)
      	   if(discprt) write(7,105) jset,SETLIK(jset)
105		   format(' Set ',i3,':  Likelihood = ',g14.7)
		endif
	   enddo
	endif
c Record the values of alpha2 and beta2 used to calculate this hjclik
c to check on correlations
	if(idebug.eq.10.or.idebug.eq.11) then
	   nab=nab+1
	   if(nab.le.kab) then
		alpha2(nab)=theta(jalpha)
		beta2(nab)=theta(jbeta)
		aloglik(nab)=hjclik
	   endif
	endif
c
c And reverse sign of HJCLIK so it is maximised
c NB penfunc is set in qset_hjc (normally zero)
	if(penalty) HJCLIK=HJCLIK - penfunc
	HJCLIK=-HJCLIK
c
	if(idebug.eq.7.or.idebug1.gt.1) then
cp	   print 143,-hjclik
	   if(discprt) write(7,143) -hjclik
143	   format(' Overall log likelihood = ',g14.7)
	endif

c NB key F1 has ktype=0, scan code=59 (see \fortran\tkey.for)
c	if(KBHIT()) then
c		ch=GETCH(ktype)
c		if(ktype.eq.0.and.ichar(ch).eq.59) goto ===	!ABORTW with F1
c	endif
c
c  Jump to here if ABORTWed
999	continue
	deallocate(olik,nopen)
c      if(abs(hjclik).lt.1.d-154) hjclik=0.d0
c      if(hjclik.gt.1.d154) hjclik=0.d0
	if(logfit) then
	   do i=1,kfit
		theta(i)=dlog(theta(i))	!restore log before returning
		thmin(i)=dlog(thmin(i))	!restore log before returning
	   enddo
	endif
c	if(idebugt.eq.1) then
c	   call TIMER(itick)
c	   print 401,10*(itick-itlick)
c	   if(discprt) write(7,401) 10*(itick-itlick)
c401	   format(
c     &   ' time in hjclik (ms) = ',i10)
c	   itlast=itick
c	endif
	deallocate(QAF,QFA,QAA,QFF)
	
	RETURN
	end

	subroutine FINDOPEN(in,j,iop,ibad,nbo,nbg,ampl,iprops,nd1,nd2)
c To find next (good) opening in HJCFIT. On entry in=index of current interval
c where search starts from (not changed by call). On exit iop=index of next
c good opening. Ibad=0 normally, but ibad=1 if end of data reached without
c finding a good opening
c Modified 06/09/02 06:02pm so that nbo,nbg accumulate bad openings, gaps
	real*4 ampl(nd1,nd2)
	integer*1 iprops(nd1,nd2)
	logical open,burst,chsvec,good,btest,badend
	COMMON/HJCBLK/Nint(10),tcrit(10),burst(10),chsvec(10),badend(10)
c
	ibad=0
	do 1 i=in,nint(j)
	   iop=i
	   open=ampl(i,j).ne.0.
c=	   if(open.and.tint(i,j).gt.0.) RETURN 	!with index of opening=iop
	   good=.not.BTEST(iprops(i,j),3)	!tint(i) was unusable (bit 3='8' set)
	   if(.not.good) then
		if(open) then
		   nbo=nbo+1
		else
		   nbg=nbg+1
		endif
	   endif
	   if(.not.good) ibad=ibad+1
	   if(open.and.good) RETURN 	!with index of opening=iop
1	continue
	ibad=-1
	RETURN
	end

	subroutine FINDGAP(in,j,iop,ibad,nbo,nbg,ampl,iprops,nd1,nd2)
c To find next (good) gap in HJCFIT. On entry in=index of current interval
c where search starts from (not changed by call). On exit iop=index of next
c good gap. Ibad=0 normally, but ibad=-1 if end of data reached without
c finding a good gap
c Modified 06/09/02 06:02pm so that nbo,nbg accumulate bad openings, gaps

	real*4 ampl(nd1,nd2)
	integer*1 iprops(nd1,nd2)
	logical shut,burst,chsvec,good,btest,badend
	COMMON/HJCBLK/Nint(10),tcrit(10),burst(10),chsvec(10),badend(10)
c
	ibad=0
	do 1 i=in,nint(j)
	   iop=i
	   shut=ampl(i,j).eq.0.
c=	   if(shut.and.tint(i,j).gt.0.) RETURN 	!with index of opening=iop
	   good=.not.BTEST(iprops(i,j),3)	!tint(i) was unusable (bit 3='8' set)
	   if(.not.good) then
		if(shut) then
		   nbg=nbg+1
		else
		   nbo=nbo+1
		endif
	   endif
	   if(shut.and.good) RETURN 	!with index of opening=iop
1	continue
	ibad=-1
	RETURN
	end
