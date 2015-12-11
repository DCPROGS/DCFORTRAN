	subroutine CVOUT1w(theta,noerr,iw,nev,smin,jfix,fitted,titlep,
     & sres,Sdev,titled,yobs,xobs,w,nj,jmiss,setx,niobs,njset,nodata,
     & dcurve,ndth,km2,cormat,kfit,nxval,xgrp,ybar,sdm,ngrp,SSwg,ndf,
     &wtext,thetasc,sdasc)
c
c Output for CVFIT
c 08/31/97 08:11am add analysis of variance analysis here? -last line
c of parameters are from getgroup call in CVIN2
c 12/11/89 03:35pm Lahey version
c Now takes correlation matrix =cormat(kmax,kmax) for display here (rather
c than in hesmat2
c
	real*4 xobs(niobs,njset),yobs(niobs,njset),w(niobs,njset)
	character*60 titled(njset)
	integer jmiss(njset),nj(njset)
	real*4 setx(njset),cormat(km2,km2)
	real theta(100),Sdev(100)
	character*20 TITLEP(100)		!names of params for fit
	integer jfix(100)
	logical fitted,noerr,nodata,dcurve
	LOGICAL CONSTR,FLINE
	logical discprt
	common/dp/discprt
	COMMON/BLOCK1/constr,nset,nfit,nsfit,
     & Xv,kmax,ncomp,nmod,fline,nomit,jomit(20),jset,ifitmode
	COMMON/BLOCK2/ castar,X1,X2,iequiv,ip1,ip2
	common/potrat/jfirst,iset,kmax1			!for pot ratios
	logical logyfit,norm,skipymax,warn
	COMMON/BLOCK3/logyfit,norm,xnorm,iostat	 !CVIN2,YCALCV,CVDISP,GETEQN,CVSIMP,CVOUT1
c for getgroup
	real*4 xgrp(niobs,njset),ybar(niobs,njset),sdm(niobs,njset)
	integer ngrp(niobs,njset),nxval(njset)
c For poptable
	allocatable	:: datasc
	character*60 datasc(:),thetasc(100),sdasc(100),wtext(100) !for ascii version of data for display
	character*45 header
	character*11 cnum1,cnum2,cnum3
	common/pwrfunc/ybarp
c (kmax=actual no of param to be estimated; kmax1=number before extra param
c  added for ifitmode=4,5)
c
c Oct 86- two new variables added to common/block2/
c IEQUIV=0 for equiv subunits; =1 for non-equiv (prev signalled via castar)
c IP1,IP2:
c  (1) mods 10,11 IP1=1 if param are K1,K2; =2 if param are K1, K2/K1.
c  (2) mods 9,11  IP2=1 if param is K2 or (K2/K1 if IP1=2)
c		  IP2=2 if param is k(-2) (must have ip2=1 if ip1=2)
c  (3) mods 9,11  IP2 set negative if k(+2) is a param  to be estimated
c
c
	warn=.false.
c
	if(dcurve) then
         if(discprt) write(7,23)
23       format(' Fixed values for curve superimposed on data')
	else if(nodata) then
         if(discprt) write(7,24)
24       format(' Parameters for the calculated curve')
	endif
	   if(dcurve) then
		header='Parameters for the calculated curve '
	   else if(nodata) then
		header='Fixed values for the curve '
	   else
		if(ifitmode.ge.3) then
		   header='Least squares fit: simultaneous'
		else
		   call INTCONV(jset,cnum1)
		   header='Least squares fit: set '//(cnum1)
		endif
	   endif
	if(.not.(fitted.or.dcurve.or.nodata)) goto 201
	if(nmod.ne.28) then
	   ALLOCATE(datasc(kmax+1))
	else
	   ALLOCATE(datasc(kmax+2))
	endif
	if(noerr) then
	  datasc(1)='        Parameter   Estimate'
	else
	  datasc(1)='        Parameter   Estimate   Approx SD'
	endif
	wtext(1)=datasc(1)
	ii=0
c
c	NB the multiplier for the info and covariance matrix is now defined
c 	in CVFIT just before call to HESMAT3, and sres is now already
c 	incorporated in cases (iw=1,5) in which residuals used for errors
	do i=1,kmax
	   it=i
	   IF(NMOD.EQ.7.AND.I.GT.3) IT=3	!REPEAT BMAX TITLE FOR I=3,4,...
	   if(jfix(i).eq.0.and.(.not.noerr)) then    !errors calc, not fixed
	   	ii=ii+1
	   	se=Sdev(i)	!NB Sdev(i) set to -1 for fixed param
	   	call REALTOCH(theta(i),cnum2,11)
	   	call REALTOCH(se,cnum3,11)
         	thetasc(i)=cnum2
	   	sdasc(i)=cnum3
         	if(discprt) write(7,41)I,TITLEP(it),THETA(I),SE
41	   	FORMAT(' Parameter ',I2,': ',a20,'= ',G13.6,' Approx SD= ',
     &	g13.6)
	   	if(se.gt.0.33*theta(i)) warn=.true.
	   else if(jfix(i).eq.1) then	!fixed (whether errors calc or not)
	      call REALTOCH(theta(i),cnum2,11)
		thetasc(i)=cnum2
         	if(discprt) write(7,412)I,TITLEP(it),THETA(I)
412	   	FORMAT(' Parameter ',I2,': ',a20,'= ',G13.6,' (fixed)')
	   else if(noerr.and.jfix(i).eq.0) then		!no errors,not fixed
	      call REALTOCH(theta(i),cnum2,11)
		thetasc(i)=cnum2
         	if(discprt) write(7,414)I,TITLEP(it),THETA(I)
414	   	FORMAT(' Parameter ',I2,': ',a20,'= ',G13.6)
	   endif
	enddo
	   nmax=0
	   npmax=0
	   do i=1,kmax
		n=len_trim(thetasc(i))
		np=len_trim(titlep(i))
		if(n.gt.nmax) nmax=n
		if(np.gt.npmax) npmax=np
	   enddo
c  	   need 3 char for 1: up to 99:  Then 2 gaps=5; then titlep=npmax char
c        then 2 blank, then nmax char for the number
	   nc=5+npmax+2+nmax	!start col for sd
	   do i=1,kmax
		call INTCONV(i,cnum1)
c		datasc(i+1)=cnum1(1:2)//':  '//titlep(i)(1:npmax)//'  '//
c     &	thetasc(i)(1:nmax)
		datasc(i+1)=cnum1(1:2)//':  '//titlep(i)(1:20)//
     &	thetasc(i)(1:20)
		if(jfix(i).eq.1) then	!fixed (whether errors calc or not)
		   datasc(i+1)=datasc(i+1)(1:45)//'(fixed)'
		else if(.not.noerr.and.jfix(i).eq.0) then
		   datasc(i+1)=datasc(i+1)(1:45)//(sdasc(i))
		endif
		wtext(i+1)=datasc(i+1)
	   enddo
	   nval=kmax+1
	   if(nmod.eq.28) then
		nval=kmax+2
	      call REALTOCH(ybarp,cnum3,11)
		datasc(kmax+2)=' where Ybar = '//(cnum3)
		wtext(kmax+2)=datasc(kmax+2)
	   endif
	   if(nval.le.20) then
		nlist=nval
	   else
		nlist=20
	   endif
	   nhelp=1
	   lenmax=len_trim((header))
	   do i=1,kmax+1
	      nl=len_trim(datasc(i))
	      if(nl.gt.lenmax) lenmax=nl
	   enddo
	   nfst=1
c  Now display correlation matrix too
	if((.not.noerr).and.kfit.le.10) then
	   DEALLOCATE(datasc)
	   ALLOCATE(datasc(kfit+1))
	   do i=1,kmax
		if(jfix(i).eq.0) then
		   call INTCONV(i,cnum1)
		   datasc(1)=' '//cnum1(1:1)
		   i1=i
		   goto 1
		endif
	   enddo
1	   do i=i1+1,kmax
		if(jfix(i).eq.0) then
	   	   call INTCONV(i,cnum1)
	       nl=len_trim(datasc(1))
	   	   datasc(1)=(datasc(1)(1:nl))//'      '//cnum1(1:4)
		endif
	   enddo
	   do i=1,kfit
		datasc(i+1)=' '
		do j=1,i
		   call DCFORMAT(cormat(i,j),6,3,cnum1)
		   if(j.eq.1) then
			datasc(i+1)=cnum1(1:6)
		   else
	          nl=len_trim(datasc(i+1))
		      datasc(i+1)=(datasc(i+1)(1:nl))//' '//cnum1(1:6)
		   endif
		   if(i.ne.j.and.abs(cormat(i,j)).gt.0.9) then
			warn=.true.
		   endif
		enddo
		wtext(nval+1+i)=datasc(i+1)
	   enddo
         wtext(nval+1)= 'Approximate correlation matrix'
	   nval=kfit+1
	   nlist=nval
	   nhelp=2
	endif

c ANCILLARY CALCS WITH FITTED PARAM FOR VARIOUS MODELS
c  (1) mods 10,11 IP1=1 if param are K1,K2; =2 if param are K1, K2/K1.
c  (2) mods 9,11  IP2=1 if param is K2 or (K2/K1 if IP1=2)
c		  IP2=2 if param is k(-2) (must have ip2=1 if ip1=2)
c  (3) mods 9,11  IP2 set negative if k(+2) is a param  to be estimated
c Value of k(+2) is in X2 if value is fixed, or in theta(kmax) if it
c is to be estimated (ip2 neg)
c
	if(logyfit) then
	   j1=0
	   do j=1,nset
	      if(jmiss(j).eq.0) then
		   j1=j1+1
			call INTCONV(j,cnum1)
		      call REALTOCH(theta(2+j1),cnum2,11)
c*			call WDIALOG(1,'Y(max) for set '//(cnum1)//
c*     &		' = '//(cnum2),11)
		   if(discprt) write(7,22) 2+j1,j,theta(2+j1)
22		   format(1x,i2,': Y(max) for set ',i2,' = ',g13.6)
		endif
	   enddo
	endif
c
	if(ip2.lt.0) x2=theta(kmax1)
	ip=iabs(ip2)
c
	if(nmod.ne.10.and.nmod.ne.11) goto 421
	if(ip2.eq.2) goto 49
      if(discprt) write(7,422)theta(3)*theta(2)
422	format(' Implied value of K2= ',g13.6,/)
      if(discprt) write(7,424)theta(3)/theta(2)
424	format(' Implied value of K2/K1= ',g13.6,/)
	goto 421
49	continue
c bit for ip2=2 (Nmod=11 only)
      if(discprt) write(7,422)theta(3)/x2
421	continue
C

	if(fitted) then
	   elmax=-smin/2.
	   if((iw.eq.1.or.iw.eq.5).and.(sres.gt.1.e-15)) then
			elmax=elmax/(sres**2)
      		if(discprt) write(7,43) sres, sres**2
43			format(' Residual error SD = ',g13.6,
     &		' (variance = ',g13.6,')')
	   endif
         if(discprt) write(7,42)NEV,SMIN,elmax
42	   FORMAT(' No of function eval= ',i8,/,' Minimum SSD=',G13.6,
     &	  ' Max log-likelihood= ',g13.6)
	   call INTCONV(nev,cnum1)
	   call REALTOCH(elmax,cnum2,11)
		
	   if(warn) then
      		if(discprt) write(7,70)
70		    format(
     &' WARNING: SOME PARAMETERS POORLY DEFINED -TRY DIFFERENT GUESSES')
	   endif
	endif
c
c ANALYSIS OF VARIANCE for fit
c Already have error within X values from getgroup call
c First get SSD due to regression (weighted and unweighted?)
c and SSD for dev from regression
c  -better calc this at same time as defycal, in main prog?
c==========insert A of V here
c
201	CONTINUE	!jump here for curve with given param
c For power function, print in the linear (log-log) form
	if(nmod.eq.28) then
	   x0=theta(1)
	   en=theta(2)
	   a=alog10(ybarp) - en*alog10(x0)	!intercept
	   if(discprt) write(7,280) ybarp,a,en
280	   format(/,
     &  ' The scaling constant, Ybar, is ',g13.6,/,
     &  ' In log-log form the power function fit can be written as:',/,
     &    '   log10(Y) = ',g13.6,' + ',g13.6,'*log10(X)',/)
	   goto 153
	endif
	if(nmod.eq.21) then	!GHK with Mg block
c Calc permeabilities from single channel cond
	gNa=theta(1)		!conductance in pS
	gCs=theta(2)		!conductance in pS
	v1=25.249		!mV at 20 deg
	F=96484.56		!Coulomb/mole
	cNa=0.15		!molar
	cCs=0.14		!molar
	PNa=(gNa*v1)/(F*cNa)    !pS*mV=femtoamps
	PCs=(gCs*v1)/(F*cCs)    !pS*mV=femtoamps
	PNa=PNa*1.e-15
	PCs=PCs*1.e-15
	if(discprt) write(7,211) PNa,PCs
211	format(' Permeability (1/(sec*molar)) for Na, Cs = ',2g13.6)
	Erev=v1*alog((PNa*cNa)/(PCs*cCs))
	Eoff=theta(3)	!offset in Erev, relative to GHK value
	if(discprt) write(7,212) Erev,Erev+Eoff
212	format(' Reversal potential (mV): predicted, fitted = ',2g13.6)
c can also look at K1/K2 and its V-dep
	   goto 153
	endif
	IF(NMOD.EQ.9.OR.NMOD.EQ.11) GOTO 152
	if(nmod.eq.14.or.nmod.eq.15) goto 314
	goto 153
c
c Section for nmod=9,11 only
152	continue
c152	AK2=THETA(2)	!for 9,11
c Value of k(+2) is in X2 if value is fixed, or in theta(kmax1) if it
c is to be estimated (ip2 neg)
	if(ip2.lt.0) x2=theta(kmax1)
	ip=iabs(ip2)
	if(ip.eq.2) goto 50
c for ip2=+/-1
	AK1=THETA(2)
	AK2=AK1
	IF(ip1.eq.1.and.(NMOD.EQ.10.OR.NMOD.EQ.11)) AK2=THETA(3)
	IF(ip1.eq.2.and.(NMOD.EQ.10.OR.NMOD.EQ.11)) AK2=THETA(3)*ak1
	goto 51
50	continue
c for ip2=+/-2 (cannot have ip1=2 in this case)
	if(nmod.eq.9) AK1=theta(2)/x2	!k-1/k+2
	ak2=ak1
	if(nmod.eq.11) AK1=theta(2)
	if(nmod.eq.11) AK2=theta(3)/x2	!k-2/k+2
51	continue
c	if(ip.eq.1.and.NMOD.EQ.11) AK2=THETA(3)
c	IF(ip.eq.2.and.NMOD.EQ.11) AK2=THETA(3)*theta(2)
C X1=MEAN BST LENGTH (SEC). X2=K(+2) (both read in CVIN)
c  (1) mods 10,11 IP1=1 if param are K1,K2; =2 if param are K1, K2/K1.
c  (2) mods 9,11  IP2=1 if param is K2 or (K2/K1 if IP1=2)
c		  IP2=2 if param is k(-2) (must have ip2=1 if ip1=2)
c  (3) mods 9,11  IP2 set negative if k(+2) is a param  to be estimated
	X4=X2*AK2			!k(+2)*K2=k(-2) 1/sec
	X5=X2*AK1			!k(+2)*K1=k(-1)
	X6=2.*X4			!DISSOC RATE=2k(-2)
	IF(NMOD.EQ.11.AND.iequiv.eq.1) X6=X4+X5
	X3=1.+THETA(1)/X6		!OPS/BST
C NOTE: (X1-(BA/X6)/(BA+X6))=TOTAL OPEN TIME/BURST
	A=X3/(X1-(BA/X6)/(BA+X6))	!ALPHA
	BA=THETA(1)/A			!BETA/ALPHA
      if(discprt) write(7,154)1.E3/A,BA,X3,1.E6/(THETA(1)+X6)
154	FORMAT(/, ' 1/alpha (ms)= ',G12.5,'  beta/alpha= ',G12.5,/,
     & ' mean no of ops/bst= ',G12.5,'  short gap length (mus)= ',G12.5)
      if(discprt) write(7,155)X4
155	FORMAT( ' k(-2)= ',G12.5)
      if(discprt) write(7,156)X5
156	FORMAT( ' Dissoc rate for subunit 1: k(-1)= ',G12.5)
	goto 153
c Now bit for nmod=14 and 15 only
314	ak11=theta(1)
	ak21=ak11/theta(2)
	ak22=ak21/theta(3)
	ak12=ak22*theta(2)	!from micro rev
      if(discprt) write(7,315)ak11,ak21,ak12,ak22
315	format(' Kij=jth sequential binding to ith subunit type',/,
     & ' Nonequiv=K12/K22=K11/K21; Coop=K11/K12=K21/K22',/,
     & ' K11,K21,K12,K22= ',4g13.6)
	if(nmod.eq.14) goto 153
c now nmod=15- calc alpha and hence beta/alpha from mean burst length (x1)
c If k(+2) is fixed it is in x2; if to be estimated x2=-1.
	beta=theta(4)
	af2=x2				!k(+2)
	if(kmax1.eq.6) af2=theta(6)	!k(+2)
	ab1=af2*ak12	!k(-2) for subunit 1
	ab2=af2*ak22	!k(-2) for subunit 2
	x3=1./(beta+ab1+ab2)	!mean short gap length
	x4=beta/(ab1+ab2)	!mean short gaps/burst
	a=(1.+x4)/(x1-x3*x4)	!alpha
	ba=beta/a
c
      if(discprt) write(7,154)1.E3/A,BA,1.+x4,1.e6*x3
c154	FORMAT(/, ' 1/alpha (ms)= ',G12.5,'  beta/alpha= ',G12.5,/,
c     & ' mean no of ops/bst= ',G12.5,'  short gap length (mus)= ',G12.5)
      if(discprt) write(7,1551)af2,ab1,ab2
1551	format(
     & ' k(+2)= ',g12.5,' (assumed same for both subunit types)',/,
     & ' k(-2) for subunit 1= ',g12.5,/,
     & ' k(-2) for subunit 2= ',g12.5)
	goto 153
C
153	continue	!all ancillary calcs finished
	call flush(7)
c
	if(nodata) goto 99
c
c/===
c*	   ans='Y'	!print them if in alpha mode but not on screen
c*	if(UC(ans).ne.'Y') GOTO 151
	jstart=jset		!unless several sets needed (jset in common)
	jend=jset
	if(ifitmode.ge.3.and.ifitmode.le.5) then
	   jstart=1
	   jend=nset
	endif
	iset=0
	ival=1	!so xv1 in call to ycalcv is used
	do 135 j=jstart,jend	!need loop only for ifitmode=3
	if(jmiss(j).eq.1) goto 135	!OMIT SPECIFIED DATA SETS
	iset=iset+1		!for COMMON -nec for ifitmode=4,5 if some sets missed
      if(discprt) write(7,138)J,TITLED(j)
138	FORMAT( ' SET #= ',I3,3X,a60)
	if(discprt) write(7,1381)
1381	format(/,4x,
     & ' X value         Y (observed)    Y (calc)        Ycalc-Yobs')
	do 136 i=1,nj(j)
c=	YC=Ycalcv(KMAX,THETA,i,j)
	xv1=xobs(i,j)
	Yc=YCALCV(kmax,theta,xv1,ival,j,setx,njset)
	RESID=YC-YOBS(I,J)
      if(discprt) write(7,137)XOBS(I,J),YOBS(I,J),YC,RESID
137	FORMAT(3X,4(G13.6,3X))
	if(nmod.ne.99) goto 136
c for polynomial print the slope too
      if(discprt) write(7,31)x2
31	format('    slope= ',g13.6)
136	continue
135	continue
c
151	CONTINUE
	goto 99
c If in graphics (normal now) this is never done!
c
142	continue
140	FORMAT(' Data set # [set 1] = ')
c*	read 104,i
104	format(i8)
	j=1
	if(i.ge.1) j=i
141	FORMAT('&X(start), delta-X, X(end) = ')
c*	read 133,xs,dx,xe
133	format(3g13.6)
	if(abs(dx).lt.1.e-20) goto 142
	a=abs((xe-xs)/dx)
	n=1 + ifix(a)
	ival=1
	do 1331 k=1,n
	xv=xs+float(k-1)*dx
c=	i=101		!SIGNAL TO YCAL TO USE XV,NOT XOBS(I,J)
c=	YCalc=YCALCV(KMAX,THETA,i,j)
	Ycalc=YCALCV(kmax,theta,xv,ival,j,setx,njset)
	if(nmod.eq.99) goto 1332
      if(discprt) write(7,143)k,XV,YCALC
143	FORMAT(i4,2(2x,g13.6))
	goto 1331
1332	continue
      if(discprt) write(7,32)k,XV,YCALC,x2
32	FORMAT(i4,3(2x,g13.6))
c for polynomial print the slope too
1331	continue
	goto 151
c
99	continue
	call flush(7)
	DEALLOCATE(datasc)
c
	RETURN
	end




