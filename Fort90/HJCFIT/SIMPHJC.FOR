	subroutine SIMPHJC(kt,THETA,stpfac,errfac,neval,nevmax,
     & fmin,FUNC,Ndisp,jfix,delmin,confac,irestrt,resfac,iconv,
     & tint,ampl,iprops,nd1,nd2,
     & Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & XAF,XFA,QexpQA,QexpQF,
     & alpha2,beta2,aloglik,kab,
     & autosim,nsims,
     & kAm,kFm,km)
c
c Use F1 to abort
c SIMPHJC is special version of SIMPLEX3 for use in HJCFIT, with data
c   arrays in call, and in all calls to FUNC (so that allocatable arrays,
c   which cannot go in common, can be used for data).
c NB in call, use kfit as first parameter, not kt
c Never have fixed parameters in HJCFIT (as far as Simplex is concerned
c Section for fixed paramters not revised for LOGFIT case
c Section for fixed paramters removed entirely in this version
c (kept in simphjc0.for).
c
c SIMPLEX3 is version of SIMPLEX that has the same modifications as in
c SIMPLEX2 (which is silent version for SCAN) as follows:
c
c Modif 08/14/03 09:24am so that niter is not reset to zero after restart
c
c Modif 01/16/02 12:34pm to fit log of rate constants (so steps in simplex
c can be geometrically spaced).  Never have fixed parameters in HJCFIT
c (as far as Simplex is concerned -section for fixed paramters not
c revised for LOGFIT case. Also modified so that input value of ICONV
c controls convegence criterion in logfit case (when logfit=F convergence
c is as before).  Section for fixed paramters removed entirely in this version
c (kept in simphjc0.for).
c
c Modif 01/16/02 03:04pm
c	logical logfit
c	common/logf/logfit
c
c  Modif 09/20/01 10:31am for 100 states. Many arrays added as params for FUNC
c   ktd=dimension of theta=200 now
c
c  Modif 03/20/00 05:55am -now ALL REAL*8 (and expects real*8 func)
c
c  Modif 09/15/95 09:18pm by addition of RESFAC to parameters -when
c	restart is done, step size is now reset to resfac*crtstep (i.e. resfac
c	was implicitly 1.0 in earlier versions) (as in simplex4).
c
c Modif 08/03/94 03:09pm so that input value of IRESTRT is taken as
c maximum number of restarts allowed.  Also, when this number of restarts
c has been done, the program is left with the best of all four options
c At present, leave when (1) best vertex, or (2) average vertex is best, but
c if (3) absmin or (4) pnew1 is best then a restart is done).  Now, if (1) or
c (2) is best so no restarts at all are done (or if irestrt=0) then
c leave with the best of all 4 options.
c NB confac,irestrt,iconv added to parameters!
c
c   INPUT
c iconv=1 -use errfac in place of crtstp (same for all parameters, like step)
c iconv=2 -take antilog to test non-log parameter as in non-logfit case
c iconv=3 -use crtstp(j)=errfac*temp(j) as in first tests (seemed to work
c		but does not make much sense?)
c
c   OUTPUT
c	irestrt=0	no restarts
c	irestrt=1	signals restart at absmin
c	irestrt=2	signals restart at after local search
c	iconv=1	converged via DELMIN
c	iconv=2     return with best vertex
c	iconv=3     return with average vertices
c	iconv=4     return with absmin
c	iconv=5     return with pnew1 (local search result)
c	iconv=6     no convergence (after nevmax iterations)
c
c
c Modif 05/19/93 05:31pm by adding common/abt/abort which can, if desired,
c be put into FUNC, so if abort set=true in func it causes clean abort here
c 05/28/92 10:09am Altered so that if NEVMAX is negative on entry AND
c discprt=true then (a) iterations are not printed to disc (but 'run
c aborted' still will be, and (b) sign of nevmax is restored.
c 06/03/89 02:39pm Lahey version of SIMP7V. Does not need to have data
c as parameter of FUNC (so Yval,,ndim,Xaxis,Freq are no longer parameters)
c 02/21/90 05:12pm Altered so
c NDISP=-1000 prints nothing at all (eg for likelihood intervals);
c NDISP=-n prints every nth iteration IF CAPLOCK (debug) is on, otherwise not.
c***06/05/89 10:40am Version that has separate section that omits squeeze/
c unsqueeze when no parameters are to be fixed.
c
c Version with option to converge when
c reduction in func is less than DELMIN (ignored if delmin is neg)
c***Converges prematurely with DELMIN criterion so altered (Sep 1986)
c so that must be less than DELMIN for 5 successive iterations before
c convergence.
c Also initial guesses now in theta on entry.
c
C USES SUBROUTINE SQZ TO ELIMINATE FIXED PARAMETERS I.E TO SQUEEZE
C VARIABLE PARAMETERS FROM INPUT ARRAY THETA INTO INTERNAL ARRAY. UNSQEEZE
C AGAIN (INSERT FIXED PARAM) FOR OUTPUT AT END AND FOR PRINTING
c**june 1985. If func alters THETA values (e.g.constrained to be >1 etc)
c then should restore these values to prog by call to sqz
c
C JUNE 1984. MODIFIED WITH FEATURES FROM I.D.HILL BASED ON O'NEILL,R
C APPLIED STATS AS47,20,338-345 (1971) AND BENYON P.R.,IBID. 25,97 (1976)
C D. COLQUHOUN (+ Caceci MS and Cacheris WP, 1984 + Ch. Methfessel +AB Cachelin)
c
c	call SIMPLEX(kt,THETA,stpfac,errfac,neval,nevmax,
c     & fmin,FUNC,Ndisp,jfix,delmin)
c
c	Where:
c	KT		: total number of parameters			input
c			: (internally K= no of variable param)
c	THETA(K)	: initial guesses (=BP) 	 input
c	THETA (K)	: final parameters		 output
c	STPFAC	: factor for initial step sizes: input (usually 0.1*init guess)
c	ERRFAC	: convergence criterion (usually 1.e-4)
c	NEVAL		: # of function evaluations		output
c	NEVMAX	: maximum number of evaluations	input
c	FMIN		: minimum value of function		output
c	FUNC		: function to be minimised		input
c	NDISP		: display SIMP every NDISP times	  .
c			: no print if ndisp=0. if ndisp <0 then print
c			: every -ndisp eval and R=ref,E=ext,C1=con on
c			: ihi side,C2=con on ref side,S=shrink,T=test conv
c	JFIX(K)	: =0 for variable, =1 for fixed parameters
c
c Internal definitions
c	FVAL(n)	=value of function (e.g. SSD) at each vertex
c	SIMP(n,k)	: simplex. n=k+1 rows=vertices. k cols=parameters
c	N		: dimension of parameter space (=K+1)
c	PNEW(K)		: param values at next vertex to be tested
c	PNEW1(K)	: ditto
c	CENTRE(k)	: centroid of hyperplane defined by all
c			: vertices except the worst
c	ABSMIN		: smallest fmin ever found
c	THMIN(K)	:  and corresponding parameters
c	LERR		: error and convergence flag
c
c ---------------------------------------------------------------------
c
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c Data for FUNC (=HJCLIK)
	real*4 tint(nd1,nd2),ampl(nd1,nd2)
	integer*1 iprops(nd1,nd2)
c Other declarations
	real*8 FUNC
	real*8 theta(200)
	real*8 step(200),crtstp(200)
	real*8 crtstp1(200)  !non-log values to test convegence with iconv=2
	real*8 temp(200),centre(200)
	real*8 pnew(200),pnew1(200),fval(201),thmin(200),simp(201,200)
	integer jfix(200)
c	logical pp	!to print progress
	logical silent,debtyp		!no print out at all; or only if caplock on
	logical autosim
c
c New parameters (to pass to FUNC=HJCLIK
	real*8 Z00A(kAm,kFm,km),Z10A(kAm,kFm,km),Z11A(kAm,kFm,km)
	real*8 Z00F(kFm,kAm,km),Z10F(kFm,kAm,km),Z11F(kFm,kAm,km)
	real*8 QEXPQA(kFm,kAm),QEXPQF(kAm,kFm)
	real*8 XAF(kAm,kFm,kAm),XFA(kFm,kAm,kFm)
  	real*8 alpha2(kab),beta2(kab),aloglik(kab)
	common/absave/nab,jalpha,jbeta		!in hjclik, hjcdisp
c
	common/eval/neval1,niter	!neval1=neval, for print in hjcasymp
c
	logical deb,debug,caplock
	integer*2 ktype
	logical KBHIT		!Utility lib
	character ch,getch
	logical abort
	logical discprt,dprt
c
	common/reset/absmin,thmin,step			!for HJCLIK
	common/dp/discprt
	common/abt/abort
	logical logfit
	common/logf/logfit
	external FUNC
c
c
c	debug()=caplock()
c	deb=caplock()
	deb=.false.
	abort=.false.
c
	call UNDER0('true')	!so underflows set=0 (Lahey manual p12.8)
	silent=ndisp.eq.-1000
	debtyp=ndisp.lt.0
	dprt=discprt.and.(nevmax.gt.1)	!iterations printed to disc
	ndisp1=iabs(ndisp)     		!so input ndisp not changed
	nevmax=iabs(nevmax)		!always pos on output
	if(silent) ndisp1=0
	if(.not.silent) print 31
31	format(' F1 key to ABORT',/,'  -calculating...')
	call CLRKB		!Remove any waiting keyboard char: Utility lib
c
	nresmax=irestrt	!save input value=max number of restarts
	istart=0		!counts number of restarts
c
c Set ndel=no of consec iterations for which reduction in func<delmin
c for convergence
	ndel=5
	idel=0		!counts up to ndel
C section to deal with fixed values
c Fixed param stay in their correct positions in theta
c
c	if(debug()) print 750,0,(jfix(i),i=1,8),(theta(i),i=1,8)
c750	format(' pos #',i2,': ',8i4,/,8f10.3)
	call sqz(theta,temp,jfix,kt)	!squeeze theta into temp
c (NB if logfit then theta and temp() contain log(rate) on entry)
c Count no of param to be fitted, and set STEP(), CRTSTP().
c iconv=1 -use errfac in place of crtstp (same for all parameters, like step)
c iconv=2 -take antilog to test non-log parameter as in non-logfit case
c iconv=3 -use crtstp(j)=errfac*temp(j) as in first tests (seemed to work
c		but does not make much sense?)
	k=0
	do j=1,kt
	   if(jfix(j).eq.0) then
		k=k+1
		if(logfit) then
		   step(k)=stpfac		!=log(fac)
		else
		   step(k)=stpfac*temp(k)
		endif
		crtstp(k)=errfac*temp(k)	!if not logfit
		if(logfit) then
		   if(iconv.eq.1) then
			crtstp(k)=errfac
		   else if(iconv.eq.2) then
			crtstp(k)=errfac
			crtstp1(k)=errfac*dexp(temp(k))	!to test convergence only
		   else if(iconv.eq.3) then
			crtstp(k)=errfac*temp(k)	!orig method -makes no sense?
		   endif
		endif
	   endif
	enddo
c
c Never have fixed parameters in HJCFIT (as far as Simplex is concerned
c Section for fixed paramters not revised for LOGFIT case
c Section for fixed paramters removed entirely in this version
c (kept in simphjc0.for).
	if(k.eq.kt) goto 3000	!no fixed parameters so use fast version
	call BELL(5)
	print 32
32	format(' ERROR IN SIMPLEX -tell DC')
c
c Now fast version for case where there are no fixed parameters
c
3000	continue
c	if(.not.silent) print 30
c30	format(' USING FAST VERSION OF SIMPLEX')
c	ndisp=iabs(ndisp)
	reffac=1.0		! reflection coeff => 1
	extfac=2.0		! extension factor > 1
	n=k+1		! # of vertices in simplex
	neval=0			!counts function evaluations
	neval1=neval		!for common
	niter	= 0
c
c
2001	continue	!return here for restart
	call NWVRTX(1,theta,simp,k)	!start values=vertex #1
c=====temp debug
c	   if(neval.ge.20) then
c		pause
c	   endif
	fval(1)=FUNC(kt,theta,
     & tint,ampl,iprops,nd1,nd2,
     & Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & XAF,XFA,QexpQA,QexpQF,
     & alpha2,beta2,aloglik,kab,
     & kAm,kFm,km)
	neval=neval+1
	neval1=neval	!for common
	call DSPOUT(kt,neval,ndsav,fval(1),theta,dprt,autosim,nsims)	!type it
	if(abort) goto 1384	!may be set true via common
	fsav=fval(1)
c initialise absmin, thmin()
	absmin=fval(1)		!starting value
	do i=1,k
	   thmin(i)=theta(i)
	enddo
c
c ----- compute offset of the vertices of the starting simplex
	fac=(sqrt(float(n))-1.)/(float(k)*sqrt(2.))
c
c ----- specify all other vertices of the starting simplex
c
	do i=2,n
	   do j=1,k
		simp (i,j)=simp (1,j) + step(j)*fac
	   enddo
	   simp (i,i-1)=simp(1,i-1) + step(i-1)*(fac+1./sqrt(2.))
c
c ----- and calculate their residuals
c
	   I1=i
	   call RCOPY(I1,SIMP,theta,k)
c=====temp debug
c	   if(neval.ge.21) then
c		pause
c	   endif
	   fval(i1)=FUNC(kt,theta,
     &	 tint,ampl,iprops,nd1,nd2,
     &	 Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     &	 XAF,XFA,QexpQA,QexpQF,
     &	 alpha2,beta2,aloglik,kab,
     &	 kAm,kFm,km)
	   if(abort) goto 1384	!may be set true via common
	   call abstst(fval(i1),theta,absmin,thmin,k)	!test for abs min
	   neval=neval+1
	   neval1=neval	!for common
	   call DSPOUT(kt,neval,ndsav,fval(i1),theta,dprt,autosim,nsims)	!type it
	enddo
c
c ----- start iteration loop here at 2000
c
	ndsav=0
2000	continue
	niter	= niter + 1
c
C find best (lowest fval) and worst (highest fval) vertices- indeces
c of these are ILO,IHI respectively. fnewlo=fval(IHI) corresponds to
c the vertex to be replaced
	flo=fval(1)
	fnewlo=flo
	ILO=1
	IHI=1
	do 151 I=2,N
	if(fval(I).GE.flo) goto 141
	flo=fval(I)
	ILO=I
141	if(fval(i).le.fnewlo) goto 151
	fnewlo=fval(i)
	IHI=I
151	continue
c
c display current best vertex
	if(silent.or.ndisp1.eq.0.or.(.not.deb.and.debtyp)) goto 1360
	if((neval-ndsav).lt.ndisp1) goto 1360
c	if(silent.or.ndisp1.eq.0.or.((neval-ndsav).lt.ndisp1)) goto 1360
	CALL RCOPY(ILO,simp,theta,k)	!copy best vertex to theta
	call DSPOUT(kt,neval,ndsav,flo,theta,dprt,autosim,nsims)	!type it
1360	continue
	if(neval.gt.nevmax) goto 1510
c
c ----- compute centroid of all vertices except the worst
c
	do j=1,k
	   centre(j)= 0.0
	enddo
c	do 1220 i=1,n
	do i=1,n
c	   if(i.eq.IHI) goto 1220
	   if(i.ne.IHI) then
		do j=1,k
		   centre(j)=centre(j) + simp (i,j)
		enddo
	   endif
c1220	   continue
	enddo
c
c ----- reflect, with next vertex taken as reflection of worst
c Parameter values that are coord of new vertex in pnew(j)
c
	do j=1,k
	   centre(j)=centre(j)/float(K)
	   pnew(j)=centre(j) - reffac*(simp(IHI,J)-centre(J))
	enddo
	neval1=neval	!for common
c=====temp debug
c	   if(neval.ge.21) then
c		pause
c	   endif
	fnew=FUNC(kt,pnew,
     & tint,ampl,iprops,nd1,nd2,
     & Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & XAF,XFA,QexpQA,QexpQF,
     & alpha2,beta2,aloglik,kab,
     & kAm,kFm,km)
	if(abort) goto 1384	!may be set true via common
	call abstst(fnew,pnew,absmin,thmin,k)	!test for abs min
	neval=neval+1
	neval1=neval	!for common
	call DSPOUT(kt,neval,ndsav,fnew,pnew,dprt,autosim,nsims)	!type it
c -----
	if (fnew.ge.flo) goto 112	!fnew worst than prev best
c
c ----- new vertex is better than previous best so extend it
c
	do j= 1,k
	   pnew1(j) =centre(j) + extfac*(pnew(j)-centre(j))
	enddo
c=====temp debug
c	if(neval.ge.21) then
c	   pause
c	endif
	fnew1=FUNC(kt,pnew1,
     & tint,ampl,iprops,nd1,nd2,
     & Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & XAF,XFA,QexpQA,QexpQF,
     & alpha2,beta2,aloglik,kab,
     & kAm,kFm,km)
	neval=neval+1
	neval1=neval	!for common
	call DSPOUT(kt,neval,ndsav,fnew1,pnew1,dprt,autosim,nsims)	!type it
	if(abort) goto 1384	!may be set true via common
c#	call sqz(theta,pnew1,jfix,kt)	!restore
	call abstst(fnew1,pnew1,absmin,thmin,k)	!test for abs min
c ----- still better?
	if (fnew1.ge.fnew) goto 119	! no- input pnew and test conv
c
c ----- yes- so extension (pnew1) inputed (or contraction inputed if
c	 arrive at 110 via 115 below)
c
110	continue
c	neval=neval+1
c	neval1=neval	!for common
	call NWVRTX (IHI,pnew1,simp,K)
	fval(IHI)=fnew1
	call abstst(fnew1,pnew1,absmin,thmin,k)	!test for abs min
	goto 1901		!convergence check
c
c come to 112 if reflected vertex not better than best vertex, so
c no extension wanted
112	continue
c ----- Is reflected vertex better than worst vertex? L=the
c number of vertices which are worse than fnew=number of vertices
c that fnew is better than.
	L=0
	do i=1,n
	   if(fval(i).gt.fnew) L=L+1
	enddo
	if(L.ge.2) goto 119	!reflected vertex better than 2 or more
c				! so input pnew and test conv
c	if(L.ge.1) goto 119	!test** more like other version? no good!
	if(L.eq.0) goto 115	!reflected vertex is better than none i.e.
c				!worse than all so contract on original
c				!(unreflected) side
c
c Get here if L=1 i.e. fnew better than one (the worst) vertex so
c input unextended reflection, pnew, then contract on new (reflected) side
c (after vertex ihi replaced by reflected vertex loop at 270 does this)
c
	call NWVRTX(IHI,pnew,simp,K)
	fval(IHI)=fnew
c
115	continue
c
c Contract on the original fval(IHI) side of the centroid
c
1107	continue
	do j= 1,k
	   pnew1(j) = centre(j)+confac*(simp(IHI,J)-centre(J))
	enddo
c=====temp debug
c	if(neval.ge.21) then
c	   pause
c	endif
	fnew1=FUNC(kt,pnew1,
     & tint,ampl,iprops,nd1,nd2,
     & Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & XAF,XFA,QexpQA,QexpQF,
     & alpha2,beta2,aloglik,kab,
     & kAm,kFm,km)
	if(abort) goto 1384	!may be set true via common
	call abstst(fnew1,pnew1,absmin,thmin,k)	!test for abs min
	neval=neval+1
	neval1=neval	!for common
	call DSPOUT(kt,neval,ndsav,fnew1,pnew1,dprt,autosim,nsims)	!type it
c
c ----- is contracted vertex better than the worst vertex
	if(fnew1.le.fval(IHI)) goto 110
c
c
c ----- no, it is still bad, shrink whole simplex towards best vertex
c
	do 118 i = 1,n
	   do 117 j = 1,k
		simp(i,j) = simp(ILO,J)+confac*(simp(i,j)-simp(ILO,J))
c  last line is D.C. version that uses confac rather than 0.5
		theta(j)=simp(i,j)
117	   continue
c=====temp debug
c	   if(neval.ge.21) then
c		pause
c	   endif
	   fval(i)=FUNC(kt,theta,
     & tint,ampl,iprops,nd1,nd2,
     & Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & XAF,XFA,QexpQA,QexpQF,
     & alpha2,beta2,aloglik,kab,
     & kAm,kFm,km)
	   if(abort) goto 1384	!may be set true via common
	   call abstst(fval(i),theta,absmin,thmin,k)	!test for abs min
	   neval=neval+1
	   neval1=neval	!for common
	   call DSPOUT(kt,neval,ndsav,fval(i),theta,dprt,autosim,nsims)	!type it
118	continue
	goto 1901	!converged?
c
c Retain reflection
119	continue
	call NWVRTX(IHI,pnew,simp,K)
	fval(IHI)=fnew
c
1901	continue
C CHECK CONVERGENCE. IF NOT CONVERGED GOTO 1000. This version uses
c diff between highest and lowest value of parameter of the n values
c that define a vertex (as in O'Neill version)
c ----- order the vertices for all vertices
c
c Define L=0 for not converged- do next iteration
c L=1 for converged via crtstp
c L=2 for converged via delmin (no restarts)
c L=3 for abort (no restarts)
	L=0	!not converged
	if(delmin.lt.0.) goto 180	!use normal convergence check
c Use delmin criterion- find f at current best vertex
	il=1
	do i=1,n
	   if(fval(i).lt.fval(il)) il=i
	enddo
	del=fsav-fval(il)	!reduction in min=pos value
	IF(DEL.LT.-1.d-5) GOTO 184	!MIN IS INCREASING! DEL NEGATIVE
	if(del.gt.delmin) goto 183
c Section when DEL < DELMIN
	idel=idel+1
	if(idel.lt.ndel) goto 1831	!not yet converged
	if(.not.silent) print 185
185	FORMAT( ' Converged via DELMIN ',/)
	L=2		!converged
	GOTO 184
C I.E. DEL POSITIVE. IF <DELMIN FINISH
183	idel=0			!reset when del > delmin
1831	fsav=fval(il)		!SAVE LAST MINIMUM
	goto 184
c
180	continue
c Can try various convergence criteria in logfit case
	if(.not.logfit) then
	   L=1		!conv via crtstp
	   do j=1,k	!test each parameter
		IL=1
		IH=1
		do i=1,n	!order values of current param
		   if(simp(i,j).lt.simp(IL,j)) IL=i
		   if(simp(i,j).gt.simp(IH,j)) IH=i
		enddo
		if((simp(IH,j)-simp(IL,j)).gt.dabs(crtstp(j))) L=0	!not conv
	   enddo
	else if(logfit) then
c iconv=1 -use errfac in place of crtstp (same for all parameters, like step)
c iconv=2 -take antilog to test non-log parameter as in non-logfit case
c iconv=3 -use crtstp(j)=errfac*temp(j) as in first tests (seemed to work
c		but does not make much sense?)
c NB for iconv=2 (only) use crtsp1() not crtstp() below (former contains the
c usual non-log crtstp values, errfac*initial guess)
	   if(iconv.eq.1.or.iconv.eq.3) then	!see definitions of crtstp
		L=1		!conv via crtstp
		do j=1,k	!test each parameter
		   IL=1
		   IH=1
		   do i=1,n	!order values of current param
			if(simp(i,j).lt.simp(IL,j)) IL=i
			if(simp(i,j).gt.simp(IH,j)) IH=i
		   enddo
		   if((simp(IH,j)-simp(IL,j)).gt.dabs(crtstp(j))) L=0	!not conv
		enddo
	   else if(iconv.eq.2) then	!crtstp in non-log units
		L=1		!conv via crtstp
		do j=1,k	!test each parameter
		   IL=1
		   IH=1
		   do i=1,n	!order values of current param
			if(simp(i,j).lt.simp(IL,j)) IL=i
			if(simp(i,j).gt.simp(IH,j)) IH=i
		   enddo
		   phigh=dexp(simp(IH,j))	!parameter(and crtstp) in orig units
		   plow=dexp(simp(IL,j))	!parameter in orig units
		   if((phigh-plow).gt.dabs(crtstp1(j))) L=0	!not conv
		enddo
	   endif
	endif
c
184	continue
c
1383	continue
	if(abort) goto 1384	!may be set true via common
	if(KBHIT()) then
	   ch=GETCH(ktype)
	   if(ktype.eq.0.and.ichar(ch).eq.59) goto 1384	!abort
	endif
c
	if (L.eq.0) goto 2000	!next iteration
	GOTO 1385
1384	if(.not.silent) then
	   print 3841
	   if(discprt) write(8,3841)
3841	   format( ' RUN ABORTED')
	   abort=.true.
	endif
	L=3	!abort
1385	CONTINUE
c
c ----- convergence attained. Options for ending in this version are:
c	(1)look at current best vertex
c	(2)look at param values averaged over vertices
c	(3)look at absmin,thmin. If better, restart at absmin, as below.
c	(4)do local search with each param +/- crtstp, as in O'Neill
c	 version, starting at current best vertex. If none are better
c	 input current best vertex. If some better restart at better
c	 value with crtstp taken as approptiately small initial step.
c
c get best vertex
	if(fval(ihi).gt.fval(ilo)) ihi=ilo	!addition by I.D.HILL
	do 123 j=1,k
123	temp(j)=simp(ihi,j)
c##123	theta(j)=simp(ihi,j)
	fnewlo=fval(ihi)
	if(.not.silent) then
	   call DSPOUT(kt,neval,ndsav,fnewlo,temp,dprt,autosim,nsims)  !type it
	endif
	fval(1)=fnewlo	!for test below
c this is best vertex
c
c check absmin
	fval(3)=absmin		!for test below
	if(L.eq.1) goto 1386	!not aborted
	if(fval(1).lt.fval(3)) goto 1254  !aborted- use best vertex
	goto 1387	!aborted- use absmin- no restarts
c
c next average over vertices-put values in pnew()
1386	continue
	do 1400 j = 1,k
	  pnew(j)	= 0.0
	  do 1390  i = 1,n
1390	    pnew(j)	= pnew(j) + simp(i,j)
1400	  pnew(j)	= pnew(j) / float(n)
c### copy pnew into theta here?
c=====temp debug
c	if(neval.ge.21) then
c	   pause
c	endif
	fval(2)=FUNC(kt,pnew,
     & tint,ampl,iprops,nd1,nd2,
     & Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & XAF,XFA,QexpQA,QexpQF,
     & alpha2,beta2,aloglik,kab,
     & kAm,kFm,km)
	neval=neval+1
	neval1=neval	!for common
	call DSPOUT(kt,neval,ndsav,fval(2),pnew,dprt,autosim,nsims)	!type it
	if(abort) goto 1384	!may be set true via common
c
c do local search. Temp() already contains best vertex after convergence, with
c corresp function value in fval(1). Put altered values in pnew1
c 06/05/89 11:43am retain TEMP here
	do j=1,k
	   pnew1(j)=temp(j)+crtstp(j)
c=====temp debug
c	   if(neval.ge.21) then
c		pause
c	   endif
	   fval(4)=FUNC(kt,temp,
     &	 tint,ampl,iprops,nd1,nd2,
     &	 Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     &	 XAF,XFA,QexpQA,QexpQF,
     &	 alpha2,beta2,aloglik,kab,
     &	 kAm,kFm,km)
	   neval=neval+1
	   neval1=neval	!for common
	   call DSPOUT(kt,neval,ndsav,fval(4),temp,dprt,autosim,nsims)	!type it
	   if(abort) goto 1384	!may be set true via common
	   if(fval(4).lt.fnewlo) goto 125	!restart at better value
c
	   pnew1(j)=temp(j)-crtstp(j)	!step in other direction
c=====temp debug
c	   if(neval.ge.21) then
c		pause
c	   endif
	   fval(4)=FUNC(kt,pnew1,
     &	 tint,ampl,iprops,nd1,nd2,
     &	 Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     &	 XAF,XFA,QexpQA,QexpQF,
     &	 alpha2,beta2,aloglik,kab,
     &	 kAm,kFm,km)
	   neval=neval+1
	   neval1=neval	!for common
	   call DSPOUT(kt,neval,ndsav,fval(4),pnew1,dprt,autosim,nsims)	!type it
	   if(abort) goto 1384	!may be set true via common
	   if(fval(4).lt.fnewlo) goto 125	!restart at better value
	enddo
c if arrive here without restart theta is unchanged best vertex
C	print 454
C454	format( ' local search does not improve on theta ')
	fval(4)=fval(1)		!restore orig func value
C
c Test which is best. Want to restart at thmin (fval(3))if this is best
c or restart at pnew1 (fval(4))if this is best. Otherwise exit with theta
c (fval(1)) or with average (fval(2)), whichever is better. For restart
c new initial values must be in Temp, and step reduced to crtstp.
125	continue
c find best value
	il=1
	do i=1,4
	   if(fval(i).lt.fval(il)) il=i
	enddo
	if(istart.lt.nresmax.and.(il.eq.3.or.il.eq.4)) then
	   if(il.eq.3) goto 1252	!restart at thmin
	   if(il.eq.4) goto 1253	!restart at pnew1
	else
	   if(il.eq.1) goto 1254	!exit with best vertex=temp
	   if(il.eq.2) goto 1255	!exit with average
	   if(il.eq.3) goto 1387	!exit with thmin
	   if(il.eq.4) goto 1388	!exit with pnew1
	endif
c
1252	if(.not.silent) print 1256
1256	format( ' Restart at absmin')
	irestrt=1		!signals restart at absmin
	istart=istart+1	!count number of restarts
	do j=1,k
	   temp(j)=thmin(j)
	enddo
	goto 1258
c
1253	if(.not.silent) print 1259
1259	format( ' Restart after local search')
	irestrt=2		!signals restart after local search
	istart=istart+1	!count number of restarts
	do 1260 j=1,k
1260	temp(j)=pnew1(j)
c
1258	continue
c NB crtstp (but not crtstp1) is log value even for iconv=2
c	if(logfit.and.iconv.eq.2) then	!crtstp is non-log for iconv=2
c	   do j=1,k
c		step(j)=resfac*crtstp(j)	!small step size for restart
c	   enddo
c	else
	   do j=1,k
		step(j)=resfac*crtstp(j)	!small step size for restart
	   enddo
c	endif
	goto 2001	!restart
c
1254	if(.not.silent) print 1262
1262	format(' Return with best vertex')
	iconv=2				!signals 'return with best vertex'
	call mcopy(temp,theta,kt)		!copy temp into theta for output
	fmin=fval(1)
 	RETURN
c
1255	if(.not.silent) print 1263
1263	format(' Return with averaged vertices')
	iconv=3				!signals 'return with average vertex'
	call mcopy(pnew,theta,kt)		!copy pnew into theta for output
	fmin=fval(2)
 	RETURN
c
1387	if(.not.silent) print 13871
13871	format(' Return with absmin')
	iconv=4				!signals 'return with absmin'
	call mcopy(thmin,theta,kt)		!copy thmin into theta for output
	fmin=fval(3)
 	RETURN
c
1388	continue
	if(.not.silent) print 1389
1389	format(' Return with result of local search')
	iconv=5				!signals 'return with pnew1'
	call mcopy(pnew1,theta,kt)	!pnew1 into theta for output
	fmin=fval(4)
	RETURN
c
1510	if(.not.silent) print 1452,nevmax
1452	format(' *** ERROR no convergence after',i8,' evaluations')
	iconv=6				!signals 'no convergence'
	RETURN

	END

	subroutine MCOPY(a,b,k)
	real*8 a(200),b(200)
c Copies A to B (for fast version)
	do 1 i=1,k
1	b(i)=a(i)
	return
	end


	subroutine RCOPY(I,simp,A,K)
	real*8 A(200),simp(201,200)
c
c copies row I of simp() into A()
	do 1 J=1,K
1	A(J)=simp(I,J)
	return
	end

	subroutine NWVRTX(I,A,simp,K)
	real*8 A(200),simp(201,200)
c
c copies A() into row I of simp()
	do 1 J=1,K
1	simp(I,J)=A(J)
	return
	end


	subroutine DSPOUT(k,Neval,ndsav,fmin,TH,dprt,autosim,nsims)
C TO DO PRINTING in simpHJC
	real*8 TH(200),fmin,th1(200)
	logical dprt
	real*8 setlik(10)		!likelihood for each data set (e.g.conc)
	logical autosim
	common/sets/setlik,nset2		!for dspout in simphjc
c   common with qset_hjc so ec50 can be printed during iterations
	logical fixec51
	real*8 qval,ec501,ec50out,x1s,x2s,fx1,fx2
	real*8 qval2,ec5012,ec50out2,x1s2,x2s2,fx12,fx22
	common/ec1/nfixec51,fixec51,qval,ec501,ec50out,i501,j501,x1s,x2s,
     &fx1,fx2,qval2,ec5012,ec50out2,i5012,j5012,x1s2,x2s2,fx12,fx22  !for print in SIMPHJC
	character*10 titpfree(200)
	common/tpf/titpfree,iptit	!common with hjcfit
c
	logical logfit
	common/logf/logfit
c For penalty function
	logical penalty
	real*8 penfunc,penfac
	common/pen/penalty,penfunc,penfac 	!for hjclik, qset_hjc
c
	nset=nset2
	if(nset.eq.1) then
	   print 1,neval,-fmin
	   if(dprt) then
		write(8,1) neval,-fmin
1		format(' n = ',i8,'  L = ',g14.7)
	   endif
	else if(nset.gt.1) then
	   if(nset.le.3) then
		print 2,neval,-fmin,(setlik(j),j=1,nset)
c		if(dprt) write(8,2) neval,-fmin,(setlik(j),j=1,nset)
		if(dprt) write(8,3) neval,-fmin,(setlik(j),j=1,nset)
2		format(i8,' L=',g14.7,' (each set = ',3g13.6,')')
3		format(i8,' L=',56x,g14.7,' (each set = ',3g13.6,')')
	   else
		print 21,neval,-fmin,(setlik(j),j=1,nset)
c		if(dprt) write(8,21) neval,-fmin,(setlik(j),j=1,nset)
		if(dprt) write(8,211) neval,-fmin,(setlik(j),j=1,nset)
21		format(i8,' L=',g14.7,' (each set = ',/,10g13.6,')')
211		format(i8,' L=',56x,g14.7,' (each set = ',/,10g13.6,')')
	   endif
	endif
	if(autosim) then
	   print 22,nsims
22	   format('&  Sim # ',i4)
	endif
c
	do m=1,k
	   if(logfit) then
		th1(m)=dexp(th(m))	!non-log for printing: th1() is local
	   else
		th1(m)=th(m)
	   endif
	enddo
c Try printing names separately, every 5 printouts
	iptit=iptit+1
	if(mod(iptit,4).eq.0) then
	   print 13,(titpfree(i),i=1,k)
	   if(dprt) write(8,13) (titpfree(i),i=1,k)
13	   format(30(6(2x,a10,1x),/))
	endif
c one version with titles
c	print 11,(titpfree(i)(1:7),th1(i),i=1,k)
c	if(dprt) write(8,11) (titpfree(i)(1:7),th1(i),i=1,k)
c11	format(25(4(1x,a7,'=',g11.4),/))
	if(k.le.6) then
	   print 11,(th1(m),m=1,k)
	   if(dprt) write(8,11) (th1(m),m=1,k)
11	   format(6g13.6)
	else
	   print 10,(th1(m),m=1,k)
	   if(dprt) write(8,10) (th1(m),m=1,k)
10	   format(30(6g13.6,/))
	endif
	if(fixec51) then
	 do ifix=1,nfixec51
	  if(ifix.eq.1) then
	    print 12,i501,j501,qval,ec50out,ec501,x1s,x2s
	    if(dprt) write(8,12) i501,j501,qval,ec50out,ec501,x1s,x2s
12	    format(' Q(',i2,',',i2,') = ',g13.6,' gives EC50 = ',f9.3,
     &	' (should be ',f9.3,')',/,'    (guesses were ',2g13.6,')')
	  else if(ifix.eq.2) then

	     print 123,i5012,j5012,qval2,ec50out2,ec5012,x1s2,x2s2
	     if(dprt) write(8,123) i5012,j5012,qval2,ec50out2,
     &	ec5012,x1s2,x2s2
123	     format(' Q(',i2,',',i2,') = ',g13.6,' gives EC50 = ',f9.3,
     &	' (should be ',f9.3,')',/,'    (guesses were ',2g13.6,')')
	  endif
	  if(penalty) then
		print 121,penfunc
		if(dprt) write(8,121) penfunc
121		format('&  Penalty = ',f9.3)
	  else
		print 122
		if(dprt) write(8,122)
122		format('&  No penalty ')
	  endif
	 enddo
c
	endif
c
c TEMPORARY DEBUG
c	if(neval.gt.500) then
c	   pause
c	endif
C
	NDSAV=NEVAL
	RETURN
	END


	subroutine abstst(fval,pnew,absmin,thmin,k)
	real*8 pnew(200),thmin(200),fval,absmin
c
	if(fval.gt.absmin) return	!fval no better
	absmin=fval
	do 1 j=1,k
1	thmin(j)=pnew(j)
	return
	end


c	subroutine simdsp(simp,fval,k)
c	real*8 simp(201,200),fval(201)
c to print whole simplex for debug
c	do 1 i=1,k+1
c1	print 2,i,(simp(i,j),j=1,k),fval(i)
c2	format(i8,4g13.6)
c	return
c	end



	subroutine sqz(a,b,jfix,k)
	real*8 a(200)
	real*8 b(200)
	integer jfix(200)
c
c squeeze a into b ( a and b could be same)
	j=0
	do 1 i=1,k
	if(jfix(i).eq.1) goto 1
	j=j+1
	b(j)=a(i)
1	continue
	return
	end

	subroutine unsqz(a,b,jfix,k)
	real*8 a(200)
	real*8 b(200)
	integer jfix(200)
c restore b into a (a and b must be different)
	j=0
	do 3 i=1,k
	if(jfix(i).eq.1) goto 3
	j=j+1
	a(i)=b(j)
3	continue
	return
	end



