	subroutine SIMPLEX4(kt,THETA,stpfac,errfac,neval,nevmax,
     & fmin,FUNC,Ndisp,jfix,delmin,confac,irestrt,iconv,
     & lowval,highval,pertval)
c
c Use F1-F10 to abort
c SIMPLEX4 is version of SIMPLEX3 that makes attempt to prevent
c  the simplex flattening itself against a boundary when a parameter
c  is reset to a boundary value in FUNC.  If this happens then
c  one of the vertices is moved AWAY from the boundary by an
c  increment pertval(j), for jth parameter.  This must be specified
c  as input for all parameters that get reset to a limit within FUNC
c  and the values for the limit must also be specified in lowval(j),
c  highval(j) (may have both lower and upper limit on any parameter)
c  NB lowval(i), highval(i) are numbered 1=1,..,kt -include ALL parameters,
c	but simp(i,j) is based on k=kt-nfix parameters only.  Therefore,
c	if some param are fixed, define correspondence via array jnum(j)
c	where jnum(j)=orig state number (j=1,..,k; state # selected from
c	1,..,kt)
C  Also now returns # of the F key hit (F1-F10) to abort as ikey in
c  the now-expanded common/abt/abort,ikey
c
c  Modif 06/18/99 09:35am to take any number of parameters (NB have to put
c  allocated dimensions of arrays into all subroutine calls)
c
c SIMPLEX3 is version of SIMPLEX that has the same modifications as in
c SIMPLEX2 (silent version for SCAN) as follows:
c
c Modif 06/04/95 07:40am by aading common/prt/idisp,disp added such that
c  if idisp.ne.0 then extra call to FUNC is made, with disp=true, each
c  time an iteration is printed (disp=false otherwise) -can then use
c  value of disp to print out additional about each iteration in FUNC.
c Modif 08/03/94 03:09pm so that input value of IRESTRT is taken as
c maximum number of restarts allowed.  Also, when this number of restarts
c has been done, the program is left with the best of all four options
c At present, leave when (1) best vertex, or (2) average vertex is best, but
c if (3) absmin or (4) pnew1 is best then a restart is done).  Now, if (1) or
c (2) is best so no restarts at all are done (or if irestrt=0) then
c leave with the best of all 4 options.
c NB confac,irestrt,iconv added to parameters!
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
c Modif 06/08/95 03:44pm to 5*k successive iterations
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
c New for simplex4
	logical flat(50)		!internal
	real*4 lowval(50),highval(50),pertval(50)		!parameter
c
	real theta(kt)
	integer jfix(kt)
	allocatable:: step,crtstp,temp,centre
	allocatable:: pnew,pnew1,fval,thmin,simp
	real*4 step(:),crtstp(:),temp(:),centre(:)
	real*4 pnew(:),pnew1(:),fval(:),thmin(:),simp(:,:)
c	real theta(20)
c	dimension step(20),crtstp(20)
c	dimension temp(20),centre(20)
c	dimension pnew(20),pnew1(20),fval(21),thmin(20),simp(21,20)
c	integer jfix(20)
	allocatable:: jnum
	integer jnum(:)        !to translate state numbering
c	integer jnum(20)        !to translate state numbering
c	logical pp	!to print progress
	logical silent,debtyp		!no print out at all; or only if caplock on
	logical deb,debug,caplock
	integer*2 ktype
	logical KBHIT		!Utility lib
	character ch,getch
	logical abort
	logical discprt,dprt,disp
	common/prt/idisp,disp
	common/dp/discprt
	common/abt/abort,ikey
	external FUNC
c
c
c	debug()=caplock()
c	deb=caplock()
	deb=.false.
	abort=.false.
	disp=.false.		!for common
c
	ALLOCATE(step(kt),crtstp(kt),temp(kt),centre(kt),jnum(kt))
	ALLOCATE(pnew(kt),pnew1(kt),thmin(kt))
	n1=kt+1
	if(n1.lt.4) n=4
	ALLOCATE(fval(n1),simp(kt+1,kt))
c	real theta(20)
c	dimension step(20),crtstp(20)
c	dimension temp(20),centre(20)
c	dimension pnew(20),pnew1(20),fval(21),thmin(20),simp(21,20)
c
	call UNDER0('true')	!so underflows set=0 (Lahey manual p12.8)
	silent=ndisp.eq.-1000
	debtyp=ndisp.lt.0
	dprt=discprt.and.(nevmax.gt.1)	!iterations printed to disc
	ndisp1=iabs(ndisp)     		!so input ndisp not changed
	nevmax=iabs(nevmax)		!always pos on output
	if(silent) ndisp1=0
	if(.not.silent) print 31
31	format(' F1-F10 key to ABORT',/,'  -calculating...')
	call CLRKB		!Remove any waiting keyboard char: Utility lib
c
	nresmax=irestrt	!save input value=max number of restarts
	istart=0		!counts number of restarts
c
C Section to deal with fixed values
c Fixed param stay in their correct positions in theta
c
c	if(debug()) print 750,0,(jfix(i),i=1,8),(theta(i),i=1,8)
c750	format(' pos #',i2,': ',8i4,/,8f10.3)
	call sqz(theta,temp,jfix,kt,kt)	!squeeze theta into temp
c
c Count k=no of param to be fitted, and set STEP(), CRTSTP().
	k=0
	do j=1,kt
	   if(jfix(j).eq.0) then
		k=k+1
		step(k)=stpfac*temp(k)
		crtstp(k)=errfac*temp(k)
		jnum(k)=j
	   endif
	enddo
c
	do j=1,k
	   flat(j)=.false.
	enddo
c
c Set ndel=no of consec iterations for which reduction in func<delmin
c for convergence
c	ndel=5
	ndel=5*k
	idel=0		!counts up to ndel
c
	if(k.eq.kt) goto 3000	!no fixed parameters so use fast version
c
c	pp=.false.
c	if(ndisp.lt.0) pp=.true.
c	ndisp=iabs(ndisp)
	reffac=1.0		! reflection coeff => 1
c	confac=0.5		! contraction coeff 0 < beta < 1
	extfac=2.0		! extension factor > 1
	n=k+1		! # of vertices in simplex
	neval=0			!counts function evaluations
c
c
1001	continue	!return here for restart
	call NWVRTX(1,temp,simp,k,kt)	!start values=vertex #1
	call unsqz(theta,temp,jfix,kt,kt)	!unsqueeze temp into theta for func
c	if(debug()) print 750,1,(jfix(i),i=1,8),(theta(i),i=1,8)
	fval(1)=func(kt,theta)		!func value for these
	fsav=fval(1)
	call sqz(theta,temp,jfix,kt,kt)	!restore
	neval=neval+1
	absmin=fval(1)		!starting value
	do i=1,k
	   thmin(i)=theta(i)	!start value if theta corresp to absmin
	enddo
c	call DSPOUT(kt,neval,ndsav,fsav,theta,dprt,kt)	!type it
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
	  I1	= i
	  call RCOPY(I1,SIMP,temp,k,kt)
	  call unsqz(theta,temp,jfix,kt,kt)	!unsqueeze temp into theta for func
	  fval(i1)=func(kt,theta)		!func value for these
	  call sqz(theta,temp,jfix,kt,kt)	!restore
	  call abstst(fval(i1),temp,absmin,thmin,k,kt)	!test for abs min
	enddo
	neval=neval+K
c
c ----- display starting simplex
c	print 1021
c1021	format (/' Starting simplex (with func values):')
c	call simdsp(simp,fval,k,kt)
c
c
c ----- start iteration loop here at 1000
c
	ndsav=0
	niter	= 0
1000	continue
	niter	= niter + 1
c
C find best (lowest fval) and worst (highest fval) vertices- indices
c of these are ILO,IHI respectively. fnewlo=fval(IHI) corresponds to
c the vertex to be replaced
	flo=fval(1)
	fnewlo=flo
	ILO=1
	IHI=1
	do i=2,n
	   if(fval(i).lt.flo) then
		flo=fval(i)
		ilo=i
	   endif
	   if(fval(i).gt.fnewlo) then
		fnewlo=fval(i)
		ihi=i
	   endif
	enddo
c	print 100,ihi,ilo,niter,neval
c100	format( 'ihi,ilo,niter ,neval= ',4i8)
c
c display current best vertex
c	if(silent.or.ndisp1.eq.0.or.(.not.debug().and.debtyp)) goto 360
	if(silent.or.ndisp1.eq.0.or.(.not.deb.and.debtyp)) goto 360
	if((neval-ndsav).lt.ndisp1) goto 360
c	if(pp) print 176
c176	format(/)
	CALL RCOPY(ILO,simp,temp,k,kt)	!copy best vertex to temp
	call unsqz(theta,temp,jfix,kt,kt)	!unsqueeze temp into theta for display
	if(idisp.ne.0) then
	   disp=.true.		!for common
	endif
	fdum=func(kt,theta)		!to print extra values in FUNC
	disp=.false.		!for common
	call DSPOUT(kt,neval,ndsav,flo,theta,dprt,kt)	!type it
c	if(idisp.ne.0) then
c	   disp=.true.		!for common
c	   fdum=func(kt,theta)		!to print extra values in FUNC
c	   disp=.false.		!for common
c	endif
360	continue
c	if(debug()) print 750,niter,(jfix(i),i=1,8),(theta(i),i=1,8)
c	if(neval.gt.nevmax) goto 510
	if(neval.gt.nevmax) then
	   if(.not.silent) print 452,nevmax
	   if(discprt) write(8,452) nevmax
452	   format(' *** Reached maximum number of',i8,' evaluations')
	   iconv=6				!signals 'no convergence'
	   goto 385
c======125 is in FAST section!
c	   goto 125	!define best fmin etc
	endif
c
c ----- compute centroid of all vertices except the worst
c
	do j=1,k
	  centre(j)= 0.0
	enddo
	do i=1,n
	   if(i.ne.ihi) then
		do j=1,k
		   centre(j)=centre(j) + simp(i,j)
		enddo
	   endif
	enddo
c
c Check whether simplex has flattened against a boundary
	do j=1,k	!check each parameter in turn
	   s=simp(1,j)
	   ieq=0
	   do i=2,n
		if(simp(i,j).eq.s) then
		   ieq=ieq+1
		endif
	   enddo
	   if(ieq.eq.n-1) then		!===or 'approx equal'?
		if(.not.flat(j)) then	!changed
		   print 71,jnum(j)
71		   format(' Simplex flattened for parameter #',i3)
		endif
		flat(j)=.true.			!parameter j is flattened
	   else
		if(flat(j)) then	!changed
		   print 72,jnum(j)
72		   format(' Simplex now OK for parameter #',i3)
		endif
		flat(j)=.false.			!parameter j is not flattened
	   endif
	enddo
c
c ----- reflect, with next vertex taken as reflection of worst
c Parameter values that are coord of new vertex in pnew(j)
c  If simplex has flattened for param #j, then all simp(i,j) same
c (=boundary value), and centre(j) is the same as this value
c so no movement produced by reflection
	do j=1,k
	   j1=jnum(j)		!orig state number (up to kt)
	   centre(j)=centre(j)/float(K)
	   if(flat(j)) then
		if(simp(ihi,j).eq.lowval(j1)) then	!======='approx equal'?
		   pnew(j)=lowval(j1) + abs(pertval(j1))
		else if(simp(ihi,j).eq.highval(j1)) then
		   pnew(j)=highval(j1) - abs(pertval(j1))
		endif
	   else
		pnew(j)=centre(j) - reffac*(simp(IHI,J)-centre(J))
	   endif
	enddo
	call unsqz(theta,pnew,jfix,kt,kt)	!unsqueeze pnew into theta for func
	fnew=func(kt,theta)			!fu5nc value for these
	call sqz(theta,pnew,jfix,kt,kt)	!restore
	neval=neval+1
	call abstst(fnew,pnew,absmin,thmin,k,kt)	!test for abs min
c	if(pp) print 170
c170	format(' R ')
c	print 101,(pnew(j),j=1,k),fnew
c101	format(' reflected. pnew,fnew= ',/,11g13.6)
c -----
	if (fnew.ge.flo) goto 12	!fnew worst than prev best
c
c ----- new vertex is better than previous best
c ----- extend it
c
	do j=1,k
	  pnew1(j)=centre(j) + extfac*(pnew(j)-centre(j))
	enddo
	call unsqz(theta,pnew1,jfix,kt,kt)	!unsqueeze pnew1 into theta for func
	fnew1=func(kt,theta)			!func value for these
	call sqz(theta,pnew1,jfix,kt,kt)	!restore
	neval=neval+1
	call abstst(fnew1,pnew1,absmin,thmin,k,kt)	!test for abs min
c ----- still better?
c	print 102,(pnew1(j),j=1,k),fnew1
c102	format(' extended. pnew1,fnew1= ',/,11g13.6)
	if (fnew1.ge.fnew) goto 19	! no- input pnew and test conv
c	if(pp) print 172
c172	format(' E ')	!successful expansion
c
c ----- yes- so extension (pnew1) inputed (or contraction inputed if
c	 arrive at 10 via 15 below)
c
10	continue
	call NWVRTX (IHI,pnew1,simp,K,kt)
	fval(IHI)=fnew1
	neval=neval+1
	call abstst(fnew1,pnew1,absmin,thmin,k,kt)	!test for abs min
c	print 103
c103	format( ' pnew1 inputed at label 10')
c	call simdsp(simp,fval,k,kt)
	goto 901		!convergence check
c
c come to 12 if reflected vertex not better than best vertex, so
c no extension wanted
12	continue
c ----- Is reflected vertex better than worst vertex? L=the
c number of vertices which are worse than fnew=number of vertices
c that fnew is better than.
	L=0
	do 13 I=1,n
13	if(fval(I).gt.fnew) L=L+1
c	if(pp) print 104,L
c104	format(' L=',i1,1x,)
c	print 1041,L
c1041	format(' L= ',i4)
	if(L.ge.2) goto 19	!reflected vertex better than 2 or more
c				! so input pnew and test conv
c	if(L.ge.1) goto 19	!test** more like other version? no good!
	if(L.eq.0) goto 15	!reflected vertex is better than none i.e.
c				!worse than all so contract on original
c				!(unreflected) side
c
c Get here if L=1 i.e. fnew better than one (the worst) vertex so
c input unextended reflection, pnew, then contract on new (reflected) side
c (after vertex ihi replaced by reflected vertex loop at 270 does this)
c
	call NWVRTX(IHI,pnew,simp,K,kt)
	fval(IHI)=fnew
c	print 105
c105	format(' pnew inputed')
c	call simdsp(simp,fval,k,kt)
c	print 106
c106	format(' contract on reflected side. pnew1=',/)
c	if(pp) print 173
c173	format(' C2 ')
c	if(pp) goto 107
c	goto 107	!to avoid print at 108
c
15	continue
c Contract on the original fval(IHI) side of the centroid
c
c	print 108
c108	format(' contract on unreflected side. pnew1,fnew1= ',/)
c	if(pp) print 174
c174	format(' C1 ')
107	continue
	do j=1,k
	   j1=jnum(j)		!orig state number (up to kt)
	   if(flat(j)) then
		if(simp(ihi,j).eq.lowval(j1)) then	!======='approx equal'?
		   pnew1(j)=lowval(j1) + abs(pertval(j1))
		else if(simp(ihi,j).eq.highval(j1)) then
		   pnew1(j)=highval(j1) - abs(pertval(j1))
		endif
	   else
		pnew1(j)=centre(j) + confac*(simp(IHI,J)-centre(J))
	   endif
	enddo
	call unsqz(theta,pnew1,jfix,kt,kt)	!unsqueeze pnew1 into theta for func
	fnew1=func(kt,theta)			!func value for these
	call sqz(theta,pnew1,jfix,kt,kt)	!restore
	neval=neval+1
	call abstst(fnew1,pnew1,absmin,thmin,k,kt)	!test for abs min
c	print 1022,(pnew1(j),j=1,k),fnew1
c1022	format(11g13.6)
c
c ----- is contracted vertex better than the worst vertex
	if(fnew1.le.fval(IHI)) goto 10
c
c
c ----- no, it is still bad, shrink whole simplex towards best vertex
c
c In next bit check whether, for any param (j) all vertices of the
c the simplex have the same function value, i.e. simplex has flattened
c against a boundary for this parameter.  The contraction is determined
c by (simp(i,j)-simp(ilo,j)), which will be zero for i=ilo, but when
c simplex has flattened against a boundary, all simp(i,j) are equal (to
c the boundary value) so (simp(i,j)-simp(ilo,j)) is always zero for all
c i values (i.e. ilo is not really defined) and process gets stuck.
c In version below vertex #1 (only) is moved away from the boundary
c (no obvious reason to perturb any particular vertex so use #1)
c/	if(flat(j)) then
c/	   j1=jnum(j)		!orig state number (up to kt)
c/	   if(simp(ihi,j).eq.lowval(j1)) then	!======='approx equal'?
c/		   pnew1(j)=lowval(j1) + abs(pertval(j1))
c/	   else if(simp(ihi,j).eq.highval(j1)) then
c/		   pnew1(j)=highval(j1) - abs(pertval(j1))
c/	   endif
c/	else
c/	   pnew1(j)=centre(j) + confac*(simp(IHI,J)-centre(J))
c/	endif
c If simplex flattens agains boundary, add perturbation to one vertex
	do i=1,n
	   do j=1,k
	      j1=jnum(j)		!orig state number (up to kt)
		if(flat(j).and.i.eq.1) then
		   if(simp(i,j).eq.lowval(j1)) then	!======='approx equal'?
			simp(i,j)=lowval(j1) + abs(pertval(j1))
		   else if(simp(ihi,j).eq.highval(j1)) then
			simp(i,j)=highval(j1) - abs(pertval(j1))
		   endif
		else
		   simp(i,j)=simp(ILO,J) + confac*(simp(i,j)-simp(ilo,j))
		endif
		temp(j)=simp(i,j)
	   enddo
	   call unsqz(theta,temp,jfix,kt,kt)	!unsqueeze temp into theta for func
	   fval(i)=func(kt,theta)		!func value for these
	   call sqz(theta,temp,jfix,kt,kt)	!restore
	   call abstst(fval(i),temp,absmin,thmin,k,kt)	!test for abs min
	enddo
	neval=neval+n
c	if(pp) print 175
c175	format(' S ')
c	print 110,ilo
c110	format( ' shrink towards vertex ',i8)
c	call simdsp(simp,fval,k)
	goto 901	!converged?
c
c Retain reflection
19	continue
	call NWVRTX(IHI,pnew,simp,K,kt)
	fval(IHI)=fnew
c	print 111
c111	format(' input pnew')
c	call simdsp(simp,fval,k)
c
c
901	continue
c	if(pp) print 171
c171	format(' T ')
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
	if(delmin.lt.0.) goto 80	!use normal convergence check
c Use delmin criterion- find f at current best vertex
	il=1
	do i=1,n
	   if(fval(i).lt.fval(il)) il=i
	enddo
	del=fsav-fval(il)	!reduction in min=pos value
c	print 16,neval,fsav,fval(il),del,delmin,(fval(i),i=1,n)
	IF(DEL.LT.-1.E-5) GOTO 84	!MIN IS INCREASING! DEL NEGATIVE
	if(del.gt.delmin) goto 83
c Section when DEL < DELMIN
	idel=idel+1
	if(idel.lt.ndel) goto 831	!not yet converged
	if(.not.silent) print 85
85	FORMAT( ' Converged via DELMIN ',/)
c	print 16,neval,fsav,fval(il),del,delmin,(fval(i),i=1,n)
c16	format(' neval,fsav,fval(il),del,delmin= ',/,i8,4g13.6,/,
c    &	' fval(i)= ',6g13.6,/,5g13.6,/,5g13.6,/,5g13.6)
	L=2		!converged
	GOTO 84
C I.E. DEL POSITIVE. IF <DELMIN FINISH
83	idel=0			!reset when del > delmin
831	fsav=fval(il)		!SAVE LAST MINIMUM
	goto 84
c
80	continue
	L=1		!conv via crtstp
	do j=1,k	!test each parameter
	   IL=1
	   IH=1
	   do i=1,n	!order values of current param
		if(simp(i,j).lt.simp(IL,j)) IL=i
		if(simp(i,j).gt.simp(IH,j)) IH=i
	   enddo
	   if((simp(IH,j)-simp(IL,j)).gt.abs(crtstp(j))) L=0	!not conv
c	   print 3711,simp(ih,j),simp(il,j),simp(ih,j)-simp(il,j),crtstp(j)
c3711	   format(' convergence test',/,4g13.6)
	enddo
c
84	continue
c
383	continue
c NB key F1 has ktype=0, scan code=59 (see \fortran\tkey.for)
	if(abort) goto 384	!may be set true via common
	if(KBHIT()) then
	   ch=GETCH(ktype)
	   ival=ichar(ch)
	   if(ktype.eq.0.and.ival.ge.59.and.ival.le.68) then
		ikey=ival-58	!set number of F key in common
		goto 384	!abort
	   endif
	endif
c
	if (L.eq.0) goto 1000	!next iteration
	GOTO 385
384	print 3841
	if(discprt) write(8,3841)
3841	format( ' RUN ABORTED')
	L=3	!abort
385	CONTINUE
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
	do 23 j=1,k
23	temp(j)=simp(ihi,j)
	fnewlo=fval(ihi)
C	print 451
C451	format( ' best vertex: ')
c Following line was commented out, but surely want to display best
c vertex here
	call unsqz(theta,temp,jfix,kt,kt)  !unsqueeze temp into theta for display
	if(.not.silent) then
	   if(idisp.ne.0) then
		disp=.true.		!for common
	   endif
	   fdum=func(kt,theta)		!to print extra values in FUNC
	   disp=.false.		!for common
	   call DSPOUT(kt,neval,ndsav,fnewlo,theta,dprt,kt)  !type it
c	   if(idisp.ne.0) then
c		disp=.true.		!for common
c		fdum=func(kt,theta)		!to print extra values in FUNC
c		disp=.false.		!for common
c	   endif
	endif
	fval(1)=fnewlo	!for test below
c this is best vertex
c
c check absmin
C	print 453
C453	format( ' absmin: ')
C	call unsqz(theta,thmin,jfix,kt,kt)	!unsqueeze thmin to theta for display
c	call DSPOUT(kt,neval,ndsav,absmin,theta,dprt,kt)	!type it
	fval(3)=absmin		!for test below
	if(L.eq.1) goto 386	!not aborted
	if(fval(1).lt.fval(3)) goto 254  !aborted- use best vertex
	goto 387	!aborted- use absmin- no restarts
c
c next average over vertices-put values in pnew()
386	continue
	do 400	j = 1,k
	  pnew(j)	= 0.0
	  do 390  i = 1,n
390	    pnew(j)	= pnew(j) + simp(i,j)
400	  pnew(j)	= pnew(j) / float(n)
	call unsqz(theta,pnew,jfix,kt,kt)	!unsqueeze pnew into theta for func
	fval(2)=func(kt,theta)		!func value for these
	call sqz(theta,pnew,jfix,kt,kt)	!restore
C	print 450
C450	format( ' averaged values: ')
C	call unsqz(theta,pnew,jfix,kt,kt)	!unsqueeze pnew into theta for display
c	call DSPOUT(kt,neval,ndsav,fval(2),theta,dprt,kt)	!type it
c
c do local search. Temp() already contains best vertex after convergence, with
c corresp function value in fval(1). Put altered values in pnew1
	do 24 j=1,k
	pnew1(j)=temp(j)+crtstp(j)
	call unsqz(theta,pnew1,jfix,kt,kt)	!unsqueeze pnew1 into theta for func
	fval(4)=func(kt,theta)		!func value for these
	call sqz(theta,pnew1,jfix,kt,kt)	!restore
	if(fval(4).lt.fnewlo) goto 25	!restart at better value
	pnew1(j)=temp(j)-crtstp(j)	!step in other direction
	call unsqz(theta,pnew1,jfix,kt,kt)	!unsqueeze pnew1 into theta for func
	fval(4)=func(kt,theta)		!func value for these
	call sqz(theta,pnew1,jfix,kt,kt)	!restore
	if(fval(4).lt.fnewlo) goto 25	!restart at better value
24	continue
c if arrive here without restart theta is unchanged best vertex
C	print 454
C454	format( ' local search does not improve on theta ')
	fval(4)=fval(1)		!restore orig func value
C
c Test which is best. Want to restart at thmin (fval(3))if this is best
c or restart at pnew1 (fval(4))if this is best. Otherwise exit with theta
c (fval(1)) or with average (fval(2)), whichever is better. For restart
c new initial values must be in Temp, and step reduced to crtstp.
25	continue
c find best value
	il=1
	do i=1,4
	   if(fval(i).lt.fval(il)) il=i
	enddo
	if(istart.lt.nresmax.and.(il.eq.3.or.il.eq.4).and.
     &	neval.le.nevmax) then
	   if(il.eq.3) goto 252	!restart at thmin
	   if(il.eq.4) goto 253	!restart at pnew1
	else
	   if(il.eq.1) goto 254	!exit with best vertex=temp
	   if(il.eq.2) goto 255	!exit with average
	   if(il.eq.3) goto 387	!exit with thmin
	   if(il.eq.4) goto 388	!exit with pnew1
	endif
c
252	if(.not.silent) print 256
256	format( ' Restart at absmin')
	irestrt=1		!signals restart at absmin
	istart=istart+1	!count number of restarts
	do 257 j=1,k
257	temp(j)=thmin(j)
	goto 258
c
253	if(.not.silent) print 259
259	format( ' Restart after local search')
	irestrt=2		!signals restart after local search
	istart=istart+1	!count number of restarts
	do 260 j=1,k
260	temp(j)=pnew1(j)
c
258	do 261 j=1,k
261	step(j)=crtstp(j)	!small step size for restart
	goto 1001	!restart
c
254	if(.not.silent) print 262
262	format( ' Return with best vertex')
	iconv=2				!signals 'Return with best vertex'
	call unsqz(theta,temp,jfix,kt,kt)	!unsqueeze temp into theta for output
	fmin=fval(1)
	DEALLOCATE(step,crtstp,temp,centre,pnew,pnew1,fval,thmin,simp)
	DEALLOCATE(jnum)
 	RETURN
c
255	if(.not.silent) print 263
263	format( ' Return with averaged vertices')
	iconv=3				!signals 'Return with averaged vertices'
	call unsqz(theta,pnew,jfix,kt,kt)	!unsqueeze pnew into theta for output
	fmin=fval(2)
	DEALLOCATE(step,crtstp,temp,centre,pnew,pnew1,fval,thmin,simp)
	DEALLOCATE(jnum)
 	RETURN
c
387	if(.not.silent) print 3871
3871	format( ' Return with absmin')
	iconv=4				!signals 'return with absmin'
	call unsqz(theta,thmin,jfix,kt,kt)	!thmin into theta for output
	fmin=fval(3)
	DEALLOCATE(step,crtstp,temp,centre,pnew,pnew1,fval,thmin,simp)
	DEALLOCATE(jnum)
 	RETURN
c
388	continue
	iconv=5				!signals 'return with pnew1'
	call unsqz(theta,pnew1,jfix,kt,kt)	!pnew1 into theta for output
	fmin=fval(4)
	DEALLOCATE(step,crtstp,temp,centre,pnew,pnew1,fval,thmin,simp)
	DEALLOCATE(jnum)
	RETURN
c
c510	if(.not.silent) print 452,nevmax
c452	format(' *** ERROR no convergence after',i8,' evaluations')
c	iconv=6				!signals 'no convergence'
c	DEALLOCATE(step,crtstp,temp,centre,pnew,pnew1,fval,thmin,simp)
c	DEALLOCATE(jnum)
c	RETURN
c
c End of general version of SIMPLEX
c Now fast version for case where there are no fixed parameters
c
3000	continue
c	if(.not.silent) print 30
c30	format(' USING FAST VERSION OF SIMPLEX')
c	ndisp=iabs(ndisp)
	reffac=1.0		! reflection coeff => 1
c	confac=0.5		! contraction coeff 0 < beta < 1
	extfac=2.0		! extension factor > 1
	n=k+1		! # of vertices in simplex
	neval=0			!counts ficntion evaluations
c
c
2001	continue	!return here for restart
	call NWVRTX(1,theta,simp,k,kt)	!start values=vertex #1
	fval(1)=func(kt,theta)		!func value for these
	fsav=fval(1)
	neval=neval+1
	absmin=fval(1)		!starting value
	do i=1,k
	   thmin(i)=theta(i)	!start value if theta corresp to absmin
	enddo
c
c ----- compute offset of the vertices of the starting simplex
	fac=(sqrt(float(n))-1.)/(float(k)*sqrt(2.))
c
c ----- specify all other vertices of the starting simplex
c
	do 1160 i=2,n
	  do 1150 j=1,k
1150	    simp (i,j)	= simp (1,j) + step(j)*fac
	  simp (i,i-1)	= simp(1,i-1) + step(i-1)*(fac+1./sqrt(2.))
c
c ----- and calculate their residuals
c
	  I1	= i
	  call RCOPY(I1,SIMP,theta,k,kt)
	fval(i1)=func(kt,theta)		!func value for these
1160	call abstst(fval(i1),theta,absmin,thmin,k,kt)	!test for abs min
	neval=neval+K
c
c ----- start iteration loop here at 2000
c
	ndsav=0
	niter	= 0
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
	do i=1,n
	   if(fval(i).lt.flo) then
		flo=fval(i)
		ilo=I
	   endif
	   if(fval(i).gt.fnewlo) then
		fnewlo=fval(i)
		ihi=i
	   endif
	enddo
c
c display current best vertex
	if(silent.or.ndisp1.eq.0.or.(.not.deb.and.debtyp)) goto 1360
	if((neval-ndsav).lt.ndisp1) goto 1360
c	if(silent.or.ndisp1.eq.0.or.((neval-ndsav).lt.ndisp1)) goto 1360
	CALL RCOPY(ILO,simp,theta,k,kt)	!copy best vertex to theta
	if(idisp.ne.0) then
	   disp=.true.		!for common
	endif
	fdum=func(kt,theta)		!to print extra values in FUNC
	disp=.false.		!for common
	call DSPOUT(kt,neval,ndsav,flo,theta,dprt,kt)	!type it
	if(idisp.ne.0) then
	   disp=.true.		!for common
c=	   fdum=func(kt,theta)		!to print extra values in FUNC
	   disp=.false.		!for common
	endif
1360	continue
c	if(neval.gt.nevmax) goto 1510
	if(neval.gt.nevmax) then
	   if(.not.silent) print 452,nevmax
	   if(discprt) write(8,452) nevmax
c452	   format(' *** Reached maximum number of',i8,' evaluations')
	   iconv=6				!signals 'no convergence'
	   goto 1385
c=======
c	   goto 25	!define best fmin etc
	endif
c
c ----- compute centroid of all vertices except the worst
c
c ----- compute centroid of all vertices except the worst
c
	do j=1,k
	  centre(j)= 0.0
	enddo
	do i=1,n
	   if(i.ne.ihi) then
		do j=1,k
		   centre(j)=centre(j) + simp(i,j)
		enddo
	   endif
	enddo
c
c Check whether simplex has flattened against a boundary
	do j=1,k	!check each parameter in turn
	   s=simp(1,j)
	   ieq=0
	   do i=2,n
		if(simp(i,j).eq.s) then
		   ieq=ieq+1
		endif
	   enddo
	   if(ieq.eq.n-1) then		!===or 'approx equal'?
		if(.not.flat(j)) then	!changed
		   print 71,jnum(j)
c71		   format(' Simplex flattened for parameter #',i3)
		endif
		flat(j)=.true.			!parameter j is flattened
	   else
		if(flat(j)) then	!changed
		   print 72,jnum(j)
c72		   format(' Simplex now OK for parameter #',i3)
		endif
		flat(j)=.false.			!parameter j is not flattened
	   endif
	enddo
c
c ----- reflect, with next vertex taken as reflection of worst
c Parameter values that are coord of new vertex in pnew(j)
c  If simplex has flattened for param #j, then all simp(i,j) same
c (=boundary value), and centre(j) is the same as this value
c so no movement produced by reflection
	do j=1,k
	   j1=jnum(j)		!orig state number (up to kt)
	   centre(j)=centre(j)/float(K)
	   if(flat(j)) then
		if(simp(ihi,j).eq.lowval(j1)) then	!======='approx equal'?
		   pnew(j)=lowval(j1) + abs(pertval(j1))
		else if(simp(ihi,j).eq.highval(j1)) then
		   pnew(j)=highval(j1) - abs(pertval(j1))
		endif
	   else
		pnew(j)=centre(j) - reffac*(simp(IHI,J)-centre(J))
	   endif
	enddo
	fnew=func(kt,pnew)			!func value for these
	neval=neval+1
	call abstst(fnew,pnew,absmin,thmin,k,kt)	!test for abs min
c -----
	if (fnew.ge.flo) goto 112	!fnew worst than prev best
c
c ----- new vertex is better than previous best so extend it
c
	do j=1,k
	  pnew1(j)=centre(j) + extfac*(pnew(j)-centre(j))
	enddo
	fnew1=func(kt,pnew1)			!func value for these
	neval=neval+1
	call abstst(fnew1,pnew1,absmin,thmin,k,kt)	!test for abs min
c ----- still better?
	if (fnew1.ge.fnew) goto 119	! no- input pnew and test conv
c
c ----- yes- so extension (pnew1) inputed (or contraction inputed if
c	 arrive at 110 via 115 below)
c
110	continue
	call NWVRTX (IHI,pnew1,simp,K,kt)
	fval(IHI)=fnew1
	neval=neval+1
	call abstst(fnew1,pnew1,absmin,thmin,k,kt)	!test for abs min
	goto 1901		!convergence check
c
c come to 112 if reflected vertex not better than best vertex, so
c no extension wanted
112	continue
c ----- Is reflected vertex better than worst vertex? L=the
c number of vertices which are worse than fnew=number of vertices
c that fnew is better than.
	L=0
	do 113 I=1,n
113	if(fval(I).gt.fnew) L=L+1
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
	call NWVRTX(IHI,pnew,simp,K,kt)
	fval(IHI)=fnew
c
115	continue
c Contract on the original fval(IHI) side of the centroid
c
1107	continue
	do j=1,k
	   if(flat(j)) then
		j1=jnum(j)		!orig state number (up to kt)
		if(simp(ihi,j).eq.lowval(j1)) then	!======='approx equal'?
		   pnew1(j)=lowval(j1) + abs(pertval(j1))
		else if(simp(ihi,j).eq.highval(j1)) then
		   pnew1(j)=highval(j1) - abs(pertval(j1))
		endif
	   else
		pnew1(j)=centre(j) + confac*(simp(IHI,J)-centre(J))
	   endif
	enddo
	fnew1=func(kt,pnew1)			!func value for these
	neval=neval+1
	call abstst(fnew1,pnew1,absmin,thmin,k,kt)	!test for abs min
c
c ----- is contracted vertex better than the worst vertex
	if(fnew1.le.fval(IHI)) goto 110
c
c
c ----- no, it is still bad, shrink whole simplex towards best vertex
c
c In next bit check whether, for any param (j) all vertices of the
c the simplex have the same function value, i.e. simplex has flattened
c against a boundary for this parameter.  The contraction is determined
c by (simp(i,j)-simp(ilo,j)), which will be zero for i=ilo, but when
c simplex has flattened against a boundary, all simp(i,j) are equal (to
c the boundary value) so (simp(i,j)-simp(ilo,j)) is always zero for all
c i values (i.e. ilo is not really defined) and process gets stuck.
c In version below vertex #1 (only) is moved away from the boundary
c (no obvious reason to perturb any particular vertex so use #1)
c/	if(flat(j)) then
c/	   j1=jnum(j)		!orig state number (up to kt)
c/	   if(simp(ihi,j).eq.lowval(j1)) then	!======='approx equal'?
c/		   pnew1(j)=lowval(j1) + abs(pertval(j1))
c/	   else if(simp(ihi,j).eq.highval(j1)) then
c/		   pnew1(j)=highval(j1) - abs(pertval(j1))
c/	   endif
c/	else
c/	   pnew1(j)=centre(j) + confac*(simp(IHI,J)-centre(J))
c/	endif
c If simplex flattens agains boundary, add perturbation to one vertex
	do i=1,n
	   do j=1,k
	      j1=jnum(j)		!orig state number (up to kt)
		if(flat(j).and.i.eq.1) then
		   if(simp(i,j).eq.lowval(j1)) then	!======='approx equal'?
			simp(i,j)=lowval(j1) + abs(pertval(j1))
		   else if(simp(ihi,j).eq.highval(j1)) then
			simp(i,j)=highval(j1) - abs(pertval(j1))
		   endif
		else
		   simp(i,j)=simp(ILO,J) + confac*(simp(i,j)-simp(ilo,j))
		endif
	      theta(j)=simp(i,j)
	   enddo
	   fval(i)=func(kt,theta)		!func value for these
	   call abstst(fval(i),theta,absmin,thmin,k,kt)	!test for abs min
	enddo
	neval=neval+n
	goto 1901	!converged?
c
c Retain reflection
119	continue
	call NWVRTX(IHI,pnew,simp,K,kt)
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
	do 181 i=1,n
181	if(fval(i).lt.fval(il)) il=i
	del=fsav-fval(il)	!reduction in min=pos value
	IF(DEL.LT.-1.E-5) GOTO 184	!MIN IS INCREASING! DEL NEGATIVE
	if(del.gt.delmin) goto 183
c Section when DEL < DELMIN
	idel=idel+1
	if(idel.lt.ndel) goto 1831	!not yet converged
	if(.not.silent) print 185
185	FORMAT( ' Converged via DELMIN ',/)
	L=2		!converged
	GOTO 84
C I.E. DEL POSITIVE. IF <DELMIN FINISH
183	idel=0			!reset when del > delmin
1831	fsav=fval(il)		!SAVE LAST MINIMUM
	goto 184
c
180	continue
	L=1		!conv via crtstp
	do 1370 j=1,k	!test each parameter
	IL=1
	IH=1
	do 1371 i=1,n	!order values of current param
	if(simp(i,j).lt.simp(IL,j)) IL=i
1371	if(simp(i,j).gt.simp(IH,j)) IH=i
	if((simp(IH,j)-simp(IL,j)).gt.abs(crtstp(j))) L=0	!not conv
1370	continue
c
c
c ----- change values for reffac,confac,extfac
c
184	continue
c
c ----- abort on F1
c
1383	continue
	if(abort) goto 1384	!may be set true via common
	if(KBHIT()) then
	   ch=GETCH(ktype)
	   ival=ichar(ch)
	   if(ktype.eq.0.and.ival.ge.59.and.ival.le.68) then
		ikey=ival-58	!set number of F key in common
		goto 1384		!abort
	   endif
	endif
c
	if (L.eq.0) goto 2000	!next iteration
	GOTO 1385
1384	if(.not.silent) then
	   print 3841
	   if(discprt) write(8,3841)
c#3841	format( ' RUN ABORTED')
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
	   if(idisp.ne.0) then
		disp=.true.		!for common
	   endif
	   fdum=func(kt,theta)		!to print extra values in FUNC
	   disp=.false.		!for common
	   call DSPOUT(kt,neval,ndsav,fnewlo,temp,dprt,kt)  !type it
		if(idisp.ne.0) then
		disp=.true.		!for common
c		fdum=func(kt,theta)		!to print extra values in FUNC
		disp=.false.		!for common
	   endif
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
c#	call unsqz(theta,pnew,jfix,kt,kt)	!unsqueeze pnew into theta for func
c#	fval(2)=func(kt,theta)		!func value for these
c### copy pnew into theta here?
	fval(2)=func(kt,pnew)		!func value for these
c#	call sqz(theta,pnew,jfix,kt,kt)	!restore
c
c do local search. Temp() already contains best vertex after convergence, with
c corresp function value in fval(1). Put altered values in pnew1
c 06/05/89 11:43am retain TEMP here
	do 124 j=1,k
	pnew1(j)=temp(j)+crtstp(j)
c##	pnew1(j)=theta(j)+crtstp(j)
c#	call unsqz(theta,pnew1,jfix,kt,kt)	!unsqueeze pnew1 into theta for func
c#	fval(4)=func(kt,theta)		!func value for these
	fval(4)=func(kt,temp)		!func value for these
c#	call sqz(theta,pnew1,jfix,kt,kt)	!restore
	if(fval(4).lt.fnewlo) goto 125	!restart at better value
	pnew1(j)=temp(j)-crtstp(j)	!step in other direction
c##	pnew1(j)=theta(j)-crtstp(j)	!step in other direction
c#	call unsqz(theta,pnew1,jfix,kt,kt)	!unsqueeze pnew1 into theta for func
c##	fval(4)=func(kt,theta)		!func value for these
	fval(4)=func(kt,pnew1)		!func value for these
c#	call sqz(theta,pnew1,jfix,kt,kt)	!restore
	if(fval(4).lt.fnewlo) goto 125	!restart at better value
124	continue
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
	if(istart.lt.nresmax.and.(il.eq.3.or.il.eq.4).and.
     &	neval.le.nevmax) then
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
	do 1257 j=1,k
1257	temp(j)=thmin(j)
c##1257	theta(j)=thmin(j)
	goto 1258
c
1253	if(.not.silent) print 1259
1259	format( ' Restart after local search')
	irestrt=2		!signals restart after local search
	istart=istart+1	!count number of restarts
	do 1260 j=1,k
1260	temp(j)=pnew1(j)
c
1258	do 1261 j=1,k
1261	step(j)=crtstp(j)	!small step size for restart
	goto 2001	!restart
c
1254	if(.not.silent) print 1262
1262	format(' Return with best vertex')
	iconv=2				!signals 'return with best vertex'
	call mcopy(temp,theta,kt,kt)		!copy temp into theta for output
	fmin=fval(1)
	DEALLOCATE(step,crtstp,temp,centre,pnew,pnew1,fval,thmin,simp)
	DEALLOCATE(jnum)
 	RETURN
c
1255	if(.not.silent) print 1263
1263	format(' Return with averaged vertices')
	iconv=3				!signals 'return with average vertex'
	call mcopy(pnew,theta,kt,kt)		!copy pnew into theta for output
	fmin=fval(2)
	DEALLOCATE(step,crtstp,temp,centre,pnew,pnew1,fval,thmin,simp)
	DEALLOCATE(jnum)
 	RETURN
c
1387	if(.not.silent) print 13871
13871	format(' Return with absmin')
	iconv=4				!signals 'return with absmin'
	call mcopy(thmin,theta,kt,kt)		!copy thmin into theta for output
	fmin=fval(3)
	DEALLOCATE(step,crtstp,temp,centre,pnew,pnew1,fval,thmin,simp)
	DEALLOCATE(jnum)
 	RETURN
c
1388	continue
	if(.not.silent) print 1389
1389	format(' Return with result of local search')
	iconv=5				!signals 'return with pnew1'
	call mcopy(pnew1,theta,kt,kt)	!pnew1 into theta for output
	fmin=fval(4)
	DEALLOCATE(step,crtstp,temp,centre,pnew,pnew1,fval,thmin,simp)
	DEALLOCATE(jnum)
	RETURN
c
c1510	if(.not.silent) print 1452,nevmax
c1452	format(' *** ERROR no convergence after',i8,' evaluations')
c	iconv=6				!signals 'no convergence'
c	DEALLOCATE(step,crtstp,temp,centre,pnew,pnew1,fval,thmin,simp)
c	DEALLOCATE(jnum)
c	RETURN

	END

	subroutine MCOPY(a,b,k,kt)
	dimension a(kt),b(kt)
c Copies A to B (for fast version)
	do 1 i=1,k
1	b(i)=a(i)
	return
	end


	subroutine RCOPY(I,simp,A,K,kt)
	dimension A(kt),simp(kt+1,kt)
c
c copies row I of simp() into A()
	do 1 J=1,K
1	A(J)=simp(I,J)
	return
	end

	subroutine NWVRTX(I,A,simp,K,kt)
	dimension A(kt),simp(kt+1,kt)
c
c copies A() into row I of simp()
	do 1 J=1,K
1	simp(I,J)=A(J)
	return
	end


	subroutine DSPOUT(k,Neval,ndsav,fmin,TH,dprt,kt)
C TO DO PRINTING in simplex
	real TH(kt)
	logical dprt
C
	if(k.le.6) then
	   print 11,neval,fmin,(th(m),m=1,k)
	   if(dprt) write(8,11) neval,fmin,(th(m),m=1,k)
11	   format(i8,g13.6,/,6g13.6)
	else
	   print 10,neval,fmin,(th(m),m=1,k)
	   if(dprt) write(8,10) neval,fmin,(th(m),m=1,k)
10	   format(i8,g13.6,/,10(5g13.6,/))
	endif
	NDSAV=NEVAL
	RETURN
	END


	subroutine abstst(fval,pnew,absmin,thmin,k,kt)
	dimension pnew(kt),thmin(kt)
c
	if(fval.gt.absmin) return	!fval no better
	absmin=fval
	do 1 j=1,k
1	thmin(j)=pnew(j)
	return
	end


c	subroutine simdsp(simp,fval,k)
c	real simp(21,20),fval(21)
cc to print whole simplex for debug
c	do 1 i=1,k+1
c1	print 2,i,(simp(i,j),j=1,k),fval(i)
c2	format(i8,4g13.6)
c	return
c	end



	subroutine sqz(a,b,jfix,k,kt)
	real a(kt)
	dimension b(kt)
	integer jfix(kt)
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

	subroutine unsqz(a,b,jfix,k,kt)
	real a(kt)
	dimension b(kt)
	integer jfix(kt)
c restore b into a (a and b must be different)
	j=0
	do 3 i=1,k
	if(jfix(i).eq.1) goto 3
	j=j+1
	a(i)=b(j)
3	continue
	return
	end



