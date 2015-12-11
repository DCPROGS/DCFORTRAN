	subroutine SIMPLEX(kt,THETA,stpfac,errfac,neval,nevmax,
     & fmin,FUNC,Ndisp,jfix,delmin)
c
c Use F1 to abort
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
	real theta(20)
	dimension step(20),crtstp(20)
	dimension temp(20),centre(20)
	dimension pnew(20),pnew1(20),fval(21),thmin(20),simp(21,20)
	integer jfix(20)
cd	logical pp	!to print progress
	logical silent,debtyp		!no print out at all; or only if caplock on
	logical deb,debug,caplock
	integer*2 ktype
	logical KBHIT		!Utility lib
	character ch,getch
	logical abort
	logical discprt,dprt
	common/dp/discprt
	common/abt/abort
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
c count no of param to be fitted, and set STEP(), CRTSTP().
	k=0
	do 70 j=1,kt
	if(jfix(j).ne.0) goto 70
	k=k+1
	step(k)=stpfac*temp(k)
	crtstp(k)=errfac*temp(k)
70	continue
c
	if(k.eq.kt) goto 3000	!no fixed parameters so use fast version
c
cd	pp=.false.
cd	if(ndisp.lt.0) pp=.true.
c	ndisp=iabs(ndisp)
	reffac=1.0		! reflection coeff => 1
	confac=0.5		! contraction coeff 0 < beta < 1
	extfac=2.0		! extension factor > 1
	n=k+1		! # of vertices in simplex
	neval=0			!counts function evaluations
c
c
1001	continue	!return here for restart
	call NWVRTX(1,temp,simp,k)	!start values=vertex #1
	call unsqz(theta,temp,jfix,kt)	!unsqueeze temp into theta for func
c	if(debug()) print 750,1,(jfix(i),i=1,8),(theta(i),i=1,8)
c	fval(1)=func(KT,theta,yval,xaxis,freq)	!func value for these
	fval(1)=func(kt,theta)		!func value for these
	fsav=fval(1)
	call sqz(theta,temp,jfix,kt)	!restore
	neval=neval+1
	absmin=fval(1)		!starting value
c	call DSPOUT(kt,neval,ndsav,fsav,theta,dprt)	!type it
c
c ----- compute offset of the vertices of the starting simplex
	fac=(sqrt(float(n))-1.)/(float(k)*sqrt(2.))
c
c ----- specify all other vertices of the starting simplex
c
	do 160 i=2,n
	  do 150 j=1,k
150	    simp (i,j)	= simp (1,j) + step(j)*fac
	  simp (i,i-1)	= simp(1,i-1) + step(i-1)*(fac+1./sqrt(2.))
c
c ----- and calculate their residuals
c
	  I1	= i
	  call RCOPY(I1,SIMP,temp,k)
	call unsqz(theta,temp,jfix,kt)	!unsqueeze temp into theta for func
	fval(i1)=func(kt,theta)		!func value for these
	call sqz(theta,temp,jfix,kt)	!restore
160	call abstst(fval(i1),temp,absmin,thmin,k)	!test for abs min
	neval=neval+K
c
c ----- display starting simplex
cD	print 1021
cD1021	format (/' Starting simplex (with func values):')
cd	call simdsp(simp,fval,k)
c
c
c ----- start iteration loop here at 1000
c
	ndsav=0
	niter	= 0
1000	continue
c	if (niter.ge.maxit) goto 510	! max # of iterations exceeded
	niter	= niter + 1
cd	print *,' niter= ',niter
c
c
C find best (lowest fval) and worst (highest fval) vertices- indeces
c of these are ILO,IHI respectively. fnewlo=fval(IHI) corresponds to
c the vertex to be replaced
	flo=fval(1)
	fnewlo=flo
	ILO=1
	IHI=1
	do 5 I=2,N
	if(fval(I).GE.flo) goto 4
	flo=fval(I)
	ILO=I
4	if(fval(i).le.fnewlo) goto 5
	fnewlo=fval(i)
	IHI=I
5	continue
cd	print 100,ihi,ilo,niter,neval
cd100	format( 'ihi,ilo,niter ,neval= ',4i8)
c
c display current best vertex
c	if(silent.or.ndisp1.eq.0.or.(.not.debug().and.debtyp)) goto 360
	if(silent.or.ndisp1.eq.0.or.(.not.deb.and.debtyp)) goto 360
	if((neval-ndsav).lt.ndisp1) goto 360
cd	if(pp) print 176
cd176	format(/)
	CALL RCOPY(ILO,simp,temp,k)	!copy best vertex to temp
	call unsqz(theta,temp,jfix,kt)	!unsqueeze temp into theta for display
	call DSPOUT(kt,neval,ndsav,flo,theta,dprt)	!type it
360	continue
c	if(debug()) print 750,niter,(jfix(i),i=1,8),(theta(i),i=1,8)
	if(neval.gt.nevmax) goto 510
c
c ----- compute centroid of all vertices except the worst
c
	do 210	j=1,k
210	  centre(j)= 0.0
	do 220	i = 1,n
	  if (I.eq.IHI) goto 220
	  do 221 j = 1,k
221	    centre(j)	= centre(j) + simp (i,j)
220	continue
c
c ----- reflect, with next vertex taken as reflection of worst
c Parameter values that are coord of new vertex in pnew(j)
c
	do 230	j=1,k
	  centre(j)	= centre(j)/float(K)
230	  pnew(j)	=centre(j) - reffac*(simp(IHI,J)-centre(J))
	call unsqz(theta,pnew,jfix,kt)	!unsqueeze pnew into theta for func
	fnew=func(kt,theta)			!func value for these
	call sqz(theta,pnew,jfix,kt)	!restore
	neval=neval+1
	call abstst(fnew,pnew,absmin,thmin,k)	!test for abs min
cd	if(pp) print 170
cd170	format(' R ')
cd	print 101,(pnew(j),j=1,k),fnew
cd101	format(' reflected. pnew,fnew= ',/,11g13.6)
c -----
	if (fnew.ge.flo) goto 12	!fnew worst than prev best
c
c ----- new vertex is better than previous best
c ----- extend it
c
	do 240	j= 1,k
240	  pnew1(j) =centre(j) + extfac*(pnew(j)-centre(j))
	call unsqz(theta,pnew1,jfix,kt)	!unsqueeze pnew1 into theta for func
	fnew1=func(kt,theta)			!func value for these
	call sqz(theta,pnew1,jfix,kt)	!restore
	neval=neval+1
	call abstst(fnew1,pnew1,absmin,thmin,k)	!test for abs min
c ----- still better?
cd	print 102,(pnew1(j),j=1,k),fnew1
cd102	format(' extended. pnew1,fnew1= ',/,11g13.6)
	if (fnew1.ge.fnew) goto 19	! no- input pnew and test conv
cd	if(pp) print 172
cd172	format(' E ')	!successful expansion
c
c ----- yes- so extension (pnew1) inputed (or contraction inputed if
c	 arrive at 10 via 15 below)
c
10	continue
	call NWVRTX (IHI,pnew1,simp,K)
	fval(IHI)=fnew1
	neval=neval+1
	call abstst(fnew1,pnew1,absmin,thmin,k)	!test for abs min
cd	print 103
cd103	format( ' pnew1 inputed at label 10')
cd	call simdsp(simp,fval,k)
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
cd	if(pp) print 104,L
cd104	format(' L=',i1,1x,)
cd	print 1041,L
cd1041	format(' L= ',i4)
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
	call NWVRTX(IHI,pnew,simp,K)
	fval(IHI)=fnew
cd	print 105
cd105	format(' pnew inputed')
cd	call simdsp(simp,fval,k)
cd	print 106
cd106	format(' contract on reflected side. pnew1=',/)
cd	if(pp) print 173
cd173	format(' C2 ')
cd	if(pp) goto 107
cd	goto 107	!to avoid print at 108
c
15	continue
c Contract on the original fval(IHI) side of the centroid
c
cd	print 108
cd108	format(' contract on unreflected side. pnew1,fnew1= ',/)
cd	if(pp) print 174
cd174	format(' C1 ')
107	continue
	do 270	J= 1,K
270	  pnew1(j) = centre(j)+confac*(simp(IHI,J)-centre(J))
	call unsqz(theta,pnew1,jfix,kt)	!unsqueeze pnew1 into theta for func
	fnew1=func(kt,theta)			!func value for these
	call sqz(theta,pnew1,jfix,kt)	!restore
	neval=neval+1
	call abstst(fnew1,pnew1,absmin,thmin,k)	!test for abs min
cd	print 1022,(pnew1(j),j=1,k),fnew1
cd1022	format(11g13.6)
c
c ----- is contracted vertex better than the worst vertex
	if(fnew1.le.fval(IHI)) goto 10
c
c
c ----- no, it is still bad, shrink whole simplex towards best vertex
c
	do 18	i = 1,n
	  do 17  j = 1,k
C	    simp(i,j) = (simp(i,j)+simp(ILO,J))*0.5	!orig version
	    simp(i,j) = simp(ILO,J)+confac*(simp(i,j)-simp(ILO,J))
c  last line is D.C. version that uses confac rather than 0.5
	    temp(j)=simp(i,j)
17	  continue
	call unsqz(theta,temp,jfix,kt)	!unsqueeze temp into theta for func
	fval(i)=func(kt,theta)		!func value for these
	call sqz(theta,temp,jfix,kt)	!restore
	call abstst(fval(i),temp,absmin,thmin,k)	!test for abs min
18	continue
	neval=neval+n
cd	if(pp) print 175
cd175	format(' S ')
cd	print 110,ilo
cd110	format( ' shrink towards vertex ',i8)
cd	call simdsp(simp,fval,k)
	goto 901	!converged?
c
c Retain reflection
19	continue
	call NWVRTX(IHI,pnew,simp,K)
	fval(IHI)=fnew
cd	print 111
cd111	format(' input pnew')
cd	call simdsp(simp,fval,k)
c
c
901	continue
cd	if(pp) print 171
cd171	format(' T ')
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
	do 81 i=1,n
81	if(fval(i).lt.fval(il)) il=i
	del=fsav-fval(il)	!reduction in min=pos value
cd	print 16,neval,fsav,fval(il),del,delmin,(fval(i),i=1,n)
	IF(DEL.LT.-1.E-5) GOTO 84	!MIN IS INCREASING! DEL NEGATIVE
	if(del.gt.delmin) goto 83
c Section when DEL < DELMIN
	idel=idel+1
	if(idel.lt.ndel) goto 831	!not yet converged
	if(.not.silent) print 85
85	FORMAT( ' Converged via DELMIN ',/)
cd	print 16,neval,fsav,fval(il),del,delmin,(fval(i),i=1,n)
cd16	format(' neval,fsav,fval(il),del,delmin= ',/,i8,4g13.6,/,
cd    &	' fval(i)= ',6g13.6,/,5g13.6,/,5g13.6,/,5g13.6)
	L=2		!converged
	GOTO 84
C I.E. DEL POSITIVE. IF <DELMIN FINISH
83	idel=0			!reset when del > delmin
831	fsav=fval(il)		!SAVE LAST MINIMUM
	goto 84
c
80	continue
	L=1		!conv via crtstp
	do 370 j=1,k	!test each parameter
	IL=1
	IH=1
	do 371 i=1,n	!order values of current param
	if(simp(i,j).lt.simp(IL,j)) IL=i
371	if(simp(i,j).gt.simp(IH,j)) IH=i
	if((simp(IH,j)-simp(IL,j)).gt.abs(crtstp(j))) L=0	!not conv
cd	print 3711,simp(ih,j),simp(il,j),simp(ih,j)-simp(il,j),crtstp(j)
cd3711	format(' convergence test',/,4g13.6)
370	continue
c
c
c ----- change values for reffac,confac,extfac
c
84	continue
c#	if (levent(2).eq.0) goto 3800	!reset orig values
c#	reffac	= 1.
c#	confac= .5
c#	extfac	= 2.0
c#3800	if(levent(1).eq.0) goto 383	!change values
c#	reffac	= 1.5
c#	confac	=.25
c#	extfac	= 1.5
c
c ----- abort on levent (0)
c
383	continue
c##	if(levent(0).eq.1) goto 384	! abort
c##	if(numlock()) goto 384	! abort  -temporary
c NB key F1 has ktype=0, scan code=59 (see \fortran\tkey.for)
	if(abort) goto 384	!may be set true via common
	if(KBHIT()) then
		ch=GETCH(ktype)
		if(ktype.eq.0.and.ichar(ch).eq.59) goto 384	!abort
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
	call unsqz(theta,temp,jfix,kt)  !unsqueeze temp into theta for display
	if(.not.silent) call DSPOUT(kt,neval,ndsav,fnewlo,theta,dprt)	!type it
	fval(1)=fnewlo	!for test below
c this is best vertex
c
c check absmin
C	print 453
C453	format( ' absmin: ')
C	call unsqz(theta,thmin,jfix,kt)	!unsqueeze thmin to theta for display
c	call DSPOUT(kt,neval,ndsav,absmin,theta,dprt)	!type it
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
	call unsqz(theta,pnew,jfix,kt)	!unsqueeze pnew into theta for func
	fval(2)=func(kt,theta)		!func value for these
	call sqz(theta,pnew,jfix,kt)	!restore
C	print 450
C450	format( ' averaged values: ')
C	call unsqz(theta,pnew,jfix,kt)	!unsqueeze pnew into theta for display
c	call DSPOUT(kt,neval,ndsav,fval(2),theta,dprt)	!type it
c
c do local search. Temp() already contains best vertex after convergence, with
c corresp function value in fval(1). Put altered values in pnew1
	do 24 j=1,k
	pnew1(j)=temp(j)+crtstp(j)
	call unsqz(theta,pnew1,jfix,kt)	!unsqueeze pnew1 into theta for func
	fval(4)=func(kt,theta)		!func value for these
	call sqz(theta,pnew1,jfix,kt)	!restore
	if(fval(4).lt.fnewlo) goto 25	!restart at better value
	pnew1(j)=temp(j)-crtstp(j)	!step in other direction
	call unsqz(theta,pnew1,jfix,kt)	!unsqueeze pnew1 into theta for func
	fval(4)=func(kt,theta)		!func value for these
	call sqz(theta,pnew1,jfix,kt)	!restore
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
	do 251 i=1,4
251	if(fval(i).lt.fval(il)) il=i
	if(il.eq.1) goto 254	!exit with best vertex=temp
	if(il.eq.2) goto 255	!exit with average
	if(il.eq.3) goto 252	!retart at thmin
	if(il.eq.4) goto 253	!restart at pnew1
c
252	if(.not.silent) print 256
256	format( ' Restart at absmin')
	do 257 j=1,k
257	temp(j)=thmin(j)
	goto 258
c
253	if(.not.silent) print 259
259	format( ' Restart after local search')
	do 260 j=1,k
260	temp(j)=pnew1(j)
c
258	do 261 j=1,k
261	step(j)=crtstp(j)	!small step size for restart
	goto 1001	!restart
c
254	if(.not.silent) print 262
262	format( ' Return with best vertex')
	call unsqz(theta,temp,jfix,kt)	!unsqueeze temp into theta for output
	fmin=fval(1)
 	RETURN
c
255	if(.not.silent) print 263
263	format( ' Return with averaged vertices')
	call unsqz(theta,pnew,jfix,kt)	!unsqueeze pnew into theta for output
	fmin=fval(2)
 	RETURN
c
387	if(.not.silent) print 3871
3871	format( ' Return with absmin')
	call unsqz(theta,thmin,jfix,kt)	!thmin into theta for output
	fmin=fval(3)
 	RETURN
c
510	if(.not.silent) print 452,nevmax
452	format(' *** ERROR no convergence after',i8,' evaluations')
	RETURN
c End of general version of SIMPLEX
c Now fast version for case where there are no fixed parameters
c
3000	continue
c	if(.not.silent) print 30
c30	format(' USING FAST VERSION OF SIMPLEX')
c	ndisp=iabs(ndisp)
	reffac	= 1.0		! reflection coeff => 1
	confac	= .5		! contraction coeff 0 < beta < 1
	extfac	= 2.0		! extension factor > 1
	n	= k+1		! # of vertices in simplex
	neval=0			!counts ficntion evaluations
c
c
2001	continue	!return here for restart
	call NWVRTX(1,theta,simp,k)	!start values=vertex #1
	fval(1)=func(kt,theta)		!func value for these
	fsav=fval(1)
	neval=neval+1
	absmin=fval(1)		!starting value
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
	  call RCOPY(I1,SIMP,theta,k)
	fval(i1)=func(kt,theta)		!func value for these
1160	call abstst(fval(i1),theta,absmin,thmin,k)	!test for abs min
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
	call DSPOUT(kt,neval,ndsav,flo,theta,dprt)	!type it
1360	continue
	if(neval.gt.nevmax) goto 1510
c
c ----- compute centroid of all vertices except the worst
c
	do 1210 j=1,k
1210	  centre(j)= 0.0
	do 1220 i = 1,n
	  if (i.eq.IHI) goto 1220
	  do 1221 j = 1,k
1221	    centre(j)	= centre(j) + simp (i,j)
1220	continue
c
c ----- reflect, with next vertex taken as reflection of worst
c Parameter values that are coord of new vertex in pnew(j)
c
	do 1230 j=1,k
	  centre(j)	= centre(j)/float(K)
1230	  pnew(j)	=centre(j) - reffac*(simp(IHI,J)-centre(J))
c##copy theta to pnew here?
c#	call unsqz(theta,pnew,jfix,kt)	!unsqueeze pnew into theta for func
c#	fnew=func(kt,theta)			!func value for these
	fnew=func(kt,pnew)			!func value for these
c#	call sqz(theta,pnew,jfix,kt)	!restore
	neval=neval+1
	call abstst(fnew,pnew,absmin,thmin,k)	!test for abs min
c -----
	if (fnew.ge.flo) goto 112	!fnew worst than prev best
c
c ----- new vertex is better than previous best so extend it
c
	do 1240 j= 1,k
1240	  pnew1(j) =centre(j) + extfac*(pnew(j)-centre(j))
c#	call unsqz(theta,pnew1,jfix,kt)	!unsqueeze pnew1 into theta for func
c#	fnew1=func(kt,theta)			!func value for these
	fnew1=func(kt,pnew1)			!func value for these
c#	call sqz(theta,pnew1,jfix,kt)	!restore
	neval=neval+1
	call abstst(fnew1,pnew1,absmin,thmin,k)	!test for abs min
c ----- still better?
	if (fnew1.ge.fnew) goto 119	! no- input pnew and test conv
c
c ----- yes- so extension (pnew1) inputed (or contraction inputed if
c	 arrive at 110 via 115 below)
c
110	continue
	call NWVRTX (IHI,pnew1,simp,K)
	fval(IHI)=fnew1
	neval=neval+1
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
	call NWVRTX(IHI,pnew,simp,K)
	fval(IHI)=fnew
c
115	continue
c Contract on the original fval(IHI) side of the centroid
c
1107	continue
	do 1270 j= 1,k
1270	  pnew1(j) = centre(j)+confac*(simp(IHI,J)-centre(J))
c#	call unsqz(theta,pnew1,jfix,kt)	!unsqueeze pnew1 into theta for func
c#	fnew1=func(kt,theta)			!func value for these
	fnew1=func(kt,pnew1)			!func value for these
c#	call sqz(theta,pnew1,jfix,kt)	!restore
	neval=neval+1
	call abstst(fnew1,pnew1,absmin,thmin,k)	!test for abs min
c
c ----- is contracted vertex better than the worst vertex
	if(fnew1.le.fval(IHI)) goto 110
c
c
c ----- no, it is still bad, shrink whole simplex towards best vertex
c
	do 118 i = 1,n
	  do 117 j = 1,k
C	    simp(i,j) = (simp(i,j)+simp(ILO,J))*0.5	!orig version
	    simp(i,j) = simp(ILO,J)+confac*(simp(i,j)-simp(ILO,J))
c  last line is D.C. version that uses confac rather than 0.5
c#	    temp(j)=simp(i,j)
	    theta(j)=simp(i,j)
117	  continue
c#	call unsqz(theta,temp,jfix,kt)	!unsqueeze temp into theta for func
	fval(i)=func(kt,theta)		!func value for these
c#	call sqz(theta,temp,jfix,kt)	!restore
c#	call abstst(fval(i),temp,absmin,thmin,k)	!test for abs min
	call abstst(fval(i),theta,absmin,thmin,k)	!test for abs min
118	continue
	neval=neval+n
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
c#	if (levent(2).eq.0) goto 3800	!reset orig values
c#	reffac	= 1.
c#	confac= .5
c#	extfac	= 2.0
c#3800	if(levent(1).eq.0) goto 383	!change values
c#	reffac	= 1.5
c#	confac	=.25
c#	extfac	= 1.5
c
c ----- abort on levent (0)
c
1383	continue
c##	if(levent(0).eq.1) goto 384	! abort
c##	if(numlock()) goto 1384	! abort  -temporary
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
	if(.not.silent) call DSPOUT(kt,neval,ndsav,fnewlo,temp,dprt)  !type it
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
c#	call unsqz(theta,pnew,jfix,kt)	!unsqueeze pnew into theta for func
c#	fval(2)=func(kt,theta)		!func value for these
c### copy pnew into theta here?
	fval(2)=func(kt,pnew)		!func value for these
c#	call sqz(theta,pnew,jfix,kt)	!restore
c
c do local search. Temp() already contains best vertex after convergence, with
c corresp function value in fval(1). Put altered values in pnew1
c 06/05/89 11:43am retain TEMP here
	do 124 j=1,k
	pnew1(j)=temp(j)+crtstp(j)
c##	pnew1(j)=theta(j)+crtstp(j)
c#	call unsqz(theta,pnew1,jfix,kt)	!unsqueeze pnew1 into theta for func
c#	fval(4)=func(kt,theta)		!func value for these
	fval(4)=func(kt,temp)		!func value for these
c#	call sqz(theta,pnew1,jfix,kt)	!restore
	if(fval(4).lt.fnewlo) goto 125	!restart at better value
	pnew1(j)=temp(j)-crtstp(j)	!step in other direction
c##	pnew1(j)=theta(j)-crtstp(j)	!step in other direction
c#	call unsqz(theta,pnew1,jfix,kt)	!unsqueeze pnew1 into theta for func
c##	fval(4)=func(kt,theta)		!func value for these
	fval(4)=func(kt,pnew1)		!func value for these
c#	call sqz(theta,pnew1,jfix,kt)	!restore
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
	do 1251 i=1,4
1251	if(fval(i).lt.fval(il)) il=i
	if(il.eq.1) goto 1254	!exit with best vertex=temp
	if(il.eq.2) goto 1255	!exit with average
	if(il.eq.3) goto 1252	!retart at thmin
	if(il.eq.4) goto 1253	!restart at pnew1
c
1252	if(.not.silent) print 1256
1256	format( ' Restart at absmin')
	do 1257 j=1,k
1257	temp(j)=thmin(j)
c##1257	theta(j)=thmin(j)
	goto 1258
c
1253	if(.not.silent) print 1259
1259	format( ' Restart after local search')
	do 1260 j=1,k
1260	temp(j)=pnew1(j)
c##1260	theta(j)=pnew1(j)
c
1258	do 1261 j=1,k
1261	step(j)=crtstp(j)	!small step size for restart
	goto 2001	!restart
c
1254	if(.not.silent) print 1262
1262	format( ' Return with best vertex')
c#	call unsqz(theta,temp,jfix,kt)	!unsqueeze temp into theta for output
	call mcopy(temp,theta,kt)		!copy temp into theta for output
	fmin=fval(1)
 	RETURN
c
1255	if(.not.silent) print 1263
1263	format( ' Return with averaged vertices')
c#	call unsqz(theta,pnew,jfix,kt)	!unsqueeze pnew into theta for output
	call mcopy(pnew,theta,kt)		!copy pnew into theta for output
	fmin=fval(2)
 	RETURN
c
1387	if(.not.silent) print 13871
13871	format( ' Return with absmin')
c#	call unsqz(theta,thmin,jfix,kt)	!thmin into theta for output
	call mcopy(thmin,theta,kt)		!copy thmin into theta for output
	fmin=fval(3)
 	RETURN
c
1510	if(.not.silent) print 1452,nevmax
1452	format(' *** ERROR no convergence after',i8,' evaluations')
	RETURN

	END

	subroutine MCOPY(a,b,k)
	dimension a(20),b(20)
c Copies A to B (for fast version)
	do 1 i=1,k
1	b(i)=a(i)
	return
	end


	subroutine RCOPY(I,simp,A,K)
	dimension A(20),simp(21,20)
c
c copies row I of simp() into A()
	do 1 J=1,K
1	A(J)=simp(I,J)
	return
	end

	subroutine NWVRTX(I,A,simp,K)
	dimension A(20),simp(21,20)
c
c copies A() into row I of simp()
	do 1 J=1,K
1	simp(I,J)=A(J)
	return
	end


	subroutine DSPOUT(k,Neval,ndsav,fmin,TH,dprt)
C TO DO PRINTING in simplex
	real TH(20)
	logical dprt
C
	if(k.le.6) then
	   print 11,neval,fmin,(th(m),m=1,k)
	   if(dprt) write(8,11) neval,fmin,(th(m),m=1,k)
11	   format(i8,g13.6,/,6g13.6)
	else
	   print 10,neval,fmin,(th(m),m=1,k)
	   if(dprt) write(8,10) neval,fmin,(th(m),m=1,k)
10	   format(i8,g13.6,/,4(5g13.6,/))
	endif
	NDSAV=NEVAL
	RETURN
	END


	subroutine abstst(fval,pnew,absmin,thmin,k)
	dimension pnew(20),thmin(20)
c
	if(fval.gt. absmin) return	!fval no better
	absmin=fval
	do 1 j=1,k
1	thmin(j)=pnew(j)
	return
	end


cd	subroutine simdsp(simp,fval,k)
cd	real simp(21,20),fval(21)
cc to print whole simplex for debug
cd	do 1 i=1,k+1
cd1	print 2,i,(simp(i,j),j=1,k),fval(i)
cd2	format(i8,4g13.6)
cd	return
cd	end



	subroutine sqz(a,b,jfix,k)
	real a(20)
	dimension b(20)
	integer jfix(20)
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
	real a(20)
	dimension b(20)
	integer jfix(20)
c restore b into a (a and b must be different)
	j=0
	do 3 i=1,k
	if(jfix(i).eq.1) goto 3
	j=j+1
	a(i)=b(j)
3	continue
	return
	end



