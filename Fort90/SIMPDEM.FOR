	subroutine SIMPLEX(kt,THETA,stpfac,errfac,neval,nevmax,
     & fmin,FUNC,delmin)
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
	external FUNC
c
c
c Set ndel=no of consec iterations for which reduction in func<delmin
c for convergence
	ndel=5
	idel=0		!counts up to ndel
	do j=1,kt
	   step(j)=stpfac*theta(j)
	   crtstp(j)=errfac*theta(j)
	enddo
c
	reffac	= 1.0		! reflection coeff => 1
	confac	= .5		! contraction coeff 0 < beta < 1
	extfac	= 2.0		! extension factor > 1
	n=k+1		! # of vertices in simplex
	neval=0			!counts ficntion evaluations
c
2001	continue	!return here for restart
	call NWVRTX(1,theta,simp,k)	!start values=vertex #1
	fval(1)=func(kt,theta)		!func value for these
	fsav=fval(1)
	neval=neval+1
	absmin=fval(1)		!starting value
	fac=(sqrt(float(n))-1.)/(float(k)*sqrt(2.))
	do 1160 i=2,n
	  do 1150 j=1,k
1150	    simp (i,j)	= simp (1,j) + step(j)*fac
	  simp (i,i-1)	= simp(1,i-1) + step(i-1)*(fac+1./sqrt(2.))
	  I1	= i
	  call RCOPY(I1,SIMP,theta,k)
	fval(i1)=func(kt,theta)		!func value for these
1160	call abstst(fval(i1),theta,absmin,thmin,k)	!test for abs min
	neval=neval+K
c
c start iteration loop here at 2000
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
	if(neval.gt.nevmax) goto 1510
	do 1210 j=1,k
1210	  centre(j)= 0.0
	do 1220 i = 1,n
	  if (i.eq.IHI) goto 1220
	  do 1221 j = 1,k
1221	    centre(j)	= centre(j) + simp (i,j)
1220	continue
c
	do 1230 j=1,k
	  centre(j)	= centre(j)/float(K)
1230	  pnew(j)	=centre(j) - reffac*(simp(IHI,J)-centre(J))
	fnew=func(kt,pnew)			!func value for these
	neval=neval+1
	call abstst(fnew,pnew,absmin,thmin,k)	!test for abs min
	if (fnew.ge.flo) goto 112	!fnew worst than prev best
	do 1240 j= 1,k
1240	  pnew1(j) =centre(j) + extfac*(pnew(j)-centre(j))
	fnew1=func(kt,pnew1)			!func value for these
	neval=neval+1
	call abstst(fnew1,pnew1,absmin,thmin,k)	!test for abs min
	if (fnew1.ge.fnew) goto 119	! no- input pnew and test conv
110	continue
	call NWVRTX (IHI,pnew1,simp,K)
	fval(IHI)=fnew1
	neval=neval+1
	call abstst(fnew1,pnew1,absmin,thmin,k)	!test for abs min
	goto 1901		!convergence check
c
112	continue
	L=0
	do 113 I=1,n
113	if(fval(I).gt.fnew) L=L+1
	if(L.ge.2) goto 119
	if(L.eq.0) goto 115	!reflected vertex is better than none i.e.
	call NWVRTX(IHI,pnew,simp,K)
	fval(IHI)=fnew
115	continue
1107	continue
	do 1270 j= 1,k
1270	  pnew1(j) = centre(j)+confac*(simp(IHI,J)-centre(J))
	fnew1=func(kt,pnew1)			!func value for these
	neval=neval+1
	call abstst(fnew1,pnew1,absmin,thmin,k)	!test for abs min
	if(fnew1.le.fval(IHI)) goto 110
	do 118 i = 1,n
	  do 117 j = 1,k
	    simp(i,j) = simp(ILO,J)+confac*(simp(i,j)-simp(ILO,J))
	    theta(j)=simp(i,j)
117	  continue
	fval(i)=func(kt,theta)		!func value for these
	call abstst(fval(i),theta,absmin,thmin,k)	!test for abs min
118	continue
	neval=neval+n
	goto 1901	!converged?
119	continue
	call NWVRTX(IHI,pnew,simp,K)
	fval(IHI)=fnew
c
1901	continue
C CHECK CONVERGENCE.
c Define L=0 for not converged- do next iteration
c L=1 for converged via crtstp
c L=2 for converged via delmin (no restarts)
c L=3 for abort (no restarts)
	L=0	!not converged
	if(delmin.lt.0.) goto 180	!use normal convergence check
	il=1
	do 181 i=1,n
181	if(fval(i).lt.fval(il)) il=i
	del=fsav-fval(il)	!reduction in min=pos value
	IF(DEL.LT.-1.E-5) GOTO 184	!MIN IS INCREASING! DEL NEGATIVE
	if(del.gt.delmin) goto 183
c Section when DEL < DELMIN
	idel=idel+1
	if(idel.lt.ndel) goto 1831	!not yet converged
	L=2		!converged via delmin
c	GOTO 84
	goto 184
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
184	continue
c
	if (L.eq.0) goto 2000	!next iteration
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
	fnewlo=fval(ihi)
	fval(1)=fnewlo	!for test below
	fval(3)=absmin		!for test below
	if(L.eq.1) goto 1386	!not aborted
	if(fval(1).lt.fval(3)) goto 1254  !aborted- use best vertex
	goto 1387	!aborted- use absmin- no restarts
1386	continue
	do 1400 j = 1,k
	  pnew(j)	= 0.0
	  do 1390  i = 1,n
1390	    pnew(j)	= pnew(j) + simp(i,j)
1400	  pnew(j)	= pnew(j) / float(n)
	fval(2)=func(kt,pnew)		!func value for these
c do local search. Temp() already contains best vertex after convergence, with
c corresp function value in fval(1). Put altered values in pnew1
c 06/05/89 11:43am retain TEMP here
	do 124 j=1,k
	pnew1(j)=temp(j)+crtstp(j)
	fval(4)=func(kt,temp)		!func value for these
	if(fval(4).lt.fnewlo) goto 125	!restart at better value
	pnew1(j)=temp(j)-crtstp(j)	!step in other direction
	fval(4)=func(kt,pnew1)		!func value for these
	if(fval(4).lt.fnewlo) goto 125	!restart at better value
124	continue
	fval(4)=fval(1)		!restore orig func value
125	continue
	il=1
	do 1251 i=1,4
1251	if(fval(i).lt.fval(il)) il=i
	if(il.eq.1) goto 1254	!exit with best vertex=temp
	if(il.eq.2) goto 1255	!exit with average
	if(il.eq.3) goto 1252	!retart at thmin
	if(il.eq.4) goto 1253	!restart at pnew1
c
1252	continue
	do 1257 j=1,k
1257	temp(j)=thmin(j)	!restart at absmin
	goto 1258
c
1253	continue
	do 1260 j=1,k		!Restart after local search
1260	temp(j)=pnew1(j)
c
1258	do 1261 j=1,k
1261	step(j)=crtstp(j)	!small step size for restart
	goto 2001	!restart
c
1254	continue		!Return with best vertex
	call mcopy(temp,theta,kt)		!copy temp into theta for output
	fmin=fval(1)
 	RETURN
c
1255	continue		!Return with averaged vertices
	call mcopy(pnew,theta,kt)		!copy pnew into theta for output
	fmin=fval(2)
 	RETURN
c
1387	continue		!Return with absmin
	call mcopy(thmin,theta,kt)		!copy thmin into theta for output
	fmin=fval(3)
 	RETURN
c
1510	print 1452,nevmax
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



