	subroutine SIMPLEX0(kt,THETA,stpfac,errfac,neval,nevmax,
     & fmin,FUNC,Ndisp,delmin)
c SIMPLEX0 is pared-down version of SIMPLEX for use in SCAN
c==========also remove abstst() and put RCOPY code in prog?
c========= Remove print out option?
c Use F1 to abort
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
C JUNE 1984. MODIFIED WITH FEATURES FROM I.D.HILL BASED ON O'NEILL,R
C APPLIED STATS AS47,20,338-345 (1971) AND BENYON P.R.,IBID. 25,97 (1976)
C D. COLQUHOUN.
c	PASCAL (original)	: Caceci MS and Cacheris WP
c		    		  'BYTE', May 1984, pp.340 ff
c
c	FORTRAN translation	: Ch. Methfessel, Max-Plank Inst.
c	Present version		: A.B. Cachelin, UCL
c
c
c
c	This version to be used as a subroutine so no input or
c	output part. Instead values are to be given in the
c	subroutine call.
c
c	CALL SIMP5(K,BP,THETA,STEP,CRTSTP,NEVAL,NEVMAX,
c		FMIN,FUNC,NDISP,JFIX,YVAL,NDIM,delmin)
c
c	Where:
c	KT		: total number of parameters			input
c			: (internally K= no of variable param)
c	THETA(K)	: initial guesses (=BP) 		input
c	THETA (K)	: final parameters			output
c	STPFAC		: factor for initial step sizes		input
c	ERRFAC		: convergence criterion	(changed	  "
c			: if param fixed)		  .
c	NEVAL		: # of function evaluations		output
c	NEVMAX		: maximum number of evaluations		input
c	FMIN		: minimum value of function		output
c	FUNC		: function to be minimised		input
c	NDISP		: display SIMP every NDISP times	  .
c			: no print if ndisp=0. if ndisp <0 then print
c			: every -ndisp eval and R=ref,E=ext,C1=con on
c			: ihi side,C2=con on ref side,S=shrink,T=test conv
c	JFIX(K)		: =0 for variable, =1 for fixed parameters
c	YVAL(NDIM)	: data for FUNC
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
	logical silent,debtyp		!no print out at all; or only if caplock on
	logical deb,debug,caplock
	integer*2 ktype
	logical KBHIT		!Utility lib
	character ch,getch
	logical discprt
	common/dp/discprt
	external FUNC
c
c
c	debug()=caplock()
c	deb=caplock()
	deb=.false.
c
	call UNDER0('true')	!so underflows set=0 (Lahey manual p12.8)
	silent=ndisp.eq.-1000
	debtyp=ndisp.lt.0
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
	k=kt				!no fixed parameters in this version
	do j=1,kt
	  step(k)=stpfac*temp(k)
	  crtstp(k)=errfac*temp(k)
	enddo
	reffac= 1.0		! reflection coeff => 1
	confac= .5		! contraction coeff 0 < beta < 1
	extfac= 2.0		! extension factor > 1
	n= k+1		! # of vertices in simplex
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
	do i=2,n
	  do j=1,k
	    simp(i,j)=simp (1,j) + step(j)*fac
	  enddo
	  simp(i,i-1)=simp(1,i-1) + step(i-1)*(fac+1./sqrt(2.))
c ----- and calculate their residuals
	  i1= i
	  call RCOPY(i1,SIMP,theta,k)
	  fval(i1)=func(kt,theta)		!func value for these
	  call abstst(fval(i1),temp,absmin,thmin,k)	!test for abs min
	enddo

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
	do I=2,N
	  if(fval(I).lt.flo) then
	    flo=fval(I)
	    ILO=I
	  endif
	  if(fval(i).gt.fnewlo) then
	    fnewlo=fval(i)
	    IHI=I
	  endif
	enddo
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
	do j=1,k
	  centre(j)= 0.0
	enddo
	do i=1,n
	  if(i.ne.IHI) then
	    do j=1,k
	    centre(j)	= centre(j) + simp (i,j)
	    enddo
	  endif
	enddo
c
c ----- reflect, with next vertex taken as reflection of worst
c Parameter values that are coord of new vertex in pnew(j)
c
	do j=1,k
	  centre(j)	= centre(j)/float(K)
	  pnew(j)	=centre(j) - reffac*(simp(IHI,J)-centre(J))
	enddo
c##copy theta to pnew here?
	fnew=func(kt,pnew)			!func value for these
	neval=neval+1
	call abstst(fnew,pnew,absmin,thmin,k)	!test for abs min
c -----
	if (fnew.ge.flo) goto 112	!fnew worst than prev best
c
c ----- new vertex is better than previous best so extend it
c
	do j=1,k
	  pnew1(j) =centre(j) + extfac*(pnew(j)-centre(j))
	enddo
	fnew1=func(kt,pnew1)			!func value for these
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
	do i=1,n
	  if(fval(i).gt.fnew) L=L+1
	enddo
	if(L.ge.2) goto 119	!reflected vertex better than 2 or more
c				! so input pnew and test conv
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
	do j=1,k
	  pnew1(j) = centre(j)+confac*(simp(IHI,J)-centre(J))
	enddo
	fnew1=func(kt,pnew1)			!func value for these
	neval=neval+1
	call abstst(fnew1,pnew1,absmin,thmin,k)	!test for abs min
c
c ----- is contracted vertex better than the worst vertex
	if(fnew1.le.fval(IHI)) goto 110
c
c
c ----- no, it is still bad, shrink whole simplex towards best vertex
c
	do i=1,n
	  do j=1,k
	    simp(i,j) = simp(ILO,J)+confac*(simp(i,j)-simp(ILO,J))
c  last line is D.C. version that uses confac rather than 0.5
	    theta(j)=simp(i,j)
	  enddo
	  fval(i)=func(kt,theta)		!func value for these
	  call abstst(fval(i),theta,absmin,thmin,k)	!test for abs min
	enddo
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
	do i=1,n
	if(fval(i).lt.fval(il)) il=i
	enddo
	del=fsav-fval(il)	!reduction in min=pos value
	IF(DEL.LT.-1.E-5) GOTO 184	!MIN IS INCREASING! DEL NEGATIVE
	if(del.gt.delmin) goto 183
c Section when DEL < DELMIN
	idel=idel+1
	if(idel.lt.ndel) goto 1831	!not yet converged
	if(.not.silent) print 185
185	FORMAT( ' Converged via DELMIN ',/)
	L=2		!converged
	goto 184
C I.E. DEL POSITIVE. IF <DELMIN FINISH
183	idel=0			!reset when del > delmin
1831	fsav=fval(il)		!SAVE LAST MINIMUM
	goto 184
c
180	continue
	L=1		!conv via crtstp
	do j=1,k	!test each parameter
	  IL=1
	  IH=1
	  do i=1,n	!order values of current param
	    if(simp(i,j).lt.simp(IL,j)) IL=i
	    if(simp(i,j).gt.simp(IH,j)) IH=i
	  enddo
	  if((simp(IH,j)-simp(IL,j)).gt.abs(crtstp(j))) L=0	!not conv
	enddo
c
184	continue
c
1383	continue
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
	do j=1,k
	   temp(j)=simp(ihi,j)
	enddo
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
	fval(2)=func(kt,pnew)		!func value for these
c
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
	goto 1258
c
1253	if(.not.silent) print 1259
1259	format( ' Restart after local search')
	do 1260 j=1,k
1260	temp(j)=pnew1(j)
c
1258	do 1261 j=1,k
1261	step(j)=crtstp(j)	!small step size for restart
	goto 2001	!restart
c
1254	if(.not.silent) print 1262
1262	format( ' Return with best vertex')
	call mcopy(temp,theta,kt)		!copy temp into theta for output
	fmin=fval(1)
 	RETURN
c
1255	if(.not.silent) print 1263
1263	format( ' Return with averaged vertices')
	call mcopy(pnew,theta,kt)		!copy pnew into theta for output
	fmin=fval(2)
 	RETURN
c
1387	if(.not.silent) print 13871
13871	format( ' Return with absmin')
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

