	subroutine ALIGNQ(avtrace,nsampav,inward,nyval,
     &  xval,yval,weight,icurvw,ndat,isym,ijoin,symsiz,
     &  icurvd,ndv1,ndimd,kwi,kwj,srate,tshift,reject)
c===============================================================
c To align queued traces before averaging, in CJFIT
c===restrict range for calc of SSD?
c===calc baseline from start not end?
c
	real*4 avtrace(nsampav)
	allocatable::scalav
	real*4 scalav(:),ax(4),ay(4)
	character*1 ans
	logical inward,redisp,reject
c for vplot
	real*4 Xval(ndv1,ndimd),Yval(ndv1,ndimd),weight(kwi,kwj)
	real*4 symsiz(ndimd)
	real*4 xcal(1,1),ycal(1,1)
	integer ndat(ndimd),isym(ndimd),ijoin(ndimd),icurvw(ndimd)
	integer icurvd(ndimd)
	logical fitted,doframe,autplt,interp,landscap
	character*40 titlex,titley
	character*44 title
	integer icol(100)
	logical mono
	COMMON/cols/icol,mono
c cursors
	character*1 ch
	character*11 cnum1,cnum2
	character*30 defolt,cans		!to hold text & result of QDIALOG
c For simplex
	real*4 theta(1)
	external SS
	jx=320
	jy=240
	ibk=icol(71)
	icf=2
	itype=0
	line=0
	idraw=-1
	islope=0
	jxlo=1
	jylo=56
	jxhi=639
	jyhi=386
	jy0=300
	ns=nsampav		!do not change nsampav until end
	dx=1000./srate	!ms between points
	redisp=.false.
	reject=.false.
c
c On entry, avtrace() contains the weighted sum, so must temporarily
c convert it to average for alignment with yval().  In fact best
c to scale so both avtrace and yval have the same peak current?
	if(inward) then
	   ym=1.e36
	   am=1.e36
	   do i=1,nyval
		if(yval(i,1).lt.ym) ym=yval(i,1)
	   enddo
	   do i=1,ns
		if(avtrace(i).lt.am) am=avtrace(i)
	   enddo
	else
	   ym=-1.e36
	   am=-1.e36
	   do i=1,nyval
		if(yval(i,1).gt.ym) ym=yval(i,1)
	   enddo
	   do i=1,ns
		if(avtrace(i).gt.am) am=avtrace(i)
	   enddo
	endif
c
c Also average last 50 points to get baseline
	s=0.0
	do i=nyval-50,nyval-1
	   s=s+yval(i,1)
	enddo
	basey=s/50.
	s=0.0
	do i=ns-49,ns
	   s=s+avtrace(i)
	enddo
	basea=s/50.
c
	sfac=abs(ym-basey)/abs(am-basea)
c	off=basea-basey
	ALLOCATE(scalav(ns))
	do i=1,ns
	   scalav(i)=basey+(avtrace(i)-basea)*sfac    !scale to same peak and base as yval()
	enddo
c
c	Make copy of yval1(i,1) in yval(i,jc) so can revert to the
c	unshifted version
	jc=3
	do i=1,nyval
	   yval(i,jc)=yval(i,1)
	enddo
c
c Try alignment by least squares.  Say lag=k points -try range of k values
c Current average (scaled) is in scalav(i), and has ns points
	kt=1
	theta(1)=50.0
	stpfac=0.1
	errfac=0.005
	nevmax=-250		!neg to signal no disc print
	ndisp=5
	delmin=-1.
	call SIMPLEXc(kt,THETA,stpfac,errfac,neval,nevmax,
     & fmin,SS,Ndisp,delmin,ns,scalav,yval,jc,ndv1,ndimd)
	kmin=ifixr(theta(1))
c Shift yval() by kmin places, and redefine nsampav
c We want the first usable point to be #1, so for left shift move elements
c of yval to the left, dropping the first kmin points. But for right shift,
c leave yval() unchanged, and shift avtrace to left by kmin points
c Positive kmin implies yval shifted to left (eg scalav(1)=yval(3) if kmin=2)
c See note at end re reduction in nsampav
100	continue	!return to try another shift
	k=iabs(kmin)
	if(kmin.gt.0) then
	   nmax=nyval-k
	   do i=1,nmax
		yval(i,jc)=yval(i+k,jc)		!shift yval for display
	   enddo
	else if(kmin.lt.0) then
	   nmax=ns-k
	   do i=1,nmax
		scalav(i)=scalav(i+k)		!shift scalav for display
	   enddo
	else
	   nmax=ns			!if kmin=0
	endif
	ns=MIN(ns,nmax)
c
	do i=1,ns
	   yval(i,2)=scalav(i)	!for display
	enddo
c
	do i=1,nyval
	   xval(i,2)=float(i-1)*dx
	   xval(i,jc)=xval(i,1)
	enddo
c
	if(.not.redisp) then
	   tshift=float(kmin)*dx
51	   print 1,tshift,fmin
1	   format(
     & ' Shift of ',f9.3,' ms (min SSD = ',g11.4,'): O.K. [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(ans.eq.'N') then
		print 31
31		format(' new shift (ms) = ')
		call INPUTr(tshift)
		kmin=ifixr(tshift/dx)
		shift=float(kmin)
	      tshift=float(kmin)*dx
		fmin=SS(kt,shift,ns,scalav,yval,jc,ndv1,ndimd)
		goto 51
	   endif
	endif
c Now display and align
	ncurvd=2
	icurvd(1)=2		!mean
	icurvd(2)=jc	!current trace
	ndat(2)=ns		!mean
	ndat(jc)=nyval	!current trace
	icurvw(2)=-1
	icurvw(jc)=-1
	isym(2)=0			!'points'
	isym(jc)=0			!'points'
	ijoin(2)=-1		!points not joined
	ijoin(jc)=-1		!points not joined
	cbig=2.5
	ifont=4
	ifitype=0		!no display of fitted parameters
	ilog=0
	itx=1			!normal tick orientation
	ity=1
	ntx=5
	nty=5
	ixlo=-1		!whole screen
	ilabel=1
	doframe=.true.
	landscap=.true.
	autplt=.false.
	fitted=.false.
	interp=.false.
	itit=0
	kthet=1
	titlex='time (ms)'
	titley='scaled current (pA)'
	do i=1,100
	   icol(i)=-1	!use default
	enddo
	isetcol=1
	icol(jc)=10		!green for current trace
	icol(2)=12		!red for mean
	ncurvc=0
	ndc1=1
	ndimc=1
	if(redisp) then
	   iscal=0
	else
	   iscal=1
	endif
c	iask=-2	!normal
c	iask=3	!come straight out of VPLOT2; leave graph on screen
	iask=2	!do not ask before leaving display; leave graph on screen
	call VPLOT5(Xval,Yval,ndat,icurvd,ncurvd,ijoin,symsiz,ndimd,
     &   XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,
     &   XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     &   iXLO,iXHI,iYLO,iYHI,y0,yinf,inumx,inumy,ncjump,nvjump,.false.,
     &   titlex,titley,ilabel,doframe,idiskq,autplt,plotonly,itit,title,
     &   cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,interp,
     &   isetcol,itrace,ndv1,ndc1,weight,kwi,kwj,icurvw,kthet)
c
	tshift=float(kmin)*dx
	call DCFORMAT(tshift,9,3,cnum1)
	call DCFORMAT(fmin,11,4,cnum2)
	ict=11	!default text colour
	call WDIALOG(1,
     &    'Shifted by '//charnb(cnum1)//' ms (min SSD = '//
     &     charnb(cnum2)//')',ict)
	ans='Y'
c	call DCASK(' O.K. to average this',ans,ans)
 	call DEFOLTa(ans,defolt)
	call QDIALOG(1,' O.K. to average this',defolt,ict,cans)
 	call GETINPa(cans,ans)
	if(ans.eq.'N') then
	   ns=nsampav	!revert to original arrays
	   do i=1,ns
		scalav(i)=basey+(avtrace(i)-basea)*sfac    !scale to same peak and base as yval()
	   enddo
	   do i=1,nyval
		yval(i,jc)=yval(i,1)
	   enddo
5	   continue
	   iopt=1
 	   call DEFOLTi(iopt,defolt)
	   call QDIALOG(1,
     & 	'(1) give new shift (2) Reject (green) trace:',
     &	defolt,ict,cans)
	   call GETINPi(cans,iopt)
	   if(iopt.eq.2) then
		reject=.true.
		goto 999
	   endif
	   iopt=1
 	   call DEFOLTi(iopt,defolt)
	   call QDIALOG(1,
     & 	'(1) mark shift with cursor (2) specify number:',
     &	defolt,ict,cans)
	   call GETINPi(cans,iopt)
	   if(iopt.eq.2) then
		call DCFORMAT(tshift,9,3,cnum1)
 		call DEFOLTr(charnb(cnum1),defolt)
		call QDIALOG(1,
     & 	' New shift (ms)',defolt,ict,cans)
		call GETINPr(cans,tshift)

		kmin=ifixr(tshift/dx)
		shift=float(kmin)
      	tshift=float(kmin)*dx
	   else if(iopt.eq.1) then
		call WDIALOG(1,
     &       'Click on alignment point for mean (red)',12)
	   idraw=-1
	   line=0
	   islope=0
	   call crossh(jx,jy0,jxlo,jylo,jxhi,jyhi,ibk,icf,itype,
     &	line,idraw,islope,ax,ay,6,70,6,1.)
	   t1=ax(1)
	   call BELL(1)
	   iltype=3		!dashed
	   call broken(iltype)
	   call graMOV(t1,ymin)
	   call gralin(t1,ymax)
		call WDIALOG(1,
     &       'Click on alignment point for trace (green)',10)
		call BELL(1)
	   call crossh(jx,jy0,jxlo,jylo,jxhi,jyhi,ibk,icf,itype,
     &	line,idraw,islope,ax,ay,6,70,6,1.)
      	t2=ax(1)
		iltype=3		!dashed
		call graMOV(t2,ymin)
		call gralin(t2,ymax)
		tshift=t2-t1+tshift	!trace already shifted, so add orig shift
		kmin=ifixr(tshift/dx)
		shift=float(kmin)
      	tshift=float(kmin)*dx
	   endif
	   s=SS(kt,shift,ns,scalav,yval,jc,ndv1,ndimd)
	   call DCFORMAT(tshift,9,3,cnum1)
	   call DCFORMAT(s,11,4,cnum2)
	   ans='Y'
 	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     &    'Shifted by '//charnb(cnum1)//' ms (SSD = '//
     &     charnb(cnum2)//')',defolt,ict,cans)
 	   call GETINPa(cans,ans)
	   if(ans.eq.'Y') then
		redisp=.true.
		goto 100
	   else
		goto 5
	   endif
	endif
c
c Now the shift is decided, define the output arrays, yval(i,1) and avtrace
c NB nsampav=number of values at present in the avtrace array -if this is
c shifted cannot go beyond point #nsampav. But if y array is shifted (kmin>0)
c then can go up to point #nyval, and if this exceeds nsampav by a number
c of points => k then no need to reduce nsampav
	tshift=float(kmin)*dx
	k=iabs(kmin)
	ns=nsampav	!revert to original nsampav
c===	ns=ns - k
	if(kmin.gt.0) then
	   nmax=nyval-k
	   do i=1,nmax
		yval(i,1)=yval(i+k,1)		!shift yval for output
	   enddo
	else if(kmin.lt.0) then
	   nmax=nsampav-k
	   do i=1,nmax
		avtrace(i)=avtrace(i+k)		!shift avtrace for output
	   enddo
	else
	   nmax=nsampav			!if kmin=0
	endif
	nsampav=MIN(nsampav,nmax)
c
999	continue
	DEALLOCATE(scalav)
 	call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	RETURN
	end

	function SS(kt,shift,ns,scalav,yval,jc,ndv1,ndimd)
c To calculate approximation to dS/dk where k=shift, but must
c be floating for bisection.  Calculated as slope=S(k+0.5)-S(k-0.5)
	real*4 shift(1),scalav(ns),yval(ndv1,ndimd)
c
	k=ifixr(shift(1))
	shift(1)=float(k)
	if(k.ge.0) then
	   i1=1
	   i2=ns-k
	else
	   i1=1-k
	   i2=ns
	endif
	s=0.0
	do i=i1,i2
	   d=scalav(i) - yval(i+k,jc)
	   s=s + d*d
	enddo
	s=s/float(ns-k)		!normalise by number of points
	SS=s
	RETURN
	end

	subroutine SIMPLEXc(kt,THETA,stpfac,errfac,neval,nevmax,
     & fmin,FUNC,Ndisp,delmin,ns,scalav,yval,jc,ndv1,ndimd)
	real*4 scalav(ns),yval(ndv1,ndimd)
c
c Version of SIMPLEX0 for alignq in cjfit
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
	real theta(kt)
	dimension step(20),crtstp(20)
	dimension temp(20),centre(20)
	dimension pnew(20),pnew1(20),fval(21),thmin(20),simp(21,20)
	logical silent,debtyp		!no print out at all; or only if caplock on
	logical deb
c	logical debug,caplock
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
	reffac=1.0		! reflection coeff => 1
	confac=.5		! contraction coeff 0 < beta < 1
	extfac=2.0		! extension factor > 1
	k=kt				!no fixed values in this version
	nv=k+1			! # of vertices in simplex
	neval=0			!counts ficntion evaluations
	do i=1,kt
	   temp(i)=theta(i)
	   step(i)=stpfac*temp(i)
	   crtstp(i)=errfac*temp(i)
	enddo
c
c
2001	continue	!return here for restart
	call NWVRTX(1,theta,simp,k)	!start values=vertex #1
	fval(1)=func(kt,theta,ns,scalav,yval,jc,ndv1,ndimd)		!func value for these
	fsav=fval(1)
	neval=neval+1
	absmin=fval(1)		!starting value
c
c ----- compute offset of the vertices of the starting simplex
	fac=(sqrt(float(nv))-1.)/(float(k)*sqrt(2.))
c
c ----- specify all other vertices of the starting simplex
c
	do i=2,nv
	  do j=1,k
	    simp(i,j)=simp (1,j) + step(j)*fac
	  enddo
	  simp(i,i-1)=simp(1,i-1) + step(i-1)*(fac+1./sqrt(2.))
c
c ----- and calculate their residuals
c
	  I1	= i
	  call RCOPY(I1,SIMP,theta,k)
	  fval(i1)=func(kt,theta,ns,scalav,yval,jc,ndv1,ndimd)		!func value for these
	  call abstst(fval(i1),theta,absmin,thmin,k)	!test for abs min
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
	do 151 i=2,nv
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
	CALL RCOPY(ILO,simp,theta,k)	!copy best vertex to theta
	call DSPOUT(kt,neval,ndsav,flo,theta,dprt)	!type it
1360	continue
	if(neval.gt.nevmax) goto 1510
c
c ----- compute centroid of all vertices except the worst
c
	do 1210 j=1,k
1210	  centre(j)= 0.0
	do 1220 i = 1,nv
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
	fnew=func(kt,pnew,ns,scalav,yval,jc,ndv1,ndimd)			!func value for these
	neval=neval+1
	call abstst(fnew,pnew,absmin,thmin,k)	!test for abs min
c -----
	if (fnew.ge.flo) goto 112	!fnew worst than prev best
c
c ----- new vertex is better than previous best so extend it
c
	do 1240 j= 1,k
1240	  pnew1(j) =centre(j) + extfac*(pnew(j)-centre(j))
	fnew1=func(kt,pnew1,ns,scalav,yval,jc,ndv1,ndimd)			!func value for these
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
	do 113 i=1,nv
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
	fnew1=func(kt,pnew1,ns,scalav,yval,jc,ndv1,ndimd)			!func value for these
	neval=neval+1
	call abstst(fnew1,pnew1,absmin,thmin,k)	!test for abs min
c
c ----- is contracted vertex better than the worst vertex
	if(fnew1.le.fval(IHI)) goto 110
c
c
c ----- no, it is still bad, shrink whole simplex towards best vertex
c
	do 118 i=1,nv
	  do 117 j=1,k
C	    simp(i,j) = (simp(i,j)+simp(ILO,J))*0.5	!orig version
	    simp(i,j) = simp(ILO,J)+confac*(simp(i,j)-simp(ILO,J))
c  last line is D.C. version that uses confac rather than 0.5
	    theta(j)=simp(i,j)
117	  continue
	fval(i)=func(kt,theta,ns,scalav,yval,jc,ndv1,ndimd)		!func value for these
	call abstst(fval(i),theta,absmin,thmin,k)	!test for abs min
118	continue
	neval=neval+nv
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
	do 181 i=1,nv
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
	GOTO 184
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
	do 1371 i=1,nv	!order values of current param
	if(simp(i,j).lt.simp(IL,j)) IL=i
1371	if(simp(i,j).gt.simp(IH,j)) IH=i
	if((simp(IH,j)-simp(IL,j)).gt.abs(crtstp(j))) L=0	!not conv
1370	continue
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
3841	format( ' RUN ABORTED')
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
	do 1400 j=1,k
	  pnew(j)=0.0
	  do 1390 i=1,nv
1390	    pnew(j)=pnew(j) + simp(i,j)
1400	  pnew(j)=pnew(j)/float(nv)
	fval(2)=func(kt,pnew,ns,scalav,yval,jc,ndv1,ndimd)		!func value for these
c
c do local search. Temp() already contains best vertex after convergence, with
c corresp function value in fval(1). Put altered values in pnew1
c 06/05/89 11:43am retain TEMP here
	do 124 j=1,k
	pnew1(j)=temp(j)+crtstp(j)
	fval(4)=func(kt,temp,ns,scalav,yval,jc,ndv1,ndimd)		!func value for these
	if(fval(4).lt.fnewlo) goto 125	!restart at better value
	pnew1(j)=temp(j)-crtstp(j)	!step in other direction
	fval(4)=func(kt,pnew1,ns,scalav,yval,jc,ndv1,ndimd)		!func value for these
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

c	subroutine MCOPY(a,b,k)
c	dimension a(20),b(20)
cc Copies A to B (for fast version)
c	do 1 i=1,k
c1	b(i)=a(i)
c	return
c	end
c
c
c	subroutine RCOPY(I,simp,A,K)
c	dimension A(20),simp(21,20)
cc
cc copies row I of simp() into A()
c	do 1 J=1,K
c1	A(J)=simp(I,J)
c	return
c	end
c
c	subroutine NWVRTX(I,A,simp,K)
c	dimension A(20),simp(21,20)
cc
cc copies A() into row I of simp()
c	do 1 J=1,K
c1	simp(I,J)=A(J)
c	return
c	end
c
c
c	subroutine DSPOUT(k,Neval,ndsav,fmin,TH,dprt)
cC TO DO PRINTING in simplex
c	real TH(20)
c	logical dprt
cC
c	if(k.le.6) then
c	   print 11,neval,fmin,(th(m),m=1,k)
c	   if(dprt) write(8,11) neval,fmin,(th(m),m=1,k)
c11	   format(i8,g13.6,/,6g13.6)
c	else
c	   print 10,neval,fmin,(th(m),m=1,k)
c	   if(dprt) write(8,10) neval,fmin,(th(m),m=1,k)
c10	   format(i8,g13.6,/,4(5g13.6,/))
c	endif
c	NDSAV=NEVAL
c	RETURN
c	END
c
c
c	subroutine abstst(fval,pnew,absmin,thmin,k)
c	dimension pnew(20),thmin(20)
cc
c	if(fval.gt. absmin) return	!fval no better
c	absmin=fval
c	do 1 j=1,k
c1	thmin(j)=pnew(j)
c	return
c	end


