	subroutine CJSIMP(vramp,kfit,jfix,nfix,titlep,fitted,logx,
     & dcurve,theta,nev,sres,smin,xval,yval,xmin,xmax,ymin,ymax,ndat,
     & icurvd,ypeak,ybase,tau1,xfit,yfit,ndv1,ndimd,idest)
c To fit data in CJFIT
c 07/15/91 04:48pm Yval made (2048,15) for compat with CJVDU (not needed here)
c Modif 03/26/91 10:23am so initial guesses (from CJVDU) provided
c Modif 03/21/91 11:56am so baseline can be fixed as mean of specified points
c  (args xval,yval,xmin,xmax,ymin,ymax,ndat,icurvd added)
	character*10 TITLEP(20)		!names of params for fit
	real*4 XVAL(ndv1,ndimd),YVAL(ndv1,ndimd)	!alloc in main prog
c=	real*4 XVAL(2048,15),YVAL(2048,15)
	dimension ndat(ndimd),icurvd(ndimd)
	real*4 Xfit(ndv1),Yfit(ndv1)		!now alloc in main prog
      character*1 ch
	character cnum*11,cnum1*11,cnum2*11
	integer fndcur
c	character*60 titmod(30)		!names of models
	character*10 t1(50)		!'standard' param names
	real*4 theta(20)
	real*4 theta0(20)
	integer JFIX(20)
c	integer*2 lrow
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	logical discprt,logx,abort
	LOGICAL constr,pon,fitted,dcurve,slock,negpar,vramp
	character*1 ans,UC,ans1
	character*4 tt1(10)
c declarations for popmenu
	character*55 strings(5),helps(5)
	character*20 title 			!for popmenu
c for exponentials when rising phase fitted
	logical fixy0
	common/fix/fixy0
c
	COMMON/BLOCK1/constr,Xv,kmax,nmod,ncomp,nfit,ip1,ip2,xjoin,slope
	common/minpar/thmin(20)
	common/dp/discprt
	common/block2/cMg
	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls
	common/err/nerr		!for dfpmin
	common/abt/abort		!for simplex and dfpmin
	DATA tt1/' b1 ',' b2 ',' b3 ',' b4 ',' b5 ',' b6 ',' b7 ',
     & ' b8 ',' b9 ','b10 '/
	external SSDCJ
	external SSDCJ1,DSSDCJ1	!for fitting exponentials with dfpmincj
c
c
	pon()=slock()
c
101	format(a1)
c for exponentials when rising phase fitted
	fixy0=.false.
	nfix=0
	do 39 i=1,20
39	jfix(i)=0
c Set minimum values for parameters if constrained positive (altered below for
c exponentials)
	do 62 i=1,20
62	thmin(i)=1.0e-20
c
	T1(1)='       K1 '
	t1(2)='       K2 '
	t1(3)='beta/alpha'
	t1(4)=' Erev(mV) '
	t1(5)='   g(E=0) '
	t1(6)='    H (mV)'
	t1(7)=' B/A (E=0)'
	t1(8)='   g(max) '
	t1(9)='   conc/K '
	t1(10)='g(Na) (pS)'
	t1(11)='g(Cs) (pS)'
	t1(12)='E(off)(mV)'
	t1(13)='H(K1) (mV)'
	t1(14)='H(K2) (mV)'
	t1(15)='     Y(0) '
c
c Select model and define parameter names (nmod=0 for exponentials)
	if(vramp) then
	   title='Choose equation'
	   strings(1)='Polynomial fit'
	   strings(2)='i/v with Exponential voltage-dep of current'
	   strings(3)='ditto with saturation (Beta/alpha V-dep)'
	   strings(4)='ditto with saturation (Affinity V-dep)'
	   strings(5)='i/v for GHK with magnesium block'
	   nval=5
	   iline=1	!default
         helps(1)='I=b0 + b1*V + b2*V*V + . . .'
	   helps(2)='                          '
	   helps(3)='                          '
	   helps(4)='                          '
	   helps(5)='                          '
	   nhelp=5
	   call POPMENU(nxlo,nylo,nyhi,strings,nval,ictm,icfm,icupm,
     &	 ibkm,title,helps,nhelp,iline,ans,ival)
c	   if(iline.ge.1.and.iline.le.nval) then	!iline=0 for ESC=cancel
	   nmod=iline
c=13	   call SCROLL(0,0,lastrow,79,1)	!scroll lines (0,5) only
c=	   call LOCATE(0,0)		!row lrow,col 0
c=	   print 12
c=12	   format(
c=     &   '&(1) Polynomial fit',/,
c=     &   ' (2) I/V with exponential voltage-dependence of current',/,
c=     &   ' (3) Ditto with saturation (beta/alpha V-dep)',/,
c=     &   ' (4) Ditto with saturation (affinity V-dep)',/,
c=     &   ' (5) I/V for GHK with Mg block',/,
c=     &   ' Model number = ')
c=	   call INPUTi(nmod)
c=	   if(nmod.lt.1.or.nmod.gt.5) goto 13
	   if(nmod.eq.5) goto 21
c
	   if(nmod.eq.1) then            !polynomial fit
		ip1=1
 		call DEFOLTi(ip1,defolt)
		call QDIALOG(1,
     &	'Degree of polynomial (1=line, 2=quadratic,...,10)'
     &	,defolt,11,cans)
		call GETINPi(cans,ip1)
c=	      call SCROLL(0,0,lastrow,79,1)	!scroll lines (0,5) only
c=	      call LOCATE(lrow,0)		!row lrow,col 0
c=		print 47
c=47		format(
c=     &	'&Degree of polynomial (1=line, 2=quadratic,...,10)= ')
c=		call INPUTi(ip1)
		if(pon()) write(7,52)ip1
      	if(discprt) write(8,52)ip1
52		format(/,' Polynomial of degree= ',i3,' fitted')
		kmax=ip1+1			!polynomial
c Can fit a different degree of polynomial ( eg a line) above x=xjoin
c as follows. Coeffs are the same for both polynomials up to coeff
c of X**ip1 (or X**ip2 if ip2<ip1)
		ip2=0		!use to signal disjoint polynomial (ip2.ne.0)
	      ans='N'
 	      call DEFOLTa(ans,defolt)
	      call QDIALOG(1,
     &	'Use a disjoint polynomial'
     &	,defolt,11,cans)
	      call GETINPa(cans,ans)
c=	      call SCROLL(0,0,lastrow,79,1)	!scroll lines (0,5) only
c=	      call LOCATE(lrow,0)		!row lrow,col 0
c=		print 48
c=48		format('& Use a disjoint polynomial [N] ? ')
c=		read 101,ans
		if(UC(ans).ne.'Y') goto 49
c=	      call SCROLL(0,0,lastrow,79,1)	!scroll lines (0,5) only
c=	      call LOCATE(lrow,0)		!row lrow,col 0
c=		print 50
c=50		format('&Make junction at x [must be 0. at present!] = ')
c=		call INPUTr(xjoin)
c=	      call SCROLL(0,0,lastrow,79,1)	!scroll lines (0,5) only
c=	      call LOCATE(lrow,0)		!row lrow,col 0
c=		print 51,xjoin
c=51		format('&Degree of polynomial above x = ',g12.4,' = ')
c=		call INPUTi(ip2)
		xjoin=0.0
 		call DEFOLTr(xjoin,defolt)
		call QDIALOG(1,
     &	'Make junction at x [must be 0. at present!]'
     &	,defolt,11,cans)
		call GETINPr(cans,xjoin)
		ip2=1
		call DCFORMAT(xjoin,6,2,cnum)
 		call DEFOLTi(ip2,defolt)
		call QDIALOG(1,
     &	'Degree of polynomial above x = '//charnb(cnum)
     &	,defolt,11,cans)
		call GETINPi(cans,ip2)
		if(ip2.gt.ip1) kmax=ip2+1	!to get right no of params
		if(pon()) write(7,521)xjoin,ip2
      	if(discprt) write(8,521)xjoin,ip2
521		format(
     & 	' Above x= ',g12.4,' polynomial of degree= ',i3,' fitted')
49		continue
		titlep(1)=t1(15)
		do 31 k=2,kmax
31		TITLEP(k)='    '//tt1(k)	!b1,b2,...
		goto 316		!MODEL DEFINED-START FITTING
	   endif
c CVFIT Models 16,17,18,21=2,3,4,5 here
c nmod=2
	   kmax=3
	   titlep(1)=t1(4)
	   titlep(2)=t1(5)
	   titlep(3)=t1(6)
	   if(nmod.eq.2) goto 316
	   kmax=5		!nmod=3
	   titlep(2)=t1(8)
	   titlep(4)=t1(9)
	   titlep(5)=t1(7)
	   if(nmod.eq.4) titlep(5)=t1(3)
	   goto 316		!MODEL DEFINED-START FITTING
c
c Model 5 ' (5) I/V for GHK with Mg block'
21	   continue
	   kmax=7
c29	   gNa=theta(1)		!conductance in pS
c30	   gCs=theta(2)		!conductance in pS
c31	   Eoff=theta(3)	!offset in Erev, relative to GHK value
c1	   aK1=theta(4)		!in mM
c3	   aK2=theta(5)		!in mM
c32	   H1=theta(6)
c33	   H2=theta(7)
	   titlep(1)=t1(10)
	   titlep(2)=t1(11)
	   titlep(3)=t1(12)
	   titlep(4)=t1(1)
	   titlep(5)=t1(2)
	   titlep(6)=t1(13)
	   titlep(7)=t1(14)
 	   call DEFOLTr(cMg,defolt)
	   call QDIALOG(1,
     &	'Magnesium concentration (mM)'
     &	,defolt,11,cans)
	   call GETINPr(cans,cMg)
c=	   call SCROLL(0,0,lastrow,79,1)	!scroll lines (0,5) only
c=	   call LOCATE(lrow,0)		!row lrow,col 0
c=	   print 317
c=317	   format('&Magnesium concentration (mM) = ')
c=	   call INPUTr(cMg)		!in common/block2/ for YCALCJ
c
316	   continue
c Print which model fitted (already done for polynomial; nmod=1)
	   if(nmod.eq.2) then
		if(pon()) write(7,55)
      	if(discprt) write(8,55)
55	      format(/,
     &   ' Fit I/V with exponential voltage-dependence of current')
	   else if(nmod.eq.3) then
		if(pon()) write(7,56)
      	if(discprt) write(8,56)
56	      format(/,
     &   ' Fit I/V with saturation (beta/alpha V-dep)')
	   else if(nmod.eq.4) then
		if(pon()) write(7,57)
      	if(discprt) write(8,57)
57	      format(/,
     &   ' Fit I/V with saturation (affinity V-dep)')
	   else if(nmod.eq.5) then
		if(pon()) write(7,58)
      	if(discprt) write(8,58)
58	      format(/,
     &   ' Fit I/V for GHK relation with Mg block')
	   endif
c Get guesses
	   negpar=.false.
	   do 131 i=1,kmax
c=	   call SCROLL(0,0,lastrow,79,1)	!scroll lines (0,5) only
c=	   call LOCATE(lrow,0)		!row lrow,col 0
c=	   print 132,i,TITLEP(i)
c=132	   format('&Parameter ',i2,': ',a10,'= ')
c=	   call INPUTr(theta(i))
	   call INTCONV(i,cnum)
 	   call DEFOLTr(theta(i),defolt)
	   call QDIALOG(1,
     &	'Parameter '//charnb(cnum)//': '//charnb(titlep(i))
     &	,defolt,11,cans)
	   call GETINPr(cans,theta(i))
	   if(theta(i).lt.0.) negpar=.true.
131	   continue
c
	else		!fit exponentials
c Will usually want initial and final current the same if rising phase fitted
c Just make sum of amplitudes=1 so final (say) amplitude is no longer
c a free parameter.  For simplex method this can be done within YCALCJ
c but for dfpmin method does not use ycalcj, so need to alter both
c ssdcj1 and dssdcj1.
	   nmod=0
53	   continue
	   if(ncomp.lt.1) ncomp=1
	   fixy0=.false.
 	   call DEFOLTi(ncomp,defolt)
	   call QDIALOG(1,
     &	'Number of exponential components to be fitted'
     &	,defolt,11,cans)
	   call GETINPi(cans,ncomp)
	   kmax=2*ncomp+1		!no of parameters
	   if(ncomp.eq.0) goto 53
c
	   ans='Y'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     &      'Fit rising phase too',defolt,11,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'Y') then
		ans='Y'
		call DEFOLTa(ans,defolt)
		call QDIALOG(1,
     &      'Fix initial current equal to asymptote',
     &   	  defolt,11,cans)
		call GETINPa(cans,ans)
		if(ans.eq.'Y') then
		   kmax=2*ncomp	!reduce # of free parameters by 1
		   fixy0=.true.
		endif
	   endif
403	   continue
c Set minimum values for parameters if constrained positive -make it 1.e-6
c for tau values (=1 nanosecond)
	   do 63 i=1,kmax-1,2
63	   thmin(i)=1.0e-6
c Define initial guesses
c	   amp=1.5*(ypeak-ybase)	!init guess
	   amp=0.9*(ypeak-ybase)	!when t=0 at 1st point fitted
	   if(ncomp.eq.1.or.fixy0) then
	      theta(1)=tau1
	      theta(2)=amp
	   else if(ncomp.eq.2) then
	      theta(1)=0.5*tau1
	      theta(2)=0.5*amp
	      theta(3)=5.*tau1
	      theta(4)=0.5*amp
	   endif
	   theta(kmax)=ybase
c
	   negpar=.false.
	   call WDIALOG(1,'Initial guesses:',11)
 	   call DEFOLTr(theta(1),defolt)
	   call QDIALOG(1,
     &	'(Param 1): tau(1) (ms)'
     &	,defolt,11,cans)
	   call GETINPr(cans,theta(1))
	   titlep(1)='  tau(1) '
 	   call DEFOLTr(theta(2),defolt)
	   call QDIALOG(1,
     &	'(Param 2): Amplitude of component 1'
     &	,defolt,11,cans)
	   call GETINPr(cans,theta(2))
	   titlep(2)='  amp(1) '
	   if(theta(2).lt.0.) negpar=.true.
	   if(ncomp.gt.1) then
		do 5 i=2,ncomp
		ix=2*i-1		!=3,5,7..
	      call INTCONV(ix,cnum)
	      call INTCONV(i,cnum1)
 	      call DEFOLTr(theta(ix),defolt)
	      call QDIALOG(1,
     &	'(Param '//charnb(cnum)//'): tau('//charnb(cnum1)//') (ms)'
     &	,defolt,11,cans)
		call GETINPr(cans,theta(ix))
	      titlep(ix)='  tau('//char(i+48)//') '
	      if(theta(ix).lt.0.) negpar=.true.
		if(fixy0.and.i.eq.ncomp) goto 40	!last amp not needed
	      call INTCONV(ix+1,cnum)
 	      call DEFOLTr(theta(ix+1),defolt)
	      call QDIALOG(1,
     &	'(Param '//charnb(cnum)//'): amplitude of component '//
     &	charnb(cnum1)
     &	,defolt,11,cans)
		call GETINPr(cans,theta(ix+1))
	      titlep(ix+1)='  amp('//char(i+48)//') '
	      if(theta(ix+1).lt.0.) negpar=.true.
5		continue
	   endif
c Now get asymptote
40	   continue
	   ans='N'
 	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     &	'Fix the asymptote'
     &	,defolt,11,cans)
	   call GETINPa(cans,ans)
	   if(UC(ans).eq.'Y') goto 38
	   call INTCONV(kmax,cnum)
	   def=theta(kmax)
 	   call DEFOLTr(def,defolt)
	   call QDIALOG(1,
     &	'(Param '//charnb(cnum)//'): asymptote'
     &	,defolt,11,cans)
	   call GETINPr(cans,theta(kmax))
	   titlep(kmax)='  Y(inf) '
	   if(theta(kmax).lt.0.) negpar=.true.
	   goto 381
c
38	   continue
	   iopt=1
 	   call DEFOLTi(iopt,defolt)
	   call QDIALOG(1,
     &	'(1) Define asymptote with cursors; (2) Type in value'
     &	,defolt,11,cans)
	   call GETINPi(cans,iopt)
	   jfix(kmax)=1	!fix theta(kmax)
	   nfix=1
	   if(iopt.eq.2) then
		call QDIALOG(1,
     &	'Specify value at which asymptote to be fixed = '
     &	,' ',11,cans)
	 	call GETINPr(cans,theta(kmax))
	   else
c		print 34
c34		format(
c     &      '&Mark START of baseline section with vertical cursor   ')
		call WDIALOG(1,
     &  'Mark START of baseline section with vertical cursor',12)
		ix=5000
		iy=7000
		call PUTCUR(ix,iy)
		ch=char(FNDCUR(ix1,iy1))
		call BELL(1)
	      x1=XWORLD(ix1)
		iltype=7       !dash-dot
		call MOVETO(x1,ymin,0,iltype)
		call MOVETO(x1,ymax,1,iltype)
		if(logx) x1=10.0*x1	!for log(x) scale -logx defined in cjvdu
		call WDIALOG(1,
     &  'Mark END of baseline section with vertical cursor',12)
		ix=9000
		iy=7000
		call PUTCUR(ix,iy)
		ch=char(FNDCUR(ix1,iy1))
		call BELL(1)
	      x2=XWORLD(ix1)
		iltype=7       !dash-dot
		call MOVETO(x2,ymin,0,iltype)
		call MOVETO(x2,ymax,1,iltype)
		if(logx) x2=10.0*x2	!for log(x) scale -logx defined in cjvdu
c 	Calc mean of these points
		s=0.0
		n=0
		j=icurvd(1)
		if(x1.lt.xval(1,j)) x1=xval(1,j)
		if(x2.gt.xval(ndat(j),j)) x2=xval(ndat(j),j)
		do 37 i=1,ndat(j)
		   x=xval(i,j)
		   if(x.lt.x1.or.x.gt.x2) goto 37
		   n=n+1
		   s=s+yval(i,j)
37		continue
		s=s/float(n)
		iltype=2
		if(logx) then
		   s1=alog10(x)
		else
		   s1=s
		endif
		call MOVETO(xmin,s1,0,iltype)
		call MOVETO(xmax,s1,1,iltype)
		call DCFORMAT(x1,8,1,cnum)
		call DCFORMAT(x2,8,1,cnum1)
		call DCFORMAT(s,7,2,cnum2)
		ans='Y'
 	      call DEFOLTa(ans,defolt)
	      call QDIALOG(1,
     &		'Asymptote from X = '//charnb(cnum)//' to '//
     &    		charnb(cnum1)//' = '//charnb(cnum2)//' O.K.',
     &		defolt,11,cans)
		call GETINPa(cans,ans)
		if(UC(ans).eq.'N') goto 38
		theta(kmax)=s
	   endif
c Check for zero guesses
381	   continue
	   do 401 i=1,kmax
	    if(theta(i).eq.0.0) then
		call BELL(2)
c		call SCROLL(0,0,lastrow,79,1)	!scroll lines (0,5) only
c		call LOCATE(lrow,0)		!row lrow,col 0
c		print 402
c402		format('&Zero initial guesses not allowed!')
		call WDIALOG(1,'Zero initial guesses not allowed!',12)
		goto 403		!new guesses
	    endif
401	   continue
c Print model
	   if(pon()) write(7,54) ncomp
         if(discprt) write(8,54) ncomp
54	   format(/,' Exponentials fitted (',i3,' components)')
	   if(fixy0) then
	      if(pon()) write(7,541)
            if(discprt) write(8,541)
541		format(' Amplitudes sum to zero so Y(0)=Y(infinity)')
	   endif
	endif		!end of guesses for exponentials
c
c Model number and initial guesses now in
	fitted=.true.
	dcurve=.false.
	ans='N'
 	call DEFOLTa(ans,defolt)
	call QDIALOG(1,
     &	'Use these values only to plot curve (no fit)'
     &	,defolt,11,cans)
	call GETINPa(cans,ans)
	if(UC(ans).eq.'Y') then
	   fitted=.false.
	   dcurve=.true.
 	   call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	   idest=201
	   RETURN
	endif
c
 	call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	errfac=1.e-4
	delmin=-1.		!do not use delmin for convergence
	confac=0.5		!parameter for simplex3
	stpfac=0.1
c
c	print 525
c525	format('&Relative error [1.e-4]= ')
c	call INPUTr(x)
c	if(x.gt.1.e-10) errfac=x
c	if(errfac.lt.0.) delmin=-errfac	!use delmin criterion
c
	if(nfix.eq.0) then
	   print 116
116	   format('&Number of parameters to be fixed [0]= ')
	   call INPUTi(nfix)
	else
	   print 1161
1161	   format(
     & '&Asymptote fixed: number of other parameters to be fixed [0]= ')
	   nf1=0
	   call INPUTi(nf1)
	   if(nf1.gt.0) nfix=nf1+1
	endif
	kfit=kmax-nfix
	if(nf1.gt.0) then
	   do 119 i=1,nf1
	   print 118,i
118	   format('&(',i2,'):   parameter #= ')
	   call INPUTi(i1)
	   jfix(i1)=1		!jfix(i)=0,1 if thata(i) free,fixed resp
119	   continue
	endif
c===========stay in graphics up to here?
c==========insert catch for zero initial guess?
c
	smin=SSDCJ(kmax,THETA,xfit,yfit,ndv1)		!for init guesses
	call DSPOUT(kmax,0,ndsav,Smin,theta,discprt)
c	if(kmax.ge.7) print 5231,1,smin,(theta(I),I=1,KMAX)
c	if(kmax.lt.7) print 5232,1,smin,(theta(I),I=1,KMAX)
c5231	format(i8,g10.3,/,10g10.3,/,10g10.3,/)
c5232	format(i6,7g11.4,/)
c
	do i=1,kmax
	   theta0(i)=theta(i)	!save guesses for simplex
	enddo
c
c
c Fit with dfpmin if fitting exponentials
	if(nmod.ne.0) goto 65
	if(nmod.eq.0.and.fixy0) goto 65
c
	do i=1,kmax-2,2
	   theta(i)=1.0/theta(i)	!convert tau to 1/tau for fitting
	enddo
	smin=SSDCJ1(THETA,xfit,yfit,ndv1)		!for init guesses
	call DSPOUT1(kmax,0,ndsav,Smin,theta,discprt)
c	if(kmax.ge.7) print 5231,1,smin,(theta(I),I=1,KMAX)
c	if(kmax.lt.7) print 5232,1,smin,(theta(I),I=1,KMAX)
c	pause 'Now dfpmin'
c      SUBROUTINE dfpmincj(theta,nt,n,gtol,iter,fret,func,dfunc,jfix,
c     & ndisp,xfit,yfit,ndv1)
	nerr=0
	print 66
66	format(/,
     & ' (1) Use SIMPLEX to fit',/,
     & ' (2) Use DFPMIN to fit',/,
     & ' Option number [1] = ')
	i=1
	call INPUTi(i)
	if(i.eq.1) goto 65
c
	k=0
	do j=1,kmax
	   if(jfix(j).eq.0) k=k+1        !number of free parameters
	enddo
	gtol=1.0e-4
	stpmx=1.0
	print 72,gtol
72	format(' Convergence criterion for slope [',g10.3,'] = ')
	call INPUTr(gtol)
	print 73,stpmx
73	format(' Maximum step length [',f7.3,'] = ')
	call INPUTr(stpmx)
	ndisp=1
	print 69
69	format(
     & '&Print every Nth estimate: N [1] = ')
	call INPUTi(i)
	if(i.ge.1) NDISP=i
	ndisp=-ndisp	!so prints to disc also
	itmax=200
	print 311
311	format(' F1 key to ABORT',/,'  -calculating...')
	call CLRKB		!Remove any waiting keyboard char: Utility lib
	abort=.false.
c      SUBROUTINE dfpmincj(theta,nt,n,gtol,iter,fret,func,dfunc,jfix,
c     & ndisp,xfit,yfit,ndv1)
      call DFPMINCJ(theta,kmax,k,gtol,iter,smin,ssdcj1,dssdcj1,stpmx,
     & itmax,jfix,ndisp,xfit,yfit,ndv1)
	sres=sqrt(Smin/float(nfit-kfit))	!ERROR S.D.
c Convert rate constants back to time constants
	do i=1,kmax-2,2
	   theta(i)=1.0/theta(i)	!convert tau to 1/tau for fitting
	enddo
	print 64
	if(discprt) write(8,64)
64	format(' End of fitting with DFPMIN')
	if(nerr.eq.5) then
	   print 70,itmax
	   if(discprt) write(8,70) itmax
70	   format(' Exceeded maximum number of iterations = ',i5)
	endif
	if(abort) then
	   print 71
	   if(discprt) write(8,71)
71	   format(/,' Run aborted ',/)
	endif
	call DSPOUT1(kmax,iter,ndsav,Smin,theta,discprt)
c	if(kmax.ge.7) then
c	   print 5231,iter,smin,(theta(i),i=1,kmax)
c	   if(discprt) write(8,5231) iter,smin,(theta(i),i=1,kmax)
c	else
c	   print 5232,iter,smin,(theta(i),i=1,kmax)
c	   if(discprt) write(8,5232) iter,smin,(theta(i),i=1,kmax)
c	endif
	print 5234, nfit,sres
	if(pon()) write(7,5234)NFIT,SRES
      if(discprt) write(8,5234)NFIT,SRES
5234	FORMAT(' Number of points= ',I8,/,' Error S.D.= ',G13.6,/)
c
	if(nerr.ne.0) then
	   print 67,nerr
67	   format(
     &	' Error #',i2,' in DFPMIN -fit again with SIMPLEX [Y] ? ')
	   ans='Y'
	else
	   print 68
68	   format(' Fit again with SIMPLEX [N] ? ')
	   ans='N'
	endif
	call INPUTa(ans)
	if(ans.eq.'N') then
	   goto 99		!results printed in cvout in main prog
	else
	   call CJOUT(nev,smin,jfix,fitted,titlep,
     &    vramp,vmin,vmax,Erev)
	endif
c
65	continue	!jump here for simplex
	do i=1,kmax
	   theta(i)=theta0(i)	!restore guesses for dpfmin
	enddo
c  Check initial guesses: if any are neg make default to NOT constrain
c to be positive
	if(negpar.or.nmod.eq.4) then
	   constr=.false.
	   ans='N'
	else
	   constr=.true.
	   ans='Y'
	endif
	print 134,ans
134	FORMAT('&Constrain parameters to be positive [',a1,'] ? ')
	read 101,ans1
	if(UC(ans1).eq.'N'.or.UC(ans1).eq.'Y') ans=ans1
	constr=ans.eq.'Y'
c
	irestrt=3
	print 312,irestrt
312	FORMAT(' Limit number of restarts to [',i2,']: n = ')
	call INPUTi(irestrt)
	ndisp=20
	print 523
523	format(
     & '&Print every Nth estimate: N [20] = ')
	call INPUTi(i)
	if(i.ge.1) NDISP=i
	nevm=-30000	!neg so iterations not printed to disc
	call SIMPCJF(kmax,THETA,stpfac,errfac,nev,nevm,
     & smin,SSDCJ,Ndisp,jfix,delmin,confac,irestrt,iconv,
     & xfit,yfit,ndv1)
	sres=sqrt(Smin/float(nfit-kfit))	!ERROR S.D.
	print 5233
	if(discprt) write(8,5233)
5233	FORMAT(' End of fitting with Simplex')
	if(abort) then
	   print 71
	   if(discprt) write(8,71)
c71	   format(/,' Run aborted ',/)
	endif
	call DSPOUT(kmax,Nev,ndsav,Smin,theta,discprt)
c	if(kmax.ge.7) then
c	   print 5231,nev,smin,(theta(i),i=1,kmax)
c	   if(discprt) write(8,5231) nev,smin,(theta(i),i=1,kmax)
c	else
c	   print 5232,nev,smin,(theta(i),i=1,kmax)
c	   if(discprt) write(8,5232) nev,smin,(theta(i),i=1,kmax)
c	endif
	print 5234, nfit,sres
	if(pon()) write(7,5234)NFIT,SRES
      if(discprt) write(8,5234)NFIT,SRES
c
99	continue
	RETURN
	end


