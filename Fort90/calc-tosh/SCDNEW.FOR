	subroutine SCDISP(cjump,vjump,noise,pulse,cfac,pinf,bmj,eigen,
     & pinf0,bmj0,bmrel,eigen0,dgamma,t0,fc,G0,cv0,k,iplotsav,
     & varconc,xcal,ycal,ntime,dt,xcalsav,ycalsav,ncsav,plotsav,
     & prepulse,bmj0z,bmj0n,bmjz,bmjn,ndc1,ndimc,
     & ncalc,drcurve,Q1,align,bm,plotocc,plotcur,plotrate,
     & varate,titlep,npv,npulse,tpgap,nlig,y00,yinfeq,nchill)
c
c To do displays of jumps or noise in SCALCS
c Put k-1 components in xcal(i,j), j=2,10 with total in xcal(i,1)
c
c Modif 06/28/01 06:14pm: y00 and yinfeq, yinfp added for Hill plots
c
c Modif 03/15/01 02:21pm for 100 states/10 ligands
c Conc not used here so few changes apart from array sizes (col numbers
c in Ycal altered in vconc1,varconc case). Don't need nljmp etc?
c
c Modif 09/20/97 08:55pm to plot time course for aligned s-s bursts
c -logical align and real*8 bm(10) added to params for these calcs
c
c Modif 03/18/95 04:19pm for general prepulse case; in this case pinf0(10),
c eigen0(10),bmj0(10,10) are as in pulse case, but also plot for base
c where there were no openings during the prepulse (bmj0z,bmjz), and
c for case where there is at least one opening during prepulse (bmj0n,bmjn).
c
c Modif 05/04/95 12:22pm so total current trace saved (in xcalsav, ycalsav)
c so it can be superimposed on next plot.
c  Modif 01/05/92 06:55pm to plot response to pulses. Rates for ON and OFF may
c be very different, so cannot use same dx for both -instead use 512 points
c for each.
c NB real*8 pinf0(10),eigen0(10),bmj0(10,10) are used only for the
c 'pulse' case, in which they are values for the ON relaxation
c
c	real*8 Amat(10,10,10),eigen(10)
c	real*8 cur0(10),tau(10),curinf	!for relaxation (OFF for 'pulse' calc)
c	real*8 cur00(10),tau0(10),curinf0	!for ON relaxation ('pulse' calc only)
	real*8 Q1(100,100)
	real*8 pinf(100),eigen(100),bmj(100,100) !for relaxation (OFF for 'pulse' calc)
	real*8 pinf0(100),eigen0(100),bmj0(100,100) !for ON relaxation ('pulse' calc only)
	real*8 t1,ct,ctot,cfac1
	real*8 bmj0z(100,100),bmj0n(100,100)			!for prepulse calcs
	real*8 bmjz(100,100),bmjn(100,100)			!for prepulse calcs
	real*8 ctotz,ctotn,curtotz,curtotn,curtot0z,curtot0n	!ditto
	real*8 bmz(100),bmn(100),curz(100),curn(100)			!ditto
	real*8 bm0z(100),bm0n(100),cur0z(100),cur0n(100)		!ditto
	real*8 curtot,curinf,cur(100),tau(100),bm(100),bmrel(100),taumax
	real*8 curinf0,cur0(100),tau0(100),bm0(100),curtot0		!for on-jump
	real*8 dgamma(100),pt(100)
	real*8 popinf
	real*4 fc(100),G0(100),cv0(100)
	real*4 t0
	integer jstat(100)
c===	integer FNDCUR
	logical cjump,vjump,jump,noise,pulse,fstdisp,prepulse
	logical drcurve,plotocc,plotboth,plotcur,varate,plotrate
	character*10 titlep(200)
	logical align		!true for aligned s-s bursts
c for fitting etc
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character cnum1*11,cnum2*11		!for dialog
	real*4 theta(100)
	integer jfix(100)
	logical fixamp,newrange,errflag
c for display
	ALLOCATABLE Xval,Yval
	real Xval(:,:),Yval(:,:)		!for VPLOT
	real*4 Xcal(ndc1,ndimc),Ycal(ndc1,ndimc)
	real*4 xcalsav(ndc1),ycalsav(ndc1)
c	real*4 xcal(2048,10),ycal(2048,10)
c	real*4 XVAL(2048,1),YVAL(2048,1)
	real*4 xfit(2048),yfit(2048),weight(2048)
c for data
c	dimension ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd)
c for calc curves
c	dimension ncal(ndimc),icurvc(ndimc),iline(ndimc)
	integer*4 ndat(1),icurvd(1),isym(1),ijoin(1)	!for data
	real*4 syms(1)				!for data
c=c****NB must set ndimc=15 in SCALCS!**************
	dimension ncal(ndimc),icurvc(ndimc),iline(ndimc)	!for calc curve
	LOGICAL pon,debug,fitted,doframe,plotonly,autplt,pspec
	logical varconc,showconc,plotsav
	character*40 titlex,titley
	character*64 title1
	logical slock,caplock,landscap,ivplot,interp
	character*1 ans,UC
	integer*4 icurvw(1)		!for VPLOT5
	real*4 wght(1,1)
c For expmax
c==	real*4 wsc(10),taus(10),ws(10),wsz(10),wsn(10),curinfs
c==	common/exp/ wsc,taus,yinf1,ncomp1
c for expmaxd
	real*8 ypeak,tpeak,texp,thigh
	real*8 wsc(100),ws(100),wsz(100),wsn(100)
	common/expd/ wsc,tau,curinf,ncomp1
	logical gaddum
	common/gad/gaddum,tauzero
c
	logical discprt
	common/dp/discprt
	integer nbound(100,10)
	real*8 bound(100,10),bmax(10),binf
	common/nbnd/nbound
	common/KBLK/kA,kB,kC,kD
	COMMON/JLOGOS/t1c(10),t2c(10),t1v(10),t2v(10),xoff1,y1v,y2v,
     & y1c,y2c
	common/fitblk/xfit,yfit,weight,nfit,ncomp,fixamp
	integer icol(100)
	logical mono
	logical cluster,student
	COMMON/cols/icol,mono
	common/user/student,cluster,iwindows
c
	external SSDSCALC
c
c
c define function
	pon()=slock()
	debug()=caplock()
c
c Use F90 routine to prevent underflow crashes??
	errflag=.true.
	call UNDFL(errflag)
	ifitype=0	!no display of fitted parameters yet
	km=10
	idisp=1
	iplot=iplotsav	!default from last run
c
	do i=1,100
	   icol(i)=-1
	enddo
	isetcol=1	!use default colours except when icol set to valid colour
	plotonly=.false.
c
	ndv1=1 		!xval not used
	ndimd=1
	ALLOCATE(Xval(ndv1,ndimd),Yval(ndv1,ndimd))
c
	jump=vjump.or.cjump
	fitted=.false.
	fstdisp=.true.
	k=kA+kB+kC+kD
	cfac1=dble(cfac)
	if(pulse) then
	   t0ms=t0*1000.			!msec for ON-jump
	   tgms=tpgap*1000.	!msec from start of one pulse to start of next
c	   t1ms=dble(t0ms)
	endif
c
	if(align) then
	   vjump=.false.
	   cjump=.false.
	   jump=.false.
	   ncjump=0
	   nvjump=0
	   kE=kA+kB
	   ncomp=kE
	   ncomp1=kE				!for expmax
	   do m=1,kE
      	tau(m)=-1.d3/eigen(m)
c		cur(m)=cfac1*bm(m)
c=      	taus(m)=sngl(tau(m))		!for expmax
c      	ws(m)=sngl(cur(m))		!for expmax
c=      	ws(m)=sngl(bm(m))		!for expmax
      	ws(m)=bm(m)			!for expmaxd
c		curtot=curtot + cur(m)
	   enddo
c define values for parameter display
	   ncomp=kE
	   ncomp1=kE
	   k1=2*ncomp+1
	   do m=1,ncomp
		j1=2*m-1		!=1,3,5..
		theta(j1)=sngl(tau(m))
c		theta(j1+1)=sngl(cur(m))	!=theta 2,4,6..
		theta(j1+1)=sngl(bm(m))	!=theta 2,4,6..
	   enddo
	   theta(k1)=0.0
	   ifitype=4	!display calculated parameters
c define xcal, ycal, ncurvc,icurvc etc
	   ncalc=1024
	   ncalc2=512	!so save of last curve OK
	   if(fstdisp) then
		taumax=-1.d200
		do m=1,kE
		   if(tau(m).gt.taumax.and.dabs(bmrel(m)).gt.0.01d0) then
			taumax=tau(m)
			min=m
		   endif
		enddo
		xmin=0.
		xmax=4.0*tau(min)		!4*slowest tau with > 1% of amp in msec
	   endif
c
	   dx=(xmax-xmin)/float(ncalc)
c NB at present this is done for Popen, not for current
	   do i=1,ncalc
		xcal(i,1)=xmin+float(i-1)*dx
		t1=dble(xcal(i,1))
		ctot=0.0d0
		do m=1,kE		!the k-1 components
		   j=m+1		!column number for components
c		   ct=cur(m)*dexp(-t1/tau(m))
		   ct=bm(m)*dexp(-t1/tau(m))
		   ctot=ctot+ct
c		   Ycal(i,j)=sngl(ct + curinf) !offset component by I(inf) for display
		   Ycal(i,j)=sngl(ct)
		   Xcal(i,j)=Xcal(i,1)
		enddo
		Ycal(i,1)=sngl(ctot)		!total Popen
c		jc=1	!column # for total current
	   enddo
	   if(iplot.eq.0) iplot=2
         print 532,iplot
532	   format(
     & ' (1) Plot P(open) vs time (with exponential components)',/,
     & ' (2) Plot P(open) vs time only',/,
     & ' (3) Finish',/,
c     & ' (3) Plot occupancy of specified state(s) against time',/,
c     & ' (4) Plot occupancy of a specified state with components',/,
c     & ' (5) Plot specified transition frequencies vs time',/,
c     & ' (6) Finish',/,
     & ' Option number [',i2,'] = ')
	   call INPUTi(iplot)
	   iplotsav=iplot
	   if(iplot.eq.3) goto 999
	   if(iplot.eq.1) then
		ncurvc=kE+1
	   else if(iplot.eq.2) then
	      ncurvc=1
	   endif
	   do i=1,ncurvc
		icurvc(i)=i
		iline(i)=2
		ncal(i)=ncalc
	   enddo
	   iline(1)=0	!continuous for total
	   icol(11)=14	!yellow for overall
	   iscal=1		!scale internally
	   ilog=0
	   inumx=-1		!X axis in fixed (Fn.d) format
	   inumy=-1
	   fitted=.true.	!or params not displayed
	   titlex='time (ms)'
	   titley='P(open)'
	   cbig=2.5
	   ifont=4		!default is duplex
	   itit=0		!no title yet
	   itx=1			!normal tic orientation
	   ity=1			!normal tic orientation
	   ilabel=1
	   xlo=-1.		!whole screen
         doframe=.true.
	   landscap=.true.
	   autplt=.false.
	   ivplot=.false.
	   interp=.false.
c	   iask=2	!do not ask before leaving display; leave graph on screen
	   iask=-2		!leave with graph erased, no query
	   goto 100
	endif		!end of align
c
c For d-r curve case, xcal, ycal already defined so define labels etc
c and skip straight to VPLOT
	if(drcurve) idisp=1
152	continue
	if(drcurve) then
	   ncurvd=0
	   ncurvc=2
	   if(.not.plotocc) then
		if(plotrate) then
	       print 400,idisp
400	       format(/,
     & ' (1) Plot peak and equilibrium concentration-response curves',/,
     & ' (2) Plot Hill-slope of (1) against concentration',/,
     & ' (3) Plot predominant rate constant against concentration',/,
     & ' (4) Plot slope of (2) against concentration',/,
     & ' (5) Finish',/,
     & ' Option number [',i2,'] = ')
c     & ' (1) Plot peak and equilibrium concentration-response curves',/,
c     & ' (2) Plot predominant rate constant against concentration',/,
c     & ' (3) Plot slope of (2) against concentration',/,
c     & ' (4) Finish',/,
c     & ' Option number [',i2,'] = ')
	       call INPUTi(idisp)
		else
	       print 401,idisp
401	       format(/,
     & ' (1) Plot peak and equilibrium concentration-response curves',/,
     & ' (2) Plot Hill-slope of (1) against concentration',/,
     & ' (3) Plot predominant time constant against concentration',/,
     & ' (4) Plot slope of (2) against concentration',/,
     & ' (5) Finish',/,
     & ' Option number [',i2,'] = ')
		endif
	   else
		if(plotrate) then
	       print 402,idisp
402	       format(/,
     & ' (1) Plot peak and equilibrium agonist binding occ. curves',/,
     & ' (2) Plot Hill-slope of (1) against concentration',/,
     & ' (3) Plot predominant rate constant against concentration',/,
     & ' (4) Plot slope of (2) against concentration',/,
     & ' (5) Finish',/,
     & ' Option number [',i2,'] = ')
c     & ' (1) Plot peak and equilibrium agonist binding occ. curves',/,
c     & ' (2) Plot predominant rate constant against concentration',/,
c     & ' (3) Plot slope of (2) against concentration',/,
c     & ' (4) Finish',/,
c     & ' Option number [',i2,'] = ')
	       call INPUTi(idisp)
		else
	       print 403,idisp
403	       format(/,
     & ' (1) Plot peak and equilibrium agonist binding occ. curves',/,
     & ' (2) Plot Hill-slope of (1) against concentration',/,
     & ' (3) Plot predominant time constant against concentration',/,
     & ' (4) Plot slope of (2) against concentration',/,
     & ' (5) Plot KA=tau(0)/[d(tau)/dxA]',/,
     & ' (6) Finish',/,
     &  ' Option number [',i2,'] = ')
c     & ' (1) Plot peak and equilibrium agonist binding occ. curves',/,
c     & ' (2) Plot predominant time constant against concentration',/,
c     & ' (3) Plot slope of (2) against concentration',/,
c     & ' (4) Plot KA=tau(0)/[d(tau)/dxA]',/,
c     & ' (5) Finish',/,
c     &  ' Option number [',i2,'] = ')
	       call INPUTi(idisp)
		 if(idisp.eq.6) then
		    idisp=5		!finish
		 else if(idisp.eq.5) then
		    idisp=6		!plot
		 endif
		endif
	   endif
	   if(idisp.eq.5) goto 999
c
	   ilog=1  	!semilog plot
	   do i=1,4
	      ncal(i)=ncalc
	      ncal(i)=ncalc
	   enddo
	   do i=1,5,2
	      iline(i)=0		!equilib continuous
	      iline(i+1)=2		!peak dashed
	   enddo
c
	   if(varate) then
	      titlex=titlep(npv)	!name of rate constant that varies
	   else
	      titlex='concentration'
	   endif
	   if(idisp.eq.1) then
c=		titley='current (pA)'
	      icurvc(1)=1
	      icurvc(2)=2
		if(.not.plotocc) then
		   if(plotcur) then
		      titley='Abs. current (P(open) (peak and equilib.)'
		   else
		      titley='Agonist P(open) (peak and equilib.)'
		   endif
		else
		   titley='Binding occupancy (peak and equilib.)'
		endif
		icol(11)=14	!yellow for equilib current
		icol(12)=12	!red for peak current

		if(.not.plotocc) then
	         print 66
	         if(discprt) write(8,66)
66		  format(/
     &	' Yellow = equilib P(open); red (dashed) = peak P(open)')
		else
	         print 661
	         if(discprt) write(8,661)
661		  format(/
     &' Yellow = equilib agonist binding occ; red (dashed)= peak occ.')
		endif
	   else if (idisp.eq.2) then	!Hill slope vs conc
		ncurvc=1
	      icurvc(1)=9
		ncal(9)=nchill
		iline(9)=0	!continuous
		ilog=1  	!semilog plot
		if(.not.plotocc) then
		   if(plotcur) then
		      titley='Hill slope for equilibrium response curve'
		   else
		      titley='Hill slope for equilibrium Popen curve'
		   endif
		else
		   titley='Hill slope for equilibrium binding curve'
		endif
		icol(11)=14	!yellow for equilib
	   else if (idisp.eq.3) then
		if(plotrate) then
		   titley='rate constant'
		   print 67
	         if(discprt) write(8,67)
67		   format(/
     &	' Yellow = rate constant with largest amplitude',/,
     &     ' red (dashed) = rate constant from time to (1-1/e) of peak')
		else
		   titley='time constant'
		   print 671
	         if(discprt) write(8,671)
671		   format(/
     &	' Yellow = time constant with largest amplitude',/,
     &     ' red (dashed) = time constant from time to (1-1/e) of peak')
		endif
		ilog=0
	      icurvc(1)=3
	      icurvc(2)=4
		icol(11)=14	!yellow
		icol(12)=12	!red
	   else if (idisp.eq.4) then
		if(plotrate) then
		   titley='d(rate)/d(conc) (1/(sec x muM))'
		else
		   titley='d(tau)/d(conc) (ms/muM)'
		endif
	      icurvc(1)=5
	      icurvc(2)=6
		ncal(5)=ncalc-1
		ncal(6)=ncalc-1
		icol(11)=14	!yellow for equilib current
		icol(12)=12	!red for peak current
		if(plotrate) then
	        print 68
	        if(discprt) write(8,68)
68		  format(/,' d(rate)/d(conc) [1/(sec x muM)] from:',/,
     &     ' Yellow = rate constant with largest amplitude',/,
     &  ' red (dashed)  = rate constant from t to (1-1/e) of peak')
		else
	        print 681
	        if(discprt) write(8,681)
681		  format(/,' d(tau)/d(conc) (ms/muM) from:',/,
     &     ' Yellow = time constant with largest amplitude',/,
     &  ' red (dashed)  = time constant from t to (1-1/e) of peak')
		endif
	   else if (idisp.eq.6) then
		titley='KA=tau(0)/[d(tau)/d(conc)] (muM)'
		ncurvc=2
	      icurvc(1)=7
	      icurvc(2)=8
		ncal(7)=ncalc-1
		ncal(8)=ncalc-1
		icol(11)=14	!yellow for predom rate
		icol(12)=12	!red for texp
	   endif
	   pause ' Now the graph'
c index in icol()=11-20 for calc curves (#1-10);
	   iscal=1		!scale internally
	   inumx=-1		!X axis in fixed (Fn.d) format
	   inumy=-1
	   goto 90	    			!define other values for vplot
	endif		!end of drcurve
c
c For npulse>1 case, xcal, ycal already defined in dpulse so define labels etc
c and skip straight to VPLOT
c Allocation to columns is different from other cases
c j=1 current
c j=2 Popen
c j=3 binding
c j=4 to j=3+k =occupancy of all k states (state #=j-3)
102	if(npulse.gt.1) then
	   ncurvd=0
	   print 41,idisp
41	   format(/,
     & ' (1) Plot current vs. time',/,
     & ' (2) Plot P(open) vs. time',/,
     & ' (3) Plot agonist binding vs time',/,
     & ' (4) Plot occupancy of specified state(s) against time',/,
     & ' (5) Finish',/,
     & ' Option number [',i2,'] = ')
	   call INPUTi(idisp)
	   if(idisp.eq.5) goto 999
c
	   ilog=0  	!arithmetic plot
	   do i=1,k+3
	      ncal(i)=ntime
	   enddo
	   titlex='time (ms)'
	   do i=1,5,2
	      iline(i)=0		!equilib continuous
	      iline(i+1)=2		!peak dashed
	   enddo
c
	   if(idisp.eq.1) then
	      ncurvc=1
	      icurvc(1)=1
	      iline(i)=0		!continuous
		titley='current (pA)'
c          rescale here for very small currents
		ym=0.0
		do i=1,ncal(1)
		   y=abs(ycal(i,1))
		   if(y.gt.ym) ym=y
		enddo
		if(ym.le.0.1) then
		   do i=1,ncal(1)
			ycal(i,1)=1000.*ycal(i,1)
		   enddo
		   titley='current (fA)'
		endif
		icol(11)=1	!blue
c		icol(12)=12
	   else if (idisp.eq.2) then
	      ncurvc=1
	      icurvc(1)=2
	      iline(i)=0		!continuous
		titley='P(open)'
		icol(11)=1	!blue
		ilog=0
	   else if (idisp.eq.3) then
	      ncurvc=1
	      icurvc(1)=3
	      iline(i)=0		!continuous
		titley='Agonist binding occupancy'
		icol(11)=1	!blue
		ilog=0
	   else if(idisp.eq.4) then
		print 54
c54	   	format(' Number of states to be plotted [all] = ')
		nstat=k	!default
		call INPUTi(nstat)
	      if(nstat.lt.k) then
		   do i=1,nstat
			print 56,i,i
c56			format('& ',i2,': plot state number [',i3,'] = ')
	      	call INPUTi(jstat(i))
		   enddo
	      else if(nstat.eq.k) then
		   do i=1,k
			jstat(i)=i
		   enddo
		endif
	      titley='state occupancy'
		ncurvc=nstat
		do i=1,nstat	!number of states to be plotted
		   j=jstat(i)	!state number
		   icurvc(i)=j+3		!occs in cols j+3 to j+k
		   iline(j)=0		!total curve continuous
		   ncal(j)=ntime
		enddo
		isetcol=1	!use default colours except when icol set to valid colour
		icol(11)=10
		icol(12)=11
		icol(13)=12
		icol(14)=13
		icol(15)=14
		icol(16)=1
		icol(17)=2
		icol(18)=3
		icol(19)=5
		icol(20)=6
	   endif
	   pause ' Now the graph'
c index in icol()=11-20 for calc curves (#1-10);
	   iscal=1		!scale internally
	   inumx=-1		!X axis in fixed (Fn.d) format
	   inumy=-1
	   ifitype=0
	   goto 90	    			!define other values for vplot
	endif		!end of npulse>1

c Do calculations for time-dependent concentration case, for which Xcal
c and Ycal have already been defined in VCONC1.  Ycal(i,j) contains occ
c of state j at time i (i=1 for t=0)
60	continue
	if(varconc) then
	   showconc=.false.
	   curtot=0.0d0
	   do j=1,kA
		curtot=curtot + cfac1*dgamma(j)*dble(ycal(1,j))
	   end do
	   ncurvd=0		!no data
	   xmax=xcal(ntime,1)
	   iscal=1		!scale internally
	   ilog=0
	   inumx=-1		!X axis in fixed (Fn.d) format
	   inumy=-1
	   titlex='time (ms)'
	   titley='occupancy'
c
	   if(iplot.eq.0) iplot=1
         print 531,iplot
531	   format(
     & ' (1) Plot current only',/,
     & ' (2) Plot occupancy of specified state(s) against time',/,
     & ' (3) Finish',/,
     & ' Option number [',i2,'] = ')
	   call INPUTi(iplot)
	   iplotsav=iplot
	   if(iplot.eq.3) goto 999
c      current now in ycal(i,k+1)
	   if(iplot.eq.1) then
		do i=1,ntime
		   ctot1=0.0
		   do j=1,kA
			ctot1=ctot1 + sngl(cfac1*dgamma(j))*ycal(i,j)
		   end do
		   ycal(i,k+1)=ctot1	!put current in j=10
c		   jc=10	!column # for total current
		   xcal(i,k+1)=xcal(i,1)
		end do
		ncurvc=1
		icurvc(1)=k+1		!plot ycal(i,10)
		iline(k+1)=0		!total curve continuous
		ncal(k+1)=ntime
	      titley='current (pA)'
c          rescale here for very small currents
		ym=0.0
		do i=1,ncal(k+1)
		   y=abs(ycal(i,k+1))
		   if(y.gt.ym) ym=y
		enddo
		if(ym.le.0.1) then
		   do i=1,ncal(k+1)
			ycal(i,k+1)=1000.*ycal(i,k+1)
		   enddo
		   titley='current (fA)'
		endif
	   else if(iplot.eq.2) then
		print 54
c54	   	format(' Number of states to be plotted [all] = ')
		nstat=k	!default
		call INPUTi(nstat)
	      if(nstat.lt.k) then
		   do i=1,nstat
			print 56,i,i
c56			format('& ',i2,': plot state number [',i3,'] = ')
	      	call INPUTi(jstat(i))
		   end do
	      else if(nstat.eq.k) then
		   do i=1,k
			jstat(i)=i
		   end do
		endif
	      titley='occupancy'
		ncurvc=nstat
		do i=1,nstat	!number of states to be plotted
		   j=jstat(i)	!state number
		   icurvc(i)=j		!plot ycal(i,j)
		   iline(j)=0		!total curve continuous
		   ncal(j)=ntime
		end do
		isetcol=1	!use default colours except when icol set to valid colour
		icol(11)=10
		icol(12)=11
		icol(13)=12
		icol(14)=13
		icol(15)=14
		icol(16)=1
		icol(17)=2
		icol(18)=3
		icol(19)=5
		icol(20)=6
	   endif
c
	   goto 90	!do graph
	endif		!end if (varconc)
c End of section for doing time-dep concentration
c
c Define values for relaxation of current (OFF relaxation in case of pulse)
	curinf=0.0d0
	do j=1,kA
	   curinf=curinf+cfac1*dgamma(j)*pinf(j)
	enddo
c==	curinfs=sngl(curinf)		!for expmax
	ncomp=k-1
	ncomp1=k-1			!for expmax
	curtot=0.0d0
	do m=1,k-1
         tau(m)=-1.d3/eigen(m)
	   bm(m)=0.0d0
	   do j=1,kA
	     bm(m)=bm(m) + dgamma(j)*bmj(m,j)
	   enddo
	   cur(m)=cfac1*bm(m)
c=       taus(m)=sngl(tau(m))		!for expmax
c=       ws(m)=sngl(cur(m))		!for expmax
         ws(m)=bm(m)			!for expmaxd
	   curtot=curtot + cur(m)
	   if(prepulse) then
	     bmz(m)=0.0d0
	     do j=1,kA
	       bmz(m)=bmz(m) + dgamma(j)*bmjz(m,j)
	     enddo
	     curz(m)=cfac1*bmz(m)
c=         wsz(m)=sngl(curz(m))		!for expmax
           wsz(m)=curz(m)			!for expmaxd
	     curtotz=curtotz + curz(m)
c
	     bmn(m)=0.0d0
	     do j=1,kA
	       bmn(m)=bmn(m) + dgamma(j)*bmjn(m,j)
	     enddo
	     curn(m)=cfac1*bmn(m)
	     curtotn=curtotn + curn(m)
c=	     wsn(m)=sngl(curn(m))		!for expmax
           wsn(m)=curn(m)			!for expmaxd
	   endif
	enddo
c
c For pulse, define values for ON relaxation of current
	if(pulse) then
	   curinf0=0.0d0
	   do 371 j=1,kA
371	   curinf0=curinf0+cfac1*dgamma(j)*pinf0(j)
	   curtot0=0.0d0
	   do m=1,k-1	!define bm0 etc for ON relaxation
      	tau0(m)=-1.d3/eigen0(m)		!for pulse
		bm0(m)=0.0d0
		do j=1,kA
	 	   bm0(m)=bm0(m) + dgamma(j)*bmj0(m,j)
		enddo
		cur0(m)=cfac1*bm0(m)
		curtot0=curtot0 + cur0(m)
		if(prepulse) then
		   bm0z(m)=0.0d0
		   do j=1,kA
	 		bm0z(m)=bm0z(m) + dgamma(j)*bmj0z(m,j)
		   enddo
		   cur0z(m)=cfac1*bm0z(m)
		   curtot0z=curtot0z + cur0z(m)
c
		   bm0n(m)=0.0d0
		   do j=1,kA
	 		bm0n(m)=bm0n(m) + dgamma(j)*bmj0n(m,j)
		   enddo
		   cur0n(m)=cfac1*bm0n(m)
		   curtot0n=curtot0n + cur0n(m)
		endif
	   enddo
	endif
c
c Define values for display
90	continue
c define values for param display
	ncomp=k-1
	ncomp1=k-1				!for expmax
	k1=2*ncomp+1
	if(.not.plotocc) then
	   do m=1,ncomp
		j1=2*m-1		!=1,3,5..
		theta(j1)=sngl(tau(m))
		theta(j1+1)=sngl(cur(m))	!=theta 2,4,6..
	   enddo
	   theta(k1)=sngl(curinf)
	   ifitype=4	!display calculated parameters
	endif
	if(drcurve) ifitype=0
	fitted=.true.	!or params not displayed
	cbig=2.5
	ifont=4		!default is duplex
	itit=0		!no title yet
	itx=1			!normal tic orientation
	ity=1			!normal tic orientation
	ilabel=1
	xlo=-1.		!whole screen
	doframe=.true.
	landscap=.true.
	autplt=.false.
c==	isdev=-1		!no SD bars -not used in vplot5
	ivplot=.false.
	interp=.false.
c	iask=2	!do not ask before leaving display; leave graph on screen
	iask=-2		!leave with graph erased, no query
c
c	if(varconc) goto 100	!jump to VPLOT -all defined above
	if(varconc) goto 325	!jump to VPLOT -all defined (skip expmaxd)
c
c Jump logos- a simple jump is always at t=0 so not much to draw -do logo
c only for pulse response for now
	nvjump=0	!not fixed for V-pulse yet!
	if(jump) then
	   if(pulse) then
		ncjump=npulse
		do i=1,ncjump
		   t1c(i)=float(i-1)*tgms	!=0, tgms, 2tgms etc
		   t2c(i)=t1c(i) + t0ms		!end of jump (msec)
		enddo
	    else
		ncjump=0
	    endif
	endif
	if(npulse.gt.1) then
	   ifitype=0
	   goto 325	!jump to vplot
	endif
c
c idisp=1 for jumps, =2 for spectrum, =3 for autocovariance
	if(.not.drcurve) then
	   if(noise) idisp=2		!default
	   if(jump.or.varconc) idisp=1
	endif
c
52	continue
	if(drcurve) goto 325	!plot d/r curves

	newrange=.false.
	if(.not.fstdisp.and.(.not.varconc).and.(.not.noise)) then
	   print 521,xmin,xmax
521	   format(
     &    ' At present plotted between ',g13.6,' and ',g13.6,' ms:')
	   call DCASK(
     &    'Recalculate values for a new time range','n',ans)
c	   read 101,ans
c101	   format(a1)
	   if(UC(ans).eq.'Y') then
		print 522,xmin,xmax
522		format(' t(min), t(max) [',f10.2,f10.2,'] = ')
		call INPUT2r(xmin,xmax)
		newrange=.true.
	      idisp=1
	   endif
	endif
	if(jump) then
	   print 30
30	   format(' (1) Display relaxation')
	   idisp=1
	endif
	if(noise) then
	   print 31
31	   format(
     &  ' (2) Display noise as spectral density function',/,
     &  ' (3) Display noise as autocovariance function')
c=	   idisp=2
	endif
	if(.not.fstdisp.and.(.not.newrange)) idisp=4
	print 32,idisp
32	format(
     & ' (4) Finish',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(idisp)
	if(idisp.eq.4.and.(.not.drcurve)) then
c Save current so it can be superimposed on the next plot if req.
	   do i=1,ncal(1)
		Xcalsav(i)=Xcal(i,1)
		Ycalsav(i)=Ycal(i,1)
		plotsav=.true.
	   enddo
	   ncsav=ncal(1)
	   goto 999
	endif
	pspec=idisp.eq.2
	if(varconc) goto 60
	goto (50,51,51) idisp
c
50	continue
c Plot relaxation
c Define range for calc curve initially from t=0 up to t=4*slowest time
c constant, ie tau(k-1)
c When pulse=true define t=0 as start of ON-jump which extends from
c t=0 to t=t0, then plot OFF-jump from t0 to t0+4*slowest tau
	if(iplot.eq.0) iplot=2
      print 53,iplot
53	format(
     & ' (1) Plot current (with exponential components)',/,
     & ' (2) Plot current only',/,
     & ' (3) Plot binding only',/,
     & ' (4) Plot current + binding',/,
     & ' (5) Plot occupancy of specified state(s) against time',/,
     & ' (6) Plot occupancy of a specified state with components',/,
     & ' (7) Plot specified transition frequencies vs time',/,
     & ' (8) Finish',/,

c     & ' (3) Plot occupancy of specified state(s) against time',/,
c     & ' (4) Plot occupancy of a specified state with components',/,
c     & ' (5) Plot specified transition frequencies vs time',/,
c     & ' (6) Finish',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(iplot)
	iplotsav=iplot
c  keep old iplot numbering
	plotocc=iplot.eq.3
	plotboth=iplot.eq.4
	noccplot=1		!if nlig=1
	if(nlig.eq.2.and.(iplot.eq.3.or.iplot.eq.4)) then 	!which ligand(s)?
	   print 42
42	   format(
     &   ' (1) Plot occupancy of ligand 1 only',/,
     &   ' (2) Plot occupancy of ligand 2 only',/,
     &   ' (3) Plot occupancy of both ligands',/,
     &   ' Option number [3] = ')
	   noccplot=3
	   call INPUTi(noccplot)
	endif
	if(iplot.eq.3.or.iplot.eq.4) iplot=2	!for options 2,3,4 above
	if(iplot.ge.5.and.iplot.le.8) then
	   iplot=iplot-2	!iplot=3 to 6
	endif
c
	if(iplot.eq.6) goto 999
c
	if(iplot.eq.5) then	!transition freqs
	   if(pulse) then
		print 64
64		format(' Transition frequencies not fixed for pulses yet!')
		goto 50
	   endif
63	   print 61
61	   format(' Plot p(i,t)*q(i,j) and p(j,t)*q(j,i): i,j = ')
	   call INPUT2i(i,j)
	   print 62,i,j,Q1(i,j),j,i,Q1(j,i)
62	   format(' Q1(',2i2,') = ',g13.6,' Q1(',2i2,') = ',g13.6)
	   if(Q1(i,j).le.0.0d0.or.Q1(j,i).le.0.0d0) goto 63
	   jstat(1)=i
	   jstat(2)=j
	   nstat=2
	else if(iplot.eq.3) then
	   print 54
54	   format(' Number of states to be plotted [all] = ')
	   nstat=k	!default
	   call INPUTi(nstat)
	   if(nstat.lt.k) then
		do 55 i=1,nstat
		print 56,i,i
56			format('& ',i2,': plot state number [',i3,'] = ')
55	      call INPUTi(jstat(i))
	   else if(nstat.eq.k) then
		do 57 i=1,k
57		jstat(i)=i
	   endif
	   plotocc=.false.
	   if(.not.pulse) then		!not yet fixed for  pulse
		ans='Y'
		call DCASK('Also plot agonist binding',ans,ans)
		plotocc=ans.eq.'Y'
	   endif
	   isetcol=1	!use default colours except when icol set to valid colour
	   icol(11)=10
	   icol(12)=11
	   icol(13)=12
	   icol(14)=13
	   icol(15)=14
	   icol(16)=1
	   icol(17)=2
	   icol(18)=3
	   icol(19)=5
	   icol(20)=6
	else if(iplot.eq.4) then
	   nstat=1
	   print 58
58	   format('& Plot state number = ')
	   call INPUTi(jstat(1))
c	   read 3,jstat(1)
	endif
c
c Set tmax for plot.  Use 4*slowest time constant, unless amplitude of
c latter is very small.  Say use 4* the slowest time constant that has
c an amplitude at least 1% of the biggest amplitude
c Relative amps were calculated (in RELOUT) in bmrel(m)
	if(fstdisp) then
	   taumax=-1.d200
	   do m=1,k-1
		if(tau(m).gt.taumax.and.dabs(bmrel(m)).gt.0.01d0) then
		   taumax=tau(m)
		   min=m
		endif
	   enddo
c	  bmin=1.d200
c	  do m=1,k-1
c	   if(bmrel(m).lt.bmin.and.dabs(bmrel(m)).gt.0.01d0) then
c		bmin=bmrel(m)
c		min=m
c	   endif
c	  end do
	endif
c
c	ncalc=512
	ncalc=1024
	ncalc2=ncalc		!except for pulses
c Get max number bound in case needed , and define bound(j).
	do n=1,nlig
	   bmax(n)=0.0d0
	   do i=1,k
		bound(i,n)=dble(float(nbound(i,n)))
		if(bound(i,n).gt.bmax(n)) bmax(n)=bound(i,n)
	   enddo
	enddo
	if(fstdisp) then
	   xmin=0.
c	   xmax=4.0*tau(k-1)		!4*slowest tau in msec
	   xmax=4.0*tau(min)		!4*slowest tau with > 1% of amp in msec
	endif
	if(pulse) then
	   ncalc2=ncalc
	   ncalc=2*ncalc
	   xmax=xmax+t0ms
	endif
c Calc values for currents
	if(iplot.eq.1.or.iplot.eq.2) then	!plot currents
	   if(.not.pulse) then
		dx=(xmax-xmin)/float(ncalc)
		do i=1,ncalc
		   xcal(i,1)=xmin+float(i-1)*dx
		   t1=dble(xcal(i,1))
		   ctot=0.0d0
		   do 11 j=2,k		!the k-1 components
			m=j-1			!components 1 to k-1
		 	ct=cur(m)*dexp(-t1/tau(m))
			ctot=ctot+ct
		 	Ycal(i,j)=sngl(ct + curinf) !offset component by I(inf) for display
			Xcal(i,j)=Xcal(i,1)
11   		   continue
		   Ycal(i,1)=sngl(ctot + curinf)		!total current
c		   jc=1	!column # for total current
c If occupancy required too, then must calculate all state occupancies
c (except those for states with no ligand not needed) and combine then
c using nbound(j) to get overall occ
c (a) plotocc but not plotboth (occs but not current)
c	jb=1 if ligand 1 only or ligand 2 only plotted
c	jb=1,2 if both ligands to be plotted
c (b) plotocc and plotboth (occs and current)
c	jb=2 if ligand 1 only or ligand 2 only plotted
c	jb=2,3 if both ligands to be plotted
		   if(plotocc.or.plotboth) then
			if(plotocc) jb=1
			if(plotboth) jb=2
			n=1		!ligand number (always 1 unless nlig=2)
			if(nlig.eq.2.and.noccplot.eq.2) n=2	 !plot ligand 2 only
			Ycal(i,jb)=0.0
			do j=1,k
			   pt(j)=0.0d0
			   do m=1,k-1
				ct=bmj(m,j)*dexp(-t1/tau(m))	!component
				pt(j)=pt(j) + ct
			   enddo
			   pt(j)=pt(j)+pinf(j)
			   Ycal(i,jb)=Ycal(i,jb)+sngl(bound(j,n)*pt(j))
			enddo
			Ycal(i,jb)=Ycal(i,jb)/sngl(bmax(n))	!to get fractional occ of all sites
			xcal(i,jb)=xcal(i,1)
			if(noccplot.eq.3) then
			   n=2
			   jb=jb+1
			   Ycal(i,jb)=0.0
			   do j=1,k
				pt(j)=0.0d0
				do m=1,k-1
				   ct=bmj(m,j)*dexp(-t1/tau(m))	!component
				   pt(j)=pt(j) + ct
				enddo
			      pt(j)=pt(j)+pinf(j)
			      Ycal(i,jb)=Ycal(i,jb)+sngl(bound(j,n)*pt(j))
			   enddo
			   Ycal(i,jb)=Ycal(i,jb)/sngl(bmax(n))	!to get fractional occ of all sites
			   xcal(i,jb)=xcal(i,1)
			endif
		   endif
		enddo		!end of i=1,ncalc
		if(plotocc) then		!define param to show if occ only displayed
c if 2 ligands display param for ligand 1
		   do m=1,k-1
			bm(m)=0.0d0
			do j=1,k
		         bm(m)=bm(m) + bound(j,1)*bmj(m,j)
			enddo
		      bm(m)=bm(m)/bmax(1)
			j1=2*m-1		!=1,3,5..
			theta(j1)=sngl(tau(m))
			theta(j1+1)=sngl(bm(m))	!=theta 2,4,6..
		   enddo
		   binf=0.0d0		!equilib binding occ
		   do j=1,k
			binf=binf + bound(j,1)*pinf(j)
		   enddo
		   binf=binf/bmax(1)
		   theta(k1)=sngl(binf)
		   ifitype=4	!display calculated parameters
		endif
	   else if(pulse) then
c  first calc ncalc2 (=512 at present) points for ON jump
		dx=(t0ms-xmin)/float(ncalc2-1) !ncalc points from t=xmin=0 to t=t0
		do i=1,ncalc2
		   xcal(i,1)=xmin+float(i-1)*dx
		   t1=dble(xcal(i,1))
		   ctot=0.0d0
		   ctotz=0.0d0
		   ctotn=0.0d0
		   do j=2,k		!the k-1 components; j=index for Ycal
			m=j-1			!components 1 to k-1
			ct=cur0(m)*dexp(-t1/tau0(m))
			ctot=ctot+ct
			ctotz=ctotz + cur0z(m)*dexp(-t1/tau0(m))
			ctotn=ctotn + cur0n(m)*dexp(-t1/tau0(m))
			Ycal(i,j)=sngl(ct + curinf0) !offset component by I(inf) for display
	    		Xcal(i,j)=Xcal(i,1)
		   enddo
		   Ycal(i,1)=sngl(ctot + curinf0)		!total current
		   if(prepulse.and.iplot.eq.2) then
		      Ycal(i,2)=sngl(ctotz + curinf0)   !current -no ops in prepulse
		      Ycal(i,3)=sngl(ctotn + curinf0)   !current =>1 ops in prepulse
		   endif
c		   jc=1	!column # for total current
		enddo
c now define OFF relaxation
c Want 1st point=513 to be at t=t0, same as last point of ON =point #512
		dx=(xmax-t0ms-xmin)/float(ncalc2-1) !ncalc points from t=t0 to xmax
		do 112 i=ncalc2+1,2*ncalc2
		   xcal(i,1)=t0ms+float(i-ncalc2-1)*dx !times for plotting starting at t=t0
		   t1=dble(xcal(i,1)-t0ms)	!offset time to start at zero for calcs
		   ctot=0.0d0
		   ctotz=0.0d0
		   ctotn=0.0d0
		   do 113 j=2,k		!the k-1 components
			m=j-1			!components 1 to k-1
			ct=cur(m)*dexp(-t1/tau(m))
			ctot=ctot+ct
			ctotz=ctotz + curz(m)*dexp(-t1/tau(m))
			ctotn=ctotn + curn(m)*dexp(-t1/tau(m))
			Ycal(i,j)=sngl(ct + curinf) !offset component by I(inf) for display
c			Ycal(i,j)=sngl(ct) 		!no offset in pulse case
			Xcal(i,j)=Xcal(i,1)
113		   continue
		   Ycal(i,1)=sngl(ctot + curinf)		!total current
		   if(prepulse.and.iplot.eq.2) then
		      Ycal(i,2)=sngl(ctotz + curinf)   !current -no ops in prepulse
		      Ycal(i,3)=sngl(ctotn + curinf)   !current =>1 ops in prepulse
		   endif
c	         jc=1	!column # for total current
112		continue
	   endif
c	 next plot state occupancies (or transition freqs: iplot=5)
	else if(iplot.eq.3.or.iplot.eq.4.or.iplot.eq.5) then
c	  For state occupancies put required occs into Yval(i,j), j=1,nstat
c	 decide which to plot below
	   if(.not.pulse) then
	 	dx=(xmax-xmin)/float(ncalc)
		do 114 i=1,ncalc
		 xcal(i,1)=xmin+float(i-1)*dx
		 t1=dble(xcal(i,1))
		 j1=0
		 do 24 n=1,nstat	!number of states to be plotted
		  j=jstat(n)	!state number
		  if(iplot.eq.5) then
		   if(n.eq.1) then
			jto=jstat(2)
		   else if(n.eq.2) then
			jto=jstat(1)
		   endif
		  endif
		  pt(j)=0.0d0
		  do 25 m=1,k-1
		   ct=bmj(m,j)*dexp(-t1/tau(m))	!component
		   pt(j)=pt(j) + ct
		   if(iplot.eq.4) then
			Ycal(i,m+1)=sngl(ct+Pinf(j))	!offset component to Pinf
			Xcal(i,m+1)=Xcal(i,1)		!j=2,3,...,k (total in j=j1=1)
		   endif
25		   continue
		   j1=j1+1
		   Ycal(i,j1)=sngl(pt(j)+Pinf(j))
		   xcal(i,j1)=xcal(i,1)
		   if(iplot.eq.5) then
			Ycal(i,j1)=Ycal(i,j1)*sngl(Q1(j,jto))
		   endif
24		 continue	!n=1,nstat
		 if(iplot.eq.3.and.plotocc) then	!add binding curve(s)
		    jb=nstat+1
		    n=1		!ligand number (always 1 unless nlig=2)
		    if(nlig.eq.2.and.noccplot.eq.2) n=2	 !plot ligand 2 only
		    Ycal(i,jb)=0.0
		    do j=1,k
			 pt(j)=0.0d0
			 do m=1,k-1
			    ct=bmj(m,j)*dexp(-t1/tau(m))	!component
			    pt(j)=pt(j) + ct
			 enddo
			 pt(j)=pt(j)+pinf(j)
			 Ycal(i,jb)=Ycal(i,jb)+sngl(bound(j,n)*pt(j))
		    enddo
		    Ycal(i,jb)=Ycal(i,jb)/sngl(bmax(n))	!to get fractional occ of all sites
		    xcal(i,jb)=xcal(i,1)
c
		    if(noccplot.eq.3) then
			 n=2
			 jb=jb+1
			 Ycal(i,jb)=0.0
			 do j=1,k
			    pt(j)=0.0d0
			    do m=1,k-1
				ct=bmj(m,j)*dexp(-t1/tau(m))	!component
				pt(j)=pt(j) + ct
			    enddo
			    pt(j)=pt(j)+pinf(j)
			    Ycal(i,jb)=Ycal(i,jb)+sngl(bound(j,n)*pt(j))
			 enddo
			 Ycal(i,jb)=Ycal(i,jb)/sngl(bmax(n))	!to get fractional occ of all sites
			 xcal(i,jb)=xcal(i,1)
		    endif
		 endif
114		continue	!i=1,ncalc
		if(iplot.eq.3.and.plotocc) then	!add binding curve
		   if(noccplot.le.2) then
	           print 662,noccplot
	           if(discprt) write(8,662) noccplot
662		     format(/,' Last curve = binding of ligand ',i2)
		     pause
		   else if(noccplot.eq.3) then
	           print 663
	           if(discprt) write(8,663)
663		     format(/,' Last 2 curves = binding of ligands 1 and 2')
		     pause
		   endif
		endif
	   else if(pulse) then
c  first calc ncalc2 (=512 at present) points for ON jump
		dx=(t0ms-xmin)/float(ncalc2-1) !ncalc points from t=xmin=0 to t=t0
		do 115 i=1,ncalc2
		 xcal(i,1)=xmin+float(i-1)*dx
		 t1=dble(xcal(i,1))
		 j1=0
		 do 26 n=1,nstat	!number of states to be plotted
		  j=jstat(n)	!state number
		  pt(j)=0.0d0
		  do 27 m=1,k-1
		   ct=bmj0(m,j)*dexp(-t1/tau0(m))	!component
		   pt(j)=pt(j) + ct
		   if(iplot.eq.4) then
			Ycal(i,m+1)=sngl(ct+Pinf0(j))	!offset component to Pinf
			Xcal(i,m+1)=Xcal(i,1)		!j=2,3,...,k (total in j=j1=1)
		   endif
27		  continue
c====print pt(j)?
		  j1=j1+1
		  Ycal(i,j1)=sngl(pt(j)+Pinf0(j))
		  xcal(i,j1)=xcal(i,1)
26		 continue
115		continue
c now define OFF relaxation
c Want 1st point=513 to be at t=t0, same as last point of ON =point #512
		dx=(xmax-t0ms-xmin)/float(ncalc2-1) !ncalc points from t=t0 to xmax
		do 116 i=ncalc2+1,2*ncalc2
		 xcal(i,1)=t0ms+float(i-ncalc2-1)*dx !times for plotting starting at t=t0
		 t1=dble(xcal(i,1)-t0ms)	!offset time to start at zero for calcs
		 j1=0
		 do 28 n=1,nstat	!number of states to be plotted
		 j=jstat(n)	!state number
		  pt(j)=0.0d0
		  do 29 m=1,k-1
		   ct=bmj(m,j)*dexp(-t1/tau(m))	!component
		   pt(j)=pt(j) + ct
		   if(iplot.eq.4) then
			Ycal(i,m+1)=sngl(ct+Pinf(j))	!offset component to Pinf
			Xcal(i,m+1)=Xcal(i,1)		!j=2,3,...,k (total in j=j1=1)
		   endif
29		  continue
c====print pt(j)?
		  j1=j1+1
		  Ycal(i,j1)=sngl(pt(j)+Pinf(j))
		  xcal(i,j1)=xcal(i,1)
28		 continue
116		continue
	   endif
	endif
c Data
	ncurvd=0
c calc curves:
	if(debug()) then
1125	  print 1121
1121	  format(
     & ' Print Xcal(i,1), Ycal(i,1) for i = i1,i2: i1,i2 [skip] = ')
c	   read 4,i1,i2
c4	   format(2i8)
	   call INPUT2i(i1,i2)
	   if(i1.eq.0) goto 1122
	   if(i2.lt.i1) i2=i1
	   do 1123 i=i1,i2
		print 1124,i,xcal(i,1),ycal(i,1)
1124		format(i8,2g13.6)
1123	   continue
	   goto 1125
1122	   continue
	endif
	icurvc(1)=1		!total
	iline(1)=0		!total curve continuous
	ncal(1)=ncalc
	do 12 j=2,k
	 ncal(j)=ncal(1)
	 icurvc(j)=j
	 iline(j)=2		!short dash for components
	 if(iplot.eq.3) then
	   iline(j)=j-1	!diff lines for diff state occs
	   if(plotocc) then
		icurvc(nstat+1)=nstat+1
	      iline(nstat+1)=0	!contin for binding
		 ncal(nstat+1)=ncal(1)
	   endif
	 endif
12	continue
	if(iplot.eq.1.or.iplot.eq.2) then	!plot current
	   if(iplot.eq.1) ncurvc=k		!components + total calc curve
	   if(iplot.eq.2) then
		if(.not.prepulse) then
		   ncurvc=1		!total only
		   if(plotboth) then		!current too
			if(noccplot.le.2) then
			   ncurvc=2		!occupancy too
			else if(noccplot.le.3) then
			   ncurvc=3		!occupancy too
			endif
		   else				!binding only
			if(noccplot.le.2) then
			   ncurvc=1		!one occupancy too
			else if(noccplot.le.3) then
			   ncurvc=2		!both occupancy too
			endif
		   endif
		else
		   ncurvc=3		!total (overall and +/- ops during prepulse)
c index in icol()=11-20 for calc curves (#1-10);
		   icol(11)=14	!yellow for overall
		   icol(12)=9	!blue for no ops during prepulse
		   icol(13)=12	!red for 1 or more ops during prepulse
		endif
	   endif
	   titley='current (pA)'
	   if(plotocc) then
		titley='Binding'
		if(nlig.eq.2) then
		   if(noccplot.eq.1) then
			titley='Binding (ligand 1)'
		   else if(noccplot.eq.2) then
			titley='Binding (ligand 2)'
		   else if(noccplot.eq.3) then
			titley='Binding (both ligands)'
		   endif
		endif
	   else if(plotboth) then
		titley='current (pA) and binding'
		if(nlig.eq.2) then
		   if(noccplot.eq.1) then
			titley='current (pA) + binding (1)'
		   else if(noccplot.eq.2) then
			titley='current (pA) + binding (2)'
		   else if(noccplot.eq.3) then
			titley='current (pA) + binding (both)'
		   endif
		endif
	   else 	!current only
c          rescale here for very small currents
		ym=0.0
		do i=1,ncal(1)
		   y=abs(ycal(i,1))
		   if(y.gt.ym) ym=y
		enddo
		if(ym.le.0.1) then
		   do i=1,ncal(1)
			ycal(i,1)=1000.*ycal(i,1)
		   enddo
		   titley='current (fA)'
		endif
	   endif
	else if(iplot.eq.3.or.iplot.eq.4) then
	   if(iplot.eq.3) then
		ncurvc=nstat	!# of states to plot occ of
		if(plotocc) ncurvc=nstat+1
	   endif
	   if(iplot.eq.4) ncurvc=k	!1 state + k-1 components of it
	   titley='occupancy'
	else if(iplot.eq.5) then
	   titley='transition frequency (1/s)'
	   ncurvc=2
	   icurvc(1)=1
	   icurvc(2)=2
	   iline(1)=0	!contin
	   iline(2)=2	!dashed
	   icol(11)=12
	   icol(12)=10
	   i=jstat(1)
	   j=jstat(2)
	   print 65,i,j,j,i
	   if(discprt) write(8,65) i,j,j,i
65	   format(
     & ' Plot transition rate from state ',i3,' to ',i3,' (continuous)'
     &  ,/,' and from state ',i3,' to ',i3,' (dashed)')
	   pause
	endif
	if(plotsav) then
	   call DCASK(
     &    'Superimpose last current vs time plot (light blue)','n',ans)
	   if(ans.eq.'Y') then
		ncurvc=ncurvc+1
		j=ncurvc
		ncal(j)=ncsav
		do i=1,ncsav
		   Xcal(i,j)=Xcalsav(i)
	 	   Ycal(i,j)=Ycalsav(i)
		enddo
		icol(j+10)=11	!light blue
		icurvc(j)=j
		iline(j)=1		!dotted for last curve
	   endif
	endif
	fitted=.true.	!or params not displayed
c	fitted=.false.
	titlex='time (ms)'
c Misc inputs
	if(.not.drcurve) ilog=0
	iscal=1		!scale internally
c  ISCAL=3 if input values of ymin,ymax,ytic only to be used; others internally set
	if(iplot.eq.3.or.iplot.eq.4) then	!occupancies
	   ymin=-0.1
	   ymax=1.0
	   ytic=0.1
	   iscal=3
	endif
	inumx=-1		!X axis in fixed (Fn.d) format
	inumy=-1
c	inumy=1		!X axis in exponent format (if log)
c	ILINE(j) defines line type for the jth calculated curve, via ZSEQ
c         (prev numbered 1-5, but now make 0-4 so 0=continuous as in Hgraph)
c		0=continuous line
c		1=long dash
c		2=short dash
c		3=long,short dash
c		4=long,short,short dash
	goto 100		!display
c
51	continue
c Display noise, initially as spectral density
c Define range for calc curve initially from f=0 up to from f=0.2*smallest
c fc (=fc(k-1) to f=5*largest fc, ie fc(1).
c Or display as autocovariance, up to 5*largest tau
	ncalc=512
	if(pspec) then
	  xmin=0.2*fc(k-1)
	  xmax=5.0*fc(1)
	  xmin1=alog10(xmin)
	  dx=alog10(xmax/xmin)/float(ncalc)	!for equal spacing in log scale
	  do 14 i=1,ncalc
	   x1=xmin1+float(i-1)*dx
	   xcal(i,1)=10.0**x1		!must be on non-log scale
	   f=xcal(i,1)
	   ct=0.0
	   do 13 j=2,k		!the k-1 components
	   m=j-1			!components 1 to k-1
	   f1=f/fc(m)
	   c=1.e-24*G0(m)/(1.0 + f1*f1)	!convert to A**2
	   ct=ct+c
	   Ycal(i,j)=c
	   Xcal(i,j)=f
13	   continue
	   Ycal(i,1)=ct	!total G(f)
14	   continue
c	   sub=char(4)
c	   sup=char(21)
	   titlex='Frequency (Hz) '
	   titley='Spectral density (A'//char(21)//'2'//char(4)//'s)'
	   ilog=3		!log/log
	   iscal=1		!scale internally
	   inumx=-1		!X axis in fixed (Fn.d) format
	   inumy=1		!X axis in exponent format (if log)
	else if(.not.pspec) then      !autocovariance
	   xmin=0.
	   xmax=5.0*tau(k-1)
	   dx=(xmax-xmin)/float(ncalc)
	   do 15 i=1,ncalc
	    xcal(i,1)=xmin+float(i-1)*dx
	    t=xcal(i,1)
	    ct=0.0
	    do 16 j=2,k		!the k-1 components
	     m=j-1			!components 1 to k-1
	     c=cv0(m)*exp(-t/tau(m))
	     ct=ct+c
	     Ycal(i,j)=c
	     Xcal(i,j)=t
16	    continue
	    Ycal(i,1)=ct		!total
15	   continue
	   titlex='time (ms)'
	   titley='autocovariance (pA'//char(21)//'2'//char(4)//')'
	   ilog=0
	   iscal=1		!scale internally
	   inumx=-1		!X axis in fixed (Fn.d) format
	   inumy=-1
	endif
c Data
	ncurvd=0
c calc curves:
	ncurvc=k		!components + total calc curve
	icurvc(1)=1		!total
	iline(1)=0		!total curve continuous
	ncal(1)=ncalc
	do 17 j=2,k
	 ncal(j)=ncal(1)
	 icurvc(j)=j
	 iline(j)=2		!short dash for components
17	continue
c
100	continue
c Find Y(max/min) and time at which it occurs -just search Ycal() for now
c -better to interpolate for greater accuracy
c NB the amplitudes (in wsc(i)) have not been multiplied by cfac1 to convert
c to pA, but curinf has, so need
	popinf=curinf/cfac1
	if(iplot.eq.1.or.iplot.eq.2) then
	 do i=1,ncomp1
	    wsc(i)=ws(i)
	 enddo
	 thigh=-1.d0	!so set internally
c	 call EXPMAXd(tpeak,ypeak,curinf,thigh,texp,
	 call EXPMAXd(tpeak,ypeak,popinf,thigh,texp,
     &	   pon(),discprt,debug(),km,nerr,nerr1,nerr2,0)
	 ypeak=ypeak*cfac1	!in pA
	 if(nerr.eq.-5) then
	    print 2
          if(pon()) write(7,2)
	    if(discprt) write(8,2)
2	    format(' No peak response found (monotonic)')
	 else if(nerr.ne.0.or.nerr1.ne.0.or.nerr2.ne.0) then
	      print 3,nerr,nerr1,nerr2
      	if(pon()) write(7,3) nerr,nerr1,nerr2
	      if(discprt) write(8,3) nerr,nerr1,nerr2
3		format(' Error in EXPMAXd: nerr,nerr1,nerr2 = ',3i3,
     &	    ' ; peak not found')
	 else
          print 35, ypeak,tpeak
          if(pon()) write(7,35) ypeak,tpeak
          if(discprt) write(8,35) ypeak,tpeak
35	    format(' Peak current = ',g12.5,' pA at t = ',f8.2,' ms')
	 endif
	 if(iplot.eq.2.and.prepulse) then
      	print 35, ypeak,tpeak
      	if(pon()) write(7,35) ypeak,tpeak
      	if(discprt) write(8,35) ypeak,tpeak
		do i=1,ncomp1
		   wsc(i)=wsz(i)
		enddo
		thigh=-1.d0	!so set internally
	      call EXPMAXd(tpeak,ypeak,popinf,thigh,texp,pon(),
     &		discprt,debug(),km,nerr,nerr1,nerr2,0)
		ypeak=ypeak*cfac1	!in pA
		if(nerr.eq.-5) then
	         print 2
      	   if(pon()) write(7,2)
	         if(discprt) write(8,2)
		 else if(nerr.ne.0.or.nerr1.ne.0.or.nerr2.ne.0) then
		   print 3,nerr,nerr1,nerr2
	         if(discprt) write(8,3) nerr,nerr1,nerr2
		else
      	   print 37, ypeak,tpeak
      	   if(pon()) write(7,37) ypeak,tpeak
      	   if(discprt) write(8,37) ypeak,tpeak
37	         format(
     &  	' Peak current (0 openings in prepulse) = ',g12.5,
     &	' pA at t = ',f8.2,' ms')
		endif
c
		do i=1,ncomp1
		   wsc(i)=wsn(i)
		enddo
		thigh=-1.d0	!so set internally
	      call EXPMAXd(tpeak,ypeak,popinf,thigh,texp,pon(),
     &	   discprt,debug(),km,nerr,nerr1,nerr2,0)
		ypeak=ypeak*cfac1	!in pA
		if(nerr.eq.-5) then
	         print 2
      	   if(pon()) write(7,2)
	         if(discprt) write(8,2)
		else if(nerr.ne.0.or.nerr1.ne.0.or.nerr2.ne.0) then
	         print 3,nerr,nerr1,nerr2
      	   if(pon()) write(7,3) nerr,nerr1,nerr2
	         if(discprt) write(8,3) nerr,nerr1,nerr2
		else if(nerr.ne.0) then
	         print 3,nerr
      	   if(pon()) write(7,3) nerr
	         if(discprt) write(8,3) nerr
		else
      	   print 38, ypeak,tpeak
      	   if(pon()) write(7,38) ypeak,tpeak
      	   if(discprt) write(8,38) ypeak,tpeak
38	         format(
     &        ' Peak current (>0 openings in prepulse) = ',g12.5,
     &        ' pA at t = ',f8.2,' ms')
		endif
	 endif
	 pause 'Hit any key for graph'
	endif		!end of iplot=1,2
c
325	continue
	iask=2	!leave on screen for fitting
c==	if(varconc.and.(.not.fitted)) iask=2	!leave on screen for fitting
c==	if(varconc.and.fitted) iask=-2	!normal
	itrace=0
	kmax=100	!dimension of theta
	kwi=1		!dimension of weight()
	kwj=1		!dimension of weight()
	y1c=ymin+0.96*(ymax-ymin)         !initial positions for jump logos
	y2c=ymin+1.00*(ymax-ymin)
	y1v=ymin+0.94*(ymax-ymin)
	y2v=ymin+0.90*(ymax-ymin)
c NB can supply only one y(inf) value for Hill plot, and this can't be
c correct for both curves if not monotonic
	call VPLOT5(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,syms,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO,XHI,YLO,YHI,y00,yinfeq,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,plotonly,itit,title1,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,interp,
     & isetcol,itrace,ndv1,ndc1,wght,kwi,kwj,icurvw,kmax,iver)
c
	fitted=.false.
	fstdisp=.false.
c
c In case of time-varying conc, ask if results to be fitted
c	if(.not.varconc) goto 52
c	if(varconc.and.ncurvc.eq.1) then
	if(varconc.and.(.not.showconc)) then
c	   call DCASK(
c     &    'Plot concentration profile in top 12% of display','n',ans)
	   ans='Y'
 	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     &    'Plot concentration profile in top 12% of display',
     &     defolt,ict,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'Y') then
		ncurvc=ncurvc+1
c==		icurvc(ncurvc)=15		!conc profile in ycal(i,15)
		icurvc(ncurvc)=k+2	!conc profile in ycal(i,k+2) -see vconc1
		ncal(k+2)=ntime
		iline(k+2)=0		!total curve continuous
		iline(k+2)=1		!dotted
		icol(10+k+2)=0		!black

c		scale conc profile to fit in top 10%
		cmin=1.e37
		cmax=-1.e37
		do i=1,ntime
		   if(ycal(i,k+2).lt.cmin) cmin=ycal(i,k+2)
		   if(ycal(i,k+2).gt.cmax) cmax=ycal(i,k+2)
		enddo
		s=0.1*(ymax-ymin)/(cmax-cmin)		!scale factor
		b=ymin+0.88*(ymax-ymin)			!new baseline
		do i=1,ntime
		   c=ycal(i,k+2)-cmin	!baseline=0
		   c=c*s			!right range
		   ycal(i,k+2)=c + b
		enddo
		showconc=.true.
		iscal=0		!use same xmin etc
		goto 325		!redisplay with conc
	   endif
	endif
	if((ncurvc.eq.1.or.(ncurvc.eq.2.and.showconc)).and.
     &	(.not.drcurve)) then
	   if(pulse.and.npulse.gt.1) then
		ans='N'
	   else
		ict=14
		call CLRDIALOG(1,0)	!restart at top of box
		ans='Y'
	 	call DEFOLTa(ans,defolt)
		call QDIALOG(1,'Fit this curve with exponentials'
     &	,defolt,ict,cans)
		call GETINPa(cans,ans)
c===temp
		call BELL(2)
		call WDIALOG(1,'CURSORS NOT YET FIXED',12)
		ans='N'
	   endif
c===end of temp
	   if(ans.eq.'Y') then
c===		call PUTCUR(5000,7000)		!start with vertical cursor central
		call CLRDIALOG(1,icb)	!restart at top of box
		call WDIALOG(1,'Mark start of region to be fitted',ict)
c===      	ch=char(FNDCUR(ix1,iy1))
		call WDIALOG(1,'Mark end of region to be fitted',ict)
c===      	ch=char(FNDCUR(ix2,iy2))
c===		tstart=XWORLD(ix1)
c===		tend=XWORLD(ix2)
		ncomp=1
 		call DEFOLTi(ncomp,defolt)
		call QDIALOG(1,'Number of components to be fitted',
     &    		defolt,ict,cans)
		call GETINPi(cans,ncomp)
c
c Put data into xfit,yfit
		j=icurvc(1)				!this is curve on display
c Find first point following tstart and last preceding tend.  For pulse
c case two sep relaxations calc and points may not be same distance apart
c for on and off jumps so find values by search
c NO -time points are not nec separated by dt? -use search for varconc too
c=		if(varconc) then
c Find first point following tstart
c=		   istart=2+ifix(tstart/dt)	!dt defined in VCONC
c and point preceeding tend
c=		   iend=1+ifix(tend/dt)
c=		else
		   do i=1,ncal(j)
			if(xcal(i,j).gt.tstart) then
			    istart=i
			    goto 33		!start point found
			endif
		   enddo
33		   continue
		   do i=ncal(j),1,-1
			if(xcal(i,j).lt.tend) then
			    iend=i
			    goto 34		!end point found
			endif
		   enddo
34		   continue
c=		endif
		if(istart.lt.1) istart=1
		if(iend.gt.ncal(j)) iend=ncal(j)
		tstart=xcal(istart,j)	!revised value
		tend=xcal(iend,j)
c
		n=0
		do i=istart,iend
		   t=xcal(i,j)-tstart		!starts at t=0
		   n=n+1
		   xfit(n)=t
		   yfit(n)=ycal(i,j)
c		define weights proportional to slope of curve
		   if(n.gt.1) then
c (NB for variable conc, times not nec equally spaced and dx not defined )
c		      w=(yfit(n)-yfit(n-1))/dx
		      w=(yfit(n)-yfit(n-1))/(xfit(n)-xfit(n-1))
			weight(n-1)=w*w
		   endif
	      enddo
		weight(n)=weight(n-1)		!last one
		nfit=n	!in common
c
c Fix ampitude at t=0.0?  Y(0)=Y(inf) + SUM[amp(i)]
	  	ans='N'
		fixamp=.false.
		call DEFOLTa(ans,defolt)
		call QDIALOG(1,'Constrain amplitude at t = 0 to Y(inf)',
     &	  defolt,ict,cans)
		call GETINPa(cans,ans)
		fixamp=ans.eq.'Y'
c
c Get guesses
		call WDIALOG(1,'Initial guesses:',ict)
		do i=1,ncomp
		   i1=2*i-1		!=1,3,5..
		   call INTCONV(i1,cnum1)
		   call INTCONV(i,cnum2)
		   call QDIALOG(1,'(Param '//CHARNB(cnum1)//
     &	     '): Tau('//CHARNB(cnum2)//') (ms)',' ',ict,cans)
		   call GETINPr(cans,theta(i1))
		   if(.not.fixamp.or.(fixamp.and.i.lt.ncomp)) then
			call INTCONV(i1+1,cnum1)
			call INTCONV(i,cnum2)
			call QDIALOG(1,'(Param '//CHARNB(cnum1)//
     &     '): Amplitude (init-final) of component '//CHARNB(cnum2),
     &     ' ',ict,cans)
	   		call GETINPr(cans,theta(i1+1))
		   endif
		enddo
		k1=2*ncomp+1
		if(fixamp) k1=k1-1
		call INTCONV(k1,cnum1)
		call QDIALOG(1,'(Param '//CHARNB(cnum1)//
     &	     '): Final asymptote',' ',ict,cans)
		call GETINPr(cans,theta(k1))
c
		nfix=0
		do j=1,20
		  jfix(j)=0	!INITIALISE
		enddo
	  	ans='N'
		call DEFOLTa(ans,defolt)
		call QDIALOG(1,'Fix any parameters',
     &	  defolt,ict,cans)
		call GETINPa(cans,ans)
		if(ans.eq.'Y') then
		   call QDIALOG(1,' Number to be fixed [0]',
     &	   ' ',ict,cans)
		   call GETINPi(cans,nfix)
		   do i=1,nfix
			call INTCONV(i,cnum1)
			call QDIALOG(1,' '//CHARNB(cnum1)//
     &	 	 ': Fix parameter #',' ',ict,cans)
			call GETINPi(cans,j)
			if(pon()) write(7,431) j,theta(j)
			if(discprt) write(8,431) j,theta(j)
431			format(' Param no ',i3, ' fixed at ',g13.6)
			jfix(j)=1
		   enddo
		endif
		call VIDEOMOD(3)		!utility lib- this makes graph go for good!
		call LOCATE(0,0)
		stpfac=0.1	!factor for step size (set in SIMP5V)
		errfac=1.0e-4		!DEFAULT
		fitted=.true.
		ndisp=20
		nevmax=-30000	!neg so iterations not printed to disc
		confac=0.5		!parameter for simplex3
		irestrt=4
c
		call SIMPLEX3(k1,theta,stpfac,errfac,nev,nevmax,
     & 	 smin,SSDSCALC,ndisp,jfix,-1.,confac,irestrt,iconv)
c
c If fixamp=true, move parameter values in theta to where they would have
c been if fixamp had not been true -so do not need to change stuff that
c calculates the fitted curves
		if(fixamp) then
		   k1=2*ncomp+1
		   theta(k1)=theta(k1-1)      !Y(inf)
		   s=0.0
		   do j=1,ncomp-1	    		!add amps for all but last term
			s=s + theta(2*j)
		   enddo
		   theta(2*ncomp)=-s		!so total amp=0
		endif
c
c Put calc curve in ycal(i,11)
c***HAVE PROBLEM WITH CURRENT VPLOT5 WHEN THERE ARE MORE THAN 10
C***ELEMENTS IN YCAL ETC -CURVE 1 MUST BE SAME COLOUR AS CURVE 11 etc!!!
c  -so use ycal(i,14) for calc curve
c
		ncalc=500
		ncurvc=ncurvc+1
		ncal(14)=ncalc
		icurvc(ncurvc)=14
		icol(14)=11
		iline(14)=2		!fitted curve dashed
c
		dxcalc=(tend-tstart)/float(ncalc-1)
		yinf=theta(k1)
		do i=1,ncalc
		   t=float(i-1)*dxcalc	!starts at 0 for calc values
		   yc=0.0
		   do j=1,ncomp
		      j1=2*j-1		!=1,3,5..
			tau1=theta(j1)
			amp=theta(j1+1)	!=2,4,6..
			yc=yc + amp*exp(-t/tau1)
		   end do
		   yc=yc+yinf
		   xcal(i,14)=t+tstart	!plot values starting at t=tstart
		   ycal(i,14)=yc
		enddo
c Print values
		i=0
		totamp=0.0
		do j=1,ncomp
		   i=i+1
		   j1=2*j-1		!=1,3,5..
		   tau1=theta(j1)
		   amp=theta(j1+1)
		   totamp=totamp+amp
	         print 20,i,tau1,amp
      	   if(pon()) write(7,20) i,tau1,amp
      	   if(discprt) write(8,20) i,tau1,amp
20		    format(' Component ',i2,': tau = ',g13.6,
     &		' ms;  amp = ',g13.6)
		end do
		y0=yinf-totamp
		if(fixamp) then
	         print 22
      	   if(pon()) write(7,22)
	         if(discprt) write(8,22)
22		   format(' Amplitude constrained to zero at t=0')
		endif
	      print 21, tstart,tend,y0,yinf
	      if(pon()) write(7,21) tstart,tend,y0,yinf
	      if(discprt) write(8,21) tstart,tend,y0,yinf
21		format(
     &	' Fitted from t1 = ',g13.6,' to t2 = ',g13.6,' ms',/,
     &	'  ( weight proportional to square of slope)',/,
     &	' amplitude (at t=t1) = ',g13.6,/,
     &	' asymptote y(inf) = ',g13.6,/,/,
     &	' Type any key for display')
		call ANYKEY
c
		iscal=0
		ifitype=4	!show fitted params
		goto 100	!display fit
	   endif
	endif
	call VIDEOMOD(3)		!if not to be fitted
	call LOCATE(0,0)
	if(drcurve) then
	   idisp=idisp+1
	   if(idisp.gt.5) idisp=5
	   goto 152
	else if(npulse.gt.1) then
	   idisp=idisp+1
	   if(idisp.gt.6) idisp=6
	   goto 102
	else
	   goto 52
	endif
999	continue
	DEALLOCATE(Xval,Yval)
	RETURN
	end

