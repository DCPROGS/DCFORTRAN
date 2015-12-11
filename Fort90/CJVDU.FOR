    	subroutine CJVDU(Xval,Yval,xval0,xvaln,vramp,sampv,
     & nsamp,itsamp,nAv,dnAv,nvjump,ilenv,igapv,ivolt1,ivolt2,
     & irate,nAc,dnAc,ncjump,ilenc,igapc,nj1,nj2,ncon,
     & t1c1,t2c1,t1v1,t2v1,vstep,samescal,rescale,ypeak,ybase,tau1,
     & ISYM,ILOG,ISCAL,IJOIN,iline,XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,
     & xlo,xhi,ylo,yhi,y0,yinf,xcross,ycross,titlex,titley,title,
     & itit,ilabel,inumx,inumy,ncurvd,ndat,icurvd,ncurvc,ncal,icurvc,
     & symsiz,idiskq,ntx,nty,itx,ity,fitted,doframe,dcurve,refit,
     & ifit,super,nsuper,isuper,xdelt,ndelt,ilrn,lrnfit,jfit,
     & ikeep1,nkeep,kstep,keepall,nsamp0,jmask,idest,Erev,
     & ndv1,ndimd,xfit,yfit,nmax,ivhclamp,ipatch,vmin,vmax,
     & average,nmean,xmean,ncrvdsav,icrvdsav,srate,usesrate,
     & xmin0,xmax0,ymin0,ymax0,logx,logy,
     & xminsav,xmaxsav,yminsav,ymaxsav,ndfit,
     & avact,tpre,avsamp,dfinter,plotqdat,align,endalign)
c
c For Hgraph display of conc Jumps in CJFIT (data from CJUMP)
c (modified from CVDISP, via old CJVDU1)
c===TO DO
c=== check def of xval0,
c=== when calc curve is plotted is t=0 shifted?
c
c===add more help (eg BEFORE choice of cursor/number for H line)
c
c  Modif 09/18/97 10:17am by adding ndfit=allocated dimension of
c xfit, yfit.  Set ndfit=1 for huge files so cannot fit traces here
c but only queue them for fit later (ndfit=1 only for iread=4 when
c nsamp is huge).
c
c Modif 10/27/96 06:34am for averaged steady-state activations (avact etc)
c
c Modif 10/19/96 04:43pm for plotting steady-state clusters after
c aligning and averaging in aver_act.for (iplot=11)
c
c Modif 10/23/95 02:27pm for residual plot, plot of asymptote as
c	calc line.  Get problems after log plots because xmin etc returned
c	from VPLOT as log values.  So after call to VPLOT, define xmin0 etc
c	as non-log values (added to parameters).
c Modif 03/13/95 04:26pm so can optionally plot only the long limb
c of ramp response (so hysteresis disguised!)
c
c Modif 12/05/94 12:18pm for Lahey V5 and VPLOT4.  Supposes that Xval, Yval
c allocated before calling (ndv1,ndimd). Xcal, Ycal allocated locally in
c this subroutine.
c PLOT TYPES
c	iplot=1 Current vs time
c	iplot=11 Current vs time for average of aligned activations from consam
c	iplot=2 Voltage vs time
c	iplot=3 RAMP: Separate agonist & control I/V (fancy calc)
c	iplot=4 RAMP: Net I/V (fancy calc)
c	iplot=5 RAMP: Separate agonist & control I/V (simple calc)
c	iplot=6 RAMP: Net I/V (simple calc)
c	iplot=7 Net JUMP
c	iplot=8 Residuals plot
c
c  Colours can now be set via icol() -with isetcol=1, any icol() elements
c that are not set to -1 will overide defaults in VPLOT -icol(i) is:
c 1-10= data sets (#1-10) (and colour for symbols etc);
c 11-20=calc curves (#1-10);
c 21=axes (or calibration bars); 22=frame; 23=axis labels (same for X,Y)
c 24=axis numbers (same for X,Y); 25=plot title; 26=param value text;
c 27=symbols (line thickness only; colour as for data sets);
c 28=jump bar (logo) for C-jumps
c 29=jump bar (logo) for V-jumps
c 30=SD bars
c 31-50=extra text (#1-20); 51-60=arrows (#1-10); 61-70=lines (#1-10)
c icol(71)=background colour for graph
c icol(72-81)= horizontal lines; icol(82-91) vertical lines
c 92-100 =spare for future
c e.g. for data set i=1,ncurvd, colour=icol(icurvd(i))
c
c 08/26/92 05:38pm Modified to use VPLOT2.  Problem arises because latter
c requires common/jlogo/ so t1c etc can no longer be parameters of CJVDU,
c but in main prog t1c() etc are already in common/cjpar/ (for READAT, RELWRT,
c and REAVER).  Therefore in main prog these arrays are called t1c1() etc
c in common/cjpar (and these are kept as param of CJVDU). On input, copy values
c of t1c1() etc to t1c() etc, and reverse on output from CJVDU
c 07/05/91 02:46pm: Modified to take nsamp>2048
c   (but not for cjump3 data).  Also has Yval1 as parameter (equivalenced with
c	Yval in main prog). VPLOTR is OK for this IF icurvd(i) contains
c	the index j, such that Yval(1,j) contains first point to be plotted
c	(even if > 2048 points so data goes on to j+1,...) (note that
c	a number defined as Yval(2049,2) is interpreted as Yval(1,3), and
c	Yval(2050,2) as Yval(2,3) etc; no error results as long as do not
c	go past Yval(2048,10) (see tests in V===================
c   (a) If one data set only read (#nj1, nj2=0) then jc=1,jv=1+ngr so need
c	2*ngr arrays altogether (OK up to ngr=10, nsamp=20480 if voltage
c	not recorded, or ngr=5, nsamp=10240 if voltage recorded)
c   (b) If two data sets read (#nj1, #nj2) then for #nj1 jc=1, and
c	for #nj2 jc=1+ngr: 2*ngr arrays altogether (OK up to ngr=5, nsamp=10240
c	if voltage not recorded. If voltage recorded then for #nj1 jv=2*ngr+1
c	and for #nj2 jv=3*ngr+1; need 4*ngr arrays altogether (OK up to ngr=2,
c	 nsamp=4096)
c	Previously for c-jumps or v-jumps 'control' , 'drug' and
c	net=drug-control were kept as part of data only in Yval(i,j),Xval(i,j),
c	where j=5,6,7 respectively.  But ALL of data was put into these (though
c	initial xmin,xmax etc chosen to show interesting part) so no need to
c	duplicate the original data in j=5,6. This removed now so can show
c	drug and con (up to 10240 points each). But if drug and control are
c	more than 3x2048=6144 points each then have no room in Yval,Xval for
c	NET data too, when ndimd=10.
c	SOLUTION is to make ndimd=15 and replace j=5,6,7 with j=jc5,jc6,jc7
c	(which will be 5,6,7 if all nsamp=<2048). For jumps use j=jc1,jc2=
c	all orig data for drug,con with j=jc7 for net so Yval(2048,15) can
c	take all 3x10240 arrays. For ramps with crude calculations j=jc1,jc2
c	have orig current data, and j=jv1,jv2 have orig voltage data (for Xval)
c	and j=jc5,jc6 for those current points that are from the ramp part of
c	the drug, control sweep only, j=jc7 for net ditto. For crude ramp
c	can thus have up to nsamp=4096 (uses j=1 to 14 in Yval(2048,15).
c	Fancy ramp calculations are limited to nsamp=2048 as before.
c		Option to show initially selected points only for jumps now
c	removed (I/V from ramps does of course need to use only the points
c	on the ramp); if this to be added again later need to ask which v-jump OR
c	which c-jump to be selected (prev version looked only at v-jumps!) but
c	better do this by adding separate question -'display selected jump
c	only?' rather then specifying at 'print 1'
c		Also may need to cope with case where sweep contains more than
c	one V-ramp -not in at present.
c
c
c Modif 07/11/91 11:10am by adding jfit as param so when jfit=4 graph
c	comes up with same scales as last time (even if nsamp etc not same)
c	and also comes up interpolated if last one was interp.
c
c Modif 06/07/91 11:03am for CJUMP3 data
c=====Plot of part of sweep only, eg single v-jump when several done, is
c=====not yet fixed (as for LPLOTCJ3) -see code around label 266
c ILENV etc now in integer*4 microseconds
c
c Options for display:
c (1) If c-jump only (1 record read in; nj2=0) then show this record (in AVCUR
c	whether averaged or not) and decide on region to be fitted
c (2) If V-jump then 2 data sets read in (nj2 > 0) so display both, and
c	then difference. If nvjump>1 then must first specify which of the
c	jumps/ramps is to be displayed. For ramps display as I/V curve
c	Also option to display Voltage vs time when sampv=true.
c
c Data in Yval(i,j) already read in, and scaled, in READREL in main prog.
c Must define Xval(i,j) here (time or voltage with appropriate j)
c (a) If one data set only read (#nj1, nj2=0) then
c	Yval(i,1)=AVCUR (if 'drug') or AVCURCON (if control) for #nj1
c			(or float(iADC) for single sweeps)
c and, if V recorded,
c	Yval(i,2)=AVOLT (if 'drug') or AVOLTCON (if control) for #nj1
c			(or float(iADC1) for single sweeps)
c (b) If two data sets read (#nj1, #nj2) then
c	Yval(i,1)=AVCUR (if 'drug') or AVCURCON (if control) for #nj1
c			(or float(iADC) for single sweeps)
c	Yval(i,2)=AVCUR (if 'drug') or AVCURCON (if control) for #nj2
c			(or float(iADC) for single sweeps)
c and, if V recorded,
c	Yval(i,3)=AVOLT (if 'drug') or AVOLTCON (if control) for #nj1
c			(or float(iADC1) for single sweeps)
c	Yval(i,4)=AVOLT (if 'drug') or AVOLTCON (if control) for #nj2
c			(or float(iADC1) for single sweeps)
c
c SUBTRACTION of I/V data to get net (drug-con) curve has some problems
c Currents to be subtracted must be at same voltage but, at given t, the
c measured V (as opposed to the command) will not, in general, be same for
c for drug and control sweeps. Furthermore for interpolation of Y at given x
c the values of X must be strictly increasing (though not necessarilly equally
c spaced in the Numerical Recipes version SPLINE, as opposed to Sigworth
c version=SPLIN2). Noise in V trace means values may not be strictly increasing
c so must fit a polynomial to it first!
c
	real*4 t1v(10),t2v(10),t1c(10),t2c(10),vstep(10),ax(10),ay(10)
	real*4 t1v1(10),t2v1(10),t1c1(10),t2c1(10)	!to avoid duplic in COMMONS
	integer*4 ilenc(10),igapc(10)	!lengths of c-jumps and gaps between them
	integer*4 ilenv(10),igapv(10)	!lengths of V-jumps and gaps between them
	integer*2 ivolt1(10),ivolt2(10)  !pots for each V jump (integer mV)
	integer*2 ivhclamp,ipatch,iv1,iv2
	integer isuper(20)		!sweep #s to be superimposed
	integer lrnfit(20)		!learn fit sequence
	character units*8,unit1*8,note*79
	logical sampv,vramp,polyOK(10),refit,onelimb,plotg
	logical subasym,subtracted,resplot,redisp,first,plotqdat
	logical rescale,def0,super,norm,allocated,logx,logy,average
	logical align,endalign,alpha,expand,censor
c
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character cnum*11,cnum1*11,cnum2*11
	character*1 ch
	integer*4 ikeep1(4,2)
	integer*2 kstep(5)
	integer*1 jmask(nmax)
	logical keepall
	ALLOCATABLE::xa,ya,yout,bval,xpoly
	real*4 xa(:),ya(:),yout(:)		!temp arrays for interpolations
	real*4 bval(:),xpoly(:,:) 		!for polynomial fit
c For averaged activations from consam
	logical avact
	real*8 dfinter
c
c For plots
	integer itype(10),ifst(10),ilast(10)
	real*4 XVAL(ndv1,ndimd),YVAL(ndv1,ndimd)
	ALLOCATABLE::XCAL,YCAL
	real*4 XCAL(:,:),YCAL(:,:)
	real*4 Xfit(ndfit),Yfit(ndfit)	!now alloc in main prog
	real*4 theta(20)
c for VPLOT5 call
	ALLOCATABLE::weight,icurvw
	real*4 weight(:,:)
	integer icurvw(:)	!new for VPLOT5
	logical plotonly
c for data
c	dimension ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd)
c for calc curves
c	dimension ncal(ndimc),icurvc(ndimc),iline(ndimc)
	integer ncal(10),icurvc(10),iline(10)
	integer ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd)  !for data
	real*4 symsiz(ndimd)				!for data
	integer icrvdsav(15)	!to save orig icurvd if points averaged
	integer*4 videotyp
	character*40 titlex,titley
	character*64 title
	logical fitted,dcurve,doframe,draft,autplt,interp
	logical landscap,samescal,usesrate
	logical discprt,pon,debug,deb,caplock,slock,constr,pprt
	character*1 ans,UC,ans1
c For ASCII output
c=	real*4 data(2048,2)
	ALLOCATABLE::data
	real*4 data(:,:)
	character*30 colhead(10)
c for calc curves
	real*4 tau(10),dxc(10)
c
	character titlem*20,charout*1 		!for popmenu
	character*50 strings(5)		!for popmenu
	character*55 helps(5)	!for popmenu
c
	logical fixy0
	common/fix/fixy0
	logical help
	common/hlp/help		!for QDIALOG
	COMMON/JLOGOS/t1c,t2c,t1v,t2v,xoff1,y1v,y2v,y1c,y2c
	COMMON/BLOCK1/constr,Xv,kmax,nmod,ncomp,nfit,ip1,ip2,xjoin,slope
	common/vrev/km,theta	!for EFUNC and this routine
	common/dp/discprt
c Add common/cols/ to allow colours to be specified for VPLOT. Declarations:
	integer icol(100)
	logical mono
	COMMON/cols/icol,mono
c
c define function
	pon()=slock()
	debug()=caplock()
c
101	format(a1)
4	format(i8)
c
c
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
	eps=1.
	if(.not.usesrate) then	!otherwise use input value of srate
	   srate=float(irate)
	endif
	pprt=pon()			!so pon() called once only to avoid int11 crashes
	do 40 i=1,10
40	polyOK(i)=.false.
	onelimb=.false.
	plotg=.false.	!plot chord conductance (not current)
	nnew=50			!default
	km=kmax		!copy for common/vrev
	cbig=2.5
	ifont=4
	subasym=.false.
	subtracted=.false.
	redisp=.false.
	first=.true.	!for first display of jumps etc in vplot
c additions for VPLOT5
	if(allocated(weight)) then
	   DEALLOCATE(weight,icurvw)
	endif
	kthet=20	!dimension of theta()
	plotonly=.false.
	kwi=1
	kwj=1
	ALLOCATE(weight(kwi,kwj),icurvw(ndimd))
	do i=1,ndimd
	   icurvw(i)=-1	!no SD defined in VPLOT5
	enddo
	if(.not.avact) dA=1.e6/srate	!# of microsec between ADC samples
c Problem with commons (see Modif notes above)
	do 33 i=1,10
	  t1c(i)=t1c1(i)
	  t2c(i)=t2c1(i)
	  t1v(i)=t1v1(i)
	  t2v(i)=t2v1(i)
33	continue
c Use default colours unless otherwise specified
	if(.not.(fitted.or.dcurve.or.samescal)) then
	   Erev=0.0		!reversal pot for calc of conductance
	   do i=1,100
		icol(i)=-1	!use default
	   enddo
	endif
c  Values for popmenu
	nxlo=100
	nylo=-1
	nyhi=470
	ictm=15		!text white
	ibkm=1		!background dark blue
	icfm=9		!frame dark blue bright
      icupm=12		!upper case red
c
c Deal with superimposed curves: define Xval,ncurvd etc
	if(super) then
	   dx=1.e3/srate
	   norm=.false.
	   if(nsuper.lt.0) then
		norm=.true.
		nsuper=iabs(nsuper)
		print 45
45		format(
     & ' Use mean current between t1,t2 to normalise baseline:',
     & ' t1,t2 (ms) = ')
	      call INPUT2r(t1,t2)
	      it1=1+ifixr(t1/dx)	!valid only for 'keepall' case
	      it2=1+ifixr(t2/dx)
	   endif
	   if(norm) then
	     s=0.
	     n=0
	     do 42 j=1,nsuper
		if(keepall) then
		  do 43 i=it1,it2
43		  s=s+yval(i,j)
		  s=s/float(it2-it1+1)
		else
		   n1=0
		   do 431 i=1,nsamp
		    t=xval(i,j)
		    if(t.gt.t1.and.t.le.t2) then
			n1=n1+1
			s=s+yval(i,j)
		    endif
431		   continue
		   s=s/float(n1)
		endif
		if(j.eq.1) then
		   s1=s		!mean for 1st trace
		else
		   do 44 i=1,nsamp
		    y=s1-s
44		   yval(i,j)=yval(i,j)+y	!add offset
		endif
42	     continue
	   endif
c
	   ncurvd=nsuper
	   do 46 i=1,nsuper
	    icurvd(i)=i
	    ndat(i)=nsamp
	    isym(i)=0			!'points'
	    ijoin(i)=-1		!points not joined
46	   continue
	   titlex='time (ms)'
	   titley='current (pA)'
	   iscal=1	!scale internally
	   goto 120		!plot straight away
	endif			!end of super
c
	if(fitted.or.dcurve) then
	   ifitype=0		!no display of fitted parameters
	   landscap=.true.
c values for I/V plot
	   if(vramp) landscap=.false.
	   autplt=.false.
	   GOTO 10
	endif
c
c Set ncurvc=0 before label 85, so ncurvc is not reset to 0 if we return
c to label 85, 80, 90 etc to redraw curve(s)
	ncurvc=0
	ndimc=1	!must give value for vplot
c Section done when averaging s-s activations from consam
85	continue
	if(avact) then
	   jc=1
	   nj2=0
	   iplot=11
	   ncurvd=1
	   icurvd(1)=1
	   ndat(1)=nsamp
	   icol(1)=12
	   isym(1)=0	!points
	   ijoin(1)=-1		!points not joined
	   xmin=-tpre	!ms
	   xmax=avsamp	!ms
	   dx=sngl(dfinter*1.d-3)
c	   dx=(xmax-xmin)/float(nsamp)
	   n=(xmax-xmin)/dx
	   if(n.ne.nsamp) then
		print 47, n,nsamp
47		format(' n, nsamp = ',2i9)
	   endif
	   s=0.0
	   n=0
	   do i=1,nsamp
		xval(i,1)=xmin + float(i-1)*dx	!starts at xmin=-tpre
		if(xval(i,1).lt.0.) then
		   s=s+yval(i,1)
		   n=n+1
		endif
	   enddo
	   s=s/float(n)	!mean of first tpre msec
	   ymin=1.e37
	   ymax=-1.e37
	   do i=1,nsamp
		y=yval(i,1) - s
		if(y.gt.ymax) ymax=y
		if(y.lt.ymin) ymin=y
		yval(i,1)=y
	   enddo
	   ymax=1.0+float(ifixr(ymax))
	   ymin=float(ifixr(ymin))-1.0
	   ilabel=1
	   titlex='time (ms)'
	   titley='mean current (pA)'
	   iscal=4 		!use input xmin,xmax,ymin,ymax only
	   goto 120		!plot it
	endif
c Define j in Yval(i,j) that hold current (and voltage if sampled)
c   (a) If one data set only read (#nj1, nj2=0) then jc=1,jv=1+ngr so need
c	2*ngr arrays altogether (OK up to ngr=10, nsamp=20480 if voltage
c	not recorded, or ngr=5, nsamp=10240 if voltage recorded)
c   (b) If two data sets read (#nj1, #nj2) then for #nj1 jc=1, and
c	for #nj2 jc=1+ngr: 2*ngr arrays altogether (OK up to ngr=5, nsamp=10240
c	if voltage not recorded. If voltage recorded then for #nj1 jv=2*ngr+1
c	and for #nj2 jv=3*ngr+1; need 4*ngr arrays altogether (OK up to ngr=2,
c	 nsamp=4096), ie 1-8, and this leaves 9,10 for NET current (in case
c	 of crude ramp calcs)
	ngr=1 + (nsamp-1)/2048	!# of 2048-point arrays for nsamp points
	if(nj2.eq.0) then
	   jc1=1		!current in yval(i,1) for #nj1
	   jv1=2		!voltage in yval(i,2) for #nj1
	   if(sampv) then
	     jc5=3		!current from points in ramp only for nj1
	   endif
	else
	   jc1=1		!current in yval(i,1) for #nj1
	   jc2=2		!current in yval(i,2) for #nj2
	   jc7=3		!net current
	   if(sampv) then
	     jv1=3		!voltage in yval(i,3) for #nj1; orig data
	     jv2=4		!voltage in yval(i,4) for #nj2; orig data
	     jc5=5		!current from points in ramp only for nj1
	     jc6=6		!current from points in ramp only for nj2
	     jc7=7		!net current
	   endif
	endif
c Swap indices so jc1,jv1 is control; jc2,jv2=drug
	if(ncon.eq.2.and.nj2.gt.0) then
	   j=jc1
	   jc1=jc2
	   jc2=j
	   j=jv1
	   jv1=jv2
	   jv2=j
	endif
c
	if(align.and.endalign) jc11=jv1
c
c Check size of currents to decide whether to use pA,nA or muA as units.
c NB if units are changed (from pA) then currents in Yval(i,jc) are
c scaled by appropriate factor; this fact is recorded only by the text
c in 'units' which must NOT be overwritten (unless values re-scaled too)
c and which must be referred to when currents in Yval(i,jc) used
c again for other plots
c
	if(samescal.or.jfit.eq.4) goto 81
	do 83 i=1,nsamp	!should be OK for nsamp>2048 if jc is correct
	  y=Yval(i,jc1)
	  if(y.lt.ymin1) ymin1=y
	  if(y.gt.ymax1) ymax1=y
	  if(nj2.gt.0) then
	    y=Yval(i,jc2)
	    if(y.lt.ymin1) ymin1=y
	    if(y.gt.ymax1) ymax1=y
	  endif
83	continue
c Fix suitable units for Yval(i,jc1), Yval(i,jc2) and scale them.
c NB if return to here must be at a point AFTER the next line
c so the current correct units (pA on entry) are not overwritten)
	units='(pA)'		!specify input units for CURUNIT
	call CURUNIT(ymin1,ymax1,ytic1,ifont,units,nu,scale)	!also does FIXAX
	if(abs(scale-1.0).gt.0.01) then
	   do 84 i=1,nsamp
	    Yval(i,jc1)=scale*Yval(i,jc1)
	    if(nj2.gt.0) Yval(i,jc2)=scale*Yval(i,jc2)
84	   continue
	endif
81	continue
c
c For jumps:
c	In general may not have an ADC point that is exactly coincident with
c then 1st point in 1st jump. The 1st jump starts at
c [iADC(nAv)-1]*dA + dnAv microsec from trigger where value for nAv has been
c stored in ifst(1)
c	t1=(float(nAv-1)*dA + dnAv)/1000. !msec from trigger to start of 1st pulse
c and ends at
c	t2=t1+float(ilenv(i))*1.e-3		!msec from trigger to end of pulse
c Best to recalc the start and end points in terms of time from trigger (msec)
c and keep in t1(),t2()
	if(nvjump.gt.0) then
	  t1v(1)=(float(nAv-1)*dA + dnAv)/1000. !msec from trigger to start of 1st pulse
	  do 211 i=1,nvjump
	    t2v(i)=t1v(i)+float(ilenv(i))*1.e-3	!msec from trigger to end of pulse
c	    if(debug()) print 213,i,t1v(i),t2v(i)
c213		format(' i,t1v(i),t2v(i) = ',i4,2g11.4)
	    if(i.eq.nvjump) goto 211
	    t1v(i+1)=t2v(i) + float(igapv(i))*1.e-3 !time for start of next pulse
211	  continue
	endif
c NB for data from plot queue t1c() etc are defined (but nAc etc are not)
	if(ncjump.gt.0.and.(.not.plotqdat)) then
	  t1c(1)=(float(nAc-1)*dA + dnAc)/1000. !msec from trigger to start of 1st pulse
	  do 212 i=1,ncjump
	    t2c(i)=t1c(i)+float(ilenc(i))*1.e-3	!msec from trigger to end of pulse
	    if(i.eq.ncjump) goto 212
	    t1c(i+1)=t2c(i) + float(igapc(i))*1.e-3 !time for start of next pulse
212	  continue
	endif
c
	iplot=1		!default initially
	ipuls=1		!ditto
c
	refit=.false.
c	ifit=1			!now learned
c
c RETURN HERE FOR NEXT DISPLAY
60	continue			!return for next display
	vramp=.false.
c First define Xval, in Xval(i,1) as though whole sweep were being plotted
c This will be used as it stands for iplot=1,2 but will be modified for
c iplot=3-6
	if(.not.(samescal.or.jfit.eq.4)) then
	  xmin=0.0
	  xmax=float(itsamp)/1000.		!msec
	  xoff1=0.
	  dx=1.e3/srate
	  if(keepall) then
	     do 130 i=1,nsamp
130	     xval(i,1)=float(i-1)*dx	!1st point at t=0
	  else
c	     dx=(xmax-xmin)/float(nsamp0)	!already def via irate (=max rate)
	     i1=0
	     do 152 i=1,nsamp0
	      if(jmask(i).eq.0) goto 152	!skipped point
	      t=float(i-1)*dx			!=msec from 0 to itsamp-1
	      i1=i1+1
	      xval(i1,1)=t
152	      continue
	   endif
	endif
c
c
c DECIDE WHAT PLOT TO DO
c NB If nj2=0 and not sampv (i.e. not ramp) then I(t) vs t (whole sweep)
c plotted without any question here
	if(nj2.eq.0) then
	   if(sampv) then
122		print 121,iplot
121		format(
     & 	' (1) Plot current - (I/t: whole sweep)',/,
     & 	' (2) Plot voltage - (V/t: whole sweep)',/,
     & 	' (3) Plot I/V curve from V-ramp',/,
     & 	' (4) Plot I/V curve from V-ramp (simple method)',/,
     & 	' (5) No more plots from this sweep',/,
     & 	' Option number [',i2,'] = ')
		call INPUTi(iplot)
		if(iplot.lt.1.or.iplot.gt.5) goto 122
		if(iplot.eq.2) goto 100		!plot voltage
		if(iplot.eq.3) goto 90		!plot I/V
		if(iplot.eq.5) goto 999
		if(iplot.eq.4) then
		   iplot=5				!so numbering as below for crude calc
		   goto 90				!plot i/v
		endif
	   endif
	   goto 80		!plot whole sweep (I vs t)
	endif
c
c Reach here only if nj2>0
c Add option to plot current for 1 event only?
c
111	continue
	if(sampv) then
	  print 1,iplot
1	  format(
     & ' (1) Plot current for BOTH sweeps - (I/t: whole sweep)',/,
     & ' (2) Plot voltage - (V/t: whole sweep)',/,
     & ' (3) Plot AGONIST and CONTROL I/V curves from ramp',/,
     & ' (4) Plot NET I/V curve from ramp',/,
     & ' (5) Plot AGONIST and CONTROL I/V with simple calculations',/,
     & ' (6) Plot NET I/V with simple calculations',/,
     & ' (7) No more plots from these sweeps',/,
     & ' Option number [',i2,'] = ')
	  call INPUTi(iplot)
	  if(iplot.lt.1.or.iplot.gt.7) goto 111
	  if(iplot.eq.7) goto 999
	else
	  print 16,iplot
16	  format(
     & ' (1) Plot current for BOTH sweeps - (I/t: whole sweep)',/,
     & ' (2) Plot NET current (difference between sweeps) (I/t)',/,
     & ' (3) No more plots from these sweeps',/,
     & ' Option number [',i2,'] = ')
	  call INPUTi(iplot)
	  if(iplot.lt.1.or.iplot.gt.3) goto 111
	  if(iplot.eq.3) goto 999
	  if(iplot.eq.2) iplot=7	!to signify net JUMP
	endif
c NB iplot=3-7 goto 90 (iplot=3-6=ramp I/V; iplot=7 is net jump)
	if(iplot.eq.11) goto 85
	goto(80,100,90,90,90,90,90) iplot
c
c Plot whole sweep (I vs t)- data in Yval for data set #nj1, and, if nj2>0
c superimpose data for nj2
80	continue
	if(samescal) then
	   samescal=.false.
	   iscal=0	!use input values of xmin etc
	   goto 120		!plot it
	endif
	if(jfit.ne.4) then
	  xmin=0.0			!these now defined above
	  xmax=float(itsamp)/1000.		!msec
	  xoff1=0.
	endif
	dx=1.e3/srate
	if(nj2.le.0) then
	   ncurvd=1
	   icurvd(1)=jc1
	   ndat(jc1)=nsamp
	   icol(jc1)=10	!bright green for control (j1=9)
	   isym(jc1)=0			!'points'
	   if(jfit.eq.4.and.jc1.eq.10.and.ijoin(10).lt.-10) then
		ijoin(10)=-101	!subract 100 as signal to interpolate
	   else
		ijoin(jc1)=-1		!points not joined
	   endif
	   do i=1,nsamp
	      i1=i
	      xval(i1,jc1)=xval(i,1)	!OK even if not keepall (I think)
	   enddo
	   if(align.and.endalign) then
		do i=1,nsamp
	         i1=i
	         xval(i1,jc11)=xval(i,1)	!OK even if not keepall (I think)
		enddo
		ndat(jc1)=nsamp
		icol(jc1)=10	!bright green for control (j1=9)
		isym(jc1)=0			!'points'
		ijoin(jc1)=-1
		ndat(jc11)=nsamp
		icol(jc11)=12	!bright red for end-aligned
		isym(jc11)=0			!'points'
		ijoin(jc11)=-1
		if(iplot1.le.0.or.iplot1.gt.4) iplot1=3	!initial default
		alpha=VIDEOTYP().eq.3		!alpha mode
		if(alpha) then
		   print 23,iplot1
23		   format(
     & ' (1) Plot only traces aligned on first opening',/,
     & ' (2) Plot only traces aligned on last shutting',/,
     & ' (3) Plot both of the above',/,
     & ' (4) Plot both of the above with latter reversed in time',/,
     & ' (5) No more plots from these sweeps',/,
     & ' Option number [',i2,'] = ')
		   call INPUTi(iplot1)
		else
		   TITLEm=' FITTING OPTIONS '
		   strings(1)='plot only traces aligned on First opening'
		   strings(2)='plot only traces aligned on Last shutting'
		   strings(3)='plot Both of the above'
		   strings(4)='plot both with latter Reversed in time'
		   strings(5)='No more plots'
		   nval=5
		   helps(1)='Choose which traces to plot'
		   helps(2)='Fourth option toggles time reversal'
		   nhelp=2
		   call POPMENU(nxlo,nylo,nyhi,strings,nval,ictm,icfm,icupm,
     &	    ibkm,titlem,helps,nhelp,ilinem,charout,ival)
		   iplot1=ilinem	!iline=0 for ESC=cancel
		endif
		if(iplot1.eq.5) goto 999
		if(iplot1.eq.1) then
		   ncurvd=1
		   icurvd(1)=jc1
		else if(iplot1.eq.2) then
		   ncurvd=1
		   icurvd(1)=jc11
		else if(iplot1.eq.3) then
		   ncurvd=2
		   icurvd(1)=jc1
		   icurvd(2)=jc11
		else if(iplot1.eq.4) then
		   ncurvd=2
		   icurvd(1)=jc1
		   icurvd(2)=jc11
		   do i=1,nsamp/2
			k=nsamp-i+1         !=n,n-1,...
			yt=Yval(i,jc11)
			Yval(i,jc11)=Yval(k,jc11)
			Yval(k,jc11)=yt
		   enddo
		endif
	   endif
	else if(nj2.ge.1) then
	   ncurvd=2
	   icurvd(1)=jc1
	   icurvd(2)=jc2
	   icol(jc1)=10	!bright green for control (j1=9)
	   icol(jc2)=12	!bright red for drug (j1=10)
	   ndat(jc1)=nsamp	!also OK for nsamp>2048 if jc appropriate
	   ndat(jc2)=nsamp
	   isym(jc1)=0
	   isym(jc2)=9		!x sign
	   symsiz(jc2)=0.1*cbig
	   ijoin(jc1)=-1		!points not joined
	   ijoin(jc2)=-1		!points not joined
	   do 823 i=1,nsamp
	    x=xval(i,1)			!time define above
	    xval(i,jc1)=x
	    xval(i,jc2)=x
823	   continue
	endif
	if(jfit.eq.4) goto 77
	ymax=ymax1		!defined above by CURUNIT for I(t) vs t plot
	ymin=ymin1		!defined above by CURUNIT for I(t) vs t plot
	ytic=ytic1		!defined above by CURUNIT for I(t) vs t plot
	ilabel=1
	titlex='time (ms)'
	titley='current '//units(1:nu)
c get xmin,xmax
	xmin1=1.e37
	xmax1=-1.e37
	do 2 j1=1,ncurvd		!scan ncurvd data sets
	j=icurvd(j1)		!data set to be scanned
	do 3 i=1,ndat(j)
	if(xval(i,j).lt.xmin1) xmin1=xval(i,j)
	if(xval(i,j).gt.xmax1) xmax1=xval(i,j)
3	continue
2	continue
	call FIXAX(xmin1,xmax1,xmin,xmax,xtic,0)	!always non-log
77	continue	!jump here if jfit=4 (same scales)
	iscal=0	!use input values of xmin etc
	   y1c=ymin+0.96*(ymax-ymin)         !initial positions for jump logos
	   y2c=ymin+1.00*(ymax-ymin)
	   y1v=ymin+0.94*(ymax-ymin)
	   y2v=ymin+0.90*(ymax-ymin)
	goto 120		!plot it
c
c
c Plot jump (I/t) or ramp (I/V); drug+control (iplot=3), or net (iplot=4)
c NB this may be for c-jump OR v-jump -just any pair of records to be
c subtracted. The whole traces are plotted in iplot=1,2; the point of
c this section is (1), for iplot=3 to preset xmin,xmax to show interesting
c bit (and to set t=0 for it), and (b), for iplot=4 to show the net trace
c (some or all of it)
90	continue
c Plot conductance rather than current for IV ?
c First check V-jumps: Check how many jumps/ramps in the sweep
C NB: if nvjump=0 then itype not defined
	if(nvjump.gt.0) then
	  nramp=0
	  do 8 i=1,nvjump
	   if(ivolt1(i).eq.ivolt2(i)) then
		itype(i)=0	!ith pulse is a jump
	   else
		itype(i)=1	!ith pulse is a ramp
		nramp=nramp+1
	   endif
8	  continue
c
c  Calculate which points from the entire sweep are to be plotted, in
c terms of index in iADC (following calc was used for drawing horizontal bars
c in LPLOTCJ), and below, but not used now to allow for fact that 1st point
c of jump may not be exactly coincident with an ADC sample; however this
c calc can still be used here -it will find ADC points at start and end of
c jump with max error of 1 point which is quite good enough for choosing
c points to be displayed).
c NB THESE VALUES USED ONLY FOR RAMPS NOW
	  j1=nAv
	  itcum=0		!cumulative time from start of 1st pulse (int*2) (musec)
	  do 21 i=1,nvjump
	  itcum=itcum+ilenv(i)
	  il=ifixr(float(itcum)/dA)   !# of elements of iADC corresp to ilen
	  j2=nAv+il		! 1st pulse is at iADC(nAv)
	  ifst(i)=j1	!index in iADC, avcur of 1st point of ith pulse
	  ilast(i)=j2	!index in iADC, avcur of last point of ith pulse
	  itcum=itcum+igapv(i)
	  ig=ifixr(float(itcum)/dA)   !# of elements of iADC corresp to igap
	  j1=nAv+ig		! 1st V-jump is at iADC(nAv)
21	  continue
	endif 	!end calcs for V-jumps
c
c NOW PLOT PULSE # ipuls. First plot the two traces (drug,control) and
c then plot difference. Calculate the difference here.
c For jumps and ramps only part of the record is plotted, so make new data
c arrays containing relevant part of data, as follows:
c 'Control' current in Yval(i,5),Xval(1,5)
c 'Drug' current in Yval(i,6),Xval(i,6)
c Difference=drug-control in Yval(i,7),Xval(i,7)
c  (NB net=#nj2 - #nj1  if ncon=1;  other way round if ncon=2)
c
c For a RAMP, plot from first point of ramp to last (point #i1 to i2)
c   Scale X in mV for ramps
c For V-JUMP start plot before ifst() to get baseline, and, to get
c   off-jump plot equal length after ilast()
c   Scale X in msec for jumps, the x=0 at time of on-jump
c   For pulse #ipuls want t=0 to correspond to t1v(ipuls)
c   First point plotted will be ADC(i1), and this point is (i1-1)*dA/1000 msec
c   from the trigger (NO- keep all points -see below)
c For V-JUMPS  or C-JUMPS keep the whole array, so that can rescale the
c   graph as required to show more or less baseline etc. Just shift t=0 to
c   start of jump before calling, and fix plausible xmin,xmax so the
c   requested jump fills most of screen initially. Need to call with ISCAL=0
c   so the input values of xmin,xmax are used
c
c CALC FOR JUMPS in this version only NET jump done here (unsubtracted
c sweeps done at label 80), ie iplot=7
c Define net current here in Yval(i,jc7)
	if(iplot.eq.7) then
	   vramp=.false.
	   nplot=nsamp
	   nnet=nsamp
	   if(debug()) then
		print 801,jc7,jc2,ngr,nplot
801		format(' jc7,jc2,ngr,nplot = ',4i8)
		pause
	   endif
	   do 15 i=1,nplot	!=nsamp for jumps
		x=xval(i,1)			!time now defined above
	      Yval(i,jc7)=Yval(i,jc2)-Yval(i,jc1)
	      Xval(i,jc7)=x
15	   continue
	endif			!end of calcs for jumps
c
c NOW CALCS FOR RAMPS
c Ask if long-limb only to be used
	ans1='N'
	if(onelimb) ans1='Y'
	call DCASK('Plot only the ''long limb'' of the ramp',ans1,ans)
	onelimb=ans.eq.'Y'
c	if(itype(ipuls).ne.0) then	!ramp
	if(iplot.ne.7) then		!ramp: iplot=3,4,5,6
	   vramp=.true.
c vstep1 redefined below (as 1.0mV at present), rather than using the
c actual step size put out from DAC
c	   vstep1=vstep(ipuls)	!indiv step size in the ramp (mV)
	   i1=ifst(ipuls) 	!for ramp
	   i2=ilast(ipuls)
	   iv1=ivolt1(ipuls)+ivhclamp
	   iv2=ivolt2(ipuls)+ivhclamp
	   if(ipatch.eq.2.or.ipatch.eq.3) then
		iv1=-iv1	!reverse pipette pot for i/o or c/a
		iv2=-iv2
	   endif
	   xmin=float(iv1)
	   xmax=float(iv2)
	   if(xmax.lt.xmin) then
		x=xmax
		xmax=xmin
		xmin=x
	   endif
	   nplot=i2-i1+1	!# of points to plot
	   nplot1=nplot
	   dx=(xmax-xmin)/float(nplot)	!for ramps
c Plot points i1 to i2 (and, for jump, mark posn of jump with bar)
c or use GETLIMB to identify points that lie on the 'long limb' of the ramp
c (these are defined in CJUMP, but as before find them empirically here
c by looking for max and min in recorded voltage), and plot only points
c il1 to il2
c	   if(debug().and.discprt) write(8,53) ipuls,i1,i2
c53	   format(' ipuls,i1,i2 = ',3i8)
c
c Crude calculations (as debug mainly -not even corrected for time
c difference between I and V samples)
   	   if(iplot.eq.5.or.iplot.eq.6) then		!crude method
		jv=jv1		!do first for control
		jc=jc1
1521		continue		!return here to do drug
		call GETLIMB(nplot1,i1,jv,iv1,iv2,il1,il2,vmin,vmax,
     &	 Yval,ndv1,ndimd)
		if(onelimb) then
		   nplot=il2-il1+1	!new value for long limb only
		   i0=il1			!start index for long limb
		else
		   i0=i1
		endif
		do 1511 i=1,nplot
c=	       ii=i1+i-1			!=i1,i1+1,...
	       ii=i0+i-1			!=i0,i0+1,...
c	       x=Yval(ii,jv)		!x=voltage
c Current on ADC0 is first point so should  for cur(i) should use
c Vm(i)= mean of V(i-1) and V(i); for cur(1) must therefore extrapolate
c so Vm(1)=V(t1)-0.5*(V(t2)-V(t1))
		 if(i.eq.1) then
		   x=Yval(ii,jv)-0.5*(Yval(ii+1,jv)-Yval(ii,jv))
		 else
		   x=0.5*(Yval(ii-1,jv)+Yval(ii,jv))
		 endif
c control
		 if(jc.eq.jc1) then
	         Yval(i,jc5)=Yval(ii,jc)	!was Yval(i,5) on LHS
	         Xval(i,jc5)=x
		 else
c drug
	         Yval(i,jc6)=Yval(ii,jc)
	         Xval(i,jc6)=x
c net
	         Yval(i,jc7)=Yval(i,jc6)-Yval(i,jc5)
	         Xval(i,jc7)=x
		 endif
1511	      continue
	      if(jv.eq.jv1.and.nj2.gt.0) then
		   jv=jv2		!do first for control
		   jc=jc2
		   goto 1521
		endif
		ncon=nplot
		ndrug=nplot
		nnet=nplot
		goto 91
	   endif				!end of crude method
c End of crude method (iplot=5,6)
c
c Note: the voltage for I/V is not sampled at exactly same time as current
c but at equispaced times.
c Current on ADC0 is first point so should  for cur(i) should use
c Vm(i)= mean of V(i-1) and V(i); for cur(1) must therefore extrapolate
c so Vm(1)=V(t1)-0.5*(V(t2)-V(t1)) (or use cubic spline interpolation for
c this too?)
c PRESENT VERSION uses neither of these methods, but first fits polynomials to
c the recorded voltage (one limb at a time). Sample rate (per channel) is
c irate so dx=1.e3/srate =# of ms between ADC samples for each channel
c so current samples are at t1(i)=(i-1)*dx=0,dx,2dx,..., and voltage samples
c are at t2(i)=(i-0.5)*dx=0.5dx,1.5dx,2.5dx,... (i=1,2,...,n). This polynomial
c used to calculate voltage at times t1(i), ie voltage values at same times
c as the current values (ie polynomial interpolation used) and this gives
c strictly increasing array of voltages (as long as the fitted poly has
c no stationary points!), not necessarily equally spaced, with their
c corresponding currents, which are suitable as input for SPLINE to
c interpolate the currents at new fixed voltages (same for drug and control)
c
c First identify where end of 1st and 2nd limbs are, as max,min voltages
c (if iv1>iv2), or as min,max if iv2>iv1. The voltages are in Yval(ii,jv1)
c for control, Yval(ii,jv2) for drug, where ii=i1+i-1, i=1,2,...,nplot
	   jv=jv1		!do first for control
	   jc=jc1
c
154	   continue		!return here to do drug
	   call GETLIMB(nplot,i1,jv,iv1,iv2,il1,il2,vmin,vmax,
     &     Yval,ndv1,ndimd)
	   if(onelimb) then
		nplot1=il2-il1+1	!new value for long limb only
c=		i0=il1			!start index for long limb
	   else
c=		i0=i1
	   endif
c Now fit polynomial to each of three limbs of the measured voltage,
c ie to points i1 to il1, points il1 to il2, and points il2 to i2.
c NB il1,il2 are defined as index in original Yval(*,jv) in which 1st
c point of ramp is Yval(i1,jv) and last point is Yval(i2,jv).
c Note that point il1 is used in fits of both 1st and 2nd limbs, and
c il2 is used in 2nd,3rd. For first limb the offset by 0.5dt has to be calc
c by extrapolation for the 1st point, but last point is half way between
c points il1-1,il1 so this should be used (rather than extrapolating 2nd
c limb backwards by 0.5dt to get voltage #il1 (and similarly for il2). Thus
c end up with nfit calc points though altogether nfit+2 have been used
c in fitting (#il1 and #il2 are used twice)
	   dt=1.e3/srate
c Interpolation to allow for fact that I,V not sampled at same moment
	   ndeg=5		!degree of polynomial to try first
c Return to 62 for fit with different degree
62	   continue
	   npar=ndeg+1
c
	   if(onelimb) then
		ist=il1 	!do 2nd limb only
		iend=il2
		iv=il1-1		!index for calc curve V(t) vs t (xcal,ycal)
	   else 		!do all three, starting with first
		ist=i1			!index in Yval(*,jv)
	 	iend=il1
		iv=0			!index for calc curve V(t) vs t (xcal,ycal)
	   endif

	   ic=0			!index for curve I(V) vs v
	   in=0			!index for net I/V curve
c Return to 68 for next limb (xpoly,ya defined afresh for each limb)
c Memory problem caused by enormous arrays in GENREG; GENREG1 is version
c that uses less space (and enormous amount of space could be saved by
c removing XPOLY and using XVAL temporarily instead, or special version
c of GENREG for polynomial regression that needs only one row of XPOLY
c since they are all the same!)
68	   continue
	   nfit=iend-ist+1	!points to be fitted for this limb
c
c Allocate Xpoly
c Allocate Xcal, Ycal (need nfit rows here, for I-Vs etc)
	   ndimc=10
	   if(avact) ndimc=2
	   ndc1=nsamp	!need all points for ramp calcs
	   if(allocated(xcal)) then
		DEALLOCATE(xcal,ycal)
	   endif
	   ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
	   nd1=npar
	   if(allocated(bval)) then
	      DEALLOCATE(bval,xpoly)
	   endif
	   if(allocated(ya)) then
		DEALLOCATE(ya,xa,yout)
	   endif
	   ALLOCATE(bval(nd1),xpoly(nfit,nd1),ya(nfit),
     &	xa(nfit),yout(nfit))
	   do 65 i=1,nfit
	      ii=ist+i-1					!=ist,ist+1,...
	      ya(i)=Yval(ii,jv)
		x=(float(i)-0.5)*dt !=0.5dt,1.5dt,2.5dt,...=actual time of V samples
		xpoly(i,1)=1.0
		do 63 j=2,npar
63		xpoly(i,j)=x**(j-1)
65	   continue
c=	   call GENREG1(bval,Xpoly,ya,npar,Nfit,20,2048,20,2048)
	   call GENREG3(bval,Xpoly,ya,npar,Nfit)
c Now calculate values at same times as current samples (0,dt,2dt,...) and
c keep them in Ycal for display (NB 1st point is extrapolated)
	   do 67 i=1,nfit
c Degree of polynomial = ndeg = kmax-1
	   x=float(i-1)*dt	!starts at 0 as for current, I(t), samples
	   x2=0.
	   y=bval(ndeg+1)
	   do 20 j=ndeg,1,-1
	   x2=x2*x + y
20	   y=y*x + bval(j)
c keep calc values in temp arrays
c	   xa(i)=x		!time
	   xa(i)=y		!voltage (y axis for plot, but x axis for interpolation)
	   ii=ist+i-1			!=ist,ist+1,...
	   ya(i)=Yval(ii,jc)		!current at time t, for interpolation
67	   continue
c Keep calculated values for display: want j=5 for control, j=6 for drug for
c the final I/V plot, so keep data for this preliminary V(t) vs t plot (to
c check polynomial fit) in j=9 for control, j=10 for drug. Note
c that orig data is plotted from point i1 to i2 only, ie 1 to nplot in
c the new (j=5,6) arrays, so first point plotted is Yval(i1,jv1)=Yval(1,5) etc
c Then repeat above for next leg [in debug version put 'calc' curve in
c xval(*,9),xval(*,10)]
	   if(jv.eq.jv1) then
		j=5			!control
		j1=9
	   else
		j=6			!drug
		j1=10
	   endif
c As each limb is fitted, store the time and voltage for plotting (to check
c fit of polynomial), and also interpolate the current (for this limb)
c at fixed voltages, and calculate points needed
c for separate control, drug I/V and for the net I/V
c Calculate the voltages from the polynomial fit: these are already in xa(i)
c [and also in ycal(iv,j), or yval(iv,j1) in debug version]. The
c corresponding (ie at same TIME) currents are in Yval(ii,jc). Copy
c current into ya(i) for interpolation
c Calculate the 2nd derivatives in Y2A(). Note that 1st derivative should
c be about zero at each end: holding current constant at ends, and max/min
c at joints between limbs (or at joints could use slope at zero of the
c polynomial fitted to next limb?)
	   yp1=0.	!slope at ends
	   ypn=0.
	   vstep1=0.2 		!use 0.2 mV for now
c	   vstep1=1.0 		!use 1.0 mV for now
c	   deb=debug()
c For case of 'onelimb' have ist=il1 so second case done only
	   if(ist.eq.i1) then	!1st leg just completed
		do 69 i=1,nfit
		 iv=iv+1
		 xcal(iv,j1)=float(iv-1)*dt	!starts at t=0  -for V(t) vs t plot
		 ycal(iv,j1)=xa(i)		!voltage for V(t) vs t plot
69		continue
		call SPLINSUB(Xa,Ya,nfit,yp1,ypn,vstep1,yout,
     & 	v1,v2,nval,1,y20,y80,x20,x80,nerr,nfit)   !defines nval values
		if(nerr.ne.0) then
		   call BELL(3)
		   call INTCONV(nerr,cnum)
		   call WDIALOG(1,
     &	  'Error in interpolation: nerr = '//charnb(cnum),12)
		endif
		do 691,i=1,nval
		 ic=ic+1
		 xval(ic,j)=v1 + float(i-1)*vstep1  !voltage for I vs V plot (v1 to v2)
		 yval(ic,j)=yout(i)  !interp current for these V for I vs V plot
		 if(deb) then
		   print 692,1,i,j,ic,xval(ic,j),yout(i)
		   if(discprt) write(8,692) 1,i,j,ic,xval(ic,j),yout(i)
692		   format(1x,i4,' i,j,ic,xval(ic,j),yout(i)=',3i5,2g13.6)
		 endif
691		continue
c		ic1=ic	!record index in data at end of 1st limb
c          If this is 2nd time through (agonist) then calc net points for
c		this limb of I/V (do here to make sure that only currents from
c		same limb are subtracted -very important if subtracted curves
c		show hysteresis)
	      if(jv.eq.jv1) then
		   icon1=ic	 !record index in control data at end of 1st limb
		else
		   idrug1=ic !record index in agonist data at end of 1st limb
		   call GETNET(xval,yval,ndv1,ndimd,1,icon1,1,
     &	    idrug1,in,debug())
		endif
		ist=il1 	!prepare for 2nd limb
		iend=il2
		goto 68 	!do 2nd limb
	   else if(ist.eq.il1) then	!2nd limb completed
		do 70 i=2,nfit          !omit duplicated point #il1
		 iv=iv+1
		 xcal(iv,j1)=float(iv-1)*dt	!time for V(t) vs t plot
		 ycal(iv,j1)=xa(i)		!voltage for ditto
70		continue
		call SPLINSUB(Xa,Ya,nfit,yp1,ypn,vstep1,yout,
     & 	v1,v2,nval,1,y20,y80,x20,x80,nerr,nfit)   !defines nval values
		if(nerr.ne.0) then
		   call BELL(3)
		   call INTCONV(nerr,cnum)
		   call WDIALOG(1,
     &	  'Error in interpolation: nerr = '//charnb(cnum),12)
		endif
		do 701,i=1,nval
		 ic=ic+1
		 xval(ic,j)=v1 + float(i-1)*vstep1  !voltage for I vs V plot (v1 to v2)
		 yval(ic,j)=yout(i)  !interp current for these V for I vs V plot
		 if(deb) then
		   print 692,2,i,j,ic,xval(ic,j),yout(i)
		   if(discprt) write(8,692) 2,i,j,ic,xval(ic,j),yout(i)
		 endif
701		continue
	      if(jv.eq.jv1) then
		   icon2=ic	  !record index in control data at end of 2nd limb
		else
		   idrug2=ic  !record index in agonist data at end of 2nd limb
		   call GETNET(xval,yval,ndv1,ndimd,icon1+1,icon2,
     &		idrug1+1,idrug2,in,debug())
		endif
		if(onelimb) goto 681		!finished
		ist=il2	!prepare for 3rd limb
		iend=i2
		goto 68	!do 3rd
	   else		!3rd limb completed
		do 71 i=2,nfit          !omit duplicated point #il2
		 iv=iv+1
		 xcal(iv,j1)=float(iv-1)*dt	!time for V(t) vs t plot
		 ycal(iv,j1)=xa(i)		!voltage for ditto
71		continue
		call SPLINSUB(Xa,Ya,nfit,yp1,ypn,vstep1,yout,
     & 	v1,v2,nval,1,y20,y80,x20,x80,nerr,nfit)   !defines nval values
		if(nerr.ne.0) then
		   call BELL(3)
		   call INTCONV(nerr,cnum)
		   call WDIALOG(1,
     &	  'Error in interpolation: nerr = '//charnb(cnum),12)
		endif
		do 711,i=1,nval
		 ic=ic+1
		 xval(ic,j)=v1 + float(i-1)*vstep1  !voltage for I vs V plot (v1 to v2)
		 yval(ic,j)=yout(i)  !interp current for these V for I vs V plot
		 if(deb) then
		   print 692,3,i,j,ic,xval(ic,j),yout(i)
		   if(discprt) write(8,692) 3,i,j,ic,xval(ic,j),yout(i)
		 endif
711		continue
	      if(jv.eq.jv1) then
		   ncon=ic	  !record index in control data at end of 2nd limb
		else
		   ndrug=ic  !record index in agonist data at end of 2nd limb
		   call GETNET(xval,yval,ndv1,ndimd,icon2+1,ncon,
     &		idrug2+1,ndrug,in,debug())
		endif
		nnet=in		!no of values for net I/V plot
	   endif
681	   continue
c Keep number of values for control (ncon) and drug (ndrug) I/V plots
	   ncal(j1)=nplot
	   if(nplot.ne.iv) then
		call BELL(2)
		print 721,nplot,iv
721		format(' NPLOT,IV disagree = ',2i8)
		pause
	   endif
c Polynomials fitted to all limbs -display (and, if nec, refit with
c higher degree polynomial). Define DATA curves
	   do 72 i=1,nplot
	   xval(i,j1)=(float(i)-0.5)*dt !=0.5dt,1.5dt,2.5dt,....=times of V samples
	   ii=i1+i-1
	   yval(i,j1)=Yval(ii,jv)	!j1=9 for control, 10 for drug
72	   continue
	   iscal=1		!scale internally
	   icol(9)=10	!bright green for control (j1=9)
	   icol(10)=12	!bright red for drug (j1=10)
	   ncurvd=1
c#	   ncurvd=2
	   icurvd(1)=j1
	   ndat(j1)=nplot
c	   isym(j1)=9		!= x sign for data
	   isym(j1)=0		!= points for data
	   symsiz(j1)=0.1*cbig
	   ijoin(j1)=-1		!points not joined
	   ilabel=1
	   titlex='time (ms)'
	   titley='membrane potential (mV)'
c calc curves:
	   ncurvc=1
	   icurvc(1)=j1
c	   iline(j1)=0		!continuous
	   iline(j1)=3		!long dash for visibility
	   icol(j1+10)=11		!light blue for fit
c misc inputs
	   ifitype=0		!no display of fitted parameters
	   ilog=0
	   itx=1			!normal tick orientation
	   ity=1
	   ntx=5
	   nty=5
 	   xlo=-1		!whole screen
	   doframe=.false.		!to show jump logos
	   landscap=.true.
	   draft=.false.
	   autplt=.false.
	   itit=0

c	   iask=2	!do not ask before leaving display; leave graph on screen
	   iask=3	!come straight out of VPLOT2; leave graph on screen
c Extra params for VPLOT (ndv1, ndc1 already defined)
	   interp=.false.
	   isetcol=1	!set colours from icol() (unless icol=-1)
	   itrace=0		!not multiple traces
c Display of polynomial fit (NB set ivplot=false for this call)
	   if(polyok(ipuls)) goto 79	!fit already checked
	   call VPLOT5(Xval,Yval,ndat,icurvd,ncurvd,ijoin,symsiz,ndimd,
     &   XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,
     &   XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     &   XLO,XHI,YLO,YHI,y0,yinf,inumx,inumy,ncjump,nvjump,.false.,
     &   titlex,titley,ilabel,doframe,idiskq,autplt,plotonly,itit,title,
     &   cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,interp,
     &   isetcol,itrace,ndv1,ndc1,weight,kwi,kwj,icurvw,kthet,iver)
	   logx=ilog.eq.1.or.ilog.eq.3.or.ilog.eq.4.or.ilog.eq.6
	   logy=ilog.eq.2.or.ilog.eq.3
	   if(logx) then
		xmin0=10.0**xmin	!non-log value for calc curves
		xmax0=10.0**xmax	!non-log value for calc curves
	   else
		xmin0=xmin		!non-log value for calc curves
		xmax0=xmax		!non-log value for calc curves
	   endif
	   if(logy) then
		ymin0=10.0**ymin	!non-log value for calc curves
		ymax0=10.0**ymax	!non-log value for calc curves
	   else
		ymin0=ymin		!non-log value for calc curves
		ymax0=ymax		!non-log value for calc curves
	   endif
	   do i=1,100
		icol(i)=-1	!use default
	   enddo
	   ans='Y'
	   if(jv.eq.jv1) then
 		call DEFOLTa(ans,defolt)
		call QDIALOG(1,
     &	  'Is (blue) fit of polynomial to control voltage O.K.',
     &	  defolt,11,cans)
	   else
 		call DEFOLTa(ans,defolt)
		call QDIALOG(1,
     &	  'Is (blue) fit of polynomial to agonist voltage O.K.',
     &	  defolt,11,cans)
	   endif
	   call GETINPa(cans,ans)
c=	   read 101,ans
	   if(UC(ans).eq.'N') then
		call INTCONV(ndeg,cnum)
		ndeg=0
 		call DEFOLTi(0,defolt)
		call QDIALOG(1,
     & 	'Polynomial was degree '//charnb(cnum)//
     & 	': Refit with degree [abandon] n =',
     &	defolt,11,cans)
		call GETINPi(cans,ndeg)
c=		call INPUTi(ndeg)
		if(ndeg.ge.1) goto 62
 		call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
		goto 230			!another display?
	   endif
c
c Repeat polynomial fit and display for agonist ramp
79	   continue
	   if(jv.eq.jv1.and.nj2.gt.0) then
	      jv=jv2		!do first for control
	      jc=jc2
	      goto 154
	   endif
	   polyok(ipuls)=.true.		!to signal that polynomials already approved
	   DEALLOCATE(bval,xpoly,ya,xa,yout)
c
c Now have ncon values for control I/V plot in xval(i,5),yval(i,5) and
c ndrug values for agonist I/V plot in xval(i,6),yval(i,6). The values
c for the net I/V curve can now be calculated in xval(i,7),yval(i,7) for
c all points with same voltage.  The voltage values in Xval have same
c separation, but do not necessarilly start and end with same values,
c and, since they contain data for all three limbs, the same voltages
c will be repeated twice (usually). Do it the slow way by testing all
c possible combos (ncon*ndrug possibilities) -no! this gives double the
c correct number of values because it subtracts also, eg, the -140 mV point
c on one limb from that on the other limb. Sorting all values also gave
c problems (esp if unsubtracted curves show hysteresis) by subtracting
c points at same Voltage but from different limbs, so check one limb at
c a time
	endif		!end of calcs for ramps
c END OF RAMP CALCS
c
c PLOT DRUG AND CONTROL RAMP (iplot=3 or 5) or NET RAMP(iplot=4 or 6)
c or NET JUMP (iplot=7)
c Now define ncurvd etc and Xval,yval. Have 2 curves to show.
91	continue
	if(iplot.eq.3.or.iplot.eq.5) then
	   ncurvd=2
	   if(nj2.eq.0) ncurvd=1	!curve for j=5 is only one
	   icurvd(1)=jc5		!control
	   icol(jc5)=10	!bright green for control
	   ndat(jc5)=ncon
	   ijoin(jc5)=-1		!not joined
	   isym(jc5)=0		!points
	   symsiz(jc5)=0.1*cbig
	   if(nj2.gt.0) then
		icurvd(2)=jc6		!drug
		icol(jc6)=12	!bright red for drug
		ndat(jc6)=ndrug
		ijoin(jc6)=-1		!not joined
		isym(jc6)=-7		!circle
		symsiz(jc6)=0.1*cbig
	   endif
	else if((iplot.eq.4.or.iplot.eq.6.or.iplot.eq.7).and.nj2.gt.0)then
	   ncurvd=1
	   icurvd(1)=jc7		!net
	   icol(jc7)=14		!yellow for net
	   ndat(jc7)=nnet
	   ijoin(jc7)=-1		!not joined
	   isym(jc7)=0		!'point'
	endif
c
c Recheck ymin,ymax for all plotted curves, and rescale units if necessary
c (do not alter ymin1 etc which are for I(t) vs (t) plot)

	   deb=debug()
	   ymin2=1.e37
	   ymax2=-1.e37
	   do 131 j1=1,ncurvd
		j=icurvd(j1)
		do 132 i=1,ndat(j)
		if(xval(i,j).lt.xmin0.or.xval(i,j).gt.xmax0) goto 132	!skip
		y=Yval(i,j)
		if(deb) print 703,i,j,y,ymin2,ymax2
703		format(' i,j,y,ymin2,ymax2=',2i5,3g12.5)
		if(y.lt.ymin2) ymin2=y
		if(y.gt.ymax2) ymax2=y
132		continue
131	   continue
c 'units'=units for data in yval(i,jc). Do not want to change this! Say
c unit1=units for plotted curves (j=5,6,7 here)
	   unit1=units
	   call CURUNIT(ymin2,ymax2,ytic2,ifont,unit1,nu1,scale)	!also does FIXAX
c	   call FIXAX(ymin1,ymax1,ymin,ymax,ytic,0)	!now done in CURUNIT
	   ymin=ymin2
	   ymax=ymax2
	   ytic=ytic2
	   if(abs(scale-1.0).gt.0.01) then
	      do 842 j1=1,ncurvd
		j=icurvd(j1)
	      do 841 i=1,nsamp
	      Yval(i,j)=scale*Yval(i,j)
841	      continue
842	      continue
	   endif
	ilabel=1
	if(vramp) then
	   titlex='membrane potential (mV)'
	   titley='current '//unit1(1:nu1)
	   iscal=1		!scale internally
	else		!for jump
	   xmin2=xmin0     !calc above for this display
	   xmax2=xmax0
	   call FIXAX(xmin2,xmax2,xmin,xmax,xtic,0)	!always non-log
	   titlex='time (ms)'
	   titley='current '//unit1(1:nu1)
	   iscal=0	!use input values of xmin etc
c
	   y1c=ymin+0.96*(ymax-ymin)         !initial positions for jump logos
	   y2c=ymin+1.00*(ymax-ymin)
	   y1v=ymin+0.94*(ymax-ymin)
	   y2v=ymin+0.90*(ymax-ymin)
	endif
	goto 120		!plot it
c
c
c Plot voltage vs time only -both traces if nj2>0
100	continue
	do i=1,100
	   icol(i)=-1	!use default
	enddo
	xmin=0.0		!these now defined above
	xoff1=0.
	xmax=float(itsamp)/1000.		!msec
	xmin0=xmin
	xmax0=xmax
	xcross=xmin
	ycross=ymin
	iscal=1		!scale internally
c	iscal=-1		!scale internally except for xcross,ycross
	dx=1.e3/srate
c	dx=(xmax0-xmin0)/float(nsamp)
	icurvd(1)=jv1
	ndat(jv1)=nsamp
	isym(jv1)=0             !points
	ijoin(jv1)=-1		! not joined
	icol(jv1)=10	!bright green
	if(nj2.eq.0) then
	   ncurvd=1
	   do 822 i=1,nsamp
	    xval(i,jv1)=xval(i,1)     !time define above
822	   continue
	else
	   ncurvd=2
	   icurvd(2)=jv2
	   icol(jv2)=12	!bright red for drug
	   ndat(jv2)=nsamp
	   isym(jv2)=9		! x signs for 2nd curve=agonist
	   symsiz(jv2)=0.2*cbig
	   ijoin(jv2)=-1		!points not joined
	   do 824 i=1,nsamp
	    x=xval(i,1)			!time define above
	    xval(i,jv1)=x
	    xval(i,jv2)=x
824	   continue
	endif
	ilabel=1
	titlex='time (ms)'
	titley='membrane potential (mV)'
c Mark requested upper and lower limits of ramp if one ramp done (put
c in as 2 calc curves)
	ncalc=201 		!number of points for voltage-marking lines
	ndimc=2
	ndc1=ncalc
	if(allocated(xcal)) then
	   DEALLOCATE(xcal,ycal)
	endif
	ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
	do 827 m=1,nvjump
	if(ivolt1(m).ne.ivolt2(m)) then
	   ncurvc=2
	   icurvc(1)=1
	   icurvc(2)=2
	   icol(1+10)=14
	   icol(2+10)=14
	   ncal(1)=ncalc
	   ncal(2)=ncalc
	   dxcalc=(xmax0-xmin0)/FLOAT(ncalc-1)         !whole range of curve
	   iv1=ivolt1(m)+ivhclamp
	   iv2=ivolt2(m)+ivhclamp
	   if(ipatch.eq.2.or.ipatch.eq.3) then
		iv1=-iv1	!reverse pipette pot for i/o or c/a
		iv2=-iv2
	   endif
	   do i=1,ncalc
		x=xmin0+float(i-1)*dxcalc	!actual time as in current Xval()
		xcal(i,1)=x
		xcal(i,2)=x
	      ycal(i,1)=iv1
	      ycal(i,2)=iv2
	   enddo
	   iline(1)=2		!short dash
	   iline(2)=2		!short dash
	   goto 120
	endif
827	continue
	goto 120
c
c Now set general inputs and call VPLOT
120	continue
c
c Misc inputs for VPLOT
	ifitype=0		!no display of fitted parameters
	ilog=0
	itx=1			!normal tick orientation
	ity=1
	ntx=5
	nty=5
	xlo=-1		!whole screen
c	symsiz(1)=-1.0 	!default symbol sizes
c	do 41 i=1,10
c41	symsiz(i)=0.4*cbig
	xcross=xmin
	ycross=ymin
c	doframe=.true.
	doframe=.false.		!to show jump logos
	landscap=.true.
c values for I/V plot
	if(vramp) then
	   landscap=.false.
	   xcross=0.
	   ycross=0.
	   iscal=-1		!so input xcross,ycross used
	   itx=0		!ticks central
	   ity=0
	   doframe=.false.
	endif
	draft=.false.
	autplt=.false.
	itit=0
c Extra params for VPLOT4 (ndv1, ndc1 already defined)
	interp=.false.
	isetcol=1
	itrace=0		!not multiple traces
c calc curves:
c
	iask=2	!do not ask before leaving display; leave graph on screen
c
c   Reduce number of points displayed on I-V plot?
	if(iplot.ge.3.and.iplot.le.6) then		!I-V curve(s)
	   if(videotyp().ne.18) then
		idev=0					!screen
	   call VGA
	   call gsetcols(0)
	   call errswi(-1)
	   call brkswi(1)
	   call chaswi(1)
	   call grfmod (1)
	   call harcha
	   call mode(18)
	   endif
	   icfd=7		!frame col for dialog box 1
	   icbd=0		!background col for dialog box (0 until fixed)
	   call DEFDIALOG(1,1,2,4,60,icbd)	!define dialog box #1
	   call OPENDIALOG(1,icfd,.true.)	!draw dialog box #1
	   n=ndat(icurvd(1))	!number of data points
	   if(n.gt.100) then
		call INTCONV(n,cnum)
		ans='N'
		if(nnew.lt.n) ans='Y'
 		call DEFOLTa(ans,defolt)
		call QDIALOG(1,charnb(cnum)//
     &       ' data points for I-V: average to reduce number',
     &        defolt,11,cans)
 		call GETINPa(cans,ans)
		if(UC(ans).ne.'Y') goto 34
	   else
		goto 34
	   endif
 	   call DEFOLTi(nnew,defolt)
	   call QDIALOG(1,
     &	'New number of data points',
     &	defolt,11,cans)
	   call GETINPi(cans,nnew)
	   npg=n/nnew		!number of points/group (may be more in last)
	   j=icurvd(1)
36	   continue		!return for ncurvd=2
	   do m=1,nnew
		i1=(m-1)*npg+1
		i2=i1+npg-1
		if(m.eq.nnew) i2=n	!last group
		sx=0.0
		sy=0.0
		do i=i1,i2
		   sx=sx + Xval(i,j)
		   sy=sy + Yval(i,j)
		enddo
		en=float(npg)
		if(m.eq.nnew) en=float(i2-i1+1)
		sx=sx/en
		sy=sy/en
		Xval(m,j)=sx
		Yval(m,j)=sy
	   enddo
	   ndat(j)=nnew
	   isym(j)=-7		!small circles after averaging
	   symsiz(j)=1.0
	   if(ncurvd.eq.2.and.j.eq.icurvd(1)) then
		j=icurvd(2)
		ndat(j)=nnew
		isym(j)=-7
		symsiz(j)=1.0
		goto 36
	   endif
c        print 32
         if(pon()) write(7,32) nnew,npg,i2-i1+1
         if(discprt) write(8,32) nnew,npg,i2-i1+1
32	   format(
     &' Number of points on I-V curve reduced to ',i4,' by averaging',/,
     & '  in groups of ',i3,' (or ',i3,' in last group)')
34	   continue
c Plot conductance rather than current for IV ?
	   ans='N'
	   if(plotg) ans='Y'
 	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     &       'Plot (chord) conductance rather than current',
     &        defolt,11,cans)
 	   call GETINPa(cans,ans)
c	   call DCASK('Plot (chord) conductance rather than current',
c     &	ans,ans)
	   plotg=ans.eq.'Y'
	   if(plotg) then
 	      call DEFOLTr(Erev,defolt)
		call QDIALOG(1,
     &	'Reversal potential (mV)',
     &	   defolt,11,cans)
		call GETINPr(cans,Erev)
		j=icurvd(1)
361		continue		!return for ncurvd=2
		i1=0
		do i=1,ndat(j)
		   dp=0.001*(Xval(i,j)-Erev)		!driving pot (volts)
		   if(abs(dp).gt.0.003) then        !at least 3 mV from Erev
			i1=i1+1
			Yval(i1,j)=Yval(i,j)/dp
			Xval(i1,j)=Xval(i,j)
		   endif
		enddo
		ndat(j)=i1		!in case some omitted
		if(ncurvd.eq.2.and.j.eq.icurvd(1)) then
		   j=icurvd(2)
		   goto 361
		endif
		titley='chord conductance'	!fix units!!
	   endif
	endif
c
c Now do all displays apart from polynomial fit
	resplot=.false.

c===========================================================

30	continue	!return here to plot
	itit=0
c
	if(.not.allocated(xcal).and.ncurvc.eq.0) then
	   ALLOCATE(Xcal(1,1),Ycal(1,1))	!dummy values needed for call
	endif
	if(first) then
	   first=.false.
	   xminsav=xmin
	   xmaxsav=xmax
	   yminsav=ymin
	   ymaxsav=ymax
	endif
c
	if(dcurve) fitted=.true.		!so parameters drawn -reset on exit
c For huge files the averaged activations can be queued but not fitted
	if(avact.and.ndfit.eq.1) then
	   call BELL(3)
	   print 48
48	   format(/,
     &  ' HUGE file -queue the plot straight away -no fitting!')
	   pause
c	   call WDIALOG(1,
c     &  'Huge file -queue the plot straight away -no fitting!',12)
	endif
	call VPLOT5(Xval,Yval,ndat,icurvd,ncurvd,ijoin,symsiz,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO,XHI,YLO,YHI,y0,yinf,inumx,inumy,ncjump,nvjump,vramp,
     & titlex,titley,ilabel,doframe,idiskq,autplt,plotonly,itit,title,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,interp,
     & isetcol,itrace,ndv1,ndc1,weight,kwi,kwj,icurvw,kthet,iver)
	if(dcurve) fitted=.false.		! -reset on exit
	logx=ilog.eq.1.or.ilog.eq.3.or.ilog.eq.4.or.ilog.eq.6
	logy=ilog.eq.2.or.ilog.eq.3
	if(logx) then
	   xmin0=10.0**xmin	!non-log value for calc curves
	   xmax0=10.0**xmax	!non-log value for calc curves
	else
	   xmin0=xmin		!non-log value for calc curves
	   xmax0=xmax		!non-log value for calc curves
	endif
	if(logy) then
	   ymin0=10.0**ymin	!non-log value for calc curves
	   ymax0=10.0**ymax	!non-log value for calc curves
	else
	   ymin0=ymin		!non-log value for calc curves
	   ymax0=ymax		!non-log value for calc curves
	endif
c
	if(redisp) goto 86	!after rescale before marking 'last point to fit'
	if(align.and.endalign) goto 80
c
c If data values were offset, then restore orig. values now
	if(subtracted) then
	   subtracted=.false.
	   j=icurvd(1)
	   do i=1,ndat(j)
		Yval(i,j)=Yval(i,j) + theta(kmax)
	   enddo
	   xmin=xminsav
	   xmax=xmaxsav
	   ymin=yminsav
	   ymax=ymaxsav
	   xcross=xmin
	   ycross=ymin
	endif
c Next bit added 06/21/97 08:32pm for re-display of same data in main prog
c Set expand=true if display was expanded in vplot (so can ask whether to
c restore original after first point to be fitted has been marked)
	expand=(xmaxsav.gt.xmax0).or.(xminsav.lt.xmin0)
c
c Is this reset of colours really necessary?
c	do i=1,100
c	   icol(i)=-1	!use default
c	enddo
	if(pprt.and.ilog.eq.4) write(7,25)y0,yinf
      if(discprt.and.ilog.eq.4) write(8,25)y0,yinf
25	format(' For Hill plot y(0), y(inf)= ',2g13.6)
	if(super) then
 	   call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	   goto 230			!another display?
	endif
c
c Option to average in groups
	ans='N'
 	call DEFOLTa(ans,defolt)
	if(average) then		!already averaged
	   call INTCONV(nmean,cnum)
	   call QDIALOG(1,
     &   'Averaged in groups of '//charnb(cnum)//': change this'
     &	,defolt,11,cans)
	else
	   call QDIALOG(1,
     &    'Average points in groups above a specified X value'
     &	,defolt,11,cans)
	endif
	call GETINPa(cans,ans)
22	if(ans.eq.'Y') then
	   i=nmean
 	   call DEFOLTi(nmean,defolt)
	   call QDIALOG(1,
     &   'Average in groups of n points (n=1 for normal): n',
     &	defolt,11,cans)
	   call GETINPi(cans,nmean)
	   if(nmean.eq.i) then	!no change in nmean, so skip
		ans='N'
		goto 22
	   endif
c Restore icurvd() for original data so if different nmean requested it
c is applied to original data, not data already decimated
	   if(average) then
	      ncurvd=ncrvdsav
	      do i=1,ncurvd
		   icurvd(i)=icrvdsav(i)
		enddo
	   endif
	   if(nmean.eq.1.and.average) then		!restore original display
		average=.false.
		if(nmod.eq.0) then
		   ifitype=4		!display fitted param for exponentials
		   if(fixy0) ifitype=-4		!new option
		endif
		goto 30	!redisplay
	   endif
	   j1=1
	   if(ncurvd.gt.1) then
 		call DEFOLTi(j1,defolt)
		call QDIALOG(1,
     &	' -which curve to be so decimated (1=green, 2=red)',
     &	defolt,11,cans)
		call GETINPi(cans,j1)
		j=icurvd(j1)
	   else
		j=icurvd(1)
	   endif
c
	   call WDIALOG(1,
     &  'Mark the X value above which points to be averaged',12)
         idraw=-1
         line=0
         call crossh(jx,jy0,jxlo,jylo,jxhi,jyhi,icol(71),icf,itype,
     &     line,idraw,islope,ax,ay,6,70,6,1.)
	   xmean=ax(1)
	   call BELL(1)
	   iltype=3		!dashed
	   call broken(iltype)
	   call graMOV(xmean,ymin)
	   call gralin(xmean,ymax)
c	   if(logx) then
c		xmean=10**xmean
c	   endif
c Now average points -keep in xval(i,10), yval(j,10) [ndimd=10 at present]
c or j=2 for avact, when ndimd=2 (may also have ndimd=2 for iread=3)
	   if(avact) then
		jm=2
		j=1
	   else
		jm=10
	      j=icurvd(j1)	!j1=index for curve to be fitted
	   endif
	   if(jm.gt.ndimd) jm=ndimd
	   x=xval(1,j)
	   i=0
	   do while(x.lt.xmean)
		i=i+1
		x=xval(i,j)
		xval(i,jm)=x
		yval(i,jm)=yval(i,j)
	   enddo
c allocate values above xmean (averaged)
	   in=i			!last xnew() allocated
	   it1=i+1
	   itlast=i+nmean
	   en=float(nmean)
	   do while(itlast.le.ndat(j))
		sx=0.0
		sy=0.0
		do it=it1,itlast
		   sx=sx+xval(it,j)
		   sy=sy+yval(it,j)
		enddo
		in=in+1
		xval(in,jm)=sx/en
		yval(in,jm)=sy/en
		it1=itlast+1
		itlast=it1+nmean-1
	   enddo
c Also average any remaining values (will have n<nmean)
	   en=float(nval-it1+1)		!number averaged in last group
	   if(it1.lt.nval) then
		sx=0.0
		sy=0.0
		do it=it1,nval
		   sx=sx+xval(it,j)
		   sy=sy+yval(it,j)
		enddo
		in=in+1
		xval(in,jm)=sx/en
		yval(in,jm)=sy/en
	   endif
c Save original ncurvd,icurvd for unaveraged data to restore display
	   if(.not.average) then
		do i=1,ncurvd
		   icrvdsav(i)=icurvd(i)
		enddo
		ncrvdsav=ncurvd
	   endif
c can now set average=true
	   average=.true.
c  Define new values
	   ndat(jm)=in	!number of new values
c	dimension ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd)
	   isym(jm)=isym(j)
	   ijoin(jm)=ijoin(j)
	   symsiz(jm)=symsiz(j)
	   icol(jm)=icol(j)
	   ncurvd=1
	   icurvd(1)=jm
	   if(nmod.eq.0) then
		ifitype=4		!display fitted param for exponentials
		if(fixy0) ifitype=-4		!new option
	   endif
	   goto 30		!display new values
	endif
c
	if(nmod.eq.0.and.fitted.and.(.not.resplot)) then	!exponentials
	   if(avact.and.ndimd.eq.2) then
	      call WDIALOG(1,
     &	'Not enough memory to display residuals',12)
		goto 26	!no room for residuals!
	   endif
	   ans='Y'
 	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     &   'Show plot of residuals'
     &	,defolt,11,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'Y') then
		resplot=.true.
		iscal=1
c Put residuals in Xval(i,j) -use j=5 in case nj2>0 and voltage recorded
		j=5
c		if(avact.or.plotqdat) j=2
		if(plotqdat) j=2
		if(avact) j=3	!now j=2 is for totcur -see aver_act
		jc=icurvd(1)	!for data being fitted
		ival=-1		!so YCALCJ uses Xv not X(i)
		n=0			!count the obs in fitted range
		do i=1,ndat(jc)
		   x=xval(i,1)
		   xv=x-xval0	!time for obs value as needed in ycalcj
		   if(xv.gt.0.0) then
			n=n+1
		      yc=YCALCJ(kmax,theta,xv1,ival)
		      Yval(n,j)=Yval(i,1)-yc
			Xval(n,j)=x
		   endif
		enddo
		ncurvd=1
c		ndat(j)=ndat(jc)
		ndat(j)=n
		icurvd(1)=j
	      ijoin(j)=-1		!points not joined
		icol(j)=14
		ncjump=0
c     and 'calc curve'=line through zero
		jcr=4
		if(avact.or.plotqdat) jcr=2
		ncurvc=1
		icurvc(1)=jcr
		icol(10+jcr)=12
		ncalc=5001
	      ndc1=ncalc
	      if(allocated(xcal)) then
	        DEALLOCATE(xcal,ycal)
		endif
		ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
		dxcalc=(xmax0-xmin0)/FLOAT(ncalc-1)
		ncal(jcr)=ncalc
		do i=1,ncalc
		   xcal(i,jcr)=xval(1,jc)+float(i-1)*dxcalc	!do not include t=0
		   ycal(i,jcr)=0.0
		enddo
		ifitype=0		!no display of fitted parameters
		iline(jcr)=0
 	      call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
		goto 30	!Display residuals
	   endif
	endif
26	if(nmod.eq.0.and.resplot.and.fitted) then
	   resplot=.false.
c restore plot of exponential in case refit wanted
	   ifitype=4
	   if(fixy0) ifitype=-4		!new option
	   iscal=1
	   ncurvd=1
	   icurvd(1)=jc
	   ncurvc=3
	   icurvc(1)=1
	   icurvc(2)=2
	   icurvc(3)=3
	   ncjump=1
	endif
c
c Insert option to write ASCII output if only one data curve displayed
	if(ncurvd.eq.1) then
	  ans='N'
 	  call DEFOLTa(ans,defolt)
	  call QDIALOG(1,
     &	'Save this trace as ASCII file'
     &	,defolt,11,cans)
	  call GETINPa(cans,ans)
	  if(UC(ans).eq.'Y') then
c Allocate DATA arrays for ASCIO
	   ndim1=ndat(1)		!dimension of DATA aray
	   ndim2=2			!dimension of DATA aray
	   ALLOCATE(data(ndim1,ndim2))
	   do 92 i=1,ndat(1)
	     data(i,1)=xval(i,1)
	     data(i,2)=yval(i,1)
92	   continue
	   ndata=ndat(1)	!number of lines of data
	   jcol=2	!two entries in each line
	   icolwid=13		!the minimum col width
	   colhead(1)='time'
	   colhead(2)='current'
cc	   call ASCIO(data,ndata,jcol,ndim1,ndim2,.true.,icolwid,colhead)
	   lentext=0		!no text added to ascii file
	   call ASCWRITE(data,ndata,jcol,text,ndim1,ndim2,lentext,
     &    icolwid,colhead,1)
	   DEALLOCATE(data)
	  endif
	endif
c
95	if(fitted) then
	   fitted=.false.
	   ncurvc=0
	   if(vramp) then
 	      call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
		goto 230	!another display?
	   else
	      goto 319			!another fit ?
	   endif
	endif
c
c Now fit it?
	if(refit) then
	   refit=.false.	!restore
	   iask=2		!restore
	   j1=1
	   if(ifit.eq.1) goto 511
	   if(ifit.eq.2) goto 313
	   if(ifit.eq.3) goto 52
	endif
	if((iplot.eq.1.or.iplot.eq.11).and.(.not.vramp)) then
	   ans='Y'
	else
	   ans='N'
	endif
c For huge files the averaged activations can be queued but not fitted
	if(avact.and.ndfit.eq.1) then
	   call WDIALOG(1,
     &  'Huge file -no fitting possible -hit key to end!',12)
 	   call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	   goto 230			!another display?
	endif
c
 	call DEFOLTa(ans,defolt)
	call QDIALOG(1,
     &	'Fit this display'
     &	,defolt,11,cans)
	call GETINPa(cans,ans)
	if(UC(ans).eq.'N') then
 	   call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	   goto 230			!another display?
	endif
	j1=1		!curve # to be fitted
	if(ncurvd.gt.1) then
 	   call DEFOLTi(j1,defolt)
	   call QDIALOG(1,
     &	' -which curve to be fitted (1=green, 2=red)',
     &	defolt,11,cans)
	   call GETINPi(cans,j1)
	endif
c Fit required so now
c (1) Fit peak, baseline and calc risetime if iplot=1
c (2) For any iplot use cursors to mark start and end of region to be fitted
c	with exponentials or I/V
	if(iplot.ne.1.and.iplot.ne.11) goto 52
51	continue
	ilrn=ilrn+1
	if(ilrn.gt.20) ilrn=20
	ifit=lrnfit(ilrn)
501	continue
	TITLEm=' FITTING OPTIONS '
	strings(1)='1. Peak & rise time'
	strings(2)='2. Mean current    '
	strings(3)='3. Time course     '
	strings(4)='4. No fit          '
	nval=4
	helps(1)='To mark first point to fit, either expand'
	helps(2)='display or plot against log(t). If rising'
	helps(3)='phase fitted with y(0)=y(inf), then first'
	helps(4)='point must be on baseline (but can omit'
	helps(5)=' points from t=0 up to specified t, if nec)'
	nhelp=5
	call POPMENU(nxlo,nylo,nyhi,strings,nval,ictm,icfm,icupm,
     &	 ibkm,titlem,helps,nhelp,ilinem,charout,ival)
	ifit=ilinem	!iline=0 for ESC=cancel
c===
	if(ifit.lt.1.or.ifit.gt.4) goto 501
	lrnfit(ilrn)=ifit
	if(ifit.eq.2) goto 313
c	if(ifit.eq.3) goto 52	!get init guesses for ybase etc first
	if(ifit.eq.4) then
 	   call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	   goto 230			!another display?
	endif
c Fit peak and risetime
511	continue
	j=icurvd(j1)
c Calc mean of last 10% of points;
c ? omit last 10 points from mean because of DC/BS expts in which V-jump done
c just before ends, which makes a transient. BUT when looking for min,max
c better to look through only the plotted points, ie for x between xmin,xmax,
c on assumption that display has been scaled to not show transients or other
c unwanted bits
	nmiss=10
	nd=ifixr(0.1*float(ndat(j)))
	s=0.
	do 54 i=ndat(j)-nd+1-nmiss,ndat(j)-nmiss
54	s=s+yval(i,j)
	ybase=s/float(nd)
	ymin2=1.e37
	ymax2=-1.e37
	do 53 i=1,ndat(j)		!loop over all plotted points
	   x=xval(i,j)
	   if(x.lt.xmin0.or.x.gt.xmax0) goto 53	!skip
	   y=yval(i,j)
	   if(y.gt.ymax2) then
		ymax2=y
		imax=i
	   endif
	   if(y.lt.ymin2) then
		ymin2=y
		imin=i
	   endif
53	continue
	if(abs(ymax2-ybase).gt.abs(ybase-ymin2)) then
	   ypeak=ymax2
	   ival=imax
	   xpeak=xval(ival,j)
	else
	   ypeak=ymin2
	   ival=imin
	   xpeak=xval(ival,j)
	endif
c Also calc rough time constant as init guess for fit
	amp=ypeak-ybase
	do 531 i=ival,ndat(j)		!loop over all plotted points
	   y=(yval(i,j)-ybase)/amp
	   itau=i
	   if(y.lt.0.37) goto 532	!jump out
531	continue
532	tau1=xval(itau,j)-xpeak		!guess for tau
c Rough ybase, ypeak now found for init guesses for fit
	if(ifit.eq.3) goto 52
c
	if(xpeak.gt.xmax0.or.xpeak.lt.xmin0) xpeak=0.5*(xmin0+xmax0) !if off scale
	if(ybase.gt.ymax0.or.ybase.lt.ymin0) ybase=0.5*(ymin0+ymax0) !if off scale
c xdelt=2ms before peak initially; learns better value
	call grapix(xpeak-xdelt,ybase,jx,jy)
	if(jx.lt.0) jx=10
 	call WDIALOG(1,
     & 'Mark the baseline, and start of rising phase; hit enter',11)
      idraw=-1
      islope=1
      call crossh(jx,jy,jxlo,jylo,jxhi,jyhi,icol(71),icf,itype,
     &  line,idraw,islope,ax,ay,6,70,6,1.)
	call BELL(1)
      xfst=ax(1)
      ybase=ay(1)
 	if(logx) then
 	   xfst=10**xfst
 	endif
 	if(logy) then
 	   ybase=10**ybase
 	endif
	iltype=3		!short dash
	call broken(iltype)
	call graMOV(xmin,ybase)
	call gralin(xmax,ybase)
	if(ypeak.gt.ymax0.or.ypeak.lt.ymin0) ypeak=0.5*(ymin0+ymax0) !if off scale
	x=xpeak
	y=ypeak
	if(x.lt.xfst) x=xfst+2.5	!(may happen for 2nd jump on same sweep)
 	if(logx) x=alog10(x)
 	if(logy) y=alog10(ypeak)
	call grapix(x,y,jx,jy0)
	call WDIALOG(1,
     &'Mark the peak amplitude, and time for peak; hit enter',11)
      call crossh(jx,jy0,jxlo,jylo,jxhi,jyhi,icol(71),icf,itype,
     &  line,idraw,islope,ax,ay,6,70,6,1.)
	call BELL(1)
      ypeak=ay(1)
      xpeak=ax(1)
 	if(logx) then
 	   xpeak=10**xpeak
 	endif
 	if(logy) then
 	   ypeak=10**ypeak
 	endif
	iltype=3		!short dash
	call broken(iltype)
	call graMOV(xmin,ypeak)
	call gralin(xmax,ypeak)
c calc new average xdelt (exc initial value)
	ndelt=ndelt+1
	if(ndelt.eq.1) then
	   xdelt=xpeak-xfst
	else
	   s=xdelt*float(ndelt-1)	!ndelt already updated
	   s=s + (xpeak-xfst)
	   s=s/float(ndelt)
	endif
	amp=ypeak-ybase
c Get risetime: interpolate between xfst and xpeak
	y20=ybase + 0.2*amp
	y80=ybase + 0.8*amp
	k=0
	if(allocated(ya)) then
	   DEALLOCATE(ya,xa,yout)
	endif
	ALLOCATE(ya(nDV1),xa(ndv1),yout(ndv1))
	do 57 i=1,ndat(j)
	   x=xval(i,j)
	   if(x.lt.xfst) goto 57
	   if(x.gt.xpeak) goto 570	!finish
	   k=k+1
	   xa(k)=xval(i,j)
	   ya(k)=yval(i,j)
57	continue
570	nfit=k
	yp1=0.	!1st deriv at ends
	ypn=0.
	call SPLINSUB(Xa,Ya,nfit,yp1,ypn,vstep1,yout,
     & v1,v2,nval,2,y20,y80,x20,x80,nerr,ndv1)
 	if(logx) then
 	   x=alog10(x20)	!mark 20/80 points
 	else
 	   x=(x20)
 	endif
 	if(logy) then
 	   y=alog10(y20)	!mark 20/80 points
 	else
 	   y=y20
 	endif
	call grapix(x,y,ix,iy)
	call hline(ix-5,ix+5,iy,icol)
	call vline(ix,iy-5,iy+5,icol)
 	if(logx) then
 	   x=(alog10(x80))	!mark 20/80 points
 	else
 	   x=(x80)
 	endif
 	if(logy) then
 	   y=(alog10(y80))	!mark 20/80 points
 	else
 	   y=(y80)
 	endif
	call grapix(x2,y,ix,iy)
	call hline(ix-5,ix+5,iy,icol)
	call vline(ix,iy-5,iy+5,icol)
	if(nerr.ne.0) then
	   call BELL(3)
	   call INTCONV(nerr,cnum)
	   call DCFORMAT(amp,8,2,cnum1)
	   call DCFORMAT(x80-x20,8,2,cnum2)
	   call WDIALOG(1,
     &	'Error in interpolation: nerr = '//charnb(cnum),12)
	   ans='N'
 	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     &    'Amp (pA) = '//charnb(cnum1)//' Risetime (20-80%) = '//
     &     charnb(cnum2)//' ms: O.K.',
     &     defolt,12,cans)
 	   call GETINPa(cans,ans)
	   if(UC(ans).eq.'Y') then
		if(pprt) write(7,59) xfst,xpeak,xpeak-xfst,
     &		ybase,ypeak,amp,x80-x20
		if(discprt) write(8,59) xfst,xpeak,xpeak-xfst,
     &		ybase,ypeak,amp,x80-x20
	   else
		ifit=1		!try again?
		TITLEm=' FITTING OPTIONS '
		strings(1)='1. Peak & rise time'
		strings(2)='2. Mean current    '
		strings(3)='3. Time course     '
		strings(4)='4. No fit          '
		nval=4
		helps(1)='To mark first point to fit'
		helps(2)=' either expand display, or'
		helps(3)=' plot against log(t)'
		nhelp=3
5011		call POPMENU(nxlo,nylo,nyhi,strings,nval,ictm,icfm,icupm,
     &	 ibkm,titlem,helps,nhelp,ilinem,charout,ival)
		ifit=ilinem	!iline=0 for ESC=cancel
		if(ifit.lt.1.or.ifit.gt.4) goto 5011
		goto 582	!skip learn!
	   endif
	else
	   call REALTOCH(amp,cnum,11)
	   call REALTOCH(x80-x20,cnum1,11)
	   ans='Y'
 	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     &   'Amp (pA) = '//charnb(cnum)//' Risetime (20-80%) (ms) = '//
     &    charnb(cnum1)//' O.K.',
     &    defolt,11,cans)
	   call GETINPa(cans,ans)
	   if(UC(ans).ne.'N') then
		if(pprt) write(7,59) xfst,xpeak,xpeak-xfst,
     &		ybase,ypeak,amp,x80-x20
		if(discprt) write(8,59) xfst,xpeak,xpeak-xfst,
     &		ybase,ypeak,amp,x80-x20
	   else
		ifit=1		!try again?
 		call WDIALOG(1,
     &    'Fit (1) peak & rise; (2) mean current; (3) time course;',11)
	 	call DEFOLTi(ifit,defolt)
		call QDIALOG(1,
     &		'(4) none: Option #',
     &     		defolt,11,cans)
		call GETINPi(cans,ifit)
		goto 582	!skip learn!
	   endif
	endif
59	format(/,' Time at start, peak = ',2g13.6,' time diff = ',g13.6,/,
     & ' Baseline = ',g13.6,' peak = ',g13.6,/,
     & ' Amplitude (pA) = ',g13.6,' Risetime (20-80%) (ms) = ',g13.6)
c	ifit=2 			!new default -now learned
319	continue		!return here after ifit=2 for next
	ilrn=ilrn+1
	if(ilrn.gt.20) ilrn=20
	ifit=lrnfit(ilrn)
	ilinem=ifit
	TITLEm=' FITTING OPTIONS '
	strings(1)='1. Peak & rise time'
	strings(2)='2. Mean current    '
	strings(3)='3. Time course     '
	strings(4)='4. No fit          '
	nval=4
	helps(1)='To mark first point to fit'
	helps(2)='either expand display, or'
	helps(3)='plot against log(t)'
	nhelp=3
	call POPMENU(nxlo,nylo,nyhi,strings,nval,ictm,icfm,icupm,
     &	 ibkm,titlem,helps,nhelp,ilinem,charout,ival)
	ifit=ilinem		!iline=0 for ESC=cancel
	lrnfit(ilrn)=ifit
582	continue
	if(ifit.eq.4) then
 	   call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	   refit=.false.
	   goto 230			!another display?
	endif
	itit=0
	refit=.true.
	ans='N'
	if(rescale) ans='Y'
 	call DEFOLTa(ans,defolt)
	call QDIALOG(1,
     & 'Rescale display before fitting'
     &	,defolt,11,cans)
	call GETINPa(cans,ans1)
c Change default only if ans='y' and ans1='n', or vice versa
	if(ans.eq.'Y'.and.ans1.eq.'N'.or.ans.eq.'N'.and.ans1.eq.'Y')
     &	rescale=.not.rescale
	if(.not.rescale) then
	   iask=3	!come straight out of VPLOTR; leave graph on screen
	   if(nmod.eq.0) then
		ifitype=4		!display fitted param for exponentials
		if(fixy0) ifitype=-4		!new option
	   endif
	   goto 30    !redraw, same scale as last display
	endif
	if(iplot.eq.11) goto 85
	goto(80,100,90,90,90,90) iplot	!recalc orig display
c
c FIT CURRENT ie average specified section of baseline and calc average
c current
313	continue
	ix1=320
	j=icurvd(j1)
	call WDIALOG(1,
     &  'Mark X value at START of section to be averaged',12)
	call BELL(1)
	idraw=-1
	line=0
	islope=0
      call crossh(ix1,jy0,jxlo,jylo,jxhi,jyhi,icol(71),icf,itype,
     &  line,idraw,islope,ax,ay,6,70,6,1.)
      x1=ax(1)
	iltype=3		!short dash
	call broken(iltype)
c	if(logx) x1=10**x1
	call graMOV(x1,ymin)
	call gralin(x1,ymax)
	call WDIALOG(1,
     &  'Mark X value at END of section to be averaged',12)
	ix2=ix1+60
	if(ix2.gt.638) ix2=638
c	if(logx) ix2=alog10(float(ix2))
      call BELL(1)
      idraw=-1
      line=0
      call crossh(ix2,jy0,jxlo,jylo,jxhi,jyhi,icol(71),icf,itype,
     &  line,idraw,islope,ax,ay,6,70,6,1.)
      x2=ax(1)
c	if(x2.le.x1) goto 313
	iltype=3		!short dash
	call graMOV(x2,ymin)
	call gralin(x2,ymax)
c	if(logx) x2=10.0**x2			!take antilog here
	s=0.0
	n=0
	do 316 i=1,ndat(1)	!loop over all plotted points
	   x=xval(i,j)
	   if(x.lt.x1.or.x.gt.x2) goto 316			!omit from mean
	   n=n+1
	   s=s+yval(i,j)
316	continue
	if(n.lt.1) then
	   call BELL(4)
	   call INTCONV(n,cnum)
	   ans='Y'
 	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     &   'ERROR: n = '//charnb(cnum)//': try again',
     &    defolt,11,cans)
	   call GETINPa(cans,ans)
	   if(UC(ans).ne.'N') goto 313
 	   call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	   goto 230			!another display?
	else
	   s=s/float(n)	!mean current
	   iltype=0		!continuous
	   call broken(iltype)
	   call graMOV(x1,s)
	   call gralin(x2,s)
	   call DCFORMAT(x1,8,2,cnum)
	   call DCFORMAT(x2,8,2,cnum1)
	   call DCFORMAT(s,8,2,cnum2)
	   ans='Y'
 	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     &    'Mean current from X = '//charnb(cnum)//' to = '//
     &     charnb(cnum1)//' ms = '//charnb(cnum2)//': O.K.',
     &     defolt,12,cans)
 	   call GETINPa(cans,ans)
	   if(UC(ans).eq.'N') then
 		call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
		goto 230			!another display?
	   endif
	   if(pprt) write(7,318) s,x2-x1,x1,x2
	   if(discprt) write(8,318) s,x2-x1,x1,x2
318	   format(/,
     & ' Mean current = ',g13.6,/,
     & ' over time interval = ',g13.6,' from X = ',g13.6,' to ',g13.6)
	endif
	ans='N'
 	call DEFOLTa(ans,defolt)
	call QDIALOG(1,'Measure another mean current',
     &  defolt,12,cans)
 	call GETINPa(cans,ans)
	if(UC(ans).eq.'Y') goto 313
c	ifit=3	!next default -now learned
	goto 319	!next fit
c
c FIT TIME COURSE
c Lines between points- 0=solid; 1=dotted;',/,
c 2-6=inc length dashes; 7=dash-dot; 8=dash-dot-dot.',/,
52	continue
	ans='N'
 	call DEFOLTa(ans,defolt)
	call QDIALOG(1,
     & 'Omit a section of points from the fit'
     &	,defolt,11,cans)
	call GETINPa(cans,ans)
	censor=ans.eq.'Y'
	j=icurvd(j1)	!j1=index for curve to be fitted
	idest=3	!to do fit next
	call WDIALOG(1,
     &  'Mark first X value be FITTED with cursor; hit enter',12)
	call BELL(1)
	idraw=-1
	islope=0
      line=0
      call crossh(jx,jy0,jxlo,jylo,jxhi,jyhi,icol(71),icf,itype,
     &  line,idraw,islope,ax,ay,6,70,6,1.)
      x1=ax(1)
	if(x1.lt.xmin) x1=xmin		!don't allow fit of data off screen
	if(vramp) then
c  Also check against smallest x value
	   if(x1.lt.vmin) x1=vmin  !keep within data range
	endif
	if(expand) then
	  iltype=3		!short dash
	  call broken(iltype)
	  call graMOV(x1,ymin)
	  call gralin(x1,ymax)
	endif
c Insert option to rescale before marking the last point to fit
c and/or option to specify last point as number?  After doing this
c jump straight back to next line (label 86) (when redisp=.true.). Best go
c back and redo whole original plot, since will probably want all
c points on display for marking the last point to fit. If display was
c against log(x) then should not need to redisplay to define the last point
c to be fitted so skip this option.
	redisp=.false.
	ans='Y'
	if(expand.and.(.not.logx)) then		!display has been expanded
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     & 'Restore display before marking last point to fit',
     &  defolt,11,cans)
	   call GETINPa(cans,ans)
c    IASK=3 to draw graph and leave at once (no boxes; no chance to alter)
	   if(ans.eq.'Y') then
		if(censor) then
		   ans='Y'
		   call DEFOLTa(ans,defolt)
		   call QDIALOG(1,
     & 'Mark the points to be OMITTED before restoring display',
     &  defolt,11,cans)
		   call GETINPa(cans,ans)
		   if(ans.eq.'Y') then
			call GETOMIT(x11,x21,nomit,
     &		 x1,ymin,ymax,logx,pprt,idest1)
		      censor=.false.		!so does not ask again
		      if(idest1.eq.230) goto 230
		   endif
		endif
		iask=3
		iscal=4 		!use input xmin,xmax,ymin,ymax only
c		iscal=0	!use input values of xmin etc
		ilog=0
		xmin=xminsav
		xmax=xmaxsav
		ymin=yminsav
		ymax=ymaxsav
		xcross=xmin
		ycross=ymin
	      redisp=.true.
		if(nmod.eq.0) then
		   ifitype=4		!display fitted param for exponentials
		   if(fixy0) ifitype=-4		!new option
		endif
		goto 30
	   endif
	endif
c
86	continue		!jump back here after rescale for last point
	iltype=3		!short dash
	redisp=.false.
	call broken(iltype)
	call graMOV(x1,ymin)
	call gralin(x1,ymax)
	if(logx) x1=10.**x1
	jx=jx+60
	call WDIALOG(1,
     &  'Mark last X value be FITTED with cursor; hit enter',12)
      idraw=-1
      line=0
      call crossh(jx,jy0,jxlo,jylo,jxhi,jyhi,icol(71),icf,itype,
     &  line,idraw,islope,ax,ay,6,70,6,1.)
	call BELL(1)
      x2=ax(1)
	if(x2.gt.xmax) x2=xmax		!don't allow fit of data off screen
	if(vramp) then
c  Also check against largest x value
	   if(x2.gt.vmax) x2=vmax  !keep within data range
	endif
	call graMOV(x2,ymin)
	call gralin(x2,ymax)
	if(logx) x2=10.0**x2			!take antilog here
	call DCFORMAT(x1,8,1,cnum)
	call DCFORMAT(x2,8,1,cnum1)
	ans='Y'
 	call DEFOLTa(ans,defolt)
	call QDIALOG(1,
     & 'Fit from X = '//charnb(cnum)//' to '//charnb(cnum1)//' O.K.'
     &	,defolt,11,cans)
	call GETINPa(cans,ans)
	if(UC(ans).eq.'N') then
 	   call DEFOLT2r(x1,x2,defolt)
	   call QDIALOG(1,
     &	'First and last X value to fit',
     &	   defolt,11,cans)
	   call GETINP2r(cans,x1,x2)
c 	   call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
c	   goto 230			!another display?
	endif
	if(pprt) write(7,3031) x1,x2
	if(discprt) write(8,3031) x1,x2
3031	format(/,
     & ' Fitted from X = ',g13.6,' to ',g13.6)
c Omit a section?
	nomit=0
	if(censor) then
	   call GETOMIT(x11,x21,nomit,
     &		x1,ymin,ymax,logx,pprt,idest1)
	   censor=.false.		!so does not ask again
	   if(idest1.eq.230) goto 230
	endif
c
c	pause		!for now
	goto 312
c
c Construct special 1-dimensional Yfit,Xfit arrays for fitting that contain
c only those points that are to be fitted
c Modified 04/17/91 02:06pm so that t=0 at first point to be fitted (so no
c longer any need to shift time origin before fitting!)
312	continue
	j=icurvd(j1)	!j1=index for curve to be fitted
	n=0
	if(vramp.and.iplot.ge.3.and.iplot.le.6) then		!I-V plot
c redefine vmin,vmax as min/max fitted value
	   vmin=1.e37  !min/max voltage (values not in ascending order!)
	   vmax=-1.e37
	   do 31 i=1,ndat(j)	!loop over all plotted points
		x=xval(i,j)
		if(x.lt.x1.or.x.gt.x2) goto 31			!omit from fit
		if(nomit.eq.1) then
		   if(x.ge.x11.and.x.le.x21) goto 31		!omit from fit
		endif
		n=n+1
		xfit(n)=x
		yfit(n)=yval(i,j)
		if(x.lt.vmin) vmin=x
		if(x.gt.vmax) vmax=x
31	   continue
	else		!not I-V plot
	   xval0=0.0
	   def0=.true.
	   do 310 i=1,ndat(j)	!loop over all plotted points
		x=xval(i,j)
		if(x.lt.x1.or.x.gt.x2) goto 310			!omit from fit
		if(nomit.eq.1) then
		   if(x.ge.x11.and.x.le.x21) goto 310		!omit from fit
		endif
		if(def0) xval0=x		!define time origin for fit
		def0=.false.
		n=n+1
		xfit(n)=x-xval0		!=0 for first point
		yfit(n)=yval(i,j)
		xvaln=x		!on exit this is last point fitted
310	   continue
	endif
	nfit=n
c Problem with commons (see Modif notes above)
	do i=1,10
	  t1c1(i)=t1c(i)
	  t2c1(i)=t2c(i)
	  t1v1(i)=t1v(i)
	  t2v1(i)=t2v(i)
	enddo
c Ensure that only the fitted data are plotted on return to 10 after fitting
c (for Vramp only?)
	if(vramp) then
	   ncurvd=1
	   icurvd(1)=j
	   if(j1.eq.1) then
		icol(j)=10		!bright green for control
		icol(j+10)=10	!for calc curve
	   else if(j1.eq.2) then
		icol(j)=12		!bright red for drug
		icol(j+10)=12	!for calc curve
	   endif
	endif
c
	if(allocated(xcal)) then
	   DEALLOCATE(xcal,ycal)
	endif
	if(allocated(ya)) then
	   DEALLOCATE(ya,xa,yout)
	endif
	if(allocated(weight)) then
	   DEALLOCATE(weight,icurvw)
	endif
c Next bit added 06/21/97 08:32pm for re-display of same data in main prog
	xminsav=xmin		!save for re-display for refit
	xmaxsav=xmax
	yminsav=ymin
	ymaxsav=ymax
	RETURN
c return with idest=3 to fit (stay in graphics until initial guesses done).
c
230	idest=1
c NB when nj2=0 and not ramp (not sampv) there is only one sort of plot
c viz I(t) vs t, so return for next data straight away
	if(nj2.eq.0.and.(.not.sampv)) goto 999	!return
	if(nj2.eq.0) then
	   iscal=0
	   goto 60
	endif
	print 61
61	format('&Same scales for display [N] ? ')
	read 101,ans
	if(UC(ans).eq.'Y') then
	   iscal=0
	   xmin=xminsav
	   xmax=xmaxsav
	   ymin=yminsav
	   ymax=ymaxsav
	   xcross=xmin
	   ycross=ymin
	   if(nmod.eq.0) then
		ifitype=4		!display fitted param for exponentials
		if(fixy0) ifitype=-4		!new option
	   endif
	   goto 30
	endif
	iplot=iplot+1
	if(iplot.eq.5) iplot=7		!skip 5,6
c	if(iplot.gt.4) iplot=1		!next default
	fitted=.false.
	goto 60			!next display
c
c
c NOW BIT DONE AFTER FIT
c Modif so that time origin is at first point fitted ie t=xval0 on current
c Xval() scale, so must shift X axis by this amount before calculating
c values.  Display line only over points actually fitted? Or show as dashed
c lines for rest of range (needs 3 sep calc curves! -or 2 anyway) If calc over
c whole range will probably get overflow when exponential is extrapolated
c a long way back which also makes this hard. Better plot from point where fit
c was started up to end of display (=xmax)
10	continue
c
c	if(debug()) pause 'after label 10 in CJVDU'
c Data sets; as last display
c
c Calc curves:
c Plot the one calc curve in two parts, with Ycal(i,2) being part above last
c fitted point=dashed line
	ncalc=5001 		!number of points for calc curves
	      ndc1=ncalc
	      if(allocated(xcal)) then
	        DEALLOCATE(xcal,ycal)
		endif
		ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
	if(vramp) then
c recalc largest and smallest xfit() value (they are not in ascending order!)
	   dxcalc=(vmax-vmin)/FLOAT(ncalc-1)     !fitted range only
	   xval0=0.0	!so xcal() OK below
	   def0=.false.		!flag for when 1st set finished
	   xvaln=xmax2
	else
	   dxcalc=(xmax0-xval0)/FLOAT(ncalc-1)         !whole range of curve
	   def0=.true.		!flag for when 1st set finished
	endif
c   Add dashed line at level of asymptote
	ncurvc=3		!for time fits
	icurvc(1)=1
	icurvc(2)=2
	icurvc(3)=3
	icol(1)=10
	icol(2)=12
	icol(11)=12
	icol(12)=12
	icol(13)=14
	iline(1)=0		!continuous line
	iline(2)=2		!short dash line
	iline(3)=1		!dotted line
	iscal=0   		!so previous xmin etc used
	ndimc=5
	ndc1=ncalc
c For exponential fit, use ncalc
c Modify so that, for exponentials, have ncalc points per time constant,
c extending up to 10*tau for each, or up to xmax0 if this later than 10*slowest
c time constant.  Need first to calculate dimension for ycal().  Have ncalc
c points per 10*tau for all components but last
c NB time constants must be in ascending order -they are now sorted in CJOUT
	if(nmod.eq.0) then
	   ncalc=300
	      ndc1=ncalc
	      if(allocated(xcal)) then
	        DEALLOCATE(xcal,ycal)
		endif
		ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
	   taufac=15.
	   enc=float(ncalc)
	   do k=1,ncomp
		tau(k)=theta(2*k-1)	!theta(2*i-1) tau=theta 1,3,5,7,9
		dxc(k)=taufac*tau(k)/enc
	   enddo
	   t=0.0
	   do k=1,ncomp-1
		t=t+tau(k)
	   enddo
	   t=xval0 + taufac*t		!time for last point for next to slowest tau
	   tend=xmax0-t
	   nclast=tend/dxc(ncomp)	!number of points for slowest time const
	   ndc1=ncalc*(ncomp-1)+nclast+1
	endif
c
	ndc1=ncalc
	if(allocated(xcal)) then
	   DEALLOCATE(xcal,ycal)
	endif
	ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
c	if(.not.allocated(xcal)) then
c	   ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
c	endif
c
	j=1
	i1=0
	deb=debug()
	ival=-1		!so YCALCJ uses Xv not X(i)
	if(vramp) then		!for I-V curve
	   ncurvc=1
	   icol(11)=14		!calc curve yellow if only one
	   dxcalc=(vmax-vmin)/FLOAT(ncalc-1)     !fitted range only
	   ncal(1)=ncalc
	   do i=1,ncalc
		xcal(i,j)=vmin+float(i-1)*dxcalc
		Xv=xcal(i,j)	!value of X (in COMMON) for YCALCJ
		Ycal(i,j)=YCALCJ(kmax,theta,xv1,ival)
	   enddo
	else				!all other curve types
	   if(nmod.eq.0) then	!exponentials
		ans='Y'
		call DCASK('Subtract asymptote before display',ans,ans)
		subasym=ans.eq.'Y'
		if(subasym) then		!subtract asymptote from data too!
		   j=icurvd(1)
		   do i=1,ndat(j)
			Yval(i,j)=Yval(i,j) - theta(kmax)
		   enddo
		   subtracted=.true.	!so offset restored after display
c Also fix ymin, ymax
		   xminsav=xmin		!save for re-display for refit
		   xmaxsav=xmax
		   yminsav=ymin
		   ymaxsav=ymax
	   	   if(logy) then
			ymin0=10.0**ymin	!non-log value for calc curves
			ymax0=10.0**ymax	!non-log value for calc curves
		   else
			ymin0=ymin		!non-log value for calc curves
			ymax0=ymax		!non-log value for calc curves
		   endif
		   ymin0=ymin0 - theta(kmax)
		   ymax0=ymax0 - theta(kmax)
		   ymin2=ymin0
		   ymax2=ymax0
		   if(logy) then
			ymin2=alog10(ymin1)
			ymax2=alog10(ymax1)
		   endif
		   call FIXAX(ymin2,ymax2,ymin,ymax,ytic,ilog)
		   ycross=ymin
		endif
	   endif
c
c Modify so that, for exponentials, have ncalc points per time constant.
c First calculate how many values are needed for slowest time constant. This
c will be >ncalc if xmax0 is below 5*slowest tau, but greater than ncalc
c if xmax0 is bigger.
	   if(nmod.eq.0) then
		i1=1
		i2=1
		xcal(i1,j)=xval0
		xcal(i1,1)=xval0
		xv=0.0	!=xcal(1,j)-xcal0 -value in common
		if(subasym) then
		   Ycal(i1,j)=YCALCJ(kmax,theta,xv1,ival)-theta(kmax)
		else
		   Ycal(i1,j)=YCALCJ(kmax,theta,xv1,ival)
		endif
		x0=xval0
		enc=float(ncalc)
		nc1=ncalc 		!for all but slowest tau
		do k=1,ncomp
		   if(k.eq.ncomp) nc1=nclast	!for slowest tau
		   do m=1,nc1
			x=x0+float(m)*dxc(k)
			if((x.gt.xvaln).and.def0) then	!start 2nd cal curve now
			   def0=.false.	!flag for when 1st set finished
			   ncal(1)=i1	!number in 1st part
			   j=2
			   i1=0           !start 2nd xcal(),Ycal()
			endif
			i1=i1+1
			i2=i2+1
			xcal(i1,j)=x
			xcal(i2,1)=x	!keep in xcal(i,1) too, for residual plot
			Xv=x-xval0		!value of X (in COMMON) for YCALCJ -starts at 0
			if(xv.lt.0.0) then
			   xv=0.0
			   xcal(i1,j)=xval0
			   xcal(i2,1)=xval0
			endif
			if(subasym) then
			   Ycal(i1,j)=YCALCJ(kmax,theta,xv1,ival)-theta(kmax)
			else
			   Ycal(i1,j)=YCALCJ(kmax,theta,xv1,ival)
			endif
		   enddo
		   x0=xcal(i1,1)	!ready for next tau
		enddo
	   else 	!not exponentials
	     do i=1,ncalc
		x=xval0+float(i-1)*dxcalc	!actual time as in current Xval()
		if((x.gt.xvaln).and.def0) then	!start 2nd cal curve now
		   def0=.false.	!flag for when 1st set finished
		   ncal(1)=i1	!number in 1st part
		   j=2
		   i1=0           !start 2nd xcal(),Ycal()
		endif
		i1=i1+1
		xcal(i1,j)=x
		xcal(i,1)=x	!keep in xcal(i,1) too, for residual plot
		Xv=x-xval0		!value of X (in COMMON) for YCALCJ -starts at 0
		if(subasym) then
		   Ycal(i1,j)=YCALCJ(kmax,theta,xv1,ival)-theta(kmax)
		else
		   Ycal(i1,j)=YCALCJ(kmax,theta,xv1,ival)
		endif
c
		if(deb) then
		  print 201,i,i1,j,xv,xcal(i1,j),ycal(i1,j)
201		  format(' i,i1,j,xv,xc(i1,j),yc(i1,j)= ',3i5,3g13.6)
		endif
	     enddo	!end of i loop
	   endif
	   if(def0) then		!x never reached xvaln, so no 2nd curve
		ncal(1)=i1		!number in 1st part
		ncurvc=2
		icurvc(2)=3		!line for asymptote is now curve #2
	   else
	      ncal(2)=i1
	   endif
c
c  Calc values for asymptote line separately, so they go from first point
c (not t=0 as may plot vs log(t)) to end, not just over fit range
	   ncal(3)=ncalc
	   dx=(xmax0-xmin0)/FLOAT(ncalc-1)         !whole range of curve
	   do i=1,ncalc
		xcal(i,3)=xval(2,1) + float(i-1)*dx   !xval(2,1)=first non-zero time
		if(subasym) then
		   ycal(i,3)=0.0		!asymptote
		else
		   ycal(i,3)=theta(kmax)		!asymptote
		endif
	   enddo
c
	endif
c
	if(nmod.eq.0) then
	   ifitype=4		!display fitted param for exponentials
	   if(fixy0) ifitype=-4		!new option
	endif
c Now get chance to fit another after fitting esp, so leave graph up?
c	iask=-2	!do not ask before leaving display; erase graph
	iask=2	!do not ask before leaving display; leave graph on screen
	goto 30		!plot it
c
999	continue
	idest=0
	if(fitted.or.dcurve) then
	   fitted=.false.
	   dcurve=.false.
	   idest=1
	else
 	   call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	   print 320
320	   format(' Print a note on the printout [N] ? ')
	   read 101,ans
	   if(UC(ans).eq.'Y') then
		call TITENT0('Type in the note:',note,79,.true.)
		if(pprt) write(7,4011) note
		if(discprt) write(8,4011) note
4011		format(' =============================================',/,
     &  	 1x,a79,/,
     &  	 ' =============================================')
	   endif
	endif
c Problem with commons (see Modif notes above)
	do i=1,10
	  t1c1(i)=t1c(i)
	  t2c1(i)=t2c(i)
	  t1v1(i)=t1v(i)
	  t2v1(i)=t2v(i)
	enddo
c
	if(allocated(xcal)) then
	   DEALLOCATE(xcal,ycal)
	endif
	if(allocated(ya)) then
	   DEALLOCATE(ya,xa,yout)
	endif
	if(allocated(weight)) then
	   DEALLOCATE(weight,icurvw)
	endif
c
c Next bit added 06/21/97 08:32pm for re-display of same data in main prog
	xminsav=xmin		!save for re-display for refit
	xmaxsav=xmax
	yminsav=ymin
	ymaxsav=ymax
	RETURN
c
	end

