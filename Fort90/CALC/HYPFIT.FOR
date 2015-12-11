	program HYPFIT
c To simulate fitting of Hill-Langmuir hyperbola as in LOB
c===To be added -Lik intervals for all three param of Hill fits!
c=== (still linear in Ymax!)
c=== also add option to omit some calcs for speed (esp LI for Hill fit!)
	real c(100),ptrue(100),w(100),sd(100),x1(100),y1(100),yobs(100)
	allocatable aK,Ym,hill,sdK,sdY,aKlo,aKhi,Ylo,Yhi,cor,Sres,Sresh
	allocatable sdKe,sdYe,core
	allocatable aKlo2,aKhi2,Ylo2,Yhi2		!for m=2 limits
	real*4 aK(:,:),Ym(:,:),hill(:),Sres(:),Sresh(:)
	real*4 sdK(:),sdY(:),aKlo(:),aKhi(:),Ylo(:),Yhi(:),cor(:)
	real*4 sdKe(:),sdYe(:),core(:)
	real*4 aKlo2(:),aKhi2(:),Ylo2(:),Yhi2(:)	!for m=2 limits
	allocatable widlK1,widlK2,asymK1,asymK2
	allocatable widlY1,widlY2,asymY1,asymY2
	real*4 widlK1(:),widlK2(:),asymK1(:),asymK2(:)
	real*4 widlY1(:),widlY2(:),asymY1(:),asymY2(:)
c  for Hill fit errors
	allocatable sdKh,sdYh,sdnh,corYK,corYn,corKn
	allocatable sdKhe,sdYhe,sdnhe,corYKe,corYne,corKne
	real*4 sdKh(:),sdYh(:),sdnh(:),corYK(:),corYn(:),corKn(:)
	real*4 sdKhe(:),sdYhe(:),sdnhe(:),corYKe(:),corYne(:),corKne(:)
c  for varv3
	allocatable yval1,ibad
	real*4 yval1(:)
	integer*1 ibad(:)
c
	logical excKs,excKm,excYs,excYm
	allocatable aKexcb,aKexcs,aKexcm,aYexcb,aYexcs,aYexcm
	real*4 aKexcb(:,:),aKexcs(:,:),aKexcm(:,:),
     & aYexcb(:,:),aYexcs(:,:),aYexcm(:,:)
	common/rand/ix,iy,iz
	common/data1/n,c,yobs,w		!for SSDHYP, HYPERR
	common/data2/ Scrit,Scrit1,Scrit2		!for hyplik/funclo/hi
	common/run/irun
c For simplex4
	logical abort
	real theta(20)
	real*4 lowval(20),highval(20),pertval(20)		!parameter
	integer jfix(20)
	common/abt/abort,ikey
c
	logical pon,slock,debug,caplock,test,deb,good,good1
	logical sdata,useres
	character*11 cdate,ctime
	character*1 ans,UC,dans,ans2
c
	logical discprt
	common/dp/discprt
	character*40 mtitle*40,filnam*32,prtport*4      !for WINPRINT
	common/dpp/filnam,prtport,ndisc,jcol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	EXTERNAL ssdhyp,ssdhill
C
c define function
	pon()=slock()
	debug()=caplock()
c
	filnam='HYPFIT.PRT'
	call WINPRINT   !print file control
      OPEN(unit=7,file=prtport,iostat=nerr)             !open printer
	print 1001
	if(pon()) write(7,1001)
	if(discprt) write(8,1001)
1001	FORMAT(' Program to simulate fit of hyperbola',/)
	call DATE1(cdate)               !DC subroutine
	call TIME(ctime)
	print 1002,cdate,ctime(1:8),mtitle
	if(pon()) write(7,1002) cdate,ctime(1:8),mtitle
	if(discprt) write(8,1002) cdate,ctime(1:8),mtitle
1002	   format(' Date of analysis: ',a11,/,' Time of analysis: ',a8,/,
     & '   Machine = ',a40)
	print 1003
1003	format(
     & ' SCROLL LOCK on for printing'/
     & ' CAPS LOCK on for debugging')
c
101	format(a1)
	dans='n'
c
	maxfac=20.	!exclude estimates > maxfac*true value
c
	test=.false.
	print 40
40	format(
     & ' Use test data (LOB example) [N] ? ')
	call INPUTa(ans)
	if(ans.eq.'Y') then
	   aKtrue=15.
	   Ymtrue=30.
	   hilltrue=1.	!fixed for fit
	   sres1=1.20027
	   n=5
	   do i=1,n
		sd(i)=sres1
		w(i)=1./sres1**2
	   enddo
	   iwt=0
	   c(1)=2.5
         yobs(1)=5.576
	   c(2)=5.
	   yobs(2)=7.282
	   c(3)=10.
	   yobs(3)=12.521
	   c(4)=20.
	   yobs(4)=16.138
	   c(5)=40.
	   yobs(5)=23.219
	   test=.true.
	   nrun=1
	   do i=1,n
		ptrue(i)=Ymtrue*c(i)/(c(i)+aKtrue)
	   enddo
c	   tval=ttable(n-2)
	   call TVALUE(n-2,tval)	!get 95% value of t
	   iwt=1
	   useres=.false.
	   aKmax=maxfac*aKtrue
	   Ymmax=maxfac*Ymtrue
	   hillmax=maxfac*hilltrue
	   print 41
	   if(pon()) write(7,41)
	   if(discprt) write(8,41)
41	   format(
     &	' Test data from LOB.',/,
     &	'   w(i)=Sres for hyperbola fit; w(i)=1.0 for Hill fit')
	   goto 10
	endif
c
c Define true values
	aKtrue=1.
	Ymtrue=1.
	hilltrue=1.
c Define max values so estimates greater than this (e.g very high Ymax from
c crazy expt) cause error estimation to be skipped.
	aKmax=maxfac*aKtrue
	Ymmax=maxfac*Ymtrue
	hillmax=maxfac*hilltrue
c Define X values=conc
c Space them logarithmically between cmin and cmax
	n=5
	print 1,n
1	format(' Number of concentrations [',i2,'] = ')
	call INPUTi(n)
	print 2
2	format(' Smallest normalised conc = ')
	call INPUTr(cmin)
	print 3
3	format('&Largest normalised conc = ')
	call INPUTr(cmax)
	dl=(alog10(cmax)-alog10(cmin))/float(n-1)
	do i=1,n
	   c(i)=10**(alog10(cmin)+float(i-1)*dl)
	   ptrue(i)=Ymtrue*c(i)/(c(i)+aKtrue)
	enddo
	print 12,aktrue,Ymtrue
	if(pon()) write(7,12) aktrue,Ymtrue
	if(discprt) write(8,12) aktrue,Ymtrue
12	format(' True K = ',g13.6,' True Ymax = ',g13.6)
	print 731
	if(pon()) write(7,731)
	if(discprt) write(8,731)
731	format(/,' True values ',/,'       x               y')
	do i=1,n
	   print 741,c(i),ptrue(i)
	   if(pon()) write(7,741) c(i),ptrue(i)
	   if(discprt) write(8,741) c(i),ptrue(i)
741	   format(g13.6,2x,g13.6)
	enddo
	print 301
	if(pon()) write(7,301)
	if(discprt) write(8,301)
c301	format(/)
c
c NB sd(i) are used only for simulating data, w(i) are used only for analysis
	iwt=1
	sdata=.true.
	useres=.false.
	print 51
	if(pon()) write(7,51)
	if(discprt) write(8,51)
51	format(
     & ' Error structure for data:')
	print 5
5	format(
     & ' (1) Constant SD (as specified)',/,
     & ' (2) SD = a + b*Ytrue',/,
     & ' (3) as (2) but weight from Yobs',/,
     & ' Option number [1] = ')
	call INPUTi(iwt)
c
52	continue
	if(iwt.eq.1) then
	   print 6
6	   format('& SD = ')
	   call INPUTr(sd1)
	   if(sdata) then		!do not redefine sd(i) if incorrect weights used
		do i=1,n
		   sd(i)=sd1
		enddo
	   endif
	   do i=1,n
		w(i)=1.0/(sd(i)*sd(i))
	   enddo
	   print 13,sd1,w(1)
	   if(pon()) write(7,13) sd1,w(1)
	   if(discprt) write(8,13) sd1,w(1)
13	   format(' Constant SD = ',g13.6,' weight = ',g13.6)
	else if(iwt.eq.2) then
	   print 7
7	   format(' a, b = ')
	   call INPUT2r(aval,bval)
	   do i=1,n
		if(sdata) sd(i)=aval+bval*ptrue(i)
		w(i)=1.0/(sd(i)*sd(i))
	   enddo
	   print 14,aval,bval
	   if(pon()) write(7,14) aval,bval
	   if(discprt) write(8,14) aval,bval
14	   format(' SD = a + b*Ytrue: a = ',g13.6,' b = ',g13.6)
	else if(iwt.eq.3) then
	   print 7
c7	   format(' a, b = ')
	   call INPUT2r(aval,bval)
	   do i=1,n
		sd(i)=aval+bval*ptrue(i)
	   enddo
	endif
	if(sdata.and.iwt.ne.4) then
	   print 53
	   sdata=.false.
53	   format(
     & ' Use true weighting for LS analysis [Y] ? ')
	   call INPUTa(ans)
	   if(ans.eq.'N') then
		print 54
		if(pon()) write(7,54)
		if(discprt) write(8,54)
54		format(/,
     & 	' Error structure assumed for weights in LS analysis:')
		print 56
56		format(
     &	' (1) Constant SD (specified)',/,
     &	' (2) SD = a + b*Ytrue',/,
     & 	' (3) Constant SD estimated from residuals',/,
     & 	' Option number [1] = ')
		call INPUTi(iwt1)
		if(iwt1.eq.3) then
		   useres=.true.
		else		!get the weights to be used for analysis
		   iwt=iwt1
		   goto 52
		endif
	   else
		if(pon()) write(7,55)
		if(discprt) write(8,55)
55		format(
     & 	' True weights used for LS analysis:',/)
	   endif
c
	   if(useres) then
		if(pon()) write(7,561)
		if(discprt) write(8,561)
561		format(
     &' All weights=1 for fitting, with errors estimated from residuals'
     & ,/,
     & ' in each expt., taking error var=Smin/(n-2)',/)
c     Set all weights=1 for fitting and insert Sres later (but leave sd(i)
c	 as set above for simulation of expts)
		do i=1,n
		   w(i)=1.0
		enddo
	   endif
	endif
c
	if(useres) then
c	   tval=ttable(n-2)
	   call TVALUE(n-2,tval)	!get 95% value of t
	else
	   tval=1.960
	endif
	em=tval*tval/2.
	print 58,tval,n-2,em
	if(pon()) write(7,58) tval,n-2,em
	if(discprt) write(8,58) tval,n-2,em
58    format(' NOTE:  '
     &'''2*SD'' means t*SD where t = ',f9.4,' (P=0.95 and ',i2,' d.f.)'
     &,/,
     &' and ''m=2'' means corresponding value for m = t*t/2 = ',f9.4,/)
c
	nrun=1000
	print 4,nrun
4	format(' Number of ''experiments to be simulated [',i5,'] = ')
	call INPUTi(nrun)
c
	call RANDSK(ix,iy,iz,0,repeat)		!read IX,IY,IZ
c
10	continue
c
c	emval=0.5
c	print 8,emval
c8	format(' Calculate m-unit likelihood intervals: m [',f5.1,'] = ')
c	call INPUTr(emval)
c
	ALLOCATE(aK(nrun,6),Ym(nrun,6),hill(nrun))
	ALLOCATE(sdK(nrun),sdY(nrun),aKlo(nrun),aKhi(nrun),
     &	Ylo(nrun),Yhi(nrun),cor(nrun),Sres(nrun),Sresh(nrun))
	ALLOCATE(sdKe(nrun),sdYe(nrun),core(nrun))
	ALLOCATE(aKlo2(nrun),aKhi2(nrun),Ylo2(nrun),Yhi2(nrun))
	ALLOCATE (sdKh(nrun),sdYh(nrun),sdnh(nrun),
     &	corYK(nrun),corYn(nrun),corKn(nrun))
	ALLOCATE (sdKhe(nrun),sdYhe(nrun),sdnhe(nrun),
     &	corYKe(nrun),corYne(nrun),corKne(nrun))
	nloK1=0	!count number of lower lik intervals that are neg
	nhiK1=0	!count number of upper lik intervals that are infinite
	nloK2=0	!ditto (m=2)
	nhiK2=0	!ditto (m=2)
c
	do irun=1,nrun
	   deb=debug()
	   if(test) goto 11
c Simulate the observations:
	   do i=1,n
		u=ranorm()
		yobs(i)=u*sd(i)+ptrue(i)
		if(iwt.eq.3) then		!replace theoretical weight by obs
		   sdobs=aval + bval*yobs(i)
		   w(i)=1.0/(sdobs*sdobs)
		endif
	   enddo
c Now have 'observations' in c(i),yobs(i) -fit them by 5 (6?) methods
c Hill plot not really possible -where does max com from?
c	(1) Lineweaver-Burk
c	(2) Scatchard
c	(3) Eadie-Hofstee
c	(4) x/y versus x
c	(5) LS
c	(6) Hill plot?
c
11	   continue
c	(1) Lineweaver-Burk
	   im=1		!Method number
	   do i=1,n
		x1(i)=1.0/c(i)
		y1(i)=1.0/yobs(i)
	   enddo
	   call LINREG(x1,y1,n,a,b)
	   Ym1=1.0/a
	   aK1=b/a
	   aK(irun,im)=aK1
	   Ym(irun,im)=Ym1
c
c	(2) Scatchard
	   im=2		!Method number
	   do i=1,n
		x1(i)=yobs(i)
		y1(i)=yobs(i)/c(i)
	   enddo
	   call LINREG(x1,y1,n,a,b)
	   Ym1=-a/b
	   aK1=-1.0/b
	   aK(irun,im)=aK1
	   Ym(irun,im)=Ym1
c
c	(3) Eadie-Hofstee
	   im=3		!Method number
	   do i=1,n
		x1(i)=yobs(i)/c(i)
		y1(i)=yobs(i)
	   enddo
	   call LINREG(x1,y1,n,a,b)
	   Ym1=a
	   aK1=-b
	   aK(irun,im)=aK1
	   Ym(irun,im)=Ym1
c
c	(4) x/y versus x
	   im=4		!Method number
	   do i=1,n
		x1(i)=c(i)
		y1(i)=c(i)/yobs(i)
	   enddo
	   call LINREG(x1,y1,n,a,b)
	   Ym1=1.0/b
	   aK1=a/b
	   aK(irun,im)=aK1
	   Ym(irun,im)=Ym1
c
c	(5) Least squares -minimise function ssdhyp(k,theta)
	   im=5		!Method number
	   theta(1)=aKtrue	!init guess=true value
	   theta(2)=Ymtrue
	   ndisp=-1000		!silent
	   nevmax=10000
	   errfac=1.e-4
	   delmin=-1.		!do not use delmin for convergence
	   confac=0.5
	   resfac=1.0
	   irestrt=3
	   stpfac=0.1
c	   nfix=0		!no param fixed
	   do i=1,20
		jfix(i)=0
	   enddo
	   pertval(1)=0.02
	   pertval(2)=0.02
	   lowval(1)=0.0
	   lowval(2)=0.0
	   highval(1)=1.e20
	   highval(2)=1.e20
c Re-set weights to 1.0 for each run now in case where error is from residuals
	   if(useres) then
		do i=1,n
		   w(i)=1.0
		enddo
	   endif
	   call SIMPLEX4(2,THETA,stpfac,errfac,neval,nevmax,
     &    smin,SSDHYP,Ndisp,jfix,delmin,confac,irestrt,resfac,iconv,
     &    lowval,highval,pertval)
c	   if(abort) then
c
c	   endif
	   aK1=theta(1)
	   Ym1=theta(2)
	   aK(irun,im)=aK1
	   Ym(irun,im)=Ym1
	   var=smin/float(n-2)
	   Sres(irun)=sqrt(var)
c In case where error is from residuals, weights were set to 1 for
c fitting, but now set weights for error analysis using Smin.  If these
c weights had been used for fitting, all deviations would have been
c multiplied by 1/sres**2, so must multiply Smin by this too, to get
c the value equivalent to Lmax = -SSDmin/(2*var)
	   if(useres) then
		do i=1,n
		   w(i)=1.0/var
		enddo
		Smin=Smin/var
	   endif
c
	   if(aK1.gt.aKmax.or.Ym1.gt.Ymmax) goto 15	!skip errors
c     Approx SD and likelihood intervals?
c Calculate Scrit
	   emval=0.5
	   Scrit1=Smin + 2.0*emval
	   emval=tval*tval/2.0		!=1.96*1.96/2 = 1.9208 usually
	   Scrit2=Smin + 2.0*emval
c Get SD from observed info matrix for Langmuir fit
	   call HYPVAR(Ym1,aK1,sdK1,sdY1,cor1)
	   sdK(irun)=sdK1
	   sdY(irun)=sdY1
	   cor(irun)=cor1
c Get SD from expected info matrix for Langmuir fit
	   call HYPVARe(Ym1,aK1,sdK1,sdY1,cor1)
	   sdKe(irun)=sdK1
	   sdYe(irun)=sdY1
	   core(irun)=cor1
c Do em=0.5 and em=2 on each cycle
	   Scrit=Scrit1		!in common for HYPLIK
	   call HYPLIK(aK1,Ym1,sdK1,aKlo1,aKhi1,Ylo1,Yhi1,nloK1,nhiK1,
     &	emval,deb)
	   aKlo(irun)=aKlo1	!m=0.5 unit intervals
	   aKhi(irun)=aKhi1
	   Ylo(irun)=Ylo1
	   Yhi(irun)=Yhi1
	   Scrit=Scrit2		!in common for HYPLIK
	   call HYPLIK(aK1,Ym1,sdK1,aKlo1,aKhi1,Ylo1,Yhi1,nloK2,nhiK2,
     &	emval,deb)
c     Store m=2 limits here
	   aKlo2(irun)=aKlo1
	   aKhi2(irun)=aKhi1
	   Ylo2(irun)=Ylo1
	   Yhi2(irun)=Yhi1
c
15	   continue
c (6) Fit Hill equation to same data (true nHill=1)
c	 -minimise function ssdhill(k,theta)
	   im=6		!Method number
	   nHtrue=1.
	   theta(1)=aKtrue	!init guess=true value
	   theta(2)=Ymtrue
	   theta(3)=nHtrue
	   ndisp=-1000		!silent
	   nevmax=10000
	   errfac=1.e-4
	   delmin=-1.		!do not use delmin for convergence
	   confac=0.5
	   resfac=1.0
	   irestrt=3
	   stpfac=0.1
c	   nfix=0		!no param fixed
	   do i=1,20
		jfix(i)=0
	   enddo
c=	   if(test) jfix(3)=1	!fix hill coeff for test data
	   pertval(1)=0.02
	   pertval(2)=0.02
	   pertval(3)=0.02
	   lowval(1)=0.0
	   lowval(2)=0.0
	   lowval(3)=0.0
	   highval(1)=1.e20
	   highval(2)=1.e20
	   highval(3)=1.e20
c Re-set weights to 1.0 for each run now in case where error is from residuals
	   if(useres.or.test) then
		do i=1,n
		   w(i)=1.0
		enddo
	   endif
	   call SIMPLEX4(3,THETA,stpfac,errfac,neval,nevmax,
     &    smin,SSDHILL,Ndisp,jfix,delmin,confac,irestrt,resfac,iconv,
     &    lowval,highval,pertval)
	   if(abort) then
c
	   endif
	   aK1=theta(1)
	   Ym1=theta(2)
	   hill1=theta(3)
	   aK(irun,im)=aK1
	   Ym(irun,im)=Ym1
	   hill(irun)=hill1
	   var=smin/float(n-3)		!n-3 df for Hill fit
	   Sresh(irun)=sqrt(var)
c In case where error is from residuals, weights were set to 1 for
c fitting, but now set weights for error analysis using Smin.  If these
c weights had been used for fitting, all deviations would have been
c multiplied by 1/sres**2, so must multiply Smin by this too, to get
c the value equivalent to Lmax = -SSDmin/(2*var)
	   if(useres) then
		do i=1,n
		   w(i)=1.0/var
		enddo
		Smin=Smin/var
	   endif
	   if(aK1.gt.aKmax.or.Ym1.gt.Ymmax.or.hill1.gt.hillmax) goto 151	!skip errors
c     Approx SD and likelihood intervals?
c Calculate Scrit
c=	   emval=0.5
c=	   Scrit1=Smin + 2.0*emval
c=	   emval=tval*tval/2.0		!=1.96*1.96/2 = 1.9208 usually
c=	   Scrit2=Smin + 2.0*emval
c= Do em=0.5 and em=2 on each cycle
c Get SD from observed information matrix for Hill fit
	   call HILLVAR(Ym1,aK1,hill1,sdK1,sdY1,sdn1,corYK1,corYn1,corKn1)
	   sdKh(irun)=sdK1
	   sdYh(irun)=sdY1
	   sdnh(irun)=sdn1
	   corYK(irun)=corYK1
	   corYn(irun)=corYn1
	   corKn(irun)=corKn1
c Get SD from expected information matrix for Hill fit
	   call HILLVARe(Ym1,aK1,hill1,sdK1,sdY1,sdn1,
     &		corYK1,corYn1,corKn1)
	   sdKhe(irun)=sdK1
	   sdYhe(irun)=sdY1
	   sdnhe(irun)=sdn1
	   corYKe(irun)=corYK1
	   corYne(irun)=corYn1
	   corKne(irun)=corKn1
151	   continue
c
c
	   if(deb) then	!print expt
		call HYPEXPT(irun,nrun,n,c,yobs,w,aK,Ym,hill,sdK,sdY,cor,
     &       aKlo,aKhi,Ylo,Yhi,aKlo2,aKhi2,Ylo2,Yhi2,aKtrue,Ymtrue,
     &	 aKmax,Ymmax,hillmax,dans)
	   endif
c
	enddo		!end of irun loop
c
c Summarise residuals
	call VARV1(Sres,nrun,ybar,vary,ymin,ymax)
	print 57,nrun,ybar,sqrt(vary),ymin,ymax
	if(pon()) write(7,57) nrun,ybar,sqrt(vary),ymin,ymax
	if(discprt) write(8,57) nrun,ybar,sqrt(vary),ymin,ymax
57	format(/,' LS fit of Hill-Langmuir hyperbola:',/,
     & ' Values for SD from residuals = sqrt[Smin/(n-2)]',/,
     & ' Mean and SD of ',i5,' values= ',g13.6,' +/-',g13.6,
     & /,' Range from ',g13.6,' to ',g13.6,/)
	call VARV1(Sresh,nrun,ybar,vary,ymin,ymax)
	print 571,nrun,ybar,sqrt(vary),ymin,ymax
	if(pon()) write(7,571) nrun,ybar,sqrt(vary),ymin,ymax
	if(discprt) write(8,571) nrun,ybar,sqrt(vary),ymin,ymax
571	format(/,' LS fit of Hill equation:',/,
     & ' Values for SD from residuals = sqrt[Smin/(n-3)]',/,
     & ' Mean and SD of ',i5,' values= ',g13.6,' +/-',g13.6,
     & /,' Range from ',g13.6,' to ',g13.6,/)
	DEALLOCATE(Sres,Sresh)
c Summarise distributions of K, Ym for hyperbola fit
	ALLOCATE(yval1(nrun),ibad(nrun))
c    First screen for bad expts
	nb=0
	do i=1,nrun
	   aK1=aK(i,5)
	   Ym1=Ym(i,5)
	   if(aK1.gt.aKmax.or.Ym1.gt.Ymmax) then
		nb=nb+1
		ibad(i)=1
		print 65,i,aK1,Ym1
		if(pon()) write(7,65) i,aK1,Ym1
		if(discprt) write(8,65) i,aK1,Ym1
65		format(
     &   	' Hyperbola fit #',i4,' omitted: K, Ym = ',2g13.6)
	   else
		ibad(i)=0
	   endif
	enddo
	ny=0
	do i=1,nrun
	   if(ibad(i).eq.0) then
	      aK1=aK(i,5)
	      ny=ny+1
	      yval1(ny)=aK1
	   endif
	enddo
	call VARV3(Yval1,ny,ybar,vary,ymin,ymax,skew)
	print 60,ny,ybar,sqrt(vary),skew,ymin,ymax
	if(pon()) write(7,60) ny,ybar,sqrt(vary),skew,ymin,ymax
	if(discprt) write(8,60) ny,ybar,sqrt(vary),skew,ymin,ymax
60	format(/,' Values for K estimates for hyperbola fit',/,
     & ' Mean and SD of ',i5,' values= ',g13.6,' +/-',g13.6,/,
     & ' Skewness index = ',g13.6,/,
     & ' Range from ',g13.6,' to ',g13.6,/)
	ny=0
	do i=1,nrun
	   if(ibad(i).eq.0) then
	      Ym1=Ym(i,5)
		ny=ny+1
		yval1(ny)=Ym1
	   endif
	enddo
	call VARV3(Yval1,ny,ybar,vary,ymin,ymax,skew)
	print 61,ny,ybar,sqrt(vary),skew,ymin,ymax
	if(pon()) write(7,61) ny,ybar,sqrt(vary),skew,ymin,ymax
	if(discprt) write(8,61) ny,ybar,sqrt(vary),skew,ymin,ymax
61	format(/,' Values for Ymax estimates  for hyperbola fit',/,
     & ' Mean and SD of ',i5,' values= ',g13.6,' +/-',g13.6,/,
     & ' Skewness index = ',g13.6,/,
     & ' Range from ',g13.6,' to ',g13.6,/)
c Summarise distributions of K, Ym, nH for Hill fit
c    First screen for bad expts
	nb=0
	do i=1,nrun
	   aK1=aK(i,6)
	   Ym1=Ym(i,6)
	   hill1=hill(i)
	   if(aK1.gt.aKmax.or.Ym1.gt.Ymmax.or.hill1.gt.hillmax) then
		nb=nb+1
		ibad(i)=1
		print 68,i,aK1,Ym1,hill1
		if(pon()) write(7,68) i,aK1,Ym1,hill1
		if(discprt) write(8,68) i,aK1,Ym1,hill1
68		format(
     &   	' Hill fit #',i4,' omitted: K, Ym, nH = ',3g13.6)
	   else
		ibad(i)=0
	   endif
	enddo
	ny=0
	do i=1,nrun
	   if(ibad(i).eq.0) then
	      aK1=aK(i,6)
	      ny=ny+1
	      yval1(ny)=aK1
	   endif
	enddo
	call VARV3(Yval1,ny,ybar,vary,ymin,ymax,skew)
	print 62,ny,ybar,sqrt(vary),skew,ymin,ymax
	if(pon()) write(7,62) ny,ybar,sqrt(vary),skew,ymin,ymax
	if(discprt) write(8,62) ny,ybar,sqrt(vary),skew,ymin,ymax
62	format(/,' Values for K estimates for Hill fit',/,
     & ' Mean and SD of ',i5,' values= ',g13.6,' +/-',g13.6,/,
     & ' Skewness index = ',g13.6,/,
     & ' Range from ',g13.6,' to ',g13.6,/)
	ny=0
	do i=1,nrun
	   if(ibad(i).eq.0) then
	      Ym1=Ym(i,6)
		ny=ny+1
		yval1(ny)=Ym1
	   endif
	enddo
	call VARV3(Yval1,ny,ybar,vary,ymin,ymax,skew)
	print 63,ny,ybar,sqrt(vary),skew,ymin,ymax
	if(pon()) write(7,63) ny,ybar,sqrt(vary),skew,ymin,ymax
	if(discprt) write(8,63) ny,ybar,sqrt(vary),skew,ymin,ymax
63	format(/,' Values for Ymax estimates for Hill fit',/,
     & ' Mean and SD of ',i5,' values= ',g13.6,' +/-',g13.6,/,
     & ' Skewness index = ',g13.6,/,
     & ' Range from ',g13.6,' to ',g13.6,/)
	ny=0
	do i=1,nrun
	   if(ibad(i).eq.0) then
	      hill1=hill(i)
		ny=ny+1
		yval1(ny)=hill1
	   endif
	enddo
	call VARV3(Yval1,ny,ybar,vary,ymin,ymax,skew)
	print 64,ny,ybar,sqrt(vary),skew,ymin,ymax
	if(pon()) write(7,64) ny,ybar,sqrt(vary),skew,ymin,ymax
	if(discprt) write(8,64) ny,ybar,sqrt(vary),skew,ymin,ymax
64	format(/,' Values for nHill estimates for Hill fit',/,
     & ' Mean and SD of ',i5,' values= ',g13.6,' +/-',g13.6,/,
     & ' Skewness index = ',g13.6,/,
     & ' Range from ',g13.6,' to ',g13.6,/)
c
	DEALLOCATE(yval1,ibad)
c
c
c NOW ANALYSE ERRORS FOR LS FIT OF HYPERBOLA
	nk1=0
	nk2=0
	nk3=0
	nk4=0
	ny1=0
	ny2=0
	ny3=0
	ny4=0
	nk1e=0
	nk2e=0
	ny1e=0
	ny2e=0

	nloK1=0
	nhiK1=0
	ninfK1=0
	ngoodK1=0
	nloK2=0
	nhiK2=0
	ninfK2=0
	ngoodK2=0

	nloY1=0
	nhiY1=0
	ninfY1=0
	ngoodY1=0
	nloY2=0
	nhiY2=0
	ninfY2=0
	ngoodY2=0

	nbig=0
c Allocate arrays to get width and asymmetry of intervals
	ALLOCATE(widlK1(nrun),widlK2(nrun),asymK1(nrun),
     &	asymK2(nrun))
	ALLOCATE(widlY1(nrun),widlY2(nrun),asymY1(nrun),
     &	asymY2(nrun))
c Arrays for K,Ym values from expts in which true value is NOT included
c in 2*SD, or m=2, limits.  Should be about 5% of nrun, but make their
c size 0.2*nrun to be in the safe side
	ndx=1+nrun/5
	ALLOCATE(aKexcb(ndx,2),aKexcs(ndx,2),aKexcm(ndx,2),
     & aYexcb(ndx,2),aYexcs(ndx,2),aYexcm(ndx,2))

	nsdK=0
	nsdY=0
	nsdKe=0
	nsdYe=0
	nwk1=0
	nwk2=0
	nwy1=0
	nwy2=0

	nkboth=0
	nkexcs=0
	nkexcm=0
	nyboth=0
	nyexcs=0
	nyexcm=0
	nkboth1=0
	nkboth2=0
	nkboth3=0
	nkboth4=0
	nkexcs1=0
	nkexcs2=0
	nkexcm1=0
	nkexcm2=0
	nyboth1=0
	nyboth2=0
	nyboth3=0
	nyboth4=0
	nyexcs1=0
	nyexcs2=0
	nyexcm1=0
	nyexcm2=0
c
	nall1=0
	excKs=.false.	!initialise in case K limits undefined when nall calc
	excKm=.false.
c
	do irun=1,nrun
c	  Check for enormous values (error estimation skipped)
	   aK1=aK(irun,5)
	   Ym1=Ym(irun,5)
	   if(aK1.gt.aKmax.or.Ym1.gt.Ymmax) then
		nbig=nbig+1
		goto 20	!skip errors
	   endif
c
c	  Check for negative low K or Y.  Low K limit may have valid
c	  negative value, or -1001. if not determined for any reason.
c	  Check for infinite upper limits -upper limit for K or Y set to 1,e6
c	   if infinite, or to -1001. if could not be determined.
c	  Check for invalid limits

c   Check whether limits include true K
c	 First SD from obs info
	   aK1=aK(irun,5)
	   sdK1=sdK(irun)
	   if(sdK1.gt.0.) then
		nsdK=nsdK+1
	      if(aK1+sdK1.ge.aKtrue.and.aK1-sdK1.le.aKtrue) nk1=nk1+1
	      sdk2=tval*sdk1		!tval=1.96 except when residuals used
	      if(aK1+sdK2.ge.aKtrue.and.aK1-sdK2.le.aKtrue) nk2=nk2+1
	   endif
c	 SD from exp info
	   sdK1=sdKe(irun)
	   if(sdK1.gt.0.) then
		nsdKe=nsdKe+1
	      if(aK1+sdK1.ge.aKtrue.and.aK1-sdK1.le.aKtrue) nk1e=nk1e+1
	      sdk2=tval*sdk1		!tval=1.96 except when residuals used
	      if(aK1+sdK2.ge.aKtrue.and.aK1-sdK2.le.aKtrue) nk2e=nk2e+1
	   endif
c
c       Next likelihood intervals
	   aKlo1=aKlo(irun)
	   aKhi1=aKhi(irun)
	   good=.true.
	   good1=.true.
	   if(aKlo1.lt.-1000.) then
		nloK1=nloK1+1
		good=.false.
	   endif
	   if(aKhi1.lt.-1000.) then
		nhiK1=nhiK1+1
		good=.false.
	   endif
	   if(aKhi1.gt.9.0e5) then
		ninfK1=ninfK1+1	!set to 1.e6 in hyplik
		good1=.false.
	   endif
	   if(good) then	!valid lower and upper limits (or upper -> infinity)
		ngoodK1=ngoodK1+1
	 	if(aKhi1.ge.aKtrue.and.aKlo1.le.aKtrue) nk3=nk3+1
	   endif
	   if(good.and.good1) then	!exclude infinite limits from width!
		nwk1=nwk1+1
		widlK1(nwK1)=aKhi1-aKlo1
		asymK1(nwK1)=(aKhi1-aK1)/(aK1-aKlo1)
	   endif
c      Repeat for m=2 limits
	   aKlo1=aKlo2(irun)
	   aKhi1=aKhi2(irun)
	   good=.true.
	   good1=.true.
	   if(aKlo1.lt.-1000.) then
		nloK2=nloK2+1
		good=.false.
	   endif
	   if(aKhi1.lt.-1000.) then
		nhiK2=nhiK2+1
		good=.false.
	   endif
	   if(aKhi1.gt.9.0e5) then
		ninfK2=ninfK2+1	!set to 1.e6 in hyplik
		good1=.false.
	   endif
	   if(good) then	!valid lower and upper limits (or upper -> infinity)
		ngoodK2=ngoodK2+1
	 	if(aKhi1.ge.aKtrue.and.aKlo1.le.aKtrue) nk4=nk4+1
	   endif
	   if(good.and.good1) then	!exclude infinite limits from width!
		nwk2=nwk2+1
		widlK2(nwK2)=aKhi1-aKlo1
		asymK2(nwK2)=(aKhi1-aK1)/(aK1-aKlo1)
	   endif
c
c Compare +/- 2*SD with m=2 interval for K
	   if(good.and.sdK1.gt.0.) then
	      sdk2=tval*sdk1		!tval=1.96 except when residuals used
	      excKs=aK1+sdK2.lt.aKtrue.or.aK1-sdK2.gt.aKtrue	!K outside 2SD limit
	 	excKm=aKhi1.lt.aKtrue.or.aKlo1.gt.aKtrue	!K outside m=2 limits
		if(excKs.and.excKm) then
		   nkboth=nkboth+1
		   if(aK1+sdK2.lt.aKtrue) nkboth1=nkboth1+1
		   if(aK1-sdK2.gt.aKtrue) nkboth2=nkboth2+1
		   if(aKhi1.lt.aKtrue) nkboth3=nkboth3+1
		   if(aKlo1.gt.aKtrue) nkboth4=nkboth4+1
		   aKexcb(nkboth,1)=aK1
		   aKexcb(nkboth,2)=Ym1
		   if(deb) then
		    print 73,irun
		    if(pon()) write(7,73) irun
		    if(discprt) write(8,73) irun
73 		    format(
     &	  ' Expt ',i5,': both 2*SD and m=2 limits exclude true K')
		    print 74,ans2
74 		    format(
     &		' Display this expt [',a1,'] ? ')
		    call INPUTa(ans2)
		    if(ans2.eq.'Y') then
	         call HYPEXPT(irun,nrun,n,c,yobs,w,aK,Ym,hill,sdK,sdY,cor,
     &       aKlo,aKhi,Ylo,Yhi,aKlo2,aKhi2,Ylo2,Yhi2,aKtrue,Ymtrue,
     &	 aKmax,Ymmax,hillmax,dans)
		    endif
		   endif
		else if(excKs.and.(.not.excKm)) then
		   nkexcs=nkexcs+1
		   if(aK1+sdK2.lt.aKtrue) nkexcs1=nkexcs1+1
		   if(aK1-sdK2.gt.aKtrue) nkexcs2=nkexcs2+1
		   aKexcs(nkexcs,1)=aK1
		   aKexcs(nkexcs,2)=Ym1
		   if(deb) then
		    print 75,irun
		    if(pon()) write(7,75) irun
		    if(discprt) write(8,75) irun
75 		    format(
     &	  ' Expt ',i5,': 2*SD limits exclude true K, but not m=2 ')
		    print 74,ans2
		    call INPUTa(ans2)
		    if(ans2.eq.'Y') then
	         call HYPEXPT(irun,nrun,n,c,yobs,w,aK,Ym,hill,sdK,sdY,cor,
     &       aKlo,aKhi,Ylo,Yhi,aKlo2,aKhi2,Ylo2,Yhi2,aKtrue,Ymtrue,
     &	 aKmax,Ymmax,hillmax,dans)
		    endif
		   endif
		else if(excKm.and.(.not.excKs)) then
		   nkexcm=nkexcm+1
		   if(aKhi1.lt.aKtrue) nkexcm1=nkexcm1+1
		   if(aKlo1.gt.aKtrue) nkexcm2=nkexcm2+1
		   aKexcm(nkexcm,1)=aK1
		   aKexcm(nkexcm,2)=Ym1
		   if(deb) then
		    print 76,irun
		    if(pon()) write(7,76) irun
		    if(discprt) write(8,76) irun
76 		    format(
     &	  ' Expt ',i5,':  m=2 limits exclude true K, but not 2*SD')
		    print 74,ans2
		    call INPUTa(ans2)
		    if(ans2.eq.'Y') then
	         call HYPEXPT(irun,nrun,n,c,yobs,w,aK,Ym,hill,sdK,sdY,cor,
     &       aKlo,aKhi,Ylo,Yhi,aKlo2,aKhi2,Ylo2,Yhi2,aKtrue,Ymtrue,
     &	 aKmax,Ymmax,hillmax,dans)
		    endif
		   endif
		endif
	    endif
c
c NEXT Ymax: check whether limits include true Ymax
c	 First SD
	   Ym1=Ym(irun,5)
c	 SD from obs info
	   sdY1=sdY(irun)
	   if(sdY1.gt.0.) then
		nsdY=nsdY+1
		if(Ym1+sdY1.ge.Ymtrue.and.Ym1-sdY1.le.Ymtrue) ny1=ny1+1
		sdY2=tval*sdY1	!tval=1.96 except when residuals used
		if(Ym1+sdY2.ge.Ymtrue.and.Ym1-sdY2.le.Ymtrue) ny2=ny2+1
	   endif
c	 SD from exp info
	   sdY1=sdYe(irun)
	   if(sdY1.gt.0.) then
		nsdYe=nsdYe+1
		if(Ym1+sdY1.ge.Ymtrue.and.Ym1-sdY1.le.Ymtrue) ny1e=ny1e+1
		sdY2=tval*sdY1	!tval=1.96 except when residuals used
		if(Ym1+sdY2.ge.Ymtrue.and.Ym1-sdY2.le.Ymtrue) ny2e=ny2e+1
	   endif
c
c       Next likelihood intervals
	   Ylo1=Ylo(irun)
	   Yhi1=Yhi(irun)
	   good=.true.
	   good1=.true.
	   if(Ylo1.lt.-1000.) then
		nloY1=nloY1+1
		good=.false.
	   endif
	   if(Yhi1.lt.-1000.) then
		nhiY1=nhiY1+1
		good=.false.
	   endif
	   if(Yhi1.gt.9.0e5) then
		ninfY1=ninfY1+1	!set to 1.e6 in hyplik
		good1=.false.
	   endif
	   if(good) then	!valid lower and upper limits (or upper -> infinity)
		ngoodY1=ngoodY1+1
	 	if(Yhi1.ge.Ymtrue.and.Ylo1.le.Ymtrue) ny3=ny3+1
	   endif
	   if(good.and.good1) then	!exclude infinite limits from width!
		nwy1=nwy1+1
		widlY1(nwY1)=Yhi1-Ylo1
		asymY1(nwY1)=(Yhi1-Ym1)/(Ym1-Ylo1)
	   endif
c      Repeat for m=2 limits
	   Ylo1=Ylo2(irun)
	   Yhi1=Yhi2(irun)
	   good=.true.
	   good1=.true.
	   if(Ylo1.lt.-1000.) then
		nloY2=nloY2+1
		good=.false.
	   endif
	   if(Yhi1.lt.-1000.) then
		nhiY2=nhiY2+1
		good=.false.
	   endif
	   if(Yhi1.gt.9.0e5) then
		ninfY2=ninfY2+1	!set to 1.e6 in hyplik
		good1=.false.
	   endif
	   if(good) then	!valid lower and upper limits (or upper -> infinity)
		ngoodY2=ngoodY2+1
	 	if(Yhi1.ge.Ymtrue.and.Ylo1.le.Ymtrue) ny4=ny4+1
	   endif
	   if(good.and.good1) then	!exclude infinite limits from width!
		nwy2=nwy2+1
		widlY2(nwY2)=Yhi1-Ylo1
		asymY2(nwY2)=(Yhi1-Ym1)/(Ym1-Ylo1)
	   endif
c Compare +/- 2*SD with m=2 interval for Ymax
	   if(good.and.sdY1.gt.0.) then
	      sdy2=tval*sdy1		!tval=1.96 unless residuals used for error
	      excYs=Ym1+sdY2.lt.Ymtrue.or.Ym1-sdY2.gt.Ymtrue	!Ym outside 2SD limit
	 	excYm=Yhi1.lt.Ymtrue.or.Ylo1.gt.Ymtrue	!Ym outside m=2 limits
		if(excYs.and.excYm.and.excKs.and.excKm) nall1=nall1+1
		if(excYs.and.excYm) then
		   nyboth=nyboth+1
		   if(Ym1+sdY2.lt.Ymtrue) nyboth1=nyboth1+1
		   if(Ym1-sdY2.gt.Ymtrue) nyboth2=nyboth2+1
		   if(Yhi1.lt.Ymtrue) nyboth3=nyboth3+1
		   if(Ylo1.gt.Ymtrue) nyboth4=nyboth4+1
		   aYexcb(nyboth,1)=aK1
		   aYexcb(nyboth,2)=Ym1
		   if(deb) then
		    print 77,irun
		    if(pon()) write(7,77) irun
		    if(discprt) write(8,77) irun
77 		    format(
     &	' Expt ',i5,': both 2*SD and m=2 limits exclude true Ymax')
		    print 74,ans2
		    call INPUTa(ans2)
		    if(ans2.eq.'Y') then
	         call HYPEXPT(irun,nrun,n,c,yobs,w,aK,Ym,hill,sdK,sdY,cor,
     &       aKlo,aKhi,Ylo,Yhi,aKlo2,aKhi2,Ylo2,Yhi2,aKtrue,Ymtrue,
     &	 aKmax,Ymmax,hillmax,dans)
		    endif
		   endif
		else if(excYs.and.(.not.excYm)) then
		   nyexcs=nyexcs+1
		   if(Ym1+sdY2.lt.Ymtrue) nyexcs1=nyexcs1+1
		   if(Ym1-sdY2.gt.Ymtrue) nyexcs2=nyexcs2+1
		   aYexcs(nyexcs,1)=aK1
		   aYexcs(nyexcs,2)=Ym1
		   if(deb) then
		    print 751,irun
		    if(pon()) write(7,751) irun
		    if(discprt) write(8,751) irun
751 		    format(
     &	' Expt ',i5,': 2*SD limits exclude true Ymax, but not m=2 ')
		    print 74,ans2
		    call INPUTa(ans2)
		    if(ans2.eq.'Y') then
	         call HYPEXPT(irun,nrun,n,c,yobs,w,aK,Ym,hill,sdK,sdY,cor,
     &       aKlo,aKhi,Ylo,Yhi,aKlo2,aKhi2,Ylo2,Yhi2,aKtrue,Ymtrue,
     &	 aKmax,Ymmax,hillmax,dans)
		    endif
		   endif
		else if(excYm.and.(.not.excYs)) then
		   nyexcm=nyexcm+1
		   if(Yhi1.lt.Ymtrue) nyexcm1=nyexcm1+1
		   if(Ylo1.gt.Ymtrue) nyexcm2=nyexcm2+1
		   aYexcm(nyexcm,1)=aK1
		   aYexcm(nyexcm,2)=Ym1
		   if(deb) then
		    print 761,irun
		    if(pon()) write(7,761) irun
		    if(discprt) write(8,761) irun
761 		    format(
     &	' Expt ',i5,':  m=2 limits exclude true Ymax, but not 2*SD')		    print 74,ans2
		    call INPUTa(ans2)
		    if(ans2.eq.'Y') then
	         call HYPEXPT(irun,nrun,n,c,yobs,w,aK,Ym,hill,sdK,sdY,cor,
     &       aKlo,aKhi,Ylo,Yhi,aKlo2,aKhi2,Ylo2,Yhi2,aKtrue,Ymtrue,
     &	 aKmax,Ymmax,hillmax,dans)
		    endif
		   endif
		endif
	    endif
c
20	   continue
	enddo		!end of do irun=1,nrun
c
c
c	en=float(nrun)
c	pk1=float(nk1)/en
c	pk2=float(nk2)/en
	pk1=float(nk1)/float(nsdK)
	pk2=float(nk2)/float(nsdK)
	pk3=float(nk3)/float(ngoodK1)
	pk4=float(nk4)/float(ngoodK2)
c	py1=float(ny1)/en
c	py2=float(ny2)/en
	py1=float(ny1)/float(nsdY)
	py2=float(ny2)/float(nsdY)
	py3=float(ny3)/float(ngoodY1)
	py4=float(ny4)/float(ngoodY2)
c
c Print error analysis for hyperbola fits
	print 18,nrun,nbig,aKmax,Ymmax
	if(pon()) write(7,18) nrun,nbig,aKmax,Ymmax
	if(discprt) write(8,18) nrun,nbig,aKmax,Ymmax
18	format(/,' ERROR ANALYSIS FOR FIT OF HYPERBOLA',/,
     & ' Total number of runs = ',i8,/,
     &' Number in which big estimates preclude error estimation = ',i8,/
     &,' (big means K >',f8.1,' or Ymax > ',f8.1,')')
	print 181,nlok1,nhiK1,ninfK1,ngoodK1
	if(pon()) write(7,181) nlok1,nhiK1,ninfK1,ngoodK1
	if(discprt) write(8,181) nlok1,nhiK1,ninfK1,ngoodK1
181	format(
     & ' Number of runs in which',/,
     & ' Lower m=0.5 limit for K is invalid  = ',i5,/,
     & ' Upper m=0.5 limit for K is invalid  = ',i5,/,
     & ' Upper m=0.5 limit for K is infinite = ',i5,/,
     & ' Both m=0.5 limits for K are valid   = ',i5,/)
	print 182,nlok2,nhiK2,ninfK2,ngoodK2
	if(pon()) write(7,182) nlok2,nhiK2,ninfK2,ngoodK2
	if(discprt) write(8,182) nlok2,nhiK2,ninfK2,ngoodK2
182	format(
     & ' Number of runs in which',/,
     & ' Lower m=2.0 limit for K is invalid  = ',i5,/,
     & ' Upper m=2.0 limit for K is invalid  = ',i5,/,
     & ' Upper m=2.0 limit for K is infinite = ',i5,/,
     & ' Both m=2.0 limits for K are valid   = ',i5,/)
	print 183,nloY1,nhiY1,ninfY1,ngoodY1
	if(pon()) write(7,183) nloY1,nhiY1,ninfY1,ngoodY1
	if(discprt) write(8,183) nloY1,nhiY1,ninfY1,ngoodY1
183	format(
     & ' Number of runs in which',/,
     & ' Lower m=0.5 limit for Ymax is invalid  = ',i5,/,
     & ' Upper m=0.5 limit for Ymax is invalid  = ',i5,/,
     & ' Upper m=0.5 limit for Ymax is infinite = ',i5,/,
     & ' Both m=0.5 limits for Ymax are valid   = ',i5,/)
	print 184,nloY2,nhiY2,ninfY2,ngoodY2
	if(pon()) write(7,184) nloY2,nhiY2,ninfY2,ngoodY2
	if(discprt) write(8,184) nloY2,nhiY2,ninfY2,ngoodY2
184	format(
     & ' Number of runs in which',/,
     & ' Lower m=2.0 limit for Ymax is invalid  = ',i5,/,
     & ' Upper m=2.0 limit for Ymax is invalid  = ',i5,/,
     & ' Upper m=2.0 limit for Ymax is infinite = ',i5,/,
     & ' Both m=2.0 limits for Ymax are valid   = ',i5,/)
c
	print 16,nsdK,pk1,pk3,pk2,pk4
	if(pon()) write(7,16) nsdK,pk1,pk3,pk2,pk4
	if(discprt) write(8,16) nsdK,pk1,pk3,pk2,pk4
16	format(' Number of runs with valid SD for K = ',i7,/,
     & ' Error analysis for K values: fraction of runs in which',/,
     & '  Ktrue is included in +/- 1 SD = ',g13.6,/,
     & '  Ktrue is included in m=0.5 unit limits = ',g13.6,/,
     & '  Ktrue is included in +/- 2 SD = ',g13.6,/,
     & '  Ktrue is included in m=2.0 unit limits = ',g13.6,/)
c
	print 17,nsdY,py1,py3,py2,py4
	if(pon()) write(7,17) nsdY,py1,py3,py2,py4
	if(discprt) write(8,17) nsdY,py1,py3,py2,py4
17	format(' Number of runs with valid SD for Ymax = ',i7,/,
     & ' Error analysis for Ymax values: fraction of runs in which',/,
     & '  Ymax true is included in +/- 1 SD = ',g13.6,/,
     & '  Ymax true is included in m=0.5 unit limits = ',g13.6,/,
     & '  Ymax true is included in +/- 2 SD = ',g13.6,/,
     & '  Ymax true is included in m=2.0 unit limits = ',g13.6,/)
c
	print 173,nall1
	if(pon()) write(7,173) nall1
	if(discprt) write(8,173) nall1
173	format(
     & ' In ',i5,' expts, both 2*SD and m=2 limits exclude true K',/,
     & '    AND both sets of limits exclude Ymax also',/)
c
	print 171,nkboth,nkexcs,nkexcm
	if(pon()) write(7,171) nkboth,nkexcs,nkexcm
	if(discprt) write(8,171) nkboth,nkexcs,nkexcm
171	format(
     & ' In ',i5,' expts, both 2*SD and m=2 limits exclude true K',/,
     & ' In ',i5,' expts, 2*SD limits exclude true K, but not m=2',/,
     & ' In ',i5,' expts, m=2 limits exclude true K, but not 2*SD',/)
c  breakdown of nkboth
	print 174,nkboth1,nkboth2,nkboth3,nkboth4
	if(pon()) write(7,174) nkboth1,nkboth2,nkboth3,nkboth4
	if(discprt) write(8,174) nkboth1,nkboth2,nkboth3,nkboth4
174	format(
     &' In experiments in which both sets of limits exclude true K:',/,
     &'  in ',i5,' expts, Ktrue is above Khat+2*SD',/,
     &'  in ',i5,' expts, Ktrue is below Khat-2*SD',/,
     &'  in ',i5,' expts, Ktrue is above Khigh (m=2)',/,
     &'  in ',i5,' expts, Ktrue is below Klow (m=2)')
c  breakdown of nkexcs
	print 175,nkexcs1,nkexcs2
	if(pon()) write(7,175) nkexcs1,nkexcs2
	if(discprt) write(8,175) nkexcs1,nkexcs2
175	format(
     &' In expts in which 2*SD limits exclude true K, but not m=2',/,
     &'  in ',i5,' expts, Ktrue is above Khat+2*SD',/,
     &'  in ',i5,' expts, Ktrue is below Khat-2*SD')
c  breakdown of nkexcm
	print 176,nkexcm1,nkexcm2
	if(pon()) write(7,176) nkexcm1,nkexcm2
	if(discprt) write(8,176) nkexcm1,nkexcm2
176	format(
     &' In expts in which m=2.0 limits exclude true K, but not 2*SD',/,
     &'  in ',i5,' expts, Ktrue is above Khigh (m=2)',/,
     &'  in ',i5,' expts, Ktrue is below Klow (m=2)',/)
c Now repeat all of above for limits that exclude Ymax
	print 172,nyboth,nyexcs,nyexcm
	if(pon()) write(7,172) nyboth,nyexcs,nyexcm
	if(discprt) write(8,172) nyboth,nyexcs,nyexcm
172	format(
     & ' In ',i5,' expts, both 2*SD and m=2 limits exclude true Ymax',/,
     & ' In ',i5,' expts, 2*SD limits exclude true Ymax, but not m=2',/,
     & ' In ',i5,' expts, m=2 limits exclude true Ymax, but not 2*SD',/)
c  breakdown of nkboth
	print 177,nyboth1,nyboth2,nyboth3,nyboth4
	if(pon()) write(7,177) nyboth1,nyboth2,nyboth3,nyboth4
	if(discprt) write(8,177) nyboth1,nyboth2,nyboth3,nyboth4
177	format(
     &' In experiments in which both sets of limits exclude true Ym:',/,
     &'  in ',i5,' expts, Ytrue is above Yhat+2*SD',/,
     &'  in ',i5,' expts, Ytrue is below Yhat-2*SD',/,
     &'  in ',i5,' expts, Ytrue is above Yhigh (m=2)',/,
     &'  in ',i5,' expts, Ytrue is below Ylow (m=2)')
c  breakdown of nyexcs
	print 178,nyexcs1,nyexcs2
	if(pon()) write(7,178) nyexcs1,nyexcs2
	if(discprt) write(8,178) nyexcs1,nyexcs2
178	format(
     &' In expts in which 2*SD limits exclude true Ymax, but not m=2',/,
     &'  in ',i5,' expts, Ytrue is above Yhat+2*SD',/,
     &'  in ',i5,' expts, Ytrue is below Yhat-2*SD')
c  breakdown of nyexcm
	print 179,nyexcm1,nyexcm2
	if(pon()) write(7,179) nyexcm1,nyexcm2
	if(discprt) write(8,179) nyexcm1,nyexcm2
179	format(
     &' In expts in which m=2 limits exclude true Ymax, but not 2*SD',/,
     &'  in ',i5,' expts, Ytrue is above Yhigh (m=2)',/,
     &'  in ',i5,' expts, Ytrue is below Ylow (m=2)',/)
c
c Print width/asymmetry
	print 21
	if(pon()) write(7,21)
	if(discprt) write(8,21)
21	format(/,' SD(K) from observed information matrix')
	ymin=0.		!valid range for input
	ymax=1.e10
	call VARV2(sdK,nrun,ybar,vary,ymin,ymax,nbad)
	n1=nrun-nbad
c	call VARV1(sdK,nsdK,YBAR,VARY,ymin,ymax)
	print 413,n1,YBAR,sqrt(vary),ymin,ymax
	if(pon()) write(7,413) n1,YBAR,sqrt(vary),ymin,ymax
	if(discprt) write(8,413) n1,YBAR,sqrt(vary),ymin,ymax
413	FORMAT(' Mean and SD of ',I5,' values= ',G13.6,' +/-',g13.6,
     & /,' Range from ',g13.6,' to ',g13.6)
	print 211
	if(pon()) write(7,211)
	if(discprt) write(8,211)
211	format(/,' SD(K) from expected information matrix')
	ymin=0.		!valid range for input
	ymax=1.e10
	call VARV2(sdKe,nrun,ybar,vary,ymin,ymax,nbad)
	n1=nrun-nbad
c	call VARV1(sdKe,nsdKe,YBAR,VARY,ymin,ymax)
	print 413,n1,YBAR,sqrt(vary),ymin,ymax
	if(pon()) write(7,413) n1,YBAR,sqrt(vary),ymin,ymax
	if(discprt) write(8,413) n1,YBAR,sqrt(vary),ymin,ymax
	print 22
	if(pon()) write(7,22)
	if(discprt) write(8,22)
22	format(/,' SD(Ymax) from observed information matrix')
	ymin=0.		!valid range for input
	ymax=1.e10
	call VARV2(sdY,nrun,ybar,vary,ymin,ymax,nbad)
	n1=nrun-nbad
c	call VARV1(sdY,nsdY,YBAR,VARY,ymin,ymax)
	print 413,n1,YBAR,sqrt(vary),ymin,ymax
	if(pon()) write(7,413) n1,YBAR,sqrt(vary),ymin,ymax
	if(discprt) write(8,413) n1,YBAR,sqrt(vary),ymin,ymax
	print 221
	if(pon()) write(7,221)
	if(discprt) write(8,221)
221	format(/,' SD(Ymax) from expected information matrix')
	ymin=0.		!valid range for input
	ymax=1.e10
	call VARV2(sdYe,nrun,ybar,vary,ymin,ymax,nbad)
	n1=nrun-nbad
c	call VARV1(sdYe,nsdYe,YBAR,VARY,ymin,ymax)
	print 413,n1,YBAR,sqrt(vary),ymin,ymax
	if(pon()) write(7,413) n1,YBAR,sqrt(vary),ymin,ymax
	if(discprt) write(8,413) n1,YBAR,sqrt(vary),ymin,ymax
c Summarise correlations (NB set to -2 if bad)
	ymin=-1.		!valid range for input
	ymax=1.
	call VARV2(cor,nrun,ybar,vary,ymin,ymax,nbad)
	n1=nrun-nbad
	print 59,n1,ybar,sqrt(vary),ymin,ymax
	if(pon()) write(7,59) n1,ybar,sqrt(vary),ymin,ymax
	if(discprt) write(8,59) n1,ybar,sqrt(vary),ymin,ymax
59	format(/,
     & ' Valid correlation coeffs. for hyperbola fit (obs info)',/,
     & ' Mean and SD of ',i5,' values= ',g13.6,' +/-',g13.6,
     & /,' Range from ',g13.6,' to ',g13.6,/)
c Summarise correlations (NB set to -2 if bad)
	ymin=-1.		!valid range for input
	ymax=1.
	call VARV2(core,nrun,ybar,vary,ymin,ymax,nbad)
	n1=nrun-nbad
	print 590,n1,ybar,sqrt(vary),ymin,ymax
	if(pon()) write(7,590) n1,ybar,sqrt(vary),ymin,ymax
	if(discprt) write(8,590) n1,ybar,sqrt(vary),ymin,ymax
590	format(/,
     & ' Valid correlation coeffs. for hyperbola fit (exp info)',/,
     & ' Mean and SD of ',i5,' values= ',g13.6,' +/-',g13.6,
     & /,' Range from ',g13.6,' to ',g13.6,/)
c
	print 23
	if(pon()) write(7,23)
	if(discprt) write(8,23)
23	format(/,' Width of m=0.5 interval for K')
	call VARV1(widlK1,nwK1,YBAR,VARY,ymin,ymax)
	print 413,nwK1,YBAR,sqrt(vary),ymin,ymax
	if(pon()) write(7,413) nwK1,YBAR,sqrt(vary),ymin,ymax
	if(discprt) write(8,413) nwK1,YBAR,sqrt(vary),ymin,ymax

	print 24
	if(pon()) write(7,24)
	if(discprt) write(8,24)
24	format(/,' Asymmetry of m=0.5 interval for K (high/low)')
	call VARV1(asymK1,nwK1,YBAR,VARY,ymin,ymax)
	print 413,nwK1,YBAR,sqrt(vary),ymin,ymax
	if(pon()) write(7,413) nwK1,YBAR,sqrt(vary),ymin,ymax
	if(discprt) write(8,413) nwK1,YBAR,sqrt(vary),ymin,ymax

	print 25
	if(pon()) write(7,25)
	if(discprt) write(8,25)
25	format(/,' Width of m=0.5 interval for Ymax')
	call VARV1(widlY1,nwY1,YBAR,VARY,ymin,ymax)
	print 413,nwY1,YBAR,sqrt(vary),ymin,ymax
	if(pon()) write(7,413) nwY1,YBAR,sqrt(vary),ymin,ymax
	if(discprt) write(8,413) nwY1,YBAR,sqrt(vary),ymin,ymax

	print 26
	if(pon()) write(7,26)
	if(discprt) write(8,26)
26	format(/,' Asymmetry of m=0.5 interval for Ymax (high/low)')
	call VARV1(asymY1,nwY1,YBAR,VARY,ymin,ymax)
	print 413,nwY1,YBAR,sqrt(vary),ymin,ymax
	if(pon()) write(7,413) nwY1,YBAR,sqrt(vary),ymin,ymax
	if(discprt) write(8,413) nwY1,YBAR,sqrt(vary),ymin,ymax

	print 27
	if(pon()) write(7,27)
	if(discprt) write(8,27)
27	format(/,' Width of m=2.0 interval for K')
	call VARV1(widlK2,nwK2,YBAR,VARY,ymin,ymax)
	print 413,nwK2,YBAR,sqrt(vary),ymin,ymax
	if(pon()) write(7,413) nwK2,YBAR,sqrt(vary),ymin,ymax
	if(discprt) write(8,413) nwK2,YBAR,sqrt(vary),ymin,ymax

	print 28
	if(pon()) write(7,28)
	if(discprt) write(8,28)
28	format(/,' Asymmetry of m=2.0 interval for K (high/low)')
	call VARV1(asymK2,nwK2,YBAR,VARY,ymin,ymax)
	print 413,nwK2,YBAR,sqrt(vary),ymin,ymax
	if(pon()) write(7,413) nwK2,YBAR,sqrt(vary),ymin,ymax
	if(discprt) write(8,413) nwK2,YBAR,sqrt(vary),ymin,ymax

	print 29
	if(pon()) write(7,29)
	if(discprt) write(8,29)
29	format(/,' Width of m=2.0 interval for Ymax')
	call VARV1(widlY2,nwY2,YBAR,VARY,ymin,ymax)
	print 413,nwY2,YBAR,sqrt(vary),ymin,ymax
	if(pon()) write(7,413) nwY2,YBAR,sqrt(vary),ymin,ymax
	if(discprt) write(8,413) nwY2,YBAR,sqrt(vary),ymin,ymax

	print 30
	if(pon()) write(7,30)
	if(discprt) write(8,30)
30	format(/,' Asymmetry of m=2.0 interval for Ymax (high/low)')
	call VARV1(asymY2,nwY2,YBAR,VARY,ymin,ymax)
	print 413,nwY2,YBAR,sqrt(vary),ymin,ymax
	if(pon()) write(7,413) nwY2,YBAR,sqrt(vary),ymin,ymax
	if(discprt) write(8,413) nwY2,YBAR,sqrt(vary),ymin,ymax
c
	print 301
	if(pon()) write(7,301)
	if(discprt) write(8,301)
301	format(/)
c
	DEALLOCATE(sdK,sdY,aKlo,aKhi,Ylo,Yhi,cor)
	DEALLOCATE(aKlo2,aKhi2,Ylo2,Yhi2)
	DEALLOCATE(widlK1,widlK2,asymK1,asymK2)
	DEALLOCATE(widlY1,widlY2,asymY1,asymY2)
	DEALLOCATE(sdKe,sdYe,core)
c
c
c NOW ANALYSE ERRORS FOR LS FIT OF HILL EQUATION
	nbig=0
	nsdK=0
	nsdY=0
	nsdn=0
	nsdKe=0
	nsdYe=0
	nsdne=0
	nK1=0
	nK2=0
	nY1=0
	nY2=0
	nn1=0
	nn2=0
	nK1e=0
	nK2e=0
	nY1e=0
	nY2e=0
	nn1e=0
	nn2e=0
	do irun=1,nrun
c	  Check for enormous values (error estimation skipped)
	   aK1=aK(irun,6)
	   Ym1=Ym(irun,6)
	   hill1=hill(irun)
	   if(aK1.gt.aKmax.or.Ym1.gt.Ymmax.or.hill1.gt.hillmax) then
		nbig=nbig+1
		goto 201	!skip errors
	   endif
c
c   Check whether limits include true K
c	 First SD from obs info
	   sdK1=sdKh(irun)
	   if(sdK1.gt.0.) then
		nsdK=nsdK+1
	      if(aK1+sdK1.ge.aKtrue.and.aK1-sdK1.le.aKtrue) nk1=nk1+1
	      sdk2=tval*sdk1		!tval=1.96 except when residuals used
	      if(aK1+sdK2.ge.aKtrue.and.aK1-sdK2.le.aKtrue) nk2=nk2+1
	   endif
c	 SD from exp info
	   sdK1=sdKhe(irun)
	   if(sdK1.gt.0.) then
		nsdKe=nsdKe+1
	      if(aK1+sdK1.ge.aKtrue.and.aK1-sdK1.le.aKtrue) nk1e=nk1e+1
	      sdk2=tval*sdk1		!tval=1.96 except when residuals used
	      if(aK1+sdK2.ge.aKtrue.and.aK1-sdK2.le.aKtrue) nk2e=nk2e+1
	   endif
c
c	 SD from obs info
	   sdY1=sdYh(irun)
	   if(sdY1.gt.0.) then
		nsdY=nsdY+1
		if(Ym1+sdY1.ge.Ymtrue.and.Ym1-sdY1.le.Ymtrue) ny1=ny1+1
		sdY2=tval*sdY1	!tval=1.96 except when residuals used
		if(Ym1+sdY2.ge.Ymtrue.and.Ym1-sdY2.le.Ymtrue) ny2=ny2+1
	   endif
c	 SD from exp info
	   sdY1=sdYhe(irun)
	   if(sdY1.gt.0.) then
		nsdYe=nsdYe+1
		if(Ym1+sdY1.ge.Ymtrue.and.Ym1-sdY1.le.Ymtrue) ny1e=ny1e+1
		sdY2=tval*sdY1	!tval=1.96 except when residuals used
		if(Ym1+sdY2.ge.Ymtrue.and.Ym1-sdY2.le.Ymtrue) ny2e=ny2e+1
	   endif
c
c	 SD from obs info
	   sdn1=sdnh(irun)
	   if(sdn1.gt.0.) then
		nsdn=nsdn+1
		if(hill1+sdn1.ge.hilltrue.and.
     &		hill1-sdn1.le.hilltrue) nn1=nn1+1
		sdn2=tval*sdn1	!tval=1.96 except when residuals used
		if(hill1+sdn2.ge.hilltrue.and.
     &		hill1-sdn2.le.hilltrue) nn2=nn2+1
	   endif
	   sdn1=sdnhe(irun)
	   if(sdn1.gt.0.) then
		nsdne=nsdne+1
		if(hill1+sdn1.ge.hilltrue.and.
     &		hill1-sdn1.le.hilltrue) nn1e=nn1e+1
		sdn2=tval*sdn1	!tval=1.96 except when residuals used
		if(hill1+sdn2.ge.hilltrue.and.
     &		hill1-sdn2.le.hilltrue) nn2e=nn2e+1
	   endif
201	   continue
	enddo
c
c Print error analysis for Hill fits
	print 185,nrun,nbig,aKmax,Ymmax,hillmax
	if(pon()) write(7,185) nrun,nbig,aKmax,Ymmax,hillmax
	if(discprt) write(8,185) nrun,nbig,aKmax,Ymmax,hillmax
185	format(/,' ERROR ANALYSIS FOR FIT OF HILL EQUATION',/,
     & ' Total number of runs = ',i8,/,
     &' Number in which big estimates preclude error estimation = ',i8,/
     &,' (big means K >',f8.1,' or Ymax > ',f8.1,' or nH > ',f8.1,')')
c
	print 21
	if(pon()) write(7,21)
	if(discprt) write(8,21)
c21	format(/,' SD(K) from observed information matrix')
	ymin=0.		!valid range for input
	ymax=1.e10
	call VARV2(sdKh,nrun,ybar,vary,ymin,ymax,nbad)
	n1=nrun-nbad
c	call VARV1(sdKh,nsdK,YBAR,VARY,ymin,ymax)
	print 413,n1,YBAR,sqrt(vary),ymin,ymax
	if(pon()) write(7,413) n1,YBAR,sqrt(vary),ymin,ymax
	if(discprt) write(8,413) n1,YBAR,sqrt(vary),ymin,ymax
c413	FORMAT(' Mean and SD of ',I5,' values= ',G13.6,' +/-',g13.6,
c     & /,' Range from ',g13.6,' to ',g13.6)
	print 211
	if(pon()) write(7,211)
	if(discprt) write(8,211)
c211	format(/,' SD(K) from expected information matrix')
	ymin=0.		!valid range for input
	ymax=1.e10
	call VARV2(sdKhe,nrun,ybar,vary,ymin,ymax,nbad)
	n1=nrun-nbad
c	call VARV1(sdKhe,nsdKe,YBAR,VARY,ymin,ymax)
	print 413,n1,YBAR,sqrt(vary),ymin,ymax
	if(pon()) write(7,413) n1,YBAR,sqrt(vary),ymin,ymax
	if(discprt) write(8,413) n1,YBAR,sqrt(vary),ymin,ymax
	print 22
	if(pon()) write(7,22)
	if(discprt) write(8,22)
c22	format(/,' SD(Ymax) from observed information matrix')
	ymin=0.		!valid range for input
	ymax=1.e10
	call VARV2(sdYh,nrun,ybar,vary,ymin,ymax,nbad)
	n1=nrun-nbad
c	call VARV1(sdYh,nsdY,YBAR,VARY,ymin,ymax)
	print 413,n1,YBAR,sqrt(vary),ymin,ymax
	if(pon()) write(7,413) n1,YBAR,sqrt(vary),ymin,ymax
	if(discprt) write(8,413) n1,YBAR,sqrt(vary),ymin,ymax
	print 221
	if(pon()) write(7,221)
	if(discprt) write(8,221)
c221	format(/,' SD(Ymax) from expected information matrix')
	ymin=0.		!valid range for input
	ymax=1.e10
	call VARV2(sdYhe,nrun,ybar,vary,ymin,ymax,nbad)
	n1=nrun-nbad
c	call VARV1(sdYhe,nsdYe,YBAR,VARY,ymin,ymax)
	print 413,n1,YBAR,sqrt(vary),ymin,ymax
	if(pon()) write(7,413) n1,YBAR,sqrt(vary),ymin,ymax
	if(discprt) write(8,413) n1,YBAR,sqrt(vary),ymin,ymax
	print 222
	if(pon()) write(7,222)
	if(discprt) write(8,222)
222	format(/,' SD(nH) from observed information matrix')
	ymin=0.		!valid range for input
	ymax=1.e10
	call VARV2(sdnh,nrun,ybar,vary,ymin,ymax,nbad)
	n1=nrun-nbad
c	call VARV1(sdnh,nsdn,YBAR,VARY,ymin,ymax)
	print 413,n1,YBAR,sqrt(vary),ymin,ymax
	if(pon()) write(7,413) n1,YBAR,sqrt(vary),ymin,ymax
	if(discprt) write(8,413) n1,YBAR,sqrt(vary),ymin,ymax
	print 223
	if(pon()) write(7,223)
	if(discprt) write(8,223)
223	format(/,' SD(nH) from expected information matrix')
	ymin=0.		!valid range for input
	ymax=1.e10
	call VARV2(sdnhe,nrun,ybar,vary,ymin,ymax,nbad)
	n1=nrun-nbad
c	call VARV1(sdnhe,nsdne,YBAR,VARY,ymin,ymax)
	print 413,n1,YBAR,sqrt(vary),ymin,ymax
	if(pon()) write(7,413) n1,YBAR,sqrt(vary),ymin,ymax
	if(discprt) write(8,413) n1,YBAR,sqrt(vary),ymin,ymax
c
c Summarise correlations (NB set to -2 if bad)
	ymin=-1.		!valid range for input
	ymax=1.
	call VARV2(corYK,nrun,ybar,vary,ymin,ymax,nbad)
	n1=nrun-nbad
	print 591,n1,ybar,sqrt(vary),ymin,ymax
	if(pon()) write(7,591) n1,ybar,sqrt(vary),ymin,ymax
	if(discprt) write(8,591) n1,ybar,sqrt(vary),ymin,ymax
591	format(/,
     & ' Valid correlation coeffs. for Hill fit (Ymax-K) (obs inf)',/,
     & ' Mean and SD of ',i5,' values= ',g13.6,' +/-',g13.6,
     & /,' Range from ',g13.6,' to ',g13.6,/)
	ymin=-1.		!valid range for input
	ymax=1.
	call VARV2(corYKe,nrun,ybar,vary,ymin,ymax,nbad)
	n1=nrun-nbad
	print 5911,n1,ybar,sqrt(vary),ymin,ymax
	if(pon()) write(7,5911) n1,ybar,sqrt(vary),ymin,ymax
	if(discprt) write(8,5911) n1,ybar,sqrt(vary),ymin,ymax
5911	format(/,
     & ' Valid correlation coeffs. for Hill fit (Ymax-K) (exp info)',/,
     & ' Mean and SD of ',i5,' values= ',g13.6,' +/-',g13.6,
     & /,' Range from ',g13.6,' to ',g13.6,/)
c Summarise correlations (NB set to -2 if bad)
	ymin=-1.		!valid range for input
	ymax=1.
	call VARV2(corYn,nrun,ybar,vary,ymin,ymax,nbad)
	n1=nrun-nbad
	print 592,n1,ybar,sqrt(vary),ymin,ymax
	if(pon()) write(7,592) n1,ybar,sqrt(vary),ymin,ymax
	if(discprt) write(8,592) n1,ybar,sqrt(vary),ymin,ymax
592	format(/,
     & ' Valid correlation coeffs. for Hill fit (Ymax-nH) (obs info)',/,
     & ' Mean and SD of ',i5,' values= ',g13.6,' +/-',g13.6,
     & /,' Range from ',g13.6,' to ',g13.6,/)
	ymin=-1.		!valid range for input
	ymax=1.
	call VARV2(corYne,nrun,ybar,vary,ymin,ymax,nbad)
	n1=nrun-nbad
	print 5921,n1,ybar,sqrt(vary),ymin,ymax
	if(pon()) write(7,5921) n1,ybar,sqrt(vary),ymin,ymax
	if(discprt) write(8,5921) n1,ybar,sqrt(vary),ymin,ymax
5921	format(/,
     & ' Valid correlation coeffs. for Hill fit (Ymax-nH) (exp info)',/,
     & ' Mean and SD of ',i5,' values= ',g13.6,' +/-',g13.6,
     & /,' Range from ',g13.6,' to ',g13.6,/)
c Summarise correlations (NB set to -2 if bad)
	ymin=-1.		!valid range for input
	ymax=1.
	call VARV2(corKn,nrun,ybar,vary,ymin,ymax,nbad)
	n1=nrun-nbad
	print 593,n1,ybar,sqrt(vary),ymin,ymax
	if(pon()) write(7,593) n1,ybar,sqrt(vary),ymin,ymax
	if(discprt) write(8,593) n1,ybar,sqrt(vary),ymin,ymax
593	format(/,
     & ' Valid correlation coeffs. for Hill fit (K-nH) (obs info)',/,
     & ' Mean and SD of ',i5,' values= ',g13.6,' +/-',g13.6,
     & /,' Range from ',g13.6,' to ',g13.6,/)
	ymin=-1.		!valid range for input
	ymax=1.
	call VARV2(corKne,nrun,ybar,vary,ymin,ymax,nbad)
	n1=nrun-nbad
	print 5931,n1,ybar,sqrt(vary),ymin,ymax
	if(pon()) write(7,5931) n1,ybar,sqrt(vary),ymin,ymax
	if(discprt) write(8,5931) n1,ybar,sqrt(vary),ymin,ymax
5931	format(/,
     & ' Valid correlation coeffs. for Hill fit (K-nH) (exp info)',/,
     & ' Mean and SD of ',i5,' values= ',g13.6,' +/-',g13.6,
     & /,' Range from ',g13.6,' to ',g13.6,/)
c
	DEALLOCATE(sdKh,sdYh,sdnh,corYK,corYn,corKn)
	DEALLOCATE(sdKhe,sdYhe,sdnhe,corYKe,corYne,corKne)
c
	pk1=float(nk1)/float(nsdK)
	pk2=float(nk2)/float(nsdK)
	py1=float(ny1)/float(nsdY)
	py2=float(ny2)/float(nsdY)
	pn1=float(nn1)/float(nsdn)
	pn2=float(nn2)/float(nsdn)
	pk1e=float(nk1e)/float(nsdKe)
	pk2e=float(nk2e)/float(nsdKe)
	py1e=float(ny1e)/float(nsdYe)
	py2e=float(ny2e)/float(nsdYe)
	pn1e=float(nn1e)/float(nsdne)
	pn2e=float(nn2e)/float(nsdne)
	print 161,nsdK,pk1,pk2
	if(pon()) write(7,161) nsdK,pk1,pk2
	if(discprt) write(8,161) nsdK,pk1,pk2
161	format(' Number of runs with valid SD for K (obs info) = ',i7,/,
     & ' Error analysis for K values: fraction of runs in which',/,
     & '  K true is included in +/- 1 SD = ',g13.6,/,
     & '  K true is included in +/- 2 SD = ',g13.6,/)
	print 1611,nsdKe,pk1e,pk2e
	if(pon()) write(7,1611) nsdKe,pk1e,pk2e
	if(discprt) write(8,1611) nsdKe,pk1e,pk2e
1611	format(' Number of runs with valid SD for K (exp info) = ',i7,/,
     & ' Error analysis for K values: fraction of runs in which',/,
     & '  K true is included in +/- 1 SD = ',g13.6,/,
     & '  K true is included in +/- 2 SD = ',g13.6,/)
c
	print 162,nsdY,py1,py2
	if(pon()) write(7,162) nsdY,py1,py2
	if(discprt) write(8,162) nsdY,py1,py2
162	format(' No. of runs with valid SD for Ymax (obs info) = ',i7,/,
     & ' Error analysis for Ymax values: fraction of runs in which',/,
     & '  Ymax true is included in +/- 1 SD = ',g13.6,/,
     & '  Ymax true is included in +/- 2 SD = ',g13.6,/)
	print 1621,nsdYe,py1e,py2e
	if(pon()) write(7,1621) nsdYe,py1e,py2e
	if(discprt) write(8,1621) nsdYe,py1e,py2e
1621	format(' No. of runs with valid SD for Ymax (exp info) = ',i7,/,
     & ' Error analysis for Ymax values: fraction of runs in which',/,
     & '  Ymax true is included in +/- 1 SD = ',g13.6,/,
     & '  Ymax true is included in +/- 2 SD = ',g13.6,/)
c
	print 163,nsdn,pn1,pn2
	if(pon()) write(7,163) nsdn,pn1,pn2
	if(discprt) write(8,163) nsdn,pn1,pn2
163	format(' No. of runs with valid SD for nHill (obs info) = ',i7,/,
     & ' Error analysis for nHill values: fraction of runs in which',/,
     & '  nHill true is included in +/- 1 SD = ',g13.6,/,
     & '  nHill true is included in +/- 2 SD = ',g13.6,/)
	print 1631,nsdne,pn1e,pn2e
	if(pon()) write(7,1631) nsdne,pn1e,pn2e
	if(discprt) write(8,1631) nsdne,pn1e,pn2e
1631	format(' No. of runs with valid SD for nHill (exp info) = ',i7,/,
     & ' Error analysis for nHill values: fraction of runs in which',/,
     & '  nHill true is included in +/- 1 SD = ',g13.6,/,
     & '  nHill true is included in +/- 2 SD = ',g13.6,/)
c
c
c Display histogram of estimates
	call HYPDISP(aK,Ym,hill,nrun,aKmax,Ymmax,hillmax,
     & ndx,aKexcb,aKexcs,aKexcm,aYexcb,aYexcs,aYexcm,
     & nkboth,nkexcs,nkexcm,nyboth,nyexcs,nyexcm)
c
	DEALLOCATE(aK,Ym)
	DEALLOCATE(aKexcb,aKexcs,aKexcm,aYexcb,aYexcs,aYexcm)
c
	if(.not.test) call RANDSK(ix,iy,iz,1,repeat)	!write IX,IY,IZ
	call ENDPRINT
	end

	function SSDHYP(k,theta)
	real theta(20)
	common/data1/n,c(100),yobs(100),w(100)	!for SSDHYP
c Reset to positive if nec
	if(theta(1).lt.1.e-21) theta(1)=1.e-21
	if(theta(2).lt.1.e-21) theta(2)=1.e-21
c
	aK=theta(1)
	Ym=theta(2)
	s=0.0
	do i=1,n
	   ycalc=Ym*c(i)/(c(i)+aK)
	   d=yobs(i)-ycalc
	   s=s + w(i)*d*d
	enddo
	SSDHYP=s
	RETURN
	end

	function SSDHILL(k,theta)
	real theta(20)
	common/data1/n,c(100),yobs(100),w(100)	!for SSDHYP
c Reset to positive if nec
	if(theta(1).lt.1.e-21) theta(1)=1.e-21
	if(theta(2).lt.1.e-21) theta(2)=1.e-21
	if(theta(3).lt.1.e-21) theta(3)=1.e-21
c
	aK=theta(1)
	Ym=theta(2)
	hill=theta(3)
	s=0.0
	do i=1,n
	   c1=(c(i)/aK)**hill
	   ycalc=Ym*c1/(1.0+c1)
	   d=yobs(i)-ycalc
	   s=s + w(i)*d*d
	enddo
	SSDHILL=s
	RETURN
	end

	subroutine LINREG(x,y,n,a,b)
c To return a,b for conventional linear regression
	real*4 x(n),y(n)
c
	en=float(n)
	sx=0.
	sxx=0.
	sy=0.
	syy=0.
	sxy=0.
	do i=1,n
	   sx=sx+x(i)
	   sxx=sxx+x(i)*x(i)
	   sy=sy+y(i)
	   syy=syy+y(i)*y(i)
	   sxy=sxy+x(i)*y(i)
	enddo
	sxx=sxx - (sx*sx)/en
	sxy=sxy - (sx*sy)/en
c
	b=sxy/sxx
	xbar=sx/en
	a=(sy/en)-b*xbar
	RETURN
	end

