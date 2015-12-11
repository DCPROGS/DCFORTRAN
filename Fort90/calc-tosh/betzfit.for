	program BETZFIT
c Modification of HYPFIT to test discrimination of 1 vs 2 intermediates
c in stoichiometry study (Laube, Kuhse & Betz, 1998)
c Fits same expts with (a) one intermediate
c and (b) 2 intermediates
c
c Modif 05/10/01 05:55pm by addition of common /fiteq/ieqn
c ieqn=1 for fit of eqn used by Betz for random subunit assoc
c ieqn=2 for fit of sum of Hill equations

c Hypfit notes
c To simulate fitting of Hill-Langmuir hyperbola as in LOB
c===To be added -Lik intervals for all three param of Hill fits!
c=== (still linear in Ymax!)
c=== also add option to omit some calcs for speed (esp LI for Hill fit!)
	real c(100),ptrue(100),w(100),sd(100),yobs(100)
	allocatable pvalue,aicdif,apair,hill2
	real*4 pvalue(:),aicdif(:),apair(:),hill2(:)
	character*7 ptitle3(10),ptitle4(10)
c  for varv3
c	allocatable yval1,ibad
c	real*4 yval1(:)
c	integer*1 ibad(:)
c
	common/rand/ix,iy,iz
	common/data1/n,c,yobs,w		!for SSDHYP, HYPERR
	common/data2/ Scrit,Scrit1,Scrit2		!for hyplik/funclo/hi
	logical conamp
	common/data3/ncomp
	common/run/irun
	common/fiteq/ieqn,conamp
c For simplex4
	logical abort
	real theta(20),thetrue(20),theta3(20),theta4(20)
	real*4 lowval(20),highval(20),pertval(20)		!parameter
	integer jfix(20),jfix1(20),jfix2(20)
	common/abt/abort,ikey
c
	logical pon,slock,debug,caplock,test,deb,repeat
	logical sdata,useres,posresp
	character*11 cdate,ctime
	character*1 ans,UC
c
	logical discprt
	common/dp/discprt
	character*40 mtitle*40,filnam*32,prtport*4      !for WINPRINT
	common/dpp/filnam,prtport,ndisc,jcol,mtitle !for WINPRINT,ENDPRINT,DISCNUM
	common/lims/lowval,highval
	EXTERNAL ssdbetz
C
c define function
	pon()=slock()
	debug()=caplock()
c
	filnam='BETZFIT.PRT'
	call WINPRINT   !print file control
      OPEN(unit=7,file=prtport,iostat=nerr)             !open printer
	print 1001
	if(pon()) write(7,1001)
	if(discprt) write(8,1001)
1001	FORMAT(' Program to simulate Betz stoichiometry expt.',/,
     & '  or fitting of Hill/Langmuir',/)
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
c
c	maxfac=20.	!exclude estimates > maxfac*true value
c
	test=.false.
c	print 40
c40	format(
c     & ' Use test data (LOB example) [N] ? ')
c	call INPUTa(ans)
c	if(ans.eq.'Y') then
c	   aKtrue=15.
c	   Ymtrue=30.
c	   hilltrue=1.	!fixed for fit
c	   sres1=1.20027
c	   n=5
c	   do i=1,n
c		sd(i)=sres1
c		w(i)=1./sres1**2
c	   enddo
c	   iwt=0
c	   c(1)=2.5
c         yobs(1)=5.576
c	   c(2)=5.
c	   yobs(2)=7.282
c	   c(3)=10.
c	   yobs(3)=12.521
c	   c(4)=20.
c	   yobs(4)=16.138
c	   c(5)=40.
c	   yobs(5)=23.219
c	   test=.true.
c	   nrun=1
c	   do i=1,n
c		ptrue(i)=Ymtrue*c(i)/(c(i)+aKtrue)
c	   enddo
cc	   tval=ttable(n-2)
c	   call TVALUE(n-2,tval)	!get 95% value of t
c	   iwt=1
c	   useres=.false.
c	   print 41
c	   if(pon()) write(7,41)
c	   if(discprt) write(8,41)
c41	   format(
c     &	' Test data from LOB.',/,
c     &	'   w(i)=Sres for hyperbola fit; w(i)=1.0 for Hill fit')
c	   goto 10
c	endif
c
c Define true values
c Define max values so estimates greater than this (e.g very high Ymax from
c crazy expt) cause error estimation to be skipped.
c	aKmax=maxfac*aKtrue
c	Ymmax=maxfac*Ymtrue
c	hillmax=maxfac*hilltrue
c Define X values=conc
c Space them logarithmically between !cmin and cmax
	n=10
	print 1,n
1	format(' Number of concentrations [',i2,'] = ')
	call INPUTi(n)
	cmin=1.0
	print 2,cmin
2	format(' Smallest conc [',f10.3,'] = ')
	call INPUTr(cmin)
	cmax=1000.
	print 3,cmax
3	format('&Largest conc [',f10.3,'] = ')
	call INPUTr(cmax)
	dl=(alog10(cmax)-alog10(cmin))/float(n-1)
	do i=1,n
	   c(i)=10**(alog10(cmin)+float(i-1)*dl)
	enddo
c
c Calculate the true Y values in ptrue()

43	continue
	ieqn=2
	print 421,ieqn
421	Format(
     & ' (1) Fit Betz-type stoichiometry Hill plots',/,
     & ' (2) Fit normal Hill/Langmuir plots',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(ieqn)
c
	if(ieqn.eq.1) then
	   print 42
42	   Format(
     & ' (1) True number of components (3 or 4) = ')
	   call INPUTi(nctrue)
	   if(nctrue.ne.3.and.nctrue.ne.4) then
		call BELL(1)
		goto 43
	   endif
	   print 12,nctrue,nctrue-2
	   if(discprt) write(8,12) nctrue,nctrue-2
12	   format(
     & ' True number of components = ',i3,' (',i2,' intermediates)')
	   ptitle3(1)='   Ymax'
	   ptitle3(2)='EC50(1)'
	   ptitle3(3)='Hill(1)'
	   ptitle3(4)='EC50(2)'
	   ptitle3(5)='Hill(2)'
	   ptitle3(6)='EC50(3)'
	   ptitle3(7)='Hill(3)'
	   ptitle3(8)='Pbinom'
	   ptitle4(1)='   Ymax'
	   ptitle4(2)='EC50(1)'
	   ptitle4(3)='Hill(1)'
	   ptitle4(4)='EC50(2)'
	   ptitle4(5)='Hill(2)'
	   ptitle4(6)='EC50(3)'
	   ptitle4(7)='Hill(3)'
	   ptitle4(8)='EC50(4)'
	   ptitle4(9)='Hill(4)'
	   ptitle4(10)='Pbinom'
	   kmax3=8
	   kmax4=10
	   if(nctrue.eq.3) then
		Ymtrue=1.0
		theta(1)=Ymtrue
		ncomp=3
		ak1true=1.5	!wildtype
		H1true=1.5	!wildtype
		ak2true=23.3	!intermed
		H2true=1.5	!intermed
		ak3true=363.	!all mut
		H3true=1.5	!all mut
		Pbtrue=0.5
		theta(1)=Ymtrue
		theta(2)=aK1true	!init guess=true value
		theta(3)=H1true
		theta(4)=aK2true	!init guess=true value
		theta(5)=H2true
		theta(6)=aK3true	!init guess=true value
		theta(7)=H3true
		theta(8)=Pbtrue
		kmax=8
	   else if(nctrue.eq.4) then
		Ymtrue=1.0
		theta(1)=Ymtrue
		ncomp=4
		ak1true=1.5	!wildtype
		H1true=1.5	!wildtype
		ak2true=9.3	!intermed1
		H2true=1.5	!intermed1
		ak3true=58.3	!intermed2
		H3true=1.5	!intermed2
		ak4true=363.	!all mut
		H4true=1.5	!all mut
		Pbtrue=0.5
		theta(1)=Ymtrue
		theta(2)=aK1true	!init guess=true value
		theta(3)=H1true
		theta(4)=aK2true	!init guess=true value
		theta(5)=H2true
		theta(6)=aK3true	!init guess=true value
		theta(7)=H3true
		theta(8)=aK4true	!init guess=true value
		theta(9)=H4true
		theta(10)=Pbtrue
		kmax=10
	   endif
	   kmtrue=kmax
	   do i=1,kmax
		thetrue(i)=theta(i)
	   enddo
	else if(ieqn.eq.2) then
	   nctrue=2	!default
	   print 62,nctrue
62	   Format(
     & ' (1) True number of components [',i2,'] = ')
	   call INPUTi(nctrue)
c	   if(nctrue.ne.1.and.nctrue.ne.2) then
c		call BELL(1)
c		goto 43
c	   endif
	   print 63,nctrue
	   if(discprt) write(8,63) nctrue
63	   format(
     & ' True number of components = ',i3)
c use ptitle3 for 1 comp, ptitle4 for 2 comp
	   ptitle3(1)='Ymax(1)'
	   ptitle3(2)='EC50(1)'
	   ptitle3(3)='Hill(1)'
	   ptitle3(4)='Ymax(2)'
	   ptitle3(5)='EC50(2)'
	   ptitle3(6)='Hill(2)'
	   ptitle3(7)='Ymax(3)'
	   ptitle3(8)='EC50(3)'
	   ptitle3(9)='Hill(3)'
c=	   if(nctrue.eq.1) then
		Ymtrue=1.0
		theta(1)=Ymtrue
		ncomp=1
		ak2true=30.7	!triplet fitted when only 1 component fitted!
		H2true=1.36		!triplet
c=		ak1true=0.75	!pair
c=		H1true=0.92		!pair
		theta(1)=Ymtrue
		theta(2)=aK2true	!init guess=true value
		theta(3)=H2true
		kmax=3
		kmax1=kmax
		kmax3=kmax		!for display
c   Set which to be fixed
		print 65
65		format(/,' For fit of one component:')
	     jfix1(1)=1	!defaults
	     jfix1(2)=0
	     jfix1(3)=0
		nfix=0
		do i=1,kmax
		   print 66,i,ptitle3(i),theta(i)
66		   format(
     &	'  Parameter ',i3,'; ',a10,' [',f8.3,'] = ')
		   call INPUTr(theta(i))
		   if(jfix1(i).eq.1) then
			print 68
68			format('&This parameter fixed: OK [Y] ? ')
		   else if(jfix1(i).eq.0) then
			print 69
69			format('&This parameter free: OK [Y] ? ')
		   endif
		   ans='Y'
		   call INPUTa(ans)
		   if(ans.eq.'N') then
			if(jfix1(i).eq.0) then
			   jfix1(i)=1
			else if(jfix1(i).eq.1) then
			   jfix1(i)=0
			endif
		   endif
		   if(jfix1(i).eq.1) nfix=nfix+1
		   nfix1=nfix
		enddo
		Ymtrue=theta(1)
		aK2true=theta(2)
		H2true=theta(3)
		do i=1,3
		   theta3(i)=theta(i)	!for print
		enddo
		npar1=kmax1-nfix1
c	   else if(nctrue.eq.2) then
	   if(nctrue.eq.2) then
		print 67
67		format(/,' For fit of two components:')
		ncomp=2
		print 64
64		format(' Fraction of component 1 (pair) = ')
		call INPUTr(pp)
c=		Ymtrue=1.0
c=		theta(1)=Ymtrue
		amp1=pp
c For fit of 2 comp when nctrue=2
		theta(1)=amp1	!pair
		ak1true=0.75	!pair
		H1true=0.92		!pair
		ak2true=30.7	!triplet
		H2true=1.36		!triplet
		amp2=1.0-pp		!triplet
		ans='Y'
		call DCASK('Constrain total amplitude to 1.0',ans,ans)
		conamp=ans.eq.'Y'
		if(.not.conamp) then
		   kmax=6
		   ptitle4(1)='Ymax(1)'
		   ptitle4(2)='EC50(1)'
		   ptitle4(3)='Hill(1)'
		   ptitle4(4)='Amp(2)'
		   ptitle4(5)='EC50(2)'
		   ptitle4(6)='Hill(2)'
		   theta(1)=amp1
		   theta(2)=aK1true	!init guess=true value
		   theta(3)=H1true
		   theta(4)=amp2	!init guess=true value
		   theta(5)=aK2true	!init guess=true value
		   theta(6)=H2true
		   jfix2(1)=0
		   jfix2(2)=1	!for default, fix all but amplitude
		   jfix2(3)=1
		   jfix2(4)=0
		   jfix2(5)=0
		   jfix2(6)=0
		else if(conamp) then
		   kmax=5
		   ptitle4(1)='Ymax(1)'
		   ptitle4(2)='EC50(1)'
		   ptitle4(3)='Hill(1)'
		   ptitle4(4)='EC50(2)'
		   ptitle4(5)='Hill(2)'
		   theta(1)=amp1
		   theta(2)=aK1true	!init guess=true value
		   theta(3)=H1true
		   theta(4)=aK2true	!init guess=true value
		   theta(5)=H2true
		   jfix2(1)=0
		   jfix2(2)=1	!for default, fix all but amplitude
		   jfix2(3)=1
		   jfix2(4)=0
		   jfix2(5)=0
		endif
		kmax2=kmax
		kmax4=kmax		!for display
c   Set which to be fixed
		nfix=0
		do i=1,kmax
		   print 66,i,ptitle4(i),theta(i)
c66		   format(
c     &	'  Parameter ',i3,'; ',a10,' [',f8.3,'] = ')
		   call INPUTr(theta(i))
		   if(jfix2(i).eq.1) then
			print 68
c68			format('&This parameter fixed: OK [Y] ? ')
		   else if(jfix2(i).eq.0) then
			print 69
c69			format('&This parameter free: OK [Y] ? ')
		   endif
		   ans='Y'
		   call INPUTa(ans)
		   if(ans.eq.'N') then
			if(jfix2(i).eq.0) then
			   jfix2(i)=1
			else if(jfix2(i).eq.1) then
			   jfix2(i)=0
			endif
		   endif
		   if(jfix2(i).eq.1) nfix=nfix+1
		   nfix2=nfix
		enddo
		npar2=kmax2-nfix2
	   endif
	   kmtrue=kmax
	   do i=1,kmax
		thetrue(i)=theta(i)
	   enddo
c Print the conditions
	   if(discprt) then
		write(8,65)
c65		format(/,' Initial guesses for fit of one component:')
		do i=1,kmax1
		   if(jfix1(i).eq.0) then
			write(8,70) i,ptitle3(i),theta3(i)
70			format(
     &		' Parameter ',i2,'  ',a10,' = ',g13.6,' (free)')
		   else if(jfix1(i).eq.1) then
			write(8,71) i,ptitle3(i),theta3(i)
71			format(
     &		' Parameter ',i2,'  ',a10,' = ',g13.6,' (fixed)')
		   endif
		enddo
		write(8,67)
c67		format(/,' Initial guesses for fit of two components:')
		do i=1,kmax2
		   if(jfix2(i).eq.0) then
			write(8,70) i,ptitle4(i),theta(i)
c70			format(
c     &		' Parameter ',i2,'  ',a10,' = ',g13.6,' (free)')
		   else if(jfix2(i).eq.1) then
			write(8,71) i,ptitle4(i),theta(i)
c71			format(
c     &		' Parameter ',i2,'  ',a10,' = ',g13.6,' (fixed)')
		   endif
		enddo
		if(conamp) then
		   write(8,72)
72		   format(
     &       ' Amplitudes of 2 component fit constrained to add to 1',/)
		endif
	   endif
	endif		!end of ieqn=2

c Now calculate ptrue()
	do i=1,n
	   ycalc=BCALC(c(i),nctrue,thetrue,kmax)
	   ptrue(i)=ycalc
	enddo
c	print 12,aktrue,Ymtrue
c	if(pon()) write(7,12) aktrue,Ymtrue
c	if(discprt) write(8,12) aktrue,Ymtrue
c12	format(' True K = ',g13.6,' True Ymax = ',g13.6)
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
	if(ieqn.eq.2.and.n.eq.10.and.cmin.eq.1.) then
	   print 511
511	   format(
     & ' (1) Constant SD (as specified)',/,
     & ' (2) SD = a + b*Ytrue',/,
     & ' (3) as (2) but weight from Yobs',/,
     & ' (4) use tabulated weights',/,
     & ' Option number [1] = ')
	else
	   print 5
5	   format(
     & ' (1) Constant SD (as specified)',/,
     & ' (2) SD = a + b*Ytrue',/,
     & ' (3) as (2) but weight from Yobs',/,
     & ' Option number [1] = ')
	endif
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
	else if(iwt.eq.4) then
	   sd(1)=0.002
	   sd(2)=0.005
	   sd(3)=0.015
	   do i=4,10
		sd(i)=0.02
	   enddo
	   do i=1,n
		w(i)=1.0/(sd(i)*sd(i))
	   enddo
	endif
c	if(sdata.and.iwt.ne.4) then
	if(sdata) then
	   print 53
	   sdata=.false.
53	   format(
     & ' Use true weighting for LS analysis [Y] ? ')
	   ans='Y'
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
	print 731
	if(discprt) write(8,731)
731	format(/,' True values ',/,
     &'       x               y             SD')
	do i=1,n
	   print 741,c(i),ptrue(i),sd(i)
	   if(discprt) write(8,741) c(i),ptrue(i),sd(i)
741	   format(g13.6,2x,g13.6,2x,g13.6)
	enddo
	print 301
	if(discprt) write(8,301)
301	format(/)
c
	ans='Y'
	call DCASK('Reset negative responses to zero',ans,ans)
	posresp=ans.eq.'Y'
c
c	if(useres) then
c	   call TVALUE(n-2,tval)	!get 95% value of t
c	else
c	   tval=1.960
c	endif
c	em=tval*tval/2.
c	print 58,tval,n-2,em
c	if(pon()) write(7,58) tval,n-2,em
c	if(discprt) write(8,58) tval,n-2,em
c58    format(' NOTE:  '
c     &'''2*SD'' means t*SD where t = ',f9.4,' (P=0.95 and ',i2,' d.f.)'
c     &,/,
c     &' and ''m=2'' means corresponding value for m = t*t/2 = ',f9.4,/)
c
c
	nrun=1000
	print 4,nrun
4	format(/,' Number of ''experiments to be simulated [',i5,'] = ')
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
	ALLOCATE(pvalue(nrun),aicdif(nrun),apair(nrun),hill2(nrun))
	n05=0
	n01=0
	n001=0
	naic=0
c
	do irun=1,nrun
	   deb=debug()
	   if(test) goto 11
c Simulate the observations:
	   do i=1,n
		u=ranorm()
		yobs(i)=u*sd(i)+ptrue(i)
		if(posresp) then
		   if(yobs(i).lt.0.0) yobs(i)=0.0
		endif
		if(iwt.eq.3) then		!replace theoretical weight by obs
		   sdobs=aval + bval*yobs(i)
		   w(i)=1.0/(sdobs*sdobs)
		endif
	   enddo
c Now have 'observations' in c(i),yobs(i) -fit them with sum of either
c 3 or 4 Hill equations (also Langmuir?)
c
11	   continue
c
15	   continue
	   if(ieqn.eq.1) then
c (1) Fit Hill equations with ncomp=3 -set guesses and fixed param
		Ymtrue=1.0
		theta(1)=Ymtrue
		ncomp=3
		ak1true=1.5	!wildtype
		H1true=1.5	!wildtype
		ak2true=23.3	!intermed
		H2true=1.5	!intermed
		ak3true=363.	!all mut
		H3true=1.5	!all mut
		theta(1)=Ymtrue
		theta(2)=aK1true	!init guess=true value
		theta(3)=H1true
		theta(4)=aK2true	!init guess=true value
		theta(5)=H2true
		theta(6)=aK3true	!init guess=true value
		theta(7)=H3true
		theta(8)=Pbtrue
		kmax=8
		kmax3=kmax
		do i=1,kmax
		   jfix(i)=0
		enddo
		jfix(2)=1	!fix ak1
		jfix(3)=1	!fix nH1
		jfix(6)=1	!fix ak3
		jfix(7)=1	!fix nH3
		nfix=4
		do i=1,kmax
		   pertval(i)=0.02
		   lowval(i)=1.e-12
		   highval(i)=1.e4	!e.g. 10 mM
		enddo
		highval(5)=8.		!max Hill coeff (to prevent underflow)
c Re-set weights to 1.0 for each run now in case where error is from residuals
		if(useres.or.test) then
		   do i=1,n
			w(i)=1.0
		   enddo
		endif
	   else if(ieqn.eq.2) then
c (1) Fit Hill equations with ncomp=1 -set guesses and fixed param
		ncomp=1
		nfix=nfix1
		kmax=kmax1
		do i=1,kmax1
		  jfix(i)=jfix1(i)
		enddo
		theta(1)=Ymtrue
		theta(2)=aK2true	!init guess=true value
		theta(3)=H2true
c     No need to fit if all paramters fixed
		do i=1,kmax
		   pertval(i)=0.02
		   lowval(i)=1.e-12
		enddo
		highval(1)=10.		!max amplitude
		highval(2)=1.e4		!max EC50
		highval(3)=6.		!max Hill coeff (to prevent underflow)
c Re-set weights to 1.0 for each run now in case where error is from residuals
		if(useres.or.test) then
		   do i=1,n
			w(i)=1.0
		   enddo
		endif
	   endif	!end of ieqn=1,2
c
	   kfit=kmax-nfix
	   npar1=kfit
	   npar3=kfit
	   if(ieqn.eq.2.and.nfix1.eq.3) then
		Smin=SSDBETZ(kmax,theta)
		GOTO 661
	   endif
	   ndisp=-1000		!silent
	   nevmax=10000
	   errfac=1.e-4
	   delmin=-1.		!do not use delmin for convergence
	   confac=0.5
	   resfac=1.0
	   irestrt=3
	   stpfac=0.1
	   iconv=0		!output, but set in case it is bad
	   call SIMPLEX4(kmax,THETA,stpfac,errfac,neval,nevmax,
     &    smin,SSDBETZ,Ndisp,jfix,delmin,confac,irestrt,resfac,iconv,
     &    lowval,highval,pertval)
661	   continue		!jump to here if all params fixed for last bit
	   do i=1,kmax
		theta3(i)=theta(i)	!save for display
	   enddo
	   if(abort) then
c
	   endif
c Record max likelihood for the likelihood ratio test
	   twopi=6.283185
	   if(useres) then
		en=float(n)
		sres2=Smin/en	!ML estimate of Sres-squared
		almax3=0.5*(en*alog(twopi*Sres2) + 1.)
	   else
		t1=0.0	!first term of likelihood
		do i=1,n
		   t1=t1+alog(twopi/w(i))
		enddo
		almax3=-0.5*(t1+Smin)	!this is weighted Smin
	   endif
c	now Akaike
	   np=npar3
	   if(ieqn.eq.2) np=npar1
	   aic3=-2.*almax3 + 2.0*float(np)
c	   aK1=theta(1)
c	   Ym1=theta(2)
c	   hill1=theta(3)
c	   aK(irun,im)=aK1
c	   Ym(irun,im)=Ym1
c	   hill(irun)=hill1
c	   var=smin/float(n-3)		!n-3 df for Hill fit
c	   Sresh(irun)=sqrt(var)
c In case where error is from residuals, weights were set to 1 for
c fitting, but now set weights for error analysis using Smin.  If these
c weights had been used for fitting, all deviations would have been
c multiplied by 1/sres**2, so must multiply Smin by this too, to get
c the value equivalent to Lmax = -SSDmin/(2*var)
c	   if(useres) then
c		do i=1,n
c		   w(i)=1.0/var
c		enddo
c		Smin=Smin/var
c	   endif
c	   if(aK1.gt.aKmax.or.Ym1.gt.Ymmax.or.hill1.gt.hillmax) goto 151	!skip errors
c     Approx SD and likelihood intervals?
c Get SD from observed information matrix for Hill fit
c	   call HILLVAR(Ym1,aK1,hill1,sdK1,sdY1,sdn1,corYK1,corYn1,corKn1)
c	   sdKh(irun)=sdK1
c	   sdYh(irun)=sdY1
c	   sdnh(irun)=sdn1
c	   corYK(irun)=corYK1
c	   corYn(irun)=corYn1
c	   corKn(irun)=corKn1
cc Get SD from expected information matrix for Hill fit
c	   call HILLVARe(Ym1,aK1,hill1,sdK1,sdY1,sdn1,
c     &		corYK1,corYn1,corKn1)
c	   sdKhe(irun)=sdK1
c	   sdYhe(irun)=sdY1
c	   sdnhe(irun)=sdn1
c	   corYKe(irun)=corYK1
c	   corYne(irun)=corYn1
c	   corKne(irun)=corKn1
151	   continue
c
c
c Repeat whole thing with ncomp=4 (ieqn=1) or ncomp=2 (ieqn=2)
c (1) Fit Hill equations with ncomp=3
	   if(ieqn.eq.1) then
		Ymtrue=1.0
		theta(1)=Ymtrue
		ncomp=4
		ak1true=1.5	!wildtype
		H1true=1.5	!wildtype
		ak2true=9.3	!intermed1
		H2true=1.5	!intermed1
		ak3true=58.3	!intermed2
		H3true=1.5	!intermed2
		ak4true=363.	!all mut
		H4true=1.5	!all mut
		theta(1)=Ymtrue
		theta(2)=aK1true	!init guess=true value
		theta(3)=H1true
		theta(4)=aK2true	!init guess=true value
		theta(5)=H2true
		theta(6)=aK3true	!init guess=true value
		theta(7)=H3true
		theta(8)=aK4true	!init guess=true value
		theta(9)=H4true
		theta(10)=Pbtrue
		kmax=10
		kmax4=kmax
		do i=1,kmax
		   jfix(i)=0
		enddo
		jfix(2)=1	!fix ak1
		jfix(3)=1	!fix nH1
		jfix(8)=1	!fix ak4
		jfix(9)=1	!fix nH4
		nfix=4
		do i=1,kmax
	  	   pertval(i)=0.02
		   lowval(i)=1.e-12
		   highval(i)=1.e4	!e.g. 10 mM
		enddo
		highval(5)=8.		!max Hill coeff (to prevent underflow)
		highval(7)=8.		!max Hill coeff (to prevent underflow)
	   else if(ieqn.eq.2) then
c (1) Fit Hill equations with ncomp=2 -set guesses and fixed param
		ncomp=2
		nfix=nfix2
		kmax=kmax2
		do i=1,kmax2
		  jfix(i)=jfix2(i)
		enddo
		if(.not.conamp) then
		   theta(1)=amp1
		   theta(2)=aK1true	!init guess=true value
		   theta(3)=H1true
		   theta(4)=amp2
		   theta(5)=aK2true	!init guess=true value
		   theta(6)=H2true
		   do i=1,kmax
			pertval(i)=0.02
			lowval(i)=1.e-12
		   enddo
		   highval(1)=10.		!max amplitude
		   highval(2)=1.e4		!max EC50
		   highval(3)=6.		!max Hill coeff (to prevent underflow)
		   highval(4)=10.		!max amplitude
		   highval(5)=1.e4	!max EC50
		   highval(6)=6.		!max Hill coeff (to prevent underflow)
		else if(conamp) then
		   theta(1)=amp1
		   theta(2)=aK1true	!init guess=true value
		   theta(3)=H1true
		   theta(4)=aK2true	!init guess=true value
		   theta(5)=H2true
		   do i=1,kmax
			pertval(i)=0.02
			lowval(i)=1.e-12
		   enddo
		   highval(1)=10.		!max amplitude
		   highval(2)=1.e4		!max EC50
		   highval(3)=6.		!max Hill coeff (to prevent underflow)
		   highval(4)=1.e4	!max EC50
		   highval(5)=6.		!max Hill coeff (to prevent underflow)
		endif
c Re-set weights to 1.0 for each run now in case where error is from residuals
		if(useres.or.test) then
		   do i=1,n
			w(i)=1.0
		   enddo
		endif
	   endif	!end of ieqn=2
c
	   kfit=kmax-nfix
	   npar2=kfit
	   npar4=kfit
c     No need to fit if all parameters fixed
	   if(nfix2.eq.6) then
	      Smin=SSDBETZ(kmax,theta)
	      GOTO 662
	   endif
	   ndisp=-1000		!silent
	   nevmax=10000
	   errfac=1.e-4
	   delmin=-1.		!do not use delmin for convergence
	   confac=0.5
	   resfac=1.0
	   irestrt=3
	   stpfac=0.1
c Re-set weights to 1.0 for each run now in case where error is from residuals
	   if(useres.or.test) then
		do i=1,n
		   w(i)=1.0
		enddo
	   endif
	   call SIMPLEX4(kmax,THETA,stpfac,errfac,neval,nevmax,
     &    smin,SSDBETZ,Ndisp,jfix,delmin,confac,irestrt,resfac,iconv,
     &    lowval,highval,pertval)
662	   continue		!jump to here if all params fixed for last bit
	   do i=1,kmax
		theta4(i)=theta(i)	!save for display
	   enddo
	   if(ieqn.eq.2.and.nctrue.eq.2) then
		apair(irun)=theta(1)
		if(conamp) then
		   hill2(irun)=theta(5)
		else
		   hill2(irun)=theta(6)
		endif
	   endif
	   if(abort) then
c
	   endif
c Record max likelihood for the likelihood ratio test
	   npar4=kfit
	   npar2=kfit
	   twopi=6.283185
	   if(useres) then
		en=float(n)
		sres2=Smin/en	!ML estimate of Sres-squared
		almax4=0.5*(en*alog(twopi*Sres2) + 1.)
	   else
		t1=0.0	!first term of likelihood
		do i=1,n
		   t1=t1+alog(twopi/w(i))
		enddo
		almax4=-0.5*(t1+Smin)	!this is weighted Smin
	   endif
c	now Akaike
	   np=npar4
	   if(ieqn.eq.2) np=npar2
	   aic4=-2.*almax4 + 2.0*float(np)
c  After fit of 4 comp can now do likelihood ratio test
	   if(almax3.gt.almax4) then
		Pvalue(irun)=1.01		!so appears in high bin
	   else
		acrit=2.0*(almax4-almax3)	!2* log-lik ratio
		if(ieqn.eq.1) then
		   ndf=npar4-npar3
		else
		   ndf=npar2-npar1
		endif
	      Pvalue(irun)=CHI2PROB(acrit,ndf)
	   endif
c Akaike -smaller value=better fit
	   AICdif(irun)=aic3-aic4	!pos if 4 comp better
c	   if(deb) then
		print 46,irun,almax3,almax4,pvalue(irun),
     &	  aic3,aic4,aicdif(irun)
		if(discprt) write(8,46) irun,almax3,almax4,pvalue(irun),
     &	  aic3,aic4,aicdif(irun)
46		format(1x,i5,3x,' Lmax3, Lmax4 = ',2g12.5,' P = ',f8.5,/,
     &	  '     AIC3, AIC4 = ',2g12.5,' AICdiff ',g16.6)
c	   endif
	   if(pvalue(irun).lt.0.05) n05=n05+1
	   if(pvalue(irun).lt.0.01) n01=n01+1
	   if(pvalue(irun).lt.0.001) n001=n001+1
	   if(aicdif(irun).gt.0.0) naic=naic+1

c Display the two fits?
c
	   if(deb) then	!print expt
		ans='N'
		call DCASK('Plot this ''experiment''',ans,ans)
		if(ans.eq.'Y') then
		   do i=1,kmax3
			print 47,i,ptitle3(i),theta3(i)
			if(discprt) write(8,47) i,ptitle3(i),theta3(i)
47			format(1x,i3,2x,a7,2x,g13.6)
		   enddo
		   do i=1,kmax4
			print 47,i,ptitle4(i),theta4(i)
			if(discprt) write(8,47) i,ptitle4(i),theta4(i)
		   enddo
		   if(ieqn.eq.2) then
		      kmax3=kmax1
		      kmax4=kmax2
		   endif
		   call BDISP(pvalue(irun),irun,nrun,c,yobs,w,n,cmin,cmax,
     &	    nctrue,thetrue,theta3,theta4,kmtrue,kmax3,kmax4,ieqn)
		endif
	   endif
c
c	   aK1=theta(1)
c	   Ym1=theta(2)
c	   hill1=theta(3)
c	   aK(irun,im)=aK1
c	   Ym(irun,im)=Ym1
c	   hill(irun)=hill1
c	   var=smin/float(n-3)		!n-3 df for Hill fit
c	   Sresh(irun)=sqrt(var)
c In case where error is from residuals, weights were set to 1 for
c fitting, but now set weights for error analysis using Smin.  If these
c weights had been used for fitting, all deviations would have been
c multiplied by 1/sres**2, so must multiply Smin by this too, to get
c the value equivalent to Lmax = -SSDmin/(2*var)
c	   if(useres) then
c		do i=1,n
c		   w(i)=1.0/var
c		enddo
c		Smin=Smin/var
c	   endif
c	   if(aK1.gt.aKmax.or.Ym1.gt.Ymmax.or.hill1.gt.hillmax) goto 151	!skip errors
c     Approx SD and likelihood intervals?
c Get SD from observed information matrix for Hill fit
c	   call HILLVAR(Ym1,aK1,hill1,sdK1,sdY1,sdn1,corYK1,corYn1,corKn1)
c	   sdKh(irun)=sdK1
c	   sdYh(irun)=sdY1
c	   sdnh(irun)=sdn1
c	   corYK(irun)=corYK1
c	   corYn(irun)=corYn1
c	   corKn(irun)=corKn1
c Get SD from expected information matrix for Hill fit
c	   call HILLVARe(Ym1,aK1,hill1,sdK1,sdY1,sdn1,
c     &		corYK1,corYn1,corKn1)
c	   sdKhe(irun)=sdK1
c	   sdYhe(irun)=sdY1
c	   sdnhe(irun)=sdn1
c	   corYKe(irun)=corYK1
c	   corYne(irun)=corYn1
c	   corKne(irun)=corKn1
c151	   continue
c
c
c	   if(deb) then	!print expt
c		call HYPEXPT(irun,nrun,n,c,yobs,w,aK,Ym,hill,sdK,sdY,cor,
c     &       aKlo,aKhi,Ylo,Yhi,aKlo2,aKhi2,Ylo2,Yhi2,aKtrue,Ymtrue,
c     &	 aKmax,Ymmax,hillmax,dans)
c	   endif
c
	enddo		!end of irun loop
c
	enrun=float(nrun)
	p05=float(n05)/enrun
	p01=float(n01)/enrun
	p001=float(n001)/enrun
	paic=float(naic)/enrun

	if(ieqn.eq.1) then
	   ncomp4=4
	else if(ieqn.eq.2) then
	   ncomp4=2
	endif
	print 45,p05,p01,p001,ncomp4,paic
	if(discprt) write(8,45) p05,p01,p001,ncomp4,paic
45	format(/,' Summary',/,
     & ' Fraction of P values below 0.05  = ',f8.5,/,
     & ' Fraction of P values below 0.01  = ',f8.5,/,
     & ' Fraction of P values below 0.001 = ',f8.5,/,
     & ' Fraction in which AIC indicates ',i2,
     &' components fit better  = ',f8.5,/)
c
c Display histogram of estimates
	call BETZDISP(pvalue,aicdif,apair,hill2,nrun,ieqn)
c
	if(.not.test) call RANDSK(ix,iy,iz,1,repeat)	!write IX,IY,IZ
	DEALLOCATE(pvalue,aicdif,apair,hill2)
	call ENDPRINT
	end


	function SSDBETZ(kmax,theta)
c Modif of hypfit version for sum of ncomp components
c
	real theta(20)
	logical conamp
	common/data1/n,c(100),yobs(100),w(100)	!for SSDHYP
	common/data3/ncomp
	common/fiteq/ieqn,conamp
	real*4 lowval(20),highval(20)
	common/lims/lowval,highval
c
c Reset to positive if nec
c 	do i=1,kmax
c	   if(theta(i).lt.1.e-21) theta(i)=1.e-21
c	enddo
	do i=1,kmax
	   if(theta(i).lt.lowval(i)) theta(i)=lowval(i)
	   if(theta(i).gt.highval(i)) theta(i)=highval(i)
	enddo
c Parameters are
c ieqn=1: overall Ymax, then K, nH in sequence for each component
c ieqn=2: amp, EC50, nH in sequence except last amp missing if
c		conamp true
	s=0.0
	do i=1,n
	   ycalc=BCALC(c(i),ncomp,theta,kmax)
	   d=yobs(i)-ycalc
	   s=s + w(i)*d*d
	enddo
	SSDBETZ=s
	RETURN
	end

