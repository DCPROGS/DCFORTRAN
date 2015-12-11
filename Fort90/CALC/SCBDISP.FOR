	subroutine SCBDISP(areasav,tausav,ncompsav,idtype,nsav,
     &  docond,dok,dokr,irmax,ndim,ndtmax)
c
c To do displays of pdfs in SCBURST
c Distribution types defined by idtype()
c Declare idtype(3,j) where
c idtype(1,j)=distribution type
c idtype(2,j)=condition variable 1 (=0 if unconditional)
c idtype(3,j)=condition variable 2 (=0 if unconditional)
c
c idtype(1,j)=1 for number of openings per burst
c 	idtype(1:3,j)=
c	1,i,0 for ops/burst conditional on start in open state #i (docond=T)
c idtype(1,j)=2 for pdf of open times
c	2,0,0 for unconditional open time
c	2,-1,0 for first opening in burst with 2 or more openings
c	2,i,0 for open time conditional on start in open state #i (docond=T)
c	2,0,k for kth open time in any burst (dok=T)
c	2,r,k for kth open time in burst with r openings (dokr=T)
c idtype(1,j)=3 for pdf of shut times
c	3,0,0 for all shut times
c	3,i,0 for shut time conditional on start in open state #i (docond=T)
c	3,0,k for kth shut time in any burst (dok=T)
c	3,r,k for kth shut time in burst with r openings (dokr=T)
c idtype(1,j)=4 for pdf of burst lengths
c	4,0,0 for all burst lengths
c	4,-1,0 for burst lengths for bursts with 2 or more openings
c idtype(1,j)=5 for pdf of total open time per burst
c	5,0,0 for all total open/burst
c	5,i,0 for total open/bst conditional on start in open state #i (docond=T)
c idtype(1,j)=6 for pdf of total shut time per burst
c	6,0,0 for all total shut/burst
c	6,i,0 for total shut/bst conditional on start in open state #i (docond=T)
c
	real*4 tausav(ndim,ndtmax),areasav(ndim,ndtmax)
	integer*4 ncompsav(ndtmax)
	integer*4 idtype(4,ndtmax)
	logical docond,dok,dokr
	character*11 cnum,cnum1
c for display
	logical allocated
	ALLOCATABLE::Xval,Yval,Xcal,Ycal
	real Xval(:,:),Yval(:,:)		!for VPLOT
	real Xcal(:,:),Ycal(:,:)		!for VPLOT
	ALLOCATABLE::tau,area
	real*4 tau(:),area(:)
c for data
	integer*4 ndat(1),icurvd(1),isym(1),ijoin(1)	!for data
	real*4 syms(1)				!for data
c for calc curves ndimc=2
	dimension ncal(2),icurvc(2),iline(2)	!for calc curve
	LOGICAL debug,fitted,doframe,plotonly,autplt
	character*40 titlex,titley
	character*64 title1
	logical caplock,landscap,ivplot,interp
	character*1 ans
	integer*4 icurvw(1)		!for VPLOT5
	real*4 wght(1,1)
	real*4 theta(200)
c
	logical discprt
	common/dp/discprt
	common/KBLK/kA,kB,kC,kD
	integer icol(100)
	logical mono
	COMMON/cols/icol,mono
c
c	integer*2 videotyp
c
c
c
c define function
	debug()=caplock()
c

c Use F90 routine to prevent underflow crashes??
c	errflag=.true.
c	call UNDFL(errflag)
c List distribution types that have been saved
c NB at least the 7 main uncondional types are defined, plus
c	2,kA,-1,0 for first opening in burst with 2 or more openings
c	4,kE+kA,-1,0 for burst lengths for bursts with 2 or more openings
c
c Misc settings for vplot
	do i=1,100
	   icol(i)=-1
	enddo
	isetcol=1	!use default colours except when icol set to valid colour
	plotonly=.false.
c
	fitted=.true.	!so parameters drawn
	k=kA+kB+kC+kD
c
c
	ndv1=1 		!xval not used
	ndimd=1
	ncalc=1024
	ndc1=ncalc
	ndimc=2
	if(allocated(xval)) deallocate(xval,yval,xcal,ycal)
	ALLOCATE(Xval(ndv1,ndimd),Yval(ndv1,ndimd))
	ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
	iscal=1		!scale internally
	inumx=-1		!X axis in fixed (Fn.d) format
	inumy=-1
c Multiply f(t) by 100 so scales better on sqrt scale
c	cbig=2.5
	cbig=2.0
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
	itrace=0	!no longer used!
	kwi=1		!dimension of weight()
	kwj=1		!dimension of weight()
c
	call BELL(1)
	itype=1
	id=1
50	continue 		!return for next plot
	print 1,itype
1	format(/,' Plot:',/,
     & ' (1) distribution of number of openings per burst',/,
     & ' (2) pdf of open times',/,
     & ' (3) pdf of shut times',/,
     & ' (4) pdf of burst lengths',/,
     & ' (5) pdf of total open time per burst',/,
     & ' (6) pdf of gaps within bursts',/,
     & ' (7) pdf of gaps between bursts',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(itype)
	id1=itype
c

c idtype(1,j)=1 for number of openings per burst
c 	idtype(1:3,j)=
c	1,i,0 for ops/burst conditional on start in open state #i (docond=T)
c idtype(1,j)=2 for pdf of open times
c	2,0,0 for unconditional open time
c	2,-1,0 for first opening in burst with 2 or more openings
c	2,i,0 for open time conditional on start in open state #i (docond=T)
c	2,0,k for kth open time in any burst (dok=T)
c	2,r,k for kth open time in burst with r openings (dokr=T)
c idtype(1,j)=3 for pdf of shut times
c	3,0,0 for all shut times
c	3,i,0 for shut time conditional on start in open state #i (docond=T)
c	3,0,k for kth shut time in any burst (dok=T)
c	3,r,k for kth shut time in burst with r openings (dokr=T)
c idtype(1,j)=4 for pdf of burst lengths
c	4,0,0 for all burst lengths
c	4,-1,0 for burst lengths for bursts with 2 or more openings
c	4,i,0 for burst lengths conditional on start in open state #i (docond=T)
c idtype(1,j)=5 for pdf of total open time per burst
c	5,0,0 for all total open/burst
c	5,i,0 for total open/bst conditional on start in open state #i (docond=T)
c idtype(1,j)=6 for pdf of total shut time per burst
c	6,0,0 for all total shut/burst
c	6,i,0 for total open/bst conditional on start in open state #i (docond=T)
c idtype(1,j)=7 for pdf of gaps between bursts
c	7,0,0 for all gaps between bursts
c
	SELECT CASE(id1)
	CASE(1)
	   goto 50		!until fixed
c====


	CASE(2)
c idtype(1,j)=2 for pdf of open times
c	2,0,0 for unconditional open time
c	2,-1,0 for first opening in burst with 2 or more openings
c	2,i,0 for open time conditional on start in open state #i (docond=T)
c	2,0,k for kth open time in any burst (dok=T)
c	2,r,k for kth open time in burst with r openings (dokr=T)
	   id1=2 	!for open states
	   print 21
21	   format(/,' Plot: ',/,
     & ' (1) pdf of all open times',/,
     & ' (2) pdf of first opening in burst with 2 or more openings')
	   if(docond) then
	      print 22
22	      format(
     & ' (3) pdf of open times conditional on start in open state')
	   endif
	   if(dok) then
	      print 23
23	      format(
     & ' (4) pdf of kth open time in any burst')
	   endif
	   if(dokr) then
	      print 24
24	      format(
     & ' (5) pdf of kth open time burst with r openings')
	   endif
	   print 25,id
25	   format(
     & ' Option number [',i2,'] = ')
	   call INPUTi(id)
	   if(id.eq.1) then
		id2=0
		id3=0
		titlex='open time'
	   else if(id.eq.2) then
		id2=-1
		id3=0
		titlex='first opening in bursts with > 1 opening'
	   else if(id.eq.3) then
		id2=1
		id3=0
		print 26,kA,id2
26		format(
     & ' open time conditional on start in open state (1-',i2,
     &	') [',i2,'] = ')
		call INPUTi(id2)
		titlex='open time Ý start in state '
		call INTCONV(id2,cnum)
		n=nblank1(titlex)
		titlex(n+2:n+3)=cnum(1:2)
	   else if(id.eq.4) then
c	2,0,k for kth open time in any burst (dok=T)
		id2=0
		id3=1
		print 27,irmax,id3
27		format(
     & ' kth open time in any burst (1-',i2,'); k [',i2,'] = ')
		call INPUTi(id3)
		titlex='kth open time in any burst, k = '
		call INTCONV(id3,cnum)
		n=nblank1(titlex)
		titlex(n+2:n+3)=cnum(1:2)
	   else if(id.eq.5) then
c	2,r,k for kth open time in burst with r openings (dokr=T)
		id2=1
		id3=1
		print 28,irmax,id2,id3
28		format(
     & ' kth open time in burst with r openings (1-',i2,'); k, r ['
     & ,i2,',',i2,'] = ')
		call INPUT2i(id3,id2)
		call INTCONV(id2,cnum)
		call INTCONV(id3,cnum1)
		titlex='opening '//cnum(1:3)//' in burst with '
     &		//cnum1(1:3)//' ops'
	   endif


	CASE(3)
c idtype(1,j)=3 for pdf of shut times
c	3,0,0 for all shut times
c	3,i,0 for shut time conditional on start in open state #i (docond=T)
c	3,0,k for kth shut time in any burst (dok=T)
c	3,r,k for kth shut time in burst with r openings (dokr=T)
	   id1=3 	!for shut states
	   print 3
3	   format(/,' Plot: ',/,
     & ' (1) pdf of all shut times')
c     & ' (2) pdf of first shut time in burst with 2 or more openings')
	   if(docond) then
	      print 31
31	      format(
     & ' (3) pdf of shut times conditional on start in shut state i')
	   endif
	   if(dok) then
	      print 32
32	      format(
     & ' (4) pdf of kth shut time in any burst')
	   endif
	   if(dokr) then
	      print 33
33	      format(
     & ' (5) pdf of kth shut time burst with r openings')
	   endif
	   print 34,id
34	   format(
     & ' Option number [',i2,'] = ')
	   call INPUTi(id)
	   if(id.eq.1) then
		id2=0
		id3=0
		titlex='shut time'
c	   else if(id.eq.2) then
c		id2=-1
c		id3=0
c		titlex='first shutting in bursts with > 1 opening'
	   else if(id.eq.3) then
		id2=1
		id3=0
		print 35,kA+1,k,id2
35		format(
     & ' shut time conditional on start in shut state (',i2,'-',i2,
     &	') [',i2,'] = ')
		call INPUTi(id2)
		titlex='shut time Ý start in state '
		call INTCONV(id2,cnum)
		n=nblank1(titlex)
		titlex(n+2:n+3)=cnum(1:2)
	   else if(id.eq.4) then
c	3,0,k for kth shut time in any burst (dok=T)
		id2=0
		id3=1
		print 36,irmax,id3
36		format(
     & ' kth shut time in any burst (1-',i2,'); k [',i2,'] = ')
		call INPUTi(id3)
		titlex='kth shut time in any burst, k = '
		call INTCONV(id3,cnum)
		n=nblank1(titlex)
		titlex(n+2:n+3)=cnum(1:2)
	   else if(id.eq.5) then
c	3,r,k for kth shut time in burst with r openings (dokr=T)
		id2=1
		id3=1
		print 37,irmax,id2,id3
37		format(
     & ' kth shut time in burst with r openings (1-',i2,'); k, r ['
     & ,i2,',',i2,'] = ')
		call INPUT2i(id3,id2)
		call INTCONV(id2,cnum)
		call INTCONV(id3,cnum1)
		titlex='shutting '//cnum(1:3)//' in burst with '
     &		//cnum1(1:3)//' ops'
	   endif

	CASE(4)
	   id1=4 	!for bursts
	   print 2
2	   format(/,' Plot: ',/,
     & ' (1) pdf of burst lengths',/,
     & ' (2) pdf of burst lengths for bursts with 2 or more openings')
c     &    ' Option number [',i2,'] = ')
	   if(docond) then		!conditional
	      print 41
41	      format(
     & ' (3) burst length conditional on start in open state i')
	   endif
	   print 25,id
c25	   format(' Option number [',i2,'] = ')
	   call INPUTi(id)
	   if(id.eq.1) then
		id2=0
		titlex='burst length'
	   else if(id.eq.2) then
		id2=-1
		titlex='burst length; bursts with > 1 opening'
	   else if(id.eq.3) then
		id2=1
		id3=0
		print 45,kA,id2
45		format(
     & ' burst length conditional on start in open state (1-',i2,
     &	') [',i2,'] = ')
		call INPUTi(id2)
		titlex='burst length Ý start in state '
		call INTCONV(id2,cnum)
		n=nblank1(titlex)
		titlex(n+2:n+3)=cnum(1:2)
	   endif

	CASE(5)
c		5,0,0 for all total open/burst
c		5,i,0 for total open/bst conditional on start in open state #i (docond=T)
	   id1=5 	!for total open/burst
	   id3=0
	   print 51
51	   format(/,' Plot: ',/,
     & ' (1) pdf of total open time per burst')
	   if(docond) then		!no conditional
	      print 52
52	      format(
     & ' (3) total open/bst conditional on start in open state i')
	   endif
	   print 25,id
c25	   format(' Option number [',i2,'] = ')
	   call INPUTi(id)
	   if(id.eq.1) then
		id2=0
		id3=0
		titlex='total open time/burst'
	   else if(id.eq.3) then
		id2=1
		id3=0
		print 55,kA,id2
55		format(
     & ' total open/burst conditional on start in open state (1-',i2,
     &	') [',i2,'] = ')
		call INPUTi(id2)
		titlex='total open/burst Ý start in state '
		call INTCONV(id2,cnum)
		n=nblank1(titlex)
		titlex(n+2:n+3)=cnum(1:2)
	   endif
	CASE(6)
c		6,0,0 for all total shut/burst
c		6,i,0 for total shut/bst conditional on start in open state #i (docond=T)
	   id1=6 	!for total shut/burst
	   print 60
60	   format(/,' Plot: ',/,
     & ' (1) pdf of total shut time per burst')
	   if(docond) then		!no conditional
	      print 61
61	      format(
     & ' (3) total shut/bst conditional on start in open state i')
	   endif
	   print 25,id
c25	   format(' Option number [',i2,'] = ')
	   call INPUTi(id)
	   if(id.eq.1) then
		id2=0
		titlex='total shut time/burst'
	   else if(id.eq.3) then
		id2=1
		id3=0
		print 65,kA,id2
65		format(
     & ' total shut/burst conditional on start in open state (1-',i2,
     &	') [',i2,'] = ')
		call INPUTi(id2)
		titlex='total shut/burst Ý start in state '
		call INTCONV(id2,cnum)
		n=nblank1(titlex)
		titlex(n+2:n+3)=cnum(1:2)
	   endif
	CASE(7)
c	7,0,0 for all gaps between bursts
	   id1=7
	   id2=0
	   id3=0
	   titlex='shut time between bursts'

	END SELECT
c
	call GETJ(id1,id2,id3,nsav,idtype,jout,ndtmax)
	if(jout.eq.0) then
	   call BELL(2)
	   print 80
80	   format(/,' Distribution not found: try again',/)
	   pause
	   goto 50
	endif
c
	ncomp=ncompsav(jout)
	if(allocated(tau)) deallocate(tau,area)
	ALLOCATE(tau(ncomp),area(ncomp))
	do i=1,ncomp
	   tau(i)=tausav(i,jout)	!in ms
	   area(i)=areasav(i,jout)
	enddo
c Now define f(t) and hence xcal, ycal etc for display
	ncalc=1024
c Put non-log values in Xcal(i,1),Ycal(i,1)
	xmin0=0.
	xmax=6.*tau(ncomp)	!milliseconds
	dx=(xmax-xmin)/float(ncalc)
	do i=1,ncalc
	   t=xmin0+float(i-1)*dx
	   xcal(i,1)=t
	   ft=0.0
	   do m=1,ncomp
		f=area(m)/tau(m)
		e=exp2(-t/tau(m))			!set to 0 if arg<-85
		if(e.gt.1.e-30/abs(f)) then		!add nothing if danger of underflow
		   ft= ft + f*e
		endif
	   enddo
	   ycal(i,1)=ft
	enddo
c
c Make log-spaced times for Xcal()
c Put log values in Xcal(i,2),Ycal(i,2)
	xmin=0.001		!ms
	xmax=6.*tau(ncomp)	!milliseconds
	dxlog=(alog10(XMAX)-alog10(xmin))/float(ncalc-1)  !increment
	do i=1,ncalc
	   x=alog10(xmin)+float(i-1)*dxlog	!this is log10 value
	   t=10.**x		!log10 taken in Vplot5
	   xcal(i,2)=t	!not log
	   ft=0.0
	   do m=1,ncomp
		f=area(m)/tau(m)
		e=exp2(-t/tau(m))			!set to 0 if arg<-85
		if(e.gt.1.e-30/abs(f)) then		!add nothing if danger of underflow
		   ft= ft + f*e
		endif
	   enddo
c Multiply f(t) by 100 so scales better on sqrt scale
	   ft=100.*ft*t*2.30259		!f(log10(t))=2.3*t*f(t)
	   ycal(i,2)=ft
	enddo
c
	iplot=1
101	print 10,iplot
10	format(
     & ' (1) Plot distribution of log(time)',/,
     & ' (2) Plot distribution of time',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(iplot)
	ncurvd=0
	ncurvc=1
	ncal(1)=ncalc
	ncal(2)=ncalc
	iline(1)=0
	iline(2)=0
	if(iplot.eq.1) then
	   ilog=6		!sqrt(y) vs log(x)
	   icurvc(1)=2	!plot ycal(i,2)
	   titley='100 f[log(t)] (sq root scale)'
	else if(iplot.eq.2) then
	   ilog=0
	   icurvc(1)=1	!plot ycal(i,1)
	   titley='prob density (1/s)'
	else
	   call BELL(1)
	   goto 101
	endif
c tau=theta(1,3,5,7,9)
c area=theta(2,4,6,8)
c IFITYPE=0 for no display of param
c IFITYPE=1 for time histos (THIST)
c IFITYPE=2 for op/bst histos (OBHIST)
	do m=1,ncomp
	   j1=2*m-1		!=1,3,5..
	   theta(j1)=tau(m)
	   theta(j1+1)=area(m)	!=theta 2,4,6..
	enddo

	ifitype=1	!display calculated parameters
	if(id1.eq.1) ifitype=2
	kmax=200	!dimension of theta
c
c	iask=2	!do not ask before leaving display; leave graph on screen
	iask=-2		!leave with graph erased, no query
c
	call VPLOT5(XVAL,YVAL,NDAT,icurvd,ncurvd,ijoin,syms,ndimd,
     & XCAL,YCAL,NCAL,icurvc,ncurvc,iline,ndimc,ISYM,ILOG,ISCAL,
     & XMIN,XMAX,YMIN,YMAX,XTIC,YTIC,xcross,ycross,ntx,nty,itx,ity,
     & XLO,XHI,YLO,YHI,y00,yinfeq,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,plotonly,itit,title1,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,interp,
     & isetcol,itrace,ndv1,ndc1,wght,kwi,kwj,icurvw,kmax,iver)
c
	ans='Y'
	call DCASK('Another display',ans,ans)
	if(ans.eq.'Y') then
	   goto 50
	endif
c
	if(allocated(xval)) deallocate(xval,yval,xcal,ycal)
	if(allocated(tau)) deallocate(tau,area)
	return
	end



	subroutine GETJ(id1,id2,id3,nsav,idtype,jout,ndtmax)
c To get j for tausav(i,j) etc in SCBDISP
	integer*4 idtype(4,ndtmax)
c
	jout=0
	do jn=1,nsav
	   if(idtype(1,jn).eq.id1) then
		if(idtype(2,jn).eq.id2.and.idtype(3,jn).eq.id3) then
		   jout=jn
		   goto 9
		endif
	   endif
	enddo
9	RETURN
	end

