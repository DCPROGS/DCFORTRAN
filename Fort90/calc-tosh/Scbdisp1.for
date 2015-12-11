	subroutine SCBDISP(areasav,tausav,ncompsav,idtype,nsav,
     &  docond,dok,dokr,ndim,ndtmax,)
c
c To do displays of pdfs in SCBURST
c Distribution types defined by idtype()
c Declare idtype(3,j) where
c idtype(1,j)=distribution type
c idtype(2,j)=condition variable 1 (=0 if unconditional)
c idtype(3,j)=condition variable 2 (=0 if unconditional)
c
c idtype(1,j)=1 for number of openings per burst
c idtype(1,j)=2 for pdf of open times
c 	idtype(1:3,j)=
c	2,0,0 for all open times
c	2,-1,0 for first opening in burst with 2 or more openings
c idtype(1,j)=3 for pdf of shut times
c	3,0,0 for all shut times
c idtype(1,j)=4 for pdf of burst lengths
c	4,0,0 for all burst lengths
c	4,-1,0 for burst lengths for bursts with 2 or more openings
c idtype(1,j)=5 for pdf of total open time per burst
c	5,0,0 for all total open/burst
c idtype(1,j)=6 for pdf of gaps within bursts
c	6,0,0 for all gaps within bursts
c idtype(1,j)=7 for pdf of gaps between bursts
c	7,0,0 for all gaps between bursts
c
	real*4 tausav(ndim,ndtmax),areasav(ndim,ndtmax)
	integer* ncompsav(ndtmax)
	integer*4 idtype(4,ndtmax)
	logical docond,dok,dokr
===========
c for display
	ALLOCATABLE Xval,Yval,Xcal,Ycal
	real Xval(:,:),Yval(:,:)		!for VPLOT
	real Xcal(:,:),Ycal(:,:)		!for VPLOT
c for data
	integer*4 ndat(1),icurvd(1),isym(1),ijoin(1)	!for data
	real*4 syms(1)				!for data
c for calc curves ndimc=2
	dimension ncal(2),icurvc(2),iline(2)	!for calc curve
	LOGICAL debug,fitted,doframe,plotonly,autplt,pspec
	logical varconc,showconc,plotsav
	character*40 titlex,titley
	character*64 title1
	logical caplock,landscap,ivplot,interp
	character*1 ans,UC
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
	errflag=.true.
	call UNDFL(errflag)
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
	fitted=.false.
	fstdisp=.true.
	k=kA+kB+kC+kD
c
	iscal=1		!scale internally
	inumx=-1		!X axis in fixed (Fn.d) format
	inumy=-1
	titlex='time (ms)'
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
	itrace=0	!no longer used!
	kwi=1		!dimension of weight()
	kwj=1		!dimension of weight()
c
c=======
c idtype(1,j)=1 for number of openings per burst
c idtype(1,j)=2 for pdf of open times
c 	idtype(1:4,j)=
c	2,kA,0,0 for all open times
c	2,kA,-1,0 for first opening in burst with 2 or more openings
c idtype(1,j)=3 for pdf of shut times
c	3,kF,0,0 for all shut times
c idtype(1,j)=4 for pdf of burst lengths
c	4,kE,0,0 for all burst lengths
c	4,kE+kA,-1,0 for burst lengths for bursts with 2 or more openings
c idtype(1,j)=5 for pdf of total open time per burst
c	5,kA,0,0 for all total open/burst
c idtype(1,j)=6 for pdf of gaps within bursts
c	6,**,0,0 for all gaps within bursts
c idtype(1,j)=7 for pdf of gaps between bursts
c	7,**,0,0 for all gaps between bursts
c=======
	ndv1=1 		!xval not used
	ndimd=1
	ncalc=1024
	ndc1=ncalc
	ndimc=2
	ALLOCATE(Xval(ndv1,ndimd),Yval(ndv1,ndimd))
	ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc)
	id1=1
	print 1,id1
1	format(/,' Plot
     & ' (1) distribution of number of openings per burst',/,
     & ' (2) pdf of open times',/,
     & ' (3) pdf of shut times',/,
     & ' (4) pdf of burst lengths',/,
     & ' (5) pdf of total open time per burst',/,
     & ' (6) pdf of gaps within bursts',/,
     & ' (7) pdf of gaps between bursts',/,
     & ' Option number [',i2,'] = ')
c
	SELECT CASE(id1)
	CASE(1)
	CASE(2)
	CASE(3)
	CASE(4)
	   if(.not.docond) then		!no conditional
		print 2,id2
2		format(/,' Plot
     & ' (1) pdf of burst lengths',/,
     & ' (2) pdf of burst lengths for bursts with 2 or more openings',/,
     &    ' Option number [',i2,'] = ')
		call INPUTi(id2)
		if(id2.eq.1) then
		   id2=0
		   titley='burst length'
		else if(id2.eq.2) then
		   id2=-1
		   titley='burst length; bursts with > 1 opening'
		endif

		call GETJ(id1,id2,k1,r1,nsav,idtype,docond,jout,ndtmax)
	   else
c======
	   endif
	CASE(5)
	CASE(6)
	CASE(7)
	END SELECT
c
	ncomp=ncompsav(jout)
	do i=1,ncomp
	   tau(i)=1000.*tausav(i,jout)	!in ms
	   area(i)=areasav(i,jout)
	enddo
c Now define f(t) and hence xcal, ycal etc for display
	ncalc=1024
c Put non-log values in Xcal(i,1),Ycal(i,1)
	xmin0=0.
	xmax=1000.	!milliseconds
	dx=(xmax-xmin)/float(ncalc)
	do i=1,ncalc
	   t=xmin0+float(i-1)*dx
	   xcal(i,1)=t
	   ft=0.0
	   do m=1,ncomp
		ft= ft + (area(m)/tau(m))*exp(t/tau(m))
	   enddo
	   ycal(i,1)=ft
	enddo
c
c Make log-spaced times for Xcal()
c Put log values in Xcal(i,2),Ycal(i,2)
	xmin=0.01		!ms
	dxlog=(alog10(XMAX)-alog10(xmin))/float(ncalc-1)  !increment
	do i=1,ncalc
	   c=alog10(xmin)+float(i-1)*dxlog	!this is log10 value
	   t=10.**x		!log10 taken in Vplot5
	   xcal(i,2)=t	!not log
	   ft=0.0
	   do m=1,ncomp
		ft= ft + (area(m)/tau(m))*exp(t/tau(m))
	   enddo
c For log(t) distn xwbase defined in CDIST as bin width in log10 units
	   ft=ft*t*2.30259		!f(log10(t))=2.3*t*f(t)
	   ycal(i,2)=ft
	enddo
c
	iplot=1
	print 10,iplot
10	format(
     & ' (1) Plot distribution of time',/,
     & ' (2) Plot distribution of log(time)',/,
     & ' Option number [',i2,'] = ')
	call INPUTi(iplot)
	ncurvd=0
	ncurvc=1
	ncal(1)=ncalc
	iline(1)=0
	if(iplot.eq.1) then
	   ilog=0
	   icurvc(1)=1	!plot ycal(i,1)
	else if(iplot.eq.1) then
	   ilog=1
	   icurvc(1)=2	!plot ycal(i,2)
	endif
	idisp=1
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
	DEALLOCATE(Xval,Yval,Xcal,Ycal)
	RETURN
	end


	subroutine GETJ(id1,id2,k1,r1,nsav,idtype,docond,jout,ndtmax)
c To get j for tausav(i,j) etc in SCBDISP
	integer*4 idtype(4,ndtmax)
	logical docond
c
	jout=0
	do jn=1,nsav
	   if(idtype(1,jn).eq.id1) then
		if(.not.docond) then
		   if(idtype(2,jn).eq.id2) then
			jout=jn
			goto 9
		   endif
		endif
	   endif
	enddo
9	RETURN
	end

