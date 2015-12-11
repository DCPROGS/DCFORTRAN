	subroutine GETPOP(xval,yval,iread,ymin1,ymax1,xmin1,xmax1,
     &   yopen,yshut,offset,tseg,npseg,dtx,npline,popen,xpval,
     &   npops,npop,imode,nppage)
c To get Popen values for PLOTSAMP
c Raw channel trace in Yval, returned as yval=yval-yshut, so baseline at zero
c (essential for separation of traces)
c Returns yopen, yshut, popen, xpval, npops=actual number of Popen values
c defined here

c Problem -stabplot uses SCAN output to get Popen, but plotsamp uses
c only the raw data!
c Could base this on code from stabplot.for, if the .scn file is read in
c but more interesting to use different method here (and has advantage that
c don't need to SCAN)
c Problem is to locate baseline, preferably without writing special graphics
c define a bit of baseline
c AT present, ymin, ymax used to define default line spacing, and OK as long as
c long as there is not a whole page with no openings -so ymax is at top of
c shut level (with openings downwards) -can just take points that ar 4 or 6 SD
c below ymax as shut level? But (a) SD needs to be calc from shut points, and
c (b) to calculate Popen a good value for open level is needed
c
c THUS cannot escape having an initial display  in which shut and open levels
c are defined with cursors. After this, check for baseline drifts on subsequent
c pages and on each page draw the shut and open levels on the raw data as check
c
c Simplest to use VPLOT5 to draw the initial plot?

c  imode=1 -does large display of whole page on one line with cursor for open and
c		shut levels
c  imode=0	-uses previous open/shut levels
c
	real*4 xval(nppage,1),yval(nppage,1)
	real*4 popen(npop),xpval(npop)
c for VPLOT
	character*40 titlex,titley
	character*64 title1
	ALLOCATABLE:: weight,icurvw
	real*4 weight(:,:)
	integer*4 icurvw(:)
	ALLOCATABLE XCAL,YCAL
	real XCAL(:,:),YCAL(:,:)		!for VPLOT
	integer ndat(1),icurvd(1),isym(1),ijoin(1)	!for VPLOT data
	real symsiz(1),theta(1)
	integer ncal(1),icurvc(1),iline(1),icol(100)		!for calc curve
	LOGICAL fitted,doframe,draft,autplt,landscap,ivplot
	logical mono,offset
	COMMON/cols/icol,mono
c For setline
	logical delete
c
	character defolt*30,cans*30		!to hold text & result of QDIALOG
c	character*11 cnum
c	character*1 ans
c
c If imode=1 then draw a whole page on one line in vplot5, and then use
c cursors to define open and shut levels
	if(imode.eq.1) then
	   ymin=ymin1	!so values in call not changed
	   ymax=ymax1	!so values in call not changed
	   xmin=xmin1	!so values in call not changed
	   xmax=xmax1	!so values in call not changed
	   ncurvd=1
	   icurvd(1)=1
c	   ndat(1)=nppage
	   ndat(1)=iread
	   ncurvc=0
	   ndv1=nppage
	   ndimd=1
	   ndc1=1
	   ndimc=1
	   if(allocated(icurvw)) then
		DEALLOCATE(icurvw,weight,xcal,ycal)
	   endif
	   kwi=1
	   kwj=1
	   ALLOCATE(icurvw(ndimd),weight(kwi,kwj),xcal(1,1),ycal(1,1))
	   do i=1,ndimd
		icurvw(i)=-1	!no SD
	   enddo
c
	   do i=1,100
		icol(i)=-1
	   enddo
	   cbig=2.5
	   ifont=3
	   ifitype=0		!no display of fitted parameters
	   ntx=5		!if not logt set initial input values
	   nty=5
	   itx=1
	   ity=1
	   ilabel=1
	   titlex='milliseconds'
	   titley='current (pA)'
	   ilog=0
	   iscal=1		!scale internally
	   xlo=-1		!whole screen
	   symsiz(1)=-1.0 	!default symbol sizes
	   isym(1)=0		!points
	   doframe=.true.
	   landscap=.true.
	   autplt=.false.		!to ensure all defaults used
	   fitted=.false.
	   inumx=-1		!don't allow scaling
	   inumy=-1
c	   iask=2		!leave on screen
	   iask=3		!exit immediately and leave on screen
	   call VPLOT5(xval,yval,ndat,icurvd,ncurvd,ijoin,symsiz,ndimd,
     & xcal,ycal,ncal,icurvc,ncurvc,iline,ndimc,isym,ilog,iscal,
     & xmin,xmax,ymin,ymax,xtic,ytic,xcross,ycross,ntx,nty,itx,ity,
     & XLO,XHI,YLO,YHI,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,draft,itit,title1,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,interp,
     & isetcol,itrace,ndv1,ndc1,weight,kwi,kwj,icurvw,kmax,iver)
c Now cursors to define open/shut levels
c (Dialogue box 1 was defined in VPLOT5)
	   call WDIALOG(1,
     & 'Adjust shut level with arrow keys/IMS; then <enter>',11)
c	   s=0.0			!use previous level
c	   do i=1,20
c		s=s+yval(i,1)
c	   enddo
c	   yshut=s/20.		!guess for baseline position (or use ymin?)
c	   yshut=ymax1-0.15
	   call NUMSET
c	   call grapix(x,yshut,iax,iay)	!x, iax not used
c	   call MOVELINE(iay,10,isavbase,ikey)	!10=green
c         call pixgra(iax,iay,x,yshut)
c	   call SETLINE(horizont,value,eps,m,n,amin,amax,
c     & icol,irhi,iclo,ibk,delete,ikey,dim)
c	   eps=0.005*yshut
	   eps=0.02		!0.02 pA (prev line is problem when yshut=0.!)
	   ic=10		!green
	   dim=0.0
	   delete=.false.
	   call SETLINE(.true.,yshut,eps,6,2,xmin,xmax,
     &    ic,8,6,71,delete,ikey,dim)
c
	   call WDIALOG(1,
     & 'Adjust open level with arrow keys/INS; then <enter>',12)
	   call NUMSET
c	   yopen=ymin1+0.15	!use prev value
c	   eps=0.005*yopen
	   eps=0.02		!0.02 pA
	   ic=12		!red
	   dim=0.0
	   delete=.false.
	   call SETLINE(.true.,yopen,eps,6,2,xmin,xmax,
     &    ic,8,6,71,delete,ikey,dim)
c	   call grapix(x,yopen,iax,iay)	!x, iax not used
c	   call MOVELINE(iay,12,isavbase,ikey)		!12=red
c         call pixgra(iax,iay,x,yopen)
c
	   amp=yopen-yshut
c	   call DCFORMAT(amp,6,2,cnum)
c	   ans='N'
	   amp1=amp
 	   call DEFOLTr(amp1,defolt)
	   call QDIALOG(1,
     &      'Amplitude (pA) ',defolt,11,cans)
	   call GETINPr(cans,amp1)
	   if(amp.lt.0.0) then	!check sign
		amp1=-abs(amp1)
	   endif
	   if(amp1.ne.amp) then
		yopen=yshut+amp1	!overide yopen with input value
	   endif
c
c	Subtract Yshut so shut level at zero (essential to get spacing of
c     alternating rows right)
	   do i=1,iread
		yval(i,1)=yval(i,1) - yshut
	   enddo
	   offset=.true.		!yval offset to zero baseline
c
	endif		!end of imode=1
c
c
c Now calculate values of P(open) for current page and keep in popen(i),
c and xpval(i)
	i1=1	!start at yval(1)
	i2=i1+npseg-1
	ns=0
	do while(i2.le.nppage)
	   s=0.0
	   do i=i1,i2
		s=s + yval(i,1)		!yshut already subtracted from yval
	   enddo
	   s=s/float(npseg)		!mean
	   ns=ns+1
	   popen(ns)=s/(yopen-yshut)
	   xpval(ns)=float(i1-1)*dtx		!time (ms) at start of segment
	   i1=i2+1
	   i2=i1+npseg-1
	enddo
	npops=ns
	if(allocated(icurvw)) then
	   DEALLOCATE(icurvw,weight,xcal,ycal)
	endif
	RETURN
	END
