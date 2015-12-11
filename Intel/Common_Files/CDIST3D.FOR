	subroutine CDIST3D(iplotype,tint,ampl,iprops,nint,nintt,ndimy,iscan,
     &treso,tresg,tres,idsav,idiskq,id1,id2,lag,nbdec,xaxis,yaxis,p0min,
     & igraph,main,ixg,iyg,graph1_1,GraphMainPanel1_1,graphics1_1,ipos,
     &	oldrecords,nplot,itty,Z00A,Z10A,Z11A,
     &     Z00F,Z10F,Z11F,XAF,XFA,QexpQA,QexpQF,kAm,kFm,km,jopen,
     &     ifnt,ifnl,alfa,beta,gama,delta,
     &     ijust,ijusx,ijusy,ijusz,
     &     xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,numx,numy,
     &     xmin,xmax,ymin,ymax,zmin,zmax,xtic,ytic,ztic,
     &     ratio,rat,radius,theta,phi,ifram,numaxi,nwidth,npow,nplace,
     &     nxstep,nystep,istyle,isup,
     &     fill,inter,axis,fillbad,autplt,cross,iplot,irt)
c)

	use menu_f90
	use hjcrecords
	real*8 tres
	integer jopen(200)
	integer :: Main
	integer :: Graph1_1(100)
	integer :: GraphMainPanel1_1(100)
	integer :: Graphics1_1(100)
c For doing bivariate distributions and dependency (modif from cdist1)
c Set the bins, and do the display, all within this subroutine?
      allocatable::yval,ygap
      allocatable::xmid,ymid,x,y,z,znew,zf
	real*4 xaxis(501),yaxis(501)
	real*4 xmid(:),ymid(:),yval(:),ygap(:),x(:),y(:),z(:,:),znew(:,:)
	real*4 zf(:,:)
      allocatable::px,py
	real*4 px(:),py(:)
      allocatable::dep,depdif
	real*4 dep(:,:),depdif(:,:)
	allocatable::badval
	logical badval(:,:)
	logical quarter,plot,landscape
	
	character*60 gfile
	character*75 xtitle,ytitle,ztitle	!output from LAXES
	integer icol(100),i3col(100)		!for DCMENU
	character*75 title1
c
	character*50 text(5)
	logical first
c
	real*4 TINT(nintt),ampl(nintt)
	allocatable::fdep,shutt,opent
	real*4 fdep(:,:),shutt(:),opent(:)
	integer*1 iprops(nintt)
	logical exass,dsmooth
	logical btest,setbad,badint(nintt),dubious(nintt)
	logical open1,open2,shut1,setbad1,setbad2,bad1,bad2,bad
	logical allocated,posneg
	character*1 ans,UC
c	character*11 cnum
	logical discprt,deb
	logical all,cross,after,monsav,fillbad
	logical mono,autplt,fill,inter,present,landplot,axis
	INTEGER ISTYLE(6),ISUP(6)
	TYPE (RECORD_ATTRIBUTES) oldrecords(25)
	common/dp/discprt
	
	character*256 string
	
	common/cars/dxsa,dysa,graph
	logical usedef,shbiv,shdep,shdif,shfit
	common/def/usedef,exass,dsmooth,shbiv,shdep,shdif,shfit		!for qdialog
	common/pixpos/ixgrid,iygrid,ixpix,iypix,wid,landscape
	
	real*8 Z00A(kAm,kFm,km),Z10A(kAm,kFm,km),Z11A(kAm,kFm,km)
	real*8 Z00F(kFm,kAm,km),Z10F(kFm,kAm,km),Z11F(kFm,kAm,km)
	real*8 QEXPQA(kFm,kAM),QEXPQF(kAm,kFm)
	real*8 XAF(kAm,kFm,kAm),XFA(kFm,kAm,kFm)

c Define functions
	
c	dubious(idum)=BTEST(iprops(idum),0)	!ampl(i) was dubious (bit 0 set ='1')
c	badint(idum)=BTEST(iprops(idum),3)	!tint(i) was unusable (bit 3='8' set)
	dubious=BTEST(iprops,0)	!ampl(i) was dubious (bit 0 set ='1')
	badint=BTEST(iprops,3)	
	 	tres=max(treso,tresg)	!for now
		nbmax=500

c
c Now define xval, yval arrays that contain the two variables to be used
c For id1=1, id1=1, x(i)=open period, y(i)=shut time that follows it
c  If either interval is set unusable, then look for next good open period
c  and start from there
c For id1=1, id2=2, xval(i)=open period, y(i)=shut time that precedes it
c
c
	continue
	goto 2251
	exass=.false.
	
	if(iscan.eq.-1003.or.iscan.ge.100) then
	   ians=gmdisplaymessagebox('',
     &' Exclude openings with ''dubious'' amps ? ',gquestion,gyesno)
	else
	 ians=gmdisplaymessagebox('',
     &' Exclude openings with ''assumed'' amps ? ',gquestion,gyesno)
	endif
	
	if(ians.eq.gyesbutton) exass=.true.
2251	continue
c
	call PDUB(exass,.false.,iscan,.false.)
	if(exass.and.id1.ne.2)  CALL GMSETTEXTSETTING(ITTY,
     & ' (i.e. ANY dubious amp excludes whole open period)')
	
	if(exass.and.discprt.and.id1.ne.2) write(7,2273)
2273	format(' (i.e. ANY dubious amp excludes whole open period)')

	if(allocated(ygap)) DEALLOCATE(ygap)
	ALLOCATE(yval(nint),ygap(nint))
	ndimy=nint 	!for debtyp
	do i=1,nint
	   yval(i)=0.0
	enddo
C
	ifst=1
	j=0   !counts periods in A (=subset of open states)
247	continue
	do while (ifst.le.nint-1)
	   !if(mod(ifst-1,50).eq.0) deb=debug()
	   
	   a1=ampl(ifst)
	   a2=ampl(ifst+1)
c	   if(.not.conam1) then
	      open1=a1.ne.0
	      open2=a2.ne.0
	      shut1=.not.open1

	   setbad1=badint(ifst)		!tint(i) was unusable
	   setbad2=badint(ifst+1)		!tint(i) was unusable
	   bad1=setbad1.or.(dubious(ifst).and.exass)
	   bad2=setbad2.or.(dubious(ifst+1).and.exass)
	   bad=.false.
	   if(bad1.or.bad2) then
		ifst=ifst+1
		n=n+1      		!count this as nth open period, although not usable
		bad=.true.
		goto 247  		!look for next good start
	   else if(shut1.and.open2) then
		n=n+1 		!count this as nth open period
		
		goto 248		!valid 'start of open period' now found
	   endif
	   ifst=ifst+1
	enddo
c if this point reached, all intervals inspected
	j=j-1
	goto 992	!all intervals inspected
c Valid 'start of open period' found now -get the rest
248	continue
	j=j+1		!count number of open periods found (reversed if bad)
	ifst=ifst+1		!index of first opening in open period just started
	Yval(j)=tint(ifst)	!length of this opening
	k=ifst+1	!index of interval that follows the first opening
c====replacement for 'find rest'
	bad=.false.
c Now find the rest of the openings in this open period
	do while (k.le.nint)
	   a1=ampl(k)

		open1=a1.ne.0
		if(exass.and.dubious(k)) bad=.true.
		if(.not.open1) goto 249		!valid end of group found

	   YVAL(j)=YVAL(j)+tint(k)	!add any subsequent openings to same YVAL
	   setbad=badint(k)		!tint(k) was unusable
	   if(setbad) bad=.true.	!this group bad, but finish anyway
	   k=k+1
	enddo
	j=j-1
	goto 992	!data finished before end found, so do not use
c
c Group completed
249	continue
	if(bad) nbad1=nbad1+1	!count # of above case


	ifst=k	!so do not skip past shut time needed to start new group

	   ygap(j)=TINT(k)   !record length of shut time following open period #j
	   if(ampl(k).ne.0) then
	
700		format(' YGAP ERROR:',i5,2g13.6,i3)
	    write(string,fmt='(a10,g13.6,g13.6,g13.6)')
     &	'Ygap error ',k,tint(k),ampl(k),iprops(k)
c		CALL GMSETTEXTSETTING(ITTY,string)
	   endif
	   
	   if(bad) then
		j=j-1	!if bad do not use. YVAL(j) gets overwritten
	
	   endif
	   if(ifst.lt.nint) goto 247	!look for start of new group
c If ifst=nint then end here, but first check that the last gap, tint(k),
c is OK -it will often be set bad, in which case, for the purpose of
c correlations in which 'following gap' is needed, we must exclude
c the open period that preceded it too.
	   if(badint(k)) j=j-2
c	endif
C N.B. does not return if ifst=nint, as if tint(nint) is start of a
C group there is no way to tell if this group has been completed-
C (the next interval might have been open too)
C  NOTE that NYVAL=naset calc here may not be identical with that
C found in calc of burst distributions because latter includes
C only those open periods that are part of valid bursts (no bad
C openings and proper end)
992	nyval=j
c Now have all open periods in yval(), and shut times between them in ygap()
c Note: specified gap is ygap(i); the open period preceding this is
c yval(i) [yval(i-lag+1) in general], and the open period following
c the specified gap is in yval(i+1) [yval(i+lag) in general].
c NB ygap not defined when amplitude constrained so omit bits that need ygap
c	Find max and min
c
	omin=1.e37
	omax=-1.e37
	smin=1.e37
	smax=-1.e37
c Only max needed for logbin, xmin, ymin needed only for fixax (NB to get min
c must skip unusable ones that are set to -1.)
	do i=1,nyval
	   if(yval(i).lt.omin) omin=yval(i)
	   if(yval(i).gt.omax) omax=yval(i)
	   if(ygap(i).lt.smin) smin=ygap(i)
	   if(ygap(i).gt.smax) smax=ygap(i)
c check for errors (exclude -1. for 'set unusable')
!	   if(yval(i).lt.treso.and.yval(i).gt.-0.9) 
!     &	write(string,fmt='(a10,i6,a5,g13.6)')
!    &	'error:yval(',i,')=',yval(i) 
!		CALL GMSETTEXTSETTING(ITTY,string)
	  
!	   if(ygap(i).lt.tresg.and.ygap(i).gt.-0.9) 
 !    &	write(string,fmt='(a10,i6,a5,g13.6)')
 !    &	'error:ygap(',i,')=',ygap(i) 
!		CALL GMSETTEXTSETTING(ITTY,string)


	enddo
	if(id1.eq.1) then
	   xmin=smin
	   xmax=smax
	   ymin=omin
	   ymax=omax
	else if(id1.eq.2) then
	   xmin=smin
	   xmax=smax
	   ymin=smin
	   ymax=smax
	else if(id1.eq.3) then
	   xmin=omin
	   xmax=omax
	   ymin=omin
	   ymax=omax
	endif
c Fix axis limits
	xmin1=xmin	!save original values
	xmax1=xmax
	ymin1=ymin
	ymax1=ymax
c Get new xmax etc
	call FIXAX(xmin1,xmax1,xmin,xmax,xtic,0)	!always non-log
	call FIXAX(ymin1,ymax1,ymin,ymax,ytic,0)
c
c Decide bin boundaries
	tres=max(treso,tresg)	!for now
	nbmax=500

	dx=exp1(alog(10.)/float(nbdec))
	xend=1. + xmax - amod(xmax,1.)
	nbinx=1+ifix(alog(xend/xaxis(1))/alog(dx))
	if(nbinx.gt.nbmax) nbinx=nbmax
	xend=(dx**nbinx)*xaxis(1)

	dy=exp1(alog(10.)/float(nbdec))
	yend=1. + ymax - amod(ymax,1.)
	nbiny=1+ifix(alog(yend/yaxis(1))/alog(dy))
	if(nbiny.gt.nbmax) nbiny=nbmax
	yend=(dy**nbiny)*yaxis(1)

	do i=1,nbinx
	   xaxis(i+1)=xaxis(1)*(dx**i)
	enddo
	xwbasex=alog10(dx)
	do i=1,nbiny
	   yaxis(i+1)=yaxis(1)*(dy**i)
	enddo
	xwbasey=alog10(dy)

!	call LOGBIN(xaxis,nbmax,tres,nbinx,nbdec,xwbasex,xmax)
!	call LOGBIN(yaxis,nbmax,tres,nbiny,nbdec,xwbasey,ymax)
c
c Allocate xmid, and define it
c NB all values except xybase are non-log at this stage -this is what is needed
c for sorting into bins, but GPLOT3D needs log values for xmid,ymid
	ALLOCATE(xmid(nbinx))
	do i=1,nbinx
	   xmid(i)=0.5*(alog10(xaxis(i))+alog10(xaxis(i+1)))   !midpoint of bin
	enddo
c
	
	
	
	
c Allocate ymid, and define it
	ALLOCATE(ymid(nbiny))
	do i=1,nbiny
	   ymid(i)=0.5*(alog10(yaxis(i))+alog10(yaxis(i+1)))   !midpoint of bin
	enddo
	
	
	CALL GMSETTEXTSETTING(ITTY,' Calculating distribution . . .')
c Allocate x,y arrays and copy the relevant open/shut times to them, for the
c distribution that is required
c For id=1, x(i)=open period, y(i)=shut time that follows it (with lag=l)
c Note: for in ygap(i);
c	the open period preceding this is yval(i), or yval(i-lag+1) in general
c	and the open period following it is in yval(i+1), or yval(i+lag) in general
c OR for open period in yval(i)
c	the gap that follows it is in ygap(i)   -or ygap(i+lag-1) in general
c	the gap that precedes it is in ygap(i-1) -or ygap(i-lag) in general
c
	if(id2.eq.3) then		!both preceding and following
	   ALLOCATE(x(2*nyval),y(2*nyval))
	else
	   ALLOCATE(x(nyval),y(nyval))
	endif
c
c Now get x,y values
c In case of id2=4 (difference between two pdfs) we need to subtract
c the z(i,j) values, or smoothed znew(i,j) values, for the two pdfs
c so go through once (with first=.true.) to get 'following', and store
c the z(i,j), or znew(i,j), values in zf(i,j) ('f' for following).  Then
c go through again (with first=.false.) and get z(i,j) for 'preceding'
c and calculate final z(i,j)=zf(i,j)-z(i,j)
c Check lag is within limits and count actual number of pairs of values
	id22=id2
	first=.true.
100	continue	!return here for 2nd pdf if id2=4
	if(id2.eq.4) then
	   if(first) then
		id22=1	!following
	   else
		id22=2	!preceding
	   endif
	endif
	if(id1.eq.1.and.id22.eq.1) then
	   xtitle='shut time (following)'
	   ytitle='open period'
	   title1='f(open, following shut)'
	   k=0
	   do i=1,nyval
		ix=i+lag-1
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=ygap(iy)
		   y(k)=yval(ix)
		endif
	   enddo
	   nval=k
	else if(id1.eq.1.and.id22.eq.2) then
c	the gap that precedes it is in ygap(i-1) -or ygap(i-lag) in general
	   xtitle='shut time (preceding)'
	   ytitle='open period'
	   title1='f(open, preceding shut)'
	   k=0
	   do i=1,nyval
		ix=i-lag
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=ygap(iy)
		   y(k)=yval(ix)
		endif
	   enddo
	   nval=k
	else if(id1.eq.1.and.id22.eq.3) then		!both
	   xtitle='shut time (adjacent)'
	   ytitle='open period'
	   title1='f(open, adjacent shut)'
	   k=0
	   do i=1,nyval 		!following
		ix=i+lag-1
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=ygap(iy)
		   y(k)=yval(ix)
		endif
	   enddo
	   do i=1,nyval		!go through again to add preceding
		ix=i-lag
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=ygap(iy)
		   y(k)=yval(ix)
		endif
	   enddo
	   nval=k
	else if(id1.eq.2.and.id22.eq.1) then
	   xtitle='shut time (following)'
	   ytitle='shut time'
	   title1='f(shut, following shut)'
	   k=0
	   do i=1,nyval
		ix=i+lag
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=ygap(iy)
		   y(k)=ygap(ix)
		endif
	   enddo
	   nval=k
	else if(id1.eq.2.and.id22.eq.2) then
	   xtitle='shut time (preceding)'
	   ytitle='shut time'
	   title1='f(shut, preceding shut)'
	   k=0
	   do i=1,nyval
		ix=i-lag
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=ygap(iy)
		   y(k)=ygap(ix)
		endif
	   enddo
	   nval=k
	else if(id1.eq.2.and.id22.eq.3) then		!both
	   xtitle='shut time (adjacent)'
	   ytitle='shut time'
	   title1='f(shut, adjacent shut)'
	   k=0
	   do i=1,nyval	!first following
		ix=i+lag
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=ygap(iy)
		   y(k)=ygap(ix)
		endif
	   enddo
	   do i=1,nyval		!add preceding
		ix=i-lag
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=ygap(iy)
		   y(k)=ygap(ix)
		endif
	   enddo
	   nval=k
	else if(id1.eq.3.and.id22.eq.1) then
	   xtitle='open period (following)'
	   ytitle='open period'
	   title1='f(open, following open)'
	   k=0
	   do i=1,nyval
		ix=i+lag
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=yval(iy)
		   y(k)=yval(ix)
		endif
	   enddo
	   nval=k
	else if(id1.eq.3.and.id22.eq.2) then
	   xtitle='open period (preceding)'
	   ytitle='open period'
	   title1='f(open, preceding open)'
	   k=0
	   do i=1,nyval
		ix=i-lag
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=yval(iy)
		   y(k)=yval(ix)
		endif
	   enddo
	   nval=k
	else if(id1.eq.3.and.id22.eq.3) then		!both
	   xtitle='open period (adjacent)'
	   ytitle='open period'
	   title1='f(open, adjacent open)'
	   k=0
	   do i=1,nyval		!first following
		ix=i+lag
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=yval(iy)
		   y(k)=yval(ix)
		endif
	   enddo
	   do i=1,nyval		!add in preceding
		ix=i-lag
		iy=i
		if(ix.ge.1.and.ix.le.nyval.and.iy.ge.1.and.iy.le.nyval) then
		   k=k+1
		   x(k)=yval(iy)
		   y(k)=yval(ix)
		endif
	   enddo
	   nval=k
	endif
c
	if(id2.eq.4) then
	   if(id1.eq.1) then
		xtitle='shut time'
		ytitle='open period'
		title1='f(open,following shut) - f(open, preceding shut'
	   else if(id1.eq.2) then
		xtitle='shut time'
		ytitle='shut time'
		title1='f(shut,following shut) - f(shut, preceding shut'
	   else if(id1.eq.3) then
		xtitle='open period'
		ytitle='open period'
		title1='f(open,following open) - f(open, preceding open'
	   endif
	endif

	ALLOCATE(z(nbinx,nbiny))
	if(allocated(badval)) DEALLOCATE(badval)
	ALLOCATE(badval(nbinx,nbiny))
	ndx=nbinx	!dimension of z,bad
	ndy=nbiny	!dimension of z,bad
	n=nval
	do i=1,nbinx
	   do j=1,nbiny
		z(i,j)=0.0
		badval(i,j)=.false.
	   enddo
	enddo
	nxlo=0
	nxhi=0
	nylo=0
	nyhi=0
c Sort into bins
	do k=1,n
	   xv=x(k)
	   yv=y(k)
c Check for values outside range of bins
	   if(xv.lt.xaxis(1)) nxlo=nxlo+1
	   if(xv.ge.xaxis(nbinx+1)) nxhi=nxhi+1
	   if(yv.lt.yaxis(1)) nylo=nylo+1
	   if(yv.ge.yaxis(nbiny+1)) nyhi=nyhi+1
c
	   do i=1,nbinx
		do j=1,nbiny
		   if(xv.ge.xaxis(i).and.xv.lt.xaxis(i+1).and.
     &		yv.ge.yaxis(j).and.yv.lt.yaxis(j+1)) then
			z(i,j)=z(i,j)+1.0
		   endif
		enddo
	   enddo
	enddo
c
c For difference between pdfs, keep the first one now, and go back for the second
	if(id2.eq.4) then
	   if(first) then
		ALLOCATE(zf(nbinx,nbiny))
		do i=1,nbinx
		   do j=1,nbiny
			zf(i,j)=z(i,j)		!save following
		   enddo
		enddo
		DEALLOCATE(z)
		first=.false.
		goto 100		!get preceding
	   else
		do i=1,nbinx
		   do j=1,nbiny
			z(i,j)=zf(i,j)-z(i,j)	 !get difference between pdfs
		   enddo
		enddo
		DEALLOCATE(zf)
	   endif
	endif
c Do inverse distance smoothing? or put this into GPLOT3D -better so
c effect can be seen more easily
	ALLOCATE(znew(nbinx,nbiny))
	power=1.0
cv	ians=gmdisplaymessagebox('','Use 3D smoothing before display',
cv     & gquestion,gyesno)
cv	if(ians.eq.gyesbutton) then
	if(dsmooth) then
	   call SMOOTH3D(z,znew,nbinx,nbiny,power,nbinx,nbiny)
	else
	   do i=1,nbinx
		do j=1,nbiny
		   znew(i,j)=z(i,j)
		enddo
	   enddo
	endif
	if(id2.ne.4) then
	   do i=1,nbinx
		do j=1,nbiny
		   znew(i,j)=sqrt(znew(i,j))
		enddo
	   enddo
	endif
	idrawt=1
	idrawx=1
	idrawy=1
	idrawz=1
		i3col(1)=1	   !contour 1
		i3col(2)=9	   !cont 2
		i3col(3)=3	   !cont 3
		i3col(4)=11   !cont 4
		i3col(5)=2	   !cont 5
		i3col(6)=10   !cont 6
		i3col(7)=8	   !cont 7
		i3col(8)=5	   !cont 8
		i3col(9)=13   !cont 9
		i3col(10)=6	   !cont 10
		i3col(11)=4	   !cont 11
		i3col(12)=12   !cont 12
		i3col(13)=14   !cont 13
		i3col(14)=15   !cont 14
		i3col(15)=7	   !cont 15
		i3col(21)=48	!axes
		i3col(22)=0   !bad region
		i3col(23)=1	!labels
		i3col(25)=1	!title
		i3col(31)=14	!upper net
		i3col(32)=8	!lower net
		i3col(33)=9 	!upper surface
		i3col(34)=10	!lower surface
		i3col(35)=12	!positive
		i3col(36)=11	!negative
		i3col(37)=4	!positive lower
		i3col(38)=9	!negative lower
		i3col(71)=7
ccc	i3col(23)=0		      !in common for gplot3d
	i3col(25)=9
	i3col(71)=7
	do i=1,nbinx
	   do j=1,nbiny
	     badval(i,j)=.false.
	   enddo
	enddo
	 if(kcol.eq.4) then
                  posneg=.true.
                  kcol=2
             else if(kcol.eq.1) then  ! contour
                  ncont=14
                  kcol=-ncont
                  posneg=.false.
             else if(kcol.eq.2) then  ! one
                  kcol=1
                  posneg=.false.
             else if(kcol.eq.3) then  ! two
                  kcol=2
                  posneg=.false.
             endif
              ncolfu=9 
			ncolfd=10
			ncolgu=14
			ncolgd=8
			ncolmp =12 
			ncolmn =9
			icol23=1
			icol25=1
			icol71=7
			icbad=12
             i3col(33)=ncolfu
             i3col(34)=ncolfd
             i3col(31)=ncolgu
             i3col(32)=ncolgd
             i3col(25)=icol25
             i3col(23)=icol23
             i3col(71)=icol71
             i3col(22)=icbad
             i3col(35)=ncolmp    !positive
			i3col(36)=ncolmn   !negative
cv	ians=gmdisplaymessagebox('','Show bivariate distribution',
cv     & gquestion,gyesno)
cv	if(ians.eq.gyesbutton) then
		if(shbiv) then
	   if(id2.ne.4) then
		ztitle='Square root(number)'
	   else
		ztitle='Number'
	   endif

	   kcol=2
	   posneg=.true.
	   isetcol=2
	 igraph=igraph+1
	ixg=ixg+20
	iyg=iyg+20
		call gauto3d(iplotype,igraph,xmid,ymid,znew,badval,nbinx,nbiny,ndx,
     &	ndy,xtitle,ytitle,ztitle,title1,idrawt,idrawx,idrawy,idrawz,
     &    quarter,idev,plot,iplot,kcol,posneg,isetcol,i3col,
     &	wxmin,wxmax,wymin,wymax,vxlop,vxhip,vylop,vyhip,
     &    xlop,xhip,ylop,yhip,main,ixg,iyg,graph1_1,
     &    GraphMainPanel1_1,graphics1_1,ipos,gfile,ixposv,iyposv,
     &	oldrecords,nplot,ifnt,ifnl,alfa,beta,gama,delta,
     &     ijust,ijusx,ijusy,ijusz,
     &     xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,numx,numy,
     &     xmin,xmax,ymin,ymax,zmin,zmax,xtic,ytic,ztic,
     &     ratio,rat,radius,theta,phi,ifram,numaxi,nwidth,npow,nplace,
     &     nxstep,nystep,istyle,isup,
     &     fill,inter,axis,fillbad,autplt,cross,jopen)
	endif
		if(id2.eq.4) goto 99
c
	ALLOCATE(dep(nbinx,nbiny),depdif(nbinx,nbiny))
!	ians=gmdisplaymessagebox('','Show dependency plot',gquestion,
!     &gyesno)
!	if(ians.eq.gnobutton) goto 98

	do i=1,nbinx
	   do j=1,nbiny
		z(i,j)=znew(i,j)
	   enddo
	enddo
c
	ALLOCATE(px(nbinx),py(nbiny))
	do i=1,nbinx	!for each row
	   s=0.0
	   do j=1,nbiny	!sum over columns
		s=s+z(i,j)
	   enddo
	   px(i)=s
	enddo
c
	do j=1,nbiny	!for each column
	   s=0.0
	   do i=1,nbinx	!sum over rows
		s=s+z(i,j)
	   enddo
	   py(j)=s
	enddo
c
	sx=0.0
	do i=1,nbinx
	   sx=sx+px(i)
	enddo
	sy=0.0
	do j=1,nbiny
	   sy=sy+py(j)
	enddo
	entot=sx		!=sy too -total number
c Divide by total to get probabilities
	do i=1,nbinx
	   px(i)=px(i)/entot
	enddo
	do j=1,nbiny
	   py(j)=py(j)/entot
	enddo
c
	
c NB must avoid divide by zero -should mark the bad values (note i,j, and
c fill in black in gplot3d?)
c	p0min=2.	!minimum value for denominator, pij0
c	p0min=0.1	!minimum value for denominator, pij0
	p0min=1.	!minimum value for denominator, pij0
	
	nbad=0
	do i=1,nbinx
	   do j=1,nbiny
		pij0=px(i)*py(j)*entot	!expected number if indep
		pij=z(i,j)			!number actually observed
		if(pij0.gt.p0min) then
		   dep(i,j)=(pij/pij0)-1.0	!dependency
		   badval(i,j)=.false.
		else
		   dep(i,j)=0.0
		   nbad=nbad+1
		   badval(i,j)=.true.
		endif
c
		depdif(i,j)=sqrt(pij)-sqrt(pij0)	!dependency difference
	   enddo
	enddo
	
	if(discprt) write(7,71) nbad,p0min
71	format(1x,i4,
     & ' values of dep(i,j) set to 0 because denominator < ',f5.2)
c
c Now plot
cv		ians=gmdisplaymessagebox('','Show dependency plot',gquestion,
cv     &	gyesno)
cv	if(ians.eq.gnobutton) goto 98
	if(.not.shdep) goto 98
	power=1.0
	
cv	ians=gmdisplaymessagebox('','Use 3D smoothing before display',
cv     & gquestion,gyesno)
cv	if(ians.eq.gyesbutton) then
	if(dsmooth) then
	   call SMOOTH3D(dep,znew,nbinx,nbiny,power,nbinx,nbiny)
	else
	   do i=1,nbinx
		do j=1,nbiny
		   znew(i,j)=dep(i,j)
		enddo
	   enddo
	endif
	ztitle='dependency'
	title1='Dependency plot'
10	continue
	   kcol=2
	   posneg=.true.
	   isetcol=2
	
	igraph=igraph+1
	ixg=ixg+20
	iyg=iyg+20
	call gauto3d(iplotype,igraph,xmid,ymid,znew,badval,nbinx,nbiny,ndx,ndy,
     &	xtitle,ytitle,ztitle,title1,idrawt,idrawx,idrawy,idrawz,
     &    quarter,idev,plot,iplot,kcol,posneg,isetcol,i3col,
     &	wxmin,wxmax,wymin,wymax,vxlop,vxhip,vylop,vyhip,
     &    xlop,xhip,ylop,yhip,main,ixg,iyg,graph1_1,
     &    GraphMainPanel1_1,graphics1_1,ipos,gfile,ixposv,iyposv,
     &	oldrecords,nplot,ifnt,ifnl,alfa,beta,gama,delta,
     &     ijust,ijusx,ijusy,ijusz,
     &     xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,numx,numy,
     &     xmin,xmax,ymin,ymax,zmin,zmax,xtic,ytic,ztic,
     &     ratio,rat,radius,theta,phi,ifram,numaxi,nwidth,npow,nplace,
     &     nxstep,nystep,istyle,isup,
     &     fill,inter,axis,fillbad,autplt,cross,jopen)
98	continue
cv	ians=gmdisplaymessagebox('','Show dependency difference plot',
cv     &gquestion,gyesno)
cv	if(ians.eq.gnobutton) goto 99
	if(.not.shdif) goto 99
	do i=1,nbinx
	   do j=1,nbiny
	     badval(i,j)=.false.
	   enddo
	enddo
	power=1.0
	
cv	ians=gmdisplaymessagebox('','Use 3D smoothing before display',
cv     &gquestion,gyesno)
cv	if(ians.eq.gyesbutton) then
	if(dsmooth) then
	   call SMOOTH3D(depdif,znew,nbinx,nbiny,power,nbinx,nbiny)
	else
	   do i=1,nbinx
		do j=1,nbiny
		   znew(i,j)=depdif(i,j)
		enddo
	   enddo
	endif
	ztitle='dependency difference'
	title1='Dependency difference plot'
	   kcol=2
	   posneg=.true.
	isetcol=2
	igraph=igraph+1
	ixg=ixg+20
	iyg=iyg+20
	call gauto3d(iplotype,igraph,xmid,ymid,znew,badval,nbinx,nbiny,ndx,ndy,
     &	xtitle,ytitle,ztitle,title1,idrawt,idrawx,idrawy,idrawz,
     &    quarter,idev,plot,iplot,kcol,posneg,isetcol,i3col,
     &	wxmin,wxmax,wymin,wymax,vxlop,vxhip,vylop,vyhip,
     &    xlop,xhip,ylop,yhip,main,ixg,iyg,graph1_1,
     &    GraphMainPanel1_1,graphics1_1,ipos,gfile,ixposv,iyposv,
     &	oldrecords,nplot,ifnt,ifnl,alfa,beta,gama,delta,
     &     ijust,ijusx,ijusy,ijusz,
     &     xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,numx,numy,
     &     xmin,xmax,ymin,ymax,zmin,zmax,xtic,ytic,ztic,
     &     ratio,rat,radius,theta,phi,ifram,numaxi,nwidth,npow,nplace,
     &     nxstep,nystep,istyle,isup,
     &     fill,inter,axis,fillbad,autplt,cross,jopen)

	
99	continue
	goto 101
!	ians=gmdisplaymessagebox('','Show fitted dependency plot',gquestion,
 !    &gyesno)
	if(ians.eq.gnobutton) goto 101
	   xmin=tres1

	   ymin=tres1

	   nvdec=10


811	   continue		!return here to redraw
	   print 75,xmin,xmax
75	   format(' FITTED DEPENDENCY PLOT',/,
     &    ' Min and max shut time (ms) [',g11.4,',',g11.4,'] = ')
	   call INPUT2r(xmin,xmax)
	   print 76,ymin,ymax
76	   format('&Min and max open time (ms) [',g11.4,',',g11.4,'] = ')
	   call INPUT2r(ymin,ymax)
	   xmin=alog10(xmin)
	   xmax=alog10(xmax)
	   ymin=alog10(ymin)
	   ymax=alog10(ymax)
	   print 77,nvdec
77	   format(' Number of values per decade[',i4,'] = ')
	   call INPUTi(nvdec)
	   ns=1 + nvdec*ifixr(xmax-xmin)
	   no=1 + nvdec*ifixr(ymax-ymin)
	   nx=ns
	   ny=no
	   ALLOCATE(fdep(nx,ny),znew(nx,ny),
     &	shutt(nx),opent(ny))
	   if(allocated(badval)) DEALLOCATE(badval)
	   ALLOCATE(badval(nx,ny))
	   do i=1,nx
		do j=1,ny
		   badval(i,j)=.false.
		enddo
	   enddo
	   delt=(xmax-xmin)/float(ns-1)
	   do i=1,ns
		shutt(i)=xmin + (float(i-1))*delt		!log values
	   enddo
	   delt=(ymax-ymin)/float(no-1)
	   do i=1,no
		opent(i)=ymin + (float(i-1))*delt		!log values
	   enddo

	   call DEPEND(tres,k,kA,kF,ns,no,shutt,opent,fDEP,
     &	 zmin,zmax,
     &	 Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     &	 XAF,XFA,QexpQA,QexpQF,
     &	 kAm,kFm,km,irt)

	   zi=ifixr(zmin)
	   if(zi.gt.zmin) then
		zmin=zi - 1.0
	   else
		zmin=zi
	   endif
	   zi=ifixr(zmax)
	   if(zi.lt.zmax) then
		zmax=zi + 1.0
	   else
		zmax=zi
	   endif
	power=1.0
	ians=gmdisplaymessagebox('','Use 3D smoothing before display',
     & gquestion,gyesno)
	if(ians.eq.gyesbutton) then
	   call SMOOTH3D(fdep,znew,nbinx,nbiny,power,nbinx,nbiny)
	else
	   do i=1,nbinx
		do j=1,nbiny
		   znew(i,j)=fdep(i,j)
		enddo
	   enddo
	endif
	  

	   idrawt=1
	   idrawx=1
	   idrawy=1
	   idrawz=1
	   icol(23)=0		      !in common for gplot3d
	   icol(25)=9
	   icol(71)=7
	   xtitle='shut time'
	   ytitle='yopen time'
	   ztitle='dependency'
	   title1='Fitted dependency plot'
	   ndx=nx
	   ndy=ny
	   kcol=2
	   posneg=.true.
	isetcol=2
	igraph=igraph+1
	ixg=ixg+20
	iyg=iyg+20
	call gauto3d(iplotype,igraph,shutt,opent,znew,badval,nbinx,nbiny,ndx,
     & ndy,
     &	xtitle,ytitle,ztitle,title1,idrawt,idrawx,idrawy,idrawz,
     &    quarter,idev,plot,iplot,kcol,posneg,isetcol,i3col,
     &	wxmin,wxmax,wymin,wymax,vxlop,vxhip,vylop,vyhip,
     &    xlop,xhip,ylop,yhip,main,ixg,iyg,graph1_1,
     &    GraphMainPanel1_1,graphics1_1,ipos,gfile,ixposv,iyposv,
     &	oldrecords,nplot,ifnt,ifnl,alfa,beta,gama,delta,
     &     ijust,ijusx,ijusy,ijusz,
     &     xst,yst,xspx,yspx,xspy,yspy,xspz,yspz,numx,numy,
     &     xmin,xmax,ymin,ymax,zmin,zmax,xtic,ytic,ztic,
     &     ratio,rat,radius,theta,phi,ifram,numaxi,nwidth,npow,nplace,
     &     nxstep,nystep,istyle,isup,
     &     fill,inter,axis,fillbad,autplt,cross,jopen)
c
	   DEALLOCATE(fdep,znew,shutt,opent,badval)

101	continue
	if(allocated(px)) then
	   DEALLOCATE(px,py)
	endif
	if(allocated(dep)) then
	   DEALLOCATE(dep)
	endif
	if(allocated(depdif)) then
	   DEALLOCATE(depdif)
	endif
	if(allocated(yval)) then
	   DEALLOCATE(yval,ygap,xmid,ymid,x,y,z,znew)
	endif
	if(allocated(badval)) then
	   DEALLOCATE(badval)
	endif
c
1	xmax=xmax1
	ymax=ymax1
	RETURN
	END


