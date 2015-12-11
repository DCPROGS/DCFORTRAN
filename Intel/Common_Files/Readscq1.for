	subroutine READSCQ1(iptype,jdat,inp,krn,nrec1,dx,srate,
     & xmin,xmax,useconsam,adcfil,ioff,istart,calfac,calfac2,ndisp1,
     & ncurvd,ncurvc,icurvd,icurvc,xval1,yval1,xcal,ycal,ndat1,isym1,
     & ncal1,ijoin1,syms1,
     & ycalc,y0,ntrans,DT,ke,filt,nfilt,stepamp,dtf,tif1,tif2,t0,
     & ntx,nty,itx,ity,titlex,titley,ilabel,
     & ndv1,ndimd,ndc1,ndimc)

c	subroutine READSCQ1(iptype,jdat,intp,krn,nrec1,dx,srate,
c     & xmin,xmax,useconsam,adcfil,ioff,istart,calfac,calfac2,ndisp1,
c     & ncurvd,ncurvc,icurvd,icurvc,xval1,yval1,xcal,ycal,ndat1,isym1,
c     & ncal1,ijoin1,syms1,
c     & ycalc,y0,ntrans,DT,ke,filt,nfilt,stepamp,dtf,tif1,tif2,t0,
c     & ntx,nty,itx,ity,titlex,titley,ilabel,
c     & ndv1,ndimd,ndc1,ndimc)

c To do 2nd read of data values from disk in plot of single channels in AUTPLOT
c
	real*4 XVAL1(ndv1,ndimd),YVAL1(ndv1,ndimd)
	real*4 XCAL(ndc1,ndimc),YCAL(ndc1,ndimc)
	integer ndat1(ndimd),isym1(ndimd),ncal1(ndimc),ijoin1(ndimd)  !for VPLOT
	integer icurvd(ndimd),icurvc(ndimc)
	real syms1(ndimd)
c    for calc curve
	real*4 stepamp(50),dt(49),filt(640)
	real*4 ycalc(5120)
	ALLOCATABLE idata
	integer*2 idata(:)
	character adcfil*33	!path for consam file
	character titlex*40,titley*40
	logical useconsam
c
	ALLOCATE(idata(jdat))
	intp=1		!no longer needed

	dx=1000./srate		!ms between points
	xmin=0.
	xmin1=0.
	xmax=xmin+float(jdat)*dx
	jdat1=jdat		!jdat1 no longer needed

	if(useconsam) then

	   OPEN(unit=14,file=adcfil,status='UNKNOWN',
     &       access='DIRECT', form='BINARY', RECL=1)
	   irec=ioff + (2*istart-1)    !record # for 1st value to be read

	   read(14,rec=irec) (idata(i),i=1,jdat1)
	   CLOSE(unit=14)
	   cfac=calfac	!CONSAM units to pA
	else	!not from consam: read data from PLOTQ
	   n=1
	   do j=1,nrec1         !read back into idata(1),...,idata(ndisp1)
		m=n+511
		if(m.gt.ndisp1) m=ndisp1
		krn=krn+1024
		read(11,rec=krn) (idata(i),i=n,m)
		n=n+512
	   enddo
	   cfac=calfac2	!intermed idata units in SCAN to pA
	endif
c Define Xval, Yval
	k=1
	do i=1,jdat1,intp
	   Yval1(k,1)=cfac*float(idata(i))
	   xval1(k,1)=xmin1+float(i-1)*dx
	   k=k+1
	enddo
	ndat1(1)=k-1
c	if(debug()) then
c	   print *,' intp,jdat1,k-1,irec,jdat = ',intp,jdat1,
c     &   k-1,irec,jdat
c	   pause
c	endif
	irec=irec+(2*jdat1) ! Disk read is in bytes
c==========what are next 2 lines!!
c	jdat=jdat-jdat1
c	xmin1=xval1(k-1,1)
c Define calc curve, if queued
	ncurvc=0
	if(iptype.eq.31.or.iptype.eq.32) then
c Calculate convolution in ycalc(1) to ycalc(ke) (NB y0,stepamp, and so calc
c curve, always in intermed units whether using CONSAM or data queued so
c scale to pA with calfac2.
	   call CONV1(ycalc,y0,ntrans,DT,ke,filt,nfilt,stepamp,dtf)
	   if(ke.gt.5120) ke=5120
c If baseline fitted at either end, add the horizontal bits between
c points if1 and if2, times of which (in microsec) are queued as tif1,tif2.
c In order to do this, define xcal() to start at t=0, rather than
c at t=t0, to leave room for extra flat bit at the start -problem
c is that we want an X value at exactly t0 (which will not generally
c be an exact multiple of dtf) so do as follows
	   i1=1
	   if(tif1.lt.t0) then	!need to add flat bit at start
		t=tif1
		do while(t.le.t0)
		   t=tif1+float(i1-1)*dtf		!starts at tif1
		   xcal(i1,1)=t*1.e-3		!in ms
		   ycal(i1,1)=ycalc(1)*calfac2	!in pA
		   i1=i1+1
		enddo
	   endif
c		After leaving this loop i1 is index of next point in Xcal,ycal
c		so next put in the conolved curve starting at this point
		i2=0
	   do i=i1,ke+i1-1
		i2=i2+1		!i2=1,2,....,ke
		t=t0 + float(i2-1)*dtf			!in microsec; starts at t0
		xcal(i,1)=1.0e-3*t				!in ms
		ycal(i,1)=ycalc(i2)*calfac2		!in pA
	   enddo
c         After this loop index of next point in Xcal,Ycal is ke+i1
	   t1=t		!last time defined above
	   i1=ke+i1
	   if(tif2.gt.t1) then
	      i2=1
	      t=t1
	      do while(t.le.tif2)
		  t=t1+float(i2-1)*dtf
		  xcal(i1,1)=t*1.e-3		!in ms
		  ycal(i1,1)=ycalc(ke)*calfac2	!in pA
		  i1=i1+1
		  i2=i2+1
		enddo
	   endif
	   ncurvc=1
	   icurvc(1)=1
c	   ncal1(1)=ke
	   ncal1(1)=i1-1
	endif
c
20	continue
	ntx=-1000	!calibration bars for X (rather than axes)
	nty=-1000	!calibration bars for Y (rather than axes)
	itx=1
	ity=1
c
	titlex=' ms                                     '		!units only
	titley=' pA                                     '		!units only
	ilabel=1
c Read data from CONSAM
	ncurvd=1
	icurvd(1)=1
	ijoin1(1)=0		!points joined
	syms1(1)=2.5
	isym1(1)=0		!points
c When calc curve plotted, show data as small circles, points not joined
	if(iptype.eq.31.or.iptype.eq.32) then
	   isym1(1)=-7		!filled circle
	   syms1(1)=0.6
	   ijoin1(1)=-1	!don't join
	endif
c
	DEALLOCATE(idata)
	RETURN
	end

