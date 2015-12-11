	subroutine MULTRACE(yval,mono,xmin,xmax,ymin,ymax,idev,
     & ndatj,ijoinj,icolj,lthj,ndv1,ndimd,ntrace,ytsep,
     & ibk,imode)
c     & reclen,ibk,imode)
c	subroutine MULTRACE(j,xval,yval,ndelt,isdev,
c     & logity,logx,logy,sqrty,y0,yinf,x0,mono,
c     & xmin,xmax,ymin,ymax,iud,tload,idev,
c     & ndatj,ijoinj,icolj,isymj,symsj,lthj,lthj,
c     & lth27,lth27,lth30,lth30,barleng,interp,npint,
c     & Xint,Yint,nint,yp1,ypn,Y2int,ndv1,ndimd
c     & ntrace,ytsep,ibk)
c
c To draw trace (eg single channel data) as several rows.
c Works for j=1 only, and assumes equally=spaced x values
c Call with ibk=icol(71)=background col for graph, to erase trace
c [Params for DRAWDAT (lines 1-6 in call)]
c  IMODE=0 -asks for number of traces etc
c  IMODE=1 -draws traces only, using input values of ntrace, ytsep
c  IMODE=2 -ditto, but does not erase graph area first.
c  reclen=duration of the whole trace = xval(ndat(1))-Xval(1) in call.
c
c ijoinj=ijoin(1) in call is now set to 1 internally because no points
c drawn here, only lines and if queued single channel record has dots
c only (as when a fitted curve is superimposed on it), so ijoin(1)=-1,
c get nothing at all here.  The value needs to be passed back to VPLOT
c so redrawn correctly.
c===========NB orig data may not cover whole width up to xmax
c======== -up to xval(ndat(1)) only: to get new time calib must use
c======== original dx value
c
c========rest viewport/graphboundary to full width (option in graph shape
c========and/or do automatically?)
c========use RESCALE so screen is in new pA units? (will have to alter
c======= scaling offset used below!)
	ALLOCATABLE::XVAL1
	real*4 YVAL(ndv1,ndimd)
	real*4 Xval1(:)
	integer*2 lthj
	logical mono
c	integer*2 lthj,lthj,lth27,lth27,lth30,lth30
c	logical tload,interp,logity,logx,logy,sqrty,mono
c other params
	character defolt*30,cans*30		!to hold text & result of QDIALOG
c
c==========make 'draw only mode'
c
	j=1		!first trace only for now
	ijoinj=1
c Find min,max for trace 1
	ymin1=1.e37
	ymax1=-1.e37
	do i=1,ndatj
	   yv=yval(i,j)
	   if(yv.lt.ymin1) ymin1=yv
	   if(yv.gt.ymax1) ymax1=yv
	enddo
c
	if(imode.ge.1) then	!draw only
	   ymin2=ymin+0.1*(ymax-ymin)		!leave 10% at bottom
	   gain=(ymax-ymin2)/(float(ntrace)*(ymax1-ymin1))
	   yofftop=ymax-gain*ymax1
	else
	   n=3
 	   call DEFOLTi(n,defolt)
	   call QDIALOG(1,
     &    'Split data for trace 1 into n rows: n',
     &	defolt,11,cans)
	   call GETINPi(cans,n)
	   ntrace=n
	endif
c
	if(ntrace.eq.1) then
	   npt=ndatj
	   nptlast=npt
	else
	   npt=ndatj/n + 1
	   nptlast=ndatj-npt*(n-1)
	endif
c Calc dx value for new Xval
	dx=(xmax-xmin)/float(npt-1)
c
	ALLOCATE(xval1(npt))
c Define xval1
	do i=1,npt
	   xval1(i)=xmin + float(i-1)*dx
	enddo
c
c Calc gain and separation so ymin1=ymin, lowest displayable trace
c -but use ymin2 rather than ymin, to leave (10%) space at bottom for
c calibration bars.
	if(imode.eq.0) then
	   ymin2=ymin+0.1*(ymax-ymin)		!leave 10% at bottom
	   ytsep=ymax1-ymin1
	   gain=(ymax-ymin2)/(float(ntrace)*ytsep)
	   ytsep=gain*ytsep			!suitable first guess?
c Calc offset so that ymax1=ymax, largest displayable point
c  (after mult by gain, highest point in tarce is gain*ymax1)
	   yofftop=ymax-gain*ymax1
	endif
c
c Delete current data set 1; call DRAWDAT with colour=background, ie
	if(imode.ne.2) then
	  if(mono) then
	    call FILLWIN(0,55,639,387,0)		!graph area
	  else
	    call FILLWIN(0,55,639,387,ibk)		!graph area
	  endif
	endif
cc ibk replaces icolj in call
c	call DRAWDAT(j,1,xval,yval,ndelt,isdev,
c     & logity,logx,logy,sqrty,y0,yinf,x0,mono,
c     & xmin,xmax,ymin,ymax,iud,tload,idev,
c     & ndatj,ijoinj,ibk,isymj,symsj,lthj,lthj,
c     & lth27,lth27,lth30,lth30,barleng,interp,npint,
c     & Xint,Yint,nint,yp1,ypn,Y2int,ndv1,ndimd)
c
c Now redraw as ntrace traces
	linetype=ijoinj
	iud=1				!draw line between points
	if(linetype.eq.-1) iud=0	!don't
	ic=icolj
	if(mono) ic=15		!bright white
	if(.not.mono) call COLTYP1(icolj)	!restore colour after SYMBOL
	call LINWID(int4(lthj))
	iy1=1		!element of yval to start at
	do j=1,ntrace
	   yoff=yofftop-float(j-1)*ytsep
	   n2=npt
	   if(j.eq.ntrace) n2=nptlast
	   jflag=0
	   do i=1,n2
		xv=xval1(i)
		i1=i+iy1-1
		yv=yoff + gain*yval(i1,1)     !NB j=index of loop!
		if(xv.ge.xmin.and.xv.le.xmax.and.
     &		yv.ge.ymin.and.yv.le.ymax)then
		   if(jflag.eq.0) then
			call MOVETO(xv,yv,0,0)   !move to 1st point in range
		   else
	  		call MOVETO(xv,yv,iud,linetype)   !join with line from last point
		   endif
		   jflag=1
		endif
	   enddo
	   iy1=iy1+npt
	enddo
c
c Draw calibration bars
c Care needed because, at present, the x,y scaling is unchanged so does
c not represent real pA or ms!  If tic is drawn the same length, the time
c it represents is smaller by a factor rowlen/reclen which is unlikely
c to be an exact integer; it is the factor by which x axis units are wrong
c time.
c Will have problem when this is redrawn! -how can LAXES2 get the correct
c calibration bars! -but if this not solved, then can't queue a multitrace
c plot at all.  Probably essential to rescale to correct units here, and
c this will change all the drawing of the traces as well as xmin, xmax etc!?
c==============???OR make new versions of DCALBAR/FIXCALBAR with an
c option to deal with multitrace plots?
c	tfac=float(npt-1)/float(ndatj-1)
c	rowlen=reclen*tfac	   !length of row of npt points
c	call FIXAX(0.,rowlen,x1,x2,x,0)	!get suitable tic length
c	xcalib=x			!one major tick in ms
c	xcalib=xcalib/tfac	!correct for wrong scaling
c	rx(2)=rx(1)+xcalib
c	call FIXCALIB(titlex,x)		!change number in title
cc
c	ry(4)=ry(3)+ycalib
c	call FIXCALIB(titley,ycalib)		!change number in title
cc     (horizontal and vertical so ry(1)=ry(2), and rx(3)=rx(4)
cc=============
	if(imode.ge.1) goto 99		!draw only
c Adjust number of traces/separation etc, and redraw
c===========
c	call ANYKEY		!for debug
99	continue
	DEALLOCATE(xval1)
	RETURN
	end
