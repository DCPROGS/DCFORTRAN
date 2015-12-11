	function FCFUNC(kmax,theta)
c===check on thmin() -surely not defined!!
c Version of SCANFUNC that fits a single opening or shutting transition
c with filter cut-off frequency as a parameter
c NB For one transition only -superfluous code removed
c
c Use fixed number of points (nfilt1, dtf1) ransmitted via common
c from INSCAN, but recalc filt1() each iteration with current fc
c
c To calculate the value to be minimized for transition fitting in SCAN.
c NB VERY IMPORTANT to have kbhit() outside the loop -causes impossible
c slowing otherwise (can it be omitted from here altogether?)!
c
c Data is in DATA(if1) to DATA(if2)
c If amplitudes are being fitted then will want some baseline before and
c after the channel transitions, so better to work with fixed data points
c (DATA(if1) to DATA(if2)).  However if working with fixed pre-estimated
c amplitudes, as in original SCAN, then want data points not to be fixed,
c at least in the earlier stages of fitting.
c
c Keep all times in microseconds with t=0 for data(1)
c Thus data(i) is at time=float(i-1)*finter.
c Time of transition (actually=time of 1st point in convolved
c	resp function(CRF)) is at t=t0 (relative to DATA(1))
c so time for the jth point of the CRF is t=t0 + float(j-1)*dtf1
c (CRF is set to initial level, y0, for all t=<t0)
c
c Parameters are in THETA
c Parameters:
c theta(1)= t0 = time in microsec from 1st point on display TO 1st transition
c theta(2)='amplitude' (floating Y units) before 1st trans
c theta(3)='amplitude' (ditto) between 1st and 2nd trans
c theta(4)=fc in kHz
c
	real*4 data(2048),theta(50)
	real*4 stepamp(50),fend(50)
	real*4 yc(2),tival(2)
	real*4 filt1(1024)
	real*4 ydcalc(2048)		!calc values from SCANFUNC (in common/ycal/)
	logical end
	logical discprt
c	logical debug
c	logical kbhit	!for debug
c	integer*2 ktype
c	character ch,getch
	logical abort
	logical fixamp,shutprev,opening(50)	!for fixamp
	integer mfix(50)		!for fixamp
	real*4 ypeak(50)			!for fixamp
c
	common/abt/abort			!common with simplex1
	common/minlen/thmin(50)		!common with scanfunc fitsub, fitsub6
	common/dp/discprt
	common/fitblk/data,if1,if2,ntrans,neval,nevfix,itype,stepamp,y0
	common/ycal/ydcalc	!calc values from SCANFUNC
	common/fixblk/fixamp,tshort,tlong,ypeak,opening,shutprev,mfix
	common/filtblk/filt1,nfilt1,dtf1,npfilt,finter,filt(640),filtf
c
	neval=neval+1
c	debug=.true.
c	debug=.false.
c	debug=neval.eq.50		!show nth evaluation only
c Need differences between convolved and observed traces at the time points
c where there are observations (no point in calc conv points more closely
c than this).
	t0=theta(1)
c Check for negative intervals.  Get problems in some cases where theta(i)
c itself goes negative, so adding 1 to it may still be neg!
	if(theta(1).le.1.) theta(1)=thmin(1)
c
c Now calculate the filter from current fc=theta(4)
c Don't need to use normfilt -just use code from it to calc filt1()
c with current fc value thus (use ficed dtf1, nfilt1)
	fc=theta(4)			!in kHz
	dtf1=1.0 		!1 microsec regardless of fc
	tmin=-409.4718/fc	!filtf=fc in kHz, so tmin gives filt1=0.001
	tmax=-tmin		!so tmax gives filt1()=0.999
90	nfilt1=1 + ifix((tmax-tmin)/dtf1)
	if(nfilt1.gt.1024) then
	   dtf1=dtf1*1.1
	   goto 90
	endif
	do i=1,nfilt1
	   t=(tmin + float(i)*dtf1)
	   filt1(i)=0.5*(1.0 + erf(5.336446e-3*fc*t))
	enddo
c
c Calc step amplitudes in stepamp() (in prev integer version was called IA())
	j1=ntrans+1
	y0=theta(j1)	!initial level before trans #1
	do j=1,ntrans
	  stepamp(j)=theta(j1+1)-theta(j1)
	  j1=j1+1
	enddo
c
	do j=1,ntrans
	  fend(j)=0.
	  do i=1,j
c	    fend(j)=fend(j)+filt1(nfilt1)*stepamp(i)	!filt(nfilt)=1 always!
	    fend(j)=fend(j) + stepamp(i)
	  enddo
	enddo
c
c Number of data points
c (1) If data points are not fixed, then calculate if1=index of the first
c	data point, data(if1), that is covered by the CRF for the current
c	t0 estimate.  This will be the first data point used, and the index=if2
c	of the last data point used is determined below by the length of the CRF
c	with the current param values (it is the last data point covered by the
c	current CRF). The values of if1 and if2 are in common, for use if the
c	number of data points reverts to being fixed (after nevfix evaluations)
c (2) If data points are fixed (from start, or after nevfix evaluations of
c	this function), data(if1) to data(if2) are used (if1,if2 in common)
c
c Define constants for calc of ival (see below)
	tend=t0 + float(nfilt1-1)*dtf1	!microsec to filt1(nfilt1)
c Loop through all data points starting at m=if1
	end=.false.		!set true when ival is beyond end of convolved response
	m=if1
	ssd=0.0
	iflag=0
	ycalc=0.
1	continue	!return here for next data point
	time=float(m-1)*finter        !time for data(m) (from data(1) at t=0)
	if(time.lt.t0) then
	   ycalc=y0
	   goto 2				!calc SSD
	endif
	if(end) goto 2		!ycalc unchanged after end of CRF
c Calc ival=index of the filter point just BEFORE the mth data point
c then calculate value of convolved response and interpolate value
c at t=time between points ival, ival+1 of it. (NB reach here only
c if time>t0)
c Now loop through all filters at each time point
	j1=1
	ival=1 + ifix((time-t0)/dtf1)
	tival(j1)=t0 + float(ival-1)*dtf1		!time for filt(ival)=yc(1)
5	continue		!return here for i=ival+1
	yc(j1)=0.0
c
c Add in the first filter separately
	if(tival(j1).lt.t0) then
	   goto 10			!and so can jump out of jt loop now
	else if(tival(j1).gt.tend) then	!passed end of filter #1
	   iflag=1		!filter #1 is finished -assign yc=cumulative value
	   yc(j1)=fend(1)	!after end of filter #1, and go to next filter
	   goto 31
	else              !time at which resp is being calc is in range
	   yc(j1)=filt1(ival)*stepamp(1)
	endif
c end of filter #1
31	continue
cc Now add other filters if ntrans>1
c
10	continue
c yc() now finished for current time point, except for addition of initial level
	yc(j1)=yc(j1) + y0
	if(j1.eq.1) then	!repeat for i=ival+1
	   j1=2
	   ival=ival+1
	   tival(2)=tival(1)+dtf1		!time for filt(ival)=yc(1)
	   goto 5
	endif
c
c Now have yc(1) and yc(2) = point # ival and ival+1 in convolved response
c Next interpolate linearly to get response at t=time
c linear interp
	b=(yc(2)-yc(1))/dtf1
	ycalc=yc(1) + b*(time-tival(1))
c
c Calculate the SSD
2	continue
	ssd=ssd + (ycalc-data(m))**2
	ydcalc(m)=ycalc		!calc value corresponding to data(m)
c Now repeat for next data point, unless finished.
c If data points are changing (neval.lt.nevfix) then fit only up to the
c   end of the convolved response function (e.g. as when amplitude after
c   last transition is pre-specified, not estimated here)
c If data points are fixed then carry on up to the last data point (data(if2))
c even though this may be well past end of response function
	end=iflag.eq.ntrans	!ival is beyond end of convolved response
	if(neval.le.nevfix) then	!data points varying
	   if(end) then
		if2=m			!record index of last data point used
		goto 99
	   endif
	else
	   if(m.eq.if2) goto 99		!data points fixed
	endif
	m=m+1
	goto 1		!next obs
c
99	continue
c Finish now
	ssd=ssd/float(if2-if1+1)	!normalise in case # of data points varies
c
	fcfunc=ssd
c For debug put abort here
c NB key F1 has ktype=0, scan code=59 (see \fortran\tkey.for)
c	if(KBHIT()) then
c		ch=GETCH(ktype)
c		if(ktype.eq.0.and.ichar(ch).eq.59) abort=.true.	!abort on SIMPLEX
c	endif
	RETURN
	end

