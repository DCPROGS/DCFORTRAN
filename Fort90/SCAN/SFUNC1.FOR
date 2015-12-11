	function SFUNC1(theta)
c Version of SCANFUNC to calculate SSD to be minimised by DFPMIN in SCAN
c NB must have number of points fixed, and amplitudes for short events
c fixed (e.g. via preliminary call to simplex) before calling dfpmin)
c (SFUNC1-test version for use with tdf1.for)
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
c Uses FILT1 (in common) for step response function (interp at fine intervals
c =dtf1 microsec, in NORMFILT)
c
c (For variants of convolution calc see CONV1, CONV31 and FUNCIN (PDP)).
c
c Parameters are in THETA
c Parameters:
c theta(1)= t0 = time in microsec from 1st point on display TO 1st transition
c theta(2)= time in microsec from 1st point on display TO 2nd transition
c . . .
c theta(nt)= time in microsec from 1st point on display TO (nt)th transition
c theta(nt+1)='amplitude' (floating Y units) before 1st trans
c theta(nt+2)='amplitude' (ditto) between 1st and 2nd trans
c theta(nt+3)='amplitude' (ditto) between 2nd and 3rd trans
c . . .
c theta(2*nt)='amplitude' (ditto) between (nt-1)th and nt'th transition
c theta(2*nt+1)='amplitude' (ditto) after (nt)th transition
c
c (some of these may be fixed!)
	real*4 data(2048),theta(50)
	real*4 stepamp(50),dt(49),fend(50)
c	integer idt(100)
	real*4 yc(2),tival(2)
	real*4 filt1(1024)
	real*4 ydcalc(2048)	!calc values from SCANFUNC (in common/ycal/)
	logical end
	logical discprt
c	logical kbhit
c	integer*2 ktype
c	character ch,getch
	logical abort
c To interpolate separate step resp func for dsfunc
	real*4 yc1(2,50)
	logical first(50)
	integer ind(50)
c
	common/abt/abort			!common with simplex1
c	common/best/thmin(50)		!common with simplex1
	common/dp/discprt
	common/fitblk/data,if1,if2,ntrans,neval,nevfix,itype,stepamp,y0
	common/ycal/ydcalc	!calc values from SCANFUNC
	common/filtblk/filt1,nfilt1,dtf1,npfilt,finter,filt(640),filtf
c	common/funcs/srfunc(2048,50)	!to save separate step resp func for dsfunc
c To save separate step resp func for dsfunc
	common/funcs/srfunc(200,50),ifst(50),npnt(50)
c
	neval=neval+1
c Need differences between convolved and observed traces at the time points
c where there are observations (no point in calc conv points more closely
c than this).
	t0=theta(1)
c Check for negative intervals.  Get problems in some cases where theta(1)
c itself goes negative, so adding 1 to it may still be neg!
c	if(theta(1).le.1.) theta(1)=thmin(1)
	if(theta(1).le.1.) theta(1)=1.
	if(ntrans.gt.1) then
	   do i=2,ntrans
c		if(theta(i).le.1.) theta(i)=thmin(i)
		if(theta(i).le.1.) theta(i)=1.
		if(theta(i).lt.theta(i-1)) then
c		   theta(i)=theta(i-1) + 10.		!minimum interval=10 mus
		   theta(i)=theta(i-1) + 1.		!minimum interval=1 mus
		endif
c Define dt() (ntrans-1 values)
		j=i-1
		dt(j)=theta(i) - t0
	   enddo
	endif
c
c (fixamp section removed from here)
c
c Calc step amplitudes in stepamp() (in prev integer version was called IA())
c NB if amplitudes are fixed this can be calc BEFORE fitting starts!
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
c (nevfix section removed)
c Define constants for calc of ival (see below)
c
	tend=t0 + float(nfilt1-1)*dtf1	!microsec to filt1(nfilt1)
c Loop through all data points starting at m=if1
	end=.false.		!set true when ival is beyond end of convolved response
	ssd=0.0
	iflag=0
	ycalc=0.
	do jt=1,ntrans
	   first(jt)=.true.
	enddo
	m=if1
c
c START OF DATA (TIME) POINT LOOP
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
c
c Return to 5 for 2nd point to inetrpolate between (NB interpolation section
c uses j1=1,2; ival,ival+1,yc(1), yc(2)
5	continue		!return here for i=ival+1
c
	yc(j1)=0.0
	do j=1,ntrans
	   yc1(j1,j)=0.0
	enddo
c
c Add in the first filter separately
	jt=1		!first step resp junction
	if(tival(j1).lt.t0) then
	   goto 10			!and so can jump out of jt loop now
	else if(tival(j1).gt.tend) then	!passed end of filter #1
	   iflag=1		!filter #1 is finished -assign yc=cumulative value
	   yc(j1)=fend(1)	!after end of filter #1, and go to next filter
	   goto 31
	else              !time at which resp is being calc is in range
	   yc(j1)=filt1(ival)*stepamp(1)
	   if(first(jt)) then
		ind(jt)=0
		ifst(jt)=m
		first(jt)=.false.
	   endif
	   yc1(j1,jt)=yc(j1)
	endif
c end of filter #1
31	continue
c Now add other filters if ntrans>1
c NB if DT() values are not an exact number of FILTER points then
c must interpolate, to get fj(time-dt) for filter #jt, since
c t=time-dt may not correspond exactly with a tabulated point
	if(ntrans.gt.1) then
	  do 3 jt=2,ntrans
	   t=tival(j1)-dt(jt-1)
	   if(t.lt.t0) then
	     goto 10			!and so can jump out of jt loop now
	   else if(t.gt.tend) then	!passed end of filter #jt
	     iflag=jt		!filter #jt is finished -assign yc=cumulative value
	     yc(j1)=fend(jt)	!after end of filter #jt, and go to next filter
	     goto 3
	   else              !time at which resp is being calc is in range
c		Interp in filter to find value at time t
		iv1=1 + ifix((t-t0)/dtf1)
		tiv1=t0 + float(iv1-1)*dtf1		!time for filt(iv1)
	      if(iv1.ge.nfilt1) then
c		  y=filt1(nfilt1)*stepamp(jt)		!filt(nfilt)=1.0 always
		  y=stepamp(jt)
	      else
	         b=(filt1(iv1+1)-filt1(iv1))/dtf1
		   y=(filt1(iv1) + b*(t-tiv1))*stepamp(jt)
	      endif
	      yc1(j1,jt)=y
		if(first(jt)) then
		   ind(jt)=0
		   ifst(jt)=m
		   first(jt)=.false.
		endif
	   endif
c
 	   yc(j1)=yc(j1) + y		!add JTth filter
c
3	  continue		!end of jt=2,ntrans loop
	endif		!end of ntrans>1 bit
c
10	continue
c yc() now finished for current time point, except for addition of initial level
c
	yc(j1)=yc(j1) + y0
c
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
c Now do interpolation of step resp, at current time point, separately
c for each transition (for dsfunc)
c=======
c Add bit to interpolate y value for each step resp function separately
c for use in dsfunc
	do jt=1,ntrans
	   if(m.ge.ifst(jt).and.iflag.lt.jt) then
		b=(yc1(2,jt)-yc1(1,jt))/dtf1
		ind(jt)=ind(jt)+1
		srfunc(ind(jt),jt)=yc1(1,jt) + b*(time-tival(1))
		npnt(jt)=ind(jt)	!=# of points recorded on exit
	   endif
	enddo
c=======
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
c
	end=iflag.eq.ntrans	!ival is beyond end of convolved response
c (nevfix removed here)
	if(m.lt.if2) then
	   m=m+1
	   goto 1		!next obs (time point)
	endif
c	if(m.eq.if2) goto 99		!data points fixed
c
99	continue
c Finish now
c==	ssd=ssd/float(if2-if1+1)	!normalise in case # of data points varies
c
	SFUNC1=ssd
c
c NB key F1 has ktype=0, scan code=59 (see \fortran\tkey.for)
c	if(KBHIT()) then
c	   ch=GETCH(ktype)
c	   if(ktype.eq.0.and.ichar(ch).eq.59) abort=.true.	!abort on SIMPLEX
c	endif
	RETURN
	end

