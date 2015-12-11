	function SCANFUNC(kmax,theta)
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
	real*4 delt(0:50)		!temp storage for transition search results
c	integer idt(100)
	real*4 yc(2),tival(2)
c	real*4 ycalc(1024)	!keep convolved trace for final display
	real*4 filt1(1024)
	real*4 ydcalc(2048)		!calc values from SCANFUNC (in common/ycal/)
	logical end
	logical debug,discprt
c	logical kbhit	!for debug
c	integer*2 ktype
c	character ch,getch
	logical abort
	logical fixamp,shutprev,opening(50)	!for fixamp
	integer ilong(50),mfix(50)		!for fixamp
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
	debug=.false.
c	debug=neval.eq.50		!show nth evaluation only
c Need differences between convolved and observed traces at the time points
c where there are observations (no point in calc conv points more closely
c than this).
	t0=theta(1)
c	if(debug) then
c	   write(8,700) t0
c700	   format(//,' t0 = ',g13.6)
c	endif
c Check for negative intervals.  Get problems in some cases where theta(i)
c itself goes negative, so adding 1 to it may still be neg!
	if(theta(1).le.1.) theta(1)=thmin(1)
	if(ntrans.gt.1) then
	   do i=2,ntrans
		if(theta(i).lt.theta(i-1)+thmin(i)) then
		   theta(i)=theta(i-1) + thmin(i)
		endif
c Define dt()
		j=i-1
		dt(j)=theta(i) - t0
c		idt(j)=ifixr(dt(j)/dtf1)	!corresp # of filter points
c		if(debug) then
c		   write(8,701) j,dt(j)
c701		   format(' j, dt(j) = ',i5,3x,g13.6)
c		endif
	   enddo
	endif
c
c If FIXAMP is true (auto-fitting only) then
c (1) Locate each opening that is < tshort (eg 2*trise)
c (2) For each such short opening check to find the nearest ylevel that
c	is both more negative (if opendown) AND longer than tlong (eg 3*trise)
c	-problem -more neg than what?!  -more neg than actual peak amp
c	 of data points-can tabulate this first to save time? -in ypeak(nt) say
c (3) If such an opening is found then set amplitude of the short
c	opening to that of the long opening -if a long one is NOT found
c	then use AVAMP for the short one
c Version when opendown=TRUE:
	if(fixamp) then
c first find the long openings -no point in proceeding if there are none
c Check levels in theta(nt+1) to theta(2*nt+1)
c First initialise mfix
	   do i=1,ntrans
		mfix(i)=-1
	   enddo
	   nlong=0
c Define interval lengths in delt()
	   do i=2,ntrans
		j=i-1  			!j=1,...,ntrans-1
		if(j.eq.1) then
		   delt(j)=dt(j)		!delt(j)=provisional tint() values
		else
		   delt(j)=dt(j)-dt(j-1)
		endif
	   enddo
c If initially open assume that the open level was 'long'
	   if(.not.shutprev) then	!initially open
		nlong=nlong+1
		ilong(nlong)=0	!note index of long openings
	   endif
	   do nt=1,ntrans-1
		if(opening(nt).and.delt(nt).gt.tlong) then
		   nlong=nlong+1
		   ilong(nlong)=nt	!note index of long openings
		endif
	   end do
c If open at end can assume that the bit on screen was long enough to
c produce a good estimate of open level
c===========NB start at end values could be set outside SCANFUNC as they
c==========do not depend on the current estimate of duration
	   if(opening(ntrans)) then
		nlong=nlong+1
		ilong(nlong)=ntrans	!note index of long openings
	   endif
	   if(nlong.eq.0) goto 9	!jump out
c -now find short openings; but do this only when opening is followed
c by shutting, eg for isolated openings, so require opening(nt+1)=false
	   do nt=1,ntrans-1	!NB fixamp true only if ntrans=>3
c		if(opening(nt).and.delt(nt).lt.tshort) then	!short opening found
		if(opening(nt).and.(.not.opening(nt+1)).and.
     &		delt(nt).lt.tshort) then
		   yshort=ypeak(nt)	!most neg data point for short opening
		   idif=ntrans	!max diff between index of short and long ops
		   jlong=-1		!in case not defined in loop below
		   do j=1,nlong	!choose nearest long opening
			i=ilong(j)
			if(theta(ntrans+i+1).lt.yshort.and.
     &				iabs(i-nt).lt.idif) then
			   jlong=i			!index of nearest long opening
			   idif=iabs(i-nt)	!update smallest difference
			endif
		   end do
c NOW RESET CURRENT LEVEL to that of the long opening -short opening #nt
c has amplitude in theta(ntrans+nt+1); nt=1,2,...,ntrans-1;
c jlong=0,1,...,ntrans (may refer to level before 1st or after last transition)
		   if(jlong.ge.0) then
			THETA(ntrans+nt+1)=THETA(ntrans+jlong+1)
			mfix(nt)=jlong
c=====		jfix(ntrans+nt+1)=0	!make sure it is not fixed
c======not easy to change jfix in middle of simplex -step lengths for
c=======the extra parameter introduced when jfix set to 0 will be crazy!
		   endif
c Record which amps so set so they can be marked on graph after fitting
c finished: set mfix(nt)=jlong when short opening nt is set to amplitude
c of opening jlong, or mfix(nt)=-1 if NOT set to anything: problem is that
c value may be so set in early part of fitting but as durations etc change
c may no longer be so set -initialise mfix to -1 each time fixamp invoked
		endif
	   end do
9	   continue
	endif		!end of FIXAMP scetion
c
c Calc step amplitudes in stepamp() (in prev integer version was called IA())
c=========NB if amplitudes are fixed this can be calc BEFORE fitting starts!
	j1=ntrans+1
	y0=theta(j1)	!initial level before trans #1
c	if(debug) then
c	   write(8,7011) y0
c7011	   format(' y0 = ',g13.6)
c	endif
	do j=1,ntrans
	  stepamp(j)=theta(j1+1)-theta(j1)
	  j1=j1+1
c	  if(debug) then
c	   write(8,702) j,stepamp(j)
c702	   format(' j, stepamp(j) = ',i5,3x,g13.6)
c	  endif
	enddo
c========need level AFTER last transition here?
c
c Next bit may be rather slow if finely-interp filter is used to avoid
c interpolation -try version in which stepamp is incorporated below as needed
c	do i=1,nfilt1
c	 do j=1,ntrans
c	   filter(i,j)=stepamp(j)*filt1(i)
c	 enddo
c	enddo
c But start and end values will be referred to frequently, so worth predefining
c them
c=========NB if amplitudes are fixed this can be calc BEFORE fitting starts!
	do j=1,ntrans
	  fend(j)=0.
	  do i=1,j
c	    fend(j)=fend(j)+filt1(nfilt1)*stepamp(i)	!filt(nfilt)=1 always!
	    fend(j)=fend(j) + stepamp(i)
	  enddo
c	  if(debug) then
c	    write(8,703) j,fend(j)
c703	    format(' j, fend(j) = ',i5,3x,g13.6)
c	  endif
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
	if(neval.lt.nevfix) then
	   if1=1 + ifix(t0/finter)	!index of 1st data point covered by filter
	endif
c
c Define constants for calc of ival (see below)
c=====no shortening possible??
c
c==	time=0.            !actual time (microsec)
	tend=t0 + float(nfilt1-1)*dtf1	!microsec to filt1(nfilt1)
c Loop through all data points starting at m=if1
	end=.false.		!set true when ival is beyond end of convolved response
	m=if1
	ssd=0.0
	iflag=0
	ycalc=0.
1	continue	!return here for next data point
	time=float(m-1)*finter        !time for data(m) (from data(1) at t=0)
c	if(time.gt.t0+dt(1)-1000.) debug=.true.	!===debug for 2nd trans
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
c	if(debug) then
c	   write(8,704) t0,tend,if1,m,ival,time,tival(j1)
c704	   format(' t0,tend,if1,m,ival,time,tival = ',/,2g13.6,3i5,2g13.6)
c	endif
5	continue		!return here for i=ival+1
c	if(debug) then
c	   write(8,713) j1,ival
c713	   format(/,' j1 = ',i5,' ival= ',i8)
c	endif
	yc(j1)=0.0
c
c Add in the first filter separately
c===============should use TIVAL not TIME here???
c===	if(time.lt.t0) then
	if(tival(j1).lt.t0) then
c	   if(debug) then
c	      write(8,810) time,tival(j1)
c810	      format(' First filter,time,tival = ',i5,2g13.6,': goto 10')
c	   endif
	   goto 10			!and so can jump out of jt loop now
c===	else if(time.gt.tend) then	!passed end of filter #1
	else if(tival(j1).gt.tend) then	!passed end of filter #1
	   iflag=1		!filter #1 is finished -assign yc=cumulative value
	   yc(j1)=fend(1)	!after end of filter #1, and go to next filter
c	   if(debug) then
c	      write(8,811) 1,yc(j1),time,tival(j1)
c811	      format(' jt,yc(j1),time,tival = ',i5,3g13.6,': goto 31')
c	   endif
	   goto 31
	else              !time at which resp is being calc is in range
	   yc(j1)=filt1(ival)*stepamp(1)
c	   if(debug) then
c	      write(8,805) ival,time,tival(j1),filt1(ival),y
c805	      format(
c     &	' filt 1 in range: ival,time,tival,filt1(ival),y = ',/,
c     &      2i5,5g13.6)
c	   endif
	endif
c end of filter #1
31	continue
c Now add other filters if ntrans>1
c NB if DT() values are not an exact number of FILTER points then
c must interpolate, to get fj(time-dt) for filter #jt, since
c t=time-dt may not correspond exactly with a tabulated point
	if(ntrans.gt.1) then
	  do 3 jt=2,ntrans
c
c===	   t=time-dt(jt-1)
	   t=tival(j1)-dt(jt-1)
	   if(t.lt.t0) then
c	   if(i.lt.1) then		!start of filter #jt not yet reached
c	     y=filter(1,jt)		!i.e. time at which response is being calc is
c	     y=0.0			!is before start of #jt, so only add zeroes
c	     if(debug) then
c	      write(8,710) jt,t
c710	      format(' jt,t = ',i5,g13.6,': goto 10')
c	     endif
	     goto 10			!and so can jump out of jt loop now
	   else if(t.gt.tend) then	!passed end of filter #jt
c	   else if(i.gt.nfilt1) then	!passed end of filter #jt
c	     y=filter(nfilt1,jt)
c	     y=fend(jt)
	     iflag=jt		!filter #jt is finished -assign yc=cumulative value
	     yc(j1)=fend(jt)	!after end of filter #jt, and go to next filter
c	     if(debug) then
c	       write(8,711) jt,yc(j1),t
c711	       format(' jt, yc(j1),t = ',i5,2g13.6,': goto 3')
c	     endif
	     goto 3
	   else              !time at which resp is being calc is in range
c		Interp in filter to find value at time t
		iv1=1 + ifix((t-t0)/dtf1)
		tiv1=t0 + float(iv1-1)*dtf1		!time for filt(iv1)
	      if(iv1.ge.nfilt1) then
c		  y=filt1(nfilt1)*stepamp(jt)		!filt(nfilt)=1.0 always
		  y=stepamp(jt)
c		  if(debug) then
c		     write(8,712) jt,iv1,tiv1,y
c712		     format(' iv1>nfilt1: jt,iv1,tiv1,y = ',/,2i5,2g13.6)
c		  endif
	      else
	         b=(filt1(iv1+1)-filt1(iv1))/dtf1
		   y=(filt1(iv1) + b*(t-tiv1))*stepamp(jt)
c		   if(debug) then
c		     write(8,705)
c     &		jt,iv1,time,tival(j1),t,filt1(iv1),filt1(iv1+1),
c     &		filt1(iv1)*stepamp(jt),filt1(iv1+1)*stepamp(jt),y
c705		     format(
c     &		' jt,iv1,time,tival,t = ', 2i5,3g13.6,/,
c     &		' filt1(iv1),filt1(iv1+1) = ',2g13.6,/,
c     &		' filt1(iv1)*step,filt1(iv1+1)*step,y = ',3g13.6)
c		   endif
	      endif
	   endif
c
 	   yc(j1)=yc(j1) + y		!add JTth filter
c	   if(debug) then
c	      write(8,706) jt,j1,yc(j1)
c706	      format(' for filt ',i3, ': j1,yc(j1) = ',i5,g13.6)
c	   endif
3	  continue		!end of jt=2,ntrans loop
	endif		!end of ntrans>1 bit
c
10	continue
c yc() now finished for current time point, except for addition of initial level
	yc(j1)=yc(j1) + y0
c	if(debug) then
c	   write(8,707) j1,yc(j1)
c707	   format(' final j1,yc(j1) = ',i5,g13.6)
c	endif
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
c	if(debug) then
c	   write(8,708) yc(1),yc(2),time,tival(1),tival(2),ycalc
c708	   format(' yc(1),yc(2),time,tival(1),tival(2),ycalc = ',/,6g13.6)
c	endif
c
c Calculate the SSD
2	continue
	ssd=ssd + (ycalc-data(m))**2
	ydcalc(m)=ycalc		!calc value corresponding to data(m)
c	if(debug) then
c	   write(8,709) m,data(m),ycalc,ssd
c709	   format(' m,data(m),ycalc,ssd = ',i5,3g13.6,//)
c	endif
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
	scanfunc=ssd
c For debug put abort here
c NB key F1 has ktype=0, scan code=59 (see \fortran\tkey.for)
c	if(KBHIT()) then
c		ch=GETCH(ktype)
c		if(ktype.eq.0.and.ichar(ch).eq.59) abort=.true.	!abort on SIMPLEX
c	endif
	RETURN
	end

