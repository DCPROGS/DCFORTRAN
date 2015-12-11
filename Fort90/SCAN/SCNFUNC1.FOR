	function SCNFUNC1(kmax,theta)
c To calculate the value to be minimized for transition fitting in SCAN.
c
c SCNFUNC1 is version of SCANFUNC for ifitmode=1 (auto-1-amp). This
c supposes one shut level and one open level so open and shut alternate.
c Actually estimate on open level (same for all ops), plus:
c  (1) when shut before and after, estimate pre- and post- baseline separately,
c and constrain all shut levels (if ntrans>2) to be mean of their current
c values.
c  (2) when shut only before, or only after, estimate one baseline level,
c and constrain all shut levels (if ntrans>2) to be same as this level.
c  (3) when open at each end, FIX all shut levels at current baseline estimate.
c
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
c plus:
c  (1) when shut before and after:
c theta(nt+1)=open level
c theta(nt+2)=baseline before 1st trans
c theta(nt+3)=baseline after last transition
c  (2) when shut initially, open at end
c theta(nt+1)=open level
c theta(nt+2)=baseline before 1st trans
c  (3) when open initially, shut at end
c theta(nt+1)=open level
c theta(nt+2)=baseline after last trans
c  (4) when open at each end
c theta(nt+1)=open level
c theta(nt+2)=baseline (FIXED parameter)
c
c Define before entry:
c itype=1 if shut before and after
c itype=2 if shut before only
c itype=3 if shut after only
c itype=4 if open at both ends
c
	real*4 data(2048),theta(50)
	real*4 stepamp(50),dt(49),fend(50)
	real*4 ydcalc(2048)		!calc values from SCANFUNC (in common/ycal/)
c	integer idt(100)
	real*4 yc(2),tival(2)
c	real*4 ycalc(1024)	!keep convolved trace for final display
	real*4 filt1(1024)
	logical end
	logical debug,discprt
c	logical kbhit	!for debug
c	integer*2 ktype
c	character ch,getch
	logical abort
c
	common/abt/abort			!common with simplex1
	common/minlen/thmin(50)		!common with scanfunc fitsub, fitsub6
	common/dp/discprt
	common/fitblk/data,if1,if2,ntrans,neval,nevfix,itype,stepamp,y0
	common/ycal/ydcalc	!calc values from SCANFUNC
	common/filtblk/filt1,nfilt1,dtf1,npfilt,finter,filt(640),ffilt
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
c		if(theta(i).le.1) theta(i)=thmin(i)
		if(theta(i).lt.theta(i-1)+thmin(i)) then
		   theta(i)=theta(i-1) + thmin(i)
		endif
c Define dt()
		j=i-1
		dt(j)=theta(i) - t0
c		idt(j)=ifixr(dt(j)/dtf1)	!corresp # of filter points
		if(debug) then
		   write(8,701) j,dt(j)
701		   format(' j, dt(j) = ',i5,3x,g13.6)
		endif
	   enddo
	endif
c
c Define all amplitudes, and then define stepamp()
c Calc step amplitudes in stepamp()
	if(itype.eq.1) then	!shut/shut (so ntrans=>2 and even)
	   y0=theta(ntrans+2)	!initial level before trans #1
	   base1=0.5*(theta(ntrans+2)+theta(ntrans+3))	!mean baseline (local var)
	   stepamp(1)=theta(ntrans+1)-theta(ntrans+2)	!use pre-baseline
	   stepamp(ntrans)=theta(ntrans+3)-theta(ntrans+1)	!use post-baseline
	   if(ntrans.gt.2) then
	      sign=-1.0
		do j=2,ntrans-1
		   stepamp(j)=sign*(theta(ntrans+1)-base1)
		   sign=-sign
		enddo
	   endif
	else if(itype.eq.2) then	!shut/open (so ntrans=>1 and odd)
	   y0=theta(ntrans+2)	!initial level before trans #1
	   base1=y0
	   stepamp(1)=theta(ntrans+1)-base1
	   if(ntrans.gt.1) then
	      sign=-1.0
		do j=2,ntrans
		   stepamp(j)=sign*(theta(ntrans+1)-base1)
		   sign=-sign
		enddo
	   endif
	else if(itype.eq.3) then	!open/shut (so ntrans=>1 and odd)
         y0=theta(ntrans+1)   !initial level open level (will be fixed param)
         base1=theta(ntrans+2)        !final baseline
         stepamp(1)=base1-theta(ntrans+1)
         if(ntrans.gt.1) then
            sign=1.0
            do j=2,ntrans
		   stepamp(j)=sign*(theta(ntrans+1)-base1)
		   sign=-sign
		enddo
	   endif
	else if(itype.eq.4) then	!open/open (so ntrans=>2 and even)
         y0=theta(ntrans+1)   !initial level open level (will be fixed param)
         base1=theta(ntrans+2)        !baseline (also fixed param)
         stepamp(1)=base1-theta(ntrans+1)
         if(ntrans.gt.1) then
            sign=1.0
            do j=2,ntrans
		   stepamp(j)=sign*(theta(ntrans+1)-base1)
		   sign=-sign
		enddo
	   endif
	endif
	if(debug) then
	   do j=1,ntrans
	     write(8,702) j,stepamp(j)
702	     format(' j, stepamp(j) = ',i5,3x,g13.6)
	   enddo
	endif
c
	do j=1,ntrans
	  fend(j)=0.
	  do i=1,j
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
	if(debug) then
	   write(8,704) t0,tend,if1,m,ival,time,tival(j1)
704	   format(' t0,tend,if1,m,ival,time,tival = ',/,2g13.6,3i5,2g13.6)
	endif
5	continue		!return here for i=ival+1
	if(debug) then
	   write(8,713) j1,ival
713	   format(/,' j1 = ',i5,' ival= ',i8)
	endif
	yc(j1)=0.0
c
c Add in the first filter separately
	if(tival(j1).lt.t0) then
	   if(debug) then
	      write(8,810) time,tival(j1)
810	      format(' First filter,time,tival = ',i5,2g13.6,': goto 10')
	   endif
	   goto 10			!and so can jump out of jt loop now
	else if(tival(j1).gt.tend) then	!passed end of filter #1
	   iflag=1		!filter #1 is finished -assign yc=cumulative value
	   yc(j1)=fend(1)	!after end of filter #1, and go to next filter
	   if(debug) then
	      write(8,811) 1,yc(j1),time,tival(j1)
811	      format(' jt,yc(j1),time,tival = ',i5,3g13.6,': goto 31')
	   endif
	   goto 31
	else              !time at which resp is being calc is in range
	   yc(j1)=filt1(ival)*stepamp(1)
	   if(debug) then
	      write(8,805) ival,time,tival(j1),filt1(ival),y
805	      format(
     &	' filt 1 in range: ival,time,tival,filt1(ival),y = ',/,
     &      2i5,5g13.6)
	   endif
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
	   t=tival(j1)-dt(jt-1)
	   if(t.lt.t0) then
	     if(debug) then
	      write(8,710) jt,t
710	      format(' jt,t = ',i5,g13.6,': goto 10')
	     endif
	     goto 10			!and so can jump out of jt loop now
	   else if(t.gt.tend) then	!passed end of filter #jt
	     iflag=jt		!filter #jt is finished -assign yc=cumulative value
	     yc(j1)=fend(jt)	!after end of filter #jt, and go to next filter
	     if(debug) then
	       write(8,711) jt,yc(j1),t
711	       format(' jt, yc(j1),t = ',i5,2g13.6,': goto 3')
	     endif
	     goto 3
	   else              !time at which resp is being calc is in range
c		Interp in filter to find value at time t
		iv1=1 + ifix((t-t0)/dtf1)
		tiv1=t0 + float(iv1-1)*dtf1		!time for filt(iv1)
	      if(iv1.ge.nfilt1) then
c		  y=filt1(nfilt1)*stepamp(jt)		!filt(nfilt)=1.0 always
		  y=stepamp(jt)
		  if(debug) then
		     write(8,712) jt,iv1,tiv1,y
712		     format(' iv1>nfilt1: jt,iv1,tiv1,y = ',/,2i5,2g13.6)
		  endif
	      else
	         b=(filt1(iv1+1)-filt1(iv1))/dtf1
		   y=(filt1(iv1) + b*(t-tiv1))*stepamp(jt)
		   if(debug) then
		     write(8,705)
     &		jt,iv1,time,tival(j1),t,filt1(iv1),filt1(iv1+1),
     &		filt1(iv1)*stepamp(jt),filt1(iv1+1)*stepamp(jt),y
705		     format(
     &		' jt,iv1,time,tival,t = ', 2i5,3g13.6,/,
     &		' filt1(iv1),filt1(iv1+1) = ',2g13.6,/,
     &		' filt1(iv1)*step,filt1(iv1+1)*step,y = ',3g13.6)
		   endif
	      endif
	   endif
c
 	   yc(j1)=yc(j1) + y		!add JTth filter
	   if(debug) then
	      write(8,706) jt,j1,yc(j1)
706	      format(' for filt ',i3, ': j1,yc(j1) = ',i5,g13.6)
	   endif
3	  continue		!end of jt=2,ntrans loop
	endif		!end of ntrans>1 bit
c
10	continue
c yc() now finished for current time point, except for addition of initial level
	yc(j1)=yc(j1) + y0
	if(debug) then
	   write(8,707) j1,yc(j1)
707	   format(' final j1,yc(j1) = ',i5,g13.6)
	endif
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
	if(debug) then
	   write(8,708) yc(1),yc(2),time,tival(1),tival(2),ycalc
708	   format(' yc(1),yc(2),time,tival(1),tival(2),ycalc = ',/,6g13.6)
	endif
c
c Calculate the SSD
2	continue
	ssd=ssd + (ycalc-data(m))**2
	ydcalc(m)=ycalc		!calc value corresponding to data(m)
	if(debug) then
	   write(8,709) m,data(m),ycalc,ssd
709	   format(' m,data(m),ycalc,ssd = ',i5,3g13.6,//)
	endif
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
	SCNFUNC1=ssd
c For debug put abort here
c NB key F1 has ktype=0, scan code=59 (see \fortran\tkey.for)
c	if(KBHIT()) then
c		ch=GETCH(ktype)
c		if(ktype.eq.0.and.ichar(ch).eq.59) abort=.true.	!abort on SIMPLEX
c	endif
	RETURN
	end

