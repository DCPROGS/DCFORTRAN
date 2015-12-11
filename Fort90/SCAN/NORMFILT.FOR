	subroutine NORMFILT(nfilt,dtf,filt,dtf1,nfilt1,filt1,
     &  tf01,tf1,tf50,tf99,tf999,nsetup,filtf)
c	subroutine NORMFILT(nfilt,dtf,filt,dtf1,nfilt1,filt1,tf0,tf50,tf1)
	real*4 filt(100),time(100),y2(100),filt1(1024)
	logical debug,caplock
c To 'normalise' a step response function (that goes from 0 to 1).
c The output filter is truncated at each end so filt1(2) corresponds (approx)
c to tf01, and filt1(nfilt1-1) corresponds (approx) to tf999
c
c Modified 07/27/95 02:40pm so that, for Gaussian filter (nsetup=-1000)
c tf1, tf50 etc are given there theoretical values (see GAUFILT.MCD), so
c search method used only for empirically measured step response
c functions.  Actually tf50 is the only value used in SCAN.
c Also altered so that filt1(1)=0.001 (rather than filt1(2) before, so
c tf01=0 always!  For Gaussian filter no need to use interpolation at all;
c just calculate filt1() at required time points!
c
c
c Finds the following times (taking first tabulated point as t=0)
c (1) tf01=time when response=0.001; ie the 'start' of the response
c (2) tf1=time when response=0.01
c (3) tf50 = time to reach 0.5
c (4) tf99=time when response=0.99
c (5) tf999 = time when response is within 0.1% of 1; i.e. abs(1-response)=0.001
c	starting the search from the end of the interolated response.
c	(when response overshoots by more than 0.1% this will be for time
c	for response=1.001, otherwise it is time for response=0.999)
c	 This is the 'end' of the response
c
c The output filter is truncated at each end so filt1(2) corresponds (approx)
c to tf01, and filt1(nfilt1-1) corresponds (approx) to tf999
c
c Input=nfilt,dtf,filt
c
c Section for a calculated Gaussian filter.
	if(nsetup.eq.-1000) then
	   dtf1=1.0 		!1 microsec regardless of fc
	   tmin=-409.4718/filtf	!filtf=fc in kHz, so tmin gives filt1=0.001
	   tmax=-tmin		!so tmax gives filt1()=0.999
90	   nfilt1=1 + ifix((tmax-tmin)/dtf1)
	   if(nfilt1.gt.1024) then
		dtf1=dtf1*1.1
		goto 90
	   endif
	   do i=1,nfilt1
		t=(tmin + float(i)*dtf1)
		filt1(i)=0.5*(1.0 + erf(5.336446e-3*filtf*t))
	   enddo
c Define tf50 etc (see GAUFILT.MCD)
	   tf01=0.0
	   tf1=101.2187/filtf
	   tf50=409.4718/filtf
	   tf99=717.7249/filtf
	   tf999=818.9436/filtf
	   RETURN
	endif
c
c Rest done only for empirical step response functions
c
	do i=1,nfilt
	   time(i)=float(i-1)*dtf	!in microseconds
	enddo
	tmax=time(nfilt)
c
	call SPLINE(time,filt,nfilt,0.0,0.0,Y2)
c Now interpolate at intervals of dtf1 microsecond; xin=0,...,tmax
	dtf1=1.0 		!1 microsec
9	nfilt1=1+ifix(tmax/dtf1)   !number of values for interpolated filter
	if(nfilt1.gt.1024) then
	   dtf1=dtf1*1.1
	   goto 9
	endif
	klo=0		!full bisection first time only
	debug=caplock()
	do i=1,nfilt1
	  xin=float(i-1)*dtf1
	  call SPLINT(time,filt,y2,nfilt,klo,khi,xin,yout)
	  filt1(i)=yout
	  if(debug) then
		print 10,i,xin,yout
10	  	format(i8,2g13.6)
		if(mod(i,20).eq.0) pause
	  endif
	enddo
c Now find 0.1% time (tf01) and normalise filter so this is the 2nd point
c (1st point=0 always)
	ycrit=0.001		!time for 0.1% of final value
	do i=1,nfilt1-1
	  if(filt1(i).eq.ycrit) then
		tf01=float(i-1)*dtf1
		i1=i
		goto 1
	  endif
	  if((filt1(i).lt.ycrit).and.(filt1(i+1).gt.ycrit)) then
		i1=i
		goto 1
	  endif
	enddo
c
	i1=1		!keep start point same if 0.1% time not found
1	continue
	do i=2,nfilt1-i1+1
	  filt1(i)=filt1(i+i1-1)	!shift left, retaining filt1(1)=0.
	enddo
c fill the rest of filt1() with 1.0
	do i=nfilt1-i1+2,nfilt1
	   filt1(i)=1.0
	enddo
c define new nfilt1
	nfilt1=nfilt1-i1+1
c
c Now search for 0.1%, 1%, 50% etc times in this truncated filter
c
	ycrit=0.001		!time for 0.1% of final value
	do i=1,nfilt1-1
	  if(filt1(i).eq.ycrit) then
		tf01=float(i-1)*dtf1
	  endif
	  if((filt1(i).lt.ycrit).and.(filt1(i+1).gt.ycrit)) then
		t1=float(i-1)*dtf1
		t2=t1+dtf1
		b=(filt1(i+1)-filt1(i))/(t2-t1)
		tf01=t1 + (ycrit-filt1(i))/b
	  endif
c
	  if(filt1(i).eq.0.01) then
		tf1=float(i-1)*dtf1
	  endif
	  if((filt1(i).lt.0.01).and.(filt1(i+1).gt.0.01)) then
		t1=float(i-1)*dtf1
		t2=t1+dtf1
		b=(filt1(i+1)-filt1(i))/(t2-t1)
		tf1=t1 + (0.01-filt1(i))/b
	  endif
c
	  if(filt1(i).eq.0.5) then
		tf50=float(i-1)*dtf1
		goto 11
	  endif
	  if((filt1(i).lt.0.5).and.(filt1(i+1).gt.0.5)) then
		t1=float(i-1)*dtf1
		t2=t1+dtf1
		b=(filt1(i+1)-filt1(i))/(t2-t1)
		tf50=t1 + (0.5-filt1(i))/b
		goto 11
	  endif
	enddo
c
11	continue
c Now start from end of interpolated filter to find tf99 and tf999
12	continue
c
c First for tf99
	ycrit=0.01
	do i=nfilt1,2,-1
	  ytest1=abs(1.0-filt1(i))
	  if(ytest1.eq.ycrit) then
		tf99=float(i-1)*dtf1
		goto 13
	  endif
	  ytest2=abs(1.0-filt1(i-1))
	  if(ytest2.lt.ytest1) then	!make ytest2 the larger
		y=ytest1
		ytest1=ytest2
		ytest2=y
	  endif
	  if(filt1(i).gt.filt1(i-1)) then
		y=1.0-ycrit       !no overshoot
	  else
		y=1.0+ycrit       !overshoot
	  endif
	  if((ytest1.lt.ycrit).and.(ytest2.gt.ycrit)) then
	      t2=float(i-1)*dtf1  !time for filt1(i)
	      t1=t2-dtf1          !time for filt1(i-1)
		b=(filt1(i)-filt1(i-1))/(t2-t1)
		tf99=t1 + (y-filt1(i-1))/b
		goto 13
	  endif
	enddo
13	continue
c Repeat for tf999
	ycrit=0.001
	do i=nfilt1,2,-1
	  ytest1=abs(1.0-filt1(i))
	  if(ytest1.eq.ycrit) then
		tf999=float(i-1)*dtf1
		i2=i
		goto 14
	  endif
	  ytest2=abs(1.0-filt1(i-1))
	  if(ytest2.lt.ytest1) then	!make ytest2 the larger
		y=ytest1
		ytest1=ytest2
		ytest2=y
	  endif
	  if(filt1(i).gt.filt1(i-1)) then
		y=1.0-ycrit       !no overshoot
	  else
		y=1.0+ycrit       !overshoot
	  endif
	  if((ytest1.lt.ycrit).and.(ytest2.gt.ycrit)) then
	      t2=float(i-1)*dtf1  !time for filt1(i)
	      t1=t2-dtf1          !time for filt1(i-1)
		b=(filt1(i)-filt1(i-1))/(t2-t1)
		tf999=t1 + (y-filt1(i-1))/b
		i2=i
		goto 14
	  endif
	enddo
	i2=nfilt1-1	!if tf999 not found
14	continue
c
c Finally remove excess points so penultimate point is close to tf999 and
c last point=1
	if(i2.lt.nfilt1-1) then
	   do i=i2+1,nfilt1
	    filt1(i)=1.0
	   enddo
	   nfilt1=i2+1
	endif
c
	RETURN
	end


