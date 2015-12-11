	subroutine PATPLOT(freqs,noutlo,nouthi,
     & revamp,nbin,xval,yval,xmin,xmax,ymin,ymax,xtic,ytic,
     & i1min,i1max,xwbase,idtype,ndimd)
c
c To process freqs() (defined in cdist1) to make histogram
c
c Amps put into 0.01 pA bins in cdist1:  construct final bins
c here.  Use the freqs() array to hold these FREQUENCIES so
c freqs(-2)=frequency of values from -0.03 to -0.02 pA
c freqs(-1)=frequency of values from -0.02 to -0.01 pA
c freqs(0)=frequency of values from -0.01 to 0.0 pA
c freqs(1)=frequency of values from 0 to 0.01 pA
c freqs(2)=frequency of values from 0.01 to 0.02 pA
c so 100*amp corresponds with upper bound of bin Yfreq(i)
c  Mid-values used to fit in Gaulik are current=0.5*(float(i-1)+float(i))/100.
c  for bin #i, e.g. for freqs(2) mid-value for current=0.015 pA
c=	real*4 Yfreq(-2047:2048)		!OK for plus/minus 20.4 pA
	real*4 freqs(-5000:5000)	!in common/pablk/ -use also for Patlak
	logical revamp,binset
	logical discprt
	character UC*1,ans*1,cnum1*11
c for VHIST
	real XVAL(0:511,ndimd),YVAL(0:511,ndimd)
c for histo data
c	integer ndat(1),icurvd(1),ijoin(1)
c for calc curves
c	integer ncal(1),icurvc(1),iline(1)
c
	common/dp/discprt
c
	binset=.false.
c
c
c Find largest and smallest non-zero entries in freqs()
	do i=5000,-5000,-1
	   if(freqs(i).ne.0.0) then
		i1max=i
		goto 5
	   endif
	enddo
5	do i=-5000,5000
	   if(freqs(i).ne.0.0) then
		i1min=i
		goto 6
	   endif
	enddo
6	continue
c Min current is that for lower bound of bin freqs(i1min)
c Max current is that for upper bound of bin freqs(i1max)
	cmin=float(i1min-1)/100.
	cmax=float(i1max)/100.
c
13	continue	!return here with binset=true if binwidth etc reset
	print 7,cmin,cmax
7	format(
     & ' Mean low-variance currents range from ',f9.3,' to ',f9.3,' pA')
c Define defaults
	dx=0.05		!0.05 pA bin width
	xwbase=dx		!if only one bin width
	npool=5 		!pool 5 bins
	xmax=float(ifix(cmax)+1)		!nearest pA above
	xmin=float(ifix(cmin)-1)		!nearest pA below
	nbin=1 + ifix((xmax-xmin)/dx)       !round up
	if(nbin.gt.510) then
	   dx=0.1
	   nbin=1 + ifix((xmax-xmin)/dx)       !round up
	endif
c	xtic=2.*xwbase	!number every 10th bin
	xtic=1.0		!major tics=1pA
	print 20,nbin,dx,xmin,xmax
20	format(1x,i3,
     & ' bins, width ',f9.3,' pA, from ',f9.3,' to ',f9.3,
     & ' pA: O.K. [Y] ')
	ans='Y'
	call INPUTa(ans)
16	continue
	if(UC(ans).eq.'N') then
	   xmin=0.		!default (others as above)
701	   print 18,xmin
18	   format(' Histogram to start at (pA) [',f5.2,'] = ')
	   call INPUTr(xmin)
	   print 19,dx,xmax
19	   format(' Bin width, Last x value (pA) [',f5.2,',',f5.2,'] = ')
	   call INPUT2r(dx,xmax)
	   dx1=dx
c binwidth must be multiple of 0.01 pA
	   npool=ifixr(100.*dx)	!eg if dx=0.047, npool=5=number of orig bins to pool
	   dx=0.01*float(npool)	! and dx reset to 0.05
	   nbin=1 + ifix((xmax-xmin)/dx)       !round up
	   xwbase=dx		!if only one bin width
	   if(nbin.gt.510) then
		call BELL(1)
		print 70
70		format(' TOO MANY BINS')
		goto 701
	   endif
	   if(abs(dx1-dx).gt.0.00001) then
	      call DCFORMAT(dx,9,5,cnum1)
	      call DCASK(
     &	  'Bin width reset to '//charnb(cnum1)//' pA','y',ans)
	      if(ans.eq.'N') goto 16
	      binset=.true.
	   endif
c	   goto 13		!recalculate for new bins
	endif
c Nbin and dx now set, so pool freqs() values to get values for histogram
c To get right index for freqs(), note that
c freqs(-1)=frequency of values from -0.03 to -0.02 pA
c freqs(-1)=frequency of values from -0.02 to -0.01 pA
c freqs(0)=frequency of values from -0.01 to 0.0 pA
c freqs(1)=frequency of values from 0 to 0.01 pA
c freqs(2)=frequency of values from 0.01 to 0.02 pA
c freqs(3)=frequency of values from 0.02 to 0.03 pA
c so current=xmin =lower bound of first bin, and frequencies for
c first bin (current = xnin to xmin+dx) are in freqs(i) to freqs(i+npool-1)
c where i=(100.*xmin)+1
c e.g. if xmin=0.02, i=3 so start at freqs(3)
c e.g. if xmin=-0.02, i=-1 so start at freqs(-1)
	xval(1,1)=xmin
	ymax=0.0
	if=1 + ifixr(100.*xmin)
	ifsav=if
	do i=1,nbin
	   xval(i+1,1)=xval(1,1) + float(i)*dx
	   tot=0
	   do i1=1,npool
	      tot=tot + freqs(if)
		if=if+1
	   enddo
	   yval(i,1)=tot
	   if(yval(i,1).gt.ymax) ymax=yval(i,1)
	enddo
	if2sav=if-1	!last bin used
c
c	HI bin is from Xval(nbin+1,j) to Xval(nbin+2,j); freq in Yval(nbin+1,j)
c Ther are noutlo/hi values from cdist1 -add in any more that result from
c having xmin> cmin or xmax>cmax
	xval(0,1)=xval(1,1)-dx		!lo bin
	xval(nbin+2,1)=xval(nbin+1,1)+dx	!hi bin
	ylo=float(noutlo)			!lo bin
	if(xmin.gt.cmin) then		!add in all bins below xmin
	   do i=-5000,ifsav-1
		ylo=ylo+freqs(i)
	   enddo
	endif
	yval(0,1)=ylo			!lo bin
c and similar for hi bin
	yhi=float(nouthi)		!hi bin
	if(xmax.lt.cmax) then		!add in all bins above xmax
	   do i=if2sav+1,5000
		yhi=yhi+freqs(i)
	   enddo
	endif
	yval(nbin+1,1)=yhi		!hi bin
c
	ymin=0.0
	ymax=ymax*1.2	!max for display
	ymax=float(1+ifix(ymax))	!ROUND UP
	ytic=10.0**ifix(alog10((ymax-ymin)*0.5))
2101	if((ymax-ymin)/ytic.lt.5.) then
	   ytic=ytic/2.
	   goto 2101	!ensure at least 5 tics so get numerical label
	endif
2102	if((ymax-ymin)/ytic.gt.11.) then
	   ytic=ytic*2.
	   goto 2102	!ensure at least 5 tics so get numerical label
	endif
	if(binset) then	!skip questions
c	   binset=.false. !reset
c	   goto 14
	endif
c
14	continue
	if(discprt) then
	   if(discprt) write(8,15) nbin,dx,xmin,xmax
15	   format(
     & ' Mean-low variance histogram: ',i4,' bins, width ',f9.5,' pA',/,
     & ' plotted between ',f7.3,' pA and ',f7.3,' pA',/)
	endif
c
	RETURN
	end

