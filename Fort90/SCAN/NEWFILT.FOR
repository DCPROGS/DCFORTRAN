	subroutine NEWFILT(filt1,nfilt1,dtf1,ntrans,ndisp,finter,
     & filt,nfilt,dtf)
	real*4 filt1(1024)	!from INSCAN/NORMFILT
	real*4 filt(640)
	character*1 ans,UC
	logical caplock
c To interpolate step response function at appropriate intervals for the
c number of points on display in SCAN
c Orig version now call NEWFILT0
c This version calculates a suitable number of points for FILT on basis
c that want 640 points for whole display (length=finter*ndisp) and the
c the time taken for 1 filter is nfilt1*dtf1 so time taken for all
c ntrans filters is ntrans1*nfilt1*dtf.  Thus the filters (if they don't
c overlap anyway) occupy a fraction
c p=ntrans*nfilt1*dtf1/(finter*ndisp)
c of the display, and therefore want p*640 points for the filters
c to be displayed, ie p*640/ntrans points per filter
c Input:
c filt1,nfilt1,dtf1
c finter=microsec between data points
c ndisp=number of data points on display
c ntrans=number of transitions on display
c Output:
c filt,nfilt,dtf
c
c   From INSCAN have FILT1() which contains values of f(t) at equal
c time increments =DTF1 (microseconds),with filt1(1)=0 at t=0
c and filt1(nfilt1)=1 at t=(nfilt1-1)*dtf1.
c Range of x must be same for both arrays
c so xmax=x0+(n-1)*dx=x0+(n1-1)*dx1, so must have
C dx/dx1=(n1-1)/(n-1) i.e. n1=1+ (n-1)*dx/dx1,
c or dx1=dx*(n-1)/(n1-1)
	nfilt=ifix(640.*float(nfilt1)*dtf1/(finter*float(ndisp)))
	if(nfilt.lt.16) nfilt=16
	if(nfilt.gt.640) nfilt=640
	dtf=dtf1*float(nfilt1-1)/float(nfilt-1)	!new interval
c
c Interpolate new filter at intervals of dtf.  In this case filt1 is
c already interp at close intervals, so linear interp will be OK
c x0=0.0 in this case
	x0=0.0
	call LINTERP(filt1,x0,dtf1,nfilt1,filt,dtf,nfilt)
c
	RETURN
	end
