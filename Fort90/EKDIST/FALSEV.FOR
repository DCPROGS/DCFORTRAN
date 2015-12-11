	subroutine falsev(tres,fc,rms,iavamp,acal,frate)
c Dec 87: modif so fc rather than jfilt in call
c To calc false event rate (per sec) in EKDIST (from RESINT)
c First calc 'threshold' as amp attained by pulse of length=tres
c (in ms)
	call UNDER0('true')	!so underflows set=0 (Lahey manual p12.8)
c	fc=1000.*0.5*float(jfilt)	!Hz
	w=tres*1.e-3			!sec
c	u=erfs(2.668*fc*w)
	u=erfs(2668.*fc*w)
	amp=abs(acal*float(iavamp))	!full amp
	phi=u*amp			!'threshold' (pA)
	var=(rms*acal)**2		!noise variance (pA)**2
c Calc rate from C & Sigworth eq. 9, with k=1
c	frate=fc*exp(-(phi*phi)/(2.*var))
	frate=1000.*fc*exp(-(phi*phi)/(2.*var))
c	print 1,fc,w,u,phi,amp,sqrt(var),frate
c1	format(' fc,w,u,phi,amp,sqrt(var),frate= ',/,7g13.6)
	return
	end


