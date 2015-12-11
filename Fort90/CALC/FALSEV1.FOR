	subroutine falsev1(tres,fc,rms,avamp,frate)
c Version for EKDIST/new SCAN (avamp, rms already in pA)
c Dec 87: modif so fc rather than jfilt in call
c To calc false event rate (per sec) in EKDIST (from RESINT)
c First calc 'threshold' as amp attained by pulse of length=tres
c (in ms)
	call UNDER0('true')	!so underflows set=0 (Lahey manual p12.8)
	w=tres*1.e-3			!sec
	u=erfs(2668.*fc*w)
	amp=avamp			!full amp (pA)
	phi=u*amp			!'threshold' (pA)
	var=(rms)**2		!noise variance (pA)**2
c Calc rate from C & Sigworth eq. 9, with k=1
	frate=1000.*fc*exp(-(phi*phi)/(2.*var))
	return
	end


