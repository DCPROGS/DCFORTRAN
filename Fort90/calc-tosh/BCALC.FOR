	function BCALC(c,ncomp,theta,kmax)
c To calculate response at conc c for given theta in BETZFIT
c
c Modif 05/10/01 05:55pm by addition of common /fiteq/ieqn
c ieqn=1 for fit of eqn used by Betz for random subunit assoc
c ieqn=2 for fit of sum of Hill equations
	logical conamp
	common/fiteq/ieqn,conamp
	real theta(20)
c
	if(ieqn.eq.1) then
	   i1=1
	   ycalc=0.0
	   nc1=ncomp-1
	   Ym=theta(1)
	   p=theta(kmax)
	   do m=1,ncomp
	 	i1=i1+1
		aK=theta(i1)
		i1=i1+1
		hill=theta(i1)
		ir=m-1
	 	c1=(c/aK)**hill
		ycalc=ycalc+binom(ir,nc1,p)*c1/(1.0+c1)
	   enddo
	   ycalc=ycalc*Ym
	else if(ieqn.eq.2) then
	   sumamp=0.0
	   if(conamp.and.ncomp.gt.1) then
	      do m=1,ncomp-1
		   amp=theta(3*m-2)
		   sumamp=sumamp+amp
		enddo
		if(sumamp.lt.0.9999) then
		   amplast=1.0 - sumamp
		else
		   amplast=0.0	!if this set, scale rest of amps to add to 1
		   fac=1.0/sumamp
		   do m=1,ncomp-1
			theta(3*m-2)=theta(3*m-2)*fac		!scale down so sum=1
		   enddo
		endif
	   endif
	   i1=0
	   ycalc=0.0
	   do m=1,ncomp
		if(conamp.and.m.eq.ncomp.and.ncomp.gt.1) then
		   amp=amplast
		else
	 	   i1=i1+1
		   amp=theta(i1)
		endif
		i1=i1+1
		aK=theta(i1)
		i1=i1+1
		hill=theta(i1)
	 	c1=(c/aK)**hill
		ycalc=ycalc + amp*c1/(1.0+c1)
	   enddo
	endif
	BCALC=ycalc
	RETURN
	end


