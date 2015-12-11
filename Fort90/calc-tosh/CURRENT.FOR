	real*4 function CURRENT(t,amps,tau,env,gamma,cinf,kA,k,km)
c Returns current at time t, (eg after call to PTCALCS) given
c
c INPUT: t (in same units as tau)
c	   amps(m,j)=amplitude for component m, state j
c	   tau(m)=time constants for component m
c	   cinf=equilibrium current
c	   gamma(j), j=1,kA = conductances of open states
c	   env = Nchan*(V-Vrev)
	real*4 amps(km,km),tau(km),gamma(10)
c
	s=0.0
	do n=1,kA
	   do m=1,k-1
		amp=env*gamma(n)*amps(m,n) !amp for component m, state n
		s=s + amp*exp(-t/tau(m))
	   enddo
	enddo
	CURRENT=s + cinf
c
	RETURN
	end


