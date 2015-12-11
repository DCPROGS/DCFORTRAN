	subroutine AMOCALL(npar,nvert,simp,theta,stepfac,functol,funk,
     & fmin,niter)
c DC subroutine to simplify call of AMOEBA.FOR from Press et al. (1992)
c (1) Sets up starting simplex in simp(21,20)
c (2) Sets corresponding function values in fval(21)
c
c SIMP should be declared in calling program, e.g. as simp(21,20).
c    	(simp is defined here, but because of problems in passing values
c	in 2-dimensional arrays with variable dimensions, it is simpler
c	to declare simp in the calling program)
c
c INPUT:
c    npar = number of parameters
c    nvert = npar+1
c    theta(npar) = initial guesses for parameters
c    stepfac = value to control initial step size, e.g. stepfac=0.1 starts
c		with step size=0.1*initial guess.
c	functol = tolerance for convergence (should be set to machine precision,
c		or a bit larger -see Press et al.)
c	funk = name of subroutine that calculates the value to be minimized
c
c OUTPUT:
c	theta = final values of parameters (in this version, set to the
c		parameters corresponding to the best vertex of final simplex).
c     fmin = corresponding minimum value for funk(theta)
c	niter = number of function evaluations done
c
	real simp(nvert,npar),fval(21),theta(npar),step(20)
	EXTERNAL funk
c
c	nvert=npar+1		! # of vertices in simplex
	do j=1,npar
	   step(j)=stepfac*theta(j)
	enddo
	do j=1,npar
	  simp(1,j)=theta(j)		!!start values=vertex #1
	enddo
	fval(1)=funk(theta)		!function value for these
	fac=(sqrt(float(nvert))-1.)/(float(npar)*sqrt(2.))
	do i=2,nvert
	   do j=1,npar
		simp(i,j)=simp (1,j) + step(j)*fac
	   enddo
	   simp(i,i-1)=simp(1,i-1) + step(i-1)*(fac+1./sqrt(2.))
	   do j=1,npar
		theta(j)=simp(i,j)	!copy paramters into theta (for funk)
	   enddo
	   fval(i)=funk(theta)		!function value for these
	enddo
c
      call AMOEBA(simp,fval,nvert,npar,npar,functol,funk,niter)
c
c	Return the best vertex
	fmin=fval(1)
	do i=2,nvert
	   if(fval(i).lt.fmin) then
		fmin=fval(i)
		do j=1,npar
		   theta(j)=simp(i,j)
		enddo
	   endif
	enddo
c
	RETURN
	end


