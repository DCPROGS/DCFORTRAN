	function RTSAFE(funcd,x1,x2,xacc,nerr,x0,xstep)

c Safe version of Newton-Raphson (combined with bisection) from Numerical Recipes
c error returns added
c In origunal, the initial guess for N-R is halfway between the limits (x1,x2)
c but limits used only if N-R if it goes out of bounds.  Therefore add x0
c (initial guess) and dx (=initial step size) as extra parameters
      INTEGER MAXIT
      REAL rtsafe,x1,x2,xacc
      EXTERNAL funcd
      PARAMETER (MAXIT=100)
      INTEGER j
      REAL df,dx,dxold,f,fh,fl,temp,xh,xl
c
	nerr=0
      call funcd(x1,fl,df)
      call funcd(x2,fh,df)
      if((fl.gt.0..and.fh.gt.0.).or.(fl.lt.0..and.fh.lt.0.)) then
c	   pause 'root must be bracketed in rtsafe'
	   nerr=1
	   RETURN
	endif
      if(fl.eq.0.)then
        rtsafe=x1
        return
      else if(fh.eq.0.)then
        rtsafe=x2
        return
      else if(fl.lt.0.)then
        xl=x1
        xh=x2
      else
        xh=x1
        xl=x2
      endif
c DC modif for initial guess
	rtsafe=x0
	dxold=xstep
c      rtsafe=.5*(x1+x2)
c      dxold=abs(x2-x1)
      dx=dxold
      call funcd(rtsafe,f,df)
      do 11 j=1,MAXIT
        if(((rtsafe-xh)*df-f)*((rtsafe-xl)*df-f).ge.0..or. abs(2.*
     *f).gt.abs(dxold*df) ) then
          dxold=dx
          dx=0.5*(xh-xl)
          rtsafe=xl+dx
          if(xl.eq.rtsafe)return
        else
          dxold=dx
          dx=f/df
          temp=rtsafe
          rtsafe=rtsafe-dx
          if(temp.eq.rtsafe)return
        endif
        if(abs(dx).lt.xacc) return
        call funcd(rtsafe,f,df)
        if(f.lt.0.) then
          xl=rtsafe
        else
          xh=rtsafe
        endif
11    continue
c      pause 'rtsafe exceeding maximum iterations'
	nerr=2
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software #1,15(15..
