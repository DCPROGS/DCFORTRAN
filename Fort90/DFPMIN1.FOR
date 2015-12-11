      SUBROUTINE dfpmin1(theta,nt,gtol,iter,fret,func,dfunc,
     & stpmx,jfix,ndisp)
c     SUBROUTINE dfpmin(p,n,gtol,iter,fret,func,dfunc)
c DC version of Davidon-Fletcher-Powell variable metric minimisation
c with explicit derivatives (routines dfpmin and lnsrch from numrec2
c (see page 421 of Press et al)
c Abort with either F1 or F2
c	Make special calling routine for original dfpmin?  What about fixed
c parameters?  These must be eliminated from the parameter array, p()
c before calling.  Need to think how func() and dfunc() (1st derivs) cope
c with this since they depend on p(), and order of parameters in p()
c depends on which ones have been eliminated!  May need to modify
c the original dfpmin to cope with this, so func, dfunc are called
c with all parameters, but skip fixed params somehow.  Also lnsrch calls
c func() once.
c 	Replace parameter array p() in the call by theta()=array of ALL
c  parameters and replace n in call with nt=total number of parameters
c and use n, p() internally to hold only the variable (non-fixed) parameters
c
c Ndisp controls print of parameters =0 for none. If ndisp negative the
c prints to disc file also (if discprt=true), if positive prints to
c screen only
C
c Error returns                                         Actiom
c nerr=0	No errors
c nerr=1	Reached itmax					Premature end
c nerr=2	Disc negative in LNSRCH				disc set to 0.0
c nerr=3	Overflow of b*b in LNSRCH			Run aborted
c
      INTEGER iter,n,NMAX,ITMAX,jfix(nt)
      REAL fret,gtol,func,EPS,STPMX,TOLX
	real theta(nt)
c      PARAMETER (NMAX=50,ITMAX=200,STPMX=100.,EPS=3.e-8,TOLX=4.*EPS)
      PARAMETER (NMAX=50,ITMAX=200,EPS=3.e-8,TOLX=4.*EPS)
      EXTERNAL dfunc,func
CU    USES dfunc,func,lnsrch
      INTEGER i,its,j
      LOGICAL check
      REAL den,fac,fad,fae,fp,stpmax,sum,sumdg,sumxi,temp,test,dg(NMAX),
     & g(NMAX),hdg(NMAX),hessin(NMAX,NMAX),pnew(NMAX),xi(NMAX)
c Declare internal arrays
	real p(nmax)
	real g1(nmax),xi1(nmax),pnew1(nmax)	!to hold all params (including fixed values)
	logical discprt,dprt,abort
	integer*2 ktype
	logical KBHIT		!Utility lib
	character ch,getch
	common/abt/abort
	logical silent
	common/err/nerr,silent	!for sfunc,dsfunc
	common/dp/discprt
c
	n=0
	do j=1,nt
	   if(jfix(j).eq.0) n=n+1        !number of free parameters
	enddo
c
	dprt=discprt.and.(ndisp.lt.0)	!iterations printed to disc
	ndisp=iabs(ndisp)
	silent=ndisp.eq.0
	abort=.false.
	itsav=0
	nerr=0
c
	nfix=nt-n
	if(nfix.eq.0) goto 100		!original code for dfpmin
c
c=      fp=func(p)
c=      call dfunc(p,g)
      fp=func(theta)
      call dfunc(theta,g1)
	call sqz1(theta,p,jfix,nt)	!squeeze non-fixed parameters into p
	call sqz1(g1,g,jfix,nt)		!squeeze non-fixed gradients into g
      sum=0.
      do i=1,n
        do j=1,n
          hessin(i,j)=0.
	  enddo
        hessin(i,i)=1.
        xi(i)=-g(i)
        sum=sum+p(i)**2
	enddo
      stpmax=STPMX*max(sqrt(sum),float(n))
c
c Iteration starts here
      do its=1,ITMAX
        iter=its
	  if(KBHIT()) then
		ch=GETCH(ktype)
		ival=ichar(ch)
		if(ktype.eq.0.and.(ival.eq.59.or.ival.eq.60)) then
		   abort=.true.
		   RETURN
		endif
        endif
c       call lnsrch(n,p,fp,g,xi,pnew,fret,stpmax,check,func)
c      SUBROUTINE lnsrch(n,xold,fold,g,p,x,f,stpmax,check,func)
c -need care for call of lnsrch! p,g,xi,pnew are arrays!
c so not only p-> theta in call, but need all-param versions of g()
c  (=g1 above), xi and pnew also
	  call unsqz1(theta,p,jfix,nt)	!!unsqueeze p into theta for dfunc
	  call unsqz1(g1,g,jfix,nt)	!ditto
	  call unsqz1(xi1,xi,jfix,nt)
c///	  call unsqz1(pnew1,pnew,jfix,nt)
c NB pnew is output, but must put values for fixed parameters into it
c before call!!
	  do i=1,nt
	     if(jfix(i).eq.1) pnew1(i)=theta(i)
	  enddo
        call LNSRCH1(nt,n,theta,fp,g1,xi1,pnew1,fret,stpmax,
     &	check,func,jfix)
c=      fp=fret
c===addition -may not be necessary?
        if(check) then
		do i=1,nt
		   pnew1(i)=theta(i)	!restore original (and don't change fp)
		enddo
	  else
c===
           fp=fret
	  endif
c//	  call sqz1(theta,p,jfix,nt)	!squeeze non-fixed parameters into p
c//	  call sqz1(g1,g,jfix,nt)	!squeeze non-fixed gradients into g
	  call sqz1(xi1,xi,jfix,nt)
	  call sqz1(pnew1,pnew,jfix,nt)
        do i=1,n
          xi(i)=pnew(i)-p(i)
          p(i)=pnew(i)
	  enddo
	  if(.not.silent) then
	   if((its-itsav).ge.ndisp) then
	    call unsqz1(theta,p,jfix,nt)	!unsqueeze p into theta for display
	    call DSPOUT1(nt,its,itsav,fp,theta,dprt)	!type it
	   endif
	  endif
        test=0.
        do i=1,n
          temp=abs(xi(i))/max(abs(p(i)),1.)
          if(temp.gt.test)test=temp
	  enddo
c=      if(test.lt.TOLX)return
        if(test.lt.TOLX) goto 99
        do i=1,n
          dg(i)=g(i)
	  enddo
c=      call dfunc(p,g)
	  call unsqz1(theta,p,jfix,nt)	!unsqueeze p into theta for dfunc
	  call unsqz1(g1,g,jfix,nt)	!unsqueeze g into g1 for dfunc
        call dfunc(theta,g1)
c//	  call sqz1(theta,p,jfix,nt)	!squeeze non-fixed parameters into p
	  call sqz1(g1,g,jfix,nt)	!squeeze non-fixed gradients into g
	  if(.not.silent) then
	   if((its-itsav).ge.ndisp) then
	    call unsqz1(theta,p,jfix,nt)	!unsqueeze p into theta for display
	    call DSPOUT1(nt,its,itsav,fp,theta,dprt)	!type it
	   endif
	  endif
        test=0.
        den=max(fret,1.)
        do i=1,n
          temp=abs(g(i))*max(abs(p(i)),1.)/den
          if(temp.gt.test)test=temp
	  enddo
c=      if(test.lt.gtol)return
        if(test.lt.gtol) goto 99
        do i=1,n
          dg(i)=g(i)-dg(i)
	  enddo
        do i=1,n
          hdg(i)=0.
          do j=1,n
            hdg(i)=hdg(i)+hessin(i,j)*dg(j)
	    enddo
	  enddo
        fac=0.
        fae=0.
        sumdg=0.
        sumxi=0.
        do i=1,n
          fac=fac+dg(i)*xi(i)
          fae=fae+dg(i)*hdg(i)
          sumdg=sumdg+dg(i)**2
          sumxi=sumxi+xi(i)**2
	  enddo
        if(fac**2.gt.EPS*sumdg*sumxi)then
          fac=1./fac
          fad=1./fae
          do i=1,n
            dg(i)=fac*xi(i)-fad*hdg(i)
	    enddo
          do i=1,n
            do j=1,n
              hessin(i,j)=hessin(i,j)+fac*xi(i)*xi(j)-fad*hdg(i)*hdg(j)+
     &          fae*dg(i)*dg(j)
	      enddo
	    enddo
        endif
        do i=1,n
          xi(i)=0.
          do j=1,n
            xi(i)=xi(i)-hessin(i,j)*g(j)
	    enddo
	  enddo
	enddo       !end of iteration loop
	if(.not.silent) then
	   print 2,itmax
2	   format(' Reached maximum number of iterations (= ',i4,')')
	   nerr=1
	endif
99	continue
c Unsqueeze all arrays before return
	call unsqz1(theta,p,jfix,nt)	!!unsqueeze p into theta for dfunc
	call unsqz1(g1,g,jfix,nt)	!ditto
	call unsqz1(xi1,xi,jfix,nt)
	call unsqz1(pnew1,pnew,jfix,nt)
      RETURN
c
c Now original code for dfpmin, with no parameters fixed
100	continue
	do i=1,n
	   p(i)=theta(i)	!rename theta as p()
	enddo
c
      fp=func(p)
      call dfunc(p,g)
      sum=0.
      do 12 i=1,n
        do 11 j=1,n
          hessin(i,j)=0.
11      continue
        hessin(i,i)=1.
        xi(i)=-g(i)
        sum=sum+p(i)**2
12    continue
      stpmax=STPMX*max(sqrt(sum),float(n))
c
c Iteration starts here
      do 27 its=1,ITMAX
        iter=its
	  if(KBHIT()) then
		ch=GETCH(ktype)
		ival=ichar(ch)
		if(ktype.eq.0.and.(ival.eq.59.or.ival.eq.60)) then
		   abort=.true.
		   RETURN
		endif
        endif
c lnsrch returns fret=func(pnew) if alam.ge.alamin, but otherwise
c pnew is reset to p before return, but fret not recalculated as func(p)
c In latter case returns with check=true.  This should be true only
c if p is close to pnew so should not matter? (see Press et al)
        call LNSRCH(n,p,fp,g,xi,pnew,fret,stpmax,check,func)
c=      fp=fret
c===addition
        if(check) then
		do i=1,nt
		   pnew1(i)=theta(i)	!restore original (and don't change fp)
		enddo
	  else
c===
           fp=fret
	  endif
        do 13 i=1,n
          xi(i)=pnew(i)-p(i)
          p(i)=pnew(i)
13      continue
	  if(.not.silent) then
	   if((its-itsav).ge.ndisp) then
	    call DSPOUT1(nt,its,itsav,fp,p,dprt)	!type it
	   endif
	  endif
        test=0.
        do 14 i=1,n
          temp=abs(xi(i))/max(abs(p(i)),1.)
          if(temp.gt.test)test=temp
14      continue
c=      if(test.lt.TOLX)return
        if(test.lt.TOLX) goto 999
        do 15 i=1,n
          dg(i)=g(i)
15      continue
        call dfunc(p,g)
	  if(.not.silent) then
	   if((its-itsav).ge.ndisp) then
	    call DSPOUT1(nt,its,itsav,fp,p,dprt)	!type it
	   endif
	  endif
        test=0.
        den=max(fret,1.)
        do 16 i=1,n
          temp=abs(g(i))*max(abs(p(i)),1.)/den
          if(temp.gt.test)test=temp
16      continue
c=      if(test.lt.gtol)return
        if(test.lt.gtol) goto 999
        do 17 i=1,n
          dg(i)=g(i)-dg(i)
17      continue
        do 19 i=1,n
          hdg(i)=0.
          do 18 j=1,n
            hdg(i)=hdg(i)+hessin(i,j)*dg(j)
18        continue
19      continue
        fac=0.
        fae=0.
        sumdg=0.
        sumxi=0.
        do 21 i=1,n
          fac=fac+dg(i)*xi(i)
          fae=fae+dg(i)*hdg(i)
          sumdg=sumdg+dg(i)**2
          sumxi=sumxi+xi(i)**2
21      continue
        if(fac**2.gt.EPS*sumdg*sumxi)then
          fac=1./fac
          fad=1./fae
          do 22 i=1,n
            dg(i)=fac*xi(i)-fad*hdg(i)
22        continue
          do 24 i=1,n
            do 23 j=1,n
              hessin(i,j)=hessin(i,j)+fac*xi(i)*xi(j)-fad*hdg(i)*hdg(j)+
     *fae*dg(i)*dg(j)
23          continue
24        continue
        endif
        do 26 i=1,n
          xi(i)=0.
          do 25 j=1,n
            xi(i)=xi(i)-hessin(i,j)*g(j)
25        continue
26      continue
27    continue
	if(.not.silent) then
	   print 2,itmax
c2	   format(' Reached maximum number of iterations (= ',i4,')')
	   nerr=1
	endif
999	continue
	do i=1,n
	   theta(i)=p(i)	!rename p() as theta for return
	enddo
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software #1,15(15..


c DC modified version of lnsrch for case when some parameters fixed
       subroutine LNSRCH1(nt,n,xold1,fold,g1,p1,x1,f,stpmax,
     &	check,func,jfix)
c       call lnsrch(nt,n,theta,fp,g1,xi1,pnew1,fret,stpmax,
c     &	check,func,jfix)
c      SUBROUTINE lnsrch(n,xold,fold,g,p,x,f,stpmax,check,func,jfix)
c Note equivalences of arrays (1st name=all param; 2nd name= no-fixed param only
c  Param #     In call        Here
c    3        theta/p       xold1/xold
c    5        g1/g           g1/g
c    6        xi1/xi         p1/p
c    7        pnew1/pnew     x1/x
      INTEGER n
	integer jfix(nt)
	real*4 g1(nt),p1(nt),x1(nt),xold1(nt)	!for full-size arrays
      LOGICAL check
c      REAL f,fold,stpmax,g(n),p(n),x(n),xold(n),func,ALF,TOLX
      REAL f,fold,stpmax,func,ALF,TOLX
      PARAMETER (nmax=50,ALF=1.e-4,TOLX=1.e-7)
	real g(nmax),p(nmax),x(nmax),xold(nmax)
c=	real pnew1(nmax),xi1(nmax),theta(nmax)	!not used here
      EXTERNAL func
CU    USES func
      INTEGER i
      REAL a,alam,alam2,alamin,b,disc,f2,fold2,rhs1,rhs2,slope,sum,temp,
     *test,tmplam
	logical silent
	common/err/nerr,silent	!for sfunc,dsfunc
c
	call sqz1(xold1,xold,jfix,nt)	!squeeze theta into xold
	call sqz1(g1,g,jfix,nt)		!squeeze non-fixed gradients into g
	call sqz1(p1,p,jfix,nt)		!squeeze p1 into p
	call sqz1(x1,x,jfix,nt)		!squeeze x1 into x
c
      check=.false.
      sum=0.
      do 11 i=1,n
        sum=sum+p(i)*p(i)
11    continue
      sum=sqrt(sum)
      if(sum.gt.stpmax)then
        do 12 i=1,n
          p(i)=p(i)*stpmax/sum
12      continue
      endif
      slope=0.
      do 13 i=1,n
        slope=slope+g(i)*p(i)
13    continue
      test=0.
      do 14 i=1,n
        temp=abs(p(i))/max(abs(xold(i)),1.)
        if(temp.gt.test)test=temp
14    continue
      alamin=TOLX/test
      alam=1.
1     continue
        do 15 i=1,n
          x(i)=xold(i)+alam*p(i)
15      continue
	  call unsqz1(x1,x,jfix,nt)		!unsqueeze x into x1
c=        f=func(x)
        f=func(x1)
	  call sqz1(x1,x,jfix,nt)		!squeeze x1 into x
        if(alam.lt.alamin)then
          do 16 i=1,n
            x(i)=xold(i)
16        continue
          check=.true.
c=        return
	    goto 99
        else if(f.le.fold+ALF*alam*slope)then
c=        return
	    goto 99
        else
          if(alam.eq.1.)then
            tmplam=-slope/(2.*(f-fold-slope))
          else
            rhs1=f-fold-alam*slope
            rhs2=f2-fold2-alam2*slope
            a=(rhs1/alam**2-rhs2/alam2**2)/(alam-alam2)
            b=(-alam2*rhs1/alam**2+alam*rhs2/alam2**2)/(alam-alam2)
            if(a.eq.0.)then
              tmplam=-slope/(2.*b)
            else
		  if(abs(b).gt.1.) then
		    if(b.gt.1.e37/b) then
		      nerr=3
		      RETURN
		    endif
		  endif
              disc=b*b-3.*a*slope
              if(disc.lt.0.) then
		    if(.not.silent) then
			print 3
3			format(' Roundoff problem in lnsrch')
		    endif
		    nerr=2
		    disc=0.0
		  endif
              tmplam=(-b+sqrt(disc))/(3.*a)
            endif
            if(tmplam.gt..5*alam)tmplam=.5*alam
          endif
        endif
        alam2=alam
        f2=f
        fold2=fold
        alam=max(tmplam,.1*alam)
      goto 1
c Return, first unsqeezing all arrays
99	continue
c//	call unsqz1(xold1,xold,jfix,nt)	!xold1 not changed by lnsrch1
c//	call unsqz1(g1,g,jfix,nt)		!g1 not changed by lnsrch1
	call unsqz1(p1,p,jfix,nt)
	call unsqz1(x1,x,jfix,nt)
	RETURN
      END
C  (C) Copr. 1986-92 Numerical Recipes Software #1,15(15..

c Original lnsrch, for case when no parameters are fixed
      SUBROUTINE lnsrch(n,xold,fold,g,p,x,f,stpmax,check,func)
      INTEGER n
      LOGICAL check
      REAL f,fold,stpmax,g(n),p(n),x(n),xold(n),func,ALF,TOLX
      PARAMETER (ALF=1.e-4,TOLX=1.e-7)
      EXTERNAL func
CU    USES func
      INTEGER i
      REAL a,alam,alam2,alamin,b,disc,f2,fold2,rhs1,rhs2,slope,sum,temp,
     *test,tmplam
	logical silent
	common/err/nerr,silent	!for sfunc,dsfunc
c
      check=.false.
      sum=0.
      do 11 i=1,n
        sum=sum+p(i)*p(i)
11    continue
      sum=sqrt(sum)
      if(sum.gt.stpmax)then
        do 12 i=1,n
          p(i)=p(i)*stpmax/sum
12      continue
      endif
      slope=0.
      do 13 i=1,n
        slope=slope+g(i)*p(i)
13    continue
      test=0.
      do 14 i=1,n
        temp=abs(p(i))/max(abs(xold(i)),1.)
        if(temp.gt.test)test=temp
14    continue
      alamin=TOLX/test
      alam=1.
1     continue
        do 15 i=1,n
          x(i)=xold(i)+alam*p(i)
15      continue
        f=func(x)
        if(alam.lt.alamin)then
          do 16 i=1,n
            x(i)=xold(i)
16        continue
          check=.true.
          return
        else if(f.le.fold+ALF*alam*slope)then
          return
        else
          if(alam.eq.1.)then
            tmplam=-slope/(2.*(f-fold-slope))
          else
            rhs1=f-fold-alam*slope
            rhs2=f2-fold2-alam2*slope
            a=(rhs1/alam**2-rhs2/alam2**2)/(alam-alam2)
            b=(-alam2*rhs1/alam**2+alam*rhs2/alam2**2)/(alam-alam2)
            if(a.eq.0.)then
              tmplam=-slope/(2.*b)
            else
c===debug: prevents overflow of disc, but just overflows elsewhere eg at "a="
c=		  if(b.gt.1.e15) then
c=		    rootd=b		!approx!!
c=		  else
c=                 disc=b*b-3.*a*slope
c=                 if(disc.lt.0.) then
c=			  pause 'roundoff problem in lnsrch'
c=			  rootd=0.0
c=			else
c=			  rootd=sqrt(disc)
c=			endif
c=		  endif
c=            tmplam=(-b+rootd)/(3.*a)
		  if(abs(b).gt.1.) then
		    if(b.gt.1.e37/b) then
		      nerr=3
		      RETURN
		    endif
		  endif
	        disc=b*b-3.*a*slope
	        if(disc.lt.0.) then
		    if(.not.silent) then
			print 3
3			format(' Roundoff problem in lnsrch')
		    endif
		    nerr=2
		    disc=0.0
		  endif
	        tmplam=(-b+sqrt(disc))/(3.*a)
            endif
            if(tmplam.gt..5*alam)tmplam=.5*alam
          endif
        endif
        alam2=alam
        f2=f
        fold2=fold
        alam=max(tmplam,.1*alam)
      goto 1
      END
C  (C) Copr. 1986-92 Numerical Recipes Software #1,15(15..



	subroutine sqz1(a,b,jfix,k)
	real a(k),b(k)
	integer jfix(k)
c
c squeeze a into b (a and b could be same)
	j=0
	do i=1,k
	   if(jfix(i).eq.0) then
		j=j+1
		b(j)=a(i)
	   endif
	enddo
	return
	end

	subroutine unsqz1(a,b,jfix,k)
	real a(k),b(k)
	integer jfix(k)
c restore b into a (a and b must be different)
	j=0
	do i=1,k
	   if(jfix(i).eq.0) then
		j=j+1
		a(i)=b(j)
	   endif
	enddo
	return
	end

	subroutine DSPOUT1(k,Neval,ndsav,fmin,TH,dprt)
C TO DO PRINTING
	real TH(k)
	logical dprt
C
	if(k.le.6) then
	   print 11,neval,fmin,(th(m),m=1,k)
	   if(dprt) write(8,11) neval,fmin,(th(m),m=1,k)
11	   format(i8,g13.6,/,6g13.6)
	else
	   print 10,neval,fmin,(th(m),m=1,k)
	   if(dprt) write(8,10) neval,fmin,(th(m),m=1,k)
10	   format(i8,g13.6,/,4(5g13.6,/))
	endif
	NDSAV=NEVAL
	RETURN
	END

