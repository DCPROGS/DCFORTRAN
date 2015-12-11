	   dodfp=minmeth.eq.2.or.minmeth.eq.3
	   if(minmeth.eq.1.or.minmeth.eq.2) then
	     call SIMPLEX3(kmax,theta,stpfac,errfac,neval1,nevmax,
     &     smin,SCANFUNC,Ndisps,jfix,delmin,confac,irestrt,iconv)
	   endif
c	   smin=SCANFUNC(kmax,theta)		!check for debug
c	   smin1=SFUNC(theta)		!check for debug
	   if(minmeth.eq.2.and.neval1.ge.nevmax) then
  	     call WDIALOG(1,' -swapping to DFPMIN now',-11)
		if(minmeth.eq.2) then		!save in case better than dfpmin
		   do i=1,kmax
			theta0(i)=theta(i)
		   enddo
		endif
	   else
		dodfp=.false.	!simplex converged so no need for dfp
	   endif
	   if((minmeth.eq.2.and.dodfp).or.minmeth.eq.3) then
		gtol=1.e-4
		stpmx=0.1
c		ndisps=0
		ndisps=-1000	!silent
		method=0	!method for derivs in dsfunc
      	call DFPMIN1(theta,kmax,gtol,iter,smin1,sfunc,dsfunc,
     &       stpmx,jfix,ndisps)
c		smin1=SFUNC(theta)		!check for debug
		if(minmeth.eq.2) then
c Calc improvement in log-likelihood produced by dfpmin1, over simplex when
c both use (NB SSD are div by if2-if1+1 in sfunc, scanfunc)
c In DFPMIN (other error codes from DSFUNC?)
c===insert check for adjacent equal amps or zero lenght intervals in
c===output from simplex before going on to dfpmin1
c nerr=1	Reached itmax					Premature end
c nerr=2	Disc negative in LNSRCH				disc set to 0.0
c nerr=3	Overflow of b*b in LNSRCH			Run aborted
		   k=0
		   do i=1,kmax
			if(jfix(i).eq.0) k=k+1
		   enddo
		   np=if2-if1+1
		   s2=smin1/float(np-k)		!error varaince
	         dll=abs(0.5*(smin-smin1)*float(if2-if1+1))/s2
	         call DCFORMAT(dll,8,2,cnum)
		   n=NBLANK(cnum)
		   if(smin1.gt.smin) then
			do i=1,kmax
			   theta(i)=theta0(i)	!revert to simplex results
			enddo
		      call WDIALOG(1,
     &	' -Simplex was better fit by L = '//cnum(1:n),11)
		   else
		      call WDIALOG(1,
     &	' -DFPMIN improved fit by L = '//cnum(1:n),11)
		   endif
		   if(nerr.ne.0) then
			call TONE(110,30)	!low note!
			call INTCONV(nerr,cnum)
		      n=NBLANK(cnum)
		      call WDIALOG(1,
     & ' -DFPMIN returned error # '//cnum(1:n),icw1)
			call POPTIME(200,150,
     &		 'DFPMIN aborted',1,14,12,14,0.5)
c    & ' -DFPMIN returned error # '//cnum(1:n)//': hit any key',icw1)
c			call ANYKEY()
		   endif
		endif
	   endif
