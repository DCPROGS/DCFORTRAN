	subroutine SETTHETA(theta,kmax,jfix,kfix,itype,ntrans)
c
c To input theta defined for ifitmode=1, and output a redefined theta
c that has elements, and kmax, defined as they would be for
c ifitmode=0, so calcs can always be done in same (ifitmode=0) way
c following simplex
c  Also redefines jfix and kfix (though fitting already done) so that
c appropriate levels marked as 'fixed'
c Modif 01/09/94 12:33pm so baseline not marked as fixed here: kfix() now
c defined before fitting in DEF_ONE, and not changed here.
c
c THETA defined thus:
c theta(1)= t0 = time in microsec from 1st point on display TO 1st transition
c theta(2)= time in microsec from 1st point on display TO 2nd transition
c . . .
c theta(nt)= time in microsec from 1st point on display TO (nt)th transition
c AND, when ifitmode=0,
c theta(nt+1)='amplitude' (floating Y units) before 1st trans
c theta(nt+2)='amplitude' (ditto) between 1st and 2nd trans
c theta(nt+3)='amplitude' (ditto) between 1st and 3rd trans
c . . .
c theta(2*nt)='amplitude' (ditto) between (nt-1)th and nt'th transition
c theta(2*nt+1)='amplitude' (ditto) after (nt)th transition
c Total number of param=2*nt+1  (some of these may be fixed!)
c
c OR,  when ifitmode=1.
c  (1) when shut before and after:
c theta(nt+1)=open level
c theta(nt+2)=baseline before 1st trans
c theta(nt+3)=baseline after last transition
c  (2) when shut initially, open at end
c theta(nt+1)=open level
c theta(nt+2)=baseline before 1st trans
c  (3) when open initially, shut at end
c theta(nt+1)=open level
c theta(nt+2)=baseline after last trans
c  (4) when open at each end
c theta(nt+1)=open level
c theta(nt+2)=baseline (FIXED parameter)
c
c Fixed amplitudes defined initially in kfix(i), i=0,...,ntrans; i=0 applies
c to current level before the first transition, kfix(i) applies to the
c amplitude of event following the ith transition.  Set kfix(i)=0 if not
c fixed; =1 if fixed at baseline; =2 if fixed at an open level
c

	real*4 theta(50),theta0(50)
	integer jfix(50),kfix(0:50)
c
c theta(1) to theta(nt) are defined same for both
	kmax0=2*ntrans+1		!number of param for ifitmode=0
c
	if(itype.eq.1) then	!shut/shut (so ntrans=>2 and even)
	   theta0(ntrans+1)=theta(ntrans+2)		!pre-baseline
	   base1=0.5*(theta(ntrans+2)+theta(ntrans+3))	!mean baseline (local var)
	   j=ntrans+2
1	   theta0(j)=theta(ntrans+1)        !open
	   j=j+1
	   if(j.gt.kmax0) goto 9
	   if(j.eq.kmax0) then
	      theta0(kmax0)=theta(ntrans+3)	!post-baseline
	   else
		theta0(j)=base1               !mean baseline
c		jfix(j)=1				!mark this as fixed
c		kfix(j-ntrans-1)=1		! -fixed at baseline
	   endif
	   j=j+1
	   if(j.gt.kmax0) goto 9
	   goto 1
c
c	   if(ntrans.gt.1) then
c            do j=ntrans+2,kmax0-1,2
c		   theta0(j)=theta(ntrans+1)
c		   theta0(j+1)=base1
c		enddo
c	   endif
c	   theta0(2*ntrans+1)=theta(ntrans+3)	!post-baseline
	else if(itype.eq.2) then	!shut/open (so ntrans=>1 and odd)
	   theta0(ntrans+1)=theta(ntrans+2)		!pre-baseline
	   j=ntrans+2
2	   theta0(j)=theta(ntrans+1)  !open
	   j=j+1
	   if(j.gt.kmax0) goto 9
	   theta0(j)=theta(ntrans+2)	!pre-baseline
c	   jfix(j)=1			!mark this as fixed
c	   kfix(j-ntrans-1)=1		! -fixed at baseline
	   j=j+1
	   if(j.gt.kmax0) goto 9
	   goto 2
c	   if(ntrans.gt.1) then
c            do j=ntrans+2,kmax0,2
c		   theta0(j)=theta(ntrans+1)        !open
c		   theta0(j+1)=theta(ntrans+2)	!pre-baseline
c		enddo
c	   endif
	else if(itype.eq.3) then	!open/shut (so ntrans=>1 and odd)
	   theta0(ntrans+1)=theta(ntrans+1)		!level before 1st trans=open
	   j=ntrans+2
3	   theta0(j)=theta(ntrans+2)  !post baseline
c	   jfix(j)=1			!mark this as fixed
c	   kfix(j-ntrans-1)=1		! -fixed at baseline
	   j=j+1
	   if(j.gt.kmax0) goto 9
	   theta0(j)=theta(ntrans+1)	!open
	   j=j+1
	   if(j.gt.kmax0) goto 9
	   goto 3
c         if(ntrans.gt.1) then
c            do j=ntrans+2,kmax0,2
c		   theta0(j)=theta(ntrans+2)        !post-baseline
c		   theta0(j+1)=theta(ntrans+1)	!open
c		enddo
c	   endif
	else if(itype.eq.4) then	!open/open (so ntrans=>2 and even)
	   theta0(ntrans+1)=theta(ntrans+1)		!level before 1st trans=open
         base1=theta(ntrans+2)        !baseline (fixed param)
	   j=ntrans+2
4	   theta0(j)=base1            !fixed baseline
c	   jfix(j)=1			!mark this as fixed
c	   kfix(j-ntrans-1)=1		! -fixed at baseline
	   j=j+1
	   if(j.gt.kmax0) goto 9
	   theta0(j)=theta(ntrans+1)	!open
	   j=j+1
	   if(j.gt.kmax0) goto 9
	   goto 4
c         if(ntrans.gt.1) then
c            do j=ntrans+2,kmax0,2
c		   theta0(j)=base1		      !fixed baseline
c		   theta0(j+1)=theta(ntrans+1)	!open
c		enddo
c	   endif
	endif
c
9	continue
c Copy revised theta0 values into theta for return to calling prog
	do i=ntrans+1,kmax0
	   theta(i)=theta0(i)
	enddo
c and revised kmax
	kmax=kmax0
c
	RETURN
	end

