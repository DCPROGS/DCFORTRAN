	subroutine TCRITS1(amean,area,ncomp,kth,noprint,tc,imode,tc1)
c TCRITS1 is version if tcrits (used in ekdist) for calulation of tcrit
c values in theory programs
c
c TCRITS is version of TCSUB/1 that uses ALL components of the shut time
c distribution (defined in amean,area,ncomp).
c Assumes that tau, area have been sorted into ascending order of tau
c values (i.e. fastest is first . . .).  In EKDIST this is now done
c in PTYP1.
c  If 1<kth<ncomp-1 then calculates tcrits (3 versions) for
c	dividing kth from kth+1 component. that outputs the three tcrit values
c  If kth.le.0 then calculates all ncomp-1 tcrits
c  If NOPRINT=true, then does no printing at all
c  If imode=0 then calculates all the tcrits (output in tc(100,3)).
c  If imode=1 then takes tc1=tcrit and just calculates misclassification
c   for this tcrit
c  Assumes that amean,area, ncomp defined on entry (add option to enter here
c	if ncomp=0?)
c
C INCLUDES BISECT IN SAME FILE.
	real*4 amean(100),area(100)
c	character*1 ans,UC
	real*4 errmax(100)
c	allocatable::tc
c	real*4 tc(:,:)
	real*4 tc(100,3)
	logical discprt,noprint
	common/dp/discprt
	COMMON/TBLK/ tau(100),a(100),ncomp1,kth1
	EXTERNAL TCFUNC1,TCFUNC2,TCFUNC3
c
c For print of misclassification only, must first find which tau values
c the tcrit lies between -assume that these are the 'correct' ones for
c definition of bursts (must assume something about what correct classification
c of shut time means to calculate misclassification)
	if(imode.eq.1) then
	   tcrit=tc1
	   do m=1,ncomp-1
		if(tcrit.ge.tau(m).and.tcrit.lt.tau(m+1)) then
		   k1=m
		   goto 55
		endif
	   enddo
55	   continue
	   if(k1.eq.ncomp) RETURN
	   print 56,k1,k1+1
	   if(discprt) write(8,56) k1,k1+1
56	   format(/,
     &' MISCLASSIFICATION assuming component ',i2,' is within bursts',/,
     & '   and component ',i2,' is between bursts',/)
	   call PRTERR(tc1,area,tau,k1,ncomp,ntot,errmax,.false.,discprt)
	   RETURN
	endif
c
	ndisp=-1	!NO print
	eps=1.e-4	! 0.1 microsec error
	epsy=-1.
c==	allocate(tc(10,3))
c
10	continue
	if(ncomp.le.0.and.(.not.noprint)) then
	   print 1
1	   format(' Number of components in shut time distribution = ')
	   call INPUTi(ncomp)
	   sa=0.0
	   do m=1,ncomp
		print 2,m
2		format(' tau(',i2,') = ')
		call INPUTr(amean(m))
		if(m.lt.ncomp) then
		print 3,m
3		   format('& fractional area(',i2,') = ')
		   call INPUTr(area(m))
		   sa=sa+area(m)
		   if(sa.gt.1.0) then
			call BELL(1)
			print 4
4			format(' AREA GREATER THAN 1: TRY AGAIN')
			ncomp=0
			goto 10
		   endif
		endif
	   enddo
	   area(ncomp)=1.0 - sa
	endif
c
c Sort again, to be sure
	call SORT2r(amean,area,ncomp,100,.true.)
c
c Make copies for common
	do i=1,ncomp
	   tau(i)=amean(i)
	   a(i)=area(i)
	enddo
	ncomp1=ncomp
	kth1=kth
c
	if(kth.le.0) then		!get all tcrits
	   n1=1
	   n2=ncomp-1
	else
	   n1=kth			!calc kth only
	   n2=kth
	endif
c
	ntot=100
c
	do k1=n1,n2
	   kth1=k1		!for common/tblk
	   print 5,k1,k1+1
	   if(discprt) write(8,5) k1,k1+1
5	   format(/,
     & ' CRITICAL TIME (tcrit) BETWEEN COMPONENTS ',i2,' AND ',i2,/)
c
	   errmax(k1)=0.0
	   do np=1,3		!3 methods
		if(np.eq.1) then
		   x1=amean(k1)		!init guess
		   x2=amean(k1+1)		!init guess
		   call BISEC0(TCFUNC1,x1,x2,0.0,tcrit,yout,eps,epsy,
     &		nerr,ndisp,.false.)
		   if(nerr.eq.0) then
		      tc(k1,np)=tcrit
		   else
			tc(k1,np)=-1.
		   endif
		   if(noprint) goto 7
	         print 52
	         if(discprt) write(8,52)
52		   format(' Equal % misclassified (DC criterion)')
		   if(nerr.ne.0) then
		      print 821,nerr
		      if(discprt) write(8,821) nerr
821 			format(' BISECTION FAILED (error # ',i2,')')
			goto 99	!next calculation type
		   endif
 		else if(np.eq.2) then
		   x1=amean(k1)		!init guess
		   x2=amean(k1+1)		!init guess
		   call BISEC0(TCFUNC2,x1,x2,0.0,tcrit,yout,eps,epsy,
     &		nerr,ndisp,.false.)
		   if(nerr.eq.0) then
		      tc(k1,np)=tcrit
		   else
			tc(k1,np)=-1.
		   endif
		   if(noprint) goto 7
	         print 53
	         if(discprt) write(8,53)
53		   FORMAT(/,
     & ' Equal numbers misclassified (Clapham & Neher criterion)')
		   if(nerr.ne.0) then
		      print 821,nerr
		      if(discprt) write(8,821) nerr
			goto 99	!next calculation type
		   endif
		else if(np.eq.3) then
		   x1=amean(k1)		!init guess
		   x2=amean(k1+1)		!init guess
		   call BISEC0(TCFUNC3,x1,x2,0.0,tcrit,yout,eps,epsy,
     &		nerr,ndisp,.false.)
		   if(nerr.eq.0) then
		      tc(k1,np)=tcrit
		   else
			tc(k1,np)=-1.
		   endif
		   if(noprint) goto 7
	         print 54
	         if(discprt) write(8,54)
54		   FORMAT(/,
     & ' Minimum total number misclassified (Jackson et al criterion)')
		   if(nerr.ne.0) then
		      print 821,nerr
		      if(discprt) write(8,821) nerr
			goto 99	!next calculation type
		   endif
		endif
7		continue
	      print 6,tcrit
	      if(discprt) write(8,6) tcrit
6		format( ' Tcrit (ms)= ',g11.4)
		call PRTERR(tcrit,area,tau,k1,ncomp,ntot,errmax,.false.,
     &	 discprt)
c		s1=0.0			!# of 'fast' events
c		enf=0.0                 !# misclassified
c
c		do m=1,k1			!sum components below tcrit (fast ones)
c		   enf=enf + area(m)*exp1(-tcrit/tau(m))
c		   s1=s1+area(m)
c		enddo
c		pf=enf/s1		!fraction misclassified
c		enf=float(ntot)*enf
cc
c		s2=0.0
c		ens=0.0
c		do m=k1+1,ncomp      !sum components above tcrit (slow ones)
c		   ens=ens + area(m)*(1.0-exp1(-tcrit/tau(m)))
c		   s2=s2+area(m)
c		enddo
c		ps=ens/s2
c		ens=float(ntot)*ens
c	      print 30,100.*pf,100.*ps
c	      if(discprt) write(8,30) 100.*pf,100.*ps
c30		FORMAT( ' % short misclassified= ',g11.4,
c     &	   ' % long misclassified= ',g11.4)
cc
c	      print 31,enf,ens
c      	if(discprt) write(8,31) enf,ens
c31		format( ' # short misclassified= ',G11.4,
c     &	' # long misclassified= ',G11.4)
c		print 32,enf+ens
c	      if(discprt) write(8,32) enf+ens
c32		format( ' Total # misclassified (out of 100)= ',G11.4)
c		if(enf+ens.gt.errmax(k1)) errmax(k1)=enf+ens
c
99		continue
	   enddo	!end of np=1,2,3
	enddo
c
	print 20
	if(discprt) write(8,20)
20	format(/,' SUMMARY of Tcrit values (-1 means not defined):',/,
     & '  Components             Tcrit (by 3 criteria)        '
     & 'Max total misclassif')
	do k1=n1,n2
	   print 21,k1,k1+1,(tc(k1,np),np=1,3),errmax(k1)
	   if(discprt) write(8,21) k1,k1+1,(tc(k1,np),np=1,3),errmax(k1)
21	   format(1x,i3,' to ',i3, 4(2x,g13.6))
	enddo
c==	deallocate(tc)
c
c	print 120
c120	FORMAT(' Another Tcrit calculation [N] ? ')
c	read 101,ans
c	if(UC(ans).eq.'Y') GOTO 10
C
	RETURN
	END



	function TCFUNC1(tcrit)
	COMMON/TBLK/ tau(100),a(100),ncomp,kth
	real*8 a2,x2,e2	!to avoid floating underflow
c C and S method -equal fraction
c
	s1=0.0			!# of 'fast' events
	enf=0.0                 !# misclassified
	do m=1,kth			!sum components below tcrit (fast ones)
	   x2=dble(tcrit)/dble(tau(m))
	   a2=dble(a(m))
	   e2=dexp1(-x2)
	   if(e2.gt.1.d-300/a2) then	!check a(m)*exp > 1,d-300
		a2=e2*a2
	   else
		a2=0.d0
	   endif
	   if(dabs(a2).lt.1.d-37) then
		a1=0.
	   else
		a1=sngl(a2)
	   endif
	   enf=enf + a1
	   s1=s1+a(m)
	enddo
	pf=enf/s1		!fraction misclassified
c
	s1=0.0
	ens=0.0
	do m=kth+1,ncomp      !sum components above tcrit (slow ones)
	   x2=dble(tcrit)/dble(tau(m))
	   a2=dble(a(m))
	   e2=1.d0-dexp1(-x2)
	   if(e2.gt.1.d-300/a2) then	!check a(m)*exp > 1,d-300
		a2=e2*a2
	   else
		a2=0.d0
	   endif
	   if(dabs(a2).lt.1.d-37) then
		a1=0.
	   else
		a1=sngl(a2)
	   endif
	   ens=ens + a1
	   s1=s1+a(m)
c	   ens=ens + a(m)*(1.0-exp1(-tcrit/tau(m)))
	enddo
	ps=ens/s1
c
	tcfunc1=ps-pf
	RETURN
	END

	function TCFUNC2(tcrit)
	COMMON/TBLK/ tau(100),a(100),ncomp,kth
	real*8 a2,x2,e2	!to avoid floating underflow
c Clapham and Neher Method -equal number misclassif
	enf=0.0                 !# misclassified
	do m=1,kth			!sum components below tcrit (fast ones)
	   x2=dble(tcrit)/dble(tau(m))
	   a2=dble(a(m))
	   e2=dexp1(-x2)
	   if(e2.gt.1.d-300/a2) then	!check a(m)*exp > 1,d-300
		a2=e2*a2
	   else
		a2=0.d0
	   endif
	   if(dabs(a2).lt.1.d-37) then
		a1=0.
	   else
		a1=sngl(a2)
	   endif
	   enf=enf + a1
c	   enf=enf + a(m)*exp1(-tcrit/tau(m))
	enddo
c
	ens=0.0
	do m=kth+1,ncomp      !sum components above tcrit (slow ones)
	   x2=dble(tcrit)/dble(tau(m))
	   e2=(1.d0-dexp1(-x2))
	   a2=dble(a(m))
	   if(e2.gt.1.d-300/a2) then	!check a(m)*exp > 1,d-700
		a2=e2*a2
	   else
		a2=0.d0
	   endif
c
	   if(dabs(a2).lt.1.d-37) then
		a1=0.
	   else
		a1=sngl(a2)
	   endif
	   ens=ens + a1
c	   ens=ens + a(m)*(1.0-exp1(-tcrit/tau(m)))
	enddo
c
	tcfunc2=ens-enf
	RETURN
	END

	function TCFUNC3(tcrit)
	COMMON/TBLK/ tau(100),a(100),ncomp,kth
c	real*8 a2,x2	!to avoid floating underflow
c Jackson et al Method
c NB despite use of exp1() can still get underflow if, say, exp1()=1.e-35
c and a(m)/tau(m) is small enough -but need care -can't divide by exp1 if
c it has been set to 0.0
	s1=0.0
	do m=1,kth			!sum components below tcrit (fast ones)
	   e=exp1(-tcrit/tau(m))
	   c=a(m)/tau(m)
	   if(e.gt.1.e-37/c) then	!check c*e>1.e-37 to avoid underflow
		s1=s1 + c*e
	   endif
c=	   s1=s1 + (a(m)/tau(m))*exp1(-tcrit/tau(m))
	enddo
	s2=0.0
	do m=kth+1,ncomp      !sum components above tcrit (slow ones)
c=	   s2=s2 + (a(m)/tau(m))*exp1(-tcrit/tau(m))
	   e=exp1(-tcrit/tau(m))
	   c=a(m)/tau(m)
	   if(e.gt.1.e-37/c) then	!check c*e>1.e-37 to avoid underflow
		s2=s2 + c*e
	   endif
	enddo
	tcfunc3=s1-s2
	RETURN
	END


	subroutine PRTERR(tcrit,area,tau,k1,ncomp,ntot,errmax,pon,discprt)
	real*4 errmax(100),tau(100),area(100)
	logical pon,discprt

		s1=0.0			!# of 'fast' events
		enf=0.0                 !# misclassified

		do m=1,k1			!sum components below tcrit (fast ones)
		   enf=enf + area(m)*exp1(-tcrit/tau(m))
		   s1=s1+area(m)
		enddo
		pf=enf/s1		!fraction misclassified
		enf=float(ntot)*enf
c
		s2=0.0
		ens=0.0
		do m=k1+1,ncomp      !sum components above tcrit (slow ones)
		   ens=ens + area(m)*(1.0-exp1(-tcrit/tau(m)))
		   s2=s2+area(m)
		enddo
		ps=ens/s2
		ens=float(ntot)*ens
	      print 30,100.*pf,100.*ps
      	if(pon) write(7,30) 100.*pf,100.*ps
	      if(discprt) write(8,30) 100.*pf,100.*ps
30		FORMAT( ' % short misclassified= ',g11.4,
     &	   ' % long misclassified= ',g11.4)
c
	      print 31,enf,ens
	      if(pon) write(7,31) enf,ens
      	if(discprt) write(8,31) enf,ens
31		format( ' # short misclassified= ',G11.4,
     &	' # long misclassified= ',G11.4)
		print 32,enf+ens
      	if(pon) write(7,32) enf+ens
	      if(discprt) write(8,32) enf+ens
32		format( ' Total # misclassified (out of 100)= ',g11.4,/)
		if(enf+ens.gt.errmax(k1)) errmax(k1)=enf+ens
c
	RETURN
	end
