	subroutine TCRITS(amean,area,ncomp,kth,noprint)
c TCRITS is version of TCSUB/1 that uses ALL components of the shut time
c distribution (defined in amean,area,ncomp).
c Assumes that tau, area have been sorted into ascending order of tau
c values (i.e. fastest is first . . .).  In EKDIST this is now done
c in PTYP1.
c  If 1<kth<ncomp-1 then calculates tcrits (3 versions) for
c	dividing kth from kth+1 component. that outputs the three tcrit values
c  If kth.le.0 then calculates all ncomp-1 tcrits
c  If NOPRINT=true, then does no printing at all
c  Assumes that amean,area, ncomp defined on entry (add option to enter here
c	if ncomp=0?)
c
	real*4 amean(10),area(10)
	character*1 ans
	logical discprt,pon,slock,noprint
	real*4 errmax(10)
	allocatable::tc
	real*4 tc(:,:)
	common/dp/discprt
	COMMON/TBLK/ tau(10),a(10),ncomp1,kth1
c Need topen=overall mean open period in BLFUNC (calc in cdist1 at present
c from observed values rather than from fit)
	common/top/topen
	EXTERNAL TCFUNC1,TCFUNC2,TCFUNC3
c
	pon()=slock()
C
	ndisp=-1	!NO print
c101	format(a1)
	eps=1.e-4	! 0.1 microsec error
	epsy=-1.
c
	ALLOCATE(tc(10,6))
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
	call SORT2r(amean,area,ncomp,10,.true.)
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
	do k=n1,n2
	   kth1=k		!for common/tblk
	   print 5,k,k+1
         if(pon()) write(7,5) k,k+1
	   if(discprt) write(8,5) k,k+1
5	   format(/,
     & ' CRITICAL TIME (tcrit) BETWEEN COMPONENTS ',i2,' AND ',i2,/)
c
	   errmax(k)=0.0
	   do np=1,3		!3 methods
		if(np.eq.1) then
		   x1=amean(k)		!init guess
		   x2=amean(k+1)		!init guess
		   call BISEC0(TCFUNC1,x1,x2,0.0,tcrit,yout,eps,epsy,
     &		nerr,ndisp,.false.)
		   tc(k,np)=tcrit
		   if(noprint) goto 7
	         print 52
      	   if(pon()) write(7,52)
	         if(discprt) write(8,52)
52		   format(' Equal % misclassified (DC criterion)')
		   if(nerr.ne.0) then
		      print 821
		      if(pon()) write(7,821)
		      if(discprt) write(8,821)
821 			format(' BISECTION FAILED')
			goto 99	!next calculation type
		   endif
 		else if(np.eq.2) then
		   x1=amean(k)		!init guess
		   x2=amean(k+1)		!init guess
		   call BISEC0(TCFUNC2,x1,x2,0.0,tcrit,yout,eps,epsy,
     &		nerr,ndisp,.false.)
		   tc(k,np)=tcrit
		   if(noprint) goto 7
	         print 53
      	   if(pon()) write(7,53)
	         if(discprt) write(8,53)
53		   FORMAT(/,
     & ' Equal numbers misclassified (Clapham & Neher criterion)')
		   if(nerr.ne.0) then
		      print 821
		      if(pon()) write(7,821)
		      if(discprt) write(8,821)
			goto 99	!next calculation type
		   endif
		else if(np.eq.3) then
		   x1=amean(k)		!init guess
		   x2=amean(k+1)		!init guess
		   call BISEC0(TCFUNC3,x1,x2,0.0,tcrit,yout,eps,epsy,
     &		nerr,ndisp,.false.)
		   tc(k,np)=tcrit
		   if(noprint) goto 7
	         print 54
      	   if(pon()) write(7,54)
	         if(discprt) write(8,54)
54		   FORMAT(/,
     & ' Minimum total number misclassified (Jackson et al criterion)')
		   if(nerr.ne.0) then
		      print 821
		      if(pon()) write(7,821)
		      if(discprt) write(8,821)
			goto 99	!next calculation type
		   endif
		endif
7		continue
	      print 6,tcrit
      	if(pon()) write(7,6) tcrit
	      if(discprt) write(8,6) tcrit
6		format( ' Tcrit (ms)= ',g11.4)
		s1=0.0			!# of 'fast' events
		enf=0.0                 !# misclassified

		do m=1,k			!sum components below tcrit (fast ones)
		   enf=enf + area(m)*exp(-tcrit/tau(m))
		   s1=s1+area(m)
		enddo
		pf=enf/s1		!fraction misclassified
		enf=float(ntot)*enf
c
		s2=0.0
		ens=0.0
		do m=k+1,ncomp      !sum components above tcrit (slow ones)
		   ens=ens + area(m)*(1.0-exp(-tcrit/tau(m)))
		   s2=s2+area(m)
		enddo
		ps=ens/s2
		ens=float(ntot)*ens
	      print 30,100.*pf,100.*ps
      	if(pon()) write(7,30) 100.*pf,100.*ps
	      if(discprt) write(8,30) 100.*pf,100.*ps
30		FORMAT( ' % short misclassified= ',g11.4,
     &	   ' % long misclassified= ',g11.4)
c
	      print 31,enf,ens
	      if(pon()) write(7,31) enf,ens
      	if(discprt) write(8,31) enf,ens
31		format( ' # short misclassified= ',G11.4,
     &	' # long misclassified= ',G11.4)
		print 32,enf+ens
      	if(pon()) write(7,32) enf+ens
	      if(discprt) write(8,32) enf+ens
32		format( ' Total # misclassified (out of 100)= ',G11.4)
		if(enf+ens.gt.errmax(k)) errmax(k)=enf+ens
c
99		continue
	   enddo	!end of np=1,2,3
	enddo
c
	print 20
20	format(/,' SUMMARY of tcrit values by 3 methods:',/,
     & '  Components             Tcrit (by 3 criteria)        '
     & 'Max total misclassif')
	do k=n1,n2
	   print 21,k,k+1,(tc(k,np),np=1,3),errmax(k)
21	   format(1x,i3,' to ',i3, 4(2x,g13.6))
	enddo
c
c Now look for
c (1) time for minimum in pdf between each pair of tau(m) =tc(i,4)
c (2) time at which dv/dt is at a minimum where v=burst length
c	i.e. the tcrit for which burst length is least sensitive to tcrit
	print 651
651	format(/,
     &' To calculate tcrit at which d(burst length)/d(Tcrit) is at a',/,
     &' minimum, need overall mean of observed open times (-1 to skip)')
	if(topen.gt.0.) then
	   print 65,topen
65	   format(' Overall mean open period = ',g13.6,' ms: O.K. [Y] ? ')
	   ans='Y'
	   call INPUTa(ans)
	   if(ans.eq.'N') topen=-1.
	endif
	if(topen.lt.0.) then
	   print 66
66	   format(' Overall mean open period = ')
	   call INPUTr(topen)
	endif
	do k=n1,n2
	   kth1=k		!for common/tblk
	   x1=amean(k)		!init guess
	   x2=amean(k+1)		!init guess
	   nval=200
	   dx=(x2-x1)/float(nval-1)
	   if(dx.lt.1.) then	!get to nearest 1 ms
		dx=1.
		nval=1 + ifix((x2-x1)/dx)
	   endif
	   ftmin=1.e37
	   slmin=1.e37
	   do i=1,nval
		tcrit=x1 + float(i-1)*dx
		ft=tcrit*FFUNC(tcrit) 	!pdf of log(t) = t*f(t)
		if(ft.lt.ftmin) then
		   ftmin=ft
		   ifmin=i
		   tc(k,4)=tcrit
		endif
		if(i.gt.1.and.topen.gt.0.) then
		   slope=(BLFUNC(tcrit)-BLFUNC(tclast))/dx
		   if(slope.lt.slmin) then
			slmin=slope
			ismin=i
			tc1=0.5*(tclast+tcrit)
			tc(k,5)=tc1
			bl1=BLFUNC(tc1)
		   endif
		endif
		tclast=tcrit
	   enddo
	   if(ifmin.eq.1.or.ifmin.eq.nval) tc(k,4)=-1.	!no min in range
	   if(ismin.eq.2.or.ismin.eq.nval) tc(k,5)=-1.	!no min in range
c print result
	   if(tc(k,4).gt.0.) then
	      print 61,k,k+1,tc(k,4)
	      if(discprt) write(8,61) k,k+1,tc(k,4)
61		format(/,/,
     & 	' TIME FOR MINIMUM IN f(log(t)) BETWEEN COMPONENTS ',
     &	i2,' AND ',i2,' = ',g13.6,' ms')
	   else if(tc(k,4).lt.0.) then
	      print 62,k,k+1
	      if(discprt) write(8,62) k,k+1
62		format(/,/,
     & 	' NO MINIMUM IN f(log(t)) BETWEEN COMPONENTS ',
     &	i2,' AND ',i2)
	   endif
	   if(topen.gt.0.) then
	    if(tc(k,5).gt.0.) then
	      print 63,k,k+1,tc(k,5),bl1
	      if(discprt) write(8,63) k,k+1,tc(k,5),bl1
63		format(/,
     & 	' TIME BETWEEN COMPONENTS ',i2,' AND ',i2,
     &      ' for minimum d(burst length)/d(Tcrit) = ',g13.6,' ms',/,
     &	'  at which mean burst length = ',g13.6,' ms')
	    else if(tc(k,5).lt.0.) then
	      print 64,k,k+1
	      if(discprt) write(8,64) k,k+1
64		format(/,
     &' NO MINIMUM IN d(burst length)/d(Tcrit) BETWEEN COMPONENTS ',
     &	i2,' AND ',i2,/)
	    endif
	   endif
	enddo
c
	DEALLOCATE(tc)
c
c	print 120
c120	FORMAT(' Another Tcrit calculation [N] ? ')
c	read 101,ans
c	if(UC(ans).eq.'Y') GOTO 10
C
	RETURN
	END


	function FFUNC(tcrit)
	COMMON/TBLK/ tau(10),a(10),ncomp,kth
c Calculates pdf
c
	s1=0.0
	do m=1,ncomp
	   s1=s1 + (a(m)/tau(m))*exp1(-tcrit/tau(m))
	enddo
	ffunc=s1
	RETURN
	end

	function BLFUNC(tcrit)
c Calculates mean burst length for specified tcrit
	COMMON/TBLK/ tau(10),a(10),ncomp,kth
c Need topen=overall mean open period in BLFUNC (calc in cdist1 at present
c from observed values rather than from fit)
	common/top/topen
c
	ts=0.
	ps=0.
	ob=0.
	t1=0.		!for gap>tcrit (-bet-burst) calc
	do i=1,ncomp
	   e1=exp1(-tcrit/tau(i))
	   ts=ts + a(i)*(tau(i)*(1.-e1)-tcrit*e1)
	   ps=ps + a(i)*(1.-e1)
	   ob=ob + a(i)*e1
	   t1=t1 + a(i)*tau(i)*e1
	enddo
c	po=topen/(ts+topen)
	gwb=ts/ps
	ob=1.0/ob
c	gbb=tcrit + t1*ob
	stb=(ob-1.)*gwb	!total shut time/bst
	otb=ob*topen	!total open time/bst
	bl=stb+otb
	BLFUNC=bl
	RETURN
	end


	function TCFUNC1(tcrit)
	COMMON/TBLK/ tau(10),a(10),ncomp,kth
c C and S method -equal fraction
c
	s1=0.0			!# of 'fast' events
	enf=0.0                 !# misclassified
	do m=1,kth			!sum components below tcrit (fast ones)
	   enf=enf + a(m)*exp1(-tcrit/tau(m))
	   s1=s1+a(m)
	enddo
	pf=enf/s1		!fraction misclassified
c
	s2=0.0
	ens=0.0
	do m=kth+1,ncomp      !sum components above tcrit (slow ones)
	   ens=ens + a(m)*(1.0-exp1(-tcrit/tau(m)))
	   s2=s2+a(m)
	enddo
	ps=ens/s2
c
	tcfunc1=ps-pf
	RETURN
	END

	function TCFUNC2(tcrit)
	COMMON/TBLK/ tau(10),a(10),ncomp,kth
c Clapham and Neher Method -equal number misclassif
	enf=0.0                 !# misclassified
	do m=1,kth			!sum components below tcrit (fast ones)
	   enf=enf + a(m)*exp1(-tcrit/tau(m))
	enddo
c
	ens=0.0
	do m=kth+1,ncomp      !sum components above tcrit (slow ones)
	   ens=ens + a(m)*(1.0-exp1(-tcrit/tau(m)))
	enddo
c
	tcfunc2=ens-enf
	RETURN
	END

	function TCFUNC3(tcrit)
	COMMON/TBLK/ tau(10),a(10),ncomp,kth
c Jackson et al Method
	s1=0.0
	do m=1,kth			!sum components below tcrit (fast ones)
	   s1=s1 + (a(m)/tau(m))*exp1(-tcrit/tau(m))
	enddo
	s2=0.0
	do m=kth+1,ncomp      !sum components above tcrit (slow ones)
	   s2=s2 + (a(m)/tau(m))*exp1(-tcrit/tau(m))
	enddo
	tcfunc3=s1-s2
	RETURN
	END

