	subroutine ACRITS(amean,area,sd,ncomp,kth,noprint)
c ACRITS is version of ACGAU that uses ALL components of the amplitude
c distribution (defined in amean,area,ncomp) -needs to be solved
c numerically if there are more than two components I think -quadratic
c no longer works

c Assumes that amean, area, sd have been sorted into ascending order of mean
c values (i.e. smallest is first . . .).
c  If 1<kth<ncomp-1 then calculates acrit for
c	dividing kth from kth+1 component
c  If kth.le.0 then calculates all ncomp-1 acrits
c  If NOPRINT=true, then does no printing at all
c
	real*4 amean(10),area(10),sd(10)
c	character*1 ans,UC
	logical discprt,pon,slock,noprint
	real*4 errmax(10)
	allocatable::ac
	real*4 ac(:,:)
	common/dp/discprt
	COMMON/ABLK/ am(10),a(10),sd1(10),ncomp1,kth1
	EXTERNAL ACFUNC1,ACFUNC2,ACFUNC3
c
	pon()=slock()
C
c
	ndisp=-1	!NO print
c101	format(a1)
	eps=1.e-5
	epsy=-1.
	allocate(ac(10,3))
c
10	continue
	if(ncomp.le.0.and.(.not.noprint)) then
	   print 1
1	   format(' Number of components in amplitude distribution = ')
	   call INPUTi(ncomp)
	   sa=0.0
	   do m=1,ncomp
		print 2,m
2		format(' mean(',i2,') = ')
		call INPUTr(amean(m))
		print 201,m
201		format('& SD(',i2,') = ')
		call INPUTr(sd(m))
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
	call SORT4r(amean,area,sd,ncomp,10,.true.)
c
c Make copies for common
	do i=1,ncomp
	   am(i)=amean(i)
	   a(i)=area(i)
	   sd1(i)=sd(i)
	enddo
	ncomp1=ncomp
	kth1=kth
c
	if(kth.le.0) then		!get all acrits
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
     & ' CRITICAL AMPLITUDE BETWEEN COMPONENTS ',i2,' AND ',i2,/)
	   errmax(k)=0.0
c
	   do np=1,3		!3 methods
		if(np.eq.1) then
		   x1=amean(k)		!init guess
		   x2=amean(k+1)		!init guess
		   call BISEC0(ACFUNC1,x1,x2,0.0,Acrit,yout,eps,epsy,
     &		nerr,ndisp,.false.)
		   ac(k,np)=acrit
		   if(noprint) goto 7
	         print 52
      	   if(pon()) write(7,52)
	         if(discprt) write(8,52)
52		   format(' Equal % misclassified')
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
		   call BISEC0(ACFUNC2,x1,x2,0.0,Acrit,yout,eps,epsy,
     &		nerr,ndisp,.false.)
		   ac(k,np)=acrit
		   if(noprint) goto 7
	         print 53
      	   if(pon()) write(7,53)
	         if(discprt) write(8,53)
53		   FORMAT(/,
     & ' Equal numbers misclassified')
		   if(nerr.ne.0) then
		      print 821
		      if(pon()) write(7,821)
		      if(discprt) write(8,821)
			goto 99	!next calculation type
		   endif
		else if(np.eq.3) then
		   x1=amean(k)		!init guess
		   x2=amean(k+1)		!init guess
		   call BISEC0(ACFUNC3,x1,x2,0.0,Acrit,yout,eps,epsy,
     &		nerr,ndisp,.false.)
		   ac(k,np)=acrit
		   if(noprint) goto 7
	         print 54
      	   if(pon()) write(7,54)
	         if(discprt) write(8,54)
54		   FORMAT(/,
     & ' Minimum total number misclassified')
		   if(nerr.ne.0) then
		      print 821
		      if(pon()) write(7,821)
		      if(discprt) write(8,821)
			goto 99	!next calculation type
		   endif
		endif
7		continue
	      print 6,acrit
      	if(pon()) write(7,6) acrit
	      if(discprt) write(8,6) acrit
6		format( ' Acrit = ',g11.4)
c Number of small amps misclassified
		en1=0.0
     		s1=0.0			!# of 'small' events
		do m=1,k			!sum components below Acrit (small ones)
		   u=(acrit-amean(m))/sd(m)
		   y=erfs(u/sqrt(2.))
		   en1=en1 + area(m)*0.5*(1.- y)
		   s1=s1+area(m)
		enddo
		p1=en1/s1		!fraction misclassified
		en1=float(ntot)*en1
c
c Number of large amps misclassified
		en2=0.0
		s2=0.0
		do m=k+1,ncomp      !sum components above acrit (large ones)
		   u=(acrit-amean(m))/sd(m)
		   y=erfs(u/sqrt(2.))
		   en2=en2 + area(m)*0.5*(1.+ y)
		   s2=s2+area(m)
		enddo
		p2=en2/s2		!fraction misclassified
		en2=float(ntot)*en2
	      print 30,100.*p1,100.*p2
      	if(pon()) write(7,30) 100.*p1,100.*p2
	      if(discprt) write(8,30) 100.*p1,100.*p2
30		FORMAT( ' % small misclassified= ',g11.4,
     &	   ' % big misclassified= ',g11.4)
c
	      print 31,en1,en2
	      if(pon()) write(7,31) en1,en2
      	if(discprt) write(8,31) en1,en2
31		format( ' # small misclassified= ',G11.4,
     &	' # big misclassified= ',G11.4)
		print 32,en1+en2
      	if(pon()) write(7,32) en1+en2
	      if(discprt) write(8,32) en1+en2
32		format( ' Total # misclassified (out of 100)= ',G11.4)
		if(en1+en2.gt.errmax(k)) errmax(k)=en1+en2
c
99		continue
	   enddo	!end of np=1,2,3
	enddo
c
	print 20
20	format(' Summary:',/,
     & '  Components             Acrit (by 3 criteria)        '
     & 'Max total misclassif')
	do k=n1,n2
	   print 21,k,k+1,(ac(k,np),np=1,3),errmax(k)
21	   format(1x,i3,' to ',i3, 4(2x,g13.6))
	enddo
	deallocate(ac)
c	print 120
c120	FORMAT(' Another Acrit calculation [N] ? ')
c	read 101,ans
c	if(UC(ans).eq.'Y') GOTO 10
C
	RETURN
	END


	function ACFUNC1(Acrit)
	COMMON/ABLK/ amean(10),area(10),sd(10),ncomp,kth
c C and S method -equal fraction
c
c Number of small amps misclassified
	en1=0.0
	s1=0.0			!# of 'small' events
	do m=1,kth			!sum components below Acrit (small ones)
	   u=(acrit-amean(m))/sd(m)
	   y=erfs(u/sqrt(2.))
	   en1=en1 + area(m)*0.5*(1.- y)
	   s1=s1+area(m)
	enddo
	p1=en1/s1		!fraction misclassified
c
c Number of large amps misclassified
	en2=0.0
	s2=0.0
	do m=kth+1,ncomp      !sum components above acrit (large ones)
	   u=(acrit-amean(m))/sd(m)
	   y=erfs(u/sqrt(2.))
	   en2=en2 + area(m)*0.5*(1.+ y)
	   s2=s2+area(m)
	enddo
	p2=en2/s2		!fraction misclassified
c
	acfunc1=p1-p2
	RETURN
	END

	function ACFUNC2(Acrit)
	COMMON/ABLK/ amean(10),area(10),sd(10),ncomp,kth
c Clapham and Neher Method -equal number misclassif
c Number of small amps misclassified
	en1=0.0
	do m=1,kth			!sum components below Acrit (small ones)
	   u=(acrit-amean(m))/sd(m)
	   y=erfs(u/sqrt(2.))
	   en1=en1 + area(m)*0.5*(1.- y)
	enddo
c Number of large amps misclassified
	en2=0.0
	do m=kth+1,ncomp      !sum components above acrit (large ones)
	   u=(acrit-amean(m))/sd(m)
	   y=erfs(u/sqrt(2.))
	   en2=en2 + area(m)*0.5*(1.+ y)
	enddo
c
	acfunc2=en1-en2
	RETURN
	END



	function ACFUNC3(Acrit)
	COMMON/ABLK/ amean(10),area(10),sd(10),ncomp,kth
c 1st deriv of number of small  misclassified
	s1=0.0
	do m=1,kth			!sum components below Acrit (small ones)
	   u=(acrit-amean(m))/sd(m)
	   s1=s1 + (area(m)/sd(m))*exp(-0.5*u*u)
	enddo
c 1st deriv of number of large amps misclassified
	s2=0.0
	do m=kth+1,ncomp      !sum components above acrit (large ones)
	   u=(acrit-amean(m))/sd(m)
	   s2=s2 - (area(m)/sd(m))*exp(-0.5*u*u)
	enddo
	acfunc3=s1+s2
	RETURN
	END

