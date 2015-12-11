	subroutine HYPLIK(aK,Ymax,sdK,aKlo,aKhi,Ylo,Yhi,nloK,nhiK,em,deb)
c To calculate likelihood intervals in case of fit of simple
c hyperbola (K,Ymax), in simpler way than general
c method in CVFIT (needs only bisection, not iterated simplex)
c Methods are as worked out in SSD-LOB.MCD
c   In a few bad cases, really need double precision e.g. the maximum
c on the upper side of Scrit contour (dY/dK=0) can be so near to the
c end of the contour that cannot distinguish it in single precision
c and in this case bisec0 may not converge properly (e.g. returns with
c n=4).  But in such cases answer is nearly OK so check that result is
c close to Scrit separately, after bisection, and if near enough then
c count as a valid result even if nerr is not zero.
c
	logical complex,badloK,badhiK,deb,retest,badKylo,badKyhi
	character ans*1,UC*1
	real svals(2)
	common/data1/ n,x(100),y(100),w(100)	!for hypvar/lik/func1/etc
	common/data2/ Scrit,Scrit1,Scrit2		!for hyplik/funclo/hi
	common/run/irun
	external func1,funclo,funchi
c
101	format(a1)
cc	epsy=0.001
c	epsy=0.0001
	epsy=-1.	!ignore
c	epsx=0.001
	epsx=0.0001
c For contours
	ncont=2
	svals(1)=Scrit1	!m=0.5 contour
	svals(2)=Scrit2	!m=2.0 contour
	badloK=.false.
	badhiK=.false.
c
c Likelihood intervals for K.  Find as solutions for K when disc=0 so the
c two solutions for Ymax are identical (i.e. at tangents to the scrit ellipse)
c NB important to get these quite accurately (eg epsy=0.0001) to provide
c valid range of K values to search for Ymax limits.
c Func1 returns disc, so goes negative (-> complex roots) when end of
c the Scrit contour is passed. If func1(1.e6) is still positive then upper
c limit for K must be infinite (or near as makes no difference). Likewise
c if func0(0) is still positive, start of contour must be below K=0 so
c lower limit for K must be negative.
	f=0.0
c Lower limit
	x1=0.
	x2=aK
	tdisc=func1(x1)
	if(tdisc.gt.0.) then	!lower limit for K is negative
	   print 11,irun
11	   format(' Run ',i6,' Lower limit for K is negative')
	   if(sdK.gt.0.) then
		x1=aK-5.*sdK
		if(func1(x1).gt.0.) then	!still bad
		   aKlo=-1001.0	!signal for indeterminate negative lower limit
		   nloK=nloK+1
		   badloK=.true.
		   goto 22		!otherwise do bisection with neg x1
		endif
	   endif
	endif
	ndisp=-2
	call BISEC0(FUNC1,x1,x2,f,Xout,Yout,epsx,epsy,
     & nerr,ndisp,.false.)
c	if(deb) then
c	   print 70,irun,xout,yout
c70	   format(' Run ',i5,' Klo = ',g13.6,' (disc = ',g13.6,')')
c	endif
	if(nerr.eq.0) then
	   aKlo=xout
	else
c	   Check if answer close enough despite fact that nerr not 0
	   tdisc=func1(xout)
	   if(abs(tdisc).lt.0.001) then
		aKlo=xout
	   else
		print 1,nerr,aK,Ymax,scrit, Scrit1, Scrit2
1    		format(' Error ',i2,' in lower limit for K.',/,
     &    ' Khat, Yhat, Scrit, Scrit1, Scrit2 = ',5g13.6)
		call BELL(1)
		print 13, irun,func1(0.),aK,func1(aK)
13		format(' Run ',i5,'
     &	 func1(0) = ',g13.6,' func1(',g11.4,') = ',g13.6)
		aKlo=-1001.0	!signal that failed to find lower limit
	      badloK=.true.
		pause
	   endif
	endif
22	continue	!jump here if lower limit neg
	if(deb.and.badloK) then
	   print 221
221	   format(' Draw the SSD contours [N] ? ')
	   read 101,ans
	   if(UC(ans).eq.'Y') call DRAWSSD(0.,10.*aK,ncont,Svals,n,x,y,w)
	endif
c
c Upper limit for K
	x1=aK
	if(sdK.gt.0.) then
	   x2=aK+10.*sdK
	else
	   x2=20.*aK
	endif
c Even 10*sd may not be enough to reach end of Scrit contour in extreme cases
c so increase it sufficiently to make func1=disc negative (complex roots, so
c above the end of contour) -if reach 1.e6 take upper limit as infinite
20	tdisc=func1(x2)
	if(tdisc.gt.0.) then
	   x2=x2*4.
	   if(x2.lt.1.e6) goto 20
	endif
	if(tdisc.lt.0.) then	!OK
	   x2sav=x2
	   f=0.0
	   call BISEC0(FUNC1,x1,x2,f,Xout,Yout,epsx,epsy,
     &	 nerr,ndisp,.false.)
	   if(nerr.eq.0) then
		aKhi=xout
	   else
c	   Check if answer close enough despite fact that nerr not 0
		tdisc=func1(xout)
		if(abs(tdisc).lt.0.001) then
		   aKhi=xout
		else
		   call BELL(1)
		   print 71,irun,xout,yout
71		   format(' Run ',i5,' Khi = ',g13.6,' (disc = ',g13.6,')')
		   print 2,nerr,aK,Ymax,scrit
2		   format(' Error ',i2,' in upper limit for K',/,
     &      ' Khat, Yhat, Scrit = ',3g13.6)
		   print 14,func1(aK),x2sav,func1(x2sav)
14		   format(' func1(Khat) = ',g13.6,' func1(',g13.6,') = '
     &		,g13.6)
		   aKhi=-1001.	!signal that failed to find upper limit
	         badhiK=.true.
		   print 221
		   read 101,ans
		   if(UC(ans).eq.'Y') call DRAWSSD(0.,10.*aK,ncont,Svals,
     &		n,x,y,w)
	      endif
	   endif
	else if(x2.ge.1.e6.and.tdisc.gt.0.) then
	   x2sav=x2
	   print 12,irun
12	   format(' Run ',i6,' Upper limit for K is infinite')
	   aKhi=1.e6	!signal for infinite upper limit
	   nhiK=nhiK+1
	   badhiK=.true.
	else
	   call BELL(2)
	   print 77
77	   format(' SHOULD NOT GET HERE')
	endif
c
c
c Limits for Ymax.  Look at scrit contour over the range of K values
c just found, and locate points where dY/dK is zero, say at Kyhi on
c upper limb of contour, Kylo on lower limb of countour.  Limits for
c Ymax are the values of Ymax that correspond to these points
c The limits for K are at ends of contour where dY/dK should be infinite,
c so move slightly in for init guesses, thus use aKlo*1.002 as lower init
c guess rather than aKlo, and similarly for aKhi (funclo and funchi can
c move them a bit further if nec)
c
c Lower limit for Ymax: first find corresponding K values -check only in the
c region of valid K values, aKlo to aKhi = coordinates of the ends of the contour
c -Need to check that aKlo and aKhi are OK (give disc=>0) -aK1 can be reset
c slightly in funclo, funchi if necessary, to correct rounding errors in
c aKlo, aKhi.
c Can get limits for Ymax if limits for K are bad?
c If aKl0 has negative lower limit, may nevertheless have
c legitimate lower limit for Ymax so try to find it.
	if(aKlo.lt.-1000.) then
	   Ylo=-1001.		!as signal that invalid
	   goto 21		!try high limit
	endif
	if(aKlo.gt.0.) then
	   x1=aKlo*1.001	!see above
	else
	   x1=aKlo*0.999
	endif
	x2=aK
	ndisp=-2
	f=0.0
c	test1=funclo(x1)		!for debug
c	test2=funclo(x2)
	call BISEC0(FUNClo,x1,x2,f,Xout,Yout,epsx,epsy,
     & nerr,ndisp,.false.)
	if(nerr.eq.0) then
	   aKylo=xout
	   badKylo=.false.
	else
c	   Check if answer close enough despite fact that nerr not 0
	   test=funclo(xout)
	   if(abs(test).lt.0.001) then
		aKylo=xout
		badKylo=.false.
	   else
		badKylo=.true.
		print 3,nerr,aK,Ymax,scrit
3		format(' Error ',i2,' in lower limit for Ymax',/,
     &    ' Khat, Yhat, Scrit = ',3g13.6)
		call BELL(1)
		if(deb) then
		   print 221
		   read 101,ans
		   if(UC(ans).eq.'Y') call DRAWSSD(0.,10.*aK,ncont,Svals,
     &		n,x,y,w)
		endif
	   endif
	endif
	if(badKylo) then
	   Ylo=-1001.	!invalid
	else
	   call Yvals(Ylow,Yhigh,aKylo,Scrit,complex)
	   Ylo=Ylow
	endif
c
c Upper limit for Ymax
21	if(badhiK) then
	   Yhi=aKhi		!-1001. or 1.e6 as signal
	   RETURN
	endif
	x1=aK
	if(aKhi.gt.0.) then
	   x2=aKhi*0.999	!see above
	else
	   x2=aKhi*1.001
	endif
	test2=funchi(x2)
	if(test2.gt.0.) then	!factor of 0.999 reduces K too much!
	   if(aKhi.gt.0.) then
		x2=aKhi*0.9999	!see above
	   else
		x2=aKhi*1.0001
	   endif
	endif
	x2sav=x2
	f=0.0
	call BISEC0(FUNChi,x1,x2,f,Xout,Yout,epsx,epsy,
     & nerr,ndisp,.false.)
	if(nerr.eq.0) then
	   aKyhi=xout
	   badKyhi=.false.
	else
c	   Check if answer close enough despite fact that nerr not 0
	   test=funchi(xout)
	   if(abs(test).lt.0.001) then
		aKyhi=xout
		badKyhi=.false.
	   else
		badKyhi=.true.
		print 4,nerr,aK,Ymax,scrit
4		format(' Error ',i2,' in upper limit for Ymax',/,
     &    ' Khat, Yhat, Scrit = ',3g13.6)
		call BELL(1)
c		retest=.false.
c		if(retest) then	!debug!
c75		   t1=funchi(z)
c		   goto 75
c		endif
		if(deb) then
		   print 221
		   read 101,ans
		   if(UC(ans).eq.'Y') call DRAWSSD(0.,10.*aK,ncont,Svals,
     &		n,x,y,w)
		endif
	   endif
	endif
	if(badKyhi) then
	   Yhi=-1001.	!invalid
	else
	   call Yvals(Ylow,Yhigh,aKyhi,Scrit,complex)
	   Yhi=Yhigh
	endif
	if(deb) then
	   print 72,irun,ylo,yhi
72	   format(' Run ',i5,' Ylo = ',g13.6,'; Yhi = ',g13.6)
	endif
c
	RETURN
	end

	subroutine Yvals(Ylo,Yhi,aK,Scrit,complex)
c	Returns the both roots for Ymax, for specified K and Scrit
	common/data1/ n,x(100),y(100),w(100)		!for func1/2
	logical complex
c
c  Calculate coefficients of quadratic for give K value (aK) and given Scrit value
c
	s=0.
	do i=1,n
	   s=s + w(i)*(x(i)/(x(i)+aK))**2
	enddo
	den=s
c
	s=0.
	do i=1,n
	   s=s + w(i)*(x(i)*y(i)/(x(i)+aK))
	enddo
	b=-2*s
c
	s=0.
	do i=1,n
	   s=s + w(i)*y(i)*y(i)
	enddo
c Coefficients are
	c= (s - Scrit)/den
	b=b/den
c and solutions are
	disc=b*b - 4*c
	if(disc.gt.0) then
	   Yhi=0.5*(-b+sqrt(disc))
	   Ylo=0.5*(-b-sqrt(disc))
	   complex=.false.
	else
	   complex=.true.
	endif
	RETURN
	end



	function FUNC1(aK1)
	common/data1/ n,x(100),y(100),w(100)		!for func1/2
	common/data2/ Scrit					!for func1/2
	s=0.
	do i=1,n
	   s=s + w(i)*(x(i)/(x(i)+aK1))**2
	enddo
	den=s
c
	s=0.
	do i=1,n
	   s=s + w(i)*(x(i)*y(i)/(x(i)+aK1))
	enddo
	b=-2*s
c
	s=0.
	do i=1,n
	   s=s + w(i)*y(i)*y(i)
	enddo
c Coefficients are
	c= (s - Scrit)/den
	b=b/den
c and solutions are
	disc=b*b - 4*c
	func1=disc
	RETURN
	end


	function FUNClo(aK1)
c to get point where dYlo/dK=0 on the lower limb of the S=Scrit contour
	common/data1/ n,x(100),y(100),w(100)		!for func1/2
	common/data2/ Scrit				!for func1
	common/run/irun
	logical complex
	common/bad/complex
c
	nbadmax=30
	aksav=ak1
	nbad=0
12	continue
c First get db/dK
	s=0.
	do i=1,n
	   s=s + w(i)*(x(i)/(x(i)+aK1))**2
	enddo
	den=s
	den2=den*den
c
	s=0.
	do i=1,n
	   s=s + w(i)*x(i)*y(i)/((x(i)+aK1)**2)
	enddo
	dudx=2*s
c
	s=0.
	do i=1,n
	   s=s + w(i)*x(i)*y(i)/(x(i)+aK1)
	enddo
	ux=-2*s
	bval=ux/den
c
	s=0.
	do i=1,n
	   s=s + w(i)*x(i)*x(i)/((x(i)+aK1)**3)
	enddo
	dvdx=-2*s

c
	dbdK=(dudx*den - ux*dvdx)/den2
c
c Next get dcdx
	s=0.
	do i=1,n
	   s=s + w(i)*y(i)*y(i)
	enddo
	ux=s - Scrit
	cval=ux/den
c
	dcdK=-ux*dvdx/den2
c
	disc=bval*bval - 4*cval
	if(disc.ge.0) then
	   dval=sqrt(disc)
	   complex=.false.
	else
	   complex=.true.
	   if(aK1.gt.0.) then
	      aK1=aK1*1.001	!increase a bit, until not complex!
	   else
		aK1=aK1*0.999
	   endif
	   nbad=nbad+1
	   if(nbad.lt.nbadmax) goto 12
	endif
c
c denominator (den*den), and factor of 0.5, omitted from slopes
	if(nbad.gt.0) print 1,irun,nbad,aksav,ak1
1	format(' run ',i6,' nbad = ',i3,' in funclo: K adj from ',
     & g11.4,' to ',g11.4)
	dYlodK=-(dbdK*(bval+dval) - 2.*dcdK)
	funclo=dYlodK
	RETURN
	end


	function FUNChi(aK1)
c to get point where dYhi/dK=0 on the upper limb of S=Scrit contour
	common/data1/ n,x(100),y(100),w(100)		!for func1/2
	common/data2/ Scrit				!for func1
	common/run/irun
	logical complex
	common/bad/complex
c
	aksav=ak1
	nbadmax=30
	nbad=0
12	continue
c First get db/dK
	s=0.
	do i=1,n
	   s=s + w(i)*(x(i)/(x(i)+aK1))**2
	enddo
	den=s
	den2=den*den
c
	s=0.
	do i=1,n
	   s=s + w(i)*x(i)*y(i)/((x(i)+aK1)**2)
	enddo
	dudx=2*s
c
	s=0.
	do i=1,n
	   s=s + w(i)*x(i)*y(i)/(x(i)+aK1)
	enddo
	ux=-2*s
	bval=ux/den
c
	s=0.
	do i=1,n
	   s=s + w(i)*x(i)*x(i)/((x(i)+aK1)**3)
	enddo
	dvdx=-2*s

c
	dbdK=(dudx*den - ux*dvdx)/den2
c
c Next get dcdx
	s=0.
	do i=1,n
	   s=s + w(i)*y(i)*y(i)
	enddo
	ux=s - Scrit
	cval=ux/den
c
	dcdK=-ux*dvdx/den2
c
	disc=bval*bval - 4*cval
	if(disc.ge.0) then
	   dval=sqrt(disc)
	   complex=.false.
	else
	   complex=.true.
	   if(aK1.gt.0.) then
	      aK1=aK1*0.999	!decrease a bit, until not complex!
	   else
		aK1=aK1*1.001
	   endif
	   nbad=nbad+1
	   if(nbad.le.nbadmax) goto 12
	endif
c
	if(nbad.gt.0) print 1,irun,nbad,aksav,ak1
1	format(' run ',i6,' nbad = ',i3,' in funchi: K adj from ',
     & g11.4,' to ',g11.4)
c denominator (den*den), and factor of 0.5, omitted from slopes
	dYhidK=dbdK*(bval-dval) - 2.*dcdK
	funchi=dYhidK
	RETURN
	end


