	program TTC2
	integer*2 iampl(1024)
	integer*1 iprops(1024)	!properties of ith interval (see SCAN)
	integer kfix(0:50)
	real*4 delt(0:50)		!temp storage for transition search results
	real*4 deltsav(0:50)		!temp storage for transition search results
c
	ntrans=6
c=	ntrans=4
c=	ntrans=3
	delt(0)=5000.	!amp=0
	delt(1)=1000.	!amp=-500
	delt(2)=10.		!amp=0
	delt(3)=2000.	!amp=-520
	delt(4)=200.	!amp=0
	delt(5)=8.		!amp=-540
c
	kt=20
c
	iampl(20)=0

c	iampl(21)=-500
	iampl(21)=0

	iampl(22)=0
c	iampl(22)=-510

c	iampl(23)=-520
	iampl(23)=0

	iampl(24)=0

	iampl(25)=-540
c	iampl(25)=0

	iampl(26)=0
c	iampl(26)=-530

c
	iprops(20)=0
	iprops(21)=0
	iprops(22)=0
	iprops(23)=0
	iprops(24)=0
	iprops(25)=0
	iprops(26)=0
c
	print 1,ntrans
1	format(' ntrans = ',i5)
	do i=0,ntrans
	   k=kt+i
	   if(i.lt.ntrans) then
	      print 2,i,delt(i),kfix(i),k,iampl(k),iprops(k)
2	      format(
     &	' delt(',i2,'),kfix = ',f9.1,i4,
     &	  '; k, amp(k), props(k) = ',i3,2i6)
	   else
	      print 3,k,iampl(k),iprops(k)
3	      format(30x,'  k, amp(k), props(k) = ',i3,2i6)
	   endif
	enddo
c
	d0sav=delt(0)
	ntsav=ntrans
c
	call TC2(kt,ntrans,iampl,iprops,delt,told,kfix)
c
	print 1,ntrans
c1	format(' ntrans = ',i5)
	do i=0,ntrans
	   k=kt+i
	   if(i.lt.ntrans) then
	      print 2,i,delt(i),kfix(i),k,iampl(k),iprops(k)
c2	      format(
c     &	' delt(',i2,') = ',f9.1,'; k, amp(k), props(k) = ',i3,2i6)
	   else
	      print 3,k,iampl(k),iprops(k)
c3	      format(21x,'  k, amp(k), props(k) = ',i3,2i6)
	   endif
	enddo
c
	if(ntrans.lt.ntsav) then
	   print 4,delt(0)-d0sav,delt(ntrans)
4	   format(' Length to be added to preceding shut time = ',f9.1,/,
     &   ' Length to be added to following shut time = ',f9.1)
	endif
	end

	subroutine TC2(kt,ntrans,iampl,iprops,delt,told,kfix)
c Test concatenation of zero amplitudes
	integer*2 iampl(1024)
	integer*1 iprops(1024)	!properties of ith interval (see SCAN)
	integer kfix(0:50)
	real*4 delt(0:50)		!temp storage for transition search results
c	logical bad,diffamp
c
c	bad=.false.
	nt=ntrans
	i=0		!index for delt()=0,1,...
	i1=0		!index for concatenated delt()=0,1,...
	k=kt		!index for iampl(), iprops()
c Need to set delt(ntrans)=0 so that 'length to be added to following gap'
c is zero in case where no zero amps are concatenated at the end of the fitting
	delt(ntrans)=0.0
c
1	if(iampl(k).eq.0.and.iampl(k+1).eq.0) then
	   icflag=1				!concatenation has occured
	   delt(i1)=delt(i1)+delt(i+1)
	   kfix(i1)=1			!only if setshut true?
	   i=i+1
         k=k+1
	   nt=nt-1
	else
	   icflag=0				!concatenation has ended
	   i=i+1
         k=k+1
	   i1=i1+1		!start new concat group
	   delt(i1)=delt(i)
	endif
	iampl(i1+kt)=iampl(k)
	iprops(i1+kt)=iprops(k)
	if(i.lt.ntrans) goto 1      !goes up to iampl(kt+ntrans)

c NOT YET DONE!
c NB -if zero values concat at STAR of fitting then length of gap BEFORE
c  the fitting, tint(kt) must be lengthened.

c NB -If zero values are concat at the end of the fitting then TOLD must be
c moved earlier (to the end of the last non=zero amplitude) so gap following
c the fitting is correct
	ntrans=nt
	RETURN
	end

