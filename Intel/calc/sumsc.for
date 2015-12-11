	subroutine SUMSC(tint1,iamp1,n1,tint2,iamp2,n2,nomit,
     &	ndim1,ndim2,prt)
c To add two simulated channels (based on test prog TSUM)
c Result in tint1,iamp1,n1
c
c NB: n2 must not be greater than ndim
c Present version has tint1, tint2 both same size=ndim, as allocated in
c calling prog (SIMSC), but tcum, icum (allocated locally) are twice this size)
c
c NB called with tint(), tint2() in place of tint1,tint2 in parameters
c
c Modif 12/01/97 07:20am by adding parameter nomit
c  if called the nomit=1 zero length intervals are omitted from output
c  if called the nomit=0 zero length intervals are NOT omitted from output
c  returns nomit=number of zero length intervals (whether omitted or not)
c  (NB zero length intervals are always removed from end of sum and
c	are not counted by nomit)
c
c (but in last version for old compiler both of these were declared
c as 40960=40*1024 so dimensions here were wrong!)
c
c OLD NOTES:
c    NB all dimensions x40 12/22/89 05:00pm
c Note that TINT2,IAMP2 are 5120=ndim1 for convenience of use in SIMSC but
c n2 must not be greater than 1024=ndim
c	real*4 tint1(1024),tint2(5120),tcum(2048)
c	integer*2 iamp1(1024),iamp2(5120)	!5120*40=204800
c	integer icum(2048)			!1024*40=40960=ndim; 2048*40=81920
c NB in last version for old compiler ndim1=4*ndim=4*1024*40=163840
	real*4 tint1(ndim1),tint2(ndim2)
	integer*2 iamp1(ndim1),iamp2(ndim2)
	allocatable::tcum,icum
	real*4 tcum(:)
	integer icum(:)
	logical pon,debug,omit0,prt
	
	logical discprt
	common/dp/discprt
c
	pon=.false.
	debug=.false.
c
	omit0=nomit.eq.1
	nomit=0
	kdim=ndim1+ndim2		!dimension of TCUM,iCUM
	ALLOCATE(tcum(kdim),icum(kdim))
c
	if(n1.gt.ndim1) n1=ndim1
	if(n2.gt.ndim2) n2=ndim2
c Initialise
	do i=1,kdim
	  tcum(i)=99.
	  icum(i)=-99
	enddo
c test data
c
c	if(.not.debug) goto 20
c	do 12 i=1,n1
c	if(pon) write(7,4)i,tint1(i),iamp1(i)
c      if(discprt) write(7,4)i,tint1(i),iamp1(i)
c12	print 4,i,tint1(i),iamp1(i)
c	if(pon) print 11
c	print 11
c	do 13 i=1,n2
c	if(pon) write(7,4)i,tint2(i),iamp2(i)
c      if(discprt) write(7,4)i,tint2(i),iamp2(i)
c13	print 4,i,tint2(i),iamp2(i)
c	print 11
c	if(pon) write(7,11)
c	if(discprt) write(7,11)
c20	continue
c
c Put cumulative times into one array, TCUM. Make parallel array of
c amps such that iCUM(i) is amplitude of chan AFTER time TCUM(i) from
c start of record (so AMP(1) not used here- see below)
c	if(prt) print 30
30	format(' Sorting cumulative durations....')
	s=0.
	do i=1,n1
	   s=s+tint1(i)
	   tcum(i)=s
	   if(i.lt.n1) icum(i)=int4(iamp1(i+1))
	enddo
c for channel #2 make TCUM negative to distinguish values from chan #1
	s=0.
	do i=1,n2
	   s=s-tint2(i)
	   tcum(n1+i)=s
	   if(i.lt.n2) icum(n1+i)=int4(iamp2(i+1))
	enddo
c Sort into ascending order of abs values of cumulative times
	N=n1+n2
	call SORT3(tcum,icum,N,Kdim,.true.,.true.)
c
c	do 3 i=1,N
c	if(debug.and.pon) write(7,4)i,tcum(i),icum(i)
c      if(debug.and.discprt) write(7,4)i,tcum(i),icum(i)
c	if(debug) print 4,i,tcum(i),icum(i)
c4	format(i3,4x,g13.6,4x,i8)
c3	continue
c Make new values for summed record
c NB summed record must end at the end of the last interval in the
c shorter record
c	tmax=tcum(n1)
c	if(tcum(n2).lt.tcum(n1)) tmax=tcum(n2)
c
c	if(prt) print 31
31	format(' Adding channel records....')
c
c Set first interval separately
	i1=iamp1(1)
	i2=iamp2(1)
	tint1(1)=abs(tcum(1))
	iamp1(1)=i1+i2
c	print 41,1,tint1(1),iamp1(1),i1,i2
c	if(pon) write(7,41)1,tint1(1),iamp1(1),i1,i2
c      if(discprt) write(7,41)1,tint1(1),iamp1(1),i1,i2
c41	format(i3,2x,g9.2,2x,i8,6x,' = (',i8,' + ',i8,')')
c
	j=0	!index for summed record
	j1=1
7	continue	!return here for next
	j=j+1	!=1,2,...
	if(j1.eq.ndim1) goto 9	!finish
	j1=j1+1
c if a transition has occurred in channel #1, reset a1 to its new amp
	if(tcum(j).gt.0.) then
	   i1=icum(j)	!transition in chan #1
c reset i2 similarly for chan #2
	else
	   i2=icum(j)	!transition in chan #2
	endif
	if(i1.eq.-99.or.i2.eq.-99) goto 9	!finished
	tint1(j1)=abs(tcum(j+1))-abs(tcum(j))
	iamp1(j1)=i1+i2
c	print 10,i1,i2
c10	format(2i8)
c omit zero length intervals (occur if have simultaneous transitions
c in both channels)
c	if(tint1(j1).gt.0.) goto 15
c	j1=j1-1		!omit zero value from final arrays
c	goto 7		!no print
c15	continue
c	if(debug) print 41,j1,tint1(j1),iamp1(j1),i1,i2
c	if(debug.and.pon) write(7,41)j1,tint1(j1),iamp1(j1),i1,i2
c      if(debug.and.discprt) write(7,41)j1,tint1(j1),iamp1(j1),i1,i2
	goto 7
c
9	continue
	n1=j1		!no of values in summed record
c Check for zero length intervals at the end
	do i=n1,1,-1
	   if(tint1(i).gt.1.e-10) then
		n11=i
		goto 5
	   endif
	enddo
5	n1=n11	!new value
c
c If omit0 then check for zero length intervals not at the end
	if(omit0) then
	   i=1
8	   if(tint1(i).lt.1.e-10) then
		nomit=nomit+1
		do j=i+1,n1
		   tint1(j-1)=tint1(j)
		   iamp1(j-1)=iamp1(j)
		enddo
		n1=n1-1
	   endif
	   i=i+1
	   if(i.le.n1) goto 8
	endif

	DEALLOCATE(tcum,icum)
	RETURN
	end


