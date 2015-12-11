	program TGUESS
c To check CHKGUESS
c
	real*4 delt(0:50),ylevel(50)
	integer kfix(0:50)
	logical opening(50),shutprev
c
	scrit=100.
	iampcrit=150
	base=300.
c	nt=5
	nt=8
c	shutprev=.true.
c	ylevel(1)=base	!init shut
c
	shutprev=.false.
	ylevel(1)=-690.	!init open
	prevlevel=-690.
c
	ylevel(2)=-700.
	ylevel(3)=-750.
	ylevel(4)=-800.
	ylevel(5)=-750.
	ylevel(6)=base
	ylevel(7)=-700
	ylevel(8)=-800
	ylevel(9)=base
	opening(1)=.true.
	opening(2)=.true.
	opening(3)=.true.
	opening(4)=.false.
	opening(5)=.false.
	opening(6)=.true.
	opening(7)=.true.
	opening(8)=.false.
	t0=1000.
	delt(1)=150.
	delt(2)=200.
	delt(3)=300.
	delt(4)=400.
	delt(5)=1200.
	delt(6)=400.
	delt(7)=100.
	kfix(0)=0
	kfix(1)=1
	kfix(2)=2
	kfix(3)=3
	kfix(4)=4
	kfix(5)=5
	kfix(6)=6
	kfix(7)=7
	kfix(8)=8
c
	print 1,nt,t0
c1	format(' ntrans = ',i5,'; t0 = ',f8.2,/,
c     &'  i    delt(i)   ylevel(i)   opening(i)  kfix(i)')
	do i=0,nt+1
	   if(i.eq.0) then
		print 2,i,delt(0),kfix(0)
c2		format(i3,3x,f8.2,27x,i3)
	   endif
	   if(i.ge.1.and.i.le.nt-1) then
		print 3,i,delt(i),ylevel(i),opening(i),kfix(i)
c3		format(i3,3x,f8.2,3x,f8.2,4x,l6,6x,i3)
	   endif
	   if(i.eq.nt) then
		print 4,i,ylevel(i),opening(i),kfix(i)
c4		format(i3,11x,3x,f8.2,4x,l6,6x,i3)
	   endif
	   if(i.eq.nt+1) then
		print 5,i,ylevel(i)
c5		format(i3,11x,3x,f8.2)
	   endif
	enddo
c
	call CHKGUESS(nt,t0,delt,ylevel,kfix,opening,scrit,
     & base,iampcrit,shutprev,prevlevel)
c
	print 1,nt,t0
1	format(' ntrans = ',i5,'; t0 = ',f8.2,/,
     &'  i    delt(i)   ylevel(i)   opening(i)  kfix(i)')
	do i=0,nt+1
	   if(i.eq.0) then
		print 2,i,delt(0),kfix(0)
2		format(i3,3x,f8.2,27x,i3)
	   endif
	   if(i.ge.1.and.i.le.nt-1) then
		print 3,i,delt(i),ylevel(i),opening(i),kfix(i)
3		format(i3,3x,f8.2,3x,f8.2,4x,l6,6x,i3)
	   endif
	   if(i.eq.nt) then
		print 4,i,ylevel(i),opening(i),kfix(i)
4		format(i3,11x,3x,f8.2,4x,l6,6x,i3)
	   endif
	   if(i.eq.nt+1) then
		print 5,i,ylevel(i)
5		format(i3,11x,3x,f8.2)
	   endif
	enddo
c
	end
