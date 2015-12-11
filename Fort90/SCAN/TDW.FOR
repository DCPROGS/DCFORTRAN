	real*4 tint(1024)
	integer*2 iampl(1024)
	integer*1 iprops(1024)	!properties of ith interval (see above)
	character expdate*11
c
	ntmax=1024
	kt=10
	do i=1,10
	   tint(i)=2.*float(i)
	   iampl(i)=i
	   iprops(i)=-i
	enddo
	ffilt=2.5
	rms=0.213
	avamp=-1610.
	calfac2=0.00123
	expdate='25-Dec-93'
	iscanver=100
	call DSKWRIT(tint,iampl,iprops,kt,ntmax,ffilt,rms,avamp,
     & calfac2,expdate,iscanver)
	end

