	subroutine RANCOMP(yval0,nyval,j1,j2,thist,bhist,obhist,idtype)
c To do randomisation test comparison between values in Yval(i,j1) and those
c in Yval(i,j2), in EKDIST
c
c  Raw data for distribution =YVAL in main prog. for use in randomisation test
	real*4 YVAL0(81920,10)
	integer nyval(10)
	allocatable::allobs,iran,randiff
	real*4 allobs(:),randiff(:)
	integer iran(:)
	logical thist,bhist,obhist
	logical repeat,censor
	character*1 ans
c	logical show
c
	common/rand/ix,iy,iz
	logical debug,caplock,deb
	logical discprt
	common/dp/discprt
c
c define function
	debug()=caplock()

c===need to know what sort of data
c===ask whether to use e.g. only bursts > t ms
c Find smaller sample and calculate observed difference
	if(nyval(j2).gt.nyval(j1)) then	!make n2 the smaller
	   j=j2		!swap j1,j2
	   j2=j1
	   j1=j
	endif
	n1=nyval(j1)
	n2=nyval(j2)
	ntot=n1+n2
	call VARV1(Yval0(1,j1),n1,ybar1,vary1,ymin1,ymax1)
	call VARV1(Yval0(1,j2),n2,ybar2,vary2,ymin2,ymax2)
	dobs=ybar1-ybar2
	if(vary1.ge.0.) sd1=sqrt(vary1)
	if(vary2.ge.0.) sd2=sqrt(vary2)
	print 16,j1,j2,j1,n1,ybar1,sd1,ymin1,ymax1,
     &  j2,n2,ybar2,sd2,ymin2,ymax2,dobs
	if(discprt)write(8,16) j1,j2,
     &  j1,n1,ybar1,sd1,ymin1,ymax1,
     &  j2,n2,ybar2,sd2,ymin2,ymax2,dobs
	ans='Y'
	call DCASK('Use all the values',ans,ans)
	censor=.false.
	if(ans.eq.'N') then
	   censor=.true.
	   if(bhist) then
		if(idtype.le.2) then
		   print 1
1		   format(' Use only values between y1, y2 openings/burst')
		else
		   print 2
2		   format(' Use only durations between y1, y2 ms')
		endif
	   else if(thist) then
		print 3
3		format(' Use only durations between y1, y2 ms')
	   endif
	   print 4,y1,y2
4	   format(' Specify y1, y2 [',f8.3,',',f8.3,'] = ')
	   call INPUT2r(y1,y2)
	   n=0
	   i=1
	   do while(i.le.n1)
		y=yval0(i,j1)
		if(y.ge.y1.and.i.le.y2) then
		   n=n+1
		   yval0(n,j1)=y
		   i=i+1
		else
		   i=i+1
		endif
	   enddo
	   n1=n
c
	   n=0
	   i=1
	   do while(i.le.n2)
		y=yval0(i,j2)
		if(y.ge.y1.and.i.le.y2) then
		   n=n+1
		   yval0(n,j2)=y
		   i=i+1
		else
		   i=i+1
		endif
	   enddo
	   n2=n
c
	   ntot=n1+n2
	   call VARV1(Yval0(1,j1),n1,ybar1,vary1,ymin1,ymax1)
	   call VARV1(Yval0(1,j2),n2,ybar2,vary2,ymin2,ymax2)
	   if(vary1.ge.0.) sd1=sqrt(vary1)
	   if(vary2.ge.0.) sd2=sqrt(vary2)
	endif
c
	dobs=ybar1-ybar2
	adobs=abs(dobs)
c
c Put all obs into one array for unpaired test
	ALLOCATE(allobs(ntot),iran(ntot))
	k=0
	stot=0.0
	do i=1,n1
	   k=k+1
	   x=yval0(i,j1)
	   stot=stot+x
	   ALLOBS(k)=x
      enddo
	do i=1,n2
	   k=k+1
	   x=yval0(i,j2)
	   stot=stot+x
	   ALLOBS(k)=x
	enddo
	if(k.ne.ntot) call BELL(10)
c
c Read (iwrite=0), or write (iwrite=1) seeds for RANDOM
	call RANDSK(ix,iy,iz,0,repeat)		!read IX,IY,IZ
c
	nran=10000
	print 19,nran
19	format(' Number of randomisations to be done [',i6,'] = ')
	call INPUTi(nran)
	ALLOCATE(randiff(nran))
c	ans='N'
c	call DCASK('Type some of the samples',ans,ans)
c	show=ans.eq.'Y'
c	if(show) then
c	   print 193
c193	   format('&Type samples of difference is between d1,d2= ')
c	   call INPUT2r(d11,d12)
c	endif
c
	en1=float(n1)
	en2=float(n2)
	ng1=0
	nl1=0
	na1=0
	ne1=0
	ne2=0
c
	do n=1,nran
	   if(mod(n-1,1000).eq.0) then
		deb=debug()
		call GETPOS(irow,icol)
		call LOCATE(irow,0)
		print 17,n
17		format('&randomising . . .',i7)
	   endif
c Choose N2 random numbers in range 0-ntot (will be bit faster if Yobs
c is the smaller sample). The LAST n2 elements contain values after call
	   call RANSAMP(n2,ntot,iran)
	   s2=0.0
	   do i=1,n2
		j=ntot-i+1
		s2=s2 + allobs(iran(j))
		if(deb) print 211,j,iran(j),allobs(iran(j))
211		format(' j,iran(j),allobs(iran(j))= ',2i5,g13.6)
	   enddo
	   yb2=s2
	   s1=stot-s2
	   yb1=s1/en1        !mean
	   yb2=s2/en2        !mean
	   dran=yb1-yb2
	   randiff(n)=dran
	   if(dran.ge.dobs) ng1=ng1+1
	   if(dran.le.dobs) nl1=nl1+1
	   if(abs(dran).ge.adobs) na1=na1+1
	   if(dran.eq.dobs) ne1=ne1+1
	   if(abs(dran).eq.adobs) ne2=ne2+1
c	   if(show) then
c		if(dran.gt.d11.and.dran.lt.d12) then
c		   do i=1,ny
c			j=ntot-i+1
c			ysamp(i)=allobs(iran(j))
c	         enddo
c		   k=0
c		   do i=1,nx
c		   xsamp(i)=allobs(iran(i))
c		   enddo
c		   call SHOWSET(xsamp,ysamp,nx,ny,nmax,
c     &		binomial,is1,if1,is2,if2,pon(),discprt)
c		   print 197,xb1,yb1,dran
c197	         format(' Means  ',g13.6,2x,g13.6,':  diff = ',g13.6)
c		endif
c	   endif
	   if(deb) print 24,yb1,yb2,dran,ng1,nl1,na1
24	   format(' yb1,yb2,dran,ng1,nl1,na1=',/,3g13.6,3i8)
	enddo		!end of loop for random samples
c
	pg1=float(ng1)/float(nran)
	pl1=float(nl1)/float(nran)
	pe1=float(ne1)/float(nran)
	pa1=float(na1)/float(nran)
	pe2=float(ne2)/float(nran)
c
	print 16,j1,j2,j1,n1,ybar1,sd1,ymin1,ymax1,
     &  j2,n2,ybar2,sd2,ymin2,ymax2,dobs
	if(discprt)write(8,16) j1,j2,
     &  j1,n1,ybar1,sd1,ymin1,ymax1,
     &  j2,n2,ybar2,sd2,ymin2,ymax2,dobs
16	format(/,' RANDOMISATION TEST',/,
     & ' Comparison of set ',i2,' and set ',i2,/,
     & ' SET ',i2,/,
     &  '  Mean and SD of ',i5,' values= ',G13.6,' +/-',g13.6,
     & /,'    (Range from ',g13.6,' to ',g13.6,')',/,
     & ' SET ',i2,/,
     &  '  Mean and SD of ',i5,' values= ',G13.6,' +/-',g13.6,
     & /,'    (Range from ',g13.6,' to ',g13.6,')',/,
     & ' Observed difference between means= ',g13.6)
	if(censor) then
	   print 5,y1,y2
	   if(discprt) write(8,5) y1,y2
5	   format(/,' Only values between ',g13.6,' and ',
     &	g13.6,' used for test',/)
	endif
	print 25,nran,pg1,pl1,pa1,ne1,pe1,ne2,pe2
	if(discprt) write(8,25) nran,pg1,pl1,pa1,ne1,pe1,ne2,pe2
25	      format(/,
     &       1x,i8,' randomisations',/,
     &       ' P values for difference between means are:',/,
     &       '  greater than or equal to observed: P = ',g13.6,/,
     &       '  less than or equal to observed: P = ',g13.6,/,
     &     '  greater than or equal in absolute value to observed: P = '
     &	 ,g13.6,/,/,
     &       ' Number equal to observed = ',i6,' (P = ',g13.6,')',/,
     &       ' Number equal in absolute value to observed = ',i6,
     &       ' (P = ',g13.6,')',/)
c
	sval=dobs
	call RANDISP(randiff,nran,sval)
c
	call RANDSK(ix,iy,iz,1,repeat)		!write IX,IY,IZ
	DEALLOCATE(allobs,iran,randiff)
	RETURN
	end


