	program testres
c To compare output of two versions of RESINT
c
	allocatable::tint1,tint2,tdiff
	allocatable::ampl1,ampl2
	allocatable::iprops1,iprops2
	real*4 tint1(:),tint2(:),ampl1(:),ampl2(:),tdiff(:)
	integer*1 iprops1(:),iprops2(:),ip1,ip2
	logical nomatch
c
	OPEN(unit=20,file='tint1.dat',status='UNKNOWN',
     &	access='TRANSPARENT')
	read(20,rec=1) nint1
c
	OPEN(unit=21,file='tint2.dat',status='UNKNOWN',
     &	access='TRANSPARENT')
	read(21,rec=1) nint2
c
	ndim=max(nint1,nint2)
	ALLOCATE(tint1(ndim),ampl1(ndim),iprops1(ndim))
	read(20,rec=1) nint1,(tint1(i),i=1,nint1),(ampl1(i),i=1,nint1),
     &	(iprops1(i),i=1,nint1)
	ALLOCATE(tint2(ndim),ampl2(ndim),iprops2(ndim))
	read(21,rec=1) nint2,(tint2(i),i=1,nint2),(ampl2(i),i=1,nint2),
     &	(iprops2(i),i=1,nint2)
	ALLOCATE(tdiff(ndim))
	CLOSE(unit=20)
	CLOSE(unit=21)
c
c Now compare the two tint() etc
	print 30
30	format(' tcrit (ms) = ')
	call INPUTr(tcrit)
	ndiff=0
	nm=0
	nexact=0
	j1=1
c
	do i=1,nint1
	   t1=tint1(i)
	   a1=ampl1(i)
	   ip1=iprops1(i)
c	   print 1,i,t1,a1,ip1
c1	   format(i6,2g13.6,i3)
c Is there a record in tint2 that matches i'th record in tint1? -if so
c print latter next to former
c
c	   j11=j1-10
c	   if(j11.lt.1) j11=1
	   j11=j1
	   j22=j1+10
	   if(j22.gt.nint2) j22=nint2
	   do j=j11,j22
		t2=tint2(j)
		a2=ampl2(j)
		ip2=iprops2(j)
		if(abs(t1-t2).le.tcrit) then
		   if(t1.eq.t2.and.a1.eq.a2.and.ip1.eq.ip2) then
			nexact=nexact+1
		   endif
c		   print 2,j,t2,a2,ip2
c2		   format(30x,i6,2g13.6,i3)
		   jlast=j
		   nomatch=.false.
		   goto 9
		else
		   nomatch=.true.
		endif
	   enddo		!end of j=j1,nint2
9	   continue
	   if(nomatch) then
	      print 20,i,t1,a1,ip1
20		format(i6,2g13.6,i3,' in tint1 has no match in tint2')
		nm=nm+1
	   else
	      print 21,i,t1,a1,ip1
21		format(i6,2g13.6,i3,' in tint1 is close to')
	      print 23,j,t2,a2,ip2,t1-t2
23		format(i6,2g13.6,i3,' in tint2: time diff = ',g13.6,/)
		ndiff=ndiff+1
		tdiff(ndiff)=t1-t2
		if(j.gt.j1) j1=j	!start point for next search through tint2
	   endif
	enddo		!end of i=1,nint1
c
	if(ndiff.gt.0) then
	   call VARV1(tdiff,ndiff,YBAR,VARY,ymin,ymax)
	   print 413,ndiff,YBAR,sqrt(vary),ymin,ymax
413	   FORMAT(/,' Mean and SD of ',I5,' values of t1-t2 = ',G13.6,
     &	' +/-',g13.6,
     & /,' Range from ',g13.6,' to ',g13.6)
	   print 32,nm
32	   format(1x,i6,' values in tint1 have no match in tint2')
	   print 33,nexact
33	   format(1x,i6,' values in tint1 have exact match in tint2')
	   pause
	endif
c
c And other way round
	do j=1,nint2
	   t2=tint2(j)
	   a2=ampl2(j)
	   ip2=iprops2(j)
c	   print 1,j,t2,a2,ip2
c1	   format(i6,2g13.6,i3)
c Is there a record in tint1 that matches j'th record in tint2? -if so
c print latter next to former
c
	   i1=1
	   do i=i1,nint1
		t1=tint1(i)
		a1=ampl1(i)
		ip1=iprops1(i)
		if(t1.eq.t2.and.a1.eq.a2.and.ip1.eq.ip2) then
c		   print 2,i,t1,a1,ip1
c2		   format(30x,i6,2g13.6,i3)
		   ilast=i
		   nomatch=.false.
		   goto 91
		else
		   nomatch=.true.
		endif
	   enddo		!end of i=i1,nint1
91	   continue
	   if(nomatch) then
	      print 22,j,t2,a2,ip2
22		format(i6,2g13.6,i3,' in tint2 has no match in tint1')
c		if(j.gt.jlast+1) then
c===
c		endif
	   else
		i1=i	!start point for next search through tint1
	   endif
	enddo		!end of j=1,nint2
	end

