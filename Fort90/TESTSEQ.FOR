	program testseq
c====================================================================
c	If you run the program the first time the user will be asked to
c	make sure if the setting is in the rest position (i.e: middle-
c	for a 5 pipe device= position 3 -so pipe 3 on cell
c	if not , the user will have to enter the position

c====================================================================

	integer*4 ivalve(100,10),test(100),itpipe(100),impuls(1000)

C=======================================================================
	print*,'How many solutions = '
	read*,ntest
	do i=1,ntest
		print*,'Enter solution (in order of testing) = '
		read*,test(i)
	enddo
	print*,'Starting position for pipe = '
	read*,ipipei

C	ivalve(k,i) gives the each series of npipes valves open at a time
c	i.e.:
c	for a sequence of test solutions s25,s2,s9:
c
c     v1  v7  v13 v19 v30
c     v3  v7  v13 v19 v25
c     v1  v11 v13 v19 v25
c
C=======================================================================

	npipes=5
	nman=npipes          ! manifolds
	ntubes=30            ! total nr of tubes
	itubeman=ntubes/nman ! tubes per manifold
	irest0=(npipes-1)/2+1  !! REST POSITION IN THE MIDDLE
	irest=irest0
	L=1
C-----------------------------------------------------------------
C    CHECK FIRST IF YOU ARE IN START POSION
	do while(ipipei.ne.irest0)
	   if(ipipei.lt.irest0) then   	! move left
	   impuls(l)=128
ccc	   str(L)='DIGTIM,A,MASK,PIN1,DT'
		L=L+1
		ipipei=ipipei+1
	   else if(ipipei.gt.irest0) then ! move right
	   impuls(l)=64
ccc	   str(L)='DIGTIM,A,MASK,PIN14,DT'
		ipipei=ipipei-1
		L=L+1
	   endif
	enddo
C-----------------------------------------------------------------


C-----------------------------------------------------------------
C	FIND VALVE OPEN AT A TIME AND PIPE ON CELL
C-----------------------------------------------------------------
C	NOW IN START POSITION: ALL CONTROLS ,PIPE IN THE MIDDLE
	do i=1,npipes
	   ivalve(1,i)=1+itubeman*(i-1)
	enddo
	itpipe(1)=irest
C----------------------------------------------------------------
	do k=2,ntest+1
	   itpipe(k)=1+(test(k-1)-1)/npipes
	   do i=1,npipes
		if(i.eq.itpipe(k)) then
c		   ivalve(k,i)=1+itubeman*(itpipe(k)-1)
		   ivalve(k,i)=(itubeman*test(k-1)+4)/npipes
		else
		   ivalve(k,i)=1+itubeman*(i-1)
		endif
	   enddo
	enddo
C----------------------------------------------------------------
C	END POSITION: BACK TO ALL CONTROL AND REST POSITION
	do i=1,npipes
         ivalve(ntest+2,i)=1+itubeman*(i-1)
	enddo
	itpipe(ntest+2)=irest
C----------------------------------------------------------------
	OPEN(unit=14,file='seqres.dat',status='UNKNOWN',
     &    access='SEQUENTIAL',form='FORMATTED')
892	   FORMAT(5I5)

	do k=1,ntest+2
	   print*,ivalve(k,1),ivalve(k,2),ivalve(k,3),ivalve(k,4),
     &		ivalve(k,5)
	   write(unit=14,fmt=892) ivalve(k,1),ivalve(k,2),ivalve(k,3),
     &   ivalve(k,4),ivalve(k,5)
	enddo

C----------------------------------------------------------------
C     FIND ORDER OF OPENING FOR VALVES AND MOVEMENTS FOR PIPES
C	ALL THIS TRIGERRED BY 1401 SIGNALS : IMPULS(L)
C----------------------------------------------------------------
C	START: OPEN ALL CONTROLS
	k=1

	do i=1,npipes
	   impuls(l)=ivalve(k,i)
ccc	   str(L)='DIGTIM,A,MASK,IVALVE(K,I),DT'
	   l=l+1
	enddo

C----------------------------------------------------------------
C	OPEN FIRST SOLUTION
	k=k+1
	impuls(l)=IVALVE(K-1,itpipe(k))
ccc	str(L)='DIGTIM,A,MASK,IVALVE(K-1,itpipe(k)),DT'    !! close it
	l=l+1
	impuls(l)=IVALVE(K,itpipe(k))
ccc	str(L)='DIGTIM,A,MASK,IVALVE(K,itpipe(k)),DT'    !! open it
	l=l+1

C----------------------------------------------------------------
c	MOVE TO FIRST SOLUTION FROM THE OPEN POSITION
	do while(itpipe(k).ne.irest)
	   if(itpipe(k).gt.irest) then   	! move left
	   impuls(l)=128
ccc	   str(L)='DIGTIM,A,MASK,PIN1,DT'
		L=L+1
		irest=irest+1
	   else if(itpipe(k).lt.irest) then ! move right
	   impuls(l)=64
ccc	   str(L)='DIGTIM,A,MASK,PIN14,DT'
		irest=irest-1
		L=L+1
	   endif
	enddo

C----------------------------------------------------------------
C	CONTINUE
	do k=3,ntest+1
	  if(itpipe(k).eq.itpipe(k-1)) then
	   if(itpipe(k).eq.npipes) then 	! move right
ccc	      str(L)='DIGTIM,A,MASK,PIN14,DT'
	      impuls(l)=64
		L=L+1
		irest=irest-1
	   else					! move leftt
		irest=irest+1
	      impuls(l)=128
ccc	      str(L)='DIGTIM,A,MASK,PIN1,DT'
		L=L+1
	   endif
	   impuls(l)=IVALVE(K-1,itpipe(k-1))
ccc	   str(L)='DIGTIM,A,MASK,IVALVE(K-1,itpipe(k-1)),DT'    !! close it
	   l=l+1
	   impuls(l)=IVALVE(K,itpipe(k))
ccc	   str(L)='DIGTIM,A,MASK,IVALVE(K,itpipe(k)),DT'    !! open next
	   l=l+1
	   if(itpipe(k).gt.irest) then   	! move left     -back
ccc	      str(L)='DIGTIM,A,MASK,PIN1,DT'
	      impuls(l)=128
		L=L+1
		irest=irest+1
	   else if(itpipe(k).lt.irest) then ! move right
	      impuls(l)=64
ccc	      str(L)='DIGTIM,A,MASK,PIN14,DT'
		irest=irest-1
		L=L+1
	   endif
	else
	   idifpipe=itpipe(k)-irest
	   if(idifpipe.gt.1) then
ccc	      str(L)='DIGTIM,A,MASK,PIN1,DT'
	      impuls(l)=128
		L=L+1
		irest=irest+1
	   else if(idifpipe.lt.-1) then
	      impuls(l)=64
ccc	      str(L)='DIGTIM,A,MASK,PIN14,DT'
		irest=irest-1
		L=L+1
	   else if(idifpipe.eq.1) then
	      impuls(l)=64
ccc	      str(L)='DIGTIM,A,MASK,PIN14,DT'
		irest=irest-1
		L=L+1
	   else if(idifpipe.eq.-1) then
ccc	      str(L)='DIGTIM,A,MASK,PIN1,DT'
	      impuls(l)=128
		L=L+1
		irest=irest+1
	   endif
	   impuls(l)=IVALVE(K-1,itpipe(k-1))
ccc	   str(L)='DIGTIM,A,MASK,IVALVE(K-1,itpipe(k-1)),DT'    !! close it
	   l=l+1
	   impuls(l)=IVALVE(K-1,itpipe(k))
ccc	   str(L)='DIGTIM,A,MASK,IVALVE(K-1,itpipe(k)),DT'    !! close it
	   l=l+1
	   impuls(l)=IVALVE(K,itpipe(k-1))
ccc	   str(L)='DIGTIM,A,MASK,IVALVE(K,itpipe(k-1)),DT'    !! open it
	   l=l+1
	   impuls(l)=IVALVE(K,itpipe(k))
ccc	   str(L)='DIGTIM,A,MASK,IVALVE(K,itpipe(k)),DT'    !! open it
	   l=l+1
	   do while(itpipe(k).ne.irest)
	      if(itpipe(k).gt.irest) then   	! move left     -back
ccc	         str(L)='DIGTIM,A,MASK,PIN1,DT'
	         impuls(l)=128
		   L=L+1
		   irest=irest+1
	      else if(itpipe(k).lt.irest) then ! move right
	         impuls(l)=64
ccc	         str(L)='DIGTIM,A,MASK,PIN14,DT'
		   irest=irest-1
		   L=L+1
	      endif
	   enddo
	  endif
	enddo

C----------------------------------------------------------------
C	MOVE TO LAST TEST SOLUTION
	if(itpipe(k).eq.npipes) then 	! move right
ccc	   str(L)='DIGTIM,A,MASK,PIN14,DT'
	   impuls(l)=64
	   L=L+1
	   irest=irest-1
	 else					! move left
	   irest=irest+1
	   impuls(l)=128
ccc	   str(L)='DIGTIM,A,MASK,PIN1,DT'
	   L=L+1
	 endif


C-------------------------------------------------------------------
C	CLOSE LAST TEST AND OPEN CONTROL
C-------------------------------------------------------------------
	impuls(l)=IVALVE(K-1,itpipe(k-1))
ccc	str(L)='DIGTIM,A,MASK,IVALVE(K-1,itpipe(k-1)),DT'    !! close it
	l=l+1
	impuls(l)=IVALVE(K,itpipe(k-1))
ccc	str(L)='DIGTIM,A,MASK,IVALVE(K,itpipe(k-1)),DT'    !! open it
	l=l+1

C-------------------------------------------------------------------
c	MOVE TO REST POSITION
C-------------------------------------------------------------------
	do while(irest.ne.irest0)
	   if(irest.lt.irest0) then   	! move left
	   impuls(l)=128
ccc	   str(L)='DIGTIM,A,MASK,PIN1,DT'
		L=L+1
		irest=irest+1
	   else if(irest.gt.irest0) then ! move right
	   impuls(l)=64
ccc	   str(L)='DIGTIM,A,MASK,PIN14,DT'
		irest=irest-1
		L=L+1
	   endif
	enddo

c=====================================================================
	print*,'Test solutions:'
893	format(A40)
	write(unit=14,fmt=893) 'Test solutions:'
	do j=1,ntest,5
	    print*,test(j),test(j+1),test(j+2),test(j+3),test(j+4)
	    write(unit=14,fmt=892) test(j),test(j+1),test(j+2),
     &    test(j+3),test(j+4)
	enddo
	print*,'Sequence:valves open/close + movments:'
	print*,'128 encodes left, 64 encodes right'
	write(unit=14,fmt=893) 'Sequence:valves open/close + movments:'
	do j=1,l,5
	    print*,impuls(j),impuls(j+1),impuls(j+2),impuls(j+3),
     &	impuls(j+4)
	    write(unit=14,fmt=892) impuls(j),impuls(j+1),impuls(j+2),
     &      impuls(j+3),
     &	impuls(j+4)
	enddo
	close(unit=14)
c======================================================================
	end








