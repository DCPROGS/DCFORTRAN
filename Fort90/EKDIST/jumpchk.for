	subroutine JUMPCHK(nset,nsweep,jumps,jumporig,kjumps0,
     & jumpomit,nomit,tint0,iampl0,ampl0,iprops0,nintt,
     & nmax,nswpmax)
c
c Check for any problems in kjumps0() here.
c
	real*4 tint0(nmax,nset),ampl0(nmax,nset)
	integer*2 iampl0(nmax,nset)
	integer*1 iprops0(nmax,nset)
	integer nintt(10)
	integer ibad1(50),ibad2(50),ibadk(50)
	logical btest,check
	integer nsweep(10),nomit(10)
	integer*2 jumps(nswpmax,nset),jumpomit(nswpmax,nset)
	integer*2 jumporig(nswpmax,nset)
	integer*4 kjumps0(nswpmax,nset)
c
	character*1 ans
	logical discprt,dsav,debug,caplock
	common/dp/discprt
c
	debug()=caplock()
c
c Check for any problems in kjumps0() here.
	ans='Y'
	call DCASK('Check jumps for errors in SCANning',ans,ans)
	check=ans.eq.'Y'
102	continue	!return to re-check
	nprobs=0	!number of problems found
c
	do j=1,nset
	   if(.not.check) goto 100
	   nswsav=nsweep(j)
c
c First make sure that iprops0=64 is always followed directly by iprops0=32
c If spurious intervals found between 64 and 32 then remove them
c If any if the intervals that are removed have been recorded in kjumps(0)
c as the start of a jump (despite fact that iprops0 is not 32) then
c remove this kjumps0 too.
	   m=1
	   nbad=0
	   do while(m.le.nintt(j)-1)
		if(BTEST(iprops0(m,j),6)) then        !bit 6='64' set
		   if(.not.BTEST(iprops0(m+1,j),5)) then        !bit 5='32' NOT set
c               look ahead for next iprops0=32
			do m1=m+1,nintt(j)
			   if(BTEST(iprops0(m1,j),5)) then        !bit 5='32' set
				nbad=nbad+1
				ibad1(nbad)=m+1
				ibad2(nbad)=m1-1
				goto 64
			   endif
			enddo
64			continue
			if(nbad.gt.0) then
		 	   print 63,ibad1(nbad),ibad2(nbad)
			   if(discprt) write(8,63)
     &			ibad1(nbad),ibad2(nbad)
63			   format(/,
     &' Spurious events found between end of one jump '
     & ,/,' and start of next: events ',i5,' to ',i5)
			endif
		   endif
		endif
		m=m+1
	   enddo
	   if(nbad.gt.0) then
		call BELL(1)
		ans='Y'
		call DCASK('Eliminate all spurious events',ans,ans)
		if(ans.eq.'Y') then
c             Need to remove any kjumps0 too?
		   nb1=0
		   do n=1,nbad
			n1=ibad1(n)
			n2=ibad2(n)
			do i2=1,nsweep(j)
			   do k=n1,n2
				if(kjumps0(i2,j).eq.k) then	!eliminate this kjumps0
				   nb1=nb1+1
				   ibadk(nb1)=i2
				endif
			   enddo
			enddo
		   enddo
		   if(nb1.gt.0) then
			k=0
			do n=1,nb1
			   ielim=ibadk(n)
			   do i2=1,nsweep(j)
				if(i2.ne.ielim) then
				   k=k+1
			         jumps(k,j)=jumps(i2,j)
			         kjumps0(k,j)=kjumps0(i2,j)-(i2-k)
				endif
			   enddo
			   jumps(nsweep(j),j)=0		!zero the end one
			   kjumps0(nsweep(j),j)=0
			   nsweep(j)=nsweep(j)-1
			enddo
		   endif
c             now remove spurious intervals
		   do n=1,nbad
			n1=ibad1(n)
			n2=ibad2(n)
			nn=n2-n1+1		!number to be eliminated
			do k=n1,nintt(j)-nn
			   tint0(k,j)=tint0(k+nn,j)
			   iampl0(k,j)=iampl0(k+nn,j)
			   ampl0(k,j)=ampl0(k+nn,j)
			   iprops0(k,j)=iprops0(k+nn,j)
			enddo
			do k=nintt(j)-nn+1,nintt(j)		!zero the rest
			   tint0(k,j)=0.
			   iampl0(k,j)=0
			   ampl0(k,j)=0.
			   iprops0(k,j)=0
			enddo
			nintt(j)=nintt(j)-nn
		   enddo
		endif
		print 65,nintt(j)+nn,nswsav,nintt(j),nsweep(j)
		if(discprt)write(8,65) nintt(j)+nn,nswsav,nintt(j),nsweep(j)
65		format(
     &	' Before correction: ',i5,' intervals in ',i3,' jumps',/,
     &	' After  correction: ',i5,' intervals in ',i3,' jumps')
		print
	   endif
c (1) Do two jumps appear to start with the same transition # (same kjumps0)?
c If so, one of the two has presumably not been analysed so it must be
c removed from jumps() and kjumps0(). There is no way for the program
c to tell which should be removed, but latency is printed so if user
c has a printout of the jumps it may be possible to see which jump has been
c analysed. It is assumed that the transitions (tint0 etc) are OK, just
c kjumps0 that is wrong.
	   i=1
	   do while(i.le.nsweep(j))
c==	   do i=1,nsweep(j)	!this ignores changes in nsweep(j) inside loop
		m=kjumps0(i,j)
		do i1=1,i-1		!has this kjumps0 occured before?
		   if(m.eq.kjumps0(i1,j)) then
			i0=i
			call BELL(1)
			if(i.lt.nintt(j)) then
			   n1=kjumps0(i0+1,j)-kjumps0(i0,j)
			else
			   n1=nintt(j)-kjumps0(i0,j)
			endif
			if(i1.lt.nintt(j)) then
			   n2=kjumps0(i1+1,j)-kjumps0(i1,j)
			else
			   n2=nintt(j)-kjumps0(i1,j)
			endif
			if(.not.BTEST(iprops0(m,j),5)) then        !bit 5='32' set
			   print 80,m
80			   format(
     &    ' Transition # ',i6,' NOT marked in SCAN as first in jump')
			endif
			print 81,i1,jumps(i1,j),i0,jumps(i0,j),m,
     &		  m,ampl0(m,j),tint0(m,j),i1,n2,i,n1
81			format(/,' PROBLEM in data',/,
     &' Jumps ',i4,' (',i4,') and ',i4,' (',i4,
     &') both appear to start at transition ',i6,/,
     & ' Transition # ',i6,' has amplitude = ',f8.3,' and duration = '
     &		  ,f8.3,' ms',/,
     &  ' Jump ',i4,' appears to contain ',i5,' fitted transitions.',/,
     &  ' Jump ',i4,' appears to contain ',i5,' fitted transitions.')
			print 82,i1,i0
82			format(/,' Choose which to remove:',/,
     &		  ' (1) jump ',i4,' was not analysed',/,
     &		  ' (2) jump ',i4,' was not analysed',/,
     &		  ' Option number [1] = ')
			iopt=1
			call INPUTi(iopt)
			if(iopt.eq.1) then
			   n=n2
			   ielim=i1
			else
			   n=n1
			   ielim=i0
			endif
			k=0
			do i2=1,nsweep(j)
			   if(i2.ne.ielim) then
				k=k+1
			      jumps(k,j)=jumps(i2,j)
			      kjumps0(k,j)=kjumps0(i2,j)
			   endif
			enddo
			jumps(nsweep(j),j)=0	!zero the end one
			kjumps0(nsweep(j),j)=0
			nsweep(j)=nsweep(j)-1
			print 83,i1,jumps(i1,j),i0,jumps(i0,j),m,
     &			ielim,jumps(ielim,j)
			if(discprt) then
			   write(8,83) i1,jumps(i1,j),i0,jumps(i0,j),m,
     &			ielim,jumps(ielim,j)
83			   format(/,
     & ' Jumps ',i4,' (',i4,') and ',i4,' (',i4,
     &		') both started at transition ',i6,/,
     &' Jump ',i4,' (sweep ',i4,') now removed',/)
 			nprobs=nprobs+1	!number of problems found
			endif
		   endif	!end of if(m.eq.kjumps0(i1,j))
		enddo		!end of do i1=1,i-1
		i=i+1
	   enddo		!end of i=1,nsweep(j)
c  End of bit for case where kjump0 values duplicated
c
c (2) Does same expt occur twice (e.g. if went BACK in scan to re-fit a jump)?
c     (a) completely eliminate one of them (inc transitions if not null)
c     (b) re-order all transitions so jumps() is in ascending order (prob
c		this matters only if stability plots added)
c If duplicate found, eliminate one (including all the transitions in that jump)
c and renumber jumps(), kjumps(), change nsweep and, if any transitions
c eliminated, also cut the from tint0(), ampl0() and iprops(i),
c and change nintt()
	   i=1
	   do while(i.le.nsweep(j))
c=	   do i=1,nsweep(j)
		k1=jumps(i,j)
		do j2=1,i-1		!has this jumps() occured before?
		   if(k1.eq.jumps(j2,j)) then
			j1=i
			call BELL(1)
			m1=kjumps0(i,j)
			if(i.lt.nintt(j)) then
			   n1=kjumps0(i+1,j)-kjumps0(i,j)
			else
			   n1=nintt(j)-kjumps0(i,j)
			endif
			m2=kjumps0(j2,j)
			if(j2.lt.nintt(j)) then
			   n2=kjumps0(j2+1,j)-kjumps0(j2,j)
			else
			   n2=nintt(j)-kjumps0(j2,j)
			endif
			print 84,k1,n2,m2,j2,n1,m1,j1
84			format(/,' PROBLEM in data',/,
     & ' Sweep ',i4,' occurs twice:',/,
     &'  (1) ',i5,' transitions starting at transition ',i6,
     &	' (jump ',i4,')',/,
     &'  (2) ',i5,' transitions starting at transition ',i6,
     &	' (jump ',i4,')')
c             Check that both are marked as first in jump
			if(.not.BTEST(iprops0(m1,j),5)) then        !bit 5='32' set
			   print 85,m1
85			   format(
     &	' ERROR: transition ',i6,' not marked as first in jump')
			endif
			if(.not.BTEST(iprops0(m2,j),5)) then        !bit 5='32' set
			   print 85,m2
c85			   format(
c     &	' ERROR: transition ',i6,' not marked as first in jump')
			endif
c Now decide which to keep
			print 86,k1,m2,j2,m1,j1
86			format(
     &	      ' Remove the record of jump ',i4,/,
     &		  ' (1) that starts at transition # ',i6,
     &			' (jump ',i4,')',/,
     &		  ' (2) that starts at transition # ',i6,
     &			' (jump ',i4,')',/,
     &		  ' Option number [1] = ')
			iopt=1
			call INPUTi(iopt)
			if(iopt.eq.1) then
			   n=n2
			   melim=m2
			   j0=j2
			else
			   n=n1
			   melim=m1
			   j0=j1
			endif
			k=0
			do i2=1,nsweep(j)
			   if(i2.ne.j0) then
				k=k+1
			      jumps(k,j)=jumps(i2,j)
			      kjumps0(k,j)=kjumps0(i2,j)
			   endif
			enddo
			jumps(nsweep(j),j)=0	!zero the end one
			kjumps0(nsweep(j),j)=0
			nsweep(j)=nsweep(j)-1
c               After eliminated jump, decrement start trans by n
			do i2=j0,nsweep(j)
			   kjumps0(i2,j)=kjumps0(i2,j)-n
			enddo
c			If n>0 then correct also tint0, ampl0, iprops0,nintt
			if(n.gt.0) then
			   do k=melim,nintt(j)-n
				tint0(k,j)=tint0(k+n,j)
				iampl0(k,j)=iampl0(k+n,j)
				ampl0(k,j)=ampl0(k+n,j)
				iprops0(k,j)=iprops0(k+n,j)
			   enddo
			   do k=nintt(j)-n+1,nintt(j)		!zero the rest
				tint0(k,j)=0.
				iampl0(k,j)=0
				ampl0(k,j)=0.
				iprops0(k,j)=0
			   enddo
			   nintt(j)=nintt(j)-n
			endif
c		    print what was done
			print 87,k1,n2,m2,j2,n1,m1,j1,n,melim
			if(discprt) write(8,87) k1,n2,m2,j2,n1,m1,j1,n,melim
87			format(
     &	 	' Sweep ',i4,' occurred twice:',/,
     &          '  (1) ',i5,' transitions starting at transition ',i6,
     & 		' (jump ',i4,')',/,
     &	    '  (2) ',i5,' transitions starting at transition ',i6,
     & 		' (jump ',i4,')',/,
     &		' The ',i5,' transitions starting at ',i6,
     &		' have been removed',/)
 			nprobs=nprobs+1	!number of problems found
			goto 161	!out of j2 loop once duplicate found
		   endif	!end of if(m.eq.kjumps0(j2,j))
		enddo		!end of do j2=1,i-1
161		continue
		i=i+1
	   enddo		!end of i=1,nsweep(j)
c
c==debug 777	   continue
c After any corrections now check consistency of kjumps0 and iprops
	   ij=0		!count jumps
	   nnull=0	!count null jumps
	   ns1=1		!initialise for GETJUMP
	   do k=1,nintt(j)
		if(BTEST(iprops0(k,j),5)) then        !bit 5='32' set
		   ij=ij+1			!count jumps
		   if(kjumps0(ij,j).ne.k) then
			call BELL(1)
			print 88,ij,j,kjumps0(ij,j),k
	 	      if(discprt) write(8,88) ij,j,kjumps0(ij,j),k
88			format(' kjumps(',i4,',',i4,') = ',i6,
     &			' is not equal to k =',i6)
		   endif
c NB if last interval has bit 32 set (=start of sweep) with nothing
c after it then last sweep must be null
		   if(k.eq.nintt(j).or.BTEST(iprops0(k+1,j),5)) then
			nnull=nnull+1
		   endif
		endif
		if(debug()) then
		 ns1=1
	       call GETJUMPs(k,ijump,njump,ns1,jumps,kjumps0,nsweep,
     & 	   j,nswpmax,nset)		 !test for debug
		   if(discprt) write(8,89) k,j,tint0(k,j),iampl0(k,j),
     &		iprops0(k,j),ijump,njump
89		   format(1x,2i6,g13.6,4i8)
		endif
c
	   enddo
c
	   if(ij.ne.nsweep(j)) then
		call BELL(1)
		print 90,j,nsweep(j),ij
	      if(discprt) write(8,90) j,nsweep(j),ij
90		format(
     &   	  ' Set ',i3,' nsweep =',i4,' not equal to ij = ',i4)
	   else
		if(nset.gt.1) then
		   print 91,j
	         if(discprt) write(8,91) j
91		   format(' Data set # ',i4)
		endif
	      print 92,nsweep(j),nnull
	      if(discprt) write(8,92) nsweep(j),nnull
92		format(' Read ',i4,' jumps, of which ',i4,' are null')
	   endif
c
	   if(nprobs.ne.0) goto 102	!go round again to re-check
c
100	   continue
c Check whether jumps() suddenly restarts for near 1, as when data is
c from more than one cjump.dat file, each one starting at (or near) jump #1
c Modified 06/18/01 10:37am. This addition is now done in DISKIN2, so
c here just print out the numbering
	   print 108
	   if(discprt) write(8,108)
108	   format(/)
	   if(nset.gt.1) then
		print 91,j
	      if(discprt) write(8,91) j
c91		format(' Data set # ',i4)
	   endif
	   print 93
	   if(discprt) write(8,93)
93	   format(
     &' Sweep # is the number in original CJUMP data.  If more than',/,
     &'  cjump.cjd file has been pooled a constant will be added to',/,
     &'  the sweep number so that no number is repeated.           ',/,
     &   ' The original and new sweep numbers are given here.')
c        Can get 18 numbers of 3 digits per line, and have ij2-ij1+1
c        numbers to print so number of lines needed is
	   ij1=1
	   ij2=nsweep(j)
	   np=ij2-ij1+1
	   nl=1+(np-1)/18
	   nleft=np - (nl-1)*18
	   ii1=1
	   ii2=ii1+18-1
	   if(nl.eq.1) ii2=ii1+nleft-1
	   do n=1,nl
		print 60,(i1,i1=ii1,ii2)
		print 61,(jumporig(i1,j),i1=ii1,ii2)
	 	print 62,(jumps(i1,j),i1=ii1,ii2)
		if(discprt) write(8,60) (i1,i1=ii1,ii2)
		if(discprt) write(8,61)(jumporig(i1,j),i1=ii1,ii2)
	 	if(discprt) write(8,62)(jumps(i1,j),i1=ii1,ii2)
60		format(/,' jump ',18(1x,i3))
61		format(' orig ',18(1x,i3))
62		format(' new  ',18(1x,i3))
		ii1=ii2+1
		ii2=ii1+18-1
		if(n.eq.nl-1) ii2=ii1+nleft-1		!next line is last
	   enddo
c	   jadd=0
c	   do i=1,nsweep(j)
c		if(jadd.eq.0) then
c		   print 94,i,jumps(i,j)
c		   if(discprt) write(8,94) i,jumps(i,j)
c94		   format(i7,3x,i7)
c		else
c		   print 95,i,jumps(i,j),jumps(i,j)-jadd
c		   if(discprt) write(8,95) i,jumps(i,j),jumps(i,j)-jadd
c95		   format(i7,3x,i7,'  (',i7,')')
c		endif
c		if(i.lt.nsweep(j).and.jumps(i+1,j).lt.jumps(i,j)) then
c		   print 961,(jumps(i1,j),i1=1,nsweep(j))
c961		   format(' Original sweep numbers:',/,10(20(1x,i3),/))
c		   print 96,i,jumps(i,j),i+1,jumps(i+1,j),jumps(i,j),i+1
c96			   format(
c     &' Jump ',i5,' is sweep ',i5,' but',/,' jump ',i5,' is sweep ',i5,/
c     &,' Add ',i5,' to all sweep numbers from jump ',i5,' O.K. [Y] ? ')
c		   ans='Y'
c		   call INPUTa(ans)
c		   if(UC(ans).ne.'N') then
c			jadd=jumps(i,j)
c			do k=i+1,nsweep(j)
c			   jumps(k,j)=jumps(k,j)+jadd
c			enddo
c		   endif
c		endif
c	   enddo
c
c  For cjumps, now check that no jumps are duplicated
c================surely already done
	   nsame=0
	   do i=1,nsweep(j)-1
		do k=i+1,nsweep(j)
		   if(i.ne.k) then
			if(jumps(i,j).eq.jumps(k,j)) then
c               Calculate number of transitions in each of the duplicated jumps
c			(NB get ni=1 for null sweep so # of transitions is ni-1
			   if(i.eq.nsweep(j)) then
				ni=nintt(j)-kjumps0(i,j)+1
			   else
				ni=(kjumps0(i+1,j)-1) - kjumps0(i,j) + 1
			   endif
			   ni=ni-1	!# of transitions
			   if(k.eq.nsweep(j)) then
				nj=nintt(j)-kjumps0(k,j)+1
			   else
				nj=(kjumps0(k+1,j)-1) - kjumps0(k,j)+1
			   endif
			   nj=nj-1	!# of transitions
			   print 32,i,ni,k,nj,jumps(i,j)
			   if(discprt) write(8,32) i,ni,k,nj,jumps(i,j)
32			   format(/,' Jump ',i4,' (',i5,
     &			' transitions) and ',/,' Jump ',
     &		     i4,' (',i5,' transitions) are both sweep # ',i4)
			   nsame=nsame+1
			endif
		   endif
		enddo
	   enddo
	   if(nsame.ne.0) call BELL(1)
c
c Omit some jumps?
	   print 108
c	   if(discprt) write(8,108)
	   if(nset.gt.1) then
		print 91,j
	      if(discprt) write(8,91) j
c91		format(' Data set # ',i4)
	   endif

	   ans='N'
	   if(nsame.gt.0) ans='Y'
	   print 2,ans
2	   format(' Omit any of these jumps [',a1,'] ? ')
	   call INPUTa(ans)
c==	   call DCASK('Omit any of these jumps',ans,ans)
	   if(ans.eq.'Y') then
		print 76,jumps(1,j),jumps(nsweep(j),j),nomit(j)
76 		format(' Sweeps ',i5,' to ',i5,' read in from SCANDAT.scn.',/,
     &	'  Last time ',i3,' of these jumps were omitted: ')
		if(nomit(j).gt.0) then
		   dsav=discprt
		   discprt=.false.
c             NB Printni2 has 1D array but following call should be OK
		   call PRINTni2(nomit(j),jumpomit(1,j))	!int*2 version
		   discprt=dsav
		   print 71
71		   format(' number    jump #   sweep #')
		   do i=1,nomit(j)
			k=jumpomit(i,j)
			if(k.ge.1.and.k.le.nsweep(j)) then
			   print 36,i,j,jumps(k,j)
36			   format(i3,6x,i5,4x,i5)
			endif
		   enddo
	      endif
	      call DCASK('Omit same sweeps again','y',ans)
		if(ans.eq.'N') then
		   do i=1,nsweep(j)
			jumpomit(i,j)=0
		   enddo
		   print 72
72		   format(' Specify numbers of sweeps to be omitted')
		   call INPUTni2(nomit(j),jumpomit(1,j),nsweep(j))	!int*2 version
		endif
	   else
		nomit(j)=0
		do i=1,nsweep(j)
		   jumpomit(i,j)=0
		enddo
	   endif
c       Print omissions
	   if(nomit(j).eq.0) then
      	if(discprt) write(8,73)
73		format(/,' No jumps omitted',/)
	   else
		print 74,nomit(j)
      	if(discprt) write(8,74) nomit(j)
74		format(/,1x,i4,' jumps omitted: ',/,
     &	' number   sweep #   jump #')
		nn=nnull 	!for values after omissions
		ns=nsweep(j) 	!for values after omissions
		do i=1,nomit(j)
		   k=jumpomit(i,j)
		   print 75,i,k,jumps(k,j)
		   if(discprt) write(8,75) i,k,jumps(k,j)
75		   format(i3,6x,i5,4x,i5)
c NB if last interval has bit 32 set (=start of sweep) with nothing
c after it then last sweep must be null
		   ns=ns-1
		   n=kjumps0(k,j)
		   if(BTEST(iprops0(n,j),5)) then        !bit 5='32' set
			if(n.eq.nintt(j).or.BTEST(iprops0(n+1,j),5)) then
			   nn=nn-1
			endif
		   endif
		enddo
	      print 251,ns,nn
	      if(discprt) write(8,251) ns,nn
251		format(/,
     &  ' After omissions have ',i4,' jumps, of which ',i4,' are null')
	   endif
c
	enddo	!end of do j=1,nset
c end of code from jumpchk
c
	RETURN
	end

