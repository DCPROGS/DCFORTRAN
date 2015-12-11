	subroutine resolution2(tint0,tint,ampl0,ampl,iprops0,iprops,nint,
     & nintt,ffilt,fc,rms,trise,nmax,nd1,index,
     & cjump,nsweep,kjumps0,kjumps,autosim,nsims,
     & sim,sres,sexp,excamp,alo,ahi)

	use menu_f90
	real*4 tint0(nmax),ampl0(nmax)
	real*4 tint(nd1),ampl(nd1)
	integer*1 iprops0(nmax),iprops(nd1)
	integer*4 index(nmax)
	
	logical btest,dubious(nmax),badgap0(nmax),badgap(nd1),setdub,setbad
	logical sim,autosim,setend
	logical RESOLV,DIFAMP,restrue		!FUNCTIONS
	logical sres,sexp
c For cjumps
	logical cjump,null,fbad,flag
	integer*4 kjumps0(nsweep),kjumps(nsweep)
	logical shutint,diffamp
	logical excamp,shut
	character*11 cstring
	character*200 string
	logical discprt
	common/dp/discprt
	common/rblck/treso,tresg,acrit,avamp
	common/tty/ittypanel,itty
	
	dubious=BTEST(iprops0,0)	!ampl0(i) was dubious (bit 0 set ='1')
	
	badgap0=BTEST(iprops0,3)	!tint0(i) was unusable (bit 3='8' set)
	badgap=BTEST(iprops,3) !tint(i) was unusable (bit 3='8' set)
c
	setend=.false.	!used only if last interval in consam not resolvable
c Initialise output arrays
	setbad=.false. !!!! added
	nint=0
	if(.not.sres) then
	   do i=1,nintt
		tint(i)=0.0
		ampl(i)=0.0
		iprops(i)=0
		index(i)=0
	   enddo
	endif

	if(treso.lt.0.00001.and.tresg.lt.0.00001) then
	   nint=nintt	!no resolution imposed
	   do i=1,nint
		tint(i)=tint0(i)	!copy input straight to output
		ampl(i)=ampl0(i)
		iprops(i)=iprops0(i)
	   enddo
	   goto 145
	endif
c
	
	if(sexp.and.sres) goto 145
      restrue=badgap0(1)
	do i=1,nintt-1
	   if((.not.badgap0(i)).and.tint0(i).lt.0.) then
	  write(string,fmt='(a10,i4,a20)') 'tint(',i,') reset as unusable'
		 CALL GMSETTEXTSETTING(ITTY,string)
		  call intconv(i,cstring) 
c	      Icall= gmDisplayMessageBox('',
c     &	  'tint('//cstring//') reset as unusable',Ginformation,GOK)
c	      print 107,i,tint0(i)
	      if(discprt) write(7,107) i,tint0(i)
107	      format('  ****tint(',i4,')= ',g13.6,'  reset as unusable')
		iprops0(i)=IBSET(iprops0(i),3)	!gap unusable; set bit 3 ='8'
	   dubious(i)=BTEST(iprops0(i),0)	!ampl0(i) was dubious (bit 0 set ='1')
	
	   badgap0(i)=BTEST(iprops0(i),3)
	   endif
	enddo
      restrue=badgap0(1)
	nc=1
	if(.not.cjump) goto 1001
	ki=0	!index for concatenated intervals
	kjumps(1)=1		!index in output tint() of 1st interval of 1st jump
c
	do 1000 ij=1,nsweep
	if(ij.gt.1) then
	   kjumps(ij)=ki+1  !index in output tint() of 1st interval of ijth jump
	endif
	i=kjumps0(ij)	 		!index in tint0() of 1st interval in jth jump
	ifirst=i
	if(ij.lt.nsweep) then
	   ilast=kjumps0(ij+1)-1	!index of last transition in ijth jump
	else
	   ilast=nintt
	endif
c First, skip null sweeps -put them straight into output
	if(i.eq.ilast) then
	   null=BTEST(iprops0(i),5)
	else
	   null=BTEST(iprops0(i),5).and.BTEST(iprops0(i+1),5)
	endif
	if(null) then
	   ki=ki+1
	   tint(ki)=tint0(i)
	   iprops(ki)=iprops0(i)	!=bit 5 = '32'
	   ampl(ki)=ampl0(i)
	   index(ki)=i			!define index
	   nnull=nnull+1
	goto 1000			!this sweep done
	endif
	kifirst=ki
	if(.not.RESOLV(ifirst,tint0,ampl0,iprops0,nintt).and.
     &	RESOLV(ifirst+1,tint0,ampl0,iprops0,nintt)) then
c		Start a (possibly) concat group at 2nd (i+1) interval (but
c		add in duration of the irresolvable orig latency)
	   fbad=.true.	!1st latency below tres, but next interval resolvable
	else
	   fbad=.false.
	endif
20	ki=ki+1
	call SUMWAMP(-1,jamp,S,amp,tin,ampav,ttot,sim)		!initialise sums
	setdub=.false.				!initialise setdub
	tint(ki)=tint0(i)			!start new concat group
	index(ki)=i			!define index
	if(i.eq.ifirst) then
	   iprops(ki)=IBSET(iprops(ki),5)	!1st latency; set bit 5 ='32'
	   if(fbad) then
		i=i+1					!move to 2nd (resolvable) interval
		tint(ki)=tint(ki)+tint0(i)	!concat with 2nd interval
		fbad=.false.
	
	   endif
	endif
	if(badgap0(i)) then	!transfer 'unusable'
	    iprops(ki)=IBSET(iprops(ki),3)	!gap unusable; set bit 3 ='8'
	endif
      shutint=ampl0(i).eq.0   !Thus is start of concatenated shut interval
      if(shutint) then
         ampav=0              !current amplitude for concat group
      else
         call SUMWAMP(1,jamp,S,ampl0(i),tint0(i),ampav,ttot,sim)
      endif
	if(dubious(i)) setdub=.true.
	nc=1
c is next interval resolvable?
70	continue
	if(i.eq.ilast) then
	   if(BTEST(iprops(i),5)) then	!already set as first, so null sweep
		goto 1000				!straight on to next sweep
	   else
		iprops(ki)=IBSET(iprops(ki),6)	!set as last; set bit 6 ='64'
	      goto 40		!END
	   endif
	endif
c for i=ilast-1 next line prints ilast values
	if(shutint) then		!look for opening that ends shut group
	   a2=ampl0(i+1)
	   if(a2.ne.0.and.RESOLV(i+1,tint0,ampl0,iprops0,nintt)) then
c          =shut group ended by open time i+1
		goto 40
	   else	!keep concatenating the current shut group

		nc=nc+1
		tin=tint(ki)
		setbad=badgap(ki)			!tint(ki) already set unusable
		tint(ki)=tin + tint0(i+1)
c if any of these intervals unusable set whole group unusable
		setbad=setbad.or.badgap0(i+1)
		if(setbad) then
		   iprops(ki)=IBSET(iprops(ki),3)	!gap unusable; set bit 3 ='8'
		endif
		if(i.lt.ilast-1) then
		   if(dubious(i+1)) setdub=.true.
		   i=i+1
		   goto 70	!continue concatenating until resolvable opening found
		endif		!otherwise carry on to 40
	   endif
c
	else if(.not.shutint) then	!look for diff amp (open or shut) that ends open group
	   a2=ampl0(i+1)
	   diffamp=DIFAMP(ampav,a2).or.a2.eq.0
	   if(diffamp.and.RESOLV(i+1,tint0,ampl0,iprops0,nintt)) then
		goto 40
	   else	!keep concatenating the current open group
		nc=nc+1
		tin=tint(ki)
		setbad=badgap(ki)			!tint(ki) already set unusable
		tint(ki)=tin + tint0(i+1)
c if any of these intervals unusable set whole group unusable
		setbad=setbad.or.badgap0(i+1)
		if(setbad) then
		   iprops(ki)=IBSET(iprops(ki),3)	!gap unusable; set bit 3 ='8'
		endif
		call SUMWAMP(1,jamp,S,ampl0(i+1),tint0(i+1),ampav,ttot,sim)
		if(dubious(i+1)) setdub=.true.
		if(i.lt.ilast-1) then
		   if(dubious(i+1)) setdub=.true.
		   i=i+1
		   goto 70	!continue concatenating until resolvable opening found
		endif		!otherwise carry on to 40
	   endif
	endif
c
c Assign amp if concatenation finished
40	continue
	if(nc.eq.1) then			!no concatenation
	   iprops(ki)=iprops0(i)	!so transfer props directly
	   if(setbad) then !e.g. when last interval not resolvable it is set bad
	       iprops(ki)=IBSET(iprops(ki),3)
	   endif
	endif
	if(shutint) then
	   ampl(ki)=0
	else
	   if(jamp.gt.0) then
	      ampl(ki)=ampav		!last mean output from sumwamp
	   else
c		print*,'RESINT ERROR #2'
		STOP
	   endif
	endif
	if(setdub) iprops(ki)=IBSET(iprops(ki),0)	!bit 0 set (='1')
	
	i=i+1
	if(i.le.ilast) goto 20		!next interval in same jump

3	nintj=ki
	i1=kifirst
	if(nintj.le.kifirst+2) goto 1000	!not enough to test in this jump
	n0=0
	do while (i1.le.nintj-2)
	   i1=i1+1
	   a1=ampl(i1)
	   a2=ampl(i1+1)
c	   if(a1.eq.a2) then
	   if(abs(a1-a2).lt.1.e-20) then
		n0=n0+1
		tint(i1)=tint(i1)+tint(i1+1)
		iprops(i1)=iprops(i1)+iprops(i1+1)
		do j1=i1+1,nintj-1
		   tint(j1)=tint(j1+1)
		   ampl(j1)=ampl(j1+1)
		   iprops(j1)=iprops(j1+1)
		   index(j1)=index(j1+1)
		enddo
		nintj=nintj-1
		ki=nintj
c!!!		print 28,ij,i,i+1,a1,nintj+1,nintj
		if(discprt) write(7,28) ij,i,i+1,a1,nintj+1,nintj
28		format(/,' Jump # ',i5,
     &' Intervals ',i5,' and ',i5,' had same amplitude = ',g11.5,/,
     &' so they have been concatenated and total number of intervals',/,
     &' reduced from ',i5,' to ',i5,/)
	   endif
	enddo
	if(n0.gt.0) goto 3	!recheck in case there were 3 consec zeros!
c
1000	continue		!end of ij=1,nsweep loop
	nint=ki
	nnull=0	!count null sweeps
	nop1=0	!count number of sweeps that start with channel open
	nopn=0	!count number of sweeps that end with channel open
	nord=0	!counts those for which none of above are true
	do i=1,nsweep
	   flag=.false.	!set if any of above is true
	   ifirst=kjumps(i)	!index in tint() of 1st interval in ith jump
	   if(i.lt.nsweep) then
		ilast=kjumps(i+1)-1	!index of last transition in ith jump
	   else
		ilast=nint
	   endif
	   if(ifirst.eq.ilast) then
		nnull=nnull+1
		flag=.true.
	   endif
	   if(ampl(ifirst).ne.0.) then
		nop1=nop1+1
		flag=.true.
	   endif
	   if(ampl(ilast).ne.0.) then
		nopn=nopn+1
		flag=.true.
	   endif
	   if(.not.flag) then
		nord=nord+1
	   endif
	enddo
c!!!      print 50,nsweep,nnull,nop1,nopn,nord
      if(discprt) write(7,50) nsweep,nnull,nop1,nopn,nord
50	format(
     & ' Number of sweeps = ',i6,/,/,
     & ' Number of null sweeps = ',i6,/,
     & ' Number of sweeps with channel open at t=0 = ',i6,/,
     & ' Number of sweeps with channel open at end = ',i6,/,
     & ' Number of for which none of these above are true = ',i6,/)
c
	nint=ki
	goto 1002		!skip regular case

1001	continue		!jump here for regular case
	ni=0	!counts concatenated intervals
	n=0
	nc=1		!# of intervals concat in each group (if nc=1 transfer
c			!properties directly from input to output)
	
C
C IF THE FIRST INTERVAL IS BOTH USABLE AND RESOLVABLE
C THIS IS STARTING POINT. IF NOT LOOK FOR FIRST INTERVAL THAT
C IS BOTH, AND IS PRECEDED BY AN RESOLVABLE INTERVAL TOO (OTHERWISE
C ITS START WILL BE DEFINED ONLY BY THE POSITION OF THE PRECEDING
C UNRESOLVABLE INTERVAL AND SO WILL BE UNRELIABLE)
	n=n+1
	restrue=.false.
	restrue=RESOLV(n,tint0,ampl0,iprops0,nintt)
	restrue=.false.
	restrue=badgap0(n)
	if(RESOLV(n,tint0,ampl0,iprops0,nintt).and.
     &	(.not.badgap0(n))) goto 2				!found it
1	n=n+1
	if(RESOLV(n,tint0,ampl0,iprops0,nintt).and.
     &  RESOLV(n-1,tint0,ampl0,iprops0,nintt).and.
     &  (.not.badgap0(n))) goto 2
	goto 1

2	ni=ni+1
c
	if(mod(n,100).eq.0) then	!check if debug on every 100th transition
c	   debon=debug()
	endif

	if(.not.(RESOLV(n,tint0,ampl0,iprops0,nintt))) then
c	   imes=gmdisplaymessagebox('','ERROR in RESINT',ginformation,gok)
	
		 CALL GMSETTEXTSETTING(ITTY,'ERROR in RESINT')
!	   print 73,n,tint0(n)
         if(discprt) write(7,73) n,tint0(n)
73	   format(' ERROR in RESINT -TELL DC: n = ',i6,g13.6,' ms')
	endif
	
	call SUMWAMP(-1,jamp,S,amp,tin,ampav,ttot,sim)	!initialise sums
	setdub=.false.				!initialise setdub
	tint(ni)=tint0(n)			!start new concat group
	nc=1
	index(ni)=n			!define index
	if(badgap0(n)) then	!transfer 'unusable'
	    iprops(ni)=IBSET(iprops(ni),3)	!gap unusable; set bit 3 ='8'
	    badgap(ni)=BTEST(iprops(ni),3)
	endif
	shutint=ampl0(n).eq.0	!Thus is start of concatenated shut interval
	if(shutint) then
	   ampav=0		!current amplitude for concat group
	else
	   call SUMWAMP(1,jamp,S,ampl0(n),tint0(n),ampav,ttot,sim)
	endif
	if(dubious(n)) setdub=.true.
c is next interval resolvable? (Return to 7 for next concat interval in a group)
7	continue
	if(n.eq.nintt) goto 4		!END

	if(shutint) then		!look for opening that ends shut group
	   a2=ampl0(n+1)
	   if(a2.ne.0.and.RESOLV(n+1,tint0,ampl0,iprops0,nintt)) then
c          =shut group ended by open time n+1
		goto 4
	   else	!keep concatenating the current shut group
		if(n.eq.nintt-1) then	!check for end of data
	         iprops(ni)=IBSET(iprops(ni),3) !interval #ki is last and set it bad
		   setbad=.true.
		   setend=.true.	!so last (irresolvable) interval omitted
		   tint(ni)=-1.
		    badgap(ni)=BTEST(iprops(ni),3)
		   goto 4	!end group
		endif

		nc=nc+1
		tin=tint(ni)
		
		setbad=badgap(ni)			!tint(ki) already set unusable
		tint(ni)=tin + tint0(n+1)
c if any of these intervals unusable set whole group unusable
		setbad=setbad.or.badgap0(n+1)
		if(setbad) then
		   iprops(ni)=IBSET(iprops(ni),3)	!gap unusable; set bit 3 ='8'
		 badgap(ni)=BTEST(iprops(ni),3)
		endif
		if(dubious(n+1)) setdub=.true.
		n=n+1
		goto 7	!continue concatenating until resolvable opening found
	   endif
c
	else if(.not.shutint) then	!look for diff amp (open or shut) that ends open group
	   a2=ampl0(n+1)
	   diffamp=DIFAMP(ampav,a2).or.a2.eq.0
	   if(diffamp.and.RESOLV(n+1,tint0,ampl0,iprops0,nintt)) then
c          =open group ended by resolvable shut time n+1, OR by resolvable
c		opening that has an amplitude different from that of the amplitude
c		of the current amplitude of the concatenated open group
		goto 4
	   else	!keep concatenating the current open group
		if(n.eq.nintt-1) then	!check for end of data
	         iprops(ni)=IBSET(iprops(ni),3) !interval #ki is last and set it bad
		    badgap(ni)=BTEST(iprops(ni),3)
		   setbad=.true.
		   setend=.true.	!so last (irresolvable) interval omitted
		   tint(ni)=-1.
		   goto 4	!end group
		endif

		nc=nc+1
		tin=tint(ni)
		setbad=badgap(ni)			!tint(ki) already set unusable
		tint(ni)=tin + tint0(n+1)
c if any of these intervals unusable set whole group unusable
		setbad=setbad.or.badgap0(n+1)
		if(setbad) then
		   iprops(ni)=IBSET(iprops(ni),3)	!gap unusable; set bit 3 ='8'
		 badgap(ni)=BTEST(iprops(ni),3)
		endif
		call SUMWAMP(1,jamp,S,ampl0(n+1),tint0(n+1),ampav,ttot,sim)
		if(dubious(n+1)) setdub=.true.
		n=n+1
		goto 7	!continue concatenating until resolvable opening found
	   endif
	endif
c
C Assign amp if concatenation finished
4	continue
	if(nc.eq.1) then			!no concatenation
	   iprops(ni)=iprops0(n)	!so transfer props directly
	    badgap(ni)=BTEST(iprops(ni),3)
	   if(setbad) then !e.g. when last interval not resolvable it is set bad
	       iprops(ni)=IBSET(iprops(ni),3)
	        badgap(ni)=BTEST(iprops(ni),3)
	   endif
	endif
	if(shutint) then
	   ampl(ni)=0
	else
	   if(jamp.gt.0) then
	      ampl(ni)=ampav		!last mean output from sumwamp
	   else
	 CALL GMSETTEXTSETTING(ITTY,'ERROR in RESINT')
c	 imes=gmdisplaymessagebox('','ERROR in RESINT',gstop,gok)
c!!!		print*,'RESINT ERROR #1'
		STOP
	   endif
	endif
	if(setdub) then
	iprops(ni)=IBSET(iprops(ni),0)	!bit 0 set (='1')
	 badgap(ni)=BTEST(iprops(ni),3)
	endif
	n=n+1

	if(setend) then
	   setend=.false.	!used only if last interval in consam not resolvable
	   goto 8
	endif
	IF(n.le.nintt) GOTO 2   !LOOK FOR NEXT CONCAT GROUP
c
c All intervals now finished
8	continue
	nint=ni
c
c Insert here a retrospective scan of the output to check for
c adjacent intervals with identical (zero or otherwise) amplitude -if
c any found, concatenate them
301	n0=0
	i=0
	do while (i.le.nint-2)
	   i=i+1
	   a1=ampl(i)
	   a2=ampl(i+1)
c	   if(a1.eq.a2) then
	   if(abs(a1-a2).lt.1.e-20) then
		n0=n0+1
		tint(i)=tint(i)+tint(i+1)
		do k=0,6
		   if(BTEST(iprops(i+1),k)) then
			iprops(i)=IBSET(iprops(i),k)	!set bits as in concat vals
		    badgap(i)=BTEST(iprops(i),3)
		   endif
		enddo
		do j=i+1,nint-1
		   tint(j)=tint(j+1)
		   ampl(j)=ampl(j+1)
		   iprops(j)=iprops(j+1)
		    badgap(j)=BTEST(iprops(j),3)
		   index(j)=index(j+1)
		enddo
		nint=nint-1
c!!!		print 27,i,i+1,a1,nint+1,nint
		if(discprt) write(7,27) i,i+1,a1,nint+1,nint
27		format(/,
     &' Intervals ',i5,' and ',i5,' had same amplitude = ',g11.5,/,
     &' so they have been concatenated and total number of intervals',/,
     &' reduced from ',i5,' to ',i5,/)
	   endif
	enddo
	if(n0.gt.0) goto 301	!recheck in case there were 3 consec zeros!
c

	if(excamp) then
	   
	   nconc4=0
	   i=0
	   ip1=0
	   do while (i.le.nint-2)
		i=i+1
		a1=ampl(i)
		a2=ampl(i+1)
		if((a1.eq.0.).and.(a2.ge.alo.and.a2.le.ahi)) then
c   look forward until next shut time found -if all intervening openings
c   are within the specified window than concatenate all of them with
c   shutting on each side into one long shut time
		   t=tint(i)+tint(i+1)
		   do j=i+2,nint
			a=ampl(j)
			shut=a.eq.0.
			if(a.ge.alo.and.a.le.ahi) then		!still in window
			   t=t + tint(j)	!group continues -add duration of opening
			else if(shut) then
			   t=t + tint(j)	!group finished -last shut time added
			   ifirst=i
			   ilast=j
			   if(badgap(ifirst).or.badgap(ilast))ip1=IBSET(ip1,3)
			   i=ifirst-1		!so i=ifirst after 1 added above
			   ncon=ilast-ifirst+1	!number concatenated
			   n=nint
			   nint=nint - (ncon-1)
			   nconc4=nconc4+1
			   
c
			   i1=ifirst	!for brevity
			   tint(i1)=t
			   iprops(i1)=ip1
			    badgap(i1)=BTEST(iprops(i1),3)
c			Move all the others down in array
			   do k=ifirst+1,nint-ncon+1
				k1=k+ncon-1
				tint(k)=tint(k1)
			 	ampl(k)=ampl(k1)
				iprops(k)=iprops(k1)
				 badgap(k)=BTEST(iprops(k),3)
				index(j)=index(k1)
			   enddo
			   goto 21
			else		!outside window -abandon the group
			   goto 21
			endif
		   enddo
		endif
21		continue
		ip1=0
	   enddo
	endif
c
c Jump to here after setting resolution for cjump
1002	continue		!finished concatenation
145	continue
c Check number unusable or dubious
	nb1=0
	nb2=0
	nb3=0
	nb4=0
	nb5=0
	do i=1,nint-1
c	   if(dubious(i)) nb1=nb1+1	!dubious refers to iprops0(), not iprops()
c	   if(badgap0(i)) nb2=nb2+1
	   if(BTEST(iprops(i),0)) nb1=nb1+1 !ampl(i) was dubious (bit 0 set ='1')
	   if(BTEST(iprops(i),1)) then 	!ampl(i) was fixed (bit 0 set ='2')
		if(ampl(i).eq.0.) then
		   nb2=nb2+1
		else
		   nb3=nb3+1
		endif
	   endif
	   if(BTEST(iprops(i),2)) nb4=nb4+1 !ampl(i) was constrained (bit 0 set ='4')
	   if(BTEST(iprops(i),3)) nb5=nb5+1 !tint(i) was unusable(bit 3 set ='8')
	enddo
c
	if(autosim.and.nsims.eq.1) then
c!!!	   print 290,NINT
	   if(discprt) write(7,290) NINT
290	   FORMAT( ' Number of resolved intervals= ',i8)
	else
c!!!	   print 289,NINT,nb1,nb2,nb3,nb4,nb5
	   if(discprt) write(7,289) NINT,nb1,nb2,nb3,nb4,nb5
289	   FORMAT( ' Number of resolved intervals= ',i8,/,
     & ' ',i5,' intervals with dubious amplitudes in output',/,
     & ' ',i5,' shut intervals with fixed amplitudes in output',/,
     & ' ',i5,' open intervals with fixed amplitudes in output',/,
     & ' ',i5,' intervals with constrained amplitudes in output',/,
     & ' ',i5,' bad intervals (undefined durations) in output',/)
	endif
	if(excamp) then
c!!!	   print 294,nconc4
	   if(discprt) write(7,294) nconc4
294	   format(/,1x,i4,
     &  ' groups of intervals with amplitudes that were all within',/,
     &  ' within exclusion window were concatenated',/)
	endif
	if(discprt) write(7,295) treso,tresg
295	format(/,' Resolution (microsec) for openings =',g13.6,/, 
     &' Resolution (microsec) for shuttings =',g13.6)
	
	 
!	if(discprt) write(7,30) 1000.*treso,zo,aamaxo,1000.*tresg,
!     & zg,aamaxg
!		call FALSEV1(treso,fc,rms,avamp,frato)
!		call FALSEV1(tresg,fc,rms,avamp,fratg)
!	if(discprt.and.ffilt.gt.0) write(7,32) frato,fratg
c	if(sexp.and.ffilt.gt.0) print 32,frato,fratg
32	format(
     & '  false event rate (per sec) for openings  = ',g13.6,/,
     & '			      for shuttings = ',g13.6)

	if(discprt) write(7,31) avamp,acrit
31	format(' For sublevels take:',/,3x,
     & ' full amplitude (pA)= ',f8.2,'; pA for real difference= ',f6.3)
30	format(/,' Resolution (microsec):',/,
     & '   for openings = ',f8.2,' (= ',g11.4,' risetimes, A/Amax = ',
     & g12.5,' )',/,
     & '   for shuttings= ',f8.2,' (= ',g11.4,' risetimes, A/Amax = ',
     & g12.5,' )',/)
c
c
	RETURN
	end

	subroutine SUMWAMP(imode,j,S,amp,tint,ampav,ttot,sim)
c To calculate sum of amplitudes, weighted by their duration (=tint)
c Returns ampav=current value of average after each call, and ttot=current
c value of denominator=total duration of the intervals used
C IF imode=-1 IT INITIALISES.
	logical sim
c
	if(imode.lt.0) then   !initialise only
	   j=0
	   S=0.0
	   ttot=0.0
	   RETURN
	endif

	if(sim.and.abs(amp).lt.1.e-30) then
	   RETURN
	endif
c
	j=j+1
	S=S+amp*tint
	ttot=ttot+tint
	if(ttot.gt.1.e-30*abs(s)) then
	   ampav=s/ttot
	else if(j.eq.1) then
	   ampav=amp
	endif
c
	RETURN
	end


	logical function RESOLV(i,tint0,ampl0,iprops0,nintt)
	real*4 tint0(nintt),ampl0(nintt)
	integer*1 iprops0(nintt)
	common/rblck/treso,tresg,acrit,avamp
	logical btest
C
C Set TRUE if tint(i) is resolvable (or unusable- assumed long)
	RESOLV=.TRUE.
	if(BTEST(iprops0(i),3)) RETURN 	!unusable
c
	if(ampl0(i).eq.0) then
	   if(tint0(i).lt.tresg) resolv=.false.
	else
	   if(tint0(i).lt.treso) resolv=.false.
	endif
	RETURN
	END


	logical function DIFAMP(amp1,amp2)
	common/rblck/treso,tresg,acrit,avamp

	a0=1.e-5 	!single precision criterion for zero amp
	DIFAMP=.true.
c=	if(amp1.eq.0.and.amp2.eq.0) DIFAMP=.false.
	if((abs(amp1).lt.a0).and.(abs(amp2).lt.a0)) DIFAMP=.false.
c=	if(amp1.ne.0.and.amp2.ne.0.and.
	if((abs(amp1).gt.a0).and.((abs(amp2).gt.a0)).and.
     & abs(amp1-amp2).le.acrit) DIFAMP=.false.
c  Line added 12/18/01 06:35am
	if(abs(amp1-amp2).lt.a0) difamp=.false.

	RETURN
	END


