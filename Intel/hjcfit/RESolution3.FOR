	subroutine RESolution3(tint0,tint,ampl0,ampl,iprops0,iprops,nint,
     & nintt,ffilt,fc,rms,trise,nmax,nd1,index,
     & cjump,nsweep,kjumps0,kjumps,autosim,nsims,
     & sim,sres,sexp,excamp,alo,ahi,resdebug,iresnth)

C
      use menu_f90
      logical excamp,shut
	character*11 cstring
	character*200 string
	real*4 tint0(nintt),ampl0(nintt)
	real*4 tint(nintt),ampl(nintt)
	integer*1 iprops0(nintt),iprops(nintt)
	integer*4 index(nintt)
	logical btest,dubious,badgap,setdub,setbad
	logical RESOLV,DIFAMP3		!FUNCTIONS
	logical sbin,shist,sres,sexp
c For cjumps
	logical cjump,null,fbad,difamp2,bad2,flag
	integer*2 kjumps0(nsweep),kjumps(nsweep)

	character*1 ans,UC
	logical discprt,resd,resdebug
	common/dp/discprt
	common/rblck/treso,tresg,acrit,avamp
      common/tty/ittypanel,itty
	dubious(i)=BTEST(iprops0(i),0)	!ampl(i) was dubious (bit 0 set ='1')
	badgap(i)=BTEST(iprops0(i),3)	!tint(i) was unusable (bit 3='8' set)
c
101	format(a1)
c Initialise output arrays
      imode=0
	if(.not.sres) then
	   do i=1,nintt
		tint(i)=0.0
		ampl(i)=0.0
		iprops(i)=0
		index(i)=0
	   enddo
	endif
c
	if(imode.eq.2) then
	   sbin=.false.			!for SCDIR
	   shist=.false.
	   sexp=.false.
	   sres=.false.
	   fc=ffilt			!kHz (-3dB)
	   trise=332.1/fc		!rise time (microsec)
	endif

	jamp=0
      S=0.0
      amp=0.0
      tin=0.0
      ampav=0.0
      ttot=0.0
      sim=.false.
      
	if(treso.lt.0.00001.and.tresg.lt.0.00001) then
	   nint=nintt	!no resolution imposed
	   do i=1,nint
		tint(i)=tint0(i)	!copy input straight to output
		ampl(i)=ampl0(i)
		iprops(i)=iprops0(i)
	   enddo
	   goto 145
	endif
	if(sexp.and.sres) goto 145
	

C CHECK FOR NEG INTERVALS, AND SET THEM
C UNUSABLE SKIPPING THOSE ALREADY SET UNUSABLE.
	do i=1,nintt-1
	   if((.not.badgap(i)).and.tint0(i).lt.0.) then
	     write(string,fmt='(a10,i4,a20)') 'tint(',i,') reset as unusable'
		 CALL GMSETTEXTSETTING(ITTY,string)
		  call intconv(i,cstring) 
	      if(discprt) write(7,107) i,tint0(i)
107	      format('  ****tint(',i4,')= ',g13.6,'  reset as unusable')
		iprops0(i)=IBSET(iprops0(i),3)	!gap unusable; set bit 3 ='8'
	   endif
	enddo
c
c Last interval now set unusable in DISKIN2
c
	
	if(sres) goto 610

	ihjc=1		!HJC always
610	if(ihjc.eq.1) then
      
         if(discprt) write(7,602)
	else
        
         if(discprt) write(7,603)
	endif
c
	ndirect=0	!count # of direct trans introduced by elim of short gaps
	nconcat=0	!count number of adjacent amps<acrit are concatenated

c NOW IMPOSE RESOLUTION
	if(.not.cjump) goto 1001

	ki=0	!index for concatenated intervals
c==	i=0	!index for original intervals ==set via kjumps() for each sweep
c
	kjumps(1)=1		!index in output tint() of 1st interval of 1st jump

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
	   
	   if(resdebug) then
	   if(discprt) write(7,501) i,tint0(i),ampl0(i),iprops0(i)
	    
	      if(discprt) write(7,505)ki,tint(ki),ampl(ki),iprops(ki),
     &	index(ki)
505	      format(27x,' out = ',i6,g12.5,g12.5,i3,i6, '(null)')
	   endif
	   
	   goto 1000			!this sweep done
	endif
c Now start on intervals when sweep is not null.  First check that latency is
c not shorter than tres (i=kjumps(ij)=index of latency for jump #ij, set above).
c If latency is shut, less than tresg, and followed by resolvable opening then
c latency becomes opening (and vice versa) (cannot just skip over first
c interval if it is bad or unresolvable, as is done normally).  If more
c than one iresolvable interval follows t=0 then concat them as usual
c to get 1st latency.  Otherwise, when 1st latency is resolvable, just
c concatenate first latency with subsequent intervals as usual, and set
c bit 5 ('32') in the result. Problem is that in regular case (and in HJC
c definition) a concat interval always starts with a resolvable event
	if(.not.RESOLV(ifirst,tint0,ampl0,iprops0,nintt).and.
     &	RESOLV(ifirst+1,tint0,ampl0,iprops0,nintt)) then
c		Start a (possibly) concat group at 2nd (i+1) interval (but
c		add in duration of the irresolvable orig latency)
	   fbad=.true.	!1st latency below tres, but next interval resolvable
	else
	   fbad=.false.
	endif
c Now start concat for cjump case
20	ki=ki+1
      if(mod(n,iresnth).eq.0) then	!check if debug on every 100th transition
	   resd=resdebug
	   if(resd) then 
	   if(discprt) write(7,501) n,tint0(n),ampl0(n),iprops0(n)
	   if(i.eq.ilast) then
	   if(discprt) write(7,504) ij
504		   format('    (last interval in jump # ',i5,')')
	   endif
	   endif
	else
	   resd=.false.
	endif
	
c
	call SUMAMP(-1,j,S,j1,s1,amp,.false.,setdub)	!initialise sums
	tint(ki)=tint0(i)			!start new concat group
	index(ki)=i			!define index
	if(i.eq.ifirst) then
	   iprops(ki)=IBSET(iprops(ki),5)	!1st latency; set bit 5 ='32'
	   if(fbad) then
		i=i+1					!move to 2nd (resolvable) interval
		tint(ki)=tint(ki)+tint0(i)	!concat with 2nd interval
		fbad=.false.
		if(resd) then
		   if(discprt) write(7,501)i,tint0(i),ampl0(i),iprops0(i)
		endif
	   endif
	endif
	if(badgap(i)) then	!transfer 'unusable'
	    iprops(ki)=IBSET(iprops(ki),3)	!gap unusable; set bit 3 ='8'
	endif
	call SUMAMP(1,j,S,j1,s1,ampl0(i),dubious(i),setdub)	!accumulate amps
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
	if(resd) then
	    if(discprt) write(7,501) i+1,tint0(i+1),ampl0(i+1),iprops0(i+1)
	endif
c==	if(i.eq.nintt) goto 40		!END
c=	if(RESOLV(i+1,tint0,ampl0,iprops0,nintt)) then   !yes: assign amp & go on
c Need sep bit here to deal with i=ilast-1 (so next interval is last in
c the jump) -because last interval is NOT nec resolvable for jump. If it is
c not resolvable then last interval becomes the preceding one, which must
c therefore have bit 6='64' set (unless it is also the first interval,
c which could happen only if all events in the sweep were below res so
c this sweep becomes null after resolution imposed!)
c
	if(.not.RESOLV(i+1,tint0,ampl0,iprops0,nintt)) then   !no (if yes goto 4)
C Do next bit if NOT resolvable. Has the interval after it got the 'same' amp
C as that before it?
c At this point i=anything up to i=ilast-1. For jumps it makes no sense
c to refer to i+2=ilast+1 which will be first interval of next jump
	   if(i.lt.ilast-1) then
		tin2=tint0(i+2)
		bad2=badgap(i+2)
		DIFAMP2=DIFAMP3(ampl0(i),ampl0(i+2))
		if(resd) then
      	   if(discprt) write(7,501) i+2,tint0(i+2),ampl0(i+2),
     &	   iprops0(i+2)
		endif
	   else	!i=ilast-1, so i+1=ilast, i+2 irrelevant
		tin2=0.
		bad2=.false.
		DIFAMP2=.false.
	   endif
c
	   tin=tint(ki)
	   setbad=badgap(ki)			!tint(ki) was unusable
c==	   if(DIFAMP(ampl0(i),ampl0(i+2))) goto 5
c==	   if(DIFAMP(ampl0(i),ampl0(i+2))) then
	   if(DIFAMP2) then
c NEXT bit done if tint(i+1) unresolvable but intervals on each
c side have 'different' amps (not done for i=ilast-1 because
c difamp2 set false above, so OK to refer to i+2 here)
c=5		continue
		tin1=tint0(i+1)
     		tint(ki)=tin + 0.5*tin1
		setbad=setbad.or.badgap(i+1)		!tin or tin1 was unusable
		if(setbad) then
		   iprops(ki)=IBSET(iprops(ki),3)	!gap unusable; set bit 3 ='8'
		endif
		if(j.gt.0) then
		   ampl(ki)=S/float(j)
		   if(setdub) iprops(ki)=IBSET(iprops(ki),0)	!bit 0 set (='1')
		else if(j1.gt.0) then
		   ampl(ki)=S1/float(j1)
		   if(setdub) iprops(ki)=IBSET(iprops(ki),0)	!bit 0 set (='1')
		endif
c         Now finished tint(ki)=value before the newly-formed direct open-open
c		transition, so print output here and start the (possibly concat)
c		level after it =tint(ki+1)
		if(ampl(ki).ne.0.and.ampl0(i+2).ne.0) then
		   ndirect=ndirect+1	!count # of direct trans introduced
		endif
		if(resd) then
		   if(discprt) write(7,507)ki,tint(ki),ampl(ki),iprops(ki),
     &		index(ki)
507	  	   format(30x,' out = ',i6,g12.5,g12.5,i3,i6,' (diff amp)')
		endif
		tint(ki+1)=0.5*tin1 + tint0(i+2)
		index(ki+1)=i+2			!define index
		setbad=badgap(i+1).or.badgap(i+2)	!tin1 or tint0(i+2) was unusable
		if(setbad) then
		   iprops(ki+1)=IBSET(iprops(ki+1),3)	!gap unusable; set bit 3 ='8'
		endif
c   tint(ki+1) (approx=tint0(i+2)) may need more things concat with it
c   so initialise sums (and setdub) and start new ones.
		call SUMAMP(-1,j,S,j1,s1,amp,.false.,setdub)	!initialise sums
		call SUMAMP(1,j,S,j1,s1,ampl0(i+2),dubious(i+2),setdub)	!accumulate amps
		ki=ki+1
		i=i+2
	      goto 70  !continue concatenating until resolvable interval found
	   else		!difamp=false
c Now bit to be done if tint0(i+1) unresolvable and intervals
c on each side have the 'same' amp. Concatenate.  May be done for i=ilast-1
c so cannot refer to i+2 (use tin2, bad2 defined above)
c=		tint(ki)=tin + tint0(i+1) + tint0(i+2)
		tint(ki)=tin + tint0(i+1) + tin2
		nconcat=nconcat+1
c if any of these intervals unusable set whole group unusable
c=		setbad=setbad.or.badgap(i+1).or.badgap(i+2)
		setbad=setbad.or.badgap(i+1).or.bad2
		if(setbad) then
		   iprops(ki)=IBSET(iprops(ki),3)	!gap unusable; set bit 3 ='8'
		endif
		if(i.lt.ilast-1) then
		   call SUMAMP(1,j,S,j1,s1,ampl0(i+2),dubious(i+2),setdub)	!accumulate amps
		   i=i+2
		   goto 70	!continue concatenating until resolvable interval found
		endif		!otherwise carry on to 40
	   endif
	endif
C Assign amp if concatenation finished
40	continue
	if(j.gt.0) then
	   ampl(ki)=S/float(j)
	   if(setdub) iprops(ki)=IBSET(iprops(ki),0)	!bit 0 set (='1')
	else if(j1.gt.0) then
	   ampl(ki)=S1/float(j1)
	   if(setdub) iprops(ki)=IBSET(iprops(ki),0)	!bit 0 set (='1')
	endif
	if(resd) then
		   if(discprt) write(7,502)ki,tint(ki),ampl(ki),iprops(ki),
     &	index(ki)
	endif
	i=i+1
	if(i.le.ilast) goto 20
c	if(i.le.nintt) goto 20
1000	continue		!end of ij=1,nsweep loop
	if(resd) then
	   do i=1,nsweep
	      
      	if(discprt) write(7,506) i,kjumps(i)
506		format('  kjumps(',i6,') = ',i6)
	   enddo
	endif
c Count null sweeps etc
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
     
       if(discprt) write(7,50) nsweep,nnull,nop1,nopn,nord
50	format(
     & ' Number of sweeps = ',i6,/,/,
     & ' Number of null sweeps = ',i6,/,
     & ' Number of sweeps with channel open at t=0 = ',i6,/,
     & ' Number of sweeps with channel open at end = ',i6,/,
     & ' Number of for which none of these above are true = ',i6,/)
c
	goto 1002		!skip regular case
c
c Concatenate intervals (REGULAR CASE)
1001	continue		!jump here for regular case
	ki=0	!counts concatenated intervals
	i=0
	ndirect=0	!count # of direct trans introduced by elim of short gaps
	
C
C IF THE FIRST INTERVAL IS BOTH USABLE AND RESOLVABLE
C THIS IS STARTING POINT. IF NOT LOOK FOR FIRST INTERVAL THAT
C IS BOTH, AND IS PRECEDED BY AN RESOLVABLE INTERVAL TOO (OTHERWISE
C ITS START WILL BE DEFINED ONLY BY THE POSITION OF THE PRECEDING
C UNRESOLVABLE INTERVAL AND SO WILL BE UNRELIABLE)
	i=i+1
	if(RESOLV(i,tint0,ampl0,iprops0,nintt).and.
     &	(.not.badgap(i))) goto 2				!found it
1	i=i+1
	if(RESOLV(i,tint0,ampl0,iprops0,nintt).and.
     &  RESOLV(i-1,tint0,ampl0,iprops0,nintt).and.
     &  (.not.badgap(i))) goto 2
	goto 1
c
C NOW START TO LOOK FOR UNRESOLVABLE INTERVALS
c
2	ki=ki+1
c
c	if(KBHIT()) then
c		ch=GETCH(ktype)
c		deb=.not.deb
c		if(ktype.eq.0.and.ichar(ch).eq.59) deb=.not.deb		!F1 hit
c	endif
      if(mod(i,iresnth).eq.0) then	!check if debug on every 100th transition
	   resd=resdebug
	!   if(discprt.and.resd) write(7,501) n,tint0(n),ampl0(n),iprops0(n)
	else
	   resd=.false.
	endif
	
c
	if(resd) then
         if(discprt) write(7,501)i,tint0(i),ampl0(i),iprops0(i)
	endif
	call SUMAMP(-1,j,S,j1,s1,amp,.false.,setdub)	!initialise sums
	tint(ki)=tint0(i)			!start new concat group
	index(ki)=i			!define index
	if(badgap(i)) then	!transfer 'unusable'
	    iprops(ki)=IBSET(iprops(ki),3)	!gap unusable; set bit 3 ='8'
	endif
	call SUMAMP(1,j,S,j1,s1,ampl0(i),dubious(i),setdub)	!accumulate amps
c is next interval resolvable?
7	continue
	if(i.eq.nintt) goto 4		!END
	if(resd) then
	   if(discprt) write(7,501) i+1,tint0(i+1),ampl0(i+1),iprops0(i+1)
501	   format(i6,g12.5,g12.5,i5)
	endif
	if(RESOLV(i+1,tint0,ampl0,iprops0,nintt)) goto 4  !yes: assign amp & go on
C Do next bit if NOT resolvable. Has the interval after it got the 'same' amp
C as that before it? Note that it is safe to refer to i+2 here
C because last interval is set unusable and so counted as resolvable always,
C so if i=nintt-1 we skip this section via last line above.
C  First check for 2 succesive irresolvable intervals (old method: not if
c ihjc=1)
	if(resd) then
	    if(discprt) write(7,501) i+2,tint0(i+2),ampl0(i+2),iprops0(i+2)
	endif
c (old method removed from here)
	tin=tint(ki)
	setbad=badgap(ki)			!tint(ki) was unusable
	if(DIFAMP3(ampl0(i),ampl0(i+2))) goto 5
c
c Now bit to be done if tint0(i+1) unresolvable and intervals
c on each side have the 'same' amp. Concatenate.
	tint(ki)=tin + tint0(i+1) + tint0(i+2)
	nconcat=nconcat+1
c if any of these intervals unusable set whole group unusable
	setbad=setbad.or.badgap(i+1).or.badgap(i+2)
	if(setbad) then
	   iprops(ki)=IBSET(iprops(ki),3)	!gap unusable; set bit 3 ='8'
	endif
	call SUMAMP(1,j,S,j1,s1,ampl0(i+2),dubious(i+2),setdub)	!accumulate amps
	i=i+2
	goto 7	!continue concatenating until resolvable interval found
c
c NEXT bit done if tint(i+1) unresolvable but intervals on each
c side have 'different' amps.
5	continue
	tin1=tint0(i+1)
	tint(ki)=tin + 0.5*tin1
	setbad=setbad.or.badgap(i+1)		!tin or tin1 was unusable
	if(setbad) then
	   iprops(ki)=IBSET(iprops(ki),3)	!gap unusable; set bit 3 ='8'
	endif
c         Now finished tint(ki)=value before the newly-formed direct open-open
c		transition, so print output here and start the (possibly concat)
c		level after it =tint(ki+1)
	if(ampl(ki).ne.0.and.ampl0(i+2).ne.0) then
	   ndirect=ndirect+1	!count # of direct trans introduced
	endif
	if(resd) then
	   if(discprt) write(7,507)ki,tint(ki),ampl(ki),iprops(ki),
     &	index(ki)
c507	   format(30x,' out = ',i6,g12.5,g12.5,i3,i6,' (diff amp)')
	endif
	tint(ki+1)=0.5*tin1 + tint0(i+2)
	index(ki+1)=i+2			!define index
	setbad=badgap(i+1).or.badgap(i+2)	!tin1 or tint0(i+2) was unusable
	if(setbad) then
	   iprops(ki+1)=IBSET(iprops(ki+1),3)	!gap unusable; set bit 3 ='8'
	endif
c
c   Concat of tint(ki) now ended so assign amp to it
10	if(j.gt.0) then
	   ampl(ki)=S/float(j)
	   if(setdub) iprops(ki)=IBSET(iprops(ki),0)	!bit 0 set (='1')
	else if(j1.gt.0) then
	   ampl(ki)=S1/float(j1)
	   if(setdub) iprops(ki)=IBSET(iprops(ki),0)	!bit 0 set (='1')
	endif
c   tint(ki+1) (approx=tint0(i+2)) may need more things concat with it
c   so initialise sums (and setdub) and start new ones.
	call SUMAMP(-1,j,S,j1,s1,amp,.false.,setdub)	!initialise sums
	call SUMAMP(1,j,S,j1,s1,ampl0(i+2),dubious(i+2),setdub)	!accumulate amps
	ki=ki+1
	i=i+2
	goto 7		!CARRY ON CONCAT
C
C Assign amp if concatenation finished
4	continue
	if(j.gt.0) then
	   ampl(ki)=S/float(j)
	   if(setdub) iprops(ki)=IBSET(iprops(ki),0)	!bit 0 set (='1')
	else if(j1.gt.0) then
	   ampl(ki)=S1/float(j1)
	   if(setdub) iprops(ki)=IBSET(iprops(ki),0)	!bit 0 set (='1')
	endif
	i=i+1
	if(resd) then
		   if(discprt) write(7,502)ki,tint(ki),ampl(ki),iprops(ki),
     &	index(ki)
502	   format(30x,' out = ',i6,g12.5,g12.5,i3,i6)
	endif
	if(i.le.nintt) goto 2
c
c
1002	continue		!finished concatenation
	nint=ki
c	if(imode.eq.2) RETURN		!for SCDIR
145	continue
c Check number unusable or dubious
	nb1=0
	nb2=0
	do i=1,nint-1
	   if(dubious(i)) nb1=nb1+1
	   if(badgap(i)) nb2=nb2+1
	enddo
c
		if(discprt) write(7,289) NINT,ndirect,nconcat,nb1,nb2
289	FORMAT( ' Number of resolved intervals= ',i8,/,' ',i4,
     & ' direct transitions introduced be removal of short gaps',/,
     & 1x,i4,' concatenations because amplitude ''equal'' on each side'
     & ' of irresolvable gap',/,
     & ' ',i4,' intervals with dubious amplitudes',/,
     & ' ',i4,' intervals with undefined durations',/)
602	format(
     & ' (1) Use HJC definition of e-openings and e-gaps')
c	print 603
603	format(
     & ' (2) Consecutive short intervals set unusable (old def)')
c
	RETURN
	END


	logical function DIFAMP3(amp1,amp2)
	common/rblck/treso,tresg,acrit,avamp
C
C FALSE (i.e. amplitudes are 'same') if (1) both amps are zero
C OR (2) both are non-zero but differ by less than ACRIT (pA)
C
	DIFAMP3=.true.
	if(amp1.eq.0.and.amp2.eq.0) DIFAMP3=.false.
	if(amp1.ne.0.and.amp2.ne.0.and.
     & abs(amp1-amp2).lt.acrit) DIFAMP3=.false.

	RETURN
	END


