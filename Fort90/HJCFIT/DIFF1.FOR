Comparing files RESINT61.FOR and \fort90\ekdist\resint6.for
****** RESINT61.FOR
        subroutine RESINT61(tint0,tint,ampl0,ampl,iprops0,iprops,nint,
     & nintt,imode,ffilt,fc,rms,trise,nmax,nd1,index,
     & cjump,nsweep,kjumps0,kjumps)
****** \fort90\ekdist\resint6.for
        subroutine RESINT6(tint0,tint,ampl0,ampl,iprops0,iprops,nint,
     & nintt,imode,ffilt,fc,rms,trise,index,
     & cjump,nsweep,kjumps0,kjumps)
******

****** RESINT61.FOR
c
c RESINT61 is version of resint6 for HJCFIT, in which dimensions of tint() etc
c are defined as in resint3.
c RESINT51 is version of resint5 for HJCFIT, in which dimensions of tint() etc
c are defined as in resint3.
c
c RESINT6 is version in which definition of resolution is exactly
****** \fort90\ekdist\resint6.for
c
c RESINT6 is version in which definition of resolution is exactly
******

****** RESINT61.FOR
c and second interval of output, but are part of first) 02/28/00 10:51am
c Resint5 is version in which kjumps0 and kjumps are integer*4
c RESINT4  is completely revised version
****** \fort90\ekdist\resint6.for
c and second interval of output, but are part of first) 02/28/00 10:51am
c
c Resint5 is version in which kjumps0 and kjumps are integer*4
c NB any changes must be made (a) in both jump and steady state sections
c AND (b) in RESINT51 (used in HJCFIT)
c
c RESINT4  is completely revised version
******

****** RESINT61.FOR
C
        real*4 tint0(nmax),ampl0(nmax)
        real*4 tint(nd1),ampl(nd1)
        integer*1 iprops0(nmax),iprops(nd1)
        integer*4 index(nmax)
c       real*4 tint0(nintt),ampl0(nintt)
c       real*4 tint(nintt),ampl(nintt)
c       integer*1 iprops0(nintt),iprops(nintt),ip1
c       integer*4 index(nintt)
        logical DEBUG,pon,slock,caplock,debon
****** \fort90\ekdist\resint6.for
C
        real*4 tint0(nintt),ampl0(nintt)
        real*4 tint(nintt),ampl(nintt)
        integer*1 iprops0(nintt),iprops(nintt),ip1
        integer*4 index(nintt)
        logical DEBUG,pon,slock,caplock,debon
******

****** RESINT61.FOR
        logical RESOLV,DIFAMP           !FUNCTIONS
        logical concat2,concat3
        logical sbin,shist,sres,sexp
****** \fort90\ekdist\resint6.for
        logical RESOLV,DIFAMP           !FUNCTIONS
        logical diffamp
        logical ifall,shutint
        logical sbin,shist,sres,sexp
******

****** RESINT61.FOR
c For cjumps
        logical cjump,null,fbad,difamp2,bad2,flag
c===    integer*2 kjumps0(nsweep),kjumps(nsweep)
        integer*4 kjumps0(nsweep),kjumps(nsweep)
        logical excamp,shut
c
****** \fort90\ekdist\resint6.for
c For cjumps
        logical cjump,null,fbad,flag
        integer*4 kjumps0(nsweep),kjumps(nsweep)
        logical excamp,shut,shut1,open2,exc2,setexc
c
******

****** RESINT61.FOR
        excamp=.false.
        if(.not.sres) then
****** \fort90\ekdist\resint6.for
        excamp=.false.
        ifall=.true.
        if(.not.sres) then
******

****** RESINT61.FOR
         if(cjump) then
         print 34,iopt
34       format(/,
     & ' (1) Carry on',/,
****** \fort90\ekdist\resint6.for
         if(cjump) then
            print 34,iopt
34          format(/,
     & ' (1) Carry on',/,
******

****** RESINT61.FOR
         else
          print 341,iopt
341       format(/,
     & ' (1) Carry on',/,
****** \fort90\ekdist\resint6.for
         else
            print 341,iopt
341         format(/,
     & ' (1) Carry on',/,
******

****** RESINT61.FOR
            excamp=.true.
            alo=-50.
****** \fort90\ekdist\resint6.for
            excamp=.true.
            ifall=.true.
            alo=-50.
******

****** RESINT61.FOR
            endif
            if(alo.gt.ahi) then
****** \fort90\ekdist\resint6.for
            endif
            if((alo.eq.0.).or.(ahi.eq.0.).or.(alo*ahi).lt.-0.0001) then
                call BELL(3)
                print 770
770             format(' Range must NOT include zero')
                goto 36
            endif
            if(alo.gt.ahi) then
******

****** RESINT61.FOR
            endif
            print 51,alo,ahi
            if(discprt) write(8,51) alo,ahi
51          format(/,
     &' Shut times that are separated by one or more openings with',/,
     &' amplitudes all between ', g13.6,'pA and ',g13.6,'pA are ',/,
     &' concatenated into one long shut time')
            goto 36
****** \fort90\ekdist\resint6.for
            endif
            i1=2
            if(ifall) i1=1
            print 511,i1
511            format(/,
     &' (1) Concatenate if ALL consec openings are in range',/,
     &' (2) Concatenate if ANY consec openings are in range',/,
     &'  Option number [',i2,'] = ')
            call INPUTi(i1)
            if(i1.eq.1) then
                 ifall=.true.
               print 51,alo,ahi
               if(discprt) write(8,51) alo,ahi
51             format(/,
     &' Shut times that are separated by one or more openings with',/,
     &' ALL amplitudes between ', g13.6,'pA and ',g13.6,'pA are ',/,
     &' concatenated into one long shut time',/)
            else if(i1.eq.2) then
                 ifall=.false.
               print 512,alo,ahi
               if(discprt) write(8,512) alo,ahi
512            format(/,
     &' Shut times that are separated by one or more openings with',/,
     &' ANY amplitudes between ', g13.6,'pA and ',g13.6,'pA are ',/,
     &' concatenated into one long shut time',/)
            endif
            goto 36
******

****** RESINT61.FOR
        print 111,acrit
111     FORMAT('& pA for ''real'' amp difference [',f6.2,'] = ')
        call INPUTr(acrit)
****** \fort90\ekdist\resint6.for
        print 111,acrit
111     FORMAT('& pA for ''real'' amplitude difference [',f6.2,'] = ')
        call INPUTr(acrit)
******

****** RESINT61.FOR
c
        ndirect=0       !count # of direct trans introduced by elim of short gaps
        nconc1=0        !count # of concat when 'same' amp each side of irres gap
        nconc2=0        !count # of concat when SHUT amp each side of irres opening
        nconc3=0        !count # of concat of adjacent resolvable openings -new
        nc=0            !# of intervals concat in each group (if nc=1 transfer
c                       !properties directly from input to output)
****** \fort90\ekdist\resint6.for
c
        nc=1            !# of intervals concat in each group (if nc=1 transfer
c                       !properties directly from input to output)
******

****** RESINT61.FOR
60      format(
     & ' Put CAPLOCK on to see values. Working . . . ')
c
****** \fort90\ekdist\resint6.for
60      format(
     & ' Now impose the resolution ',/,
     & ' To see what is being done, put CAP LOCK on now',/,
     & ' (press any key to continue)')
        call ANYKEY()
c
******

****** RESINT61.FOR
c
        call SUMWAMP(-1,j,S,amp,tin,ampav,ttot)         !initialise sums
        setdub=.false.                          !initialise setdub
****** \fort90\ekdist\resint6.for
c
        call SUMWAMP(-1,jamp,S,amp,tin,ampav,ttot)              !initialise sums
        setdub=.false.                          !initialise setdub
******

****** RESINT61.FOR
        endif
        call SUMWAMP(1,j,S,ampl0(i),tint0(i),ampav,ttot)
        if(dubious(i)) setdub=.true.
****** \fort90\ekdist\resint6.for
        endif
        shutint=ampl0(i).eq.0   !Thus is start of concatenated shut interval
        if(shutint) then
           ampav=0              !current amplitude for concat group
        else
           call SUMWAMP(1,jamp,S,ampl0(i),tint0(i),ampav,ttot)
        endif
        if(dubious(n)) setdub=.true.
        if(dubious(i)) setdub=.true.
******

****** RESINT61.FOR
c
c CONCAT3=true when next interval irresolvable so 3 (or more) intervals
c concatenated.
c CONCAT2=true if i and i+1 both open, and have 'same' amplitude,
c and i+1 is resolvable, so 2 adjacent openings are concatenated (as for
c open periods except that weighted mean amp is assigned to result)
        if(RESOLV(i+1,tint0,ampl0,iprops0,nintt)) then
           a1=ampl0(i)
           a2=ampl0(i+1)
           concat2=(a1.ne.0.).and.(a2.ne.0.).and.(.not.DIFAMP(a1,a2))
           concat3=.false.
        else
           concat2=.false.
           concat3=.true.
        endif
c
        if(concat3) then
           a0=ampl0(i)
           a1=ampl0(i+1)
           a2=ampl0(i+2)
           if(i.lt.ilast-1) then
                if(debon) then
                   print 501,i+2,tint0(i+2),ampl0(i+2),iprops0(i+2)
           if(discprt) write(8,501) i+2,tint0(i+2),ampl0(i+2),
     &       iprops0(i+2)
                endif
                setbad=badgap(ki)                       !tint(ki) already set unusable
                tin2=tint0(i+2)
                bad2=badgap0(i+2)
c===            difamp2=DIFAMP(a0,a2)
c FIX JUMPS 07/02/98 07:13pm AS WAS DONE FOR STEADY STATE ON 11/19/97
C Just replacing def of difamp2 as here seems fine
              difamp2=DIFAMP(a0,a2).and.
     &    RESOLV(i+2,tint0,ampl0,iprops0,nintt)
           else !i=ilast-1, so i+1=ilast, i+2 irrelevant
                tin2=0.
                bad2=.false.
                DIFAMP2=.false.
           endif
C Do next bit if NOT resolvable. Has the interval after it got the 'same' amp
C as that before it?
c At this point i=anything up to i=ilast-1. For jumps it makes no sense
c to refer to i+2=ilast+1 which will be first interval of next jump
c
           tin=tint(ki)
           setbad=badgap(ki)                    !tint(ki) already unusable
           if(DIFAMP2) then
c NEXT bit done if tint(i+1) unresolvable but intervals on each
c side have 'different' amps (not done for i=ilast-1 because
c difamp2 set false above, so OK to refer to i+2 here)
c=5             continue
              nc=nc+1
                tin1=tint0(i+1)
                tint(ki)=tin + 0.5*tin1
                setbad=setbad.or.badgap0(i+1)           !tin or tin1 was unusable
                if(setbad) then
****** \fort90\ekdist\resint6.for
c
        if(shutint) then                !look for opening that ends shut group
           a2=ampl0(i+1)
           if(a2.ne.0.and.RESOLV(i+1,tint0,ampl0,iprops0,nintt)) then
c          =shut group ended by open time i+1
                goto 40
           else !keep concatenating the current shut group
c               if(i.eq.ilast-1) then   !check for end of data
c                iprops(ki)=IBSET(iprops(ki),3) !interval #ki is last and set it bad
c                  setbad=.true.
c                  setend=.true.        !so last (irresolvable) interval omitted
c                  tint(ki)=-1.
c                  goto 40      !end group
c               endif
                nc=nc+1
                tin=tint(ki)
                setbad=badgap(ki)                       !tint(ki) already set unusable
                tint(ki)=tin + tint0(i+1)
c if any of these intervals unusable set whole group unusable
                setbad=setbad.or.badgap0(i+1)
                if(setbad) then
******

****** RESINT61.FOR
                endif
                if(j.gt.0) then !?not nec here, now ampl(ki) not used below?
                   ampl(ki)=ampav
                   if(setdub) iprops(ki)=IBSET(iprops(ki),0)    !bit 0 set (='1')
                endif
c         Now finished tint(ki)=value before the newly-formed direct open-open
c               transition, so print output here and start the (possibly concat)
c               level after it =tint(ki+1)
c=              if(ampl(ki).ne.0.and.ampl0(i+2).ne.0) then
                if(a0.ne.0.and.a1.eq.0.and.a2.ne.0) then
                   ndirect=ndirect+1    !count # of direct trans introduced
                endif
                if(debon) then
                   print 507,ki,tint(ki),ampl(ki),iprops(ki),index(ki)
                   if(discprt) write(8,507)ki,tint(ki),ampl(ki),iprops(ki),
     &          index(ki)
507                format(27x,' out = ',i6,g12.5,g12.5,i3,i6,' (diff amp)')
                endif
                tint(ki+1)=0.5*tin1 + tint0(i+2)
                index(ki+1)=i+2                 !define index
                setbad=badgap0(i+1).or.badgap0(i+2)     !tin1 or tint0(i+2) was unusable
                if(setbad) then
                   iprops(ki+1)=IBSET(iprops(ki+1),3)   !gap unusable; set bit 3 ='8'
                endif
c   tint(ki+1) (approx=tint0(i+2)) may need more things concat with it
c   so initialise sums (and setdub) and start new ones.
c=              call SUMAMP(-1,j,S,j1,s1,amp,.false.,setdub)    !initialise sums
c=              call SUMAMP(1,j,S,j1,s1,ampl0(i+2),dubious(i+2),setdub) !accumulate amps
                call SUMWAMP(-1,j,S,amp,tin,ampav,ttot)         !initialise sums
                setdub=.false.                          !initialise setdub
                call SUMWAMP(1,j,S,ampl0(i+2),tint0(i+2),ampav,ttot)
                if(dubious(i+2)) setdub=.true.
                ki=ki+1
                i=i+2
              goto 70  !continue concatenating until resolvable interval found
           else         !difamp=false
c Now bit to be done if tint0(i+1) unresolvable and intervals
c on each side have the 'same' amp. Concatenate.  May be done for i=ilast-1
c so cannot refer to i+2 (use tin2, bad2 defined above)
c=              tint(ki)=tin + tint0(i+1) + tint0(i+2)
                tint(ki)=tin + tint0(i+1) + tin2
              nc=nc+2
                if((a0.eq.0.).and.(a2.eq.0.)) then
                   nconc2=nconc2+1
                else
                   nconc1=nconc1+1
                endif
c if any of these intervals unusable set whole group unusable
c=              setbad=setbad.or.badgap0(i+1).or.badgap0(i+2)
                setbad=setbad.or.badgap0(i+1).or.bad2
                if(setbad) then
****** \fort90\ekdist\resint6.for
                endif
                if(i.lt.ilast-1) then
                   if(dubious(i+1)) setdub=.true.
                   i=i+1
                   goto 70      !continue concatenating until resolvable opening found
                endif           !otherwise carry on to 40
           endif
c
        else if(.not.shutint) then      !look for diff amp (open or shut) that ends open group
           a2=ampl0(i+1)
           diffamp=DIFAMP(ampav,a2).or.a2.eq.0
           if(diffamp.and.RESOLV(i+1,tint0,ampl0,iprops0,nintt)) then
c          =open group ended by resolvable shut time i+1, OR by resolvable
c               opening that has an amplitude different from that of the amplitude
c               of the current amplitude of the concatenated open group
                goto 40
           else !keep concatenating the current open group
c               if(i.eq.ilast-1) then   !check for end of data
c                iprops(ki)=IBSET(iprops(ki),3) !interval #ki is last and set it bad
c                  setbad=.true.
c                  setend=.true.        !so last (irresolvable) interval omitted
c                  tint(ki)=-1.
c                  goto 40      !end group
c               endif
                nc=nc+1
                tin=tint(ki)
                setbad=badgap(ki)                       !tint(ki) already set unusable
                tint(ki)=tin + tint0(i+1)
c if any of these intervals unusable set whole group unusable
                setbad=setbad.or.badgap0(i+1)
                if(setbad) then
******

****** RESINT61.FOR
                endif
                if(i.lt.ilast-1) then
                   call SUMWAMP(1,j,S,ampl0(i+2),tint0(i+2),ampav,ttot)
                   if(dubious(i+2)) setdub=.true.
                   i=i+2
                   goto 70      !continue concatenating until resolvable interval found
                endif           !otherwise carry on to 40
****** \fort90\ekdist\resint6.for
                endif
                call SUMWAMP(1,jamp,S,ampl0(i+1),tint0(i+1),ampav,ttot)
                if(dubious(i+1)) setdub=.true.
                if(i.lt.ilast-1) then
                   if(dubious(i+1)) setdub=.true.
                   i=i+1
                   goto 70      !continue concatenating until resolvable opening found
                endif           !otherwise carry on to 40
******

****** RESINT61.FOR
           endif
        else if(concat2) then
c Now concat adjacent openings (as for open periods)
           nc=nc+2
           tin=tint(ki)
           setbad=badgap(ki)                    !tint(ki) already set unusable
           tint(ki)=tin + tint0(i+1)
           nconc3=nconc3+1
c if any of these intervals unusable set whole group unusable
           setbad=setbad.or.badgap0(i+1)
           if(setbad) then
                iprops(ki)=IBSET(iprops(ki),3)  !gap unusable; set bit 3 ='8'
           endif
           if(i.lt.ilast-1) then
              call SUMWAMP(1,j,S,ampl0(i+1),tint0(i+1),ampav,ttot)
              if(dubious(i+1)) setdub=.true.
              i=i+1
              goto 70   !continue concatenating until resolvable interval found
           endif                !otherwise carry on to 40
        endif
****** \fort90\ekdist\resint6.for
           endif
        endif
******

****** RESINT61.FOR
           iprops(ki)=iprops0(i)        !so transfer props directly
        endif
        if(j.gt.0) then
           ampl(ki)=ampav               !last mean output from sumwamp
           if(setdub) iprops(ki)=IBSET(iprops(ki),0)    !bit 0 set (='1')
        else
           print*,'RESINT ERROR #2'
           STOP
        endif
        if(debon) then
****** \fort90\ekdist\resint6.for
           iprops(ki)=iprops0(i)        !so transfer props directly
           if(setbad) then !e.g. when last interval not resolvable it is set bad
               iprops(ki)=IBSET(iprops(ki),3)
           endif
        endif
        if(shutint) then
           ampl(ki)=0
        else
           if(jamp.gt.0) then
              ampl(ki)=ampav            !last mean output from sumwamp
           else
                print*,'RESINT ERROR #2'
                STOP
           endif
        endif
        if(setdub) iprops(ki)=IBSET(iprops(ki),0)       !bit 0 set (='1')
        if(debon) then
******

****** RESINT61.FOR
c
c Concatenate intervals (REGULAR CASE)
c NB symbols i, ki have been schanged to n, ni from here on (to cope with
****** \fort90\ekdist\resint6.for
c
c Concatenate intervals (STEADY STATE CASE)
c NB There are two reasons to concatenate adjacent intervals -(a) because 2nd
c interval is irresolvable, and (b) because 2nd interval, though resolvable
c has same amplitude (open or shut) as first.
c NB symbols i, ki have been schanged to n, ni from here on (to cope with
******

****** RESINT61.FOR
        n=0
        ndirect=0       !count # of direct trans introduced by elim of short gaps
        nconc1=0        !count # of concat when 'same' amp each side of irres gap
        nconc2=0        !count # of concat when SHUT amp each side of irres opening
        nconc3=0        !count # of concat of adjacent resolvable openings -new
        nc=0            !# of intervals concat in each group (if nc=1 transfer
c                       !properties directly from input to output)
****** \fort90\ekdist\resint6.for
        n=0
        nc=1            !# of intervals concat in each group (if nc=1 transfer
c                       !properties directly from input to output)
******

****** RESINT61.FOR
c (return to 2 for start of next concat group)
2       ni=ni+1
****** \fort90\ekdist\resint6.for
c (return to 2 for start of next concat group)
c Simplified version 02/25/00 07:01am
c (1) A concantenated shut period starts with a good, resolvable shutting
c       and ends when first good resolvable opening found.
c       Length of concat shut period=sum of all durations before the resol opening
c       Amplitude of concat shut period=0
c (2) A concantenated open period starts with a good, resolvable opening
c       and ends when first good resolvable interval is found that
c       has a different amplitude (either shut, or open but diff amplitude).
c       Length of concat open period=sum of all concatenated durations
c       Amplitude of concat open period weighted mean amp of all concat intervals
c Simpler to have separate code for shut groups and for open groups. If first
c interval of group is shut then set shutint=true.
c
c First interval of any concat group must be good and resolvable so
c insert warning to check this
2       ni=ni+1
******

****** RESINT61.FOR
c
        if(debon) then
****** \fort90\ekdist\resint6.for
c
c First interval in each concatenated group must be resolvable, but may
c be bad (in which case next group will be bad)
        if(.not.(RESOLV(n,tint0,ampl0,iprops0,nintt))) then
           call BELL(1)
           print 73,n,tint0(n)
         if(discprt) write(8,73) n,tint0(n)
73         format(' ERROR in RESINT -TELL DC: n = ',i6,g13.6,' ms')
        endif
        if(debon) then
******

****** RESINT61.FOR
        endif
        call SUMWAMP(-1,j,S,amp,tin,ampav,ttot)         !initialise sums
        setdub=.false.                          !initialise setdub
****** \fort90\ekdist\resint6.for
        endif
        call SUMWAMP(-1,jamp,S,amp,tin,ampav,ttot)              !initialise sums
        setdub=.false.                          !initialise setdub
******

****** RESINT61.FOR
        endif
        call SUMWAMP(1,j,S,ampl0(n),tint0(n),ampav,ttot)
        if(dubious(n)) setdub=.true.
c is next interval resolvable? (Return to 7 for next concat interval in a group)
****** \fort90\ekdist\resint6.for
        endif
        shutint=ampl0(n).eq.0   !Thus is start of concatenated shut interval
        if(shutint) then
           ampav=0              !current amplitude for concat group
        else
           call SUMWAMP(1,jamp,S,ampl0(n),tint0(n),ampav,ttot)
        endif
        if(dubious(n)) setdub=.true.
c
c is next interval resolvable? (Return to 7 for next concat interval in a group)
******

****** RESINT61.FOR
        endif

c CONCAT3=true when next interval irresolvable so 3 (or more) intervals
c concatenated.
c CONCAT2=true if i and i+1 both open, and have 'same' amplitude,
c and i+1 is resolvable, so 2 adjacent openings are concatenated (as for
c open periods except that weighted mean amp is assigned to result)
        if(RESOLV(n+1,tint0,ampl0,iprops0,nintt)) then
           a1=ampl0(n)
           a2=ampl0(n+1)
           concat2=(a1.ne.0.).and.(a2.ne.0.).and.(.not.DIFAMP(a1,a2))
           concat3=.false.
        else
           concat2=.false.
           concat3=.true.
        endif
c TEST WITH CONCAT2=FALSE ================
c===    CONCAT2=.FALSE.
c
c
C Do next bit if NOT resolvable. Has the interval after it got the 'same' amp
C as that before it?
c NB if i=nintt-1 then cannot refer to i+2.  If last interval, tint0(i+1)
c =tint0(ninnt) is not resolvable, then must
c (a) if tint0(i)=tint0(ninnt-1) was shut, and followed by irresolvable opening
c       then count last interval as an unusable shut time
c (b) if tint0(i)=tint0(ninnt-1) was open, and followed by irresolvable shutting
c       (or an irresolvable opening of either same or different amplitude) then
c       count last interval as an unusable open time
c
        if(concat3) then
           if(n.eq.nintt-1) then        !fix as above
              iprops(ni)=IBSET(iprops(ni),3) !interval #ki is last and set it bad
                setbad=.true.
                setend=.true.   !so last (irresolvable) interval omitted
                tint(ni)=-1.
                goto 4
           endif
****** \fort90\ekdist\resint6.for
        endif
c
        if(shutint) then                !look for opening that ends shut group
           a2=ampl0(n+1)
           if(a2.ne.0.and.RESOLV(n+1,tint0,ampl0,iprops0,nintt)) then
c          =shut group ended by open time n+1
                goto 4
           else !keep concatenating the current shut group
                if(n.eq.nintt-1) then   !check for end of data
                 iprops(ni)=IBSET(iprops(ni),3) !interval #ki is last and set it bad
                   setbad=.true.
                   setend=.true.        !so last (irresolvable) interval omitted
                   tint(ni)=-1.
                   goto 4       !end group
                endif
c               if(debon) then
c                  print 501,n+2,tint0(n+2),ampl0(n+2),iprops0(n+2)
c          if(discprt) write(8,501) n+2,tint0(n+2),ampl0(n+2),
c     &     iprops0(n+2)
c               endif
                nc=nc+1
                tin=tint(ni)
                setbad=badgap(ni)                       !tint(ki) already set unusable
                tint(ni)=tin + tint0(n+1)
c if any of these intervals unusable set whole group unusable
                setbad=setbad.or.badgap0(n+1)
                if(setbad) then
                   iprops(ni)=IBSET(iprops(ni),3)       !gap unusable; set bit 3 ='8'
                endif
                if(dubious(n+1)) setdub=.true.
                n=n+1
                goto 7  !continue concatenating until resolvable opening found
           endif
******

****** RESINT61.FOR
c
           if(debon) then
                print 501,n+2,tint0(n+2),ampl0(n+2),iprops0(n+2)
        if(discprt) write(8,501) n+2,tint0(n+2),ampl0(n+2),
     &     iprops0(n+2)
           endif
           tin=tint(ni)
           setbad=badgap(ni)                    !tint(ki) already set unusable
           a0=ampl0(n)
           a1=ampl0(n+1)
           a2=ampl0(n+2)
c  Modif 11/19/97 06:51am (see above) -end the concatenation ONLY if the
c last (n+2) event is resolvable!
c==        if(DIFAMP(a0,a2)) then
           if(DIFAMP(a0,a2).and.
     &    RESOLV(n+2,tint0,ampl0,iprops0,nintt)) then
              nc=nc+1
c NEXT bit done if tint(i+1) unresolvable but intervals on each
c side have 'different' amps.
                tin1=tint0(n+1)
                tint(ni)=tin + 0.5*tin1
                setbad=setbad.or.badgap0(n+1)           !tin or tin1 was unusable
                if(setbad) then
****** \fort90\ekdist\resint6.for
c
        else if(.not.shutint) then      !look for diff amp (open or shut) that ends open group
           a2=ampl0(n+1)
           diffamp=DIFAMP(ampav,a2).or.a2.eq.0
           if(diffamp.and.RESOLV(n+1,tint0,ampl0,iprops0,nintt)) then
c          =open group ended by resolvable shut time n+1, OR by resolvable
c               opening that has an amplitude different from that of the amplitude
c               of the current amplitude of the concatenated open group
                goto 4
           else !keep concatenating the current open group
                if(n.eq.nintt-1) then   !check for end of data
                 iprops(ni)=IBSET(iprops(ni),3) !interval #ki is last and set it bad
                   setbad=.true.
                   setend=.true.        !so last (irresolvable) interval omitted
                   tint(ni)=-1.
                   goto 4       !end group
                endif
c               if(debon) then
c                  print 501,n+2,tint0(n+2),ampl0(n+2),iprops0(n+2)
c          if(discprt) write(8,501) n+2,tint0(n+2),ampl0(n+2),
c     &     iprops0(n+2)
c               endif
                nc=nc+1
                tin=tint(ni)
                setbad=badgap(ni)                       !tint(ki) already set unusable
                tint(ni)=tin + tint0(n+1)
c if any of these intervals unusable set whole group unusable
                setbad=setbad.or.badgap0(n+1)
                if(setbad) then
******

****** RESINT61.FOR
                endif
c         Now finished tint(ki)=value before the newly-formed direct open-open
c               transition, so print output here and start the (possibly concat)
c               level after it =tint(ki+1)
                if(a0.ne.0.and.a1.eq.0.and.a2.ne.0) then
                   ndirect=ndirect+1    !count # of direct trans introduced
                endif
                tint(ni+1)=0.5*tin1 + tint0(n+2)
                index(ni+1)=n+2                 !define index
                setbad=badgap0(n+1).or.badgap0(n+2)     !tin1 or tint0(i+2) was unusable
                if(setbad) then
                   iprops(ni+1)=IBSET(iprops(ni+1),3)   !gap unusable; set bit 3 ='8'
                endif
c   Concat of tint(ki) now ended so assign amp to it
                if(j.gt.0) then
                   ampl(ni)=ampav               !last mean output from sumwamp
                   if(setdub) iprops(ni)=IBSET(iprops(ni),0)    !bit 0 set (='1')
                else
                   print*,'RESINT ERROR #1'
                   STOP
                endif
                if(debon) then
                   print 507,ni,tint(ni),ampl(ni),iprops(ni),index(ni)
                   if(discprt) write(8,507)ni,tint(ni),ampl(ni),iprops(ni),
     &     index(ni)
c507             format(27x,' out = ',i6,g12.5,g12.5,i3,i6,' (diff amp)')
                endif
                call SUMWAMP(-1,j,S,amp,tin,ampav,ttot)         !initialise sums
                setdub=.false.                          !initialise setdub
                call SUMWAMP(1,j,S,ampl0(n+2),tint0(n+2),ampav,ttot)
                if(dubious(n+2)) setdub=.true.
                ni=ni+1
                n=n+2
c
           else         !if 'same' amp each side of irresolvable interval
c Now bit to be done if tint0(i+1) unresolvable and intervals
c on each side have the 'same' amp. Concatenate.
              nc=nc+2
                tint(ni)=tin + tint0(n+1) + tint0(n+2)
                if((a0.eq.0.).and.(a2.eq.0.)) then
                   nconc2=nconc2+1
                else
                   nconc1=nconc1+1
                endif
c if any of these intervals unusable set whole group unusable
                setbad=setbad.or.badgap0(n+1).or.badgap0(n+2)
                if(setbad) then
                   iprops(ni)=IBSET(iprops(ni),3)       !gap unusable; set bit 3 ='8'
                endif
                call SUMWAMP(1,j,S,ampl0(n+2),tint0(n+2),ampav,ttot)
                if(dubious(n+2)) setdub=.true.
                n=n+2
           endif
           goto 7       !continue concatenating until resolvable interval found
c
        else if(concat2) then
c Now concat adjacent openings (as for open periods)
           nc=nc+2
           tin=tint(ni)
           setbad=badgap(ni)                    !tint(ki) already set unusable
           tint(ni)=tin + tint0(n+1)
           nconc3=nconc3+1
c if any of these intervals unusable set whole group unusable
           setbad=setbad.or.badgap0(n+1)
           if(setbad) then
                iprops(ni)=IBSET(iprops(ni),3)  !gap unusable; set bit 3 ='8'
           endif
           call SUMWAMP(1,j,S,ampl0(n+1),tint0(n+1),ampav,ttot)
           if(dubious(n+1)) setdub=.true.
           n=n+1
           goto 7       !continue concatenating until resolvable interval found
        endif
****** \fort90\ekdist\resint6.for
                endif
                call SUMWAMP(1,jamp,S,ampl0(n+1),tint0(n+1),ampav,ttot)
                if(dubious(n+1)) setdub=.true.
                n=n+1
                goto 7  !continue concatenating until resolvable opening found
           endif
        endif
******

****** RESINT61.FOR
c
c
C Assign amp if concatenation finished
****** \fort90\ekdist\resint6.for
c
C Assign amp if concatenation finished
******

****** RESINT61.FOR
        endif
        if(j.gt.0) then
           ampl(ni)=ampav               !last mean output from sumwamp
           if(setdub) iprops(ni)=IBSET(iprops(ni),0)    !bit 0 set (='1')
        else
           print*,'RESINT ERROR #1'
           STOP
        endif
        n=n+1
****** \fort90\ekdist\resint6.for
        endif
        if(shutint) then
           ampl(ni)=0
        else
           if(jamp.gt.0) then
              ampl(ni)=ampav            !last mean output from sumwamp
           else
                print*,'RESINT ERROR #1'
                STOP
           endif
        endif
        if(setdub) iprops(ni)=IBSET(iprops(ni),0)       !bit 0 set (='1')
        n=n+1
******
****** RESINT61.FOR
        if(excamp) then
           pause 'Cap lock on for debug'
           debon=debug()
           if(debon) then
              print 29
              if(discprt) write(8,29)
29            format(/,
     &' Following intervals both shut and all between have amplitude',/,
     &'  in exclusion window, so they have been concatenated into',/,
****** \fort90\ekdist\resint6.for
        if(excamp) then
           print 71
71         format(' Now remove specified amplitudes',/,
     &    ' To see what is being done, put CAP LOCK on now',/,
     &    ' (press any key to continue)')
           call ANYKEY()
c
           debon=debug()
           if(.not.ifall) goto 72
c
           if(debon) then
               print 29
               if(discprt) write(8,29)
29             format(/,
     &' The following pairs of shut intervals are separated by',/,
     &'   openings that ALL have amplitudes in the',/,
     &'  in exclusion window, so they have been concatenated into',/,
******

****** RESINT61.FOR
                                iprops(k)=iprops(k1)
                                index(j)=index(k1)
                           enddo
****** \fort90\ekdist\resint6.for
                                iprops(k)=iprops(k1)
                                index(k)=index(k1)
                           enddo
******

****** RESINT61.FOR
                           goto 21
                        else            !outside window -abandon the group
                           goto 21
****** \fort90\ekdist\resint6.for
                           goto 21
                        else            !amp outside window -abandon the group
                           goto 21
******

Resync failed.  Files are too different
****** RESINT61.FOR
           enddo
        endif
c
c Jump to here after setting resolution for cjump
1002    continue                !finished concatenation
c       if(imode.eq.2) RETURN           !for SCDIR
145     continue
c Check number unusable or dubious
        nb1=0
        nb2=0
        nb3=0
        nb4=0
        nb5=0
        do i=1,nint-1
c          if(dubious(i)) nb1=nb1+1     !dubious refers to iprops0(), not iprops()
c          if(badgap0(i)) nb2=nb2+1
           if(BTEST(iprops(i),0)) nb1=nb1+1 !ampl(i) was dubious (bit 0 set ='1')
           if(BTEST(iprops(i),1)) then  !ampl(i) was fixed (bit 0 set ='2')
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
        print 289,NINT,ndirect,nconc1,nconc2,nconc3,nb1,nb2,nb3,nb4,nb5
        if(pon()) write(7,289) NINT,ndirect,nconc1,nconc2,nconc3,nb1,
     &  nb2,nb3,nb4,nb5
        if(discprt) write(8,289) NINT,ndirect,nconc1,nconc2,nconc3,nb1,
     &  nb2,nb3,nb4,nb5
289     FORMAT( ' Number of resolved intervals= ',i8,/,' ',i4,
     & ' direct transitions introduced by removal of short gaps',/,
     & 1x,i5,' concatenations because amplitude ''same'' on each side'
     & ' of irresolvable gap',/,
     & 1x,i5,' concatenations because shut on each side'
     & ' of irresolvable opening',/,
     & 1x,i5,' concatenations because amplitude of adjacent resolvable'
     & ' openings is ''same''',/,
     & ' ',i5,' intervals with dubious amplitudes in output',/,
     & ' ',i5,' shut intervals with fixed amplitudes in output',/,
     & ' ',i5,' open intervals with fixed amplitudes in output',/,
     & ' ',i5,' intervals with constrained amplitudes in output',/,
     & ' ',i5,' intervals with undefined durations in output',/)
        if(excamp) then
           print 294,nconc4
           if(discprt) write(8,294) nconc4
294        format(/,1x,i4,
     &  ' groups of intervals with amplitudes that were all within',/,
     &  ' within exclusion window were concatenated',/)
        endif
        if(debug()) then
25              print 23,nint
23              format(
     & ' Print intervals and amplitudes # n1,n2 (0=skip) [1,',i6,'] = ')
                n1=1
                n2=nint
                call INPUT2i(n1,n2)
                if(n1.gt.0) then
                  if(n2.lt.n1) n2=n1
                  do i=n1,n2
                     print 26,i,tint(i),ampl(i),iprops(i),index(i)
                     if(discprt) write(8,26) i,tint(i),ampl(i),iprops(i),
     &          index(i)
26                   format(i6,g13.6,2x,g13.6,2x,i6,2x,i8)
                  enddo
                  goto 25
                endif
        endif
c
        RETURN
        END


        subroutine SUMWAMP(imode,j,S,amp,tint,ampav,ttot)
c To calculate sum of amplitudes, weighted by their duration (=tint)
c Returns ampav=current value of average after each call, and ttot=current
c value of denominator=total duration of the intervals used
C IF imode=-1 IT INITIALISES.
c
        if(imode.lt.0) then   !initialise only
           j=0
           S=0.0
           ttot=0.0
           RETURN
        endif
c
c Insert precaution in case of 'zero' duration intervals
c   Note that, although avamp is calculated afresh at each call, it is
c not actually used (at present) until concatenation is finished, so
c a value calculated when j=1 (as below) will not be used unless there
c is no concatenation and this is the only amplitude
c   In this version of j>1 and ttot (total length) is still near zero
c then avamp is undefined (this would occur only when all the durations
c in a concatenation are near-zero!)
        j=j+1
        S=S+amp*tint
        ttot=ttot+tint
****** \fort90\ekdist\resint6.for
           enddo
           goto 74      !end of 'ifall' calcs
c
c Now case where ANY amp outside range counts (.not.ifall)
72         continue
c          if(debon) then
c               print 295
c             if(discprt) write(8,295)
c295          format(/,
c     &' The following pairs of shut intervals are separated by',/,
c     &'   openings that ANY of which have amplitudes in the',/,
c     &'  in exclusion window, so they have been concatenated into',/,
c     &'  one shut time, and total number of intervals reduced')
c          endif
c The output tint() should start with an opening, so must first find a valid
c start = sequence of one or more consec openings all outside exclusion range
           i=0
           do while (i.le.nint-1)
                i=i+1
                a1=ampl(i)
                a2=ampl(i+1)
                shut1=a1.eq.0.
                open2=a2.ne.0.
                exc2=(a2.ge.alo.and.a2.le.ahi)  !a2 in exclusion range
                if(shut1.and.open2.and.(.not.exc2)) then        !putative start
                   setexc=.false.       !set true if amp in exclusion range found
                   do j=i+1,nint-1
                        a=ampl(j)
                        shut=a.eq.0.
                        if(a.ge.alo.and.a.le.ahi) then  !amp in exclusion window
                           setexc=.true.
                        endif
                        if(shut) goto 800
                   enddo
                   goto 801             !get here if reach end of data ====?
800                continue             !ampl(j) is shut
c          next shutting found, but if setexc true still have not got good start
                   if(setexc) then
                        i=j-1           !so starts at shut interval j after i=i+1
                        goto 801
                   else
                        ifirst=i+1      !first opening of seq of non-excluded openings
                        iend=j-1
                        goto 802
                   endif
                endif
801             continue
           enddo        !end of do while to look for starting point
802        continue
c Now ready to start at tint(istart) = first opening of a seq of 1 or more
c good (non-excluded) openings
c Define:
c ifirst=index of first (=shut) interval in group to be concat
c ilast=index of last (=shut) interval in group to be concat
c  so ilast+1=index of first non-exc opening in seq of one or more non-exc openings
c iend=index of last opening in seq of one or more non-exc openings
c
c At this point we know that k=istart to k=iend is a sequence of one or
c more good (non-excluded) openings, so they can be copied directly tp output
           ilast=ifirst-1
           m=0
           do k=ilast+1,iend
                m=m+1
                tint(m)=tint(k)
                ampl(m)=ampl(k)
                iprops(m)=iprops(k)
                if(debon) then
                   print 296,m,tint(m),ampl(m),iprops(m),k
                   if(discprt) write(8,296) m,tint(m),
     &          ampl(m),iprops(m),k
296                format(i5,2g13.6,i5,
     &          ' (original interval # ',i5,')')
                endif
           enddo
c Now ready to start on the rest of the data.  tint(iend+1) must be shut (and
c may be start if a concat group_, and it must be
c followed by an opening which may or may not be excluded
           ifirst=iend+1
           nconc4=0
           ip1=0
           i=ifirst-1   !=ifirst when i=i+1 below
c
           do while (i.le.nint-1)
                i=i+1
                a1=ampl(i)
                a2=ampl(i+1)
                shut1=a1.eq.0.
                open2=a2.ne.0.
                exc2=(a2.ge.alo.and.a2.le.ahi)  !a2 in exclusion range
                if(shut1.and.open2.and.(.not.exc2)) then        !putative start
                   setexc=.false.       !set true if amp in exclusion range found
                   do j=i+1,nint-1
                        a=ampl(j)
                        shut=a.eq.0.
                        if(a.ge.alo.and.a.le.ahi) then  !amp in exclusion window
                           setexc=.true.
                        endif
                        if(shut) goto 900
                   enddo
                   goto 901             !get here if reach end of data ====?
******


