	subroutine GETOPER(jset,tint,ampl,iprops,nint,nd1,nd2)
c New subroutine for HJCFIT that replaces open and shut times in tint()
c with open PERIODS and shut times, so this does not have to be done
c every time HJCLIK is called
c If excop=true, then open periods that are flanked on BOTH sides by
c gaps in specified range are omitted, and shut times on each side
c concatenated into a single shut time (set bad if either of the two
c shut times is bad)
c Modif to change logical excop(10) to real*4 iexcop(10) (same size) and
c  iexcop(j)=0 :  no open periods excluded (same as excop=false)
c  iexcop(j)=1 :  open periods excluded when gap on both sides longer than
c			specified (same as excop=true)
c  iexcop(j)=3 :  Exclude ALL isolated openings (bursts with one opening)
c			(usable only when tcrit supplied), and suitable only for high
c			conc records with essentially no mono-liganded openings
c
	real*4 tint(nd1,nd2),ampl(nd1,nd2)
	integer*1 iprops(nd1,nd2)
	integer nint(10)
	ALLOCATABLE:: tnew,anew,ipnew	!now alloc/dealloc as needed
	real*4 tnew(:),anew(:)
	integer*1 ipnew(:)
	logical bad,bad1,open,open1,good
c For exclusion of openings
	real*4 gaplo(10),gaphi(10)
	integer nskip(10)
c	logical excopen
	integer*4 iexcop(10)
	common/exop/iexcop,gaplo,gaphi,nskip		!for getoper
c	logical excop(10)
c	common/exop/excop,gaplo,gaphi,nskip
c
	logical debug,caplock,deb
	logical discprt
	common/dp/discprt
c
	debug()=caplock()
c
	deb=debug()
c
	if(allocated(tnew)) DEALLOCATE(tnew)
	ALLOCATE(tnew(nint(jset)))
	if(allocated(anew)) DEALLOCATE(anew)
	ALLOCATE(anew(nint(jset)))
	if(allocated(ipnew)) DEALLOCATE(ipnew)
	ALLOCATE(ipnew(nint(jset)))
c
	in=1		!counter for intervals
	j=0			!index in tval()
c	islast=0		!index in tint() of last shut period
	nbo=0
	nbg=0
c Start at first (good) opening
	call FINDOPEN(in,jset,iop,ibad,nbo,nbg,ampl,iprops,nd1,nd2)
	if(ibad.gt.0) nbg=nbg+ibad	!actually ibad counts bad ops too
	if(ibad.eq.-1) goto 991		!no good openings!
	in=iop		!make the opening the current obs
c
90	continue		!return here for next opening
c 	GET LENGTH OF OPEN PERIOD-look forward to see if there are more openings
c     Unusable open times should not occur (fix in new SCAN!), but if they
c	do, end the group and start again at the next (good) opening
	top=0.	!to accum length of open period
	amp=0.
	nop=0		!number of openings/open period
	i1=in
	iofst=-1	!records index in tint() of 1st opening in the open period
	do jn=i1,nint(jset)
	   open=ampl(jn,jset).ne.0.
	   good=.not.BTEST(iprops(jn,jset),3)	!tint(i) was unusable (bit 3='8' set)
	   if(open) then
		if(good) then
		   top=top+tint(jn,jset)
		   amp=amp+ampl(jn,jset)
		   nop=nop+1
		   if(iofst.eq.-1) iofst=jn !index in tint of 1st opening in open period
c		   iolast=jn		!index in tint of last opening in open period
		   if(deb) then
			print 80,jn,tint(jn,jset),ampl(jn,jset),
     &		 iprops(jn,jset)
			if(discprt) write(8,80) jn,tint(jn,jset),
     &		  ampl(jn,jset),iprops(jn,jset)
80			format(1x,i6,2g13.6,i4)
		   endif
		else		!bad opening -should not occur -print if found
c		   if(deb) then
			nbo=nbo+1
			call BELL(2)
			print 81,jn,tint(jn,jset),ampl(jn,jset),
     &		 iprops(jn,jset)
			if(discprt) write(8,81) jn,tint(jn,jset),
     &		  ampl(jn,jset),iprops(jn,jset)
81			format(1x,i6,2g13.6,i4,
     &  ' (bad opening -skips open period; previous shut time set bad)')
c			set the previous opening, already set in tnew(j),
c			as bad, because some openings/shuttings may be
c			skipped before next opn period found
			if(anew(j).ne.0.0) pause ' Error 5 in GETOPER'
			ipnew(j)=8
c		   endif
		   in=jn
		   goto 91
		endif
	   else
		inext=jn	!index of shutting that ends open period
		goto 52	!shut, so jump out
	   endif
	enddo
c 	Get here if last opening reached (and it is good), but in this case
c 	a gap is not found and inext not updated. Therefore set inext=nint+1 to
c 	signal (below) that end reached
	inext=nint(jset)+1
c
52	continue	!shutting found (index=inext), so jumped out
	topen=top
	if(nop.gt.0) then
	   amp=amp/(float(nop))
	endif
c Now have an open period, length=topen
c Put the open period into tnew()
	j=j+1
	tnew(j)=topen
	anew(j)=amp
	ipnew(j)=0
	if(deb) then
	   print 83,j,tnew(j),anew(j),ipnew(j)
	   if(discprt) write(8,83) j,tnew(j),anew(j),ipnew(j)
83	   format(10x,' open period:' i6,2g13.6,i4)
	endif
c At this point the length of the last good shutting will still be in tshut
c (except for 1st open period, for which islast=0 still) so can be used for
c iplot=4 as long as it was adjacent to the opening just found (eg haven't
c skipped a bad burst in between), which will be so if iofst=islast+1
cc===DEBUG CHECK
c	if(.not.good) then
c         print 83,in,jn,tshut,topen
c         if(discprt) write(8,83) in,jn,tshut,topen
c83	   format(' Bad interval at line 710',/,
c     &	' in,jn,tshut,topen = ',2i8,3x,2g13.6)
c	   call DEBPRT(jn,nint,jset,tint,ampl,iprops,nd1,nd2)
c	endif
c
c NOW GET THE NEXT SHUT TIME (or end the group)
c At present in=index of 1st opening in open period
c If last interval reached, and it is open, then no more gaps, and inext was
c set to nint+1, above, so end the last open period at this opening
	if(inext.gt.nint(jset)) goto 92
	in=inext		!should be shut -check, for debug anyway!
	open=ampl(in,jset).ne.0.
	if(open) then
	   call BELL(2)
	   print 61,in
61	   format(' INTERVAL # ',i5,' should be shut')
	endif
c Check for 2 adjacent gaps, or bad gap
	bad=BTEST(iprops(in,jset),3)	!tint(i) was unusable (bit 3='8' set)
	if(in.lt.nint(jset)) then
	   bad1=ampl(in+1,jset).eq.0.    !also bad if next interval is shut too
	endif
	if(deb) then
	   print 82,in,tint(in,jset),ampl(in,jset),
     &    iprops(in,jset)
	   if(discprt) write(8,82) in,tint(in,jset),
     &     ampl(in,jset),iprops(in,jset)
82	   format(1x,i6,2g13.6,i4,' shut time ends open period')
	   if(bad) then
		print 85,bad,bad1
		if(discprt) write(8,85) bad,bad1
85		format(' Bad shut time (bad,bad1 = ',2l4,')')
	   endif
	endif
c
c Now have a good shut time, in tshut say
	tshut=tint(in,jset)
	j=j+1
	tnew(j)=tshut
	anew(j)=0.
	ipnew(j)=iprops(in,jset)	!may be bad
	if(deb) then
	   print 84,j,tnew(j),anew(j),ipnew(j)
	   if(discprt) write(8,84) j,tnew(j),anew(j),ipnew(j)
84	   format(10x,' shut time:' i6,2g13.6,i4)
	endif
	if(bad.or.bad1) then
	   nbg=nbg+1
	   goto 92
	endif
c
c	islast=in	!index in tint() of last shut period
c End of shutting.  Next interval should be an opening
	in=in+1
	if(in.gt.nint(jset)) goto 91		!end of data
	open=ampl(in,jset).ne.0.
	if(.not.open) then
	   call BELL(2)
	   print 611,in
611	   format(' INTERVAL # ',i5,' should be open')
	endif
	goto 90		!get next open period
c
c End of open period when it ends with a shutting (either because a bad
c opening is found, or because last interval in the data is a (good) shutting
c (neither of these should happen with real data!)
91	continue
c Now find next good opening and start new group (unless end of data reached)
	if(in.ge.nint(jset)) goto 991
c must first find a gap that precedes next good opening
	call FINDGAP(in,jset,is,ibad,nbo,nbg,ampl,iprops,nd1,nd2)
	if(ibad.gt.0) nbg=nbg+ibad	!actually ibad counts bad ops too
	if(ibad.eq.-1) goto 991
	in=is		!index of the gap
	call FINDOPEN(in,jset,iop,ibad,nbo,nbg,ampl,iprops,nd1,nd2) !look for good opening
	if(ibad.gt.0) nbg=nbg+ibad	!actually ibad counts bad ops too
	if(ibad.eq.-1) goto 991
	in=iop			!index of the opening
	goto 90			!start new group with the next good opening
c
c
92	continue
c Now find next good opening and start new group (unless end of data reached)
	if(in.ge.nint(jset)) goto 991
c Interval #in should be shut at this point, so now find next good opening
c to start a new group
	call FINDOPEN(in,jset,iop,ibad,nbo,nbg,ampl,iprops,nd1,nd2) !look for good opening
	if(ibad.gt.0) nbg=nbg+ibad	!actually ibad counts bad ops too
	if(ibad.eq.-1) goto 991
	in=iop			!index of the opening
522	continue
	goto 90			!start new group with the next good opening
c
991	continue		!end if definition of open and shut times
	nyval=j
c
c END OF DATA REACHED -tnew etc defined
c
c Check that open and shut times now alternate
	nbo1=0
	nbg1=0
	do i=1,nyval-1
	   open=anew(i).ne.0.
	   if(open) then
		if(BTEST(ipnew(i),3)) nbo1=nbo1+1	!tint(i) was unusable (bit 3='8' set)
	   else
		if(BTEST(ipnew(i),3)) nbg1=nbg1+1	!tint(i) was unusable (bit 3='8' set)
	   endif
	   open1=anew(i+1).ne.0.
	   if(open.eqv.open1) then
		call BELL(2)
		print 86,i,anew(i),i+1,anew(i+1)
		if(discprt) write(8,86) i,anew(i),i+1,anew(i+1)
86		format(' Open and shut should alternate but ',/,
     &	'  amp(',i6,' ) = ',g13.6,', and amp(',i6,' ) = ',g13.6)
	   endif
	enddo
c
	print 70,jset,nint(jset),nbo,nbg,nyval,nbo1,nbg1
	if(discprt) write(8,70) jset,nint(jset),nbo,nbg,nyval,nbo1,nbg1
70	format(' Set ',i3,': Conversion to open periods',/,
     &' Input: ',i7,' transitions (',i4,' bad openings, ',i4,
     &	' bad gaps)',/,
     &' Output: ',i7,' transitions (',i4,' bad open periods, ',i4,
     &	' bad gaps)',/)
c
c Revise the new record if excop=true
c Addition 06/03/02 10:07am to skip open periods that are bordered on BOTH
c sides by shut times in specified range
	nskip(jset)=0
c Copy data back to tint(), ampl() etc, which now contain alternating
c open periods and shut times
c==	if(.not.excop(jset)) then
	if(iexcop(jset).eq.0) then
	   do i=1,nyval
		tint(i,jset)=tnew(i)
		ampl(i,jset)=anew(i)
		iprops(i,jset)=ipnew(i)
	   enddo
	   nint(jset)=nyval
c
	else if(iexcop(jset).ge.1) then	!for iexcop=1 or 2
	   i=0		!index in original tint()
	   in=1		!index in tint() after any omissions made (tint(1) unchanged)
	   open=anew(1).ne.0.
	   if(.not.open) pause 'Error 1 in GETOPER'
	   glo=gaplo(jset)
	   ghi=gaphi(jset)
	   i=3			!start with 2nd open time
	   ts=tnew(i-1)		!tint(2)=first shut time, initially
	   do while(i.le.nyval-1)
		open=anew(i).ne.0.
		if(.not.open) then
		   pause 'Error 2 in GETOPER'
		endif
		ts=tsnext		!from last cycle
		tsnext=tnew(i+1)

c if the gap before and after current opening are both within range then
c look forwards until the next gap that is out of range is found in case
c there are several consecutive gaps all within range -concatenate all of them
		if(ts.ge.glo.and.ts.lt.ghi.AND.
     &       tsnext.ge.glo.and.tsnext.lt.ghi) then	!gap in spec range
		   i1=i+1 	!tint(i+1) already known to be shut and in range
		   tconcat=tnew(i-1)+tnew(i)+tnew(i+1)
		   m1=i			!index of last opening concatenated into long gap
		   tsnext=tnew(i+1) 	!=ts for next cycle
		   i=i+2
		   bad=.false.
		   if(BTEST(ipnew(i-1),3)) bad=.true.	!one of the shut time is bad
		   if(BTEST(ipnew(i+1),3)) bad=.true.	!one of the shut time is bad
		   do m=i1+2,nyval,2
			open=anew(m).ne.0.
			if(open) pause 'Error 3 in GETOPER'
			ts1=tnew(m)
			if(ts1.ge.glo.and.ts1.lt.ghi) then
			   tconcat=tconcat+tnew(m-1)+ts1	!add next open,shut
			   if(BTEST(ipnew(m),3)) bad=.true.	!one of the shut time is bad
			   tsnext=ts1 	!=ts for next cycle
			   m1=m-1
			   i=i+2
			else
			   goto 9	!jump out
			endif
		   enddo
c          at this point, m1=index in original tint of last opening
c		 that was concatenated into a long gap
9		   nskip(jset)=nskip(jset)+1
		   in=in+1
		   tint(in,jset)=tconcat
		   ampl(in,jset)=0.
c Interval has been concatenated so always set bad?
c  (though if the open periods that have been eliminated were a different
c   sort of channel the concatenated shut time would be good)
c===		   iprops(in,jset)=8
		   if(bad) then
			iprops(in,jset)=8
		   else
			iprops(in,jset)=0
		   endif
c           opening after the concatenated shut time
		   in=in+1
		   i1=m1+2		!index of next open period after concat gap
		   tint(in,jset)=tnew(i1)		!open period #i+2
		   ampl(in,jset)=anew(i1)
		   iprops(in,jset)=ipnew(i1)
		   i=i+2		!look at next opening
		else			!case where gaps each side are NOT both in range
		   in=in+1
		   tint(in,jset)=tnew(i-1)	!shut time before opening #i
		   ampl(in,jset)=anew(i-1)
		   iprops(in,jset)=ipnew(i-1)
		   in=in+1
		   tint(in,jset)=tnew(i)		!open period #1
		   ampl(in,jset)=anew(i)
		   iprops(in,jset)=ipnew(i)
		   i=i+2		!look at next opening
		endif
	   enddo
	   if(i.eq.nyval+1) then 	!add the last shutting
		in=in+1
		tint(in,jset)=tnew(i-1)	!last shut time
		ampl(in,jset)=anew(i-1)
		iprops(in,jset)=ipnew(i-1)
	   endif
c reset nint()
	   nint(jset)=in
	endif
	DEALLOCATE(tnew,anew,ipnew)
c
c Check again that open and shut times now alternate
	nbo1=0
	nbg1=0
	do i=1,nint(jset)-1
	   open=ampl(i,jset).ne.0.
	   if(open) then
		if(BTEST(ipnew(i),3)) nbo1=nbo1+1	!tint(i) was unusable (bit 3='8' set)
	   else
		if(BTEST(ipnew(i),3)) nbg1=nbg1+1	!tint(i) was unusable (bit 3='8' set)
	   endif
	   open1=ampl(i+1,jset).ne.0.
	   if(open.eqv.open1) then
		call BELL(2)
		print 86,i,ampl(i,jset),i+1,ampl(i+1,jset)
		if(discprt) write(8,86) i,ampl(i,jset),i+1,ampl(i+1,jset)
c86		format(' Open and shut should alternate but ',/,
c     &	'  amp(',i6,' ) = ',g13.6,', and amp(',i6,' ) = ',g13.6)
	   endif
	enddo
c
	if(iexcop(jset).ne.0) then
	   print 71,nint(jset),nbo1,nbg1
	   if(discprt) write(8,71) nint(jset),nbo1,nbg1
71	   format(' After excluding specified open periods',/,
     &' Output: ',i7,' transitions (',i4,' bad open periods, ',i4,
     &	' bad gaps)',/)
	endif
c
c
	RETURN
	end

