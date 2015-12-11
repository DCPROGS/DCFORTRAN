	subroutine REMBAD(tint,iampl,nint,nbad,isbad,iebad,pon)
c To remove bad sections of record, as defined by stability plot in EKDIST
c Modified 05/30/92 09:43am so that excision of a bad section is marked by
c an unusable gap, in place of the bad bit. To do this, find the first and last
c good openings in each good bit and put these, and all intervals between
c them, into the revised tint(); and insert an unusable gap between the
c last opening of one good bit, and the first opening of the next
	real TINT(20480)
	integer*2 iampl(20480)
	logical discprt,pon
	logical caplock,debug
c	character*1 ans,UC
	integer isbad(20),iebad(20)
	integer isgood(20),iegood(20)
	common/dp/discprt
c
c Define good bits
	debug=caplock()
	if(isbad(1).eq.1.and.iebad(nbad).eq.nint) then
	   ngood=nbad-1
	   do 10 i=1,ngood
	    isgood(i)=iebad(i)+1
	    iegood(i)=isbad(i+1)-1
10	   continue
	else if(isbad(1).eq.1.and.iebad(nbad).lt.nint) then
	   ngood=nbad
	   do 11 i=1,ngood-1
	    isgood(i)=iebad(i)+1
	    iegood(i)=isbad(i+1)-1
11	   continue
	   isgood(ngood)=iebad(nbad)+1
	   iegood(ngood)=nint
	else if(isbad(1).gt.1.and.iebad(nbad).eq.nint) then
	   ngood=nbad
	   isgood(1)=1
	   iegood(1)=isbad(1)-1
	   do 12 i=2,ngood
	    isgood(i)=iebad(i-1)+1
	    iegood(i)=isbad(i)-1
12	   continue
	else if(isbad(1).gt.1.and.iebad(nbad).lt.nint) then
	   ngood=nbad+1
	   isgood(1)=1
	   iegood(1)=isbad(1)-1
	   do 13 i=2,ngood-1
	    isgood(i)=iebad(i-1)+1
	    iegood(i)=isbad(i)-1
13	   continue
	   isgood(ngood)=iebad(nbad)+1
	   iegood(ngood)=nint
	endif
	if(debug) then
	  do 700 i=1,ngood
	  i1=isgood(i)
	  i2=iegood(i)
	  print 701,i,i1,tint(i1),iampl(i1),i2,tint(i2),iampl(i2)
701	  format(' ',i3,' start good: ',i8,g13.6,i8,/,
     &                '   end good: ',i8,g13.6,i8)
700	  continue
	  pause
	endif
c
c First revise each good bit, if necessary, so that it starts and ends with
c a usable opening
	do 3 n=1,ngood
	 i1=isgood(n)		!start of good bit #n
	 do 4 i=i1,nint
	  iamp=IAVAL(i)
	  if(iamp.ne.0.and.tint(i).gt.1.e-5) then
		isgood(n)=i		!revised start
		goto 5		!jump out
	  endif
4	 continue
c
5	 continue
	 i2=iegood(n)		!end of good bit #n
	 do 6 i=i2,i1,-1
	  iamp=IAVAL(i)
	  if(iamp.ne.0.and.tint(i).gt.1.e-5) then
		iegood(n)=i        !revised end
		goto 3		!jump out
	  endif
6	 continue
3	continue
c
7	continue
c
	if(debug) then
	  do 710 i=1,ngood
	  i1=isgood(i)
	  i2=iegood(i)
	  print 701,i,i1,tint(i1),iampl(i1),i2,tint(i2),iampl(i2)
710	  continue
	  pause
	endif
c
c Now allocate new tint(), iampl
	j=0		!index for new TINT,iampl
	do 31 i=1,nint
	   do 32 n=1,ngood
	   if(i.ge.isgood(n).and.i.le.iegood(n)) then	!point is good
	      j=j+1
	      tint(j)=tint(i)
	      iampl(j)=iampl(i)
	      if(debug) print 703,i,j,tint(j),iampl(j)
703	      format(' i,j,tint(j),iampl(j) = ',2i8,g13.6,i8)
		if(i.eq.iegood(n).and.n.lt.nint) then	!insert unusable gap next
		   j=j+1
		   tint(j)=-1.
		   iampl(j)=0
		   if(debug) print 703,i,j,tint(j),iampl(j)
		endif
	   endif
32	   continue
31	continue
	nintsav=nint	!for use below
	nint=j		!new number of intervals
	if(debug) pause
c
c Redefine bad bits in case they were revised above. But care needed
c if good bits were contracted slightly above, to make them start and
c end with good opening, make sure that this does not introduce a spurious
c new bad bit here, consisting of a few points at very start or very end
c of sample.  In fact better omit this revision.  When unrevised bad
c bits are read from .INI file the good bits used to revise tint() will
c get revised correctly as above, each time
c	if(isgood(1).eq.1.and.iegood(ngood).eq.nintsav) then
c	   nbad=ngood-1
c	   do 110 i=1,nbad
c	    isbad(i)=iegood(i)+1
c	    iebad(i)=isgood(i+1)-1
c110	   continue
c	else if(isgood(1).eq.1.and.iegood(ngood).lt.nintsav) then
c	   nbad=ngood
c	   do 111 i=1,nbad-1
c	    isbad(i)=iegood(i)+1
c	    iebad(i)=isgood(i+1)-1
c111	   continue
c	   isbad(nbad)=iegood(ngood)+1
c	   iebad(nbad)=nintsav
c	else if(isgood(1).gt.1.and.iegood(ngood).eq.nintsav) then
c	   nbad=ngood
c	   isbad(1)=1
c	   iebad(1)=isgood(1)-1
c	   do 112 i=2,nbad
c	    isbad(i)=iegood(i-1)+1
c	    iebad(i)=isgood(i)-1
c112	   continue
c	else if(isgood(1).gt.1.and.iegood(ngood).lt.nintsav) then
c	   nbad=ngood+1
c	   isbad(1)=1
c	   iebad(1)=isgood(1)-1
c	   do 113 i=2,nbad-1
c	    isbad(i)=iegood(i-1)+1
c	    iebad(i)=isgood(i)-1
c113	   continue
c	   isbad(nbad)=iegood(ngood)+1
c	   iebad(nbad)=nintsav
c	endif
c	if(debug) then
c	  do 705 i=1,nbad
c	  print 706,i,isbad(i),iebad(i)
c706	  format(' i,isbad(i),iebad(i) = ',3i8)
c705	  continue
c	  pause
c	endif
c
c Print what has been done
      print 33,nbad,nint
      if(pon) write(7,33) nbad,nint
      if(discprt) write(8,33) nbad,nint
33	format(
     & ' After',i3,' bad sections omitted (from stability plot) there',
     & /,' are now ',i5,' good intervals. Bad sections were ',/,
     & '    from interval #    to #')
	do 34 n=1,nbad
      print 35,n,isbad(n),iebad(n)
      if(pon) write(7,35) n,isbad(n),iebad(n)
      if(discprt) write(8,35) n,isbad(n),iebad(n)
35	format(i8,3x,i8,2x,i8)
34	continue
	print 108
	if(pon) write(7,108)
	if(discprt) write(8,108)
108	format(/)
c
	RETURN
	end

