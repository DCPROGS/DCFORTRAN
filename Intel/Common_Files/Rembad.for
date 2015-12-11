	subroutine REMBAD(tint,ampl,iprops,nint,nintt,nbad,
     &	isbad,iebad,index,pon)
c	subroutine REMBAD(tint,iampl,nint,nbad,isbad,iebad,pon)
c To remove bad sections of record, as defined by stability plot in EKDIST
c Modified 05/30/92 09:43am so that excision of a bad section is marked by
c an unusable gap, in place of the bad bit. To do this, find the first and last
c good openings in each good bit and put these, and all intervals between
c them, into the revised tint(); and insert an unusable gap between the
c last opening of one good bit, and the first opening of the next
	real*4 TINT(nintt),ampl(nintt)
	integer*1 iprops(nintt)
c=	real TINT(20480)
c=	integer*2 iampl(20480)
	logical discprt,pon
	
	logical btest,badgap,dubious
c	character*1 ans,UC
	integer isbad(20),iebad(20)
	integer isgood(20),iegood(20)
	integer*4 index(nintt)
	common/dp/discprt
c

	dubious(i)=BTEST(iprops(i),0)	!ampl(i) was dubious (bit 0 set ='1')
	badgap(i)=BTEST(iprops(i),3)	!tint(i) was unusable (bit 3='8' set)
c
	
c Define good bits
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
	
c
c First revise each good bit, if necessary, so that it starts and ends with
c a usable opening
	do 3 n=1,ngood
	 i1=isgood(n)		!start of good bit #n
	 do 4 i=i1,nint
c=	  iamp=IAVAL(i)
	  amp=ampl(i)
c=	  if(iamp.ne.0.and.tint(i).gt.1.e-5) then
	  if(amp.ne.0.and.(.not.badgap(i)).and.tint(i).gt.1.e-5) then
		isgood(n)=i		!revised start
		goto 5		!jump out
	  endif
4	 continue
c
5	 continue
	 i2=iegood(n)		!end of good bit #n
	 do 6 i=i2,i1,-1
	  amp=ampl(i)
	  if(amp.ne.0.and.(.not.badgap(i)).and.tint(i).gt.1.e-5) then
		iegood(n)=i        !revised end
		goto 3		!jump out
	  endif
6	 continue
3	continue
c
7	continue
c

c Now allocate new tint(), ampl, iprops
	j=0		!index for new TINT,iampl
	do 31 i=1,nint
	   do 32 n=1,ngood
	   if(i.ge.isgood(n).and.i.le.iegood(n)) then	!point is good
	      j=j+1
	      tint(j)=tint(i)
	      ampl(j)=ampl(i)
		iprops(j)=iprops(i)
	     
703	      format(' i,j,tint(j),iampl(j) = ',2i8,2g13.6)
		if(i.eq.iegood(n)) then	!insert unusable gap next
		   j=j+1
		   tint(j)=1.	!arbitaray unusable shut time
		   ampl(j)=0
		   iprops(j)=IBSET(iprops(j),3)   !set bit 3='8'
		  
		endif
	   endif
32	   continue
31	continue
	nintsav=nint	!for use below
	nint=j		!new number of intervals
	
c Print what has been done
      
      if(discprt) write(7,33) nbad,nintsav,nint
33	format(
     & ' After',i3,' bad sections omitted (from stability plot) the',/,
     & ' number of intervals is reduced from ',i6,' to ',i6,/,
     & ' The bad sections were ',/,
     & '      from interval #       to #')
	do n=1,nbad
	   i1=isbad(n)
	   i2=iebad(n)
         
       
         if(discprt) write(7,34) n,i1,i2
34	   format(i8,3x,i8,2x,i8,'  (after resolution imposed)')
        
       
         if(discprt) write(7,35) n,index(i1),index(i2)
35	   format(i8,3x,i8,2x,i8,'  (before resolution imposed)')
	enddo


	if(discprt) write(7,108)
108	format(/)
c
	RETURN
	end

