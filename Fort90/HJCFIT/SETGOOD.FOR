	subroutine SETGOOD(isgood,iegood,jset,tint,ampl,iprops,
     &	nint,nintt,index,nmax,nset)
c
c  To convert tint() etc so that points n1 to n2 only are used, in HJCFIT
c  (similar to \ekdist\rembad.for)
	real*4 TINT(nmax,nset),ampl(nmax,nset)
	integer*1 iprops(nmax,nset)
	integer*4 index(nmax)
	integer isgood(10),iegood(10)  !use points n1 to n2 for set j
	logical discprt
	logical caplock,debug
	logical btest,badgap,dubious
c	character*1 ans,UC
	common/dp/discprt
c
c Define functions
c	debug()=caplock()
	dubious(i)=BTEST(iprops(i,jset),0)	!ampl(i) was dubious (bit 0 set ='1')
	badgap(i)=BTEST(iprops(i,jset),3)	!tint(i) was unusable (bit 3='8' set)
c
	debug=caplock()
c	n1=isgood(jset)
c	n2=iegood(jset)
c First revise each good bit, if necessary, so that it starts and ends with
c a usable opening
	ngood=1	!only one good bit per set at present
	j=jset
	do n=1,ngood
c	   i1=isgood(n)		!start of good bit #n
	   i1=isgood(j)		!start of good bit for set j
	   do i=i1,nint
		amp=ampl(i,j)
		if(amp.ne.0.and.(.not.badgap(i)).and.tint(i,j).gt.1.e-5)then
		   isgood(j)=i		!revised start
		   goto 5		!jump out
		endif
	   enddo
c
5	   continue
	   i2=iegood(j)		!end of good bit for set j
	   do i=i2,i1,-1
		amp=ampl(i,j)
		if(amp.ne.0.and.(.not.badgap(i)).and.tint(i,j).gt.1.e-5)then
		   iegood(j)=i        !revised end
		   goto 3		!jump out
		endif
	   enddo
3	   continue
	enddo
c
7	continue
c
c Now allocate new tint(), ampl, iprops
	j=jset
	k=0		!index for new TINT,iampl
	do i=1,nint
	   do n=1,ngood
		if(i.ge.isgood(j).and.i.le.iegood(j)) then	!point is good
		   k=k+1
	         tint(k,j)=tint(i,j)
	         ampl(k,j)=ampl(i,j)
		   iprops(k,j)=iprops(i,j)
	         if(debug) print 703,i,k,tint(k,j),ampl(k,j)
703	         format(' i,k,j,tint(k,j),iampl(k,j) = ',3i8,2g13.6)
c==?		   if(i.eq.iegood(j).and.n.lt.nint) then	!insert unusable gap next
		   if(i.eq.iegood(j).and.k.lt.nmax) then	!insert unusable gap next
			k=k+1
			ampl(k,j)=0
			iprops(k,j)=IBSET(iprops(k,j),3)   !set bit 3='8'
			if(debug) print 703,i,k,j,tint(k,j),ampl(k,j)
		   endif
		endif
	   enddo	!n=1,ngood
	enddo		!i=1,nint
c
	nintsav=nint	!for use below
	nint=k		!new number of intervals
	if(debug) pause
c
c
c Print what has been done
      print 33
      if(discprt) write(8,33)
33	format(/,' Define good section for each set',/,
     & ' Set #    Orig number  Use from n1 - n2',
     & '     New number')

	j=jset
	ngood=1
	do n=1,ngood
	   i1=isgood(j)
	   i2=iegood(j)
         print 34,jset,nintsav,i1,i2,nint
         if(discprt) write(8,34) jset,nintsav,i1,i2,nint
34	   format(i4,5x,i8,3x,i8,' - ',i8,4x,i8,'  (after resolution)')
         print 35,jset,index(i1),index(i2),nintt
         if(discprt) write(8,35) jset,index(i1),index(i2),nintt
35	   format(i4,16x,i8,' - ',i8,4x,i8,'  (before resolution)')
	enddo
	pause
	print 108
	if(discprt) write(8,108)
108	format(/)
c
	RETURN
	end

