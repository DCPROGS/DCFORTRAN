	subroutine READSEC1(isec,idata,nrlast,nsec,ioff,nbuf,nread,novlap,
     & scalfac,iyoff,ins1,ins2,ins11,ins12)
c Version of READSEC that returns abs index of 1st and last points actually
c read (including overlaps)
c Version of READSEC2 (see SAMPED) used for SCAN
c isec=section number to be read into IDATA
c nsec=total number of sections
c ioff=byte offset to start of data in CONSAM.DAT
c nbuf=number of points per section
c novlap=number of points in overlap sections
c scalfac,iyoff= for scaling data before return

c ins1=1 + (isec-1)*nbuf = abs index of 1st point in section
c ins2=isec*nbuf = abs index of last point in section
c ins11=ins1 - novlap = abs index of 1st point actually read in (inc overlap)
c ins12=ins2 + novlap = abs index of last point actually read in (inc overlap)

c (for 1st and last sections have no overlap at start/end respectively)
	integer*2 idata(1-novlap:nbuf+novlap)
c	character str*11
	logical discprt,debug,caplock
	common/dp/discprt
c
	debug()=caplock()
c
c	call INTCONV(isec,str)
c	call BELL(1)
c	call NEWPEN(11)
c	call GTEXT(0,0,'READING SECTION NUMBER '//str(1:3)//'        ')
c
c Point #1, of section #isec, has index in whole data set of
	ins1 = 1 + (isec-1)*nbuf
	ins11=ins1-novlap      !actual index that includes extension
	if(isec.eq.1) ins11=1
c The first data point, idata(1) starts at byte (record #) ioff+1, idata(2) at
c ioff+3, idata(3) at ioff+5 -so point #in (in whole data) starts at
c byte # ioff+(2*in-1)
	irec=ioff + (2*ins11-1)    !record # for 1st value to be read
c Define number of values to read
	nread=nbuf+2*novlap			!extension at both ends
	if(nsec.gt.1) then
	   if(isec.eq.1.and.nsec.gt.1) then
		nread=nbuf+novlap
		ins2=ins1+nbuf
	   else if(isec.eq.nsec) then
		nread=novlap+nrlast	!last sec
		ins2=ins1+nrlast
	   endif
	else if(nsec.eq.1) then
	   nread=nrlast			!only one section!
	   ins2=ins1+nrlast
	endif
	ins12=ins11+nread
c
c Now calc index in IDATA of points to be read
	i1=-(novlap-1)		!=1 if novlap=0
	if(isec.eq.1) i1=1
	i2=i1+nread-1
	if(debug()) then
c	   print 700,ins1,ins11,irec,nread,i1,i2,nbuf,isec
	   call set_cursor(0,20)
		print*,ins1,ins11,irec,nread,i1,i2,nbuf,isec
	   if(discprt) write(8,700) ins1,ins11,irec,nread,i1,i2,nbuf,
     &	isec
700	   format(' In READSEC: ins1,ins11,irec,nread,i1,i2,nbuf,isec',
     &	/,8i10)
	endif
c
	read(14,rec=irec) (idata(i),i=i1,i2)
c Set to zero after last data point
	if(isec.eq.nsec.and.i2.lt.nbuf) then
	  do i=i2,nbuf
	    idata(i)=0
	  enddo
	endif
c Scale here
	do i=i1,i2
	   re=float(idata(i))*scalfac
	   idata(i)=int2(ifixr(re) + iyoff)
	enddo
c
	RETURN
	end

