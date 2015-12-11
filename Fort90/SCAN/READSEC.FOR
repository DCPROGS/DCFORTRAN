	subroutine READSEC(isec,idata,nrlast,nsec,ioff,nbuf,nread,novlap,
     & sfac2,in1,in2,jn1,jn2,is1,is2,js1,js2,
     & cjump,invert)
c Version of READSEC2 (see SAMPED) used for SCAN
c
c For new cjump5 format for cjump.dat, ioff is set to appropriate byte #
c for reading the jump data before calling readsec, so no changes needed here
c
c Modif 11/15/96 03:18pm so data inverted when invert=true (no longer
c use negative dgain to do this)
c Modified 01/10/95 04:45pm to read CJUMP data, rather than CONSAM data,
c when cjump=.true.
c
c Version of READSEC that returns indices of 1st and last points
c To read in a section of nmax data points (nrlast points for the last section)
c Input:
c   isec=section number to be read into IDATA
c   nsec=total number of sections
c   ioff=byte offset to start of data in CONSAM.DAT
c   nbuf=number of points per section
c   novlap=number of points in overlap sections
c   scalfac,iyoff= for scaling data before return
c Output:
c   idata()
c  Absolute indices:
c   in1=1 + (isec-1)*nbuf = abs index of 1st point in section (exc overlap)
c   in2=isec*nbuf = abs index of last point in section (exc overlap)
c   jn1=in1 - novlap = abs index of 1st point actually read in (inc overlap)
c   jn2=in2 + novlap = abs index of last point actually read in (inc overlap)
c  Indices in idata (within section)
c   is1=1 = index of 1st point in section (exc overlap)
c   is2=nbuf (or nrlast for isec=nsec)= index of last point (exc overlap)
c   js1=is1 - novlap = index of 1st point (inc overlap)
c   js2=is2 + novlap = index of last point (inc overlap)
c
c SCALING:
C 1401 units are -32768 t0 +32752 in 16 unit increments; dividing
c by 16 gives range as -2048 to +2047 (as for CED502) =-5.0V to +4.9976V
c So 32768/5=6553.6 units/volt, or, for CED502 data 2048/5=409.6
c Now (05/21/93 09:22am) scale in 2 stages to avoid loss of original
c precision by scaling in READSEC to display range integers  (0-480, or
c actually yd1 to yd2= 127 to 457).  Calculate sfac2 to scale orig
c values to integers that cover the range up to at least 4096, so as to
c keep original precision, so numbers are larger than required for display
c by a FACTOR, nscale, that is in integer power of 2.  Also do not add
c offset in READSEC.  Then display routines have only to divide by
c NSCALE and add offset, which should be very fast operations.
c
c	character ADCFIL*30
	integer*2 idata(1-novlap:nbuf+novlap)
	character cnum*11,cnum1*11
	logical cjump,invert
	logical discprt,debug,caplock
	common/dp/discprt
c
	debug()=caplock()
c
	if(cjump) goto 100
c
c Calc indices for section #isec
	call CALCIND(isec,nbuf,nrlast,novlap,nsec,
     & in1,in2,jn1,jn2,is1,is2,js1,js2,nread,jn1next)
c The first data point, idata(1) starts at byte (record #) ioff+1, idata(2) at
c ioff+3, idata(3) at ioff+5 -so point #in (in whole data) starts at
c byte # ioff+(2*in-1)
	irec=ioff + (2*jn1-1)    !record # for 1st value to be read
c
	i1=js1	!for brevity!!
	i2=js2

	if(debug()) then
	   call set_cursor(0,20)
	   print 700,in1,jn1,irec,nread,i1,i2,nbuf,isec
	   if(discprt) write(8,700) in1,jn1,irec,nread,i1,i2,nbuf,
     &	isec
700	   format(' In READSEC2: in1,jn1,irec,nr,i1,i2,nbuf,isec',
     &	/,8i10)
	endif
c
c	call TIMER(n1)
c	OPEN(unit=14,file=ADCFIL,status='UNKNOWN',access='TRANSPARENT')
	read(14,rec=irec) (idata(i),i=i1,i2)
c	CLOSE(unit=14)
c	call TIMER(n2)
c Set to zero after last data point
	if(isec.eq.nsec.and.i2.lt.nbuf) then
	  do i=i2,nbuf
	    idata(i)=0
	  enddo
	endif
	if(debug()) then
	   imin=32767
	   imax=-32768
	   do i=i1,i2
		if(idata(i).gt.imax) imax=idata(i)
		if(idata(i).lt.imin) imin=idata(i)
	   enddo
         call INTCONV(int4(imin),cnum)
         call INTCONV(int4(imax),cnum1)
	   call WDIALOG(1,
     & 'Min, max in raw data = '//charnb(cnum)//', '//charnb(cnum1),11)
	endif
c Scale here
	do i=i1,i2
	   y=float(idata(i))*sfac2
	   idata(i)=int2(y+SIGN(0.5,y))     !correct rounding
	enddo
	if(invert) then
	   do i=i1,i2
		idata(i)=-idata(i)
	   enddo
	endif
c
c	call TIMER(n3)
c	call LOCATE(4,0)
c	print 1,10.*float(n2-n1),10.*float(n3-n2)
c1	format(' Disc read (ms) = ',g13.6,/,' scaling (ms) = ',g13.6)
c
c END of CONSAM read
	RETURN
c
c Now section to read CJUMP data into IDATA(i) (see READREL in CJFIT)
c Could try allocating nbuf so that one sweep always fits into one
c section only, so can set novlap=0 and nbuf=nsamp before first call
c of this subroutine. -would me more general, though, to have possibility
c of dividing a single sweep into sections (10 sec at 40 kHz=0.8 Mb -might
c be too big for some machines) -do this
100	continue
c Calc indices for section #isec
	call CALCIND(isec,nbuf,nrlast,novlap,nsec,
     & in1,in2,jn1,jn2,is1,is2,js1,js2,nread,jn1next)
	i1=js1	!index within current section (for brevity!!)
	i2=js2
c Following loop reads the whole sweep -but may want to read only
c one section of it, so much easier to read back binary data as transparent.
c This is tested in TREAD.FOR.  Byte offset, ioff, for start of binary data
c for current sweep, is calculated in READJUMP.
c	nrec=1 + (nsamp-1)/256		!for idata(i)  =int*2 so 1 rec=256 values
c	lastn=nsamp - (nrec-1)*256	!number to read from last record
c	n=1
c	do j=1,nrec
c	   m=n+255
c	   if(j.eq.nrec) m=n+lastn-1
c	   irec=irec+1
c	   read(14,rec=irec) (idata(i),i=n,m)
c	   n=n+256
c	enddo
	irec=ioff + (2*jn1-1)    !byte # for 1st value to be read
	read(14,rec=irec) (idata(i),i=i1,i2)
c
c Scale the jump data
	do i=i1,i2
	   y=float(idata(i))*sfac2
	   idata(i)=int2(y+SIGN(0.5,y))     !correct rounding
	enddo
	if(invert) then
	   do i=i1,i2
		idata(i)=-idata(i)
	   enddo
	endif
c
	RETURN
	end


