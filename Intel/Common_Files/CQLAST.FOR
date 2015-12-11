	subroutine CQLAST(charmod,ilast,jlast)
c To find last row and col of CHARMOD that contain info. Look for a row that
c is all blanks
c Problem with new qmechs.dat -the blank characters may be ascii=00 rather
c than ascii '  '=32 32 (20 20 in hexadecimal)
c
c Modified 03/04/02 04:42pm to elimnate any blank rows that may occur
c above the first row in which characters appear
c
	character*2 charmod(25,40)
	character*2 e32,e0,ch		!for 'empty'
	logical blank
c
	nrow=25
	ncol=40
	e32='  '		!=char(32)//char(32)
	e0=char(0)//char(0)
c
c Look for blank rows above the model
	nblank=0
	do i=1,nrow
	   blank=.true.
	   do j=1,ncol
		ch=charmod(i,j)
		if(ch.ne.e32.and.ch.ne.e0) blank=.false.
	   enddo
	   if(blank) then
		nblank=nblank+1
	   else
		goto 7	!first non-blank row found
	   endif
	enddo
c  If there are blank rows at the top, eliminate them from charmod
c my moving all rows up nblnak places (and blanking the last nblank rows)
c
7	if(nblank.gt.0) then
	   do i=nblank+1,nrow
		do j=1,ncol
		   ch=charmod(i,j)
		   i1=i-nblank
		   charmod(i1,j)=ch
		enddo
	   enddo
	endif
c
	do i=1,nrow
	   nc=0		!no of cols containing non-blank characters
	   ilast=i-1
	   do j=1,ncol
		ch=charmod(i,j)
		if(ch.ne.e32.and.ch.ne.e0) nc=nc+1
	   enddo
	   if(nc.eq.0) goto 3	!jump out- blank row found
	enddo
	ilast=nrow	!if do not jump out
3	continue	!row # ilast is last one with info
c
	do j=1,ncol
	   nr=0		!no of rows containing non-blank characters
	   jlast=j-1
	   do i=1,nrow
		ch=charmod(i,j)
		if(ch.ne.e32.and.ch.ne.e0) nr=nr+1
	   enddo
	   if(nr.eq.0) goto 6	!jump out- blank col found
	enddo
	jlast=ncol	!if do not jump out
6	continue	!col # jlast is last one with info
	RETURN
	end


