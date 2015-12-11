	subroutine ASCIO2(ioff,ncols,nrows,len,
     & datfil,xnum,nd1,nd2)
c To read numbers into xnum(i,j) after inputs defined by call to
c ASCIO1, and xnum allocated.
	real*4 xnum(nd1,nd2)
	character*18 ascnum
	character datfil*33
	character*1 string(:)
	allocatable :: string
	logical graph
	integer*2 videotyp
c
	graph=VIDEOTYP().eq.18	!may not be in graphics for refitting
	if(graph) then
	   ict=11	!text colour for DIALOG box
	endif
c Re-read string
      OPEN(unit=14,file=DATFIL,status='UNKNOWN',
     &    access='TRANSPARENT')
	allocate(string(1:len))
	do i=1,len
	   read(14,75) string(i)
75	   format(a1)
	enddo
	CLOSE(unit=14)

c Now locate the numbers
	istart=ioff
	inext=istart
	n=0		!counts converted numbers
	i=1		!row number
	j=0		!col number
	do while(inext.lt.len)
	   i1=inext
	   call FINDSEP(i1,string,len,isep,iend,inext)
	   if(isep.eq.0) then
		if(graph) then
		   call WDIALOG(1,'No separator found',12)
		else
		   print *, 'No separator found'
		endif
	   endif
	   n=iend-i1+1
	   ascnum='                  '
	   do k=1,n
	      if(k.le.18) ascnum(k:k)=string(k+i1-1)
	   enddo
	   n=n+1
	   j=j+1	!next col
	   if(j.gt.ncols) then
		j=1		!back to first col
		i=i+1		!next row
	   endif
	   call CHTOREAL(ascnum,xnum(i,j))
	enddo
c	ntot=n
c
c WRITE SECTION
c======
c1	continue
999	RETURN
	end






