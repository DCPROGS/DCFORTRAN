	subroutine ASCIO1(ioff,ilinhead,ncols,nrows,len,
     & datfil,header1)
c To read ascii files
c ASCIO1 is first part of ASCIODC. It asks for file name, and
c returns ioff,ilinhead,ncols,nrows,ntot,datfil and header (if they
c are not already known) so array for the numbers can be allocated in
c the calling program (then call ASCIO2 to get the numbers)
c read=true/false (false-write)
c Modified 02/23/95 03:08pm to detect whether in graphics mode and write
c to screen appropriately (assumes dialog box 1 defined and open before
c entry if in graphics)
c
c INPUT FOR READING
c Where are numbers: give either:
c ioff=offset (bytes) to first numerical value (ie byte # for start
c   of 1st digit) (0 if not known)
c or
c ilinhead=number of lines of header (ignored if -1).
c	If ilinhead => 0 then expects first number to start
c	  after (ilinhead) <CR><LF> encountered
c	If ilinhead=-1 on input, then attempt made to locate header and
c	  return ilinhead.  All rows in which the first 'number' contains
c	  characters that are not valid in a number (inc spaces) are
c	  counted as header (and blank rows at the top are counted as
c	  part of header, but blank rows after the first valid number
c	  are returned as zero values at present).
c
c Number of values
c ncols= number of columns of values
c nrows=number of rows
c If ncols=0 then assumes columns defined by <CR><LF> and returns value
c if nrows=0 assumes numbers are separated by either space(s) (ascii 32)
c	or comma, and returns nrows
c If both 0 then works out number of values as above, and checks vs
c total file length
c
	character datfil*33
c	character*1 ans,UC
	allocatable::string,header
	character*1 string(:),header(:)
	character header1*(*)
c	logical read,digchar1
	logical digchar1
	logical present,graph
	integer*2 videotyp
c
	graph=VIDEOTYP().eq.18	!may not be in graphics for refitting
	if(graph) then
	   ict=11	!text colour for DIALOG box
	endif
c
c	if(.not.read) goto 1
c READ SECTION
25	continue
	if(graph) then
	   call TDIALOG(1,'File name (+path) to read ASCII data from:',
     &	datfil,33,.false.,ict)
	else
	   call TITENT0('File name (+path) to read ASCII data from:',
     &	datfil,33,.false.)
	endif
	INQUIRE(file=DATFIL,exist=present,flen=len)
	if(.not.present) then
	   call BELL(2)
	   if(graph) then
		n=NBLANK(datfil)
		call WDIALOG(1,datfil(1:n)//' does not exist',ict)
	   else
		print 24,DATFIL
24		format('&',1x,a12,' does not exist')
	   endif
	   goto 25
	else if(present.and.len.gt.0) then
         OPEN(unit=14,file=DATFIL,status='UNKNOWN',
     &    access='TRANSPARENT')
	   allocate(string(1:len))
	   do i=1,len
		read(14,75) string(i)
75		format(a1)
	   enddo
	   CLOSE(unit=14)
	endif
c
c Now locate data
c	if(ioff.le.0) then
c	   call FINDCRLF(ilinhead,string,len,ioff)
c	endif
c If neither ioff nor ilinhead specified, try to locate start of data by
c looking for the first line that begins with a number rather than a letter
	if(ioff.le.0.and.ilinhead.ge.0) then
	   call FINDCRLF(ilinhead,string,len,ioff)
	else if(ioff.le.0.and.ilinhead.lt.0) then
c	   Check if the first 'number' is number or text -if it is a number
c	   assume there is no header
	   i1=1
	   call FINDSEP(i1,string,len,isep,iend,inext)
	   n=iend-i1+1
	   do i=1,n
		ival=ichar(string(i+i1-1))
		if(.not.digchar1(ival)) goto 3	!not a number, so have a header
	   enddo
	   irow=0
	   ioff=1		!ilinhead=0
	   goto 23		!first number found
3	   continue
c now look at first 'number' in 2nd, 3rd ...row and check whether it is a valid
c number
	   irow=1
	   do while(inext.lt.len)
	      call FINDCRLF(irow,string,len,ioff)
		call FINDSEP(ioff,string,len,isep,iend,inext)
		n=iend-ioff+1
		if(n.le.0) goto 31	!not a number, so check next row
		do i=1,n
		   ival=ichar(string(i+ioff-1))
		   if(.not.digchar1(ival)) goto 31	!not a number, so check next row
		enddo
		goto 23		!first number found
31		irow=irow+1
	    enddo
	    if(graph) then
		call WDIALOG(1,'Header not located',12)
	    else
		print 26
26		format(' Header not located')
	    endif
23	    continue
	    ilinhead=irow
	endif
c Insert check that string(ioff) is a number
	do while(.not.digchar1(ichar(string(ioff))))
	   ioff=ioff+1
	enddo
	istart=ioff
c
c Print header one line at a time
	if(istart.gt.1) then
	   lenhead=istart-1
	   allocate(header(1:lenhead))
	   do i=1,lenhead
		header(i)=string(i)
		header1(i:i)=string(i)
	   enddo
c	   if(graph) then
c		n=NBLANK(header1)
c		call WDIALOG(1,header1(1:n),ict)
c	   else
c		print 4
c4		format(1x)
c		do i=1,lenhead
c		   print 5, header(i)
c5		   format('&',a1)
c		enddo
c	   endif
	   deallocate(header)
	endif
c Now locate the numbers
	inext=istart
	ncols=1
	nrows=0
	j=0		!counts converted numbers
	do while(inext.lt.len)
	   i1=inext
	   call FINDSEP(i1,string,len,isep,iend,inext)
	   if(isep.eq.1.and.iflag.ne.1) then
		ncols=ncols+1
	   else if(isep.eq.2) then
		iflag=1	!so cols counted first time only
		nrows=nrows+1
	   else if(isep.eq.0) then
		if(graph) then
		   call WDIALOG(1,'No separator found',12)
		else
		   print *, 'No separator found'
		endif
	   endif
c	   n=iend-i1+1
c	   ascnum='                  '
c	   do i=1,n
c		if(i.gt.18) goto 6
c	      ascnum(i:i)=string(i+i1-1)
c	   enddo
6	   j=j+1
c	   call CHTOREAL(ascnum,xnum(j))
	enddo
c	ntot=j
	if(j.ne.nrows*ncols) then
	  if(graph) then
	    call WDIALOG(1,' Error in N (ASCIO1)',12)
	  else
	    print 28
28	    format(' Error in N (ASCIO1)')
	  endif
	endif
c
c WRITE SECTION
c======
1	continue
	DEALLOCATE(string)
999	RETURN
	end
