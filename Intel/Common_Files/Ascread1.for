	subroutine ASCREAD1(ioff,ilinhead,ncols,nrows,len,
     & datfil,colhead,text,lentext,dcfile)
c To read ascii files
c (a) Files written by ASCWRITE, which start with 'dcformat',
c	can be read without any details being supplied (returns
c	dcfile=true for such files: in this case colhead should
c	return with column headings)
c (b) Other ASCII files -must specify number of lines of header
c	(if set to -1 in call, then number is requested here)
c     Now neither prints nor returns the header.
c (based on ascio1.for)
c NB ASCREAD1 returns nrows, ncols etc so data array can be allocated
c to correct size in calling program. Then read data into this array
c with ASCREAD2.
c NB ASCREAD3/4 are similar but cope with case where columns are
c not of equal length
c
c Modif 11/22/95 07:40am so colhead declared as character colhead*20(ncols)
c SO ON INPUT, A VALUE OF NCOLS THAT IS BIG ENOUGH MUST BE SPECIFIED -it
c will get overwritten by actual no of columns for output (alternative
c would be to not read the column headings here but allocate colhead()
c to right size in calling prog and read headings in ASCREAD2)
c
c OLD NOTES for ascio1.
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
c if nrows=0 assumes numbers are separated by either space(s) (ascii 32),
c	tab(s) (ascii 9) or comma (ascii 44), and returns nrows
c If both 0 then works out number of values as above, and checks vs
c total file length
c
	character datfil*33
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character*18 ascnum
	character*20 colhead(ncols)
	character*70 text
	allocatable::string
	character*1 string(:)
	logical digchar1
	logical present,graph,dcfile

	ict=11	!text colour for DIALOG box
25	continue
       OPEN(unit=11,file=DATFIL,status='UNKNOWN',
     &   access='DIRECT',form='BINARY',RECL=1)
C	OPEN(unit=11,file=DATfil,status='UNKNOWN',
C     &    access='SEQUENTIAL',form='FORMATTED')
	   allocate(string(1:len))
	  
		read(11,REC=1) string(1:LEN)

	   
	   CLOSE(unit=11)
c	endif
c
c Now locate data
c First check whether first 8 characters are 'dcformat'
c If so, read icolwid, ncols, nrows.
	dcfile=.false.
	do i=1,8
	   ascnum(i:i)=string(i)
	enddo
	if(ascnum(1:8).eq.'dcformat') then
	   do i=1,18
		ascnum(i:i)=' '	!clear it again
	   enddo
	   dcfile=.true.
	   ilinhead=2   !2 rows of header for dcformat files (3 if lentext>0)
	   i1=1
	   call FINDSEP(i1,string,len,isep,iend,inext)
c      string(inext)=first character of icolwid now
	   i1=inext
	   call FINDSEP(i1,string,len,isep,iend,inext)
	   do i=i1,iend
		j=i-i1+1
		ascnum(j:j)=string(i)
	   enddo
c      string(inext)=first character of icolwid now
	   call CHTOREAL(ascnum,colwid)
	   icolwid=int(colwid)
c Now have col width so rest of numbers are easily found: next get ncol,nrow
	   do i=1,18
		ascnum(i:i)=' '	!clear it again
	   enddo
	   i1=2*icolwid+1
	   do i=i1,3*icolwid
		j=i-i1+1
		ascnum(j:j)=string(i)
	   enddo
	   call CHTOREAL(ascnum,x)
	   ncols=int(x)
	   do i=1,18
		ascnum(i:i)=' '	!clear it again
	   enddo
	   i1=3*icolwid+1
	   do i=i1,4*icolwid
		j=i-i1+1
		ascnum(j:j)=string(i)
	   enddo
	   call CHTOREAL(ascnum,x)
	   nrows=int(x)
	   do i=1,18
		ascnum(i:i)=' '	!clear it again
	   enddo
	   i1=4*icolwid+1
	   do i=i1,5*icolwid
		j=i-i1+1
		ascnum(j:j)=string(i)
	   enddo
	   call CHTOREAL(ascnum,xn)
	   lentext=int(xn)       !length of text array
c Next get text
	   if(lentext.gt.0) then
		ilinhead=3
		call FINDCRLF(1,string,len,i1)	!i1=byte # for char after 1st cr/lf
		do i=1,lentext
		   text(i:i)=string(i1+i-1)
		enddo
	   endif
c Now get column headings
	   call FINDCRLF(2,string,len,i1)	!i1=byte # for char after 1st cr/lf
	   do k=1,ncols
		i2=i1+icolwid-1
		do i=i1,i2
		   j=i-i1+1
		   colhead(k)(j:j)=string(i)
		enddo
		i1=i1+icolwid
	   enddo
c	   goto
	endif
c
c Now stuff done when file does not start with DCFORMAT
c If neither ioff nor ilinhead specified, try to locate start of data by
c looking for the first line that begins with a number rather than a letter
30	continue
	if(ioff.le.0.and.ilinhead.ge.0) then
	   call FINDCRLF(ilinhead,string,len,ioff)
	else if(ioff.le.0.and.ilinhead.lt.0) then
c	   Check if the first 'number' is number or text -if it is a number
c	   assume there is no header
	   i1=1
	   irow=1
	   do while(i1.le.len)
		ival=ichar(string(i1))
		if(ival.eq.32.or.ival.eq.9.or.ival.eq.44) then
		   i1=i1+1			!blank, tab, comma
		else if(ival.eq.13) then	!CR
		   i1=i1+2				!skip LF
		   irow=irow+1
		else if(digchar1(ival)) then	!number
		   ilinhead=irow-1
		   ioff=i1
		   goto 23
		else		!char other than number, blank or CR/LF
		   i1=i1+1
	         call FINDCRLF(irow,string,len,i1)
		   ioff=i1
		   irow=irow+1	!goto first char on next row
		endif
	   enddo
23	    continue
c Check the value
	    isav=ilinhead
	    if(ilinhead.ne.isav) then
		ioff=-1	!so findcrlf called after 30
		goto 30	!ilinhead wrong -try again
	    endif
	endif
c
c Insert check that string(ioff) is a number
100	do while(.not.digchar1(ichar(string(ioff))))
	   ioff=ioff+1
	enddo
	ioff=ioff+1
	if(.not.digchar1(ichar(string(ioff)))) then
         goto 100
	else
	   ioff=ioff-1
	endif
	istart=ioff

c Now locate the numbers
	if(dcfile) goto 1
c
	iflag=0
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
	   endif
6	   j=j+1
	enddo
	if(j.ne.nrows*ncols) then
c	    call WDIALOG(1,' Error in N (ASCIO1)',12)
	endif
c
1	continue
	DEALLOCATE(string)
999	RETURN
	end
