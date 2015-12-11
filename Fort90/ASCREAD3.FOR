	subroutine ASCREAD3(ioff,ilinhead,ncols,nrows,nrowmax,len,
     & datfil,colhead,text,lentext,dcfile)
c ASCREAD3 is version of ASCREAD1 that deals with special case where
c nrows=array to allow different number of values
c in each column -assumes ncols, nrows(j) specified at input
c This version assumes that ncols is specified in the call,
c (1) identifies header if any
c (2) just asks for number of values in each column (attempts to identify
c this automatically proved difficult (esp given that could have more than
c one consecutive empty field) -would be OK if fields were always
c comma-delimited, but no easy way to tell whether a series of consecutive
c nulls, spaces, tabs constitute one separator or whether they include
c a blank field(s)!
c
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
c
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
	integer nrows(ncols)
	character datfil*33
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character*18 ascnum
	character*20 colhead(ncols)
	character*70 text
c	character*1 ans,UC
	allocatable::string
	character*1 string(:)
c	allocatable::header
c	character*1 header(:)
c	character header1*(*)
c	logical read,digchar1
	logical digchar1
	logical present,graph,dcfile
	integer*2 videotyp
c
	graph=VIDEOTYP().eq.18	!may not be in graphics for refitting
	if(graph) then
	   ict=11	!text colour for DIALOG box
	endif
c
25	continue
	if(graph) then
	   call TDIALOG(1,'File name (+path) to read ASCII data from:',
     &	datfil,33,.true.,ict)
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
	   print 71
71	   format(' Reading ASCII data . . .')
         OPEN(unit=14,file=DATFIL,status='UNKNOWN',
     &    access='TRANSPARENT')
	   allocate(string(1:len))
	   do i=1,len
		read(14,75) string(i)
75		format(a1)
	   enddo
	   CLOSE(unit=14)
	   print 72
72	   format(' Reading finished')
	endif
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
	   icolwid=ifixr(colwid)
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
	   ncols=ifixr(x)
	   do i=1,18
		ascnum(i:i)=' '	!clear it again
	   enddo
	   i1=3*icolwid+1
	   do i=i1,4*icolwid
		j=i-i1+1
		ascnum(j:j)=string(i)
	   enddo
	   call CHTOREAL(ascnum,x)
c=	   nrows=ifixr(x)
	   nrowmax=ifixr(x)
	   do i=1,18
		ascnum(i:i)=' '	!clear it again
	   enddo
	   i1=4*icolwid+1
	   do i=i1,5*icolwid
		j=i-i1+1
		ascnum(j:j)=string(i)
	   enddo
	   call CHTOREAL(ascnum,x)
	   lentext=ifixr(x)       !length of text array
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
	    if(graph) then
		call WDIALOG(1,'Header not located',12)
	    else
		print 26
26		format(' Header not located')
	    endif
23	    continue
c Check the value
	    isav=ilinhead
	    if(graph) then
		call DEFOLTi(ilinhead,defolt)
		call QDIALOG(1,'Number of lines in file header',
     &	   defolt,ict,cans)
		call GETINPi(cans,ilinhead)
	    else
		print 21,ilinhead
21		format(' Number of lines in file header [',i2,'] = ')
		call INPUTi(ilinhead)
	    endif
	    if(ilinhead.ne.isav) then
		ioff=-1	!so findcrlf called after 30
		goto 30	!ilinhead wrong -try again
	    endif
	endif
c
c Insert check that string(ioff) is a number
	do while(.not.digchar1(ichar(string(ioff))))
	   ioff=ioff+1
	enddo
	istart=ioff
c
	do j=1,ncols
	   nrows(j)=0	!initialise
	enddo
c Make guess at number of rows (as in ascread1) -should be OK if all
c columns contain same number of values
	inext=istart
	irows=0
	do while(inext.lt.len)
	   i1=inext
	   call FINDSEP(i1,string,len,isep,iend,inext)
	   if(isep.eq.2) then
		irows=irows+1
	   endif
c	   if(inext.gt.len-30) then
c		print 70,inext,iend,isep
c70		format(1x,3i8)
c		pause
c	   endif
	enddo
c  Ask for number if values in 1st, 2nd column
	do j=1,ncols
	   nrows(j)=irows		!initialise to default
	enddo
	nrows(1)=irows		!default
	do j=1,ncols
	   if(graph) then
	   else
		print 150,j,nrows(j)
150		format('&Number of values in column ',i2,': [',i7,'] = ')
		call INPUTi(nrows(j))
	   endif
	enddo
c No need for rest of code from ascread1, only function of which was to
c find the number of rows and columns!
	nrowmax=0
	do j=1,ncols
	   if(nrows(j).gt.nrowmax) nrowmax=nrows(j)
	enddo
c
1	continue
	DEALLOCATE(string)
999	RETURN
	end
