	subroutine ASCREAD3(ioff,ilinhead,ncols,nrows,nrowmax,len,
     & datfil,colhead,text,lentext,dcfile)

	integer nrows(ncols)
	character datfil*33
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character*18 ascnum
	character*20 colhead(ncols)
	character*70 text
c	character*1 ans,UC
	allocatable::string
	character*1 string(:)
	logical digchar1
	logical present,graph,dcfile
	
c
	OPEN(unit=11,file=DATFIL,status='UNKNOWN',
     &   access='DIRECT',form='BINARY',RECL=1)
!      OPEN(unit=14,file=DATFIL,status='UNKNOWN',
!     &    access='TRANSPARENT')
	   allocate(string(1:len))
	   do i=1,len
		read(14,75) string(i)
75		format(a1)
	   enddo
	   
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
!	    imes=gmdisplaymessagebox('','Header not located',ginformation,gok
		
23	    continue
	    isav=ilinhead
!	   	call QDIALOG(1,'Number of lines in file header',
!     &	   defolt,ict,cans)
		
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
